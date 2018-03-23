000001011228     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND')
000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101011127     H*--------------------------------------------------------------*
000400020103     H*             - anagrafico raggruppamenti -                    *
000500000000     H*--------------------------------------------------------------*
000501011228     Fansif01L  IF   E           K DISK
000600011228     Fycoa821d  CF   E             WORKSTN
000900011228     Fynstr00f  UF a E             DISK
007701011127      *
007702020103     D Psds           SDS
007703020103     D  PgmName          *PROC
007704011228     D KPJBA         E DS
007705011228     D* Richiamo a XATB
007706011228     D angg93ds      E DS
007707011228     D XATBDS        E DS
007708011228     D XTabDS        E DS                  inz
007709011228    >D xsoc001ds     E DS
007710011228     D ana007ds      e ds                  inz
007711011228     D* DS Interna per dati di output di XSOC
007712011228     D XSOCDS          DS          1000
007713011127     D DTAiso          S               D   INZ
007714011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
007715011127     D DTADAL          S               D   INZ
007716011127     D DTAAL           S               D   INZ
007717011228     d data            ds            10
007718011228     d aa                      3      4
007719011228     d mm                      6      7
007720011228     d gg                      9     10
007721011228     D com6            S              6
013400000000     C*----------------------------------------------------*
013700000000     C                   EXSR      AZZER
013701011228     c                   if        nrr <> 0
013702011228     c                   exsr      srcarica
013703011228     c                   end
013704011127     C*
013705011228     C  nlr              DO        *HIVAL
013706011127     c                   EXSR      CLR00
013709011228     c                   if        *in99
013710011228     c                   setoff                                       99
013711011228     c                   write     r01
013712011228     c                   seton                                        99
013713011228     c                   end
013714011228     C                   IF        mod = '4 '
013715011228     C                             OR
013716011228     C                             mod = '5 '
013717011228     C                   WRITE     R01
013718011228     C                   EXFMT     R01Protect
013719011228     C                   ELSE
013720011228     C                   EXFMT     R01
013721011228     C                   ENDIF
014000000000     C                   SETOFF                                       99
014100011127     C* FINE LAVORO
014101011228     C                   IF        *INKC
014102011228     C                   MOVE      'C'           CMD
014200011228     C                   LEAVE
014201011228     c                   end
014202011228     C* GUIDA
014203011228     C                   IF        *INKL
014204011228     C                   MOVE      'L'           CMD
014205011228     C                   LEAVE
014206011228     c                   end
014207020128     C* ripristino
014208020128     C                   IF        *INKe
014209020128     C                   exsr      srcarica
014210020128     C                   iter
014211020128     c                   end
014212011127     C* RICERCA
014213011127     C   KD              EXSR      RIC01
014300011228     C* controlli
014301011127     C                   EXSR      CTR01
014302011127     C   99              ITER
014303011228     C* AGGIORNA
014304011228     C                   IF        *INKF
014305011228     C                   EXSR      SRAGGIO
014306011228     C                   IF        *IN41
014307011228     C                   SETON                                        99
014308011228     c                   iter
014309011228     c                   end
014310011228     C                   MOVE      'F'           CMD
014311011228     C                   LEAVE
014312011228     c                   end
014313011228     c                   enddo
024902011127     c*
025000000000     C                   SETON                                        LR
025023011127     c**********************************************************************
025024011228     c* carica 1° formato
025025011127     c**********************************************************************
025026011228     C     srcarica      BEGSR
025027011228     C     nrr           chain     ynstr00f
025028011228     C                   if        %found(ynstr00f)
025030011228     c                   movel     strscg        codragv
025031011228     c                   movel     strdescr      deslunv
025032011228     c                   movel     strdesbrev    desbrev
025033011228     c                   movel     strlivrise    livrisv
025034011228     c                   movel     strrgabent    regabiv
025037011228     c                   z-add     0             dtadalv
025038011228     c                   if        strdtinval <> *loval
025039011228     c                   move      strdtinval    data
025040011228     c                   eval      com6 = gg + mm + aa
025041011228     c                   move      com6          dtadalv
025042011228     c                   end
025043020103     c                   z-add     999999        dtaalv
025044011228     c                   if        strdtfival <> *hival
025045011228     c                   move      strdtfival    data
025046011228     c                   eval      com6 = gg + mm + aa
025047011228     c                   move      com6          dtaalv
025048011228     c                   end
025049011228     c                   exsr      ctr01
025050011228     C                   end
025051011228     C                   ENDSR
025052011228     c**********************************************************************
025053011228     c* pulisce 1° formato
025054011228     c**********************************************************************
025055011127     C     clr00         BEGSR
025056011127     C                   Z-ADD     0             §RIGA
025057011127     C                   Z-ADD     0             §COLON
025058011127     C                   CLEAR                   §FLD
025059011127     C                   ENDSR
025060011127     C******************************************************
025061011127     C* CONTROLLI PRIMO VIDEO
025062011127     C******************************************************
025063011127     C     CTR01         BEGSR
025064011228     c                   setoff                                       99
025065011228     c                   if        mod <> '4 '
025070011228     c* raggruppamento obbligatorio
025071011228     c     codragv       ifeq      *blanks
025072011228     c                   seton                                        0299
025074011228     c                   end
025075011228     c* descriz.lunga obbligatoria
025076011228     c     deslunv       ifeq      *blanks
025077011228     c                   seton                                        0399
025079011228     c                   end
025080011228     c* livello riservatezza
025081011228     c                   if        livrisv < '0' or livrisv > '9'
025082011228     c                   seton                                        0499
025084011228     c                   end
025085011228     c* regola abilitazione da tabella G93
025086020103     c                   movel     *blanks       desabiv
025087011228     c                   if        regabiv = *blanks
025088011228     c                   seton                                        0599
025089011228     c                   else
025090020107     c                   exsr      srregabi
025112011228     c                   end
025113011127     C* CONTROLLO DATE
025114011127     C                   MOVEL     *LOVAL        DTADAL
025115011228     C                   IF        dtadalv <> 0
025116011228     C     *dmy          test(d)                 dtadalv                06
025117011228     c                   if        *in06
025118020103     C                   SETON                                        99
025119020103     c                   else
025121011228     C     *dmy          MOVE      dtadalv       DTAEUR
025123011127     C                   MOVEL     DTAEUR        DTADAL
025124020103     C                   END
025125011127     C                   END
025126011127     C* CONTROLLO AL
025127011228     C                   MOVEL     *hival        DTAAL
025128020103     C                   IF        dtaALv <> 999999
025129011228     C     *dmy          test(d)                 dtaalv                 07
025130011228     c                   if        *in07
025131011127     C                   SETON                                        99
025132020103     c                   else
025133011228     C     *dmy          MOVE      dtaalv        DTAEUR
025134011127     C                   MOVEL     DTAEUR        DTAAL
025135020103     C                   END
025136011127     C                   END
025137011127     C*
025138020103     C                   IF        (dtaDALv <> 0 OR dtaALv <> 999999) AND
025139011127     C                              DTADAL > DTAAL
025140011228     C                   SETON                                        060799
025141011127     C                   END
025142011228     C                   END
025143011127     C*
025144011127     C                   ENDSR
025145011127     C******************************************************
025146020107     C* regola abilitazione
025147011127     C******************************************************
025148020107     C     srregabi      BEGSR
025149020107     C*
025150020107     C                   CLEAR                   angG93ds
025151020107     C                   MOVEL     XSCLIN        XTBLIN
025152020107     C                   MOVEL     XSCSOC        XTBAZI
025153020107     C                   MOVEL     XSCCUT        XTBUTE
025154020107      *
025155020107     C                   MOVE      'G93'         XTBcod
025156020107     c                   move      *all'0'       Xtbkey
025157020107     C                   MOVE      REGABIV       XTBKEY
025158020107     C                   MOVE      '1'           XTBRIC
025159020107     C                   MOVE      '0'           XTBALC
025160020107     C                   MOVE      '0'           XTBERR
025161020107     C                   CALLb     'XATB'
025162020107     C                   PARM                    XATBDS
025163020107     C                   MOVE      *BLANK        XTBALC
025164020107     C     XTBERR        IFNE      '0'
025165020107     C                   SETON                                        0599
025166020107     C                   ELSE
025167020107     C                   MOVEL     XTBUNI        angG93ds
025168020107     C                   MOVEL     desg93        DESABIV
025169020107     C                   ENDIF
025170020107     C*
025171020107     C                   ENDSR
025172020107     C******************************************************
025173020107     C* RICERCA PRIMO VIDEO
025174020107     C******************************************************
025175020107     C     RIC01         BEGSR
025176011130     c                   select
025177011228     C*regola abilitazione
025178011228     C                   when      §fld = 'REGABIV'
025179011228     C                   RESET                   XTABDS
025180011228     C                   MOVE      '01'          XTAOPZ
025181011228     C                   MOVE      XSCSOC        XTAAZI
025182011228     C                   MOVE      XSCLIN        XTALIN
025183011228     C                   Z-ADD     1             XTARIG
025184011228     C                   Z-ADD     1             XTACOL
025185011228     C                   CLEAR                   KPJBU
025186011228     C                   MOVEL     XTABDS        KPJBU
025187011228     C                   CALL      'ANGG93R'
025188011228     C                   PARM                    KPJBA
025189011228     C                   MOVEL     KPJBU         XTABDS
025190011228     C     XTAERR        IFNE      '1'
025191011228     C     XTAKEY        ANDNE     *BLANK
025192011228     C                   MOVE      XtaKey        regabiv
025193011228     C                   ENDIF
025194011130     C                   ENDsl
025195011130     C* POSIZIONAMENTO CURSORE
025196011130     C                   Z-ADD     §RIGA         §RIG
025197011130     C                   Z-ADD     §COLON        §COL
025198011127     C                   ENDSR
025199011228     c**********************************************************************
025200011228     c* AGGIORNA ARCHIVIO
025201011228     c**********************************************************************
025202011228     C     srAGGIO       BEGSR
025203011228     C                   SETOFF                                       41
025204011228     c                   seton                                        90
025205011228     c                   if        mod <> '3 '
025206011228     C     nrr           chain     ynstr00f                           90
025207011228     c                   end
025208020103     c                   movel     xscsoc        strsocieta
025209011228     c                   movel     CODRAGV       strscg
025210011228     c                   movel     DESLUNV       strdescr
025211011228     C                   IF        DESBREV = *BLANKS
025212011228     c                   movel     DESLUNV       strdesbrev
025213011228     C                   ELSE
025214011228     c                   movel     DESBREV       strdesbrev
025215011228     C                   END
025216011228     c                   movel     livrisv       strlivrise
025217011228     c                   movel     regabiv       strrgabent
025218011228     c                   move      dtadal        strdtinval
025219011228     c                   move      dtaal         strdtfival
025220011228     c                   if        mod <> '4 '
025221011228     c  n90              update    ynstr000                             41
025222011228     c   90              write     ynstr000                             41
025223011228     c                   else
025224011228     c  n90              delete    ynstr000
025225011228     c                   end
025226011228     C                   ENDSR
046501011127     C******************************************************
046800000000     C** PULIZIA CAMPI DS
046801011127     C******************************************************
046901011127     C     AZZER         BEGSR
047300011228     C                   MOVEL     *BLANKS       codragv
047400011228     C                   MOVEL     *BLANKS       deslunv
047700011228     C                   MOVEL     *BLANKS       desbrev
047800011228     C                   MOVEL     '0'           livrisv
047900020107     C                   MOVEL     '01'          regabiv
048000011228     C                   MOVEL     *BLANKS       desabiv
048001020107     C                   exsr      srregabi
048700011228     C                   Z-ADD     0             dtadalv
048800020103     C                   Z-ADD     999999        dtaalv
048900000000     C                   ENDSR
054102011127     C******************************************************
054103011127     C     *INZSR        BEGSR
054104011127     C******************************************************
054144011127     C*
054145011127     C     *ENTRY        PLIST                                                  *
054146011228     C                   PARM                    nrr              15 0          *
054147011228     C                   PARM                    mod               2            *
054148011228     C                   PARM                    cmd               1            *
054149011228     c     mod           comp      '5 '                                   40
054150020103     c                   select
054151020103     c                   when      mod = '1 '
054152020103     c                   eval      modv = 'INSERIMENTO'
054153020103     c                   when      mod = '2 '
054154020103     c                   eval      modv = 'MODIFICA'
054155020103     c                   when      mod = '3 '
054156020103     c                   eval      modv = 'COPIA'
054157020103     c                   when      mod = '4 '
054158020103     c                   eval      modv = 'CANCELLA'
054159020103     c                   when      mod = '5 '
054160020103     c                   eval      modv = 'VISUALIZZAZIONE'
054161020103     c                   endsl
054162011228      * Reperimento dati società
054163011228     C                   clear                   SocXsc
054164011228     C                   eval      tipxsc = 'SOC001'
054165011228     C                   exsr      rtvsocieta
054166011228     C                   if        rtnxsc = '1'
054167011228     C                   seton                                        lr
054168011228     C                   else
054169011228     C                   MOVEL     XSOCDS        xsoc001ds
054170011228     C                   endif
054171011127     C                   ENDSR
054200011228     C*---------------------------------------------------------------------
054300011228     c*  Reperisco società.
054400011228     C*---------------------------------------------------------------------
054500011228     C     rtvsocieta    begsr
054600011228     C*
054700011228     C                   CALLb     'XSOC'
054800011228   >>C                   Parm                    TipXsc            6
054900011228   >>C                   Parm                    SocXsc            3
055000011228   >>C                   Parm                    CdsXsc            9 0
055100011228   >>C                   Parm                    ModXsc            3
055200011228   >>C                   Parm      *BLANK        RtnXsc            1
055300011228     C                   Parm                    XsocDS
055400011228   >>C                   Parm                    Kpjba
055500011228     C*
055600011228     C                   endsr
