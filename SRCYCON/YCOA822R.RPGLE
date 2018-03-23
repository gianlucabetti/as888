000001011228     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND')
000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101011127     H*--------------------------------------------------------------*
000400020103     H*             - parametro modulo -                             *
000500000000     H*--------------------------------------------------------------*
000600011228     Fycoa822d  CF   E             WORKSTN
000900020103     Fynsta00f  UF a E             DISK
007701011127      *
007702011228     D KPJBA         E DS
007703011228     D* Richiamo a XATB
007704011228     D angg80ds      E DS
007705011228     D XATBDS        E DS
007706011228     D XTabDS        E DS                  inz
007707011228    >D xsoc001ds     E DS
007708020108     D xchkautds     e ds                  inz
007709020102    >D ynsta         E DS                  extname(ynsta00f)
007710020102     D Ptr             S               *
007711020102     D                                     INZ(%ADDR(statprif1))
007712020102     D fent            S              3
007713020102     D                                     DIM(8)
007714020102     D                                     BASED(Ptr)
007717011228     D* DS Interna per dati di output di XSOC
007718020102     D yy              S              2  0
007720011127     D DTAiso          S               D   INZ
007721011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
007722011127     D DTADAL          S               D   INZ
007723011127     D DTAAL           S               D   INZ
007724011228     d data            ds            10
007725011228     d aa                      3      4
007726011228     d mm                      6      7
007727011228     d gg                      9     10
007728011228     D com6            S              6
007729020102     D ent             S              3
007730020102     d sent            s              3    inz dim(8)
007732020102     d vent            s              3    inz dim(8)
007733020102     d vdes            s             20    inz dim(8)
007734020102     ir01
007735020102     i              ent1v                       vent(1)
007736020102     i              ent2v                       vent(2)
007737020102     i              ent3v                       vent(3)
007738020102     i              ent4v                       vent(4)
007739020102     i              ent5v                       vent(5)
007740020102     i              ent6v                       vent(6)
007741020102     i              ent7v                       vent(7)
007742020102     i              ent8v                       vent(8)
007743020102     i              desent1                     vdes(1)
007744020102     i              desent2                     vdes(2)
007745020102     i              desent3                     vdes(3)
007746020102     i              desent4                     vdes(4)
007747020102     i              desent5                     vdes(5)
007748020102     i              desent6                     vdes(6)
007749020102     i              desent7                     vdes(7)
007750020102     i              desent8                     vdes(8)
013400000000     C*----------------------------------------------------*
013702011228     c                   exsr      srcarica
013704011127     C*
013705020108     C                   DO        *HIVAL
013706011127     c                   EXSR      CLR00
013709011228     c                   if        *in99
013710011228     c                   setoff                                       99
013711011228     c                   write     r01
013712011228     c                   seton                                        99
013713011228     c                   end
013720011228     C                   EXFMT     R01
014000000000     C                   SETOFF                                       99
014100011127     C* FINE LAVORO
014101011228     C                   IF        *INKC
014200011228     C                   LEAVE
014201011228     c                   end
014202011228     C* GUIDA
014203011228     C                   IF        *INKL
014205011228     C                   LEAVE
014206011228     c                   end
014207020128     C* ripristino
014208020128     C                   IF        *INKE
014209020128     c                   exsr      srcarica
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
014311011228     C                   LEAVE
014312011228     c                   end
014313011228     c                   enddo
024902011127     c*
025000000000     C                   SETON                                        LR
025023011127     c**********************************************************************
025024011228     c* carica 1° formato
025025011127     c**********************************************************************
025026011228     C     srcarica      BEGSR
025027020128     C     1             setll     ynsta00f
025028020128     C                   read      ynsta00f                               81
025029020103     c                   z-add     999999        dtaalv
025030020103     c                   z-add     0             dtadalv
025031020103     c                   movel     'CDC'         vent(1)
025032020103     C                   if        not *in81
025033020102     c                   movea     fent          vent
025037011228     c                   move      staann        annov
025038011228     c                   move      stames        mesev
025039011228     c                   if        stadmd <> *loval
025040011228     c                   move      stadmd        data
025041011228     c                   eval      com6 = gg + mm + aa
025042011228     c                   move      com6          dtadalv
025043011228     c                   else
025045011228     c                   end
025047011228     c                   if        stadma <> *hival
025048011228     c                   move      stadma        data
025049011228     c                   eval      com6 = gg + mm + aa
025050011228     c                   move      com6          dtaalv
025051011228     c                   end
025052011228     c                   exsr      ctr01
025054011228     C                   end
025055011228     C                   ENDSR
025056011228     c**********************************************************************
025057011228     c* pulisce 1° formato
025058011228     c**********************************************************************
025059011127     C     clr00         BEGSR
025060011127     C                   Z-ADD     0             §RIGA
025061011127     C                   Z-ADD     0             §COLON
025062011127     C                   CLEAR                   §FLD
025063011127     C                   ENDSR
025064011127     C******************************************************
025065011127     C* CONTROLLI PRIMO VIDEO
025066011127     C******************************************************
025067011127     C     CTR01         BEGSR
025068011228     c                   setoff                                       99
025069011228     C* CONTROLLO DATE
025070011228     C                   MOVEL     *LOVAL        DTADAL
025071020103     c                   if        dtadalv <> 0
025072011228     C     *dmy          test(d)                 dtadalv                09
025073011228     c                   if        *in09
025074011228     C                   SETON                                        99
025075020103     c                   else
025077011228     C     *dmy          MOVE      dtadalv       DTAEUR
025078011228     C                   MOVEL     DTAEUR        DTADAL
025079020103     C                   END
025080020103     C                   END
025081011228     C* CONTROLLO AL
025082011228     C                   MOVEL     *hival        DTAAL
025083020103     c                   if        dtaalv <> 999999
025084011228     C     *dmy          test(d)                 dtaalv                 10
025085011228     c                   if        *in10
025086011228     C                   SETON                                        99
025087020103     c                   else
025089011228     C     *dmy          MOVE      dtaalv        DTAEUR
025090011228     C                   MOVEL     DTAEUR        DTAAL
025091020103     C                   END
025092020103     c                   end
025093011228     C*
025094020103     C                   IF        (dtaDALv <> 0 OR dtaALv <> 999999) AND
025095011228     C                              DTADAL > DTAAL
025096011228     C                   SETON                                        091099
025097011228     C                   END
025098011228     c* anno
025099020103     c                   if        annov < 2002
025100011228     c                   seton                                        1199
025101011228     C                   END
025102011228     c* mese
025103011228     c                   if        mesev = 0
025104011228     c                   seton                                        1299
025107011228     C                   END
025108011228     c                   if        mesev > 12
025109011228     c                   seton                                        1299
025111011228     C                   END
025112020102     c* tipo entità
025113020102     c                   clear                   vdes
025114020102     c                   do        8             xx                2 0
025115020102     c                   if        vent(1) = *blanks
025116011228     c                   seton                                        0199
025118020102     c                   end
025119020102     c                   if        vent(xx) <> *blanks
025120020102     c                   movel     vent(xx)      ent
025121011228     c                   exsr      srent
025122020102     C                   MOVEL     desg80        vdes(xx)
025123020128     c                   if        xtberr <> '0'
025124020102     c                   movel     '1'           *in(xx)
025126011228     c                   end
025127020102     c                   end
025128020102     C                   ENDdo
025130020102     c* controllo che non ci siano entità duplicate
025131020102     c                   clear                   sent
025133020102     c                   do        8             xx
025135020102     c                   if        vent(xx) <> *blanks
025137020102     c     vent(xx)      lookup    sent                                   70
025138020102     c                   if        *in70
025139020102     c                   seton                                        99
025140020102     c                   move      '1'           *in(xx)
025141020102     c                   leave
025142020102     c                   else
025143020102     c                   z-add     1             yy
025144020102     c     '   '         lookup    sent(yy)                               70
025145020102     c                   if        *in70
025146020102     c                   movel     vent(xx)      sent(yy)
025147020102     c                   end
025148020102     c                   end
025149020102     c                   end
025214011127     C                   ENDDO
025215011127     C*
025216011127     C                   ENDSR
025217011127     C******************************************************
025218011228     C* entità
025219011127     C******************************************************
025220011228     C     srent         BEGSR
025221011228     C                   CLEAR                   angG80ds
025222011228     C                   MOVEL     XSCLIN        XTBLIN
025223011228     C                   MOVEL     XSCSOC        XTBAZI
025224011228     C                   MOVEL     XSCCUT        XTBUTE
025225011228      *
025226011228     C                   MOVE      'G80'         XTBcod
025227011228     c                   move      *all'0'       Xtbkey
025228011228     C                   MOVE      ent           XTBKEY
025229011228     C                   MOVE      '1'           XTBRIC
025230011228     C                   MOVE      '0'           XTBALC
025231011228     C                   MOVE      '0'           XTBERR
025232011228     C                   CALLb     'XATB'
025233011228     C                   PARM                    XATBDS
025234011228     C                   MOVE      *BLANK        XTBALC
025235011228     C     XTBERR        IFNE      '0'
025236011228     C                   clear                   angG80ds
025237011228     C                   SETON                                        99
025239011228     C                   ELSE
025240011228     C                   MOVEL     XTBUNI        angG80ds
025241011228     c                   end
025242011228     C                   ENDSR
025243011228     C******************************************************
025244011228     C* RICERCA PRIMO VIDEO
025245011228     C******************************************************
025246011228     C     RIC01         BEGSR
025247020102     c                   movea     vent          sent
025248011130     c                   select
025259011228     C*entità
025260011228     C                   when      §fld = 'ENT1V'
025261020102     c                   movel     vent(1)       ent
025262011228     c                   exsr      ricent
025264020102     c                   movel     ent           vent(1)
025265011228     C                   when      §fld = 'ENT2V'
025266020102     c                   movel     vent(2)       ent
025267011228     c                   exsr      ricent
025268020102     c                   movel     ent           vent(2)
025269011228     C                   when      §fld = 'ENT3V'
025270020102     c                   movel     vent(3)       ent
025271011228     c                   exsr      ricent
025272020102     c                   movel     ent           vent(3)
025273011228     C                   when      §fld = 'ENT4V'
025274020102     c                   movel     vent(4)       ent
025275011228     c                   exsr      ricent
025276020102     c                   movel     ent           vent(4)
025277011228     C                   when      §fld = 'ENT5V'
025278020102     c                   movel     vent(5)       ent
025279011228     c                   exsr      ricent
025280020102     c                   movel     ent           vent(5)
025281011228     C                   when      §fld = 'ENT6V'
025282020102     c                   movel     vent(6)       ent
025283011228     c                   exsr      ricent
025284020102     c                   movel     ent           vent(6)
025285011228     C                   when      §fld = 'ENT7V'
025286020102     c                   movel     vent(7)       ent
025287011228     c                   exsr      ricent
025288020102     c                   movel     ent           vent(7)
025289011228     C                   when      §fld = 'ENT8V'
025290020102     c                   movel     vent(8)       ent
025291011228     c                   exsr      ricent
025292020102     c                   movel     ent           vent(8)
025293011130     C                   ENDsl
025294011130     C* POSIZIONAMENTO CURSORE
025295011130     C                   Z-ADD     §RIGA         §RIG
025296011130     C                   Z-ADD     §COLON        §COL
025297011127     C                   ENDSR
025298011228     c**********************************************************************
025299011228     c* ricerca entità
025300011228     c**********************************************************************
025301011228     C     ricent        BEGSR
025302011228     C                   RESET                   XTABDS
025303011228     C                   MOVE      '01'          XTAOPZ
025304011228     C                   MOVE      XSCSOC        XTAAZI
025305011228     C                   MOVE      XSCLIN        XTALIN
025306011228     C                   Z-ADD     1             XTARIG
025307011228     C                   Z-ADD     1             XTACOL
025308011228     C                   CLEAR                   KPJBU
025309011228     C                   MOVEL     XTABDS        KPJBU
025310011228     C                   CALL      'ANGG80R'
025311011228     C                   PARM                    KPJBA
025312011228     C                   MOVEL     KPJBU         XTABDS
025313011228     C     XTAERR        IFNE      '1'
025314011228     C     XTAKEY        ANDNE     *BLANK
025315011228     C                   MOVE      XtaKey        ent
025316011228     C                   ENDIF
025317011228     C                   ENDSR
025318011228     c**********************************************************************
025319011228     c* AGGIORNA ARCHIVIO
025320011228     c**********************************************************************
025321011228     C     srAGGIO       BEGSR
025322020103     c     1             setll     ynsta00f
025323020103     C                   read      ynsta00f                               81
025324020102     c                   movea     vent          fent
025328011228     c                   move      dtadal        stadmd
025329011228     c                   move      dtaal         stadma
025330011228     c                   move      annov         staann
025331011228     c                   move      mesev         stames
025332020103     c                   if        not *in81
025333011228     c                   update    ynsta000
025334020103     c                   else
025335020103     c                   write     ynsta000
025336011228     c                   end
025337011228     C                   ENDSR
054102011127     C******************************************************
054103011127     C     *INZSR        BEGSR
054104011127     C******************************************************
054105020104      *
054106020104     C     *ENTRY        PLIST
054107020104     C                   PARM                    Kpjba
054150011228      * Reperimento dati società
054151011228     C                   clear                   SocXsc
054152011228     C                   eval      tipxsc = 'SOC001'
054153011228     C                   exsr      rtvsocieta
054154020108     C                   IF        RtnXsc = '1'
054156020108     C                   exsr      uscita
054159011228     C                   endif
054160020108     ** Inserire qui un eventuale controllo utente autorizzato.
054161020108     C                   CLEAR                   XChkAutDS
054162020108     C                   EVAL      XcaVfu = 'BASE'
054163020108     C                   EXSR      ChkAut
054164020108     C                   IF        XcaErr = '1'
054165020108     C                   exsr      uscita
054166020108     C                   ENDIF
054167011127     C                   ENDSR
054168020108     C******************************************************
054169020108     c*      Reperisco autorizzazioni per l'uso del pgm
054170020108     C     chkaut        BEGSR
054171020108     C******************************************************
054172020108     C*
054173020108     C                   EVAL      XcaSoc = XscSoc
054174020108     C                   EVAL      XcaGrp = XscGrp
054175020108     C                   EVAL      XcaFnc = 'YSTIME'
054176020108     C                   EVAL      XcaTck = 'CK'
054177020108     C                   EVAL      XcaCut = XscCut
054178020108     C                   EVAL      XcaNau = '1'
054179020108      *
054180020108     C                   CALLB     'XCHKAUT'
054181020108     C                   PARM                    XChkAutDS
054182020108     C*                                                    *
054183020108     C                   ENDSR
054200011228     C*---------------------------------------------------------------------
054300011228     c*  Reperisco società.
054400011228     C*---------------------------------------------------------------------
054500011228     C     rtvsocieta    begsr
054601020108     ** Mi collego ad una società.
054602020108     C                   CALLB     'XSOC'
054603020108   >>C                   PARM      'SOC001'      TipXsc            6
054604020108   >>C                   PARM      *BLANK        SocXsc            3
054605020108   >>C                   PARM      *ZERO         CdsXsc            9 0
054606020108   >>C                   PARM      *BLANK        ModXsc            3
054607020108   >>C                   PARM      *BLANK        RtnXsc            1
054608020108     C                   PARM                    XSoc001DS
054609020108   >>C                   PARM                    Kpjba
055500011228     C*
055600011228     C                   endsr
055700020108     ***********************************************************************
055900020108     ** Uscita dal programma.
056100020108     ***********************************************************************
056200020108     C     Uscita        BEGSR
056300020108
056400020108     C                   EVAL      *INLR = *ON
056500020108     C                   RETURN
056600020108
056700020108     C                   ENDSR
