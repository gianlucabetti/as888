000100011221      /TITLE Stime per conto economico.
000200011221
000300011221     H DFTACTGRP(*NO) BNDDIR('PJXBND')
000400930111     H DECEDIT('0,') DATEDIT(*DMY.)
000500011221
000600011221     FAnEnt01L  IF   E           K DISK
000700011219     FYnStR01L  IF   E           K DISK
000800011219     FYnStA00F  IF   E           K DISK
000900011221     F                                     USROPN
001000011219     FYnStI00F  O    E             DISK
001100011219     FYnStI01L  UF   E           K DISK
001200011221     F                                     RENAME(YnSti000:YnSti001)
001300011219     FYnStI02L  IF   E           K DISK
001400011221     F                                     RENAME(YnSti000:YnSti002)
001500011219     FYcoA810D  CF   E             WORKSTN
001600921223     F                                     SFILE(CA81SF2:SF2NRR)
001700011221
001800011221     D                SDS
001900011221     D PgmNam            *PROC
002001020107
002002020221     D ystimeds      e ds                  inz
002003020107
002004020107     D xchkautds     e ds                  inz
002005020107
002006020107     D XAUTUNIDS     e ds                  inz
002007020107
002100011219     D XSoc001DS     E DS
002200011219     D                                     INZ
002300011221     D Ana032DS      E DS
002400011221     D                                     INZ
002500011221     D YCOA81        E DS                  EXTNAME(YCOA810DS)
002600011221
002700891030     D KPJBA         E DS
002800921223     D  MSGERR               453    502
003700020107     D DatEu           s               D
003800020107     D WrkDtInMese     s               D
003900020107     D WrkDtFiMese     s               D
004000020107     D WrkDataChar     s             10
004100011219
004200921222     C                   EXSR      DEFCAM
004300921222     C     *INKC         DOWEQ     '0'
004400011221
004500011221     C                   IF        *IN99
004600011221     C                   EVAL      *IN99 = *OFF
004700011221     C                   WRITE     CA81D1
004800011221     C                   EVAL      *IN99 = *ON
004900011221     C                   ENDIF
005000011221
005100921222     C                   EXFMT     CA81D1
005200011221
005300011221     C                   IF        NOT *INKC
005400011221
005500011221     ** F4=Richiesta.
005600011221     C                   IF        *INKD
005700011221     C                   EXSR      F4Richiesta
005800011221     C                   ITER
005900011221     C                   ENDIF
006000011221
006100921222     C                   EXSR      CTRD1
006200011221
006300011221     ** F2=Decodifica.
006400011221     C                   IF        *INKB
006500011221     C                   ITER
006600011221     C                   ENDIF
007300980115     C* CARICO SUBFILE
007400921222     C  N99              DO
007500921223     C                   EXSR      CARSF2
007600930108     C                   SETOFF                                       83
007700921222     C     *INKF         DOWEQ     '0'
007800921222     C     *INKL         ANDEQ     '0'
007900930108     C     *IN49         ANDEQ     '0'
008000930108     C     *IN97         OREQ      '1'
008100930108     C                   WRITE     CA81D3
008200921222     C                   EXFMT     CA81CT2
008300011221
008400011221     C                   IF        *INKC
008500011221     C                   EXSR      Uscita
008600011221     C                   ENDIF
008601020128     c* ripristino
008602020128     C                   IF        *INKE
008603020128     C                   EXSR      carsf2
008604020128     c                   iter
008605020128     C                   ENDIF
008700011221
008800930111     C   KL              SETOFF                                       97
008900921222     C  NKL              EXSR      CTRSF2
009000960708     C  N98
009100960708     CAN KF
009200960708     CANN97              EXSR      AGGIOR
009300921222     C                   END
009400921222     C                   END
009500921222     C                   END
009600921222     C*
009700921222     C                   END
009800011221
009900921222     C     FINE          TAG
010000011221
010100921222     C                   SETON                                        LR
010200921222     C**************************************************************************
010300921222     C*----------
010400921222     C* CONTROLLI VIDEO1
010500921222     C*----------
010600921223     C     CTRD1         BEGSR                                                  *
010700921224     C                   SETOFF                                       99
011900921224     C*
012000011221     C     VIDANN        IFEQ      0
012100011221     C     VIDMES        OREQ      0
012200921223     C                   SETON                                        12
012300011221     C                   ELSE
012400011221     ** Determino il giorno iniziale e finale del mese.
012500011221     C                   EVAL      WrkDataChar = %EDITC(VidAnn:'X')
012600011221     C                             + '-' + %EDITC(VidMes:'X') + '-01'
012700011221     C     *ISO          MOVE      WrkDataChar   WrkDtInMese
012800011221     C     WrkDtInMese   ADDDUR    1:*M          WrkDtFiMese
012900011221     C                   SUBDUR    1:*D          WrkDtFiMese
013000921223     C                   END
013100921223     C*----------
013200921223     C* VERIFICO SE IMMESSO C.D.C. VALIDO
013300921223     C*----------
013400011221     C                   CLEAR                   VIDDES
013500011221     C     VIDCDC        IFNE      *BLANK
013600011221     C                   MOVEL     'CDC'         ENTTPRIFAN
013700011221     C                   MOVEL     VIDCDC        EntEntita
013800011221     C     KACO          CHAIN     AnEnt01L                           13
013900011221     C                   IF        NOT *IN13
014000011221     C                   MOVEL     EntDesBrev    VIDDES
014100011221     C                   IF        EntDtFin = *LOVAL
014200011221     C                   EVAL      EntDtFin = *HIVAL
014300011221     C                   ENDIF
014400011221     C                   IF        (WrkDtInMese >= EntDtIn
014500011221     C                              AND
014600011221     C                              WrkDtInMese <= EntDtFin)
014700011221     C                             OR
014800011221     C                             (WrkDtFiMese >= EntDtIn
014900011221     C                              AND
015000011221     C                              WrkDtFiMese <= EntDtFin)
015100011221     C                   ELSE
015200011221     C                   SETON                                        13
015300011221     C                   END
015400011221     C                   ENDIF
015500921223     C                   ELSE
015600921223     C                   SETON                                        13
015700921223     C                   END
015800921223     C*----------
015900020104     C* CONTROLLO SE L'UTENTE E' ABILITATO AD USARE IL CDC
016300921223     C*----------
016301020104     C                   CLEAR                   XAUTUNIDS
016302020104     C                   MOVEL     KNMUS         XAUPRF
016303020104     C                   MOVEL     XSCSOC        XAUSOC
016304020104     C                   MOVEL     VIDCDC        XAUUNI
016305020104     C                   CALL      'XAUTUNI'
016306020104     C                   PARM                    XAUTUNIDS
016307020104     C                   IF        XAUABL = '0'
016308020104     C                   SETON                                        15
016309020104     C                   ELSE
016310020104     C                   SETOFF                                       15
016311020104     C                   END
017100921223     C*----------
017300020107     C* ACCENDO INDICATORE DI PROTEZIONE CAMPI
017400020107     C* SE IL MESE CHE SI MANUTENZIONA NON E' QUELLO
017500930111     C* IN TABELLA OPPURE SE IL MESE E' QUELLO GIUSTO MA IL PERIODO
017600930111     C* NON E' COMPRESO TRA LE DUE DATE IMMESSE IN TABELLA
017601020108     C                   SETOFF                                       98
017602020108     C     VIDANN        IFNE      StaAnn
017603020108     C     VIDMES        ORNE      StaMes
017604020108     C     DATEU         ORLT      StaDmd
017605020108     C     DATEU         ORGT      StaDma
017606020108     c* Per alcuni utenti (es: controllo di gestione) è sempre
017607020108     c* possibile la manutenzione
017803020108     C                   CLEAR                   XChkAutDS
017804020108     C                   EVAL      XcaVfu = 'FUORIPERIODO'
017805020221     C                   EVAL      XcaFnc = 'YSTIME'
017806020108     C                   EXSR      ChkAut
017807020108     C                   IF        xcaerr = '1'
018600930111     C                   SETON                                        98
018601020128     C                   EVAL      VidOperaz = 'Visualizza'
018700921223     C                   END
018701020108     C                   END
018900921223     C*
019000020104     C   12
019200921224     COR 13
019400921224     COR 15              SETON                                        99
019500011221     C*
019600921223     C                   ENDSR
019700011221
019800011221     C*----------
019900011221     C* RICERCA E SCELTA CENTRO DI COSTO
020000921223     C*----------
020100011221     C     F4Richiesta   BEGSR
020200011221
020300011221     C                   SELECT
020400011221
020500011221     C                   WHEN      RTNCSRFLD = 'VIDCDC'
020600011221     C                   CLEAR                   Ana032DS
020700011221     C                   EVAL      Opz032 = '01'
020800011221     C                   EVAL      Soc032 = XscSoc
020900011221     C                   EVAL      TpE032 = 'CDC'
021000020128     C                   EVAL      FSC032 = 'S'
021100011221     C                   EVAL      Kpjbu = Ana032DS
021200011221     C                   CALL      'ANA032R'
021300011221     C                   PARM                    Kpjba
021400011221     C                   EVAL      Ana032DS = Kpjbu
021500011221     C                   IF        Ret032 = *BLANK
021600011221     C                   EVAL      VidCdc = Ent032
021700011221     C                   EVAL      VidDes = Des032
021800011221     C                   ENDIF
021900011221
022000011221     C                   ENDSL
022100011221
022200921223     C                   ENDSR
022300011221
022400930111     C*----------
022500020107     C* AGGIORNAMENTO STIME
022600930111     C*----------
022700921223     C     AGGIOR        BEGSR
022800930112     C                   DO        TOTSF2        X
022900930108     C     X             CHAIN     CA81SF2                            86
023000930108     C  N86              DO
023100930108     C* SE LA STIMA E' GIA' PRESENTE IN ARCHIVIO LA AGGIORNO ALTRIMENTI LA
023200011219     C* AGGIUNGO SU YnStI00F
023500020107     C                   MOVEL     SF2RAG        StrSCG
023700011219     C     KAST01        CHAIN     YnStI01L                           85
023800930111     C* AGGIORNO I CAMPI DEL FILE
023900011221     C                   MOVEL     'CDC'         STITPRIF1
024000011221     C                   MOVEL     SF2RAG        STISCG
024100930111     C                   Z-ADD     VIDANN        STIANN
024200930111     C                   Z-ADD     VIDMES        STIMES
024300930111     C                   Z-ADD     SF2ITO        STIITO
024400930111     C                   MOVEL     SF2STO        STISTO
024500930113     C* SE IL RECORD ESISTEVA GIA' ED E' STATO MODIFICATO AGGIORNO
024600930113     C* PROFILO UTENTE E AZZERO DATI TRASMISSIONE
024700930113     C  N85H2ITO         IFNE      SF2ITO
024800930113     C     H2STO         ORNE      SF2STO
024900020204     C                   time                    STIdtimm
024901020204     C                   MOVEL     KNMUS         STIPRF
025000011221     C                   UPDATE    YnSti001
025100930113     C                   ELSE
025200011221     C                   UNLOCK    YnSti01L
025300930113     C                   END
025400930113     C   85              DO
025401020204     C                   time                    STIdtimm
025500930113     C                   MOVEL     KNMUS         STIPRF
025600011221     C                   WRITE     YnSti000
025700930113     C                   END
025800930113     C*
025900930111     C                   END
026000930111     C                   END
026100930111     C*
026200921223     C                   ENDSR
026300930108     C*----------
026400930108     C* CARICAMENTO SUBFILE RAGGR. DI BILANCIO * CA81SF2 *
026500930108     C*----------
026600921223     C     CARSF2        BEGSR
026603020104     C*
026700930108     C                   SETON                                        70
026800930108     C                   Z-ADD     0             SF2NRR
026900930108     C                   WRITE     CA81CT2
027000930108     C                   SETOFF                                       7086
027100930108     C* CONTROLLO SE SONO GIA' STATI MEMORIZZATI I RAGGRUPPAMENTI PER QUEL MESE
027200011219     C                   Z-ADD     VIDANN        stiann
027300011219     C                   Z-ADD     VIDMES        stimes
027400011219     C                   MOVE      VIDCDC        stisdc
027500011219     C     KASTI         SETLL     YnStI02L                               87
027501020128     c* solo se sono abilitato
027600020128     C     *IN98         IFeq      '0'
027601020128     C     *IN87         IFEQ      '1'
027700011221     C                   EVAL      VidOperaz = ' Modifica '
027701020107     c                   else
027702020107     C                   EVAL      VidOperaz = 'Immissione'
027703020107     c                   end
027704020128     c                   end
029800020107     C*
030300020107     C     xscsoc        SETLL     YnStR01L
030400020107     C                   DO        *hival
030500020107     C     xscsoc        READE     YnStR01L                               86
030700020107     C   86              leave
030701020107     c* controllo il livello di riservatezza
030702020221     c                   if        strlivrise > stiliv
030703020107     c                   iter
030704020107     c                   end
030705020107     c* controllo le date  validità
031100020107     C                   if         WrkDtInMese <= StrDtFiVal and
031300020107     C                              WrkDtFiMese >= StrDtInVal
031600020107     c* controllo se esiste già la stima
031601020107     C     KAST01        chain     YnStI01L
031602020107     c                   if        %found(ynsti01l)
031616020107     C                   Z-ADD     STIITO        SF2ITO
031617020107     C                   Z-ADD     STIITO        H2ITO
031618020107     C                   MOVEL     STISTO        SF2STO
031619020107     C                   MOVEL     STISTO        H2STO
031620020107     c                   else
031900930108     C                   Z-ADD     0             SF2ITO
031901020107     C                   Z-ADD     0             h2ito
032000930108     C                   MOVEL     *BLANKS       SF2STO
032001020107     C                   MOVEL     *BLANKS       h2STO
032002020107     c                   endif
032003020107     C                   MOVEL     StrScg        SF2RAG
032004020107     C                   MOVEL     STRDESCR      SF2DES
032100930108     C                   ADD       1             SF2NRR
032200930108     C                   WRITE     CA81SF2
032400020107     C                   endif
033000020107     C                   enddo
033100930108     C*
033200930108     C     SF2NRR        COMP      0                                      49
033300930112     C                   Z-ADD     SF2NRR        TOTSF2            4 0
033400930114     C                   Z-ADD     1             HKEY
033500921223     C                   ENDSR
033600930108     C*----------
033700930108     C* CONTROLLI SUBFILE RAGGR. DI BILANCIO * CA81SF2 *
033800930108     C*----------
033900921223     C     CTRSF2        BEGSR
034000020128     C                   SETOFF                                       9790
034100930111     C  NKF              SETOFF                                       83
034200930112     C                   DO        TOTSF2        X                 4 0
034300930114     C     X             CHAIN     CA81SF2                            86
034301020128     c*
034302020128     c                   if        not *in97 and not *in90 and
034303020128     C                             SF2ITO > 0
034500930212     C                   Z-ADD     X             HKEY
034600930114     C                   SETON                                        90
034700930114     C                   END
034800020107     C* CONTROLLO CHE SE IMPORTO = A 0 IL SEGNO
034900930108     C* SIA INDICATO 'N' ALTRIMENTI MESSAGGIO DI AVVERTIMENTO
034901020226     C* SOLO PER FILIALE
035000020226     C                   IF        sf2ito = 0 and
035001020226     c                             (xscgrp = 101 or xscgrp = 102)
035100930108     C     SF2STO        IFNE      'N'
035200930111     C   KF              SETON                                        17
035400020128     C   KF              DO
035500930114     C                   Z-ADD     X             HKEY              4 0
035700930114     C                   END
035800930108     C                   END
035900930111     C                   END
036000930108     C* CONTROLLO CHE SE DIGITATO IMPORTO IL SEGNO NON SIA 'N'
036100930108     C     SF2ITO        IFGT      0
036200930111     C     SF2STO        IFEQ      'N'
036300930108     C                   SETON                                        18
036500930212     C                   Z-ADD     X             HKEY
036800930108     C                   END
036900930111     C                   END
037000930111     C* CONTROLLO CHE SE DIGITATO IMPORTO CI SIA IL 'A' O 'D'
037100930111     C     SF2ITO        IFGT      0
037200930111     C     SF2STO        ANDEQ     *BLANKS
037300930111     C                   SETON                                        19
037500930212     C                   Z-ADD     X             HKEY
037800930111     C                   END
037900930212     C* CONTROLLO CHE SE IMPORTO = 0 IL SEGNO NON SIA 'A' O 'D'
038000930212     C     SF2ITO        IFEQ      0
038100930212     C     SF2STO        ANDEQ     'A'
038200930212     C     SF2ITO        OREQ      0
038300930212     C     SF2STO        ANDEQ     'D'
038400930212     C                   SETON                                        20
038600930212     C                   Z-ADD     X             HKEY
038900930212     C                   END
039000930212     C*
039100930108     C   16
039200930108     COR 18
039300930111     COR 19
039400930212     COR 20
039500930108     COR 17              SETON                                        97
039600930111     C                   UPDATE    CA81SF2
039700930111     C                   END
039800930111     C*
039900921223     C                   ENDSR
041500921222     C*----------
041600921222     C* INIZIALIZZAZIONE
041700921222     C*----------
041800921223     C     DEFCAM        BEGSR                                                  *
041900921224     C                   SETOFF                                       98
042000921223     C     *ENTRY        PLIST                                                  *
042100921223     C                   PARM                    KPJBA                          *
042200011219
042300011219     ** Mi collego ad una società.
042400011219     C                   CALLB     'XSOC'
042500011219   >>C                   PARM      'SOC001'      TipXsc            6
042600011219   >>C                   PARM      *BLANK        SocXsc            3
042700011219   >>C                   PARM      *ZERO         CdsXsc            9 0
042800011219   >>C                   PARM      *BLANK        ModXsc            3
042900011219   >>C                   PARM      *BLANK        RtnXsc            1
043000011219     C                   PARM                    XSoc001DS
043100011219   >>C                   PARM                    Kpjba
043200011219
043300011219     ** Collegamento fallito.
043400011219     C                   IF        RtnXsc = '1'
043500011219     C                   EVAL      MsgErr =
043600011219     C                             'Non è possibile collegarsi alla società '
043700011219     C                             + XscSoc + '.'
043800011219     C                   EVAL      *INLR = *ON
043900011219     C                   RETURN
044000011219     C                   ENDIF
044100011219
044200011219     C                   EVAL      RSut = XScDsi
044300011221
044400011221     C                   EVAL      StiSocieta = XscSoc
044500011221     C                   EVAL      StrSocieta = XscSoc
044600011221     C                   EVAL      EntSocieta = XscSoc
044808020221     ** Reperisce il livello di riservatezza
044809020221     C                   CLEAR                   XChkAutDS
044810020221     C                   EVAL      XcaVfu = 'STIME'
044811020221     C                   EVAL      XcaFnc = 'YSTIM2'
044812020221     C                   EXSR      ChkAut
044813020221     C                   IF        XcaErr = '1'
044814020221     C                   exsr      uscita
044815020221     c                   else
044816020221     c                   movel(p)  xcadds        ystimeds
044817020221     C                   ENDIF
045000011219     ** Reperisco parametri generali.
045100011219     C                   OPEN      YnStA00F
045200011219     C                   READ      YnStA00F
045300011219     C                   CLOSE     YnStA00F
045400011219     C                   IF        %EOF
045500011219     C                   EVAL      MsgErr =
045600011219     C                             'Mancano i parametri nel file YNSTA00F.'
045700020108     C                   exsr      uscita
045900011219     C                   ENDIF
046000011219
046100921223     C*----------                                          *
046200921223     C* PROPONGO I CAMPI A VIDEO PRENDENDOLI DA TABELLA    *
046300921223     C*----------                                          *
046400011219     C                   Z-ADD     StaAnn        VIDANN                         *
046500011219     C                   Z-ADD     StaMes        VIDMES                         *
046600011219
046700011219     C                   TIME                    DatEu                          *
048300011219     C* POSIZIONAMENTO YnStI00F                            *
048400930108     C     KASTI         KLIST                                                  *
048500020107     C                   KFLD                    xscsoc                         *
048600020107     C                   KFLD                    vidANN                         *
048700020107     C                   KFLD                    vidMES                         *
048800011219     C                   KFLD                    STITPRIF1                      *
048900020107     C                   KFLD                    vidcdc                         *
049000930108     C*
049100930111     C     KAST01        KLIST                                                  *
049200011219     C                   KFLD                    STISOCIETA                     *
049300011219     C                   KFLD                    STITPRIF1                      *
049400020107     C                   KFLD                    vidcdc                         *
049500020107     C                   KFLD                    strscg                         *
049600020107     C                   KFLD                    vidann                         *
049700020107     C                   KFLD                    vidmes                         *
049800011219
049900011219     C                   EVAL      STITPRIF1 = 'CDC'
050000011219
050100921222     C* POSIZIONAMENTO PIANO DEI CONTI                     *
050200920825     C     KACO          KLIST                                                  *
050300011219     C                   KFLD                    ENTSOCIETA                     *
050400011219     C                   KFLD                    ENTTPRIFAN                     *
050500011219     C                   KFLD                    ENTENTITA                      *
050600921223     C*                                                    *
051300921223     C                   ENDSR
051301020104     C******************************************************
051302020108     c*      Reperisco autorizzazioni per l'uso del pgm
051303020108     C     chkaut        BEGSR
051304020104     C******************************************************
051305020104     C*
051307020108     C                   EVAL      XcaSoc = XscSoc
051308020108     C                   EVAL      XcaGrp = XscGrp
051311020108     C                   EVAL      XcaTck = 'CK'
051312020226     C                   EVAL      XcaPRF = KNMUS
051313020226     C                   EVAL      XcaCut = XscCut
051314020108     C                   EVAL      XcaNau = '1'
051315020108      *
051316020108     C                   CALLB     'XCHKAUT'
051317020108     C                   PARM                    XChkAutDS
051318020108     C*                                                    *
051319020108     C                   ENDSR
051400920825     C******************************************************
051500011221
051600011221     ***********************************************************************
051700011221     **
051800011221     ** Uscita dal programma.
051900011221     **
052000011221     ***********************************************************************
052100011221     C     Uscita        BEGSR
052200011221
052300011221     C                   EVAL      *INLR = *ON
052400011221     C                   RETURN
052500011221
052600011221     C                   ENDSR
052700011221     ***********************************************************************
