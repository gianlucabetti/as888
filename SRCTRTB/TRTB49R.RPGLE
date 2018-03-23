000100891030     F*--------------------------------------------------------------*
000200931209     F* GESTIONE TABELLA 2G
000300891030     F*----------------------* USO DEGLI INDICATORI *----------------*
000400931209     F*  KC        FINE                                              *
000500931209     F*  KF        CONFERMA                                          *
000600931209     F*  KL        RITORNO                                           *
000700931209     F*--------------------------------------------------------------*
000800891030     F*  02        VARIAZIONE                                        *
000900931209     F*  10        ESAMINATI TUTTI I FLAG                            *
001000931209     F*  80        CHAIN SU TABEL                                    *
001100931209     F*--------------------------------------------------------------*
001200931209     F*  21        ERRMSG: CAMPO OBBLIGATORIO (CODICE 1° VIDEATA)    *
001300931209     F*  22        ERRMSG: CODICE BOLLA ERRATO CARATTERI CONSENTITI  *
001400941005     F*                    B  (§2GCBO = CODICE BOLLA X ADD.SPESE)    *
001500931209     F*  23        ERRMSG: CODICE BOLLA ERRATO CARATTERI CONSENTITI  *
001600941005     F*                    A (§2GCCO = CODICE BOLLA C/SERVIZIO)      *
001700931209     F*  99        ERRORE GENERICO                                   *
001800891030     F*--------------------------------------------------------------*
001900931209     FTRTB49D   CF   E             WORKSTN
002000891030     FTABEL00F  UF A E           K DISK
002100891030     D*--------------------------------------------------------------*
002200891030     D*                        I N P U T                             *
002300891030     D*--------------------------------------------------------------*
002400931209     D DS2G          E DS
002500040720     D DS2G2         E DS
002600900608      *
002700891030     D KPJBA         E DS
002800911118      *
002900911118     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
003000971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
003100990702     D* DS PER INVERSIONE DATE
003200990702     D WLBDA8          DS
003300990702     D  G02DAT                 1      8  0
003400990702     D  G02INV                 9     16  0
003500990702     D  G02ERR                17     17
003600990702     D  G02TGI                18     22  0
003700931209     C*--------------------------------------------------------------*
003800931209     C*    C I C L O    P R I N C I P A L E                          *
003900931209     C*--------------------------------------------------------------*
004000931209     C*
004100931209     C*  Loop gestione varie videate fino a rich. fine elaboraz.
004200931209     C     £FINE         DOUEQ     'S'
004300931209     C*
004400931209     C     TIPVID        CASEQ     '2'           GESVD2
004500931209     C     TIPVID        CASEQ     '3'           GESVD3
004600170407     C     TIPVID        CASEQ     '4'           GESVD4
004700931209     C                   END
004800931209     C*
004900931209     C                   END
005000931209     C* Fine programma
005100931209     C     FINE          TAG
005200931209     C                   SETON                                        LR
005300931209     C*--------------------------------------------------------------*
005400931209     C*  GESVD2: Gestione prima videata                              *
005500931209     C*--------------------------------------------------------------*
005600931209     C     GESVD2        BEGSR
005700170407     C*
005800931209     C*  Emissione videata
005900931209     C                   EXFMT     TB49D2
006000990702     C                   SETOFF                                       99
006100931209     C*----------------------------------------------------*
006200931209     C*  Fine elaborazione
006300931209     C     *INKC         IFEQ      '1'
006400931209     C                   MOVEL     'S'           £FINE
006500931209     C                   GOTO      FINVD2
006600931209     C                   END
006700931209     C*  Controlli
006800931209     C                   EXSR      CNTVD2
006900931209     C*----------------------------------------------------*
007000931209     C*  Se non ci sono errori passo alla videata succesiva
007100170407     C  n99              MOVEL     '4'           TIPVID
007200170407     C*
007300931209     C     FINVD2        ENDSR
007400931209     C*--------------------------------------------------------------*
007500931209     C*  GESVD3: Gestione terza videata                              *
007600931209     C*--------------------------------------------------------------*
007700931209     C     GESVD3        BEGSR
007800170407     C*
007900931209     C*  Emissione videata
008000931209     C                   EXFMT     TB49D3
008100931209     C  N10              SETON                                        10
008200170407     C                   SETOFF                                       99
008300931209     C*----------------------------------------------------*
008400931209     C*  Fine elaborazione
008500931209     C     *INKC         IFEQ      '1'
008600931209     C                   MOVEL     'S'           £FINE
008700931209     C                   GOTO      FINVD3
008800931209     C                   END
008900931209     C*----------------------------------------------------*
009000170407     C*  Ritorno
009100931209     C     *INKL         IFEQ      '1'
009200170407     C                   MOVEL     '4'           TIPVID
009300931209     C                   GOTO      FINVD3
009400931209     C                   END
009500990702     C*----------------------------------------------------*
009600990702     C*  Controlli
009700990702     C                   EXSR      CNTVD3
009800931209     C*----------------------------------------------------*
009900931209     C*  Conferma
010000170407     c  n99              if        *inkf
010100170407     C                   EXSR      AGGIOR
010200170407     C                   MOVEL     'S'           £FINE
010300170407     C                   GOTO      FINVD3
010400170407     c                   endif
010500170407     C*
010600931209     C     FINVD3        ENDSR
010700931209     C*--------------------------------------------------------------*
010800931209     C*  CNTVD2: Controllo seconda videata                           *
010900931209     C*--------------------------------------------------------------*
011000931209     C     CNTVD2        BEGSR
011100170407     C*
011200931209     C*  Controllo codice bollo addebito spese
011300941005     C     VCBO          IFNE      'B '
011400931209     C                   SETON                                        2299
011500931209     C                   GOTO      FICTRL
011600931209     C                   END
011700931209     C*----------------------------------------------------*
011800941005     C*  Controllo codice bollA X INVIO LETTERA MITTENTE
011900941005     C     VCCO          IFNE      '£ '
012000931209     C                   SETON                                        2399
012100931209     C                   GOTO      FICTRL
012200931209     C                   END
012300170407     C*-
012400931209     C     FICTRL        ENDSR
012500990702     C*--------------------------------------------------------------*
012600990702     C*  CNTVD3: Controllo TERZA videata                           *
012700990702     C*--------------------------------------------------------------*
012800990702     C     CNTVD3        BEGSR
012900170407     C*
013000990702     C*  Se è stata impostata la data di inizio periodo
013100990702     C*  controllo che sia formalmente valida la data inizio periodo
013200990702     C     VDPI          IFNE      0
013300990702     C                   CLEAR                   WLBDA8
013400990702     C                   Z-ADD     VDPI          G02DAT
013500990702     C                   CALL      'XSRDA8'
013600990702     C                   PARM                    WLBDA8
013700990702     C     G02ERR        IFEQ      '1'
013800990702     C                   SETON                                        2499
013900990702     C                   GOTO      FINCT3
014000990702     C                   ELSE
014100990702     C                   Z-ADD     G02DAT        VDPI
014200990702     C                   Z-ADD     G02INV        DATINI            8 0
014300990702     C                   END
014400990702     C                   END
014500990702     C*  controllo che sia formalmente valida la data FINE periodo
014600990702     C     VDPF          IFNE      0
014700990702     C                   CLEAR                   WLBDA8
014800990702     C                   Z-ADD     VDPF          G02DAT
014900990702     C                   CALL      'XSRDA8'
015000990702     C                   PARM                    WLBDA8
015100990702     C     G02ERR        IFEQ      '1'
015200990702     C                   SETON                                        2599
015300990702     C                   GOTO      FINCT3
015400990702     C                   ELSE
015500990702     C                   Z-ADD     G02DAT        VDPF
015600990702     C                   Z-ADD     G02INV        DATFIN            8 0
015700990702     C                   END
015800990702     C                   END
015900990702     C     DATINI        IFGT      DATFIN
016000990702     C                   SETON                                        2699
016100990702     C                   GOTO      FINCT3
016200990702     C                   END
016300990702     C*
016400040720     C     V2DPI         IFNE      0
016500040720     C                   CLEAR                   WLBDA8
016600040720     C                   Z-ADD     V2DPI         G02DAT
016700040720     C                   CALL      'XSRDA8'
016800040720     C                   PARM                    WLBDA8
016900040720     C     G02ERR        IFEQ      '1'
017000040720     C                   SETON                                        2799
017100040720     C                   GOTO      FINCT3
017200040720     C                   ELSE
017300040720     C                   Z-ADD     G02DAT        V2DPI
017400040720     C                   Z-ADD     G02INV        DATINI
017500040720     C                   END
017600040720     C                   END
017700040720     C*  controllo che sia formalmente valida la data FINE periodo
017800040720     C     V2DPF         IFNE      0
017900040720     C                   CLEAR                   WLBDA8
018000040720     C                   Z-ADD     V2DPF         G02DAT
018100040720     C                   CALL      'XSRDA8'
018200040720     C                   PARM                    WLBDA8
018300040720     C     G02ERR        IFEQ      '1'
018400040720     C                   SETON                                        2899
018500040720     C                   GOTO      FINCT3
018600040720     C                   ELSE
018700040720     C                   Z-ADD     G02DAT        V2DPF
018800040720     C                   Z-ADD     G02INV        DATFIN            8 0
018900040720     C                   END
019000040720     C                   END
019100040720     C     DATINI        IFGT      DATFIN
019200040720     C                   SETON                                        2999
019300040720     C                   GOTO      FINCT3
019400040720     C                   END
019500041109     C*data consegna richiesta periodo stop controlli
019600041109     C     V2DrI         IFNE      0
019700041109     C                   CLEAR                   WLBDA8
019800041109     C                   Z-ADD     V2DrI         G02DAT
019900041109     C                   CALL      'XSRDA8'
020000041109     C                   PARM                    WLBDA8
020100041109     C     G02ERR        IFEQ      '1'
020200041109     C                   SETON                                        3099
020300041109     C                   GOTO      FINCT3
020400041109     C                   ELSE
020500041109     C                   Z-ADD     G02DAT        V2DrI
020600041109     C                   Z-ADD     G02INV        DATINI
020700041109     C                   END
020800041109     C                   END
020900041109     C*  controllo che sia formalmente valida la data FINE periodo
021000041109     C     V2DrF         IFNE      0
021100041109     C                   CLEAR                   WLBDA8
021200041109     C                   Z-ADD     V2DrF         G02DAT
021300041109     C                   CALL      'XSRDA8'
021400041109     C                   PARM                    WLBDA8
021500041109     C     G02ERR        IFEQ      '1'
021600041109     C                   SETON                                        3199
021700041109     C                   GOTO      FINCT3
021800041109     C                   ELSE
021900041109     C                   Z-ADD     G02DAT        V2DrF
022000041109     C                   Z-ADD     G02INV        DATFIN            8 0
022100041109     C                   END
022200041109     C                   END
022300041109     C     DATINI        IFGT      DATFIN
022400041109     C                   SETON                                        3299
022500041109     C                   GOTO      FINCT3
022600041109     C                   END
022700990702     C     FINCT3        ENDSR
022800170407     C*--------------------------------------------------------------*
022900170407     C*  GESVD4: Gestione quarta videata                              *
023000170407     C*--------------------------------------------------------------*
023100170407     C     GESVD4        BEGSR
023200170407     C*
023300170407     C*  Emissione videata
023400170407     C                   EXFMT     TB49D4
023500170407     C  N10              SETON                                        10
023600170407     C                   SETOFF                                       99
023700170407     C*----------------------------------------------------*
023800170407     C*  Fine elaborazione
023900170407     C     *INKC         IFEQ      '1'
024000170407     C                   MOVEL     'S'           £FINE
024100170407     C                   GOTO      FINVD4
024200170407     C                   END
024300170407     C*----------------------------------------------------*
024400170407     C*  Ritorno prima videata
024500170407     C     *INKL         IFEQ      '1'
024600170407     C                   MOVEL     '2'           TIPVID
024700170407     C                   GOTO      FINVD4
024800170407     C                   END
024900170407     C*----------------------------------------------------*
025000170407     C*  Controlli
025100170407     C                   EXSR      CNTVD4
025200170407     C*  Se non ci sono errori passo alla videata succesiva
025300170407     C  n99              MOVEL     '3'           TIPVID
025400170407     C*
025500170407     C     FINVD4        ENDSR
025600170407     C*--------------------------------------------------------------*
025700170407     C*  CNTVD4: Controllo quarta videata                           *
025800170407     C*--------------------------------------------------------------*
025900170407     C     CNTVD4        BEGSR
026000170407     c                   setoff                                       99
026100170407     C*
026200170407     C*data attesa ima periodo stop controlli
026300170407     C     V4dimai       IFNE      0
026400170407     C                   CLEAR                   WLBDA8
026500170407     C                   Z-ADD     V4dimai       G02DAT
026600170407     C                   CALL      'XSRDA8'
026700170407     C                   PARM                    WLBDA8
026800170407     C     G02ERR        IFEQ      '1'
026900170407     C                   SETON                                        3099
027000170407     C                   GOTO      FINCT4
027100170407     C                   ELSE
027200170407     C                   Z-ADD     G02DAT        V4dimai
027300170407     C                   Z-ADD     G02INV        DATINI
027400170407     C                   END
027500170407     C                   END
027600170407     C*  controllo che sia formalmente valida la data FINE periodo
027700170407     C     V4dimaf       IFNE      0
027800170407     C                   CLEAR                   WLBDA8
027900170407     C                   Z-ADD     V4Dimaf       G02DAT
028000170407     C                   CALL      'XSRDA8'
028100170407     C                   PARM                    WLBDA8
028200170407     C     G02ERR        IFEQ      '1'
028300170407     C                   SETON                                        3199
028400170407     C                   GOTO      FINCT4
028500170407     C                   ELSE
028600170407     C                   Z-ADD     G02DAT        V4Dimaf
028700170407     C                   Z-ADD     G02INV        DATFIN            8 0
028800170407     C                   END
028900170407     C                   END
029000170407     C     DATINI        IFGT      DATFIN
029100170407     C                   SETON                                        3299
029200170407     C                   GOTO      FINCT4
029300170407     C                   END
029400170407     C     FINCT4        ENDSR
029500931209     C*--------------------------------------------------------------*
029600931209     C*  INZVD2: Inizializzazione seconda videata                    *
029700931209     C*--------------------------------------------------------------*
029800931209     C     INZVD2        BEGSR
029900170407     C*
030000931209     C*  Imposto parametri a video da DS 2G
030100941005     C                   MOVEL     §2GCBO        VCBO
030200931209     C                   MOVEL     §2GGGL        VGGL
030300931209     C                   MOVEL     §2GGCH        VGCH
030400931209     C                   MOVEL     §2GGRI        VGRI
030500941005     C                   MOVEL     §2GCCO        VCCO
030600950213     C                   MOVEL     §2GESE        VESE
030700931209     C                   MOVEL     §2GHMG        VHMG
030800931209     C                   MOVEL     §2GHMD        VHMD
030900931209     C                   MOVEL     §2GGDA        VGDA
031000931209     C                   MOVEL     §2GGEA        VGEA
031100931209     C                   MOVEL     §2GGRA        VGRA
031200931209     C                   MOVEL     §2GCRP        VCRP
031300931209     C                   MOVEL     §2GGRP        VGRP
031400931209     C                   MOVEL     §2GGRD        VGRD
031500931209     C                   MOVEL     §2GGRC        VGRC
031600931209     C                   MOVEL     §2GRAA        VRAA
031700931209     C                   MOVEL     §2GREA        VREA
031800040720     c                   z-add     §2GLMT        vLMT
031900051216     c                   z-add     §2GLat        vLMTt
032000040720     c                   z-add     §2GGDP        vGDP
032100040720     c                   z-add     §2GGFE        vGFE
032200160523     c                   z-add     §2GGRE        vGRE
032300040720     c                   z-add     §2GLDR        vLDR
032400040720     c                   movel     §2GMDR        vMDR
032500040720     c                   movel     §2GADR        vADR
032600990705     C                   MOVEL     §2GDPI        G02INV
032700990705     C                   MOVEL     '3'           G02ERR
032800990705     C                   CALL      'XSRDA8'
032900990705     C                   PARM                    WLBDA8
033000990705     C                   MOVEL     G02DAT        VDPI
033100990705     C                   MOVEL     §2GDPF        G02INV
033200990705     C                   MOVEL     '3'           G02ERR
033300990705     C                   CALL      'XSRDA8'
033400990705     C                   PARM                    WLBDA8
033500990705     C                   MOVEL     G02DAT        VDPF
033600040720      *seconda ds ds2g2
033700040720     C                   MOVEL     §2G2DPI       G02INV
033800040720     C                   MOVEL     '3'           G02ERR
033900040720     C                   CALL      'XSRDA8'
034000040720     C                   PARM                    WLBDA8
034100040720     C                   MOVEL     G02DAT        V2DPI
034200040720     C                   MOVEL     §2G2DPF       G02INV
034300040720     C                   MOVEL     '3'           G02ERR
034400040720     C                   CALL      'XSRDA8'
034500040720     C                   PARM                    WLBDA8
034600040720     C                   MOVEL     G02DAT        V2DPF
034700041109
034800041109     C                   MOVEL     §2G2DrI       G02INV
034900041109     C                   MOVEL     '3'           G02ERR
035000041109     C                   CALL      'XSRDA8'
035100041109     C                   PARM                    WLBDA8
035200041109     C                   MOVEL     G02DAT        V2DrI
035300041109     C                   MOVEL     §2G2DrF       G02INV
035400041109     C                   MOVEL     '3'           G02ERR
035500041109     C                   CALL      'XSRDA8'
035600041109     C                   PARM                    WLBDA8
035700041109     C                   MOVEL     G02DAT        V2DrF
035800170407
035900170605     c                   movel     §2G2G_IMG     vg_img
036000170605     c                   movel     §2G2KGIMG     vg_pkg
036100170407     c
036200170407
036300170407     C                   MOVEL     §2G2dimai     G02INV
036400170407     C                   MOVEL     '3'           G02ERR
036500170407     C                   CALL      'XSRDA8'
036600170407     C                   PARM                    WLBDA8
036700170407     C                   MOVEL     G02DAT        V4dimai
036800170407
036900170407     C                   MOVEL     §2G2dimaf     G02INV
037000170407     C                   MOVEL     '3'           G02ERR
037100170407     C                   CALL      'XSRDA8'
037200170407     C                   PARM                    WLBDA8
037300170407     C                   MOVEL     G02DAT        V4dimaf
037400170407     C*
037500931209     C                   MOVEL     '2'           TIPVID
037600170407     c                   eval      vcod='2G'
037700170407     C*
037800931209     C                   ENDSR
037900931209     C*--------------------------------------------------------------*
038000931209     C*  AGGIOR: Aggiornamento paramtri da video a file              *
038100931209     C*--------------------------------------------------------------*
038200931209     C     AGGIOR        BEGSR
038300170407     C*
038400170605     c***                movel     vg_img        §2GG_IMG
038500170605     c***                movel     vg_pkg        §2gKGIMG
038600170407     c                   movel     vg_img        §2G2G_IMG
038700170407     c                   movel     vg_pkg        §2g2KGIMG
038800941005     C                   MOVEL     VCBO          §2GCBO
038900931209     C                   MOVEL     VGGL          §2GGGL
039000931209     C                   MOVEL     VGCH          §2GGCH
039100931209     C                   MOVEL     VGRI          §2GGRI
039200941005     C                   MOVEL     VCCO          §2GCCO
039300950213     C                   MOVEL     VESE          §2GESE
039400931209     C                   MOVEL     VHMG          §2GHMG
039500931209     C                   MOVEL     VHMD          §2GHMD
039600931209     C                   MOVEL     VGDA          §2GGDA
039700931209     C                   MOVEL     VGEA          §2GGEA
039800931209     C                   MOVEL     VGRA          §2GGRA
039900931209     C                   MOVEL     VCRP          §2GCRP
040000931209     C                   MOVEL     VGRP          §2GGRP
040100931209     C                   MOVEL     VGRD          §2GGRD
040200931209     C                   MOVEL     VGRC          §2GGRC
040300931209     C                   MOVEL     VRAA          §2GRAA
040400931209     C                   MOVEL     VREA          §2GREA
040500040720     c                   z-add     vLMT          §2GLMT
040600051216     c                   z-add     vLMTt         §2GLat
040700040720     c                   z-add     vGDP          §2GGDP
040800040720     c                   z-add     vGFE          §2GGFE
040900160523     c                   z-add     vGRE          §2GGRE
041000040720     c                   z-add     vLDR          §2GLDR
041100040720     c                   movel     vMDR          §2GMDR
041200040720     c                   movel     vADR          §2GADR
041300990705     C                   MOVEL     VDPI          G02DAT
041400990705     C                   MOVEL     ' '           G02ERR
041500990705     C                   CALL      'XSRDA8'
041600990705     C                   PARM                    WLBDA8
041700990705     C                   MOVEL     G02INV        §2GDPI
041800990705     C                   MOVEL     VDPF          G02DAT
041900990705     C                   MOVEL     ' '           G02ERR
042000990705     C                   CALL      'XSRDA8'
042100990705     C                   PARM                    WLBDA8
042200990705     C                   MOVEL     G02INV        §2GDPF
042300040720      *seconda ds ds2g2
042400040720     C                   MOVEL     V2DPI         G02DAT
042500040720     C                   MOVEL     ' '           G02ERR
042600040720     C                   CALL      'XSRDA8'
042700040720     C                   PARM                    WLBDA8
042800040720     C                   MOVEL     G02INV        §2G2DPI
042900040720     C                   MOVEL     V2DPF         G02DAT
043000040720     C                   MOVEL     ' '           G02ERR
043100040720     C                   CALL      'XSRDA8'
043200040720     C                   PARM                    WLBDA8
043300040720     C                   MOVEL     G02INV        §2G2DPF
043400041109
043500041109     C                   MOVEL     V2DrI         G02DAT
043600041109     C                   MOVEL     ' '           G02ERR
043700041109     C                   CALL      'XSRDA8'
043800041109     C                   PARM                    WLBDA8
043900041109     C                   MOVEL     G02INV        §2G2DrI
044000041109     C                   MOVEL     V2DrF         G02DAT
044100041109     C                   MOVEL     ' '           G02ERR
044200041109     C                   CALL      'XSRDA8'
044300041109     C                   PARM                    WLBDA8
044400041109     C                   MOVEL     G02INV        §2G2DrF
044500170407
044600170407     C                   MOVEL     V4dimai       G02DAT
044700170407     C                   MOVEL     ' '           G02ERR
044800170407     C                   CALL      'XSRDA8'
044900170407     C                   PARM                    WLBDA8
045000170407     C                   MOVEL     G02INV        §2G2dimai
045100170407     C                   MOVEL     V4dimaf       G02DAT
045200170407     C                   MOVEL     ' '           G02ERR
045300170407     C                   CALL      'XSRDA8'
045400170407     C                   PARM                    WLBDA8
045500170407     C                   MOVEL     G02INV        §2G2dimaf
045600931209     C*----------------------------------------------------*
045700040720     C*  EsEguo aggiornamento
045800040720     C                   MOVEL     *BLANKS       TBwKEY
045900040720     C                   MOVEL     '1'           TBwKEY
046000040720     C     KEYTAB        CHAIN     TABEL                              80
046100931209     C     *IN80         IFEQ      '0'
046200040720     C                   MOVEL(p)  DS2G          TBLUNI
046300040720     C                   MOVEL     *BLANKS       TBLFTR
046400040720     C                   Z-ADD     0             TBLDTR
046500931209     C                   UPDATE    TABEL
046600931209     C                   END
046700040720     C                   MOVEL     *BLANKS       TBwKEY
046800040720     C                   MOVEL     '2'           TBwKEY
046900040720     C     KEYTAB        CHAIN     TABEL                              80
047000040720     C     *IN80         IFEQ      '0'
047100040720     C                   MOVEL(p)  DS2G2         TBLUNI
047200040720     C                   MOVEL     *BLANKS       TBLFTR
047300040720     C                   Z-ADD     0             TBLDTR
047400040720     C                   UPDATE    TABEL
047500040720     C                   END
047600931209     C*----------------------------------------------------*
047700931209     C*  Ritorno alla prima videata
047800040720     C                   MOVEL     '2'           TIPVID
047900931209     C*----------------------------------------------------*
048000931209     C                   ENDSR
048100931209     C*--------------------------------------------------------------*
048200931209     C*  *INZSR: Operazioni iniziali                                 *
048300931209     C*--------------------------------------------------------------*
048400931209     C     *INZSR        BEGSR
048500931209     C*
048600931209     C     *ENTRY        PLIST
048700931209     C                   PARM                    KPJBA
048800931209     C*----------------------------------------------------*
048900931209     C*  Richiamo XPARUT
049000931209     C                   Z-ADD     1             CODUT
049100931209     C                   CALL      'XPARUT'
049200931209     C                   PARM                    UTEDSE
049300931209     C                   MOVEL     RAGUT         RSUT
049400931209     C*----------------------------------------------------*
049500931209     C* Definizione chiavi                                 *
049600931209     C     KEYTAB        KLIST
049700931209     C                   KFLD                    TBWKUT
049800931209     C                   KFLD                    TBWCOD
049900931209     C                   KFLD                    TBWKEY
050000931209     C*----------------------------------------------------*
050100931209     C* Definizione variabili                              *
050200931209     C     *LIKE         DEFINE    TBLKUT        TBWKUT
050300931209     C     *LIKE         DEFINE    TBLCOD        TBWCOD
050400931209     C     *LIKE         DEFINE    TBLKEY        TBWKEY
050500931209     C*----------------------------------------------------*
050600931209     C* Controllo se utente è un EDP
050700931209     C                   SETOFF                                       79
050800931209     C                   MOVEL     KNMUS         PROF              3
050900931209     C     PROF          IFNE      'EDP'
051000931209     C                   SETON                                        79
051100931209     C                   END
051200931209     C*----------------------------------------------------*
051300931209     C*  Inizializzazione variabili
051400931209     C                   MOVEL     'N'           £FINE             1
051500040720     C                   MOVEL     '2'           TIPVID            1
051600931209     C                   Z-ADD     1             TBWKUT
051700931209     C                   MOVE      '2G'          TBWCOD
051800971210     C******************************************************
051900971210     C* Richiamo pgm per controllo modificabilità tabella
052000040720     C*                  CLEAR                   TRUL12
052100040720     C*                  MOVEL     'L'           I12TLA
052200040720     C*                  MOVEL     KNSIF         I12NSI
052300040720     C*                  MOVEL     '2G'          I12COD
052400040720     C*                  CALL      'TRUL12R'
052500040720     C*                  PARM                    TRUL12
052600040720     C*    O12ERR        IFEQ      '1'
052700040720     C*                  GOTO      FINE
052800040720     C*                  END
052900040720     C                   MOVEL     *BLANKS       TBwKEY
053000040720     C                   MOVEL     '1       '    TBwKEY
053100040720     C     KEYTAB        CHAIN     TABEL                              80
053200040720     C     *IN80         IFEQ      '0'
053300040720     C                   MOVEL(p)  TBLUNI        DS2G
053400040720     C                   ENDIF
053500040720     C                   MOVEL     *BLANKS       TBwKEY
053600040720     C                   MOVEL     '2       '    TBwKEY
053700040720     C     KEYTAB        CHAIN     TABEL                              80
053800040720     C     *IN80         IFEQ      '0'
053900040720     C                   MOVEL(p)  TBLUNI        DS2G2
054000040720     C                   ENDIF
054100040720     C                   EXSR      INZVD2
054200971210     C******************************************************
054300931209     C                   ENDSR
054400931209     O*-----------------------------------------------------*
