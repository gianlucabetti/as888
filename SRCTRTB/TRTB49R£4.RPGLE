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
004600931209     C                   END
004700931209     C*
004800931209     C                   END
004900931209     C* Fine programma
005000931209     C     FINE          TAG
005100931209     C                   SETON                                        LR
005200931209     C*--------------------------------------------------------------*
005300931209     C*  GESVD2: Gestione prima videata                              *
005400931209     C*--------------------------------------------------------------*
005500931209     C     GESVD2        BEGSR
005600931209     C*----------------------------------------------------*
005700931209     C*  Emissione videata
005800931209     C                   EXFMT     TB49D2
005900990702     C                   SETOFF                                       99
006000931209     C*----------------------------------------------------*
006100931209     C*  Fine elaborazione
006200931209     C     *INKC         IFEQ      '1'
006300931209     C                   MOVEL     'S'           £FINE
006400931209     C                   GOTO      FINVD2
006500931209     C                   END
006600931209     C*  Controlli
006700931209     C                   EXSR      CNTVD2
006800931209     C*----------------------------------------------------*
006900931209     C*  Conferma
007000931209     C  N99*INKF         IFEQ      '1'
007100931209     C                   EXSR      AGGIOR
007200931209     C                   ELSE
007300931209     C*----------------------------------------------------*
007400931209     C*  Se non ci sono errori passo alla videata succesiva
007500931209     C                   MOVEL     '3'           TIPVID
007600931209     C                   END
007700931209     C*----------------------------------------------------*
007800931209     C     FINVD2        ENDSR
007900931209     C*--------------------------------------------------------------*
008000931209     C*  GESVD3: Gestione terza videata                              *
008100931209     C*--------------------------------------------------------------*
008200931209     C     GESVD3        BEGSR
008300931209     C*----------------------------------------------------*
008400931209     C*  Emissione videata
008500931209     C                   EXFMT     TB49D3
008600931209     C  N10              SETON                                        10
008700990702     C                   SETOFF                                       90
008800931209     C*----------------------------------------------------*
008900931209     C*  Fine elaborazione
009000931209     C     *INKC         IFEQ      '1'
009100931209     C                   MOVEL     'S'           £FINE
009200931209     C                   GOTO      FINVD3
009300931209     C                   END
009400931209     C*----------------------------------------------------*
009500931209     C*  Ritorno prima videata
009600931209     C     *INKL         IFEQ      '1'
009700931209     C                   MOVEL     '2'           TIPVID
009800931209     C                   GOTO      FINVD3
009900931209     C                   END
010000990702     C*----------------------------------------------------*
010100990702     C*  Controlli
010200990702     C                   EXSR      CNTVD3
010300931209     C*----------------------------------------------------*
010400931209     C*  Conferma
010500931209     C   KF              EXSR      AGGIOR
010600931209     C*----------------------------------------------------*
010700931209     C     FINVD3        ENDSR
010800931209     C*--------------------------------------------------------------*
010900931209     C*  CNTVD2: Controllo seconda videata                           *
011000931209     C*--------------------------------------------------------------*
011100931209     C     CNTVD2        BEGSR
011200931209     C*----------------------------------------------------*
011300931209     C*  Controllo codice bollo addebito spese
011400941005     C     VCBO          IFNE      'B '
011500931209     C                   SETON                                        2299
011600931209     C                   GOTO      FICTRL
011700931209     C                   END
011800931209     C*----------------------------------------------------*
011900941005     C*  Controllo codice bollA X INVIO LETTERA MITTENTE
012000941005     C     VCCO          IFNE      '£ '
012100931209     C                   SETON                                        2399
012200931209     C                   GOTO      FICTRL
012300931209     C                   END
012400931209     C*----------------------------------------------------*
012500931209     C     FICTRL        ENDSR
012600990702     C*--------------------------------------------------------------*
012700990702     C*  CNTVD3: Controllo TERZA videata                           *
012800990702     C*--------------------------------------------------------------*
012900990702     C     CNTVD3        BEGSR
013000990702     C*----------------------------------------------------*
013100990702     C*  Se è stata impostata la data di inizio periodo
013200990702     C*  controllo che sia formalmente valida la data inizio periodo
013300990702     C     VDPI          IFNE      0
013400990702     C                   CLEAR                   WLBDA8
013500990702     C                   Z-ADD     VDPI          G02DAT
013600990702     C                   CALL      'XSRDA8'
013700990702     C                   PARM                    WLBDA8
013800990702     C     G02ERR        IFEQ      '1'
013900990702     C                   SETON                                        2499
014000990702     C                   GOTO      FINCT3
014100990702     C                   ELSE
014200990702     C                   Z-ADD     G02DAT        VDPI
014300990702     C                   Z-ADD     G02INV        DATINI            8 0
014400990702     C                   END
014500990702     C                   END
014600990702     C*  controllo che sia formalmente valida la data FINE periodo
014700990702     C     VDPF          IFNE      0
014800990702     C                   CLEAR                   WLBDA8
014900990702     C                   Z-ADD     VDPF          G02DAT
015000990702     C                   CALL      'XSRDA8'
015100990702     C                   PARM                    WLBDA8
015200990702     C     G02ERR        IFEQ      '1'
015300990702     C                   SETON                                        2599
015400990702     C                   GOTO      FINCT3
015500990702     C                   ELSE
015600990702     C                   Z-ADD     G02DAT        VDPF
015700990702     C                   Z-ADD     G02INV        DATFIN            8 0
015800990702     C                   END
015900990702     C                   END
016000990702     C     DATINI        IFGT      DATFIN
016100990702     C                   SETON                                        2699
016200990702     C                   GOTO      FINCT3
016300990702     C                   END
016400990702     C*
016500040720     C     V2DPI         IFNE      0
016600040720     C                   CLEAR                   WLBDA8
016700040720     C                   Z-ADD     V2DPI         G02DAT
016800040720     C                   CALL      'XSRDA8'
016900040720     C                   PARM                    WLBDA8
017000040720     C     G02ERR        IFEQ      '1'
017100040720     C                   SETON                                        2799
017200040720     C                   GOTO      FINCT3
017300040720     C                   ELSE
017400040720     C                   Z-ADD     G02DAT        V2DPI
017500040720     C                   Z-ADD     G02INV        DATINI
017600040720     C                   END
017700040720     C                   END
017800040720     C*  controllo che sia formalmente valida la data FINE periodo
017900040720     C     V2DPF         IFNE      0
018000040720     C                   CLEAR                   WLBDA8
018100040720     C                   Z-ADD     V2DPF         G02DAT
018200040720     C                   CALL      'XSRDA8'
018300040720     C                   PARM                    WLBDA8
018400040720     C     G02ERR        IFEQ      '1'
018500040720     C                   SETON                                        2899
018600040720     C                   GOTO      FINCT3
018700040720     C                   ELSE
018800040720     C                   Z-ADD     G02DAT        V2DPF
018900040720     C                   Z-ADD     G02INV        DATFIN            8 0
019000040720     C                   END
019100040720     C                   END
019200040720     C     DATINI        IFGT      DATFIN
019300040720     C                   SETON                                        2999
019400040720     C                   GOTO      FINCT3
019500040720     C                   END
019600041109     C*data consegna richiesta periodo stop controlli
019700041109     C     V2DrI         IFNE      0
019800041109     C                   CLEAR                   WLBDA8
019900041109     C                   Z-ADD     V2DrI         G02DAT
020000041109     C                   CALL      'XSRDA8'
020100041109     C                   PARM                    WLBDA8
020200041109     C     G02ERR        IFEQ      '1'
020300041109     C                   SETON                                        3099
020400041109     C                   GOTO      FINCT3
020500041109     C                   ELSE
020600041109     C                   Z-ADD     G02DAT        V2DrI
020700041109     C                   Z-ADD     G02INV        DATINI
020800041109     C                   END
020900041109     C                   END
021000041109     C*  controllo che sia formalmente valida la data FINE periodo
021100041109     C     V2DrF         IFNE      0
021200041109     C                   CLEAR                   WLBDA8
021300041109     C                   Z-ADD     V2DrF         G02DAT
021400041109     C                   CALL      'XSRDA8'
021500041109     C                   PARM                    WLBDA8
021600041109     C     G02ERR        IFEQ      '1'
021700041109     C                   SETON                                        3199
021800041109     C                   GOTO      FINCT3
021900041109     C                   ELSE
022000041109     C                   Z-ADD     G02DAT        V2DrF
022100041109     C                   Z-ADD     G02INV        DATFIN            8 0
022200041109     C                   END
022300041109     C                   END
022400041109     C     DATINI        IFGT      DATFIN
022500041109     C                   SETON                                        3299
022600041109     C                   GOTO      FINCT3
022700041109     C                   END
022800990702     C     FINCT3        ENDSR
022900931209     C*--------------------------------------------------------------*
023000931209     C*  INZVD2: Inizializzazione seconda videata                    *
023100931209     C*--------------------------------------------------------------*
023200931209     C     INZVD2        BEGSR
023300931209     C*----------------------------------------------------*
023400931209     C*  Imposto parametri a video da DS 2G
023500170124     c                   movel     §2GG_IMG      vg_img
023600170124     c                   movel     §2GKGIMG      vg_pkg
023700941005     C                   MOVEL     §2GCBO        VCBO
023800931209     C                   MOVEL     §2GGGL        VGGL
023900931209     C                   MOVEL     §2GGCH        VGCH
024000931209     C                   MOVEL     §2GGRI        VGRI
024100941005     C                   MOVEL     §2GCCO        VCCO
024200950213     C                   MOVEL     §2GESE        VESE
024300931209     C                   MOVEL     §2GHMG        VHMG
024400931209     C                   MOVEL     §2GHMD        VHMD
024500931209     C                   MOVEL     §2GGDA        VGDA
024600931209     C                   MOVEL     §2GGEA        VGEA
024700931209     C                   MOVEL     §2GGRA        VGRA
024800931209     C                   MOVEL     §2GCRP        VCRP
024900931209     C                   MOVEL     §2GGRP        VGRP
025000931209     C                   MOVEL     §2GGRD        VGRD
025100931209     C                   MOVEL     §2GGRC        VGRC
025200931209     C                   MOVEL     §2GRAA        VRAA
025300931209     C                   MOVEL     §2GREA        VREA
025400040720     c                   z-add     §2GLMT        vLMT
025500051216     c                   z-add     §2GLat        vLMTt
025600040720     c                   z-add     §2GGDP        vGDP
025700040720     c                   z-add     §2GGFE        vGFE
025800160523     c                   z-add     §2GGRE        vGRE
025900040720     c                   z-add     §2GLDR        vLDR
026000040720     c                   movel     §2GMDR        vMDR
026100040720     c                   movel     §2GADR        vADR
026200990705     C                   MOVEL     §2GDPI        G02INV
026300990705     C                   MOVEL     '3'           G02ERR
026400990705     C                   CALL      'XSRDA8'
026500990705     C                   PARM                    WLBDA8
026600990705     C                   MOVEL     G02DAT        VDPI
026700990705     C                   MOVEL     §2GDPF        G02INV
026800990705     C                   MOVEL     '3'           G02ERR
026900990705     C                   CALL      'XSRDA8'
027000990705     C                   PARM                    WLBDA8
027100990705     C                   MOVEL     G02DAT        VDPF
027200040720      *seconda ds ds2g2
027300040720     C                   MOVEL     §2G2DPI       G02INV
027400040720     C                   MOVEL     '3'           G02ERR
027500040720     C                   CALL      'XSRDA8'
027600040720     C                   PARM                    WLBDA8
027700040720     C                   MOVEL     G02DAT        V2DPI
027800040720     C                   MOVEL     §2G2DPF       G02INV
027900040720     C                   MOVEL     '3'           G02ERR
028000040720     C                   CALL      'XSRDA8'
028100040720     C                   PARM                    WLBDA8
028200040720     C                   MOVEL     G02DAT        V2DPF
028300041109
028400041109     C                   MOVEL     §2G2DrI       G02INV
028500041109     C                   MOVEL     '3'           G02ERR
028600041109     C                   CALL      'XSRDA8'
028700041109     C                   PARM                    WLBDA8
028800041109     C                   MOVEL     G02DAT        V2DrI
028900041109     C                   MOVEL     §2G2DrF       G02INV
029000041109     C                   MOVEL     '3'           G02ERR
029100041109     C                   CALL      'XSRDA8'
029200041109     C                   PARM                    WLBDA8
029300041109     C                   MOVEL     G02DAT        V2DrF
029400931209     C*----------------------------------------------------*
029500931209     C                   MOVEL     '2'           TIPVID
029600931209     C*----------------------------------------------------*
029700931209     C                   ENDSR
029800931209     C*--------------------------------------------------------------*
029900931209     C*  AGGIOR: Aggiornamento paramtri da video a file              *
030000931209     C*--------------------------------------------------------------*
030100931209     C     AGGIOR        BEGSR
030200931209     C*----------------------------------------------------*
030300170124     c                   movel     vg_img        §2GG_IMG
030400170125     c                   movel     vg_pkg        §2gKGIMG
030500941005     C                   MOVEL     VCBO          §2GCBO
030600931209     C                   MOVEL     VGGL          §2GGGL
030700931209     C                   MOVEL     VGCH          §2GGCH
030800931209     C                   MOVEL     VGRI          §2GGRI
030900941005     C                   MOVEL     VCCO          §2GCCO
031000950213     C                   MOVEL     VESE          §2GESE
031100931209     C                   MOVEL     VHMG          §2GHMG
031200931209     C                   MOVEL     VHMD          §2GHMD
031300931209     C                   MOVEL     VGDA          §2GGDA
031400931209     C                   MOVEL     VGEA          §2GGEA
031500931209     C                   MOVEL     VGRA          §2GGRA
031600931209     C                   MOVEL     VCRP          §2GCRP
031700931209     C                   MOVEL     VGRP          §2GGRP
031800931209     C                   MOVEL     VGRD          §2GGRD
031900931209     C                   MOVEL     VGRC          §2GGRC
032000931209     C                   MOVEL     VRAA          §2GRAA
032100931209     C                   MOVEL     VREA          §2GREA
032200040720     c                   z-add     vLMT          §2GLMT
032300051216     c                   z-add     vLMTt         §2GLat
032400040720     c                   z-add     vGDP          §2GGDP
032500040720     c                   z-add     vGFE          §2GGFE
032600160523     c                   z-add     vGRE          §2GGRE
032700040720     c                   z-add     vLDR          §2GLDR
032800040720     c                   movel     vMDR          §2GMDR
032900040720     c                   movel     vADR          §2GADR
033000990705     C                   MOVEL     VDPI          G02DAT
033100990705     C                   MOVEL     ' '           G02ERR
033200990705     C                   CALL      'XSRDA8'
033300990705     C                   PARM                    WLBDA8
033400990705     C                   MOVEL     G02INV        §2GDPI
033500990705     C                   MOVEL     VDPF          G02DAT
033600990705     C                   MOVEL     ' '           G02ERR
033700990705     C                   CALL      'XSRDA8'
033800990705     C                   PARM                    WLBDA8
033900990705     C                   MOVEL     G02INV        §2GDPF
034000040720      *seconda ds ds2g2
034100040720     C                   MOVEL     V2DPI         G02DAT
034200040720     C                   MOVEL     ' '           G02ERR
034300040720     C                   CALL      'XSRDA8'
034400040720     C                   PARM                    WLBDA8
034500040720     C                   MOVEL     G02INV        §2G2DPI
034600040720     C                   MOVEL     V2DPF         G02DAT
034700040720     C                   MOVEL     ' '           G02ERR
034800040720     C                   CALL      'XSRDA8'
034900040720     C                   PARM                    WLBDA8
035000040720     C                   MOVEL     G02INV        §2G2DPF
035100041109
035200041109     C                   MOVEL     V2DrI         G02DAT
035300041109     C                   MOVEL     ' '           G02ERR
035400041109     C                   CALL      'XSRDA8'
035500041109     C                   PARM                    WLBDA8
035600041109     C                   MOVEL     G02INV        §2G2DrI
035700041109     C                   MOVEL     V2DrF         G02DAT
035800041109     C                   MOVEL     ' '           G02ERR
035900041109     C                   CALL      'XSRDA8'
036000041109     C                   PARM                    WLBDA8
036100041109     C                   MOVEL     G02INV        §2G2DrF
036200931209     C*----------------------------------------------------*
036300040720     C*  EsEguo aggiornamento
036400040720     C                   MOVEL     *BLANKS       TBwKEY
036500040720     C                   MOVEL     '1'           TBwKEY
036600040720     C     KEYTAB        CHAIN     TABEL                              80
036700931209     C     *IN80         IFEQ      '0'
036800040720     C                   MOVEL(p)  DS2G          TBLUNI
036900040720     C                   MOVEL     *BLANKS       TBLFTR
037000040720     C                   Z-ADD     0             TBLDTR
037100931209     C                   UPDATE    TABEL
037200931209     C                   END
037300040720     C                   MOVEL     *BLANKS       TBwKEY
037400040720     C                   MOVEL     '2'           TBwKEY
037500040720     C     KEYTAB        CHAIN     TABEL                              80
037600040720     C     *IN80         IFEQ      '0'
037700040720     C                   MOVEL(p)  DS2G2         TBLUNI
037800040720     C                   MOVEL     *BLANKS       TBLFTR
037900040720     C                   Z-ADD     0             TBLDTR
038000040720     C                   UPDATE    TABEL
038100040720     C                   END
038200931209     C*----------------------------------------------------*
038300931209     C*  Ritorno alla prima videata
038400040720     C                   MOVEL     '2'           TIPVID
038500931209     C*----------------------------------------------------*
038600931209     C                   ENDSR
038700931209     C*--------------------------------------------------------------*
038800931209     C*  *INZSR: Operazioni iniziali                                 *
038900931209     C*--------------------------------------------------------------*
039000931209     C     *INZSR        BEGSR
039100931209     C*
039200931209     C     *ENTRY        PLIST
039300931209     C                   PARM                    KPJBA
039400931209     C*----------------------------------------------------*
039500931209     C*  Richiamo XPARUT
039600931209     C                   Z-ADD     1             CODUT
039700931209     C                   CALL      'XPARUT'
039800931209     C                   PARM                    UTEDSE
039900931209     C     CODUT         CABEQ     -1            FINE
040000931209     C                   MOVEL     RAGUT         RSUT
040100931209     C*----------------------------------------------------*
040200931209     C* Definizione chiavi                                 *
040300931209     C     KEYTAB        KLIST
040400931209     C                   KFLD                    TBWKUT
040500931209     C                   KFLD                    TBWCOD
040600931209     C                   KFLD                    TBWKEY
040700931209     C*----------------------------------------------------*
040800931209     C* Definizione variabili                              *
040900931209     C     *LIKE         DEFINE    TBLKUT        TBWKUT
041000931209     C     *LIKE         DEFINE    TBLCOD        TBWCOD
041100931209     C     *LIKE         DEFINE    TBLKEY        TBWKEY
041200931209     C*----------------------------------------------------*
041300931209     C* Controllo se utente è un EDP
041400931209     C                   SETOFF                                       79
041500931209     C                   MOVEL     KNMUS         PROF              3
041600931209     C     PROF          IFNE      'EDP'
041700931209     C                   SETON                                        79
041800931209     C                   END
041900931209     C*----------------------------------------------------*
042000931209     C*  Inizializzazione variabili
042100931209     C                   MOVEL     'N'           £FINE             1
042200040720     C                   MOVEL     '2'           TIPVID            1
042300931209     C                   Z-ADD     1             TBWKUT
042400931209     C                   MOVE      '2G'          TBWCOD
042500971210     C******************************************************
042600971210     C* Richiamo pgm per controllo modificabilità tabella
042700040720     C*                  CLEAR                   TRUL12
042800040720     C*                  MOVEL     'L'           I12TLA
042900040720     C*                  MOVEL     KNSIF         I12NSI
043000040720     C*                  MOVEL     '2G'          I12COD
043100040720     C*                  CALL      'TRUL12R'
043200040720     C*                  PARM                    TRUL12
043300040720     C*    O12ERR        IFEQ      '1'
043400040720     C*                  GOTO      FINE
043500040720     C*                  END
043600040720     C                   MOVEL     *BLANKS       TBwKEY
043700040720     C                   MOVEL     '1       '    TBwKEY
043800040720     C     KEYTAB        CHAIN     TABEL                              80
043900040720     C     *IN80         IFEQ      '0'
044000040720     C                   MOVEL(p)  TBLUNI        DS2G
044100040720     C                   ENDIF
044200040720     C                   MOVEL     *BLANKS       TBwKEY
044300040720     C                   MOVEL     '2       '    TBwKEY
044400040720     C     KEYTAB        CHAIN     TABEL                              80
044500040720     C     *IN80         IFEQ      '0'
044600040720     C                   MOVEL(p)  TBLUNI        DS2G2
044700040720     C                   ENDIF
044800040720     C                   EXSR      INZVD2
044900971210     C******************************************************
045000931209     C                   ENDSR
045100931209     O*-----------------------------------------------------*
