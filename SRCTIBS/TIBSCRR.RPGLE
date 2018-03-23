000100061201    ?H* TIBSCRR *-----------------------------------------------------*
000200061201?    H*            CREAZIONE NUMERATORI DI GRUPPO - FILTRO -         *
000300011206?    H*---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000500061201     FTIBScrD   CF   E             WORKSTN
000600011206     FAZORG01L  IF   E           K DISK
000700011206?    D*---------------------------------------------------------------*
000800011206     D L1              S              3  0 DIM(30)                              P.O.GESTITE
000900011206?    D*
001000061201     D* PASSAGGIO DATI PER CREAZIONE numeratori di gruppo  - AZNUM00F -
001100061201     d*                                       e di filiale - FLNUF00F -
001200011206     D PARAM           DS
001300011206     D* Codice P.O.
001400011206     D  PARFIL                 3      5  0
001500061201     D* terminal partenza di  PARFIL per numeratori partenza
001600061201     D  partfp                 6      8  0
001700011206     D* Codice Numeratore
001800061201     D  parCNU                 9     11
001900011206     D* Tipo P.O.
002000061201     D  parFTF                12     12
002100011206     D* Anno
002200061201     D  parANN                13     16  0
002300020206     D* Società
002400061201     D  parSOC                17     19
002500061201     D*
002600011206     D WLBDAT          DS                  INZ
002700011206     D  G02DAT                 1      8  0
002800011206     D  G02INV                 9     16  0
002900011206     D  G02ERR                17     17
003000011206     D  G02TGI                18     22  0
003100011206?    D*
003200011206     D* DS PER TRUL06R - CARICAMENTO £X
003300011206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003400011206     D  LIN                    1     90  0
003500011206     D                                     DIM(30)                              P.O. COMODO
003600011206     D*
003700011206     D* DS PER TIBS02R - Gestione ricerca / controllo tabelle
003800011206     D TIBS02DS      E DS                  INZ
003900011206     D*
004000011206     D FNLV55DS      E DS                  INZ
004100011206?    D*
004200011206     D KPJBA         E DS
004300011206     D CNCR80        E DS
004400011206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004500011206     D*
004600011206     C*****************************************************************
004700011206     C* RIEPILOGO INDICATORI
004800011206     C*****************************************************************
004900011206     C* 30    - DI COMODO
005000011206     C* 35    - USATO PER TESTN
005100011206     C* 40/43 - INDICATORI PER MESSAGGI DI ERRORE
005200011206     C* 90    - SEGNALA LA PRESENZA DI ERRORI
005300011206     C*****************************************************************
005400011206     C     *ENTRY        PLIST
005500011206     C                   PARM                    KPJBA
005600011206     C                   Z-ADD     1             CODUT
005700011206     C                   CALL      'X§PARUT'
005800011206     C                   PARM                    UT§DSE
005900011206     C                   MOVEL     RAGUT         RSUT             20
006000011206     C                   MOVEL     REC80         CNCR80
006100020206     C*
006200011206     C* DATA DEL GIORNO
006300011206     C                   TIME                    W0140            14 0
006400011206     C* UDATE IN GGMMAAAA
006500011206     C                   MOVE      W0140         WDTGIO            8 0
006600011206     C* UDATE IN AAAAMMGG
006700011206     C                   Z-ADD     WDTGIO        G02DAT
006800011206     C                   MOVEL     *BLANK        G02ERR
006900011206     C                   CALL      'XSRDA8'
007000011206     C                   PARM                    WLBDAT
007100011206     C                   MOVEL     G02INV        DATEU             8 0
007200061201     c                   movel     dateu         annocor           4 0
007300061206     C                   MOVEL     G02dat        viddat
007400011206     C*
007500011206     C* PULIZIA CAMPI FORMATO
007600011206     C                   Z-ADD     WDTGIO        VIDAAN
007700061201     c                   clear                   vidult
007800061201     c                   clear                   decult
007900011206     C*
008000011206?    C     INIZIO        TAG
008100011206     C                   CLEAR                   VIDPOD
008200061201     C                   CLEAR                   decPOD
008300061201     c                   clear                   vidtfp
008400061201     c                   clear                   dectfp
008500061201     c                   clear                   savann            4 0
008600011206     C*
008700011206     C     FOR01         TAG
008800061201     C                   EXFMT     BScrD01
008900011206     C*
009000061201?    C* F3 - FINE LAVORO
009100011206     C   KC              GOTO      FINE
009200011206     C*
009300011206?    C* CONTROLLI FORMATO
009400011206     C                   EXSR      CONTR
009500011206     C  NKF
009600011206     COR 90              GOTO      FOR01
009700011206     C*
009800061201?    C* F6 - ELABORAZIONE
009900061201     C
010000061201     c* 1° lancio: numeratori partenza con ANNO
010100061201     c                   clear                   parsoc
010200061201     c                   clear                   parcnu
010300061201     c                   movel     vidaAn        parann
010400061201     c                   movel     'P'           parftf
010500061201     c                   movel     vidtfp        partfp
010600011206     C                   Z-ADD     VIDPOD        PARFIL
010700011206     C                   MOVEL     PARAM         KPJBU
010800020110if  2C                   if        knmus = *all'1'
010900011206     C                   call      'TIBS18R'
011000011206     C                   parm                    KPJBA
011100011206x   2C                   else
011200011206     C                   MOVEL     'BS18'        KCOAZ
011300011206     C                   CALL      'BCH10'
011400011206     C                   PARM                    KPJBA
011500011206e   2C                   endif
011600061201     c
011700061201if  2C                   if        knmus = *all'1'
011800061201     C                   call      'FNLV06R'
011900061201     C                   parm                    KPJBA
012000061201x   2C                   else
012100061201     C                   MOVEL     'LV06'        KCOAZ
012200061201     C                   CALL      'BCH10'
012300061201     C                   PARM                    KPJBA
012400061201e   2C                   endif
012500061201     c
012600061201     c* 2° lancio: numeratori partenza senza anno
012700061201     c                   clear                   parann
012800061201     C                   MOVEL     PARAM         KPJBU
012900061201if  2C                   if        knmus = *all'1'
013000061201     C                   call      'TIBS18R'
013100061201     C                   parm                    KPJBA
013200061201x   2C                   else
013300061201     C                   MOVEL     'BS18'        KCOAZ
013400061201     C                   CALL      'BCH10'
013500061201     C                   PARM                    KPJBA
013600061201e   2C                   endif
013700061201     c
013800061201if  2C                   if        knmus = *all'1'
013900061201     C                   call      'FNLV06R'
014000061201     C                   parm                    KPJBA
014100061201x   2C                   else
014200061201     C                   MOVEL     'LV06'        KCOAZ
014300061201     C                   CALL      'BCH10'
014400061201     C                   PARM                    KPJBA
014500061201e   2C                   endif
014600061201     c
014700061201     c* 3° lancio: numeratori arrivo con anno
014800061201     c                   clear                   parsoc
014900061201     c                   clear                   parcnu
015000061201     c                   movel     vidaan        parann
015100061201     c                   movel     'A'           parftf
015200061201     c                   clear                   partfp
015300061201     C                   Z-ADD     VIDPOD        PARFIL
015400061201     C                   MOVEL     PARAM         KPJBU
015500061201if  2C                   if        knmus = *all'1'
015600061201     C                   call      'TIBS18R'
015700061201     C                   parm                    KPJBA
015800061201x   2C                   else
015900061201     C                   MOVEL     'BS18'        KCOAZ
016000061201     C                   CALL      'BCH10'
016100061201     C                   PARM                    KPJBA
016200061201e   2C                   endif
016300061201     c
016400061201if  2C                   if        knmus = *all'1'
016500061201     C                   call      'FNLV06R'
016600061201     C                   parm                    KPJBA
016700061201x   2C                   else
016800061201     C                   MOVEL     'LV06'        KCOAZ
016900061201     C                   CALL      'BCH10'
017000061201     C                   PARM                    KPJBA
017100061201e   2C                   endif
017200061201     c
017300061201     c* 4° lancio: numeratori Arrivo   senza anno
017400061201     c                   clear                   parann
017500061201     C                   MOVEL     PARAM         KPJBU
017600061201if  2C                   if        knmus = *all'1'
017700061201     C                   call      'TIBS18R'
017800061201     C                   parm                    KPJBA
017900061201x   2C                   else
018000061201     C                   MOVEL     'BS18'        KCOAZ
018100061201     C                   CALL      'BCH10'
018200061201     C                   PARM                    KPJBA
018300061201e   2C                   endif
018400061201     c
018500061201if  2C                   if        knmus = *all'1'
018600061201     C                   call      'FNLV06R'
018700061201     C                   parm                    KPJBA
018800061201x   2C                   else
018900061201     C                   MOVEL     'LV06'        KCOAZ
019000061201     C                   CALL      'BCH10'
019100061201     C                   PARM                    KPJBA
019200061201e   2C                   endif
019300011206     C*
019400061201     c                   movel     vidpod        vidult
019500061201     c                   movel     decpod        decult
019600061201     C*
019700011206     C                   GOTO      INIZIO
019800011206     C*
019900011206     C     FINE          TAG
020000011206     C                   SETON                                        LR
020100011206     C*
020200011206?    C*---------------------------------------------------------------*
020300011206?    C* CONTR * CONTROLLI FORMATO                                     *
020400011206?    C*---------------------------------------------------------------*
020500011206     C     CONTR         BEGSR
020600011206     C                   SETOFF                                       90
020700020206     C*
020800011206     C*
020900020206?    C****  P.O.  ****
021000061201     c                   clear                   decpod
021100011206IF  1C     VIDPOD        IFEQ      0
021200011206     C                   SETON                                        4290
021300011206     C                   GOTO      ENDCTR
021400011206X   1C                   ELSE
021500011206     C*
021600011206IF  2C     VIDPOD        IFGT      0
021700011206     C     VIDPOD        CHAIN     AZORG                              30
021800011206     C*
021900011206IF  3C     *IN30         IFEQ      *ON
022000011206     C     ORGFVA        ORNE      *BLANKS
022100011206     C     ORGFAG        OREQ      'V'
022200011206     C                   SETON                                        4290
022300011206     C                   GOTO      ENDCTR
022400041130E   3C                   ENDIF
022500061201     c                   movel     orgdes        decpod
022600020206     C*
022700011206E   2C                   ENDIF
022800011206E   1C                   ENDIF
022900061206     c*
023000061206     c****  data di riferimento ****
023100061206     c                   if        viddat=0
023200061206     C                   SETON                                        4590
023300061206     C                   GOTO      ENDCTR
023400061206E   3C                   ENDIF
023500061206     c
023600061206     C                   Z-ADD     viddat        G02DAT
023700061206     C                   MOVEL     *BLANK        G02ERR
023800061206     C                   CALL      'XSRDA8'
023900061206     C                   PARM                    WLBDAT
024000061206     c                   if        g02err='1'
024100061206     C                   SETON                                        4590
024200061206     C                   GOTO      ENDCTR
024300061206E   3C                   ENDIF
024400061206     C                   MOVEL     G02INV        datinv            8 0
024500061206     c                   movel     g02dat        viddat
024600061206     C*
024700011206     C*
024800020206?    C****  TERMINAL DI PARTENZA (P.O. ELABORATORE)  ****
024900061201     c* lo calcolo
025000011206     C                   CLEAR                   FNLV55DS
025100011206     C                   MOVEL     'P'           D55TPT
025200061201     C                   MOVEL     VIDpod        D55LIN
025300061206     C                   MOVEL     datinv        d55DRF
025400011206     C                   CALL      'FNLV55R'
025500011206     C                   PARM                    FNLV55DS
025600061201     c
025700061201     c                   movel     d55tfp        vidtfp
025800061201     c     vidtfp        chain     azorg01l
025900061201     c                   if        %found(azorg01l)
026000061201     c                   movel     orgdes        dectfp
026100061201     c                   else
026200061201     c                   movel     *all'?'       dectfp
026300061201     c                   seton                                        4390
026400061201     C                   GOTO      ENDCTR
026500061201E   3C                   ENDIF
026600011206     C*
026700061201     c* Anno obbligatorio
026800061201     c                   if        vidaan=0
026900061201     c                   seton                                        4490
027000061201     C                   GOTO      ENDCTR
027100061201E   3C                   ENDIF
027200061201     c* Per forzatura
027300061201     c                   if        vidaan<>savann
027400061201     c                   clear                   forz
027500061201     c                   movel     vidaan        savann
027600061201     c                   endif
027700061201     c
027800061201     c* Se non è l'anno corrente errore forzabile
027900061201     c                   if        vidaan<>annocor  and forz='0'
028000061201     c                   seton                                        4090
028100061201     c                   movel     '1'           forz              1
028200061201     C                   GOTO      ENDCTR
028300061201E   3C                   ENDIF
028400011206     C*
028500011206     C     ENDCTR        ENDSR
