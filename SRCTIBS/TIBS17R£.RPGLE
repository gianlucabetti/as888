000100011206    ?H* TIBS17R *-----------------------------------------------------*
000200020114?    H*             CREAZIONE NUMERATORI DI GRUPPO - FILTRO -         *
000300011206?    H*---------------------------------------------------------------*
000400011206     H DECEDIT('0,') DATEDIT(*DMY.)
000500011206     FTIBS17D   CF   E             WORKSTN
000600011206     FAZORG01L  IF   E           K DISK
000700011206?    D*---------------------------------------------------------------*
000800011206     D L1              S              3  0 DIM(30)                              P.O.GESTITE
000900011206?    D*
001000011206     D PARAM           DS
001100011206     D* Codice P.O.
001200011206     D  PARFIL                 3      5  0
001300011206     D* Codice P.O. elaboratore di PARFIL
001400011206     D  VIDFEL                 6      8  0
001500011206     D* Codice Numeratore
001600011206     D  VIDCNU                 9     11
001700011206     D* Tipo P.O.
001800011206     D  VIDFTF                12     12
001900011206     D* Anno
002000011206     D  VIDAAN                13     16  0
002100020206     D* Società
002200020206     D  VIDSOC                17     19
002300011206     D*
002400011206     D WLBDAT          DS                  INZ
002500011206     D  G02DAT                 1      8  0
002600011206     D  G02INV                 9     16  0
002700011206     D  G02ERR                17     17
002800011206     D  G02TGI                18     22  0
002900011206?    D*
003000011206     D* DS PER TRUL06R - CARICAMENTO £X
003100011206     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003200011206     D  LIN                    1     90  0
003300011206     D                                     DIM(30)                              P.O. COMODO
003400011206     D*
003500011206     D* DS PER TIBS02R - Gestione ricerca / controllo tabelle
003600011206     D TIBS02DS      E DS                  INZ
003700011206     D*
003800011206     D FNLV55DS      E DS                  INZ
003900011206?    D*
004000011206     D KPJBA         E DS
004100011206     D CNCR80        E DS
004200011206     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004300011206     D*
004400011206     C*****************************************************************
004500011206     C* RIEPILOGO INDICATORI
004600011206     C*****************************************************************
004700011206     C* 30    - DI COMODO
004800011206     C* 35    - USATO PER TESTN
004900011206     C* 40/43 - INDICATORI PER MESSAGGI DI ERRORE
005000011206     C* 90    - SEGNALA LA PRESENZA DI ERRORI
005100011206     C*****************************************************************
005200011206     C     *ENTRY        PLIST
005300011206     C                   PARM                    KPJBA
005400011206     C                   Z-ADD     1             CODUT
005500011206     C                   CALL      'X§PARUT'
005600011206     C                   PARM                    UT§DSE
005700011206     C                   MOVEL     RAGUT         RSUT             20
005800011206     C                   MOVEL     REC80         CNCR80
005900020206     C*
006000011206     C* DATA DEL GIORNO
006100011206     C                   TIME                    W0140            14 0
006200011206     C* UDATE IN GGMMAAAA
006300011206     C                   MOVE      W0140         WDTGIO            8 0
006400011206     C* UDATE IN AAAAMMGG
006500011206     C                   Z-ADD     WDTGIO        G02DAT
006600011206     C                   MOVEL     *BLANK        G02ERR
006700011206     C                   CALL      'XSRDA8'
006800011206     C                   PARM                    WLBDAT
006900011206     C                   MOVEL     G02INV        DATEU             8 0
007000011206     C*
007100011206     C* PULIZIA CAMPI FORMATO
007200020213     C                   CLEAR                   VIDSOC
007300011206     C                   Z-ADD     WDTGIO        VIDAAN
007400011206     C                   CLEAR                   VIDCNU
007500011206     C                   Z-ADD     SIMFEL        VIDFEL
007600011206     C                   MOVEL     'A'           VIDFTF
007700011206     C*
007800011206?    C     INIZIO        TAG
007900011206     C                   CLEAR                   VIDPOD
008000011206     C                   CLEAR                   VIDPOA
008100011206     C                   CLEAR                   VIDL1
008200011206     C*
008300011206     C     FOR01         TAG
008400011206     C                   EXFMT     BS17D01
008500011206     C*
008600011206?    C* CMD3 - FINE LAVORO
008700011206     C   KC              GOTO      FINE
008800011206     C*
008900011206?    C* CONTROLLI FORMATO
009000011206     C                   EXSR      CONTR
009100011206     C  NKF
009200011206     COR 90              GOTO      FOR01
009300011206     C*
009400011206?    C* CMD6 - ELABORAZIONE
009500011206SEL 1C                   select
009600011206W   1C                   WHEN          VIDPOD > 0
009700011206     C                             and VIDPOA = 0
009800011206     C                   Z-ADD     VIDPOD        PARFIL
009900011206     C                   MOVEL     PARAM         KPJBU
010000020110if  2C                   if        knmus = *all'1'
010100011206     C                   call      'TIBS18R'
010200011206     C                   parm                    KPJBA
010300011206x   2C                   else
010400011206     C                   MOVEL     'BS18'        KCOAZ
010500011206     C                   CALL      'BCH10'
010600011206     C                   PARM                    KPJBA
010700011206e   2C                   endif
010800011206     C*
010900011206W   1C                   WHEN          VIDPOD > 0
011000011206     C                             and VIDPOA > 0
011100011206     C                   Z-ADD     VIDPOD        W004DA            4 0
011200011206DO  2C     W004DA        DOWLE     VIDPOA
011300011206     C                   Z-ADD     W004DA        VIDPOD
011400011206     C     VIDPOD        CHAIN     AZORG01L                           30
011500011206IF  3C  N30ORGFVA        IFEQ      ' '
011600011206     C     ORGFAG        ANDNE     'V'
011700011206     C                   Z-ADD     VIDPOD        PARFIL
011800011206     C                   MOVEL     PARAM         KPJBU
011900020110if  4C                   if        knmus = *all'1'
012000011206     C                   call      'TIBS18R'
012100011206     C                   parm                    KPJBA
012200011206x   4C                   else
012300011206     C                   MOVEL     'BS18'        KCOAZ
012400011206     C                   CALL      'BCH10'
012500011206     C                   PARM                    KPJBA
012600011206e   4C                   endif
012700011206E   3C                   ENDIF
012800011206     C                   ADD       1             W004DA
012900011206E   2C                   ENDDO
013000011206     C*
013100011206W   1C                   WHEN      VIDL1 = 'S'
013200011206     C* CARICO LA L1
013300011206     C                   CLEAR                   DSUL06
013400011206     C                   MOVE      '£1'          D06COD
013500011206     C                   MOVEL     SIMFEL        D06KEY
013600011206     C                   MOVEL     'L'           D06TLA
013700011206     C                   MOVEL     DSUL06        KPJBU
013800011206     C                   CALL      'TRUL06R'
013900011206     C                   PARM                    KPJBA
014000011206     C                   MOVEL     KPJBU         DSUL06
014100011206     C                   MOVEA     LIN           L1
014200011206     C                   Z-ADD     1             XX                3 0
014300011206DO  2C     L1(XX)        DOWGT     0
014400011206     C                   Z-ADD     L1(XX)        PARFIL
014500011206     C                   MOVEL     PARAM         KPJBU
014600020110if  3C                   if        knmus = *all'1'
014700011206     C                   call      'TIBS18R'
014800011206     C                   parm                    KPJBA
014900011206x   3C                   else
015000011206     C                   MOVEL     'BS18'        KCOAZ
015100011206     C                   CALL      'BCH10'
015200011206     C                   PARM                    KPJBA
015300011206e   3C                   endif
015400011206     C                   ADD       1             XX
015500011206E   2C                   ENDDO
015600011206     C*
015700011206E   1C                   ENDSL
015800011206     C*
015900011206     C                   GOTO      INIZIO
016000011206     C*
016100011206     C     FINE          TAG
016200011206     C                   SETON                                        LR
016300011206     C*
016400011206?    C*---------------------------------------------------------------*
016500011206?    C* CONTR * CONTROLLI FORMATO                                     *
016600011206?    C*---------------------------------------------------------------*
016700011206     C     CONTR         BEGSR
016800011206     C                   SETOFF                                       90
016900020206     C*
017000020206?    C****  CODICE SOCIETA'  ****
017100020206     C* => Nessun controllo previsto per ora !
017200020206     C*    Sarà quindi possibile immettere un range di P.O. tra i quali
017300020206     C*     ce ne possono essere alcuni di società diverse.
017400011206     C*
017500011206?    C****  CODICE NUMERATORE  ****
017600011206     C     '?'           SCAN      VIDCNU                                 90
017700011206     C*
017800011206IF  1C     *IN90         IFEQ      *ON
017900011206     C                   SETON                                        90
018000011206     C                   CLEAR                   VIDCNU
018100011206     C                   clear                   TIBS02DS
018200011206     C                   movel     'R'           t02mod
018300011206     C                   movel     knsif         t02sif
018400011206     C                   movel     'CNU'         t02cod
018500011206     C                   movel     *blanks       t02ke1
018600011206     C                   call      'TIBS02R'
018700011206     C                   parm                    KPJBA
018800011206     C                   parm                    TIBS02DS
018900011206     C*
019000011206     C* CODICE SELEZIONATO
019100011206IF  2C     T02ke1        IFNE      *BLANKS
019200011206     C                   MOVEL     T02ke1        VIDCNU
019300011206E   2C                   ENDIF
019400011206     C                   GOTO      ENDCTR
019500011206E   1C                   ENDIF
019600011206     C*
019700011206     C* CONTROLLO ESISTENZA DEL NUMERATORE
019800011206IF  1C     VIDCNU        IFNE      *BLANKS
019900011206     C                   clear                   TIBS02DS
020000011206     C                   movel     'C'           t02mod
020100011206     C                   movel     knsif         t02sif
020200011206     C                   movel     'CNU'         t02cod
020300011206     C                   movel(p)  VIDCNU        t02ke1
020400011206     C                   call      'TIBS02R'
020500011206     C                   parm                    KPJBA
020600011206     C                   parm                    TIBS02DS
020700011206IF  2C                   IF        T02err <> *blanks
020800011206     C* CODICE NUMERATORE ERRATO
020900011206     C                   SETON                                        4190
021000011206     C                   GOTO      ENDCTR
021100011206E   2C                   ENDIF
021200011206E   1C                   ENDIF
021300011206     C*
021400020206?    C****  P.O.  ****
021500011206IF  1C     VIDPOD        IFEQ      0
021600011206     C     VIDPOA        ANDEQ     0
021700011206     C     VIDL1         ANDEQ     ' '
021800011206     C                   SETON                                        4290
021900011206     C                   GOTO      ENDCTR
022000011206X   1C                   ELSE
022100011206     C*
022200011206IF  2C     VIDPOD        IFGT      0
022300011206     C     VIDPOA        ANDEQ     0
022400011206     C     VIDPOD        CHAIN     AZORG                              30
022500011206     C*
022600011206IF  3C     *IN30         IFEQ      *ON
022700011206     C     ORGFVA        ORNE      *BLANKS
022800011206     C     ORGFAG        OREQ      'V'
022900011206     C                   SETON                                        4290
023000011206     C                   GOTO      ENDCTR
023100011206E   3C                   ENDIF
023200020206     C*
023300011206E   2C                   ENDIF
023400011206E   1C                   ENDIF
023500011206     C*
023600020206?    C****  TERMINAL DI PARTENZA (P.O. ELABORATORE)  ****
023700011206IF  1C     VIDFEL        IFEQ      0
023800020206     C*
023900011206IF  2C     VIDFTF        IFEQ      'P'
024000011206     C                   SETON                                        4390
024100011206     C                   GOTO      ENDCTR
024200011206E   2C                   ENDIF
024300011206     C*
024400011206X   1C                   ELSE
024500011206     C*
024600011206     C     VIDFEL        CHAIN     AZORG                              30
024700011206IF  2C     *IN30         IFEQ      *ON
024800011206     C     ORGFVA        ORNE      *BLANKS
024900011206     C                   SETON                                        4390
025000011206     C                   GOTO      ENDCTR
025100011206E   2C                   ENDIF
025200020206     C*
025300011206     C* VEDO SE LA FILIALE E' UN TERMINAL DI PARTENZA
025400011206     C                   CLEAR                   FNLV55DS
025500011206     C                   MOVEL     'P'           D55TPT
025600011206     C                   MOVEL     VIDFEL        D55LIN
025700011206     C                   MOVEL     DATEU         D55DRF
025800011206     C                   CALL      'FNLV55R'
025900011206     C                   PARM                    FNLV55DS
026000011206IF  2C     D55ERR        IFNE      ' '
026100011206     C                   CLEAR                   D55TFP
026200011206E   2C                   ENDIF
026300011206     C*
026400011206     C* SE NON E' UN PRIMO LIVELLO ERRORE
026500011206IF  2C     VIDFEL        IFNE      D55TFP
026600011206     C                   SETON                                        4390
026700011206     C                   GOTO      ENDCTR
026800011206E   2C                   ENDIF
026900011206E   1C                   ENDIF
027000011206     C*
027100011206     C* NON POSSO CHIEDERE TUTTA LA L1 E VIDPOD/VIDPOA
027200011206IF  1C     VIDL1         IFEQ      'S'
027300011206     C     VIDPOD        ANDGT     0
027400011206     C     VIDL1         OREQ      'S'
027500011206     C     VIDPOA        ANDGT     0
027600011206     C                   SETON                                        4490
027700011206     C                   GOTO      ENDCTR
027800011206E   1C                   ENDIF
027900011206     C*
028000011206     C     ENDCTR        ENDSR
