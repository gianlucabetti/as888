000100951009     H*------------------------------------------------------------------------*
000200951124     H* CONTROLLO TRAINI (FILTRO)                                      NO      *
000300951009     H*------------------------------------------------------------------------*
000400951009     H DECEDIT('0,') DATEDIT(*DMY.)
000500951009     F*------------------------------------------------------------------------*
000600951010     FTNTL30D   CF   E             WORKSTN
000700951009     FTNTLT01L  IF   E           K DISK
000800070517     FTNTLZ01L  IF   E           K DISK
000900951009     FAZORG01L  IF   E           K DISK
001000951011     FTNTST01L  IF   E           K DISK
001100951206     FTNTST03L  IF   E           K DISK
001200951206     F                                     RENAME(TNTST000:TNTST3)
001300951030     FTABEL00F  IF   E           K DISK
001400951016     F*TNTSR02LIF  E           K        DISK
001500951206     FAZCLN01L  IF   E           K DISK
001600150113     D MSG             S             78    DIM(34) CTDATA PERRCD(1)             MSG VIDEO
001700951030     D SMS             S              1    DIM(78)                              DI COMODO
001800951016     D*                   MAT        31  1               FESTIV.MATTINO
001900951206     D TAB             S              1    DIM(7) CTDATA PERRCD(1)              GG. SETTIMANA
002000951206     D TABDAY          S              9    DIM(7) ALT(TAB)
002100960123     D S1Y             S              1    DIM(10)                              SIMBOLI CALEND
002200960123     D FET             S              1    DIM(10)                              FLAG.FESTIVO
002201150113     d cod             s                   like(tblcod)
002202150113     d §key            s                   like(tblkey)
002300951009     D WLBDA8          DS
002400951009     D  G08DAT                 1      8  0
002500951009     D  G08INV                 9     16  0
002600951009     D  G08ERR                17     17
002700951009     D  G08TGI                18     22  0
002800951016     D WGIDAT          DS
002900951016     D  GI8DAT                 1      8  0
003000951016     D  GI8INV                 9     16  0
003100951016     D  GI8TGI                17     21  0
003200951009     D PARAM           DS
003300951218     D* Data foglio viaggio teorica dal
003400951023     D  PARDTD                 1      8  0
003500951218     D* data foglio viaggio teorica al
003600951023     D  PARDTA                 9     16  0
003700951218     D* linea partenza foglio viaggio
003800951016     D  V1CLNP                17     19  0
003900951019     D* data traino
004000951016     D  PARDTN                20     27  0
004100951019     D* numero traino
004200951016     D  PARTRN                28     34  0
004300951218     D* stampa f.v. reali non abbinati+bis/eccezionali
004400951220     D  V1CFVB                35     35
004500951220     D  V1CABB                36     36
004600951218     D* stampa fogli viaggio teorici non abbinati a reali
004700951220     D  V1CVUO                37     37
004800951218     D* stampa ritardi/anticipi
004900951220     D  V1CRIT                38     38
005000951218     D* stampa traini non autorizzati al pagamento
005100951220     D  V1CTAR                39     39
005200951220     D  V1CCNF                40     40
005300960304     D* FLAG PER STAMPA ELENCO TRAINI CON TRAZIONISTI FASULLI
005400960304     D  V1CFAS                41     41
005500960304     D  WOPZ                  35     41
005600951031     D* Data decorrenza traino
005700960304     D  PARDDE                42     49  0
005800960301     D* Ora per controllo anomalie
005900960304     D  PARORA                50     53  0
006000070517     D  parPDR                54     60  0
006100070517     D  V1CFFA                61     63  0
006200070517     D  parTRN1               64     70  0
006201150113     D  v1ccrt                71     71
006300070517     D*****
006400070517     D* PASSAGGIO DATI ALLA RICERCA TRAZIONISTI  - TRUL16R -
006500070517     D PARAM3          DS
006600070517     D* TIPOLOGIA T_=TRAZIONISTA
006700070517     D  PA3TIP                 1      2
006800070517     D* RAGIONE SOCIALE TRAZIONISTA
006900070517     D  PA3RSC                 3     37
007000070517     D* S.I. TRAZIONISTA
007100070517     D  PA3CSF                38     40
007200070517     D* CODICE TRAZIONISTA
007300070517     D  PA3PDR                41     47  0
007400070517     D* PA3FLG = "3" --> NON ESISTONO TRAZIONISTI PER LA CHIAVE
007500070517     D*                  ALFABETICA RICHIESTA
007600070517     D  PA3FLG                48     48
007700960131     D                 DS
007800960131     D  WDATA                  1      8
007900960131     D  GG                     1      2
008000960131     D  MM                     3      4
008100960131     D  AA                     5      8
008200951206     D                 DS
008300951206     D  CLNPOM                 1     31
008400951206     D  POM                    1     31
008500951206     D                                     DIM(31)                              FESTIV.POMER.
008600951009     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
008700951121     D DS00          E DS                  EXTNAME(TNTL00DS)
008800951019     D TNTL30        E DS                  EXTNAME(TNTL30DS)
008900971204     D DSLV55        E DS                  EXTNAME(FNLV55DS)
009000951009     D KPJBA         E DS
009100951030     D DSTV          E DS
009200960123     D DS1Y          E DS
009201150113     D DStc          E DS
009300000626     D OG143         E DS
009400951009     D                SDS
009500951009     D  V1CPGM                 1     10
009600951009     C****************************************************************
009700951009     C*  RIEPILOGO INDICATORI
009800951009     C***************************************************************
009900951011     C* 01    - ERRORE DATA TRAINO
010000951011     C* 02    - ERRORE NUMERO TRAINO
010100951218     C* 03    - ERRORE DATA FOGLIO VIAGGIO TEORICA DAL
010200951218     C* 04    - ERRORE LINEA DI PARTENZA FOGLIO VIAGGIO
010300951218     C* 05    - ERRORE DATA FOGLIO VIAGGIO TEORICA AL
010400951220     C* 06/08 - ERRORE OPZIONI DI STAMPA
010500951009     C* 30    - COMODO
010600951012     C* 31    - COMODO
010700951009     C* 90    - ERRORE GENERICO DSPF
010800951009     C***************************************************************
010900960220     C                   IN        TNTL30
011000960220     C                   CLEAR                   WLBDA8
011100960220     C                   MOVE      D30DAT        G08INV
011200960220     C                   MOVE      '3'           G08ERR
011300960220     C                   CALL      'XSRDA8'
011400960220     C                   PARM                    WLBDA8
011500960220     C                   MOVE      G08DAT        V1CULT
011600951009     C     INIZIO        TAG
011700951011     C                   CLEAR                   V1CDTN
011800951011     C                   CLEAR                   V1CTRN
011900951023     C                   CLEAR                   V1CDTD
012000951023     C                   CLEAR                   V1CDTA
012100951011     C                   CLEAR                   V1CLNP
012200951011     C                   CLEAR                   V1DLNP
012300951206     C                   CLEAR                   V1DGSE
012400951019     C                   MOVE      'S'           V1CFVB
012500951220     C                   MOVE      'N'           V1CABB
012600951220     C                   MOVE      'S'           V1CVUO
012700951218     C                   MOVE      ' '           V1CRIT
012800951218     C                   MOVE      ' '           V1CTAR
012900951220     C                   MOVE      ' '           V1CCNF
013000960304     C                   MOVE      ' '           V1CFAS
013100070517     C                   CLEAR                   V1CPDR
013200070517     C                   CLEAR                   V1DPDR
013300070517     C                   CLEAR                   V1CFFA
013400070517     C                   CLEAR                   V1CTRN1
013401150113     C                   CLEAR                   V1Ccrt
013402150113     C                   CLEAR                   V1dcrt
013500951009     C* Emissione formato video
013600951009     C     EMISS         TAG
013700951011     C                   EXFMT     TL30D01
013800951009     C                   SETOFF                                       90
013900951009     C                   CLEAR                   V1ZMSG
014000951009     C     *INKC         CABEQ     *ON           FINE
014100951009     C* Richiamo routine controlli
014200951018     C                   EXSR      CTRD01
014300951019    1C     *IN90         IFEQ      *ON
014400951009     C     *INKF         OREQ      *OFF
014500951009     C                   GOTO      EMISS
014600951019    1C                   END
014700951124     C* Richiamo batch controllo traini effettuati
014800951019    1C     *INKF         IFEQ      *ON
014900960301     C                   CLEAR                   PARORA
015000951009     C                   MOVEL     PARAM         KPJBU
015100010823     C                   MOVE      'TL41'        KCOAZ
015200951009     C                   CALL      'BCH10'
015300951009     C                   PARM                    KPJBA
015400951218     C* Se lanciato controllo per dtt e lnp f.v.=0 memorizzo data f.v.
015500951218     C* teorica al da usare in routine controlli nel caso di eventuali
015600951206     C* altri lanci per dtt (Infatti l'unico caso in cui il batch di
015700951218     C* controllo traini aggiorna la data ultimo controllo è per i con-
015800951218     C* trolli per data f.v. teorica di tutte le linee di partenza f.v)
015900951023    2C     PARDTA        IFNE      *ZEROS
016000951019     C     V1CLNP        ANDEQ     *ZEROS
016100951023     C     PARDTA        ANDGT     W30DAT
016200951023     C                   MOVE      PARDTA        W30DAT
016300960220     C                   CLEAR                   WLBDA8
016400960220     C                   MOVE      W30DAT        G08INV
016500960220     C                   MOVE      '3'           G08ERR
016600960220     C                   CALL      'XSRDA8'
016700960220     C                   PARM                    WLBDA8
016800960220     C                   MOVE      G08DAT        V1CULT
016900951019    2C                   END
017000951009     C                   GOTO      INIZIO
017100951019    1C                   END
017200951009     C     FINE          TAG
017300951019     C*
017400971204     C                   CLEAR                   DSLV55
017500971204     C                   MOVEL     'C'           D55TLA
017600971204     C                   CALL      'FNLV55R'
017700971204     C                   PARM                    DSLV55
017800971204     C*
017900951009     C                   SETON                                        LR
018000951009     C****************  CONTROLLI VIDEO   *****************************
018100951009     C     CTRD01        BEGSR
018200951009     C                   SETOFF                                       010203
018300951019     C                   SETOFF                                       040506
018400150113     C                   SETOFF                                       070812
018500070517     C                   SETOFF                                       111009
018600951023     C                   CLEAR                   PARDTD
018700951023     C                   CLEAR                   PARDTA
018800951016     C                   CLEAR                   V1DLNP
018900951011     C                   CLEAR                   PARDTN
019000951011     C                   CLEAR                   PARTRN
019100951031     C                   CLEAR                   PARDDE
019200951206     C                   CLEAR                   V1DGSE
019300070517     c                   clear                   parpdr
019400070517     c                   clear                   partrn1
019500951121     C**** VERIFICO CHE IL CONTROLLO SIA PER DTT OPPURE PER DATA TRAINO
019600951218     C* Data f.v. teorica o Data traino obbligatoria
019700951023    1C     V1CDTD        IFEQ      *ZEROS
019800951121     C     V1CDTN        ANDEQ     *ZEROS
019900951009     C                   MOVEL     MSG(1)        V1ZMSG
020000951121     C                   SETON                                        010590
020100951009     C                   GOTO      ENDCTR
020200951009    1C                   END
020300951218     C* Errore se immessi sia data f.v. teorica che Data traino
020400951023    1C     V1CDTD        IFNE      *ZEROS
020500951121     C     V1CDTN        ANDNE     *ZEROS
020600951011     C                   MOVEL     MSG(2)        V1ZMSG
020700951121     C                   SETON                                        010590
020800951011     C                   GOTO      ENDCTR
020900951011    1C                   END
021000951218     C**** CONTROLLO TRAINI PER DATA F.V.TEORICA/LINEA DI PARTENZA F.V.
021100951218     C* Data foglio viaggio teorica dal
021200951023    1C     V1CDTD        IFNE      *ZEROS
021300951019     C* controllo prima di tutto se è possibile in questo momento
021400951023     C* lanciare controlli per dtt verificando flag blocco di dtaara
021500951020     C                   IN        TNTL30
021600951206    2C     D30BLC        IFEQ      *BLANKS
021700951020     C     D30BLC        OREQ      '2'
021800951206    3C     D30DAT        IFGT      W30DAT
021900951020     C                   MOVE      D30DAT        W30DAT
022000951206    3C                   END
022100951206   X2C                   ELSE
022200951020     C                   SETON                                        0190
022300951020     C                   MOVEL     MSG(4)        V1ZMSG
022400951020     C                   GOTO      ENDCTR
022500951206    2C                   END
022600951016     C                   CLEAR                   WLBDA8
022700951023     C                   MOVE      V1CDTD        G08DAT
022800951016     C                   CALL      'XSRDA8'
022900951016     C                   PARM                    WLBDA8
023000951016    2C     G08ERR        IFEQ      '1'
023100951017     C                   MOVEL     MSG(3)        V1ZMSG
023200951016     C                   SETON                                        0190
023300951016     C                   GOTO      ENDCTR
023400951016    2C                   END
023500951023     C                   MOVE      G08DAT        V1CDTD
023600951023     C                   MOVE      G08INV        PARDTD
023700951023     C                   MOVE      G08TGI        TGIDTD
023800951016     C**                   MOVE G08INV    KDAT
023900951016     C**                   EXSR CTRFES
024000951023     C**Errore data tratta -> Richiesto controllo di un giorno festivo
024100951016     C**         COMFES    IFEQ 'S'
024200951016     C**                   SETON                     030490
024300951016     C**                   MOVELMSG,14    V1ZMSG
024400951016     C**                   GOTO ENDCTR
024500951016     C**                   END
024600951218     C* Impossibile controllare per data f.v. teorica se non è stato
024700951023     C* controllato il giorno precedente
024800951023    2C     PARDTD        IFGT      W30DAT
024900951016     C* calcolo data ultimo controllo espressa in numero giorni per
025000951218     C* poterla confrontare con data f.v teorica
025100951016     C                   CLEAR                   WLBDA8
025200951019     C                   MOVE      W30DAT        G08INV
025300951016     C                   MOVE      '3'           G08ERR
025400951016     C                   CALL      'XSRDA8'
025500951016     C                   PARM                    WLBDA8
025600951016     C                   ADD       1             G08TGI
025700951023    3C     G08TGI        IFLT      TGIDTD
025800951016     C                   CLEAR                   WGIDAT
025900951016     C                   MOVE      G08TGI        GI8TGI
026000951016     C                   CALL      'XSRGI8'
026100951016     C                   PARM                    WGIDAT
026200951016     C* confronto le due date
026300951016     C**                   EXSR CTRDAT
026400951016     C**         WERR      IFEQ '1'
026500960131     C                   MOVEA     MSG(13)       SMS
026600960131     C                   MOVE      GI8DAT        WDATA
026700960131     C                   MOVEA     GG            SMS(57)
026800960131     C                   MOVEA     MM            SMS(60)
026900960131     C                   MOVEA     AA            SMS(63)
027000960131     C                   MOVEA     SMS           V1ZMSG
027100951016     C                   SETON                                        0190
027200951016     C                   GOTO      ENDCTR
027300951016     C**                   END
027400951016    3C                   END
027500080919    2C                   END
027501080919     c* Errore data dal se già pulita nella griglia
027502080919     c                   if        pardtd<=wdtpul
027506080912     C                   eval      v1zmsg=msg(33)
027507080912     c                   seton                                        0190
027508080912     c                   goto      endctr
027511080912     c                   endif
027600951218     C* Data foglio viaggio teorica al
027700951023    2C     V1CDTA        IFNE      *ZEROS
027800951016     C                   CLEAR                   WLBDA8
027900951023     C                   MOVE      V1CDTA        G08DAT
028000951016     C                   CALL      'XSRDA8'
028100951016     C                   PARM                    WLBDA8
028200951016    3C     G08ERR        IFEQ      '1'
028300951017     C                   MOVEL     MSG(3)        V1ZMSG
028400951016     C                   SETON                                        0290
028500951016     C                   GOTO      ENDCTR
028600951016    3C                   END
028700951023     C                   MOVE      G08DAT        V1CDTA
028800951023     C                   MOVE      G08INV        PARDTA
028900951023    3C     PARDTD        IFGT      PARDTA
029000951016     C                   MOVEL     MSG(17)       V1ZMSG
029100951016     C                   SETON                                        010290
029200951016     C                   GOTO      ENDCTR
029300951019    3C                   END
029400951121     C* Controllo che periodo richiesto sia <= 31 Giorni
029500951121     C     G08TGI        SUB       TGIDTD        W0050             5 0
029600951206    3C     W0050         IFGT      30
029700951121     C                   MOVEL     MSG(21)       V1ZMSG
029800951121     C                   SETON                                        010290
029900951121     C                   GOTO      ENDCTR
030000951206    3C                   END
030100960131     C* Se controllo a partire da una data già controllata si può al
030200960131     C* massimo controllare fino alla data ultimo controllo
030300960131     C* (Questo per permettere al batch controllo traini di aggiornare
030400960131     C* correttamente le date controllo)
030500960131     C     PARDTD        IFLE      W30DAT
030600960131     C     PARDTA        ANDGT     W30DAT
030700960131     C                   MOVEA     MSG(27)       SMS
030800960131     C                   MOVE      V1CDTD        WDATA
030900960131     C                   MOVEA     GG            SMS(32)
031000960131     C                   MOVEA     MM            SMS(35)
031100960131     C                   MOVEA     AA            SMS(38)
031200960131     C                   MOVE      W30DAT        GG
031300960131     C                   MOVE      W30DAT        W0040             4 0
031400960131     C                   MOVEL     W0040         MM
031500960131     C                   MOVEA     GG            SMS(74)
031600960131     C                   MOVEA     MM            SMS(77)
031700960131     C                   MOVEA     SMS           V1ZMSG
031800960131     C                   SETON                                        010290
031900960131     C                   GOTO      ENDCTR
032000960131     C                   END
032100951019   X2C                   ELSE
032200951023     C                   MOVE      PARDTD        PARDTA
032300951016    2C                   END
032400951218     C* Linea di partenza foglio viaggio
032500951016    2C     V1CLNP        IFNE      *ZEROS
032600951016     C     V1CLNP        CHAIN     AZORG01L                           30
032700951016     C* Inesistente in organigramma o annullato
032800951016    3C     *IN30         IFEQ      *ON
032900951016     C     ORGFVA        ORNE      *BLANKS
033000971204     C     ORGFAG        ORNE      'A'
033100971204     C     ORGFAG        ANDNE     'F'
033200951016     C                   SETON                                        0390
033300951016     C                   MOVEL     MSG(11)       V1ZMSG
033400951016     C                   GOTO      ENDCTR
033500951016    3C                   END
033600951016     C*
033700951016     C                   MOVEL     ORGDES        V1DLNP
033800000626     C                   MOVEL     ORGDE3        OG143
033900000626     C* Controllo se è terminal solo se linea immessa non è p.o. poste
034000000626   2AC     §OGPT         IFNE      'S'
034100971204     C* Non è un terminal di partenza
034200971204     C                   CLEAR                   DSLV55
034300971204     C                   MOVEL     'P'           D55TPT
034400971204     C                   MOVEL     V1CLNP        D55LIN
034500971204     C                   MOVEL     PARDTD        D55DRF
034600971204     C                   CALL      'FNLV55R'
034700971204     C                   PARM                    DSLV55
034800971204    3C     D55ERR        IFNE      ' '
034900971204     C     D55TFP        ORNE      ORGFIL
035000951016     C                   SETON                                        0390
035100951016     C                   MOVEL     MSG(12)       V1ZMSG
035200951016    3C                   END
035300000626   2AC                   END
035400951016    2C                   END
035500951016    1C                   END
035600951124     C**** CONTROLLO TRAINI PER N R . / D A T A   T R A I N O
035700951121     C* CONTROLLI DATA TRAINO
035800951121    1C     V1CDTN        IFNE      *ZEROS
035900951121     C                   CLEAR                   WLBDA8
036000951121     C                   MOVE      V1CDTN        G08DAT
036100951121     C                   CALL      'XSRDA8'
036200951121     C                   PARM                    WLBDA8
036300951121    2C     G08ERR        IFEQ      '1'
036400951121     C* Data traino errata
036500951121     C                   MOVEL     MSG(3)        V1ZMSG
036600951121     C                   SETON                                        0590
036700951121     C                   GOTO      ENDCTR
036800951121    2C                   END
036900951121     C                   MOVE      G08DAT        V1CDTN
037000951121     C                   MOVE      G08INV        PARDTN
037100951121     C                   MOVEL     G08DAT        W006A             6
037200951121     C                   MOVE      G08DAT        W002A             2
037300951121     C                   MOVE      W002A         W006A
037400951121     C                   CALL      'XGIOSE1'
037500951121     C                   PARM                    W006A             6
037600951121     C                   PARM                    GIOSET            1
037700951206     C     GIOSET        LOOKUP    TAB           TABDAY                   30
037800951206     C                   MOVEL     TABDAY        V1DGSE
037900951206     C* Se il giorno richiesto è festivo verifico che esista la griglia
038000951206     C* perchè se non esiste il controllo non è ammesso
038100951206     C                   MOVEL     PARDTN        KANN
038200951206     C                   MOVE      PARDTN        W0040             4 0
038300951206     C                   MOVEL     W0040         KMES
038400960131     C                   MOVE      W0040         GI                2 0
038500951206     C     KCLN          CHAIN     AZCLN01L                           30
038600960123    2C     *IN30         IFEQ      *OFF
038700960131     C     POM(GI)       ANDNE     ' '
038800960123     C                   Z-ADD     1             XY                2 0
038900960131     C     POM(GI)       LOOKUP    S1Y(XY)                                30
039000960123    3C   30FET(XY)       IFNE      *BLANKS
039100951206     C* Il giorno è festivo: controllo se esiste la griglia
039200951206     C     PARDTN        SETLL     TNTST03L                               30
039300960123    4C     *IN30         IFEQ      *OFF
039400951206     C                   MOVEL     MSG(22)       V1ZMSG
039500951206     C                   SETON                                        0590
039600951206     C                   GOTO      ENDCTR
039700960206   X4C                   ELSE
039800960206     C                   CLEAR                   W001A
039900960206     C     PARDTN        READE     TNTST03L                               30
040000960206    5C     *IN30         DOWEQ     *OFF
040100960220    6C     TSTATB        IFNE      *BLANKS
040200960206     C     PARDTN        READE     TNTST03L                               30
040300960206   X6C                   ELSE
040400960206     C                   MOVE      '1'           W001A             1
040500960206     C                   SETON                                        30
040600960206    6C                   END
040700960206    5C                   ENDDO
040800960206     C     W001A         IFEQ      *BLANKS
040900960206     C                   MOVEL     MSG(22)       V1ZMSG
041000960206     C                   SETON                                        0590
041100960206     C                   GOTO      ENDCTR
041200960206     C                   END
041300960123    4C                   END
041400960123    3C                   END
041500960123    2C                   END
041600951121     C* CONTROLLI NUMERO TRAINO
041700951121     C* Gestione '?' su numero traino
041800951121    2C     V1CTRN        IFNE      *BLANKS
041900951121     C     V1CTRN        ANDNE     *ZEROS
042000951121     C     '?'           SCAN      V1CTRN                                 30
042100951121    3C     *IN30         IFEQ      *ON
042200951121     C                   CLEAR                   DS00
042300951121     C                   Z-ADD     PARDTN        D00DDE
042400951121     C                   MOVEL     DS00          KPJBU
042500951121     C                   CALL      'TNTL17R'
042600951121     C                   PARM                    KPJBA
042700951121     C                   MOVEL     KPJBU         DS00
042800951121     C                   MOVE      D00TRN        V1CTRN
042900951121    3C                   END
043000951121    2C                   END
043100951121     C*
043200951121    2C     V1CTRN        IFEQ      *BLANKS
043300951121     C     V1CTRN        OREQ      *ZEROS
043400951121     C* Numero traino obbligatorio
043500951121     C                   SETON                                        0490
043600951121     C                   MOVEL     MSG(15)       V1ZMSG
043700951121     C                   GOTO      ENDCTR
043800951121   X2C                   ELSE
043900951016     C                   TESTN                   V1CTRN               30
044000951121    3C     *IN30         IFEQ      *ON
044100951016     C                   MOVE      V1CTRN        W001A             1
044200951121    4C     W001A         IFLT      '0'
044300951016     C                   SETOFF                                       30
044400951121    4C                   END
044500951121    3C                   END
044600951121    3C     *IN30         IFEQ      *OFF
044700951016     C                   SETON                                        0490
044800951016     C                   MOVEL     MSG(7)        V1ZMSG
044900951016     C                   GOTO      ENDCTR
045000951121    3C                   END
045100951016     C                   MOVE      V1CTRN        PARTRN
045200951121    2C                   END
045300951016     C* Controllo esistenza in griglia del traino richiesto
045400960201     C     KTRN          CHAIN     TNTST01L                           30
045500951016     C* Se non trovata griglia controllo:
045600951016     C* - che esista in anagrafica traini non annullato
045700951030     C* - che il traino sia utilizzabile nei controlli
045800951030     C*   (dipende dal tipo)
045900960424     C* - che il traino non sia un bis SE NrTraino < 9000000
046000951016     C* - che non sia scaduto rispetto alla data richiesta
046100951016     C* - che sia previsto nel giorno richiesto
046200960201    2C     *IN30         IFEQ      *ON
046300951016     C     KTRN          SETLL     TNTLT01L                               30
046400951016     C   30KTRN          READE     TNTLT01L                               31
046500951016     C  N30PARTRN        READPE    TNTLT01L                               31
046600951121    3C  N30*IN31         DOWEQ     *OFF
046700951016     C     TLTATB        ANDNE     *BLANKS
046800951016     C     PARTRN        READPE    TNTLT01L                               31
046900951121    3C                   ENDDO
047000951025     C* non trovato o annullato: traino inesistente o annullato o non
047100951025     C* decorrente
047200951121    3C     *IN31         IFEQ      *ON
047300960220     C     TLTATB        ORNE      *BLANKS
047400951016     C                   SETON                                        0490
047500951016     C                   MOVEL     MSG(8)        V1ZMSG
047600951016     C                   GOTO      ENDCTR
047700951121    3C                   END
047800951121     C* tipo traino da non utilizzare nei controlli di sede
047900951030     C                   MOVEL     'TV'          KCOD
048000951030     C                   MOVEL(P)  TLTTTR        KKEY
048100951122     C                   MOVEL     'S'           §TVUTC
048200951030     C     KTAB          CHAIN     TABEL00F                           31
048300951121    3C     *IN31         IFEQ      *OFF
048400951030     C     TBLFLG        ANDEQ     *BLANKS
048500951030     C                   MOVEL     TBLUNI        DSTV
048600951121    3C                   END
048700951121    3C     §TVUTC        IFEQ      *BLANKS
048800951030     C                   SETON                                        0490
048900951030     C                   MOVEA     MSG(19)       SMS
049000951030     C                   MOVEA     §TVDES        SMS(31)
049100951030     C                   MOVEA     SMS           V1ZMSG
049200951030     C                   GOTO      ENDCTR
049300951121    3C                   END
049400960424     C* Traino bis < 9000000     e non esiste in griglia: controllo
049500951115     C* non ammesso
049600951121    3C     TLTBIS        IFNE      ' '
049700960424     C     TLTTRN        ANDLT     9000000
049800951115     C                   SETON                                        0490
049900951115     C                   MOVEL     MSG(20)       V1ZMSG
050000951115     C                   GOTO      ENDCTR
050100951121    3C                   END
050200951016     C* traino scaduto
050300951121    3C     *IN30         IFEQ      *OFF
050400951030     C     TLTDSC        ANDLT     PARDTN
050500951016     C                   SETON                                        0490
050600951016     C                   MOVEL     MSG(9)        V1ZMSG
050700951016     C                   GOTO      ENDCTR
050800951121    3C                   END
050900951016     C* traino non previsto nel giorno richiesto
051000951016     C     GIOSET        SCAN      TLTGSE                                 30
051100951121    3C     *IN30         IFEQ      *OFF
051200951016     C                   SETON                                        0490
051300951016     C                   MOVEL     MSG(10)       V1ZMSG
051400951016     C                   GOTO      ENDCTR
051500951121    3C                   END
051600951031     C* In questo caso passo al batch anche la data decorrenza
051700951031     C                   Z-ADD     TLTDDE        PARDDE
051800960201   X2C                   ELSE
051900960201     C* Traino annullato
052000960220     C     TSTATB        IFNE      *BLANKS
052100960201     C                   SETON                                        0490
052200960201     C                   MOVEL     MSG(28)       V1ZMSG
052300960201     C                   GOTO      ENDCTR
052400960201     C                   END
052500951121    2C                   END
052600951121    1C                   END
052700951013     C*
052800951218     C**** SE DATA F.V. TEORICA DAL = 0 --> DATA F.V. TEOR. AL E LINEA
052900951218     C*    DI PARTENZA DEVONO ESSERE = 0
053000951023     C     V1CDTD        IFEQ      *ZEROS
053100951016     C     V1CLNP        ANDNE     *ZEROS
053200951016     C                   SETON                                        010390
053300951016     C                   MOVEL     MSG(6)        V1ZMSG
053400951016     C                   GOTO      ENDCTR
053500951016     C                   END
053600951023     C     V1CDTD        IFEQ      *ZEROS
053700951023     C     V1CDTA        ANDNE     *ZEROS
053800951016     C                   SETON                                        010290
053900951016     C                   MOVEL     MSG(16)       V1ZMSG
054000951016     C                   GOTO      ENDCTR
054100951016     C                   END
054200951121     C**** SE DATA TRAINO = 0 --> NUMERO TRAINO DEVE ESSERE = 0
054300951121     C     V1CDTN        IFEQ      *ZEROS
054400951121     C     V1CTRN        ANDNE     *ZEROS
054500951121     C     V1CTRN        ANDNE     *BLANKS
054600951016     C                   SETON                                        040590
054700951012     C                   MOVEL     MSG(5)        V1ZMSG
054800951019     C                   GOTO      ENDCTR
054900951012     C                   END
055000951218     C* VERIFICO CHE ALMENO UNA DELLE OPZIONI DI STAMPA SIA DIVERSA DA
055100951218     C* BLANKS
055200951218     C     WOPZ          IFEQ      *BLANKS
055300951019     C                   SETON                                        0690
055400951019     C                   MOVEL     MSG(18)       V1ZMSG
055500951220     C                   GOTO      ENDCTR
055600951019     C                   END
055700960305     C* NON E' POSSIBILE RICHIEDERE LA STAMPA DEI FV NON ABBINATI
055800960305     C* SE CONTROLLI PER N/DATA TRAINO
055900960305     C     V1CFVB        IFEQ      'S'
056000960305     C     PARDTD        ANDEQ     *ZEROS
056100960305     C                   SETON                                        0690
056200960305     C                   MOVEL     MSG(29)       V1ZMSG
056300960305     C                   GOTO      ENDCTR
056400960305     C                   END
056500951220     C* SE RICHIESTA LA STAMPA DEI F.V. REALI NON ABBINATI CONTROLLO
056600951220     C* CHE IL SUO SOTTOPARAMETRO SIA DIVERSO DA BLANK
056700951220     C     V1CFVB        IFEQ      'S'
056800951220     C     V1CABB        ANDEQ     *BLANKS
056900951220     C                   SETON                                        0790
057000951220     C                   MOVEL     MSG(23)       V1ZMSG
057100951220     C                   GOTO      ENDCTR
057200951220     C                   END
057300951220     C* SE IMMESSO SOTTOPARAMETRO DEVE ESSERCI ANCHE IL PARAMETRO
057400951220     C* PRINCIPALE
057500951220     C     V1CABB        IFNE      *BLANKS
057600951220     C     V1CFVB        ANDEQ     *BLANKS
057700951220     C                   SETON                                        0790
057800951220     C                   MOVEL     MSG(25)       V1ZMSG
057900951220     C                   GOTO      ENDCTR
058000951220     C                   END
058100070517     C* SE RICHIESTA LA STAMPA DEI RITARDI CONTROLLO I SUOI SOTTOPARA-
058200070517     C* METRI
058300070517     c                   if        v1crit<>*blanks
058400070522     c* la selezione dei sottoparametri della stampa dei ritardi non
058500070522     c* è gestita se richiesti i controlli per data traino
058600070522     c                   if        v1cdtn>0 and (v1cpdr<>*blanks or
058700150113     c                             v1cffa>0 or v1ctrn1<>*blanks or
058701150113     c                             v1ccrt<>*blanks)
058800070522     c                   seton                                        90
058900070522     c                   if        v1cpdr<>*blanks
059000070522     c                   seton                                        11
059100070522     c                   endif
059200070522     c                   if        v1cffa>0
059300070522     c                   seton                                        10
059400070522     c                   endif
059500070522     c                   if        v1ctrn1<>*blanks
059600070522     c                   seton                                        09
059700070522     c                   endif
059701150113     c                   if        v1ccrt<>*blanks
059702150113     c                   seton                                        12
059703150113     c                   endif
059800070522     c                   movel     msg(32)       v1zmsg
059900070522     c                   goto      endctr
060000070522     c                   endif
060100070517     c* trazionista
060200070517    1C     V1CPDR        IFNE      *BLANKS
060300070517    1C     V1CPDR        ANDNE     *ZEROS
060400070517     C* RICERCA
060500070517     C     '?'           SCAN      V1CPDR                                 30
060600070517    2C     *IN30         IFEQ      *ON
060700070517     C                   EVAL      PA3TIP = 'T '                                PAR-1
060800070517     C                   clear                   PA3RSC                         PAR-2
060900070517     C                   CLEAR                   PA3CSF                         PAR-3
061000070517     C                   CLEAR                   PA3PDR                         PAR-4
061100070517     C                   CLEAR                   PA3FLG                         PAR-5
061200070517     C                   MOVEL     PARAM3        KPJBU
061300070517     C                   CALL      'TRUL16R'
061400070517     C                   PARM                    KPJBA
061500070517     C                   MOVEL     KPJBU         PARAM3
061600070517     C                   MOVEL     PA3PDR        V1CPDR                         CODICE
061700070517     C                   MOVEL     PA3RSC        V1DPDR                         RAG.SOCIALE
061800070517    2C                   ENDIF
061900070517     C*
062000070517     C                   TESTN                   V1CPDR               31
062100070517     C                   MOVE      V1CPDR        W001A             1
062200070517    2C     *IN31         IFEQ      *OFF
062300070517     C     W001A         ORLT      '0'
062400070517     C                   MOVEL     MSG(30)       V1ZMSG
062500070517     C                   SETON                                        9011
062600070517     C                   GOTO      ENDCTR
062700070517   X2C                   ELSE
062800070517     C* CONTROLLO SULL'ANAGRAFICA
062900070517     C                   MOVE      V1CPDR        ktlpdr
063000070517     C     KTNTLZ        CHAIN     TNTLZ01L                           31
063100070517    3C     *IN31         IFEQ      *ON
063200070517     C     TLZATB        ORNE      ' '
063300070517     C                   MOVEL     MSG(30)       V1zMSG
063400070517     C                   SETON                                        9011
063500070517     C                   GOTO      ENDCTr
063600070517   X3C                   ELSE
063700070517     C                   MOVEL     TLZRSC        V1DPDR
063800070517     C                   MOVE      v1cpdr        parPDR
063900070517    3C                   ENDIF
064000070517    2C                   ENDIF
064100070517    1C                   ENDIF
064200070517     c* Filiale fermata arrivo
064300070517     c                   if        v1cffa>0
064400070517     C     V1Cffa        CHAIN     AZORG01L                           30
064500070517     C* Inesistente in organigramma o annullato
064600070517    3C     *IN30         IFEQ      *ON
064700070517     C     ORGFVA        ORNE      *BLANKS
064800070517     C     ORGFAG        ORNE      'A'
064900070517     C     ORGFAG        ANDNE     'F'
065000070517     C                   SETON                                        1090
065100070517     C                   MOVEL     MSG(31)       V1ZMSG
065200070517     C                   GOTO      ENDCTR
065300070517    3C                   END
065400070517     c                   endif
065500070517     c* numero traino
065600070517     C* Gestione '?' su numero traino
065700070517    2C     V1CTRN1       IFNE      *BLANKS
065800070517     C     V1CTRN1       ANDNE     *ZEROS
065900070517     C     '?'           SCAN      V1CTRN1                                30
066000070517    3C     *IN30         IFEQ      *ON
066100070517     C                   CLEAR                   DS00
066200070517     C                   Z-ADD     PARDde        D00DDE
066300070517     C                   MOVEL     DS00          KPJBU
066400070517     C                   CALL      'TNTL17R'
066500070517     C                   PARM                    KPJBA
066600070517     C                   MOVEL     KPJBU         DS00
066700070517     C                   MOVE      D00TRN        V1CTRN1
066800070517    3C                   END
066900070521     C                   TESTN                   V1CTRN1              30
067000070517    3C     *IN30         IFEQ      *ON
067100070521     C                   MOVE      V1CTRN1       W001A             1
067200070517    4C     W001A         IFLT      '0'
067300070517     C                   SETOFF                                       30
067400070517    4C                   END
067500070517    3C                   END
067600070517    3C     *IN30         IFEQ      *OFF
067700070521     C                   SETON                                        0990
067800070517     C                   MOVEL     MSG(7)        V1ZMSG
067900070517     C                   GOTO      ENDCTR
068000070517    3C                   END
068100070517     c                   move      v1ctrn1       partrn1
068200070517    2C                   END
068201150113     c* Causale ritardo
068202150113     C* Gestione '?' su causale ritardo
068203150113    2c                   if        v1ccrt<>*blanks
068204150113    3c                   if        v1ccrt='?'
068205150113     C                   CLEAR                   §KEY
068206150113     c                   eval      cod='TC'
068207150113     C                   CALL      'X§TABER'
068208150113     C                   PARM                    CODUT
068209150113     C                   PARM                    COD
068210150113     C                   PARM                    §KEY
068211150113     C                   PARM                    §DES             25
068212150113     C                   MOVEL     §KEY          v1ccrt
068213150113     C                   MOVEL     §des          v1dcrt
068214150113    3c                   endif
068215150113    3c                   if        v1ccrt<>*blanks
068216150113     C* tipo traino da non utilizzare nei controlli di sede
068217150113     C                   MOVEL     'TC'          KCOD
068218150113     C                   MOVEL(P)  V1CCRT        KKEY
068219150113     C     KTAB          CHAIN     TABEL00F
068220150113    4C                   IF        NOt %found(tabel00f)
068221150113     C                   SETON                                        1290
068222150113     C                   MOVEL     MSG(34)       V1ZMSG
068223150113     C                   GOTO      ENDCTR
068224150113     c                   endif
068225150113     c                   movel     tbluni        dstc
068226150113     c                   movel     §TCDES        v1dcrt
068227150113    3c                   endif
068228150113    2c                   endif
068300070517     c                   endif
068400951220     C* SE RICHIESTA LA STAMPA DEI TRAINI NON AUTORIZZATI AL PAGAMENTO
068500951220     C* CONTROLLO CHE IL SUO SOTTOPARAMETRO SIA DIVERSO DA BLANK
068600951220     C     V1CTAR        IFEQ      'S'
068700951220     C     V1CCNF        ANDEQ     *BLANKS
068800951220     C                   SETON                                        0890
068900951220     C                   MOVEL     MSG(24)       V1ZMSG
069000951220     C                   GOTO      ENDCTR
069100951220     C                   END
069200951220     C* SE IMMESSO SOTTOPARAMETRO DEVE ESSERCI ANCHE IL PARAMETRO
069300951220     C* PRINCIPALE
069400951220     C     V1CTAR        IFEQ      *BLANKS
069500951220     C     V1CCNF        ANDNE     *BLANKS
069600951220     C                   SETON                                        0890
069700951220     C                   MOVEL     MSG(26)       V1ZMSG
069800951220     C                   END
069900951012     C*
070000951009     C     ENDCTR        ENDSR
070100951013     C******************  CONTROLLO DATA RICHIESTA  *******************
070200951016     C*          CTRDAT    BEGSR
070300951016     C*                    MOVE ' '       WERR    1
070400951016     C*                    MOVELG08TGI    WTGI
070500951023    1C*          WTGI      DOWLTTGIDTD
070600951013     C* Determino a che data corrisponde WTGI
070700951016     C*                    CLEARWGIDAT
070800951016     C*                    MOVE WTGI      GI8TGI
070900951016     C*                    CALL 'XSRGI8'
071000951016     C*                    PARM           WGIDAT
071100951016     C*                    MOVE GI8INV    KDAT
071200951013     C* Controllo se data festiva
071300951016     C*                    EXSR CTRFES
071400951013     C* Data non festiva: segnalo errore e forzo uscita dal ciclo
071500951016    2C*          COMFES    IFEQ 'N'
071600951016     C*                    MOVE '1'       WERR
071700951023     C*                    Z-ADDTGIDTD    WTGI
071800951016   X2C*                    ELSE
071900951013     C* Data festiva: aggiungo 1 a WTGI per controllare la data
072000951013     C* successiva
072100951016     C*                    ADD  1         WTGI
072200951016    2C*                    END
072300951016    1C*                    ENDDO
072400951016     C*                    ENDSR
072500951013     C***************   CONTROLLO SE FESTIVO  *************************
072600951016     C*          CTRFES    BEGSR
072700951016     C*                    MOVE ' '       COMFES  1
072800951020     C* Verifico se esiste griglia per questa data/LNP
072900951031     C* Attenzione! La key non è più per dtt/lnp ma per tfp/dtt!!!!!
073000951016    1C*          V1CLNP    IFEQ *ZEROS
073100951016     C*          KDAT      SETLLTNTSR02L                 30
073200951016     C*                    ELSE
073300951016     C*          KTSR      SETLLTNTSR02L                 30
073400951016    1C*                    END
073500951013     C* Esiste griglia --> non è festivo
073600951016    1C*          *IN30     IFEQ *ON
073700951016     C*                    MOVE 'N'       COMFES
073800951016   X1C*                    ELSE
073900951013     C* Verifico se data festiva leggendo il calendario Bartolini
074000951016     C*                    MOVE V1CLNP    KTFP
074100951016     C*                    MOVE *ZEROS    KTFA
074200951016     C*                    MOVELKDAT      KANN
074300951016     C*                    MOVE KDAT      W0040   40
074400951016     C*                    MOVELW0040     KMES
074500960131     C*                    MOVE W0040     GI      20
074600951016     C*          KCLN      CHAINAZCLN01L             30
074700951016    2C*          *IN30     IFEQ *OFF
074800960131    3C*          MAT,GI    IFEQ 'F'
074900960131     C*          POM,GI    ANDEQ'F'
075000951013     C* Data festiva
075100951016     C*                    MOVE 'S'       COMFES
075200951016   X3C*                    ELSE
075300951013     C* Data non festiva
075400951016     C*                    MOVE 'N'       COMFES
075500951016    3C*                    END
075600951016    2C*                    END
075700951016    1C*                    END
075800951016     C*                    ENDSR
075900951009     C****************  ROUTINE INIZIALE  *****************************
076000951009     C     *INZSR        BEGSR
076100951009     C     *ENTRY        PLIST
076200951009     C                   PARM                    KPJBA
076300951009     C*
076400951009     C                   Z-ADD     1             CODUT
076500951009     C                   CALL      'X§PARUT'
076600951009     C                   PARM                    UT§DSE
076700951009     C                   MOVEL     RAGUT         V1CRSU
076800951009     C*
076900951016     C*          *LIKE     DEFN G08TGI    WTGI
077000951206     C     *LIKE         DEFINE    CLNANN        KANN
077100951206     C     *LIKE         DEFINE    CLNMES        KMES
077200951206     C     *LIKE         DEFINE    CLNTFP        KTFP
077300951206     C     *LIKE         DEFINE    CLNTFA        KTFA
077400951016     C*          *LIKE     DEFN GI8INV    KDAT
077500951023     C     *LIKE         DEFINE    G08TGI        TGIDTD
077600951019     C     *LIKE         DEFINE    D30DAT        W30DAT
077700951030     C     *LIKE         DEFINE    TBLCOD        KCOD
077800951030     C     *LIKE         DEFINE    TBLKEY        KKEY
077900070517     C     *LIKE         DEFINE    TLZPDR        KTLPDR
078000070517     C     *LIKE         DEFINE    TLZTIP        KTLTIP
078100951013     C*
078200951012     C     KTRN          KLIST
078300951012     C                   KFLD                    PARTRN
078400951012     C                   KFLD                    PARDTN
078500070517     C     KTNTLZ        KLIST
078600070517     C                   KFLD                    KTLTIP
078700070517     C                   KFLD                    KTLPDR
078800070517     C                   EVAL      KTLTIP = 'T '
078900951030     C     KTAB          KLIST
079000951030     C                   KFLD                    CODUT
079100951030     C                   KFLD                    KCOD
079200951030     C                   KFLD                    KKEY
079300960123     C     KTAB0         KLIST
079400960123     C                   KFLD                    CODUT
079500960123     C                   KFLD                    KCOD
079600951206     C     KCLN          KLIST
079700951206     C                   KFLD                    KTFP
079800951206     C                   KFLD                    KTFA
079900951206     C                   KFLD                    KANN
080000951206     C                   KFLD                    KMES
080100960123     C                   Z-ADD     68            KTFP
080200951206     C                   CLEAR                   KTFA
080300951016     C*          KTSR      KLIST
080400951016     C*                    KFLD           V1CLNP
080500951031     C*                    KFLD           KDAT
080600951019     C* AREA DATI -- > DATA ULTIMO CONTROLLO
080700951019     C     *DTAARA       DEFINE                  TNTL30
080800960123     C* CARICAMENTO TABELLA 1Y - SIMBOLI CALENDARIO BARTOLINI
080900960123     C* (CARICAMENTO DEI SIMBOLI FESTIVI PER PROCDEURA TRAINI)
081000960123     C                   MOVE      *ZEROS        XY
081100960123     C                   MOVEL     '1Y'          KCOD
081200960123     C     KTAB0         SETLL     TABEL00F
081300960123     C     KTAB0         READE     TABEL00F                               30
081400960123     C     *IN30         DOWEQ     *OFF
081500960123     C     TBLFLG        IFEQ      *BLANKS
081600960123     C                   MOVEL     TBLUNI        DS1Y
081700960123     C     §1YFET        IFNE      *BLANKS
081800960123     C                   ADD       1             XY
081900960123     C                   MOVEL     TBLKEY        S1Y(XY)
082000960123     C                   MOVE      §1YFET        FET(XY)
082100960123     C                   END
082200960123     C                   END
082300960123     C     KTAB0         READE     TABEL00F                               30
082400960123     C                   ENDDO
082401080919     C* REPERISCE LA DATA TRAINO PIU' RECENTE PULITA
082402080919     C     *DTAARA       DEFINE                  TNTL58            8 0
082403080919     C                   IN        TNTL58
082404080919     C                   MOVEL     TNTL58        wdtpul            8 0
082500951009     C                   ENDSR
082600951206** MSG
082700951215Data foglio viaggio teorica o data traino OBBLIGATORIA                         1
082800951215Immettere la Data foglio viaggio teorica OPPURE la Data traino                 2
082900951017Data errata                                                                    3
083000951215CONTROLLO PER DATA F.V.TEOR. AL MOMENTO NON EFFETTUABILE.SGRIGLIAMENTO IN CORSO4
083100951121Se data traino = 0 anche numero traino deve essere = 0                         5
083200951215Se data foglio viaggio teorica dal = 0 anche linea partenza f.v. deve essere=0 6
083300951011Numero traino errato                                                           7
083400951025Numero traino inesistente o annullato o non decorrente per la data richiesta   8
083500951011Traino scaduto rispetto alla data richiesta                                    9
083600951011Traino non previsto nel giorno richiesto                                      10
083700971204Linea di partenza foglio viaggio errata                                       11
083800971204La linea di partenza foglio viaggio deve essere un terminal di partenza       12
083900951016Non ammesso controllo con questa data : controllare dal XX/XX/XXXX            13
084000951018Non ammesso il controllo di un giorno festivo                                 14
084100951121Numero traino obbligatorio se immessa data traino                             15
084200951215Se data dal = 0 anche data al deve essere = 0                                 16
084300951218Data Dal > di Data Al                                                         17
084400951019Scegliere almeno una delle OPZIONI DI STAMPA                                  18
084500951030Il traino richiesto è di tipo XXXXXXXXXXXXXXXXXXXXXXXXX-CONTROLLO NON AMMESSO-19
084600960424Traino bis non previsto nella data richiesta (Griglia inesistente)            20
084700951121Periodo massimo di richiesta: 31 giorni                                       21
084800951206Non trovati traini da controllare per la data festiva richiesta               22
084900951220Specificare quali f.v. reali devono essere stampati                           23
085000951220Specificare quali traini non autorizzati al pagamento devono essere stampati  24
085100951220Richiedere la stampa dei f.v. reali non abbinati O lasciare vuoto questo campo25
085200951220Richiedere stampa dei traini non autorizz. oppure lasciare vuoto questo campo 26
085300960131Per controllare da data tratta XX/XX/XXXX controllare al massimo fino al XX/XX27
085400960201Traino annullato                                                              28
085500960305Stampa FV reali non abbinati non ammessa nei controlli per Data/Numero Traino 29
085600070517Codice TRAZIONISTA inesistente
085700070517Filiale errata
085800070522Parzializzazione non gestita nei controlli per Data/Numero Traino             32
085801080919Data foglio viaggio teorica non elaborabile per mancanza di dati
085802150113Causale Ritardo errata
085900951206** TAB/TABDAY
0860009512061LUNEDI
0861009512062MARTEDI
0862009512063MERCOLEDI
0863009512064GIOVEDI
0864009512065VENERDI
0865009512066SABATO
0866009512067DOMENICA
