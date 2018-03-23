000100000000     H*------------------------------------------------------------------------*
000200001016     H* CONTROLLO ASSORBIMENTI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800001016     FTIST44D   CF   E             WORKSTN INFDS(DEVDS1)
000900001016     F                                     SFILE(ST44DMS:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100001106     FWFASS00F  IF   E           K DISK    USROPN
001200000000     FAZORG01L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI
001800000000     D*-------------------
001900000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002000000000     D*-------------------
002100000000     D* COMANDI
002200000000     D*-------------------
002300000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002400000000     D*-------------------
002500000000     D* DESCRIZIONI TASTI FUNZIONALI
002600000000     D*-------------------
002700000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002800000000     D*-------------------
002900000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003000000000     D*-------------------
003100000000     D FUV             S             19    DIM(24)
003200000000     D*-------------------
003300000000     D*------------------------------------------------------------------------*
003400000000     D* INPUT
003500000000     D*------------------------------------------------------------------------*
003600000000     D*-------------------
003700000000     D* RIDEFINIZIONE SCHIERA -FUD-
003800000000     D*-------------------
003900000000     D FUDDS           DS
004000000000     D*                -DESCRIZIONE PARZIALE
004100000000     D  FUDDE1                 1      1
004200000000     D*                -FUNZIONE
004300000000     D  FUDFUN                 2      3
004400000000     D*                -DESCRIZIONE PARZIALE
004500000000     D  FUDDE2                 4     19
004600000000     D*                -FILLER
004700000000     D  FUDFI0                20     34
004800000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004900000000     D  FUDATT                35     35
005000000000     D*                -FILLER
005100000000     D  FUDFI1                36     80
005200000000     D*                -DESCRIZIONE COMPLETA
005300000000     D  FUDDES                 1     19
005400000000     D*                -RIDEFINIZIONE RIGA
005500000000     D  FUDRIG                 1     80
005600000000     D*-------------------
005700000000     D* DS "XMSG" - GESTIONE MESSAGGI AL PGM
005800000000     D*-------------------
005900000000     D DSMSG         E DS
006000000000     D*-------------------
006100000000     D* DS GESTIONE TASTI FUNZIONALI
006200000000     D*-------------------
006300000000     D DSKEY         E DS
006400000000     D DEVDS1          DS
006500000000     D  KEY                  369    369
006600000000     D*-------------------
006700000000     D* DS KG E MC STANDARD
006800000000     D*-------------------
006900000000     D DS$4          E DS
007000000000     D*-------------------
007100000000     D* DS "XSRDA8" - CONTROLLO DATA(8)
007200000000     D*-------------------
007300971128     D WLBDA8          DS
007400000000     D  G08DAT                 1      8  0
007500000000     D  G08INV                 9     16  0
007600000000     D  G08ERR                17     17
007700000000     D  G08TGI                18     22  0
007800990522     D WL08            DS
007900990522     D  L08DA                  1      8  0
008000990522     D  L08A                   9     16  0
008100990522     D  L08L                  17     21  0
008200971128     D*---------------------
008300971128     D* DS REPEIMENTO TERMINALs
008400971128     D*---------------------
008500971128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
008600020226     D*-------------------
008700020226     D* DS DEFINIZIONE TABELLA "NETWORK"
008800020226     D*-------------------
008900020226     D DNTW          E DS
009000020226     D*-------------------
009100020226     D* DS RICHIAMO SCELTA TABELLE
009200020226     D*-------------------
009300020226     D TIBS02        E DS                  EXTNAME(TIBS02DS)
009400000000     D*-------------------
009500000000     D* ARCHITETTURA
009600000000     D*-------------------
009700000000     D KPJBA         E DS
009800001016     D*-------------------
009900001016     D* DS PARAMETRI DI LANCIO PGM
010000001016     D*-------------------
010100010417     D PARAM           DS
010200001016     D  PARDAI                 1      8  0
010300001016     D  PARDAF                 9     16  0
010400001016     D  PARKG1                17     19  0
010500001016     D  PARKG2                20     22  0
010600001016     D  PARKG3                23     25  0
010700001016     D  PARKG4                26     28  0
010800001016     D  PARKG5                29     31  0
010900001016     D  PARMC1                32     34  2
011000001016     D  PARMC2                35     37  2
011100001016     D  PARMC3                38     40  2
011200001016     D  PARMC4                41     43  2
011300001016     D  PARMC5                44     46  2
011400001016     D  PARNAZ                47     49
011500001016     D  PARCAD                50     51
011600001016     D  PARCAA                52     53
011700001016     D  PARTP1                54     56  0
011800001016     D  PARTP2                57     59  0
011900001016     D  PARTP3                60     62  0
012000001016     D  PARTP4                63     65  0
012100001016     D  PARTP5                66     68  0
012200001016     D  PARTP6                69     71  0
012300001016     D  PARTP7                72     74  0
012400001016     D  PARTP8                75     77  0
012500001016     D  PARTP9                78     80  0
012600001016     D  PARTP0                81     83  0
012700020226     D  PARNT1                84     86
012800020226     D  PARNT2                87     89
012900020226     D  PARNT3                90     92
013000020226     D  PARNT4                94     96
013100020226     D  PARNT5                97     99
013200020226     D  PARNT6               100    102
013300020226     D  PARGEN               103    103
013400020226     D  PARDET               104    104
013500020226     D  PARGIO               105    107  0
013600020226     D  PARIAD               108    115  0
013700020226     D  PARFAD               116    123  0
013800020226     D  PARDRI               124    131  0
013900000000     C*------------------------------------------------------------------------*
014000000000     C* MAIN LINE
014100000000     C*------------------------------------------------------------------------*
014200010417     C*
014300010417     C* POI INIZIALIZZO LA DS DI PASSAGGIO PARAMETRI
014400010417     C                   RESET                   PARAM
014500000000     C*
014600000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
014700000000IF  1C     ERRL00        IFEQ      '0'
014800000000     C*
014900000000     C* CICLO FINO A CHE:
015000000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)
015100000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)
015200000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
015300000000DO  2C     F03L00        DOUEQ     '1'
015400000000     C     F12L00        OREQ      '1'
015500000000     C     FLFINE        OREQ      '1'
015600000000     C*
015700000000     C* EMETTE FORMATO VIDEO
015800000000     C                   EXSR      BUTFMR
015900000000     C*
016000000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
016100000000     C                   EXSR      FUNCON
016200000000     C*
016300000000E   2C                   ENDDO
016400000000E   1C                   ENDIF
016500971128     C*
016600971128     C* OPERAZIONI FINALI
016700971128     C                   EXSR      FINSR
016800000000     C*
016900000000     C                   SETON                                        LR
017000971128     C*------------------------------------------------------------------------*
017100971128     C* FINSR - OPERAZIONI FINALI
017200971128     C*------------------------------------------------------------------------*
017300971128     C     FINSR         BEGSR
017400971128     C*
017500971128     C* CHIUDE I PGM
017600971128     C                   CLEAR                   DSLV55
017700971128     C                   MOVEL     'C'           D55TLA
017800971128     C                   CALL      'FNLV55R'
017900971128     C                   PARM                    DSLV55
018000971128     C*
018100971128     C                   ENDSR
018200000000     C*------------------------------------------------------------------------*
018300000000     C* BUTMFR - EMISSIONE FORMATO A VIDEO
018400000000     C*------------------------------------------------------------------------*
018500000000     C     BUTFMR        BEGSR
018600000000     C*
018700000000     C* EMETTE TESTATA
018800001016     C                   WRITE     ST44D01
018900000000     C*
019000000000     C* VISUALIZZA MESSAGGIO
019100000000     C                   EXSR      SNDMSG
019200000000     C*
019300000000     C* EMETTE FUNZIONI
019400001016     C                   WRITE     ST44D02
019500000000     C*
019600000000     C* EMETTE FORMATO VIDEO
019700001016     C                   EXFMT     ST44D03
019800000000     C*
019900000000     C* AZZERA FLAG DI ERRORE LIVELLO
020000000000     C                   MOVEL     '0'           ERRL00
020100000000     C*
020200000000     C* AZZERA INDICATORI DI ERRORE
020300001110     C                   SETOFF                                       6996
020400001016     C                   SETOFF                                       717274
020500001016     C                   SETOFF                                       757677
020600001016     C                   SETOFF                                       787980
020700001016     C                   SETOFF                                       819495
020800001016     C                   SETOFF                                       828384
020900001016     C                   SETOFF                                       858687
021000001016     C                   SETOFF                                       888990
021100001016     C                   SETOFF                                       919293
021200020226     C                   SETOFF                                       606162
021300020226     C                   SETOFF                                       636465
021400000000     C*
021500000000     C                   ENDSR
021600000000     C*------------------------------------------------------------------------*
021700000000     C* SNDMSG - MANDA MESSAGGIO
021800000000     C*------------------------------------------------------------------------*
021900000000     C     SNDMSG        BEGSR
022000000000     C*
022100000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
022200000000     C                   MOVEL     '1'           *IN40
022300001016     C                   WRITE     ST44DMC
022400000000     C                   MOVEL     '0'           *IN40
022500000000     C*
022600000000     C* VISUALIZZAZIONE MESSAGGIO
022700000000     C                   Z-ADD     0             MSGULT            4 0
022800000000     C                   CALL      'XMSG'        XMSGPA
022900000000     C     DSMSER        IFEQ      0
023000000000     C                   MOVEL     DSMSKE        MSGKEY
023100000000     C                   ENDIF
023200000000     C                   Z-ADD     1             MSGULT
023300001016     C                   WRITE     ST44DMS                                      SFL MESS
023400001016     C                   WRITE     ST44DMC                                      CONTR MESS
023500000000     C*
023600000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
023700000000     C                   MOVEL     *BLANKS       DSMSMS
023800000000     C*
023900000000     C                   ENDSR
024000000000     C*------------------------------------------------------------------------*
024100000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
024200000000     C*------------------------------------------------------------------------*
024300000000     C     ENTGES        BEGSR
024400000000     C*
024500000000     C* CONTROLLA I CAMPI IMMESSI
024600000000     C                   EXSR      DECODI
024700000000     C*
024800000000     C                   ENDSR
024900001106     C*------------------------------------------------------------------------*
025000001106     C* F01GES - GESTIONE TASTO FUNZIONALE F01=AIUTO
025100001106     C*------------------------------------------------------------------------*
025200001106     C     F01GES        BEGSR
025300001106     C*
025400001106     C* EMETTE LA WINDOWS DI AIUTO
025500001106     C                   EXFMT     ST44WW1
025600001106     C*
025700001106     C                   ENDSR
025800000000     C*------------------------------------------------------------------------*
025900000000     C* F03GES - GESTIONE TASTO FUNZIONALE F03=FINE LAVORO
026000000000     C*------------------------------------------------------------------------*
026100000000     C     F03GES        BEGSR
026200000000     C*
026300000000     C* IMPOSTA FLAG F03
026400000000     C                   MOVEL     '1'           F03L00
026500000000     C*
026600000000     C                   ENDSR
026700000000     C*------------------------------------------------------------------------*
026800000000     C* F06GES - GESTIONE TASTO FUNZIONALE F06=LANCIO
026900000000     C*------------------------------------------------------------------------*
027000000000     C     F06GES        BEGSR
027100000000     C*
027200000000     C* CONTROLLA I CAMPI IMMESSI
027300000000     C                   EXSR      DECODI
027400000000     C*
027500000000     C* LANCIA IL PROGRAMMA DI STAMPA
027600000000IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORE
027700000000     C*
027800000000     C* CARICA I TFP SELEZIONATI
027900000000     C                   EXSR      VIDPAR
028000010417     C                   MOVEL(p)  PARAM         KPJBU
028100001016     C                   MOVEL     'ST45'        KCOAZ
028200010308     C                   CALL      'BCH10'
028300010308 MOM C*                  CALL      'TIST45R'
028400000000     C                   PARM                    KPJBA
028500000000E   1C                   ENDIF
028600000000     C*
028700000000     C                   ENDSR
028800000000     C*------------------------------------------------------------------------*
028900000000     C* F10GES - GESTIONE TASTO FUNZIONALE F10=ALTRE SELEZIONI
029000000000     C*------------------------------------------------------------------------*
029100000000     C     F10GES        BEGSR
029200970624     C*
029300000000     C*
029400000000     C                   ENDSR
029500000000     C*------------------------------------------------------------------------*
029600000000     C* F12GES - GESTIONE TASTO FUNZIONALE F12=RITORNO
029700000000     C*------------------------------------------------------------------------*
029800000000     C     F12GES        BEGSR
029900000000     C*
030000000000     C* IMPOSTO FLAG F12
030100000000     C                   MOVEL     '1'           F12L00
030200000000     C*
030300000000     C                   ENDSR
030400000000     C*------------------------------------------------------------------------*
030500000000     C* F22GES - GESTIONE TASTO FUNZIONALE F22=GESTIONE STAMPE
030600000000     C*------------------------------------------------------------------------*
030700000000     C     F22GES        BEGSR
030800000000     C*
030900000000     C* WRKSPLF - GESTIONE STAMPE
031000000000     C                   Z-ADD     80            LENGH            15 5
031100000000     C                   CALL      'QCAEXEC'
031200000000     C                   PARM                    CMD(1)
031300000000     C                   PARM                    LENGH
031400000000     C*
031500000000     C                   ENDSR
031600000000     C*------------------------------------------------------------------------*
031700000000     C* F24GES - GESTIONE TASTO FUNZIONALE F24=ALTRE FUNZIONI
031800000000     C*------------------------------------------------------------------------*
031900000000     C     F24GES        BEGSR
032000000000     C*
032100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
032200000000     C                   EXSR      FUNGES
032300000000     C*
032400000000     C                   ENDSR
032500000000     C*------------------------------------------------------------------------*
032600001016     C* DECODI - CONTROLLA CAMPI E DECODIFICA
032700000000     C*------------------------------------------------------------------------*
032800000000     C     DECODI        BEGSR
032900000000     C*--------------------
033000000000     C* DATA INIZIALE
033100000000     C*--------------------
033200001016     C     ERRL00        IFEQ      '0'
033300000000     C                   Z-ADD     VIDDTI        G08DAT
033400000000     C                   Z-ADD     *ZEROS        G08INV
033500000000     C                   MOVEL     '0'           G08ERR
033600000000     C                   CALL      'XSRDA8'
033700971128     C                   PARM                    WLBDA8
033800000000     C                   Z-ADD     G08DAT        VIDDTI
033900001016     C     G08ERR        IFEQ      '0'
034000000000     C                   Z-ADD     G08INV        WDTI              8 0
034100001016     C                   ELSE
034200000000     C                   MOVEL     '1'           ERRL00
034300000000     C                   MOVEL     MSG(16)       DSMSMS
034400000000     C                   SETON                                        71
034500001016     C                   ENDIF
034600001016     C                   ENDIF
034700000000     C*--------------------
034800000000     C* DATA FINALE
034900000000     C*--------------------
035000001016     C     ERRL00        IFEQ      '0'
035100000000     C                   Z-ADD     VIDDTF        G08DAT
035200000000     C                   Z-ADD     *ZEROS        G08INV
035300000000     C                   MOVEL     '0'           G08ERR
035400000000     C                   CALL      'XSRDA8'
035500971128     C                   PARM                    WLBDA8
035600000000     C                   Z-ADD     G08DAT        VIDDTF
035700001016     C     G08ERR        IFEQ      '0'
035800000000     C                   Z-ADD     G08INV        WDTF              8 0
035900001016     C                   ELSE
036000000000     C                   MOVEL     '1'           ERRL00
036100000000     C                   MOVEL     MSG(16)       DSMSMS
036200000000     C                   SETON                                        72
036300001016     C                   ENDIF
036400001016     C                   ENDIF
036500001110     C*--------------------
036600001110     C* DATA DI RIFERIMENTO
036700001110     C*--------------------
036800001110     C     ERRL00        IFEQ      '0'
036900001110     C     VIDDRI        ANDNE     *ZEROS
037000001110     C                   Z-ADD     VIDDRI        G08DAT
037100001110     C                   Z-ADD     *ZEROS        G08INV
037200001110     C                   MOVEL     '0'           G08ERR
037300001110     C                   CALL      'XSRDA8'
037400001110     C                   PARM                    WLBDA8
037500001110     C                   Z-ADD     G08DAT        VIDDRI
037600001110     C     G08ERR        IFEQ      '0'
037700001110     C                   Z-ADD     G08INV        WDRI              8 0
037800001110     C                   ELSE
037900001110     C                   MOVEL     '1'           ERRL00
038000001110     C                   MOVEL     MSG(16)       DSMSMS
038100001110     C                   SETON                                        96
038200001110     C                   ENDIF
038300001110     C                   ENDIF
038400001109     C*--------------------
038500001109     C* DATA INIZIALE/FINALE SE NON RICHIESTA RIGENERAZIONE FILE
038600001109     C*--------------------
038700001109     C     ERRL00        IFEQ      '0'
038800001109     C     VIDGEN        IFEQ      'N'
038900001109     C                   OPEN      WFASS00F
039000001109     C                   READ      WFASS00F
039100001109     C                   IF        NOT %EOF(WFASS00F)
039200001109     C* DATA INIZIALE
039300001109     C                   Z-ADD     *ZEROS        G08DAT
039400001109     C                   Z-ADD     ASSDAI        G08INV
039500001109     C                   MOVEL     '3'           G08ERR
039600001109     C                   CALL      'XSRDA8'
039700001109     C                   PARM                    WLBDA8
039800001109     C     G08ERR        IFEQ      '0'
039900001109     C                   Z-ADD     G08DAT        DEPDTI            8 0
040000001109     C                   ELSE
040100001109     C                   MOVEL     '1'           ERRL00
040200001109     C                   MOVEL     MSG(16)       DSMSMS
040300001109     C                   SETON                                        71
040400001109     C                   ENDIF
040500001109     C* DATA FINALE
040600001109     C                   Z-ADD     *ZEROS        G08DAT
040700001109     C                   Z-ADD     ASSDAF        G08INV
040800001109     C                   MOVEL     '3'           G08ERR
040900001109     C                   CALL      'XSRDA8'
041000001109     C                   PARM                    WLBDA8
041100001109     C     G08ERR        IFEQ      '0'
041200001109     C                   Z-ADD     G08DAT        DEPDTF            8 0
041300001109     C                   ELSE
041400001109     C                   MOVEL     '1'           ERRL00
041500001109     C                   MOVEL     MSG(16)       DSMSMS
041600001109     C                   SETON                                        71
041700001109     C                   ENDIF
041800001109     C* VERIFICA RANGE RICHIESTO A VIDEO
041900001113     C                   IF        WDTI <> ASSDAI
042000001109     C                   Z-ADD     DEPDTI        VIDDTI
042100001109     C                   ENDIF
042200001113     C                   IF        WDTF <> ASSDAF
042300001109     C                   Z-ADD     DEPDTF        VIDDTF
042400001109     C                   ENDIF
042500001109     C                   ELSE
042600001109     C                   MOVEL     '1'           ERRL00
042700001109     C                   MOVEL     MSG(17)       DSMSMS
042800001109     C                   MOVEL     'S'           VIDGEN
042900001109     C                   ENDIF
043000001109     C                   CLOSE     WFASS00F
043100001109     C                   ENDIF
043200001109     C                   ENDIF
043300001106     C*--------------------
043400001106     C* GIORNI LAVORATIVI
043500001106     C*--------------------
043600001106     C     ERRL00        IFEQ      '0'
043700001106     C     VIDGIO        IFEQ      *ZEROS
043800001106     C* SE 0 PROPONGO:
043900001106     C                   CLEAR                   WL08
044000001106     C                   Z-ADD     WDTI          L08DA
044100001106     C                   Z-ADD     WDTF          L08A
044200001106     C                   CALL      'XSRLAV8'
044300001106     C                   PARM                    WL08
044400001106     C                   MOVE      L08L          VIDGIO
044500001106     C                   MOVEL     '1'           ERRL00
044600001106     C                   MOVEL     MSG(8)        DSMSMS
044700001106     C                   SETON                                        69
044800001106     C                   ENDIF
044900001106     C                   ENDIF
045000981019     C*--------------------
045100001016     C* LIMITI FASCE PESO/VOLUME
045200981019     C*--------------------
045300000000     C* ALMENO UN LIMITE
045400001016     C     ERRL00        IFEQ      '0'
045500001016     C     VIDKG1        IFEQ      *ZEROS
045600000000     C     VIDKG2        ANDEQ     *ZEROS
045700000000     C     VIDKG3        ANDEQ     *ZEROS
045800000000     C     VIDKG4        ANDEQ     *ZEROS
045900001016     C     VIDKG5        ANDEQ     *ZEROS
046000000000     C     VIDMC1        ANDEQ     *ZEROS
046100000000     C     VIDMC2        ANDEQ     *ZEROS
046200000000     C     VIDMC3        ANDEQ     *ZEROS
046300000000     C     VIDMC4        ANDEQ     *ZEROS
046400001016     C     VIDMC5        ANDEQ     *ZEROS
046500000000     C                   MOVEL     '1'           ERRL00
046600000000     C                   MOVEL     MSG(5)        DSMSMS
046700000000     C                   SETON                                        747576
046800000000     C                   SETON                                        777879
046900001016     C                   SETON                                        808194
047000001016     C                   SETON                                        95
047100001016     C                   ENDIF
047200001016     C                   ENDIF
047300020226     C*--------------------
047400020226     C* NETWORK
047500020226     C*--------------------
047600020226     C     ERRL00        IFEQ      '0'
047700020226     C     VIDNT1        IFNE      *BLANKS
047800020226     C                   EVAL      DEPNTW = VIDNT1
047900020226     C                   EXSR      CHKNTW
048000020226     C                   EVAL      VIDNT1 = DEPNTW
048100020226     C     ERRL00        IFEQ      '1'
048200020226     C                   MOVEL     MSG(18)       DSMSMS
048300020226     C                   SETON                                        60
048400020226     C                   ENDIF
048500020226     C                   ENDIF
048600020226     C                   ENDIF
048700020226     C*
048800020226     C     ERRL00        IFEQ      '0'
048900020226     C     VIDNT2        IFNE      *BLANKS
049000020226     C                   EVAL      DEPNTW = VIDNT2
049100020226     C                   EXSR      CHKNTW
049200020226     C                   EVAL      VIDNT2 = DEPNTW
049300020226     C     ERRL00        IFEQ      '1'
049400020226     C                   MOVEL     MSG(18)       DSMSMS
049500020226     C                   SETON                                        61
049600020226     C                   ENDIF
049700020226     C                   ENDIF
049800020226     C                   ENDIF
049900020226     C*
050000020226     C     ERRL00        IFEQ      '0'
050100020226     C     VIDNT3        IFNE      *BLANKS
050200020226     C                   EVAL      DEPNTW = VIDNT3
050300020226     C                   EXSR      CHKNTW
050400020226     C                   EVAL      VIDNT3 = DEPNTW
050500020226     C     ERRL00        IFEQ      '1'
050600020226     C                   MOVEL     MSG(18)       DSMSMS
050700020226     C                   SETON                                        62
050800020226     C                   ENDIF
050900020226     C                   ENDIF
051000020226     C                   ENDIF
051100020226     C*
051200020226     C     ERRL00        IFEQ      '0'
051300020226     C     VIDNT4        IFNE      *BLANKS
051400020226     C                   EVAL      DEPNTW = VIDNT4
051500020226     C                   EXSR      CHKNTW
051600020226     C                   EVAL      VIDNT4 = DEPNTW
051700020226     C     ERRL00        IFEQ      '1'
051800020226     C                   MOVEL     MSG(18)       DSMSMS
051900020226     C                   SETON                                        63
052000020226     C                   ENDIF
052100020226     C                   ENDIF
052200020226     C                   ENDIF
052300020226     C*
052400020226     C     ERRL00        IFEQ      '0'
052500020226     C     VIDNT5        IFNE      *BLANKS
052600020226     C                   EVAL      DEPNTW = VIDNT5
052700020226     C                   EXSR      CHKNTW
052800020226     C                   EVAL      VIDNT5 = DEPNTW
052900020226     C     ERRL00        IFEQ      '1'
053000020226     C                   MOVEL     MSG(18)       DSMSMS
053100020226     C                   SETON                                        64
053200020226     C                   ENDIF
053300020226     C                   ENDIF
053400020226     C                   ENDIF
053500020226     C*
053600020226     C     ERRL00        IFEQ      '0'
053700020226     C     VIDNT6        IFNE      *BLANKS
053800020226     C                   EVAL      DEPNTW = VIDNT6
053900020226     C                   EXSR      CHKNTW
054000020226     C                   EVAL      VIDNT6 = DEPNTW
054100020226     C     ERRL00        IFEQ      '1'
054200020226     C                   MOVEL     MSG(18)       DSMSMS
054300020226     C                   SETON                                        65
054400020226     C                   ENDIF
054500020226     C                   ENDIF
054600020226     C                   ENDIF
054700000000     C*--------------------
054800000000     C* TERMINAL PARTENZA
054900000000     C*--------------------
055000000000     C* PRIMO
055100001016     C     ERRL00        IFEQ      '0'
055200001016     C     VIDTP1        IFGT      *ZEROS
055300000000     C                   MOVEL     VIDTP1        WLIN
055400000000     C                   EXSR      CHKLIN
055500001016     C     ERRL00        IFEQ      '1'
055600000000     C                   SETON                                        84
055700001016     C                   ENDIF
055800001016     C                   ENDIF
055900001016     C                   ENDIF
056000000000     C* SECONDO
056100001016     C     ERRL00        IFEQ      '0'
056200001016     C     VIDTP2        IFGT      *ZEROS
056300000000     C                   MOVEL     VIDTP2        WLIN
056400000000     C                   EXSR      CHKLIN
056500001016     C     ERRL00        IFEQ      '1'
056600000000     C                   SETON                                        85
056700001016     C                   ENDIF
056800001016     C                   ENDIF
056900001016     C                   ENDIF
057000000000     C* TERZO
057100001016     C     ERRL00        IFEQ      '0'
057200001016     C     VIDTP3        IFGT      *ZEROS
057300000000     C                   MOVEL     VIDTP3        WLIN
057400000000     C                   EXSR      CHKLIN
057500001016     C     ERRL00        IFEQ      '1'
057600000000     C                   SETON                                        86
057700001016     C                   ENDIF
057800001016     C                   ENDIF
057900001016     C                   ENDIF
058000000000     C* QUARTO
058100001016     C     ERRL00        IFEQ      '0'
058200001016     C     VIDTP4        IFGT      *ZEROS
058300001016     C                   MOVEL     VIDTP4        WLIN
058400000000     C                   EXSR      CHKLIN
058500001016     C     ERRL00        IFEQ      '1'
058600000000     C                   SETON                                        87
058700001016     C                   ENDIF
058800001016     C                   ENDIF
058900001016     C                   ENDIF
059000000000     C* QUINTO
059100001016     C     ERRL00        IFEQ      '0'
059200001016     C     VIDTP5        IFGT      *ZEROS
059300000000     C                   MOVEL     VIDTP5        WLIN
059400000000     C                   EXSR      CHKLIN
059500001016     C     ERRL00        IFEQ      '1'
059600000000     C                   SETON                                        88
059700001016     C                   ENDIF
059800001016     C                   ENDIF
059900001016     C                   ENDIF
060000000000     C* SESTO
060100001016     C     ERRL00        IFEQ      '0'
060200001016     C     VIDTP6        IFGT      *ZEROS
060300000000     C                   MOVEL     VIDTP6        WLIN
060400000000     C                   EXSR      CHKLIN
060500001016     C     ERRL00        IFEQ      '1'
060600000000     C                   SETON                                        89
060700001016     C                   ENDIF
060800001016     C                   ENDIF
060900001016     C                   ENDIF
061000000000     C* SETTIMO
061100001016     C     ERRL00        IFEQ      '0'
061200001016     C     VIDTP7        IFGT      *ZEROS
061300001016     C                   MOVEL     VIDTP7        WLIN
061400000000     C                   EXSR      CHKLIN
061500001016     C     ERRL00        IFEQ      '1'
061600000000     C                   SETON                                        90
061700001016     C                   ENDIF
061800001016     C                   ENDIF
061900001016     C                   ENDIF
062000000000     C* OTTAVO
062100001016     C     ERRL00        IFEQ      '0'
062200001016     C     VIDTP8        IFGT      *ZEROS
062300000000     C                   MOVEL     VIDTP8        WLIN
062400000000     C                   EXSR      CHKLIN
062500001016     C     ERRL00        IFEQ      '1'
062600000000     C                   SETON                                        91
062700001016     C                   ENDIF
062800001016     C                   ENDIF
062900001016     C                   ENDIF
063000000000     C* NONO
063100001016     C     ERRL00        IFEQ      '0'
063200001016     C     VIDTP9        IFGT      *ZEROS
063300000000     C                   MOVEL     VIDTP9        WLIN
063400000000     C                   EXSR      CHKLIN
063500001016     C     ERRL00        IFEQ      '1'
063600000000     C                   SETON                                        92
063700001016     C                   ENDIF
063800001016     C                   ENDIF
063900001016     C                   ENDIF
064000000000     C* DECIMO
064100001016     C     ERRL00        IFEQ      '0'
064200001016     C     VIDTP0        IFGT      *ZEROS
064300000000     C                   MOVEL     VIDTP0        WLIN
064400000000     C                   EXSR      CHKLIN
064500001016     C     ERRL00        IFEQ      '1'
064600000000     C                   SETON                                        93
064700001016     C                   ENDIF
064800001016     C                   ENDIF
064900001016     C                   ENDIF
065000000000     C*
065100000000     C                   ENDSR
065200000000     C*------------------------------------------------------------------------*
065300000000     C* CHKLIN - CONTROLLA LA FILIALE
065400000000     C*------------------------------------------------------------------------*
065500000000     C     CHKLIN        BEGSR
065600000000     C*
065700000000     C* RICHIESTO ELENCO FILIALI
065800000000     C                   MOVE      WLIN          WLIN1             1
065900000000     C                   MOVEL     WLIN          WLIN2             1
066000001016     C     WLIN1         IFEQ      '?'
066100000000     C     WLIN2         OREQ      '?'
066200000000     C                   MOVEL     *BLANKS       §COD1             3
066300000000     C                   MOVE      *BLANKS       §DES1
066400971203     C                   MOVE      *BLANKS       §PRA
066500971203     C                   CALL      'TNSD24R'
066600000000     C                   PARM                    §COD1
066700000000     C                   PARM                    §TIP              1
066800000000     C                   PARM                    §DES1            25
066900971203     C                   PARM                    §PRA              3
067000000000     C                   MOVEL     §COD1         WLIN              3
067100000000     C                   MOVEL     §DES1         WDLIN            20
067200001016     C                   ENDIF
067300000000     C*
067400000000     C* CONTROLLA ESISTENZA DEL CODICE IN ARCHIVIO
067500000000     C                   MOVE      WLIN          KORFIL
067600001016     C     KEYORG        CHAIN     AZORG01L                           99
067700000000     C     *IN99         IFEQ      *OFF                                         *ESISTENTE
067800000000     C     ORGFVA        ANDEQ     *BLANKS                                      *NON ANNULLATO
067900001016     C     ORGFAG        IFEQ      'F'                                          *SOLO FILIALI
068000000000     C     ORGFAG        OREQ      'A'                                          *SOLO AGENZIE
068100000000     C                   MOVEL     ORGDES        WDLIN
068200971128     C*
068300971128     C* REPERISCE IL TERMINAL
068400971128     C                   CLEAR                   DSLV55
068500971128     C                   MOVEL     'P'           D55TPT
068600971128     C                   Z-ADD     ORGFIL        D55LIN
068700971128     C                   Z-ADD     DATCOR        D55DRF
068800971128     C                   CALL      'FNLV55R'
068900971128     C                   PARM                    DSLV55
069000001016     C     D55ERR        IFNE      *BLANKS
069100971128     C                   Z-ADD     ORGFIL        D55TFP
069200001016     C                   ENDIF
069300001016     C                   ELSE                                                   *NON VALIDO
069400000000     C                   MOVEL     '1'           ERRL00
069500000000     C                   MOVEL     MSG(14)       DSMSMS
069600000000     C                   MOVEL     *ALL'*'       WDLIN
069700001016     C                   ENDIF
069800001016     C                   ELSE                                                   *INESISTENTE
069900000000     C                   MOVEL     '1'           ERRL00
070000000000     C                   MOVEL     MSG(14)       DSMSMS
070100000000     C                   MOVEL     *ALL'*'       WDLIN
070200001016     C                   ENDIF
070300000000     C*
070400000000     C                   ENDSR
070500000000     C*------------------------------------------------------------------------*
070600000000     C* AZZVID - PULISCE IL VIDEO DA EMETTRE
070700000000     C*------------------------------------------------------------------------*
070800000000     C     AZZVID        BEGSR
070900010417     C*
071000010417     C* PER PRIMA COSA INIZIALIZZO IL FORMATO VIDEO
071100010417     C                   CLEAR                   ST44D03
071200000000     C*
071300000000     C* RECUPERA KG E MC DI DEFAULT
071400020906     C                   MOVEL     1             KTBKUT
071500000000     C                   MOVEL     '$4'          KTBCOD
071600000000     C                   MOVEL     *BLANKS       KTBKEY
071700000000     C     KEYTAB        CHAIN     TABEL00F                           99
071800001016     C     *IN99         IFEQ      *ON
071900000000     C     TBLFLG        OREQ      '*'
072000001016     C                   ELSE
072100000000     C                   MOVEL     TBLUNI        DS$4
072200000000     C                   Z-ADD     §$4KG1        VIDKG1
072300000000     C                   Z-ADD     §$4KG2        VIDKG2
072400000000     C                   Z-ADD     §$4KG3        VIDKG3
072500000000     C                   Z-ADD     §$4KG4        VIDKG4
072600001016     C                   Z-ADD     *zeros        VIDKG5
072700981019     C                   Z-ADD     §$4MC1        VIDMC1
072800981019     C                   Z-ADD     §$4MC2        VIDMC2
072900981019     C                   Z-ADD     §$4MC3        VIDMC3
073000981019     C                   Z-ADD     §$4MC4        VIDMC4
073100001016     C                   Z-ADD     *zeros        VIDMC5
073200001016     C                   ENDIF
073300001106     C                   MOVEL     'N'           VIDGEN
073400001106     C                   MOVEL     'S'           VIDDET
073500000000     C*
073600000000     C                   ENDSR
073700000000     C*------------------------------------------------------------------------*
073800001106     C* VIDPAR - IMPOSTA I PARAMETRI DA PASSARE
073900000000     C*------------------------------------------------------------------------*
074000000000     C     VIDPAR        BEGSR
074100000000     C*
074200001016     C                   Z-ADD     WDTI          PARDAI
074300001016     C                   Z-ADD     WDTF          PARDAF
074400001016     C                   MOVEL     VIDNZD        PARNAZ
074500001016     C                   MOVEL     VIDCAD        PARCAD
074600001016     C                   MOVEL     VIDCAA        PARCAA
074700001016     C                   Z-ADD     VIDKG1        PARKG1
074800001016     C                   Z-ADD     VIDMC1        PARMC1
074900001016     C                   Z-ADD     VIDKG2        PARKG2
075000001016     C                   Z-ADD     VIDMC2        PARMC2
075100001016     C                   Z-ADD     VIDKG3        PARKG3
075200001016     C                   Z-ADD     VIDMC3        PARMC3
075300001016     C                   Z-ADD     VIDKG4        PARKG4
075400001016     C                   Z-ADD     VIDMC4        PARMC4
075500001016     C                   Z-ADD     VIDKG5        PARKG5
075600001016     C                   Z-ADD     VIDMC5        PARMC5
075700001016     C                   Z-ADD     VIDTP1        PARTP1
075800001016     C                   Z-ADD     VIDTP2        PARTP2
075900001016     C                   Z-ADD     VIDTP3        PARTP3
076000001016     C                   Z-ADD     VIDTP4        PARTP4
076100001016     C                   Z-ADD     VIDTP5        PARTP5
076200001016     C                   Z-ADD     VIDTP6        PARTP6
076300001016     C                   Z-ADD     VIDTP7        PARTP7
076400001016     C                   Z-ADD     VIDTP8        PARTP8
076500001016     C                   Z-ADD     VIDTP9        PARTP9
076600001016     C                   Z-ADD     VIDTP0        PARTP0
076700001106     C                   MOVEL     VIDNT1        PARNT1
076800001106     C                   MOVEL     VIDNT2        PARNT2
076900001106     C                   MOVEL     VIDNT3        PARNT3
077000001106     C                   MOVEL     VIDNT4        PARNT4
077100001106     C                   MOVEL     VIDNT5        PARNT5
077200001106     C                   MOVEL     VIDNT6        PARNT6
077300001106     C                   MOVEL     VIDGEN        PARGEN
077400001106     C                   MOVEL     VIDDET        PARDET
077500001106     C                   Z-ADD     VIDGIO        PARGIO
077600001109     C                   Z-ADD     VIDDTI        PARIAD
077700001109     C                   Z-ADD     VIDDTF        PARFAD
077800001110     C                   Z-ADD     WDRI          PARDRI
077900001016     C*
078000000000     C                   ENDSR
078100020226     C*------------------------------------------------------------------------*
078200020226     C* CHKNTW - CONTROLLA IL NETWORK
078300020226     C*------------------------------------------------------------------------*
078400020226     C     CHKNTW        BEGSR
078500020226     C*
078600020226     C* RICHIESTO ELENCO
078700020226     C     '?'           SCAN      DEPNTW                                 99
078800020226     C* SCELTA VALORE DA TABELLA
078900020226     C     *IN99         IFEQ      *ON
079000020226     C                   CLEAR                   TIBS02
079100020226     C                   MOVEL     'NTW'         T02COD
079200020226     C                   MOVEL     KNSIF         T02SIF
079300020226     C                   MOVEL     'R'           T02MOD
079400020226     C                   CALL      'TIBS02R'
079500020226     C                   PARM                    KPJBA
079600020226     C                   PARM                    TIBS02
079700020226     C*
079800020226     C                   ELSE
079900020226     C* VERIFICA VALORE IN TABELLA
080000020226     C                   CLEAR                   TIBS02
080100020226     C                   MOVEL     'NTW'         T02COD
080200020226     C                   MOVEL     KNSIF         T02SIF
080300020226     C                   MOVEL     DEPNTW        T02KE1
080400020226     C                   MOVEL     'C'           T02MOD
080500020226     C                   CALL      'TIBS02R'
080600020226     C                   PARM                    KPJBA
080700020226     C                   PARM                    TIBS02
080800020226     C                   ENDIF
080900020226     C*
081000020226     C     T02ERR        IFEQ      *BLANKS
081100020226     C                   MOVEL     T02UNI        DNTW
081200020226     C                   MOVEL     T02KE1        DEPNTW
081300020226     C                   ELSE
081400020226     C                   MOVEL     '1'           ERRL00
081500020226     C                   ENDIF
081600020226     C*
081700020226     C                   ENDSR
081800000000     C*------------------------------------------------------------------------*
081900000000     C* *INZSR - OOPERAZIONI INIZIALI
082000000000     C*------------------------------------------------------------------------*
082100000000     C     *INZSR        BEGSR
082200000000     C*--------------------
082300000000     C* RICEVIMENTO PARAMETRI
082400000000     C*--------------------
082500000000     C     *ENTRY        PLIST
082600000000     C                   PARM                    KPJBA
082700000000     C*--------------------
082800000000     C* VARIABILI RIFERITE AL DATABASE
082900000000     C*--------------------
083000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
083100000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABELL0F
083200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
083300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
083400000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY                         *CHIAVE FUNZIONALE
083500000000     C*--------------------
083600000000     C* CHAIVI DI LETTURA
083700000000     C*--------------------
083800000000     C* LETTURA AZORG01L - COMPLETA
083900000000     C     KEYORG        KLIST
084000000000     C                   KFLD                    KORFIL
084100000000     C* LETTURA TABEL00F - COMPLETA
084200000000     C     KEYTAB        KLIST
084300000000     C                   KFLD                    KTBKUT
084400000000     C                   KFLD                    KTBCOD
084500000000     C                   KFLD                    KTBKEY
084600000000     C*
084700000000     C* REPERISCE I PARAMETRI PER PASSAGGIO MESSAGGI AL PGM
084800000000     C     XMSGPA        PLIST
084900000000     C                   PARM                    DSMSMS
085000000000     C                   PARM                    DSMSPG
085100000000     C                   PARM                    DSMSKE
085200000000     C                   PARM                    DSMSER
085300000000     C*
085400000000     C* REPERISCE LA DSKEY -TASTI PREMUTI
085500000000     C                   CALL      'XKEY'
085600000000     C                   PARM                    DSKEY
085700000000     C*
085800000000     C* IMPOSTA PER CL=XMSG -FISSI IN TUTTO IL PGM-
085900000000     C*   -NOME PGM CHIAMANTE
086000000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
086100001016     C                   MOVEL     'TIST44R'     DSMSPG
086200001016     C                   MOVEL     'TIST44R'     MSGQUE
086300000000     C                   MOVEL     *BLANKS       MSGKEY
086400000000     C                   MOVEL     *BLANKS       DSMSMS
086500000000     C*
086600000000     C* IMPOSTA INDICE PER EMISSIONE FUNZIONI A VIDEO
086700000000     C                   Z-ADD     0             FUNGRU            5 0
086800000000     C*
086900000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
087000000000     C                   EXSR      FUNGES
087100000000     C*--------------------
087200000000     C* VARIABILI DI CONTROLLO
087300000000     C*--------------------
087400000000     C                   MOVEL     '0'           FINL00            1            *FINE PGM
087500000000     C                   MOVEL     '0'           F03L00            1            *TASTO F03
087600000000     C                   MOVEL     '0'           F12L00            1            *TASTO F12
087700000000     C                   MOVEL     '0'           ERRL00            1            *ERRORE
087800000000     C                   MOVEL     '0'           FLFINE            1            *FINE FUNZIONE
087900020226     C                   MOVEL     *BLANKS       DEPNTW            3            *NETWORK WRK
088000001016     C*
088100971128     C* CALCOLA LA DATA CORRENTE
088200971128     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
088300971128     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
088400971128     C                   Z-ADD     WN8           DATA              8 0
088500971128     C                   Z-ADD     WN8           G08DAT
088600971128     C                   Z-ADD     *ZEROS        G08INV
088700971128     C                   MOVEL     '0'           G08ERR
088800971128     C                   CALL      'XSRDA8'
088900971128     C                   PARM                    WLBDA8
089000971128     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
089100000000     C*
089200000000     C* PULISCE IL VIDEO DA EMETTERE
089300000000     C                   EXSR      AZZVID
089400000000     C*
089500000000     C                   ENDSR
089600000000     C*------------------------------------------------------------------------*
089700000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
089800000000     C*------------------------------------------------------------------------*
089900000000     C     FUNGES        BEGSR
090000000000     C*
090100000000     C* PULISCO CAMPI DI LAVORO
090200000000     C                   Z-ADD     0             W                 5 0
090300000000     C*
090400000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090500000000     C* E CARICO TABELLA FUNZIONI ATTIVE
090600000000     C     1             DO        24            I
090700000000     C                   MOVEL     FUD(I)        FUDRIG
090800000000     C*
090900000000     C* PULIZIA FUNZIONI PRECEDENTI
091000000000     C                   MOVEL     *BLANKS       FUV(I)
091100000000     C*
091200000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
091300000000     C* (SALTA F24)
091400000000     C     FUDATT        IFEQ      'S'
091500000000     C     FUDFUN        ANDNE     '24'
091600000000     C                   ADD       1             W
091700000000     C                   MOVEL     FUDDES        FUV(W)
091800000000     C                   END
091900000000     C                   END                                                    DO WHILE
092000000000     C*
092100000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
092200000000     C* ALTRIMENTI DISATTIVO F24
092300000000     C     1             DO        24            I
092400000000     C                   MOVEL     FUD(I)        FUDRIG
092500000000     C*
092600000000     C     FUDFUN        IFEQ      '24'
092700000000     C     W             IFGT      6
092800000000     C                   MOVEL     'S'           FUDATT
092900000000     C                   ELSE
093000000000     C                   MOVEL     *BLANKS       FUDATT
093100000000     C                   END                                                    W GT 8
093200000000     C                   MOVEL     FUDRIG        FUD(I)
093300000000     C                   END
093400000000     C                   END                                                    DO WHILE
093500000000     C*
093600000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
093700000000     C     W             DIV       6             I
093800000000     C                   MVR                     Z
093900000000     C*
094000000000     C     Z             IFNE      0
094100000000     C     I             ADD       1             Z
094200000000     C                   ELSE
094300000000     C                   Z-ADD     I             Z                 5 0
094400000000     C                   END
094500000000     C*
094600000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
094700000000     C                   ADD       1             FUNGRU
094800000000     C     FUNGRU        IFGT      Z
094900000000     C     FUNGRU        ORLT      1
095000000000     C                   Z-ADD     1             FUNGRU
095100000000     C                   END
095200000000     C*
095300000000     C* PRIMO GRUPPO DI 6 FUNZIONI
095400000000     C     FUNGRU        IFEQ      1
095500000000     C                   MOVEL     FUV(1)        FUN001
095600000000     C                   MOVEL     FUV(2)        FUN002
095700000000     C                   MOVEL     FUV(3)        FUN003
095800000000     C                   MOVEL     FUV(4)        FUN004
095900000000     C                   MOVEL     FUV(5)        FUN005
096000000000     C                   MOVEL     FUV(6)        FUN006
096100000000     C                   END
096200000000     C*
096300000000     C* SECONDO GRUPPO DI 6 FUNZIONI
096400000000     C     FUNGRU        IFEQ      2
096500000000     C                   MOVEL     FUV(7)        FUN001
096600000000     C                   MOVEL     FUV(8)        FUN002
096700000000     C                   MOVEL     FUV(9)        FUN003
096800000000     C                   MOVEL     FUV(10)       FUN004
096900000000     C                   MOVEL     FUV(11)       FUN005
097000000000     C                   MOVEL     FUV(12)       FUN006
097100000000     C                   END
097200000000     C*
097300000000     C* TERZO GRUPPO DI 6 FUNZIONI
097400000000     C     FUNGRU        IFEQ      3
097500000000     C                   MOVEL     FUV(13)       FUN001
097600000000     C                   MOVEL     FUV(14)       FUN002
097700000000     C                   MOVEL     FUV(15)       FUN003
097800000000     C                   MOVEL     FUV(16)       FUN004
097900000000     C                   MOVEL     FUV(17)       FUN005
098000000000     C                   MOVEL     FUV(18)       FUN006
098100000000     C                   END
098200000000     C*
098300000000     C* QUARTO GRUPPO DI 6 FUNZIONI
098400000000     C     FUNGRU        IFEQ      4
098500000000     C                   MOVEL     FUV(19)       FUN001
098600000000     C                   MOVEL     FUV(20)       FUN002
098700000000     C                   MOVEL     FUV(21)       FUN003
098800000000     C                   MOVEL     FUV(22)       FUN004
098900000000     C                   MOVEL     *BLANKS       FUN005
099000000000     C                   MOVEL     *BLANKS       FUN006
099100000000     C                   END
099200000000     C*
099300000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099400000000     C                   MOVEL     *BLANKS       FUN007
099500000000     C                   MOVEL     *BLANKS       FUN008
099600000000     C*
099700000000     C     1             DO        24            I
099800000000     C                   MOVEL     FUD(I)        FUDRIG
099900000000     C     FUDFUN        IFEQ      '24'
100000000000     C     FUDATT        ANDEQ     'S'
100100000000     C                   MOVEL     FUDDES        FUN008
100200000000     C                   END
100300000000     C                   END                                                    DO
100400000000     C*
100500000000     C                   ENDSR
100600000000     C*------------------------------------------------------------------------*
100700000000     C* FUNCON - GESTIONE TASTI FUNZIONALI RICHIESTI
100800000000     C*------------------------------------------------------------------------*
100900000000     C     FUNCON        BEGSR
101000000000     C*
101100000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
101200000000     C                   EXSR      FUNVAR
101300000000     C*
101400000000     C* PULISCE CAMPI DI LAVORO
101500000000     C                   Z-ADD     0             I                 5 0
101600000000     C                   Z-ADD     0             W                 5 0
101700000000     C*
101800000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
101900000000IF  1C     FUNKEY        IFNE      *BLANKS
102000000000DO  2C     1             DO        24            I
102100000000     C                   MOVEL     FUD(I)        FUDRIG
102200000000     C*
102300000000     C* SALVA INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102400000000IF  3C     FUNKEY        IFEQ      FUDFUN
102500000000     C     FUDATT        ANDEQ     'S'
102600000000     C                   Z-ADD     I             W
102700000000E   3C                   ENDIF
102800000000E   2C                   ENDDO
102900000000     C*
103000000000     C* FUNZIONE NON ATTIVA, ERRORE
103100000000IF  2C     W             IFEQ      0
103200000000     C                   MOVEL     MSG(1)        DSMSMS
103300000000E   2C                   ENDIF
103400000000E   1C                   ENDIF
103500000000     C*
103600000000     C* FUNZIONE ATTIVA, PROCEDE
103700000000IF  1C     W             IFNE      0
103800000000     C     FUNKEY        OREQ      *BLANKS
103900000000IF  2C     FUNKEY        IFEQ      *BLANKS
104000000000     C                   EXSR      ENTGES
104100000000E   2C                   ENDIF
104200020311IF  2C*    FUNKEY        IFEQ      '01'
104300020311     C*                  EXSR      F01GES                                       *FINE LAVORO
104400020311E   2C*                  ENDIF
104500000000IF  2C     FUNKEY        IFEQ      '03'
104600000000     C                   EXSR      F03GES                                       *FINE LAVORO
104700000000E   2C                   ENDIF
104800000000IF  2C     FUNKEY        IFEQ      '06'
104900000000     C                   EXSR      F06GES                                       *LANCIO
105000000000E   2C                   ENDIF
105100000000IF  2C     FUNKEY        IFEQ      '10'
105200000000     C                   EXSR      F10GES                                       *ALTRE SELEZIONI
105300000000E   2C                   ENDIF
105400000000IF  2C     FUNKEY        IFEQ      '12'
105500000000     C                   EXSR      F12GES                                       *RITORNO
105600000000E   2C                   ENDIF
105700000000IF  2C     FUNKEY        IFEQ      '22'
105800000000     C                   EXSR      F22GES                                       *GESTIONE STAMPE
105900000000E   2C                   ENDIF
106000000000IF  2C     FUNKEY        IFEQ      '24'
106100000000     C                   EXSR      F24GES                                       *ALTRE FUNZIONI
106200000000E   2C                   ENDIF
106300000000E   1C                   ENDIF
106400000000     C*
106500000000     C                   ENDSR
106600000000     C*------------------------------------------------------------------------*
106700000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
106800000000     C*------------------------------------------------------------------------*
106900000000     C     FUNVAR        BEGSR
107000000000     C*
107100000000     C* INIZIALIZZA VARIABILE -FUNKEY-
107200000000     C*
107300000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
107400000000     C     KEY           IFEQ      ENTER
107500000000     C                   MOVEL     *BLANKS       FUNKEY
107600000000     C                   END
107700000000     C     KEY           IFEQ      F1
107800000000     C                   MOVEL     '01'          FUNKEY
107900000000     C                   END
108000000000     C     KEY           IFEQ      F2
108100000000     C                   MOVEL     '02'          FUNKEY
108200000000     C                   END
108300000000     C     KEY           IFEQ      F3
108400000000     C                   MOVEL     '03'          FUNKEY
108500000000     C                   END
108600000000     C     KEY           IFEQ      F4
108700000000     C                   MOVEL     '04'          FUNKEY
108800000000     C                   END
108900000000     C     KEY           IFEQ      F5
109000000000     C                   MOVEL     '05'          FUNKEY
109100000000     C                   END
109200000000     C     KEY           IFEQ      F6
109300000000     C                   MOVEL     '06'          FUNKEY
109400000000     C                   END
109500000000     C     KEY           IFEQ      F7
109600000000     C                   MOVEL     '07'          FUNKEY
109700000000     C                   END
109800000000     C     KEY           IFEQ      F8
109900000000     C                   MOVEL     '08'          FUNKEY
110000000000     C                   END
110100000000     C     KEY           IFEQ      F9
110200000000     C                   MOVEL     '09'          FUNKEY
110300000000     C                   END
110400000000     C     KEY           IFEQ      F10
110500000000     C                   MOVEL     '10'          FUNKEY
110600000000     C                   END
110700000000     C     KEY           IFEQ      F11
110800000000     C                   MOVEL     '11'          FUNKEY
110900000000     C                   END
111000000000     C     KEY           IFEQ      F12
111100000000     C                   MOVEL     '12'          FUNKEY
111200000000     C                   END
111300000000     C     KEY           IFEQ      F13
111400000000     C                   MOVEL     '13'          FUNKEY
111500000000     C                   END
111600000000     C     KEY           IFEQ      F14
111700000000     C                   MOVEL     '14'          FUNKEY
111800000000     C                   END
111900000000     C     KEY           IFEQ      F15
112000000000     C                   MOVEL     '15'          FUNKEY
112100000000     C                   END
112200000000     C     KEY           IFEQ      F16
112300000000     C                   MOVEL     '16'          FUNKEY
112400000000     C                   END
112500000000     C     KEY           IFEQ      F17
112600000000     C                   MOVEL     '17'          FUNKEY
112700000000     C                   END
112800000000     C     KEY           IFEQ      F18
112900000000     C                   MOVEL     '18'          FUNKEY
113000000000     C                   END
113100000000     C     KEY           IFEQ      F19
113200000000     C                   MOVEL     '19'          FUNKEY
113300000000     C                   END
113400000000     C     KEY           IFEQ      F20
113500000000     C                   MOVEL     '20'          FUNKEY
113600000000     C                   END
113700000000     C     KEY           IFEQ      F21
113800000000     C                   MOVEL     '21'          FUNKEY
113900000000     C                   END
114000000000     C     KEY           IFEQ      F22
114100000000     C                   MOVEL     '22'          FUNKEY
114200000000     C                   END
114300000000     C     KEY           IFEQ      F23
114400000000     C                   MOVEL     '23'          FUNKEY
114500000000     C                   END
114600000000     C     KEY           IFEQ      F24
114700000000     C                   MOVEL     '24'          FUNKEY
114800000000     C                   END
114900000000     C     KEY           IFEQ      ROLLUP
115000000000     C                   MOVEL     'RU'          FUNKEY
115100000000     C                   END
115200000000     C     KEY           IFEQ      HELP
115300000000     C                   MOVEL     'HP'          FUNKEY
115400000000     C                   END
115500000000     C                   ENDSR
115600000000     C*------------------------------------------------------------------------*
115700000000     C* NOTE PER COMPILAZIONE SCHIERE
115800000000     C*------------------------------------------------------------------------*
115900000000     C* -MSG-
116000000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
116100000000     C* (NON TOCCARE MSG,1)
116200000000     C*
116300000000     C* -CMD-
116400000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
116500000000     C*
116600000000     C* -FUD-
116700000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
116800000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
116900000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
117000000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
117100000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
117200000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
117300000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
117400000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
117500000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
117600000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
117700000000     C*
117800000000     C*------------------------------------------------------------------------*
117900000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
118000001016TIST44R- Tasto funzionale non gestito.                                         1
118100001016TIST44R- INSERIRE I GIORNI LAVORATIVI                                          2
118200001016TIST44R- Inserire tutti i limiti KG o nessuno                                  3
118300001016TIST44R- Inserire tutti i limiti MC o nessuno                                  4
118400001016TIST44R- Inserire almeno una serie di limiti o KG o MC                         5
118500001016TIST44R- Se inserito il punto operativo, inserire anche il suo AS/400          6
118600001016TIST44R- 'S' per stampare il dettaglio dei CAP e 'N' per i soli totali zona    7
118700001106TIST44R- Inserire i giorni lavorativi                                          8
118800001016TIST44R- 9                                                                     9
118900001016TIST44R- 10                                                                   10
119000001016TIST44R- 11                                                                   11
119100001016TIST44R- 12                                                                   12
119200001016TIST44R- Area non accettabile                                                 13
119300001016TIST44R- Punto operativo non accettabile                                      14
119400001016TIST44R- Non è un AS/400                                                      15
119500001016TIST44R- Data non valida                                                      16
119600001106TIST44R- File "ASSORBIMENTI" vuoto, necessario rigenerare l'archivio.         17
119700020226TIST44R- Network inesistente                                                  18
119800001016TIST44R- 19                                                                   19
119900001016TIST44R- 20                                                                   20
120000000000** ======== SCHIERA: CMD  (COMANDI)                        ================
120100000000WRKSPLF
120200000000
120300000000
120400000000
120500000000
120600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
120700020311F01=Aiuto          -        (S/N)=N                        funz.01¬
120800000000                   -        (S/N)=N                        funz.02¬
120900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
121000000000                   -        (S/N)=N                        funz.04¬
121100000000                   -        (S/N)=N                        funz.05¬
121200000000F06=Lancio         -        (S/N)=S                        funz.06¬
121300000000                   -        (S/N)=N                        funz.07¬
121400000000                   -        (S/N)=N                        funz.08¬
121500000000                   -        (S/N)=N                        funz.09¬
121600001016F10=Altre selezioni-        (S/N)=N                        funz.10¬
121700000000                   -        (S/N)=N                        funz.11¬
121800000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
121900000000                   -        (S/N)=N                        funz.13¬
122000000000                   -        (S/N)=N                        funz.14¬
122100000000                   -        (S/N)=N                        funz.15¬
122200000000                   -        (S/N)=N                        funz.16¬
122300000000                   -        (S/N)=N                        funz.17¬
122400000000                   -        (S/N)=N                        funz.18¬
122500000000                   -        (S/N)=N                        funz.19¬
122600000000                   -        (S/N)=N                        funz.20¬
122700000000                   -        (S/N)=N                        funz.21¬
122800000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
122900000000                   -        (S/N)=N                        funz.23¬
123000000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
