000100000000     H*------------------------------------------------------------------------*
000200950626     H* STATISTICHE AFFIDABILITA' - RICHIESTA PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800000000     FTNSS30D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(SS30DMS:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI
001700000000     D*-------------------
001800950626     D MSG             S             78    DIM(30) CTDATA PERRCD(1)
001900000000     D*-------------------
002000000000     D* COMANDI
002100000000     D*-------------------
002200000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002300000000     D*-------------------
002400000000     D* DESCRIZIONI TASTI FUNZIONALI
002500000000     D*-------------------
002600000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002700000000     D*-------------------
002800000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002900000000     D*-------------------
003000000000     D FUV             S             19    DIM(24)
003100000000     D*------------------------------------------------------------------------*
003200000000     D* INPUT
003300000000     D*------------------------------------------------------------------------*
003400000000     D*----------------
003500000000     D* RIDEFINIZIONE SCHIERA -FUD-
003600000000     D*----------------
003700000000     D FUDDS           DS
003800000000     D  FUDDE1                 1      1
003900000000     D  FUDFUN                 2      3
004000000000     D  FUDDE2                 4     19
004100000000     D  FUDFI0                20     34
004200000000     D  FUDATT                35     35
004300000000     D  FUDFI1                36     80
004400000000     D  FUDDES                 1     19
004500000000     D  FUDRIG                 1     80
004600000000     D*----------------
004700000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800000000     D*----------------
004900000000     D DSMSG         E DS
005000000000     D*----------------
005100000000     D* DS CONTROLLO DATE
005200000000     D*----------------
005300971020     D WLBDA8          DS
005400971020     D  G08DAT                 1      8  0
005500971020     D  G08INV                 9     16  0
005600971020     D  G08ERR                17     17
005700971020     D  G08TGI                18     22  0
005800000000     D*----------------
005900000000     D* DS PER GESTIONE TASTI FUNZIONALI
006000000000     D*----------------
006100000000     D DSKEY         E DS
006200000000     D DEVDS1          DS
006300000000     D  KEY                  369    369
006400971020     D*------------------
006500971020     D* DS "FNLV55R" - REPERIMENTO TERMINALs
006600971020     D*------------------
006700971020     D DSLV55        E DS                  EXTNAME(FNLV55DS)
006800000000     D*----------------
006900000000     D* ARCHITETTURA
007000000000     D*----------------
007100000000     D KPJBA         E DS
007200970401     D*----------------
007300970401     D* DS PER TABELLA CODICI AREA
007400970401     D*----------------
007500970401     D DS05          E DS
007600970401     D*----------------
007700970401     D* DS PER TABELLA CODICI DISTRETTO
007800970401     D*----------------
007900970401     D DS17          E DS
008000000000     D*----------------
008100000000     D* DS PARAMETRI DI LANCIO PGM
008200000000     D*----------------
008300950626     D PARAM           DS
008400000000     D  PARANN                 1      4  0
008500000000     D  PARMMI                 5      6  0
008600000000     D  PARMMF                 7      8  0
008700000000     D  PARSNC                 9      9
008800000000     D  PARSDC                10     10
008900000000     D  PARSNA                11     11
009000000000     D  PARSDA                12     12
009100000000     D  PARSNR                13     13
009200000000     D  PARDIV                14     14
009300000000     D  PARARE                15     17  0
009400000000     D  PARFLP                18     20  0
009500000000     D  PARTFP                21     23  0
009600000000     D  PARFLA                24     26  0
009700000000     D  PARTFA                27     29  0
009800000000     D  PARDTI                30     37  0
009900000000     D  PARDTF                38     45  0
010000000000     D  PARTLA                46     46
010100000000     D  PARSNP                47     47
010200950712     D  PARTOT                48     48
010300950712     D  PARIOE                49     49
010400970826     D  PARCFP                50     50
010500000000     C*------------------------------------------------------------------------*
010600000000     C* MAIN LINE
010700000000     C*------------------------------------------------------------------------*
010800000000     C*
010900000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
011000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
011100000000     C*
011200000000     C* CICLO FINO A CHE:
011300000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011400000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
011500000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
011600000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
011700000000     C     F12L00        OREQ      '1'
011800000000     C     FLFINE        OREQ      '1'
011900000000     C*
012000000000     C* EMETTE FORMATO VIDEO
012100000000     C                   EXSR      BUTFMR
012200000000     C*
012300000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
012400000000     C                   EXSR      FUNCON
012500000000     C                   END                                                    <-- 2 --
012600000000     C                   END                                                    <-- 1 --
012700971020     C* OPERAZIONI FINALI
012800971020     C                   EXSR      FINSR
012900971020     C*
013000000000     C                   SETON                                        LR
013100000000     C*------------------------------------------------------------------------*
013200000000     C* EMISSIONE FORMATO A VIDEO
013300000000     C*------------------------------------------------------------------------*
013400000000     C     BUTFMR        BEGSR
013500000000     C*
013600000000     C* WRITE RECORD TESTATA
013700000000     C                   WRITE     SS30D01
013800000000     C*
013900000000     C* VISUALIZZAZIONE MESSAGGIO
014000000000     C                   EXSR      SNDMSG
014100000000     C*
014200000000     C* WRITE RECORD FUNZIONI
014300000000     C                   WRITE     SS30D02
014400000000     C*
014500000000     C* EXFMT RECORD DETTAGLIO
014600000000     C                   EXFMT     SS30D03
014700000000     C*
014800000000     C* AZZERO FLAG DI ERRORE LIVELLO
014900000000     C                   MOVEL     '0'           ERRL00
015000000000     C*
015100000000     C* AZZERO FLAG DI ERRORE
015200000000     C                   SETOFF                                       69
015300000000     C                   SETOFF                                       707172
015400000000     C                   SETOFF                                       737475
015500000000     C                   SETOFF                                       767778
015600000000     C                   SETOFF                                       798081
015700000000     C                   SETOFF                                       828384
015800000000     C                   SETOFF                                       858687
015900970826     C                   SETOFF                                       8889
016000000000     C*
016100000000     C                   ENDSR
016200000000     C*------------------------------------------------------------------------*
016300000000     C* SNDMSG - MANDA MESSAGGIO
016400000000     C*------------------------------------------------------------------------*
016500000000     C     SNDMSG        BEGSR
016600000000     C*
016700000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
016800000000     C                   MOVEL     '1'           *IN40
016900000000     C                   WRITE     SS30DMC
017000000000     C                   MOVEL     '0'           *IN40
017100000000     C*
017200000000     C* VISUALIZZAZIONE MESSAGGIO
017300000000     C                   Z-ADD     0             MSGULT            4 0
017400000000     C                   CALL      'XMSG'        XMSGPA
017500000000     C     DSMSER        IFEQ      0
017600000000     C                   MOVEL     DSMSKE        MSGKEY
017700000000     C                   END
017800000000     C                   Z-ADD     1             MSGULT
017900000000     C                   WRITE     SS30DMS                                      SFL MESS
018000000000     C                   WRITE     SS30DMC                                      CONTR MESS
018100000000     C*
018200000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
018300000000     C                   MOVEL     *BLANKS       DSMSMS
018400000000     C*
018500000000     C                   ENDSR
018600000000     C*------------------------------------------------------------------------*
018700000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
018800000000     C*------------------------------------------------------------------------*
018900000000     C     ENTGES        BEGSR
019000000000     C*
019100000000     C* CONTROLLA I CAMPI IMMESSI
019200000000     C                   EXSR      DECODI
019300000000     C*
019400000000     C                   ENDSR
019500000000     C*------------------------------------------------------------------------*
019600000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
019700000000     C*------------------------------------------------------------------------*
019800000000     C     F01GES        BEGSR
019900000000     C                   ENDSR
020000000000     C*------------------------------------------------------------------------*
020100000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
020200000000     C*------------------------------------------------------------------------*
020300000000     C     F03GES        BEGSR
020400000000     C*
020500000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
020600000000     C                   MOVEL     '1'           F03L00
020700000000     C*
020800000000     C                   ENDSR
020900000000     C*------------------------------------------------------------------------*
021000000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
021100000000     C*------------------------------------------------------------------------*
021200000000     C     F06GES        BEGSR
021300000000     C*
021400000000     C* LANCIO IL PROGRAMMA DI STAMPA
021500000000     C                   MOVEL     'SS31'        KCOAZ
021600000000     C                   EXSR      VIDPAR
021700950626     C                   MOVEL     PARAM         KPJBU
021800950626     C                   CALL      'BCH10'
021900000000     C                   PARM                    KPJBA
022000000000     C*
022100950627     C* RIMETTE FILE DA RIGENERARE = 'N' PER SICUREZZA
022200950627     C                   MOVEL     'N'           VIDSNR                         *FILE DA RIGENERARE
022300970826     C                   MOVEL     'N'           VIDCFP                         *FILE PC DA RICREARE
022400950627     C*
022500000000     C                   ENDSR
022600000000     C*------------------------------------------------------------------------*
022700000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
022800000000     C*------------------------------------------------------------------------*
022900000000     C     F07GES        BEGSR
023000000000     C                   ENDSR
023100000000     C*------------------------------------------------------------------------*
023200000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
023300000000     C*------------------------------------------------------------------------*
023400000000     C     F08GES        BEGSR
023500000000     C                   ENDSR
023600000000     C*------------------------------------------------------------------------*
023700000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
023800000000     C*------------------------------------------------------------------------*
023900000000     C     F12GES        BEGSR
024000000000     C*
024100000000     C* IMPOSTO FLAG DI LIVELLO
024200000000     C                   MOVEL     '1'           F12L00
024300000000     C*
024400000000     C                   ENDSR
024500000000     C*------------------------------------------------------------------------*
024600000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
024700000000     C*------------------------------------------------------------------------*
024800000000     C     F21GES        BEGSR
024900000000     C                   ENDSR
025000000000     C*------------------------------------------------------------------------*
025100000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
025200000000     C*------------------------------------------------------------------------*
025300000000     C     F22GES        BEGSR
025400000000     C*
025500000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
025600000000     C                   Z-ADD     80            LENGH            15 5
025700000000     C                   CALL      'QCAEXEC'
025800000000     C                   PARM                    CMD(1)
025900000000     C                   PARM                    LENGH
026000000000     C*
026100000000     C                   ENDSR
026200000000     C*------------------------------------------------------------------------*
026300000000     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
026400000000     C*------------------------------------------------------------------------*
026500000000     C     F24GES        BEGSR
026600000000     C*
026700000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
026800000000     C                   EXSR      FUNGES
026900000000     C*
027000000000     C                   ENDSR
027100000000     C*------------------------------------------------------------------------*
027200000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
027300000000     C*------------------------------------------------------------------------*
027400000000     C     HELGES        BEGSR
027500000000     C                   ENDSR
027600000000     C*------------------------------------------------------------------------*
027700000000     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
027800000000     C*------------------------------------------------------------------------*
027900000000     C     DECODI        BEGSR
028000000000     C*
028100000000     C* CONTROLLO FLAG TIPO DI LANCIO
028200000000     C     ERRL00        IFEQ      '0'
028300000000     C     VIDTLA        IFEQ      'S'
028400000000     C     VIDTLA        OREQ      'E'
028500000000     C                   ELSE
028600000000     C                   MOVEL     '1'           ERRL00
028700000000     C                   MOVEL     MSG(19)       DSMSMS
028800000000     C                   SETON                                        69
028900000000     C                   END
029000000000     C                   END
029100000000     C*
029200000000     C* CONTROLLO IN BASE AL TIPO DI LANCIO:
029300000000     C* ====> LANCIO PER SALDI:        ANNO/MESE
029400000000     C     VIDTLA        IFEQ      'S'                                          --- 1 -->
029500000000     C*
029600000000     C* CONTROLLO ANNO
029700000000IF  1C     ERRL00        IFEQ      '0'
029800000000IF  2C     VIDANN        IFEQ      *ZEROS
029900000000     C                   MOVEL     '1'           ERRL00
030000000000     C                   MOVEL     MSG(3)        DSMSMS
030100000000     C                   SETON                                        70
030200000000X   2C                   ELSE
030300000000IF  3C     VIDANN        IFLT      1900
030400000000     C                   MOVEL     '1'           ERRL00
030500000000     C                   MOVEL     MSG(3)        DSMSMS
030600000000     C                   SETON                                        70
030700000000E   3C                   END
030800000000E   2C                   END
030900000000E   1C                   END
031000000000     C*
031100000000     C* CONTROLLO MESE INIZIO / FINE
031200000000     C     ERRL00        IFEQ      '0'
031300000000     C     VIDMMI        IFEQ      *ZEROS                                       *INIZIO
031400000000     C     VIDMMI        ORGT      12
031500000000     C                   MOVEL     '1'           ERRL00
031600000000     C                   MOVEL     MSG(4)        DSMSMS
031700000000     C                   SETON                                        71
031800000000     C                   END
031900000000     C                   END
032000000000     C     ERRL00        IFEQ      '0'
032100000000     C     VIDMMF        IFEQ      *ZEROS                                       *FINE
032200000000     C                   Z-ADD     VIDMMI        VIDMMF                         *LO IMPOSTO COME INI
032300000000     C                   ELSE
032400000000     C     VIDMMF        IFGT      12
032500000000     C                   MOVEL     '1'           ERRL00
032600000000     C                   MOVEL     MSG(5)        DSMSMS
032700000000     C                   SETON                                        72
032800000000     C                   END
032900000000     C                   END
033000000000     C                   END
033100000000     C     ERRL00        IFEQ      '0'                                          *INIZIO MAGGIORE FIN
033200000000     C     VIDMMF        IFNE      *ZEROS
033300000000     C     VIDMMI        IFGT      VIDMMF
033400000000     C                   MOVEL     '1'           ERRL00
033500000000     C                   MOVEL     MSG(6)        DSMSMS
033600000000     C                   SETON                                        7172
033700000000     C                   END
033800000000     C                   END
033900000000     C                   END
034000000000     C*
034100000000     C* CONTROLLO FLAG FILE DA RIGENERARE
034200000000     C     ERRL00        IFEQ      '0'
034300000000     C     VIDSNR        IFEQ      'S'
034400000000     C     VIDSNR        OREQ      'N'
034500000000     C                   ELSE
034600000000     C                   MOVEL     '1'           ERRL00
034700000000     C                   MOVEL     MSG(11)       DSMSMS
034800000000     C                   SETON                                        77
034900000000     C                   END
035000000000     C                   END
035100970826     C*
035200970826     C* CONTROLLO FLAG FILE PC DA RICREARE
035300970826     C     ERRL00        IFEQ      '0'
035400970826     C     VIDCFP        IFEQ      'S'
035500970826     C     VIDCFP        OREQ      'N'
035600970826     C                   ELSE
035700970826     C                   MOVEL     '1'           ERRL00
035800970826     C                   MOVEL     MSG(21)       DSMSMS
035900970826     C                   SETON                                        89
036000970826     C                   END
036100970826     C                   END
036200000000     C*
036300000000     C* CONTROLLO DATA DI INIZIO (NON DEVE ESSERE INSERITA SE LANCIO X SALDI)
036400000000     C     ERRL00        IFEQ      '0'
036500000000     C     VIDDTI        IFNE      *ZEROS
036600000000     C                   MOVEL     '1'           ERRL00
036700000000     C                   MOVEL     MSG(17)       DSMSMS
036800000000     C                   SETON                                        84
036900000000     C                   END
037000000000     C                   END
037100000000     C*
037200000000     C* CONTROLLO DATA DI FINE (NON DEVE ESSERE INSERITA SE LANCIO X SALDI)
037300000000     C     ERRL00        IFEQ      '0'
037400000000     C     VIDDTF        IFNE      *ZEROS
037500000000     C                   MOVEL     '1'           ERRL00
037600000000     C                   MOVEL     MSG(17)       DSMSMS
037700000000     C                   SETON                                        85
037800000000     C                   END
037900000000     C                   END
038000000000     C*
038100000000     C                   END                                                    <-- 1 ---
038200000000     C*
038300000000     C* ===> LANCIO ESTEMPORANEO:     DATA INIZIO/DATA FINE
038400000000     C     VIDTLA        IFEQ      'E'                                          --- 1 -->
038500000000     C*
038600000000     C* CONTROLLO DATA DI INIZIO
038700000000     C     ERRL00        IFEQ      '0'
038800971020     C                   Z-ADD     VIDDTI        G08DAT
038900971020     C                   Z-ADD     *ZEROS        G08INV
039000971020     C                   MOVEL     '0'           G08ERR
039100000000     C                   CALL      'XSRDA8'
039200971020     C                   PARM                    WLBDA8
039300971020     C     G08ERR        IFEQ      '0'
039400971020     C                   Z-ADD     G08INV        WDTI              8 0
039500971020     C                   Z-ADD     G08DAT        VIDDTI
039600000000     C                   ELSE
039700000000     C                   MOVEL     '1'           ERRL00
039800000000     C                   MOVEL     MSG(16)       DSMSMS
039900000000     C                   SETON                                        84
040000000000     C                   END
040100000000     C                   END
040200000000     C*
040300000000     C* CONTROLLO DATA DI FINE
040400000000     C     ERRL00        IFEQ      '0'
040500971020     C                   Z-ADD     VIDDTF        G08DAT
040600971020     C                   Z-ADD     *ZEROS        G08INV
040700971020     C                   MOVEL     '0'           G08ERR
040800000000     C                   CALL      'XSRDA8'
040900971020     C                   PARM                    WLBDA8
041000971020     C     G08ERR        IFEQ      '0'
041100971020     C                   Z-ADD     G08INV        WDTF              8 0
041200971020     C                   Z-ADD     G08DAT        VIDDTF
041300000000     C                   ELSE
041400000000     C                   MOVEL     '1'           ERRL00
041500000000     C                   MOVEL     MSG(16)       DSMSMS
041600000000     C                   SETON                                        85
041700000000     C                   END
041800000000     C                   END
041900000000     C*
042000000000     C* CONTROLLO ANNO (NON DEVE ESSERE INSERITO SE LANCIO ESTEMPORANEO)
042100000000     C     ERRL00        IFEQ      '0'
042200000000     C     VIDANN        IFNE      *ZEROS
042300000000     C                   MOVEL     '1'           ERRL00
042400000000     C                   MOVEL     MSG(18)       DSMSMS
042500000000     C                   SETON                                        70
042600000000     C                   END
042700000000     C                   END
042800000000     C*
042900000000     C* CONTROLLO MESE INIZIO (NON DEVE ESSERE INSERITO SE LANCIO ESTEMPORANEO)
043000000000     C     ERRL00        IFEQ      '0'
043100000000     C     VIDMMI        IFNE      *ZEROS
043200000000     C                   MOVEL     '1'           ERRL00
043300000000     C                   MOVEL     MSG(18)       DSMSMS
043400000000     C                   SETON                                        71
043500000000     C                   END
043600000000     C                   END
043700000000     C*
043800000000     C* CONTROLLO MESE FINE (NON DEVE ESSERE INSERITO SE LANCIO ESTEMPORANEO)
043900000000     C     ERRL00        IFEQ      '0'
044000000000     C     VIDMMF        IFNE      *ZEROS
044100000000     C                   MOVEL     '1'           ERRL00
044200000000     C                   MOVEL     MSG(18)       DSMSMS
044300000000     C                   SETON                                        72
044400000000     C                   END
044500000000     C                   END
044600000000     C*
044700000000     C                   END                                                    <-- 1 ---
044800000000     C*
044900000000     C* CONTROLLO FLAG AFFIDABILITA' CLIENTE
045000000000     C     ERRL00        IFEQ      '0'
045100000000     C     VIDSNC        IFEQ      'S'
045200000000     C     VIDSNC        OREQ      'N'
045300000000     C     VIDSDC        IFEQ      'S'                                          *SINT/DETT
045400000000     C     VIDSDC        OREQ      'D'
045500000000     C                   ELSE
045600000000     C                   MOVEL     '1'           ERRL00
045700000000     C                   MOVEL     MSG(8)        DSMSMS
045800000000     C                   SETON                                        74
045900000000     C                   END
046000000000     C                   ELSE
046100000000     C                   MOVEL     '1'           ERRL00
046200000000     C                   MOVEL     MSG(7)        DSMSMS
046300000000     C                   SETON                                        73
046400000000     C                   END
046500000000     C                   END
046600000000     C*
046700000000     C* CONTROLLO FLAG AFFIDABILITA' CONSEGNE
046800000000     C     ERRL00        IFEQ      '0'
046900000000     C     VIDSNA        IFEQ      'S'
047000000000     C     VIDSNA        OREQ      'N'
047100000000     C     VIDSDA        IFEQ      'S'                                          *SINT/DETT
047200000000     C     VIDSDA        OREQ      'D'
047300000000     C                   ELSE
047400000000     C                   MOVEL     '1'           ERRL00
047500000000     C                   MOVEL     MSG(10)       DSMSMS
047600000000     C                   SETON                                        76
047700000000     C                   END
047800000000     C                   ELSE
047900000000     C                   MOVEL     '1'           ERRL00
048000000000     C                   MOVEL     MSG(9)        DSMSMS
048100000000     C                   SETON                                        75
048200000000     C                   END
048300000000     C                   END
048400000000     C*
048500000000     C* CONTROLLO FLAG AFFIDABILITA' PARTENZE
048600000000     C     ERRL00        IFEQ      '0'
048700000000     C     VIDSNP        IFEQ      'S'
048800000000     C     VIDSNP        OREQ      'N'
048900000000     C                   ELSE
049000000000     C                   MOVEL     '1'           ERRL00
049100000000     C                   MOVEL     MSG(7)        DSMSMS
049200000000     C                   SETON                                        86
049300000000     C                   END
049400000000     C                   END
049500950626     C*
049600950626     C* CONTROLLA IL TIPO DI DATI IN STATISTICA
049700950626IF  1C     ERRL00        IFEQ      '0'
049800950626IF  2C     VIDIOE        IFEQ      'I'
049900950626     C     VIDIOE        OREQ      'M'
050000950626     C     VIDIOE        OREQ      'X'
050100950626     C     VIDIOE        OREQ      *BLANKS
050200950626X   2C                   ELSE
050300950626     C                   MOVEL     '1'           ERRL00
050400950626     C                   MOVEL     MSG(29)       DSMSMS
050500950626     C                   SETON                                        88
050600950626E   2C                   ENDIF
050700950626E   1C                   ENDIF
050800000000     C*
050900000000     C* CONTROLLO FLAG STAMPA TOTALI AREA/DIVIZIONE/GENERALE PER DETTAGLIO
051000000000IF  1C     ERRL00        IFEQ      '0'
051100000000IF  2C     VIDTOT        IFEQ      'S'
051200000000     C     VIDTOT        OREQ      'N'
051300000000X   2C                   ELSE
051400000000     C                   MOVEL     '1'           ERRL00
051500000000     C                   MOVEL     MSG(20)       DSMSMS
051600000000     C                   SETON                                        87
051700000000E   2C                   ENDIF
051800000000E   1C                   ENDIF
051900000000     C*
052000000000     C* CONTROLLO LA DIVISIONE
052100000000     C                   MOVEL     *BLANKS       VDDDIV
052200000000     C     ERRL00        IFEQ      '0'
052300000000     C     VIDDIV        IFNE      *BLANKS
052400000000     C                   EXSR      CHKDIV
052500000000     C     ERRL00        IFEQ      '1'
052600000000     C                   MOVEL     '1'           ERRL00
052700000000     C                   MOVEL     MSG(12)       DSMSMS
052800000000     C                   SETON                                        78
052900000000     C                   END
053000000000     C                   END
053100000000     C                   END
053200000000     C*
053300000000     C* CONTROLLO L'AREA
053400000000     C                   MOVEL     *BLANKS       VDDARE
053500000000     C     ERRL00        IFEQ      '0'
053600000000     C     VIDARE        IFNE      *BLANKS
053700000000     C                   EXSR      CHKARE
053800000000     C     ERRL00        IFEQ      '1'
053900000000     C                   MOVEL     '1'           ERRL00
054000000000     C                   MOVEL     MSG(13)       DSMSMS
054100000000     C                   SETON                                        79
054200000000     C                   END
054300000000     C                   END
054400000000     C                   END
054500000000     C*
054600000000     C* CONTROLLO LA FILIALE DI PARTENZA
054700000000     C                   MOVEL     *BLANKS       VDDFLP
054800000000     C     ERRL00        IFEQ      '0'
054900000000     C     VIDFLP        IFNE      *BLANKS
055000000000     C                   MOVE      VIDFLP        WLIN
055100000000     C                   EXSR      CHKLIN
055200000000     C     ERRL00        IFEQ      '0'
055300000000     C                   MOVEL     WLIN          VIDFLP
055400000000     C                   MOVEL     WDLIN         VDDFLP
055500000000     C                   ELSE
055600000000     C                   SETON                                        80
055700000000     C                   END
055800000000     C                   END
055900000000     C                   END
056000000000     C*
056100000000     C* CONTROLLO IL TERMINAL DI PARTENZA
056200000000     C                   MOVEL     *BLANKS       VDDTFP
056300971020IF  1C     ERRL00        IFEQ      '0'
056400971020IF  2C     VIDTFP        IFNE      *BLANKS
056500000000     C                   MOVE      VIDTFP        WLIN
056600000000     C                   EXSR      CHKLIN
056700971020IF  3C     ERRL00        IFNE      '0'
056800971020     C                   SETON                                        81
056900971020X   3C                   ELSE
057000000000     C                   MOVEL     WLIN          VIDTFP
057100000000     C                   MOVEL     WDLIN         VDDTFP
057200971020     C                   CLEAR                   DSLV55
057300971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
057400971020     C                   MOVEL     'P'           D55TPT                         *ECCEZIONE ARRIVO
057500971020     C                   MOVEL     VIDTFP        D55LIN                         *LINEA
057600971020     C                   Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
057700971020     C                   CALL      'FNLV55R'
057800971020     C                   PARM                    DSLV55
057900971020     C                   MOVEL     D55TFP        WTF               3
058000971020IF  4C     VIDTFP        IFNE      WTF
058100000000     C                   MOVEL     '1'           ERRL00
058200000000     C                   MOVEL     MSG(15)       DSMSMS
058300000000     C                   SETON                                        81
058400971020E   4C                   ENDIF
058500971020E   3C                   ENDIF
058600971020E   2C                   ENDIF
058700971020E   1C                   ENDIF
058800000000     C*
058900000000     C* CONTROLLO LA FILIALE DI ARRIVO
059000000000     C                   MOVEL     *BLANKS       VDDFLA
059100000000     C     ERRL00        IFEQ      '0'
059200000000     C     VIDFLA        IFNE      *BLANKS
059300000000     C                   MOVE      VIDFLA        WLIN
059400000000     C                   EXSR      CHKLIN
059500000000     C     ERRL00        IFEQ      '0'
059600000000     C                   MOVEL     WLIN          VIDFLA
059700000000     C                   MOVEL     WDLIN         VDDFLA
059800000000     C                   ELSE
059900000000     C                   SETON                                        82
060000000000     C                   END
060100000000     C                   END
060200000000     C                   END
060300000000     C*
060400000000     C* CONTROLLO IL TERMINAL DI ARRIVO
060500000000     C                   MOVEL     *BLANKS       VDDTFA
060600971020IF  1C     ERRL00        IFEQ      '0'
060700971020IF  2C     VIDTFA        IFNE      *BLANKS
060800000000     C                   MOVE      VIDTFA        WLIN
060900000000     C                   EXSR      CHKLIN
061000971020IF  3C     ERRL00        IFNE      '0'
061100971020     C                   SETON                                        83
061200971020X   3C                   ELSE
061300000000     C                   MOVEL     WLIN          VIDTFA
061400000000     C                   MOVEL     WDLIN         VDDTFA
061500971020     C                   CLEAR                   DSLV55
061600971020     C                   MOVEL     *BLANKS       D55TLA                         *TIPO LANCIO
061700971020     C                   MOVEL     'A'           D55TPT                         *ECCEZIONE ARRIVO
061800971020     C                   MOVEL     VIDTFA        D55LIN                         *LINEA
061900971020     C                   Z-ADD     DATCOR        D55DRF                         *DATA RIFERIMENTO
062000971020     C                   CALL      'FNLV55R'
062100971020     C                   PARM                    DSLV55
062200971020     C                   MOVEL     D55TFA        WTF               3
062300971020IF  4C     VIDTFA        IFNE      WTF
062400000000     C                   MOVEL     '1'           ERRL00
062500000000     C                   MOVEL     MSG(15)       DSMSMS
062600000000     C                   SETON                                        83
062700971020E   4C                   ENDIF
062800971020E   3C                   ENDIF
062900971020E   2C                   ENDIF
063000971020E   1C                   ENDIF
063100000000     C*
063200000000     C                   ENDSR
063300000000     C*------------------------------------------------------------------------*
063400000000     C*  CHKDIV - CONTROLLA LA DIVISIONE
063500000000     C*------------------------------------------------------------------------*
063600000000     C     CHKDIV        BEGSR
063700000000     C*
063800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
063900000000     C     VIDDIV        IFEQ      '?'                                          --- 1 -->
064000020906     C                   MOVEL     1             §KUT              1 0
064100000000     C                   MOVEL     '17'          §COD              2
064200000000     C                   MOVE      *BLANKS       §DES1
064300000000     C                   CALL      'X§TABER'
064400000000     C                   PARM                    §KUT
064500000000     C                   PARM                    §COD
064600000000     C                   PARM                    §KEY              8
064700970401     C                   PARM                    §DES             25
064800000000     C                   MOVEL     §KEY          VIDDIV
064900000000     C                   MOVEL     §DES          VDDDIV
065000000000     C*
065100000000     C                   ELSE                                                   <-- 1 -->
065200000000     C*
065300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
065400020906     C                   MOVEL     1             KTBUTE
065500000000     C                   MOVE      '17'          KTBCOD
065600000000     C                   MOVE      *BLANKS       KTBKEY
065700000000     C                   MOVEL     VIDDIV        KTBKEY
065800000000     C     KEYTAB        CHAIN     TABEL                              99
065900000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
066000000000     C     TBLFLG        OREQ      '*'
066100000000     C                   MOVEL     '1'           ERRL00
066200000000     C                   MOVEL     *ALL'*'       VDDDIV
066300000000     C                   ELSE                                                   <-- 2 -->
066400970401     C                   MOVEL     TBLUNI        DS17
066500970401     C                   MOVEL     §17DES        VDDDIV
066600000000     C                   END                                                    <-- 2 ---
066700000000     C*
066800000000     C                   END                                                    <-- 1 ---
066900000000     C*
067000000000     C                   ENDSR
067100000000     C*------------------------------------------------------------------------*
067200000000     C*  CHKARE - CONTROLLA L'AREA
067300000000     C*------------------------------------------------------------------------*
067400000000     C     CHKARE        BEGSR
067500000000     C*
067600000000     C* CONTROLLO SE E' STATO INSERITO '?'
067700000000     C     '?'           SCAN      VIDARE        A                 1 0    99
067800000000     C*
067900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
068000000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
068100020906     C                   MOVEL     1             §KUT              1 0
068200000000     C                   MOVEL     '05'          §COD              2
068300000000     C                   MOVE      *BLANKS       §DES1
068400000000     C                   CALL      'X§TABER'
068500000000     C                   PARM                    §KUT
068600000000     C                   PARM                    §COD
068700000000     C                   PARM                    §KEY              8
068800970401     C                   PARM                    §DES             25
068900000000     C                   MOVEL     §KEY          VIDARE
069000000000     C                   MOVEL     §DES          VDDARE
069100000000     C*
069200000000     C                   ELSE                                                   <-- 1 -->
069300000000     C*
069400000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
069500020906     C                   MOVEL     1             KTBUTE
069600000000     C                   MOVE      '05'          KTBCOD
069700000000     C                   MOVE      *BLANKS       KTBKEY
069800000000     C                   MOVEL     VIDARE        KTBKEY
069900000000     C     KEYTAB        CHAIN     TABEL                              99
070000000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
070100000000     C     TBLFLG        OREQ      '*'
070200000000     C                   MOVEL     '1'           ERRL00
070300000000     C                   MOVEL     *ALL'*'       VDDARE
070400000000     C                   ELSE                                                   <-- 2 -->
070500970401     C                   MOVEL     TBLUNI        DS05
070600970401     C                   MOVEL     §05DES        VDDARE
070700000000     C                   END                                                    <-- 2 ---
070800000000     C*
070900000000     C                   END                                                    <-- 1 ---
071000000000     C*
071100000000     C                   ENDSR
071200000000     C*------------------------------------------------------------------------*
071300000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
071400000000     C*------------------------------------------------------------------------*
071500000000     C     CHKLIN        BEGSR
071600000000     C*
071700000000     C* CONTROLLO SE E' STATO INSERITO '?'
071800000000     C     '?'           SCAN      WLIN          A                 1 0    99
071900000000     C*
072000000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
072100000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
072200000000     C                   MOVEL     *BLANKS       §COD1             3
072300000000     C                   MOVE      *BLANKS       §DES1
072400971203     C                   MOVE      *BLANKS       §PRA
072500971203     C                   CALL      'TNSD24R'
072600000000     C                   PARM                    §COD1
072700000000     C                   PARM                    §TIP              1
072800000000     C                   PARM                    §DES1            25
072900971203     C                   PARM                    §PRA              3
073000000000     C                   MOVEL     §COD1         WLIN              3
073100000000     C                   MOVEL     §DES1         WDLIN            20
073200000000     C*
073300000000     C                   END                                                    <-- 1 ---
073400000000     C*
073500000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
073600000000     C                   MOVE      WLIN          KORFIL
073700000000     C     KEYORG        CHAIN     AZORG                              99
073800000000     C     *IN99         IFEQ      '0'                                          --- 2 -->
073900000000     C                   MOVEL     ORGFIL        WLIN
074000000000     C                   MOVEL     ORGDES        WDLIN
074100000000     C                   ELSE                                                   <-- 2 -->
074200000000     C                   MOVEL     '1'           ERRL00
074300000000     C                   MOVEL     MSG(14)       DSMSMS
074400000000     C                   MOVEL     *ALL'*'       WDLIN
074500000000     C                   END                                                    <-- 2 ---
074600000000     C     ORGFAG        IFEQ      'V'                                          --- 2 -->
074700000000     C                   MOVEL     '1'           ERRL00
074800000000     C                   MOVEL     MSG(14)       DSMSMS
074900000000     C                   MOVEL     *ALL'*'       WDLIN
075000000000     C                   END                                                    <-- 2 ---
075100000000     C*
075200000000     C                   ENDSR
075300000000     C*------------------------------------------------------------------------*
075400000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
075500000000     C*------------------------------------------------------------------------*
075600000000     C     AZZVID        BEGSR
075700000000     C*
075800000000     C                   Z-ADD     *ZEROS        VIDANN
075900000000     C                   Z-ADD     *ZEROS        VIDMMI
076000000000     C                   Z-ADD     *ZEROS        VIDMMF
076100950713     C                   MOVEL     'S'           VIDTLA                         *LANCIO PER SALDI
076200000000     C                   MOVEL     'N'           VIDSNR                         *FILE DA RIGENERARE
076300970826     C                   MOVEL     'N'           VIDCFP                         *FILE PC DA RICREARE
076400000000     C                   MOVEL     'S'           VIDSNC                         *AFF.TA' CLIENTE
076500000000     C                   MOVEL     'S'           VIDSDC                         *DETTAGLIO CLIENTE
076600000000     C                   MOVEL     'S'           VIDSNA                         *AFF.TA' CONSEGNA
076700000000     C                   MOVEL     'S'           VIDSDA                         *DETTAGLIO CONSEGNA
076800000000     C                   MOVEL     'S'           VIDSNP                         *AFF.TA PARTENZE
076900950626     C                   MOVEL     'I'           VIDIOE                         *DATI IN STATISTICA
077000000000     C                   MOVEL     'N'           VIDTOT                         *TOTALI IN STAMPA
077100000000     C*
077200000000     C                   ENDSR
077300000000     C*------------------------------------------------------------------------*
077400000000     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
077500000000     C*------------------------------------------------------------------------*
077600000000     C     VIDPAR        BEGSR
077700000000     C*
077800000000     C                   Z-ADD     VIDANN        PARANN                         *ANNO
077900000000     C                   Z-ADD     VIDMMI        PARMMI                         *MESE INIZIALE
078000000000     C                   Z-ADD     VIDMMF        PARMMF                         *MESE FINALE
078100000000     C                   MOVEL     VIDSNC        PARSNC                         *CLENTE
078200000000     C                   MOVEL     VIDSDC        PARSDC                         *DETTAGLIO CLIENTE
078300000000     C                   MOVEL     VIDSNA        PARSNA                         *CONSEGNA
078400000000     C                   MOVEL     VIDSDA        PARSDA                         *DETTAGLIO CONSEGNA
078500000000     C                   MOVEL     VIDSNP        PARSNP                         *PARTENZA
078600000000     C                   MOVEL     VIDTOT        PARTOT                         *TOTALI AREA/DIV/GEN
078700950626     C                   MOVEL     VIDIOE        PARIOE                         *DATI IN STATISTICA
078800000000     C*
078900970827     C* SE TIPO LANCIO = 'E' ====> FORZA RIGENERAZIONE WFILE E FILE PC
079000000000     C     VIDTLA        IFEQ      'E'
079100970827     C                   MOVEL     'S'           PARSNR                         *CREAZIONE WFILE
079200970827     C                   MOVEL     'S'           PARCFP                         *CREAZIONE FILE PC
079300000000     C                   ELSE
079400970827     C                   MOVEL     VIDSNR        PARSNR                         *CREAZIONE WFILE
079500970827     C                   MOVEL     VIDCFP        PARCFP                         *CREAZIONE FILE PC
079600000000     C                   END
079700000000     C*
079800000000     C                   MOVEL     VIDDIV        PARDIV                         *DIVISIONE
079900000000     C                   MOVE      VIDARE        PARARE                         *AREA
080000000000     C                   MOVE      VIDFLP        PARFLP                         *FILIALE PARTENZA
080100000000     C                   MOVE      VIDTFP        PARTFP                         *TERMINAL PARTENZA
080200000000     C                   MOVE      VIDFLA        PARFLA                         *FILIALE ARRIVO
080300000000     C                   MOVE      VIDTFA        PARTFA                         *TERMINAL ARRIVO
080400000000     C                   Z-ADD     VIDDTI        PARDTI                         *DATA INIZIALE
080500000000     C                   Z-ADD     VIDDTF        PARDTF                         *DATA FINALE
080600000000     C                   MOVEL     VIDTLA        PARTLA                         *TIPO LANCIO
080700000000     C*
080800000000     C                   ENDSR
080900971020     C*------------------------------------------------------------------------*
081000971020     C* FINSR - OPERAZIONI FINALI
081100971020     C*------------------------------------------------------------------------*
081200971020     C     FINSR         BEGSR
081300971020     C* FNLV55R
081400971020     C                   CLEAR                   DSLV55                         *RECUPERA TERMINALs
081500971020     C                   MOVEL     'C'           D55TLA
081600971020     C                   CALL      'FNLV55R'
081700971020     C                   PARM                    DSLV55
081800971020     C*
081900971020     C                   ENDSR
082000000000     C*------------------------------------------------------------------------*
082100000000     C* *INZSR - ROUTINE INIZIALE
082200000000     C*------------------------------------------------------------------------*
082300000000     C     *INZSR        BEGSR
082400000000     C*
082500000000     C* RICEVIMENTO PARAMETRI
082600000000     C     *ENTRY        PLIST
082700000000     C                   PARM                    KPJBA
082800000000     C*
082900000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
083000000000     C     KEYORG        KLIST
083100000000     C                   KFLD                    KORFIL
083200000000     C*
083300000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
083400000000     C     KEYTAB        KLIST
083500000000     C                   KFLD                    KTBUTE
083600000000     C                   KFLD                    KTBCOD
083700000000     C                   KFLD                    KTBKEY
083800000000     C*
083900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084000000000     C     XMSGPA        PLIST
084100000000     C                   PARM                    DSMSMS
084200000000     C                   PARM                    DSMSPG
084300000000     C                   PARM                    DSMSKE
084400000000     C                   PARM                    DSMSER
084500000000     C*
084600000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
084700000000     C                   CALL      'XKEY'
084800000000     C                   PARM                    DSKEY
084900000000     C*
085000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
085100000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
085200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
085300000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
085400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
085500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
085600000000     C*
085700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
085800000000     C*   -NOME PGM CHIAMANTE
085900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
086000000000     C                   MOVEL     'TNSS30R'     DSMSPG
086100000000     C                   MOVEL     'TNSS30R'     MSGQUE
086200000000     C                   MOVEL     *BLANKS       MSGKEY
086300000000     C                   MOVEL     *BLANKS       DSMSMS
086400000000     C*
086500000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
086600000000     C                   Z-ADD     0             FUNGRU            5 0
086700000000     C*
086800000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
086900000000     C                   EXSR      FUNGES
087000000000     C*
087100000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
087200000000     C                   MOVEL     '0'           F12L00            1
087300000000     C                   MOVEL     '0'           F03L00            1
087400000000     C                   MOVEL     '0'           FINL00            1
087500000000     C                   MOVEL     '0'           ERRL00            1
087600000000     C                   MOVEL     '0'           FLFINE            1
087700971020     C*
087800971020     C* REPERISCE LA DATA CORRENTE
087900971020     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
088000971020     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
088100971020     C                   Z-ADD     N8            G08DAT
088200971020     C                   Z-ADD     *ZEROS        G08INV
088300971020     C                   MOVEL     '0'           G08ERR
088400971020     C                   CALL      'XSRDA8'
088500971020     C                   PARM                    WLBDA8
088600971020     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
088700000000     C*
088800000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
088900000000     C                   EXSR      AZZVID
089000000000     C*
089100000000     C                   ENDSR
089200000000     C*------------------------------------------------------------------------*
089300000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
089400000000     C*------------------------------------------------------------------------*
089500000000     C     FUNGES        BEGSR
089600000000     C*
089700000000     C* PULISCO CAMPI DI LAVORO
089800000000     C                   Z-ADD     0             W                 5 0
089900000000     C*
090000000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090100000000     C* E CARICO TABELLA FUNZIONI ATTIVE
090200000000     C     1             DO        24            I
090300000000     C                   MOVEL     FUD(I)        FUDRIG
090400000000     C*
090500000000     C* PULIZIA FUNZIONI PRECEDENTI
090600000000     C                   MOVEL     *BLANKS       FUV(I)
090700000000     C*
090800000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
090900000000     C* (SALTA F24)
091000000000     C     FUDATT        IFEQ      'S'
091100000000     C     FUDFUN        ANDNE     '24'
091200000000     C                   ADD       1             W
091300000000     C                   MOVEL     FUDDES        FUV(W)
091400000000     C                   END
091500000000     C                   END                                                    DO WHILE
091600000000     C*
091700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
091800000000     C* ALTRIMENTI DISATTIVO F24
091900000000     C     1             DO        24            I
092000000000     C                   MOVEL     FUD(I)        FUDRIG
092100000000     C*
092200000000     C     FUDFUN        IFEQ      '24'
092300000000     C     W             IFGT      6
092400000000     C                   MOVEL     'S'           FUDATT
092500000000     C                   ELSE
092600000000     C                   MOVEL     *BLANKS       FUDATT
092700000000     C                   END                                                    W GT 8
092800000000     C                   MOVEL     FUDRIG        FUD(I)
092900000000     C                   END
093000000000     C                   END                                                    DO WHILE
093100000000     C*
093200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
093300000000     C     W             DIV       6             I
093400000000     C                   MVR                     Z
093500000000     C*
093600000000     C     Z             IFNE      0
093700000000     C     I             ADD       1             Z
093800000000     C                   ELSE
093900000000     C                   Z-ADD     I             Z                 5 0
094000000000     C                   END
094100000000     C*
094200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
094300000000     C                   ADD       1             FUNGRU
094400000000     C     FUNGRU        IFGT      Z
094500000000     C     FUNGRU        ORLT      1
094600000000     C                   Z-ADD     1             FUNGRU
094700000000     C                   END
094800000000     C*
094900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
095000000000     C     FUNGRU        IFEQ      1
095100000000     C                   MOVEL     FUV(1)        FUN001
095200000000     C                   MOVEL     FUV(2)        FUN002
095300000000     C                   MOVEL     FUV(3)        FUN003
095400000000     C                   MOVEL     FUV(4)        FUN004
095500000000     C                   MOVEL     FUV(5)        FUN005
095600000000     C                   MOVEL     FUV(6)        FUN006
095700000000     C                   END
095800000000     C*
095900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
096000000000     C     FUNGRU        IFEQ      2
096100000000     C                   MOVEL     FUV(7)        FUN001
096200000000     C                   MOVEL     FUV(8)        FUN002
096300000000     C                   MOVEL     FUV(9)        FUN003
096400000000     C                   MOVEL     FUV(10)       FUN004
096500000000     C                   MOVEL     FUV(11)       FUN005
096600000000     C                   MOVEL     FUV(12)       FUN006
096700000000     C                   END
096800000000     C*
096900000000     C* TERZO GRUPPO DI 6 FUNZIONI
097000000000     C     FUNGRU        IFEQ      3
097100000000     C                   MOVEL     FUV(13)       FUN001
097200000000     C                   MOVEL     FUV(14)       FUN002
097300000000     C                   MOVEL     FUV(15)       FUN003
097400000000     C                   MOVEL     FUV(16)       FUN004
097500000000     C                   MOVEL     FUV(17)       FUN005
097600000000     C                   MOVEL     FUV(18)       FUN006
097700000000     C                   END
097800000000     C*
097900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
098000000000     C     FUNGRU        IFEQ      4
098100000000     C                   MOVEL     FUV(19)       FUN001
098200000000     C                   MOVEL     FUV(20)       FUN002
098300000000     C                   MOVEL     FUV(21)       FUN003
098400000000     C                   MOVEL     FUV(22)       FUN004
098500000000     C                   MOVEL     *BLANKS       FUN005
098600000000     C                   MOVEL     *BLANKS       FUN006
098700000000     C                   END
098800000000     C*
098900000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099000000000     C                   MOVEL     *BLANKS       FUN007
099100000000     C                   MOVEL     *BLANKS       FUN008
099200000000     C*
099300000000     C     1             DO        24            I
099400000000     C                   MOVEL     FUD(I)        FUDRIG
099500000000     C     FUDFUN        IFEQ      '24'
099600000000     C     FUDATT        ANDEQ     'S'
099700000000     C                   MOVEL     FUDDES        FUN008
099800000000     C                   END
099900000000     C                   END                                                    DO
100000000000     C                   ENDSR
100100000000     C*------------------------------------------------------------------------*
100200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
100300000000     C*------------------------------------------------------------------------*
100400000000     C     FUNCON        BEGSR
100500000000     C*
100600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
100700000000     C                   EXSR      FUNVAR
100800000000     C*
100900000000     C* PULISCO CAMPI DI LAVORO
101000000000     C                   Z-ADD     0             I                 5 0
101100000000     C                   Z-ADD     0             W                 5 0
101200000000     C*
101300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
101400000000     C     FUNKEY        IFNE      *BLANKS
101500000000     C     1             DO        24            I
101600000000     C                   MOVEL     FUD(I)        FUDRIG
101700000000     C*
101800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
101900000000     C     FUNKEY        IFEQ      FUDFUN
102000000000     C     FUDATT        ANDEQ     'S'
102100000000     C                   Z-ADD     I             W
102200000000     C                   END
102300000000     C                   END                                                    DO WHILE
102400000000     C*
102500000000     C* FUNZIONE NON ATTIVA (ERRORE)
102600000000     C     W             IFEQ      0
102700000000     C*
102800000000     C* EMISSIONE MESSAGGIO DI ERRORE
102900000000     C                   MOVEL     MSG(1)        DSMSMS
103000000000     C*
103100000000     C                   END                                                    W=0
103200000000     C                   END                                                    FUNKEY NE *BLANKS
103300000000     C*
103400000000     C* FUNZIONE ATTIVA (PROCEDI ...)
103500000000     C     W             IFNE      0
103600000000     C     FUNKEY        OREQ      *BLANKS
103700000000     C*
103800000000     C     FUNKEY        IFEQ      *BLANKS
103900000000     C                   EXSR      ENTGES
104000000000     C                   END
104100000000     C     FUNKEY        IFEQ      '01'
104200000000     C                   EXSR      F01GES
104300000000     C                   END
104400000000     C     FUNKEY        IFEQ      '03'
104500000000     C                   EXSR      F03GES
104600000000     C                   END
104700000000     C     FUNKEY        IFEQ      '06'
104800000000     C                   EXSR      F06GES
104900000000     C                   END
105000000000     C     FUNKEY        IFEQ      '07'
105100000000     C                   EXSR      F07GES
105200000000     C                   END
105300000000     C     FUNKEY        IFEQ      '08'
105400000000     C                   EXSR      F08GES
105500000000     C                   END
105600000000     C     FUNKEY        IFEQ      '12'
105700000000     C                   EXSR      F12GES
105800000000     C                   END
105900000000     C     FUNKEY        IFEQ      '21'
106000000000     C                   EXSR      F21GES
106100000000     C                   END
106200000000     C     FUNKEY        IFEQ      '22'
106300000000     C                   EXSR      F22GES
106400000000     C                   END
106500000000     C     FUNKEY        IFEQ      '24'
106600000000     C                   EXSR      F24GES
106700000000     C                   END
106800000000     C     FUNKEY        IFEQ      'HP'
106900000000     C                   EXSR      HELGES
107000000000     C                   END
107100000000     C*
107200000000     C                   END
107300000000     C*
107400000000     C                   ENDSR
107500000000     C*------------------------------------------------------------------------*
107600000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
107700000000     C*------------------------------------------------------------------------*
107800000000     C     FUNVAR        BEGSR
107900000000     C*
108000000000     C* INIZIALIZZA VARIABILE -FUNKEY-
108100000000     C*
108200000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
108300000000     C     KEY           IFEQ      ENTER
108400000000     C                   MOVEL     *BLANKS       FUNKEY
108500000000     C                   END
108600000000     C     KEY           IFEQ      F1
108700000000     C                   MOVEL     '01'          FUNKEY
108800000000     C                   END
108900000000     C     KEY           IFEQ      F2
109000000000     C                   MOVEL     '02'          FUNKEY
109100000000     C                   END
109200000000     C     KEY           IFEQ      F3
109300000000     C                   MOVEL     '03'          FUNKEY
109400000000     C                   END
109500000000     C     KEY           IFEQ      F4
109600000000     C                   MOVEL     '04'          FUNKEY
109700000000     C                   END
109800000000     C     KEY           IFEQ      F5
109900000000     C                   MOVEL     '05'          FUNKEY
110000000000     C                   END
110100000000     C     KEY           IFEQ      F6
110200000000     C                   MOVEL     '06'          FUNKEY
110300000000     C                   END
110400000000     C     KEY           IFEQ      F7
110500000000     C                   MOVEL     '07'          FUNKEY
110600000000     C                   END
110700000000     C     KEY           IFEQ      F8
110800000000     C                   MOVEL     '08'          FUNKEY
110900000000     C                   END
111000000000     C     KEY           IFEQ      F9
111100000000     C                   MOVEL     '09'          FUNKEY
111200000000     C                   END
111300000000     C     KEY           IFEQ      F10
111400000000     C                   MOVEL     '10'          FUNKEY
111500000000     C                   END
111600000000     C     KEY           IFEQ      F11
111700000000     C                   MOVEL     '11'          FUNKEY
111800000000     C                   END
111900000000     C     KEY           IFEQ      F12
112000000000     C                   MOVEL     '12'          FUNKEY
112100000000     C                   END
112200000000     C     KEY           IFEQ      F13
112300000000     C                   MOVEL     '13'          FUNKEY
112400000000     C                   END
112500000000     C     KEY           IFEQ      F14
112600000000     C                   MOVEL     '14'          FUNKEY
112700000000     C                   END
112800000000     C     KEY           IFEQ      F15
112900000000     C                   MOVEL     '15'          FUNKEY
113000000000     C                   END
113100000000     C     KEY           IFEQ      F16
113200000000     C                   MOVEL     '16'          FUNKEY
113300000000     C                   END
113400000000     C     KEY           IFEQ      F17
113500000000     C                   MOVEL     '17'          FUNKEY
113600000000     C                   END
113700000000     C     KEY           IFEQ      F18
113800000000     C                   MOVEL     '18'          FUNKEY
113900000000     C                   END
114000000000     C     KEY           IFEQ      F19
114100000000     C                   MOVEL     '19'          FUNKEY
114200000000     C                   END
114300000000     C     KEY           IFEQ      F20
114400000000     C                   MOVEL     '20'          FUNKEY
114500000000     C                   END
114600000000     C     KEY           IFEQ      F21
114700000000     C                   MOVEL     '21'          FUNKEY
114800000000     C                   END
114900000000     C     KEY           IFEQ      F22
115000000000     C                   MOVEL     '22'          FUNKEY
115100000000     C                   END
115200000000     C     KEY           IFEQ      F23
115300000000     C                   MOVEL     '23'          FUNKEY
115400000000     C                   END
115500000000     C     KEY           IFEQ      F24
115600000000     C                   MOVEL     '24'          FUNKEY
115700000000     C                   END
115800000000     C     KEY           IFEQ      ROLLUP
115900000000     C                   MOVEL     'RU'          FUNKEY
116000000000     C                   END
116100000000     C     KEY           IFEQ      HELP
116200000000     C                   MOVEL     'HP'          FUNKEY
116300000000     C                   END
116400000000     C                   ENDSR
116500000000     C*------------------------------------------------------------------------*
116600000000     C* NOTE PER COMPILAZIONE SCHIERE
116700000000     C*------------------------------------------------------------------------*
116800000000     C* -MSG-
116900000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
117000000000     C* (NON TOCCARE MSG,1)
117100000000     C*
117200000000     C* -CMD-
117300000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
117400000000     C*
117500000000     C* -FUD-
117600000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
117700000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
117800000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
117900000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
118000000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
118100000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
118200000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
118300000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
118400000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
118500000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
118600000000     C*
118700000000     C*------------------------------------------------------------------------*
118800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
118900000000TNSS30R- Tasto funzionale non gestito.                                         1
119000000000TNSS30R-                                                                       2
119100000000TNSS30R- Anno non accettabile.                                                 3
119200000000TNSS30R- Mese inizio non accettabile.                                          4
119300000000TNSS30R- Mese fine non accettabile.                                            5
119400000000TNSS30R- Mese inizio maggiore mese fine.                                       6
119500000000TNSS30R- Rispondere 'S' per stampare e 'N' per non stampare.                   7
119600000000TNSS30R- Rispondere 'S' per stampa sintetica e 'D' per stampa in dettaglio.    8
119700000000TNSS30R- Rispondere 'S' per stampare e 'N' per non stampare.                   9
119800000000TNSS30R- Rispondere 'S' per stampa sintetica e 'D' per stampa in dettaglio.   10
119900000000TNSS30R- Rispondere 'S' per rigenerare il file e 'N' per non rigenerare.      11
120000000000TNSS30R- Divisione non accettabile.                                           12
120100000000TNSS30R- Area non accettabile.                                                13
120200961003TNSS30R- Punto operativo / terminal non accettabile                           14
120300000000TNSS30R- Non è un terminal.                                                   15
120400000000TNSS30R- Data non valida.                                                     16
120500000000TNSS30R- Parametro non accettabile se scelto lancio per saldi.                17
120600000000TNSS30R- Parametro non accettabile se scelto lancio estemporaneo.             18
120700000000TNSS30R- Tipo lancio non accettabile. Inserire 'S' o 'E'.                     19
120800000000TNSS30R- Solo per dettaglio: 'S' per stampare i totali,'N' per non stamparli  20
120900970826TNSS30R- Immettere 'S' per creare il file PC, 'N' per non crearlo             21
121000980403TNSS30R- Non si possono lanciare le statistiche di gruppo in SDI !!!          22
121100950626TNSS30R- 23
121200950626TNSS30R- 24
121300950626TNSS30R- 25
121400950626TNSS30R- 26
121500950626TNSS30R- 27
121600950626TNSS30R- 28
121700950626TNSS30R- Immettere il tipo di dati che si vuole in statistica                 29
121800950626TNSS30R- 30
121900000000** ======== SCHIERA: CMD  (COMANDI)                        ================
122000000000WRKSPLF
122100000000
122200000000
122300000000
122400000000
122500000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
122600000000                   -        (S/N)=N                        funz.01¬
122700000000                   -        (S/N)=N                        funz.02¬
122800000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
122900000000                   -        (S/N)=N                        funz.04¬
123000000000                   -        (S/N)=N                        funz.05¬
123100000000F06=Lancio         -        (S/N)=S                        funz.06¬
123200000000                   -        (S/N)=N                        funz.07¬
123300000000                   -        (S/N)=N                        funz.08¬
123400000000                   -        (S/N)=N                        funz.09¬
123500000000                   -        (S/N)=N                        funz.10¬
123600000000                   -        (S/N)=N                        funz.11¬
123700000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
123800000000                   -        (S/N)=N                        funz.13¬
123900000000                   -        (S/N)=N                        funz.14¬
124000000000                   -        (S/N)=N                        funz.15¬
124100000000                   -        (S/N)=N                        funz.16¬
124200000000                   -        (S/N)=N                        funz.17¬
124300000000                   -        (S/N)=N                        funz.18¬
124400000000                   -        (S/N)=N                        funz.19¬
124500000000                   -        (S/N)=N                        funz.20¬
124600000000                   -        (S/N)=N                        funz.21¬
124700000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
124800000000                   -        (S/N)=N                        funz.23¬
124900000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
