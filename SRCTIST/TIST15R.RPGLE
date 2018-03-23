000100000000     H*------------------------------------------------------------------------*
000200991019     H* STATISTICA PESI - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800991019     FTIST15D   CF   E             WORKSTN INFDS(DEVDS1)
000900991229     F                                     SFILE(ST15DMS:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI
001700000000     D*-------------------
001800000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
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
003400000000     D*-------------------
003500000000     D* RIDEFINIZIONE SCHIERA -FUD-
003600000000     D*-------------------
003700000000     D FUDDS           DS
003800000000     D*                -DESCRIZIONE PARZIALE
003900000000     D  FUDDE1                 1      1
004000000000     D*                -FUNZIONE
004100000000     D  FUDFUN                 2      3
004200000000     D*                -DESCRIZIONE PARZIALE
004300000000     D  FUDDE2                 4     19
004400000000     D*                -FILLER
004500000000     D  FUDFI0                20     34
004600000000     D*                -FLAG FUNZ.ATTIVA (S/N)
004700000000     D  FUDATT                35     35
004800000000     D*                -FILLER
004900000000     D  FUDFI1                36     80
005000000000     D*                -DESCRIZIONE COMPLETA
005100000000     D  FUDDES                 1     19
005200000000     D*                -RIDEFINIZIONE RIGA
005300000000     D  FUDRIG                 1     80
005400000000     D*-------------------
005500000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
005600000000     D*-------------------
005700000000     D DSMSG         E DS
005800000000     D*-------------------
005900020917     D* REPERIMENTO DATI UTENTE
006000000000     D*-------------------
006100020917     D TIBS34DS      E DS                                                       *Profili utente
006200020917     D DDATIUTE      E DS                                                       *Dati utente
006300020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006400970401     D*----------------
006500970401     D* DS PER TABELLA CODICI AREA
006600970401     D*----------------
006700970401     D DS05          E DS
006800970401     D*----------------
006900970401     D* DS PER TABELLA CODICI DISTRETTO
007000970401     D*----------------
007100970401     D DS17          E DS
007200000000     D*-------------------
007300980403     D* DS DATI STATISTICHE GRUPPO
007400000000     D*-------------------
007500980403     D DS5S          E DS
007600991019     D*-------------------
007700991019     D* DS PER REPERIMENTO PARAMETRI DI DFT
007800991019     D*-------------------
007900991019     D DSTP          E DS
008000000000     D*-------------------
008100000000     D* DS PARAMETRI DI LANCIO PGM
008200000000     D*-------------------
008300950703     D PARAM           DS
008400000000     D  PARTLA                 1      1
008500000000     D  PARANN                 2      5  0
008600000000     D  PARMMI                 6      7  0
008700000000     D  PARMMF                 8      9  0
008800000000     D  PARSNR                10     10
008900991025     D  PARPEI                27     31  2
009000000000     D  PARDIV                32     32
009100000000     D  PARARE                33     35  0
009200000000     D  PARFIL                36     38  0
009300000000     D  PARDET                39     39
009400000000     D  PARTOT                40     40
009500991026     D  PARRG1                41     44  1
009600991026     D  PARRG2                45     48  1
009700991026     D  PARTAR                49     53  3
009800991026     D  PARPES                54     58  2
009900991229     D  PARMIN                59     63  3
010000000706     D  PARPOS                64     64
010100011112     D  PARNEG                65     65
010200050105     D  PARSAL                66     66
010300000000     D*-------------------
010400000000     D* DS PER GESTIONE TASTI FUNZIONALI
010500000000     D*-------------------
010600000000     D DSKEY         E DS
010700000000     D DEVDS1          DS
010800000000     D  KEY                  369    369
010900000000     D*-------------------
011000000000     D* ARCHITETTURA
011100000000     D*-------------------
011200000000     D KPJBA         E DS
011300000000     C*------------------------------------------------------------------------*
011400000000     C* MAIN LINE
011500000000     C*------------------------------------------------------------------------*
011600000000     C*
011700000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
011800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
011900000000     C*
012000000000     C* CICLO FINO A CHE:
012100000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012200000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012300000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012400000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
012500000000     C     F12L00        OREQ      '1'
012600000000     C     FLFINE        OREQ      '1'
012700000000     C*
012800000000     C* EMETTE FORMATO VIDEO
012900000000     C                   EXSR      BUTFMR
013000000000     C*
013100000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
013200000000     C                   EXSR      FUNCON
013300000000     C*
013400000000     C                   END                                                    <-- 2 --
013500000000     C*
013600000000     C                   END                                                    <-- 1 --
013700000000     C*
013800000000     C                   SETON                                        LR
013900000000     C*------------------------------------------------------------------------*
014000000000     C* EMISSIONE FORMATO A VIDEO
014100000000     C*------------------------------------------------------------------------*
014200000000     C     BUTFMR        BEGSR
014300000000     C*
014400000000     C* WRITE RECORD TESTATA
014500991229     C                   WRITE     ST15D01
014600000000     C*
014700000000     C* VISUALIZZAZIONE MESSAGGIO
014800000000     C                   EXSR      SNDMSG
014900000000     C*
015000000000     C* WRITE RECORD FUNZIONI
015100991229     C                   WRITE     ST15D02
015200991019     C*
015300991019     C* EXFMT RECORD DETTAGLIO
015400991229     C                   EXFMT     ST15D03
015500991019     C*
015600000000     C* AZZERO FLAG DI ERRORE LIVELLO
015700000000     C                   MOVEL     '0'           ERRL00
015800000000     C*
015900000000     C* AZZERO FLAG DI ERRORE
016000991025     C                   SETOFF                                         7071
016100000000     C                   SETOFF                                       727377
016200050105     C                   SETOFF                                       76
016300000000     C                   SETOFF                                       787983
016400000000     C                   SETOFF                                       848586
016500950703     C                   SETOFF                                       818082
016600000000     C*
016700000000     C                   ENDSR
016800000000     C*------------------------------------------------------------------------*
016900000000     C* SNDMSG - MANDA MESSAGGIO
017000000000     C*------------------------------------------------------------------------*
017100000000     C     SNDMSG        BEGSR
017200000000     C*
017300000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
017400000000     C                   MOVEL     '1'           *IN40
017500991229     C                   WRITE     ST15DMC
017600000000     C                   MOVEL     '0'           *IN40
017700000000     C*
017800000000     C* VISUALIZZAZIONE MESSAGGIO
017900000000     C                   Z-ADD     0             MSGULT            4 0
018000000000     C                   CALL      'XMSG'        XMSGPA
018100000000     C     DSMSER        IFEQ      0
018200000000     C                   MOVEL     DSMSKE        MSGKEY
018300000000     C                   END
018400000000     C                   Z-ADD     1             MSGULT
018500991229     C                   WRITE     ST15DMS                                      SFL MESS
018600991229     C                   WRITE     ST15DMC                                      CONTR MESS
018700000000     C*
018800000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
018900000000     C                   MOVEL     *BLANKS       DSMSMS
019000000000     C*
019100000000     C                   ENDSR
019200000000     C*------------------------------------------------------------------------*
019300000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
019400000000     C*------------------------------------------------------------------------*
019500000000     C     ENTGES        BEGSR
019600000000     C*
019700000000     C* CONTROLLA I CAMPI IMMESSI
019800000000     C                   EXSR      DECODI
019900000000     C*
020000000000     C                   ENDSR
020100000000     C*------------------------------------------------------------------------*
020200000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
020300000000     C*------------------------------------------------------------------------*
020400000000     C     F01GES        BEGSR
020500000000     C                   ENDSR
020600000000     C*------------------------------------------------------------------------*
020700000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
020800000000     C*------------------------------------------------------------------------*
020900000000     C     F03GES        BEGSR
021000000000     C*
021100000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
021200000000     C                   MOVEL     '1'           F03L00
021300000000     C*
021400000000     C                   ENDSR
021500000000     C*------------------------------------------------------------------------*
021600000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
021700000000     C*------------------------------------------------------------------------*
021800991020     C     F06GES        BEGSR
021900000000     C*
022000991105     C                   EXSR      DECODI
022100991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
022200991105     C     ERRL00        IFEQ      '0'
022300991105     C*
022400000000     C* LANCIO IL PROGRAMMA DI STAMPA
022500991022     C                   MOVEL     'ST16'        KCOAZ
022600000000     C                   EXSR      VIDPAR
022700950703     C                   MOVEL     PARAM         KPJBU
022800000517     C                   CALL      'BCH10'
022900000517 MOM C*                    CALL 'TIST16C'
023000991021     C                   PARM                    KPJBA
023100000000     C*
023200991123     C* RIMETTE FILE DA RIGENERARE = 'N' PER SICUREZZA
023300991123     C                   MOVEL     'N'           VIDSNR
023400991123     C*
023500991105     C                   ENDIF
023600991105     C*
023700000000     C                   ENDSR
023800000000     C*------------------------------------------------------------------------*
023900000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
024000000000     C*------------------------------------------------------------------------*
024100000000     C     F07GES        BEGSR
024200000000     C                   ENDSR
024300000000     C*------------------------------------------------------------------------*
024400000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
024500000000     C*------------------------------------------------------------------------*
024600000000     C     F08GES        BEGSR
024700000000     C                   ENDSR
024800000000     C*------------------------------------------------------------------------*
024900000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
025000000000     C*------------------------------------------------------------------------*
025100000000     C     F12GES        BEGSR
025200000000     C*
025300000000     C* IMPOSTO FLAG DI LIVELLO
025400000000     C                   MOVEL     '1'           F12L00
025500000000     C*
025600000000     C                   ENDSR
025700000000     C*------------------------------------------------------------------------*
025800000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
025900000000     C*------------------------------------------------------------------------*
026000000000     C     F21GES        BEGSR
026100000000     C                   ENDSR
026200000000     C*------------------------------------------------------------------------*
026300000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
026400000000     C*------------------------------------------------------------------------*
026500000000     C     F22GES        BEGSR
026600000000     C*
026700991020     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
026800000000     C                   Z-ADD     80            LENGH            15 5
026900000000     C                   CALL      'QCAEXEC'
027000000000     C                   PARM                    CMD(1)
027100000000     C                   PARM                    LENGH
027200000000     C*
027300000000     C                   ENDSR
027400000000     C*------------------------------------------------------------------------*
027500991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
027600000000     C*------------------------------------------------------------------------*
027700000000     C     F24GES        BEGSR
027800000000     C*
027900000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
028000000000     C                   EXSR      FUNGES
028100000000     C*
028200000000     C                   ENDSR
028300000000     C*------------------------------------------------------------------------*
028400000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
028500000000     C*------------------------------------------------------------------------*
028600000000     C     HELGES        BEGSR
028700000000     C                   ENDSR
028800991020     C*------------------------------------------------------------------------*
028900991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
029000000000     C*------------------------------------------------------------------------*
029100000000     C     DECODI        BEGSR
029200000000     C*
029300000000     C* CONTROLLO ANNO
029400991020     C     ERRL00        IFEQ      '0'
029500991020     C     VIDANN        IFEQ      *ZEROS
029600000000     C                   MOVEL     '1'           ERRL00
029700000000     C                   MOVEL     MSG(3)        DSMSMS
029800000000     C                   SETON                                        70
029900991020     C                   ELSE
030000991020     C     VIDANN        IFLT      1900
030100000000     C                   MOVEL     '1'           ERRL00
030200000000     C                   MOVEL     MSG(3)        DSMSMS
030300000000     C                   SETON                                        70
030400991020     C                   END
030500991020     C                   END
030600991020     C                   END
030700000000     C*
030800000000     C* CONTROLLO MESE INIZIO / FINE
030900000000     C     ERRL00        IFEQ      '0'
031000000000     C     VIDMMI        IFEQ      *ZEROS                                       *INIZIO
031100000000     C     VIDMMI        ORGT      12
031200000000     C                   MOVEL     '1'           ERRL00
031300000000     C                   MOVEL     MSG(4)        DSMSMS
031400000000     C                   SETON                                        71
031500000000     C                   END
031600000000     C                   END
031700000000     C     ERRL00        IFEQ      '0'
031800000000     C     VIDMMF        IFEQ      *ZEROS                                       *FINE
031900000000     C                   Z-ADD     VIDMMI        VIDMMF                         *LO IMPOSTO COME INI
032000000000     C                   ELSE
032100000000     C     VIDMMF        IFGT      12
032200000000     C                   MOVEL     '1'           ERRL00
032300000000     C                   MOVEL     MSG(5)        DSMSMS
032400000000     C                   SETON                                        72
032500000000     C                   END
032600000000     C                   END
032700000000     C                   END
032800000000     C     ERRL00        IFEQ      '0'                                          *INIZIO MAGGIORE FIN
032900000000     C     VIDMMF        IFNE      *ZEROS
033000000000     C     VIDMMI        IFGT      VIDMMF
033100000000     C                   MOVEL     '1'           ERRL00
033200000000     C                   MOVEL     MSG(6)        DSMSMS
033300000000     C                   SETON                                        7172
033400000000     C                   END
033500000000     C                   END
033600000000     C                   END
033700000000     C*
033800000000     C* CONTROLLO FLAG FILE DA RIGENERARE
033900000000     C     ERRL00        IFEQ      '0'
034000000000     C     VIDSNR        IFEQ      'S'
034100000000     C     VIDSNR        OREQ      'N'
034200000000     C                   ELSE
034300000000     C                   MOVEL     '1'           ERRL00
034400000000     C                   MOVEL     MSG(11)       DSMSMS
034500000000     C                   SETON                                        77
034600000000     C                   END
034700000000     C                   END
034800050105     C*
034900050105     C* CONTROLLO FLAG FILE DA RIGENERARE saldi x cliente
035000050105     C     ERRL00        IFEQ      '0'
035100050105     C     VIDSal        IFEQ      'S'
035200050105     C     VIDSal        OREQ      'N'
035300050105     C                   ELSE
035400050105     C                   MOVEL     '1'           ERRL00
035500050105     C                   MOVEL     MSG(11)       DSMSMS
035600050105     C                   SETON                                        76
035700050105     C                   END
035800050105     C                   END
035900000000     C*
036000991020     C* CONTROLLO PERCENTUALE
036100000000     C     ERRL00        IFEQ      '0'
036200991104     C     VIDPEI        IFLT      *ZEROS
036300991104     C     VIDPEI        ORGT      100
036400000000     C                   MOVEL     '1'           ERRL00
036500991104     C                   MOVEL     MSG(15)       DSMSMS
036600000000     C                   SETON                                        86
036700000000     C                   END
036800000000     C                   END
036900000000     C*
037000991025     C     ERRL00        IFEQ      '0'
037100991104     C     VIDPES        IFLT      *ZEROS
037200991104     C     VIDPES        ORGT      100
037300991025     C                   MOVEL     '1'           ERRL00
037400991104     C                   MOVEL     MSG(15)       DSMSMS
037500991025     C                   SETON                                        87
037600991025     C                   END
037700991025     C                   END
037800991020     C*
037900991020     C* CONTROLLO RANGE INIZ/FIN PERC. RAPPORTO PESI X ELAB.
038000991104     C     ERRL00        IFEQ      '0'
038100991104     C     VIDRG1        IFGT      VIDRG2
038200991104     C                   MOVEL     '1'           ERRL00
038300991104     C                   MOVEL     MSG(7)        DSMSMS
038400991104     C                   SETON                                        8485
038500991104     C                   END
038600991104     C                   END
038700991028     C*
038800991028     C* CONTROLLO FLAG DI STAMPA TOTALI SOLO SE SCELTO DETTAGLIO
038900991104     C     ERRL00        IFEQ      '0'
039000991104     C     VICDET        IFEQ      'S'
039100991104     C                   MOVEL     'N'           VICTOT
039200991104     C                   END
039300991104     C                   END
039400000000     C*
039500000000     C* CONTROLLO LA DIVISIONE
039600000000     C                   MOVEL     *BLANKS       VDDDIV
039700000000     C     ERRL00        IFEQ      '0'
039800000000     C     VIDDIV        IFNE      *BLANKS
039900000000     C                   EXSR      CHKDIV
040000000000     C     ERRL00        IFEQ      '1'
040100000000     C                   MOVEL     '1'           ERRL00
040200000000     C                   MOVEL     MSG(12)       DSMSMS
040300000000     C                   SETON                                        78
040400000000     C                   END
040500000000     C                   END
040600000000     C                   END
040700000000     C*
040800000000     C* CONTROLLO L'AREA
040900000000     C                   MOVEL     *BLANKS       VDDARE
041000000000     C     ERRL00        IFEQ      '0'
041100000000     C     VIDARE        IFNE      *BLANKS
041200000000     C                   EXSR      CHKARE
041300000000     C     ERRL00        IFEQ      '1'
041400000000     C                   MOVEL     '1'           ERRL00
041500000000     C                   MOVEL     MSG(13)       DSMSMS
041600000000     C                   SETON                                        79
041700000000     C                   END
041800000000     C                   END
041900000000     C                   END
042000000000     C*
042100000000     C* CONTROLLO LA FILIALE
042200000000     C                   MOVEL     *BLANKS       VDDFLP
042300000000     C     ERRL00        IFEQ      '0'
042400000000     C     VIDFLP        IFNE      *BLANKS
042500000000     C                   MOVE      VIDFLP        WLIN
042600000000     C                   EXSR      CHKLIN
042700000000     C     ERRL00        IFEQ      '0'
042800000000     C                   MOVEL     WLIN          VIDFLP
042900000000     C                   MOVEL     WDLIN         VDDFLP
043000000000     C                   ELSE
043100000000     C                   SETON                                        80
043200000000     C                   END
043300000000     C                   END
043400000000     C                   END
043500000000     C*
043600000000     C                   ENDSR
043700000000     C*------------------------------------------------------------------------*
043800000000     C*  CHKDIV - CONTROLLA LA DIVISIONE
043900000000     C*------------------------------------------------------------------------*
044000000000     C     CHKDIV        BEGSR
044100000000     C*
044200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
044300000000     C     VIDDIV        IFEQ      '?'                                          --- 1 -->
044400000000     C                   MOVEL     CODUT         §KUT              1 0
044500000000     C                   MOVEL     '17'          §COD              2
044600000000     C                   MOVE      *BLANKS       §DES1
044700000000     C                   CALL      'X§TABER'
044800000000     C                   PARM                    §KUT
044900000000     C                   PARM                    §COD
045000000000     C                   PARM                    §KEY              8
045100970401     C                   PARM                    §DES             25
045200000000     C                   MOVEL     §KEY          VIDDIV
045300000000     C                   MOVEL     §DES          VDDDIV
045400000000     C*
045500000000     C                   ELSE                                                   <-- 1 -->
045600000000     C*
045700000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
045800000000     C                   MOVEL     CODUT         KTBUTE
045900000000     C                   MOVE      '17'          KTBCOD
046000000000     C                   MOVE      *BLANKS       KTBKEY
046100000000     C                   MOVEL     VIDDIV        KTBKEY
046200000000     C     KEYTAB        CHAIN     TABEL                              99
046300000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
046400000000     C     TBLFLG        OREQ      '*'
046500000000     C                   MOVEL     '1'           ERRL00
046600000000     C                   MOVEL     *ALL'*'       VDDDIV
046700000000     C                   ELSE                                                   <-- 2 -->
046800970401     C                   MOVEL     TBLUNI        DS17
046900970401     C                   MOVEL     §17DES        VDDDIV
047000000000     C                   END                                                    <-- 2 ---
047100000000     C*
047200000000     C                   END                                                    <-- 1 ---
047300000000     C*
047400000000     C                   ENDSR
047500000000     C*------------------------------------------------------------------------*
047600000000     C*  CHKARE - CONTROLLA L'AREA
047700000000     C*------------------------------------------------------------------------*
047800000000     C     CHKARE        BEGSR
047900000000     C*
048000000000     C* CONTROLLO SE E' STATO INSERITO '?'
048100000000     C     '?'           SCAN      VIDARE        A                 1 0    99
048200000000     C*
048300000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
048400000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
048500000000     C                   MOVEL     CODUT         §KUT              1 0
048600000000     C                   MOVEL     '05'          §COD              2
048700000000     C                   MOVE      *BLANKS       §DES1
048800000000     C                   CALL      'X§TABER'
048900000000     C                   PARM                    §KUT
049000000000     C                   PARM                    §COD
049100000000     C                   PARM                    §KEY              8
049200970401     C                   PARM                    §DES             25
049300000000     C                   MOVEL     §KEY          VIDARE
049400000000     C                   MOVEL     §DES          VDDARE
049500000000     C*
049600000000     C                   ELSE                                                   <-- 1 -->
049700000000     C*
049800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
049900000000     C                   MOVEL     CODUT         KTBUTE
050000000000     C                   MOVE      '05'          KTBCOD
050100000000     C                   MOVE      *BLANKS       KTBKEY
050200000000     C                   MOVEL     VIDARE        KTBKEY
050300000000     C     KEYTAB        CHAIN     TABEL                              99
050400000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
050500000000     C     TBLFLG        OREQ      '*'
050600000000     C                   MOVEL     '1'           ERRL00
050700000000     C                   MOVEL     *ALL'*'       VDDARE
050800000000     C                   ELSE                                                   <-- 2 -->
050900970401     C                   MOVEL     TBLUNI        DS05
051000970401     C                   MOVEL     §05DES        VDDARE
051100000000     C                   END                                                    <-- 2 ---
051200000000     C*
051300000000     C                   END                                                    <-- 1 ---
051400000000     C*
051500000000     C                   ENDSR
051600000000     C*------------------------------------------------------------------------*
051700000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
051800000000     C*------------------------------------------------------------------------*
051900000000     C     CHKLIN        BEGSR
052000000000     C*
052100000000     C* CONTROLLO SE E' STATO INSERITO '?'
052200000000     C     '?'           SCAN      WLIN          A                 1 0    99
052300000000     C*
052400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
052500000000IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
052600000000     C                   MOVEL     *BLANKS       §COD1             3
052700000000     C                   MOVE      *BLANKS       §DES1
052800971203     C                   MOVE      *BLANKS       §PRA
052900971203     C                   CALL      'TNSD24R'
053000000000     C                   PARM                    §COD1
053100000000     C                   PARM                    §TIP              1
053200000000     C                   PARM                    §DES1            25
053300971203     C                   PARM                    §PRA              3
053400000000     C                   MOVEL     §COD1         WLIN              3
053500000000     C                   MOVEL     §DES1         WDLIN            20
053600000000     C*
053700000000X   1C                   ELSE
053800000000     C*
053900000000     C* CARATTERI ALFABETICI NON VALIDI
054000000000IF  2C     WLIN          IFLT      *ZEROS
054100000000     C                   MOVEL     '1'           ERRL00
054200000000     C                   MOVEL     MSG(14)       DSMSMS
054300000000     C                   MOVEL     *ALL'*'       WDLIN
054400000000X   2C                   ELSE
054500000000     C*
054600000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
054700000000     C                   MOVE      WLIN          KORFIL
054800000000     C     KEYORG        CHAIN     AZORG                              99
054900000000IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
055000000000     C                   MOVEL     ORGDES        WDLIN
055100000000X   3C                   ELSE                                                   <-- 2 -->
055200000000     C                   MOVEL     '1'           ERRL00
055300000000     C                   MOVEL     MSG(14)       DSMSMS
055400000000     C                   MOVEL     *ALL'*'       WDLIN
055500000000E   3C                   END                                                    <-- 2 ---
055600000000IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
055700000000     C                   MOVEL     '1'           ERRL00
055800000000     C                   MOVEL     MSG(14)       DSMSMS
055900000000     C                   MOVEL     *ALL'*'       WDLIN
056000000000E   3C                   END                                                    <-- 2 ---
056100000000E   2C                   END                                                    <-- 2 ---
056200000000E   1C                   ENDIF
056300000000     C*
056400000000     C                   ENDSR
056500000000     C*------------------------------------------------------------------------*
056600000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
056700000000     C*------------------------------------------------------------------------*
056800000000     C     AZZVID        BEGSR
056900000000     C*
057000000000     C                   Z-ADD     *ZEROS        VIDANN
057100000000     C                   Z-ADD     *ZEROS        VIDMMI
057200000000     C                   Z-ADD     *ZEROS        VIDMMF
057300000000     C                   MOVEL     'N'           VIDSNR
057400050105     C                   MOVEL     'N'           VIDSAL
057500950704     C                   MOVEL     'S'           VICDET
057600000000     C                   MOVEL     'N'           VICTOT
057700150615     c* richiesta poste non serve più tolto dal video
057800150615     C                   MOVEL     'N'           VICPOS            1
057900011112     C                   MOVEL     'N'           VICNEG
058000991020     C*-------                                                         --------*
058100991020     C* REP. VALORI LIMITE TABELLATI PER DEFAULT
058200991020     C*-------                                                         --------*
058300991020     C                   MOVEL     CODUT         KTBUTE
058400991020     C                   MOVEL     'TP'          KTBCOD
058500991020     C     KE2TAB        CHAIN     TABEL00F                           99
058600991020     C     *IN99         IFEQ      *ON
058700991020     C     TBLFLG        OREQ      '*'
058800991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI GENERICI=1 e =100
058900991029     C                   Z-ADD     10            VIDPEI
059000991029     C                   Z-ADD     10            VIDPES
059100991029     C                   Z-ADD     -999          VIDRG1
059200991029     C                   Z-ADD     999           VIDRG2
059300991020     C                   ELSE                                                   <-- 2 -->
059400991020     C                   MOVEL     TBLUNI        DSTP                                       NTO BOLL
059500991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI TABELLATI
059600991020     C                   Z-ADD     §TPDPD        VIDRG1
059700991026     C     VIDRG1        MULT      -1            VIDRG1
059800991026     C                   Z-ADD     §TPDPA        VIDRG2
059900991025     C                   Z-ADD     §TPCOI        VIDPEI
060000991025     C                   Z-ADD     §TPCOS        VIDPES
060100991229     C                   Z-ADD     §TPMIN        VIDMIN
060200991020     C                   Z-ADD     §TPTAR        PARTAR
060300000000     C                   END                                                    <-- 2 ---
060400000000     C*
060500000000     C                   ENDSR
060600000000     C*------------------------------------------------------------------------*
060700991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
060800000000     C*------------------------------------------------------------------------*
060900000000     C     VIDPAR        BEGSR
061000000000     C*
061100000000     C                   Z-ADD     VIDANN        PARANN
061200000000     C                   Z-ADD     VIDMMI        PARMMI
061300000000     C                   Z-ADD     VIDMMF        PARMMF
061400000000     C                   MOVEL     VIDSNR        PARSNR
061500050105     C                   MOVEL     VIDsal        PARsal
061600000000     C                   MOVEL     VIDDIV        PARDIV
061700000000     C                   MOVE      VIDARE        PARARE
061800000000     C                   MOVE      VIDFLP        PARFIL
061900991025     C                   Z-ADD     VIDPEI        PARPEI
062000991025     C                   Z-ADD     VIDPES        PARPES
062100000000     C                   MOVEL     VICDET        PARDET
062200000000     C                   MOVEL     VICTOT        PARTOT
062300991020     C                   Z-ADD     VIDRG1        PARRG1
062400991020     C                   Z-ADD     VIDRG2        PARRG2
062500991229     C                   Z-ADD     VIDMIN        PARMIN
062600000706     C                   MOVEL     VICPOS        PARPOS
062700011112     C                   MOVEL     VICNEG        PARNEG
062800991214     C*
062900000000     C                   ENDSR
063000020917     C*--------------------------------------------------------------------------------------------*
063100020917     C* REPERISCE I DATI UTENTE
063200020917     C*--------------------------------------------------------------------------------------------*
063300020917     C     REPDATIUTE    BEGSR
063400020917     C*
063500020917     C* INIZIALIZZA VARIABILI DI WRK
063600020917     C                   CLEAR                   TIBS34DS
063700020917     C                   CLEAR                   AZUTEDS
063800020917     C                   CLEAR                   DDATIUTE
063900020917     C*
064000020917     C     *DTAARA       DEFINE    §azute        azuteds
064100020917     C     *DTAARA       DEFINE    §datiute      ddatiute
064200020917     C                   IN(E)     *DTAARA
064300020917if  1C                   IF        %Error
064400020917     c                   EVAL      I34Tla = 'L'
064500020917     C                   CALL      'TIBS34R'
064600020917     C                   PARM                    Tibs34Ds
064700020917     C                   IN        *DTAARA
064800020917e   1C                   ENDIF
064900020917     C*
065000020917     C                   ENDSR
065100000000     C*------------------------------------------------------------------------*
065200980403     C* *INZSR - OPERAZIONI INIZIALI
065300000000     C*------------------------------------------------------------------------*
065400980403     C     *INZSR        BEGSR
065500000000     C*
065600000000     C* RICEVIMENTO PARAMETRI
065700000000     C     *ENTRY        PLIST
065800000000     C                   PARM                    KPJBA
065900000000     C*
066000000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
066100000000     C     KEYORG        KLIST
066200000000     C                   KFLD                    KORFIL
066300000000     C*
066400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
066500000000     C     KEYTAB        KLIST
066600000000     C                   KFLD                    KTBUTE
066700000000     C                   KFLD                    KTBCOD
066800000000     C                   KFLD                    KTBKEY
066900980403     C*
067000980403     C* CHIAVE LETTURA TABEL00F - PARZIALE
067100980403     C     KE2TAB        KLIST
067200980403     C                   KFLD                    KTBUTE
067300980403     C                   KFLD                    KTBCOD
067400000000     C*
067500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
067600000000     C     XMSGPA        PLIST
067700000000     C                   PARM                    DSMSMS
067800000000     C                   PARM                    DSMSPG
067900000000     C                   PARM                    DSMSKE
068000000000     C                   PARM                    DSMSER
068100000000     C*
068200000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
068300000000     C                   CALL      'XKEY'
068400000000     C                   PARM                    DSKEY
068500000000     C*
068600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
068700000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
068800000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
068900000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
069000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
069100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
069200000000     C*
069300000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
069400000000     C*   -NOME PGM CHIAMANTE
069500000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
069600991019     C                   MOVEL     'TIST15R'     DSMSPG
069700991019     C                   MOVEL     'TIST15R'     MSGQUE
069800000000     C                   MOVEL     *BLANKS       MSGKEY
069900000000     C                   MOVEL     *BLANKS       DSMSMS
070000000000     C*
070100000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
070200000000     C                   Z-ADD     0             FUNGRU            5 0
070300000000     C*
070400000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
070500000000     C                   EXSR      FUNGES
070600000000     C*
070700000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
070800000000     C                   MOVEL     '0'           F12L00            1
070900000000     C                   MOVEL     '0'           F03L00            1
071000000000     C                   MOVEL     '0'           FINL00            1
071100000000     C                   MOVEL     '0'           ERRL00            1
071200000000     C                   MOVEL     '0'           FLFINE            1
071300000000     C*
071400000000     C* LANCIO ROUTINE PER VERIFICARE SE
071500000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
071600000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
071700020917     C                   EXSR      REPDATIUTE
071800020917     C                   Z-ADD     1             CODUT             1 0
071900000000     C*
072000000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
072100000000     C                   EXSR      AZZVID
072200150409     c* se profilo <> da EDP proteggo i campi per la rigenerazione dei file
072300150409     c                   setoff                                       75
072400150409     c                   if        %subst(knmus:1:3)<>'EDP'
072500150409     c                   seton                                        75
072600150409     c                   endif
072700000000     C*
072800000000     C                   ENDSR
072900000000     C*------------------------------------------------------------------------*
073000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
073100000000     C*------------------------------------------------------------------------*
073200000000     C     FUNGES        BEGSR
073300000000     C*
073400000000     C* PULISCO CAMPI DI LAVORO
073500000000     C                   Z-ADD     0             W                 5 0
073600000000     C*
073700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
073800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
073900000000     C     1             DO        24            I
074000000000     C                   MOVEL     FUD(I)        FUDRIG
074100000000     C*
074200000000     C* PULIZIA FUNZIONI PRECEDENTI
074300000000     C                   MOVEL     *BLANKS       FUV(I)
074400000000     C*
074500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
074600000000     C* (SALTA F24)
074700000000     C     FUDATT        IFEQ      'S'
074800000000     C     FUDFUN        ANDNE     '24'
074900000000     C                   ADD       1             W
075000000000     C                   MOVEL     FUDDES        FUV(W)
075100000000     C                   END
075200000000     C                   END                                                    DO WHILE
075300000000     C*
075400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
075500000000     C* ALTRIMENTI DISATTIVO F24
075600000000     C     1             DO        24            I
075700000000     C                   MOVEL     FUD(I)        FUDRIG
075800000000     C*
075900000000     C     FUDFUN        IFEQ      '24'
076000000000     C     W             IFGT      6
076100000000     C                   MOVEL     'S'           FUDATT
076200000000     C                   ELSE
076300000000     C                   MOVEL     *BLANKS       FUDATT
076400000000     C                   END                                                    W GT 8
076500000000     C                   MOVEL     FUDRIG        FUD(I)
076600000000     C                   END
076700000000     C                   END                                                    DO WHILE
076800000000     C*
076900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
077000000000     C     W             DIV       6             I
077100000000     C                   MVR                     Z
077200000000     C*
077300000000     C     Z             IFNE      0
077400000000     C     I             ADD       1             Z
077500000000     C                   ELSE
077600000000     C                   Z-ADD     I             Z                 5 0
077700000000     C                   END
077800000000     C*
077900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
078000000000     C                   ADD       1             FUNGRU
078100000000     C     FUNGRU        IFGT      Z
078200000000     C     FUNGRU        ORLT      1
078300000000     C                   Z-ADD     1             FUNGRU
078400000000     C                   END
078500000000     C*
078600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
078700000000     C     FUNGRU        IFEQ      1
078800000000     C                   MOVEL     FUV(1)        FUN001
078900000000     C                   MOVEL     FUV(2)        FUN002
079000000000     C                   MOVEL     FUV(3)        FUN003
079100000000     C                   MOVEL     FUV(4)        FUN004
079200000000     C                   MOVEL     FUV(5)        FUN005
079300000000     C                   MOVEL     FUV(6)        FUN006
079400000000     C                   END
079500000000     C*
079600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
079700000000     C     FUNGRU        IFEQ      2
079800000000     C                   MOVEL     FUV(7)        FUN001
079900000000     C                   MOVEL     FUV(8)        FUN002
080000000000     C                   MOVEL     FUV(9)        FUN003
080100000000     C                   MOVEL     FUV(10)       FUN004
080200000000     C                   MOVEL     FUV(11)       FUN005
080300000000     C                   MOVEL     FUV(12)       FUN006
080400000000     C                   END
080500000000     C*
080600000000     C* TERZO GRUPPO DI 6 FUNZIONI
080700000000     C     FUNGRU        IFEQ      3
080800000000     C                   MOVEL     FUV(13)       FUN001
080900000000     C                   MOVEL     FUV(14)       FUN002
081000000000     C                   MOVEL     FUV(15)       FUN003
081100000000     C                   MOVEL     FUV(16)       FUN004
081200000000     C                   MOVEL     FUV(17)       FUN005
081300000000     C                   MOVEL     FUV(18)       FUN006
081400000000     C                   END
081500000000     C*
081600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
081700000000     C     FUNGRU        IFEQ      4
081800000000     C                   MOVEL     FUV(19)       FUN001
081900000000     C                   MOVEL     FUV(20)       FUN002
082000000000     C                   MOVEL     FUV(21)       FUN003
082100000000     C                   MOVEL     FUV(22)       FUN004
082200000000     C                   MOVEL     *BLANKS       FUN005
082300000000     C                   MOVEL     *BLANKS       FUN006
082400000000     C                   END
082500000000     C*
082600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
082700000000     C                   MOVEL     *BLANKS       FUN007
082800000000     C                   MOVEL     *BLANKS       FUN008
082900000000     C*
083000000000     C     1             DO        24            I
083100000000     C                   MOVEL     FUD(I)        FUDRIG
083200000000     C     FUDFUN        IFEQ      '24'
083300000000     C     FUDATT        ANDEQ     'S'
083400000000     C                   MOVEL     FUDDES        FUN008
083500000000     C                   END
083600000000     C                   END                                                    DO
083700000000     C                   ENDSR
083800000000     C*------------------------------------------------------------------------*
083900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
084000000000     C*------------------------------------------------------------------------*
084100000000     C     FUNCON        BEGSR
084200000000     C*
084300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
084400000000     C                   EXSR      FUNVAR
084500000000     C*
084600000000     C* PULISCO CAMPI DI LAVORO
084700000000     C                   Z-ADD     0             I                 5 0
084800000000     C                   Z-ADD     0             W                 5 0
084900000000     C*
085000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
085100000000     C     FUNKEY        IFNE      *BLANKS
085200000000     C     1             DO        24            I
085300000000     C                   MOVEL     FUD(I)        FUDRIG
085400000000     C*
085500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
085600000000     C     FUNKEY        IFEQ      FUDFUN
085700000000     C     FUDATT        ANDEQ     'S'
085800000000     C                   Z-ADD     I             W
085900000000     C                   END
086000000000     C                   END                                                    DO WHILE
086100000000     C*
086200000000     C* FUNZIONE NON ATTIVA (ERRORE)
086300000000     C     W             IFEQ      0
086400000000     C*
086500000000     C* EMISSIONE MESSAGGIO DI ERRORE
086600000000     C                   MOVEL     MSG(1)        DSMSMS
086700000000     C*
086800000000     C                   END                                                    W=0
086900000000     C                   END                                                    FUNKEY NE *BLANKS
087000000000     C*
087100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
087200000000     C     W             IFNE      0
087300000000     C     FUNKEY        OREQ      *BLANKS
087400000000     C*
087500000000     C     FUNKEY        IFEQ      *BLANKS
087600000000     C                   EXSR      ENTGES
087700000000     C                   END
087800000000     C     FUNKEY        IFEQ      '01'
087900000000     C                   EXSR      F01GES
088000000000     C                   END
088100000000     C     FUNKEY        IFEQ      '03'
088200000000     C                   EXSR      F03GES
088300000000     C                   END
088400000000     C     FUNKEY        IFEQ      '06'
088500000000     C                   EXSR      F06GES
088600000000     C                   END
088700000000     C     FUNKEY        IFEQ      '07'
088800000000     C                   EXSR      F07GES
088900000000     C                   END
089000000000     C     FUNKEY        IFEQ      '08'
089100000000     C                   EXSR      F08GES
089200000000     C                   END
089300000000     C     FUNKEY        IFEQ      '12'
089400000000     C                   EXSR      F12GES
089500000000     C                   END
089600000000     C     FUNKEY        IFEQ      '21'
089700000000     C                   EXSR      F21GES
089800000000     C                   END
089900000000     C     FUNKEY        IFEQ      '22'
090000000000     C                   EXSR      F22GES
090100000000     C                   END
090200000000     C     FUNKEY        IFEQ      '24'
090300000000     C                   EXSR      F24GES
090400000000     C                   END
090500000000     C     FUNKEY        IFEQ      'HP'
090600000000     C                   EXSR      HELGES
090700000000     C                   END
090800000000     C*
090900000000     C                   END
091000000000     C*
091100000000     C                   ENDSR
091200000000     C*------------------------------------------------------------------------*
091300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
091400000000     C*------------------------------------------------------------------------*
091500000000     C     FUNVAR        BEGSR
091600000000     C*
091700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
091800000000     C*
091900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
092000000000     C     KEY           IFEQ      ENTER
092100000000     C                   MOVEL     *BLANKS       FUNKEY
092200000000     C                   END
092300000000     C     KEY           IFEQ      F1
092400000000     C                   MOVEL     '01'          FUNKEY
092500000000     C                   END
092600000000     C     KEY           IFEQ      F2
092700000000     C                   MOVEL     '02'          FUNKEY
092800000000     C                   END
092900000000     C     KEY           IFEQ      F3
093000000000     C                   MOVEL     '03'          FUNKEY
093100000000     C                   END
093200000000     C     KEY           IFEQ      F4
093300000000     C                   MOVEL     '04'          FUNKEY
093400000000     C                   END
093500000000     C     KEY           IFEQ      F5
093600000000     C                   MOVEL     '05'          FUNKEY
093700000000     C                   END
093800000000     C     KEY           IFEQ      F6
093900000000     C                   MOVEL     '06'          FUNKEY
094000000000     C                   END
094100000000     C     KEY           IFEQ      F7
094200000000     C                   MOVEL     '07'          FUNKEY
094300000000     C                   END
094400000000     C     KEY           IFEQ      F8
094500000000     C                   MOVEL     '08'          FUNKEY
094600000000     C                   END
094700000000     C     KEY           IFEQ      F9
094800000000     C                   MOVEL     '09'          FUNKEY
094900000000     C                   END
095000000000     C     KEY           IFEQ      F10
095100000000     C                   MOVEL     '10'          FUNKEY
095200000000     C                   END
095300000000     C     KEY           IFEQ      F11
095400000000     C                   MOVEL     '11'          FUNKEY
095500000000     C                   END
095600000000     C     KEY           IFEQ      F12
095700000000     C                   MOVEL     '12'          FUNKEY
095800000000     C                   END
095900000000     C     KEY           IFEQ      F13
096000000000     C                   MOVEL     '13'          FUNKEY
096100000000     C                   END
096200000000     C     KEY           IFEQ      F14
096300000000     C                   MOVEL     '14'          FUNKEY
096400000000     C                   END
096500000000     C     KEY           IFEQ      F15
096600000000     C                   MOVEL     '15'          FUNKEY
096700000000     C                   END
096800000000     C     KEY           IFEQ      F16
096900000000     C                   MOVEL     '16'          FUNKEY
097000000000     C                   END
097100000000     C     KEY           IFEQ      F17
097200000000     C                   MOVEL     '17'          FUNKEY
097300000000     C                   END
097400000000     C     KEY           IFEQ      F18
097500000000     C                   MOVEL     '18'          FUNKEY
097600000000     C                   END
097700000000     C     KEY           IFEQ      F19
097800000000     C                   MOVEL     '19'          FUNKEY
097900000000     C                   END
098000000000     C     KEY           IFEQ      F20
098100000000     C                   MOVEL     '20'          FUNKEY
098200000000     C                   END
098300000000     C     KEY           IFEQ      F21
098400000000     C                   MOVEL     '21'          FUNKEY
098500000000     C                   END
098600000000     C     KEY           IFEQ      F22
098700000000     C                   MOVEL     '22'          FUNKEY
098800000000     C                   END
098900000000     C     KEY           IFEQ      F23
099000000000     C                   MOVEL     '23'          FUNKEY
099100000000     C                   END
099200000000     C     KEY           IFEQ      F24
099300000000     C                   MOVEL     '24'          FUNKEY
099400000000     C                   END
099500000000     C     KEY           IFEQ      ROLLUP
099600000000     C                   MOVEL     'RU'          FUNKEY
099700000000     C                   END
099800000000     C     KEY           IFEQ      HELP
099900000000     C                   MOVEL     'HP'          FUNKEY
100000000000     C                   END
100100000000     C                   ENDSR
100200000000     C*------------------------------------------------------------------------*
100300000000     C* NOTE PER COMPILAZIONE SCHIERE
100400000000     C*------------------------------------------------------------------------*
100500000000     C* -MSG-
100600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
100700000000     C* (NON TOCCARE MSG,1)
100800000000     C*
100900000000     C* -CMD-
101000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
101100000000     C*
101200000000     C* -FUD-
101300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
101400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
101500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
101600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
101700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
101800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
101900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
102000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
102100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
102200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
102300000000     C*
102400000000     C*------------------------------------------------------------------------*
102500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
102600991019TIST15R- Tasto funzionale non gestito.                                         1
102700991020TIST15R- Percentuale non accettabile.                                          2
102800991019TIST15R- Anno non accettabile.                                                 3
102900991019TIST15R- Mese inizio non accettabile.                                          4
103000991019TIST15R- Mese fine non accettabile.                                            5
103100991019TIST15R- Mese inizio maggiore mese fine.                                       6
103200991020TIST15R- Range non valido, % iniziale > % finale.                              7
103300991019TIST15R- Immettere 'S' se si vogliono includere i volumi "reali" nella stampa. 8
103400991020TIST15R- Percentuale iniziale non accettabile.                         .       9
103500991020TIST15R- Percentuale finale non accettabile.                           io.    10
103600991019TIST15R- Immettere 'S' per rigenerare il file e 'N' per non rigenerare.       11
103700991019TIST15R- Divisione non accettabile.                                           12
103800991019TIST15R- Area non accettabile.                                                13
103900991019TIST15R- Punto operativo / terminal non accettabile                           14
104000991104TIST15R- Percentuale non accettabile, valori corretti da 0 a 100.             15
104100991019TIST15R- Data non valida.                                                     16
104200991019TIST15R- Parametro non accettabile se scelto lancio per saldi.                17
104300991019TIST15R- Parametro non accettabile se scelto lancio estemporaneo.             18
104400991020TIST15R- Tipo lancio non accettabile. Inserire 'S'.                           19
104500991019TIST15R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
104600000000** ======== SCHIERA: CMD  (COMANDI)                        ================
104700000000WRKSPLF
104800000000
104900000000
105000000000
105100000000
105200000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
105300000000                   -        (S/N)=N                        funz.01¬
105400000000                   -        (S/N)=N                        funz.02¬
105500000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
105600000000                   -        (S/N)=N                        funz.04¬
105700000000                   -        (S/N)=N                        funz.05¬
105800000000F06=Lancio         -        (S/N)=S                        funz.06¬
105900000000                   -        (S/N)=N                        funz.07¬
106000000000                   -        (S/N)=N                        funz.08¬
106100000000                   -        (S/N)=N                        funz.09¬
106200000000                   -        (S/N)=N                        funz.10¬
106300000000                   -        (S/N)=N                        funz.11¬
106400000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
106500000000                   -        (S/N)=N                        funz.13¬
106600000000                   -        (S/N)=N                        funz.14¬
106700000000                   -        (S/N)=N                        funz.15¬
106800000000                   -        (S/N)=N                        funz.16¬
106900000000                   -        (S/N)=N                        funz.17¬
107000000000                   -        (S/N)=N                        funz.18¬
107100000000                   -        (S/N)=N                        funz.19¬
107200000000                   -        (S/N)=N                        funz.20¬
107300000000                   -        (S/N)=N                        funz.21¬
107400000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
107500000000                   -        (S/N)=N                        funz.23¬
107600000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
