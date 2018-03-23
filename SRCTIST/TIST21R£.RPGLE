000100000000     H*------------------------------------------------------------------------*
000200991117     H* STATISTICA SERVIZI STRATEGI - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800991117     FTIST21D   CF   E             WORKSTN INFDS(DEVDS1)
000900000000     F                                     SFILE(SS35DMS:MSGULT)
001000991117     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200991118     FTIVSS02L  IF   E           K DISK
001300000000     D*------------------------------------------------------------------------*
001400000000     D* SCHIERE
001500000000     D*------------------------------------------------------------------------*
001600000000     D*-------------------
001700000000     D* MESSAGGI
001800000000     D*-------------------
001900011228     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
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
003200991124     D*------------------
003300991124     D* DS - CONVERSIONE DATA IN FORMATO ALFA  (10)
003400991124     D*------------------
003500991124     D                 DS                  INZ
003600991124     D  GGA                    1      2
003700991124     D  SEP1                   3      3
003800991124     D  MMA                    4      5
003900991124     D  SEP2                   6      6
004000991124     D  AAAAA                  7     10
004100991124     D  DATAA                  1     10
004200991117     D*------------------
004300991124     D* DS - X CONTROLLO E X INVERSIONE VELOCE DATA (8 NUM)
004400991117     D*------------------
004500991117     D                 DS                  INZ
004600991117     D  AAINV                  1      4  0
004700991117     D  MMINV                  5      6  0
004800991117     D  GGINV                  7      8  0
004900991117     D  INVDATA                1      8  0
005000991124     D*------------------
005100991124     D                 DS                  INZ
005200991124     D  GG1                    1      1
005300991124     D  GG2                    2      2
005400991124     D  FIL1                   3      3
005500991124     D  MM1                    4      4
005600991124     D  MM2                    5      5
005700991124     D  FIL2                   6      6
005800991124     D  AAAA1                  7      7
005900991124     D  AAAA2                  8      8
006000991124     D  AAAA3                  9      9
006100991124     D  AAAA4                 10     10
006200991124     D  CHKDATA                1     10
006300000000     D*------------------------------------------------------------------------*
006400000000     D* INPUT
006500000000     D*------------------------------------------------------------------------*
006600000000     D*-------------------
006700000000     D* RIDEFINIZIONE SCHIERA -FUD-
006800000000     D*-------------------
006900000000     D FUDDS           DS
007000000000     D*                -DESCRIZIONE PARZIALE
007100000000     D  FUDDE1                 1      1
007200000000     D*                -FUNZIONE
007300000000     D  FUDFUN                 2      3
007400000000     D*                -DESCRIZIONE PARZIALE
007500000000     D  FUDDE2                 4     19
007600000000     D*                -FILLER
007700000000     D  FUDFI0                20     34
007800000000     D*                -FLAG FUNZ.ATTIVA (S/N)
007900000000     D  FUDATT                35     35
008000000000     D*                -FILLER
008100000000     D  FUDFI1                36     80
008200000000     D*                -DESCRIZIONE COMPLETA
008300000000     D  FUDDES                 1     19
008400000000     D*                -RIDEFINIZIONE RIGA
008500000000     D  FUDRIG                 1     80
008600000000     D*-------------------
008700000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
008800000000     D*-------------------
008900000000     D DSMSG         E DS
009000000000     D*-------------------
009100020917     D* REPERIMENTO DATI UTENTE
009200020917     D*-------------------
009300020917     D TIBS34DS      E DS                                                       *Profili utente
009400020917     D DDATIUTE      E DS                                                       *Dati utente
009500020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009600970401     D*----------------
009700970401     D* DS PER TABELLA CODICI AREA
009800970401     D*----------------
009900970401     D DS05          E DS
010000970401     D*----------------
010100970401     D* DS PER TABELLA CODICI DISTRETTO
010200970401     D*----------------
010300970401     D DS17          E DS
010400000000     D*-------------------
010500980403     D* DS DATI STATISTICHE GRUPPO
010600000000     D*-------------------
010700980403     D DS5S          E DS
010800000000     D*-------------------
010900000000     D* DS PARAMETRI DI LANCIO PGM
011000000000     D*-------------------
011100950703     D PARAM           DS
011200991122     D  PARDAI                 1      8  0
011300991122     D  PARDAF                 9     16  0
011400991122     D  PARSNR                17     17
011500991122     D  PARDIV                18     18
011600991122     D  PARARE                19     21  0
011700991122     D  PARFIL                22     24  0
011800991122     D  PARKSC                25     31  0
011900991122     D  PARDET                32     32
012000011228     D  PARGIO                33     35  0
012100000000     D*-------------------
012200000000     D* DS PER GESTIONE TASTI FUNZIONALI
012300000000     D*-------------------
012400000000     D DSKEY         E DS
012500000000     D DEVDS1          DS
012600000000     D  KEY                  369    369
012700000000     D*-------------------
012800000000     D* ARCHITETTURA
012900000000     D*-------------------
013000000000     D KPJBA         E DS
013100011228     D*-------------------
013200991118     D* DS REPERIMENTO DATI UTENTE
013300991118     D*-------------------
013400991118     DBS69DS         E DS                  extname(TIBS69DS)
013500991118     DACODS          E DS                  extname(CNACO00F)
013600991118     DINDDS          E DS                  extname(CNIND00F)
013700991118     DCLPDS          E DS                  extname(CNCLP00F)
013800991118     DCLSDS          E DS                  extname(FNCLS00F)
013900011228     D*-------------------
014000011228     D* DS REPERIMENTO NUMERO GG LAVORATIVI TRA 2 DATE
014100011228     D*-------------------
014200011228     D WL08            DS
014300011228     D  L08DA                  1      8  0
014400011228     D  L08A                   9     16  0
014500011228     D  L08L                  17     21  0
014600020805     D*-------------------
014700020805     D* CAPOCONTO / SOTTOCONTO DI RITORNO "XCLIR"
014800020805     D*-------------------
014900020805     D KSC             S              7  0 DIM(30)
015000020805     D KSA             S              4    DIM(30)                              *ALFANUMERICO
015100020805     D*------------------
015200020805     D* DS "XCLIR"
015300020805     D*------------------
015400020805     D DSXC          E DS                  EXTNAME(XCLIRDS)
015500020805     D*------------------
015600020805     D* DS PER PASSAGGIO CAMPI PACKED
015700020805     D*------------------
015800020805     D                 DS
015900020805     D  DSKSN                  1      4P 0
016000020805     D  DSKSA                  1      4
016100000000     C*------------------------------------------------------------------------*
016200000000     C* MAIN LINE
016300000000     C*------------------------------------------------------------------------*
016400000000     C*
016500000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
016600000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
016700000000     C*
016800000000     C* CICLO FINO A CHE:
016900000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
017000000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
017100000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
017200000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
017300000000     C     F12L00        OREQ      '1'
017400000000     C     FLFINE        OREQ      '1'
017500000000     C*
017600000000     C* EMETTE FORMATO VIDEO
017700000000     C                   EXSR      BUTFMR
017800000000     C*
017900000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
018000000000     C                   EXSR      FUNCON
018100000000     C*
018200000000     C                   END                                                    <-- 2 --
018300000000     C*
018400000000     C                   END                                                    <-- 1 --
018500991118     C*
018600991118     C* OPERAZIONI FINALI
018700991118     C                   EXSR      FINSR
018800991118     C*
018900000000     C                   SETON                                        LR
019000991118     C*------------------------------------------------------------------------*
019100991118     C* FINSR - OPERAZIONI FINALI
019200991118     C*------------------------------------------------------------------------*
019300991118     C     FINSR         BEGSR
019400991118     C*
019500991118     C* CHIUDE I FILE DEI PGM CHIAMATI
019600991118     C                   CLEAR                   BS69DS
019700991118     C                   CLEAR                   ACODS
019800991118     C                   CLEAR                   INDDS
019900991118     C                   CLEAR                   CLPDS
020000991118     C                   CLEAR                   CLSDS
020100991118     C                   MOVEL     'C'           I69TLA
020200991118     C                   MOVEL     KNSIF         I69SIF
020300991118     C                   CALL      'TIBS69R'
020400991118     C                   PARM                    BS69DS
020500991118     C                   PARM                    ACODS
020600991118     C                   PARM                    INDDS
020700991118     C                   PARM                    CLPDS
020800991118     C                   PARM                    CLSDS
020900991118     C*
021000991118     C                   ENDSR
021100000000     C*------------------------------------------------------------------------*
021200000000     C* EMISSIONE FORMATO A VIDEO
021300000000     C*------------------------------------------------------------------------*
021400000000     C     BUTFMR        BEGSR
021500000000     C*
021600000000     C* WRITE RECORD TESTATA
021700000000     C                   WRITE     SS35D01
021800000000     C*
021900000000     C* VISUALIZZAZIONE MESSAGGIO
022000000000     C                   EXSR      SNDMSG
022100000000     C*
022200000000     C* WRITE RECORD FUNZIONI
022300000000     C                   WRITE     SS35D02
022400991019     C*
022500991019     C* EXFMT RECORD DETTAGLIO
022600991019     C                   EXFMT     SS35D03
022700991019     C*
022800000000     C* AZZERO FLAG DI ERRORE LIVELLO
022900000000     C                   MOVEL     '0'           ERRL00
023000000000     C*
023100000000     C* AZZERO FLAG DI ERRORE
023200011228     C                   SETOFF                                       697071
023300000000     C                   SETOFF                                       727377
023400000000     C                   SETOFF                                       787983
023500000000     C                   SETOFF                                       848586
023600991117     C                   SETOFF                                       808182
023700000000     C*
023800000000     C                   ENDSR
023900000000     C*------------------------------------------------------------------------*
024000000000     C* SNDMSG - MANDA MESSAGGIO
024100000000     C*------------------------------------------------------------------------*
024200000000     C     SNDMSG        BEGSR
024300000000     C*
024400000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
024500000000     C                   MOVEL     '1'           *IN40
024600000000     C                   WRITE     SS35DMC
024700000000     C                   MOVEL     '0'           *IN40
024800000000     C*
024900000000     C* VISUALIZZAZIONE MESSAGGIO
025000000000     C                   Z-ADD     0             MSGULT            4 0
025100000000     C                   CALL      'XMSG'        XMSGPA
025200000000     C     DSMSER        IFEQ      0
025300000000     C                   MOVEL     DSMSKE        MSGKEY
025400000000     C                   END
025500000000     C                   Z-ADD     1             MSGULT
025600000000     C                   WRITE     SS35DMS                                      SFL MESS
025700000000     C                   WRITE     SS35DMC                                      CONTR MESS
025800000000     C*
025900000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
026000000000     C                   MOVEL     *BLANKS       DSMSMS
026100000000     C*
026200000000     C                   ENDSR
026300000000     C*------------------------------------------------------------------------*
026400000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
026500000000     C*------------------------------------------------------------------------*
026600000000     C     ENTGES        BEGSR
026700000000     C*
026800000000     C* CONTROLLA I CAMPI IMMESSI
026900000000     C                   EXSR      DECODI
027000000000     C*
027100000000     C                   ENDSR
027200000000     C*------------------------------------------------------------------------*
027300000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
027400000000     C*------------------------------------------------------------------------*
027500000000     C     F01GES        BEGSR
027600000000     C                   ENDSR
027700000000     C*------------------------------------------------------------------------*
027800000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
027900000000     C*------------------------------------------------------------------------*
028000000000     C     F03GES        BEGSR
028100000000     C*
028200000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
028300000000     C                   MOVEL     '1'           F03L00
028400000000     C*
028500000000     C                   ENDSR
028600000000     C*------------------------------------------------------------------------*
028700000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
028800000000     C*------------------------------------------------------------------------*
028900991020     C     F06GES        BEGSR
029000000000     C*
029100991105     C                   EXSR      DECODI
029200991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
029300991105     C     ERRL00        IFEQ      '0'
029400991105     C*
029500000000     C* LANCIO IL PROGRAMMA DI STAMPA
029600991117     C                   MOVEL     'ST22'        KCOAZ
029700000000     C                   EXSR      VIDPAR
029800950703     C                   MOVEL     PARAM         KPJBU
029900011227 MOM C                   CALL      'BCH10'
030000011227     C*                  CALL      'TIST22C'
030100991021     C                   PARM                    KPJBA
030200991124     C*
030300991124     C* RIMETTE FILE DA RIGENERARE = 'N' PER SICUREZZA
030400991124     C                   MOVEL     'N'           VIDSNR
030500991105     C                   ENDIF
030600991105     C*
030700000000     C                   ENDSR
030800000000     C*------------------------------------------------------------------------*
030900000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
031000000000     C*------------------------------------------------------------------------*
031100000000     C     F07GES        BEGSR
031200000000     C                   ENDSR
031300000000     C*------------------------------------------------------------------------*
031400000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
031500000000     C*------------------------------------------------------------------------*
031600000000     C     F08GES        BEGSR
031700000000     C                   ENDSR
031800000000     C*------------------------------------------------------------------------*
031900000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
032000000000     C*------------------------------------------------------------------------*
032100000000     C     F12GES        BEGSR
032200000000     C*
032300000000     C* IMPOSTO FLAG DI LIVELLO
032400000000     C                   MOVEL     '1'           F12L00
032500000000     C*
032600000000     C                   ENDSR
032700000000     C*------------------------------------------------------------------------*
032800000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
032900000000     C*------------------------------------------------------------------------*
033000000000     C     F21GES        BEGSR
033100000000     C                   ENDSR
033200000000     C*------------------------------------------------------------------------*
033300000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
033400000000     C*------------------------------------------------------------------------*
033500000000     C     F22GES        BEGSR
033600000000     C                   ENDSR
033700000000     C*------------------------------------------------------------------------*
033800991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
033900000000     C*------------------------------------------------------------------------*
034000000000     C     F24GES        BEGSR
034100000000     C*
034200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
034300000000     C                   EXSR      FUNGES
034400000000     C*
034500000000     C                   ENDSR
034600000000     C*------------------------------------------------------------------------*
034700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
034800000000     C*------------------------------------------------------------------------*
034900000000     C     HELGES        BEGSR
035000000000     C                   ENDSR
035100991020     C*------------------------------------------------------------------------*
035200991105     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
035300000000     C*------------------------------------------------------------------------*
035400000000     C     DECODI        BEGSR
035500991117     C*
035600991117     C* CONTROLLO VELOCE DATE
035700991118     C                   EVAL      VIDDAI=%TRIM(VIDDAI)
035800991117     C                   MOVEL     VIDDAI        CHKDATA
035900991118     C     FIL1          IFNE      '/'
036000991117     C     FIL2          ANDNE     '/'
036100991118     C     ' '           CHECKR    VIDDAI        POS               1 0
036200991118     C     POS           IFNE      8
036300991118     C                   MOVEL     '1'           ERRL00
036400991118     C                   MOVEL     MSG(3)        DSMSMS
036500991118     C                   SETON                                        70
036600991118     C                   ELSE
036700991118     C     AAAA3         IFEQ      *blanks
036800991118     C     AAAA4         ANDEQ     *blanks
036900991117     C                   MOVEL     AAAA2         AAAA4
037000991117     C                   MOVEL     AAAA1         AAAA3
037100991117     C                   MOVEL     FIL2          AAAA2
037200991117     C                   MOVEL     MM2           AAAA1
037300991117     C                   MOVEL     MM1           MM2
037400991117     C                   MOVEL     FIL1          MM1
037500991117     C                   MOVEL     '/'           FIL1
037600991117     C                   MOVEL     '/'           FIL2
037700991117     C                   ENDIF
037800991117     C                   MOVEL     CHKDATA       VIDDAI
037900991118     C                   ENDIF
038000991118     C                   ENDIF
038100991117     C*
038200991117     C                   MOVEL     VIDDAI        DATAA
038300991117     C                   MOVE      AAAAA         AAINV
038400991117     C                   MOVE      MMA           MMINV
038500991117     C                   MOVE      GGA           GGINV
038600991117     C                   Z-ADD     INVDATA       VIHDAI
038700991117     C*
038800991117     C* CONTROLLO DATE
038900991020     C     ERRL00        IFEQ      '0'
039000991117     C     VIDDAI        IFEQ      *BLANKS
039100000000     C                   MOVEL     '1'           ERRL00
039200000000     C                   MOVEL     MSG(3)        DSMSMS
039300000000     C                   SETON                                        70
039400991020     C                   ELSE
039500991117     C     AAINV         IFLT      1900
039600000000     C                   MOVEL     '1'           ERRL00
039700000000     C                   MOVEL     MSG(3)        DSMSMS
039800000000     C                   SETON                                        70
039900991020     C                   END
040000991117     C     MMINV         IFGT      12
040100991117     C     MMINV         ORLT      01
040200991117     C                   MOVEL     '1'           ERRL00
040300991117     C                   MOVEL     MSG(3)        DSMSMS
040400991117     C                   SETON                                        70
040500991117     C                   END
040600991117     C     GGINV         IFGT      31
040700991117     C     GGINV         ORLT      01
040800991117     C                   MOVEL     '1'           ERRL00
040900991117     C                   MOVEL     MSG(3)        DSMSMS
041000991117     C                   SETON                                        70
041100991117     C                   END
041200991020     C                   END
041300991020     C                   END
041400991117     C*
041500991118     C                   EVAL      VIDDAF=%TRIM(VIDDAF)
041600991118     C                   MOVEL     VIDDAF        CHKDATA
041700991118     C     FIL1          IFNE      '/'
041800991118     C     FIL2          ANDNE     '/'
041900991118     C     ' '           CHECKR    VIDDAF        POS               1 0
042000991118     C     POS           IFNE      8
042100991118     C                   MOVEL     '1'           ERRL00
042200991118     C                   MOVEL     MSG(3)        DSMSMS
042300991118     C                   SETON                                        71
042400991118     C                   ELSE
042500991118     C     AAAA3         IFEQ      *blanks
042600991118     C     AAAA4         ANDEQ     *blanks
042700991117     C                   MOVEL     AAAA2         AAAA4
042800991117     C                   MOVEL     AAAA1         AAAA3
042900991117     C                   MOVEL     FIL2          AAAA2
043000991117     C                   MOVEL     MM2           AAAA1
043100991117     C                   MOVEL     MM1           MM2
043200991117     C                   MOVEL     FIL1          MM1
043300991117     C                   MOVEL     '/'           FIL1
043400991117     C                   MOVEL     '/'           FIL2
043500991117     C                   ENDIF
043600991117     C                   MOVEL     CHKDATA       VIDDAF
043700991118     C                   ENDIF
043800991118     C                   ENDIF
043900991117     C*
044000991117     C                   MOVEL     VIDDAF        DATAA
044100991117     C                   MOVEL     AAAAA         AAINV
044200991117     C                   MOVEL     MMA           MMINV
044300991117     C                   MOVEL     GGA           GGINV
044400991117     C                   Z-ADD     INVDATA       VIHDAF
044500991117     C*
044600991117     C* CONTROLLO DATE
044700991117     C     ERRL00        IFEQ      '0'
044800991117     C     VIDDAF        IFEQ      *BLANKS
044900991117     C                   MOVEL     '1'           ERRL00
045000991117     C                   MOVEL     MSG(3)        DSMSMS
045100991117     C                   SETON                                        71
045200991117     C                   ELSE
045300991117     C     AAINV         IFLT      1900
045400991117     C                   MOVEL     '1'           ERRL00
045500991117     C                   MOVEL     MSG(3)        DSMSMS
045600991117     C                   SETON                                        71
045700991117     C                   END
045800991117     C     MMINV         IFGT      12
045900991117     C     MMINV         ORLT      01
046000991117     C                   MOVEL     '1'           ERRL00
046100991117     C                   MOVEL     MSG(3)        DSMSMS
046200991117     C                   SETON                                        71
046300991117     C                   END
046400991117     C     GGINV         IFGT      31
046500991117     C     GGINV         ORLT      01
046600991117     C                   MOVEL     '1'           ERRL00
046700991117     C                   MOVEL     MSG(3)        DSMSMS
046800991117     C                   SETON                                        71
046900991117     C                   END
047000991117     C                   END
047100991117     C                   END
047200991117     C*
047300000000     C     ERRL00        IFEQ      '0'                                          *INIZIO MAGGIORE FIN
047400991117     C     VIHDAI        IFGT      VIHDAF
047500000000     C                   MOVEL     '1'           ERRL00
047600000000     C                   MOVEL     MSG(6)        DSMSMS
047700991117     C                   SETON                                        7071
047800000000     C                   END
047900000000     C                   END
048000991124     C*
048100991124     C* CONTROLLO CORRETTEZZA DI "CALENDARIO" DELLE DATE
048200991124     C     *ISO          TEST(D)                 VIHDAI                 11
048300991124     C     *IN11         IFEQ      *ON
048400991124     C                   MOVEL     1             ERRL00
048500991124     C                   MOVEL     MSG(3)        DSMSMS
048600991124     C                   SETON                                        70
048700991124     C                   ENDIF
048800991124     C*
048900991124     C     *ISO          TEST(D)                 VIHDAF                 11
049000991124     C     *IN11         IFEQ      *ON
049100991124     C                   MOVEL     1             ERRL00
049200991124     C                   MOVEL     MSG(3)        DSMSMS
049300991124     C                   SETON                                        71
049400991124     C                   ENDIF
049500011228     C*
049600011228     C* GIORNI LAVORATIVI
049700011228     C     ERRL00        IFEQ      '0'
049800011228     C     VIDGIO        IFEQ      *ZEROS
049900011228     C* SE 0 PROPONGO:
050000011228     C                   CLEAR                   WL08
050100011228     C                   Z-ADD     VIHDAI        L08DA
050200011228     C                   Z-ADD     VIHDAF        L08A
050300011228     C                   CALL      'XSRLAV8'
050400011228     C                   PARM                    WL08
050500011228     C                   MOVE      L08L          VIDGIO
050600011228     C                   MOVEL     '1'           ERRL00
050700011228     C                   MOVEL     MSG(21)       DSMSMS
050800011228     C                   SETON                                        69
050900011228     C                   ENDIF
051000011228     C                   ENDIF
051100000000     C*
051200000000     C* CONTROLLO FLAG FILE DA RIGENERARE
051300000000     C     ERRL00        IFEQ      '0'
051400000000     C     VIDSNR        IFEQ      'S'
051500000000     C     VIDSNR        OREQ      'N'
051600000000     C                   ELSE
051700000000     C                   MOVEL     '1'           ERRL00
051800000000     C                   MOVEL     MSG(11)       DSMSMS
051900000000     C                   SETON                                        77
052000000000     C                   END
052100000000     C                   END
052200991028     C*
052300991117     C* CONTROLLO FLAG STAMPA SINTETICA O DETTAGLIO
052400991117     C     ERRL00        IFEQ      '0'
052500991117     C     VIDDET        IFEQ      'S'
052600991117     C     VIDDET        OREQ      'D'
052700991124     C     VIDDET        OREQ      'T'
052800991117     C                   ELSE
052900991117     C                   MOVEL     '1'           ERRL00
053000991117     C                   MOVEL     MSG(11)       DSMSMS
053100991117     C                   SETON                                        80
053200991117     C                   END
053300991117     C                   END
053400000000     C*
053500000000     C* CONTROLLO LA DIVISIONE
053600000000     C                   MOVEL     *BLANKS       VDDDIV
053700000000     C     ERRL00        IFEQ      '0'
053800000000     C     VIDDIV        IFNE      *BLANKS
053900000000     C                   EXSR      CHKDIV
054000000000     C     ERRL00        IFEQ      '1'
054100000000     C                   MOVEL     '1'           ERRL00
054200000000     C                   MOVEL     MSG(12)       DSMSMS
054300000000     C                   SETON                                        78
054400000000     C                   END
054500000000     C                   END
054600000000     C                   END
054700000000     C*
054800000000     C* CONTROLLO L'AREA
054900000000     C                   MOVEL     *BLANKS       VDDARE
055000000000     C     ERRL00        IFEQ      '0'
055100000000     C     VIDARE        IFNE      *BLANKS
055200000000     C                   EXSR      CHKARE
055300000000     C     ERRL00        IFEQ      '1'
055400000000     C                   MOVEL     '1'           ERRL00
055500000000     C                   MOVEL     MSG(13)       DSMSMS
055600000000     C                   SETON                                        79
055700000000     C                   END
055800000000     C                   END
055900000000     C                   END
056000000000     C*
056100000000     C* CONTROLLO LA FILIALE
056200000000     C                   MOVEL     *BLANKS       VDDFLP
056300000000     C     ERRL00        IFEQ      '0'
056400000000     C     VIDFLP        IFNE      *BLANKS
056500000000     C                   MOVE      VIDFLP        WLIN
056600000000     C                   EXSR      CHKLIN
056700000000     C     ERRL00        IFEQ      '0'
056800000000     C                   MOVEL     WLIN          VIDFLP
056900000000     C                   MOVEL     WDLIN         VDDFLP
057000000000     C                   ELSE
057100000000     C                   SETON                                        80
057200000000     C                   END
057300000000     C                   END
057400000000     C                   END
057500991117     C*
057600991117     C* CONTROLLO IL CLIENTE
057700991117     C                   MOVEL     *BLANKS       VDDKSC
057800991117     C     ERRL00        IFEQ      '0'
057900991117     C     VIDKSC        IFNE      *BLANKS
058000991117     C                   MOVE      VIDKSC        WKSC
058100991117     C                   EXSR      CHKKSC
058200991117     C     ERRL00        IFEQ      '0'
058300991117     C                   MOVEL     WKSC          VIDKSC
058400991117     C                   MOVEL     WDKSC         VDDKSC
058500991117     C                   ELSE
058600991117     C                   SETON                                        81
058700991117     C                   END
058800991117     C                   END
058900991117     C                   END
059000000000     C*
059100000000     C                   ENDSR
059200000000     C*------------------------------------------------------------------------*
059300000000     C*  CHKDIV - CONTROLLA LA DIVISIONE
059400000000     C*------------------------------------------------------------------------*
059500000000     C     CHKDIV        BEGSR
059600000000     C*
059700000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
059800000000     C     VIDDIV        IFEQ      '?'                                          --- 1 -->
059900000000     C                   MOVEL     CODUT         §KUT              1 0
060000000000     C                   MOVEL     '17'          §COD              2
060100000000     C                   MOVE      *BLANKS       §DES1
060200000000     C                   CALL      'X§TABER'
060300000000     C                   PARM                    §KUT
060400000000     C                   PARM                    §COD
060500000000     C                   PARM                    §KEY              8
060600970401     C                   PARM                    §DES             25
060700000000     C                   MOVEL     §KEY          VIDDIV
060800000000     C                   MOVEL     §DES          VDDDIV
060900000000     C*
061000000000     C                   ELSE                                                   <-- 1 -->
061100000000     C*
061200000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
061300000000     C                   MOVEL     CODUT         KTBUTE
061400000000     C                   MOVE      '17'          KTBCOD
061500000000     C                   MOVE      *BLANKS       KTBKEY
061600000000     C                   MOVEL     VIDDIV        KTBKEY
061700000000     C     KEYTAB        CHAIN     TABEL                              99
061800000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
061900000000     C     TBLFLG        OREQ      '*'
062000000000     C                   MOVEL     '1'           ERRL00
062100000000     C                   MOVEL     *ALL'*'       VDDDIV
062200000000     C                   ELSE                                                   <-- 2 -->
062300970401     C                   MOVEL     TBLUNI        DS17
062400970401     C                   MOVEL     §17DES        VDDDIV
062500000000     C                   END                                                    <-- 2 ---
062600000000     C*
062700000000     C                   END                                                    <-- 1 ---
062800000000     C*
062900000000     C                   ENDSR
063000000000     C*------------------------------------------------------------------------*
063100000000     C*  CHKARE - CONTROLLA L'AREA
063200000000     C*------------------------------------------------------------------------*
063300000000     C     CHKARE        BEGSR
063400000000     C*
063500000000     C* CONTROLLO SE E' STATO INSERITO '?'
063600000000     C     '?'           SCAN      VIDARE        A                 1 0    99
063700000000     C*
063800000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
063900000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
064000000000     C                   MOVEL     CODUT         §KUT              1 0
064100000000     C                   MOVEL     '05'          §COD              2
064200000000     C                   MOVE      *BLANKS       §DES1
064300000000     C                   CALL      'X§TABER'
064400000000     C                   PARM                    §KUT
064500000000     C                   PARM                    §COD
064600000000     C                   PARM                    §KEY              8
064700970401     C                   PARM                    §DES             25
064800000000     C                   MOVEL     §KEY          VIDARE
064900000000     C                   MOVEL     §DES          VDDARE
065000000000     C*
065100000000     C                   ELSE                                                   <-- 1 -->
065200000000     C*
065300000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
065400000000     C                   MOVEL     CODUT         KTBUTE
065500000000     C                   MOVE      '05'          KTBCOD
065600000000     C                   MOVE      *BLANKS       KTBKEY
065700000000     C                   MOVEL     VIDARE        KTBKEY
065800000000     C     KEYTAB        CHAIN     TABEL                              99
065900000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
066000000000     C     TBLFLG        OREQ      '*'
066100000000     C                   MOVEL     '1'           ERRL00
066200000000     C                   MOVEL     *ALL'*'       VDDARE
066300000000     C                   ELSE                                                   <-- 2 -->
066400970401     C                   MOVEL     TBLUNI        DS05
066500970401     C                   MOVEL     §05DES        VDDARE
066600000000     C                   END                                                    <-- 2 ---
066700000000     C*
066800000000     C                   END                                                    <-- 1 ---
066900000000     C*
067000000000     C                   ENDSR
067100000000     C*------------------------------------------------------------------------*
067200000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
067300000000     C*------------------------------------------------------------------------*
067400000000     C     CHKLIN        BEGSR
067500000000     C*
067600000000     C* CONTROLLO SE E' STATO INSERITO '?'
067700000000     C     '?'           SCAN      WLIN          A                 1 0    99
067800000000     C*
067900000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
068000000000IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
068100000000     C                   MOVEL     *BLANKS       §COD1             3
068200000000     C                   MOVE      *BLANKS       §DES1
068300971203     C                   MOVE      *BLANKS       §PRA
068400971203     C                   CALL      'TNSD24R'
068500000000     C                   PARM                    §COD1
068600000000     C                   PARM                    §TIP              1
068700991117     C                   PARM                    §DES1            25
068800971203     C                   PARM                    §PRA              3
068900000000     C                   MOVEL     §COD1         WLIN              3
069000000000     C                   MOVEL     §DES1         WDLIN            20
069100000000     C*
069200000000X   1C                   ELSE
069300000000     C*
069400000000     C* CARATTERI ALFABETICI NON VALIDI
069500000000IF  2C     WLIN          IFLT      *ZEROS
069600000000     C                   MOVEL     '1'           ERRL00
069700000000     C                   MOVEL     MSG(14)       DSMSMS
069800000000     C                   MOVEL     *ALL'*'       WDLIN
069900000000X   2C                   ELSE
070000000000     C*
070100000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
070200000000     C                   MOVE      WLIN          KORFIL
070300000000     C     KEYORG        CHAIN     AZORG                              99
070400000000IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
070500000000     C                   MOVEL     ORGDES        WDLIN
070600000000X   3C                   ELSE                                                   <-- 2 -->
070700000000     C                   MOVEL     '1'           ERRL00
070800000000     C                   MOVEL     MSG(14)       DSMSMS
070900000000     C                   MOVEL     *ALL'*'       WDLIN
071000000000E   3C                   END                                                    <-- 2 ---
071100000000IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
071200000000     C                   MOVEL     '1'           ERRL00
071300000000     C                   MOVEL     MSG(14)       DSMSMS
071400000000     C                   MOVEL     *ALL'*'       WDLIN
071500000000E   3C                   END                                                    <-- 2 ---
071600000000E   2C                   END                                                    <-- 2 ---
071700000000E   1C                   ENDIF
071800000000     C*
071900000000     C                   ENDSR
072000991117     C*------------------------------------------------------------------------*
072100991117     C*  CHKKSC - CONTROLLA CLIENTE
072200991117     C*------------------------------------------------------------------------*
072300991117     C     CHKKSC        BEGSR
072400991117     C*
072500991117     C* CONTROLLO SE E' STATO INSERITO '?'
072600991117     C     '?'           SCAN      WKSC          A                 1 0    99
072700991117     C*
072800991117     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
072900020805IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
073000020805     C                   CLEAR                   DSXC
073100020923     C                   Z-ADD     DUTKCI        DXCCAP                         *CAPOCONTO
073200020805     C                   CALL      'XCLIR'
073300020805     C                   PARM                    DSXC
073400020805     C                   MOVEA     DXCKSC        KSA                            *SCOMPATTA CODICI
073500020805     C     1             DO        30            J                 4 0
073600020805     C                   MOVE      KSA(J)        DSKSA                          *SCH ALF -> DS ALF
073700020805     C                   Z-ADD     DSKSN         KSC(J)                         *DS NUM -> SCH NUM
073800020805     C                   ENDDO
073900020805     C                   MOVEL     KSC(1)        WKSC              7
074000991117     C*
074100991117X   1C                   ELSE
074200991117     C*
074300991117     C* CARATTERI ALFABETICI NON VALIDI
074400991117IF  2C     WKSC          IFLT      *ZEROS
074500991117     C                   MOVEL     '1'           ERRL00
074600991117     C                   MOVEL     MSG(14)       DSMSMS
074700020805     C                   MOVEL     *ALL'*'       WDKSC            30
074800991117X   2C                   ELSE
074900991117     C*
075000991117     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
075100991118     C                   MOVEL     *ALL'0'       KVSKSU
075200991118     C                   MOVE      WKSC          KVSKSU
075300991118     C     KVSKSU        CHAIN     TIVSS02L                           99
075400991117IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
075500991118     C* reperimento "Ragione Sociale" del Cliente Unificante
075600991118     C                   CLEAR                   BS69DS
075700991118     C                   CLEAR                   ACODS
075800991118     C                   MOVEL     KNSIF         I69SIF
075900991118     C                   MOVE      KVSKSU        I69KAC
076000991118     C                   CALL      'TIBS69R'
076100991118     C                   PARM                    BS69DS
076200991118     C                   PARM                    ACODS
076300991118     C                   PARM                    INDDS
076400991118     C                   PARM                    CLPDS
076500991118     C                   PARM                    CLSDS
076600991118     C     O69ERR        IFNE      '1'
076700991118     C                   MOVEL     ACORAG        VDDKSC
076800991118     C                   ELSE
076900991118     C                   MOVEL     *ALL'*'       VDDKSC
077000991118     C                   ENDIF
077100991117X   3C                   ELSE                                                   <-- 2 -->
077200991117     C                   MOVEL     '1'           ERRL00
077300991118     C                   MOVEL     MSG(15)       DSMSMS
077400991118     C                   MOVEL     *ALL'*'       VDDKSC
077500991117E   3C                   END                                                    <-- 2 ---
077600991117E   2C                   END                                                    <-- 2 ---
077700991117E   1C                   ENDIF
077800991117     C*
077900991117     C                   ENDSR
078000000000     C*------------------------------------------------------------------------*
078100000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
078200000000     C*------------------------------------------------------------------------*
078300000000     C     AZZVID        BEGSR
078400000000     C*
078500991117     C                   MOVEL     *blanks       VIDDAI
078600991117     C                   MOVEL     *blanks       VIDDAF
078700000119     C                   MOVEL     'S'           VIDSNR
078800991117     C                   MOVEL     'S'           VIDDET
078900000000     C*
079000000000     C                   ENDSR
079100000000     C*------------------------------------------------------------------------*
079200991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
079300000000     C*------------------------------------------------------------------------*
079400000000     C     VIDPAR        BEGSR
079500000000     C*
079600991122     C                   MOVEL     VIHDAI        PARDAI
079700991122     C                   MOVEL     VIHDAF        PARDAF
079800000000     C                   MOVEL     VIDSNR        PARSNR
079900000000     C                   MOVEL     VIDDIV        PARDIV
080000991122     C                   MOVEL     VIDARE        PARARE
080100991122     C                   MOVEL     VIDFLP        PARFIL
080200991117     C                   MOVE      VIDKSC        PARKSC
080300991117     C                   MOVEL     VIDDET        PARDET
080400011228     C                   Z-ADD     VIDGIO        PARGIO
080500000000     C*
080600000000     C                   ENDSR
080700020917     C*--------------------------------------------------------------------------------------------*
080800020917     C* REPERISCE I DATI UTENTE
080900020917     C*--------------------------------------------------------------------------------------------*
081000020917     C     REPDATIUTE    BEGSR
081100020917     C*
081200020917     C* INIZIALIZZA VARIABILI DI WRK
081300020917     C                   CLEAR                   TIBS34DS
081400020917     C                   CLEAR                   AZUTEDS
081500020917     C                   CLEAR                   DDATIUTE
081600020917     C*
081700020917     C     *DTAARA       DEFINE    §azute        azuteds
081800020917     C     *DTAARA       DEFINE    §datiute      ddatiute
081900020917     C                   IN(E)     *DTAARA
082000020917if  1C                   IF        %Error
082100020917     c                   EVAL      I34Tla = 'L'
082200020917     C                   CALL      'TIBS34R'
082300020917     C                   PARM                    Tibs34Ds
082400020917     C                   IN        *DTAARA
082500020917e   1C                   ENDIF
082600020917     C*
082700020917     C                   ENDSR
082800000000     C*------------------------------------------------------------------------*
082900980403     C* *INZSR - OPERAZIONI INIZIALI
083000000000     C*------------------------------------------------------------------------*
083100980403     C     *INZSR        BEGSR
083200000000     C*
083300000000     C* RICEVIMENTO PARAMETRI
083400000000     C     *ENTRY        PLIST
083500000000     C                   PARM                    KPJBA
083600991117     C*
083700991117     C* CHIAVE LETTURA TABEL00F - COMPLETA
083800991117     C     KEYTAB        KLIST
083900991117     C                   KFLD                    KTBUTE
084000991117     C                   KFLD                    KTBCOD
084100991117     C                   KFLD                    KTBKEY
084200000000     C*
084300000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
084400000000     C     KEYORG        KLIST
084500000000     C                   KFLD                    KORFIL
084600000000     C*
084700000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084800000000     C     XMSGPA        PLIST
084900000000     C                   PARM                    DSMSMS
085000000000     C                   PARM                    DSMSPG
085100000000     C                   PARM                    DSMSKE
085200000000     C                   PARM                    DSMSER
085300000000     C*
085400000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
085500000000     C                   CALL      'XKEY'
085600000000     C                   PARM                    DSKEY
085700000000     C*
085800000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
085900000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
086000000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
086100991118     C     *LIKE         DEFINE    VSSKSU        KVSKSU
086200000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
086300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
086400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
086500000000     C*
086600000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
086700000000     C*   -NOME PGM CHIAMANTE
086800000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
086900991117     C                   MOVEL     'TIST21R'     DSMSPG
087000991117     C                   MOVEL     'TIST21R'     MSGQUE
087100000000     C                   MOVEL     *BLANKS       MSGKEY
087200000000     C                   MOVEL     *BLANKS       DSMSMS
087300000000     C*
087400000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
087500000000     C                   Z-ADD     0             FUNGRU            5 0
087600000000     C*
087700000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
087800000000     C                   EXSR      FUNGES
087900000000     C*
088000000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
088100000000     C                   MOVEL     '0'           F12L00            1
088200000000     C                   MOVEL     '0'           F03L00            1
088300000000     C                   MOVEL     '0'           FINL00            1
088400000000     C                   MOVEL     '0'           ERRL00            1
088500000000     C                   MOVEL     '0'           FLFINE            1
088600991117     C                   MOVEL     '/'           SEP1                           *VALIDITA' RECORD
088700991117     C                   MOVEL     '/'           SEP2                           *VALIDITA' RECORD
088800000000     C*
088900000000     C* LANCIO ROUTINE PER VERIFICARE SE
089000000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
089100000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
089200020917     C                   EXSR      REPDATIUTE
089300020917     C                   Z-ADD     1             CODUT             1 0
089400000000     C*
089500000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
089600000000     C                   EXSR      AZZVID
089700000000     C*
089800000000     C                   ENDSR
089900000000     C*------------------------------------------------------------------------*
090000000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
090100000000     C*------------------------------------------------------------------------*
090200000000     C     FUNGES        BEGSR
090300000000     C*
090400000000     C* PULISCO CAMPI DI LAVORO
090500000000     C                   Z-ADD     0             W                 5 0
090600000000     C*
090700000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
090800000000     C* E CARICO TABELLA FUNZIONI ATTIVE
090900000000     C     1             DO        24            I
091000000000     C                   MOVEL     FUD(I)        FUDRIG
091100000000     C*
091200000000     C* PULIZIA FUNZIONI PRECEDENTI
091300000000     C                   MOVEL     *BLANKS       FUV(I)
091400000000     C*
091500000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
091600000000     C* (SALTA F24)
091700000000     C     FUDATT        IFEQ      'S'
091800000000     C     FUDFUN        ANDNE     '24'
091900000000     C                   ADD       1             W
092000000000     C                   MOVEL     FUDDES        FUV(W)
092100000000     C                   END
092200000000     C                   END                                                    DO WHILE
092300000000     C*
092400000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
092500000000     C* ALTRIMENTI DISATTIVO F24
092600000000     C     1             DO        24            I
092700000000     C                   MOVEL     FUD(I)        FUDRIG
092800000000     C*
092900000000     C     FUDFUN        IFEQ      '24'
093000000000     C     W             IFGT      6
093100000000     C                   MOVEL     'S'           FUDATT
093200000000     C                   ELSE
093300000000     C                   MOVEL     *BLANKS       FUDATT
093400000000     C                   END                                                    W GT 8
093500000000     C                   MOVEL     FUDRIG        FUD(I)
093600000000     C                   END
093700000000     C                   END                                                    DO WHILE
093800000000     C*
093900000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
094000000000     C     W             DIV       6             I
094100000000     C                   MVR                     Z
094200000000     C*
094300000000     C     Z             IFNE      0
094400000000     C     I             ADD       1             Z
094500000000     C                   ELSE
094600000000     C                   Z-ADD     I             Z                 5 0
094700000000     C                   END
094800000000     C*
094900000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
095000000000     C                   ADD       1             FUNGRU
095100000000     C     FUNGRU        IFGT      Z
095200000000     C     FUNGRU        ORLT      1
095300000000     C                   Z-ADD     1             FUNGRU
095400000000     C                   END
095500000000     C*
095600000000     C* PRIMO GRUPPO DI 6 FUNZIONI
095700000000     C     FUNGRU        IFEQ      1
095800000000     C                   MOVEL     FUV(1)        FUN001
095900000000     C                   MOVEL     FUV(2)        FUN002
096000000000     C                   MOVEL     FUV(3)        FUN003
096100000000     C                   MOVEL     FUV(4)        FUN004
096200000000     C                   MOVEL     FUV(5)        FUN005
096300000000     C                   MOVEL     FUV(6)        FUN006
096400000000     C                   END
096500000000     C*
096600000000     C* SECONDO GRUPPO DI 6 FUNZIONI
096700000000     C     FUNGRU        IFEQ      2
096800000000     C                   MOVEL     FUV(7)        FUN001
096900000000     C                   MOVEL     FUV(8)        FUN002
097000000000     C                   MOVEL     FUV(9)        FUN003
097100000000     C                   MOVEL     FUV(10)       FUN004
097200000000     C                   MOVEL     FUV(11)       FUN005
097300000000     C                   MOVEL     FUV(12)       FUN006
097400000000     C                   END
097500000000     C*
097600000000     C* TERZO GRUPPO DI 6 FUNZIONI
097700000000     C     FUNGRU        IFEQ      3
097800000000     C                   MOVEL     FUV(13)       FUN001
097900000000     C                   MOVEL     FUV(14)       FUN002
098000000000     C                   MOVEL     FUV(15)       FUN003
098100000000     C                   MOVEL     FUV(16)       FUN004
098200000000     C                   MOVEL     FUV(17)       FUN005
098300000000     C                   MOVEL     FUV(18)       FUN006
098400000000     C                   END
098500000000     C*
098600000000     C* QUARTO GRUPPO DI 6 FUNZIONI
098700000000     C     FUNGRU        IFEQ      4
098800000000     C                   MOVEL     FUV(19)       FUN001
098900000000     C                   MOVEL     FUV(20)       FUN002
099000000000     C                   MOVEL     FUV(21)       FUN003
099100000000     C                   MOVEL     FUV(22)       FUN004
099200000000     C                   MOVEL     *BLANKS       FUN005
099300000000     C                   MOVEL     *BLANKS       FUN006
099400000000     C                   END
099500000000     C*
099600000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
099700000000     C                   MOVEL     *BLANKS       FUN007
099800000000     C                   MOVEL     *BLANKS       FUN008
099900000000     C*
100000000000     C     1             DO        24            I
100100000000     C                   MOVEL     FUD(I)        FUDRIG
100200000000     C     FUDFUN        IFEQ      '24'
100300000000     C     FUDATT        ANDEQ     'S'
100400000000     C                   MOVEL     FUDDES        FUN008
100500000000     C                   END
100600000000     C                   END                                                    DO
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
101000000000     C*------------------------------------------------------------------------*
101100000000     C     FUNCON        BEGSR
101200000000     C*
101300000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
101400000000     C                   EXSR      FUNVAR
101500000000     C*
101600000000     C* PULISCO CAMPI DI LAVORO
101700000000     C                   Z-ADD     0             I                 5 0
101800000000     C                   Z-ADD     0             W                 5 0
101900000000     C*
102000000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
102100000000     C     FUNKEY        IFNE      *BLANKS
102200000000     C     1             DO        24            I
102300000000     C                   MOVEL     FUD(I)        FUDRIG
102400000000     C*
102500000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
102600000000     C     FUNKEY        IFEQ      FUDFUN
102700000000     C     FUDATT        ANDEQ     'S'
102800000000     C                   Z-ADD     I             W
102900000000     C                   END
103000000000     C                   END                                                    DO WHILE
103100000000     C*
103200000000     C* FUNZIONE NON ATTIVA (ERRORE)
103300000000     C     W             IFEQ      0
103400000000     C*
103500000000     C* EMISSIONE MESSAGGIO DI ERRORE
103600000000     C                   MOVEL     MSG(1)        DSMSMS
103700000000     C*
103800000000     C                   END                                                    W=0
103900000000     C                   END                                                    FUNKEY NE *BLANKS
104000000000     C*
104100000000     C* FUNZIONE ATTIVA (PROCEDI ...)
104200000000     C     W             IFNE      0
104300000000     C     FUNKEY        OREQ      *BLANKS
104400000000     C*
104500000000     C     FUNKEY        IFEQ      *BLANKS
104600000000     C                   EXSR      ENTGES
104700000000     C                   END
104800000000     C     FUNKEY        IFEQ      '01'
104900000000     C                   EXSR      F01GES
105000000000     C                   END
105100000000     C     FUNKEY        IFEQ      '03'
105200000000     C                   EXSR      F03GES
105300000000     C                   END
105400000000     C     FUNKEY        IFEQ      '06'
105500000000     C                   EXSR      F06GES
105600000000     C                   END
105700000000     C     FUNKEY        IFEQ      '07'
105800000000     C                   EXSR      F07GES
105900000000     C                   END
106000000000     C     FUNKEY        IFEQ      '08'
106100000000     C                   EXSR      F08GES
106200000000     C                   END
106300000000     C     FUNKEY        IFEQ      '12'
106400000000     C                   EXSR      F12GES
106500000000     C                   END
106600000000     C     FUNKEY        IFEQ      '21'
106700000000     C                   EXSR      F21GES
106800000000     C                   END
106900000000     C     FUNKEY        IFEQ      '22'
107000000000     C                   EXSR      F22GES
107100000000     C                   END
107200000000     C     FUNKEY        IFEQ      '24'
107300000000     C                   EXSR      F24GES
107400000000     C                   END
107500000000     C     FUNKEY        IFEQ      'HP'
107600000000     C                   EXSR      HELGES
107700000000     C                   END
107800000000     C*
107900000000     C                   END
108000000000     C*
108100000000     C                   ENDSR
108200000000     C*------------------------------------------------------------------------*
108300000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
108400000000     C*------------------------------------------------------------------------*
108500000000     C     FUNVAR        BEGSR
108600000000     C*
108700000000     C* INIZIALIZZA VARIABILE -FUNKEY-
108800000000     C*
108900000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
109000000000     C     KEY           IFEQ      ENTER
109100000000     C                   MOVEL     *BLANKS       FUNKEY
109200000000     C                   END
109300000000     C     KEY           IFEQ      F1
109400000000     C                   MOVEL     '01'          FUNKEY
109500000000     C                   END
109600000000     C     KEY           IFEQ      F2
109700000000     C                   MOVEL     '02'          FUNKEY
109800000000     C                   END
109900000000     C     KEY           IFEQ      F3
110000000000     C                   MOVEL     '03'          FUNKEY
110100000000     C                   END
110200000000     C     KEY           IFEQ      F4
110300000000     C                   MOVEL     '04'          FUNKEY
110400000000     C                   END
110500000000     C     KEY           IFEQ      F5
110600000000     C                   MOVEL     '05'          FUNKEY
110700000000     C                   END
110800000000     C     KEY           IFEQ      F6
110900000000     C                   MOVEL     '06'          FUNKEY
111000000000     C                   END
111100000000     C     KEY           IFEQ      F7
111200000000     C                   MOVEL     '07'          FUNKEY
111300000000     C                   END
111400000000     C     KEY           IFEQ      F8
111500000000     C                   MOVEL     '08'          FUNKEY
111600000000     C                   END
111700000000     C     KEY           IFEQ      F9
111800000000     C                   MOVEL     '09'          FUNKEY
111900000000     C                   END
112000000000     C     KEY           IFEQ      F10
112100000000     C                   MOVEL     '10'          FUNKEY
112200000000     C                   END
112300000000     C     KEY           IFEQ      F11
112400000000     C                   MOVEL     '11'          FUNKEY
112500000000     C                   END
112600000000     C     KEY           IFEQ      F12
112700000000     C                   MOVEL     '12'          FUNKEY
112800000000     C                   END
112900000000     C     KEY           IFEQ      F13
113000000000     C                   MOVEL     '13'          FUNKEY
113100000000     C                   END
113200000000     C     KEY           IFEQ      F14
113300000000     C                   MOVEL     '14'          FUNKEY
113400000000     C                   END
113500000000     C     KEY           IFEQ      F15
113600000000     C                   MOVEL     '15'          FUNKEY
113700000000     C                   END
113800000000     C     KEY           IFEQ      F16
113900000000     C                   MOVEL     '16'          FUNKEY
114000000000     C                   END
114100000000     C     KEY           IFEQ      F17
114200000000     C                   MOVEL     '17'          FUNKEY
114300000000     C                   END
114400000000     C     KEY           IFEQ      F18
114500000000     C                   MOVEL     '18'          FUNKEY
114600000000     C                   END
114700000000     C     KEY           IFEQ      F19
114800000000     C                   MOVEL     '19'          FUNKEY
114900000000     C                   END
115000000000     C     KEY           IFEQ      F20
115100000000     C                   MOVEL     '20'          FUNKEY
115200000000     C                   END
115300000000     C     KEY           IFEQ      F21
115400000000     C                   MOVEL     '21'          FUNKEY
115500000000     C                   END
115600000000     C     KEY           IFEQ      F22
115700000000     C                   MOVEL     '22'          FUNKEY
115800000000     C                   END
115900000000     C     KEY           IFEQ      F23
116000000000     C                   MOVEL     '23'          FUNKEY
116100000000     C                   END
116200000000     C     KEY           IFEQ      F24
116300000000     C                   MOVEL     '24'          FUNKEY
116400000000     C                   END
116500000000     C     KEY           IFEQ      ROLLUP
116600000000     C                   MOVEL     'RU'          FUNKEY
116700000000     C                   END
116800000000     C     KEY           IFEQ      HELP
116900000000     C                   MOVEL     'HP'          FUNKEY
117000000000     C                   END
117100000000     C                   ENDSR
117200000000     C*------------------------------------------------------------------------*
117300000000     C* NOTE PER COMPILAZIONE SCHIERE
117400000000     C*------------------------------------------------------------------------*
117500000000     C* -MSG-
117600000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
117700000000     C* (NON TOCCARE MSG,1)
117800000000     C*
117900000000     C* -CMD-
118000000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
118100000000     C*
118200000000     C* -FUD-
118300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
118400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
118500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
118600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
118700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
118800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
118900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
119000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
119100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
119200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
119300000000     C*
119400000000     C*------------------------------------------------------------------------*
119500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
119600991117TIST21R- Tasto funzionale non gestito.                                         1
119700991117TIST21R- Percentuale non accettabile.                                          2
119800991117TIST21R- Data non accettabile.                                                 3
119900991117TIST21R- Mese inizio non accettabile.                                          4
120000991117TIST21R- Mese fine non accettabile.                                            5
120100991117TIST21R- Data inizio maggiore data fine.                                       6
120200991117TIST21R- Range non valido, % iniziale > % finale.                              7
120300991117TIST21R- Immettere 'S' se si vogliono includere i volumi "reali" nella stampa. 8
120400991117TIST21R- Percentuale iniziale non accettabile.                         .       9
120500991117TIST21R- Percentuale finale non accettabile.                           io.    10
120600991117TIST21R- Immettere 'S' per rigenerare il file e 'N' per non rigenerare.       11
120700991117TIST21R- Divisione non accettabile.                                           12
120800991117TIST21R- Area non accettabile.                                                13
120900991117TIST21R- Punto operativo / terminal non accettabile                           14
121000991117TIST21R- Percentuale non accettabile, valori corretti da 0 a 100.             15
121100991117TIST21R- Data non valida.                                                     16
121200991117TIST21R- Parametro non accettabile se scelto lancio per saldi.                17
121300991117TIST21R- Parametro non accettabile se scelto lancio estemporaneo.             18
121400991117TIST21R- Tipo lancio non accettabile. Inserire 'S'.                           19
121500991117TIST21R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
121600011228TIST21R- Inserire i giorni lavorativi                                         21
121700000000** ======== SCHIERA: CMD  (COMANDI)                        ================
121800000000WRKSPLF
121900000000
122000000000
122100000000
122200000000
122300000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
122400000000                   -        (S/N)=N                        funz.01¬
122500000000                   -        (S/N)=N                        funz.02¬
122600000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
122700000000                   -        (S/N)=N                        funz.04¬
122800000000                   -        (S/N)=N                        funz.05¬
122900000000F06=Lancio         -        (S/N)=S                        funz.06¬
123000000000                   -        (S/N)=N                        funz.07¬
123100000000                   -        (S/N)=N                        funz.08¬
123200000000                   -        (S/N)=N                        funz.09¬
123300000000                   -        (S/N)=N                        funz.10¬
123400000000                   -        (S/N)=N                        funz.11¬
123500000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
123600000000                   -        (S/N)=N                        funz.13¬
123700000000                   -        (S/N)=N                        funz.14¬
123800000000                   -        (S/N)=N                        funz.15¬
123900000000                   -        (S/N)=N                        funz.16¬
124000000000                   -        (S/N)=N                        funz.17¬
124100000000                   -        (S/N)=N                        funz.18¬
124200000000                   -        (S/N)=N                        funz.19¬
124300000000                   -        (S/N)=N                        funz.20¬
124400000000                   -        (S/N)=N                        funz.21¬
124500000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
124600000000                   -        (S/N)=N                        funz.23¬
124700000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
