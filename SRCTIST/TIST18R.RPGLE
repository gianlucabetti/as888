000100000000     H*------------------------------------------------------------------------*
000200991029     H* STATISTICA VOLUMI - PARAMETRI
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800991029     FTIST18D   CF   E             WORKSTN INFDS(DEVDS1)
000900991229     F                                     SFILE(ST18DMS:MSGULT)
001000000000     FTABEL00F  IF   E           K DISK
001100000000     FAZORG01L  IF   E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* MESSAGGI
001700000000     D*-------------------
001800030910     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
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
006000020917     D*-------------------
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
008300991103     D PARAM           DS                  INZ
008400000000     D  PARTLA                 1      1
008500000000     D  PARANN                 2      5  0
008600000000     D  PARMMI                 6      7  0
008700000000     D  PARMMF                 8      9  0
008800000000     D  PARSNR                10     10
008900991029     D  PARPID                27     31  2
009000000000     D  PARDIV                32     32
009100000000     D  PARARE                33     35  0
009200000000     D  PARFIL                36     38  0
009300000000     D  PARDET                39     39
009400000000     D  PARTOT                40     40
009500991026     D  PARRG1                41     44  1
009600991026     D  PARRG2                45     48  1
009700991029     D  PARPIA                49     53  2
009800991029     D  PARPSD                54     58  2
009900991029     D  PARPSA                59     63  2
010000991103     D  PAROK                 65     66
010100991229     D  PARMIN                67     71  3
010200000706     D  PARPOS                72     72
010300030910     D  PARNT1                73     75
010400030910     D  PARNT2                76     78
010500030910     D  PARNT3                79     81
010600030910     D  PARNT4                82     84
010700050105     D  PARSAL                85     85
010800000000     D*-------------------
010900000000     D* DS PER GESTIONE TASTI FUNZIONALI
011000000000     D*-------------------
011100000000     D DSKEY         E DS
011200000000     D DEVDS1          DS
011300000000     D  KEY                  369    369
011400000000     D*-------------------
011500000000     D* ARCHITETTURA
011600000000     D*-------------------
011700000000     D KPJBA         E DS
011800000000     C*------------------------------------------------------------------------*
011900000000     C* MAIN LINE
012000000000     C*------------------------------------------------------------------------*
012100000000     C*
012200000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
012300000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
012400000000     C*
012500000000     C* CICLO FINO A CHE:
012600000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012700000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
012800000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
012900000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
013000000000     C     F12L00        OREQ      '1'
013100000000     C     FLFINE        OREQ      '1'
013200000000     C*
013300000000     C* EMETTE FORMATO VIDEO
013400000000     C                   EXSR      BUTFMR
013500000000     C*
013600000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
013700000000     C                   EXSR      FUNCON
013800000000     C*
013900000000     C                   END                                                    <-- 2 --
014000000000     C*
014100000000     C                   END                                                    <-- 1 --
014200000000     C*
014300000000     C                   SETON                                        LR
014400000000     C*------------------------------------------------------------------------*
014500000000     C* EMISSIONE FORMATO A VIDEO
014600000000     C*------------------------------------------------------------------------*
014700000000     C     BUTFMR        BEGSR
014800000000     C*
014900000000     C* WRITE RECORD TESTATA
015000991229     C                   WRITE     ST18D01
015100000000     C*
015200000000     C* VISUALIZZAZIONE MESSAGGIO
015300000000     C                   EXSR      SNDMSG
015400000000     C*
015500000000     C* WRITE RECORD FUNZIONI
015600991229     C                   WRITE     ST18D02
015700991019     C*
015800991019     C* EXFMT RECORD DETTAGLIO
015900991229     C                   EXFMT     ST18D03
016000991019     C*
016100000000     C* AZZERO FLAG DI ERRORE LIVELLO
016200000000     C                   MOVEL     '0'           ERRL00
016300000000     C*
016400000000     C* AZZERO FLAG DI ERRORE
016500991025     C                   SETOFF                                         7071
016600000000     C                   SETOFF                                       727377
016700050105     C                   SETOFF                                           76
016800000000     C                   SETOFF                                       787983
016900000000     C                   SETOFF                                       848586
017000950703     C                   SETOFF                                       818082
017100030910     C                   SETOFF                                       919293
017200030910     C                   SETOFF                                       94
017300000000     C*
017400000000     C                   ENDSR
017500000000     C*------------------------------------------------------------------------*
017600000000     C* SNDMSG - MANDA MESSAGGIO
017700000000     C*------------------------------------------------------------------------*
017800000000     C     SNDMSG        BEGSR
017900000000     C*
018000000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
018100000000     C                   MOVEL     '1'           *IN40
018200991229     C                   WRITE     ST18DMC
018300000000     C                   MOVEL     '0'           *IN40
018400000000     C*
018500000000     C* VISUALIZZAZIONE MESSAGGIO
018600000000     C                   Z-ADD     0             MSGULT            4 0
018700000000     C                   CALL      'XMSG'        XMSGPA
018800000000     C     DSMSER        IFEQ      0
018900000000     C                   MOVEL     DSMSKE        MSGKEY
019000000000     C                   END
019100000000     C                   Z-ADD     1             MSGULT
019200991229     C                   WRITE     ST18DMS                                      SFL MESS
019300991229     C                   WRITE     ST18DMC                                      CONTR MESS
019400000000     C*
019500000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
019600000000     C                   MOVEL     *BLANKS       DSMSMS
019700000000     C*
019800000000     C                   ENDSR
019900000000     C*------------------------------------------------------------------------*
020000000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
020100000000     C*------------------------------------------------------------------------*
020200000000     C     ENTGES        BEGSR
020300000000     C*
020400000000     C* CONTROLLA I CAMPI IMMESSI
020500000000     C                   EXSR      DECODI
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* F01GES -GESTIONE TASTO FUNZIONALE F01 -
021000000000     C*------------------------------------------------------------------------*
021100000000     C     F01GES        BEGSR
021200000000     C                   ENDSR
021300000000     C*------------------------------------------------------------------------*
021400000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
021500000000     C*------------------------------------------------------------------------*
021600000000     C     F03GES        BEGSR
021700000000     C*
021800000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
021900000000     C                   MOVEL     '1'           F03L00
022000000000     C*
022100000000     C                   ENDSR
022200000000     C*------------------------------------------------------------------------*
022300000000     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA
022400000000     C*------------------------------------------------------------------------*
022500991020     C     F06GES        BEGSR
022600991105     C*
022700991105     C                   EXSR      DECODI
022800991105     C* VEDO SE A VIDEO TUTTO OK ALTRIMETI LA CONFERMA NON E' VALIDA
022900991105     C     ERRL00        IFEQ      '0'
023000991103     C*
023100991104     C* CONSIDERAZIONI FONDAMENTALI X L'AGGIORNAMENTO AUTOM. TARIFFE
023200991104     C*
023300991104     C     VICSIM        IFEQ      'S'                                          NO AGG. TARIFFE
023400991104     C*
023500991104     C* LANCIO IL PROGRAMMA DI AGGIORNAMENTO E/O STAMPA
023600991104     C                   MOVEL     'ST19'        KCOAZ
023700991104     C                   EXSR      VIDPAR
023800991104     C                   MOVEL     PARAM         KPJBU
023900030910     C                   CALL      'BCH10'
024000030910 MOM C*                  CALL      'TIST19C'
024100991104     C                   PARM                    KPJBA
024200991104     C                   ELSE                                                   SI AGG. TARIFFE
024300991104     C*
024400991103     C     *IN20         IFEQ      *OFF
024500991103     C*
024600991104     C* SI AGGIORNAMENTO TARIFFE AUTOM. => CONFERMA 'NO' PER SICUREZZA
024700991104     C                   MOVEL     'NO'          VIDOK
024800991103     C                   SETOFF                                       KF
024900991103     C                   SETON                                        20
025000991103     C*
025100991103     C     *INKC         DOWEQ     '0'
025200991103     C     *INKF         ANDEQ     '0'
025300991103     C*
025400991103     C* EXFMT WINDOW CONFERMA
025500991229     C                   EXFMT     ST18DWW
025600991103     C*
025700991103     C                   ENDDO
025800000000     C*
025900991103     C     *INKF         IFEQ      '1'
026000991103     C* LANCIO IL PROGRAMMA DI AGGIORNAMENTO E/O STAMPA
026100991103     C                   MOVEL     'ST19'        KCOAZ
026200991103     C                   EXSR      VIDPAR
026300991103     C                   MOVEL     PARAM         KPJBU
026400991124     C                   CALL      'BCH10'
026500991124 MOM C*                    CALL 'TIST19C'
026600991103     C                   PARM                    KPJBA
026700991103     C                   ENDIF
026800991103     C                   SETOFF                                       20
026900991103     C*
027000991103     C                   ENDIF
027100991104     C*
027200991104     C                   ENDIF
027300991124     C*
027400991124     C* RIMETTE FILE DA RIGENERARE = 'N' PER SICUREZZA
027500991124     C                   MOVEL     'N'           VIDSNR
027600050105     C                   MOVEL     'N'           VIDSal
027700991105     C*
027800991105     C                   ENDIF
027900000000     C*
028000000000     C                   ENDSR
028100000000     C*------------------------------------------------------------------------*
028200000000     C* F07GES -GESTIONE TASTO FUNZIONALE F07 -
028300000000     C*------------------------------------------------------------------------*
028400000000     C     F07GES        BEGSR
028500000000     C                   ENDSR
028600000000     C*------------------------------------------------------------------------*
028700000000     C* F08GES -GESTIONE TASTO FUNZIONALE F08 -
028800000000     C*------------------------------------------------------------------------*
028900000000     C     F08GES        BEGSR
029000000000     C                   ENDSR
029100000000     C*------------------------------------------------------------------------*
029200000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
029300000000     C*------------------------------------------------------------------------*
029400000000     C     F12GES        BEGSR
029500000000     C*
029600000000     C* IMPOSTO FLAG DI LIVELLO
029700991103     C                   MOVEL     '1'           F12L00
029800000000     C*
029900000000     C                   ENDSR
030000000000     C*------------------------------------------------------------------------*
030100000000     C* F21GES -GESTIONE TASTO FUNZIONALE F21 -
030200000000     C*------------------------------------------------------------------------*
030300000000     C     F21GES        BEGSR
030400000000     C                   ENDSR
030500000000     C*------------------------------------------------------------------------*
030600000000     C* F22GES -GESTIONE TASTO FUNZIONALE F22 - GEST.STAMPE
030700000000     C*------------------------------------------------------------------------*
030800000000     C     F22GES        BEGSR
030900000000     C*
031000991020     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
031100000000     C                   Z-ADD     80            LENGH            15 5
031200000000     C                   CALL      'QCAEXEC'
031300000000     C                   PARM                    CMD(1)
031400000000     C                   PARM                    LENGH
031500000000     C*
031600000000     C                   ENDSR
031700000000     C*------------------------------------------------------------------------*
031800991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
031900000000     C*------------------------------------------------------------------------*
032000000000     C     F24GES        BEGSR
032100000000     C*
032200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
032300000000     C                   EXSR      FUNGES
032400000000     C*
032500000000     C                   ENDSR
032600000000     C*------------------------------------------------------------------------*
032700000000     C* HELGES -GESTIONE TASTO FUNZIONALE HELP
032800000000     C*------------------------------------------------------------------------*
032900000000     C     HELGES        BEGSR
033000000000     C                   ENDSR
033100991020     C*------------------------------------------------------------------------*
033200991103     C*  DECODI - CONTROLLA CAMPI E DECODIFICA
033300000000     C*------------------------------------------------------------------------*
033400000000     C     DECODI        BEGSR
033500000000     C*
033600000000     C* CONTROLLO ANNO
033700991020     C     ERRL00        IFEQ      '0'
033800991020     C     VIDANN        IFEQ      *ZEROS
033900000000     C                   MOVEL     '1'           ERRL00
034000000000     C                   MOVEL     MSG(3)        DSMSMS
034100000000     C                   SETON                                        70
034200991020     C                   ELSE
034300991020     C     VIDANN        IFLT      1900
034400000000     C                   MOVEL     '1'           ERRL00
034500000000     C                   MOVEL     MSG(3)        DSMSMS
034600000000     C                   SETON                                        70
034700991020     C                   END
034800991020     C                   END
034900991020     C                   END
035000000000     C*
035100000000     C* CONTROLLO MESE INIZIO / FINE
035200000000     C     ERRL00        IFEQ      '0'
035300000000     C     VIDMMI        IFEQ      *ZEROS                                       *INIZIO
035400000000     C     VIDMMI        ORGT      12
035500000000     C                   MOVEL     '1'           ERRL00
035600000000     C                   MOVEL     MSG(4)        DSMSMS
035700000000     C                   SETON                                        71
035800000000     C                   END
035900000000     C                   END
036000000000     C     ERRL00        IFEQ      '0'
036100000000     C     VIDMMF        IFEQ      *ZEROS                                       *FINE
036200000000     C                   Z-ADD     VIDMMI        VIDMMF                         *LO IMPOSTO COME INI
036300000000     C                   ELSE
036400000000     C     VIDMMF        IFGT      12
036500000000     C                   MOVEL     '1'           ERRL00
036600000000     C                   MOVEL     MSG(5)        DSMSMS
036700000000     C                   SETON                                        72
036800000000     C                   END
036900000000     C                   END
037000000000     C                   END
037100000000     C     ERRL00        IFEQ      '0'                                          *INIZIO MAGGIORE FIN
037200000000     C     VIDMMF        IFNE      *ZEROS
037300000000     C     VIDMMI        IFGT      VIDMMF
037400000000     C                   MOVEL     '1'           ERRL00
037500000000     C                   MOVEL     MSG(6)        DSMSMS
037600000000     C                   SETON                                        7172
037700000000     C                   END
037800000000     C                   END
037900000000     C                   END
038000000000     C*
038100000000     C* CONTROLLO FLAG FILE DA RIGENERARE
038200000000     C     ERRL00        IFEQ      '0'
038300000000     C     VIDSNR        IFEQ      'S'
038400000000     C     VIDSNR        OREQ      'N'
038500000000     C                   ELSE
038600000000     C                   MOVEL     '1'           ERRL00
038700000000     C                   MOVEL     MSG(11)       DSMSMS
038800000000     C                   SETON                                        77
038900000000     C                   END
039000000000     C                   END
039100050105     C*
039200050105     C* CONTROLLO FLAG FILE DA RIGENERARE saldi x cliente
039300050105     C     ERRL00        IFEQ      '0'
039400050105     C     VIDSal        IFEQ      'S'
039500050105     C     VIDSal        OREQ      'N'
039600050105     C                   ELSE
039700050105     C                   MOVEL     '1'           ERRL00
039800050105     C                   MOVEL     MSG(11)       DSMSMS
039900050105     C                   SETON                                        76
040000050105     C                   END
040100050105     C                   END
040200000000     C*
040300991029     C* CONTROLLO PERCENTUALI
040400000000     C     ERRL00        IFEQ      '0'
040500991103     C     VIDPID        IFLT      *ZEROS
040600991103     C     VIDPID        ORGT      100
040700000000     C                   MOVEL     '1'           ERRL00
040800991103     C                   MOVEL     MSG(15)       DSMSMS
040900000000     C                   SETON                                        86
041000000000     C                   END
041100000000     C                   END
041200991029     C*
041300991025     C     ERRL00        IFEQ      '0'
041400991103     C     VIDPSD        IFLT      *ZEROS
041500991103     C     VIDPSD        ORGT      100
041600991025     C                   MOVEL     '1'           ERRL00
041700991103     C                   MOVEL     MSG(15)       DSMSMS
041800991025     C                   SETON                                        87
041900991025     C                   END
042000991025     C                   END
042100991029     C*
042200991029     C* CONTROLLO RANGE INIZ/FIN %  RAPP. E SCOST VOL E COLLI VARI
042300991020     C     ERRL00        IFEQ      '0'
042400991020     C     VIDRG1        IFGT      VIDRG2
042500991020     C                   MOVEL     '1'           ERRL00
042600991020     C                   MOVEL     MSG(7)        DSMSMS
042700991020     C                   SETON                                        8485
042800991020     C                   END
042900991020     C                   END
043000991103     C*
043100000000     C* CONTROLLO LA DIVISIONE
043200000000     C                   MOVEL     *BLANKS       VDDDIV
043300000000     C     ERRL00        IFEQ      '0'
043400000000     C     VIDDIV        IFNE      *BLANKS
043500000000     C                   EXSR      CHKDIV
043600000000     C     ERRL00        IFEQ      '1'
043700000000     C                   MOVEL     '1'           ERRL00
043800000000     C                   MOVEL     MSG(12)       DSMSMS
043900000000     C                   SETON                                        78
044000000000     C                   END
044100000000     C                   END
044200000000     C                   END
044300000000     C*
044400000000     C* CONTROLLO L'AREA
044500000000     C                   MOVEL     *BLANKS       VDDARE
044600000000     C     ERRL00        IFEQ      '0'
044700000000     C     VIDARE        IFNE      *BLANKS
044800000000     C                   EXSR      CHKARE
044900000000     C     ERRL00        IFEQ      '1'
045000000000     C                   MOVEL     '1'           ERRL00
045100000000     C                   MOVEL     MSG(13)       DSMSMS
045200000000     C                   SETON                                        79
045300000000     C                   END
045400000000     C                   END
045500000000     C                   END
045600000000     C*
045700000000     C* CONTROLLO LA FILIALE
045800000000     C                   MOVEL     *BLANKS       VDDFLP
045900000000     C     ERRL00        IFEQ      '0'
046000000000     C     VIDFLP        IFNE      *BLANKS
046100000000     C                   MOVE      VIDFLP        WLIN
046200000000     C                   EXSR      CHKLIN
046300000000     C     ERRL00        IFEQ      '0'
046400000000     C                   MOVEL     WLIN          VIDFLP
046500000000     C                   MOVEL     WDLIN         VDDFLP
046600000000     C                   ELSE
046700000000     C                   SETON                                        80
046800000000     C                   END
046900000000     C                   END
047000000000     C                   END
047100030910     C*
047200030910     C* CONTROLLO 1° NETWORK TARIFFA
047300030910     C     ERRL00        IFEQ      '0'
047400030910     C     VIDNT1        IFNE      *BLANKS
047500030910     C                   IF        VIDNT1 <> 'ITA' AND
047600030910     C                             VIDNT1 <> 'FED' AND
047700030910     C                             VIDNT1 <> 'EEX' AND
047800030910     C                             VIDNT1 <> 'DPD'
047900030910     C                   MOVEL     '1'           ERRL00
048000030910     C                   MOVEL     MSG(21)       DSMSMS
048100030910     C                   SETON                                        91
048200030910     C                   ENDIF
048300030910     C                   ENDIF
048400030910     C                   ENDIF
048500030910     C*
048600030910     C* CONTROLLO 2° NETWORK TARIFFA
048700030910     C     ERRL00        IFEQ      '0'
048800030910     C     VIDNT2        IFNE      *BLANKS
048900030910     C                   IF        VIDNT2 <> 'ITA' AND
049000030910     C                             VIDNT2 <> 'FED' AND
049100030910     C                             VIDNT2 <> 'EEX' AND
049200030910     C                             VIDNT2 <> 'DPD'
049300030910     C                   MOVEL     '1'           ERRL00
049400030910     C                   MOVEL     MSG(21)       DSMSMS
049500030910     C                   SETON                                        92
049600030910     C                   ENDIF
049700030910     C                   ENDIF
049800030910     C                   ENDIF
049900030910     C*
050000030910     C* CONTROLLO 3° NETWORK TARIFFA
050100030910     C     ERRL00        IFEQ      '0'
050200030910     C     VIDNT3        IFNE      *BLANKS
050300030910     C                   IF        VIDNT3 <> 'ITA' AND
050400030910     C                             VIDNT3 <> 'FED' AND
050500030910     C                             VIDNT3 <> 'EEX' AND
050600030910     C                             VIDNT3 <> 'DPD'
050700030910     C                   MOVEL     '1'           ERRL00
050800030910     C                   MOVEL     MSG(21)       DSMSMS
050900030910     C                   SETON                                        93
051000030910     C                   ENDIF
051100030910     C                   ENDIF
051200030910     C                   ENDIF
051300030910     C*
051400030910     C* CONTROLLO 4° NETWORK TARIFFA
051500030910     C     ERRL00        IFEQ      '0'
051600030910     C     VIDNT4        IFNE      *BLANKS
051700030910     C                   IF        VIDNT4 <> 'ITA' AND
051800030910     C                             VIDNT4 <> 'FED' AND
051900030910     C                             VIDNT4 <> 'EEX' AND
052000030910     C                             VIDNT4 <> 'DPD'
052100030910     C                   MOVEL     '1'           ERRL00
052200030910     C                   MOVEL     MSG(21)       DSMSMS
052300030910     C                   SETON                                        94
052400030910     C                   ENDIF
052500030910     C                   ENDIF
052600030910     C                   ENDIF
052700000000     C*
052800000000     C                   ENDSR
052900000000     C*------------------------------------------------------------------------*
053000000000     C*  CHKDIV - CONTROLLA LA DIVISIONE
053100000000     C*------------------------------------------------------------------------*
053200000000     C     CHKDIV        BEGSR
053300000000     C*
053400000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
053500000000     C     VIDDIV        IFEQ      '?'                                          --- 1 -->
053600000000     C                   MOVEL     CODUT         §KUT              1 0
053700000000     C                   MOVEL     '17'          §COD              2
053800000000     C                   MOVE      *BLANKS       §DES1
053900000000     C                   CALL      'X§TABER'
054000000000     C                   PARM                    §KUT
054100000000     C                   PARM                    §COD
054200000000     C                   PARM                    §KEY              8
054300970401     C                   PARM                    §DES             25
054400000000     C                   MOVEL     §KEY          VIDDIV
054500000000     C                   MOVEL     §DES          VDDDIV
054600000000     C*
054700000000     C                   ELSE                                                   <-- 1 -->
054800000000     C*
054900000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
055000000000     C                   MOVEL     CODUT         KTBUTE
055100000000     C                   MOVE      '17'          KTBCOD
055200000000     C                   MOVE      *BLANKS       KTBKEY
055300000000     C                   MOVEL     VIDDIV        KTBKEY
055400000000     C     KEYTAB        CHAIN     TABEL                              99
055500000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
055600000000     C     TBLFLG        OREQ      '*'
055700000000     C                   MOVEL     '1'           ERRL00
055800000000     C                   MOVEL     *ALL'*'       VDDDIV
055900000000     C                   ELSE                                                   <-- 2 -->
056000970401     C                   MOVEL     TBLUNI        DS17
056100970401     C                   MOVEL     §17DES        VDDDIV
056200000000     C                   END                                                    <-- 2 ---
056300000000     C*
056400000000     C                   END                                                    <-- 1 ---
056500000000     C*
056600000000     C                   ENDSR
056700000000     C*------------------------------------------------------------------------*
056800000000     C*  CHKARE - CONTROLLA L'AREA
056900000000     C*------------------------------------------------------------------------*
057000000000     C     CHKARE        BEGSR
057100000000     C*
057200000000     C* CONTROLLO SE E' STATO INSERITO '?'
057300000000     C     '?'           SCAN      VIDARE        A                 1 0    99
057400000000     C*
057500000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. TABELLE
057600000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
057700000000     C                   MOVEL     CODUT         §KUT              1 0
057800000000     C                   MOVEL     '05'          §COD              2
057900000000     C                   MOVE      *BLANKS       §DES1
058000000000     C                   CALL      'X§TABER'
058100000000     C                   PARM                    §KUT
058200000000     C                   PARM                    §COD
058300000000     C                   PARM                    §KEY              8
058400970401     C                   PARM                    §DES             25
058500000000     C                   MOVEL     §KEY          VIDARE
058600000000     C                   MOVEL     §DES          VDDARE
058700000000     C*
058800000000     C                   ELSE                                                   <-- 1 -->
058900000000     C*
059000000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
059100000000     C                   MOVEL     CODUT         KTBUTE
059200000000     C                   MOVE      '05'          KTBCOD
059300000000     C                   MOVE      *BLANKS       KTBKEY
059400000000     C                   MOVEL     VIDARE        KTBKEY
059500000000     C     KEYTAB        CHAIN     TABEL                              99
059600000000     C     *IN99         IFEQ      '1'                                          --- 2 -->
059700000000     C     TBLFLG        OREQ      '*'
059800000000     C                   MOVEL     '1'           ERRL00
059900000000     C                   MOVEL     *ALL'*'       VDDARE
060000000000     C                   ELSE                                                   <-- 2 -->
060100970401     C                   MOVEL     TBLUNI        DS05
060200970401     C                   MOVEL     §05DES        VDDARE
060300000000     C                   END                                                    <-- 2 ---
060400000000     C*
060500000000     C                   END                                                    <-- 1 ---
060600000000     C*
060700000000     C                   ENDSR
060800000000     C*------------------------------------------------------------------------*
060900000000     C*  CHKLIN - CONTROLLA FILIALI E TERMINAL
061000000000     C*------------------------------------------------------------------------*
061100000000     C     CHKLIN        BEGSR
061200000000     C*
061300000000     C* CONTROLLO SE E' STATO INSERITO '?'
061400000000     C     '?'           SCAN      WLIN          A                 1 0    99
061500000000     C*
061600000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROG. LINEE ARRIVO
061700000000IF  1C     *IN99         IFEQ      '1'                                          --- 1 -->
061800000000     C                   MOVEL     *BLANKS       §COD1             3
061900000000     C                   MOVE      *BLANKS       §DES1
062000971203     C                   MOVE      *BLANKS       §PRA
062100971203     C                   CALL      'TNSD24R'
062200000000     C                   PARM                    §COD1
062300000000     C                   PARM                    §TIP              1
062400000000     C                   PARM                    §DES1            25
062500971203     C                   PARM                    §PRA              3
062600000000     C                   MOVEL     §COD1         WLIN              3
062700000000     C                   MOVEL     §DES1         WDLIN            20
062800000000     C*
062900000000X   1C                   ELSE
063000000000     C*
063100000000     C* CARATTERI ALFABETICI NON VALIDI
063200000000IF  2C     WLIN          IFLT      *ZEROS
063300000000     C                   MOVEL     '1'           ERRL00
063400000000     C                   MOVEL     MSG(14)       DSMSMS
063500000000     C                   MOVEL     *ALL'*'       WDLIN
063600000000X   2C                   ELSE
063700000000     C*
063800000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN AZORG
063900000000     C                   MOVE      WLIN          KORFIL
064000000000     C     KEYORG        CHAIN     AZORG                              99
064100000000IF  3C     *IN99         IFEQ      '0'                                          --- 2 -->
064200000000     C                   MOVEL     ORGDES        WDLIN
064300000000X   3C                   ELSE                                                   <-- 2 -->
064400000000     C                   MOVEL     '1'           ERRL00
064500000000     C                   MOVEL     MSG(14)       DSMSMS
064600000000     C                   MOVEL     *ALL'*'       WDLIN
064700000000E   3C                   END                                                    <-- 2 ---
064800000000IF  3C     ORGFAG        IFEQ      'V'                                          --- 2 -->
064900000000     C                   MOVEL     '1'           ERRL00
065000000000     C                   MOVEL     MSG(14)       DSMSMS
065100000000     C                   MOVEL     *ALL'*'       WDLIN
065200000000E   3C                   END                                                    <-- 2 ---
065300000000E   2C                   END                                                    <-- 2 ---
065400000000E   1C                   ENDIF
065500000000     C*
065600000000     C                   ENDSR
065700000000     C*------------------------------------------------------------------------*
065800000000     C*  AZZVID - AZZERA O ABBLENCA O IMPOSTA TUTTI I CAMPI DEL FILE VIDEO
065900000000     C*------------------------------------------------------------------------*
066000000000     C     AZZVID        BEGSR
066100000000     C*
066200000000     C                   Z-ADD     *ZEROS        VIDANN
066300000000     C                   Z-ADD     *ZEROS        VIDMMI
066400000000     C                   Z-ADD     *ZEROS        VIDMMF
066500000000     C                   MOVEL     'N'           VIDSNR
066600050105     C                   MOVEL     'N'           VIDSal
066700950704     C                   MOVEL     'S'           VICDET
066800000000     C                   MOVEL     'N'           VICTOT
066900000706     C                   MOVEL     'N'           VICPOS
067000991104     C                   MOVEL     'S'           VICSIM
067100991020     C*-------                                                         --------*
067200991020     C* REP. VALORI LIMITE TABELLATI PER DEFAULT
067300991020     C*-------                                                         --------*
067400991020     C                   MOVEL     CODUT         KTBUTE
067500991020     C                   MOVEL     'TP'          KTBCOD
067600991020     C     KE2TAB        CHAIN     TABEL00F                           99
067700991020     C     *IN99         IFEQ      *ON
067800991020     C     TBLFLG        OREQ      '*'
067900991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI GENERICI=1 e =100
068000991029     C                   Z-ADD     10            VIDPID
068100991029     C                   Z-ADD     10            VIDPSD
068200991029     C                   Z-ADD     -999          VIDRG1
068300991029     C                   Z-ADD     999           VIDRG2
068400991020     C                   ELSE                                                   <-- 2 -->
068500991020     C                   MOVEL     TBLUNI        DSTP                                       NTO BOLL
068600991020     C* IMPOSTO CAMPI A VIDEO CON PARAMETRI TABELLATI
068700991029     C                   Z-ADD     §TVDPD        VIDRG1
068800991026     C     VIDRG1        MULT      -1            VIDRG1
068900991029     C                   Z-ADD     §TVDPA        VIDRG2
069000991029     C                   Z-ADD     §TVCID        VIDPID
069100991029     C                   Z-ADD     §TVCSD        VIDPSD
069200991229     C                   Z-ADD     §TVMIN        VIDMIN
069300991029     C                   END                                                    <-- 2 ---
069400000000     C*
069500000000     C                   ENDSR
069600000000     C*------------------------------------------------------------------------*
069700991020     C*  VIDPAR - IMPOSTA I PARAMETRI DI LANCIO DAL VIDEO
069800000000     C*------------------------------------------------------------------------*
069900000000     C     VIDPAR        BEGSR
070000000000     C*
070100000000     C                   Z-ADD     VIDANN        PARANN
070200000000     C                   Z-ADD     VIDMMI        PARMMI
070300000000     C                   Z-ADD     VIDMMF        PARMMF
070400000000     C                   MOVEL     VIDSNR        PARSNR
070500050105     C                   MOVEL     VIDsal        PARsal
070600000000     C                   MOVEL     VIDDIV        PARDIV
070700000000     C                   MOVE      VIDARE        PARARE
070800000000     C                   MOVE      VIDFLP        PARFIL
070900991029     C                   Z-ADD     VIDPID        PARPID
071000991029     C                   Z-ADD     VIDPSD        PARPSD
071100000000     C                   MOVEL     VICDET        PARDET
071200000000     C                   MOVEL     VICTOT        PARTOT
071300991020     C                   Z-ADD     VIDRG1        PARRG1
071400991020     C                   Z-ADD     VIDRG2        PARRG2
071500991229     C                   Z-ADD     VIDMIN        PARMIN
071600991104     C                   MOVEL     VIDOK         PAROK
071700000706     C                   MOVEL     VICPOS        PARPOS
071800030910     C                   MOVEL     VIDNT1        PARNT1
071900030910     C                   MOVEL     VIDNT2        PARNT2
072000030910     C                   MOVEL     VIDNT3        PARNT3
072100030910     C                   MOVEL     VIDNT4        PARNT4
072200000000     C*
072300000000     C                   ENDSR
072400020917     C*--------------------------------------------------------------------------------------------*
072500020917     C* REPERISCE I DATI UTENTE
072600020917     C*--------------------------------------------------------------------------------------------*
072700020917     C     REPDATIUTE    BEGSR
072800020917     C*
072900020917     C* INIZIALIZZA VARIABILI DI WRK
073000020917     C                   CLEAR                   TIBS34DS
073100020917     C                   CLEAR                   AZUTEDS
073200020917     C                   CLEAR                   DDATIUTE
073300020917     C*
073400020917     C     *DTAARA       DEFINE    §azute        azuteds
073500020917     C     *DTAARA       DEFINE    §datiute      ddatiute
073600020917     C                   IN(E)     *DTAARA
073700020917if  1C                   IF        %Error
073800020917     c                   EVAL      I34Tla = 'L'
073900020917     C                   CALL      'TIBS34R'
074000020917     C                   PARM                    Tibs34Ds
074100020917     C                   IN        *DTAARA
074200020917e   1C                   ENDIF
074300020917     C*
074400020917     C                   ENDSR
074500000000     C*------------------------------------------------------------------------*
074600980403     C* *INZSR - OPERAZIONI INIZIALI
074700000000     C*------------------------------------------------------------------------*
074800980403     C     *INZSR        BEGSR
074900000000     C*
075000000000     C* RICEVIMENTO PARAMETRI
075100000000     C     *ENTRY        PLIST
075200000000     C                   PARM                    KPJBA
075300000000     C*
075400000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
075500000000     C     KEYORG        KLIST
075600000000     C                   KFLD                    KORFIL
075700000000     C*
075800000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
075900000000     C     KEYTAB        KLIST
076000000000     C                   KFLD                    KTBUTE
076100000000     C                   KFLD                    KTBCOD
076200000000     C                   KFLD                    KTBKEY
076300980403     C*
076400980403     C* CHIAVE LETTURA TABEL00F - PARZIALE
076500980403     C     KE2TAB        KLIST
076600980403     C                   KFLD                    KTBUTE
076700980403     C                   KFLD                    KTBCOD
076800000000     C*
076900000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
077000000000     C     XMSGPA        PLIST
077100000000     C                   PARM                    DSMSMS
077200000000     C                   PARM                    DSMSPG
077300000000     C                   PARM                    DSMSKE
077400000000     C                   PARM                    DSMSER
077500000000     C*
077600000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
077700000000     C                   CALL      'XKEY'
077800000000     C                   PARM                    DSKEY
077900000000     C*
078000000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
078100000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
078200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
078300000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE
078400000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
078500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
078600000000     C*
078700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
078800000000     C*   -NOME PGM CHIAMANTE
078900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
079000991029     C                   MOVEL     'TIST18R'     DSMSPG
079100991029     C                   MOVEL     'TIST18R'     MSGQUE
079200000000     C                   MOVEL     *BLANKS       MSGKEY
079300000000     C                   MOVEL     *BLANKS       DSMSMS
079400000000     C*
079500000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
079600000000     C                   Z-ADD     0             FUNGRU            5 0
079700000000     C*
079800000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
079900000000     C                   EXSR      FUNGES
080000000000     C*
080100000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
080200000000     C                   MOVEL     '0'           F12L00            1
080300000000     C                   MOVEL     '0'           F03L00            1
080400000000     C                   MOVEL     '0'           FINL00            1
080500000000     C                   MOVEL     '0'           ERRL00            1
080600000000     C                   MOVEL     '0'           FLFINE            1
080700000000     C*
080800000000     C* LANCIO ROUTINE PER VERIFICARE SE
080900000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
081000000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
081100020917     C                   EXSR      REPDATIUTE
081200020917     C                   Z-ADD     1             CODUT             1 0
081300000000     C*
081400000000     C* IMPOSTO I VALORI DI DEFAULT A VIDEO
081500000000     C                   EXSR      AZZVID
081501150409     c* se profilo <> da EDP proteggo i campi per la rigenerazione dei file
081502150409     c                   setoff                                       75
081503150409     c                   if        %subst(knmus:1:3)<>'EDP'
081504150409     c                   seton                                        75
081505150409     c                   endif
081600000000     C*
081700000000     C                   ENDSR
081800000000     C*------------------------------------------------------------------------*
081900000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
082000000000     C*------------------------------------------------------------------------*
082100000000     C     FUNGES        BEGSR
082200000000     C*
082300000000     C* PULISCO CAMPI DI LAVORO
082400000000     C                   Z-ADD     0             W                 5 0
082500000000     C*
082600000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
082700000000     C* E CARICO TABELLA FUNZIONI ATTIVE
082800000000     C     1             DO        24            I
082900000000     C                   MOVEL     FUD(I)        FUDRIG
083000000000     C*
083100000000     C* PULIZIA FUNZIONI PRECEDENTI
083200000000     C                   MOVEL     *BLANKS       FUV(I)
083300000000     C*
083400000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
083500000000     C* (SALTA F24)
083600000000     C     FUDATT        IFEQ      'S'
083700000000     C     FUDFUN        ANDNE     '24'
083800000000     C                   ADD       1             W
083900000000     C                   MOVEL     FUDDES        FUV(W)
084000000000     C                   END
084100000000     C                   END                                                    DO WHILE
084200000000     C*
084300000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
084400000000     C* ALTRIMENTI DISATTIVO F24
084500000000     C     1             DO        24            I
084600000000     C                   MOVEL     FUD(I)        FUDRIG
084700000000     C*
084800000000     C     FUDFUN        IFEQ      '24'
084900000000     C     W             IFGT      6
085000000000     C                   MOVEL     'S'           FUDATT
085100000000     C                   ELSE
085200000000     C                   MOVEL     *BLANKS       FUDATT
085300000000     C                   END                                                    W GT 8
085400000000     C                   MOVEL     FUDRIG        FUD(I)
085500000000     C                   END
085600000000     C                   END                                                    DO WHILE
085700000000     C*
085800000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
085900000000     C     W             DIV       6             I
086000000000     C                   MVR                     Z
086100000000     C*
086200000000     C     Z             IFNE      0
086300000000     C     I             ADD       1             Z
086400000000     C                   ELSE
086500000000     C                   Z-ADD     I             Z                 5 0
086600000000     C                   END
086700000000     C*
086800000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
086900000000     C                   ADD       1             FUNGRU
087000000000     C     FUNGRU        IFGT      Z
087100000000     C     FUNGRU        ORLT      1
087200000000     C                   Z-ADD     1             FUNGRU
087300000000     C                   END
087400000000     C*
087500000000     C* PRIMO GRUPPO DI 6 FUNZIONI
087600000000     C     FUNGRU        IFEQ      1
087700000000     C                   MOVEL     FUV(1)        FUN001
087800000000     C                   MOVEL     FUV(2)        FUN002
087900000000     C                   MOVEL     FUV(3)        FUN003
088000000000     C                   MOVEL     FUV(4)        FUN004
088100000000     C                   MOVEL     FUV(5)        FUN005
088200000000     C                   MOVEL     FUV(6)        FUN006
088300000000     C                   END
088400000000     C*
088500000000     C* SECONDO GRUPPO DI 6 FUNZIONI
088600000000     C     FUNGRU        IFEQ      2
088700000000     C                   MOVEL     FUV(7)        FUN001
088800000000     C                   MOVEL     FUV(8)        FUN002
088900000000     C                   MOVEL     FUV(9)        FUN003
089000000000     C                   MOVEL     FUV(10)       FUN004
089100000000     C                   MOVEL     FUV(11)       FUN005
089200000000     C                   MOVEL     FUV(12)       FUN006
089300000000     C                   END
089400000000     C*
089500000000     C* TERZO GRUPPO DI 6 FUNZIONI
089600000000     C     FUNGRU        IFEQ      3
089700000000     C                   MOVEL     FUV(13)       FUN001
089800000000     C                   MOVEL     FUV(14)       FUN002
089900000000     C                   MOVEL     FUV(15)       FUN003
090000000000     C                   MOVEL     FUV(16)       FUN004
090100000000     C                   MOVEL     FUV(17)       FUN005
090200000000     C                   MOVEL     FUV(18)       FUN006
090300000000     C                   END
090400000000     C*
090500000000     C* QUARTO GRUPPO DI 6 FUNZIONI
090600000000     C     FUNGRU        IFEQ      4
090700000000     C                   MOVEL     FUV(19)       FUN001
090800000000     C                   MOVEL     FUV(20)       FUN002
090900000000     C                   MOVEL     FUV(21)       FUN003
091000000000     C                   MOVEL     FUV(22)       FUN004
091100000000     C                   MOVEL     *BLANKS       FUN005
091200000000     C                   MOVEL     *BLANKS       FUN006
091300000000     C                   END
091400000000     C*
091500000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
091600000000     C                   MOVEL     *BLANKS       FUN007
091700000000     C                   MOVEL     *BLANKS       FUN008
091800000000     C*
091900000000     C     1             DO        24            I
092000000000     C                   MOVEL     FUD(I)        FUDRIG
092100000000     C     FUDFUN        IFEQ      '24'
092200000000     C     FUDATT        ANDEQ     'S'
092300000000     C                   MOVEL     FUDDES        FUN008
092400000000     C                   END
092500000000     C                   END                                                    DO
092600000000     C                   ENDSR
092700000000     C*------------------------------------------------------------------------*
092800000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
092900000000     C*------------------------------------------------------------------------*
093000000000     C     FUNCON        BEGSR
093100000000     C*
093200000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
093300000000     C                   EXSR      FUNVAR
093400000000     C*
093500000000     C* PULISCO CAMPI DI LAVORO
093600000000     C                   Z-ADD     0             I                 5 0
093700000000     C                   Z-ADD     0             W                 5 0
093800000000     C*
093900000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
094000000000     C     FUNKEY        IFNE      *BLANKS
094100000000     C     1             DO        24            I
094200000000     C                   MOVEL     FUD(I)        FUDRIG
094300000000     C*
094400000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
094500000000     C     FUNKEY        IFEQ      FUDFUN
094600000000     C     FUDATT        ANDEQ     'S'
094700000000     C                   Z-ADD     I             W
094800000000     C                   END
094900000000     C                   END                                                    DO WHILE
095000000000     C*
095100000000     C* FUNZIONE NON ATTIVA (ERRORE)
095200000000     C     W             IFEQ      0
095300000000     C*
095400000000     C* EMISSIONE MESSAGGIO DI ERRORE
095500000000     C                   MOVEL     MSG(1)        DSMSMS
095600000000     C*
095700000000     C                   END                                                    W=0
095800000000     C                   END                                                    FUNKEY NE *BLANKS
095900000000     C*
096000000000     C* FUNZIONE ATTIVA (PROCEDI ...)
096100000000     C     W             IFNE      0
096200000000     C     FUNKEY        OREQ      *BLANKS
096300000000     C*
096400000000     C     FUNKEY        IFEQ      *BLANKS
096500000000     C                   EXSR      ENTGES
096600000000     C                   END
096700000000     C     FUNKEY        IFEQ      '01'
096800000000     C                   EXSR      F01GES
096900000000     C                   END
097000000000     C     FUNKEY        IFEQ      '03'
097100000000     C                   EXSR      F03GES
097200000000     C                   END
097300000000     C     FUNKEY        IFEQ      '06'
097400000000     C                   EXSR      F06GES
097500000000     C                   END
097600000000     C     FUNKEY        IFEQ      '07'
097700000000     C                   EXSR      F07GES
097800000000     C                   END
097900000000     C     FUNKEY        IFEQ      '08'
098000000000     C                   EXSR      F08GES
098100000000     C                   END
098200000000     C     FUNKEY        IFEQ      '12'
098300000000     C                   EXSR      F12GES
098400000000     C                   END
098500000000     C     FUNKEY        IFEQ      '21'
098600000000     C                   EXSR      F21GES
098700000000     C                   END
098800000000     C     FUNKEY        IFEQ      '22'
098900000000     C                   EXSR      F22GES
099000000000     C                   END
099100000000     C     FUNKEY        IFEQ      '24'
099200000000     C                   EXSR      F24GES
099300000000     C                   END
099400000000     C     FUNKEY        IFEQ      'HP'
099500000000     C                   EXSR      HELGES
099600000000     C                   END
099700000000     C*
099800000000     C                   END
099900000000     C*
100000000000     C                   ENDSR
100100000000     C*------------------------------------------------------------------------*
100200000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
100300000000     C*------------------------------------------------------------------------*
100400000000     C     FUNVAR        BEGSR
100500000000     C*
100600000000     C* INIZIALIZZA VARIABILE -FUNKEY-
100700000000     C*
100800000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
100900000000     C     KEY           IFEQ      ENTER
101000000000     C                   MOVEL     *BLANKS       FUNKEY
101100000000     C                   END
101200000000     C     KEY           IFEQ      F1
101300000000     C                   MOVEL     '01'          FUNKEY
101400000000     C                   END
101500000000     C     KEY           IFEQ      F2
101600000000     C                   MOVEL     '02'          FUNKEY
101700000000     C                   END
101800000000     C     KEY           IFEQ      F3
101900000000     C                   MOVEL     '03'          FUNKEY
102000000000     C                   END
102100000000     C     KEY           IFEQ      F4
102200000000     C                   MOVEL     '04'          FUNKEY
102300000000     C                   END
102400000000     C     KEY           IFEQ      F5
102500000000     C                   MOVEL     '05'          FUNKEY
102600000000     C                   END
102700991103     C     KEY           IFEQ      F6
102800000000     C                   MOVEL     '06'          FUNKEY
102900000000     C                   END
103000000000     C     KEY           IFEQ      F7
103100000000     C                   MOVEL     '07'          FUNKEY
103200000000     C                   END
103300000000     C     KEY           IFEQ      F8
103400000000     C                   MOVEL     '08'          FUNKEY
103500000000     C                   END
103600000000     C     KEY           IFEQ      F9
103700000000     C                   MOVEL     '09'          FUNKEY
103800000000     C                   END
103900000000     C     KEY           IFEQ      F10
104000000000     C                   MOVEL     '10'          FUNKEY
104100000000     C                   END
104200000000     C     KEY           IFEQ      F11
104300000000     C                   MOVEL     '11'          FUNKEY
104400000000     C                   END
104500000000     C     KEY           IFEQ      F12
104600000000     C                   MOVEL     '12'          FUNKEY
104700000000     C                   END
104800000000     C     KEY           IFEQ      F13
104900000000     C                   MOVEL     '13'          FUNKEY
105000000000     C                   END
105100000000     C     KEY           IFEQ      F14
105200000000     C                   MOVEL     '14'          FUNKEY
105300000000     C                   END
105400000000     C     KEY           IFEQ      F15
105500000000     C                   MOVEL     '15'          FUNKEY
105600000000     C                   END
105700000000     C     KEY           IFEQ      F16
105800000000     C                   MOVEL     '16'          FUNKEY
105900000000     C                   END
106000000000     C     KEY           IFEQ      F17
106100000000     C                   MOVEL     '17'          FUNKEY
106200000000     C                   END
106300000000     C     KEY           IFEQ      F18
106400000000     C                   MOVEL     '18'          FUNKEY
106500000000     C                   END
106600000000     C     KEY           IFEQ      F19
106700000000     C                   MOVEL     '19'          FUNKEY
106800000000     C                   END
106900000000     C     KEY           IFEQ      F20
107000000000     C                   MOVEL     '20'          FUNKEY
107100000000     C                   END
107200000000     C     KEY           IFEQ      F21
107300000000     C                   MOVEL     '21'          FUNKEY
107400000000     C                   END
107500000000     C     KEY           IFEQ      F22
107600000000     C                   MOVEL     '22'          FUNKEY
107700000000     C                   END
107800000000     C     KEY           IFEQ      F23
107900000000     C                   MOVEL     '23'          FUNKEY
108000000000     C                   END
108100000000     C     KEY           IFEQ      F24
108200000000     C                   MOVEL     '24'          FUNKEY
108300000000     C                   END
108400000000     C     KEY           IFEQ      ROLLUP
108500000000     C                   MOVEL     'RU'          FUNKEY
108600000000     C                   END
108700000000     C     KEY           IFEQ      HELP
108800000000     C                   MOVEL     'HP'          FUNKEY
108900000000     C                   END
109000000000     C                   ENDSR
109100000000     C*------------------------------------------------------------------------*
109200000000     C* NOTE PER COMPILAZIONE SCHIERE
109300000000     C*------------------------------------------------------------------------*
109400000000     C* -MSG-
109500000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
109600000000     C* (NON TOCCARE MSG,1)
109700000000     C*
109800000000     C* -CMD-
109900000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
110000000000     C*
110100000000     C* -FUD-
110200000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
110300000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
110400000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
110500000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
110600000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
110700000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
110800000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
110900000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
111000000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
111100000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
111200000000     C*
111300000000     C*------------------------------------------------------------------------*
111400991103** ======== SCHIERA: MSG  (MESSAGGI)                       ================
111500991029TIST18R- Tasto funzionale non gestito.                                         1
111600991029TIST18R- Percentuale non accettabile.                                          2
111700991029TIST18R- Anno non accettabile.                                                 3
111800991029TIST18R- Mese inizio non accettabile.                                          4
111900991029TIST18R- Mese fine non accettabile.                                            5
112000991029TIST18R- Mese inizio maggiore mese fine.                                       6
112100991029TIST18R- Range non valido, % iniziale > % finale.                              7
112200991029TIST18R- Immettere 'S' se si vogliono includere i volumi "reali" nella stampa. 8
112300991029TIST18R- Percentuale iniziale non accettabile.                         .       9
112400991029TIST18R- Percentuale finale non accettabile.                           io.    10
112500991029TIST18R- Immettere 'S' per rigenerare il file e 'N' per non rigenerare.       11
112600991029TIST18R- Divisione non accettabile.                                           12
112700991029TIST18R- Area non accettabile.                                                13
112800991029TIST18R- Punto operativo / terminal non accettabile                           14
112900991104TIST18R- Percentuale non accettabile, valori corretti da 0 a 100.             15
113000991029TIST18R- Data non valida.                                                     16
113100991029TIST18R- Parametro non accettabile se scelto lancio per saldi.                17
113200991029TIST18R- Parametro non accettabile se scelto lancio estemporaneo.             18
113300991029TIST18R- Tipo lancio non accettabile. Inserire 'S'.                           19
113400991029TIST18R- Non si possono lanciare le statistiche di gruppo in SDI !!!          20
113500030910TIST18R- Valore non valido, valori possibili FED/EEX/DPD/ITA                  21
113600000000** ======== SCHIERA: CMD  (COMANDI)                        ================
113700000000WRKSPLF
113800000000
113900000000
114000000000
114100000000
114200000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
114300000000                   -        (S/N)=N                        funz.01¬
114400000000                   -        (S/N)=N                        funz.02¬
114500000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
114600000000                   -        (S/N)=N                        funz.04¬
114700000000                   -        (S/N)=N                        funz.05¬
114800000000F06=Lancio         -        (S/N)=S                        funz.06¬
114900000000                   -        (S/N)=N                        funz.07¬
115000000000                   -        (S/N)=N                        funz.08¬
115100000000                   -        (S/N)=N                        funz.09¬
115200000000                   -        (S/N)=N                        funz.10¬
115300000000                   -        (S/N)=N                        funz.11¬
115400991103F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
115500000000                   -        (S/N)=N                        funz.13¬
115600000000                   -        (S/N)=N                        funz.14¬
115700000000                   -        (S/N)=N                        funz.15¬
115800000000                   -        (S/N)=N                        funz.16¬
115900000000                   -        (S/N)=N                        funz.17¬
116000000000                   -        (S/N)=N                        funz.18¬
116100000000                   -        (S/N)=N                        funz.19¬
116200000000                   -        (S/N)=N                        funz.20¬
116300000000                   -        (S/N)=N                        funz.21¬
116400000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
116500000000                   -        (S/N)=N                        funz.23¬
116600991103F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
