000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200970221     F* FNLV23RQ *---------------------------------------------------*
000300910520     F*               STAMPA SEGNACOLLO BORDERO'                     *
000400970221     F*               CON RJS  QB440                                 *
000500910403     F*--------------------------------------------------------------*
000600941216     FFNLV22P   O    F  132        PRINTER
000700951021     D §RI             S              1    DIM(25)
000800910403     D                 DS
000900910515     D  §N5                    1      5  0
001000910515     D  §S5                    1      5
001100910515     D                                     DIM(5)
001200910515     D                 DS
001300910515     D  §N7                    1      7  0
001400910515     D  §S7                    1      7
001500910403     D                                     DIM(7)
001600910515     D DSBLB         E DS
001700910515     D                 DS
001800910515     D  DATA                   1      6  0
001900910515     D  GG                     1      2
002000910515     D  MM                     3      4
002100910515     D  AA                     5      6
002200910403     I/SPACE 3
002300910403     C*---------------------------------------------------------------*
002400910521     C     *ENTRY        PLIST
002500910521     C                   PARM                    DSBLB
002600910403     C*
002700941216     C* VERIFICA SE I CAMPI SONO IMPOSTATI
002800920221     C     §LBARR        COMP      *BLANKS                            1717
002900941216     C     §LBNSP        COMP      *ZEROS                             1616
003000941216     C     §LBSCA        COMP      *BLANKS                            1515
003100941216     C     §LBNBR        COMP      *ZEROS                             1414
003200941216     C     §LBINO        COMP      *BLANK                             1313
003300910515     C** INIZIALIZZA STAMPANTE
003400941216     C                   EXCEPT    LV23
003500910403     C*
003600910515     C** STAMPA INTESTAZIONE
003700910515     C*
003800941216     C                   EXCEPT    LV23D1
003900910515     C*
004000910515     C*  STAMPA N° F.V. e DATA
004100910515     C*
004200910515     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. F.V.
004300910403     C                   SETOFF                                       77
004400910515     C                   MOVE      *BLANK        §A5               5
004500910515     C                   Z-ADD     §LBNFV        §N5
004600910515     C                   DO        5             §1                1 0
004700910515     C     §S5(§1)       IFEQ      *ZERO
004800910515     C                   MOVE      *BLANK        §S5(§1)
004900910403     C                   ELSE
005000910403     C                   SETON                                        77
005100910403     C                   END
005200910403     C  N77              END
005300910515     C                   MOVEA     §S5           §A5
005400910515     C* METTO DATA FOGLIO VIAGGIO SU RIGA DI STAMPA
005500910515     C                   MOVE      §LBDAT        DATA
005600910515     C*
005700941216     C                   EXCEPT    LV23H
005800910515     C*
005900941216     C* STAMPO NUMERO BORDERO' (NON PIU' USATO)
006000930216     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. BORDERO'
006100930216     C                   SETOFF                                       77
006200930216     C                   MOVE      *BLANK        §A5
006300930216     C                   Z-ADD     §LBNBR        §N5
006400930216     C                   DO        5             §1
006500930216     C     §S5(§1)       IFEQ      *ZERO
006600930216     C                   MOVE      *BLANK        §S5(§1)
006700930216     C                   ELSE
006800930216     C                   SETON                                        77
006900930216     C                   END
007000930216     C  N77              END
007100930216     C                   MOVEA     §S5           §A5
007200930216     C*
007300941216     C   14              EXCEPT    LV23X2
007400930216     C*
007500910515     C* STAMPA NUMERO SPEDIZIONI
007600910403     C*
007700910515     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. SPEDIZIONI
007800910515     C                   SETOFF                                       77
007900910515     C                   MOVE      *BLANK        §A7               7
008000910515     C                   Z-ADD     §LBNSP        §N7
008100910515     C                   DO        7             §1
008200910515     C     §S7(§1)       IFEQ      *ZERO
008300910515     C                   MOVE      *BLANK        §S7(§1)
008400910515     C                   ELSE
008500910515     C                   SETON                                        77
008600910515     C                   END
008700910515     C  N77              END
008800910515     C                   MOVEA     §S7           §A7
008900910515     C*
009000941216     C   16              EXCEPT    LV23D2
009100910515     C*
009200910515     C* STAMPA DATI VETTORE
009300941216     C                   EXCEPT    LV23D3
009400910515     C*
009500910515     C* STAMPA DATI CONDUCENTE
009600941216     C                   EXCEPT    LV23D4
009700910515     C*
009800910515     C                   MOVEA     *BLANKS       §RI
009900910515     C* STAMPA LINEA PARTENZA + DECODIFICA
010000910403     C*
010100910516     C                   MOVEA     §LBLNP        §RI(1)
010200910516     C                   MOVEA     §LBDLP        §RI(5)
010300910515     C*
010400941216     C                   EXCEPT    LV23D5
010500910515     C*
010600910515     C                   MOVEA     *BLANKS       §RI
010700910515     C* STAMPA LINEA ARRIVO + DECODIFICA
010800910515     C*
010900910516     C                   MOVEA     §LBLNA        §RI(1)
011000910516     C                   MOVEA     §LBDLA        §RI(5)
011100910515     C*
011200941216     C                   EXCEPT    LV23D6
011300910515     C*
011400910515     C* STAMPA FILIALE DI SCARICO
011500910515     C*
011600941216     C   15              EXCEPT    LV23D7
011700910403     C*
011800941216     C* STAMPA FILIALE DI INOLTRO
011900941216     C   13              EXCEPT    LV23D8
012000941216     C*
012100941216     C* STAMPA SE PARTITO CON ARRIVI (NON PIU USATO)
012200941216     C   17              EXCEPT    LV23X8
012300910403     C*
012400910403     C* STAMPA CHIUSURA ETICHETTA
012500941216     C                   EXCEPT    LV23D9
012600910403     C*
012700910403     C                   SETON                                        LR
012800010731      *-------------------------------------------------------------------
012900941216     OFNLV22P   E            LV23        1
013000970221     O                                            9 '~^"LV23";'
013100010731     O          E            LV23        1
013200010731     O*LOGICA
013300010731     O                                           16 '00001;0;265;070;'
013400010731     O          E            LV23        1
013500010731     O*FISICA
013600010731     O                                           21 'UTOF;2700;TEAR;0;SPB;'
013700010731     O          E            LV23        1
013800010731     O*GIRO SCRITTA A 180°
013900010731     O                                            9 'FLIP;300;'
014000010731     O          E            LV23        1
014100010731     O*                                       FONT
014200010731     O                                            7 'DHR;$1;'
014300010731     O********* Foglio Viaggio
014400010731     O          E            LV23D1      1
014500010731     O*                                       CARATTERE
014600010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
014700010731     O          E            LV23d1      1
014800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
014900010731     O                                           17 'VBR;30;HBR;120;'
015000010731     O          E            LV23D1      1
015100010731     O                                            1 '"'
015200010731     O                                            5 'F.V.'
015300010731     O                                            7 '";'
015400010731     O********* N° F.V. + Data
015500010731     O          E            LV23H       1
015600010731     O*                                       CARATTERE
015700010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
015800010731     O          E            LV23H       1
015900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016000010731     O                                           16 'VBR;030;HBR;190;'
016100010731     O          E            LV23H       1
016200010731     O                                            6 '"n.:";'
016300010731     O          E            LV23H       1
016400010731     O*                                       CARATTERE
016500010731     O                                           21 'DCH;25;DCW;35;ICS;03;'
016600010731     O          E            LV23H       1
016700010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016800010731     O                                           16 'VBR;030;HBR;260;'
016900010731     O          E            LV23H       1
017000010731     O                                            1 '"'
017100010731     O                       §A5                  6
017200010731     O                                            8 '";'
017300010731     O          E            LV23H       1
017400010731     O*                                       CARATTERE
017500010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
017600010731     O          E            LV23H       1
017700010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
017800010731     O                                           16 'VBR;030;HBR;400;'
017900010731     O          E            LV23H       1
018000010731     O                                            7 '"del:";'
018100010731     O          E            LV23H       1
018200010731     O*                                       CARATTERE
018300010731     O                                           21 'DCH;25;DCW;35;ICS;03;'
018400010731     O          E            LV23H       1
018500010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
018600010731     O                                           16 'VBR;030;HBR;470;'
018700010731     O          E            LV23H       1
018800010731     O                                            1 '"'
018900010731     O                       GG                   3
019000010731     O                                            4 '/'
019100010731     O                       MM                   6
019200010731     O                                            7 '/'
019300010731     O                       AA                   9
019400010731     O                                           11 '";'
019500930216     O********* N° Borderò
019600941216     O          E            LV23X2      1
019700930216     O*                                       CARATTERE
019800930216     O                                           21 'DCH;20;DCW;20;ICS;03;'
019900941216     O          E            LV23X2      1
020000930216     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020100010731     O                                           16 'VBR;052;HBR;120;'
020200941216     O          E            LV23X2      1
020300941216     O                                           17 '"Bordero'' n.:";'
020400941216     O          E            LV23X2      1
020500930216     O*                                       CARATTERE
020600010731     O                                           21 'DCH;25;DCW;30;ICS;03;'
020700941216     O          E            LV23X2      1
020800930216     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020900010731     O                                           16 'VBR;052;HBR;280;'
021000941216     O          E            LV23X2      1
021100930216     O                                            1 '"'
021200951021     O                       §A5                  6
021300951021     O                                            8 '";'
021400910515     O********* N° Spedizioni
021500941216     O          E            LV23D2      1
021600910516     O*                                       CARATTERE
021700930216     O                                           21 'DCH;20;DCW;20;ICS;03;'
021800941216     O          E            LV23D2      1
021900910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
022000010731     O                                           16 'VBR;052;HBR;380;'
022100941216     O          E            LV23D2      1
022200941216     O                                           17 '"Spedizioni n.:";'
022300941216     O          E            LV23D2      1
022400910517     O*                                       CARATTERE
022500010731     O                                           21 'DCH;25;DCW;35;ICS;03;'
022600941216     O          E            LV23D2      1
022700910517     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
022800010731     O                                           16 'VBR;052;HBR;550;'
022900941216     O          E            LV23D2      1
023000910517     O                                            1 '"'
023100951021     O                       §A7                  8
023200951021     O                                           10 '";'
023300910515     O********* Vettore
023400941216     O          E            LV23D3      1
023500910403     O*                                       CARATTERE
023600930216     O                                           23 'DCH;20;DCW;20;ICS;03;'
023700941216     O          E            LV23D3      1
023800910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
023900010731     O                                           16 'VBR;080;HBR;120;'
024000941216     O          E            LV23D3      1
024100941216     O                                           11 '"Vettore:";'
024200941216     O          E            LV23D3      1
024300910516     O*                                       CARATTERE
024400930216     O                                           23 'DCH;30;DCW;30;ICS;03;'
024500941216     O          E            LV23D3      1
024600910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
024700010731     O                                           16 'VBR;080;HBR;240;'
024800941216     O          E            LV23D3      1
024900910517     O                                            1 '"'
025000951021     O                       §LBVET              16
025100951021     O                                           18 '";'
025200910515     O********* Conducente
025300941216     O          E            LV23D4      1
025400910403     O*                                       CARATTERE
025500930216     O                                           23 'DCH;20;DCW;20;ICS;03;'
025600941216     O          E            LV23D4      1
025700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
025800010731     O                                           16 'VBR;108;HBR;120;'
025900941216     O          E            LV23D4      1
026000941216     O                                           14 '"Conducente:";'
026100941216     O          E            LV23D4      1
026200910516     O*                                       CARATTERE
026300930216     O                                           23 'DCH;30;DCW;30;ICS;03;'
026400941216     O          E            LV23D4      1
026500910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
026600010731     O                                           16 'VBR;108;HBR;270;'
026700941216     O          E            LV23D4      1
026800910517     O                                            1 '"'
026900951021     O                       §LBCON              16
027000951021     O                                           18 '";'
027100910515     O********* Linea Partenza + decodifica
027200941216     O          E            LV23D5      1
027300910403     O*                                       CARATTERE
027400930216     O                                           23 'DCH;35;DCW;35;ICS;03;'
027500941216     O          E            LV23D5      1
027600910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
027700010731     O                                           16 'VBR;140;HBR;80;'
027800941216     O          E            LV23D5      1
027900941216     O                                            6 '"da:";'
028000941216     O          E            LV23D5      1
028100910516     O*                                       CARATTERE
028200010731     O                                           23 'DCH;40;DCW;50;ICS;03;'
028300941216     O          E            LV23D5      1
028400910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
028500010731     O                                           16 'VBR;140;HBR;160;'
028600941216     O          E            LV23D5      1
028700910516     O                                            1 '"'
028800951021     O                       §RI                 26
028900951021     O                                           28 '";'
029000910515     O********* Linea Arrivo   + decodifica
029100941216     O          E            LV23D6      1
029200910403     O*                                       CARATTERE
029300930216     O                                           21 'DCH;35;DCW;35;ICS;03;'
029400941216     O          E            LV23D6      1
029500910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
029600010731     O                                           16 'VBR;175;HBR;80;'
029700941216     O          E            LV23D6      1
029800941216     O                                            6 '"a: ";'
029900941216     O          E            LV23D6      1
030000910516     O*                                       CARATTERE
030100010731     O                                           21 'DCH;40;DCW;50;ICS;03;'
030200941216     O          E            LV23D6      1
030300910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
030400010731     O                                           16 'VBR;175;HBR;160;'
030500941216     O          E            LV23D6      1
030600910516     O                                            1 '"'
030700951021     O                       §RI                 26
030800951021     O                                           28 '";'
030900910515     O********* Filiale di scarico
031000941216     O          E            LV23D7      1
031100910515     O*                                       CARATTERE
031200010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
031300941216     O          E            LV23D7      1
031400910515     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
031500010731     O                                           16 'VBR;210;HBR;60;'
031600941216     O          E            LV23D7      1
031700941216     O                                           11 '"Scarico:";'
031800941216     O          E            LV23D7      1
031900910516     O*                                       CARATTERE
032000010731     O                                           21 'DCH;35;DCW;30;ICS;03;'
032100941216     O          E            LV23D7      1
032200910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
032300010731     O                                           16 'VBR;210;HBR;200;'
032400941216     O          E            LV23D7      1
032500910516     O                                            1 '"'
032600910516     O                       §LBSCA              17
032700910516     O                                           19 '";'
032800910515     O********* Filiale di inoltro
032900941216     O          E            LV23D8      1
033000910515     O*                                       CARATTERE
033100010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
033200941216     O          E            LV23D8      1
033300910515     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
033400010731     O                                           16 'VBR;240;HBR;60;'
033500941216     O          E            LV23D8      1
033600920427     O                                           13 '"V I A  :";'
033700941216     O          E            LV23D8      1
033800910516     O*                                       CARATTERE
033900010731     O                                           21 'DCH;35;DCW;30;ICS;03;'
034000941216     O          E            LV23D8      1
034100910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
034200010731     O                                           16 'VBR;240;HBR;200;'
034300941216     O          E            LV23D8      1
034400910516     O                                            1 '"'
034500910516     O                       §LBINO              17
034600910516     O                                           19 '";'
034700920221     O********* FILIALE PARTITA CON GLI ARRIVI
034800941216     O          E            LV23X8      1
034900010731     O                                           22 'DCH;40;DCW;065;ICS;03;'
035000941216     O          E            LV23X8      1
035100920221     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
035200010731     O                                           16 'VBR;240;HBR;575;'
035300920221     O                                           17 '"'
035400920221     O                       §LBARR              18
035500920221     O                                           20 '";'
035600941216     O          E            LV23X8      1
035700010731     O                                           21 'HOME;VBR;200;HBR;550;'
035800941216     O          E            LV23X8      1
035900920221     O*                                                L   H
036000010731     O                                           16 'DCBX;0;0;080;50;'
036100910403     O********* Chiusura etchetta
036200941216     O          E            LV23D9      1
036300970221     O                                            5 'TRM;\'
