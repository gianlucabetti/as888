000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200941216     F* FNLV23R  *---------------------------------------------------*
000300910520     F*               STAMPA SEGNACOLLO BORDERO'                     *
000400910403     F*--------------------------------------------------------------*
000500941216     FFNLV22P   O    F  132        PRINTER
000600951021     D §RI             S              1    DIM(25)
000700910403     D                 DS
000800910515     D  §N5                    1      5  0
000900910515     D  §S5                    1      5
001000910515     D                                     DIM(5)
001100910515     D                 DS
001200910515     D  §N7                    1      7  0
001300910515     D  §S7                    1      7
001400910403     D                                     DIM(7)
001500910515     D DSBLB         E DS
001600910515     D                 DS
001700910515     D  DATA                   1      6  0
001800910515     D  GG                     1      2
001900910515     D  MM                     3      4
002000910515     D  AA                     5      6
002100910403     I/SPACE 3
002200910403     C*---------------------------------------------------------------*
002300910521     C     *ENTRY        PLIST
002400910521     C                   PARM                    DSBLB
002500910403     C*
002600941216     C* VERIFICA SE I CAMPI SONO IMPOSTATI
002700920221     C     §LBARR        COMP      *BLANKS                            1717
002800941216     C     §LBNSP        COMP      *ZEROS                             1616
002900941216     C     §LBSCA        COMP      *BLANKS                            1515
003000941216     C     §LBNBR        COMP      *ZEROS                             1414
003100941216     C     §LBINO        COMP      *BLANK                             1313
003200910515     C** INIZIALIZZA STAMPANTE
003300941216     C                   EXCEPT    LV23
003400910403     C*
003500910515     C** STAMPA INTESTAZIONE
003600910515     C*
003700941216     C                   EXCEPT    LV23D1
003800910515     C*
003900910515     C*  STAMPA N° F.V. e DATA
004000910515     C*
004100910515     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. F.V.
004200910403     C                   SETOFF                                       77
004300910515     C                   MOVE      *BLANK        §A5               5
004400910515     C                   Z-ADD     §LBNFV        §N5
004500910515     C                   DO        5             §1                1 0
004600910515     C     §S5(§1)       IFEQ      *ZERO
004700910515     C                   MOVE      *BLANK        §S5(§1)
004800910403     C                   ELSE
004900910403     C                   SETON                                        77
005000910403     C                   END
005100910403     C  N77              END
005200910515     C                   MOVEA     §S5           §A5
005300910515     C* METTO DATA FOGLIO VIAGGIO SU RIGA DI STAMPA
005400910515     C                   MOVE      §LBDAT        DATA
005500910515     C*
005600941216     C                   EXCEPT    LV23H
005700910515     C*
005800941216     C* STAMPO NUMERO BORDERO' (NON PIU' USATO)
005900930216     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. BORDERO'
006000930216     C                   SETOFF                                       77
006100930216     C                   MOVE      *BLANK        §A5
006200930216     C                   Z-ADD     §LBNBR        §N5
006300930216     C                   DO        5             §1
006400930216     C     §S5(§1)       IFEQ      *ZERO
006500930216     C                   MOVE      *BLANK        §S5(§1)
006600930216     C                   ELSE
006700930216     C                   SETON                                        77
006800930216     C                   END
006900930216     C  N77              END
007000930216     C                   MOVEA     §S5           §A5
007100930216     C*
007200941216     C   14              EXCEPT    LV23X2
007300930216     C*
007400910515     C* STAMPA NUMERO SPEDIZIONI
007500910403     C*
007600910515     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. SPEDIZIONI
007700910515     C                   SETOFF                                       77
007800910515     C                   MOVE      *BLANK        §A7               7
007900910515     C                   Z-ADD     §LBNSP        §N7
008000910515     C                   DO        7             §1
008100910515     C     §S7(§1)       IFEQ      *ZERO
008200910515     C                   MOVE      *BLANK        §S7(§1)
008300910515     C                   ELSE
008400910515     C                   SETON                                        77
008500910515     C                   END
008600910515     C  N77              END
008700910515     C                   MOVEA     §S7           §A7
008800910515     C*
008900941216     C   16              EXCEPT    LV23D2
009000910515     C*
009100910515     C* STAMPA DATI VETTORE
009200941216     C                   EXCEPT    LV23D3
009300910515     C*
009400910515     C* STAMPA DATI CONDUCENTE
009500941216     C                   EXCEPT    LV23D4
009600910515     C*
009700910515     C                   MOVEA     *BLANKS       §RI
009800910515     C* STAMPA LINEA PARTENZA + DECODIFICA
009900910403     C*
010000910516     C                   MOVEA     §LBLNP        §RI(1)
010100910516     C                   MOVEA     §LBDLP        §RI(5)
010200910515     C*
010300941216     C                   EXCEPT    LV23D5
010400910515     C*
010500910515     C                   MOVEA     *BLANKS       §RI
010600910515     C* STAMPA LINEA ARRIVO + DECODIFICA
010700910515     C*
010800910516     C                   MOVEA     §LBLNA        §RI(1)
010900910516     C                   MOVEA     §LBDLA        §RI(5)
011000910515     C*
011100941216     C                   EXCEPT    LV23D6
011200910515     C*
011300910515     C* STAMPA FILIALE DI SCARICO
011400910515     C*
011500941216     C   15              EXCEPT    LV23D7
011600910403     C*
011700941216     C* STAMPA FILIALE DI INOLTRO
011800941216     C   13              EXCEPT    LV23D8
011900941216     C*
012000941216     C* STAMPA SE PARTITO CON ARRIVI (NON PIU USATO)
012100941216     C   17              EXCEPT    LV23X8
012200910403     C*
012300910403     C* STAMPA CHIUSURA ETICHETTA
012400941216     C                   EXCEPT    LV23D9
012500910403     C*
012600910403     C                   SETON                                        LR
012700941216     OFNLV22P   E            LV23        1
012800951021     O******                                 inizializz.CONVERTITORE
012900951021     O                                           20 '&&??%%P%%7E%^"LV23";'
013000951021     O******                                 inizializzazione VIDEO
013100951021     O**                                  9 '~^"LV22";'
013200941216     O          E            LV23        1
013300910403     O*LOGICA
013400010731     O                                           16 '00001;0;265;000;'
013500941216     O          E            LV23        1
013600910403     O*FISICA
013700010731     O                                           14 'UTOF;2700;SPB;'
013800941216     O          E            LV23        1
013900910403     O*GIRO SCRITTA A 180°
014000010731     O                                            9 'FLIP;300;'
014100941216     O          E            LV23        1
014200910403     O*                                       FONT
014300910403     O                                            7 'DHR;$1;'
014400910515     O********* Foglio Viaggio
014500941216     O          E            LV23D1      1
014600010731     O*                                       CARATTERE
014700010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
014800010731     O          E            LV23d1      1
014900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
015000010731     O                                           17 'VBR;30;HBR;120;'
015100010731     O          E            LV23D1      1
015200010731     O                                            1 '"'
015300010731     O                                            5 'F.V.'
015400010731     O                                            7 '";'
015500010731     O********* N° F.V. + Data
015600010731     O          E            LV23H       1
015700010731     O*                                       CARATTERE
015800010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
015900010731     O          E            LV23H       1
016000010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016100010731     O                                           16 'VBR;030;HBR;190;'
016200010731     O          E            LV23H       1
016300010731     O                                            6 '"n.:";'
016400010731     O          E            LV23H       1
016500010731     O*                                       CARATTERE
016600010731     O                                           21 'DCH;25;DCW;35;ICS;03;'
016700010731     O          E            LV23H       1
016800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016900010731     O                                           16 'VBR;030;HBR;260;'
017000010731     O          E            LV23H       1
017100010731     O                                            1 '"'
017200010731     O                       §A5                  6
017300010731     O                                            8 '";'
017400010731     O          E            LV23H       1
017500010731     O*                                       CARATTERE
017600010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
017700010731     O          E            LV23H       1
017800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
017900010731     O                                           16 'VBR;030;HBR;400;'
018000010731     O          E            LV23H       1
018100010731     O                                            7 '"del:";'
018200010731     O          E            LV23H       1
018300010731     O*                                       CARATTERE
018400010731     O                                           21 'DCH;25;DCW;35;ICS;03;'
018500010731     O          E            LV23H       1
018600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
018700010731     O                                           16 'VBR;030;HBR;470;'
018800010731     O          E            LV23H       1
018900010731     O                                            1 '"'
019000010731     O                       GG                   3
019100010731     O                                            4 '/'
019200010731     O                       MM                   6
019300010731     O                                            7 '/'
019400010731     O                       AA                   9
019500010731     O                                           11 '";'
019600010731     O********* N° Borderò
019700010731     O          E            LV23X2      1
019800010731     O*                                       CARATTERE
019900010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
020000010731     O          E            LV23X2      1
020100010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020200010731     O                                           16 'VBR;052;HBR;120;'
020300010731     O          E            LV23X2      1
020400010731     O                                           17 '"Bordero'' n.:";'
020500010731     O          E            LV23X2      1
020600010731     O*                                       CARATTERE
020700010731     O                                           21 'DCH;25;DCW;30;ICS;03;'
020800010731     O          E            LV23X2      1
020900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
021000010731     O                                           16 'VBR;052;HBR;280;'
021100010731     O          E            LV23X2      1
021200010731     O                                            1 '"'
021300010731     O                       §A5                  6
021400010731     O                                            8 '";'
021500010731     O********* N° Spedizioni
021600010731     O          E            LV23D2      1
021700010731     O*                                       CARATTERE
021800010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
021900010731     O          E            LV23D2      1
022000010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
022100010731     O                                           16 'VBR;052;HBR;380;'
022200010731     O          E            LV23D2      1
022300010731     O                                           17 '"Spedizioni n.:";'
022400010731     O          E            LV23D2      1
022500010731     O*                                       CARATTERE
022600010731     O                                           21 'DCH;25;DCW;35;ICS;03;'
022700010731     O          E            LV23D2      1
022800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
022900010731     O                                           16 'VBR;052;HBR;570;'
023000010731     O          E            LV23D2      1
023100010731     O                                            1 '"'
023200010731     O                       §A7                  8
023300010731     O                                           10 '";'
023400010731     O********* Vettore
023500010731     O          E            LV23D3      1
023600010731     O*                                       CARATTERE
023700010731     O                                           23 'DCH;20;DCW;20;ICS;03;'
023800010731     O          E            LV23D3      1
023900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
024000010731     O                                           16 'VBR;080;HBR;120;'
024100010731     O          E            LV23D3      1
024200010731     O                                           11 '"Vettore:";'
024300010731     O          E            LV23D3      1
024400010731     O*                                       CARATTERE
024500010731     O                                           23 'DCH;30;DCW;30;ICS;03;'
024600010731     O          E            LV23D3      1
024700010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
024800010731     O                                           16 'VBR;080;HBR;240;'
024900010731     O          E            LV23D3      1
025000010731     O                                            1 '"'
025100010731     O                       §LBVET              16
025200010731     O                                           18 '";'
025300010731     O********* Conducente
025400010731     O          E            LV23D4      1
025500010731     O*                                       CARATTERE
025600010731     O                                           23 'DCH;20;DCW;20;ICS;03;'
025700010731     O          E            LV23D4      1
025800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
025900010731     O                                           16 'VBR;108;HBR;120;'
026000010731     O          E            LV23D4      1
026100010731     O                                           14 '"Conducente:";'
026200010731     O          E            LV23D4      1
026300010731     O*                                       CARATTERE
026400010731     O                                           23 'DCH;30;DCW;30;ICS;03;'
026500010731     O          E            LV23D4      1
026600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
026700010731     O                                           16 'VBR;108;HBR;270;'
026800010731     O          E            LV23D4      1
026900010731     O                                            1 '"'
027000010731     O                       §LBCON              16
027100010731     O                                           18 '";'
027200010731     O********* Linea Partenza + decodifica
027300010731     O          E            LV23D5      1
027400010731     O*                                       CARATTERE
027500010731     O                                           23 'DCH;35;DCW;35;ICS;03;'
027600010731     O          E            LV23D5      1
027700010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
027800010731     O                                           16 'VBR;140;HBR;80;'
027900010731     O          E            LV23D5      1
028000010731     O                                            6 '"da:";'
028100010731     O          E            LV23D5      1
028200010731     O*                                       CARATTERE
028300010731     O                                           23 'DCH;40;DCW;50;ICS;03;'
028400010731     O          E            LV23D5      1
028500010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
028600010731     O                                           16 'VBR;140;HBR;160;'
028700010731     O          E            LV23D5      1
028800010731     O                                            1 '"'
028900010731     O                       §RI                 26
029000010731     O                                           28 '";'
029100010731     O********* Linea Arrivo   + decodifica
029200010731     O          E            LV23D6      1
029300010731     O*                                       CARATTERE
029400010731     O                                           21 'DCH;35;DCW;35;ICS;03;'
029500010731     O          E            LV23D6      1
029600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
029700010731     O                                           16 'VBR;175;HBR;80;'
029800010731     O          E            LV23D6      1
029900010731     O                                            6 '"a: ";'
030000010731     O          E            LV23D6      1
030100010731     O*                                       CARATTERE
030200010731     O                                           21 'DCH;40;DCW;50;ICS;03;'
030300010731     O          E            LV23D6      1
030400010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
030500010731     O                                           16 'VBR;175;HBR;160;'
030600010731     O          E            LV23D6      1
030700010731     O                                            1 '"'
030800010731     O                       §RI                 26
030900010731     O                                           28 '";'
031000010731     O********* Filiale di scarico
031100010731     O          E            LV23D7      1
031200010731     O*                                       CARATTERE
031300010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
031400010731     O          E            LV23D7      1
031500010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
031600010731     O                                           16 'VBR;210;HBR;60;'
031700010731     O          E            LV23D7      1
031800010731     O                                           11 '"Scarico:";'
031900010731     O          E            LV23D7      1
032000010731     O*                                       CARATTERE
032100010731     O                                           21 'DCH;35;DCW;30;ICS;03;'
032200010731     O          E            LV23D7      1
032300010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
032400010731     O                                           16 'VBR;210;HBR;200;'
032500010731     O          E            LV23D7      1
032600010731     O                                            1 '"'
032700010731     O                       §LBSCA              17
032800010731     O                                           19 '";'
032900010731     O********* Filiale di inoltro
033000010731     O          E            LV23D8      1
033100010731     O*                                       CARATTERE
033200010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
033300010731     O          E            LV23D8      1
033400010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
033500010731     O                                           16 'VBR;240;HBR;60;'
033600010731     O          E            LV23D8      1
033700010731     O                                           13 '"V I A  :";'
033800010731     O          E            LV23D8      1
033900010731     O*                                       CARATTERE
034000010731     O                                           21 'DCH;35;DCW;30;ICS;03;'
034100010731     O          E            LV23D8      1
034200010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
034300010731     O                                           16 'VBR;240;HBR;200;'
034400010731     O          E            LV23D8      1
034500010731     O                                            1 '"'
034600010731     O                       §LBINO              17
034700010731     O                                           19 '";'
034800010731     O********* FILIALE PARTITA CON GLI ARRIVI
034900010731     O          E            LV23X8      1
035000010731     O                                           22 'DCH;40;DCW;065;ICS;03;'
035100010731     O          E            LV23X8      1
035200010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
035300010731     O                                           16 'VBR;240;HBR;575;'
035400010731     O                                           17 '"'
035500010731     O                       §LBARR              18
035600010731     O                                           20 '";'
035700010731     O          E            LV23X8      1
035800010731     O                                           21 'HOME;VBR;200;HBR;550;'
035900010731     O          E            LV23X8      1
036000010731     O*                                                L   H
036100010731     O                                           16 'DCBX;0;0;080;50;'
036200910403     O********* Chiusura etchetta
036300941216     O          E            LV23D9      1
036400951021     O********                               CON COVERTITORE
036500951021     O                                            9 'TRM;%%5C%'
