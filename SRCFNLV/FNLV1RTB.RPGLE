000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200970430     F* FNLV21R  *---------------------------------------------------*
000300970421     F*               STAMPA SEGNACOLLO F.V.                         *
000400970221     F*               CON RJS  QB440                                 *
000500970421     F*          UTILIZZA LA STESSA DS DELL'ETICHETTA BORDERO'       *
000600970421     F*          MA STAMPA SOLO I CAMPI NECESSARI                    *
000700970430     F*   RISPETTO A QB440 VBR+10 E LOGICA -30 ANZICHE' + 100        *
000800910403     F*--------------------------------------------------------------*
000900941216     FFNLV22P   O    F  132        PRINTER
001000951021     D §RI             S              1    DIM(25)
001100970421     D §TCA            S              4    DIM(30)
001200970421     D                 DS
001300970421     D  §CB                    1      6  0
001400970421     D                                     DIM(6)
001500970421     D  §CA                    1      6
001600970421     D                                     DIM(6)
001700970421     D                 DS
001800970421     D  §2                     1      2  0
001900970421     D                                     DIM(2)
002000970421     D  §2A                    1      2
002100970421     D                                     DIM(2)
002200970421     D                 DS
002300970421     D  §C                     1      5  0
002400970421     D                                     DIM(5)
002500970421     D  WNFV                   1      5
002600910403     D                 DS
002700910515     D  §N5                    1      5  0
002800910515     D  §S5                    1      5
002900910515     D                                     DIM(5)
003000910515     D                 DS
003100910515     D  §N7                    1      7  0
003200910515     D  §S7                    1      7
003300910403     D                                     DIM(7)
003400970421     D                 DS
003500970421     D  §TCAX                  1    120
003600970421     D  §TC1                   1     24
003700970421     D                                     DIM(6)
003800970421     D  §TC2                  25     56
003900970421     D                                     DIM(8)
004000970421     D  §TC3                  57     88
004100970421     D                                     DIM(8)
004200970421     D  §TC4                  89    120
004300970421     D                                     DIM(8)
004400910515     D DSBLB         E DS
004500970421     D  §TC                  112    201  0
004600970421     D                                     DIM(30)
004700910515     D                 DS
004800910515     D  DATA                   1      6  0
004900910515     D  GG                     1      2
005000910515     D  MM                     3      4
005100910515     D  AA                     5      6
005200910403     I/SPACE 3
005300910403     C*---------------------------------------------------------------*
005400910521     C     *ENTRY        PLIST
005500910521     C                   PARM                    DSBLB
005600970421     C*
005700970421     C* EDITO LA SCHIERA
005800970421     C                   DO        30            Y                 2 0
005900970421     C     §TC(Y)        IFGT      *ZERO
006000970421     C                   MOVE      §TC(Y)        §TCA(Y)
006100970421     C                   ENDIF
006200970421     C                   ENDDO
006300970421     C                   MOVEA     §TCA          §TCAX
006400970421     C*
006500970421     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
006600970421     C                   MOVE      §LBNFV        WNFV              5
006700970421     C                   EXSR      M10
006800970421     C                   MOVEA     §CA           §C6A              6
006900970421     C                   MOVE      §C6A          §CDBAR            6
007000970421     C* (CAMPO UNICO BARCODE E COSTANTI)
007100970421     C                   MOVE      *BLANK        §§CDB1            7
007200970421     C                   MOVE      §CDBAR        §§CDB1
007300970421     C                   MOVEL     '"'           §§CDB1
007400970421     C                   MOVE      *BLANK        §§CDB2            9
007500970421     C                   MOVEL     §§CDB1        §§CDB2
007600970421     C                   MOVE      '";'          §§CDB2
007700910515     C** INIZIALIZZA STAMPANTE
007800970421     C                   EXCEPT    LV21
007900910515     C*
008000970421     C                   EXCEPT    LV21D1
008100910515     C*
008200910515     C*  STAMPA N° F.V. e DATA
008300910515     C*
008400910515     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. F.V.
008500910403     C                   SETOFF                                       77
008600910515     C                   MOVE      *BLANK        §A5               5
008700910515     C                   Z-ADD     §LBNFV        §N5
008800910515     C                   DO        5             §1                1 0
008900910515     C     §S5(§1)       IFEQ      *ZERO
009000910515     C                   MOVE      *BLANK        §S5(§1)
009100910403     C                   ELSE
009200910403     C                   SETON                                        77
009300910403     C                   END
009400910403     C  N77              END
009500910515     C                   MOVEA     §S5           §A5
009600910515     C* METTO DATA FOGLIO VIAGGIO SU RIGA DI STAMPA
009700910515     C                   MOVE      §LBDAT        DATA
009800910515     C*
009900970421     C                   EXCEPT    LV21H
010000910515     C*
010100970421     C* STAMPA LINEA ARRIVO + DECODIFICA
010200910515     C                   MOVEA     *BLANKS       §RI
010300910516     C                   MOVEA     §LBLNA        §RI(1)
010400910516     C                   MOVEA     §LBDLA        §RI(5)
010500910515     C*
010600970421     C                   EXCEPT    LV21D6
010700910515     C*
010800970421     C* STAMPA TERMINAL DA CARICARE SUL FOGLIO
010900910515     C*
011000970421     C                   EXCEPT    LV21D7
011100970421     C*
011200970421     C* STAMPA BARCODE N.F.V.
011300970421     C*
011400970421     C                   EXCEPT    LV21D8
011500910403     C*
011600910403     C* STAMPA CHIUSURA ETICHETTA
011700970421     C                   EXCEPT    LV21D9
011800910403     C*
011900910403     C                   SETON                                        LR
012000970421     C/SPACE 005
012100970421     C*****************************************************
012200970421     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
012300970421     C*****************************************************
012400970421     C     M10           BEGSR
012500970421     C                   MOVE      *ZERO         §TT               3 0
012600970421     C                   MOVE      *ZERO         §TT2              3 0
012700970421     C                   MOVE      *ZERO         §1                1 0
012800970421     C                   Z-ADD     6             §S                2 0
012900970421     C                   DO        5             §N                2 0
013000970421     C                   SUB       1             §S
013100970421     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
013200970421     C                   MOVE      §C(§N)        §CB(§N)
013300970421     C     §N            DIV       2             §CTR              3 0
013400970421     C                   MVR                     §RST              3 010
013500970421     C     *IN(10)       IFEQ      '1'
013600970421     C     §C(§S)        MULT      2             §N2               2 0
013700970421     C                   MOVE      §N2           §A2               2
013800970421     C                   MOVEA     §A2           §2A
013900970421     C                   XFOOT     §2            §CTR
014000970421     C                   ELSE
014100970421     C                   Z-ADD     §C(§S)        §CTR
014200970421     C                   END
014300970421     C                   ADD       §CTR          §TT
014400970421     C                   END
014500970421     C*
014600970421     C     §TT           ADD       9             §TT2
014700970421     C                   MOVE      0             §TT2
014800970421     C     §TT2          SUB       §TT           §1
014900970421     C*
015000970421     C                   MOVE      §1            §CB(6)
015100970421     C                   ENDSR
015200970421     OFNLV22P   E            LV21        1
015300951021     O******                                 inizializz.CONVERTITORE
015400970430     O                                           20 '&&??%%P%%7E%^"LV21";'
015500951021     O******                                 inizializzazione VIDEO
015600970430     O**                                  9 '~^"LV21";'
015700970421     O          E            LV21        1
015800910403     O*LOGICA
015900020131     O                                           16 '00001;0;265;-80;'
016000970421     O          E            LV21        1
016100910403     O*FISICA
016200970221     O** X TX340/240
016300010731     O                                           14 'UTOF;2700;SPB;'
016400970221     O** X QB400
016500970430     O**                                 21 'UTOF;4100;TEAR;0;SPB;'
016600970421     O          E            LV21        1
016700910403     O*GIRO SCRITTA A 180°
016800010731     O                                            9 'FLIP;300;'
016900970421     O          E            LV21        1
017000910403     O*                                       FONT
017100910403     O                                            7 'DHR;$1;'
017200910515     O********* Foglio Viaggio
017300010731     O          E            LV21D1      1
017400010731     O*                                       CARATTERE
017500010731     O                                           21 'DCH;30;DCW;30;ICS;03;'
017600010731     O          E            LV21D1      1
017700010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
017800020131     O                                           17 'VBR;45;HBR;120;'
017900010731     O          E            LV21D1      1
018000010731     O                                            1 '"'
018100010731     O                                            5 'F.V.'
018200010731     O                                            7 '";'
018300010731     O********* N° F.V. + Data
018400010731     O          E            LV21H       1
018500010731     O*                                       CARATTERE
018600010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
018700010731     O          E            LV21H       1
018800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
018900020131     O                                           16 'VBR;045;HBR;190;'
019000010731     O          E            LV21H       1
019100010731     O                                            6 '"n.:";'
019200010731     O          E            LV21H       1
019300010731     O*                                       CARATTERE
019400010731     O                                           21 'DCH;30;DCW;40;ICS;03;'
019500010731     O          E            LV21H       1
019600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
019700020131     O                                           16 'VBR;045;HBR;260;'
019800010731     O          E            LV21H       1
019900010731     O                                            1 '"'
020000010731     O                       §A5                  6
020100010731     O                                            8 '";'
020200010731     O          E            LV21H       1
020300010731     O*                                       CARATTERE
020400010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
020500010731     O          E            LV21H       1
020600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020700020131     O                                           16 'VBR;045;HBR;400;'
020800010731     O          E            LV21H       1
020900010731     O                                            7 '"del:";'
021000010731     O          E            LV21H       1
021100010731     O*                                       CARATTERE
021200010731     O                                           21 'DCH;30;DCW;40;ICS;03;'
021300010731     O          E            LV21H       1
021400010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
021500020131     O                                           16 'VBR;045;HBR;470;'
021600010731     O          E            LV21H       1
021700010731     O                                            1 '"'
021800010731     O                       GG                   3
021900010731     O                                            4 '/'
022000010731     O                       MM                   6
022100010731     O                                            7 '/'
022200010731     O                       AA                   9
022300010731     O                                           11 '";'
022400010731     O********* Linea FINALE Arrivo   + decodifica
022500010731     O          E            LV21D6      1
022600010731     O*                                       CARATTERE
022700010731     O                                           21 'DCH;50;DCW;45;ICS;03;'
022800010731     O          E            LV21D6      1
022900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
023000020131     O                                           17 'VBR;085;HBR;140;'
023100010731     O          E            LV21D6      1
023200010731     O                                            1 '"'
023300010731     O                       §RI                 26
023400010731     O                                           28 '";'
023500010731     O********* TERMINAL DA CARICARE
023600010731     O          E            LV21D7      1
023700010731     O*                                       CARATTERE
023800010731     O                                           21 'DCH;15;DCW;20;ICS;03;'
023900010731     O          E            LV21D7      1
024000010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
024100020131     O                                           16 'VBR;105;HBR;140;'
024200010731     O          E            LV21D7      1
024300010731     O                                           24 '"Terminal da Caricare:";'
024400010731     O          E            LV21D7      1
024500010731     O*                                       CARATTERE
024600010731     O                                           21 'DCH;30;DCW;35;ICS;03;'
024700010731     O* 1° riga di terminal
024800010731     O          E            LV21D7      1
024900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
025000020131     O                                           16 'VBR;130;HBR;120;'
025100010731     O          E            LV21D7      1
025200010731     O                                            1 '"'
025300010731     O                       §TC1                25
025400010731     O                                           27 '";'
025500010731     O* 2° riga di terminal
025600010731     O          E            LV21D7      1
025700010731     O*                                       CARATTERE
025800010731     O                                           21 'DCH;25;DCW;30;ICS;03;'
025900010731     O          E            LV21D7      1
026000010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
026100020131     O                                           16 'VBR;150;HBR;150;'
026200010731     O          E            LV21D7      1
026300010731     O                                            1 '"'
026400010731     O                       §TC2                33
026500010731     O                                           35 '";'
026600010731     O* 3° riga di terminal
026700010731     O          E            LV21D7      1
026800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
026900020131     O                                           16 'VBR;175;HBR;150;'
027000010731     O          E            LV21D7      1
027100010731     O                                            1 '"'
027200010731     O                       §TC3                33
027300010731     O                                           35 '";'
027400010731     O* 4° riga di terminal
027500010731     O          E            LV21D7      1
027600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
027700020131     O                                           16 'VBR;200;HBR;150;'
027800010731     O          E            LV21D7      1
027900010731     O                                            1 '"'
028000010731     O                       §TC4                33
028100010731     O                                           35 '";'
028200010731     O********* Bar Code distinta
028300010731     O          E            LV21D8      1
028400020131     O                                           16 'VBR;270;HBR;090;'
028500010731     O          E            LV21D8      1
028600010731     O*                                      BARCODE
028700010731     O                                           22 'BDEF;8;BNEW;3;BWEW;10;'
028800010731     O          E            LV21D8      1
028900010731     O                                           14 'BCSH;065;BCST;'
029000010731     O*
029100010731     O          E            LV21D8      1
029200010731     O                       §§CDB2               9
029300010731     O          E            LV21D8      1
029400010731     O                                            5 'BSTP;'
029500910403     O********* Chiusura etchetta
029600970421     O          E            LV21D9      1
029700951021     O********                               CON COVERTITORE
029800970430     O                                            9 'TRM;%%5C%'
029900951021     O********                               CON VIDEO
030000970430     O**                                  5 'TRM;\'
