000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200970421     F* FNLV21RQ *---------------------------------------------------*
000300970421     F*               STAMPA SEGNACOLLO F.V.                         *
000400970221     F*               CON RJS  QB440                                 *
000500970421     F*          UTILIZZA LA STESSA DS DELL'ETICHETTA BORDERO'       *
000600970421     F*          MA STAMPA SOLO I CAMPI NECESSARI                    *
000700910403     F*--------------------------------------------------------------*
000800941216     FFNLV22P   O    F  132        PRINTER
000900951021     D §RI             S              1    DIM(25)
001000970421     D §TCA            S              4    DIM(30)
001100970421     D                 DS
001200970421     D  §CB                    1      6  0
001300970421     D                                     DIM(6)
001400970421     D  §CA                    1      6
001500970421     D                                     DIM(6)
001600970421     D                 DS
001700970421     D  §2                     1      2  0
001800970421     D                                     DIM(2)
001900970421     D  §2A                    1      2
002000970421     D                                     DIM(2)
002100970421     D                 DS
002200970421     D  §C                     1      5  0
002300970421     D                                     DIM(5)
002400970421     D  WNFV                   1      5
002500910403     D                 DS
002600910515     D  §N5                    1      5  0
002700910515     D  §S5                    1      5
002800910515     D                                     DIM(5)
002900910515     D                 DS
003000910515     D  §N7                    1      7  0
003100910515     D  §S7                    1      7
003200910403     D                                     DIM(7)
003300970421     D                 DS
003400970421     D  §TCAX                  1    120
003500970421     D  §TC1                   1     24
003600970421     D                                     DIM(6)
003700970421     D  §TC2                  25     56
003800970421     D                                     DIM(8)
003900970421     D  §TC3                  57     88
004000970421     D                                     DIM(8)
004100970421     D  §TC4                  89    120
004200970421     D                                     DIM(8)
004300910515     D DSBLB         E DS
004400970421     D  §TC                  112    201  0
004500970421     D                                     DIM(30)
004600910515     D                 DS
004700910515     D  DATA                   1      6  0
004800910515     D  GG                     1      2
004900910515     D  MM                     3      4
005000910515     D  AA                     5      6
005100910403     I/SPACE 3
005200910403     C*---------------------------------------------------------------*
005300910521     C     *ENTRY        PLIST
005400910521     C                   PARM                    DSBLB
005500970421     C*
005600970421     C* EDITO LA SCHIERA
005700970421     C                   DO        30            Y                 2 0
005800970421     C     §TC(Y)        IFGT      *ZERO
005900970421     C                   MOVE      §TC(Y)        §TCA(Y)
006000970421     C                   ENDIF
006100970421     C                   ENDDO
006200970421     C                   MOVEA     §TCA          §TCAX
006300970421     C*
006400970421     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
006500970421     C                   MOVE      §LBNFV        WNFV              5
006600970421     C                   EXSR      M10
006700970421     C                   MOVEA     §CA           §C6A              6
006800970421     C                   MOVE      §C6A          §CDBAR            6
006900970421     C* (CAMPO UNICO BARCODE E COSTANTI)
007000970421     C                   MOVE      *BLANK        §§CDB1            7
007100970421     C                   MOVE      §CDBAR        §§CDB1
007200970421     C                   MOVEL     '"'           §§CDB1
007300970421     C                   MOVE      *BLANK        §§CDB2            9
007400970421     C                   MOVEL     §§CDB1        §§CDB2
007500970421     C                   MOVE      '";'          §§CDB2
007600910515     C** INIZIALIZZA STAMPANTE
007700970421     C                   EXCEPT    LV21
007800910515     C*
007900970421     C                   EXCEPT    LV21D1
008000910515     C*
008100910515     C*  STAMPA N° F.V. e DATA
008200910515     C*
008300910515     C* TOGLI GLI ZERI NON SIGNIFICATIVI DAL NUM. F.V.
008400910403     C                   SETOFF                                       77
008500910515     C                   MOVE      *BLANK        §A5               5
008600910515     C                   Z-ADD     §LBNFV        §N5
008700910515     C                   DO        5             §1                1 0
008800910515     C     §S5(§1)       IFEQ      *ZERO
008900910515     C                   MOVE      *BLANK        §S5(§1)
009000910403     C                   ELSE
009100910403     C                   SETON                                        77
009200910403     C                   END
009300910403     C  N77              END
009400910515     C                   MOVEA     §S5           §A5
009500910515     C* METTO DATA FOGLIO VIAGGIO SU RIGA DI STAMPA
009600910515     C                   MOVE      §LBDAT        DATA
009700910515     C*
009800970421     C                   EXCEPT    LV21H
009900910515     C*
010000970421     C* STAMPA LINEA ARRIVO + DECODIFICA
010100910515     C                   MOVEA     *BLANKS       §RI
010200910516     C                   MOVEA     §LBLNA        §RI(1)
010300910516     C                   MOVEA     §LBDLA        §RI(5)
010400910515     C*
010500970421     C                   EXCEPT    LV21D6
010600910515     C*
010700970421     C* STAMPA TERMINAL DA CARICARE SUL FOGLIO
010800910515     C*
010900970421     C                   EXCEPT    LV21D7
011000970421     C*
011100970421     C* STAMPA BARCODE N.F.V.
011200970421     C*
011300970421     C                   EXCEPT    LV21D8
011400910403     C*
011500910403     C* STAMPA CHIUSURA ETICHETTA
011600970421     C                   EXCEPT    LV21D9
011700910403     C*
011800910403     C                   SETON                                        LR
011900970421     C/SPACE 005
012000970421     C*****************************************************
012100970421     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
012200970421     C*****************************************************
012300970421     C     M10           BEGSR
012400970421     C                   MOVE      *ZERO         §TT               3 0
012500970421     C                   MOVE      *ZERO         §TT2              3 0
012600970421     C                   MOVE      *ZERO         §1                1 0
012700970421     C                   Z-ADD     6             §S                2 0
012800970421     C                   DO        5             §N                2 0
012900970421     C                   SUB       1             §S
013000970421     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
013100970421     C                   MOVE      §C(§N)        §CB(§N)
013200970421     C     §N            DIV       2             §CTR              3 0
013300970421     C                   MVR                     §RST              3 010
013400970421     C     *IN(10)       IFEQ      '1'
013500970421     C     §C(§S)        MULT      2             §N2               2 0
013600970421     C                   MOVE      §N2           §A2               2
013700970421     C                   MOVEA     §A2           §2A
013800970421     C                   XFOOT     §2            §CTR
013900970421     C                   ELSE
014000970421     C                   Z-ADD     §C(§S)        §CTR
014100970421     C                   END
014200970421     C                   ADD       §CTR          §TT
014300970421     C                   END
014400970421     C*
014500970421     C     §TT           ADD       9             §TT2
014600970421     C                   MOVE      0             §TT2
014700970421     C     §TT2          SUB       §TT           §1
014800970421     C*
014900970421     C                   MOVE      §1            §CB(6)
015000970421     C                   ENDSR
015100970421     OFNLV22P   E            LV21        1
015200970421     O                                            9 '~^"LV21";'
015300970421     O          E            LV21        1
015400910403     O*LOGICA
015500010731     O                                           16 '00001;0;265;070;'
015600970421     O          E            LV21        1
015700910403     O*FISICA
015800010731     O                                           21 'UTOF;2700;TEAR;0;SPB;'
015900970421     O          E            LV21        1
016000910403     O*GIRO SCRITTA A 180°
016100010731     O                                            9 'FLIP;300;'
016200970421     O          E            LV21        1
016300910403     O*                                       FONT
016400910403     O                                            7 'DHR;$1;'
016500910515     O********* Foglio Viaggio
016600970421     O          E            LV21D1      1
016700910403     O*                                       CARATTERE
016800010731     O                                           21 'DCH;30;DCW;30;ICS;03;'
016900970421     O          E            LV21D1      1
017000910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
017100010731     O                                           17 'VBR;30;HBR;120;'
017200970421     O          E            LV21D1      1
017300910403     O                                            1 '"'
017400010731     O                                            5 'F.V.'
017500010731     O                                            7 '";'
017600910515     O********* N° F.V. + Data
017700970421     O          E            LV21H       1
017800910403     O*                                       CARATTERE
017900930216     O                                           21 'DCH;20;DCW;20;ICS;03;'
018000970421     O          E            LV21H       1
018100910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
018200010731     O                                           16 'VBR;030;HBR;190;'
018300970421     O          E            LV21H       1
018400941216     O                                            6 '"n.:";'
018500970421     O          E            LV21H       1
018600910516     O*                                       CARATTERE
018700010731     O                                           21 'DCH;30;DCW;40;ICS;03;'
018800970421     O          E            LV21H       1
018900910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
019000010731     O                                           16 'VBR;030;HBR;260;'
019100970421     O          E            LV21H       1
019200910520     O                                            1 '"'
019300951021     O                       §A5                  6
019400951021     O                                            8 '";'
019500970421     O          E            LV21H       1
019600910516     O*                                       CARATTERE
019700930216     O                                           21 'DCH;20;DCW;20;ICS;03;'
019800970421     O          E            LV21H       1
019900910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020000010731     O                                           16 'VBR;030;HBR;400;'
020100970421     O          E            LV21H       1
020200970421     O                                            7 '"del:";'
020300970421     O          E            LV21H       1
020400910516     O*                                       CARATTERE
020500010731     O                                           21 'DCH;30;DCW;40;ICS;03;'
020600970421     O          E            LV21H       1
020700910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020800010731     O                                           16 'VBR;030;HBR;470;'
020900970421     O          E            LV21H       1
021000970423     O                                            1 '"'
021100970423     O                       GG                   3
021200970423     O                                            4 '/'
021300970423     O                       MM                   6
021400970423     O                                            7 '/'
021500970423     O                       AA                   9
021600970423     O                                           11 '";'
021700970421     O********* Linea FINALE Arrivo   + decodifica
021800970421     O          E            LV21D6      1
021900910516     O*                                       CARATTERE
022000010731     O                                           21 'DCH;50;DCW;45;ICS;03;'
022100970421     O          E            LV21D6      1
022200910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
022300010731     O                                           17 'VBR;070;HBR;140;'
022400970421     O          E            LV21D6      1
022500910516     O                                            1 '"'
022600951021     O                       §RI                 26
022700951021     O                                           28 '";'
022800970421     O********* TERMINAL DA CARICARE
022900970421     O          E            LV21D7      1
023000910515     O*                                       CARATTERE
023100970421     O                                           21 'DCH;20;DCW;20;ICS;03;'
023200970421     O          E            LV21D7      1
023300910515     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
023400010731     O                                           16 'VBR;090;HBR;140;'
023500970421     O          E            LV21D7      1
023600970421     O                                           24 '"Terminal da Caricare:";'
023700970421     O          E            LV21D7      1
023800910516     O*                                       CARATTERE
023900010731     O                                           21 'DCH;35;DCW;40;ICS;03;'
024000970421     O* 1° riga di terminal
024100970421     O          E            LV21D7      1
024200910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
024300010731     O                                           16 'VBR;115;HBR;120;'
024400970421     O          E            LV21D7      1
024500910516     O                                            1 '"'
024600970421     O                       §TC1                25
024700970421     O                                           27 '";'
024800970421     O* 2° riga di terminal
024900970421     O          E            LV21D7      1
025000970421     O*                                       CARATTERE
025100010731     O                                           21 'DCH;25;DCW;30;ICS;03;'
025200970421     O          E            LV21D7      1
025300970421     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
025400010731     O                                           16 'VBR;135;HBR;150;'
025500970421     O          E            LV21D7      1
025600970421     O                                            1 '"'
025700970421     O                       §TC2                33
025800970421     O                                           35 '";'
025900970421     O* 3° riga di terminal
026000970421     O          E            LV21D7      1
026100970421     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
026200010731     O                                           16 'VBR;160;HBR;150;'
026300970421     O          E            LV21D7      1
026400970421     O                                            1 '"'
026500970421     O                       §TC3                33
026600970421     O                                           35 '";'
026700970421     O* 4° riga di terminal
026800970421     O          E            LV21D7      1
026900970421     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
027000010731     O                                           16 'VBR;185;HBR;150;'
027100970421     O          E            LV21D7      1
027200970421     O                                            1 '"'
027300970421     O                       §TC4                33
027400970421     O                                           35 '";'
027500970421     O********* Bar Code distinta
027600970421     O          E            LV21D8      1
027700010731     O                                           16 'VBR;255;HBR;090;'
027800970421     O          E            LV21D8      1
027900970421     O*                                      BARCODE
028000970421     O                                           22 'BDEF;8;BNEW;3;BWEW;10;'
028100970421     O          E            LV21D8      1
028200970421     O                                           14 'BCSH;065;BCST;'
028300970421     O*
028400970421     O          E            LV21D8      1
028500970421     O                       §§CDB2               9
028600970421     O          E            LV21D8      1
028700970421     O                                            5 'BSTP;'
028800910403     O********* Chiusura etchetta
028900970421     O          E            LV21D9      1
029000951021     O********                               CON COVERTITORE
029100970221     O**                                  9 'TRM;%%5C%'
029200951021     O********                               CON VIDEO
029300970221     O                                            5 'TRM;\'
