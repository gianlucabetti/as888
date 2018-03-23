000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200950515     F* FNLV25R  *---------------------------------------------------*
000300970421     F*               STAMPA SEGNACOLLO SPUNTA USCITA                *
000400910403     F*--------------------------------------------------------------*
000500941216     FFNLV22P   O    F  132        PRINTER
000600950523     D                 DS
000700950523     D  §CB                    1      6  0
000800950523     D                                     DIM(6)
000900950523     D  §CA                    1      6
001000950523     D                                     DIM(6)
001100950523     D                 DS
001200950523     D  §2                     1      2  0
001300950523     D                                     DIM(2)
001400950523     D  §2A                    1      2
001500950523     D                                     DIM(2)
001600950525     D                 DS
001700950525     D  §C                     1      5  0
001800950523     D                                     DIM(5)
001900950525     D  WNFV                   1      5
002000950523     D DSLV25        E DS                  EXTNAME(FNLV25DS)
002100910515     D                 DS
002200950515     D  DATA                   1      8  0
002300950515     D  GG                     7      8
002400950515     D  MM                     5      6
002500950515     D  AA                     1      4
002600910403     I/SPACE 3
002700910403     C*---------------------------------------------------------------*
002800910521     C     *ENTRY        PLIST
002900950523     C                   PARM                    DSLV25
003000950525     C* ACCORCIA ANAGRAFICA PADRONCINO
003100950525     C                   MOVEL     D25RSC        W25RSC           25
003200910403     C*
003300910515     C** INIZIALIZZA STAMPANTE
003400950523     C                   EXCEPT    LV25
003500910403     C*
003600950523     C** STAMPA cod.padroncino
003700950523     C                   EXCEPT    LV25D1
003800910515     C*
003900950523     C*  STAMPA descrizione padroncino
004000950523     C                   EXCEPT    LV25H
004100950525     C*
004200950525     C*  STAMPA DATA DISTINTA
004300950525     C                   EXCEPT    LV25H1
004400950525     C*
004500950525     C* STAMPA Numero bolle
004600950525     C                   EXCEPT    LV25D8
004700950525     C*
004800950525     C* STAMPA note
004900950525     C                   EXCEPT    LV25D7
005000910515     C*
005100950525     C* STAMPA NUMERO DISTINTA
005200950523     C                   EXCEPT    LV25X2
005300950523     C*
005400950523     C* STAMPA BARCODE
005500950523     C*
005600950523     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
005700950525     C                   MOVE      D25NFV        WNFV              5
005800950523     C                   EXSR      M10
005900950523     C                   MOVEA     §CA           §C6A              6
006000950523     C                   MOVE      §C6A          §CDBAR            6
006100950523     C* (CAMPO UNICO BARCODE E COSTANTI)
006200950523     C                   MOVE      *BLANK        §§CDB1            7
006300950525     C                   MOVE      §CDBAR        §§CDB1
006400950523     C                   MOVEL     '"'           §§CDB1
006500950523     C                   MOVE      *BLANK        §§CDB2            9
006600950523     C                   MOVEL     §§CDB1        §§CDB2
006700950523     C                   MOVE      '";'          §§CDB2
006800950523     C                   EXCEPT    LV25D6
006900910403     C*
007000910403     C* STAMPA CHIUSURA ETICHETTA
007100950523     C                   EXCEPT    LV25D9
007200910403     C*
007300910403     C                   SETON                                        LR
007400950523     C/SPACE 005
007500950523     C*****************************************************
007600950523     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
007700950523     C*****************************************************
007800950523     C     M10           BEGSR
007900950523     C                   MOVE      *ZERO         §TT               3 0
008000950523     C                   MOVE      *ZERO         §TT2              3 0
008100950523     C                   MOVE      *ZERO         §1                1 0
008200950523     C                   Z-ADD     6             §S                2 0
008300950523     C                   DO        5             §N                2 0
008400950523     C                   SUB       1             §S
008500950523     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
008600950523     C                   MOVE      §C(§N)        §CB(§N)
008700950523     C     §N            DIV       2             §CTR              3 0
008800950523     C                   MVR                     §RST              3 010
008900950523     C     *IN(10)       IFEQ      '1'
009000950523     C     §C(§S)        MULT      2             §N2               2 0
009100950523     C                   MOVE      §N2           §A2               2
009200950523     C                   MOVEA     §A2           §2A
009300950523     C                   XFOOT     §2            §CTR
009400950523     C                   ELSE
009500950523     C                   Z-ADD     §C(§S)        §CTR
009600950523     C                   END
009700950523     C                   ADD       §CTR          §TT
009800950523     C                   END
009900950523     C*
010000950523     C     §TT           ADD       9             §TT2
010100950523     C                   MOVE      0             §TT2
010200950523     C     §TT2          SUB       §TT           §1
010300950523     C*
010400950523     C                   MOVE      §1            §CB(6)
010500950523     C                   ENDSR
010600950523     OFNLV22P   E            LV25        1
010700951021     O******                                 inizializz.CON CONVERTIT.
010800951021     O                                           20 '&&??%%P%%7E%^"LV25";'
010900950523     O          E            LV25        1
011000910403     O*LOGICA
011100010731     O                                           16 '00001;0;265;000;'
011200950523     O          E            LV25        1
011300910403     O*FISICA
011400010731     O                                           14 'UTOF;2700;SPB;'
011500950523     O          E            LV25        1
011600910403     O*GIRO SCRITTA A 180°
011700010731     O                                            9 'FLIP;300;'
011800950523     O          E            LV25        1
011900910403     O*                                       FONT
012000910403     O                                            7 'DHR;$1;'
012100950523     O********* Costante Padroncino
012200950523     O          E            LV25D1      1
012300010731     O*                                       CARATTERE
012400010731     O                                           21 'DCH;30;DCW;30;ICS;03;'
012500010731     O          E            LV25D1      1
012600010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
012700010731     O                                           15 'VBR;30;HBR;120;'
012800010731     O          E            LV25D1      1
012900010731     O                                            1 '"'
013000010731     O                                           12 'Padroncino:'
013100010731     O                       D25PDR              23 '   .    '
013200010731     O                                           25 '";'
013300010731     O********* descrizione padroncino
013400010731     O          E            LV25H       1
013500010731     O*                                       CARATTERE
013600010731     O                                           21 'DCH;30;DCW;35;ICS;03;'
013700010731     O          E            LV25H       1
013800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
013900010731     O                                           16 'VBR;065;HBR;120;'
014000010731     O          E            LV25H       1
014100010731     O                                            1 '"'
014200010731     O                       W25RSC              26
014300010731     O                                           28 '";'
014400010731     O********* DATA DISTINTA
014500010731     O          E            LV25H1      1
014600010731     O*                                       CARATTERE
014700010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
014800010731     O          E            LV25H1      1
014900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
015000010731     O                                           16 'VBR;090;HBR;120;'
015100010731     O          E            LV25H1      1
015200010731     O                                            6 '"Data:'
015300010731     O                       D25DDC              20 '  /  /    '
015400010731     O                                           23 '";'
015500010731     O********* Num. Bolle
015600010731     O          E            LV25D7      1
015700010731     O*                                       CARATTERE
015800010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
015900010731     O          E            LV25D7      1
016000010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016100010731     O                                           17 'VBR;120;HBR;120;'
016200010731     O          E            LV25D7      1
016300010731     O                                           12 '"N.Bolle: ";'
016400010731     O          E            LV25D7      1
016500010731     O*                                       CARATTERE
016600010731     O                                           21 'DCH;40;DCW;30;ICS;03;'
016700010731     O          E            LV25D7      1
016800010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016900011114     O                                           16 'VBR;120;HBR;300;'
017000010731     O          E            LV25D7      1
017100010731     O                                            1 '"'
017200010731     O                       D25NBL        Z      6
017300010731     O                                            8 '";'
017400010731     O********* Note
017500010731     O          E            LV25D8      1
017600010731     O*                                       CARATTERE
017700010731     O                                           21 'DCH;30;DCW;30;ICS;03;'
017800010731     O          E            LV25D8      1
017900010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
018000010731     O                                           16 'VBR;150;HBR;030;'
018100010731     O          E            LV25D8      1
018200010731     O                                            1 '"'
018300010731     O                       D25NOT              38
018400010731     O                                           40 '";'
018500010731     O********* Bar Code distinta
018600010731     O          E            LV25D6      1
018700010731     O                                           16 'VBR;240;HBR;090;'
018800010731     O          E            LV25D6      1
018900010731     O*                                      BARCODE
019000010731     O                                           22 'BDEF;8;BNEW;3;BWEW;10;'
019100010731     O          E            LV25D6      1
019200010731     O                                           14 'BCSH;080;BCST;'
019300010731     O*
019400010731     O          E            LV25D6      1
019500010731     O                       §§CDB2               9
019600010731     O          E            LV25D6      1
019700010731     O                                            5 'BSTP;'
019800010731     O********* N° distinta
019900010731     O          E            LV25X2      1
020000010731     O*                                       CARATTERE
020100010731     O                                           21 'DCH;20;DCW;20;ICS;03;'
020200010731     O          E            LV25X2      1
020300010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020400010731     O                                           16 'VBR;190;HBR;360;'
020500010731     O          E            LV25X2      1
020600010731     O                                           16 '"Num.Distinta";'
020700010731     O          E            LV25X2      1
020800010731     O*                                       CARATTERE
020900010731     O                                           23 'DCH;050;DCW;050;ICS;03;'
021000010731     O          E            LV25X2      1
021100010731     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
021200010731     O                                           16 'VBR;230;HBR;360;'
021300010731     O          E            LV25X2      1
021400010731     O                                            1 '"'
021500010731     O                       D25NFV               6
021600010731     O                                            8 '";'
021700910403     O********* Chiusura etchetta
021800950523     O          E            LV25D9      1
021900951021     O********                               CON COVERTITORE
022000951021     O                                            9 'TRM;%%5C%'
022100951021     O********                               CON VIDEO
022200951021     O**                                  5 'TRM;\'
