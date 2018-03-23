000100910403     H DECEDIT('0,') DATEDIT(*DMY.)
000200950515     F* FNLV25R  *---------------------------------------------------*
000300970421     F*               STAMPA SEGNACOLLO DISTINTA USCITA              *
000400970221     F*               CON RJS  QB440                                 *
000500910403     F*--------------------------------------------------------------*
000600941216     FFNLV22P   O    F  132        PRINTER
000700950523     D                 DS
000800950523     D  §CB                    1      6  0
000900950523     D                                     DIM(6)
001000950523     D  §CA                    1      6
001100950523     D                                     DIM(6)
001200950523     D                 DS
001300950523     D  §2                     1      2  0
001400950523     D                                     DIM(2)
001500950523     D  §2A                    1      2
001600950523     D                                     DIM(2)
001700950525     D                 DS
001800950525     D  §C                     1      5  0
001900950523     D                                     DIM(5)
002000950525     D  WNFV                   1      5
002100950523     D DSLV25        E DS                  EXTNAME(FNLV25DS)
002200910515     D                 DS
002300950515     D  DATA                   1      8  0
002400950515     D  GG                     7      8
002500950515     D  MM                     5      6
002600950515     D  AA                     1      4
002700910403     I/SPACE 3
002800910403     C*---------------------------------------------------------------*
002900910521     C     *ENTRY        PLIST
003000950523     C                   PARM                    DSLV25
003100950525     C* ACCORCIA ANAGRAFICA PADRONCINO
003200950525     C                   MOVEL     D25RSC        W25RSC           25
003300950607     C**
003400910403     C*
003500910515     C** INIZIALIZZA STAMPANTE
003600950523     C                   EXCEPT    LV25
003700910403     C*
003800950523     C** STAMPA cod.padroncino
003900950523     C                   EXCEPT    LV25D1
004000910515     C*
004100950523     C*  STAMPA descrizione padroncino
004200950523     C                   EXCEPT    LV25H
004300950525     C*
004400950525     C*  STAMPA DATA DISTINTA
004500950525     C                   EXCEPT    LV25H1
004600950525     C*
004700950525     C* STAMPA Numero bolle
004800950525     C                   EXCEPT    LV25D8
004900950525     C*
005000950525     C* STAMPA note
005100950525     C                   EXCEPT    LV25D7
005200910515     C*
005300950525     C* STAMPA NUMERO DISTINTA
005400950523     C                   EXCEPT    LV25X2
005500950523     C*
005600950523     C* STAMPA BARCODE
005700950523     C*
005800950523     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
005900950525     C                   MOVE      D25NFV        WNFV              5
006000950523     C                   EXSR      M10
006100950523     C                   MOVEA     §CA           §C6A              6
006200950523     C                   MOVE      §C6A          §CDBAR            6
006300950523     C* (CAMPO UNICO BARCODE E COSTANTI)
006400950523     C                   MOVE      *BLANK        §§CDB1            7
006500950525     C                   MOVE      §CDBAR        §§CDB1
006600950523     C                   MOVEL     '"'           §§CDB1
006700950523     C                   MOVE      *BLANK        §§CDB2            9
006800950523     C                   MOVEL     §§CDB1        §§CDB2
006900950523     C                   MOVE      '";'          §§CDB2
007000950523     C                   EXCEPT    LV25D6
007100910403     C*
007200910403     C* STAMPA CHIUSURA ETICHETTA
007300950523     C                   EXCEPT    LV25D9
007400910403     C*
007500910403     C                   SETON                                        LR
007600950523     C/SPACE 005
007700950523     C*****************************************************
007800950523     C* CALCOLA CIFRA DI CONTROLLO SECONDO IL MODULO 10 IBM
007900950523     C*****************************************************
008000950523     C     M10           BEGSR
008100950523     C                   MOVE      *ZERO         §TT               3 0
008200950523     C                   MOVE      *ZERO         §TT2              3 0
008300950523     C                   MOVE      *ZERO         §1                1 0
008400950523     C                   Z-ADD     6             §S                2 0
008500950523     C                   DO        5             §N                2 0
008600950523     C                   SUB       1             §S
008700950523     C* INTANTO SPOSTO DA UNA SCHIERA AL CAMPO COD.BARRE
008800950523     C                   MOVE      §C(§N)        §CB(§N)
008900950523     C     §N            DIV       2             §CTR              3 0
009000950523     C                   MVR                     §RST              3 010
009100950523     C     *IN(10)       IFEQ      '1'
009200950523     C     §C(§S)        MULT      2             §N2               2 0
009300950523     C                   MOVE      §N2           §A2               2
009400950523     C                   MOVEA     §A2           §2A
009500950523     C                   XFOOT     §2            §CTR
009600950523     C                   ELSE
009700950523     C                   Z-ADD     §C(§S)        §CTR
009800950523     C                   END
009900950523     C                   ADD       §CTR          §TT
010000950523     C                   END
010100950523     C*
010200950523     C     §TT           ADD       9             §TT2
010300950523     C                   MOVE      0             §TT2
010400950523     C     §TT2          SUB       §TT           §1
010500950523     C*
010600950523     C                   MOVE      §1            §CB(6)
010700950523     C                   ENDSR
010800950523     OFNLV22P   E            LV25        1
010900970221     O                                            9 '~^"LV25";'
011000950523     O          E            LV25        1
011100910403     O*LOGICA
011200010731     O                                           16 '00001;0;265;070;'
011300950523     O          E            LV25        1
011400910403     O*FISICA
011500010731     O                                           21 'UTOF;2700;TEAR;0;SPB;'
011600950523     O          E            LV25        1
011700910403     O*GIRO SCRITTA A 180°
011800010731     O                                            9 'FLIP;300;'
011900950523     O          E            LV25        1
012000910403     O*                                       FONT
012100910403     O                                            7 'DHR;$1;'
012200950523     O********* Costante Padroncino
012300950523     O          E            LV25D1      1
012400910403     O*                                       CARATTERE
012500010731     O                                           21 'DCH;30;DCW;30;ICS;03;'
012600950523     O          E            LV25D1      1
012700910403     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
012800010731     O                                           15 'VBR;30;HBR;120;'
012900950523     O          E            LV25D1      1
013000910403     O                                            1 '"'
013100950525     O                                           12 'Padroncino:'
013200950525     O                       D25PDR              23 '   .    '
013300950525     O                                           25 '";'
013400950523     O********* descrizione padroncino
013500950523     O          E            LV25H       1
013600910403     O*                                       CARATTERE
013700010731     O                                           21 'DCH;30;DCW;35;ICS;03;'
013800950523     O          E            LV25H       1
013900910516     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
014000010731     O                                           16 'VBR;065;HBR;120;'
014100950523     O          E            LV25H       1
014200950523     O                                            1 '"'
014300950525     O                       W25RSC              26
014400950525     O                                           28 '";'
014500950525     O********* DATA DISTINTA
014600950525     O          E            LV25H1      1
014700950525     O*                                       CARATTERE
014800010731     O                                           21 'DCH;25;DCW;25;ICS;03;'
014900950525     O          E            LV25H1      1
015000950525     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
015100010731     O                                           16 'VBR;090;HBR;120;'
015200950525     O          E            LV25H1      1
015300950525     O                                            6 '"Data:'
015400950525     O                       D25DDC              20 '  /  /    '
015500950525     O                                           23 '";'
015600950525     O********* Num. Bolle
015700950525     O          E            LV25D7      1
015800950525     O*                                       CARATTERE
015900950525     O                                           21 'DCH;20;DCW;20;ICS;03;'
016000950525     O          E            LV25D7      1
016100950525     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
016200010731     O                                           17 'VBR;120;HBR;120;'
016300950525     O          E            LV25D7      1
016400950525     O                                           12 '"N.Bolle: ";'
016500950525     O          E            LV25D7      1
016600950525     O*                                       CARATTERE
016700010731     O                                           21 'DCH;40;DCW;30;ICS;03;'
016800950525     O          E            LV25D7      1
016900950525     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
017000011114     O                                           16 'VBR;120;HBR;300;'
017100950525     O          E            LV25D7      1
017200950525     O                                            1 '"'
017300950525     O                       D25NBL        Z      6
017400950525     O                                            8 '";'
017500950525     O********* Note
017600950525     O          E            LV25D8      1
017700950525     O*                                       CARATTERE
017800950525     O                                           21 'DCH;30;DCW;30;ICS;03;'
017900950525     O          E            LV25D8      1
018000950525     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
018100010731     O                                           16 'VBR;150;HBR;100;'
018200950525     O          E            LV25D8      1
018300950525     O                                            1 '"'
018400950525     O                       D25NOT              38
018500950525     O                                           40 '";'
018600950523     O********* Bar Code distinta
018700950523     O          E            LV25D6      1
018800010731     O                                           16 'VBR;240;HBR;090;'
018900950523     O          E            LV25D6      1
019000950523     O*                                      BARCODE
019100950523     O                                           22 'BDEF;8;BNEW;3;BWEW;10;'
019200950523     O          E            LV25D6      1
019300010731     O                                           14 'BCSH;080;BCST;'
019400950523     O*
019500950523     O          E            LV25D6      1
019600950523     O                       §§CDB2               9
019700950523     O          E            LV25D6      1
019800950523     O                                            5 'BSTP;'
019900950525     O********* N° distinta
020000950525     O          E            LV25X2      1
020100950525     O*                                       CARATTERE
020200950525     O                                           21 'DCH;20;DCW;20;ICS;03;'
020300950525     O          E            LV25X2      1
020400950525     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
020500010731     O                                           16 'VBR;190;HBR;360;'
020600950525     O          E            LV25X2      1
020700950525     O                                           16 '"Num.Distinta";'
020800950525     O          E            LV25X2      1
020900950525     O*                                       CARATTERE
021000010731     O                                           23 'DCH;050;DCW;050;ICS;03;'
021100950525     O          E            LV25X2      1
021200950525     O*                         POSIZIONE: VERT.(A/B) ORIZZ.(D-S)
021300010731     O                                           16 'VBR;230;HBR;360;'
021400950525     O          E            LV25X2      1
021500950525     O                                            1 '"'
021600950525     O                       D25NFV               6
021700950525     O                                            8 '";'
021800910403     O********* Chiusura etchetta
021900950523     O          E            LV25D9      1
022000970221     O                                            5 'TRM;\'
