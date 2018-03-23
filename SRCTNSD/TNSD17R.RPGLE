000100941115     H* TNSD17R *----------------------------------------------------*
000200900405     H*         - STAMPA ORGANIGRAMMA - FILTRO -                     *
000300000000     H*--------------------------------------------------------------*
000400900405     FAZORG01L  IF   E           K DISK
000500941115     FTNSD17D   CF   E             WORKSTN
000600020621     DTNSD17DS       E DS
000700020621     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000800020621     D S02DS         E DS                  EXTNAME(TIBS02DS)
000900900131     D KPJBA         E DS
001000900131     D  LIBSYS                92    101
001100910226     D  §SEDE                247    250
001200900131     C*---------------------------------------------------------------*
001300900131     C     *ENTRY        PLIST
001400900131     C                   PARM                    KPJBA
001500900131     C                   Z-ADD     1             CODUT
001600020621     C                   CALL      'X§PARUT'
001700020621     C                   PARM                    UT§DSE
001800900131     C                   MOVEL     RAGUT         RSUT             20
001900910226     C     §SEDE         COMP      'SEDE'                                 88
002000000000     C*----------------------------------------------------*
002100000000     C     CICLO         TAG
002200000000     C                   Z-ADD     0             VCFI
002300900405     C                   Z-ADD     0             VNPG
002400900405     C                   MOVEL     'S'           VTST
002500000000     C     INIZIO        TAG
002600020621      *
002700020621     C                   EXFMT     SD17D01
002800020621     C   KC              GOTO      FINE
002900041001     c                   setoff                                       98
003000000000     C* CONTROLLI:
003100000000     C** FILIALE
003200910226     C  N88VCFI          IFNE      999
003300900405     C     VCFI          CHAIN     AZORG                              50
003400000000     C   50              GOTO      INIZIO
003500000000     C                   END
003600030523      * Controlli da non fare se richiesta stampa ORG.SEDE
003700030523     C
003800030523     C  N88              DO
003900030523      *
004000020621      * Network1
004100020621     C                   CLEAR                   VNTW1D
004200020621   1aC     VNTW1         IFNE      *BLANKS
004300020621     C     '?'           SCAN      VNTW1                                  31
004400020621   2aC                   IF        *in31 = *on
004500020621      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
004600020621     C                   SETON                                        98
004700020621     C                   CLEAR                   S02DS
004800020621     C                   MOVEL     'R'           T02MOD
004900020621     C                   MOVEL     KNSIF         T02SIF
005000020621     C                   MOVEL     'NTW'         T02COD
005100020621     C                   CALL      'TIBS02R'
005200020621     C                   PARM                    KPJBA
005300020621     C                   PARM                    S02DS
005400020621   3aC     T02ERR        IFEQ      *BLANKS
005500020621     C                   MOVEL     T02KE1        VNTW1
005600020621     C                   MOVEL     T02UNI        VNTW1D
005700020621   3-C                   ENDIF
005800020621      *
005900020621   2eC                   ELSE
006000020621     C                   CLEAR                   S02DS
006100020621     C                   MOVEL     'C'           T02MOD
006200020621     C                   MOVEL     KNSIF         T02SIF
006300020621     C                   MOVEL     'NTW'         T02COD
006400020621     C                   MOVEL     *BLANKS       T02KE1
006500020621     C                   MOVEL     VNTW1         T02KE1
006600020621     C                   CALL      'TIBS02R'
006700020621     C                   PARM                    KPJBA
006800020621     C                   PARM                    S02DS
006900020621      *
007000020621   3aC     T02ERR        IFEQ      *BLANKS
007100020621     C                   MOVEL     T02UNI        VNTW1D
007200020621     C                   ELSE
007300020621     C                   SETON                                        9851
007400020621   3-C                   ENDIF
007500020621   2-C                   ENDIF
007600020621   1-C                   ENDIF
007700020621      * Network2
007800020621     C                   CLEAR                   VNTW2D
007900020621   1aC     VNTW2         IFNE      *BLANKS
008000020621     C     '?'           SCAN      VNTW2                                  31
008100020621   2aC                   IF        *in31 = *on
008200020621      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
008300020621     C                   SETON                                        98
008400020621     C                   CLEAR                   S02DS
008500020621     C                   MOVEL     'R'           T02MOD
008600020621     C                   MOVEL     KNSIF         T02SIF
008700020621     C                   MOVEL     'NTW'         T02COD
008800020621     C                   CALL      'TIBS02R'
008900020621     C                   PARM                    KPJBA
009000020621     C                   PARM                    S02DS
009100020621   3aC     T02ERR        IFEQ      *BLANKS
009200020621     C                   MOVEL     T02KE1        VNTW2
009300020621     C                   MOVEL     T02UNI        VNTW2D
009400020621   3-C                   ENDIF
009500020621      *
009600020621   2eC                   ELSE
009700020621     C                   CLEAR                   S02DS
009800020621     C                   MOVEL     'C'           T02MOD
009900020621     C                   MOVEL     KNSIF         T02SIF
010000020621     C                   MOVEL     'NTW'         T02COD
010100020621     C                   MOVEL     *BLANKS       T02KE1
010200020621     C                   MOVEL     VNTW2         T02KE1
010300020621     C                   CALL      'TIBS02R'
010400020621     C                   PARM                    KPJBA
010500020621     C                   PARM                    S02DS
010600020621      *
010700020621   3aC     T02ERR        IFEQ      *BLANKS
010800020621     C                   MOVEL     T02UNI        VNTW2D
010900020621   3eC                   ELSE
011000020621     C                   SETON                                        9852
011100020621   3-C                   ENDIF
011200020621   2-C                   ENDIF
011300020621   1-C                   ENDIF
011400020621      * Network3
011500020621     C                   CLEAR                   VNTW3D
011600020621   1aC     VNTW3         IFNE      *BLANKS
011700020621     C     '?'           SCAN      VNTW3                                  31
011800020621   2aC                   IF        *in31 = *on
011900020621      * PER RIEMETTERE IL VIDEO ALMENO UNA VOLTA DOPO LE RICERCHE
012000020621     C                   SETON                                        98
012100020621     C                   CLEAR                   S02DS
012200020621     C                   MOVEL     'R'           T02MOD
012300020621     C                   MOVEL     KNSIF         T02SIF
012400020621     C                   MOVEL     'NTW'         T02COD
012500020621     C                   CALL      'TIBS02R'
012600020621     C                   PARM                    KPJBA
012700020621     C                   PARM                    S02DS
012800020621   3aC     T02ERR        IFEQ      *BLANKS
012900020621     C                   MOVEL     T02KE1        VNTW3
013000020621     C                   MOVEL     T02UNI        VNTW3D
013100020621   3-C                   ENDIF
013200020621      *
013300020621   2eC                   ELSE
013400020621     C                   CLEAR                   S02DS
013500020621     C                   MOVEL     'C'           T02MOD
013600020621     C                   MOVEL     KNSIF         T02SIF
013700020621     C                   MOVEL     'NTW'         T02COD
013800020621     C                   MOVEL     *BLANKS       T02KE1
013900020621     C                   MOVEL     VNTW3         T02KE1
014000020621     C                   CALL      'TIBS02R'
014100020621     C                   PARM                    KPJBA
014200020621     C                   PARM                    S02DS
014300020621      *
014400020621   3aC     T02ERR        IFEQ      *BLANKS
014500020621     C                   MOVEL     T02UNI        VNTW3D
014600020621   3eC                   ELSE
014700020621     C                   SETON                                        9853
014800020621   3-C                   ENDIF
014900020621   2-C                   ENDIF
015000020621   1-C                   ENDIF
015100130530     C                   ENDDO
015200020621     C  NKF
015300020621     COR 98              GOTO      INIZIO
015400000000     C* PASSAGGIO PARAMETRI IN KBJBU E LANCIO BATCH DI STAMPA LETTERE
015500020621     C   KF              DO
015600020621      *
015700020621     C                   CLEAR                   TNSD17DS
015800030523     C                   MOVE      VCFI          D17POS
015900020621     C                   MOVE      VNPG          D17PGI
016000020621     C                   MOVE      VTST          D17TPS
016100020621     C                   MOVEL     VNTW1         D17NTW1
016200020621     C                   MOVEL     VNTW2         D17NTW2
016300020621     C                   MOVEL     VNTW3         D17NTW3
016400020621     C                   MOVE      VTPO          D17TPO
016500981113     C**
016600030331     C  N88              CALL      'TNSD18C1'
016700981113     C                   PARM                    KPJBA
016800020621     C                   PARM                    TNSD17DS
016900981222     C   88              CALL      'TNSD38C'
017000981113     C                   PARM                    KPJBA
017100020621     C                   PARM                    TNSD17DS
017200971124     C                   GOTO      FINE
017300000000     C                   END
017400000000     C                   GOTO      CICLO
017500000000     C     FINE          TAG
017600000000     C                   SETON                                        LR
