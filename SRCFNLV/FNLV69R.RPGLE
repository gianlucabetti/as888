000100980223     H DECEDIT('0,') DATEDIT(*DMY.)
000200010320
000300020621     FFLTR351L  IF   E           K DISK
000400980303     FAZORG01L  IF   E           K DISK
000500980305     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600010320
000700020621     D TE              S             66    DIM(16) CTDATA PERRCD(1)
000800020621      *
000900020621     D KPJBA         E DS
001000980302     D PARAM1          DS                  INZ
001100980302     D  PA1CTL                 1      1
001200980302     D  PA1DPL                 2      3
001300980302     D  PA1APL                 4      5
001400020621     D  PA1POG                 6      8
001500020621      *
001600020621     D UT§DSE0F      E DS
001700020621     D CNCR80        E DS
001800010320     D dtr3des       e ds
001900020621      *
002000020621     D W0140           S             14  0 inz
002100020621     D UDATE8          S              8  0 inz
002200020621     D UTIME           S              6  0 inz
002300020621     D RSUT            S             20    inz
002400020621     D WRKBAI          S              2    inz
002500020621     D SA1NPL          S                   like(TR3npl) inz
002600020621     D STADES          S             20    inz
002700020621     D STPDES          S             20    inz
002800010320
002900020621      *****************************************************************
003000020621      * RIEPILOGO INDICATORI
003100020621      *****************************************************************
003200020621      * 31/   - DI COMODO
003300020621      * 40/42 - ERRORI
003400020621      *****************************************************************
003500020621      *----------------------------------------------------------------
003600020621      * DEFINIZIONE CHIAVI
003700020621     C     K03TR350      KLIST
003800020621     C                   KFLD                    PA1POG
003900020621     C                   KFLD                    PA1CTL
004000020621     C                   KFLD                    PA1DPL
004100020621     C     K02TR350      KLIST
004200020621     C                   KFLD                    PA1POG
004300020621     C                   KFLD                    PA1CTL
004400020621      *----------------------------------------------------------------
004500980223     C     *ENTRY        PLIST
004600980223     C                   PARM                    KPJBA
004700980303     C                   MOVEL     KPJBU         PARAM1
004800980303      *
004900020621      * PRENDO DATA E ORA CORRENTE
005000020621     C                   TIME                    W0140
005100020621     C                   MOVE      W0140         UDATE8
005200020621     C                   MOVEL     W0140         UTIME
005300980303      *
005400980223     C                   Z-ADD     1             CODUT
005500980223     C                   CALL      'X§PARUT'
005600020621     C                   PARM                    UT§DSE0F
005700020621     C                   MOVEL     RAGUT         RSUT
005800980223     C                   MOVEL     REC80         CNCR80
005900020621      *----------------------------------------------------------------
006000020621     C     K03TR350      SETLL     FLTR3500
006100020621     C     K02TR350      READE     FLTR3500                               33
006200020621    1C     *IN33         DOWEQ     *OFF
006300020621      *
006400020621      * CONTROLLO CON I LIMITI IMPOSTATI A VIDEO
006500020621    2C     TR3NPL        IFLE      PA1APL
006600020621      *
006700020621    3C     SA1NPL        IFEQ      *BLANK
006800980305     C     SA1NPL        ORNE      TR3NPL
006900020621      * SE NON E' IL PRIMO PIANO DI SMISTAMENTO CHE STAMPO STAMPO LA
007000020621      *  RIGA DI FINE PIANO
007100020621    4C     SA1NPL        IFNE      *BLANKS
007200980305     C                   EXCEPT    FIN
007300020621   -4C                   ENDIF
007400020621      * STAMPO LA TESTA
007500980305     C                   SETON                                            10
007600980304     C                   EXCEPT    TESTA
007700980305     C                   SETON                                            10
007800020621     C                   MOVE      TR3NPL        SA1NPL
007900020621   -3C                   ENDIF
008000020621      * METTO LA BAIA IN UN CAMPO DI 2 CARATTTERI
008100020621     C                   MOVE      TR3BAI        WRKBAI
008200020621      * SE LE LINEE HANNO UN VALORE DI 998 O 999 O 997 O 996 o 995
008300020621      * NON VADO A RECUPERARE LA DECODIFICA DAL FILE AZORG
008400020621    3C                   SELECT
008500020621     C     TR3LNP        WHENEQ    '999'
008600980305     C                   MOVEL     *BLANKS       STPDES
008700980305     C                   MOVEL     'TUTTE'       STPDES
008800020621      *
008900000221     C     TR3LNP        WHENEQ    '998'
009000980305     C                   MOVEL     *BLANKS       STPDES
009100020621     C                   MOVEL     TE(13)        STPDES
009200020621      *
009300000221     C     TR3LNP        WHENEQ    '997'
009400000221     C                   MOVEL     *BLANKS       STPDES
009500020621     C                   MOVEL     TE(14)        STPDES
009600020621      *
009700000221     C     TR3LNP        WHENEQ    '996'
009800000221     C                   MOVEL     *BLANKS       STPDES
009900020621     C                   MOVEL     TE(15)        STPDES
010000020621      *
010100020621     C     TR3LNP        WHENEQ    '995'
010200020621     C                   MOVEL     *BLANKS       STPDES
010300020621     C                   MOVEL     TE(16)        STPDES
010400020621      *
010500020621   X3C                   OTHER
010600020621     C                   MOVE      TR3LNP        ORGfil
010700020621     C     ORGfil        CHAIN     AZORG01L                           34
010800020621     C  N34              MOVEL     ORGDES        STPDES
010900980304     C   34              MOVEL     *BLANKS       STPDES
011000020621      *
011100020621   -3C                   ENDSL
011200020621      *
011300020621    3C                   SELECT
011400020621      *
011500000221     C     TR3LNA        WHENEQ    '999'
011600980305     C                   MOVEL     *BLANKS       STADES
011700980305     C                   MOVEL     'TUTTE'       STADES
011800020621      *
011900000221     C     TR3LNA        WHENEQ    '998'
012000980305     C                   MOVEL     *BLANKS       STADES
012100020621     C                   MOVEL     TE(13)        STADES
012200020621      *
012300000221     C     TR3LNA        WHENEQ    '997'
012400000221     C                   MOVEL     *BLANKS       STADES
012500020621     C                   MOVEL     TE(14)        STADES
012600020621      *
012700000221     C     TR3LNA        WHENEQ    '996'
012800000221     C                   MOVEL     *BLANKS       STADES
012900020621     C                   MOVEL     TE(15)        STADES
013000020621      *
013100020621     C     TR3LNA        WHENEQ    '995'
013200020621     C                   MOVEL     *BLANKS       STADES
013300020621     C                   MOVEL     TE(16)        STADES
013400020621      *
013500020621   X3C                   OTHER
013600020621     C                   MOVE      TR3LNA        ORGfil
013700020621     C     ORGfil        CHAIN     AZORG01L                           34
013800020621     C  N34              MOVEL     ORGDES        STADES
013900980304     C   34              MOVEL     *BLANKS       STADES
014000020621      *
014100020621   -3C                   ENDSL
014200980304      *
014300980304     C   OF              EXCEPT    TESTA
014400980304     C   OF              SETOFF                                       OF
014500020621      *
014600010627     C                   MOVEL     TR3DES        dtr3des
014700020621      *
014800980304     C                   EXCEPT    DET
014900020621     C     K02TR350      READE     FLTR3500                               33
015000020621      *
015100020621   X2C                   ELSE
015200980305     C                   SETON                                        33
015300020621      *
015400020621   -2C                   ENDIF
015500980305      *
015600020621   -1C                   ENDDO
015700980305      *
015800020621    1C     SA1NPL        IFNE      *BLANKS
015900980305     C                   EXCEPT    FIN
016000020621   -1C                   ENDIF
016100020621     C                   MOVEL     *ON           *INLR
016200020621      *
016300980305     OQSYSPRT   E            TESTA            02
016400980303     O                       TE(1)               66
016500980303     O                       TE(2)              132
016600980303     O                       RSUT                20
016700980303     O                                           21 '/'
016800980303     O                       SIMFEL              24
016900980303     O                       UDATE8             119 '  /  /    '
017000980303     O                       PAGE          Z    132
017100020621     O          E            TESTA            03
017200980303     O                       KNSIF               10
017300980303     O                       KNMUS               31
017400980305     O                       UTIME              119 '  :  :  '
017500020621     O          E            TESTA       0
017600020621     O                       TE(8)              108
017700020621     O                       PA1POG              78
017800020621     O          E            TESTA       1
017900980305     O                       TE(5)               66
018000980305     O                       TE(6)              132
018100020621     O                       PA1CTL              57
018200020621     O                       PA1DPL              72
018300020621     O                       PA1APL              86
018400020621     O          E       10   TESTA       2
018500980305     O                       TE(7)              108
018600980305     O                       TR3CTL              71
018700980305     O                       TR3NPL              76
018800020621     O          E            TESTA       1
018900020621     O                       TE(11)              66
019000020621     O                       TE(12)             132
019100020621     O          E            TESTA       1
019200980305     O                       TE(3)               66
019300980305     O                       TE(4)              132
019400020621     O          E            TESTA       1
019500020621     O                       TE(11)              66
019600020621     O                       TE(12)             132
019700980305     O          E            DET         1
019800020621     O                       TE(9)               66
019900020621     O                       TE(10)             132
020000980305     O          E            DET         1
020100020621     O                       TE(9)               66
020200020621     O                       TE(10)             132
020300980305     O                       TR3LNP               5
020400980305     O                                              '_'
020500980305     O                       STPDES              26
020600980305     O                       TR3LNA              32
020700980305     O                                              '_'
020800980305     O                       STADES              53
020900980305     O                       TR3ZON              59
021000980305     O                       WRKBAI              66
021100010320     O                       §tr3pista           73
021200010320     O                       §tr3operat          80
021300010320     O                       §tr3pallet          88
021400010320     O                       §tr3descri         119
021500010320     O                       TR3PRB             125
021600980305     O          E            FIN         1
021700020621     O                       TE(11)              66
021800020621     O                       TE(12)             132
021900000719**
022000980305                                     ***  P I A N I    D I    S M    1
022100980420I S T A M E N T O  ***         FNLV69R                   PAG. XXXX   2
022200980305! Linea  Partenza          ! Linea Arrivo             ! Zona ! Bai   3
022300010320a ! Pista ! Opt ! Pallet ! Descrizione                ! Prg !        4
022400020621                                       Tipo Lavorazione X  dal Pia   5
022500020621no  XX  al Piano  XX                                                 6
022600980305****   Piano di smistamento X / XX   ****                            7
022700020621***  P.O. di gestione del piano  XXX  ***                            8
022800020621!                          !                          !      !       9
022900020621  !       !     !        !                            !     !        10
023000020621.__________________________.__________________________.______.____   11
023100020621__._______._____.________.____________________________._____.        12
023200020621NON LETTI                                                            13
023300020621NON QUADRATI                                                         14
023400020621NON STAMPATI                                                         15
023500020621INVERSIONI                                                           16
