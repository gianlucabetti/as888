000100980223     H DECEDIT('0,') DATEDIT(*DMY.)
000200010320
000300980302     FFLTR300F  IF   E           K DISK
000400980303     FAZORG01L  IF   E           K DISK
000500980305     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600010320
000700000221     D TE              S             66    DIM(14) CTDATA PERRCD(1)
000800980302     D PARAM1          DS                  INZ
000900980302     D  PA1CTL                 1      1
001000980302     D  PA1DPL                 2      3
001100980302     D  PA1APL                 4      5
001200980223     D KPJBA         E DS
001300980302     D CNCR80        E DS
001400010320     D UT§DSE0f      E DS
001500010320     D dtr3des       e ds
001600010320
001700980223     I/SPACE 3
001800980223     C*****************************************************************
001900980223     C* RIEPILOGO INDICATORI
002000980223     C*****************************************************************
002100980227     C* 31/   - DI COMODO
002200980227     C* 40/42 - ERRORI
002300980223     C*****************************************************************
002400980223     C     *ENTRY        PLIST
002500980223     C                   PARM                    KPJBA
002600980303     C                   MOVEL     KPJBU         PARAM1
002700980303      *
002800980303     C** PRENDO DATA E ORA CORRENTE
002900980303     C                   TIME                    W0140            14 0
003000980303     C                   MOVE      W0140         UDATE8            8 0
003100980303     C                   MOVEL     W0140         UTIME             6 0
003200980303      *
003300980223     C                   Z-ADD     1             CODUT
003400980223     C                   CALL      'X§PARUT'
003500010320     C                   PARM                    UT§DSE0f
003600980303     C                   MOVEL     RAGUT         RSUT             20
003700980223     C                   MOVEL     REC80         CNCR80
003800980304      *-------------------------------------------------------------------------
003900980304     C     KFLT3         KLIST
004000980304     C                   KFLD                    PA1CTL
004100980304     C                   KFLD                    PA1DPL
004200980304      *-------------------------------------------------------------------------
004300980304     C*
004400980304     C     KFLT3         SETLL     FLTR300F
004500980304     C     PA1CTL        READE     FLTR300F                               33
004600980304     C     *IN33         DOWEQ     *OFF
004700980305     C* CONTROLLO CON I LIMITI IMPOSTATI A VIDEO
004800980305     C     TR3NPL        IFLE      PA1APL
004900980305     C*
005000980304     C     SA1NPL        IFEQ      *BLANK
005100980305     C     SA1NPL        ORNE      TR3NPL
005200980305     C* SE NON E' IL PRIMO PIANO DI SMISTAMENTO CHE STAMPO STAMPO LA RIGA
005300980305     C* DI FINE PIANO
005400980305     C     SA1NPL        IFNE      *BLANKS
005500980305     C                   EXCEPT    FIN
005600980305     C                   END
005700980305     C* STAMPO LA TESTA
005800980305     C                   SETON                                            10
005900980304     C                   EXCEPT    TESTA
006000980305     C                   SETON                                            10
006100980305     C                   MOVE      TR3NPL        SA1NPL            2
006200980304     C                   ENDIF
006300980305     C* METTO LA BAIA IN UN CAMPO DI 2 CARATTTERI
006400980305     C                   MOVE      TR3BAI        WRKBAI            2
006500000221     C* SE LE LINEE HANNO UN VALORE DI 998 O 999 O 997 O 996
006600000221     C* NON VADO A RECUPERARE LA DECODIFICA DAL FILE AZORG
006700000221     C                   SELECT
006800000221     C     TR3LNP        WHENEQ    '999'
006900980305     C                   MOVEL     *BLANKS       STPDES
007000980305     C                   MOVEL     'TUTTE'       STPDES
007100000221     C*
007200000221     C     TR3LNP        WHENEQ    '998'
007300980305     C                   MOVEL     *BLANKS       STPDES
007400980305     C                   MOVEL     TE(8)         STPDES
007500000221     C*
007600000221     C     TR3LNP        WHENEQ    '997'
007700000221     C                   MOVEL     *BLANKS       STPDES
007800000221     C                   MOVEL     TE(13)        STPDES
007900000221     C*
008000000221     C     TR3LNP        WHENEQ    '996'
008100000221     C                   MOVEL     *BLANKS       STPDES
008200000221     C                   MOVEL     TE(14)        STPDES
008300000221     C*
008400000221     C                   OTHER
008500980305     C*
008600980305     C                   MOVE      TR3LNP        WLIN              3 0
008700980305     C     WLIN          CHAIN     AZORG01L                           34
008800980304     C  N34              MOVEL     ORGDES        STPDES           20
008900980304     C   34              MOVEL     *BLANKS       STPDES
009000980304     C*
009100000221     C                   ENDSL
009200980305     C*
009300980305     C*
009400000221     C                   SELECT
009500000221     C*
009600000221     C     TR3LNA        WHENEQ    '999'
009700980305     C                   MOVEL     *BLANKS       STADES
009800980305     C                   MOVEL     'TUTTE'       STADES
009900000221     C*
010000000221     C     TR3LNA        WHENEQ    '998'
010100980305     C                   MOVEL     *BLANKS       STADES
010200980305     C                   MOVEL     TE(8)         STADES
010300000221     C*
010400000221     C     TR3LNA        WHENEQ    '997'
010500000221     C                   MOVEL     *BLANKS       STADES
010600000221     C                   MOVEL     TE(13)        STADES
010700000221     C*
010800000221     C     TR3LNA        WHENEQ    '996'
010900000221     C                   MOVEL     *BLANKS       STADES
011000000221     C                   MOVEL     TE(14)        STADES
011100000221     C*
011200000221     C                   OTHER
011300980305     C*
011400980305     C                   MOVE      TR3LNA        WLIN
011500980305     C     WLIN          CHAIN     AZORG01L                           34
011600980304     C  N34              MOVEL     ORGDES        STADES           20
011700980304     C   34              MOVEL     *BLANKS       STADES
011800980305     C*
011900000221     C                   ENDSL
012000980304      *
012100980304     C   OF              EXCEPT    TESTA
012200980304     C   OF              SETOFF                                       OF
012300010320
012400010627     C                   MOVEL     TR3DES        dtr3des
012500980304     C*
012600980304     C                   EXCEPT    DET
012700980304     C     PA1CTL        READE     FLTR300F                               33
012800980305     C                   ELSE
012900980305     C                   SETON                                        33
013000980305     C                   ENDIF
013100980305      *
013200980304     C                   ENDDO
013300980305      *
013400980305     C     SA1NPL        IFNE      *BLANKS
013500980305     C                   EXCEPT    FIN
013600980305     C                   END
013700980305     C                   SETON                                        LR
013800980304     C*
013900980305     OQSYSPRT   E            TESTA            02
014000980303     O                       TE(1)               66
014100980303     O                       TE(2)              132
014200980303     O                       RSUT                20
014300980303     O                                           21 '/'
014400980303     O                       SIMFEL              24
014500980303     O                       UDATE8             119 '  /  /    '
014600980303     O                       PAGE          Z    132
014700980305     O          E            TESTA          0 03
014800980303     O                       KNSIF               10
014900980303     O                       KNMUS               31
015000980305     O                       UTIME              119 '  :  :  '
015100980305     O          E            TESTA       0
015200980305     O                       TE(5)               66
015300980305     O                       TE(6)              132
015400980305     O                       PA1CTL              57
015500980305     O                       PA1DPL              72
015600980305     O                       PA1APL              86
015700980305     O          E       10   TESTA            05
015800980305     O                       TE(7)              108
015900980305     O                       TR3CTL              71
016000980305     O                       TR3NPL              76
016100980305     O          E            TESTA            06
016200980305     O                       TE(11)              66
016300980305     O                       TE(12)             132
016400980305     O          E            TESTA            07
016500980305     O                       TE(3)               66
016600980305     O                       TE(4)              132
016700980305     O          E            TESTA            08
016800980305     O                       TE(11)              66
016900980305     O                       TE(12)             132
017000980305     O          E            DET         1
017100980305     O                       TE(9)               66
017200980305     O                       TE(10)             132
017300980305     O          E            DET         1
017400980305     O                       TE(9)               66
017500980305     O                       TE(10)             132
017600980305     O                       TR3LNP               5
017700980305     O                                              '_'
017800980305     O                       STPDES              26
017900980305     O                       TR3LNA              32
018000980305     O                                              '_'
018100980305     O                       STADES              53
018200980305     O                       TR3ZON              59
018300980305     O                       WRKBAI              66
018400010320     O                       §tr3pista           73
018500010320     O                       §tr3operat          80
018600010320     O                       §tr3pallet          88
018700010320     O                       §tr3descri         119
018800010320     O                       TR3PRB             125
018900980305     O          E            FIN         1
019000980305     O                       TE(11)              66
019100980305     O                       TE(12)             132
019200000719**
019300980305                                     ***  P I A N I    D I    S M    1
019400980420I S T A M E N T O  ***         FNLV69R                   PAG. XXXX   2
019500980305! Linea  Partenza          ! Linea Arrivo             ! Zona ! Bai   3
019600010320a ! Pista ! Opt ! Pallet ! Descrizione                ! Prg !        4
019700980305                                       Tipo Lavorazione X  dal Pia   5
019800980305no  XX  al Piano  XX                                                 6
019900980305****   Piano di smistamento X / XX   ****                            7
020000980305NON LETTI                                                            8
020100980305!                          !                          !      !       9
020200010320  !       !     !        !                            !     !        10
020300980305.__________________________.__________________________.______.____   11
020400010320__._______._____.________.____________________________._____.        12
020500000221NON QUADRATI                                                         13
020600000719NON STAMPATI                                                         14
