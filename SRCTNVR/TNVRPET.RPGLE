000100010801     FFNDCT01L  IF   E           K DISK
000200010801     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000300981125
000400010801     D WLINEA          S                    LIKE(DCTLNA)
000500010801     D WLINEA1         S                    LIKE(DCTLNA)
000600010706
000700010706     D CNCR80        E DS
000800010706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000900010706     D KPJBA         E DS
001000010801     D DDCT01        E DS
001100010706
001200010706     D TRUL06DS      E DS
001300010706     D  LIN                    1     90  0    DIM(30)
001400010706
001500010706      * CARICO TABELLA £1
001600010706     C                   Z-ADD     1             CODUT
001700010706     C                   CALL      'X§PARUT'
001800010706     C                   PARM                    UT§DSE
001900010706     C                   MOVEL     RAGUT         RSUT             20
002000010706     C                   MOVEL     REC80         CNCR80
002100010706      *
002200010706     C                   CLEAR                   TRUL06DS
002300010706     C                   MOVE      '£1'          D06COD
002400010706     C                   MOVEL     SIMFEL        D06KEY
002500010706     C                   MOVEL(P)  TRUL06DS      KPJBU
002600010706     C                   CALL      'TRUL06R'
002700010706     C                   PARM                    KPJBA
002800010706     C                   MOVEL     KPJBU         TRUL06DS
002900010620
003000010801     C                   EXCEPT    TESTA
003100010801     C                   EVAL      *INOF = *OFF
003200010620
003300010801     C     *LOVAL        SETLL     FNDCT01L
003400010801     C                   READ      FNDCT000
003500981125
003600010801     C                   DOW       NOT %EOF(FNDCT01L)
003700010620
003800010801     C* TESTO SE LA LINEA DI PARTENZA È IN £1
003900010801     C                   MOVEL     DCTFLO        DDCT01
004000010801     C                   MOVE      §DCTLNPC      WLINEA1
004100010801     C     WLINEA1       LOOKUP    LIN                                    99
004200010801     C                   IF        *IN99 = *OFF
004300010801     C                   Z-ADD     WLINEA1       WLINEA
004400010801     C                   GOTO      SOTTO
004500010801     C                   ENDIF
004600010801
004700010801     C* TESTO SE LA LINEA DI ARRIVO È IN £1
004800010801     C     DCTLNA        LOOKUP    LIN                                    99
004900010801     C                   IF        *IN99 = *OFF
005000010801     C                   Z-ADD     DCTLNA        WLINEA                         PARTENZA/ARRIVO
005100010801     C                   GOTO      SOTTO
005200010801     C                   ENDIF
005300010801
005400010801     C* SE ENTRAMBE SONO IN £1 NON DEVO FARE I CONTROLLI
005500010801     C                   GOTO      CONTINUA
005600010801
005700010801     C     SOTTO         TAG
005800010801     C                   IF        *INOF = *ON
005900010801     C                   EXCEPT    TESTA
006000010801     C                   EVAL      *INOF = *OFF
006100010801     C                   ENDIF
006200010801
006300010801     C                   IF        WLINEA <> DCTFL1
006400010801     C                   EXCEPT    DETTA
006500010801     C                   ENDIF
006600010801
006700010801     C     CONTINUA      TAG
006800010801     C                   READ      FNDCT000
006900010706
007000010801     C                   ENDDO
007100010620
007200010801     C                   EVAL      *INLR = *ON
007300010706      *-------------------------------------------------
007400010801     OQSYSPRT   E            TESTA             1
007500010801     O                                           66 'CAMBIO P.O. DI TRASMISS'
007600010801     O                                           76 'IONE FNDCT'
007700010801     O                                           98 '*TNVRDCT*'
007800010801     O                       UDATE         Y    115
007900010801     O                                          128 'Pag.'
008000010801     O                       PAGE          Z    132
008100010801     O*
008200010801     O          E            TESTA
008300010801     O                                           66 'CAMBIO P.O. DI TRASMISS'
008400010801     O                                           76 'IONE FNDCT'
008500010801     O*
008600010801     O          E            TESTA       2
008700010801     O                                           25 '-------------------------'
008800010801     O                                           48 '-----------------------'
008900010801     O                                           73 'PUNTI OPERATIVI GESTIBILI'
009000010801     O                                           98 '-------------------------'
009100010801     O                                          119 '---------------------'
009200010801     O*
009300010801     O          E            TESTA       1
009400010801     O                       LIN(1)        Z      3
009500010801     O                       LIN(2)        Z      7
009600010801     O                       LIN(3)        Z     11
009700010801     O                       LIN(4)        Z     15
009800010801     O                       LIN(5)        Z     19
009900010801     O                       LIN(6)        Z     23
010000010801     O                       LIN(7)        Z     27
010100010801     O                       LIN(8)        Z     31
010200010801     O                       LIN(9)        Z     35
010300010801     O                       LIN(10)       Z     39
010400010801     O                       LIN(11)       Z     43
010500010801     O                       LIN(12)       Z     47
010600010801     O                       LIN(13)       Z     51
010700010801     O                       LIN(14)       Z     55
010800010801     O                       LIN(15)       Z     59
010900010801     O                       LIN(16)       Z     63
011000010801     O                       LIN(17)       Z     67
011100010801     O                       LIN(18)       Z     71
011200010801     O                       LIN(19)       Z     75
011300010801     O                       LIN(20)       Z     79
011400010801     O                       LIN(21)       Z     83
011500010801     O                       LIN(22)       Z     87
011600010801     O                       LIN(23)       Z     91
011700010801     O                       LIN(24)       Z     95
011800010801     O                       LIN(25)       Z     99
011900010801     O                       LIN(26)       Z    103
012000010801     O                       LIN(27)       Z    107
012100010801     O                       LIN(28)       Z    111
012200010801     O                       LIN(29)       Z    115
012300010801     O                       LIN(30)       Z    119
012400010801     O*
012500010801     O          E            TESTA       2
012600010801     O                                            9 'ANNO C.A.'
012700010801     O                                           30 'FILIALE APERTURA'
012800010801     O                                           41 'NUMERO'
012900010801     O                                           58 'LINEA ARRIVO'
013000010801     O                                           77 'LINEA PARTENZA'
013100010801     O                                           88 'DCTFL1'
013200010801     O                                          103 'NUOVO P.O.'
013300010801     O*
013400010801     O          E            DETTA       1
013500010801     O                       DCTAAC               6
013600010803     O                       DCTFIL              23
013700010801     O                       DCTNCA        Z     41
013800010801     O                       DCTLNA              54
013900010801     O                       §DCTLNPC            72
014000010801     O                       DCTFL1              86
014100010801     O                       WLINEA              99
