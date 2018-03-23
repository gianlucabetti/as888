000100010802     FFNDCT01L  UF   E           K DISK
000400981125
000500010801     D WLINEA          S                    LIKE(DCTLNA)
000501010801     D WLINEA1         S                    LIKE(DCTLNA)
000600010706
000700010706     D CNCR80        E DS
000800010706     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001000010706     D KPJBA         E DS
001001010801     D DDCT01        E DS
001100010706
001200010706     D TRUL06DS      E DS
001300010706     D  LIN                    1     90  0    DIM(30)
001400010706
001600010706      * CARICO TABELLA £1
001700010706     C                   Z-ADD     1             CODUT
001800010706     C                   CALL      'X§PARUT'
001900010706     C                   PARM                    UT§DSE
002000010706     C                   MOVEL     RAGUT         RSUT             20
002100010706     C                   MOVEL     REC80         CNCR80
002200010706      *
002300010706     C                   CLEAR                   TRUL06DS
002400010706     C                   MOVE      '£1'          D06COD
002500010706     C                   MOVEL     SIMFEL        D06KEY
002600010706     C                   MOVEL(P)  TRUL06DS      KPJBU
002700010706     C                   CALL      'TRUL06R'
002800010706     C                   PARM                    KPJBA
002900010706     C                   MOVEL     KPJBU         TRUL06DS
003000010620
003400010801     C     *LOVAL        SETLL     FNDCT01L
003500010801     C                   READ      FNDCT000
003600981125
003700010801     C                   DOW       NOT %EOF(FNDCT01L)
003800010620
003801010801     C* TESTO SE LA LINEA DI PARTENZA È IN £1
003802010801     C                   MOVEL     DCTFLO        DDCT01
003803010801     C                   MOVE      §DCTLNPC      WLINEA1
003900010801     C     WLINEA1       LOOKUP    LIN                                    99
004000010801     C                   IF        *IN99 = *OFF
004001010801     C                   Z-ADD     WLINEA1       WLINEA
004300010801     C                   GOTO      SOTTO
004301010801     C                   ENDIF
004302010801
004303010801     C* TESTO SE LA LINEA DI ARRIVO È IN £1
004304010801     C     DCTLNA        LOOKUP    LIN                                    99
004305010801     C                   IF        *IN99 = *OFF
004306010801     C                   Z-ADD     DCTLNA        WLINEA                         PARTENZA/ARRIVO
004307010801     C                   GOTO      SOTTO
004308010801     C                   ENDIF
004309010801
004310010801     C* SE ENTRAMBE SONO IN £1 NON DEVO FARE I CONTROLLI
004311010801     C                   GOTO      CONTINUA
004312010801
004313010801     C     SOTTO         TAG
004502010801
004503010801     C                   IF        WLINEA <> DCTFL1
004600010802     C                   MOVE      *BLANKS       DCTFL1
004601010802     C                   MOVE      WLINEA        DCTFL1
004602010802     C                   UPDATE    FNDCT000
004603010801     C                   ENDIF
004604010801
004605010801     C     CONTINUA      TAG
004606010801     C                   READ      FNDCT000
005300010706
005500010801     C                   ENDDO
005600010620
005700010801     C                   EVAL      *INLR = *ON
