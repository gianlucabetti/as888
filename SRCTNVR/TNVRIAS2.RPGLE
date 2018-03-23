000100950202     H DECEDIT('0,') DATEDIT(*DMY.)
000200950202      **************************************************************************
000300040517     FFNBLP01L  UF   E           K DISK
000400040517     FFNARB01L  IF   E           K DISK
000500040517     FFIARBF1C  IF   E           K DISK
000600040517     F                                     IGNORE(FNARBD00)
000700040517     F                                     IGNORE(FNARBK00)
000800040517     F                                     IGNORE(FNARBG00)
000900040517     F                                     IGNORE(FNARBM00)
001000040517     F                                     IGNORE(FNARBV00)
001100040517     F                                     IGNORE(FNARBP00)
001200041213     Fprtf198   O    F  198        PRINTER OFLIND(*INOF)
001300950202     I*
001400950202     C*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
001500000630     C     KBLP          KLIST
001600040517     C                   KFLD                    ARBAAS
001700040517     C                   KFLD                    ARBLNP
001800040517     C                   KFLD                    ARBNRS
001900040517     C                   KFLD                    ARBNSP
002000040517     C*
002100040517     C                   CLEAR                   WCONT
002200040517     C                   CLEAR                   WCONTF
002300000630     C*
002400950202     C*
002500040517     C     *LOVAL        SETLL     FNARB01L
002600040517     C                   READ      FNARB01L                               31
002700950202     C*
002800950512    1C     *IN31         DOWEQ     *OFF
002900040517   1AC     ARBIAS        IFGT      *ZEROS
003000040517     C     ARBQFT        ORGT      *ZEROS
003100040517     C                   CLEAR                   WTASS
003200040517     C     KBLP          CHAIN     FNBLP01L                           31
003300040517    2C     *IN31         IFEQ      *OFF
003400040517    3C     ARBIAS        IFNE      BLPIAS
003500040517     C     ARBVAS        ORNE      BLPVAS
003600040517     C     ARBQFT        ORNE      BLPQFT
003700040517     C* IGNORO SE BOLLA FRANCO
003800040517    4C     ARBCBO        IFEQ      '$ '
003900040517     C     ARBCBO        OREQ      '£ '
004000040517     C     ARBCBO        OREQ      'A '
004100040517     C     ARBCBO        OREQ      'B '
004200040517     C     ARBCBO        OREQ      'H '
004300040517     C     ARBCBO        OREQ      'I '
004400040517     C     ARBCBO        OREQ      'J '
004500040517     C     ARBCBO        OREQ      'K '
004600040517     C     ARBCBO        OREQ      'L '
004700040517     C     ARBCBO        OREQ      'Q '
004800040517     C     ARBCBO        OREQ      'X '
004900040517     C     ARBCBO        OREQ      'Y '
005000040517     C     ARBCBO        OREQ      '1 '
005100040517     C     ARBCBO        OREQ      '4 '
005200040517     C     ARBCBO        OREQ      '5 '
005300040517     C                   ADD       1             WCONTF            5 0
005400041213     C                   EXCEPT    SPUNTA
005500040517     C                   ELSE
005600040517     C* CONTROLLO SE PRESENTE VARIAZIONE TASSAZIONE
005700040517     C     KBLP          SETLL     FIARBF1C                               31
005800040517     C   31              MOVEL     'X'           WTASS             1
005900040517     C                   ADD       1             WCONT             5 0
006000000630     C*
006100000630     C                   EXCEPT    SPUNTA
006200040517     C*
006300041213     c* solo se non trovata variazione aggiorno
006400040517     C                   Z-ADD     ARBIAS        BLPIAS
006500040517     C                   MOVEL     ARBVAS        BLPVAS
006600040517     C                   Z-ADD     ARBQFT        BLPQFT
006700041213     C                   EXCEPT    AGGBLP
006800040517     C                   ENDIF
006900000630    3C                   ENDIF
007000000630     C**
007100950512    2C                   ENDIF
007200040517   1AC                   ENDIF
007300950512     C*
007400040517     C                   READ      FNARB01L                               31
007500000630    1C                   ENDDO
007600000630     C*
007700000630     C                   EXCEPT    CONTA
007800950512     C                   SETON                                        LR
007900950202     C*
008000040517     OFNBLP000  E            AGGBLP
008100040517     O                       BLPIAS
008200040517     O                       BLPVAS
008300040517     O                       BLPQFT
008400041213     Oprtf198   E            SPUNTA      1
008500040517     O                                              'SPED:'
008600040517     O                       ARBAAS            +  1
008700040517     O                       ARBLNP            +  1
008800040517     O                       ARBNRS        Z   +  1
008900040517     O                       ARBNSP        Z   +  1
009000041213     O                       ARBmgs            +  1 '  /  '
009100041213     O                       ARBdcm            +  1 '    /  /  '
009200041213     O                       ARBcbo            +  1
009300040517     O                                         +  2 'VAR.TASSAZ:'
009400040517     O                       WTASS             +  1
009500040517     O                                         +  2 'ARR:'
009600040517     O                       ARBIAS        2   +  1
009700040517     O                       ARBVAS            +  1
009800040517     O                       ARBQFT        2   +  1
009900040517     O                                         +  2 'PAR:'
010000040517     O                       BLPIAS        2   +  1
010100040517     O                       BLPVAS            +  1
010200040517     O                       BLPQFT        2   +  1
010300041213     O          E            CONTA       1
010400000630     O                                              'TOTALE:'
010500040517     O                                         +  2 'BOLLE AGGIOR:'
010600040517     O                       WCONT         Z   +  1
010700040517     O                                         +  5 'BOLLE FRANCO'
010800040517     O                       WCONTF        Z   +  2
