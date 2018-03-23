000100021008      /TITLE List ILE Program Information (QBNLPGMI) API
000200021009
000300021009     D UserSpaceName   C                   'QBNLPGMI  QTEMP'
000400021008
000500021009     D PrmPgm          S             10                                         Programma
000600021009     D PrmLib          S             10                                         Libreria programma
000700160111     D pOut_SrcFileN   S             10                                         Source File
000800160111     D pOut_SrcLibN    S             10                                         Source File Library
000900160111     D pOut_SrcMbrN    S             10                                         Source File Member
001000160111
001100021008      /COPY QSYSINC/QRPGLESRC,QUSGEN
001200021008      /COPY QSYSINC/QRPGLESRC,QBNLPGMI
001300021008      /COPY QSYSINC/QRPGLESRC,QUSEC
001400021008
001500021008     D CrtUS01         S             20                                         Qualified user space
001600021008     D                                     INZ(UserSpaceName)
001700021008     D CrtUS02         S             10                                         Extended attribute
001800021008     D CrtUS03         S              9B 0                                      Initial size
001900021008     D                                     INZ(4096)
002000021008     D CrtUS04         S              1                                         Initial value
002100021008     D                                     INZ(X'00')
002200021008     D CrtUS05         S             10                                         Public authority
002300021008     D                                     INZ('*EXCLUDE')
002400021008     D CrtUS06         S             50                                         Text description
002500021009     D                                     INZ('List ILE Program Information')
002600021008     D CrtUS07         S             10                                         Replace
002700021008     D                                     INZ('*YES')
002800021008
002900021008     D RtvUS01         S             20                                         Qualified user space
003000021008     D                                     INZ(UserSpaceName)
003100021008     D RtvUS02         S              9B 0                                      Starting position
003200021008     D RtvUS03         S              9B 0                                      Length of data
003300021008
003400021008     D LPgmI01         S             20                                         Qualified user space
003500021008     D                                     INZ(UserSpaceName)
003600021008     D LPgmI02         S              8                                         Format name
003700021008     D                                     INZ('PGML0100')
003800021008     D LPgmI03         S             20                                         Qualified ILE progra
003900021008
004000021009     D Count           S
004100021009     D                                     LIKE(QUSNBRLE)
004200021008
004300021008     C     *ENTRY        PLIST
004400021008     C                   PARM                    PrmPgm
004500021008     C                   PARM                    PrmLib
004600160111     C                   PARM                    pOut_SrcFileN
004700160111     C                   PARM                    pOut_SrcLibN
004800160111     C                   PARM                    pOut_SrcMbrN
004900021008
005000021008     ** Create User Space (QUSCRTUS) API
005100021008     C                   CLEAR                   QUSEC
005200021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
005300021008
005400021008     C                   CALL      'QUSCRTUS'
005500021008     C                   PARM                    CrtUS01
005600021008     C                   PARM                    CrtUS02
005700021008     C                   PARM                    CrtUS03
005800021008     C                   PARM                    CrtUS04
005900021008     C                   PARM                    CrtUS05
006000021008     C                   PARM                    CrtUS06
006100021008     C                   PARM                    CrtUS07
006200021008     C                   PARM                    QUSEC
006300021008
006400021008     ** List ILE Program Information (QBNLPGMI) API
006500021008     C                   EVAL      LPgmI03 = PrmPgm + PrmLib
006600021008     C                   CLEAR                   QUSEC
006700021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
006800021008
006900021008     C                   CALL      'QBNLPGMI'
007000021008     C                   PARM                    LPgmI01
007100021008     C                   PARM                    LPgmI02
007200021008     C                   PARM                    LPgmI03
007300021008     C                   PARM                    QUSEC
007400021008
007500021009     ** Retrieve User Space (QUSRTVUS) API
007600021009     ** Generic Header
007700021009     C                   EVAL      RtvUS03 = %LEN(QUSH0100)
007800021008     C                   CLEAR                   QUSEC
007900021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
008000021008
008100021008     C                   CALL      'QUSRTVUS'
008200021008     C                   PARM                    RtvUS01
008300021008     C                   PARM      1             RtvUS02
008400021009     C                   PARM                    RtvUS03
008500021008     C                   PARM                    QUSH0100
008600021008     C                   PARM                    QUSEC
008700021008
008800021009     ** Retrieve User Space (QUSRTVUS) API
008900021009     ** Informazioni programma
009000021008     C                   EVAL      RtvUS02 = QUSOLD + 1
009100021008     C                   EVAL      RtvUS03 = QUSSEE
009200021008     C                   EVAL      Count = 1
009300021008
009400160111     ** prendiamo come assunto che in BRT un pgm sia composto da 1 modulo e basta
009500160111     C***                DOW       Count <= QUSNBRLE
009600160111     C                   DOW       Count  = 1
009700021008
009800021008     C                   CLEAR                   QUSEC
009900021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
010000021008
010100021008     C                   CALL      'QUSRTVUS'
010200021008     C                   PARM                    RtvUS01
010300021008     C                   PARM                    RtvUS02
010400021008     C                   PARM                    RtvUS03
010500021008     C                   PARM                    QBNL0100
010600021008     C                   PARM                    QUSEC
010700021008
010800160111     ** A questo punto nei campi c'è:
010900160111     ** QBNSFILN : Source File Name
011000160111     ** QBNSFLN  : Source File Library Name
011100160111     ** QBNSFILM : Source File Member
011200160111     C                   EVAL      pOut_SrcFileN = QBNSFILN
011300160111     C                   EVAL      pOut_SrcLibN  = QBNSFLN
011400160111     C                   EVAL      pOut_SrcMbrN  = QBNSFILM
011500160111
011600160111     C***                EVAL      RtvUS02 = RtvUS02 + QUSSEE
011700160111     C                   EVAL      Count = Count + 1
011800021008
011900021008     C                   ENDDO
012000021008
012100021009     C                   RETURN
