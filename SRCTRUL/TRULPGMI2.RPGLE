000100021008      /TITLE List ILE Program Information (QBNLPGMI) API
000200021008
000300021009     FTrulPgmIF O    E             DISK
000400021009
000500021009     D UserSpaceName   C                   'QBNLPGMI  QTEMP'
000600021008
000700021009     D PrmPgm          S             10                                         Programma
000800021009     D PrmLib          S             10                                         Libreria programma
000900021009     D PrmSysName      S              8                                         Sistema
001000021008
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
004600021009     C                   PARM                    PrmSysName
004700021008
004800021008     ** Create User Space (QUSCRTUS) API
004900021008     C                   CLEAR                   QUSEC
005000021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
005100021008
005200021008     C                   CALL      'QUSCRTUS'
005300021008     C                   PARM                    CrtUS01
005400021008     C                   PARM                    CrtUS02
005500021008     C                   PARM                    CrtUS03
005600021008     C                   PARM                    CrtUS04
005700021008     C                   PARM                    CrtUS05
005800021008     C                   PARM                    CrtUS06
005900021008     C                   PARM                    CrtUS07
006000021008     C                   PARM                    QUSEC
006100021008
006200021008     ** List ILE Program Information (QBNLPGMI) API
006300021008     C                   EVAL      LPgmI03 = PrmPgm + PrmLib
006400021008     C                   CLEAR                   QUSEC
006500021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
006600021008
006700021008     C                   CALL      'QBNLPGMI'
006800021008     C                   PARM                    LPgmI01
006900021008     C                   PARM                    LPgmI02
007000021008     C                   PARM                    LPgmI03
007100021008     C                   PARM                    QUSEC
007200021008
007300021009     ** Retrieve User Space (QUSRTVUS) API
007400021009     ** Generic Header
007500021009     C                   EVAL      RtvUS03 = %LEN(QUSH0100)
007600021008     C                   CLEAR                   QUSEC
007700021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
007800021008
007900021008     C                   CALL      'QUSRTVUS'
008000021008     C                   PARM                    RtvUS01
008100021008     C                   PARM      1             RtvUS02
008200021009     C                   PARM                    RtvUS03
008300021008     C                   PARM                    QUSH0100
008400021008     C                   PARM                    QUSEC
008500021008
008600021009     ** Retrieve User Space (QUSRTVUS) API
008700021009     ** Informazioni programma
008800021008     C                   EVAL      RtvUS02 = QUSOLD + 1
008900021008     C                   EVAL      RtvUS03 = QUSSEE
009000021008     C                   EVAL      Count = 1
009100021008
009200021008     C                   DOW       Count <= QUSNBRLE
009300021008
009400021008     C                   CLEAR                   QUSEC
009500021008     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
009600021008
009700021008     C                   CALL      'QUSRTVUS'
009800021008     C                   PARM                    RtvUS01
009900021008     C                   PARM                    RtvUS02
010000021008     C                   PARM                    RtvUS03
010100021008     C                   PARM                    QBNL0100
010200021008     C                   PARM                    QUSEC
010300021008
010400021008     ** A questo punto nel campo
010500021008     ** QBNMCT  c'è il timestamp di creazione del modulo
010600021008     ** QBNSFUT c'è il timestamp di aggiornamento del sorgente
010700021009     C                   EVAL      QBNSysName = PrmSysName
010800021009     C                   WRITE     PgmL0100
010900021008
011000021008     C                   EVAL      RtvUS02 = RtvUS02 + QUSSEE
011100021008     C                   EVAL      Count = Count + 1
011200021008
011300021008     C                   ENDDO
011400021008
011500021009     C                   RETURN
