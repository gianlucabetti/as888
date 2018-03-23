000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030804
000300030805     FVARIA_ASS IP   E             DISK     prefix(VAR:3)
000400030805     FTITAS30C  UF   E           K DISK
000500030805     FTITA730C  UF A E           K DISK
000600030805
000700961108     C*
000800030805     C                   EVAL      *IN99 = *On
000900030805     C*
001000980702     C     KTAS          KLIST
001100030805     C                   KFLD                    VARAAS
001200030805     C                   KFLD                    VARLNP
001300030805     C                   KFLD                    VARNRS
001400030805     C                   KFLD                    VARNSP
001500030805     C                   KFLD                    VARTBL
001600030804
001700030805     C     KTAS          CHAIN     TITAS30C
001800030805    0C                   IF        %FOUND(TITAS30C)
001900030805
002000030805    1C                   IF        TASFIV > 900   and
002100030805     C                             (TASdFT = 20030731 or tasdFT = 20030801)
002200030804
002300030805     C                   CLEAR                   WSV               1
002400030805     C                   CLEAR                   WVA              11 3
002500030805     C                   SELECT
002600030805     c                   WHEN      VARSV1 = '&'
002700030805     c                   movel     VARSV1        Wsv
002800030805     c                   movel     VARVA1        WVA
002900030805     c                   WHEN      VARSV2 = '&'
003000030805     c                   movel     VARSV2        Wsv
003100030805     c                   movel     VARVA2        WVA
003200030805     c                   WHEN      VARSV3 = '&'
003300030805     c                   movel     VARSV3        Wsv
003400030805     c                   movel     VARVA3        WVA
003500030805     c                   ENDSL
003600030805
003700030805     C                   SELECT
003800030805     C                   WHEN      TASSV1 = *BLANKS
003900030805     c                   Z-ADD     WVA           TASVA1
004000030805     c                   MOVEL     WSV           TASSV1
004100030805     C                   WHEN      TASSV2 = *BLANKS
004200030805     c                   Z-ADD     WVA           TASVA2
004300030805     c                   MOVEL     WSV           TASSV2
004400030805     C                   WHEN      TASSV3 = *BLANKS
004500030805     c                   Z-ADD     WVA           TASVA3
004600030805     c                   MOVEL     WSV           TASSV3
004700030805     C                   OTHER
004800030805     c                   Z-ADD     WVA           TA7VAN
004900030805     c                   MOVEL     WSV           TA7SVN
005000030805     c                   Z-ADD     TASAAS        TA7AAS
005100030805     c                   Z-ADD     TASLNP        TA7LNP
005200030805     c                   Z-ADD     TASNRS        TA7NRS
005300030805     c                   Z-ADD     TASNSP        TA7NSP
005400030805     c                   MOVEL     TASTBL        TA7TBL
005500030805     C   99              WRITE     TITA7000
005600030805     C                   ENDSL
005700030805
005800030805     C                   Z-ADD     VARIMV        TASIMV
005900030805     C                   eval      TASdiv = 'EUR'
006000030805     C   99              UPDATE    TITAS000
006100030805    1C                   ENDIF
006200030805    0C                   ENDIF
