000100121003      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200121003     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300121003
000400121003      *---------------------------------------------------------------*
000500121003
000600121003     fTitas40c  if   e           k disk
000700121003     FCNACO00F  IF   E           K DISK
000800121003     FWFSER00F  O    E             DISK
000900121003     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000121003      *
001100121003      // - Flags booleani
001200121003     d $End            s               n   inz(*off)
001300121003      *---------------------------------------------------------------*
001400121003     d TIiftds       e ds                  inz  extname(TIift00F)
001500121003
001600121003      *---------------------------------------------------------------*
001700121003      *
001800121003     c/exec sql
001900121003     c+     set  option  DYNUSRPRF = *OWNER,
002000121003     c+                  CLOSQLCSR = *ENDMOD
002100121003     c/end-exec
002200121003     c/exec sql
002300121003     c+     declare c1 cursor for
002400121003     c+     select * from tiift00f  where
002500121003     c+     IFTDFT BETWEEN 20120904 AND 20121001
002600121003     c+     for fetch only
002700121003     c/end-exec
002800121003      *
002900121003     c/exec sql
003000121003     c+ open c1
003100121003     c/end-exec
003200121003      /free
003300121003         IF sqlcode < 0;
003400121003           $End = *on;
003500121003         ENDIF;
003600121003
003700121003         DOW not $End;
003800121003           exec sql
003900121003             fetch next from C1  into :tiiftds ;
004000121003
004100121003           IF sqlcod = 100 or sqlcod < 0;
004200121003             $End = *on;
004300121003             leave;
004400121003           ENDIF;
004500121003
004600121003
004700121003           If   iftnft <> 999999 ;
004800121003           exsr elaFatt     ;
004900121003           endif ;
005000121003         ENDDO;
005100121003
005200121003
005300121003         exec sql
005400121003           close c1 ;
005500121003      /end-free
005600121003     c                   SETON                                        LR
005700121003     C     ELAFATT       BEGSR
005800121003     C                   CLEAR                   TOTSPED           7 0
005900121003     C                   CLEAR                   TOTIMV           13 3
006000121003     C                   CLEAR                   TOTCSER           7 0
006100121003     C                   CLEAR                   TOTIMVSER        13 3
006200121003     c*
006300121003     c     ktas          klist
006400121003     c                   kfld                    TASFIV
006500121003     c                   kfld                    IFTNFT
006600121003     c                   kfld                    IFTDFT
006700121003     c*
006800121003     c     kACO          klist
006900121003     c                   kfld                    KUT               1 0
007000121003     c                   kfld                    KCC               4 0
007100121003     c                   kfld                    IFTCFA
007200121003     C                   EVAL      KUT = 1
007300121003     C                   EVAL      KCC = 0151
007400121003     C*
007500121003     C                   IF        IFTDFT <> 20120930
007600121003     C                   EVAL      TASFIV = 998
007700121003     C                   ELSE
007800121003     C                   EVAL      TASFIV = 996
007900121003     C                   ENDIF
008000121003
008100121003     C     KTAS          SETLL     TITAS40C
008200121003     C                   DO        *HIVAL
008300121003     C     KTAS          READE     TITAS40C
008400121003      *
008500121003     C                   IF        %EOF(TITAS40C)
008600121003     C                   LEAVE
008700121003     C                   ENDIF
008800121003      *
008900121003     C                   ADD       1             TOTSPED
009000121003     C                   ADD       TASIMV        TOTIMV
009100121003     C                   IF        TASTBL = 'F7' OR TASTBL = 'FS' OR
009200121003     C                             TASTBL = 'FC' OR TASTBL = 'AS' OR
009300121003     C                             TASTBL = 'AP'
009400121003     C                   ADD       1             TOTCSER
009500121003     C                   ADD       TASIMV        TOTIMVSER
009600121003     C                   ENDIF
009700121003
009800121003     C                   ENDDO
009900121003
010000121003      * STAMPO
010100121003     C* STAMPO TESTATA
010200121003     C                   IF        TOTCSER > 0
010300121003     C                   CLEAR                   WFSER000
010400121003     C                   EVAL      SERFIV = TASFIV
010500121003     C                   EVAL      SERNFT = TASNFT
010600121003     C                   EVAL      SERDFT = TASDFT
010700121003     C                   EVAL      SERCFA = IFTCFA
010800121003     C                   EVAL      SERCBO = IFTCBO
010900121003     C                   EVAL      SERNSP = TOTSPED
011000121003     C                   EVAL      SERIMV = TOTIMV
011100121003     C                   EVAL      SERNSPCS = TOTCSER
011200121003     C                   EVAL      SERIMVCS = TOTIMVSER
011300121003
011400121003     C     KACO          CHAIN     CNACO00F
011500121003     C                   IF        %FOUND(CNACO00F)
011600121003     C                   EVAL      SERRAG = ACORAG
011700121003     C                   ENDIF
011800121003
011900121003     C                   WRITE     WFSER000
012000121003
012100121003     C     *INOF         IFEQ      *ON
012200121003     C     *IN07         OREQ      *OFF
012300121003     C                   SETON                                        07
012400121003     C                   EXCEPT    TESTA
012500121003     C                   SETOFF                                       OF
012600121003     C                   ENDIF
012700121003
012800121003     c                   except    DETT
012900121003     C                   ENDIF
013000121003
013100121003     C                   ENDSR
013200121003      *
013300121003
013400121003     OQSYSPRT   E            TESTA            02
013500121003     O                                           60 ' FATTURE SETTEMBRE 2012'
013600121003     O                       PAGE          Z    132
013700121003     O          E            TESTA            03
013800121003     O                                           13 'COD.BOLLET.'
013900121003     O                                          + 1 'INT.FATTUR.'
014000121003     O                                          + 1 'LIBRO IVA'
014100121003     O                                          + 1 'NUM.FATT.'
014200121003     O                                          + 1 'DTA.FATT.'
014300121003     O                                          + 1 'TOT.SPED.FATT.'
014400121003     O                                          + 1 'TOT.IMPON.FAT.'
014500121003     O                                          + 1 'TOT.SPED.C/S'
014600121003     O                                          + 1 'TOT.IMPON.C/S'
014700121003     O*--------------------------------------------------------------*
014800121003     O*     DETTAGLIO                                                *
014900121003     O*--------------------------------------------------------------*
015000121003     O          E            DETT        1
015100121003     O                       IFTCBO        Z     12
015200121003     O                       IFTCFA        Z    + 3
015300121003     O                       TASFIV             + 6
015400121003     O                       TASNFT             + 2
015500121003     O                       TASDFT             + 3 '    /  /  '
015600121003     O                       TOTSPED       Z    + 3
015700121003     O                       TOTIMV        2    + 3
015800121003     O                       TOTCSER       Z    + 3
015900121003     O                       TOTIMVSER     2    + 3
