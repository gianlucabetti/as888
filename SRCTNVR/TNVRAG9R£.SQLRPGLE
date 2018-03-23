000100121003      *PARMS OPTION(*NOXREF) TGTRLS(*CURRENT)
000200121003     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000300121003
000400121003      *---------------------------------------------------------------*
000500121003
000600121003     fTitas40c  if   e           k disk
000700121003     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800121003      *
000900121003      // - Flags booleani
001000121003     d $End            s               n   inz(*off)
001100121003      *---------------------------------------------------------------*
001200121003     d TIiftds       e ds                  inz  extname(TIift00F)
001300121003     d                                          prefix(S)
001400121003
001500121003      *---------------------------------------------------------------*
001600121003      *
001700121003     c/exec sql
001800121003     c+     set  option  DYNUSRPRF = *OWNER,
001900121003     c+                  CLOSQLCSR = *ENDMOD
002000121003     c/end-exec
002100121003     c/exec sql
002200121003     c+     declare c1 cursor for
002300121003     c+     select * from tiift00f  where
002400121003     c+     IFTDFT BETWEEN 20120904 AND 20121001
002500121003     c+     for fetch only
002600121003     c/end-exec
002700121003      *
002800121003     c/exec sql
002900121003     c+ open c1
003000121003     c/end-exec
003100121003      /free
003200121003         IF sqlcode < 0;
003300121003           $End = *on;
003400121003         ENDIF;
003500121003
003600121003         DOW not $End;
003700121003           exec sql
003800121003             fetch next from C1  into :tiiftds ;
003900121003
004000121003           IF sqlcod = 100 or sqlcod < 0;
004100121003             $End = *on;
004200121003             leave;
004300121003           ENDIF;
004400121003
004500121003
004600121003           exsr elaFatt     ;
004700121003         ENDDO;
004800121003
004900121003
005000121003         exec sql
005100121003           close c1 ;
005200121003      /end-free
005300121003     c                   SETON                                        LR
005400121003     C     ELAFATT       BEGSR
005500121003     C                   CLEAR                   TOTSPED           7 0
005600121003     C                   CLEAR                   TOTIMV           13 3
005700121003     C                   CLEAR                   TOTCSER           7 0
005800121003     C                   CLEAR                   TOTIMVSER        13 3
005900121003     c*
006000121003     c     ktas          klist
006100121003     c                   kfld                    TASFIV
006200121003     c                   kfld                    IFTNFT
006300121003     c                   kfld                    IFTDFT
006400121003     c                   kfld                    IFTCBO
006500121003     C*
006600121003     C                   IF        IFTDFT <> 20120930
006700121003     C                   EVAL      TASFIV = 998
006800121003     C                   ELSE
006900121003     C                   EVAL      TASFIV = 996
007000121003     C                   ENDIF
007100121003     C     KTAS          SETLL     TITAS40C
007200121003     C                   DO        *HIVAL
007300121003     C     KTAS          READE     TITAS40C
007400121003      *
007500121003     C                   IF        %EOF(TITAS40C)
007600121003     C                   LEAVE
007700121003     C                   ENDIF
007800121003      *
007900121003     C                   ADD       1             TOTSPED
008000121003     C                   ADD       TASIMV        TOTIMV
008100121003     C                   IF        TASTBL = 'F7' OR TASTBL = 'FS' OR
008200121003     C                             TASTBL = 'FC' OR TASTBL = 'AS' OR
008300121003     C                             TASTBL = 'AP'
008400121003     C                   ADD       1             TOTCSER
008500121003     C                   ADD       TASIMV        TOTIMVSER
008600121003     C                   ENDIF
008700121003
008800121003     C                   ENDDO
008900121003
009000121003      * STAMPO
009100121003     C* STAMPO TESTATA
009200121003     C     *INOF         IFEQ      *ON
009300121003     C                   SETON                                        07
009400121003     C                   EXCEPT    TESTA
009500121003     C                   SETOFF                                       OF07
009600121003     C                   ENDIF
009700121003
009800121003     c                   except    fAtt
009900121003      *
010000121003
010100121003     OQSYSPRT   E            TESTA            02
010200121003     O                                           60 ' FATTURE SETTEMBRE 2012'
010300121003     O                       PAGE          Z    132
010400121003     O          E            TESTA            03
010500121003     O                                           13 'COD.BOLLET.'
010600121003     O                                          + 1 'INT.FATTUR.'
010700121003     O                                          + 1 'LIBRO IVA'
010800121003     O                                          + 1 'NUM.FATT.'
010900121003     O                                          + 1 'DTA.FATT.'
011000121003     O                                          + 1 'TOT.SPED.FATT.'
011100121003     O                                          + 1 'TOT.IMPON.FAT.'
011200121003     O                                          + 1 'TOT.SPED.C/S'
011300121003     O                                          + 1 'TOT.IMPON.C/S'
011400121003     O*--------------------------------------------------------------*
011500121003     O*     DETTAGLIO                                                *
011600121003     O*--------------------------------------------------------------*
011700121003     O          E            DETT        1
011800121003     O                       IFTCBO               7
011900121003     O                       IFTCFA             + 3
012000121003     O                       TASFIV             + 3
012100121003     O                       TASNFT             + 3
012200121003     O                       TASDFT             + 3
012300121003     O                       TOTSPED            + 3
012400121003     O                       TOTIMV             + 3
012500121003     O                       TOTCSER            + 3
012600121003     O                       TOTIMVSER          + 3
