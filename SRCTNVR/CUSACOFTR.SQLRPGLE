000100031112     D tibs34ds      e ds
000200031112     D tibs56ds      e ds
000300031112     D ddatiute      e ds
000400031112     D azuteds       e ds                  extname(AZUTE00F)
000500031112     D POG             S              3    DIM(250)  inz                        FIL GESTITE
000600031112     D i               S              5I 0
000700031112     D WrkSQLStr       S            256
000800031112
000900031112     C     *dtaara       define    §azute        azuteds
001000031112     C     *dtaara       define    §datiute      ddatiute
001100031112
001200031112     C                   in(E)     *dtaara
001300031112     C                   IF        %Error  or  RSUT = *blanks
001400031112     C                   clear                   tibs34ds
001500031112     C                   call      'TIBS34R'
001600031112     C                   parm                    Tibs34Ds
001700031112     C                   in        *dtaara
001800031112     C                   ENDIF
001900031112
002000031112     C                   EXSR      CARPO
002100031112     C                   EXSR      Trasmetti
002200031112
002300031112     C                   EVAL      *INLR = *ON
002400031112
002500031112     C**************************************************************************
002600031112     C*    CARICO P.O. presenti sull'as di sede
002700031112     C**************************************************************************
002800031112     C     CARPO         BEGSR
002900031112      *
003000031112     C                   clear                   tibs56ds
003100031112     C                   eval      I56ppo = simfel
003200031112     C                   eval      I56mod = 'POA'
003300031112     C                   call      'TIBS56R'
003400031112     C                   parm                    tibs56ds
003500031112     C                   moveA     o56ski        POG
003600031112      *
003700031112     C                   ENDSR
003800031112
003900031112     ***********************************************************************
004000031112     **
004100031112     **
004200031112     **
004300031112     ***********************************************************************
004400031112     C     Trasmetti     BEGSR
004500031112
004600031112     C                   EVAL      WrkSQLStr = 'UPDATE CNACO00F'
004700031112     C                             + ' SET ACOFTR = '' '', ACODTR = 0'
004800031112     C                             + ' WHERE ACOKUT = 1 AND ACOKCC = 151-
004900031112     C                              AND ACOFLT IN ('
005000031112
005100031112     C                   DO        250           i
005200031112     C                   IF        POG(i) = *BLANK OR POG(i) = *ZERO
005300031112     C                   LEAVE
005400031112     C                   ENDIF
005500031112     C                   IF        i > 1
005600031112     C                   EVAL      WrkSQLStr = %TRIMR(WrkSQLStr) + ' ,'
005700031112     C                   ENDIF
005800031112     C                   EVAL      WrkSQLStr = %TRIMR(WrkSQLStr) + ' ' + POG(i)
005900031112     C                   ENDDO
006000031112
006100031112     C                   EVAL      WrkSQLStr = %TRIMR(WrkSQLStr) + ')'
006200031112
006300031112     C/EXEC SQL
006400031112     C+ EXECUTE IMMEDIATE :WrkSQLStr
006500031112     C/END-EXEC
006600031112
006700031112     C                   ENDSR
