000100060207     dfifttds        e ds                  extname(fiftt00f)
000200060207     D wrkGetLista     S           4096
000300060207     D                                     VARYING
000400060207     c                   move      20060206      t05dti            8 0
000500060207     c                   move      20060207      t05dtf            8 0
000600051219     **?Imposto la parte fissa dell'istruzione.
000700051219     C                   EVAL      wrkGetLista =
000800060207     C                             'SELECT *  -
000900060207     C                             FROM Fiftt00f where -
001000051219     C                             ftttsr <>'' '' AND fttdcv BETWEEN ' +
001100051219     C                             %EDITC(t05dti:'X') + ' AND ' +
001200051219     C                             %EDITC(t05dtf:'X')
001300051219     **?Solo lettura.
001400051219     C                   EVAL      wrkGetLista = wrkGetLista + ' FOR READ ONLY'
001500051219     C
001600051219     C/EXEC SQL
001700051219     C+ PREPARE s1 FROM :wrkGetLista
001800051219     C/END-EXEC
001900051219     C
002000051219     C/EXEC SQL
002100051219     C+ DECLARE A1 CURSOR FOR S1
002200051219     C/END-EXEC
002300051219     C
002400051219     C/EXEC SQL
002500051219     C+ OPEN a1
002600051219     C/END-EXEC
002700051214
002800051129     C                   DO        *hival
002900051129     C/EXEC SQL
003000060207     C+ FETCH NEXT FROM a1 INTO :fifttds
003100051129     C/END-EXEC
003200051129     C                   SELECT
003300051129     c*
003400051129     C                   WHEN      SqlCod = 100
003500051129     c                   leave
003600051129     C                   WHEN      SqlCod < 0
003700051129     C                   seton                                        H1
003800051129     c                   exsr      srfine
003900051129     c                   other
004000060207     c                   Call      'TNVR021RAB'
004100060207     c                   Parm                    fttpdr
004200060207     c                   Parm                    ftttsr
004300060207     c                   Parm                    fttddc
004400060207     c                   Parm                    fttndc
004500051129     C                   ENDSL
004600051129      *
004700051129     C                   ENDDO
004800051129     C/EXEC SQL
004900051219     C+ CLOSE a1
005000051129     C/END-EXEC
005100060207     c                   seton                                        lr
005200051129     C**************************************************************************
005300051129     C* esco con codice d'errore
005400051129     C**************************************************************************
005500051129     C     srfine        BEGSR
005600051129     c                   seton                                        lr
005700051129     c                   return
005800051129     c*
005900051129     c                   endsr
