000100000920     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*---------------------------------------------------------------*
000300071113     ffiar401l  iF   e           k disk
000400071112     Fsysprt    O    F  132        PRINTER OFLIND(*INOF)
000500071112
000600071112     d fnblpds       e ds                  extname(fnblp00f)
000700071113     d wtrc            S              1    Inz('M')
000800071113     d conta           S              5  0
000900071112
001000071113     C     kar4          klist
001100071113     C                   kfld                    blpaas
001200071113     C                   kfld                    blplnp
001300071113     C                   kfld                    blpnrs
001400071113     C                   kfld                    blpnsp
001500071113     C                   kfld                    wtrc
001600071113     c                   clear                   conta
001700071112
001800071112     C/EXEC SQL
001900071113     C+ DECLARE A1 CURSOR FOR SELECT *
002000071113     C+ FROM fnblp01l
002100071113     C+ WHERE blpdim=20071105 and blpdrt between 1 and 20071104
002200071112     C/END-EXEC
002300071112
002400071112     C/EXEC SQL
002500071112     C+ OPEN A1
002600071112     C/END-EXEC
002700071112
002800071112    1C                   Do        *hival
002900071112
003000071112     C/EXEC SQL
003100071113     C+ FETCH NEXT FROM A1 INTO :fnblpds
003200071112     C/END-EXEC
003300071112
003400071112    2c                   if        sqlcod = 100
003500071112     c                             or sqlcod < 0
003600071112     c                   leave
003700071112    2c                   endif
003800071112
003900071113     c     kar4          chain     fiar401l
004000071113     c                   if        not %found(fiar401l)
004100071113     c                   add       1             conta
004200071113     c                   except    detta
004300071113     c                   endif
004400071112
004500071112    1c                   enddo
004600071112
004700071112     C/EXEC SQL
004800071112     C+ CLOSE A1
004900071112     C/END-EXEC
005000071112
005100071112     C                   EVAL      *INLR = *ON
005200071112
005300071112     Osysprt    E            detta          1
005400071113     o                       conta               +2
005500071112     O                       blpaas              +2
005600071112     O                       blplnp              +2
005700071112     O                       blpnrs              +2
005800071112     O                       blpnsp              +2
