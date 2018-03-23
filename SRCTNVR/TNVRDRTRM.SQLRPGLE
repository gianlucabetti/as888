000100000920     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*---------------------------------------------------------------*
000300071112     FFNBLP01l  UF   E           k DISK
000400071112     FFnorm01L  IF   E           K DISK
000500071112     Fsysprt    O    F  132        PRINTER OFLIND(*INOF)
000600071112
000700071112     d fnblpds       e ds                  extname(fnblp00f)
000800071112     d fiar4ds       e ds                  extname(fiar400f)
000900071112     d dsbl4m        e ds
001000071112     d s_drt           s                   like(blpdrt)
001100071112
001200071112     c     korm          klist
001300071112     c                   kfld                    §b4poe
001400071112     c                   kfld                    §b4nsr
001500071112     c                   kfld                    §b4nor
001600071112     c                   kfld                    §b4nrv
001700071112     c     kblp          klist
001800071112     c                   kfld                    blpaas
001900071112     c                   kfld                    blplnp
002000071112     c                   kfld                    blpnrs
002100071112     c                   kfld                    blpnsp
002200071112
002300071112     C/EXEC SQL
002400071112     C+ DECLARE A1 CURSOR FOR SELECT fnblp01l.*, fiar401l.*
002500071112     C+ FROM fnblp01l join fiar401l on
002600071112     C+ blpaas=ar4aas and blplnp=ar4lnp and blpnrs=ar4nrs and
002700071112     C+ blpnsp=ar4nsp
002800071112     C+ WHERE blpdim=20071112 and blpdrt < 20071112
002900071112     C+ and ar4trc = 'M'
003000071112     C/END-EXEC
003100071112
003200071112     C/EXEC SQL
003300071112     C+ OPEN A1
003400071112     C/END-EXEC
003500071112
003600071112    1C                   Do        *hival
003700071112
003800071112     C/EXEC SQL
003900071112     C+ FETCH NEXT FROM A1 INTO :fnblpds, :fiar4ds
004000071112     C/END-EXEC
004100071112
004200071112    2c                   if        sqlcod = 100
004300071112     c                             or sqlcod < 0
004400071112     c                   leave
004500071112    2c                   endif
004600071112
004700071112     c* se partita stampa ed escludo
004800071112    2c                   if        blpft2<>' '
004900071112     c                   except    detta
005000071112     c                   iter
005100071112    2C                   ENDIF
005200071112
005300071112     c* vedo se legata ad orm
005400071112     c                   movel     ar4not        dsbl4m
005500071112     c     korm          chain     fnorm01l
005600071112    2c                   if        %found(fnorm01l) and ormtor<>'P'
005700071112    3C                   IF        ormfao=400 or ormfao=500 or ormfao=600
005800071112     c     kblp          chain     fnblp01l
005900071112    4c                   if        %found(fnblp01l)
006000071112     c                   z-add     blpdrt        s_drt
006100071112    5c                   if        ormfao=400
006200071112     c                   z-add     ormddc        blpdrt
006300071112   x5c                   else
006400071112     c                   z-add     ormdfo        blpdrt
006500071112    5c                   endif
006600071112     c                   if        blpdrt<>s_drt
006700071112     c                   except    aggblp
006800071112     c                   endif
006900071112    4c                   endif
007000071112   x3c                   else
007100071112     c                   except    detta
007200071112    3c                   endif
007300071112    2c                   endif
007400071112
007500071112    1c                   enddo
007600071112
007700071112     C/EXEC SQL
007800071112     C+ CLOSE A1
007900071112     C/END-EXEC
008000071112
008100071112     C                   EVAL      *INLR = *ON
008200071112
008300071112     Ofnblp000  e            aggblp
008400071112     O                       blpdrt
008500071112     Osysprt    E            detta          1
008600071112     O                       blpaas              +2
008700071112     O                       blplnp              +2
008800071112     O                       blpnrs              +2
008900071112     O                       blpnsp              +2
009000071112     O                       blpft2              +2
009100071112     O                       ormfao              +2
