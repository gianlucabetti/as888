000100020318     H DECEDIT('0,') DATEDIT(*DMY/) option(*nodebugio)
000200020318
000300110406     funmov01l  uf   e           k disk
000400110406     funana01l  if   e           k disk
000500020318
000600110406     c
000700110406     c     kuna          klist
000800110406     c                   kfld                    unmtip
000900110406     c                   kfld                    unmcod
001000110406     c
001100110406     c
001200110406     c     *loval        setll     unmov01l
001300110406     c                   read      unmov01l
001400110406     c
001500110406     c                   dow       not %eof(unmov01l)
001600110406     c     kuna          chain     unana01l
001700110406     c                   if        %found(unana01l)
001800110406     c                   eval      unmmat=unamat
001900110406     c                   update    unmov000
002000110406     c                   endif
002100110406     c
002200110406     c                   read      unmov01l
002300110406     c                   enddo
002400020320
002500110406     c                   seton                                        lr
