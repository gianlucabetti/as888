000100020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200070928     Ffiar401l  uF   E           K DISK
000300070928     Ffiar400f  IF   E             DISK    prefix(L_)
000400070928     f                                     rename(fiar4000:fiar4003)
000500070928      *
000600070928     c     kar4          klist
000700070928     c                   kfld                    l_ar4aas
000800070928     c                   kfld                    l_ar4lnp
000900070928     c                   kfld                    l_ar4nrs
001000070928     c                   kfld                    l_ar4nsp
001100070928     c                   kfld                    l_ar4trc
001200070928     c
001300020603     c                   do        *hival
001400070928     c                   read      fiar400f
001500070928     c                   if        %eof(fiar400F)
001600020603     c                   leave
001700020603     c                   end
001800070928     c*
001900070928     c                   if        l_ar4trc <> 'A'
002000020603     c                   iter
002100020603     c                   end
002200070928     c
002300070928     c* Chaino per update
002400070928     c     kar4          chain     fiar401l
002500070928     c* Pulisco i campi
002600070928     c                   eval      %subst(ar4not:2:14)='              '
002700070928     c                   update    fiar4000
002800070928     c
002900020603     c                   enddo
003000070928     c
003100020603     c                   seton                                        lr
