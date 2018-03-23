000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300020318     ftntbe01l  if   e           k disk
000400090316     D trmz77ds      e ds
000500090316     d kcod            s                   like(tbecod)
000600020318     d key1            s                   like(tbeke1)
000700040309     D kpjba         E DS
000800000607      *--------------------------------------------------------------------
000900000607     c
001000000607     c     *entry        plist
001100090316     c                   parm                    trmz77ds
001200090316     c                   movel     *all'N'       o77uni
001300020319     c* se sono utente EDP sono abilitato a tutto
001400090316     c                   if        %subst(i77prf: 1: 3) = 'EDP'
001500090316     c                   movel     *all'S'       o77uni
001600020319     c                   else
001700020318     c     ktbe          klist
001800020318     c                   kfld                    kcod
001900020318     c                   kfld                    key1
002000090316     c                   eval      kcod = 'AUA'
002100000607
002200090316      *cerco con chiave completa (Profilo per esteso)
002300090316     c                   eval      key1 =  i77prf
002400090316     c     ktbe          chain     tntbe01l
002500090316     c                   if        %found(tntbe01l)
002600090316     c                   movel     tbeuni        o77uni
002700020318     c                   else
002800090316      *cerco con chiave parziale (solo primi 3 caratteri profilo)
002900090316     c                   eval      key1 = %subst(i77prf: 1: 3)
003000090316     c     ktbe          chain     tntbe01l
003100090316     c                   if        %found(tntbe01l)
003200090316     c                   movel     tbeuni        o77uni
003300090316     c                   else
003400090316     c                   move      *all'N'       o77uni
003500020318     c                   end
003600020319     c                   end
003700020318
003800090316     c                   end
003900000607     c                   seton                                        lr
