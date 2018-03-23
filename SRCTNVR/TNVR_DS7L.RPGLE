000100020715     H DATEDIT(*YMD.)
000200000921
000300000921      *------------------------------------------------------------------------*
000400000921      *                                                                        *
000500061113      *  aggiorna campo §7LVDCO E §7LVDCOP                                     *
000600000921      *------------------------------------------------------------------------*
000700000921
000800061031     ftabel00f  uf   e           k disk
000900020403
001000061113     d ds7L          e ds
001100001009
001200020403     c     ktabel        klist
001300020403     c                   kfld                    codut             1 0
001400020403     c                   kfld                    kcod              2
001500020403
001600020403     c                   eval      codut = 1
001700061031     c                   do        4
001800061113     c                   eval      kcod = '7L'
001900010709
002000020715     c     ktabel        setll     tabel00f
002100020715     c     ktabel        reade     tabel00f
002200020715     C                   dow       NOT  %EOF(TABEL00F)
002300010709
002400061113     c                   movel     tbluni        ds7L
002500061113     c                   movel     §7LVDC        §7LVDCO
002600061113     c                   movel     §7LVDP        §7LVDCOP
002700061113     c                   movel     ds7l          tbluni
002800061031     c                   clear                   tblftr
002900061031     c                   clear                   tbldtr
003000061031     c                   update    tabel
003100020403
003200020715     c     ktabel        reade     tabel00f
003300020715     c                   enddo
003400061031     c
003500061031     c                   add       1             codut
003600061031     c                   enddo
003700020715
003800020403     c                   eval      *inlr = *on
