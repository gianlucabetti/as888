000100020715     H DATEDIT(*YMD.)
000200000921
000300000921      *------------------------------------------------------------------------*
000400000921      *                                                                        *
000500080423      *  pulisce ex campi �15cex/�15cfe/�15cdp in quanto non pi� utilizzati    *
000600080423      *                                                                        *
000700000921      *------------------------------------------------------------------------*
000800000921
000900061031     ftabel00f  uf   e           k disk
001000020403
001100061031     d ds15          e ds
001200001009
001300020403     c     ktabel        klist
001400020403     c                   kfld                    codut             1 0
001500020403     c                   kfld                    kcod              2
001600020403
001700020403     c                   eval      codut = 1
001800061031     c                   do        4
001900061031     c                   eval      kcod = '15'
002000010709
002100020715     c     ktabel        setll     tabel00f
002200020715     c     ktabel        reade     tabel00f
002300020715     C                   dow       NOT  %EOF(TABEL00F)
002400010709
002500061031     c                   movel     tbluni        ds15
002600080424     c                   clear                   �15fl1
002700061031     c                   movel     ds15          tbluni
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
