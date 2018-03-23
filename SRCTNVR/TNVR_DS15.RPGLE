000100020715     H DATEDIT(*YMD.)
000200000921
000300000921      *------------------------------------------------------------------------*
000400000921      *                                                                        *
000500061031      *  aggiorna campo §25CEX = s per abilitare tutte le nazione euroexpres   *
000600061031      *       al C/A                                                           *
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
002600061031     c                   movel     ' '           §15cex
002700061031     c                   if        §15eex='S'
002800061031     c                   movel     'S'           §15cex
002900061031     c                   endif
003000061031     c                   movel     ' '           §15cdp
003100061031     c                   movel     ' '           §15cfe
003200061031     c                   movel     ds15          tbluni
003300061031     c                   clear                   tblftr
003400061031     c                   clear                   tbldtr
003500061031     c                   update    tabel
003600020403
003700020715     c     ktabel        reade     tabel00f
003800020715     c                   enddo
003900061031     c
004000061031     c                   add       1             codut
004100061031     c                   enddo
004200020715
004300020403     c                   eval      *inlr = *on
