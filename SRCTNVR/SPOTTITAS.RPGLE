000100020228     ftitas00F  IP   E             DISK
000200020228     fTiTAa30c  IF   E           K DISK
000300020228     fOOTAa30C  UF a E           K DISK    rename(TiTAa000:OOTAa000)
000400020228     f                                     rename(TiTAaP00:OOTAaP00)
000500020228     fTiTA430c  IF   E           K DISK
000600020228     fOOTA430C  UF a E           K DISK    rename(TiTA4000:OOTA4000)
000700020228     f                                     rename(TiTA4P00:OOTA4P00)
000800020228     fTiTA730c  IF   E           K DISK
000900020228     fOOTA730C  UF a E           K DISK    rename(TiTA7000:OOTA7000)
001000020228     f                                     rename(TiTA7P00:OOTA7P00)
001100981125
001200981125     c     kbol          klist
001300020228     c                   kfld                    tasaas
001400020228     c                   kfld                    taslnp
001500020228     c                   kfld                    tasnrs
001600020228     c                   kfld                    tasnsp
001700020228     c     kbola         klist
001800020228     c                   kfld                    taaaas
001900020228     c                   kfld                    taalnp
002000020228     c                   kfld                    taanrs
002100020228     c                   kfld                    taansp
002200020228     c                   kfld                    taatrc
002300020228     c     kbol4         klist
002400020228     c                   kfld                    ta4aas
002500020228     c                   kfld                    ta4lnp
002600020228     c                   kfld                    ta4nrs
002700020228     c                   kfld                    ta4nsp
002800020228     c                   kfld                    ta4trc
002900020228     c     kbol7         klist
003000020228     c                   kfld                    ta7aas
003100020228     c                   kfld                    ta7lnp
003200020228     c                   kfld                    ta7nrs
003300020228     c                   kfld                    ta7nsp
003400020228     c                   kfld                    ta7tbl
003500020228     c                   kfld                    ta7svn
003600981125
003700020228     c     kbol          setll     titaa30c
003800020228     c                   do        *hival
003900020228     c     kbol          reade     titaa30c
004000020228     c                   IF        not %eof(titaa30c)
004100020228     c     kbola         chain     ootaa30c                           01
004200020228     c   01              write     OOTAa000
004300020228     c                   else
004400020228     c                   leave
004500010404     c                   endif
004600020228     c                   enddo
004700010404
004800020228     c     kbol          setll     tita430c
004900020228     c                   do        *hival
005000020228     c     kbol          reade     tita430c
005100020228     c                   IF        not %eof(tita430c)
005200020228     c     kbol4         chain     oota430c                           01
005300020228     c   01              write     OOTA4000
005400020228     c                   else
005500020228     c                   leave
005600020228     c                   endif
005700020228     c                   enddo
005800020228
005900020228     c     kbol          setll     tita730c
006000020228     c                   do        *hival
006100020228     c     kbol          reade     tita730c
006200020228     c                   IF        not %eof(tita730c)
006300020228     c     kbol7         chain     oota730c                           01
006400020228     c   01              write     OOTA7000
006500020228     c                   else
006600020228     c                   leave
006700020228     c                   endif
006800020228     c                   enddo
006900020228
