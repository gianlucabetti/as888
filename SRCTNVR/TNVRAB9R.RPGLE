000100940224     H*--------------------------------------------------------------*
000200030205     FFIFRE00F  UF   E             DISK
000300030205     FTNTBE01L  iF   E           k DISK
000400900131     C*---------------------------------------------------------------*
000500030205     d dcre          e ds
000600030205     C*---------------------------------------------------------------*
000700030205     c     ktbe          klist
000800030205     c                   kfld                    tbecod
000900030205     c                   kfld                    tbeke1
001000030205     c                   kfld                    tbeke2
001100030205      *
001200030205     c                   eval      tbecod = 'CRE'
001300030205     c                   clear                   tbeke1
001400030205     c                   clear                   tbeke2
001500030515     c*
001600030515     c                   do        *hival
001700030515     c                   read      fifre00f
001800030515     c* fine file
001900030515     c                   if        %eoF(Fifre00f)
002000030515     c                   leave
002100030515     c                   end
002200030515     c* solo rettifiche autotrasp.
002300030515     c                   if        fretip <> 'A'
002400030515     c                   iter
002500030515     c                   end
002600030205     C*
002700030205     c                   movel     frecre        tbeke1
002800030205     c     ktbe          chain     tntbe01l
002900030205     c                   if        %Found(tntbe01l)
003000030205     c                   movel     tbeuni        dcre
003100030205     c                   eval      fretsr = §CrTSr
003200030205     c                   end
003300030205     c                   update    fifre000
003400030205     c                   enddo
003500000616     C*
003600000000     C                   SETON                                        LR
