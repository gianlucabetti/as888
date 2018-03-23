000100020715     H DATEDIT(*YMD.)
000200000921      *------------------------------------------------------------------------*
000300000921      *                                                                        *
000400000921      *------------------------------------------------------------------------*
000500150616     faitra00f  uf   e           k disk
000600150616      *
000700150616     c                   read      aitra00f
000800150616     C                   dow       NOT  %EOF(aitra00F)
000900150616      *-
001000150616     c                   if        trafiller <> *blank
001100150616     c                   clear                   data8             8
001200150616     c                   clear                   data_08           8
001300150616     c                   eval      data8 = %subst(trafiller:12:8)
001400150616      * la data è in GGMMAAAA
001500150616     c                   if        %subst(data8:5:2)='20'
001600150616     c                   eval      data_08 = %subst(data8:5:4)+
001700150616     c                             %subst(data8:3:2) + %subst(data8:1:2)
001800150616     c                   move      data_08       tradp4
001900150616     c                   else
002000150616     c                   move      data8         tradp4
002100150616     c                   end
002200150616      *
002300150616     c                   clear                   trafiller
002400150616     c                   end
002500150616      *-
002600150616     c                   update    aitra000
002700150616     c                   read      aitra00f
002800020715     c                   enddo
002900150616      *
003000020403     c                   eval      *inlr = *on
