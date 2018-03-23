000100020509     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020509
000300140619     d Tntu05ds      e ds
000400091124     d Kpjba         e ds
000500091124
000600091124     c     *entry        plist
000700091124     c                   parm                    kpjba
000800040923
000900140619     c                   Clear                   Tntu05ds
001000140619     c                   eval      kpjbu = tntu05ds
001100091014
001200140619     c                   Call      'TNTU96R'
001300091124     c                   Parm                    kpjba
001400020808
001500020529     c                   eval      *inlr = *on
