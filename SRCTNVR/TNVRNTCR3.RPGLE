000100020605     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020605     ftfntc01l  uf   e           k disk
000300020605     c     'D'           setll     tfntc01l
000400020605    2c                   do        *hival
000500020605     c     'D'           reade     tfntc01l
000600020605     c                   if        %eof(tfntc01l)
000700020605     c                   leave
000800020605     c                   endif
000900020605     c                   delete    tfntc
001000020605    2c                   enddo
001100020605
001200020605     c                   eval      *inlr = *on
