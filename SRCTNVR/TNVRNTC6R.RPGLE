000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200090306     Ftfntc01l  uF   E           K DISK
000600090306     Fwfntc00f  O    E             DISK    rename(tfntc:tfntcw)
000601090306     d
000602090306     d ds_tfntc      e ds                  extname(tfntc00f)
000603090306     d  key                    1     18
000604090306     d
000605090306     d savrnt          s                   like(ntcrnt)
000606090306     d savkey          s                   like(key)
000700041209     D
001300041209     d*
004100041210     c*
007600090306     c     *loval        setll     tfntc01l
007700041209     c                   do        *hival
007800090306     c                   read      tfntc01l
007900041210     c* fine file
008000090306     c                   if        %eof(tfntc01l)
008100041209     c                   leave
008200041209     c                   endif
008201090306     c                   if        ntctnt<> '02' and
008202090306     c                             ntctnt<> '03' and
008203090306     c                             ntctnt<> '06' and
008204090306     c                             ntctnt<> '07' and
008205090306     c                             ntctnt<> '08' and
008206090306     c                             ntctnt<> '83' and
008207090306     c                             ntctnt<> '84' and
008208090306     c                             ntctnt<> '85' and
008209090306     c                             ntctnt<> '86' and
008210090306     c                             ntctnt<> '87' and
008211090306     c                             ntctnt<> '88'
008212090306     c                   iter
008213090306     c                   endif
008214090306     c                   if        savkey=key    and savkey<>*blanks
008215090309     c                   if        savrnt<>ntcrnt
008216090309     c                   write     tfntcw
008217090309     c                   endif
008218090309     c                   delete    tfntc
008222090306     c                   endif
008223090306     c                   eval      savkey=key
008224090306     c                   eval      savrnt=ntcrnt
019800041210     c*
019900041209     c                   enddo
020700961108     C*
020800000000     C                   SETON                                        LR
