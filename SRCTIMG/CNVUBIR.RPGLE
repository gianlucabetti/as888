000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000201131030     Ftmamp01L  UF   E           K DISK
001800000607      *--------------------------------------------------------------------
001900000607     c
002000131030     c     *loval        setll     tmamp01l
002100131030     c                   do        *hival
002101131030     c                   read      tmamp01l
002102131030     c                   if        %eof(tmamp01l)
002105131030     C                   leave
002106131030     C                   endif
002107131030     c                   if        ampfil <> *blank
002108131030     c                   iter
002109131030     c                   endif
002110131030     c                   movel     ampubi        ampfil
002111131030     c                   update    tmamp000
002804131030     c                   enddo
002900131030     c                   seton                                        lr
