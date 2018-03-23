000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300140226     ftabel00f  uf   e           k disk
000400140226     ftitas31c  if   e           k disk
000401140226     fcnaco00f  if   e           k disk
000800000607      *--------------------------------------------------------------------
000801140226     c     kaco          klist
000802140226     c                   kfld                    KUT               1 0
000803140226     c                   kfld                    KCC               4 0
000804140226     c                   kfld                    wrk7              7 0
000805140226      *
000806140226     c                   z-add     1             KUT
000807140226     c                   z-add     151           KCC
000900040409      * lettura
001000140226     c     ktab          setll     tabel00f
001100000607     c                   do        *hival
001200140226     c     ktab          reade     tabel00f
001300140226     c                   if        %eof(tabel00f)
001400000607     c                   leave
001500000607     c                   end
001501140226     c                   movel     tblkey        wrk7              7 0
002000140226     c     wrk7          chain     titas31c
002100140226     c                   if        %found(titas31c)
002200140218     c                   iter
002300051130     c                   end
002301140226     c     kaco          chain     cnaco00f
002302140226     c                   if        %found(cnaco00f) and acoabl <> *blank
002303140226     c                   delete    tabel000
002304140226     c                   endif
006600000607
006700051130     c                   enddo
006800000607     c                   seton                                        lr
