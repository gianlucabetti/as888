000100060421     H DECEDIT('0,') DATEDIT(*YMD.)
000200050930     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300050930
000400100514     Fcnc0wa02d CF   E             WORKSTN
000700050930
000900100517     D totale          s             11s 2
005800060302      *--------------------------------------------------------------------
005801100514     C     *entry        plist
005802100514     C                   parm                    totale
005803100514     c                   clear                   vtotale
005804100521     c                   setoff                                       66
005805100521     c     emetti        tag
006600060503     c                   do        *hival
006800060421     c                   exfmt     f01
006801100521     c                   setoff                                       66
009201100521     c                   if        vtotale <= totale
009300100514     c                   z-add     vtotale       totale
009301100521     c                   else
009302100521     c                   seton                                        66
009303100521     c                   goto      emetti
009304100521     c                   endif
009305100521     c   kf              leave
009306100521     c                   enddo
009307100517     c                   write     frcdta
009400060302     c                   seton                                        lr
009500050930
