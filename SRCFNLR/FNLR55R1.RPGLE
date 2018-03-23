000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300000608     ffnlr55d1  cf   e             workstn
000400000607     D kpjba         e ds
000500000607     D dslr01        e ds
000501020823     D ddatiute      e ds
000502020823     D azuteds       e ds                  extname(AZUTE00F)
000503020823     D tibs34ds      E DS                  inz
000504020823     c* ds x passaggio parametri
000505020823     D fnlr55          DS
000506020823     D  po55                          3  0
000507020823     d  knfv5                         5  0
000600000607      *--------------------------------------------------------------------
000700000607     c
000800000607     c     *entry        plist
000900000607     c                   parm                    kpjba
001001020823     C* Decodifica Elaboratore
001002020823     c     *dtaara       define    §azute        azuteds
001003020823     c     *dtaara       define    §datiute      ddatiute
001004020823     C                   in(E)     *dtaara
001005020823     C                   IF        %Error  or  RSUT = *blanks
001006020823     C                   call      'TIBS34R'
001007020823     C                   parm                    Tibs34Ds
001008020823     C                   in        *dtaara
001009020823     c                   ENDIF
001011020823     c                   move      *zeros        v1cndd
001100000607
001200000607     c                   do        *hival
001300000607     c                   exfmt     video1
001400000607     c   kc              leave
001500000607     C     '?'           SCAN      V1CNDD                                 32
001600000607     c                   if        *in32
001700000607     c                   clear                   dslr01
001800000607     c                   move      '§'           dlrric
001900000607     c                   move      4             dlrnpg
002000000607     c                   z-add     0             dlrnfv
002001020823     c                   z-add     dutpou        dlrfgs
002100000607     c                   movel     dslr01        kpjbu
002200000607     c                   call      'FNLR02R'
002300000607     c                   parm                    kpjba
002400000607     c                   movel     kpjbu         dslr01
002500000607     c                   movel     dlrnfv        v1cndd
002600000607     c                   else
002700000608     c                   move      v1cndd        dlrnfv5           5 0
002800000608     c                   z-add     dlrnfv5       dlrnfv
002900000607     c                   endif
002901020823     c* attualmente forzo il po utente. Un domani si può prevedere il po
002902020823     c* opzionale a video
003000000607     c                   if        *inkf and dlrnfv > 0
003100020823     c                   z-add     DUTPOU        po55
003101020823     c                   z-add     dlrnfv        knfv5
003102020823     c                   movel(p)  fnlr55        kpjbu
003200000608     c                   call      'FNLR55R2'
003300000607     c                   parm                    kpjba
003400000607     c                   leave
003500000607     c                   endif
003600000607     c                   enddo
003700000607     c                   seton                                        lr
