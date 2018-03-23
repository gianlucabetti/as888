000100970526     h*--------------------------------------------------------------------------------------------*
000200060313     h* Gestione calendario eventi particolari - giuda
000300970526     h*--------------------------------------------------------------------------------------------*
000400970526     h DECEDIT('0,') DATEDIT(*DMY.)
000500970526     f*--------------------------------------------------------------------------------------------*
000600060313     d tisip0ds      E DS                                                       *ds di procedura
000700970609     d kpjba         E DS                                                       *architettura
000800970526     c*--------------------------------------------------------------------------------------------*
000900060313     c     *entry        plist
001000060313     c                   parm                    kpjba
001100060313     c*
001200060313     c* imposta la ds di procedura
001300060313     c                   CLEAR                   tisip0ds
001400060313     c                   MOVEL     kpjbu         dp0op0                         *livello procedura
001500060313     c                   MOVEL     *blanks       dp0op1                         *livello programma
001600060313     c                   MOVEL     '0'           dp0f03                         *premuto F03
001700060313     c                   MOVEL     '0'           dp0f12                         *premuto F12
001800060313     c                   MOVEL     '0'           dp0err                         *errore
001900060313     c                   call      'TISIP0R'
002000060313     c                   parm                    kpjba
002100060313     c                   parm                    tisip0ds
002200970526     C*
002300970526     C                   SETON                                        lr
