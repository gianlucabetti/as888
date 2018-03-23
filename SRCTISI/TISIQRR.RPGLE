000100970526     h*--------------------------------------------------------------------------------------------*
000200060922     h* Gestione cappario utilizzi particolari - giuda
000300970526     h*--------------------------------------------------------------------------------------------*
000400970526     h DECEDIT('0,') DATEDIT(*DMY.)
000500970526     f*--------------------------------------------------------------------------------------------*
000600060922     d tisiq0ds      E DS                                                       *ds di procedura
000700970609     d kpjba         E DS                                                       *architettura
000800970526     c*--------------------------------------------------------------------------------------------*
000900060313     c     *entry        plist
001000060313     c                   parm                    kpjba
001100060313     c*
001200060313     c* imposta la ds di procedura
001300060922     c                   CLEAR                   tisiq0ds
001400060922     c                   MOVEL     kpjbu         dq0op0                         *livello procedura
001500060922     c                   MOVEL     *blanks       dq0op1                         *livello programma
001600060922     c                   MOVEL     '0'           dq0f03                         *premuto F03
001700060922     c                   MOVEL     '0'           dq0f12                         *premuto F12
001800060922     c                   MOVEL     '0'           dq0err                         *errore
001900060922     c                   call      'TISIQ0R'
002000060313     c                   parm                    kpjba
002100060922     c                   parm                    tisiQ0ds
002200970526     C*
002300970526     C                   SETON                                        lr
