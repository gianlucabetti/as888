000100991102     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200991102
000300991102     D kpjba         e ds
000400991102     D xusrkeyds     e ds
000500991102     D uereg           s            300
000600991102     D ueerr           s              1
000700991102     D uemsgid         s              7
000801170724
000802161116      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
000803170724      /COPY GAITRASRC/SRCPROTOPR,YIBS34R
000804170721
000900991102     C     *entry        plist
001000991102     C                   parm                    kpjba
001100991102     C                   parm                    uereg
001200991102     C                   parm                    ueerr
001300991102     C                   parm                    uemsgid
001400991102     C*
001500991102     C                   eval      xusrkeyds = kpjbu
001600170721     C* Se sono in visualizzazione non eseguo i controlli.
001700170721     C                   if        xukopz = '05'
001701170721     C                   clear                   ueerr
001702170721     C                   clear                   uemsgid
001703170721     C                   return
001704170721     C                   endif
001705170721     C
001800991102     C                   call(e)   'YCO1410C'
001900991102     C                   parm                    uereg
002000991102     C                   parm                    ueerr
002100991102     C                   parm                    uemsgid
002200991102     C                   parm                    xukopz
002400991102     C*
002500991102     C                   if        %error
002600991102     C                   eval      ueerr = '1'
002700991102     C                   eval      uemsgid = 'Y000003'
002800991102     C                   endif
002900161116
002901170721      // Rigenero le aree dati dell'utente lavoro.
002902170724     C                   CALLP     Yibs34r()
002903170724
003000991102     C                   return
