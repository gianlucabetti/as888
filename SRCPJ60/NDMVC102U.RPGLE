000100991102     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200991102
000300991102     D xusrkeyds     e ds                  inz
000400991102     D uereg           s            300
000500991102     D ueerr           s              1
000600991102     D uemsgid         s              7
000603170724
000605161116      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
000700170724      /COPY GAITRASRC/SRCPROTOPR,YIBS34R
000701170721
000800991102     C     *entry        plist
000900991102     C                   parm                    uereg
001000991102     C                   parm                    ueerr
001100991102     C                   parm                    uemsgid
001200991102     C*
001300991102     C                   call(e)   'YCO1420C'
001400991102     C                   parm                    uereg
001500991102     C                   parm                    ueerr
001600991102     C                   parm                    uemsgid
001700991102     C                   parm      *blanks       xukopz
001800991102     C*
001900991102     C                   if        %error
002000991102     C                   eval      ueerr = '1'
002100991102     C                   eval      uemsgid = 'Y000003'
002200991102     C                   endif
002300161116
002301170721      // Rigenero le aree dati dell'utente lavoro.
002302170724     C                   CALLP     Yibs34r()
002303170724
002400991102     C                   return
