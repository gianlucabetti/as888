000100991102     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200991102
000300991102     D kpjba         e ds
000400991102     D xusrkeyds     e ds
000500991102     D uereg           s            300
000600991102     D ueerr           s              1
000700991102     D uemsgid         s              7
000701161116
000702161116      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
000800991102
000900991102     C     *entry        plist
001000991102     C                   parm                    kpjba
001100991102     C                   parm                    uereg
001200991102     C                   parm                    ueerr
001300991102     C                   parm                    uemsgid
001400991102     C*
001500991102     C                   eval      xusrkeyds = kpjbu
001600991102     C* Se non sono in visualizzazzione eseguo i controlli.
001700991102     C                   if        xukopz <> '05'
001800991102     C                   call(e)   'YCO1410C'
001900991102     C                   parm                    uereg
002000991102     C                   parm                    ueerr
002100991102     C                   parm                    uemsgid
002200991102     C                   parm                    xukopz
002300991102     C                   endif
002400991102     C*
002500991102     C                   if        %error
002600991102     C                   eval      ueerr = '1'
002700991102     C                   eval      uemsgid = 'Y000003'
002800991102     C                   endif
002801161116
002900161116            // cancello aree dati in qtemp
002901161116              ExecuteCommand('DLTDTAARA DTAARA(QTEMP/§DATIUTE)');
002902161116              ExecuteCommand('DLTDTAARA DTAARA(QTEMP/§AZUTE)');
002903161116
003000991102     C                   return
