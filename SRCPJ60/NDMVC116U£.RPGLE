000100991102     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200991102
000300991102     D xusrkeyds     e ds                  inz
000400991102     D uereg           s            300
000500991102     D ueerr           s              1
000600991102     D uemsgid         s              7
000700011116     D WrkErr          S              1
000701161116
000702161116      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
000800991102
000900991102     C     *entry        plist
001000991102     C                   parm                    uereg
001100991102     C                   parm                    ueerr
001200991102     C                   parm                    uemsgid
001300130902      * pulisco i campi ad ogni chiamata
001400130902     C                   clear                   ueerr
001500130902     C                   clear                   uemsgid
001600991102     C*
001700991102     C                   call(e)   'YCO1410C'
001800991102     C                   parm                    uereg
001900011116     C                   parm      *BLANK        WrkErr
002000991102     C                   parm                    uemsgid
002100991102     C                   parm      '04'          xukopz
002200991102     C*
002300011116     C                   SELECT
002400011116     C                   WHEN      %error
002500991102     C                   eval      ueerr = '1'
002600991102     C                   eval      uemsgid = 'Y000003'
002700991102     C                   return
002800011116     C                   WHEN      WrkErr = '1'
002900011116     C                   eval      ueerr = '1'
003000011116     C                   ENDSL
003100991102     C*
003200991102     C                   call(e)   'YCO1420C'
003300991102     C                   parm                    uereg
003400011116     C                   parm      *BLANK        WrkErr
003500991102     C                   parm                    uemsgid
003600991102     C                   parm      '04'          xukopz
003700991102     C*
003800011116     C                   SELECT
003900011116     C                   WHEN      %error
004000011116     C                   eval      ueerr = '1'
004100011116     C                   eval      uemsgid = 'Y000003'
004200011116     C                   return
004300011116     C                   WHEN      WrkErr = '1'
004400011116     C                   eval      ueerr = '1'
004500011116     C                   ENDSL
004501161116
004600161116            // cancello aree dati in qtemp
004601161116              ExecuteCommand('DLTDTAARA DTAARA(QTEMP/§DATIUTE)');
004604161116              ExecuteCommand('DLTDTAARA DTAARA(QTEMP/§AZUTE)');
004605161116
004700991102     C                   return
