000100051024     D
000200051024     D* Informazioni sul job
000300051024     D psds           sds
000400051024     D  psjobname            244    253
000500051024     D  psuser               254    263
000600051024     D  psjobnbr             264    269
000700051019     D*
000800051019     D**************
000900051019     D* DS ESTERNE DI PROCEDURE
001000051019     D**************
001100051024     D XLSTSPLDS     E DS
001200051019     D XLSTSPLDS2    E DS
001300051024     D*
001400041006     D********
001500051019     D* SCHIERE SPOOL RITORNATI IN OUT
001600041006     D********
001700051024     D skSpool         S                   like(XLSTSPLDS2) dim(9999)
001800051019     D                                     inz(*blanks)
001900051019     D*
002000051017     C*---------------------------------------------------------------*
002100051024     C     *ENTRY        PLIST
002200051024     C                   PARM                    wOraDa            6
002300051024     C                   PARM                    wOraA             6
002400051024     C*
002500051024     C* Valorizzo la ds d procedura x il *pgm utilità reperimento lista spool
002600051024     C                   clear                   XLSTSPLDS
002700051024     C                   eval      §LST_USER  = 'EDPFG'
002800051024     C***                eval      §LST_QJOBN = psjobname + psuser + psjobnbr
002900051024     C                   eval      §LST_QJOBN = '*'
003000051024     C                   eval      §LST_DATDA = '1051024'
003100051024     C                   eval      §LST_ORADA = wOraDa
003200051024     C                   eval      §LST_DATA  = '1051024'
003300051024     C                   eval      §LST_ORAA  = wOraA
003400051024     C                   call      'XLSTSPL'
003500051024     C                   parm                    XLSTSPLDS
003600051024     C*
003700051024     C* Quindi se tutto ok muovo lista primi 1000 spool reperiti in schiera d wrk
003800051024     C                   if        §LST_ESITO <> 'E'
003900051024     C                   movea     §LST_LISTA    skSpool
004000051024     C                   endif
004100051017     C*
004200051017     C* Ed esco
004300910702     C                   SETON                                          LR
