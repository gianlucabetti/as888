000100070202     h decedit('0,') option(*nodebugio)
000200070315      *--------------------------------------------------------------------------------------------*
000300080219      * Pgm autostartjob che per ogni p.o. sottomette                                              *
000400080219      * un JOB per la ricezione dati da pistola radio                                              *
000500070315      *--------------------------------------------------------------------------------------------*
000600070207     fazorg01l  if   e           k disk
000601080219     fazute01l  if   e           k disk
000700070207
000800070202     d parpo           s              3
000801080219     d profilo         s             10
000802080219     d comman          S            100                                         *comando CLP
000803080219     d lenght          S             15  5 INZ(100)                             *lunghezza comando
000900070315
001000070315      *--------------------------------------------------------------------------------------------*
001100070205
001200070207     c     *loval        setll     azorg01l
001300070207     c                   do        *hival
001400070606     c                   read      azorg01l
001500070205
001600070607      * fine file
001700070207    1c                   if        %eof(azorg01l)
001800070207     c                   leave
001900070207     c                   endif
002000070607
002100070607      * escludo gli annullati e se non è Filiale o Agenzia
002200070607     c                   if        orgfva <> *blanks or
002300070607     c                             (orgfag <> 'F' and orgfag <> 'A')
002400070607     c                   iter
002500070607     c                   endif
002501080219
002502080219     c                   move      orgfil        parpo
002503080219
002504080219      * verifico esistenza profilo
002505080219     C                   eval      profilo = 'PR' + parpo
002506080219     c     profilo       chain     azute000
002600070205
002601080219     c                   if        %found
002602080219     c                   eval      comman =
002603080219     c                             'sbmjob cmd(call pgm(TRTCP8C) PARM('''
002604080219     c                              + parpo + ''')) job(' + profilo + ') -
002606080219     c                              JOBQ(WIFIPR) USER(' + profilo + ')'
002900080219
002901080219     c                   call      'QCMDEXC'
002902080219     c                   parm                    comman
002903080219     c                   parm                    lenght
003002080219     c                   endif
003100070205
003200070205     c                   enddo
003300070205
003400070205     c                   eval      *inlr = *on
