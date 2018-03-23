000100041230     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200041230      *---------------------------------------------------------------*
000300050111      * Modifico stato nota FITGN                                     *
000400041230      *---------------------------------------------------------------*
000500050111     fFITGN01L  uf   e           k disk
000600041230      *
000700050112      *
000800050111     D NOch            C                   CONST('Modificato codice chiusura')
000900050111     D NOpo            C                   CONST('Modificato P.O. responsabi')
001000050111     D NOrp            C                   CONST('Modificato Utente in Gesti')
001100050111
001200041231     c                   do        *hival
001300050112     c                   read      fitgn01l
001400050112     c                   if        %eof(fitgn01l)
001500041231     c                   leave
001600041231     c                   endif
001700050112      * se flag già impostato ad 'N' leggo il successivo
001800050112     c                   If        tgnsts = 'N'
001900041231     c                   iter
002000041231     c                   endif
002100041231
002200050112     c                   exsr      CTR_note
002300041230      *
002400041231     c                   enddo
002500041231      *
002600041231     c                   seton                                        lr
002700041230      *
002800041230      *---------------------------------------------------------------*
002900050112      * Routine controllo note
003000041230      *---------------------------------------------------------------*
003100050112     c     CTR_note      Begsr
003200041230
003300050112      * verifico se la nota è uguale a uno di queste costanti
003400050112
003500050112     c                   if        %subst(tgnnot:1:26) = noch or
003600050112     c                             %subst(tgnnot:1:26) = nopo or
003700050112     c                             %subst(tgnnot:1:26) = norp
003800050112     c                   eval      tgnsts = 'N'
003900050112     c                   update    fitgn000
004000050112      * leggo il record successivo perchè sono sempre 2
004100050112     c                   read      fitgn01l
004200050112     c                   if        NOT %EOF(fitgn01l)
004300050112     c                   eval      tgnsts = 'N'
004400050112     c                   update    fitgn000
004500050112     c                   endif
004600050112     c                   endif
004700050112
004800041230     c                   endsr
