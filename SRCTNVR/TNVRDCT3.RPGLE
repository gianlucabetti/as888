000100050217     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050217      *------------------------------------------------------------------------*
000300050225      * Elimina c.a. annullate
000400050217      *------------------------------------------------------------------------*
000500050225     fFndct01l  if   e           k disk
000600050225
000700050225     d Fidn11ds      e ds
000800050225     d kpjba         e ds
000900050225
001000050225     c     *Entry        plist
001100050225     c                   parm                    kpjba
001200050217
001300050217     c                   Do        *Hival
001400050225     c                   Read      Fndct01l
001500050225     c                   If        %Eof(Fndct01l)
001600050217     c                   Leave
001700050217     c                   EndIf
001800050225     c                   If        dctatb <> 'A'
001900050225     c                   Iter
002000050225     c                   EndIf
002100050225
002200050225     c                   Clear                   Fidn11ds
002300050225     c                   Eval      i11mod = '*'
002400050225     c                   Eval      i11aac = dctaac
002500050225     c                   Eval      i11fil = dctfil
002600050225     c                   Eval      i11nca = dctnca
002700050225     c                   call      'FIDN11R'
002800050225     c                   parm                    kpjba
002900050225     c                   parm                    fidn11ds
003000050217
003100050217     c                   EndDo
003200050217
003300050217     c                   Eval      *Inlr = *On
003400050217
