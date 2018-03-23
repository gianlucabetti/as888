000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300041011      *             Autorizzazioni tariffe                              *
000400041011      *-----------------------------------------------------------------*
000500041011     fKpprf00f  if   e             Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011     fAzorg01l  if   e           k Disk
000800041011
000900041011     d dute01        e ds
001000041011     d og148         e ds
001100041011
001200041011     c                   Do        *Hival
001300041011     c                   Read      Kpprf00f
001400041011      * fine file
001500041011     c                   If        %Eof(Kpprf00f)
001600041011     c                   Leave
001700041011     c                   EndIf
001800041011
001900041011     c     PrfNmu        Chain     Azute01l
002000041011     c                   If        Not %Found(Azute01l)
002100041011     c                   Iter
002200041011     c                   EndIf
002300041011      * scaduto non aggiorno
002400041011     c                   If        UteDsc <= *date
002500041011     c                   Iter
002600041011     c                   EndIf
002700041011
002800041011     c                   Movel     UteFaf        Dute01
002900041011
003000041011     c                   Select
003100041011
003200041021     c                   When      %Subst(UteUte:1:3) <> 'REA' and
003300041021     c                             %Subst(UteUte:1:3) <> 'RED' and
003400041021     c                             UteFil = 113  and §uteGtc = 'RA'
003500041021     c                   Eval      §UteGtc = 'TP'
003600041021     c                   Eval      §UteCtc = 'TP'
003700041011
003800041011     c                   Movel     Dute01        UteFaf
003900041011     c                   Update    Azute000
004000041021
004100041021     c                   EndSl
004200041011
004300041011     c                   EndDo
004400041011
004500041011     c                   Eval      *InLr = *On
