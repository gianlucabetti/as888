000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300091125      *             Autorizzazioni utente                               *
000400041011      *-----------------------------------------------------------------*
000500070111     fKpprf01l  if   e           K Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011     fAzorg01l  if   e           k Disk
000800041011
000900041011     d dute01        e ds
001000041011     d og148         e ds
001100041011
001200070111     c     *loval        setll     Kpprf01l
001300041011     c                   Do        *Hival
001400070111     c                   Read      Kpprf01l
001500041011      * fine file
001600070111     c                   If        %Eof(Kpprf01l)
001700041011     c                   Leave
001800041011     c                   EndIf
001900091125     c                   if        prfnsi='FILTRA201 '
002000041011
002100041011     c     PrfNmu        Chain     Azute01l
002200041011     c                   If        Not %Found(Azute01l)
002300041011     c                   Iter
002400041011     c                   EndIf
002500041011      * scaduto non aggiorno
002600041011     c                   If        UteDsc <= *date
002700041011     c                   Iter
002800041011     c                   EndIf
002900041011
003000041011     c                   Movel     UteFaf        Dute01
003100041011
003200041011     c                   Select
003300041011
003400110509     c***                When      %Subst(UteUte:1:3) = 'REA' or
003500110509     c***                          %Subst(UteUte:1:3) = 'RED'
003600110509     c***                Eval      §UTEMNTSP = 'S'
003601110509     c                   When      %Subst(UteUte:1:3) = 'REA'
003603110509     c                   Eval      §UTEstp = 'S'
003700041011
003800041011     c                   Movel     Dute01        UteFaf
003900041011     c                   Update    Azute000
004000041021
004100041021     c                   EndSl
004200070111     c                   endif
004300041011
004400041011     c                   EndDo
004500041011
004600041011     c                   Eval      *InLr = *On
