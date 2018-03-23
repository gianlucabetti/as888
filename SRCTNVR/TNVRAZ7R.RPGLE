000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300041011      *             Autorizzazioni tariffe                              *
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
001900041011
002000041011     c     PrfNmu        Chain     Azute01l
002100041011     c                   If        Not %Found(Azute01l)
002200041011     c                   Iter
002300041011     c                   EndIf
002400041011      * scaduto non aggiorno
002500041011     c                   If        UteDsc <= *date
002600041011     c                   Iter
002700041011     c                   EndIf
002800041011
002900041011     c                   Movel     UteFaf        Dute01
003000041011
003100041011     c                   Select
003200041011
003300070410     c                   When      %Subst(UteUte:1:3) = 'RED' or
003400070410     c                             %Subst(UteUte:1:3) = 'EDP'
003500070410     c                   Eval      §Uteflna = 'S'
003600041011
003700041011     c                   Movel     Dute01        UteFaf
003800041011     c                   Update    Azute000
003900041021
004000041021     c                   EndSl
004100041011
004200041011     c                   EndDo
004300041011
004400041011     c                   Eval      *InLr = *On
