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
003100041207
003200041207     c                   When      %Subst(UteUte:1:3) = 'RED'
003300041207     c                             and %Subst(PrfNsi:1:3) <> 'FIL'
003400041220     c                   Eval      助tePot = 'NO'
003500041220     c                   Eval      助teCli = 'NO'
003600041207
003700041207     c                   When      %Subst(UteUte:1:3) = 'RED'
003800041220     c                   Eval      助tePot = 'DI'
003900041220     c                   Eval      助teCli = 'DI'
004000041220
004100041220     c                   When      %Subst(UteUte:1:3) = 'DIV'
004200041220     c                             and %Subst(PrfNsi:1:3) <> 'FIL'
004300041220     c                   Eval      助tePot = 'NO'
004400041220     c                   Eval      助teCli = 'NO'
004500041220
004600041220     c                   When      %Subst(UteUte:1:3) = 'DIV'
004700041220     c                   Eval      助tePot = 'DI'
004800041220     c                   Eval      助teCli = 'DI'
004900041207
005000041207     c                   When      UteFil = 46
005100041207     c                   If        (%Subst(UteUte:1:3) = 'ASC' or
005200041207     c                              %Subst(UteUte:1:3) = 'POC')
005300041220     c                   Eval      助tePot = 'NO'
005400041220     c                   Eval      助teCli = 'NO'
005500041207     c                   Else
005600041220     c                   Eval      助tePot = 'AZ'
005700041220     c                   Eval      助teCli = 'AZ'
005800041207     c                   EndIf
005900041011
006000041207     c                   When      %Subst(PrfNsi:1:3) <> 'FIL'
006100041220     c                   Eval      助tePot = 'NO'
006200041220     c                   Eval      助teCli = 'NO'
006300041207
006400041220     c                   When      Utefil = 26 or Utefil = 31 or
006500041220     c                             Utefil = 32 or Utefil = 53 or
006600041220     c                             Utefil = 62 or Utefil = 66 or
006700041220     c                             Utefil = 113
006800041220     c                   Eval      助tePot = 'RA'
006900041220     c                   Eval      助teCli = 'TP'
007000041207
007100041207     c                   Other
007200041207
007300041207     c                   Clear                   og148
007400041207     c     UteFil        Chain     Azorg01l
007500041207     c                   If        %Found(Azorg01l)
007600041207     c                   Eval      og148 = orgde8
007700041207     c                   EndIf
007800041207     c                   If        OrgDit = 'BAR' and 吶glpo = '1'
007900041220     c                   Eval      助tePot = 'RA'
008000041220     c                   Eval      助teCli = 'RA'
008100041207     c                   EndIf
008200041207     c                   If        OrgDit = 'SDI' and 吶glpo = '1'
008300041220     c                   Eval      助tePot = 'RA'
008400041220     c                   Eval      助teCli = 'TP'
008500041207     c                   EndIf
008600041207     c                   If        吶glpo = '2' or 吶glpo = *Blanks
008700041220     c                   Eval      助tePot = 'RA'
008800041220     c                   Eval      助teCli = 'TP'
008900041207     c                   EndIf
009000041207
009100041207     c                   EndSl
009200041011
009300041011     c                   Movel     Dute01        UteFaf
009400041011     c                   Update    Azute000
009500041011
009600041011     c                   EndDo
009700041011
009800041011     c                   Eval      *InLr = *On
