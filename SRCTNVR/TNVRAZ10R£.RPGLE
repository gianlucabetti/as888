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
003200041011     c                   When      %Subst(UteUte:1:6) = 'EDPCED'
003300041011     c                   Eval      助teGtc = 'RA'
003400041011     c                   Eval      助teCtc = 'RA'
003500041011
003600041011     c                   When      %Subst(UteUte:1:3) = 'RED'
003700041011     c                             and %Subst(PrfNsi:1:3) <> 'FIL'
003800041011     c                   Eval      助teGtc = 'NO'
003900041011     c                   Eval      助teCtc = 'NO'
004000041011
004100041011     c                   When      %Subst(UteUte:1:3) = 'RED'
004200041011     c                   Eval      助teGtc = 'DI'
004300041011     c                   Eval      助teCtc = 'DI'
004400041011
004500041011     c                   When      UteFil = 46
004600041011     c                   If        (%Subst(UteUte:1:3) = 'ASC' or
004700041011     c                              %Subst(UteUte:1:3) = 'POC')
004800041011     c                   Eval      助teGtc = 'NO'
004900041011     c                   Eval      助teCtc = 'NO'
005000041011     c                   Else
005100041011     c                   Eval      助teGtc = 'AZ'
005200041011     c                   Eval      助teCtc = 'AZ'
005300041011     c                   EndIf
005400041011
005500041011     c                   When      %Subst(PrfNsi:1:3) <> 'FIL'
005600041011     c                   Eval      助teGtc = 'NO'
005700041011     c                   Eval      助teCtc = 'NO'
005800041011
005900041011     c                   Other
006000041011
006100041011     c                   Clear                   og148
006200041011     c     UteFil        Chain     Azorg01l
006300041011     c                   If        %Found(Azorg01l)
006400041011     c                   Eval      og148 = orgde8
006500041011     c                   EndIf
006600041011     c                   If        OrgDit = 'BAR' and 吶glpo = '1'
006700041011     c                   Eval      助teGtc = 'RA'
006800041011     c                   Eval      助teCtc = 'RA'
006900041011     c                   EndIf
007000041011     c                   If        OrgDit = 'SDI' and 吶glpo = '1'
007100041011     c                   Eval      助teGtc = 'TP'
007200041011     c                   Eval      助teCtc = 'TP'
007300041011     c                   EndIf
007400041011     c                   If        吶glpo = '2' or 吶glpo = *Blanks
007500041011     c                   Eval      助teGtc = 'PO'
007600041011     c                   Eval      助teCtc = 'PO'
007700041011     c                   EndIf
007800041011
007900041011     c                   EndSl
008000041011
008100041011     c                   Movel     Dute01        UteFaf
008200041011     c                   Update    Azute000
008300041011
008400041011     c                   EndDo
008500041011
008600041011     c                   Eval      *InLr = *On
