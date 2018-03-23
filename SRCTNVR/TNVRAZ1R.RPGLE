000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300041011      *             Autorizzazioni tariffe                              *
000400041011      *-----------------------------------------------------------------*
000500041011     fAzute00f  uf   e             Disk
000600041011     fAzorg01l  if   e           k Disk
000700041011
000800041011     d dute01        e ds
000900041011     d og148         e ds
001000041011
001100041011     c                   Do        *Hival
001200041011     c                   Read      Azute00f
001300041011      * fine file
001400041011     c                   If        %Eof(Azute00f)
001500041011     c                   Leave
001600041011     c                   EndIf
001700041011      * scaduto non aggiorno
001800041011     c                   If        UteDsc <= *date
001900041011     c                   Iter
002000041011     c                   EndIf
002100041011      * utente pst o vdl non aggiorno
002200041011     c                   If        %Subst(UteUte:1:3) = 'PST' or
002300041011     c                             %Subst(UteUte:1:3) = 'VDL' or
002400041011     c                             %Subst(UteUte:1:6) = 'EDPCED'
002500041011     c                   Iter
002600041011     c                   EndIf
002700041011
002800041011     c                   Movel     UteFaf        Dute01
002900041011
003000041011     c                   Select
003100041011     c                   When      %Subst(UteUte:1:3) = 'RED'
003200041011     c                   Eval      助teGtc = 'DI'
003300041011     c                   Eval      助teCtc = 'DI'
003400041011     c                   When      UteFil = 46
003500041011     c                   If        (%Subst(UteUte:1:3) = 'ASC' or
003600041011     c                              %Subst(UteUte:1:3) = 'POC')
003700041011     c                   Eval      助teGtc = 'NO'
003800041011     c                   Eval      助teCtc = 'NO'
003900041011     c                   Else
004000041011     c                   Eval      助teGtc = 'AZ'
004100041011     c                   Eval      助teCtc = 'AZ'
004200041011     c                   EndIf
004300041011
004400041011     c                   Other
004500041011
004600041011     c                   Clear                   og148
004700041011     c     UteFil        Chain     Azorg01l
004800041011     c                   If        %Found(Azorg01l)
004900041011     c                   Eval      og148 = orgde8
005000041011     c                   EndIf
005100041011     c                   If        OrgDit = 'BAR' and 吶glpo = '1'
005200041011     c                   Eval      助teGtc = 'RA'
005300041011     c                   Eval      助teCtc = 'RA'
005400041011     c                   EndIf
005500041011     c                   If        OrgDit = 'SDI' and 吶glpo = '1'
005600041011     c                   Eval      助teGtc = 'TP'
005700041011     c                   Eval      助teCtc = 'TP'
005800041011     c                   EndIf
005900041011     c                   If        吶glpo = '2' or 吶glpo = *Blanks
006000041011     c                   Eval      助teGtc = 'PO'
006100041011     c                   Eval      助teCtc = 'PO'
006200041011     c                   EndIf
006300041011
006400041011     c                   EndSl
006500041011
006600041011     c                   Movel     Dute01        UteFaf
006700041011     c                   Update    Azute000
006800041011
006900041011     c                   EndDo
007000041011
007100041011     c                   Eval      *InLr = *On
