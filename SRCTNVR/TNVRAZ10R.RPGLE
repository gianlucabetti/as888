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
002700120313      * Se non di filiale non aggiorno
002800120313     c                   IF        %Subst(PrfNsi:1:3) <> 'FIL'
002900120313     c                   Iter
003000120313     c                   ENDIF
003100041011
003200041011     c                   Movel     UteFaf        Dute01
003300041011
003400120313     c                   IF        助teMbs = *blanks
003500120313     c                   Eval      助teMbs = 助teGtc
003600120313     c                   ENDIF
003700120313
003800120313     c                   IF        助teIbs = *blanks
003900120313     c                   Eval      助teIbs = 助teGtc
004000120313     c                   ENDIF
004100041011
004200041011     c                   Movel     Dute01        UteFaf
004300041011     c                   Update    Azute000
004400041011
004500041011     c                   EndDo
004600041011
004700041011     c                   Eval      *InLr = *On
