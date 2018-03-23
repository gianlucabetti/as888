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
001100120706     dInizUte          s              3
001200041011
001300041011     c                   Do        *Hival
001400041011     c                   Read      Kpprf00f
001500041011      * fine file
001600041011     c                   If        %Eof(Kpprf00f)
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
002800120313      * Se non di filiale non aggiorno
002900120717     c                   IF        %Subst(PrfNsi:1:3) <> 'FIL'
003000120716     c                   Iter
003100120716     c                   ENDIF
003200041011
003300041011     c                   Movel     UteFaf        Dute01
003400041011
003500120706     c* ad esclusione di alcuni profili tolGo l'abilitazione dalla manut bolle di sede
003600120706     c                   EVAL      INIZUTE=%Subst(uteute:1:3)
003700120717     C                   IF        INIZUTE= 'RAM'
003800120717     c                   EVAL      §UteMbs = 'RA'
003900120706     C                   ENDIF
004000041011
004100041011     c                   Movel     Dute01        UteFaf
004200041011     c                   Update    Azute000
004300041011
004400041011     c                   EndDo
004500041011
004600041011     c                   Eval      *InLr = *On
