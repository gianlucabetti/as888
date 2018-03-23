000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300041011      *             Autorizzazioni tariffe                              *
000400041011      *-----------------------------------------------------------------*
000500041011     fKpprf00f  if   e             Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011     fAzorg01l  if   e           k Disk
000800041011
000900120706     D PROF            S              3    DIM(25) CTDATA PERRCD(25)
001000041011     d dute01        e ds
001100041011     d og148         e ds
001200120706     dInizUte          s              3
001300041011
001400041011     c                   Do        *Hival
001500041011     c                   Read      Kpprf00f
001600041011      * fine file
001700041011     c                   If        %Eof(Kpprf00f)
001800041011     c                   Leave
001900041011     c                   EndIf
002000041011
002100041011     c     PrfNmu        Chain     Azute01l
002200041011     c                   If        Not %Found(Azute01l)
002300041011     c                   Iter
002400041011     c                   EndIf
002500041011      * scaduto non aggiorno
002600041011     c                   If        UteDsc <= *date
002700041011     c                   Iter
002800041011     c                   EndIf
002900120313      * Se non di filiale non aggiorno
003000120716     c                   IF        %Subst(PrfNsi:1:3) <> 'FIL' and
003100120716     c                             %Subst(PrfNsi:1:3) <> 'GAI'
003200120716     c                   Iter
003300120716     c                   ENDIF
003400041011
003500041011     c                   Movel     UteFaf        Dute01
003600041011
003700120706     c* ad esclusione di alcuni profili tolGo l'abilitazione dalla manut bolle di sede
003800120706     c                   EVAL      INIZUTE=%Subst(uteute:1:3)
003900120706     C                   IF        %LOOKUP(INIZUTE:ProF)= 0
004000120706     c                   EVAL      §UteMbs = 'NO'
004100120706     C                   ENDIF
004200041011
004300041011     c                   Movel     Dute01        UteFaf
004400041011     c                   Update    Azute000
004500041011
004600041011     c                   EndDo
004700041011
004800041011     c                   Eval      *InLr = *On
004900120706**
005000120713ASSCOMREAREDEDPFATINTRP1RP2
