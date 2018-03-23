000100151020     /*END
000200151020      *-----------------------------------------------------------------*
000300160610      *             Aut. statistica movimento                           *
000400151020      *-----------------------------------------------------------------*
000500041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000600151020
000700070111     fKpprf01l  if   e           K Disk
000800041011     fAzute01l  uf   e           k Disk
000900041011
001000041011     d dute01        e ds
001100041011
001200150204     c                   Eval      *InLr = *On
001300150204      *
001400070111     c     *loval        setll     Kpprf01l
001500041011     c                   Do        *Hival
001600070111     c                   Read      Kpprf01l
001700041011      * fine file
001800070111     c                   If        %Eof(Kpprf01l)
001900041011     c                   Leave
002000041011     c                   EndIf
002100151020      * Utenti di filiale
002200151020     c                   if        PRFnsi = 'FILTRA201 '  or
002300151020     c                             PRFnsi = 'FILTRAPFP '
002400041011
002500041011     c     PrfNmu        Chain     Azute01l
002600041011     c                   If        Not %Found(Azute01l)
002700041011     c                   Iter
002800041011     c                   EndIf
002900140605
003000041011      * scaduto non aggiorno
003100041011     c                   If        UteDsc <= *date
003200041011     c                   Iter
003300041011     c                   EndIf
003400041011
003500041011     c                   Movel     UteFaf        Dute01
003600041011
003700160610     c                   select
003800160610     c                   when      %Subst(uteute:1:3) = 'RED'
003900160610     c                   Eval      §UTEistmo = 'DI'
004000160610     c                   when      %Subst(uteute:1:3) = 'REA'
004100160610     c                   Eval      §UTEistmo = 'RA'
004200160610     c                   when      %Subst(uteute:1:2) = 'RP'
004300160610     c                   Eval      §UTEistmo = 'PO'
004400160610
004500160610      * solo se non già impostato
004600160610     c                   other
004700160610     c                   Eval      §UTEistmo = 'NO'
004800160610     c                   endsl
004900151020
005000101019     c                   Movel     Dute01        UteFaf
005100151020
005200101019     c                   Update    Azute000
005300151020
005400070111     c                   endif
005500041011
005600041011     c                   EndDo
