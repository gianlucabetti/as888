000100151020     /*END
000200151020      *-----------------------------------------------------------------*
000300151020      *             Aut. Manutenzione R.A. clienti                      *
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
003700151020      * solo se non già impostata
003800151020     c                   IF        §UTErac = *blanks
003900151020     c                   Eval      §UTErac = 'RA'
004000140605     c                   ENDIF
004100151020
004200101019     c                   Movel     Dute01        UteFaf
004300151020
004400101019     c                   Update    Azute000
004500151020
004600070111     c                   endif
004700041011
004800041011     c                   EndDo
