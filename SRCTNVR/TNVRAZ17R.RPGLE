000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300160224      *             Aut. Bolle filiale e ORM e RA                       *
000400041011      *-----------------------------------------------------------------*
000500070111     fKpprf01l  if   e           K Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011
000800041011     d dute01        e ds
000900041011
001000070111     c     *loval        setll     Kpprf01l
001100041011     c                   Do        *Hival
001200070111     c                   Read      Kpprf01l
001300041011      * fine file
001400070111     c                   If        %Eof(Kpprf01l)
001500041011     c                   Leave
001600041011     c                   EndIf
001700140605      * Utenti di filiale
001800160224     c                   if        prfnsi = 'FILTRA201 ' or
001801160224     c                             PRFnsi = 'FILTRAPFP '
001900041011
002000041011     c     PrfNmu        Chain     Azute01l
002100041011     c                   If        Not %Found(Azute01l)
002200041011     c                   Iter
002300041011     c                   EndIf
002400140605
002500160224      * solo utenti della filiale 067
002600160226     c                   IF        UTEfil =  067
002700160226     c                   iter
002800160226     c                   ENDIF
002900140605
003000041011      * scaduto non aggiorno
003100041011     c                   If        UteDsc <= *date
003200041011     c                   Iter
003300041011     c                   EndIf
003400041011
003500041011     c                   Movel     UteFaf        Dute01
003600041011
003700160224      * aggiorno sempre
003900160224     c                   Eval      §UTEmbf = 'RC'
004200160224     c                   Eval      §UTEorm = 'RC'
004201160224     c                   Eval      §UTErac = 'RC'
004400101019     c                   Movel     Dute01        UteFaf
004500101019     c                   Update    Azute000
004600070111     c                   endif
004700041011
004800041011     c                   EndDo
004900041011
005000041011     c                   Eval      *InLr = *On
