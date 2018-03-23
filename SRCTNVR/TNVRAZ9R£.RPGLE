000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300141201      *             Aut. Bolle filiale e ORM                            *
000400041011      *-----------------------------------------------------------------*
000500070111     fKpprf01l  if   e           K Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011
000800041011     d dute01        e ds
000900041011
001000150204     c     *entry        plist
001100150204     c                   parm                    wFIL             15 5
001200150204      *
001300150204     c                   Eval      *InLr = *On
001400150204      *
001500150204     c                   if        %parms() = *zero   or
001600150204     c                             wFIL < 1  or  wFIL >= 500
001700150204     c                   return
001800150204     c                   endif
001900150204      *
002000070111     c     *loval        setll     Kpprf01l
002100041011     c                   Do        *Hival
002200070111     c                   Read      Kpprf01l
002300041011      * fine file
002400070111     c                   If        %Eof(Kpprf01l)
002500041011     c                   Leave
002600041011     c                   EndIf
002700140605      * Utenti di filiale
002800140605     c                   if        prfnsi = 'FILTRA201 '
002900041011
003000041011     c     PrfNmu        Chain     Azute01l
003100041011     c                   If        Not %Found(Azute01l)
003200041011     c                   Iter
003300041011     c                   EndIf
003400140605
003500150204      * solo utenti della filiale ricevuta come parametro
003600150204     c                   IF        UTEfil <> wFIL
003700140605     c                   iter
003800140605     c                   ENDIF
003900140605
004000041011      * scaduto non aggiorno
004100041011     c                   If        UteDsc <= *date
004200041011     c                   Iter
004300041011     c                   EndIf
004400041011
004500041011     c                   Movel     UteFaf        Dute01
004600041011
004700140605      * solo se non già impostato
004800140605     c                   IF        §UTEmbf = *blanks
004900140605     c                   Eval      §UTEmbf = 'RA'
005000140605     c                   ENDIF
005100140605     c                   IF        §UTEorm = *blanks
005200140605     c                   Eval      §UTEorm = 'RA'
005300140605     c                   ENDIF
005400101019     c                   Movel     Dute01        UteFaf
005500101019     c                   Update    Azute000
005600070111     c                   endif
005700041011
005800041011     c                   EndDo
