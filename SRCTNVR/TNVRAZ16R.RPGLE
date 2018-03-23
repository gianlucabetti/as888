000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300160926      *  aggiorna abilitazione addebiti AZ > AL RA > RL                 *
000400160926      * ampliamento con anche filiali logistiche
000500041011      *-----------------------------------------------------------------*
000600070111     fKpprf01l  if   e           K Disk
000700041011     fAzute01l  uf   e           k Disk
000800041011
000900041011     d dute01        e ds
001000041011
001100150204      *
001200150204     c                   Eval      *InLr = *On
001300150204      *
001400070111     c     *loval        setll     Kpprf01l
001500041011     c                   Do        *Hival
001600070111     c                   Read      Kpprf01l
001700041011      * fine file
001800070111     c                   If        %Eof(Kpprf01l)
001900041011     c                   Leave
002000041011     c                   EndIf
002100140605      * Utenti di filiale
002200041011
002300041011     c     PrfNmu        Chain     Azute01l
002400041011     c                   If        Not %Found(Azute01l)
002500041011     c                   Iter
002600041011     c                   EndIf
002700140605
002800041011      * scaduto non aggiorno
002900041011     c                   If        UteDsc <= *date
003000041011     c                   Iter
003100041011     c                   EndIf
003200041011
003300041011     c                   Movel     UteFaf        Dute01
003400151106     c                   select
003500160926     c                   when      助TEadd = 'AZ'
003600160926     c                   Eval      助TEadd = 'AL'
003700160926     c                   when      助TEadd = 'RA'
003800160926     c                   Eval      助TEadd = 'RL'
003900160926     c                   other
004000160926     c                   iter
004100151106     c                   endsl
004200151106      *
004300101019     c                   Movel     Dute01        UteFaf
004400101019     c                   Update    Azute000
004500041011
004600041011     c                   EndDo
