000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300150210      *             gestione bancali                                    *
000400041011      *-----------------------------------------------------------------*
000500070111     fKpprf01l  if   e           K Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011
000800041011     d dute01        e ds
000900041011
001000150204      *
001100150204     c                   Eval      *InLr = *On
001200150204      *
001300070111     c     *loval        setll     Kpprf01l
001400041011     c                   Do        *Hival
001500070111     c                   Read      Kpprf01l
001600041011      * fine file
001700070111     c                   If        %Eof(Kpprf01l)
001800041011     c                   Leave
001900041011     c                   EndIf
002000140605      * Utenti di filiale
002100041011
002200041011     c     PrfNmu        Chain     Azute01l
002300041011     c                   If        Not %Found(Azute01l)
002400041011     c                   Iter
002500041011     c                   EndIf
002600140605
002700041011      * scaduto non aggiorno
002800041011     c                   If        UteDsc <= *date
002900041011     c                   Iter
003000041011     c                   EndIf
003100041011
003200041011     c                   Movel     UteFaf        Dute01
003201151106     c                   select
003202151106     c                   when      %Subst(uteute:1:3) = 'CDG'
003208151106     c                   Eval      助TEadd = 'AZ'
003209151106     c                   Eval      助TEaddt = 'G'
003210151106     c                   when      %Subst(uteute:1:3) = 'RED'
003211151106     c                   Eval      助TEadd = 'DI'
003212151106     c                   Eval      助TEaddt = 'G'
003213151106     c                   when      %Subst(uteute:1:3) = 'REA'
003214151106     c                   Eval      助TEadd = 'RA'
003215151106     c                   Eval      助TEaddt = 'G'
003216151106     c                   when      %Subst(uteute:1:2) = 'RP'
003217151106     c                   Eval      助TEadd = 'PO'
003218151106     c                   Eval      助TEaddt = 'G'
003300041011
003400140605      * solo se non gi� impostato
003401151106     c                   other
003600151028     c                   Eval      助TEadd = 'NO'
003601151028     c                   Eval      助TEaddt = *blank
003602151106     c                   endsl
003603151106      *
003800101019     c                   Movel     Dute01        UteFaf
003900101019     c                   Update    Azute000
004000041011
004100041011     c                   EndDo
