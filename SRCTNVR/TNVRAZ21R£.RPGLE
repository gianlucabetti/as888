000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300141201      *             Aut. Bolle filiale e ORM                            *
000400041011      *-----------------------------------------------------------------*
000500070111     fKpprf01l  if   e           K Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011
000800041011     d dute01        e ds
000900041011
001000151014     c*//  *entry        plist
001100151014     c*//                parm                    wFIL             15 5
001200150204      *
001300150204     c                   Eval      *InLr = *On
001400150204      *
001500151014     c*//                if        %parms() = *zero   or
001600151014     c*//                          wFIL < 1  or  wFIL >= 500
001700151014     c*//                return
001800151014     c*//                endif
001900150204      *
002000070111     c     *loval        setll     Kpprf01l
002100041011     c                   Do        *Hival
002200070111     c                   Read      Kpprf01l
002300041011      * fine file
002400070111     c                   If        %Eof(Kpprf01l)
002500041011     c                   Leave
002600041011     c                   EndIf
002700151014      *//* Utenti di filiale
002800151014      * Utenti di filiale di 2° livello
002900151014     c*//                if        prfnsi = 'FILTRA201 '
003000151014     c                   if        %subst(PRFnsi:1:6) = 'FILTRA'
003100151014     c                             and  ( %subst(PRFnmu:1:3) = 'UTE'
003200151014     c                              or    %subst(PRFnmu:1:3) = 'RP2' )
003300041011
003400041011     c     PrfNmu        Chain     Azute01l
003500041011     c                   If        Not %Found(Azute01l)
003600041011     c                   Iter
003700041011     c                   EndIf
003800140605
003900151014      *//* solo utenti della filiale ricevuta come parametro
004000151014     c*//                IF        UTEfil <> wFIL
004100151014     c*//                iter
004200151014     c*//                ENDIF
004300140605
004400041011      * scaduto non aggiorno
004500041011     c                   If        UteDsc <= *date
004600041011     c                   Iter
004700041011     c                   EndIf
004800041011
004900041011     c                   Movel     UteFaf        Dute01
005000041011
005100140605      * solo se non già impostato
005200140605     c                   IF        §UTEmbf = *blanks
005300140605     c                   Eval      §UTEmbf = 'RA'
005400140605     c                   ENDIF
005500140605     c                   IF        §UTEorm = *blanks
005600140605     c                   Eval      §UTEorm = 'RA'
005700140605     c                   ENDIF
005800101019     c                   Movel     Dute01        UteFaf
005900101019     c                   Update    Azute000
006000070111     c                   endif
006100041011
006200041011     c                   EndDo
