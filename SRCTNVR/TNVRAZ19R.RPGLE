000100151020     /*END
000200151020      *-----------------------------------------------------------------*
000300170914      *             Aut. Certificazioni                                 *
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
003601170912      *autisti mezzi
003602170912     c                   Eval      §UTECERG  = 'NO'
003603170912     c                   Eval      §UTECERGA = ' '
003604170912     c                   Eval      §UTECERGM = ' '
003605170912      *società
003606170912     c                   Eval      §UTECERS  = 'NO'
003607170912     c                   Eval      §UTECERSL = ' '
003608170913      *stampe se filiale abilito come standard la filiale per la sede tutto
003609170913     c                   if        PRFnsi = 'FILTRA201 '  or
003610170913     c                             PRFnsi = 'FILTRAPFP '
003611170913     c                   Eval      §UTECERV  = 'PO'
003612170913     c                   else
003613170913     c                   Eval      §UTECERV  = 'AZ'
003614170913     c                   endif
003615171030      * impostazioni generiche per interrogazione tutti vedono tutto al momento
003616171030     c                   Eval      §UTECERVI = 'AZ'
003617171030     c                   Eval      §UTECERSV = 'AZ'
003618170913     c*
003700160610     c                   select
003701170912      * Profili EDP
003800170912     c                   when      %Subst(uteute:1:3) = 'EDP'
003801170912      *autisti mezzi
003900170912     c                   Eval      §UTECERG  = 'AZ'
003901170912     c                   Eval      §UTECERGA = '2'
003902170912     c                   Eval      §UTECERGM = '2'
003903170912      *società
003904170912     c                   Eval      §UTECERS  = 'AZ'
003905170912     c                   Eval      §UTECERSL = '2'
003906170912      *stampe
003907170912     c                   Eval      §UTECERV  = 'AZ'
003908170912      *
003909170912      * Profili EDP
003910170912     c                   when      %Subst(uteute:1:3) = 'COC'
003911170912      *autisti mezzi
003912170912     c                   Eval      §UTECERG  = 'AZ'
003913170912     c                   Eval      §UTECERGA = '2'
003914170912     c                   Eval      §UTECERGM = '2'
003915170912      *società
003916170912     c                   Eval      §UTECERS  = 'AZ'
003917170912     c                   Eval      §UTECERSL = '2'
003918170912      *stampe
003919170912     c                   Eval      §UTECERV  = 'AZ'
003920170912      * Profili AUTOMEZZI
003921171010     c                   when      %Subst(uteute:1:3) = 'AUT' or
003922171010     c                             %Subst(uteute:1:3) = 'OFB'
003923170912      *autisti mezzi
003924170912     c                   Eval      §UTECERG  = 'AZ'
003925171030     c                   Eval      §UTECERGA = *blank
003926170912     c                   Eval      §UTECERGM = '2'
003927170912      *società
003928171030     c                   Eval      §UTECERS  = 'NO'
003929171030     c                   Eval      §UTECERSL = ' '
003930170912      *stampe
003931170912     c                   Eval      §UTECERV  = 'AZ'
003932170912      *
003933170913      * Profili generici di sede
003934170914     c                   when      %Subst(uteute:1:3) = 'DLO' or
003935170914     c                             %Subst(uteute:1:3) = 'ACQ' or
003936170914     c                             %Subst(uteute:1:3) = 'QUA' or
003937170915     c                             %Subst(uteute:1:3) = 'LEG' or
003938170914     c                             %Subst(uteute:1:3) = 'GEN' or
003939170914     c                             %Subst(uteute:1:3) = 'RIC' or
003940170914     c                             %Subst(uteute:1:3) = 'ASS' or
003941170914     c                             %Subst(uteute:1:3) = 'SIC' or
003942170914     c                             %Subst(uteute:1:3) = 'SEG' or
003943170914     c                             %Subst(uteute:1:3) = 'CDG' or
003944170914     c                             %Subst(uteute:1:3) = 'FOR' or
003945170914     c                             %Subst(uteute:1:3) = 'TLC'
003946170914      *autisti mezzi
003947171030     c                   Eval      §UTECERG  = 'NO'
003948171030     c                   Eval      §UTECERGA = ' '
003949171030     c                   Eval      §UTECERGM = ' '
003950170914      *società
003951171030     c                   Eval      §UTECERS  = 'NO'
003952171030     c                   Eval      §UTECERSL = ' '
003953170914      *stampe
003954170914     c                   Eval      §UTECERV  = 'AZ'
003955170912      *
003956170912     c                   when      %Subst(uteute:1:3) = 'RED'
003957170912      *autisti mezzi
003958170912     c                   Eval      §UTECERG  = 'DI'
003959171030     c                   Eval      §UTECERGA = ' '
003960170912     c                   Eval      §UTECERGM = '3'
003961170912      *società
003962170912     c                   Eval      §UTECERS  = 'DI'
003963170912     c                   Eval      §UTECERSL = '3'
003964170912      *stampe
003965170912     c                   Eval      §UTECERV  = 'DI'
003966170912      *
004000160610     c                   when      %Subst(uteute:1:3) = 'REA'
004001170912      *autisti mezzi
004002170912     c                   Eval      §UTECERG  = 'RA'
004003171030     c                   Eval      §UTECERGA = ' '
004004170912     c                   Eval      §UTECERGM = '3'
004005170912      *società
004006170912     c                   Eval      §UTECERS  = 'RA'
004007170912     c                   Eval      §UTECERSL = '3'
004008170912      *stampe
004009170912     c                   Eval      §UTECERV  = 'RA'
004010170912      *
004011170914     c                   when      %Subst(uteute:1:3) = 'RF'
004012170914      *autisti mezzi
004013171030     c                   Eval      §UTECERG  = 'NO'
004014171030     c                   Eval      §UTECERGA = ' '
004015171030     c                   Eval      §UTECERGM = ' '
004016170914      *società
004017171030     c                   Eval      §UTECERS  = 'NO'
004018171030     c                   Eval      §UTECERSL = ' '
004019170914      *stampe
004020170914     c                   Eval      §UTECERV  = 'PO'
004021170914      *
004022170913     c                   when      %Subst(uteute:1:3) = 'POC' or
004023170913     c                             %Subst(uteute:1:3) = 'COA'
004024170912      *autisti mezzi
004025170912     c                   Eval      §UTECERG  = 'TP'
004026171030     c                   Eval      §UTECERGA = ' '
004027170912     c                   Eval      §UTECERGM = '3'
004028170912      *società
004029170912     c                   Eval      §UTECERS  = 'TP'
004030170912     c                   Eval      §UTECERSL = '3'
004031170912      *stampe
004032170912     c                   Eval      §UTECERV  = 'TP'
004033170912      *
004800160610     c                   endsl
004900151020
005000101019     c                   Movel     Dute01        UteFaf
005100151020
005200101019     c                   Update    Azute000
005500041011
005600041011     c                   EndDo
