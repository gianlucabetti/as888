000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300041011      *             Autorizzazioni tariffe                              *
000400041011      *-----------------------------------------------------------------*
000500041011     fKpprf00f  if   e             Disk
000600041011     fAzute01l  uf   e           k Disk
000700041011
000800041011     d dute01        e ds
000900041011
001000041011     c                   Do        *Hival
001100041011     c                   Read      Kpprf00f
001200041011      * fine file
001300041011     c                   If        %Eof(Kpprf00f)
001400041011     c                   Leave
001500041011     c                   EndIf
001600041011
001700041011     c     PrfNmu        Chain     Azute01l
001800041011     c                   If        Not %Found(Azute01l)
001900041011     c                   Iter
002000041011     c                   EndIf
002100041011      * scaduto non aggiorno
002200041011     c                   If        UteDsc <= *date
002300041011     c                   Iter
002400041011     c                   EndIf
002500041011
002600041011     c                   Movel     UteFaf        Dute01
002700041011
002800130807      * Imposto autorizzazioni per Richiesta Assistenza Clienti
002900130807     c                   IF        %subst(UTEute:1:3) = 'QUA' or
003000130807     c                             %subst(UTEute:1:3) = 'EDP' or
003100130807     c                             %subst(UTEute:1:3) = 'RIC'
003200130807     c                   eval      §UTEvrac = 'S'
003300130807     c                   ENDIF
003400130807
003500130807      * Imposto autorizzazioni per Danni
003600130807     c                   IF        %subst(UTEute:1:3) = 'ISP' or
003700130807     c                             %subst(UTEute:1:3) = 'EDP'
003800130807     c                   eval      §UTEmresca = 'S'
003900130807     c                   ENDIF
004000041011
004100041011     c                   Movel     Dute01        UteFaf
004200041011     c                   Update    Azute000
004300041011
004400041011     c                   EndDo
004500041011
004600041011     c                   Eval      *InLr = *On
