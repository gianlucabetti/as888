000100041011     h Decedit('0,') Datedit(*ymd.) option(*nodebugio)
000200041011      *-----------------------------------------------------------------*
000300110216      *             controlli autorizzaizoni sui profili                *
000400041011      *-----------------------------------------------------------------*
000500041011     fAzute00f  uf   e             Disk
000600110216     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700041011
000800041011     d dute01        e ds
000900041011
001000041011     c                   Do        *Hival
001100041011     c                   Read      Azute00f
001200041011      * fine file
001300041011     c                   If        %Eof(Azute00f)
001400041011     c                   Leave
001500041011     c                   EndIf
001600110216      * scaduto non consdiero
001700041011     c                   If        UteDsc <= *date
001800041011     c                   Iter
001900041011     c                   EndIf
002000041011
002100041011     c                   Movel     UteFaf        Dute01
002200110216     c                   if        §utembs='AR' or §uteiun='AR' or
002300110216     c                             §utecun='AR' or §uteibs='AR' or
002400110216     c                             §uteist='AR' or §utegtc='AR' or
002500110216     c                             §utectc='AR' or §utepot='AR' or
002600110216     c                             §utecli='AR' or §uteeca='AR' or
002700110216     c                             §utembf='AR' or §UTEPSWAUT='AR' or
002800110216     c                             §UTEPSWMAG='AR' or §UTEPSWR='AR' or
002900110216     c                             uteaut='AR'
003000110216     c                   except    stampa
003100041011
003200110216     c                   endif
003300041011
003400041011
003500041011     c                   EndDo
003600041011
003700041011     c                   Eval      *InLr = *On
003800110216     OQSYSPRT   E            stampa      1
003900110216     O                                           66 'Profilo con abilitazione'
004000110216     O                                        +   1 '"AR" :'
004100110216     O                       uteute           +   2
