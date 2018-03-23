000100050201     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200050201
000300050427     fFndct01l  if   e           k Disk
000400050427     fFndcf01l  if   e           K Disk
000500050610     fqsysprt   o    f  132        printer oflind(*inof)
000600050427
000700050610     d kdctaac         s                   like(dctaac) inz(2005)
000800050613     d totp            s              5  0 inz
000900050613     d tott            s              5  0 inz
001000050610     d wdtfase         s                   like(dcfdfc)
001100050610     d wfase           s                   like(dcffca)
001200050201
001300050201      *------------------------------------------------------------------------*
001400050201
001500050427     c     kfndct        klist
001600050427     c                   kfld                    dctaac
001700050427     c                   kfld                    dctfil
001800050427     c                   kfld                    dctnca
001900050310
002000050610do  1c     kdctaac       Setll     Fndct01l
002100050427      * Leggo le ca
002200050610do  1c                   Do        *Hival
002300050610     c     kdctaac       Reade     Fndct01l
002400050310      * Fine file
002500050427     c                   If        %Eof(Fndct01l)
002600050310     c                   Leave
002700050310     c                   EndIf
002800050310
002900050610      * solo quelle chiuse con cau 03
003000050610     c                   If        dctdch = *Zeros
003100050610     c                   Iter
003200050427     c                   EndIf
003300050610     c                   If        dctcch <> '03'
003400050610     c                   Iter
003500050610     c                   EndIf
003600050427
003700050610      * cerco la penultima fase
003800050427     c     kfndct        Setgt     fndcf01l
003900050610     c                   do        2
004000050427     c     kfndct        Readpe    fndcf01l
004100050610     c                   Eval      wdtfase = dcfdfc
004200050610     c                   Eval      wfase = dcffca
004300050610     c                   enddo
004400050427
004500050610     c                   If        not *In99 or *Inof
004600050610     c                   except    testa
004700050610     c                   eval      *in99 = *On
004800050610     c                   EndIf
004900050613
005000050613      * se pratica stampo e conteggio se penultima fase >= 545
005100050613     c                   If        dctfpr = 'P' and wfase >= 545
005200050613     c                   add       1             totp
005300050610     c                   except    riga
005400050610     c                   EndIf
005500050613
005600050613      * se transattiva stampo e conteggio se penultima fase >= 215
005700050613     c                   If        dctfpr = 'T' and wfase >= 215
005800050613     c                   add       1             tott
005900050613     c                   except    riga
006000050613     c                   EndIf
006100050310
006200050310     c                   EndDo
006300050613
006400050613     c                   except    totale
006500050310
006600050310     c                   Eval      *InLr = *On
006700050610
006800050610     oqsysprt   e            Testa            01
006900050610     o                                           46 'CA 2005 chiuse con causale'
007000050610     o                                        +   1 'PAREGGIO - 03'
007100050610     o                       *date         y     80
007200050610     o                                           95 'Pag.'
007300050610     o                       Page          z    100
007400050610     o          e            Testa          2 03
007500050610     o                                           16 'C.A.'
007600050610     o                                           37 'Data chiusura'
007700050610     o                                           45 'Cau'
007800050610     o                                           65 'Fase'
007900050610     o                                           80 'Data fase'
008000050610     o          e            Riga           1
008100050610     o                       dctaac        z      4
008200050610     o                       dctfil           +   1
008300050610     o                       dctnca        z  +   1
008400050610     o                       dctdch              37 '    /  /  '
008500050610     o                       dctcch              45
008600050610     o                       wfase               65
008700050610     o                       wdtfase             80 '    /  /  '
008800050613     o          e            Totale         2
008900050613     o                                           15 'C.A. Pratica'
009000050613     o                       totp          z  +   1
009100050613     o                                        +   1 'C.A. Transattiva'
009200050613     o                       tott          z  +   1
