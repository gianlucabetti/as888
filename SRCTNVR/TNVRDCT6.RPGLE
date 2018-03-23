000100050201     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200050201
000300070105     ffndct01l  if   e           k disk
000400070105     ffndra01l  if   e           K disk
000500070105     fqsysprt   o    f  132        printer oflind(*inof)
000600050427
000700070105     d kdctaac         s                   like(dctaac) inz(2005)
000800070105
000900070105     d ddct01        e ds
001000050201
001100050201      *------------------------------------------------------------------------*
001200050201
001300050427     c     kfndct        klist
001400050427     c                   kfld                    dctaac
001500050427     c                   kfld                    dctfil
001600050427     c                   kfld                    dctnca
001700050310
001800070105      * Leggo le c.a.
001900070105     c     kdctaac       setll     fndct01l
002000070105do  1c                   do        *hival
002100070105     c     kdctaac       reade     fndct01l
002200050310      * Fine file
002300070105     c                   if        %eof(fndct01l)
002400070105     c                   leave
002500070105     c                   endif
002600050427
002700070105     c                   eval      ddct01 = dctflo
002800070105
002900070105      * solo le c.a. con recupero A.I.G.
003000070105     c                   if        §dctaffi <> 'A'
003100070105     c                   iter
003200070105     c                   endif
003300050427
003400070105      * cerco i dati relativi all'A.I.G.
003500070105     c     kfndct        chain     fndra01l
003600070105     c                   if        %found(fndra01l)
003700070105     c                   if        dradvr = 0 or drairi = 0
003800070105
003900070105     c                   if        not *in99 or *inof
004000070105     c                   except    testa
004100070105     c                   eval      *in99 = *on
004200070105     c                   endif
004300050427
004400070105     c                   except    riga
004500070105     c                   endif
004600070105     c                   endif
004700050310
004800070105     c                   enddo
004900050310
005000070105     c                   eval      *inlr = *on
005100070105
005200070105     oqsysprt   e            testa            01
005300070105     o                                           46 'CA con dati A.I.G. non'
005400070105     o                                        +   1 'completi'
005500070105     o                       *date         y     80
005600070105     o                                           95 'Pag.'
005700070105     o                       page          z    100
005800070105     o          e            testa          2 03
005900070105     o                                           16 'C.A.'
006000070105     o                                           37 'Data chiusura'
006100070105     o                                           55 'Data richiesta'
006200070105     o                                           70 'Data ricezione'
006300070105     o                                           90 'Importo recuperato'
006400070105     o          e            riga           1
006500070105     o                       dctaac        z      4
006600070105     o                       dctfil           +   1
006700070105     o                       dctnca        z  +   1
006800070105     o                       dctdch              37 '    /  /  '
006900070105     o                       dradrr              55 '    /  /  '
007000070105     o                       dradvr              70 '    /  /  '
007100070105     o                       drairi        4     90
007200070105     o                       dradir              95
