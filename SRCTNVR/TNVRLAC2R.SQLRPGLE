000100940224     H*--------------------------------------------------------------*
000200100421     FTNTBE01L  uF   E           K DISK
000300161025     Fqsysprt   O    F  132        PRINTER
000400100421      *---------------------------------------------------------------*
000500161024     d dlac          e ds
000600161025     d wKsc            s              7a
000700100408      *---------------------------------------------------------------*
000800161025       DCL-PR TNVRLAC2R extpgm   END-PR;
000900161025
001000161024     c                   eval      tbeCOD = 'LAC'
001100161024      *
001200161024     c                   except    testata
001300100421      *
001400100416     C     TbeCOD        setll     TnTbe01l
001500100421      *
001600161024     c                   dou       %EoF(TnTbe01l)
001700161024     c     TbeCOD        reade     TnTbe01l
001800161024     c                   if        not %eof()
001900100416      *
002000161024     c                   movel     tbeUNI        DLAC
002100161024      *
002200161024      * inizializzo i campi §LACFID / CPY / DST
002300161024     c                   clear                   §LACFiD
002400161024     c                   clear                   §LACCpy
002500161024     c                   clear                   §LACDst
002600161024      *
002700161025      * cliente che riceve CD/DVD
002800161025     c                   if        §LACAUT='S'
002900161025      * inizializzo §LACFID con i primi 3 char del cliente di raggruppamento
003000161025     c                   eval      §LACFiD = %dec(
003100161025     c                                       %subst(%editc(§LACKsU:'X') : 1 : 3)
003200161025     c                                       : 3 : 0)
003300161025     c                   endif
003400161025      *
003500161025      * sia che il cliente riceva il DVD che non, recepisco il valore dei campi nuovi dalle note
003600161025      * e poi epuro le note
003700161025      *
003800161025      * valorizzo §LACFID, aggiornare §LACNOTE
003900161025      * se nelle note è indicato 'INVIARE A POCxxx', xxx è il valore da mettere su §LACFID
004000161025     c                   if        %scan('INVIARE A POC' : §LACNote) <> 0
004100161024     c                   monitor
004200161024     c                   eval      §LACFiD = %dec(%subst(§LACNote :
004300161024     c                              %scan('INVIARE A POC' : §LACNote) + 13 : 3)
004400161024     c                              : 3 : 0)
004500161024     c* il dato reperito non è numerico
004600161024     c                   on-error
004700161025     c                   eval      wKsc = TBEKE1
004800161024     c                   except    riga01
004900161024     c                   iter
005000161024     c                   endmon
005100161024      * pulisco le note di quanto messo in §LACFid
005200161024     c                   eval      %subst(§LACNote :
005300161024     c                              %scan('INVIARE A POC' : §LACNote) : 16)
005400161024     c                             = *blank
005500161024     c                   eval      §LACNote = %trim(§LACNote)
005600161024     c                   endif
005700161025      *
005800161025      * valorizzo §LACFID, aggiornare §LACNOTE
005900161025      * se nelle note è indicato 'INVIARE POCxxx', xxx è il valore da mettere su §LACFID
006000161025     c                   if        %scan('INVIARE POC' : §LACNote) <> 0
006100161025     c                   monitor
006200161025     c                   eval      §LACFiD = %dec(%subst(§LACNote :
006300161025     c                              %scan('INVIARE POC' : §LACNote) + 11 : 3)
006400161025     c                              : 3 : 0)
006500161025     c* il dato reperito non è numerico
006600161025     c                   on-error
006700161025     c                   eval      wKsc = TBEKE1
006800161025     c                   except    riga01
006900161025     c                   iter
007000161025     c                   endmon
007100161025      * pulisco le note di quanto messo in §LACFid
007200161025     c                   eval      %subst(§LACNote :
007300161025     c                              %scan('INVIARE POC' : §LACNote) : 14)
007400161025     c                             = *blank
007500161025     c                   eval      §LACNote = %trim(§LACNote)
007600161025     c                   endif
007700161025      *
007800161024     c                   eval      tbeUNI  =  DLAC
007900100416      *   aggiorna sempre
008000100421     c                   update    tntbe000
008100161024     c                   endif
008200161024     c                   enddo
008300100416      *
008400000000     C                   SETON                                        LR
008500161024      *---------------------------------------------------------------*
008600161024     Oqsysprt   E            testata        1 02
008700161024     o                                           10 'TNVRLAC2R'
008800161024     Oqsysprt   E            riga01      1
008900161025     o                                           14 'KE1: '
009000161025     O                       wKSC               + 1
009100161025     O                                          + 2 'NOTA: '
009200161024     O                       §LACNote           + 1
009300161025     O                                          + 2 'filiale DVD non '
009400161025     O                                          + 1 'numerica'
009500161024      **
