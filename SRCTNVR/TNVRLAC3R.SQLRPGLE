000100940224     H*--------------------------------------------------------------*
000200100421     FTNTBE01L  uF   E           K DISK
000300161025     Fqsysprt   O    F  132        PRINTER
000400100421      *---------------------------------------------------------------*
000500161024     d dlac          e ds
000600161025     d wKsc            s              7a
000700100408      *---------------------------------------------------------------*
000800161024     c                   eval      tbeCOD = 'LAC'
000900161024      *
001000161024     c                   except    testata
001100100421      *
001200100416     C     TbeCOD        setll     TnTbe01l
001300100421      *
001400161024     c                   dou       %EoF(TnTbe01l)
001500161024     c     TbeCOD        reade     TnTbe01l
001600161024     c                   if        not %eof()
001700100416      *
001800161024     c                   movel     tbeUNI        DLAC
001900161024      *
002000161024      * inizializzo i campi §LACFID / CPY / DST (FID dovrebbe essere già stato fatto)
002100161024     c***                clear                   §LACFiD
002200161024     c                   clear                   §LACCpy
002300161024     c                   clear                   §LACDst
002400161025      *
002500161025      * cliente che riceve CD/DVD
002600161025     c                   if        §LACAUT='S'
002700161025      * inizializzo §LACCPY a 1
002800161025     c                   eval      §LACCpy = 1
002900161025     c                   endif
003000161025      *
003100161025      * sia che il cliente riceva il DVD che non, recepisco il valore dei campi nuovi dalle note
003200161025      * e poi epuro le note
003300161024      *
003400161024      * valorizzo §LACCPY, aggiorno §LACNOTE
003500161024      * se nelle note è indicato 'x COPIE', x è il valore da mettere su §LACCPY
003600161024     c                   if        %scan(' COPIE' : §LACNote) <> 0
003700161024     c                   monitor
003800161024     c                   eval      §LACCpy = %dec(%subst(§LACNote :
003900161024     c                              %scan(' COPIE' : §LACNote) - 1 : 1)
004000161024     c                              : 1 : 0)
004100161024     c* il dato reperito non è numerico
004200161024     c                   on-error
004300161025     c                   eval      wKsc = TBEKE1
004400161024     c                   except    riga01
004500161024     c                   iter
004600161024     c                   endmon
004700161024      * pulisco le note di quanto messo in §LACCpy
004800161024     c                   eval      %subst(§LACNote :
004900161024     c                              %scan(' COPIE' : §LACNote) - 1 : 7)
005000161024     c                             = *blank
005100161024     c                   eval      §LACNote = %trim(§LACNote)
005200161024     c                   endif
005300161025      *
005400161024     c                   eval      tbeUNI  =  DLAC
005500100416      *   aggiorna sempre
005600100421     c                   update    tntbe000
005700161024     c                   endif
005800161024     c                   enddo
005900100416      *
006000000000     C                   SETON                                        LR
006100161024      *---------------------------------------------------------------*
006200161024     Oqsysprt   E            testata        1 02
006300161024     o                                           10 'TNVRLAC3R'
006400161024     Oqsysprt   E            riga01      1
006500161025     o                                           14 'KE1: '
006600161025     O                       wKSC               + 1
006700161025     O                                          + 2 'NOTA: '
006800161024     O                       §LACNote           + 1
006900161025     O                                          + 2 'nr.copie non '
007000161024     O                                          + 1 'numerico'
007100161024      **
