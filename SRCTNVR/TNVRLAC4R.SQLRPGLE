000100940224     H*--------------------------------------------------------------*
000200100421     FTNTBE01L  uF   E           K DISK
000300161025     Fqsysprt   O    F  132        PRINTER
000400100421      *---------------------------------------------------------------*
000500161024     d dlac          e ds
000600100408      *---------------------------------------------------------------*
000700161024     c                   eval      tbeCOD = 'LAC'
000800161024      *
000900161025     c***                except    testata
001000100421      *
001100100416     C     TbeCOD        setll     TnTbe01l
001200100421      *
001300161024     c                   dou       %EoF(TnTbe01l)
001400161024     c     TbeCOD        reade     TnTbe01l
001500161024     c                   if        not %eof()
001600100416      *
001700161024     c                   movel     tbeUNI        DLAC
001800161024      *
001900161024      * inizializzo i campi §LACFID / CPY / DST (FID E CPY dovrebbero essere già stati fatti)
002000161024     c***                clear                   §LACFiD
002100161024     c***                clear                   §LACCpy
002200161024     c                   clear                   §LACDst
002300161024      *
002400161025      * valorizzo §LACDST, aggiornare §LACNOTE
002500161025      * se nelle note è indicato 'INVIARE A xxxxxxx', xxxxxxx è il valore da mettere su §LACDST
002600161024     c                   if        %scan('INVIARE A ' : §LACNote) <> 0
002700161025     c***                monitor
002800161025     c                   eval      §LACDst = %subst(§LACNote :
002900161025     c                              %scan('INVIARE A ' : §LACNote) + 10 : 7)
003000161024     c* il dato reperito non è numerico
003100161025     c***                on-error
003200161025     c***                except    riga01
003300161025     c***                iter
003400161025     c***                endmon
003500161025      * pulisco le note di quanto messo in §LACDst
003600161024     c                   eval      %subst(§LACNote :
003700161025     c                              %scan('INVIARE A ' : §LACNote) : 17)
003800161024     c                             = *blank
003900161024     c                   eval      §LACNote = %trim(§LACNote)
004000161024     c                   endif
004100161024      *
004200161024     c                   eval      tbeUNI  =  DLAC
004300100416      *   aggiorna sempre
004400100421     c                   update    tntbe000
004500161024     c                   endif
004600161024     c                   enddo
004700100416      *
004800000000     C                   SETON                                        LR
004900161024      *---------------------------------------------------------------*
005000161024     Oqsysprt   E            testata        1 02
005100161025     o                                           10 'TNVRLAC4R'
005200161024     Oqsysprt   E            riga01      1
005300161024      **
