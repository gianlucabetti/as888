000100940224     H*--------------------------------------------------------------*
000200100421     FTNTBE01L  uF   E           K DISK
000300161024     Fqsysprt   O    F  198        PRINTER
000400100421      *---------------------------------------------------------------*
000500161024     d dlac          e ds
000600100408      *---------------------------------------------------------------*
000700161024     c                   eval      tbeCOD = 'LAC'
000800161024      *
000900161024     c                   except    testata
001000100421      *
001100100416     C     TbeCOD        setll     TnTbe01l
001200100421      *
001300161024     c                   dou       %EoF(TnTbe01l)
001400161024     c     TbeCOD        reade     TnTbe01l
001500161024     c                   if        not %eof()
001600100416      *
001700161024     c                   movel     tbeUNI        DLAC
001800161024      *
001900161024      * inizializzo i campi §LACFID / CPY / DST
002000161024     c                   clear                   §LACFiD
002100161024     c                   clear                   §LACCpy
002200161024     c                   clear                   §LACDst
002300161024      *
002400161024      * valorizzo §LACFID, aggiornare §LACNOTE
002500161024      * se nelle note è indicato 'INVIARE A POCxxx', xxx è il valore da mettere su §LACFID
002600161024     c                   if        %scan('INVIARE A POC' : §LACNote) <> 0
002700161024     c                   monitor
002800161024     c                   eval      §LACFiD = %dec(%subst(§LACNote :
002900161024     c                              %scan('INVIARE A POC' : §LACNote) + 13 : 3)
003000161024     c                              : 3 : 0)
003100161024     c* il dato reperito non è numerico
003200161024     c                   on-error
003300161024     c                   except    riga01
003400161024     c                   iter
003500161024     c                   endmon
003600161024      * pulisco le note di quanto messo in §LACFid
003700161024     c                   eval      %subst(§LACNote :
003800161024     c                              %scan('INVIARE A POC' : §LACNote) : 16)
003900161024     c                             = *blank
004000161024     c                   eval      §LACNote = %trim(§LACNote)
004100161024     c                   endif
004200161024      *
004300161024     c                   eval      tbeUNI  =  DLAC
004400100416      *   aggiorna sempre
004500100421     c                   update    tntbe000
004600161024     c                   endif
004700161024     c                   enddo
004800100416      *
004900000000     C                   SETON                                        LR
005000161024      *---------------------------------------------------------------*
005100161024     Oqsysprt   E            testata        1 02
005200161024     o                                           10 'TNVRLAC2R'
005300161024     Oqsysprt   E            riga01      1
005400161024     o                                           26 'Il rcd di TNRBE con KE1: '
005500161024     O                       TBEKe1             + 1
005600161024     O                                          + 2 'e nota: '
005700161024     O                       §LACNote           + 1
005800161024     O                                          + 2 'ha la filiale di arrivo '
005900161024     O                                          + 1 'DVD non numerica.'
006000161024      **
