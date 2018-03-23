000200120320     H DEBUG DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000201120314      *
000300120601     ffiadt03l  uf a e           k disk
000301120601     ffiadd01l  uf a e           k disk
000700050118
000701120601     d paralleg        ds
000702120601     d  paroldnrc                     7s 0
000703120601     d  parolddat                     8s 0
000704120601     d  parnewnrc                     7s 0
000705120601     d  parnewdat                     8s 0
001200050114
001900000607      *--------------------------------------------------------------------
002000050114
002001120314     c     *entry        plist
002002120601     c                   parm                    paralleg
002003120601     c     kadd          klist
002004120601     c                   kfld                    adtpdr
002005120601     c                   kfld                    prgdett           3 0
002006120314
002007120601     c                   z-add     paroldnrc     contratto         7 0
002100120314      * reperimento dati file
002200050114      *
002201120601     c     contratto     setll     fiadt03l
002202120601     c                   do        *hival
002500120601     c     contratto     reade     fiadt03l
002600120601     c                   if        %eof(fiadt03l)
002700000607     c                   leave
002800000607     c                   end
002801120601      * se scaduto non aggiorna
002802120601     c                   if        adtdst < parolddat
002803120601     c                   iter
002804120601     c                   end
002805120601      *  aggiorna testata vecchia
002806120601     c                   z-add     parolddat     adtdst
002807120601     c                   z-add     adtprg        prgdett
002808120601     c                   update    fiadt000
002809120601      *  prepara testata nuova decorrenza
002811120601     c                   clear                   adtdcn
002812120601     c                   clear                   adtduv
002813120601     c                   clear                   adtdrc
002814120601     c                   clear                   adtunodc
002815120601     c                   clear                   adtdfa
002816120605     c                   clear                   adtnrc
002817120605     c                   clear                   adtfil
002818120601     c                   z-add     parnewdat     adtddt
002819120601     c                   z-add     parnewdat     adtdts
002820120605     c                   z-add     20391231      adtdst
002821120601     c                   eval      adtprg = adtprg + 1
002822120601     c                   write     fiadt000
002823120601     c                   exsr      dettaglio
002824120314      *
003800040507     c                   enddo
003900120314     c                   return
004000120601     c**********************************************************************
004100120601     C     dettaglio     BEGSR
004200120601     c**********************************************************************
004300120601      *
004600120601     c     kadd          setll     fiadd01l
004601120601     c                   do        *hival
004602120601     c     kadd          reade     fiadd01l
004700120601     c                   if        %eof(fiadd01l)
004900120601     c                   leave
005000120601     c                   endif
005100120601     c                   eval      addprg = adtprg
005200120601     c                   write     fiadd000
005900120601     c                   enddo
006000120601     c
006100120601     C                   endsr
