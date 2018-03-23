000100120320     H DEBUG DECEDIT('0,') DATEDIT(*ymd/) option(*nodebugio)
000200120314      *
000300120601     ffiadt03l  uf a e           k disk
000400120601     ffiadd01l  uf a e           k disk
000500050118
000600120601     d paralleg        ds
000700120601     d  paroldnrc                     7s 0
000800120601     d  parolddat                     8s 0
000900120601     d  parnewnrc                     7s 0
001000120601     d  parnewdat                     8s 0
001100050114
001200000607      *--------------------------------------------------------------------
001300050114
001400120314     c     *entry        plist
001500120601     c                   parm                    paralleg
001600120601     c     kadd          klist
001700120601     c                   kfld                    adtpdr
001800120601     c                   kfld                    prgdett           3 0
001900120314
002000120601     c                   z-add     paroldnrc     contratto         7 0
002100120314      * reperimento dati file
002200050114      *
002300120601     c     contratto     setll     fiadt03l
002400120601     c                   do        *hival
002500120601     c     contratto     reade     fiadt03l
002600120601     c                   if        %eof(fiadt03l)
002700000607     c                   leave
002800000607     c                   end
002900120601      * se scaduto non aggiorna
003000120601     c                   if        adtdst < parolddat
003100120601     c                   iter
003200120601     c                   end
003300120601      *  aggiorna testata vecchia
003400120601     c                   z-add     parolddat     adtdst
003500120601     c                   z-add     adtprg        prgdett
003600120601     c                   update    fiadt000
003700120601      *  prepara testata nuova decorrenza
003800120601     c                   clear                   adtdcn
003900120601     c                   clear                   adtduv
004000120601     c                   clear                   adtdrc
004100120601     c                   clear                   adtunodc
004200120601     c                   clear                   adtdfa
004300120605     c                   clear                   adtnrc
004400120605     c                   clear                   adtfil
004500141001     c                   z-add     parnewnrc     adtnrc
004600120601     c                   z-add     parnewdat     adtddt
004700120601     c                   z-add     parnewdat     adtdts
004800120605     c                   z-add     20391231      adtdst
004900120601     c                   eval      adtprg = adtprg + 1
005000120601     c                   write     fiadt000
005100120601     c                   exsr      dettaglio
005200120314      *
005300040507     c                   enddo
005400120314     c                   return
005500120601     c**********************************************************************
005600120601     C     dettaglio     BEGSR
005700120601     c**********************************************************************
005800120601      *
005900120601     c     kadd          setll     fiadd01l
006000120601     c                   do        *hival
006100120601     c     kadd          reade     fiadd01l
006200120601     c                   if        %eof(fiadd01l)
006300120601     c                   leave
006400120601     c                   endif
006500120601     c                   eval      addprg = adtprg
006600120601     c                   write     fiadd000
006700120601     c                   enddo
006800120601     c
006900120601     C                   endsr
