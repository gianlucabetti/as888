000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161026      * Il pgm aggiorna i record valorizzati con le competenze
000400060125      *
000500060125      *****************************************************************
000600161026     Ffifss01l  IF   E           K DISK
000700161026     Fbitvl00f  uf   E             DISK
000800030429     F*---------------------------------------------------------------------------------
000900000607     D kpjba         e ds
001000161124     D kpjbas          s                   like(kpjba)
001100060125     d dataoggi        s               d   datfmt(*eur)
001200060125     d data_gg1        s               d   datfmt(*iso)
001300020205     c**********************************************************************
001400030429      *
001500161124     c                   movel     'BI04VAL'     kpjbu
001600161026     c     *start        setll     bitvl00f
001700161026     c                   do        *hival
001800161026     c                   read      bitvl00f
001900161026     c                   if        %eof(bitvl00f)
002000161026     c                   leave
002100161026     c                   endif
002200161026     c     kfss          chain     fifss01l
002300161026     c                   if        %found(fifss01l)
002400161026     c                   eval      tvlice   = fss001b + fss005b
002500161026     c                   eval      tvlicea  = fss035b + fss016b + fss019b +
002600161026     c                             fss034b + fssxx1b + fssxx2b
002700161026     c                   update    bitvl000
002800161026     c                   endif
002900161026     c                   enddo
003000161124     c                   eval      kpjbas = kpjba
003100161124     c                   call      'TRULQLKR'
003200161124     c                   parm                    kpjbas
003300161124     c                   clear                   kpjbu
003400000607     c                   seton                                        lr
003500020205     C**-------------------------------------------------------------**
003600000607     c     *inzsr        begsr
003700060125     C**-------------------------------------------------------------**
003800000607     c     *entry        plist
003900020205     c                   parm                    kpjba
004000030429      *----------*
004100030429     C* GIRO DATA ODIERNA
004200030429     C                   TIME                    W0140            14 0
004300030429     C                   MOVE      W0140         UDATE8            8 0
004400160126     C                   MOVEl     W0140         oracor            6 0
004500060125     C     *eur          movel     UDATE8        dataoggi
004600160201     c                   move      dataoggi      data_gg1
004700160201     c                   move      data_gg1      oggi              8 0
004800060125     C     dataoggi      subdur    1:*d          data_gg1
004900160201     c                   move      data_gg1      ieri              8 0
005000161025      *
005100161026     c     kfss          klist
005200161026     c                   kFLD                    tvlFGS
005300161026     c                   kFLD                    tvlDfv
005400161026     c                   kFLD                    tvlPDR
005500020402     c*
005600020402     c                   endsr
