000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161026      * Il pgm aggiorna i record valorizzati con le competenze
000400060125      *
000500060125      *****************************************************************
000600161026     Ffifss01l  IF   E           K DISK
000700161026     Fbitvl00f  uf   E             DISK
000800030429     F*---------------------------------------------------------------------------------
000900000607     D kpjba         e ds
001000060125     d dataoggi        s               d   datfmt(*eur)
001100060125     d data_gg1        s               d   datfmt(*iso)
001200020205     c**********************************************************************
001300030429      *
001400161026     c     *start        setll     bitvl00f
001500161026     c                   do        *hival
001600161026     c                   read      bitvl00f
001700161026     c                   if        %eof(bitvl00f)
001800161026     c                   leave
001900161026     c                   endif
002000161026     c     kfss          chain     fifss01l
002100161026     c                   if        %found(fifss01l)
002200161026     c                   eval      tvlice   = fss001b + fss005b
002300161026     c                   eval      tvlicea  = fss035b + fss016b + fss019b +
002400161026     c                             fss034b + fssxx1b + fssxx2b
002500161026     c                   update    bitvl000
002600161026     c                   endif
002700161026     c                   enddo
002800000607     c                   seton                                        lr
002900020205     C**-------------------------------------------------------------**
003000000607     c     *inzsr        begsr
003100060125     C**-------------------------------------------------------------**
003200000607     c     *entry        plist
003300020205     c                   parm                    kpjba
003400030429      *----------*
003500030429     C* GIRO DATA ODIERNA
003600030429     C                   TIME                    W0140            14 0
003700030429     C                   MOVE      W0140         UDATE8            8 0
003800160126     C                   MOVEl     W0140         oracor            6 0
003900060125     C     *eur          movel     UDATE8        dataoggi
004000160201     c                   move      dataoggi      data_gg1
004100160201     c                   move      data_gg1      oggi              8 0
004200060125     C     dataoggi      subdur    1:*d          data_gg1
004300160201     c                   move      data_gg1      ieri              8 0
004400161025      *
004500161026     c     kfss          klist
004600161026     c                   kFLD                    tvlFGS
004700161026     c                   kFLD                    tvlDfv
004800161026     c                   kFLD                    tvlPDR
004900020402     c*
005000020402     c                   endsr
