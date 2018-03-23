000100900418     H DECEDIT('0,') DATEDIT(*DMY.)
000200050704     H* Fieu49R *----------------------------------------------------*
000300900418     H*                                                              *
000400050704     H* Scrittura del fidpo00F                                       *
000500900418     H*--------------------------------------------------------------*
000600061005     Ffidpo01l  o    E             DISK
000700050926     D KPJBA         E DS
000800050704     D Fieu49ds      E DS
000900050704     D trul33ds      E DS
001000050704
001100050926     D numero          s                   like(dponor)
001200050704     D KPJBus          s                   like(kpjbu)
001300050926      **?__________________________________________________________________ */
001400050704      *    main
001500050926      **?__________________________________________________________________ */
001600050704      * richiamo in Write non devo trovare il rekord
001700050704     c     *entry        plist
001800050926     C                   parm                    fieu49ds
001900050712     c                   clear                   fidpo000
002000050926      * reperimento numero  orm export in scrittura
002100050704      * in immissione dell'orm da Bartolini verso DpD
002200050926     c                   exsr      numero_orm
002300050926     C* fisso
002400050704     C*
002500050926     C* rif. orm dpd
002600060810     c                   move      '0837'        DPOdep
002700050929     c                   z-add     numero        DPOord
002800050926     C* rif. orm bart.
002900050929     c                   z-add     iD49POE       DPOpoe
003000050929     c                   z-add     iD49NRS       DPOnrs
003100050929     c                   z-add     iD49NOR       DPOnor
003200050929     c                   z-add     iD49NRV       DPOnrv
003300050929     c                   z-add     iD49dar       DPOdtorm
003400061005     c                   z-add     10            dposts
003500050704     c                   write     fidpo000                             99
003600050929     c                   move      *in99         od49esito
003700050929     c                   if        not *in99
003900050926     C* rif. orm dpd in ds-out
004000060810     c                   move      DPODEP        oD49DEP
004100050929     c                   z-add     DPOORD        oD49ORD
004200050711     c                   endif
004300050704      *
004400050704     c                   seton                                        rt
004500050926      **?__________________________________________________________________ */
004600050926     c     numero_orm    begsr
004700050926      **?__________________________________________________________________ */
004800050704     c                   eval      kpjbus=kpjbu
004900050704     c                   clear                   trul33ds
005000050704     c                   move      'L'           i33tla
005100050704     c                   z-add     304           i33cnu
005200050704     c                   z-add     1             i33num
005300050704     c                   movel     trul33ds      kpjbu
005400050704     c                   call      'TRUL33R'
005500050704     c                   PARM                    kpjba
005600050704     c                   movel     kpjbu         trul33ds
005700050926     c                   z-add     o33nri        numero
005800050704     c                   eval      kpjbu=kpjbus
005900050704
006000050704     c                   endsr
