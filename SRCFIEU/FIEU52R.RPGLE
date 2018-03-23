000100050707     H DECEDIT('0.') DATEDIT(*YMD.)
000200050927     H* Fieu52R *----------------------------------------------------*
000300900418     H*                                                              *
000400050927     H* AGGIORNA FIDPO                                               *
000500060920     h* record in stato 10 TRASMESSI PORTARE A 19 ORM EXPORT IN FASE 999
000600050919     h*                                                              *
000700050920      **?__________________________________________________________________ */
000800050919     Ffidpo04l  uf   E           k DISK    commit
000900050927     Ffnorm01l  if   E           k DISK
001000900418     D KPJBA         E DS
001100050704
001200050705     D KPJBus          s                   like(kpjbu)
001300050705     D primo           s              1
001400060920     D status          s                   like(dposts)
001500050920      **?__________________________________________________________________ */
001600050704      *    main
001700050920      **?__________________________________________________________________ */
001800050919
001801060920     c                   z-add     10            status
001900050927     c     status        setll     fidpo04l
002000050705     c                   do        *hival
002100050927     c     status        reade     fidpo04l
002200050705
002300050927     c                   if        %eof(fidpo04l)
002400050705     c                   leave
002500050705     c                   endif
002600050705
002700050919      * se ancora da trasmettere bypassa
002800050919     c                   if        dpodtr = 0
002900050919     c                   iter
003000050919     c                   endif
003100060814
003200050927      * ricerca su orf motivo mancata consegna con ORM e fase 999
003300050927     c     korm          chain     fnorm01l
003400050927     c                   if        %found(fnorm01l)
003500050927     c                   if        ormfao = 999
003600050927     c                   z-add     0             DPODTR
003700050927     c                   z-add     0             DPONTR
003800060920     c                   z-add     19            DPOsts
003900050919     c                   update    fidpo000                             99
004000050927     c                   endif
004100050927     c                   endif
004200050705
004300050705     c                   enddo
004400050707     c                   commit
004500050704      *
004600050705     c                   seton                                        lr
004700050920      *
004800050920      **?__________________________________________________________________ */
004900050512     c     *inzsr        begsr
005000050920      **?__________________________________________________________________ */
005100050512     c*
005200050705     c     *entry        plist
005300050705     C                   parm                    kpjba
005400050705     C                   TIME                    ORADAT           14 0
005500050705     C                   MOVEL     ORADAT        ORATR             6 0
005600050920     c                   move      *date         udtymd            8 0
005700050927     c     korm          klist
005800050920     c                   kfld                    DPOPOE
005900050920     c                   kfld                    DPONRS
006000050920     c                   kfld                    DPONOR
006100050920     c                   kfld                    DPONRV
006200050920
006300050512     c                   endsr
