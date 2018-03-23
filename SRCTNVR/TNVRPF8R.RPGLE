000100051007      *
000200051007     h decedit('0,') datedit(*dmy/)
000300051007      *
000400051007     fTNVRPF8D  cf   e             workstn
000500051007     fAZORG01L  if   e           k disk
000600051007      *
000700051007     d KPJBA         e ds
000800051007      * - Parametri x Controllo profilo utenti
000900051007     d TIBS34DS      e ds                  inz
001000051007      * - Ds di riferimento al file esterno AZUTE00F
001100051007     d AZUTEds       e ds                  extname(AZUTE00F)
001200051007      * - Ds per dati organigramma
001300051007     d DDatiUte      e ds
001400051007      *
001500051007     d Status         sds           333
001600051007     d   SDSpgm          *proc
001700051007      *
001800051007     d WLBDAT          ds                  inz
001900051007     d  G02DAT                 1      8  0 inz
002000051007     d  G02INV                 9     16  0 inz
002100051007     d  G02ERR                17     17    inz
002200051007     d  G02TGI                18     22  0 inz
002300020130      *
002400051007     d PARAM           ds
002500030508     d  PARDDS                        8  0
002600030508     d  PARADS                        8  0
002700030910     d  PARPUL                        2
002800030910     d  PARINV                        2
002900030508      *
003000051007     d INVDDS          s              8  0 inz
003100051007     d INVADS          s              8  0 inz
003200051007     d XX              s              2  0 inz
003300990426      * ---------------------------------------------------------
003400990427
003500051007      *
003600051007     c     *Entry        plist
003700051007     c                   parm                    KPJBA
003800051007      *
003900051007      *   G E S T I O N E   V I D E O  1
004000051007     c                   DO        *hival
004100030509      *
004200030509     c                   setoff                                       28
004300990427      *
004400051007     c                   exfmt     Video01
004500990427      *
004600990428      * F3  = FINE
004700051007     c                   if        *inKC
004800051007     c                   leave
004900051007     c                   endif
005000051007      *
005100051007     c                   exsr      CtrD01
005200051007      *
005300051007     c                   if        *in28
005400051007     c                   iter
005500051007     c                   endif
005600030508      *
005700030508      * F6  = CONFERMA
005800051007     c                   if        *inKF
005900051007     c                   movel     PARAM         KPJBU
006000051007     c                   call      'TNVRPF9C'
006100051007     c                   parm                    KPJBA
006200051007     c                   leave
006300051007     c                   endif
006400051007      *
006500051007     c                   ENDDO
006600990426      *
006700051007     c                   eval      *inLR = *on
006800990427      **********************************************************************
006900990427      *  CONTROLLI VIDEATA 1
007000990427      **********************************************************************
007100051007     c     CTRD01        BEGSR
007200051007      *
007300030507      * CONTROLLO LA DATA SPEDIZIONE
007400051007     c                   clear                   INVDDS
007500051007     c                   if        VIDdds > 0  and  NOT *in28
007600051007     c                   clear                   WLBDAT
007700051007     c                   z-add     VIDdds        G02DAT
007800051007     c                   call      'XSRDA8'
007900051007     c                   parm                    WLBDAT
008000051007    1c                   if        G02err = *on
008100051007     c                   seton                                        5128
008200051007   X1c                   else
008300051007     c                   move      G02inv        INVdds
008400051007     c                   move      G02dat        VIDdds
008500051007    1c                   endif
008600030508     c                   endif
008700051007      *
008800030508      * CONTROLLO LA DATA SPEDIZIONE
008900051007     C                   clear                   INVADS
009000051007     c                   if        VIDads > 0  and  NOT *in28
009100051007     c                   clear                   WLBdat
009200051007     c                   z-add     VIDads        G02dat
009300051007     c                   call      'XSRDA8'
009400051007     c                   parm                    WLBdat
009500051007    1c                   if        G02err = *on
009600051007     c                   seton                                        5228
009700051007   X1c                   else
009800051007     c                   move      G02inv        INVads
009900051007     c                   move      G02dat        VIDads
010000051007    1c                   endif
010100051007     c                   endif
010200051007      *
010300030508      * verifico se le date sono corrette
010400051007     c                   if             *in28  = *off
010500051007     c                             and  INVads > 0
010600051007     c                             and  INVdds > 0
010700051007     c                             and  INVads < INVdds
010800051007     c                   seton                                        5528
010900030508     c                   endif
011000051007      *
011100051007     c                   if        *in28  = *off
011200051007     c                   eval      PARDDS = INVDDS
011300051007     c                   eval      PARADS = INVADS
011400051007     c                   eval      PARPUL = 'SI'
011500051007     c                   eval      PARINV = 'SI'
011600051007     c                   endif
011700051007      *
011800051007     c                   ENDSR
011900010905
012000990426      *****************************************************************
012100990426      *  ROUTINE INIZIALE
012200990426      *****************************************************************
012300051007     c     *INZSR        BEGSR
012400051007      *
012500051007      * Reperimento Dati del job (Utente/Operativi)                   *
012600051007     c     *dtaara       define    §azute        azuteds
012700051007     c     *dtaara       define    §datiute      ddatiute
012800051007      *
012900051007     c                   in(E)     *dtaara
013000051007     c                   IF        %ERROR or RSUT = *blanks
013100051007     c                   clear                   Tibs34Ds
013200051007     c                   call      'TIBS34R'
013300051007     c                   parm                    Tibs34Ds
013400051007     c                   in        *dtaara
013500051007     c                   ENDIF
013600030910      *
013700051007     c                   eval      VTCpgm = SDSpgm
013800051007     c                   eval      VIDpul = 'NO'
013900051007     c                   eval      VIDinv = 'NO'
014000030910      *
014100051007     c                   ENDSR
