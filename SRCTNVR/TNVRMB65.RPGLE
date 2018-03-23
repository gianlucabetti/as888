000100050302     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200001006
000300001010
000400100322     FFNORM06L  UF A E           K DISK
000500001010
000600001010      *------------------------------------------------------------------------*
000700001023
000800100322     d wpor            s              3  0 inz(0)
000900100322     d wpoe            s              3  0
001000100322     d wnsr            s              2  0
001100100322     d wnor            s              7  0
001200100322     d wnrv            s              2  0
001300001117
001400001116     D TISI92DS      E DS
001500030623     D TRUL90DS      E DS
001600001023     D FIOR05DS      E DS
001700001010     D TIBS02DS      E DS
001800001010     D FNLV14DS      E DS
001900001010     D FNLV13DS      E DS
002000001010     D TISI95DS      E DS
002100060929     d tisi97ds      e ds
002200001011     D FIOR01DS      E DS
002300070905     d fior37ds      e ds
002400001025     D FIOR06DS      E DS
002500010411     D FIOR30DS      E DS
002600010704     D DTASV         E DS
002700070905     d og147         e ds
002800010206     D OG148         E DS
002900010515     D OG143         E DS
003000060929     d c_og143       e ds                  extname(og143)
003100060929     d                                     prefix(c_)
003200010202     D DDFT          E DS
003300010301     D DCMR          E DS
003400081028     d a_dcmr        e ds                  extname(dcmr) prefix(a_)
003500010314     D DFAR          E DS
003600011113     D DORM01        E DS
003700081125     d dorf01        e ds
003800010201     D TRUL06DS      E DS
003900010201     D  LIN                    1     90  0
004000010201     D                                     DIM(30)
004100030922     d Trul33ds      e ds
004200040419     d dOsr          e ds
004300050323     d Tibs10ds      e ds
004400050323     d  skfigli               21   5520  0 dim(500)
004500060109     d fnlv55ds      e ds
004600061023     d fieu55ds      e ds
004700070122     d fior56ds      e ds
004800070913     d fidg09ds      e ds
004900071031     d dagr          e ds
005000081126     d ds3idp        e ds
005100100219     d Tisie3ds      e ds
005200100222     d Tisie8ds      e ds
005300100222     d ds15          e ds
005400060203
005500060203      * Tabella "03" = Codici Zona
005600060203     d ds03          e ds                  inz
005700080812      * passaggio parametri per controllo se ritiro all'estero fattibile
005800080812     d fnlv12ds      e ds                  inz
005900090225      * decorrenza per info appuntamento orm commissionati
006000090225     d dvpodeco      e ds
006100090330      * ritorno dati cliente
006200090330     d ds3k          e ds                  inz
006300100204      * personalizzazione VAO per cliente
006400100204     d dpvo          e ds
006500100224      * Invio e-mail di preavviso
006600100224     d FIOR52DS      e ds
006700001010
006800001010      *
006900001010     D KPJBA         E DS
007000060203      * - Parametri x Controllo profilo utenti
007100060203     d TIBS34ds      e ds
007200060203      * - Ds di riferimento al file esterno AZUTE00F
007300060203     d AZUTEds       e ds                  extname(AZUTE00F)
007400060203      * - Ds per dati organigramma
007500060203     d DDatiUte      e ds
007600100218
007700100218      // ds per controllo email
007800100218     d dsemail       e ds
007900001010      *
008000001010     D                SDS
008100001010     D  VTCPGM                 1     10
008200100219
008300100219      //---------------------------------------------------------------
008400100219      //?Definizione procedure utilizzate.
008500100219      //---------------------------------------------------------------
008600100224
008700100218
008800100218      //---------------------------------------------------------------
008900100218      //?Definizione key-list.
009000100218      //---------------------------------------------------------------
009100100218
009200001010
009300001006      *------------------------------------------------------------------------*
009400001009
009500100322     c     wpor          setll     fnorm06l
009600100322     c                   do        *hival
009700100322     c     wpor          reade     fnorm06l
009800100322     c                   if        %eof(fnorm06l)
009900100322     c                   leave
010000100322     c                   endif
010100100322     c                   eval      wpoe = ormpoe
010200100322     c                   eval      wnsr = ormnsr
010300100322     c                   eval      wnor = ormnor
010400100322     c                   eval      wnrv = ormnrv
010500100322
010600100322     C                   exsr      Sr_Contrpor
010700100322     c                   update    fnorm000
010800100322
010900100322     c                   enddo
011000010329
011100001010
011200001006     C                   eval      *inlr = *on
011300050323
011400001016      **********************************************************************
011500001016      * CONTROLLI     * P.O. ritiro *
011600001016      **********************************************************************
011700001016     C     Sr_Contrpor   begsr
011800100322
011900100322     C                   clear                   TISI95DS
012000100322     C                   clear                   FNLV13DS
012100100322     C                   movel     ' '           I95tla
012200100322     C                   movel     '7'           I95tcn
012300100322     C                   movel     ormcar        I95cap
012400100322     C                   movel     ormnar        I95nar
012500100322     C                   movel     orminr        I95ind
012600100322     C                   movel(p)  ormlor        I95loc
012700100322     C                   movel     ormprr        I95prv
012800100322     C                   movel     'S'           I13af0
012900100322     C                   movel     'S'           I13af1
013000100322     C                   movel     'S'           I13sz2
013100100322     C                   movel     'S'           I13la3
013200100322     C                   movel     'S'           I13sz3
013300100322     C                   movel     'S'           I13cnv
013400100322
013500100322     C                   call      'FNLV13R'
013600100322     C                   parm                    KPJBA
013700100322     C                   parm                    FNLV13DS
013800100322     C                   parm                    TISI95DS
013900100322
014000100322
014100100322     C                   movel     O95lna        ormpor
014200010301
014300001016     C     endsrcpor     endsr
