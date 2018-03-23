000100070119      * FNLSE6R *-----------------------------------------------------*
000200070124      *?STATISTICA NUMERO LDV "A4" ED "A5" PER DATA/LNP/LNA - FILTRO ?*
000300070119      *---------------------------------------------------------------*
000400070119     h decedit('0,') datedit(*dmy/)
000500070119      *---------------------------------------------------------------*
000600070119     fFNLSE6D   cf   e             workstn
000700070119      *---------------------------------------------------------------*
000800070119      *
000900070119      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001000070119      *
001100070119      *
001200070119      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
001300070119      *
001400070119     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
001500070119      *
001600070119      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
001700070119      *
001800070119     d KPJBA         e ds
001900070119      * - Parametri per pgm. batch - FNLSE6R
002000070119     d FNLSE6ds        ds                  inz
002100070119     d  DE6dti                        8s 0 inz(*loval)
002200070119     d  DE6dtf                        8s 0 inz(*hival)
002300070119      *
002400070119      * - Parametri x Controllo profilo utenti
002500070119     d TIBS34ds      e ds
002600070119      * - Ds di riferimento al file esterno AZUTE00F
002700070119     d AZUTEds       e ds                       extname(AZUTE00F)
002800070119      * - Ds per dati organigramma
002900070119     d dDatiUte      e ds
003000070119      *
003100070119      * - Parametri per pgm. controllo data
003200070119     d WLBdat          ds                  inz
003300070119     d  G08dat                        8  0 inz
003400070119     d  G08inv                        8  0 inz
003500070119     d  G08err                        1    inz('3')
003600070119     d  G08tgi                        5  0 inz
003700070119      *
003800070119     d Status         sds
003900070119     d  V1Cpgm           *proc
004000070119      *
004100070119      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
004200070119      *
004300070119      * - flags
004400070119     d $Fine           s              1    inz(*off)
004500070119      *
004600070119      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
004700070119      *
004800070119
004900070119      *---------------------------------------------------------------*
005000070119      *? RIEPILOGO INDICATORI                                        ?*
005100070119      *---------------------------------------------------------------*
005200070119      * 28   - Emissione messaggio di errore                          *
005300070119      * 51   - Data Spedizione iniziale errata                        *
005400070119      * 52   - Data Spedizione  finale  errata                        *
005500070119      * 90   - Riemissione videata                                    *
005600070119      *---------------------------------------------------------------*
005700070119      *
005800070119     c     *Entry        plist
005900070119     c                   parm                    KPJBA
006000070119      *
006100070119      * Gestione videata di lancio D01
006200070119do  1c                   DOU       $Fine = *on
006300070119      *
006400070119     c                   exfmt     LSE6D01
006500070119     c                   clear                   V1Dmsg
006600070119     c                   setoff                                       28  90
006700070119      *
006800070119sel 2c                   SELECT
006900070119      * - F3 = Fine
007000070119w   2c                   WHEN      *inKC
007100070119     c                   eval      $Fine = *on
007200070119      *
007300070119      * - F9 = Personalizzazione Batch
007400070119w   2c                   WHEN      *inKI
007500070119     c                   eval      Kcoaz = 'LSEF'
007600070119     c                   call      'BCH09'
007700070119     c                   parm                    KPJBA
007800070119      *
007900070119      * Controllo video D01
008000070119x   2c                   OTHER
008100070119     c                   exsr      CtrD01
008200070119      * - F6 = Lancio batch (se NO errori)
008300070119if  3c                   if                *inKF
008400070119     c                             and NOT *in90
008500070119     c                   exsr      SbmJob
008600070119     c                   eval      $Fine = *on
008700070119e   3c                   endif
008800070119      *
008900070119e   2c                   ENDSL
009000070119      *
009100070119e   1c                   ENDDO
009200070119      *
009300070119     c                   eval      *inLR = *on
009400070119      *
009500070119      *---------------------------------------------------------------*
009600070119      *?Operazioni Iniziali                                          ?*
009700070119      *---------------------------------------------------------------*
009800070119     c     *InzSR        BEGSR
009900070119      *
010000070119      * Reperimento dati job
010100070119     c     *dtaara       define    §azute        AZUTEds
010200070119     c     *dtaara       define    §datiute      dDATIUTE
010300070119      *
010400070119     c                   in(E)     *dtaara
010500070119     c                   IF        %ERROR or RSUT = *blanks
010600070119     c                   clear                   Tibs34Ds
010700070119     c                   call      'TIBS34R'
010800070119     c                   parm                    Tibs34Ds
010900070119     c                   in        *dtaara
011000070119     c                   ENDIF
011100070119      *
011200070119     c                   ENDSR
011300070119      *
011400070119      *---------------------------------------------------------------*
011500070119      *?Controlli videata D01                                        ?*
011600070119      *---------------------------------------------------------------*
011700070119     c     CtrD01        BEGSR
011800070119      *
011900070119      * Data Spedizione iniziale
012000070119     c                   clear                   WLBdat
012100070119     c                   z-add     V1Cdsi        G08dat
012200070119     c                   call      'XSRDA8'
012300070119     c                   parm                    WLBdat
012400070119     c                   if        G08err = *on
012500070119     c                   seton                                        285190
012600070119     c                   eval      V1Dmsg = $Msg(1)
012700070119     c                   leavesr
012800070119     c                   endif
012900070119     c                   z-add     G08dat        V1Cdsi
013000070119     c                   z-add     G08inv        DE6dti
013100070119      *
013200070119      * Data Spedizione finale
013300070119     c                   clear                   WLBdat
013400070119     c                   z-add     V1Cdsf        G08dat
013500070119     c                   call      'XSRDA8'
013600070119     c                   parm                    WLBdat
013700070119     c                   if        G08err = *on
013800070119     c                   seton                                        285290
013900070119     c                   eval      V1Dmsg = $Msg(1)
014000070119     c                   leavesr
014100070119     c                   endif
014200070119     c                   z-add     G08dat        V1Cdsf
014300070119     c                   z-add     G08inv        DE6dtf
014400070119      *
014500070119      * Range di date
014600070119     c                   if        DE6dti > DE6dtf
014700070119     c                   seton                                        285190
014800070119     c                   eval      V1Dmsg = $Msg(2)
014900070119     c                   leavesr
015000070119     c                   endif
015100070119      *
015200070119     c                   ENDSR
015300070119      *
015400070119      *---------------------------------------------------------------*
015500070119      *?Sottomissione lavoro batch                                   ?*
015600070119      *---------------------------------------------------------------*
015700070119     c     SbmJob        BEGSR
015800070119      *
015900070119      * (parametri già impostati nella subr. "CtrD01")
016000070119     c                   movel(p)  FNLSE6ds      KPJBU
016100070119     c                   movel     'LSEF'        Kcoaz
016200070119      *
016300070119     c                   if        KNMUS = *all'1'
016400070119     c                   call      'FNLSE6C1'
016500070119     c                   parm                    KPJBA
016600070119     c                   else
016700070119     c                   call      'BCH10'
016800070119     c                   parm                    KPJBA
016900070119     c                   endif
017000070119      *
017100070119     c                   ENDSR
017200070119
017300070119** - $Msg -------------------------------------------------------------------*
017400070119Data in formato errato                                                         1
017500070119Range di date errato                                                           2
