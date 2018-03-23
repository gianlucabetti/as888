000100060330      * FNLS16R *-----------------------------------------------------*
000200060330      *?STATISTICA NUMERO PAGINE PACKING-LIST PER CLIENTE            ?*
000300060330      *---------------------------------------------------------------*
000400060330     h decedit('0,') datedit(*dmy/)
000500060330      *---------------------------------------------------------------*
000600060330     fFNLSE3D   cf   e             workstn
000700060330      *---------------------------------------------------------------*
000800060330      *
000900060330      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001000060330      *
001100131009     d c_CmdDspDbf     c                   const('DSPDBF +
001200131009     d                                            file(*libl/WFPDL00F) +
001300131009     d                                            mbr(*first) +
001400131009     d                                            output(*)')
001500060330      *
001600060330      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
001700060330      *
001800060331     d $Msg            s             78    dim( 2)  ctdata  perrcd(1)
001900060330      *
002000060330      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
002100060330      *
002200060330     d KPJBA         e ds
002300060330      * - Parametri per pgm. batch - FNLSE4R
002400060330     d FNLSE4ds        ds                  inz
002500060330     d  DE4dti                        8  0 inz(*loval)
002600060330     d  DE4dtf                        8  0 inz(*hival)
002700060330      *
002800060330      * - Parametri x Controllo profilo utenti
002900060330     d TIBS34ds      e ds
003000060330      * - Ds di riferimento al file esterno AZUTE00F
003100060330     d AZUTEds       e ds                       extname(AZUTE00F)
003200060330      * - Ds per dati organigramma
003300060330     d dDatiUte      e ds
003400060330      *
003500060330      * - Parametri per pgm. controllo data
003600060330     d WLBdat          ds                  inz
003700060330     d  G08dat                        8  0 inz
003800060330     d  G08inv                        8  0 inz
003900060330     d  G08err                        1    inz('3')
004000060330     d  G08tgi                        5  0 inz
004100060330      *
004200060330     d Status         sds
004300060330     d  V1Cpgm           *proc
004400060330      *
004500060330      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
004600060330      *
004700060330      * - flags
004800060330     d $Fine           s              1    inz(*off)
004900131009      *
005000131009      *?  P R O T O T I P I - P R O C E D U R E   - - - - - - - - - -?*
005100131009      *
005200131009      * - Execuzione comando di sistema
005300131009      /copy gaitrasrc/srcProtoPR,QCMDEXC
005400060330      *
005500060330      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
005600060330      *
005700060330
005800060330      *---------------------------------------------------------------*
005900060330      *? RIEPILOGO INDICATORI                                        ?*
006000060330      *---------------------------------------------------------------*
006100060330      * 28   - Emissione messaggio di errore                          *
006200060330      * 51   - Data Spedizione iniziale errata                        *
006300060330      * 52   - Data Spedizione  finale  errata                        *
006400060330      * 90   - Riemissione videata                                    *
006500060330      *---------------------------------------------------------------*
006600060330      *
006700060330     c     *Entry        plist
006800060330     c                   parm                    KPJBA
006900060330      *
007000060330      * Gestione videata di lancio D01
007100060330do  1c                   DOU       $Fine = *on
007200060330      *
007300060330     c                   exfmt     LSE3D01
007400060330     c                   clear                   V1Dmsg
007500060330     c                   setoff                                       28  90
007600060330      *
007700060330sel 2c                   SELECT
007800060330      * - F3 = Fine
007900060330w   2c                   WHEN      *inKC
008000060330     c                   eval      $Fine = *on
008100060330     c***                leave
008200060331      *
008300060331      * - F9 = Personalizzazione Batch
008400060331w   2c                   WHEN      *inKI
008500060331     c                   eval      Kcoaz = 'LSE4'
008600060331     c                   call      'BCH09'
008700060331     c                   parm                    KPJBA
008800131009      *
008900131009      * - F11 = Visualizzazione WFPDL00F
009000131009w   2c                   WHEN      *inKK
009100131009      /free
009200131009          callp(e) ExecuteCommand ( c_CmdDspDbf : %len(c_CmdDspDbf) );
009300131009      /end-free
009400060330      *
009500060330      * Controllo video D01
009600060330x   2c                   OTHER
009700060330     c                   exsr      CtrD01
009800060330      * - F6 = Lancio batch (se NO errori)
009900060330if  3c                   if                *inKF
010000060330     c                             and NOT *in90
010100060330     c                   exsr      SbmJob
010200060330     c                   eval      $Fine = *on
010300060330     c***                leave
010400060330e   3c                   endif
010500060330      *
010600060330e   2c                   ENDSL
010700060330      *
010800060330e   1c                   ENDDO
010900060330      *
011000060330     c                   eval      *inLR = *on
011100060330      *
011200060330      *---------------------------------------------------------------*
011300060330      *?Operazioni Iniziali                                          ?*
011400060330      *---------------------------------------------------------------*
011500060330     c     *InzSR        BEGSR
011600060330      *
011700060330      * Reperimento dati job
011800060330     c     *dtaara       define    §azute        AZUTEds
011900060330     c     *dtaara       define    §datiute      dDATIUTE
012000060330      *
012100060330     c                   in(E)     *dtaara
012200060330     c                   IF        %ERROR or RSUT = *blanks
012300060330     c                   clear                   Tibs34Ds
012400060330     c                   call      'TIBS34R'
012500060330     c                   parm                    Tibs34Ds
012600060330     c                   in        *dtaara
012700060330     c                   ENDIF
012800060330      *
012900060330     c                   ENDSR
013000060330      *
013100060330      *---------------------------------------------------------------*
013200060330      *?Controlli videata D01                                        ?*
013300060330      *---------------------------------------------------------------*
013400060330     c     CtrD01        BEGSR
013500060330      *
013600060330      * Data Spedizione iniziale
013700060330     c                   clear                   WLBdat
013800060330     c                   z-add     V1Cdsi        G08dat
013900060330     c                   call      'XSRDA8'
014000060330     c                   parm                    WLBdat
014100060330     c                   if        G08err = *on
014200060330     c                   seton                                        285190
014300060331     c                   eval      V1Dmsg = $Msg(1)
014400060330     c                   leavesr
014500060330     c                   endif
014600060330     c                   z-add     G08dat        V1Cdsi
014700060330     c                   z-add     G08inv        DE4dti
014800060330      *
014900060330      * Data Spedizione finale
015000060330     c                   clear                   WLBdat
015100060330     c                   z-add     V1Cdsf        G08dat
015200060330     c                   call      'XSRDA8'
015300060330     c                   parm                    WLBdat
015400060330     c                   if        G08err = *on
015500060330     c                   seton                                        285290
015600060331     c                   eval      V1Dmsg = $Msg(1)
015700060330     c                   leavesr
015800060330     c                   endif
015900060330     c                   z-add     G08dat        V1Cdsf
016000060330     c                   z-add     G08inv        DE4dtf
016100060330      *
016200060330      * Range di date
016300060330     c                   if        DE4dti > DE4dtf
016400060330     c                   seton                                        285190
016500060331     c                   eval      V1Dmsg = $Msg(2)
016600060330     c                   leavesr
016700060330     c                   endif
016800060330      *
016900060330     c                   ENDSR
017000060330      *
017100060330      *---------------------------------------------------------------*
017200060330      *?Sottomissione lavoro batch                                   ?*
017300060330      *---------------------------------------------------------------*
017400060330     c     SbmJob        BEGSR
017500060330      *
017600060330      * (parametri già impostati nella subr. "CtrD01")
017700060330     c                   movel(p)  FNLSE4ds      KPJBU
017800060330     c                   movel     'LSE4'        Kcoaz
017900060330      *
018000060330     c                   if        KNMUS = '1111111111'
018100060331     c                   call      'FNLSE4C'
018200060330     c                   parm                    KPJBA
018300060330     c                   else
018400060330     c                   call      'BCH10'
018500060330     c                   parm                    KPJBA
018600060330     c                   endif
018700060330      *
018800060330     c                   ENDSR
018900060331
019000060331** - $Msg -------------------------------------------------------------------*
019100060331Data in formato errato                                                         1
019200060331Range di date errato                                                           2
