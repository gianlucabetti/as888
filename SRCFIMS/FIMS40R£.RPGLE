000100130530     /*PRM  dbgview(*source)
000200130530     /*END
000300061017      *---------------------------------------------------------------*
000400061017      *?FIMS30R - Interrogazione spunte da PC VdL                    ?*
000500061017      *---------------------------------------------------------------*
000600061017
000700061017     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000800061017
000900061017      *---------------------------------------------------------------*
001000980623
001100130530     fFLTR702L  if   e           k disk
001200061017     fAZORG01L  if   e           k disk
001300061017     fTNTBE01L  if   e           k disk
001400061017      *
001500061017     fFIMS40D   cf   e             workstn
001600061017     f                                     sfile(MS40S01:S01nrr)
001700061017     f                                     infds(DSFMT)
001800061017
001900980521      *------------------------------------------------------------------------*
002000061017
002100061017      *
002200061017      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002300061017      *
002400061018      * - Tasti di funzione
002500061017     d C_RollUp        c                   const(x'F5')
002600061018      * - Nr. di righe del sfl (SFLPAG)
002700061018     d C_SflPag        c                   const(07)
002800061017      *
002900061017      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
003000061017      *
003100061017      * - Messaggi di errore
003200061018     d $Msg            s             78    dim(12)  ctdata perrcd(1)
003300061018      *
003400061018      * - Layout: Parcel Entrance Point
003500061018     d $MSLpep         s                   dim(100) like(§MSLpep) inz
003600061018      * - Layout: Tipo Applicazione
003700061018     d $MSLapl         s                   dim(100) like(§MSLapl) inz
003800061017      *
003900061017      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
004000061017      *
004100061017     d KPJBA         e ds
004101140305     d fims40ds      e ds
004200061017      *
004300061017      * - Parametri per richiamo al pgm di controllo profilo utenti
004400061017     d Tibs34Ds      e ds                  inz
004500061017      * - Ds di riferimento al file esterno AzUte00F
004600061017     d AzuteDs       e ds                  ExtName(AzUte00F)
004700061017      * - Ds per dati organigramma
004800061017     d DDatiUte      e ds
004900061017      *
005000061017     d Og143         e ds                  inz
005100061017      *
005200061018      * - Ricerca / Controllo Tabelle
005300061017     d TIBS02ds      e ds                  inz
005400061018      * - Tab. "MFP" = Applicazioni installate c/o P.O.
005500061018     d dMFP          e ds                  inz
005600061018      * - Tab. "MSL" = Layout macchinone smistacolli
005700061018     D dMSL          e ds                  inz
005800061017      *
005900061017     d Status         sds           333
006000061017     d   SDSpgm          *proc
006100061017      *
006200061017     d DSFMT           ds           512
006300061017     d  £Tasto               369    369
006400061017     d  £Nrg                 370    370
006500061017     d  £Ncl                 371    371
006600061017     d  £SFLnrr              378    379b 0
006700061017      *
006800061017     d WLBDAT          ds                  inz
006900061017     d  G02dat                        8  0 inz
007000061017     d  G02inv                        8  0 inz
007100061017     d  G02err                        1    inz('3')
007200061017     d  G02tgi                        4  0 inz
007300061018      *
007400061018     d FIMS41ds        ds                  inz
007500061018     d  MS41fil                       3  0 inz
007600061018     d  MS41dat                       8  0 inz
007700061018     d  MS41ora                       6  0 inz
007800130529     d  MS41mil                            inz   like(TR7mil)
007900130529     d  MS41pd1                            inz   like(TR7pd1)
008000130529     d  MS41pd2                            inz   like(TR7pd2)
008100061017      *
008200061017      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
008300061017      *
008400061017      * - Flag booleani
008500061024     d $First          s              1    inz(*on)
008600061017     d $Fine           s              1    inz(*off)
008700061017     d $InzS01         s              1    inz(*off)
008800061018      *
008900061017      * - Variabili di controllo dspf
009000061017     d S01nrr          s                   like(C01rcd) inz
009100061018      *
009200061018      * Indici di schiera
009300061018     ***d xx              s              3  0 inz
009400061018     d yy              s              3  0 inz
009500061018      *
009600061017      * - Campi di controllo
009700061018     d Kfil            s                   like(TR7fil) inz
009800061018     d Ktim            s                   like(TR7tim) inz
009900061025     d SAVpos          s                   like(V1Cpos) inz(*loval)
010000061017     d SAVpd1          s                   like(V1Cpd1) inz
010100061017     d SAVpd2          s                   like(V1Cpd2) inz
010200061017     d SAVpep          s                   like(V1Cpep) inz
010300061017     d SAVtim          s                   like(V1Ctim) inz
010400061017     d SAVora          s                   like(V1Cora) inz
010500061017     d SAVapl          s                   like(V1Capl) inz
010600061017     d W1Ctim          s                   like(V1Ctim) inz
010700061017      *
010800061017     d Wmin            s              2  0 inz
010900061018     d wPag            s              4  0 inz
011000061018     d wRig            s              3  0 inz
011100061025     d W_SflPag        s              4  0 inz
011200061017      *
011300061017      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
011400061017      *
011500130530      * - FLTR702L
011600130530     c     K02TR702      klist
011700061018     c                   kfld                    Kfil
011800061018     c                   kfld                    Ktim
011900061018      * - TNTBE01L
012000061018     c     K02TBE01      klist
012100061018     c                   kfld                    TBEcod
012200061018     c                   kfld                    TBEke1
012300061017
012400061018      *---------------------------------------------------------------*
012500061017
012600061017
012700061017      *===============================================================*
012800061017      *  ?I N D I C A T O R I?                                        *
012900061017      *---------------------------------------------------------------*
013000061017      *  10 - Comodo                                                  *
013100061017      *  28 - Emissione messaggio di errore a video                   *
013200061017      *  30 - Ripulisce il subfile                                    *
013300061017      *  31 - NON emette il subfile                                   *
013400061017      *  32 - Record modificato nel subfile (sflnxtchg)               *
013500061025      *  33 - Fine dati nel subfile         (sflend)                  *
013600061025      ***40 - Posizionamento cursore sul campo Opzione              ***
013700061025      *  41 - P.O.         errato                                     *
013800061025      *  42 - Data Spunta  errata                                     *
013900061025      *  43 - Ora  Spunta  errata                                     *
014000061025      *  44 - Applicazione errata                                     *
014100061018      *  90 - Riemissione videata                                     *
014200061018      *===============================================================*
014300061018
014400061017     c     *Entry        plist
014500061017     c                   parm                    KPJBA
014501140305     c                   parm                    fims40ds
014600061017      *
014700140305     c                   exsr      InzS01
014701140305     c* Se ricevuto anche il secondo parametro devo impostare i dati del sfl control
014702140305     c* con i dati ricevuti e caricare il sfl
014703140305     c                   if        %parms>1
014704140305     c                   eval      v1cpos=i40pes
014705140305     c                   eval      v1cpd1=%editc(i40lnp:'X')+%editc(i40lna:'X')+
014706140305     c                             %editc(i40nrs:'X')+%editc(i40nsc:'X')
014707140305     c                   eval      v1ctim=i40dfs
014708140305     c                   clear                   WLBdat
014709140305     c                   eval      G02dat  = V1Ctim
014710140305     c                   call      'XSRDA8'
014711140305     c                   parm                    WLBdat
014718140305     c                   eval      W1Ctim  = G02inv
014719140305     c                   eval      $InzS01 =  *on
014720140305     c                   endif
014800061017      *
014900061017     c                   dow       $Fine   =  *off
015000061017     c                   exsr      GesS01
015100061017     c                   enddo
015200061017      *
015300061017     c                   eval      *inLR   =  *on
015400061024
015500061024      *---------------------------------------------------------------*
015600061024      *?Operazioni iniziali                                          ?*
015700061024      *---------------------------------------------------------------*
015800061024     c     *InzSR        BEGSR
015900061024      *
016000061024      * Reperimento dati aziendali
016100061024     c     *dtaara       define    §azute        AZUTEds
016200061024     c     *dtaara       define    §datiute      dDATIUTE
016300061024     c                   in(E)     *dtaara
016400061024     c                   if        %ERROR or RSUT = *blanks
016500061024     c                   clear                   Tibs34Ds
016600061024     c                   call      'TIBS34R'
016700061024     c                   parm                    Tibs34Ds
016800061024     c                   in        *dtaara
016900061024     c                   endif
017000061114      *
017100061114     c                   eval      T01pgm  =  SDSpgm
017200061024      *
017300061024     c                   ENDSR
017400061017
017500061017      *---------------------------------------------------------------*
017600061018      *?Gestione SubFile S01                                         ?*
017700061017      *---------------------------------------------------------------*
017800061017     c     GesS01        BEGSR
017900061017      *
018000061017      * Inizializzazione videata
018100061018if  1c                   if        $InzS01 =  *on
018200061017     c                   exsr      InzS01
018300061025     c*** non qui:       eval      $InzS01 =  *off
018400061018e   1c                   endif
018500061017      *
018600061017      * Emissione subfile
018700061017     c                   eval      *in30   =  (S01nrr <= *zeros)
018800061017     c                   write     MS40T01
018900061017     c                   write     MS40Z01
019000061025     c                   if            S01nrr  <= *zeros
019100061025     c                             and $InzS01 =  *on
019200061025     c                   write     MS40N01
019300061025     c                   endif
019400061017     c                   exfmt     MS40C01
019500061116     c                   setoff                                       28  90
019600061025     c                   eval      $InzS01 =  *off
019700061017      *
019800061017sel 1c                   SELECT
019900061017w   1c                   WHEN      £Tasto  =  C_RollUp
020000061025if  2c                   if        (W_SflPag + 1) * C_SflPag <= 9999
020100061018     c                   exsr      RollUpS01
020200061025x   2c                   else
020300061025     c                   seton                                        28  90
020400061025     c                   eval      V1Dmsg  = $Msg(10)
020500061025     c*** non serve:     leavesr
020600061025e   2c                   endif
020700061017w   1c                   WHEN      *inKC   =  *on
020800061017     c                   eval      $Fine   =  *on
020900061017x   1c                   OTHER
021000061024      * - Gestione opzioni
021100061024if  2c                   if        S01nrr  >  *zeros
021200061018     c                   exsr      OpzS01
021300061024e   2c                   endif
021400061017     c***                if        *in28
021500061017     c***                leavesr
021600061017     c***                endif
021700061024      * - Controllo variazione selezioni
021800061024if  2c                   if             V1Cpos <> SAVpos
021900061024     c                             or   V1Cpd1 <> SAVpd1
022000061024     c                             or   V1Cpd2 <> SAVpd2
022100061024     c                             or   V1Cpep <> SAVpep
022200061024     c                             or   V1Ctim <> SAVtim
022300061024     c                             or   V1Cora <> SAVora
022400061024     c                             or   V1Capl <> SAVapl
022500061024     c                   exsr      CtrC01
022600061024if  3c                   if        *in90
022700061024     c                   leavesr
022800061024e   3c                   endif
022900061024     c                   eval      $InzS01 =  *on
023000061024     c                   eval      SAVpos  =  V1Cpos
023100061024     c                   eval      SAVpd1  =  V1Cpd1
023200061024     c                   eval      SAVpd2  =  V1Cpd2
023300061024     c                   eval      SAVpep  =  V1Cpep
023400061024     c                   eval      SAVtim  =  V1Ctim
023500061024     c                   eval      SAVora  =  V1Cora
023600061024     c                   eval      SAVapl  =  V1Capl
023700061024e   2c                   endif
023800061017e   1c                   ENDSL
023900061017      *
024000061017     c                   ENDSR
024100061018
024200061018      *---------------------------------------------------------------*
024300061018      *?Controllo SubFile Control C01                                ?*
024400061018      *---------------------------------------------------------------*
024500061018     c     CtrC01        BEGSR
024600061018      *
024700061018      * Reset indicatori
024800061018     c                   movea     *zeros        *in(50)
024900061018      *
025000061018      * P.O. spunta
025100061018if  1c                   if        V1Cpos  = *zeros
025200061025     c                   seton                                        284190
025300061018     c                   eval      V1Dmsg  = $Msg(2)
025400061018     c                   leavesr
025500061018x   1c                   else
025600061018     c     V1Cpos        chain     AZORG
025700061018sel 2c                   select
025800061018w   2c                   when      NOT %found(AZORG01L)
025900061025     c                   seton                                        284190
026000061018     c                   eval      V1Dmsg  = $Msg(6)
026100061018     c                   leavesr
026200061018w   2c                   when      ORGfva <> *blanks
026300061025     c                   seton                                        284190
026400061018     c                   eval      V1Dmsg  = $Msg(3)
026500061018     c                   leavesr
026600061018w   2c                   when           ORGfag <> 'F'
026700061018     c                             and  ORGfag <> 'A'
026800061025     c                   seton                                        284190
026900061018     c                   eval      V1Dmsg  = $Msg(4)
027000061018     c                   leavesr
027100061018e   2c                   endsl
027200061018e   1c                   endif
027300061018      *
027400061018      * Data spunta
027500061018if  1c                   if        V1Ctim <> *zeros
027600061018     c                   clear                   WLBdat
027700061018     c                   eval      G02dat  = V1Ctim
027800061018     c                   call      'XSRDA8'
027900061018     c                   parm                    WLBdat
028000061018if  2c                   if        G02err  = *on
028100061025     c                   seton                                        284290
028200061018     c                   eval      V1Dmsg  = $Msg(7)
028300061018     c                   leavesr
028400061018e   2c                   endif
028500061018     c                   eval      V1Ctim  = G02dat
028600061018     c                   eval      W1Ctim  = G02inv
028700061018e   1c                   endif
028800061018      *
028900061018      * Ora spunta
029000061018sel 1c                   select
029100061018w   1c                   when           V1Ctim = *zeros
029200061018     c                             and  V1Cora > *zeros
029300061025     c                   seton                                        284390
029400061018     c                   eval      V1Dmsg  = $Msg(9)
029500061018     c                   leavesr
029600061018w   1c                   when           V1Cora < *zeros
029700061018     c                             or   V1Cora > 2359
029800061025     c                   seton                                        284390
029900061018     c                   eval      V1Dmsg  = $Msg(8)
030000061018     c                   leavesr
030100061018e   1c                   endsl
030200061018     c                   move      V1Cora        Wmin
030300061018if  1c                   if        Wmin    > 59
030400061025     c                   seton                                        284390
030500061018     c                   eval      V1Dmsg  = $Msg(8)
030600061018     c                   leavesr
030700061018e   1c                   endif
030800061018      *
030900061018      * Ricerca sul campo tipo applicazione
031000061018     c***  '?'           scan      V1Capl                                 90
031100061018if  1c***                if        *in90
031200061018     c                   if        %scan('?' : V1Capl) > *zeros
031300061018     c                   eval      *in90   = *on
031400061018     c                   clear                   V1Capl
031500061018     c                   clear                   TIBS02ds
031600061018     c                   eval      T02mod  = 'R'
031700061018     c                   eval      T02sif  = KNSIF
031800061018     c                   eval      T02cod  = 'MTP'
031900061018     c                   call      'TIBS02R'
032000061018     c                   parm                    KPJBA
032100061018     c                   parm                    TIBS02ds
032200061018     c                   if        T02err  = *blanks
032300061018     c                   movel     T02ke1        V1Capl
032400061018     c                   endif
032500061018     c                   leavesr
032600061018e   1c                   endif
032700061018      *
032800061018      * Se ho valorizzato V2CAPL controllo che non sia impostata la
032900061018      * postazione (V2CPEP), e che il valido esista nella tabella 'MPT'
033000061018if  1c                   if             V1Capl <> *blanks
033100061018     c                             and  V1Cpep <> *blanks
033200061025     c                   seton                                        284590
033300061018     c                   eval      V1Dmsg  = $Msg(12)
033400061018     c                   leavesr
033500061018e   1c                   endif
033600061018      *
033700061018      * Postazione in tabella "MPT"
033800061018if  1c                   if        V1Capl <> *blanks
033900061018     c                   clear                   TIBS02ds
034000061018     c                   clear                   TIBS02ds
034100061018     c                   eval      T02mod  = 'C'
034200061018     c                   eval      T02sif  = KNSIF
034300061018     c                   eval      T02cod  = 'MTP'
034400061018     c                   movel     V1Capl        T02ke1
034500061018     c                   call      'TIBS02R'
034600061018     c                   parm                    KPJBA
034700061018     c                   parm                    TIBS02ds
034800061018if  2c                   if        T02err <> *blanks
034900061025     c                   seton                                        284490
035000061018     c                   eval      V1Dmsg  = $Msg(11)
035100061018     c                   leavesr
035200061018e   2c                   endif
035300061018e   1c                   endif
035400061018      *
035500061018     c                   ENDSR
035600061017
035700061017      *---------------------------------------------------------------*
035800061018      *?Inizializzazione SubFile S01                                 ?*
035900061017      *---------------------------------------------------------------*
036000061017     c     InzS01        BEGSR
036100061017      *
036200061017      * Pulizia subfile
036300061017     c                   seton                                        3031
036400061017     c                   write     MS40C01
036500061018     c                   setoff                                         3133
036600061017      *
036700061018     c                   clear                   W_SflPag
036800061018     c                   clear                   C01rcd
036900061018     c                   clear                   C01csr
037000061018     c                   clear                   S01nrr
037100061018     c                   clear                   V1Dmsg
037200061018     c                   setoff                                       28  90
037300061018     c                   movea     *zeros        *in(41)
037400061017      *
037500061018if  1c                   if        V1Cpos <= *zeros
037600061018     c                   eval      V1Cpos  = DUTtfp
037700061017if  2c                   if             DUTlpo = '2'
037800061017     c                             or   DUTlpo = *blanks
037900061018     c                   movel     DUTde3        Og143
038000061017if  3c                   if        §OGpt   = 'S'
038100061018     c                   eval      V1Cpos  = DUTpou
038200061017e   3c                   endif
038300061017e   2c                   endif
038400061017e   1c                   endif
038500061017      *
038600061017      * Se inizializzazione iniziale: esco senza caricare nulla
038700061017     c                   if        $InzS01 = *off
038800061017     c                   leavesr
038900061017     c                   endif
039000061017      *
039100061018      * Caricamento tabelle del P.O spunta selezionato in schiera
039200061018     c                   exsr      sr_CarTAB
039300061018      *
039400061018     c                   movel     V1Cpos        Kfil
039500061018     c                   if        V1Ctim <> *zeros
039600061018     c                   eval      Ktim    = %trim( %editw
039700061018     c                                           (W1Ctim:'0        ') )
039800061018     c                                     + %trim( %editw
039900061018     c                                           (V1Cora:'0    ') )
040000061018     c                                     + '00'
040100130530     c     K02TR702      setll     FLTR7000
040200061018     c                   else
040300061018     c     Kfil          setll     FLTR7000
040400061018     c                   endif
040500061018     c     Kfil          reade     FLTR7000
040600061018      *
040700130530do  1c***                DOW       NOT  %eof(FLTR702L)
040800061018     c                   exsr      RollUpS01
040900061025e   1c***                ENDDO
041000061025     *** *
041100061025     *** * Visualizzazione del SFL (se ci sono dati)
041200061025     ***c                   eval      *in30   = (S01nrr = *zeros)
041300061025     *** *
041400061025     *** * Attivazione (eventuale) del SFLEND
041500130530     ***c                   eval      *in33   = (%eof(FLTR702L))
041600061018      *
041700061018     c                   ENDSR
041800061018
041900061018      *---------------------------------------------------------------*
042000061018      *?Caricamento pagina di dati nel SubFile S01                   ?*
042100061018      *---------------------------------------------------------------*
042200061018     c     RollUpS01     BEGSR
042300061018      *
042400061018     c                   eval      S01nrr      = (W_SflPag * C_SflPag)
042500061018     c                   add       1             W_SflPag
042600061018     c                   eval      *in32       = *off
042700061018      *
042800061018      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
042900130530do  1c                   doW       NOT  %eof(FLTR702L)
043000061018     c                             and  S01nrr < (W_SflPag * C_SflPag)
043100061018     c                   exsr      CarS01
043200061018     c     Kfil          reade     FLTR7000
043300061018e   1c                   enddo
043400061025      *
043500061025      * Visualizzazione del SFL (se ci sono dati)
043600061025     c                   eval      *in30   = (S01nrr = *zeros)
043700061025      *
043800061025      * Attivazione (eventuale) del SFLEND
043900130530     c                   eval      *in33   = (%eof(FLTR702L))
044000061025      *
044100061025      * Posizionamento cursore al 1° rcd della pagina
044200061025if  1c                   if        S01nrr  > *zeros
044300061025     c     S01nrr        div       C_SflPag      wPag
044400061025     c                   mvr                     wRig
044500061025     c                   eval      C01rcd  = wPag * C_SflPag
044600061025     c                   if        wRig    > *zeros
044700061025     c                   eval      C01rcd  = C01rcd + 1
044800061025     c                   else
044900061025     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
045000061025     c                   endif
045100061025x   1c                   else
045200061025     c                   clear                   C01rcd
045300061025e   1c                   endif
045400061025     c                   eval      C01csr  = C01rcd
045500061018      *
045600061018     c                   ENDSR
045700061018
045800061018      *---------------------------------------------------------------*
045900061018      *?Caricamento singolo record nel SubFile S01                   ?*
046000061018      *---------------------------------------------------------------*
046100061018     c     CarS01        BEGSR
046200061018      *
046300061018      * Selezione record
046400061018sel 1c                   SELECT
046500061018      * - Data
046600061018w   1c***                WHEN           V1Ctim <> *zeros
046700061018     c***                          and  TR7tim  > %trim( %editw
046800061018     c***                                         (V1Ctim:'0        '))
046900061018     c***                                       + '999999'
047000061018     c***  *hival        setgt     FLTR7000
047100061018     c***                leavesr
047200061018      * - Data
047300061018w   1c                   WHEN           V1Ctim <> *zeros
047400061018     c                             and  %trim( %editw
047500061018     c                                    (W1Ctim:'0        ') ) <>
047600061018     c                                    %subst(TR7tim:1:8)
047700061018     c     *hival        setgt     FLTR7000
047800061018     c                   leavesr
047900061018      * - Ora
048000061018w   1c                   WHEN           V1Cora <> *zeros
048100061018     c                             and  %trim(%editw(V1Cora:'0    '))
048200061025     c                                          > %subst(TR7tim:9:4)
048300061018     c                   leavesr
048400061018      * - Record "annullato"
048500061018w   1c                   WHEN           TR7cod  = '10001'
048600061018     c                             or   TR7cod  = '10009'
048700061018     c                   leavesr
048800061018e   1c                   ENDSL
048900061018      * - Barcode letto da Scanner Datalogic
049000061018if  1c                   if        V1Cpd1 <> *blanks
049100061018     c***  ' '           checkr    V1Cpd1        xx
049200061018     c***                eval      yy      = %scan( %subst
049300061018     c***                                    (V1Cpd1:1:xx) : TR7pd1 )
049400061018     c                   eval      yy      = %scan( %trimr(V1Cpd1)
049500061018     c                                            : TR7pd1 )
049600061018if  2c                   if        yy      = *zeros
049700061018     c                   leavesr
049800061018e   2c                   endif
049900061018e   1c                   endif
050000061018      * - Barcode letto da Pistola
050100061018if  1c                   if        V1Cpd2 <> *blanks
050200061018     c***  ' '           checkr    V1Cpd2        xx
050300061018     c***                eval      yy      = %scan( %subst
050400061018     c***                                    (V1Cpd2:1:xx) : TR7pd2 )
050500061018     c                   eval      yy      = %scan( %trimr(V1Cpd2)
050600061018     c                                            : TR7pd2 )
050700061018if  2c                   if        yy      = *zeros
050800061018     c                   leavesr
050900061018e   2c                   endif
051000061018e   1c                   endif
051100061018      * - Postazione
051200061018     c                   if        V1Cpep <> *blanks
051300061018     c***  ' '           checkr    V1Cpep        xx
051400061018     c***                eval      yy      = %scan( %subst
051500061018     c***                                    (V1Cpep:1:xx) : TR7pep )
051600061018     c                   eval      yy      = %scan( %trimr(V1Cpep)
051700061018     c                                            : TR7pep )
051800061018     c                   if        yy      = *zeros
051900061018     c                   leavesr
052000061018     c                   endif
052100061018     c                   endif
052200061018      * - Tipo applicazione
052300061018      *   Se non trovo la postazione in tab. "MSL" considero il record
052400061018      *   valido solo se ho selezionato il tipo applicazione principale
052500061018if  1c                   if        V1Capl <> *blanks
052600061018     c                   eval      yy      = 1
052700061018     c     TR7pep        lookup    $MSLpep(yy)                            10
052800061018if  2c                   if             *in10
052900061018     c                             and  V1Capl <> $MSLapl(yy)
053000061018     c                   leavesr
053100061018e   2c                   endif
053200061018if  2c                   if        NOT  *in10
053300061018     c                             and  V1Capl <> §MFPapl
053400061018     c                   leavesr
053500061018e   2c                   endif
053600061018e   1c                   endif
053700061018      *
053800061018      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
053900061018     c                   exsr      RieS01
054000061018      *
054100061018      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
054200061018     c                   add       1             S01nrr
054300061018     c                   write     MS40S01
054400061018      *
054500061018     c                   ENDSR
054600061018
054700061018      *---------------------------------------------------------------*
054800061018      *?Riempimento singolo record del SubFile S01                   ?*
054900061018      *---------------------------------------------------------------*
055000061018     c     RieS01        BEGSR
055100061018      *
055200061018     c                   clear                   MS40S01
055300061018      *
055400061018      * - Campi hidden
055500061018     c                   movel     TR7tim        V2Camg
055600061018     c                   reset                   WLBdat
055700061018     c                   eval      G02inv  = V2Camg
055800061018     c                   call      'XSRDA8'
055900061018     c                   parm                    wlbdat
056000061018     c                   eval      V2Cgma  = G02dat
056100061018     c                   eval      V2Cmil  = TR7mil
056200130529     c                   eval      V2Hpd1  = TR7pd1
056300130529     c                   eval      V2Hpd2  = TR7pd2
056400061018      *
056500061018      * - Campi visualizzati nel 1° rec. del sfl
056600061018     c                   movel     V2Cgma        V2Cmg
056700061018     c                   move      TR7tim        V2Chms
056800061018     c                   eval      V2Cctl  = TR7ctl
056900061018     c                   eval      V2Cnfv  = TR7nfv
057000061024     c***                eval      V2Cpd1  = TR7pd1
057100061024     c***                eval      V2Cpd2  = TR7pd2
057200061024     c                   move      TR7pd1        V2Cpd1
057300061024     c                   move      TR7pd2        V2Cpd2
057400061018      * - Campi visualizzati nel 2° rec. del sfl
057500061018     c                   eval      V2Cpep  = TR7pep
057600061018     c                   eval      V2C020  = TR7020
057700061018     c                   eval      V2Cpod  = TR7pod
057800061018     c                   eval      V2Cods  = TR7ods
057900061018     c                   eval      V2Ccrc  = TR7crc
058000061018      *
058100061018     c                   ENDSR
058200061018
058300061018      *---------------------------------------------------------------*
058400061018      *?Gestione singola opzione nel SubFile S01                     ?*
058500061018      *---------------------------------------------------------------*
058600061018     c     OpzS01        BEGSR
058700061018      *
058800061018     c                   readc     MS40S01
058900061018      *
059000061018do  1c                   DOW       NOT  %eof(FIMS40D)
059100061018      *
059200061018     c                   eval      *in32       = *off
059300061018     c                   movea     *zeros        *in(40)
059400061018     c                   z-add     S01nrr        C01rcd
059500061018      *
059600061018sel 2c                   SELECT
059700061018      * - Nessuna opzione
059800061018w   2c                   WHEN      S1Copz      = *blanks
059900061018      * - 5 = Visualizzazione
060000061018w   2c                   WHEN      S1Copz      = '5'
060100061018     c                   exsr      sr_OpzS1_5
060200061018      * - ? = Opzione NON valida
060300061018x   2c***                OTHER
060400061025     c***                seton                                        284090
060500061018     c***                eval      V1Dmsg      = $Msg(??)
060600061018e   2c                   ENDSL
060700061018      *
060800061018      * Aggiornamento sfl
060900061018sel 2c                   select
061000061018w   2c                   when      *in28
061100061018     c                   eval      *in32       = *on
061200061018     c                   z-add     C01rcd        C01csr
061300061018w   2c                   when      *in90       = *on
061400061018     c                   z-add     C01rcd        C01csr
061500061018     c                   clear                   S1Copz
061600061018x   2c                   other
061700061018     c                   clear                   S1Copz
061800061018e   2c                   endsl
061900061018     c                   UPDATE    MS40S01
062000061018if  2c                   if        *in28  OR  *in90
062100061018     c                   leave
062200061018e   2c                   endif
062300061018      *
062400061018     c                   readc     MS40S01
062500061018      *
062600061018e   1c                   ENDDO
062700061018      *
062800061018     c                   ENDSR
062900061018
063000061018      *---------------------------------------------------------------*
063100061018      *?Visualizzazione singola lettura                              ?*
063200061018      *---------------------------------------------------------------*
063300061018     c     sr_OpzS1_5    BEGSR
063400061018      *
063500061018     c                   clear                   FIMS41ds
063600061018     c                   eval      MS41fil = V1Cpos
063700061018     c                   eval      MS41dat = V2Camg
063800061018     c                   eval      MS41ora = V2Chms
063900061018     c                   eval      MS41mil = V2Cmil
064000130529     c                   eval      MS41pd1 = V2Hpd1
064100130529     c                   eval      MS41pd2 = V2Hpd2
064200061018      *
064300061018     c                   movel(p)  FIMS41ds      KPJBU
064400061018      *
064500061018     c                   call      'FIMS41R'
064600061018     c                   parm                    KPJBA
064700061018      *
064800061018     c                   ENDSR
064900061018      *---------------------------------------------------------------*
065000061018      *?Caricamento Tabelle occorrenti                               ?*
065100061018      *---------------------------------------------------------------*
065200061018     c     sr_CarTAB     BEGSR
065300061018      *
065400061018      * Tab. "MSL" = Layout macchinone smistacolli
065500061018     c                   clear                   yy
065600061018     c                   clear                   dMSL
065700061018     c                   eval      TBEcod  = 'MSL'
065800061018     c                   movel(p)  V1Cpos        TBEke1
065900061018     c     K02TBE01      setll     TNTBE000
066000061018     c     K02TBE01      reade     TNTBE000
066100061018do  1c                   dow       NOT %eof(TNTBE01L)
066200061018if  2c                   if        TBEatb  = *blanks
066300061018     c                   add       1             yy
066400061018     c                   movel     TBEuni        dMSL
066500061018     c                   eval      $MSLpep(yy) = §MSLpep
066600061018     c                   eval      $MSLapl(yy) = §MSLapl
066700061018e   2c                   endif
066800061018     c     K02TBE01      reade     TNTBE000
066900061018e   1c                   enddo
067000061018      *
067100061018      * Tipo applicazione principale del P.O.
067200061018     c                   eval      TBEcod  = 'MFP'
067300061018     c                   movel(p)  V1Cpos        TBEke1
067400061018     c     K02TBE01      chain     TNTBE000
067500061018if  1c                   if             %found(TNTBE01L)
067600061018     c                             and  TBEatb = *blanks
067700061018     c                   movel     TBEuni        dMFP
067800021206     c                   endif
067900021206      *
068000021206     c                   ENDSR
068100010608
068200061018      *---------------------------------------------------------------*
068300061018
068400061018** - $MSG -------------------------------------------------------------------*
068500061025     L I B E R O                                                                1
068600061018Inserire Punto Operativo                                                        2
068700061018Punto Operativo annullato                                                       3
068800061018Punto Operativo non valido                                                      4
068900061025     L I B E R O                                                                5
069000061018Codice inesistente                                                              6
069100061018Data errata                                                                     7
069200061018Ora formalmente errata                                                          8
069300061018Non è possibile inserire un orario senza data                                   9
069400061018Ampiezza massima subfile superata                                              10
069500061018Applicazione errata                                                            11
069600061018Postazione/Applicazione sono in alternativa                                    12
