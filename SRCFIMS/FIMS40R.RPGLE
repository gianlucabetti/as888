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
003800161025     d $MSLap5         s                   dim(100) like(§MSLap5) inz
003900061017      *
004000061017      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
004100061017      *
004200061017     d KPJBA         e ds
004300140305     d fims40ds      e ds
004400061017      *
004500061017      * - Parametri per richiamo al pgm di controllo profilo utenti
004600061017     d Tibs34Ds      e ds                  inz
004700061017      * - Ds di riferimento al file esterno AzUte00F
004800061017     d AzuteDs       e ds                  ExtName(AzUte00F)
004900061017      * - Ds per dati organigramma
005000061017     d DDatiUte      e ds
005100061017      *
005200061017     d Og143         e ds                  inz
005300061017      *
005400061018      * - Ricerca / Controllo Tabelle
005500061017     d TIBS02ds      e ds                  inz
005600061018      * - Tab. "MFP" = Applicazioni installate c/o P.O.
005700061018     d dMFP          e ds                  inz
005800061018      * - Tab. "MSL" = Layout macchinone smistacolli
005900061018     D dMSL          e ds                  inz
006000061017      *
006100061017     d Status         sds           333
006200061017     d   SDSpgm          *proc
006300061017      *
006400061017     d DSFMT           ds           512
006500061017     d  £Tasto               369    369
006600061017     d  £Nrg                 370    370
006700061017     d  £Ncl                 371    371
006800061017     d  £SFLnrr              378    379b 0
006900061017      *
007000061017     d WLBDAT          ds                  inz
007100061017     d  G02dat                        8  0 inz
007200061017     d  G02inv                        8  0 inz
007300061017     d  G02err                        1    inz('3')
007400061017     d  G02tgi                        4  0 inz
007500061018      *
007600061018     d FIMS41ds        ds                  inz
007700061018     d  MS41fil                       3  0 inz
007800061018     d  MS41dat                       8  0 inz
007900061018     d  MS41ora                       6  0 inz
008000130529     d  MS41mil                            inz   like(TR7mil)
008100130529     d  MS41pd1                            inz   like(TR7pd1)
008200130529     d  MS41pd2                            inz   like(TR7pd2)
008300061017      *
008400061017      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
008500061017      *
008600061017      * - Flag booleani
008700061024     d $First          s              1    inz(*on)
008800061017     d $Fine           s              1    inz(*off)
008900061017     d $InzS01         s              1    inz(*off)
009000061018      *
009100061017      * - Variabili di controllo dspf
009200061017     d S01nrr          s                   like(C01rcd) inz
009300061018      *
009400061018      * Indici di schiera
009500061018     d yy              s              3  0 inz
009600061018      *
009700061017      * - Campi di controllo
009800061018     d Kfil            s                   like(TR7fil) inz
009900061018     d Ktim            s                   like(TR7tim) inz
010000061025     d SAVpos          s                   like(V1Cpos) inz(*loval)
010100061017     d SAVpd1          s                   like(V1Cpd1) inz
010200061017     d SAVpd2          s                   like(V1Cpd2) inz
010300061017     d SAVpep          s                   like(V1Cpep) inz
010400061017     d SAVtim          s                   like(V1Ctim) inz
010500061017     d SAVora          s                   like(V1Cora) inz
010600061017     d SAVapl          s                   like(V1Capl) inz
010700061017     d W1Ctim          s                   like(V1Ctim) inz
010800061017      *
010900061017     d Wmin            s              2  0 inz
011000061018     d wPag            s              4  0 inz
011100061018     d wRig            s              3  0 inz
011200061025     d W_SflPag        s              4  0 inz
011300061017      *
011400061017      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
011500061017      *
011600130530      * - FLTR702L
011700130530     c     K02TR702      klist
011800061018     c                   kfld                    Kfil
011900061018     c                   kfld                    Ktim
012000061018      * - TNTBE01L
012100061018     c     K02TBE01      klist
012200061018     c                   kfld                    TBEcod
012300061018     c                   kfld                    TBEke1
012400061017
012500061018      *---------------------------------------------------------------*
012600061017
012700061017
012800061017      *===============================================================*
012900061017      *  ?I N D I C A T O R I?                                        *
013000061017      *---------------------------------------------------------------*
013100061017      *  10 - Comodo                                                  *
013200061017      *  28 - Emissione messaggio di errore a video                   *
013300061017      *  30 - Ripulisce il subfile                                    *
013400061017      *  31 - NON emette il subfile                                   *
013500061017      *  32 - Record modificato nel subfile (sflnxtchg)               *
013600061025      *  33 - Fine dati nel subfile         (sflend)                  *
013700061025      ***40 - Posizionamento cursore sul campo Opzione              ***
013800061025      *  41 - P.O.         errato                                     *
013900061025      *  42 - Data Spunta  errata                                     *
014000061025      *  43 - Ora  Spunta  errata                                     *
014100061025      *  44 - Applicazione errata                                     *
014200061018      *  90 - Riemissione videata                                     *
014300061018      *===============================================================*
014400061018
014500061017     c     *Entry        plist
014600061017     c                   parm                    KPJBA
014700140305     c                   parm                    fims40ds
014800061017      *
014900140305     c                   exsr      InzS01
015000140305     c* Se ricevuto anche il secondo parametro devo impostare i dati del sfl control
015100140305     c* con i dati ricevuti e caricare il sfl
015200140305     c                   if        %parms>1
015300140305     c                   eval      v1cpos=i40pes
015400140305     c                   eval      v1cpd1=%editc(i40lnp:'X')+%editc(i40lna:'X')+
015500140305     c                             %editc(i40nrs:'X')+%editc(i40nsc:'X')
015600140305     c                   eval      v1ctim=i40dfs
015700140305     c                   clear                   WLBdat
015800140305     c                   eval      G02dat  = V1Ctim
015900140305     c                   call      'XSRDA8'
016000140305     c                   parm                    WLBdat
016100140305     c                   eval      W1Ctim  = G02inv
016200140305     c                   eval      $InzS01 =  *on
016300140305     c                   endif
016400061017      *
016500061017     c                   dow       $Fine   =  *off
016600061017     c                   exsr      GesS01
016700061017     c                   enddo
016800061017      *
016900061017     c                   eval      *inLR   =  *on
017000061024
017100061024      *---------------------------------------------------------------*
017200061024      *?Operazioni iniziali                                          ?*
017300061024      *---------------------------------------------------------------*
017400061024     c     *InzSR        BEGSR
017500061024      *
017600061024      * Reperimento dati aziendali
017700061024     c     *dtaara       define    §azute        AZUTEds
017800061024     c     *dtaara       define    §datiute      dDATIUTE
017900061024     c                   in(E)     *dtaara
018000061024     c                   if        %ERROR or RSUT = *blanks
018100061024     c                   clear                   Tibs34Ds
018200061024     c                   call      'TIBS34R'
018300061024     c                   parm                    Tibs34Ds
018400061024     c                   in        *dtaara
018500061024     c                   endif
018600061114      *
018700061114     c                   eval      T01pgm  =  SDSpgm
018800061024      *
018900061024     c                   ENDSR
019000061017
019100061017      *---------------------------------------------------------------*
019200061018      *?Gestione SubFile S01                                         ?*
019300061017      *---------------------------------------------------------------*
019400061017     c     GesS01        BEGSR
019500061017      *
019600061017      * Inizializzazione videata
019700061018if  1c                   if        $InzS01 =  *on
019800061017     c                   exsr      InzS01
019900061025     c*** non qui:       eval      $InzS01 =  *off
020000061018e   1c                   endif
020100061017      *
020200061017      * Emissione subfile
020300061017     c                   eval      *in30   =  (S01nrr <= *zeros)
020400061017     c                   write     MS40T01
020500061017     c                   write     MS40Z01
020600061025     c                   if            S01nrr  <= *zeros
020700061025     c                             and $InzS01 =  *on
020800061025     c                   write     MS40N01
020900061025     c                   endif
021000061017     c                   exfmt     MS40C01
021100061116     c                   setoff                                       28  90
021200061025     c                   eval      $InzS01 =  *off
021300061017      *
021400061017sel 1c                   SELECT
021500061017w   1c                   WHEN      £Tasto  =  C_RollUp
021600061025if  2c                   if        (W_SflPag + 1) * C_SflPag <= 9999
021700061018     c                   exsr      RollUpS01
021800061025x   2c                   else
021900061025     c                   seton                                        28  90
022000061025     c                   eval      V1Dmsg  = $Msg(10)
022100061025     c*** non serve:     leavesr
022200061025e   2c                   endif
022300061017w   1c                   WHEN      *inKC   =  *on
022400061017     c                   eval      $Fine   =  *on
022500061017x   1c                   OTHER
022600061024      * - Gestione opzioni
022700061024if  2c                   if        S01nrr  >  *zeros
022800061018     c                   exsr      OpzS01
022900061024e   2c                   endif
023000061017     c***                if        *in28
023100061017     c***                leavesr
023200061017     c***                endif
023300061024      * - Controllo variazione selezioni
023400061024if  2c                   if             V1Cpos <> SAVpos
023500061024     c                             or   V1Cpd1 <> SAVpd1
023600061024     c                             or   V1Cpd2 <> SAVpd2
023700061024     c                             or   V1Cpep <> SAVpep
023800061024     c                             or   V1Ctim <> SAVtim
023900061024     c                             or   V1Cora <> SAVora
024000061024     c                             or   V1Capl <> SAVapl
024100061024     c                   exsr      CtrC01
024200061024if  3c                   if        *in90
024300061024     c                   leavesr
024400061024e   3c                   endif
024500061024     c                   eval      $InzS01 =  *on
024600061024     c                   eval      SAVpos  =  V1Cpos
024700061024     c                   eval      SAVpd1  =  V1Cpd1
024800061024     c                   eval      SAVpd2  =  V1Cpd2
024900061024     c                   eval      SAVpep  =  V1Cpep
025000061024     c                   eval      SAVtim  =  V1Ctim
025100061024     c                   eval      SAVora  =  V1Cora
025200061024     c                   eval      SAVapl  =  V1Capl
025300061024e   2c                   endif
025400061017e   1c                   ENDSL
025500061017      *
025600061017     c                   ENDSR
025700061018
025800061018      *---------------------------------------------------------------*
025900061018      *?Controllo SubFile Control C01                                ?*
026000061018      *---------------------------------------------------------------*
026100061018     c     CtrC01        BEGSR
026200061018      *
026300061018      * Reset indicatori
026400061018     c                   movea     *zeros        *in(50)
026500061018      *
026600061018      * P.O. spunta
026700061018if  1c                   if        V1Cpos  = *zeros
026800061025     c                   seton                                        284190
026900061018     c                   eval      V1Dmsg  = $Msg(2)
027000061018     c                   leavesr
027100061018x   1c                   else
027200061018     c     V1Cpos        chain     AZORG
027300061018sel 2c                   select
027400061018w   2c                   when      NOT %found(AZORG01L)
027500061025     c                   seton                                        284190
027600061018     c                   eval      V1Dmsg  = $Msg(6)
027700061018     c                   leavesr
027800061018w   2c                   when      ORGfva <> *blanks
027900061025     c                   seton                                        284190
028000061018     c                   eval      V1Dmsg  = $Msg(3)
028100061018     c                   leavesr
028200061018w   2c                   when           ORGfag <> 'F'
028300061018     c                             and  ORGfag <> 'A'
028400061025     c                   seton                                        284190
028500061018     c                   eval      V1Dmsg  = $Msg(4)
028600061018     c                   leavesr
028700061018e   2c                   endsl
028800061018e   1c                   endif
028900061018      *
029000061018      * Data spunta
029100061018if  1c                   if        V1Ctim <> *zeros
029200061018     c                   clear                   WLBdat
029300061018     c                   eval      G02dat  = V1Ctim
029400061018     c                   call      'XSRDA8'
029500061018     c                   parm                    WLBdat
029600061018if  2c                   if        G02err  = *on
029700061025     c                   seton                                        284290
029800061018     c                   eval      V1Dmsg  = $Msg(7)
029900061018     c                   leavesr
030000061018e   2c                   endif
030100061018     c                   eval      V1Ctim  = G02dat
030200061018     c                   eval      W1Ctim  = G02inv
030300061018e   1c                   endif
030400061018      *
030500061018      * Ora spunta
030600061018sel 1c                   select
030700061018w   1c                   when           V1Ctim = *zeros
030800061018     c                             and  V1Cora > *zeros
030900061025     c                   seton                                        284390
031000061018     c                   eval      V1Dmsg  = $Msg(9)
031100061018     c                   leavesr
031200061018w   1c                   when           V1Cora < *zeros
031300061018     c                             or   V1Cora > 2359
031400061025     c                   seton                                        284390
031500061018     c                   eval      V1Dmsg  = $Msg(8)
031600061018     c                   leavesr
031700061018e   1c                   endsl
031800061018     c                   move      V1Cora        Wmin
031900061018if  1c                   if        Wmin    > 59
032000061025     c                   seton                                        284390
032100061018     c                   eval      V1Dmsg  = $Msg(8)
032200061018     c                   leavesr
032300061018e   1c                   endif
032400061018      *
032500061018      * Ricerca sul campo tipo applicazione
032600061018     c                   if        %scan('?' : V1Capl) > *zeros
032700061018     c                   eval      *in90   = *on
032800061018     c                   clear                   V1Capl
032900061018     c                   clear                   TIBS02ds
033000061018     c                   eval      T02mod  = 'R'
033100061018     c                   eval      T02sif  = KNSIF
033200061018     c                   eval      T02cod  = 'MTP'
033300061018     c                   call      'TIBS02R'
033400061018     c                   parm                    KPJBA
033500061018     c                   parm                    TIBS02ds
033600061018     c                   if        T02err  = *blanks
033700061018     c                   movel     T02ke1        V1Capl
033800061018     c                   endif
033900061018     c                   leavesr
034000061018e   1c                   endif
034100061018      *
034200061018      * Se ho valorizzato V2CAPL controllo che non sia impostata la
034300061018      * postazione (V2CPEP), e che il valido esista nella tabella 'MPT'
034400061018if  1c                   if             V1Capl <> *blanks
034500061018     c                             and  V1Cpep <> *blanks
034600061025     c                   seton                                        284590
034700061018     c                   eval      V1Dmsg  = $Msg(12)
034800061018     c                   leavesr
034900061018e   1c                   endif
035000061018      *
035100061018      * Postazione in tabella "MPT"
035200061018if  1c                   if        V1Capl <> *blanks
035300061018     c                   clear                   TIBS02ds
035400061018     c                   clear                   TIBS02ds
035500061018     c                   eval      T02mod  = 'C'
035600061018     c                   eval      T02sif  = KNSIF
035700061018     c                   eval      T02cod  = 'MTP'
035800061018     c                   movel     V1Capl        T02ke1
035900061018     c                   call      'TIBS02R'
036000061018     c                   parm                    KPJBA
036100061018     c                   parm                    TIBS02ds
036200061018if  2c                   if        T02err <> *blanks
036300061025     c                   seton                                        284490
036400061018     c                   eval      V1Dmsg  = $Msg(11)
036500061018     c                   leavesr
036600061018e   2c                   endif
036700061018e   1c                   endif
036800061018      *
036900061018     c                   ENDSR
037000061017
037100061017      *---------------------------------------------------------------*
037200061018      *?Inizializzazione SubFile S01                                 ?*
037300061017      *---------------------------------------------------------------*
037400061017     c     InzS01        BEGSR
037500061017      *
037600061017      * Pulizia subfile
037700061017     c                   seton                                        3031
037800061017     c                   write     MS40C01
037900061018     c                   setoff                                         3133
038000061017      *
038100061018     c                   clear                   W_SflPag
038200061018     c                   clear                   C01rcd
038300061018     c                   clear                   C01csr
038400061018     c                   clear                   S01nrr
038500061018     c                   clear                   V1Dmsg
038600061018     c                   setoff                                       28  90
038700061018     c                   movea     *zeros        *in(41)
038800061017      *
038900061018if  1c                   if        V1Cpos <= *zeros
039000061018     c                   eval      V1Cpos  = DUTtfp
039100061017if  2c                   if             DUTlpo = '2'
039200061017     c                             or   DUTlpo = *blanks
039300061018     c                   movel     DUTde3        Og143
039400061017if  3c                   if        §OGpt   = 'S'
039500061018     c                   eval      V1Cpos  = DUTpou
039600061017e   3c                   endif
039700061017e   2c                   endif
039800061017e   1c                   endif
039900061017      *
040000061017      * Se inizializzazione iniziale: esco senza caricare nulla
040100061017     c                   if        $InzS01 = *off
040200061017     c                   leavesr
040300061017     c                   endif
040400061017      *
040500061018      * Caricamento tabelle del P.O spunta selezionato in schiera
040600061018     c                   exsr      sr_CarTAB
040700061018      *
040800061018     c                   movel     V1Cpos        Kfil
040900061018     c                   if        V1Ctim <> *zeros
041000061018     c                   eval      Ktim    = %trim( %editw
041100061018     c                                           (W1Ctim:'0        ') )
041200061018     c                                     + %trim( %editw
041300061018     c                                           (V1Cora:'0    ') )
041400061018     c                                     + '00'
041500130530     c     K02TR702      setll     FLTR7000
041600061018     c                   else
041700061018     c     Kfil          setll     FLTR7000
041800061018     c                   endif
041900061018     c     Kfil          reade     FLTR7000
042000061018      *
042100130530do  1c***                DOW       NOT  %eof(FLTR702L)
042200061018     c                   exsr      RollUpS01
042300061025e   1c***                ENDDO
042400061025     *** *
042500061025     *** * Visualizzazione del SFL (se ci sono dati)
042600061025     ***c                   eval      *in30   = (S01nrr = *zeros)
042700061025     *** *
042800061025     *** * Attivazione (eventuale) del SFLEND
042900130530     ***c                   eval      *in33   = (%eof(FLTR702L))
043000061018      *
043100061018     c                   ENDSR
043200061018
043300061018      *---------------------------------------------------------------*
043400061018      *?Caricamento pagina di dati nel SubFile S01                   ?*
043500061018      *---------------------------------------------------------------*
043600061018     c     RollUpS01     BEGSR
043700061018      *
043800061018     c                   eval      S01nrr      = (W_SflPag * C_SflPag)
043900061018     c                   add       1             W_SflPag
044000061018     c                   eval      *in32       = *off
044100061018      *
044200061018      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
044300130530do  1c                   doW       NOT  %eof(FLTR702L)
044400061018     c                             and  S01nrr < (W_SflPag * C_SflPag)
044500061018     c                   exsr      CarS01
044600061018     c     Kfil          reade     FLTR7000
044700061018e   1c                   enddo
044800061025      *
044900061025      * Visualizzazione del SFL (se ci sono dati)
045000061025     c                   eval      *in30   = (S01nrr = *zeros)
045100061025      *
045200061025      * Attivazione (eventuale) del SFLEND
045300130530     c                   eval      *in33   = (%eof(FLTR702L))
045400061025      *
045500061025      * Posizionamento cursore al 1° rcd della pagina
045600061025if  1c                   if        S01nrr  > *zeros
045700061025     c     S01nrr        div       C_SflPag      wPag
045800061025     c                   mvr                     wRig
045900061025     c                   eval      C01rcd  = wPag * C_SflPag
046000061025     c                   if        wRig    > *zeros
046100061025     c                   eval      C01rcd  = C01rcd + 1
046200061025     c                   else
046300061025     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
046400061025     c                   endif
046500061025x   1c                   else
046600061025     c                   clear                   C01rcd
046700061025e   1c                   endif
046800061025     c                   eval      C01csr  = C01rcd
046900061018      *
047000061018     c                   ENDSR
047100061018
047200061018      *---------------------------------------------------------------*
047300061018      *?Caricamento singolo record nel SubFile S01                   ?*
047400061018      *---------------------------------------------------------------*
047500061018     c     CarS01        BEGSR
047600061018      *
047700061018      * Selezione record
047800061018sel 1c                   SELECT
047900061018      * - Data
048000061018w   1c                   WHEN           V1Ctim <> *zeros
048100061018     c                             and  %trim( %editw
048200061018     c                                    (W1Ctim:'0        ') ) <>
048300061018     c                                    %subst(TR7tim:1:8)
048400061018     c     *hival        setgt     FLTR7000
048500061018     c                   leavesr
048600061018      * - Ora
048700061018w   1c                   WHEN           V1Cora <> *zeros
048800061018     c                             and  %trim(%editw(V1Cora:'0    '))
048900061025     c                                          > %subst(TR7tim:9:4)
049000061018     c                   leavesr
049100061018      * - Record "annullato"
049200061018w   1c                   WHEN           TR7cod  = '10001'
049300061018     c                             or   TR7cod  = '10009'
049400061018     c                   leavesr
049500061018e   1c                   ENDSL
049600061018      * - Barcode letto da Scanner Datalogic
049700061018if  1c                   if        V1Cpd1 <> *blanks
049800061018     c                   eval      yy      = %scan( %trimr(V1Cpd1)
049900061018     c                                            : TR7pd1 )
050000061018if  2c                   if        yy      = *zeros
050100061018     c                   leavesr
050200061018e   2c                   endif
050300061018e   1c                   endif
050400061018      * - Barcode letto da Pistola
050500061018if  1c                   if        V1Cpd2 <> *blanks
050600061018     c                   eval      yy      = %scan( %trimr(V1Cpd2)
050700061018     c                                            : TR7pd2 )
050800061018if  2c                   if        yy      = *zeros
050900061018     c                   leavesr
051000061018e   2c                   endif
051100061018e   1c                   endif
051200061018      * - Postazione
051300061018     c                   if        V1Cpep <> *blanks
051400061018     c                   eval      yy      = %scan( %trimr(V1Cpep)
051500061018     c                                            : TR7pep )
051600061018     c                   if        yy      = *zeros
051700061018     c                   leavesr
051800061018     c                   endif
051900061018     c                   endif
052000061018      * - Tipo applicazione
052100061018      *   Se non trovo la postazione in tab. "MSL" considero il record
052200061018      *   valido solo se ho selezionato il tipo applicazione principale
052300061018if  1c                   if        V1Capl <> *blanks
052400061018     c                   eval      yy      = 1
052500061018     c     TR7pep        lookup    $MSLpep(yy)                            10
052600061018if  2c                   if             *in10
052700161025     c                             and  V1Capl <> $MSLapl(yy)
052800161025     c                             and  V1Capl <> $MSLap5(yy)
052900061018     c                   leavesr
053000061018e   2c                   endif
053100061018if  2c                   if        NOT  *in10
053200061018     c                             and  V1Capl <> §MFPapl
053300061018     c                   leavesr
053400061018e   2c                   endif
053500061018e   1c                   endif
053600061018      *
053700061018      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
053800061018     c                   exsr      RieS01
053900061018      *
054000061018      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
054100061018     c                   add       1             S01nrr
054200061018     c                   write     MS40S01
054300061018      *
054400061018     c                   ENDSR
054500061018
054600061018      *---------------------------------------------------------------*
054700061018      *?Riempimento singolo record del SubFile S01                   ?*
054800061018      *---------------------------------------------------------------*
054900061018     c     RieS01        BEGSR
055000061018      *
055100061018     c                   clear                   MS40S01
055200061018      *
055300061018      * - Campi hidden
055400061018     c                   movel     TR7tim        V2Camg
055500061018     c                   reset                   WLBdat
055600061018     c                   eval      G02inv  = V2Camg
055700061018     c                   call      'XSRDA8'
055800061018     c                   parm                    wlbdat
055900061018     c                   eval      V2Cgma  = G02dat
056000061018     c                   eval      V2Cmil  = TR7mil
056100130529     c                   eval      V2Hpd1  = TR7pd1
056200130529     c                   eval      V2Hpd2  = TR7pd2
056300061018      *
056400061018      * - Campi visualizzati nel 1° rec. del sfl
056500061018     c                   movel     V2Cgma        V2Cmg
056600061018     c                   move      TR7tim        V2Chms
056700061018     c                   eval      V2Cctl  = TR7ctl
056800061018     c                   eval      V2Cnfv  = TR7nfv
056900061024     c                   move      TR7pd1        V2Cpd1
057000061024     c                   move      TR7pd2        V2Cpd2
057100061018      * - Campi visualizzati nel 2° rec. del sfl
057200061018     c                   eval      V2Cpep  = TR7pep
057300061018     c                   eval      V2C020  = TR7020
057400061018     c                   eval      V2Cpod  = TR7pod
057500061018     c                   eval      V2Cods  = TR7ods
057600061018     c                   eval      V2Ccrc  = TR7crc
057700061018      *
057800061018     c                   ENDSR
057900061018
058000061018      *---------------------------------------------------------------*
058100061018      *?Gestione singola opzione nel SubFile S01                     ?*
058200061018      *---------------------------------------------------------------*
058300061018     c     OpzS01        BEGSR
058400061018      *
058500061018     c                   readc     MS40S01
058600061018      *
058700061018do  1c                   DOW       NOT  %eof(FIMS40D)
058800061018      *
058900061018     c                   eval      *in32       = *off
059000061018     c                   movea     *zeros        *in(40)
059100061018     c                   z-add     S01nrr        C01rcd
059200061018      *
059300061018sel 2c                   SELECT
059400061018      * - Nessuna opzione
059500061018w   2c                   WHEN      S1Copz      = *blanks
059600061018      * - 5 = Visualizzazione
059700061018w   2c                   WHEN      S1Copz      = '5'
059800061018     c                   exsr      sr_OpzS1_5
059900061018      * - ? = Opzione NON valida
060000061018x   2c***                OTHER
060100061025     c***                seton                                        284090
060200061018     c***                eval      V1Dmsg      = $Msg(??)
060300061018e   2c                   ENDSL
060400061018      *
060500061018      * Aggiornamento sfl
060600061018sel 2c                   select
060700061018w   2c                   when      *in28
060800061018     c                   eval      *in32       = *on
060900061018     c                   z-add     C01rcd        C01csr
061000061018w   2c                   when      *in90       = *on
061100061018     c                   z-add     C01rcd        C01csr
061200061018     c                   clear                   S1Copz
061300061018x   2c                   other
061400061018     c                   clear                   S1Copz
061500061018e   2c                   endsl
061600061018     c                   UPDATE    MS40S01
061700061018if  2c                   if        *in28  OR  *in90
061800061018     c                   leave
061900061018e   2c                   endif
062000061018      *
062100061018     c                   readc     MS40S01
062200061018      *
062300061018e   1c                   ENDDO
062400061018      *
062500061018     c                   ENDSR
062600061018
062700061018      *---------------------------------------------------------------*
062800061018      *?Visualizzazione singola lettura                              ?*
062900061018      *---------------------------------------------------------------*
063000061018     c     sr_OpzS1_5    BEGSR
063100061018      *
063200061018     c                   clear                   FIMS41ds
063300061018     c                   eval      MS41fil = V1Cpos
063400061018     c                   eval      MS41dat = V2Camg
063500061018     c                   eval      MS41ora = V2Chms
063600061018     c                   eval      MS41mil = V2Cmil
063700130529     c                   eval      MS41pd1 = V2Hpd1
063800130529     c                   eval      MS41pd2 = V2Hpd2
063900061018      *
064000061018     c                   movel(p)  FIMS41ds      KPJBU
064100061018      *
064200061018     c                   call      'FIMS41R'
064300061018     c                   parm                    KPJBA
064400061018      *
064500061018     c                   ENDSR
064600061018      *---------------------------------------------------------------*
064700061018      *?Caricamento Tabelle occorrenti                               ?*
064800061018      *---------------------------------------------------------------*
064900061018     c     sr_CarTAB     BEGSR
065000061018      *
065100061018      * Tab. "MSL" = Layout macchinone smistacolli
065200061018     c                   clear                   yy
065300061018     c                   clear                   dMSL
065400061018     c                   eval      TBEcod  = 'MSL'
065500061018     c                   movel(p)  V1Cpos        TBEke1
065600061018     c     K02TBE01      setll     TNTBE000
065700061018     c     K02TBE01      reade     TNTBE000
065800061018do  1c                   dow       NOT %eof(TNTBE01L)
065900061018if  2c                   if        TBEatb  = *blanks
066000061018     c                   add       1             yy
066100061018     c                   movel     TBEuni        dMSL
066200061018     c                   eval      $MSLpep(yy) = §MSLpep
066300061018     c                   eval      $MSLapl(yy) = §MSLapl
066400161025     c                   eval      $MSLap5(yy) = §MSLap5
066500061018e   2c                   endif
066600061018     c     K02TBE01      reade     TNTBE000
066700061018e   1c                   enddo
066800061018      *
066900061018      * Tipo applicazione principale del P.O.
067000061018     c                   eval      TBEcod  = 'MFP'
067100061018     c                   movel(p)  V1Cpos        TBEke1
067200061018     c     K02TBE01      chain     TNTBE000
067300061018if  1c                   if             %found(TNTBE01L)
067400061018     c                             and  TBEatb = *blanks
067500061018     c                   movel     TBEuni        dMFP
067600021206     c                   endif
067700021206      *
067800021206     c                   ENDSR
067900010608
068000061018      *---------------------------------------------------------------*
068100061018
068200061018** - $MSG -------------------------------------------------------------------*
068300061025     L I B E R O                                                                1
068400061018Inserire Punto Operativo                                                        2
068500061018Punto Operativo annullato                                                       3
068600061018Punto Operativo non valido                                                      4
068700061025     L I B E R O                                                                5
068800061018Codice inesistente                                                              6
068900061018Data errata                                                                     7
069000061018Ora formalmente errata                                                          8
069100061018Non è possibile inserire un orario senza data                                   9
069200061018Ampiezza massima subfile superata                                              10
069300061018Applicazione errata                                                            11
069400061018Postazione/Applicazione sono in alternativa                                    12
