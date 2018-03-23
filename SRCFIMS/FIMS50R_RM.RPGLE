000100061017      *---------------------------------------------------------------*
000200070306      *?FIMS50R - Interrogazione Log spunte da PDA                   ?*
000300061017      *---------------------------------------------------------------*
000400061017
000500061017     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600061017
000700061017      *---------------------------------------------------------------*
000800980623
000900070306     fFipsl01l  if   e           k disk
001000080117     f                                     infds(fipslnrr)
001100061017     fAZORG01L  if   e           k disk
001200070608     fTabel00f  if   e           k disk
001300061017      *
001400070306     fFIMS50D   cf   e             workstn
001500070306     f                                     sfile(MS50S01:S01nrr)
001600061017     f                                     infds(DSFMT)
001700061017
001800980521      *------------------------------------------------------------------------*
001900061017
002000061017      *
002100061017      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002200061017      *
002300061018      * - Tasti di funzione
002400061017     d C_RollUp        c                   const(x'F5')
002500061018      * - Nr. di righe del sfl (SFLPAG)
002600070306     d C_SflPag        c                   const(14)
002700061017      *
002800061017      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
002900061017      *
003000061017      * - Messaggi di errore
003100061018     d $Msg            s             78    dim(12)  ctdata perrcd(1)
003200070608     D AN              S              1    dim(30)
003300070608     D AN_D            S              7    dim(30)
003400070608     d n               s              3  0 inz
003500061017      *
003600061017      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
003700061017      *
003800061017     d KPJBA         e ds
003900061017      *
004000061017      * - Parametri per richiamo al pgm di controllo profilo utenti
004100061017     d Tibs34Ds      e ds                  inz
004200061017      * - Ds di riferimento al file esterno AzUte00F
004300061017     d AzuteDs       e ds                  ExtName(AzUte00F)
004400061017      * - Ds per dati organigramma
004500061017     d DDatiUte      e ds
004600061017      *
004700061018      * - Ricerca / Controllo Tabelle
004800061017     d TIBS02ds      e ds                  inz
004900061017      *
005000070608     d ds3e          e ds
005100070608
005200061017     d Status         sds           333
005300061017     d   SDSpgm          *proc
005400061017      *
005500061017     d DSFMT           ds           512
005600061017     d  £Tasto               369    369
005700061017     d  £Nrg                 370    370
005800061017     d  £Ncl                 371    371
005900061017     d  £SFLnrr              378    379b 0
006000061017      *
006100061017     d WLBDAT          ds                  inz
006200061017     d  G02dat                        8  0 inz
006300061017     d  G02inv                        8  0 inz
006400061017     d  G02err                        1    inz('3')
006500061017     d  G02tgi                        4  0 inz
006600061018      *
006700070608     d FIMS51ds        ds                  inz
006800080117     d  Ms51nrr                       7  0 inz
006900080117     d* MS51fil                       3  0 inz
007000080117     d* MS51dat                       8  0 inz
007100080117     d* MS51ora                       6  0 inz
007200080117     d* MS51mil                       3    inz
007300080117     d* MS51iddisp                   35    inz
007400080117
007500080117     D Fipslnrr        DS
007600080117     D  pslnrr               397    400B 0
007700061017      *
007800061017      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
007900061017      *
008000061017      * - Flag booleani
008100061017     d $Fine           s              1    inz(*off)
008200061017     d $InzS01         s              1    inz(*off)
008300061018      *
008400061017      * - Variabili di controllo dspf
008500061017     d S01nrr          s                   like(C01rcd) inz
008600061018      *
008700061018      * Indici di schiera
008800061018     d yy              s              3  0 inz
008900061018      *
009000061017      * - Campi di controllo
009100070608     d Kfil            s                   like(pslfil) inz
009200070608     d Ktim            s                   like(psldtorar) inz
009300061025     d SAVpos          s                   like(V1Cpos) inz(*loval)
009400070608     d SAVdtr          s                   like(V1Cdtr) inz
009500070611     d w1cdtr          s                   like(V1Cdtr) inz
009600070608     d SAVhrr          s                   like(V1Chrr) inz
009700070608     d SAVbar          s                   like(V1Cbar) inz
009800070608     d SAViddisp       s                   like(V1Ciddisp) inz
009900070608     D cod             S                   LIKE(tblcod)
010000070608     D codut           S                   LIKE(tblkut) inz(1)
010100061017      *
010200061017     d Wmin            s              2  0 inz
010300061018     d wPag            s              4  0 inz
010400061018     d wRig            s              3  0 inz
010500061025     d W_SflPag        s              4  0 inz
010600080201     d sav_nrr         s                   like(C01rcd) inz
010700061017      *
010800061017      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
010900061017      *
011000070608      * - FIPSL01L
011100070608     c     K02PSL01      klist
011200070608     c                   kfld                    Kfil
011300070608     c                   kfld                    Ktim
011400070608      * - Tabel00f
011500070608     c     kTAB2         klist
011600070608     c                   kfld                    codut
011700070608     c                   kfld                    cod
011800061017
011900061018      *---------------------------------------------------------------*
012000061017
012100061017
012200061017      *===============================================================*
012300061017      *  ?I N D I C A T O R I?                                        *
012400061017      *---------------------------------------------------------------*
012500061017      *  10 - Comodo                                                  *
012600061017      *  28 - Emissione messaggio di errore a video                   *
012700061017      *  30 - Ripulisce il subfile                                    *
012800061017      *  31 - NON emette il subfile                                   *
012900061017      *  32 - Record modificato nel subfile (sflnxtchg)               *
013000061025      *  33 - Fine dati nel subfile         (sflend)                  *
013100061025      ***40 - Posizionamento cursore sul campo Opzione              ***
013200061025      *  41 - P.O.         errato                                     *
013300061025      *  42 - Data Spunta  errata                                     *
013400061025      *  43 - Ora  Spunta  errata                                     *
013500061025      *  44 - Applicazione errata                                     *
013600061018      *  90 - Riemissione videata                                     *
013700061018      *===============================================================*
013800061018
013900061017     c     *Entry        plist
014000061017     c                   parm                    KPJBA
014100061017      *
014200061017     c                   exsr      InzS01
014300061017      *
014400061017     c                   dow       $Fine   =  *off
014500061017     c                   exsr      GesS01
014600061017     c                   enddo
014700061017      *
014800061017     c                   eval      *inLR   =  *on
014900061024
015000061024      *---------------------------------------------------------------*
015100061024      *?Operazioni iniziali                                          ?*
015200061024      *---------------------------------------------------------------*
015300061024     c     *InzSR        BEGSR
015400061024      *
015500061024      * Reperimento dati aziendali
015600061024     c     *dtaara       define    §azute        AZUTEds
015700061024     c     *dtaara       define    §datiute      dDATIUTE
015800061024     c                   in(E)     *dtaara
015900061024     c                   if        %ERROR or RSUT = *blanks
016000061024     c                   clear                   Tibs34Ds
016100061024     c                   call      'TIBS34R'
016200061024     c                   parm                    Tibs34Ds
016300061024     c                   in        *dtaara
016400061024     c                   endif
016500061114      *
016600061114     c                   eval      T01pgm  =  SDSpgm
016700061024      *
016800061024     c                   ENDSR
016900061017
017000061017      *---------------------------------------------------------------*
017100061018      *?Gestione SubFile S01                                         ?*
017200061017      *---------------------------------------------------------------*
017300061017     c     GesS01        BEGSR
017400061017      *
017500061017      * Inizializzazione videata
017600061018if  1c                   if        $InzS01 =  *on
017700061017     c                   exsr      InzS01
017800061018e   1c                   endif
017900080201
018000080201     c                   clear                   sav_nrr
018100061017      *
018200061017      * Emissione subfile
018300061017     c                   eval      *in30   =  (S01nrr <= *zeros)
018400070608     c                   write     MS50T01
018500070608     c                   write     MS50Z01
018600061025     c                   if            S01nrr  <= *zeros
018700061025     c                             and $InzS01 =  *on
018800070608     c                   write     MS50N01
018900061025     c                   endif
019000070608     c                   exfmt     MS50C01
019100061116     c                   setoff                                       28  90
019200061025     c                   eval      $InzS01 =  *off
019300061017      *
019400061017sel 1c                   SELECT
019500061017w   1c                   WHEN      £Tasto  =  C_RollUp
019600061025if  2c                   if        (W_SflPag + 1) * C_SflPag <= 9999
019700061018     c                   exsr      RollUpS01
019800061025x   2c                   else
019900061025     c                   seton                                        28  90
020000061025     c                   eval      V1Dmsg  = $Msg(10)
020100061025e   2c                   endif
020200061017w   1c                   WHEN      *inKC   =  *on
020300061017     c                   eval      $Fine   =  *on
020400061017x   1c                   OTHER
020500061024      * - Gestione opzioni
020600061024if  2c                   if        S01nrr  >  *zeros
020700061018     c                   exsr      OpzS01
020800061024e   2c                   endif
020900061024      * - Controllo variazione selezioni
021000061024if  2c                   if             V1Cpos <> SAVpos
021100070608     c                             or   V1Cbar <> SAVbar
021200070608     c                             or   V1Cdtr <> SAVdtr
021300070608     c                             or   V1Chrr <> SAVhrr
021400070608     c                             or   V1Ciddisp<>saviddisp
021500061024     c                   exsr      CtrC01
021600061024if  3c                   if        *in90
021700061024     c                   leavesr
021800061024e   3c                   endif
021900071031     c****               eval      $InzS01 =  *on
022000061024     c                   eval      SAVpos  =  V1Cpos
022100070608     c                   eval      SAVdtr  =  V1Cdtr
022200070608     c                   eval      SAVhrr  =  V1Chrr
022300070608     c                   eval      SAVbar  =  V1Cbar
022400070608     c                   eval      SAViddisp = v1ciddisp
022500080201     c                   else
022600080201     c                   z-add     s01nrr        sav_nrr
022700061024e   2c                   endif
022800071031     c                   eval      $InzS01 =  *on
022900061017e   1c                   ENDSL
023000061017      *
023100061017     c                   ENDSR
023200061018
023300061018      *---------------------------------------------------------------*
023400061018      *?Controllo SubFile Control C01                                ?*
023500061018      *---------------------------------------------------------------*
023600061018     c     CtrC01        BEGSR
023700061018      *
023800061018      * Reset indicatori
023900061018     c                   movea     *zeros        *in(50)
024000061018      *
024100061018      * P.O. spunta
024200061018if  1c                   if        V1Cpos  = *zeros
024300061025     c                   seton                                        284190
024400061018     c                   eval      V1Dmsg  = $Msg(2)
024500061018     c                   leavesr
024600061018x   1c                   else
024700061018     c     V1Cpos        chain     AZORG
024800061018sel 2c                   select
024900061018w   2c                   when      NOT %found(AZORG01L)
025000061025     c                   seton                                        284190
025100061018     c                   eval      V1Dmsg  = $Msg(6)
025200061018     c                   leavesr
025300061018w   2c                   when      ORGfva <> *blanks
025400061025     c                   seton                                        284190
025500061018     c                   eval      V1Dmsg  = $Msg(3)
025600061018     c                   leavesr
025700061018w   2c                   when           ORGfag <> 'F'
025800061018     c                             and  ORGfag <> 'A'
025900061025     c                   seton                                        284190
026000061018     c                   eval      V1Dmsg  = $Msg(4)
026100061018     c                   leavesr
026200061018e   2c                   endsl
026300061018e   1c                   endif
026400061018      *
026500061018      * Data spunta
026600070608if  1c                   if        V1Cdtr <> *zeros
026700061018     c                   clear                   WLBdat
026800070608     c                   eval      G02dat  = V1Cdtr
026900061018     c                   call      'XSRDA8'
027000061018     c                   parm                    WLBdat
027100061018if  2c                   if        G02err  = *on
027200061025     c                   seton                                        284290
027300061018     c                   eval      V1Dmsg  = $Msg(7)
027400061018     c                   leavesr
027500061018e   2c                   endif
027600070608     c                   eval      V1Cdtr  = G02dat
027700070608     c                   eval      W1Cdtr  = G02inv
027800061018e   1c                   endif
027900061018      *
028000061018      * Ora spunta
028100061018sel 1c                   select
028200070608w   1c                   when           V1Cdtr = *zeros
028300070608     c                             and  V1Chrr > *zeros
028400061025     c                   seton                                        284390
028500061018     c                   eval      V1Dmsg  = $Msg(9)
028600061018     c                   leavesr
028700070608w   1c                   when           V1Chrr < *zeros
028800070608     c                             or   V1Chrr > 2359
028900061025     c                   seton                                        284390
029000061018     c                   eval      V1Dmsg  = $Msg(8)
029100061018     c                   leavesr
029200061018e   1c                   endsl
029300070608     c                   move      V1Chrr        Wmin
029400061018if  1c                   if        Wmin    > 59
029500061025     c                   seton                                        284390
029600061018     c                   eval      V1Dmsg  = $Msg(8)
029700061018     c                   leavesr
029800061018e   1c                   endif
029900061018      *
030000061018     c                   ENDSR
030100061017
030200061017      *---------------------------------------------------------------*
030300061018      *?Inizializzazione SubFile S01                                 ?*
030400061017      *---------------------------------------------------------------*
030500061017     c     InzS01        BEGSR
030600061017      *
030700061017      * Pulizia subfile
030800061017     c                   seton                                        3031
030900070604     c                   write     MS50C01
031000061018     c                   setoff                                         3133
031100061017      *
031200061018     c                   clear                   W_SflPag
031300061018     c                   clear                   C01rcd
031400061018     c                   clear                   C01csr
031500061018     c                   clear                   S01nrr
031600061018     c                   clear                   V1Dmsg
031700061018     c                   setoff                                       28  90
031800061018     c                   movea     *zeros        *in(41)
031900061017      *
032000061018if  1c                   if        V1Cpos <= *zeros
032100061018     c                   eval      V1Cpos  = DUTtfp
032200061017e   1c                   endif
032300061017      *
032400061017      * Se inizializzazione iniziale: esco senza caricare nulla
032500061017     c                   if        $InzS01 = *off
032600061017     c                   leavesr
032700061017     c                   endif
032800061017      *
032900061018      * Caricamento tabelle del P.O spunta selezionato in schiera
033000061018     c                   exsr      sr_CarTAB
033100061018      *
033200061018     c                   movel     V1Cpos        Kfil
033300070608     c                   if        V1Cdtr <> *zeros
033400061018     c                   eval      Ktim    = %trim( %editw
033500070608     c                                           (W1Cdtr:'0        ') )
033600061018     c                                     + %trim( %editw
033700070611     c                                           (V1Chrr:'0    ') )
033800061018     c                                     + '00'
033900070608     c     K02psl01      setll     FIPSL000
034000061018     c                   else
034100070608     c     Kfil          setll     FIPSL000
034200061018     c                   endif
034300070608     c     Kfil          reade     FIPSL000
034400061018      *
034500061018     c                   exsr      RollUpS01
034600061018      *
034700061018     c                   ENDSR
034800061018
034900061018      *---------------------------------------------------------------*
035000061018      *?Caricamento pagina di dati nel SubFile S01                   ?*
035100061018      *---------------------------------------------------------------*
035200061018     c     RollUpS01     BEGSR
035300061018      *
035400061018     c                   eval      S01nrr      = (W_SflPag * C_SflPag)
035500061018     c                   add       1             W_SflPag
035600061018     c                   eval      *in32       = *off
035700061018      *
035800061018      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
035900070608do  1c                   doW       NOT  %eof(FIPSL01L)
036000061018     c                             and  S01nrr < (W_SflPag * C_SflPag)
036100061018     c                   exsr      CarS01
036200070608     c     Kfil          reade     FIPSL000
036300061018e   1c                   enddo
036400061025      *
036500061025      * Visualizzazione del SFL (se ci sono dati)
036600061025     c                   eval      *in30   = (S01nrr = *zeros)
036700061025      *
036800061025      * Attivazione (eventuale) del SFLEND
036900070608     c                   eval      *in33   = (%eof(FIPSL01L))
037000061025      *
037100061025      * Posizionamento cursore al 1° rcd della pagina
037200061025if  1c                   if        S01nrr  > *zeros
037300061025     c     S01nrr        div       C_SflPag      wPag
037400061025     c                   mvr                     wRig
037500061025     c                   eval      C01rcd  = wPag * C_SflPag
037600061025     c                   if        wRig    > *zeros
037700061025     c                   eval      C01rcd  = C01rcd + 1
037800061025     c                   else
037900061025     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
038000061025     c                   endif
038100061025x   1c                   else
038200061025     c                   clear                   C01rcd
038300061025e   1c                   endif
038400061025     c                   eval      C01csr  = C01rcd
038500080201     c                   if        sav_nrr>0
038600080201     c                   eval      c01csr=sav_nrr
038700080201     c                   eval      c01rcd=sav_nrr
038800080201     c                   endif
038900061018      *
039000061018     c                   ENDSR
039100061018
039200061018      *---------------------------------------------------------------*
039300061018      *?Caricamento singolo record nel SubFile S01                   ?*
039400061018      *---------------------------------------------------------------*
039500061018     c     CarS01        BEGSR
039600061018      *
039700061018      * Selezione record
039800061018sel 1c                   SELECT
039900061018      * - Data
040000070608w   1c                   WHEN           V1Cdtr <> *zeros
040100061018     c                             and  %trim( %editw
040200070608     c                                    (W1Cdtr:'0        ') ) <>
040300070608     c                                    %subst(psldtorar:1:8)
040400070608     c     *hival        setgt     FIPSL000
040500061018     c                   leavesr
040600061018      * - Ora
040700070611w   1c                   WHEN           V1Chrr <> *zeros
040800070611     c                             and  %trim(%editw(V1Chrr:'0    '))
040900070608     c                                          > %subst(psldtorar:9:4)
041000061018     c                   leavesr
041100061018e   1c                   ENDSL
041200070608     c* - parzializzazione per barcode
041300070608    1c                   if        v1cbar<>*blanks
041400070608     c                   eval      yy      = %scan( %trimr(V1Cbar)
041500070608     c                                            : pslbarco)
041600070608if  2c                   if        yy      = *zeros
041700070608     c                   leavesr
041800070608e   2c                   endif
041900070608    1c                   endif
042000070608     c* - parzializzazione per id-dispositivo
042100070608     c                   if        v1ciddisp<>*blanks and v1ciddisp<>
042200070608     c                             psliddisp
042300070608     c                   leavesr
042400070608     c                   endif
042500080121     c* - parzializzazione per solo errori
042600080121     c                   if        v1cerr='S' and pslerr=*blanks
042700080121     c                   leavesr
042800080121     c                   endif
042900061018      *
043000061018      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
043100061018     c                   exsr      RieS01
043200061018      *
043300061018      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
043400061018     c                   add       1             S01nrr
043500070608     c                   write     MS50S01
043600061018      *
043700061018     c                   ENDSR
043800061018
043900061018      *---------------------------------------------------------------*
044000061018      *?Riempimento singolo record del SubFile S01                   ?*
044100061018      *---------------------------------------------------------------*
044200061018     c     RieS01        BEGSR
044300061018      *
044400070608     c                   clear                   MS50S01
044500061018      *
044600061018      * - Campi hidden
044700070608     c                   movel     psldtorar     V2Camg
044800061018     c                   reset                   WLBdat
044900061018     c                   eval      G02inv  = V2Camg
045000061018     c                   call      'XSRDA8'
045100061018     c                   parm                    wlbdat
045200061018     c                   eval      V2Cgma  = G02dat
045300070608     c                   eval      V2Cmil  = pslmilles
045400070611     c                   eval      v2ciddisp=psliddisp
045500080117     c                   z-add     pslnrr        s1cnrr
045600061018      *
045700070608      * - Campi visualizzati
045800070608     c                   movel     V2Cgma        s1cdtr
045900070608     c                   move      psldtorar     s1chrr
046000070611     c                   eval      s1cnpg  = pslnpg
046100070611     c                   move      pslnfv        s1cnfv
046200070608     c                   eval      s1cbar  = pslbarco
046300070611     c                   eval      s1cpru  = pslusr
046400070608     c                   eval      s1cman  = pslman
046500070608     c* anomalia --> visualizzo la decodifica
046600070608     c                   clear                   s1canm
046700070608     c                   eval      n = %lookup(pslcan:an)
046800070608     c                   if        n>0
046900070608     c                   eval      s1canm=an_D(n)
047000070608     c                   endif
047100070608     c                   eval      s1cdup  = psldup
047200070608     c                   eval      s1cerr  = pslerr
047300061018      *
047400061018     c                   ENDSR
047500061018
047600061018      *---------------------------------------------------------------*
047700061018      *?Gestione singola opzione nel SubFile S01                     ?*
047800061018      *---------------------------------------------------------------*
047900061018     c     OpzS01        BEGSR
048000061018      *
048100070608     c                   readc     MS50S01
048200061018      *
048300070608do  1c                   DOW       NOT  %eof(FIMS50D)
048400061018      *
048500061018     c                   eval      *in32       = *off
048600061018     c                   movea     *zeros        *in(40)
048700061018     c                   z-add     S01nrr        C01rcd
048800061018      *
048900061018sel 2c                   SELECT
049000061018      * - Nessuna opzione
049100061018w   2c                   WHEN      S1Copz      = *blanks
049200061018      * - 5 = Visualizzazione
049300061018w   2c                   WHEN      S1Copz      = '5'
049400061018     c                   exsr      sr_OpzS1_5
049500061018      * - ? = Opzione NON valida
049600061018x   2c***                OTHER
049700061025     c***                seton                                        284090
049800061018     c***                eval      V1Dmsg      = $Msg(??)
049900061018e   2c                   ENDSL
050000061018      *
050100061018      * Aggiornamento sfl
050200061018sel 2c                   select
050300061018w   2c                   when      *in28
050400061018     c                   eval      *in32       = *on
050500061018     c                   z-add     C01rcd        C01csr
050600061018w   2c                   when      *in90       = *on
050700061018     c                   z-add     C01rcd        C01csr
050800061018     c                   clear                   S1Copz
050900061018x   2c                   other
051000061018     c                   clear                   S1Copz
051100061018e   2c                   endsl
051200070608     c                   UPDATE    MS50S01
051300061018if  2c                   if        *in28  OR  *in90
051400061018     c                   leave
051500061018e   2c                   endif
051600061018      *
051700070608     c                   readc     MS50S01
051800061018      *
051900061018e   1c                   ENDDO
052000061018      *
052100061018     c                   ENDSR
052200061018
052300061018      *---------------------------------------------------------------*
052400061018      *?Visualizzazione singola lettura                              ?*
052500061018      *---------------------------------------------------------------*
052600061018     c     sr_OpzS1_5    BEGSR
052700061018      *
052800070611     c                   clear                   FIMS51ds
052900080117     c*                  eval      MS51fil = V1Cpos
053000080117     c*                  eval      MS51dat = V2Camg
053100080117     c*                  eval      MS51ora = s1chrr
053200080117     c*                  eval      MS51mil = V2Cmil
053300080117     c*                  eval      MS51iddisp = V2Ciddisp
053400080117     c                   z-add     s1cnrr        ms51nrr
053500070608      *
053600070611     c                   movel(p)  FIMS51ds      KPJBU
053700070608      *
053800070611     c                   call      'FIMS51R'
053900070611     c                   parm                    KPJBA
054000061018      *
054100061018     c                   ENDSR
054200061018      *---------------------------------------------------------------*
054300061018      *?Caricamento Tabelle occorrenti                               ?*
054400061018      *---------------------------------------------------------------*
054500061018     c     sr_CarTAB     BEGSR
054600061018      *
054700070608     c* carico la tabella delle anomalie spunte che mando ai PDA
054800070608     c                   z-add     1             yy
054900070608     c                   movel     '3E'          cod
055000070611     c     ktab2         chain     tabel                              10
055100070611     c     *in10         doweq     *off
055200070608     c                   movel     tbluni        ds3e
055300070608     c     §3epda        ifne      'N'
055400070608     C                   MOVEL     TBLKEY        AN(yy)
055500070608     C                   MOVEL     §3edes        AN_D(yy)
055600070608     C                   ADD       1             yy
055700070608     C                   ENDIF
055800070608     c**
055900070611     c     ktab2         reade     tabel                                  10
056000070608     C                   ENDDO
056100021206      *
056200021206     c                   ENDSR
056300010608
056400061018      *---------------------------------------------------------------*
056500061018
056600061018** - $MSG -------------------------------------------------------------------*
056700061025     L I B E R O                                                                1
056800061018Inserire Punto Operativo                                                        2
056900061018Punto Operativo annullato                                                       3
057000061018Punto Operativo non valido                                                      4
057100061025     L I B E R O                                                                5
057200061018Codice inesistente                                                              6
057300061018Data errata                                                                     7
057400061018Ora formalmente errata                                                          8
057500061018Non è possibile inserire un orario senza data                                   9
057600061018Ampiezza massima subfile superata                                              10
057700070608     L I B E R O                                                               11
057800070608     L I B E R O                                                               12
