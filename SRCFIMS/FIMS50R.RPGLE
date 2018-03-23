000100061017      *---------------------------------------------------------------*
000200070306      *?FIMS50R - Interrogazione Log spunte da PDA                   ?*
000300061017      *---------------------------------------------------------------*
000400061017
000500061017     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600061017
000700061017      *---------------------------------------------------------------*
000800980623
000900101213     fFipsl01l  if   e           k disk    usropn extfile(wlibfile01)
001000100730     f                                     infds(FipslNrr)
001100101213     fFipsl02l  if   e           k disk    usropn extfile(wlibfile02)
001200100730     f                                     rename(Fipsl000:Fipsl002)
001300100730     f                                     infds(Fipslnrr2)
001400061017     fAZORG01L  if   e           k disk
001500070608     fTabel00f  if   e           k disk
001600061017      *
001700070306     fFIMS50D   cf   e             workstn
001800070306     f                                     sfile(MS50S01:S01nrr)
001900061017     f                                     infds(DSFMT)
002000061017
002100980521      *------------------------------------------------------------------------*
002200061017
002300061017      *
002400061017      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002500061017      *
002600061018      * - Tasti di funzione
002700061017     d C_RollUp        c                   const(x'F5')
002800061018      * - Nr. di righe del sfl (SFLPAG)
002900070306     d C_SflPag        c                   const(14)
002901101213      // - Libreria/file per ovrdbf FIPSL01L
002902101213     d c_libfile01     c                   const('FILTRA201/FIPSL01L')
002903101213     d c_libfile01p    c                   const('FILTRAPRD/FIPSL01L')
002904101213      // - Libreria/file per ovrdbf FIPSL02L
002905101213     d c_libfile02     c                   const('FILTRA201/FIPSL02L')
002906101213     d c_libfile02p    c                   const('FILTRAPRD/FIPSL02L')
003000061017      *
003100061017      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
003200061017      *
003300061017      * - Messaggi di errore
003400061018     d $Msg            s             78    dim(12)  ctdata perrcd(1)
003500100730     D $AN             S              1    dim(30)
003600100730     D $AN_D           S              7    dim(30)
003700061017      *
003800061017      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
003900061017      *
004000061017     d KPJBA         e ds
004100061017      *
004200061017      * - Parametri per richiamo al pgm di controllo profilo utenti
004300061017     d Tibs34Ds      e ds                  inz
004400061017      * - Ds di riferimento al file esterno AzUte00F
004500061017     d AzuteDs       e ds                  ExtName(AzUte00F)
004600061017      * - Ds per dati organigramma
004700061017     d DDatiUte      e ds
004800061017      *
004900061018      * - Ricerca / Controllo Tabelle
005000061017     d TIBS02ds      e ds                  inz
005100061017      *
005200070608     d ds3e          e ds
005300100730      *
005400100730      * - Status ds
005500061017     d Status         sds           333
005600061017     d   SDSpgm          *proc
005700061017      *
005800100730      * - InfDs del *dspf FIMS50D
005900100730     d DSfmt           ds           512
006000061017     d  £Tasto               369    369
006100061017     d  £Nrg                 370    370
006200061017     d  £Ncl                 371    371
006300061017     d  £SFLnrr              378    379b 0
006400100730      *
006500100730      * - InfDs del file FIPSL01L
006600100730     d FipslNrr        ds
006700100730     d  PSLnrr               397    400b 0
006800100730      *
006900100730      * - InfDs del file FIPSL02L
007000100730     d FipslNrr2       ds
007100100730     d  PSLnrr2              397    400b 0
007200061017      *
007300100730      * - Parametri per *pgm XSRDA8 (controllo/inversione date)
007400100730     d WLBdat          ds                  inz
007500061017     d  G02dat                        8  0 inz
007600061017     d  G02inv                        8  0 inz
007700061017     d  G02err                        1    inz('3')
007800061017     d  G02tgi                        4  0 inz
007900061018      *
008000100730      * - Parametri per *pgm FIMS51R (Interrogazione Log spunte da PDA)
008100070608     d FIMS51ds        ds                  inz
008200100730     d  MS51nrr                       7  0 inz
008300061017      *
008400061017      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
008500061017      *
008600061017      * - Flag booleani
008700061017     d $Fine           s              1    inz(*off)
008800061017     d $InzS01         s              1    inz(*off)
008900061018      *
009000061017      * - Variabili di controllo dspf
009100061017     d S01nrr          s                   like(C01rcd) inz
009200061018      *
009300061018      * Indici di schiera
009400061018     d yy              s              3  0 inz
009500100730     d xn              s              3  0 inz
009600061018      *
009700061017      * - Campi di controllo
009800100730     d Kfil            s                   like(PSLfil)    inz
009900100730     d Ktim            s                   like(PSLdtorar) inz
010000100730     d SAVpos          s                   like(V1Cpos)    inz(*loval)
010100100730     d SAVdtr          s                   like(V1Cdtr)    inz
010200100730     d SAVhrr          s                   like(V1Chrr)    inz
010300100730     d SAVbar          s                   like(V1Cbar)    inz
010400100805     d SAVpru          s                   like(V1Cpru)    inz
010500070608     d SAViddisp       s                   like(V1Ciddisp) inz
010600100730     d SAVerr          s                   like(V1Cerr)    inz
010700100730     d W1cdtr          s                   like(V1Cdtr)    inz
010800100730     d Cod             s                   like(TBLcod)
010900100730     d CodUt           s                   like(TBLkut)    inz(1)
011000061017      *
011100100730      * - Campi di comodo
011200100730     d wMin            s              2  0 inz
011300061018     d wPag            s              4  0 inz
011400061018     d wRig            s              3  0 inz
011500100730     d w_SflPag        s              4  0 inz
011501101213     d wlibfile01      s             21    inz
011502101213     d wlibfile02      s             21    inz
011600061017      *
011700061017      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
011800061017      *
011900070608      * - FIPSL01L
012000070608     c     K02PSL01      klist
012100070608     c                   kfld                    Kfil
012200070608     c                   kfld                    Ktim
012300100730      * - TABEL00F
012400070608     c     kTAB2         klist
012500100730     c                   kfld                    CodUt
012600100730     c                   kfld                    Cod
012700061017
012800061018      *---------------------------------------------------------------*
012900061017
013000061017
013100061017      *===============================================================*
013200061017      *  ?I N D I C A T O R I?                                        *
013300061017      *---------------------------------------------------------------*
013400061017      *  10 - Comodo                                                  *
013500061017      *  28 - Emissione messaggio di errore a video                   *
013600061017      *  30 - Ripulisce il subfile                                    *
013700061017      *  31 - NON emette il subfile                                   *
013800061017      *  32 - Record modificato nel subfile (sflnxtchg)               *
013900061025      *  33 - Fine dati nel subfile         (sflend)                  *
014000061025      ***40 - Posizionamento cursore sul campo Opzione              ***
014100061025      *  41 - P.O.         errato                                     *
014200061025      *  42 - Data Spunta  errata                                     *
014300061025      *  43 - Ora  Spunta  errata                                     *
014400061025      *  44 - Applicazione errata                                     *
014500061018      *  90 - Riemissione videata                                     *
014600061018      *===============================================================*
014700061018
014800061017     c     *Entry        plist
014900061017     c                   parm                    KPJBA
015000080512
015100061017      *
015101101213     C                   exsr      routinz
015200061017     c                   exsr      InzS01
015300061017      *
015400061017     c                   dow       $Fine   =  *off
015500061017     c                   exsr      GesS01
015600061017     c                   enddo
015700061017      *
015800061017     c                   eval      *inLR   =  *on
015900061024
016000061024      *---------------------------------------------------------------*
016100061024      *?Operazioni iniziali                                          ?*
016200061024      *---------------------------------------------------------------*
016300061024     c     *InzSR        BEGSR
016400061024      *
016500061024      * Reperimento dati aziendali
016600061024     c     *dtaara       define    §azute        AZUTEds
016700061024     c     *dtaara       define    §datiute      dDATIUTE
016800061024     c                   in(E)     *dtaara
016900061024     c                   if        %ERROR or RSUT = *blanks
017000061024     c                   clear                   Tibs34Ds
017100061024     c                   call      'TIBS34R'
017200061024     c                   parm                    Tibs34Ds
017300061024     c                   in        *dtaara
017400061024     c                   endif
017500061114      *
017600061114     c                   eval      T01pgm  =  SDSpgm
017700061024      *
017800061024     c                   ENDSR
017900061017
017901101213      *---------------------------------------------------------------*
017902101213      *?Operazioni iniziali                                          ?*
017903101213      *---------------------------------------------------------------*
017904101213     c     routinz       BEGSR
017905101213      *
017906101213      /free
017907101213       //?controllo se ambiente di test
017908101213         if %subst(knsif:7:1) = 'P';
017909101213           wlibfile01 = c_libfile01p;
017910101213           wlibfile02 = c_libfile02p;
017911101213         else;
017912101213           wlibfile01 = c_libfile01;
017913101213           wlibfile02 = c_libfile02;
017914101213         endif;
017915101213       //?apro file fipsl*
017916101213         open fipsl01l;
017917101213         open fipsl02l;
017918101213      /end-free
017919101213      *
017920101213     c                   ENDSR
018000061017      *---------------------------------------------------------------*
018100061018      *?Gestione SubFile S01                                         ?*
018200061017      *---------------------------------------------------------------*
018300061017     c     GesS01        BEGSR
018400061017      *
018500061017      * Inizializzazione videata
018600061018if  1c                   if        $InzS01 =  *on
018700061017     c                   exsr      InzS01
018800061018e   1c                   endif
018900061017      *
019000061017      * Emissione subfile
019100061017     c                   eval      *in30   =  (S01nrr <= *zeros)
019200070608     c                   write     MS50T01
019300070608     c                   write     MS50Z01
019400061025     c                   if            S01nrr  <= *zeros
019500061025     c                             and $InzS01 =  *on
019600070608     c                   write     MS50N01
019700061025     c                   endif
019800070608     c                   exfmt     MS50C01
019900061116     c                   setoff                                       28  90
020000061025     c                   eval      $InzS01 =  *off
020100061017      *
020200061017sel 1c                   SELECT
020300100730      * - RollUp = Pagina avanti
020400061017w   1c                   WHEN      £Tasto  =  C_RollUp
020500061025if  2c                   if        (W_SflPag + 1) * C_SflPag <= 9999
020600061018     c                   exsr      RollUpS01
020700061025x   2c                   else
020800061025     c                   seton                                        28  90
020900061025     c                   eval      V1Dmsg  = $Msg(10)
021000061025e   2c                   endif
021100100730      * - F3 = Fine
021200061017w   1c                   WHEN      *inKC   =  *on
021300061017     c                   eval      $Fine   =  *on
021400100730      *
021500061017x   1c                   OTHER
021600061024      * - Gestione opzioni
021700061024if  2c                   if        S01nrr  >  *zeros
021800061018     c                   exsr      OpzS01
021900061024e   2c                   endif
022000061024      * - Controllo variazione selezioni
022100100730if  2c                   If             V1Cpos    <> SAVpos
022200100730     c                             or   V1Cbar    <> SAVbar
022300100730     c                             or   V1Cdtr    <> SAVdtr
022400100730     c                             or   V1Chrr    <> SAVhrr
022500100805     c                             or   V1Cpru    <> SAVpru
022600100730     c                             or   V1Ciddisp <> SAViddisp
022700100730     c                             or   V1Cerr    <> SAVerr
022800061024     c                   exsr      CtrC01
022900061024if  3c                   if        *in90
023000061024     c                   leavesr
023100061024e   3c                   endif
023200100730     c                   eval      $InzS01   =  *on
023300100730     c                   eval      SAVpos    =  V1Cpos
023400100730     c                   eval      SAVdtr    =  V1Cdtr
023500100730     c                   eval      SAVhrr    =  V1Chrr
023600100730     c                   eval      SAVbar    =  V1Cbar
023700100805     c                   eval      SAVpru    =  V1Cpru
023800100805     c                   eval      SAViddisp =  V1Ciddisp
023900100805     c                   eval      SAVerr    =  V1Cerr
024000100730e   2c                   EndIf
024100100730      *
024200061017e   1c                   ENDSL
024300061017      *
024400061017     c                   ENDSR
024500061018
024600061018      *---------------------------------------------------------------*
024700061018      *?Controllo SubFile Control C01                                ?*
024800061018      *---------------------------------------------------------------*
024900061018     c     CtrC01        BEGSR
025000061018      *
025100061018      * Reset indicatori
025200061018     c                   movea     *zeros        *in(50)
025300080512     c
025400061018      *
025500061018      * P.O. spunta
025600080512     c                   if        v1cpos <> 999 or  %subst(knmus:1:3)<>'EDP'
025700061018if  1c                   if        V1Cpos  = *zeros
025800061025     c                   seton                                        284190
025900061018     c                   eval      V1Dmsg  = $Msg(2)
026000061018     c                   leavesr
026100061018x   1c                   else
026200061018     c     V1Cpos        chain     AZORG
026300061018sel 2c                   select
026400061018w   2c                   when      NOT %found(AZORG01L)
026500061025     c                   seton                                        284190
026600061018     c                   eval      V1Dmsg  = $Msg(6)
026700061018     c                   leavesr
026800061018w   2c                   when      ORGfva <> *blanks
026900061025     c                   seton                                        284190
027000061018     c                   eval      V1Dmsg  = $Msg(3)
027100061018     c                   leavesr
027200061018w   2c                   when           ORGfag <> 'F'
027300061018     c                             and  ORGfag <> 'A'
027400061025     c                   seton                                        284190
027500061018     c                   eval      V1Dmsg  = $Msg(4)
027600061018     c                   leavesr
027700061018e   2c                   endsl
027800061018e   1c                   endif
027900080512     c                   endif
028000061018      *
028100061018      * Data spunta
028200070608if  1c                   if        V1Cdtr <> *zeros
028300061018     c                   clear                   WLBdat
028400070608     c                   eval      G02dat  = V1Cdtr
028500061018     c                   call      'XSRDA8'
028600061018     c                   parm                    WLBdat
028700061018if  2c                   if        G02err  = *on
028800061025     c                   seton                                        284290
028900061018     c                   eval      V1Dmsg  = $Msg(7)
029000061018     c                   leavesr
029100061018e   2c                   endif
029200070608     c                   eval      V1Cdtr  = G02dat
029300070608     c                   eval      W1Cdtr  = G02inv
029400061018e   1c                   endif
029500061018      *
029600061018      * Ora spunta
029700061018sel 1c                   select
029800070608w   1c                   when           V1Cdtr = *zeros
029900070608     c                             and  V1Chrr > *zeros
030000061025     c                   seton                                        284390
030100061018     c                   eval      V1Dmsg  = $Msg(9)
030200061018     c                   leavesr
030300070608w   1c                   when           V1Chrr < *zeros
030400070608     c                             or   V1Chrr > 2359
030500061025     c                   seton                                        284390
030600061018     c                   eval      V1Dmsg  = $Msg(8)
030700061018     c                   leavesr
030800061018e   1c                   endsl
030900070608     c                   move      V1Chrr        Wmin
031000061018if  1c                   if        Wmin    > 59
031100061025     c                   seton                                        284390
031200061018     c                   eval      V1Dmsg  = $Msg(8)
031300061018     c                   leavesr
031400061018e   1c                   endif
031500061018      *
031600061018     c                   ENDSR
031700061017
031800061017      *---------------------------------------------------------------*
031900061018      *?Inizializzazione SubFile S01                                 ?*
032000061017      *---------------------------------------------------------------*
032100061017     c     InzS01        BEGSR
032200061017      *
032300061017      * Pulizia subfile
032400061017     c                   seton                                        3031
032500070604     c                   write     MS50C01
032600061018     c                   setoff                                         3133
032700061017      *
032800061018     c                   clear                   W_SflPag
032900061018     c                   clear                   C01rcd
033000061018     c                   clear                   C01csr
033100061018     c                   clear                   S01nrr
033200061018     c                   clear                   V1Dmsg
033300061018     c                   setoff                                       28  90
033400061018     c                   movea     *zeros        *in(41)
033500080512
033600100730     c                   if        %subst(knmus:1:3) = 'EDP'
033700100730     c                   eval      *in45 = *on
033800080512     c                   endif
033900061017      *
034000061018if  1c                   if        V1Cpos <= *zeros
034100061018     c                   eval      V1Cpos  = DUTtfp
034200061017e   1c                   endif
034300061017      *
034400061017      * Se inizializzazione iniziale: esco senza caricare nulla
034500061017     c                   if        $InzS01 = *off
034600061017     c                   leavesr
034700061017     c                   endif
034800061017      *
034900061018      * Caricamento tabelle del P.O spunta selezionato in schiera
035000061018     c                   exsr      sr_CarTAB
035100061018      *
035200100730     c                   if        V1Cpos <> 999
035300061018     c                   movel     V1Cpos        Kfil
035400070608     c                   if        V1Cdtr <> *zeros
035500061018     c                   eval      Ktim    = %trim( %editw
035600070608     c                                           (W1Cdtr:'0        ') )
035700061018     c                                     + %trim( %editw
035800070611     c                                           (V1Chrr:'0    ') )
035900061018     c                                     + '00'
036000070608     c     K02psl01      setll     FIPSL000
036100061018     c                   else
036200070608     c     Kfil          setll     FIPSL000
036300061018     c                   endif
036400070608     c     Kfil          reade     FIPSL000
036500080512     c                   else
036600080512     c                   if        V1Cdtr <> *zeros
036700080512     c                   eval      Ktim    = %trim( %editw
036800080512     c                                           (W1Cdtr:'0        ') )
036900080512     c                                     + %trim( %editw
037000080512     c                                           (V1Chrr:'0    ') )
037100080512     c                                     + '00'
037200080512     c     ktim          setll     fipsl002
037300080512     c                   else
037400080512     c     *loval        setll     fipsl002
037500080512     c                   endif
037600080512     c                   read      fipsl002
037700080512     c                   endif
037800061018      *
037900061018     c                   exsr      RollUpS01
038000061018      *
038100061018     c                   ENDSR
038200061018
038300061018      *---------------------------------------------------------------*
038400061018      *?Caricamento pagina di dati nel SubFile S01                   ?*
038500061018      *---------------------------------------------------------------*
038600061018     c     RollUpS01     BEGSR
038700061018      *
038800061018     c                   eval      S01nrr      = (W_SflPag * C_SflPag)
038900061018     c                   add       1             W_SflPag
039000061018     c                   eval      *in32       = *off
039100061018      *
039200061018      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
039300080512     c                   if        v1cpos<>999
039400070608do  1c                   doW       NOT  %eof(FIPSL01L)
039500061018     c                             and  S01nrr < (W_SflPag * C_SflPag)
039600061018     c                   exsr      CarS01
039700070608     c     Kfil          reade     FIPSL000
039800061018e   1c                   enddo
039900080512     c                   else
040000080512do  1c                   doW       NOT  %eof(FIPSL02L)
040100080512     c                             and  S01nrr < (W_SflPag * C_SflPag)
040200080512     c                   exsr      CarS01
040300080512     c                   read      FIPSL002
040400080512e   1c                   enddo
040500080512     c                   endif
040600061025      *
040700061025      * Visualizzazione del SFL (se ci sono dati)
040800061025     c                   eval      *in30   = (S01nrr = *zeros)
040900061025      *
041000061025      * Attivazione (eventuale) del SFLEND
041100080512     c                   if        v1cpos<>999
041200070608     c                   eval      *in33   = (%eof(FIPSL01L))
041300080512     c                   else
041400080512     c                   eval      *in33   = (%eof(FIPSL02L))
041500080512     c                   endif
041600061025      *
041700061025      * Posizionamento cursore al 1° rcd della pagina
041800061025if  1c                   if        S01nrr  > *zeros
041900061025     c     S01nrr        div       C_SflPag      wPag
042000061025     c                   mvr                     wRig
042100061025     c                   eval      C01rcd  = wPag * C_SflPag
042200061025     c                   if        wRig    > *zeros
042300061025     c                   eval      C01rcd  = C01rcd + 1
042400061025     c                   else
042500061025     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
042600061025     c                   endif
042700061025x   1c                   else
042800061025     c                   clear                   C01rcd
042900061025e   1c                   endif
043000061025     c                   eval      C01csr  = C01rcd
043100061018      *
043200061018     c                   ENDSR
043300061018
043400061018      *---------------------------------------------------------------*
043500061018      *?Caricamento singolo record nel SubFile S01                   ?*
043600061018      *---------------------------------------------------------------*
043700061018     c     CarS01        BEGSR
043800061018      *
043900061018      * Selezione record
044000061018sel 1c                   SELECT
044100061018      * - Data
044200070608w   1c                   WHEN           V1Cdtr <> *zeros
044300061018     c                             and  %trim( %editw
044400070608     c                                    (W1Cdtr:'0        ') ) <>
044500070608     c                                    %subst(psldtorar:1:8)
044600080512     c                   if        v1cpos<>999
044700070608     c     *hival        setgt     FIPSL000
044800080512     c                   else
044900080512     c     *hival        setgt     FIPSL002
045000080512     c                   endif
045100061018     c                   leavesr
045200061018      * - Ora
045300070611w   1c                   WHEN           V1Chrr <> *zeros
045400070611     c                             and  %trim(%editw(V1Chrr:'0    '))
045500070608     c                                          > %subst(psldtorar:9:4)
045600080512     c                   if        v1cpos<>999
045700061018     c                   leavesr
045800080512     c                   else
045900080512     c     *hival        setgt     FIPSL002
046000080512     c                   endif
046100061018e   1c                   ENDSL
046200100730      * - parzializzazione per barcode
046300100730    1c                   if        V1Cbar <> *blanks
046400070608     c                   eval      yy      = %scan( %trimr(V1Cbar)
046500100730     c                                            : PSLbarco)
046600070608if  2c                   if        yy      = *zeros
046700070608     c                   leavesr
046800070608e   2c                   endif
046900070608    1c                   endif
047000100805      * - parzializzazione per profilo utente
047100100805     c                   if        V1Cpru <> *blanks   and
047200100805     c                             V1Cpru <> PSLusr
047300100805     c                   leavesr
047400100805     c                   endif
047500100730      * - parzializzazione per id-dispositivo
047600100730     c                   if        V1Ciddisp <> *blanks   and
047700100730     c                             V1Ciddisp <> PSLiddisp
047800070608     c                   leavesr
047900070608     c                   endif
048000100730      * - parzializzazione per solo errori
048001120127     c                   select
048100120127     c                   when      V1Cerr = 'S'
048200120127     c                   if        PSLerr = *blanks
048300080121     c                   leavesr
048301120127     c                   endif
048401120127     c                   when      V1Cerr = 'I'
048402120127     c                   if        PSLerr <> 'I'
048403120127     c                   leavesr
048404120127     c                   endif
048405120127     c                   when      V1Cerr = 'L'
048406120127     c                   if        PSLerr <> 'L'
048407120127     c                   leavesr
048408120127     c                   endif
048409120127     c                   endsl
048500061018      *
048600061018      * Caricamento dati nel record del sfl  - - - - - - - - - - - - -*
048700061018     c                   exsr      RieS01
048800061018      *
048900061018      * Scrittura record subfile - - - - - - - - - - - - - - - - - - -*
049000061018     c                   add       1             S01nrr
049100070608     c                   write     MS50S01
049200061018      *
049300061018     c                   ENDSR
049400061018
049500061018      *---------------------------------------------------------------*
049600061018      *?Riempimento singolo record del SubFile S01                   ?*
049700061018      *---------------------------------------------------------------*
049800061018     c     RieS01        BEGSR
049900061018      *
050000070608     c                   clear                   MS50S01
050100061018      *
050200061018      * - Campi hidden
050300070608     c                   movel     psldtorar     V2Camg
050400061018     c                   reset                   WLBdat
050500061018     c                   eval      G02inv  = V2Camg
050600061018     c                   call      'XSRDA8'
050700061018     c                   parm                    wlbdat
050800061018     c                   eval      V2Cgma  = G02dat
050900070608     c                   eval      V2Cmil  = pslmilles
051000070611     c                   eval      v2ciddisp=psliddisp
051100080512     c                   if        v1cpos<>999
051200080117     c                   z-add     pslnrr        s1cnrr
051300080512     c                   else
051400080512     c                   z-add     pslnrr2       s1cnrr
051500080512     c                   endif
051600061018      *
051700070608      * - Campi visualizzati
051800070608     c                   movel     V2Cgma        s1cdtr
051900070608     c                   move      psldtorar     s1chrr
052000070611     c                   eval      s1cnpg  = pslnpg
052100070611     c                   move      pslnfv        s1cnfv
052200070608     c                   eval      s1cbar  = pslbarco
052300070611     c                   eval      s1cpru  = pslusr
052400070608     c                   eval      s1cman  = pslman
052500070608     c* anomalia --> visualizzo la decodifica
052600070608     c                   clear                   s1canm
052700100730     c                   eval      xn = %lookup(pslcan : $an)
052800100730     c                   if        xn > *zero
052900100730     c                   eval      s1canm = $an_D(xn)
053000070608     c                   endif
053100070608     c                   eval      s1cdup  = psldup
053200070608     c                   eval      s1cerr  = pslerr
053300061018      *
053400061018     c                   ENDSR
053500061018
053600061018      *---------------------------------------------------------------*
053700061018      *?Gestione singola opzione nel SubFile S01                     ?*
053800061018      *---------------------------------------------------------------*
053900061018     c     OpzS01        BEGSR
054000061018      *
054100070608     c                   readc     MS50S01
054200061018      *
054300070608do  1c                   DOW       NOT  %eof(FIMS50D)
054400061018      *
054500061018     c                   eval      *in32       = *off
054600061018     c                   movea     *zeros        *in(40)
054700061018     c                   z-add     S01nrr        C01rcd
054800061018      *
054900061018sel 2c                   SELECT
055000061018      * - Nessuna opzione
055100061018w   2c                   WHEN      S1Copz      = *blanks
055200061018      * - 5 = Visualizzazione
055300061018w   2c                   WHEN      S1Copz      = '5'
055400061018     c                   exsr      sr_OpzS1_5
055500061018      * - ? = Opzione NON valida
055600061018x   2c***                OTHER
055700061025     c***                seton                                        284090
055800061018     c***                eval      V1Dmsg      = $Msg(??)
055900061018e   2c                   ENDSL
056000061018      *
056100061018      * Aggiornamento sfl
056200061018sel 2c                   select
056300061018w   2c                   when      *in28
056400061018     c                   eval      *in32       = *on
056500061018     c                   z-add     C01rcd        C01csr
056600061018w   2c                   when      *in90       = *on
056700061018     c                   z-add     C01rcd        C01csr
056800061018     c                   clear                   S1Copz
056900061018x   2c                   other
057000061018     c                   clear                   S1Copz
057100061018e   2c                   endsl
057200070608     c                   UPDATE    MS50S01
057300061018if  2c                   if        *in28  OR  *in90
057400061018     c                   leave
057500061018e   2c                   endif
057600061018      *
057700070608     c                   readc     MS50S01
057800061018      *
057900061018e   1c                   ENDDO
058000061018      *
058100061018     c                   ENDSR
058200061018
058300061018      *---------------------------------------------------------------*
058400061018      *?Visualizzazione singola lettura                              ?*
058500061018      *---------------------------------------------------------------*
058600061018     c     sr_OpzS1_5    BEGSR
058700061018      *
058800070611     c                   clear                   FIMS51ds
058900100730     c                   z-add     s1cnrr        MS51nrr
059000070608      *
059100070611     c                   movel(p)  FIMS51ds      KPJBU
059200070608      *
059300070611     c                   call      'FIMS51R'
059400070611     c                   parm                    KPJBA
059500061018      *
059600061018     c                   ENDSR
059700061018      *---------------------------------------------------------------*
059800061018      *?Caricamento Tabelle occorrenti                               ?*
059900061018      *---------------------------------------------------------------*
060000061018     c     sr_CarTAB     BEGSR
060100061018      *
060200070608     c* carico la tabella delle anomalie spunte che mando ai PDA
060300070608     c                   z-add     1             yy
060400070608     c                   movel     '3E'          cod
060500070611     c     ktab2         chain     tabel                              10
060600070611     c     *in10         doweq     *off
060700070608     c                   movel     tbluni        ds3e
060800070608     c     §3epda        ifne      'N'
060900100730     C                   MOVEL     TBLKEY        $AN(yy)
061000100730     C                   MOVEL     §3edes        $AN_D(yy)
061100070608     C                   ADD       1             yy
061200070608     C                   ENDIF
061300070608     c**
061400070611     c     ktab2         reade     tabel                                  10
061500070608     C                   ENDDO
061600021206      *
061700021206     c                   ENDSR
061800010608
061900061018      *---------------------------------------------------------------*
062000061018
062100061018** - $MSG -------------------------------------------------------------------*
062200061025     L I B E R O                                                                1
062300061018Inserire Punto Operativo                                                        2
062400061018Punto Operativo annullato                                                       3
062500061018Punto Operativo non valido                                                      4
062600061025     L I B E R O                                                                5
062700061018Codice inesistente                                                              6
062800061018Data errata                                                                     7
062900061018Ora formalmente errata                                                          8
063000061018Non è possibile inserire un orario senza data                                   9
063100061018Ampiezza massima subfile superata                                              10
063200070608     L I B E R O                                                               11
063300070608     L I B E R O                                                               12
