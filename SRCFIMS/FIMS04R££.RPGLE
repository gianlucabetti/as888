000100031029      *===============================================================*
000200031029      * FIMS04R * Lista Piano di Smistamento per Picking              *
000300031029      *===============================================================*
000400031029
000500031029     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600031029
000700031029      *---------------------------------------------------------------*
000800031029
000900031029     fFLTR901L  if   e           k disk
001000031029      *
001100031029     fFNFVV01L  if   e           k disk
001200031029     fFIAPD01L  if   e           k disk
001300031029     fAZORG01L  if   e           k disk
001400040528     fTNTBE01L  if   e           k disk
001500031029      *
001600040705     fFIMS04P   o    e             printer oflind(*IN40)
001700031029
001800031029      *---------------------------------------------------------------*
001900031029
002000031029      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002100031029      *
002200040607     d DigitN          c                   const('0123456789')
002300060214     d C_PO_a          c                   const(006)
002400040518     d C_MaxRecPf      c                   const(600)
002500031029      *
002600031029      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002700031029      *
002800031113      * - Messaggi di errore
002900031113     d $Msg            s             78    dim(1)  ctdata  perrcd(1)
003000031113      *
003100031113      * - F.V. immessi a video (da scartare da FNLR02R)
003200031113     d $FV             s                   dim(C_MaxRecPf) inz
003300031113     d                                     like(FVVnfv)
003400040528      *
003500040528      * - Postazioni IMA da tab. "MPT"
003600040528     d $MPT            s                   dim(20)         inz
003700040528     d                                     like(TR9bai)
003800040528     d $MPTd           s                   dim(20)         inz
003900040528     d                                     like(dMPT)
004000031029      *
004100031029      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004200031029      *
004300031029      * - Parametri ricevuti
004400031029     d KPJBA         e ds
004500031029     d FIMS04ds      e ds
004600031029      * - Parametri x Controllo profilo utenti
004700031029     d TIBS34DS      e ds
004800031029      * - Ds di riferimento al file esterno AZUTE00F
004900031029     d AZUTEDS       e ds                  extname(AZUTE00F)
005000031029      * - Ds per dati organigramma
005100031029     d DDatiUte      e ds
005200031029      *
005300031029      * - Parametri x Interrogazione tabelle
005400031029     d TIBS02DS      e ds                  inz
005500031029     d   T02mod      e                     inz('C')
005600031029     d   T02cod      e                     inz('MFP')
005700031029      * - Tab. "MFP"
005800031029     d dMFP          e ds                  inz
005900031029     d   §MFPap                1     10    dim(10)
006000040528      * - Tab. "MPT"
006100040528     d dMPT          e ds                  inz
006200031113      *
006300031113      * - Parametri standard nuovi x SRCFNLS
006400031113     d DSLR01        e ds                  inz
006500031113     d   DLRric      e                     inz('S')
006600031113     d   DLRtfv      e                     inz('3')
006700031113     d   DLRnpg      e                     inz(4)
006800031113     d   DLRgal      e                     inz(31129999)
006900031113     d   DLRaal      e                     inz(99991231)
007000031029      *
007100031029     d Status         sds
007200031029     d   SDSpgm          *proc
007300031029      *
007400031029      * - Controllo/Inversione date
007500031029     d WLBda8          ds                  inz
007600031029     d   G08dat                       8  0 inz
007700031029     d   G08inv                       8  0 inz
007800031029     d   G08err                       1    inz
007900031029     d   G08tgi                       5  0 inz
008000031029      *
008100031029     d DSdata8         ds             8    inz
008200031029     d   DSdt8gg                      2  0 inz
008300031029     d   DSdt8mm                      2  0 inz
008400031029     d   DSdt8aa                      4  0 inz
008500031029     d DSdata6         ds             6    inz
008600031029     d   DSdt6gg                      2  0 inz
008700031029     d   DSdt6mm                      2  0 inz
008800031029     d   DSdt6aa                      2  0 inz
008900031029      *
009000031029      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
009100031029      *
009200031029      * - Flags
009300031029     d $Default        s              1    inz(*off)
009400031029     d $VDL            s              1    inz(*off)
009500031029     d $Stampa         s              1    inz(*off)
009600031029      *
009700031029      * - Indici di schiera
009800031113     d XX              s              3  0 inz
009900031029      *
010000031029      * - Variabili
010100031029
010200031029      *---------------------------------------------------------------*
010300031029      *  RIEPILOGO INDICATORI                                         *
010400031029      *---------------------------------------------------------------*
010500031029      * 10    - Comodo                                                *
010600040705      * 40    - OverFlow                                              *
010700040705      * 41    - Stampa del Piano di Default                           *
010800040705      * 42    - Stampa del Piano giornaliero DPD                      *
010900031029      *---------------------------------------------------------------*
011000031029
011100031029     c     *Entry        plist
011200031029     c                   parm                    KPJBA
011300031029      *
011400031029      * Operazioni Iniziali
011500031029     c                   exsr      SR_INIZ
011600031029      *
011700031029      * Posizionamento iniziale e 1ª lettura
011800031029     c     K03FLTR9      setll     FLTR9000
011900031029     c     K03FLTR9      reade     FLTR9000
012000031029      *
012100031029      * Ciclo di lettura del file principale
012200031029do  1c                   dow       NOT %eof(FLTR901L)
012300031029      *
012400031029      * - Decodifiche
012500031029     c                   exsr      SR_DECOD
012600031029      *
012700031029      * - Stampa
012800031029     c                   exsr      SR_STAMPA
012900031029      *
013000031029     c     K03FLTR9      reade     FLTR9000
013100031029e   1c                   enddo
013200031029      *
013300031029      * Fine
013400031029     c                   exsr      SR_ENDPGM
013500031029      *
013600031029      *---------------------------------------------------------------*
013700031029      * Operazioni Iniziali                                           *
013800031029      *---------------------------------------------------------------*
013900031029     c     SR_INIZ       BEGSR
014000031029      *
014100031029     c                   movel     KPJBU         FIMS04DS
014200031029      *
014300031029      * Reperimento dei dati del job
014400031029     c                   exsr      DatiJob
014500031029      *
014600031029      * Impostazione dei campi chiave fissi
014700031029     c                   eval      APDtip   = 'A'
014800031029      *
014900031029      * Decodifica del P.O. in gestione
015000031029     c     D04pog        chain     AZORG
015100031029if  1c                   if        not %found(AZORG01L)
015200031029     c                             or  ORGfva = '*'
015300031029     c                   movel     *all'? '      PT1dpo
015400031029x   1c                   else
015500031029     c                   movel     ORGdes        PT1dpo
015600031029e   1c                   endif
015700031029      * Controllo se P.O. in gestione ha VDL
015800031029     c                   exsr      Ctrl_VDL
015900040528      * Caricamento chiera tab. MPT
016000040528     c                   exsr      Tab_MPT
016100031029      *
016200031029      * Reperimento della data del F.V.
016300031029     c                   z-add     D04nfva       FVVnfv
016400031029     c     K03FNFVV      chain     FNFVV000
016500031029if  1c                   if            %found(FNFVV01L)
016600031029     c                             and FVVatb =  *blanks
016700031029     c                   clear                   WLBda8
016800031029     c                   eval      G08inv   = FVVdfv
016900031029     c                   eval      G08err   = '3'
017000031029     c                   call      'XSRDA8'
017100031029     c                   parm                    WLBda8
017200031029     c                   eval      PT1dfv   = G08dat
017300031029if  2c                   if        FVVfcf  <> *blanks
017400031029     c                   eval      PT1fcf   = 'C H I U S O'
017500031029e   2c                   endif
017600031029e   1c                   endif
017700031029      *
017800031029      * Stampa della testata in 1ª pagina
017900031029     c                   eval      PT1pgm   = SDSpgm
018000031029if  1c                   if            D04npga = *zeros
018100031029     c                             and D04nfva = *zeros
018200031029     c                   eval      PT1des   = 'P I A N O   '
018300031029     c                                      + 'D I   D E F A U L T'
018400031029     c                   eval      $Default = *on
018500031029x   1c                   else
018600031029     c                   eval      PT1des   = 'P I A N O   '
018700031029     c                                      + 'G I O R N A L I E R O'
018800031029     c                   eval      $Default = *off
018900031029e   1c                   endif
019000040705     c                   eval      *in41    = ($Default = *on)
019100040705     c     K03FLTR9      chain     FLTR9000
019200040705     c                   eval      *in42    = (%found(FLTR901L)
019300040705     c                                         and TR9fl2 = 'D')
019400031029     c                   eval      PT1pog   = D04pog
019500031029     c                   eval      PT1npga  = D04npga
019600031029     c                   eval      PT1nfva  = D04nfva
019700031029     c                   write     MS04T0
019800031029if  1c                   if        $VDL     = *off
019900031029     c                   write     MS04T1
020000031029x   1c                   else
020100031029     c                   write     MS04T2
020200031029e   1c                   endif
020300040705     c                   eval      *in40    = *off
020400031029      *
020500031029     c                   ENDSR
020600031029      *
020700031029      *---------------------------------------------------------------*
020800031029      * Reperimento Dati del job (Utente/Operativi)                   *
020900031029      *---------------------------------------------------------------*
021000031029     c     DatiJob       BEGSR
021100031029      *
021200031029     c     *dtaara       define    §azute        azuteds
021300031029     c     *dtaara       define    §datiute      ddatiute
021400031029      *
021500031029     c                   in(E)     *dtaara
021600031029     c                   IF        %ERROR or RSUT = *blanks
021700031029     c                   clear                   Tibs34Ds
021800031029     c                   call      'TIBS34R'
021900031029     c                   parm                    Tibs34Ds
022000031029     c                   in        *dtaara
022100031029     c                   ENDIF
022200031029      *
022300031029     c                   ENDSR
022400031029      *
022500031029      *---------------------------------------------------------------*
022600031029      * Controllo se P.O. in gestione con VDL                         *
022700031029      *---------------------------------------------------------------*
022800031029     c     Ctrl_VDL      BEGSR
022900031029      *
023000031029     c                   reset                   $VDL
023100031029     c                   clear                   dMFP
023200031029      *
023300031029     c                   reset                   TIBS02ds
023400031029     c                   movel     KNSIF         T02sif
023500031029     c                   movel     D04pog        T02ke1
023600031029     c                   call      'TIBS02R'
023700031029     c                   parm                    KPJBA
023800031029     c                   parm                    TIBS02ds
023900031029if  1c                   if        T02err = *blanks
024000031029     c                   movel     T02uni        dMFP
024100031029e   1c                   endif
024200031029      *
024300060214if  1c                   if        D04pog <> C_PO_a
024400060214      *
024500031029     c     'V'           lookup    §MFPap                                 10
024600040630     c  N10'C'           lookup    §MFPap                                 10
024700031029     c                   movel     *in10         $VDL
024800060214      *
024900060214e   1c                   endif
025000031029      *
025100031029     c                   ENDSR
025200040528      *
025300040528      *---------------------------------------------------------------*
025400040528      * Caricamento schiera tab. MPT                                  *
025500040528      *---------------------------------------------------------------*
025600040528     c     Tab_MPT       BEGSR
025700040528      *
025800040528     c                   clear                   XX
025900040528     c                   clear                   $MPT
026000040528     c                   clear                   $MPTd
026100040528      *
026200040528     c                   movel     'MPT'         TBEcod
026300040528     c     TBEcod        setll     TNTBE000
026400040528     c     TBEcod        reade     TNTBE000
026500040528      *
026600040528do  1c                   dow       NOT %eof(TNTBE01L)
026700040528      *
026800040528if  2c                   if        TBEatb  = *blanks
026900040528     c                   movel     TBEuni        dMPT
027000040528     c                   add       1             xx
027100040528     c                   movel     TBEke1        $MPT(xx)
027200040528     c                   movel     dMPT          $MPTd(xx)
027300040528e   2c                   endif
027400040528      *
027500040528     c     TBEcod        reade     TNTBE000
027600040528e   1c                   enddo
027700040528      *
027800040528     c                   ENDSR
027900031029      *
028000031029      *---------------------------------------------------------------*
028100031029      * Decodifiche                                                   *
028200031029      *---------------------------------------------------------------*
028300031029     c     SR_DECOD      BEGSR
028400040528      *
028500040528     c                   clear                   APDrsc
028600040528if  1c                   if        TR9pdc = *zeros
028700040528      * Postazione IMA
028800040528     c                   eval      xx    = 1
028900040528     c                   eval      *in10 = *off
029000040528     c     TR9bai        lookup    $MPT(xx)                               10
029100040528if  2c                   if        *in10 = *on
029200040528     c                   movel     $MPTd(xx)     APDrsc
029300040607x   2c                   else
029400040607      * P.O.
029500040607     c     DigitN        check     TR9bai                                 10
029600040607if  3c                   if        *in10  =  *off
029700040607     c                   move      TR9bai        ORGfil
029800040607     c     ORGfil        chain     AZORG
029900040607if  4c                   if        %found(AZORG01L)
030000040607     c                   movel(p)  ORGdes        APDrsc
030100040607e   4c                   endif
030200040607e   3c                   endif
030300040528e   2c                   endif
030400031029      *
030500040528x   1c                   else
030600040528      *
030700031029      * Autotrasportatore
030800031029     c     K02FIAPD      chain     FIAPD000
030900040528if  2***c                   if        NOT %found(FIAPD01L)
031000040528     ***c                             or  APDatb <> *blanks
031100040528     ***c                   clear                   APDrsc
031200040528e   2***c                   endif
031300040528      *
031400040528e   1c                   endif
031500031029      *
031600031029     c                   ENDSR
031700031029      *
031800031029      *---------------------------------------------------------------*
031900031029      * Stampa singola riga                                           *
032000031029      *---------------------------------------------------------------*
032100031029     c     SR_STAMPA     BEGSR
032200031029      *
032300031029      * Inversione data del foglio viaggio (se piano giornaliero)
032400031029if  1c                   if        $Default = *off
032500031029     c                   clear                   WLBda8
032600031029     c                   reset                   DSdata8
032700031029     c                   reset                   DSdata6
032800031029if  2c                   if        TR9dfv   > *zeros
032900031029     c                   eval      G08inv   = TR9dfv
033000031029     c                   eval      G08err   = '3'
033100031029     c                   call      'XSRDA8'
033200031029     c                   parm                    WLBda8
033300031029     c                   move      G08dat        DSdata8
033400031029     c                   z-add     DSdt8gg       DSdt6gg
033500031029     c                   z-add     DSdt8mm       DSdt6mm
033600031029     c                   z-add     DSdt8aa       DSdt6aa
033700031029e   2c                   endif
033800031029e   1c                   endif
033900031029      *
034000031029if  1c                   if        $VDL     = *off
034100031029      * P.O. senza VDL
034200031029     c                   clear                   MS04D1
034300040705     c                   eval      *in41    = ($Default = *on)
034400031029      * - dati di default (ma anche del piano giornaliero)
034500031029     c                   move      TR9pep        P1Cpep
034600031029     c                   movel     TR9des        P1Cdes
034700031029     c                   movel     TR9bai        P1Cbai
034800031029     c                   z-add     TR9pdc        P1Cpdc
034900031029     c                   movel     APDrsc        P1Dpdc
035000031029      * - dati specifici aggiuntivi del piano giornaliero
035100031029if  2c                   if        $Default = *off
035200031029     c                   z-add     TR9nfv        P1Cnfv1
035300031029     c                   movel     TR9fl1        P1Cfpp1
035400031029     c                   move      DSdata6       P1Cdfv1
035500031029     c                   exsr      CercaIIfv
035600031029e   2c                   endif
035700031029      *
035800031029x   1c                   else
035900031029      * P.O. con VDL
036000031029     c                   clear                   MS04D2
036100040705     c                   eval      *in41    = ($Default = *on)
036200031029      * - dati di default (ma anche del piano giornaliero)
036300031029     c                   movel     TR9pep        P2Cpep
036400161207     c*//                movel     TR9des        P2Cdes
036500161207     c                   eval      P2Cdes = %subst( TR9des : 1 : 28 )
036600031029     c                   movel     TR9bnl        P2Cbnl
036700040305     c                   movel     TR9bai        P2Cbai
036800170214     c                   if        %subst( TR9des : 29 ) >= *zeros
036900170214     c                   eval      P2Cznc = %subst( TR9des : 29 )
037000161213     c                   endif
037100031029     c                   z-add     TR9pdc        P2Cpdc
037200031029     c                   movel     APDrsc        P2Dpdc
037300031029      * - dati specifici aggiuntivi del piano giornaliero
037400031029if  2c                   if        $Default = *off
037500031107     c                   z-add     TR9nfv        P2Cnfv1
037600031107     c                   movel     TR9fl1        P2Cfpp1
037700031107     c                   move      DSdata6       P2Cdfv1
037800031107     c                   exsr      CercaIIfv
037900031029e   2c                   endif
038000031029      *
038100031029e   1c                   endif
038200031029      *
038300031029      * STAMPA singolo record del Piano di Smistamento
038400031029      *
038500040705if  1c                   if        *in40    = *on
038600031029     c                   write     MS04T0
038700031029if  2c                   if        $VDL     = *off
038800031029     c                   write     MS04T1
038900031029x   2c                   else
039000031029     c                   write     MS04T2
039100031029e   2c                   endif
039200040705     c                   eval      *in40    = *off
039300031029e   1c                   endif
039400031029      *
039500031029if  1c                   if        $VDL     = *off
039600031029     c                   write     MS04D1
039700031029x   1c                   else
039800031029     c                   write     MS04D2
039900031029e   1c                   endif
040000031029      *
040100031029     c                   eval      $Stampa  = *on
040200031029      *
040300031029     c                   ENDSR
040400031029      *
040500031029      *---------------------------------------------------------------*
040600031029      * Reperimento eventuale secondo F.V. per postazione/baia        *
040700031029      *---------------------------------------------------------------*
040800031029     c     CercaIIfv     BEGSR
040900031113      *
041000031113      * memorizzo il F.V. in input tra quelli inseriti nel piano
041100031113if  1c                   if            $VDL    =  *off
041200031113     c                             and TR9bai  <> 'IMA'
041300031113     c                             OR  $VDL    =  *on
041400031113     c                             and TR9bnl  <> 'S'
041500040305     c                             and TR9bai  <> 'IMA'
041600031113     c                   add       1             xx
041700031113     c                   z-add     TR9nfv        $FV(xx)
041800031113e   1c                   endif
041900031029      *
042000040305     c                   add       1             TR9prg
042100031107     c     K06FLTR9      chain     FLTR9000
042200031029      *
042300031107if  1c                   if        %found(FLTR901L)
042400031029      *
042500031029     c                   clear                   WLBda8
042600031029     c                   reset                   DSdata8
042700031029     c                   reset                   DSdata6
042800031029if  2c                   if        TR9dfv   > *zeros
042900031029     c                   eval      G08inv   = TR9dfv
043000031029     c                   eval      G08err   = '3'
043100031029     c                   call      'XSRDA8'
043200031029     c                   parm                    WLBda8
043300031029     c                   move      G08dat        DSdata8
043400031029     c                   z-add     DSdt8gg       DSdt6gg
043500031029     c                   z-add     DSdt8mm       DSdt6mm
043600031029     c                   z-add     DSdt8aa       DSdt6aa
043700031029e   2c                   endif
043800031029      *
043900031107if  2c                   if        $VDL     = *off
044000031029     c                   z-add     TR9nfv        P1Cnfv2
044100031029     c                   movel     TR9fl1        P1Cfpp2
044200031029     c                   move      DSdata6       P1Cdfv2
044300031107x   2c                   else
044400031107     c                   z-add     TR9nfv        P2Cnfv2
044500031107     c                   movel     TR9fl1        P2Cfpp2
044600031107     c                   move      DSdata6       P2Cdfv2
044700031107e   2c                   endif
044800031113      *
044900031113      * memorizzo il F.V. in input tra quelli inseriti nel piano
045000031113if  2c                   if            $VDL    =  *off
045100031113     c                             and TR9bai  <> 'IMA'
045200031113     c                             OR  $VDL    =  *on
045300031113     c                             and TR9bnl  <> 'S'
045400031113     c                   add       1             xx
045500031113     c                   z-add     TR9nfv        $FV(xx)
045600031113e   2c                   endif
045700040305      *
045800040305x   1c                   else
045900031029      *
046000040305      * se non trovo altri f.v. per la locazione in esame:
046100040305      *   riposiziono il ciclo di lettura sull'ultimo record elaborato
046200040305     c                   sub       1             TR9prg
046300040305     c     K06FLTR9      setgt     FLTR9000
046400040305      *
046500031029e   1c                   endif
046600031029      *
046700031029     c                   ENDSR
046800031113      *
046900031113      *---------------------------------------------------------------*
047000031113      * Controllo esistenza di distinte aperte non inserite nel piano *
047100031113      *---------------------------------------------------------------*
047200031113     c     SR_DIST_AP    BEGSR
047300031113      *
047400031113      * Richiamo pgm. FNLR02R per fare il controllo
047500031113     c                   reset                   DSLR01
047600031113     c                   z-add     PT1dfv        DLRgda                         Dal gg/mm/aaaa
047700031113     c                   z-add     FVVdfv        DLRada                         Dal aaaa/mm/gg
047800031113     c                   z-add     D04pog        DLRfgs                         P.O. gestione
047900031113     c                   movel     SDSpgm        DLRpgm                         Nome programma
048000031113     c                   movel(p)  DSLR01        KPJBU
048100031113     c                   call      'FNLR02R'
048200031113     c                   parm                    KPJBA
048300031113     c                   parm                    $FV
048400031113     c                   movel     KPJBU         DSLR01
048500031113      *
048600031113      * Stampo (eventualmente) la segnalazione
048700031113if  1c                   if        DLRfs1 = *on
048800031113      *
048900040705if  2c                   if        *in40    = *on
049000031113     c                   write     MS04T0
049100031113if  3c                   if        $VDL     = *off
049200031113     c                   write     MS04T1
049300031113x   3c                   else
049400031113     c                   write     MS04T2
049500031113e   3c                   endif
049600040705     c                   eval      *in40    = *off
049700031113e   2c                   endif
049800031113      *
049900031113     c                   clear                   MS04N0
050000031113     c                   movel(p)  $Msg(1)       PN0txt
050100031113     c                   write     MS04N0
050200031113      *
050300031113e   1c                   endif
050400031113      *
050500031113     c                   ENDSR
050600031029      *
050700031029      *---------------------------------------------------------------*
050800031029      * Operazioni Finali                                             *
050900031029      *---------------------------------------------------------------*
051000031029     c     SR_ENDPGM     BEGSR
051100031029      *
051200031029      * Segnalazione dell'eventuale mancanza di dati da stampare
051300031029if  1c                   if        $Stampa  = *off
051400031029if  2c                   if        $VDL     = *off
051500031029     c                   write     MS04N1
051600031029x   2c                   else
051700031029     c                   write     MS04N2
051800031029e   2c                   endif
051900031029e   1c                   endif
052000031113      *
052100031113      * Se stampa di piano giornaliero verifico l'esistenza di
052200031113      *   distinte aperte NON inserite nel piano (quindi non listate)
052300031113if  1c                   if        D04nfva > *zeros
052400031113     c                   exsr      SR_DIST_AP
052500031113e   1c                   endif
052600031029      *
052700031029      * Stampa di "Fine Lista"
052800040705if  1c                   if        *in40    = *on
052900031113     c                   write     MS04T0
053000031113if  2c                   if        $VDL     = *off
053100031113     c                   write     MS04T1
053200031113x   2c                   else
053300031113     c                   write     MS04T2
053400031113e   2c                   endif
053500040705     c                   eval      *in40    = *off
053600031113e   1c                   endif
053700031029     c                   write     MS04F0
053800031029      *
053900031029      * Termino
054000031029     c                   eval      *INLR    = *on
054100031029     c                   return
054200031029      *
054300031029     c                   ENDSR
054400031029      *
054500031029      *---------------------------------------------------------------*
054600031029      * Definizione Key-List                                          *
054700031029      *---------------------------------------------------------------*
054800031029     c     DefKLIST      BEGSR
054900031029      *
055000031029      * file FLTR901L
055100031029     c     K03FLTR9      klist
055200031029     c                   kfld                    D04pog
055300031029     c                   kfld                    D04npga
055400031029     c                   kfld                    D04nfva
055500031107     c     K06FLTR9      klist
055600031029     c                   kfld                    TR9fgs
055700031029     c                   kfld                    TR9npga
055800031029     c                   kfld                    TR9nfva
055900031029     c                   kfld                    TR9pep
056000040305     c                   kfld                    TR9prg
056100031029     c                   kfld                    TR9bai
056200031029      *
056300031029      * file FNFVV01L
056400031029     c     K03FNFVV      klist
056500031029     c                   kfld                    D04npga
056600031029     c                   kfld                    FVVnfv
056700031029     c                   kfld                    D04pog
056800031029      *
056900031029      * file FIAPD01L
057000031029     c     K02FIAPD      klist
057100031029     c                   kfld                    APDtip
057200031029     c                   kfld                    TR9pdc
057300031029      *
057400031029     c                   ENDSR
057500031113
057600031113      *---------------------------------------------------------------*
057700031113      * SCHIERE A TEMPO DI COMPILAZIONE                               *
057800031113      *---------------------------------------------------------------*
057900031113** - $MSG
058000031113ATTENZIONE: esistono distinte aperte non inserite nel piano.                      1
