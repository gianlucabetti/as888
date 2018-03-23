000100031029      *===============================================================*
000200031029      * FIMS04R * Lista Piano di Smistamento per Picking              *
000300031029      *===============================================================*
000400031029
000500031029     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600031029
000700031029      *---------------------------------------------------------------*
000800031029
000900171103     fFLTR902L  if   e           k disk
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
011800171103     c     K04FLTR9      setll     FLTR9000
011900171103     c     K04FLTR9      reade     FLTR9000
012000031029      *
012100031029      * Ciclo di lettura del file principale
012200171103do  1c                   dow       NOT %eof(FLTR902L)
012300031029      *
012400031029      * - Decodifiche
012500031029     c                   exsr      SR_DECOD
012600031029      *
012700031029      * - Stampa
012800031029     c                   exsr      SR_STAMPA
012900031029      *
013000171103     c     K04FLTR9      reade     FLTR9000
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
019100171103     c     K04FLTR9      chain     FLTR9000
019200171103     c                   eval      *in42    = (%found(FLTR902L)
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
024300171102if  1c*//                if        D04pog <> C_PO_a
024400171102if  1c                   if        D04tps = *blanks
024500060214      *
024600031029     c     'V'           lookup    §MFPap                                 10
024700171102     ***c  N10'C'           lookup    §MFPap                                 10
024800031029     c                   movel     *in10         $VDL
024900060214      *
025000171102x   1c                   else
025100171102      *
025200171102     c                   eval      $VDL = ( D04tps = 'V' )
025300171102      *
025400060214e   1c                   endif
025500031029      *
025600031029     c                   ENDSR
025700040528      *
025800040528      *---------------------------------------------------------------*
025900040528      * Caricamento schiera tab. MPT                                  *
026000040528      *---------------------------------------------------------------*
026100040528     c     Tab_MPT       BEGSR
026200040528      *
026300040528     c                   clear                   XX
026400040528     c                   clear                   $MPT
026500040528     c                   clear                   $MPTd
026600040528      *
026700040528     c                   movel     'MPT'         TBEcod
026800040528     c     TBEcod        setll     TNTBE000
026900040528     c     TBEcod        reade     TNTBE000
027000040528      *
027100040528do  1c                   dow       NOT %eof(TNTBE01L)
027200040528      *
027300040528if  2c                   if        TBEatb  = *blanks
027400040528     c                   movel     TBEuni        dMPT
027500040528     c                   add       1             xx
027600040528     c                   movel     TBEke1        $MPT(xx)
027700040528     c                   movel     dMPT          $MPTd(xx)
027800040528e   2c                   endif
027900040528      *
028000040528     c     TBEcod        reade     TNTBE000
028100040528e   1c                   enddo
028200040528      *
028300040528     c                   ENDSR
028400031029      *
028500031029      *---------------------------------------------------------------*
028600031029      * Decodifiche                                                   *
028700031029      *---------------------------------------------------------------*
028800031029     c     SR_DECOD      BEGSR
028900040528      *
029000040528     c                   clear                   APDrsc
029100040528if  1c                   if        TR9pdc = *zeros
029200040528      * Postazione IMA
029300040528     c                   eval      xx    = 1
029400040528     c                   eval      *in10 = *off
029500040528     c     TR9bai        lookup    $MPT(xx)                               10
029600040528if  2c                   if        *in10 = *on
029700040528     c                   movel     $MPTd(xx)     APDrsc
029800040607x   2c                   else
029900040607      * P.O.
030000040607     c     DigitN        check     TR9bai                                 10
030100040607if  3c                   if        *in10  =  *off
030200040607     c                   move      TR9bai        ORGfil
030300040607     c     ORGfil        chain     AZORG
030400040607if  4c                   if        %found(AZORG01L)
030500040607     c                   movel(p)  ORGdes        APDrsc
030600040607e   4c                   endif
030700040607e   3c                   endif
030800040528e   2c                   endif
030900031029      *
031000040528x   1c                   else
031100040528      *
031200031029      * Autotrasportatore
031300031029     c     K02FIAPD      chain     FIAPD000
031400040528if  2***c                   if        NOT %found(FIAPD01L)
031500040528     ***c                             or  APDatb <> *blanks
031600040528     ***c                   clear                   APDrsc
031700040528e   2***c                   endif
031800040528      *
031900040528e   1c                   endif
032000031029      *
032100031029     c                   ENDSR
032200031029      *
032300031029      *---------------------------------------------------------------*
032400031029      * Stampa singola riga                                           *
032500031029      *---------------------------------------------------------------*
032600031029     c     SR_STAMPA     BEGSR
032700031029      *
032800031029      * Inversione data del foglio viaggio (se piano giornaliero)
032900031029if  1c                   if        $Default = *off
033000031029     c                   clear                   WLBda8
033100031029     c                   reset                   DSdata8
033200031029     c                   reset                   DSdata6
033300031029if  2c                   if        TR9dfv   > *zeros
033400031029     c                   eval      G08inv   = TR9dfv
033500031029     c                   eval      G08err   = '3'
033600031029     c                   call      'XSRDA8'
033700031029     c                   parm                    WLBda8
033800031029     c                   move      G08dat        DSdata8
033900031029     c                   z-add     DSdt8gg       DSdt6gg
034000031029     c                   z-add     DSdt8mm       DSdt6mm
034100031029     c                   z-add     DSdt8aa       DSdt6aa
034200031029e   2c                   endif
034300031029e   1c                   endif
034400031029      *
034500031029if  1c                   if        $VDL     = *off
034600031029      * P.O. senza VDL
034700031029     c                   clear                   MS04D1
034800040705     c                   eval      *in41    = ($Default = *on)
034900031029      * - dati di default (ma anche del piano giornaliero)
035000031029     c                   move      TR9pep        P1Cpep
035100171102     c*//                movel     TR9des        P1Cdes
035200171106     c                   eval      P1Cdes = %subst( TR9des : 1 : 25 )
035300031029     c                   movel     TR9bai        P1Cbai
035400031029     c                   z-add     TR9pdc        P1Cpdc
035500031029     c                   movel     APDrsc        P1Dpdc
035600031029      * - dati specifici aggiuntivi del piano giornaliero
035700031029if  2c                   if        $Default = *off
035800031029     c                   z-add     TR9nfv        P1Cnfv1
035900031029     c                   movel     TR9fl1        P1Cfpp1
036000031029     c                   move      DSdata6       P1Cdfv1
036100031029     c                   exsr      CercaIIfv
036200031029e   2c                   endif
036300031029      *
036400031029x   1c                   else
036500031029      * P.O. con VDL
036600031029     c                   clear                   MS04D2
036700040705     c                   eval      *in41    = ($Default = *on)
036800031029      * - dati di default (ma anche del piano giornaliero)
036900031029     c                   movel     TR9pep        P2Cpep
037000161207     c*//                movel     TR9des        P2Cdes
037100171106     c                   eval      P2Cdes = %subst( TR9des : 1 : 25 )
037200031029     c                   movel     TR9bnl        P2Cbnl
037300040305     c                   movel     TR9bai        P2Cbai
037400170214     c                   if        %subst( TR9des : 29 ) >= *zeros
037500170214     c                   eval      P2Cznc = %subst( TR9des : 29 )
037600161213     c                   endif
037700031029     c                   z-add     TR9pdc        P2Cpdc
037800031029     c                   movel     APDrsc        P2Dpdc
037900031029      * - dati specifici aggiuntivi del piano giornaliero
038000031029if  2c                   if        $Default = *off
038100031107     c                   z-add     TR9nfv        P2Cnfv1
038200031107     c                   movel     TR9fl1        P2Cfpp1
038300031107     c                   move      DSdata6       P2Cdfv1
038400031107     c                   exsr      CercaIIfv
038500031029e   2c                   endif
038600031029      *
038700031029e   1c                   endif
038800031029      *
038900031029      * STAMPA singolo record del Piano di Smistamento
039000031029      *
039100040705if  1c                   if        *in40    = *on
039200031029     c                   write     MS04T0
039300031029if  2c                   if        $VDL     = *off
039400031029     c                   write     MS04T1
039500031029x   2c                   else
039600031029     c                   write     MS04T2
039700031029e   2c                   endif
039800040705     c                   eval      *in40    = *off
039900031029e   1c                   endif
040000031029      *
040100031029if  1c                   if        $VDL     = *off
040200031029     c                   write     MS04D1
040300031029x   1c                   else
040400031029     c                   write     MS04D2
040500031029e   1c                   endif
040600031029      *
040700031029     c                   eval      $Stampa  = *on
040800031029      *
040900031029     c                   ENDSR
041000031029      *
041100031029      *---------------------------------------------------------------*
041200031029      * Reperimento eventuale secondo F.V. per postazione/baia        *
041300031029      *---------------------------------------------------------------*
041400031029     c     CercaIIfv     BEGSR
041500031113      *
041600031113      * memorizzo il F.V. in input tra quelli inseriti nel piano
041700031113if  1c                   if            $VDL    =  *off
041800031113     c                             and TR9bai  <> 'IMA'
041900031113     c                             OR  $VDL    =  *on
042000031113     c                             and TR9bnl  <> 'S'
042100040305     c                             and TR9bai  <> 'IMA'
042200031113     c                   add       1             xx
042300031113     c                   z-add     TR9nfv        $FV(xx)
042400031113e   1c                   endif
042500031029      *
042600040305     c                   add       1             TR9prg
042700171103     c     K07FLTR9      chain     FLTR9000
042800031029      *
042900171103if  1c                   if        %found(FLTR902L)
043000031029      *
043100031029     c                   clear                   WLBda8
043200031029     c                   reset                   DSdata8
043300031029     c                   reset                   DSdata6
043400031029if  2c                   if        TR9dfv   > *zeros
043500031029     c                   eval      G08inv   = TR9dfv
043600031029     c                   eval      G08err   = '3'
043700031029     c                   call      'XSRDA8'
043800031029     c                   parm                    WLBda8
043900031029     c                   move      G08dat        DSdata8
044000031029     c                   z-add     DSdt8gg       DSdt6gg
044100031029     c                   z-add     DSdt8mm       DSdt6mm
044200031029     c                   z-add     DSdt8aa       DSdt6aa
044300031029e   2c                   endif
044400031029      *
044500031107if  2c                   if        $VDL     = *off
044600031029     c                   z-add     TR9nfv        P1Cnfv2
044700031029     c                   movel     TR9fl1        P1Cfpp2
044800031029     c                   move      DSdata6       P1Cdfv2
044900031107x   2c                   else
045000031107     c                   z-add     TR9nfv        P2Cnfv2
045100031107     c                   movel     TR9fl1        P2Cfpp2
045200031107     c                   move      DSdata6       P2Cdfv2
045300031107e   2c                   endif
045400031113      *
045500031113      * memorizzo il F.V. in input tra quelli inseriti nel piano
045600031113if  2c                   if            $VDL    =  *off
045700031113     c                             and TR9bai  <> 'IMA'
045800031113     c                             OR  $VDL    =  *on
045900031113     c                             and TR9bnl  <> 'S'
046000031113     c                   add       1             xx
046100031113     c                   z-add     TR9nfv        $FV(xx)
046200031113e   2c                   endif
046300040305      *
046400040305x   1c                   else
046500031029      *
046600040305      * se non trovo altri f.v. per la locazione in esame:
046700040305      *   riposiziono il ciclo di lettura sull'ultimo record elaborato
046800040305     c                   sub       1             TR9prg
046900171103     c     K07FLTR9      setgt     FLTR9000
047000040305      *
047100031029e   1c                   endif
047200031029      *
047300031029     c                   ENDSR
047400031113      *
047500031113      *---------------------------------------------------------------*
047600031113      * Controllo esistenza di distinte aperte non inserite nel piano *
047700031113      *---------------------------------------------------------------*
047800031113     c     SR_DIST_AP    BEGSR
047900031113      *
048000031113      * Richiamo pgm. FNLR02R per fare il controllo
048100031113     c                   reset                   DSLR01
048200031113     c                   z-add     PT1dfv        DLRgda                         Dal gg/mm/aaaa
048300031113     c                   z-add     FVVdfv        DLRada                         Dal aaaa/mm/gg
048400031113     c                   z-add     D04pog        DLRfgs                         P.O. gestione
048500031113     c                   movel     SDSpgm        DLRpgm                         Nome programma
048600031113     c                   movel(p)  DSLR01        KPJBU
048700031113     c                   call      'FNLR02R'
048800031113     c                   parm                    KPJBA
048900031113     c                   parm                    $FV
049000031113     c                   movel     KPJBU         DSLR01
049100031113      *
049200031113      * Stampo (eventualmente) la segnalazione
049300031113if  1c                   if        DLRfs1 = *on
049400031113      *
049500040705if  2c                   if        *in40    = *on
049600031113     c                   write     MS04T0
049700031113if  3c                   if        $VDL     = *off
049800031113     c                   write     MS04T1
049900031113x   3c                   else
050000031113     c                   write     MS04T2
050100031113e   3c                   endif
050200040705     c                   eval      *in40    = *off
050300031113e   2c                   endif
050400031113      *
050500031113     c                   clear                   MS04N0
050600031113     c                   movel(p)  $Msg(1)       PN0txt
050700031113     c                   write     MS04N0
050800031113      *
050900031113e   1c                   endif
051000031113      *
051100031113     c                   ENDSR
051200031029      *
051300031029      *---------------------------------------------------------------*
051400031029      * Operazioni Finali                                             *
051500031029      *---------------------------------------------------------------*
051600031029     c     SR_ENDPGM     BEGSR
051700031029      *
051800031029      * Segnalazione dell'eventuale mancanza di dati da stampare
051900031029if  1c                   if        $Stampa  = *off
052000031029if  2c                   if        $VDL     = *off
052100031029     c                   write     MS04N1
052200031029x   2c                   else
052300031029     c                   write     MS04N2
052400031029e   2c                   endif
052500031029e   1c                   endif
052600031113      *
052700031113      * Se stampa di piano giornaliero verifico l'esistenza di
052800031113      *   distinte aperte NON inserite nel piano (quindi non listate)
052900031113if  1c                   if        D04nfva > *zeros
053000031113     c                   exsr      SR_DIST_AP
053100031113e   1c                   endif
053200031029      *
053300031029      * Stampa di "Fine Lista"
053400040705if  1c                   if        *in40    = *on
053500031113     c                   write     MS04T0
053600031113if  2c                   if        $VDL     = *off
053700031113     c                   write     MS04T1
053800031113x   2c                   else
053900031113     c                   write     MS04T2
054000031113e   2c                   endif
054100040705     c                   eval      *in40    = *off
054200031113e   1c                   endif
054300031029     c                   write     MS04F0
054400031029      *
054500031029      * Termino
054600031029     c                   eval      *INLR    = *on
054700031029     c                   return
054800031029      *
054900031029     c                   ENDSR
055000031029      *
055100031029      *---------------------------------------------------------------*
055200031029      * Definizione Key-List                                          *
055300031029      *---------------------------------------------------------------*
055400031029     c     DefKLIST      BEGSR
055500031029      *
055600031029      * file FLTR901L
055700171103     c     K04FLTR9      klist
055800031029     c                   kfld                    D04pog
055900031029     c                   kfld                    D04npga
056000031029     c                   kfld                    D04nfva
056100171103     c                   kfld                    D04tps
056200171103     c     K07FLTR9      klist
056300031029     c                   kfld                    TR9fgs
056400031029     c                   kfld                    TR9npga
056500031029     c                   kfld                    TR9nfva
056600171103     c                   kfld                    TR9tps
056700031029     c                   kfld                    TR9pep
056800040305     c                   kfld                    TR9prg
056900031029     c                   kfld                    TR9bai
057000031029      *
057100031029      * file FNFVV01L
057200031029     c     K03FNFVV      klist
057300031029     c                   kfld                    D04npga
057400031029     c                   kfld                    FVVnfv
057500031029     c                   kfld                    D04pog
057600031029      *
057700031029      * file FIAPD01L
057800031029     c     K02FIAPD      klist
057900031029     c                   kfld                    APDtip
058000031029     c                   kfld                    TR9pdc
058100031029      *
058200031029     c                   ENDSR
058300031113
058400031113      *---------------------------------------------------------------*
058500031113      * SCHIERE A TEMPO DI COMPILAZIONE                               *
058600031113      *---------------------------------------------------------------*
058700031113** - $MSG
058800031113ATTENZIONE: esistono distinte aperte non inserite nel piano.                      1
