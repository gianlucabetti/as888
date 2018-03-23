000100131216     /*END
000200060804      *===============================================================*
000300060807      *?TNTA30R * Manutenzione prezzo medio del gasolio              ?*
000400060804      *===============================================================*
000500060804
000600060804     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700060804
000800060804      *---------------------------------------------------------------*
000900060804
001000060807     fTNTBE01L  if   e           k disk
001100060807      *
001200060807     fTIPMG01L  if A e           k disk
001300141120
001400141120      // - File prezzo Base Gasolio
001500141120     fTIDPB00F  o    e             disk
001600060807      *
001700060807     fTNTA30D   cf   e             workstn
001800080226     f                                     infds(InfDspF)
001900060807     f                                     sfile(TA30S01:S01nrr)
002000060804
002100060804      *---------------------------------------------------------------*
002200060804
002300060807      *
002400060807      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002500060807      *
002600060807      * - Tasti di funzione
002700060807     d RollUp          c                   const(x'F5')
002800060807      * - Nr. di righe del sfl (SFLPAG)
002900060807     d C_SflPag        c                   const(14)
003000060804      *
003100060804      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003200060804      *
003300131216     d $Msg            s             78    dim(11)  ctdata  perrcd(1)
003400060807     d $IPG            s                   dim(999) like(PMGsca)  inz
003500060807     d                                              ascend
003600060807     d $IPGmin         s                   dim(999) like(PMGpmg)  inz
003700060807     d                                              ascend
003800060807     d $IPGmax         s                   dim(999) like(PMGpmg)  inz
003900060807     d                                              ascend
004000060804      *
004100060804      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
004200060804      *
004300131216      * - Parametri ricevuti
004400060804     d KPJBA         e ds
004500131216     d TNTA30ds      e ds
004600060804      *
004700060804      * - Parametri x Controllo profilo utenti
004800060804     d TIBS34ds      e ds
004900060804      * - Ds di riferimento al file esterno AZUTE00F
005000060804     d AZUTEds       e ds                  extname(AZUTE00F)
005100060804      * - Ds per dati organigramma
005200060804     d DDatiUte      e ds
005300060804      *
005400060804      * - Parametri x interrogazione tabelle
005500060804     d TIBS02ds      e ds                  inz
005600060804     d   T02mod      e                     inz('C')
005700060804     d   T02cod      e                     inz('IPG')
005800060810      * - Tab. "IPG" = Scaglioni di incremento del prezzo del gasolio
005900060810     d dIPG          e ds                  inz
006000141120
006100141120       // -?Recupero Data/Scaglione da Prezzo gasolio?
006200141120     d TRULDPBDS     e ds                  inz
006300060804      *
006400060804     d Status         sds           333
006500060804     d   SDSpgm          *proc
006600060807      *
006700080226     d InfDspF         ds           512
006800060807     d  £Tasto               369    369
006900080226     d  £SFLnrr              378    379b 0
007000060804      *
007100131216      * - Parametri per controllo data e transform in giorni - XSRDA8 -
007200060804     d WLBdat          ds                  inz
007300060804     d  G08dat                        8  0 inz
007400060804     d  G08inv                        8  0 inz
007500060804     d  G08err                        1    inz('3')
007600060804     d  G08tgi                        5  0 inz
007700060804      *
007800060804      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
007900060804      *
008000131216      * - Flags booleani
008100131216     d $Fine           s               n   inz
008200131216     d $F10            s               n   inz
008300131216     d $InzS01         s               n   inz(*on)
008400131216     d $InzD01         s               n   inz(*on)
008500131216     d $InzD02         s               n   inz(*on)
008600080506     d $First          s               n   inz(*on)
008700131216     d $Called         s               n   inz
008800060807      *
008900060804     d $Video          s              1    inz('1')
009000131216      *
009100060804      * - Indici di schiere
009200060807     d xx              s              5  0 inz
009300060807      *
009400060807      * Campi associati al SFL
009500060807     d S01nrr          s                   like(C01rcd) inz
009600060807     d SavS01csr       s                   like(C01rcd) inz
009700060807     d W_SflPag        s              3  0 inz
009800060807     d wPag            s              4  0 inz
009900060807     d wRig            s              3  0 inz
010000060804      *
010100060804      * - Altri campi
010200131216     d wCountOpz1      s              3  0 inz
010300060804     d wDate           s              8  0 inz
010400061221     d w0060           s              6  0 inz
010500061221     d wDataDMY        s               d   inz  datfmt(*dmy)
010600061221     d wData_1         s               d   inz  datfmt(*jul)
010700061221     d wData_2         s               d   inz  datfmt(*jul)
010800060807     d W1Cdtd          s                   inz  like(PMGdtd)
010900070110     d SAVpmg          s                   inz  like(PMGpmg)
011000070110     ***d SAVsca          s                   inz  like(PMGsca)
011100141120
011200141120       //--------------------------------------------------------------
011300141120       //?Definizione Procedure usate.                                 ?
011400141120       //--------------------------------------------------------------
011500141124       // -?Recupero Data/Scaglione da Prezzo gasolio?
011600141120     d TRULDPBR        pr                  extpgm('TRULDPBR')
011700141120     d   kpjba                             likeds(KPJBA)
011800141120     d   truldpbds                         likeds(TRULDPBDS)
011900141124
012000141124       // -?Pgm interrogazione VMA
012100141124     d TNTA34R         pr                  extpgm('TNTA34R')
012200141124     d   kpjba                             likeds(KPJBA)
012300060804
012400060804      *---------------------------------------------------------------*
012500060804      * I N D I C A T O R I                                           *
012600060804      *---------------------------------------------------------------*
012700131216      *  06     - Abilitato F6=Conferma in D02                        *
012800131216      *  10     - Abilitato F10=Inserimento in D01                    *
012900131216      *  25     - Comodo                                              *
013000060804      *  28     - Emissione messaggio di errore a video               *
013100060807      *  30     - Pulizia del subfile                                 *
013200060807      *  31     - NON emissione del subfile                           *
013300060807      *  32     - Record modificato nel subfile (SflNxtChg)           *
013400060807      *  33     - Fine dati nel subfile         (SflEnd)              *
013500131216      *  40     - Abilitata opz. "1=Seleziona"                        *
013600060807      *  50     - Opzione errata                                      *
013700060804      *  51     - Data decorrenza errata                              *
013800060804      *  52     - Non reperito scaglione per il prezzo medio indicato *
013900060804      *  90     - Generico di errore                                  *
014000060804      *---------------------------------------------------------------*
014100060804
014200060804     c     *Entry        plist
014300060804     c                   parm                    KPJBA
014400131216     c                   parm                    TNTA30ds
014500060804      *
014600060804      * Operazioni Iniziali
014700060804     c                   exsr      RoutInz
014800060804      *
014900060804      * Gestione Video
015000060804do  1c                   dow       $Fine   = *off
015100060807     c     $Video        caseq     '1'           GesS01
015200060807     c     $Video        caseq     '2'           GesD01
015300060807     c     $Video        caseq     '3'           GesD02
015400060807     c                   endcs
015500060807e   1c                   enddo
015600060804      *
015700060804      * Fine
015800060804     c                   movel     *on           *inLR
015900060804      *
016000060804      *---------------------------------------------------------------*
016100060804      * Operazioni Iniziali                                           *
016200060804      *---------------------------------------------------------------*
016300060804     c     RoutInz       BEGSR
016400060804      *
016500060804      * Reperisco dati job
016600060804     c                   exsr      DatiJob
016700060804      *
016800060804     c                   movel     SDSpgm        V1Tpgm
016900060807     c                   clear                   WLBdat
017000060807     c                   eval      G08dat  = *date
017100060807     c                   call      'XSRDA8'
017200060807     c                   parm                    WLBdat
017300060807     c                   movel     G08inv        wDate
017400131216      *
017500131216      * Verifica SE *pgm richiamato per Selezione
017600131216     c                   eval      $Called = (%parms() > 1)
017700131216      *
017800131216      * Pulizia degli eventuali parametri di output
017900131216     c                   if        $Called
018000131216     c                   clear                   oTA30dtd
018100131216     c                   clear                   oTA30fxx
018200131216     c                   eval      oTA30err = *off
018300131216     c                   clear                   oTA30msg
018400131216     c                   endif
018500060804      *
018600060804      * Intabellamento tab. "IPG"
018700060804     c                   clear                   xx
018800060807     c                   clear                   $IPG
018900060807     c                   clear                   $IPGmin
019000060807     c                   clear                   $IPGmax
019100060804     c                   eval      TBEcod  = 'IPG'
019200060804     c     TBEcod        setll     TNTBE000
019300060804     c     TBEcod        reade     TNTBE000
019400060804      *
019500060804do  1c                   DOW       NOT  %eof(TNTBE01L)
019600060804      *
019700060804if  2c                   if        TBEatb  = *blanks
019800060807     c                   movel     TBEuni        dIPG
019900060804     c                   add       1             xx
020000060807     c                   movel     TBEke1        $IPG(xx)
020100060810     c                   z-add     §IPGmin       $IPGmin(xx)
020200060810     c                   z-add     §IPGmax       $IPGmax(xx)
020300060804e   2c                   endif
020400060804      *
020500060804     c     TBEcod        reade     TNTBE000
020600060804      *
020700060804e   1c                   ENDDO
020800060804      *
020900060804     c                   ENDSR
021000060804      *
021100060804      *---------------------------------------------------------------*
021200060804      * Reperimento Dati del job (Utente/Operativi)                   *
021300060804      *---------------------------------------------------------------*
021400060804     c     DatiJob       BEGSR
021500060804      *
021600060804     c     *dtaara       define    §azute        azuteds
021700060804     c     *dtaara       define    §datiute      ddatiute
021800060804      *
021900060804     c                   in(E)     *dtaara
022000060804     c                   IF        %ERROR or RSUT = *blanks
022100060804     c                   clear                   Tibs34Ds
022200060804     c                   call      'TIBS34R'
022300060804     c                   parm                    Tibs34Ds
022400060804     c                   in        *dtaara
022500060804     c                   ENDIF
022600060804      *
022700060804     c                   ENDSR
022800060807      *
022900060807      *---------------------------------------------------------------*
023000060807      * Gestione videata C01/S01                                      *
023100060807      *---------------------------------------------------------------*
023200060807     c     GesS01        BEGSR
023300060807      *
023400060807      * Inizializzo la videata
023500060807if  1c                   if        $InzS01 = *on
023600060807     c                   exsr      InzS01
023700060807     c                   eval      $InzS01     = *off
023800060807e   1c                   endif
023900060807      *
024000060807      * Posiziono il cursore
024100060807if  1c                   if        C01csr  > *zeros
024200060807     c                   z-add     C01csr        C01rcd
024300060807x   1c                   else
024400060807     c                   z-add     1             C01rcd
024500060807e   1c                   endif
024600060807      *
024700060807      * Emissione Testata e Piede con tasti funzionali abilitati
024800060807     c                   write     TA30T01
024900060807     c                   write     TA30P01
025000060807      * Emissione videata
025100060807     c                   exfmt     TA30C01
025200060807     c                   setoff                                       28  90
025300060807     c                   clear                   V1Dmsg
025400131216if  1c                   if        $Called
025500131216     c                   clear                   wCountOpz1
025600131216     c                   clear                   oTA30dtd
025700131216e   1c                   endif
025800060807      *
025900060807sel 1c                   SELECT
026000060807      * F3=Fine
026100060807w   1c                   WHEN      *inKC
026200060807     c                   exsr      F03S01
026300060807      * F10=Inserimento
026400060807w   1c                   WHEN      *inKJ
026500060807     c                   exsr      F10S01
026600060807      * Roll-UP
026700060807w   1c                   WHEN      £Tasto      = RollUp
026800060807     c                   exsr      RollUpS01
026900060807      *
027000060807x   1c                   OTHER
027100060807      * Gestione Opzioni
027200060807     c                   exsr      OpzS01
027300060807if  2c                   if        *in90
027400060807     c                   leavesr
027500060807e   2c                   endif
027600060807      *
027700060807e   1c                   ENDSL
027800060807      *
027900060807     c                   ENDSR
028000060807      *
028100060807      *---------------------------------------------------------------*
028200060807      * Inizializzazione videata C01/S01                              *
028300060807      *---------------------------------------------------------------*
028400060807     c     InzS01        BEGSR
028500060807      *
028600060807      * Pulizia subfile
028700060807     c                   seton                                        3031
028800060807     c                   write     TA30C01
028900060807     c                   setoff                                         3133
029000060807      *
029100060807     c                   clear                   C01rcd
029200060807     c                   clear                   C01csr
029300060807     c                   clear                   S01nrr
029400060807     c                   clear                   V1Dmsg
029500060807     c                   setoff                                       28  90
029600060807     c                   movea     *zeros        *in(51)
029700060807      *
029800060807      * Caricamento dati dal file TIPMG00F
029900131216     c                   if        %parms() < 2  or
030000131216     c                             iTA30dtd = *zero
030100060807     c     *hival        setgt     TIPMG000
030200131216     c                   else
030300131216     c     iTA30dtd      setgt     TIPMG000
030400131216     c                   endif
030500060807     c                   read      TIPMG000
030600060807      *
030700060807     c                   exsr      RollUpS01
030800060807      *
030900060807      * Visualizzazione del SFL (se ci sono dati)
031000080226      * (già impostato in subr. RollUpS01)
031100080226     c***                eval      *in30   = (S01nrr = *zeros)
031200060807      *
031300060807      * Attivazione (eventuale) del SFLEND
031400080226      * (già impostato in subr. RollUpS01)
031500080226     c***                eval      *in33   = %eof(TIPMG01L)
031600060807      *
031700060807      * Posizionamento cursore al 1° rcd della pagina
031800080226      * (già impostato in subr. RollUpS01)
031900080226if  1c***                if        S01nrr  > *zeros
032000080226     c***  S01nrr        div       C_SflPag      wPag
032100080226     c***                mvr                     wRig
032200080226     c***                eval      C01rcd  = wPag * C_SflPag
032300080226     c***                if        wRig    > *zeros
032400080226     c***                eval      C01rcd  = C01rcd + 1
032500080226     c***                else
032600080226     c***                eval      C01rcd  = C01rcd - C_SflPag  + 1
032700080226     c***                endif
032800080226x   1c***                else
032900080226     c***                clear                   C01rcd
033000080226e   1c***                endif
033100080226     c***                eval      C01csr  = C01rcd
033200131216      *
033300131216      * (Dis)Abilitazione tasti funzionali
033400131216      * · F10=Inserimento
033500131216     c                   eval      *in10   = ($Called = *off)
033600131216     c                   eval      *in40   = ($Called = *on  and
033700131216     c                                        iTA30opz = '1')
033800060807      *
033900060807     c                   ENDSR
034000060807      *
034100060807      *---------------------------------------------------------------*
034200060807      * Caricamento pagina successiva di dati                         *
034300060807      *---------------------------------------------------------------*
034400060807     c     RollUpS01     BEGSR
034500070105      *
034600060807     c                   eval      S01nrr  = (W_SflPag * C_SflPag)
034700060807     c                   add       1             W_SflPag
034800060807     c                   eval      *in32   = *off
034900060807      *
035000060807      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
035100060807do  1c                   DOW       NOT  %eof(TIPMG01L)
035200060807     c                             and  S01nrr < (W_SflPag * C_SflPag)
035300060807      *
035400060807     c                   exsr      RieS01
035500060807     c***                eval      *in32   = *on   ->   *off
035600060807     c                   add       1             S01nrr
035700060807     c                   write     TA30S01
035800070105      *
035900070105      * Memorizzazione dell'ultimo scaglione memorizzato
036000070105if  1c                   if        S01nrr  = 1
036100070110     c                   eval      SAVpmg  = PMGpmg
036200070110     ***c                   eval      SAVsca  = PMGsca
036300070105e   1c                   endif
036400060807      *
036500060807     c                   read      TIPMG000
036600060807      *
036700060807e   1c                   ENDDO
036800080226      *
036900080226      * Visualizzazione del SFL (se ci sono dati)
037000080226     c                   eval      *in30   = (S01nrr = *zeros)
037100080226      *
037200080226      * Attivazione (eventuale) del SFLEND
037300080226     c                   eval      *in33   = %eof(TIPMG01L)
037400080226      *
037500080226      * Posizionamento cursore al 1° rcd della pagina
037600080226if  1c                   if        S01nrr  > *zeros
037700080226     c     S01nrr        div       C_SflPag      wPag
037800080226     c                   mvr                     wRig
037900080226     c                   eval      C01rcd  = wPag * C_SflPag
038000080226     c                   if        wRig    > *zeros
038100080226     c                   eval      C01rcd  = C01rcd + 1
038200080226     c                   else
038300080226     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
038400080226     c                   endif
038500080226x   1c                   else
038600080226     c                   clear                   C01rcd
038700080226e   1c                   endif
038800080226     c                   eval      C01csr  = C01rcd
038900060807      *
039000060807     c                   ENDSR
039100060807      *
039200060807      *---------------------------------------------------------------*
039300060807      * Impostazione dati in singolo record del sfl S01               *
039400060807      *---------------------------------------------------------------*
039500060807     c     RieS01        BEGSR
039600060807      *
039700060807     c                   clear                   TA30S01
039800060807      *
039900060807      * Campi hidden
040000060807     c                   eval      S1Cdtd  = PMGdtd
040100060807      *
040200060807     c***                clear                   S1Copz
040300060807      * Data decorrenza
040400131216     c                   eval      H1Ddtd  = PMGdtd
040500060807     c                   reset                   WLBdat
040600060807     c                   eval      G08inv  = PMGdtd
040700060807     c                   call      'XSRDA8'
040800060807     c                   parm                    WLBdat
040900060807     c                   eval      S1Cdtd  = G08dat
041000060807      * Prezzo medio gasolio
041100060807     c                   eval      S1Cpmg  = PMGpmg
041200060807      * Scaglione incrementi
041300060807     c                   eval      S1Csca  = PMGsca
041400060807      * Profilo utente inserimento
041500060807     c                   eval      S1Cute  = PMGute
041600060807      * Data inserimento
041700060807     c                   reset                   WLBdat
041800060807     c                   eval      G08inv  = PMGdti
041900060807     c                   call      'XSRDA8'
042000060807     c                   parm                    WLBdat
042100060807     c                   eval      S1Cdti  = G08dat
042200060807      *
042300060807     c                   ENDSR
042400060807      *
042500060807      *---------------------------------------------------------------*
042600060807      * Gestione tasto funzionale F03 da videata S01                  *
042700060807      *---------------------------------------------------------------*
042800060807     c     F03S01        BEGSR
042900131216      *
043000131216      * Impostazione parametri di output
043100131216     c                   if        $Called
043200131216     c                   eval      oTA30fxx = '03'
043300131216     c                   endif
043400060807      *
043500060807      * Chiusura del programma
043600060807     c                   eval      $Fine   = *on
043700060807      *
043800060807     c                   ENDSR
043900060807      *
044000060807      *---------------------------------------------------------------*
044100060807      * Gestione tasto funzionale F10 da videata S01                  *
044200060807      *---------------------------------------------------------------*
044300060807     c     F10S01        BEGSR
044400060807      *
044500060807      * Passaggio alla videata di immissione (D01)
044600060807     c                   eval      $Video  = '2'
044700060807     c                   eval      $inzD01 = *on
044800060807     c                   eval      $inzD02 = *on
044900060808      *
045000060808     c                   eval      $F10    = *on
045100060807      *
045200060807     c                   ENDSR
045300060807      *
045400060807      *---------------------------------------------------------------*
045500060807      * Gestione opzioni subfile                                      *
045600060807      *---------------------------------------------------------------*
045700060807     c     OpzS01        BEGSR
045800060807      *
045900060807     c                   clear                   SavS01csr
046000060807      *
046100060807     c                   readc     TA30S01
046200060807      *
046300060807do  1c                   DOW       NOT %eof(TNTA30D)
046400060807      *
046500060807     c                   eval      *in32   = *off
046600060807     c                   movea     *zeros        *in(51)
046700060807     c                   z-add     S01nrr        C01rcd
046800060807      *
046900060807sel 2c                   SELECT
047000060807      * - Nessuna opzione
047100060807w   2c                   WHEN      S1Copz  = *blanks
047200131216      * - 1 = Selezionato prezzo medio gasolio
047300131216w   2c                   WHEN      S1Copz  = '1'  and  *in40
047400131216     c                   exsr      sr_OpzS1_1
047500060807      * - 5 = Visualizzazione prezzo medio gasolio
047600060807w   2c                   WHEN      S1Copz  = '5'
047700060807     c                   exsr      sr_OpzS1_5
047800060807      * - ? = Opzione NON valida
047900060807x   2c                   OTHER
048000060807     c                   seton                                        285190
048100070110     c                   eval      V1Dmsg  = $Msg(1)
048200060807e   2c                   ENDSL
048300060807      *
048400060807      * Memorizzazione nrr del sfl con la 1ª opzione
048500060807      * per riposizionarvici il cursore dopo il tasto "Invio"
048600060807if  1c                   if             S1Copz   <> *blanks
048700060807     c                             and (SavS01csr = *zeros
048800060807     c                              or  SavS01csr < C01rcd)
048900060807     c                   eval      SavS01csr   = C01rcd
049000060807e   1c                   endif
049100060807      *
049200060807      * Aggiornamento sfl
049300060807sel 2c                   select
049400060807w   2c                   when      *in28
049500060807     c                   eval      *in32       = *on
049600060807     c                   z-add     C01rcd        C01csr
049700060807w   2c                   when      *in90       = *on
049800060807     c                   z-add     C01rcd        C01csr
049900131216if  3c                   if        S1Copz <> '1'
050000060807     c                   clear                   S1Copz
050100131216x   2c                   else
050200131216     c                   eval      *in32       = *on
050300131216e   3c                   endif
050400060807x   2c                   other
050500131216if  3c                   if        S1Copz <> '1'
050600060807     c                   clear                   S1Copz
050700131216x   2c                   else
050800131216     c                   eval      *in32       = *on
050900131216e   3c                   endif
051000060807e   2c                   endsl
051100060807     c                   UPDATE    TA30S01
051200060807if  2c                   if        *in28  OR  *in90
051300060807     c                   leave
051400060807e   2c                   endif
051500060807      *
051600060807     c                   readc     TA30S01
051700060807      *
051800060807e   1c                   ENDDO
051900060807      *
052000060807      * Riposiziono il cursore sul record contenente la prima opzione
052100060807      *  (se non sono stati rilevati errori)
052200060807if  1c                   if        NOT *in28  and  NOT *in90
052300060807     c                             and SavS01csr  > *zeros
052400060807     c                   z-add     SavS01csr     C01csr
052500060807e   1c                   endif
052600131216      *
052700131216      * Uscita dal *pgm
052800131216      *  (se richiamato per selezionare un prezzo medio  &
052900131216      *      selezionato un solo prezzo medio)
053000131216if  1c                   if        NOT *in28  and  NOT *in90  and
053100131216     c                             $Called    = *on           and
053200131216     c                             wCountOpz1 = 1
053300131216     c                   eval      $Fine = *on
053400131216     c                   eval      *in90 = *on
053500131216e   1c                   endif
053600060807      *
053700060807     c                   ENDSR
053800131216      *
053900131216      *---------------------------------------------------------------*
054000131216      * S1_Opz.1 = Selezionato singolo prezzo medio di gasolio        *
054100131216      *---------------------------------------------------------------*
054200131216     c     sr_OpzS1_1    BEGSR
054300131216      *
054400131216     c                   add       1             wCountOpz1
054500131216      *
054600131216     c                   if        wCountOpz1 > 1
054700131216     c                   reset                   WLBdat
054800131216     c                   eval      G08inv  = oTA30dtd
054900131216     c                   call      'XSRDA8'
055000131216     c                   parm                    WLBdat
055100131216     c                   seton                                        285190
055200131216     c                   eval      V1Dmsg  = %trimr( $Msg(11) ) + ' '
055300131216     c                                     + %editc( G08dat : 'Y' )
055400131216     c                   leavesr
055500131216     c                   endif
055600131216      *
055700131216     c                   eval      oTA30dtd = H1Ddtd
055800131216      *
055900131216     c                   ENDSR
056000060807      *
056100060807      *---------------------------------------------------------------*
056200060807      * S1_Opz.5 = Visualizzazione singolo prezzo medio di gasolio    *
056300060807      *---------------------------------------------------------------*
056400060807     c     sr_OpzS1_5    BEGSR
056500060807      *
056600060807      * Impostazione dati in D01 (solo visualizzato - protetto)
056700060807     c                   clear                   TA30D01
056800060807     c                   eval      V1Cdtd  = S1Cdtd
056900060807     c*** NO !!!         exsr      CtrD01
057000060807     c                   eval      $F10    = *off
057100060807      *
057200060807      * Passaggio alla videata D02 per visualizzazione singolo
057300060807      *   prezzo medio di gasolio
057400060807     c                   eval      $Video  = '3'
057500060807     c                   eval      $InzD02 = *on
057600060807      *
057700060807do  1c                   doW       $Video  = '3'
057800060807     c                   exsr      GesD02
057900060807e   1c                   enddo
058000060807      *
058100060807     c                   ENDSR
058200060804      *
058300060804      *---------------------------------------------------------------*
058400060804      * Gestione videata D01                                          *
058500060804      *---------------------------------------------------------------*
058600060804     c     GesD01        BEGSR
058700060804      *
058800060804      * Inizializzazione videata
058900060804if  1c                   if        $InzD01 = *on
059000060804     c                   exsr      InzD01
059100060804     c                   movel     *off          $InzD01
059200060804e   1c                   endif
059300060804      *
059400060804      * Emissione Testata e Piede con tasti funzionali abilitati
059500060807     c                   eval      *in03   = *off
059600060807     c                   eval      *in06   = *off
059700060807     c                   write     TA30T01
059800060807     c                   write     TA30P02
059900060804      * Emissione videata
060000060807     c                   exfmt     TA30D01
060100060804     c                   setoff                                       28  90
060200060804     c                   clear                   V1Dmsg
060300060804      *
060400060804sel 1c                   SELECT
060500060807      * F12=Ritorno
060600060807w   1c                   WHEN      *inKL
060700060807     c                   exsr      F12D01
060800060804      *
060900060804x   1c                   OTHER
061000060804     c                   exsr      CtrD01
061100060804      *
061200060804if  2c                   if        *in90
061300060804     c                   leavesr
061400060804e   2c                   endif
061500060804      *
061600060804     c                   eval      $InzD02 = *on
061700060807     c                   eval      $Video  = '3'
061800060804      *
061900060804e   1c                   ENDSL
062000060804      *
062100060804     c                   ENDSR
062200060807      *
062300060807      *---------------------------------------------------------------*
062400060807      * Inizializzazione videata D01                                  *
062500060807      *---------------------------------------------------------------*
062600060807     c     InzD01        BEGSR
062700060807      *
062800060807     c                   clear                   TA30D01
062900060807      *
063000060807     c                   ENDSR
063100060807      *
063200060807      *---------------------------------------------------------------*
063300060807      * Gestione tasto funzionale F12 da videata D01                  *
063400060807      *---------------------------------------------------------------*
063500060807     c     F12D01        BEGSR
063600060807      *
063700060807      * Torno alla videata precedente
063800060807     c                   eval      $Video  = '1'
063900060807      *
064000060807     c                   ENDSR
064100060804      *
064200060804      *---------------------------------------------------------------*
064300060804      * Controllo dati immessi in videata D01                         *
064400060804      *---------------------------------------------------------------*
064500060804     c     CtrD01        BEGSR
064600060804      *
064700060804     c                   movea     *zeros        *in(51)
064800060804      *
064900060804      * Controllo formale data
065000060804     c                   clear                   WLBdat
065100060804if  1c                   if        V1Cdtd  > *zeros
065200060804     c                   eval      G08dat  = V1Cdtd
065300060804     c                   call      'XSRDA8'
065400060804     c                   parm                    WLBdat
065500060804x   1c                   else
065600060804     c                   eval      G08err  = *on
065700060804e   1c                   endif
065800060804if  1c                   if        G08err  = *on
065900060804     c                   seton                                        285190
066000070110     c                   eval      V1Dmsg  = $Msg(2)
066100060804     c                   leavesr
066200060804e   1c                   endif
066300060804     c                   eval      V1Cdtd  = G08dat
066400060804     c                   eval      W1Cdtd  = G08inv
066500060804      *
066600060804      * Controllo validità data
066700061221      * (DEVE essere più recente dell'ultima già inserita in TIPMG00F)
066800061221     c     W1Cdtd        setgt     TIPMG000
066900061221     c                   readp     TIPMG000
067000061221if  1c                   if        NOT  %eof(TIPMG01L)
067100061221     c                             and  PMGdtd >= W1Cdtd
067200060808     c                   reset                   WLBdat
067300060808     c                   eval      G08inv  = PMGdtd
067400060808     c                   call      'XSRDA8'
067500060808     c                   parm                    WLBdat
067600060804     c                   seton                                        285190
067700070110     c                   eval      V1Dmsg  = %trim($Msg(3))
067800060808     c                                     + ' '
067900060808     c                                     + %editc(G08dat:'Y')
068000060804     c                   leavesr
068100060808x   1c***                else
068200060808     c*** già così:      eval      $F10    = *on
068300060807e   1c                   endif
068400061221      * (DEVE essere di una settimana successiva a quella nell'ultimo
068500061221      *  record già inserito in TIPMG00F)
068600070110     c***  W1Cdtd        setgt     TIPMG000
068700070110     c     *hival        setgt     TIPMG000
068800061221     c                   read      TIPMG000
068900061221if  1c                   if        NOT  %eof(TIPMG01L)
069000061221     c     *iso          move      PMGdtd        wData_1
069100061221     c     *iso          move      W1Cdtd        wData_2
069200061221if  2c                   if        %diff( wData_2 : wData_1 : *days )
069300061221     c                                    <> 7
069400061221     c                   reset                   WLBdat
069500061221     c                   eval      G08inv  = PMGdtd
069600061221     c                   call      'XSRDA8'
069700061221     c                   parm                    WLBdat
069800061221     c                   seton                                        285190
069900070110     c                   eval      V1Dmsg  = %trim($Msg(4))
070000061221     c                                     + ' '
070100061221     c                                     + %editc(G08dat:'Y')
070200061221     c                   leavesr
070300061221e   2c                   endif
070400061221e   1c                   endif
070500070110      * (NON può essere futura)
070600070110if  1c                   if        W1Cdtd  > Wdate
070700070110     c                   reset                   WLBdat
070800070110     c                   seton                                        285190
070900070110     c                   eval      V1Dmsg  = $Msg(5)
071000070110     c                   leavesr
071100070110e   1c                   endif
071200061221      *
071300061221      * La data decorrenza DEVE essere un lunedì!
071400061221     c     *iso          movel     W1Cdtd        wDataDMY
071500061221     c     *dmy          movel     wDataDMY      w0060
071600061221     c                   movel     w0060         wGgMmAa
071700061221     c                   call      'XGIOSE1'
071800061221     c                   parm                    wGgMmAa           6
071900061221     c                   parm      *blank        wGioSet           1
072000061221     c                   if        wGioSet <> '1'
072100061221     c                   seton                                        285190
072200061221     c                   eval      V1Dmsg  = $Msg(6)
072300061221     c                   leavesr
072400061221     c                   endif
072500060804      *
072600060804     c                   ENDSR
072700060804      *
072800060804      *---------------------------------------------------------------*
072900060804      * Gestione videata D02                                          *
073000060804      *---------------------------------------------------------------*
073100060804     c     GesD02        BEGSR
073200060804      *
073300060804      * Inizializzazione videata
073400060804if  1c                   if        $InzD02 = *on
073500060804     c                   exsr      InzD02
073600060804     c                   movel     *off          $InzD02
073700060804e   1c                   endif
073800060804      *
073900060804      * Emissione Testata e Piede con tasti funzionali abilitati
074000060808      *                   e videata precedente (D01)
074100060808     c                   if        NOT  *in28
074200060808     c                   write     TA30T01
074300060807     c                   write     TA30P02
074400060808     c                   write     TA30D01
074500060808     c                   endif
074600060804      * Emissione videata
074700060807if  1c                   if        $F10    = *off
074800060807     c                   write     TA30D02
074900060807     c                   exfmt     PROTECT
075000060807     c                   else
075100060807     c                   write     PROTECT
075200060807     c                   exfmt     TA30D02
075300060807     c                   endif
075400060804     c                   setoff                                       28  90
075500060804     c                   clear                   V1Dmsg
075600060804      *
075700060804sel 1c                   SELECT
075800060807      * F3=Fine visualizzazioni
075900060807w   1c                   WHEN      *inKC
076000060807     c                   exsr      F03D02
076100060804      * F12=Ritorno
076200060804w   1c                   WHEN      *inKL
076300060804     c                   exsr      F12D02
076400061221      * F13=Interrogazione tab. IPG
076500061221w   1c                   WHEN      *inKM
076600061221     c                   exsr      F13D02
076700060807      * Invio (in visualizzazione)
076800060807w   1c                   WHEN      *in06   = *off
076900060807     c***                leavesr
077000060807      * Invio / F6 (in immissione)
077100060804x   1c                   OTHER
077200060804     c                   exsr      CtrD02
077300060807if  2c                   if        *in90
077400060804     c                   leavesr
077500060804e   2c                   endif
077600060804      * F6=Conferma
077700060804if  2c                   if             *inKF   = *on
077800060804     c                             and  *in06   = *on
077900060804     c                   exsr      Wrt_TIPMG
078000060807if  3c                   if        NOT  *in90
078100070110     c                   eval      SAVpmg  = PMGpmg
078200070110     ***c                   eval      SAVsca  = PMGsca
078300060807     c                   eval      $InzS01 = *on
078400060804     c                   eval      $Video  = '1'
078500060807e   3c                   endif
078600060804e   2c                   endif
078700060804      *
078800060804e   1c                   ENDSL
078900060804      *
079000060804     c                   ENDSR
079100060807      *
079200060807      *---------------------------------------------------------------*
079300060807      * Inizializzazione videata D02                                  *
079400060807      *---------------------------------------------------------------*
079500060807     c     InzD02        BEGSR
079600060807      *
079700060807     c                   clear                   TA30D02
079800080506     c                   reset                   $First
079900060807      *
080000060807      * Impostazione indicatori:
080100060807      * *in03 = abilita F3 per ritorno a S01 senza proseguire con
080200060807      *         l'elaborazione delle opzioni successive
080300060808      *         In fase di immissione consente il ritorno al subfile
080400060808      *         senza passare dalla videata D01...
080500060808     c                   eval      *in03   = *on
080600060807      * *in06 = abilita F6 per inserimento record
080700060807     c                   eval      *in06   = ($F10  = *on)
080800060807      *
080900060807      * Se è stata richiesta l'opzione 5=Visualizzazione
081000060807      *   si reperiscono i dati da visualizzare dal sfl:
081100060807if  1c                   IF             S1Copz = '5'
081200060807     c                             and  $F10   = *off
081300060807      *
081400060807      * Visualizzazione dati
081500060807     c***                eval      V1Cdtd  = S1Cdtd
081600060807     c                   eval      V1Cpmg  = S1Cpmg
081700060807     c                   eval      V1Csca  = S1Csca
081800060807     c                   eval      V1Cute  = S1Cute
081900060807     c                   eval      V1Cdti  = S1Cdti
082000060807      * - Decodifica dati
082100060807      *   (lo scaglione viene memorizzato nel record all'inserimento;
082200060807      *    meglio non eseguire il controllo, perchè potrebbero essere
082300060807      *    stati modificati gli scaglioni DOPO l'inserimento di questo
082400060807      *    record...)
082500060807     c                   eval      xx      = 1
082600131216     c                   eval      *in25   = *off
082700131216     c     V1Csca        lookup    $IPG(xx)                               25
082800131216if  2c                   if        *in25
082900060807     c                   eval      V1Dsca  = 'Valido da '
083000060807     c                                     + %trim(
083100060807     c                                         %editc( $IPGmin(xx) :
083200060807     c                                         '1') )
083300060807     c                                     + ' a '
083400060807     c                                     + %trim(
083500060807     c                                         %editc( $IPGmax(xx) :
083600060807     c                                         '1') )
083700060808     c                                     + ' Euro/Litro'
083800060807e   2c                   endif
083900060807      *
084000060807e   1c                   ENDIF
084100060807      *
084200060807     c                   ENDSR
084300060807      *
084400060807      *---------------------------------------------------------------*
084500060807      * Gestione tasto funzionale F03 da videata D02                  *
084600060807      *---------------------------------------------------------------*
084700060807     c     F03D02        BEGSR
084800060807      *
084900060808      * Ritorno alla 1ª videata (subfile)
085000060807     c                   eval      $Video  = '1'
085100060807     c                   eval      *in90   = *on
085200060807      *
085300060807     c                   ENDSR
085400060804      *
085500060804      *---------------------------------------------------------------*
085600060804      * Gestione tasto funzionale F12 da videata D02                  *
085700060804      *---------------------------------------------------------------*
085800060804     c     F12D02        BEGSR
085900060804      *
086000060804      * Ritorno alla videata precedente
086100060807     c                   if        *in06   = *on
086200060807     c                   eval      $Video  = '2'
086300060807     c                   else
086400060807     c                   eval      $Video  = '1'
086500060807     c                   endif
086600060804      *
086700060804     c                   ENDSR
086800061221      *
086900061221      *---------------------------------------------------------------*
087000061221      * Gestione tasto funzionale F13 da videata D02                  *
087100061221      *---------------------------------------------------------------*
087200061221     c     F13D02        BEGSR
087300061221      *
087400061221      * Richiamo pgm. TNTB58R per interrogazione tab. "IPG"
087500061221      * (scaglioni di incremento del prezzo del gasolio)
087600061221     c                   movel(p)  '05'          KPJBU
087700061221     c                   call      'TNTB58R'
087800061221     c                   parm                    KPJBA
087900061221      *
088000061221     c                   ENDSR
088100060804      *
088200060804      *---------------------------------------------------------------*
088300060804      * Controllo dati immessi in videata D02                         *
088400060804      *---------------------------------------------------------------*
088500060804     c     CtrD02        BEGSR
088600060804      *
088700060804     c                   movea     *zeros        *in(51)
088800080506     c                   clear                   V1Dtxt
088900060804      *
089000060804      * Controllo prezzo medio
089100060804      * reperendo lo scaglione di incremento relativo al prezzo
089200060808      * (considerando che NON ci DOVREBBERO essere periodi "in comune"
089300060808      *  in tab. "IPG", dovrebbe essere sufficiente la LOOKUP)
089400061221      * - Prezzo medio obbligatorio
089500061221if  1c                   if        V1Cpmg <= *zeros
089600061221     c                   seton                                        285190
089700070110     c                   eval      V1Dmsg  = $Msg(7)
089800061221     c                   leavesr
089900061221e   1c                   endif
090000060807     c                   clear                   V1Dsca
090100070105     c                   eval      xx      = 1
090200131216     c                   eval      *in25   = *off
090300131216     c     V1Cpmg        lookup    $IPGmax(xx)                        25  25
090400060808sel 1c                   SELECT
090500060808      * - Scaglione non reperito (x importo né maggiore né uguale)
090600131216w   1c                   WHEN      NOT *in25
090700060808     c                   seton                                        285190
090800070110     c                   eval      V1Dmsg  = $Msg(8)
090900060808     c                   leavesr
091000060808      * - Scaglione reperito
091100060808      *   (importo V1CPMG compreso nei range in tab. "IPG")
091200070105w   1c                   WHEN      V1Cpmg >= $IPGmin(xx)
091300070105     c                   eval      V1Csca  = $IPG(xx)
091400070105     c                   eval      V1Dsca  = 'Valido da '
091500070105     c                                     + %trim(
091600070105     c                                         %editc( $IPGmin(xx) :
091700070105     c                                         '1') )
091800070105     c                                     + ' a '
091900070105     c                                     + %trim(
092000070105     c                                         %editc( $IPGmax(xx) :
092100070105     c                                         '1') )
092200070105     c                                     + ' Euro/Litro'
092300070110     *** * - - Aumento max consentito di 2 (due) scaglioni
092400070110if  2***c                   if             V1Csca > (SAVsca + 2)
092500070110     ***c                             or   V1Csca < (SAVsca - 2)
092600070110      * - - Consentita una differenza max del ±2% rispetto all'ultimo
092700070110      *     prezzo medio inserito
092800090107      *     => Portato delta massimo al ±4%.
092900070110if  2c                   if             SAVpmg > *zeros
093000070110     c                             and (V1Cpmg > ( SAVpmg +
093100090107     c                                           ((SAVpmg * 4) / 100))
093200070110     c                              or  V1Cpmg < ( SAVpmg -
093300090107     c                                           ((SAVpmg * 4) / 100)))
093400080506if  3c                   if        %subst(knmus : 1 : 3) <> 'EDP'
093500070105     c                   seton                                        285590
093600070105     c                   eval      V1Dmsg  = $Msg(9)
093700070105     c                   leavesr
093800080506x   3c                   else
093900080506     c                   eval      V1Dtxt  = 'N.B.: '
094000080506     c                                     + %subst($Msg(9) : 1 : 58)
094100080506     c                   if        *inKF   and   $First = *on
094200080506     c                   eval      *in90   = *on
094300080506     c                   eval      $First  = *off
094400080506     c                   endif
094500080506e   3c                   endif
094600070105e   2c                   endif
094700060808      * - Scaglione reperito, ma con limite massimo inferiore
094800060808      *   all'importo in V1CPMG -DOVREBBE?essere impossibile!!!
094900060808x   1c                   OTHER
095000060808     c                   seton                                        285190
095100070110     c                   eval      V1Dmsg  = $Msg(8)
095200060808     c                   leavesr
095300060808e   1c                   ENDSL
095400060804      *
095500060804     c                   ENDSR
095600060804      *
095700060804      *---------------------------------------------------------------*
095800060804      * Scrittura record in file TIPMG00F                             *
095900060804      *---------------------------------------------------------------*
096000060804     c     Wrt_TIPMG     BEGSR
096100060804      *
096200060804     c                   clear                   TIPMG000
096300060804      *
096400060804     c                   eval      PMGdtd = W1Cdtd
096500060804     c                   eval      PMGpmg = V1Cpmg
096600060804     c                   eval      PMGsca = V1Csca
096700060804     c                   eval      PMGute = KNMUS
096800060804     c                   eval      PMGdti = wDate
096900060804      *                  __________________
097000060807     c                   WRITE(e)  TIPMG000
097100060804      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
097200060807     c                   if        %error
097300060807     c                   eval      $Video = '2'
097400060807     c                   seton                                        285190
097500070110     c                   eval      V1Dmsg = $Msg(10)
097600060807     c*** superfluo:     leavesr
097700141120     c                   leavesr
097800060807     c                   endif
097900141120
098000141120      /free
098100141120       //?se tutto ok scrivo in automatico nuovo rcd del VMA
098200141120       //?Richiamo pgm per verificare se devo scrivere nuovo rcd
098300141120       //?il TRUL torna anche i dati da scrivere su TIPDB00F
098400141120         clear TRULDPBDS;
098500141120         IDPBsca = PMGsca;
098600141120         truldpbr (kpjba:TRULDPBDS);
098700141120       //?se torna errore vuol dire che ho sbagliato lo scaglione
098800141120       //?oppure che non devo scrivere il nuovo rcd
098900141124         IF  ODPBerr = *blanks;
099000141120       //?Scrivo il nuovo rcd
099100141124           clear  TIDPB000;
099200141124           DPBdtd = wDate;
099300141124           DPBdpb = ODPBdpb;
099400141124           DPBpma = ODPBpma;
099500141124           DPBvma = ODPBvma;
099600141124           DPBsvm = ODPBsvm;
099700141124           DPBute = knmus;
099800141124           DPBdti = wDate;
099900141124           write TIDPB000;
100000141124         ENDIF;
100100141124       //?Richiamo pgm interrogazione VMA
100200141124         clear kpjbu;
100300141124         %subst(kpjbu:1:1) = 'R';
100400141124         %subst(kpjbu:2:78) = ODPBmsg;
100500141124         tnta34r (kpjba);
100600141120      /end-free
100700060807      *
100800060804     c                   ENDSR
100900060804
101000060804** - $MSG -------------------------------------------------------------------*
101100070110Opzione errata                                                                 1
101200070110Data formalmente errata                                                        2
101300070110Data decorrenza NON succesiva a quella dell'ultimo record inserito:            3
101400070110Questa data DEVE essere di 7 gg. successiva all'ultima inserita:               4
101500070110Non ammesse decorrenze "future"                                                5
101600070110La data decorrenza DEVE essere un lunedì                                       6
101700070110Prezzo medio obbligatorio                                                      7
101800070110Scaglione di incremento non reperito per questo prezzo medio: AVVISARE IL CED! 8
101900090107Prezzo medio differente all'ultimo inserito di OLTRE il 4%: AVVISARE IL CED!   9
102000070110Rilevati errori in fase di scrittura record nel file TIPMG00F                 10
102100131216Già selezionato il prezzo medio al giorno                                     11
