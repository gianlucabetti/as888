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
001300060807      *
001400060807     fTNTA30D   cf   e             workstn
001500080226     f                                     infds(InfDspF)
001600060807     f                                     sfile(TA30S01:S01nrr)
001700060804
001800060804      *---------------------------------------------------------------*
001900060804
002000060807      *
002100060807      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002200060807      *
002300060807      * - Tasti di funzione
002400060807     d RollUp          c                   const(x'F5')
002500060807      * - Nr. di righe del sfl (SFLPAG)
002600060807     d C_SflPag        c                   const(14)
002700060804      *
002800060804      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002900060804      *
003000131216     d $Msg            s             78    dim(11)  ctdata  perrcd(1)
003100060807     d $IPG            s                   dim(999) like(PMGsca)  inz
003200060807     d                                              ascend
003300060807     d $IPGmin         s                   dim(999) like(PMGpmg)  inz
003400060807     d                                              ascend
003500060807     d $IPGmax         s                   dim(999) like(PMGpmg)  inz
003600060807     d                                              ascend
003700060804      *
003800060804      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003900060804      *
004000131216      * - Parametri ricevuti
004100060804     d KPJBA         e ds
004200131216     d TNTA30ds      e ds
004300060804      *
004400060804      * - Parametri x Controllo profilo utenti
004500060804     d TIBS34ds      e ds
004600060804      * - Ds di riferimento al file esterno AZUTE00F
004700060804     d AZUTEds       e ds                  extname(AZUTE00F)
004800060804      * - Ds per dati organigramma
004900060804     d DDatiUte      e ds
005000060804      *
005100060804      * - Parametri x interrogazione tabelle
005200060804     d TIBS02ds      e ds                  inz
005300060804     d   T02mod      e                     inz('C')
005400060804     d   T02cod      e                     inz('IPG')
005500060810      * - Tab. "IPG" = Scaglioni di incremento del prezzo del gasolio
005600060810     d dIPG          e ds                  inz
005700060804      *
005800060804     d Status         sds           333
005900060804     d   SDSpgm          *proc
006000060807      *
006100080226     d InfDspF         ds           512
006200060807     d  £Tasto               369    369
006300080226     d  £SFLnrr              378    379b 0
006400060804      *
006500131216      * - Parametri per controllo data e transform in giorni - XSRDA8 -
006600060804     d WLBdat          ds                  inz
006700060804     d  G08dat                        8  0 inz
006800060804     d  G08inv                        8  0 inz
006900060804     d  G08err                        1    inz('3')
007000060804     d  G08tgi                        5  0 inz
007100060804      *
007200060804      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
007300060804      *
007400131216      * - Flags booleani
007500131216     d $Fine           s               n   inz
007600131216     d $F10            s               n   inz
007700131216     d $InzS01         s               n   inz(*on)
007800131216     d $InzD01         s               n   inz(*on)
007900131216     d $InzD02         s               n   inz(*on)
008000080506     d $First          s               n   inz(*on)
008100131216     d $Called         s               n   inz
008200060807      *
008300060804     d $Video          s              1    inz('1')
008400131216      *
008500060804      * - Indici di schiere
008600060807     d xx              s              5  0 inz
008700060807      *
008800060807      * Campi associati al SFL
008900060807     d S01nrr          s                   like(C01rcd) inz
009000060807     d SavS01csr       s                   like(C01rcd) inz
009100060807     d W_SflPag        s              3  0 inz
009200060807     d wPag            s              4  0 inz
009300060807     d wRig            s              3  0 inz
009400060804      *
009500060804      * - Altri campi
009600131216     d wCountOpz1      s              3  0 inz
009700060804     d wDate           s              8  0 inz
009800061221     d w0060           s              6  0 inz
009900061221     d wDataDMY        s               d   inz  datfmt(*dmy)
010000061221     d wData_1         s               d   inz  datfmt(*jul)
010100061221     d wData_2         s               d   inz  datfmt(*jul)
010200060807     d W1Cdtd          s                   inz  like(PMGdtd)
010300070110     d SAVpmg          s                   inz  like(PMGpmg)
010400070110     ***d SAVsca          s                   inz  like(PMGsca)
010500060804
010600060804      *---------------------------------------------------------------*
010700060804      * I N D I C A T O R I                                           *
010800060804      *---------------------------------------------------------------*
010900131216      *  06     - Abilitato F6=Conferma in D02                        *
011000131216      *  10     - Abilitato F10=Inserimento in D01                    *
011100131216      *  25     - Comodo                                              *
011200060804      *  28     - Emissione messaggio di errore a video               *
011300060807      *  30     - Pulizia del subfile                                 *
011400060807      *  31     - NON emissione del subfile                           *
011500060807      *  32     - Record modificato nel subfile (SflNxtChg)           *
011600060807      *  33     - Fine dati nel subfile         (SflEnd)              *
011700131216      *  40     - Abilitata opz. "1=Seleziona"                        *
011800060807      *  50     - Opzione errata                                      *
011900060804      *  51     - Data decorrenza errata                              *
012000060804      *  52     - Non reperito scaglione per il prezzo medio indicato *
012100060804      *  90     - Generico di errore                                  *
012200060804      *---------------------------------------------------------------*
012300060804
012400060804     c     *Entry        plist
012500060804     c                   parm                    KPJBA
012600131216     c                   parm                    TNTA30ds
012700060804      *
012800060804      * Operazioni Iniziali
012900060804     c                   exsr      RoutInz
013000060804      *
013100060804      * Gestione Video
013200060804do  1c                   dow       $Fine   = *off
013300060807     c     $Video        caseq     '1'           GesS01
013400060807     c     $Video        caseq     '2'           GesD01
013500060807     c     $Video        caseq     '3'           GesD02
013600060807     c                   endcs
013700060807e   1c                   enddo
013800060804      *
013900060804      * Fine
014000060804     c                   movel     *on           *inLR
014100060804      *
014200060804      *---------------------------------------------------------------*
014300060804      * Operazioni Iniziali                                           *
014400060804      *---------------------------------------------------------------*
014500060804     c     RoutInz       BEGSR
014600060804      *
014700060804      * Reperisco dati job
014800060804     c                   exsr      DatiJob
014900060804      *
015000060804     c                   movel     SDSpgm        V1Tpgm
015100060807     c                   clear                   WLBdat
015200060807     c                   eval      G08dat  = *date
015300060807     c                   call      'XSRDA8'
015400060807     c                   parm                    WLBdat
015500060807     c                   movel     G08inv        wDate
015600131216      *
015700131216      * Verifica SE *pgm richiamato per Selezione
015800131216     c                   eval      $Called = (%parms() > 1)
015900131216      *
016000131216      * Pulizia degli eventuali parametri di output
016100131216     c                   if        $Called
016200131216     c                   clear                   oTA30dtd
016300131216     c                   clear                   oTA30fxx
016400131216     c                   eval      oTA30err = *off
016500131216     c                   clear                   oTA30msg
016600131216     c                   endif
016700060804      *
016800060804      * Intabellamento tab. "IPG"
016900060804     c                   clear                   xx
017000060807     c                   clear                   $IPG
017100060807     c                   clear                   $IPGmin
017200060807     c                   clear                   $IPGmax
017300060804     c                   eval      TBEcod  = 'IPG'
017400060804     c     TBEcod        setll     TNTBE000
017500060804     c     TBEcod        reade     TNTBE000
017600060804      *
017700060804do  1c                   DOW       NOT  %eof(TNTBE01L)
017800060804      *
017900060804if  2c                   if        TBEatb  = *blanks
018000060807     c                   movel     TBEuni        dIPG
018100060804     c                   add       1             xx
018200060807     c                   movel     TBEke1        $IPG(xx)
018300060810     c                   z-add     §IPGmin       $IPGmin(xx)
018400060810     c                   z-add     §IPGmax       $IPGmax(xx)
018500060804e   2c                   endif
018600060804      *
018700060804     c     TBEcod        reade     TNTBE000
018800060804      *
018900060804e   1c                   ENDDO
019000060804      *
019100060804     c                   ENDSR
019200060804      *
019300060804      *---------------------------------------------------------------*
019400060804      * Reperimento Dati del job (Utente/Operativi)                   *
019500060804      *---------------------------------------------------------------*
019600060804     c     DatiJob       BEGSR
019700060804      *
019800060804     c     *dtaara       define    §azute        azuteds
019900060804     c     *dtaara       define    §datiute      ddatiute
020000060804      *
020100060804     c                   in(E)     *dtaara
020200060804     c                   IF        %ERROR or RSUT = *blanks
020300060804     c                   clear                   Tibs34Ds
020400060804     c                   call      'TIBS34R'
020500060804     c                   parm                    Tibs34Ds
020600060804     c                   in        *dtaara
020700060804     c                   ENDIF
020800060804      *
020900060804     c                   ENDSR
021000060807      *
021100060807      *---------------------------------------------------------------*
021200060807      * Gestione videata C01/S01                                      *
021300060807      *---------------------------------------------------------------*
021400060807     c     GesS01        BEGSR
021500060807      *
021600060807      * Inizializzo la videata
021700060807if  1c                   if        $InzS01 = *on
021800060807     c                   exsr      InzS01
021900060807     c                   eval      $InzS01     = *off
022000060807e   1c                   endif
022100060807      *
022200060807      * Posiziono il cursore
022300060807if  1c                   if        C01csr  > *zeros
022400060807     c                   z-add     C01csr        C01rcd
022500060807x   1c                   else
022600060807     c                   z-add     1             C01rcd
022700060807e   1c                   endif
022800060807      *
022900060807      * Emissione Testata e Piede con tasti funzionali abilitati
023000060807     c                   write     TA30T01
023100060807     c                   write     TA30P01
023200060807      * Emissione videata
023300060807     c                   exfmt     TA30C01
023400060807     c                   setoff                                       28  90
023500060807     c                   clear                   V1Dmsg
023600131216if  1c                   if        $Called
023700131216     c                   clear                   wCountOpz1
023800131216     c                   clear                   oTA30dtd
023900131216e   1c                   endif
024000060807      *
024100060807sel 1c                   SELECT
024200060807      * F3=Fine
024300060807w   1c                   WHEN      *inKC
024400060807     c                   exsr      F03S01
024500060807      * F10=Inserimento
024600060807w   1c                   WHEN      *inKJ
024700060807     c                   exsr      F10S01
024800060807      * Roll-UP
024900060807w   1c                   WHEN      £Tasto      = RollUp
025000060807     c                   exsr      RollUpS01
025100060807      *
025200060807x   1c                   OTHER
025300060807      * Gestione Opzioni
025400060807     c                   exsr      OpzS01
025500060807if  2c                   if        *in90
025600060807     c                   leavesr
025700060807e   2c                   endif
025800060807      *
025900060807e   1c                   ENDSL
026000060807      *
026100060807     c                   ENDSR
026200060807      *
026300060807      *---------------------------------------------------------------*
026400060807      * Inizializzazione videata C01/S01                              *
026500060807      *---------------------------------------------------------------*
026600060807     c     InzS01        BEGSR
026700060807      *
026800060807      * Pulizia subfile
026900060807     c                   seton                                        3031
027000060807     c                   write     TA30C01
027100060807     c                   setoff                                         3133
027200060807      *
027300060807     c                   clear                   C01rcd
027400060807     c                   clear                   C01csr
027500060807     c                   clear                   S01nrr
027600060807     c                   clear                   V1Dmsg
027700060807     c                   setoff                                       28  90
027800060807     c                   movea     *zeros        *in(51)
027900060807      *
028000060807      * Caricamento dati dal file TIPMG00F
028100131216     c                   if        %parms() < 2  or
028200131216     c                             iTA30dtd = *zero
028300060807     c     *hival        setgt     TIPMG000
028400131216     c                   else
028500131216     c     iTA30dtd      setgt     TIPMG000
028600131216     c                   endif
028700060807     c                   read      TIPMG000
028800060807      *
028900060807     c                   exsr      RollUpS01
029000060807      *
029100060807      * Visualizzazione del SFL (se ci sono dati)
029200080226      * (già impostato in subr. RollUpS01)
029300080226     c***                eval      *in30   = (S01nrr = *zeros)
029400060807      *
029500060807      * Attivazione (eventuale) del SFLEND
029600080226      * (già impostato in subr. RollUpS01)
029700080226     c***                eval      *in33   = %eof(TIPMG01L)
029800060807      *
029900060807      * Posizionamento cursore al 1° rcd della pagina
030000080226      * (già impostato in subr. RollUpS01)
030100080226if  1c***                if        S01nrr  > *zeros
030200080226     c***  S01nrr        div       C_SflPag      wPag
030300080226     c***                mvr                     wRig
030400080226     c***                eval      C01rcd  = wPag * C_SflPag
030500080226     c***                if        wRig    > *zeros
030600080226     c***                eval      C01rcd  = C01rcd + 1
030700080226     c***                else
030800080226     c***                eval      C01rcd  = C01rcd - C_SflPag  + 1
030900080226     c***                endif
031000080226x   1c***                else
031100080226     c***                clear                   C01rcd
031200080226e   1c***                endif
031300080226     c***                eval      C01csr  = C01rcd
031400131216      *
031500131216      * (Dis)Abilitazione tasti funzionali
031600131216      * · F10=Inserimento
031700131216     c                   eval      *in10   = ($Called = *off)
031800131216     c                   eval      *in40   = ($Called = *on  and
031900131216     c                                        iTA30opz = '1')
032000060807      *
032100060807     c                   ENDSR
032200060807      *
032300060807      *---------------------------------------------------------------*
032400060807      * Caricamento pagina successiva di dati                         *
032500060807      *---------------------------------------------------------------*
032600060807     c     RollUpS01     BEGSR
032700070105      *
032800060807     c                   eval      S01nrr  = (W_SflPag * C_SflPag)
032900060807     c                   add       1             W_SflPag
033000060807     c                   eval      *in32   = *off
033100060807      *
033200060807      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
033300060807do  1c                   DOW       NOT  %eof(TIPMG01L)
033400060807     c                             and  S01nrr < (W_SflPag * C_SflPag)
033500060807      *
033600060807     c                   exsr      RieS01
033700060807     c***                eval      *in32   = *on   ->   *off
033800060807     c                   add       1             S01nrr
033900060807     c                   write     TA30S01
034000070105      *
034100070105      * Memorizzazione dell'ultimo scaglione memorizzato
034200070105if  1c                   if        S01nrr  = 1
034300070110     c                   eval      SAVpmg  = PMGpmg
034400070110     ***c                   eval      SAVsca  = PMGsca
034500070105e   1c                   endif
034600060807      *
034700060807     c                   read      TIPMG000
034800060807      *
034900060807e   1c                   ENDDO
035000080226      *
035100080226      * Visualizzazione del SFL (se ci sono dati)
035200080226     c                   eval      *in30   = (S01nrr = *zeros)
035300080226      *
035400080226      * Attivazione (eventuale) del SFLEND
035500080226     c                   eval      *in33   = %eof(TIPMG01L)
035600080226      *
035700080226      * Posizionamento cursore al 1° rcd della pagina
035800080226if  1c                   if        S01nrr  > *zeros
035900080226     c     S01nrr        div       C_SflPag      wPag
036000080226     c                   mvr                     wRig
036100080226     c                   eval      C01rcd  = wPag * C_SflPag
036200080226     c                   if        wRig    > *zeros
036300080226     c                   eval      C01rcd  = C01rcd + 1
036400080226     c                   else
036500080226     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
036600080226     c                   endif
036700080226x   1c                   else
036800080226     c                   clear                   C01rcd
036900080226e   1c                   endif
037000080226     c                   eval      C01csr  = C01rcd
037100060807      *
037200060807     c                   ENDSR
037300060807      *
037400060807      *---------------------------------------------------------------*
037500060807      * Impostazione dati in singolo record del sfl S01               *
037600060807      *---------------------------------------------------------------*
037700060807     c     RieS01        BEGSR
037800060807      *
037900060807     c                   clear                   TA30S01
038000060807      *
038100060807      * Campi hidden
038200060807     c                   eval      S1Cdtd  = PMGdtd
038300060807      *
038400060807     c***                clear                   S1Copz
038500060807      * Data decorrenza
038600131216     c                   eval      H1Ddtd  = PMGdtd
038700060807     c                   reset                   WLBdat
038800060807     c                   eval      G08inv  = PMGdtd
038900060807     c                   call      'XSRDA8'
039000060807     c                   parm                    WLBdat
039100060807     c                   eval      S1Cdtd  = G08dat
039200060807      * Prezzo medio gasolio
039300060807     c                   eval      S1Cpmg  = PMGpmg
039400060807      * Scaglione incrementi
039500060807     c                   eval      S1Csca  = PMGsca
039600060807      * Profilo utente inserimento
039700060807     c                   eval      S1Cute  = PMGute
039800060807      * Data inserimento
039900060807     c                   reset                   WLBdat
040000060807     c                   eval      G08inv  = PMGdti
040100060807     c                   call      'XSRDA8'
040200060807     c                   parm                    WLBdat
040300060807     c                   eval      S1Cdti  = G08dat
040400060807      *
040500060807     c                   ENDSR
040600060807      *
040700060807      *---------------------------------------------------------------*
040800060807      * Gestione tasto funzionale F03 da videata S01                  *
040900060807      *---------------------------------------------------------------*
041000060807     c     F03S01        BEGSR
041100131216      *
041200131216      * Impostazione parametri di output
041300131216     c                   if        $Called
041400131216     c                   eval      oTA30fxx = '03'
041500131216     c                   endif
041600060807      *
041700060807      * Chiusura del programma
041800060807     c                   eval      $Fine   = *on
041900060807      *
042000060807     c                   ENDSR
042100060807      *
042200060807      *---------------------------------------------------------------*
042300060807      * Gestione tasto funzionale F10 da videata S01                  *
042400060807      *---------------------------------------------------------------*
042500060807     c     F10S01        BEGSR
042600060807      *
042700060807      * Passaggio alla videata di immissione (D01)
042800060807     c                   eval      $Video  = '2'
042900060807     c                   eval      $inzD01 = *on
043000060807     c                   eval      $inzD02 = *on
043100060808      *
043200060808     c                   eval      $F10    = *on
043300060807      *
043400060807     c                   ENDSR
043500060807      *
043600060807      *---------------------------------------------------------------*
043700060807      * Gestione opzioni subfile                                      *
043800060807      *---------------------------------------------------------------*
043900060807     c     OpzS01        BEGSR
044000060807      *
044100060807     c                   clear                   SavS01csr
044200060807      *
044300060807     c                   readc     TA30S01
044400060807      *
044500060807do  1c                   DOW       NOT %eof(TNTA30D)
044600060807      *
044700060807     c                   eval      *in32   = *off
044800060807     c                   movea     *zeros        *in(51)
044900060807     c                   z-add     S01nrr        C01rcd
045000060807      *
045100060807sel 2c                   SELECT
045200060807      * - Nessuna opzione
045300060807w   2c                   WHEN      S1Copz  = *blanks
045400131216      * - 1 = Selezionato prezzo medio gasolio
045500131216w   2c                   WHEN      S1Copz  = '1'  and  *in40
045600131216     c                   exsr      sr_OpzS1_1
045700060807      * - 5 = Visualizzazione prezzo medio gasolio
045800060807w   2c                   WHEN      S1Copz  = '5'
045900060807     c                   exsr      sr_OpzS1_5
046000060807      * - ? = Opzione NON valida
046100060807x   2c                   OTHER
046200060807     c                   seton                                        285190
046300070110     c                   eval      V1Dmsg  = $Msg(1)
046400060807e   2c                   ENDSL
046500060807      *
046600060807      * Memorizzazione nrr del sfl con la 1ª opzione
046700060807      * per riposizionarvici il cursore dopo il tasto "Invio"
046800060807if  1c                   if             S1Copz   <> *blanks
046900060807     c                             and (SavS01csr = *zeros
047000060807     c                              or  SavS01csr < C01rcd)
047100060807     c                   eval      SavS01csr   = C01rcd
047200060807e   1c                   endif
047300060807      *
047400060807      * Aggiornamento sfl
047500060807sel 2c                   select
047600060807w   2c                   when      *in28
047700060807     c                   eval      *in32       = *on
047800060807     c                   z-add     C01rcd        C01csr
047900060807w   2c                   when      *in90       = *on
048000060807     c                   z-add     C01rcd        C01csr
048100131216if  3c                   if        S1Copz <> '1'
048200060807     c                   clear                   S1Copz
048300131216x   2c                   else
048400131216     c                   eval      *in32       = *on
048500131216e   3c                   endif
048600060807x   2c                   other
048700131216if  3c                   if        S1Copz <> '1'
048800060807     c                   clear                   S1Copz
048900131216x   2c                   else
049000131216     c                   eval      *in32       = *on
049100131216e   3c                   endif
049200060807e   2c                   endsl
049300060807     c                   UPDATE    TA30S01
049400060807if  2c                   if        *in28  OR  *in90
049500060807     c                   leave
049600060807e   2c                   endif
049700060807      *
049800060807     c                   readc     TA30S01
049900060807      *
050000060807e   1c                   ENDDO
050100060807      *
050200060807      * Riposiziono il cursore sul record contenente la prima opzione
050300060807      *  (se non sono stati rilevati errori)
050400060807if  1c                   if        NOT *in28  and  NOT *in90
050500060807     c                             and SavS01csr  > *zeros
050600060807     c                   z-add     SavS01csr     C01csr
050700060807e   1c                   endif
050800131216      *
050900131216      * Uscita dal *pgm
051000131216      *  (se richiamato per selezionare un prezzo medio  &
051100131216      *      selezionato un solo prezzo medio)
051200131216if  1c                   if        NOT *in28  and  NOT *in90  and
051300131216     c                             $Called    = *on           and
051400131216     c                             wCountOpz1 = 1
051500131216     c                   eval      $Fine = *on
051600131216     c                   eval      *in90 = *on
051700131216e   1c                   endif
051800060807      *
051900060807     c                   ENDSR
052000131216      *
052100131216      *---------------------------------------------------------------*
052200131216      * S1_Opz.1 = Selezionato singolo prezzo medio di gasolio        *
052300131216      *---------------------------------------------------------------*
052400131216     c     sr_OpzS1_1    BEGSR
052500131216      *
052600131216     c                   add       1             wCountOpz1
052700131216      *
052800131216     c                   if        wCountOpz1 > 1
052900131216     c                   reset                   WLBdat
053000131216     c                   eval      G08inv  = oTA30dtd
053100131216     c                   call      'XSRDA8'
053200131216     c                   parm                    WLBdat
053300131216     c                   seton                                        285190
053400131216     c                   eval      V1Dmsg  = %trimr( $Msg(11) ) + ' '
053500131216     c                                     + %editc( G08dat : 'Y' )
053600131216     c                   leavesr
053700131216     c                   endif
053800131216      *
053900131216     c                   eval      oTA30dtd = H1Ddtd
054000131216      *
054100131216     c                   ENDSR
054200060807      *
054300060807      *---------------------------------------------------------------*
054400060807      * S1_Opz.5 = Visualizzazione singolo prezzo medio di gasolio    *
054500060807      *---------------------------------------------------------------*
054600060807     c     sr_OpzS1_5    BEGSR
054700060807      *
054800060807      * Impostazione dati in D01 (solo visualizzato - protetto)
054900060807     c                   clear                   TA30D01
055000060807     c                   eval      V1Cdtd  = S1Cdtd
055100060807     c*** NO !!!         exsr      CtrD01
055200060807     c                   eval      $F10    = *off
055300060807      *
055400060807      * Passaggio alla videata D02 per visualizzazione singolo
055500060807      *   prezzo medio di gasolio
055600060807     c                   eval      $Video  = '3'
055700060807     c                   eval      $InzD02 = *on
055800060807      *
055900060807do  1c                   doW       $Video  = '3'
056000060807     c                   exsr      GesD02
056100060807e   1c                   enddo
056200060807      *
056300060807     c                   ENDSR
056400060804      *
056500060804      *---------------------------------------------------------------*
056600060804      * Gestione videata D01                                          *
056700060804      *---------------------------------------------------------------*
056800060804     c     GesD01        BEGSR
056900060804      *
057000060804      * Inizializzazione videata
057100060804if  1c                   if        $InzD01 = *on
057200060804     c                   exsr      InzD01
057300060804     c                   movel     *off          $InzD01
057400060804e   1c                   endif
057500060804      *
057600060804      * Emissione Testata e Piede con tasti funzionali abilitati
057700060807     c                   eval      *in03   = *off
057800060807     c                   eval      *in06   = *off
057900060807     c                   write     TA30T01
058000060807     c                   write     TA30P02
058100060804      * Emissione videata
058200060807     c                   exfmt     TA30D01
058300060804     c                   setoff                                       28  90
058400060804     c                   clear                   V1Dmsg
058500060804      *
058600060804sel 1c                   SELECT
058700060807      * F12=Ritorno
058800060807w   1c                   WHEN      *inKL
058900060807     c                   exsr      F12D01
059000060804      *
059100060804x   1c                   OTHER
059200060804     c                   exsr      CtrD01
059300060804      *
059400060804if  2c                   if        *in90
059500060804     c                   leavesr
059600060804e   2c                   endif
059700060804      *
059800060804     c                   eval      $InzD02 = *on
059900060807     c                   eval      $Video  = '3'
060000060804      *
060100060804e   1c                   ENDSL
060200060804      *
060300060804     c                   ENDSR
060400060807      *
060500060807      *---------------------------------------------------------------*
060600060807      * Inizializzazione videata D01                                  *
060700060807      *---------------------------------------------------------------*
060800060807     c     InzD01        BEGSR
060900060807      *
061000060807     c                   clear                   TA30D01
061100060807      *
061200060807     c                   ENDSR
061300060807      *
061400060807      *---------------------------------------------------------------*
061500060807      * Gestione tasto funzionale F12 da videata D01                  *
061600060807      *---------------------------------------------------------------*
061700060807     c     F12D01        BEGSR
061800060807      *
061900060807      * Torno alla videata precedente
062000060807     c                   eval      $Video  = '1'
062100060807      *
062200060807     c                   ENDSR
062300060804      *
062400060804      *---------------------------------------------------------------*
062500060804      * Controllo dati immessi in videata D01                         *
062600060804      *---------------------------------------------------------------*
062700060804     c     CtrD01        BEGSR
062800060804      *
062900060804     c                   movea     *zeros        *in(51)
063000060804      *
063100060804      * Controllo formale data
063200060804     c                   clear                   WLBdat
063300060804if  1c                   if        V1Cdtd  > *zeros
063400060804     c                   eval      G08dat  = V1Cdtd
063500060804     c                   call      'XSRDA8'
063600060804     c                   parm                    WLBdat
063700060804x   1c                   else
063800060804     c                   eval      G08err  = *on
063900060804e   1c                   endif
064000060804if  1c                   if        G08err  = *on
064100060804     c                   seton                                        285190
064200070110     c                   eval      V1Dmsg  = $Msg(2)
064300060804     c                   leavesr
064400060804e   1c                   endif
064500060804     c                   eval      V1Cdtd  = G08dat
064600060804     c                   eval      W1Cdtd  = G08inv
064700060804      *
064800060804      * Controllo validità data
064900061221      * (DEVE essere più recente dell'ultima già inserita in TIPMG00F)
065000061221     c     W1Cdtd        setgt     TIPMG000
065100061221     c                   readp     TIPMG000
065200061221if  1c                   if        NOT  %eof(TIPMG01L)
065300061221     c                             and  PMGdtd >= W1Cdtd
065400060808     c                   reset                   WLBdat
065500060808     c                   eval      G08inv  = PMGdtd
065600060808     c                   call      'XSRDA8'
065700060808     c                   parm                    WLBdat
065800060804     c                   seton                                        285190
065900070110     c                   eval      V1Dmsg  = %trim($Msg(3))
066000060808     c                                     + ' '
066100060808     c                                     + %editc(G08dat:'Y')
066200060804     c                   leavesr
066300060808x   1c***                else
066400060808     c*** già così:      eval      $F10    = *on
066500060807e   1c                   endif
066600061221      * (DEVE essere di una settimana successiva a quella nell'ultimo
066700061221      *  record già inserito in TIPMG00F)
066800070110     c***  W1Cdtd        setgt     TIPMG000
066900070110     c     *hival        setgt     TIPMG000
067000061221     c                   read      TIPMG000
067100061221if  1c                   if        NOT  %eof(TIPMG01L)
067200061221     c     *iso          move      PMGdtd        wData_1
067300061221     c     *iso          move      W1Cdtd        wData_2
067400061221if  2c                   if        %diff( wData_2 : wData_1 : *days )
067500061221     c                                    <> 7
067600061221     c                   reset                   WLBdat
067700061221     c                   eval      G08inv  = PMGdtd
067800061221     c                   call      'XSRDA8'
067900061221     c                   parm                    WLBdat
068000061221     c                   seton                                        285190
068100070110     c                   eval      V1Dmsg  = %trim($Msg(4))
068200061221     c                                     + ' '
068300061221     c                                     + %editc(G08dat:'Y')
068400061221     c                   leavesr
068500061221e   2c                   endif
068600061221e   1c                   endif
068700070110      * (NON può essere futura)
068800070110if  1c                   if        W1Cdtd  > Wdate
068900070110     c                   reset                   WLBdat
069000070110     c                   seton                                        285190
069100070110     c                   eval      V1Dmsg  = $Msg(5)
069200070110     c                   leavesr
069300070110e   1c                   endif
069400061221      *
069500061221      * La data decorrenza DEVE essere un lunedì!
069600061221     c     *iso          movel     W1Cdtd        wDataDMY
069700061221     c     *dmy          movel     wDataDMY      w0060
069800061221     c                   movel     w0060         wGgMmAa
069900061221     c                   call      'XGIOSE1'
070000061221     c                   parm                    wGgMmAa           6
070100061221     c                   parm      *blank        wGioSet           1
070200061221     c                   if        wGioSet <> '1'
070300061221     c                   seton                                        285190
070400061221     c                   eval      V1Dmsg  = $Msg(6)
070500061221     c                   leavesr
070600061221     c                   endif
070700060804      *
070800060804     c                   ENDSR
070900060804      *
071000060804      *---------------------------------------------------------------*
071100060804      * Gestione videata D02                                          *
071200060804      *---------------------------------------------------------------*
071300060804     c     GesD02        BEGSR
071400060804      *
071500060804      * Inizializzazione videata
071600060804if  1c                   if        $InzD02 = *on
071700060804     c                   exsr      InzD02
071800060804     c                   movel     *off          $InzD02
071900060804e   1c                   endif
072000060804      *
072100060804      * Emissione Testata e Piede con tasti funzionali abilitati
072200060808      *                   e videata precedente (D01)
072300060808     c                   if        NOT  *in28
072400060808     c                   write     TA30T01
072500060807     c                   write     TA30P02
072600060808     c                   write     TA30D01
072700060808     c                   endif
072800060804      * Emissione videata
072900060807if  1c                   if        $F10    = *off
073000060807     c                   write     TA30D02
073100060807     c                   exfmt     PROTECT
073200060807     c                   else
073300060807     c                   write     PROTECT
073400060807     c                   exfmt     TA30D02
073500060807     c                   endif
073600060804     c                   setoff                                       28  90
073700060804     c                   clear                   V1Dmsg
073800060804      *
073900060804sel 1c                   SELECT
074000060807      * F3=Fine visualizzazioni
074100060807w   1c                   WHEN      *inKC
074200060807     c                   exsr      F03D02
074300060804      * F12=Ritorno
074400060804w   1c                   WHEN      *inKL
074500060804     c                   exsr      F12D02
074600061221      * F13=Interrogazione tab. IPG
074700061221w   1c                   WHEN      *inKM
074800061221     c                   exsr      F13D02
074900060807      * Invio (in visualizzazione)
075000060807w   1c                   WHEN      *in06   = *off
075100060807     c***                leavesr
075200060807      * Invio / F6 (in immissione)
075300060804x   1c                   OTHER
075400060804     c                   exsr      CtrD02
075500060807if  2c                   if        *in90
075600060804     c                   leavesr
075700060804e   2c                   endif
075800060804      * F6=Conferma
075900060804if  2c                   if             *inKF   = *on
076000060804     c                             and  *in06   = *on
076100060804     c                   exsr      Wrt_TIPMG
076200060807if  3c                   if        NOT  *in90
076300070110     c                   eval      SAVpmg  = PMGpmg
076400070110     ***c                   eval      SAVsca  = PMGsca
076500060807     c                   eval      $InzS01 = *on
076600060804     c                   eval      $Video  = '1'
076700060807e   3c                   endif
076800060804e   2c                   endif
076900060804      *
077000060804e   1c                   ENDSL
077100060804      *
077200060804     c                   ENDSR
077300060807      *
077400060807      *---------------------------------------------------------------*
077500060807      * Inizializzazione videata D02                                  *
077600060807      *---------------------------------------------------------------*
077700060807     c     InzD02        BEGSR
077800060807      *
077900060807     c                   clear                   TA30D02
078000080506     c                   reset                   $First
078100060807      *
078200060807      * Impostazione indicatori:
078300060807      * *in03 = abilita F3 per ritorno a S01 senza proseguire con
078400060807      *         l'elaborazione delle opzioni successive
078500060808      *         In fase di immissione consente il ritorno al subfile
078600060808      *         senza passare dalla videata D01...
078700060808     c                   eval      *in03   = *on
078800060807      * *in06 = abilita F6 per inserimento record
078900060807     c                   eval      *in06   = ($F10  = *on)
079000060807      *
079100060807      * Se è stata richiesta l'opzione 5=Visualizzazione
079200060807      *   si reperiscono i dati da visualizzare dal sfl:
079300060807if  1c                   IF             S1Copz = '5'
079400060807     c                             and  $F10   = *off
079500060807      *
079600060807      * Visualizzazione dati
079700060807     c***                eval      V1Cdtd  = S1Cdtd
079800060807     c                   eval      V1Cpmg  = S1Cpmg
079900060807     c                   eval      V1Csca  = S1Csca
080000060807     c                   eval      V1Cute  = S1Cute
080100060807     c                   eval      V1Cdti  = S1Cdti
080200060807      * - Decodifica dati
080300060807      *   (lo scaglione viene memorizzato nel record all'inserimento;
080400060807      *    meglio non eseguire il controllo, perchè potrebbero essere
080500060807      *    stati modificati gli scaglioni DOPO l'inserimento di questo
080600060807      *    record...)
080700060807     c                   eval      xx      = 1
080800131216     c                   eval      *in25   = *off
080900131216     c     V1Csca        lookup    $IPG(xx)                               25
081000131216if  2c                   if        *in25
081100060807     c                   eval      V1Dsca  = 'Valido da '
081200060807     c                                     + %trim(
081300060807     c                                         %editc( $IPGmin(xx) :
081400060807     c                                         '1') )
081500060807     c                                     + ' a '
081600060807     c                                     + %trim(
081700060807     c                                         %editc( $IPGmax(xx) :
081800060807     c                                         '1') )
081900060808     c                                     + ' Euro/Litro'
082000060807e   2c                   endif
082100060807      *
082200060807e   1c                   ENDIF
082300060807      *
082400060807     c                   ENDSR
082500060807      *
082600060807      *---------------------------------------------------------------*
082700060807      * Gestione tasto funzionale F03 da videata D02                  *
082800060807      *---------------------------------------------------------------*
082900060807     c     F03D02        BEGSR
083000060807      *
083100060808      * Ritorno alla 1ª videata (subfile)
083200060807     c                   eval      $Video  = '1'
083300060807     c                   eval      *in90   = *on
083400060807      *
083500060807     c                   ENDSR
083600060804      *
083700060804      *---------------------------------------------------------------*
083800060804      * Gestione tasto funzionale F12 da videata D02                  *
083900060804      *---------------------------------------------------------------*
084000060804     c     F12D02        BEGSR
084100060804      *
084200060804      * Ritorno alla videata precedente
084300060807     c                   if        *in06   = *on
084400060807     c                   eval      $Video  = '2'
084500060807     c                   else
084600060807     c                   eval      $Video  = '1'
084700060807     c                   endif
084800060804      *
084900060804     c                   ENDSR
085000061221      *
085100061221      *---------------------------------------------------------------*
085200061221      * Gestione tasto funzionale F13 da videata D02                  *
085300061221      *---------------------------------------------------------------*
085400061221     c     F13D02        BEGSR
085500061221      *
085600061221      * Richiamo pgm. TNTB58R per interrogazione tab. "IPG"
085700061221      * (scaglioni di incremento del prezzo del gasolio)
085800061221     c                   movel(p)  '05'          KPJBU
085900061221     c                   call      'TNTB58R'
086000061221     c                   parm                    KPJBA
086100061221      *
086200061221     c                   ENDSR
086300060804      *
086400060804      *---------------------------------------------------------------*
086500060804      * Controllo dati immessi in videata D02                         *
086600060804      *---------------------------------------------------------------*
086700060804     c     CtrD02        BEGSR
086800060804      *
086900060804     c                   movea     *zeros        *in(51)
087000080506     c                   clear                   V1Dtxt
087100060804      *
087200060804      * Controllo prezzo medio
087300060804      * reperendo lo scaglione di incremento relativo al prezzo
087400060808      * (considerando che NON ci DOVREBBERO essere periodi "in comune"
087500060808      *  in tab. "IPG", dovrebbe essere sufficiente la LOOKUP)
087600061221      * - Prezzo medio obbligatorio
087700061221if  1c                   if        V1Cpmg <= *zeros
087800061221     c                   seton                                        285190
087900070110     c                   eval      V1Dmsg  = $Msg(7)
088000061221     c                   leavesr
088100061221e   1c                   endif
088200060807     c                   clear                   V1Dsca
088300070105     c                   eval      xx      = 1
088400131216     c                   eval      *in25   = *off
088500131216     c     V1Cpmg        lookup    $IPGmax(xx)                        25  25
088600060808sel 1c                   SELECT
088700060808      * - Scaglione non reperito (x importo né maggiore né uguale)
088800131216w   1c                   WHEN      NOT *in25
088900060808     c                   seton                                        285190
089000070110     c                   eval      V1Dmsg  = $Msg(8)
089100060808     c                   leavesr
089200060808      * - Scaglione reperito
089300060808      *   (importo V1CPMG compreso nei range in tab. "IPG")
089400070105w   1c                   WHEN      V1Cpmg >= $IPGmin(xx)
089500070105     c                   eval      V1Csca  = $IPG(xx)
089600070105     c                   eval      V1Dsca  = 'Valido da '
089700070105     c                                     + %trim(
089800070105     c                                         %editc( $IPGmin(xx) :
089900070105     c                                         '1') )
090000070105     c                                     + ' a '
090100070105     c                                     + %trim(
090200070105     c                                         %editc( $IPGmax(xx) :
090300070105     c                                         '1') )
090400070105     c                                     + ' Euro/Litro'
090500070110     *** * - - Aumento max consentito di 2 (due) scaglioni
090600070110if  2***c                   if             V1Csca > (SAVsca + 2)
090700070110     ***c                             or   V1Csca < (SAVsca - 2)
090800070110      * - - Consentita una differenza max del ±2% rispetto all'ultimo
090900070110      *     prezzo medio inserito
091000090107      *     => Portato delta massimo al ±4%.
091100070110if  2c                   if             SAVpmg > *zeros
091200070110     c                             and (V1Cpmg > ( SAVpmg +
091300090107     c                                           ((SAVpmg * 4) / 100))
091400070110     c                              or  V1Cpmg < ( SAVpmg -
091500090107     c                                           ((SAVpmg * 4) / 100)))
091600080506if  3c                   if        %subst(knmus : 1 : 3) <> 'EDP'
091700070105     c                   seton                                        285590
091800070105     c                   eval      V1Dmsg  = $Msg(9)
091900070105     c                   leavesr
092000080506x   3c                   else
092100080506     c                   eval      V1Dtxt  = 'N.B.: '
092200080506     c                                     + %subst($Msg(9) : 1 : 58)
092300080506     c                   if        *inKF   and   $First = *on
092400080506     c                   eval      *in90   = *on
092500080506     c                   eval      $First  = *off
092600080506     c                   endif
092700080506e   3c                   endif
092800070105e   2c                   endif
092900060808      * - Scaglione reperito, ma con limite massimo inferiore
093000060808      *   all'importo in V1CPMG -DOVREBBE?essere impossibile!!!
093100060808x   1c                   OTHER
093200060808     c                   seton                                        285190
093300070110     c                   eval      V1Dmsg  = $Msg(8)
093400060808     c                   leavesr
093500060808e   1c                   ENDSL
093600060804      *
093700060804     c                   ENDSR
093800060804      *
093900060804      *---------------------------------------------------------------*
094000060804      * Scrittura record in file TIPMG00F                             *
094100060804      *---------------------------------------------------------------*
094200060804     c     Wrt_TIPMG     BEGSR
094300060804      *
094400060804     c                   clear                   TIPMG000
094500060804      *
094600060804     c                   eval      PMGdtd = W1Cdtd
094700060804     c                   eval      PMGpmg = V1Cpmg
094800060804     c                   eval      PMGsca = V1Csca
094900060804     c                   eval      PMGute = KNMUS
095000060804     c                   eval      PMGdti = wDate
095100060804      *                  __________________
095200060807     c                   WRITE(e)  TIPMG000
095300060804      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
095400060807     c                   if        %error
095500060807     c                   eval      $Video = '2'
095600060807     c                   seton                                        285190
095700070110     c                   eval      V1Dmsg = $Msg(10)
095800060807     c*** superfluo:     leavesr
095900060807     c                   endif
096000060807      *
096100060804     c                   ENDSR
096200060804
096300060804** - $MSG -------------------------------------------------------------------*
096400070110Opzione errata                                                                 1
096500070110Data formalmente errata                                                        2
096600070110Data decorrenza NON succesiva a quella dell'ultimo record inserito:            3
096700070110Questa data DEVE essere di 7 gg. successiva all'ultima inserita:               4
096800070110Non ammesse decorrenze "future"                                                5
096900070110La data decorrenza DEVE essere un lunedì                                       6
097000070110Prezzo medio obbligatorio                                                      7
097100070110Scaglione di incremento non reperito per questo prezzo medio: AVVISARE IL CED! 8
097200090107Prezzo medio differente all'ultimo inserito di OLTRE il 4%: AVVISARE IL CED!   9
097300070110Rilevati errori in fase di scrittura record nel file TIPMG00F                 10
097400131216Già selezionato il prezzo medio al giorno                                     11
