000100060804      *===============================================================*
000200110728      *?TRTC70R * Manutenzione File partner euroexpress              ?*
000300060804      *===============================================================*
000400131011
000500131011     /*PRM  dbgview(*source)
000600131011     /*END
000700131011
000800131011      *---------------------------------------------------------------*
000900060804
001000060804     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001100060804
001200060804      *---------------------------------------------------------------*
001300060804
001400110719     fAZORG01L  if   e           k disk
001500060807      *
001600110719     fTIPEX01L  uf A e           k disk
001700060807      *
001800110728     fTRTC70D   cf   e             workstn
001900080226     f                                     infds(InfDspF)
002000110728     f                                     sfile(TC70S01:S01nrr)
002100060804
002200060804      *---------------------------------------------------------------*
002300060804
002400060807      *
002500060807      * Costanti - - - - - - - - - - - - - - - - - - - - - - - - - - -*
002600060807      *
002700060807      * - Tasti di funzione
002800060807     d RollUp          c                   const(x'F5')
002900060807      * - Nr. di righe del sfl (SFLPAG)
003000060807     d C_SflPag        c                   const(14)
003100060804      *
003200060804      * Schiere  - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003300060804      *
003400110728     d $Msg            s             78    dim(13)  ctdata  perrcd(1)
003500060804      *
003600060804      * Ds - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*
003700060804      *
003800060804     d KPJBA         e ds
003900060804      *
004000060804      * - Parametri x Controllo profilo utenti
004100060804     d TIBS34ds      e ds
004200060804      * - Ds di riferimento al file esterno AZUTE00F
004300060804     d AZUTEds       e ds                  extname(AZUTE00F)
004400060804      * - Ds per dati organigramma
004500060804     d DDatiUte      e ds
004600110720       // - Reperimento dati anagrafici
004700110720     d TIBS69ds      e ds
004800110720     d DS_cnaco      e ds                  inz extname(CNACO00F)
004900110720     d DS_cnind      e ds                  inz extname(CNIND00F)
005000110720     d DS_cnclp      e ds                  inz extname(CNCLP00F)
005100110720     d DS_fncls      e ds                  inz extname(FNCLS00F)
005200060804      *
005300060804     d Status         sds           333
005400060804     d   SDSpgm          *proc
005500060807      *
005600080226     d InfDspF         ds           512
005700060807     d  £Tasto               369    369
005800080226     d  £SFLnrr              378    379b 0
005900060804      *
006000060804      * Parametri per controllo data e transform in giorni - XSRDA8 -
006100060804     d WLBdat          ds                  inz
006200060804     d  G08dat                        8  0 inz
006300060804     d  G08inv                        8  0 inz
006400060804     d  G08err                        1    inz('3')
006500060804     d  G08tgi                        5  0 inz
006600060804      *
006700060804      * Variabili  - - - - - - - - - - - - - - - - - - - - - - - - - -*
006800060804      *
006900060804      * - Flags
007000131011     d $Fine           s               n   inz
007100131011     d $F10            s               n   inz
007200131011     d $InzS01         s               n   inz(*on)
007300131011     d $InzD01         s               n   inz(*on)
007400131011     d $InzD02         s               n   inz(*on)
007500131011     d $OkParz         s               n   inz(*on)
007600060807      *
007700060804     d $Video          s              1    inz('1')
007800060807      *
007900060807      * Campi associati al SFL
008000060807     d S01nrr          s                   like(C01rcd) inz
008100060807     d SavS01csr       s                   like(C01rcd) inz
008200060807     d W_SflPag        s              3  0 inz
008300060807     d wPag            s              4  0 inz
008400060807     d wRig            s              3  0 inz
008500060804      *
008600060804      * - Altri campi
008700060804     d wDate           s              8  0 inz
008800110720     d Kfil            s                   inz  like(PEXfil)
008900110720     d Kdds            s                   inz  like(PEXdds)
009000110721     d W1Cdds          s                   inz  like(PEXdds)
009100110721     d W1Cdss          s                   inz  like(PEXdss)
009200110721     d W1Cddd          s                   inz  like(PEXddd)
009300110721     d W1Cdsd          s                   inz  like(PEXdsd)
009400131011     d W1Cdta          s                   inz  like(PEXdds)
009500110721      * - Parametri per TNSD24R
009600110721     d §§Cod           s              3    inz
009700110721     d §§Tip           s                   like(ORGfag)  inz
009800110721     d §§Des           s             25    inz
009900110721     d §§Pra           s              3    inz
010000060804
010100060804      *---------------------------------------------------------------*
010200060804      * I N D I C A T O R I                                           *
010300060804      *---------------------------------------------------------------*
010400060807      *  06     - Abilitato F6=Conferma in D02
010500060804      *  10     - Comodo                                              *
010600060804      *  28     - Emissione messaggio di errore a video               *
010700060807      *  30     - Pulizia del subfile                                 *
010800060807      *  31     - NON emissione del subfile                           *
010900060807      *  32     - Record modificato nel subfile (SflNxtChg)           *
011000060807      *  33     - Fine dati nel subfile         (SflEnd)              *
011100060807      *  50     - Opzione errata                                      *
011200060804      *  51     - Data decorrenza errata                              *
011300060804      *  52     - Non reperito scaglione per il prezzo medio indicato *
011400060804      *  90     - Generico di errore                                  *
011500060804      *---------------------------------------------------------------*
011600060804
011700060804     c     *Entry        plist
011800060804     c                   parm                    KPJBA
011900060804      *
012000060804      * Operazioni Iniziali
012100060804     c                   exsr      RoutInz
012200060804      *
012300060804      * Gestione Video
012400060804do  1c                   dow       $Fine   = *off
012500060807     c     $Video        caseq     '1'           GesS01
012600060807     c     $Video        caseq     '2'           GesD01
012700060807     c     $Video        caseq     '3'           GesD02
012800060807     c                   endcs
012900060807e   1c                   enddo
013000060804      *
013100060804      * Fine
013200060804     c                   movel     *on           *inLR
013300060804      *
013400060804      *---------------------------------------------------------------*
013500060804      * Operazioni Iniziali                                           *
013600060804      *---------------------------------------------------------------*
013700110721     c     RoutInz       Begsr
013800060804      *
013900131011      * Reperimento dati job
014000060804     c                   exsr      DatiJob
014100060804      *
014200131011      * Reperimento data odierna
014300060804     c                   movel     SDSpgm        V1Tpgm
014400060807     c                   clear                   WLBdat
014500060807     c                   eval      G08dat  = *date
014600060807     c                   call      'XSRDA8'
014700060807     c                   parm                    WLBdat
014800060807     c                   movel     G08inv        wDate
014900131011      *
015000131011     c                   eval      C1Cdta  = *date
015100131011     c                   eval      W1Cdta  = wDate
015200110720      *
015300131011      * Definizione Key-List
015400131011      * - File FIPEX01L
015500131011     c     KPEX          klist
015600131011     c                   kfld                    Kfil
015700131011     c                   kfld                    Kdds
015800060804      *
015900060804     c                   ENDSR
016000060804      *
016100060804      *---------------------------------------------------------------*
016200060804      * Reperimento Dati del job (Utente/Operativi)                   *
016300060804      *---------------------------------------------------------------*
016400060804     c     DatiJob       BEGSR
016500060804      *
016600060804     c     *dtaara       define    §azute        azuteds
016700060804     c     *dtaara       define    §datiute      ddatiute
016800060804      *
016900060804     c                   in(E)     *dtaara
017000060804     c                   IF        %ERROR or RSUT = *blanks
017100060804     c                   clear                   Tibs34Ds
017200060804     c                   call      'TIBS34R'
017300060804     c                   parm                    Tibs34Ds
017400060804     c                   in        *dtaara
017500060804     c                   ENDIF
017600060804      *
017700060804     c                   ENDSR
017800060807      *
017900060807      *---------------------------------------------------------------*
018000060807      * Gestione videata C01/S01                                      *
018100060807      *---------------------------------------------------------------*
018200060807     c     GesS01        BEGSR
018300060807      *
018400060807      * Inizializzo la videata
018500060807if  1c                   if        $InzS01 = *on
018600060807     c                   exsr      InzS01
018700060807     c                   eval      $InzS01     = *off
018800060807e   1c                   endif
018900060807      *
019000060807      * Emissione Testata e Piede con tasti funzionali abilitati
019100110728     c                   write     TC70T01
019200110728     c                   write     TC70P01
019300131017      *
019400131017      * Posizionamento del cursore
019500131017      *?C1CsrRrn contiene il numero di riga del subfile su cui?
019600131017      *?era posizionato il cursore; impostandolo in C1RcdNbr?
019700131017      *?si visualizza la pagina che vedeva l'utente quando ha?
019800131017      *?premuto l'ultimo tasto.?
019900131017     c                   if        C01Csr > *zero
020000131017     c                   eval      C01Rcd = C01Csr
020100131017     c                   else
020200131017     c                   eval      C01Rcd = 1
020300131017     c                   write     TC70S00
020400131017     c                   endif
020500131017      *
020600060807      * Emissione videata
020700110728     c                   exfmt     TC70C01
020800060807     c                   setoff                                       28  90
020900060807     c                   clear                   V1Dmsg
021000060807      *
021100060807sel 1c                   SELECT
021200060807      * F3=Fine
021300060807w   1c                   WHEN      *inKC
021400060807     c                   exsr      F03S01
021500060807      * F10=Inserimento
021600060807w   1c                   WHEN      *inKJ
021700060807     c                   exsr      F10S01
021800060807      * Roll-UP
021900060807w   1c                   WHEN      £Tasto      = RollUp
022000060807     c                   exsr      RollUpS01
022100060807      *
022200060807x   1c                   OTHER
022300131011      * Controllo Subfile Control
022400131011     c                   exsr      CtrC01
022500131011if  2c                   if        *in90
022600131011     c                   leavesr
022700131011e   2c                   endif
022800060807      * Gestione Opzioni
022900060807     c                   exsr      OpzS01
023000060807if  2c                   if        *in90
023100060807     c                   leavesr
023200060807e   2c                   endif
023300060807      *
023400060807e   1c                   ENDSL
023500060807      *
023600060807     c                   ENDSR
023700060807      *
023800060807      *---------------------------------------------------------------*
023900060807      * Inizializzazione videata C01/S01                              *
024000060807      *---------------------------------------------------------------*
024100060807     c     InzS01        BEGSR
024200060807      *
024300060807      * Pulizia subfile
024400060807     c                   seton                                        3031
024500110728     c                   write     TC70C01
024600060807     c                   setoff                                         3133
024700060807      *
024800131017     c                   clear                   W_SflPag
024900060807     c                   clear                   C01rcd
025000060807     c                   clear                   C01csr
025100060807     c                   clear                   S01nrr
025200060807     c                   clear                   V1Dmsg
025300060807     c                   setoff                                       28  90
025400060807     c                   movea     *zeros        *in(51)
025500060807      *
025600110719      * Caricamento dati dal file TIPEX00F
025700110719     c     *LOVAL        setll     TIPEX000
025800110719     c                   read      TIPEX000
025900060807      *
026000060807     c                   exsr      RollUpS01
026100131017      *
026200131017      * -?Impaginazione subfile?
026300131017      *  ?-> forza l'impaginazione sul 1° rec. del subfile?
026400131017     c                   if        S01nrr > *zero
026500131017     c                   eval      C01Rcd = 1
026600131017     c                   eval      C01Csr = 1
026700131017     c                   else
026800131017     c                   clear                   C01Rcd
026900131017     c                   endif
027000131017      *
027100060807     c                   ENDSR
027200060807      *
027300060807      *---------------------------------------------------------------*
027400060807      * Caricamento pagina successiva di dati                         *
027500060807      *---------------------------------------------------------------*
027600060807     c     RollUpS01     BEGSR
027700070105      *
027800060807     c                   eval      S01nrr  = (W_SflPag * C_SflPag)
027900060807     c                   add       1             W_SflPag
028000060807     c                   eval      *in32   = *off
028100060807      *
028200060807      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
028300110719do  1c                   DOW       NOT  %eof(TIPEX01L)
028400060807     c                             and  S01nrr < (W_SflPag * C_SflPag)
028500060807      *
028600131011     c                   exsr      sr_Parzializz
028700131011if  2c                   if        $OkParz = *on
028800060807     c                   exsr      RieS01
028900060807     c                   add       1             S01nrr
029000110728     c                   write     TC70S01
029100131011e   2c                   endif
029200060807      *
029300110719     c                   read      TIPEX000
029400060807      *
029500060807e   1c                   ENDDO
029600080226      *
029700080226      * Visualizzazione del SFL (se ci sono dati)
029800131017     c                   eval      *in30   = (S01nrr <= *zeros)
029900080226      *
030000080226      * Attivazione (eventuale) del SFLEND
030100110719     c                   eval      *in33   = %eof(TIPEX01L)
030200080226      *
030300080226      * Posizionamento cursore al 1° rcd della pagina
030400080226if  1c                   if        S01nrr  > *zeros
030500080226     c     S01nrr        div       C_SflPag      wPag
030600080226     c                   mvr                     wRig
030700080226     c                   eval      C01rcd  = wPag * C_SflPag
030800080226     c                   if        wRig    > *zeros
030900080226     c                   eval      C01rcd  = C01rcd + 1
031000080226     c                   else
031100080226     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
031200080226     c                   endif
031300080226x   1c                   else
031400080226     c                   clear                   C01rcd
031500080226e   1c                   endif
031600080226     c                   eval      C01csr  = C01rcd
031700060807      *
031800060807     c                   ENDSR
031900131011      *
032000131011      *---------------------------------------------------------------*
032100131011      * Selezione dei dati in lettura in base alle parzializzazioni   *
032200131011      *   inserite nel Subfile-Control                                *
032300131011      *---------------------------------------------------------------*
032400131011     c     sr_Parzializz BEGSR
032500131011      *
032600131011     c                   eval      $OkParz = *off
032700131011      *
032800131011     c                   select
032900131011      *
033000131011      * - Data "Partner attivi AL" NON inserita nel SflCtl
033100131011     c                   when      W1Cdta  = *zero
033200131011     c                   eval      $OkParz = *on
033300131011     c                   leavesr
033400131011      *
033500131011      * - Partner attivo quel giorno (su quella linea)
033600131011     c                   when      W1Cdta >= PEXdds  and
033700131011     c                             W1Cdta <= PEXdss
033800131011     c                   eval      $OkParz = *on
033900131011     c                   leavesr
034000131011      *
034100131011     c                   endsl
034200131011      *
034300131011     c                   ENDSR
034400060807      *
034500060807      *---------------------------------------------------------------*
034600060807      * Impostazione dati in singolo record del sfl S01               *
034700060807      *---------------------------------------------------------------*
034800060807     c     RieS01        BEGSR
034900060807      *
035000110728     c                   clear                   TC70S01
035100060807      *
035200060807      * Campi hidden
035300110719     c                   eval      H1Cfil  = PEXfil
035400110719     c                   eval      H1Ccod  = PEXcod
035500110719     c                   eval      H1Cdds  = PEXdds
035600110719      *
035700110719      * Linea spedizioni
035800110719     c                   eval      S1Cfil  = PEXfil
035900131017     c     PEXfil        chain     AZORG
036000110719     c                   If        %found(Azorg01l)
036100110719     c                   movel     ORGdes        S1Dfil
036200110719     c                   else
036300110719     c                   eval      S1Dfil = *all'?'
036400110719     c                   endif
036500110720      * Codice Partner
036600110720     c                   eval      S1Ccod  = PEXcod
036700110720     c                   clear                   TIBS69DS
036800110720     c                   z-add     PEXcod        I69kac
036900110720      *
037000110720     c                   CALL      'TIBS69R'
037100110720     c                   PARM                    tibs69DS
037200110720     c                   PARM                    DS_cnaco
037300110720     c                   PARM                    DS_cnind
037400110720     c                   PARM                    DS_cnclp
037500110720     c                   PARM                    DS_fncls
037600110720     c                   If        o69err = ' '
037700110720     c                   movel     ACOrag        S1Dcod
037800110720     c                   else
037900110720     c                   eval      S1Dcod = *all'?'
038000110720     c                   endif
038100060807      *
038200110720      * Data decorrenza spedizioni
038300060807     c                   reset                   WLBdat
038400110720     c                   eval      G08inv  = PEXdds
038500060807     c                   call      'XSRDA8'
038600060807     c                   parm                    WLBdat
038700110720     c                   eval      S1Cdds  = G08dat
038800110720      *
038900110720      * Data scadenza   spedizioni
039000110720     c                   reset                   WLBdat
039100110720     c                   eval      G08inv  = PEXdss
039200110720     c                   call      'XSRDA8'
039300110720     c                   parm                    WLBdat
039400110720     c                   eval      S1Cdss  = G08dat
039500110720      *
039600110720      * Data decorrenza spedizioni
039700110720     c                   reset                   WLBdat
039800110720     c                   eval      G08inv  = PEXddd
039900110720     c                   call      'XSRDA8'
040000110720     c                   parm                    WLBdat
040100110720     c                   eval      S1Cddd  = G08dat
040200110720      *
040300110720      * Data scadenza   spedizioni
040400110720     c                   reset                   WLBdat
040500110720     c                   eval      G08inv  = PEXdsd
040600110720     c                   call      'XSRDA8'
040700110720     c                   parm                    WLBdat
040800110720     c                   eval      S1Cdsd  = G08dat
040900060807      * Data inserimento
041000060807     c                   reset                   WLBdat
041100110720     c                   eval      G08inv  = PEXdti
041200060807     c                   call      'XSRDA8'
041300060807     c                   parm                    WLBdat
041400110720     c                   eval      H1Cdti  = G08dat
041500060807      *
041600060807     c                   ENDSR
041700060807      *
041800060807      *---------------------------------------------------------------*
041900060807      * Gestione tasto funzionale F03 da videata S01                  *
042000060807      *---------------------------------------------------------------*
042100060807     c     F03S01        BEGSR
042200060807      *
042300060807      * Chiusura del programma
042400060807     c                   eval      $Fine   = *on
042500060807      *
042600060807     c                   ENDSR
042700060807      *
042800060807      *---------------------------------------------------------------*
042900060807      * Gestione tasto funzionale F10 da videata S01                  *
043000060807      *---------------------------------------------------------------*
043100060807     c     F10S01        BEGSR
043200060807      *
043300060807      * Passaggio alla videata di immissione (D01)
043400060807     c                   eval      $Video  = '2'
043500060807     c                   eval      $inzD01 = *on
043600060807     c                   eval      $inzD02 = *on
043700060808      *
043800060808     c                   eval      $F10    = *on
043900060807      *
044000060807     c                   ENDSR
044100131011      *
044200131011      *---------------------------------------------------------------*
044300131011      * Controllo Subfile-Control                                     *
044400131011      *---------------------------------------------------------------*
044500131011     c     CtrC01        BEGSR
044600131011      *
044700131011      * Conrollo data "Partner attivi AL"
044800131011if  1c                   IF        C1Cdta  = *zero
044900131011     c                   if        W1Cdta <> *zero
045000131011     c                   eval      $InzS01 = *on
045100131011     c                   endif
045200131011     c                   clear                   W1Cdta
045300131011x   1c                   ELSE
045400131011     c                   clear                   WLBdat
045500131011     c                   eval      G08dat  = C1Cdta
045600131011     c                   call      'XSRDA8'
045700131011     c                   parm                    WLBdat
045800131011     c                   movel     G08inv        wDate
045900131011if  2c                   if        G08err  = *on
046000131011     c                   seton                                        286090
046100131011     c                   eval      V1Dmsg  = $Msg(2)
046200131011     c                   leavesr
046300131011e   2c                   endif
046400131011if  2c                   if        G08inv <> W1Cdta
046500131011     c                   eval      $InzS01 = *on
046600131011e   2c                   endif
046700131011     c                   eval      C1Cdta  = G08dat
046800131011     c                   eval      W1Cdta  = G08inv
046900131011e   1c                   ENDIF
047000131011      *
047100131011     c                   ENDSR
047200060807      *
047300060807      *---------------------------------------------------------------*
047400060807      * Gestione opzioni subfile                                      *
047500060807      *---------------------------------------------------------------*
047600060807     c     OpzS01        BEGSR
047700060807      *
047800060807     c                   clear                   SavS01csr
047900060807      *
048000110728     c                   readc     TC70S01
048100060807      *
048200110728do  1c                   DOW       NOT %eof(TRTC70D)
048300060807      *
048400060807     c                   eval      *in32   = *off
048500060807     c                   movea     *zeros        *in(51)
048600060807     c                   z-add     S01nrr        C01rcd
048700060807      *
048800060807sel 2c                   SELECT
048900060807      * - Nessuna opzione
049000060807w   2c                   WHEN      S1Copz  = *blanks
049100110720      * - 5 = Visualizzazione partner associato alla linea
049200060807w   2c                   WHEN      S1Copz  = '5'
049300110721     c                   exsr      sr_OpzS1
049400110720      * - 2 = Modifica        partner associato alla linea
049500110720w   2c                   WHEN      S1Copz  = '2'
049600110721     c                   exsr      sr_OpzS1
049700060807      * - ? = Opzione NON valida
049800060807x   2c                   OTHER
049900060807     c                   seton                                        285190
050000070110     c                   eval      V1Dmsg  = $Msg(1)
050100060807e   2c                   ENDSL
050200060807      *
050300060807      * Memorizzazione nrr del sfl con la 1ª opzione
050400060807      * per riposizionarvici il cursore dopo il tasto "Invio"
050500060807if  1c                   if             S1Copz   <> *blanks
050600060807     c                             and (SavS01csr = *zeros
050700060807     c                              or  SavS01csr < C01rcd)
050800060807     c                   eval      SavS01csr   = C01rcd
050900060807e   1c                   endif
051000060807      *
051100060807      * Aggiornamento sfl
051200060807sel 2c                   select
051300060807w   2c                   when      *in28
051400060807     c                   eval      *in32       = *on
051500060807     c                   z-add     C01rcd        C01csr
051600060807w   2c                   when      *in90       = *on
051700060807     c                   z-add     C01rcd        C01csr
051800060807     c                   clear                   S1Copz
051900060807x   2c                   other
052000060807     c                   clear                   S1Copz
052100060807e   2c                   endsl
052200110728     c                   UPDATE    TC70S01
052300060807if  2c                   if        *in28  OR  *in90
052400060807     c                   leave
052500060807e   2c                   endif
052600060807      *
052700110728     c                   readc     TC70S01
052800060807      *
052900060807e   1c                   ENDDO
053000060807      *
053100060807      * Riposiziono il cursore sul record contenente la prima opzione
053200060807      *  (se non sono stati rilevati errori)
053300060807if  1c                   if        NOT *in28  and  NOT *in90
053400060807     c                             and SavS01csr  > *zeros
053500060807     c                   z-add     SavS01csr     C01csr
053600060807e   1c                   endif
053700060807      *
053800060807     c                   ENDSR
053900060807      *
054000060807      *---------------------------------------------------------------*
054100110720      * S1_Opz.5 = Visualizzazione linea partner euroexpress          *
054200060807      *---------------------------------------------------------------*
054300110721     c     sr_OpzS1      BEGSR
054400060807      *
054500060807      * Impostazione dati in D01 (solo visualizzato - protetto)
054600110728     c                   clear                   TC70D01
054700110721     c                   eval      V1Cdti  = H1Cdti
054800060807     c                   eval      $F10    = *off
054900060807      *
055000060807      * Passaggio alla videata D02 per visualizzazione singolo
055100110720      *   record linea partner euroxpress
055200060807     c                   eval      $Video  = '3'
055300060807     c                   eval      $InzD02 = *on
055400060807      *
055500060807do  1c                   doW       $Video  = '3'
055600060807     c                   exsr      GesD02
055700060807e   1c                   enddo
055800060807      *
055900060807     c                   ENDSR
056000060804      *
056100060804      *---------------------------------------------------------------*
056200060804      * Gestione videata D01                                          *
056300060804      *---------------------------------------------------------------*
056400060804     c     GesD01        BEGSR
056500060804      *
056600060804      * Inizializzazione videata
056700060804if  1c                   if        $InzD01 = *on
056800060804     c                   exsr      InzD01
056900060804     c                   movel     *off          $InzD01
057000060804e   1c                   endif
057100060804      *
057200060804      * Emissione Testata e Piede con tasti funzionali abilitati
057300060807     c                   eval      *in03   = *off
057400060807     c                   eval      *in06   = *off
057500110728     c                   write     TC70T01
057600110728     c                   write     TC70P02
057700060804      * Emissione videata
057800110728     c                   exfmt     TC70D01
057900060804     c                   setoff                                       28  90
058000060804     c                   clear                   V1Dmsg
058100060804      *
058200060804sel 1c                   SELECT
058300060807      * F12=Ritorno
058400060807w   1c                   WHEN      *inKL
058500060807     c                   exsr      F12D01
058600060804      *
058700060804x   1c                   OTHER
058800060804     c                   exsr      CtrD01
058900060804      *
059000060804if  2c                   if        *in90
059100060804     c                   leavesr
059200060804e   2c                   endif
059300060804      *
059400060804     c                   eval      $InzD02 = *on
059500060807     c                   eval      $Video  = '3'
059600060804      *
059700060804e   1c                   ENDSL
059800060804      *
059900060804     c                   ENDSR
060000060807      *
060100060807      *---------------------------------------------------------------*
060200060807      * Inizializzazione videata D01                                  *
060300060807      *---------------------------------------------------------------*
060400060807     c     InzD01        BEGSR
060500060807      *
060600110728     c                   clear                   TC70D01
060700060807      *
060800060807     c                   ENDSR
060900060807      *
061000060807      *---------------------------------------------------------------*
061100060807      * Gestione tasto funzionale F12 da videata D01                  *
061200060807      *---------------------------------------------------------------*
061300060807     c     F12D01        BEGSR
061400060807      *
061500060807      * Torno alla videata precedente
061600060807     c                   eval      $Video  = '1'
061700060807      *
061800060807     c                   ENDSR
061900060804      *
062000060804      *---------------------------------------------------------------*
062100060804      * Controllo dati immessi in videata D01                         *
062200060804      *---------------------------------------------------------------*
062300060804     c     CtrD01        BEGSR
062400060804      *
062500060804     c                   movea     *zeros        *in(51)
062600110720      *
062700110720      * Controllo Linea
062800110721if  1c                   if        V1Cfil  = *zeros or V1Cfil = *blanks
062900110720     c                   seton                                        285290
063000110720     c                   eval      V1Dmsg  = $Msg(4)
063100110720     c                   leavesr
063200110720e   1c                   endif
063300110720      *
063400131011     c                   If        %scan('?' : V1Cfil) > *zeros
063500110720     c                   CALL      'TNSD24R'
063600110720     c                   PARM                    §§Cod
063700110720     c                   PARM                    §§Tip
063800110720     c                   PARM                    §§DES
063900110720     c                   PARM                    §§PRA
064000110720     c                   movel     §§COD         V1Cfil
064100110720     c                   movel     §§DES         V1Dfil
064200110720     c                   endif
064300131011     c                   IF        V1Cfil > *zeros
064400110721     c                   movel     V1Cfil        Kfil
064500131011     c     Kfil          chain     AZORG
064600131011     c                   If        Not %found(AZORG01L)
064700110720     c                   seton                                        285290
064800110720     c                   eval      V1Dmsg  = $Msg(4)
064900110720     c                   leavesr
065000110721     c                   else
065100131011     c                   movel     ORGdes        V1Dfil
065200110720     c                   endif
065300110721     c                   endif
065400110721      *
065500110721      * Controllo Codice partner
065600110721if  1c                   if        V1Ccod  = *zeros
065700110721     c                   seton                                        285390
065800110721     c                   eval      V1Dmsg  = $Msg(11)
065900110721     c                   leavesr
066000110721e   1c                   endif
066100110721      *
066200110721      * Verifico  Codice partner
066300110721     c                   clear                   TIBS69DS
066400110721     c                   z-add     V1Ccod        I69kac
066500110721      *
066600110721     c                   CALL      'TIBS69R'
066700110721     c                   PARM                    tibs69DS
066800110721     c                   PARM                    DS_cnaco
066900110721     c                   PARM                    DS_cnind
067000110721     c                   PARM                    DS_cnclp
067100110721     c                   PARM                    DS_fncls
067200110721if  1c                   If        o69err = ' '
067300110721     c                   movel     ACOrag        V1Dcod
067400110721     c                   else
067500110721     c                   seton                                        285390
067600110721     c                   eval      V1Dmsg  = $Msg(12)
067700110721     c                   leavesr
067800110721e   1c                   endif
067900060804      *
068000060804      * Controllo formale data
068100060804     c                   clear                   WLBdat
068200110720if  1c                   if        V1Cdds  > *zeros
068300110720     c                   eval      G08dat  = V1Cdds
068400060804     c                   call      'XSRDA8'
068500060804     c                   parm                    WLBdat
068600060804x   1c                   else
068700060804     c                   eval      G08err  = *on
068800060804e   1c                   endif
068900060804if  1c                   if        G08err  = *on
069000060804     c                   seton                                        285190
069100070110     c                   eval      V1Dmsg  = $Msg(2)
069200060804     c                   leavesr
069300060804e   1c                   endif
069400110720     c                   eval      V1Cdds  = G08dat
069500110720     c                   eval      W1Cdds  = G08inv
069600060804      *
069700110720      * verifico se esiste già il record con questa chiave
069800131011     c                   movel     V1Cfil        Kfil
069900131011     c                   move      W1Cdds        Kdds
070000110721     c     Kpex          chain(n)  TIPEX01L
070100110720     c                   If        %Found(Tipex01l)
070200110720     c                   seton                                        285290
070300110720     c                   eval      V1Dmsg  = $Msg(6)
070400110720     c                   leavesr
070500110720     c                   Endif
070600110720     c
070700110720      * Controllo validità data in presenza di un altro partner con la stessa linea
070800110720      * (DEVE essere scaduto il precedente)
070900110720     c     Kfil          setgt     TIPEX01l
071000110721     c     Kfil          readpe(n) TIPEX01l
071100110720if  1c                   if        NOT  %eof(TIPEX01L)
071200110720      * verifico la data decorrenza spedizioni se la decorrenza precedente è maggiore
071300110720      * di quella appena inserita errore
071400110728     c*                  If        Pexdds > w1cdds
071500110728     c*                  seton                                        285290
071600110728     c*                  eval      V1Dmsg  = $Msg(3)
071700110728     c*                  leavesr
071800110728     c*                  Endif
071900110720     c
072000110720      * verifico la data scadenza  spedizioni se la scadenza precedente è maggiore
072100110720      * della data decorrenza di quella appena inserita errore
072200110720     c                   If        Pexdss > w1cdds
072300110728     c                             and w1cdds > PEXdds
072400110720     c                   seton                                        285290
072500110720     c                   eval      V1Dmsg  = $Msg(5)
072600110720     c                   leavesr
072700110720     c                   Endif
072800131011      *
072900110720     c                   Endif
073000061221      *
073100060804     c                   ENDSR
073200060804      *
073300060804      *---------------------------------------------------------------*
073400060804      * Gestione videata D02                                          *
073500060804      *---------------------------------------------------------------*
073600060804     c     GesD02        BEGSR
073700060804      *
073800060804      * Inizializzazione videata
073900060804if  1c                   if        $InzD02 = *on
074000060804     c                   exsr      InzD02
074100060804     c                   movel     *off          $InzD02
074200060804e   1c                   endif
074300060804      *
074400060804      * Emissione Testata e Piede con tasti funzionali abilitati
074500060808      *                   e videata precedente (D01)
074600060808     c                   if        NOT  *in28
074700110728     c                   write     TC70T01
074800110728     c                   write     TC70P02
074900110728     c                   write     TC70D01
075000060808     c                   endif
075100060804      * Emissione videata
075200110720if  1c                   if        $F10    = *off and S1Copz = '5'
075300110728     c                   write     TC70D02
075400060807     c                   exfmt     PROTECT
075500060807     c                   else
075600060807     c                   write     PROTECT
075700110728     c                   exfmt     TC70D02
075800060807     c                   endif
075900060804     c                   setoff                                       28  90
076000060804     c                   clear                   V1Dmsg
076100060804      *
076200060804sel 1c                   SELECT
076300060807      * F3=Fine visualizzazioni
076400060807w   1c                   WHEN      *inKC
076500060807     c                   exsr      F03D02
076600060804      * F12=Ritorno
076700060804w   1c                   WHEN      *inKL
076800060804     c                   exsr      F12D02
076900060807      * Invio (in visualizzazione)
077000060807w   1c                   WHEN      *in06   = *off
077100060807      * Invio / F6 (in immissione)
077200060804x   1c                   OTHER
077300060804     c                   exsr      CtrD02
077400060807if  2c                   if        *in90
077500060804     c                   leavesr
077600060804e   2c                   endif
077700060804      * F6=Conferma
077800060804if  2c                   if             *inKF   = *on
077900060804     c                             and  *in06   = *on
078000110721     c                   exsr      Wrt_TIPEX_upd
078100060807if  3c                   if        NOT  *in90
078200060807     c                   eval      $InzS01 = *on
078300060804     c                   eval      $Video  = '1'
078400060807e   3c                   endif
078500060804e   2c                   endif
078600060804      *
078700060804e   1c                   ENDSL
078800060804      *
078900060804     c                   ENDSR
079000060807      *
079100060807      *---------------------------------------------------------------*
079200060807      * Inizializzazione videata D02                                  *
079300060807      *---------------------------------------------------------------*
079400060807     c     InzD02        BEGSR
079500060807      *
079600110728     c                   clear                   TC70D02
079700060807      *
079800060807      * Impostazione indicatori:
079900060807      * *in03 = abilita F3 per ritorno a S01 senza proseguire con
080000060807      *         l'elaborazione delle opzioni successive
080100060808      *         In fase di immissione consente il ritorno al subfile
080200060808      *         senza passare dalla videata D01...
080300060808     c                   eval      *in03   = *on
080400110721      * *in06 = abilita F6 per inserimento e la manutenzione del record
080500110721     c                   eval      *in06   = ($F10  = *on) or (S1Copz = '2')
080600060807      *
080700110721      * Se è stata richiesta l'opzione 5=Visualizzazione o 2=Modifica
080800060807      *   si reperiscono i dati da visualizzare dal sfl:
080900110721     c                   IF        $F10   = *off
081000060807      *
081100060807      * Visualizzazione dati
081200110721     c                   movel     S1Cfil        V1Cfil
081300110721     c                   movel     S1Dfil        V1Dfil
081400110721     c                   movel     S1Ccod        V1Ccod
081500110721     c                   movel     S1Dcod        V1Dcod
081600110721     c                   eval      V1Cdds  = S1Cdds
081700110721     c                   eval      V1Cdss  = S1Cdss
081800110720     c                   eval      V1Cddd  = S1Cddd
081900110720     c                   eval      V1Cdsd  = S1Cdsd
082000110720     c                   eval      V1Cdti  = H1Cdti
082100110728     c                   eval      W1Cdds  = H1Cdds
082200060807      *
082300060807e   1c                   ENDIF
082400060807      *
082500060807     c                   ENDSR
082600060807      *
082700060807      *---------------------------------------------------------------*
082800060807      * Gestione tasto funzionale F03 da videata D02                  *
082900060807      *---------------------------------------------------------------*
083000060807     c     F03D02        BEGSR
083100060807      *
083200060808      * Ritorno alla 1ª videata (subfile)
083300060807     c                   eval      $Video  = '1'
083400060807     c                   eval      *in90   = *on
083500060807      *
083600060807     c                   ENDSR
083700060804      *
083800060804      *---------------------------------------------------------------*
083900060804      * Gestione tasto funzionale F12 da videata D02                  *
084000060804      *---------------------------------------------------------------*
084100060804     c     F12D02        BEGSR
084200060804      *
084300060804      * Ritorno alla videata precedente
084400131011     c                   if        *in06   = *on and $F10 = *on
084500060807     c                   eval      $Video  = '2'
084600060807     c                   else
084700060807     c                   eval      $Video  = '1'
084800060807     c                   endif
084900060804      *
085000060804     c                   ENDSR
085100061221      *
085200060804      *---------------------------------------------------------------*
085300060804      * Controllo dati immessi in videata D02                         *
085400060804      *---------------------------------------------------------------*
085500060804     c     CtrD02        BEGSR
085600060804      *
085700060804     c                   movea     *zeros        *in(51)
085800060804      *
085900110720      * Controllo DATA scadenza spedizioni
086000110720if  1c                   if        V1Cdss <= *zeros
086100110720     c                   seton                                        285490
086200070110     c                   eval      V1Dmsg  = $Msg(7)
086300061221     c                   leavesr
086400061221e   1c                   endif
086500110720      * Controllo formale data
086600110720     c                   clear                   WLBdat
086700110720if  1c                   if        V1Cdss  > *zeros
086800110720     c                   eval      G08dat  = V1Cdss
086900110720     c                   call      'XSRDA8'
087000110720     c                   parm                    WLBdat
087100110720x   1c                   else
087200110720     c                   eval      G08err  = *on
087300110720e   1c                   endif
087400110720if  1c                   if        G08err  = *on
087500110720     c                   seton                                        285490
087600110720     c                   eval      V1Dmsg  = $Msg(2)
087700110720     c                   leavesr
087800110720e   1c                   endif
087900110720     c                   eval      V1Cdss  = G08dat
088000110720     c                   eval      W1Cdss  = G08inv
088100110720      *
088200110721      * Controllo se data scadenza spedizione maggiore o uguale a quella di decorrenza
088300110720if  1c                   If        W1Cdss < W1Cdds
088400110720     c                   seton                                        285490
088500110721     c                   eval      V1Dmsg  = $Msg(8)
088600110720     c                   leavesr
088700110720e   1c                   endif
088800110728      *
088900110728      * se esiste un record successivo con data decorrenza maggiore di quella che inserisco
089000110728      * l'attuale data scadenza deve essere minore della decorrenza successiva
089100131011     c                   movel     V1Cfil        Kfil
089200131011     c                   move      W1Cdds        Kdds
089300110728     c     Kpex          setgt     TIPEX01L
089400110728     c     Kfil          reade(n)  TIPEX01L
089500110728     c                   If        not %eof(Tipex01l)
089600110728      * verifico la data scadenza che sia minore alla data decorrenza
089700110728     c                   If        w1cdss > PEXdds
089800110728     c                   reset                   WLBdat
089900110728     c                   eval      G08inv  = PEXdds
090000110728     c                   call      'XSRDA8'
090100110728     c                   parm                    WLBdat
090200110728     c                   seton                                        285490
090300110728     c                   eval      V1Dmsg  = %trim($Msg(13))
090400110728     c                                     + ' '
090500110728     c                                     + %editc(G08dat:'Y')
090600110728     c                   leavesr
090700110728     c                   Endif
090800110728     c                   Endif
090900060804      *
091000110721      * Controllo se le date di validità Rivalsa
091100110721      * Controllo DATA decorrenza Rivalsa
091200110721if  1c                   if        (V1Cddd = *zeros and V1Cdsd > *zeros)
091300110721     c                              or (V1Cddd > *zeros and V1Cdsd = *zeros)
091400110721     c                   seton                                        285590
091500110721     c                   eval      V1Dmsg  = $Msg(10)
091600110721     c                   leavesr
091700110721e   1c                   endif
091800110721      * Controllo formale data
091900110721     c                   clear                   WLBdat
092000110721if  1c                   if        V1Cddd  > *zeros
092100110721     c                   eval      G08dat  = V1Cddd
092200110721     c                   call      'XSRDA8'
092300110721     c                   parm                    WLBdat
092400110721e   1c                   endif
092500110721if  1c                   if        G08err  = *on
092600110721     c                   seton                                        285590
092700110721     c                   eval      V1Dmsg  = $Msg(2)
092800110721     c                   leavesr
092900110721e   1c                   endif
093000110721     c                   eval      V1Cddd  = G08dat
093100110721     c                   eval      W1Cddd  = G08inv
093200131011      *
093300110721      * Controllo formale data
093400110721     c                   clear                   WLBdat
093500110721if  1c                   if        V1Cdsd  > *zeros
093600110721     c                   eval      G08dat  = V1Cdsd
093700110721     c                   call      'XSRDA8'
093800110721     c                   parm                    WLBdat
093900110721e   1c                   endif
094000110721if  1c                   if        G08err  = *on
094100110721     c                   seton                                        285690
094200110721     c                   eval      V1Dmsg  = $Msg(2)
094300110721     c                   leavesr
094400110721e   1c                   endif
094500110721     c                   eval      V1Cdsd  = G08dat
094600110721     c                   eval      W1Cdsd  = G08inv
094700131011      *
094800131011if  1c                   if        W1Cdsd < W1Cddd
094900110721     c                   seton                                        285590
095000110721     c                   eval      V1Dmsg  = $Msg(8)
095100110721     c                   leavesr
095200110721e   1c                   endif
095300131011      *
095400110727      * verifico se le date di validità rivalsa sono all'interno delle date spedizione se <> da zero
095500110727     c                   if        w1cddd > 0 and w1cdsd > 0 and
095600110727     c                             (w1cddd < w1cdds or
095700110727     c                             w1cdsd > w1cdss )
095800110721     c                   seton                                        285590
095900110721     c                   eval      V1Dmsg  = $Msg(9)
096000110721     c                   leavesr
096100110721     c                   endif
096200110721      *
096300060804     c                   ENDSR
096400060804      *
096500060804      *---------------------------------------------------------------*
096600110721      * Scrittura record in file TIPEX00F                             *
096700060804      *---------------------------------------------------------------*
096800110721     c     Wrt_TIPEX_upd BEGSR
096900060804      *
097000131011if  1c                   If        $F10 = *on
097100131011      *
097200110721     c                   clear                   TIPEX000
097300060804      *
097400110721     c                   eval      PEXfil = Kfil
097500110721     c                   eval      PEXcod = V1Ccod
097600110721     c                   eval      PEXdds = W1Cdds
097700110721     c                   eval      PEXdss = W1Cdss
097800110721     c                   eval      PEXddd = W1Cddd
097900110721     c                   eval      PEXdsd = W1Cdsd
098000110721     c                   eval      PEXdti = wDate
098100060804      *                  __________________
098200110721     c                   WRITE(e)  TIPEX000
098300060804      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
098400131011x   1c                   Else
098500131011      *
098600131011     c                   movel     S1Cfil        Kfil
098700131011     c                   movel     H1Cdds        Kdds
098800131011     c     Kpex          chain     TIPEX000
098900131011if  2c                   If        %Found(Tipex01l)
099000110721     c                   eval      PEXdss = W1Cdss
099100110721     c                   eval      PEXddd = W1Cddd
099200110721     c                   eval      PEXdsd = W1Cdsd
099300110721      *                  __________________
099400110721     c                   UPDATE    TIPEX000
099500110721      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
099600131011e   2c                   EndIf
099700131011      *
099800131011e   1c                   EndIf
099900131011      *
100000060804     c                   ENDSR
100100060804
100200060804** - $MSG -------------------------------------------------------------------*
100300070110Opzione errata                                                                 1
100400070110Data formalmente errata                                                        2
100500110720Data decorrenza NON succesiva a quella dell'ultimo record inserito             3
100600110720Obbligatorio inserire la linea di spedizione                                   4
100700110720Data scadenza dell'ultimo record inserito successiva alla data decorrenza      5
100800110721E' già presente questa linea spedizioni con la stessa data decorrenza          6
100900110720Data scadenza spedizioni obbligatoria. Impostare 31/12/2039                    7
101000110721Data scadenza deve essere maggiore della data decorrenza                       8
101100110721Data decorrenza e scadenza Rivalsa fuori dall'intervallo date spedizioni       9
101200110721Inserire entrambe le date relative alla Rivalsa                               10
101300110728Obbligatorio inserire il codice del Partner                                   11
101400110721Codice Partner inesistente                                                    12
101500110728La data scadenza spedizione non deve essere maggiore di                       13
