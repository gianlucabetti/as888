000100060925      *---------------------------------------------------------------*
000200060927      *?CAPPARIO DPD - GESTIONE VERSIONI                             ?*
000300060925      *---------------------------------------------------------------*
000400060925
000500060925     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060925
000700060925      *---------------------------------------------------------------*
000800060925
000900060925      * Versioni
001000170628     fDPCVE11I  if   e           k disk
001100170628     f                                     rename(DPCVE100:DPCVE101)
001200170628     fDPCVE12I  if   e           k disk
001300170628     f                                     rename(DPCVE100:DPCVE102)
001400170628     fTISID6D   cf   e             workstn
001500170628     f                                     sfile(SID6S01:S01nrr)
001600060926     f                                     infds(DSFMT)
001700060925
001800060925      *---------------------------------------------------------------*
001900060925
002000060925      *
002100060925      *  ?C O S T A N T I?  - - - - - - - - - - - - - - - - - - - - - *
002200060925      *
002300060925      * - Nr. di righe del sfl (SFLPAG)
002400060926     d C_SflPag        c                   const(14)
002500060926      * - Tasti funzionali
002600060926     d Enter           c                   const(x'F1')
002700060926     d RollDwn         c                   const(x'F4')
002800060926     d RollUp          c                   const(x'F5')
002900060925      *
003000060925      *  ?S C H I E R E?  - - - - - - - - - - - - - - - - - - - - - - *
003100060925      *
003200060925      * - Messaggi di errore
003300061109     d $Msg            s             78    dim( 6) ctdata  perrcd(1)
003400060925      *
003500060925      *  ?S T R U T T U R E   D A T I?  - - - - - - - - - - - - - - - *
003600060925      *
003700060925      * - Architettura
003800060925     d KPJBA         e ds
003900060925      *
004000060925      * - Reperimento dati utente
004100060925     d TIBS34ds      e ds                                                       *Profili utente
004200060925     d dDatiUte      e ds                                                       *Dati utente
004300060925     d AZUTEds       e ds                  extname(AZUTE00F)                    *Utenti
004400060926      *
004500170628      * - Parametri per TISID7R (Manutenzione versione cappario DPD)
004600060927     d TISI00ds      e ds                  inz
004700060927     d   D00f03      e                     inz(*off)                            *Premuto F3=Fine
004800060927     d   D00f12      e                     inz(*off)                            *Premuto F12=Ritorno
004900060927     d   D00err      e                     inz(*off)                            **on=ERRORE
005000170628     d ds_DPCVE      e ds                  inz  extname(DPCVE10F)
005100060926     d                                          prefix(X:1)
005200060925      *
005300060925     d Status         sds           333
005400060925     d   SDSpgm          *proc
005500060926      *
005600060926     d DSFMT           ds           512
005700060926     d  £Tasto               369    369
005800060926     d  £SFLnrr              378    379b 0
005900060925      *
006000060925      * - Controllo/Inversione date ("XSRDA8")
006100060926     d WLBdat          ds                  inz
006200060925     d  G08dat                 1      8  0 inz
006300060925     d  G08inv                 9     16  0 inz
006400060926     d  G08err                17     17    inz('3')
006500060925     d  G08tgi                18     22  0 inz
006600060925      *
006700060925      *  ?V A R I A B I L I?  - - - - - - - - - - - - - - - - - - - - *
006800060925      *
006900060925      * - Flags
007000060926     d $InzS01         s              1    inz(*on)
007100060925     d $Fine           s              1    inz(*off)
007200060926     d $EoF            s              1    inz(*off)
007300060926     d $OK             s              1    inz(*off)
007400060925      *
007500060925      * - Gestione subfile
007600060925     d S01nrr          s                   like(C01rcd) inz
007700060926     d wPag            s              4  0 inz
007800060926     d wRig            s              3  0 inz
007900060926     d W_SflPag        s              3  0 inz
008000060925      *
008100060926     d $Ord            s              3  0 inz
008200060926     d SAVpdr          s                   like(C1Cpdr) inz
008300060926     d W1Cpdd          s                   like(CVEdde) inz
008400060926     d W1Cpdr          s                   like(CVEdde) inz
008500170628     d WVER            s              5s 0              inz
008600060925      *
008700060925      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
008800060925      *
008900060925
009000060925      *---------------------------------------------------------------*
009100060925      *  RIEPILOGO INDICATORI                                         *
009200060925      *---------------------------------------------------------------*
009300060925      * 10    - Comodo                                                *
009400060925      * 28    - Emissione del messaggio di errore a video             *
009500060925      * 30    - Pulizia del subfile                                   *
009600060925      * 31    - NO emissione del subfile                              *
009700060925      * 33    - Fine dati nel subfile         (sflend)                *
009800060925      * 90    - Generico di errore                                    *
009900060925      *---------------------------------------------------------------*
010000060925
010100060925     c     *Entry        plist
010200060925     c                   parm                    KPJBA
010300060925      *
010400060925      * Operazioni Iniziali
010500060925     c                   exsr      RoutInz
010600060925      *
010700060925      * Gestione Video
010800060925do  1c                   DOW       $Fine   = *off
010900060925     c                   exsr      GesS01
011000060925e   1c                   ENDDO
011100060925      *
011200060925      * Operazioni Finali
011300060925     c                   eval      *inLR   = *on
011400060925
011500060925      *---------------------------------------------------------------*
011600060925      *?Operazioni Iniziali                                          ?*
011700060925      *---------------------------------------------------------------*
011800060925     c     RoutInz       BEGSR
011900060925      *
012000060925      * Reperimento dati job
012100060925     c                   exsr      DatiJob
012200060925      *
012300060925      * Impostazione nome programma a video
012400060925     c                   movel     SDSpgm        V1Tpgm
012500060926      *
012600170628     c***                eval      $Ord    = 1
012700060926      *
012800060925     c                   ENDSR
012900060925
013000060925      *---------------------------------------------------------------*
013100060925      *?Reperimento Dati del job (Utente/Operativi)                  ?*
013200060925      *---------------------------------------------------------------*
013300060925     c     DatiJob       BEGSR
013400060925      *
013500060925     c     *dtaara       define    §azute        azuteds
013600060925     c     *dtaara       define    §datiute      ddatiute
013700060925      *
013800060925     c                   in(E)     *dtaara
013900060925     c                   IF        %ERROR or RSUT = *blanks
014000060925     c                   clear                   Tibs34Ds
014100060925     c                   call      'TIBS34R'
014200060925     c                   parm                    Tibs34Ds
014300060925     c                   in        *dtaara
014400060925     c                   ENDIF
014500060925      *
014600060925     c                   ENDSR
014700060925
014800060925      *---------------------------------------------------------------*
014900060925      *?Gestione videata S01                                         ?*
015000060925      *---------------------------------------------------------------*
015100060925     c     GesS01        BEGSR
015200060925      *
015300060925      * Inizializzazione videata
015400060925if  1c                   if        $InzS01 = *on
015500060925     c                   exsr      InzS01
015600060926     c                   eval      $InzS01 = *off
015700060925e   1c                   endif
015800060925      *
015900060925      * Scrittura di testata e di riga tasti funzionali abilitati
016000060925if  1c                   if        NOT *in90
016100170628     c                   write     SID6T01
016200170628     c                   write     SID6Z01
016300060925e   1c                   endif
016400060926      *
016500060926      * Posizionamento del cursore
016600060927sel 1c                   select
016700060927w   1c                   when      S01nrr <= *zeros
016800061109     c                   eval      *in51   = *on
016900170628     c                   write     SID6D02
017000060927w   1c                   when      C01csr  > *zeros
017100060926     c                   z-add     C01csr        C01rcd
017200060927e   1c                   endsl
017300060926      *
017400060925      * Emissione videata
017500170628     c                   exfmt     SID6C01
017600060926     c                   z-add     £SFLnrr       C01rcd
017700060925     c                   setoff                                       28  90
017800060925     c                   clear                   V1Dmsg
017900060925      *
018000060925sel 1c                   SELECT
018100060925      * F3=Fine
018200060925w   1c                   WHEN      *inKC
018300060925     c                   exsr      F03S01
018400060926      *
018500060926      * F9=Altri dati (SflDrop)
018600060926w   1c***                WHEN      *inKI
018700060926      *
018800060926      * Roll-UP
018900060926w   1c                   WHEN      £Tasto      = RollUp
019000060926     c                   exsr      RollUpS01
019100060925      *
019200060925      * Controllo posizionamenti / opzioni
019300060925x   1c                   OTHER
019400060926     c                   exsr      CtrC01
019500060926sel 2c                   SELECT
019600060926      * - *Err in posizionamenti
019700060926w   2c                   WHEN      *in90
019800060926      * - F5=Rivisualizzazione
019900060926w   2c                   WHEN      *inKE
020000060926     c                   exsr      F05S01
020100060926      * - F6=Inserimento
020200060926w   2c                   WHEN      *inKF
020300060926     c                   exsr      F06S01
020400060926      * - Opzioni
020500060926x   2c                   OTHER
020600060926     c                   exsr      OpzS01
020700060926e   2c                   ENDSL
020800060925      *
020900060925e   1c                   ENDSL
021000060925      *
021100060925     c                   ENDSR
021200060926
021300060926      *---------------------------------------------------------------*
021400060926      *?Inizializzazione videata S01                                 ?*
021500060926      *---------------------------------------------------------------*
021600060926     c     InzS01        BEGSR
021700060926      *
021800060926      * Pulizia subfile
021900060926     c                   seton                                        3031
022000170628     c                   write     SID6C01
022100060926     c                   setoff                                         3133
022200060926      *
022300060926     c                   reset                   $EoF
022400060926     c                   clear                   W_SflPag
022500060926     c                   clear                   C01rcd
022600060926     c                   clear                   C01csr
022700060926     c                   clear                   S01nrr
022800060926     c                   clear                   V1Dmsg
022900060926     c                   setoff                                       28  90
023000061109     c                   movea     *zeros        *in(50)
023100060926      *
023200060926      * Caricamento dei dati da presentare nel sfl
023300060926sel 1c                   select
023400060926w   1c                   when      $Ord    = 1
023500060926x   1c                   other
023600170628     c     W1Cpdd        setll     DPCVE102
023700170628     c                   read      DPCVE102
023800170628     c                   eval      $EoF    = (%eof(DPCVE12I))
023900060926e   1c                   endsl
024000060926do  1c                   DOW           $EoF   = *off
024100060926     c                             and S01nrr < C_SflPag
024200060926     c                   exsr      RollUpS01
024300060926e   1c                   ENDDO
024400060926      *
024500060926      * Impaginazione subfile
024600060926      * -> forza l'impaginazione sul 1° rec. del subfile
024700060926if  1c                   if        S01nrr  >  *zeros
024800060926     c                   eval      C01rcd  =  1
024900060926     c                   eval      C01csr  =  1
025000060926x   1c                   else
025100060926     c                   clear                   C01rcd
025200060926e   1c                   endif
025300061109      *
025400061109      * Impostazione indicatori per attributi di visualizzazione
025500061109     c                   eval      *in44   = (%subst(KNMUS:1:3)='EDP')
025600060926      *
025700060926     c                   ENDSR
025800060926
025900060926      *---------------------------------------------------------------*
026000060926      *?Controllo videata C01                                        ?*
026100060926      *---------------------------------------------------------------*
026200060926     c     CtrC01        BEGSR
026300060926      *
026400060926      * Controllo data decorrenza
026500060926     c                   clear                   W1Cpdd
026600060926      *
026700060926      * Controllo giorno di selezione
026800060926     c                   clear                   W1Cpdr
026900060926if  1c                   if        C1Cpdr <> *zeros
027000060926     c                   clear                   WLBdat
027100060926     c                   eval      G08dat  = C1Cpdr
027200060926     c                   eval      G08err  = '0'
027300060926     c                   call      'XSRDA8'
027400060926     c                   parm                    WLBdat
027500060926if  2c                   if        G08err  = *off
027600060926     c                   eval      C1Cpdr  = G08dat
027700060926     c                   eval      W1Cpdr  = G08inv
027800060926x   2c                   else
027900061109     c                   seton                                        285390
028000060926     c                   eval      V1Dmsg  = $Msg(2)
028100060926     c                   leavesr
028200060926e   2c                   endif
028300060926e   1c                   endif
028400060926      *
028500060926      * Verifica ordinamento richiesto
028600060926     c                   eval      $Ord    = 2
028700060926      *
028800060926      * Impostazione condizione di inizializzazione del SubFile
028900170628     c                   if             C1Cpdr <> SAVpdr
029000060926     c                   eval      $InzS01 = *on
029100060926     c                   eval      SAVpdr  = C1Cpdr
029200060926e   1c                   endif
029300060926      *
029400060926     c                   ENDSR
029500060925
029600060925      *---------------------------------------------------------------*
029700060925      *?Gestione tasto funzionale F3 da videata C01/S01              ?*
029800060925      *?F3=Fine                                                      ?*
029900060925      *---------------------------------------------------------------*
030000060925     c     F03S01        BEGSR
030100060925      *
030200060925      * Chiusura del programma
030300060925     c                   eval      $Fine   = *on
030400060925      *
030500060925     c                   ENDSR
030600060925
030700060925      *---------------------------------------------------------------*
030800060925      *?Gestione tasto funzionale F5 da videata C01/S01              ?*
030900060925      *?F5=Rivisualizzazione                                         ?*
031000060925      *---------------------------------------------------------------*
031100060925     c     F05S01        BEGSR
031200060925      *
031300060925      * Ricaricamento del subfile
031400060925     c                   eval      $InzS01 = *on
031500060925      *
031600060925     c                   ENDSR
031700060925
031800060925      *---------------------------------------------------------------*
031900060925      *?Gestione tasto funzionale F6 da videata C01/S01              ?*
032000060925      *?F6=Inserimento                                               ?*
032100060925      *---------------------------------------------------------------*
032200060925     c     F06S01        BEGSR
032300060925      *
032400060925      * Inserimento consentito SOLO in sede
032500060925if  1c                   if        DUTlpo <> 'S'
032600060925     c                   seton                                        285190
032700061109     c                   eval      V1Dmsg  = $Msg(4)
032800060925     c                   leavesr
032900060925e   1c                   endif
033000060925      *
033100170628     c                   exsr      Call_TISID7
033200060925      *
033300060925     c                   ENDSR
033400060926
033500060926      *---------------------------------------------------------------*
033600060926      *?Gestione tasto funzionale RollUp da videata C01/S01          ?*
033700060926      *?RollUp=Pagina Successiva                                     ?*
033800060926      *---------------------------------------------------------------*
033900060926     c     RollUpS01     BEGSR
034000060926      *
034100060926     c                   eval      S01nrr  = (W_SflPag * C_SflPag)
034200060926     c                   add       1             W_SflPag
034300060926     c                   eval      *in32   = *off
034400060926      *
034500060926      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
034600060926do  1c                   DOW            $EoF   = *off
034700060926     c                             and  S01nrr < (W_SflPag * C_SflPag)
034800060926      *
034900060926      * - Selezione del record dell'archivio
035000060926     c                   exsr      sr_SelRec
035100060926      *
035200060926      *   e Caricamento dati nel record del subfile
035300060926if  2c                   if        $OK     = *on
035400060926     c                   exsr      CarS01
035500060926e   2c                   endif
035600060926      *
035700060926      * - Lettura record successivo nell'archivio
035800060926sel 2c                   select
035900060926w   2c                   when      $Ord    = 1
036000170628     c                   read      DPCVE101
036100170628     c                   eval      $EoF    = (%eof(DPCVE11I))
036200060926w   2c***                when      $Ord    = 2
036300060926x   2c                   other
036400170628     c                   read      DPCVE102
036500170628     c                   eval      $EoF    = (%eof(DPCVE12I))
036600060926e   2c                   endsl
036700060926      *
036800060926e   1c                   ENDDO
036900060926      *
037000060926      * Visualizzazione del SFL (se ci sono dati)
037100060927     c                   eval      *in30   = (S01nrr <= *zeros)
037200060926      *
037300060926      * Attivazione (eventuale) del SFLEND
037400060927     c                   eval      *in33   = ($EoF = *on)
037500060926      *
037600060926      * Posizionamento cursore al 1° rcd della pagina
037700060926if  1c                   if        S01nrr  > *zeros
037800060926     c     S01nrr        div       C_SflPag      wPag
037900060926     c                   mvr                     wRig
038000060926     c                   eval      C01rcd  = wPag * C_SflPag
038100060926     c                   if        wRig    > *zeros
038200060926     c                   eval      C01rcd  = C01rcd + 1
038300060926     c                   else
038400060926     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
038500060926     c                   endif
038600060926x   1c                   else
038700060926     c                   clear                   C01rcd
038800060926e   1c                   endif
038900060926     c                   eval      C01csr  = C01rcd
039000060926      *
039100060926     c                   ENDSR
039200060926
039300060926      *---------------------------------------------------------------*
039400060926      *?Selezione record in base alle selezioni                      ?*
039500060926      *---------------------------------------------------------------*
039600060926     c     sr_SelRec     BEGSR
039700060926      *
039800060926     c                   eval      $OK     = *off
039900060926      *
040000060926sel 1c                   select
040100060926      * Selezione per data validità
040200060926w   1c                   when           C1Cpdr <> *zeros
040300060926     c                             and (W1Cpdr  < CVEdde
040400060926     c                             or   W1Cpdr  > CVEdsc)
040500060926     c                   leavesr
040600060926e   1c                   endsl
040700060926      *
040800060926     c                   eval      $OK     = *on
040900060926      *
041000060926     c                   ENDSR
041100060926
041200060926      *---------------------------------------------------------------*
041300060926      *?Caricamento singolo record nel SubFile S01                   ?*
041400060926      *---------------------------------------------------------------*
041500060926     c     CarS01        BEGSR
041600060926      *
041700170628     c                   clear                   SID6S01
041800060926      * Campi hidden
041900060926     c                   eval      S1Hdde  = CVEdde
042000060926     c                   eval      S1Hdsc  = CVEdsc
042100060926      * Campi di solo output
042200060926     c                   eval      S1Catb  = CVEatb
042300060926     c                   eval      S1Cver  = CVEver
042400060926     c                   reset                   WLBdat
042500060926     c                   eval      G08inv  = CVEdde
042600060926     c                   call      'XSRDA8'
042700060926     c                   parm                    WLBdat
042800060926if  1c                   if        G08err  = *off
042900060926     c                   eval      S1Cdde  = G08dat
043000060926e   1c                   endif
043100060926     c                   reset                   WLBdat
043200060926     c                   eval      G08inv  = CVEdsc
043300060926     c                   call      'XSRDA8'
043400060926     c                   parm                    WLBdat
043500060926if  1c                   if        G08err  = *off
043600060926     c                   eval      S1Cdsc  = G08dat
043700060926e   1c                   endif
043800170628     c                   eval      S1CBCID = CVEBCID
043900060926     c                   eval      S1Cverd = CVEverd
044000060926      *
044100060926     c                   add       1             S01nrr
044200170628     c                   write     SID6S01
044300060926      *
044400060926     c                   ENDSR
044500060926
044600060926      *---------------------------------------------------------------*
044700060926      *?Controllo opzioni subfile                                    ?*
044800060926      *---------------------------------------------------------------*
044900060926     c     OpzS01        BEGSR
045000060926      *
045100170628     c                   readc     SID6S01
045200060926      *
045300170628do  1c                   DOW       NOT %eof(TISID6D)
045400060926      *
045500060926     c                   eval      *in32   = *off
045600061109     c                   movea     *zeros        *in(50)
045700060926     c                   z-add     S01nrr        C01rcd
045800060926      *
045900060926sel 2c                   SELECT
046000060926      * - Nessuna opzione
046100060926w   2c                   WHEN      S1Copz  = *zeros
046200060926      * - 2 = Modifica
046300060926      * - 3 = Copia
046400060926      * - 5 = Visualizzazione
046500060926w   2c                   WHEN           S1Copz = 02
046600061006     c***                          or   S1Copz = 03
046700061006     c                             or   S1Copz = 05
046800060927if  3c                   if             S1Copz  = 02
046900060927     c                             and  S1Catb <> *blanks
047000060927     c                   seton                                        285090
047100061109     c                   eval      V1Dmsg  = $Msg(5)
047200060927     c                   leavesr
047300060927e   3c                   endif
047400170628     c                   exsr      Call_TISID7
047500061109      * - 4 = Cancellazione
047600061109w   2c                   WHEN           S1Copz  = 04
047700061109     c                             and  %subst(KNMUS:1:3) = 'EDP'
047800170630        // emetto windows di richiesta conferma
047900170630        dou *inkf or *inkl;
048000170630          W1DMS1 = 'Si conferma la cancellazione fisica dei dati?';
048100170630          W1DMS2 = '(F6 per confermare)';
048200170630          exfmt SID6W01;
048300170630          select;
048400170630           when *inkf = *on;
048500170628     C                   EVAL      wVER = S1CVER
048600170628     C                   call      'TISID3R2'
048700170628     C                   parm                    wVER
048800170630          endsl;
048900170630        enddo;
049000170628
049100170628      * - 7 = Allineamento
049200170628w   2c                   WHEN      S1Copz  = 07
049300170628     c                             and  %subst(KNMUS:1:3) = 'EDP'
049400170628if  3c                   if        S1Catb  = 'A'
049500170628     C                   call      'TISID3R1'
049600170628x   3c                   else
049700170628     c                   seton                                        285090
049800170628     c                   eval      V1Dmsg  = $Msg(6)
049900170628     c                   leavesr
050000170628e   3c                   endif
050100060926      * - ? = Opzione NON valida
050200060926x   2c                   OTHER
050300060926     c                   seton                                        285090
050400060926     c                   eval      V1Dmsg  = $Msg(3)
050500060926e   2c                   ENDSL
050600060926      *
050700060926      * Aggiornamento sfl
050800060926sel 2c                   select
050900060926w   2c                   when      *in28
051000060926     c                   eval      *in32   = *on
051100060926     c                   z-add     C01rcd        C01csr
051200060926w   2c                   when      *in90   = *on
051300060926     c                   z-add     C01rcd        C01csr
051400060926     c                   clear                   S1Copz
051500060926x   2c                   other
051600060926     c                   clear                   S1Copz
051700060926e   2c                   endsl
051800170628     c                   UPDATE    SID6S01
051900060926if  2c                   if        *in28  OR  *in90
052000060926     c                   leave
052100060926e   2c                   endif
052200170630      * Ricaricamento del subfile
052300170630     c                   eval      $InzS01 = *on
052400060926      *
052500170628     c                   readc     SID6S01
052600060926      *
052700060926e   1c                   ENDDO
052800060926      *
052900060926     c                   ENDSR
053000060926
053100060926      *---------------------------------------------------------------*
053200060926      *?Richiamo programma di manutenzione singolo record            ?*
053300060926      *---------------------------------------------------------------*
053400170628     c     Call_TISID7   BEGSR
053500060926      *
053600060926     c                   clear                   ds_DPCVE
053700060927     c                   reset                   TISI00ds
053800060927      *
053900060927     c                   if        *inKF
054000060927     c                   eval      D00op1  = 'F06'                              *Livello pgm
054100060927     c                   else
054200060927     c                   eval      D00op1  = 'O'                                *Livello pgm
054300060927     c                                     + %trim(%editw(S1Copz:'0  '))
054400060927     c                   eval      D00dri  = S1Hdde
054500060927     c                   eval      D00ver  = S1Cver
054600060927     c                   eval      D00dde  = S1Hdde
054700060927     c                   endif
054800060926      *
054900060927     c                   movel(p)  TISI00ds      KPJBU
055000170628     c                   call      'TISID7R'
055100060926     c                   parm                    KPJBA
055200060926     c                   parm                    ds_DPCVE
055300060927     c                   movel     KPJBU         TISI00ds
055400060926      *
055500060927sel 1c                   SELECT
055600060926      * Se ritorna premuto F3, chiude il programma
055700060927w   1c                   WHEN      D00f03  = *on
055800060926     c                   eval      $Fine   = *on
055900060926      * Se ritorna premuto F12, non fa nulla
056000060927w   1c                   WHEN      D00f12  = *on
056100060926      * Se ritorna un errore, lo visualizza
056200060927w   1c                   WHEN      D00err  = *on
056300060926     c                   seton                                        28  90
056400060927     c                   eval      V1Dmsg  = D00msg
056500061109      * Altrimenti, ricarica il subfile (se F6=Inserimento o se opz.
056600061109      *   3/4)
056700060927w   1c                   WHEN           D00op1  = 'F06'
056800060927     c                             or   S1Copz  = 3
056900061109     c                             or   S1Copz  = 4
057000060927     c                   eval      $InzS01 = *on
057100060927      * Altrimenti, aggiorna dati nel record del sfl (se opz. 2/4/7)
057200060927w   1c                   WHEN           S1Copz  = 2
057300061109     c***                          or   S1Copz  = 4
057400060927     c                             or   S1Copz  = 7
057500060926     c                   eval      S1Hdde  = XVEdde
057600060926     c                   eval      S1Hdsc  = XVEdsc
057700060926     c                   eval      S1Catb  = XVEatb
057800060926     c                   eval      S1Cver  = XVEver
057900060926     c                   reset                   WLBdat
058000060926     c                   eval      G08inv  = XVEdde
058100060926     c                   call      'XSRDA8'
058200060926     c                   parm                    WLBdat
058300060927if  2c                   if        G08err  = *off
058400060926     c                   eval      S1Cdde  = G08dat
058500060927e   2c                   endif
058600060926     c                   reset                   WLBdat
058700060926     c                   eval      G08inv  = XVEdsc
058800060926     c                   call      'XSRDA8'
058900060926     c                   parm                    WLBdat
059000060927if  2c                   if        G08err  = *off
059100060926     c                   eval      S1Cdsc  = G08dat
059200060927e   2c                   endif
059300170628     c                   eval      S1CBCID = XVEBCID
059400060926     c                   eval      S1Cverd = XVEverd
059500060927e   1c                   ENDSL
059600060926      *
059700060926     c                   ENDSR
059800060925
059900060925** - $MSG -------------------------------------------------------------------*
060000060926Accettato solo un parametro di inizio lista                                    1
060100060926Data formalmente errata                                                        2
060200060926Opzione errata                                                                 3
060300061109Inserimento consentito ai soli utenti di SEDE
060400060926Record annullato. Consentito solo il ripristino                                4
060500060926Record non annullato. Impossibile ripristinare                                 5
