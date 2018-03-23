000100060925      *---------------------------------------------------------------*
000200060927      *?CAPPARIO DPD - GESTIONE VERSIONI                             ?*
000300060925      *---------------------------------------------------------------*
000400060925
000500060925     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060925
000700060925      *---------------------------------------------------------------*
000800060925
000900060925      * Versioni
001000060925     fDPCVE01L  if   e           k disk
001100060926     f                                     rename(DPCVE000:DPCVE001)
001200060925     fDPCVE02L  if   e           k disk
001300060925     f                                     rename(DPCVE000:DPCVE002)
001400060925     fTISIE6D   cf   e             workstn
001500060925     f                                     sfile(SIE6S01:S01nrr)
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
003300060926     d $Msg            s             78    dim( 5) ctdata  perrcd(1)
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
004500060926      * - Parametri per TISIE7R (Manutenzione versione cappario DPD)
004600060927     d TISI00ds      e ds                  inz
004700060927     d   D00f03      e                     inz(*off)                            *Premuto F3=Fine
004800060927     d   D00f12      e                     inz(*off)                            *Premuto F12=Ritorno
004900060927     d   D00err      e                     inz(*off)                            **on=ERRORE
005000060926     d ds_DPCVE      e ds                  inz  extname(DPCVE00F)
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
008200060926     d SAVpve          s                   like(C1Cpve) inz
008300060926     d SAVpdd          s                   like(C1Cpdd) inz
008400060926     d SAVpdr          s                   like(C1Cpdr) inz
008500060926     d W1Cpdd          s                   like(CVEdde) inz
008600060926     d W1Cpdr          s                   like(CVEdde) inz
008700060925      *
008800060925      *  ?K E Y - L I S T?  - - - - - - - - - - - - - - - - - - - - - *
008900060925      *
009000060925
009100060925      *---------------------------------------------------------------*
009200060925      *  RIEPILOGO INDICATORI                                         *
009300060925      *---------------------------------------------------------------*
009400060925      * 10    - Comodo                                                *
009500060925      * 28    - Emissione del messaggio di errore a video             *
009600060925      * 30    - Pulizia del subfile                                   *
009700060925      * 31    - NO emissione del subfile                              *
009800060925      * 33    - Fine dati nel subfile         (sflend)                *
009900060925      * 90    - Generico di errore                                    *
010000060925      *---------------------------------------------------------------*
010100060925
010200060925     c     *Entry        plist
010300060925     c                   parm                    KPJBA
010400060925      *
010500060925      * Operazioni Iniziali
010600060925     c                   exsr      RoutInz
010700060925      *
010800060925      * Gestione Video
010900060925do  1c                   DOW       $Fine   = *off
011000060925     c                   exsr      GesS01
011100060925e   1c                   ENDDO
011200060925      *
011300060925      * Operazioni Finali
011400060925     c                   eval      *inLR   = *on
011500060925
011600060925      *---------------------------------------------------------------*
011700060925      *?Operazioni Iniziali                                          ?*
011800060925      *---------------------------------------------------------------*
011900060925     c     RoutInz       BEGSR
012000060925      *
012100060925      * Reperimento dati job
012200060925     c                   exsr      DatiJob
012300060925      *
012400060925      * Impostazione nome programma a video
012500060925     c                   movel     SDSpgm        V1Tpgm
012600060926      *
012700060926     c                   eval      $Ord    = 1
012800060926      *
012900060925     c                   ENDSR
013000060925
013100060925      *---------------------------------------------------------------*
013200060925      *?Reperimento Dati del job (Utente/Operativi)                  ?*
013300060925      *---------------------------------------------------------------*
013400060925     c     DatiJob       BEGSR
013500060925      *
013600060925     c     *dtaara       define    §azute        azuteds
013700060925     c     *dtaara       define    §datiute      ddatiute
013800060925      *
013900060925     c                   in(E)     *dtaara
014000060925     c                   IF        %ERROR or RSUT = *blanks
014100060925     c                   clear                   Tibs34Ds
014200060925     c                   call      'TIBS34R'
014300060925     c                   parm                    Tibs34Ds
014400060925     c                   in        *dtaara
014500060925     c                   ENDIF
014600060925      *
014700060925     c                   ENDSR
014800060925
014900060925      *---------------------------------------------------------------*
015000060925      *?Gestione videata S01                                         ?*
015100060925      *---------------------------------------------------------------*
015200060925     c     GesS01        BEGSR
015300060925      *
015400060925      * Inizializzazione videata
015500060925if  1c                   if        $InzS01 = *on
015600060925     c                   exsr      InzS01
015700060926     c                   eval      $InzS01 = *off
015800060925e   1c                   endif
015900060925      *
016000060925      * Scrittura di testata e di riga tasti funzionali abilitati
016100060925if  1c                   if        NOT *in90
016200060925     c                   write     SIE6T01
016300060925     c                   write     SIE6Z01
016400060925e   1c                   endif
016500060926      *
016600060926      * Posizionamento del cursore
016700060927sel 1c                   select
016800060927w   1c                   when      S01nrr <= *zeros
016900060926     c                   eval      *in41   = *on
017000060926     c                   write     SIE6D02
017100060927w   1c                   when      C01csr  > *zeros
017200060926     c                   z-add     C01csr        C01rcd
017300060927e   1c                   endsl
017400060926      *
017500060925      * Emissione videata
017600060925     c                   exfmt     SIE6C01
017700060926     c                   z-add     £SFLnrr       C01rcd
017800060925     c                   setoff                                       28  90
017900060925     c                   clear                   V1Dmsg
018000060925      *
018100060925sel 1c                   SELECT
018200060925      * F3=Fine
018300060925w   1c                   WHEN      *inKC
018400060925     c                   exsr      F03S01
018500060926      *
018600060926      * F9=Altri dati (SflDrop)
018700060926w   1c***                WHEN      *inKI
018800060926      *
018900060926      * Roll-UP
019000060926w   1c                   WHEN      £Tasto      = RollUp
019100060926     c                   exsr      RollUpS01
019200060925      *
019300060925      * Controllo posizionamenti / opzioni
019400060925x   1c                   OTHER
019500060926     c                   exsr      CtrC01
019600060926sel 2c                   SELECT
019700060926      * - *Err in posizionamenti
019800060926w   2c                   WHEN      *in90
019900060926      * - F5=Rivisualizzazione
020000060926w   2c                   WHEN      *inKE
020100060926     c                   exsr      F05S01
020200060926      * - F6=Inserimento
020300060926w   2c                   WHEN      *inKF
020400060926     c                   exsr      F06S01
020500060926      * - Opzioni
020600060926x   2c                   OTHER
020700060926     c                   exsr      OpzS01
020800060926e   2c                   ENDSL
020900060925      *
021000060925e   1c                   ENDSL
021100060925      *
021200060925     c                   ENDSR
021300060926
021400060926      *---------------------------------------------------------------*
021500060926      *?Inizializzazione videata S01                                 ?*
021600060926      *---------------------------------------------------------------*
021700060926     c     InzS01        BEGSR
021800060926      *
021900060926      * Pulizia subfile
022000060926     c                   seton                                        3031
022100060926     c                   write     SIE6C01
022200060926     c                   setoff                                         3133
022300060926      *
022400060926     c                   reset                   $EoF
022500060926     c                   clear                   W_SflPag
022600060926     c                   clear                   C01rcd
022700060926     c                   clear                   C01csr
022800060926     c                   clear                   S01nrr
022900060926     c                   clear                   V1Dmsg
023000060926     c                   setoff                                       28  90
023100060926     c                   movea     *zeros        *in(41)
023200060926      *
023300060926      * Caricamento dei dati da presentare nel sfl
023400060926sel 1c                   select
023500060926w   1c                   when      $Ord    = 1
023600060926     c     C1Cpve        setll     DPCVE001
023700060926     c                   read      DPCVE001
023800060926     c                   eval      $EoF    = (%eof(DPCVE01L))
023900060926w   1c***                when      $Ord    = 2
024000060926x   1c                   other
024100060926     c     W1Cpdd        setll     DPCVE002
024200060926     c                   read      DPCVE002
024300060926     c                   eval      $EoF    = (%eof(DPCVE02L))
024400060926e   1c                   endsl
024500060926do  1c                   DOW           $EoF   = *off
024600060926     c                             and S01nrr < C_SflPag
024700060926     c                   exsr      RollUpS01
024800060926e   1c                   ENDDO
024900060926      *
025000060926      * Impaginazione subfile
025100060926      * -> forza l'impaginazione sul 1° rec. del subfile
025200060926if  1c                   if        S01nrr  >  *zeros
025300060926     c                   eval      C01rcd  =  1
025400060926     c                   eval      C01csr  =  1
025500060926x   1c                   else
025600060926     c                   clear                   C01rcd
025700060926e   1c                   endif
025800060926      *
025900060926     c                   ENDSR
026000060926
026100060926      *---------------------------------------------------------------*
026200060926      *?Controllo videata C01                                        ?*
026300060926      *---------------------------------------------------------------*
026400060926     c     CtrC01        BEGSR
026500060926      *
026600060926      * Controllo numero parametri inseriti
026700060926if  1c                   if             C1Cpve <> *zeros
026800060926     c                             and  C1Cpdd <> *zeros
026900060926     c                   seton                                        284190
027000060926     c                   seton                                          42
027100060926     c                   eval      V1Dmsg  = $Msg(1)
027200060926     c                   leavesr
027300060926e   1c                   endif
027400060926      *
027500060926      * Controllo data decorrenza
027600060926     c                   clear                   W1Cpdd
027700060926if  1c                   if        C1Cpdd <> *zeros
027800060926     c                   clear                   WLBdat
027900060926     c                   eval      G08dat  = C1Cpdd
028000060926     c                   eval      G08err  = '0'
028100060926     c                   call      'XSRDA8'
028200060926     c                   parm                    WLBdat
028300060926if  2c                   if        G08err  = *off
028400060926     c                   eval      C1Cpdd  = G08dat
028500060926     c                   eval      W1Cpdd  = G08inv
028600060926x   2c                   else
028700060926     c                   seton                                        284290
028800060926     c                   eval      V1Dmsg  = $Msg(2)
028900060926     c                   leavesr
029000060926e   2c                   endif
029100060926e   1c                   endif
029200060926      *
029300060926      * Controllo giorno di selezione
029400060926     c                   clear                   W1Cpdr
029500060926if  1c                   if        C1Cpdr <> *zeros
029600060926     c                   clear                   WLBdat
029700060926     c                   eval      G08dat  = C1Cpdr
029800060926     c                   eval      G08err  = '0'
029900060926     c                   call      'XSRDA8'
030000060926     c                   parm                    WLBdat
030100060926if  2c                   if        G08err  = *off
030200060926     c                   eval      C1Cpdr  = G08dat
030300060926     c                   eval      W1Cpdr  = G08inv
030400060926x   2c                   else
030500060926     c                   seton                                        284390
030600060926     c                   eval      V1Dmsg  = $Msg(2)
030700060926     c                   leavesr
030800060926e   2c                   endif
030900060926e   1c                   endif
031000060926      *
031100060926      * Verifica ordinamento richiesto
031200060926if  1c                   if        C1Cpve <> *zeros
031300060926     c                   eval      $Ord    = 1
031400060926x   1c                   else
031500060926     c                   eval      $Ord    = 2
031600060926e   1c                   endif
031700060926      *
031800060926      * Impostazione condizione di inizializzazione del SubFile
031900060926if  1c                   if             C1Cpve <> SAVpve
032000060926     c                             or   C1Cpdd <> SAVpdd
032100060926     c                             or   C1Cpdr <> SAVpdr
032200060926     c                   eval      $InzS01 = *on
032300060926     c                   eval      SAVpve  = C1Cpve
032400060926     c                   eval      SAVpdd  = C1Cpdd
032500060926     c                   eval      SAVpdr  = C1Cpdr
032600060926e   1c                   endif
032700060926      *
032800060926     c                   ENDSR
032900060925
033000060925      *---------------------------------------------------------------*
033100060925      *?Gestione tasto funzionale F3 da videata C01/S01              ?*
033200060925      *?F3=Fine                                                      ?*
033300060925      *---------------------------------------------------------------*
033400060925     c     F03S01        BEGSR
033500060925      *
033600060925      * Chiusura del programma
033700060925     c                   eval      $Fine   = *on
033800060925      *
033900060925     c                   ENDSR
034000060925
034100060925      *---------------------------------------------------------------*
034200060925      *?Gestione tasto funzionale F5 da videata C01/S01              ?*
034300060925      *?F5=Rivisualizzazione                                         ?*
034400060925      *---------------------------------------------------------------*
034500060925     c     F05S01        BEGSR
034600060925      *
034700060925      * Ricaricamento del subfile
034800060925     c                   eval      $InzS01 = *on
034900060925      *
035000060925     c                   ENDSR
035100060925
035200060925      *---------------------------------------------------------------*
035300060925      *?Gestione tasto funzionale F6 da videata C01/S01              ?*
035400060925      *?F6=Inserimento                                               ?*
035500060925      *---------------------------------------------------------------*
035600060925     c     F06S01        BEGSR
035700060925      *
035800060925      * Inserimento consentito SOLO in sede
035900060925if  1c                   if        DUTlpo <> 'S'
036000060925     c                   seton                                        285190
036100060925     c                   eval      V1Dmsg  = $Msg(3)
036200060925     c                   leavesr
036300060925e   1c                   endif
036400060925      *
036500060927     c                   exsr      Call_TISIE7
036600060925      *
036700060925     c                   ENDSR
036800060926
036900060926      *---------------------------------------------------------------*
037000060926      *?Gestione tasto funzionale RollUp da videata C01/S01          ?*
037100060926      *?RollUp=Pagina Successiva                                     ?*
037200060926      *---------------------------------------------------------------*
037300060926     c     RollUpS01     BEGSR
037400060926      *
037500060926     c                   eval      S01nrr  = (W_SflPag * C_SflPag)
037600060926     c                   add       1             W_SflPag
037700060926     c                   eval      *in32   = *off
037800060926      *
037900060926      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
038000060926do  1c                   DOW            $EoF   = *off
038100060926     c                             and  S01nrr < (W_SflPag * C_SflPag)
038200060926      *
038300060926      * - Selezione del record dell'archivio
038400060926     c                   exsr      sr_SelRec
038500060926      *
038600060926      *   e Caricamento dati nel record del subfile
038700060926if  2c                   if        $OK     = *on
038800060926     c                   exsr      CarS01
038900060926e   2c                   endif
039000060926      *
039100060926      * - Lettura record successivo nell'archivio
039200060926sel 2c                   select
039300060926w   2c                   when      $Ord    = 1
039400060926     c                   read      DPCVE001
039500060926     c                   eval      $EoF    = (%eof(DPCVE01L))
039600060926w   2c***                when      $Ord    = 2
039700060926x   2c                   other
039800060926     c                   read      DPCVE002
039900060926     c                   eval      $EoF    = (%eof(DPCVE02L))
040000060926e   2c                   endsl
040100060926      *
040200060926e   1c                   ENDDO
040300060926      *
040400060926      * Visualizzazione del SFL (se ci sono dati)
040500060927     c                   eval      *in30   = (S01nrr <= *zeros)
040600060926      *
040700060926      * Attivazione (eventuale) del SFLEND
040800060927     c                   eval      *in33   = ($EoF = *on)
040900060926      *
041000060926      * Posizionamento cursore al 1° rcd della pagina
041100060926if  1c                   if        S01nrr  > *zeros
041200060926     c     S01nrr        div       C_SflPag      wPag
041300060926     c                   mvr                     wRig
041400060926     c                   eval      C01rcd  = wPag * C_SflPag
041500060926     c                   if        wRig    > *zeros
041600060926     c                   eval      C01rcd  = C01rcd + 1
041700060926     c                   else
041800060926     c                   eval      C01rcd  = C01rcd - C_SflPag + 1
041900060926     c                   endif
042000060926x   1c                   else
042100060926     c                   clear                   C01rcd
042200060926e   1c                   endif
042300060926     c                   eval      C01csr  = C01rcd
042400060926      *
042500060926     c                   ENDSR
042600060926
042700060926      *---------------------------------------------------------------*
042800060926      *?Selezione record in base alle selezioni                      ?*
042900060926      *---------------------------------------------------------------*
043000060926     c     sr_SelRec     BEGSR
043100060926      *
043200060926     c                   eval      $OK     = *off
043300060926      *
043400060926sel 1c                   select
043500060926      * Selezione per data validità
043600060926w   1c                   when           C1Cpdr <> *zeros
043700060926     c                             and (W1Cpdr  < CVEdde
043800060926     c                             or   W1Cpdr  > CVEdsc)
043900060926     c                   leavesr
044000060926e   1c                   endsl
044100060926      *
044200060926     c                   eval      $OK     = *on
044300060926      *
044400060926     c                   ENDSR
044500060926
044600060926      *---------------------------------------------------------------*
044700060926      *?Caricamento singolo record nel SubFile S01                   ?*
044800060926      *---------------------------------------------------------------*
044900060926     c     CarS01        BEGSR
045000060926      *
045100060926     c                   clear                   SIE6S01
045200060926      * Campi hidden
045300060926     c                   eval      S1Hdde  = CVEdde
045400060926     c                   eval      S1Hdsc  = CVEdsc
045500060926      * Campi di solo output
045600060926     c                   eval      S1Catb  = CVEatb
045700060926     c                   eval      S1Cver  = CVEver
045800060926     c                   reset                   WLBdat
045900060926     c                   eval      G08inv  = CVEdde
046000060926     c                   call      'XSRDA8'
046100060926     c                   parm                    WLBdat
046200060926if  1c                   if        G08err  = *off
046300060926     c                   eval      S1Cdde  = G08dat
046400060926e   1c                   endif
046500060926     c                   reset                   WLBdat
046600060926     c                   eval      G08inv  = CVEdsc
046700060926     c                   call      'XSRDA8'
046800060926     c                   parm                    WLBdat
046900060926if  1c                   if        G08err  = *off
047000060926     c                   eval      S1Cdsc  = G08dat
047100060926e   1c                   endif
047200060926     c                   eval      S1Cdes  = CVEdes
047300060926     c                   eval      S1Cverd = CVEverd
047400060926      *
047500060926     c                   add       1             S01nrr
047600060926     c                   write     SIE6S01
047700060926      *
047800060926     c                   ENDSR
047900060926
048000060926      *---------------------------------------------------------------*
048100060926      *?Controllo opzioni subfile                                    ?*
048200060926      *---------------------------------------------------------------*
048300060926     c     OpzS01        BEGSR
048400060926      *
048500060926     c                   readc     SIE6S01
048600060926      *
048700060926do  1c                   DOW       NOT %eof(TISIE6D)
048800060926      *
048900060926     c                   eval      *in32   = *off
049000060926     c                   movea     *zeros        *in(40)
049100060926     c                   z-add     S01nrr        C01rcd
049200060926      *
049300060926sel 2c                   SELECT
049400060926      * - Nessuna opzione
049500060926w   2c                   WHEN      S1Copz  = *zeros
049600060926      * - 2 = Modifica
049700060926      * - 3 = Copia
049800060926      * - 5 = Visualizzazione
049900060926w   2c                   WHEN           S1Copz = 02
050000061006     c***                          or   S1Copz = 03
050100061006     c                             or   S1Copz = 05
050200060927if  3c                   if             S1Copz  = 02
050300060927     c                             and  S1Catb <> *blanks
050400060927     c                   seton                                        285090
050500060927     c                   eval      V1Dmsg  = $Msg(4)
050600060927     c                   leavesr
050700060927e   3c                   endif
050800060926     c                   exsr      Call_TISIE7
050900060926      * - 4 = Annullamento
051000061006w   2c***                WHEN      S1Copz  = 04
051100061006if  3c***                if        S1Catb  = *blanks
051200061006     c***                exsr      Call_TISIE7
051300061006x   3c***                else
051400061006     c***                seton                                        285090
051500061006     c***                eval      V1Dmsg  = $Msg(4)
051600061006     c***                leavesr
051700061006e   3c***                endif
051800060926      * - 7 = Ripristino
051900061006w   2c***                WHEN      S1Copz  = 07
052000061006if  3c***                if        S1Catb  = 'A'
052100061006     c***                exsr      Call_TISIE7
052200061006x   3c***                else
052300061006     c***                seton                                        285090
052400061006     c***                eval      V1Dmsg  = $Msg(5)
052500061006     c***                leavesr
052600061006e   3c***                endif
052700060926      * - ? = Opzione NON valida
052800060926x   2c                   OTHER
052900060926     c                   seton                                        285090
053000060926     c                   eval      V1Dmsg  = $Msg(3)
053100060926e   2c                   ENDSL
053200060926      *
053300060926      * Aggiornamento sfl
053400060926sel 2c                   select
053500060926w   2c                   when      *in28
053600060926     c                   eval      *in32   = *on
053700060926     c                   z-add     C01rcd        C01csr
053800060926w   2c                   when      *in90   = *on
053900060926     c                   z-add     C01rcd        C01csr
054000060926     c                   clear                   S1Copz
054100060926x   2c                   other
054200060926     c                   clear                   S1Copz
054300060926e   2c                   endsl
054400060926     c                   UPDATE    SIE6S01
054500060926if  2c                   if        *in28  OR  *in90
054600060926     c                   leave
054700060926e   2c                   endif
054800060926      *
054900060926     c                   readc     SIE6S01
055000060926      *
055100060926e   1c                   ENDDO
055200060926      *
055300060926     c                   ENDSR
055400060926
055500060926      *---------------------------------------------------------------*
055600060926      *?Richiamo programma di manutenzione singolo record            ?*
055700060926      *---------------------------------------------------------------*
055800060926     c     Call_TISIE7   BEGSR
055900060926      *
056000060926     c                   clear                   ds_DPCVE
056100060927     c                   reset                   TISI00ds
056200060927      *
056300060927     c                   if        *inKF
056400060927     c                   eval      D00op1  = 'F06'                              *Livello pgm
056500060927     c                   else
056600060927     c                   eval      D00op1  = 'O'                                *Livello pgm
056700060927     c                                     + %trim(%editw(S1Copz:'0  '))
056800060927     c                   eval      D00dri  = S1Hdde
056900060927     c                   eval      D00ver  = S1Cver
057000060927     c                   eval      D00dde  = S1Hdde
057100060927     c                   endif
057200060926      *
057300060927     c                   movel(p)  TISI00ds      KPJBU
057400060926     c                   call      'TISIE7R'
057500060926     c                   parm                    KPJBA
057600060926     c                   parm                    ds_DPCVE
057700060927     c                   movel     KPJBU         TISI00ds
057800060926      *
057900060927sel 1c                   SELECT
058000060926      * Se ritorna premuto F3, chiude il programma
058100060927w   1c                   WHEN      D00f03  = *on
058200060926     c                   eval      $Fine   = *on
058300060926      * Se ritorna premuto F12, non fa nulla
058400060927w   1c                   WHEN      D00f12  = *on
058500060926      * Se ritorna un errore, lo visualizza
058600060927w   1c                   WHEN      D00err  = *on
058700060926     c                   seton                                        28  90
058800060927     c                   eval      V1Dmsg  = D00msg
058900060927      * Altrimenti, ricarica il subfile (se F6=Inserimento)
059000060927w   1c                   WHEN           D00op1  = 'F06'
059100060927     c                             or   S1Copz  = 3
059200060927     c                   eval      $InzS01 = *on
059300060927      * Altrimenti, aggiorna dati nel record del sfl (se opz. 2/4/7)
059400060927w   1c                   WHEN           S1Copz  = 2
059500060927     c                             or   S1Copz  = 4
059600060927     c                             or   S1Copz  = 7
059700060926     c                   eval      S1Hdde  = XVEdde
059800060926     c                   eval      S1Hdsc  = XVEdsc
059900060926     c                   eval      S1Catb  = XVEatb
060000060926     c                   eval      S1Cver  = XVEver
060100060926     c                   reset                   WLBdat
060200060926     c                   eval      G08inv  = XVEdde
060300060926     c                   call      'XSRDA8'
060400060926     c                   parm                    WLBdat
060500060927if  2c                   if        G08err  = *off
060600060926     c                   eval      S1Cdde  = G08dat
060700060927e   2c                   endif
060800060926     c                   reset                   WLBdat
060900060926     c                   eval      G08inv  = XVEdsc
061000060926     c                   call      'XSRDA8'
061100060926     c                   parm                    WLBdat
061200060927if  2c                   if        G08err  = *off
061300060926     c                   eval      S1Cdsc  = G08dat
061400060927e   2c                   endif
061500060926     c                   eval      S1Cdes  = XVEdes
061600060926     c                   eval      S1Cverd = XVEverd
061700060927e   1c                   ENDSL
061800060926      *
061900060926     c                   ENDSR
062000060925
062100060925** - $MSG -------------------------------------------------------------------*
062200060926Accettato solo un parametro di inizio lista                                    1
062300060926Data formalmente errata                                                        2
062400060926Opzione errata                                                                 3
062500060926Record annullato. Consentito solo il ripristino                                4
062600060926Record non annullato. Impossibile ripristinare                                 5
