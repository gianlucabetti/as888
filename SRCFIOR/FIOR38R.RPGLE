000100071024      * FIOR38R *-----------------------------------------------------*
000200071023      *?ATTRIBUZIONE GIRO AD ANAGRAFICA CLIENTI RITIRO               ?*
000300071023      *---------------------------------------------------------------*
000400071023
000500071023     h decedit('0,') datedit(*YMD.) option(*nodebugio)
000600071023
000700071023      *---------------------------------------------------------------*
000800071023
000900071023     fFNACR11L  if   e           k disk
001000071023      *
001100071024     fFIOR38D   cf   e             workstn usropn
001200071024     f                                     sfile(OR38S01:S01nrr)
001300071023     f                                     infds(DSFMT)
001400071023
001500071023      *
001600071023      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
001700071023      *
001800071023      * - Nr. di righe del sfl (SFLPAG)
001900071023     d C_SflPag        c                   const(05)
002000071023      * - Tasti funzionali
002100071023     d C_Enter         c                   const(x'F1')
002200071023     d C_RollUp        c                   const(x'F5')
002300071023      *
002400071023      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002500071023      *
002600071023      * - Messaggi di errore
002700071023     D $Msg            S             78    dim( 3)  ctdata  perrcd(1)
002800071023      *
002900071023      *?  S T R U T T U R E   D A T I   - - - - - - - - - - - - - - -?*
003000071023      *
003100071023      * - Parametri
003200071023     d KPJBA         e ds
003300071024     d FIOR38ds      e ds                  inz
003400071023      *
003500071023      * - Reperimento dati utente
003600071023     d TIBS34ds      e ds
003700071023     d AZUTEds       e ds                  extname(AZUTE00F)
003800071023     d dDatiUte      e ds
003900071023      *
004000071023      * - Gestione anagrafica giri
004100071023     d FIDG09ds      e ds                  inz
004200071023      *
004300071023     d Status         sds           333
004400071023     d  SDSpgm           *proc
004500071023      *
004600071023     d DSFMT           ds           512
004700071023     d  £Tasto               369    369
004800071023     d  £SFLnrr              378    379b 0
004900071023      *
005000071023      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
005100071023      *
005200071023      * - Flags
005300071023     d $InzS01         s              1    inz(*on)
005400071023     d $Fine           s              1    inz(*off)
005500071023     d $EoF            s              1    inz(*off)
005600071023     d $OK             s              1    inz(*off)
005700071023      * - Indici di schiera
005800071023     d xx              s              3  0 inz
005900071023      * - Gestione video
006000071023     d S01nrr          s                   like(C01RcdNbr) inz
006100071023     d wPag            s              4  0 inz
006200071023     d wRig            s              3  0 inz
006300071023     d W_SflPag        s              3  0 inz
006400071023      * - Campi di Comodo
006500071023      *
006600071023      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
006700071023      *
006800071023
006900071023      *---------------------------------------------------------------*
007000071023      *?RIEPILOGO INDICATORI                                         ?*
007100071023      *---------------------------------------------------------------*
007200071023      *  25    - Comodo                                               *
007300071023      *  28    - Emissione del messaggio di errore a video            *
007400071023      *  30    - Pulizia del subfile                            - C01 *
007500071023      *  31    - NO emissione del subfile                       - C01 *
007600071023      *  32    - Attivazione SflNxtChg                          - C01 *
007700071023      *  33    - Fine dati nel subfile         (sflend)         - C01 *
007800071023      *  41    - Gestione campo opzione                         - S01 *
007900071023      *  50    - Opzione errata                                 - S01 *
008000071023      *  90    - Generico di errore                                   *
008100071023      *---------------------------------------------------------------*
008200071023
008300071023     c     *Entry        plist
008400071023     c                   parm                    KPJBA
008500071023      *
008600071023      * Operazioni iniziali
008700071023     c                   exsr      RoutInz
008800071023      *
008900071023      * Gestione video
009000071023do  1c                   dow       $Fine   = *off
009100071023     c                   exsr      GesS01
009200071023e   1c                   enddo
009300071023      *
009400071023      * Operazioni finali
009500071024     c                   movel(p)  FIOR38ds      KPJBU
009600071112     c                   eval      *inLR   = *on
009700071023
009800071023      *---------------------------------------------------------------*
009900071023      *?*InzSr                                                       ?*
010000071023      *---------------------------------------------------------------*
010100071023     c     *InzSr        BEGSR
010200071023      *
010300071023      * Reperimento dati job
010400071023     c                   exsr      DatiJob
010500071023      *
010600071023     c                   ENDSR
010700071023
010800071023      *---------------------------------------------------------------*
010900071023      *?Reperimento Dati del job (Utente/Operativi)                  ?*
011000071023      *---------------------------------------------------------------*
011100071023     c     DatiJob       BEGSR
011200071023      *
011300071023     c     *dtaara       define    §azute        azuteds
011400071023     c     *dtaara       define    §datiute      ddatiute
011500071023      *
011600071023     c                   in(E)     *dtaara
011700071023     c                   IF        %ERROR or RSUT = *blanks
011800071023     c                   clear                   Tibs34Ds
011900071023     c                   call      'TIBS34R'
012000071023     c                   parm                    Tibs34Ds
012100071023     c                   in        *dtaara
012200071023     c                   ENDIF
012300071023      *
012400071023     c                   ENDSR
012500071023
012600071023      *---------------------------------------------------------------*
012700071023      *?Operazioni Iniziali                                          ?*
012800071023      *---------------------------------------------------------------*
012900071023     c     RoutInz       BEGSR
013000071023      *
013100071023     c                   eval      $Fine    = *off
013200071023     c                   eval      $EoF     = *off
013300071023     c                   eval      $InzS01  = *on
013400071023      *
013500071023     c                   if        KPJBU   <> *blanks
013600071024     c                   movel     KPJBU         FIOR38ds
013700071023     c                   else
013800071024     c                   clear                   FIOR38ds
013900071023     c                   eval      $Fine    = *on
014000071023     c                   leavesr
014100071023     c                   endif
014200071023      *
014300071023      * Pulizia campi di output
014400071024     c                   clear                   O38pocgi
014500071024     c                   clear                   O38cgi
014600071024     c                   clear                   O38chk
014700071024     c                   clear                   O38ret
014800071024     c                   clear                   O38err
014900071024     c                   clear                   O38msg
015000071023      *
015100071023      * Controllo parametri ricevuti
015200071023sel 1c                   SELECT
015300071023      * - Controllo opzione richiesta:
015400071023      *   " " = Solo visualizzazione elenco;
015500071023      *   "1" = Selezione singolo giro;
015600071023      *   "9" = Verifica esistenza giri.
015700071024w   1c                   WHEN           I38opz <> *blanks
015800071024     c                             and  I38opz <> '1'
015900071024     c                             and  I38opz <> '9'
016000071024     c                   eval      O38err   = 'E'
016100071024     c                   eval      O38msg   = $Msg(01)
016200071023     c                   eval      $Fine    = *on
016300071023     c                   leavesr
016400071023      * - Controllo codice cliente ritiro (obbligatorio)
016500071024w   1c                   WHEN      I38cro  <= *zeros
016600071024     c                   eval      O38err   = 'E'
016700071024     c                   eval      O38msg   = $Msg(02)
016800071023     c                   eval      $Fine    = *on
016900071023     c                   leavesr
017000071023e   1c                   ENDSL
017100071023      *
017200071023      * Richiesta verifica esistenza giri ritiro per cliente
017300071024     c                   if        I38opz   = '9'
017400071023     c                   exsr      sr_ChkCGI
017500071023     c                   eval      $Fine    = *on
017600071023     c                   leavesr
017700071023     c                   endif
017800071023      *
017900071023      * Apertura display file
018000071024     c                   if        NOT  %open(FIOR38D)
018100071024     c                   open      FIOR38D
018200071023     c                   endif
018300071023      *
018400071023      * Impostazione nome programma a video
018500071023     c                   movel     SDSpgm        C01pgm
018600071023      *
018700071023      * Impostazione dati in testata
018800071024     c                   move      I38cro        C01cro
018900071023      *
019000071023      * Impostazione attributi di visualizzazione in testata
019100071026     c*** NON qui:       eval      *in41    = (I38opz = '1')
019200071023      *
019300071023     c                   ENDSR
019400071023
019500071023      *---------------------------------------------------------------*
019600071023      *?Verifica esistenza giri ritiro per cliente ritiro            ?*
019700071023      *---------------------------------------------------------------*
019800071023     c     sr_ChkCGI     BEGSR
019900071023      *
020000071024     c                   eval      O38chk   = *off
020100071023      *
020200071024     c     I38cro        setll     FNACR100
020300071024     c     I38cro        reade     FNACR100
020400071023do  1c                   DOW       NOT  %eof(FNACR11L)
020500071023      * - Selezione del record dell'archivio
020600071023     c                   exsr      sr_SelRec
020700071023if  2c                   if        $Ok      = *on
020800071024     c                   eval      O38chk   = *on
020900071023     c                   leavesr
021000071023e   2c                   endif
021100071024     c     I38cro        reade     FNACR100
021200071023e   1c                   ENDDO
021300071023      *
021400071023     c                   ENDSR
021500071023
021600071023      *---------------------------------------------------------------*
021700071023      *?Gestione videata S01                                         ?*
021800071023      *---------------------------------------------------------------*
021900071023     c     GesS01        BEGSR
022000071023      *
022100071023      * Inizializzazione videata
022200071023if  1c                   if        $InzS01   = *on
022300071023     c                   exsr      InzS01
022400071023     c                   eval      $InzS01   = *off
022500071023e   1c                   endif
022600071023      *
022700071023      * Emissione videata
022800071023      * (con posizionamento del cursore)
022900071024     c                   write     OR38W01
023000071023sel 1c                   select
023100071023w   1c                   when      S01nrr   <= *zeros
023200071024     c                   write     OR38D01
023300071023w   1c                   when      C01CsrRrn > *zeros
023400071023     c                   z-add     C01CsrRrn     C01RcdNbr
023500071023e   1c                   endsl
023600071024     c                   exfmt     OR38C01
023700071023     c                   z-add     £SFLnrr       C01RcdNbr
023800071023     c                   setoff                                       28  90
023900071023     c                   clear                   V1Dmsg
024000071023      *
024100071023sel 1c                   SELECT
024200071023      * F5=Rivisualizzazione
024300071023w   2c                   WHEN      *inKE
024400071023     c                   eval      $InzS01   = *on
024500071023      * F12=Ritorno
024600071023w   2c                   WHEN      *inKL
024700071023     c                   exsr      F12S01
024800071023      * Roll-UP
024900071023w   1c                   WHEN      £Tasto    = C_RollUp
025000071023     c                   exsr      RollUpS01
025100071023      * Controllo opzioni
025200071023x   1c                   OTHER
025300071024     c                   if        I38opz    = '1'
025400071023     c                   exsr      OpzS01
025500071023     c                   endif
025600071023      *
025700071023e   1c                   ENDSL
025800071023      *
025900071023     c                   ENDSR
026000071023
026100071023      *---------------------------------------------------------------*
026200071023      *?Inizializzazione videata S01                                 ?*
026300071023      *---------------------------------------------------------------*
026400071023     c     InzS01        BEGSR
026500071023      *
026600071023      * Pulizia subfile
026700071023     c                   seton                                        3031
026800071024     c                   write     OR38C01
026900071023     c                   setoff                                         3133
027000071023      *
027100071023     c                   reset                   $EoF
027200071023     c                   clear                   W_SflPag
027300071023     c                   clear                   C01RcdNbr
027400071023     c                   clear                   C01CsrRrn
027500071023     c                   clear                   S01nrr
027600071023     c                   clear                   V1Dmsg
027700071023     c                   setoff                                       28  90
027800071026     c                   movea     *zeros        *in(40)
027900071023      *
028000071023      * Caricamento dei dati da presentare nel sfl
028100071024     c     I38cro        setll     FNACR100
028200071024     c     I38cro        reade     FNACR100
028300071023     c                   eval      $EoF     = (%eof(FNACR11L))
028400071023do  1c                   DOW            $EoF   = *off
028500071023     c                             and  S01nrr < C_SflPag
028600071023     c                   exsr      RollUpS01
028700071023e   1c                   ENDDO
028800071023      *
028900071023      * Impaginazione subfile
029000071023      * -> forza l'impaginazione sul 1° rec. del subfile
029100071023if  1c                   if        S01nrr    > *zeros
029200071023     c                   eval      C01RcdNbr = 1
029300071023     c                   eval      C01CsrRrn = 1
029400071023x   1c                   else
029500071023     c                   clear                   C01RcdNbr
029600071023e   1c                   endif
029700071023      *
029800071023      * Visualizzazione subfile SOLO SE ci sono dati
029900071023     c*** già fatto:     eval      *in30     = (S01nrr <= *zeros)
030000071023      * Eventuale attivazione del SFLEND
030100071023     c*** già fatto:     eval      *in33     = ($EoF = *on)
030200071023      *
030300071023     c                   ENDSR
030400071023
030500071023      *---------------------------------------------------------------*
030600071023      *?Inizializzazione videata S01                                 ?*
030700071023      *---------------------------------------------------------------*
030800071023     c     RollUpS01     BEGSR
030900071023      *
031000071023     c                   eval      S01nrr    = (W_SflPag * C_SflPag)
031100071023     c                   add       1             W_SflPag
031200071023     c                   eval      *in32     = *off
031300071023      *
031400071023      * Ciclo di caricamento dati nel sfl / lettura rec. successivo
031500071023do  1c                   DOW            $EoF   = *off
031600071023     c                             and  S01nrr < (W_SflPag * C_SflPag)
031700071023      *
031800071023      * - Selezione del record dell'archivio
031900071023     c                   exsr      sr_SelRec
032000071023      *
032100071023      *   e Caricamento dati nel record del subfile
032200071023if  2c                   if        $OK       = *on
032300071023     c                   exsr      CarS01
032400071023     c                   add       1             S01nrr
032500071024     c                   write     OR38S01
032600071023e   2c                   endif
032700071023      *
032800071023      * - Lettura record successivo nell'archivio
032900071024     c     I38cro        reade     FNACR100
033000071023     c                   eval      $EoF      = (%eof(FNACR11L)      OR
033100071024     c                                         (I38pocgi <> *zeros  and
033200071024     c                                          I38pocgi <> ACR1pocgi) )
033300071023      *
033400071023e   1c                   ENDDO
033500071023      *
033600071023      * Visualizzazione del SFL (se ci sono dati)
033700071023     c                   eval      *in30     = (S01nrr <= *zeros)
033800071023      *
033900071023      * Attivazione (eventuale) del SFLEND
034000071023     c                   eval      *in33     = ($EoF = *on)
034100071023      *
034200071023      * Posizionamento cursore al 1° rcd della pagina
034300071023if  1c                   if        S01nrr    > *zeros
034400071023     c     S01nrr        div       C_SflPag      wPag
034500071023     c                   mvr                     wRig
034600071023     c                   eval      C01RcdNbr = wPag * C_SflPag
034700071023     c                   if        wRig      > *zeros
034800071023     c                   eval      C01RcdNbr = C01RcdNbr + 1
034900071023     c                   else
035000071023     c                   eval      C01RcdNbr = C01RcdNbr - C_SflPag + 1
035100071023     c                   endif
035200071023x   1c                   else
035300071023     c                   clear                   C01RcdNbr
035400071023e   1c                   endif
035500071023     c                   eval      C01CsrRrn = C01RcdNbr
035600071023      *
035700071023     c                   ENDSR
035800071023
035900071023      *---------------------------------------------------------------*
036000071023      *?Selezione record in base alle selezioni                      ?*
036100071023      *---------------------------------------------------------------*
036200071023     c     sr_SelRec     BEGSR
036300071023      *
036400071023     c                   eval      $OK       = *off
036500071023      *
036600071023sel 1c                   select
036700071023      * Record annullato
036800071023w   1c                   when      ACR1atb  <> *blanks
036900071023     c                   leavesr
037000071023      * Filiale giro / senza giro
037100071023      * per selezionare tutti i giri del cliente di quella filiale
037200071024w   1c                   when           I38pocgi <> *zeros
037300071024     c                             and  I38cgi   =  *blanks
037400071024     c                             and  I38pocgi <> ACR1pocgi
037500071023     c                   leavesr
037600071023      * Filiale giro / con   giro
037700071023      * per escludere   quel giro    del cliente di quella filiale
037800071024w   1c                   when           I38pocgi <> *zeros
037900071024     c                             and  I38cgi   <> *blanks
038000071024     c                             and  I38pocgi  = ACR1pocgi
038100071024     c                             and  I38cgi    = ACR1cgi
038200071023     c                   leavesr
038300071023      * Codice giro  / senza filiale
038400071023      * per selezionare quel giro del cliente di qualsiasi filiale
038500071023      * (per ora impossibile: è previsto un solo giro x filiale)
038600071024w   1c                   when           I38pocgi  = *zeros
038700071024     c                             and  I38cgi   <> *blanks
038800071024     c                             and  I38cgi   <> ACR1cgi
038900071023     c                   leavesr
039000071023e   1c                   endsl
039100071023      *
039200071023     c                   eval      $OK       = *on
039300071023      *
039400071023     c                   ENDSR
039500071023
039600071023      *---------------------------------------------------------------*
039700071023      *?Caricamento singolo record nel SubFile S01                   ?*
039800071023      *---------------------------------------------------------------*
039900071023     c     CarS01        BEGSR
040000071023      *
040100071024     c                   clear                   OR38S01
040200071026      *
040300071026      * Abilitazione opzioni
040400071026      * - Opz. 1 = Selezione
040500071026     c                   eval      *in41     = (I38opz  = '1')
040600071023      * Campi Hidden
040700071023      * Campi Output
040800071023     c                   eval      S1Cpocgi  = ACR1pocgi
040900071023     c                   eval      S1Ccgi    = ACR1cgi
041000071023      * - Decodifica codice giro
041100071023     c                   clear                   FIDG09ds
041200071023     c                   eval      D09iop0   = '001'
041300071023     c                   eval      D09ifgs   = ACR1pocgi
041400071023     c                   eval      D09icgi   = ACR1cgi
041500071023     c                   eval      D09idat   = *date
041600071023     c                   eval      D09itug   = 'R'
041700071023     c                   movel(p)  FIDG09ds      KPJBU
041800071023     c                   call      'FIDG09R'
041900071023     c                   parm                    KPJBA
042000071023     c                   movel     KPJBU         FIDG09ds
042100071023if  1c                   if        D09oerr   = *on
042200071023     c                   eval      S1Dcgi    = *all'? '
042300071023x   1c                   else
042400071023     c                   movel     D09odes       S1Dcgi
042500071023e   1c                   endif
042600071108      *
042700071108      * Posizionamento sursore sul campo opzione del 1° rec. nel sfl
042800071108      *   (anche se visualizzazione)
042900071108     c                   if        S01nrr    = *zeros
043000071108     c                   eval      *in50     = *on
043100071108     c                   endif
043200071023      *
043300071023     c                   ENDSR
043400071023
043500071023      *---------------------------------------------------------------*
043600071023      *?Gestione tasto funzionale F12 da videata S01                 ?*
043700071023      *?F3=Fine                                                      ?*
043800071023      *---------------------------------------------------------------*
043900071023     c     F03S01        BEGSR
044000071023      *
044100071023      * Chiusura del programma
044200071024     c                   eval      O38ret    = '1'
044300071023     c                   eval      $Fine     = *on
044400071023      *
044500071023     c                   ENDSR
044600071023
044700071023      *---------------------------------------------------------------*
044800071023      *?Gestione tasto funzionale F12 da videata S01                 ?*
044900071023      *?F12=Ritorno                                                  ?*
045000071023      *---------------------------------------------------------------*
045100071023     c     F12S01        BEGSR
045200071023      *
045300071023      * Chiusura del programma
045400071024     c                   eval      O38ret    = '2'
045500071023     c                   eval      $Fine     = *on
045600071023      *
045700071023     c                   ENDSR
045800071023
045900071023      *---------------------------------------------------------------*
046000071023      *?Controllo opzioni subfile                                    ?*
046100071023      *---------------------------------------------------------------*
046200071023     c     OpzS01        BEGSR
046300071023      *
046400071024     c                   readc     OR38S01
046500071023      *
046600071024do  1c                   DOW       NOT  %eof(FIOR38D)
046700071023      *
046800071023     c                   eval      *in32     = *off
046900071023     c                   movea     *zeros        *in(50)
047000071023     c                   z-add     S01nrr        C01RcdNbr
047100071023      *
047200071023sel 2c                   SELECT
047300071023      * - Nessuna opzione
047400071023w   2c                   WHEN      S1Copz    = *zeros
047500071023      * - 1 = Selezione
047600071023w   2c                   WHEN           S1Copz  = 1
047700071024     c                             and  I38opz  = '1'
047800071024     c                   eval      O38pocgi  = S1Cpocgi
047900071024     c                   eval      O38cgi    = S1Ccgi
048000071024     c                   eval      O38ret    = *off
048100071023     c                   eval      $Fine     = *on
048200071023     c                   eval      *in90     = *on
048300071023      * - ? = Opzione NON valida
048400071023x   2c                   OTHER
048500071023     c                   seton                                        285090
048600071023     c                   eval      V1Dmsg    = $Msg(01)
048700071023e   2c                   ENDSL
048800071023      *
048900071023      * Memorizzazione nrr del sfl con la 1ª opzione
049000071023      * per riposizionarvici il cursore dopo il tasto "Invio"
049100071023if  2c***                if             S1Copz   <> *blanks
049200071023     c***                          and (SavS01csr = *zeros
049300071023     c***                           or  SavS01csr < C01RcdNbr)
049400071023     c***                eval      SavS01csr = C01RcdNbr
049500071023e   2c***                endif
049600071023      *
049700071023      * Aggiornamento sfl
049800071023sel 2c                   select
049900071023w   2c                   when      *in28
050000071023     c                   eval      *in32     = *on
050100071023     c                   z-add     C01RcdNbr     C01CsrRrn
050200071023w   2c                   when      *in90     = *on
050300071023     c                   z-add     C01RcdNbr     C01CsrRrn
050400071023     c                   clear                   S1Copz
050500071023x   2c                   other
050600071023     c                   z-add     C01RcdNbr     C01CsrRrn
050700071023     c                   clear                   S1Copz
050800071023e   2c                   endsl
050900071024     c                   UPDATE    OR38S01
051000071023if  2c                   if        *in28  OR  *in90
051100071023     c                   leave
051200071023e   2c                   endif
051300071023      *
051400071024     c                   readc     OR38S01
051500071023      *
051600071023e   1c                   ENDDO
051700071023      *
051800071023      * Riposiziono il cursore sul record contenente la prima opzione
051900071023      *  (se non sono stati rilevati errori)
052000071023if  1c***                if        NOT *in28  and  NOT *in90
052100071023     c***                          and SavS01csr  > *zeros
052200071023     c***                z-add     SavS01csr     C01CsrRrn
052300071023e   1c***                endif
052400071023      *
052500071023     c                   ENDSR
052600071023
052700071023** - $MSG -------------------------------------------------------------------*
052800071023Opzione errata                                                                  1
052900071023Codice cliente ritiro obbligatorio                                              2
053000071023Codice giro di ritiro NON in vigore  --  Enter per forzare                      3
