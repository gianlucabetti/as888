000100060403      *---------------------------------------------------------------*
000200060403      * Gestione tabella "CLI" = Abilitazioni a Clienti               *
000300060403      *---------------------------------------------------------------*
000400060403
000500060403     h decedit('0,') datedit(*dmy/)
000600110809
000700110809      *?  ATTENZIONE!!  ?
000800110809      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000900110809      *?    'Interrogazione abilitazioni clienti'              ?
001000110809      *?    In caso di aggiunta/modifica campi alla tabella    ?
001100110809      *?    verificare se sono validi per la visualizzazione   ?
001200110809      *?    da TA61                                            ?
001300120516
001400121016      *?    In caso di aggiunta/modifica campi alla tabella    ?
001500121016      *?    aggiornare anche l'HELP del video (TNTB53H)        ?
001600121016
001700120516      *?  - 16/05/2012 aggiunti campo §CLIFTS
001800120516      *?    questo campo non lo utilizzo in TA61 in quanto lo abbiamo
001900120516      *?    legato ai dati di fatturazione (F17 da TA60)
002000060403
002100060403      *---------------------------------------------------------------*
002200060403      *   A R C H I V I                                               *
002300060403      *---------------------------------------------------------------*
002400060403      *
002500071207     fTabel00f  if   e           k disk
002600120919     Fazorg01L  IF   E           K DISK
002700170214     FTFNTC01L  uF a E           K DISK
002800091125     fTNTBE01L  uf a e           k disk
002900060403      *
003000060403     fTNTB53D   cf   e             workstn
003100060403
003200060403      *---------------------------------------------------------------*
003300060403      *   C O S T A N T I                                             *
003400060403      *---------------------------------------------------------------*
003500060403      *
003600060403     d DigitN          c                   const('0123456789')
003700060403
003800060403      *---------------------------------------------------------------*
003900060403      *   S C H I E R E                                               *
004000060403      *---------------------------------------------------------------*
004100060403      *
004200060403     d $Opz            s             15    dim( 6) ctdata perrcd(1)             Decodifica OPZ
004300170214     d $Msg            s             78    dim(14) ctdata perrcd(1)             Messaggi video
004400060403
004500060403      *---------------------------------------------------------------*
004600060403      *   S T R U T T U R E   D A T I                                 *
004700060403      *---------------------------------------------------------------*
004800060403      *
004900060403      * Parametri
005000060403     d KPJBA         e ds
005100060403      *
005200060403      * Passaggio Parametri al pgm TIBS02R
005300060403     d TIBS02ds      e ds                  inz
005400060403     d  T02mod       e                     inz('R')
005500060403     d  T02cod       e                     inz('CLI')
005600060403      *
005700130724      * Tabella CLI = Clienti ABILITAZIONI: copia LDV/etichetta DPD
005800060403     d dCLI          e ds                  inz
005900071207
006000071207     d ds3c          e ds                  inz
006100131002     d ds5e          e ds                  inz
006200060403      *
006300060403      * Tracciato record file TNTBE00F
006400060403     d TNTBEds       e ds                  extname(TNTBE00F) inz
006500060403     d xTNTBEds      e ds                  extname(TNTBE00F) inz
006600060403     d                                     prefix(TBX:3)
006700060403      *
006800060403     d TIBS34ds      e ds                  inz
006900060403     d dDatiUte      e ds                  inz
007000060403     d AZUTEds       e ds                  extname(AZUTE00F) inz
007100060403      *
007200060403      * Decodifica anagrafica clienti
007300060403     d TIBS69ds      e ds                  inz
007400060403     d ds_CNACO      e ds                  extname(CNACO00F) inz
007500060403     d ds_CNIND      e ds                  extname(CNIND00F) inz
007600060403     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
007700060403     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
007800151105      *
007900060403     d WLBdat          ds                  inz
008000060403     d  G02dat                 1      8  0 inz
008100060403     d  G02inv                 9     16  0 inz
008200060403     d  G02err                17     17    inz('3')
008300060403     d  G02tgi                18     22  0 inz
008400060403      *
008500060403     d Status         sds
008600060403     d  VTCpgm           *proc
008700060403
008800060403      *---------------------------------------------------------------*
008900060403      *   V A R I A B I L I                                           *
009000060403      *---------------------------------------------------------------*
009100060403      *
009200060403     d $Fine           s              1a   inz(*off)
009300060403     d $InzV1          s              1a   inz(*on)
009400060403     d $InzV2          s              1a   inz(*off)
009500060403     d $InzW1          s              1a   inz(*off)
009600060403     d $TipVid         s              1a   inz('1')
009700060403     d $TIBS02         s              1a   inz(*off)
009800060403     d*** Win             s             99a   inz(*zeros)
009900060403     d wTasto          s              2a   inz(*zeros)
010000060403     d wDate           s               d   datfmt(*iso) inz(*loval)
010100071207     d w_diskb         s              1a
010200071207     d w_forzb         s              1a
010300071207     d codut           s              1  0 inz(1)
010400071207     d cod             s                   like(tblcod)
010500071207     d key             s                   like(tblkey)
010600131002     d §key            s                   like(tblkey)
010700131002     d werr_tsp        s              1
010800060403
010900170214       // - TFNTC01L
011000170214     d k04tFNTC01    e ds                  extname(TFNTC01L : *key)
011100170214     d                                     prefix(k_)   inz
011200170214
011300060403      *---------------------------------------------------------------*
011400060403      *   M A I N   L I N E                                           *
011500060403      *---------------------------------------------------------------*
011600060403      *  Riepilogo indicatori utilizzati:                             *
011700060403      *  --------------------------------                             *
011800060403      *  01 - Record inesistente (inserimento)                        *
011900060403      *  02 - Record esistente   (modifica)                           *
012000060403      *  04 - Record annullato   (ripristino)                         *
012100060403      *  10 - Comodo                                                  *
012200060403      *  22 - Errori in scrittura record (WRITE)                      *
012300060403      *  28 - Emissione messaggio di errore a video                   *
012400060403      *  50 - ERR: Codice cliente                                     *
012500060403      *  51 - ERR: Ragione sociale cliente                            *
012600060403      *  52 - ERR: flag Stampa copia LdV                              *
012700060403      *  53 - ERR: flag Stampa etichetta DPD in partenza              *
012800130724      *                                                               *
012900060403      *  55 - ERR: flag Obbligo parzializz. x LNA in conferma bolle   *
013000060525      *  56 - ERR: flag Stampa LdV personalizzata - FNLSB5R           *
013100071218      *  59 - ERR: flag mantiene DCR in conf.bolle                    *
013200131001      *  63 - ERR:                                                    *
013300131001      *  64 - ERR:                                                    *
013400131001      *  65 - ERR:                                                    *
013500060403      *  90 - Riemissione videata                                     *
013600060403      *---------------------------------------------------------------*
013700060403      *
013800060403      * Operazioni iniziali
013900060403     c                   exsr      RutInz
014000060403      *
014100060403      * Gestione video
014200060403     c                   dow       $Fine   = *off
014300060403     c     $TipVid       caseq     '1'           GesV1
014400060403     c     $TipVid       caseq     '2'           GesV2
014500170210     c     $TipVid       caseq     '3'           GesV3
014600060403     c     $TipVid       caseq     'A'           GesW1
014700060403     c                   endcs
014800060403     c                   enddo
014900060403      *
015000060403      * Fine
015100060403     c                   if        $TIBS02 = *on
015200060403     c                   clear                   TIBS02ds
015300060403     c                   movel     'C'           T02tla
015400060403     c                   call      'TIBS02R'
015500060403     c                   parm                    KPJBA
015600060403     c                   parm                    TIBS02ds
015700060403     c                   endif
015800060403      *
015900060403     c                   movel     *on           *inLR
016000060403
016100060403      *---------------------------------------------------------------*
016200060403      * RutInz - Operazioni Iniziali                                  *
016300060403      *---------------------------------------------------------------*
016400060403     c     RutInz        BEGSR
016500060403      *
016600060403      * Ricezione parametri
016700060403     c     *entry        plist
016800060403     c                   parm                    KPJBA
016900060403      *
017000060403      * Definizioni chiavi di accesso
017100060403     c     K05TBE01      klist                                                  *tntbe01l
017200060403     c                   kfld                    TBEcod                         -tabella
017300060403     c                   kfld                    TBEke1                         -chiave uno
017400060403     c                   kfld                    TBEke2                         -chiave due
017500060403     c                   kfld                    TBElin                         -lingua
017600060403     c                   kfld                    TBEsif                         -s.informativo
017700071207     C*
017800071207     C     KTAB2         KLIST
017900071207     C                   KFLD                    codut
018000071207     C                   KFLD                    cod
018100071207     C                   KFLD                    key
018200060403      *
018300060403      * Reperisco la data odierna
018400060403     c                   movel     *date         Wdate
018500060403      *
018600060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
018700060403     c     *dtaara       define    §azute        AZUTEds
018800060403     c     *dtaara       define    §datiute      dDatiUte
018900060403      *
019000060403     c                   clear                   AZUTEds
019100060403     c                   clear                   dDatiUte
019200060403     c                   clear                   TIBS34ds
019300060403     c                   in(E)     *dtaara
019400060403if  1c                   if        %Error  or  RSUT = *blanks
019500060403     c                   call      'TIBS34R'
019600060403     c                   parm                    TIBS34ds
019700060403     c                   in        *dtaara
019800060403e   1c                   endif
019900060403      *-- Verifica errori e autorità profilo
020000060403sel 1c                   SELECT
020100060403      *-- controllo se ho errori nei dati utente
020200060403      *--   nel qual caso NON risulta un profilo abilitato
020300060403w   1c                   WHEN      DUTerr  = 'E'
020400060403     c                   eval      $Fine   = *on
020500060403      *
020600060403      *-- CONTROLLO AUTORITA'
020700060403      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
020800060403      *-- se il chiamante non richiede autorità specifica verificare
020900060403      *--   quella del profilo
021000060403      *-- se il chiamante richiede autorità specifica verificarla,
021100060403      *--  se è blank verificare quella del profilo
021200060403      *
021300060403      * se UTEAUT = *BLANK non siamo in sede
021400060403w   1c                   WHEN      UTEaut  = *blank
021500060403      *
021600060403x   1c                   OTHER
021700060403      *
021800060403e   1c                   ENDSL
021900060403      *
022000060403      * Aggancio dati generali della tabella in esame
022100060403     c                   clear                   TBEcod
022200060403     c                   move      *zeros        TBEke1
022300060403     c                   move      T02cod        TBEke1
022400060403     c                   clear                   TBEke2
022500060403     c                   clear                   TBElin
022600060403     c                   movel     KNSIF         TBEsif
022700060403     c     K05TBE01      chain     TNTBE01L
022800060403     c                   if        not %found(TNTBE01L)
022900060403     c                   clear                   TBEsif
023000060403     c     K05TBE01      chain     TNTBE01L
023100060403     c                   endif
023200060403     c                   if        %found(TNTBE01L)
023300060403     c                   movel     TNTBEds       xTNTBEds
023400060403     c                   else
023500060403     c                   clear                   xTNTBEds
023600060403     c                   endif
023700060403      *
023800060403     c                   clear                   TB53V1
023900120925     C*
024000120925     C* ctrl i dati presenti in KPJBU
024100120925     C* se da pos. 1 x 7 ci sono solo caratteri numerici, allora è arrivato in ingresso il codice
024200120925     C* cliente, quindi lo testo e se corretto passo subito alla seconda videata
024300120925x   1c                   IF        KPJBU <> *blank
024400120925     C* faccio in modo che anche se devo andare in videata 1 per errore rimanga il codice ricevuto
024500120925     c                   clear                   TB53V1
024600120925if  1c                   EVAL      $InzV1  = *off
024700120925x   1c                   EVAL      V1CKsc = %subst(KPJBU : 1 : 7)
024800120925x   1c                   TESTN                   V1CKsc               10
024900120925     C                   IF        *in10 = *on                                  *E'NUMERO
025000120925     C* e l'ultimo carattere non è il segno ma una cifra anch'essa
025100120925     C                             and %subst(V1CKsc : 7 : 1) >= '0'
025200120925     c                   exsr      CtrV1
025300120925     C* se non rilevati errori
025400120925if  2c                   if        *in90 = *off
025500120925     C* Passo alla videata di dettaglio
025600120925     c                   eval      $InzV2  = *on
025700120925     c                   eval      $TipVid = '2'
025800120925     C                   ENDIF
025900120925     C                   ENDIF
026000120925     C                   ENDIF
026100060403      *
026200060403     c                   ENDSR
026300060403
026400060403      *---------------------------------------------------------------*
026500060403      * GESV1  - Gestione videata selezione codice tabella            *
026600060403      *---------------------------------------------------------------*
026700060403     c     GesV1         BEGSR
026800060403      *
026900060403      * Inizializzazione videata
027000060403if  1c                   if        $InzV1  = *on
027100060403     c                   exsr      InzV1
027200060403     c                   movel     *off          $InzV1
027300060403e   1c                   endif
027400060403      *
027500060403      * Scrivo la testata
027600060403     c                   write     TB53T1
027700060403      *
027800060403      * Se esistono errori sulla videata
027900060403      * emetto la write del formato a indicatori spenti per vedere
028000060403      * le eventuali decodifiche
028100060403      *?...se ci fossero campi con tanto di decodifica in 'sta videata?
028200060403if  1c***                if        *in90
028300060403     c***                movea     *in           Win
028400060403     c***                movea     *zeros        *in(50)
028500060403     c***                write     TB53V1
028600060403     c***                movea     Win           *in
028700060403e   1c***                endif
028800060403      *
028900060403     c                   exfmt     TB53V1
029000060403     c                   setoff                                       28  90
029100060403     c                   clear                   V1Dmsg
029200060403      *
029300060403sel 1c                   select
029400060403      * F3=Fine
029500060403w   1c                   when      *inKC
029600060403     c                   exsr      F03V1
029700060403     c                   leavesr
029800060403      *
029900060403      * Controllo dati immessi a video
030000060403x   1c                   other
030100060403     c                   exsr      CtrV1
030200060403      * - Rilevati Errori
030300060403if  2c                   if        *in90
030400060403     c                   leavesr
030500060403e   2c                   endif
030600060403      * - Passaggio alla videata di dettaglio
030700060403     c                   eval      $InzV2  = *on
030800060403     c                   eval      $TipVid = '2'
030900060403      *
031000060403e   1c                   endsl
031100060403      *
031200060403     c                   ENDSR
031300060403
031400060403      *---------------------------------------------------------------*
031500060403      * INZV1  - Inizializzazione prima videata (V1)                  *
031600060403      *---------------------------------------------------------------*
031700060403     c     InzV1         BEGSR
031800060403      *
031900060403     c                   clear                   TB53V1
032000060403     *** *
032100060403     ***c                   movea     *zeros        *in(50)
032200060403     ***c                   movea     '00000'       *in(01)
032300060403      *
032400060403      * Imposto il campo "codice cliente" per l'interrogazione
032500060403     c                   move      '?'           V1Cksc
032600060403      *
032700060403     c                   ENDSR
032800060403
032900060403      *---------------------------------------------------------------*
033000060403      * CTRV1  - Controllo e decodifica prima videata                 *
033100060403      *---------------------------------------------------------------*
033200060403     c     CtrV1         BEGSR
033300060403      *
033400060403     c                   movea     *zeros        *in(50)
033500060403      *
033600060403      * Codice cliente
033700060403      * - Ricerca:
033800060403     c     '?'           scan      V1Cksc                                 10
033900060403if  1c                   if        *in10
034000060403     c                   eval      $TIBS02 = *on
034100060403     c                   clear                   V1Cksc
034200060403     c                   seton                                          5090
034300060403     c                   reset                   TIBS02ds
034400060403     c                   movel     KNSIF         T02sif
034500060403if  2c                   if            DUTlpo             <> 'S'
034600060403     c                             and %subst(DUTute:1:3) <> 'EDP'
034700060403     c                   movel     DUTpou        T02ke1
034800060403e   2c                   endif
034900060403     c                   call      'TIBS02R'
035000060403     c                   parm                    KPJBA
035100060403     c                   parm                    TIBS02ds
035200060403if  2c                   if        T02err  = *blanks
035300060403     c                   movel     T02uni        dCLI
035400060403     c                   movel     T02ke1        V1Cksc
035500060403x   2c                   else
035600060403     c                   seton                                        285090
035700060403     c                   movel     T02msg        V1Dmsg
035800060403     c                   leavesr
035900060403     c                   endif
036000060403e   1c                   endif
036100060403      * - Controllo:
036200060403      * - - effettiva immissione
036300060403if  1c                   if           V1Cksc = *blanks
036400060403     c                             or V1Cksc = *zeros
036500060403     c                   seton                                        285090
036600060403     c                   eval      V1Dmsg  = $Msg(1)
036700060403     c                   leavesr
036800060403e   1c                   endif
036900060403      * - - numericità
037000060403     c     DigitN        check     V1Cksc                                 10
037100060403if  1c                   if        *in10   =  *on
037200060403     c                   seton                                        285090
037300060403     c                   eval      V1Dmsg  = $Msg(2)
037400060403     c                   leavesr
037500060403e   1c                   endif
037600060403      * - - decodifica
037700060403     c                   exsr      DecodCLI
037800060403      * - - "appartenenza" al P.O. dell'utente
037900060403     c                   if            %subst(V1Cksc:1:3) <> %char(DUTpou)
038000060403     c                             and DUTlpo             <> 'S'
038100060403     c                             and %subst(DUTute:1:3) <> 'EDP'
038200060403     c                   seton                                        285090
038300060403     c                   eval      V1Dmsg  = $Msg(2)
038400060403     c                   leavesr
038500060403     c                   endif
038600060403      *
038700060403     c                   ENDSR
038800060403
038900060403      *---------------------------------------------------------------*
039000060403      * DecodCLI   * Decodifica codice cliente                        *
039100060403      *---------------------------------------------------------------*
039200060403     c     DecodCLI      BEGSR
039300060403      *
039400060403     c                   clear                   TIBS69ds
039500060403     c                   clear                   ds_CNACO
039600060403     c                   clear                   ds_CNIND
039700060403     c                   clear                   ds_CNCLP
039800060403     c                   clear                   ds_FNCLS
039900060403     c                   move      V1Cksc        I69kac
040000060403     c                   call      'TIBS69R'
040100060403     c                   parm                    TIBS69ds
040200060403     c                   parm                    ds_CNACO
040300060403     c                   parm                    ds_CNIND
040400060403     c                   parm                    ds_CNCLP
040500060403     c                   parm                    ds_FNCLS
040600060403      *
040700060403     c                   if        O69err  = *blanks
040800060403     c                   movel     ACOrag        V2Dksc
040900060403     c                   else
041000060403     c                   seton                                        285090
041100060403     c                   eval      V1Dmsg  = $Msg(2)
041200060403     C                   endif
041300060403      *
041400060403     c                   ENDSR
041500060403
041600060403      *---------------------------------------------------------------*
041700060403      * F03V1  - Tasto funzionale F03 -> Fine programma               *
041800060403      *---------------------------------------------------------------*
041900060403     c     F03V1         BEGSR
042000060403      *
042100060403     c                   movel     *on           $Fine                          fine pgm
042200060403      *
042300060403     c                   ENDSR
042400060403
042500060403      *---------------------------------------------------------------*
042600060403      * GESV2  - Gestione videata dettaglio dati                      *
042700060403      *---------------------------------------------------------------*
042800060403     c     GesV2         BEGSR
042900060403      *
043000060403      * Inizializzazione videata
043100060403     c                   if        $InzV2  = *on
043200060403     c                   exsr      InzV2
043300060403     c                   move      *off          $InzV2
043400060403     c                   endif
043500060403      *
043600060403      * Scrivo la testata
043700060403     c                   write     TB53T1
043800060403      *
043900060403      * Se esistono errori sulla videata
044000060403      * emetto la write del formato a indicatori spenti per vedere
044100060403      * le eventuali decodifiche
044200060403      *?...se l'unica decodifica NON fosse già stata visualizzata?
044300060403      *  ?in fase di prima emissione?
044400060403if  1c***                if        *in90
044500060403     c***                movea     *in           Win
044600060403     c***                movea     *zeros        *in(50)
044700060403     c***                write     TB53V2
044800060403     c***                movea     Win           *in
044900060403e   1c***                endif
045000060403      *
045100060403if  1***c                   if        *in05
045200060403     ***c                   write     TB53V2
045300060403     ***c                   exfmt     PROTECT
045400060403x   1***c                   else
045500060403     c                   exfmt     TB53V2
045600060403e   1***c                   endif
045700060403     c                   setoff                                       28  90
045800060403     c                   clear                   V1Dmsg
045900060403     c                   reset                   wTasto
046000060403      *
046100060403sel 1c                   select
046200170210      * RollUp
046300170210w   1c                   when      *in31
046400170210     c                   eval      $TipVid = '3'
046500060403      * F03=Fine
046600060403w   1c                   when      *inKC
046700060403     c                   exsr      F03V1
046800060403     c***                leavesr
046900060403      * F12=Ritorno
047000060403w   1c                   when      *inKL
047100060403     c                   exsr      F12V2
047200060403     c***                leavesr
047300060403      *
047400060403x   1c                   other
047500060403      * Controllo dati immessi a video
047600060403      * (non si fanno se richisto l'annullamento)
047700060403if  2c                   if        not *inKQ
047800060403     c                   exsr      CtrV2
047900170214     C* se non ci sono errori nella prima parte della videata, controllo la seconda
048000170214w   2c                   if        not *in90
048100170210     c                   exsr      CtrV3
048200170214     C* se ci sono errori nella seconda parte della videata, emetto un errore generico
048300170214w   2c                   if        *in90
048400170214     c                   seton                                        28
048500170214     c                   eval      V1Dmsg  = $Msg(12)
048600170214     c                   endif
048700170214     c                   endif
048800060403e   2c                   endif
048900060403      * Aggiornamento se non ci sono errori
049000060403sel 2c                   select
049100060403      * - Rilevati errori
049200060403w   2c                   when      *in90
049300060403      * - F5=Ripristino
049400060403w   2c                   when      *inKE
049500060403     c                   eval      wTasto  = '05'
049600060403      * - F6=Conferma
049700060403w   2c                   when      *inKF
049800060403     c                   eval      wTasto  = '06'
049900060403      * - F16=Annullamento
050000060403w   2c                   when      *inKQ
050100060403     c                   eval      wTasto  = '16'
050200060403e   2c                   endsl
050300060403     ***c                   exsr      AggTBE
050400060403if  2c                   if        wTasto  > *zeros
050500060403     c                   eval      $InzW1  = *on
050600060403     c                   eval      $TipVid = 'A'
050700060403e   2c                   endif
050800060403e   1c                   endsl
050900060403      *
051000060403     c                   ENDSR
051100170210
051200170210      *---------------------------------------------------------------*
051300170210      * GESV3  - Gestione altra videata dettaglio dati                      *
051400170210      *---------------------------------------------------------------*
051500170210     c     GesV3         BEGSR
051600170210      *
051700170210      * Scrivo la testata
051800170210     c                   write     TB53T1
051900170210      *
052000170210      * Se esistono errori sulla videata
052100170210      * emetto la write del formato a indicatori spenti per vedere
052200170210      * le eventuali decodifiche
052300170210      *?...se l'unica decodifica NON fosse già stata visualizzata?
052400170210      *  ?in fase di prima emissione?
052500170210if  1c***                if        *in90
052600170210     c***                movea     *in           Win
052700170210     c***                movea     *zeros        *in(50)
052800170210     c***                write     TB53V3
052900170210     c***                movea     Win           *in
053000170210e   1c***                endif
053100170210      *
053200170210if  1***c                   if        *in05
053300170210     ***c                   write     TB53V2
053400170210     ***c                   exfmt     PROTECT
053500170210x   1***c                   else
053600170210     c                   exfmt     TB53V3
053700170210e   1***c                   endif
053800170210     c                   setoff                                       28  90
053900170210     c                   clear                   V1Dmsg
054000170210     c                   reset                   wTasto
054100170210      *
054200170210sel 1c                   select
054300170210      * RollDown
054400170210w   1c                   when      *in32
054500170210     c                   eval      $TipVid = '2'
054600170210      * F03=Fine
054700170210w   1c                   when      *inKC
054800170210     c                   exsr      F03V1
054900170210     c***                leavesr
055000170210      * F12=Ritorno
055100170210w   1c                   when      *inKL
055200170210     c                   exsr      F12V2
055300170210     c***                leavesr
055400170210      *
055500170210x   1c                   other
055600170210      * Controllo dati immessi a video
055700170210      * (non si fanno se richisto l'annullamento)
055800170210if  2c                   if        not *inKQ
055900170210     c                   exsr      CtrV3
056000170214     C* se non ci sono errori nella seconda parte della videata, controllo la prima
056100170214w   2c                   if        not *in90
056200170214     c                   exsr      CtrV2
056300170214     C* se ci sono errori nella prima parte della videata, emetto un errore generico
056400170214w   2c                   if        *in90
056500170214     c                   seton                                        28
056600170214     c                   eval      V1Dmsg  = $Msg(13)
056700170214     c                   endif
056800170214     c                   endif
056900170210e   2c                   endif
057000170210      * Aggiornamento se non ci sono errori
057100170210sel 2c                   select
057200170210      * - Rilevati errori
057300170210w   2c                   when      *in90
057400170210      * - F5=Ripristino
057500170210w   2c                   when      *inKE
057600170210     c                   eval      wTasto  = '05'
057700170210      * - F6=Conferma
057800170210w   2c                   when      *inKF
057900170210     c                   eval      wTasto  = '06'
058000170210      * - F16=Annullamento
058100170210w   2c                   when      *inKQ
058200170210     c                   eval      wTasto  = '16'
058300170210e   2c                   endsl
058400170210     ***c                   exsr      AggTBE
058500170210if  2c                   if        wTasto  > *zeros
058600170210     c                   eval      $InzW1  = *on
058700170210     c                   eval      $TipVid = 'A'
058800170210e   2c                   endif
058900170210e   1c                   endsl
059000170210      *
059100170210     c                   ENDSR
059200060403
059300060403      *---------------------------------------------------------------*
059400060403      * INZV2  - Inizializzazione seconda videata (V2)                *
059500060403      *---------------------------------------------------------------*
059600060403     c     InzV2         BEGSR
059700060403      *
059800060525     c                   clear                   TB53V2
059900060403      *
060000060403      * reimposto il campo chiave (qui di solo output)
060100060403     c                   movel     V1Cksc        V2Cksc
060200060403     c                   movel     ACOrag        V2Dksc
060300071207      * verifico se cliente diskB e pulisco relativo campo di forzatura
060400071207     c                   clear                   w_diskb
060500071207     c                   clear                   w_forzb
060600071207     C                   MOVEL     '3C'          COD
060700071207     C                   MOVEL(P)  v2cksc        KEY
060800071207     C                   CLEAR                   DS3C
060900071207     C     ktab2         chain     tabel
061000071214     c                   if        %found(tabel00f) and tblflg=' '
061100071207     C                   movel     tbluni        ds3c
061200071207     c                   endif
061300071207     c                   if        §3cnrs=0
061400071207     c                   eval      w_diskb='N'
061500071207     c                   endif
061600071207     c
061700060403      *
061800060403      * Aggancio la tabella, se trovo il codice sono in modifica
061900060403      * o ripristino (se record annullato), altrimenti in immissione
062000060403     c                   clear                   dCLI
062100060403     c                   exsr      ChnTBE
062200060403      *
062300060403sel 1c                   SELECT
062400060403      * IMMISSIONE
062500060403w   1c                   WHEN      NOT %found(TNTBE01L)
062600060403     c                   eval      *in01   = *on
062700060403     c                   eval      T1opz   = $Opz(01)
062800060403      * MODIFICA
062900060403w   1c                   WHEN          %found(TNTBE01L)
063000060403     c                             and TBEatb  = *blanks
063100060403     c                   eval      *in02   = *on
063200060403     c                   eval      T1opz   = $Opz(02)
063300060403      * RIPRISTINO
063400060403w   1c                   WHEN          %found(TNTBE01L)
063500060403     c                             and TBEatb <> *blanks
063600060403     c                   eval      *in04   = *on
063700060403     c                   eval      T1opz   = $Opz(06)
063800060403e   1c                   ENDSL
063900060403      *
064000060403if  1c                   if        %found(TNTBE01L)
064100060403     c                   movel     TBEuni        dCLI
064200060403     c                   eval      V2Crag  = §CLIrag
064300060403     c                   eval      V2Cldv  = §CLIldv
064400060403     c                   eval      V2Cdpd  = §CLIdpd
064500120919     c                   MOVEL     §CLIFI1       V2ClF1
064600120919     c                   MOVEL     §CLIFI2       V2ClF2
064700120919     c                   MOVEL     §CLIFI3       V2ClF3
064800060403     c                   eval      V2Clna  = §CLIlna
064900060525     c                   eval      V2Clvp  = §CLIlvp
065000071207     c                   eval      V2Cdkb  = §CLIdkb
065100071218     c                   eval      V2Cdcr  = §CLIdcr
065200110805     c***                eval      V2Cidt  = §CLIidt
065300110921     c                   eval      V2Cban  = §CLIban
065400091125     c                   eval      V2Cgcp  = §CLIgcp
065500100324     c                   eval      V2Cbrd  = §CLIbrd
065600120516     c                   eval      V2Cfts  = §CLIfts
065700120618     c                   eval      V2Cemd  = §CLIemd
065800130320     c                   eval      V2Cpdfp = §CLIpdfp
065900151105     c                   eval      V2Cemdp = §CLIemdp
066000160120     c                   eval      V2Cemsms= §CLIemsms
066100131002     c* peso e volume medio: verifico se i campi contengono numeri
066200131002     c                   clear                   v2cpsm
066300131002     c                   clear                   v2cvlm
066400131002     c                   testn                   §clipsm              10
066500131002     c                   if        *in10 and %subst(§clipsm:7:1)>='0'
066600131002     c                   move      §clipsm       v2cpsm
066700131002     c                   endif
066800131002     c                   testn                   §clivlm              10
066900131002     c                   if        *in10 and %subst(§clivlm:5:1)>='0'
067000131002     c                   move      §clivlm       v2cvlm
067100131002     c                   endif
067200131001     c                   eval      V2Ctspf = §CLItspf
067300131002     c                   if        v2ctspf<>*blanks
067400131002     c                   exsr      ctrtsp
067500131002     c                   endif
067600131001     c                   eval      V2Csup  = §CLIsup
067700161207      *
067800161207      * imposto inizializzazioni per inserimento
067900161207e   1c                   else
068000161207     c                   EVAL      V2CRag = V2Dksc
068100161207e   1c                   endif
068200161207      *
068300170214     c                   eval      V2MITOR  = §CLIMITOR
068400170214     c                   eval      V2TFNTC  = §CLITFNTC
068500170214     c                   eval      V2RNT1 = *blank
068600170214     c                   eval      V2RNT2 = *blank
068700170214     c* vedo se c'era già qualche nota
068800170214          k_NTCAPL =  'C';
068900170214          k_NTCNK1 =  '0151' + %trim(TBEKE1);
069000170214          k_NTCNK2 =  *BLANK;
069100170214          k_NTCTNT =  'AM';
069200170214          chain(n)  %kds(k04tFNTC01)  TFNTC;
069300170214          if %found();
069400170214     c                   eval      V2RNT1 = NTCRNT
069500170214          endif;
069600170214          k_NTCAPL =  'C';
069700170214          k_NTCNK1 =  '0151' + %trim(TBEKE1);
069800170214          k_NTCNK2 =  *BLANK;
069900170214          k_NTCTNT =  'AS';
070000170214          chain(n)  %kds(k04tFNTC01)  TFNTC;
070100170214          if %found();
070200170214     c                   eval      V2RNT2 = NTCRNT
070300170214          endif;
070400170214     c* se c'era qualcosa nelle note, imposto a S il flag forzatora
070500170214     c                   if        V2RNT1 <> *blank or
070600170214     c                             V2RNT2 <> *blank
070700170214     c                   eval      V2TFNTC = 'S'
070800170214     c                   endif
070900170214     c                   eval      V2NOIMTT = §CLINOIMTT
071000170905     c                   eval      V2blkal  = §CLIblkalr
071100170214      *
071200060403     c                   ENDSR
071300060403
071400060403      *---------------------------------------------------------------*
071500060403      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
071600060403      *---------------------------------------------------------------*
071700060403     c     F12V2         BEGSR
071800060403      *
071900060403     c                   eval      $TipVid = '1'
072000060403      *
072100060403     c                   ENDSR
072200060403
072300060403      *---------------------------------------------------------------*
072400060403      * CTRV2  - Controllo e decodifica seconda videata               *
072500060403      *---------------------------------------------------------------*
072600060403     c     CtrV2         BEGSR
072700060403      *
072800060403     c                   movea     *zeros        *in(50)
072900060403      *
073000060403sel 1c                   SELECT
073100060403      *
073200060403      * Ragione sociale obbligatoria
073300060403w   1c                   WHEN      V2Crag  = *blanks
073400060403     c                   seton                                        285190
073500060403     c                   eval      V1Dmsg  = $Msg(3)
073600060403     c***                leavesr
073700060403      *
073800060403      * Flag errato "Stampa copia LdV"
073900060403w   1c                   WHEN          V2Cldv <> *blanks
074000060403     c                             and V2Cldv <> 'S'
074100060403     c                   seton                                        285290
074200060403     c                   eval      V1Dmsg  = $Msg(4)
074300060403     c***                leavesr
074400060403      *
074500060403      * Flag errato "Stampa etichetta DPD in partenza"
074600060403w   1c                   WHEN          V2Cdpd <> *blanks
074700060403     c                             and V2Cdpd <> 'S'
074800060403     c                   seton                                        285390
074900060403     c                   eval      V1Dmsg  = $Msg(4)
075000060403     c***                leavesr
075100060403      *
075200060403      * Flag errato "Obbligo parzializzaz. per LNA in conferma bolle"
075300060403w   1c                   WHEN          V2Clna <> *blanks
075400060403     c                             and V2Clna <> 'S'
075500060403     c                   seton                                        285590
075600060403     c                   eval      V1Dmsg  = $Msg(4)
075700060525      *
075800060525      * Flag errato "Stampa LdV personalizzata - FNLSB5R"
075900060525      * (flag SOLO indicativo - non testato da alcun pgm.)
076000060525w   1c                   WHEN          V2Clvp <> *blanks
076100060525     c                             and V2Clvp <> 'S'
076200060525     c                   seton                                        285690
076300060525     c                   eval      V1Dmsg  = $Msg(4)
076400060525     c***                leavesr
076500071207      * Flag errato "manutenzione lna su bolla non partita cliente DKB"
076600071207w   1c                   WHEN          V2Cdkb <> *blanks
076700071207     c                             and V2Cdkb <> 'S'
076800071207     c                   seton                                        285890
076900071207     c                   eval      V1Dmsg  = $Msg(4)
077000071218      * Flag errato "mantiene Data Consegna Richiesta in conferma bolle"
077100071218w   1c                   WHEN          V2Cdcr <> *blanks
077200071218     c                             and V2Cdcr <> 'S'
077300071218     c                   seton                                        285990
077400071218     c                   eval      V1Dmsg  = $Msg(4)
077500131001      * Instradamento merce personalizzato
077600131001      * peso/volume: o tutti e due o nessuno
077700131001     c                   when           v2cpsm>0 or v2cvlm>0
077800131001     c                   if        v2cpsm=0 or v2cvlm=0
077900131001     c                   seton                                        286390
078000131001     c                   eval      V1Dmsg  = $Msg(6)
078100131001     c                   else
078200131001     c* tipo servizio forzato: controllo e decodifico, se immesso
078300131002     c                   if        v2ctspf<>*blanks
078400131002     c                   exsr      ctrtsp
078500131002     c                   select
078600131002     c                   when      werr_tsp='1'
078700131001     c                   seton                                        286490
078800131001     c                   eval      V1Dmsg  = $Msg(9)
078900131002     c                   when      werr_tsp='R'
079000131002     c                   seton                                        90
079100131002     c                   endsl
079200131001     c                   endif
079300131001     c                   endif
079400131001     c* errore se immesso il tipo servizio forzato ma non immesso peso/vlm medio
079500131002     c                   when      v2ctspf<>*blanks and v2cpsm=0
079600131001     c                   seton                                        286490
079700131002     c                   eval      V1Dmsg  = $Msg(10)
079800131001     c* errore se immesso l'utilizzo del supermercato e non immesso peso/vlm medio
079900131001     c                   when      v2csup<>*blanks and v2cpsm=0
080000131001     c                   seton                                        286590
080100131002     c                   eval      V1Dmsg  = $Msg(10)
080200060403      *
080300060403      * Nessuna abilitazione effettuata
080400060403w   1c                   WHEN          V2Cldv  = *blanks
080500060403     c                             and V2Cdpd  = *blanks
080600060403     c                             and V2Clna  = *blanks
080700060525     c                             and V2Clvp  = *blanks
080800071207     c                             and V2Cdkb  = *blanks
080900071218     c                             and V2Cdcr  = *blanks
081000110805     c****                         and V2Cidt  = *blanks
081100110921     c                             and V2Cban  = *blanks
081200091125     c                             and V2Cgcp  = *blanks
081300100324     c                             and V2Cbrd  = *blanks
081400120516     c                             and V2Cfts  = *blanks
081500120618     c                             and V2Cemd  = *blanks
081600120919     c                             and V2ClF1  = 0
081700120919     c                             and V2ClF2  = 0
081800120919     c                             and V2ClF3  = 0
081900130320     c                             and V2Cpdfp = *blanks
082000131001     c                             and V2Cpsm  = *zeros
082100160120     c                             and V2Cemdp = ' '
082200160120     c                             and V2Cemsms= ' '
082300170214     c                             and V2mitor = ' '
082400170214     c                             and V2tfntc = ' '
082500170214     c                             and V2noimtt= ' '
082600060403     c                   seton                                        285290
082700060403     c                   eval      V1Dmsg  = $Msg(5)
082800060403     c***                leavesr
082900071207      * messaggio forzabile se scelta abilitaz.per lna diskB ma cliente
083000071207      * non è disk B
083100071207     c                   when           v2cdkb='S' and W_diskB='N'
083200071207     c                             and  w_forzB=*blanks
083300071207     c                   eval      w_forzB='S'
083400071207     c                   seton                                        285890
083500071207     c                   eval      V1Dmsg  = $Msg(7)
083600060403      *
083700060403e   1c                   ENDSL
083800120919      *
083900120919      * Ctrl Filiale 1
084000120919w   1c                   IF        V2ClF1 <> 0
084100120919     c     V2ClF1        chain     azorg01l
084200120919     c                   if        not %Found(azorg01l)
084300120919     c                   seton                                        286090
084400120919     c                   eval      V1Dmsg  = $Msg(8)
084500120919     c                   else
084600120919     c                   if        ORGFVA <> *blank
084700120919     c                             or (ORGFAG <> 'F' and ORGFAG <> 'A')
084800120919     c                   seton                                        286090
084900120919     c                   eval      V1Dmsg  = $Msg(8)
085000120919     c                   endif
085100120919     c                   endif
085200120919     c                   endif
085300120919      *
085400120919      * Ctrl Filiale 2
085500120919w   1c                   IF        V2ClF2 <> 0
085600120919     c     V2ClF2        chain     azorg01l
085700120919     c                   if        not %Found(azorg01l)
085800120919     c                   seton                                        286190
085900120919     c                   eval      V1Dmsg  = $Msg(8)
086000120919     c                   else
086100120919     c                   if        ORGFVA <> *blank
086200120919     c                             or (ORGFAG <> 'F' and ORGFAG <> 'A')
086300120919     c                   seton                                        286190
086400120919     c                   eval      V1Dmsg  = $Msg(8)
086500120919     c                   endif
086600120919     c                   endif
086700120919     c                   endif
086800120919      *
086900120919      * Ctrl Filiale 3
087000120919w   1c                   IF        V2ClF3 <> 0
087100120919     c     V2ClF3        chain     azorg01l
087200120919     c                   if        not %Found(azorg01l)
087300120919     c                   seton                                        286290
087400120919     c                   eval      V1Dmsg  = $Msg(8)
087500120919     c                   else
087600120919     c                   if        ORGFVA <> *blank
087700120919     c                             or (ORGFAG <> 'F' and ORGFAG <> 'A')
087800120919     c                   seton                                        286290
087900120919     c                   eval      V1Dmsg  = $Msg(8)
088000120919     c                   endif
088100120919     c                   endif
088200120919     c                   endif
088300060403      *
088400060403     c                   ENDSR
088500170210
088600170210      *---------------------------------------------------------------*
088700170210      * CTRV3  - Controllo e decodifica seconda videata               *
088800170210      *---------------------------------------------------------------*
088900170210     c     CtrV3         BEGSR
089000170210      *
089100170210     c                   movea     *zeros        *in(50)
089200170210      *
089300170214     c* se il flag forzatura ragione sociale è S, sono obbligatorie entrambe le ragioni sociali
089400170214     c                   if        V2TFNTC = 'S'
089500170214     c                   if        V2RNT1 = *blank or
089600170214     c                             V2RNT2 = *blank
089700170214     c                   seton                                        286690
089800170214     c                   eval      V1Dmsg  = $Msg(14)
089900170214     c                   endif
090000170214     c                   endif
090100170210      *
090200170210     c                   ENDSR
090300060403
090400060403      *---------------------------------------------------------------*
090500060403      * GESW1  - Gestione videata dati relativi alla trasmissione     *
090600060403      *---------------------------------------------------------------*
090700060403     c     GesW1         BEGSR
090800060403      *
090900060403      * Inizializzazione videata
091000060403if  1c                   if        $InzW1 = *on
091100060403     c                   exsr      InzW1
091200060403     c                   movel     *off          $InzW1
091300060403e   1c                   endif
091400060403      *
091500060403if  1***c                   if        *in05
091600060403     ***c                   write     TB53W1
091700060403     ***c                   exfmt     PROTECT
091800060403x   1***c                   else
091900060403     c                   exfmt     TB53W1
092000060403e   1***c                   endif
092100060403     c                   eval      *in90 = *off
092200060403     c                   clear                   W1MSG
092300060403      *
092400060403sel 1c                   select
092500060403      * F12=Ritorno
092600060403w   1c                   when      *inKL
092700060403     c                   exsr      F12W1
092800060403     c                   goto      EndGesW1
092900060403e   1c                   endsl
093000060403      *
093100060403      * Controllo dati immessi a video
093200060403     c                   exsr      CtrW1
093300060403      *
093400060403      * Aggiornamento se non ci sono errori
093500060403if  1c                   if        not *in90 and *inKF
093600060403     c                   exsr      AggTBE
093700060403e   1c                   endif
093800060403      *
093900060403     c     EndGesW1      ENDSR
094000060403
094100060403      *---------------------------------------------------------------*
094200060403      * INZW1  - Inzializzazione window (W1)                          *
094300060403      *---------------------------------------------------------------*
094400060403     c     InzW1         BEGSR
094500060403      *
094600060403     c                   movea     *zeros        *in(50)
094700060403      *
094800060403sel 1c                   select
094900060403      *
095000060403      * F5=Ripristino
095100060403w   1c                   when      *inKE   and  *in04
095200060403     c                   eval      W1FTT = TBEftt
095300060403      *
095400060403      * F6=Conferma
095500060403w   1c                   when      *inKF
095600060403sel 2c                   select
095700060403      *   Immissione
095800060403w   2c                   when      *in01
095900060403     c                   eval      W1FTT = TBXftt
096000060403      *   Modifica / Ripristino
096100060403w   2c                   when      *in02   or    *in04
096200060403     c                   eval      W1FTT = TBEftt
096300060403e   2c                   endsl
096400060403      *
096500060403      * F16=Annullamento
096600060403w   1c                   when      *inKQ   and  not *in04
096700060403     c                   eval      W1FTT = TBEftt
096800060403      *
096900060403e   1c                   endsl
097000060403      *
097100060403      * Se NON immissione: visualizzo i dati relativi all'ultima
097200060403      *   trasmissione
097300060403if  1c                   if        not *in01
097400060403     c                   eval      W1FLT = TBEflt
097500060403     c                   eval      W1FTR = TBEftr
097600060403if  2c                   if        TBEdtr <> 0
097700060403     c                   clear                   WLBDAT
097800060403     c                   z-add     TBEdtr        G02inv
097900060403     c                   movel     '3'           G02err
098000060403     c                   call      'XSRDA8'
098100060403     c                   parm                    WLBDAT
098200060403     c                   z-add     G02dat        W1DTR
098300060403e   2c                   endif
098400060403e   1c                   endif
098500060403      *
098600060403     c                   ENDSR
098700060403
098800060403      *---------------------------------------------------------------*
098900060403      * CTRW1  - Controllo e decodifica window                        *
099000060403      *---------------------------------------------------------------*
099100060403     c     CtrW1         BEGSR
099200060403      *
099300060403     c                   movea     *zeros        *in(50)
099400060403      *
099500060403     c     EndCtrW1      ENDSR
099600060403
099700060403      *---------------------------------------------------------------*
099800060403      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
099900060403      *---------------------------------------------------------------*
100000060403     c     F12W1         BEGSR
100100060403      *
100200060403     c                   eval      $TipVid = '2'
100300060403      *
100400060403     c                   ENDSR
100500060403
100600060403      *---------------------------------------------------------------*
100700060403      * CHNTBE * Aggancio tabella                                     *
100800060403      *---------------------------------------------------------------*
100900060403     c     ChnTBE        BEGSR
101000060403      *
101100060403     c                   movel     T02cod        TBEcod
101200060403     c                   movel(p)  V2Cksc        TBEke1
101300060403     c                   clear                   TBEke2
101400060403     c                   clear                   TBElin
101500060403     c                   movel     KNSIF         TBEsif
101600060403     c     K05TBE01      chain     TNTBE01L
101700060403      * Se non ho trovato il record con il sistema informativo
101800060403      * che ho in linea, lo abblenco
101900060403if  1c                   if        not %found(TNTBE01L)
102000060403     c                   clear                   TBEsif
102100060403     c     K05TBE01      chain     TNTBE01L
102200060403e   1c                   endif
102300060403      *
102400060403     c                   ENDSR
102500060403
102600060403      *---------------------------------------------------------------*
102700060403      * AGGTBE * Aggiornamento tabella                                *
102800060403      *---------------------------------------------------------------*
102900060403     c     AggTBE        BEGSR
103000060403      *
103100060403sel 1c                   SELECT
103200060403      *
103300060403      * F5=Ripristino
103400060403w   1c                   WHEN      wTasto='05'  and  *in04
103500060403     c                   clear                   TBEatb
103600060525     c                   movel     W1ftt         TBEftt
103700060403     c                   clear                   TBEftr
103800060403     c                   UPDATE    TNTBE000
103900060403      *
104000060403      * F6=Conferma
104100060403w   1c                   WHEN      wTasto='06'
104200060403     c                   exsr      RieTBE
104300060525     c*** già in RieTBE: movel     W1ftt         TBEftt
104400060403sel 2c                   select
104500060403      *   Immissione
104600060403w   2c                   when      *in01
104700060403     c                   clear                   TBEflt
104800060403     c                   clear                   TBEdtr
104900060403     c                   WRITE     TNTBE000                             22
105000060403      *   Modifica / Ripristino
105100060403w   2c                   when      *in02   or    *in04
105200060403     c                   UPDATE    TNTBE000
105300060403e   2c                   endsl
105400170214     c*
105500170214     c* se il flag forzatura ragione sociale è S, scrivo/modifico i valori delle ragioni sociali
105600170214     c                   if        V2TFNTC = 'S'
105700170214     c                   exsr      GestRagSoc
105800170214     c                   endif
105900060403      *
106000060403      * F16=Annullamento
106100060403w   1c                   WHEN      wTasto='16'  and  not *in04
106200060403     c                   movel     'A'           TBEatb
106300060525     c                   movel     W1ftt         TBEftt
106400060403     c                   clear                   TBEftr
106500060403     c                   UPDATE    TNTBE000
106600060403      *
106700060403e   1c                   ENDSL
106800060403      *
106900060403      * Torno alla prima videata che carico come da inizio pgm
107000060403     c                   movel     '1'           $TipVid
107100060403     c                   movel     *on           $InzV1
107200060403     c                   movel     *on           $InzV2
107300060403     c                   movel     *on           $InzW1
107400060403     c
107500060403     c                   ENDSR
107600060403
107700060403      *---------------------------------------------------------------*
107800060403      * RIETBE * Riempimento dati tabella                             *
107900060403      *---------------------------------------------------------------*
108000060403     c     RieTBE        BEGSR
108100060403      *
108200060403     c                   clear                   TBEatb
108300060403     c                   if        TBXsif <> *blanks
108400060403     c                   movel     KNSIF         TBEsif
108500060403     c                   else
108600060403     c                   clear                   TBEsif
108700060403     c                   endif
108800060403     c                   movel     TBXapl        TBEapl
108900060403     c                   movel     T02cod        TBEcod
109000060403     c                   movel     V2Cksc        TBEke1
109100060403     c                   clear                   TBEke2
109200060403      *
109300060403     c                   clear                   dCLI
109400060403     c                   eval      §CLIrag = V2Crag
109500060403     c                   eval      §CLIldv = V2Cldv
109600060403     c                   eval      §CLIdpd = V2Cdpd
109700060403     c                   eval      §CLIlna = V2Clna
109800060525     c                   eval      §CLIlvp = V2Clvp
109900071207     c                   eval      §CLIdkb = V2Cdkb
110000071218     c                   eval      §CLIdcr = V2Cdcr
110100110805     c***                eval      §CLIidt = V2Cidt
110200110921     c                   eval      §CLIban = V2Cban
110300091125     c                   eval      §CLIgcp = V2Cgcp
110400100324     c                   eval      §CLIbrd = V2Cbrd
110500120516     c                   eval      §CLIfts = V2Cfts
110600120618     c                   eval      §CLIemd = V2Cemd
110700130320     c                   eval      §CLIpdfp= V2Cpdfp
110800151105     c                   eval      §CLIemdp= V2Cemdp
110900160120     c                   eval      §CLIemsms=V2Cemsms
111000131002     c                   move      v2cpsm        §clipsm
111100131002     c                   move      v2cvlm        §clivlm
111200131001     c                   eval      §CLItspf= V2Ctspf
111300131001     c                   eval      §CLIsup = V2Csup
111400120919     c                   MOVEL     V2ClF1        §CLIFi1
111500120919     c                   MOVEL     V2ClF2        §CLIFi2
111600120919     c                   MOVEL     V2ClF3        §CLIFi3
111700170214      *
111800170214     c                   movel     V2MITOR       §CLIMITOR
111900170214     c                   movel     V2TFNTC       §CLITFNTC
112000170214     c                   movel     V2NOIMTT      §CLINOIMTT
112100170904     c                   movel     V2blkal       §CLIblkalr
112200170214      *
112300060403     c                   movel(p)  dCLI          TBEuni
112400170214      *
112500060525     c                   movel     W1ftt         TBEftt
112600060403     c                   clear                   TBEflt
112700060403     c                   clear                   TBEftr
112800060403     c                   clear                   TBEdtr
112900060403      *
113000060403     c                   ENDSR
113100131002      *---------------------------------------------------------------*
113200170214      * GestRagSoc * gestione ragioni sociali in terza videata
113300131002      *---------------------------------------------------------------*
113400170214     c     GestRagSoc    BEGSR
113500170214      *
113600170214      * se sono qui, il flag di forzatura Ragioni Sociali è a SI, quindi conunque ci dovranno
113700170214      * essere 2 rcd su TFNTC
113800170214      * controllo se i rcd esistono
113900170214          k_NTCAPL =  'C';
114000170214          k_NTCNK1 =  '0151' + %trim(TBEKE1);
114100170214          k_NTCNK2 =  *BLANK;
114200170214          k_NTCTNT =  'AM';
114300170214          chain  %kds(k04tFNTC01)  TFNTC;
114400170214          if %found();
114500170214            NTCRNT = V2RNT1;
114600170214            update TFNTC;
114700170214          else;
114800170214            NTCAPL =  'C';
114900170214            NTCNK1 =  '0151' + %trim(TBEKE1);
115000170214            NTCNK2 =  *BLANK;
115100170214            NTCTNT =  'AM';
115200170214            NTCSNS = 'S';
115300170214            NTCRNT = V2RNT1;
115400170214            write TFNTC;
115500170214          endif;
115600170214          k_NTCAPL =  'C';
115700170214          k_NTCNK1 =  '0151' + %trim(TBEKE1);
115800170214          k_NTCNK2 =  *BLANK;
115900170214          k_NTCTNT =  'AS';
116000170214          chain  %kds(k04tFNTC01)  TFNTC;
116100170214          if %found();
116200170214            NTCRNT = V2RNT2;
116300170214            update TFNTC;
116400170214          else;
116500170214            NTCAPL =  'C';
116600170214            NTCNK1 =  '0151' + %trim(TBEKE1);
116700170214            NTCNK2 =  *BLANK;
116800170214            NTCTNT =  'AS';
116900170214            NTCSNS = 'S';
117000170214            NTCRNT = V2RNT2;
117100170214            write TFNTC;
117200170214          endif;
117300170214      *
117400170214     c                   ENDSR
117500170214      *---------------------------------------------------------------*
117600170214      * ctrtsp * Controllo tipo servizio forzato per instrad.personaliz.
117700170214      *---------------------------------------------------------------*
117800170214     c     ctrtsp        BEGSR
117900170214
118000131002     c                   clear                   werr_tsp
118100131002     c                   clear                   v2dtspf
118200131002
118300131002     c                   move      '5E'          cod
118400131002     c                   if        v2ctspf='?'
118500131002     C                   CLEAR                   §KEY
118600131002     C                   CALL      'X§TABER'
118700131002     C                   PARM                    CODUT
118800131002     C                   PARM                    COD
118900131002     C                   PARM                    §KEY
119000131002     C                   PARM                    §DES             25
119100131002     C                   MOVEL     §KEY          v2ctspf
119200131002     C                   MOVEL     §des          v2dtspf
119300131002     c                   eval      werr_tsp='R'
119400131002     c                   else
119500131002     c                   movel(p)  v2ctspf       key
119600131002     c     ktab2         chain     tabel00f
119700131002     c                   if        %found(tabel00f) and tblflg=' '
119800131002     C                   movel     tbluni        ds5e
119900131002     c                   movel     §5edes        v2dtspf
120000131002     c                   else
120100131002     c                   eval      werr_tsp='1'
120200131002     c                   endif
120300131002     c                   endif
120400131002     c                   ENDSR
120500060403
120600060403** - $Opz ----*
120700060403  Inserimento
120800060403    Modifica
120900060403     Copia
121000060403  Annullamento
121100060403Visualizzazione
121200060403   ANNULLATO
121300060403** - $Msg -------------------------------------------------------------------*
121400060403Immettere il codice cliente                                                    01
121500060403Codice cliente errato                                                          02
121600060403Ragione sociale obbligatoria                                                   03
121700070607Unico  valore  previsto  S=Sì  (  _=No )                                       04
121800060403Effettuare ALMENO una abilitazione per il cliente                              05
121900131001Se immesso il Peso immettere anche il Volume e viceversa                       06
122000071207Attenzione il cliente selezionato non è un DisK "B". Enter per forzare         07
122100120919Cod.Filiale inesistente o non valido o non appartenente a Filiale/Agenzia      08
122200131001Tipo Servizio Errato o non utilizzabile                                        09
122300131001Richiesta non valida: immettere anche il Peso e il Volume medio                10
122400170918                                                                               11
122500170214Errori nella videata successiva                                                12
122600170214Errori nella videata precedente                                                13
122700170214Se Forzatura Ragione Sociale da note AM/AS = S, entrambe sono obbligatorie     14
