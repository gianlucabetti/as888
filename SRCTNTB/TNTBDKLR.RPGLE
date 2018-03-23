000100060403      *---------------------------------------------------------------*
000200130408      * Gestione tabella "DKL" = Clienti con chiusura forzata C.A.    *
000300060403      *---------------------------------------------------------------*
000400060403
000500130408     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600060403
000700060403      *---------------------------------------------------------------*
000800060403      *   A R C H I V I                                               *
000900060403      *---------------------------------------------------------------*
001000060403      *
001100091125     fTNTBE01L  uf a e           k disk
001200060403      *
001300130408     fTNTBDKLD  cf   e             workstn
001400060403
001500060403      *---------------------------------------------------------------*
001600060403      *   C O S T A N T I                                             *
001700060403      *---------------------------------------------------------------*
001800060403      *
001900130409     d DigitN          c                   const('0123456789')
002000060403
002100060403      *---------------------------------------------------------------*
002200060403      *   S C H I E R E                                               *
002300060403      *---------------------------------------------------------------*
002400060403      *
002500060403     d $Opz            s             15    dim( 6) ctdata perrcd(1)             Decodifica OPZ
002600120919     d $Msg            s             78    dim( 8) ctdata perrcd(1)             Messaggi video
002700060403
002800060403      *---------------------------------------------------------------*
002900060403      *   S T R U T T U R E   D A T I                                 *
003000060403      *---------------------------------------------------------------*
003100060403      *
003200060403      * Parametri
003300060403     d KPJBA         e ds
003400060403      *
003500060403      * Passaggio Parametri al pgm TIBS02R
003600060403     d TIBS02ds      e ds                  inz
003700060403     d  T02mod       e                     inz('R')
003800130409     d  T02cod       e                     inz('DKL')
003900130409
004000130409      * TIBS02DS per ricerche no su DKL
004100130409     d ATIBS02DS     e ds                  extname(TIBS02DS) inz
004200130409     d                                     prefix(a)
004300060403      *
004400130408      * Tabella DKL = Clienti con chiusura forzata c.a.
004500130409     d dDKL          e ds                  inz
004600071207
004700071207     d ds3c          e ds                  inz
004800060403      *
004900060403      * Tracciato record file TNTBE00F
005000060403     d TNTBEds       e ds                  extname(TNTBE00F) inz
005100060403     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005200060403     d                                     prefix(TBX:3)
005300060403      *
005400060403     d TIBS34ds      e ds                  inz
005500060403     d dDatiUte      e ds                  inz
005600060403     d AZUTEds       e ds                  extname(AZUTE00F) inz
005700060403      *
005800060403      * Decodifica anagrafica clienti
005900060403     d TIBS69ds      e ds                  inz
006000060403     d ds_CNACO      e ds                  extname(CNACO00F) inz
006100060403     d ds_CNIND      e ds                  extname(CNIND00F) inz
006200060403     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
006300060403     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
006400060403      *
006500060403     d WLBdat          ds                  inz
006600060403     d  G02dat                 1      8  0 inz
006700060403     d  G02inv                 9     16  0 inz
006800060403     d  G02err                17     17    inz('3')
006900060403     d  G02tgi                18     22  0 inz
007000060403      *
007100060403     d Status         sds
007200060403     d  VTCpgm           *proc
007300060403
007400060403      *---------------------------------------------------------------*
007500060403      *   V A R I A B I L I                                           *
007600060403      *---------------------------------------------------------------*
007700060403      *
007800060403     d $Fine           s              1a   inz(*off)
007900060403     d $InzV1          s              1a   inz(*on)
008000060403     d $InzV2          s              1a   inz(*off)
008100060403     d $InzW1          s              1a   inz(*off)
008200060403     d $TipVid         s              1a   inz('1')
008300060403     d $TIBS02         s              1a   inz(*off)
008400060403     d wTasto          s              2a   inz(*zeros)
008500060403     d wDate           s               d   datfmt(*iso) inz(*loval)
008600060403
008700060403      *---------------------------------------------------------------*
008800060403      *   M A I N   L I N E                                           *
008900060403      *---------------------------------------------------------------*
009000060403      *  Riepilogo indicatori utilizzati:                             *
009100060403      *  --------------------------------                             *
009200060403      *  01 - Record inesistente (inserimento)                        *
009300060403      *  02 - Record esistente   (modifica)                           *
009400060403      *  04 - Record annullato   (ripristino)                         *
009500060403      *  10 - Comodo                                                  *
009600060403      *  22 - Errori in scrittura record (WRITE)                      *
009700060403      *  28 - Emissione messaggio di errore a video                   *
009800130409      *  50 - ERR: Codice referente                                   *
009900130409      *  51 - ERR: numero MAX c.a. da chiudere                        *
010000130409      *  52 - ERR: numeratore                                         *
010100130409      *  53 - ERR: causale chiusura c.a.                              *
010200060403      *  90 - Riemissione videata                                     *
010300060403      *---------------------------------------------------------------*
010400060403      *
010500060403      * Operazioni iniziali
010600060403     c                   exsr      RutInz
010700060403      *
010800060403      * Gestione video
010900060403     c                   dow       $Fine   = *off
011000060403     c     $TipVid       caseq     '1'           GesV1
011100060403     c     $TipVid       caseq     '2'           GesV2
011200060403     c     $TipVid       caseq     'A'           GesW1
011300060403     c                   endcs
011400060403     c                   enddo
011500060403      *
011600060403      * Fine
011700060403     c                   if        $TIBS02 = *on
011800060403     c                   clear                   TIBS02ds
011900060403     c                   movel     'C'           T02tla
012000060403     c                   call      'TIBS02R'
012100060403     c                   parm                    KPJBA
012200060403     c                   parm                    TIBS02ds
012300060403     c                   endif
012400060403      *
012500060403     c                   movel     *on           *inLR
012600060403
012700060403      *---------------------------------------------------------------*
012800060403      * RutInz - Operazioni Iniziali                                  *
012900060403      *---------------------------------------------------------------*
013000060403     c     RutInz        BEGSR
013100060403      *
013200060403      * Ricezione parametri
013300060403     c     *entry        plist
013400060403     c                   parm                    KPJBA
013500060403      *
013600060403      * Definizioni chiavi di accesso
013700060403     c     K05TBE01      klist                                                  *tntbe01l
013800060403     c                   kfld                    TBEcod                         -tabella
013900060403     c                   kfld                    TBEke1                         -chiave uno
014000060403     c                   kfld                    TBEke2                         -chiave due
014100060403     c                   kfld                    TBElin                         -lingua
014200060403     c                   kfld                    TBEsif                         -s.informativo
014300060403      *
014400060403      * Reperisco la data odierna
014500060403     c                   movel     *date         Wdate
014600060403      *
014700060403      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
014800060403     c     *dtaara       define    §azute        AZUTEds
014900060403     c     *dtaara       define    §datiute      dDatiUte
015000060403      *
015100060403     c                   clear                   AZUTEds
015200060403     c                   clear                   dDatiUte
015300060403     c                   clear                   TIBS34ds
015400060403     c                   in(E)     *dtaara
015500060403if  1c                   if        %Error  or  RSUT = *blanks
015600060403     c                   call      'TIBS34R'
015700060403     c                   parm                    TIBS34ds
015800060403     c                   in        *dtaara
015900060403e   1c                   endif
016000060403      *-- Verifica errori e autorità profilo
016100060403sel 1c                   SELECT
016200060403      *-- controllo se ho errori nei dati utente
016300060403      *--   nel qual caso NON risulta un profilo abilitato
016400060403w   1c                   WHEN      DUTerr  = 'E'
016500060403     c                   eval      $Fine   = *on
016600060403      *
016700060403      *-- CONTROLLO AUTORITA'
016800060403      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
016900060403      *-- se il chiamante non richiede autorità specifica verificare
017000060403      *--   quella del profilo
017100060403      *-- se il chiamante richiede autorità specifica verificarla,
017200060403      *--  se è blank verificare quella del profilo
017300060403      *
017400060403      * se UTEAUT = *BLANK non siamo in sede
017500060403w   1c                   WHEN      UTEaut  = *blank
017600060403      *
017700060403x   1c                   OTHER
017800060403      *
017900060403e   1c                   ENDSL
018000060403      *
018100060403      * Aggancio dati generali della tabella in esame
018200060403     c                   clear                   TBEcod
018300060403     c                   move      *zeros        TBEke1
018400060403     c                   move      T02cod        TBEke1
018500060403     c                   clear                   TBEke2
018600060403     c                   clear                   TBElin
018700060403     c                   movel     KNSIF         TBEsif
018800060403     c     K05TBE01      chain     TNTBE01L
018900060403     c                   if        not %found(TNTBE01L)
019000060403     c                   clear                   TBEsif
019100060403     c     K05TBE01      chain     TNTBE01L
019200060403     c                   endif
019300060403     c                   if        %found(TNTBE01L)
019400060403     c                   movel     TNTBEds       xTNTBEds
019500060403     c                   else
019600060403     c                   clear                   xTNTBEds
019700060403     c                   endif
019800060403      *
019900130409     c                   clear                   TBDKLV1
020000120925     C*
020100120925     C* ctrl i dati presenti in KPJBU
020200120925     C* se da pos. 1 x 7 ci sono solo caratteri numerici, allora è arrivato in ingresso il codice
020300120925     C* cliente, quindi lo testo e se corretto passo subito alla seconda videata
020400120925x   1c                   IF        KPJBU <> *blank
020500120925     C* faccio in modo che anche se devo andare in videata 1 per errore rimanga il codice ricevuto
020600130409     c                   clear                   TBDKLV1
020700120925if  1c                   EVAL      $InzV1  = *off
020800130409x   1c                   EVAL      V01Ksc = %subst(KPJBU : 1 : 7)
020900130409x   1c                   TESTN                   V01Ksc               10
021000120925     C                   IF        *in10 = *on                                  *E'NUMERO
021100120925     C* e l'ultimo carattere non è il segno ma una cifra anch'essa
021200130409     C                             and %subst(V01Ksc : 7 : 1) >= '0'
021300120925     c                   exsr      CtrV1
021400120925     C* se non rilevati errori
021500120925if  2c                   if        *in90 = *off
021600120925     C* Passo alla videata di dettaglio
021700120925     c                   eval      $InzV2  = *on
021800120925     c                   eval      $TipVid = '2'
021900120925     C                   ENDIF
022000120925     C                   ENDIF
022100120925     C                   ENDIF
022200060403      *
022300060403     c                   ENDSR
022400060403
022500060403      *---------------------------------------------------------------*
022600060403      * GESV1  - Gestione videata selezione codice tabella            *
022700060403      *---------------------------------------------------------------*
022800060403     c     GesV1         BEGSR
022900060403      *
023000060403      * Inizializzazione videata
023100060403if  1c                   if        $InzV1  = *on
023200060403     c                   exsr      InzV1
023300060403     c                   movel     *off          $InzV1
023400060403e   1c                   endif
023500060403      *
023600060403      * Scrivo la testata
023700130409     c                   write     TBDKLT0
023800130409      * emetto la videata
023900130409     c                   exfmt     TBDKLV1
024000060403     c                   setoff                                       28  90
024100130409     c                   clear                   V01msg
024200060403      *
024300060403sel 1c                   select
024400060403      * F3=Fine
024500060403w   1c                   when      *inKC
024600060403     c                   exsr      F03V1
024700060403     c                   leavesr
024800060403      *
024900060403      * Controllo dati immessi a video
025000060403x   1c                   other
025100060403     c                   exsr      CtrV1
025200060403      * - Rilevati Errori
025300060403if  2c                   if        *in90
025400060403     c                   leavesr
025500060403e   2c                   endif
025600060403      * - Passaggio alla videata di dettaglio
025700060403     c                   eval      $InzV2  = *on
025800060403     c                   eval      $TipVid = '2'
025900060403      *
026000060403e   1c                   endsl
026100060403      *
026200060403     c                   ENDSR
026300060403
026400060403      *---------------------------------------------------------------*
026500060403      * INZV1  - Inizializzazione prima videata (V1)                  *
026600060403      *---------------------------------------------------------------*
026700060403     c     InzV1         BEGSR
026800060403      *
026900130409     c                   clear                   TBDKLV1
027000060403      *
027100060403      * Imposto il campo "codice cliente" per l'interrogazione
027200130409     c                   move      '?'           V01ksc
027300060403      *
027400060403     c                   ENDSR
027500060403
027600060403      *---------------------------------------------------------------*
027700060403      * CTRV1  - Controllo e decodifica prima videata                 *
027800060403      *---------------------------------------------------------------*
027900060403     c     CtrV1         BEGSR
028000060403      *
028100060403     c                   movea     *zeros        *in(50)
028200060403      *
028300060403      * Codice cliente
028400060403      * - Ricerca:
028500130409     c     '?'           scan      V01ksc                                 10
028600060403if  1c                   if        *in10
028700060403     c                   eval      $TIBS02 = *on
028800130409     c                   clear                   V01ksc
028900060403     c                   seton                                          5090
029000060403     c                   reset                   TIBS02ds
029100060403     c                   movel     KNSIF         T02sif
029200060403     c                   call      'TIBS02R'
029300060403     c                   parm                    KPJBA
029400060403     c                   parm                    TIBS02ds
029500060403if  2c                   if        T02err  = *blanks
029600130409     c                   movel     T02uni        dDKL
029700130409     c                   movel     T02ke1        V01ksc
029800060403x   2c                   else
029900060403     c                   seton                                        285090
030000130409     c                   movel     T02msg        V01msg
030100060403     c                   leavesr
030200060403     c                   endif
030300060403e   1c                   endif
030400060403      * - Controllo:
030500060403      * - - effettiva immissione
030600130409if  1c                   if           V01ksc = *blanks
030700130409     c                             or V01ksc = *zeros
030800060403     c                   seton                                        285090
030900130409     c                   eval      V01msg  = $Msg(1)
031000060403     c                   leavesr
031100060403e   1c                   endif
031200060403      * - - numericità
031300130409     c     DigitN        check     V01ksc                                 10
031400060403if  1c                   if        *in10   =  *on
031500060403     c                   seton                                        285090
031600130409     c                   eval      V01msg  = $Msg(1)
031700060403     c                   leavesr
031800060403e   1c                   endif
031900060403      * - - decodifica
032000060403     c                   exsr      DecodCLI
032100060403      * - - "appartenenza" al P.O. dell'utente
032200130409     c                   if            %subst(V01ksc:1:3) <> %char(DUTpou)
032300060403     c                             and DUTlpo             <> 'S'
032400060403     c                             and %subst(DUTute:1:3) <> 'EDP'
032500060403     c                   seton                                        285090
032600130409     c                   eval      V01msg  = $Msg(1)
032700060403     c                   leavesr
032800060403     c                   endif
032900060403      *
033000060403     c                   ENDSR
033100060403
033200060403      *---------------------------------------------------------------*
033300060403      * DecodCLI   * Decodifica codice cliente                        *
033400060403      *---------------------------------------------------------------*
033500060403     c     DecodCLI      BEGSR
033600060403      *
033700060403     c                   clear                   TIBS69ds
033800060403     c                   clear                   ds_CNACO
033900060403     c                   clear                   ds_CNIND
034000060403     c                   clear                   ds_CNCLP
034100060403     c                   clear                   ds_FNCLS
034200130409     c                   move      V01ksc        I69kac
034300060403     c                   call      'TIBS69R'
034400060403     c                   parm                    TIBS69ds
034500060403     c                   parm                    ds_CNACO
034600060403     c                   parm                    ds_CNIND
034700060403     c                   parm                    ds_CNCLP
034800060403     c                   parm                    ds_FNCLS
034900060403      *
035000060403     c                   if        O69err  = *blanks
035100130409     c                   movel     ACOrag        V01kscd
035200060403     c                   else
035300060403     c                   seton                                        285090
035400130409     c                   eval      V01msg  = $Msg(1)
035500060403     C                   endif
035600060403      *
035700060403     c                   ENDSR
035800060403
035900060403      *---------------------------------------------------------------*
036000060403      * F03V1  - Tasto funzionale F03 -> Fine programma               *
036100060403      *---------------------------------------------------------------*
036200060403     c     F03V1         BEGSR
036300060403      *
036400060403     c                   movel     *on           $Fine                          fine pgm
036500060403      *
036600060403     c                   ENDSR
036700060403
036800060403      *---------------------------------------------------------------*
036900060403      * GESV2  - Gestione videata dettaglio dati                      *
037000060403      *---------------------------------------------------------------*
037100060403     c     GesV2         BEGSR
037200060403      *
037300060403      * Inizializzazione videata
037400060403     c                   if        $InzV2  = *on
037500060403     c                   exsr      InzV2
037600060403     c                   move      *off          $InzV2
037700060403     c                   endif
037800060403      *
037900060403      * Scrivo la testata
038000130409     c                   write     TBDKLT0
038100130409     c                   exfmt     TBDKLV2
038200060403     c                   setoff                                       28  90
038300130409     c                   clear                   V02msg
038400060403     c                   reset                   wTasto
038500060403      *
038600060403sel 1c                   select
038700060403      * F03=Fine
038800060403w   1c                   when      *inKC
038900060403     c                   exsr      F03V1
039000130409      * F04=Numeratore
039100130409w   1c                   when      *inKD
039200130409     c                   exsr      F04V2
039300060403      * F12=Ritorno
039400060403w   1c                   when      *inKL
039500060403     c                   exsr      F12V2
039600060403      *
039700060403x   1c                   other
039800060403      * Controllo dati immessi a video
039900060403      * (non si fanno se richisto l'annullamento)
040000060403if  2c                   if        not *inKQ
040100060403     c                   exsr      CtrV2
040200060403e   2c                   endif
040300060403      * Aggiornamento se non ci sono errori
040400060403sel 2c                   select
040500060403      * - Rilevati errori
040600060403w   2c                   when      *in90
040700060403      * - F5=Ripristino
040800060403w   2c                   when      *inKE
040900060403     c                   eval      wTasto  = '05'
041000060403      * - F6=Conferma
041100060403w   2c                   when      *inKF
041200060403     c                   eval      wTasto  = '06'
041300060403      * - F16=Annullamento
041400060403w   2c                   when      *inKQ
041500060403     c                   eval      wTasto  = '16'
041600060403e   2c                   endsl
041700060403if  2c                   if        wTasto  > *zeros
041800060403     c                   eval      $InzW1  = *on
041900060403     c                   eval      $TipVid = 'A'
042000060403e   2c                   endif
042100060403e   1c                   endsl
042200060403      *
042300060403     c                   ENDSR
042400060403
042500060403      *---------------------------------------------------------------*
042600060403      * INZV2  - Inizializzazione seconda videata (V2)                *
042700060403      *---------------------------------------------------------------*
042800060403     c     InzV2         BEGSR
042900060403      *
043000130409     c                   clear                   TBDKLV2
043100060403      *
043200060403      * reimposto il campo chiave (qui di solo output)
043300130409     c                   movel     V01ksc        V02ksc
043400130409     c                   movel     V01kscd       V02kscd
043500071207     c
043600060403      *
043700060403      * Aggancio la tabella, se trovo il codice sono in modifica
043800060403      * o ripristino (se record annullato), altrimenti in immissione
043900130409     c                   clear                   dDKL
044000060403     c                   exsr      ChnTBE
044100060403      *
044200060403sel 1c                   SELECT
044300060403      * IMMISSIONE
044400060403w   1c                   WHEN      NOT %found(TNTBE01L)
044500060403     c                   eval      *in01   = *on
044600130409     c                   eval      T01opz   = $Opz(01)
044700060403      * MODIFICA
044800060403w   1c                   WHEN          %found(TNTBE01L)
044900060403     c                             and TBEatb  = *blanks
045000060403     c                   eval      *in02   = *on
045100130409     c                   eval      T01opz   = $Opz(02)
045200060403      * RIPRISTINO
045300060403w   1c                   WHEN          %found(TNTBE01L)
045400060403     c                             and TBEatb <> *blanks
045500060403     c                   eval      *in04   = *on
045600130409     c                   eval      T01opz   = $Opz(06)
045700060403e   1c                   ENDSL
045800060403      *
045900060403if  1c                   if        %found(TNTBE01L)
046000130409     c                   movel     TBEuni        dDKL
046100130409     c                   eval      V02nca  = §DKLnca
046200130409     c                   eval      V02cnu  = %editc(§DKLcnu:'X')
046300130409     c                   eval      V02cau  = §DKLcau
046400060403e   1c                   endif
046500060403      *
046600060403     c                   ENDSR
046700130409
046800130409      *---------------------------------------------------------------*
046900130409      * F04V2  - Tasto funzionale F04 -> Gestione numeratore          *
047000130409      *---------------------------------------------------------------*
047100130409     c     F04V2         BEGSR
047200130409
047300130411     c                   clear                   kpjbu
047400130409     c                   call      'TNTB24R'
047500130409     c                   parm                    kpjba
047600130409
047700130409     c                   ENDSR
047800060403
047900060403      *---------------------------------------------------------------*
048000060403      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
048100060403      *---------------------------------------------------------------*
048200060403     c     F12V2         BEGSR
048300060403      *
048400060403     c                   eval      $TipVid = '1'
048500060403      *
048600060403     c                   ENDSR
048700060403
048800060403      *---------------------------------------------------------------*
048900060403      * CTRV2  - Controllo e decodifica seconda videata               *
049000060403      *---------------------------------------------------------------*
049100060403     c     CtrV2         BEGSR
049200060403      *
049300060403     c                   movea     *zeros        *in(50)
049400060403      *
049500060403sel 1c                   SELECT
049600060403      *
049700130409      * Numero MAX c.a. obbligatorio
049800130409w   1c                   WHEN          V02nca = *zeros
049900130409     c                   seton                                        285190
050000130409     c                   eval      V02msg  = $Msg(2)
050100060403      *
050200130409      * Causale chiusura obbligatoria
050300130409w   1c                   WHEN          V02cau = *blanks
050400130409     c                   seton                                        285290
050500130409     c                   eval      V02msg  = $Msg(3)
050600060403      *
050700130409      * Codice numertore obbligatorio
050800130409w   1c                   WHEN          V02cnu = *blanks or
050900130409     c                                 V02cnu = *zeros
051000130409     c                   seton                                        285390
051100130409     c                   eval      V02msg  = $Msg(4)
051200060403      *
051300060403e   1c                   ENDSL
051400120919      *
051500130409      * Ctrl la causale
051600130409     c                   IF        %scan('?':V02cau) > 0
051700130409     c                   clear                   aTIBS02ds
051800130409     c                   eval      aT02mod = 'R'
051900130409     c                   eval      aT02cod = 'CCH'
052000130409     c                   movel     KNSIF         aT02sif
052100130409     c                   call      'TIBS02R'
052200130409     c                   parm                    KPJBA
052300130409     c                   parm                    aTIBS02ds
052400130409     c                   if        aT02err  = *blanks
052500130409     c                   seton                                          5290
052600130409     c                   eval      V02cau  = aT02ke1
052700130409     c                   leavesr
052800130409     c                   endif
052900130409     c                   endif
053000130409
053100130409     c                   clear                   aTIBS02ds
053200130409     c                   eval      aT02mod = 'C'
053300130409     c                   eval      aT02cod = 'CCH'
053400130409     c                   eval      at02ke1 = V02cau
053500130409     c                   movel     KNSIF         aT02sif
053600130409     c                   call      'TIBS02R'
053700130409     c                   parm                    KPJBA
053800130409     c                   parm                    aTIBS02ds
053900130409     c                   if        aT02err  <>  *blanks
054000130409     c                   seton                                        285290
054100130409     c                   eval      V02msg  = $Msg(3)
054200130409     c                   leavesr
054300120919     c                   endif
054400120919      *
054500130409      * Ctrl il numeratore
054600130409     c                   IF        %scan('?':V02cnu) > 0
054700130409     c                   clear                   aTIBS02ds
054800130409     c                   eval      aT02mod = 'R'
054900130409     c                   eval      aT02cod = 'CNU'
055000130409     c                   movel     KNSIF         aT02sif
055100130409     c                   call      'TIBS02R'
055200130409     c                   parm                    KPJBA
055300130409     c                   parm                    aTIBS02ds
055400130409     c                   if        aT02err  = *blanks
055500130409     c                   seton                                          5390
055600130409     c                   eval      V02cnu  = aT02ke1
055700130409     c                   leavesr
055800130409     c                   endif
055900130409     c                   endif
056000130409
056100130409     c     DigitN        check     V02cnu                                 10
056200130409if  1c                   if        *in10   =  *on
056300130409     c                   seton                                        285390
056400130409     c                   eval      V02msg  = $Msg(4)
056500130409     c                   leavesr
056600130409e   1c                   endif
056700130409
056800130409     c                   clear                   aTIBS02ds
056900130409     c                   eval      aT02mod = 'C'
057000130409     c                   eval      aT02cod = 'CNU'
057100130409     c                   eval      at02ke1 = V02cnu
057200130409     c                   movel     KNSIF         aT02sif
057300130409     c                   call      'TIBS02R'
057400130409     c                   parm                    KPJBA
057500130409     c                   parm                    aTIBS02ds
057600130409     c                   if        aT02err  <>  *blanks
057700130409     c                   seton                                        285390
057800130409     c                   eval      V02msg  = $Msg(4)
057900130409     c                   leavesr
058000130409     c                   endif
058100060403      *
058200060403     c                   ENDSR
058300060403
058400060403      *---------------------------------------------------------------*
058500060403      * GESW1  - Gestione videata dati relativi alla trasmissione     *
058600060403      *---------------------------------------------------------------*
058700060403     c     GesW1         BEGSR
058800060403      *
058900060403      * Inizializzazione videata
059000060403if  1c                   if        $InzW1 = *on
059100060403     c                   exsr      InzW1
059200060403     c                   movel     *off          $InzW1
059300060403e   1c                   endif
059400060403      *
059500130409     c                   exfmt     TBDKLW1
059600060403     c                   eval      *in90 = *off
059700060403     c                   clear                   W1MSG
059800060403      *
059900060403sel 1c                   select
060000060403      * F12=Ritorno
060100060403w   1c                   when      *inKL
060200060403     c                   exsr      F12W1
060300060403     c                   goto      EndGesW1
060400060403e   1c                   endsl
060500060403      *
060600060403      * Controllo dati immessi a video
060700060403     c                   exsr      CtrW1
060800060403      *
060900060403      * Aggiornamento se non ci sono errori
061000060403if  1c                   if        not *in90 and *inKF
061100060403     c                   exsr      AggTBE
061200060403e   1c                   endif
061300060403      *
061400060403     c     EndGesW1      ENDSR
061500060403
061600060403      *---------------------------------------------------------------*
061700060403      * INZW1  - Inzializzazione window (W1)                          *
061800060403      *---------------------------------------------------------------*
061900060403     c     InzW1         BEGSR
062000060403      *
062100060403     c                   movea     *zeros        *in(50)
062200060403      *
062300060403sel 1c                   select
062400060403      *
062500060403      * F5=Ripristino
062600060403w   1c                   when      *inKE   and  *in04
062700060403     c                   eval      W1FTT = TBEftt
062800060403      *
062900060403      * F6=Conferma
063000060403w   1c                   when      *inKF
063100060403sel 2c                   select
063200060403      *   Immissione
063300060403w   2c                   when      *in01
063400060403     c                   eval      W1FTT = TBXftt
063500060403      *   Modifica / Ripristino
063600060403w   2c                   when      *in02   or    *in04
063700060403     c                   eval      W1FTT = TBEftt
063800060403e   2c                   endsl
063900060403      *
064000060403      * F16=Annullamento
064100060403w   1c                   when      *inKQ   and  not *in04
064200060403     c                   eval      W1FTT = TBEftt
064300060403      *
064400060403e   1c                   endsl
064500060403      *
064600060403      * Se NON immissione: visualizzo i dati relativi all'ultima
064700060403      *   trasmissione
064800060403if  1c                   if        not *in01
064900060403     c                   eval      W1FLT = TBEflt
065000060403     c                   eval      W1FTR = TBEftr
065100060403if  2c                   if        TBEdtr <> 0
065200060403     c                   clear                   WLBDAT
065300060403     c                   z-add     TBEdtr        G02inv
065400060403     c                   movel     '3'           G02err
065500060403     c                   call      'XSRDA8'
065600060403     c                   parm                    WLBDAT
065700060403     c                   z-add     G02dat        W1DTR
065800060403e   2c                   endif
065900060403e   1c                   endif
066000060403      *
066100060403     c                   ENDSR
066200060403
066300060403      *---------------------------------------------------------------*
066400060403      * CTRW1  - Controllo e decodifica window                        *
066500060403      *---------------------------------------------------------------*
066600060403     c     CtrW1         BEGSR
066700060403      *
066800060403     c                   movea     *zeros        *in(50)
066900060403      *
067000060403     c     EndCtrW1      ENDSR
067100060403
067200060403      *---------------------------------------------------------------*
067300060403      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
067400060403      *---------------------------------------------------------------*
067500060403     c     F12W1         BEGSR
067600060403      *
067700060403     c                   eval      $TipVid = '2'
067800060403      *
067900060403     c                   ENDSR
068000060403
068100060403      *---------------------------------------------------------------*
068200060403      * CHNTBE * Aggancio tabella                                     *
068300060403      *---------------------------------------------------------------*
068400060403     c     ChnTBE        BEGSR
068500060403      *
068600060403     c                   movel     T02cod        TBEcod
068700130409     c                   movel(p)  V02ksc        TBEke1
068800060403     c                   clear                   TBEke2
068900060403     c                   clear                   TBElin
069000060403     c                   movel     KNSIF         TBEsif
069100060403     c     K05TBE01      chain     TNTBE01L
069200060403      * Se non ho trovato il record con il sistema informativo
069300060403      * che ho in linea, lo abblenco
069400060403if  1c                   if        not %found(TNTBE01L)
069500060403     c                   clear                   TBEsif
069600060403     c     K05TBE01      chain     TNTBE01L
069700060403e   1c                   endif
069800060403      *
069900060403     c                   ENDSR
070000060403
070100060403      *---------------------------------------------------------------*
070200060403      * AGGTBE * Aggiornamento tabella                                *
070300060403      *---------------------------------------------------------------*
070400060403     c     AggTBE        BEGSR
070500060403      *
070600060403sel 1c                   SELECT
070700060403      *
070800060403      * F5=Ripristino
070900060403w   1c                   WHEN      wTasto='05'  and  *in04
071000060403     c                   clear                   TBEatb
071100060525     c                   movel     W1ftt         TBEftt
071200060403     c                   clear                   TBEftr
071300060403     c                   UPDATE    TNTBE000
071400060403      *
071500060403      * F6=Conferma
071600060403w   1c                   WHEN      wTasto='06'
071700060403     c                   exsr      RieTBE
071800060403sel 2c                   select
071900060403      *   Immissione
072000060403w   2c                   when      *in01
072100060403     c                   clear                   TBEflt
072200060403     c                   clear                   TBEdtr
072300060403     c                   WRITE     TNTBE000                             22
072400060403      *   Modifica / Ripristino
072500060403w   2c                   when      *in02   or    *in04
072600060403     c                   UPDATE    TNTBE000
072700060403e   2c                   endsl
072800060403      *
072900060403      * F16=Annullamento
073000060403w   1c                   WHEN      wTasto='16'  and  not *in04
073100060403     c                   movel     'A'           TBEatb
073200060525     c                   movel     W1ftt         TBEftt
073300060403     c                   clear                   TBEftr
073400060403     c                   UPDATE    TNTBE000
073500060403      *
073600060403e   1c                   ENDSL
073700060403      *
073800060403      * Torno alla prima videata che carico come da inizio pgm
073900060403     c                   movel     '1'           $TipVid
074000060403     c                   movel     *on           $InzV1
074100060403     c                   movel     *on           $InzV2
074200060403     c                   movel     *on           $InzW1
074300060403     c
074400060403     c                   ENDSR
074500060403
074600060403      *---------------------------------------------------------------*
074700060403      * RIETBE * Riempimento dati tabella                             *
074800060403      *---------------------------------------------------------------*
074900060403     c     RieTBE        BEGSR
075000060403      *
075100060403     c                   clear                   TBEatb
075200060403     c                   if        TBXsif <> *blanks
075300060403     c                   movel     KNSIF         TBEsif
075400060403     c                   else
075500060403     c                   clear                   TBEsif
075600060403     c                   endif
075700060403     c                   movel     TBXapl        TBEapl
075800060403     c                   movel     T02cod        TBEcod
075900130409     c                   movel     V02ksc        TBEke1
076000060403     c                   clear                   TBEke2
076100060403      *
076200130409     c                   clear                   dDKL
076300130409     c                   eval      §DKLrag = V02kscd
076400130409     c                   eval      §DKLnca = V02nca
076500130409     c                   eval      §DKLcnu = %dec(V02cnu:3:0)
076600130409     c                   eval      §DKLcau = V02cau
076700130409     c                   movel(p)  dDKL          TBEuni
076800060403      *
076900060525     c                   movel     W1ftt         TBEftt
077000060403     c                   clear                   TBEflt
077100060403     c                   clear                   TBEftr
077200060403     c                   clear                   TBEdtr
077300060403      *
077400060403     c                   ENDSR
077500060403
077600060403** - $Opz ----*
077700060403  Inserimento
077800060403    Modifica
077900060403     Copia
078000060403  Annullamento
078100060403Visualizzazione
078200060403   ANNULLATO
078300060403** - $Msg -------------------------------------------------------------------*
078400130409Codice riferimento errato                                                      01
078500130409Numero MAX c.a. errato                                                         02
078600130409Causale chiusura c.a. errata                                                   03
078700130409Codice numeratore errato                                                       04
