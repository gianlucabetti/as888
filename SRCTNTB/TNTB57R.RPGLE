000100060606      *---------------------------------------------------------------*
000200060606      * Gestione tabella "KPI" = Obiettivi di produttività complessiva*
000300060606      *                          di un impianto automatico smistacolli*
000400060606      *---------------------------------------------------------------*
000500060606
000600060606     h decedit('0,') datedit(*dmy/)
000700060606
000800060606      *---------------------------------------------------------------*
000900060606      *   A R C H I V I                                               *
001000060606      *---------------------------------------------------------------*
001100060606      *
001200060606     fTNTBE01L  Uf A e           k disk
001300060606      *
001400060606     fTNTB57D   cf   e             workstn
001500060606
001600060606      *---------------------------------------------------------------*
001700060606      *   C O S T A N T I                                             *
001800060606      *---------------------------------------------------------------*
001900060606      *
002000060606     d C_CodTab        c                   const('KPI')
002100060606
002200060606      *---------------------------------------------------------------*
002300060606      *   S C H I E R E                                               *
002400060606      *---------------------------------------------------------------*
002500060606      *
002600060606     d $Opz            s             15    dim( 6) ctdata perrcd(1)             Decodifica OPZ
002700060606     d $Msg            s             78    dim( 5) ctdata perrcd(1)             Messaggi video
002800060606
002900060606      *---------------------------------------------------------------*
003000060606      *   S T R U T T U R E   D A T I                                 *
003100060606      *---------------------------------------------------------------*
003200060606      *
003300060606      * Parametri
003400060606     d KPJBA         e ds
003500060606      *
003600060606      * Passaggio Parametri al pgm TIBS02R
003700060606     d TIBS02ds      e ds                  inz
003800060606     d  T02mod       e                     inz('R')
003900060606     d  T02cod       e                     inz('KPI')
004000060606      *
004100060606      * Tabella KPI = Obiettivi di Produttività per un impianto
004200060606      *               automatico smistacolli
004300060606     d dKPI          e ds                  inz
004400060606      * Tabella MFP = P.O. applicazione principale
004500060606     d dMFP          e ds                  inz
004600060606      * Tabella MTP = Macchinone tipo applicazione
004700060606     d dMTP          e ds                  inz
004800060606      *
004900060606      * Tracciato record file TNTBE00F
005000060606     d TNTBEds       e ds                  extname(TNTBE00F) inz
005100060606     d xTNTBEds      e ds                  extname(TNTBE00F) inz
005200060606     d                                     prefix(TBX:3)
005300060606      *
005400060606     d TIBS34ds      e ds                  inz
005500060606     d dDatiUte      e ds                  inz
005600060606     d AZUTEds       e ds                  extname(AZUTE00F) inz
005700060606      *
005800060606     d WLBdat          ds                  inz
005900060606     d  G02dat                 1      8  0 inz
006000060606     d  G02inv                 9     16  0 inz
006100060606     d  G02err                17     17    inz('3')
006200060606     d  G02tgi                18     22  0 inz
006300060606      *
006400060606     d Status         sds
006500060606     d  VTCpgm           *proc
006600060606
006700060606      *---------------------------------------------------------------*
006800060606      *   V A R I A B I L I                                           *
006900060606      *---------------------------------------------------------------*
007000060606      *
007100060606     d $Called         s              1a   inz(*off)
007200060606     d $Fine           s              1a   inz(*off)
007300060606     d $InzV1          s              1a   inz(*on)
007400060606     d $InzV2          s              1a   inz(*on)
007500060606     d $InzW1          s              1a   inz(*on)
007600060606     d $TIBS02         s              1a   inz(*off)
007700060606     d $Video          s              1a   inz('1')
007800060606     d wTasto          s              2a   inz(*zeros)
007900060606     d wDate           s               d   datfmt(*iso) inz(*loval)
008000060606
008100060606      *---------------------------------------------------------------*
008200060606      *   M A I N   L I N E                                           *
008300060606      *---------------------------------------------------------------*
008400060606      *  Riepilogo indicatori utilizzati:                             *
008500060606      *  --------------------------------                             *
008600060606      *  01 - Record inesistente (inserimento)                        *
008700060606      *  02 - Record esistente   (modifica)                           *
008800060606      *  04 - Record annullato   (ripristino)                         *
008900060606      *  10 - Comodo                                                  *
009000060606      *  22 - Errori in scrittura record (WRITE)                      *
009100060606      *  28 - Emissione messaggio di errore a video                   *
009200060606      *  50 - ERR: Categoria F.V. errata                              *
009300060606      *  51 - ERR: Tipo Applicazione errato                           *
009400060606      *  52 - ERR: Descrizione mancante                               *
009500060606      *  53 - ERR: Produttività banco (colli/min.) errata             *
009600060606      *  54 - ERR: Scostamento limite errato                          *
009700060606      *  90 - Riemissione videata                                     *
009800060606      *---------------------------------------------------------------*
009900060606      *
010000060606      * Operazioni iniziali
010100060606     c                   exsr      RoutInz
010200060606      *
010300060606      * Gestione video
010400060606     c                   dow       $Fine   = *off
010500060606     c     $Video        caseq     '1'           GesV1
010600060606     c     $Video        caseq     '2'           GesV2
010700060606     c     $Video        caseq     'A'           GesW1
010800060606     c                   endcs
010900060606     c                   enddo
011000060606      *
011100060606      * Fine
011200060606     c                   if        $TIBS02 = *on
011300060606     c                   clear                   TIBS02ds
011400060606     c                   movel     'C'           T02tla
011500060606     c                   call      'TIBS02R'
011600060606     c                   parm                    KPJBA
011700060606     c                   parm                    TIBS02ds
011800060606     c                   endif
011900060606      *
012000060606     c                   movel     *on           *inLR
012100060606
012200060606      *---------------------------------------------------------------*
012300060606      * RoutInz - Operazioni Iniziali                                 *
012400060606      *---------------------------------------------------------------*
012500060606     c     RoutInz       BEGSR
012600060606      *
012700060606      * Ricezione parametri
012800060606     c     *entry        plist
012900060606     c                   parm                    KPJBA
013000060606      *
013100060606      * Definizioni chiavi di accesso
013200060606     c     K05TBE01      klist                                                  *tntbe01l
013300060606     c                   kfld                    TBEcod                         -tabella
013400060606     c                   kfld                    TBEke1                         -chiave uno
013500060606     c                   kfld                    TBEke2                         -chiave due
013600060606     c                   kfld                    TBElin                         -lingua
013700060606     c                   kfld                    TBEsif                         -s.informativo
013800060606      *
013900060606      * Reperisco la data odierna
014000060606     c                   movel     *date         Wdate
014100060606      *
014200060606      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
014300060606     c     *dtaara       define    §azute        AZUTEds
014400060606     c     *dtaara       define    §datiute      dDatiUte
014500060606      *
014600060606     c                   clear                   AZUTEds
014700060606     c                   clear                   dDatiUte
014800060606     c                   clear                   TIBS34ds
014900060606     c                   in(E)     *dtaara
015000060606if  1c                   if        %Error  or  RSUT = *blanks
015100060606     c                   call      'TIBS34R'
015200060606     c                   parm                    TIBS34ds
015300060606     c                   in        *dtaara
015400060606e   1c                   endif
015500060606      *-- Verifica errori e autorità profilo
015600060606sel 1c                   SELECT
015700060606      *-- controllo se ho errori nei dati utente
015800060606      *--   nel qual caso NON risulta un profilo abilitato
015900060606w   1c                   WHEN      DUTerr  = 'E'
016000060606     c                   eval      $Fine   = *on
016100060606      *
016200060606      *-- CONTROLLO AUTORITA'
016300060606      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
016400060606      *-- se il chiamante non richiede autorità specifica verificare
016500060606      *--   quella del profilo
016600060606      *-- se il chiamante richiede autorità specifica verificarla,
016700060606      *--  se è blank verificare quella del profilo
016800060606      *
016900060606      * se UTEAUT = *BLANK non siamo in sede
017000060606w   1c                   WHEN      UTEaut  = *blank
017100060606      *
017200060606x   1c                   OTHER
017300060606      *
017400060606e   1c                   ENDSL
017500060606      *
017600060606      * Aggancio dati generali della tabella in esame
017700060606     c                   clear                   TBEcod
017800060606     c                   move      *zeros        TBEke1
017900060606     c                   move      C_CodTab      TBEke1
018000060606     c                   clear                   TBEke2
018100060606     c                   clear                   TBElin
018200060606     c                   movel     KNSIF         TBEsif
018300060606     c     K05TBE01      chain     TNTBE01L
018400060606     c                   if        not %found(TNTBE01L)
018500060606     c                   clear                   TBEsif
018600060606     c     K05TBE01      chain     TNTBE01L
018700060606     c                   endif
018800060606     c                   if        %found(TNTBE01L)
018900060606     c                   movel     TNTBEds       xTNTBEds
019000060606     c                   else
019100060606     c                   clear                   xTNTBEds
019200060606     c                   endif
019300060606      *
019400060606      * Verifico se ho ricevuto parametri
019500060606if  1c                   if        %subst(KPJBU:1:7) = *blanks
019600060606     c                   leavesr
019700060606e   1c                   endif
019800060606      *
019900060606      * Controllo i parametri ricevuti
020000060606     c                   eval      $Called = *on
020100060606     c                   clear                   TB57V1
020200060606     c                   eval      V1Cnpg  = %subst(KPJBU :
020300060606     c                                              1 :
020400060606     c                                              %len(V1Cnpg))
020500060606     c                   eval      V1Capl  = %subst(KPJBU :
020600060606     c                                              %len(V1Cnpg)+1 :
020700060606     c                                              %len(V1Capl))
020800060606     c                   exsr      CtrV1
020900060606if  1c                   if        *in28
021000060606     c                   movel(p)  V1Dmsg        KPJBU
021100060606     c                   eval      $Fine   = *on
021200060606     c                   leavesr
021300060606x   1c                   else
021400060606     c                   eval      $Video  = '2'
021500060606e   1c                   endif
021600060606      *
021700060606     c                   ENDSR
021800060606
021900060606      *---------------------------------------------------------------*
022000060606      * GESV1  - Gestione videata selezione codice tabella            *
022100060606      *---------------------------------------------------------------*
022200060606     c     GesV1         BEGSR
022300060606      *
022400060606      * Inizializzazione videata
022500060606if  1c                   if        $InzV1  = *on
022600060606     c                   exsr      InzV1
022700060606     c                   movel     *off          $InzV1
022800060606e   1c                   endif
022900060606      *
023000060606      * Scrivo la testata
023100060606     c                   write     TB57T1
023200060606      *
023300060606     c                   exfmt     TB57V1
023400060606     c                   setoff                                       28  90
023500060606     c                   clear                   V1Dmsg
023600060606      *
023700060606sel 1c                   select
023800060606      *
023900060606      * F3=Fine
024000060606w   1c                   when      *inKC
024100060606     c                   exsr      F03V1
024200060606      *
024300060606      * Controllo dati immessi a video
024400060606x   1c                   other
024500060606     c                   exsr      CtrV1
024600060606      * - Rilevati Errori
024700060606if  2c                   if        *in90
024800060606     c                   leavesr
024900060606e   2c                   endif
025000060606      * - Passaggio alla videata di dettaglio
025100060606     c                   eval      $InzV2  = *on
025200060606     c                   eval      $Video  = '2'
025300060606      *
025400060606e   1c                   endsl
025500060606      *
025600060606     c                   ENDSR
025700060606
025800060606      *---------------------------------------------------------------*
025900060606      * INZV1  - Inizializzazione prima videata (V1)                  *
026000060606      *---------------------------------------------------------------*
026100060606     c     InzV1         BEGSR
026200060606      *
026300060606     c                   clear                   TB57V1
026400060606      *
026500060606      * Imposto il campo "Categoria F.V." per l'interrogazione
026600060606     c                   eval      V1Cnpg  =  '?'
026700060606      *
026800060606      * Imposto il campo "Tipo Applicazione" per l'interrogazione
026900060606     c                   clear                   dMFP
027000060606     c                   eval      $TIBS02 =  *on
027100060606     c                   clear                   TIBS02ds
027200060606     c                   eval      T02mod  =  'C'
027300060606     c                   eval      T02sif  =  KNSIF
027400060606     c                   eval      T02cod  =  'MFP'
027500060606     c                   movel(p)  DUTpou        T02ke1
027600060606     c                   call      'TIBS02R'
027700060606     c                   parm                    KPJBA
027800060606     c                   parm                    TIBS02ds
027900060606     c                   if        T02err  =  *blanks
028000060606     c                   movel     T02uni        dMFP
028100060606     c                   endif
028200060606     c                   eval      V1Capl  =  §MFPapl
028300060606      *
028400060606     c                   ENDSR
028500060606
028600060606      *---------------------------------------------------------------*
028700060606      * CTRV1  - Controllo e decodifica prima videata                 *
028800060606      *---------------------------------------------------------------*
028900060606     c     CtrV1         BEGSR
029000060606      *
029100060606     c                   movea     *zeros        *in(50)
029200060606      *
029300060606      * Categoria Foglio
029400060606      * - Ricerca:
029500060606     c     '?'           scan      V1Cnpg                                 10
029600060606if  1c                   if        *in10
029700060606     c                   eval      $TIBS02 =  *on
029800060606     c                   clear                   V1Cnpg
029900060606     c                   seton                                          5090
030000060606     c                   reset                   TIBS02ds
030100060606     c                   eval      T02sif  =  KNSIF
030200060606     c                   call      'TIBS02R'
030300060606     c                   parm                    KPJBA
030400060606     c                   parm                    TIBS02ds
030500060606if  2c                   if        T02err  = *blanks
030600060606     c                   movel     T02ke1        V1Cnpg
030700060606     c                   movel     T02ke2        V1Capl
030800060606x   2c                   else
030900060606     c                   seton                                        285090
031000060606     c                   movel     T02msg        V1Dmsg
031100060606     c                   leavesr
031200060606     c                   endif
031300060606e   1c                   endif
031400060606      * - Controllo:
031500060606     c                   if             V1Cnpg  <> '2'
031600060606     c                             and  V1Cnpg  <> '5'
031700060606     c                   seton                                        285090
031800060606     c                   eval      V1Dmsg  =  $Msg(1)
031900060606     c                   leavesr
032000060606     c                   endif
032100060606      *
032200060606      * Tipo Applicazione
032300060606      * - Ricerca:
032400060606     c     '?'           scan      V1Capl                                 10
032500060606if  1c                   if        *in10
032600060606     c                   eval      $TIBS02 =  *on
032700060606     c                   clear                   V1Capl
032800060606     c                   clear                   TIBS02ds
032900060606     c                   eval      T02mod  =  'R'
033000060606     c                   eval      T02sif  =  KNSIF
033100060606     c                   eval      T02cod  =  'MTP'
033200060606if  2c                   if            DUTlpo             <> 'S'
033300060606     c                             and %subst(DUTute:1:3) <> 'EDP'
033400060606     c                   movel     DUTpou        T02ke1
033500060606e   2c                   endif
033600060606     c                   call      'TIBS02R'
033700060606     c                   parm                    KPJBA
033800060606     c                   parm                    TIBS02ds
033900060606if  2c                   if        T02err  = *blanks
034000060606     c                   movel     T02uni        dMTP
034100060606     c                   movel     T02ke1        V1Capl
034200060606     c                   movel     §MTPdes       V1Dapl
034300060606     c                   seton                                          5190
034400060606x   2c                   else
034500060606     c                   seton                                        285190
034600060606     c                   movel     T02msg        V1Dmsg
034700060606     c                   leavesr
034800060606     c                   endif
034900060606e   1c                   endif
035000060606      * - Controllo:
035100060606      * - - effettiva immissione
035200060606      * - - decodifica
035300060606if  1c                   IF        V1Capl  <> *blanks
035400060606     c                   clear                   TIBS02ds
035500060606     c                   eval      T02mod  =  'C'
035600060606     c                   eval      T02sif  =  KNSIF
035700060606     c                   eval      T02cod  =  'MTP'
035800060606     c                   movel(p)  V1Capl        T02ke1
035900060606     c                   call      'TIBS02R'
036000060606     c                   parm                    KPJBA
036100060606     c                   parm                    TIBS02ds
036200060606if  2c                   if        T02err  = *blanks
036300060606     c                   movel     T02uni        dMTP
036400060606     c                   movel     §MTPdes       V1Dapl
036500060606x   2c                   else
036600060606     c                   seton                                        285190
036700060626     c***                movel     T02msg        V1Dmsg
036800060626     c                   eval      V1Dmsg  =  $Msg(2)
036900060606     c                   leavesr
037000060606e   2c                   endif
037100060606x   1c                   ELSE
037200060606     c                   clear                   V1Dapl
037300060606e   1c                   ENDIF
037400060606      *
037500060606     c                   ENDSR
037600060606
037700060606      *---------------------------------------------------------------*
037800060606      * F03V1  - Tasto funzionale F03 -> Fine programma               *
037900060606      *---------------------------------------------------------------*
038000060606     c     F03V1         BEGSR
038100060606      *
038200060606     c                   movel     *on           $Fine                          fine pgm
038300060606      *
038400060606     c                   ENDSR
038500060606
038600060606      *---------------------------------------------------------------*
038700060606      * GESV2  - Gestione videata dettaglio dati                      *
038800060606      *---------------------------------------------------------------*
038900060606     c     GesV2         BEGSR
039000060606      *
039100060606      * Inizializzazione videata
039200060606     c                   if        $InzV2  = *on
039300060606     c                   exsr      InzV2
039400060606     c                   move      *off          $InzV2
039500060606     c                   endif
039600060606      *
039700060606      * Scrivo la testata
039800060606     c                   write     TB57T1
039900060606      *
040000060606if  1***c                   if        *in05
040100060606     ***c                   write     TB57V2
040200060606     ***c                   exfmt     PROTECT
040300060606x   1***c                   else
040400060606     c                   exfmt     TB57V2
040500060606e   1***c                   endif
040600060606     c                   setoff                                       28  90
040700060606     c                   clear                   V1Dmsg
040800060606     c                   reset                   wTasto
040900060606      *
041000060606sel 1c                   select
041100060606      *
041200060606      * F3=Fine
041300060606w   1c                   when      *inKC
041400060606     c                   exsr      F03V1
041500060606      *
041600060606      * F12=Ritorno
041700060606w   1c                   when      *inKL
041800060606     c                   exsr      F12V2
041900060606      *
042000060606x   1c                   other
042100060606      * Controllo dati immessi a video
042200060606      * (non si fanno se richisto l'annullamento)
042300060606if  2c                   if        not *inKQ
042400060606     c                   exsr      CtrV2
042500060606e   2c                   endif
042600060606      * Aggiornamento se non ci sono errori
042700060606sel 2c                   select
042800060606      * - Rilevati errori
042900060606w   2c                   when      *in90
043000060606      * - F5=Ripristino
043100060606w   2c                   when      *inKE
043200060606     c                   eval      wTasto  = '05'
043300060606      * - F6=Conferma
043400060606w   2c                   when      *inKF
043500060606     c                   eval      wTasto  = '06'
043600060606      * - F16=Annullamento
043700060606w   2c                   when      *inKQ
043800060606     c                   eval      wTasto  = '16'
043900060606e   2c                   endsl
044000060606     ***c                   exsr      AggTBE
044100060606if  2c                   if        wTasto  > *zeros
044200060606     c                   eval      $InzW1  = *on
044300060606     c                   eval      $Video  = 'A'
044400060606e   2c                   endif
044500060606e   1c                   endsl
044600060606      *
044700060606     c                   ENDSR
044800060606
044900060606      *---------------------------------------------------------------*
045000060606      * INZV2  - Inizializzazione seconda videata (V2)                *
045100060606      *---------------------------------------------------------------*
045200060606     c     InzV2         BEGSR
045300060606      *
045400060606     c                   clear                   TB57V2
045500060606      *
045600060606      * reimposto il campo chiave (qui di solo output)
045700060606     c                   movel     V1Cnpg        V2Cnpg
045800060606     c                   movel     V1Capl        V2Capl
045900060606     c                   movel     V1Dapl        V2Dapl
046000060606      *
046100060606      * Aggancio la tabella, se trovo il codice sono in modifica
046200060606      * o ripristino (se record annullato), altrimenti in immissione
046300060606     c                   exsr      ChnTBE
046400060606      *
046500060606sel 1c                   SELECT
046600060606      * IMMISSIONE
046700060606w   1c                   WHEN      NOT %found(TNTBE01L)
046800060606     c                   eval      *in01   = *on
046900060606     c                   eval      T1opz   = $Opz(01)
047000060606      * MODIFICA
047100060606w   1c                   WHEN          %found(TNTBE01L)
047200060606     c                             and TBEatb  = *blanks
047300060606     c                   eval      *in02   = *on
047400060606     c                   eval      T1opz   = $Opz(02)
047500060606      * RIPRISTINO
047600060606w   1c                   WHEN          %found(TNTBE01L)
047700060606     c                             and TBEatb <> *blanks
047800060606     c                   eval      *in04   = *on
047900060606     c                   eval      T1opz   = $Opz(06)
048000060606e   1c                   ENDSL
048100060606      *
048200060606      * Impostazione campi a video
048300060606     c                   eval      V2Cdes  = §KPIdes
048400060606     c                   eval      V2Cpba  = §KPIpba
048500060606     c                   eval      V2Cpsc  = §KPIpsc
048600060606      *
048700060606      * Decodifiche
048800060606      *
048900060606     c                   ENDSR
049000060606
049100060606      *---------------------------------------------------------------*
049200060606      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
049300060606      *---------------------------------------------------------------*
049400060606     c     F12V2         BEGSR
049500060606      *
049600060606     c                   UNLOCK    TNTBE01L
049700060606      *
049800060606     c                   if        $Called = *off
049900060606      * - Ritorno alla videata precedente
050000060606     c                   eval      $Video  = '1'
050100060606     c                   else
050200060606      * - Uscita dal pgm
050300060606     c                   exsr      F03V1
050400060606     c                   endif
050500060606      *
050600060606     c                   ENDSR
050700060606
050800060606      *---------------------------------------------------------------*
050900060606      * CTRV2  - Controllo e decodifica seconda videata               *
051000060606      *---------------------------------------------------------------*
051100060606     c     CtrV2         BEGSR
051200060606      *
051300060606     c                   movea     *zeros        *in(50)
051400060606      *
051500060606     c                   select
051600060606      *
051700060606      * Descrizione obbligatoria
051800060606     c                   when      V2Cdes  =  *blanks
051900060606     c                   seton                                        285290
052000060606     c                   eval      V1Dmsg  =  $Msg(3)
052100060606      *
052200060606      * Produttività (colli/minuto) del banco obbligatoria
052300060606     c                   when      V2Cpba  <= *zeros
052400060606     c                   seton                                        285390
052500060606     c                   eval      V1Dmsg  =  $Msg(4)
052600060606      *
052700060606      * % scostamento limite obbligatoria
052800060606     c                   when      V2Cpsc  <= *zeros
052900060606     c                   seton                                        285490
053000060606     c                   eval      V1Dmsg  =  $Msg(5)
053100060606      *
053200060606     c                   endsl
053300060606      *
053400060606     c                   ENDSR
053500060606
053600060606      *---------------------------------------------------------------*
053700060606      * GESW1  - Gestione videata dati relativi alla trasmissione     *
053800060606      *---------------------------------------------------------------*
053900060606     c     GesW1         BEGSR
054000060606      *
054100060606      * Inizializzazione videata
054200060606if  1c                   if        $InzW1 = *on
054300060606     c                   exsr      InzW1
054400060606     c                   movel     *off          $InzW1
054500060606e   1c                   endif
054600060606      *
054700060606if  1***c                   if        *in05
054800060606     ***c                   write     TB57W1
054900060606     ***c                   exfmt     PROTECT
055000060606x   1***c                   else
055100060606     c                   exfmt     TB57W1
055200060606e   1***c                   endif
055300060606     c                   eval      *in90 = *off
055400060606     c                   clear                   W1MSG
055500060606      *
055600060606sel 1c                   select
055700060606      * F12=Ritorno
055800060606w   1c                   when      *inKL
055900060606     c                   exsr      F12W1
056000060606     c                   leavesr
056100060606e   1c                   endsl
056200060606      *
056300060606      * Controllo dati immessi a video
056400060606     c                   exsr      CtrW1
056500060606      *
056600060606      * Aggiornamento se non ci sono errori
056700060606if  1c                   if        not *in90 and *inKF
056800060606     c                   exsr      AggTBE
056900060606e   1c                   endif
057000060606      *
057100060606     c                   ENDSR
057200060606
057300060606      *---------------------------------------------------------------*
057400060606      * INZW1  - Inzializzazione window (W1)                          *
057500060606      *---------------------------------------------------------------*
057600060606     c     InzW1         BEGSR
057700060606      *
057800060606     c                   movea     *zeros        *in(50)
057900060606      *
058000060606sel 1c                   select
058100060606      *
058200060606      * F5=Ripristino
058300060606w   1c                   when      *inKE   and  *in04
058400060606     c                   eval      W1FTT = TBEftt
058500060606      *
058600060606      * F6=Conferma
058700060606w   1c                   when      *inKF
058800060606sel 2c                   select
058900060606      *   Immissione
059000060606w   2c                   when      *in01
059100060606     c                   eval      W1FTT = TBXftt
059200060606      *   Modifica / Ripristino
059300060606w   2c                   when      *in02   or    *in04
059400060606     c                   eval      W1FTT = TBEftt
059500060606e   2c                   endsl
059600060606      *
059700060606      * F16=Annullamento
059800060606w   1c                   when      *inKQ   and  not *in04
059900060606     c                   eval      W1FTT = TBEftt
060000060606      *
060100060606e   1c                   endsl
060200060606      *
060300060606      * Se NON immissione: visualizzo i dati relativi all'ultima
060400060606      *   trasmissione
060500060606if  1c                   if        not *in01
060600060606     c                   eval      W1FLT = TBEflt
060700060606     c                   eval      W1FTR = TBEftr
060800060606if  2c                   if        TBEdtr <> 0
060900060606     c                   clear                   WLBDAT
061000060606     c                   z-add     TBEdtr        G02inv
061100060606     c                   movel     '3'           G02err
061200060606     c                   call      'XSRDA8'
061300060606     c                   parm                    WLBDAT
061400060606     c                   z-add     G02dat        W1DTR
061500060606e   2c                   endif
061600060606e   1c                   endif
061700060606      *
061800060606     c                   ENDSR
061900060606
062000060606      *---------------------------------------------------------------*
062100060606      * CTRW1  - Controllo e decodifica window                        *
062200060606      *---------------------------------------------------------------*
062300060606     c     CtrW1         BEGSR
062400060606      *
062500060606     c                   movea     *zeros        *in(50)
062600060606      *
062700060606     c                   ENDSR
062800060606
062900060606      *---------------------------------------------------------------*
063000060606      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
063100060606      *---------------------------------------------------------------*
063200060606     c     F12W1         BEGSR
063300060606      *
063400060606     c                   eval      $Video  = '2'
063500060606      *
063600060606     c                   ENDSR
063700060606
063800060606      *---------------------------------------------------------------*
063900060606      * CHNTBE * Aggancio tabella                                     *
064000060606      *---------------------------------------------------------------*
064100060606     c     ChnTBE        BEGSR
064200060606      *
064300060606     c                   clear                   dKPI
064400060606      *
064500060606     c                   movel     'KPI'         TBEcod
064600060606     c                   movel(p)  V1Cnpg        TBEke1
064700060606     c                   movel(p)  V1Capl        TBEke2
064800060606     c                   clear                   TBElin
064900060606     c                   movel     KNSIF         TBEsif
065000060606     c     K05TBE01      chain     TNTBE01L
065100060606      * Se non ho trovato il record con il sistema informativo
065200060606      * che ho in linea, lo abblenco
065300060606if  1c                   if        not %found(TNTBE01L)
065400060606     c                   clear                   TBEsif
065500060606     c     K05TBE01      chain     TNTBE01L
065600060606e   1c                   endif
065700060606      *
065800060606     c                   if        %found(TNTBE01L)
065900060606     c                   movel     TBEuni        dKPI
066000060606     c                   endif
066100060606      *
066200060606     c                   ENDSR
066300060606
066400060606      *---------------------------------------------------------------*
066500060606      * AGGTBE * Aggiornamento tabella                                *
066600060606      *---------------------------------------------------------------*
066700060606     c     AggTBE        BEGSR
066800060606      *
066900060606sel 1c                   SELECT
067000060606      *
067100060606      * F5=Ripristino
067200060606w   1c                   WHEN      wTasto='05'  and  *in04
067300060606     c                   clear                   TBEatb
067400060606     c                   movel     W1ftt         TBEftt
067500060606     c                   clear                   TBEftr
067600060606     c                   UPDATE    TNTBE000
067700060606      *
067800060606      * F6=Conferma
067900060606w   1c                   WHEN      wTasto='06'
068000060606     c                   exsr      RieTBE
068100060606     c                   movel     W1ftt         TBEftt
068200060606sel 2c                   select
068300060606      *   Immissione
068400060606w   2c                   when      *in01
068500060606     c                   clear                   TBEflt
068600060606     c                   clear                   TBEdtr
068700060606     c                   WRITE     TNTBE000                             22
068800060606      *   Modifica / Ripristino
068900060606w   2c                   when      *in02   or    *in04
069000060606     c                   UPDATE    TNTBE000
069100060606e   2c                   endsl
069200060606      *
069300060606      * F16=Annullamento
069400060606w   1c                   WHEN      wTasto='16'  and  not *in04
069500060606     c                   movel     'A'           TBEatb
069600060606     c                   movel     W1ftt         TBEftt
069700060606     c                   clear                   TBEftr
069800060606     c                   UPDATE    TNTBE000
069900060606      *
070000060606e   1c                   ENDSL
070100060606      *
070200060606     c                   if        $Called = *off
070300060606      * - Ritorno alla prima videata che carico come da inizio pgm
070400060606     c                   movel     '1'           $Video
070500060606     c                   movel     *on           $InzV1
070600060606     c                   movel     *on           $InzV2
070700060606     c                   movel     *on           $InzW1
070800060606     c                   else
070900060606      * - Uscita dal pgm
071000060606     c                   exsr      F03V1
071100060606     c                   endif
071200060606     c
071300060606     c                   ENDSR
071400060606
071500060606      *---------------------------------------------------------------*
071600060606      * RIETBE * Riempimento dati tabella                             *
071700060606      *---------------------------------------------------------------*
071800060606     c     RieTBE        BEGSR
071900060606      *
072000060606     c                   clear                   TBEatb
072100060606     c                   if        TBXsif  <> *blanks
072200060606     c                   movel     KNSIF         TBEsif
072300060606     c                   else
072400060606     c                   clear                   TBEsif
072500060606     c                   endif
072600060606     c                   movel     TBXapl        TBEapl
072700060606     c                   movel     C_CodTab      TBEcod
072800060606     c                   movel(p)  V1Cnpg        TBEke1
072900060606     c                   movel(p)  V1Capl        TBEke2
073000060606      *
073100060606     c                   clear                   dKPI
073200060606     c                   eval      §KPIdes =  V2Cdes
073300060606     c                   eval      §KPIpba =  V2Cpba
073400060606     c                   eval      §KPIpsc =  V2Cpsc
073500060606     c                   movel(p)  dKPI          TBEuni
073600060606      *
073700060606     c                   clear                   TBEftt
073800060606     c                   clear                   TBEflt
073900060606     c                   clear                   TBEftr
074000060606     c                   clear                   TBEdtr
074100060606      *
074200060606     c                   ENDSR
074300060606
074400060606** - $Opz ----*
074500060606  Inserimento
074600060606    Modifica
074700060606     Copia
074800060606  Annullamento
074900060606Visualizzazione
075000060606   ANNULLATO
075100060606** - $Msg -------------------------------------------------------------------*
075200060606Categoria Foglio errata                                                        01
075300060606Tipo Applicazione errato                                                       02
075400060606Descrizione obbligatoria                                                       03
075500060606Produttività del banco (numero colli/minuto) errata o mancante                 04
075600060606Percentuale di scostamento errata o mancante                                   05
