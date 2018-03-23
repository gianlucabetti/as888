000100060410      *---------------------------------------------------------------*
000200110804      * Gestione tabella "FAB" = Forzatura AC BASE                    *
000300060410      *---------------------------------------------------------------*
000400060410
000500060410     h decedit('0,') datedit(*dmy/)
000600101007     h dftactgrp(*no)
000700101007     h bnddir('UBBNDDIR')
000800110804
000900110804      *?  ATTENZIONE!!  ?
001000110804      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
001100110804      *?    'Interrogazione abilitazioni clienti'              ?
001200110804      *?    In caso di aggiunta/modifica campi alla tabella    ?
001300110804      *?    verificare se sono validi per la visualizzazione   ?
001400110804      *?    da TA61                                            ?
001500060410
001600060410      *---------------------------------------------------------------*
001700060410      *   A R C H I V I                                               *
001800060410      *---------------------------------------------------------------*
001900060410      *
002000060410     fTABEL00F  if   e           k disk
002100060420     fAZORG01L  if   e           k disk
002200060410      *
002300060410     fTNTBE01L  Uf A e           k disk
002400101007     f                                     extfile(wLibFile)  usropn
002500060410      *
002600110804     fTNTB89D   cf   e             workstn
002700060410
002800060410      *---------------------------------------------------------------*
002900060410      *   C O S T A N T I                                             *
003000060410      *---------------------------------------------------------------*
003100060410      *
003200101007      * - Costante per controllo "caratteri solo numerici"
003300060410     d DigitN          c                   const('0123456789')
003400101007       //
003500101007       // -?Costanti per la definizione delle schiere con i nomi?
003600101007       //  ?degli iSeries da elaborare e delle relative librerie?
003700101007     d c_NrSyst        c                   const(2)
003800101007     d c_NrLibr        c                   const(2)
003900060410
004000060410      *---------------------------------------------------------------*
004100060410      *   S C H I E R E                                               *
004200060410      *---------------------------------------------------------------*
004300060410      *
004400101007       // -?iSeries  &  Librerie con entrambi i file tabelle?
004500101007     d $iSystem        s                   like(currSysNetA)
004600101007     d                                     dim(c_NrSyst)
004700101007     d                                     ctdata   perrcd( 1)
004800101007     d $SisInf         s                   like(ds_Libr)
004900101007     d                                     dim(c_NrSyst)
005000101007     d                                     alt($iSystem)
005100101007      *
005200101007      * - Decodifica opzioni
005300101007     d $Opz            s             15    dim( 6) ctdata perrcd(1)
005400101007      *
005500101007      * - Messaggi di errore
005600101007     d $Msg            s             78    dim( 7) ctdata perrcd(1)
005700060410
005800060410      *---------------------------------------------------------------*
005900060410      *   S T R U T T U R E   D A T I                                 *
006000060410      *---------------------------------------------------------------*
006100060410      *
006200060410      * Parametri
006300060410     d KPJBA         e ds
006400110804     d TNTA61ds      e ds
006500060410      *
006600060410      * Passaggio Parametri al pgm TIBS02R
006700060410     d TIBS02ds      e ds                  inz
006800060410     d  T02mod       e                     inz('R')
006900110804     d  T02cod       e                     inz('FAB')
007000060410      *
007100110804      * Tabella FAB
007200110804     d dFAB          e ds                  inz
007300060410      *
007400060410      * Tracciato record file TNTBE00F
007500060410     d TNTBEds       e ds                  extname(TNTBE00F) inz
007600060410     d xTNTBEds      e ds                  extname(TNTBE00F) inz
007700060410     d                                     prefix(TBX:3)
007800060410      *
007900060410     d TIBS34ds      e ds                  inz
008000060410     d dDatiUte      e ds                  inz
008100060410     d AZUTEds       e ds                  extname(AZUTE00F) inz
008200110811
008300110811       // - Controllo/Decodifica cliente
008400110811     d TIBS69ds      e ds                  inz
008500110811     d ds_CNACO      e ds                  extname(CNACO00F) inz
008600110811     d ds_CNIND      e ds                  extname(CNIND00F) inz
008700110811     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
008800110811     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
008900110804      *
009000110804     d WLBdat          ds                  inz
009100110804     d  G02dat                 1      8  0 inz
009200110804     d  G02inv                 9     16  0 inz
009300110804     d  G02err                17     17    inz('3')
009400110804     d  G02tgi                18     22  0 inz
009500060410      *
009600101007       //
009700101007       // -?Ridefinizione elenco librerie in elaborare le tabelle?
009800101007     d ds_Libr         ds                  inz
009900101007     d  $Libr                        10    dim(c_NrLibr) inz
010000060410      *
010100101007      * - Status ds
010200060410     d Status         sds
010300060410     d  VTCpgm           *proc
010400060410
010500060410      *---------------------------------------------------------------*
010600060410      *   V A R I A B I L I                                           *
010700060410      *---------------------------------------------------------------*
010800060410      *
010900101007      * - Flags booleani
011000101007     d $Called         s               n   inz(*off)
011100101007     d $Fine           s               n   inz(*off)
011200101007     d $InzV1          s               n   inz(*on)
011300101007     d $InzV2          s               n   inz(*on)
011400101007     d $InzW1          s               n   inz(*on)
011500110804     d $NoInt          s               n   inz(*on)
011600101007     d $TIBS02         s               n   inz(*off)
011700101007      * - Variabili per la gestione dei video
011800060410     d $Video          s              1a   inz('1')
011900060410     d wTasto          s              2a   inz(*zeros)
012000101007      * - Indici di schiera
012100101007     d xx              s              3  0 inz
012200101007      * - Campi di comodo
012300110804     d wDate           s               d   datfmt(*iso) inz(*loval)
012400060410     d WWWksc          s                   like(I69kac) inz
012500101007       // -?Nome del sistema?
012600101007     d currSysNeta     s              8a   inz
012700101007       // -?Nome esteso Libreria/File del file tabella?
012800101007     d wLibFile        s             21a   inz
012900101007       // -?Campi di comodo?
013000101007     d w_iSystem       s              1  0 inz
013100101007     d w_SisInf        s              3  0 inz
013200101007
013300101007       //--------------------------------------------------------------
013400101007       //?Definizione prototipi procedure.                             ?
013500101007       //--------------------------------------------------------------
013600101007
013700101007       // -?Reperimento NETA sistema AS/400 corrente?
013800101007      /copy gaitrasrc/srcProtoPr,UBRTVNETA
013900060410
014000060410      *---------------------------------------------------------------*
014100060410      *   M A I N   L I N E                                           *
014200060410      *---------------------------------------------------------------*
014300060410      *  Riepilogo indicatori utilizzati:                             *
014400060410      *  --------------------------------                             *
014500060410      *  01 - Record inesistente (inserimento)                        *
014600060410      *  02 - Record esistente   (modifica)                           *
014700060410      *  04 - Record annullato   (ripristino)                         *
014800060410      *  10 - Comodo                                                  *
014900110805      *  11 - OFF = Interrogazione da TA61                            *
015000060410      *  22 - Errori in scrittura record (WRITE)                      *
015100060410      *  28 - Emissione messaggio di errore a video                   *
015200060410      *  40 - Posizionamento curosre su campo V1DKSC x ricerca        *
015300060410      *  50 - ERR: Codice cliente                                     *
015400060410      *  90 - Riemissione videata                                     *
015500060410      *---------------------------------------------------------------*
015600060410      *
015700060410      * Operazioni iniziali
015800060410     c                   exsr      RoutInz
015900060410      *
016000060410      * Gestione video
016100060410     c                   dow       $Fine   = *off
016200060410     c     $Video        caseq     '1'           GesV1
016300060410     c     $Video        caseq     '2'           GesV2
016400060410     c     $Video        caseq     'A'           GesW1
016500060410     c                   endcs
016600060410     c                   enddo
016700060410      *
016800060410      * Fine
016900060410     c                   if        $TIBS02 = *on
017000060410     c                   clear                   TIBS02ds
017100060410     c                   movel     'C'           T02tla
017200060410     c                   call      'TIBS02R'
017300060410     c                   parm                    KPJBA
017400060410     c                   parm                    TIBS02ds
017500060410     c                   endif
017600060410      *
017700101007     c                   return
017800060410
017900060410      *---------------------------------------------------------------*
018000060410      * RoutInz - Operazioni Iniziali                                 *
018100060410      *---------------------------------------------------------------*
018200060410     c     RoutInz       BEGSR
018300060410      *
018400060410      * Ricezione parametri
018500060410     c     *entry        plist
018600060410     c                   parm                    KPJBA
018700110804     c                   parm                    TNTA61ds
018800060410      *
018900060410      * Definizioni chiavi di accesso
019000060410     c     K05TBE01      klist                                                  *tntbe01l
019100060410     c                   kfld                    TBEcod                         -tabella
019200060410     c                   kfld                    TBEke1                         -chiave uno
019300060410     c                   kfld                    TBEke2                         -chiave due
019400060410     c                   kfld                    TBElin                         -lingua
019500060410     c                   kfld                    TBEsif                         -s.informativo
019600060410      *
019700060410     c     K03TABEL      klist
019800060410     c                   kfld                    TBLkut
019900060410     c                   kfld                    TBLcod
020000060410     c                   kfld                    TBLkey
020100060410     c                   eval      TBLkut  =  1
020200060410     c                   clear                   TBLcod
020300060410     c                   movel     *zeros        TBEke1
020400101007      /free
020500101007
020600101007         *inLR = *on;
020700101007
020800101007         // -?Verifica del sistema AS/400 corrente?
020900101007         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
021000101007           $Fine = *on;
021100101007           leavesr;
021200101007         endif;
021300101007
021400101007         // -?Impostazione elenco librerie in cui gestire le tabelle?
021500101007         //  ?(a seconda del sistema in cui si stà lavorando)?
021600101007         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
021700101007         if  w_iSystem = *zero;
021800101007           $Fine = *on;
021900101007           leavesr;
022000101007         endif;
022100101007
022200101007         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
022300101007         w_SisInf = 1;
022400101007         exsr  sr_OpenFileTab;
022500101007
022600101007      /end-free
022700060410      *
022800060410      * Reperisco la data odierna
022900060410     c                   movel     *date         Wdate
023000060410      *
023100060410      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
023200060410     c     *dtaara       define    §azute        AZUTEds
023300060410     c     *dtaara       define    §datiute      dDatiUte
023400060410      *
023500060410     c                   clear                   AZUTEds
023600060410     c                   clear                   dDatiUte
023700060410     c                   clear                   TIBS34ds
023800060410     c                   in(E)     *dtaara
023900060410if  1c                   if        %Error  or  RSUT = *blanks
024000060410     c                   call      'TIBS34R'
024100060410     c                   parm                    TIBS34ds
024200060410     c                   in        *dtaara
024300060410e   1c                   endif
024400060410      *-- Verifica errori e autorità profilo
024500060410sel 1c                   SELECT
024600060410      *-- controllo se ho errori nei dati utente
024700060410      *--   nel qual caso NON risulta un profilo abilitato
024800060410w   1c                   WHEN      DUTerr  = 'E'
024900060410     c                   eval      $Fine   = *on
025000060410      *
025100060410      *-- CONTROLLO AUTORITA'
025200060410      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
025300060410      *-- se il chiamante non richiede autorità specifica verificare
025400060410      *--   quella del profilo
025500060410      *-- se il chiamante richiede autorità specifica verificarla,
025600060410      *--  se è blank verificare quella del profilo
025700060410      *
025800060410      * se UTEAUT = *BLANK non siamo in sede
025900060410w   1c                   WHEN      UTEaut  = *blank
026000060410      *
026100060410x   1c                   OTHER
026200060410      *
026300060410e   1c                   ENDSL
026400060410      *
026500060410      * Aggancio dati generali della tabella in esame
026600110914      * se non richiamato da interrogazione TA61
026700110914     c                   IF        %parms() = 1
026800060410     c                   clear                   TBEcod
026900060410     c                   move      *zeros        TBEke1
027000060410     c                   move      T02cod        TBEke1
027100060410     c                   clear                   TBEke2
027200060410     c                   clear                   TBElin
027300060410     c                   movel     KNSIF         TBEsif
027400060410     c     K05TBE01      chain     TNTBE01L
027500060410     c                   if        not %found(TNTBE01L)
027600060410     c                   clear                   TBEsif
027700060410     c     K05TBE01      chain     TNTBE01L
027800060410     c                   endif
027900060410     c                   if        %found(TNTBE01L)
028000060410     c                   movel     TNTBEds       xTNTBEds
028100060410     c                   else
028200060410     c                   clear                   xTNTBEds
028300060410     c                   endif
028400110914     c                   ENDIF
028500110804
028600110804      * Richiamato da interrogazione TA61
028700110804     c                   IF        %parms() > 1
028800110804     c                   clear                   TB89V1
028900110804     c                   eval      V1Cksc  = %editc(TA61ksc:'X')
029000110804     c                   exsr      CtrV1
029100110804     c                   IF        *in28
029200110804     c                   eval      TA61err = *on
029300110804     c                   eval      TA61msg = V1Dmsg
029400110804     c                   eval      $Fine = *on
029500110804     c                   leavesr
029600110804     c                   ENDIF
029700110804     c                   eval      V1Cksc = %editc(TA61ksc:'X')
029800110804     c                   eval      $NoInt  = *off
029900110804     c                   eval      $Called = *on
030000110804     c                   eval      $Video  = '2'
030100110804
030200110804      * Richiamato a modo vecchio (senza tnta61ds)
030300110804     c                   ELSE
030400060410      *
030500060410      * Verifico se ho ricevuto parametri
030600060410if  1c                   if        %subst(KPJBU:1:7) = *blanks
030700060410     c                   leavesr
030800060410e   1c                   endif
030900060410      *
031000060410      * Controllo i parametri ricevuti
031100060410     c                   eval      $Called = *on
031200110804     c                   clear                   TB89V1
031300060410     c                   eval      V1Cksc  = %subst(KPJBU:1:8)
031400060410     c                   exsr      CtrV1
031500060410if  1c                   if        *in28
031600060410     c                   movel(p)  V1Dmsg        KPJBU
031700060410     c                   eval      $Fine   = *on
031800060410     c                   leavesr
031900060410x   1c                   else
032000060410     c                   eval      $Video  = '2'
032100060410e   1c                   endif
032200110804
032300110804     c                   ENDIF
032400060410      *
032500060410     c                   ENDSR
032600101007      /free
032700101007
032800101007       //--------------------------------------------------------------
032900101007       //?Apertura dei files tabelle nel sistema informativo impostato.?
033000101007       //--------------------------------------------------------------
033100101007       BEGSR  sr_OpenFileTab;
033200101007
033300101007         // -?Chiusura (eventuale) archivio?
033400101007         if  %open(TNTBE01L);
033500101007           close  TNTBE01L;
033600101007         endif;
033700101007
033800101007         // -?Apertura archivio?
033900101007         ds_Libr  = $SisInf(w_iSystem);
034000101007         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
034100101007         open  TNTBE01L;
034200101007
034300101007       ENDSR;
034400101007
034500101007      /end-free
034600060410
034700060410      *---------------------------------------------------------------*
034800060410      * GESV1  - Gestione videata selezione codice tabella            *
034900060410      *---------------------------------------------------------------*
035000060410     c     GesV1         BEGSR
035100060410      *
035200060410      * Inizializzazione videata
035300060410if  1c                   if        $InzV1  = *on
035400060410     c                   exsr      InzV1
035500060410     c                   movel     *off          $InzV1
035600060410e   1c                   endif
035700060410      *
035800060410      * Scrivo la testata
035900110804     c                   write     TB89T1
036000060410      *
036100110804     c                   exfmt     TB89V1
036200060410     c                   setoff                                       28  90
036300060410     c                   clear                   V1Dmsg
036400060410      *
036500060410sel 1c                   select
036600060410      * F3=Fine
036700060410w   1c                   when      *inKC
036800060410     c                   exsr      F03V1
036900060410      * F7=Ricerca cliente
037000060410w   1c                   when           *inKG
037100060410     c                             and  *in40   = *off
037200060410     c                   eval      *in40   = *on
037300060410     c                   eval      *in50   = *off
037400060411     c                   move      V1Cksc        WWWksc
037500060411     c                   clear                   V1Cksc
037600060410w   1c                   when           *in40
037700060410     c                             and (V1Cksc  = *blanks
037800060410     c                              or  V1Cksc  = *zeros)
037900060410     c                   exsr      SearchCLI
038000060410     c                   eval      *in40   = *off
038100060410     c                   eval      *in50   = *on
038200060410      *
038300060410      * Controllo dati immessi a video
038400060410x   1c                   other
038500060410     c                   exsr      CtrV1
038600060410      * - Rilevati Errori
038700060410if  2c                   if        *in90
038800060410     c                   leavesr
038900060410e   2c                   endif
039000060410      * - Passaggio alla videata di dettaglio
039100060410     c                   eval      $InzV2  = *on
039200060410     c                   eval      $Video  = '2'
039300060410      *
039400060410e   1c                   endsl
039500060410      *
039600060410     c                   ENDSR
039700060410
039800060410      *---------------------------------------------------------------*
039900060410      * INZV1  - Inizializzazione prima videata (V1)                  *
040000060410      *---------------------------------------------------------------*
040100060410     c     InzV1         BEGSR
040200060410      *
040300110804     c                   clear                   TB89V1
040400060410      *
040500060410      * Imposto il campo "codice cliente" per l'interrogazione
040600060410     c                   move      '?'           V1Cksc
040700060410      *
040800060410     c                   ENDSR
040900060410
041000060410      *---------------------------------------------------------------*
041100060410      * CTRV1  - Controllo e decodifica prima videata                 *
041200060410      *---------------------------------------------------------------*
041300060410     c     CtrV1         BEGSR
041400060410      *
041500060410     c                   movea     *zeros        *in(50)
041600060410      *
041700060410      * Codice cliente
041800060410      * - Ricerca:
041900060410     c     '?'           scan      V1Cksc                                 10
042000060410if  1c                   if        *in10
042100060410     c                   eval      $TIBS02 = *on
042200060410     c                   clear                   V1Cksc
042300060410     c                   seton                                          5090
042400060410     c                   reset                   TIBS02ds
042500060410     c                   movel     KNSIF         T02sif
042600060410if  2c                   if            DUTlpo             <> 'S'
042700060410     c                             and %subst(DUTute:1:3) <> 'EDP'
042800060410     c                   movel     DUTpou        T02ke1
042900060410e   2c                   endif
043000060410     c                   call      'TIBS02R'
043100060410     c                   parm                    KPJBA
043200060410     c                   parm                    TIBS02ds
043300060410if  2c                   if        T02err  = *blanks
043400110804     c                   movel     T02uni        dFAB
043500060410     c                   movel     T02ke1        V1Cksc
043600060410x   2c                   else
043700060410     c                   seton                                        285090
043800060410     c                   movel     T02msg        V1Dmsg
043900060410     c                   leavesr
044000060410     c                   endif
044100060410e   1c                   endif
044200060410      * - Controllo:
044300060410      * - - effettiva immissione
044400060410if  1c                   if            V1Cksc  = *blanks
044500060410     c                             or  V1Cksc  = *zeros
044600060410     c                   seton                                        285090
044700060410     c                   eval      V1Dmsg  = $Msg(1)
044800060410     c                   leavesr
044900060410e   1c                   endif
045000060410      * - - numericità
045100060410     c     DigitN        check     V1Cksc                                 10
045200060410if  1c                   if        *in10   =  *on
045300060410     c                   seton                                        285090
045400060410     c                   eval      V1Dmsg  = $Msg(2)
045500060410     c                   leavesr
045600060410e   1c                   endif
045700060410      * - - decodifica
045800060410     c                   move      V1Cksc        WWWksc
045900060410     c                   exsr      DecodCLI
046000060410     c                   movel     ACOrag        V1Dksc
046100110804      * il prossimo controllo non lo faccio se interrogazione
046200110804     c                   IF        not *in11
046300110804     c                   leavesr
046400110804     c                   ENDIF
046500110804
046600060410      * - - "appartenenza" al P.O. dell'utente
046700060410     c                   if            %subst(V1Cksc:1:3) <> %char(DUTpou)
046800060410     c                             and DUTlpo             <> 'S'
046900060410     c                             and %subst(DUTute:1:3) <> 'EDP'
047000060410     c                   seton                                        285090
047100060410     c                   eval      V1Dmsg  = $Msg(2)
047200060410     c                   leavesr
047300060410     c                   endif
047400060410      *
047500060410     c                   ENDSR
047600060410
047700060410      *---------------------------------------------------------------*
047800060410      * SearchCLI  * Ricerca cliente                                  *
047900060410      *---------------------------------------------------------------*
048000060410     c     SearchCLI     BEGSR
048100060410      *
048200060411     c                   movel(p)  V1Dksc        PAXdmt
048300060410     c                   call      'XALFA3BR'
048400060410     c                   parm      RSUT          PAXdut           30
048500060410     c                   parm                    TBLkut
048600060411     c                   parm                    PAXdmt           48
048700060410     c                   parm      DUTkci        PAXccc            4 0
048800060410     c                   parm      9             PAXsta            1 0
048900060410     c                   parm                    PAXflr           90
049000060410     c                   parm      *blanks       PAXdit            3
049100060410     c                   parm      1             PAXnum            2 0
049200060410     c                   parm                    PAXksc           80
049300060410     c                   parm                    PAXksm          140
049400060410     c                   parm                    PAXkdm           60
049500060410      *
049600060410      *   · Imposto il codice selezionato a video
049700060411      *     (se selezionato e non premuto F12 - nel qual caso
049800060411      *      ripristino, invece, il codice savlavto)
049900060410if  1c                   if        PAXsta >= *zeros
050000060410     c                   movel     PAXksm        V1Cksc
050100060410     c                   movel     PAXdmt        V1Dksc
050200060411x   1c                   else
050300060411     c                   move      WWWksc        V1Cksc
050400060410e   1c                   endif
050500060410      *
050600060410     c                   ENDSR
050700060410
050800060410      *---------------------------------------------------------------*
050900060410      * DecodCLI   * Decodifica codice cliente                        *
051000060410      *---------------------------------------------------------------*
051100060410     c     DecodCLI      BEGSR
051200060410      *
051300060410     c                   clear                   TIBS69ds
051400060410     c                   clear                   ds_CNACO
051500060410     c                   clear                   ds_CNIND
051600060410     c                   clear                   ds_CNCLP
051700060410     c                   clear                   ds_FNCLS
051800060410     c                   eval      I69kac  = WWWksc
051900060410     c                   call      'TIBS69R'
052000060410     c                   parm                    TIBS69ds
052100060410     c                   parm                    ds_CNACO
052200060410     c                   parm                    ds_CNIND
052300060410     c                   parm                    ds_CNCLP
052400060410     c                   parm                    ds_FNCLS
052500060410      *
052600060410     c                   if        O69err <> *blanks
052700060410     c                   seton                                        285090
052800110811     c                   eval      V1Dmsg  = O69msg
052900060410     C                   endif
053000060410      *
053100060410     c                   ENDSR
053200060410
053300060410      *---------------------------------------------------------------*
053400060410      * F03V1  - Tasto funzionale F03 -> Fine programma               *
053500060410      *---------------------------------------------------------------*
053600060410     c     F03V1         BEGSR
053700060410      *
053800060410     c                   movel     *on           $Fine                          fine pgm
053900060410      *
054000060410     c                   ENDSR
054100060410
054200060410      *---------------------------------------------------------------*
054300060410      * GESV2  - Gestione videata dettaglio dati                      *
054400060410      *---------------------------------------------------------------*
054500060410     c     GesV2         BEGSR
054600060410      *
054700060410      * Inizializzazione videata
054800060410     c                   if        $InzV2  = *on
054900060410     c                   exsr      InzV2
055000060410     c                   move      *off          $InzV2
055100060410     c                   endif
055200060410      *
055300060410      * Scrivo la testata
055400110804     c                   write     TB89T1
055500101007      *
055600110804     c                   exfmt     TB89V2
055700101007      *
055800060410     c                   setoff                                       28  90
055900060410     c                   clear                   V1Dmsg
056000060410     c                   reset                   wTasto
056100060410      *
056200060410sel 1c                   select
056300060420      * F3=Fine
056400060410w   1c                   when      *inKC
056500060410     c                   exsr      F03V1
056600060410      * F12=Ritorno
056700060410w   1c                   when      *inKL
056800060410     c                   exsr      F12V2
056900060410      *
057000060410x   1c                   other
057100060410      * Controllo dati immessi a video
057200060410      * (non si fanno se richisto l'annullamento)
057300060410if  2c                   if        not *inKQ
057400060410     c                   exsr      CtrV2
057500060410e   2c                   endif
057600060410      * Aggiornamento se non ci sono errori
057700060410sel 2c                   select
057800060410      * - Rilevati errori
057900060410w   2c                   when      *in90
058000060410      * - F5=Ripristino
058100060410w   2c                   when      *inKE
058200060410     c                   eval      wTasto  = '05'
058300060410      * - F6=Conferma
058400060410w   2c                   when      *inKF
058500060410     c                   eval      wTasto  = '06'
058600060410      * - F16=Annullamento
058700060410w   2c                   when      *inKQ
058800060410     c                   eval      wTasto  = '16'
058900060410e   2c                   endsl
059000060410if  2c                   if        wTasto  > *zeros
059100101007       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
059200101007       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
059300101007       //  ?NON ha più senso gestire la richiesta di trasmissione!?
059400101007     ***c                   eval      $InzW1  = *on
059500101007     ***c                   eval      $Video  = 'A'
059600101007     c                   exsr      AggTBE
059700060410e   2c                   endif
059800060410e   1c                   endsl
059900060410      *
060000060410     c                   ENDSR
060100060410
060200060410      *---------------------------------------------------------------*
060300060410      * INZV2  - Inizializzazione seconda videata (V2)                *
060400060410      *---------------------------------------------------------------*
060500060410     c     InzV2         BEGSR
060600060410      *
060700110804     c                   clear                   TB89V2
060800110804     c                   eval      *in11 = $NoInt
060900060410      *
061000060410      * reimposto il campo chiave (qui di solo output)
061100060410     c                   movel     V1Cksc        V2Cksc
061200060410     c                   movel     V1Dksc        V2Dksc
061300101007      /free
061400101007
061500101007         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
061600101007         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
061700101007           w_SisInf = 1;
061800101007           exsr  sr_OpenFileTab;
061900101007         endif;
062000101007
062100101007      /end-free
062200060410      *
062300060410      * Aggancio la tabella, se trovo il codice sono in modifica
062400060410      * o ripristino (se record annullato), altrimenti in immissione
062500060410     c                   exsr      ChnTBE
062600060410      *
062700060410sel 1c                   SELECT
062800060410      * IMMISSIONE
062900060410w   1c                   WHEN      NOT %found(TNTBE01L)
063000060410     c                   eval      *in01   = *on
063100060410     c                   eval      T1opz   = $Opz(01)
063200060410      * MODIFICA
063300060410w   1c                   WHEN          %found(TNTBE01L)
063400060410     c                             and TBEatb  = *blanks
063500060410     c                   eval      *in02   = *on
063600060410     c                   eval      T1opz   = $Opz(02)
063700060410      * RIPRISTINO
063800060410w   1c                   WHEN          %found(TNTBE01L)
063900060410     c                             and TBEatb <> *blanks
064000060410     c                   eval      *in04   = *on
064100060410     c                   eval      T1opz   = $Opz(06)
064200060410e   1c                   ENDSL
064300060410      *
064400060410      * Impostazione campi a video
064500110804     c                   eval      V2Citr = d§FABitr
064600060410      *
064700060410      * Decodifiche
064800060410     c                   exsr      CtrV2
064900060410     c                   setoff                                       28  90
065000060410     c                   movea     *zeros        *in(50)
065100060410     c                   clear                   V1Dmsg
065200060410      *
065300060410     c                   ENDSR
065400060410
065500060410      *---------------------------------------------------------------*
065600060410      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
065700060410      *---------------------------------------------------------------*
065800060410     c     F12V2         BEGSR
065900060410      *
066000060410     c                   UNLOCK    TNTBE01L
066100060410      *
066200060410     c                   if        $Called = *off
066300060410      * - Ritorno alla videata precedente
066400060410     c                   eval      $Video  = '1'
066500060410     c                   else
066600060410      * - Uscita dal pgm
066700060410     c                   exsr      F03V1
066800060410     c                   endif
066900060410      *
067000060410     c                   ENDSR
067100060410
067200060410      *---------------------------------------------------------------*
067300060410      * CTRV2  - Controllo e decodifica seconda videata               *
067400060410      *---------------------------------------------------------------*
067500060410     c     CtrV2         BEGSR
067600060410      *
067700060410     c                   movea     *zeros        *in(50)
067800110804
067900110804     c                   IF        V2Citr = 0
068000110804     c                   seton                                        285190
068100110804     c                   eval      V1Dmsg  = $Msg(3)
068200110804     c                   leavesr
068300110804     c                   ENDIF
068400060410      *
068500060410     c                   ENDSR
068600060410
068700060410      *---------------------------------------------------------------*
068800060410      * GESW1  - Gestione videata dati relativi alla trasmissione     *
068900060410      *---------------------------------------------------------------*
069000060410     c     GesW1         BEGSR
069100060410      *
069200060410      * Inizializzazione videata
069300060410if  1c                   if        $InzW1 = *on
069400060410     c                   exsr      InzW1
069500060410     c                   movel     *off          $InzW1
069600060410e   1c                   endif
069700060410      *
069800110804     c                   exfmt     TB89W1
069900060410     c                   eval      *in90 = *off
070000060410     c                   clear                   W1MSG
070100060410      *
070200060410sel 1c                   select
070300060410      * F12=Ritorno
070400060410w   1c                   when      *inKL
070500060410     c                   exsr      F12W1
070600060410     c                   leavesr
070700060410e   1c                   endsl
070800060410      *
070900060410      * Controllo dati immessi a video
071000060410     c                   exsr      CtrW1
071100060410      *
071200060410      * Aggiornamento se non ci sono errori
071300060410if  1c                   if        not *in90 and *inKF
071400060410     c                   exsr      AggTBE
071500060410e   1c                   endif
071600060410      *
071700060410     c                   ENDSR
071800060410
071900060410      *---------------------------------------------------------------*
072000060410      * INZW1  - Inzializzazione window (W1)                          *
072100060410      *---------------------------------------------------------------*
072200060410     c     InzW1         BEGSR
072300060410      *
072400060410     c                   movea     *zeros        *in(50)
072500060410      *
072600060410sel 1c                   select
072700060410      *
072800060410      * F5=Ripristino
072900060410w   1c                   when      *inKE   and  *in04
073000060410     c                   eval      W1FTT = TBEftt
073100060410      *
073200060410      * F6=Conferma
073300060410w   1c                   when      *inKF
073400060410sel 2c                   select
073500060410      *   Immissione
073600060410w   2c                   when      *in01
073700060410     c                   eval      W1FTT = TBXftt
073800060410      *   Modifica / Ripristino
073900060410w   2c                   when      *in02   or    *in04
074000060410     c                   eval      W1FTT = TBEftt
074100060410e   2c                   endsl
074200060410      *
074300060410      * F16=Annullamento
074400060410w   1c                   when      *inKQ   and  not *in04
074500060410     c                   eval      W1FTT = TBEftt
074600060410      *
074700060410e   1c                   endsl
074800060410      *
074900060410      * Se NON immissione: visualizzo i dati relativi all'ultima
075000060410      *   trasmissione
075100060410if  1c                   if        not *in01
075200060410     c                   eval      W1FLT = TBEflt
075300060410     c                   eval      W1FTR = TBEftr
075400060410if  2c                   if        TBEdtr <> 0
075500060410     c                   clear                   WLBDAT
075600060410     c                   z-add     TBEdtr        G02inv
075700060410     c                   movel     '3'           G02err
075800060410     c                   call      'XSRDA8'
075900060410     c                   parm                    WLBDAT
076000060410     c                   z-add     G02dat        W1DTR
076100060410e   2c                   endif
076200060410e   1c                   endif
076300060410      *
076400060410     c                   ENDSR
076500060410
076600060410      *---------------------------------------------------------------*
076700060410      * CTRW1  - Controllo e decodifica window                        *
076800060410      *---------------------------------------------------------------*
076900060410     c     CtrW1         BEGSR
077000060410      *
077100060410     c                   movea     *zeros        *in(50)
077200060410      *
077300060411     c                   ENDSR
077400060410
077500060410      *---------------------------------------------------------------*
077600060410      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
077700060410      *---------------------------------------------------------------*
077800060410     c     F12W1         BEGSR
077900060410      *
078000060410     c                   eval      $Video  = '2'
078100060410      *
078200060410     c                   ENDSR
078300060410
078400060410      *---------------------------------------------------------------*
078500060410      * CHNTBE * Aggancio tabella                                     *
078600060410      *---------------------------------------------------------------*
078700060410     c     ChnTBE        BEGSR
078800060410      *
078900110804     c                   clear                   dFAB
079000060410      *
079100060410     c                   movel     T02cod        TBEcod
079200060410     c                   movel(p)  V1Cksc        TBEke1
079300060410     c                   clear                   TBEke2
079400060410     c                   clear                   TBElin
079500060410     c                   movel     KNSIF         TBEsif
079600060410     c     K05TBE01      chain     TNTBE01L
079700060410      * Se non ho trovato il record con il sistema informativo
079800060410      * che ho in linea, lo abblenco
079900060410if  1c                   if        not %found(TNTBE01L)
080000060410     c                   clear                   TBEsif
080100060410     c     K05TBE01      chain     TNTBE01L
080200060410e   1c                   endif
080300060410      *
080400060410     c                   if        %found(TNTBE01L)
080500110804     c                   movel     TBEuni        dFAB
080600060410     c                   endif
080700060410      *
080800060410     c                   ENDSR
080900060410
081000060410      *---------------------------------------------------------------*
081100060410      * AGGTBE * Aggiornamento tabella                                *
081200060410      *---------------------------------------------------------------*
081300060410     c     AggTBE        BEGSR
081400060410      *
081500101007      /free
081600101007
081700101007         // -?Ciclo di elaborazione per ogni sistema informativo?
081800101007         For  w_SisInf = 1  To  %elem($Libr);
081900101007
082000101007           // -?Apertura (eventuale) del file ed aggancio record?
082100101007           if  w_SisInf > 1;
082200101007             exsr  sr_OpenFileTab;
082300101007             exsr  ChnTBE;
082400101007           endif;
082500101007
082600101007         //? N.B.                                                      ?
082700101007         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
082800101007         //   trasmissione (vedi flag TBEFTR).                        ?
082900101007         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
083000101007         //   subito nello stesso file di entrambi i S.I. - in due    ?
083100101007         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
083200101007
083300101007      /end-free
083400060410sel 1c                   SELECT
083500060410      *
083600060410      * F5=Ripristino
083700060410w   1c                   WHEN      wTasto='05'  and  *in04
083800101007      /free
083900101007               if  %found(TNTBE01L);
084000101007      /end-free
084100060410     c                   clear                   TBEatb
084200060410     c                   clear                   TBEftr
084300060410     c                   UPDATE    TNTBE000
084400101007      /free
084500101007               else;
084600101007                 // - Record appena cancellato fisicamente
084700101007                 exsr  RieTBE;
084800101007                 WRITE  TNTBE000;
084900101007               endif;
085000101007      /end-free
085100060410      *
085200060410      * F6=Conferma
085300060410w   1c                   WHEN      wTasto='06'
085400060410     c                   exsr      RieTBE
085500060410sel 2c                   select
085600060410      *   Immissione
085700060410w   2c                   when      *in01
085800060410     c                   WRITE     TNTBE000                             22
085900060410      *   Modifica / Ripristino
086000060410w   2c                   when      *in02   or    *in04
086100060410     c                   UPDATE    TNTBE000
086200060410e   2c                   endsl
086300060410      *
086400060410      * F16=Annullamento
086500060410w   1c                   WHEN      wTasto='16'  and  not *in04
086600060410     c                   movel     'A'           TBEatb
086700060410     c                   clear                   TBEftr
086800060410     c                   UPDATE    TNTBE000
086900060410      *
087000060410e   1c                   ENDSL
087100101007      /free
087200101007
087300101007         EndFor;
087400101007
087500101007      /end-free
087600060410      *
087700060410     c                   if        $Called = *off
087800060410      * - Ritorno alla prima videata che carico come da inizio pgm
087900060410     c                   movel     '1'           $Video
088000060410     c                   movel     *on           $InzV1
088100060410     c                   movel     *on           $InzV2
088200060410     c                   movel     *on           $InzW1
088300060410     c                   else
088400060410      * - Uscita dal pgm
088500060410     c                   exsr      F03V1
088600060410     c                   endif
088700060410     c
088800060410     c                   ENDSR
088900060410
089000060410      *---------------------------------------------------------------*
089100060410      * RIETBE * Riempimento dati tabella                             *
089200060410      *---------------------------------------------------------------*
089300060410     c     RieTBE        BEGSR
089400060410      *
089500060410     c                   clear                   TBEatb
089600060410     c                   if        TBXsif <> *blanks
089700060410     c                   movel     KNSIF         TBEsif
089800060410     c                   else
089900060410     c                   clear                   TBEsif
090000060410     c                   endif
090100060410     c                   movel     TBXapl        TBEapl
090200060410     c                   movel     T02cod        TBEcod
090300060410     c                   movel     V1Cksc        TBEke1
090400060410     c                   clear                   TBEke2
090500060410      *
090600110804     c                   clear                   dFAB
090700110804     c                   eval      d§FABitr = V2Citr
090800110804     c                   movel(p)  dFAB          TBEuni
090900060410      *
091000101007     ***c                   clear                   TBEftt
091100101007     ***c                   clear                   TBEflt
091200101007     ***c                   clear                   TBEftr
091300101007     ***c                   clear                   TBEdtr
091400101007      /free
091500101007
091600101007         TBEftt = 'S';
091700101007         TBEflt = TBXflt;
091800101007
091900101007         select;
092000101007           // - Annullamento o Ripristino
092100101007           when  (wTasto='05'  and  *in04)   or
092200101007                 (wTasto='16'  and  not *in04);
092300101007             clear  TBEftr;
092400101007             clear  TBEdtr;
092500101007           // - Aggiornamento o Scrittura in sede
092600101007           when  w_SisInf = 1;
092700101007             TBEftr = 'T';
092800101007             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
092900101007                                     : 1 : 8 ) );
093000101007           // - Aggiornamento o Scrittura in filiale
093100101007           other;
093200101007             TBEftr = 'R';
093300101007             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
093400101007                                     : 1 : 8 ) );
093500101007         endsl;
093600101007
093700101007      /end-free
093800060410      *
093900060410     c                   ENDSR
094000060410
094100101007       //--------------------------------------------------------------
094200101007       //?Schiere a tempo di compilazione.                             ?
094300101007       //--------------------------------------------------------------
094400101007
094500101007** - $iSystem / $SisInf:?Sistemi AS/400 & Librerie con il file TABEL00F?
094600101007SETRAS  GAITRAGRU FILTRAGRU
094700101007AS888   GAITRAGRPSFILTRAGRPF
094800060410** - $Opz ----*
094900060410  Inserimento
095000060410    Modifica
095100060410     Copia
095200060410  Annullamento
095300060410Visualizzazione
095400060410   ANNULLATO
095500060410** - $Msg -------------------------------------------------------------------*
095600060410Immettere il codice cliente                                                    01
095700060410Codice cliente errato                                                          02
095800110804Immettere l'importo                                                            03
