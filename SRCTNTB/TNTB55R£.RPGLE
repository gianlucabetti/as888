000100060410      *---------------------------------------------------------------*
000200060410      * Gestione tabella "DSC" = Distinte Consegne PC - WFDSC         *
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
002600060410     fTNTB55D   cf   e             workstn
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
006900060410     d  T02cod       e                     inz('DSC')
007000060410      *
007100060410      * Tabella DSC = Distinte Consegne PC - WFDSC
007200060410     d dDSC          e ds                  inz
007300060410      *
007400060410      * Tracciato record file TNTBE00F
007500060410     d TNTBEds       e ds                  extname(TNTBE00F) inz
007600060410     d xTNTBEds      e ds                  extname(TNTBE00F) inz
007700060410     d                                     prefix(TBX:3)
007800060410      *
007900060410     d TIBS34ds      e ds                  inz
008000060410     d dDatiUte      e ds                  inz
008100060410     d AZUTEds       e ds                  extname(AZUTE00F) inz
008200060410      *
008300060410      * Decodifica anagrafica clienti
008400060410     d TIBS69ds      e ds                  inz
008500060410     d ds_CNACO      e ds                  extname(CNACO00F) inz
008600060410     d ds_CNIND      e ds                  extname(CNIND00F) inz
008700060410     d ds_CNCLP      e ds                  extname(CNCLP00F) inz
008800060410     d ds_FNCLS      e ds                  extname(FNCLS00F) inz
008900060410      *
009000060410      * Tabella 3T  = Tipo Scarico
009100060410     d ds3T          e ds                  inz
009200060420      *
009300060420     d dsV2CPO         ds                  inz
009400060420     d  V2Cpo001
009500060420     d  V2Cpo002
009600060420     d  V2Cpo003
009700060420     d  V2Cpo004
009800060420     d  V2Cpo005
009900060420     d  V2Cpo006
010000060420     d  V2Cpo007
010100060420     d  V2Cpo008
010200060420     d  V2Cpo009
010300060420     d  V2Cpo010
010400060420     d  $V2Cpo                 1     30    inz  dim(10)
010500060420     d dsV2DPO         ds                  inz
010600060420     d  V2Dpo001
010700060420     d  V2Dpo002
010800060420     d  V2Dpo003
010900060420     d  V2Dpo004
011000060420     d  V2Dpo005
011100060420     d  V2Dpo006
011200060420     d  V2Dpo007
011300060420     d  V2Dpo008
011400060420     d  V2Dpo009
011500060420     d  V2Dpo010
011600060420     d  $V2Dpo                 1    200    inz  dim(10)
011700060410      *
011800060410     d WLBdat          ds                  inz
011900060410     d  G02dat                 1      8  0 inz
012000060410     d  G02inv                 9     16  0 inz
012100060410     d  G02err                17     17    inz('3')
012200060410     d  G02tgi                18     22  0 inz
012300060420      *
012400060420      * - DS Parametri per ricerca Punto Operativo
012500060420     d TNSD24ds        ds                  inz
012600060420     d  §cod                          3    inz
012700060420     d  §tip                          1    inz
012800060420     d  §des                         25    inz
012900060420     d  §pra                          3    inz
013000101007       //
013100101007       // -?Ridefinizione elenco librerie in elaborare le tabelle?
013200101007     d ds_Libr         ds                  inz
013300101007     d  $Libr                        10    dim(c_NrLibr) inz
013400060410      *
013500101007      * - Status ds
013600060410     d Status         sds
013700060410     d  VTCpgm           *proc
013800060410
013900060410      *---------------------------------------------------------------*
014000060410      *   V A R I A B I L I                                           *
014100060410      *---------------------------------------------------------------*
014200060410      *
014300101007      * - Flags booleani
014400101007     d $Called         s               n   inz(*off)
014500101007     d $Fine           s               n   inz(*off)
014600101007     d $InzV1          s               n   inz(*on)
014700101007     d $InzV2          s               n   inz(*on)
014800101007     d $InzW1          s               n   inz(*on)
014900110804     d $NoInt          s               n   inz(*on)
015000101007     d $TIBS02         s               n   inz(*off)
015100101007      * - Variabili per la gestione dei video
015200060410     d $Video          s              1a   inz('1')
015300060410     d wTasto          s              2a   inz(*zeros)
015400101007      * - Indici di schiera
015500101007     d xx              s              3  0 inz
015600101007      * - Campi di comodo
015700060410     d wDate           s               d   datfmt(*iso) inz(*loval)
015800060410     d WWWksc          s                   like(I69kac) inz
015900101007       // -?Nome del sistema?
016000101007     d currSysNeta     s              8a   inz
016100101007       // -?Nome esteso Libreria/File del file tabella?
016200101007     d wLibFile        s             21a   inz
016300101007       // -?Campi di comodo?
016400101007     d w_iSystem       s              1  0 inz
016500101007     d w_SisInf        s              3  0 inz
016600101007
016700101007       //--------------------------------------------------------------
016800101007       //?Definizione prototipi procedure.                             ?
016900101007       //--------------------------------------------------------------
017000101007
017100101007       // -?Reperimento NETA sistema AS/400 corrente?
017200101007      /copy gaitrasrc/srcProtoPr,UBRTVNETA
017300060410
017400060410      *---------------------------------------------------------------*
017500060410      *   M A I N   L I N E                                           *
017600060410      *---------------------------------------------------------------*
017700060410      *  Riepilogo indicatori utilizzati:                             *
017800060410      *  --------------------------------                             *
017900060410      *  01 - Record inesistente (inserimento)                        *
018000060410      *  02 - Record esistente   (modifica)                           *
018100060410      *  04 - Record annullato   (ripristino)                         *
018200060410      *  10 - Comodo                                                  *
018300110804      *  11 - Interrogazione da TA61                                  *
018400060410      *  22 - Errori in scrittura record (WRITE)                      *
018500060410      *  28 - Emissione messaggio di errore a video                   *
018600060410      *  40 - Posizionamento curosre su campo V1DKSC x ricerca        *
018700060410      *  50 - ERR: Codice cliente                                     *
018800060410      *  51 - ERR: Tipo scarico dati (tab. "3T")                      *
018900060410      *  52 - ERR: Codice cliente unificante                          *
019000060410      *  90 - Riemissione videata                                     *
019100060410      *---------------------------------------------------------------*
019200060410      *
019300060410      * Operazioni iniziali
019400060410     c                   exsr      RoutInz
019500060410      *
019600060410      * Gestione video
019700060410     c                   dow       $Fine   = *off
019800060410     c     $Video        caseq     '1'           GesV1
019900060410     c     $Video        caseq     '2'           GesV2
020000060410     c     $Video        caseq     'A'           GesW1
020100060410     c                   endcs
020200060410     c                   enddo
020300060410      *
020400060410      * Fine
020500060410     c                   if        $TIBS02 = *on
020600060410     c                   clear                   TIBS02ds
020700060410     c                   movel     'C'           T02tla
020800060410     c                   call      'TIBS02R'
020900060410     c                   parm                    KPJBA
021000060410     c                   parm                    TIBS02ds
021100060410     c                   endif
021200060410      *
021300101007     c                   return
021400060410
021500060410      *---------------------------------------------------------------*
021600060410      * RoutInz - Operazioni Iniziali                                 *
021700060410      *---------------------------------------------------------------*
021800060410     c     RoutInz       BEGSR
021900060410      *
022000060410      * Ricezione parametri
022100060410     c     *entry        plist
022200060410     c                   parm                    KPJBA
022300110804     c                   parm                    TNTA61ds
022400060410      *
022500060410      * Definizioni chiavi di accesso
022600060410     c     K05TBE01      klist                                                  *tntbe01l
022700060410     c                   kfld                    TBEcod                         -tabella
022800060410     c                   kfld                    TBEke1                         -chiave uno
022900060410     c                   kfld                    TBEke2                         -chiave due
023000060410     c                   kfld                    TBElin                         -lingua
023100060410     c                   kfld                    TBEsif                         -s.informativo
023200060410      *
023300060410     c     K03TABEL      klist
023400060410     c                   kfld                    TBLkut
023500060410     c                   kfld                    TBLcod
023600060410     c                   kfld                    TBLkey
023700060410     c                   eval      TBLkut  =  1
023800060410     c                   clear                   TBLcod
023900060410     c                   movel     *zeros        TBEke1
024000101007      /free
024100101007
024200101007         *inLR = *on;
024300101007
024400101007         // -?Verifica del sistema AS/400 corrente?
024500101007         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
024600101007           $Fine = *on;
024700101007           leavesr;
024800101007         endif;
024900101007
025000101007         // -?Impostazione elenco librerie in cui gestire le tabelle?
025100101007         //  ?(a seconda del sistema in cui si stà lavorando)?
025200101007         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
025300101007         if  w_iSystem = *zero;
025400101007           $Fine = *on;
025500101007           leavesr;
025600101007         endif;
025700101007
025800101007         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
025900101007         w_SisInf = 1;
026000101007         exsr  sr_OpenFileTab;
026100101007
026200101007      /end-free
026300060410      *
026400060410      * Reperisco la data odierna
026500060410     c                   movel     *date         Wdate
026600060410      *
026700060410      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
026800060410     c     *dtaara       define    §azute        AZUTEds
026900060410     c     *dtaara       define    §datiute      dDatiUte
027000060410      *
027100060410     c                   clear                   AZUTEds
027200060410     c                   clear                   dDatiUte
027300060410     c                   clear                   TIBS34ds
027400060410     c                   in(E)     *dtaara
027500060410if  1c                   if        %Error  or  RSUT = *blanks
027600060410     c                   call      'TIBS34R'
027700060410     c                   parm                    TIBS34ds
027800060410     c                   in        *dtaara
027900060410e   1c                   endif
028000060410      *-- Verifica errori e autorità profilo
028100060410sel 1c                   SELECT
028200060410      *-- controllo se ho errori nei dati utente
028300060410      *--   nel qual caso NON risulta un profilo abilitato
028400060410w   1c                   WHEN      DUTerr  = 'E'
028500060410     c                   eval      $Fine   = *on
028600060410      *
028700060410      *-- CONTROLLO AUTORITA'
028800060410      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
028900060410      *-- se il chiamante non richiede autorità specifica verificare
029000060410      *--   quella del profilo
029100060410      *-- se il chiamante richiede autorità specifica verificarla,
029200060410      *--  se è blank verificare quella del profilo
029300060410      *
029400060410      * se UTEAUT = *BLANK non siamo in sede
029500060410w   1c                   WHEN      UTEaut  = *blank
029600060410      *
029700060410x   1c                   OTHER
029800060410      *
029900060410e   1c                   ENDSL
030000060410      *
030100060410      * Aggancio dati generali della tabella in esame
030200110914      * se non richiamato da interrogazione TA61
030300110914     c                   IF        %parms() = 1
030400060410     c                   clear                   TBEcod
030500060410     c                   move      *zeros        TBEke1
030600060410     c                   move      T02cod        TBEke1
030700060410     c                   clear                   TBEke2
030800060410     c                   clear                   TBElin
030900060410     c                   movel     KNSIF         TBEsif
031000060410     c     K05TBE01      chain     TNTBE01L
031100060410     c                   if        not %found(TNTBE01L)
031200060410     c                   clear                   TBEsif
031300060410     c     K05TBE01      chain     TNTBE01L
031400060410     c                   endif
031500060410     c                   if        %found(TNTBE01L)
031600060410     c                   movel     TNTBEds       xTNTBEds
031700060410     c                   else
031800060410     c                   clear                   xTNTBEds
031900060410     c                   endif
032000110914     c                   ENDIF
032100110804
032200110804      * Richiamato da interrogazione TA61
032300110804     c                   IF        %parms() > 1
032400110804     c                   clear                   TB55V1
032500110804     c                   eval      V1Cksc  = %editc(TA61ksc:'X')
032600110804     c                   exsr      CtrV1
032700110804     c                   IF        *in28
032800110804     c                   eval      TA61err = *on
032900110804     c                   eval      TA61msg = V1Dmsg
033000110804     c                   eval      $Fine = *on
033100110804     c                   leavesr
033200110804     c                   ENDIF
033300110804     c                   eval      V1Cksc = %editc(TA61ksc:'X')
033400110804     c                   eval      $NoInt  = *off
033500110804     c                   eval      *in11   = *off
033600110804     c                   eval      $Called = *on
033700110804     c                   eval      $Video  = '2'
033800110804
033900110804      * Richiamato a modo vecchio (senza tnta61ds)
034000110804     c                   ELSE
034100060410      *
034200060410      * Verifico se ho ricevuto parametri
034300060410if  1c                   if        %subst(KPJBU:1:7) = *blanks
034400060410     c                   leavesr
034500060410e   1c                   endif
034600060410      *
034700060410      * Controllo i parametri ricevuti
034800060410     c                   eval      $Called = *on
034900060410     c                   clear                   TB55V1
035000060410     c                   eval      V1Cksc  = %subst(KPJBU:1:8)
035100060410     c                   exsr      CtrV1
035200060410if  1c                   if        *in28
035300060410     c                   movel(p)  V1Dmsg        KPJBU
035400060410     c                   eval      $Fine   = *on
035500060410     c                   leavesr
035600060410x   1c                   else
035700060410     c                   eval      $Video  = '2'
035800060410e   1c                   endif
035900110804
036000110804     c                   ENDIF
036100060410      *
036200060410     c                   ENDSR
036300101007      /free
036400101007
036500101007       //--------------------------------------------------------------
036600101007       //?Apertura dei files tabelle nel sistema informativo impostato.?
036700101007       //--------------------------------------------------------------
036800101007       BEGSR  sr_OpenFileTab;
036900101007
037000101007         // -?Chiusura (eventuale) archivio?
037100101007         if  %open(TNTBE01L);
037200101007           close  TNTBE01L;
037300101007         endif;
037400101007
037500101007         // -?Apertura archivio?
037600101007         ds_Libr  = $SisInf(w_iSystem);
037700101007         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
037800101007         open  TNTBE01L;
037900101007
038000101007       ENDSR;
038100101007
038200101007      /end-free
038300060410
038400060410      *---------------------------------------------------------------*
038500060410      * GESV1  - Gestione videata selezione codice tabella            *
038600060410      *---------------------------------------------------------------*
038700060410     c     GesV1         BEGSR
038800060410      *
038900060410      * Inizializzazione videata
039000060410if  1c                   if        $InzV1  = *on
039100060410     c                   exsr      InzV1
039200060410     c                   movel     *off          $InzV1
039300060410e   1c                   endif
039400060410      *
039500060410      * Scrivo la testata
039600060410     c                   write     TB55T1
039700060410      *
039800060410     c                   exfmt     TB55V1
039900060410     c                   setoff                                       28  90
040000060410     c                   clear                   V1Dmsg
040100060410      *
040200060410sel 1c                   select
040300060410      * F3=Fine
040400060410w   1c                   when      *inKC
040500060410     c                   exsr      F03V1
040600060410      * F7=Ricerca cliente
040700060410w   1c                   when           *inKG
040800060410     c                             and  *in40   = *off
040900060410     c                   eval      *in40   = *on
041000060410     c                   eval      *in50   = *off
041100060411     c                   move      V1Cksc        WWWksc
041200060411     c                   clear                   V1Cksc
041300060410w   1c                   when           *in40
041400060410     c                             and (V1Cksc  = *blanks
041500060410     c                              or  V1Cksc  = *zeros)
041600060410     c                   exsr      SearchCLI
041700060410     c                   eval      *in40   = *off
041800060410     c                   eval      *in50   = *on
041900060410      *
042000060410      * Controllo dati immessi a video
042100060410x   1c                   other
042200060410     c                   exsr      CtrV1
042300060410      * - Rilevati Errori
042400060410if  2c                   if        *in90
042500060410     c                   leavesr
042600060410e   2c                   endif
042700060410      * - Passaggio alla videata di dettaglio
042800060410     c                   eval      $InzV2  = *on
042900060410     c                   eval      $Video  = '2'
043000060410      *
043100060410e   1c                   endsl
043200060410      *
043300060410     c                   ENDSR
043400060410
043500060410      *---------------------------------------------------------------*
043600060410      * INZV1  - Inizializzazione prima videata (V1)                  *
043700060410      *---------------------------------------------------------------*
043800060410     c     InzV1         BEGSR
043900060410      *
044000060410     c                   clear                   TB55V1
044100060410      *
044200060410      * Imposto il campo "codice cliente" per l'interrogazione
044300060410     c                   move      '?'           V1Cksc
044400060410      *
044500060410     c                   ENDSR
044600060410
044700060410      *---------------------------------------------------------------*
044800060410      * CTRV1  - Controllo e decodifica prima videata                 *
044900060410      *---------------------------------------------------------------*
045000060410     c     CtrV1         BEGSR
045100060410      *
045200060410     c                   movea     *zeros        *in(50)
045300060410      *
045400060410      * Codice cliente
045500060410      * - Ricerca:
045600060410     c     '?'           scan      V1Cksc                                 10
045700060410if  1c                   if        *in10
045800060410     c                   eval      $TIBS02 = *on
045900060410     c                   clear                   V1Cksc
046000060410     c                   seton                                          5090
046100060410     c                   reset                   TIBS02ds
046200060410     c                   movel     KNSIF         T02sif
046300060410if  2c                   if            DUTlpo             <> 'S'
046400060410     c                             and %subst(DUTute:1:3) <> 'EDP'
046500060410     c                   movel     DUTpou        T02ke1
046600060410e   2c                   endif
046700060410     c                   call      'TIBS02R'
046800060410     c                   parm                    KPJBA
046900060410     c                   parm                    TIBS02ds
047000060410if  2c                   if        T02err  = *blanks
047100060410     c                   movel     T02uni        dDSC
047200060410     c                   movel     T02ke1        V1Cksc
047300060410x   2c                   else
047400060410     c                   seton                                        285090
047500060410     c                   movel     T02msg        V1Dmsg
047600060410     c                   leavesr
047700060410     c                   endif
047800060410e   1c                   endif
047900060410      * - Controllo:
048000060410      * - - effettiva immissione
048100060410if  1c                   if            V1Cksc  = *blanks
048200060410     c                             or  V1Cksc  = *zeros
048300060410     c                   seton                                        285090
048400060410     c                   eval      V1Dmsg  = $Msg(1)
048500060410     c                   leavesr
048600060410e   1c                   endif
048700060410      * - - numericità
048800060410     c     DigitN        check     V1Cksc                                 10
048900060410if  1c                   if        *in10   =  *on
049000060410     c                   seton                                        285090
049100060410     c                   eval      V1Dmsg  = $Msg(2)
049200060410     c                   leavesr
049300060410e   1c                   endif
049400060410      * - - decodifica
049500060410     c                   move      V1Cksc        WWWksc
049600060410     c                   exsr      DecodCLI
049700060410     c                   movel     ACOrag        V1Dksc
049800110804      * il prossimo controllo non lo faccio se interrogazione
049900110804     c                   IF        not *in11
050000110804     c                   leavesr
050100110804     c                   ENDIF
050200110804
050300060410      * - - "appartenenza" al P.O. dell'utente
050400060410     c                   if            %subst(V1Cksc:1:3) <> %char(DUTpou)
050500060410     c                             and DUTlpo             <> 'S'
050600060410     c                             and %subst(DUTute:1:3) <> 'EDP'
050700060410     c                   seton                                        285090
050800060410     c                   eval      V1Dmsg  = $Msg(2)
050900060410     c                   leavesr
051000060410     c                   endif
051100060410      *
051200060410     c                   ENDSR
051300060410
051400060410      *---------------------------------------------------------------*
051500060410      * SearchCLI  * Ricerca cliente                                  *
051600060410      *---------------------------------------------------------------*
051700060410     c     SearchCLI     BEGSR
051800060410      *
051900060411     c                   select
052000060411     c                   when      $Video  = '1'
052100060411     c                   movel(p)  V1Dksc        PAXdmt
052200060411     c                   when      $Video  = '2'
052300060411     c                   movel(p)  V2Dksu        PAXdmt
052400060411     c                   endsl
052500060411      *
052600060410     c                   call      'XALFA3BR'
052700060410     c                   parm      RSUT          PAXdut           30
052800060410     c                   parm                    TBLkut
052900060411     c                   parm                    PAXdmt           48
053000060410     c                   parm      DUTkci        PAXccc            4 0
053100060410     c                   parm      9             PAXsta            1 0
053200060410     c                   parm                    PAXflr           90
053300060410     c                   parm      *blanks       PAXdit            3
053400060410     c                   parm      1             PAXnum            2 0
053500060410     c                   parm                    PAXksc           80
053600060410     c                   parm                    PAXksm          140
053700060410     c                   parm                    PAXkdm           60
053800060410      *
053900060410      *   · Imposto il codice selezionato a video
054000060411      *     (se selezionato e non premuto F12 - nel qual caso
054100060411      *      ripristino, invece, il codice savlavto)
054200060410if  1c                   if        PAXsta >= *zeros
054300060411     c                   select
054400060411     c                   when      $Video  = '1'
054500060410     c                   movel     PAXksm        V1Cksc
054600060410     c                   movel     PAXdmt        V1Dksc
054700060411     c                   when      $Video  = '2'
054800060411     c                   movel     PAXksm        V2Cksu
054900060411     c                   movel     PAXdmt        V2Dksu
055000060411     c                   endsl
055100060411x   1c                   else
055200060411     c                   select
055300060411     c                   when      $Video  = '1'
055400060411     c                   move      WWWksc        V1Cksc
055500060411     c                   when      $Video  = '2'
055600060411     c                   move      WWWksc        V2Cksu
055700060411     c                   endsl
055800060410e   1c                   endif
055900060410      *
056000060410     c                   ENDSR
056100060410
056200060410      *---------------------------------------------------------------*
056300060410      * DecodCLI   * Decodifica codice cliente                        *
056400060410      *---------------------------------------------------------------*
056500060410     c     DecodCLI      BEGSR
056600060410      *
056700060410     c                   clear                   TIBS69ds
056800060410     c                   clear                   ds_CNACO
056900060410     c                   clear                   ds_CNIND
057000060410     c                   clear                   ds_CNCLP
057100060410     c                   clear                   ds_FNCLS
057200060410     c                   eval      I69kac  = WWWksc
057300060410     c                   call      'TIBS69R'
057400060410     c                   parm                    TIBS69ds
057500060410     c                   parm                    ds_CNACO
057600060410     c                   parm                    ds_CNIND
057700060410     c                   parm                    ds_CNCLP
057800060410     c                   parm                    ds_FNCLS
057900060410      *
058000060410     c                   if        O69err <> *blanks
058100060410     c                   seton                                        285090
058200060410     c                   eval      V1Dmsg  = $Msg(2)
058300060410     C                   endif
058400060410      *
058500060410     c                   ENDSR
058600060410
058700060410      *---------------------------------------------------------------*
058800060410      * Ctr_3T     * Decodifica tipo scarico (tab. 3T)                *
058900060410      *---------------------------------------------------------------*
059000060410     c     Ctr_3T        BEGSR
059100060410      *
059200060410     c                   clear                   ds3T
059300060410      *
059400060410     c                   eval      TBLcod = '3T'
059500060410     c                   movel(p)  V2Ctsc        TBLkey
059600060410      *
059700060410     c     K03TABEL      chain     TABEL
059800060410      *
059900060410     c                   if            %found(TABEL00F)
060000060410     c                             and TBLflg = *blanks
060100060410     c                   movel     TBLuni        ds3T
060200060410     c                   movel     §3Tdes        V2Dtsc
060300060410     c                   else
060400060410     c                   eval      *in28   = *on
060500060410     c                   endif
060600060410      *
060700060410     c                   ENDSR
060800060410
060900060410      *---------------------------------------------------------------*
061000060410      * F03V1  - Tasto funzionale F03 -> Fine programma               *
061100060410      *---------------------------------------------------------------*
061200060410     c     F03V1         BEGSR
061300060410      *
061400060410     c                   movel     *on           $Fine                          fine pgm
061500060410      *
061600060410     c                   ENDSR
061700060410
061800060410      *---------------------------------------------------------------*
061900060410      * GESV2  - Gestione videata dettaglio dati                      *
062000060410      *---------------------------------------------------------------*
062100060410     c     GesV2         BEGSR
062200060410      *
062300060410      * Inizializzazione videata
062400060410     c                   if        $InzV2  = *on
062500060410     c                   exsr      InzV2
062600060410     c                   move      *off          $InzV2
062700060410     c                   endif
062800060410      *
062900060410      * Scrivo la testata
063000060410     c                   write     TB55T1
063100101007      *
063200060410     c                   exfmt     TB55V2
063300101007      *
063400060410     c                   setoff                                       28  90
063500060410     c                   clear                   V1Dmsg
063600060410     c                   reset                   wTasto
063700060410      *
063800060410sel 1c                   select
063900060420      * F3=Fine
064000060410w   1c                   when      *inKC
064100060410     c                   exsr      F03V1
064200060411      * F7=Ricerca cliente
064300060411w   1c                   when           *inKG
064400060411     c                             and  *in40   = *off
064500060411     c                   eval      *in40   = *on
064600060411     c                   eval      *in52   = *off
064700060411     c                   move      V2Cksu        WWWksc
064800060411     c                   clear                   V2Cksu
064900060411w   1c                   when           *in40
065000060411     c                             and  V2Cksu  = *zeros
065100060411     c                   exsr      SearchCLI
065200060411     c                   eval      *in40   = *off
065300060411     c                   eval      *in52   = *on
065400060410      * F12=Ritorno
065500060410w   1c                   when      *inKL
065600060410     c                   exsr      F12V2
065700060410      *
065800060410x   1c                   other
065900060410      * Controllo dati immessi a video
066000060410      * (non si fanno se richisto l'annullamento)
066100060410if  2c                   if        not *inKQ
066200060410     c                   exsr      CtrV2
066300060410e   2c                   endif
066400060410      * Aggiornamento se non ci sono errori
066500060410sel 2c                   select
066600060410      * - Rilevati errori
066700060410w   2c                   when      *in90
066800060410      * - F5=Ripristino
066900060410w   2c                   when      *inKE
067000060410     c                   eval      wTasto  = '05'
067100060410      * - F6=Conferma
067200060410w   2c                   when      *inKF
067300060410     c                   eval      wTasto  = '06'
067400060410      * - F16=Annullamento
067500060410w   2c                   when      *inKQ
067600060410     c                   eval      wTasto  = '16'
067700060410e   2c                   endsl
067800060410if  2c                   if        wTasto  > *zeros
067900101007       // -?Ora che si aggiornano contemporaneamente le tabelle   ?
068000101007       //  ?nelle 2 librerie dei 2 sistemi informativi            ?
068100101007       //  ?NON ha più senso gestire la richiesta di trasmissione!?
068200101007     ***c                   eval      $InzW1  = *on
068300101007     ***c                   eval      $Video  = 'A'
068400101007     c                   exsr      AggTBE
068500060410e   2c                   endif
068600060410e   1c                   endsl
068700060410      *
068800060410     c                   ENDSR
068900060410
069000060410      *---------------------------------------------------------------*
069100060410      * INZV2  - Inizializzazione seconda videata (V2)                *
069200060410      *---------------------------------------------------------------*
069300060410     c     InzV2         BEGSR
069400060410      *
069500060410     c                   clear                   TB55V2
069600110804     c                   eval      *in11 = $NoInt
069700060410      *
069800060410      * reimposto il campo chiave (qui di solo output)
069900060410     c                   movel     V1Cksc        V2Cksc
070000060410     c                   movel     V1Dksc        V2Dksc
070100101007      /free
070200101007
070300101007         // -?Apertura file TNTBE01L del 1° S.I. (sede)?
070400101007         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
070500101007           w_SisInf = 1;
070600101007           exsr  sr_OpenFileTab;
070700101007         endif;
070800101007
070900101007      /end-free
071000060410      *
071100060410      * Aggancio la tabella, se trovo il codice sono in modifica
071200060410      * o ripristino (se record annullato), altrimenti in immissione
071300060410     c                   exsr      ChnTBE
071400060410      *
071500060410sel 1c                   SELECT
071600060410      * IMMISSIONE
071700060410w   1c                   WHEN      NOT %found(TNTBE01L)
071800060410     c                   eval      *in01   = *on
071900060410     c                   eval      T1opz   = $Opz(01)
072000060410      * MODIFICA
072100060410w   1c                   WHEN          %found(TNTBE01L)
072200060410     c                             and TBEatb  = *blanks
072300060410     c                   eval      *in02   = *on
072400060410     c                   eval      T1opz   = $Opz(02)
072500060410      * RIPRISTINO
072600060410w   1c                   WHEN          %found(TNTBE01L)
072700060410     c                             and TBEatb <> *blanks
072800060410     c                   eval      *in04   = *on
072900060410     c                   eval      T1opz   = $Opz(06)
073000060410e   1c                   ENDSL
073100060410      *
073200060410      * Impostazione campi a video
073300060410     c                   eval      V2Ctsc  = §DSCtsc
073400060410     c                   eval      V2Cksu  = §DSCksu
073500060420     c                   if        %subst(TBEuni:10:30) >= *zeros
073600060420     c                   if        §DSCpo001 > *zeros
073700060420     c                   move      §DSCpo001     V2Cpo001
073800060420     c                   endif
073900060420     c                   if        §DSCpo002 > *zeros
074000060420     c                   move      §DSCpo002     V2Cpo002
074100060420     c                   endif
074200060420     c                   if        §DSCpo003 > *zeros
074300060420     c                   move      §DSCpo003     V2Cpo003
074400060420     c                   endif
074500060420     c                   if        §DSCpo004 > *zeros
074600060420     c                   move      §DSCpo004     V2Cpo004
074700060420     c                   endif
074800060420     c                   if        §DSCpo005 > *zeros
074900060420     c                   move      §DSCpo005     V2Cpo005
075000060420     c                   endif
075100060420     c                   if        §DSCpo006 > *zeros
075200060420     c                   move      §DSCpo006     V2Cpo006
075300060420     c                   endif
075400060420     c                   if        §DSCpo007 > *zeros
075500060420     c                   move      §DSCpo007     V2Cpo007
075600060420     c                   endif
075700060420     c                   if        §DSCpo008 > *zeros
075800060420     c                   move      §DSCpo008     V2Cpo008
075900060420     c                   endif
076000060420     c                   if        §DSCpo009 > *zeros
076100060420     c                   move      §DSCpo009     V2Cpo009
076200060420     c                   endif
076300060420     c                   if        §DSCpo010 > *zeros
076400060420     c                   move      §DSCpo010     V2Cpo010
076500060420     c                   endif
076600060420     c                   endif
076700060410      *
076800060410      * Decodifiche
076900060410     c                   exsr      CtrV2
077000060410     c                   setoff                                       28  90
077100060410     c                   movea     *zeros        *in(50)
077200060410     c                   clear                   V1Dmsg
077300060410      *
077400060410     c                   ENDSR
077500060410
077600060410      *---------------------------------------------------------------*
077700060410      * F12V2  - Tasto funzionale F12 -> Ritorno                      *
077800060410      *---------------------------------------------------------------*
077900060410     c     F12V2         BEGSR
078000060410      *
078100060410     c                   UNLOCK    TNTBE01L
078200060410      *
078300060410     c                   if        $Called = *off
078400060410      * - Ritorno alla videata precedente
078500060410     c                   eval      $Video  = '1'
078600060410     c                   else
078700060410      * - Uscita dal pgm
078800060410     c                   exsr      F03V1
078900060410     c                   endif
079000060410      *
079100060410     c                   ENDSR
079200060410
079300060410      *---------------------------------------------------------------*
079400060410      * CTRV2  - Controllo e decodifica seconda videata               *
079500060410      *---------------------------------------------------------------*
079600060410     c     CtrV2         BEGSR
079700060410      *
079800060410     c                   movea     *zeros        *in(50)
079900060410      *
080000060410      * Tipo scarico dati
080100060410      * - Ricerca:
080200060410     c     '?'           scan      V2Ctsc                                 10
080300060410if  1c                   if        *in10
080400060410     c                   eval      $TIBS02 = *on
080500060410     c                   clear                   V2Ctsc
080600060410     c                   seton                                          5190
080700060410     c                   call      'X§TABER'
080800060410     c                   parm                    TBLkut
080900060410     c                   parm      '3T'          TBLcod
081000060410     c                   parm      *blanks       TBLkey
081100060410     c                   parm                    §des
081200060410     c                   movel     TBLkey        V2Ctsc
081300060410     c                   movel     §des          V2Dtsc
081400060410     c                   leavesr
081500060410e   1c                   endif
081600060410      * - Controllo:
081700060410if  1c                   IF        V2Ctsc  = *blanks
081800060410     c                   seton                                        285190
081900060410     c                   eval      V1Dmsg  = $Msg(3)
082000060410     c                   leavesr
082100060410x   1c                   ELSE
082200060410     c                   exsr      Ctr_3T
082300060410if  2c                   if        *in28
082400060410     c                   seton                                          5190
082500060410     c                   eval      V1Dmsg  = $Msg(4)
082600060410     c                   leavesr
082700060410e   2c                   endif
082800060410e   1c                   ENDIF
082900060410      *
083000060410      * Cliente unificante
083100060410if  1c                   IF        V2Cksu  = *zeros
083200060410     c                   seton                                        285290
083300060410     c                   eval      V1Dmsg  = $Msg(5)
083400060410     c                   leavesr
083500060410x   1c                   ELSE
083600060410     c                   move      V2Cksu        WWWksc
083700060410     c                   exsr      DecodCLI
083800060410     c                   movel     ACOrag        V2Dksu
083900060410if  2c                   if        *in28
084000060410     c                   seton                                          5290
084100060410     c                   eval      V1Dmsg  = $Msg(6)
084200060410     c                   leavesr
084300060410e   2c                   endif
084400060410e   1c                   ENDIF
084500060420      *
084600060420      * P.O. abilitati
084700060420     c                   clear                   xx
084800060420do  1c                   DOW       xx      < %elem($V2Cpo)
084900060420      *
085000060420     c                   add       1             xx
085100060420      * - Ricerca:
085200060420     c     '?'           scan      $V2Cpo(xx)
085300060420if  2c                   if        %found
085400060420     c***                clear                   $V2Dpo(xx)
085500060420     c                   eval      *in90       = *on
085600060420     c                   eval      *in(52+xx)  = *on
085700060420     c                   clear                   TNSD24ds
085800060420     c                   call      'TNSD24R'
085900060420     c                   parm                    §cod
086000060420     c                   parm                    §tip
086100060420     c                   parm                    §des
086200060420     c                   parm                    §pra
086300060420if  3c                   if        §cod        > *zeros
086400060420     c                   movel     §cod          $V2Cpo(xx)
086500060420     c                   movel     §des          $V2Dpo(xx)
086600060420e   3c                   endif
086700060420e   2c                   endif
086800060420      * - Controllo:
086900060420      * - - effettiva immissione
087000060420if  2c                   if            $V2Cpo(xx) = *blanks
087100060420     c                             or  $V2Cpo(xx) = *zeros
087200060420     c                   clear                   $V2Cpo(xx)
087300060420     c                   clear                   $V2Dpo(xx)
087400060420     c                   iter
087500060420e   2c                   endif
087600060420      * - - numericità
087700060420     c     DigitN        check     $V2Cpo(xx)                             10
087800060420if  2c                   if        *in10       = *on
087900060420     c                   seton                                        28  90
088000060420     c                   eval      *in(52+xx)  = *on
088100060420     c                   eval      V1Dmsg      = $Msg(7)
088200060420     c                   leavesr
088300060420e   2c                   endif
088400060420      * - - decodifica
088500060420     c                   move      $V2Cpo(xx)    ORGfil
088600060420     c     ORGfil        chain     AZORG
088700060420if  2c                   if             %found(AZORG01L)
088800060420     c                             and  ORGfva = *blanks
088900060420     c                   movel     ORGdes        $V2Dpo(xx)
089000060420     c                   else
089100060420     c                   seton                                        28  90
089200060420     c                   eval      *in(52+xx)  = *on
089300060420     c                   eval      V1Dmsg      = $Msg(7)
089400060420     c                   leavesr
089500060420e   2c                   endif
089600060420      *
089700060420e   1c                   ENDDO
089800060410      *
089900060410     c                   ENDSR
090000060410
090100060410      *---------------------------------------------------------------*
090200060410      * GESW1  - Gestione videata dati relativi alla trasmissione     *
090300060410      *---------------------------------------------------------------*
090400060410     c     GesW1         BEGSR
090500060410      *
090600060410      * Inizializzazione videata
090700060410if  1c                   if        $InzW1 = *on
090800060410     c                   exsr      InzW1
090900060410     c                   movel     *off          $InzW1
091000060410e   1c                   endif
091100060410      *
091200060410     c                   exfmt     TB55W1
091300060410     c                   eval      *in90 = *off
091400060410     c                   clear                   W1MSG
091500060410      *
091600060410sel 1c                   select
091700060410      * F12=Ritorno
091800060410w   1c                   when      *inKL
091900060410     c                   exsr      F12W1
092000060410     c                   leavesr
092100060410e   1c                   endsl
092200060410      *
092300060410      * Controllo dati immessi a video
092400060410     c                   exsr      CtrW1
092500060410      *
092600060410      * Aggiornamento se non ci sono errori
092700060410if  1c                   if        not *in90 and *inKF
092800060410     c                   exsr      AggTBE
092900060410e   1c                   endif
093000060410      *
093100060410     c                   ENDSR
093200060410
093300060410      *---------------------------------------------------------------*
093400060410      * INZW1  - Inzializzazione window (W1)                          *
093500060410      *---------------------------------------------------------------*
093600060410     c     InzW1         BEGSR
093700060410      *
093800060410     c                   movea     *zeros        *in(50)
093900060410      *
094000060410sel 1c                   select
094100060410      *
094200060410      * F5=Ripristino
094300060410w   1c                   when      *inKE   and  *in04
094400060410     c                   eval      W1FTT = TBEftt
094500060410      *
094600060410      * F6=Conferma
094700060410w   1c                   when      *inKF
094800060410sel 2c                   select
094900060410      *   Immissione
095000060410w   2c                   when      *in01
095100060410     c                   eval      W1FTT = TBXftt
095200060410      *   Modifica / Ripristino
095300060410w   2c                   when      *in02   or    *in04
095400060410     c                   eval      W1FTT = TBEftt
095500060410e   2c                   endsl
095600060410      *
095700060410      * F16=Annullamento
095800060410w   1c                   when      *inKQ   and  not *in04
095900060410     c                   eval      W1FTT = TBEftt
096000060410      *
096100060410e   1c                   endsl
096200060410      *
096300060410      * Se NON immissione: visualizzo i dati relativi all'ultima
096400060410      *   trasmissione
096500060410if  1c                   if        not *in01
096600060410     c                   eval      W1FLT = TBEflt
096700060410     c                   eval      W1FTR = TBEftr
096800060410if  2c                   if        TBEdtr <> 0
096900060410     c                   clear                   WLBDAT
097000060410     c                   z-add     TBEdtr        G02inv
097100060410     c                   movel     '3'           G02err
097200060410     c                   call      'XSRDA8'
097300060410     c                   parm                    WLBDAT
097400060410     c                   z-add     G02dat        W1DTR
097500060410e   2c                   endif
097600060410e   1c                   endif
097700060410      *
097800060410     c                   ENDSR
097900060410
098000060410      *---------------------------------------------------------------*
098100060410      * CTRW1  - Controllo e decodifica window                        *
098200060410      *---------------------------------------------------------------*
098300060410     c     CtrW1         BEGSR
098400060410      *
098500060410     c                   movea     *zeros        *in(50)
098600060410      *
098700060411     c                   ENDSR
098800060410
098900060410      *---------------------------------------------------------------*
099000060410      * F21W1  - Tasto funzionale F12 -> Ritorno                      *
099100060410      *---------------------------------------------------------------*
099200060410     c     F12W1         BEGSR
099300060410      *
099400060410     c                   eval      $Video  = '2'
099500060410      *
099600060410     c                   ENDSR
099700060410
099800060410      *---------------------------------------------------------------*
099900060410      * CHNTBE * Aggancio tabella                                     *
100000060410      *---------------------------------------------------------------*
100100060410     c     ChnTBE        BEGSR
100200060410      *
100300060410     c                   clear                   dDSC
100400060410      *
100500060410     c                   movel     T02cod        TBEcod
100600060410     c                   movel(p)  V1Cksc        TBEke1
100700060410     c                   clear                   TBEke2
100800060410     c                   clear                   TBElin
100900060410     c                   movel     KNSIF         TBEsif
101000060410     c     K05TBE01      chain     TNTBE01L
101100060410      * Se non ho trovato il record con il sistema informativo
101200060410      * che ho in linea, lo abblenco
101300060410if  1c                   if        not %found(TNTBE01L)
101400060410     c                   clear                   TBEsif
101500060410     c     K05TBE01      chain     TNTBE01L
101600060410e   1c                   endif
101700060410      *
101800060410     c                   if        %found(TNTBE01L)
101900060410     c                   movel     TBEuni        dDSC
102000060410     c                   endif
102100060410      *
102200060410     c                   ENDSR
102300060410
102400060410      *---------------------------------------------------------------*
102500060410      * AGGTBE * Aggiornamento tabella                                *
102600060410      *---------------------------------------------------------------*
102700060410     c     AggTBE        BEGSR
102800060410      *
102900101007      /free
103000101007
103100101007         // -?Ciclo di elaborazione per ogni sistema informativo?
103200101007         For  w_SisInf = 1  To  %elem($Libr);
103300101007
103400101007           // -?Apertura (eventuale) del file ed aggancio record?
103500101007           if  w_SisInf > 1;
103600101007             exsr  sr_OpenFileTab;
103700101007             exsr  ChnTBE;
103800101007           endif;
103900101007
104000101007         //? N.B.                                                      ?
104100101007         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
104200101007         //   trasmissione (vedi flag TBEFTR).                        ?
104300101007         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
104400101007         //   subito nello stesso file di entrambi i S.I. - in due    ?
104500101007         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
104600101007
104700101007      /end-free
104800060410sel 1c                   SELECT
104900060410      *
105000060410      * F5=Ripristino
105100060410w   1c                   WHEN      wTasto='05'  and  *in04
105200101007      /free
105300101007               if  %found(TNTBE01L);
105400101007      /end-free
105500060410     c                   clear                   TBEatb
105600060410     c                   clear                   TBEftr
105700060410     c                   UPDATE    TNTBE000
105800101007      /free
105900101007               else;
106000101007                 // - Record appena cancellato fisicamente
106100101007                 exsr  RieTBE;
106200101007                 WRITE  TNTBE000;
106300101007               endif;
106400101007      /end-free
106500060410      *
106600060410      * F6=Conferma
106700060410w   1c                   WHEN      wTasto='06'
106800060410     c                   exsr      RieTBE
106900060410sel 2c                   select
107000060410      *   Immissione
107100060410w   2c                   when      *in01
107200060410     c                   WRITE     TNTBE000                             22
107300060410      *   Modifica / Ripristino
107400060410w   2c                   when      *in02   or    *in04
107500060410     c                   UPDATE    TNTBE000
107600060410e   2c                   endsl
107700060410      *
107800060410      * F16=Annullamento
107900060410w   1c                   WHEN      wTasto='16'  and  not *in04
108000060410     c                   movel     'A'           TBEatb
108100060410     c                   clear                   TBEftr
108200060410     c                   UPDATE    TNTBE000
108300060410      *
108400060410e   1c                   ENDSL
108500101007      /free
108600101007
108700101007         EndFor;
108800101007
108900101007      /end-free
109000060410      *
109100060410     c                   if        $Called = *off
109200060410      * - Ritorno alla prima videata che carico come da inizio pgm
109300060410     c                   movel     '1'           $Video
109400060410     c                   movel     *on           $InzV1
109500060410     c                   movel     *on           $InzV2
109600060410     c                   movel     *on           $InzW1
109700060410     c                   else
109800060410      * - Uscita dal pgm
109900060410     c                   exsr      F03V1
110000060410     c                   endif
110100060410     c
110200060410     c                   ENDSR
110300060410
110400060410      *---------------------------------------------------------------*
110500060410      * RIETBE * Riempimento dati tabella                             *
110600060410      *---------------------------------------------------------------*
110700060410     c     RieTBE        BEGSR
110800060410      *
110900060410     c                   clear                   TBEatb
111000060410     c                   if        TBXsif <> *blanks
111100060410     c                   movel     KNSIF         TBEsif
111200060410     c                   else
111300060410     c                   clear                   TBEsif
111400060410     c                   endif
111500060410     c                   movel     TBXapl        TBEapl
111600060410     c                   movel     T02cod        TBEcod
111700060410     c                   movel     V1Cksc        TBEke1
111800060410     c                   clear                   TBEke2
111900060410      *
112000060410     c                   clear                   dDSC
112100060410     c                   eval      §DSCtsc = V2Ctsc
112200060410     c                   eval      §DSCksu = V2Cksu
112300060420     c                   if        V2Cpo001  > *zeros
112400060420     c                   move      V2Cpo001      §DSCpo001
112500060420     c                   endif
112600060420     c                   if        V2Cpo002  > *zeros
112700060420     c                   move      V2Cpo002      §DSCpo002
112800060420     c                   endif
112900060420     c                   if        V2Cpo003  > *zeros
113000060420     c                   move      V2Cpo003      §DSCpo003
113100060420     c                   endif
113200060420     c                   if        V2Cpo004  > *zeros
113300060420     c                   move      V2Cpo004      §DSCpo004
113400060420     c                   endif
113500060420     c                   if        V2Cpo005  > *zeros
113600060420     c                   move      V2Cpo005      §DSCpo005
113700060420     c                   endif
113800060420     c                   if        V2Cpo006  > *zeros
113900060420     c                   move      V2Cpo006      §DSCpo006
114000060420     c                   endif
114100060420     c                   if        V2Cpo007  > *zeros
114200060420     c                   move      V2Cpo007      §DSCpo007
114300060420     c                   endif
114400060420     c                   if        V2Cpo008  > *zeros
114500060420     c                   move      V2Cpo008      §DSCpo008
114600060420     c                   endif
114700060420     c                   if        V2Cpo009  > *zeros
114800060420     c                   move      V2Cpo009      §DSCpo009
114900060420     c                   endif
115000060420     c                   if        V2Cpo010  > *zeros
115100060420     c                   move      V2Cpo010      §DSCpo010
115200060420     c                   endif
115300060410     c                   movel(p)  dDSC          TBEuni
115400060410      *
115500101007     ***c                   clear                   TBEftt
115600101007     ***c                   clear                   TBEflt
115700101007     ***c                   clear                   TBEftr
115800101007     ***c                   clear                   TBEdtr
115900101007      /free
116000101007
116100101007         TBEftt = 'S';
116200101007         TBEflt = TBXflt;
116300101007
116400101007         select;
116500101007           // - Annullamento o Ripristino
116600101007           when  (wTasto='05'  and  *in04)   or
116700101007                 (wTasto='16'  and  not *in04);
116800101007             clear  TBEftr;
116900101007             clear  TBEdtr;
117000101007           // - Aggiornamento o Scrittura in sede
117100101007           when  w_SisInf = 1;
117200101007             TBEftr = 'T';
117300101007             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
117400101007                                     : 1 : 8 ) );
117500101007           // - Aggiornamento o Scrittura in filiale
117600101007           other;
117700101007             TBEftr = 'R';
117800101007             TBEdtr = %int( %subst( %char( %dec( %timestamp() ) )
117900101007                                     : 1 : 8 ) );
118000101007         endsl;
118100101007
118200101007      /end-free
118300060410      *
118400060410     c                   ENDSR
118500060410
118600101007       //--------------------------------------------------------------
118700101007       //?Schiere a tempo di compilazione.                             ?
118800101007       //--------------------------------------------------------------
118900101007
119000101007** - $iSystem / $SisInf:?Sistemi AS/400 & Librerie con il file TABEL00F?
119100101007SETRAS  GAITRAGRU FILTRAGRU
119200101007AS888   GAITRAGRPSFILTRAGRPF
119300060410** - $Opz ----*
119400060410  Inserimento
119500060410    Modifica
119600060410     Copia
119700060410  Annullamento
119800060410Visualizzazione
119900060410   ANNULLATO
120000060410** - $Msg -------------------------------------------------------------------*
120100060410Immettere il codice cliente                                                    01
120200060410Codice cliente errato                                                          02
120300060410Tipo Scarico Dati obbligatorio                                                 03
120400060410Tipo Scarico Dati errato                                                       04
120500060410Codice cliente unificante obbligatorio                                         05
120600060410Codice cliente unificante errato                                               06
120700060420Codice P.O. errato                                                             07
