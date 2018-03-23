000100121217       //==============================================================
000200121218       //?Scadenza abilitazione al servizio Strategi "EasySped WEB"    ?
000300121217       //==============================================================
000400141008
000500121217       //--------------------------------------------------------------
000600121217       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700121217       //--------------------------------------------------------------
000800121217
000900121217     /*PRM dbgview(*source)
001000121217     /*PRM commit(*none)
001100121217     /*END
001200121217
001300121217       //--------------------------------------------------------------
001400121217       //?Specifiche di controllo.                                     ?
001500121217       //--------------------------------------------------------------
001600121217
001700121217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800121217     h dftactgrp(*no)
001900121217     h alwnull(*inputonly)
002000121217     h bnddir('UBBNDDIR')
002100121217
002200121217       //--------------------------------------------------------------
002300121217       //?Dichiarazione file.                                          ?
002400121217       //--------------------------------------------------------------
002500121217
002600141008       // -?Tabella "3C"?
002700141008     fTABEL00F  if   e           k disk    extfile(wLibTABEL) usropn
002800121220       // -?Tabella "3EW"?
002900140929     fTNTBE01L  If   e           k disk    extfile(wLibTNTBE)  usropn
003000141008       // -?Abilitazione clienti ai servizi internet?
003100141008     fTIVSS02L  If   e           k disk    usropn
003200121217
003300121217       //--------------------------------------------------------------
003400121217       //?Definizione costanti.                                        ?
003500121217       //--------------------------------------------------------------
003600121217
003700121217       // -?Codice utente conti?
003800121217     d c_Kut           c                   const(1)
003900121218
004000121218       // -?Costanti per la definizione delle schiere con i nomi?
004100121218       //  ?degli iSeries da elaborare e delle relative librerie?
004200140925     d c_NrSyst        c                   const(2)
004300140925     d c_NrLibr        c                   const(2)
004400121217
004500121217       //--------------------------------------------------------------
004600121217       //?Definizione schiere.                                         ?
004700121217       //--------------------------------------------------------------
004800121217
004900121218       // -?iSeries  &  Librerie con entrambi i file tabelle?
005000121218     d $iSystem        s                   like(currSysNetA)
005100121218     d                                     dim(c_NrSyst)
005200121218     d                                     ctdata   perrcd( 1)
005300121218     d $Libraries      s                   like(ds_Libr)
005400121218     d                                     dim(c_NrSyst)
005500121218     d                                     alt($iSystem)
005600121217
005700121217       //--------------------------------------------------------------
005800121217       //?Definizione aree dati.                                       ?
005900121217       //--------------------------------------------------------------
006000121217
006100121218       // -?Dati utente?
006200121218     d §AzUte        e ds                  extname(AZUTE00F)
006300121218     d                                     dtaara
006400121218     d §DatiUte      e ds                  extname(dDatiUte)
006500121218     d                                     dtaara
006600121217
006700121217       //--------------------------------------------------------------
006800121217       //?Definizione strutture dati.                                  ?
006900121217       //--------------------------------------------------------------
007000121217
007100121217       // -?Parametri in input?
007200121218     d KPJBA         e ds
007300121217
007400121217       // -?Tabelle usate:?
007500121217       // ·?Tab. "3EW" = Dati assegnati alla postazione EasyWEB?
007600121217     d d3EW          e ds                  inz  qualified
007700141008       // -?Tab. "3C" = Invio dati - BOLLETTAZIONE?
007800141008     d ds3C          e ds                  inz
007900121217
008000121217       // -?Parametri x Ridefinizione dati utente estesi x mailing *msg?
008100121217     d TRTCM1ds      e ds                  inz
008200121218         //?·§CM1mitt = Indirizzo e-mail del mittente?
008300121217     d   §CM1mitt    e                     inz('ced')
008400121218         //?·§CM1dst  = Indirizzo e-mail del destinatario?
008500121217     d   §CM1dst     e                     inz('ced@brt.it')
008600121218         //?·§CM1tips = Tipo lettera via e-mail:?
008700121218         // ?"LET" = testo allegato in corpo con logo?
008800121218         //         ?(richiede righe libere iniziali per il logo)?
008900121218         // ?"COR" = testo integrato senza logo?
009000121218         //         ?(non consente né UNDERLINE né HIGHLIGHT)?
009100121217     d   §CM1tips    e                     inz('COR')
009200121218         //?·§CM1po   = Filiale?
009300121217     d   §CM1po      e                     inz('046')
009400121218         //?·§CM1var  = Oggetto e-mail?
009500121217     d   §CM1var     e                     inz('*OBJM*+
009600121217     d                                     Nr MAX clienti estraibili -
009700121217     d                                     dalla s.p. di trasm. anagr-
009800121217     d                                     af. mittenti')
009900121218         //?·§CM1sts  = Stato?
010000121217     d   §CM1sts     e                     inz(*off)
010100121218         //?·§CM1sts  = Id processo?
010200121217     d   §CM1idp     e                     inz('2')
010300121221
010400121221       // -?Dati estratti via SQL (da TNTBE00F - "3EW")?
010500121221     d*// wSQL_dati       ds                  inz  qualified
010600121221     d*//   wSQLatb                           inz  like(TBEatb)
010700121221     d*//   wSQLksu                           inz  like(TBEke1)
010800121221     d*//   wSQLsun                           inz  like(TBEke2)
010900121221     d*//   wSQLuni                           inz  like(TBEuni)
011000121221       // -?Dati estratti via SQL (da TIVSS00F)?
011100140925     d TIVSS00F      e ds
011200121221     d wSQL_dati       ds                  inz  qualified
011300121221     d   wSQLsun                           inz  like(VSSsun)
011400121221     d   wSQLksu                           inz  like(VSSksu)
011500121221     d   wSQLisv                           inz  like(VSSisv)
011600121221     d   wSQLdsc                           inz  like(VSSdsc)
011700121218
011800121218       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
011900140925     d ds_Libr         ds                  inz
012000121218     d   $Libr                       10    dim(c_NrLibr) inz
012100140925
012200140925       // -?Clienti errati
012300141008     d  ErrKSU         s             10     dim(99999)
012400121217
012500121217       // -?Status?
012600121217     d Psds           sds
012700121217     d   SDSpgm          *proc
012800121217     d*//JobName             244    253                                         Job name
012900121217     d*//JobUser             254    263                                         User name
013000121217     d*//JobNumber           264    269s 0                                      Job number
013100121217
013200121217       //--------------------------------------------------------------
013300121217       //?Definizione variabili.                                       ?
013400121217       //--------------------------------------------------------------
013500121217
013600121217       // -?Flags booleani?
013700121217     d $EoF            s               n   inz
013800121221     d $no3EW          s               n   inz
013900140925     d $Almeno1ClienteAttivo...
014000140925     d                 s               n   inz
014100141008     d $PadreBloccatoAmm...
014200141008     d                 s               n   inz
014300141009     d $PossibileCliSost...
014400141008     d                 s               n   inz
014500170921     d $MaiSegnacolli...
014600170921     d                 s               n   inz
014700140925     d wTipErr1        s               n   inz
014800140925     d wTipErr2        s               n   inz
014900140925     d debug1000       s               n   inz
015000121217
015100121217       // -?Indici di schiera?
015200121217     d xx              s              5  0 inz
015300121217     d yy              s              5  0 inz
015400121217     d ww              s              5  0 inz
015500140925     d wX              s              5  0 inz
015600140925     d wY              s              5  0 inz
015700170926     d wScriviTipo     s              3  0 inz
015800121218
015900121218       // -?Nome esteso Libreria/File dei 2 file tabelle?
016000121220     d wLibTABEL       s             21a   inz
016100121218     d wLibTNTBE       s             21a   inz
016200121217
016300121217       // -?Campi di comodo?
016400121218     d w_iSystem       s              1  0 inz
016500121218     d w_SisInf        s              3  0 inz
016600121217     d wDate           s              8  0 inz
016700121218     d wErr            s             10i 0 inz
016800121218
016900121217       // -?Stringa SQL da eseguire?
017000121217     d wSQL            s           2048    inz  varying
017100121217
017200121217       //--------------------------------------------------------------
017300121217       //?Definizione prototipi procedure.                             ?
017400121217       //--------------------------------------------------------------
017500121218
017600121218       // -?Reperimento NETA sistema AS/400 corrente?
017700121218     d currSysNeta     s              8a   inz
017800121218      /copy gaitrasrc/srcProtoPR,UBRTVNETA
017900121218
018000121218       // -?Reperimento dati utente?
018100121218     d TIBS34ds      e ds
018200121218      /copy gaitrasrc/srcProtoPR,TIBS34R
018300121217
018400121217       // -?Reperimento legami in tab. "3C"?
018500121217      /copy gaitrasrc/srcProtoPI,UBLEG3C
018600121217      /copy gaitrasrc/srcProtoPR,UBLEG3C
018700121217
018800121217       // -?Verifica "blocco" clienti?
018900121217      /copy gaitrasrc/srcProtoPI,UBCHKABL
019000121217      /copy gaitrasrc/srcProtoPR,UBCHKABL
019100121217
019200121217       // -?Aggiornamento flag §3EWUPD della tab. "3EW"?
019300121217     d wRtnEsito       s             10i 0 inz
019400121217      /copy gaitrasrc/srcProtoPR,UB§3EWUPD
019500121217
019600121217       // -?Parametri API QCAPCMD (Process Commands)?
019700121217     d Qcmd            s           2048    inz  varying
019800121217      /copy qSysInc/qRpgleSrc,QCAPCMD
019900121217       // -?API QCAPCMD (Process Commands)?
020000121217      /copy gaitrasrc/srcProtoPR,QCAPCMD
020100170921
020200170921       // -?Reperimento ultimo segnacollo utilizzato?
020300170921      /copy gaitrasrc/srcProtoPI,ubLastNSC
020400170921      /copy gaitrasrc/srcProtoPR,ubLastNSC
020500121217
020600121217       // -?Parametri gestione errori API.?
020700121217      /copy qsysinc/qrpglesrc,QUSEC
020800121217
020900121217       //--------------------------------------------------------------
021000121217       //?Definizione key-list.                                        ?
021100121217       //--------------------------------------------------------------
021200121217
021300121217       // -?File TNTBE01L?
021400121217     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
021500121217     d                                     prefix(k_)   inz
021600141008
021700141008       // -?TABEL00F?
021800141008     d k03tabel00    e ds                  extname(TABEL00F : *key)
021900141008     d                                     prefix(k_)  inz
022000121220
022100141008       // -?File TIVSS02L?
022200141009     d k03tivss02    e ds                  extname(TIVSS02L : *key)
022300121220     d                                     prefix(k_)   inz
022400121217
022500121217       //--------------------------------------------------------------
022600121217       //?Definizione parametri procedura.                             ?
022700121217       //--------------------------------------------------------------
022800121217
022900121217     c     *Entry        plist
023000121217     c                   parm                    KPJBA
023100121217
023200121217      /free
023300121217
023400121217       //--------------------------------------------------------------
023500121217       //?M A I N - L I N E                                            ?
023600121217       //--------------------------------------------------------------
023700121217
023800121217       // -?Operazioni iniziali?
023900121217       exsr sr_RoutInz;
024000121217
024100121217       // -?Ciclo di reperimento clienti in tab. "3C"?
024200121218       doW  Not $EoF;
024300121217         exsr  sr_ReadCursor;
024400121218       enddo;
024500140925
024600140925       // -?viene mandata una e-mail a chi è preposto?
024700140925       exsr  sr_SendEmail;
024800121217
024900121217       // -?Operazioni finali?
025000121217       exsr sr_RoutEnd;
025100121217
025200121217       //--------------------------------------------------------------
025300121217       //?Operazioni iniziali.                                         ?
025400121217       //--------------------------------------------------------------
025500121217       BEGSR  sr_RoutInz;
025600121217
025700121217         exec sql  set option  dynusrprf = *owner,
025800121217                               closqlcsr = *endmod;
025900121217
026000121217         *inLR = *on;
026100121218
026200121218         // -?Verifica del sistema AS/400 corrente?
026300121218         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
026400121218           $EoF = *on;
026500121218           leavesr;
026600121218         endif;
026700121218
026800121218         // -?Impostazione elenco librerie in cui gestire le tabelle?
026900121218         //  ?(a seconda del sistema in cui si stà lavorando)?
027000121218         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
027100121218         if  w_iSystem = *zero;
027200121218           $EoF = *on;
027300121218           leavesr;
027400121218         endif;
027500121217
027600121217         // -?Reperimento data odierna (fmt aaaa/mm/gg)?
027700121217         wDate = %dec( %date() );
027800121219
027900121219         // -?Reperimento dati job?
028000121219         exsr  sr_DatiJob;
028100121220
028200141008         // -?Apertura file TABEL00F di sede?
028300121221         w_SisInf = 1;
028400141008         exsr  sr_OpenFileTBL;
028500141008
028600141008         // -?Apertura file TNTBE01L di sede?
028700141008         w_SisInf = 1;
028800141008         exsr  sr_OpenFileTBE;
028900140925
029000140925         // se è stato passato un indirizzo email specifico, uso quello altrimenti uso i default
029100140925         if %subst( kpjbu : 1 : 50 ) <> *blank;
029200140925           peml = %trim(%subst( kpjbu : 1 : 50 ));
029300140925
029400140925         else;
029500140925
029600140925           // a seconda del sistema AS/400 corrente imposto il ricevente
029700140925           if %subst (currSysNeta:1:6) = 'SETRAS';
029800140925             peml = 'cedalert@brt.it';
029900140925             pcceml = *blanks;
030000140925           else;
030100140925             peml = 'cedvas@brt.it';
030200140925             pcceml = 'luciano.carini@brt.it';
030300140925           endif;
030400140925         endif;
030500140925
030600140925         pogg = 'BARTVAS - ANOMALIE SERVIZIO EW';
030700140925         pmsg =
030800140925          'Alla data: ' +
030900140925          %subst(%editc(wDate:'X'):7:2) + '/' +
031000140925          %subst(%editc(wDate:'X'):5:2) + '/' +
031100140925          %subst(%editc(wDate:'X'):1:4) +
031200140925          ' per i seguenti clienti risultano non congruenti le +
031300140925          configurazioni.' +
031400140925          ' :/N';
031500121217
031600121217         // -?Preparazione stringa SQL?
031700121217         exsr  sr_PrepSQL;
031800121217
031900121217         // -?Apertura cursore SQL?
032000121217         exsr  sr_OpenCursor;
032100140925
032200140925         clear  wX;
032300140925         clear  wY;
032400130208
032500140925       ENDSR;
032600121219
032700121219       //--------------------------------------------------------------
032800121219       //?Reperimento Dati del job (Utente/Operativi).                 ?
032900121219       //--------------------------------------------------------------
033000121219       BEGSR  sr_DatiJob;
033100121219
033200121219         in(E) §AzUte;
033300121219         if NOT %error;
033400121219           in(E) §DatiUte;
033500121219         endif;
033600121219         if %error or RSut = *blanks;
033700121219           clear TIBS34ds;
033800121219           tibs34r ( tibs34ds );
033900121219           in §AzUte;
034000121219           in §DatiUte;
034100121219         endif;
034200140925
034300121219       ENDSR;
034400121217
034500121217       //--------------------------------------------------------------
034600121217       //?Preparazione stringa SQL                                     ?
034700121217       //--------------------------------------------------------------
034800121217       BEGSR  sr_PrepSQL;
034900121221
035000121221         wSQL = 'select VSSsun, VSSksu, VSSisv, VSSdsc +
035100121221                   from TIVSS00F +
035200121221                  where VSSisv = ''EW'' +
035300121221                  order by VSSksu, VSSsun +
035400140926         // in caso di debug conviene avere poche righe da testare
035500170927         //       fetch first 100 rows only  +
035600140926                    for fetch only';
035700121217
035800121217       ENDSR;
035900121217
036000121217       //--------------------------------------------------------------
036100121217       //?Apertura cursore.                                            ?
036200121217       //--------------------------------------------------------------
036300121217       BEGSR  sr_OpenCursor;
036400121217
036500121217         // -?Dichiarazione cursore?
036600121217         exec sql   prepare S1   from :wSQL;
036700121217         exec sql   declare C1   insensitive   cursor for S1;
036800121217
036900121217         // -?Apertura del cursore?
037000121217         exec sql   open C1;
037100121217
037200121217         if  SQLcode < *zero;
037300121217           $EoF = *on;
037400121217           wErr = 1;
037500121217           exsr  sr_SendEmail;
037600121217         endif;
037700121217
037800121217       ENDSR;
037900121217
038000121217       //--------------------------------------------------------------
038100121217       //?Lettura cursore.                                             ?
038200121217       //--------------------------------------------------------------
038300121217       BEGSR  sr_ReadCursor;
038400121217
038500121217         clear  wSQL_dati;
038600121217
038700121217         exec sql   fetch next   from C1   into :wSQL_dati;
038800141008
038900141008         //le righe successive servono per facilitare il debug
039000141008         //wY += 1;
039100141008         //if %rem(wY:1000) = 0;
039200141008         //  debug1000 = *on;
039300141008         //endif;
039400141008         //debug1000 = *off;
039500121217
039600121217         select;
039700121217
039800121217           when  SQLcode < *zero;
039900121217             $EoF = *on;
040000121217             wErr = 2;
040100121217             exsr  sr_SendEmail;
040200121217
040300121217           when  SQLcode = 100;
040400121217             $EoF = *on;
040500121217
040600121217           other;
040700121220             exsr  sr_ElabUnif;
040800121217
040900121217         endsl;
041000121217
041100121217       ENDSR;
041200121217
041300121217       //--------------------------------------------------------------
041400121217       //?Chiusura cursore.                                            ?
041500121217       //--------------------------------------------------------------
041600121217       BEGSR  sr_CloseCursor;
041700121217
041800121217         // -?Chiusura del cursore?
041900121217         exec sql   close C1;
042000121217
042100121217       ENDSR;
042200121217
042300121217       //--------------------------------------------------------------
042400140925       //?Elaborazione singolo unificante
042500140925       //?Questa routine:
042600140925       //?1) reperisce lo stato della tab. 3EW rispetto al rcd di TIVSS letto;
042700141009       //?2) verifico se è stato aperto un cliente a sostituzione di quello letto;
042800141009       //?3) reperisce la lista dei figli del cliente unificante del rcd di TIVSS letto
042900140925       //?   e per ognuno, compreso il padre, reperisce lo stato nei termini del blocco anagrafico;
043000141009       //?4) analizzo se ci sono incongruenze tra il rcd di TIVSS letto con i precedenti punti
043100121217       //--------------------------------------------------------------
043200121220       BEGSR  sr_ElabUnif;
043300121218
043400121218         clear  d3EW;
043500140925         clear  $no3EW;
043600140925         clear  $Almeno1ClienteAttivo;
043700121221
043800121221         // -?Reimpostazione TNTBE di sede in "prima lettura"?
043900121221         w_SisInf = 1;
044000121221         exsr  sr_OpenFileTBE;
044100140925
044200140925       //?1) reperisce lo stato della tab. 3EW rispetto al rcd di TIVSS letto;
044300121221
044400130121         // -?Reperimento tabella "3EW"?
044500121221         clear  k05tntbe01;
044600121221         k_TBEcod = '3EW';
044700121221         k_TBEke1 = wSQL_dati.wSQLksu;
044800121221         k_TBEke2 = wSQL_dati.wSQLsun;
044900121221         chain(n)  %kds(k05tntbe01)  TNTBE000;
045000121221         if  %found(TNTBE01L)  and  TBEatb = *blank;
045100121221           d3EW = TBEuni;
045200121221         endif;
045300140925
045400140925         // -?Memorizza Cliente non più in tab. "3EW"?
045500140925         // -?          oppure presente ma annullato?
045600140925         // -?          oppure presente ma annullato applicativamente?
045700140925         $no3EW    = (Not  %found(TNTBE01L)  or
045800140925                      %found(TNTBE01L)  and  (TBEatb <> *blank  or
045900140925                                             d3EW.§3EWfaa <> *blank));
046000140925
046100141009       //?2) verifico siamo nella situazione in cui si potrebbe essere aperto un cliente
046200141009       //?   a sostituzione di quello letto;
046300141008         exsr sr_CliSost;
046400141008
046500141009       //?3) reperisce la lista dei figli del cliente unificante del rcd di TIVSS letto
046600140925       //?   e per ognuno, compreso il padre, reperisce lo stato nei termini del blocco anagrafico;
046700141008
046800140925         // -?Reperimento dello stato della famiglia in termini di blocco anagrafico?
046900121218         exsr  sr_ChkABL;
047000121218
047100141009       //?4) analizzo se ci sono incongruenze tra il rcd di TIVSS letto con i precedenti punti
047200140925
047300140925         // -?Analisi congruenza TIVSS-3C-3EW e spedizione email?                 ?
047400140925         exsr  sr_Analize_VSS_3C_3EW;
047500121217
047600121217       ENDSR;
047700121218
047800121218       //--------------------------------------------------------------
047900141009       //?2) verifico siamo nella situazione in cui si potrebbe essere aperto un cliente
048000141009       //?   a sostituzione di quello letto;
048100141008       // potrebbe accadere che il cliente è stato fatto scadere su TIVSS ma che la tab 3EW
048200141008       // è ancora attiva o che ha dei figli ancora attivi;
048300141009       // in questo caso potrebbe essere che il cliente era unificante di se stesso ed aveva
048400141008       // il support a BRT = ESWEB, poi, per qualche motivo, gli è stato dato un unificante
048500141008       // diverso da se stesso ed è stato aperto su TIVSS00F facendo scadere il precedente.
048600141008       // Se questo accade, allora siamo incappati in una sostituzione, in tutti gli altri casi:
048700141008       // - il cliente è padre di se stesso,
048800141008       // - il cliente non ha supporto a BRT = ESWEB,
048900141009       // - il cliente unificante è presente e attivo su TIVSS più volte,
049000141008       // siamo di fronte a una anomalia.
049100121218       //--------------------------------------------------------------
049200141008       BEGSR  sr_CliSost;
049300141008
049400170928       wDescrKSU = *all'*';
049500170928       $PossibileCliSost = *off;
049600141008
049700141008       // -?Reperimento dati da tab.3C del cliente letto su TIVSS
049800141008       clear  ds3C;
049900141008       k_TBLkut = c_KUT;
050000141008       k_TBLcod = '3C';
050100141008       k_TBLkey = %subst(wSQL_dati.wSQLksu:2:7);
050200141008       chain  %kds( k03tabel00 )  TABEL;
050300141008
050400141008       // -?proseguo solo se Cliente trovato in tab. "3C"?
050500141008       if  %found(TABEL00F);
050600141008         ds3C = TBLuni;
050700170928         wDescrKSU = §3CRag;
050800141008         select;
050900141008           // -?se il cliente è padre di se stesso
051000141009           //  ?esco dalla routine perché non siamo sicuramente nel caso di sostituzione cliente?
051100141008           when §3CCKS = %dec(%subst(wSQL_dati.wSQLksu:2:7):7:0) ;
051200141008             leavesr;
051300141008           // -?se il cliente (NON padre di se stesso) non ha Supporto a BRT = ESWEB?
051400141009           //  ?esco dalla routine perché non siamo sicuramente nel caso di sostituzione cliente?
051500141009           //  ?(perché sostituire su TIVSS un cliente che non dovrebbe esserci?)?
051600141008           when §3CCBA <> 'ESWEB';
051700141008             leavesr;
051800141008         endsl;
051900141008
052000141009         // -?se sono qui vuol dire che sono ancora in una situazione di possibile sostituzione?
052100141009         //  ?quindi controllo se ci sono più volte lo stesso cliente (non scaduto) su TIVSS?
052200141008         wY = 0;
052300141008         if not %open(tivss02l);
052400141008           open tivss02l;
052500141008         endif;
052600141009         clear  k03TIVSS02;
052700141009         k_VSSksu = '0' + %editc(§3CCKS : 'X');
052800141008         k_VSSisv = 'EW';
052900141009         setll  %kds(k03TIVSS02 : 2)  TIVSS000;
053000141009         reade(n)  %kds(k03TIVSS02 : 2)  TIVSS000;
053100141008         dow not %eof;
053200141008           // se il cliente letto non è scaduto
053300141008           if wDate >= VSSdde and wDate <= VSSdsc;
053400141008             // conto quanti sono
053500141008             wY += 1;
053600141008           endif;
053700141009           reade(n)  %kds(k03TIVSS02 : 2)  TIVSS000;
053800141008         enddo;
053900141008         // -?se ce n'è più d'uno
054000141009         //  ?esco perché questa è già una situazione anomala?
054100141008         if wY > 1;
054200141008           leavesr;
054300141008         endif;
054400141008
054500141009         // -?se sono qui vuol dire che potenzialmente sono in una situazione di sostituzione?
054600141009         //  ?ne sarò certo se il TIVSS del cliente in esame è scaduto?
054700141009         $PossibileCliSost = *on;
054800141008
054900141008       endif;
055000141008
055100141008       ENDSR;
055200141008
055300141008       //--------------------------------------------------------------
055400141008       //?Reperisce la lista dei figli del cliente unificante del rcd di TIVSS letto
055500141008       //?e per ognuno, compreso il padre, reperisce lo stato nei termini del blocco anagrafico;
055600141008       //--------------------------------------------------------------
055700141008       BEGSR  sr_ChkABL;
055800121218
055900121218         // -?Reperimento famiglia completa del cliente in esame?
056000121218         clear  ubLeg3C_FlgPF;
056100121218         clear  ubLeg3C_Padre;
056200121218         clear  ubLeg3C_SKC;
056300121218         clear  ubLeg3C_SKEW;
056400121218
056500121218         If  wSQL_dati.wSQLksu > *zero;
056600121218
056700121218           if  ubLeg3C_Rtv ( %int( %trim( wSQL_dati.wSQLksu ) ) :
056800121218                             ubLeg3C_FlgPF :
056900121218                             ubLeg3C_Padre :
057000121218                             ubLeg3C_SKC :
057100121218                             ubLeg3C_SKEW ) < *zero;
057200121220             if  sch_SKC(1) = *zero;
057300121220               sch_SKC(1) = %int( %trim( wSQL_dati.wSQLksu ) );
057400121220             endif;
057500121218           endif;
057600121218
057700121218         EndIf;
057800121218
057900121218         // -?Impostazione schiera dei soli clienti della famiglia che?
058000140925         //  ?usano Supporto a BRT = ESWEB?
058100121218         clear  yy;
058200121218         clear  ww;
058300121218         clear  sk_KSC;
058400121218         clear  sk_ABL;
058500121218
058600121218         For  yy = 1  To  %elem(sch_SKEW);
058700121218
058800121218           select;
058900121218
059000121218             when  sch_SKC(yy)  = *zero;
059100121218               leave;
059200121218
059300121218             when  sch_SKEW(yy) = *on;
059400121218               ww += 1;
059500121218               sk_KSC(ww) = sch_SKC(yy);
059600121218
059700121218           endsl;
059800121218
059900121218         EndFor;
060000121218
060100121218         // -?Reperimento dell'eventuale blocco di ciascuno dei clienti?
060200121218         //  ?qui sopra intabellati?
060300140925         // lo eseguo anche se il cliente è padre di se stesso in modo da non avere CNACO in
060400140925         // questo pgm
060500140925         ubChkABL_Chk ( ubChkABL_Ksc : ubChkABL_Abl );
060600121218
060700141008         // -?Verifica se il Padre è bloccato amministrativamente?
060800141008         $PadreBloccatoAmm = *off;
060900141008         if sk_Abl(1) = '7' or sk_Abl(1) = '8';
061000141008           $PadreBloccatoAmm = *on;
061100141008         endif;
061200141008
061300141008         // -?Verifica se c'è almeno un cliente (tra Padre e Figli) attivo e non bloccato?
061400141008         $Almeno1ClienteAttivo = *off;
061500141008         For  yy = 1  To  ww;
061600121218
061700121218           select;
061800121218
061900121218             when  sk_KSC(yy) = *zero;
062000121218               leave;
062100121218
062200121218             // ·?? = Cliente NON trovato in anagrafica?
062300121218             // ·?A = Cliente annullato in anagrafica?
062400121218             // ·?8/* = Cliente bloccato?
062500121218             when  sk_ABL(yy) = *blank;
062600140925               $Almeno1ClienteAttivo = *on;
062700140925               // al primo trovato posso uscire dal ciclo
062800121218               leave;
062900121218
063000121218           endsl;
063100121218
063200121218         EndFor;
063300121218
063400121218       ENDSR;
063500121217
063600121217       //--------------------------------------------------------------
063700140925       //?Analizza congruenza tra stato del rcd di TIVSS letto con stato tab. 3C e 3EW
063800121217       //--------------------------------------------------------------
063900140925       BEGSR  sr_Analize_VSS_3C_3EW;
064000121217
064100140925         // -?Se il rcd letto è scaduto?
064200140925         IF  wSQL_dati.wSQLdsc <= wDate;
064300130208
064400140925           select;
064500140925           // se la tab. 3EW risulta ancora attiva
064600141008             when $no3EW = *off
064700141008           // e non siamo nel caso di sostituzione cliente
064800141009              and $PossibileCliSost = *off;
064900140925               // memorizzo come anomalia 1
065000140925               wX += 1;
065100170928               ErrKSU(wX) = '2' + wSQL_dati.wSQLksu;
065200140925           // se esiste almeno un cliente della famiglia ancora attivo
065300141008             when $Almeno1ClienteAttivo = *on
065400141008           // e non siamo nel caso di sostituzione cliente
065500141009              and $PossibileCliSost = *off;
065600140926               // memorizzo come anomalia 1
065700140925               wX += 1;
065800170928               ErrKSU(wX) = '2' + wSQL_dati.wSQLksu;
065900140925           endsl;
066000121217
066100140925         // -?Se il rcd letto NON è scaduto?
066200140925         else;
066300170921           $MaiSegnacolli = *off;
066400170921           // controlli quanti segnacolli ha generato finora
066500170922           if  ubLastNSC_Rtv (§3CNrS :
066600170922                             %dec(%subst(wSQL_dati.wSQLksu:2:3):3:0) :
066700170925                             ubLeg3C_SKC : *omit :
066800170921                             pOutMaxNSC : pOutMaxLNP :
066900170921                             pOutMaxAAS : pOutMaxMGS :
067000170921                             pOutMaxNSP :
067100170921                             pOutLastNSC : pOutLastLNP :
067200170921                             pOutLastAAS : pOutLastMGS :
067300170922                             pOutLastNSP ) >= *zero;
067400170921             if pOutMaxNSc = 0;
067500170921               $MaiSegnacolli = *on;
067600170921             endif;
067700170921           endif;
067800170921           // se il driver va in errore non si sa se ha staccato o no segnacolli, per cui pensiamo
067900170921           // positivo e diciamo che l'ha fatto
068000140925
068100140925           select;
068200170921             // se la tab. 3EW risulta NON attiva
068300170921             when $no3EW = *on
068400170921             // e non ho mai staccato segnacolli
068500170921             and $MaiSegnacolli = *on;
068600140925               // memorizzo come anomalia 2
068700140925               wX += 1;
068800170928               ErrKSU(wX) = '1' + wSQL_dati.wSQLksu;
068900141009               // se il cliente Padre è bloccato amministrativamente
069000141009               if $PadreBloccatoAmm = *on;
069100141009                 // lo segnalo perché il messaggio di errore deve avvertire di ciò
069200141009                 %subst(ErrKSU(wX):10:1) = '*';
069300141009               endif;
069400170921             // se non ci sono clienti della famiglia ancora attivi
069500170921             when $Almeno1ClienteAttivo = *off
069600170921             // e non ho mai staccato segnacolli
069700170921             and $MaiSegnacolli = *on;
069800140925               // memorizzo come anomalia 2
069900140925               wX += 1;
070000170928               ErrKSU(wX) = '1' + wSQL_dati.wSQLksu;
070100141008               // se il cliente Padre è bloccato amministrativamente
070200141008               if $PadreBloccatoAmm = *on;
070300141008                 // lo segnalo perché il messaggio di errore deve avvertire di ciò
070400141009                 %subst(ErrKSU(wX):10:1) = '*';
070500141008               endif;
070600140925           endsl;
070700140925
070800140925         endif;
070900140925
071000121217       ENDSR;
071100121218
071200121218       //--------------------------------------------------------------
071300121218       //?Apertura dei files tabelle nel sistema informativo impostato.?
071400121218       //--------------------------------------------------------------
071500121221       BEGSR  sr_OpenFileTBE;
071600121221
071700121221         ds_Libr   = $Libraries(w_iSystem);
071800121221         if  %subst(wLibTNTBE : 1 : 10) = $Libr(w_SisInf);
071900170925           leavesr;
072000170925         endif;
072100121218
072200121218         // -?Chiusura (eventuale) file tabelle?
072300121218         if  %open(TNTBE01L);
072400121218           close  TNTBE01L;
072500121218         endif;
072600121218
072700121218         // -?Apertura file tabelle?
072800121218         wLibTNTBE = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
072900121218         open  TNTBE01L;
073000121218
073100121218       ENDSR;
073200141008
073300141008       //--------------------------------------------------------------
073400141008       //?Apertura dei files tabelle nel sistema informativo impostato.?
073500141008       //--------------------------------------------------------------
073600141008       BEGSR  sr_OpenFileTBL;
073700141008
073800141008         ds_Libr   = $Libraries(w_iSystem);
073900141008         if  %subst(wLibTABEL : 1 : 10) = $Libr(w_SisInf);
074000141008           leavesr;
074100141008         endif;
074200141008
074300141008         // -?Chiusura (eventuale) file tabelle?
074400141008         if  %open(TABEL00F);
074500141008           close  TABEL00F;
074600141008         endif;
074700141008
074800141008         // -?Apertura file tabelle?
074900141008         wLibTABEL = %trimr( $Libr(w_SisInf) ) + '/' + 'TABEL00F';
075000141008         open  TABEL00F;
075100141008
075200141008       ENDSR;
075300121217
075400121217       //--------------------------------------------------------------
075500121217       //?Invio e-mail di avviso errore.                               ?
075600121217       //--------------------------------------------------------------
075700121217       BEGSR  sr_SendEmail;
075800121217
075900140925         // -?Ordino la schiera dei clienti in errore per tipo anomalia e codice?
076000140925         sorta %subarr(ErrKSU:1:wX);
076100140925
076200140925         // -?Leggo la schiera ordinata rompendo per tipologia errore?
076300140925         wTipErr1 = *off;
076400140925         wTipErr2 = *off;
076500140925         for  yy = 1  To  wX;
076600140925           //se la tipologia errore è la 1
076700140925           //ed è il primo errore per quella tipologia
076800140925           if %subst(ErrKSU(yy):1:1) = '1'
076900140925           and wTipErr1 = *off;
077000170926             // scrivo la segnalazione del tipo errore 1
077100140925             wTipErr1 = *on;
077200170928             pmsg = %trimr(pmsg) + ':/N' + ':/N' +
077300170928              'Servizio EW attivo ma nessun cliente attivo configurato.';
077400170926             // attivo il contatore di quanti scriverne
077500170926             wScriviTipo = 0;
077600170926           endif;
077700140925           //se la tipologia errore è la 2
077800140925           //ed è il primo errore per quella tipologia
077900140925           if %subst(ErrKSU(yy):1:1) = '2'
078000140925           and wTipErr2 = *off;
078100170926             // scrivo la segnalazione del tipo errore 2
078200140925             wTipErr2 = *on;
078300170928             pmsg = %trimr(pmsg) + ':/N' +
078400170928              'Servizio EW scaduto ma presenti configurazioni da verificare.';
078500170926             // attivo il contatore di quanti scriverne
078600170926             wScriviTipo = 0;
078700140925           endif;
078800170926           // lo aggiungo nella e-mail (solo 30 per tipo errore)
078900170926           if wScriviTipo < 30;
079000170926             // attivo il contatore di quanti scriverne
079100170926             wScriviTipo += 1;
079200170926             exsr  AggEmail;
079300170926           endif;
079400140925         endfor;
079500140926
079600140926         // se ho scritto almeno 1 errore
079700140926         if wX > 0;
079800140926         // invio e-mail
079900140926           exsr EmailInvia;
080000140926         endif;
080100121217
080200121217       ENDSR;
080300121217
080400121217       //--------------------------------------------------------------
080500121217       //?Operazioni finali.                                           ?
080600121217       //--------------------------------------------------------------
080700121217       BEGSR  sr_RoutEnd;
080800140925
080900121217         // -?Chiusura applicazioni precedentemente aperte?
081000121217         ubLeg3C_Close ();
081100121217         ubChkABL_Close ();
081200121217
081300121217         // -?Chiusura cursore SQL?
081400121217         exsr  sr_CloseCursor;
081500121218
081600121218         // -?Chiusura archivi?
081700121218         if  %open(TNTBE01L);
081800121218           close  TNTBE01L;
081900121218         endif;
082000141008         if  %open(TABEL00F);
082100141008           close  TABEL00F;
082200141008         endif;
082300141008         if %open(tivss02l);
082400141008           close tivss02l;
082500141008         endif;
082600121217
082700121217         // -?Chiusura pgm?
082800121217         return;
082900121217
083000121217       ENDSR;
083100121218
083200121218       //--------------------------------------------------------------
083300121218       //?Esecuzione del comando (già impostato).                      ?
083400121218       //--------------------------------------------------------------
083500121218       BEGSR  sr_ExecCmd;
083600121218
083700121218         clear Qcap0100;
083800121218         Qcabcsdh = *off;
083900121218         Qcapa    = *off;
084000121218         Qcacmdss = *off;
084100121218         Qcaerved = *allX'00';
084200121218
084300121218         clear Qusec;
084400121218         Qusbprv  = %size(Qusec);
084500121218
084600121218         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
084700121218                           %size(Qcap0100) : 'CPOP0100' : *omit :
084800121218                           0 : 0 : Qusec);
084900121218
085000121218         //if  Qusei <> *blank;
085100121218         //  ...;
085200121218         //endif;
085300121218
085400121218       ENDSR;
085500121217
085600121217      /end-free
085700121218
085800140925     C*------------------------------------------------------------------------*
085900140925     C* AggEmail: aggiunge rcd all'e-mail
086000140925     C*------------------------------------------------------------------------*
086100140925     C     AggEmail      BEGSR
086200140925     C*
086300140925     C                   EVAL      pmsg = %trimr(pmsg) +
086400140925     C                             ':/N' +
086500170906     C                             'Cli: ' +
086600140929     C                             %subst(ErrKSU(yy):3:7)
086700141008     C                   IF        %subst(ErrKSU(yy):10:1)='*'
086800141008     C                   EVAL      pmsg = %trimr(pmsg) +
086900170928     C                             '  (Bloccato)'
087000141008     C                   ENDIF
087100140925     C*
087200140925     C                   ENDSR
087300140925     C*------------------------------------------------------------------------*
087400140925     C* EmailInvia - Invio e-mail di conferma forzatura
087500140925     C*------------------------------------------------------------------------*
087600140925     C     EmailInvia    BEGSR
087700140925     C*
087800140925     C* invio e-mail
087900140925     C                   call      'TIS701C1'
088000140925     C                   parm                    peml            253
088100140925     C                   parm                    pcceml          253
088200140925     C                   parm                    pogg             44
088300140925     C                   parm                    pmsg           5000
088400140925     C                   parm                    pesito            1
088500140925     C*
088600140925     C                   ENDSR
088700121218** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
088800121218SETRAS  GAITRAGRU FILTRAGRU
088900121218AS888   GAITRAGRPSFILTRAGRPF
