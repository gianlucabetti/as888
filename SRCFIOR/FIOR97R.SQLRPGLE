000100110613      //---------------------------------------------------------------
000200130906      //?FIOR97R - Calcolo Data Ritiro
000300110613      //---------------------------------------------------------------
000400080108     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500080326     h dftactgrp(*no) actgrp(*caller)
000600110613
000700110613      //---------------------------------------------------------------
000800110613      //?Dichiarazione file.
000900110613      //---------------------------------------------------------------
001000161028     fAZCLN01L  if   e           k disk
001100110613
001200110613      //---------------------------------------------------------------
001300110613      //?Definizione costanti.
001400110613      //---------------------------------------------------------------
001500110613
001600110613      //---------------------------------------------------------------
001700110613      //?Definizione schiere.
001800110613      //---------------------------------------------------------------
001900110613
002000110613      //---------------------------------------------------------------
002100110613      //?Definizione aree dati.
002200110613      //---------------------------------------------------------------
002300110613
002400110613      //---------------------------------------------------------------
002500110613      //?Definizione strutture dati.
002600110613      //---------------------------------------------------------------
002700110613      // - Parametri ricevuti
002800110613     d KPJBA         e ds
002900130906     d FIOR97ds      e ds
003000161027     d FIOR971ds     e ds
003100161027     d wFIOR971ds      ds                  likeds(FIOR971DS)
003200160526
003300160526      // Ritorna orari servizio ritiri estero
003400160526     d FIORE00DS     e ds
003500111205
003600130906      // - Ricerca/Controllo tabelle
003700130906     d TIBS02ds      e ds                  inz
003800170623
003900170623      // - Ricerca Orari per Filiale
004000170623     d TISIO9DS      e ds                  inz
004100131010
004200131010      // - Orari Servizi
004300140319     d TRULORSDS     e ds
004400140319     d TRULOR2ds     e ds
004500130906
004600130906      // - Cerco primo giorno lavorativo
004700130906     d TRUL13ds      e ds                  inz
004800160114
004900160114      // - Aggiunge/Toglie gg/mm dalla data
005000160114     d XGIOLAVDS     e ds                  inz
005100130906
005200130906      // - Tabella DFT
005300130906     d dDFT          e ds                  inz
005400161024
005500161024      // - Tabella GPD - Giorni Posticipo Data Ritiro
005600161024     d dGPD          e ds                  inz
005700161024     d  skGiorni               1     14s 0 dim(7)
005800170315     d  skGiorniA             15     28s 0 dim(7)
005900131111
006000131111      // - Sk per filiali abilitate a Orari di Servizio
006100131111     d FilOkOrari      ds           256
006200131111     d  skFilOkOrari           1    255    dim(85)
006300161028
006400161028      // - Sk Mattina/Pomeriggio per gg calendario
006500161028     d CLNmat          ds
006600161028     d  mat                    1     31    dim(31)
006700161028     d CLNpom          ds
006800161028     d  pom                    1     31    dim(31)
006900110613
007000110613      //---------------------------------------------------------------
007100110613      //?Definizione variabili globali.
007200110613      //---------------------------------------------------------------
007300111205      // - Campi di comodo
007400131111     d alleore         s                   like(IOR97oao)
007500130917     d dataiso         s               d   datfmt(*iso)
007600161104     d DataCalcolo     s                   like(IOR97dao)
007700161104     d DataRitiroStd   s                   like(IOR97dao)
007800161028     d giorno          s              2s 0 inz
007900161024     d peso            s              7s 1 inz
008000161104     d Oggi            s              8s 0 inz
008100161104     d wdao            s                   like(IOR97dao)
008200161104     d wdar            s                   like(OOR97dar)
008300161024     d wDay_Of_Week    s              2  0 inz
008400130917     d wgg             s              2  0
008500130917     d wora1           s              6  0
008600130917     d wora2           s              6  0
008700130917     d wora3           s              6  0
008800161024     d xx              s              3  0 inz
008900110613
009000110613      // - Flags booleani
009100161104     d isFesta         s               n   inz(*off)
009200161024     d wprg867         s               n   inz(*off)
009300110613
009400110613      //---------------------------------------------------------------
009500110613      //?Definizione procedure esterne.
009600110613      //---------------------------------------------------------------
009700160526      // Ritorna Orari Servizio Ritiro Estero
009800160526     d fiore00r        pr                  extpgm('FIORE00R')
009900160526     d  fiore00ds                          likeds(FIORE00DS)
010000170623
010100170623      // - Ricerca orari di filiale
010200170623     d tisio9r         pr                  extpgm('TISIO9R')
010300170623     d  kpjba                              likeds(KPJBA)
010400170623     d  tisio9ds                           likeds(TISIO9DS)
010500131010
010600131010      // - Ricerca orari di servizio
010700140319     d trulorsr        pr                  extpgm('TRULORSR')
010800140319     d  kpjba                              likeds(KPJBA)
010900140319     d  trulorsds                          likeds(TRULORSDS)
011000140319     d  trulor2ds                          likeds(TRULOR2DS)
011100140319     d                                     options (*nopass)
011200160114
011300160114      // - Aggiunge/Toglie gg/mm dalla data
011400160114     d XGIOLAV         pr                  extpgm('XGIOLAV')
011500160114     d  xgiolavds                          likeds(xgiolavds)
011600111205
011700111205      //---------------------------------------------------------------
011800111205      //?Definizione prototipi.
011900111205      //---------------------------------------------------------------
012000130906      /copy gaitrasrc/srcprotopr,tibs02r
012100130906      /copy gaitrasrc/srcprotopr,trul13r
012200110613
012300110613      //---------------------------------------------------------------
012400110613      //?Definizione key-list.
012500110613      //---------------------------------------------------------------
012600161028      // - File AZCLN01L
012700161028     d k04azcln      e ds                  extname(AZCLN01L:*key)
012800161028     d                                     prefix(k_)
012900161028     d                                     inz
013000110613
013100110613      //---------------------------------------------------------------
013200110613      //?Main.
013300110613      //---------------------------------------------------------------
013400110613
013500110613     c     *Entry        plist
013600110613     c                   parm                    KPJBA
013700130906     c                   parm                    fior97ds
013800161027     c                   parm                    wfior971ds
013900110613
014000110613      /free
014100110613
014200110613       //?Operazioni iniziali
014300110613       exsr RoutInz;
014400161104
014500161104       //?Calcolo data massimo ritiro
014600161104       exsr CercoDataMassima;
014700161104
014800161104       //?Imposto data dalla quale parto a calcolare la data ritiro
014900161104       exsr CercoDataCalcolo;
015000161104
015100161104       //?Preparo dati per calcolo data ritiro
015200161104       exsr PreparaDati;
015300110613
015400170208       //?Calcolo la data ritiro in maniera Standard
015500170208       exsr Elabora;
015600170208
015700161104       //?Calcolo data ritiro con tabella GPD
015800161104       //?per Mittente e Ordinante NON codificato e peso inferiore a 5 Kg.
015900161104       //?ma NO Prepagato e no Ricevuta Ritiro
016000170522       //?e NO orm Export
016100161104       //?e no se Data Pronta Merce > Oggi
016200161104       //?e se in tabella ho trovato che devo posticipare
016300161104       IF  wprg867 and IOR97pkg < d§DFTpkgdt and IOR97mcod <> 'S' and
016400161104           IOR97tor <> 'P' and IOR97srm <> 'S' and
016500170522           IOR97naz = *blanks and
016600161104           IOR97dpm <= Oggi and
016700161104           IOR97ocod <> 'S' and skGiorni(wDay_Of_Week) > 0;
016800170208         exsr NewElabora;
016900170524       //?ritorno al chiamante che l'ORM è da posticipare
017000170524         OOR97postd = 'S';
017100170616       //?ritorno al chiamante che l'ORM si può anticipare
017200170616         IF  OOR97dmin < OOR97dar;
017300170616           OOR97antd = 'S';
017400170616         ENDIF;
017500161104       ENDIF;
017600110613
017700110613       //?Operazioni finali
017800110613       exsr RoutEnd;
017900110613
018000110613       //--------------------------------------------------------------
018100110613       //?Operazioni iniziali.
018200110613       //--------------------------------------------------------------
018300110613       BEGSR RoutInz;
018400110613
018500130906         clear OOR97dar;
018600130917         clear OOR97dmin;
018700130917         clear OOR97dmaxb;
018800130917         clear OOR97dmaxf;
018900130906         clear OOR97err;
019000130906         clear OOR97msg;
019100161024
019200161024       //?Controllo se passato o meno il terzo parametro
019300161024         IF  %parms < 3;
019400161027           clear FIOR971DS;
019500161024           wprg867 = *off;
019600161024         ELSE;
019700161027           FIOR971DS = wFIOR971DS;
019800161024           wprg867 = *on;
019900161024         ENDIF;
020000110613
020100111205       //?Se non ho la Filiale Emissione esco dal pgm con errore
020200130906         IF  IOR97poe = *zeros;
020300130906           OOR97err = '1';
020400130906           OOR97msg = 'Filiale Emissione non presente';
020500161104           exsr Routend;
020600161104           return;
020700110613         ENDIF;
020800110613
020900111205       //?Se non ho la Filile di Ritiro esco dal pgm con errore
021000170622       //?se richiamato per wprg867 on non ritorno più il messaggio
021100170622       //?di filiale ritiro a 0.
021200170622       //?Per il recupero della tabella GPD uso std 999 negli altri
021300170622       //?casi in cui serve la filiale ritiro se a zero uso la filial emissione
021400170622         IF  IOR97por = *zeros and not wprg867;
021500130906           OOR97err = '1';
021600130906           OOR97msg = 'Filiale Ritiro non presente';
021700161104           exsr Routend;
021800161104           return;
021900110613         ENDIF;
022000161024
022100130911       //?Se non ho la Data Immissione imposto data del sistema
022200130906         IF  IOR97dao = *zeros;
022300130911           IOR97dao = %dec(%date());
022400130906         ENDIF;
022500130906
022600130911       //?Se non ho l'Ora Immissione imposto ora del sistema
022700130906         IF  IOR97oao = *zeros;
022800130911           IOR97oao = %dec(%time());
022900130906         ENDIF;
023000130911
023100130911       //?Se non ho il codice tempo ritiro lo imposto a '01'
023200130911         IF  IOR97gf2 = *blanks;
023300130911           IOR97gf2 = '01';
023400130911         ENDIF;
023500161024
023600161024       //?Aggancio la tabella GPD con filiale ritiro ORM
023700161024         clear TIBS02ds;
023800161024         clear dGPD;
023900161024         T02mod = 'C';
024000161024         T02cod = 'GPD';
024100161024         T02sif = KNSIF;
024200170622       //?se non ho la filiale ritiro vado subito con generico 999
024300170622         IF  IOR97por > 0;
024400161024         T02ke1 = %editc(IOR97por:'X');
024500170622         ELSE;
024600170622           T02ke1 = '999';
024700170622         ENDIF;
024800161024         TNTBE_RicercaControllo (kpjba : tibs02ds);
024900161024         IF  T02err = *blanks;
025000161024           dGPD = T02uni;
025100161024         ELSE;
025200161024         //?Non trovo GPD con filiale ritiro
025300161024         //?la cerco con filiale generica 999
025400161024           clear TIBS02ds;
025500161024           T02mod = 'C';
025600161024           T02cod = 'GPD';
025700161024           T02sif = KNSIF;
025800161024           T02ke1 = '999';
025900161024           TNTBE_RicercaControllo (kpjba : tibs02ds);
026000161024           IF  T02err = *blanks;
026100161024             dGPD = T02uni;
026200161024           ENDIF;
026300161024         ENDIF;
026400130906
026500130917       //?Aggancio la tabella DFT con filiale ritiro ORM
026600130906         clear TIBS02ds;
026700130906         clear dDFT;
026800130906         T02mod = 'C';
026900130906         T02cod = 'DFT';
027000130909         T02sif = KNSIF;
027100130917         T02ke1 = %editc(IOR97por:'X');
027200130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
027300130906         IF  T02err = *blanks;
027400130906           dDFT = T02uni;
027500161027         ELSE;
027600161027       //?Se non ho trovato DFT con filile ritiro ORM
027700130906       //?aggancio la tabella DFT con filiale generica 999
027800130906         clear TIBS02ds;
027900130906         clear dDFT;
028000130906         T02mod = 'C';
028100130906         T02cod = 'DFT';
028200130909         T02sif = KNSIF;
028300130906         T02ke1 = '999';
028400130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
028500130906         IF  T02err = *blanks;
028600130906           dDFT = T02uni;
028700130906         ENDIF;
028800161027         ENDIF;
028900140319
029000140319       //?Se non devo calcolare con pikup area metto fisso il flag = a '01'
029100140319         IF  d§DFTpkt <> *blanks;
029200140319           IOR97gf2 = '01';
029300140319         ENDIF;
029400161109
029500161109       //?Se gg in tab. DFT vuoti imposto fissi
029600161109         IF  d§DFTgg1 = *blanks;
029700161109           d§DFTgg1 = '10';
029800161109         ENDIF;
029900161109         IF  d§DFTgg2 = *blanks;
030000161109           d§DFTgg2 = '25';
030100161109         ENDIF;
030200161109
030300161109       //?Imposto Oggi
030400161109         Oggi = %dec(%date());
030500130917
030600130917       //?Recupero orari limite ritiro in giornata con filiale ritiro
030700140319         clear trulorsds;
030800140319         clear trulor2ds;
030900170622       //?se non ho la filiale ritiro vado con la filiale emissione
031000170622         IF  IOR97por > 0;
031100140319         IOREfil = IOR97por;
031200170622         ELSE;
031300170622           IOREfil = IOR97poe;
031400170622         ENDIF;
031500140319         IOREcap = IOR97cap;
031600140319         IOREloc = IOR97loc;
031700161028         IF  IOR97dpm > IOR97dao;
031800161028           IOREdta = IOR97dpm;
031900161028         ELSE;
032000140319         IOREdta = IOR97dao;
032100161028         ENDIF;
032200140319         IOREtser = 'R';
032300140319         IOREgf2 = IOR97gf2;
032400140319         trulorsr (kpjba:trulorsds:trulor2ds);
032500140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
032600170623           //leavesr;
032700140319         ENDIF;
032800140319         wora1 = OOR2lrsc * 100;
032900140319         wora2 = OOR2lrnc * 100;
033000140319         wora3 = OOR2lrnk * 100;
033100160526
033200160526       //?Recupero orari limite ritiro in giornata con filiale ritiro
033300160526       //?e nazione per ritiri estero
033400160526         IF  IOR97naz <> *blanks;
033500160526           clear FIORE00DS;
033600170622       //?se non ho la filiale ritiro vado con la filiale emissione
033700170622           IF  IOR97por > 0;
033800160526           IORE00por = IOR97por;
033900170622           ELSE;
034000170622             IORE00por = IOR97poe;
034100170622           ENDIF;
034200160526           IORE00nar = IOR97naz;
034300160526           fiore00r (fiore00ds);
034400160526           IF  OORE00err = *blanks;
034500160526             wora1 = OORE00lrg * 100;
034600160526             wora2 = OORE00lrg * 100;
034700160526             wora3 = OORE00lrg * 100;
034800160526           ENDIF;
034900160526         ENDIF;
035000170623
035100170623       //?Se non ho il cap (caso di richiamo da WEB per Yoox)
035200170623       //?e non ho gli orari limiti per i ritiri chiamo direttamente il TISIO9R
035300170623       //?per recuperarli in modo da prendere quelli della filiale
035400170623         IF  IOR97web = 'S' and IOR97cap = *blanks and
035500170623             wora1 = *zeros and wora2 = *zeros and wora3 = *zeros;
035600170623           DO0tser = 'R';
035700170623           IF  IOR97por > 0;
035800170623             DO0lna = IOR97por;
035900170623           ELSE;
036000170623             DO0lna = IOR97poe;
036100170623           ENDIF;
036200170623           tisio9r (kpjba:tisio9ds);
036300170623           wora1 = DO0olrsc * 100;
036400170623           wora2 = DO0olrnc * 100;
036500170623           wora3 = DO0olrnk * 100;
036600170623         ENDIF;
036700161104
036800161104       ENDSR;
036900161104
037000161104       //--------------------------------------------------------------
037100161104       //?Calcolo Data Massimo ritiro.
037200161104       //--------------------------------------------------------------
037300161104       BEGSR CercoDataMassima;
037400130917
037500161104       //?Calcolo data massima ritiro bloccante da data immissione ORM
037600130917       //?da WEB
037700130917         IF  IOR97web = 'S';
037800130917           wgg = %dec(d§DFTgg1:2:0);
037900130917         ELSE;
038000130917       //?da AS400
038100130917           wgg = %dec(d§DFTgg2:2:0);
038200130917         ENDIF;
038300161104         wdao = IOR97dao;
038400160114         exsr CalcolaGG;
038500161104         OOR97dmaxb = wdar;
038600130917
038700161104       //?Calcolo data massima ritiro forzabile da data immissione ORM
038800130917       //?da WEB
038900130917         IF  IOR97web = 'S';
039000140407           OOR97dmaxf = OOR97dmaxb;
039100130917         ELSE;
039200130917       //?da AS400
039300130917           wgg = %dec(d§DFTgg1:2:0);
039400161104           wdao = IOR97dao;
039500160114           exsr CalcolaGG;
039600161104           OOR97dmaxf = wdar;
039700130917         ENDIF;
039800161104
039900161104       ENDSR;
040000161104
040100161104       //--------------------------------------------------------------
040200161104       //?Imposto Data di Calcolo.
040300161104       //--------------------------------------------------------------
040400161104       BEGSR CercoDataCalcolo;
040500161104
040600161104         DataCalcolo = Oggi;
040700161104
040800161104       //?Se data pronta merce è maggiore di oggi
040900161104       //?la data calcolo diventa la data pronta merce
041000161104         IF  IOR97dpm > 0 and IOR97dpm > Oggi;
041100161104           DataCalcolo = IOR97dpm;
041200161104         ENDIF;
041300161104
041400161104       ENDSR;
041500161104
041600161104       //--------------------------------------------------------------
041700161104       //?Preparo dati per calcolo data ritiro.
041800161104       //--------------------------------------------------------------
041900161104       BEGSR PreparaDati;
042000161104
042100161104         isFesta = *off;
042200130917
042300161104       //?Per il calcolo della data ritiro standard
042400161104       //?sommo 1 gg lavorativo alla data immissione ORM
042500161104         wdao = IOR97dao;
042600161104         wgg = 1;
042700161104         exsr CalcolaGG;
042800161104         DataRitiroStd = wdar;
042900161027
043000161104       //?Per il calcolo della data ritiro con tabella GPD
043100161104       //?devo cercare che giorno della settimana è la data calcolo
043200161104         dataiso = %date(datacalcolo);
043300161027         clear wDay_Of_Week;
043400161027         exec sql
043500161027         set :wDay_Of_Week = dayofweek_iso(:dataiso);
043600161104
043700161104       //?Se data pronta merce maggiore data immissione
043800161104       //?la data ritiro diventa la data pronta merce
043900161104       //?quindi controllo se la data pronta merce è festivo
044000161104         IF  IOR97dpm > IOR97dao;
044100161104           clear k_CLNtfp;
044200170622       //?se non ho la filiale ritiro vado con la filiale emissione
044300170622           IF  IOR97por > 0;
044400161104           k_CLNtfa = IOR97por;
044500170622           ELSE;
044600170622             k_CLNtfa = IOR97poe;
044700170622           ENDIF;
044800161104           k_CLNann = %dec(%subst(%editc(IOR97dpm:'X'):1:4):4:0);
044900161104           k_CLNmes = %dec(%subst(%editc(IOR97dpm:'X'):5:2):2:0);
045000161104           giorno = %dec(%subst(%editc(IOR97dpm:'X'):7:2):2:0);
045100161104           chain %kds(k04azcln) AZCLN01L;
045200161104           IF  %found(AZCLN01L) and
045300161104              (mat(giorno) = 'F' or pom(giorno) = 'F');
045400161104             isFesta = *on;
045500161104           ENDIF;
045600161104         ENDIF;
045700130906
045800110613       ENDSR;
045900161024
046000161024       //--------------------------------------------------------------
046100161024       //?Elabora data ritiro per non cod. con peso < 5 Kg.
046200161024       //--------------------------------------------------------------
046300161024       BEGSR NewElabora;
046400170208
046500170208       //?Come prima cosa imposto la data minima = alla data ritiro calcolata
046600170208       //?in modo standard
046700170208         OOR97dmin = OOR97dar;
046800161027
046900161028       //?Calcolo xx giorni lavorativi da oggi o Data Pronta Merce
047000161104         wdao = DataCalcolo;
047100161104         wgg = skGiorni(wDay_Of_Week);
047200161104         exsr CalcolaGG;
047300161104         OOR97dar = wdar;
047400170315
047500170616       //?Calcolo la data minima in base ai gg impostati in tabella
047600170616         IF  skGiorniA(wDay_Of_Week) > 0;
047700170315           wdao = OOR97dmin;
047800170316           wgg = skGiorniA(wDay_Of_Week);
047900170315           exsr CalcolaGG;
048000170315           OOR97dmin = wdar;
048100170315         ENDIF;
048200161024
048300161024       ENDSR;
048400110613
048500110613       //--------------------------------------------------------------
048600110613       //?Elabora.
048700110613       //--------------------------------------------------------------
048800110613       BEGSR Elabora;
048900161104
049000161104       //?Se Data Pronta Merce > Data Immissione
049100161104       //?Data Ritiro = Data Pronta Merce
049200161104         IF  IOR97dpm > IOR97dao;
049300161104           OOR97dar = IOR97dpm;
049400161104         //?Se Data Pronta Merce è festivo
049500161104         //?cerco il primo giorno lavorativo da data pronta merce
049600161104           IF  isFesta;
049700161104             wdao = IOR97dpm;
049800161104             wgg = 1;
049900161104             exsr CalcolaGG;
050000161104             OOR97dar = wdar;
050100161104           ENDIF;
050200161104           OOR97dmin = OOR97dar;
050300161104           leavesr;
050400161104         ENDIF;
050500161104
050600161104       //?Imposto subito la data ritiro con la data ritiro standard (domani)
050700170208         OOR97dar = DataRitiroStd;
050800130917
050900161104         SELECT;
051000130917       //?Se ORM prepagato o ORM con ricevuta di ritiro
051100130917       //?data ritiro sempre domani (primo giorno lavorativo)
051200161104         WHEN  IOR97tor = 'P' or IOR97srm = 'S';
051300130917       //?Se ora immissione ORM dopo la terza ora limite
051400130917       //?per ritiro in giornata
051500130918       //?(dopo le 14:00)
051600130917       //?data ritiro sempre domani (primo giorno lavorativo)
051700161104         WHEN  IOR97oao > wora3;
051800130918       //?Se ora immissione ORM dopo la seconda ora limite
051900130918       //?per ritiro in giornata
052000130918       //?(dopo le 12:30 ma prima della 14:00)
052100130918       //?per ORM commissionato o pikup 02
052200130918       //?data ritiro sempre domani (primo giorno lavorativo)
052300161104         WHEN  IOR97oao > wora2 and
052400161104              (IOR97com = 'S' or IOR97gf2 <> '01');
052500130918       //?Se ora immissione ORM dopo la seconda ora limite
052600130918       //?per ritiro in giornata
052700130918       //?(dopo le 12:30 ma prima della 14:00)
052800130918       //?data ritiro = domani (primo giorno lavorativo)
052900161104         WHEN  IOR97oao > wora2;
053000130918       //?Se ora immissione ORM dopo la prima ora limite
053100130918       //?per ritiro in giornata
053200130918       //?(dopo le 11:00 ma prima della 12:30)
053300130918       //?per ORM commissionato o pikup 02
053400130918       //?data ritiro = domani (primo giorno lavorativo)
053500161104         WHEN  IOR97oao > wora1 and
053600161104              (IOR97com = 'S' or IOR97gf2 <> '01');
053700130918       //?Se ora immissione ORM dopo la prima ora limite
053800130918       //?per ritiro in giornata
053900130918       //?(dopo le 11:00 ma prima della 12:30)
054000130918       //?data ritiro sempre = oggi
054100161104         WHEN  IOR97oao > wora1 and IOR97com <> 'S';
054200170208           OOR97dar = Oggi;
054300140507       //?Se ora immissione ORM inferiore a prima ora limite
054400140507       //?per ritiro in giornata
054500140507       //?(entro le 11:00)
054600140507       //?per ORM commissionato con invio Alert
054700140507       //?data ritiro = domani (primo giorno lavorativo)
054800161104         WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
054900161104               IOR97alert = 'S';
055000130917       //?Se ora immissione ORM inferiore a prima ora limite
055100130917       //?per ritiro in giornata
055200130918       //?(entro le 11:00)
055300130918       //?data ritiro = oggi
055400161104         WHEN  IOR97oao <= wora1;
055500170208           OOR97dar = Oggi;
055600130917         ENDSL;
055700161104
055800161104       //?Calcolo la data minima (possibilità di anticipo)
055900161104         exsr DataMinima;
056000110613
056100110613       ENDSR;
056200161104
056300161104       //--------------------------------------------------------------
056400161104       //?Calcola gg lavorativi da data immissione ORM
056500161104       //--------------------------------------------------------------
056600161104       BEGSR CalcolaGG;
056700161104
056800161104         clear XGIOLAVDS;
056900161104         IXGLdata = wdao;
057000161104         IXGLadd  = 'S';
057100161104         IXGLgg   = wgg;
057200161104         IXGLlav  = 'S';
057300161104         IXGLpa   = 'P';
057400170622       //?se non ho la filiale ritiro vado con la filiale emissione
057500170622         IF  IOR97por > 0;
057600161104         IXGLfil  = IOR97por;
057700170622         ELSE;
057800170622           IXGLfil  = IOR97poe;
057900170622         ENDIF;
058000161104         XGIOLAV (xgiolavds);
058100161104         IF  OXGLerr = *blanks;
058200161104           wdar = OXGLdata;
058300161104         ELSE;
058400161104           wdar = IOR97dao;
058500161104         ENDIF;
058600161104
058700161104       ENDSR;
058800161104
058900161104       //--------------------------------------------------------------
059000161104       //?Calcolo data minima (possibilità di anticipo).
059100161104       //--------------------------------------------------------------
059200161104       BEGSR DataMinima;
059300161104
059400161104       //?Imposto subito la data minima uguale alla data ritiro
059500161104       //?in questo modo non c'è anticipo
059600161104         OOR97dmin = DataRitiroStd;
059700161104
059800161104       //?Imposto quando posso anticipare
059900161104         SELECT;
060000161104       //?Se ora immissione ORM dopo la terza ora limite
060100161104       //?per ritiro in giornata
060200161104       //?(dopo le 14:00)
060300161104       //?no anticipo
060400161104           WHEN  IOR97oao > wora3;
060500161104       //?Se ora immissione ORM dopo la seconda ora limite
060600161104       //?per ritiro in giornata
060700161104       //?(dopo le 12:30 ma prima della 14:00)
060800161104       //?per ORM commissionato o pikup 02
060900161104       //?no anticipo
061000161104           WHEN  IOR97oao > wora2 and
061100161104                (IOR97com = 'S' or IOR97gf2 <> '01');
061200161104       //?Se ora immissione ORM dopo la seconda ora limite
061300161104       //?per ritiro in giornata
061400161104       //?(dopo le 12:30 ma prima della 14:00)
061500161104       //?anticipo se mittente Codificato e no da WEB
061600161104           WHEN  IOR97oao > wora2;
061700161104             IF  IOR97mcod = 'S' and IOR97web <> 'S';
061800161104               OOR97dmin = Oggi;
061900161104             ENDIF;
062000161104       //?Se ora immissione ORM dopo la prima ora limite
062100161104       //?per ritiro in giornata
062200161104       //?(dopo le 11:00 ma prima della 12:30)
062300161104       //?per ORM commissionato o pikup 02
062400161104       //?anticipo se no da WEB
062500161104           WHEN  IOR97oao > wora1 and
062600161104                (IOR97com = 'S' or IOR97gf2 <> '01');
062700161104             IF  IOR97web <> 'S';
062800161104               OOR97dmin = Oggi;
062900161104             ENDIF;
063000161104       //?Se ora immissione ORM dopo la prima ora limite
063100161104       //?per ritiro in giornata
063200161104       //?(dopo le 11:00 ma prima della 12:30)
063300161104       //?anticipo
063400161104           WHEN  IOR97oao > wora1 and IOR97com <> 'S';
063500161104             OOR97dmin = Oggi;
063600161104       //?Se ora immissione ORM inferiore a prima ora limite
063700161104       //?per ritiro in giornata
063800161104       //?(entro le 11:00)
063900161104       //?per ORM commissionato con invio Alert
064000161104       //?anticipo, però poi NON viene inviato alert
064100161104           WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
064200161104                 IOR97alert = 'S';
064300161104             OOR97dmin = Oggi;
064400161104       //?Se ora immissione ORM inferiore a prima ora limite
064500161104       //?per ritiro in giornata
064600161104       //?(entro le 11:00)
064700161104       //?anticipo
064800161104           WHEN  IOR97oao <= wora1;
064900161104             OOR97dmin = Oggi;
065000161104         ENDSL;
065100161104
065200161104       ENDSR;
065300110613
065400110613       //--------------------------------------------------------------
065500110613       //?Operazioni finali.
065600110613       //--------------------------------------------------------------
065700110613       BEGSR RoutEnd;
065800170413
065900170413         IF  wprg867;
066000170413           wFIOR971DS = FIOR971DS;
066100170413         ENDIF;
066200110613
066300110613         *inLR = *on;
066400110613         return;
066500110613
066600110613       ENDSR;
066700110613
066800110613      /end-free
