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
003800131010
003900131010      // - Orari Servizi
004000140319     d TRULORSDS     e ds
004100140319     d TRULOR2ds     e ds
004200130906
004300130906      // - Cerco primo giorno lavorativo
004400130906     d TRUL13ds      e ds                  inz
004500160114
004600160114      // - Aggiunge/Toglie gg/mm dalla data
004700160114     d XGIOLAVDS     e ds                  inz
004800130906
004900130906      // - Tabella DFT
005000130906     d dDFT          e ds                  inz
005100161024
005200161024      // - Tabella GPD - Giorni Posticipo Data Ritiro
005300161024     d dGPD          e ds                  inz
005400161024     d  skGiorni               1     14s 0 dim(7)
005500170315     d  skGiorniA             15     28s 0 dim(7)
005600131111
005700131111      // - Sk per filiali abilitate a Orari di Servizio
005800131111     d FilOkOrari      ds           256
005900131111     d  skFilOkOrari           1    255    dim(85)
006000161028
006100161028      // - Sk Mattina/Pomeriggio per gg calendario
006200161028     d CLNmat          ds
006300161028     d  mat                    1     31    dim(31)
006400161028     d CLNpom          ds
006500161028     d  pom                    1     31    dim(31)
006600110613
006700110613      //---------------------------------------------------------------
006800110613      //?Definizione variabili globali.
006900110613      //---------------------------------------------------------------
007000111205      // - Campi di comodo
007100131111     d alleore         s                   like(IOR97oao)
007200130917     d dataiso         s               d   datfmt(*iso)
007300161104     d DataCalcolo     s                   like(IOR97dao)
007400161104     d DataRitiroStd   s                   like(IOR97dao)
007500161028     d giorno          s              2s 0 inz
007600161024     d peso            s              7s 1 inz
007700161104     d Oggi            s              8s 0 inz
007800161104     d wdao            s                   like(IOR97dao)
007900161104     d wdar            s                   like(OOR97dar)
008000161024     d wDay_Of_Week    s              2  0 inz
008100130917     d wgg             s              2  0
008200130917     d wora1           s              6  0
008300130917     d wora2           s              6  0
008400130917     d wora3           s              6  0
008500161024     d xx              s              3  0 inz
008600110613
008700110613      // - Flags booleani
008800161104     d isFesta         s               n   inz(*off)
008900161024     d wprg867         s               n   inz(*off)
009000110613
009100110613      //---------------------------------------------------------------
009200110613      //?Definizione procedure esterne.
009300110613      //---------------------------------------------------------------
009400160526      // Ritorna Orari Servizio Ritiro Estero
009500160526     d fiore00r        pr                  extpgm('FIORE00R')
009600160526     d  fiore00ds                          likeds(FIORE00DS)
009700131010
009800131010      // - Ricerca orari di servizio
009900140319     d trulorsr        pr                  extpgm('TRULORSR')
010000140319     d  kpjba                              likeds(KPJBA)
010100140319     d  trulorsds                          likeds(TRULORSDS)
010200140319     d  trulor2ds                          likeds(TRULOR2DS)
010300140319     d                                     options (*nopass)
010400160114
010500160114      // - Aggiunge/Toglie gg/mm dalla data
010600160114     d XGIOLAV         pr                  extpgm('XGIOLAV')
010700160114     d  xgiolavds                          likeds(xgiolavds)
010800111205
010900111205      //---------------------------------------------------------------
011000111205      //?Definizione prototipi.
011100111205      //---------------------------------------------------------------
011200130906      /copy gaitrasrc/srcprotopr,tibs02r
011300130906      /copy gaitrasrc/srcprotopr,trul13r
011400110613
011500110613      //---------------------------------------------------------------
011600110613      //?Definizione key-list.
011700110613      //---------------------------------------------------------------
011800161028      // - File AZCLN01L
011900161028     d k04azcln      e ds                  extname(AZCLN01L:*key)
012000161028     d                                     prefix(k_)
012100161028     d                                     inz
012200110613
012300110613      //---------------------------------------------------------------
012400110613      //?Main.
012500110613      //---------------------------------------------------------------
012600110613
012700110613     c     *Entry        plist
012800110613     c                   parm                    KPJBA
012900130906     c                   parm                    fior97ds
013000161027     c                   parm                    wfior971ds
013100110613
013200110613      /free
013300110613
013400110613       //?Operazioni iniziali
013500110613       exsr RoutInz;
013600161104
013700161104       //?Calcolo data massimo ritiro
013800161104       exsr CercoDataMassima;
013900161104
014000161104       //?Imposto data dalla quale parto a calcolare la data ritiro
014100161104       exsr CercoDataCalcolo;
014200161104
014300161104       //?Preparo dati per calcolo data ritiro
014400161104       exsr PreparaDati;
014500110613
014600170208       //?Calcolo la data ritiro in maniera Standard
014700170208       exsr Elabora;
014800170208
014900161104       //?Calcolo data ritiro con tabella GPD
015000161104       //?per Mittente e Ordinante NON codificato e peso inferiore a 5 Kg.
015100161104       //?ma NO Prepagato e no Ricevuta Ritiro
015200170522       //?e NO orm Export
015300161104       //?e no se Data Pronta Merce > Oggi
015400161104       //?e se in tabella ho trovato che devo posticipare
015500161104       IF  wprg867 and IOR97pkg < d§DFTpkgdt and IOR97mcod <> 'S' and
015600161104           IOR97tor <> 'P' and IOR97srm <> 'S' and
015700170522           IOR97naz = *blanks and
015800161104           IOR97dpm <= Oggi and
015900161104           IOR97ocod <> 'S' and skGiorni(wDay_Of_Week) > 0;
016000170208         exsr NewElabora;
016100170524       //?ritorno al chiamante che l'ORM è da posticipare
016200170524         OOR97postd = 'S';
016300161104       ENDIF;
016400110613
016500110613       //?Operazioni finali
016600110613       exsr RoutEnd;
016700110613
016800110613       //--------------------------------------------------------------
016900110613       //?Operazioni iniziali.
017000110613       //--------------------------------------------------------------
017100110613       BEGSR RoutInz;
017200110613
017300130906         clear OOR97dar;
017400130917         clear OOR97dmin;
017500130917         clear OOR97dmaxb;
017600130917         clear OOR97dmaxf;
017700130906         clear OOR97err;
017800130906         clear OOR97msg;
017900161024
018000161024       //?Controllo se passato o meno il terzo parametro
018100161024         IF  %parms < 3;
018200161027           clear FIOR971DS;
018300161024           wprg867 = *off;
018400161024         ELSE;
018500161027           FIOR971DS = wFIOR971DS;
018600161024           wprg867 = *on;
018700161024         ENDIF;
018800110613
018900111205       //?Se non ho la Filiale Emissione esco dal pgm con errore
019000130906         IF  IOR97poe = *zeros;
019100130906           OOR97err = '1';
019200130906           OOR97msg = 'Filiale Emissione non presente';
019300161104           exsr Routend;
019400161104           return;
019500110613         ENDIF;
019600110613
019700111205       //?Se non ho la Filile di Ritiro esco dal pgm con errore
019800130906         IF  IOR97por = *zeros;
019900130906           OOR97err = '1';
020000130906           OOR97msg = 'Filiale Ritiro non presente';
020100161104           exsr Routend;
020200161104           return;
020300110613         ENDIF;
020400161024
020500130911       //?Se non ho la Data Immissione imposto data del sistema
020600130906         IF  IOR97dao = *zeros;
020700130911           IOR97dao = %dec(%date());
020800130906         ENDIF;
020900130906
021000130911       //?Se non ho l'Ora Immissione imposto ora del sistema
021100130906         IF  IOR97oao = *zeros;
021200130911           IOR97oao = %dec(%time());
021300130906         ENDIF;
021400130911
021500130911       //?Se non ho il codice tempo ritiro lo imposto a '01'
021600130911         IF  IOR97gf2 = *blanks;
021700130911           IOR97gf2 = '01';
021800130911         ENDIF;
021900161024
022000161024       //?Aggancio la tabella GPD con filiale ritiro ORM
022100161024         clear TIBS02ds;
022200161024         clear dGPD;
022300161024         T02mod = 'C';
022400161024         T02cod = 'GPD';
022500161024         T02sif = KNSIF;
022600161024         T02ke1 = %editc(IOR97por:'X');
022700161024         TNTBE_RicercaControllo (kpjba : tibs02ds);
022800161024         IF  T02err = *blanks;
022900161024           dGPD = T02uni;
023000161024         ELSE;
023100161024         //?Non trovo GPD con filiale ritiro
023200161024         //?la cerco con filiale generica 999
023300161024           clear TIBS02ds;
023400161024           T02mod = 'C';
023500161024           T02cod = 'GPD';
023600161024           T02sif = KNSIF;
023700161024           T02ke1 = '999';
023800161024           TNTBE_RicercaControllo (kpjba : tibs02ds);
023900161024           IF  T02err = *blanks;
024000161024             dGPD = T02uni;
024100161024           ENDIF;
024200161024         ENDIF;
024300130906
024400130917       //?Aggancio la tabella DFT con filiale ritiro ORM
024500130906         clear TIBS02ds;
024600130906         clear dDFT;
024700130906         T02mod = 'C';
024800130906         T02cod = 'DFT';
024900130909         T02sif = KNSIF;
025000130917         T02ke1 = %editc(IOR97por:'X');
025100130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
025200130906         IF  T02err = *blanks;
025300130906           dDFT = T02uni;
025400161027         ELSE;
025500161027       //?Se non ho trovato DFT con filile ritiro ORM
025600130906       //?aggancio la tabella DFT con filiale generica 999
025700130906         clear TIBS02ds;
025800130906         clear dDFT;
025900130906         T02mod = 'C';
026000130906         T02cod = 'DFT';
026100130909         T02sif = KNSIF;
026200130906         T02ke1 = '999';
026300130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
026400130906         IF  T02err = *blanks;
026500130906           dDFT = T02uni;
026600130906         ENDIF;
026700161027         ENDIF;
026800140319
026900140319       //?Se non devo calcolare con pikup area metto fisso il flag = a '01'
027000140319         IF  d§DFTpkt <> *blanks;
027100140319           IOR97gf2 = '01';
027200140319         ENDIF;
027300161109
027400161109       //?Se gg in tab. DFT vuoti imposto fissi
027500161109         IF  d§DFTgg1 = *blanks;
027600161109           d§DFTgg1 = '10';
027700161109         ENDIF;
027800161109         IF  d§DFTgg2 = *blanks;
027900161109           d§DFTgg2 = '25';
028000161109         ENDIF;
028100161109
028200161109       //?Imposto Oggi
028300161109         Oggi = %dec(%date());
028400130917
028500130917       //?Recupero orari limite ritiro in giornata con filiale ritiro
028600140319         clear trulorsds;
028700140319         clear trulor2ds;
028800140319         IOREfil = IOR97por;
028900140319         IOREcap = IOR97cap;
029000140319         IOREloc = IOR97loc;
029100161028         IF  IOR97dpm > IOR97dao;
029200161028           IOREdta = IOR97dpm;
029300161028         ELSE;
029400140319         IOREdta = IOR97dao;
029500161028         ENDIF;
029600140319         IOREtser = 'R';
029700140319         IOREgf2 = IOR97gf2;
029800140319         trulorsr (kpjba:trulorsds:trulor2ds);
029900140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
030000140319           leavesr;
030100140319         ENDIF;
030200140319         wora1 = OOR2lrsc * 100;
030300140319         wora2 = OOR2lrnc * 100;
030400140319         wora3 = OOR2lrnk * 100;
030500160526
030600160526       //?Recupero orari limite ritiro in giornata con filiale ritiro
030700160526       //?e nazione per ritiri estero
030800160526         IF  IOR97naz <> *blanks;
030900160526           clear FIORE00DS;
031000160526           IORE00por = IOR97por;
031100160526           IORE00nar = IOR97naz;
031200160526           fiore00r (fiore00ds);
031300160526           IF  OORE00err = *blanks;
031400160526             wora1 = OORE00lrg * 100;
031500160526             wora2 = OORE00lrg * 100;
031600160526             wora3 = OORE00lrg * 100;
031700160526           ENDIF;
031800160526         ENDIF;
031900161104
032000161104       ENDSR;
032100161104
032200161104       //--------------------------------------------------------------
032300161104       //?Calcolo Data Massimo ritiro.
032400161104       //--------------------------------------------------------------
032500161104       BEGSR CercoDataMassima;
032600130917
032700161104       //?Calcolo data massima ritiro bloccante da data immissione ORM
032800130917       //?da WEB
032900130917         IF  IOR97web = 'S';
033000130917           wgg = %dec(d§DFTgg1:2:0);
033100130917         ELSE;
033200130917       //?da AS400
033300130917           wgg = %dec(d§DFTgg2:2:0);
033400130917         ENDIF;
033500161104         wdao = IOR97dao;
033600160114         exsr CalcolaGG;
033700161104         OOR97dmaxb = wdar;
033800130917
033900161104       //?Calcolo data massima ritiro forzabile da data immissione ORM
034000130917       //?da WEB
034100130917         IF  IOR97web = 'S';
034200140407           OOR97dmaxf = OOR97dmaxb;
034300130917         ELSE;
034400130917       //?da AS400
034500130917           wgg = %dec(d§DFTgg1:2:0);
034600161104           wdao = IOR97dao;
034700160114           exsr CalcolaGG;
034800161104           OOR97dmaxf = wdar;
034900130917         ENDIF;
035000161104
035100161104       ENDSR;
035200161104
035300161104       //--------------------------------------------------------------
035400161104       //?Imposto Data di Calcolo.
035500161104       //--------------------------------------------------------------
035600161104       BEGSR CercoDataCalcolo;
035700161104
035800161104         DataCalcolo = Oggi;
035900161104
036000161104       //?Se data pronta merce è maggiore di oggi
036100161104       //?la data calcolo diventa la data pronta merce
036200161104         IF  IOR97dpm > 0 and IOR97dpm > Oggi;
036300161104           DataCalcolo = IOR97dpm;
036400161104         ENDIF;
036500161104
036600161104       ENDSR;
036700161104
036800161104       //--------------------------------------------------------------
036900161104       //?Preparo dati per calcolo data ritiro.
037000161104       //--------------------------------------------------------------
037100161104       BEGSR PreparaDati;
037200161104
037300161104         isFesta = *off;
037400130917
037500161104       //?Per il calcolo della data ritiro standard
037600161104       //?sommo 1 gg lavorativo alla data immissione ORM
037700161104         wdao = IOR97dao;
037800161104         wgg = 1;
037900161104         exsr CalcolaGG;
038000161104         DataRitiroStd = wdar;
038100161027
038200161104       //?Per il calcolo della data ritiro con tabella GPD
038300161104       //?devo cercare che giorno della settimana è la data calcolo
038400161104         dataiso = %date(datacalcolo);
038500161027         clear wDay_Of_Week;
038600161027         exec sql
038700161027         set :wDay_Of_Week = dayofweek_iso(:dataiso);
038800161104
038900161104       //?Se data pronta merce maggiore data immissione
039000161104       //?la data ritiro diventa la data pronta merce
039100161104       //?quindi controllo se la data pronta merce è festivo
039200161104         IF  IOR97dpm > IOR97dao;
039300161104           clear k_CLNtfp;
039400161104           k_CLNtfa = IOR97por;
039500161104           k_CLNann = %dec(%subst(%editc(IOR97dpm:'X'):1:4):4:0);
039600161104           k_CLNmes = %dec(%subst(%editc(IOR97dpm:'X'):5:2):2:0);
039700161104           giorno = %dec(%subst(%editc(IOR97dpm:'X'):7:2):2:0);
039800161104           chain %kds(k04azcln) AZCLN01L;
039900161104           IF  %found(AZCLN01L) and
040000161104              (mat(giorno) = 'F' or pom(giorno) = 'F');
040100161104             isFesta = *on;
040200161104           ENDIF;
040300161104         ENDIF;
040400130906
040500110613       ENDSR;
040600161024
040700161024       //--------------------------------------------------------------
040800161024       //?Elabora data ritiro per non cod. con peso < 5 Kg.
040900161024       //--------------------------------------------------------------
041000161024       BEGSR NewElabora;
041100170208
041200170208       //?Come prima cosa imposto la data minima = alla data ritiro calcolata
041300170208       //?in modo standard
041400170208         OOR97dmin = OOR97dar;
041500161027
041600161028       //?Calcolo xx giorni lavorativi da oggi o Data Pronta Merce
041700161104         wdao = DataCalcolo;
041800161104         wgg = skGiorni(wDay_Of_Week);
041900161104         exsr CalcolaGG;
042000161104         OOR97dar = wdar;
042100170208
042200170208       //?Se richiamato da WEB la data minima deve essere = alla data ritiro
042300170208       //?in quanto non diamo possibilità di anticipo
042400170209         IF  IOR97web = 'S';
042500170208           OOR97dmin = OOR97dar;
042600170208         ENDIF;
042700170315
042800170315       //?Se NON da WEB calcolo la data minima in base ai gg impostati in tabella
042900170315         IF  IOR97web <> 'S' and skGiorniA(wDay_Of_Week) > 0;
043000170315           wdao = OOR97dmin;
043100170316           wgg = skGiorniA(wDay_Of_Week);
043200170315           exsr CalcolaGG;
043300170315           OOR97dmin = wdar;
043400170315         ENDIF;
043500161024
043600161024       ENDSR;
043700110613
043800110613       //--------------------------------------------------------------
043900110613       //?Elabora.
044000110613       //--------------------------------------------------------------
044100110613       BEGSR Elabora;
044200161104
044300161104       //?Se Data Pronta Merce > Data Immissione
044400161104       //?Data Ritiro = Data Pronta Merce
044500161104         IF  IOR97dpm > IOR97dao;
044600161104           OOR97dar = IOR97dpm;
044700161104         //?Se Data Pronta Merce è festivo
044800161104         //?cerco il primo giorno lavorativo da data pronta merce
044900161104           IF  isFesta;
045000161104             wdao = IOR97dpm;
045100161104             wgg = 1;
045200161104             exsr CalcolaGG;
045300161104             OOR97dar = wdar;
045400161104           ENDIF;
045500161104           OOR97dmin = OOR97dar;
045600161104           leavesr;
045700161104         ENDIF;
045800161104
045900161104       //?Imposto subito la data ritiro con la data ritiro standard (domani)
046000170208         OOR97dar = DataRitiroStd;
046100130917
046200161104         SELECT;
046300130917       //?Se ORM prepagato o ORM con ricevuta di ritiro
046400130917       //?data ritiro sempre domani (primo giorno lavorativo)
046500161104         WHEN  IOR97tor = 'P' or IOR97srm = 'S';
046600130917       //?Se ora immissione ORM dopo la terza ora limite
046700130917       //?per ritiro in giornata
046800130918       //?(dopo le 14:00)
046900130917       //?data ritiro sempre domani (primo giorno lavorativo)
047000161104         WHEN  IOR97oao > wora3;
047100130918       //?Se ora immissione ORM dopo la seconda ora limite
047200130918       //?per ritiro in giornata
047300130918       //?(dopo le 12:30 ma prima della 14:00)
047400130918       //?per ORM commissionato o pikup 02
047500130918       //?data ritiro sempre domani (primo giorno lavorativo)
047600161104         WHEN  IOR97oao > wora2 and
047700161104              (IOR97com = 'S' or IOR97gf2 <> '01');
047800130918       //?Se ora immissione ORM dopo la seconda ora limite
047900130918       //?per ritiro in giornata
048000130918       //?(dopo le 12:30 ma prima della 14:00)
048100130918       //?data ritiro = domani (primo giorno lavorativo)
048200161104         WHEN  IOR97oao > wora2;
048300130918       //?Se ora immissione ORM dopo la prima ora limite
048400130918       //?per ritiro in giornata
048500130918       //?(dopo le 11:00 ma prima della 12:30)
048600130918       //?per ORM commissionato o pikup 02
048700130918       //?data ritiro = domani (primo giorno lavorativo)
048800161104         WHEN  IOR97oao > wora1 and
048900161104              (IOR97com = 'S' or IOR97gf2 <> '01');
049000130918       //?Se ora immissione ORM dopo la prima ora limite
049100130918       //?per ritiro in giornata
049200130918       //?(dopo le 11:00 ma prima della 12:30)
049300130918       //?data ritiro sempre = oggi
049400161104         WHEN  IOR97oao > wora1 and IOR97com <> 'S';
049500170208           OOR97dar = Oggi;
049600140507       //?Se ora immissione ORM inferiore a prima ora limite
049700140507       //?per ritiro in giornata
049800140507       //?(entro le 11:00)
049900140507       //?per ORM commissionato con invio Alert
050000140507       //?data ritiro = domani (primo giorno lavorativo)
050100161104         WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
050200161104               IOR97alert = 'S';
050300130917       //?Se ora immissione ORM inferiore a prima ora limite
050400130917       //?per ritiro in giornata
050500130918       //?(entro le 11:00)
050600130918       //?data ritiro = oggi
050700161104         WHEN  IOR97oao <= wora1;
050800170208           OOR97dar = Oggi;
050900130917         ENDSL;
051000161104
051100161104       //?Calcolo la data minima (possibilità di anticipo)
051200161104         exsr DataMinima;
051300110613
051400110613       ENDSR;
051500161104
051600161104       //--------------------------------------------------------------
051700161104       //?Calcola gg lavorativi da data immissione ORM
051800161104       //--------------------------------------------------------------
051900161104       BEGSR CalcolaGG;
052000161104
052100161104         clear XGIOLAVDS;
052200161104         IXGLdata = wdao;
052300161104         IXGLadd  = 'S';
052400161104         IXGLgg   = wgg;
052500161104         IXGLlav  = 'S';
052600161104         IXGLpa   = 'P';
052700161104         IXGLfil  = IOR97por;
052800161104         XGIOLAV (xgiolavds);
052900161104         IF  OXGLerr = *blanks;
053000161104           wdar = OXGLdata;
053100161104         ELSE;
053200161104           wdar = IOR97dao;
053300161104         ENDIF;
053400161104
053500161104       ENDSR;
053600161104
053700161104       //--------------------------------------------------------------
053800161104       //?Calcolo data minima (possibilità di anticipo).
053900161104       //--------------------------------------------------------------
054000161104       BEGSR DataMinima;
054100161104
054200161104       //?Imposto subito la data minima uguale alla data ritiro
054300161104       //?in questo modo non c'è anticipo
054400161104         OOR97dmin = DataRitiroStd;
054500161104
054600161104       //?Imposto quando posso anticipare
054700161104         SELECT;
054800161104       //?Se ora immissione ORM dopo la terza ora limite
054900161104       //?per ritiro in giornata
055000161104       //?(dopo le 14:00)
055100161104       //?no anticipo
055200161104           WHEN  IOR97oao > wora3;
055300161104       //?Se ora immissione ORM dopo la seconda ora limite
055400161104       //?per ritiro in giornata
055500161104       //?(dopo le 12:30 ma prima della 14:00)
055600161104       //?per ORM commissionato o pikup 02
055700161104       //?no anticipo
055800161104           WHEN  IOR97oao > wora2 and
055900161104                (IOR97com = 'S' or IOR97gf2 <> '01');
056000161104       //?Se ora immissione ORM dopo la seconda ora limite
056100161104       //?per ritiro in giornata
056200161104       //?(dopo le 12:30 ma prima della 14:00)
056300161104       //?anticipo se mittente Codificato e no da WEB
056400161104           WHEN  IOR97oao > wora2;
056500161104             IF  IOR97mcod = 'S' and IOR97web <> 'S';
056600161104               OOR97dmin = Oggi;
056700161104             ENDIF;
056800161104       //?Se ora immissione ORM dopo la prima ora limite
056900161104       //?per ritiro in giornata
057000161104       //?(dopo le 11:00 ma prima della 12:30)
057100161104       //?per ORM commissionato o pikup 02
057200161104       //?anticipo se no da WEB
057300161104           WHEN  IOR97oao > wora1 and
057400161104                (IOR97com = 'S' or IOR97gf2 <> '01');
057500161104             IF  IOR97web <> 'S';
057600161104               OOR97dmin = Oggi;
057700161104             ENDIF;
057800161104       //?Se ora immissione ORM dopo la prima ora limite
057900161104       //?per ritiro in giornata
058000161104       //?(dopo le 11:00 ma prima della 12:30)
058100161104       //?anticipo
058200161104           WHEN  IOR97oao > wora1 and IOR97com <> 'S';
058300161104             OOR97dmin = Oggi;
058400161104       //?Se ora immissione ORM inferiore a prima ora limite
058500161104       //?per ritiro in giornata
058600161104       //?(entro le 11:00)
058700161104       //?per ORM commissionato con invio Alert
058800161104       //?anticipo, però poi NON viene inviato alert
058900161104           WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
059000161104                 IOR97alert = 'S';
059100161104             OOR97dmin = Oggi;
059200161104       //?Se ora immissione ORM inferiore a prima ora limite
059300161104       //?per ritiro in giornata
059400161104       //?(entro le 11:00)
059500161104       //?anticipo
059600161104           WHEN  IOR97oao <= wora1;
059700161104             OOR97dmin = Oggi;
059800161104         ENDSL;
059900161104
060000161104       ENDSR;
060100110613
060200110613       //--------------------------------------------------------------
060300110613       //?Operazioni finali.
060400110613       //--------------------------------------------------------------
060500110613       BEGSR RoutEnd;
060600170413
060700170413         IF  wprg867;
060800170413           wFIOR971DS = FIOR971DS;
060900170413         ENDIF;
061000110613
061100110613         *inLR = *on;
061200110613         return;
061300110613
061400110613       ENDSR;
061500110613
061600110613      /end-free
