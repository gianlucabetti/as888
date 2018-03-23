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
015200161104       //?e no se Data Pronta Merce > Oggi
015300161104       //?e se in tabella ho trovato che devo posticipare
015400161104       IF  wprg867 and IOR97pkg < d§DFTpkgdt and IOR97mcod <> 'S' and
015500161104           IOR97tor <> 'P' and IOR97srm <> 'S' and
015600161104           IOR97dpm <= Oggi and
015700161104           IOR97ocod <> 'S' and skGiorni(wDay_Of_Week) > 0;
015800170208         exsr NewElabora;
015900161104       ENDIF;
016000110613
016100110613       //?Operazioni finali
016200110613       exsr RoutEnd;
016300110613
016400110613       //--------------------------------------------------------------
016500110613       //?Operazioni iniziali.
016600110613       //--------------------------------------------------------------
016700110613       BEGSR RoutInz;
016800110613
016900130906         clear OOR97dar;
017000130917         clear OOR97dmin;
017100130917         clear OOR97dmaxb;
017200130917         clear OOR97dmaxf;
017300130906         clear OOR97err;
017400130906         clear OOR97msg;
017500161024
017600161024       //?Controllo se passato o meno il terzo parametro
017700161024         IF  %parms < 3;
017800161027           clear FIOR971DS;
017900161024           wprg867 = *off;
018000161024         ELSE;
018100161027           FIOR971DS = wFIOR971DS;
018200161024           wprg867 = *on;
018300161024         ENDIF;
018400110613
018500111205       //?Se non ho la Filiale Emissione esco dal pgm con errore
018600130906         IF  IOR97poe = *zeros;
018700130906           OOR97err = '1';
018800130906           OOR97msg = 'Filiale Emissione non presente';
018900161104           exsr Routend;
019000161104           return;
019100110613         ENDIF;
019200110613
019300111205       //?Se non ho la Filile di Ritiro esco dal pgm con errore
019400130906         IF  IOR97por = *zeros;
019500130906           OOR97err = '1';
019600130906           OOR97msg = 'Filiale Ritiro non presente';
019700161104           exsr Routend;
019800161104           return;
019900110613         ENDIF;
020000161024
020100130911       //?Se non ho la Data Immissione imposto data del sistema
020200130906         IF  IOR97dao = *zeros;
020300130911           IOR97dao = %dec(%date());
020400130906         ENDIF;
020500130906
020600130911       //?Se non ho l'Ora Immissione imposto ora del sistema
020700130906         IF  IOR97oao = *zeros;
020800130911           IOR97oao = %dec(%time());
020900130906         ENDIF;
021000130911
021100130911       //?Se non ho il codice tempo ritiro lo imposto a '01'
021200130911         IF  IOR97gf2 = *blanks;
021300130911           IOR97gf2 = '01';
021400130911         ENDIF;
021500161024
021600161024       //?Aggancio la tabella GPD con filiale ritiro ORM
021700161024         clear TIBS02ds;
021800161024         clear dGPD;
021900161024         T02mod = 'C';
022000161024         T02cod = 'GPD';
022100161024         T02sif = KNSIF;
022200161024         T02ke1 = %editc(IOR97por:'X');
022300161024         TNTBE_RicercaControllo (kpjba : tibs02ds);
022400161024         IF  T02err = *blanks;
022500161024           dGPD = T02uni;
022600161024         ELSE;
022700161024         //?Non trovo GPD con filiale ritiro
022800161024         //?la cerco con filiale generica 999
022900161024           clear TIBS02ds;
023000161024           T02mod = 'C';
023100161024           T02cod = 'GPD';
023200161024           T02sif = KNSIF;
023300161024           T02ke1 = '999';
023400161024           TNTBE_RicercaControllo (kpjba : tibs02ds);
023500161024           IF  T02err = *blanks;
023600161024             dGPD = T02uni;
023700161024           ENDIF;
023800161024         ENDIF;
023900130906
024000130917       //?Aggancio la tabella DFT con filiale ritiro ORM
024100130906         clear TIBS02ds;
024200130906         clear dDFT;
024300130906         T02mod = 'C';
024400130906         T02cod = 'DFT';
024500130909         T02sif = KNSIF;
024600130917         T02ke1 = %editc(IOR97por:'X');
024700130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
024800130906         IF  T02err = *blanks;
024900130906           dDFT = T02uni;
025000161027         ELSE;
025100161027       //?Se non ho trovato DFT con filile ritiro ORM
025200130906       //?aggancio la tabella DFT con filiale generica 999
025300130906         clear TIBS02ds;
025400130906         clear dDFT;
025500130906         T02mod = 'C';
025600130906         T02cod = 'DFT';
025700130909         T02sif = KNSIF;
025800130906         T02ke1 = '999';
025900130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
026000130906         IF  T02err = *blanks;
026100130906           dDFT = T02uni;
026200130906         ENDIF;
026300161027         ENDIF;
026400140319
026500140319       //?Se non devo calcolare con pikup area metto fisso il flag = a '01'
026600140319         IF  d§DFTpkt <> *blanks;
026700140319           IOR97gf2 = '01';
026800140319         ENDIF;
026900161109
027000161109       //?Se gg in tab. DFT vuoti imposto fissi
027100161109         IF  d§DFTgg1 = *blanks;
027200161109           d§DFTgg1 = '10';
027300161109         ENDIF;
027400161109         IF  d§DFTgg2 = *blanks;
027500161109           d§DFTgg2 = '25';
027600161109         ENDIF;
027700161109
027800161109       //?Imposto Oggi
027900161109         Oggi = %dec(%date());
028000130917
028100130917       //?Recupero orari limite ritiro in giornata con filiale ritiro
028200140319         clear trulorsds;
028300140319         clear trulor2ds;
028400140319         IOREfil = IOR97por;
028500140319         IOREcap = IOR97cap;
028600140319         IOREloc = IOR97loc;
028700161028         IF  IOR97dpm > IOR97dao;
028800161028           IOREdta = IOR97dpm;
028900161028         ELSE;
029000140319         IOREdta = IOR97dao;
029100161028         ENDIF;
029200140319         IOREtser = 'R';
029300140319         IOREgf2 = IOR97gf2;
029400140319         trulorsr (kpjba:trulorsds:trulor2ds);
029500140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
029600140319           leavesr;
029700140319         ENDIF;
029800140319         wora1 = OOR2lrsc * 100;
029900140319         wora2 = OOR2lrnc * 100;
030000140319         wora3 = OOR2lrnk * 100;
030100160526
030200160526       //?Recupero orari limite ritiro in giornata con filiale ritiro
030300160526       //?e nazione per ritiri estero
030400160526         IF  IOR97naz <> *blanks;
030500160526           clear FIORE00DS;
030600160526           IORE00por = IOR97por;
030700160526           IORE00nar = IOR97naz;
030800160526           fiore00r (fiore00ds);
030900160526           IF  OORE00err = *blanks;
031000160526             wora1 = OORE00lrg * 100;
031100160526             wora2 = OORE00lrg * 100;
031200160526             wora3 = OORE00lrg * 100;
031300160526           ENDIF;
031400160526         ENDIF;
031500161104
031600161104       ENDSR;
031700161104
031800161104       //--------------------------------------------------------------
031900161104       //?Calcolo Data Massimo ritiro.
032000161104       //--------------------------------------------------------------
032100161104       BEGSR CercoDataMassima;
032200130917
032300161104       //?Calcolo data massima ritiro bloccante da data immissione ORM
032400130917       //?da WEB
032500130917         IF  IOR97web = 'S';
032600130917           wgg = %dec(d§DFTgg1:2:0);
032700130917         ELSE;
032800130917       //?da AS400
032900130917           wgg = %dec(d§DFTgg2:2:0);
033000130917         ENDIF;
033100161104         wdao = IOR97dao;
033200160114         exsr CalcolaGG;
033300161104         OOR97dmaxb = wdar;
033400130917
033500161104       //?Calcolo data massima ritiro forzabile da data immissione ORM
033600130917       //?da WEB
033700130917         IF  IOR97web = 'S';
033800140407           OOR97dmaxf = OOR97dmaxb;
033900130917         ELSE;
034000130917       //?da AS400
034100130917           wgg = %dec(d§DFTgg1:2:0);
034200161104           wdao = IOR97dao;
034300160114           exsr CalcolaGG;
034400161104           OOR97dmaxf = wdar;
034500130917         ENDIF;
034600161104
034700161104       ENDSR;
034800161104
034900161104       //--------------------------------------------------------------
035000161104       //?Imposto Data di Calcolo.
035100161104       //--------------------------------------------------------------
035200161104       BEGSR CercoDataCalcolo;
035300161104
035400161104         DataCalcolo = Oggi;
035500161104
035600161104       //?Se data pronta merce è maggiore di oggi
035700161104       //?la data calcolo diventa la data pronta merce
035800161104         IF  IOR97dpm > 0 and IOR97dpm > Oggi;
035900161104           DataCalcolo = IOR97dpm;
036000161104         ENDIF;
036100161104
036200161104       ENDSR;
036300161104
036400161104       //--------------------------------------------------------------
036500161104       //?Preparo dati per calcolo data ritiro.
036600161104       //--------------------------------------------------------------
036700161104       BEGSR PreparaDati;
036800161104
036900161104         isFesta = *off;
037000130917
037100161104       //?Per il calcolo della data ritiro standard
037200161104       //?sommo 1 gg lavorativo alla data immissione ORM
037300161104         wdao = IOR97dao;
037400161104         wgg = 1;
037500161104         exsr CalcolaGG;
037600161104         DataRitiroStd = wdar;
037700161027
037800161104       //?Per il calcolo della data ritiro con tabella GPD
037900161104       //?devo cercare che giorno della settimana è la data calcolo
038000161104         dataiso = %date(datacalcolo);
038100161027         clear wDay_Of_Week;
038200161027         exec sql
038300161027         set :wDay_Of_Week = dayofweek_iso(:dataiso);
038400161104
038500161104       //?Se data pronta merce maggiore data immissione
038600161104       //?la data ritiro diventa la data pronta merce
038700161104       //?quindi controllo se la data pronta merce è festivo
038800161104         IF  IOR97dpm > IOR97dao;
038900161104           clear k_CLNtfp;
039000161104           k_CLNtfa = IOR97por;
039100161104           k_CLNann = %dec(%subst(%editc(IOR97dpm:'X'):1:4):4:0);
039200161104           k_CLNmes = %dec(%subst(%editc(IOR97dpm:'X'):5:2):2:0);
039300161104           giorno = %dec(%subst(%editc(IOR97dpm:'X'):7:2):2:0);
039400161104           chain %kds(k04azcln) AZCLN01L;
039500161104           IF  %found(AZCLN01L) and
039600161104              (mat(giorno) = 'F' or pom(giorno) = 'F');
039700161104             isFesta = *on;
039800161104           ENDIF;
039900161104         ENDIF;
040000130906
040100110613       ENDSR;
040200161024
040300161024       //--------------------------------------------------------------
040400161024       //?Elabora data ritiro per non cod. con peso < 5 Kg.
040500161024       //--------------------------------------------------------------
040600161024       BEGSR NewElabora;
040700170208
040800170208       //?Come prima cosa imposto la data minima = alla data ritiro calcolata
040900170208       //?in modo standard
041000170208         OOR97dmin = OOR97dar;
041100161027
041200161028       //?Calcolo xx giorni lavorativi da oggi o Data Pronta Merce
041300161104         wdao = DataCalcolo;
041400161104         wgg = skGiorni(wDay_Of_Week);
041500161104         exsr CalcolaGG;
041600161104         OOR97dar = wdar;
041700170208
041800170208       //?Se richiamato da WEB la data minima deve essere = alla data ritiro
041900170208       //?in quanto non diamo possibilità di anticipo
042000170209         IF  IOR97web = 'S';
042100170208           OOR97dmin = OOR97dar;
042200170208         ENDIF;
042300170315
042400170315       //?Se NON da WEB calcolo la data minima in base ai gg impostati in tabella
042500170315         IF  IOR97web <> 'S' and skGiorniA(wDay_Of_Week) > 0;
042600170315           wdao = OOR97dmin;
042700170316           wgg = skGiorniA(wDay_Of_Week);
042800170315           exsr CalcolaGG;
042900170315           OOR97dmin = wdar;
043000170315         ENDIF;
043100161024
043200161024       ENDSR;
043300110613
043400110613       //--------------------------------------------------------------
043500110613       //?Elabora.
043600110613       //--------------------------------------------------------------
043700110613       BEGSR Elabora;
043800161104
043900161104       //?Se Data Pronta Merce > Data Immissione
044000161104       //?Data Ritiro = Data Pronta Merce
044100161104         IF  IOR97dpm > IOR97dao;
044200161104           OOR97dar = IOR97dpm;
044300161104         //?Se Data Pronta Merce è festivo
044400161104         //?cerco il primo giorno lavorativo da data pronta merce
044500161104           IF  isFesta;
044600161104             wdao = IOR97dpm;
044700161104             wgg = 1;
044800161104             exsr CalcolaGG;
044900161104             OOR97dar = wdar;
045000161104           ENDIF;
045100161104           OOR97dmin = OOR97dar;
045200161104           leavesr;
045300161104         ENDIF;
045400161104
045500161104       //?Imposto subito la data ritiro con la data ritiro standard (domani)
045600170208         OOR97dar = DataRitiroStd;
045700130917
045800161104         SELECT;
045900130917       //?Se ORM prepagato o ORM con ricevuta di ritiro
046000130917       //?data ritiro sempre domani (primo giorno lavorativo)
046100161104         WHEN  IOR97tor = 'P' or IOR97srm = 'S';
046200130917       //?Se ora immissione ORM dopo la terza ora limite
046300130917       //?per ritiro in giornata
046400130918       //?(dopo le 14:00)
046500130917       //?data ritiro sempre domani (primo giorno lavorativo)
046600161104         WHEN  IOR97oao > wora3;
046700130918       //?Se ora immissione ORM dopo la seconda ora limite
046800130918       //?per ritiro in giornata
046900130918       //?(dopo le 12:30 ma prima della 14:00)
047000130918       //?per ORM commissionato o pikup 02
047100130918       //?data ritiro sempre domani (primo giorno lavorativo)
047200161104         WHEN  IOR97oao > wora2 and
047300161104              (IOR97com = 'S' or IOR97gf2 <> '01');
047400130918       //?Se ora immissione ORM dopo la seconda ora limite
047500130918       //?per ritiro in giornata
047600130918       //?(dopo le 12:30 ma prima della 14:00)
047700130918       //?data ritiro = domani (primo giorno lavorativo)
047800161104         WHEN  IOR97oao > wora2;
047900130918       //?Se ora immissione ORM dopo la prima ora limite
048000130918       //?per ritiro in giornata
048100130918       //?(dopo le 11:00 ma prima della 12:30)
048200130918       //?per ORM commissionato o pikup 02
048300130918       //?data ritiro = domani (primo giorno lavorativo)
048400161104         WHEN  IOR97oao > wora1 and
048500161104              (IOR97com = 'S' or IOR97gf2 <> '01');
048600130918       //?Se ora immissione ORM dopo la prima ora limite
048700130918       //?per ritiro in giornata
048800130918       //?(dopo le 11:00 ma prima della 12:30)
048900130918       //?data ritiro sempre = oggi
049000161104         WHEN  IOR97oao > wora1 and IOR97com <> 'S';
049100170208           OOR97dar = Oggi;
049200140507       //?Se ora immissione ORM inferiore a prima ora limite
049300140507       //?per ritiro in giornata
049400140507       //?(entro le 11:00)
049500140507       //?per ORM commissionato con invio Alert
049600140507       //?data ritiro = domani (primo giorno lavorativo)
049700161104         WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
049800161104               IOR97alert = 'S';
049900130917       //?Se ora immissione ORM inferiore a prima ora limite
050000130917       //?per ritiro in giornata
050100130918       //?(entro le 11:00)
050200130918       //?data ritiro = oggi
050300161104         WHEN  IOR97oao <= wora1;
050400170208           OOR97dar = Oggi;
050500130917         ENDSL;
050600161104
050700161104       //?Calcolo la data minima (possibilità di anticipo)
050800161104         exsr DataMinima;
050900110613
051000110613       ENDSR;
051100161104
051200161104       //--------------------------------------------------------------
051300161104       //?Calcola gg lavorativi da data immissione ORM
051400161104       //--------------------------------------------------------------
051500161104       BEGSR CalcolaGG;
051600161104
051700161104         clear XGIOLAVDS;
051800161104         IXGLdata = wdao;
051900161104         IXGLadd  = 'S';
052000161104         IXGLgg   = wgg;
052100161104         IXGLlav  = 'S';
052200161104         IXGLpa   = 'P';
052300161104         IXGLfil  = IOR97por;
052400161104         XGIOLAV (xgiolavds);
052500161104         IF  OXGLerr = *blanks;
052600161104           wdar = OXGLdata;
052700161104         ELSE;
052800161104           wdar = IOR97dao;
052900161104         ENDIF;
053000161104
053100161104       ENDSR;
053200161104
053300161104       //--------------------------------------------------------------
053400161104       //?Calcolo data minima (possibilità di anticipo).
053500161104       //--------------------------------------------------------------
053600161104       BEGSR DataMinima;
053700161104
053800161104       //?Imposto subito la data minima uguale alla data ritiro
053900161104       //?in questo modo non c'è anticipo
054000161104         OOR97dmin = DataRitiroStd;
054100161104
054200161104       //?Imposto quando posso anticipare
054300161104         SELECT;
054400161104       //?Se ora immissione ORM dopo la terza ora limite
054500161104       //?per ritiro in giornata
054600161104       //?(dopo le 14:00)
054700161104       //?no anticipo
054800161104           WHEN  IOR97oao > wora3;
054900161104       //?Se ora immissione ORM dopo la seconda ora limite
055000161104       //?per ritiro in giornata
055100161104       //?(dopo le 12:30 ma prima della 14:00)
055200161104       //?per ORM commissionato o pikup 02
055300161104       //?no anticipo
055400161104           WHEN  IOR97oao > wora2 and
055500161104                (IOR97com = 'S' or IOR97gf2 <> '01');
055600161104       //?Se ora immissione ORM dopo la seconda ora limite
055700161104       //?per ritiro in giornata
055800161104       //?(dopo le 12:30 ma prima della 14:00)
055900161104       //?anticipo se mittente Codificato e no da WEB
056000161104           WHEN  IOR97oao > wora2;
056100161104             IF  IOR97mcod = 'S' and IOR97web <> 'S';
056200161104               OOR97dmin = Oggi;
056300161104             ENDIF;
056400161104       //?Se ora immissione ORM dopo la prima ora limite
056500161104       //?per ritiro in giornata
056600161104       //?(dopo le 11:00 ma prima della 12:30)
056700161104       //?per ORM commissionato o pikup 02
056800161104       //?anticipo se no da WEB
056900161104           WHEN  IOR97oao > wora1 and
057000161104                (IOR97com = 'S' or IOR97gf2 <> '01');
057100161104             IF  IOR97web <> 'S';
057200161104               OOR97dmin = Oggi;
057300161104             ENDIF;
057400161104       //?Se ora immissione ORM dopo la prima ora limite
057500161104       //?per ritiro in giornata
057600161104       //?(dopo le 11:00 ma prima della 12:30)
057700161104       //?anticipo
057800161104           WHEN  IOR97oao > wora1 and IOR97com <> 'S';
057900161104             OOR97dmin = Oggi;
058000161104       //?Se ora immissione ORM inferiore a prima ora limite
058100161104       //?per ritiro in giornata
058200161104       //?(entro le 11:00)
058300161104       //?per ORM commissionato con invio Alert
058400161104       //?anticipo, però poi NON viene inviato alert
058500161104           WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
058600161104                 IOR97alert = 'S';
058700161104             OOR97dmin = Oggi;
058800161104       //?Se ora immissione ORM inferiore a prima ora limite
058900161104       //?per ritiro in giornata
059000161104       //?(entro le 11:00)
059100161104       //?anticipo
059200161104           WHEN  IOR97oao <= wora1;
059300161104             OOR97dmin = Oggi;
059400161104         ENDSL;
059500161104
059600161104       ENDSR;
059700110613
059800110613       //--------------------------------------------------------------
059900110613       //?Operazioni finali.
060000110613       //--------------------------------------------------------------
060100110613       BEGSR RoutEnd;
060200110613
060300110613         *inLR = *on;
060400110613         return;
060500110613
060600110613       ENDSR;
060700110613
060800110613      /end-free
