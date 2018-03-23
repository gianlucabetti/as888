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
005500131111
005600131111      // - Sk per filiali abilitate a Orari di Servizio
005700131111     d FilOkOrari      ds           256
005800131111     d  skFilOkOrari           1    255    dim(85)
005900161028
006000161028      // - Sk Mattina/Pomeriggio per gg calendario
006100161028     d CLNmat          ds
006200161028     d  mat                    1     31    dim(31)
006300161028     d CLNpom          ds
006400161028     d  pom                    1     31    dim(31)
006500110613
006600110613      //---------------------------------------------------------------
006700110613      //?Definizione variabili globali.
006800110613      //---------------------------------------------------------------
006900111205      // - Campi di comodo
007000131111     d alleore         s                   like(IOR97oao)
007100130917     d dataiso         s               d   datfmt(*iso)
007200161104     d DataCalcolo     s                   like(IOR97dao)
007300161104     d DataRitiroStd   s                   like(IOR97dao)
007400161028     d giorno          s              2s 0 inz
007500161024     d peso            s              7s 1 inz
007600161104     d Oggi            s              8s 0 inz
007700161104     d wdao            s                   like(IOR97dao)
007800161104     d wdar            s                   like(OOR97dar)
007900161024     d wDay_Of_Week    s              2  0 inz
008000130917     d wgg             s              2  0
008100130917     d wora1           s              6  0
008200130917     d wora2           s              6  0
008300130917     d wora3           s              6  0
008400161024     d xx              s              3  0 inz
008500110613
008600110613      // - Flags booleani
008700161104     d isFesta         s               n   inz(*off)
008800161024     d wprg867         s               n   inz(*off)
008900110613
009000110613      //---------------------------------------------------------------
009100110613      //?Definizione procedure esterne.
009200110613      //---------------------------------------------------------------
009300160526      // Ritorna Orari Servizio Ritiro Estero
009400160526     d fiore00r        pr                  extpgm('FIORE00R')
009500160526     d  fiore00ds                          likeds(FIORE00DS)
009600131010
009700131010      // - Ricerca orari di servizio
009800140319     d trulorsr        pr                  extpgm('TRULORSR')
009900140319     d  kpjba                              likeds(KPJBA)
010000140319     d  trulorsds                          likeds(TRULORSDS)
010100140319     d  trulor2ds                          likeds(TRULOR2DS)
010200140319     d                                     options (*nopass)
010300160114
010400160114      // - Aggiunge/Toglie gg/mm dalla data
010500160114     d XGIOLAV         pr                  extpgm('XGIOLAV')
010600160114     d  xgiolavds                          likeds(xgiolavds)
010700111205
010800111205      //---------------------------------------------------------------
010900111205      //?Definizione prototipi.
011000111205      //---------------------------------------------------------------
011100130906      /copy gaitrasrc/srcprotopr,tibs02r
011200130906      /copy gaitrasrc/srcprotopr,trul13r
011300110613
011400110613      //---------------------------------------------------------------
011500110613      //?Definizione key-list.
011600110613      //---------------------------------------------------------------
011700161028      // - File AZCLN01L
011800161028     d k04azcln      e ds                  extname(AZCLN01L:*key)
011900161028     d                                     prefix(k_)
012000161028     d                                     inz
012100110613
012200110613      //---------------------------------------------------------------
012300110613      //?Main.
012400110613      //---------------------------------------------------------------
012500110613
012600110613     c     *Entry        plist
012700110613     c                   parm                    KPJBA
012800130906     c                   parm                    fior97ds
012900161027     c                   parm                    wfior971ds
013000110613
013100110613      /free
013200110613
013300110613       //?Operazioni iniziali
013400110613       exsr RoutInz;
013500161104
013600161104       //?Calcolo data massimo ritiro
013700161104       exsr CercoDataMassima;
013800161104
013900161104       //?Imposto data dalla quale parto a calcolare la data ritiro
014000161104       exsr CercoDataCalcolo;
014100161104
014200161104       //?Preparo dati per calcolo data ritiro
014300161104       exsr PreparaDati;
014400110613
014500161104       //?Calcolo data ritiro con tabella GPD
014600161104       //?per Mittente e Ordinante NON codificato e peso inferiore a 5 Kg.
014700161104       //?ma NO Prepagato e no Ricevuta Ritiro
014800161104       //?e no se Data Pronta Merce > Oggi
014900161104       //?e se in tabella ho trovato che devo posticipare
015000161104       IF  wprg867 and IOR97pkg < d§DFTpkgdt and IOR97mcod <> 'S' and
015100161104           IOR97tor <> 'P' and IOR97srm <> 'S' and
015200161104           IOR97dpm <= Oggi and
015300161104           IOR97ocod <> 'S' and skGiorni(wDay_Of_Week) > 0;
015400161104         exsr NewElabora;
015500161104       ELSE;
015600161104       //?Calcolo data ritiro Standard
015700161104           exsr Elabora;
015800161104       ENDIF;
015900110613
016000110613       //?Operazioni finali
016100110613       exsr RoutEnd;
016200110613
016300110613       //--------------------------------------------------------------
016400110613       //?Operazioni iniziali.
016500110613       //--------------------------------------------------------------
016600110613       BEGSR RoutInz;
016700110613
016800130906         clear OOR97dar;
016900130917         clear OOR97dmin;
017000130917         clear OOR97dmaxb;
017100130917         clear OOR97dmaxf;
017200130906         clear OOR97err;
017300130906         clear OOR97msg;
017400161024
017500161024       //?Controllo se passato o meno il terzo parametro
017600161024         IF  %parms < 3;
017700161027           clear FIOR971DS;
017800161024           wprg867 = *off;
017900161024         ELSE;
018000161027           FIOR971DS = wFIOR971DS;
018100161024           wprg867 = *on;
018200161024         ENDIF;
018300110613
018400111205       //?Se non ho la Filiale Emissione esco dal pgm con errore
018500130906         IF  IOR97poe = *zeros;
018600130906           OOR97err = '1';
018700130906           OOR97msg = 'Filiale Emissione non presente';
018800161104           exsr Routend;
018900161104           return;
019000110613         ENDIF;
019100110613
019200111205       //?Se non ho la Filile di Ritiro esco dal pgm con errore
019300130906         IF  IOR97por = *zeros;
019400130906           OOR97err = '1';
019500130906           OOR97msg = 'Filiale Ritiro non presente';
019600161104           exsr Routend;
019700161104           return;
019800110613         ENDIF;
019900161024
020000130911       //?Se non ho la Data Immissione imposto data del sistema
020100130906         IF  IOR97dao = *zeros;
020200130911           IOR97dao = %dec(%date());
020300130906         ENDIF;
020400130906
020500130911       //?Se non ho l'Ora Immissione imposto ora del sistema
020600130906         IF  IOR97oao = *zeros;
020700130911           IOR97oao = %dec(%time());
020800130906         ENDIF;
020900130911
021000130911       //?Se non ho il codice tempo ritiro lo imposto a '01'
021100130911         IF  IOR97gf2 = *blanks;
021200130911           IOR97gf2 = '01';
021300130911         ENDIF;
021400161024
021500161024       //?Aggancio la tabella GPD con filiale ritiro ORM
021600161024         clear TIBS02ds;
021700161024         clear dGPD;
021800161024         T02mod = 'C';
021900161024         T02cod = 'GPD';
022000161024         T02sif = KNSIF;
022100161024         T02ke1 = %editc(IOR97por:'X');
022200161024         TNTBE_RicercaControllo (kpjba : tibs02ds);
022300161024         IF  T02err = *blanks;
022400161024           dGPD = T02uni;
022500161024         ELSE;
022600161024         //?Non trovo GPD con filiale ritiro
022700161024         //?la cerco con filiale generica 999
022800161024           clear TIBS02ds;
022900161024           T02mod = 'C';
023000161024           T02cod = 'GPD';
023100161024           T02sif = KNSIF;
023200161024           T02ke1 = '999';
023300161024           TNTBE_RicercaControllo (kpjba : tibs02ds);
023400161024           IF  T02err = *blanks;
023500161024             dGPD = T02uni;
023600161024           ENDIF;
023700161024         ENDIF;
023800130906
023900130917       //?Aggancio la tabella DFT con filiale ritiro ORM
024000130906         clear TIBS02ds;
024100130906         clear dDFT;
024200130906         T02mod = 'C';
024300130906         T02cod = 'DFT';
024400130909         T02sif = KNSIF;
024500130917         T02ke1 = %editc(IOR97por:'X');
024600130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
024700130906         IF  T02err = *blanks;
024800130906           dDFT = T02uni;
024900161027         ELSE;
025000161027       //?Se non ho trovato DFT con filile ritiro ORM
025100130906       //?aggancio la tabella DFT con filiale generica 999
025200130906         clear TIBS02ds;
025300130906         clear dDFT;
025400130906         T02mod = 'C';
025500130906         T02cod = 'DFT';
025600130909         T02sif = KNSIF;
025700130906         T02ke1 = '999';
025800130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
025900130906         IF  T02err = *blanks;
026000130906           dDFT = T02uni;
026100130906         ENDIF;
026200161027         ENDIF;
026300140319
026400140319       //?Se non devo calcolare con pikup area metto fisso il flag = a '01'
026500140319         IF  d§DFTpkt <> *blanks;
026600140319           IOR97gf2 = '01';
026700140319         ENDIF;
026800161109
026900161109       //?Se gg in tab. DFT vuoti imposto fissi
027000161109         IF  d§DFTgg1 = *blanks;
027100161109           d§DFTgg1 = '10';
027200161109         ENDIF;
027300161109         IF  d§DFTgg2 = *blanks;
027400161109           d§DFTgg2 = '25';
027500161109         ENDIF;
027600161109
027700161109       //?Imposto Oggi
027800161109         Oggi = %dec(%date());
027900130917
028000130917       //?Recupero orari limite ritiro in giornata con filiale ritiro
028100140319         clear trulorsds;
028200140319         clear trulor2ds;
028300140319         IOREfil = IOR97por;
028400140319         IOREcap = IOR97cap;
028500140319         IOREloc = IOR97loc;
028600161028         IF  IOR97dpm > IOR97dao;
028700161028           IOREdta = IOR97dpm;
028800161028         ELSE;
028900140319         IOREdta = IOR97dao;
029000161028         ENDIF;
029100140319         IOREtser = 'R';
029200140319         IOREgf2 = IOR97gf2;
029300140319         trulorsr (kpjba:trulorsds:trulor2ds);
029400140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
029500140319           leavesr;
029600140319         ENDIF;
029700140319         wora1 = OOR2lrsc * 100;
029800140319         wora2 = OOR2lrnc * 100;
029900140319         wora3 = OOR2lrnk * 100;
030000160526
030100160526       //?Recupero orari limite ritiro in giornata con filiale ritiro
030200160526       //?e nazione per ritiri estero
030300160526         IF  IOR97naz <> *blanks;
030400160526           clear FIORE00DS;
030500160526           IORE00por = IOR97por;
030600160526           IORE00nar = IOR97naz;
030700160526           fiore00r (fiore00ds);
030800160526           IF  OORE00err = *blanks;
030900160526             wora1 = OORE00lrg * 100;
031000160526             wora2 = OORE00lrg * 100;
031100160526             wora3 = OORE00lrg * 100;
031200160526           ENDIF;
031300160526         ENDIF;
031400161104
031500161104       ENDSR;
031600161104
031700161104       //--------------------------------------------------------------
031800161104       //?Calcolo Data Massimo ritiro.
031900161104       //--------------------------------------------------------------
032000161104       BEGSR CercoDataMassima;
032100130917
032200161104       //?Calcolo data massima ritiro bloccante da data immissione ORM
032300130917       //?da WEB
032400130917         IF  IOR97web = 'S';
032500130917           wgg = %dec(d§DFTgg1:2:0);
032600130917         ELSE;
032700130917       //?da AS400
032800130917           wgg = %dec(d§DFTgg2:2:0);
032900130917         ENDIF;
033000161104         wdao = IOR97dao;
033100160114         exsr CalcolaGG;
033200161104         OOR97dmaxb = wdar;
033300130917
033400161104       //?Calcolo data massima ritiro forzabile da data immissione ORM
033500130917       //?da WEB
033600130917         IF  IOR97web = 'S';
033700140407           OOR97dmaxf = OOR97dmaxb;
033800130917         ELSE;
033900130917       //?da AS400
034000130917           wgg = %dec(d§DFTgg1:2:0);
034100161104           wdao = IOR97dao;
034200160114           exsr CalcolaGG;
034300161104           OOR97dmaxf = wdar;
034400130917         ENDIF;
034500161104
034600161104       ENDSR;
034700161104
034800161104       //--------------------------------------------------------------
034900161104       //?Imposto Data di Calcolo.
035000161104       //--------------------------------------------------------------
035100161104       BEGSR CercoDataCalcolo;
035200161104
035300161104         DataCalcolo = Oggi;
035400161104
035500161104       //?Se data pronta merce è maggiore di oggi
035600161104       //?la data calcolo diventa la data pronta merce
035700161104         IF  IOR97dpm > 0 and IOR97dpm > Oggi;
035800161104           DataCalcolo = IOR97dpm;
035900161104         ENDIF;
036000161104
036100161104       ENDSR;
036200161104
036300161104       //--------------------------------------------------------------
036400161104       //?Preparo dati per calcolo data ritiro.
036500161104       //--------------------------------------------------------------
036600161104       BEGSR PreparaDati;
036700161104
036800161104         isFesta = *off;
036900130917
037000161104       //?Per il calcolo della data ritiro standard
037100161104       //?sommo 1 gg lavorativo alla data immissione ORM
037200161104         wdao = IOR97dao;
037300161104         wgg = 1;
037400161104         exsr CalcolaGG;
037500161104         DataRitiroStd = wdar;
037600161027
037700161104       //?Per il calcolo della data ritiro con tabella GPD
037800161104       //?devo cercare che giorno della settimana è la data calcolo
037900161104         dataiso = %date(datacalcolo);
038000161027         clear wDay_Of_Week;
038100161027         exec sql
038200161027         set :wDay_Of_Week = dayofweek_iso(:dataiso);
038300161104
038400161104       //?Se data pronta merce maggiore data immissione
038500161104       //?la data ritiro diventa la data pronta merce
038600161104       //?quindi controllo se la data pronta merce è festivo
038700161104         IF  IOR97dpm > IOR97dao;
038800161104           clear k_CLNtfp;
038900161104           k_CLNtfa = IOR97por;
039000161104           k_CLNann = %dec(%subst(%editc(IOR97dpm:'X'):1:4):4:0);
039100161104           k_CLNmes = %dec(%subst(%editc(IOR97dpm:'X'):5:2):2:0);
039200161104           giorno = %dec(%subst(%editc(IOR97dpm:'X'):7:2):2:0);
039300161104           chain %kds(k04azcln) AZCLN01L;
039400161104           IF  %found(AZCLN01L) and
039500161104              (mat(giorno) = 'F' or pom(giorno) = 'F');
039600161104             isFesta = *on;
039700161104           ENDIF;
039800161104         ENDIF;
039900130906
040000110613       ENDSR;
040100161024
040200161024       //--------------------------------------------------------------
040300161024       //?Elabora data ritiro per non cod. con peso < 5 Kg.
040400161024       //--------------------------------------------------------------
040500161024       BEGSR NewElabora;
040600161027
040700161028       //?Calcolo xx giorni lavorativi da oggi o Data Pronta Merce
040800161104         wdao = DataCalcolo;
040900161104         wgg = skGiorni(wDay_Of_Week);
041000161104         exsr CalcolaGG;
041100161104         OOR97dar = wdar;
041200161104         DataRitiroStd = wdar;
041300161027
041400161104       //?Calcolo la data minima (possibilità di anticipo)
041500161104         exsr DataMinima;
041600161024
041700161024       ENDSR;
041800110613
041900110613       //--------------------------------------------------------------
042000110613       //?Elabora.
042100110613       //--------------------------------------------------------------
042200110613       BEGSR Elabora;
042300161104
042400161104       //?Se Data Pronta Merce > Data Immissione
042500161104       //?Data Ritiro = Data Pronta Merce
042600161104         IF  IOR97dpm > IOR97dao;
042700161104           OOR97dar = IOR97dpm;
042800161104         //?Se Data Pronta Merce è festivo
042900161104         //?cerco il primo giorno lavorativo da data pronta merce
043000161104           IF  isFesta;
043100161104             wdao = IOR97dpm;
043200161104             wgg = 1;
043300161104             exsr CalcolaGG;
043400161104             OOR97dar = wdar;
043500161104           ENDIF;
043600161104           OOR97dmin = OOR97dar;
043700161104           leavesr;
043800161104         ENDIF;
043900161104
044000161104       //?Imposto subito la data ritiro con la data ritiro standard (domani)
044100161104         OOR97dar  = DataRitiroStd;
044200130917
044300161104         SELECT;
044400130917       //?Se ORM prepagato o ORM con ricevuta di ritiro
044500130917       //?data ritiro sempre domani (primo giorno lavorativo)
044600161104         WHEN  IOR97tor = 'P' or IOR97srm = 'S';
044700130917       //?Se ora immissione ORM dopo la terza ora limite
044800130917       //?per ritiro in giornata
044900130918       //?(dopo le 14:00)
045000130917       //?data ritiro sempre domani (primo giorno lavorativo)
045100161104         WHEN  IOR97oao > wora3;
045200130918       //?Se ora immissione ORM dopo la seconda ora limite
045300130918       //?per ritiro in giornata
045400130918       //?(dopo le 12:30 ma prima della 14:00)
045500130918       //?per ORM commissionato o pikup 02
045600130918       //?data ritiro sempre domani (primo giorno lavorativo)
045700161104         WHEN  IOR97oao > wora2 and
045800161104              (IOR97com = 'S' or IOR97gf2 <> '01');
045900130918       //?Se ora immissione ORM dopo la seconda ora limite
046000130918       //?per ritiro in giornata
046100130918       //?(dopo le 12:30 ma prima della 14:00)
046200130918       //?data ritiro = domani (primo giorno lavorativo)
046300161104         WHEN  IOR97oao > wora2;
046400130918       //?Se ora immissione ORM dopo la prima ora limite
046500130918       //?per ritiro in giornata
046600130918       //?(dopo le 11:00 ma prima della 12:30)
046700130918       //?per ORM commissionato o pikup 02
046800130918       //?data ritiro = domani (primo giorno lavorativo)
046900161104         WHEN  IOR97oao > wora1 and
047000161104              (IOR97com = 'S' or IOR97gf2 <> '01');
047100130918       //?Se ora immissione ORM dopo la prima ora limite
047200130918       //?per ritiro in giornata
047300130918       //?(dopo le 11:00 ma prima della 12:30)
047400130918       //?data ritiro sempre = oggi
047500161104         WHEN  IOR97oao > wora1 and IOR97com <> 'S';
047600161104           OOR97dar  = Oggi;
047700140507       //?Se ora immissione ORM inferiore a prima ora limite
047800140507       //?per ritiro in giornata
047900140507       //?(entro le 11:00)
048000140507       //?per ORM commissionato con invio Alert
048100140507       //?data ritiro = domani (primo giorno lavorativo)
048200161104         WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
048300161104               IOR97alert = 'S';
048400130917       //?Se ora immissione ORM inferiore a prima ora limite
048500130917       //?per ritiro in giornata
048600130918       //?(entro le 11:00)
048700130918       //?data ritiro = oggi
048800161104         WHEN  IOR97oao <= wora1;
048900161104           OOR97dar  = Oggi;
049000130917         ENDSL;
049100161104
049200161104       //?Calcolo la data minima (possibilità di anticipo)
049300161104         exsr DataMinima;
049400110613
049500110613       ENDSR;
049600161104
049700161104       //--------------------------------------------------------------
049800161104       //?Calcola gg lavorativi da data immissione ORM
049900161104       //--------------------------------------------------------------
050000161104       BEGSR CalcolaGG;
050100161104
050200161104         clear XGIOLAVDS;
050300161104         IXGLdata = wdao;
050400161104         IXGLadd  = 'S';
050500161104         IXGLgg   = wgg;
050600161104         IXGLlav  = 'S';
050700161104         IXGLpa   = 'P';
050800161104         IXGLfil  = IOR97por;
050900161104         XGIOLAV (xgiolavds);
051000161104         IF  OXGLerr = *blanks;
051100161104           wdar = OXGLdata;
051200161104         ELSE;
051300161104           wdar = IOR97dao;
051400161104         ENDIF;
051500161104
051600161104       ENDSR;
051700161104
051800161104       //--------------------------------------------------------------
051900161104       //?Calcolo data minima (possibilità di anticipo).
052000161104       //--------------------------------------------------------------
052100161104       BEGSR DataMinima;
052200161104
052300161104       //?Imposto subito la data minima uguale alla data ritiro
052400161104       //?in questo modo non c'è anticipo
052500161104         OOR97dmin = DataRitiroStd;
052600161104
052700161104       //?Imposto quando posso anticipare
052800161104         SELECT;
052900161104       //?Se ora immissione ORM dopo la terza ora limite
053000161104       //?per ritiro in giornata
053100161104       //?(dopo le 14:00)
053200161104       //?no anticipo
053300161104           WHEN  IOR97oao > wora3;
053400161104       //?Se ora immissione ORM dopo la seconda ora limite
053500161104       //?per ritiro in giornata
053600161104       //?(dopo le 12:30 ma prima della 14:00)
053700161104       //?per ORM commissionato o pikup 02
053800161104       //?no anticipo
053900161104           WHEN  IOR97oao > wora2 and
054000161104                (IOR97com = 'S' or IOR97gf2 <> '01');
054100161104       //?Se ora immissione ORM dopo la seconda ora limite
054200161104       //?per ritiro in giornata
054300161104       //?(dopo le 12:30 ma prima della 14:00)
054400161104       //?anticipo se mittente Codificato e no da WEB
054500161104           WHEN  IOR97oao > wora2;
054600161104             IF  IOR97mcod = 'S' and IOR97web <> 'S';
054700161104               OOR97dmin = Oggi;
054800161104             ENDIF;
054900161104       //?Se ora immissione ORM dopo la prima ora limite
055000161104       //?per ritiro in giornata
055100161104       //?(dopo le 11:00 ma prima della 12:30)
055200161104       //?per ORM commissionato o pikup 02
055300161104       //?anticipo se no da WEB
055400161104           WHEN  IOR97oao > wora1 and
055500161104                (IOR97com = 'S' or IOR97gf2 <> '01');
055600161104             IF  IOR97web <> 'S';
055700161104               OOR97dmin = Oggi;
055800161104             ENDIF;
055900161104       //?Se ora immissione ORM dopo la prima ora limite
056000161104       //?per ritiro in giornata
056100161104       //?(dopo le 11:00 ma prima della 12:30)
056200161104       //?anticipo
056300161104           WHEN  IOR97oao > wora1 and IOR97com <> 'S';
056400161104             OOR97dmin = Oggi;
056500161104       //?Se ora immissione ORM inferiore a prima ora limite
056600161104       //?per ritiro in giornata
056700161104       //?(entro le 11:00)
056800161104       //?per ORM commissionato con invio Alert
056900161104       //?anticipo, però poi NON viene inviato alert
057000161104           WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
057100161104                 IOR97alert = 'S';
057200161104             OOR97dmin = Oggi;
057300161104       //?Se ora immissione ORM inferiore a prima ora limite
057400161104       //?per ritiro in giornata
057500161104       //?(entro le 11:00)
057600161104       //?anticipo
057700161104           WHEN  IOR97oao <= wora1;
057800161104             OOR97dmin = Oggi;
057900161104         ENDSL;
058000161104
058100161104       ENDSR;
058200110613
058300110613       //--------------------------------------------------------------
058400110613       //?Operazioni finali.
058500110613       //--------------------------------------------------------------
058600110613       BEGSR RoutEnd;
058700110613
058800110613         *inLR = *on;
058900110613         return;
059000110613
059100110613       ENDSR;
059200110613
059300110613      /end-free
