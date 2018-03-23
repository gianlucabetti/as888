000100110613      //---------------------------------------------------------------
000200130906      //?FIOR97R - Calcolo Data Ritiro
000300110613      //---------------------------------------------------------------
000400080108     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000500080326     h dftactgrp(*no) actgrp(*caller)
000600110613
000700110613      //---------------------------------------------------------------
000800110613      //?Dichiarazione file.
000900110613      //---------------------------------------------------------------
001000110613
001100110613      //---------------------------------------------------------------
001200110613      //?Definizione costanti.
001300110613      //---------------------------------------------------------------
001400110613
001500110613      //---------------------------------------------------------------
001600110613      //?Definizione schiere.
001700110613      //---------------------------------------------------------------
001800110613
001900110613      //---------------------------------------------------------------
002000110613      //?Definizione aree dati.
002100110613      //---------------------------------------------------------------
002200110613
002300110613      //---------------------------------------------------------------
002400110613      //?Definizione strutture dati.
002500110613      //---------------------------------------------------------------
002600110613
002700110613      // - Parametri ricevuti
002800110613     d KPJBA         e ds
002900130906     d FIOR97ds      e ds
003000111205
003100130906      // - Ricerca/Controllo tabelle
003200130906     d TIBS02ds      e ds                  inz
003300131010
003400131010      // - Orari Servizi
003500140319     d TRULORSDS     e ds
003600140319     d TRULOR2ds     e ds
003700130906
003800130906      // - Cerco primo giorno lavorativo
003900130906     d TRUL13ds      e ds                  inz
004000160114
004100160114      // - Aggiunge/Toglie gg/mm dalla data
004200160114     d XGIOLAVDS     e ds                  inz
004300130906
004400130906      // - Tabella DFT
004500130906     d dDFT          e ds                  inz
004600131111
004700131111      // - Sk per filiali abilitate a Orari di Servizio
004800131111     d FilOkOrari      ds           256
004900131111     d  skFilOkOrari           1    255    dim(85)
005000110613
005100110613      //---------------------------------------------------------------
005200110613      //?Definizione variabili globali.
005300110613      //---------------------------------------------------------------
005400110616
005500111205      // - Campi di comodo
005600131111     d alleore         s                   like(IOR97oao)
005700130917     d dataiso         s               d   datfmt(*iso)
005800130917     d wdata           s                   like(OOR97dar)
005900160114     d wdatamax        s                   like(OOR97dar)
006000130917     d wgg             s              2  0
006100130917     d wora1           s              6  0
006200130917     d wora2           s              6  0
006300130917     d wora3           s              6  0
006400110613
006500110613      // - Flags booleani
006600130906     d wFine           s               n   inz(*off)
006700110613
006800110613      //---------------------------------------------------------------
006900110613      //?Definizione procedure esterne.
007000110613      //---------------------------------------------------------------
007100131010
007200131010      // - Ricerca orari di servizio
007300140319     d trulorsr        pr                  extpgm('TRULORSR')
007400140319     d  kpjba                              likeds(KPJBA)
007500140319     d  trulorsds                          likeds(TRULORSDS)
007600140319     d  trulor2ds                          likeds(TRULOR2DS)
007700140319     d                                     options (*nopass)
007800160114
007900160114      // - Aggiunge/Toglie gg/mm dalla data
008000160114     d XGIOLAV         pr                  extpgm('XGIOLAV')
008100160114     d  xgiolavds                          likeds(xgiolavds)
008200111205
008300111205      //---------------------------------------------------------------
008400111205      //?Definizione prototipi.
008500111205      //---------------------------------------------------------------
008600130906
008700130906      /copy gaitrasrc/srcprotopr,tibs02r
008800130906      /copy gaitrasrc/srcprotopr,trul13r
008900110613
009000110613      //---------------------------------------------------------------
009100110613      //?Definizione key-list.
009200110613      //---------------------------------------------------------------
009300110613
009400110613      //---------------------------------------------------------------
009500110613      //?Main.
009600110613      //---------------------------------------------------------------
009700110613
009800110613     c     *Entry        plist
009900110613     c                   parm                    KPJBA
010000130906     c                   parm                    fior97ds
010100110613
010200110613      /free
010300110613
010400110613       //?Operazioni iniziali
010500110613       exsr RoutInz;
010600110613
010700110613       //?Elaboro
010800130906       DOW  wFine = *off;
010900110613         exsr Elabora;
011000110613       ENDDO;
011100110613
011200110613       //?Operazioni finali
011300110613       exsr RoutEnd;
011400110613
011500110613       //--------------------------------------------------------------
011600110613       //?Operazioni iniziali.
011700110613       //--------------------------------------------------------------
011800110613       BEGSR RoutInz;
011900110613
012000130906         clear OOR97dar;
012100130917         clear OOR97dmin;
012200130917         clear OOR97dmaxb;
012300130917         clear OOR97dmaxf;
012400130906         clear OOR97err;
012500130906         clear OOR97msg;
012600110613
012700111205       //?Se non ho la Filiale Emissione esco dal pgm con errore
012800130906         IF  IOR97poe = *zeros;
012900130906           OOR97err = '1';
013000130906           OOR97msg = 'Filiale Emissione non presente';
013100130906           wFine = *on;
013200110613           leavesr;
013300110613         ENDIF;
013400110613
013500111205       //?Se non ho la Filile di Ritiro esco dal pgm con errore
013600130906         IF  IOR97por = *zeros;
013700130906           OOR97err = '1';
013800130906           OOR97msg = 'Filiale Ritiro non presente';
013900130906           wFine = *on;
014000110613           leavesr;
014100110613         ENDIF;
014200130906
014300130911       //?Se non ho la Data Immissione imposto data del sistema
014400130906         IF  IOR97dao = *zeros;
014500130911           IOR97dao = %dec(%date());
014600130906         ENDIF;
014700130906
014800130911       //?Se non ho l'Ora Immissione imposto ora del sistema
014900130906         IF  IOR97oao = *zeros;
015000130911           IOR97oao = %dec(%time());
015100130906         ENDIF;
015200130911
015300130911       //?Se non ho il codice tempo ritiro lo imposto a '01'
015400130911         IF  IOR97gf2 = *blanks;
015500130911           IOR97gf2 = '01';
015600130911         ENDIF;
015700130906
015800130917       //?Aggancio la tabella DFT con filiale ritiro ORM
015900130906         clear TIBS02ds;
016000130906         clear dDFT;
016100130906         T02mod = 'C';
016200130906         T02cod = 'DFT';
016300130909         T02sif = KNSIF;
016400130917         T02ke1 = %editc(IOR97por:'X');
016500130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
016600130906         IF  T02err = *blanks;
016700130906           dDFT = T02uni;
016800130906           leavesr;
016900130906         ENDIF;
017000110613
017100130917       //?Se arrivo qua non ho trovato DFT con filile ritiro ORM quindi
017200130906       //?aggancio la tabella DFT con filiale generica 999
017300130906         clear TIBS02ds;
017400130906         clear dDFT;
017500130906         T02mod = 'C';
017600130906         T02cod = 'DFT';
017700130909         T02sif = KNSIF;
017800130906         T02ke1 = '999';
017900130906         TNTBE_RicercaControllo (kpjba : tibs02ds);
018000130906         IF  T02err = *blanks;
018100130906           dDFT = T02uni;
018200130906         ENDIF;
018300140319
018400140319       //?Se non devo calcolare con pikup area metto fisso il flag = a '01'
018500140319         IF  d§DFTpkt <> *blanks;
018600140319           IOR97gf2 = '01';
018700140319         ENDIF;
018800130917
018900130917       //?Recupero orari limite ritiro in giornata con filiale ritiro
019000140319         clear trulorsds;
019100140319         clear trulor2ds;
019200140319         IOREfil = IOR97por;
019300140319         IOREcap = IOR97cap;
019400140319         IOREloc = IOR97loc;
019500140319         IOREdta = IOR97dao;
019600140319         IOREtser = 'R';
019700140319         IOREgf2 = IOR97gf2;
019800140319         trulorsr (kpjba:trulorsds:trulor2ds);
019900140319         IF  OOREerr <> *blanks or OOR2err <> *blanks;
020000140319           leavesr;
020100140319         ENDIF;
020200140319         wora1 = OOR2lrsc * 100;
020300140319         wora2 = OOR2lrnc * 100;
020400140319         wora3 = OOR2lrnk * 100;
020500130917
020600130917       //?Se gg in tab. DFT vuoti imposto fissi
020700130917         IF  d§DFTgg1 = *blanks;
020800160114           d§DFTgg1 = '10';
020900130917         ENDIF;
021000130917         IF  d§DFTgg2 = *blanks;
021100130917           d§DFTgg2 = '25';
021200130917         ENDIF;
021300130917
021400130917       //?Calcolo data massima bloccante
021500130917       //?da WEB
021600130917         IF  IOR97web = 'S';
021700130917           wgg = %dec(d§DFTgg1:2:0);
021800130917         ELSE;
021900130917       //?da AS400
022000130917           wgg = %dec(d§DFTgg2:2:0);
022100130917         ENDIF;
022200160114       //?Sommo i gg lavorativi alla data immissione ORM
022300160114         exsr CalcolaGG;
022400160114         OOR97dmaxb = wdatamax;
022500130917
022600130917       //?Calcolo data massima ritiro forzabile
022700130917       //?da WEB
022800130917         IF  IOR97web = 'S';
022900140407           OOR97dmaxf = OOR97dmaxb;
023000130917         ELSE;
023100130917       //?da AS400
023200130917           wgg = %dec(d§DFTgg1:2:0);
023300160114           exsr CalcolaGG;
023400160114           OOR97dmaxf = wdatamax;
023500130917         ENDIF;
023600130917
023700130917       //?Calcolo primo gg lavorativo da data immissione ORM
023800130917         clear TRUL13DS;
023900130917         I13mod = 'A';
024000130917         I13fil = IOR97por;
024100130917         I13din = IOR97dao;
024200130917         I13gio = 1;
024300130917         trul13r (trul13ds);
024400130917         IF  O13err <> *blanks;
024500130917           OOR97err = '1';
024600130917           OOR97msg = O13msg;
024700130917           leavesr;
024800130917         ENDIF;
024900130917         wdata = O13dfi;
025000130906
025100110613       ENDSR;
025200160114
025300160114       //--------------------------------------------------------------
025400160114       //?Calcola gg lavorativi da data immissione ORM
025500160114       //--------------------------------------------------------------
025600160114       BEGSR CalcolaGG;
025700160114
025800160114         clear XGIOLAVDS;
025900160114         IXGLdata = IOR97dao;
026000160114         IXGLadd  = 'S';
026100160114         IXGLgg   = wgg;
026200160114         IXGLlav  = 'S';
026300160114         IXGLpa   = 'P';
026400160114         IXGLfil  = IOR97por;
026500160114         XGIOLAV (xgiolavds);
026600160114         IF  OXGLerr = *blanks;
026700160114           wdatamax = OXGLdata;
026800160114         ELSE;
026900160114           wdatamax = IOR97dao;
027000160114         ENDIF;
027100160114
027200160114       ENDSR;
027300110613
027400110613       //--------------------------------------------------------------
027500110613       //?Elabora.
027600110613       //--------------------------------------------------------------
027700110613       BEGSR Elabora;
027800110613
027900130906         wFine = *on;
028000130917
028100130917         SELECT;
028200130917
028300130917       //?Se ORM prepagato o ORM con ricevuta di ritiro
028400130917       //?data ritiro sempre domani (primo giorno lavorativo)
028500130917           WHEN  IOR97tor = 'P' or IOR97srm = 'S';
028600130917             OOR97dar  = wdata;
028700140331             OOR97dmin = wdata;
028800130917
028900130917       //?Se ora immissione ORM dopo la terza ora limite
029000130917       //?per ritiro in giornata
029100130918       //?(dopo le 14:00)
029200130917       //?data ritiro sempre domani (primo giorno lavorativo)
029300130917           WHEN  IOR97oao > wora3;
029400130917             OOR97dar  = wdata;
029500130917             OOR97dmin = wdata;
029600130918
029700130918       //?Se ora immissione ORM dopo la seconda ora limite
029800130918       //?per ritiro in giornata
029900130918       //?(dopo le 12:30 ma prima della 14:00)
030000130918       //?per ORM commissionato o pikup 02
030100130918       //?data ritiro sempre domani (primo giorno lavorativo)
030200130918           WHEN  IOR97oao > wora2 and
030300130918                (IOR97com = 'S' or IOR97gf2 <> '01');
030400130918             OOR97dar  = wdata;
030500130918             OOR97dmin = wdata;
030600130918
030700130918       //?Se ora immissione ORM dopo la seconda ora limite
030800130918       //?per ritiro in giornata
030900130918       //?(dopo le 12:30 ma prima della 14:00)
031000130918       //?data ritiro = domani (primo giorno lavorativo)
031100130918           WHEN  IOR97oao > wora2;
031200130918             OOR97dar  = wdata;
031300130918             OOR97dmin = wdata;
031400130918         //?con possibilità di anticipare ad oggi se mittente codificato
031500140407         //?se ORM da WEB non posso anticipare
031600140407             IF  IOR97mcod = 'S' and IOR97web <> 'S';
031700130918               OOR97dmin = IOR97dao;
031800130918             ENDIF;
031900130918
032000130918       //?Se ora immissione ORM dopo la prima ora limite
032100130918       //?per ritiro in giornata
032200130918       //?(dopo le 11:00 ma prima della 12:30)
032300130918       //?per ORM commissionato o pikup 02
032400130918       //?data ritiro = domani (primo giorno lavorativo)
032500130918       //?con possibilità di anticipare ad oggi
032600130918           WHEN  IOR97oao > wora1 and
032700130918                (IOR97com = 'S' or IOR97gf2 <> '01');
032800130918             OOR97dar  = wdata;
032900140408         //?se ORM da WEB non posso anticipare
033000140408             IF  IOR97web = 'S';
033100140408               OOR97dmin = wdata;
033200140408             ELSE;
033300130918             OOR97dmin = IOR97dao;
033400140408             ENDIF;
033500130918
033600130918       //?Se ora immissione ORM dopo la prima ora limite
033700130918       //?per ritiro in giornata
033800130918       //?(dopo le 11:00 ma prima della 12:30)
033900130918       //?data ritiro sempre = oggi
034000130918           WHEN  IOR97oao > wora1 and IOR97com <> 'S';
034100130918             OOR97dar  = IOR97dao;
034200130918             OOR97dmin = IOR97dao;
034300140507
034400140507       //?Se ora immissione ORM inferiore a prima ora limite
034500140507       //?per ritiro in giornata
034600140507       //?(entro le 11:00)
034700140507       //?per ORM commissionato con invio Alert
034800140507       //?data ritiro = domani (primo giorno lavorativo)
034900140507       //?con possibilità di anticipare ad oggi, però poi NON viene inviato alert
035000140507           WHEN  IOR97oao <= wora1 and IOR97com = 'S' and
035100140507                 IOR97alert = 'S';
035200140507             OOR97dar  = wdata;
035300140507             OOR97dmin = IOR97dao;
035400130917
035500130917       //?Se ora immissione ORM inferiore a prima ora limite
035600130917       //?per ritiro in giornata
035700130918       //?(entro le 11:00)
035800130918       //?data ritiro = oggi
035900130918           WHEN  IOR97oao <= wora1;
036000130917             OOR97dar  = IOR97dao;
036100130917             OOR97dmin = IOR97dao;
036200130917         ENDSL;
036300110613
036400110613       ENDSR;
036500131111
036600131111       //--------------------------------------------------------------
036700131111       //?Calcola data ritiro
036800131111       //--------------------------------------------------------------
036900131111       BEGSR calDAR;
037000131111
037100131111       //?Prendo ora limite mattino
037200131111       //?se supera l'ora limite passo il ritiro al giorno dopo
037300131111       //?passo al giorno dopo anche se ORM con Ricevuta di Ritiro
037400131111       //?passo al giorno dopo anche se ORM Prepagato
037500131111         IF  IOR97oao > alleore or IOR97srm = 'S' or IOR97tor = 'P';
037600131111           clear TRUL13DS;
037700131111           I13mod = 'A';
037800131111           I13fil = IOR97por;
037900131111           I13din = IOR97dao;
038000131111           I13gio = 1;
038100131111           trul13r (trul13ds);
038200131111           IF  O13err <> *blanks;
038300131111             OOR97err = '1';
038400131111             OOR97msg = O13msg;
038500131111             leavesr;
038600131111           ENDIF;
038700131111           OOR97dar = O13dfi;
038800131111           OOR97dmin = O13dfi;
038900131111           leavesr;
039000131111         ENDIF;
039100131111
039200131111       //?Se non supera l'ora limite
039300131111       //?ritiro al pomeriggio dello stesso giorno
039400131111       //?passo al giorno dopo se ORM con Ricevuta di Ritiro
039500131111       //?passo al giorno dopo se ORM Prepagato
039600131111         OOR97dar = IOR97dao;
039700131111         OOR97dmin = IOR97dao;
039800131111
039900131111       ENDSR;
040000110613
040100110613       //--------------------------------------------------------------
040200110613       //?Operazioni finali.
040300110613       //--------------------------------------------------------------
040400110613       BEGSR RoutEnd;
040500110613
040600110613         *inLR = *on;
040700110613         return;
040800110613
040900110613       ENDSR;
041000110613
041100110613      /end-free
