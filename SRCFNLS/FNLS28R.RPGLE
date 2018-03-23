000100090511     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200090511      //---------------------------------------------------------------
000300090511      //?FNLS28R - Controllo/Scrittura ORM per bolla RC
000400090511      //---------------------------------------------------------------
000500090511
000600090511      //---------------------------------------------------------------
000700090511      //?Dichiarazione file.
000800090511      //---------------------------------------------------------------
000900090512     ftntbe01l  uf   e           k disk
001000090512     ffnacr01l  if   e           k disk
001100091119     ffnorm07l  if   e           k disk
001200090511
001300090511      //---------------------------------------------------------------
001400090511      //?Definizione costanti.
001500090511      //---------------------------------------------------------------
001600090513
001700090525      // - Libreria
001800090525     d c_lib           c                   const('GAITRAGRU')
001900090525     d c_libp          c                   const('GAITRAGRPS')
002000090512
002100090512      // - Tabella OSR
002200090512     d c_tabosr        c                   const('OSR')
002300090511
002400090511      //---------------------------------------------------------------
002500090511      //?Definizione schiere.
002600090511      //---------------------------------------------------------------
002700090512     d msg             s             78    dim(10) ctdata perrcd(1)
002800130116     d skCLIosr        s              7  0 dim(900)
002900130116     d skLNPNRSosr     s              5    dim(900)
003000130116
003100090511      //---------------------------------------------------------------
003200090511      //?Definizione aree dati.
003300090511      //---------------------------------------------------------------
003400090511
003500090511      //---------------------------------------------------------------
003600090511      //?Definizione strutture dati.
003700090511      //---------------------------------------------------------------
003800090512
003900090512      // - Tabella "OSR" = Serie ORM
004000090512     d dosr          e ds                  inz
004100091119
004200091119      // - ds per chiave aggancio ORM con riferimento ALFA
004300091119     d dskeyorm        ds
004400091119     d  wflgbolla                     1
004500091119     d  wblpaas                       2  0
004600091119     d  wblplnp                       3  0
004700091119     d  wblpnrs                       2  0
004800091119     d  wblpnsp                       7  0
004900090525
005000090525      // - File FIVAB00F
005100090525     d fivabds       e ds                  extname(fivab00f)
005200090512
005300090512      // - Controllo/Aggiornamento ORM RC da traduttore
005400090512      //   valori per o24err:
005500090512      //              00 = No errori
005600090512      //              01 = ORM non trovato
005700090512      //              02 = ORM allocato
005800090512      //              03 = ORM non aggiornato no fase 100
005900090512      //              04 = ORM non aggiornato no lna
006000090512     d fnls24ds      e ds                  inz
006100090512
006200090512      // Parametri
006300090513     d fnls28ds      e ds
006400090512
006500090512      // Parametri
006600090512     d kpjba         e ds
006700090512
006800090512      // - Gestione tabelle: controllo e ricerca
006900090512     d tibs02ds      e ds                  inz
007000090525
007100090525      // - File TIVLR00F
007200090525     d tivlrds       e ds                  extname(tivlr00f)
007300090511
007400090511      //---------------------------------------------------------------
007500090511      //?Definizione variabili globali.
007600090511      //---------------------------------------------------------------
007700090512
007800090512      // - Flags booleani
007900090513     d $cliabil        s              1n   inz(*off)
008000090512     d $prosegui       s              1n   inz(*off)
008100090512
008200090512      // - Indici di schiera
008300090512     d xx              s              3  0 inz
008400090512
008500090512      // - Campi di comodo
008600090525     d intipl          s              1a   inz
008700090513     d wacr            s                   like(acrcro)
008800090525     d wlib            s             10    inz
008900090512     d wnsr            s              2  0 inz
009000090511
009100090511      //---------------------------------------------------------------
009200090511      //?Definizione procedure esterne.
009300090511      //---------------------------------------------------------------
009400090512
009500090512      // - Controllo/Aggiornamento ORM RC da traduttore
009600090512     d fnls24r         pr                  extpgm('FNLS24R')
009700090512     d  fnls24ds                           likeds(fnls24ds)
009800090512
009900090512      // - Reperimento tabella
010000090513     d tibs02r         pr                  extpgm('TIBS02R')
010100090513     d  kpjba                              likeds(kpjba)    const
010200090513     d  tibs02ds                           likeds(tibs02ds)
010300090525
010400090525      // - Richiamo pgm per scrittura dati ORM RC
010500090610     d titv84c         pr                  extpgm('TITV84C')
010600090610     d  wlib                         10a
010700090525     d  intipl                        1a
010800090525     d  tivlrds                            likeds(tivlrds)
010900090525     d  fivabds                            likeds(fivabds)
011000090511
011100090511      //---------------------------------------------------------------
011200090511      //?Definizione key-list.
011300090511      //---------------------------------------------------------------
011400090512
011500090512       // - File TNTBE01L
011600090512     d k02tntbe01    e ds                  extname(TNTBE01L : *key)
011700090512     d                                     prefix(k_)  inz
011800090511
011900090511      //---------------------------------------------------------------
012000090511      //?Main.
012100090511      //---------------------------------------------------------------
012200090512
012300090512
012400090512     c     *entry        plist
012500090512     c                   parm                    kpjba
012600090512     c                   parm                    fnls28ds
012700090512
012800090512      /free
012900090512
013000090512       //?operazioni iniziali
013100090512         exsr routinz;
013200090512
013300090512       //?controllo tipo lancio
013400090512         if i28tla <> *blanks and i28tla <> 'C' and i28tla <> 'L';
013500090512           clear i28tla;
013600090512         endif;
013700090512
013800090512       //?elaboro
013900090512         select;
014000090512           when i28tla <> 'C';
014100090512             exsr elabora;
014200090512           when i28tla =  'C';
014300090525       //?richiamo pgm per chiusura file
014400090525             intipl = 'C';
014500090525             clear tivlrds;
014600090525             clear fivabds;
014700090610             titv84c(wlib:intipl:tivlrds:fivabds);
014800090512         endsl;
014900090512
015000090512       //?fine
015100090512         exsr endpgm;
015200090512
015300090512       // ----------------------------------------------------------------------
015400090512       //?Elaborazione.
015500090512       // ----------------------------------------------------------------------
015600090512       begsr elabora;
015700090512
015800090624       //?per prima cosa controllo i dati passati per agganciare la tabella
015900090624       //?OSR
016000090624
016100090624       //?errore se manca la lnp
016200090624         if i28lnp = 0;
016300090624           o28err = 01;
016400090624           o28msg = msg(o28err);
016500090624           leavesr;
016600090624         endif;
016700090624
016800090624       //?errore se manca ccm
016900090624         if i28ccm = 0;
017000090624           o28err = 02;
017100090624           o28msg = msg(o28err);
017200090624           leavesr;
017300090624         endif;
017400090624
017500090624       //?controllo se il cliente è abilitato alla OSR
017600130116         xx = %lookup(I28ccm:skCLIosr);
017700091012         IF xx = 0;
017800090624           o28err = 03;
017900090624           o28msg = msg(o28err);
018000090624           leavesr;
018100090624         endif;
018200090624
018300130116         wnsr = %dec(%subst(skLNPNRSosr(xx):4:2):2:0);
018400090624
018500090624       //?aggancio tabella OSR per recupero dati x ORM da tabella
018600090624         clear dosr;
018700090624         clear tibs02ds;
018800090624         t02mod = 'C';
018900090624         t02cod = 'OSR';
019000130116         t02ke1 = skLNPNRSosr(xx);
019100090624         tibs02r(kpjba:tibs02ds);
019200090624         if t02err = *blank;
019300090624           dosr = t02uni;
019400090624         endif;
019500090624
019600090624       //?se cliente ha VAB personalizzato
019700090624         if d§osrvas = 'S';
019800090624       //?cerco l'ORM creato dal traduttore
019900090624           exsr ls24;
020000090624         else;
020100090624       //?se da ls24 torna che devo proseguire vado avanti
020200090624           exsr ormrc;
020300090624         endif;
020400090512
020500090512       endsr;
020600090512
020700090512       // ----------------------------------------------------------------------
020800090512       //?Richiamo FNLS24R.
020900090512       // ----------------------------------------------------------------------
021000090512       begsr ls24;
021100090512
021200090512       //?imposto la ds
021300091022        // clear fnls24ds;
021400090512         i24tla = i28tla;
021500090512         i24ope = i28ope;
021600090512         i24ksc = i28ccm;
021700090513         i24aas = %subst(%editc(i28aas:'X'):3:2);
021800090512         i24spe = %editc(i28lnp:'X') + %editc(i28nrs:'X') + %editc(i28nsp:'X');
021900090512         i24lna = i28lna;
022000090512         i24znc = i28znc;
022100090512         i24naas = i28naas;
022200091022         //i24lnp = i28nlnp;
022300091022         i24lnp = i28lnp;
022400091022         //i24nrs = i28nnrs;
022500091022         i24nrs = i28nrs;
022600091022         //i24nsp = i28nnsp;
022700091022         i24nsp = i28nsp;
022800090512
022900090512       //?richiamo pgm
023000090611         fnls24r(fnls24ds);
023100090512
023200090624       //?passo l'errore al chiamante
023300090512         if o24err <> 0;
023400090512           o28err = o24err;
023500090512           o28msg = o24msg;
023600090512           leavesr;
023700090512         endif;
023800090624       //?passo i dati al chiamante
023900090512         o28poe = o24poe;
024000090512         o28nsr = o24nsr;
024100090512         o28nor = o24nor;
024200090512         o28nrv = o24nrv;
024300090512
024400090512       endsr;
024500090512
024600090512       // ----------------------------------------------------------------------
024700090512       //?Controllo/Scrivo ORM RC.
024800090512       // ----------------------------------------------------------------------
024900090512       begsr ormrc;
025000090512
025100090512       //?se richiamato per controllo
025200090512         if i28ope = 'C';
025300090512           exsr controllo;
025400090512         endif;
025500090512
025600090512       //?se richiamato per scrittura
025700090512         if i28ope = 'A';
025800090512           exsr scrivo;
025900090512         endif;
026000090512
026100090512       endsr;
026200090512
026300090512       // ----------------------------------------------------------------------
026400090512       //?Controllo se ok tutti i dati per ORM RC.
026500090512       // ----------------------------------------------------------------------
026600090512       begsr controllo;
026700090525
026800090525       //?se non richiesto COR o CRC da tebella controllo che il ccm passato
026900090525       //?sia presente su fnacr come '333' e come '444'
027000090525         if d§osrcor = 0;
027100090624           wacr = i28ccm * 1000 + 333;
027200090525           chain wacr fnacr01l;
027300090525           if not %found(fnacr01l);
027400090624             o28err = 06;
027500090525             o28msg = msg(o28err);
027600090525             leavesr;
027700090525           endif;
027800090525         endif;
027900090525         if d§osrcrc = 0;
028000090525           wacr = i28ccm * 1000 + 444;
028100090525           chain wacr fnacr01l;
028200090525           if not %found(fnacr01l);
028300090624             o28err = 07;
028400090525             o28msg = msg(o28err);
028500090525             leavesr;
028600090525           endif;
028700090525         endif;
028800090512
028900090512       //?errore se manca destinatario
029000090512         if i28rsd = *blanks or i28ind = *blanks or i28cad = *blanks or
029100090611            (i28prd = *blanks and i28nzd = *blanks);
029200090513           o28err = 04;
029300090513           o28msg = msg(o28err);
029400090513           leavesr;
029500090512         endif;
029600090512
029700090512       //?errore se manca linea di arrivo
029800090512         if i28lna = 0;
029900090512           o28err = 05;
030000090512           o28msg = msg(o28err);
030100090512           leavesr;
030200090512         endif;
030300090512
030400090512       endsr;
030500090512
030600090512       // ----------------------------------------------------------------------
030700090512       //?Scrivo ORM RC.
030800090512       // ----------------------------------------------------------------------
030900090512       begsr scrivo;
031000090525
031100090525         clear intipl;
031200090525         clear tivlrds;
031300090525         clear fivabds;
031400090525
031500090525       //?--> imposto i dati per creare ORM
031600090525       //?cliente
031700090525         vlrksc = *all'0';
031800090525         %subst(vlrksc:2:7) = %editc(i28ccm:'X');
031900090525       //?serie
032000090525         %subst(vlrppt:4:2) = %editc(wnsr:'X');
032100120327       //?FGS
032200120327         vabfgs = i28lnp;
032300090525       //?spedizione
032400090611         vabaas = i28naas;
032500090525         vablnp = i28lnp;
032600090525         vabnrs = i28nrs;
032700090525         vabnsp = i28nsp;
032800090609       //?destinatario
032900090525         vabrsd = i28rsd;
033000090525         vabind = i28ind;
033100090525         vabcad = i28cad;
033200090525         vablod = i28lod;
033300090525         vabprd = i28prd;
033400090525         vabnzd = i28nzd;
033500090525       //?cliente ordinante
033600090525         if d§osrcor <> *zeros;
033700090525           vabccm = d§osrcor;
033800090525         else;
033900090525           vabccm = i28ccm;
034000090525         endif;
034100090525       //?cliente consegna
034200090914         if d§osrcrc <> *zeros;
034300090525           %subst(vlrppt:6:7) = %editc(d§osrcrc:'X');
034400090525         else;
034500090525           %subst(vlrppt:6:7) = %editc(i28ccm:'X');
034600090525         endif;
034700090525           %subst(vlrppt:13:3) = '444';
034800090525       //?colli e peso
034900090624         if d§osrfcp = 'B';
035000090610           vabncl = i28ncl;
035100090611           vabpkb = i28pkb;
035200090611           %subst(vlrppt:35:1) = 'S';
035300090610         endif;
035400090624         if d§osrfcp = '1';
035500090611           %subst(vlrppt:35:1) = ' ';
035600090525         endif;
035700090525       //?chi paga
035800090610           %subst(vlrppt:36:1) = d§osrpag;
035900090610       //?ksc e ctr
036000090610         if d§osrpag = 'D';
036100090610           wacr = %dec(%subst(vlrppt:6:10):10:0);
036200090610         endif;
036300090610         if d§osrpag = 'O';
036400090610           wacr = vabccm * 1000 + 333;
036500090610         endif;
036600090610         chain wacr fnacr01l;
036700090610           if  %found(fnacr01l);
036800090610             if acrksc > 0;
036900090610               %subst(vlrppt:16:7) = %editc(acrksc:'X');
037000090610             else;
037100090610               %subst(vlrppt:16:7) = %subst(%editc(%dec(acrcro):'X'):1:7);
037200090610             endif;
037300090610             if acrccc <> 999;
037400090610               %subst(vlrppt:23:3) = %subst(%editc(%dec(acrccc):'X'):2:3);
037500090610             endif;
037600090610           endif;
037700090611       //?filiale ritiro
037800090611         %subst(vlrppt:37:3) = %editc(i28lna:'X');
037900090525       //?filiale consegna
038000090525         if d§osrpoc <> *zeros;
038100090525           %subst(vlrppt:32:3) = %editc(d§osrpoc:'X');
038200090525         endif;
038300090525
038400090525       //?--> richiamo pgm per creare ORM
038500090610         titv84c(wlib:intipl:tivlrds:fivabds);
038600091119
038700091119       //?aggancio ORM appena creato con n. bolla per recupero
038800091119       //?n. ORM da utilizzare in scrittura bolla per scrivere rcd
038900091119       //?di AR5 RCC
039000091119         wflgbolla = 'B';
039100091119         wblpaas = %dec(%subst(%editc(vabaas:'X'):3:2):2:0);
039200091119         wblplnp = vablnp;
039300091119         wblpnrs = vabnrs;
039400091119         wblpnsp = vabnsp;
039500091119         chain dskeyorm fnorm07l;
039600091119         if  %found(fnorm07l);
039700091119           o28poe = ormpoe;
039800091119           o28nsr = ormnsr;
039900091119           o28nor = ormnor;
040000091119           o28nrv = ormnrv;
040100091119         endif;
040200090512
040300090512       endsr;
040400090512
040500090512       // ----------------------------------------------------------------------
040600090512       //?Operazioni iniziali.
040700090512       // ----------------------------------------------------------------------
040800090512       begsr routinz;
040900090512
041000090512         clear o28err;
041100090512         clear o28msg;
041200090512         clear o28poe;
041300090512         clear o28nsr;
041400090512         clear o28nor;
041500090512         clear o28nrv;
041600090513
041700090513       //?controllo se ambiente di test
041800090513         if %subst(knsif:7:1) = 'P';
041900090611           wlib = c_libp;
042000090513         else;
042100090611           wlib = c_lib;
042200090513         endif;
042300090624
042400090624       //?carico tutta la tabella OSR
042500090624         exsr carosr;
042600090512
042700090512       endsr;
042800090512
042900090512       // ----------------------------------------------------------------------
043000090512       //?Operazioni finali.
043100090512       // ----------------------------------------------------------------------
043200090512       begsr endpgm;
043300090512
043400090512       //?restituzione dati al chiamante
043500090512         select;
043600090512           when o28err = 0 and i28tla <> 'C';
043700090512             if o24poe <> 0;
043800090512               o28poe = o24poe;
043900090512               o28nsr = o24nsr;
044000090512               o28nor = o24nor;
044100090512               o28nrv = o24nrv;
044200090512             endif;
044300090512
044400090512           when o28err > 0;
044500090512             o28msg = msg(o28err);
044600090512         endsl;
044700090512
044800090512         if i28tla = *blanks;
044900090512           *inrt = *on;
045000090512         else;
045100090512           *inlr = *on;
045200090512         endif;
045300090512
045400090512         return;
045500090512
045600090512       endsr;
045700090512
045800090512       // ----------------------------------------------------------------------
045900090512       //?Carico tabella OSR.
046000090512       // ----------------------------------------------------------------------
046100090512       begsr carosr;
046200090512
046300090512         clear xx;
046400090512
046500090512       //?carico tutta la tabella OSR delle LNP/NRS ORM
046600090512         setll    c_tabosr tntbe01l;
046700090512         reade(n) c_tabosr tntbe01l;
046800090512
046900090512         dow not %eof(tntbe01l);
047000130122           IF  TBEatb = *blanks;
047100090512           dosr = tbeuni;
047200130116       //?carico in sk tutti i clienti che trovo
047300130116       //?skCLIosr = cliente
047400130116       //?skLNPNRSosr = LNP + serie (TBEke1)
047500130116           IF d§osrcli <> 0;
047600130116             xx = %lookup(d§OSRcli:skCLIosr);
047700130116             IF  xx = 0;
047800130116               xx = %lookup(*zeros:skCLIosr);
047900130116               IF  xx > 0;
048000130116                 skCLIosr(xx) = d§OSRcli;
048100130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
048200130116               ENDIF;
048300130116             ENDIF;
048400130116           ENDIF;
048500130116
048600130116           IF d§osrcl2 <> 0;
048700130116             xx = %lookup(d§OSRcl2:skCLIosr);
048800130116             IF  xx = 0;
048900130116               xx = %lookup(*zeros:skCLIosr);
049000130116               IF  xx > 0;
049100130116                 skCLIosr(xx) = d§OSRcl2;
049200130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
049300130116               ENDIF;
049400130116             ENDIF;
049500130116           ENDIF;
049600090512
049700130116           IF d§osrcl3 <> 0;
049800130116             xx = %lookup(d§OSRcl3:skCLIosr);
049900130116             IF  xx = 0;
050000130116               xx = %lookup(*zeros:skCLIosr);
050100130116               IF  xx > 0;
050200130116                 skCLIosr(xx) = d§OSRcl3;
050300130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
050400130116               ENDIF;
050500130116             ENDIF;
050600130116           ENDIF;
050700090512
050800130116           IF d§osrcl4 <> 0;
050900130116             xx = %lookup(d§OSRcl4:skCLIosr);
051000130116             IF  xx = 0;
051100130116               xx = %lookup(*zeros:skCLIosr);
051200130116               IF  xx > 0;
051300130116                 skCLIosr(xx) = d§OSRcl4;
051400130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
051500130116               ENDIF;
051600130116             ENDIF;
051700130116           ENDIF;
051800090512
051900130116           IF d§osrcl5 <> 0;
052000130116             xx = %lookup(d§OSRcl5:skCLIosr);
052100130116             IF  xx = 0;
052200130116               xx = %lookup(*zeros:skCLIosr);
052300130116               IF  xx > 0;
052400130116                 skCLIosr(xx) = d§OSRcl5;
052500130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
052600130116               ENDIF;
052700130116             ENDIF;
052800130116           ENDIF;
052900090512
053000130116           IF d§osrcl6 <> 0;
053100130116             xx = %lookup(d§OSRcl6:skCLIosr);
053200130116             IF  xx = 0;
053300130116               xx = %lookup(*zeros:skCLIosr);
053400130116               IF  xx > 0;
053500130116                 skCLIosr(xx) = d§OSRcl6;
053600130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
053700130116               ENDIF;
053800130116             ENDIF;
053900130116           ENDIF;
054000090512
054100130116           IF d§osrcl7 <> 0;
054200130116             xx = %lookup(d§OSRcl7:skCLIosr);
054300130116             IF  xx = 0;
054400130116               xx = %lookup(*zeros:skCLIosr);
054500130116               IF  xx > 0;
054600130116                 skCLIosr(xx) = d§OSRcl7;
054700130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
054800130116               ENDIF;
054900130116             ENDIF;
055000130116           ENDIF;
055100090512
055200130116           IF d§osrcl8 <> 0;
055300130116             xx = %lookup(d§OSRcl8:skCLIosr);
055400130116             IF  xx = 0;
055500130116               xx = %lookup(*zeros:skCLIosr);
055600130116               IF  xx > 0;
055700130116                 skCLIosr(xx) = d§OSRcl8;
055800130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
055900130116               ENDIF;
056000130116             ENDIF;
056100130116           ENDIF;
056200090512
056300130116           IF d§osrcl9 <> 0;
056400130116             xx = %lookup(d§OSRcl9:skCLIosr);
056500130116             IF  xx = 0;
056600130116               xx = %lookup(*zeros:skCLIosr);
056700130116               IF  xx > 0;
056800130116                 skCLIosr(xx) = d§OSRcl9;
056900130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
057000130116               ENDIF;
057100130116             ENDIF;
057200130116           ENDIF;
057300090512
057400130116           IF d§osrcl10 <> 0;
057500130116             xx = %lookup(d§OSRcl10:skCLIosr);
057600130116             IF  xx = 0;
057700130116               xx = %lookup(*zeros:skCLIosr);
057800130116               IF  xx > 0;
057900130116                 skCLIosr(xx) = d§OSRcl10;
058000130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
058100130116               ENDIF;
058200130116             ENDIF;
058300130116           ENDIF;
058400090512
058500130116           IF d§osrcl11 <> 0;
058600130116             xx = %lookup(d§OSRcl11:skCLIosr);
058700130116             IF  xx = 0;
058800130116               xx = %lookup(*zeros:skCLIosr);
058900130116               IF  xx > 0;
059000130116                 skCLIosr(xx) = d§OSRcl11;
059100130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
059200130116               ENDIF;
059300130116             ENDIF;
059400130116           ENDIF;
059500090512
059600130116           IF d§osrcl12 <> 0;
059700130116             xx = %lookup(d§OSRcl12:skCLIosr);
059800130116             IF  xx = 0;
059900130116               xx = %lookup(*zeros:skCLIosr);
060000130116               IF  xx > 0;
060100130116                 skCLIosr(xx) = d§OSRcl12;
060200130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
060300130116               ENDIF;
060400130116             ENDIF;
060500130116           ENDIF;
060600090512
060700130116           IF d§osrcl13 <> 0;
060800130116             xx = %lookup(d§OSRcl13:skCLIosr);
060900130116             IF  xx = 0;
061000130116               xx = %lookup(*zeros:skCLIosr);
061100130116               IF  xx > 0;
061200130116                 skCLIosr(xx) = d§OSRcl13;
061300130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
061400130116               ENDIF;
061500130116             ENDIF;
061600130116           ENDIF;
061700090512
061800130116           IF d§osrcl14 <> 0;
061900130116             xx = %lookup(d§OSRcl14:skCLIosr);
062000130116             IF  xx = 0;
062100130116               xx = %lookup(*zeros:skCLIosr);
062200130116               IF  xx > 0;
062300130116                 skCLIosr(xx) = d§OSRcl14;
062400130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
062500130116               ENDIF;
062600130116             ENDIF;
062700130116           ENDIF;
062800090512
062900130116           IF d§osrcl15 <> 0;
063000130116             xx = %lookup(d§OSRcl15:skCLIosr);
063100130116             IF  xx = 0;
063200130116               xx = %lookup(*zeros:skCLIosr);
063300130116               IF  xx > 0;
063400130116                 skCLIosr(xx) = d§OSRcl15;
063500130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
063600130116               ENDIF;
063700130116             ENDIF;
063800130116           ENDIF;
063900090512
064000130116           IF d§osrcl16 <> 0;
064100130116             xx = %lookup(d§OSRcl16:skCLIosr);
064200130116             IF  xx = 0;
064300130116               xx = %lookup(*zeros:skCLIosr);
064400130116               IF  xx > 0;
064500130116                 skCLIosr(xx) = d§OSRcl16;
064600130116                 skLNPNRSosr(xx) = %subst(TBEke1:1:5);
064700130116               ENDIF;
064800130116             ENDIF;
064900130116           ENDIF;
065000130122
065100130122           ENDIF;
065200090512
065300090512           reade(n) c_tabosr tntbe01l;
065400090512         enddo;
065500090512
065600090512       endsr;
065700090512
065800090512** msg - lungo 78                                                            *
065900090512FNLS28R - LNP non passata                                                     01
066000090512FNLS28R - Cliente non passato                                                 02
066100090512FNLS28R - Cliente non abilitato alla OSR                                      03
066200090512FNLS28R - Dati destinatario non passati                                       04
066300090512FNLS28R - Linea di arrivo non passata                                         05
066400090624FNLS28R - Cliente non codificato in anagrafica clienti ritiro con "333"       06
066500090624FNLS28R - Cliente non codificato in anagrafica clienti ritiro con "444"       07
