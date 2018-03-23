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
002800090512     d skcliosr        s             10    dim(900)
002900090512     d sklnpnrsosr     s              5    dim(900)
003000090512
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
004100090512
004200090512      // - ds tabella OSR lnp+cli
004300090512     d dscliosr        ds
004400091119     d  wlnposr                       3
004500091119     d  wcliosr                       7
004600091119
004700091119      // - ds per chiave aggancio ORM con riferimento ALFA
004800091119     d dskeyorm        ds
004900091119     d  wflgbolla                     1
005000091119     d  wblpaas                       2  0
005100091119     d  wblplnp                       3  0
005200091119     d  wblpnrs                       2  0
005300091119     d  wblpnsp                       7  0
005400090525
005500090525      // - File FIVAB00F
005600090525     d fivabds       e ds                  extname(fivab00f)
005700090512
005800090512      // - Controllo/Aggiornamento ORM RC da traduttore
005900090512      //   valori per o24err:
006000090512      //              00 = No errori
006100090512      //              01 = ORM non trovato
006200090512      //              02 = ORM allocato
006300090512      //              03 = ORM non aggiornato no fase 100
006400090512      //              04 = ORM non aggiornato no lna
006500090512     d fnls24ds      e ds                  inz
006600090512
006700090512      // Parametri
006800090513     d fnls28ds      e ds
006900090512
007000090512      // Parametri
007100090512     d kpjba         e ds
007200090512
007300090512      // - Gestione tabelle: controllo e ricerca
007400090512     d tibs02ds      e ds                  inz
007500090525
007600090525      // - File TIVLR00F
007700090525     d tivlrds       e ds                  extname(tivlr00f)
007800090511
007900090511      //---------------------------------------------------------------
008000090511      //?Definizione variabili globali.
008100090511      //---------------------------------------------------------------
008200090512
008300090512      // - Flags booleani
008400090513     d $cliabil        s              1n   inz(*off)
008500090512     d $prosegui       s              1n   inz(*off)
008600090512
008700090512      // - Indici di schiera
008800090512     d xx              s              3  0 inz
008900090512
009000090512      // - Campi di comodo
009100090525     d intipl          s              1a   inz
009200090513     d wacr            s                   like(acrcro)
009300090525     d wlib            s             10    inz
009400090512     d wnsr            s              2  0 inz
009500090511
009600090511      //---------------------------------------------------------------
009700090511      //?Definizione procedure esterne.
009800090511      //---------------------------------------------------------------
009900090512
010000090512      // - Controllo/Aggiornamento ORM RC da traduttore
010100090512     d fnls24r         pr                  extpgm('FNLS24R')
010200090512     d  fnls24ds                           likeds(fnls24ds)
010300090512
010400090512      // - Reperimento tabella
010500090513     d tibs02r         pr                  extpgm('TIBS02R')
010600090513     d  kpjba                              likeds(kpjba)    const
010700090513     d  tibs02ds                           likeds(tibs02ds)
010800090525
010900090525      // - Richiamo pgm per scrittura dati ORM RC
011000090610     d titv84c         pr                  extpgm('TITV84C')
011100090610     d  wlib                         10a
011200090525     d  intipl                        1a
011300090525     d  tivlrds                            likeds(tivlrds)
011400090525     d  fivabds                            likeds(fivabds)
011500090511
011600090511      //---------------------------------------------------------------
011700090511      //?Definizione key-list.
011800090511      //---------------------------------------------------------------
011900090512
012000090512       // - File TNTBE01L
012100090512     d k02tntbe01    e ds                  extname(TNTBE01L : *key)
012200090512     d                                     prefix(k_)  inz
012300090511
012400090511      //---------------------------------------------------------------
012500090511      //?Main.
012600090511      //---------------------------------------------------------------
012700090512
012800090512
012900090512     c     *entry        plist
013000090512     c                   parm                    kpjba
013100090512     c                   parm                    fnls28ds
013200090512
013300090512      /free
013400090512
013500090512       //?operazioni iniziali
013600090512         exsr routinz;
013700090512
013800090512       //?controllo tipo lancio
013900090512         if i28tla <> *blanks and i28tla <> 'C' and i28tla <> 'L';
014000090512           clear i28tla;
014100090512         endif;
014200090512
014300090512       //?elaboro
014400090512         select;
014500090512           when i28tla <> 'C';
014600090512             exsr elabora;
014700090512           when i28tla =  'C';
014800090525       //?richiamo pgm per chiusura file
014900090525             intipl = 'C';
015000090525             clear tivlrds;
015100090525             clear fivabds;
015200090610             titv84c(wlib:intipl:tivlrds:fivabds);
015300090512         endsl;
015400090512
015500090512       //?fine
015600090512         exsr endpgm;
015700090512
015800090512       // ----------------------------------------------------------------------
015900090512       //?Elaborazione.
016000090512       // ----------------------------------------------------------------------
016100090512       begsr elabora;
016200090512
016300090624       //?per prima cosa controllo i dati passati per agganciare la tabella
016400090624       //?OSR
016500090624
016600090624       //?errore se manca la lnp
016700090624         if i28lnp = 0;
016800090624           o28err = 01;
016900090624           o28msg = msg(o28err);
017000090624           leavesr;
017100090624         endif;
017200090624
017300090624       //?errore se manca ccm
017400090624         if i28ccm = 0;
017500090624           o28err = 02;
017600090624           o28msg = msg(o28err);
017700090624           leavesr;
017800090624         endif;
017900090624
018000090624       //?controllo se il cliente è abilitato alla OSR
018100090624         wlnposr = %editc(i28lnp:'X');
018200090624         wcliosr = %editc(i28ccm:'X');
018300091012         xx = %lookup(dscliosr:skcliosr);
018400091012         IF xx = 0;
018500090624           o28err = 03;
018600090624           o28msg = msg(o28err);
018700090624           leavesr;
018800090624         endif;
018900090624
019000090624         wnsr = %dec(%subst(sklnpnrsosr(xx):4:2):2:0);
019100090624
019200090624       //?aggancio tabella OSR per recupero dati x ORM da tabella
019300090624         clear dosr;
019400090624         clear tibs02ds;
019500090624         t02mod = 'C';
019600090624         t02cod = 'OSR';
019700090624         t02ke1 = sklnpnrsosr(xx);
019800090624         tibs02r(kpjba:tibs02ds);
019900090624         if t02err = *blank;
020000090624           dosr = t02uni;
020100090624         endif;
020200090624
020300090624       //?se cliente ha VAB personalizzato
020400090624         if d§osrvas = 'S';
020500090624       //?cerco l'ORM creato dal traduttore
020600090624           exsr ls24;
020700090624         else;
020800090624       //?se da ls24 torna che devo proseguire vado avanti
020900090624           exsr ormrc;
021000090624         endif;
021100090512
021200090512       endsr;
021300090512
021400090512       // ----------------------------------------------------------------------
021500090512       //?Richiamo FNLS24R.
021600090512       // ----------------------------------------------------------------------
021700090512       begsr ls24;
021800090512
021900090512       //?imposto la ds
022000091022        // clear fnls24ds;
022100090512         i24tla = i28tla;
022200090512         i24ope = i28ope;
022300090512         i24ksc = i28ccm;
022400090513         i24aas = %subst(%editc(i28aas:'X'):3:2);
022500090512         i24spe = %editc(i28lnp:'X') + %editc(i28nrs:'X') + %editc(i28nsp:'X');
022600090512         i24lna = i28lna;
022700090512         i24znc = i28znc;
022800090512         i24naas = i28naas;
022900091022         //i24lnp = i28nlnp;
023000091022         i24lnp = i28lnp;
023100091022         //i24nrs = i28nnrs;
023200091022         i24nrs = i28nrs;
023300091022         //i24nsp = i28nnsp;
023400091022         i24nsp = i28nsp;
023500090512
023600090512       //?richiamo pgm
023700090611         fnls24r(fnls24ds);
023800090512
023900090624       //?passo l'errore al chiamante
024000090512         if o24err <> 0;
024100090512           o28err = o24err;
024200090512           o28msg = o24msg;
024300090512           leavesr;
024400090512         endif;
024500090624       //?passo i dati al chiamante
024600090512         o28poe = o24poe;
024700090512         o28nsr = o24nsr;
024800090512         o28nor = o24nor;
024900090512         o28nrv = o24nrv;
025000090512
025100090512       endsr;
025200090512
025300090512       // ----------------------------------------------------------------------
025400090512       //?Controllo/Scrivo ORM RC.
025500090512       // ----------------------------------------------------------------------
025600090512       begsr ormrc;
025700090512
025800090512       //?se richiamato per controllo
025900090512         if i28ope = 'C';
026000090512           exsr controllo;
026100090512         endif;
026200090512
026300090512       //?se richiamato per scrittura
026400090512         if i28ope = 'A';
026500090512           exsr scrivo;
026600090512         endif;
026700090512
026800090512       endsr;
026900090512
027000090512       // ----------------------------------------------------------------------
027100090512       //?Controllo se ok tutti i dati per ORM RC.
027200090512       // ----------------------------------------------------------------------
027300090512       begsr controllo;
027400090525
027500090525       //?se non richiesto COR o CRC da tebella controllo che il ccm passato
027600090525       //?sia presente su fnacr come '333' e come '444'
027700090525         if d§osrcor = 0;
027800090624           wacr = i28ccm * 1000 + 333;
027900090525           chain wacr fnacr01l;
028000090525           if not %found(fnacr01l);
028100090624             o28err = 06;
028200090525             o28msg = msg(o28err);
028300090525             leavesr;
028400090525           endif;
028500090525         endif;
028600090525         if d§osrcrc = 0;
028700090525           wacr = i28ccm * 1000 + 444;
028800090525           chain wacr fnacr01l;
028900090525           if not %found(fnacr01l);
029000090624             o28err = 07;
029100090525             o28msg = msg(o28err);
029200090525             leavesr;
029300090525           endif;
029400090525         endif;
029500090512
029600090512       //?errore se manca destinatario
029700090512         if i28rsd = *blanks or i28ind = *blanks or i28cad = *blanks or
029800090611            (i28prd = *blanks and i28nzd = *blanks);
029900090513           o28err = 04;
030000090513           o28msg = msg(o28err);
030100090513           leavesr;
030200090512         endif;
030300090512
030400090512       //?errore se manca linea di arrivo
030500090512         if i28lna = 0;
030600090512           o28err = 05;
030700090512           o28msg = msg(o28err);
030800090512           leavesr;
030900090512         endif;
031000090512
031100090512       endsr;
031200090512
031300090512       // ----------------------------------------------------------------------
031400090512       //?Scrivo ORM RC.
031500090512       // ----------------------------------------------------------------------
031600090512       begsr scrivo;
031700090525
031800090525         clear intipl;
031900090525         clear tivlrds;
032000090525         clear fivabds;
032100090525
032200090525       //?--> imposto i dati per creare ORM
032300090525       //?cliente
032400090525         vlrksc = *all'0';
032500090525         %subst(vlrksc:2:7) = %editc(i28ccm:'X');
032600090525       //?serie
032700090525         %subst(vlrppt:4:2) = %editc(wnsr:'X');
032800120327       //?FGS
032900120327         vabfgs = i28lnp;
033000090525       //?spedizione
033100090611         vabaas = i28naas;
033200090525         vablnp = i28lnp;
033300090525         vabnrs = i28nrs;
033400090525         vabnsp = i28nsp;
033500090609       //?destinatario
033600090525         vabrsd = i28rsd;
033700090525         vabind = i28ind;
033800090525         vabcad = i28cad;
033900090525         vablod = i28lod;
034000090525         vabprd = i28prd;
034100090525         vabnzd = i28nzd;
034200090525       //?cliente ordinante
034300090525         if d§osrcor <> *zeros;
034400090525           vabccm = d§osrcor;
034500090525         else;
034600090525           vabccm = i28ccm;
034700090525         endif;
034800090525       //?cliente consegna
034900090914         if d§osrcrc <> *zeros;
035000090525           %subst(vlrppt:6:7) = %editc(d§osrcrc:'X');
035100090525         else;
035200090525           %subst(vlrppt:6:7) = %editc(i28ccm:'X');
035300090525         endif;
035400090525           %subst(vlrppt:13:3) = '444';
035500090525       //?colli e peso
035600090624         if d§osrfcp = 'B';
035700090610           vabncl = i28ncl;
035800090611           vabpkb = i28pkb;
035900090611           %subst(vlrppt:35:1) = 'S';
036000090610         endif;
036100090624         if d§osrfcp = '1';
036200090611           %subst(vlrppt:35:1) = ' ';
036300090525         endif;
036400090525       //?chi paga
036500090610           %subst(vlrppt:36:1) = d§osrpag;
036600090610       //?ksc e ctr
036700090610         if d§osrpag = 'D';
036800090610           wacr = %dec(%subst(vlrppt:6:10):10:0);
036900090610         endif;
037000090610         if d§osrpag = 'O';
037100090610           wacr = vabccm * 1000 + 333;
037200090610         endif;
037300090610         chain wacr fnacr01l;
037400090610           if  %found(fnacr01l);
037500090610             if acrksc > 0;
037600090610               %subst(vlrppt:16:7) = %editc(acrksc:'X');
037700090610             else;
037800090610               %subst(vlrppt:16:7) = %subst(%editc(%dec(acrcro):'X'):1:7);
037900090610             endif;
038000090610             if acrccc <> 999;
038100090610               %subst(vlrppt:23:3) = %subst(%editc(%dec(acrccc):'X'):2:3);
038200090610             endif;
038300090610           endif;
038400090611       //?filiale ritiro
038500090611         %subst(vlrppt:37:3) = %editc(i28lna:'X');
038600090525       //?filiale consegna
038700090525         if d§osrpoc <> *zeros;
038800090525           %subst(vlrppt:32:3) = %editc(d§osrpoc:'X');
038900090525         endif;
039000090525
039100090525       //?--> richiamo pgm per creare ORM
039200090610         titv84c(wlib:intipl:tivlrds:fivabds);
039300091119
039400091119       //?aggancio ORM appena creato con n. bolla per recupero
039500091119       //?n. ORM da utilizzare in scrittura bolla per scrivere rcd
039600091119       //?di AR5 RCC
039700091119         wflgbolla = 'B';
039800091119         wblpaas = %dec(%subst(%editc(vabaas:'X'):3:2):2:0);
039900091119         wblplnp = vablnp;
040000091119         wblpnrs = vabnrs;
040100091119         wblpnsp = vabnsp;
040200091119         chain dskeyorm fnorm07l;
040300091119         if  %found(fnorm07l);
040400091119           o28poe = ormpoe;
040500091119           o28nsr = ormnsr;
040600091119           o28nor = ormnor;
040700091119           o28nrv = ormnrv;
040800091119         endif;
040900090512
041000090512       endsr;
041100090512
041200090512       // ----------------------------------------------------------------------
041300090512       //?Operazioni iniziali.
041400090512       // ----------------------------------------------------------------------
041500090512       begsr routinz;
041600090512
041700090512         clear o28err;
041800090512         clear o28msg;
041900090512         clear o28poe;
042000090512         clear o28nsr;
042100090512         clear o28nor;
042200090512         clear o28nrv;
042300090513
042400090513       //?controllo se ambiente di test
042500090513         if %subst(knsif:7:1) = 'P';
042600090611           wlib = c_libp;
042700090513         else;
042800090611           wlib = c_lib;
042900090513         endif;
043000090624
043100090624       //?carico tutta la tabella OSR
043200090624         exsr carosr;
043300090512
043400090512       endsr;
043500090512
043600090512       // ----------------------------------------------------------------------
043700090512       //?Operazioni finali.
043800090512       // ----------------------------------------------------------------------
043900090512       begsr endpgm;
044000090512
044100090512       //?restituzione dati al chiamante
044200090512         select;
044300090512           when o28err = 0 and i28tla <> 'C';
044400090512             if o24poe <> 0;
044500090512               o28poe = o24poe;
044600090512               o28nsr = o24nsr;
044700090512               o28nor = o24nor;
044800090512               o28nrv = o24nrv;
044900090512             endif;
045000090512
045100090512           when o28err > 0;
045200090512             o28msg = msg(o28err);
045300090512         endsl;
045400090512
045500090512         if i28tla = *blanks;
045600090512           *inrt = *on;
045700090512         else;
045800090512           *inlr = *on;
045900090512         endif;
046000090512
046100090512         return;
046200090512
046300090512       endsr;
046400090512
046500090512       // ----------------------------------------------------------------------
046600090512       //?Carico tabella OSR.
046700090512       // ----------------------------------------------------------------------
046800090512       begsr carosr;
046900090512
047000090512         clear xx;
047100090512
047200090512       //?carico tutta la tabella OSR delle LNP/NRS ORM
047300090512         setll    c_tabosr tntbe01l;
047400090512         reade(n) c_tabosr tntbe01l;
047500090512
047600090512         dow not %eof(tntbe01l);
047700090512           dosr = tbeuni;
047800090512       //?carico in sk tutti e 16 i codici che si possono indicare x ogni serie
047900090512           if d§osrcli <> 0;
048000090512             xx += 1;
048100090512             sklnpnrsosr(xx) = tbeke1;
048200090512             wlnposr = %subst(tbeke1:1:3);
048300090512             wcliosr = %editc(d§osrcli:'X');
048400090512             skcliosr(xx) = dscliosr;
048500090512           endif;
048600090512
048700090512           if d§osrcl2 <> 0;
048800090512             xx += 1;
048900090512             sklnpnrsosr(xx) = tbeke1;
049000090512             wlnposr = %subst(tbeke1:1:3);
049100090512             wcliosr = %editc(d§osrcl2:'X');
049200090512             skcliosr(xx) = dscliosr;
049300090512           endif;
049400090512
049500090512           if d§osrcl3 <> 0;
049600090512             xx += 1;
049700090512             sklnpnrsosr(xx) = tbeke1;
049800090512             wlnposr = %subst(tbeke1:1:3);
049900090512             wcliosr = %editc(d§osrcl3:'X');
050000090512             skcliosr(xx) = dscliosr;
050100090512           endif;
050200090512
050300090512           if d§osrcl4 <> 0;
050400090512             xx += 1;
050500090512             sklnpnrsosr(xx) = tbeke1;
050600090512             wlnposr = %subst(tbeke1:1:3);
050700090512             wcliosr = %editc(d§osrcl4:'X');
050800090512             skcliosr(xx) = dscliosr;
050900090512           endif;
051000090512
051100090512           if d§osrcl5 <> 0;
051200090512             xx += 1;
051300090512             sklnpnrsosr(xx) = tbeke1;
051400090512             wlnposr = %subst(tbeke1:1:3);
051500090512             wcliosr = %editc(d§osrcl5:'X');
051600090512             skcliosr(xx) = dscliosr;
051700090512           endif;
051800090512
051900090512           if d§osrcl6 <> 0;
052000090512             xx += 1;
052100090512             sklnpnrsosr(xx) = tbeke1;
052200090512             wlnposr = %subst(tbeke1:1:3);
052300090512             wcliosr = %editc(d§osrcl6:'X');
052400090512             skcliosr(xx) = dscliosr;
052500090512           endif;
052600090512
052700090512           if d§osrcl7 <> 0;
052800090512             xx += 1;
052900090512             sklnpnrsosr(xx) = tbeke1;
053000090512             wlnposr = %subst(tbeke1:1:3);
053100090512             wcliosr = %editc(d§osrcl7:'X');
053200090512             skcliosr(xx) = dscliosr;
053300090512           endif;
053400090512
053500090512           if d§osrcl8 <> 0;
053600090512             xx += 1;
053700090512             sklnpnrsosr(xx) = tbeke1;
053800090512             wlnposr = %subst(tbeke1:1:3);
053900090512             wcliosr = %editc(d§osrcl8:'X');
054000090512             skcliosr(xx) = dscliosr;
054100090512           endif;
054200090512
054300090512           if d§osrcl9 <> 0;
054400090512             xx += 1;
054500090512             sklnpnrsosr(xx) = tbeke1;
054600090512             wlnposr = %subst(tbeke1:1:3);
054700090512             wcliosr = %editc(d§osrcl9:'X');
054800090512             skcliosr(xx) = dscliosr;
054900090512           endif;
055000090512
055100090512           if d§osrcl10 <> 0;
055200090512             xx += 1;
055300090512             sklnpnrsosr(xx) = tbeke1;
055400090512             wlnposr = %subst(tbeke1:1:3);
055500090512             wcliosr = %editc(d§osrcl10:'X');
055600090512             skcliosr(xx) = dscliosr;
055700090512           endif;
055800090512
055900090512           if d§osrcl11 <> 0;
056000090512             xx += 1;
056100090512             sklnpnrsosr(xx) = tbeke1;
056200090512             wlnposr = %subst(tbeke1:1:3);
056300090512             wcliosr = %editc(d§osrcl11:'X');
056400090512             skcliosr(xx) = dscliosr;
056500090512           endif;
056600090512
056700090512           if d§osrcl12 <> 0;
056800090512             xx += 1;
056900090512             sklnpnrsosr(xx) = tbeke1;
057000090512             wlnposr = %subst(tbeke1:1:3);
057100090512             wcliosr = %editc(d§osrcl12:'X');
057200090512             skcliosr(xx) = dscliosr;
057300090512           endif;
057400090512
057500090512           if d§osrcl13 <> 0;
057600090512             xx += 1;
057700090512             sklnpnrsosr(xx) = tbeke1;
057800090512             wlnposr = %subst(tbeke1:1:3);
057900090512             wcliosr = %editc(d§osrcl13:'X');
058000090512             skcliosr(xx) = dscliosr;
058100090512           endif;
058200090512
058300090512           if d§osrcl14 <> 0;
058400090512             xx += 1;
058500090512             sklnpnrsosr(xx) = tbeke1;
058600090512             wlnposr = %subst(tbeke1:1:3);
058700090512             wcliosr = %editc(d§osrcl14:'X');
058800090512             skcliosr(xx) = dscliosr;
058900090512           endif;
059000090512
059100090512           if d§osrcl15 <> 0;
059200090512             xx += 1;
059300090512             sklnpnrsosr(xx) = tbeke1;
059400090512             wlnposr = %subst(tbeke1:1:3);
059500090512             wcliosr = %editc(d§osrcl15:'X');
059600090512             skcliosr(xx) = dscliosr;
059700090512           endif;
059800090512
059900090512           if d§osrcl16 <> 0;
060000090512             xx += 1;
060100090512             sklnpnrsosr(xx) = tbeke1;
060200090512             wlnposr = %subst(tbeke1:1:3);
060300090512             wcliosr = %editc(d§osrcl16:'X');
060400090512             skcliosr(xx) = dscliosr;
060500090512           endif;
060600090512
060700090512           reade(n) c_tabosr tntbe01l;
060800090512         enddo;
060900090512
061000090512       endsr;
061100090512
061200090512** msg - lungo 78                                                            *
061300090512FNLS28R - LNP non passata                                                     01
061400090512FNLS28R - Cliente non passato                                                 02
061500090512FNLS28R - Cliente non abilitato alla OSR                                      03
061600090512FNLS28R - Dati destinatario non passati                                       04
061700090512FNLS28R - Linea di arrivo non passata                                         05
061800090624FNLS28R - Cliente non codificato in anagrafica clienti ritiro con "333"       06
061900090624FNLS28R - Cliente non codificato in anagrafica clienti ritiro con "444"       07
