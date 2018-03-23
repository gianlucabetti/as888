000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300090217      //--------------------------------------------------------------
000400090217      //
000500090217      // Creazione bolle addebito per Pod Image
000600090217      //
000700090217      //--------------------------------------------------------------
000800040511
000900090217     ffnrig01l  uf   e           k Disk
001000090217     ftitas30c  if   e           k Disk    prefix(f_)
001100090217     ftitaa30c  if   e           k Disk    prefix(f_)
001200090217     ftabel00f  if   e           k Disk
001300090217     fazorg01l  if   e           k Disk
001400090217     ftitas00f  o    e           k Disk    rename(Titas000:Titas0)
001500090217     ftitaa00f  o    e           k Disk    rename(Titaa000:Titaa0)
001600090217     ftita400f  o    e           k Disk
001700040511
001800040511
001900090217      //--------------------------------------------------------------
002000090217      //  Variabili
002100090217      //--------------------------------------------------------------
002200090304     d codut           s                   like(tblkut) inz(1)
002300090304     d krig            s                   like(rigdela) inz
002400090427     d oldksc          s                   like(rigksc)
002500090318     d wdsp            s                   like(rigdsp)
002600090303     d wfil            s                   like(orgfil) inz(102)
002700090217     d wnewora         s               t
002800090220     d wnumbolla       s             15
002900090217     d woldora         s               t
003000090220     d woggi           s              8  0
003100090220     d wora            s              6  0
003200090421     d w0110           s             11  0
003300090220     d $fine           s               n
003400090317     d $notaao         s               n
003500090317     d $nota4a         s               n
003600040511
003700090217      //--------------------------------------------------------------
003800090217      // Definizione aree dati.
003900090217      //--------------------------------------------------------------
004000090217      // - Dati utente
004100090217     d §azute        e ds                  extname(azute00f)
004200090217     d                                     dtaara
004300090217     d §datiute      e ds                  extname(ddatiute)
004400090217     d                                     dtaara
004500090217
004600090217      //--------------------------------------------------------------
004700090217      // Definizione strutture dati.
004800090217      //--------------------------------------------------------------
004900090217
005000090217      // - Parametri ricevuti
005100090217     d kpjba         e ds
005200090217
005300090217      // - Reperimento dati utente
005400090217     d tibs34ds      e ds
005500090217
005600090217      // - Lettura anagrafiche cliente
005700090217     d tibs69ds      e ds                  inz
005800090317     d ds_CNACO      e ds                  extname(CNACO00F)
005900090317     d ds_CNIND      e ds                  extname(CNIND00F)
006000090317     d ds_CNCLP      e ds                  extname(CNCLP00F)
006100090317     d ds_FNCLS      e ds                  extname(FNCLS00F)
006200090318
006300090318       // - Parametri per Ricerca/controllo tabelle
006400090318     d TIBS02ds      e ds                  inz
006500090420
006600090420       // - Parametri per Reperimento legami
006700090420     d tibs10ds      e ds                  inz
006800090420     d  skfigli               21   5520  0 dim(500)
006900090217
007000090217      // - Tabella 3a
007100090217     d ds3a          e ds
007200090318       // - Tabella "GED" = Divise area gestionale
007300090318     d dGED          e ds                  inz
007400090217
007500090217     d dta4a         e ds
007600090217     d fnlv55ds      e ds
007700090217     d trul34ds      e ds
007800090217     d  f_i34tla     e                     extfld(i34tla)
007900090217     d  f_o34err     e                     extfld(o34err)
008000090217     d  f_o34msg     e                     extfld(o34msg)
008100090217
008200090217     d oldbolla        ds
008300090217     d  oldaas                 1      4  0
008400090217     d  oldlnp                 5      7  0
008500090217     d  oldnrs                 8      9  0
008600090217     d  oldnsp                10     16  0
008700090217
008800090217     d newbolla        ds
008900090304     d  rigaas                 1      4  0
009000090304     d  riglnp                 5      7  0
009100090304     d  rignrs                 8      9  0
009200090304     d  rignsp                10     16  0
009300090217
009400090217     d oldrich         ds
009500090217     d  olddata                1      8  0
009600090217     d  oldora                 9     14  0
009700090217
009800090217     d newrich         ds
009900090217     d  rigdata                1      8  0
010000090217     d  rigora                 9     14  0
010100090217
010200090217      //--------------------------------------------------------------
010300090217      // Definizione procedure usate.
010400090217      //--------------------------------------------------------------
010500090220      // - Reperimento terminal di arrivo/partenza
010600090317      /copy gaitrasrc/srcprotopr,fnlv55r
010700090220
010800090217      // - Reperimento dati utente
010900090317      /copy gaitrasrc/srcprotopr,tibs34r
011000090217
011100090217      // - Lettura anagrafiche cliente
011200090317      /copy gaitrasrc/srcprotopr,tibs69r
011300090318
011400090318       // - Ricerca/Controllo tabelle
011500090318      /copy gaitrasrc/srcprotopr,tibs02r
011600090420
011700090420       // - Reperimento legami
011800090420      /copy gaitrasrc/srcprotopr,tibs10r
011900090304
012000090304      // - Numero spedizione
012100090304     d trul34r         pr                  extpgm('TRUL34R')
012200090304     d  kpjba                              likeds(KPJBA)
012300090304     d  trul34ds                           likeds(TRUL34ds)
012400090217
012500090217      //--------------------------------------------------------------
012600090217     c     *Entry        plist
012700090217     c                   parm                    kpjba
012800090217
012900090217      /free
013000090217
013100090217       // Recupero dati del JOB
013200090220          exsr DatiJob;
013300090220
013400090220          $fine = *off;
013500090220          woggi = %dec(%date());
013600090220          wora  = %dec(%time());
013700090304
013800090304          setll krig fnrig01l;
013900090217
014000090220       // Leggo fnrig
014100090220          dow not $fine;
014200090304            reade krig fnrig01l;
014300090220
014400090304            if %eof(fnrig01l);
014500090220              $fine = *on;
014600090220              leave;
014700090220            endif;
014800090316
014900090316       // elaboro solo i Pod Image da Internet, FTP, CD
015000090316            if rigcau <> 'I' and rigcau <> 'P';
015100090316              iter;
015200090316            endif;
015300090317
015400090317       // imposto il n. bolla
015500090317            if rignsp > 0;
015600090325              wnumbolla =  %subst(%editc(%dec(rigaas):'X'):3:2) +
015700090317                           %editc(%dec(riglnp):'X') +
015800090317                           %editc(%dec(rignrs):'X') + %editc(%dec(rignsp):'X');
015900090317            else;
016000090317              wnumbolla = '1';
016100090317            endif;
016200090318
016300090318       // se data spedizione presente la uso altrimenti uso la data
016400090318       // di generazione del rcd su FNRIG
016500090318            if rigdsp <> *zeros;
016600090318              wdsp = rigdsp;
016700090318            else;
016800090318              wdsp = rigdata;
016900090318            endif;
017000090316
017100090316       // elaboro le richieste fatte da internet
017200090316            if rigcau = 'I';
017300090316              exsr sr_elabI;
017400090316            endif;
017500090316
017600090316       // elaboro gli FTP e i CD
017700090316            if rigcau = 'P';
017800090316              exsr sr_elabP;
017900090316            endif;
018000090317
018100090316       // aggiorno FNRIG sempre
018200090316            rigdela = *date;
018300090316            update fnrig000;
018400090316
018500090316          enddo;
018600090316
018700090316          *inlr = *on;
018800090316
018900090316       //------------------------------------------------------------------------*
019000090316       //  Elaboro le richieste fatte da internet
019100090316       //------------------------------------------------------------------------*
019200090316          begsr sr_elabI;
019300090317
019400090317            $notaao = *off;
019500090317            $nota4a = *off;
019600090420
019700090420       // ricerco i figli del codice unificante se POD
019800090420            clear tibs10ds;
019900090427            d10drf = rigdata;
020000090420            d10tle = 'WW';
020100090420            d10paf = 'F';
020200090420            d10cod = rigksc;
020300090420            tibs10r(tibs10ds);
020400090217
020500090427       // bolla diversa elaboro
020600090220            if newbolla <> oldbolla;
020700090218
020800090220       // ma solo se la bolla è presente su titas
020900090220              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
021000090220              if not %found(titas30c);
021100090317                leavesr;
021200090220              endif;
021300090217
021400090220       // elaboro la spedizione letta
021500090220              exsr elabora;
021600090220       // mi salvo la spedizione letta
021700090220              oldbolla = newbolla;
021800090427       //          il cliente
021900090427              oldksc = rigksc;
022000090427       //          data/ora richiesta
022100090220              oldrich  = newrich;
022200090427              leavesr;
022300090427            endif;
022400090217
022500090427       // stessa bolla
022600090427       // cliente diverso elaboro
022700090427            if rigksc <> oldksc;
022800090427
022900090427       // elaboro la spedizione letta
023000090427              exsr elabora;
023100090427       // mi salvo la spedizione letta
023200090427              oldbolla = newbolla;
023300090427       //          il cliente
023400090427              oldksc = rigksc;
023500090427       //          data/ora richiesta
023600090427              oldrich  = newrich;
023700090427              leavesr;
023800090427            endif;
023900090427
024000090427       // stessa bolla e stesso cliente
024100090427       // se la richiesta è stata fatta lo stesso giorno e stessa hhmmss
024200090427       // non elaboro
024300090427            if oldrich = newrich;
024400090220       // in questo caso non elaboro il rcd di fnrig e imposto il campo rignoadd
024500090427              rignoadd = 'N';
024600090427              leavesr;
024700090427            endif;
024800090217
024900090220       // se la richiesta è stata fatta lo stesso giorno
025000090220       // ma in hhmmss diversi
025100090427            if olddata = rigdata;
025200090220       // controllo quanti minuti sono passati tra una richiesta e l'altra
025300090427              woldora = %time(oldora);
025400090427              wnewora = %time(rigora);
025500090427       // se non sono passati 30 minuti non elaboro
025600090427              if %diff(wnewora : woldora : *seconds) <= 1800;
025700090427                rignoadd = 'N';
025800090427                leavesr;
025900090427              endif;
026000090427
026100090427       // se sono passati 30 minuti elaboro
026200090427              exsr elabora;
026300090220       // mi salvo la spedizione letta
026400090427              oldbolla = newbolla;
026500090427       //          il cliente
026600090427              oldksc = rigksc;
026700090427       //          data/ora richiesta
026800090427              oldrich  = newrich;
026900090427              leavesr;
027000090427            endif;
027100090427
027200090427       // se la richiesta è stata fatta in giorni diversi elaboro
027300090427            exsr elabora;
027400090220       // mi salvo la spedizione letta
027500090427            oldbolla = newbolla;
027600090427       //          il cliente
027700090427            oldksc = rigksc;
027800090427       //          data/ora richiesta
027900090427            oldrich  = newrich;
028000090220
028100090316          ENDSR;
028200090316
028300090316       //------------------------------------------------------------------------*
028400090316       //  Elaboro FTP e CD
028500090316       //------------------------------------------------------------------------*
028600090316          begsr sr_elabP;
028700090316
028800090317            $notaao = *off;
028900090317            $nota4a = *off;
029000090318
029100090318       // imposto il KSC e CTR da Fnrig
029200090318            f_tasksc = rigksc;
029300090318            f_tasctr = %dec(rigctr:3:0);
029400090317
029500090318       // Elaborazione a riepilogo
029600090317            if rignsp = 0;
029700090317       // --> imposto la lnp = alla filiale cliente
029800090317              f_taslnp = %dec(%subst(%editc(rigksc:'X'):1:3):3:0);
029900090317       // --> imposto la lna = alla lnp
030000090317              f_taslna = f_taslnp;
030100090317       // --> non devo impostare il titaa rcd 'O' mitt.originale
030200090317       //     e nemmeno rif.mittente alfabetico tita4 rcd 'A'
030300090317              $notaao = *on;
030400090317              $nota4a = *on;
030500090318
030600090318       // Elaborazione a bolla
030700090318            else;
030800090318       // --> aggancio la bolla per recuperare i dati che servono
030900090318       //     per la scrittura della bolla addebito
031000090318              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
031100090318       //     se la bolla non la trovo non genero addebito
031200090318              if not %found(titas30c);
031300090318                leavesr;
031400090318              endif;
031500090317            endif;
031600090317
031700090316            exsr elabora;
031800090316
031900090316          ENDSR;
032000090220
032100090220       //------------------------------------------------------------------------*
032200090220       //  Elaboro la bolla letta
032300090220       //------------------------------------------------------------------------*
032400090220          begsr Elabora;
032500090220
032600090220       // -- > T I T A S 0 0 0
032700090303       // --------------------
032800090220            clear titas0;
032900090220
033000090220       // Anno
033100090318            tasaas = %dec(%subst(%editc(wdsp:'X'):1:4):4:0);
033200090220       // Linea di partenza
033300090220            taslnp = f_taslnp;
033400090220       // Codice bolla
033500090220            tascbo = 'FI';
033600090220       // Tipo bolla
033700090220            clear ds3a;
033800090220            tblkey = tascbo;
033900090220            chain (codut:'3A':tblkey) tabel00f;
034000090304            if not %found(tabel00f) or tblflg <> *blanks;
034100090220              leavesr;
034200090220            ENDIF;
034300090220            ds3a = tbluni;
034400090220            tastbl = §3atb1;
034500090220       // Mese/giorno
034600090318            tasmgs = %dec(%subst(%editc(wdsp:'X'):5:4):4:0);
034700090421
034800090220       // Cliente tassazione + codice tariffa
034900090421       // per PodImage non da INTERENT quello che era memorizzato su FNRIG
035000090421            if rigcau = 'P';
035100090421              tasksc = f_tasksc;
035200090421              tasctr = f_tasctr;
035300090421       // per PodImage da INTERENT quello della bolla
035400090421            else;
035500090421       // controllo se il cliente che ha effettuato la richiesta è il ksc o il ccm
035600090421       // della bolla
035700090421              select;
035800090421                when f_tasksc = rigksc;
035900090421                  tasksc = f_tasksc;
036000090421                  tasctr = f_tasctr;
036100090421                when f_tasccm = rigksc;
036200090421                  tasksc = f_tasccm;
036300090421                  tasctr = *zeros;
036400090421                other;
036500090421       // altrimenti controllo se è uno dei suoi figli
036600090421                  w0110 = f_tasksc;
036700090421                  if %lookup(w0110:skfigli) > *zeros;
036800090421                    tasksc = f_tasksc;
036900090421                    tasctr = f_tasctr;
037000090421                  else;
037100090421                    w0110 = f_tasccm;
037200090421                    if %lookup(w0110:skfigli) > *zeros;
037300090421                      tasksc = f_tasccm;
037400090421                      tasctr = *zeros;
037500090421                    else;
037600090421       // se non è uno dei figli non devo generare l'addebito
037700090421                      rignoadd = 'Y';
037800090421                      leavesr;
037900090421                    endif;
038000090421                  endif;
038100090421              endsl;
038200090421            endif;
038300090421
038400090220       // se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
038500090220            if %subst(%editc(tasksc:'X'):1:3) = '102' or
038600090220               %subst(%editc(tasksc:'X'):1:3) = '101';
038700090220               rignoadd = 'X';
038800090220               leavesr;
038900090220            ENDIF;
039000090220       // Linea di arrivo
039100090220            taslna = f_taslna;
039200090220       // Numero Colli
039300090317            tasncl = rigncl;
039400090220       // Peso bollettato + flag
039500090220            taspkb = 1;
039600090220            tasfpb = 'R';
039700090220       // Peso fatturato + flag
039800090220            taspkf = 1;
039900090220            tasfpf = 'R';
040000090220       // Volume bollettato + flag
040100090304            tasvlb = 0,001;
040200090220            tasfvb = 'R';
040300090220       // Volume fatturato + flag
040400090220            tasvlf = 0,001;
040500090220            tasfvf = 'R';
040600090220       // Tipo servizio
040700090220            tastsp = 'C';
040800090220       // Destinatario
040900090220            clear tibs69ds;
041000090220            i69kac = tasksc;
041100090220            i69kin = tasksc;
041200090317            tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
041300090220            if o69err = '1';
041400090220              leavesr;
041500090220            ENDIF;
041600090220            tasrsd = acorag;
041700090220            tasind = indvia;
041800090220            tascad = indcae;
041900090220            taslod = indcit;
042000090220            tasprd = indprv;
042100090220            tasnzd = indsta;
042200090220       // Codice tassazione destinatario
042300090220            tascts = '40';
042400090220       // Flag inoltro
042500090220            tasfin = 'C';
042600090220       // Cliente mittente
042700090220            tasccm = taslnp * 10000 + 8888;
042800090220       // Riferimento mittente numerico
042900090220            tasrmn = %dec(wnumbolla:15:0);
043000090220       // Codice tassazione mittente
043100090220            tasmct = '40';
043200090220       // Flag anteporto
043300090220            tasfap = 'C';
043400090220       // Natura merce
043500090220            tasnas = 'BOLLA FIRMATA';
043600090220       // Filiale bollettazione
043700090220            tasflb = taslnp;
043800090220       // Terminal Arrivo
043900090220            clear fnlv55ds;
044000090220            d55tpt = 'A';
044100090220            d55lin = taslna;
044200090220            d55drf = *date;
044300090220            fnlv55r (fnlv55ds);
044400090220            if d55err <> *blanks;
044500090220              leavesr;
044600090220            ENDIF;
044700090220            tastfa = d55tfa;
044800090220       // Terminal partenza
044900090220            clear fnlv55ds;
045000090220            d55tpt = 'P';
045100090220            d55lin = taslnp;
045200090220            d55drf = *date;
045300090220            fnlv55r (fnlv55ds);
045400090220            if d55err <> *blanks;
045500090220              leavesr;
045600090220            ENDIF;
045700090220            tastfp = d55Tfp;
045800090220       // Data immissione
045900090220            tasdim = woggi;
046000090220       // Sigla operatore
046100090220            tassop = '*';
046200090220       // Codice autotrasportatore
046300090220            taspdr = taslnp * 10000 + 999;
046400090220       // Data/Ora ritiro
046500090220            tasdrt = woggi;
046600090318            tashrt = %dec(%subst(%editc(wora:'X'):1:4):4:0);
046700090220       // Flag prestazione autotrasportatore
046800090220            tasfpp = 'P';
046900090220       // Trattamento merce
047000090220            tasctm = §3actm;
047100090220       // Data Borderò
047200090220            tasdbr = woggi;
047300090220       // Flag merce portata a magazzino
047400090220            tasfdn = 'S';
047500090220       // Flag ddt si/no
047600090220            tasll1 = 'Y';
047700090220       // Nazione e cap mittente
047800090220            tasnzm = indsta;
047900090220            tascam = indcae;
048000090220       // Varia se da impostare a zero
048100090220            if rigtas = 'N';
048200090220              tassv1 = §3asva;
048300090220            ENDIF;
048400090318       // Varia + importo se da forzare la tassazione
048500090318            if rigtas = 'F';
048600090318              tassv1 = §3asva;
048700090318              tasdiv = §gedcn;
048800090318              tasva1 = rigimp;
048900090318              tasimv = rigimp;
049000090318            ENDIF;
049100090220       // Falg bartolini/sdi
049200090304            Tasscl = 'B';
049300090303
049400090303       // Numero spedizione (Flnuf cod. 3)
049500090303            clear trul34ds;
049600090303            i34aas = tasaas;
049700090303            i34lnp = taslnp;
049800090304            trul34r(kpjba: trul34ds);
049900090303            if f_o34err <> *zeros;
050000090303              leavesr;
050100090303            ENDIF;
050200090303            tasnsp = o34nsp;
050300090303
050400090303       // -- > T I T A A 0 0 0
050500090303       // --------------------
050600090303
050700090303       // Rcd. 'O' - mittente originale
050800090317       // se richiesto
050900090317          if not $notaao;
051000090303            clear titaa0;
051100090303       // chiave spedizione
051200090303            taaaas = tasaas;
051300090303            taalnp = taslnp;
051400090303            taanrs = tasnrs;
051500090303            taansp = tasnsp;
051600090303       // tipo record
051700090303            taatrc = 'O';
051800090303       // ragione sociale destinatario
051900090303            taarsc = f_tasrsd;
052000090303            write TITAA0;
052100090317          endif;
052200090303
052300090303       // Rcd. 'M' - mittente
052400090303            clear titaa0;
052500090303       // chiave spedizione
052600090303            taaaas = tasaas;
052700090303            taalnp = taslnp;
052800090303            taanrs = tasnrs;
052900090303            taansp = tasnsp;
053000090303       // tipo record
053100090303            taatrc = 'M';
053200090303       // dati della filiale Sede 102
053300090303            chain wfil azorg01l;
053400090303            if not %found(azorg01l) or orgfva <> *blanks;
053500090303              clear orgind;
053600090303              clear orgcpf;
053700090303              clear orgloc;
053800090303              clear orgpro;
053900090303            endif;
054000090303
054100090303            taarsc = 'POD IMAGE';
054200090303            taaind = orgind;
054300090303            taacap = %editc(orgcpf:'X');
054400090303            taaloc = orgloc;
054500090303            taaprv = orgpro;
054600090303            write TITAA0;
054700090303
054800090303       // -- > T I T A 4 0 0 0
054900090303       // --------------------
055000090303
055100090303       // Rcd. 'A' - riferimenti
055200090317       // se richiesto
055400090304            clear tita4000;
055500090303            clear dta4a;
055600090303       // chiave spedizione
055700090303            ta4aas = tasaas;
055800090303            ta4lnp = taslnp;
055900090303            ta4nrs = tasnrs;
056000090303            ta4nsp = tasnsp;
056100090303       // tipo record
056200090303            ta4trc = 'A';
056300090303       // riferimento mittente alfabetico
056301160122            §ta4anas = 'BOLLA FIRMATA';
056302160210
056303160210          if not $nota4a;
056400090422            §ta4arma = %editc(f_tasrmn:'Z');
056401160210          endif;
056500090303            ta4not = dta4a;
056600090303            write TITA4000;
056800090303
056900090303            write TITAS0;
057000090303
057100090220          ENDSR;
057200090217
057300090217       //--------------------------------------------------------------
057400090218       // Reperimento Dati del job (Utente/Operativi).
057500090217       //--------------------------------------------------------------
057600090217       BEGSR DatiJob;
057700090217
057800090217         in(E) §AzUte;
057900090217         if NOT %error;
058000090217           in(E) §DatiUte;
058100090217         endif;
058200090217         if %error or RSut = *blanks;
058300090217           clear TIBS34ds;
058400090217           tibs34r(tibs34ds);
058500090217           in §AzUte;
058600090217           in §DatiUte;
058700090217         endif;
058800090318
058900090318           // - Reperimento divisa moneta di conto gestionale
059000090318           clear  dGED;
059100090318           clear  TIBS02ds;
059200090318           t02mod = 'C';
059300090318           T02cod = 'GED';
059400090318           T02ke1 = '1';
059500090318           TNTBE_RicercaControllo(kpjba : tibs02ds);
059600090318           if  T02err = *blank;
059700090318             dGED = T02uni;
059800090318           endif;
059900040511
060000090220       ENDSR;
