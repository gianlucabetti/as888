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
002400090318     d wdsp            s                   like(rigdsp)
002500090303     d wfil            s                   like(orgfil) inz(102)
002600090217     d wnewora         s               t
002700090220     d wnumbolla       s             15
002800090217     d woldora         s               t
002900090220     d woggi           s              8  0
003000090220     d wora            s              6  0
003100090421     d w0110           s             11  0
003200090220     d $fine           s               n
003300090317     d $notaao         s               n
003400090317     d $nota4a         s               n
003500040511
003600090217      //--------------------------------------------------------------
003700090217      // Definizione aree dati.
003800090217      //--------------------------------------------------------------
003900090217      // - Dati utente
004000090217     d §azute        e ds                  extname(azute00f)
004100090217     d                                     dtaara
004200090217     d §datiute      e ds                  extname(ddatiute)
004300090217     d                                     dtaara
004400090217
004500090217      //--------------------------------------------------------------
004600090217      // Definizione strutture dati.
004700090217      //--------------------------------------------------------------
004800090217
004900090217      // - Parametri ricevuti
005000090217     d kpjba         e ds
005100090217
005200090217      // - Reperimento dati utente
005300090217     d tibs34ds      e ds
005400090217
005500090217      // - Lettura anagrafiche cliente
005600090217     d tibs69ds      e ds                  inz
005700090317     d ds_CNACO      e ds                  extname(CNACO00F)
005800090317     d ds_CNIND      e ds                  extname(CNIND00F)
005900090317     d ds_CNCLP      e ds                  extname(CNCLP00F)
006000090317     d ds_FNCLS      e ds                  extname(FNCLS00F)
006100090318
006200090318       // - Parametri per Ricerca/controllo tabelle
006300090318     d TIBS02ds      e ds                  inz
006400090420
006500090420       // - Parametri per Reperimento legami
006600090420     d tibs10ds      e ds                  inz
006700090420     d  skfigli               21   5520  0 dim(500)
006800090217
006900090217      // - Tabella 3a
007000090217     d ds3a          e ds
007100090318       // - Tabella "GED" = Divise area gestionale
007200090318     d dGED          e ds                  inz
007300090217
007400090217     d dta4a         e ds
007500090217     d fnlv55ds      e ds
007600090217     d trul34ds      e ds
007700090217     d  f_i34tla     e                     extfld(i34tla)
007800090217     d  f_o34err     e                     extfld(o34err)
007900090217     d  f_o34msg     e                     extfld(o34msg)
008000090217
008100090217     d oldbolla        ds
008200090217     d  oldaas                 1      4  0
008300090217     d  oldlnp                 5      7  0
008400090217     d  oldnrs                 8      9  0
008500090217     d  oldnsp                10     16  0
008600090217
008700090217     d newbolla        ds
008800090304     d  rigaas                 1      4  0
008900090304     d  riglnp                 5      7  0
009000090304     d  rignrs                 8      9  0
009100090304     d  rignsp                10     16  0
009200090217
009300090217     d oldrich         ds
009400090217     d  olddata                1      8  0
009500090217     d  oldora                 9     14  0
009600090217
009700090217     d newrich         ds
009800090217     d  rigdata                1      8  0
009900090217     d  rigora                 9     14  0
010000090217
010100090217      //--------------------------------------------------------------
010200090217      // Definizione procedure usate.
010300090217      //--------------------------------------------------------------
010400090220      // - Reperimento terminal di arrivo/partenza
010500090317      /copy gaitrasrc/srcprotopr,fnlv55r
010600090220
010700090217      // - Reperimento dati utente
010800090317      /copy gaitrasrc/srcprotopr,tibs34r
010900090217
011000090217      // - Lettura anagrafiche cliente
011100090317      /copy gaitrasrc/srcprotopr,tibs69r
011200090318
011300090318       // - Ricerca/Controllo tabelle
011400090318      /copy gaitrasrc/srcprotopr,tibs02r
011500090420
011600090420       // - Reperimento legami
011700090420      /copy gaitrasrc/srcprotopr,tibs10r
011800090304
011900090304      // - Numero spedizione
012000090304     d trul34r         pr                  extpgm('TRUL34R')
012100090304     d  kpjba                              likeds(KPJBA)
012200090304     d  trul34ds                           likeds(TRUL34ds)
012300090217
012400090217      //--------------------------------------------------------------
012500090217     c     *Entry        plist
012600090217     c                   parm                    kpjba
012700090217
012800090217      /free
012900090217
013000090217       // Recupero dati del JOB
013100090220          exsr DatiJob;
013200090220
013300090220          $fine = *off;
013400090220          woggi = %dec(%date());
013500090220          wora  = %dec(%time());
013600090304
013700090304          setll krig fnrig01l;
013800090217
013900090220       // Leggo fnrig
014000090220          dow not $fine;
014100090304            reade krig fnrig01l;
014200090220
014300090304            if %eof(fnrig01l);
014400090220              $fine = *on;
014500090220              leave;
014600090220            endif;
014700090316
014800090316       // elaboro solo i Pod Image da Internet, FTP, CD
014900090316            if rigcau <> 'I' and rigcau <> 'P';
015000090316              iter;
015100090316            endif;
015200090317
015300090317       // imposto il n. bolla
015400090317            if rignsp > 0;
015500090325              wnumbolla =  %subst(%editc(%dec(rigaas):'X'):3:2) +
015600090317                           %editc(%dec(riglnp):'X') +
015700090317                           %editc(%dec(rignrs):'X') + %editc(%dec(rignsp):'X');
015800090317            else;
015900090317              wnumbolla = '1';
016000090317            endif;
016100090318
016200090318       // se data spedizione presente la uso altrimenti uso la data
016300090318       // di generazione del rcd su FNRIG
016400090318            if rigdsp <> *zeros;
016500090318              wdsp = rigdsp;
016600090318            else;
016700090318              wdsp = rigdata;
016800090318            endif;
016900090316
017000090316       // elaboro le richieste fatte da internet
017100090316            if rigcau = 'I';
017200090316              exsr sr_elabI;
017300090316            endif;
017400090316
017500090316       // elaboro gli FTP e i CD
017600090316            if rigcau = 'P';
017700090316              exsr sr_elabP;
017800090316            endif;
017900090317
018000090316       // aggiorno FNRIG sempre
018100090316            rigdela = *date;
018200090316            update fnrig000;
018300090316
018400090316          enddo;
018500090316
018600090316          *inlr = *on;
018700090316
018800090316       //------------------------------------------------------------------------*
018900090316       //  Elaboro le richieste fatte da internet
019000090316       //------------------------------------------------------------------------*
019100090316          begsr sr_elabI;
019200090317
019300090317            $notaao = *off;
019400090317            $nota4a = *off;
019500090420
019600090420       // ricerco i figli del codice unificante se POD
019700090420            clear tibs10ds;
019800090420            d10drf = *date;
019900090420            d10tle = 'WW';
020000090420            d10paf = 'F';
020100090420            d10cod = rigksc;
020200090420            tibs10r(tibs10ds);
020300090217
020400090220       // se bolla diversa elaboro
020500090220            if newbolla <> oldbolla;
020600090218
020700090220       // ma solo se la bolla è presente su titas
020800090220              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
020900090220              if not %found(titas30c);
021000090317                leavesr;
021100090220              endif;
021200090217
021300090220       // elaboro la spedizione letta
021400090220              exsr elabora;
021500090220       // mi salvo la spedizione letta
021600090220              oldbolla = newbolla;
021700090220              oldrich  = newrich;
021800090217
021900090220       // se stessa bolla
022000090220            else;
022100090220       // controllo se la richiesta è stata fatta lo stesso giorno e stessa hhmmss
022200090220              if oldrich = newrich;
022300090220       // in questo caso non elaboro il rcd di fnrig e imposto il campo rignoadd
022400090220                rignoadd = 'N';
022500090217
022600090220       // se la richiesta è stata fatta lo stesso giorno
022700090220              else;
022800090220       // ma in hhmmss diversi
022900090220                if olddata = rigdata;
023000090220       // controllo quanti minuti sono passati tra una richiesta e l'altra
023100090220                  woldora = %time(oldora);
023200090220                  wnewora = %time(rigora);
023300090220       // se non sono passati 30 minuti non genero l'addebito
023400090220                  if %diff(wnewora : woldora : *seconds) <= 1800;
023500090220                    rignoadd = 'N';
023600090220                  else;
023700090220       // elaboro la spedizione letta
023800090220                    exsr elabora;
023900090220       // mi salvo la spedizione letta
024000090220                    oldbolla = newbolla;
024100090220                    oldrich  = newrich;
024200090303                  endif;
024300090220       // se la richiesta è stata fatta in giorni diversi
024400090220                else;
024500090220       // elaboro la spedizione letta
024600090220                  exsr elabora;
024700090220       // mi salvo la spedizione letta
024800090220                  oldbolla = newbolla;
024900090220                  oldrich  = newrich;
025000090303                endif;
025100090303              endif;
025200090303            endif;
025300090220
025400090316          ENDSR;
025500090316
025600090316       //------------------------------------------------------------------------*
025700090316       //  Elaboro FTP e CD
025800090316       //------------------------------------------------------------------------*
025900090316          begsr sr_elabP;
026000090316
026100090317            $notaao = *off;
026200090317            $nota4a = *off;
026300090318
026400090318       // imposto il KSC e CTR da Fnrig
026500090318            f_tasksc = rigksc;
026600090318            f_tasctr = %dec(rigctr:3:0);
026700090317
026800090318       // Elaborazione a riepilogo
026900090317            if rignsp = 0;
027000090317       // --> imposto la lnp = alla filiale cliente
027100090317              f_taslnp = %dec(%subst(%editc(rigksc:'X'):1:3):3:0);
027200090317       // --> imposto la lna = alla lnp
027300090317              f_taslna = f_taslnp;
027400090317       // --> non devo impostare il titaa rcd 'O' mitt.originale
027500090317       //     e nemmeno rif.mittente alfabetico tita4 rcd 'A'
027600090317              $notaao = *on;
027700090317              $nota4a = *on;
027800090318
027900090318       // Elaborazione a bolla
028000090318            else;
028100090318       // --> aggancio la bolla per recuperare i dati che servono
028200090318       //     per la scrittura della bolla addebito
028300090318              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
028400090318       //     se la bolla non la trovo non genero addebito
028500090318              if not %found(titas30c);
028600090318                leavesr;
028700090318              endif;
028800090317            endif;
028900090317
029000090316            exsr elabora;
029100090316
029200090316          ENDSR;
029300090220
029400090220       //------------------------------------------------------------------------*
029500090220       //  Elaboro la bolla letta
029600090220       //------------------------------------------------------------------------*
029700090220          begsr Elabora;
029800090220
029900090220       // -- > T I T A S 0 0 0
030000090303       // --------------------
030100090220            clear titas0;
030200090220
030300090220       // Anno
030400090318            tasaas = %dec(%subst(%editc(wdsp:'X'):1:4):4:0);
030500090220       // Linea di partenza
030600090220            taslnp = f_taslnp;
030700090220       // Codice bolla
030800090220            tascbo = 'FI';
030900090220       // Tipo bolla
031000090220            clear ds3a;
031100090220            tblkey = tascbo;
031200090220            chain (codut:'3A':tblkey) tabel00f;
031300090304            if not %found(tabel00f) or tblflg <> *blanks;
031400090220              leavesr;
031500090220            ENDIF;
031600090220            ds3a = tbluni;
031700090220            tastbl = §3atb1;
031800090220       // Mese/giorno
031900090318            tasmgs = %dec(%subst(%editc(wdsp:'X'):5:4):4:0);
032000090421
032100090220       // Cliente tassazione + codice tariffa
032200090421       // per PodImage non da INTERENT quello che era memorizzato su FNRIG
032300090421            if rigcau = 'P';
032400090421              tasksc = f_tasksc;
032500090421              tasctr = f_tasctr;
032600090421       // per PodImage da INTERENT quello della bolla
032700090421            else;
032800090421       // controllo se il cliente che ha effettuato la richiesta è il ksc o il ccm
032900090421       // della bolla
033000090421              select;
033100090421                when f_tasksc = rigksc;
033200090421                  tasksc = f_tasksc;
033300090421                  tasctr = f_tasctr;
033400090421                when f_tasccm = rigksc;
033500090421                  tasksc = f_tasccm;
033600090421                  tasctr = *zeros;
033700090421                other;
033800090421       // altrimenti controllo se è uno dei suoi figli
033900090421                  w0110 = f_tasksc;
034000090421                  if %lookup(w0110:skfigli) > *zeros;
034100090421                    tasksc = f_tasksc;
034200090421                    tasctr = f_tasctr;
034300090421                  else;
034400090421                    w0110 = f_tasccm;
034500090421                    if %lookup(w0110:skfigli) > *zeros;
034600090421                      tasksc = f_tasccm;
034700090421                      tasctr = *zeros;
034800090421                    else;
034900090421       // se non è uno dei figli non devo generare l'addebito
035000090421                      rignoadd = 'Y';
035100090421                      leavesr;
035200090421                    endif;
035300090421                  endif;
035400090421              endsl;
035500090421            endif;
035600090421
035700090220       // se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
035800090220            if %subst(%editc(tasksc:'X'):1:3) = '102' or
035900090220               %subst(%editc(tasksc:'X'):1:3) = '101';
036000090220               rignoadd = 'X';
036100090220               leavesr;
036200090220            ENDIF;
036300090220       // Linea di arrivo
036400090220            taslna = f_taslna;
036500090220       // Numero Colli
036600090317            tasncl = rigncl;
036700090220       // Peso bollettato + flag
036800090220            taspkb = 1;
036900090220            tasfpb = 'R';
037000090220       // Peso fatturato + flag
037100090220            taspkf = 1;
037200090220            tasfpf = 'R';
037300090220       // Volume bollettato + flag
037400090304            tasvlb = 0,001;
037500090220            tasfvb = 'R';
037600090220       // Volume fatturato + flag
037700090220            tasvlf = 0,001;
037800090220            tasfvf = 'R';
037900090220       // Tipo servizio
038000090220            tastsp = 'C';
038100090220       // Destinatario
038200090220            clear tibs69ds;
038300090220            i69kac = tasksc;
038400090220            i69kin = tasksc;
038500090317            tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
038600090220            if o69err = '1';
038700090220              leavesr;
038800090220            ENDIF;
038900090220            tasrsd = acorag;
039000090220            tasind = indvia;
039100090220            tascad = indcae;
039200090220            taslod = indcit;
039300090220            tasprd = indprv;
039400090220            tasnzd = indsta;
039500090220       // Codice tassazione destinatario
039600090220            tascts = '40';
039700090220       // Flag inoltro
039800090220            tasfin = 'C';
039900090220       // Cliente mittente
040000090220            tasccm = taslnp * 10000 + 8888;
040100090220       // Riferimento mittente numerico
040200090220            tasrmn = %dec(wnumbolla:15:0);
040300090220       // Codice tassazione mittente
040400090220            tasmct = '40';
040500090220       // Flag anteporto
040600090220            tasfap = 'C';
040700090220       // Natura merce
040800090220            tasnas = 'BOLLA FIRMATA';
040900090220       // Filiale bollettazione
041000090220            tasflb = taslnp;
041100090220       // Terminal Arrivo
041200090220            clear fnlv55ds;
041300090220            d55tpt = 'A';
041400090220            d55lin = taslna;
041500090220            d55drf = *date;
041600090220            fnlv55r (fnlv55ds);
041700090220            if d55err <> *blanks;
041800090220              leavesr;
041900090220            ENDIF;
042000090220            tastfa = d55tfa;
042100090220       // Terminal partenza
042200090220            clear fnlv55ds;
042300090220            d55tpt = 'P';
042400090220            d55lin = taslnp;
042500090220            d55drf = *date;
042600090220            fnlv55r (fnlv55ds);
042700090220            if d55err <> *blanks;
042800090220              leavesr;
042900090220            ENDIF;
043000090220            tastfp = d55Tfp;
043100090220       // Data immissione
043200090220            tasdim = woggi;
043300090220       // Sigla operatore
043400090220            tassop = '*';
043500090220       // Codice autotrasportatore
043600090220            taspdr = taslnp * 10000 + 999;
043700090220       // Data/Ora ritiro
043800090220            tasdrt = woggi;
043900090318            tashrt = %dec(%subst(%editc(wora:'X'):1:4):4:0);
044000090220       // Flag prestazione autotrasportatore
044100090220            tasfpp = 'P';
044200090220       // Trattamento merce
044300090220            tasctm = §3actm;
044400090220       // Data Borderò
044500090220            tasdbr = woggi;
044600090220       // Flag merce portata a magazzino
044700090220            tasfdn = 'S';
044800090220       // Flag ddt si/no
044900090220            tasll1 = 'Y';
045000090220       // Nazione e cap mittente
045100090220            tasnzm = indsta;
045200090220            tascam = indcae;
045300090220       // Varia se da impostare a zero
045400090220            if rigtas = 'N';
045500090220              tassv1 = §3asva;
045600090220            ENDIF;
045700090318       // Varia + importo se da forzare la tassazione
045800090318            if rigtas = 'F';
045900090318              tassv1 = §3asva;
046000090318              tasdiv = §gedcn;
046100090318              tasva1 = rigimp;
046200090318              tasimv = rigimp;
046300090318            ENDIF;
046400090220       // Falg bartolini/sdi
046500090304            Tasscl = 'B';
046600090303
046700090303       // Numero spedizione (Flnuf cod. 3)
046800090303            clear trul34ds;
046900090303            i34aas = tasaas;
047000090303            i34lnp = taslnp;
047100090304            trul34r(kpjba: trul34ds);
047200090303            if f_o34err <> *zeros;
047300090303              leavesr;
047400090303            ENDIF;
047500090303            tasnsp = o34nsp;
047600090303
047700090303       // -- > T I T A A 0 0 0
047800090303       // --------------------
047900090303
048000090303       // Rcd. 'O' - mittente originale
048100090317       // se richiesto
048200090317          if not $notaao;
048300090303            clear titaa0;
048400090303       // chiave spedizione
048500090303            taaaas = tasaas;
048600090303            taalnp = taslnp;
048700090303            taanrs = tasnrs;
048800090303            taansp = tasnsp;
048900090303       // tipo record
049000090303            taatrc = 'O';
049100090303       // ragione sociale destinatario
049200090303            taarsc = f_tasrsd;
049300090303            write TITAA0;
049400090317          endif;
049500090303
049600090303       // Rcd. 'M' - mittente
049700090303            clear titaa0;
049800090303       // chiave spedizione
049900090303            taaaas = tasaas;
050000090303            taalnp = taslnp;
050100090303            taanrs = tasnrs;
050200090303            taansp = tasnsp;
050300090303       // tipo record
050400090303            taatrc = 'M';
050500090303       // dati della filiale Sede 102
050600090303            chain wfil azorg01l;
050700090303            if not %found(azorg01l) or orgfva <> *blanks;
050800090303              clear orgind;
050900090303              clear orgcpf;
051000090303              clear orgloc;
051100090303              clear orgpro;
051200090303            endif;
051300090303
051400090303            taarsc = 'POD IMAGE';
051500090303            taaind = orgind;
051600090303            taacap = %editc(orgcpf:'X');
051700090303            taaloc = orgloc;
051800090303            taaprv = orgpro;
051900090303            write TITAA0;
052000090303
052100090303       // -- > T I T A 4 0 0 0
052200090303       // --------------------
052300090303
052400090303       // Rcd. 'A' - riferimenti
052500090317       // se richiesto
052600090317          if not $nota4a;
052700090304            clear tita4000;
052800090303            clear dta4a;
052900090303       // chiave spedizione
053000090303            ta4aas = tasaas;
053100090303            ta4lnp = taslnp;
053200090303            ta4nrs = tasnrs;
053300090303            ta4nsp = tasnsp;
053400090303       // tipo record
053500090303            ta4trc = 'A';
053600090303       // riferimento mittente alfabetico
053700090422            §ta4arma = %editc(f_tasrmn:'Z');
053800090303            ta4not = dta4a;
053900090303            write TITA4000;
054000090317          endif;
054100090303
054200090303            write TITAS0;
054300090303
054400090220          ENDSR;
054500090217
054600090217       //--------------------------------------------------------------
054700090218       // Reperimento Dati del job (Utente/Operativi).
054800090217       //--------------------------------------------------------------
054900090217       BEGSR DatiJob;
055000090217
055100090217         in(E) §AzUte;
055200090217         if NOT %error;
055300090217           in(E) §DatiUte;
055400090217         endif;
055500090217         if %error or RSut = *blanks;
055600090217           clear TIBS34ds;
055700090217           tibs34r(tibs34ds);
055800090217           in §AzUte;
055900090217           in §DatiUte;
056000090217         endif;
056100090318
056200090318           // - Reperimento divisa moneta di conto gestionale
056300090318           clear  dGED;
056400090318           clear  TIBS02ds;
056500090318           t02mod = 'C';
056600090318           T02cod = 'GED';
056700090318           T02ke1 = '1';
056800090318           TNTBE_RicercaControllo(kpjba : tibs02ds);
056900090318           if  T02err = *blank;
057000090318             dGED = T02uni;
057100090318           endif;
057200040511
057300090220       ENDSR;
