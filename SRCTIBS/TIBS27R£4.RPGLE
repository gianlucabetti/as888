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
003100090220     d $fine           s               n
003200090317     d $notaao         s               n
003300090317     d $nota4a         s               n
003400040511
003500090217      //--------------------------------------------------------------
003600090217      // Definizione aree dati.
003700090217      //--------------------------------------------------------------
003800090217      // - Dati utente
003900090217     d §azute        e ds                  extname(azute00f)
004000090217     d                                     dtaara
004100090217     d §datiute      e ds                  extname(ddatiute)
004200090217     d                                     dtaara
004300090217
004400090217      //--------------------------------------------------------------
004500090217      // Definizione strutture dati.
004600090217      //--------------------------------------------------------------
004700090217
004800090217      // - Parametri ricevuti
004900090217     d kpjba         e ds
005000090217
005100090217      // - Reperimento dati utente
005200090217     d tibs34ds      e ds
005300090217
005400090217      // - Lettura anagrafiche cliente
005500090217     d tibs69ds      e ds                  inz
005600090317     d ds_CNACO      e ds                  extname(CNACO00F)
005700090317     d ds_CNIND      e ds                  extname(CNIND00F)
005800090317     d ds_CNCLP      e ds                  extname(CNCLP00F)
005900090317     d ds_FNCLS      e ds                  extname(FNCLS00F)
006000090318
006100090318       // - Parametri per Ricerca/controllo tabelle
006200090318     d TIBS02ds      e ds                  inz
006300090217
006400090217      // - Tabella 3a
006500090217     d ds3a          e ds
006600090318       // - Tabella "GED" = Divise area gestionale
006700090318     d dGED          e ds                  inz
006800090217
006900090217     d dta4a         e ds
007000090217     d fnlv55ds      e ds
007100090217     d trul34ds      e ds
007200090217     d  f_i34tla     e                     extfld(i34tla)
007300090217     d  f_o34err     e                     extfld(o34err)
007400090217     d  f_o34msg     e                     extfld(o34msg)
007500090217
007600090217     d oldbolla        ds
007700090217     d  oldaas                 1      4  0
007800090217     d  oldlnp                 5      7  0
007900090217     d  oldnrs                 8      9  0
008000090217     d  oldnsp                10     16  0
008100090217
008200090217     d newbolla        ds
008300090304     d  rigaas                 1      4  0
008400090304     d  riglnp                 5      7  0
008500090304     d  rignrs                 8      9  0
008600090304     d  rignsp                10     16  0
008700090217
008800090217     d oldrich         ds
008900090217     d  olddata                1      8  0
009000090217     d  oldora                 9     14  0
009100090217
009200090217     d newrich         ds
009300090217     d  rigdata                1      8  0
009400090217     d  rigora                 9     14  0
009500090217
009600090217      //--------------------------------------------------------------
009700090217      // Definizione procedure usate.
009800090217      //--------------------------------------------------------------
009900090220      // - Reperimento terminal di arrivo/partenza
010000090317      /copy gaitrasrc/srcprotopr,fnlv55r
010100090220
010200090217      // - Reperimento dati utente
010300090317      /copy gaitrasrc/srcprotopr,tibs34r
010400090217
010500090217      // - Lettura anagrafiche cliente
010600090317      /copy gaitrasrc/srcprotopr,tibs69r
010700090318
010800090318       // - Ricerca/Controllo tabelle
010900090318      /copy gaitrasrc/srcprotopr,tibs02r
011000090304
011100090304      // - Numero spedizione
011200090304     d trul34r         pr                  extpgm('TRUL34R')
011300090304     d  kpjba                              likeds(KPJBA)
011400090304     d  trul34ds                           likeds(TRUL34ds)
011500090217
011600090217      //--------------------------------------------------------------
011700090217     c     *Entry        plist
011800090217     c                   parm                    kpjba
011900090217
012000090217      /free
012100090217
012200090217       // Recupero dati del JOB
012300090220          exsr DatiJob;
012400090220
012500090220          $fine = *off;
012600090220          woggi = %dec(%date());
012700090220          wora  = %dec(%time());
012800090304
012900090304          setll krig fnrig01l;
013000090217
013100090220       // Leggo fnrig
013200090220          dow not $fine;
013300090304            reade krig fnrig01l;
013400090220
013500090304            if %eof(fnrig01l);
013600090220              $fine = *on;
013700090220              leave;
013800090220            endif;
013900090316
014000090316       // elaboro solo i Pod Image da Internet, FTP, CD
014100090316            if rigcau <> 'I' and rigcau <> 'P';
014200090316              iter;
014300090316            endif;
014400090317
014500090317       // imposto il n. bolla
014600090317            if rignsp > 0;
014700090325              wnumbolla =  %subst(%editc(%dec(rigaas):'X'):3:2) +
014800090317                           %editc(%dec(riglnp):'X') +
014900090317                           %editc(%dec(rignrs):'X') + %editc(%dec(rignsp):'X');
015000090317            else;
015100090317              wnumbolla = '1';
015200090317            endif;
015300090318
015400090318       // se data spedizione presente la uso altrimenti uso la data
015500090318       // di generazione del rcd su FNRIG
015600090318            if rigdsp <> *zeros;
015700090318              wdsp = rigdsp;
015800090318            else;
015900090318              wdsp = rigdata;
016000090318            endif;
016100090316
016200090316       // elaboro le richieste fatte da internet
016300090316            if rigcau = 'I';
016400090316              exsr sr_elabI;
016500090316            endif;
016600090316
016700090316       // elaboro gli FTP e i CD
016800090316            if rigcau = 'P';
016900090316              exsr sr_elabP;
017000090316            endif;
017100090317
017200090316       // aggiorno FNRIG sempre
017300090316            rigdela = *date;
017400090316            update fnrig000;
017500090316
017600090316          enddo;
017700090316
017800090316          *inlr = *on;
017900090316
018000090316       //------------------------------------------------------------------------*
018100090316       //  Elaboro le richieste fatte da internet
018200090316       //------------------------------------------------------------------------*
018300090316          begsr sr_elabI;
018400090317
018500090317            $notaao = *off;
018600090317            $nota4a = *off;
018700090217
018800090220       // se bolla diversa elaboro
018900090220            if newbolla <> oldbolla;
019000090218
019100090220       // ma solo se la bolla è presente su titas
019200090220              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
019300090220              if not %found(titas30c);
019400090317                leavesr;
019500090220              endif;
019600090217
019700090220       // elaboro la spedizione letta
019800090220              exsr elabora;
019900090220       // mi salvo la spedizione letta
020000090220              oldbolla = newbolla;
020100090220              oldrich  = newrich;
020200090217
020300090220       // se stessa bolla
020400090220            else;
020500090220       // controllo se la richiesta è stata fatta lo stesso giorno e stessa hhmmss
020600090220              if oldrich = newrich;
020700090220       // in questo caso non elaboro il rcd di fnrig e imposto il campo rignoadd
020800090220                rignoadd = 'N';
020900090217
021000090220       // se la richiesta è stata fatta lo stesso giorno
021100090220              else;
021200090220       // ma in hhmmss diversi
021300090220                if olddata = rigdata;
021400090220       // controllo quanti minuti sono passati tra una richiesta e l'altra
021500090220                  woldora = %time(oldora);
021600090220                  wnewora = %time(rigora);
021700090220       // se non sono passati 30 minuti non genero l'addebito
021800090220                  if %diff(wnewora : woldora : *seconds) <= 1800;
021900090220                    rignoadd = 'N';
022000090220                  else;
022100090220       // elaboro la spedizione letta
022200090220                    exsr elabora;
022300090220       // mi salvo la spedizione letta
022400090220                    oldbolla = newbolla;
022500090220                    oldrich  = newrich;
022600090303                  endif;
022700090220       // se la richiesta è stata fatta in giorni diversi
022800090220                else;
022900090220       // elaboro la spedizione letta
023000090220                  exsr elabora;
023100090220       // mi salvo la spedizione letta
023200090220                  oldbolla = newbolla;
023300090220                  oldrich  = newrich;
023400090303                endif;
023500090303              endif;
023600090303            endif;
023700090220
023800090316          ENDSR;
023900090316
024000090316       //------------------------------------------------------------------------*
024100090316       //  Elaboro FTP e CD
024200090316       //------------------------------------------------------------------------*
024300090316          begsr sr_elabP;
024400090316
024500090317            $notaao = *off;
024600090317            $nota4a = *off;
024700090318
024800090318       // imposto il KSC e CTR da Fnrig
024900090318            f_tasksc = rigksc;
025000090318            f_tasctr = %dec(rigctr:3:0);
025100090317
025200090318       // Elaborazione a riepilogo
025300090317            if rignsp = 0;
025400090317       // --> imposto la lnp = alla filiale cliente
025500090317              f_taslnp = %dec(%subst(%editc(rigksc:'X'):1:3):3:0);
025600090317       // --> imposto la lna = alla lnp
025700090317              f_taslna = f_taslnp;
025800090317       // --> non devo impostare il titaa rcd 'O' mitt.originale
025900090317       //     e nemmeno rif.mittente alfabetico tita4 rcd 'A'
026000090317              $notaao = *on;
026100090317              $nota4a = *on;
026200090318
026300090318       // Elaborazione a bolla
026400090318            else;
026500090318       // --> aggancio la bolla per recuperare i dati che servono
026600090318       //     per la scrittura della bolla addebito
026700090318              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
026800090318       //     se la bolla non la trovo non genero addebito
026900090318              if not %found(titas30c);
027000090318                leavesr;
027100090318              endif;
027200090317            endif;
027300090317
027400090316            exsr elabora;
027500090316
027600090316          ENDSR;
027700090220
027800090220       //------------------------------------------------------------------------*
027900090220       //  Elaboro la bolla letta
028000090220       //------------------------------------------------------------------------*
028100090220          begsr Elabora;
028200090220
028300090220       // -- > T I T A S 0 0 0
028400090303       // --------------------
028500090220            clear titas0;
028600090220
028700090220       // Anno
028800090318            tasaas = %dec(%subst(%editc(wdsp:'X'):1:4):4:0);
028900090220       // Linea di partenza
029000090220            taslnp = f_taslnp;
029100090220       // Codice bolla
029200090220            tascbo = 'FI';
029300090220       // Tipo bolla
029400090220            clear ds3a;
029500090220            tblkey = tascbo;
029600090220            chain (codut:'3A':tblkey) tabel00f;
029700090304            if not %found(tabel00f) or tblflg <> *blanks;
029800090220              leavesr;
029900090220            ENDIF;
030000090220            ds3a = tbluni;
030100090220            tastbl = §3atb1;
030200090220       // Mese/giorno
030300090318            tasmgs = %dec(%subst(%editc(wdsp:'X'):5:4):4:0);
030400090220       // Cliente tassazione + codice tariffa
030500090220            tasksc = f_tasksc;
030600090220            tasctr = f_tasctr;
030700090220       // se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
030800090220            if %subst(%editc(tasksc:'X'):1:3) = '102' or
030900090220               %subst(%editc(tasksc:'X'):1:3) = '101';
031000090220               rignoadd = 'X';
031100090220               leavesr;
031200090220            ENDIF;
031300090220       // Linea di arrivo
031400090220            taslna = f_taslna;
031500090220       // Numero Colli
031600090317            tasncl = rigncl;
031700090220       // Peso bollettato + flag
031800090220            taspkb = 1;
031900090220            tasfpb = 'R';
032000090220       // Peso fatturato + flag
032100090220            taspkf = 1;
032200090220            tasfpf = 'R';
032300090220       // Volume bollettato + flag
032400090304            tasvlb = 0,001;
032500090220            tasfvb = 'R';
032600090220       // Volume fatturato + flag
032700090220            tasvlf = 0,001;
032800090220            tasfvf = 'R';
032900090220       // Tipo servizio
033000090220            tastsp = 'C';
033100090220       // Destinatario
033200090220            clear tibs69ds;
033300090220            i69kac = tasksc;
033400090220            i69kin = tasksc;
033500090317            tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
033600090220            if o69err = '1';
033700090220              leavesr;
033800090220            ENDIF;
033900090220            tasrsd = acorag;
034000090220            tasind = indvia;
034100090220            tascad = indcae;
034200090220            taslod = indcit;
034300090220            tasprd = indprv;
034400090220            tasnzd = indsta;
034500090220       // Codice tassazione destinatario
034600090220            tascts = '40';
034700090220       // Flag inoltro
034800090220            tasfin = 'C';
034900090220       // Cliente mittente
035000090220            tasccm = taslnp * 10000 + 8888;
035100090220       // Riferimento mittente numerico
035200090220            tasrmn = %dec(wnumbolla:15:0);
035300090220       // Codice tassazione mittente
035400090220            tasmct = '40';
035500090220       // Flag anteporto
035600090220            tasfap = 'C';
035700090220       // Natura merce
035800090220            tasnas = 'BOLLA FIRMATA';
035900090220       // Filiale bollettazione
036000090220            tasflb = taslnp;
036100090220       // Terminal Arrivo
036200090220            clear fnlv55ds;
036300090220            d55tpt = 'A';
036400090220            d55lin = taslna;
036500090220            d55drf = *date;
036600090220            fnlv55r (fnlv55ds);
036700090220            if d55err <> *blanks;
036800090220              leavesr;
036900090220            ENDIF;
037000090220            tastfa = d55tfa;
037100090220       // Terminal partenza
037200090220            clear fnlv55ds;
037300090220            d55tpt = 'P';
037400090220            d55lin = taslnp;
037500090220            d55drf = *date;
037600090220            fnlv55r (fnlv55ds);
037700090220            if d55err <> *blanks;
037800090220              leavesr;
037900090220            ENDIF;
038000090220            tastfp = d55Tfp;
038100090220       // Data immissione
038200090220            tasdim = woggi;
038300090220       // Sigla operatore
038400090220            tassop = '*';
038500090220       // Codice autotrasportatore
038600090220            taspdr = taslnp * 10000 + 999;
038700090220       // Data/Ora ritiro
038800090220            tasdrt = woggi;
038900090318            tashrt = %dec(%subst(%editc(wora:'X'):1:4):4:0);
039000090220       // Flag prestazione autotrasportatore
039100090220            tasfpp = 'P';
039200090220       // Trattamento merce
039300090220            tasctm = §3actm;
039400090220       // Data Borderò
039500090220            tasdbr = woggi;
039600090220       // Flag merce portata a magazzino
039700090220            tasfdn = 'S';
039800090220       // Flag ddt si/no
039900090220            tasll1 = 'Y';
040000090220       // Nazione e cap mittente
040100090220            tasnzm = indsta;
040200090220            tascam = indcae;
040300090220       // Varia se da impostare a zero
040400090220            if rigtas = 'N';
040500090220              tassv1 = §3asva;
040600090220            ENDIF;
040700090318       // Varia + importo se da forzare la tassazione
040800090318            if rigtas = 'F';
040900090318              tassv1 = §3asva;
041000090318              tasdiv = §gedcn;
041100090318              tasva1 = rigimp;
041200090318              tasimv = rigimp;
041300090318            ENDIF;
041400090220       // Falg bartolini/sdi
041500090304            Tasscl = 'B';
041600090303
041700090303       // Numero spedizione (Flnuf cod. 3)
041800090303            clear trul34ds;
041900090303            i34aas = tasaas;
042000090303            i34lnp = taslnp;
042100090304            trul34r(kpjba: trul34ds);
042200090303            if f_o34err <> *zeros;
042300090303              leavesr;
042400090303            ENDIF;
042500090303            tasnsp = o34nsp;
042600090303
042700090303       // -- > T I T A A 0 0 0
042800090303       // --------------------
042900090303
043000090303       // Rcd. 'O' - mittente originale
043100090317       // se richiesto
043200090317          if not $notaao;
043300090303            clear titaa0;
043400090303       // chiave spedizione
043500090303            taaaas = tasaas;
043600090303            taalnp = taslnp;
043700090303            taanrs = tasnrs;
043800090303            taansp = tasnsp;
043900090303       // tipo record
044000090303            taatrc = 'O';
044100090303       // ragione sociale destinatario
044200090303            taarsc = f_tasrsd;
044300090303            write TITAA0;
044400090317          endif;
044500090303
044600090303       // Rcd. 'M' - mittente
044700090303            clear titaa0;
044800090303       // chiave spedizione
044900090303            taaaas = tasaas;
045000090303            taalnp = taslnp;
045100090303            taanrs = tasnrs;
045200090303            taansp = tasnsp;
045300090303       // tipo record
045400090303            taatrc = 'M';
045500090303       // dati della filiale Sede 102
045600090303            chain wfil azorg01l;
045700090303            if not %found(azorg01l) or orgfva <> *blanks;
045800090303              clear orgind;
045900090303              clear orgcpf;
046000090303              clear orgloc;
046100090303              clear orgpro;
046200090303            endif;
046300090303
046400090303            taarsc = 'POD IMAGE';
046500090303            taaind = orgind;
046600090303            taacap = %editc(orgcpf:'X');
046700090303            taaloc = orgloc;
046800090303            taaprv = orgpro;
046900090303            write TITAA0;
047000090303
047100090303       // -- > T I T A 4 0 0 0
047200090303       // --------------------
047300090303
047400090303       // Rcd. 'A' - riferimenti
047500090317       // se richiesto
047600090317          if not $nota4a;
047700090304            clear tita4000;
047800090303            clear dta4a;
047900090303       // chiave spedizione
048000090303            ta4aas = tasaas;
048100090303            ta4lnp = taslnp;
048200090303            ta4nrs = tasnrs;
048300090303            ta4nsp = tasnsp;
048400090303       // tipo record
048500090303            ta4trc = 'A';
048600090303       // riferimento mittente alfabetico
048700090303            §ta4arma = %editc(f_tasrmn:'X');
048800090303            ta4not = dta4a;
048900090303            write TITA4000;
049000090317          endif;
049100090303
049200090303            write TITAS0;
049300090303
049400090220          ENDSR;
049500090217
049600090217       //--------------------------------------------------------------
049700090218       // Reperimento Dati del job (Utente/Operativi).
049800090217       //--------------------------------------------------------------
049900090217       BEGSR DatiJob;
050000090217
050100090217         in(E) §AzUte;
050200090217         if NOT %error;
050300090217           in(E) §DatiUte;
050400090217         endif;
050500090217         if %error or RSut = *blanks;
050600090217           clear TIBS34ds;
050700090217           tibs34r(tibs34ds);
050800090217           in §AzUte;
050900090217           in §DatiUte;
051000090217         endif;
051100090318
051200090318           // - Reperimento divisa moneta di conto gestionale
051300090318           clear  dGED;
051400090318           clear  TIBS02ds;
051500090318           t02mod = 'C';
051600090318           T02cod = 'GED';
051700090318           T02ke1 = '1';
051800090318           TNTBE_RicercaControllo(kpjba : tibs02ds);
051900090318           if  T02err = *blank;
052000090318             dGED = T02uni;
052100090318           endif;
052200040511
052300090220       ENDSR;
