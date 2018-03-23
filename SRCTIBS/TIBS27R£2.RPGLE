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
002400090303     d wfil            s                   like(orgfil) inz(102)
002500090217     d wnewora         s               t
002600090220     d wnumbolla       s             15
002700090217     d woldora         s               t
002800090220     d woggi           s              8  0
002900090220     d wora            s              6  0
003000090220     d $fine           s               n
003100040511
003200090217      //--------------------------------------------------------------
003300090217      // Definizione aree dati.
003400090217      //--------------------------------------------------------------
003500090217      // - Dati utente
003600090217     d §azute        e ds                  extname(azute00f)
003700090217     d                                     dtaara
003800090217     d §datiute      e ds                  extname(ddatiute)
003900090217     d                                     dtaara
004000090217
004100090217      //--------------------------------------------------------------
004200090217      // Definizione strutture dati.
004300090217      //--------------------------------------------------------------
004400090217
004500090217      // - Parametri ricevuti
004600090217     d kpjba         e ds
004700090217
004800090217      // - Reperimento dati utente
004900090217     d tibs34ds      e ds
005000090217
005100090217      // - Lettura anagrafiche cliente
005200090217     d tibs69ds      e ds                  inz
005300090217     d cnaco00f      e ds                  inz
005400090217     d cnind00f      e ds                  inz
005500090217     d cnclp00f      e ds                  inz
005600090217     d fncls00f      e ds                  inz
005700090217
005800090217      // - Tabella 3a
005900090217     d ds3a          e ds
006000090217
006100090217     d dta4a         e ds
006200090217     d fnlv55ds      e ds
006300090217     d trul34ds      e ds
006400090217     d  f_i34tla     e                     extfld(i34tla)
006500090217     d  f_o34err     e                     extfld(o34err)
006600090217     d  f_o34msg     e                     extfld(o34msg)
006700090217
006800090217     d oldbolla        ds
006900090217     d  oldaas                 1      4  0
007000090217     d  oldlnp                 5      7  0
007100090217     d  oldnrs                 8      9  0
007200090217     d  oldnsp                10     16  0
007300090217
007400090217     d newbolla        ds
007500090304     d  rigaas                 1      4  0
007600090304     d  riglnp                 5      7  0
007700090304     d  rignrs                 8      9  0
007800090304     d  rignsp                10     16  0
007900090217
008000090217     d oldrich         ds
008100090217     d  olddata                1      8  0
008200090217     d  oldora                 9     14  0
008300090217
008400090217     d newrich         ds
008500090217     d  rigdata                1      8  0
008600090217     d  rigora                 9     14  0
008700090217
008800090217      //--------------------------------------------------------------
008900090217      // Definizione procedure usate.
009000090217      //--------------------------------------------------------------
009100090220      // - Reperimento terminal di arrivo/partenza
009200090220     d fnlv55r         pr                  extpgm('FNLV55R')
009300090220     d  fnlv55ds                           likeds(FNLV55ds)
009400090220
009500090217      // - Reperimento dati utente
009600090217     d tibs34r         pr                  extpgm('TIBS34R')
009700090217     d  tibs34ds                           likeds(TIBS34ds)
009800090217
009900090217      // - Lettura anagrafiche cliente
010000090217     d tibs69r         pr                  extpgm('TIBS69R')
010100090217     d  tibs69ds                           likeds(TIBS69ds)
010200090217     d  cnaco00f                           likeds(CNACO00F)
010300090217     d  cnind00f                           likeds(CNIND00F)
010400090217     d  cnclp00f                           likeds(CNCLP00F)
010500090217     d  fncls00f                           likeds(FNCLS00F)
010600090304
010700090304      // - Numero spedizione
010800090304     d trul34r         pr                  extpgm('TRUL34R')
010900090304     d  kpjba                              likeds(KPJBA)
011000090304     d  trul34ds                           likeds(TRUL34ds)
011100090217
011200090217      //--------------------------------------------------------------
011300090217     c     *Entry        plist
011400090217     c                   parm                    kpjba
011500090217
011600090217      /free
011700090217
011800090217       // Recupero dati del JOB
011900090220          exsr DatiJob;
012000090220
012100090220          $fine = *off;
012200090220          woggi = %dec(%date());
012300090220          wora  = %dec(%time());
012400090304
012500090304          setll krig fnrig01l;
012600090217
012700090220       // Leggo fnrig
012800090220          dow not $fine;
012900090304            reade krig fnrig01l;
013000090220
013100090304            if %eof(fnrig01l);
013200090220              $fine = *on;
013300090220              leave;
013400090220            endif;
013500090217
013600090220       // se bolla diversa elaboro
013700090220            if newbolla <> oldbolla;
013800090218
013900090220       // ma solo se la bolla è presente su titas
014000090220              chain (rigaas:riglnp:rignrs:rignsp) titas30c;
014100090220              if not %found(titas30c);
014200090220                iter;
014300090220              endif;
014400090217
014500090220       // elaboro la spedizione letta
014600090220              exsr elabora;
014700090220       // mi salvo la spedizione letta
014800090220              oldbolla = newbolla;
014900090220              oldrich  = newrich;
015000090217
015100090220       // se stessa bolla
015200090220            else;
015300090220       // controllo se la richiesta è stata fatta lo stesso giorno e stessa hhmmss
015400090220              if oldrich = newrich;
015500090220       // in questo caso non elaboro il rcd di fnrig e imposto il campo rignoadd
015600090220                rignoadd = 'N';
015700090217
015800090220       // se la richiesta è stata fatta lo stesso giorno
015900090220              else;
016000090220       // ma in hhmmss diversi
016100090220                if olddata = rigdata;
016200090220       // controllo quanti minuti sono passati tra una richiesta e l'altra
016300090220                  woldora = %time(oldora);
016400090220                  wnewora = %time(rigora);
016500090220       // se non sono passati 30 minuti non genero l'addebito
016600090220                  if %diff(wnewora : woldora : *seconds) <= 1800;
016700090220                    rignoadd = 'N';
016800090220                  else;
016900090220       // elaboro la spedizione letta
017000090220                    exsr elabora;
017100090220       // mi salvo la spedizione letta
017200090220                    oldbolla = newbolla;
017300090220                    oldrich  = newrich;
017400090303                  endif;
017500090220       // se la richiesta è stata fatta in giorni diversi
017600090220                else;
017700090220       // elaboro la spedizione letta
017800090220                  exsr elabora;
017900090220       // mi salvo la spedizione letta
018000090220                  oldbolla = newbolla;
018100090220                  oldrich  = newrich;
018200090303                endif;
018300090303              endif;
018400090303            endif;
018500090218
018600090220       // aggiorno FNRIG sempre
018700090220            rigdela = *date;
018800090220            update fnrig000;
018900090217
019000090303          enddo;
019100090218
019200090220          *inlr = *on;
019300090220
019400090220
019500090220       //------------------------------------------------------------------------*
019600090220       //  Elaboro la bolla letta
019700090220       //------------------------------------------------------------------------*
019800090220          begsr Elabora;
019900090220
020000090220
020100090220       // -- > T I T A S 0 0 0
020200090303       // --------------------
020300090220            clear titas0;
020400090220
020500090220       // Anno
020600090220            tasaas = %dec(%subst(%editc(rigdata:'X'):1:4):4:0);
020700090220       // Linea di partenza
020800090220            taslnp = f_taslnp;
020900090220       // Codice bolla
021000090220            tascbo = 'FI';
021100090220       // Tipo bolla
021200090220            clear ds3a;
021300090220            tblkey = tascbo;
021400090220            chain (codut:'3A':tblkey) tabel00f;
021500090304            if not %found(tabel00f) or tblflg <> *blanks;
021600090220              leavesr;
021700090220            ENDIF;
021800090220            ds3a = tbluni;
021900090220            tastbl = §3atb1;
022000090220       // Mese/giorno
022100090220            tasmgs = %dec(%subst(%editc(rigdata:'X'):5:4):4:0);
022200090220       // Cliente tassazione + codice tariffa
022300090220            tasksc = f_tasksc;
022400090220            tasctr = f_tasctr;
022500090220       // se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
022600090220            if %subst(%editc(tasksc:'X'):1:3) = '102' or
022700090220               %subst(%editc(tasksc:'X'):1:3) = '101';
022800090220               rignoadd = 'X';
022900090220               leavesr;
023000090220            ENDIF;
023100090220       // Linea di arrivo
023200090220            taslna = f_taslna;
023300090220       // Numero Colli
023400090220            tasncl = 1;
023500090220       // Peso bollettato + flag
023600090220            taspkb = 1;
023700090220            tasfpb = 'R';
023800090220       // Peso fatturato + flag
023900090220            taspkf = 1;
024000090220            tasfpf = 'R';
024100090220       // Volume bollettato + flag
024200090304            tasvlb = 0,001;
024300090220            tasfvb = 'R';
024400090220       // Volume fatturato + flag
024500090220            tasvlf = 0,001;
024600090220            tasfvf = 'R';
024700090220       // Tipo servizio
024800090220            tastsp = 'C';
024900090220       // Destinatario
025000090220            clear tibs69ds;
025100090220            i69kac = tasksc;
025200090220            i69kin = tasksc;
025300090304            tibs69r(tibs69ds:cnaco00f:cnind00f:cnclp00f:fncls00f);
025400090220            if o69err = '1';
025500090220              leavesr;
025600090220            ENDIF;
025700090220            tasrsd = acorag;
025800090220            tasind = indvia;
025900090220            tascad = indcae;
026000090220            taslod = indcit;
026100090220            tasprd = indprv;
026200090220            tasnzd = indsta;
026300090220       // Codice tassazione destinatario
026400090220            tascts = '40';
026500090220       // Flag inoltro
026600090220            tasfin = 'C';
026700090220       // Cliente mittente
026800090220            tasccm = taslnp * 10000 + 8888;
026900090220       // Riferimento mittente numerico
027000090304            wnumbolla =  %subst(%editc(%dec(rigaas):'X'):2:2) +
027100090220                         %editc(%dec(riglnp):'X') +
027200090220                         %editc(%dec(rignrs):'X') + %editc(%dec(rignsp):'X');
027300090220            tasrmn = %dec(wnumbolla:15:0);
027400090220       // Codice tassazione mittente
027500090220            tasmct = '40';
027600090220       // Flag anteporto
027700090220            tasfap = 'C';
027800090220       // Natura merce
027900090220            tasnas = 'BOLLA FIRMATA';
028000090220       // Filiale bollettazione
028100090220            tasflb = taslnp;
028200090220       // Terminal Arrivo
028300090220            clear fnlv55ds;
028400090220            d55tpt = 'A';
028500090220            d55lin = taslna;
028600090220            d55drf = *date;
028700090220            fnlv55r (fnlv55ds);
028800090220            if d55err <> *blanks;
028900090220              leavesr;
029000090220            ENDIF;
029100090220            tastfa = d55tfa;
029200090220       // Terminal partenza
029300090220            clear fnlv55ds;
029400090220            d55tpt = 'P';
029500090220            d55lin = taslnp;
029600090220            d55drf = *date;
029700090220            fnlv55r (fnlv55ds);
029800090220            if d55err <> *blanks;
029900090220              leavesr;
030000090220            ENDIF;
030100090220            tastfp = d55Tfp;
030200090220       // Data immissione
030300090220            tasdim = woggi;
030400090220       // Sigla operatore
030500090220            tassop = '*';
030600090220       // Codice autotrasportatore
030700090220            taspdr = taslnp * 10000 + 999;
030800090220       // Data/Ora ritiro
030900090220            tasdrt = woggi;
031000090318            tashrt = %dec(%subst(%editc(wora:'X'):1:4):4:0);
031100090220       // Flag prestazione autotrasportatore
031200090220            tasfpp = 'P';
031300090220       // Trattamento merce
031400090220            tasctm = §3actm;
031500090220       // Data Borderò
031600090220            tasdbr = woggi;
031700090220       // Flag merce portata a magazzino
031800090220            tasfdn = 'S';
031900090220       // Flag ddt si/no
032000090220            tasll1 = 'Y';
032100090220       // Nazione e cap mittente
032200090220            tasnzm = indsta;
032300090220            tascam = indcae;
032400090220       // Varia se da impostare a zero
032500090220            if rigtas = 'N';
032600090220              tassv1 = §3asva;
032700090220            ENDIF;
032800090220       // Falg bartolini/sdi
032900090304            Tasscl = 'B';
033000090303
033100090303       // Numero spedizione (Flnuf cod. 3)
033200090303            clear trul34ds;
033300090303            i34aas = tasaas;
033400090303            i34lnp = taslnp;
033500090304            trul34r(kpjba: trul34ds);
033600090303            if f_o34err <> *zeros;
033700090303              leavesr;
033800090303            ENDIF;
033900090303            tasnsp = o34nsp;
034000090303
034100090303       // -- > T I T A A 0 0 0
034200090303       // --------------------
034300090303
034400090303       // Rcd. 'O' - mittente originale
034500090303            clear titaa0;
034600090303       // chiave spedizione
034700090303            taaaas = tasaas;
034800090303            taalnp = taslnp;
034900090303            taanrs = tasnrs;
035000090303            taansp = tasnsp;
035100090303       // tipo record
035200090303            taatrc = 'O';
035300090303       // ragione sociale destinatario
035400090303            taarsc = f_tasrsd;
035500090303            write TITAA0;
035600090303
035700090303       // Rcd. 'M' - mittente
035800090303            clear titaa0;
035900090303       // chiave spedizione
036000090303            taaaas = tasaas;
036100090303            taalnp = taslnp;
036200090303            taanrs = tasnrs;
036300090303            taansp = tasnsp;
036400090303       // tipo record
036500090303            taatrc = 'M';
036600090303       // dati della filiale Sede 102
036700090303            wfil = 102;
036800090303            chain wfil azorg01l;
036900090303            if not %found(azorg01l) or orgfva <> *blanks;
037000090303              clear orgind;
037100090303              clear orgcpf;
037200090303              clear orgloc;
037300090303              clear orgpro;
037400090303            endif;
037500090303
037600090303            taarsc = 'POD IMAGE';
037700090303            taaind = orgind;
037800090303            taacap = %editc(orgcpf:'X');
037900090303            taaloc = orgloc;
038000090303            taaprv = orgpro;
038100090303            write TITAA0;
038200090303
038300090303       // -- > T I T A 4 0 0 0
038400090303       // --------------------
038500090303
038600090303       // Rcd. 'A' - riferimenti
038700090304            clear tita4000;
038800090303            clear dta4a;
038900090303       // chiave spedizione
039000090303            ta4aas = tasaas;
039100090303            ta4lnp = taslnp;
039200090303            ta4nrs = tasnrs;
039300090303            ta4nsp = tasnsp;
039400090303       // tipo record
039500090303            ta4trc = 'A';
039600090303       // riferimento mittente alfabetico
039700090303            §ta4arma = %editc(f_tasrmn:'X');
039800090303            ta4not = dta4a;
039900090303            write TITA4000;
040000090303
040100090303            write TITAS0;
040200090303
040300090220          ENDSR;
040400090217
040500090217       //--------------------------------------------------------------
040600090218       // Reperimento Dati del job (Utente/Operativi).
040700090217       //--------------------------------------------------------------
040800090217       BEGSR DatiJob;
040900090217
041000090217         in(E) §AzUte;
041100090217         if NOT %error;
041200090217           in(E) §DatiUte;
041300090217         endif;
041400090217         if %error or RSut = *blanks;
041500090217           clear TIBS34ds;
041600090217           tibs34r(tibs34ds);
041700090217           in §AzUte;
041800090217           in §DatiUte;
041900090217         endif;
042000040511
042100090220       ENDSR;
