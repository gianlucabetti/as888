000100040511     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200040511
000300090217      //--------------------------------------------------------------
000400090217      //
000500130130      // Creazione bolle per fatturare FUEL non fatturato
000600090217      //
000700090217      //--------------------------------------------------------------
000800040511
000900090217     ftitas30c  if   e           k Disk    prefix(f_)
001000090217     ftitaa30c  if   e           k Disk    prefix(f_)
001100090217     ftabel00f  if   e           k Disk
001200090217     fazorg01l  if   e           k Disk
001300090217     ftitas00f  o    e           k Disk    rename(Titas000:Titas0)
001400040511
001500090217      //--------------------------------------------------------------
001600090217      //  Variabili
001700090217      //--------------------------------------------------------------
001800090304     d codut           s                   like(tblkut) inz(1)
001900130130     d oldksc          s                   like(cft1ksc)
002000130130     d wdata           s              8  0
002100130130     d wdiffe          s                   like(cft1dfuel)
002200090220     d wora            s              6  0
002300090220     d $fine           s               n
002400040511
002500090217      //--------------------------------------------------------------
002600090217      // Definizione aree dati.
002700090217      //--------------------------------------------------------------
002800090217      // - Dati utente
002900090217     d §azute        e ds                  extname(azute00f)
003000090217     d                                     dtaara
003100090217     d §datiute      e ds                  extname(ddatiute)
003200090217     d                                     dtaara
003300090217
003400090217      //--------------------------------------------------------------
003500090217      // Definizione strutture dati.
003600090217      //--------------------------------------------------------------
003700090217
003800090217      // - Parametri ricevuti
003900090217     d kpjba         e ds
004000090217
004100090217      // - Reperimento dati utente
004200090217     d tibs34ds      e ds
004300090217
004400090217      // - Lettura anagrafiche cliente
004500090217     d tibs69ds      e ds                  inz
004600090317     d ds_CNACO      e ds                  extname(CNACO00F)
004700090317     d ds_CNIND      e ds                  extname(CNIND00F)
004800090317     d ds_CNCLP      e ds                  extname(CNCLP00F)
004900090317     d ds_FNCLS      e ds                  extname(FNCLS00F)
005000090318
005100090318       // - Parametri per Ricerca/controllo tabelle
005200090318     d TIBS02ds      e ds                  inz
005300090217
005400090217      // - Tabella 3a
005500090217     d ds3a          e ds
005600130130
005700090318       // - Tabella "GED" = Divise area gestionale
005800090318     d dGED          e ds                  inz
005900090217
006000090217     d fnlv55ds      e ds
006100090217     d trul34ds      e ds
006200090217     d  f_i34tla     e                     extfld(i34tla)
006300090217     d  f_o34err     e                     extfld(o34err)
006400090217     d  f_o34msg     e                     extfld(o34msg)
006500130130
006600130130      // - File WFCFT10F
006700130130     d wfcftds       e ds                  extname(WFCFT10F)
006800090217
006900090217      //--------------------------------------------------------------
007000090217      // Definizione procedure usate.
007100090217      //--------------------------------------------------------------
007200090220      // - Reperimento terminal di arrivo/partenza
007300090317      /copy gaitrasrc/srcprotopr,fnlv55r
007400090220
007500090217      // - Reperimento dati utente
007600090317      /copy gaitrasrc/srcprotopr,tibs34r
007700090217
007800090217      // - Lettura anagrafiche cliente
007900090317      /copy gaitrasrc/srcprotopr,tibs69r
008000090318
008100090318       // - Ricerca/Controllo tabelle
008200090318      /copy gaitrasrc/srcprotopr,tibs02r
008300090304
008400090304      // - Numero spedizione
008500090304     d trul34r         pr                  extpgm('TRUL34R')
008600090304     d  kpjba                              likeds(KPJBA)
008700090304     d  trul34ds                           likeds(TRUL34ds)
008800090217
008900090217      //--------------------------------------------------------------
009000090217     c     *Entry        plist
009100090217     c                   parm                    kpjba
009200090217
009300090217      /free
009400090217
009500090217       // Recupero dati del JOB
009600090220          exsr DatiJob;
009700130130
009800130130       //?Impostazione opzioni per SQL?
009900130130         exec sql   set  option  DynUsrPrf = *Owner,
010000130130                                 CloSqlCsr = *EndMod;
010100090220
010200090220          $fine = *off;
010300130130          wdata = 20130131;
010400130130          wora  = %dec(%time());
010500090304
010600130130          $fine = *off;
010700130130
010800130130       //?Leggo file WF confronto x cliente/tariffa
010900130130         exec sql
011000130130           DECLARE  wfcf cursor FOR
011100130130           SELECT   *    from WFCFT10F
011200130130           ORDER BY CFT1ksc, CFT1ctr;
011300130130
011400130130         exec sql
011500130130           open wfcf;
011600130130           IF sqlcode < 0;
011700130130             $fine = *on;
011800130130           ENDIF;
011900130130
012000130130         DOW not $fine;
012100130130           exec sql
012200130130             FETCH NEXT from wfcf into :WFCFTds;
012300130130           IF sqlcod = 100 or sqlcod < 0;
012400130130             $fine = *on;
012500130130             leave;
012600130130           ENDIF;
012700090316
012800130130         // elaboro a cambio cliente se ancora in negativo
012900130130           IF  CFT1ksc <> oldksc;
013000130130             IF  oldksc > 0 and wdiffe < 0;
013100130130               exsr elabora;
013200130130             ENDIF;
013300130130             clear wdiffe;
013400130130             oldksc = CFT1ksc;
013500130130           ENDIF;
013600130130
013700130130         // elaboro solo se differenza negativa
013800130130           IF  CFT1dfuel < 0;
013900130130             wdiffe += CFT1dfuel;
014000130130           ENDIF;
014100090316
014200090316          enddo;
014300090316
014400090316          *inlr = *on;
014500090220
014600090220       //------------------------------------------------------------------------*
014700090220       //  Elaboro la bolla letta
014800090220       //------------------------------------------------------------------------*
014900090220          begsr Elabora;
015000090220
015100090220       // -- > T I T A S 0 0 0
015200090303       // --------------------
015300090220            clear titas0;
015400090220
015500090220       // Anno
015600130130            tasaas = %dec(%subst(%editc(wdata:'X'):1:4):4:0);
015700090220       // Linea di partenza
015800130130            taslnp = %dec(%subst(%editc(oldksc:'X'):1:3):3:0);
015900130130            chain taslnp azorg01l;
016000130130            if not %found(azorg01l) or orgfva <> *blanks;
016100130130              clear orgcts;
016200130130            endif;
016300090220       // Codice bolla
016400130130            tascbo = 'B ';
016500090220       // Tipo bolla
016600090220            clear ds3a;
016700090220            tblkey = tascbo;
016800090220            chain (codut:'3A':tblkey) tabel00f;
016900090304            if not %found(tabel00f) or tblflg <> *blanks;
017000090220              leavesr;
017100090220            ENDIF;
017200090220            ds3a = tbluni;
017300090220            tastbl = §3atb1;
017400090220       // Mese/giorno
017500130130            tasmgs = %dec(%subst(%editc(wdata:'X'):5:4):4:0);
017600090220       // Cliente tassazione + codice tariffa
017700130130            tasksc = oldksc;
017800130130            tasctr = 0;
017900090421
018000090220       // se il cliente a cui addebitare il POD è della filiale 102 o 101 non genero la bolla
018100130130            if %subst(%editc(oldksc:'X'):1:3) = '102' or
018200130130               %subst(%editc(oldksc:'X'):1:3) = '101';
018300090220               leavesr;
018400090220            ENDIF;
018500090220       // Linea di arrivo
018600130130            taslna = TASlnp;
018700090220       // Numero Colli
018800130130            tasncl = 1;
018900090220       // Peso bollettato + flag
019000090220            taspkb = 1;
019100090220            tasfpb = 'R';
019200090220       // Peso fatturato + flag
019300090220            taspkf = 1;
019400090220            tasfpf = 'R';
019500090220       // Volume bollettato + flag
019600090304            tasvlb = 0,001;
019700090220            tasfvb = 'R';
019800090220       // Volume fatturato + flag
019900090220            tasvlf = 0,001;
020000090220            tasfvf = 'R';
020100090220       // Tipo servizio
020200090220            tastsp = 'C';
020300090220       // Destinatario
020400090220            clear tibs69ds;
020500090220            i69kac = tasksc;
020600090220            i69kin = tasksc;
020700090317            tibs69r(TIBS69ds : ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS);
020800090220            if o69err = '1';
020900090220              leavesr;
021000090220            ENDIF;
021100130130            tasrsd = 'Rec.10-16/12';
021200090220            tasind = indvia;
021300090220            tascad = indcae;
021400090220            taslod = indcit;
021500090220            tasprd = indprv;
021600090220            tasnzd = indsta;
021700090220       // Codice tassazione destinatario
021800130130            tascts = orgcts;
021900090220       // Flag inoltro
022000090220            tasfin = 'C';
022100090220       // Cliente mittente
022200130130            tasccm = oldksc;
022300090220       // Riferimento mittente numerico
022400130130            tasrmn = 1;
022500090220       // Codice tassazione mittente
022600130130            tasmct = orgcts;
022700090220       // Flag anteporto
022800090220            tasfap = 'C';
022900090220       // Natura merce
023000130130            tasnas = 'RECUPERO FUEL';
023100090220       // Filiale bollettazione
023200090220            tasflb = taslnp;
023300090220       // Terminal Arrivo
023400090220            clear fnlv55ds;
023500090220            d55tpt = 'A';
023600090220            d55lin = taslna;
023700090220            d55drf = *date;
023800090220            fnlv55r (fnlv55ds);
023900090220            if d55err <> *blanks;
024000090220              leavesr;
024100090220            ENDIF;
024200090220            tastfa = d55tfa;
024300090220       // Terminal partenza
024400090220            clear fnlv55ds;
024500090220            d55tpt = 'P';
024600090220            d55lin = taslnp;
024700090220            d55drf = *date;
024800090220            fnlv55r (fnlv55ds);
024900090220            if d55err <> *blanks;
025000090220              leavesr;
025100090220            ENDIF;
025200090220            tastfp = d55Tfp;
025300090220       // Data immissione
025400130130            tasdim = wdata;
025500090220       // Sigla operatore
025600090220            tassop = '*';
025700090220       // Codice autotrasportatore
025800090220            taspdr = taslnp * 10000 + 999;
025900090220       // Data/Ora ritiro
026000130130            tasdrt = wdata;
026100090318            tashrt = %dec(%subst(%editc(wora:'X'):1:4):4:0);
026200090220       // Flag prestazione autotrasportatore
026300090220            tasfpp = 'P';
026400090220       // Trattamento merce
026500090220            tasctm = §3actm;
026600090220       // Data Borderò
026700130130            tasdbr = wdata;
026800090220       // Flag merce portata a magazzino
026900090220            tasfdn = 'S';
027000090220       // Flag ddt si/no
027100090220            tasll1 = 'Y';
027200090220       // Nazione e cap mittente
027300090220            tasnzm = indsta;
027400090220            tascam = indcae;
027500130130       // Divisa + imponibile
027600130130            tasdiv = §gedcn;
027700130130            tasimv = %abs(wdiffe);
027800130130       // Varia 'f'
027900130130            tassv1 = 'f';
028000130130            tasva1 = %abs(wdiffe);
028100090220       // Falg bartolini/sdi
028200090304            Tasscl = 'B';
028300090303
028400090303       // Numero spedizione (Flnuf cod. 3)
028500090303            clear trul34ds;
028600090303            i34aas = tasaas;
028700090303            i34lnp = taslnp;
028800090304            trul34r(kpjba: trul34ds);
028900090303            if f_o34err <> *zeros;
029000090303              leavesr;
029100090303            ENDIF;
029200090303            tasnsp = o34nsp;
029300090303
029400090303            write TITAS0;
029500090303
029600090220          ENDSR;
029700090217
029800090217       //--------------------------------------------------------------
029900090218       // Reperimento Dati del job (Utente/Operativi).
030000090217       //--------------------------------------------------------------
030100090217       BEGSR DatiJob;
030200090217
030300090217         in(E) §AzUte;
030400090217         if NOT %error;
030500090217           in(E) §DatiUte;
030600090217         endif;
030700090217         if %error or RSut = *blanks;
030800090217           clear TIBS34ds;
030900090217           tibs34r(tibs34ds);
031000090217           in §AzUte;
031100090217           in §DatiUte;
031200090217         endif;
031300090318
031400090318           // - Reperimento divisa moneta di conto gestionale
031500090318           clear  dGED;
031600090318           clear  TIBS02ds;
031700090318           t02mod = 'C';
031800090318           T02cod = 'GED';
031900090318           T02ke1 = '1';
032000090318           TNTBE_RicercaControllo(kpjba : tibs02ds);
032100090318           if  T02err = *blank;
032200090318             dGED = T02uni;
032300090318           endif;
032400040511
032500090220       ENDSR;
