000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080731      // ----------------------------------------------------------------------
000300080725      //
000400081110      //         STATISTICA GIORNALIERA ANOMALIE  ?
000500080725      //
000600080731      // ----------------------------------------------------------------------
000700080725      // ? DICHIARAZIONE DEI FILE ?
000800080731      // ----------------------------------------------------------------------
000900070209
001000081112     ffnfvv01l  if   e           k disk
001100081110     ftabel00f  if   e           k disk
001200121005     ftntbe01l  uf   e           k disk
001300081112     fwfsga01l  uf a e           k disk
001400070209
001500080725      // ? RIEPILOGO INDICATORI ?
001600081110
001700080725      // ? V A R I A B I L I ?
001800081112     d dataiso         s               d   datfmt(*iso)
001900081111     d dataal          s               d   datfmt(*iso)
002000081114     d oldbrvpes       s                   like(brvpes)
002100081112     d stringasql      s            500    Varying
002200081110     d wdata           s              8  0 inz
002300081110     d xx              s              3  0 inz
002400081112     d $arr            s               n
002500081112     d $fine           s               n
002600090223     d $noconta        s               n
002700081112     d $par            s               n
002800081117     d $richiamato     s               n
002900081112     d $ultima         s               n
003000070209
003100080725      // ? S C H I E R E ?
003200081110     d sk3e            s              1    dim(99)
003300090223     d skfil           s              3  0 dim(90) inz
003400070209
003500080725      // ? D S   I N T E R N E / E S T E R N E ?
003600081112     d oldcollo        ds
003700081112     d  oldbrvlnp                          like(brvlnp)
003800081112     d  oldbrvlna                          like(brvlna)
003900081112     d  oldbrvnrs                          like(brvnrs)
004000081112     d  oldbrvnsc                          like(brvnsc)
004100081112
004200081112     d collo           ds
004300081112     d  cbrvlnp                            like(brvlnp)
004400081112     d  cbrvlna                            like(brvlna)
004500081112     d  cbrvnrs                            like(brvnrs)
004600081112     d  cbrvnsc                            like(brvnsc)
004700081112
004800081110     d ds3e          e ds
004900081110     d dsdf          e ds
005000081111     d fnbrvds       e ds                  extname(fnbrv00f)
005100081111     d kpjba         e ds
005200081110     d tibs02ds      e ds
005300070209
005400080728      // ? COSTANTI ?
005500080801
005600080728      // ? PROTOTIPI ?
005700081110      /copy gaitrasrc/srcprotopr,tibs02r
005800070209
005900080725      // ----------------------------------------------------------------------
006000080725
006100080725     c     *entry        plist
006200080725     c                   parm                    kpjba
006300071217
006400080725      /free
006500081111
006600081111         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006700080725
006800080731       //?operazioni iniziali
006900080725         exsr routinz;
007000081111
007100081111       //?ciclo per ogni data da elaborare
007200081112         dow dataiso < dataal;
007300081112
007400081112       //?imposto la data da elaborare
007500081112          dataiso = dataiso + %days(1);
007600081112          wdata = %dec(dataiso);
007700081112
007800081112       //?elaboro SQL
007900081112          exsr elabora;
008000081114
008100081114       //?aggiorno la ultima elaborazione in tabella SDF
008200121005       //?se pgm non richiamato
008300081117          if not $richiamato;
008400121005           exsr tabsdf;
008500081117          endif;
008600081112
008700081111         enddo;
008800081110
008900080729         *inlr = *on;
009000080725
009100080731       // ----------------------------------------------------------------------
009200080731       //?Operazioni iniziali.
009300080731       // ----------------------------------------------------------------------
009400080725       begsr routinz;
009500080725
009600081110       //?carico sk anomalie escluse le anomalie di rientro
009700081110         clear xx;
009800081110         tblkut = 1;
009900081110         tblcod = '3E';
010000081110         setll (tblkut:tblcod) tabel00f;
010100081110         dow not %eof(tabel00f);
010200081110          reade (tblkut:tblcod) tabel00f;
010300081110          ds3e = tbluni;
010400081110          if §3efta <> 'R';
010500081110           xx += 1;
010600081110           sk3e(xx) = tblkey;
010700081110          endif;
010800081110         enddo;
010900081117
011000081117       //?se passata la data dalla kpjba devo elaborare solo quella
011100081117         if %subst(kpjbu:1:8) > *zeros;
011200081117          wdata = %dec(%subst(kpjbu:1:8):8:0);
011300081117          dataal = %date(wdata:*iso);
011400081117          dataiso = dataal;
011500081118          dataiso = dataiso - %days(1);
011600081117          $richiamato = *on;
011700081117          leavesr;
011800081117         endif;
011900080725
012000081117          $richiamato = *off;
012100081111       //?imposto la data del giorno e calcolo la data al (oggi - 1)
012200080725          wdata = %dec(%date());
012300081111          dataal = %date(wdata:*iso);
012400081113          dataal = dataal - %days(1);
012500081110
012600081111       //?cerco l'ultima data elaborazione
012700081110          clear tibs02ds;
012800081110          t02mod = 'C';
012900081110          t02sif = knsif;
013000081110          t02cod = 'SDF';
013100081111          t02ke1 = '046';
013200081111          TNTBE_RicercaControllo(kpjba:tibs02ds);
013300081110          dsdf = t02uni;
013400081112          dataiso = %date(§sdfsga:*iso);
013500080725
013600080725       endsr;
013700081112
013800081112       // ----------------------------------------------------------------------
013900081112       //?Elaborazione spunte
014000081112       // ----------------------------------------------------------------------
014100081112       begsr elabora;
014200081112
014300081112         $fine = *off;
014400081112
014500081112       //?preparo la stringa SQL
014600081112         exsr stringa;
014700081112
014800081112       //?preparo il cursore per SQL sulle spunte
014900081112         exec sql prepare s from :stringasql;
015000081112         exec sql declare spunte cursor for s;
015100081112
015200081112       //?apro il cursore
015300081112         exec sql open spunte;
015400081112         if sqlcode < 0;
015500081112          leavesr;
015600081112         endif;
015700081112
015800081112       //?leggo sql estratto per le spunte interessate
015900081112         dow not $fine;
016000081112          exec sql fetch next from spunte into: fnbrvds;
016100081112
016200081112       //?fine file o errore sql esco
016300081112          if sqlcod = 100 or sqlcod < 0;
016400081112           $fine = *on;
016500081209           iter;
016600081112          endif;
016700081112
016800081112       //?leggo le spunte
016900081112          exsr leggi;
017000081112         enddo;
017100081112
017200081112       //?chiudo il cursore
017300081112         exec sql close spunte;
017400081112
017500081112       endsr;
017600080725
017700081111       // ----------------------------------------------------------------------
017800081111       //?Preparo la stringa sql
017900081111       // ----------------------------------------------------------------------
018000081111       begsr stringa;
018100081111
018200090219          stringasql = 'select * from fnbrv00f '
018300081114                     + ' where'
018400090219                     + ' brvdcs = ' + %editc(wdata:'X')
018500090219                     + ' and brvcan in (';
018600081114
018700081114       //?ciclo sulla schiera delle anomalie da elaborare
018800081114          for xx = 1 to 99;
018900081114            if sk3e(xx) <> *blanks;
019000081114              if xx > 1;
019100081114                stringasql = %trim(stringasql) + ',';
019200081114              endif;
019300081114              stringasql = %trim(stringasql) + ' ' + '''' + sk3e(xx) +
019400081114                           '''';
019500081114            endif;
019600081114           endfor;
019700081114
019800081114          stringasql = %trim(stringasql) + ')'
019900090219                    + ' order by brvlnp,'
020000090219                    + ' brvlna, brvnrs, brvnsc,'
020100090219                    + ' brvhcs desc,'
020200090219                    + ' brvpes';
020300081111
020400081111       endsr;
020500081112
020600081112       // ----------------------------------------------------------------------
020700081112       //?Leggo le spunte
020800081112       // ----------------------------------------------------------------------
020900081112       begsr leggi;
021000081112
021100081112         cbrvlnp = brvlnp;
021200081112         cbrvlna = brvlna;
021300081112         cbrvnrs = brvnrs;
021400081112         cbrvnsc = brvnsc;
021500081112
021600081112       //?a rottura di collo
021700081112         if collo <> oldcollo;
021800090223
021900090223       //?pulisco la sk delle filiali spunta
022000090223          clear skfil;
022100081112
022200081112       //?sto leggendo il file in ordine di ORA più alta
022300081112       //?quindi a rottura di collo è per forza l'ultima spunta assoluta
022400081112          $ultima = *on;
022500081112       //?aggiorno il n. di spunte fatte per giorno/filiale/anomalia
022600081112          exsr conta;
022700081112
022800081112       //?memorizzo il collo che ho appena letto
022900081112          oldbrvlnp = brvlnp;
023000081112          oldbrvlna = brvlna;
023100081112          oldbrvnrs = brvnrs;
023200081112          oldbrvnsc = brvnsc;
023300081112       //?memorizzo la filiale che ho appena letto
023400081114          oldbrvpes = brvpes;
023500081112
023600081112         else;
023700081112
023800081112       //?a parità di collo ma a rottura di filiale
023900081114          if brvpes <> oldbrvpes;
024000081112       //?non è l'ultima spunta assoluta
024100081112           $ultima = *off;
024200081112       //?aggiorno il n. di spunte fatte per giorno/filiale/anomalia
024300081112           exsr conta;
024400081112       //?memorizzo la filiale che ho appena letto
024500081114           oldbrvpes = brvpes;
024600081112          endif;
024700081112
024800081112         endif;
024900081112
025000081112       endsr;
025100081112
025200081112       // ----------------------------------------------------------------------
025300081112       //?Conto e memorizzo le anomalie
025400081112       // ----------------------------------------------------------------------
025500081112       begsr conta;
025600090223
025700090223       //?prima controllo se filiale già elaborata per il segnacollo
025800090223         $noconta = *off;
025900090223         exsr filiale;
026000090223         if $noconta = *on;
026100090223          leavesr;
026200090223         endif;
026300081112
026400081112       //?prima controllo se foglio arrivi o partenze
026500081112         exsr foglio;
026600081112
026700081114         chain (wdata:brvpes:brvcan) wfsga01l;
026800081112
026900081112       //?aggiorno
027000081112          if %found(wfsga01l);
027100081112       //?partenza
027200081112           if $par;
027300081112            sgapar += 1;
027400081112           endif;
027500081112       //?arrivo
027600081112           if $arr;
027700081112            sgaarr += 1;
027800081112           endif;
027900081112       //?ultima spunta assoluta
028000081112           if $ultima;
028100081112            sgausa += 1;
028200081112           endif;
028300081112           update wfsga000;
028400081112          endif;
028500081112
028600081112       //?scrivo
028700081112          if not %found(wfsga01l);
028800081112           clear wfsga000;
028900081112           sgadata = wdata;
029000081114           sgapes = brvpes;
029100081112           sgacan = brvcan;
029200081112       //?partenza
029300081112           if $par;
029400081112            sgapar = 1;
029500081112           endif;
029600081112       //?arrivo
029700081112           if $arr;
029800081112            sgaarr = 1;
029900081112           endif;
030000081112       //?ultima spunta assoluta
030100081112           if $ultima;
030200081112            sgausa = 1;
030300081112           endif;
030400081112           write wfsga000;
030500081112          endif;
030600081112
030700081112       endsr;
030800090223
030900090223       // ----------------------------------------------------------------------
031000090223       //?Controllo se filiale già elaborata per il segnacollo
031100090223       // ----------------------------------------------------------------------
031200090223       begsr filiale;
031300090223
031400090223         if %lookup(brvpes:skfil) > *zeros;
031500090223          $noconta = *on;
031600090223          leavesr;
031700090223         endif;
031800090223         xx = %lookup(*zeros:skfil);
031900090223         skfil(xx) = brvpes;
032000090223
032100090223       endsr;
032200081112
032300081112       // ----------------------------------------------------------------------
032400081112       //?Controllo se foglio arrivi o partenze
032500081112       // ----------------------------------------------------------------------
032600081112       begsr foglio;
032700081112
032800081112         $arr = *off;
032900081112         $par = *off;
033000081112
033100081112         select;
033200081112       //?foglio inventario
033300081112         when brvnpg = 3;
033400081112          chain (brvnpg:brvnfv:brvfgs) fnfvv01l;
033500081112          if %found(fnfvv01l);
033600081112           $par = (fvvspg = 'P');
033700081112           $arr = (fvvspg <> 'P');
033800081112          endif;
033900081112       //?foglio partenze
034000081113         when brvnpg = 1 or brvnpg = 5;
034100081112          $par = *on;
034200081112         other;
034300081112       //?foglio arrivi
034400081112          $arr = *on;
034500081112         endsl;
034600081112
034700081112       endsr;
034800081112
034900081112       // ----------------------------------------------------------------------
035000081112       //?Aggiorno la data ultima elaborazione in tabella SDF
035100081112       // ----------------------------------------------------------------------
035200121005         begsr tabsdf;
035300081112
035400121005         tbecod = 'SDF';
035500121005         tbeke1 = '046';
035600121005         chain (tbecod:tbeke1) tntbe01l;
035700121005         if %found(tntbe01l);
035800121005          dsdf = tbeuni;
035900121005          eval §sdfsga = wdata;
036000121005          tbeuni = dsdf;
036100121005          update tntbe000;
036200121005         endif;
036300081112
036400121005         endsr;
