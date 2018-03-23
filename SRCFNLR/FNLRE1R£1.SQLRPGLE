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
001200081112     ftntbe01l  uf   e           k disk
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
002600081112     d $par            s               n
002700081117     d $richiamato     s               n
002800081112     d $ultima         s               n
002900070209
003000080725      // ? S C H I E R E ?
003100081110     d sk3e            s              1    dim(99)
003200070209
003300080725      // ? D S   I N T E R N E / E S T E R N E ?
003400081112     d oldcollo        ds
003500081112     d  oldbrvlnp                          like(brvlnp)
003600081112     d  oldbrvlna                          like(brvlna)
003700081112     d  oldbrvnrs                          like(brvnrs)
003800081112     d  oldbrvnsc                          like(brvnsc)
003900081112
004000081112     d collo           ds
004100081112     d  cbrvlnp                            like(brvlnp)
004200081112     d  cbrvlna                            like(brvlna)
004300081112     d  cbrvnrs                            like(brvnrs)
004400081112     d  cbrvnsc                            like(brvnsc)
004500081112
004600081110     d ds3e          e ds
004700081110     d dsdf          e ds
004800081111     d fnbrvds       e ds                  extname(fnbrv00f)
004900081111     d kpjba         e ds
005000081110     d tibs02ds      e ds
005100070209
005200080728      // ? COSTANTI ?
005300080801
005400080728      // ? PROTOTIPI ?
005500081110      /copy gaitrasrc/srcprotopr,tibs02r
005600070209
005700080725      // ----------------------------------------------------------------------
005800080725
005900080725     c     *entry        plist
006000080725     c                   parm                    kpjba
006100071217
006200080725      /free
006300081111
006400081111         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006500080725
006600080731       //?operazioni iniziali
006700080725         exsr routinz;
006800081111
006900081111       //?ciclo per ogni data da elaborare
007000081112         dow dataiso < dataal;
007100081112
007200081112       //?imposto la data da elaborare
007300081112          dataiso = dataiso + %days(1);
007400081112          wdata = %dec(dataiso);
007500081112
007600081112       //?elaboro SQL
007700081112          exsr elabora;
007800081114
007900081114       //?aggiorno la ultima elaborazione in tabella SDF
008000081117       //?se pgm non richiamato
008100081117          if not $richiamato;
008200081117           exsr tabsdf;
008300081117          endif;
008400081112
008500081111         enddo;
008600081110
008700080729         *inlr = *on;
008800080725
008900080731       // ----------------------------------------------------------------------
009000080731       //?Operazioni iniziali.
009100080731       // ----------------------------------------------------------------------
009200080725       begsr routinz;
009300080725
009400081110       //?carico sk anomalie escluse le anomalie di rientro
009500081110         clear xx;
009600081110         tblkut = 1;
009700081110         tblcod = '3E';
009800081110         setll (tblkut:tblcod) tabel00f;
009900081110         dow not %eof(tabel00f);
010000081110          reade (tblkut:tblcod) tabel00f;
010100081110          ds3e = tbluni;
010200081110          if §3efta <> 'R';
010300081110           xx += 1;
010400081110           sk3e(xx) = tblkey;
010500081110          endif;
010600081110         enddo;
010700081117
010800081117       //?se passata la data dalla kpjba devo elaborare solo quella
010900081117         if %subst(kpjbu:1:8) > *zeros;
011000081117          wdata = %dec(%subst(kpjbu:1:8):8:0);
011100081117          dataal = %date(wdata:*iso);
011200081117          dataiso = dataal;
011300081118          dataiso = dataiso - %days(1);
011400081117          $richiamato = *on;
011500081117          leavesr;
011600081117         endif;
011700080725
011800081117          $richiamato = *off;
011900081111       //?imposto la data del giorno e calcolo la data al (oggi - 1)
012000080725          wdata = %dec(%date());
012100081111          dataal = %date(wdata:*iso);
012200081113          dataal = dataal - %days(1);
012300081110
012400081111       //?cerco l'ultima data elaborazione
012500081110          clear tibs02ds;
012600081110          t02mod = 'C';
012700081110          t02sif = knsif;
012800081110          t02cod = 'SDF';
012900081111          t02ke1 = '046';
013000081111          TNTBE_RicercaControllo(kpjba:tibs02ds);
013100081110          dsdf = t02uni;
013200081112          dataiso = %date(§sdfsga:*iso);
013300080725
013400080725       endsr;
013500081112
013600081112       // ----------------------------------------------------------------------
013700081112       //?Elaborazione spunte
013800081112       // ----------------------------------------------------------------------
013900081112       begsr elabora;
014000081112
014100081112         $fine = *off;
014200081112
014300081112       //?preparo la stringa SQL
014400081112         exsr stringa;
014500081112
014600081112       //?preparo il cursore per SQL sulle spunte
014700081112         exec sql prepare s from :stringasql;
014800081112         exec sql declare spunte cursor for s;
014900081112
015000081112       //?apro il cursore
015100081112         exec sql open spunte;
015200081112         if sqlcode < 0;
015300081112          leavesr;
015400081112         endif;
015500081112
015600081112       //?leggo sql estratto per le spunte interessate
015700081112         dow not $fine;
015800081112          exec sql fetch next from spunte into: fnbrvds;
015900081112
016000081112       //?fine file o errore sql esco
016100081112          if sqlcod = 100 or sqlcod < 0;
016200081112           $fine = *on;
016300081209           iter;
016400081112          endif;
016500081112
016600081112       //?leggo le spunte
016700081112          exsr leggi;
016800081112         enddo;
016900081112
017000081112       //?chiudo il cursore
017100081112         exec sql close spunte;
017200081112
017300081112       endsr;
017400080725
017500081111       // ----------------------------------------------------------------------
017600081111       //?Preparo la stringa sql
017700081111       // ----------------------------------------------------------------------
017800081111       begsr stringa;
017900081111
018000081114          stringasql = 'select * from fnbrv00f as a'
018100081114                     + ' where'
018200081114                     + ' a.brvdcs = ' + %editc(wdata:'X')
018300081114                     + ' and a.brvcan in (';
018400081114
018500081114       //?ciclo sulla schiera delle anomalie da elaborare
018600081114          for xx = 1 to 99;
018700081114            if sk3e(xx) <> *blanks;
018800081114              if xx > 1;
018900081114                stringasql = %trim(stringasql) + ',';
019000081114              endif;
019100081114              stringasql = %trim(stringasql) + ' ' + '''' + sk3e(xx) +
019200081114                           '''';
019300081114            endif;
019400081114           endfor;
019500081114
019600081114          stringasql = %trim(stringasql) + ')'
019700081114                    + ' and a.brvhcs = ('
019800081114                    + ' select max (brvhcs) from'
019900081114                    + ' fnbrv00f as b where'
020000081114                    + ' a.brvlnp = b.brvlnp and'
020100081114                    + ' a.brvlna = b.brvlna and'
020200081114                    + ' a.brvnrs = b.brvnrs and'
020300081114                    + ' a.brvnsc = b.brvnsc and'
020400081114                    + ' a.brvpes = b.brvpes and'
020500081114                    + ' a.brvdcs = b.brvdcs)'
020600081114                    + ' order by a.brvlnp,'
020700081114                    + ' a.brvlna, a.brvnrs, a.brvnsc,'
020800081114                    + ' a.brvhcs desc,'
020900081114                    + ' a.brvpes';
021000081111
021100081111       endsr;
021200081112
021300081112       // ----------------------------------------------------------------------
021400081112       //?Leggo le spunte
021500081112       // ----------------------------------------------------------------------
021600081112       begsr leggi;
021700081112
021800081112         cbrvlnp = brvlnp;
021900081112         cbrvlna = brvlna;
022000081112         cbrvnrs = brvnrs;
022100081112         cbrvnsc = brvnsc;
022200081112
022300081112       //?a rottura di collo
022400081112         if collo <> oldcollo;
022500081112
022600081112       //?sto leggendo il file in ordine di ORA più alta
022700081112       //?quindi a rottura di collo è per forza l'ultima spunta assoluta
022800081112          $ultima = *on;
022900081112       //?aggiorno il n. di spunte fatte per giorno/filiale/anomalia
023000081112          exsr conta;
023100081112
023200081112       //?memorizzo il collo che ho appena letto
023300081112          oldbrvlnp = brvlnp;
023400081112          oldbrvlna = brvlna;
023500081112          oldbrvnrs = brvnrs;
023600081112          oldbrvnsc = brvnsc;
023700081112       //?memorizzo la filiale che ho appena letto
023800081114          oldbrvpes = brvpes;
023900081112
024000081112         else;
024100081112
024200081112       //?a parità di collo ma a rottura di filiale
024300081114          if brvpes <> oldbrvpes;
024400081112       //?non è l'ultima spunta assoluta
024500081112           $ultima = *off;
024600081112       //?aggiorno il n. di spunte fatte per giorno/filiale/anomalia
024700081112           exsr conta;
024800081112       //?memorizzo la filiale che ho appena letto
024900081114           oldbrvpes = brvpes;
025000081112          endif;
025100081112
025200081112         endif;
025300081112
025400081112       endsr;
025500081112
025600081112       // ----------------------------------------------------------------------
025700081112       //?Conto e memorizzo le anomalie
025800081112       // ----------------------------------------------------------------------
025900081112       begsr conta;
026000081112
026100081112       //?prima controllo se foglio arrivi o partenze
026200081112         exsr foglio;
026300081112
026400081114         chain (wdata:brvpes:brvcan) wfsga01l;
026500081112
026600081112       //?aggiorno
026700081112          if %found(wfsga01l);
026800081112       //?partenza
026900081112           if $par;
027000081112            sgapar += 1;
027100081112           endif;
027200081112       //?arrivo
027300081112           if $arr;
027400081112            sgaarr += 1;
027500081112           endif;
027600081112       //?ultima spunta assoluta
027700081112           if $ultima;
027800081112            sgausa += 1;
027900081112           endif;
028000081112           update wfsga000;
028100081112          endif;
028200081112
028300081112       //?scrivo
028400081112          if not %found(wfsga01l);
028500081112           clear wfsga000;
028600081112           sgadata = wdata;
028700081114           sgapes = brvpes;
028800081112           sgacan = brvcan;
028900081112       //?partenza
029000081112           if $par;
029100081112            sgapar = 1;
029200081112           endif;
029300081112       //?arrivo
029400081112           if $arr;
029500081112            sgaarr = 1;
029600081112           endif;
029700081112       //?ultima spunta assoluta
029800081112           if $ultima;
029900081112            sgausa = 1;
030000081112           endif;
030100081112           write wfsga000;
030200081112          endif;
030300081112
030400081112       endsr;
030500081112
030600081112       // ----------------------------------------------------------------------
030700081112       //?Controllo se foglio arrivi o partenze
030800081112       // ----------------------------------------------------------------------
030900081112       begsr foglio;
031000081112
031100081112         $arr = *off;
031200081112         $par = *off;
031300081112
031400081112         select;
031500081112       //?foglio inventario
031600081112         when brvnpg = 3;
031700081112          chain (brvnpg:brvnfv:brvfgs) fnfvv01l;
031800081112          if %found(fnfvv01l);
031900081112           $par = (fvvspg = 'P');
032000081112           $arr = (fvvspg <> 'P');
032100081112          endif;
032200081112       //?foglio partenze
032300081113         when brvnpg = 1 or brvnpg = 5;
032400081112          $par = *on;
032500081112         other;
032600081112       //?foglio arrivi
032700081112          $arr = *on;
032800081112         endsl;
032900081112
033000081112       endsr;
033100081112
033200081112       // ----------------------------------------------------------------------
033300081112       //?Aggiorno la data ultima elaborazione in tabella SDF
033400081112       // ----------------------------------------------------------------------
033500081112       begsr tabsdf;
033600081112
033700081112         tbecod = 'SDF';
033800081112         tbeke1 = '046';
033900081114         chain (tbecod:tbeke1) tntbe01l;
034000081112         if %found(tntbe01l);
034100081112          dsdf = tbeuni;
034200081112          eval §sdfsga = wdata;
034300081112          tbeuni = dsdf;
034400081112          update tntbe000;
034500081112         endif;
034600081112
034700081112       endsr;
