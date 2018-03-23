000100090224     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200081117      // ----------------------------------------------------------------------
000300081117      //
000400090223      //         TOTALE COLLI CON ANOMALIA  ?
000500081117      //
000600081117      // ----------------------------------------------------------------------
000700081117      // ? DICHIARAZIONE DEI FILE ?
000800081117      // ----------------------------------------------------------------------
000900090223     ftabel00f  if   e           k disk
001000090223     fsysprt    o    f  132        printer oflind(*inof)
001100081117
001200081117      // ----------------------------------------------------------------------
001300081117      // ? RIEPILOGO INDICATORI ?
001400090223
001500090223      // ? S C H I E R E ?
001600090223     d sk3e            s              1    dim(99)
001700081117
001800081117      // ? V A R I A B I L I ?
001900090223     d dataa           s             10    inz
002000090223     d datad           s             10    inz
002100090224     d conta           s             10  0 inz
002200090223     d stringasql      s            500    Varying
002300090223     d wdataa          s              8  0 inz
002400090223     d wdatad          s              8  0 inz
002500090223     d xx              s              3  0 inz
002600090223
002700090223      // ? D S   I N T E R N E / E S T E R N E ?
002800090223     d §azute        e ds                  extname(azute00f)
002900090223     d                                     dtaara
003000090223     d §datiute      e ds                  extname(ddatiute)
003100090223     d                                     dtaara
003200090223     d ds3e          e ds
003300081117     d kpjba         e ds
003400090223     d tibs34ds      e ds                  inz
003500081117
003600081117      // ? COSTANTI ?
003700081117
003800081119      // ? PROCEDURE USATE NON NEI PROTOTIPI ?
003900081119
004000081117      // ? PROTOTIPI ?
004100090223      /copy gaitrasrc/srcprotopr,tibs34r
004200081117
004300081117     c     *entry        plist
004400081117     c                   parm                    kpjba
004500081117
004600081117      /free
004700081117
004800081119       //?operazioni iniziali
004900081119         exsr routinz;
005000090224
005100081117         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005200090223
005300090223       //?preparo la stringa SQL
005400090223         exsr stringa;
005500090223
005600090223       //?preparo il cursore per SQL sulle spunte
005700090223         exec sql prepare s from :stringasql;
005800090223         exec sql declare spunte cursor for s;
005900090223
006000090223       //?apro il cursore
006100090223         exec sql open spunte;
006200090223         if sqlcode < 0;
006300090223          *inlr = *on;
006400090223          return;
006500090223         endif;
006600090224
006700090224         clear conta;
006800081117
006900090223       //?leggo sql estratto per le spunte interessate
007000090224          exec sql fetch next from spunte into :conta;
007100090223
007200090223       //?fine file o errore sql esco
007300090223          if sqlcod = 100 or sqlcod < 0;
007400090224           *inlr = *on;
007500090224           return;
007600090223          endif;
007700090223
007800090223       //?stampo il totale
007900090223          exsr stampa;
008000081117
008100081117         *inlr = *on;
008200081119
008300081119       // ----------------------------------------------------------------------
008400081119       //?Operazioni iniziali.
008500081119       // ----------------------------------------------------------------------
008600081119       begsr routinz;
008700090223
008800090223       //?recupero dei dati utente
008900090223          in(e) §azute;
009000090223          if not %error;
009100090223           in(e) §datiute;
009200090223          endif;
009300090223          if %error or rsut = *blanks;
009400090223           tibs34r(tibs34ds);
009500090223           in §azute;
009600090223           in §datiute;
009700090223          endif;
009800090223
009900090223       //?carico sk anomalie escluse le anomalie di rientro
010000090223         clear xx;
010100090223         tblkut = 1;
010200090223         tblcod = '3E';
010300090223         setll (tblkut:tblcod) tabel00f;
010400090223         dow not %eof(tabel00f);
010500090223          reade (tblkut:tblcod) tabel00f;
010600090223          ds3e = tbluni;
010700090223          if §3efta <> 'R';
010800090223           xx += 1;
010900090223           sk3e(xx) = tblkey;
011000090223          endif;
011100090223         enddo;
011200090223
011300090223       //?date elaborazione
011400090223         wdatad = %dec(%subst(kpjbu:1:8):8:0);
011500090223         wdataa = %dec(%subst(kpjbu:9:8):8:0);
011600090223
011700090223       //?imposto il range di date in stampa
011800090223         datad = %subst(kpjbu:7:2) + '/' +
011900090223                 %subst(kpjbu:5:2) + '/' +
012000090223                 %subst(kpjbu:1:4);
012100090223         dataa = %subst(kpjbu:15:2) + '/' +
012200090223                 %subst(kpjbu:13:2) + '/' +
012300090223                 %subst(kpjbu:9:4);
012400090223
012500090223       //?stampo testata
012600090223         except testa;
012700081119
012800081119       endsr;
012900090223
013000090223       // ----------------------------------------------------------------------
013100090223       //?Preparo la stringa sql
013200090223       // ----------------------------------------------------------------------
013300090223       begsr stringa;
013400090223
013500090224          stringasql = 'select count(*) from'
013600090224                     + ' (select distinct brvlnp,'
013700090224                     + ' brvlna, brvnrs, brvnsc';
013800090224          if %subst(knsif:7:1) = 'P';
013900090224            stringasql = %trim(stringasql)
014000090224                     + ' from filtraprd/fnbrv00f';
014100090224          else;
014200090224            stringasql = %trim(stringasql)
014300090224                     + ' from filtra201/fnbrv00f';
014400090224          endif;
014500090224            stringasql = %trim(stringasql)
014600090223                     + ' where'
014700090223                     + ' brvdcs between ' + %editc(wdatad:'X')
014800090223                     + ' and ' + %editc(wdataa:'X')
014900090223                     + ' and brvcan in (';
015000090223
015100090223       //?ciclo sulla schiera delle anomalie da elaborare
015200090223          for xx = 1 to 99;
015300090223            if sk3e(xx) <> *blanks;
015400090223              if xx > 1;
015500090223                stringasql = %trim(stringasql) + ',';
015600090223              endif;
015700090223              stringasql = %trim(stringasql) + ' ' + '''' + sk3e(xx) +
015800090224                           '''';
015900090224            endif;
016000090224           endfor;
016100090224
016200090224          stringasql = %trim(stringasql) + ')'
016300090224                     + ' group by brvlnp, brvlna, brvnrs, brvnsc)'
016400090224                     + ' as a';
016500090223
016600090223       endsr;
016700090223
016800090223       // ----------------------------------------------------------------------
016900090223       //?Stampo il totale
017000090223       // ----------------------------------------------------------------------
017100090223       begsr stampa;
017200090223
017300090223         except riga;
017400090223
017500090223       endsr;
017600090223
017700090223      /end-free
017800090224
017900090224     osysprt    e            testa          2  2
018000090223     o                       rsut                21
018100090223     o                                         + 10 'Totale colli con anomalia '
018200090223     o                       udate         y   + 10
018300090223     o                                         +  3 'FNLRE8R1'
018400090223     o                                         +  3 'Pag.'
018500090223     o                       page          z   +  1
018600090223
018700090223     o          e            testa       1  1
018800090223     o                                         +  3 ' '
018900090224     o          e            riga        1  1
019000090224     o                                           15 'Spunte caricate'
019100090224     o                                         +  1 'dal: '
019200090224     o                       datad             +  1
019300090224     o                                         +  1 'al: '
019400090224     o                       dataa             +  1
019500090224     o                                         +  5 'TOTALE:'
019600090224     o                       conta         2   +  2
