000100081117     h decedit('0,') datedit(*ymd.) option(*nodebugio)
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
002100090223     d conta           s              5i 0 inz
002200090223     d stringasql      s            500    Varying
002300090223     d wdataa          s              8  0 inz
002400090223     d wdatad          s              8  0 inz
002500090223     d xx              s              3  0 inz
002600081117     d $fine           s               n   inz(*off)
002700090223
002800090223      // ? D S   I N T E R N E / E S T E R N E ?
002900090223     d §azute        e ds                  extname(azute00f)
003000090223     d                                     dtaara
003100090223     d §datiute      e ds                  extname(ddatiute)
003200090223     d                                     dtaara
003300090223     d ds3e          e ds
003400090223     d fnbrvds       e ds                  extname(fnbrv00f)
003500081117     d kpjba         e ds
003600090223     d tibs34ds      e ds                  inz
003700081117
003800081117      // ? COSTANTI ?
003900081117
004000081119      // ? PROCEDURE USATE NON NEI PROTOTIPI ?
004100081119
004200081117      // ? PROTOTIPI ?
004300090223      /copy gaitrasrc/srcprotopr,tibs34r
004400081117
004500081117     c     *entry        plist
004600081117     c                   parm                    kpjba
004700081117
004800081117      /free
004900081117
005000081119       //?operazioni iniziali
005100081119         exsr routinz;
005200090224
005300081117         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
005400090223
005500090223       //?preparo la stringa SQL
005600090223         exsr stringa;
005700090223
005800090223       //?preparo il cursore per SQL sulle spunte
005900090223         exec sql prepare s from :stringasql;
006000090223         exec sql declare spunte cursor for s;
006100090223
006200090223       //?apro il cursore
006300090223         exec sql open spunte;
006400090223         if sqlcode < 0;
006500090223          *inlr = *on;
006600090223          return;
006700090223         endif;
006800090224
006900090224         clear conta;
007000081117
007100090223       //?leggo sql estratto per le spunte interessate
007200090223         dow not $fine;
007300090224          exec sql fetch next from spunte into :brvlnp, :brvlna, :brvnrs,
007400090224                                               :brvnsc;
007500090223
007600090223       //?fine file o errore sql esco
007700090223          if sqlcod = 100 or sqlcod < 0;
007800090223           $fine = *on;
007900090223           iter;
008000090223          endif;
008100090224
008200090224       //?leggo le spunte
008300090224          exsr leggi;
008400090224         enddo;
008500090223
008600090223       //?stampo il totale
008700090223          exsr stampa;
008800081117
008900081117         *inlr = *on;
009000081119
009100081119       // ----------------------------------------------------------------------
009200081119       //?Operazioni iniziali.
009300081119       // ----------------------------------------------------------------------
009400081119       begsr routinz;
009500090223
009600090223       //?recupero dei dati utente
009700090223          in(e) §azute;
009800090223          if not %error;
009900090223           in(e) §datiute;
010000090223          endif;
010100090223          if %error or rsut = *blanks;
010200090223           tibs34r(tibs34ds);
010300090223           in §azute;
010400090223           in §datiute;
010500090223          endif;
010600090223
010700090223       //?carico sk anomalie escluse le anomalie di rientro
010800090223         clear xx;
010900090223         tblkut = 1;
011000090223         tblcod = '3E';
011100090223         setll (tblkut:tblcod) tabel00f;
011200090223         dow not %eof(tabel00f);
011300090223          reade (tblkut:tblcod) tabel00f;
011400090223          ds3e = tbluni;
011500090223          if §3efta <> 'R';
011600090223           xx += 1;
011700090223           sk3e(xx) = tblkey;
011800090223          endif;
011900090223         enddo;
012000090223
012100090223       //?date elaborazione
012200090223         wdatad = %dec(%subst(kpjbu:1:8):8:0);
012300090223         wdataa = %dec(%subst(kpjbu:9:8):8:0);
012400090223
012500090223       //?imposto il range di date in stampa
012600090223         datad = %subst(kpjbu:7:2) + '/' +
012700090223                 %subst(kpjbu:5:2) + '/' +
012800090223                 %subst(kpjbu:1:4);
012900090223         dataa = %subst(kpjbu:15:2) + '/' +
013000090223                 %subst(kpjbu:13:2) + '/' +
013100090223                 %subst(kpjbu:9:4);
013200090223
013300090223       //?stampo testata
013400090223         except testa;
013500081119
013600081119       endsr;
013700090223
013800090223       // ----------------------------------------------------------------------
013900090223       //?Preparo la stringa sql
014000090223       // ----------------------------------------------------------------------
014100090223       begsr stringa;
014200090223
014300090224          stringasql = 'select distinct brvlnp,'
014400090224                     + ' brvlna, brvnrs, brvnsc'
014500090224                     + ' from filtra201/fnbrv00f'
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
016300090223                     + ' group by brvlnp, brvlna, brvnrs, brvnsc';
016400090223
016500090223       endsr;
016600090224
016700090224       // ----------------------------------------------------------------------
016800090224       //?Leggo e conto i colli spuntati
016900090224       // ----------------------------------------------------------------------
017000090224       begsr leggi;
017100090224
017200090224       //?pulisco la sk delle filiali spunta
017300090224          conta = conta +1;
017400090224
017500090224       endsr;
017600090223
017700090223       // ----------------------------------------------------------------------
017800090223       //?Stampo il totale
017900090223       // ----------------------------------------------------------------------
018000090223       begsr stampa;
018100090223
018200090223         except riga;
018300090223
018400090223       endsr;
018500090223
018600090223      /end-free
018700090223
018800090223     osysprt    E            testa          2  2
018900090223     o                       rsut                21
019000090223     o                                         + 10 'Totale colli con anomalia '
019100090223     o                                         +  1 'dal: '
019200090223     o                       datad             +  1
019300090223     o                                         +  1 'al: '
019400090223     o                       dataa             +  1
019500090223     o                       udate         y   + 10
019600090223     o                                         +  3 'FNLRE8R1'
019700090223     o                                         +  3 'Pag.'
019800090223     o                       page          z   +  1
019900090223
020000090223     o          e            testa       1  1
020100090223     o                                         +  3 ' '
020200090223     o          e            riga        1  1
020300090223     o                                         +  3 'TOTALE:'
020400090223     o                       conta         z   +  2
