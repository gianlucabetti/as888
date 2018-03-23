000100081117     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200081117      // ----------------------------------------------------------------------
000300081117      //
000400090223      //         TOTALE COLLI CON ANOMALIA  ?
000500081117      //
000600081117      // ----------------------------------------------------------------------
000700081117      // ? DICHIARAZIONE DEI FILE ?
000800081117      // ----------------------------------------------------------------------
000900081119     ftabel00f  if   e           k disk
001000090223     ffnlre8d   cf   e             workstn
001100081117
001200081117      // ----------------------------------------------------------------------
001300081117      // ? RIEPILOGO INDICATORI ?
001400081117
001500081117      // ? V A R I A B I L I ?
001600090223     d wdataa          s              8  0
001700090223     d wdatad          s              8  0
001800081119     d wdatamin        s              8  0
001900081117     d $fine           s               n   inz(*off)
002000081117
002100081117      // ? S C H I E R E ?
002200081117     d msg             s             78    dim(15) ctdata perrcd(1)
002300081117
002400081117      // ? D S   I N T E R N E / E S T E R N E ?
002500081117     d wlbdat          ds                  inz
002600081117     d  g02dat                 1      8  0
002700081117     d  g02inv                 9     16  0
002800081117     d  g02err                17     17
002900081117     d  g02tgi                18     22  0
003000081117
003100081119     d wgidat          ds                  inz
003200081119     d  giodat                 1      8  0
003300081119     d  gioinv                 9     16  0
003400081119     d  giotgi                17     21  0
003500081119
003600090223     d ds5a          e ds
003700081117     d kpjba         e ds
003800081117
003900081117      // ? COSTANTI ?
004000090223
004100090223      // ? PROCEDURE USATE NON NEI PROTOTIPI ?
004200081119     d xsrgi8          pr                  extpgm('XSRGI8')
004300081119     d  wgidat                             likeds(wgidat)
004400090223
004500090223     d bch10           pr                  extpgm('BCH10')
004600090223     d  kpjba                              likeds(kpjba)
004700081119
004800081117      // ? PROTOTIPI ?
004900081117      /copy gaitrasrc/srcprotopr,xsrda8
005000081117
005100081117     c     *entry        plist
005200081117     c                   parm                    kpjba
005300081117
005400081117      /free
005500081117
005600081119       //?operazioni iniziali
005700081119         exsr routinz;
005800081117
005900081117         dow not $fine;
006000081117       //?gestione prima videata
006100081117           exsr gesvid;
006200081117         enddo;
006300081117
006400081117         *inlr = *on;
006500081119
006600081119       // ----------------------------------------------------------------------
006700081119       //?Operazioni iniziali.
006800081119       // ----------------------------------------------------------------------
006900081119       begsr routinz;
007000081119
007100081119         $fine = *off;
007200081119
007300081119       //?cerco i gg di pulizia statistica anomalie
007400090223         clear ds5a;
007500081119         tblkut = 1;
007600081119         tblcod = '5A';
007700090223         tblkey = '1';
007800081119         chain (tblkut:tblcod:tblkey) tabel00f;
007900081119         if %found(tabel00f);
008000090223          ds5a = tbluni;
008100081119         endif;
008200081119
008300090223       //?calcolo la data di pulizia spunte con anomalie
008400081119         clear wlbdat;
008500090223         g02inv = *date;
008600081119         g02err = '3';
008700081119         xsrda8(wlbdat);
008800081119
008900090223         giotgi = g02tgi - §5ablp;
009000081119         xsrgi8(wgidat);
009100081119         wdatamin = gioinv;
009200081119
009300081119       endsr;
009400081117
009500081117       // ----------------------------------------------------------------------
009600081117       //?Gestione videata
009700081117       // ----------------------------------------------------------------------
009800081117       begsr gesvid;
009900081117
010000081117       //?emetto la videata
010100090223          exfmt lre8w01;
010200081117
010300081117       //?spengo indicatori di comodo
010400081117          *in28 = *off;
010500081117       //?pulisco campo messaggi
010600081117          clear w1cmsg;
010700081117
010800081117       //?esecuzione comandi
010900081117         select;
011000081117
011100081117       //?f3=fine
011200081117          when *inkc;
011300081117           exsr sr_fine;
011400081117       //?f6=conferma
011500081117          when *inkf;
011600090223           exsr sr_contrdata;
011700090223           if *in28;
011800090223            leavesr;
011900090223           endif;
012000081117           exsr sr_elabora;
012100081117         other;
012200081117
012300081117       //?controllo la data
012400081117          exsr sr_contrdata;
012500081117          if *in28;
012600081117           leavesr;
012700081117          endif;
012800081117         endsl;
012900081117
013000081117       endsr;
013100081117
013200081117       // ----------------------------------------------------------------------
013300081117       //?Controllo data
013400081117       // ----------------------------------------------------------------------
013500081117       begsr sr_contrdata;
013600090223
013700090223       //?spengo indicatori di comodo
013800090223          *in40 = *off;
013900090223          *in41 = *off;
014000081117
014100081117       //?pulisco campi di comodo
014200090223         clear wdatad;
014300090223         clear wdataa;
014400081117
014500090223       //?DATA DAL
014600081117       //?è obbligatoria
014700090223         if w1cdatad = *zeros;
014800081117          *in28 = *on;
014900090223          *in40 = *on;
015000081117          w1cmsg = msg(01);
015100081117          leavesr;
015200081117         endif;
015300081117
015400081117       //?deve essere una data valida
015500081117         clear wlbdat;
015600090223         g02dat = w1cdatad;
015700081117         xsrda8(wlbdat);
015800081117         if g02err = '1';
015900081117          *in28 = *on;
016000090223          *in40 = *on;
016100081117          w1cmsg = msg(01);
016200081117          leavesr;
016300081117         endif;
016400081117
016500090224         w1cdatad = g02dat;
016600090223         wdatad = g02inv;
016700081119
016800081119       //?non deve essere = a oggi
016900090223         if wdatad = *date;
017000081119          *in28 = *on;
017100090223          *in40 = *on;
017200081119          w1cmsg = msg(02);
017300081119          leavesr;
017400081119         endif;
017500081119
017600081119       //?non inferiore alla data limite pulizia
017700090223         if wdatad < wdatamin;
017800081119          *in28 = *on;
017900090223          *in40 = *on;
018000081119          w1cmsg = msg(03);
018100081119          w1cmsg = %trim(w1cmsg) + ' ' +
018200081119                   %subst(%editc(%dec(wdatamin):'X'):7:2) +
018300081119                   '/' +
018400081119                   %subst(%editc(%dec(wdatamin):'X'):5:2) +
018500081119                   '/' +
018600081119                   %subst(%editc(%dec(wdatamin):'X'):1:4);
018700081119          leavesr;
018800081119         endif;
018900090223
019000090223       //?DATA AL
019100090223       //?è obbligatoria
019200090223         if w1cdataa = *zeros;
019300090223          *in28 = *on;
019400090223          *in41 = *on;
019500090223          w1cmsg = msg(01);
019600090223          leavesr;
019700090223         endif;
019800090223
019900090223       //?deve essere una data valida
020000090223         clear wlbdat;
020100090223         g02dat = w1cdataa;
020200090223         xsrda8(wlbdat);
020300090223         if g02err = '1';
020400090223          *in28 = *on;
020500090223          *in41 = *on;
020600090223          w1cmsg = msg(01);
020700090223          leavesr;
020800090223         endif;
020900090223
021000090223         w1cdataa = g02dat;
021100090223         wdataa = g02inv;
021200090223
021300090223       //?non deve essere = a oggi
021400090223         if wdataa = *date;
021500090223          *in28 = *on;
021600090223          *in41 = *on;
021700090223          w1cmsg = msg(02);
021800090223          leavesr;
021900090223         endif;
022000090223
022100090223       //?non deve essere < della data da
022200090223         if wdataa < wdatad;
022300090223          *in28 = *on;
022400090223          *in41 = *on;
022500090223          w1cmsg = msg(04);
022600090223          leavesr;
022700090223         endif;
022800081117
022900081117       endsr;
023000081117
023100081117       // ----------------------------------------------------------------------
023200081117       //?Elaborazione
023300081117       // ----------------------------------------------------------------------
023400081117       begsr sr_elabora;
023500081117
023600090223       //?richiamo il pgm di elaborazione
023700090223         kcoaz = 'RE81';
023800090223         %subst(kpjbu:1:8) = %char(wdatad);
023900090223         %subst(kpjbu:9:8) = %char(wdataa);
024000090223         bch10(kpjba);
024100081117
024200081117       //?finito di elaborare esco dal pgm
024300081117           exsr sr_fine;
024400081117
024500081117       endsr;
024600081117
024700081117       // ----------------------------------------------------------------------
024800081117       //?Fine programma
024900081117       // ----------------------------------------------------------------------
025000081117       begsr sr_fine;
025100081117
025200081117         *inlr = *on;
025300081117         return;
025400081117
025500081117       endsr;
025600081117
025700081117** MSG  Lungh. 30             *
025800081117Data errata                     01
025900081119Non si può elaborare oggi       02
026000081119Data minore del                 03
026100090223Range di date errato            04
