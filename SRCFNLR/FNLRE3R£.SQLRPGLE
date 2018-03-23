000100081117     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200081117      // ----------------------------------------------------------------------
000300081117      //
000400081117      //         STATISTICA GIORNALIERA ANOMALIE  ?
000500081117      //          rielaborazione di un giorno
000600081117      //
000700081117      // ----------------------------------------------------------------------
000800081117      // ? DICHIARAZIONE DEI FILE ?
000900081117      // ----------------------------------------------------------------------
001000081119     ftabel00f  if   e           k disk
001100081117     ffnlre3d   cf   e             workstn
001200081117
001300081117      // ----------------------------------------------------------------------
001400081117      // ? RIEPILOGO INDICATORI ?
001500081117
001600081117      // ? V A R I A B I L I ?
001700090415     d i61rep          s              1    inz('S')
001800090415     d o61drf          s              8  0
001900081119     d wdata           s              8  0
002000081119     d wdatamin        s              8  0
002100081117     d $fine           s               n   inz(*off)
002200081117
002300081117      // ? S C H I E R E ?
002400081117     d msg             s             78    dim(15) ctdata perrcd(1)
002500081117
002600081117      // ? D S   I N T E R N E / E S T E R N E ?
002700081117     d wlbdat          ds                  inz
002800081117     d  g02dat                 1      8  0
002900081117     d  g02inv                 9     16  0
003000081117     d  g02err                17     17
003100081117     d  g02tgi                18     22  0
003200081117
003300081119     d wgidat          ds                  inz
003400081119     d  giodat                 1      8  0
003500081119     d  gioinv                 9     16  0
003600081119     d  giotgi                17     21  0
003700081119
003800081119     d ds5a2         e ds
003900081117     d kpjba         e ds
004000081117
004100081117      // ? COSTANTI ?
004200081117
004300081119      // ? PROCEDURE USATE NON NEI PROTOTIPI ?
004400081119     d fnlv61r         pr                  extpgm('FNLV61R')
004500081119     d  o61drf                        8  0
004600090415     d  i61rep                        1
004700081119
004800081119     d xsrgi8          pr                  extpgm('XSRGI8')
004900081119     d  wgidat                             likeds(wgidat)
005000081119
005100081117      // ? PROTOTIPI ?
005200081117      /copy gaitrasrc/srcprotopr,fnlre1r
005300081117      /copy gaitrasrc/srcprotopr,xsrda8
005400081117
005500081117     c     *entry        plist
005600081117     c                   parm                    kpjba
005700081117
005800081117      /free
005900081117
006000081119       //?operazioni iniziali
006100081119         exsr routinz;
006200081117
006300081117         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006400081117
006500081117         dow not $fine;
006600081117       //?gestione prima videata
006700081117           exsr gesvid;
006800081117         enddo;
006900081117
007000081117         *inlr = *on;
007100081119
007200081119       // ----------------------------------------------------------------------
007300081119       //?Operazioni iniziali.
007400081119       // ----------------------------------------------------------------------
007500081119       begsr routinz;
007600081119
007700081119         $fine = *off;
007800081119
007900081119       //?cerco i gg di pulizia statistica anomalie
008000081119         clear ds5a2;
008100081119         tblkut = 1;
008200081119         tblcod = '5A';
008300081119         tblkey = '2';
008400081119         chain (tblkut:tblcod:tblkey) tabel00f;
008500081119         if %found(tabel00f);
008600081119          ds5a2 = tbluni;
008700081119         endif;
008800081119
008900081119       //?data di riferimento per le pulizie
009000090415         fnlv61r(o61drf: i61rep);
009100081119
009200081119       //?calcolo la data di pulizia statistica anomalie
009300081119         clear wlbdat;
009400081119         g02inv = o61drf;
009500081119         g02err = '3';
009600081119         xsrda8(wlbdat);
009700081119
009800081119         giotgi = g02tgi - §5asga;
009900081119         xsrgi8(wgidat);
010000081119         wdatamin = gioinv;
010100081119
010200081119       endsr;
010300081117
010400081117       // ----------------------------------------------------------------------
010500081117       //?Gestione videata
010600081117       // ----------------------------------------------------------------------
010700081117       begsr gesvid;
010800081117
010900081117       //?emetto la videata
011000081117          exfmt lre3w01;
011100081117
011200081117       //?spengo indicatori di comodo
011300081117          *in28 = *off;
011400081117       //?pulisco campo messaggi
011500081117          clear w1cmsg;
011600081117
011700081117       //?esecuzione comandi
011800081117         select;
011900081117
012000081117       //?f3=fine
012100081117          when *inkc;
012200081117           exsr sr_fine;
012300081117       //?f6=conferma
012400081117          when *inkf;
012500081117           exsr sr_elabora;
012600081117         other;
012700081117
012800081117       //?controllo la data
012900081117          exsr sr_contrdata;
013000081117          if *in28;
013100081117           leavesr;
013200081117          endif;
013300081117         endsl;
013400081117
013500081117       endsr;
013600081117
013700081117       // ----------------------------------------------------------------------
013800081117       //?Controllo data
013900081117       // ----------------------------------------------------------------------
014000081117       begsr sr_contrdata;
014100081117
014200081117       //?pulisco campi di comodo
014300081117         clear wdata;
014400081117
014500081117       //?è obbligatoria
014600081117         if w1cdata = *zeros;
014700081117          *in28 = *on;
014800081117          w1cmsg = msg(01);
014900081117          leavesr;
015000081117         endif;
015100081117
015200081117       //?deve essere una data valida
015300081117         clear wlbdat;
015400081117         g02dat = w1cdata;
015500081117         xsrda8(wlbdat);
015600081117         if g02err = '1';
015700081117          *in28 = *on;
015800081117          w1cmsg = msg(01);
015900081117          leavesr;
016000081117         endif;
016100081117
016200081119         w1cdata = g02dat;
016300081119         wdata = g02inv;
016400081119
016500081119       //?non deve essere = a oggi
016600081119         if wdata = *date;
016700081119          *in28 = *on;
016800081119          w1cmsg = msg(02);
016900081119          leavesr;
017000081119         endif;
017100081119
017200081119       //?non inferiore alla data limite pulizia
017300081119         if wdata < wdatamin;
017400081119          *in28 = *on;
017500081119          w1cmsg = msg(03);
017600081119          w1cmsg = %trim(w1cmsg) + ' ' +
017700081119                   %subst(%editc(%dec(wdatamin):'X'):7:2) +
017800081119                   '/' +
017900081119                   %subst(%editc(%dec(wdatamin):'X'):5:2) +
018000081119                   '/' +
018100081119                   %subst(%editc(%dec(wdatamin):'X'):1:4);
018200081119          leavesr;
018300081119         endif;
018400081117
018500081117       endsr;
018600081117
018700081117       // ----------------------------------------------------------------------
018800081117       //?Elaborazione
018900081117       // ----------------------------------------------------------------------
019000081117       begsr sr_elabora;
019100081117
019200081117       //?per prima cosa devo cancellare la data richiesta
019300081117         exec sql delete from wfsga00f where sgadata = :wdata;
019400081117
019500081117       //?poi richiamo il pgm di elaborazione
019600081119         kpjbu = %char(wdata);
019700081117         fnlre1r(kpjba);
019800081117
019900081117       //?finito di elaborare esco dal pgm
020000081117           exsr sr_fine;
020100081117
020200081117       endsr;
020300081117
020400081117       // ----------------------------------------------------------------------
020500081117       //?Fine programma
020600081117       // ----------------------------------------------------------------------
020700081117       begsr sr_fine;
020800081117
020900081117         *inlr = *on;
021000081117         return;
021100081117
021200081117       endsr;
021300081117
021400081117** MSG  Lungh. 30             *
021500081117Data errata                     01
021600081119Non si può elaborare oggi       02
021700081119Data minore del                 03
