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
001700081119     d o61drf          s              8  0
001800081119     d wdata           s              8  0
001900081119     d wdatamin        s              8  0
002000081117     d $fine           s               n   inz(*off)
002100081117
002200081117      // ? S C H I E R E ?
002300081117     d msg             s             78    dim(15) ctdata perrcd(1)
002400081117
002500081117      // ? D S   I N T E R N E / E S T E R N E ?
002600081117     d wlbdat          ds                  inz
002700081117     d  g02dat                 1      8  0
002800081117     d  g02inv                 9     16  0
002900081117     d  g02err                17     17
003000081117     d  g02tgi                18     22  0
003100081117
003200081119     d wgidat          ds                  inz
003300081119     d  giodat                 1      8  0
003400081119     d  gioinv                 9     16  0
003500081119     d  giotgi                17     21  0
003600081119
003700081119     d ds5a2         e ds
003800081117     d kpjba         e ds
003900081117
004000081117      // ? COSTANTI ?
004100081117
004200081119      // ? PROCEDURE USATE NON NEI PROTOTIPI ?
004300081119     d fnlv61r         pr                  extpgm('FNLV61R')
004400081119     d  o61drf                        8  0
004500081119
004600081119     d xsrgi8          pr                  extpgm('XSRGI8')
004700081119     d  wgidat                             likeds(wgidat)
004800081119
004900081117      // ? PROTOTIPI ?
005000081117      /copy gaitrasrc/srcprotopr,fnlre1r
005100081117      /copy gaitrasrc/srcprotopr,xsrda8
005200081117
005300081117     c     *entry        plist
005400081117     c                   parm                    kpjba
005500081117
005600081117      /free
005700081117
005800081119       //?operazioni iniziali
005900081119         exsr routinz;
006000081117
006100081117         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
006200081117
006300081117         dow not $fine;
006400081117       //?gestione prima videata
006500081117           exsr gesvid;
006600081117         enddo;
006700081117
006800081117         *inlr = *on;
006900081119
007000081119       // ----------------------------------------------------------------------
007100081119       //?Operazioni iniziali.
007200081119       // ----------------------------------------------------------------------
007300081119       begsr routinz;
007400081119
007500081119         $fine = *off;
007600081119
007700081119       //?cerco i gg di pulizia statistica anomalie
007800081119         clear ds5a2;
007900081119         tblkut = 1;
008000081119         tblcod = '5A';
008100081119         tblkey = '2';
008200081119         chain (tblkut:tblcod:tblkey) tabel00f;
008300081119         if %found(tabel00f);
008400081119          ds5a2 = tbluni;
008500081119         endif;
008600081119
008700081119       //?data di riferimento per le pulizie
008800081119         fnlv61r(o61drf);
008900081119
009000081119       //?calcolo la data di pulizia statistica anomalie
009100081119         clear wlbdat;
009200081119         g02inv = o61drf;
009300081119         g02err = '3';
009400081119         xsrda8(wlbdat);
009500081119
009600081119         giotgi = g02tgi - §5asga;
009700081119         xsrgi8(wgidat);
009800081119         wdatamin = gioinv;
009900081119
010000081119       endsr;
010100081117
010200081117       // ----------------------------------------------------------------------
010300081117       //?Gestione videata
010400081117       // ----------------------------------------------------------------------
010500081117       begsr gesvid;
010600081117
010700081117       //?emetto la videata
010800081117          exfmt lre3w01;
010900081117
011000081117       //?spengo indicatori di comodo
011100081117          *in28 = *off;
011200081117       //?pulisco campo messaggi
011300081117          clear w1cmsg;
011400081117
011500081117       //?esecuzione comandi
011600081117         select;
011700081117
011800081117       //?f3=fine
011900081117          when *inkc;
012000081117           exsr sr_fine;
012100081117       //?f6=conferma
012200081117          when *inkf;
012300081117           exsr sr_elabora;
012400081117         other;
012500081117
012600081117       //?controllo la data
012700081117          exsr sr_contrdata;
012800081117          if *in28;
012900081117           leavesr;
013000081117          endif;
013100081117         endsl;
013200081117
013300081117       endsr;
013400081117
013500081117       // ----------------------------------------------------------------------
013600081117       //?Controllo data
013700081117       // ----------------------------------------------------------------------
013800081117       begsr sr_contrdata;
013900081117
014000081117       //?pulisco campi di comodo
014100081117         clear wdata;
014200081117
014300081117       //?è obbligatoria
014400081117         if w1cdata = *zeros;
014500081117          *in28 = *on;
014600081117          w1cmsg = msg(01);
014700081117          leavesr;
014800081117         endif;
014900081117
015000081117       //?deve essere una data valida
015100081117         clear wlbdat;
015200081117         g02dat = w1cdata;
015300081117         xsrda8(wlbdat);
015400081117         if g02err = '1';
015500081117          *in28 = *on;
015600081117          w1cmsg = msg(01);
015700081117          leavesr;
015800081117         endif;
015900081117
016000081119         w1cdata = g02dat;
016100081119         wdata = g02inv;
016200081119
016300081119       //?non deve essere = a oggi
016400081119         if wdata = *date;
016500081119          *in28 = *on;
016600081119          w1cmsg = msg(02);
016700081119          leavesr;
016800081119         endif;
016900081119
017000081119       //?non inferiore alla data limite pulizia
017100081119         if wdata < wdatamin;
017200081119          *in28 = *on;
017300081119          w1cmsg = msg(03);
017400081119          w1cmsg = %trim(w1cmsg) + ' ' +
017500081119                   %subst(%editc(%dec(wdatamin):'X'):7:2) +
017600081119                   '/' +
017700081119                   %subst(%editc(%dec(wdatamin):'X'):5:2) +
017800081119                   '/' +
017900081119                   %subst(%editc(%dec(wdatamin):'X'):1:4);
018000081119          leavesr;
018100081119         endif;
018200081117
018300081117       endsr;
018400081117
018500081117       // ----------------------------------------------------------------------
018600081117       //?Elaborazione
018700081117       // ----------------------------------------------------------------------
018800081117       begsr sr_elabora;
018900081117
019000081117       //?per prima cosa devo cancellare la data richiesta
019100081117         exec sql delete from wfsga00f where sgadata = :wdata;
019200081117
019300081117       //?poi richiamo il pgm di elaborazione
019400081119         kpjbu = %char(wdata);
019500081117         fnlre1r(kpjba);
019600081117
019700081117       //?finito di elaborare esco dal pgm
019800081117           exsr sr_fine;
019900081117
020000081117       endsr;
020100081117
020200081117       // ----------------------------------------------------------------------
020300081117       //?Fine programma
020400081117       // ----------------------------------------------------------------------
020500081117       begsr sr_fine;
020600081117
020700081117         *inlr = *on;
020800081117         return;
020900081117
021000081117       endsr;
021100081117
021200081117** MSG  Lungh. 30             *
021300081117Data errata                     01
021400081119Non si può elaborare oggi       02
021500081119Data minore del                 03
