000100080930     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080930
000300080930      // ----------------------------------------------------------------------
000400080930      //
000500080930      //         Creazione file testi per new fil. 1 livello  ?
000600080930      //
000700080930      // ----------------------------------------------------------------------
000800080930      // ? DICHIARAZIONE DEI FILE ?
000900080930      // ----------------------------------------------------------------------
001000080930
001100080930     fazorg01l  if   e           k disk
001200080930     ffntxt00f  o    e             disk    rename(fntxt000:fntxt01)
001300080930     f                                     prefix(new)
001400080930     ftibsc2d   cf   e             workstn
001500080930
001600080930      // ? V A R I A B I L I ?
001700080930     d conta           s              5i 0
001800080930     d $fine           s               n
001900080930     d $finetxt        s               n
002000080930
002100080930      // ? S C H I E R E ?
002200080930     d msg             s             78    dim(15) ctdata perrcd(1)
002300080930
002400080930      // ? D S   I N T E R N E / E S T E R N E ?
002500080930     d §azute        e ds                  extname(azute00f)
002600080930     d                                     dtaara
002700080930     d §datiute      e ds                  extname(ddatiute)
002800080930     d                                     dtaara
002900080930
003000080930     d fnlv50ds      e ds
003100080930     d fnlv55ds      e ds
003200080930     d fntxtds       e ds                  extname(fntxt00f)
003300080930     d fntxtnewds    e ds                  extname(fntxt00f)
003400080930     d                                     prefix(new)
003500080930     d kpjba         e ds
003600080930     d tibs34ds      e ds                  inz
003700080930
003800080930      // ? PROTOTIPI ?
003900080930      /copy gaitrasrc/srcprotopr,fnlv50r
004000080930      /copy gaitrasrc/srcprotopr,fnlv55r
004100080930      /copy gaitrasrc/srcprotopr,tibs34r
004200080930
004300080930      // ----------------------------------------------------------------------
004400080930
004500080930     c     *entry        plist
004600080930     c                   parm                    kpjba
004700080930
004800080930      /free
004900080930
005000080930         exec sql
005100080930          set option dynusrprf = *owner, closqlcsr = *endmod;
005200080930
005300080930       //?operazioni iniziali
005400080930         exsr routinz;
005500080930
005600080930         dow not $fine;
005700080930          exsr gesvid;
005800080930         enddo;
005900080930
006000080930         *inlr = *on;
006100080930
006200080930       // ----------------------------------------------------------------------
006300080930       //?Gestione videata
006400080930       // ----------------------------------------------------------------------
006500080930       begsr gesvid;
006600080930
006700080930       //?emetto la videata
006800080930         exfmt bsc2d00;
006900080930
007000080930       //?spengo indicatori di comodo
007100080930         *in28 = *off;
007200080930       //?pulisco campo messaggi
007300080930         clear w1cmsg;
007400080930
007500080930       //?f3=fine
007600080930         if *inkc;
007700080930          $fine = *on;
007800080930          leavesr;
007900080930         endif;
008000080930
008100080930       //?controllo i dati della videata
008200080930         exsr sr_contrfil;
008300080930         if *in28;
008400080930          leavesr;
008500080930         endif;
008600080930
008700080930       //?controllo che non ci siano già testi per la filiale
008800080930         exsr sr_contrtxt;
008900080930         if *in28;
009000080930          leavesr;
009100080930         endif;
009200080930
009300080930       //?f6=elabora
009400080930         if *inkf;
009500080930          exsr sr_elabora;
009600080930          $fine = *on;
009700080930          leavesr;
009800080930         endif;
009900080930
010000080930       endsr;
010100080930
010200080930       // ----------------------------------------------------------------------
010300080930       //?Controllo filiale
010400080930       // ----------------------------------------------------------------------
010500080930       begsr sr_contrfil;
010600080930
010700080930         *in40 = *off;
010800080930         clear w1dfil;
010900080930
011000080930         chain w1cfil azorg01l;
011100080930         if  not %found(azorg01l) or orgfva <> *blanks;
011200080930          *in28 = *on;
011300080930          *in40 = *on;
011400080930          w1cmsg = msg(01);
011500080930          leavesr;
011600080930         endif;
011700080930
011800080930         w1dfil = orgdes;
011900080930
012000080930       //?deve essere in gestione all'utente
012100080930         clear fnlv50ds;
012200080930         d50pru = knmus;
012300080930         d50fgs = w1cfil;
012400080930         fnlv50r(fnlv50ds);
012500080930         if d50err <> *blanks;
012600080930          *in28 = *on;
012700080930          *in40 = *on;
012800080930          w1cmsg = d50msg;
012900080930          leavesr;
013000080930         endif;
013100080930
013200080930       //?deve essere un primo livello
013300080930         clear fnlv55ds;
013400080930         d55tpt = 'P';
013500080930         d55lin = w1cfil;
013600080930         d55drf = *date;
013700080930         fnlv55r(fnlv55ds);
013800080930         if d55err <> *blanks;
013900080930          *in28 = *on;
014000080930          *in40 = *on;
014100080930          w1cmsg = msg(01);
014200080930          leavesr;
014300080930         endif;
014400080930         if d55tfp <> w1cfil;
014500080930          *in28 = *on;
014600080930          *in40 = *on;
014700080930          w1cmsg = msg(03);
014800080930          leavesr;
014900080930         endif;
015000080930
015100080930       endsr;
015200080930
015300080930       // ----------------------------------------------------------------------
015400080930       //?Controllo che non ci siano già testi per la filiale
015500080930       // ----------------------------------------------------------------------
015600080930       begsr sr_contrtxt;
015700080930
015800080930         clear conta;
015900080930
016000080930         exec sql
016100080930          select count(*) into :conta from fntxt00f where
016200080930          txtpol = :w1cfil;
016300080930       //?se ho trovato almeno una riga di testo errore
016400080930         if conta > *zeros;
016500080930          *in28 = *on;
016600080930          w1cmsg = msg(02);
016700080930          leavesr;
016800080930         endif;
016900080930
017000080930       endsr;
017100080930
017200080930       // ----------------------------------------------------------------------
017300080930       //?Elabora
017400080930       // ----------------------------------------------------------------------
017500080930       begsr sr_elabora;
017600080930
017700080930         $finetxt = *off;
017800080930
017900080930         exec sql
018000080930          declare testi cursor for select fntxt01l.*
018100080930          from fntxt01l where txtpol = 0
018200080930          order by txtlet, txtpol, txtlin, txtnrr;
018300080930
018400080930         exec sql open testi;
018500080930
018600080930         dow not $finetxt;
018700080930          exec sql fetch next from testi into: fntxtds;
018800080930
018900080930          if sqlcod = 100 or sqlcod < 0;
019000080930           $finetxt = *on;
019100080930           leave;
019200080930          endif;
019300080930
019400080930          exsr sr_scrivi;
019500080930
019600080930         enddo;
019700080930
019800080930       //?chiuso cursore
019900080930         exec sql close testi;
020000080930
020100080930       endsr;
020200080930
020300080930       // ----------------------------------------------------------------------
020400080930       //?Scrivo testi per nuova filiale
020500080930       // ----------------------------------------------------------------------
020600080930       begsr sr_scrivi;
020700080930
020800080930         fntxtnewds = fntxtds;
020900080930         newtxtpol = w1cfil;
021000080930         write fntxt01;
021100080930
021200080930       endsr;
021300080930
021400080930       // ----------------------------------------------------------------------
021500080930       //?Operazioni iniziali.
021600080930       // ----------------------------------------------------------------------
021700080930       begsr routinz;
021800080930
021900080930       //?recupero dei dati utente
022000080930         in(e) §azute;
022100080930          if not %error;
022200080930          in(e) §datiute;
022300080930         endif;
022400080930         if %error or rsut = *blanks;
022500080930          tibs34r(tibs34ds);
022600080930          in §azute;
022700080930          in §datiute;
022800080930         endif;
022900080930
023000080930       //?imposto la filiale in gestione
023100080930         w1cfil = dutpou;
023200080930       //?decodifico la filiale gestione
023300080930         w1dfil = dutdpo;
023400080930
023500080930         $fine =*off;
023600080930
023700080930       endsr;
023800080930
023900080930** MSG  Lungh. 78                                                            *
024000080930Filiale errata                                                                01
024100080930Testi già esistenti per la filiale                                            02
024200080930Filiale non di primo livello                                                  03
