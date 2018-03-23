000100090708      //---------------------------------------------------------------
000200091013      //?FNLRW1R - Controllo spedizioni Wurth         - batch -
000300091013      //?          codici 0430841 - 0434470
000400090708      //---------------------------------------------------------------
000500090708
000600090708     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000700090708
000800090708      //---------------------------------------------------------------
000900090708      //?Dichiarazione file.
001000090708      //---------------------------------------------------------------
001100091013     ffnorm01l  if   e           k disk
001200091013     ffnlrw1p   o    e             printer oflind(*in99)
001300090708
001400090708      //---------------------------------------------------------------
001500090511      //?Definizione costanti.
001600090511      //---------------------------------------------------------------
001700090511
001800090511      //---------------------------------------------------------------
001900090511      //?Definizione schiere.
002000090511      //---------------------------------------------------------------
002100090708
002200090511      //---------------------------------------------------------------
002300090511      //?Definizione aree dati.
002400090511      //---------------------------------------------------------------
002500090708
002600090708      // - Dati utente
002700090708     d §AzUte        e ds                  extname(AZUTE00F)
002800090708     d                                     dtaara
002900090708     d §DatiUte      e ds                  extname(dDatiUte)
003000090708     d                                     dtaara
003100090511
003200090511      //---------------------------------------------------------------
003300090511      //?Definizione strutture dati.
003400090511      //---------------------------------------------------------------
003500090708
003600090708      // - Parametri ricevuti
003700090708     d KPJBA         e ds
003800091012
003900091012      // - Parametri per Reperimento legami
004000091012     d tibs10ds      e ds                  inz
004100091012     d  skfigli               21   5520  0 dim(500)
004200090708
004300090708      // - Reperimento dati utente
004400090708     d TIBS34ds      e ds
004500090805
004600091012      // File bolle
004700091013     d fnarbds       e ds                  extname(FNARB00F)
004800091013     d fiar4ds       e ds                  extname(FIAR400F)
004900090511
005000090511      //---------------------------------------------------------------
005100090511      //?Definizione variabili globali.
005200090511      //---------------------------------------------------------------
005300090708
005400090708      // - Flags booleani
005500091013     d $End_no_ORM     s               n   inz(*off)
005600091013     d $End_si_ORM     s               n   inz(*off)
005700090708
005800090708      // - Campi di comodo
005900090721     d wdata           s              8  0
006000090721     d wDate_EUR       s               d   datfmt(*eur)
006100090721     d wDate_ISO       s               d   datfmt(*iso)
006200091013     d wddc            s                   like(arbddc)
006300091013     d wormcor         s                   like(arbksc)
006400091013     d w0110           s             11  0
006500091013     d $conta          s              3  0 inz
006600090511
006700090511      //---------------------------------------------------------------
006800090511      //?Definizione procedure esterne.
006900090511      //---------------------------------------------------------------
007000090708
007100090708      // - Reperimento dati utente
007200090708     d tibs34r         pr                  extpgm('TIBS34R')
007300090708     d  tibs34ds                           likeds(TIBS34ds)
007400091012
007500091012      // - Reperimento legami
007600091012      /copy gaitrasrc/srcprotopr,tibs10r
007700090511
007800090511      //---------------------------------------------------------------
007900090511      //?Definizione key-list.
008000090511      //---------------------------------------------------------------
008100091013
008200091013       // - File FNORM01L
008300091013     d k04fnorm      e ds                  extname(FNORM01L:*key)
008400091013     d                                     prefix(k_)
008500091013     d                                     inz
008600090708
008700090708      //---------------------------------------------------------------
008800090708      //?Riepilogo indicatori.
008900090708      //---------------------------------------------------------------
009000090708
009100091013      // 99    : overflow
009200090511
009300090511      //---------------------------------------------------------------
009400090708      //?M A I N - L I N E
009500090511      //---------------------------------------------------------------
009600090708
009700090708     c     *Entry        plist
009800090708     c                   parm                    KPJBA
009900090708
010000090708      /free
010100090708
010200090708       //?Operazioni iniziali
010300090708       exsr sr_RoutInz;
010400090708
010500091013       //?Stampo testata
010600091013       write lrw1t01;
010700091013
010800091013       //?Controllo se ci sono spedizioni senza ORM
010900091013       exsr sr_noOrm;
011000091013
011100091013       //?Controllo se ci sono spedizioni con ORM e ordinante no WURTH
011200091013       exsr sr_siOrm;
011300090708
011400090708       //?Operazioni finali
011500090708       exsr sr_RoutEnd;
011600090708
011700090708       //--------------------------------------------------------------
011800090708       //?Operazioni iniziali.
011900090708       //--------------------------------------------------------------
012000090708       BEGSR sr_RoutInz;
012100090708
012200090708         //?Reperimento dati job
012300090708         exsr sr_DatiJob;
012400090708
012500090708         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
012600090805
012700091012         //?Carico sk figli unificante Wurth
012800091012         exsr sr_CarFigli;
012900091013
013000091013         //?Imposto la data ricevuta
013100091013         IF %subst(kpjbu:1:8) > *zeros;
013200091013           wdata = %dec(%subst(kpjbu:1:8):8:0);
013300091013         ELSE;
013400091013          wdata = %dec(%date());
013500091013         ENDIF;
013600091013
013700091013         wddc = wdata;
013800090708
013900090708       ENDSR;
014000090708
014100090708       //--------------------------------------------------------------
014200090708       //?Reperimento Dati del job (Utente/Operativi).
014300090708       //--------------------------------------------------------------
014400090708       BEGSR sr_DatiJob;
014500090708
014600090708         in(E) §AzUte;
014700090722         IF  NOT %error;
014800090708           in(E) §DatiUte;
014900090722         ENDIF;
015000090722         IF  %error or RSut = *blanks;
015100090708           clear TIBS34ds;
015200090708           tibs34r(tibs34ds);
015300090708           in §AzUte;
015400090708           in §DatiUte;
015500090722         ENDIF;
015600090708
015700090708       ENDSR;
015800090805
015900090805       //--------------------------------------------------------------
016000091012       //?Carico Figli Unificante Wurth.
016100090805       //--------------------------------------------------------------
016200091012       BEGSR sr_CarFigli;
016300090805
016400091013       // ricerco i figli del codice unificante WURTH
016500091012            clear tibs10ds;
016600091012            d10drf = *date;
016700091012            d10tle = 'WW';
016800091012            d10paf = 'F';
016900091012            d10cod = 0430839;
017000091012            tibs10r(tibs10ds);
017100090805
017200090805       ENDSR;
017300091013
017400091013       //--------------------------------------------------------------
017500091013       //?Controllo se ci sono spedizoni senza ORM.
017600091013       //--------------------------------------------------------------
017700091013       BEGSR sr_noOrm;
017800091013
017900091013         $conta = 0;
018000091013
018100091013         exec sql
018200091013          declare ARB_NO_ORM cursor for
018300091013          select fnarb00f.* from fnarb00f exception join fiar400f on
018400091013          arbaas = ar4aas and arblnp = ar4lnp and arbnrs = ar4nrs and
018500091013          arbnsp = ar4nsp
018600091013          where arblna = 277 and arbddc = :wddc and
018700091015          arbksc in (0430841, 0434470) and ar4trc = 'M'and
018800091015          arbcbo in ('2 ', '6 ');
018900091013
019000091013         exec sql
019100091013           open ARB_NO_ORM;
019200091013           IF sqlcode < 0;
019300091013             leavesr;
019400091013           ENDIF;
019500091013
019600091013         DOW not $End_no_ORM;
019700091013           exec sql
019800091013             fetch next from ARB_NO_ORM into :fnarbds;
019900091013             IF sqlcod = 100 or sqlcod < 0;
020000091013               $End_no_ORM = *on;
020100091013               leave;
020200091013             ENDIF;
020300091013
020400091013       //?Stampo testata
020500091013           IF  $conta = 0;
020600091013             write lrw1t02;
020700091013             write lrw1r01;
020800091013             $conta = 1;
020900091013           ENDIF;
021000091013
021100091013       //?Salto pagina
021200091013           IF  *in99;
021300091013             write lrw1t01;
021400091013             write lrw1t02;
021500091013             write lrw1r01;
021600091013             *in99 = *off;
021700091013           ENDIF;
021800091013
021900091013       //?giro la data distinta
022000091013           wDate_ISO = %date(arbddc:*iso);
022100091013           wDate_EUR = wDate_ISO;
022200091013           pddc = %dec(wDate_EUR);
022300091013
022400091013       //?Stampo riga
022500091013           write lrw1d01;
022600091013
022700091013         ENDDO;
022800091013
022900091013         exec sql close ARB_NO_ORM;
023000091013
023100091013       ENDSR;
023200091013
023300091013       //--------------------------------------------------------------
023400091013       //?Controllo se ci sono spedizoni con ORM e ordinante no WURTH.
023500091013       //--------------------------------------------------------------
023600091013       BEGSR sr_siOrm;
023700091013
023800091013         $conta = 0;
023900091013
024000091013         exec sql
024100091013          declare ARB_SI_ORM cursor for
024200091013          select fnarb00f.*, fiar400f.* from fnarb00f join fiar400f on
024300091013          arbaas = ar4aas and arblnp = ar4lnp and arbnrs = ar4nrs and
024400091013          arbnsp = ar4nsp
024500091013          where arblna = 277 and arbddc = :wddc and
024600091015          arbksc in (0430841, 0434470) and ar4trc = 'M' and
024700091015          arbcbo in ('2 ', '6 ');
024800091013
024900091013         exec sql
025000091013           open ARB_SI_ORM;
025100091013           IF sqlcode < 0;
025200091013             leavesr;
025300091013           ENDIF;
025400091013
025500091013         DOW not $End_si_ORM;
025600091013           exec sql
025700091013             fetch next from ARB_SI_ORM into :fnarbds, :fiar4ds;
025800091013             IF sqlcod = 100 or sqlcod < 0;
025900091013               $End_si_ORM = *on;
026000091013               leave;
026100091013             ENDIF;
026200091013
026300091013             k_ormpoe = %dec(%subst(ar4not:1:3):3:0);
026400091013             k_ormnsr = %dec(%subst(ar4not:4:2):2:0);
026500091013             k_ormnor = %dec(%subst(ar4not:6:7):7:0);
026600091013             k_ormnrv = %dec(%subst(ar4not:13:2):2:0);
026700091013       //?aggancio ORM per controllo ordinante
026800091013             chain  %kds(K04fnorm) FNORM01L;
026900091013             IF  not %found(FNORM01L);
027000091013               iter;
027100091013             ENDIF;
027200091013
027300091013       //?controllo ordinante ORM
027400091013             wormcor = ormcor/1000;
027500091013             w0110   = wormcor;
027600091013             IF  %lookup(w0110:skfigli) > *zeros;
027700091013               iter;
027800091013             ENDIF;
027900091013
028000091013       //?Stampo testata
028100091013           IF  $conta = 0;
028200091013             write lrw1t03;
028300091013             write lrw1r02;
028400091013             $conta = 1;
028500091013           ENDIF;
028600091013
028700091013       //?Salto pagina
028800091013           IF  *in99;
028900091013             write lrw1t01;
029000091013             write lrw1t03;
029100091013             write lrw1r02;
029200091013             *in99 = *off;
029300091013           ENDIF;
029400091013
029500091013       //?giro la data distinta
029600091013           wDate_ISO = %date(arbddc:*iso);
029700091013           wDate_EUR = wDate_ISO;
029800091013           pddc = %dec(wDate_EUR);
029900091013
030000091013       //?Stampo riga
030100091013           write lrw1d02;
030200091013
030300091013         ENDDO;
030400091013
030500091013         exec sql close ARB_SI_ORM;
030600091013
030700091013       ENDSR;
030800090708
030900090708       //--------------------------------------------------------------
031000090708       //?Operazioni finali.
031100090708       //--------------------------------------------------------------
031200090708       BEGSR sr_RoutEnd;
031300091013
031400091013       //?Stampo fine
031500091013         write lrw1f01;
031600090708
031700090708         *inLR = *on;
031800090708         return;
031900090708
032000090708       ENDSR;
