000100071227     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000200071227
000300160629     ftrecvoc6d cf   e             workstn
000400160701     ftabel00f  if   e           k disk
000500071227
000600071228      //-----------------------------------------------------------------------
000700071228      //? RIEPILOGO INDICATORI
000800071228      //-----------------------------------------------------------------------
000900071228      // 28 - emette messaggio
001000071228      // 40 - data dal
001100071228      // 41 - data al
001200071228      //-----------------------------------------------------------------------
001300071227
001400071228      //-----------------------------------------------------------------------
001500071228      //? VARIABILI
001600071228      //-----------------------------------------------------------------------
001700160701     d codut           s              1  0 inz(1)
001800160701     d kcod            s                   like(tblcod)
001900160701     d kkey            s                   like(tblkey)
002000160701     d ds$3          e ds
002100071228     d wdataisoda      s               d   datfmt(*ISO)
002200071228     d wdataisoal      s               d   datfmt(*ISO)
002300071227     d wmesi           s              2  0
002400071228     d $fine           s              1    inz(*off)
002500071228     d $inzd01         s              1    inz(*on)
002600071227
002700071228      //-----------------------------------------------------------------------
002800071228      //? SCHIERE
002900071228      //-----------------------------------------------------------------------
003000071227     d msg             s             78    dim(10) ctdata perrcd(1)
003100071227
003200071228      //-----------------------------------------------------------------------
003300071228      //? STRUTTURE DATI
003400071228      //-----------------------------------------------------------------------
003500071227     d param           ds
003600071227     d  wdatada                1      8  0
003700071227     d  wdataal                9     16  0
003800160705     d  annov                 17     20  0
003900160722     d  datarif               21     28  0
004000071228
004100071228     d wlbdat          ds                  inz
004200071228     d  g08dat                        8  0 inz
004300071228     d  g08inv                        8  0 inz
004400071228     d  g08err                        1    inz
004500071228     d  g08tgi                        5  0 inz
004600071227
004700071227     d kpjba         e ds
004800071228     d tibs34ds      e ds                  inz
004900071228
005000071228     d                sds
005100071228     d   vidpgm          *proc
005200071228
005300071228      //---------------------------------------------------------------
005400071228      //? AREE DATI
005500071228      //---------------------------------------------------------------
005600071228     d �azute        e ds                  extname(azute00f)
005700071228     d                                     dtaara
005800071228     d �datiute      e ds                  extname(ddatiute)
005900071228     d                                     dtaara
006000071228
006100071228      //---------------------------------------------------------------
006200071228      //? PROGRAMMI RICHIAMATI
006300071228      //---------------------------------------------------------------
006400071228      // reperimento dati utente
006500071228     d tibs34r         pr                  extpgm('TIBS34R')
006600071228     d  tibs34ds                           likeds(tibs34ds)
006700071228
006800071228      // controllo ed inversione date
006900071228     d xsrda8          pr                  extpgm('XSRDA8')
007000071228     d  wlbdat                             likeds(wlbdat)
007100071228
007200071228      // sottomissione lavoro batch
007300071228     d bch10           pr                  extpgm('BCH10')
007400071228     d  kpjba                              likeds(kpjba)
007500071227
007600071228      //---------------------------------------------------------------
007700071228
007800071228     c     *entry        plist
007900071228     c                   parm                    kpjba
008000160701     c     ktabel        klist
008100160701     c                   kfld                    codut
008200160701     c                   kfld                    kcod
008300160701     c                   kfld                    kkey
008400160701     c* verifico se analizzare il TASP e il CEVP
008500160701     c                   eval      kcod = '$3'
008600160701     c                   movel     '1'           kkey
008700160701     c     ktabel        chain     tabel00f
008800160701     c                   if        %found(tabel00f)
008900160701     c                   eval      ds$3 = tbluni
009000160701     c                   end
009100160705     c                   move      *date         annov
009200071228
009300071228      /free
009400071228
009500071228       // operazioni iniziali
009600071228       exsr sr_routinz;
009700071227
009800071228       // gestione video
009900071228       dow $fine = *off;
010000071228            exsr sr_gesd01;
010100071228       enddo;
010200071228
010300071228       // Operazioni finali
010400071228       exsr sr_routend;
010500071228
010600071228
010700071228       //---------------------------------------------------------------
010800071228       //?Operazioni iniziali
010900071228       //---------------------------------------------------------------
011000071228       begsr sr_routinz;
011100071228
011200071228       // dati job
011300071228         exsr sr_datijob;
011400071228
011500071228       endsr;
011600071228
011700071228       //--------------------------------------------------------------
011800071228       //?Dati del job
011900071228       //--------------------------------------------------------------
012000071228       begsr sr_datijob;
012100071228
012200071228         in(e) �azute;
012300071228         if not %error;
012400071228           in(e) �datiute;
012500071228         endif;
012600071228         if %error or rsut = *blanks;
012700071228           clear tibs34ds;
012800071228           tibs34r(tibs34ds);
012900071228           in �azute;
013000071228           in �datiute;
013100071228         endif;
013200071228
013300071228       endsr;
013400071228
013500071228       //--------------------------------------------------------------
013600071228       //?Gestione video
013700071228       //--------------------------------------------------------------
013800071228       begsr sr_gesd01;
013900071228
014000071228       // inizializzo il video
014100071228         if $inzd01 = *on;
014200071228           exsr sr_inzd01;
014300071228           $inzd01 = *off;
014400071228         endif;
014500071228
014600071228       // emissione video
014700071228         exfmt ss01d01;
014800071228         *in28  = *off;
014900071228         clear v1cmsg;
015000071228
015100071228         select;
015200071228       // F3=Fine
015300071228           when *inkc;
015400071228             exsr sr_f03;
015500071228           when *inkf;
015600071228             exsr sr_contrd01;
015700071228             if *in28;
015800071228               leavesr;
015900071228             endif;
016000071228             exsr sr_f06;
016100071228       // Enter
016200071228           other;
016300071228             exsr sr_contrd01;
016400071228             if *in28;
016500071228               leavesr;
016600071228             endif;
016700071228         endsl;
016800071228
016900071228       endsr;
017000071228
017100071228       //--------------------------------------------------------------
017200071228       //?Inizializzo video
017300071228       //--------------------------------------------------------------
017400071228       begsr sr_inzd01;
017500071228
017600160629         v1cdtda = 0;
017700160629         v1cdtal = 0;
017800160722         v1drif  = 0;
017900071228
018000071228       endsr;
018100071228
018200071228       //--------------------------------------------------------------
018300071228       //?Gestione tasto F3 da video
018400071228       //--------------------------------------------------------------
018500071228       begsr sr_f03;
018600071228
018700071228       // chiude il programma
018800071228         $fine = *on;
018900071228
019000071228       endsr;
019100071228
019200071228       //--------------------------------------------------------------
019300071228       //?Gestione tasto F6 da video
019400071228       //--------------------------------------------------------------
019500071228       begsr sr_f06;
019600071228
019700071228       // lancio elaborazione batch
019800160628         kcoaz = 'ECV7';
019900071228         kpjbu = param;
020000071228           BCH10(kpjba);
020100071228
020200071228       // uscita dal pgm
020300071228         $fine = *on;
020400071228
020500071228       endsr;
020600071228
020700071228       //--------------------------------------------------------------
020800071228       //?Controllo dati immessi a video
020900071228       //--------------------------------------------------------------
021000071228       begsr sr_contrd01;
021100071228
021200071228         *in40 = *off;
021300071228         *in41 = *off;
021400160722         *in42 = *off;
021500071228
021600071228       // data da obbligatoria
021700071228         if v1cdtda = *zeros;
021800071228             *in28 = *on;
021900071228             *in40 = *on;
022000071228             v1cmsg = msg(01);
022100071228             leavesr;
022200071228         endif;
022300071228       // controllo data da
022400071228         clear wlbdat;
022500071228          g08dat = v1cdtda;
022600071228          xsrda8(wlbdat);
022700071228           if g08err = *on;
022800071228               *in28 = *on;
022900071228               *in40 = *on;
023000071228               v1cmsg = msg(01);
023100071228               leavesr;
023200071228           endif;
023300071228          v1cdtda = g08dat;
023400071228          wdatada = g08inv;
023500071228
023600071228       // data al obbligatoria
023700071228         if v1cdtal = *zeros;
023800071228             *in28 = *on;
023900071228             *in41 = *on;
024000071228             v1cmsg = msg(01);
024100071228             leavesr;
024200071228         endif;
024300071228       // controllo data al
024400071228         clear wlbdat;
024500071228          g08dat = v1cdtal;
024600071228          xsrda8(wlbdat);
024700071228           if g08err = *on;
024800071228               *in28 = *on;
024900071228               *in41 = *on;
025000071228               v1cmsg = msg(01);
025100071228               leavesr;
025200071228           endif;
025300071228          v1cdtal = g08dat;
025400071228          wdataal = g08inv;
025500071228
025600071228       // controllo data da non superiore data al
025700071228         if wdatada > wdataal;
025800071228             *in28 = *on;
025900071228             *in40 = *on;
026000071228             v1cmsg = msg(02);
026100071228             leavesr;
026200071228          endif;
026300071228
026400160701       // controllo che la data finale sia in tas10
026500160701         if �$3d10 > wdatada;
026600160701             *in28 = *on;
026700160701             *in40 = *on;
026800160701             v1cmsg = msg(03);
026900160701             leavesr;
027000160701          endif;
027100160722
027200160722       // data di riferimento
027300160722         if v1drif  = *zeros;
027400160722             *in28 = *on;
027500160722             *in42 = *on;
027600160722             v1cmsg = msg(01);
027700160722             leavesr;
027800160722         endif;
027900160722       // controllo data al
028000160722         clear wlbdat;
028100160722          g08dat = v1drif;
028200160722          xsrda8(wlbdat);
028300160722           if g08err = *on;
028400160722               *in28 = *on;
028500160722               *in42 = *on;
028600160722               v1cmsg = msg(01);
028700160722               leavesr;
028800160722           endif;
028900160722          v1drif  = g08dat;
029000160722          datarif = g08inv;
029100160722
029200071228
029300071228       endsr;
029400071228
029500071228       //--------------------------------------------------------------
029600071228       //?Operazioni finali.
029700071228       //--------------------------------------------------------------
029800071228       begsr sr_routend;
029900071228
030000071228         *inlr = *on;
030100071228         return;
030200071228
030300071228       endsr;
030400071228
030500071228      /end-free
030600071227
030700071227      *------------------------------------------------------------------------*
030800071227** msg
030900071227Data errata                                                                   01
031000071227Data iniziale non puo' essere maggiore della data finale                      02
031100160701Il periodo da elaborare non � presente negli archivi (solo TAS10)             03
