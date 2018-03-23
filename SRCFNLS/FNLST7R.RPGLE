000100110620     /*PRM dbgview(*source)
000200040107      *---------------------------------------------------------------*
000300130517      * ELENCO TRAZIONI in ARRIVO                   - FILTRO -        *
000400040107      *---------------------------------------------------------------*
000500040107
000600040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700040107
000800040107      *---------------------------------------------------------------*
000900040107
001000130516     ftabel00f  if   e           k disk
001100130517     fazorg01l  if   e           k disk
001200140620     fazcln01l  if   e           k disk
001300130515     ffnlst7d   cf   E             workstn
001400040112      *
001500040107      *---------------------------------------------------------------*
001600040107      *
001700040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
001800040107      *
001900130516     d err             s             79    dim(4) ctdata perrcd(1)
002000040107      *
002100040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
002200040107      *
002300130515     d fnlst8ds      e ds
002400130515     d Tibs34ds      e ds                  inz
002500130515     d §azute        e ds                  Extname(Azute00f)
002600130515     d                                     dtaara
002700130515     d §Datiute      e ds                  EXTname(ddatiute)
002800130515     d                                     dtaara
002900090828
003000040107     d KPJBA         e ds
003100130516     d ds1x          e ds
003200140620     D clnmat          DS
003300140620     D  mat                    1     31    dim(31)
003400140620     D clnpom          DS
003500140620     D  pom                    1     31    dim(31)
003600040123      *
003700040109      *
003800090409
003900040107      *
004000040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004100040109      *
004200040109      * - Campi di comodo
004300130516     d num_days        s             15p 0
004400130516     D w0060           S              6  0
004500130516     d vidddainv       s                   like(viddda)
004600130516     d viddalinv       s                   like(viddal)
004700130516     D wkey            S                   like(tblkey)
004800130516     d time            s               t
004900111205     d dataiso         s               d   datfmt(*iso)
005000130516     d dataisoda       s               d   datfmt(*iso)
005100130516     d dataisoa        s               d   datfmt(*iso)
005200111205     d dataeur         s               d   datfmt(*eur)
005300111205     d datacur         s               d   datfmt(*iso)
005400130516     d datacurdal      s               d   datfmt(*iso)
005500130516     d datacural       s               d   datfmt(*iso)
005600130516     d timecur         s              6  0
005700111205     d flerr           s              1
005800140620     d wfes            s              1
005900111202      // - Campi associati al video
006000111206     d $Video          s              2    inz('D1')
006100090828      *
006200090828      * - Flags booleani
006300090828     d $Finerec        s              1    inz(*off)
006400111202     d $Fine           s               n   inz(*off)
006500111207     d $End            s               n   inz(*off)
006600111202     d $InzD01         s               n   inz(*on)
006700111205
006800081023
006900130515     d bch10           pr                  extpgm('BCH10')
007000130515     d  kpjba                              likeds(kpjba)
007100130515
007200090402      // ? PROTOTIPI ?
007300090402      /copy gaitrasrc/srcprotopr,tibs34r
007400111206
007500040107
007600040107      *===============================================================*
007700040107      *  RIEPILOGO INDICATORI                                         *
007800040107      *---------------------------------------------------------------*
007900090827      *  XX    -
008000040107      *===============================================================*
008100040107
008200040107     c     *Entry        plist
008300040107     c                   parm                    KPJBA
008400090827     c
008500040123      *
008600090331      /free
008700111202       //?Gestione video
008800111202       DOW $Fine = *off;
008900111202             exsr GesD01;
009000111202       ENDDO;
009100090610
009200090827       eval *inlr=*on;
009300111206       //-------------------------------------------------------------*
009400111206       //                                                             *
009500111206       //-------------------------------------------------------------*
009600111206       Begsr Gesd01;
009700111206       // Inizializzazione videata
009800111206          if $inzd01;
009900140703              exsr sr_inzd01;
010000130516              $InzD01  = *off;
010100111206          endif;
010200130515             exfmt lst7d01;
010300111206             clear vidmsg;
010400111206             *in28=*off;
010500111206             *in90=*off;
010600111206             select;
010700111206             // F3=Fine
010800111206                when *inkc;
010900111206                   $fine=*on;
011000111206                   leavesr;
011100111206                other;
011200111206                   exsr ctrd01;
011300111206                   if *in90;
011400111206                      leavesr;
011500111206                   endif;
011600130516                   if *inkf;
011700130515                      clear fnlst8ds;
011800130516                      lst8dal=vidddainv;
011900130516                      lst8al=viddalinv;
012000130516                      lst8hdl=vidhda;
012100130516                      lst8hal=vidhal;
012200130515                      kpjbu=fnlst8ds;
012300130515                      kcoaz='LST8';
012400130515                      bch10(kpjba);
012500130515                      $fine=*on;
012600130515                      leavesr;
012700111206                   endif;
012800111206             endsl;
012900111206       endsr;
013000130516       //-------------------------------------------------------------*
013100130516       // Inizializzazione prima videata                              *
013200130516       //-------------------------------------------------------------*
013300130516       Begsr sr_inzD01;
013400130517         vidtfa=dutpou;
013500130517         clear viddfa;
013600130517         chain (dutpou) azorg01l;
013700130517         if %found(azorg01l);
013800130517            viddfa=orgdes ;
013900130517         endif;
014000130516         clear vidmsg;
014100130516         clear viddda;
014200130516         clear vidhda;
014300130516         clear viddal;
014400130516         clear vidhal;
014500130516         viddda=%dec(datacurdal:*eur);
014600130516         viddal=%dec(datacural:*eur);
014700130516       // ora al = ora di soglia presa da tabella
014800130516         vidhal=§1xhho;
014900130516       // ora dal = ora di soglia presa da tabella + 1 minuto
015000130516         time=%time((vidhal*100):*hms)+%minutes(1) ;
015100130516         w0060=%dec(time)      ;
015200130516         vidhda=%dec(%subst(%editc(w0060:'X'):1:4):4:0);
015300130516         *in28=*off;
015400130516           *in40=*off;
015500130516           *in41=*off;
015600130516           *in42=*off;
015700130516       endsr;
015800111206       //-------------------------------------------------------------*
015900111206       // Controllo prima videata                                     *
016000111206       //-------------------------------------------------------------*
016100111206       Begsr ctrd01  ;
016200130516
016300130516       *in40=*off;
016400130516       *in41=*off;
016500130516       *in42=*off;
016600130516
016700130516       // data dal
016800130516
016900130516       if %subst(%editc(viddda:'X'):1:2)='00';
017000130516          test(DE) *dmy viddda;
017100120102          if %error;
017200120102             *in90=*on;
017300130516             *in40=*on;
017400120102             *in28=*on;
017500120102             vidmsg=err(1);
017600120102             leavesr;
017700120102          else;
017800130516             dataiso=%date(viddda:*dmy);
017900120102          endif;
018000120102       else;
018100130516          test(DE) *eur viddda;
018200120102          if %error;
018300120102             *in90=*on;
018400130516             *in40=*on;
018500120102             *in28=*on;
018600120102             vidmsg=err(1);
018700120102             leavesr;
018800120102          else;
018900130516             dataiso=%date(viddda:*eur);
019000120102          endif;
019100120102       endif;
019200111206          dataeur=dataiso;
019300130516          viddda=%dec(dataeur);
019400130516          vidddainv=%dec(dataiso);
019500130516
019600130516       // data al
019700130516
019800130516       if %subst(%editc(viddal:'X'):1:2)='00';
019900130516          test(DE) *dmy viddal;
020000130516          if %error;
020100130516             *in90=*on;
020200130516             *in42=*on;
020300130516             *in28=*on;
020400130516             vidmsg=err(1);
020500130516             leavesr;
020600130516          else;
020700130516             dataiso=%date(viddal:*dmy);
020800130516          endif;
020900130516       else;
021000130516          test(DE) *eur viddal;
021100130516          if %error;
021200130516             *in90=*on;
021300130516             *in42=*on;
021400130516             *in28=*on;
021500130516             vidmsg=err(1);
021600130516             leavesr;
021700130516          else;
021800130516             dataiso=%date(viddal:*eur);
021900130516          endif;
022000130516       endif;
022100130516          dataeur=dataiso;
022200130516          viddal=%dec(dataeur);
022300130516          viddalinv=%dec(dataiso);
022400130516
022500130516       // data/ora dal no maggiore di data/ora al
022600130516          if vidddainv>viddalinv;
022700130516             *in90=*on;
022800130516             *in40=*on;
022900130516             *in28=*on;
023000130516             vidmsg=err(2);
023100130516             leavesr;
023200130516          endif;
023300130516       // se data dal=data al allora ora dal no maggiore di ora al
023400130516          if vidddainv=viddalinv and vidhda>vidhal;
023500130516             *in90=*on;
023600130516             *in41=*on;
023700130516             *in28=*on;
023800130516             vidmsg=err(3);
023900130516             leavesr;
024000130516          endif;
024100130516       // il range tra le due date non deve superare 7gg
024200130516           dataisoda=%date(vidddainv:*iso);
024300130516           dataisoa=%date(viddalinv:*iso);
024400130516           num_days=%diff(dataisoa: dataisoda: *days);
024500130516           num_days+=1;
024600130516           if num_days>7;
024700130516             *in90=*on;
024800130516             *in40=*on;
024900130516             *in28=*on;
025000130516             vidmsg=err(4);
025100130516             vidmsg=%trim(vidmsg) + ' sette giorni';
025200130516             leavesr;
025300130516           endif;
025400111206
025500111206       endsr;
025600090402       //-------------------------------------------------------------*
025700090402       //Routine Iniziale                                             *
025800090402       //-------------------------------------------------------------*
025900090414       begsr *inzsr;
026000090402
026100090402       //Reperisco dati job
026200090917         exsr datijob;
026300130517
026400130516
026500130516       // Leggo tabella "1X" per recuperare l'ora di soglia trazioni
026600130516       clear ds1x;
026700130517       wkey=%editc(dutpou:'X')+'OST '     ;
026800130516       chain (1:'1X': wkey) tabel00f  ;
026900130517  1    if not %found(tabel00f) or tblflg<>' '  ;
027000130516          chain (1:'1X': '   OST ') tabel00f  ;
027100130516  1    endif     ;
027200130516       ds1x=tbluni             ;
027300130517
027400130517       // Reperisco la data corrente
027500130517          datacur=(%date());
027600130517          timecur=%dec(%time());
027700130517
027800130517          if timecur>§1xhho*100;
027900130517             datacural=datacur +%days(1);
028000140620       // se la data al calcolata è festiva incremento fino al giorno lavora-
028100140620       // tivo successivo
028200140620             exsr sr_datafes;
028300140703
028400140703             datacurdal=datacur;
028500140703             exsr sr_datafesdal;
028600130517          else;
028700130517             datacural=datacur;
028800140703             exsr sr_datafes;
028900140703
029000130517             datacurdal=datacur -%days(1);
029100140623       // se la data dal calcolata è festiva decremento fino al giorno lavora-
029200140623       // tivo precedente
029300140623             exsr sr_datafesdal;
029400130517          endif;
029500090402
029600090402       EndSr;
029700140620       //--------------------------------------------------------------
029800140620       //?Controllo data se festiva + restituzione data prima data non festiva
029900140620       //--------------------------------------------------------------
030000140620       BEGSR  sr_DataFes;
030100140620       wfes='1';
030200140620       dow wfes='1';
030300140620       chain (0:dutpou:%subdt(datacural:*years):
030400140620                        %subdt(datacural:*MONTHS)) azcln01l;
030500140620          if %found(azcln01l) and mat(%subdt(datacural:*days)) = 'F'
030600140620                               or pom(%subdt(datacural:*days)) = 'F';
030700140620       // data festiva: la incremento di un giorno
030800140620             datacural=datacural+%days(1);
030900140620          else;
031000140620            clear wfes;
031100140620          endif;
031200140620       enddo;
031300140620       endsr;
031400140623       //--------------------------------------------------------------
031500140623       //?Controllo data se festiva + restituzione data prima data non festiva
031600140623       //--------------------------------------------------------------
031700140623       BEGSR  sr_DataFesdal;
031800140623       wfes='1';
031900140623       dow wfes='1';
032000140623       chain (0:dutpou:%subdt(datacurdal:*years):
032100140623                        %subdt(datacurdal:*MONTHS)) azcln01l;
032200140623          if %found(azcln01l) and mat(%subdt(datacurdal:*days)) = 'F'
032300140623                               or pom(%subdt(datacurdal:*days)) = 'F';
032400140623       // data festiva: la decremento di un giorno
032500140623             datacurdal=datacurdal-%days(1);
032600140623          else;
032700140623            clear wfes;
032800140623          endif;
032900140623       enddo;
033000140623       endsr;
033100040108
033200090402       //-------------------------------------------------------------*
033300090402       //Reperimento Dati del job (Utente/Operativi)                  *
033400090402       //-------------------------------------------------------------*
033500090918       Begsr DatiJob;
033600090403
033700090917          in(e) §azute;
033800090917          if not %error;
033900090917           in(e) §datiute;
034000090917          endif;
034100090917          if %error or rsut = *blanks;
034200090917           tibs34r(tibs34ds);
034300090917           in §azute;
034400090917           in §datiute;
034500090917          endif;
034600111205
034700090917         endsr;
034800040108
034900111205**
035000130515Data mancante o errata                                                        01
035100130516Data al minore di data dal                                                    02
035200130516Intervallo di tempo non corretto: correggere gli orari                        03
035300130516Non è possibile richiedere un periodo superiore di                            04
