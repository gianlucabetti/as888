000100110620     /*PRM dbgview(*source)
001000040107      *---------------------------------------------------------------*
001100130517      * ELENCO TRAZIONI in ARRIVO                   - FILTRO -        *
001200040107      *---------------------------------------------------------------*
001300040107
001400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001600040107
001700040107      *---------------------------------------------------------------*
001800040107
001801130516     ftabel00f  if   e           k disk
001802130517     fazorg01l  if   e           k disk
002001130515     ffnlst7d   cf   E             workstn
002700040112      *
002800040107      *---------------------------------------------------------------*
003400040107      *
003500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040107      *
003703130516     d err             s             79    dim(4) ctdata perrcd(1)
004700040107      *
004800040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004900040107      *
004901130515     d fnlst8ds      e ds
004902130515     d Tibs34ds      e ds                  inz
004903130515     d §azute        e ds                  Extname(Azute00f)
004904130515     d                                     dtaara
004905130515     d §Datiute      e ds                  EXTname(ddatiute)
004906130515     d                                     dtaara
006100090828
006200040107     d KPJBA         e ds
006201130516     d ds1x          e ds
008200040123      *
008900040109      *
009200090409
009300040107      *
009400040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
011000040109      *
011100040109      * - Campi di comodo
011101130516     d num_days        s             15p 0
011102130516     D w0060           S              6  0
011202130516     d vidddainv       s                   like(viddda)
011203130516     d viddalinv       s                   like(viddal)
011204130516     D wkey            S                   like(tblkey)
011205130516     d time            s               t
011206111205     d dataiso         s               d   datfmt(*iso)
011207130516     d dataisoda       s               d   datfmt(*iso)
011208130516     d dataisoa        s               d   datfmt(*iso)
011209111205     d dataeur         s               d   datfmt(*eur)
011210111205     d datacur         s               d   datfmt(*iso)
011211130516     d datacurdal      s               d   datfmt(*iso)
011212130516     d datacural       s               d   datfmt(*iso)
011213130516     d timecur         s              6  0
011214111205     d flerr           s              1
011215111202      // - Campi associati al video
011216111206     d $Video          s              2    inz('D1')
018500090828      *
018600090828      * - Flags booleani
018700090828     d $Finerec        s              1    inz(*off)
018701111202     d $Fine           s               n   inz(*off)
018702111207     d $End            s               n   inz(*off)
018703111202     d $InzD01         s               n   inz(*on)
018705111205
018800081023
018801130515     d bch10           pr                  extpgm('BCH10')
018802130515     d  kpjba                              likeds(kpjba)
018803130515
018900090402      // ? PROTOTIPI ?
019000090402      /copy gaitrasrc/srcprotopr,tibs34r
019001111206
019500040107
019600040107      *===============================================================*
019700040107      *  RIEPILOGO INDICATORI                                         *
019800040107      *---------------------------------------------------------------*
019900090827      *  XX    -
020000040107      *===============================================================*
020100040107
020200040107     c     *Entry        plist
020300040107     c                   parm                    KPJBA
020400090827     c
020600040123      *
020700090331      /free
020801111202       //?Gestione video
020802111202       DOW $Fine = *off;
020805111202             exsr GesD01;
020813111202       ENDDO;
020900090610
026100090827       eval *inlr=*on;
026101111206       //-------------------------------------------------------------*
026102111206       //                                                             *
026103111206       //-------------------------------------------------------------*
026104111206       Begsr Gesd01;
026105111206       // Inizializzazione videata
026106111206          if $inzd01;
026107130516              exsr sr_inzd01;
026108130516              $InzD01  = *off;
026125111206          endif;
026126130515             exfmt lst7d01;
026127111206             clear vidmsg;
026128111206             *in28=*off;
026129111206             *in90=*off;
026130111206             select;
026131111206             // F3=Fine
026132111206                when *inkc;
026133111206                   $fine=*on;
026134111206                   leavesr;
026135111206                other;
026136111206                   exsr ctrd01;
026137111206                   if *in90;
026138111206                      leavesr;
026139111206                   endif;
026140130516                   if *inkf;
026141130515                      clear fnlst8ds;
026142130516                      lst8dal=vidddainv;
026143130516                      lst8al=viddalinv;
026144130516                      lst8hdl=vidhda;
026145130516                      lst8hal=vidhal;
026146130515                      kpjbu=fnlst8ds;
026147130515                      kcoaz='LST8';
026148130515                      bch10(kpjba);
026149130515                      $fine=*on;
026150130515                      leavesr;
026151111206                   endif;
026152111206             endsl;
026153111206       endsr;
026154130516       //-------------------------------------------------------------*
026155130516       // Inizializzazione prima videata                              *
026156130516       //-------------------------------------------------------------*
026157130516       Begsr sr_inzD01;
026158130517         vidtfa=dutpou;
026159130517         clear viddfa;
026160130517         chain (dutpou) azorg01l;
026161130517         if %found(azorg01l);
026162130517            viddfa=orgdes ;
026163130517         endif;
026164130516         clear vidmsg;
026165130516         clear viddda;
026166130516         clear vidhda;
026167130516         clear viddal;
026168130516         clear vidhal;
026169130516         viddda=%dec(datacurdal:*eur);
026170130516         viddal=%dec(datacural:*eur);
026171130516       // ora al = ora di soglia presa da tabella
026172130516         vidhal=§1xhho;
026173130516       // ora dal = ora di soglia presa da tabella + 1 minuto
026174130516         time=%time((vidhal*100):*hms)+%minutes(1) ;
026175130516         w0060=%dec(time)      ;
026176130516         vidhda=%dec(%subst(%editc(w0060:'X'):1:4):4:0);
026177130516         *in28=*off;
026178130516           *in40=*off;
026179130516           *in41=*off;
026180130516           *in42=*off;
026181130516       endsr;
026182111206       //-------------------------------------------------------------*
026183111206       // Controllo prima videata                                     *
026184111206       //-------------------------------------------------------------*
026185111206       Begsr ctrd01  ;
026186130516
026187130516       *in40=*off;
026188130516       *in41=*off;
026189130516       *in42=*off;
026190130516
026191130516       // data dal
026192130516
026193130516       if %subst(%editc(viddda:'X'):1:2)='00';
026194130516          test(DE) *dmy viddda;
026195120102          if %error;
026196120102             *in90=*on;
026197130516             *in40=*on;
026198120102             *in28=*on;
026199120102             vidmsg=err(1);
026200120102             leavesr;
026201120102          else;
026202130516             dataiso=%date(viddda:*dmy);
026203120102          endif;
026204120102       else;
026205130516          test(DE) *eur viddda;
026206120102          if %error;
026207120102             *in90=*on;
026208130516             *in40=*on;
026209120102             *in28=*on;
026210120102             vidmsg=err(1);
026211120102             leavesr;
026212120102          else;
026213130516             dataiso=%date(viddda:*eur);
026214120102          endif;
026215120102       endif;
026216111206          dataeur=dataiso;
026217130516          viddda=%dec(dataeur);
026218130516          vidddainv=%dec(dataiso);
026219130516
026220130516       // data al
026221130516
026222130516       if %subst(%editc(viddal:'X'):1:2)='00';
026223130516          test(DE) *dmy viddal;
026224130516          if %error;
026225130516             *in90=*on;
026226130516             *in42=*on;
026227130516             *in28=*on;
026228130516             vidmsg=err(1);
026229130516             leavesr;
026230130516          else;
026231130516             dataiso=%date(viddal:*dmy);
026232130516          endif;
026233130516       else;
026234130516          test(DE) *eur viddal;
026235130516          if %error;
026236130516             *in90=*on;
026237130516             *in42=*on;
026238130516             *in28=*on;
026239130516             vidmsg=err(1);
026240130516             leavesr;
026241130516          else;
026242130516             dataiso=%date(viddal:*eur);
026243130516          endif;
026244130516       endif;
026245130516          dataeur=dataiso;
026246130516          viddal=%dec(dataeur);
026247130516          viddalinv=%dec(dataiso);
026248130516
026249130516       // data/ora dal no maggiore di data/ora al
026250130516          if vidddainv>viddalinv;
026251130516             *in90=*on;
026252130516             *in40=*on;
026253130516             *in28=*on;
026254130516             vidmsg=err(2);
026255130516             leavesr;
026256130516          endif;
026257130516       // se data dal=data al allora ora dal no maggiore di ora al
026258130516          if vidddainv=viddalinv and vidhda>vidhal;
026259130516             *in90=*on;
026260130516             *in41=*on;
026261130516             *in28=*on;
026262130516             vidmsg=err(3);
026263130516             leavesr;
026264130516          endif;
026265130516       // il range tra le due date non deve superare 7gg
026266130516           dataisoda=%date(vidddainv:*iso);
026267130516           dataisoa=%date(viddalinv:*iso);
026268130516           num_days=%diff(dataisoa: dataisoda: *days);
026269130516           num_days+=1;
026270130516           if num_days>7;
026271130516             *in90=*on;
026272130516             *in40=*on;
026273130516             *in28=*on;
026274130516             vidmsg=err(4);
026275130516             vidmsg=%trim(vidmsg) + ' sette giorni';
026276130516             leavesr;
026277130516           endif;
026278111206
026279111206       endsr;
026414090402       //-------------------------------------------------------------*
026415090402       //Routine Iniziale                                             *
026500090402       //-------------------------------------------------------------*
026600090414       begsr *inzsr;
026700090402
026800090402       //Reperisco dati job
026900090917         exsr datijob;
026901130517
026912130516
026913130516       // Leggo tabella "1X" per recuperare l'ora di soglia trazioni
026914130516       clear ds1x;
026915130517       wkey=%editc(dutpou:'X')+'OST '     ;
026916130516       chain (1:'1X': wkey) tabel00f  ;
026917130517  1    if not %found(tabel00f) or tblflg<>' '  ;
026918130516          chain (1:'1X': '   OST ') tabel00f  ;
026919130516  1    endif     ;
026920130516       ds1x=tbluni             ;
026921130517
026922130517       // Reperisco la data corrente
026923130517          datacur=(%date());
026924130517          timecur=%dec(%time());
026925130517
026926130517          if timecur>§1xhho*100;
026927130517             datacural=datacur +%days(1);
026928130517             datacurdal=datacur;
026929130517          else;
026930130517             datacural=datacur;
026931130517             datacurdal=datacur -%days(1);
026932130517          endif;
030300090402
030400090402       EndSr;
030500040108
034400090402       //-------------------------------------------------------------*
034500090402       //Reperimento Dati del job (Utente/Operativi)                  *
034600090402       //-------------------------------------------------------------*
034700090918       Begsr DatiJob;
034800090403
034900090917          in(e) §azute;
035000090917          if not %error;
035100090917           in(e) §datiute;
035200090917          endif;
035300090917          if %error or rsut = *blanks;
035400090917           tibs34r(tibs34ds);
035500090917           in §azute;
035600090917           in §datiute;
035700090917          endif;
035701111205
036000090917         endsr;
036100040108
155300111205**
155500130515Data mancante o errata                                                        01
155600130516Data al minore di data dal                                                    02
155700130516Intervallo di tempo non corretto: correggere gli orari                        03
155800130516Non è possibile richiedere un periodo superiore di                            04
