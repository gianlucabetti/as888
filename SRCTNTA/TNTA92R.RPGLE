000100060317     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060317
000300060317      *------------------------------------------------------------------------*
000400060317      *
000500081007      *  Elenco  Tariffe  Inutilizzate
000600060317      *
000700060317      *------------------------------------------------------------------------*
000800060317
000900060320     fazorg01l  if   e           k disk
001000060320     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001100081007     ftabel00f  if   e           k disk
001200060927     ftnta92d   cf   e             workstn
001300060317
001400071126     ddsind            ds                  based(punt1)
001500071126     Dend                             1N   OVERLAY(DSIND:3)
001600071126     Dconferma                        1N   OVERLAY(DSIND:6)
001700060317      *------------------------------------------------------------------------*
001800060317      *  RIEPILOGO INDICATORI
001900060317      *------------------------------------------------------------------------*
002000060320      * 08 - utente non abilitato
002100060320      * 12 - utente di sede
002200060320      * 28 - errore generico
002300060320      * 30 - comodo
002400060320      * 40 - errore p.o.
002500060927      * 42 - Data errata
002600071126      * 43 - Data errata
002700060320      * 46 - autorizzazione distretto
002800080709      * 47/96 - errore sulle aree
002900060317      *------------------------------------------------------------------------*
003000060317
003100060317      *------------------------------------------------------------------------*
003200060317      *   V A R I A B I L I
003300060317      *------------------------------------------------------------------------*
003400060320     d wabi            s                   like(uteaut)
003500060320     d w003a           s              3
003600071126     d xx              s              3  0
003700060320     d yy              s              3  0
003800081007     d savdna          s              8  0
003900081007     D savdus          s              8  0
004000081007     D savdusdmy       s              8  0
004100071128
004200081007     d Dataiso         s               d   datfmt(*iso)
004300081007     d Dataeur         s               d   datfmt(*eur)
004400060317
004500060317      *------------------------------------------------------------------------*
004600060317      *   S C H I E R E
004700060317      *------------------------------------------------------------------------*
004800080708     d car             s              3    dim(50)
004900060320     d msg             s             78    dim(10) ctdata perrcd(1)
005000071128     d skpog           s              3  0 dim(250) inz(*zeros)
005100080708     d skcar           s              3  0 dim(50) inz(*zeros)
005200060317
005300060317      *------------------------------------------------------------------------*
005400060317      *   D S   I N T E R N E / E S T E R N E
005500060317      *------------------------------------------------------------------------*
005600060317     d parm01          ds
005700060321     d  parfil                 1      3  0
005800080708     d  parcar                 4    153    dim(50)
005900080708     d  pardus               154    161  0
006000080708     d  pardna               162    169  0
006100080708     d  parfile              170    170
006200080708     d  parabi               171    172
006300080710     d  parcrf               173    173
006400060320
006500060320     d ds_car          ds
006600060320     d  v1car1
006700060320     d  v1car2
006800060320     d  v1car3
006900060320     d  v1car4
007000060320     d  v1car5
007100060320     d  v1car6
007200060320     d  v1car7
007300060320     d  v1car8
007400060320     d  v1car9
007500060320     d  v1car0
007600071126     d  v1ca11
007700071126     d  v1ca12
007800071126     d  v1ca13
007900071126     d  v1ca14
008000071126     d  v1ca15
008100071126     d  v1ca16
008200071126     d  v1ca17
008300071126     d  v1ca18
008400071126     d  v1ca19
008500071126     d  v1ca20
008600071126     d  v1ca21
008700071126     d  v1ca22
008800071126     d  v1ca23
008900071126     d  v1ca24
009000071126     d  v1ca25
009100071126     d  v1ca26
009200071126     d  v1ca27
009300071126     d  v1ca28
009400071126     d  v1ca29
009500071126     d  v1ca30
009600071126     d  v1ca31
009700071126     d  v1ca32
009800071126     d  v1ca33
009900071126     d  v1ca34
010000071126     d  v1ca35
010100071126     d  v1ca36
010200071126     d  v1ca37
010300071126     d  v1ca38
010400071126     d  v1ca39
010500071126     d  v1ca40
010600080708     d  v1ca41
010700080708     d  v1ca42
010800080708     d  v1ca43
010900080708     d  v1ca44
011000080708     d  v1ca45
011100080708     d  v1ca46
011200080708     d  v1ca47
011300080708     d  v1ca48
011400080708     d  v1ca49
011500080708     d  v1ca50
011600080708     d  area_video                    3a   dim(50) overlay(ds_car)
011700060927      * Parametri per controllo data e transform in giorni - XSRDA8 -
011800060927     d WLBdat          ds
011900060927     d  G02dat                        8  0
012000060927     d  G02inv                        8  0
012100060927     d  G02err                        1
012200060927     d  G02tgi                        5  0
012300060317
012400071127     d §azute        e ds                  extname(azute00f)
012500071127     d                                     dtaara
012600071127     d §datiute      e ds                  extname(ddatiute)
012700071127     d                                     dtaara
012800081007     d ds$3          e ds
012900060320     d dlat          e ds
013000060320     d dute01        e ds
013100060317     d kpjba         e ds
013200060320     d tibs02ds      e ds
013300060317     d tibs34ds      e ds
013400060320     d trul31ds      e ds
013500071128     d  PO_abilitati                  3a   dim(250) overlay(o31pog)
013600080708     d trul31ds2     e ds
013700080708     d  Aree_abilita                  3a   dim(050) overlay(o31arg)
013800060317
013900060317     d                sds
014000060317     d  vtcpgm                 1     10
014100071127
014200071127      *------------------------------------------------------------------------*
014300071127      * pgm richiamati
014400071127      *------------------------------------------------------------------------*
014500071127     d tibs34r         pr                  extpgm('TIBS34R')
014600071127     d  tibs34ds                           likeds(tibs34ds)
014700071127
014800071127     d tibs02r         pr                  extpgm('TIBS02R')
014900071127     d  kpjba                              likeds(kpjba)
015000071127     d  tibs02ds                           likeds(tibs02ds)
015100071127
015200071128     d trul31r         pr                  extpgm('TRUL31R')
015300071127     d  kpjba                              likeds(kpjba)
015400071128     d  trul31ds                           likeds(trul31ds)
015500080708     d  trul31ds2                          likeds(trul31ds2)
015600071127
015700071127     d xsrda8          pr                  extpgm('XSRDA8')
015800071127     d  wlbdat                             likeds(wlbdat)
015900071127
016000071127     d bch10           pr                  extpgm('BCH10')
016100071127     d  kpjba                              likeds(kpjba)
016200071127
016300080707     d tnta93r         pr                  extpgm('TNTA93R')
016400080707     d  kpjba                              likeds(kpjba)
016500080707
016600081007      //---------------------------------------------------------------
016700081007      //?Definizione key-list.
016800081007      //---------------------------------------------------------------
016900081007
017000081007      // - File TABEL00F
017100081007     d k03tabel      e ds                  extname(TABEL00F:*key)
017200050408      *------------------------------------------------------------------------*
017300071126      /free
017400071126             punt1 = %addr(*in) ;
017500071126                                      // pulizia videata //
017600071126             exsr sr_pulvid ;
017700071126                                      // emetto la videata //
017800071126             exfmt ta92d01;
017900071126         Dow not end ;
018000071126             *in28 = *off ;
018100071126                                      // controlli videata //
018200071126             exsr sr_contr;
018300071126                                      // 28 acceso errore //
018400071126             if not *in28 and conferma;
018500071126                exsr sr_conferma;
018600071126                leave;
018700071126             endif;
018800071126
018900071126             exfmt ta92d01;
019000071126         enddo;
019100050408
019200071126         *inlr = *on;
019300060320
019400071128       // ---------------- Routine di pulizia del video --------------- //
019500071126         begsr sr_pulvid ;
019600050408
019700071126            v1cfil= 0;
019800071126            v1dfil= 'Tutti';
019900071128            clear ds_car ;
020000060320
020100071126            select;
020200071128       // se abilitazione distretto faccio vedere le aree gestite  //
020300071126            when wabi = 'DI' or wabi = 'AZ';
020400071126            exsr sr_cararee;
020500071126            *in46 = *on;
020600060320
020700071128       // imposto il p.o. da elaborare //
020800071126            when  not *in12 ;
020900071126            v1cfil = dutpou;
021000071126            chain v1cfil azorg01l ;
021100071126            If  %found(azorg01l) and orgfva = *blanks ;
021200071126                v1dfil = orgdes;
021300071126            endif;
021400071126
021500071126            endsl;
021600060320
021700071126         endsr;
021800060320
021900071126
022000071128       // ---- Routine di caricamento aree in gestione ---- //
022100071126         begsr sr_cararee ;
022200071126
022300071126         // valorizzo i campi del video //
022400080709         Area_video = %editc(skcar:'Z');
022500071126
022600071126         endsr;
022700071126
022800071126
022900071128       // ---- Routine di controllo del video ---- //
023000071126         begsr sr_contr ;
023100060320
023200071128       // se utente non abilitato riemetto la videata con l'errore  //
023300071126           if  *in08 ;
023400071126              *in28 = *on ;
023500071126              leavesr;
023600071126           endif;
023700071128       // reset indicatori //
023800071127           *in40 = *off;
023900071127           *in42 = *off;
024000080709           For xx = 47 to 96 ;
024100071127             *in(xx) = *off ;
024200071127           endfor ;
024300071128       // verifico il p.o. se immesso //
024400071127           If  v1cfil > 0 ;
024500071128       // verifico se è valido //
024600071127              chain v1cfil azorg01l ;
024700071127              If not %found(azorg01l) or orgfva <> *blanks ;
024800071127                   *in28 = *on ;
024900071127                   *in40 = *on ;
025000071127                   v1cmsg = msg(03);
025100071127                   leavesr;
025200071127              endif;
025300071128       // verifico se è una filiale valida //
025400071127              if  orgfag <> 'A' and orgfag <> 'F';
025500071127                   *in28 = *on;
025600071127                   *in40 = *on;
025700071127                   v1cmsg = msg(03);
025800071127                   leavesr;
025900071127              endif;
026000071128       // controllo se il p.o. è in gestione //
026100071128              if %lookup(v1cfil:skpog) = 0;
026200071127                   *in28 = *on;
026300071127                   *in40 = *on;
026400071127                   v1cmsg = msg(02);
026500071127                   leavesr;
026600071127              endif;
026700071127              v1dfil = orgdes;
026800071127           else;
026900071127              v1dfil ='Tutti';
027000071127           endif;
027100060320
027200071128       // se immesso il p.o. non posso immettere le aree e viceversa //
027300071127           if v1cfil <> *zeros and ds_car <> *blanks;
027400071127              *in28 = *on ;
027500071127              *in40 = *on ;
027600071127              *in47 = *on ;
027700071127              v1cmsg = msg(04) ;
027800071127              leavesr;
027900071127           endif;
028000060320
028100071128       // controllo le aree immesse //
028200071128             parcar = area_video;
028300071128       // Ricerca //
028400080709           For xx = 1 to 50 ;
028500071127               yy = (46 + xx);
028600071127               if parcar(xx) <> *blanks ;
028700080709                  if %lookup(parcar(xx):aree_abilita) = 0;
028800071127                     v1cmsg = msg(05);
028900071127                     *in(yy) = *on;
029000071127                     *in28 = *on;
029100071127                     leavesr;
029200071127                  endif;
029300071127               endif;
029400071127           endfor;
029500060320
029600071128           area_video = parcar;
029700060927
029800071128       // controllo data ultima spedizione presente su titas //
029900050408
030000071127           clear wlbdat;
030100071127           g02dat = v1cdus;
030200071127           g02err = ' ';
030300071127           xsrda8(wlbdat);
030400071127           if  g02err = '1';
030500071127             v1cmsg = msg(6);
030600071127             *in42 = *on ;
030700071127             *in28 = *on ;
030800071127           endif;
030900081007        // controllo se inserita data ultima spedizione minore della data //
031000081007        // trovata in tabella $3 o se maggiore o guale ad oggi  //
031100081007           If  g02inv < savdus ;
031200081007             v1cmsg = (%subst(msg(7): 1: 50)+ %editw(savdusdmy: '  /  /    '));
031300081007             *in42 = *on ;
031400081007             *in28 = *on ;
031500081007           endif;
031600081007           If  g02inv > %dec(%date) ;
031700081007             v1cmsg = msg(8);
031800081007             *in42 = *on ;
031900081007             *in28 = *on ;
032000081007           endif;
032100071127           v1cdus = g02dat;
032200071127           pardus = g02inv;
032300071127
032400071128       // controllo data acquisizione clienti  //
032500071127
032600071127           clear wlbdat;
032700071128           g02dat = v1cdna;
032800071127           g02err = ' ';
032900071127           xsrda8(wlbdat);
033000071127           if  g02err = '1';
033100071127             v1cmsg = msg(6);
033200071127             *in43 = *on ;
033300071127             *in28 = *on ;
033400071127           endif;
033500081007        // controllo se la data nascita del codice è maggiore di oggi //
033600081007           If  g02inv > *date ;
033700081007             v1cmsg = msg(8);
033800081007             *in43 = *on ;
033900081007             *in28 = *on ;
034000081007           endif;
034100071128           v1cdna = g02dat;
034200071128           pardna = g02inv;
034300071127
034400071127       endsr;
034500060320
034600071128       // ---- Routine di conferma e richiamo pgm batch ---- //
034700071127         begsr sr_conferma ;
034800071127
034900071127           parfil = v1cfil;
035000071127           parfile = 'F';
035100071127           parabi = wabi;
035200080710           parcrf = v1ccrf;
035300071127           kpjbu = parm01;
035400080711         // se sono i sede modifico la coda di input //
035500080711           If *in12 ;
035600080711           kqieb = 'KEDPCED';
035700080711           else ;
035800080711           kqieb = *blanks ;
035900080711           endif ;
036000080711        // tnta93r(kpjba);  //
036100080711           kcoaz = 'TA93';
036200080711           bch10(Kpjba);
036300050408
036400071127         endsr;
036500071127
036600071127       // ----- Routine iniziale ----- //
036700071127         begsr *inzsr;
036800071127
036900071127      /end-free
037000050408
037100060320     c     *entry        plist
037200060320     c                   parm                    kpjba
037300050510
037400071127
037500071127      /free
037600071127         in(e) §azute;
037700071127         if not %error;
037800071127          in(e) §datiute;
037900071127         endif;
038000071127         if %error or rsut = *blanks;
038100071128          clear tibs34ds;
038200071127          tibs34r(tibs34ds);
038300071127          in §azute;
038400071127          in §datiute;
038500071127         endif;
038600071127
038700071127         clear wabi;
038800071127         clear dlat;
038900060320
039000071128       // verifica errori e autorità profilo //
039100071128         select;
039200071128       // se ho errori nei dati utente esco dal pgm //
039300071128         when duterr = 'E' ;
039400071127           *in08 = *on;
039500071127           *in28 = *on;
039600071127           v1cmsg = msg(01);
039700071127           leavesr;
039800071128       //  se non c'è l'abilitazione                                     //
039900071128       //  * --> se 1° livello, abilitazioni al terminal                 //
040000071128       //  *     se 2° livello, abilitazioni al punto operativo          //
040100071128       //  *     se sede è impossibile ma se capita mando a fine il pgm  //
040200071128         when uteaut = *Blanks;
040300071128           if dutlpo = '1' ;
040400071127              wabi   = 'TP';
040500071128           endif;
040600071128           if dutlpo = '2';
040700071127              wabi   = 'PO';
040800071128           endif;
040900071128           if dutlpo = 'S';
041000071127              wabi   = 'AZ';
041100071128           endif ;
041200071128       //  carica le abilitazioni del profilo  //
041300071128         other;
041400071127           dute01 = utefaf;
041500071128           if §utegtc <> *Blanks ;
041600071127              wabi = §utegtc;
041700071128           else;
041800071127              wabi = uteaut;
041900071128           endif;
042000071128         endsl;
042100060320
042200071128       //  controllo se ok l'abilitazione dell'utente //
042300071127         clear tibs02ds;
042400071127         t02mod = 'C';
042500071127         t02sif = knsif;
042600071127         t02cod = 'LAT';
042700071127         t02ke1 = wabi;
042800071127         tibs02r(kpjba:tibs02ds);
042900071127         if t02err = *blanks;
043000071127            dlat = t02uni;
043100071127         endif;
043200071128       //  errore o non abilitato //
043300071127         if t02err <> *blanks or §latabi = 'S';
043400071127            *in08 = *on;
043500071127            *in28 = *on;
043600071127            v1cmsg = msg(01) ;
043700071127            leavesr;
043800071127         endif;
043900060320
044000080708       //  reperimento dei p.o. e dell'aree gestibili dall'utente //
044100071127         clear trul31ds;
044200071127         i31abi = wabi ;
044300071127         i31cdi = dutdis;
044400071127         i31car = dutare;
044500071127         i31cpo = dutpou;
044600080708         trul31r(kpjba:trul31ds:trul31ds2);
044700071128         if o31pog = *zeros ;
044800071128            *in08 = *on;
044900071128            *in28 = *on;
045000071128            v1cmsg = msg(01);
045100071128            leavesr;
045200071128         else;
045300071128            skpog = %dec(PO_abilitati:3:0) ;
045400080708            skcar = %dec(Aree_abilita:3:0) ;
045500071128         endif;
045600060320
045700071128         *in12 = (simfel = *zeros);
045800080710
045900080710        // imposto campo creazione file //
046000080710         v1ccrf = 'N' ;
046100071128
046200071128        // imposto le date di controllo //
046300081007        // recupero la data ultima spedizione dalla tabella $3 //
046400081007         tblkut = 1 ;
046500081007         tblcod = '$3' ;
046600081007         tblkey = '1       ';
046700081007         chain  %kds(k03tabel : 3) tabel00f;
046800081007         if   %found(TABEL00F);
046900081007         ds$3 = tbluni;
047000081007         else ;
047100081007         clear ds$3 ;
047200081007         endif ;
047300081007         if  §$3dp0 > 0 ;
047400081007         savdus = §$3dp0 ;
047500081007         Dataiso=%date(§$3dp0:*iso)  ;
047600081007         dataeur=Dataiso ;
047700081007         v1cdus=%dec(dataeur)  ;
047800081007         savdusdmy = v1cdus ;
047900081022         endif ;
048000081022         v1cdus  = %dec(%date - %months(6): *eur);
048100081022         v1cdna  = %dec(%date - %months(3): *eur);
048200081022         savdna = %dec(%date - %months(3));
048300050408
048400071128       endsr;
048500071128       /end-free
048600050408      *
048700060320** MSG  Lungh. 78                                                            *
048800060320Utente non autorizzato alla funzione                                          01
048900080708Filiale non in gestione all'utente                                            02
049000080708Filiale errata                                                                03
049100080709Filiale valorizzata non compatibile con aree selezionate                      04
049200060320Codice Area non in gestione all'utente                                        05
049300060927Data Errata                                                                   06
049400081007Data ultima spedizione non deve essere minore di                              07
049500081007La data non può essere maggiore di oggi                                       08
