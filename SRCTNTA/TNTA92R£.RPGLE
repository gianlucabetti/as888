000100071128     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200060317     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000300060317
000400060317      *------------------------------------------------------------------------*
000500060317      *
000600081007      *  Elenco  Tariffe  Inutilizzate
000700060317      *
000800060317      *------------------------------------------------------------------------*
000900060317
001000060320     fazorg01l  if   e           k disk
001100060320     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001200081007     ftabel00f  if   e           k disk
001300060927     ftnta92d   cf   e             workstn
001400060317
001500071126     ddsind            ds                  based(punt1)
001600071126     Dend                             1N   OVERLAY(DSIND:3)
001700071126     Dconferma                        1N   OVERLAY(DSIND:6)
001800060317      *------------------------------------------------------------------------*
001900060317      *  RIEPILOGO INDICATORI
002000060317      *------------------------------------------------------------------------*
002100060320      * 08 - utente non abilitato
002200060320      * 12 - utente di sede
002300060320      * 28 - errore generico
002400060320      * 30 - comodo
002500060320      * 40 - errore p.o.
002600060927      * 42 - Data errata
002700071126      * 43 - Data errata
002800060320      * 46 - autorizzazione distretto
002900080709      * 47/96 - errore sulle aree
003000060317      *------------------------------------------------------------------------*
003100060317
003200060317      *------------------------------------------------------------------------*
003300060317      *   V A R I A B I L I
003400060317      *------------------------------------------------------------------------*
003500060320     d wabi            s                   like(uteaut)
003600060320     d w003a           s              3
003700071126     d xx              s              3  0
003800060320     d yy              s              3  0
003900081007     d savdna          s              8  0
004000081007     D savdus          s              8  0
004100081007     D savdusdmy       s              8  0
004200071128
004300081007     d Dataiso         s               d   datfmt(*iso)
004400081007     d Dataeur         s               d   datfmt(*eur)
004500060317
004600060317      *------------------------------------------------------------------------*
004700060317      *   S C H I E R E
004800060317      *------------------------------------------------------------------------*
004900080708     d car             s              3    dim(50)
005000060320     d msg             s             78    dim(10) ctdata perrcd(1)
005100071128     d skpog           s              3  0 dim(250) inz(*zeros)
005200080708     d skcar           s              3  0 dim(50) inz(*zeros)
005300060317
005400060317      *------------------------------------------------------------------------*
005500060317      *   D S   I N T E R N E / E S T E R N E
005600060317      *------------------------------------------------------------------------*
005700060317     d parm01          ds
005800060321     d  parfil                 1      3  0
005900080708     d  parcar                 4    153    dim(50)
006000080708     d  pardus               154    161  0
006100080708     d  pardna               162    169  0
006200080708     d  parfile              170    170
006300080708     d  parabi               171    172
006400080710     d  parcrf               173    173
006500060320
006600060320     d ds_car          ds
006700060320     d  v1car1
006800060320     d  v1car2
006900060320     d  v1car3
007000060320     d  v1car4
007100060320     d  v1car5
007200060320     d  v1car6
007300060320     d  v1car7
007400060320     d  v1car8
007500060320     d  v1car9
007600060320     d  v1car0
007700071126     d  v1ca11
007800071126     d  v1ca12
007900071126     d  v1ca13
008000071126     d  v1ca14
008100071126     d  v1ca15
008200071126     d  v1ca16
008300071126     d  v1ca17
008400071126     d  v1ca18
008500071126     d  v1ca19
008600071126     d  v1ca20
008700071126     d  v1ca21
008800071126     d  v1ca22
008900071126     d  v1ca23
009000071126     d  v1ca24
009100071126     d  v1ca25
009200071126     d  v1ca26
009300071126     d  v1ca27
009400071126     d  v1ca28
009500071126     d  v1ca29
009600071126     d  v1ca30
009700071126     d  v1ca31
009800071126     d  v1ca32
009900071126     d  v1ca33
010000071126     d  v1ca34
010100071126     d  v1ca35
010200071126     d  v1ca36
010300071126     d  v1ca37
010400071126     d  v1ca38
010500071126     d  v1ca39
010600071126     d  v1ca40
010700080708     d  v1ca41
010800080708     d  v1ca42
010900080708     d  v1ca43
011000080708     d  v1ca44
011100080708     d  v1ca45
011200080708     d  v1ca46
011300080708     d  v1ca47
011400080708     d  v1ca48
011500080708     d  v1ca49
011600080708     d  v1ca50
011700080708     d  area_video                    3a   dim(50) overlay(ds_car)
011800060927      * Parametri per controllo data e transform in giorni - XSRDA8 -
011900060927     d WLBdat          ds
012000060927     d  G02dat                        8  0
012100060927     d  G02inv                        8  0
012200060927     d  G02err                        1
012300060927     d  G02tgi                        5  0
012400060317
012500071127     d §azute        e ds                  extname(azute00f)
012600071127     d                                     dtaara
012700071127     d §datiute      e ds                  extname(ddatiute)
012800071127     d                                     dtaara
012900081007     d ds$3          e ds
013000060320     d dlat          e ds
013100060320     d dute01        e ds
013200060317     d kpjba         e ds
013300060320     d tibs02ds      e ds
013400060317     d tibs34ds      e ds
013500060320     d trul31ds      e ds
013600071128     d  PO_abilitati                  3a   dim(250) overlay(o31pog)
013700080708     d trul31ds2     e ds
013800080708     d  Aree_abilita                  3a   dim(050) overlay(o31arg)
013900060317
014000060317     d                sds
014100060317     d  vtcpgm                 1     10
014200071127
014300071127      *------------------------------------------------------------------------*
014400071127      * pgm richiamati
014500071127      *------------------------------------------------------------------------*
014600071127     d tibs34r         pr                  extpgm('TIBS34R')
014700071127     d  tibs34ds                           likeds(tibs34ds)
014800071127
014900071127     d tibs02r         pr                  extpgm('TIBS02R')
015000071127     d  kpjba                              likeds(kpjba)
015100071127     d  tibs02ds                           likeds(tibs02ds)
015200071127
015300071128     d trul31r         pr                  extpgm('TRUL31R')
015400071127     d  kpjba                              likeds(kpjba)
015500071128     d  trul31ds                           likeds(trul31ds)
015600080708     d  trul31ds2                          likeds(trul31ds2)
015700071127
015800071127     d xsrda8          pr                  extpgm('XSRDA8')
015900071127     d  wlbdat                             likeds(wlbdat)
016000071127
016100071127     d bch10           pr                  extpgm('BCH10')
016200071127     d  kpjba                              likeds(kpjba)
016300071127
016400080707     d tnta93r         pr                  extpgm('TNTA93R')
016500080707     d  kpjba                              likeds(kpjba)
016600080707
016700081007      //---------------------------------------------------------------
016800081007      //?Definizione key-list.
016900081007      //---------------------------------------------------------------
017000081007
017100081007      // - File TABEL00F
017200081007     d k03tabel      e ds                  extname(TABEL00F:*key)
017300050408      *------------------------------------------------------------------------*
017400071126      /free
017500071126             punt1 = %addr(*in) ;
017600071126                                      // pulizia videata //
017700071126             exsr sr_pulvid ;
017800071126                                      // emetto la videata //
017900071126             exfmt ta92d01;
018000071126         Dow not end ;
018100071126             *in28 = *off ;
018200071126                                      // controlli videata //
018300071126             exsr sr_contr;
018400071126                                      // 28 acceso errore //
018500071126             if not *in28 and conferma;
018600071126                exsr sr_conferma;
018700071126                leave;
018800071126             endif;
018900071126
019000071126             exfmt ta92d01;
019100071126         enddo;
019200050408
019300071126         *inlr = *on;
019400060320
019500071128       // ---------------- Routine di pulizia del video --------------- //
019600071126         begsr sr_pulvid ;
019700050408
019800071126            v1cfil= 0;
019900071126            v1dfil= 'Tutti';
020000071128            clear ds_car ;
020100060320
020200071126            select;
020300071128       // se abilitazione distretto faccio vedere le aree gestite  //
020400071126            when wabi = 'DI' or wabi = 'AZ';
020500071126            exsr sr_cararee;
020600071126            *in46 = *on;
020700060320
020800071128       // imposto il p.o. da elaborare //
020900071126            when  not *in12 ;
021000071126            v1cfil = dutpou;
021100071126            chain v1cfil azorg01l ;
021200071126            If  %found(azorg01l) and orgfva = *blanks ;
021300071126                v1dfil = orgdes;
021400071126            endif;
021500071126
021600071126            endsl;
021700060320
021800071126         endsr;
021900060320
022000071126
022100071128       // ---- Routine di caricamento aree in gestione ---- //
022200071126         begsr sr_cararee ;
022300071126
022400071126         // valorizzo i campi del video //
022500080709         Area_video = %editc(skcar:'Z');
022600071126
022700071126         endsr;
022800071126
022900071126
023000071128       // ---- Routine di controllo del video ---- //
023100071126         begsr sr_contr ;
023200060320
023300071128       // se utente non abilitato riemetto la videata con l'errore  //
023400071126           if  *in08 ;
023500071126              *in28 = *on ;
023600071126              leavesr;
023700071126           endif;
023800071128       // reset indicatori //
023900071127           *in40 = *off;
024000071127           *in42 = *off;
024100080709           For xx = 47 to 96 ;
024200071127             *in(xx) = *off ;
024300071127           endfor ;
024400071128       // verifico il p.o. se immesso //
024500071127           If  v1cfil > 0 ;
024600071128       // verifico se è valido //
024700071127              chain v1cfil azorg01l ;
024800071127              If not %found(azorg01l) or orgfva <> *blanks ;
024900071127                   *in28 = *on ;
025000071127                   *in40 = *on ;
025100071127                   v1cmsg = msg(03);
025200071127                   leavesr;
025300071127              endif;
025400071128       // verifico se è una filiale valida //
025500071127              if  orgfag <> 'A' and orgfag <> 'F';
025600071127                   *in28 = *on;
025700071127                   *in40 = *on;
025800071127                   v1cmsg = msg(03);
025900071127                   leavesr;
026000071127              endif;
026100071128       // controllo se il p.o. è in gestione //
026200071128              if %lookup(v1cfil:skpog) = 0;
026300071127                   *in28 = *on;
026400071127                   *in40 = *on;
026500071127                   v1cmsg = msg(02);
026600071127                   leavesr;
026700071127              endif;
026800071127              v1dfil = orgdes;
026900071127           else;
027000071127              v1dfil ='Tutti';
027100071127           endif;
027200060320
027300071128       // se immesso il p.o. non posso immettere le aree e viceversa //
027400071127           if v1cfil <> *zeros and ds_car <> *blanks;
027500071127              *in28 = *on ;
027600071127              *in40 = *on ;
027700071127              *in47 = *on ;
027800071127              v1cmsg = msg(04) ;
027900071127              leavesr;
028000071127           endif;
028100060320
028200071128       // controllo le aree immesse //
028300071128             parcar = area_video;
028400071128       // Ricerca //
028500080709           For xx = 1 to 50 ;
028600071127               yy = (46 + xx);
028700071127               if parcar(xx) <> *blanks ;
028800080709                  if %lookup(parcar(xx):aree_abilita) = 0;
028900071127                     v1cmsg = msg(05);
029000071127                     *in(yy) = *on;
029100071127                     *in28 = *on;
029200071127                     leavesr;
029300071127                  endif;
029400071127               endif;
029500071127           endfor;
029600060320
029700071128           area_video = parcar;
029800060927
029900071128       // controllo data ultima spedizione presente su titas //
030000050408
030100071127           clear wlbdat;
030200071127           g02dat = v1cdus;
030300071127           g02err = ' ';
030400071127           xsrda8(wlbdat);
030500071127           if  g02err = '1';
030600071127             v1cmsg = msg(6);
030700071127             *in42 = *on ;
030800071127             *in28 = *on ;
030900071127           endif;
031000081007        // controllo se inserita data ultima spedizione minore della data //
031100081007        // trovata in tabella $3 o se maggiore o guale ad oggi  //
031200081007           If  g02inv < savdus ;
031300081007             v1cmsg = (%subst(msg(7): 1: 50)+ %editw(savdusdmy: '  /  /    '));
031400081007             *in42 = *on ;
031500081007             *in28 = *on ;
031600081007           endif;
031700081007           If  g02inv > %dec(%date) ;
031800081007             v1cmsg = msg(8);
031900081007             *in42 = *on ;
032000081007             *in28 = *on ;
032100081007           endif;
032200071127           v1cdus = g02dat;
032300071127           pardus = g02inv;
032400071127
032500071128       // controllo data acquisizione clienti  //
032600071127
032700071127           clear wlbdat;
032800071128           g02dat = v1cdna;
032900071127           g02err = ' ';
033000071127           xsrda8(wlbdat);
033100071127           if  g02err = '1';
033200071127             v1cmsg = msg(6);
033300071127             *in43 = *on ;
033400071127             *in28 = *on ;
033500071127           endif;
033600081007        // controllo se la data nascita del codice è maggiore di oggi //
033700081007           If  g02inv > *date ;
033800081007             v1cmsg = msg(8);
033900081007             *in43 = *on ;
034000081007             *in28 = *on ;
034100081007           endif;
034200071128           v1cdna = g02dat;
034300071128           pardna = g02inv;
034400071127
034500071127       endsr;
034600060320
034700071128       // ---- Routine di conferma e richiamo pgm batch ---- //
034800071127         begsr sr_conferma ;
034900071127
035000071127           parfil = v1cfil;
035100071127           parfile = 'F';
035200071127           parabi = wabi;
035300080710           parcrf = v1ccrf;
035400071127           kpjbu = parm01;
035500080711         // se sono i sede modifico la coda di input //
035600080711           If *in12 ;
035700080711           kqieb = 'KEDPCED';
035800080711           else ;
035900080711           kqieb = *blanks ;
036000080711           endif ;
036100080711        // tnta93r(kpjba);  //
036200080711           kcoaz = 'TA93';
036300080711           bch10(Kpjba);
036400050408
036500071127         endsr;
036600071127
036700071127       // ----- Routine iniziale ----- //
036800071127         begsr *inzsr;
036900071127
037000071127      /end-free
037100050408
037200060320     c     *entry        plist
037300060320     c                   parm                    kpjba
037400050510
037500071127
037600071127      /free
037700071127         in(e) §azute;
037800071127         if not %error;
037900071127          in(e) §datiute;
038000071127         endif;
038100071127         if %error or rsut = *blanks;
038200071128          clear tibs34ds;
038300071127          tibs34r(tibs34ds);
038400071127          in §azute;
038500071127          in §datiute;
038600071127         endif;
038700071127
038800071127         clear wabi;
038900071127         clear dlat;
039000060320
039100071128       // verifica errori e autorità profilo //
039200071128         select;
039300071128       // se ho errori nei dati utente esco dal pgm //
039400071128         when duterr = 'E' ;
039500071127           *in08 = *on;
039600071127           *in28 = *on;
039700071127           v1cmsg = msg(01);
039800071127           leavesr;
039900071128       //  se non c'è l'abilitazione                                     //
040000071128       //  * --> se 1° livello, abilitazioni al terminal                 //
040100071128       //  *     se 2° livello, abilitazioni al punto operativo          //
040200071128       //  *     se sede è impossibile ma se capita mando a fine il pgm  //
040300071128         when uteaut = *Blanks;
040400071128           if dutlpo = '1' ;
040500071127              wabi   = 'TP';
040600071128           endif;
040700071128           if dutlpo = '2';
040800071127              wabi   = 'PO';
040900071128           endif;
041000071128           if dutlpo = 'S';
041100071127              wabi   = 'AZ';
041200071128           endif ;
041300071128       //  carica le abilitazioni del profilo  //
041400071128         other;
041500071127           dute01 = utefaf;
041600071128           if §utegtc <> *Blanks ;
041700071127              wabi = §utegtc;
041800071128           else;
041900071127              wabi = uteaut;
042000071128           endif;
042100071128         endsl;
042200060320
042300071128       //  controllo se ok l'abilitazione dell'utente //
042400071127         clear tibs02ds;
042500071127         t02mod = 'C';
042600071127         t02sif = knsif;
042700071127         t02cod = 'LAT';
042800071127         t02ke1 = wabi;
042900071127         tibs02r(kpjba:tibs02ds);
043000071127         if t02err = *blanks;
043100071127            dlat = t02uni;
043200071127         endif;
043300071128       //  errore o non abilitato //
043400071127         if t02err <> *blanks or §latabi = 'S';
043500071127            *in08 = *on;
043600071127            *in28 = *on;
043700071127            v1cmsg = msg(01) ;
043800071127            leavesr;
043900071127         endif;
044000060320
044100080708       //  reperimento dei p.o. e dell'aree gestibili dall'utente //
044200071127         clear trul31ds;
044300071127         i31abi = wabi ;
044400071127         i31cdi = dutdis;
044500071127         i31car = dutare;
044600071127         i31cpo = dutpou;
044700080708         trul31r(kpjba:trul31ds:trul31ds2);
044800071128         if o31pog = *zeros ;
044900071128            *in08 = *on;
045000071128            *in28 = *on;
045100071128            v1cmsg = msg(01);
045200071128            leavesr;
045300071128         else;
045400071128            skpog = %dec(PO_abilitati:3:0) ;
045500080708            skcar = %dec(Aree_abilita:3:0) ;
045600071128         endif;
045700060320
045800071128         *in12 = (simfel = *zeros);
045900080710
046000080710        // imposto campo creazione file //
046100080710         v1ccrf = 'N' ;
046200071128
046300071128        // imposto le date di controllo //
046400081007        // recupero la data ultima spedizione dalla tabella $3 //
046500081007         tblkut = 1 ;
046600081007         tblcod = '$3' ;
046700081007         tblkey = '1       ';
046800081007         chain  %kds(k03tabel : 3) tabel00f;
046900081007         if   %found(TABEL00F);
047000081007         ds$3 = tbluni;
047100081007         else ;
047200081007         clear ds$3 ;
047300081007         endif ;
047400081007         if  §$3dp0 > 0 ;
047500081007         savdus = §$3dp0 ;
047600081007         Dataiso=%date(§$3dp0:*iso)  ;
047700081007         dataeur=Dataiso ;
047800081007         v1cdus=%dec(dataeur)  ;
047900081007         savdusdmy = v1cdus ;
047901081022         endif ;
048100081022         v1cdus  = %dec(%date - %months(6): *eur);
048300081022         v1cdna  = %dec(%date - %months(3): *eur);
048400081022         savdna = %dec(%date - %months(3));
048500050408
048600071128       endsr;
048700071128       /end-free
048800050408      *
048900060320** MSG  Lungh. 78                                                            *
049000060320Utente non autorizzato alla funzione                                          01
049100080708Filiale non in gestione all'utente                                            02
049200080708Filiale errata                                                                03
049300080709Filiale valorizzata non compatibile con aree selezionate                      04
049400060320Codice Area non in gestione all'utente                                        05
049500060927Data Errata                                                                   06
049600081007Data ultima spedizione non deve essere minore di                              07
049700081007La data non può essere maggiore di oggi                                       08
