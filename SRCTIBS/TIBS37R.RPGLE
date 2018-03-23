000100081020     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*NEW)
000200080930
000300080930      // ----------------------------------------------------------------------
000400080930      //
000500081014      //         Modifica password utenti di rete  ?
000600080930      //
000700080930      // ----------------------------------------------------------------------
000800080930      // ? DICHIARAZIONE DEI FILE ?
000900080930      // ----------------------------------------------------------------------
001000080930
001100081014     fazorg01l  if   e           k disk
001200081014     fwflpr00f  o    e             disk
001300081014     ftibs37d   cf   e             workstn
001400080930
001500080930      // ? V A R I A B I L I ?
001600081014     d wabi            s                   like(uteaut)
001700081014     d wfil            s              3  0
001800081028     d wlen            s              3  0
001900080930     d $fine           s               n
002000081020     d $lenaut         s              2  0
002100081020     d $lenmag         s              2  0
002200081014     d $video          s             10
002300081014     d $utebartaut     s               n
002400081014     d $utebartmag     s               n
002500081014      //?variabili per richiamo a UBCHKLGN
002600081014     d piuser          s             10a
002700081014     d pipassword      s             10a
002800081014     d piusrhdl        s             12a
002900081014     d pilgnsts        s             10a
003000090217      //?variabili per richiamo a TIBS37R1
003100090217     d  inusr          s             15a
003200090217     d  inpwd          s             15a
003300090217     d  indomtyp       s              1a
003400090217     d  outsts         s              1a
003500090217     d  outmsg         s             80a
003600080930
003700080930      // ? S C H I E R E ?
003800080930     d msg             s             78    dim(15) ctdata perrcd(1)
003900080930
004000080930      // ? D S   I N T E R N E / E S T E R N E ?
004100080930     d §azute        e ds                  extname(azute00f)
004200080930     d                                     dtaara
004300080930     d §datiute      e ds                  extname(ddatiute)
004400080930     d                                     dtaara
004500080930
004600081014     d dlat          e ds
004700081014     d dute01        e ds
004800080930     d kpjba         e ds
004900080930     d tibs34ds      e ds                  inz
005000081014     d trul31ds      e ds
005100081020     d  skpog                 10    759  0 dim(250)
005200081020
005300081020      // ? COSTANTI ?
005400081020     d c_digit         c                   const('1234567890')
005500081020     d c_lenmax        c                   const(10)
005600081020     d c_lenminaut     c                   const(4)
005700081020     d c_lenminmag     c                   const(2)
005800081020     d c_lenminr       c                   const(8)
005900090217
006000090217      // ? PROCEDURA ?
006100090217     d tibs37r1        pr                  extpgm('TIBS37R1')
006200090217     d  inusr                        15a
006300090217     d  inpwd                        15a
006400090217     d  indomtyp                      1a
006500090217     d  outsts                        1a
006600090217     d  outmsg                       80a
006700080930
006800080930      // ? PROTOTIPI ?
006900080930      /copy gaitrasrc/srcprotopr,tibs34r
007000081014      /copy gaitrasrc/srcprotopr,trul31r
007100081014      /copy gaitrasrc/srcprotopr,ubchklgn
007200080930
007300080930      // ----------------------------------------------------------------------
007400080930
007500080930     c     *entry        plist
007600080930     c                   parm                    kpjba
007700080930
007800080930      /free
007900080930
008000080930       //?operazioni iniziali
008100080930         exsr routinz;
008200080930
008300080930         dow not $fine;
008400081014
008500081014       //?gestione delle videate
008600081014          select;
008700081014
008800081014       //?gestione richiesta password AS400
008900081014          when $video = 'D01';
009000081014          exsr gespswas;
009100081014
009200081014       //?gestione richiesta modifica password utente di rete
009300081014          when $video = 'D02';
009400081014          exsr gespswr;
009500081014
009600081014          endsl;
009700081014
009800080930         enddo;
009900080930
010000080930         *inlr = *on;
010100081014
010200081014       // ----------------------------------------------------------------------
010300081014       //?Richiesta password AS400 del profilo utente collegato
010400081014       // ----------------------------------------------------------------------
010500081014       begsr gespswas;
010600081014
010700081014       //?imposto il profilo
010800081014         w1cute = dutute;
010900081014
011000081014       //?emetto la videata
011100081014         exfmt bs37d01;
011200081014
011300081014       //?spengo indicatori di comodo
011400081014         *in28 = *off;
011500081014       //?pulisco campo messaggi
011600081014         clear w1cmsg;
011700081014
011800081014       //?f3=fine
011900081014         if *inkc;
012000081014          $fine = *on;
012100081014          leavesr;
012200081014         endif;
012300081014
012400081014       //?controllo la password AS400 del profilo collegato
012500081014         exsr contrpswas;
012600081014         if *in28;
012700081014          clear w1cpsw;
012800081014          leavesr;
012900081014         endif;
013000081014
013100081014         $video = 'D02';
013200081014
013300081014       endsr;
013400081014
013500081014       // ----------------------------------------------------------------------
013600081014       //?Controllo la password AS400 del profilo collegato
013700081014       // ----------------------------------------------------------------------
013800081014       begsr contrpswas;
013900081014
014000081014       //?deve essere inserita
014100081014         if w1cpsw = *blanks;
014200081014          *in28 = *on;
014300081014          w1cmsg = msg(02);
014400081014          leavesr;
014500081014         endif;
014600081014
014700081014       //?verifico se esatta
014800081014         piuser = w1cute;
014900081014         pipassword = w1cpsw;
015000081014         clear piusrhdl;
015100081014         clear pilgnsts;
015200081020         if ubchklgn_Check(piuser:pipassword:piusrhdl:pilgnsts)=0;
015300081020          if pilgnsts <> '*SUCCESS';
015400081020           *in28 = *on;
015500081020           w1cmsg = msg(03);
015600081020           leavesr;
015700081020          endif;
015800081020         else;
015900081020           *in28 = *on;
016000081020           w1cmsg = msg(03);
016100081020           leavesr;
016200081020         endif;
016300081014
016400081014       endsr;
016500080930
016600080930       // ----------------------------------------------------------------------
016700081014       //?Richiesta modifica password utente di rete
016800080930       // ----------------------------------------------------------------------
016900081014       begsr gespswr;
017000080930
017100080930       //?emetto la videata
017200081014         exfmt bs37d02;
017300080930
017400080930       //?spengo indicatori di comodo
017500080930         *in28 = *off;
017600080930       //?pulisco campo messaggi
017700081014         clear w2cmsg;
017800080930
017900080930       //?f3=fine
018000080930         if *inkc;
018100080930          $fine = *on;
018200080930          leavesr;
018300080930         endif;
018400080930
018500081014       //?controllo l'utente di rete
018600081014         exsr contrute;
018700081014         if *in28;
018800081014          leavesr;
018900081014         endif;
019000080930
019100081014       //?controllo la password dell'utente di rete
019200081014         exsr contrpswr;
019300080930         if *in28;
019400080930          leavesr;
019500080930         endif;
019600080930
019700080930       //?f6=elabora
019800080930         if *inkf;
019900081014          exsr aggiorna;
020000090217          if *in28;
020100090217           leavesr;
020200090217          endif;
020300090217
020400081014          exsr wrtlog;
020500080930          $fine = *on;
020600080930          leavesr;
020700080930         endif;
020800080930
020900080930       endsr;
021000081014
021100081014       // ----------------------------------------------------------------------
021200081014       //?Controllo l'utente di rete
021300081014       // ----------------------------------------------------------------------
021400081014       begsr contrute;
021500081014
021600081014       //?spengo indicatori di posizionamento
021700081014         *in40 = *off;
021800081014
021900081014       //?pulisco campo di comodo per la filiale
022000081014         clear wfil;
022100081014
022200081014       //?utente di rete obbligatorio
022300081014         if w2cute = *blanks;
022400081014          *in28 = *on;
022500081014          *in40 = *on;
022600081014          w2cmsg = msg(04);
022700081014          leavesr;
022800081014         endif;
022900081014
023000081014       //?controllo se utente valido di filiale, cioè con codice filiale nel profilo
023100081014         if %subst(w2cute:4:3) < *zeros and %subst(w2cute:3:3) < *zeros;
023200081014          *in28 = *on;
023300081014          *in40 = *on;
023400081014          w2cmsg = msg(05);
023500081014          leavesr;
023600081014         endif;
023700081014
023800081014       //?controllo se utente BartAut, BartMag o altro utente
023900081021
024000081021       //?provo con utente BartAut/Mag (filiale dalla posizione 3)
024100081021         if %subst(w2cute:3:3) > *zeros and
024200081021           (%subst(w2cute:1:2) = 'AU' or %subst(w2cute:1:2) = 'PR');
024300081021          wfil = %dec(%subst(w2cute:3:3):3:0);
024400081021          if %subst(w2cute:1:2) = 'AU';
024500081021           $utebartaut = *on;
024600081021           $utebartmag = *off;
024700081021          endif;
024800081021          if %subst(w2cute:1:2) = 'PR';
024900081021           $utebartaut = *off;
025000081021           $utebartmag = *on;
025100081021          endif;
025200081021       //?controllo se filiale valida
025300081021          chain wfil azorg01l;
025400081021          if not %found(azorg01l) or orgfva <> *blanks;
025500081021           *in28 = *on;
025600081021           *in40 = *on;
025700081021           w2cmsg = msg(06);
025800081021           leavesr;
025900081021          endif;
026000081021       //?controllo se profilo AS può variare utente della filiale
026100081021          clear trul31ds;
026200081021          if %subst(w2cute:1:2) = 'AU';
026300081021           if §utepswaut = 'NO';
026400081021            *in28 = *on;
026500081021            *in40 = *on;
026600081021            w2cmsg = msg(12);
026700081021            leavesr;
026800081021           endif;
026900081021           i31abi = §utepswaut;
027000081021          else;
027100081021           if §utepswmag = 'NO';
027200081021            *in28 = *on;
027300081021            *in40 = *on;
027400081021            w2cmsg = msg(13);
027500081021            leavesr;
027600081021           endif;
027700081021           i31abi = §utepswmag;
027800081021          endif;
027900081021       //?reperimento delle filiali gestibili dal profilo AS
028000081021          exsr contrfil;
028100081021          if *in28;
028200081021           leavesr;
028300081021          endif;
028400081021          leavesr;
028500081021         endif;
028600081014
028700081014       //?provo con Altro utente (filiale dalla posizione 4)
028800081021         if %subst(w2cute:4:3) > *zeros;
028900081021          wfil = %dec(%subst(w2cute:4:3):3:0);
029000081014          $utebartaut = *off;
029100081014          $utebartmag = *off;
029200081014       //?controllo se filiale valida
029300081020          chain wfil azorg01l;
029400081021          if not %found(azorg01l) or orgfva <> *blanks;
029500081014           *in28 = *on;
029600081014           *in40 = *on;
029700081014           w2cmsg = msg(06);
029800081014           leavesr;
029900081014          endif;
030000081014       //?controllo se profilo AS può variare utente della filiale
030100081021          if §utepswr = 'NO';
030200081021           *in28 = *on;
030300081021           *in40 = *on;
030400081021           w2cmsg = msg(14);
030500081021           leavesr;
030600081021          endif;
030700081014       //?reperimento delle filiali gestibili dal profilo AS
030800081014          clear trul31ds;
030900081014          i31abi = §utepswr;
031000081014          exsr contrfil;
031100081014          if *in28;
031200081014           leavesr;
031300081014          endif;
031400081021          leavesr;
031500081014         endif;
031600081021
031700081021       //?se arrivo qua non ho fatto nessun controllo precedente e quindi
031800081021       //?l'utente non è valido
031900081021         *in28 = *on;
032000081021         *in40 = *on;
032100081021         w2cmsg = msg(05);
032200081014
032300081014       endsr;
032400081014
032500081014       // ----------------------------------------------------------------------
032600081014       //?Carico le filiali gestite dal profilo AS
032700081014       //?e controllo se può gestire la filiale dell'utente di rete
032800081014       // ----------------------------------------------------------------------
032900081014       begsr contrfil;
033000081014
033100081021         i31cdi = dutdis;
033200081021         i31car = dutare;
033300081021         i31cpo = dutpou;
033400081021         trul31r(kpjba:trul31ds);
033500081014
033600081014       //?controllo se filiale utente è gestibile dal profilo AS
033700081021         if %lookup(wfil:skpog) = *zeros;
033800081021          *in28 = *on;
033900081021          *in40 = *on;
034000081021          w2cmsg = msg(07);
034100081021          leavesr;
034200081021         endif;
034300081014
034400081014       endsr;
034500081014
034600081014       // ----------------------------------------------------------------------
034700081014       //?Controllo la password dell'utente di rete
034800081014       // ----------------------------------------------------------------------
034900081014       begsr contrpswr;
035000081014
035100081014       //?spengo indicatori di posizionamento
035200081014         *in41 = *off;
035300081014
035400081014       //?password obbligatoria
035500081014         if w2cpsw = *blanks;
035600081014          *in28 = *on;
035700081014          *in41 = *on;
035800081014          w2cmsg = msg(08);
035900081014          leavesr;
036000081014         endif;
036100081014
036200081014       //?se utente BartAut la password deve essere minimo 4 massimo 10 numerica
036300081020         if $utebartaut;
036400081020       //?controllo lunghezza password
036500081020          if %len(%trimr(w2cpsw)) < c_lenminaut and
036600081020             %len(%trimr(w2cpsw)) > c_lenmax;
036700081020           *in28 = *on;
036800081020           *in41 = *on;
036900081020           w2cmsg = msg(09);
037000081020           leavesr;
037100081020          endif;
037200081020       //?controllo che i caratteri siano tutti cifre
037300081020          $lenaut = %len(%trimr(w2cpsw));
037400081020          if %checkr(c_digit:w2cpsw:$lenaut) > *zero;
037500081020           *in28 = *on;
037600081020           *in41 = *on;
037700081020           w2cmsg = msg(09);
037800081020           leavesr;
037900081020          endif;
038000081014         endif;
038100081014
038200081014       //?se utente BartMag la password deve essere minimo 2 massimo 10 numerica
038300081014         if $utebartmag;
038400081020       //?controllo lunghezza password
038500081020          if %len(%trimr(w2cpsw)) < c_lenminmag and
038600081020             %len(%trimr(w2cpsw)) > c_lenmax;
038700081020           *in28 = *on;
038800081020           *in41 = *on;
038900081020           w2cmsg = msg(10);
039000081020           leavesr;
039100081020          endif;
039200081020       //?controllo che i caratteri siano tutti cifre
039300081020          $lenmag = %len(%trimr(w2cpsw));
039400081020          if %checkr(c_digit:w2cpsw:$lenmag) > *zero;
039500081020           *in28 = *on;
039600081020           *in41 = *on;
039700081020           w2cmsg = msg(10);
039800081020           leavesr;
039900081020          endif;
040000081014         endif;
040100081014
040200081020       //?se altro utente la password deve essere minimo 8 caratteri
040300081014         if not $utebartaut and not $utebartmag;
040400081020       //?controllo lunghezza password
040500081020          if %len(%trimr(w2cpsw)) < c_lenminr;
040600081020           *in28 = *on;
040700081020           *in41 = *on;
040800081020           w2cmsg = msg(11);
040900081020           leavesr;
041000081020          endif;
041100081014         endif;
041200081028
041300081028       //?la password non deve avere dei campi blank in mezzo
041400081028         wlen = %checkr(' ':w2cpsw);
041500081028         if %scan(' ':%subst(w2cpsw:1:wlen)) > *zeros;
041600081028          *in28 = *on;
041700081028          *in41 = *on;
041800081028          w2cmsg = msg(15);
041900081028          leavesr;
042000081028         endif;
042100081014
042200081014       endsr;
042300080930
042400080930       // ----------------------------------------------------------------------
042500081014       //?Aggiorna password dell'utente di rete
042600080930       // ----------------------------------------------------------------------
042700081014       begsr aggiorna;
042800090217
042900090217         inusr = w2cute;
043000090217         inpwd = w2cpsw;
043100090217         if $utebartaut = *off and $utebartmag = *off;
043200090217          indomtyp = '1';
043300090217         else;
043400090217          indomtyp = '2';
043500090217         endif;
043600090217         clear outsts;
043700090217         clear outmsg;
043800090217
043900090217         tibs37r1(inusr:inpwd:indomtyp:outsts:outmsg);
044000090217
044100090217         if outsts <> '0';
044200090217          *in28 = *on;
044300090217          w2cmsg = outmsg;
044400090217          leavesr;
044500090217         endif;
044600080930
044700080930       endsr;
044800081014
044900081014       // ----------------------------------------------------------------------
045000081014       //?Scrive file di LOG sempre anche se non viene variata la password
045100081014       // ----------------------------------------------------------------------
045200081014       begsr wrtlog;
045300081014
045400081014         clear wflpr000;
045500081014         lpruteas = w1cute;
045600081014         lprvideo = knmtd;
045700081014         lprdata = %dec(%date());
045800081014         lprora = %dec(%time());
045900081014         lpruter = w2cute;
046000081014         lpresito = 'OK';
046100081014         write wflpr000;
046200081014
046300081014       endsr;
046400080930
046500080930       // ----------------------------------------------------------------------
046600080930       //?Operazioni iniziali.
046700080930       // ----------------------------------------------------------------------
046800080930       begsr routinz;
046900080930
047000080930       //?recupero dei dati utente
047100080930         in(e) §azute;
047200080930          if not %error;
047300080930          in(e) §datiute;
047400080930         endif;
047500080930         if %error or rsut = *blanks;
047600080930          tibs34r(tibs34ds);
047700080930          in §azute;
047800080930          in §datiute;
047900080930         endif;
048000081014
048100081014         clear wabi;
048200081014         clear dlat;
048300081014
048400081014       //?verifica errori e autorizzazioni profilo AS400
048500081014         select;
048600081014       //?se ci sono errori nei dati utente errore per poi uscire dal pgm
048700081014         when duterr = 'E';
048800081014          *in28 = *on;
048900081021          exfmt bs37d00;
049000081014          $fine =*on;
049100081014          leavesr;
049200081014       //?imposto di l'abilitazione standard se non trovata sul profilo
049300081014       //?--> se profilo di 1° livello abilitazione al terminal
049400081014       //?--> se profilo di 2° livello abilitazione alla filiale
049500081014       //?--> se profilo di sede abilitazione 'NO' perchè la funzione va fatta
049600081014       //?                                         solo con profili di filiale
049700081014         when uteaut = *blanks;
049800081014          if dutlpo = '1';
049900081014           wabi = 'TP';
050000081014          endif;
050100081014          if dutlpo = '2';
050200081014           wabi = 'PO';
050300081014          endif;
050400081014          if dutlpo = 'S';
050500081014           wabi = 'NO';
050600081014          endif;
050700081014       //?carico le abilitazioni del profilo
050800081014         other;
050900081014          dute01 = utefaf;
051000081014         endsl;
051100081014
051200081014       //?controllo se l'utente è abilitato alla modifica delle password
051300081014       //?chiodo per comodità il 'NO' senza andare a controllare sulla tabella LAT
051400081014         if wabi = 'NO' or (§utepswaut = 'NO' and §utepswmag = 'NO' and
051500081014                            §utepswr = 'NO');
051600081021          exfmt bs37d00;
051700081014          $fine =*on;
051800081014          leavesr;
051900081014         endif;
052000080930
052100080930         $fine =*off;
052200081014         $video = 'D01';
052300080930
052400080930       endsr;
052500080930
052600080930** MSG  Lungh. 78                                                            *
052700081014Utente non autorizzato alla Funzione richiesta                                01
052800081014Immettere la password AS400                                                   02
052900081014Password AS400 errata                                                         03
053000081014Immettere l'utente di rete                                                    04
053100081014Utente di rete non valido per la modifica                                     05
053200081014Filiale utente non valida                                                     06
053300081014Filiale utente di rete non in gestione al profilo AS                          07
053400081014Immettere la nuova password                                                   08
053500081020La password DEVE essere composta da min 4 cifre a max 10 cifre                09
053600081020La password DEVE essere composta da min 2 cifre a max 10 cifre                10
053700081020La password DEVE essere composta da almeno 8 caratteri                        11
053800081021Utente non autorizzato alla modifica per utenti BartAut                       12
053900081021Utente non autorizzato alla modifica per utenti BartMag                       13
054000081021Utente non autorizzato alla modifica per utenti di rete                       14
054100081028La password NON deve contenere degli spazi                                    15
