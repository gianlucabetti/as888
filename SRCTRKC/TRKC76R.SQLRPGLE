000100150217      //--------------------------------------------------------------
000200150218      //?TRKC76R - Dati da Visualizzare o Parzializzazioni Trattative
000300150217      //--------------------------------------------------------------
000400150217
000500150217     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150217
000700150217      //---------------------------------------------------------------
000800150217      //?Dichiarazione file.
000900150217      //---------------------------------------------------------------
001000150217
001100150217      // - Anagrafica Campagna
001200150217     fTICMP01L  if   e           k disk
001300150220
001400150220      // - Fasi Campagna
001500150220     fTICMF01L  if   e           k disk
001600150217
001700150217      // - Anagrafica Potenziali
001800150217     fTNCPO01L  if   e           k disk
001900150217
002000150217      // - Trattative/Offerte/Tariffe
002100150217     fTIVIS05L  if   e           k disk
002200150217     fTIVOF11L  if   e           k disk
002300150217     fTNTAM01L  if   e           k disk
002400150217
002500150217      // - Scatti ISTAT
002600150218     fTISIS01L  if   e           k disk    usropn
002700150217
002800150217      //---------------------------------------------------------------
002900150217      //?Definizione costanti.
003000150217      //---------------------------------------------------------------
003100150217
003200150220     d FaseObjTf       c                   const(' TF')
003300150217     d FaseObjTtr      c                   const(' TR')
003400150218     d FaseObjCf       c                   const(' CF')
003500150217
003600150217      //---------------------------------------------------------------
003700150217      //?Definizione schiere.
003800150217      //---------------------------------------------------------------
003900150217
004000150217      // - Sk ISTAT Data e %
004100150217     d IstatData       s              8s 0 dim(999)
004200150217     d IstatPerc       s              5s 2 dim(999)
004300150217
004400150217      // - Messaggi di errore
004500150217     d Msg             s             78    dim(20) ctdata perrcd(1)
004600150217
004700150217      //---------------------------------------------------------------
004800150217      //?Definizione aree dati.
004900150217      //---------------------------------------------------------------
005000150217
005100150217      // - Dati utente
005200150217     d §AzUte        e ds                  extname(AZUTE00F)
005300150217     d                                     dtaara
005400150217     d §DatiUte      e ds                  extname(dDatiUte)
005500150217     d                                     dtaara
005600150217
005700150217      //---------------------------------------------------------------
005800150217      //?Definizione strutture dati.
005900150217      //---------------------------------------------------------------
006000150217
006100150217      // - Status
006200150217     d Psds           sds
006300150217     d   SDSpgm          *proc
006400150217
006500150217      // - Parametri ricevuti
006600150217     d KPJBA         e ds
006700150217     d TRKC76DS      e ds
006800150218
006900150218      // - Ricerca/Controllo tabelle
007000150218     d TIBS02DS      e ds                  inz
007100150217
007200150217      // - Reperimento dati utente
007300150217     d TIBS34DS      e ds
007400150217
007500150217      // - Reperimento dati Anagrafica Clienti
007600150217      /copy gaitrasrc/srcprotopi,TIBS69R
007700150217
007800150217      //---------------------------------------------------------------
007900150217      //?Definizione variabili globali.
008000150217      //---------------------------------------------------------------
008100150217
008200150217      // - Campi di comodo data
008300150217     d Data_EUR        s               d   datfmt(*eur)
008400150217     d Data_ISO        s               d   datfmt(*iso)
008500150217     d wData           s              8s 0 inz
008600150217
008700150217      // - Campi di comodo
008800150217     d Anno            s              4s 0 inz
008900160128     d Annodic         s              4s 0 inz
009000150217     d AnnoIstat       s              4s 0 inz
009100150409     d DataConf        s              6s 0 inz
009200150409     d DataTar         s              6s 0 inz
009300150217     d Oggi            s              8s 0 inz
009400150409     d wAnno           s                   like(OKC76anno)
009500150409     d wMese           s                   like(OKC76mese)
009600150217
009700150217      // - Indici di schiera
009800150217     d xx              s              4s 0 inz
009900150217
010000150217      // - Flags booleani
010100150422     d NoTam           s               n   inz(*off)
010200150217     d wEnd            s               n   inz(*off)
010300150303     d wOff            s               n   inz(*off)
010400150218     d wPrimo          s               n   inz(*off)
010500150217
010600150217      //---------------------------------------------------------------
010700150217      //?Definizione procedure usate.
010800150217      //---------------------------------------------------------------
010900150217
011000150217      //---------------------------------------------------------------
011100150217      //?Definizione Prototipi.
011200150217      //---------------------------------------------------------------
011300150218
011400150218      /copy gaitrasrc/srcprotopr,TIBS02R
011500150217      /copy gaitrasrc/srcprotopr,TIBS34R
011600150217      /copy gaitrasrc/srcprotopr,TIBS69R
011700150217
011800150217      //---------------------------------------------------------------
011900150217      //?Definizione key-list.
012000150217      //---------------------------------------------------------------
012100150217
012200150217      //---------------------------------------------------------------
012300150217      //?M A I N - L I N E
012400150217      //---------------------------------------------------------------
012500150217
012600150217     c     *Entry        plist
012700150217     c                   parm                    kpjba
012800150217     c                   parm                    TRKC76DS
012900150217
013000150217      /free
013100150217
013200150217       //?Operazioni iniziali
013300150217       exsr RoutInz;
013400150217
013500150217       //?Controllo dati passati
013600150218       IF  IKC76ric <> 'C';
013700150218         exsr Controlla;
013800150218       ENDIF;
013900150218
014000150218       //?Elaborazioni diverse in base alla fase passata
014100150218       SELECT;
014200150218
014300150218       //?Elaborazione per Chiusura file
014400150218       WHEN  IKC76ric = 'C';
014500150218         *inlr = *on;
014600150218         return;
014700150218       //?Elaborazione per Parzializzazione
014800150218       WHEN  IKC76ric = 'P';
014900150218         exsr Parzializza;
015000150218       //?Elaborazione per Fase Trattativa
015100150218       WHEN  IKC76acm = FaseObjTtr;
015200150218         exsr Trattativa;
015300150218       //?Elaborazione per Fase Confronto Fatturazione
015400150218       WHEN  IKC76acm = FaseObjCf;
015500150218         exsr Confronto;
015600150218
015700150218       ENDSL;
015800150217
015900150217       //?Operazioni finali
016000150217       exsr RoutEnd;
016100150217
016200150217       //--------------------------------------------------------------
016300150217       //?Operazioni iniziali.
016400150217       //--------------------------------------------------------------
016500150217       BEGSR RoutInz;
016600150217
016700150217       //?Imposto oggi
016800150217         Oggi = %dec(%date());
016900150217
017000150217       //?Reperimento dati job
017100150217         exsr DatiJob;
017200150217
017300150302         clear OKC76dfa;
017400150220         clear OKC76flag;
017500150223         clear OKC76peaa;
017600150217         clear OKC76pea;
017700150224         clear OKC76anno;
017800150224         clear OKC76mese;
017900150220         clear OKC76nrv;
018000150225         clear OKC76forza;
018100150217         clear OKC76err;
018200150217         clear OKC76msg;
018300150217
018400150217       ENDSR;
018500150217
018600150217       //--------------------------------------------------------------
018700150217       //?Reperimento Dati del job (Utente/Operativi).
018800150217       //--------------------------------------------------------------
018900150217       BEGSR DatiJob;
019000150217
019100150217         in(E) §AzUte;
019200150217         IF  NOT %error;
019300150217           in(E) §DatiUte;
019400150217         ENDIF;
019500150217         IF  %error or RSut = *blanks;
019600150217           clear TIBS34ds;
019700150217           tibs34r(tibs34ds);
019800150217           in §AzUte;
019900150217           in §DatiUte;
020000150217         ENDIF;
020100150217
020200150217       ENDSR;
020300150217
020400150217       //--------------------------------------------------------------
020500150217       //?Controllo i dati passati.
020600150217       //--------------------------------------------------------------
020700150217       BEGSR Controlla;
020800150218
020900150218       //?Tipo richiamo
021000150218         IF  IKC76ric <> 'P' and IKC76ric <> 'F';
021100150218           OKC76err = 'E';
021200150218           OKC76msg = Msg(01);
021300150218           exsr RoutEnd;
021400150218         ENDIF;
021500150217
021600150217       //?Numero campagna obbligatorio
021700150217         IF  IKC76ncm = 0;
021800150217           OKC76err = 'E';
021900150218           OKC76msg = Msg(02);
022000150217           exsr RoutEnd;
022100150217         ENDIF;
022200150217       //?Deve essere una campagna valida
022300150217         chain (IKC76ncm) TICMP01L;
022400150409         IF  not %found(TICMP01L);
022500150217           OKC76err = 'E';
022600150218           OKC76msg = Msg(02);
022700150217           exsr RoutEnd;
022800150217         ENDIF;
022900150409         IF  %subst(CMPflo:1:6) > *zeros;
023000150409           wAnno = %dec(%subst(CMPflo:1:4):4:0);
023100150409           wMese = %dec(%subst(CMPflo:5:2):2:0);
023200150409           DataConf = (wAnno * 100) + wMese;
023300150409         ENDIF;
023400150217
023500150217       //?Almeno 1 tra KSU/KSC/CPO deve esserci
023600150217         IF  IKC76ksu = 0 and IKC76ksc = 0 and IKC76cpo = 0;
023700150217           OKC76err = 'E';
023800150218           OKC76msg = Msg(03);
023900150217           exsr RoutEnd;
024000150217         ENDIF;
024100150217       //?Deve essere una codice valido
024200150217         IF  IKC76ksu > 0;
024300150217           clear TIBS69DS;
024400150217           I69kac = %dec(IKC76ksu:7:0);
024500150217           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
024600150217           IF  O69err <> *blanks;
024700150217             OKC76err = 'E';
024800150218             OKC76msg = Msg(03);
024900150217             exsr RoutEnd;
025000150217           ENDIF;
025100150217         ENDIF;
025200150217         IF  IKC76ksc > 0;
025300150217           clear TIBS69DS;
025400150217           I69kac = %dec(IKC76ksc:7:0);
025500150217           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
025600150217           IF  O69err <> *blanks;
025700150217             OKC76err = 'E';
025800150218             OKC76msg = Msg(03);
025900150217             exsr RoutEnd;
026000150217           ENDIF;
026100150217         ENDIF;
026200150217         IF  IKC76cpo > 0;
026300150217           chain (IKC76cpo) TNCPO01L;
026400150217           IF  not %found(TNCPO01L);
026500150217             OKC76err = 'E';
026600150218             OKC76msg = Msg(03);
026700150217             exsr RoutEnd;
026800150217           ENDIF;
026900150217         ENDIF;
027000150303
027100150303       //?Per Parzializzazione esco dai controlli
027200150303         IF  IKC76ric = 'P';
027300150303           leavesr;
027400150303         ENDIF;
027500150218
027600150218       //?Fase obbligatoria
027700150218         IF  IKC76acm = *blanks;
027800150218           OKC76err = 'E';
027900150218           OKC76msg = Msg(04);
028000150218           exsr RoutEnd;
028100150218         ENDIF;
028200150218       //?Deve essere una fase valida
028300150218         clear TIBS02DS;
028400150218         T02mod = 'C';
028500150218         T02cod = 'FCM';
028600150218         T02ke1 = IKC76acm;
028700150218         T02sif = KNSIF;
028800150218         TNTBE_RicercaControllo  (kpjba : tibs02ds);
028900150218         IF  T02err <> *blanks;
029000150218           OKC76err = 'E';
029100150218           OKC76msg = Msg(04);
029200150218           exsr RoutEnd;
029300150218         ENDIF;
029400150218
029500150218       ENDSR;
029600150218
029700150218       //--------------------------------------------------------------
029800150218       //?Parzializzazione Trattative.
029900150218       //--------------------------------------------------------------
030000150218       BEGSR Parzializza;
030100150303
030200150303         wOff = *off;
030300150303
030400150303       //?Parzializza è fatta SOLO per le trattative
030500150303       //?quindi la fase TR la cerco subito
030600150303         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTtr) TICMF01L;
030700150303
030800150303       //?Trattative avviate
030900150303       //?Trattative avviate con offerta
031000150303       //?Trattative avviate senza offerta
031100150303       //?Trattative non forzate
031200150303       //?deve esserci la fase TR
031300150303         IF  (IKC76flgp = 'A ' or IKC76flgp = 'AO' or
031400150303              IKC76flgp = 'AN' or IKC76flgp = 'T ') and
031500150303               not %found(TICMF01L);
031600150303           OKC76err = 'E';
031700150303           exsr RoutEnd;
031800150303         ENDIF;
031900150303
032000150303       //?Trattative non forzate
032100150303       //?e ho la fase TR ok
032200150303         IF  %found(TICMF01L) and IKC76flgp = 'T ';
032300150303           leavesr;
032400150303         ENDIF;
032500150303
032600150303       //?Se non ho la fase TR provo con la fase TF
032700150303         IF  not %found(TICMF01L);
032800150303           chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTf) TICMF01L;
032900150303         //?se non trovo neanche la TF non ho trattative quindi vado via
033000150303           IF  not %found(TICMF01L);
033100150303             OKC76err = 'E';
033200150303             exsr RoutEnd;
033300150303           ENDIF;
033400150303         ENDIF;
033500150303
033600150303       //?Se non ho il numero della trattativa è una Trattativa Forzata
033700150303         IF  CMFnrv = 0 and IKC76flgp = 'F ';
033800150303           leavesr;
033900150303         ENDIF;
034000150303
034100150303       //?Trattativa forzata è una trattativa chiusa positiva
034200150303         IF  CMFnrv = 0 and IKC76flgp = 'CN';
034300150303           OKC76err = 'E';
034400150303           exsr RoutEnd;
034500150303         ENDIF;
034600150303
034700150303       //?Se non ho il numero della trattativa
034800150303       //?e sono state richieste
034900150303       //?Trattative Chiuse o
035000150303       //?Trattative Chiuse Positive
035100150303       //?fine dei giochi
035200150303         IF  CMFnrv = 0 and (IKC76flgp = 'C ' or IKC76flgp = 'CP');
035300150303           leavesr;
035400150303         ENDIF;
035500150303
035600150303       //?Se arrivo qua ho il numero trattativa quindi aggancio la trattativa
035700150303         chain (CMFnrv) TIVIS05L;
035800150303         //?non dovrebbe succedere
035900150303         //?ma se non trovo la trattativa esco con errore
036000150303         IF  not %found(TIVIS05L);
036100150303           OKC76err = 'E';
036200150303           exsr RoutEnd;
036300150303         ENDIF;
036400150303
036500150303       //?Se la trattativa ha un esito
036600150303         IF  VISesi <> *blanks;
036700150303       //?ed è stato richiesto
036800150303           SELECT;
036900150303         //?Trattative Chiuse
037000150303           WHEN  IKC76flgp = 'C ';
037100150303             leavesr;
037200150303         //?Trattative Chiuse Positive
037300150303           WHEN  VISesi = 'P ' and IKC76flgp = 'CP';
037400150303             leavesr;
037500150303         //?Trattative Chiuse No Positive
037600150303           WHEN  VISesi <> 'P ' and IKC76flgp = 'CN';
037700150303             leavesr;
037800150303           ENDSL;
037900150303         ENDIF;
038000150303
038100150303       //?Se la trattativa non ha un esito
038200150303         IF  VISesi = *blanks;
038300150303         //?ed è stato richiesto
038400150303         //?Trattative Avviate
038500150303           IF  IKC76flgp = 'A ';
038600150303             leavesr;
038700150303           ENDIF;
038800150303
038900150303         //?Leggo le offerte (no le 'H'-congelata e '*'-annullata)
039000150303           setll (VISnrv) TIVOF11L;
039100150303           reade (VISnrv) TIVOF11L;
039200150303           DOW not %eof(TIVOF11L);
039300150303         //?se trovo anche solo 1 offerta valida
039400150303             IF  VOFeso <> 'H' and VOFeso <> '*';
039500150303         //?è stato richiesto
039600150303         //?Trattative Avviate con Offerta
039700150303               IF  IKC76flgp = 'AO';
039800150303                 leavesr;
039900150303               ENDIF;
040000150303              wOff = *on;
040100150303              leave;
040200150303             ENDIF;
040300150303             reade (VISnrv) TIVOF11L;
040400150303           ENDDO;
040500150303
040600150303         //?se non ho trovato offerte valide
040700150303         //?è stato richiesto
040800150303         //?Trattative Avviate senza Offerta
040900150303           IF  not wOff and IKC76flgp = 'AN';
041000150303             leavesr;
041100150303           ENDIF;
041200150303         ENDIF;
041300150303
041400150303       //?se arrivo a questo punto qualcosa non è andato bene
041500150303       //?quindi ritorno errore
041600150303
041700150303         OKC76err = 'E';
041800150218
041900150218       ENDSR;
042000150218
042100150218       //--------------------------------------------------------------
042200150218       //?Trattativa.
042300150218       //--------------------------------------------------------------
042400150218       BEGSR Trattativa;
042500150220
042600150220       //?Aggancio la fase TR
042700150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:IKC76acm) TICMF01L;
042800150220       //?se non trovo la TR cerco se c'è la TF
042900150220         IF  not %found(TICMF01L);
043000150220           exsr Forzata;
043100150220         //?fine dei giochi
043200150220           leavesr;
043300150220         ENDIF;
043400150220
043500150220       //?OK ho trovato la TR
043600150220       //?Imposto già alcuni campi da tornare al chiamante
043700150302         OKC76dfa = CMFdfa;
043800150223         OKC76peaa = %editc(CMFpea:'J');
043900150223         OKC76pea  = CMFpea;
044000150220         OKC76nrv = CMFnrv;
044100150220
044200150220       //?Aggancio la Trattativa
044300150220         chain (CMFnrv) TIVIS05L;
044400150220         IF  not %found(TIVIS05L);
044500150220           OKC76err = 'E';
044600150220           OKC76msg = Msg(05);
044700150220           exsr RoutEnd;
044800150220         ENDIF;
044900150218
045000150218       //?Carico i dati dell'ISTAT solo la prima volta
045100150218         IF  not wPrimo;
045200150218           exsr CaricaISTAT;
045300150218           wPrimo = *on;
045400150218         ENDIF;
045500150218
045600150218       //?Cerco i dati da tornare al chiamante per la trattativa
045700150218         exsr DatiTTR;
045800150220
045900150218       //?Dati ISTAT
046000150220         IF  OKC76flag = 'N' and IKC76istat = 'S';
046100150218           exsr DatiISTAT;
046200150218         ENDIF;
046300150218
046400150218       ENDSR;
046500150218
046600150218       //--------------------------------------------------------------
046700150218       //?Confronto Fatturazione.
046800150218       //--------------------------------------------------------------
046900150218       BEGSR Confronto;
047000150218
047100150220       //?Aggancio la fase CF
047200150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:IKC76acm) TICMF01L;
047300150220         IF  not %found(TICMF01L);
047400150220           OKC76err = 'N';
047500150220           exsr RoutEnd;
047600150220         ENDIF;
047700150302
047800150302         OKC76dfa = CMFdfa;
047900150220
048000150220       //?Imposto la percentuale se è stato confrontato
048100150220         IF  CMFnocf <> 'N';
048200150223           OKC76peaa = %editc(CMFpea:'J');
048300150223           OKC76pea  = CMFpea;
048400150224         //?Imposto anno e mese del confronto
048500150224           OKC76anno = CMFaacf;
048600150224           OKC76mese = CMFmmcf;
048700150220         ENDIF;
048800150220       //?Imposto il falg a N se è un NON confrontato
048900150220         IF  CMFnocf = 'N';
049000150220           OKC76flag = 'N';
049100150220         ENDIF;
049200150414
049300150414       //?se campagna di tipo 'L'
049400150414       //?fine dei giochi passo quello che ho trovato
049500150414         IF  CMPtpc = 'L';
049600150414           exsr RoutEnd;
049700150414         ENDIF;
049800150414
049900150414       //?se campagna di tipo 'A'
050000150414       //?Data confronto fase CF deve essere uguale a data confronto campagna
050100150414       //?uso stesso campo data tariffa
050200150414         DataTar = (CMFaacf * 100) + CMFmmcf;
050300150414         IF  CMPtpc = 'A' and (DataConf <> DataTar or DataTar = 0);
050400150414           OKC76err = 'N';
050500150414           exsr RoutEnd;
050600150414           leavesr;
050700150414         ENDIF;
050800150220
050900150220       //?Cerco se c'è la TR
051000150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTtr) TICMF01L;
051100150220       //?se non trovo la TR cerco se c'è la TF
051200150220         IF  not %found(TICMF01L);
051300150220           chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTf) TICMF01L;
051400150220           IF  not %found(TICMF01L);
051500150220         //?se non trovo la TR e la TF è come se non ci fosse la fase CF
051600150220             OKC76err = 'N';
051700150220             exsr RoutEnd;
051800150220             leavesr;
051900150220           ENDIF;
052000150409         //?Se c'è la TF è come se fosse esito positivo e quindi
052100150409         //?devo verificare la data impostata per la fase TF con la data
052200150409         //?confronto fatturazione
052300150409           DataTar = (CMFaacf * 100) + CMFmmcf;
052400150409         //?se Data Confronto Fatturazione < Data Offerta Convalidata
052500150409         //?è come se non ci fosse la fase CF
052600150409           IF  DataConf < DataTar or DataTar = 0;
052700150409             OKC76err = 'N';
052800150409             exsr RoutEnd;
052900150409             leavesr;
053000150409           ENDIF;
053100150409         ENDIF;
053200150220
053300150223       //?Se c'è la TR devo verificare se la trattativa ha un esito
053400150220         IF  CMFnrv > 0;
053500150220           chain  (CMFnrv) TIVIS05L;
053600150409         //?non dovrebbe succedere
053700150409         //?ma se non trovo la trattativa esco con errore
053800150409           IF  not %found(TIVIS05L);
053900150409             OKC76err = 'E';
054000150409             exsr RoutEnd;
054100150409           ENDIF;
054200150409           SELECT;
054300150409         //?se la trattativa non ha esito è come se non ci fosse la fase CF
054400150409           WHEN  VISesi = *blanks;
054500150220             OKC76err = 'N';
054600150220             exsr RoutEnd;
054700150220             leavesr;
054800150409         //?se la trattativa ha esito negativo OK CF
054900150409           WHEN  VISesi <> 'P ';
055000150409             leavesr;
055100150409         //?se la trattativa ha esito positivo devo verificare che
055200150409         //?il periodo confronto fatturazione sia maggiore/uguale alla
055300150409         //?data della tariffa convalidata
055400150409           WHEN  VISesi = 'P ';
055500150422             NoTam = *off;
055600150409         //?cerco l'offerta convalidata
055700150409             setll (VISnrv) TIVOF11L;
055800150409             DOW  not wEnd;
055900150409               reade (VISnrv) TIVOF11L;
056000150409               IF  %eof(TIVOF11L);
056100150409                 leave;
056200150409               ENDIF;
056300150409               IF  VOFeso <> 'C';
056400150409                 iter;
056500150409               ENDIF;
056600150409             //?Cerco la tariffa convalidata
056700150409               chain (VOFksc:VOFctr:VOFprt) TNTAM01L;
056800150409               IF  not %found(TNTAM01L);
056900150409                 clear DataTar;
057000150422                 NoTam = *on;
057100150422                 iter;
057200150409               ENDIF;
057300150409               wAnno = %dec(%subst(%editc(TAMddt:'X'):1:4):4:0);
057400150409               wMese = %dec(%subst(%editc(TAMddt:'X'):5:2):2:0);
057500150409               DataTar = (wAnno * 100) + wMese;
057600150422               NoTam = *off;
057700150409               leave;
057800150409             ENDDO;
057900150422           //?se arrivo qua con NoTam attivo vuol dire che offerta convalidata
058000150422           //?ma tariffa cancellata
058100160128           //?in questo caso devo impostare come data tariffa
058200160128           //?Gennaio dell'anno inizio campagna
058300150422             IF  NoTam;
058400160128               Data_ISO = %date(CMPdic);
058500160128               Annodic  = %subdt(Data_ISO:*years);
058600160128               DataTar  = Annodic * 100 + 01;
058700150422             ENDIF;
058800150409           //?se Data Confronto Fatturazione < Data Offerta Convalidata
058900150409           //?è come se non ci fosse la fase CF
059000150409             IF  DataConf < DataTar or DataTar = 0;
059100150409               OKC76err = 'N';
059200150409               exsr RoutEnd;
059300150409               leavesr;
059400150409             ENDIF;
059500150409           ENDSL;
059600150220         ENDIF;
059700150218
059800150218       ENDSR;
059900150220
060000150220       //--------------------------------------------------------------
060100150220       //?Trattativa Forzata.
060200150220       //--------------------------------------------------------------
060300150220       BEGSR Forzata;
060400150220
060500150220       //?Aggancio la fase TF
060600150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTf) TICMF01L;
060700150220         IF  not %found(TICMF01L);
060800150220           OKC76err = 'N';
060900150220           exsr RoutEnd;
061000150220         ENDIF;
061100150220
061200150220       //?Imposto i campi da tornare al chiamante
061300150302         OKC76dfa = CMFdfa;
061400150225         OKC76flag  = 'P';
061500150225         OKC76peaa  = %editc(CMFpea:'J');
061600150225         OKC76pea   = CMFpea;
061700150225         OKC76anno  = CMFaacf;
061800150225         OKC76mese  = CMFmmcf;
061900150225         OKC76forza = 'F';
062000150220
062100150220       ENDSR;
062200150218
062300150218       //--------------------------------------------------------------
062400150218       //?Carico i dati dell'ISTAT.
062500150218       //--------------------------------------------------------------
062600150218       BEGSR CaricaISTAT;
062700150218
062800150218         open TISIS01L;
062900150218
063000150218       //?Carico sk ISTAT Data e % Scatto
063100150220         wEnd = *off;
063200150220         clear SISsca;
063300150220         setll (SISsca) TISIS01L;
063400150220         DOW  not wEnd;
063500150220           read TISIS01L;
063600150220           IF  %Eof(TISIS01L);
063700150220             wEnd = *on;
063800150220             leave;
063900150220           ENDIF;
064000150220           IstatData(SISsca) = SISdata;
064100150220           IstatPerc(SISsca) = SISpunti;
064200150220         ENDDO;
064300150218
064400150218         close TISIS01L;
064500150217
064600150217       ENDSR;
064700150217
064800150217       //--------------------------------------------------------------
064900150217       //?Recupero di dati da tornare al chiamante.
065000150217       //--------------------------------------------------------------
065100150217       BEGSR DatiTTR;
065200150223
065300150223         wEnd = *off;
065400150217
065500150217       //?Se la trattativa è chiusa
065600150414         IF  VISesi <> *blanks;
065700150217         //?Se esito NON Positivo imposto 'N' di negativo
065800150217         //?azzero andamento TR
065900150217         //?e vado via
066000150217           IF  VISesi <> 'P ';
066100150220             OKC76flag = 'N';
066200150223             clear OKC76peaa;
066300150223             clear OKC76pea;
066400150217             leavesr;
066500150217           ENDIF;
066600150220           OKC76flag = 'P';
066700150217
066800150217         //?Se esito Positivo cerco l'offerta convalidata
066900150217         //?per impostare il mese di decorrenza della tariffa convalidata
067000150422           NoTam = *off;
067100150217           setll (VISnrv) TIVOF11L;
067200150217           DOW  not wEnd;
067300150217             reade (VISnrv) TIVOF11L;
067400150217             IF  %eof(TIVOF11L);
067500150409               leave;
067600150217             ENDIF;
067700150217             IF  VOFeso <> 'C';
067800150217               iter;
067900150217             ENDIF;
068000150217           //?Cerco la tariffa convalidata
068100150217             chain (VOFksc:VOFctr:VOFprt) TNTAM01L;
068200150217             IF  not %found(TNTAM01L);
068300150422               NoTam = *on;
068400150422               iter;
068500150217             ENDIF;
068600150422             NoTam = *off;
068700150224             OKC76anno = %dec(%subst(%editc(TAMddt:'X'):1:4):4:0);
068800150224             OKC76mese = %dec(%subst(%editc(TAMddt:'X'):5:2):2:0);
068900150409             leave;
069000150217           ENDDO;
069100150422         //?se arrivo qua con NoTam attivo vuol dire che offerta convalidata
069200150422         //?ma tariffa cancellata
069300160128         //?in questo caso devo impostare come data tariffa
069400160128         //?Gennaio dell'anno inizio campagna
069500150422           IF  NoTam;
069600160128               Data_ISO  = %date(CMPdic);
069700160128               Annodic   = %subdt(Data_ISO:*years);
069800160128               OKC76anno = Annodic;
069900160128               OKC76mese = 01;
070000150422           ENDIF;
070100150217         ENDIF;
070200150217
070300150217       //?Se la trattativa non è chiusa
070400150414         IF  VISesi = *blanks;
070500150217         //?Leggo le offerte (no le 'H'-congelata e '*'-annullata)
070600150217           setll (VISnrv) TIVOF11L;
070700150217           reade (VISnrv) TIVOF11L;
070800150217           DOW not %eof(TIVOF11L);
070900150217         //?se trovo anche solo 1 offerta valida
071000150217         //?imposto 'A' o 'P' in caso di esito Positivo
071100150217         //?e vado via
071200150217             IF  VOFeso <> 'H' and VOFeso <> '*';
071300150217               IF  VISesi = 'P ';
071400150220                 OKC76flag = VISesi;
071500150217               ELSE;
071600150220                 OKC76flag = 'A';
071700150217               ENDIF;
071800150217               leavesr;
071900150217             ENDIF;
072000150217             reade (VISnrv) TIVOF11L;
072100150217           ENDDO;
072200150217         //?se arrivo qua vuol dire che non ho trovato nessuna
072300150217         //?offerta valida
072400150217         //?imposto 'A' lo stesso o la 'P' se esito positivo
072500150217         //?ma azzero andamento TR
072600150217           IF  VISesi = 'P ';
072700150220             OKC76flag = VISesi;
072800150217           ELSE;
072900150220             OKC76flag = 'A';
073000150217           ENDIF;
073100150223           clear OKC76peaa;
073200150223           clear OKC76pea;
073300150217         ENDIF;
073400150217
073500150217       ENDSR;
073600150217
073700150217       //--------------------------------------------------------------
073800150217       //?Dati ISTAT.
073900150217       //--------------------------------------------------------------
074000150217       BEGSR DatiISTAT;
074100150224
074200150217       //?Cerco anno corrente
074300150306         Data_ISO = %date(Oggi);
074400150306         Anno     = %subdt(Data_ISO:*years);
074500150217
074600150217       //?Cerco percentuale Istat in corso
074700150217         xx = 1;
074800150217         FOR xx by 1 to %elem(IstatData);
074900150217           IF  IstatData(xx) = 0;
075000150217             leave;
075100150217           ENDIF;
075200150217           wData = IstatData(xx);
075300150306           Data_ISO = %date(IstatData(xx));
075400150306           AnnoIstat = %subdt(Data_ISO:*years);
075500150217           IF  Anno = AnnoIstat;
075600150223             OKC76peaa = %editc(IstatPerc(xx):'J');
075700150223             OKC76pea  = IstatPerc(xx);
075800150217             leave;
075900150217           ENDIF;
076000150217         ENDFOR;
076100150217
076200150217       ENDSR;
076300150217
076400150217       //--------------------------------------------------------------
076500150217       //?Operazioni finali.
076600150217       //--------------------------------------------------------------
076700150217       BEGSR RoutEnd;
076800150217
076900150218         *inRT = *on;
077000150217         return;
077100150217
077200150217       ENDSR;
077300150217
077400150217      /end-free
077500150217
077600150217       //--------------------------------------------------------------
077700150217       //?Schiere a tempo di compilazione.
077800150217       //--------------------------------------------------------------
077900150217
078000150217** -- MSG -------------------------------------------------------------------*
078100150218Tipo richiamo errato                                                           1
078200150218Campagna commerciale errata                                                    2
078300150218Cliente/Potenziale errato                                                      3
078400150218Fase errata                                                                    4
078500150218Trattativa errata                                                              5
