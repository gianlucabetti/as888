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
008900150217     d AnnoIstat       s              4s 0 inz
009000150409     d DataConf        s              6s 0 inz
009100150409     d DataTar         s              6s 0 inz
009200150217     d Oggi            s              8s 0 inz
009300150409     d wAnno           s                   like(OKC76anno)
009400150409     d wMese           s                   like(OKC76mese)
009500150217
009600150217      // - Indici di schiera
009700150217     d xx              s              4s 0 inz
009800150217
009900150217      // - Flags booleani
010000150422     d NoTam           s               n   inz(*off)
010100150217     d wEnd            s               n   inz(*off)
010200150303     d wOff            s               n   inz(*off)
010300150218     d wPrimo          s               n   inz(*off)
010400150217
010500150217      //---------------------------------------------------------------
010600150217      //?Definizione procedure usate.
010700150217      //---------------------------------------------------------------
010800150217
010900150217      //---------------------------------------------------------------
011000150217      //?Definizione Prototipi.
011100150217      //---------------------------------------------------------------
011200150218
011300150218      /copy gaitrasrc/srcprotopr,TIBS02R
011400150217      /copy gaitrasrc/srcprotopr,TIBS34R
011500150217      /copy gaitrasrc/srcprotopr,TIBS69R
011600150217
011700150217      //---------------------------------------------------------------
011800150217      //?Definizione key-list.
011900150217      //---------------------------------------------------------------
012000150217
012100150217      //---------------------------------------------------------------
012200150217      //?M A I N - L I N E
012300150217      //---------------------------------------------------------------
012400150217
012500150217     c     *Entry        plist
012600150217     c                   parm                    kpjba
012700150217     c                   parm                    TRKC76DS
012800150217
012900150217      /free
013000150217
013100150217       //?Operazioni iniziali
013200150217       exsr RoutInz;
013300150217
013400150217       //?Controllo dati passati
013500150218       IF  IKC76ric <> 'C';
013600150218         exsr Controlla;
013700150218       ENDIF;
013800150218
013900150218       //?Elaborazioni diverse in base alla fase passata
014000150218       SELECT;
014100150218
014200150218       //?Elaborazione per Chiusura file
014300150218       WHEN  IKC76ric = 'C';
014400150218         *inlr = *on;
014500150218         return;
014600150218       //?Elaborazione per Parzializzazione
014700150218       WHEN  IKC76ric = 'P';
014800150218         exsr Parzializza;
014900150218       //?Elaborazione per Fase Trattativa
015000150218       WHEN  IKC76acm = FaseObjTtr;
015100150218         exsr Trattativa;
015200150218       //?Elaborazione per Fase Confronto Fatturazione
015300150218       WHEN  IKC76acm = FaseObjCf;
015400150218         exsr Confronto;
015500150218
015600150218       ENDSL;
015700150217
015800150217       //?Operazioni finali
015900150217       exsr RoutEnd;
016000150217
016100150217       //--------------------------------------------------------------
016200150217       //?Operazioni iniziali.
016300150217       //--------------------------------------------------------------
016400150217       BEGSR RoutInz;
016500150217
016600150217       //?Imposto oggi
016700150217         Oggi = %dec(%date());
016800150217
016900150217       //?Reperimento dati job
017000150217         exsr DatiJob;
017100150217
017200150302         clear OKC76dfa;
017300150220         clear OKC76flag;
017400150223         clear OKC76peaa;
017500150217         clear OKC76pea;
017600150224         clear OKC76anno;
017700150224         clear OKC76mese;
017800150220         clear OKC76nrv;
017900150225         clear OKC76forza;
018000150217         clear OKC76err;
018100150217         clear OKC76msg;
018200150217
018300150217       ENDSR;
018400150217
018500150217       //--------------------------------------------------------------
018600150217       //?Reperimento Dati del job (Utente/Operativi).
018700150217       //--------------------------------------------------------------
018800150217       BEGSR DatiJob;
018900150217
019000150217         in(E) §AzUte;
019100150217         IF  NOT %error;
019200150217           in(E) §DatiUte;
019300150217         ENDIF;
019400150217         IF  %error or RSut = *blanks;
019500150217           clear TIBS34ds;
019600150217           tibs34r(tibs34ds);
019700150217           in §AzUte;
019800150217           in §DatiUte;
019900150217         ENDIF;
020000150217
020100150217       ENDSR;
020200150217
020300150217       //--------------------------------------------------------------
020400150217       //?Controllo i dati passati.
020500150217       //--------------------------------------------------------------
020600150217       BEGSR Controlla;
020700150218
020800150218       //?Tipo richiamo
020900150218         IF  IKC76ric <> 'P' and IKC76ric <> 'F';
021000150218           OKC76err = 'E';
021100150218           OKC76msg = Msg(01);
021200150218           exsr RoutEnd;
021300150218         ENDIF;
021400150217
021500150217       //?Numero campagna obbligatorio
021600150217         IF  IKC76ncm = 0;
021700150217           OKC76err = 'E';
021800150218           OKC76msg = Msg(02);
021900150217           exsr RoutEnd;
022000150217         ENDIF;
022100150217       //?Deve essere una campagna valida
022200150217         chain (IKC76ncm) TICMP01L;
022300150409         IF  not %found(TICMP01L);
022400150217           OKC76err = 'E';
022500150218           OKC76msg = Msg(02);
022600150217           exsr RoutEnd;
022700150217         ENDIF;
022800150409         IF  %subst(CMPflo:1:6) > *zeros;
022900150409           wAnno = %dec(%subst(CMPflo:1:4):4:0);
023000150409           wMese = %dec(%subst(CMPflo:5:2):2:0);
023100150409           DataConf = (wAnno * 100) + wMese;
023200150409         ENDIF;
023300150217
023400150217       //?Almeno 1 tra KSU/KSC/CPO deve esserci
023500150217         IF  IKC76ksu = 0 and IKC76ksc = 0 and IKC76cpo = 0;
023600150217           OKC76err = 'E';
023700150218           OKC76msg = Msg(03);
023800150217           exsr RoutEnd;
023900150217         ENDIF;
024000150217       //?Deve essere una codice valido
024100150217         IF  IKC76ksu > 0;
024200150217           clear TIBS69DS;
024300150217           I69kac = %dec(IKC76ksu:7:0);
024400150217           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
024500150217           IF  O69err <> *blanks;
024600150217             OKC76err = 'E';
024700150218             OKC76msg = Msg(03);
024800150217             exsr RoutEnd;
024900150217           ENDIF;
025000150217         ENDIF;
025100150217         IF  IKC76ksc > 0;
025200150217           clear TIBS69DS;
025300150217           I69kac = %dec(IKC76ksc:7:0);
025400150217           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
025500150217           IF  O69err <> *blanks;
025600150217             OKC76err = 'E';
025700150218             OKC76msg = Msg(03);
025800150217             exsr RoutEnd;
025900150217           ENDIF;
026000150217         ENDIF;
026100150217         IF  IKC76cpo > 0;
026200150217           chain (IKC76cpo) TNCPO01L;
026300150217           IF  not %found(TNCPO01L);
026400150217             OKC76err = 'E';
026500150218             OKC76msg = Msg(03);
026600150217             exsr RoutEnd;
026700150217           ENDIF;
026800150217         ENDIF;
026900150303
027000150303       //?Per Parzializzazione esco dai controlli
027100150303         IF  IKC76ric = 'P';
027200150303           leavesr;
027300150303         ENDIF;
027400150218
027500150218       //?Fase obbligatoria
027600150218         IF  IKC76acm = *blanks;
027700150218           OKC76err = 'E';
027800150218           OKC76msg = Msg(04);
027900150218           exsr RoutEnd;
028000150218         ENDIF;
028100150218       //?Deve essere una fase valida
028200150218         clear TIBS02DS;
028300150218         T02mod = 'C';
028400150218         T02cod = 'FCM';
028500150218         T02ke1 = IKC76acm;
028600150218         T02sif = KNSIF;
028700150218         TNTBE_RicercaControllo  (kpjba : tibs02ds);
028800150218         IF  T02err <> *blanks;
028900150218           OKC76err = 'E';
029000150218           OKC76msg = Msg(04);
029100150218           exsr RoutEnd;
029200150218         ENDIF;
029300150218
029400150218       ENDSR;
029500150218
029600150218       //--------------------------------------------------------------
029700150218       //?Parzializzazione Trattative.
029800150218       //--------------------------------------------------------------
029900150218       BEGSR Parzializza;
030000150303
030100150303         wOff = *off;
030200150303
030300150303       //?Parzializza è fatta SOLO per le trattative
030400150303       //?quindi la fase TR la cerco subito
030500150303         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTtr) TICMF01L;
030600150303
030700150303       //?Trattative avviate
030800150303       //?Trattative avviate con offerta
030900150303       //?Trattative avviate senza offerta
031000150303       //?Trattative non forzate
031100150303       //?deve esserci la fase TR
031200150303         IF  (IKC76flgp = 'A ' or IKC76flgp = 'AO' or
031300150303              IKC76flgp = 'AN' or IKC76flgp = 'T ') and
031400150303               not %found(TICMF01L);
031500150303           OKC76err = 'E';
031600150303           exsr RoutEnd;
031700150303         ENDIF;
031800150303
031900150303       //?Trattative non forzate
032000150303       //?e ho la fase TR ok
032100150303         IF  %found(TICMF01L) and IKC76flgp = 'T ';
032200150303           leavesr;
032300150303         ENDIF;
032400150303
032500150303       //?Se non ho la fase TR provo con la fase TF
032600150303         IF  not %found(TICMF01L);
032700150303           chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTf) TICMF01L;
032800150303         //?se non trovo neanche la TF non ho trattative quindi vado via
032900150303           IF  not %found(TICMF01L);
033000150303             OKC76err = 'E';
033100150303             exsr RoutEnd;
033200150303           ENDIF;
033300150303         ENDIF;
033400150303
033500150303       //?Se non ho il numero della trattativa è una Trattativa Forzata
033600150303         IF  CMFnrv = 0 and IKC76flgp = 'F ';
033700150303           leavesr;
033800150303         ENDIF;
033900150303
034000150303       //?Trattativa forzata è una trattativa chiusa positiva
034100150303         IF  CMFnrv = 0 and IKC76flgp = 'CN';
034200150303           OKC76err = 'E';
034300150303           exsr RoutEnd;
034400150303         ENDIF;
034500150303
034600150303       //?Se non ho il numero della trattativa
034700150303       //?e sono state richieste
034800150303       //?Trattative Chiuse o
034900150303       //?Trattative Chiuse Positive
035000150303       //?fine dei giochi
035100150303         IF  CMFnrv = 0 and (IKC76flgp = 'C ' or IKC76flgp = 'CP');
035200150303           leavesr;
035300150303         ENDIF;
035400150303
035500150303       //?Se arrivo qua ho il numero trattativa quindi aggancio la trattativa
035600150303         chain (CMFnrv) TIVIS05L;
035700150303         //?non dovrebbe succedere
035800150303         //?ma se non trovo la trattativa esco con errore
035900150303         IF  not %found(TIVIS05L);
036000150303           OKC76err = 'E';
036100150303           exsr RoutEnd;
036200150303         ENDIF;
036300150303
036400150303       //?Se la trattativa ha un esito
036500150303         IF  VISesi <> *blanks;
036600150303       //?ed è stato richiesto
036700150303           SELECT;
036800150303         //?Trattative Chiuse
036900150303           WHEN  IKC76flgp = 'C ';
037000150303             leavesr;
037100150303         //?Trattative Chiuse Positive
037200150303           WHEN  VISesi = 'P ' and IKC76flgp = 'CP';
037300150303             leavesr;
037400150303         //?Trattative Chiuse No Positive
037500150303           WHEN  VISesi <> 'P ' and IKC76flgp = 'CN';
037600150303             leavesr;
037700150303           ENDSL;
037800150303         ENDIF;
037900150303
038000150303       //?Se la trattativa non ha un esito
038100150303         IF  VISesi = *blanks;
038200150303         //?ed è stato richiesto
038300150303         //?Trattative Avviate
038400150303           IF  IKC76flgp = 'A ';
038500150303             leavesr;
038600150303           ENDIF;
038700150303
038800150303         //?Leggo le offerte (no le 'H'-congelata e '*'-annullata)
038900150303           setll (VISnrv) TIVOF11L;
039000150303           reade (VISnrv) TIVOF11L;
039100150303           DOW not %eof(TIVOF11L);
039200150303         //?se trovo anche solo 1 offerta valida
039300150303             IF  VOFeso <> 'H' and VOFeso <> '*';
039400150303         //?è stato richiesto
039500150303         //?Trattative Avviate con Offerta
039600150303               IF  IKC76flgp = 'AO';
039700150303                 leavesr;
039800150303               ENDIF;
039900150303              wOff = *on;
040000150303              leave;
040100150303             ENDIF;
040200150303             reade (VISnrv) TIVOF11L;
040300150303           ENDDO;
040400150303
040500150303         //?se non ho trovato offerte valide
040600150303         //?è stato richiesto
040700150303         //?Trattative Avviate senza Offerta
040800150303           IF  not wOff and IKC76flgp = 'AN';
040900150303             leavesr;
041000150303           ENDIF;
041100150303         ENDIF;
041200150303
041300150303       //?se arrivo a questo punto qualcosa non è andato bene
041400150303       //?quindi ritorno errore
041500150303
041600150303         OKC76err = 'E';
041700150218
041800150218       ENDSR;
041900150218
042000150218       //--------------------------------------------------------------
042100150218       //?Trattativa.
042200150218       //--------------------------------------------------------------
042300150218       BEGSR Trattativa;
042400150220
042500150220       //?Aggancio la fase TR
042600150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:IKC76acm) TICMF01L;
042700150220       //?se non trovo la TR cerco se c'è la TF
042800150220         IF  not %found(TICMF01L);
042900150220           exsr Forzata;
043000150220         //?fine dei giochi
043100150220           leavesr;
043200150220         ENDIF;
043300150220
043400150220       //?OK ho trovato la TR
043500150220       //?Imposto già alcuni campi da tornare al chiamante
043600150302         OKC76dfa = CMFdfa;
043700150223         OKC76peaa = %editc(CMFpea:'J');
043800150223         OKC76pea  = CMFpea;
043900150220         OKC76nrv = CMFnrv;
044000150220
044100150220       //?Aggancio la Trattativa
044200150220         chain (CMFnrv) TIVIS05L;
044300150220         IF  not %found(TIVIS05L);
044400150220           OKC76err = 'E';
044500150220           OKC76msg = Msg(05);
044600150220           exsr RoutEnd;
044700150220         ENDIF;
044800150218
044900150218       //?Carico i dati dell'ISTAT solo la prima volta
045000150218         IF  not wPrimo;
045100150218           exsr CaricaISTAT;
045200150218           wPrimo = *on;
045300150218         ENDIF;
045400150218
045500150218       //?Cerco i dati da tornare al chiamante per la trattativa
045600150218         exsr DatiTTR;
045700150220
045800150218       //?Dati ISTAT
045900150220         IF  OKC76flag = 'N' and IKC76istat = 'S';
046000150218           exsr DatiISTAT;
046100150218         ENDIF;
046200150218
046300150218       ENDSR;
046400150218
046500150218       //--------------------------------------------------------------
046600150218       //?Confronto Fatturazione.
046700150218       //--------------------------------------------------------------
046800150218       BEGSR Confronto;
046900150218
047000150220       //?Aggancio la fase CF
047100150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:IKC76acm) TICMF01L;
047200150220         IF  not %found(TICMF01L);
047300150220           OKC76err = 'N';
047400150220           exsr RoutEnd;
047500150220         ENDIF;
047600150302
047700150302         OKC76dfa = CMFdfa;
047800150220
047900150220       //?Imposto la percentuale se è stato confrontato
048000150220         IF  CMFnocf <> 'N';
048100150223           OKC76peaa = %editc(CMFpea:'J');
048200150223           OKC76pea  = CMFpea;
048300150224         //?Imposto anno e mese del confronto
048400150224           OKC76anno = CMFaacf;
048500150224           OKC76mese = CMFmmcf;
048600150220         ENDIF;
048700150220       //?Imposto il falg a N se è un NON confrontato
048800150220         IF  CMFnocf = 'N';
048900150220           OKC76flag = 'N';
049000150220         ENDIF;
049100150414
049200150414       //?se campagna di tipo 'L'
049300150414       //?fine dei giochi passo quello che ho trovato
049400150414         IF  CMPtpc = 'L';
049500150414           exsr RoutEnd;
049600150414         ENDIF;
049700150414
049800150414       //?se campagna di tipo 'A'
049900150414       //?Data confronto fase CF deve essere uguale a data confronto campagna
050000150414       //?uso stesso campo data tariffa
050100150414         DataTar = (CMFaacf * 100) + CMFmmcf;
050200150414         IF  CMPtpc = 'A' and (DataConf <> DataTar or DataTar = 0);
050300150414           OKC76err = 'N';
050400150414           exsr RoutEnd;
050500150414           leavesr;
050600150414         ENDIF;
050700150220
050800150220       //?Cerco se c'è la TR
050900150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTtr) TICMF01L;
051000150220       //?se non trovo la TR cerco se c'è la TF
051100150220         IF  not %found(TICMF01L);
051200150220           chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTf) TICMF01L;
051300150220           IF  not %found(TICMF01L);
051400150220         //?se non trovo la TR e la TF è come se non ci fosse la fase CF
051500150220             OKC76err = 'N';
051600150220             exsr RoutEnd;
051700150220             leavesr;
051800150220           ENDIF;
051900150409         //?Se c'è la TF è come se fosse esito positivo e quindi
052000150409         //?devo verificare la data impostata per la fase TF con la data
052100150409         //?confronto fatturazione
052200150409           DataTar = (CMFaacf * 100) + CMFmmcf;
052300150409         //?se Data Confronto Fatturazione < Data Offerta Convalidata
052400150409         //?è come se non ci fosse la fase CF
052500150409           IF  DataConf < DataTar or DataTar = 0;
052600150409             OKC76err = 'N';
052700150409             exsr RoutEnd;
052800150409             leavesr;
052900150409           ENDIF;
053000150409         ENDIF;
053100150220
053200150223       //?Se c'è la TR devo verificare se la trattativa ha un esito
053300150220         IF  CMFnrv > 0;
053400150220           chain  (CMFnrv) TIVIS05L;
053500150409         //?non dovrebbe succedere
053600150409         //?ma se non trovo la trattativa esco con errore
053700150409           IF  not %found(TIVIS05L);
053800150409             OKC76err = 'E';
053900150409             exsr RoutEnd;
054000150409           ENDIF;
054100150409           SELECT;
054200150409         //?se la trattativa non ha esito è come se non ci fosse la fase CF
054300150409           WHEN  VISesi = *blanks;
054400150220             OKC76err = 'N';
054500150220             exsr RoutEnd;
054600150220             leavesr;
054700150409         //?se la trattativa ha esito negativo OK CF
054800150409           WHEN  VISesi <> 'P ';
054900150409             leavesr;
055000150409         //?se la trattativa ha esito positivo devo verificare che
055100150409         //?il periodo confronto fatturazione sia maggiore/uguale alla
055200150409         //?data della tariffa convalidata
055300150409           WHEN  VISesi = 'P ';
055400150422             NoTam = *off;
055500150409         //?cerco l'offerta convalidata
055600150409             setll (VISnrv) TIVOF11L;
055700150409             DOW  not wEnd;
055800150409               reade (VISnrv) TIVOF11L;
055900150409               IF  %eof(TIVOF11L);
056000150409                 leave;
056100150409               ENDIF;
056200150409               IF  VOFeso <> 'C';
056300150409                 iter;
056400150409               ENDIF;
056500150409             //?Cerco la tariffa convalidata
056600150409               chain (VOFksc:VOFctr:VOFprt) TNTAM01L;
056700150409               IF  not %found(TNTAM01L);
056800150409                 clear DataTar;
056900150422                 NoTam = *on;
057000150422                 iter;
057100150409               ENDIF;
057200150409               wAnno = %dec(%subst(%editc(TAMddt:'X'):1:4):4:0);
057300150409               wMese = %dec(%subst(%editc(TAMddt:'X'):5:2):2:0);
057400150409               DataTar = (wAnno * 100) + wMese;
057500150422               NoTam = *off;
057600150409               leave;
057700150409             ENDDO;
057800150422           //?se arrivo qua con NoTam attivo vuol dire che offerta convalidata
057900150422           //?ma tariffa cancellata
058000150422           //?in questo caso devo impostare come data tariffa 01/2015
058100150422             IF  NoTam;
058200150422               DataTar = 201501;
058300150422             ENDIF;
058400150409           //?se Data Confronto Fatturazione < Data Offerta Convalidata
058500150409           //?è come se non ci fosse la fase CF
058600150409             IF  DataConf < DataTar or DataTar = 0;
058700150409               OKC76err = 'N';
058800150409               exsr RoutEnd;
058900150409               leavesr;
059000150409             ENDIF;
059100150409           ENDSL;
059200150220         ENDIF;
059300150218
059400150218       ENDSR;
059500150220
059600150220       //--------------------------------------------------------------
059700150220       //?Trattativa Forzata.
059800150220       //--------------------------------------------------------------
059900150220       BEGSR Forzata;
060000150220
060100150220       //?Aggancio la fase TF
060200150220         chain (IKC76ncm:IKC76ksu:IKC76ksc:IKC76cpo:FaseObjTf) TICMF01L;
060300150220         IF  not %found(TICMF01L);
060400150220           OKC76err = 'N';
060500150220           exsr RoutEnd;
060600150220         ENDIF;
060700150220
060800150220       //?Imposto i campi da tornare al chiamante
060900150302         OKC76dfa = CMFdfa;
061000150225         OKC76flag  = 'P';
061100150225         OKC76peaa  = %editc(CMFpea:'J');
061200150225         OKC76pea   = CMFpea;
061300150225         OKC76anno  = CMFaacf;
061400150225         OKC76mese  = CMFmmcf;
061500150225         OKC76forza = 'F';
061600150220
061700150220       ENDSR;
061800150218
061900150218       //--------------------------------------------------------------
062000150218       //?Carico i dati dell'ISTAT.
062100150218       //--------------------------------------------------------------
062200150218       BEGSR CaricaISTAT;
062300150218
062400150218         open TISIS01L;
062500150218
062600150218       //?Carico sk ISTAT Data e % Scatto
062700150220         wEnd = *off;
062800150220         clear SISsca;
062900150220         setll (SISsca) TISIS01L;
063000150220         DOW  not wEnd;
063100150220           read TISIS01L;
063200150220           IF  %Eof(TISIS01L);
063300150220             wEnd = *on;
063400150220             leave;
063500150220           ENDIF;
063600150220           IstatData(SISsca) = SISdata;
063700150220           IstatPerc(SISsca) = SISpunti;
063800150220         ENDDO;
063900150218
064000150218         close TISIS01L;
064100150217
064200150217       ENDSR;
064300150217
064400150217       //--------------------------------------------------------------
064500150217       //?Recupero di dati da tornare al chiamante.
064600150217       //--------------------------------------------------------------
064700150217       BEGSR DatiTTR;
064800150223
064900150223         wEnd = *off;
065000150217
065100150217       //?Se la trattativa è chiusa
065200150414         IF  VISesi <> *blanks;
065300150217         //?Se esito NON Positivo imposto 'N' di negativo
065400150217         //?azzero andamento TR
065500150217         //?e vado via
065600150217           IF  VISesi <> 'P ';
065700150220             OKC76flag = 'N';
065800150223             clear OKC76peaa;
065900150223             clear OKC76pea;
066000150217             leavesr;
066100150217           ENDIF;
066200150220           OKC76flag = 'P';
066300150217
066400150217         //?Se esito Positivo cerco l'offerta convalidata
066500150217         //?per impostare il mese di decorrenza della tariffa convalidata
066600150422           NoTam = *off;
066700150217           setll (VISnrv) TIVOF11L;
066800150217           DOW  not wEnd;
066900150217             reade (VISnrv) TIVOF11L;
067000150217             IF  %eof(TIVOF11L);
067100150409               leave;
067200150217             ENDIF;
067300150217             IF  VOFeso <> 'C';
067400150217               iter;
067500150217             ENDIF;
067600150217           //?Cerco la tariffa convalidata
067700150217             chain (VOFksc:VOFctr:VOFprt) TNTAM01L;
067800150217             IF  not %found(TNTAM01L);
067900150422               NoTam = *on;
068000150422               iter;
068100150217             ENDIF;
068200150422             NoTam = *off;
068300150224             OKC76anno = %dec(%subst(%editc(TAMddt:'X'):1:4):4:0);
068400150224             OKC76mese = %dec(%subst(%editc(TAMddt:'X'):5:2):2:0);
068500150409             leave;
068600150217           ENDDO;
068700150422         //?se arrivo qua con NoTam attivo vuol dire che offerta convalidata
068800150422         //?ma tariffa cancellata
068900150422         //?in questo caso devo impostare come data tariffa 01/2015
069000150422           IF  NoTam;
069100150422             OKC76anno = 2015;
069200150422             OKC76mese = 01;
069300150422           ENDIF;
069400150217         ENDIF;
069500150217
069600150217       //?Se la trattativa non è chiusa
069700150414         IF  VISesi = *blanks;
069800150217         //?Leggo le offerte (no le 'H'-congelata e '*'-annullata)
069900150217           setll (VISnrv) TIVOF11L;
070000150217           reade (VISnrv) TIVOF11L;
070100150217           DOW not %eof(TIVOF11L);
070200150217         //?se trovo anche solo 1 offerta valida
070300150217         //?imposto 'A' o 'P' in caso di esito Positivo
070400150217         //?e vado via
070500150217             IF  VOFeso <> 'H' and VOFeso <> '*';
070600150217               IF  VISesi = 'P ';
070700150220                 OKC76flag = VISesi;
070800150217               ELSE;
070900150220                 OKC76flag = 'A';
071000150217               ENDIF;
071100150217               leavesr;
071200150217             ENDIF;
071300150217             reade (VISnrv) TIVOF11L;
071400150217           ENDDO;
071500150217         //?se arrivo qua vuol dire che non ho trovato nessuna
071600150217         //?offerta valida
071700150217         //?imposto 'A' lo stesso o la 'P' se esito positivo
071800150217         //?ma azzero andamento TR
071900150217           IF  VISesi = 'P ';
072000150220             OKC76flag = VISesi;
072100150217           ELSE;
072200150220             OKC76flag = 'A';
072300150217           ENDIF;
072400150223           clear OKC76peaa;
072500150223           clear OKC76pea;
072600150217         ENDIF;
072700150217
072800150217       ENDSR;
072900150217
073000150217       //--------------------------------------------------------------
073100150217       //?Dati ISTAT.
073200150217       //--------------------------------------------------------------
073300150217       BEGSR DatiISTAT;
073400150224
073500150217       //?Cerco anno corrente
073600150306         Data_ISO = %date(Oggi);
073700150306         Anno     = %subdt(Data_ISO:*years);
073800150217
073900150217       //?Cerco percentuale Istat in corso
074000150217         xx = 1;
074100150217         FOR xx by 1 to %elem(IstatData);
074200150217           IF  IstatData(xx) = 0;
074300150217             leave;
074400150217           ENDIF;
074500150217           wData = IstatData(xx);
074600150306           Data_ISO = %date(IstatData(xx));
074700150306           AnnoIstat = %subdt(Data_ISO:*years);
074800150217           IF  Anno = AnnoIstat;
074900150223             OKC76peaa = %editc(IstatPerc(xx):'J');
075000150223             OKC76pea  = IstatPerc(xx);
075100150217             leave;
075200150217           ENDIF;
075300150217         ENDFOR;
075400150217
075500150217       ENDSR;
075600150217
075700150217       //--------------------------------------------------------------
075800150217       //?Operazioni finali.
075900150217       //--------------------------------------------------------------
076000150217       BEGSR RoutEnd;
076100150217
076200150218         *inRT = *on;
076300150217         return;
076400150217
076500150217       ENDSR;
076600150217
076700150217      /end-free
076800150217
076900150217       //--------------------------------------------------------------
077000150217       //?Schiere a tempo di compilazione.
077100150217       //--------------------------------------------------------------
077200150217
077300150217** -- MSG -------------------------------------------------------------------*
077400150218Tipo richiamo errato                                                           1
077500150218Campagna commerciale errata                                                    2
077600150218Cliente/Potenziale errato                                                      3
077700150218Fase errata                                                                    4
077800150218Trattativa errata                                                              5
