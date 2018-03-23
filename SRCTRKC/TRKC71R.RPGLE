000100141106      //--------------------------------------------------------------
000200150112      //?TRKC71R - SCRITTURA FASE CAMPAGNA COMMERCIALE
000300141106      //--------------------------------------------------------------
000400141106
000500141106     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141106
000700141106      //---------------------------------------------------------------
000800141106      //?Dichiarazione file.
000900141106      //---------------------------------------------------------------
001000141106
001100141106      // - File Clienti in Campagna
001200141106     fTICMC01L  uf   e           k disk
001300141106
001400141106      // - File fasi Campagna
001500141106     fTICMF00F  o    e             disk
001600141114
001700141114      // - Anagrafica Campagna
001800141114     fTICMP01L  if   e           k disk
001900141114
002000141114      // - Anagrafica Potenziali
002100141114     fTNCPO01L  if   e           k disk
002200141106
002300141106      //---------------------------------------------------------------
002400141106      //?Definizione costanti.
002500141106      //---------------------------------------------------------------
002600141112     d FaseObjTtr      c                   const(' TR')
002700141106
002800141106      //---------------------------------------------------------------
002900141106      //?Definizione schiere.
003000141106      //---------------------------------------------------------------
003100141106
003200141106      // - Messaggi di errore
003300141106     d Msg             s             78    dim(20) ctdata perrcd(1)
003400141106
003500141106      //---------------------------------------------------------------
003600141106      //?Definizione aree dati.
003700141106      //---------------------------------------------------------------
003800141106
003900141106      // - Dati utente
004000141106     d §AzUte        e ds                  extname(AZUTE00F)
004100141106     d                                     dtaara
004200141106     d §DatiUte      e ds                  extname(dDatiUte)
004300141106     d                                     dtaara
004400141106
004500141106      //---------------------------------------------------------------
004600141106      //?Definizione strutture dati.
004700141106      //---------------------------------------------------------------
004800141106
004900141106      // - Status
005000141106     d Psds           sds
005100141106     d   SDSpgm          *proc
005200141106
005300141106      // - Parametri ricevuti
005400141106     d KPJBA         e ds
005500150112     d TRKC71DS      e ds
005600141106
005700141106      // - Ricerca/Controllo tabelle
005800141106     d TIBS02DS      e ds                  inz
005900141106
006000141106      // - Reperimento dati utente
006100141106     d TIBS34DS      e ds
006200141114
006300141114      // - Reperimento dati Anagrafica Clienti
006400141114      /copy gaitrasrc/srcprotopi,TIBS69R
006500141106
006600141106      // - Tabella FCM - Fase Campagna
006700141106     d dFCM          e ds
006800141106
006900141106      //---------------------------------------------------------------
007000141106      //?Definizione variabili globali.
007100141106      //---------------------------------------------------------------
007200141106
007300141106      // - Campi di comodo data
007400141106     d Data_EUR        s               d   datfmt(*eur)
007500141106     d Data_ISO        s               d   datfmt(*iso)
007600141106
007700141106      // - Campi di comodo
007800141106     d Oggi            s              8s 0 inz
007900141106
008000141106      //---------------------------------------------------------------
008100141106      //?Definizione procedure usate.
008200141106      //---------------------------------------------------------------
008300141106
008400141106      //---------------------------------------------------------------
008500141106      //?Definizione Prototipi.
008600141106      //---------------------------------------------------------------
008700141106      /copy gaitrasrc/srcprotopr,TIBS02R
008800141106      /copy gaitrasrc/srcprotopr,TIBS34R
008900141114      /copy gaitrasrc/srcprotopr,TIBS69R
009000141106
009100141106      //---------------------------------------------------------------
009200141106      //?Definizione key-list.
009300141106      //---------------------------------------------------------------
009400141106
009500141106      //---------------------------------------------------------------
009600141106      //?M A I N - L I N E
009700141106      //---------------------------------------------------------------
009800141106
009900141106     c     *Entry        plist
010000141106     c                   parm                    kpjba
010100150112     c                   parm                    TRKC71DS
010200141106
010300141106      /free
010400141106
010500141106       //?Operazioni iniziali
010600141106       exsr RoutInz;
010700141106
010800141106       //?Controllo dati passati
010900141106       exsr Controlla;
011000141106
011100141106       //?Scrive la fase
011200141106       exsr Scrive;
011300141106
011400141106       //?Aggiorna Cliente in Campagna
011500141112       IF  §FCMaggufe = 'S';
011600141112         exsr Aggiorna;
011700141112       ENDIF;
011800141106
011900141106       //?Operazioni finali
012000141106       exsr RoutEnd;
012100141106
012200141106       //--------------------------------------------------------------
012300141106       //?Operazioni iniziali.
012400141106       //--------------------------------------------------------------
012500141106       BEGSR RoutInz;
012600141106
012700141106       //?Imposto oggi
012800141106         Oggi = %dec(%date());
012900141106
013000141106       //?Reperimento dati job
013100141106         exsr DatiJob;
013200141106
013300150112         clear OKC71err;
013400150112         clear OKC71msg;
013500141106
013600141106       ENDSR;
013700141106
013800141106       //--------------------------------------------------------------
013900141106       //?Reperimento Dati del job (Utente/Operativi).
014000141106       //--------------------------------------------------------------
014100141106       BEGSR DatiJob;
014200141106
014300141106         in(E) §AzUte;
014400141106         IF  NOT %error;
014500141106           in(E) §DatiUte;
014600141106         ENDIF;
014700141106         IF  %error or RSut = *blanks;
014800141106           clear TIBS34ds;
014900141106           tibs34r(tibs34ds);
015000141106           in §AzUte;
015100141106           in §DatiUte;
015200141106         ENDIF;
015300141106
015400141106       ENDSR;
015500141106
015600141106       //--------------------------------------------------------------
015700141106       //?Controllo i dati passati.
015800141106       //--------------------------------------------------------------
015900141106       BEGSR Controlla;
016000141106
016100141106       //?Numero campagna obbligatorio
016200150112         IF  IKC71ncm = 0;
016300150112           OKC71err = 'E';
016400150112           OKC71msg = Msg(01);
016500141106           exsr RoutEnd;
016600141106         ENDIF;
016700141114       //?Deve essere una campagna valida
016800150112         chain (IKC71ncm) TICMP01L;
016900150507         IF  not %found(TICMP01L);
017000150112           OKC71err = 'E';
017100150112           OKC71msg = Msg(01);
017200141114           exsr RoutEnd;
017300141114         ENDIF;
017400141106
017500141106       //?Almeno 1 tra KSU/KSC/CPO deve esserci
017600150112         IF  IKC71ksu = 0 and IKC71ksc = 0 and IKC71cpo = 0;
017700150112           OKC71err = 'E';
017800150112           OKC71msg = Msg(02);
017900141106           exsr RoutEnd;
018000141106         ENDIF;
018100141114       //?Deve essere una codice valido
018200150112         IF  IKC71ksu > 0;
018300141114           clear TIBS69DS;
018400150112           I69kac = %dec(IKC71ksu:7:0);
018500141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
018600141114           IF  O69err <> *blanks;
018700150112             OKC71err = 'E';
018800150112             OKC71msg = Msg(02);
018900141114             exsr RoutEnd;
019000141114           ENDIF;
019100141114         ENDIF;
019200150112         IF  IKC71ksc > 0;
019300141114           clear TIBS69DS;
019400150112           I69kac = %dec(IKC71ksc:7:0);
019500141114           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
019600141114           IF  O69err <> *blanks;
019700150112             OKC71err = 'E';
019800150112             OKC71msg = Msg(02);
019900141114             exsr RoutEnd;
020000141114           ENDIF;
020100141114         ENDIF;
020200150112         IF  IKC71cpo > 0;
020300150112           chain (IKC71cpo) TNCPO01L;
020400141114           IF  not %found(TNCPO01L);
020500150112             OKC71err = 'E';
020600150112             OKC71msg = Msg(02);
020700141114             exsr RoutEnd;
020800141114           ENDIF;
020900141114         ENDIF;
021000141106
021100141106       //?Deve essere indicata la fase da creare
021200150112         IF  IKC71acm = *blanks;
021300150112           OKC71err = 'E';
021400150112           OKC71msg = Msg(03);
021500141106           exsr RoutEnd;
021600141106         ENDIF;
021700141106
021800141106       //?Deve essere una fase valida
021900141106         clear TIBS02DS;
022000141106         clear dFCM;
022100141106         T02mod = 'C';
022200141106         T02cod = 'FCM';
022300150112         T02ke1 = IKC71acm;
022400141106         T02sif = KNSIF;
022500141106         TNTBE_RicercaControllo  (kpjba : tibs02ds);
022600141106         IF  T02err <> *blanks;
022700150112           OKC71err = 'E';
022800150112           OKC71msg = Msg(06);
022900141106           exsr RoutEnd;
023000141106         ENDIF;
023100141106       //?Dati Fase
023200141106         dFCM = T02uni;
023300141126
023400141126       //?Se fase che aggiorna la testata NON deve essere inferiore alla fase già presente
023500141126         IF  §FCMaggufe = 'S';
023600150112           chain(n) (IKC71ncm:IKC71ksu:IKC71ksc:IKC71cpo) TICMC01L;
023700150112           IF  %found(TICMC01L) and CMCufe > IKC71acm;
023800150112             OKC71err = 'E';
023900150112             OKC71msg = Msg(08);
024000141126             exsr RoutEnd;
024100141126           ENDIF;
024200141126         ENDIF;
024300141106
024400141112       //?Se fase trattativa deve essere indicato anche il n.trattativa
024500150112         IF  IKC71acm = FaseObjTtr and IKC71nrv = 0;
024600150112           OKC71err = 'E';
024700150112           OKC71msg = Msg(05);
024800141106           exsr RoutEnd;
024900141106         ENDIF;
025000141114
025100141114       //?Se prevista deve essere presente la causale
025200150112         IF  §FCMres = 'S' and IKC71cch = *blanks;
025300150112           OKC71err = 'E';
025400150112           OKC71msg = Msg(06);
025500141114           exsr RoutEnd;
025600141114         ENDIF;
025700141114
025800141114       //?Deve essere una causale valida
025900150112         IF  IKC71cch <> *blanks;
026000141117           clear TIBS02DS;
026100141117           T02mod = 'C';
026200141117           T02cod = 'ECM';
026300150112           T02ke1 = IKC71cch;
026400141117           T02sif = KNSIF;
026500141117           TNTBE_RicercaControllo  (kpjba : tibs02ds);
026600141117           IF  T02err <> *blanks;
026700150112             OKC71err = 'E';
026800150112             OKC71msg = Msg(07);
026900141117             exsr RoutEnd;
027000141117           ENDIF;
027100141114         ENDIF;
027200141106
027300141106       ENDSR;
027400141106
027500141106       //--------------------------------------------------------------
027600141106       //?Scrivo la fase.
027700141106       //--------------------------------------------------------------
027800141106       BEGSR Scrive;
027900141106
028000141106         clear TICMF000;
028100150112         CMFncm = IKC71ncm;
028200150112         CMFksu = IKC71ksu;
028300150112         CMFksc = IKC71ksc;
028400150112         CMFcpo = IKC71cpo;
028500150112         CMFacm = IKC71acm;
028600150112         CMFcch = IKC71cch;
028700150123         if  iKC71dfa > *zero;
028800150123           CMFdfa = iKC71dfa;
028900150123         else;
029000150123           CMFdfa = %dec(%date());
029100150123         endif;
029200150123         if  iKC71hfc > *zero;
029300150123           CMFhfc = iKC71hfc;
029400150123         else;
029500150123           CMFhfc = %dec(%time());
029600150123         endif;
029700141106         CMFfef = DUTpou;
029800141106         CMFpru = knmus;
029900150112         CMFpea = IKC71pea;
030000150112         CMFnrv = IKC71nrv;
030100150122       //?Imposto se fase automatica
030200150122         IF  IKC71aut = 'A';
030300150122           CMFaut = IKC71aut;
030400150122         ENDIF;
030500150122         IF  §FCMefa = 'A' and IKC71aut <> 'A';
030600141106           CMFaut = §FCMefa;
030700141106         ENDIF;
030800150127       //?Imposto flag di NON confrontabile se mi arriva dal chiamante
030900150127         CMFnocf = IKC71nocf;
031000150204       //?Imposto il nuovo KSU
031100150204         IF IKC71nksu > 0;
031200150204           CMFnksu = IKC71nksu;
031300150204         //ELSE;
031400150204         //  CMFnksu = CMFksu;
031500150204         ENDIF;
031600150127       //?Imposto aaaa/mm del confronto fatturazione
031700150127         CMFaacf = IKC71aacf;
031800150127         CMFmmcf = IKC71mmcf;
031900141106
032000141106         write TICMF000;
032100141106
032200141106       ENDSR;
032300141106
032400141106       //--------------------------------------------------------------
032500141106       //?Aggiorno Cliente in Campagna.
032600141106       //--------------------------------------------------------------
032700141106       BEGSR Aggiorna;
032800141106
032900141106         chain (CMFncm:CMFksu:CMFksc:CMFcpo) TICMC01L;
033000150112         IF  %found(TICMC01L);
033100141106           CMCufe = CMFacm;
033200141106           CMCcch = CMFcch;
033300150116           IF  CMCcch <> *blanks;
033400150116             CMCdch = %dec(%date());
033500150116           ELSE;
033600150116             clear CMCdch;
033700150116           ENDIF;
033800141106           update TICMC000;
033900141106         ENDIF;
034000141106
034100141106       ENDSR;
034200141106
034300141106       //--------------------------------------------------------------
034400141106       //?Operazioni finali.
034500141106       //--------------------------------------------------------------
034600141106       BEGSR RoutEnd;
034700141106
034800141106         *inLR = *on;
034900141106         return;
035000141106
035100141106       ENDSR;
035200141106
035300141106      /end-free
035400141106
035500141106       //--------------------------------------------------------------
035600141106       //?Schiere a tempo di compilazione.
035700141106       //--------------------------------------------------------------
035800141106
035900141106** -- MSG -------------------------------------------------------------------*
036000141114Campagna commerciale errata                                                    1
036100141114Cliente/Potenziale errato                                                      2
036200141106Manca la fase                                                                  3
036300141106Manca la percentuale                                                           4
036400141106Manca la trattativa                                                            5
036500141106Fase inesistente                                                               6
036600141114Causale errata                                                                 7
036700141126Si sta aggiornando il cliente con una fase più bassa di quella già memorizzata 8
