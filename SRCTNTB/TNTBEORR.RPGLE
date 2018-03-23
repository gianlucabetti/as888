000100150129       //--------------------------------------------------------------
000200160531       //?GESTIONE TABELLA "EOR" - Orari Servizio Ritiri Export
000300150129       //--------------------------------------------------------------
000400150129     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500150129
000600150129       //--------------------------------------------------------------
000700150129       //?Dichiarazione file.                                          ?
000800150129       //--------------------------------------------------------------
000900150129     fTNTBE01L  uf a e           k disk
001000150129     fTNTBE11L  uf a e           k disk    rename(TNTBE000:TNTBE11)
001100160531
001200160531     fAZORG01L  if   e           k disk
001300160531     fTABEL00F  if   e           k disk
001400150129
001500160531     fTNTBEORD  cf   e             workstn sfile(TBEORS01:S01nrr)
001600150129
001700150129       //--------------------------------------------------------------
001800150129       //?Definizione costanti.                                        ?
001900150129       //--------------------------------------------------------------
002000150129     d Errore          c                   '1'
002100150129     d Eseguito        c                   '0'
002200160531     d Digits          c                   const('0123456789')
002300150129
002400150129       //--------------------------------------------------------------
002500150129       //?Definizione schiere.                                         ?
002600150129       //--------------------------------------------------------------
002700150129       // - Messaggi a video
002800160601     d Msg             s             78    dim(10) ctdata perrcd(1)
002900150129
003000150129       //--------------------------------------------------------------
003100150129       //?Definizione aree dati.                                       ?
003200150129       //--------------------------------------------------------------
003300150129       // - Dati utente
003400150129     d §AzUte        e ds                  extname(AZUTE00F)
003500150129     d                                     dtaara
003600150129     d §DatiUte      e ds                  extname(dDatiUte)
003700150129     d                                     dtaara
003800150129
003900150129       //--------------------------------------------------------------
004000150129       //?Definizione strutture dati.                                  ?
004100150129       //--------------------------------------------------------------
004200150129       // - Status
004300150129     d Psds           sds
004400150129     d   SDSpgm          *proc
004500150129
004600150129       // - Parametri ricevuti
004700150129     d KPJBA         e ds
004800160531     d TNTBEORDS     e ds
004900150129
005000150129       // - Reperimento dati utente
005100150129     d TIBS34ds      e ds
005200150129
005300160531       // - Tabella "EOR"
005400160531     d dEOR          e ds
005500160531
005600160531       // - Tabella "15"
005700160531     d ds15          e ds
005800150129
005900150129       // - Controllo data
006000150129     d wlbdat          ds                  inz
006100150129     d  G02dat                 1      8  0
006200150129     d  G02inv                 9     16  0
006300150129     d  G02err                17     17
006400150129     d  G02tgi                18     22  0
006500150129
006600150129       //--------------------------------------------------------------
006700150129       //?Definizione procedure usate.                                 ?
006800150129       //--------------------------------------------------------------
006900150129       // - Reperimento dati utente
007000150129      /copy gaitrasrc/srcProtoPR,TIBS34R
007100150129
007200150129       // - Controllo data
007300150129     d xsrda8          pr                  extpgm('XSRDA8')
007400150129     d  wlbdat                             likeds(wlbdat)
007500160531
007600160531       // - Ricerca Filiale
007700160531      /copy gaitrasrc/srcprotopr,TNSD24R
007800160531
007900160531       // - Ricerca Nazione
008000160531      /copy gaitrasrc/srcprotopr,trul19r
008100150129
008200150129       //--------------------------------------------------------------
008300150129       //?Definizione variabili globali.                               ?
008400150129       //--------------------------------------------------------------
008500150129       // - Campi di comodo
008600150129     d blanks          s                   like(d1descopz)
008700150129     d comando         s              1
008800150129     d kTBEke1         s                   like(TBEke1)
008900160531     d kTBEke2         s                   like(TBEke2)
009000150129     d len             s              5u 0
009100150129     d s01nrr          s              4  0
009200150129     d savopzione      s                   like(s1opzione)
009300150129     d savrcdnbr       s                   like(c1rcdnbr)
009400150129     d wrkkey          s              1
009500150129     d wrkeofs01       s              1
009600150129     d wrkcars01       s              1
009700150129     d wrkcarw01       s              1
009800150129     d $video          s             10
009900160531     d w0030           s                   like(ORGfil) inz
010000160531
010100160531       // - Ricerca Filiale
010200160531     d pinFIL          s              3
010300160531     d pinFAG          s              1
010400160531     d pinDES          s             25
010500160531     d pinDIT          s              3
010600160531
010700160531      // - Campi x ricerca tabelle TABEL
010800160531     d idTabella       s              2
010900160531     d Ordinamento     s              1
011000160531     d idElemento      s              8
011100160531     d TastoUscita     s              1
011200160531
011300160531      //---------------------------------------------------------------
011400160531      //?Definizione key-list.
011500160531      //---------------------------------------------------------------
011600160531       // - File TABEL00F
011700160531     d k03tabel      e ds                  extname(TABEL00F:*key)
011800160531     d                                     prefix(k_)
011900150129
012000150129       //--------------------------------------------------------------
012100150129       //?Riepilogo indicatori.                                        ?
012200150129       //--------------------------------------------------------------
012300150129       // 01 - ON ricerca -- OFF manutenzione
012400150129       // 02 - proteggo campo fase
012500150202       // 03 - proteggo campi video
012600150129       // 04 - visualizzazione
012700150202       // 20 - gestione subfile
012800150202       // 21 - gestione subfile
012900150202       // 22 - gestione subfile
013000150129       // 28 - messaggio errore
013100160531       // 40 - filiale
013200160531       // 41 - nazione
013300160531       // 42 - ora limite ritiro in giornata
013400160531       // 43 - ora minimo
013500160531       // 44 - ora massimo
013600150129       //--------------------------------------------------------------
013700150129
013800150129       //--------------------------------------------------------------
013900150129
014000150129     c     *Entry        plist
014100150129     c                   parm                    kpjba
014200150129
014300150129      /free
014400150129
014500150129       exsr sr_datijob;
014600150129       exsr sr_parm;
014700150129       exsr sr_c01;
014800150129       exsr sr_uscita;
014900150129
015000150129       // ------------------------------------------------------------------------
015100150129       // Elaborazione parametri ricevuti
015200150129       // ------------------------------------------------------------------------
015300150129       BEGSR sr_parm;
015400150129
015500160531         TNTBEORDS = kpjbu;
015600150129
015700150129         SELECT;
015800150129       // ricerca e scleta
015900160531         WHEN  BEORfun = '1';
016000150129           *in01 = *on;
016100150129       // manutenzione
016200160531         WHEN  BEORfun = *blanks;
016300150129           *in01 = *off;
016400150129       // altrimenti
016500150129         OTHER;
016600160531           BEOREsito = Errore;
016700150129           exsr sr_uscita;
016800150129         ENDSL;
016900150129
017000150129       ENDSR;
017100150129
017200150129       // ------------------------------------------------------------------------
017300150129       // Gestione subfile
017400150129       // ------------------------------------------------------------------------
017500150129       BEGSR sr_c01;
017600150129
017700150129       // imposto variabili
017800150129         wrkcars01 = *on;
017900150129         $video = 'C01';
018000150129
018100150129       // inizio elaborazione subfile
018200150129         DOU  $video <> 'C01';
018300150129
018400150129       // caricamento subfile
018500150129           exsr sr_cars01;
018600150129
018700150129       // c1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
018800150129       // impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
018900150129       // l'ultimo tasto
019000150129           IF  c1csrrrn > 0;
019100150129             c1rcdnbr = c1csrrrn;
019200150129           ELSE;
019300150129             c1rcdnbr = savrcdnbr;
019400150129           ENDIF;
019500150129
019600150129       // se non so quale pagina visualizzare forzo pagina 1
019700150129           IF  c1rcdnbr < 1;
019800150129             c1rcdnbr = 1;
019900150129           ENDIF;
020000150129
020100150129       // salvo il record number del subfile
020200150129           savrcdnbr = c1rcdnbr;
020300150129
020400150129       // Emissione del subfile
020500160531           write TBEORP01;
020600160531           exfmt TBEORC01;
020700150129
020800150129       // controllo tasti funzionali del subfile
020900150129           SELECT;
021000150129
021100150129       // F3=Fine
021200150129           WHEN  *inkc;
021300150129             $video = *blanks;
021400160531             BEORricez = 'C';
021500150129             exsr sr_uscita;
021600150129
021700150129       // F5=Refresh
021800150129           WHEN  *inke;
021900150129             wrkcars01 = *on;
022000150129
022100150129       // F10=Immissione
022200150129           WHEN  *inkj;
022300150129             $video = 'D01';
022400160531             clear TNTBEORDS;
022500160531             BEORcomand = 'J';
022600150129             exsr sr_d01;
022700150129
022800150129       // F13=Ripetizione
022900150129           WHEN  *inkm;
023000150129             exsr sr_ripetiopz;
023100150129
023200150129       // in tutti gli altri casi
023300150129           OTHER;
023400150129       // controllo ed elaborazione scelte su subfile
023500150129             exsr sr_gestsfl;
023600150129           ENDSL;
023700150129
023800150129       // fine elaborazione 'C01'
023900150129         ENDDO;
024000150129
024100150129       ENDSR;
024200150129
024300150129       // ------------------------------------------------------------------------
024400150129       // Caricamento subfile
024500150129       // ------------------------------------------------------------------------
024600150129       BEGSR sr_cars01;
024700150129
024800150129       // se è stato richiesto il caricamento del subfile
024900150129         IF  wrkcars01 = *on;
025000150129
025100150129       // inizializzo il subfile
025200150129           s01nrr = 0;
025300150129           *in20 = *on;
025400160531           write TBEORC01;
025500150129           *in20 = *off;
025600150129           *in21 = *off;
025700150129           *in22 = *off;
025800150129           *in23 = *off;
025900150129
026000150129       // imposto la chiave di posizionamento e lettura file
026100160531           TBEcod = 'EOR';
026200150129           TBEke1 = c1setll;
026300150129
026400150129       // se è stato scelto il posizionamento
026500150129           IF  c1setll <> *blanks;
026600150129             wrkkey = '2';
026700150129       // altrimenti
026800150129           ELSE;
026900150129             wrkkey = '1';
027000150129           ENDIF;
027100150129
027200150129       // posizionamento file
027300150129           exsr sr_setlltbe01;
027400150129
027500150129       // fino a che non è fine file
027600150129           DOU  %eof(TNTBE01L);
027700150129
027800150129         // leggo file
027900150129             reade(n) TBEcod TNTBE01L;
028000150129
028100150129         // fine file esco
028200150129             IF  %eof(TNTBE01L);
028300150129               leave;
028400150129             ENDIF;
028500150129
028600150129         // se in "ricerca/scelta" non carico i records annullati
028700150129             IF  (*in01 and TBEatb = *blanks) or not *in01;
028800150129         // scrivo subfile
028900150129               clear s1opzione;
029000150129               exsr sr_wtrs01;
029100150129             ENDIF;
029200150129
029300150129           ENDDO;
029400150129
029500150129       // fine caricamento subfile
029600150129         ENDIF;
029700150129
029800150129       // se scritto almeno un record
029900150129         IF  s01nrr > 0;
030000150129       // indicatore di sflend
030100150129           *in21 = *on;
030200150129         ENDIF;
030300150129
030400150129       // se non scritto neanche un record
030500150129         IF  s01nrr = 0;
030600150129       // indicatore di sfldsp
030700150129           *in23 = *on;
030800150129         ENDIF;
030900150129
031000150129       // disattivo opzione di caricamento subfile
031100150129         wrkcars01 = *off;
031200150129
031300150129       ENDSR;
031400150129
031500150129       // ------------------------------------------------------------------------
031600150129       // Gestione videata
031700150129       // ------------------------------------------------------------------------
031800150129       BEGSR sr_d01;
031900150129
032000150129       // imposto il video a seconda della funzione richiesta
032100150129         exsr sr_setvideo;
032200150129
032300150129       // imposto variabile
032400150129         $video = 'D01';
032500150129
032600150129       // fino a che la variabile resta settata come 'D01'
032700150129         DOU $video <> 'D01';
032800150129
032900150129           *in02 = *off;
033000150129           *in03 = *off;
033100150129           *in04 = *off;
033200150129
033300150129       // se immessa opzione di 'scelta'
033400160531           IF  BEORopzio = '01';
033500160531             BEORke1 = s1TBEke1;
033600160531             BEORke2 = s1TBEke2;
033700160531             BEORuni = s1TBEuni;
033800150129             exsr sr_uscita;
033900150129
034000150129         // se immessa un'altra opzione
034100150129           ELSE;
034200150129         // se non è immissione/copia proteggo il campo della causale
034300160531             IF  BEORcomand <> 'J' and BEORopzio <> '03';
034400150129               *in02 = *on;
034500150129             ENDIF;
034600150129          // se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
034700150129          // proteggo i campi del video
034800160531             IF  BEORopzio = '04' or BEORopzio = '05';
034900150129               *in03 = *on;
035000150129             ENDIF;
035100150129          // se immessa opzione di 'visualizzazione'
035200150129          // non attivo F6
035300160531             IF  BEORopzio = '05';
035400150129               *in04 = *on;
035500150129             ENDIF;
035600150129          // emetto il video
035700160531             exfmt TBEORD01;
035800150129           ENDIF;
035900150129
036000150129         // reset indicatore generico di errore
036100150129           *in28 = *off;
036200150129
036300150129         // pulisco il campo messaggi
036400150129           clear vd1msg;
036500150129
036600150129         // controllo tasti funzionali del video
036700150129           SELECT;
036800150129
036900150129         // F3=Fine
037000150129           WHEN  *inkc;
037100160531             BEORricez = 'C';
037200150129             $video = *blanks;
037300150129             unlock TNTBE01L;
037400150129             exsr sr_uscita;
037500150129
037600150129         // F6=Conferma
037700150129           WHEN  *inkf;
037800160531             BEORricez = 'F';
037900150129         // controllo e decodifico i dati del video
038000150129             exsr sr_ctrd01;
038100150129         // conferma per annullo/ripristino
038200160531             IF  BEORopzio = '04';
038300150129               SELECT;
038400150129           // annullamento
038500150129               WHEN  TBEatb = *blanks;
038600150129                 TBEatb = 'A';
038700150129           // ripristino
038800150129               WHEN  TBEatb = 'A';
038900150129                 clear TBEatb;
039000150129               ENDSL;
039100150129             ENDIF;
039200150129
039300150129         // se non riscontrati errori emetto la finestra con i dati per la trasmissione
039400150129             IF  not *in28;
039500150129               wrkcarw01 = *on;
039600150129               $video = 'W01';
039700150129               exsr sr_w01;
039800150129             ENDIF;
039900150129
040000150129         // F8=Record successivo
040100150129           WHEN  *inkh;
040200150129             clear s1opzione;
040300150129             wrkcars01 = *off;
040400150129             $video = 'C01';
040500160531             BEORricez = 'H';
040600150129
040700150129         // F12=Ritorno
040800150129           WHEN  *inkl;
040900150129             clear s1opzione;
041000160531             BEORricez = 'L';
041100150129             unlock TNTBE01L;
041200150129         // se non è f12 da immissione/copia non ricarico il subfile
041300160531             IF  BEORcomand = 'J' or BEORopzio = '03';
041400150129               wrkcars01 = *on;
041500150129             ELSE;
041600150129               wrkcars01 = *off;
041700150129             ENDIF;
041800150129             $video = 'C01';
041900150129
042000150129         // Invio
042100150129           OTHER;
042200150129             IF  not *in03;
042300150129               exsr sr_ctrd01;
042400150129             ENDIF;
042500150129
042600150129           ENDSL;
042700150129
042800150129       // fine gestione 'D01'
042900150129         ENDDO;
043000150129
043100150129       ENDSR;
043200150129
043300150129       // ------------------------------------------------------------------------
043400150129       // Ripeto opzione in tutte le righe del subfile
043500150129       // ------------------------------------------------------------------------
043600150129       BEGSR sr_ripetiopz;
043700150129
043800150129         IF  c1csrrrn > 0;
043900150129           s01nrr = c1csrrrn;
044000160531           chain s01nrr TBEORS01;
044100150129           IF  %found and s1opzione > 0;
044200150129             savopzione = s1opzione;
044300150129             *in22 = *on;
044400160531             update TBEORS01;
044500150129
044600150129             wrkeofs01 = *off;
044700150129             DOU  wrkeofs01 = *on;
044800150129               s01nrr = s01nrr + 1;
044900160531               chain s01nrr TBEORS01;
045000150129               IF  %found;
045100150129                 s1opzione = savopzione;
045200160531                 update TBEORS01;
045300150129               ELSE;
045400150129                 wrkeofs01 = *on;
045500150129               ENDIF;
045600150129             ENDDO;
045700150129
045800150129             *in22 = *off;
045900150129
046000150129           ENDIF;
046100150129
046200150129         ENDIF;
046300150129
046400150129       ENDSR;
046500150129
046600150129       // ------------------------------------------------------------------------
046700150129       // Imposto i dati a video
046800150129       // ------------------------------------------------------------------------
046900150129       BEGSR sr_setvideo;
047000150129
047100150129       // pulisco il formato video D01
047200150129         exsr sr_puld01;
047300150129
047400150129       // controllo se 'immissione' 'modifica' 'copoa' o altro
047500150129         SELECT;
047600150129
047700150129       // F10=Immissione
047800160531         WHEN  BEORcomand = 'J';
047900150129           d1descopz = 'Immissione';
048000150129
048100150129       // Opzione "02"=modifica
048200160531         WHEN  BEORopzio = '02';
048300150129           d1descopz = 'Modifica';
048400150129           exsr sr_imposta;
048500150129
048600150129       // Opzione "03"=copia
048700160531         WHEN  BEORopzio = '03';
048800150129           d1descopz = 'Copia';
048900150129           exsr sr_imposta;
049000150129
049100150129       // Opzione "04"=cancellazione/ripristino
049200160531         WHEN  BEORopzio = '04';
049300150129           exsr sr_imposta;
049400150129        // se richiesta 'cancellazione'
049500150129           IF  TBEatb = *blanks;
049600150129             d1descopz = 'Annullamento';
049700150129           ENDIF;
049800150129        // se richiesto 'ripristino'
049900150129           IF TBEatb = 'A';
050000150129             d1descopz = 'Ripristino';
050100150129           ENDIF;
050200150129
050300150129       // Opzione "05"=visualizzazione
050400160531         WHEN  BEOROpzio ='05';
050500150129           d1descopz = 'Visualizzazione';
050600150129           exsr sr_imposta;
050700150129
050800150129       // Fine scelta
050900150129         ENDSL;
051000150129
051100150129       // centro la descrizione della funzione nel formato video
051200150129         len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
051300150129         d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
051400150129
051500150129       ENDSR;
051600150129
051700150129       // ------------------------------------------------------------------------
051800150129       // controllo video
051900150129       // ------------------------------------------------------------------------
052000150129       BEGSR sr_ctrd01;
052100150129
052200150129         *in28 = *off;
052300150129         *in40 = *off;
052400150129         *in41 = *off;
052500160531         *in42 = *off;
052600160531         *in43 = *off;
052700160531         *in44 = *off;
052800150129
052900160531       // --> Filiale Ritiro
053000160531       // Ricerca
053100160531         IF  %scan('?':d1tbeke1) > 0;
053200160531           clear pinFIL;
053300160531           clear pinFAG;
053400160531           clear pinDES;
053500160531           clear pinDIT;
053600160531           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
053700160531           IF  pinFIL > *zeros;
053800160531             D1tbeke1  = pinFIL;
053900160531             Destbeke1 = pinDES;
054000160531           ENDIF;
054100160531           *in40 = *on;
054200160531           leavesr;
054300160531         ENDIF;
054400160531
054500160531       // No a blank
054600160531         IF  d1tbeke1 = *blanks;
054700160531           vd1msg = Msg(01);
054800160531           *in28 = *on;
054900160531           *in40 = *on;
055000160531           leavesr;
055100160531         ENDIF;
055200160531
055300160531       // Solo numeri
055400160531         IF  %check(digits:d1tbeke1) > 0;
055500160531           vd1msg = Msg(01);
055600160531           *in28 = *on;
055700160531           *in40 = *on;
055800160531           leavesr;
055900160531         ENDIF;
056000160531
056100160531       // Valida
056200160531         w0030 = %dec(d1tbeke1:3:0);
056300160531         chain (w0030) AZORG01L;
056400160531         IF  not %found(AZORG01L) or ORGfva <> *blanks or
056500160531            (ORGfag <> 'F' and ORGfag <> 'A');
056600160531           vd1msg = Msg(01);
056700160531           *in28 = *on;
056800160531           *in40 = *on;
056900160531           leavesr;
057000160531         ENDIF;
057100160531         destbeke1 = ORGdes;
057200160601
057300160601       // Deve essere una filiale estera
057400160601         IF  ORGfl1 <> 'E';
057500160601           vd1msg = Msg(06);
057600160601           *in28 = *on;
057700160601           *in40 = *on;
057800160601           leavesr;
057900160601         ENDIF;
058000160531
058100160531       // --> Nazione
058200160531       // Ricerca
058300160531         clear ds15;
058400160531         IF  %scan('?':d1tbeke2) > 0;
058500160531           idTabella = '15';
058600160531           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
058700160531                            tastoUscita);
058800160601           d1tbeke2 = idElemento;
058900160531           *in41 = *on;
059000160531           leavesr;
059100160531         ENDIF;
059200160531
059300160531       // Valida
059400160531         IF  d1tbeke2 <> *blanks;
059500160531           k_TBLkut = 1;
059600160531           k_TBLcod = '15';
059700160531           k_TBLkey = d1tbeke2;
059800160531           chain %kds(K03tabel) TABEL00F;
059900160531           IF  not %found(TABEL00F) or TBLflg <> *blanks;
060000160601           *in28 = *on;
060100160531             vd1msg = Msg(02);
060200160531             *in41 = *on;
060300160531             leavesr;
060400160531           ENDIF;
060500160531           ds15 = TBLuni;
060600160531           Destbeke2 = §15des;
060700160601         ENDIF;
060800160601
060900150129
061000150129       // se immissione controllo che non esista già
061100160531         IF  BEORcomand = 'J' or BEORopzio = '03';
061200160531           TBEcod = 'EOR';
061300150129           TBEke1 = d1tbeke1;
061400150129           TBEke2 = d1tbeke2;
061500150129           clear TBElin;
061600150129           chain(n) (TBEcod:TBEke1:TBEke2:TBElin) TNTBE01L;
061700150129           IF  %found(TNTBE01L);
061800150129             vd1msg = Msg(03);
061900150129             *in28 = *on;
062000160531             *in41 = *on;
062100150129             leavesr;
062200150129           ENDIF;
062300150129         ENDIF;
062400160531
062500160531       // --> Ora limite ritiro in giornata
062600160531       // No a 0
062700160531         IF  d1EORlrg = 0;
062800160531           vd1msg = Msg(04);
062900160531           *in28 = *on;
063000160531           *in42 = *on;
063100160531           leavesr;
063200160531         ENDIF;
063300160531
063400160531       // --> Ora minimo
063500160531       // No a 0
063600160531         IF  d1EORmin = 0;
063700160531           vd1msg = Msg(04);
063800160531           *in28 = *on;
063900160531           *in43 = *on;
064000160531           leavesr;
064100160531         ENDIF;
064200160531
064300160531       // --> Ora massimo
064400160531       // No a 0
064500160531         IF  d1EORmax = 0;
064600160531           vd1msg = Msg(04);
064700160531           *in28 = *on;
064800160531           *in44 = *on;
064900160531           leavesr;
065000160531         ENDIF;
065100160531
065200160531       // Ora minima no superiore a ora massimo
065300160531         IF  d1EORmin > d1EORmax;
065400160531           vd1msg = Msg(05);
065500160531           *in28 = *on;
065600160531           *in43 = *on;
065700160531           leavesr;
065800160531         ENDIF;
065900150129
066000150129       ENDSR;
066100150129
066200150129       // ------------------------------------------------------------------------
066300150129       // Gestione video dati di trasmissione
066400150129       // ------------------------------------------------------------------------
066500150129       BEGSR sr_w01;
066600150129
066700150129       // imposto i dati a video
066800150129         exsr sr_carw01;
066900150129
067000150129       // fino a che la variabile resta settata come 'W01'
067100150129         DOU  $video <> 'W01';
067200150129
067300150129       // reset indicatore generico di errore
067400150129           *in28 = *off;
067500150129
067600150129       // emetto il video
067700160531           exfmt TBEORW01;
067800150129
067900150129       // controllo tasti funzionali del video
068000150129           SELECT;
068100150129
068200150129       // F6=Conferma
068300150129           WHEN  *inkf;
068400150129         // controllo i dati del video
068500150129             exsr sr_ctrw01;
068600150129         // se non riscontrati errori aggiorno il record corrente
068700150129             IF  not *in28;
068800150129               exsr sr_aggiorna;
068900150202               IF  not *in28;
069000160531                 IF  BEORcomand = 'J';
069100150202                   $video = 'D01';
069200150202                   exsr sr_setvideo;
069300150202                 ELSE;
069400150202                   $video = 'C01';
069500150202                 ENDIF;
069600150202               ENDIF;
069700150129             ENDIF;
069800150129
069900150129       // F12=Ritorno
070000150129           WHEN  *inkl;
070100150129             $video = 'D01';
070200160531             clear BEORricez;
070300150129
070400150129       // Invio
070500150129           OTHER;
070600150129             exsr sr_ctrw01;
070700150129
070800150129           ENDSL;
070900150129
071000150129       // fine gestione 'W01'
071100150129         ENDDO;
071200150129
071300150129       ENDSR;
071400150129
071500150129       // ------------------------------------------------------------------------
071600150129       // Imposto i dati di trasmissione
071700150129       // ------------------------------------------------------------------------
071800150129       BEGSR sr_carw01;
071900150129
072000150129       // se pulisco i campi
072100150129         clear w1ftt;
072200150129         clear w1flt;
072300150129         clear w1ftr;
072400150129         clear w1dtr;
072500150129
072600150129       // se non immissione imposto i campi del file
072700160531         IF  BEORcomand <> 'J';
072800150129           w1ftt = TBEftt;
072900150129           w1flt = TBEflt;
073000150129           w1ftr = TBEftr;
073100150129       // imposto la data
073200150129           IF  TBEdtr <> *zeros;
073300150129             clear wlbdat;
073400150129             G02inv = TBEdtr;
073500150129             G02err = '3';
073600150129             xsrda8(wlbdat);
073700150129             w1dtr = G02dat;
073800150129           ENDIF;
073900150129
074000150129         ENDIF;
074100150129
074200150129       ENDSR;
074300150129
074400150129       // ------------------------------------------------------------------------
074500150129       // Controllo i dati di trasmissione
074600150129       // ------------------------------------------------------------------------
074700150129       BEGSR sr_ctrw01;
074800150129
074900150129       ENDSR;
075000150129
075100150129       // ------------------------------------------------------------------------
075200150129       // Aggiorno tabella
075300150129       // ------------------------------------------------------------------------
075400150129       BEGSR sr_aggiorna;
075500150129
075600150129       // imposto campi del file
075700150129         clear TBElin;
075800150129         TBEke1 = d1tbeke1;
075900150129         TBEke2 = d1tbeke2;
076000150129
076100160531         §EORlrg = d1EORlrg;
076200160531         §EORmin = d1EORmin;
076300160531         §EORmax = d1EORmax;
076400150129
076500160531         TBEuni = dEOR;
076600150129
076700150129         TBEftt = w1ftt;
076800150129         TBEflt = w1flt;
076900150129         clear TBEftr;
077000150129         clear TBEdtr;
077100150129
077200150129       // controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
077300150129         SELECT;
077400150129
077500150129       // F10=immissione - Opzione "03"=copia
077600160531         WHEN  BEORcomand = 'J' or BEORopzio = '03';
077700150129       // scrivo nuovo record con gestione errore per chiave duplicata
077800150129           write TNTBE000;
077900150129
078000150129       // Opzione "02"=modifica
078100160531         WHEN  BEORopzio = '02';
078200150129       // update record e controllo errore per chiave duplicata
078300150129           update TNTBE000;
078400150129
078500150129       // Opzione "04"=cancellazione/ripristino
078600160531         WHEN  BEORopzio = '04';
078700150129           update TNTBE000;
078800150129
078900150129         ENDSL;
079000150129
079100150129       ENDSR;
079200150129
079300150129       // ------------------------------------------------------------------------
079400150129       // pulizia video
079500150129       // ------------------------------------------------------------------------
079600150129       BEGSR sr_puld01;
079700150129
079800150129         clear d1descopz;
079900150129         clear d1tbeke1;
080000150129         clear d1tbeke2;
080100160531         clear d1EORlrg;
080200160531         clear d1EORmin;
080300160531         clear d1EORmax;
080400160531         clear destbeke1;
080500160531         clear destbeke2;
080600150129
080700150129         *in28 = *off;
080800150129         *in40 = *off;
080900150129         *in41 = *off;
081000160531         *in42 = *off;
081100160531         *in43 = *off;
081200160531         *in44 = *off;
081300150129
081400150129       ENDSR;
081500150129
081600150129       // ------------------------------------------------------------------------
081700150129       // Imposto i dati a video
081800150129       // ------------------------------------------------------------------------
081900150129       BEGSR sr_imposta;
082000150129
082100150129       // recupero i dati dal file
082200150129         kTBEke1 = s1tbeke1;
082300160531         kTBEke2 = s1tbeke2;
082400150129         clear TBElin;
082500160531         chain (TBEcod:kTBEke1:kTBEke2:TBElin) TNTBE01L;
082600150129
082700150129       // imposto i campi a video
082800150129         d1tbeke2 = TBEke2;
082900150129         d1tbeke1 = TBEke1;
083000160531         dEOR = TBEuni;
083100160531         d1EORlrg = §EORlrg;
083200160531         d1EORmin = §EORmin;
083300160531         d1EORmax = §EORmax;
083400160601
083500160601       // Decodifico
083600160601         w0030 = %dec(d1tbeke1:3:0);
083700160601         chain (w0030) AZORG01L;
083800160601         IF  %found(AZORG01L);
083900160601           destbeke1 = ORGdes;
084000160601         ENDIF;
084100160601
084200160601         IF  d1tbeke2 <> *blanks;
084300160601           k_TBLkut = 1;
084400160601           k_TBLcod = '15';
084500160601           k_TBLkey = d1tbeke2;
084600160601           chain %kds(K03tabel) TABEL00F;
084700160601           IF  %found(TABEL00F);
084800160601             ds15 = TBLuni;
084900160601           ENDIF;
085000160601           Destbeke2 = §15des;
085100160601         ENDIF;
085200150129
085300150129       ENDSR;
085400150129
085500150129       // ------------------------------------------------------------------------
085600150129       // Gestione del subfile
085700150129       // ------------------------------------------------------------------------
085800150129       BEGSR sr_gestsfl;
085900150129
086000150129       // set flag
086100150129         wrkeofs01 = *off;
086200150129
086300150129       // inizio lettura subfile
086400150129         DOW  wrkeofs01 = *off and *in21;
086500160531           readc TBEORS01;
086600150129       // se fine subfile
086700150129           IF  %eof;
086800150129             wrkcars01 = *on;
086900150129             leave;
087000150129           ENDIF;
087100150129       // se è stata immessa un'opzione
087200150129           IF  s1opzione <> *zeros;
087300150129         // reset ds di servizio
087400160531             clear TNTBEORDS;
087500150129         // controllo ed elaborazione opzione immessa
087600150129             SELECT;
087700150129         // opzione "01"=scelta
087800150129             WHEN  s1opzione = 1 and *in01;
087900160531               BEORopzio = '01';
088000150129         // opzione "02"=modifica
088100150129             WHEN  s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
088200160531               BEORopzio = '02';
088300150129         // opzione "03"=copia
088400150129             WHEN  s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
088500160531               BEORopzio = '03';
088600150129         // opzione "04"=annullo/ripristino
088700150129             WHEN s1opzione = 4 and not *in01;
088800160531               BEORopzio = '04';
088900150129         // opzione "05"=visualizzazione
089000150129             WHEN s1opzione = 5;
089100160531               BEORopzio = '05';
089200150129             ENDSL;
089300150129
089400150129         // se immessa almeno un opzione valida
089500160531             IF  BEORopzio <> *blanks;
089600150129         // gstione del formato video
089700150129               exsr sr_d01;
089800150129         // se la gestione si è chiusa con ...
089900150129               SELECT;
090000150129           // F6=Conferma
090100160531               WHEN  BEORricez = 'F';
090200150129                 clear s1opzione;
090300150129                 wrkcars01 = *on;
090400150129          // F12=Ritorno
090500160531               WHEN  BEORricez = 'L';
090600150129                 clear s1opzione;
090700150129                 wrkeofs01 = *on;
090800150129          // altrimenti
090900150129               OTHER;
091000150129                 *in22 = *on;
091100150129               ENDSL;
091200150129
091300160531               update TBEORS01;
091400150129               *in22 = *off;
091500150129         // fine opzione
091600150129             ENDIF;
091700150129       // fine opzione
091800150129           ENDIF;
091900150129
092000150129         ENDDO;
092100150129
092200150129       ENDSR;
092300150129
092400150129       // ------------------------------------------------------------------------
092500150129       // Posizionamento sul file
092600150129       // ------------------------------------------------------------------------
092700150129       BEGSR sr_setlltbe01;
092800150129
092900150129         SELECT;
093000150129       // chiave totale
093100150129         WHEN  wrkkey = '1';
093200150129           setll TBEcod TNTBE01L;
093300150129       // chiave parziale
093400150129         WHEN  wrkkey = '2';
093500150129           setll (TBEcod:TBEke1) TNTBE01L;
093600150129         ENDSL;
093700150129
093800150129       ENDSR;
093900150129
094000150129       // ------------------------------------------------------------------------
094100150129       // Scrivo subfile
094200150129       // ------------------------------------------------------------------------
094300150129       BEGSR sr_wtrs01;
094400150129
094500150129       // se non raggiunto limite massimo di caricamento
094600150129         IF  s01nrr < 9999;
094700150129       // imposto campi di subfile
094800150129           exsr sr_sets01;
094900150129       // imposto numeratore righe e numero relativo di record
095000150129           s01nrr = s01nrr + 1;
095100150129       // scrivo subfile
095200160531           write TBEORS01;
095300150129         ENDIF;
095400150129
095500150129       ENDSR;
095600150129
095700150129       // ------------------------------------------------------------------------
095800150129       // Imposto campi del subfile
095900150129       // ------------------------------------------------------------------------
096000150129       BEGSR sr_sets01;
096100150129
096200150129         s1tbeke1 = TBEke1;
096300150129         s1tbeke2 = TBEke2;
096400160531         dEOR = TBEuni;
096500160531         s1EORlrg = §EORlrg;
096600160531         s1EORmin = §EORmin;
096700160531         s1EORmax = §EORmax;
096800150129         s1tbeatb = TBEatb;
096900150129
097000150129       ENDSR;
097100150129
097200150129       // ------------------------------------------------------------------------
097300150129       // Routine iniziale
097400150129       // ------------------------------------------------------------------------
097500150129         BEGSR sr_datijob;
097600150129
097700150129         in(E) §AzUte;
097800150129         IF  NOT %error;
097900150129           in(E) §DatiUte;
098000150129         ENDIF;
098100150129         IF  %error or RSut = *blanks;
098200150129           clear TIBS34ds;
098300150129           tibs34r(tibs34ds);
098400150129           in §AzUte;
098500150129           in §DatiUte;
098600150129         ENDIF;
098700150129
098800150129         ENDSR;
098900150129
099000150129       // ------------------------------------------------------------------------
099100150129       // Uscita dal programma
099200150129       // ------------------------------------------------------------------------
099300150129         BEGSR sr_uscita;
099400150129
099500160531           IF  BEOResito = *blanks;
099600160531             BEOResito = Eseguito;
099700150129           ENDIF;
099800150129
099900160531           kpjbu = TNTBEORDS;
100000150129
100100150129           *inlr = *on;
100200150129           return;
100300150129
100400150129         ENDSR;
100500150129
100600150129      /end-free
100700150129
100800150129** -MSG-                                                                     *
100900160531Filiale errata                                                                 01
101000160531Nazione errata                                                                 02
101100160531Key Tabella già esistente                                                      03
101200160531Ora errata                                                                     04
101300160531Orari incongruenti                                                             05
101400160601Ammesse solo filiali estere                                                    06
