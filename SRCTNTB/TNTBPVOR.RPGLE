000100140128       //==============================================================
000200140128       //
000300140128       //?TNTBPVOR - Gestione tabella PVO
000400140128       //
000500140128       //==============================================================
000600140128
000700140128       //--------------------------------------------------------------
000800140128       //?Specifiche di controllo.                                     ?
000900140128       //--------------------------------------------------------------
001000060502     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001100140128
001200140128       //--------------------------------------------------------------
001300140128       //?Dichiarazione file.                                          ?
001400140128       //--------------------------------------------------------------
001500140128
001600140128       // -?File tabelle
001700140128     fTNTBE01L  uf a e           k disk
001800140129
001900140129       // -?File Anagrafico clienti
002000140129     fCNACO00F  if   e           k disk
002100140129     fFNACR01L  if   e           k disk
002200140128
002300140128       // -?File video
002400140128     fTNTBPVOD  cf   e             workstn sfile(TBPVOS01:s01nrr)
002500060428
002600140128       //--------------------------------------------------------------
002700140128       //?Indicatori.
002800140128       //--------------------------------------------------------------
002900140128       // 01 - ON ricerca -- OFF manutenzione
003000140128       // 02 - proteggo campo fase
003100140128       // 03 - proteggo campi video 01
003200140128       // 04 - visualizzazione
003300140128       // 05 - visualizzo subfile lingue
003400140128       // 20 - gestione subfile 01
003500140128       // 21 - gestione subfile 01
003600140128       // 22 - gestione subfile 01
003700140128       // 23 - gestione subfile 01
003800140128       // 24 - gestione subfile 02
003900140128       // 25 - gestione subfile 02
004000140128       // 28 - messaggio errore
004100140128       // 40 - Key 1
004200140128       // 41 - Key 2
004300140128       // 42 - Almeno 1 dato
004400060502
004500140128       //--------------------------------------------------------------
004600140128       //?Schiere.
004700140128       //--------------------------------------------------------------
004800060502     d msg             s             78    dim(17) ctdata perrcd(1)
004900060427
005000140128       //--------------------------------------------------------------
005100140128       //?Campi di work.
005200140128       //--------------------------------------------------------------
005300060428     d blanks          s                   like(d1descopz)
005400060502     d comando         s              1
005500060505     d ktbeke1         s                   like(tbeke1)
005600140129     d kacokut         s                   like(ACOkut) inz(1)
005700140129     d kacoksc         s                   like(ACOksc)
005800140129     d kacrcro         s                   like(ACRcro)
005900060428     d len             s              5u 0
006000060505     d s01nrr          s              4  0
006100060428     d savopzione      s                   like(s1opzione)
006200060428     d savrcdnbr       s                   like(c1rcdnbr)
006300060428     d wrkkey          s              1
006400060428     d wrkeofs01       s              1
006500060428     d wrkcars01       s              1
006600060505     d wrkcarw01       s              1
006700140128     d wvideo          s             10
006800060428
006900140128       //--------------------------------------------------------------
007000140128       //?DS interne/esterne.
007100140128       //--------------------------------------------------------------
007200060504     d wlbdat          ds                  inz
007300060504     d  g02dat                 1      8  0
007400060504     d  g02inv                 9     16  0
007500060504     d  g02err                17     17
007600060504     d  g02tgi                18     22  0
007700060428
007800140128     d dPVO          e ds
007900060502     d kpjba         e ds
008000060503     d tibs34ds      e ds                  inz
008100140128     d tntbPVODS     e ds
008200060428     d §azute        e ds                  extname(azute00f)
008300060428     d                                     dtaara
008400060428     d §datiute      e ds                  extname(ddatiute)
008500060428     d                                     dtaara
008600010503
008700060428     d psds           sds
008800060428     d  pgmname          *proc
008900060428
009000140128       //--------------------------------------------------------------
009100140128       //?PGM richiamati.
009200140128       //--------------------------------------------------------------
009300060428     d tibs34r         pr                  extpgm('TIBS34R')
009400060502     d  tibs34ds                           likeds(tibs34ds)
009500060504
009600060504     d xsrda8          pr                  extpgm('XSRDA8')
009700060504     d  wlbdat                             likeds(wlbdat)
009800060428
009900140128       //--------------------------------------------------------------
010000140128       //?Costanti.
010100140128       //--------------------------------------------------------------
010200060428     d errore          c                   '1'
010300060428     d eseguito        c                   '0'
010400010503
010500140128       //--------------------------------------------------------------
010600010423
010700060428      /free
010800060428
010900060428       exsr sr_parm;
011000060428       exsr sr_c01;
011100060428       exsr sr_uscita;
011200010423
011300060502       // ------------------------------------------------------------------------
011400140128       //?Elaborazione parametri ricevuti.
011500060502       // ------------------------------------------------------------------------
011600140128       BEGSR  sr_parm;
011700060428
011800140128         TNTBPVODS = kpjbu;
011900060428
012000140128         SELECT;
012100140128
012200140128       //?Ricerca e scelta
012300140128          WHEN  BPVOfun = '1';
012400140128            *in01 = *on;
012500140128       //?Manutenzione
012600140128          WHEN  BPVOfun = *blanks;
012700140128           *in01 = *off;
012800140128       //?Altrimenti
012900140128          OTHER;
013000140128           BPVOesito = errore;
013100060428           exsr sr_uscita;
013200140128
013300140128         ENDSL;
013400060428
013500140128       ENDSR;
013600010430
013700060502       // ------------------------------------------------------------------------
013800140128       //?Gestione subfile.
013900060502       // ------------------------------------------------------------------------
014000140128       BEGSR  sr_c01;
014100060428
014200140128       //?Imposto variabili
014300140128         wrkcars01 = *on;
014400140128         wvideo = 'C01';
014500060428
014600140128       //?Inizio elaborazione subfile
014700140129         DOU  wvideo <> 'C01';
014800060428
014900140128         //?Caricamento subfile
015000140128           exsr sr_cars01;
015100060428
015200140128         //?C1csrrrn contiene il numero di riga del subfile su cui era posizionato il cursore
015300140128         //?impostando c1rcdnbr visualizzo la pagina che vedeva l'utente quando ha premuto
015400140128         //?l'ultimo tasto
015500140128           IF  c1csrrrn > 0;
015600140128             c1rcdnbr = c1csrrrn;
015700140128           ELSE;
015800140128             c1rcdnbr = savrcdnbr;
015900140128           ENDIF;
016000060428
016100140128         //?Se non so quale pagina visualizzare forzo pagina 1
016200140128           IF c1rcdnbr < 1;
016300140128             c1rcdnbr = 1;
016400140128           ENDIF;
016500060428
016600140128         //?Salvo il record number del subfile
016700140128           savrcdnbr = c1rcdnbr;
016800060428
016900140128         //?Emissione del subfile
017000140128           write TBPVOp01;
017100140128           exfmt TBPVOc01;
017200060428
017300140128         //?Controllo tasti funzionali del subfile
017400140128           SELECT;
017500060428
017600140128           //?F3=Fine
017700140128             WHEN  *inkc;
017800140128               wvideo = *blanks;
017900140128               BPVOricez = 'C';
018000140128               exsr sr_uscita;
018100140128           //?F5=Refresh
018200140128             WHEN  *inke;
018300140128               wrkcars01 = *on;
018400060428
018500140128           //?F10=Immissione
018600140128             WHEN  *inkj;
018700140128               wvideo = 'D01';
018800140128               clear TNTBPVODS;
018900140128               BPVOcomand = 'J';
019000140128               exsr sr_d01;
019100060428
019200140128           //?F13=Ripetizione
019300140128             WHEN  *inkm;
019400140128               exsr sr_ripetiopz;
019500060428
019600140128           //?In tutti gli altri casi
019700140128             OTHER;
019800140128           //?Controllo ed elaborazione scelte su subfile
019900140128               exsr sr_gestsfl;
020000140128
020100140128           ENDSL;
020200060428
020300140128       //?Fine elaborazione 'C01'
020400140128         ENDDO;
020500060428
020600140128       ENDSR;
020700060428
020800060502       // ------------------------------------------------------------------------
020900140128       //?Caricamento subfile.
021000060502       // ------------------------------------------------------------------------
021100140128       BEGSR  sr_cars01;
021200010430
021300140128       //?Se è stato richiesto il caricamento del subfile
021400140128         IF  wrkcars01 = *on;
021500060428
021600140128         //?Inizializzo il subfile
021700140128           s01nrr = 0;
021800140128           *in20 = *on;
021900140128           write TBPVOc01;
022000140128           *in20 = *off;
022100140128           *in21 = *off;
022200140128           *in22 = *off;
022300140128           *in23 = *off;
022400060428
022500140128         //?Imposto la chiave di posizionamento e lettura file
022600140128           tbecod = 'PVO';
022700140128           tbeke1 = c1setll;
022800060428
022900140128         //?Se è stato scelto il posizionamento
023000140128           IF  c1setll <> *blanks;
023100140128             wrkkey = '2';
023200140128         //?Altrimenti
023300140128           ELSE;
023400140128             wrkkey = '1';
023500140128           ENDIF;
023600060428
023700140128         //?Posizionamento file
023800140128           exsr sr_setlltbe01;
023900060428
024000140128         //?Fino a che non è fine file
024100140128           DOU  %eof(TNTBE01L);
024200060428
024300140128         //?leggo file
024400140128             reade(n) tbecod tntbe01l;
024500060428
024600140128           //?Fine file esco
024700140128             IF  %eof(tntbe01l);
024800140128               leave;
024900140128             ENDIF;
025000060428
025100140128           //?Se in "ricerca/scelta" non carico i records annullati
025200140128             IF  (*in01 and tbeatb = *blanks) or not *in01;
025300140128             //?Scrivo subfile
025400140128               clear s1opzione;
025500140128               exsr sr_wtrs01;
025600140128             ENDIF;
025700060428
025800140128           ENDDO;
025900060428
026000140128       //?Fine caricamento subfile
026100140128         ENDIF;
026200060428
026300140128       //?Se scritto almeno un record
026400140128         IF  s01nrr > 0;
026500140128         //?Indicatore di sflend
026600140128           *in21 = *on;
026700140128         ENDIF;
026800060428
026900140128       //?Se non scritto neanche un record
027000140128         IF  s01nrr = 0;
027100140128         //?Indicatore di sfldsp
027200140128           *in23 = *on;
027300140128         ENDIF;
027400060428
027500140128       //?Disattivo opzione di caricamento subfile
027600140128         wrkcars01 = *off;
027700060428
027800140128       ENDSR;
027900060428
028000060502       // ------------------------------------------------------------------------
028100140128       //?Gestione videata.
028200060502       // ------------------------------------------------------------------------
028300140128       BEGSR  sr_d01;
028400060428
028500140128       //?Imposto il video a seconda della funzione richiesta
028600140128         exsr sr_setvideo;
028700060428
028800140128       //?Imposto variabile
028900140128         wvideo = 'D01';
029000060428
029100140128       //?Fino a che la variabile resta settata come 'D01'
029200140128         DOU  wvideo <> 'D01';
029300060428
029400140128           *in02 = *off;
029500140128           *in03 = *off;
029600140128           *in04 = *off;
029700060428
029800140128         //?Se immessa opzione di 'scelta'
029900140128           IF  BPVOopzio = '01';
030000140128             BPVOke1 = s1tbeke1;
030100140128             BPVOke2 = s1tbeke2;
030200140128             exsr sr_uscita;
030300060428
030400140128         //?Se immessa un'altra opzione
030500140128           ELSE;
030600140128         //?Se non è immissione/copia proteggo il campo della causale
030700140128             IF  BPVOcomand <> 'J' and BPVOopzio <> '03';
030800140128               *in02 = *on;
030900140128             ENDIF;
031000140128         //?Se immessa opzione di 'visualizzazione' 'cancellazione/ripristino'
031100140128         //?proteggo i campi del video
031200140128             IF  BPVOopzio = '04' or BPVOopzio = '05';
031300140128               *in03 = *on;
031400140128             ENDIF;
031500140128         //?Se immessa opzione di 'visualizzazione'
031600140128         //?non attivo F6
031700140128             IF  BPVOopzio = '05';
031800140128               *in04 = *on;
031900140128             ENDIF;
032000140128         //?Emetto il video
032100140128             exfmt TBPVOd01;
032200140128           ENDIF;
032300060428
032400140128         //?Reset indicatore generico di errore
032500140128           *in28 = *off;
032600060508
032700140128         //?Pulisco il campo messaggi
032800140128           clear vd1msg;
032900010430
033000140128         //?Controllo tasti funzionali del video
033100140128           SELECT;
033200060428
033300140128           //?F3=Fine
033400140128             WHEN  *inkc;
033500140128               BPVOricez = 'C';
033600140128               wvideo = *blanks;
033700140128               unlock tntbe01l;
033800140128               exsr sr_uscita;
033900060428
034000140128           //?F6=Conferma
034100140128             WHEN  *inkf;
034200140128               BPVOricez = 'F';
034300140128           //?Controllo e decodifico i dati del video
034400140128               exsr sr_ctrd01;
034500140128           //?Conferma per annullo/ripristino
034600140128               IF  BPVOopzio = '04';
034700140128                 SELECT;
034800140128             //?Annullamento
034900140128                   WHEN  tbeatb = *blanks;
035000140128                     tbeatb = 'A';
035100140128             //?Ripristino
035200140128                   WHEN  tbeatb = 'A';
035300140128                     clear tbeatb;
035400140128                 ENDSL;
035500140128               ENDIF;
035600060428
035700140128           //?Se non riscontrati errori emetto la finestra con i dati per la trasmissione
035800140128               IF  not *in28;
035900140128                 wrkcarw01 = *on;
036000140128                 wvideo = 'W01';
036100140128                 exsr sr_w01;
036200140128               ENDIF;
036300060428
036400140128           //?F8=Record successivo
036500140128             WHEN   *inkh;
036600140128               clear s1opzione;
036700140128               wrkcars01 = *off;
036800140128               wvideo = 'C01';
036900140128               BPVOricez = 'H';
037000060505
037100140128           //?F12=Ritorno
037200140128             WHEN  *inkl;
037300140128               clear s1opzione;
037400140128               BPVOricez = 'L';
037500140128               unlock tntbe01l;
037600140128             //?Se non è f12 da immissione/copia non ricarico il subfile
037700140128               IF  BPVOcomand = 'J' or BPVOopzio = '03';
037800140128                 wrkcars01 = *on;
037900140128               ELSE;
038000140128                 wrkcars01 = *off;
038100140128               ENDIF;
038200140128               wvideo = 'C01';
038300060428
038400140128           //?Invio
038500140128             OTHER;
038600140128               IF  not *in03;
038700140128                 exsr sr_ctrd01;
038800140128               ENDIF;
038900060428
039000140128           ENDSL;
039100060428
039200140128       //?Fine gestione 'D01'
039300140128         ENDDO;
039400060428
039500140128       ENDSR;
039600060502
039700060502       // ------------------------------------------------------------------------
039800140128       //?Ripeto opzione in tutte le righe del subfile.
039900060502       // ------------------------------------------------------------------------
040000140128       BEGSR  sr_ripetiopz;
040100060428
040200140128         IF  c1csrrrn > 0;
040300140128           s01nrr = c1csrrrn;
040400140128           chain s01nrr TBPVOs01;
040500140128           IF  %found and s1opzione > 0;
040600140128             savopzione = s1opzione;
040700140128             *in22 = *on;
040800140128             update TBPVOs01;
040900060428
041000140128             wrkeofs01 = *off;
041100140128             DOU  wrkeofs01 = *on;
041200140128               s01nrr = s01nrr + 1;
041300140128               chain s01nrr TBPVOs01;
041400140128               IF  %found;
041500140128                 s1opzione = savopzione;
041600140128                 update TBPVOs01;
041700140128               ELSE;
041800140128                 wrkeofs01 = *on;
041900140128               ENDIF;
042000140128             ENDDO;
042100060428
042200140128             *in22 = *off;
042300060428
042400140128           ENDIF;
042500060428
042600140128         ENDIF;
042700060428
042800140128       ENDSR;
042900060428
043000060502       // ------------------------------------------------------------------------
043100140128       //?Imposto i dati a video.
043200060502       // ------------------------------------------------------------------------
043300140128       BEGSR  sr_setvideo;
043400060428
043500140128       //?Pulisco il formato video D01
043600140128         exsr sr_puld01;
043700060428
043800140128       //?Controllo se 'immissione' 'modifica' 'copoa' o altro
043900140128         SELECT;
044000060428
044100140128         //?F10=Immissione
044200140128           WHEN  BPVOcomand = 'J';
044300140128             d1descopz = 'Immissione';
044400060428
044500140128         //?Opzione "02"=modifica
044600140128           WHEN  BPVOopzio = '02';
044700140128             d1descopz = 'Modifica';
044800140128             exsr sr_imposta;
044900060428
045000140128         //?Opzione "03"=copia
045100140128           WHEN  BPVOopzio = '03';
045200140128             d1descopz = 'Copia';
045300140128             exsr sr_imposta;
045400060428
045500140128         //?Opzione "04"=cancellazione/ripristino
045600140128           WHEN  BPVOopzio = '04';
045700140128             exsr sr_imposta;
045800140128           //?Se richiesta 'cancellazione'
045900140128             IF  tbeatb = *blanks;
046000140128               d1descopz = 'Annullamento';
046100140128             ENDIF;
046200140128           //?se richiesto 'ripristino'
046300140128             IF  tbeatb = 'A';
046400140128               d1descopz = 'Ripristino';
046500140128             ENDIF;
046600060502
046700140128         //?Opzione "05"=visualizzazione
046800140128           WHEN  BPVOOpzio ='05';
046900140128             d1descopz = 'Visualizzazione';
047000140128             exsr sr_imposta;
047100060502
047200140128       //?Fine scelta
047300140128         ENDSL;
047400060502
047500140128       //?Centro la descrizione della funzione nel formato video
047600060502        len = (%len(d1descopz) - %len(%trimr(d1descopz))) / 2;
047700060502        d1descopz = %subst(blanks:1:len) + %trimr(d1descopz);
047800060502
047900140128       ENDSR;
048000060502
048100060502       // ------------------------------------------------------------------------
048200140128       //?Controllo video.
048300060502       // ------------------------------------------------------------------------
048400140128       BEGSR  sr_ctrd01;
048500060502
048600140128         *in28 = *off;
048700140128         *in40 = *off;
048800140128         *in41 = *off;
048900140128         *in42 = *off;
049000010430
049100140128       //?Cliente
049200140128         IF  d1tbeke1 = *blanks;
049300140128           vd1msg = msg(01);
049400140128           *in28 = *on;
049500140128           *in40 = *on;
049600140128           leavesr;
049700140128         ENDIF;
049800140129
049900140129       //?Se lungo 7
050000140129         IF  %subst(D1tbeke1:8:3) = *blanks;
050100140129       //?deve essere numerica fino a 7
050200140129         IF (%subst(d1tbeke1:1:1) < *zeros and %subst(d1tbeke1:1:1) <> *blanks)
050300140128         or (%subst(d1tbeke1:2:1) < *zeros and %subst(d1tbeke1:2:1) <> *blanks)
050400140128         or (%subst(d1tbeke1:3:1) < *zeros and %subst(d1tbeke1:3:1) <> *blanks)
050500140128         or (%subst(d1tbeke1:4:1) < *zeros and %subst(d1tbeke1:4:1) <> *blanks)
050600140128         or (%subst(d1tbeke1:5:1) < *zeros and %subst(d1tbeke1:5:1) <> *blanks)
050700140128         or (%subst(d1tbeke1:6:1) < *zeros and %subst(d1tbeke1:6:1) <> *blanks)
050800140128         or (%subst(d1tbeke1:7:1) < *zeros and %subst(d1tbeke1:7:1) <> *blanks);
050900140128           vd1msg = msg(04);
051000140128           *in28 = *on;
051100140128           *in40 = *on;
051200140128           leavesr;
051300140129         ENDIF;
051400140129       //?Controllo se esiste in CNACO
051500140129           kacoksc = %int(%subst(d1TBEke1:1:7));
051600140129           chain (kacokut:DUTkci:kacoksc) CNACO00F;
051700140129           IF  not %found(CNACO00F);
051800140129             vd1msg = msg(05);
051900140129             *in28 = *on;
052000140129             *in40 = *on;
052100140129             leavesr;
052200140129           ENDIF;
052300140129           d1descli = ACOrag;
052400140129         ENDIF;
052500140129
052600140129       //?Se lungo 10
052700140129         IF  %subst(D1tbeke1:8:3) <> *blanks;
052800140129       //?deve essere numerica fino a 10
052900140129           IF (%subst(d1tbeke1:1:1) < *zeros and
053000140129               %subst(d1tbeke1:1:1) <> *blanks)
053100140129           or (%subst(d1tbeke1:2:1) < *zeros and
053200140129               %subst(d1tbeke1:2:1) <> *blanks)
053300140129           or (%subst(d1tbeke1:3:1) < *zeros and
053400140129               %subst(d1tbeke1:3:1) <> *blanks)
053500140129           or (%subst(d1tbeke1:4:1) < *zeros and
053600140129               %subst(d1tbeke1:4:1) <> *blanks)
053700140129           or (%subst(d1tbeke1:5:1) < *zeros and
053800140129               %subst(d1tbeke1:5:1) <> *blanks)
053900140129           or (%subst(d1tbeke1:6:1) < *zeros and
054000140129               %subst(d1tbeke1:6:1) <> *blanks)
054100140129           or (%subst(d1tbeke1:7:1) < *zeros and
054200140129               %subst(d1tbeke1:7:1) <> *blanks)
054300140129           or (%subst(d1tbeke1:8:1) < *zeros and
054400140129               %subst(d1tbeke1:8:1) <> *blanks)
054500140129           or (%subst(d1tbeke1:9:1) < *zeros and
054600140129               %subst(d1tbeke1:9:1) <> *blanks)
054700140129           or (%subst(d1tbeke1:10:1) < *zeros and
054800140129               %subst(d1tbeke1:10:1) <> *blanks);
054900140129             vd1msg = msg(04);
055000140129             *in28 = *on;
055100140129             *in40 = *on;
055200140129             leavesr;
055300140129           ENDIF;
055400140129           kacrcro = %int(%subst(d1TBEke1:1:10));
055500140129           chain (kacrcro) FNACR01L;
055600140129           IF  not %found(FNACR01L);
055700140129             vd1msg = msg(05);
055800140129             *in28 = *on;
055900140129             *in40 = *on;
056000140129             leavesr;
056100140129           ENDIF;
056200140129           d1descli = ACRrsr;
056300140129         ENDIF;
056400140128
056500140128       //?Key 2
056600140128         IF  d1tbeke2 = *blanks;
056700140128           vd1msg = msg(02);
056800140128           *in28 = *on;
056900140128           *in41 = *on;
057000140128           leavesr;
057100140128         ENDIF;
057200060508
057300140128       //?Se immissione controllo che non esista già
057400140128         IF  BPVOcomand = 'J' or BPVOopzio = '03';
057500140128           tbecod = 'PVO';
057600140128           tbeke1 = d1tbeke1;
057700140128           tbeke2 = d1tbeke2;
057800140128           clear tbelin;
057900140128           chain(n) (tbecod:tbeke1:tbeke2:tbelin) tntbe01l;
058000140128           IF  %found(tntbe01l);
058100140128             vd1msg = msg(03);
058200140128             *in28 = *on;
058300140128             *in40 = *on;
058400140128             leavesr;
058500140128           ENDIF;
058600140128         ENDIF;
058700140128
058800140128       //?Almeno un dato inserito
058900140128         IF  D1pvocb = *blanks and D1pvofr = *blanks and
059000140128             D1pvoks = *blanks and D1pvofd = *blanks and
059100140128             D1pvoic = *blanks and D1pvocomc = *blanks and
059200140128             D1pvosrm = *blanks and D1pvopre = *blanks;
059300140129             vd1msg = msg(04);
059400140128           *in28 = *on;
059500140128           *in42 = *on;
059600140128           leavesr;
059700140128         ENDIF;
059800060502
059900140128       ENDSR;
060000060504
060100060504       // ------------------------------------------------------------------------
060200140128       //?Gestione video dati di trasmissione.
060300060504       // ------------------------------------------------------------------------
060400140128       BEGSR  sr_w01;
060500060504
060600140128       //?Imposto i dati a video
060700140128         exsr sr_carw01;
060800060504
060900140128       //?Fino a che la variabile resta settata come 'W01'
061000140128         DOU  wvideo <> 'W01';
061100060504
061200140128         //?Reset indicatore generico di errore
061300140128           *in28 = *off;
061400060504
061500140128         //?Emetto il video
061600140128           exfmt TBPVOw01;
061700060504
061800140128         //?Controllo tasti funzionali del video
061900140128           SELECT;
062000060504
062100140128         //?F6=Conferma
062200140128             WHEN  *inkf;
062300140128           //?controllo i dati del video
062400140128               exsr sr_ctrw01;
062500140128          //?se non riscontrati errori aggiorno il record corrente
062600140128               IF  not *in28;
062700140128                 exsr sr_aggiorna;
062800140128                 IF  not *in28;
062900140128                   IF  BPVOcomand = 'J';
063000140128                     wvideo = 'D01';
063100140128                     exsr sr_setvideo;
063200140128                   ELSE;
063300140128                     wvideo = 'C01';
063400140128                   ENDIF;
063500140128                 ENDIF;
063600140128               ENDIF;
063700060504
063800140128         //?F12=Ritorno
063900140128             WHEN  *inkl;
064000140128               wvideo = 'D01';
064100140128               clear BPVOricez;
064200060504
064300140128         //?Invio
064400140128             OTHER;
064500140128               exsr sr_ctrw01;
064600060504
064700140128           ENDSL;
064800060504
064900140128       //?fine gestione 'W01'
065000140128         ENDDO;
065100060504
065200140128       ENDSR;
065300060504
065400060504       // ------------------------------------------------------------------------
065500140128       //?Imposto i dati di trasmissione.
065600060504       // ------------------------------------------------------------------------
065700140128       BEGSR  sr_carw01;
065800060504
065900140128       //?Pulisco i campi
066000060504         clear w1ftt;
066100060504         clear w1flt;
066200060504         clear w1ftr;
066300060504         clear w1dtr;
066400060504
066500140128       //?Se non immissione imposto i campi del file
066600140128         IF  BPVOcomand <> 'J';
066700140128           w1ftt = tbeftt;
066800140128           w1flt = tbeflt;
066900140128           w1ftr = tbeftr;
067000140128         //?Imposto la data
067100140128           IF  tbedtr <> *zeros;
067200140128             clear wlbdat;
067300140128             g02inv = tbedtr;
067400140128             g02err = '3';
067500140128             xsrda8(wlbdat);
067600140128             w1dtr = g02dat;
067700140128           ENDIF;
067800060504
067900140128         ENDIF;
068000060504
068100140128       ENDSR;
068200060504
068300060504       // ------------------------------------------------------------------------
068400140128       //?Controllo i dati di trasmissione.
068500060504       // ------------------------------------------------------------------------
068600140128       BEGSR  sr_ctrw01;
068700060504
068800140128       ENDSR;
068900060502
069000060502       // ------------------------------------------------------------------------
069100140128       //?Aggiorno tabella.
069200060502       // ------------------------------------------------------------------------
069300140128       BEGSR  sr_aggiorna;
069400060502
069500140128       //?Imposto campi del file
069600140128         clear tbelin;
069700140128         tbeke1 = d1tbeke1;
069800140128         tbeke2 = d1tbeke2;
069900060502
070000140128         d§PVOcb = d1pvocb;
070100140128         d§PVOfr = d1pvofr;
070200140128         d§PVOks = d1pvoks;
070300140128         d§PVOfd = d1pvofd;
070400140128         d§PVOic = d1pvoic;
070500140128         d§PVOcomc = d1pvocomc;
070600140128         d§PVOsrm = d1pvosrm;
070700140128         d§PVOpre = d1pvopre;
070800060502
070900140128         tbeuni = dPVO;
071000060502
071100140128         tbeftt = w1ftt;
071200140128         tbeflt = w1flt;
071300140128         clear tbeftr;
071400140128         clear tbedtr;
071500060502
071600140128       //?Controllo quale tasto funzione o opzione ha richiesto l'aggiornamento
071700140128         SELECT;
071800060502
071900140128         //?F10=immissione - Opzione "03"=copia
072000140128           WHEN  BPVOcomand = 'J' or BPVOopzio = '03';
072100140128           //?scrivo nuovo record con gestione errore per chiave duplicata
072200140128             write tntbe000;
072300060502
072400140128         //?Opzione "02"=modifica
072500140128           WHEN  BPVOopzio = '02';
072600140128           //?update record e controllo errore per chiave duplicata
072700140128             update tntbe000;
072800060502
072900140128         //?Opzione "04"=cancellazione/ripristino
073000140128           WHEN  BPVOopzio = '04';
073100140128             update tntbe000;
073200010430
073300140128         ENDSL;
073400060502
073500140128       ENDSR;
073600060502
073700060502       // ------------------------------------------------------------------------
073800140128       //?Pulizia video.
073900060502       // ------------------------------------------------------------------------
074000140128       BEGSR  sr_puld01;
074100060502
074200140128         clear d1descopz;
074300140128         clear d1tbeke1;
074400140129         clear d1descli;
074500140128         clear d1tbeke2;
074600140128         clear d1pvocb;
074700140128         clear d1pvofr;
074800140128         clear d1pvoks;
074900140128         clear d1pvofd;
075000140128         clear d1pvoic;
075100140128         clear d1pvocomc;
075200140128         clear d1pvosrm;
075300140128         clear d1pvopre;
075400060504
075500140128         *in28 = *off;
075600140128         *in40 = *off;
075700140128         *in41 = *off;
075800140128         *in42 = *off;
075900060502
076000140128       ENDSR;
076100060502
076200060502       // ------------------------------------------------------------------------
076300140128       //?Imposto i dati a video.
076400060502       // ------------------------------------------------------------------------
076500140128       BEGSR  sr_imposta;
076600060502
076700140128       //?Recupero i dati dal file
076800140128         ktbeke1 = s1tbeke1;
076900140128         clear tbelin;
077000140128         chain (tbecod:ktbeke1:s1tbeke2:tbelin) tntbe01l;
077100060502
077200140128       //?Imposto i campi a video
077300140128         d1tbeke2 = tbeke2;
077400140128         d1tbeke1 = tbeke1;
077500140128         dPVO = tbeuni;
077600140128         d1pvocb = d§PVOcb;
077700140128         d1pvofr = d§PVOfr;
077800140128         d1pvoks = d§PVOks;
077900140128         d1pvofd = d§PVOfd;
078000140128         d1pvoic = d§PVOic;
078100140128         d1pvocomc = d§PVOcomc;
078200140128         d1pvosrm = d§PVOsrm;
078300140128         d1pvopre = d§PVOpre;
078400140129
078500140129       //?Controllo se key 1 lunga 7
078600140129         IF  %subst(TBEke1:8:3) = *blanks;
078700140129         //?Prendo la ragione sociale dall'anagrafica clienti PdC
078800140129           kacoksc = %int(%subst(TBEke1:1:7));
078900140129           chain (kacokut:DUTkci:kacoksc) CNACO00F;
079000140129           IF  %found(CNACO00F);
079100140129             d1descli = ACOrag;
079200140129           ENDIF;
079300140129         ENDIF;
079400140129       //?Controllo se key 1 lunga 10
079500140129         IF  %subst(TBEke1:8:3) <> *blanks;
079600140129         //?Prendo la ragione sociale dall'anagrafica clienti ritiro
079700140129           kacrcro = %int(%subst(TBEke1:1:10));
079800140129           chain (kacrcro) FNACR01L;
079900140129           IF  %found(FNACR01L);
080000140129             d1descli = ACRrsr;
080100140129           ENDIF;
080200140129         ENDIF;
080300060502
080400140128       ENDSR;
080500060502
080600060502       // ------------------------------------------------------------------------
080700140128       //?Gestione del subfile.
080800060502       // ------------------------------------------------------------------------
080900140128       BEGSR  sr_gestsfl;
081000060502
081100140128       //?Set flag
081200140128         wrkeofs01 = *off;
081300060502
081400140128       //?Inizio lettura subfile
081500140128         DOW  wrkeofs01 = *off and *in21;
081600140128           readc TBPVOs01;
081700140128         //?se fine subfile
081800140128           IF  %eof;
081900140128             wrkcars01 = *on;
082000140128             leave;
082100140128           ENDIF;
082200140128         //?Se è stata immessa un'opzione
082300140128           IF  s1opzione <> *zeros;
082400140128           //?Reset ds di servizio
082500140128             clear TNTBPVODS;
082600140128           //?controllo ed elaborazione opzione immessa
082700140128             SELECT;
082800140128             //?opzione "01"=scelta
082900140128               WHEN  s1opzione = 1 and *in01;
083000140128                 BPVOopzio = '01';
083100140128             //?opzione "02"=modifica
083200140128               WHEN  s1opzione = 2 and not *in01 and s1tbeatb = *blanks;
083300140128                 BPVOopzio = '02';
083400140128             //?opzione "03"=copia
083500140128               WHEN  s1opzione = 3 and not *in01 and s1tbeatb = *blanks;
083600140128                 BPVOopzio = '03';
083700140128            //?opzione "04"=annullo/ripristino
083800140128               WHEN  s1opzione = 4 and not *in01;
083900140128                 BPVOopzio = '04';
084000140128           //?opzione "05"=visualizzazione
084100140128               WHEN  s1opzione = 5;
084200140128                 BPVOopzio = '05';
084300140128             ENDSL;
084400010503
084500140128         //?se immessa almeno un opzione valida
084600140128             IF  BPVOopzio <> *blanks;
084700140128           //?gstione del formato video
084800140128               exsr sr_d01;
084900140128             //?se la gestione si è chiusa con ...
085000140128               SELECT;
085100140128               //?F6=Conferma
085200140128                 WHEN  BPVOricez = 'F';
085300140128                   clear s1opzione;
085400140128                   wrkcars01 = *on;
085500140128               //?F12=Ritorno
085600140128                 WHEN  BPVOricez = 'L';
085700140128                   clear s1opzione;
085800140128                   wrkeofs01 = *on;
085900140128               //?altrimenti
086000140128                 OTHER;
086100140128                   *in22 = *on;
086200140128               ENDSL;
086300060502
086400140128               update TBPVOs01;
086500140128               *in22 = *off;
086600140128           //?fine opzione
086700140128             ENDIF;
086800140128         //?fine opzione
086900140128           ENDIF;
087000060502
087100140128         ENDDO;
087200060502
087300140128       ENDSR;
087400060502
087500060502       // ------------------------------------------------------------------------
087600140128       //?Posizionamento sul file.
087700060502       // ------------------------------------------------------------------------
087800140128       BEGSR  sr_setlltbe01;
087900060502
088000140128         SELECT;
088100140128       //?Chiave totale
088200140128           WHEN  wrkkey = '1';
088300140128             setll tbecod tntbe01l;
088400140128       //?Chiave parziale
088500140128           WHEN  wrkkey = '2';
088600140128             setll (tbecod:tbeke1) tntbe01l;
088700140128         ENDSL;
088800060502
088900140128       ENDSR;
089000060502
089100060502       // ------------------------------------------------------------------------
089200140128       //?Scrivo subfile.
089300060502       // ------------------------------------------------------------------------
089400140128       BEGSR  sr_wtrs01;
089500060502
089600140128       //?Se non raggiunto limite massimo di caricamento
089700140128         IF  s01nrr < 9999;
089800140128         //?Imposto campi di subfile
089900140128           exsr sr_sets01;
090000140128         //?Imposto numeratore righe e numero relativo di record
090100140128           s01nrr = s01nrr + 1;
090200140128         //?scrivo subfile
090300140128           write TBPVOs01;
090400140128         ENDIF;
090500060502
090600140128       ENDSR;
090700060502
090800060502       // ------------------------------------------------------------------------
090900140128       //?Imposto campi del subfile.
091000060502       // ------------------------------------------------------------------------
091100140128       BEGSR  sr_sets01;
091200060502
091300140128         s1tbeke1 = tbeke1;
091400140128         s1tbeke2 = tbeke2;
091500140128         dPVO = tbeuni;
091600140128         s1tbeatb = tbeatb;
091700140129
091800140129         clear  s1descli;
091900140129         //?Controllo se key 1 lunga 7
092000140129         IF  %subst(TBEke1:8:3) = *blanks;
092100140129           //?Prendo la ragione sociale dall'anagrafica clienti PdC
092200140129           kacoksc = %int(%subst(TBEke1:1:7));
092300140129           chain (kacokut:DUTkci:kacoksc) CNACO00F;
092400140129           IF  %found(CNACO00F);
092500140129             s1descli = ACOrag;
092600140129           ENDIF;
092700140129         ENDIF;
092800140129         //?Controllo se key 1 lunga 10
092900140129         IF  %subst(TBEke1:8:3) <> *blanks;
093000140129           //?Prendo la ragione sociale dall'anagrafica clienti ritiro
093100140129           kacrcro = %int(%subst(TBEke1:1:10));
093200140129           chain (kacrcro) FNACR01L;
093300140129           IF  %found(FNACR01L);
093400140129             s1descli = ACRrsr;
093500140129           ENDIF;
093600140129         ENDIF;
093700060502
093800140128       ENDSR;
093900060428
094000060502       // ------------------------------------------------------------------------
094100140128       //?Routine iniziale.
094200060502       // ------------------------------------------------------------------------
094300140128       BEGSR  *inzsr;
094400060428
094500060428      /end-free
094600060428
094700060428     c     *entry        plist
094800060428     c                   parm                    kpjba
094900060428
095000060428      /free
095100060428         in(e) §azute;
095200140128         IF  not %error;
095300140128           in(e) §datiute;
095400140128         ENDIF;
095500140128         IF  %error or rsut = *blanks;
095600140128           tibs34r(tibs34ds);
095700140128           in §azute;
095800140128           in §datiute;
095900140128         ENDIF;
096000060428
096100140128       ENDSR;
096200060502
096300060502       // ------------------------------------------------------------------------
096400140128       //?Uscita dal programma.
096500060502       // ------------------------------------------------------------------------
096600140128       BEGSR  sr_uscita;
096700060502
096800140128         IF  BPVOesito = *blanks;
096900140128           BPVOesito = eseguito;
097000140128         ENDIF;
097100060502
097200140128         kpjbu = TNTBPVODS;
097300060502
097400140128         *inlr = *on;
097500140128         return;
097600060502
097700140128       ENDSR;
097800060502
097900060502      /end-free
098000060502
098100060502** -MSG-                                                                     *
098200140128Immettere il codice cliente lungo 10 o lungo 7                                 01
098300140128Immettere il tipo key 2 "O" - "C" - "RCK"                                      02
098400140128Tabella già esistente                                                          03
098500140128Immettere almeno un dato                                                       04
098600140129Codice non trovato in anagrafica                                               05
