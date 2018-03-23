000100130301       //======================================================================
000200130304       //?TNTE60R - Stampa simulazione delta                            (batch)  ?
000300130301       //======================================================================
000400120705
000500120705       //--------------------------------------------------------------
000600120705       //?Specifiche di controllo.                                     ?
000700120705       //--------------------------------------------------------------
000800120705
000900120705     h decedit('0,') datedit(*ymd.) option(*nodebugio)
001000120705
001100120705       //--------------------------------------------------------------
001200120705       //?Dichiarazione file.                                          ?
001300120705       //--------------------------------------------------------------
001400130306
001500130306       //?File organigramma
001600130306     fAZORG01L  if   e           k disk
001700130306
001800130306       //?File dettaglio simulazione delta
001900130306     fWFDBF01L  if   e           k disk
002000120705
002100130301       //?Tabelle?
002200120705     fTABEL00F  if   e           k disk
002300130304
002400130304       //?Tariffa/offerta
002500130304     fTNTAM01L  if   e           k disk
002600130304     fTITAD04L  if   e           k disk
002700130301
002800130301       //?Trattativa
002900130301     fTIVIS05L  if   e           k disk
003000120705
003100130301       //?File di stampa?
003200130306     fTNTE60P   o    e             printer oflind(*in99)
003300130305       //?File di stampa dettaglio?
003400130306     fTNTE60P1  o    e             printer oflind(*in98) usropn
003500120705
003600120705       //--------------------------------------------------------------
003700120705       //?Definizione costanti.                                        ?
003800120705       //--------------------------------------------------------------
003900120705
004000130301       //?Codice utente x TABEL00F?
004100120705     d c_Kut           c                   const(1)
004200120705
004300120705       //--------------------------------------------------------------
004400120705       //?Definizione schiere.                                         ?
004500120705       //--------------------------------------------------------------
004600120705
004700130301       //?Varia, importo e n° spedizioni x Singolo Cliente?
004800130301     d sk_Voce         s                   dim(99)  inz  like(WF1sv1)
004900120706     d sk_Impo         s             12  3 dim(99)  inz
005000120706     d sk_Sped         s              9  0 dim(99)  inz
005100130305
005200130305       //?Descrizione Cliente - linea di partenza
005300130305     d sk_des          s             30    dim(05)  inz
005400130304
005500130304       //?Codici tassazione
005600130304     d cts             s              2    dim(600) inz
005700130304
005800130304       //?Scaglioni
005900130304     d ssgl            s             13  3 dim(200) inz
006000130304
006100130304       //?Schiere totali
006200130305     d Tnsp            s              6  0 dim(200)                             n.Sped.
006300130305     d Gnsp            s              6  0 dim(200)                             n.Sped.
006400130305     d Tpkg            s              9  1 dim(200)                             Peso
006500130305     d Gpkg            s              9  1 dim(200)                             Peso
006600130305     d Tncl            s              7  0 dim(200)                             n.Colli
006700130305     d Gncl            s              7  0 dim(200)                             n.Colli
006800130305     d Tvlb            s              7  3 dim(200)                             Volume
006900130305     d Gvlb            s              7  3 dim(200)                             Volume
007000130305     d Tqft            s             15  3 dim(200)                             Qta Fatt.
007100130305     d Gqft            s             15  3 dim(200)                             Qta Fatt.
007200130305     d Tric            s             18  5 dim(200)                             Ricavi
007300130305     d Gric            s             18  5 dim(200)                             Ricavi
007400130305     d Tcom            s             18  5 dim(200)                             Compet.
007500130305     d Gcom            s             18  5 dim(200)                             Compet.
007600120705
007700120705       //--------------------------------------------------------------
007800120705       //?Definizione aree dati.                                       ?
007900120705       //--------------------------------------------------------------
008000120705
008100130301       //?Dati utente?
008200120705     d §AzUte        e ds                  extname(AZUTE00F)
008300120705     d                                     dtaara
008400120705     d §DatiUte      e ds                  extname(dDatiUte)
008500120705     d                                     dtaara
008600120705
008700120705       //--------------------------------------------------------------
008800120705       //?Definizione strutture dati.                                  ?
008900120705       //--------------------------------------------------------------
009000120705
009100130301       //?Parametri ricevuti?
009200120705     d KPJBA         e ds
009300130301
009400130301       //?Parametri per il pgm chiamato
009500130301     d param           ds                  inz
009600130301     d  parks1                 1      7  0
009700130301     d  parks2                 8     14  0
009800130301     d  parks3                15     21  0
009900130301     d  parks4                22     28  0
010000130301     d  parks5                29     35  0
010100130305     d  sk_ksc                 1     35  0 dim(5)
010200130301     d  parta1                36     38  0
010300130301     d  parta2                39     41  0
010400130301     d  parta3                42     44  0
010500130301     d  parta4                45     47  0
010600130301     d  parta5                48     50  0
010700130301     d  parln1                51     53  0
010800130301     d  parln2                54     56  0
010900130301     d  parln3                57     59  0
011000130301     d  parln4                60     62  0
011100130301     d  parln5                63     65  0
011200130305     d  sk_lnp                51     65  0 dim(5)
011300130301     d  pards1                66     73  0
011400130301     d  pards2                74     81  0
011500130301     d  parct1                82     83
011600130301     d  parct2                84     85
011700130301     d  parct3                86     87
011800130301     d  parct4                88     89
011900130301     d  parct5                90     91
012000130301     d  parct6                92     93
012100130301     d  parct7                94     95
012200130301     d  parct8                96     97
012300130301     d  parct9                98     99
012400130301     d  parct10              100    101
012500130301     d  parkgd               102    108  1
012600130301     d  parkga               109    115  1
012700130301     d  parcod               116    121  0
012800130301     d  parcoa               122    127  0
012900130301     d  partaof              128    128
013000130301     d  parksc               129    135  0
013100130301     d  parctr               136    138  0
013200130301     d  parprg               139    141  0
013300130301     d  parlnp               142    144  0
013400130301     d  pardtcal             145    145
013500130301     d  pardet               146    146
013600130301     d  parnrv               147    153  0
013700130301     d  parddt               154    161  0
013800130306
013900130306       //?Data Spedizione
014000130306     d                 ds
014100130306     d  WFDaas                 1      4  0
014200130306     d  WFDmgs                 5      8  0
014300130306     d  wfddsp                 1      8  0
014400120705
014500130306       //?Tabella "CC" = Codici Varie
014600120705     d dsCC          e ds                  inz
014700130306
014800130306       //?Tabella "CT" = Codici Tassazione?
014900130306     d dsCT          e ds                  inz
015000130301
015100130301       //?File Work Varie
015200130301     d WFDBF10F      e ds                  extname(WFDBF10F)
015300120705
015400130301       //?Status ds?
015500120705     d Status         sds
015600120705     d   SDSpgm          *proc
015700130305     d   SDSJob              244    253
015800120705
015900120705       //--------------------------------------------------------------
016000120705       //?Definizione variabili globali.                               ?
016100120705       //--------------------------------------------------------------
016200130301
016300130301       //?Flag booleani
016400130301     d wEnd            s               n   inz(*off)
016500130304     d wTAD            s               n   inz(*off)
016600130306     d wTOT            s               n   inz(*off)
016700120705
016800130301       //?Indici di schiera?
016900130304     d xi              s              4  0 inz
017000130304     d xc              s              4  0 inz
017100120705     d xx              s              3  0 inz
017200120705     d yy              s              3  0 inz
017300130305     d ww              s              3  0 inz
017400130305
017500130305       // -?Campi Data
017600130305     d wData_ISO       s               d   datfmt(*iso)
017700130305     d wData_EUR       s               d   datfmt(*eur)
017800120705
017900130301       //?Campi di comodo?
018000130304     d ultSGL          s              6  0 inz
018100130304     d ultCTS          s              6  0 inz
018200130301     d wConta          s              1  0 inz
018300130306     d wCTS            s              2    inz
018400130306     d wdelta          s              4  1 inz
018500130305     d wDES            s             30    inz
018600130306     d wGAP            s             18  5 inz
018700130305     d wImpo           s                   like(WF1va1)   inz
018800130305     d wKSC            s              7  0 inz
018900130305     d wLNP            s              3  0 inz
019000130305     d wVaria          s                   like(WF1sv1)   inz
019100120705
019200120705       //--------------------------------------------------------------
019300120705       //?Definizione prototipi procedure.                             ?
019400120705       //--------------------------------------------------------------
019500120705
019600130301       //?Reperimento dati utente?
019700120705     d TIBS34ds      e ds                  inz
019800120705      /copy gaitrasrc/srcProtoPR,TIBS34R
019900120705
020000130301       //?Controllo/Decodifica cliente?
020100120705      /copy gaitrasrc/srcProtoPI,TIBS69R
020200120705      /copy gaitrasrc/srcProtoPR,TIBS69R
020300120705
020400130301       //?Controllo formale / Inversione data?
020500120705     d WLBdat          ds                  inz
020600120705     d   G08dat                1      8  0 inz
020700120705     d   G08inv                9     16  0 inz
020800120705     d   G08err               17     17    inz('3')
020900120705     d   G08tgi               18     22  0 inz
021000120705      /copy gaitrasrc/srcProtoPR,XSRDA8
021100120705
021200120705       //--------------------------------------------------------------
021300120705       //?Definizione key-list.                                        ?
021400120705       //--------------------------------------------------------------
021500120705
021600130301       //?File TABEL00F?
021700120705     d k03tabel00    e ds                  extname(TABEL00F : *key)
021800120705     d                                     prefix(k_)   inz
021900120705
022000120705       //--------------------------------------------------------------
022100120705       //?M A I N - L I N E                                            ?
022200120705       //--------------------------------------------------------------
022300120705
022400120705     c     *Entry        plist
022500120705     c                   parm                    KPJBA
022600120705
022700120705      /free
022800120705
022900130301       //?Operazioni iniziali?
023000130301       exsr  RoutInz;
023100120705
023200130304       //?Carico scaglioni
023300130304       exsr  Scaglioni;
023400130304
023500130304       //?Stampa la pagina dei parametri richiesi a video
023600130304       exsr  Parametri;
023700130305
023800130305       //?Elaboro per ogni cod. tassazione della tariffa/offerta
023900130306       exsr  Simulazione;
024000130304
024100130301       //?Elaborazione file riepilogo varie?
024200130307       exsr  ElabVarie;
024300120705
024400120705
024500130301       //?Operazioni finali?
024600130301       exsr  RoutEnd;
024700120705
024800120705       //--------------------------------------------------------------
024900120705       //?Operazioni iniziali.                                         ?
025000120705       //--------------------------------------------------------------
025100130301       BEGSR  RoutInz;
025200130301
025300130301         exec sql  set option  dynusrprf = *owner,
025400130301                               closqlcsr = *endmod;
025500120705
025600130301       //?Impostazione parametri ricevuti?
025700130301         param = kpjbu;
025800120705
025900130301       //?Reperimento dati job?
026000130301         exsr  DatiJob;
026100120705
026200130301       //?Impostazione campi fissi?
026300120705         clear  k03tabel00;
026400120705         k_TBLkut = c_Kut;
026500120705
026600130301       //?Impostazione campi in testata?
026700120705         P1Trsu = RsUt;
026800120705         P1Tpgm = SDSpgm;
026900130306
027000130306       //?Se richiesto apro file di stampa per dettaglio
027100130306         IF  pardet = 'S';
027200130306           open TNTE60P1;
027300130306         ENDIF;
027400120705
027500120705       ENDSR;
027600120705
027700120705       //--------------------------------------------------------------
027800120705       //?Reperimento Dati del job (Utente/Operativi).                 ?
027900120705       //--------------------------------------------------------------
028000130301       BEGSR  DatiJob;
028100120705
028200120705         in(E) §AzUte;
028300120705         if NOT %error;
028400120705           in(E) §DatiUte;
028500120705         endif;
028600120705         if %error or RSut = *blanks;
028700120705           clear TIBS34ds;
028800120705           tibs34r ( tibs34ds );
028900120705           in §AzUte;
029000120705           in §DatiUte;
029100120705         endif;
029200120705
029300120705       ENDSR;
029400130304
029500130304       //--------------------------------------------------------------
029600130304       //?Carico scaglioni.
029700130304       //--------------------------------------------------------------
029800130304       BEGSR  Scaglioni;
029900130304
030000130304         clear xi;
030100130304         clear xc;
030200130304         wTAD = *off;
030300130304
030400130304         IF  parlnp = 0;
030500130304           setll (parksc:parctr:parprg) TITAD04L;
030600130304           reade (parksc:parctr:parprg) TITAD04L;
030700130304         ELSE;
030800130304           setll (parksc:parctr:parprg:parlnp) TITAD04L;
030900130304           reade (parksc:parctr:parprg:parlnp) TITAD04L;
031000130304         ENDIF;
031100130304         DOW  not %eof(TITAD04L);
031200130304           wTAD = *on;
031300130304           exsr memTAD;
031400130304
031500130304           IF  parlnp = 0;
031600130304             reade (parksc:parctr:parprg) TITAD04L;
031700130304           ELSE;
031800130304             reade (parksc:parctr:parprg:parlnp) TITAD04L;
031900130304           ENDIF;
032000130304         ENDDO;
032100130304
032200130304         ultSGL = xi;
032300130306         ultcts = xc;
032400130304
032500130304         IF  not wTAD;
032600130304           exsr RoutEnd;
032700130304         ENDIF;
032800130304
032900130304       ENDSR;
033000130304
033100130304       //--------------------------------------------------------------
033200130304       //?Memorizza gli scaglioni.
033300130304       //--------------------------------------------------------------
033400130304       BEGSR  memTAD;
033500130304
033600130305         IF  %lookup(TADsgl:ssgl) = 0;
033700130305           xi += 1;
033800130305           ssgl(xi) = TADsgl;
033900130305         ENDIF;
034000130305
034100130305         IF  %lookup(TADcts:cts) = 0;
034200130305           xc += 1;
034300130307           cts(xc) = TADcts;
034400130305         ENDIF;
034500130304
034600130304       ENDSR;
034700130304
034800130304       //--------------------------------------------------------------
034900130304       //?Stampa pagina parametri.
035000130304       //--------------------------------------------------------------
035100130304       BEGSR  Parametri;
035200130304
035300130305       //?Decodifico i clienti richiesti
035400130305         ww = 1;
035500130305         FOR ww by 1 to 5;
035600130305           IF  sk_ksc(ww) <> *zeros;
035700130305             wKSC = sk_ksc(ww);
035800130305             clear wDES;
035900130305             exsr decoKSC;
036000130305             sk_des(ww) = wDES;
036100130305           ENDIF;
036200130305         ENDFOR;
036300130305
036400130305         desksc01 = sk_des(01);
036500130305         desksc02 = sk_des(02);
036600130305         desksc03 = sk_des(03);
036700130305         desksc04 = sk_des(04);
036800130305         desksc05 = sk_des(05);
036900130305
037000130305       //?Decodifico le Linee di partenza richieste
037100130305         ww = 1;
037200130306         clear sk_des;
037300130305         FOR ww by 1 to 5;
037400130305           IF  sk_lnp(ww) <> *zeros and sk_lnp(ww) <> 999;
037500130305             wLNP = sk_lnp(ww);
037600130305             clear wDES;
037700130305             exsr decoLNP;
037800130306             sk_des(ww) = wDES;
037900130305           ENDIF;
038000130305         ENDFOR;
038100130305
038200130305         deslnp01 = sk_des(01);
038300130305         deslnp02 = sk_des(02);
038400130305         deslnp03 = sk_des(03);
038500130305         deslnp04 = sk_des(04);
038600130305         deslnp05 = sk_des(05);
038700130305
038800130305       //?Giro le date per la stampa
038900130305         wData_ISO = %date(pards1:*iso);
039000130305         wData_EUR = wData_ISO;
039100130305         p1dtsda = %dec(wData_EUR);
039200130305         wData_ISO = %date(pards2:*iso);
039300130305         wData_EUR = wData_ISO;
039400130305         p1dtsal = %dec(wData_EUR);
039500130305
039600130305       //?Decodifico Cliente/Offerta
039700130305         IF  partaof = 'T';
039800130305           clear wDES;
039900130305           wksc = parksc;
040000130305           exsr decoKSC;
040100130305           destar = wDES;
040200130305           stptar = parksc;
040300130305         ELSE;
040400130305           stptar = parnrv;
040500130305           chain parnrv TIVIS05L;
040600130305           IF  %found(TIVIS05L);
040700130305             destar = VISrag;
040800130305           ENDIF;
040900130305         ENDIF;
041000130305
041100130305       //?Decodifico le Linea di partenza tariffa
041200130305         clear wDES;
041300130305         wLNP = parlnp;
041400130305         exsr decoLNP;
041500130305         deslnp = wDES;
041600130305
041700130305       //?Data calcolo
041800130305         IF  pardtcal = 'O';
041900130305           desdtcal = 'OGGI';
042000130305         ELSE;
042100130305           desdtcal = 'DATA SPEDIZIONE';
042200130305         ENDIF;
042300130305
042400130305       //?Stampo la pagina dei parametri di lancio
042500130305         write TE60PAR;
042600130306
042700130306       //?Stampo la testata
042800130306         write TE60T01;
042900130306
043000130306       //?Se richiesta stampo la testata del dettaglio
043100130306         IF  pardet = 'S';
043200130306           write TE601T01;
043300130306         ENDIF;
043400130304
043500130304       ENDSR;
043600130305
043700130305       //--------------------------------------------------------------
043800130305       //?Decodifico Cliente.
043900130305       //--------------------------------------------------------------
044000130305       BEGSR  decoKSC;
044100130305
044200130305         clear tibs69ds;
044300130305         I69kac = %dec(wKSC:7:0);
044400130305         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
044500130305         IF  O69err = *blanks;
044600130305           wDES = ACOrag;
044700130305         ENDIF;
044800120705
044900130305       ENDSR;
045000130305
045100130305       //--------------------------------------------------------------
045200130305       //?Decodifico Linea di Partenza.
045300130305       //--------------------------------------------------------------
045400130306       BEGSR  decoLNP;
045500130305
045600130305         chain wLNP AZORG01L;
045700130305         IF  %found(AZORG01L) and ORGFVA = *blanks and ORGfag <> 'V';
045800130305           wDES = ORGdes;
045900130305         ENDIF;
046000130305
046100130305       ENDSR;
046200130305
046300130305       //--------------------------------------------------------------
046400130305       //?Elaborazione della simulazione delta.
046500130305       //--------------------------------------------------------------
046600130305       BEGSR  Simulazione;
046700130305
046800130305       //?ciclo per tutti i codici di tassazione caricati dalla tariffa
046900130305         xc = 1;
047000130305         FOR xc by 1 to ultcts;
047100130305           wEnd = *off;
047200130305
047300130305       //?Leggo il file di work
047400130305           setll (SDSJob:cts(xc)) WFDBF01L;
047500130307           reade (SDSJob:cts(xc)) WFDBF01L;
047600130305
047700130307         //?ciclo fino a rottura di codice tassazione
047800130307           DOW  not %eof(WFDBF01L);
047900130307         //?elaboro solo le spedizioni calcolate correttamente
048000130307             IF  WFDerr = *blanks;
048100130307               exsr MEMspe;
048200130307             ENDIF;
048300130307         //?se richiesto stampo il dettaglio
048400130307             IF  pardet = 'S';
048500130307               exsr STAdet;
048600130307             ENDIF;
048700130307             reade (SDSJob:cts(xc)) WFDBF01L;
048800130307           ENDDO;
048900130305
049000130305         //?Stampo per codice di tassazione
049100130305           exsr STActs;
049200130305
049300130305         //?azzero le schiere dei calcoli
049400130305           clear Tnsp;
049500130305           clear Tpkg;
049600130305           clear Tncl;
049700130305           clear Tvlb;
049800130305           clear Tqft;
049900130305           clear Tric;
050000130305           clear Tcom;
050100130305
050200130305         ENDFOR;
050300130305
050400130305         //?Stampo il totale generale
050500130305         exsr STAgen;
050600130305
050700130305       ENDSR;
050800130305
050900130305       //--------------------------------------------------------------
051000130305       //?Memorizzo i dati per la stampa.
051100130305       //--------------------------------------------------------------
051200130305       BEGSR  MEMspe;
051300130305
051400130305         xi = 1;
051500130305         FOR xi by 1 to %elem(ssgl);
051600130306           IF  WFDsgl = ssgl(xi);
051700130305             IF  WFDcom > 0;
051800130305               Tnsp(xi) += 1;
051900130305               Gnsp(xi) += 1;
052000130305             ENDIF;
052100130305             Tpkg(xi) += WFDpkg;
052200130305             Gpkg(xi) += WFDpkg;
052300130305             Tncl(xi) += WFDncl;
052400130305             Gncl(xi) += WFDncl;
052500130305             Tvlb(xi) += WFDvlb;
052600130305             Gvlb(xi) += WFDvlb;
052700130305             Tqft(xi) += WFDqft;
052800130305             Gqft(xi) += WFDqft;
052900130306             Tric(xi) += WFDric;
053000130306             Gric(xi) += WFDric;
053100130306             Tcom(xi) += WFDcom;
053200130306             Gcom(xi) += WFDcom;
053300130305           ENDIF;
053400130305         ENDFOR;
053500130305
053600130305       ENDSR;
053700130306
053800130306       //--------------------------------------------------------------
053900130306       //?Stampa dettaglio.
054000130306       //--------------------------------------------------------------
054100130306       BEGSR  STAdet;
054200130306
054300130306       //?Decodifico codice tassazione
054400130306         clear stcts;
054500130306         clear stdcts;
054600130306         stcts = cts(xc);
054700130306         wCTS = cts(xc);
054800130306         clear wDES;
054900130306         exsr decoCTS;
055000130306         stdcts = wDES;
055100130306
055200130306       //?Giro la data della spedizione
055300130306         wData_ISO = %date(wfddsp:*iso);
055400130306         wData_EUR = wData_ISO;
055500130306         stpdsp = %dec(wData_EUR);
055600130306
055700130306       //?Se errore stampo l'errore al posto dei costi/ricavi
055800130306         *in51 = (WFDerr <> *blanks);
055900130306
056000130306       //?salto pagina
056100130306         IF  *in98;
056200130306           write TE601T01;
056300130306           *in98 = *off;
056400130306         ENDIF;
056500130306
056600130306       //?stampa riga
056700130306         write TE601D01;
056800130306
056900130306       ENDSR;
057000130306
057100130306       //--------------------------------------------------------------
057200130306       //?Stampa per codice tassazione.
057300130306       //--------------------------------------------------------------
057400130306       BEGSR  STActs;
057500130306
057600130306       //?Stampo una riga per scaglione
057700130306         xi = 1;
057800130307         FOR xi by 1 to ultsgl;
057900130306           stsgl = ssgl(xi);
058000130306           stnus = Tnsp(xi);
058100130306           stpkg = Tpkg(xi);
058200130306           stncl = Tncl(xi);
058300130306           stvlb = Tvlb(xi);
058400130306           stqft = Tqft(xi);
058500130306           stric = Tric(xi);
058600130306           stcom = Tcom(xi);
058700130306           exsr STArig;
058800130306         ENDFOR;
058900130306
059000130306       //?Stampa riga a totale codice tassazione
059100130306         clear stsgl;
059200130306         stnus = %xfoot(Tnsp);
059300130306         stpkg = %xfoot(Tpkg);
059400130306         stncl = %xfoot(Tncl);
059500130306         stvlb = %xfoot(Tvlb);
059600130306         stqft = %xfoot(Tqft);
059700130306         stric = %xfoot(Tric);
059800130306         stcom = %xfoot(Tcom);
059900130306         exsr STArig;
060000130306
060100130306       //?Stampa riga vuota
060200130306         write  TE60V01;
060300130306
060400130306       ENDSR;
060500130306
060600130306       //--------------------------------------------------------------
060700130306       //?Stampa totali generali.
060800130306       //--------------------------------------------------------------
060900130306       BEGSR  STAgen;
061000130306
061100130306       //?Imposto che stampa totali generali
061200130306         wTOT = *on;
061300130306
061400130306       //?Stampo una riga per scaglione
061500130306         xi = 1;
061600130307         FOR xi by 1 to ultsgl;
061700130306           stsgl = ssgl(xi);
061800130306           stnus = Gnsp(xi);
061900130306           stpkg = Gpkg(xi);
062000130306           stncl = Gncl(xi);
062100130306           stvlb = Gvlb(xi);
062200130306           stqft = Gqft(xi);
062300130306           stric = Gric(xi);
062400130306           stcom = Gcom(xi);
062500130306           exsr STArig;
062600130306         ENDFOR;
062700130306
062800130306       //?Stampa riga a totale
062900130306         clear stsgl;
063000130306         stnus = %xfoot(Gnsp);
063100130306         stpkg = %xfoot(Gpkg);
063200130306         stncl = %xfoot(Gncl);
063300130306         stvlb = %xfoot(Gvlb);
063400130306         stqft = %xfoot(Gqft);
063500130306         stric = %xfoot(Gric);
063600130306         stcom = %xfoot(Gcom);
063700130306         exsr STArig;
063800130306
063900130306       ENDSR;
064000130306
064100130306       //--------------------------------------------------------------
064200130306       //?Stampa la riga.
064300130306       //--------------------------------------------------------------
064400130306       BEGSR  STArig;
064500130306
064600130306         clear stcts;
064700130306         clear stdcts;
064800130306
064900130306       //?Solo per il primo scaglione
065000130306         IF  stsgl = ssgl(1);
065100130306
065200130306         //?Se sto stampando i totali generali imposto la scritta
065300130306         //?'TOTALE GENERALE' nella colonna del codice tassazione
065400130306           IF  wTOT;
065500130306             clear stcts;
065600130306             stdcts = 'TOTALE GENERALE';
065700130306           ENDIF;
065800130306
065900130306         //?Se sto stampando il dettaglio per scaglione
066000130306         //?Decodifico il codice tassazione
066100130306           IF  not wTOT;
066200130306             stcts = cts(xc);
066300130306             wCTS = cts(xc);
066400130306             clear wDES;
066500130306             exsr decoCTS;
066600130306             stdcts = wDES;
066700130306           ENDIF;
066800130306         ENDIF;
066900130306
067000130306       //?Stampo la riga dei totali per codice tassazione
067100130306       //?Al posto dello scaglione scrivo 'TOTALE'
067200130306         *in50 = (stsgl = 0);
067300130306
067400130306       //?Calcola la percentuale DELTA
067500130306         wGAP = (stric - stcom) * 100;
067600130306         IF  stric > 0;
067700130306           wGAP = wGAP / stric;
067800130306         ENDIF;
067900130306         eval(h) wdelta = wGAP;
068000130306
068100130306       //?se +/- 1000 imposto +/- 999,9
068200130306         SELECT;
068300130306           WHEN  wdelta >= 0 and wGAP >= 1000;
068400130306             stdel = +999,9;
068500130306           WHEN  wdelta < 0 and wGAP <= -1000;
068600130306             stdel = -999,9;
068700130306           OTHER;
068800130306             stdel = wdelta;
068900130306         ENDSL;
069000130306
069100130306       //?salto pagina
069200130306         IF  *in99;
069300130306           write TE60T01;
069400130306           *in99 = *off;
069500130306         ENDIF;
069600130306
069700130306       //?stampa riga
069800130306         write TE60D01;
069900130306
070000130306       ENDSR;
070100130306
070200130306       //--------------------------------------------------------------
070300130306       //?Decodifico Codice tassazione.
070400130306       //--------------------------------------------------------------
070500130306       BEGSR  decoCTS;
070600130306
070700130306         clear dsCT;
070800130306         k_TBLcod = 'CT';
070900130306         k_TBLkey = wCTS;
071000130306         chain  %kds( k03tabel00 )  TABEL;
071100130306         IF  %found(TABEL00F) and TBLflg = *blanks;
071200130306           dsCT = TBLuni;
071300130306         ENDIF;
071400130306         wDES = §CTdes;
071500130306
071600130306       ENDSR;
071700130305
071800120705       //--------------------------------------------------------------
071900130301       //?Elaborazione file riepilogo varie
072000120705       //--------------------------------------------------------------
072100130307       BEGSR  ElabVarie;
072200130305
072300130305         wEnd = *off;
072400130301
072500130301       //?Dichiarazione cursore
072600130301         exec sql
072700130301           DECLARE WFDBF cursor for
072800130301           SELECT * from WFDBF10F;
072900130301
073000130301       //?Apertura del cursore
073100130301         exec sql
073200130301           open WFDBF;
073300130301
073400130301         IF sqlcode < 0;
073500130301           wEnd = *on;
073600130301         ENDIF;
073700130301
073800130301         DOW not wEnd;
073900130301           exec sql
074000130301             fetch next from WFDBF into :WFDBF10F;
074100130301           IF  sqlcod = 100 or sqlcod < 0;
074200130301             wEnd = *on;
074300130301             leave;
074400130301           ENDIF;
074500120705
074600130301       //?Sommo le varie
074700130301           exsr varieWF;
074800130301
074900130301         ENDDO;
075000130301
075100130301         exec sql
075200130301           close WFDBF;
075300130307
075400130307         //?Stampa le varie
075500130307         exsr  StampaVarie;
075600120705
075700120705       ENDSR;
075800130301
075900130301       //--------------------------------------------------------------
076000130301       //?Varie del Work File in SK.                                   ?
076100130301       //--------------------------------------------------------------
076200130307       BEGSR  varieWF;
076300130307
076400130307       //?Imposto il porto nella prima voce sempre
076500130307         sk_Voce(1) = '.';
076600130307         IF  WF1por > 0;
076700130307           sk_Impo(1) += WF1por;
076800130307           sk_Sped(1) += 1;
076900130307         ENDIF;
077000130301
077100130307       //?poi schiero le varie
077200130301         IF  WF1sv1 <> *blanks;
077300130301           Wvaria = WF1sv1;
077400130301           Wimpo  = WF1va1;
077500130301           exsr  TOTvaria;
077600130301         ENDIF;
077700130301
077800130301         IF  WF1sv2 <> *blanks;
077900130301           Wvaria = WF1sv2;
078000130301           Wimpo  = WF1va2;
078100130301           exsr  TOTvaria;
078200130301         ENDIF;
078300130301
078400130301         IF  WF1sv3 <> *blanks;
078500130301           Wvaria = WF1sv3;
078600130301           Wimpo  = WF1va3;
078700130301           exsr  TOTvaria;
078800130301         ENDIF;
078900130301
079000130301         IF  WF1sv4 <> *blanks;
079100130301           Wvaria = WF1sv4;
079200130301           Wimpo  = WF1va4;
079300130301           exsr  TOTvaria;
079400130301         ENDIF;
079500130301
079600130301         IF  WF1sv5 <> *blanks;
079700130301           Wvaria = WF1sv5;
079800130301           Wimpo  = WF1va5;
079900130301           exsr  TOTvaria;
080000130301         ENDIF;
080100130301
080200130301         IF  WF1sv6 <> *blanks;
080300130301           Wvaria = WF1sv6;
080400130301           Wimpo  = WF1va6;
080500130301           exsr  TOTvaria;
080600130301         ENDIF;
080700130301
080800130301         IF  WF1sv7 <> *blanks;
080900130301           Wvaria = WF1sv7;
081000130301           Wimpo  = WF1va7;
081100130301           exsr  TOTvaria;
081200130301         ENDIF;
081300130301
081400130301         IF  WF1sv8 <> *blanks;
081500130301           Wvaria = WF1sv8;
081600130301           Wimpo  = WF1va8;
081700130301           exsr  TOTvaria;
081800130301         ENDIF;
081900130301
082000130301         IF  WF1sv9 <> *blanks;
082100130301           Wvaria = WF1sv9;
082200130301           Wimpo  = WF1va9;
082300130301           exsr  TOTvaria;
082400130301         ENDIF;
082500130301
082600130301         IF  WF1s10 <> *blanks;
082700130301           Wvaria = WF1s10;
082800130301           Wimpo  = WF1v10;
082900130301           exsr  TOTvaria;
083000130301         ENDIF;
083100130301
083200130301         IF  WF1s11 <> *blanks;
083300130301           Wvaria = WF1s11;
083400130301           Wimpo  = WF1v11;
083500130301           exsr  TOTvaria;
083600130301         ENDIF;
083700130301
083800130301         IF  WF1s12 <> *blanks;
083900130301           Wvaria = WF1s12;
084000130301           Wimpo  = WF1v12;
084100130301           exsr  TOTvaria;
084200130301         ENDIF;
084300130301
084400130301         IF  WF1s13 <> *blanks;
084500130301           Wvaria = WF1s13;
084600130301           Wimpo  = WF1v13;
084700130301           exsr  TOTvaria;
084800130301         ENDIF;
084900130301
085000130301         IF  WF1s14 <> *blanks;
085100130301           Wvaria = WF1s14;
085200130301           Wimpo  = WF1v14;
085300130301           exsr  TOTvaria;
085400130301         ENDIF;
085500130301
085600130301         IF  WF1s15 <> *blanks;
085700130301           Wvaria = WF1s15;
085800130301           Wimpo  = WF1v15;
085900130301           exsr  TOTvaria;
086000130301         ENDIF;
086100130301
086200130301         IF  WF1s16 <> *blanks;
086300130301           Wvaria = WF1s16;
086400130301           Wimpo  = WF1v16;
086500130301           exsr  TOTvaria;
086600130301         ENDIF;
086700130301
086800130301         IF  WF1s17 <> *blanks;
086900130301           Wvaria = WF1s17;
087000130301           Wimpo  = WF1v17;
087100130301           exsr  TOTvaria;
087200130301         ENDIF;
087300130301
087400130301         IF  WF1s18 <> *blanks;
087500130301           Wvaria = WF1s18;
087600130301           Wimpo  = WF1v18;
087700130301           exsr  TOTvaria;
087800130301         ENDIF;
087900130301
088000130301         IF  WF1s19 <> *blanks;
088100130301           Wvaria = WF1s19;
088200130301           Wimpo  = WF1v19;
088300130301           exsr  TOTvaria;
088400130301         ENDIF;
088500130301
088600130301         IF  WF1s20 <> *blanks;
088700130301           Wvaria = WF1s20;
088800130301           Wimpo  = WF1v20;
088900130301           exsr  TOTvaria;
089000130301         ENDIF;
089100130301
089200130301       ENDSR;
089300120705
089400120705       //--------------------------------------------------------------
089500120705       //?Incremento totali per voce in schiera.                       ?
089600120705       //--------------------------------------------------------------
089700130301       BEGSR  TOTvaria;
089800120705
089900130307         xx = %lookup(Wvaria : sk_Voce:2);
090000130301         IF  xx = *zero;
090100130307           xx = %lookup(*blank : sk_Voce:2);
090200130301           IF  xx > *zero;
090300120705             sk_Voce(xx) = Wvaria;
090400130301           ENDIF;
090500130301         ENDIF;
090600120705
090700120705         sk_Impo(xx) += Wimpo;
090800120705         sk_Sped(xx) += 1;
090900120705
091000120705       ENDSR;
091100120705
091200120705       //--------------------------------------------------------------
091300130307       //?Stampa le VARIE.
091400120705       //--------------------------------------------------------------
091500130307       BEGSR  StampaVarie;
091600120705
091700130301       //?Stampa testate?
091800130306         write  TE60T02;
091900130306         write  TE60T03;
092000120705
092100130301       //?Stampa delle varie di 2 in 2?
092200120705         xx = 1;
092300130307
092400130307       //?Se non c'è il porto (sempre in prima posizione) inizio a
092500130307       //?stampare dalla seconda varia
092600130307         IF  sk_Impo(1) = *zeros;
092700130307           xx = 2;
092800130307         ENDIF;
092900130307
093000130307         DOW  sk_Voce(xx) <> *blank;
093100120705
093200130301           wConta = 1;
093300130306           clear  TE60D02;
093400120705
093500130301           DOW  wConta <= 2  and  sk_Voce(xx) <> *blank;
093600130301             wVaria = sk_Voce(xx);
093700130307
093800130307             IF  wConta = 1;
093900130307             //?nella prima posizione c'è sempre il porto
094000130307               IF xx = 1;
094100130307                 P1Dsv1 = 'TRASPORTO';
094200130307               ELSE;
094300130307                 exsr  sr_TabCC;
094400130307                 P1Csv1 = sk_Voce(xx) + ' -';
094500130307                 P1Dsv1 = §CCdes;
094600130307               ENDIF;
094700130304               P1Cva1  = sk_Impo(xx);
094800130304               P1Cspe1 = sk_Sped(xx);
094900130304             ELSE;
095000130307               exsr  sr_TabCC;
095100130304               P1Csv2  = sk_Voce(xx) + ' -';
095200130304               P1Dsv2  = §CCdes;
095300130304               P1Cva2  = sk_Impo(xx);
095400130304               P1Cspe2 = sk_Sped(xx);
095500130304             ENDIF;
095600120705             xx += 1;
095700130301             wConta += 1;
095800130301           ENDDO;
095900120705
096000130306           write  TE60D02;
096100120705
096200130301         ENDDO;
096300130306
096400130306       //?Stampa riga finale
096500130306         write  TE60V02;
096600120705
096700120705       ENDSR;
096800120705
096900120705       //--------------------------------------------------------------
097000120705       //?Decodifica Sigla Varia.                                      ?
097100120705       //--------------------------------------------------------------
097200120705       BEGSR  sr_TabCC;
097300120705
097400120705         clear  dsCC;
097500120705         k_TBLcod = 'CC';
097600130301         k_TBLkey = 'VARIE  ' + wVaria;
097700120705
097800120705         chain  %kds( k03tabel00 )  TABEL;
097900120705
098000120705         if  %found(TABEL00F);
098100120705           dsCC = TBLuni;
098200120705         else;
098300120705           §CCdes = *all'?';
098400120705         endif;
098500120705
098600120705       ENDSR;
098700120705
098800120705       //--------------------------------------------------------------
098900130301       //?Routine Finale.
099000120705       //--------------------------------------------------------------
099100130301       BEGSR  RoutEnd;
099200120710
099300130301       //?Stampa "*** FINE STAMPA ***"?
099400130306         write  TE60E01;
099500130306
099600130306       //?Stampa "*** FINE STAMPA ***"?
099700130306       //?per dettaglio
099800130306         IF  pardet = 'S';
099900130306           write  TE601E01;
100000130306         ENDIF;
100100120705
100200130301       //?Chiusura funzioni precedentemente aperte?
100300120705         reset  TIBS69ds;
100400120705         tibs69r( tibs69ds :
100500120705                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
100600130301
100700130301         *inLR = *on;
100800120705
100900130301       //?Chiusura pgm?
101000120705         return;
101100120705
101200120705       ENDSR;
