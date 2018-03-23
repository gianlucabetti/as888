000100130301       //======================================================================
000200130304       //?TNTE60R - Stampa simulazione delta                            (batch)  ?
000300130301       //======================================================================
000400130502
000500130502       //--------------------------------------------------------------
000600130502       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130502       //--------------------------------------------------------------
000800130502
000900130502     /*END
001000120705
001100120705       //--------------------------------------------------------------
001200120705       //?Specifiche di controllo.                                     ?
001300120705       //--------------------------------------------------------------
001400120705
001500120705     h decedit('0,') datedit(*ymd.) option(*nodebugio)
001600120705
001700120705       //--------------------------------------------------------------
001800120705       //?Dichiarazione file.                                          ?
001900120705       //--------------------------------------------------------------
002000130306
002100130306       //?File organigramma
002200130306     fAZORG01L  if   e           k disk
002300130306
002400130306       //?File dettaglio simulazione delta
002500130306     fWFDBF01L  if   e           k disk
002600120705
002700130301       //?Tabelle?
002800120705     fTABEL00F  if   e           k disk
002900130304
003000130304       //?Tariffa/offerta
003100130304     fTNTAM01L  if   e           k disk
003200130304     fTITAD04L  if   e           k disk
003300130301
003400130301       //?Trattativa
003500130301     fTIVIS05L  if   e           k disk
003600120705
003700130301       //?File di stampa?
003800130306     fTNTE60P   o    e             printer oflind(*in99)
003900130305       //?File di stampa dettaglio?
004000130306     fTNTE60P1  o    e             printer oflind(*in98) usropn
004100120705
004200120705       //--------------------------------------------------------------
004300120705       //?Definizione costanti.                                        ?
004400120705       //--------------------------------------------------------------
004500120705
004600130301       //?Codice utente x TABEL00F?
004700120705     d c_Kut           c                   const(1)
004800120705
004900120705       //--------------------------------------------------------------
005000120705       //?Definizione schiere.                                         ?
005100120705       //--------------------------------------------------------------
005200120705
005300130301       //?Varia, importo e n° spedizioni x Singolo Cliente?
005400130301     d sk_Voce         s                   dim(99)  inz  like(WF1sv1)
005500120706     d sk_Impo         s             12  3 dim(99)  inz
005600120706     d sk_Sped         s              9  0 dim(99)  inz
005700130305
005800130305       //?Descrizione Cliente - linea di partenza
005900130305     d sk_des          s             30    dim(05)  inz
006000130304
006100130304       //?Codici tassazione
006200130304     d cts             s              2    dim(600) inz
006300130304
006400130304       //?Scaglioni
006500130304     d ssgl            s             13  3 dim(200) inz
006600130304
006700130304       //?Schiere totali
006800130305     d Tnsp            s              6  0 dim(200)                             n.Sped.
006900130305     d Gnsp            s              6  0 dim(200)                             n.Sped.
007000130305     d Tpkg            s              9  1 dim(200)                             Peso
007100130305     d Gpkg            s              9  1 dim(200)                             Peso
007200130305     d Tncl            s              7  0 dim(200)                             n.Colli
007300130305     d Gncl            s              7  0 dim(200)                             n.Colli
007400130305     d Tvlb            s              7  3 dim(200)                             Volume
007500130305     d Gvlb            s              7  3 dim(200)                             Volume
007600130305     d Tqft            s             15  3 dim(200)                             Qta Fatt.
007700130305     d Gqft            s             15  3 dim(200)                             Qta Fatt.
007800130305     d Tric            s             18  5 dim(200)                             Ricavi
007900130305     d Gric            s             18  5 dim(200)                             Ricavi
008000130305     d Tcom            s             18  5 dim(200)                             Compet.
008100130305     d Gcom            s             18  5 dim(200)                             Compet.
008200120705
008300120705       //--------------------------------------------------------------
008400120705       //?Definizione aree dati.                                       ?
008500120705       //--------------------------------------------------------------
008600120705
008700130301       //?Dati utente?
008800120705     d §AzUte        e ds                  extname(AZUTE00F)
008900120705     d                                     dtaara
009000120705     d §DatiUte      e ds                  extname(dDatiUte)
009100120705     d                                     dtaara
009200120705
009300120705       //--------------------------------------------------------------
009400120705       //?Definizione strutture dati.                                  ?
009500120705       //--------------------------------------------------------------
009600120705
009700130301       //?Parametri ricevuti?
009800120705     d KPJBA         e ds
009900130301
010000130301       //?Parametri per il pgm chiamato
010100130301     d param           ds                  inz
010200130301     d  parks1                 1      7  0
010300130301     d  parks2                 8     14  0
010400130301     d  parks3                15     21  0
010500130301     d  parks4                22     28  0
010600130301     d  parks5                29     35  0
010700130305     d  sk_ksc                 1     35  0 dim(5)
010800130301     d  parta1                36     38  0
010900130301     d  parta2                39     41  0
011000130301     d  parta3                42     44  0
011100130301     d  parta4                45     47  0
011200130301     d  parta5                48     50  0
011300130301     d  parln1                51     53  0
011400130301     d  parln2                54     56  0
011500130301     d  parln3                57     59  0
011600130301     d  parln4                60     62  0
011700130301     d  parln5                63     65  0
011800130305     d  sk_lnp                51     65  0 dim(5)
011900130301     d  pards1                66     73  0
012000130301     d  pards2                74     81  0
012100130301     d  parct1                82     83
012200130301     d  parct2                84     85
012300130301     d  parct3                86     87
012400130301     d  parct4                88     89
012500130301     d  parct5                90     91
012600130301     d  parct6                92     93
012700130301     d  parct7                94     95
012800130301     d  parct8                96     97
012900130301     d  parct9                98     99
013000130301     d  parct10              100    101
013100130301     d  parkgd               102    108  1
013200130301     d  parkga               109    115  1
013300130301     d  parcod               116    121  0
013400130301     d  parcoa               122    127  0
013500130301     d  partaof              128    128
013600130301     d  parksc               129    135  0
013700130301     d  parctr               136    138  0
013800130301     d  parprg               139    141  0
013900130301     d  parlnp               142    144  0
014000130301     d  pardtcal             145    145
014100130301     d  pardet               146    146
014200130301     d  parnrv               147    153  0
014300130301     d  parddt               154    161  0
014400130306
014500130306       //?Data Spedizione
014600130306     d                 ds
014700130306     d  WFDaas                 1      4  0
014800130306     d  WFDmgs                 5      8  0
014900130306     d  wfddsp                 1      8  0
015000120705
015100130306       //?Tabella "CC" = Codici Varie
015200120705     d dsCC          e ds                  inz
015300130306
015400130306       //?Tabella "CT" = Codici Tassazione?
015500130306     d dsCT          e ds                  inz
015600130301
015700130301       //?File Work Varie
015800130301     d WFDBF10F      e ds                  extname(WFDBF10F)
015900120705
016000130301       //?Status ds?
016100120705     d Status         sds
016200120705     d   SDSpgm          *proc
016300130305     d   SDSJob              244    253
016400120705
016500120705       //--------------------------------------------------------------
016600120705       //?Definizione variabili globali.                               ?
016700120705       //--------------------------------------------------------------
016800130301
016900130301       //?Flag booleani
017000130301     d wEnd            s               n   inz(*off)
017100130304     d wTAD            s               n   inz(*off)
017200130306     d wTOT            s               n   inz(*off)
017300120705
017400130301       //?Indici di schiera?
017500130304     d xi              s              4  0 inz
017600130304     d xc              s              4  0 inz
017700120705     d xx              s              3  0 inz
017800120705     d yy              s              3  0 inz
017900130305     d ww              s              3  0 inz
018000130305
018100130305       // -?Campi Data
018200130305     d wData_ISO       s               d   datfmt(*iso)
018300130305     d wData_EUR       s               d   datfmt(*eur)
018400120705
018500130301       //?Campi di comodo?
018600130304     d ultSGL          s              6  0 inz
018700130304     d ultCTS          s              6  0 inz
018800130301     d wConta          s              1  0 inz
018900130306     d wCTS            s              2    inz
019000130306     d wdelta          s              4  1 inz
019100130305     d wDES            s             30    inz
019200130306     d wGAP            s             18  5 inz
019300130305     d wImpo           s                   like(WF1va1)   inz
019400130305     d wKSC            s              7  0 inz
019500130305     d wLNP            s              3  0 inz
019600130305     d wVaria          s                   like(WF1sv1)   inz
019700120705
019800120705       //--------------------------------------------------------------
019900120705       //?Definizione prototipi procedure.                             ?
020000120705       //--------------------------------------------------------------
020100120705
020200130301       //?Reperimento dati utente?
020300120705     d TIBS34ds      e ds                  inz
020400120705      /copy gaitrasrc/srcProtoPR,TIBS34R
020500120705
020600130301       //?Controllo/Decodifica cliente?
020700120705      /copy gaitrasrc/srcProtoPI,TIBS69R
020800120705      /copy gaitrasrc/srcProtoPR,TIBS69R
020900120705
021000130301       //?Controllo formale / Inversione data?
021100120705     d WLBdat          ds                  inz
021200120705     d   G08dat                1      8  0 inz
021300120705     d   G08inv                9     16  0 inz
021400120705     d   G08err               17     17    inz('3')
021500120705     d   G08tgi               18     22  0 inz
021600120705      /copy gaitrasrc/srcProtoPR,XSRDA8
021700120705
021800120705       //--------------------------------------------------------------
021900120705       //?Definizione key-list.                                        ?
022000120705       //--------------------------------------------------------------
022100120705
022200130301       //?File TABEL00F?
022300120705     d k03tabel00    e ds                  extname(TABEL00F : *key)
022400120705     d                                     prefix(k_)   inz
022500120705
022600120705       //--------------------------------------------------------------
022700120705       //?M A I N - L I N E                                            ?
022800120705       //--------------------------------------------------------------
022900120705
023000120705     c     *Entry        plist
023100120705     c                   parm                    KPJBA
023200120705
023300120705      /free
023400120705
023500130301       //?Operazioni iniziali?
023600130301       exsr  RoutInz;
023700120705
023800130304       //?Carico scaglioni
023900130304       exsr  Scaglioni;
024000130304
024100130304       //?Stampa la pagina dei parametri richiesi a video
024200130304       exsr  Parametri;
024300130305
024400130305       //?Elaboro per ogni cod. tassazione della tariffa/offerta
024500130306       exsr  Simulazione;
024600130304
024700130301       //?Elaborazione file riepilogo varie?
024800130307       exsr  ElabVarie;
024900120705
025000120705
025100130301       //?Operazioni finali?
025200130301       exsr  RoutEnd;
025300120705
025400120705       //--------------------------------------------------------------
025500120705       //?Operazioni iniziali.                                         ?
025600120705       //--------------------------------------------------------------
025700130301       BEGSR  RoutInz;
025800130301
025900130301         exec sql  set option  dynusrprf = *owner,
026000130301                               closqlcsr = *endmod;
026100120705
026200130301       //?Impostazione parametri ricevuti?
026300130301         param = kpjbu;
026400120705
026500130301       //?Reperimento dati job?
026600130301         exsr  DatiJob;
026700120705
026800130301       //?Impostazione campi fissi?
026900120705         clear  k03tabel00;
027000120705         k_TBLkut = c_Kut;
027100120705
027200130301       //?Impostazione campi in testata?
027300120705         P1Trsu = RsUt;
027400120705         P1Tpgm = SDSpgm;
027500130306
027600130306       //?Se richiesto apro file di stampa per dettaglio
027700130306         IF  pardet = 'S';
027800130306           open TNTE60P1;
027900130306         ENDIF;
028000120705
028100120705       ENDSR;
028200120705
028300120705       //--------------------------------------------------------------
028400120705       //?Reperimento Dati del job (Utente/Operativi).                 ?
028500120705       //--------------------------------------------------------------
028600130301       BEGSR  DatiJob;
028700120705
028800120705         in(E) §AzUte;
028900120705         if NOT %error;
029000120705           in(E) §DatiUte;
029100120705         endif;
029200120705         if %error or RSut = *blanks;
029300120705           clear TIBS34ds;
029400120705           tibs34r ( tibs34ds );
029500120705           in §AzUte;
029600120705           in §DatiUte;
029700120705         endif;
029800120705
029900120705       ENDSR;
030000130304
030100130304       //--------------------------------------------------------------
030200130304       //?Carico scaglioni.
030300130304       //--------------------------------------------------------------
030400130304       BEGSR  Scaglioni;
030500130304
030600130304         clear xi;
030700130304         clear xc;
030800130304         wTAD = *off;
030900130304
031000130304         IF  parlnp = 0;
031100130304           setll (parksc:parctr:parprg) TITAD04L;
031200130304           reade (parksc:parctr:parprg) TITAD04L;
031300130304         ELSE;
031400130304           setll (parksc:parctr:parprg:parlnp) TITAD04L;
031500130304           reade (parksc:parctr:parprg:parlnp) TITAD04L;
031600130304         ENDIF;
031700130304         DOW  not %eof(TITAD04L);
031800130304           wTAD = *on;
031900130304           exsr memTAD;
032000130304
032100130304           IF  parlnp = 0;
032200130304             reade (parksc:parctr:parprg) TITAD04L;
032300130304           ELSE;
032400130304             reade (parksc:parctr:parprg:parlnp) TITAD04L;
032500130304           ENDIF;
032600130304         ENDDO;
032700130304
032800130304         ultSGL = xi;
032900130306         ultcts = xc;
033000130304
033100130304         IF  not wTAD;
033200130304           exsr RoutEnd;
033300130304         ENDIF;
033400130304
033500130304       ENDSR;
033600130304
033700130304       //--------------------------------------------------------------
033800130304       //?Memorizza gli scaglioni.
033900130304       //--------------------------------------------------------------
034000130304       BEGSR  memTAD;
034100130304
034200130305         IF  %lookup(TADsgl:ssgl) = 0;
034300130305           xi += 1;
034400130305           ssgl(xi) = TADsgl;
034500130305         ENDIF;
034600130305
034700130305         IF  %lookup(TADcts:cts) = 0;
034800130305           xc += 1;
034900130307           cts(xc) = TADcts;
035000130305         ENDIF;
035100130304
035200130304       ENDSR;
035300130304
035400130304       //--------------------------------------------------------------
035500130304       //?Stampa pagina parametri.
035600130304       //--------------------------------------------------------------
035700130304       BEGSR  Parametri;
035800130304
035900130305       //?Decodifico i clienti richiesti
036000130305         ww = 1;
036100130305         FOR ww by 1 to 5;
036200130305           IF  sk_ksc(ww) <> *zeros;
036300130305             wKSC = sk_ksc(ww);
036400130305             clear wDES;
036500130305             exsr decoKSC;
036600130305             sk_des(ww) = wDES;
036700130305           ENDIF;
036800130305         ENDFOR;
036900130305
037000130305         desksc01 = sk_des(01);
037100130305         desksc02 = sk_des(02);
037200130305         desksc03 = sk_des(03);
037300130305         desksc04 = sk_des(04);
037400130305         desksc05 = sk_des(05);
037500130305
037600130305       //?Decodifico le Linee di partenza richieste
037700130305         ww = 1;
037800130306         clear sk_des;
037900130305         FOR ww by 1 to 5;
038000130305           IF  sk_lnp(ww) <> *zeros and sk_lnp(ww) <> 999;
038100130305             wLNP = sk_lnp(ww);
038200130305             clear wDES;
038300130305             exsr decoLNP;
038400130306             sk_des(ww) = wDES;
038500130305           ENDIF;
038600130305         ENDFOR;
038700130305
038800130305         deslnp01 = sk_des(01);
038900130305         deslnp02 = sk_des(02);
039000130305         deslnp03 = sk_des(03);
039100130305         deslnp04 = sk_des(04);
039200130305         deslnp05 = sk_des(05);
039300130305
039400130305       //?Giro le date per la stampa
039500130305         wData_ISO = %date(pards1:*iso);
039600130305         wData_EUR = wData_ISO;
039700130305         p1dtsda = %dec(wData_EUR);
039800130305         wData_ISO = %date(pards2:*iso);
039900130305         wData_EUR = wData_ISO;
040000130305         p1dtsal = %dec(wData_EUR);
040100130305
040200130305       //?Decodifico Cliente/Offerta
040300130305         IF  partaof = 'T';
040400130305           clear wDES;
040500130305           wksc = parksc;
040600130305           exsr decoKSC;
040700130305           destar = wDES;
040800130305           stptar = parksc;
040900130305         ELSE;
041000130305           stptar = parnrv;
041100130305           chain parnrv TIVIS05L;
041200130305           IF  %found(TIVIS05L);
041300130305             destar = VISrag;
041400130305           ENDIF;
041500130305         ENDIF;
041600130305
041700130305       //?Decodifico le Linea di partenza tariffa
041800130305         clear wDES;
041900130305         wLNP = parlnp;
042000130305         exsr decoLNP;
042100130305         deslnp = wDES;
042200130305
042300130305       //?Data calcolo
042400130305         IF  pardtcal = 'O';
042500130305           desdtcal = 'OGGI';
042600130305         ELSE;
042700130305           desdtcal = 'DATA SPEDIZIONE';
042800130305         ENDIF;
042900130305
043000130305       //?Stampo la pagina dei parametri di lancio
043100130305         write TE60PAR;
043200130306
043300130306       //?Stampo la testata
043400130306         write TE60T01;
043500130306
043600130306       //?Se richiesta stampo la testata del dettaglio
043700130306         IF  pardet = 'S';
043800130306           write TE601T01;
043900130306         ENDIF;
044000130304
044100130304       ENDSR;
044200130305
044300130305       //--------------------------------------------------------------
044400130305       //?Decodifico Cliente.
044500130305       //--------------------------------------------------------------
044600130305       BEGSR  decoKSC;
044700130305
044800130305         clear tibs69ds;
044900130305         I69kac = %dec(wKSC:7:0);
045000130305         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
045100130305         IF  O69err = *blanks;
045200130305           wDES = ACOrag;
045300130305         ENDIF;
045400120705
045500130305       ENDSR;
045600130305
045700130305       //--------------------------------------------------------------
045800130305       //?Decodifico Linea di Partenza.
045900130305       //--------------------------------------------------------------
046000130306       BEGSR  decoLNP;
046100130305
046200130305         chain wLNP AZORG01L;
046300130305         IF  %found(AZORG01L) and ORGFVA = *blanks and ORGfag <> 'V';
046400130305           wDES = ORGdes;
046500130305         ENDIF;
046600130305
046700130305       ENDSR;
046800130305
046900130305       //--------------------------------------------------------------
047000130305       //?Elaborazione della simulazione delta.
047100130305       //--------------------------------------------------------------
047200130305       BEGSR  Simulazione;
047300130305
047400130305       //?ciclo per tutti i codici di tassazione caricati dalla tariffa
047500130305         xc = 1;
047600130305         FOR xc by 1 to ultcts;
047700130305           wEnd = *off;
047800130305
047900130305       //?Leggo il file di work
048000130305           setll (SDSJob:cts(xc)) WFDBF01L;
048100130307           reade (SDSJob:cts(xc)) WFDBF01L;
048200130305
048300130307         //?ciclo fino a rottura di codice tassazione
048400130307           DOW  not %eof(WFDBF01L);
048500130307         //?elaboro solo le spedizioni calcolate correttamente
048600130307             IF  WFDerr = *blanks;
048700130307               exsr MEMspe;
048800130307             ENDIF;
048900130307         //?se richiesto stampo il dettaglio
049000130307             IF  pardet = 'S';
049100130307               exsr STAdet;
049200130307             ENDIF;
049300130307             reade (SDSJob:cts(xc)) WFDBF01L;
049400130307           ENDDO;
049500130305
049600130305         //?Stampo per codice di tassazione
049700130305           exsr STActs;
049800130305
049900130305         //?azzero le schiere dei calcoli
050000130305           clear Tnsp;
050100130305           clear Tpkg;
050200130305           clear Tncl;
050300130305           clear Tvlb;
050400130305           clear Tqft;
050500130305           clear Tric;
050600130305           clear Tcom;
050700130305
050800130305         ENDFOR;
050900130305
051000130305         //?Stampo il totale generale
051100130305         exsr STAgen;
051200130305
051300130305       ENDSR;
051400130305
051500130305       //--------------------------------------------------------------
051600130305       //?Memorizzo i dati per la stampa.
051700130305       //--------------------------------------------------------------
051800130305       BEGSR  MEMspe;
051900130305
052000130305         xi = 1;
052100130305         FOR xi by 1 to %elem(ssgl);
052200130306           IF  WFDsgl = ssgl(xi);
052300130305             IF  WFDcom > 0;
052400130305               Tnsp(xi) += 1;
052500130305               Gnsp(xi) += 1;
052600130305             ENDIF;
052700130305             Tpkg(xi) += WFDpkg;
052800130305             Gpkg(xi) += WFDpkg;
052900130305             Tncl(xi) += WFDncl;
053000130305             Gncl(xi) += WFDncl;
053100130305             Tvlb(xi) += WFDvlb;
053200130305             Gvlb(xi) += WFDvlb;
053300130305             Tqft(xi) += WFDqft;
053400130305             Gqft(xi) += WFDqft;
053500130306             Tric(xi) += WFDric;
053600130306             Gric(xi) += WFDric;
053700130306             Tcom(xi) += WFDcom;
053800130306             Gcom(xi) += WFDcom;
053900130305           ENDIF;
054000130305         ENDFOR;
054100130305
054200130305       ENDSR;
054300130306
054400130306       //--------------------------------------------------------------
054500130306       //?Stampa dettaglio.
054600130306       //--------------------------------------------------------------
054700130306       BEGSR  STAdet;
054800130306
054900130306       //?Decodifico codice tassazione
055000130306         clear stcts;
055100130306         clear stdcts;
055200130306         stcts = cts(xc);
055300130306         wCTS = cts(xc);
055400130306         clear wDES;
055500130306         exsr decoCTS;
055600130306         stdcts = wDES;
055700130306
055800130306       //?Giro la data della spedizione
055900130306         wData_ISO = %date(wfddsp:*iso);
056000130306         wData_EUR = wData_ISO;
056100130306         stpdsp = %dec(wData_EUR);
056200130306
056300130306       //?Se errore stampo l'errore al posto dei costi/ricavi
056400130306         *in51 = (WFDerr <> *blanks);
056500130306
056600130306       //?salto pagina
056700130306         IF  *in98;
056800130306           write TE601T01;
056900130306           *in98 = *off;
057000130306         ENDIF;
057100130306
057200130306       //?stampa riga
057300130306         write TE601D01;
057400130306
057500130306       ENDSR;
057600130306
057700130306       //--------------------------------------------------------------
057800130306       //?Stampa per codice tassazione.
057900130306       //--------------------------------------------------------------
058000130306       BEGSR  STActs;
058100130306
058200130306       //?Stampo una riga per scaglione
058300130306         xi = 1;
058400130307         FOR xi by 1 to ultsgl;
058500130306           stsgl = ssgl(xi);
058600130306           stnus = Tnsp(xi);
058700130306           stpkg = Tpkg(xi);
058800130306           stncl = Tncl(xi);
058900130306           stvlb = Tvlb(xi);
059000130306           stqft = Tqft(xi);
059100130306           stric = Tric(xi);
059200130306           stcom = Tcom(xi);
059300130306           exsr STArig;
059400130306         ENDFOR;
059500130306
059600130306       //?Stampa riga a totale codice tassazione
059700130306         clear stsgl;
059800130306         stnus = %xfoot(Tnsp);
059900130306         stpkg = %xfoot(Tpkg);
060000130306         stncl = %xfoot(Tncl);
060100130306         stvlb = %xfoot(Tvlb);
060200130306         stqft = %xfoot(Tqft);
060300130306         stric = %xfoot(Tric);
060400130306         stcom = %xfoot(Tcom);
060500130306         exsr STArig;
060600130306
060700130306       //?Stampa riga vuota
060800130306         write  TE60V01;
060900130306
061000130306       ENDSR;
061100130306
061200130306       //--------------------------------------------------------------
061300130306       //?Stampa totali generali.
061400130306       //--------------------------------------------------------------
061500130306       BEGSR  STAgen;
061600130306
061700130306       //?Imposto che stampa totali generali
061800130306         wTOT = *on;
061900130306
062000130306       //?Stampo una riga per scaglione
062100130306         xi = 1;
062200130307         FOR xi by 1 to ultsgl;
062300130306           stsgl = ssgl(xi);
062400130306           stnus = Gnsp(xi);
062500130306           stpkg = Gpkg(xi);
062600130306           stncl = Gncl(xi);
062700130306           stvlb = Gvlb(xi);
062800130306           stqft = Gqft(xi);
062900130306           stric = Gric(xi);
063000130306           stcom = Gcom(xi);
063100130306           exsr STArig;
063200130306         ENDFOR;
063300130306
063400130306       //?Stampa riga a totale
063500130306         clear stsgl;
063600130306         stnus = %xfoot(Gnsp);
063700130306         stpkg = %xfoot(Gpkg);
063800130306         stncl = %xfoot(Gncl);
063900130306         stvlb = %xfoot(Gvlb);
064000130306         stqft = %xfoot(Gqft);
064100130306         stric = %xfoot(Gric);
064200130306         stcom = %xfoot(Gcom);
064300130306         exsr STArig;
064400130306
064500130306       ENDSR;
064600130306
064700130306       //--------------------------------------------------------------
064800130306       //?Stampa la riga.
064900130306       //--------------------------------------------------------------
065000130306       BEGSR  STArig;
065100130306
065200130306         clear stcts;
065300130306         clear stdcts;
065400130306
065500130306       //?Solo per il primo scaglione
065600130306         IF  stsgl = ssgl(1);
065700130306
065800130306         //?Se sto stampando i totali generali imposto la scritta
065900130306         //?'TOTALE GENERALE' nella colonna del codice tassazione
066000130306           IF  wTOT;
066100130306             clear stcts;
066200130306             stdcts = 'TOTALE GENERALE';
066300130306           ENDIF;
066400130306
066500130306         //?Se sto stampando il dettaglio per scaglione
066600130306         //?Decodifico il codice tassazione
066700130306           IF  not wTOT;
066800130306             stcts = cts(xc);
066900130306             wCTS = cts(xc);
067000130306             clear wDES;
067100130306             exsr decoCTS;
067200130306             stdcts = wDES;
067300130306           ENDIF;
067400130306         ENDIF;
067500130306
067600130306       //?Stampo la riga dei totali per codice tassazione
067700130306       //?Al posto dello scaglione scrivo 'TOTALE'
067800130306         *in50 = (stsgl = 0);
067900130306
068000130306       //?Calcola la percentuale DELTA
068100130306         wGAP = (stric - stcom) * 100;
068200130306         IF  stric > 0;
068300130306           wGAP = wGAP / stric;
068400130306         ENDIF;
068500130502         //eval(h) wdelta = wGAP;   //?Va in errore se wGAP > 999,9 o < -999,9?
068600130502         select;
068700130502           when  wGAP >= 1000;
068800130502             wDelta = +999,9;
068900130502           when  wGAP <= -1000;
069000130502             wDelta = -999,9;
069100130502           other;
069200130502             eval(h) wDelta = wGAP;
069300130502         ENDSL;
069400130306
069500130306       //?se +/- 1000 imposto +/- 999,9
069600130306         SELECT;
069700130306           WHEN  wdelta >= 0 and wGAP >= 1000;
069800130306             stdel = +999,9;
069900130306           WHEN  wdelta < 0 and wGAP <= -1000;
070000130306             stdel = -999,9;
070100130306           OTHER;
070200130306             stdel = wdelta;
070300130306         ENDSL;
070400130306
070500130306       //?salto pagina
070600130306         IF  *in99;
070700130306           write TE60T01;
070800130306           *in99 = *off;
070900130306         ENDIF;
071000130306
071100130306       //?stampa riga
071200130306         write TE60D01;
071300130306
071400130306       ENDSR;
071500130306
071600130306       //--------------------------------------------------------------
071700130306       //?Decodifico Codice tassazione.
071800130306       //--------------------------------------------------------------
071900130306       BEGSR  decoCTS;
072000130306
072100130306         clear dsCT;
072200130306         k_TBLcod = 'CT';
072300130306         k_TBLkey = wCTS;
072400130306         chain  %kds( k03tabel00 )  TABEL;
072500130306         IF  %found(TABEL00F) and TBLflg = *blanks;
072600130306           dsCT = TBLuni;
072700130306         ENDIF;
072800130306         wDES = §CTdes;
072900130306
073000130306       ENDSR;
073100130305
073200120705       //--------------------------------------------------------------
073300130301       //?Elaborazione file riepilogo varie
073400120705       //--------------------------------------------------------------
073500130307       BEGSR  ElabVarie;
073600130305
073700130305         wEnd = *off;
073800130301
073900130301       //?Dichiarazione cursore
074000130301         exec sql
074100130301           DECLARE WFDBF cursor for
074200130301           SELECT * from WFDBF10F;
074300130301
074400130301       //?Apertura del cursore
074500130301         exec sql
074600130301           open WFDBF;
074700130301
074800130301         IF sqlcode < 0;
074900130301           wEnd = *on;
075000130301         ENDIF;
075100130301
075200130301         DOW not wEnd;
075300130301           exec sql
075400130301             fetch next from WFDBF into :WFDBF10F;
075500130301           IF  sqlcod = 100 or sqlcod < 0;
075600130301             wEnd = *on;
075700130301             leave;
075800130301           ENDIF;
075900120705
076000130301       //?Sommo le varie
076100130301           exsr varieWF;
076200130301
076300130301         ENDDO;
076400130301
076500130301         exec sql
076600130301           close WFDBF;
076700130307
076800130307         //?Stampa le varie
076900130307         exsr  StampaVarie;
077000120705
077100120705       ENDSR;
077200130301
077300130301       //--------------------------------------------------------------
077400130301       //?Varie del Work File in SK.                                   ?
077500130301       //--------------------------------------------------------------
077600130307       BEGSR  varieWF;
077700130307
077800130307       //?Imposto il porto nella prima voce sempre
077900130307         sk_Voce(1) = '.';
078000130307         IF  WF1por > 0;
078100130307           sk_Impo(1) += WF1por;
078200130307           sk_Sped(1) += 1;
078300130307         ENDIF;
078400130301
078500130307       //?poi schiero le varie
078600130301         IF  WF1sv1 <> *blanks;
078700130301           Wvaria = WF1sv1;
078800130301           Wimpo  = WF1va1;
078900130301           exsr  TOTvaria;
079000130301         ENDIF;
079100130301
079200130301         IF  WF1sv2 <> *blanks;
079300130301           Wvaria = WF1sv2;
079400130301           Wimpo  = WF1va2;
079500130301           exsr  TOTvaria;
079600130301         ENDIF;
079700130301
079800130301         IF  WF1sv3 <> *blanks;
079900130301           Wvaria = WF1sv3;
080000130301           Wimpo  = WF1va3;
080100130301           exsr  TOTvaria;
080200130301         ENDIF;
080300130301
080400130301         IF  WF1sv4 <> *blanks;
080500130301           Wvaria = WF1sv4;
080600130301           Wimpo  = WF1va4;
080700130301           exsr  TOTvaria;
080800130301         ENDIF;
080900130301
081000130301         IF  WF1sv5 <> *blanks;
081100130301           Wvaria = WF1sv5;
081200130301           Wimpo  = WF1va5;
081300130301           exsr  TOTvaria;
081400130301         ENDIF;
081500130301
081600130301         IF  WF1sv6 <> *blanks;
081700130301           Wvaria = WF1sv6;
081800130301           Wimpo  = WF1va6;
081900130301           exsr  TOTvaria;
082000130301         ENDIF;
082100130301
082200130301         IF  WF1sv7 <> *blanks;
082300130301           Wvaria = WF1sv7;
082400130301           Wimpo  = WF1va7;
082500130301           exsr  TOTvaria;
082600130301         ENDIF;
082700130301
082800130301         IF  WF1sv8 <> *blanks;
082900130301           Wvaria = WF1sv8;
083000130301           Wimpo  = WF1va8;
083100130301           exsr  TOTvaria;
083200130301         ENDIF;
083300130301
083400130301         IF  WF1sv9 <> *blanks;
083500130301           Wvaria = WF1sv9;
083600130301           Wimpo  = WF1va9;
083700130301           exsr  TOTvaria;
083800130301         ENDIF;
083900130301
084000130301         IF  WF1s10 <> *blanks;
084100130301           Wvaria = WF1s10;
084200130301           Wimpo  = WF1v10;
084300130301           exsr  TOTvaria;
084400130301         ENDIF;
084500130301
084600130301         IF  WF1s11 <> *blanks;
084700130301           Wvaria = WF1s11;
084800130301           Wimpo  = WF1v11;
084900130301           exsr  TOTvaria;
085000130301         ENDIF;
085100130301
085200130301         IF  WF1s12 <> *blanks;
085300130301           Wvaria = WF1s12;
085400130301           Wimpo  = WF1v12;
085500130301           exsr  TOTvaria;
085600130301         ENDIF;
085700130301
085800130301         IF  WF1s13 <> *blanks;
085900130301           Wvaria = WF1s13;
086000130301           Wimpo  = WF1v13;
086100130301           exsr  TOTvaria;
086200130301         ENDIF;
086300130301
086400130301         IF  WF1s14 <> *blanks;
086500130301           Wvaria = WF1s14;
086600130301           Wimpo  = WF1v14;
086700130301           exsr  TOTvaria;
086800130301         ENDIF;
086900130301
087000130301         IF  WF1s15 <> *blanks;
087100130301           Wvaria = WF1s15;
087200130301           Wimpo  = WF1v15;
087300130301           exsr  TOTvaria;
087400130301         ENDIF;
087500130301
087600130301         IF  WF1s16 <> *blanks;
087700130301           Wvaria = WF1s16;
087800130301           Wimpo  = WF1v16;
087900130301           exsr  TOTvaria;
088000130301         ENDIF;
088100130301
088200130301         IF  WF1s17 <> *blanks;
088300130301           Wvaria = WF1s17;
088400130301           Wimpo  = WF1v17;
088500130301           exsr  TOTvaria;
088600130301         ENDIF;
088700130301
088800130301         IF  WF1s18 <> *blanks;
088900130301           Wvaria = WF1s18;
089000130301           Wimpo  = WF1v18;
089100130301           exsr  TOTvaria;
089200130301         ENDIF;
089300130301
089400130301         IF  WF1s19 <> *blanks;
089500130301           Wvaria = WF1s19;
089600130301           Wimpo  = WF1v19;
089700130301           exsr  TOTvaria;
089800130301         ENDIF;
089900130301
090000130301         IF  WF1s20 <> *blanks;
090100130301           Wvaria = WF1s20;
090200130301           Wimpo  = WF1v20;
090300130301           exsr  TOTvaria;
090400130301         ENDIF;
090500130301
090600130301       ENDSR;
090700120705
090800120705       //--------------------------------------------------------------
090900120705       //?Incremento totali per voce in schiera.                       ?
091000120705       //--------------------------------------------------------------
091100130301       BEGSR  TOTvaria;
091200120705
091300130307         xx = %lookup(Wvaria : sk_Voce:2);
091400130301         IF  xx = *zero;
091500130307           xx = %lookup(*blank : sk_Voce:2);
091600130301           IF  xx > *zero;
091700120705             sk_Voce(xx) = Wvaria;
091800130301           ENDIF;
091900130301         ENDIF;
092000120705
092100120705         sk_Impo(xx) += Wimpo;
092200120705         sk_Sped(xx) += 1;
092300120705
092400120705       ENDSR;
092500120705
092600120705       //--------------------------------------------------------------
092700130307       //?Stampa le VARIE.
092800120705       //--------------------------------------------------------------
092900130307       BEGSR  StampaVarie;
093000120705
093100130301       //?Stampa testate?
093200130306         write  TE60T02;
093300130306         write  TE60T03;
093400120705
093500130301       //?Stampa delle varie di 2 in 2?
093600120705         xx = 1;
093700130307
093800130307       //?Se non c'è il porto (sempre in prima posizione) inizio a
093900130307       //?stampare dalla seconda varia
094000130307         IF  sk_Impo(1) = *zeros;
094100130307           xx = 2;
094200130307         ENDIF;
094300130307
094400130307         DOW  sk_Voce(xx) <> *blank;
094500120705
094600130301           wConta = 1;
094700130306           clear  TE60D02;
094800120705
094900130301           DOW  wConta <= 2  and  sk_Voce(xx) <> *blank;
095000130301             wVaria = sk_Voce(xx);
095100130307
095200130307             IF  wConta = 1;
095300130307             //?nella prima posizione c'è sempre il porto
095400130307               IF xx = 1;
095500130307                 P1Dsv1 = 'TRASPORTO';
095600130307               ELSE;
095700130307                 exsr  sr_TabCC;
095800130307                 P1Csv1 = sk_Voce(xx) + ' -';
095900130307                 P1Dsv1 = §CCdes;
096000130307               ENDIF;
096100130304               P1Cva1  = sk_Impo(xx);
096200130304               P1Cspe1 = sk_Sped(xx);
096300130304             ELSE;
096400130307               exsr  sr_TabCC;
096500130304               P1Csv2  = sk_Voce(xx) + ' -';
096600130304               P1Dsv2  = §CCdes;
096700130304               P1Cva2  = sk_Impo(xx);
096800130304               P1Cspe2 = sk_Sped(xx);
096900130304             ENDIF;
097000120705             xx += 1;
097100130301             wConta += 1;
097200130301           ENDDO;
097300120705
097400130306           write  TE60D02;
097500120705
097600130301         ENDDO;
097700130306
097800130306       //?Stampa riga finale
097900130306         write  TE60V02;
098000120705
098100120705       ENDSR;
098200120705
098300120705       //--------------------------------------------------------------
098400120705       //?Decodifica Sigla Varia.                                      ?
098500120705       //--------------------------------------------------------------
098600120705       BEGSR  sr_TabCC;
098700120705
098800120705         clear  dsCC;
098900120705         k_TBLcod = 'CC';
099000130301         k_TBLkey = 'VARIE  ' + wVaria;
099100120705
099200120705         chain  %kds( k03tabel00 )  TABEL;
099300120705
099400120705         if  %found(TABEL00F);
099500120705           dsCC = TBLuni;
099600120705         else;
099700120705           §CCdes = *all'?';
099800120705         endif;
099900120705
100000120705       ENDSR;
100100120705
100200120705       //--------------------------------------------------------------
100300130301       //?Routine Finale.
100400120705       //--------------------------------------------------------------
100500130301       BEGSR  RoutEnd;
100600120710
100700130301       //?Stampa "*** FINE STAMPA ***"?
100800130306         write  TE60E01;
100900130306
101000130306       //?Stampa "*** FINE STAMPA ***"?
101100130306       //?per dettaglio
101200130306         IF  pardet = 'S';
101300130306           write  TE601E01;
101400130306         ENDIF;
101500120705
101600130301       //?Chiusura funzioni precedentemente aperte?
101700120705         reset  TIBS69ds;
101800120705         tibs69r( tibs69ds :
101900120705                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
102000130301
102100130301         *inLR = *on;
102200120705
102300130301       //?Chiusura pgm?
102400120705         return;
102500120705
102600120705       ENDSR;
