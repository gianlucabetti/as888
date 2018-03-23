000100130301       //======================================================================
000200160929       //?TICF41R - Stampa simulazione delta                            (batch)  ?
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
003800160929     fTICF41P   o    e             printer oflind(*in99)
003900130305       //?File di stampa dettaglio?
004000160929     fTICF41P1  o    e             printer oflind(*in98) usropn
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
007400130719     d Tvlb            s              9  3 dim(200)                             Volume
007500130719     d Gvlb            s              9  3 dim(200)                             Volume
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
010101160929     d  parlnp                 1      3  0
010102160929     d  pards1                 4     11  0
010103160929     d  pards2                12     19  0
010104160929     d  parksc                20     26  0
010105160929     d  parctr                27     29  0
010106160929     d  parprg                30     32  0
010107160929     d  pardtcal              33     33
010108160929     d  pardet                34     34
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
027800160929           open TICF41P1;
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
031400130304           setll (parksc:parctr:parprg:parlnp) TITAD04L;
031500130304           reade (parksc:parctr:parprg:parlnp) TITAD04L;
031700130304         DOW  not %eof(TITAD04L);
031800130304           wTAD = *on;
031900130304           exsr memTAD;
032000130304
032400130304             reade (parksc:parctr:parprg:parlnp) TITAD04L;
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
039400130305       //?Giro le date per la stampa
039500130305         wData_ISO = %date(pards1:*iso);
039600130305         wData_EUR = wData_ISO;
039700130305         p1dtsda = %dec(wData_EUR);
039800130305         wData_ISO = %date(pards2:*iso);
039900130305         wData_EUR = wData_ISO;
040000130305         p1dtsal = %dec(wData_EUR);
040100130305
040200130305       //?Decodifico Cliente/Offerta
040400130305           clear wDES;
040500130305           wksc = parksc;
040600130305           exsr decoKSC;
040700130305           destar = wDES;
040800130305           stptar = parksc;
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
054900131007         clear sttbl;
055000131007         clear sterr;
055100130306       //?Decodifico codice tassazione
055200130306         clear stcts;
055300130306         clear stdcts;
055400130306         stcts = cts(xc);
055500130306         wCTS = cts(xc);
055600130306         clear wDES;
055700130306         exsr decoCTS;
055800130306         stdcts = wDES;
055900130306
056000130306       //?Giro la data della spedizione
056100130306         wData_ISO = %date(wfddsp:*iso);
056200130306         wData_EUR = wData_ISO;
056300130306         stpdsp = %dec(wData_EUR);
056400130306
056500131007       //?Se errore stampo l'errore al posto dei costi/ricavi ma solo se non è NONC
056600131007         *in51 = (WFDerr <> *blanks) and (%subst(wfderr: 75: 4) <> 'NONC');
056700131007         *in52 = (WFDerr <> *blanks) and (%subst(wfderr: 75: 4)  = 'NONC');
056800131007         STERR = %subst(WFDERR: 75: 4);
056900131007       // bolla non calcolabile mi carico le competenze dai file ECCET / ECCED
057000131007         if  *in52 ;
057100131007             STNUS1 +=1 ;
057200131007             STRIC1 +=WFDRIC ;
057300131007             STCOM1 +=WFDCOM ;
057400131007             sttbl = %subst(wfderr: 72: 2);
057500131007         endif ;
057600131007
057700130306
057800130306       //?salto pagina
057900130306         IF  *in98;
058000130306           write TE601T01;
058100130306           *in98 = *off;
058200130306         ENDIF;
058300130306
058400130306       //?stampa riga
058500130306         write TE601D01;
058600130306
058700130306       ENDSR;
058800130306
058900130306       //--------------------------------------------------------------
059000130306       //?Stampa per codice tassazione.
059100130306       //--------------------------------------------------------------
059200130306       BEGSR  STActs;
059300130306
059400130306       //?Stampo una riga per scaglione
059500130306         xi = 1;
059600130307         FOR xi by 1 to ultsgl;
059700130306           stsgl = ssgl(xi);
059800130306           stnus = Tnsp(xi);
059900130306           stpkg = Tpkg(xi);
060000130306           stncl = Tncl(xi);
060100130306           stvlb = Tvlb(xi);
060200130306           stqft = Tqft(xi);
060300130306           stric = Tric(xi);
060400130306           stcom = Tcom(xi);
060500130306           exsr STArig;
060600130306         ENDFOR;
060700130306
060800130306       //?Stampa riga a totale codice tassazione
060900130306         clear stsgl;
061000130306         stnus = %xfoot(Tnsp);
061100130306         stpkg = %xfoot(Tpkg);
061200130306         stncl = %xfoot(Tncl);
061300130306         stvlb = %xfoot(Tvlb);
061400130306         stqft = %xfoot(Tqft);
061500130306         stric = %xfoot(Tric);
061600130306         stcom = %xfoot(Tcom);
061700130306         exsr STArig;
061800131008
061900131008       //?Stampa riga vuota
062000131008         write  TE60V01;
062100130306
062200130306
062300130306       ENDSR;
062400130306
062500130306       //--------------------------------------------------------------
062600130306       //?Stampa totali generali.
062700130306       //--------------------------------------------------------------
062800130306       BEGSR  STAgen;
062900130306
063000130306       //?Imposto che stampa totali generali
063100130306         wTOT = *on;
063200130306
063300130306       //?Stampo una riga per scaglione
063400130306         xi = 1;
063500130307         FOR xi by 1 to ultsgl;
063600130306           stsgl = ssgl(xi);
063700130306           stnus = Gnsp(xi);
063800130306           stpkg = Gpkg(xi);
063900130306           stncl = Gncl(xi);
064000130306           stvlb = Gvlb(xi);
064100130306           stqft = Gqft(xi);
064200130306           stric = Gric(xi);
064300130306           stcom = Gcom(xi);
064400130306           exsr STArig;
064500130306         ENDFOR;
064600130306
064700130306       //?Stampa riga a totale
064800130306         clear stsgl;
064900130306         stnus = %xfoot(Gnsp);
065000130306         stpkg = %xfoot(Gpkg);
065100130306         stncl = %xfoot(Gncl);
065200130306         stvlb = %xfoot(Gvlb);
065300130306         stqft = %xfoot(Gqft);
065400130306         stric = %xfoot(Gric);
065500130306         stcom = %xfoot(Gcom);
065600130306         exsr STArig;
065700130306
065800131008       //?Imposto che stampa totali bolle non calcolabili
065900131008
066000131008       //?Stampa riga a totale
066100131008         stnus2= stnus1 ;
066200131008         stric2= stric1 ;
066300131008         stcom2= stcom1 ;
066400131008
066500131008       //?stampa riga
066600131008         write TE60N02;
066700131008
066800130306       ENDSR;
066900130306
067000130306       //--------------------------------------------------------------
067100130306       //?Stampa la riga.
067200130306       //--------------------------------------------------------------
067300130306       BEGSR  STArig;
067400130306
067500130306         clear stcts;
067600130306         clear stdcts;
067700130306
067800130306       //?Solo per il primo scaglione
067900130306         IF  stsgl = ssgl(1);
068000130306
068100130306         //?Se sto stampando i totali generali imposto la scritta
068200130306         //?'TOTALE GENERALE' nella colonna del codice tassazione
068300130306           IF  wTOT;
068400130306             clear stcts;
068500130306             stdcts = 'TOTALE GENERALE';
068600130306           ENDIF;
068700130306
068800130306         //?Se sto stampando il dettaglio per scaglione
068900130306         //?Decodifico il codice tassazione
069000130306           IF  not wTOT;
069100130306             stcts = cts(xc);
069200130306             wCTS = cts(xc);
069300130306             clear wDES;
069400130306             exsr decoCTS;
069500130306             stdcts = wDES;
069600130306           ENDIF;
069700130306         ENDIF;
069800130306
069900130306       //?Stampo la riga dei totali per codice tassazione
070000130306       //?Al posto dello scaglione scrivo 'TOTALE'
070100130306         *in50 = (stsgl = 0);
070200130306
070300130306       //?Calcola la percentuale DELTA
070400130306         wGAP = (stric - stcom) * 100;
070500130306         IF  stric > 0;
070600130306           wGAP = wGAP / stric;
070700130306         ENDIF;
070800130502         //eval(h) wdelta = wGAP;   //?Va in errore se wGAP > 999,9 o < -999,9?
070900130502         select;
071000130502           when  wGAP >= 1000;
071100130502             wDelta = +999,9;
071200130502           when  wGAP <= -1000;
071300130502             wDelta = -999,9;
071400130502           other;
071500130502             eval(h) wDelta = wGAP;
071600130502         ENDSL;
071700130306
071800130306       //?se +/- 1000 imposto +/- 999,9
071900130306         SELECT;
072000130306           WHEN  wdelta >= 0 and wGAP >= 1000;
072100130306             stdel = +999,9;
072200130306           WHEN  wdelta < 0 and wGAP <= -1000;
072300130306             stdel = -999,9;
072400130306           OTHER;
072500130306             stdel = wdelta;
072600130306         ENDSL;
072700130306
072800130306       //?salto pagina
072900130306         IF  *in99;
073000130306           write TE60T01;
073100130306           *in99 = *off;
073200130306         ENDIF;
073300130306
073400130306       //?stampa riga
073500130306         write TE60D01;
073600130306
073700130306       ENDSR;
073800130306
073900130306       //--------------------------------------------------------------
074000130306       //?Decodifico Codice tassazione.
074100130306       //--------------------------------------------------------------
074200130306       BEGSR  decoCTS;
074300130306
074400130306         clear dsCT;
074500130306         k_TBLcod = 'CT';
074600130306         k_TBLkey = wCTS;
074700130306         chain  %kds( k03tabel00 )  TABEL;
074800130306         IF  %found(TABEL00F) and TBLflg = *blanks;
074900130306           dsCT = TBLuni;
075000130306         ENDIF;
075100130306         wDES = §CTdes;
075200130306
075300130306       ENDSR;
075400130305
075500120705       //--------------------------------------------------------------
075600130301       //?Elaborazione file riepilogo varie
075700120705       //--------------------------------------------------------------
075800130307       BEGSR  ElabVarie;
075900130305
076000130305         wEnd = *off;
076100130301
076200130301       //?Dichiarazione cursore
076300130301         exec sql
076400130301           DECLARE WFDBF cursor for
076500130301           SELECT * from WFDBF10F;
076600130301
076700130301       //?Apertura del cursore
076800130301         exec sql
076900130301           open WFDBF;
077000130301
077100130301         IF sqlcode < 0;
077200130301           wEnd = *on;
077300130301         ENDIF;
077400130301
077500130301         DOW not wEnd;
077600130301           exec sql
077700130301             fetch next from WFDBF into :WFDBF10F;
077800130301           IF  sqlcod = 100 or sqlcod < 0;
077900130301             wEnd = *on;
078000130301             leave;
078100130301           ENDIF;
078200120705
078300130301       //?Sommo le varie
078400130301           exsr varieWF;
078500130301
078600130301         ENDDO;
078700130301
078800130301         exec sql
078900130301           close WFDBF;
079000130307
079100130307         //?Stampa le varie
079200130307         exsr  StampaVarie;
079300120705
079400120705       ENDSR;
079500130301
079600130301       //--------------------------------------------------------------
079700130301       //?Varie del Work File in SK.                                   ?
079800130301       //--------------------------------------------------------------
079900130307       BEGSR  varieWF;
080000130307
080100130307       //?Imposto il porto nella prima voce sempre
080200130307         sk_Voce(1) = '.';
080300130307         IF  WF1por > 0;
080400130307           sk_Impo(1) += WF1por;
080500130307           sk_Sped(1) += 1;
080600130307         ENDIF;
080700130301
080800130307       //?poi schiero le varie
080900130301         IF  WF1sv1 <> *blanks;
081000130301           Wvaria = WF1sv1;
081100130301           Wimpo  = WF1va1;
081200130301           exsr  TOTvaria;
081300130301         ENDIF;
081400130301
081500130301         IF  WF1sv2 <> *blanks;
081600130301           Wvaria = WF1sv2;
081700130301           Wimpo  = WF1va2;
081800130301           exsr  TOTvaria;
081900130301         ENDIF;
082000130301
082100130301         IF  WF1sv3 <> *blanks;
082200130301           Wvaria = WF1sv3;
082300130301           Wimpo  = WF1va3;
082400130301           exsr  TOTvaria;
082500130301         ENDIF;
082600130301
082700130301         IF  WF1sv4 <> *blanks;
082800130301           Wvaria = WF1sv4;
082900130301           Wimpo  = WF1va4;
083000130301           exsr  TOTvaria;
083100130301         ENDIF;
083200130301
083300130301         IF  WF1sv5 <> *blanks;
083400130301           Wvaria = WF1sv5;
083500130301           Wimpo  = WF1va5;
083600130301           exsr  TOTvaria;
083700130301         ENDIF;
083800130301
083900130301         IF  WF1sv6 <> *blanks;
084000130301           Wvaria = WF1sv6;
084100130301           Wimpo  = WF1va6;
084200130301           exsr  TOTvaria;
084300130301         ENDIF;
084400130301
084500130301         IF  WF1sv7 <> *blanks;
084600130301           Wvaria = WF1sv7;
084700130301           Wimpo  = WF1va7;
084800130301           exsr  TOTvaria;
084900130301         ENDIF;
085000130301
085100130301         IF  WF1sv8 <> *blanks;
085200130301           Wvaria = WF1sv8;
085300130301           Wimpo  = WF1va8;
085400130301           exsr  TOTvaria;
085500130301         ENDIF;
085600130301
085700130301         IF  WF1sv9 <> *blanks;
085800130301           Wvaria = WF1sv9;
085900130301           Wimpo  = WF1va9;
086000130301           exsr  TOTvaria;
086100130301         ENDIF;
086200130301
086300130301         IF  WF1s10 <> *blanks;
086400130301           Wvaria = WF1s10;
086500130301           Wimpo  = WF1v10;
086600130301           exsr  TOTvaria;
086700130301         ENDIF;
086800130301
086900130301         IF  WF1s11 <> *blanks;
087000130301           Wvaria = WF1s11;
087100130301           Wimpo  = WF1v11;
087200130301           exsr  TOTvaria;
087300130301         ENDIF;
087400130301
087500130301         IF  WF1s12 <> *blanks;
087600130301           Wvaria = WF1s12;
087700130301           Wimpo  = WF1v12;
087800130301           exsr  TOTvaria;
087900130301         ENDIF;
088000130301
088100130301         IF  WF1s13 <> *blanks;
088200130301           Wvaria = WF1s13;
088300130301           Wimpo  = WF1v13;
088400130301           exsr  TOTvaria;
088500130301         ENDIF;
088600130301
088700130301         IF  WF1s14 <> *blanks;
088800130301           Wvaria = WF1s14;
088900130301           Wimpo  = WF1v14;
089000130301           exsr  TOTvaria;
089100130301         ENDIF;
089200130301
089300130301         IF  WF1s15 <> *blanks;
089400130301           Wvaria = WF1s15;
089500130301           Wimpo  = WF1v15;
089600130301           exsr  TOTvaria;
089700130301         ENDIF;
089800130301
089900130301         IF  WF1s16 <> *blanks;
090000130301           Wvaria = WF1s16;
090100130301           Wimpo  = WF1v16;
090200130301           exsr  TOTvaria;
090300130301         ENDIF;
090400130301
090500130301         IF  WF1s17 <> *blanks;
090600130301           Wvaria = WF1s17;
090700130301           Wimpo  = WF1v17;
090800130301           exsr  TOTvaria;
090900130301         ENDIF;
091000130301
091100130301         IF  WF1s18 <> *blanks;
091200130301           Wvaria = WF1s18;
091300130301           Wimpo  = WF1v18;
091400130301           exsr  TOTvaria;
091500130301         ENDIF;
091600130301
091700130301         IF  WF1s19 <> *blanks;
091800130301           Wvaria = WF1s19;
091900130301           Wimpo  = WF1v19;
092000130301           exsr  TOTvaria;
092100130301         ENDIF;
092200130301
092300130301         IF  WF1s20 <> *blanks;
092400130301           Wvaria = WF1s20;
092500130301           Wimpo  = WF1v20;
092600130301           exsr  TOTvaria;
092700130301         ENDIF;
092800130301
092900130301       ENDSR;
093000120705
093100120705       //--------------------------------------------------------------
093200120705       //?Incremento totali per voce in schiera.                       ?
093300120705       //--------------------------------------------------------------
093400130301       BEGSR  TOTvaria;
093500120705
093600130307         xx = %lookup(Wvaria : sk_Voce:2);
093700130301         IF  xx = *zero;
093800130307           xx = %lookup(*blank : sk_Voce:2);
093900130301           IF  xx > *zero;
094000120705             sk_Voce(xx) = Wvaria;
094100130301           ENDIF;
094200130301         ENDIF;
094300120705
094400120705         sk_Impo(xx) += Wimpo;
094500120705         sk_Sped(xx) += 1;
094600120705
094700120705       ENDSR;
094800120705
094900120705       //--------------------------------------------------------------
095000130307       //?Stampa le VARIE.
095100120705       //--------------------------------------------------------------
095200130307       BEGSR  StampaVarie;
095300120705
095400130301       //?Stampa testate?
095500130306         write  TE60T02;
095600130306         write  TE60T03;
095700120705
095800130301       //?Stampa delle varie di 2 in 2?
095900120705         xx = 1;
096000130307
096100130307       //?Se non c'è il porto (sempre in prima posizione) inizio a
096200130307       //?stampare dalla seconda varia
096300130307         IF  sk_Impo(1) = *zeros;
096400130307           xx = 2;
096500130307         ENDIF;
096600130307
096700130307         DOW  sk_Voce(xx) <> *blank;
096800120705
096900130301           wConta = 1;
097000130306           clear  TE60D02;
097100120705
097200130301           DOW  wConta <= 2  and  sk_Voce(xx) <> *blank;
097300130301             wVaria = sk_Voce(xx);
097400130307
097500130307             IF  wConta = 1;
097600130307             //?nella prima posizione c'è sempre il porto
097700130307               IF xx = 1;
097800130307                 P1Dsv1 = 'TRASPORTO';
097900130307               ELSE;
098000130307                 exsr  sr_TabCC;
098100130307                 P1Csv1 = sk_Voce(xx) + ' -';
098200130307                 P1Dsv1 = §CCdes;
098300130307               ENDIF;
098400130304               P1Cva1  = sk_Impo(xx);
098500130304               P1Cspe1 = sk_Sped(xx);
098600130304             ELSE;
098700130307               exsr  sr_TabCC;
098800130304               P1Csv2  = sk_Voce(xx) + ' -';
098900130304               P1Dsv2  = §CCdes;
099000130304               P1Cva2  = sk_Impo(xx);
099100130304               P1Cspe2 = sk_Sped(xx);
099200130304             ENDIF;
099300120705             xx += 1;
099400130301             wConta += 1;
099500130301           ENDDO;
099600120705
099700130306           write  TE60D02;
099800120705
099900130301         ENDDO;
100000130306
100100130306       //?Stampa riga finale
100200130306         write  TE60V02;
100300120705
100400120705       ENDSR;
100500120705
100600120705       //--------------------------------------------------------------
100700120705       //?Decodifica Sigla Varia.                                      ?
100800120705       //--------------------------------------------------------------
100900120705       BEGSR  sr_TabCC;
101000120705
101100120705         clear  dsCC;
101200120705         k_TBLcod = 'CC';
101300130301         k_TBLkey = 'VARIE  ' + wVaria;
101400120705
101500120705         chain  %kds( k03tabel00 )  TABEL;
101600120705
101700120705         if  %found(TABEL00F);
101800120705           dsCC = TBLuni;
101900120705         else;
102000120705           §CCdes = *all'?';
102100120705         endif;
102200120705
102300120705       ENDSR;
102400120705
102500120705       //--------------------------------------------------------------
102600130301       //?Routine Finale.
102700120705       //--------------------------------------------------------------
102800130301       BEGSR  RoutEnd;
102900120710
103000130301       //?Stampa "*** FINE STAMPA ***"?
103100130306         write  TE60E01;
103200130306
103300130306       //?Stampa "*** FINE STAMPA ***"?
103400130306       //?per dettaglio
103500130306         IF  pardet = 'S';
103600131007           IF STNUS1 > 0;
103700131007              WRITE TE601T02 ;
103800131007           ENDIF ;
103900130306           write  TE601E01;
104000130306         ENDIF;
104100120705
104200130301       //?Chiusura funzioni precedentemente aperte?
104300120705         reset  TIBS69ds;
104400120705         tibs69r( tibs69ds :
104500120705                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
104600130301
104700130301         *inLR = *on;
104800120705
104900130301       //?Chiusura pgm?
105000120705         return;
105100120705
105200120705       ENDSR;
