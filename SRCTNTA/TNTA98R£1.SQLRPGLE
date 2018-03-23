000100080206      //--------------------------------------------------------------
000200100708      //?TNTA98R - Elenco trattative
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000100527     fAZORG01L  if   e           k disk
001100100325     fTABEL00F  if   e           k disk
001200100329     fTNTBE01L  if   e           k disk
001300100713     fTNTA98P   o    e             printer infds(prtinfo)
001400100713     f                                     oflind(*in99)
001500100310
001600080206      //---------------------------------------------------------------
001700090406      //?Definizione costanti.
001800080206      //---------------------------------------------------------------
001900080207
002000100329      // - Tabella tipo trattativa
002100100329     d C_tab           c                   const('TTR')
002200080206
002300080206      //---------------------------------------------------------------
002400080206      //?Definizione schiere.
002500080206      //---------------------------------------------------------------
002600100714
002700100714      // - Agenti Unificanti
002800100714     d $AgeU           s              7  0 dim(3500)
002900100329
003000110118      // - Tipo trattative --> ordinamento(2)  + codice(1) +
003100110118      //                       descrizione(25) + info s/n(1)
003200110118     d $Ttr            s             29    dim(10)
003300100714
003400100714      // - Totali Generali commerciali ESTERNI
003500100714     d $TotECaldo      s              6  0 dim(10)
003600100714     d $TotECaldoF     s             12  0 dim(10)
003700100714     d $TotECaldoS     s              8  0 dim(10)
003800100714     d $TotENeg        s              6  0 dim(10)
003900100714     d $TotENegF       s             12  0 dim(10)
004000100714     d $TotENegS       s              8  0 dim(10)
004100100714     d $TotEPos        s              6  0 dim(10)
004200100714     d $TotEPosF       s             12  0 dim(10)
004300100714     d $TotEPosS       s              8  0 dim(10)
004400100714     d $TotE01Fat      s             12  0 dim(10)
004500100714     d $TotE01Spe      s              8  0 dim(10)
004600100714     d $TotE01Tr       s              6  0 dim(10)
004700100714     d $TotE02Fat      s             12  0 dim(10)
004800100714     d $TotE02Spe      s              8  0 dim(10)
004900100714     d $TotE02Tr       s              6  0 dim(10)
005000100714     d $TotE03Fat      s             12  0 dim(10)
005100100714     d $TotE03Spe      s              8  0 dim(10)
005200100714     d $TotE03Tr       s              6  0 dim(10)
005300110112      // - Totali Generali commerciali ESTERNI di cui estero
005400110112     d $TotECaldoe     s              6  0 dim(10)
005500110112     d $TotECaldoFe    s             12  0 dim(10)
005600110112     d $TotECaldoSe    s              8  0 dim(10)
005700110112     d $TotENege       s              6  0 dim(10)
005800110112     d $TotENegFe      s             12  0 dim(10)
005900110112     d $TotENegSe      s              8  0 dim(10)
006000110112     d $TotEPose       s              6  0 dim(10)
006100110112     d $TotEPosFe      s             12  0 dim(10)
006200110112     d $TotEPosSe      s              8  0 dim(10)
006300110112     d $TotE01Fate     s             12  0 dim(10)
006400110112     d $TotE01Spee     s              8  0 dim(10)
006500110112     d $TotE01Tre      s              6  0 dim(10)
006600110112     d $TotE02Fate     s             12  0 dim(10)
006700110112     d $TotE02Spee     s              8  0 dim(10)
006800110112     d $TotE02Tre      s              6  0 dim(10)
006900110112     d $TotE03Fate     s             12  0 dim(10)
007000110112     d $TotE03Spee     s              8  0 dim(10)
007100110112     d $TotE03Tre      s              6  0 dim(10)
007200100713
007300100714      // - Totali Generali commerciali INTERNI
007400100714     d $TotICaldo      s              6  0 dim(10)
007500100714     d $TotICaldoF     s             12  0 dim(10)
007600100714     d $TotICaldoS     s              8  0 dim(10)
007700100714     d $TotINeg        s              6  0 dim(10)
007800100714     d $TotINegF       s             12  0 dim(10)
007900100714     d $TotINegS       s              8  0 dim(10)
008000100714     d $TotIPos        s              6  0 dim(10)
008100100714     d $TotIPosF       s             12  0 dim(10)
008200100714     d $TotIPosS       s              8  0 dim(10)
008300100714     d $TotI01Fat      s             12  0 dim(10)
008400100714     d $TotI01Spe      s              8  0 dim(10)
008500100714     d $TotI01Tr       s              6  0 dim(10)
008600100714     d $TotI02Fat      s             12  0 dim(10)
008700100714     d $TotI02Spe      s              8  0 dim(10)
008800100714     d $TotI02Tr       s              6  0 dim(10)
008900100714     d $TotI03Fat      s             12  0 dim(10)
009000100714     d $TotI03Spe      s              8  0 dim(10)
009100100714     d $TotI03Tr       s              6  0 dim(10)
009200110112      // - Totali Generali commerciali INTERNI di cui estero
009300110112     d $TotICaldoe     s              6  0 dim(10)
009400110112     d $TotICaldoFe    s             12  0 dim(10)
009500110112     d $TotICaldoSe    s              8  0 dim(10)
009600110112     d $TotINege       s              6  0 dim(10)
009700110112     d $TotINegFe      s             12  0 dim(10)
009800110112     d $TotINegSe      s              8  0 dim(10)
009900110112     d $TotIPose       s              6  0 dim(10)
010000110112     d $TotIPosFe      s             12  0 dim(10)
010100110112     d $TotIPosSe      s              8  0 dim(10)
010200110112     d $TotI01Fate     s             12  0 dim(10)
010300110112     d $TotI01Spee     s              8  0 dim(10)
010400110112     d $TotI01Tre      s              6  0 dim(10)
010500110112     d $TotI02Fate     s             12  0 dim(10)
010600110112     d $TotI02Spee     s              8  0 dim(10)
010700110112     d $TotI02Tre      s              6  0 dim(10)
010800110112     d $TotI03Fate     s             12  0 dim(10)
010900110112     d $TotI03Spee     s              8  0 dim(10)
011000110112     d $TotI03Tre      s              6  0 dim(10)
011100100713
011200100713      // - Totali Unificante x tipo
011300100713     d $UniCaldo       s              6  0 dim(10)
011400100713     d $UniCaldoF      s             12  0 dim(10)
011500100713     d $UniCaldoS      s              8  0 dim(10)
011600100713     d $UniNeg         s              6  0 dim(10)
011700100713     d $UniNegF        s             12  0 dim(10)
011800100713     d $UniNegS        s              8  0 dim(10)
011900100713     d $UniPos         s              6  0 dim(10)
012000100713     d $UniPosF        s             12  0 dim(10)
012100100713     d $UniPosS        s              8  0 dim(10)
012200100713     d $Uni01Fat       s             12  0 dim(10)
012300100713     d $Uni01Spe       s              8  0 dim(10)
012400100713     d $Uni01Tr        s              6  0 dim(10)
012500100713     d $Uni02Fat       s             12  0 dim(10)
012600100713     d $Uni02Spe       s              8  0 dim(10)
012700100713     d $Uni02Tr        s              6  0 dim(10)
012800100713     d $Uni03Fat       s             12  0 dim(10)
012900100713     d $Uni03Spe       s              8  0 dim(10)
013000100713     d $Uni03Tr        s              6  0 dim(10)
013100110112      // - Totali Unificante x tipo di cui estero
013200110112     d $UniCaldoe      s              6  0 dim(10)
013300110112     d $UniCaldoFe     s             12  0 dim(10)
013400110112     d $UniCaldoSe     s              8  0 dim(10)
013500110112     d $UniNege        s              6  0 dim(10)
013600110112     d $UniNegFe       s             12  0 dim(10)
013700110112     d $UniNegSe       s              8  0 dim(10)
013800110112     d $UniPose        s              6  0 dim(10)
013900110112     d $UniPosFe       s             12  0 dim(10)
014000110112     d $UniPosSe       s              8  0 dim(10)
014100110112     d $Uni01Fate      s             12  0 dim(10)
014200110112     d $Uni01Spee      s              8  0 dim(10)
014300110112     d $Uni01Tre       s              6  0 dim(10)
014400110112     d $Uni02Fate      s             12  0 dim(10)
014500110112     d $Uni02Spee      s              8  0 dim(10)
014600110112     d $Uni02Tre       s              6  0 dim(10)
014700110112     d $Uni03Fate      s             12  0 dim(10)
014800110112     d $Uni03Spee      s              8  0 dim(10)
014900110112     d $Uni03Tre       s              6  0 dim(10)
015000080206
015100080206      //---------------------------------------------------------------
015200080206      //?Definizione aree dati.
015300080206      //---------------------------------------------------------------
015400080206
015500080206      // - Dati utente
015600080206     d §AzUte        e ds                  extname(AZUTE00F)
015700080206     d                                     dtaara
015800080206     d §DatiUte      e ds                  extname(dDatiUte)
015900080206     d                                     dtaara
016000080206
016100080206      //---------------------------------------------------------------
016200080206      //?Definizione strutture dati.
016300080206      //---------------------------------------------------------------
016400100310
016500100310     d PrtINFO         ds
016600100310     d  CurLine              367    368i 0
016700080206
016800080206      // - Status
016900080206     d Psds           sds
017000080206     d   SDSpgm          *proc
017100080206
017200080206      // - Parametri ricevuti
017300080206     d KPJBA         e ds
017400100524     d TNTA98DS      e ds
017500080206
017600100308      // - Ricerca/Controllo tabelle
017700100308     d TIBS02ds      e ds                  inz
017800100308
017900080206      // - Reperimento dati utente
018000100305     d TIBS34DS      e ds
018100100714
018200100714      // - controllo abilitazioni
018300100714     d TNTAA1DS      e ds                  inz
018400100714
018500100714      // - Carico Filiali abilitate all'utente
018600100714     d TRUL31DS      e ds
018700100714     d POG                    10    759    DIM(250)
018800110119      // - Carico Aree abilitate all'utente
018900110119     d TRUL31DS2     e ds
019000110119     d CAR                    22    171    DIM(50)
019100100325
019200100325      // - Tabella CCO = Causale contatto
019300100325     d dCCO          e ds                  inz
019400100308
019500100324      // - Tabella ITS = Sottotipi INFO trattativa
019600100324     d dITS          e ds                  inz
019700100329
019800100329      // - Tabella TTR = Tipo trattativa
019900100329     d dTTR          e ds                  inz
020000100325
020100100325      // - Tabella 01 = Codici Commerciali
020200100325     d ds01          e ds                  inz
020300110119
020400110119      // - Tabella 05 = Codici Aree
020500110119     d ds05          e ds                  inz
020600100527
020700100527      // File di work
020800100527     d WFRCTDS       e ds                  extname(WFRCT00F)
020900090629
021000080206      //---------------------------------------------------------------
021100080206      //?Definizione variabili globali.
021200080206      //---------------------------------------------------------------
021300080206
021400080206      // - Flags booleani
021500100714     d $DetCmm         s               n   inz(*off)
021600100305     d $End            s               n   inz(*off)
021700100714     d $EndAge         s               n   inz(*off)
021800100714     d $Esterno        s               n   inz(*off)
021900100714     d $Interno        s               n   inz(*off)
022000100714     d $TtrOk          s               n   inz(*off)
022100100325
022200100325      // - Indici di schiera
022300100326     d xx              s              4  0 inz
022400100714     d yy              s              4  0 inz
022500100329     d ww              s              4  0 inz
022600100714     d zz              s              4  0 inz
022700080206
022800090806       // - Stringa SQL da eseguire
022900090806     d wSQL            s           2048    Varying        inz
023000080206
023100090401      // - Campi di comodo data
023200100305     d wDate_EUR       s               d   datfmt(*eur)
023300100305     d wDate_ISO       s               d   datfmt(*iso)
023400100305
023500100305      // - Campi di comodo
023600100713     d wConc           s                   like(RCTconc1)
023700100329     d wConta          s              2  0 inz
023800100715     d wFun            s                   like(RCTfun)
023900100329     d wOggi           s              8  0 inz
024000100305     d wTrat           s            196    inz(*all'-')
024100100603     d w120            s             12  0 inz
024200100603     d w60             s              6  0 inz
024300100325
024400090508      //---------------------------------------------------------------
024500090807      //?Definizione procedure esterne.
024600090508      //---------------------------------------------------------------
024700100120
024800110119      // - Filiali e aree abilitate
024900110119     d trul31r         pr                  extpgm('TRUL31R')
025000110119     d  kpjba                              likeds(KPJBA)
025100110119     d  trul31ds                           likeds(trul31ds)
025200110119     d  trul31ds2                          likeds(trul31ds2)
025300100120
025400100120      //---------------------------------------------------------------
025500100120      //?prototipi
025600100120      //---------------------------------------------------------------
025700090807
025800080626      /copy gaitrasrc/srcprotopr,tibs02r
025900080626      /copy gaitrasrc/srcprotopr,tibs34r
026000100714      /copy gaitrasrc/srcprotopr,tntaa1c
026100090807
026200080206      //---------------------------------------------------------------
026300080206      //?Definizione key-list.
026400080206      //---------------------------------------------------------------
026500100325
026600100325       // - File TABEL00F
026700100325     d k03tabel      e ds                  extname(TABEL00F:*key)
026800100325     d                                     prefix(k_)
026900100325     d                                     inz
027000080206
027100080206      //---------------------------------------------------------------
027200080206      //?Riepilogo indicatori.
027300080206      //---------------------------------------------------------------
027400100326
027500100713      // 01    : Condizione stampa trattative AVVIATE
027600100524      // 02    : Condizione stampa trattative CHIUSE
027700100524      // 03    : Condizione stampa trattative IN CORSO
027800110119      // 09    : Condizione stampa selezione per Area
027900100330      // 10    : Condiziona stampa attività scadute
028000100330      // 11    : Condiziona stampa prossima attività
028100100528      // 12    : Condiziona stampa totale importi nei totali x tipo tr.
028200100601      // 13    : Condizione stampa totale generale commerciali esterni
028300100716      // 14    : Condizione stampa totale generale commerciali interni
028400110118      // 15    : NON stampa Spedizioni nei totali x tipo tr. se 'A'
028500100326      // 99    : Salto pagina
028600090807
028700080206      //---------------------------------------------------------------
028800080206
028900080206      //---------------------------------------------------------------
029000080206      //?M A I N - L I N E
029100080206      //---------------------------------------------------------------
029200080206
029300080206     c     *Entry        plist
029400080206     c                   parm                    KPJBA
029500080206
029600080206      /free
029700080206
029800090807       //?Operazioni iniziali
029900080206       exsr RoutInz;
030000100524
030100100524       //?Stampo la pagina dei parametri di lancio
030200100524       write TA98PAR;
030300100714
030400100714       //?Ciclo per sk unificanti e stampo
030500100714       zz = 1;
030600100714       FOR zz by 1 to %elem($AgeU);
030700100714         IF  $AgeU(zz) <> *zeros;
030800100714
030900100714         //?Elaboro le trattative AVVIATE nel periodo elaborazione
031000100810           IF  ITA98dtavi <> 0;
031100100810             exsr sr_Avviate;
031200100810           ENDIF;
031300100714
031400100714         //?Elaboro le trattative CHIUSE nel periodo elaborazione
031500100810           IF  ITA98dtchi <> 0;
031600100810             exsr sr_Chiuse;
031700100810           ENDIF;
031800100714
031900100714         //?Elaboro le trattative IN CORSO nel periodo elaborazione
032000100810           IF  ITA98dtica <> 0;
032100100810            exsr sr_InCorso;
032200100810           ENDIF;
032300100714
032400100714         //?Stampo i totali per unificante
032500100716         //?solo se richiesto
032600100716           IF  ITA98cmmt = 'S';
032700100714             exsr sr_StpTotUni;
032800100714           ENDIF;
032900100716         //?Sommo i totali per unificante
033000100716           exsr sr_MemTotUni;
033100100714         ENDIF;
033200100714       ENDFOR;
033300100714
033400100714       //?Stampo i totali generali
033500100714       IF  ITA98cmm = 0
033600100714       //?e solo se richiesto
033700100714           and ITA98gent = 'S';
033800100714       //?la prima volta per commerciali interni
033900100715         IF  ITA98elcmm = 'I' or ITA98elcmm = *blanks;
034000100715           *in13 = *on;
034100100716           *in14 = *off;
034200100715           exsr sr_StpTotGenI;
034300100715         ENDIF;
034400100714       //?la seconda volta per commerciali esterni
034500100715         IF  ITA98elcmm = 'E' or ITA98elcmm = *blanks;
034600100715           *in13 = *off;
034700100716           *in14 = *on;
034800100715           exsr sr_StpTotGenE;
034900100715         ENDIF;
035000100716       //?la terza volta per TUTTI i commerciali
035100100716         IF  ITA98elcmm = *blanks;
035200100716           *in13 = *off;
035300100716           *in14 = *off;
035400100716           exsr sr_StpTotGen;
035500100716         ENDIF;
035600100714       ENDIF;
035700100714
035800100714       //?Stampo la fine
035900100714       write TA98FINE;
036000100714
036100100714       //?Operazioni finali
036200100714       exsr RoutEnd;
036300100714
036400100714       //--------------------------------------------------------------
036500100714       //?Operazioni iniziali.
036600100714       //--------------------------------------------------------------
036700100714       BEGSR RoutInz;
036800100714
036900100714         //?Imposto la ds con i dati della KPJBU
037000100714         TNTA98ds = kpjbu;
037100100714
037200100714         //?Reperimento dati job
037300100714         exsr DatiJob;
037400100714
037500100714         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
037600100714
037700100714         //?Imposto nome programma
037800100714         NomePgm = SDSpgm;
037900100714
038000100714         //?Imposto dati fissi
038100100714         k_tblkut = 1;
038200100714
038300100714         //?Imposto data e ora elaborazione
038400100714         wOggi     = %dec(%date());
038500100714         wDate_ISO = %date(wOggi:*ISO);
038600100714         wDate_EUR = wDate_ISO;
038700100714         DataEla   = %dec(wDate_EUR);
038800100714         OraEla    = %dec(%time());
038900110119
039000110119         //?Stampo l'area richiesta
039100110119         IF  ITA98car > 0;
039200110119           clear ds05;
039300110119           k_tblcod = '05';
039400110119           k_tblkey = %editc(ITA98car:'X');
039500110119           chain  %kds(K03tabel) TABEL00F;
039600110119           IF  %found( TABEL00F );
039700110119             ds05 = TBLuni;
039800110119           ENDIF;
039900110119           pDesCar = §05des;
040000110119         ENDIF;
040100110119
040200110119         //?Stampo che richieste tutte le aree
040300110119         IF  ITA98car = 0;
040400110119           pDesCar = 'Tutte';
040500110119         ENDIF;
040600100714
040700100714         //?Stampo l'unificante richiesto
040800100714         IF  ITA98cmm > 0;
040900100714           clear ds01;
041000100714           k_tblcod = '01';
041100100714           k_tblkey = %editc(ITA98cmm:'X');
041200100714           chain  %kds(K03tabel) TABEL00F;
041300100714           IF  %found( TABEL00F );
041400100714             ds01 = TBLuni;
041500100714           ENDIF;
041600100714           pDesCmm = §01age;
041700100714         ENDIF;
041800100714
041900100714         //?Stampo che richiesti tutti gli unificanti
042000100714         IF  ITA98cmm = 0;
042100100714           pDesCmm = 'Tutti';
042200100714         ENDIF;
042300100714
042400100714         //?Imposto data avvio
042500100714         IF  ITA98dtavi > 0;
042600100714           wDate_ISO = %date(ITA98dtavi:*iso);
042700100714           wDate_EUR = wDate_ISO;
042800100714           pDtavI    = %dec(wDate_EUR);
042900100714           wDate_ISO = %date(ITA98dtavf:*iso);
043000100714           wDate_EUR = wDate_ISO;
043100100714           pDtavF    = %dec(wDate_EUR);
043200100714         ENDIF;
043300100714
043400100714         //?Imposto data chiusura
043500100714         IF  ITA98dtchi > 0;
043600100714           wDate_ISO = %date(ITA98dtchi:*iso);
043700100714           wDate_EUR = wDate_ISO;
043800100714           pDtchI    = %dec(wDate_EUR);
043900100714           wDate_ISO = %date(ITA98dtchf:*iso);
044000100714           wDate_EUR = wDate_ISO;
044100100714           pDtchF    = %dec(wDate_EUR);
044200100714         ENDIF;
044300100714
044400100714         //?Imposto data in corso
044500100714         IF  ITA98dtica > 0;
044600100714           wDate_ISO = %date(ITA98dtica:*iso);
044700100714           wDate_EUR = wDate_ISO;
044800100714           pDticA    = %dec(wDate_EUR);
044900100714         ENDIF;
045000100714
045100100714         //?Imposto data presunto affidamento
045200100714         IF  ITA98dtaff > 0;
045300100714           wDate_ISO = %date(ITA98dtaff:*iso);
045400100714           wDate_EUR = wDate_ISO;
045500100714           pDtaff    = %dec(wDate_EUR);
045600100714         ENDIF;
045700110114
045800110114         //?Imposto il segno del Traffico totale DA/A
045900110114         pVlttsi = '+';
046000110114         IF  ITA98vltti < 0;
046100110114           pVlttsi = '-';
046200110114         ENDIF;
046300110114         pVlttsf = '+';
046400110114         IF  ITA98vlttf < 0;
046500110114           pVlttsf = '-';
046600110114         ENDIF;
046700110114
046800110114         //?Imposto il segno dell'Aumento/Sconto DA/A
046900110114         pAussi = '+';
047000110114         IF  ITA98ausi < 0;
047100110114           pAussi = '-';
047200110114         ENDIF;
047300110114         pAussf = '+';
047400110114         IF  ITA98ausf < 0;
047500110114           pAussf = '-';
047600110114         ENDIF;
047700100714
047800100714         //?Carico sk Tipi trattavia con relativo ordinamento
047900100714         //?e descrizione
048000100714         clear xx;
048100100714         setll C_tab TNTBE000;
048200100714         reade C_tab TNTBE000;
048300100714
048400100714         DOW not %eof(TNTBE01L);
048500100714           dTTR = TBEuni;
048600100714           IF  TBEatb = *blanks;
048700100714             xx += 1;
048800100714             $Ttr(xx) = %editc(§TTRord:'X') + %subst(TBEke1:1:1) +
048900110118                        §TTRdes + §TTRinfo;
049000100714           ENDIF;
049100100714           reade C_tab TNTBE000;
049200100714         ENDDO;
049300100714
049400100714         sorta $Ttr;
049500100714
049600100714         clear $AgeU;
049700100714
049800100714       //?Carico in sk l'unificante richiesto
049900100714         IF  ITA98cmm > 0;
050000100714           $AgeU(01) = ITA98cmm;
050100100714         ENDIF;
050200100714
050300100714       //?Carico gli unificanti delle filiali gestite dall'utente
050400100714         IF  ITA98cmm = 0;
050500100714           exsr sr_CarAgeUni;
050600100714           sorta $AgeU;
050700100714         ENDIF;
050800100714
050900100714       ENDSR;
051000100714
051100100714       //--------------------------------------------------------------
051200100714       //?Elaboro le trattative AVVIATE.
051300100714       //--------------------------------------------------------------
051400100714       BEGSR sr_Avviate;
051500100714
051600100714       //?Imposto flag per identificare che sto elaborando le
051700100714       //?trattative AVVIATE
051800100714         *in01 = *on;
051900100714         *in02 = *off;
052000100714         *in03 = *off;
052100100714
052200100714       //?Elabora le trattative
052300100714         exsr sr_Elabora;
052400100714
052500100714       ENDSR;
052600100714
052700100714       //--------------------------------------------------------------
052800100714       //?Elaboro le trattative CHIUSE.
052900100714       //--------------------------------------------------------------
053000100714       BEGSR sr_Chiuse;
053100100714
053200100714       //?Imposto flag per identificare che sto elaborando le
053300100714       //?trattative CHIUSE
053400100714         *in01 = *off;
053500100714         *in02 = *on;
053600100714         *in03 = *off;
053700100714
053800100714       //?Elabora le trattative
053900100714         exsr sr_Elabora;
054000100714
054100100714       ENDSR;
054200100714
054300100714       //--------------------------------------------------------------
054400100714       //?Elaboro le trattative IN CORSO.
054500100714       //--------------------------------------------------------------
054600100714       BEGSR sr_InCorso;
054700100714
054800100714       //?Imposto flag per identificare che sto elaborando le
054900100714       //?trattative IN CORSO
055000100714         *in01 = *off;
055100100714         *in02 = *off;
055200100714         *in03 = *on;
055300100714
055400100714       //?Elabora le trattative
055500100714         exsr sr_Elabora;
055600100714
055700100714       ENDSR;
055800100714
055900100714       //--------------------------------------------------------------
056000100714       //?Stampo i totali per unificante.
056100100714       //--------------------------------------------------------------
056200100714       BEGSR sr_StpTotUni;
056300100716
056400100716       //?Se non ho calcolato niente non stampo
056500100716         IF  %xfoot($Uni01Tr) = 0  and
056600100716             %xfoot($Uni02Tr) = 0  and
056700100716             %xfoot($Uni03Tr) = 0;
056800100716           leavesr;
056900100716         ENDIF;
057000100810
057100100810       //?Se dettaglio non richiesto non ho il commerciale nel campo di stampa
057200100810         IF  ITA98det = 'N';
057300100810           pCodUni = RCTrgf;
057400100810           pDesUni = RCTdrg;
057500100810         ENDIF;
057600100714
057700100714       //?Stampo la testata comune a tutti e 3 i totali
057800100714         *in01 = *off;
057900100714         *in02 = *off;
058000100714         *in03 = *off;
058100100714         write TA98PAGINA;
058200100714         wrkriga = '*' + wTrat + '*';
058300100714         write TA98SEPARA;
058400100714         write TA98TESTA;
058500100714         write TA98SEPARA;
058600100714
058700100714       //?Trattative AVVIATE
058800100715         IF  ITA98dtavi > 0;
058900100715           clear TA98TOTA;
059000100715           ptDtavI = pDtavI;
059100100715           ptDtavF = pDtavF;
059200100908           w60 = %xfoot($Uni01Tr);
059300100908           IF  w60 > 99900;
059400100908             pTotTr = *all'9';
059500100908           ELSE;
059600100908             pTotTr = w60;
059700100908           ENDIF;
059800110112           w60 = %xfoot($Uni01Tre);
059900110112           IF  w60 > 99900;
060000110112             pTotTre = *all'9';
060100110112           ELSE;
060200110112             pTotTre = w60;
060300110112           ENDIF;
060400100908           w120 = %xfoot($Uni01Fat);
060500100908           IF  w120 > 999999900;
060600100908             pTotFat = *all'9';
060700100908           ELSE;
060800100908             pTotFat = w120;
060900100908           ENDIF;
061000110112           w120 = %xfoot($Uni01Fate);
061100110112           IF  w120 > 999999900;
061200110112             pTotFate = *all'9';
061300110112           ELSE;
061400110112             pTotFate = w120;
061500110112           ENDIF;
061600100715           pTotSpe = %xfoot($Uni01Spe);
061700110112           pTotSpee = %xfoot($Uni01Spee);
061800100715           write TA98TOTA;
061900100714
062000100715         //?Di cui per tipo trattativa
062100100715           clear TA98TOTAD;
062200100715           ww = 1;
062300100715           FOR ww by 1 to %elem($Ttr);
062400100715             IF  $Ttr(ww) <> *blanks;
062500100715               pTtr     = %subst($Ttr(ww):3:1);
062600100715               pDesTtr  = %subst($Ttr(ww):4:25);
062700100908               w60      = $Uni01Tr(ww);
062800100908               IF  w60 > 99900;
062900100908                 pTotTrd = *all'9';
063000100908               ELSE;
063100100908                 pTotTrd = w60;
063200100908               ENDIF;
063300110112               w60      = $Uni01Tre(ww);
063400110112               IF  w60 > 99900;
063500110112                 pTotTrde = *all'9';
063600110112               ELSE;
063700110112                 pTotTrde = w60;
063800110112               ENDIF;
063900100908               w120     = $Uni01Fat(ww);
064000100908               IF  w120 > 999999900;
064100100908                 pTotFatd = *all'9';
064200100908               ELSE;
064300100908                 pTotFatd = w120;
064400100908               ENDIF;
064500110112               w120     = $Uni01Fate(ww);
064600110112               IF  w120 > 999999900;
064700110112                 pTotFatde = *all'9';
064800110112               ELSE;
064900110112                 pTotFatde = w120;
065000110112               ENDIF;
065100100715               pTotSped = $Uni01Spe(ww);
065200110112               pTotSpede = $Uni01Spee(ww);
065300100715               write TA98TOTAD;
065400100715               clear TA98TOTAD;
065500100715             ENDIF;
065600100715           ENDFOR;
065700100715         ENDIF;
065800100714
065900110112       //?Trattative CHIUSE
066000100715         IF  ITA98dtchi > 0;
066100110112         //?totale + Di cui POSITIVE
066200100715           clear TA98TOTC;
066300100715           ptDtchI = pDtchI;
066400100715           ptDtchF = pDtchF;
066500100908           w60 = %xfoot($Uni02Tr);
066600100908           IF  w60 > 99900;
066700100908             pTotTr = *all'9';
066800100908           ELSE;
066900100908             pTotTr = w60;
067000100908           ENDIF;
067100110112           w60 = %xfoot($Uni02Tre);
067200110112           IF  w60 > 99900;
067300110112             pTotTre = *all'9';
067400110112           ELSE;
067500110112             pTotTre = w60;
067600110112           ENDIF;
067700100908           w120 = %xfoot($Uni02Fat);
067800100908           IF  w120 > 999999900;
067900100908             pTotFat = *all'9';
068000100908           ELSE;
068100100908             pTotFat = w120;
068200100908           ENDIF;
068300110112           w120 = %xfoot($Uni02Fate);
068400110112           IF  w120 > 999999900;
068500110112             pTotFate = *all'9';
068600110112           ELSE;
068700110112             pTotFate = w120;
068800110112           ENDIF;
068900100715           pTotSpe  = %xfoot($Uni02Spe);
069000110112           pTotSpee = %xfoot($Uni02Spee);
069100100908           w60 = %xfoot($UniPos);
069200100908           IF  w60 > 99900;
069300100908             pTotPos = *all'9';
069400100908           ELSE;
069500100908             pTotPos = w60;
069600100908           ENDIF;
069700110112           w60 = %xfoot($UniPose);
069800110112           IF  w60 > 99900;
069900110112             pTotPose = *all'9';
070000110112           ELSE;
070100110112             pTotPose = w60;
070200110112           ENDIF;
070300100908           w120 = %xfoot($UniPosF);
070400100908           IF  w120 > 999999900;
070500100908             pTotPFat = *all'9';
070600100908           ELSE;
070700100908             pTotPFat = w120;
070800100908           ENDIF;
070900110112           w120 = %xfoot($UniPosFe);
071000110112           IF  w120 > 999999900;
071100110112             pTotPFate = *all'9';
071200110112           ELSE;
071300110112             pTotPFate = w120;
071400110112           ENDIF;
071500100715           pTotPSpe = %xfoot($UniPosS);
071600110112           pTotPSpee = %xfoot($UniPosSe);
071700110113           write TA98TOTC;
071800110112
071900110112         //?Di cui per tipo trattativa
072000110112           clear TA98TOTCD;
072100110112           ww = 1;
072200110112           FOR ww by 1 to %elem($Ttr);
072300110112             IF  $Ttr(ww) <> *blanks;
072400110112               pTtr      = %subst($Ttr(ww):3:1);
072500110112               pDesTtr   = %subst($Ttr(ww):4:25);
072600110112               w60       = $Uni02Tr(ww);
072700110112               IF  w60 > 99900;
072800110112                 pTotTrd = *all'9';
072900110112               ELSE;
073000110112                 pTotTrd = w60;
073100110112               ENDIF;
073200110112               w60       = $Uni02Tre(ww);
073300110112               IF  w60 > 99900;
073400110112                 pTotTrde = *all'9';
073500110112               ELSE;
073600110112                 pTotTrde = w60;
073700110112               ENDIF;
073800110112               w120      = $Uni02Fat(ww);
073900110112               IF  w120 > 999999900;
074000110112                 pTotFatd = *all'9';
074100110112               ELSE;
074200110112                 pTotFatd = w120;
074300110112               ENDIF;
074400110112               w120      = $Uni02Fate(ww);
074500110112               IF  w120 > 999999900;
074600110112                 pTotFatde = *all'9';
074700110112               ELSE;
074800110112                 pTotFatde = w120;
074900110112               ENDIF;
075000110112               pTotSped  = $Uni02Spe(ww);
075100110112               pTotSpede = $Uni02Spee(ww);
075200110112               w60       = $UniPos(ww);
075300110112               IF  w60 > 99900;
075400110112                 pTotPosd = *all'9';
075500110112               ELSE;
075600110112                 pTotPosd = w60;
075700110112               ENDIF;
075800110112               w60       = $UniPose(ww);
075900110112               IF  w60 > 99900;
076000110112                 pTotPosde = *all'9';
076100110112               ELSE;
076200110112                 pTotPosde = w60;
076300110112               ENDIF;
076400110112               w120      = $UniPosF(ww);
076500110112               IF  w120 > 999999900;
076600110112                 pTotPFatd = *all'9';
076700110112               ELSE;
076800110112                 pTotPFatd = w120;
076900110112               ENDIF;
077000110112               w120      = $UniPosFe(ww);
077100110112               IF  w120 > 999999900;
077200110112                 pTotPFatde = *all'9';
077300110112               ELSE;
077400110112                 pTotPFatde = w120;
077500110112               ENDIF;
077600110112               pTotPSped = $UniPosS(ww);
077700110112               pTotPSpede = $UniPosSe(ww);
077800110112               write TA98TOTCD;
077900110112               clear TA98TOTCD;
078000110112             ENDIF;
078100110112           ENDFOR;
078200110112
078300110112         //?di cui NEGATIVE
078400110112           clear TA98TOTC1;
078500110112           w60 = %xfoot($UniNeg);
078600110112           IF  w60 > 99900;
078700110112             pTotNeg = *all'9';
078800110112           ELSE;
078900110112             pTotNeg = w60;
079000110112           ENDIF;
079100110112           w60 = %xfoot($UniNege);
079200110112           IF  w60 > 99900;
079300110112             pTotNege = *all'9';
079400110112           ELSE;
079500110112             pTotNege = w60;
079600110112           ENDIF;
079700110112           w120 = %xfoot($UniNegF);
079800110112           IF  w120 > 999999900;
079900110112             pTotNFat = *all'9';
080000110112           ELSE;
080100110112             pTotNFat = w120;
080200110112           ENDIF;
080300110112           w120 = %xfoot($UniNegFe);
080400110112           IF  w120 > 999999900;
080500110112             pTotNFate = *all'9';
080600110112           ELSE;
080700110112             pTotNFate = w120;
080800110112           ENDIF;
080900110112           pTotNSpe = %xfoot($UniNegS);
081000110112           pTotNSpee = %xfoot($UniNegSe);
081100110112           write TA98TOTC1;
081200100714
081300100715         //?Di cui per tipo trattativa
081400110112           clear TA98TOTCD1;
081500100715           ww = 1;
081600100715           FOR ww by 1 to %elem($Ttr);
081700100715             IF  $Ttr(ww) <> *blanks;
081800110117               pTtr      = %subst($Ttr(ww):3:1);
081900110117               pDesTtr   = %subst($Ttr(ww):4:25);
082000100908               w60       = $UniNeg(ww);
082100100908               IF  w60 > 99900;
082200100908                 pTotNegd = *all'9';
082300100908               ELSE;
082400100908                 pTotNegd = w60;
082500100908               ENDIF;
082600110112               w60       = $UniNege(ww);
082700110112               IF  w60 > 99900;
082800110112                 pTotNegde = *all'9';
082900110112               ELSE;
083000110112                 pTotNegde = w60;
083100110112               ENDIF;
083200100908               w120      = $UniNegF(ww);
083300100908               IF  w120 > 999999900;
083400100908                 pTotNFatd = *all'9';
083500100908               ELSE;
083600100908                 pTotNFatd = w120;
083700100908               ENDIF;
083800110112               w120      = $UniNegFe(ww);
083900110112               IF  w120 > 999999900;
084000110112                 pTotNFatde = *all'9';
084100110112               ELSE;
084200110112                 pTotNFatde = w120;
084300110112               ENDIF;
084400100715               pTotNSped = $UniNegS(ww);
084500110112               pTotNSpede = $UniNegSe(ww);
084600110112               write TA98TOTCD1;
084700110112               clear TA98TOTCD1;
084800100715             ENDIF;
084900100715           ENDFOR;
085000100715         ENDIF;
085100100714
085200100714       //?Trattative IN CORSO
085300100715         IF  ITA98dtica > 0;
085400100715           clear TA98TOTX;
085500100715           ptDtica = pDtica;
085600100908           w60 = %xfoot($Uni03Tr);
085700100908           IF  w60 > 99900;
085800100908             pTotTr = *all'9';
085900100908           ELSE;
086000100908             pTotTr = w60;
086100100908           ENDIF;
086200110112           w60 = %xfoot($Uni03Tre);
086300110112           IF  w60 > 99900;
086400110112             pTotTre = *all'9';
086500110112           ELSE;
086600110112             pTotTre = w60;
086700110112           ENDIF;
086800100908           w120 = %xfoot($Uni03Fat);
086900100908           IF  w120 > 999999900;
087000100908             pTotFat = *all'9';
087100100908           ELSE;
087200100908             pTotFat = w120;
087300100908           ENDIF;
087400110112           w120 = %xfoot($Uni03Fate);
087500110112           IF  w120 > 999999900;
087600110112             pTotFate = *all'9';
087700110112           ELSE;
087800110112             pTotFate = w120;
087900110112           ENDIF;
088000100715           pTotSpe    = %xfoot($Uni03Spe);
088100110112           pTotSpee   = %xfoot($Uni03Spee);
088200100908           w60 = %xfoot($UniCaldo);
088300100908           IF  w60 > 99900;
088400100908             pTotCaldo = *all'9';
088500100908           ELSE;
088600100908             pTotCaldo = w60;
088700100908           ENDIF;
088800110112           w60 = %xfoot($UniCaldoe);
088900110112           IF  w60 > 99900;
089000110112             pTotCaldoe = *all'9';
089100110112           ELSE;
089200110112             pTotCaldoe = w60;
089300110112           ENDIF;
089400100908           w120 = %xfoot($UniCaldoF);
089500100908           IF  w120 > 999999900;
089600100908             pTotCalF = *all'9';
089700100908           ELSE;
089800100908             pTotCalF = w120;
089900100908           ENDIF;
090000110112           w120 = %xfoot($UniCaldoFe);
090100110112           IF  w120 > 999999900;
090200110112             pTotCalFe = *all'9';
090300110112           ELSE;
090400110112             pTotCalFe = w120;
090500110112           ENDIF;
090600100715           pTotCalS   = %xfoot($UniCaldoS);
090700110112           pTotCalSe  = %xfoot($UniCaldoSe);
090800100715           write TA98TOTX;
090900100714
091000100715         //?Di cui per tipo trattativa
091100100715           clear TA98TOTXD;
091200100715           ww = 1;
091300100715           FOR ww by 1 to %elem($Ttr);
091400100715             IF  $Ttr(ww) <> *blanks;
091500100715               pTtr       = %subst($Ttr(ww):3:1);
091600100715               pDesTtr    = %subst($Ttr(ww):4:25);
091700100908               w60        = $Uni03Tr(ww);
091800100908               IF  w60 > 99900;
091900100908                 pTotTrd = *all'9';
092000100908               ELSE;
092100100908                 pTotTrd = w60;
092200100908               ENDIF;
092300110112               w60        = $Uni03Tre(ww);
092400110112               IF  w60 > 99900;
092500110112                 pTotTrde = *all'9';
092600110112               ELSE;
092700110112                 pTotTrde = w60;
092800110112               ENDIF;
092900100908               w120       = $Uni03Fat(ww);
093000100908               IF  w120 > 999999900;
093100100908                 pTotFatd = *all'9';
093200100908               ELSE;
093300100908                 pTotFatd = w120;
093400100908               ENDIF;
093500110112               w120       = $Uni03Fate(ww);
093600110112               IF  w120 > 999999900;
093700110112                 pTotFatde = *all'9';
093800110112               ELSE;
093900110112                 pTotFatde = w120;
094000110112               ENDIF;
094100100715               pTotSped   = $Uni03Spe(ww);
094200110112               pTotSpede  = $Uni03Spee(ww);
094300100908               w60        = $UniCaldo(ww);
094400100908               IF  w60 > 99900;
094500100908                 pTotCaldod = *all'9';
094600100908               ELSE;
094700100908                 pTotCaldod = w60;
094800100908               ENDIF;
094900110112               w60        = $UniCaldoe(ww);
095000110112               IF  w60 > 99900;
095100110113                 pToCaldode = *all'9';
095200110112               ELSE;
095300110113                 pToCaldode = w60;
095400110112               ENDIF;
095500100908               w120       = $UniCaldoF(ww);
095600100908               IF  w120 > 999999900;
095700100908                 pTotCalFd = *all'9';
095800100908               ELSE;
095900100908                 pTotCalFd = w120;
096000100908               ENDIF;
096100110112               w120       = $UniCaldoFe(ww);
096200110112               IF  w120 > 999999900;
096300110112                 pTotCalFde = *all'9';
096400110112               ELSE;
096500110112                 pTotCalFde = w120;
096600110112               ENDIF;
096700100715               pTotCalSd  = $UniCaldoS(ww);
096800110112               pTotCalSde = $UniCaldoSe(ww);
096900100715               write TA98TOTXD;
097000100715               clear TA98TOTXD;
097100100715             ENDIF;
097200100715           ENDFOR;
097300100715         ENDIF;
097400100716
097500100716       ENDSR;
097600100716
097700100716       //--------------------------------------------------------------
097800100716       //?Sommo i totali per unificante.
097900100716       //--------------------------------------------------------------
098000100716       BEGSR sr_MemTotUni;
098100100716
098200100716       //?Imposto se commerciale INTERNO o ESTERNO
098300101006         IF  wFun = 'COMIN' or wFun = 'ASC' or wFun = 'SA';
098400100716           $Interno = *on;
098500100716           $Esterno = *off;
098600100716         ENDIF;
098700101006         IF  wFun <> 'COMIN' and wFun <> 'ASC' and wFun <> 'SA';
098800100716           $Interno = *off;
098900100716           $Esterno = *on;
099000100716         ENDIF;
099100100714
099200100714       //?Sommo i totali dei commerciali nelle sk dei totali generali
099300100714       //?divise per commerciali INTERNI ed ESTERNI
099400100714         ww = 1;
099500100714         FOR ww by 1 to %elem($Ttr);
099600100714           IF  $Ttr(ww) <> *blanks;
099700100714         //?commerciali INTERNI
099800100714             IF  $Interno;
099900100714               $TotI01Tr(ww)   += $Uni01Tr(ww);
100000100714               $TotI01Fat(ww)  += $Uni01Fat(ww);
100100100714               $TotI01Spe(ww)  += $Uni01Spe(ww);
100200100716               $TotI02Tr(ww)   += $Uni02Tr(ww);
100300100716               $TotI02Fat(ww)  += $Uni02Fat(ww);
100400100716               $TotI02Spe(ww)  += $Uni02Spe(ww);
100500100714               $TotIPos(ww)    += $UniPos(ww);
100600100714               $TotIPosF(ww)   += $UniPosF(ww);
100700100714               $TotIPosS(ww)   += $UniPosS(ww);
100800100714               $TotINeg(ww)    += $UniNeg(ww);
100900100714               $TotINegF(ww)   += $UniNegF(ww);
101000100714               $TotINegS(ww)   += $UniNegS(ww);
101100100714               $TotI03Tr(ww)   += $Uni03Tr(ww);
101200100714               $TotI03Fat(ww)  += $Uni03Fat(ww);
101300100714               $TotI03Spe(ww)  += $Uni03Spe(ww);
101400100714               $TotICaldo(ww)  += $UniCaldo(ww);
101500100714               $TotICaldoF(ww) += $UniCaldoF(ww);
101600100714               $TotICaldoS(ww) += $UniCaldoS(ww);
101700110112           //?di cui estero
101800110112               $TotI01Tre(ww)   += $Uni01Tre(ww);
101900110112               $TotI01Fate(ww)  += $Uni01Fate(ww);
102000110112               $TotI01Spee(ww)  += $Uni01Spee(ww);
102100110112               $TotI02Tre(ww)   += $Uni02Tre(ww);
102200110112               $TotI02Fate(ww)  += $Uni02Fate(ww);
102300110112               $TotI02Spee(ww)  += $Uni02Spee(ww);
102400110112               $TotIPose(ww)    += $UniPose(ww);
102500110112               $TotIPosFe(ww)   += $UniPosFe(ww);
102600110112               $TotIPosSe(ww)   += $UniPosSe(ww);
102700110112               $TotINege(ww)    += $UniNege(ww);
102800110112               $TotINegFe(ww)   += $UniNegFe(ww);
102900110112               $TotINegSe(ww)   += $UniNegSe(ww);
103000110112               $TotI03Tre(ww)   += $Uni03Tre(ww);
103100110112               $TotI03Fate(ww)  += $Uni03Fate(ww);
103200110112               $TotI03Spee(ww)  += $Uni03Spee(ww);
103300110112               $TotICaldoe(ww)  += $UniCaldoe(ww);
103400110112               $TotICaldoFe(ww) += $UniCaldoFe(ww);
103500110112               $TotICaldoSe(ww) += $UniCaldoSe(ww);
103600100714             ENDIF;
103700100714         //?commerciali ESTERNI
103800100714             IF  $Esterno;
103900100714               $TotE01Tr(ww)   += $Uni01Tr(ww);
104000100714               $TotE01Fat(ww)  += $Uni01Fat(ww);
104100100714               $TotE01Spe(ww)  += $Uni01Spe(ww);
104200100714               $TotE02Tr(ww)   += $Uni02Tr(ww);
104300100714               $TotE02Fat(ww)  += $Uni02Fat(ww);
104400100714               $TotE02Spe(ww)  += $Uni02Spe(ww);
104500100714               $TotEPos(ww)    += $UniPos(ww);
104600100714               $TotEPosF(ww)   += $UniPosF(ww);
104700100714               $TotEPosS(ww)   += $UniPosS(ww);
104800100714               $TotENeg(ww)    += $UniNeg(ww);
104900100714               $TotENegF(ww)   += $UniNegF(ww);
105000100714               $TotENegS(ww)   += $UniNegS(ww);
105100100714               $TotE03Tr(ww)   += $Uni03Tr(ww);
105200100714               $TotE03Fat(ww)  += $Uni03Fat(ww);
105300100714               $TotE03Spe(ww)  += $Uni03Spe(ww);
105400100714               $TotECaldo(ww)  += $UniCaldo(ww);
105500100714               $TotECaldoF(ww) += $UniCaldoF(ww);
105600100714               $TotECaldoS(ww) += $UniCaldoS(ww);
105700110112           //?di cui estero
105800110112               $TotE01Tre(ww)   += $Uni01Tre(ww);
105900110112               $TotE01Fate(ww)  += $Uni01Fate(ww);
106000110112               $TotE01Spee(ww)  += $Uni01Spee(ww);
106100110112               $TotE02Tre(ww)   += $Uni02Tre(ww);
106200110112               $TotE02Fate(ww)  += $Uni02Fate(ww);
106300110112               $TotE02Spee(ww)  += $Uni02Spee(ww);
106400110112               $TotEPose(ww)    += $UniPose(ww);
106500110112               $TotEPosFe(ww)   += $UniPosFe(ww);
106600110112               $TotEPosSe(ww)   += $UniPosSe(ww);
106700110112               $TotENege(ww)    += $UniNege(ww);
106800110112               $TotENegFe(ww)   += $UniNegFe(ww);
106900110112               $TotENegSe(ww)   += $UniNegSe(ww);
107000110112               $TotE03Tre(ww)   += $Uni03Tre(ww);
107100110112               $TotE03Fate(ww)  += $Uni03Fate(ww);
107200110112               $TotE03Spee(ww)  += $Uni03Spee(ww);
107300110112               $TotECaldoe(ww)  += $UniCaldoe(ww);
107400110112               $TotECaldoFe(ww) += $UniCaldoFe(ww);
107500110112               $TotECaldoSe(ww) += $UniCaldoSe(ww);
107600100714             ENDIF;
107700100714           ENDIF;
107800100714         ENDFOR;
107900100714
108000100714       //?Pulisco le schiere per Unificante
108100100714         clear $Uni01Tr;
108200100714         clear $Uni01Fat;
108300100714         clear $Uni01Spe;
108400100714         clear $Uni02Tr;
108500100714         clear $Uni02Fat;
108600100714         clear $Uni02Spe;
108700100714         clear $UniPos;
108800100714         clear $UniPosF;
108900100714         clear $UniPosS;
109000100714         clear $UniNeg;
109100100714         clear $UniNegF;
109200100714         clear $UniNegS;
109300100714         clear $Uni03Tr;
109400100714         clear $Uni03Fat;
109500100714         clear $Uni03Spe;
109600100714         clear $UniCaldo;
109700100714         clear $UniCaldoF;
109800100714         clear $UniCaldoS;
109900110112         clear $Uni01Tre;
110000110112         clear $Uni01Fate;
110100110112         clear $Uni01Spee;
110200110112         clear $Uni02Tre;
110300110112         clear $Uni02Fate;
110400110112         clear $Uni02Spee;
110500110112         clear $UniPose;
110600110112         clear $UniPosFe;
110700110112         clear $UniPosSe;
110800110112         clear $UniNege;
110900110112         clear $UniNegFe;
111000110112         clear $UniNegSe;
111100110112         clear $Uni03Tre;
111200110112         clear $Uni03Fate;
111300110112         clear $Uni03Spee;
111400110112         clear $UniCaldoe;
111500110112         clear $UniCaldoFe;
111600110112         clear $UniCaldoSe;
111700100714
111800100714       ENDSR;
111900100714
112000100714       //--------------------------------------------------------------
112100100714       //?Stampo i totali generali commerciali INTERNI.
112200100714       //--------------------------------------------------------------
112300100714       BEGSR sr_StpTotGenI;
112400100714
112500100714       //?Stampo la testata
112600100714         *in01 = *off;
112700100714         *in02 = *off;
112800100714         *in03 = *off;
112900100714         write TA98PAGINA;
113000100714         wrkriga = '*' + wTrat + '*';
113100100714         write TA98SEPARA;
113200100714         write TA98TESTAG;
113300100714         write TA98SEPARA;
113400100714
113500100714       //?Trattative AVVIATE
113600100715         IF  ITA98dtavi > 0;
113700100715           clear TA98TOTA;
113800100715           ptDtavI = pDtavI;
113900100715           ptDtavF = pDtavF;
114000100715           w60 = %xfoot($TotI01Tr);
114100100715           IF  w60 > 99900;
114200100715             pTotTr = *all'9';
114300100715           ELSE;
114400100715             pTotTr = w60;
114500100715           ENDIF;
114600110113           w60 = %xfoot($TotI01Tre);
114700110113           IF  w60 > 99900;
114800110113             pTotTre = *all'9';
114900110113           ELSE;
115000110113             pTotTre = w60;
115100110113           ENDIF;
115200100715           w120 = %xfoot($TotI01Fat);
115300100715           IF  w120 > 999999900;
115400100715             pTotFat = *all'9';
115500100715           ELSE;
115600100715             pTotFat = w120;
115700100715           ENDIF;
115800110113           w120 = %xfoot($TotI01Fate);
115900110113           IF  w120 > 999999900;
116000110113             pTotFate = *all'9';
116100110113           ELSE;
116200110113             pTotFate = w120;
116300110113           ENDIF;
116400100715           pTotSpe = %xfoot($TotI01Spe);
116500110113           pTotSpee = %xfoot($TotI01Spee);
116600100715           write TA98TOTA;
116700100714
116800100715         //?Di cui per tipo trattativa
116900100715           clear TA98TOTAD;
117000100715           ww = 1;
117100100715           FOR ww by 1 to %elem($Ttr);
117200100715             IF  $Ttr(ww) <> *blanks;
117300100715               pTtr     = %subst($Ttr(ww):3:1);
117400100715               pDesTtr  = %subst($Ttr(ww):4:25);
117500100908               w60      = $TotI01Tr(ww);
117600100908               IF  w60 > 99900;
117700100908                 pTotTrd = *all'9';
117800100908               ELSE;
117900100908                 pTotTrd = w60;
118000100908               ENDIF;
118100110113               w60      = $TotI01Tre(ww);
118200110113               IF  w60 > 99900;
118300110113                 pTotTrde = *all'9';
118400110113               ELSE;
118500110113                 pTotTrde = w60;
118600110113               ENDIF;
118700100908               w120     = $TotI01Fat(ww);
118800100908               IF  w120 > 999999900;
118900100908                 pTotFatd = *all'9';
119000100908               ELSE;
119100100908                 pTotFatd = w120;
119200100908               ENDIF;
119300110113               w120     = $TotI01Fate(ww);
119400110113               IF  w120 > 999999900;
119500110113                 pTotFatde = *all'9';
119600110113               ELSE;
119700110113                 pTotFatde = w120;
119800110113               ENDIF;
119900100715               pTotSped = $TotI01Spe(ww);
120000110113               pTotSpede = $TotI01Spee(ww);
120100100715               write TA98TOTAD;
120200100715               clear TA98TOTAD;
120300100715             ENDIF;
120400100715           ENDFOR;
120500100715         ENDIF;
120600100714
120700100714       //?Trattative CHIUSE
120800100715         IF  ITA98dtchi > 0;
120900110113         //?totale + Di cui POSITIVE
121000100715           clear TA98TOTC;
121100100715           ptDtchI = pDtchI;
121200100715           ptDtchF = pDtchF;
121300100715           w60 = %xfoot($TotI02Tr);
121400100715           IF  w60 > 99900;
121500100715             pTotTr = *all'9';
121600100715           ELSE;
121700100715             pTotTr = w60;
121800100715           ENDIF;
121900110113           w60 = %xfoot($TotI02Tre);
122000110113           IF  w60 > 99900;
122100110113             pTotTre = *all'9';
122200110113           ELSE;
122300110113             pTotTre = w60;
122400110113           ENDIF;
122500100715           w120 = %xfoot($TotI02Fat);
122600100715           IF  w120 > 999999900;
122700100715             pTotFat = *all'9';
122800100715           ELSE;
122900100715             pTotFat = w120;
123000100715           ENDIF;
123100110113           w120 = %xfoot($TotI02Fate);
123200110113           IF  w120 > 999999900;
123300110113             pTotFate = *all'9';
123400110113           ELSE;
123500110113             pTotFate = w120;
123600110113           ENDIF;
123700100715           pTotSpe  = %Xfoot($TotI02Spe);
123800110113           pTotSpee = %Xfoot($TotI02Spee);
123900100715           w60 = %xfoot($TotIPos);
124000100715           IF  w60 > 99900;
124100100715             pTotPos = *all'9';
124200100715           ELSE;
124300100715             pTotPos = w60;
124400100715           ENDIF;
124500110113           w60 = %xfoot($TotIPose);
124600110113           IF  w60 > 99900;
124700110113             pTotPose = *all'9';
124800110113           ELSE;
124900110113             pTotPose = w60;
125000110113           ENDIF;
125100100715           w120 = %xfoot($TotIPosF);
125200100715           IF  w120 > 999999900;
125300100715             pTotPFat = *all'9';
125400100715           ELSE;
125500100715             pTotPFat = w120;
125600100715           ENDIF;
125700110113           w120 = %xfoot($TotIPosFe);
125800110113           IF  w120 > 999999900;
125900110113             pTotPFate = *all'9';
126000110113           ELSE;
126100110113             pTotPFate = w120;
126200110113           ENDIF;
126300100715           pTotPSpe = %xfoot($TotIPosS);
126400110113           pTotPSpee = %xfoot($TotIPosSe);
126500100715           write TA98TOTC;
126600110113
126700110113         //?Di cui per tipo trattativa
126800110113           clear TA98TOTCD;
126900110113           ww = 1;
127000110113           FOR ww by 1 to %elem($Ttr);
127100110113             IF  $Ttr(ww) <> *blanks;
127200110113               pTtr      = %subst($Ttr(ww):3:1);
127300110113               pDesTtr   = %subst($Ttr(ww):4:25);
127400110113               w60       = $TotI02Tr(ww);
127500110113               IF  w60 > 99900;
127600110113                 pTotTrd = *all'9';
127700110113               ELSE;
127800110113                 pTotTrd = w60;
127900110113               ENDIF;
128000110113               w60       = $TotI02Tre(ww);
128100110113               IF  w60 > 99900;
128200110113                 pTotTrde = *all'9';
128300110113               ELSE;
128400110113                 pTotTrde = w60;
128500110113               ENDIF;
128600110113               w120      = $TotI02Fat(ww);
128700110113               IF  w120 > 999999900;
128800110113                 pTotFatd = *all'9';
128900110113               ELSE;
129000110113                 pTotFatd = w120;
129100110113               ENDIF;
129200110113               w120      = $TotI02Fate(ww);
129300110113               IF  w120 > 999999900;
129400110113                 pTotFatde = *all'9';
129500110113               ELSE;
129600110113                 pTotFatde = w120;
129700110113               ENDIF;
129800110113               pTotSped  = $TotI02Spe(ww);
129900110113               pTotSpede = $TotI02Spee(ww);
130000110113               w60       = $TotIPos(ww);
130100110113               IF  w60 > 99900;
130200110113                 pTotPosd = *all'9';
130300110113               ELSE;
130400110113                 pTotPosd = w60;
130500110113               ENDIF;
130600110113               w60       = $TotIPose(ww);
130700110113               IF  w60 > 99900;
130800110113                 pTotPosde = *all'9';
130900110113               ELSE;
131000110113                 pTotPosde = w60;
131100110113               ENDIF;
131200110113               w120      = $TotIPosF(ww);
131300110113               IF  w120 > 999999900;
131400110113                 pTotPFatd = *all'9';
131500110113               ELSE;
131600110113                 pTotPFatd = w120;
131700110113               ENDIF;
131800110113               w120      = $TotIPosFe(ww);
131900110113               IF  w120 > 999999900;
132000110113                 pTotPFatde = *all'9';
132100110113               ELSE;
132200110113                 pTotPFatde = w120;
132300110113               ENDIF;
132400110113               pTotPSped = $TotIPosS(ww);
132500110113               pTotPSpede = $TotIPosSe(ww);
132600110113               write TA98TOTCD;
132700110113               clear TA98TOTCD;
132800110113             ENDIF;
132900110113           ENDFOR;
133000110113
133100110113         //?di cui NEGATIVE
133200110113           clear TA98TOTC1;
133300110113           w60 = %xfoot($TotINeg);
133400110113           IF  w60 > 99900;
133500110113             pTotNeg = *all'9';
133600110113           ELSE;
133700110113             pTotNeg = w60;
133800110113           ENDIF;
133900110113           w60 = %xfoot($TotINege);
134000110113           IF  w60 > 99900;
134100110113             pTotNege = *all'9';
134200110113           ELSE;
134300110113             pTotNege = w60;
134400110113           ENDIF;
134500110113           w120 = %xfoot($TotINegF);
134600110113           IF  w120 > 999999900;
134700110113             pTotNFat = *all'9';
134800110113           ELSE;
134900110113             pTotNFat = w120;
135000110113           ENDIF;
135100110113           w120 = %xfoot($TotINegFe);
135200110113           IF  w120 > 999999900;
135300110113             pTotNFate = *all'9';
135400110113           ELSE;
135500110113             pTotNFate = w120;
135600110113           ENDIF;
135700110113           pTotNSpe = %xfoot($TotINegS);
135800110113           pTotNSpee = %xfoot($TotINegSe);
135900110113           write TA98TOTC1;
136000100714
136100100715         //?Di cui per tipo trattativa
136200110113           clear TA98TOTCD1;
136300100715           ww = 1;
136400100715           FOR ww by 1 to %elem($Ttr);
136500100715             IF  $Ttr(ww) <> *blanks;
136600110117               pTtr      = %subst($Ttr(ww):3:1);
136700110117               pDesTtr   = %subst($Ttr(ww):4:25);
136800100908               w60       = $TotINeg(ww);
136900100908               IF  w60 > 99900;
137000100908                 pTotNegd = *all'9';
137100100908               ELSE;
137200100908                 pTotNegd = w60;
137300100908               ENDIF;
137400110113               w60       = $TotINege(ww);
137500110113               IF  w60 > 99900;
137600110113                 pTotNegde = *all'9';
137700110113               ELSE;
137800110113                 pTotNegde = w60;
137900110113               ENDIF;
138000100908               w120      = $TotINegF(ww);
138100100908               IF  w120 > 999999900;
138200100908                 pTotNFatd = *all'9';
138300100908               ELSE;
138400100908                 pTotNFatd = w120;
138500100908               ENDIF;
138600110113               w120      = $TotINegFe(ww);
138700110113               IF  w120 > 999999900;
138800110113                 pTotNFatde = *all'9';
138900110113               ELSE;
139000110113                 pTotNFatde = w120;
139100110113               ENDIF;
139200100715               pTotNSped = $TotINegS(ww);
139300110113               pTotNSpede = $TotINegSe(ww);
139400110113               write TA98TOTCD1;
139500110113               clear TA98TOTCD1;
139600100715             ENDIF;
139700100715           ENDFOR;
139800100715         ENDIF;
139900100714
140000100714       //?Trattative IN CORSO
140100100715         IF  ITA98dtica > 0;
140200100715           clear TA98TOTX;
140300100715           ptDtica = pDticA;
140400100715           w60 = %xfoot($TotI03Tr);
140500100715           IF  w60 > 99900;
140600100715             pTotTr = *all'9';
140700100715           ELSE;
140800100715             pTotTr = w60;
140900100715           ENDIF;
141000110113           w60 = %xfoot($TotI03Tre);
141100110113           IF  w60 > 99900;
141200110113             pTotTre = *all'9';
141300110113           ELSE;
141400110113             pTotTre = w60;
141500110113           ENDIF;
141600100715           w120 = %xfoot($TotI03Fat);
141700100715           IF  w120 > 999999900;
141800100715             pTotFat = *all'9';
141900100715           ELSE;
142000100715             pTotFat = w120;
142100100715           ENDIF;
142200110113           w120 = %xfoot($TotI03Fate);
142300110113           IF  w120 > 999999900;
142400110113             pTotFate = *all'9';
142500110113           ELSE;
142600110113             pTotFate = w120;
142700110113           ENDIF;
142800100715           pTotSpe = %xfoot($TotI03Spe);
142900110113           pTotSpee = %xfoot($TotI03Spee);
143000100715           w60 = %xfoot($TotICaldo);
143100100715           IF  w60 > 99900;
143200100715             pTotCaldo = *all'9';
143300100715           ELSE;
143400100715             pTotCaldo = w60;
143500100715           ENDIF;
143600110113           w60 = %xfoot($TotICaldoe);
143700110113           IF  w60 > 99900;
143800110113             pTotCaldoe = *all'9';
143900110113           ELSE;
144000110113             pTotCaldoe = w60;
144100110113           ENDIF;
144200100715           w120 = %xfoot($TotICaldoF);
144300100715           IF  w120 > 999999900;
144400100715             pTotCalF = *all'9';
144500100715           ELSE;
144600100715             pTotCalF = w120;
144700100715           ENDIF;
144800110113           w120 = %xfoot($TotICaldoFe);
144900110113           IF  w120 > 999999900;
145000110113             pTotCalFe = *all'9';
145100110113           ELSE;
145200110113             pTotCalFe = w120;
145300110113           ENDIF;
145400100715           pTotCalS = %xfoot($TotICaldoS);
145500110113           pTotCalSe = %xfoot($TotICaldoSe);
145600100715           write TA98TOTX;
145700100714
145800100715         //?Di cui per tipo trattativa
145900100715           clear TA98TOTXD;
146000100715           ww = 1;
146100100715           FOR ww by 1 to %elem($Ttr);
146200100715             IF  $Ttr(ww) <> *blanks;
146300100715               pTtr       = %subst($Ttr(ww):3:1);
146400100715               pDesTtr    = %subst($Ttr(ww):4:25);
146500100908               w60        = $TotI03Tr(ww);
146600100908               IF  w60 > 99900;
146700100908                 pTotTrd = *all'9';
146800100908               ELSE;
146900100908                 pTotTrd = w60;
147000100908               ENDIF;
147100110113               w60        = $TotI03Tre(ww);
147200110113               IF  w60 > 99900;
147300110113                 pTotTrde = *all'9';
147400110113               ELSE;
147500110113                 pTotTrde = w60;
147600110113               ENDIF;
147700100908               w120       = $TotI03Fat(ww);
147800100908               IF  w120 > 999999900;
147900100908                 pTotFatd = *all'9';
148000100908               ELSE;
148100100908                 pTotFatd = w120;
148200100908               ENDIF;
148300110113               w120       = $TotI03Fate(ww);
148400110113               IF  w120 > 999999900;
148500110113                 pTotFatde = *all'9';
148600110113               ELSE;
148700110113                 pTotFatde = w120;
148800110113               ENDIF;
148900100715               pTotSped   = $TotI03Spe(ww);
149000110113               pTotSpede  = $TotI03Spee(ww);
149100100908               w60        = $TotICaldo(ww);
149200100908               IF  w60 > 99900;
149300100908                 pTotCaldod = *all'9';
149400100908               ELSE;
149500100908                 pTotCaldod = w60;
149600100908               ENDIF;
149700110113               w60        = $TotICaldoe(ww);
149800110113               IF  w60 > 99900;
149900110113                 pToCaldode = *all'9';
150000110113               ELSE;
150100110113                 pToCaldode = w60;
150200110113               ENDIF;
150300100908               w120       = $TotICaldoF(ww);
150400100908               IF  w120 > 999999900;
150500100908                 pTotCalFd = *all'9';
150600100908               ELSE;
150700100908                 pTotCalFd = w120;
150800100908               ENDIF;
150900110113               w120       = $TotICaldoFe(ww);
151000110113               IF  w120 > 999999900;
151100110113                 pTotCalFde = *all'9';
151200110113               ELSE;
151300110113                 pTotCalFde = w120;
151400110113               ENDIF;
151500100715               pTotCalSd  = $TotICaldoS(ww);
151600110113               pTotCalSde = $TotICaldoSe(ww);
151700100715               write TA98TOTXD;
151800100715               clear TA98TOTXD;
151900100715             ENDIF;
152000100715           ENDFOR;
152100100715         ENDIF;
152200100714
152300100714       ENDSR;
152400100714
152500100714       //--------------------------------------------------------------
152600100714       //?Stampo i totali generali commerciali ESTERNI.
152700100714       //--------------------------------------------------------------
152800100714       BEGSR sr_StpTotGenE;
152900100714
153000100714       //?Stampo la testata
153100100714         *in01 = *off;
153200100714         *in02 = *off;
153300100714         *in03 = *off;
153400100714         write TA98PAGINA;
153500100714         wrkriga = '*' + wTrat + '*';
153600100714         write TA98SEPARA;
153700100714         write TA98TESTAG;
153800100714         write TA98SEPARA;
153900100714
154000100714       //?Trattative AVVIATE
154100100715         IF  ITA98dtavi > 0;
154200100715           clear TA98TOTA;
154300100715           ptDtavI = pDtavI;
154400100715           ptDtavF = pDtavF;
154500100715           w60 = %xfoot($TotE01Tr);
154600100715           IF  w60 > 99900;
154700100715             pTotTr = *all'9';
154800100715           ELSE;
154900100715             pTotTr = w60;
155000100715           ENDIF;
155100110113           w60 = %xfoot($TotE01Tre);
155200110113           IF  w60 > 99900;
155300110113             pTotTre = *all'9';
155400110113           ELSE;
155500110113             pTotTre = w60;
155600110113           ENDIF;
155700100715           w120 = %xfoot($TotE01Fat);
155800100715           IF  w120 > 999999900;
155900100715             pTotFat = *all'9';
156000100715           ELSE;
156100100715             pTotFat = w120;
156200100715           ENDIF;
156300110113           w120 = %xfoot($TotE01Fate);
156400110113           IF  w120 > 999999900;
156500110113             pTotFate = *all'9';
156600110113           ELSE;
156700110113             pTotFate = w120;
156800110113           ENDIF;
156900100715           pTotSpe = %xfoot($TotE01Spe);
157000110113           pTotSpee = %xfoot($TotE01Spee);
157100100715           write TA98TOTA;
157200100714
157300100715         //?Di cui per tipo trattativa
157400100715           clear TA98TOTAD;
157500100715           ww = 1;
157600100715           FOR ww by 1 to %elem($Ttr);
157700100715             IF  $Ttr(ww) <> *blanks;
157800100715               pTtr     = %subst($Ttr(ww):3:1);
157900100715               pDesTtr  = %subst($Ttr(ww):4:25);
158000100908               w60      = $TotE01Tr(ww);
158100100908               IF  w60 > 99900;
158200100908                 pTotTrd = *all'9';
158300100908               ELSE;
158400100908                 pTotTrd = w60;
158500100908               ENDIF;
158600110113               w60      = $TotE01Tre(ww);
158700110113               IF  w60 > 99900;
158800110113                 pTotTrde = *all'9';
158900110113               ELSE;
159000110113                 pTotTrde = w60;
159100110113               ENDIF;
159200100908               w120     = $TotE01Fat(ww);
159300100908               IF  w120 > 999999900;
159400100908                 pTotFatd = *all'9';
159500100908               ELSE;
159600100908                 pTotFatd = w120;
159700100908               ENDIF;
159800110113               w120     = $TotE01Fate(ww);
159900110113               IF  w120 > 999999900;
160000110113                 pTotFatde = *all'9';
160100110113               ELSE;
160200110113                 pTotFatde = w120;
160300110113               ENDIF;
160400100715               pTotSped = $TotE01Spe(ww);
160500110113               pTotSpede = $TotE01Spee(ww);
160600100715               write TA98TOTAD;
160700100715               clear TA98TOTAD;
160800100715             ENDIF;
160900100715           ENDFOR;
161000100715         ENDIF;
161100100714
161200100714       //?Trattative CHIUSE
161300100715         IF  ITA98dtchi > 0;
161400110113         //?totale + Di cui POSITIVE
161500100715           clear TA98TOTC;
161600100715           ptDtchI = pDtchI;
161700100715           ptDtchF = pDtchF;
161800100715           w60 = %xfoot($TotE02Tr);
161900100715           IF  w60 > 99900;
162000100715             pTotTr = *all'9';
162100100715           ELSE;
162200100715             pTotTr = w60;
162300100715           ENDIF;
162400110113           w60 = %xfoot($TotE02Tre);
162500110113           IF  w60 > 99900;
162600110113             pTotTre = *all'9';
162700110113           ELSE;
162800110113             pTotTre = w60;
162900110113           ENDIF;
163000100715           w120 = %xfoot($TotE02Fat);
163100100715           IF  w120 > 999999900;
163200100715             pTotFat = *all'9';
163300100715           ELSE;
163400100715             pTotFat = w120;
163500100715           ENDIF;
163600110113           w120 = %xfoot($TotE02Fate);
163700110113           IF  w120 > 999999900;
163800110113             pTotFate = *all'9';
163900110113           ELSE;
164000110113             pTotFate = w120;
164100110113           ENDIF;
164200100715           pTotSpe = %Xfoot($TotE02Spe);
164300110113           pTotSpee = %Xfoot($TotE02Spee);
164400100715           w60 = %xfoot($TotEPos);
164500100715           IF  w60 > 99900;
164600100715             pTotPos = *all'9';
164700100715           ELSE;
164800100715             pTotPos = w60;
164900100715           ENDIF;
165000110113           w60 = %xfoot($TotEPose);
165100110113           IF  w60 > 99900;
165200110113             pTotPose = *all'9';
165300110113           ELSE;
165400110113             pTotPose = w60;
165500110113           ENDIF;
165600100715           w120 = %xfoot($TotEPosF);
165700100715           IF  w120 > 999999900;
165800100715             pTotPFat = *all'9';
165900100715           ELSE;
166000100715             pTotPFat = w120;
166100100715           ENDIF;
166200110113           w120 = %xfoot($TotEPosFe);
166300110113           IF  w120 > 999999900;
166400110113             pTotPFate = *all'9';
166500110113           ELSE;
166600110113             pTotPFate = w120;
166700110113           ENDIF;
166800100715           pTotPSpe = %xfoot($TotEPosS);
166900110113           pTotPSpee = %xfoot($TotEPosSe);
167000110113           write TA98TOTC;
167100110113
167200110113         //?Di cui per tipo trattativa
167300110113           clear TA98TOTCD;
167400110113           ww = 1;
167500110113           FOR ww by 1 to %elem($Ttr);
167600110113             IF  $Ttr(ww) <> *blanks;
167700110113               pTtr      = %subst($Ttr(ww):3:1);
167800110113               pDesTtr   = %subst($Ttr(ww):4:25);
167900110113               w60       = $TotE02Tr(ww);
168000110113               IF  w60 > 99900;
168100110113                 pTotTrd = *all'9';
168200110113               ELSE;
168300110113                 pTotTrd = w60;
168400110113               ENDIF;
168500110113               w60       = $TotE02Tre(ww);
168600110113               IF  w60 > 99900;
168700110113                 pTotTrde = *all'9';
168800110113               ELSE;
168900110113                 pTotTrde = w60;
169000110113               ENDIF;
169100110113               w120      = $TotE02Fat(ww);
169200110113               IF  w120 > 999999900;
169300110113                 pTotFatd = *all'9';
169400110113               ELSE;
169500110113                 pTotFatd = w120;
169600110113               ENDIF;
169700110113               w120      = $TotE02Fate(ww);
169800110113               IF  w120 > 999999900;
169900110113                 pTotFatde = *all'9';
170000110113               ELSE;
170100110113                 pTotFatde = w120;
170200110113               ENDIF;
170300110113               pTotSped  = $TotE02Spe(ww);
170400110113               pTotSpede = $TotE02Spee(ww);
170500110113               w60       = $TotEPos(ww);
170600110113               IF  w60 > 99900;
170700110113                 pTotPosd = *all'9';
170800110113               ELSE;
170900110113                 pTotPosd = w60;
171000110113               ENDIF;
171100110113               w60       = $TotEPose(ww);
171200110113               IF  w60 > 99900;
171300110113                 pTotPosde = *all'9';
171400110113               ELSE;
171500110113                 pTotPosde = w60;
171600110113               ENDIF;
171700110113               w120      = $TotEPosF(ww);
171800110113               IF  w120 > 999999900;
171900110113                 pTotPFatd = *all'9';
172000110113               ELSE;
172100110113                 pTotPFatd = w120;
172200110113               ENDIF;
172300110113               w120      = $TotEPosFe(ww);
172400110113               IF  w120 > 999999900;
172500110113                 pTotPFatde = *all'9';
172600110113               ELSE;
172700110113                 pTotPFatde = w120;
172800110113               ENDIF;
172900110113               pTotPSped = $TotEPosS(ww);
173000110113               pTotPSpede = $TotEPosSe(ww);
173100110113               write TA98TOTCD;
173200110113               clear TA98TOTCD;
173300110113             ENDIF;
173400110113           ENDFOR;
173500110113
173600110113         //?di cui NEGATIVE
173700110113           clear TA98TOTC1;
173800100715           w60 = %xfoot($TotENeg);
173900100715           IF  w60 > 99900;
174000100715             pTotNeg = *all'9';
174100100715           ELSE;
174200100715             pTotNeg = w60;
174300100715           ENDIF;
174400110113           w60 = %xfoot($TotENege);
174500110113           IF  w60 > 99900;
174600110113             pTotNege = *all'9';
174700110113           ELSE;
174800110113             pTotNege = w60;
174900110113           ENDIF;
175000100715           w120 = %xfoot($TotENegF);
175100100715           IF  w120 > 999999900;
175200100715             pTotNFat = *all'9';
175300100715           ELSE;
175400100715             pTotNFat = w120;
175500100715           ENDIF;
175600110113           w120 = %xfoot($TotENegFe);
175700110113           IF  w120 > 999999900;
175800110113             pTotNFate = *all'9';
175900110113           ELSE;
176000110113             pTotNFate = w120;
176100110113           ENDIF;
176200100715           pTotNSpe = %xfoot($TotENegS);
176300110113           pTotNSpee = %xfoot($TotENegSe);
176400110113           write TA98TOTC1;
176500100714
176600100715         //?Di cui per tipo trattativa
176700110113           clear TA98TOTCD1;
176800100715           ww = 1;
176900100715           FOR ww by 1 to %elem($Ttr);
177000100715             IF  $Ttr(ww) <> *blanks;
177100110117               pTtr      = %subst($Ttr(ww):3:1);
177200110117               pDesTtr   = %subst($Ttr(ww):4:25);
177300100908               w60       = $TotENeg(ww);
177400100908               IF  w60 > 99900;
177500100908                 pTotNegd = *all'9';
177600100908               ELSE;
177700100908                 pTotNegd = w60;
177800100908               ENDIF;
177900110113               w60       = $TotENege(ww);
178000110113               IF  w60 > 99900;
178100110113                 pTotNegde = *all'9';
178200110113               ELSE;
178300110113                 pTotNegde = w60;
178400110113               ENDIF;
178500100908               w120      = $TotENegF(ww);
178600100908               IF  w120 > 999999900;
178700100908                 pTotNFatd = *all'9';
178800100908               ELSE;
178900100908                 pTotNFatd = w120;
179000100908               ENDIF;
179100110113               w120      = $TotENegFe(ww);
179200110113               IF  w120 > 999999900;
179300110113                 pTotNFatde = *all'9';
179400110113               ELSE;
179500110113                 pTotNFatde = w120;
179600110113               ENDIF;
179700100715               pTotNSped = $TotENegS(ww);
179800110113               pTotNSpede = $TotENegSe(ww);
179900110113               write TA98TOTCD1;
180000110113               clear TA98TOTCD1;
180100100715             ENDIF;
180200100715           ENDFOR;
180300100715         ENDIF;
180400100714
180500100714       //?Trattative IN CORSO
180600100715         IF  ITA98dtica > 0;
180700100715           clear TA98TOTX;
180800100715           ptDtica = pDticA;
180900100715           w60 = %xfoot($TotE03Tr);
181000100715           IF  w60 > 99900;
181100100715             pTotTr = *all'9';
181200100715           ELSE;
181300100715             pTotTr = w60;
181400100715           ENDIF;
181500110113           w60 = %xfoot($TotE03Tre);
181600110113           IF  w60 > 99900;
181700110113             pTotTre = *all'9';
181800110113           ELSE;
181900110113             pTotTre = w60;
182000110113           ENDIF;
182100100715           w120 = %xfoot($TotE03Fat);
182200100715           IF  w120 > 999999900;
182300100715             pTotFat = *all'9';
182400100715           ELSE;
182500100715             pTotFat = w120;
182600100715           ENDIF;
182700110113           w120 = %xfoot($TotE03Fate);
182800110113           IF  w120 > 999999900;
182900110113             pTotFate = *all'9';
183000110113           ELSE;
183100110113             pTotFate = w120;
183200110113           ENDIF;
183300100715           pTotSpe = %xfoot($TotE03Spe);
183400110113           pTotSpee = %xfoot($TotE03Spee);
183500100715           w60 = %xfoot($TotECaldo);
183600100715           IF  w60 > 99900;
183700100715             pTotCaldo = *all'9';
183800100715           ELSE;
183900100715             pTotCaldo = w60;
184000100715           ENDIF;
184100110113           w60 = %xfoot($TotECaldoe);
184200110113           IF  w60 > 99900;
184300110113             pTotCaldoe = *all'9';
184400110113           ELSE;
184500110113             pTotCaldoe = w60;
184600110113           ENDIF;
184700100715           w120 = %xfoot($TotECaldoF);
184800100715           IF  w120 > 999999900;
184900100715             pTotCalF = *all'9';
185000100715           ELSE;
185100100715             pTotCalF = w120;
185200100715           ENDIF;
185300110113           w120 = %xfoot($TotECaldoFe);
185400110113           IF  w120 > 999999900;
185500110113             pTotCalFe = *all'9';
185600110113           ELSE;
185700110113             pTotCalFe = w120;
185800110113           ENDIF;
185900100715           pTotCalS = %xfoot($TotECaldoS);
186000110113           pTotCalSe = %xfoot($TotECaldoSe);
186100100715           write TA98TOTX;
186200100714
186300100715         //?Di cui per tipo trattativa
186400100715           clear TA98TOTXD;
186500100715           ww = 1;
186600100715           FOR ww by 1 to %elem($Ttr);
186700100715             IF  $Ttr(ww) <> *blanks;
186800100715               pTtr       = %subst($Ttr(ww):3:1);
186900100715               pDesTtr    = %subst($Ttr(ww):4:25);
187000100908               w60        = $TotE03Tr(ww);
187100100908               IF  w60 > 99900;
187200100908                 pTotTrd = *all'9';
187300100908               ELSE;
187400100908                 pTotTrd = w60;
187500100908               ENDIF;
187600110113               w60        = $TotE03Tre(ww);
187700110113               IF  w60 > 99900;
187800110113                 pTotTrde = *all'9';
187900110113               ELSE;
188000110113                 pTotTrde = w60;
188100110113               ENDIF;
188200100908               w120       = $TotE03Fat(ww);
188300100908               IF  w120 > 999999900;
188400100908                 pTotFatd = *all'9';
188500100908               ELSE;
188600100908                 pTotFatd = w120;
188700100908               ENDIF;
188800110113               w120       = $TotE03Fate(ww);
188900110113               IF  w120 > 999999900;
189000110113                 pTotFatde = *all'9';
189100110113               ELSE;
189200110113                 pTotFatde = w120;
189300110113               ENDIF;
189400100715               pTotSped   = $TotE03Spe(ww);
189500110113               pTotSpede  = $TotE03Spee(ww);
189600100908               w60        = $TotECaldo(ww);
189700100908               IF  w60 > 99900;
189800100908                 pTotCaldod = *all'9';
189900100908               ELSE;
190000100908                 pTotCaldod = w60;
190100100908               ENDIF;
190200110113               w60        = $TotECaldoe(ww);
190300110113               IF  w60 > 99900;
190400110113                 pToCaldode = *all'9';
190500110113               ELSE;
190600110113                 pToCaldode = w60;
190700110113               ENDIF;
190800100908               w120       = $TotECaldoF(ww);
190900100908               IF  w120 > 999999900;
191000100908                 pTotCalFd = *all'9';
191100100908               ELSE;
191200100908                 pTotCalFd = w120;
191300100908               ENDIF;
191400110113               w120       = $TotECaldoFe(ww);
191500110113               IF  w120 > 999999900;
191600110113                 pTotCalFde = *all'9';
191700110113               ELSE;
191800110113                 pTotCalFde = w120;
191900110113               ENDIF;
192000100715               pTotCalSd  = $TotECaldoS(ww);
192100110113               pTotCalSde = $TotECaldoSe(ww);
192200100715               write TA98TOTXD;
192300100715               clear TA98TOTXD;
192400100715             ENDIF;
192500100715           ENDFOR;
192600100715         ENDIF;
192700100714
192800100714       ENDSR;
192900100716
193000100716       //--------------------------------------------------------------
193100100716       //?Stampo i totali generali di TUTTI i commerciali.
193200100716       //--------------------------------------------------------------
193300100716       BEGSR sr_StpTotGen;
193400100716
193500100716       //?Stampo la testata
193600100716         *in01 = *off;
193700100716         *in02 = *off;
193800100716         *in03 = *off;
193900100716         write TA98PAGINA;
194000100716         wrkriga = '*' + wTrat + '*';
194100100716         write TA98SEPARA;
194200100716         write TA98TESTAG;
194300100716         write TA98SEPARA;
194400100716
194500100716       //?Trattative AVVIATE
194600100716         IF  ITA98dtavi > 0;
194700100716           clear TA98TOTA;
194800100716           ptDtavI = pDtavI;
194900100716           ptDtavF = pDtavF;
195000100716           w60 = %xfoot($TotI01Tr) + %xfoot($TotE01Tr);
195100100716           IF  w60 > 99900;
195200100716             pTotTr = *all'9';
195300100716           ELSE;
195400100716             pTotTr = w60;
195500100716           ENDIF;
195600110113           w60 = %xfoot($TotI01Tre) + %xfoot($TotE01Tre);
195700110113           IF  w60 > 99900;
195800110113             pTotTre = *all'9';
195900110113           ELSE;
196000110113             pTotTre = w60;
196100110113           ENDIF;
196200100716           w120 = %xfoot($TotI01Fat) + %xfoot($TotE01Fat);
196300100716           IF  w120 > 999999900;
196400100716             pTotFat = *all'9';
196500100716           ELSE;
196600100716             pTotFat = w120;
196700100716           ENDIF;
196800110113           w120 = %xfoot($TotI01Fate) + %xfoot($TotE01Fate);
196900110113           IF  w120 > 999999900;
197000110113             pTotFate = *all'9';
197100110113           ELSE;
197200110113             pTotFate = w120;
197300110113           ENDIF;
197400100716           pTotSpe = %xfoot($TotI01Spe) + %xfoot($TotE01Spe);
197500110113           pTotSpee = %xfoot($TotI01Spee) + %xfoot($TotE01Spee);
197600100716           write TA98TOTA;
197700100716
197800100716         //?Di cui per tipo trattativa
197900100716           clear TA98TOTAD;
198000100716           ww = 1;
198100100716           FOR ww by 1 to %elem($Ttr);
198200100716             IF  $Ttr(ww) <> *blanks;
198300100716               pTtr     = %subst($Ttr(ww):3:1);
198400100716               pDesTtr  = %subst($Ttr(ww):4:25);
198500100908               w60      = $TotI01Tr(ww) + $TotE01Tr(ww);
198600100908               IF  w60 > 99900;
198700100908                 pTotTrd = *all'9';
198800100908               ELSE;
198900100908                 pTotTrd = w60;
199000100908               ENDIF;
199100110113               w60      = $TotI01Tre(ww) + $TotE01Tre(ww);
199200110113               IF  w60 > 99900;
199300110113                 pTotTrde = *all'9';
199400110113               ELSE;
199500110113                 pTotTrde = w60;
199600110113               ENDIF;
199700100908               w120     = $TotI01Fat(ww) + $TotE01Fat(ww);
199800100908               IF  w120 > 999999900;
199900100908                 pTotFatd = *all'9';
200000100908               ELSE;
200100100908                 pTotFatd = w120;
200200100908               ENDIF;
200300110113               w120     = $TotI01Fate(ww) + $TotE01Fate(ww);
200400110113               IF  w120 > 999999900;
200500110113                 pTotFatde = *all'9';
200600110113               ELSE;
200700110113                 pTotFatde = w120;
200800110113               ENDIF;
200900100716               pTotSped = $TotI01Spe(ww) + $TotE01Spe(ww);
201000110113               pTotSpede = $TotI01Spee(ww) + $TotE01Spee(ww);
201100100716               write TA98TOTAD;
201200100716               clear TA98TOTAD;
201300100716             ENDIF;
201400100716           ENDFOR;
201500100716         ENDIF;
201600100716
201700100716       //?Trattative CHIUSE
201800100716         IF  ITA98dtchi > 0;
201900110113         //?totale + Di cui POSITIVE
202000100716           clear TA98TOTC;
202100100716           ptDtchI = pDtchI;
202200100716           ptDtchF = pDtchF;
202300100716           w60 = %xfoot($TotI02Tr) + %xfoot($TotE02Tr);
202400100716           IF  w60 > 99900;
202500100716             pTotTr = *all'9';
202600100716           ELSE;
202700100716             pTotTr = w60;
202800100716           ENDIF;
202900110113           w60 = %xfoot($TotI02Tre) + %xfoot($TotE02Tre);
203000110113           IF  w60 > 99900;
203100110113             pTotTre = *all'9';
203200110113           ELSE;
203300110113             pTotTre = w60;
203400110113           ENDIF;
203500100716           w120 = %xfoot($TotI02Fat) + %xfoot($TotE02Fat);
203600100716           IF  w120 > 999999900;
203700100716             pTotFat = *all'9';
203800100716           ELSE;
203900100716             pTotFat = w120;
204000100716           ENDIF;
204100110113           w120 = %xfoot($TotI02Fate) + %xfoot($TotE02Fate);
204200110113           IF  w120 > 999999900;
204300110113             pTotFate = *all'9';
204400110113           ELSE;
204500110113             pTotFate = w120;
204600110113           ENDIF;
204700100716           pTotSpe  = %xfoot($TotI02Spe) + %xfoot($TotE02Spe);
204800110113           pTotSpee = %xfoot($TotI02Spee) + %xfoot($TotE02Spee);
204900100716           w60 = %xfoot($TotIPos) + %xfoot($TotEPos);
205000100716           IF  w60 > 99900;
205100100716             pTotPos = *all'9';
205200100716           ELSE;
205300100716             pTotPos = w60;
205400100716           ENDIF;
205500110113           w60 = %xfoot($TotIPose) + %xfoot($TotEPose);
205600110113           IF  w60 > 99900;
205700110113             pTotPose = *all'9';
205800110113           ELSE;
205900110113             pTotPose = w60;
206000110113           ENDIF;
206100100716           w120 = %xfoot($TotIPosF) + %xfoot($TotEPosF);
206200100716           IF  w120 > 999999900;
206300100716             pTotPFat = *all'9';
206400100716           ELSE;
206500100716             pTotPFat = w120;
206600100716           ENDIF;
206700110113           w120 = %xfoot($TotIPosFe) + %xfoot($TotEPosFe);
206800110113           IF  w120 > 999999900;
206900110113             pTotPFate = *all'9';
207000110113           ELSE;
207100110113             pTotPFate = w120;
207200110113           ENDIF;
207300100716           pTotPSpe = %xfoot($TotIPosS) + %xfoot($TotEPosS);
207400110113           pTotPSpee = %xfoot($TotIPosSe) + %xfoot($TotEPosSe);
207500110113           write TA98TOTC;
207600110113
207700110113         //?Di cui per tipo trattativa
207800110113           clear TA98TOTCD;
207900110113           ww = 1;
208000110113           FOR ww by 1 to %elem($Ttr);
208100110113             IF  $Ttr(ww) <> *blanks;
208200110113               pTtr      = %subst($Ttr(ww):3:1);
208300110113               pDesTtr   = %subst($Ttr(ww):4:25);
208400110113               w60       = $TotI02Tr(ww)  + $TotE02Tr(ww);
208500110113               IF  w60 > 99900;
208600110113                 pTotTrd = *all'9';
208700110113               ELSE;
208800110113                 pTotTrd = w60;
208900110113               ENDIF;
209000110113               w60       = $TotI02Tre(ww)  + $TotE02Tre(ww);
209100110113               IF  w60 > 99900;
209200110113                 pTotTrde = *all'9';
209300110113               ELSE;
209400110113                 pTotTrde = w60;
209500110113               ENDIF;
209600110113               w120      = $TotI02Fat(ww) + $TotE02Fat(ww);
209700110113               IF  w120 > 999999900;
209800110113                 pTotFatd = *all'9';
209900110113               ELSE;
210000110113                 pTotFatd = w120;
210100110113               ENDIF;
210200110113               w120      = $TotI02Fate(ww) + $TotE02Fate(ww);
210300110113               IF  w120 > 999999900;
210400110113                 pTotFatde = *all'9';
210500110113               ELSE;
210600110113                 pTotFatde = w120;
210700110113               ENDIF;
210800110113               pTotSped  = $TotI02Spe(ww) + $TotE02Spe(ww);
210900110113               pTotSpede = $TotI02Spee(ww) + $TotE02Spee(ww);
211000110113               w60       = $TotIPos(ww)   + $TotEPos(ww);
211100110113               IF  w60 > 99900;
211200110113                 pTotPosd = *all'9';
211300110113               ELSE;
211400110113                 pTotPosd = w60;
211500110113               ENDIF;
211600110113               w60       = $TotIPose(ww)   + $TotEPose(ww);
211700110113               IF  w60 > 99900;
211800110113                 pTotPosde = *all'9';
211900110113               ELSE;
212000110113                 pTotPosde = w60;
212100110113               ENDIF;
212200110113               w120      = $TotIPosF(ww)  + $TotEPosF(ww);
212300110113               IF  w120 > 999999900;
212400110113                 pTotPFatd = *all'9';
212500110113               ELSE;
212600110113                 pTotPFatd = w120;
212700110113               ENDIF;
212800110113               w120      = $TotIPosFe(ww)  + $TotEPosFe(ww);
212900110113               IF  w120 > 999999900;
213000110113                 pTotPFatde = *all'9';
213100110113               ELSE;
213200110113                 pTotPFatde = w120;
213300110113               ENDIF;
213400110113               pTotPSped = $TotIPosS(ww)  + $TotEPosS(ww);
213500110113               pTotPSpede = $TotIPosSe(ww)  + $TotEPosSe(ww);
213600110113               write TA98TOTCD;
213700110113               clear TA98TOTCD;
213800110113             ENDIF;
213900110113           ENDFOR;
214000110113
214100110113         //?di cui NEGATIVE
214200110113           clear TA98TOTC1;
214300100716           w60 = %xfoot($TotINeg) + %xfoot($TotENeg);
214400100716           IF  w60 > 99900;
214500100716             pTotNeg = *all'9';
214600100716           ELSE;
214700100716             pTotNeg = w60;
214800100716           ENDIF;
214900110113           w60 = %xfoot($TotINege) + %xfoot($TotENege);
215000110113           IF  w60 > 99900;
215100110113             pTotNege = *all'9';
215200110113           ELSE;
215300110113             pTotNege = w60;
215400110113           ENDIF;
215500100716           w120 = %xfoot($TotINegF) + %xfoot($TotENegF);
215600100716           IF  w120 > 999999900;
215700100716             pTotNFat = *all'9';
215800100716           ELSE;
215900100716             pTotNFat = w120;
216000100716           ENDIF;
216100110113           w120 = %xfoot($TotINegFe) + %xfoot($TotENegFe);
216200110113           IF  w120 > 999999900;
216300110113             pTotNFate = *all'9';
216400110113           ELSE;
216500110113             pTotNFate = w120;
216600110113           ENDIF;
216700100716           pTotNSpe = %xfoot($TotINegS) + %xfoot($TotENegS);
216800110113           pTotNSpee = %xfoot($TotINegSe) + %xfoot($TotENegSe);
216900110113           write TA98TOTC1;
217000100716
217100100716         //?Di cui per tipo trattativa
217200110113           clear TA98TOTCD1;
217300100716           ww = 1;
217400100716           FOR ww by 1 to %elem($Ttr);
217500100716             IF  $Ttr(ww) <> *blanks;
217600110117               pTtr      = %subst($Ttr(ww):3:1);
217700110117               pDesTtr   = %subst($Ttr(ww):4:25);
217800100908               w60       = $TotINeg(ww)   + $TotENeg(ww);
217900100908               IF  w60 > 99900;
218000100908                 pTotNegd = *all'9';
218100100908               ELSE;
218200100908                 pTotNegd = w60;
218300100908               ENDIF;
218400110113               w60       = $TotINege(ww)   + $TotENege(ww);
218500110113               IF  w60 > 99900;
218600110113                 pTotNegde = *all'9';
218700110113               ELSE;
218800110113                 pTotNegde = w60;
218900110113               ENDIF;
219000100908               w120      = $TotINegF(ww)  + $TotENegF(ww);
219100100908               IF  w120 > 999999900;
219200100908                 pTotNFatd = *all'9';
219300100908               ELSE;
219400100908                 pTotNFatd = w120;
219500100908               ENDIF;
219600110113               w120      = $TotINegFe(ww)  + $TotENegFe(ww);
219700110113               IF  w120 > 999999900;
219800110113                 pTotNFatde = *all'9';
219900110113               ELSE;
220000110113                 pTotNFatde = w120;
220100110113               ENDIF;
220200100716               pTotNSped = $TotINegS(ww)  + $TotENegS(ww);
220300110113               pTotNSpede = $TotINegSe(ww)  + $TotENegSe(ww);
220400110113               write TA98TOTCD1;
220500110113               clear TA98TOTCD1;
220600100716             ENDIF;
220700100716           ENDFOR;
220800100716         ENDIF;
220900100716
221000100716       //?Trattative IN CORSO
221100100716         IF  ITA98dtica > 0;
221200100716           clear TA98TOTX;
221300100716           ptDtica = pDticA;
221400100716           w60 = %xfoot($TotI03Tr) + %xfoot($TotE03Tr);
221500100716           IF  w60 > 99900;
221600100716             pTotTr = *all'9';
221700100716           ELSE;
221800100716             pTotTr = w60;
221900100716           ENDIF;
222000110113           w60 = %xfoot($TotI03Tre) + %xfoot($TotE03Tre);
222100110113           IF  w60 > 99900;
222200110113             pTotTre = *all'9';
222300110113           ELSE;
222400110113             pTotTre = w60;
222500110113           ENDIF;
222600100716           w120 = %xfoot($TotI03Fat) + %xfoot($TotE03Fat);
222700100716           IF  w120 > 999999900;
222800100716             pTotFat = *all'9';
222900100716           ELSE;
223000100716             pTotFat = w120;
223100100716           ENDIF;
223200110113           w120 = %xfoot($TotI03Fate) + %xfoot($TotE03Fate);
223300110113           IF  w120 > 999999900;
223400110113             pTotFate = *all'9';
223500110113           ELSE;
223600110113             pTotFate = w120;
223700110113           ENDIF;
223800100716           pTotSpe = %xfoot($TotI03Spe) + %xfoot($TotE03Spe);
223900110113           pTotSpee = %xfoot($TotI03Spee) + %xfoot($TotE03Spee);
224000100716           w60 = %xfoot($TotICaldo) + %xfoot($TotECaldo);
224100100716           IF  w60 > 99900;
224200100716             pTotCaldo = *all'9';
224300100716           ELSE;
224400100716             pTotCaldo = w60;
224500100716           ENDIF;
224600110113           w60 = %xfoot($TotICaldoe) + %xfoot($TotECaldoe);
224700110113           IF  w60 > 99900;
224800110113             pTotCaldoe = *all'9';
224900110113           ELSE;
225000110113             pTotCaldoe = w60;
225100110113           ENDIF;
225200100716           w120 = %xfoot($TotICaldoF) + %xfoot($TotECaldoF);
225300100716           IF  w120 > 999999900;
225400100716             pTotCalF = *all'9';
225500100716           ELSE;
225600100716             pTotCalF = w120;
225700100716           ENDIF;
225800110113           w120 = %xfoot($TotICaldoFe) + %xfoot($TotECaldoFe);
225900110113           IF  w120 > 999999900;
226000110113             pTotCalFe = *all'9';
226100110113           ELSE;
226200110113             pTotCalFe = w120;
226300110113           ENDIF;
226400100716           pTotCalS = %xfoot($TotICaldoS) + %xfoot($TotECaldoS);
226500110113           pTotCalSe = %xfoot($TotICaldoSe) + %xfoot($TotECaldoSe);
226600100716           write TA98TOTX;
226700100716
226800100716         //?Di cui per tipo trattativa
226900100716           clear TA98TOTXD;
227000100716           ww = 1;
227100100716           FOR ww by 1 to %elem($Ttr);
227200100716             IF  $Ttr(ww) <> *blanks;
227300100716               pTtr       = %subst($Ttr(ww):3:1);
227400100716               pDesTtr    = %subst($Ttr(ww):4:25);
227500100908               w60        = $TotI03Tr(ww)   + $TotE03Tr(ww);
227600100908               IF  w60 > 99900;
227700100908                 pTotTrd = *all'9';
227800100908               ELSE;
227900100908                 pTotTrd = w60;
228000100908               ENDIF;
228100110113               w60        = $TotI03Tre(ww)   + $TotE03Tre(ww);
228200110113               IF  w60 > 99900;
228300110113                 pTotTrde = *all'9';
228400110113               ELSE;
228500110113                 pTotTrde = w60;
228600110113               ENDIF;
228700100908               w120       = $TotI03Fat(ww)  + $TotE03Fat(ww);
228800100908               IF  w120 > 999999900;
228900100908                 pTotFatd = *all'9';
229000100908               ELSE;
229100100908                 pTotFatd = w120;
229200100908               ENDIF;
229300110113               w120       = $TotI03Fate(ww)  + $TotE03Fate(ww);
229400110113               IF  w120 > 999999900;
229500110113                 pTotFatde = *all'9';
229600110113               ELSE;
229700110113                 pTotFatde = w120;
229800110113               ENDIF;
229900100716               pTotSped   = $TotI03Spe(ww)  + $TotE03Spe(ww);
230000110113               pTotSpede  = $TotI03Spee(ww)  + $TotE03Spee(ww);
230100100908               w60        = $TotICaldo(ww)  + $TotECaldo(ww);
230200100908               IF  w60 > 99900;
230300100908                 pTotCaldod = *all'9';
230400100908               ELSE;
230500100908                 pTotCaldod = w60;
230600100908               ENDIF;
230700110113               w60        = $TotICaldoe(ww)  + $TotECaldoe(ww);
230800110113               IF  w60 > 99900;
230900110113                 pToCaldode = *all'9';
231000110113               ELSE;
231100110113                 pToCaldode = w60;
231200110113               ENDIF;
231300100908               w120       = $TotICaldoF(ww) + $TotECaldoF(ww);
231400100908               IF  w120 > 999999900;
231500100908                 pTotCalFd = *all'9';
231600100908               ELSE;
231700100908                 pTotCalFd = w120;
231800100908               ENDIF;
231900110113               w120       = $TotICaldoFe(ww) + $TotECaldoFe(ww);
232000110113               IF  w120 > 999999900;
232100110113                 pTotCalFde = *all'9';
232200110113               ELSE;
232300110113                 pTotCalFde = w120;
232400110113               ENDIF;
232500100716               pTotCalSd  = $TotICaldoS(ww) + $TotECaldoS(ww);
232600110113               pTotCalSde = $TotICaldoSe(ww) + $TotECaldoSe(ww);
232700100716               write TA98TOTXD;
232800100716               clear TA98TOTXD;
232900100716             ENDIF;
233000100716           ENDFOR;
233100100716         ENDIF;
233200100716
233300100716       ENDSR;
233400100714
233500100714       //--------------------------------------------------------------
233600100714       //?Operazioni finali.
233700100714       //--------------------------------------------------------------
233800100714       BEGSR RoutEnd;
233900100714
234000100714         *inLR = *on;
234100100714         return;
234200100714
234300100714       ENDSR;
234400100714
234500100714       //--------------------------------------------------------------
234600100714       //?Elaboro le trattative.
234700100714       //--------------------------------------------------------------
234800100714       BEGSR sr_Elabora;
234900100714
235000100714       //?Stampo le trattative in ordine come da tabella TTR
235100100714         ww = 1;
235200100714         FOR ww by 1 to %elem($Ttr);
235300100714           IF  $Ttr(ww) <> *blanks;
235400100714             *in12 = *off;
235500110118             *in15 = *off;
235600100810             clear pCodTtr;
235700100810             $Ttrok = *off;
235800110118           //?Se NO info trattative non deve stampare importi
235900110118             IF %subst($Ttr(ww):29:1) = 'N';
236000100714               *in12 = *on;
236100100714             ENDIF;
236200110118           //?Se Tipo trattativa 'A' non deve stampare le spedizoni
236300110118             IF %subst($Ttr(ww):3:1) = 'A';
236400110118               *in15 = *on;
236500110118             ENDIF;
236600100714             exsr sr_StpTratta;
236700100714           //?Stampo i totali per tipo trattativa
236800100714           //?solo se richiesto il dettaglio
236900100714             IF  pTtrTr > 0 and ITA98det = 'S' and $TtrOk and $DetCmm;
237000100714
237100100714             //?Salto pagina
237200100714               IF  CurLine >= 61;
237300100714                 exsr sr_StpSalto;
237400100714                 *in99 = *off;
237500100714               ENDIF;
237600100714               wrkriga = '*' + wTrat + '*';
237700100714               write TA98SEPARA;
237800100714               write TA98TTR;
237900110222              // IF  pTtrTre > 0;
238000110112                 write TA98TTRE;
238100110222              // ENDIF;
238200100714               wrkriga = '*' + wTrat + '*';
238300100714               write TA98SEPARA;
238400100921             ENDIF;
238500100714
238600100908             //?Memorizzo i totali in sk x unificante
238700100921             SELECT;
238800100921               WHEN  *in01;
238900100921                 $Uni01Tr(ww)  += pTtrTr;
239000100921                 $Uni01Fat(ww) += pTtrFat;
239100100921                 $Uni01Spe(ww) += pTtrSpe;
239200110113                 $Uni01Tre(ww)  += pTtrTre;
239300110113                 $Uni01Fate(ww) += pTtrFate;
239400110113                 $Uni01Spee(ww) += pTtrSpee;
239500100921               WHEN  *in02;
239600100921                 $Uni02Tr(ww)  += pTtrTr;
239700100921                 $Uni02Fat(ww) += pTtrFat;
239800100921                 $Uni02Spe(ww) += pTtrSpe;
239900110113                 $Uni02Tre(ww)  += pTtrTre;
240000110113                 $Uni02Fate(ww) += pTtrFate;
240100110113                 $Uni02Spee(ww) += pTtrSpee;
240200100921               WHEN  *in03;
240300100921                 $Uni03Tr(ww)  += pTtrTr;
240400100921                 $Uni03Fat(ww) += pTtrFat;
240500100921                 $Uni03Spe(ww) += pTtrSpe;
240600110113                 $Uni03Tre(ww)  += pTtrTre;
240700110113                 $Uni03Fate(ww) += pTtrFate;
240800110113                 $Uni03Spee(ww) += pTtrSpee;
240900100921             ENDSL;
241000100714
241100100714             clear pTtrTr;
241200100714             clear pTtrFat;
241300100714             clear pTtrSpe;
241400110112             clear pTtrTre;
241500110112             clear pTtrFate;
241600110112             clear pTtrSpee;
241700100714           ENDIF;
241800100714         ENDFOR;
241900100714
242000100714       ENDSR;
242100100714
242200100714       //--------------------------------------------------------------
242300100714       //?Stampo le trattative.
242400100714       //--------------------------------------------------------------
242500100714       BEGSR sr_StpTratta;
242600100714
242700100714         $End = *off;
242800100715         clear wConta;
242900100714
243000100714         //?Imposto la stringa per SQL
243100100714         exsr  sr_PrepSQL;
243200100714
243300100714         //?Dichiarazione cursore
243400100714         exec sql
243500100714           prepare S1   from :wSQL;
243600100714         exec sql
243700100714           declare WRK  cursor for S1;
243800100714
243900100714         //?Apertura del cursore
244000100714         exec sql
244100100714           open WRK;
244200100714
244300100714         IF sqlcode < 0;
244400100714           $End = *on;
244500100714         ENDIF;
244600100714
244700100714         DOW not $End;
244800100714           exec sql
244900100714             fetch next from WRK into :wfrctds;
245000100714           IF sqlcod = 100 or sqlcod < 0;
245100100714             $End = *on;
245200100714             leave;
245300100714           ENDIF;
245400100714
245500100714         //?Controllo se commerciale da stampare
245600100714           exsr sr_CtrCmmOk;
245700100714
245800100714         //?Controllo se tipo trattativa da stampare
245900100714           exsr sr_CtrTtrOk;
246000100714
246100100714        //?Solo se richiesto il dettaglio
246200100714           IF  ITA98det = 'S' and $TtrOk and $DetCmm;
246300100714         //?Se mai stampata o è salto pagina
246400100714         //?faccio il salto pagina
246500100714             IF  wConta = 0 or CurLine >= 65;
246600100714             //?Stampo salto pagina
246700100714               exsr sr_StpSalto;
246800100714               wConta += 1;
246900100714               *in99 = *off;
247000100714             ENDIF;
247100100714           //?A cambio tipo trattativa
247200100715             IF  %subst($Ttr(ww):3:1) <> pCodTtr;
247300100714             //?Stampo separazione per tipo trattativa
247400100715               exsr sr_StpTipoTtr;
247500100715             ENDIF;
247600100714           ENDIF;
247700100714
247800100714         //?Stampo la trattative
247900100714           exsr sr_StpDettaglio;
248000100714         //?Stampo le INFO
248100100714           exsr sr_StpINFO;
248200100915         //?Stampo i concorrenti  +  codice cliente + vecchio convalidato
248300100714           exsr sr_StpConc;
248400100714         //?Stampo se cliente caldo
248500100714           exsr sr_StpCaldo;
248600100714         //?Stampo le attività scadute e la prossima da fare
248700100714           exsr sr_StpAttivita;
248800100714
248900100714         //?Sommo il nr. trattative
249000100714           PTtrTr += 1;
249100110112         //?Sommo il nr. trattative con traffico estero per 'N' e 'I'
249200110112           IF  (RCTttr = 'I' or RCTttr = 'N') and
249300110112                RCTfate > 0;
249400110112             PTtrTre += 1;
249500110112           ENDIF;
249600100714
249700100714         ENDDO;
249800100714
249900100714         exec sql
250000100714           close WRK;
250100100714
250200100714       ENDSR;
250300100714
250400100714       //--------------------------------------------------------------
250500100714       //?Preparazione stringa SQL.
250600100714       //--------------------------------------------------------------
250700100714       BEGSR sr_PrepSQL;
250800100714
250900100714         wSQL = 'select * from WFRCT00F +
251000100714                 where rctrgf = ' +
251100100714                 %editc($AgeU(zz):'X');
251200100714
251300100714       //?trattative avviate-chiuse-in corso
251400100714         IF  *in01;
251500100714           wSQL += ' and rcttrc = ''A''';
251600100714         ENDIF;
251700100714         IF  *in02;
251800100714           wSQL += ' and rcttrc = ''C''';
251900100714         ENDIF;
252000100714         IF  *in03;
252100100714           wSQL += ' and rcttrc = ''I''';
252200100714         ENDIF;
252300100714
252400100714       //?Solo il tipo trattativa che sto leggendo
252500100714         wSQL += ' and rctttr =  ''' + %subst($Ttr(ww):3:1) + '''';
252600100714
252700100714       //?ordinamento richiesto a video
252800100714         SELECT;
252900100714       //? ragione sociale
253000100714           WHEN  ITA98ord = 'R';
253100100715             wSQL += ' order by RCTrag, RCTnrv';
253200100714       //? data avvio
253300100714           WHEN  ITA98ord = 'D';
253400100715             wSQL += ' order by RCTdavt, RCTrag';
253500100714       //? valore traffico totale
253600100714           WHEN  ITA98ord = 'V';
253700100716             wSQL += ' order by RCTfatt desc, RCTrag';
253800100714         ENDSL;
253900100714
254000100714         wSQL += ' for fetch only';
254100100714
254200100714       ENDSR;
254300100714
254400100714       //--------------------------------------------------------------
254500100714       //?Controllo se commerciale da stampare.
254600100714       //--------------------------------------------------------------
254700100714       BEGSR sr_CtrCmmOk;
254800100714
254900100714         $DetCmm = *on;
255000100714
255100100714       //?Stampa solo dei commerciali Interni
255200101006         IF  ITA98detc = 'I' and RCTfun <> 'COMIN' and
255300101006             RCTfun <> 'ASC' and RCTfun <> 'SA';
255400100714           $DetCmm = *off;
255500100714         ENDIF;
255600100714
255700100714       //?Stampa solo dei commerciali Esterni
255800101006         IF  ITA98detc = 'E' and (RCTfun = 'COMIN' or
255900101006             RCTfun = 'ASC' or RCTfun = 'SA');
256000100714           $DetCmm = *off;
256100100714         ENDIF;
256200100715
256300100715       //?Salvo la funzione del commerciale
256400100715         wFun = RCTfun;
256500100714
256600100714       ENDSR;
256700100714
256800100714       //--------------------------------------------------------------
256900100714       //?Controllo se tipo trattativa da stampare.
257000100714       //--------------------------------------------------------------
257100100714       BEGSR Sr_CtrTtrOk;
257200100714
257300100714         $TtrOk = *on;
257400100714
257500100714         IF  ITA98ttrio = 'I' and
257600100714            RCTttr <> ITA98ttr1 and
257700100714            RCTttr <> ITA98ttr2 and
257800100714            RCTttr <> ITA98ttr3 and
257900110112            RCTttr <> ITA98ttr4 and
258000110112            RCTttr <> ITA98ttr5 and
258100110112            RCTttr <> ITA98ttr6;
258200100714           $TtrOk = *off;
258300100714         ENDIF;
258400100714
258500100714         IF  ITA98ttrio = 'O' and
258600100714            (RCTttr = ITA98ttr1 or
258700100714             RCTttr = ITA98ttr2 or
258800100714             RCTttr = ITA98ttr3 or
258900110112             RCTttr = ITA98ttr4 or
259000110112             RCTttr = ITA98ttr5 or
259100110112             RCTttr = ITA98ttr6);
259200100714           $TtrOk = *off;
259300100714         ENDIF;
259400100714
259500100714       ENDSR;
259600100714
259700100714       //--------------------------------------------------------------
259800100714       //?Stampo il salto pagina.
259900100714       //--------------------------------------------------------------
260000100714       BEGSR sr_StpSalto;
260100100714
260200100714         pCodUni = RCTrgf;
260300100714         pDesUni = RCTdrg;
260400100714
260500100714         //?Stampo la testata
260600100714         write TA98PAGINA;
260700100714         wrkriga = '*' + wTrat + '*';
260800100714         write TA98SEPARA;
260900100714         write TA98TESTA;
261000100714         write TA98SEPARA;
261100100714         //?Stampo le intestazioni delle colonne
261200100714         write TA98INTCOL;
261300100714
261400100714       ENDSR;
261500100714
261600100714       //--------------------------------------------------------------
261700100714       //?Stampo la separazione per tipo trattativa.
261800100714       //--------------------------------------------------------------
261900100714       BEGSR sr_StpTipoTtr;
262000100714
262100100714         pCodTtr = %subst($Ttr(ww):3:1);
262200100714         pDesTtr = %subst($Ttr(ww):4:25);
262300100714
262400100714       //?Salto pagina
262500100714         IF  CurLine >= 63;
262600100714           exsr  sr_StpSalto;
262700100714           *in99 = *off;
262800100714         ENDIF;
262900100714
263000100714         clear wrkriga;
263100100714         write TA98SEPARA;
263200100714         write TA98TESTTR;
263300100714         write TA98SEPARA;
263400100714
263500100714       ENDSR;
263600100714
263700100714       //--------------------------------------------------------------
263800100714       //?Stampo le trattative.
263900100714       //--------------------------------------------------------------
264000100714       BEGSR sr_StpDettaglio;
264100100714
264200100714         clear pKsc;
264300100714         clear pOfferta;
264400100714         clear pClv;
264500100714         clear pVisDat;
264600100714         clear pDesCmmi;
264700100714         clear pVisDch;
264800100714         clear pDesEsi;
264900120919         clear pNrapp;
265000120919         clear pAtcDco;
265100100714
265200100714         //?CHIODO 'A' e 'M' come tipo trattativa
265300110530         //?Se trattativa di tipo Aumento/Sconto, Mantenimento,
265400110530         //?Incremento o Restyling
265500100714         //?stampo il codice cliente
265600110530         IF  RCTttr = 'A' or RCTttr = 'M' or
265700110530             RCTttr = 'I' or RCTttr = 'R';
265800100714           pKsc = RCTksc;
265900100714         ENDIF;
266000100714
266100100714         //?stampo se c'è offerta
266200100714         IF  RCToff = 'S';
266300100714           pOfferta = 'OFF';
266400100714         ENDIF;
266500100714
266600100714         //?Importanza cliente/potenziale
266700100714         pClv = RCTtic;
266800100714
266900100714         //?Data apertura trattativa
267000100714         wDate_ISO = %date(RCTdavt:*iso);
267100100714         wDate_EUR = wDate_ISO;
267200100714         pVisDat   = %dec(wDate_EUR);
267300100714
267400100714         //?Commerciale interno
267500100714         pDesCmmi = RCTdcmmi;
267600100714
267700100714         //?Filiale gestione trattativa
267800100714         pFilCmm = RCTfgs;
267900100714
268000100714         //?Conto gli appuntamenti
268100100714         IF  RCtnrapp > 0;
268200100714           wDate_ISO = %date(RCTduapp:*iso);
268300100714           wDate_EUR = wDate_ISO;
268400100714           pAtcDco   = %dec(wDate_EUR);
268500100714           pNrApp    = RCTnrapp;
268600100714         ENDIF;
268700100714
268800100714         //?Data chiusura trattativa
268900100714         IF  RCTdch > 0;
269000100714           wDate_ISO = %date(RCTdch:*iso);
269100100714           wDate_EUR = wDate_ISO;
269200100714           pVisDch   = %dec(wDate_EUR);
269300100714         ENDIF;
269400100714
269500100714         //?Esito trattativa
269600100714         IF  RCTepn = 'P ';
269700100714           pDesEsi = 'POSITIVO';
269800100714         //?Se sto elaborando le trattative CHIUSE
269900100714         //?conto direttamente in sk per Unificante
270000100714           IF  *in02;
270100100714             $UniPos(ww) += 1;
270200100714             $UniPosF(ww) += RCTfatt;
270300100714             $UniPosS(ww) += RCTnspt;
270400110607           //?di cui estero
270500110607             IF  RCTfate > 0;
270600110607               $UniPose(ww) += 1;
270700110607               $UniPosFe(ww) += RCTfate;
270800110607               $UniPosSe(ww) += RCTnspe;
270900110607             ENDIF;
271000100714           ENDIF;
271100100714         ENDIF;
271200100714         IF  RCTepn = 'N ';
271300100714           pDesEsi = 'NEGATIVO';
271400100714           clear TIBS02ds;
271500100714           clear dCCO;
271600100714           T02mod = 'C';
271700100714           T02cod = 'CCO';
271800100714           T02ke1 = RCTesi;
271900100714           T02sif = KNSIF;
272000100714           TNTBE_RicercaControllo (kpjba : tibs02ds);
272100100714           IF  T02err = *blanks;
272200100714             dCCO = T02uni;
272300100714           ENDIF;
272400100714           pDesEsi = §CCOdes;
272500100714         //?Se sto elaborando le trattative CHIUSE
272600100714         //?conto direttamente in sk per Unificante
272700100714           IF  *in02;
272800100714             $UniNeg(ww) += 1;
272900100714             $UniNegF(ww) += RCTfatt;
273000100714             $UniNegS(ww) += RCTnspt;
273100110607           //?di cui estero
273200110607             IF  RCTfate > 0;
273300110607               $UniNege(ww) += 1;
273400110607               $UniNegFe(ww) += RCTfate;
273500110607               $UniNegSe(ww) += RCTnspe;
273600110607             ENDIF;
273700100714           ENDIF;
273800100714         ENDIF;
273900100714
274000100714         //?Se richiesto il dettaglio
274100100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
274200100714           //?Salto pagina
274300100716           IF  CurLine >= 61;
274400100714             exsr  sr_StpSalto;
274500100714             exsr  sr_StpTipoTtr;
274600100714             *in99 = *off;
274700100714           ENDIF;
274800100714
274900100714           write TA98RIGA;
275000100714         ENDIF;
275100100714
275200100714       ENDSR;
275300100714
275400100714       //--------------------------------------------------------------
275500100714       //?Stampo le INFO.
275600100714       //--------------------------------------------------------------
275700100714       BEGSR sr_StpINFO;
275800100714
275900100714         clear pFatTot;
276000100714         clear pFatEst;
276100100714         clear pSpeTot;
276200100714         clear pSpeEst;
276300110114         clear pAuSc;
276400100714         clear pDelta;
276500100714         clear pPkg;
276600100714
276700100714         //?Dati traffico/spedizioni totale
276800110113         IF  RCTfatt <> 0;
276900110113           PFatTot = 'Valore Trattativa: ' + %trim(%editc(RCTfatt:'K'));
277000110113           pSpeTot = 'Sped. Trattativa: ' +
277100110113                       %trim(%editc(RCTnspt:'2'));
277200100715           pTtrFat += RCTfatt;
277300100715           PTtrSpe += RCTnspt;
277400100715         ENDIF;
277500100714
277600100714         //?Dati traffico/spedizioni estere
277700100715         IF  RCTfate > 0;
277800110113           PFatEst = 'Di cui estero: ' + %trim(%editc(RCTfate:'2'));
277900110113           pSpeEst = 'Di cui estero: ' + %trim(%editc(RCTnspe:'2'));
278000110112           //?Se trattativa di tipo 'N' o 'I' sommo anche il traffico estero
278100110112           IF  RCTttr = 'N' or RCTttr = 'I';
278200110112             pTtrFate += RCTfate;
278300110112             PTtrSpee += RCTnspe;
278400110112           ENDIF;
278500100715         ENDIF;
278600110114
278700110114         //?Dati Aumento/Sconto
278800110114         IF RCTausc <> 0;
278900110114           pAuSc = 'Aum./Sc.: ' + %trim(%editc(RCTausc:'K')) + ' %';
279000110114         ENDIF;
279100100714
279200100715         //?Dati delta
279300100715         IF RCTdel <> *blanks;
279400100715           pDelta = 'Delta Previsto:';
279500110114           pDelta = %trim(pDelta) + ' ' + %trim(RCTdel) +
279600110114                    RCTdels + ' %';
279700100715         ENDIF;
279800100714
279900100714         //?Dati peso
280000100715         IF  RCTpkg > 0;
280100110113           pPkg = 'Peso medio Sped.: ' + %trim(%editc(RCTpkg:'Z')) +
280200100715                    ' Kg.';
280300100715         ENDIF;
280400100714
280500100714         //?Se richiesto il dettaglio
280600100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
280700100714           //?Salto pagina
280800100714           IF  CurLine >= 65;
280900100714             exsr sr_StpSalto;
281000100714             exsr sr_StpTipoTtr;
281100100714             *in99 = *off;
281200100714           ENDIF;
281300100714
281400100714           IF  pFatTot <> *blanks;
281500100714             write TA98INFO;
281600100714           ENDIF;
281700100714         ENDIF;
281800100714
281900100714       ENDSR;
282000100714
282100100714       //--------------------------------------------------------------
282200100714       //?Stampo i Concorrenti.
282300100714       //--------------------------------------------------------------
282400100714       BEGSR sr_StpConc;
282500100714
282600100714         clear pConc;
282700100714         clear wConc;
282800100714
282900100714         IF  RCTconc1 <> *blanks;
283000100714           pConc = 'Concorrenti:';
283100100714           wConc = RCTconc1;
283200100714           exsr sr_DecConc;
283300100714           pConc = %trim(pConc) + ' ' + §ITSdes;
283400100714         ENDIF;
283500100714
283600100714         IF  RCTconc2 <> *blanks;
283700100714           wConc = RCTconc2;
283800100714           exsr sr_DecConc;
283900100915         //?Aggiungo concorrente fino al massimo di 158 byte
284000100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
284100100714             Pconc = %trim(Pconc) + ', Altri...';
284200100714           ELSE;
284300100714             pConc = %trim(pConc) + ', ' + §ITSdes;
284400100714           ENDIF;
284500100714         ENDIF;
284600100714
284700100714         IF  RCTconc3 <> *blanks;
284800100714           wConc = RCTconc3;
284900100714           exsr sr_DecConc;
285000100915         //?Aggiungo concorrente fino al massimo di 158 byte
285100100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
285200100714             Pconc = %trim(Pconc) + ', Altri...';
285300100714           ELSE;
285400100714             pConc = %trim(pConc) + ', ' + §ITSdes;
285500100714           ENDIF;
285600100714         ENDIF;
285700100714
285800100714         IF  RCTconc4 <> *blanks;
285900100714           wConc = RCTconc4;
286000100714           exsr sr_DecConc;
286100100915         //?Aggiungo concorrente fino al massimo di 158 byte
286200100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
286300100714             Pconc = %trim(Pconc) + ', Altri...';
286400100714           ELSE;
286500100714             pConc = %trim(pConc) + ', ' + §ITSdes;
286600100714           ENDIF;
286700100714         ENDIF;
286800100714
286900100714         IF  RCTconc5 <> *blanks;
287000100714           wConc = RCTconc5;
287100100714           exsr sr_DecConc;
287200100915         //?Aggiungo concorrente fino al massimo di 158 byte
287300100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
287400100714             Pconc = %trim(Pconc) + ', Altri...';
287500100714           ELSE;
287600100714             pConc = %trim(pConc) + ', ' + §ITSdes;
287700100714           ENDIF;
287800100714         ENDIF;
287900100714
288000100714         IF  RCTconc6 <> *blanks;
288100100714           wConc = RCTconc6;
288200100714           exsr sr_DecConc;
288300100915         //?Aggiungo concorrente fino al massimo di 158 byte
288400100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
288500100714             Pconc = %trim(Pconc) + ', Altri...';
288600100714           ELSE;
288700100714             pConc = %trim(pConc) + ', ' + §ITSdes;
288800100714           ENDIF;
288900100714         ENDIF;
289000100714
289100100714         IF  RCTconc7 <> *blanks;
289200100714           wConc = RCTconc7;
289300100714           exsr sr_DecConc;
289400100915         //?Aggiungo concorrente fino al massimo di 158 byte
289500100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
289600100714             Pconc = %trim(Pconc) + ', Altri...';
289700100714           ELSE;
289800100714             pConc = %trim(pConc) + ', ' + §ITSdes;
289900100714           ENDIF;
290000100714         ENDIF;
290100100714
290200100714         IF  RCTconc8 <> *blanks;
290300100714           wConc = RCTconc8;
290400100714           exsr sr_DecConc;
290500100915         //?Aggiungo concorrente fino al massimo di 158 byte
290600100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
290700100714             Pconc = %trim(Pconc) + ', Altri...';
290800100714           ELSE;
290900100714             pConc = %trim(pConc) + ', ' + §ITSdes;
291000100714           ENDIF;
291100100714         ENDIF;
291200100714
291300100714         IF  RCTconc9 <> *blanks;
291400100714           wConc = RCTconc9;
291500100714           exsr sr_DecConc;
291600100915         //?Aggiungo concorrente fino al massimo di 158 byte
291700100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
291800100714             Pconc = %trim(Pconc) + ', Altri...';
291900100714           ELSE;
292000100714             pConc = %trim(pConc) + ', ' + §ITSdes;
292100100714           ENDIF;
292200100714         ENDIF;
292300100714
292400100714         IF  RCTconc10 <> *blanks;
292500100714           wConc = RCTconc10;
292600100714           exsr sr_DecConc;
292700100915         //?Aggiungo concorrente fino al massimo di 158 byte
292800100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
292900100714             Pconc = %trim(Pconc) + ', Altri...';
293000100714           ELSE;
293100100714             pConc = %trim(pConc) + ', ' + §ITSdes;
293200100714           ENDIF;
293300100714         ENDIF;
293400100915
293500100915         //?In caso di stampa trattative CHIUSA - NUOVA stampo
293600100915         //?il codice cliente + vecchio convalidato
293700100915         clear pKscNew;
293800100915         IF  *in02 and RCTttr = 'N' and RCTksc > 0;
293900100915           pKscNew = 'Codificato: ' +
294000100915                      %editc(RCTksc:'Z');
294100100915         ENDIF;
294200100714
294300100714         //?Se richiesto il dettaglio
294400100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
294500100714           //?Salto pagina
294600100714           IF  CurLine >= 65;
294700100714             exsr sr_StpSalto;
294800100714             exsr sr_StpTipoTtr;
294900100714             *in99 = *off;
295000100714           ENDIF;
295100100714
295200100714           IF  pConc <> *blanks;
295300100714             write TA98CONC;
295400100714           ENDIF;
295500100714         ENDIF;
295600100714
295700100714       ENDSR;
295800100714
295900100714       //--------------------------------------------------------------
296000100714       //?Stampo se cliente caldo.
296100100714       //--------------------------------------------------------------
296200100714       BEGSR sr_StpCaldo;
296300100714
296400100714       //?Se esiste offerta accettata imposto cliente caldo
296500100714         IF  RCTcaldo = 'S';
296600100714           wDate_ISO = %date(RCTdpaff:*iso);
296700100714           wDate_EUR = wDate_ISO;
296800100714           pVOFdpa   = %dec(wDate_EUR);
296900100714
297000100714         //?Se richiesto il dettaglio
297100100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
297200100714             //?Salto pagina
297300100714             IF  CurLine >= 65;
297400100714              exsr sr_StpSalto;
297500100714              exsr sr_StpTipoTtr;
297600100714               *in99 = *off;
297700100714             ENDIF;
297800100714
297900100714             write TA98CALDO;
298000100714           ENDIF;
298100100714
298200100714         //?Se sto elaborando le trattative IN CORSO
298300100714           IF  *in03;
298400100714         //?conto se caldo direttamente in sk per Unificante
298500100714             $UniCaldo(ww) += 1;
298600100714             $UniCaldoF(ww) += RCTfatt;
298700100714             $UniCaldoS(ww) += RCTnspt;
298800100714           ENDIF;
298900100714
299000100714         ENDIF;
299100100714
299200100714       ENDSR;
299300100714
299400100714       //--------------------------------------------------------------
299500100714       //?Stampo le attività scadute e la prossima da fare.
299600100714       //--------------------------------------------------------------
299700100714       BEGSR sr_StpAttivita;
299800100714
299900100714         *in10 = *off;
300000100714         *in11 = *off;
300100100714
300200100714       //?Attività scadute
300300100714         IF  RCTnrscad > 0;
300400100714           *in10 = *on;
300500100715           pNrAttSc = RCTnrscad;
300600100714         ELSE;
300700100714       //?Attività future
300800100714           IF  RCTtat <> *blanks;
300900100714             *in11 = *on;
301000100714             pTatNew = RCTtat;
301100100714             wDate_ISO = %date(RCTdad:*iso);
301200100714             wDate_EUR = wDate_ISO;
301300100714             pDadNew   = %dec(wDate_EUR);
301400100714             pHdaNew   = %dec(%subst(%editc(RCThda:'X'):1:4):4:0);
301500100714             pCauNew   = RCTcad;
301600100714         //?Decodifico la causale attività
301700100714             clear TIBS02ds;
301800100714             clear dCCO;
301900100714             T02mod = 'C';
302000100714             T02cod = 'CCO';
302100100714             T02ke1 = pCauNew;
302200100714             T02sif = KNSIF;
302300100714             TNTBE_RicercaControllo (kpjba : tibs02ds);
302400100714             IF  T02err = *blanks;
302500100714               dCCO = T02uni;
302600100714             ENDIF;
302700100714             pDCauNew = §CCOdes;
302800100714           ENDIF;
302900100714         ENDIF;
303000100714
303100100715       //?Se richiesto il dettaglio
303200100715       //?Stampo riga attività scadute e futura
303300100810         IF  ITA98det = 'S' and  (*in10 or *in11) and
303400100810             $TtrOk and $DetCmm;
303500100715         //?Salto pagina
303600100715           IF  CurLine >= 65;
303700100715             exsr sr_StpSalto;
303800100715             exsr sr_StpTipoTtr;
303900100715             *in99 = *off;
304000100715           ENDIF;
304100100715           write TA98ATTIV;
304200100714         ENDIF;
304300100714
304400100714       ENDSR;
304500100714
304600100714       //--------------------------------------------------------------
304700100714       //?Decodifico i concorrenti.
304800100714       //--------------------------------------------------------------
304900100714       BEGSR Sr_DecConc;
305000100714
305100100714         clear TIBS02ds;
305200100714         clear dITS;
305300100714         T02mod = 'C';
305400100714         T02cod = 'ITS';
305500100714         T02ke1 = '10';
305600100714         T02ke2 = wConc;
305700100714         T02sif = KNSIF;
305800100714         TNTBE_RicercaControllo (kpjba : tibs02ds);
305900100714         IF  T02err = *blanks;
306000100714           dITS = T02uni;
306100100714         ENDIF;
306200100714
306300100714       ENDSR;
306400100714
306500100714       //--------------------------------------------------------------
306600100714       //?Reperimento Dati del job (Utente/Operativi).
306700100714       //--------------------------------------------------------------
306800100714       BEGSR DatiJob;
306900100714
307000100714         in(E) §AzUte;
307100100714         if NOT %error;
307200100714           in(E) §DatiUte;
307300100714         endif;
307400100714         if %error or RSut = *blanks;
307500100714           clear TIBS34ds;
307600100714           tibs34r(tibs34ds);
307700100714           in §AzUte;
307800100714           in §DatiUte;
307900100714         endif;
308000100714
308100100714       //?Richiamo pgm di controllo autorizzazioni utente così mi torna
308200100714       //?il codice autorizzazione relativo alle tariffe clienti
308300100714       //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
308400100714         reset TNTAA1DS;
308500100714         ITAA1caut = '§utegtc';
308600100714         kpjbu     = TNTAA1DS;
308700100714         tntaa1c (kpjba);
308800100714         TNTAA1DS = kpjbu;
308900100714
309000100714       //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
309100110922         IF  OTAA1cabi = 'AZ'and DUTpou <> 046;
309200100714           OTAA1cabi = 'RA';
309300100714         ENDIF;
309400100714
309500100714       //?Carico filiali abilitate all'utente
309600100714       //?Non controllo se caricate o no le filiali
309700100714       //?tanto se non ho caricato nessuna filiale e la richiesta
309800100714       //?è per tutti i commerciali, non stampo niente
309900100714         clear TRUL31DS;
310000110119         clear TRUL31DS2;
310100110119         *in09 = *off;
310200100714         I31abi = OTAA1cabi;
310300100714         I31cdi = DUTdis;
310400100714         I31car = DUTare;
310500100714         I31cpo = DUTpou;
310600110119         trul31r (kpjba : trul31ds : trul31ds2);
310700110119         IF  car(2) > *zeros;
310800110119           *in09 = *on;
310900110119         ENDIF;
311000100714
311100100714       ENDSR;
311200100714
311300100714       //--------------------------------------------------------------
311400100714       //?Carico sk agenti unificanti.
311500100714       //--------------------------------------------------------------
311600100714       BEGSR sr_CarAgeUni;
311700100714
311800100714       //?Carico gli agenti unificanti delle filiali gestite dall'utente
311900100714         clear xx;
312000100714         clear $AGEU;
312100100714         $EndAge = *off;
312200100714
312300100714       //?Imposto la stringa per SQL
312400100714         wSQL = 'select TBLKEY +
312500100714                 from TABEL00F +
312600100714                 where TBLCOD = ''01'' and TBLFLG = '' '' and +
312700100714                       substr(TBLUNI, 26, 3) in (';
312800100714
312900100714         yy = 0;
313000100714         xx = 1;
313100100714         FOR xx by 1 to %elem(pog);
313200100714           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
313300100714             IF yy > 0;
313400100714               wSQL += ', ';
313500100714             ELSE;
313600100714               yy = 1;
313700100714             ENDIF;
313800100714             wSQL += '''';
313900100714             wSQL += pog(xx);
314000100714             wSQL += '''';
314100100714           ENDIF;
314200100714         ENDFOR;
314300100714
314400100714         wSQL += ') and TBLKEY = substr(TBLUNI, 26, 7) and +
314500110113                    substr(TBLUNI, 78, 1) = '' ''';
314600100714
314700100714       //?Controllo funzione commerciale da elaborare
314800100714         IF  ITA98elcmm = 'I';
314900101006           wSQL += ' and (substr(TBLUNI, 73, 5) = ''COMIN''' +
315000101006                   ' or substr(TBLUNI, 73, 5) = ''ASC  ''' +
315100101006                   ' or substr(TBLUNI, 73, 5) = ''SA   '')';
315200100714         ENDIF;
315300100714         IF  ITA98elcmm = 'E';
315400101006           wSQL += ' and substr(TBLUNI, 73, 5) <> ''COMIN''' +
315500101006                   ' and substr(TBLUNI, 73, 5) <> ''ASC  ''' +
315600101006                   ' and substr(TBLUNI, 73, 5) <> ''SA   ''';
315700100714         ENDIF;
315800100714
315900100714       //?Ordino per codice commerciale
316000100714         wSQL += ' order by TBLKEY +
316100100714                   for fetch only';
316200100714
316300100714       //?Dichiarazione cursore
316400100714         exec sql
316500100714           prepare A1   from :wSQL;
316600100714         exec sql
316700100714           declare AGEU cursor for A1;
316800100714
316900100714       //?Apertura del cursore
317000100714         exec sql
317100100714           open AGEU;
317200100714
317300100714         IF sqlcode < 0;
317400100714           $EndAge = *on;
317500100714         ENDIF;
317600100714
317700100714       //?Carico la schiera degli agenti unificanti
317800100714         yy = 0;
317900100714         DOW not $EndAge;
318000100714           exec sql
318100100714             fetch next from AGEU into :TBLKEY;
318200100714           IF sqlcod = 100 or sqlcod < 0;
318300100714             $EndAge = *on;
318400100714             leave;
318500100714           ENDIF;
318600100714
318700100714         yy += 1;
318800100714         $AGEU(yy) = %dec(%subst(TBLKEY:1:7):7:0);
318900100714
319000100714         ENDDO;
319100100714
319200100714         exec sql
319300100714           close AGEU;
319400100714
319500100714       ENDSR;
319600100714
319700100714      /end-free
