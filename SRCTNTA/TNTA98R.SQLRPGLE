000100080206      //--------------------------------------------------------------
000200100708      //?TNTA98R - Elenco trattative
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000130905
001100100527     fAZORG01L  if   e           k disk
001200130905     fAZCMM01L  if   e           k disk
001300100325     fTABEL00F  if   e           k disk
001400100329     fTNTBE01L  if   e           k disk
001500100713     fTNTA98P   o    e             printer infds(prtinfo)
001600100713     f                                     oflind(*in99)
001700100310
001800080206      //---------------------------------------------------------------
001900090406      //?Definizione costanti.
002000080206      //---------------------------------------------------------------
002100080207
002200100329      // - Tabella tipo trattativa
002300100329     d C_tab           c                   const('TTR')
002400080206
002500080206      //---------------------------------------------------------------
002600080206      //?Definizione schiere.
002700080206      //---------------------------------------------------------------
002800100714
002900100714      // - Agenti Unificanti
003000100714     d $AgeU           s              7  0 dim(3500)
003100100329
003200110118      // - Tipo trattative --> ordinamento(2)  + codice(1) +
003300110118      //                       descrizione(25) + info s/n(1)
003400110118     d $Ttr            s             29    dim(10)
003500100714
003600100714      // - Totali Generali commerciali ESTERNI
003700100714     d $TotECaldo      s              6  0 dim(10)
003800100714     d $TotECaldoF     s             12  0 dim(10)
003900180220     d $TotECaldoS     s              9  0 dim(10)
004000100714     d $TotENeg        s              6  0 dim(10)
004100100714     d $TotENegF       s             12  0 dim(10)
004200180220     d $TotENegS       s              9  0 dim(10)
004300100714     d $TotEPos        s              6  0 dim(10)
004400100714     d $TotEPosF       s             12  0 dim(10)
004500180220     d $TotEPosS       s              9  0 dim(10)
004600100714     d $TotE01Fat      s             12  0 dim(10)
004700180220     d $TotE01Spe      s              9  0 dim(10)
004800100714     d $TotE01Tr       s              6  0 dim(10)
004900100714     d $TotE02Fat      s             12  0 dim(10)
005000180220     d $TotE02Spe      s              9  0 dim(10)
005100100714     d $TotE02Tr       s              6  0 dim(10)
005200100714     d $TotE03Fat      s             12  0 dim(10)
005300180220     d $TotE03Spe      s              9  0 dim(10)
005400100714     d $TotE03Tr       s              6  0 dim(10)
005500110112      // - Totali Generali commerciali ESTERNI di cui estero
005600110112     d $TotECaldoe     s              6  0 dim(10)
005700110112     d $TotECaldoFe    s             12  0 dim(10)
005800180220     d $TotECaldoSe    s              9  0 dim(10)
005900110112     d $TotENege       s              6  0 dim(10)
006000110112     d $TotENegFe      s             12  0 dim(10)
006100180220     d $TotENegSe      s              9  0 dim(10)
006200110112     d $TotEPose       s              6  0 dim(10)
006300110112     d $TotEPosFe      s             12  0 dim(10)
006400180220     d $TotEPosSe      s              9  0 dim(10)
006500110112     d $TotE01Fate     s             12  0 dim(10)
006600180220     d $TotE01Spee     s              9  0 dim(10)
006700110112     d $TotE01Tre      s              6  0 dim(10)
006800110112     d $TotE02Fate     s             12  0 dim(10)
006900180220     d $TotE02Spee     s              9  0 dim(10)
007000110112     d $TotE02Tre      s              6  0 dim(10)
007100110112     d $TotE03Fate     s             12  0 dim(10)
007200180220     d $TotE03Spee     s              9  0 dim(10)
007300110112     d $TotE03Tre      s              6  0 dim(10)
007400100713
007500100714      // - Totali Generali commerciali INTERNI
007600100714     d $TotICaldo      s              6  0 dim(10)
007700100714     d $TotICaldoF     s             12  0 dim(10)
007800180220     d $TotICaldoS     s              9  0 dim(10)
007900100714     d $TotINeg        s              6  0 dim(10)
008000100714     d $TotINegF       s             12  0 dim(10)
008100180220     d $TotINegS       s              9  0 dim(10)
008200100714     d $TotIPos        s              6  0 dim(10)
008300100714     d $TotIPosF       s             12  0 dim(10)
008400180220     d $TotIPosS       s              9  0 dim(10)
008500100714     d $TotI01Fat      s             12  0 dim(10)
008600180220     d $TotI01Spe      s              9  0 dim(10)
008700100714     d $TotI01Tr       s              6  0 dim(10)
008800100714     d $TotI02Fat      s             12  0 dim(10)
008900180220     d $TotI02Spe      s              9  0 dim(10)
009000100714     d $TotI02Tr       s              6  0 dim(10)
009100100714     d $TotI03Fat      s             12  0 dim(10)
009200180220     d $TotI03Spe      s              9  0 dim(10)
009300100714     d $TotI03Tr       s              6  0 dim(10)
009400110112      // - Totali Generali commerciali INTERNI di cui estero
009500110112     d $TotICaldoe     s              6  0 dim(10)
009600110112     d $TotICaldoFe    s             12  0 dim(10)
009700180220     d $TotICaldoSe    s              9  0 dim(10)
009800110112     d $TotINege       s              6  0 dim(10)
009900110112     d $TotINegFe      s             12  0 dim(10)
010000180220     d $TotINegSe      s              9  0 dim(10)
010100110112     d $TotIPose       s              6  0 dim(10)
010200110112     d $TotIPosFe      s             12  0 dim(10)
010300180220     d $TotIPosSe      s              9  0 dim(10)
010400110112     d $TotI01Fate     s             12  0 dim(10)
010500180220     d $TotI01Spee     s              9  0 dim(10)
010600110112     d $TotI01Tre      s              6  0 dim(10)
010700110112     d $TotI02Fate     s             12  0 dim(10)
010800180220     d $TotI02Spee     s              9  0 dim(10)
010900110112     d $TotI02Tre      s              6  0 dim(10)
011000110112     d $TotI03Fate     s             12  0 dim(10)
011100180220     d $TotI03Spee     s              9  0 dim(10)
011200110112     d $TotI03Tre      s              6  0 dim(10)
011300100713
011400100713      // - Totali Unificante x tipo
011500100713     d $UniCaldo       s              6  0 dim(10)
011600100713     d $UniCaldoF      s             12  0 dim(10)
011700180220     d $UniCaldoS      s              9  0 dim(10)
011800100713     d $UniNeg         s              6  0 dim(10)
011900100713     d $UniNegF        s             12  0 dim(10)
012000180220     d $UniNegS        s              9  0 dim(10)
012100100713     d $UniPos         s              6  0 dim(10)
012200100713     d $UniPosF        s             12  0 dim(10)
012300180220     d $UniPosS        s              9  0 dim(10)
012400100713     d $Uni01Fat       s             12  0 dim(10)
012500180220     d $Uni01Spe       s              9  0 dim(10)
012600100713     d $Uni01Tr        s              6  0 dim(10)
012700100713     d $Uni02Fat       s             12  0 dim(10)
012800180220     d $Uni02Spe       s              9  0 dim(10)
012900100713     d $Uni02Tr        s              6  0 dim(10)
013000100713     d $Uni03Fat       s             12  0 dim(10)
013100180220     d $Uni03Spe       s              9  0 dim(10)
013200100713     d $Uni03Tr        s              6  0 dim(10)
013300110112      // - Totali Unificante x tipo di cui estero
013400110112     d $UniCaldoe      s              6  0 dim(10)
013500110112     d $UniCaldoFe     s             12  0 dim(10)
013600180220     d $UniCaldoSe     s              9  0 dim(10)
013700110112     d $UniNege        s              6  0 dim(10)
013800110112     d $UniNegFe       s             12  0 dim(10)
013900180220     d $UniNegSe       s              9  0 dim(10)
014000110112     d $UniPose        s              6  0 dim(10)
014100110112     d $UniPosFe       s             12  0 dim(10)
014200180220     d $UniPosSe       s              9  0 dim(10)
014300110112     d $Uni01Fate      s             12  0 dim(10)
014400180220     d $Uni01Spee      s              9  0 dim(10)
014500110112     d $Uni01Tre       s              6  0 dim(10)
014600110112     d $Uni02Fate      s             12  0 dim(10)
014700180220     d $Uni02Spee      s              9  0 dim(10)
014800110112     d $Uni02Tre       s              6  0 dim(10)
014900110112     d $Uni03Fate      s             12  0 dim(10)
015000180220     d $Uni03Spee      s              9  0 dim(10)
015100110112     d $Uni03Tre       s              6  0 dim(10)
015200080206
015300080206      //---------------------------------------------------------------
015400080206      //?Definizione aree dati.
015500080206      //---------------------------------------------------------------
015600080206
015700080206      // - Dati utente
015800080206     d §AzUte        e ds                  extname(AZUTE00F)
015900080206     d                                     dtaara
016000080206     d §DatiUte      e ds                  extname(dDatiUte)
016100080206     d                                     dtaara
016200080206
016300080206      //---------------------------------------------------------------
016400080206      //?Definizione strutture dati.
016500080206      //---------------------------------------------------------------
016600100310
016700100310     d PrtINFO         ds
016800100310     d  CurLine              367    368i 0
016900080206
017000080206      // - Status
017100080206     d Psds           sds
017200080206     d   SDSpgm          *proc
017300080206
017400080206      // - Parametri ricevuti
017500080206     d KPJBA         e ds
017600100524     d TNTA98DS      e ds
017700080206
017800100308      // - Ricerca/Controllo tabelle
017900100308     d TIBS02ds      e ds                  inz
018000100308
018100080206      // - Reperimento dati utente
018200100305     d TIBS34DS      e ds
018300100714
018400100714      // - controllo abilitazioni
018500100714     d TNTAA1DS      e ds                  inz
018600100714
018700100714      // - Carico Filiali abilitate all'utente
018800100714     d TRUL31DS      e ds
018900100714     d POG                    10    759    DIM(250)
019000110119      // - Carico Aree abilitate all'utente
019100110119     d TRUL31DS2     e ds
019200110119     d CAR                    22    171    DIM(50)
019300100325
019400100325      // - Tabella CCO = Causale contatto
019500100325     d dCCO          e ds                  inz
019600100308
019700100324      // - Tabella ITS = Sottotipi INFO trattativa
019800100324     d dITS          e ds                  inz
019900100329
020000100329      // - Tabella TTR = Tipo trattativa
020100100329     d dTTR          e ds                  inz
020200110119
020300110119      // - Tabella 05 = Codici Aree
020400110119     d ds05          e ds                  inz
020500100527
020600100527      // File di work
020700100527     d WFRCTDS       e ds                  extname(WFRCT00F)
020800090629
020900080206      //---------------------------------------------------------------
021000080206      //?Definizione variabili globali.
021100080206      //---------------------------------------------------------------
021200080206
021300080206      // - Flags booleani
021400100714     d $DetCmm         s               n   inz(*off)
021500100305     d $End            s               n   inz(*off)
021600100714     d $EndAge         s               n   inz(*off)
021700100714     d $Esterno        s               n   inz(*off)
021800100714     d $Interno        s               n   inz(*off)
021900100714     d $TtrOk          s               n   inz(*off)
022000100325
022100100325      // - Indici di schiera
022200100326     d xx              s              4  0 inz
022300100714     d yy              s              4  0 inz
022400100329     d ww              s              4  0 inz
022500100714     d zz              s              4  0 inz
022600080206
022700090806       // - Stringa SQL da eseguire
022800090806     d wSQL            s           2048    Varying        inz
022900080206
023000090401      // - Campi di comodo data
023100100305     d wDate_EUR       s               d   datfmt(*eur)
023200100305     d wDate_ISO       s               d   datfmt(*iso)
023300100305
023400100305      // - Campi di comodo
023500100713     d wConc           s                   like(RCTconc1)
023600100329     d wConta          s              2  0 inz
023700100715     d wFun            s                   like(RCTfun)
023800100329     d wOggi           s              8  0 inz
023900100305     d wTrat           s            196    inz(*all'-')
024000100603     d w120            s             12  0 inz
024100100603     d w60             s              6  0 inz
024200100325
024300090508      //---------------------------------------------------------------
024400090807      //?Definizione procedure esterne.
024500090508      //---------------------------------------------------------------
024600100120
024700110119      // - Filiali e aree abilitate
024800110119     d trul31r         pr                  extpgm('TRUL31R')
024900110119     d  kpjba                              likeds(KPJBA)
025000110119     d  trul31ds                           likeds(trul31ds)
025100110119     d  trul31ds2                          likeds(trul31ds2)
025200100120
025300100120      //---------------------------------------------------------------
025400100120      //?prototipi
025500100120      //---------------------------------------------------------------
025600090807
025700080626      /copy gaitrasrc/srcprotopr,tibs02r
025800080626      /copy gaitrasrc/srcprotopr,tibs34r
025900100714      /copy gaitrasrc/srcprotopr,tntaa1c
026000090807
026100080206      //---------------------------------------------------------------
026200080206      //?Definizione key-list.
026300080206      //---------------------------------------------------------------
026400100325
026500100325       // - File TABEL00F
026600100325     d k03tabel      e ds                  extname(TABEL00F:*key)
026700100325     d                                     prefix(k_)
026800100325     d                                     inz
026900080206
027000080206      //---------------------------------------------------------------
027100080206      //?Riepilogo indicatori.
027200080206      //---------------------------------------------------------------
027300100326
027400100713      // 01    : Condizione stampa trattative AVVIATE
027500100524      // 02    : Condizione stampa trattative CHIUSE
027600100524      // 03    : Condizione stampa trattative IN CORSO
027700110119      // 09    : Condizione stampa selezione per Area
027800100330      // 10    : Condiziona stampa attività scadute
027900100330      // 11    : Condiziona stampa prossima attività
028000100528      // 12    : Condiziona stampa totale importi nei totali x tipo tr.
028100100601      // 13    : Condizione stampa totale generale commerciali esterni
028200100716      // 14    : Condizione stampa totale generale commerciali interni
028300110118      // 15    : NON stampa Spedizioni nei totali x tipo tr. se 'A'
028400100326      // 99    : Salto pagina
028500090807
028600080206      //---------------------------------------------------------------
028700080206
028800080206      //---------------------------------------------------------------
028900080206      //?M A I N - L I N E
029000080206      //---------------------------------------------------------------
029100080206
029200080206     c     *Entry        plist
029300080206     c                   parm                    KPJBA
029400080206
029500080206      /free
029600080206
029700090807       //?Operazioni iniziali
029800080206       exsr RoutInz;
029900100524
030000100524       //?Stampo la pagina dei parametri di lancio
030100100524       write TA98PAR;
030200100714
030300100714       //?Ciclo per sk unificanti e stampo
030400100714       zz = 1;
030500100714       FOR zz by 1 to %elem($AgeU);
030600100714         IF  $AgeU(zz) <> *zeros;
030700100714
030800100714         //?Elaboro le trattative AVVIATE nel periodo elaborazione
030900100810           IF  ITA98dtavi <> 0;
031000100810             exsr sr_Avviate;
031100100810           ENDIF;
031200100714
031300100714         //?Elaboro le trattative CHIUSE nel periodo elaborazione
031400100810           IF  ITA98dtchi <> 0;
031500100810             exsr sr_Chiuse;
031600100810           ENDIF;
031700100714
031800100714         //?Elaboro le trattative IN CORSO nel periodo elaborazione
031900100810           IF  ITA98dtica <> 0;
032000100810            exsr sr_InCorso;
032100100810           ENDIF;
032200100714
032300100714         //?Stampo i totali per unificante
032400100716         //?solo se richiesto
032500100716           IF  ITA98cmmt = 'S';
032600100714             exsr sr_StpTotUni;
032700100714           ENDIF;
032800100716         //?Sommo i totali per unificante
032900100716           exsr sr_MemTotUni;
033000100714         ENDIF;
033100100714       ENDFOR;
033200100714
033300100714       //?Stampo i totali generali
033400100714       IF  ITA98cmm = 0
033500100714       //?e solo se richiesto
033600100714           and ITA98gent = 'S';
033700100714       //?la prima volta per commerciali interni
033800100715         IF  ITA98elcmm = 'I' or ITA98elcmm = *blanks;
033900100715           *in13 = *on;
034000100716           *in14 = *off;
034100100715           exsr sr_StpTotGenI;
034200100715         ENDIF;
034300100714       //?la seconda volta per commerciali esterni
034400100715         IF  ITA98elcmm = 'E' or ITA98elcmm = *blanks;
034500100715           *in13 = *off;
034600100716           *in14 = *on;
034700100715           exsr sr_StpTotGenE;
034800100715         ENDIF;
034900100716       //?la terza volta per TUTTI i commerciali
035000100716         IF  ITA98elcmm = *blanks;
035100100716           *in13 = *off;
035200100716           *in14 = *off;
035300100716           exsr sr_StpTotGen;
035400100716         ENDIF;
035500100714       ENDIF;
035600100714
035700100714       //?Stampo la fine
035800100714       write TA98FINE;
035900100714
036000100714       //?Operazioni finali
036100100714       exsr RoutEnd;
036200100714
036300100714       //--------------------------------------------------------------
036400100714       //?Operazioni iniziali.
036500100714       //--------------------------------------------------------------
036600100714       BEGSR RoutInz;
036700100714
036800100714         //?Imposto la ds con i dati della KPJBU
036900100714         TNTA98ds = kpjbu;
037000100714
037100100714         //?Reperimento dati job
037200100714         exsr DatiJob;
037300100714
037400100714         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
037500100714
037600100714         //?Imposto nome programma
037700100714         NomePgm = SDSpgm;
037800100714
037900100714         //?Imposto dati fissi
038000100714         k_tblkut = 1;
038100100714
038200100714         //?Imposto data e ora elaborazione
038300100714         wOggi     = %dec(%date());
038400100714         wDate_ISO = %date(wOggi:*ISO);
038500100714         wDate_EUR = wDate_ISO;
038600100714         DataEla   = %dec(wDate_EUR);
038700100714         OraEla    = %dec(%time());
038800110119
038900110119         //?Stampo l'area richiesta
039000110119         IF  ITA98car > 0;
039100110119           clear ds05;
039200110119           k_tblcod = '05';
039300110119           k_tblkey = %editc(ITA98car:'X');
039400110119           chain  %kds(K03tabel) TABEL00F;
039500110119           IF  %found( TABEL00F );
039600110119             ds05 = TBLuni;
039700110119           ENDIF;
039800110119           pDesCar = §05des;
039900110119         ENDIF;
040000110119
040100110119         //?Stampo che richieste tutte le aree
040200110119         IF  ITA98car = 0;
040300110119           pDesCar = 'Tutte';
040400110119         ENDIF;
040500100714
040600100714         //?Stampo l'unificante richiesto
040700100714         IF  ITA98cmm > 0;
040800130905           chain  (iTA98cmm)  AZCMM000;
040900130905           IF  %found( AZCMM01L );
041000130905             pDesCmm = CMMdes;
041100100714           ENDIF;
041200100714         ENDIF;
041300100714
041400100714         //?Stampo che richiesti tutti gli unificanti
041500100714         IF  ITA98cmm = 0;
041600100714           pDesCmm = 'Tutti';
041700100714         ENDIF;
041800100714
041900100714         //?Imposto data avvio
042000100714         IF  ITA98dtavi > 0;
042100100714           wDate_ISO = %date(ITA98dtavi:*iso);
042200100714           wDate_EUR = wDate_ISO;
042300100714           pDtavI    = %dec(wDate_EUR);
042400100714           wDate_ISO = %date(ITA98dtavf:*iso);
042500100714           wDate_EUR = wDate_ISO;
042600100714           pDtavF    = %dec(wDate_EUR);
042700100714         ENDIF;
042800100714
042900100714         //?Imposto data chiusura
043000100714         IF  ITA98dtchi > 0;
043100100714           wDate_ISO = %date(ITA98dtchi:*iso);
043200100714           wDate_EUR = wDate_ISO;
043300100714           pDtchI    = %dec(wDate_EUR);
043400100714           wDate_ISO = %date(ITA98dtchf:*iso);
043500100714           wDate_EUR = wDate_ISO;
043600100714           pDtchF    = %dec(wDate_EUR);
043700100714         ENDIF;
043800100714
043900100714         //?Imposto data in corso
044000100714         IF  ITA98dtica > 0;
044100100714           wDate_ISO = %date(ITA98dtica:*iso);
044200100714           wDate_EUR = wDate_ISO;
044300100714           pDticA    = %dec(wDate_EUR);
044400100714         ENDIF;
044500100714
044600100714         //?Imposto data presunto affidamento
044700100714         IF  ITA98dtaff > 0;
044800100714           wDate_ISO = %date(ITA98dtaff:*iso);
044900100714           wDate_EUR = wDate_ISO;
045000100714           pDtaff    = %dec(wDate_EUR);
045100100714         ENDIF;
045200110114
045300110114         //?Imposto il segno del Traffico totale DA/A
045400110114         pVlttsi = '+';
045500110114         IF  ITA98vltti < 0;
045600110114           pVlttsi = '-';
045700110114         ENDIF;
045800110114         pVlttsf = '+';
045900110114         IF  ITA98vlttf < 0;
046000110114           pVlttsf = '-';
046100110114         ENDIF;
046200110114
046300110114         //?Imposto il segno dell'Aumento/Sconto DA/A
046400110114         pAussi = '+';
046500110114         IF  ITA98ausi < 0;
046600110114           pAussi = '-';
046700110114         ENDIF;
046800110114         pAussf = '+';
046900110114         IF  ITA98ausf < 0;
047000110114           pAussf = '-';
047100110114         ENDIF;
047200100714
047300100714         //?Carico sk Tipi trattavia con relativo ordinamento
047400100714         //?e descrizione
047500100714         clear xx;
047600100714         setll C_tab TNTBE000;
047700100714         reade C_tab TNTBE000;
047800100714
047900100714         DOW not %eof(TNTBE01L);
048000100714           dTTR = TBEuni;
048100100714           IF  TBEatb = *blanks;
048200100714             xx += 1;
048300100714             $Ttr(xx) = %editc(§TTRord:'X') + %subst(TBEke1:1:1) +
048400110118                        §TTRdes + §TTRinfo;
048500100714           ENDIF;
048600100714           reade C_tab TNTBE000;
048700100714         ENDDO;
048800100714
048900100714         sorta $Ttr;
049000100714
049100100714         clear $AgeU;
049200100714
049300100714       //?Carico in sk l'unificante richiesto
049400100714         IF  ITA98cmm > 0;
049500100714           $AgeU(01) = ITA98cmm;
049600100714         ENDIF;
049700100714
049800100714       //?Carico gli unificanti delle filiali gestite dall'utente
049900100714         IF  ITA98cmm = 0;
050000100714           exsr sr_CarAgeUni;
050100100714           sorta $AgeU;
050200100714         ENDIF;
050300100714
050400100714       ENDSR;
050500100714
050600100714       //--------------------------------------------------------------
050700100714       //?Elaboro le trattative AVVIATE.
050800100714       //--------------------------------------------------------------
050900100714       BEGSR sr_Avviate;
051000100714
051100100714       //?Imposto flag per identificare che sto elaborando le
051200100714       //?trattative AVVIATE
051300100714         *in01 = *on;
051400100714         *in02 = *off;
051500100714         *in03 = *off;
051600100714
051700100714       //?Elabora le trattative
051800100714         exsr sr_Elabora;
051900100714
052000100714       ENDSR;
052100100714
052200100714       //--------------------------------------------------------------
052300100714       //?Elaboro le trattative CHIUSE.
052400100714       //--------------------------------------------------------------
052500100714       BEGSR sr_Chiuse;
052600100714
052700100714       //?Imposto flag per identificare che sto elaborando le
052800100714       //?trattative CHIUSE
052900100714         *in01 = *off;
053000100714         *in02 = *on;
053100100714         *in03 = *off;
053200100714
053300100714       //?Elabora le trattative
053400100714         exsr sr_Elabora;
053500100714
053600100714       ENDSR;
053700100714
053800100714       //--------------------------------------------------------------
053900100714       //?Elaboro le trattative IN CORSO.
054000100714       //--------------------------------------------------------------
054100100714       BEGSR sr_InCorso;
054200100714
054300100714       //?Imposto flag per identificare che sto elaborando le
054400100714       //?trattative IN CORSO
054500100714         *in01 = *off;
054600100714         *in02 = *off;
054700100714         *in03 = *on;
054800100714
054900100714       //?Elabora le trattative
055000100714         exsr sr_Elabora;
055100100714
055200100714       ENDSR;
055300100714
055400100714       //--------------------------------------------------------------
055500100714       //?Stampo i totali per unificante.
055600100714       //--------------------------------------------------------------
055700100714       BEGSR sr_StpTotUni;
055800100716
055900100716       //?Se non ho calcolato niente non stampo
056000100716         IF  %xfoot($Uni01Tr) = 0  and
056100100716             %xfoot($Uni02Tr) = 0  and
056200100716             %xfoot($Uni03Tr) = 0;
056300100716           leavesr;
056400100716         ENDIF;
056500100810
056600100810       //?Se dettaglio non richiesto non ho il commerciale nel campo di stampa
056700100810         IF  ITA98det = 'N';
056800100810           pCodUni = RCTrgf;
056900100810           pDesUni = RCTdrg;
057000100810         ENDIF;
057100100714
057200100714       //?Stampo la testata comune a tutti e 3 i totali
057300100714         *in01 = *off;
057400100714         *in02 = *off;
057500100714         *in03 = *off;
057600100714         write TA98PAGINA;
057700100714         wrkriga = '*' + wTrat + '*';
057800100714         write TA98SEPARA;
057900100714         write TA98TESTA;
058000100714         write TA98SEPARA;
058100100714
058200100714       //?Trattative AVVIATE
058300100715         IF  ITA98dtavi > 0;
058400100715           clear TA98TOTA;
058500100715           ptDtavI = pDtavI;
058600100715           ptDtavF = pDtavF;
058700100908           w60 = %xfoot($Uni01Tr);
058800100908           IF  w60 > 99900;
058900100908             pTotTr = *all'9';
059000100908           ELSE;
059100100908             pTotTr = w60;
059200100908           ENDIF;
059300110112           w60 = %xfoot($Uni01Tre);
059400110112           IF  w60 > 99900;
059500110112             pTotTre = *all'9';
059600110112           ELSE;
059700110112             pTotTre = w60;
059800110112           ENDIF;
059900100908           w120 = %xfoot($Uni01Fat);
060000100908           IF  w120 > 999999900;
060100100908             pTotFat = *all'9';
060200100908           ELSE;
060300100908             pTotFat = w120;
060400100908           ENDIF;
060500110112           w120 = %xfoot($Uni01Fate);
060600110112           IF  w120 > 999999900;
060700110112             pTotFate = *all'9';
060800110112           ELSE;
060900110112             pTotFate = w120;
061000110112           ENDIF;
061100100715           pTotSpe = %xfoot($Uni01Spe);
061200110112           pTotSpee = %xfoot($Uni01Spee);
061300100715           write TA98TOTA;
061400100714
061500100715         //?Di cui per tipo trattativa
061600100715           clear TA98TOTAD;
061700100715           ww = 1;
061800100715           FOR ww by 1 to %elem($Ttr);
061900100715             IF  $Ttr(ww) <> *blanks;
062000100715               pTtr     = %subst($Ttr(ww):3:1);
062100100715               pDesTtr  = %subst($Ttr(ww):4:25);
062200100908               w60      = $Uni01Tr(ww);
062300100908               IF  w60 > 99900;
062400100908                 pTotTrd = *all'9';
062500100908               ELSE;
062600100908                 pTotTrd = w60;
062700100908               ENDIF;
062800110112               w60      = $Uni01Tre(ww);
062900110112               IF  w60 > 99900;
063000110112                 pTotTrde = *all'9';
063100110112               ELSE;
063200110112                 pTotTrde = w60;
063300110112               ENDIF;
063400100908               w120     = $Uni01Fat(ww);
063500100908               IF  w120 > 999999900;
063600100908                 pTotFatd = *all'9';
063700100908               ELSE;
063800100908                 pTotFatd = w120;
063900100908               ENDIF;
064000110112               w120     = $Uni01Fate(ww);
064100110112               IF  w120 > 999999900;
064200110112                 pTotFatde = *all'9';
064300110112               ELSE;
064400110112                 pTotFatde = w120;
064500110112               ENDIF;
064600100715               pTotSped = $Uni01Spe(ww);
064700110112               pTotSpede = $Uni01Spee(ww);
064800100715               write TA98TOTAD;
064900100715               clear TA98TOTAD;
065000100715             ENDIF;
065100100715           ENDFOR;
065200100715         ENDIF;
065300100714
065400110112       //?Trattative CHIUSE
065500100715         IF  ITA98dtchi > 0;
065600110112         //?totale + Di cui POSITIVE
065700100715           clear TA98TOTC;
065800100715           ptDtchI = pDtchI;
065900100715           ptDtchF = pDtchF;
066000100908           w60 = %xfoot($Uni02Tr);
066100100908           IF  w60 > 99900;
066200100908             pTotTr = *all'9';
066300100908           ELSE;
066400100908             pTotTr = w60;
066500100908           ENDIF;
066600110112           w60 = %xfoot($Uni02Tre);
066700110112           IF  w60 > 99900;
066800110112             pTotTre = *all'9';
066900110112           ELSE;
067000110112             pTotTre = w60;
067100110112           ENDIF;
067200100908           w120 = %xfoot($Uni02Fat);
067300100908           IF  w120 > 999999900;
067400100908             pTotFat = *all'9';
067500100908           ELSE;
067600100908             pTotFat = w120;
067700100908           ENDIF;
067800110112           w120 = %xfoot($Uni02Fate);
067900110112           IF  w120 > 999999900;
068000110112             pTotFate = *all'9';
068100110112           ELSE;
068200110112             pTotFate = w120;
068300110112           ENDIF;
068400100715           pTotSpe  = %xfoot($Uni02Spe);
068500110112           pTotSpee = %xfoot($Uni02Spee);
068600100908           w60 = %xfoot($UniPos);
068700100908           IF  w60 > 99900;
068800100908             pTotPos = *all'9';
068900100908           ELSE;
069000100908             pTotPos = w60;
069100100908           ENDIF;
069200110112           w60 = %xfoot($UniPose);
069300110112           IF  w60 > 99900;
069400110112             pTotPose = *all'9';
069500110112           ELSE;
069600110112             pTotPose = w60;
069700110112           ENDIF;
069800100908           w120 = %xfoot($UniPosF);
069900100908           IF  w120 > 999999900;
070000100908             pTotPFat = *all'9';
070100100908           ELSE;
070200100908             pTotPFat = w120;
070300100908           ENDIF;
070400110112           w120 = %xfoot($UniPosFe);
070500110112           IF  w120 > 999999900;
070600110112             pTotPFate = *all'9';
070700110112           ELSE;
070800110112             pTotPFate = w120;
070900110112           ENDIF;
071000100715           pTotPSpe = %xfoot($UniPosS);
071100110112           pTotPSpee = %xfoot($UniPosSe);
071200110113           write TA98TOTC;
071300110112
071400110112         //?Di cui per tipo trattativa
071500110112           clear TA98TOTCD;
071600110112           ww = 1;
071700110112           FOR ww by 1 to %elem($Ttr);
071800110112             IF  $Ttr(ww) <> *blanks;
071900110112               pTtr      = %subst($Ttr(ww):3:1);
072000110112               pDesTtr   = %subst($Ttr(ww):4:25);
072100110112               w60       = $Uni02Tr(ww);
072200110112               IF  w60 > 99900;
072300110112                 pTotTrd = *all'9';
072400110112               ELSE;
072500110112                 pTotTrd = w60;
072600110112               ENDIF;
072700110112               w60       = $Uni02Tre(ww);
072800110112               IF  w60 > 99900;
072900110112                 pTotTrde = *all'9';
073000110112               ELSE;
073100110112                 pTotTrde = w60;
073200110112               ENDIF;
073300110112               w120      = $Uni02Fat(ww);
073400110112               IF  w120 > 999999900;
073500110112                 pTotFatd = *all'9';
073600110112               ELSE;
073700110112                 pTotFatd = w120;
073800110112               ENDIF;
073900110112               w120      = $Uni02Fate(ww);
074000110112               IF  w120 > 999999900;
074100110112                 pTotFatde = *all'9';
074200110112               ELSE;
074300110112                 pTotFatde = w120;
074400110112               ENDIF;
074500110112               pTotSped  = $Uni02Spe(ww);
074600110112               pTotSpede = $Uni02Spee(ww);
074700110112               w60       = $UniPos(ww);
074800110112               IF  w60 > 99900;
074900110112                 pTotPosd = *all'9';
075000110112               ELSE;
075100110112                 pTotPosd = w60;
075200110112               ENDIF;
075300110112               w60       = $UniPose(ww);
075400110112               IF  w60 > 99900;
075500110112                 pTotPosde = *all'9';
075600110112               ELSE;
075700110112                 pTotPosde = w60;
075800110112               ENDIF;
075900110112               w120      = $UniPosF(ww);
076000110112               IF  w120 > 999999900;
076100110112                 pTotPFatd = *all'9';
076200110112               ELSE;
076300110112                 pTotPFatd = w120;
076400110112               ENDIF;
076500110112               w120      = $UniPosFe(ww);
076600110112               IF  w120 > 999999900;
076700110112                 pTotPFatde = *all'9';
076800110112               ELSE;
076900110112                 pTotPFatde = w120;
077000110112               ENDIF;
077100110112               pTotPSped = $UniPosS(ww);
077200110112               pTotPSpede = $UniPosSe(ww);
077300110112               write TA98TOTCD;
077400110112               clear TA98TOTCD;
077500110112             ENDIF;
077600110112           ENDFOR;
077700110112
077800110112         //?di cui NEGATIVE
077900110112           clear TA98TOTC1;
078000110112           w60 = %xfoot($UniNeg);
078100110112           IF  w60 > 99900;
078200110112             pTotNeg = *all'9';
078300110112           ELSE;
078400110112             pTotNeg = w60;
078500110112           ENDIF;
078600110112           w60 = %xfoot($UniNege);
078700110112           IF  w60 > 99900;
078800110112             pTotNege = *all'9';
078900110112           ELSE;
079000110112             pTotNege = w60;
079100110112           ENDIF;
079200110112           w120 = %xfoot($UniNegF);
079300110112           IF  w120 > 999999900;
079400110112             pTotNFat = *all'9';
079500110112           ELSE;
079600110112             pTotNFat = w120;
079700110112           ENDIF;
079800110112           w120 = %xfoot($UniNegFe);
079900110112           IF  w120 > 999999900;
080000110112             pTotNFate = *all'9';
080100110112           ELSE;
080200110112             pTotNFate = w120;
080300110112           ENDIF;
080400110112           pTotNSpe = %xfoot($UniNegS);
080500110112           pTotNSpee = %xfoot($UniNegSe);
080600110112           write TA98TOTC1;
080700100714
080800100715         //?Di cui per tipo trattativa
080900110112           clear TA98TOTCD1;
081000100715           ww = 1;
081100100715           FOR ww by 1 to %elem($Ttr);
081200100715             IF  $Ttr(ww) <> *blanks;
081300110117               pTtr      = %subst($Ttr(ww):3:1);
081400110117               pDesTtr   = %subst($Ttr(ww):4:25);
081500100908               w60       = $UniNeg(ww);
081600100908               IF  w60 > 99900;
081700100908                 pTotNegd = *all'9';
081800100908               ELSE;
081900100908                 pTotNegd = w60;
082000100908               ENDIF;
082100110112               w60       = $UniNege(ww);
082200110112               IF  w60 > 99900;
082300110112                 pTotNegde = *all'9';
082400110112               ELSE;
082500110112                 pTotNegde = w60;
082600110112               ENDIF;
082700100908               w120      = $UniNegF(ww);
082800100908               IF  w120 > 999999900;
082900100908                 pTotNFatd = *all'9';
083000100908               ELSE;
083100100908                 pTotNFatd = w120;
083200100908               ENDIF;
083300110112               w120      = $UniNegFe(ww);
083400110112               IF  w120 > 999999900;
083500110112                 pTotNFatde = *all'9';
083600110112               ELSE;
083700110112                 pTotNFatde = w120;
083800110112               ENDIF;
083900100715               pTotNSped = $UniNegS(ww);
084000110112               pTotNSpede = $UniNegSe(ww);
084100110112               write TA98TOTCD1;
084200110112               clear TA98TOTCD1;
084300100715             ENDIF;
084400100715           ENDFOR;
084500100715         ENDIF;
084600100714
084700100714       //?Trattative IN CORSO
084800100715         IF  ITA98dtica > 0;
084900100715           clear TA98TOTX;
085000100715           ptDtica = pDtica;
085100100908           w60 = %xfoot($Uni03Tr);
085200100908           IF  w60 > 99900;
085300100908             pTotTr = *all'9';
085400100908           ELSE;
085500100908             pTotTr = w60;
085600100908           ENDIF;
085700110112           w60 = %xfoot($Uni03Tre);
085800110112           IF  w60 > 99900;
085900110112             pTotTre = *all'9';
086000110112           ELSE;
086100110112             pTotTre = w60;
086200110112           ENDIF;
086300100908           w120 = %xfoot($Uni03Fat);
086400100908           IF  w120 > 999999900;
086500100908             pTotFat = *all'9';
086600100908           ELSE;
086700100908             pTotFat = w120;
086800100908           ENDIF;
086900110112           w120 = %xfoot($Uni03Fate);
087000110112           IF  w120 > 999999900;
087100110112             pTotFate = *all'9';
087200110112           ELSE;
087300110112             pTotFate = w120;
087400110112           ENDIF;
087500100715           pTotSpe    = %xfoot($Uni03Spe);
087600110112           pTotSpee   = %xfoot($Uni03Spee);
087700100908           w60 = %xfoot($UniCaldo);
087800100908           IF  w60 > 99900;
087900100908             pTotCaldo = *all'9';
088000100908           ELSE;
088100100908             pTotCaldo = w60;
088200100908           ENDIF;
088300110112           w60 = %xfoot($UniCaldoe);
088400110112           IF  w60 > 99900;
088500110112             pTotCaldoe = *all'9';
088600110112           ELSE;
088700110112             pTotCaldoe = w60;
088800110112           ENDIF;
088900100908           w120 = %xfoot($UniCaldoF);
089000100908           IF  w120 > 999999900;
089100100908             pTotCalF = *all'9';
089200100908           ELSE;
089300100908             pTotCalF = w120;
089400100908           ENDIF;
089500110112           w120 = %xfoot($UniCaldoFe);
089600110112           IF  w120 > 999999900;
089700110112             pTotCalFe = *all'9';
089800110112           ELSE;
089900110112             pTotCalFe = w120;
090000110112           ENDIF;
090100100715           pTotCalS   = %xfoot($UniCaldoS);
090200110112           pTotCalSe  = %xfoot($UniCaldoSe);
090300100715           write TA98TOTX;
090400100714
090500100715         //?Di cui per tipo trattativa
090600100715           clear TA98TOTXD;
090700100715           ww = 1;
090800100715           FOR ww by 1 to %elem($Ttr);
090900100715             IF  $Ttr(ww) <> *blanks;
091000100715               pTtr       = %subst($Ttr(ww):3:1);
091100100715               pDesTtr    = %subst($Ttr(ww):4:25);
091200100908               w60        = $Uni03Tr(ww);
091300100908               IF  w60 > 99900;
091400100908                 pTotTrd = *all'9';
091500100908               ELSE;
091600100908                 pTotTrd = w60;
091700100908               ENDIF;
091800110112               w60        = $Uni03Tre(ww);
091900110112               IF  w60 > 99900;
092000110112                 pTotTrde = *all'9';
092100110112               ELSE;
092200110112                 pTotTrde = w60;
092300110112               ENDIF;
092400100908               w120       = $Uni03Fat(ww);
092500100908               IF  w120 > 999999900;
092600100908                 pTotFatd = *all'9';
092700100908               ELSE;
092800100908                 pTotFatd = w120;
092900100908               ENDIF;
093000110112               w120       = $Uni03Fate(ww);
093100110112               IF  w120 > 999999900;
093200110112                 pTotFatde = *all'9';
093300110112               ELSE;
093400110112                 pTotFatde = w120;
093500110112               ENDIF;
093600100715               pTotSped   = $Uni03Spe(ww);
093700110112               pTotSpede  = $Uni03Spee(ww);
093800100908               w60        = $UniCaldo(ww);
093900100908               IF  w60 > 99900;
094000100908                 pTotCaldod = *all'9';
094100100908               ELSE;
094200100908                 pTotCaldod = w60;
094300100908               ENDIF;
094400110112               w60        = $UniCaldoe(ww);
094500110112               IF  w60 > 99900;
094600110113                 pToCaldode = *all'9';
094700110112               ELSE;
094800110113                 pToCaldode = w60;
094900110112               ENDIF;
095000100908               w120       = $UniCaldoF(ww);
095100100908               IF  w120 > 999999900;
095200100908                 pTotCalFd = *all'9';
095300100908               ELSE;
095400100908                 pTotCalFd = w120;
095500100908               ENDIF;
095600110112               w120       = $UniCaldoFe(ww);
095700110112               IF  w120 > 999999900;
095800110112                 pTotCalFde = *all'9';
095900110112               ELSE;
096000110112                 pTotCalFde = w120;
096100110112               ENDIF;
096200100715               pTotCalSd  = $UniCaldoS(ww);
096300110112               pTotCalSde = $UniCaldoSe(ww);
096400100715               write TA98TOTXD;
096500100715               clear TA98TOTXD;
096600100715             ENDIF;
096700100715           ENDFOR;
096800100715         ENDIF;
096900100716
097000100716       ENDSR;
097100100716
097200100716       //--------------------------------------------------------------
097300100716       //?Sommo i totali per unificante.
097400100716       //--------------------------------------------------------------
097500100716       BEGSR sr_MemTotUni;
097600100716
097700100716       //?Imposto se commerciale INTERNO o ESTERNO
097800101006         IF  wFun = 'COMIN' or wFun = 'ASC' or wFun = 'SA';
097900100716           $Interno = *on;
098000100716           $Esterno = *off;
098100100716         ENDIF;
098200101006         IF  wFun <> 'COMIN' and wFun <> 'ASC' and wFun <> 'SA';
098300100716           $Interno = *off;
098400100716           $Esterno = *on;
098500100716         ENDIF;
098600100714
098700100714       //?Sommo i totali dei commerciali nelle sk dei totali generali
098800100714       //?divise per commerciali INTERNI ed ESTERNI
098900100714         ww = 1;
099000100714         FOR ww by 1 to %elem($Ttr);
099100100714           IF  $Ttr(ww) <> *blanks;
099200100714         //?commerciali INTERNI
099300100714             IF  $Interno;
099400100714               $TotI01Tr(ww)   += $Uni01Tr(ww);
099500100714               $TotI01Fat(ww)  += $Uni01Fat(ww);
099600100714               $TotI01Spe(ww)  += $Uni01Spe(ww);
099700100716               $TotI02Tr(ww)   += $Uni02Tr(ww);
099800100716               $TotI02Fat(ww)  += $Uni02Fat(ww);
099900100716               $TotI02Spe(ww)  += $Uni02Spe(ww);
100000100714               $TotIPos(ww)    += $UniPos(ww);
100100100714               $TotIPosF(ww)   += $UniPosF(ww);
100200100714               $TotIPosS(ww)   += $UniPosS(ww);
100300100714               $TotINeg(ww)    += $UniNeg(ww);
100400100714               $TotINegF(ww)   += $UniNegF(ww);
100500100714               $TotINegS(ww)   += $UniNegS(ww);
100600100714               $TotI03Tr(ww)   += $Uni03Tr(ww);
100700100714               $TotI03Fat(ww)  += $Uni03Fat(ww);
100800100714               $TotI03Spe(ww)  += $Uni03Spe(ww);
100900100714               $TotICaldo(ww)  += $UniCaldo(ww);
101000100714               $TotICaldoF(ww) += $UniCaldoF(ww);
101100100714               $TotICaldoS(ww) += $UniCaldoS(ww);
101200110112           //?di cui estero
101300110112               $TotI01Tre(ww)   += $Uni01Tre(ww);
101400110112               $TotI01Fate(ww)  += $Uni01Fate(ww);
101500110112               $TotI01Spee(ww)  += $Uni01Spee(ww);
101600110112               $TotI02Tre(ww)   += $Uni02Tre(ww);
101700110112               $TotI02Fate(ww)  += $Uni02Fate(ww);
101800110112               $TotI02Spee(ww)  += $Uni02Spee(ww);
101900110112               $TotIPose(ww)    += $UniPose(ww);
102000110112               $TotIPosFe(ww)   += $UniPosFe(ww);
102100110112               $TotIPosSe(ww)   += $UniPosSe(ww);
102200110112               $TotINege(ww)    += $UniNege(ww);
102300110112               $TotINegFe(ww)   += $UniNegFe(ww);
102400110112               $TotINegSe(ww)   += $UniNegSe(ww);
102500110112               $TotI03Tre(ww)   += $Uni03Tre(ww);
102600110112               $TotI03Fate(ww)  += $Uni03Fate(ww);
102700110112               $TotI03Spee(ww)  += $Uni03Spee(ww);
102800110112               $TotICaldoe(ww)  += $UniCaldoe(ww);
102900110112               $TotICaldoFe(ww) += $UniCaldoFe(ww);
103000110112               $TotICaldoSe(ww) += $UniCaldoSe(ww);
103100100714             ENDIF;
103200100714         //?commerciali ESTERNI
103300100714             IF  $Esterno;
103400100714               $TotE01Tr(ww)   += $Uni01Tr(ww);
103500100714               $TotE01Fat(ww)  += $Uni01Fat(ww);
103600100714               $TotE01Spe(ww)  += $Uni01Spe(ww);
103700100714               $TotE02Tr(ww)   += $Uni02Tr(ww);
103800100714               $TotE02Fat(ww)  += $Uni02Fat(ww);
103900100714               $TotE02Spe(ww)  += $Uni02Spe(ww);
104000100714               $TotEPos(ww)    += $UniPos(ww);
104100100714               $TotEPosF(ww)   += $UniPosF(ww);
104200100714               $TotEPosS(ww)   += $UniPosS(ww);
104300100714               $TotENeg(ww)    += $UniNeg(ww);
104400100714               $TotENegF(ww)   += $UniNegF(ww);
104500100714               $TotENegS(ww)   += $UniNegS(ww);
104600100714               $TotE03Tr(ww)   += $Uni03Tr(ww);
104700100714               $TotE03Fat(ww)  += $Uni03Fat(ww);
104800100714               $TotE03Spe(ww)  += $Uni03Spe(ww);
104900100714               $TotECaldo(ww)  += $UniCaldo(ww);
105000100714               $TotECaldoF(ww) += $UniCaldoF(ww);
105100100714               $TotECaldoS(ww) += $UniCaldoS(ww);
105200110112           //?di cui estero
105300110112               $TotE01Tre(ww)   += $Uni01Tre(ww);
105400110112               $TotE01Fate(ww)  += $Uni01Fate(ww);
105500110112               $TotE01Spee(ww)  += $Uni01Spee(ww);
105600110112               $TotE02Tre(ww)   += $Uni02Tre(ww);
105700110112               $TotE02Fate(ww)  += $Uni02Fate(ww);
105800110112               $TotE02Spee(ww)  += $Uni02Spee(ww);
105900110112               $TotEPose(ww)    += $UniPose(ww);
106000110112               $TotEPosFe(ww)   += $UniPosFe(ww);
106100110112               $TotEPosSe(ww)   += $UniPosSe(ww);
106200110112               $TotENege(ww)    += $UniNege(ww);
106300110112               $TotENegFe(ww)   += $UniNegFe(ww);
106400110112               $TotENegSe(ww)   += $UniNegSe(ww);
106500110112               $TotE03Tre(ww)   += $Uni03Tre(ww);
106600110112               $TotE03Fate(ww)  += $Uni03Fate(ww);
106700110112               $TotE03Spee(ww)  += $Uni03Spee(ww);
106800110112               $TotECaldoe(ww)  += $UniCaldoe(ww);
106900110112               $TotECaldoFe(ww) += $UniCaldoFe(ww);
107000110112               $TotECaldoSe(ww) += $UniCaldoSe(ww);
107100100714             ENDIF;
107200100714           ENDIF;
107300100714         ENDFOR;
107400100714
107500100714       //?Pulisco le schiere per Unificante
107600100714         clear $Uni01Tr;
107700100714         clear $Uni01Fat;
107800100714         clear $Uni01Spe;
107900100714         clear $Uni02Tr;
108000100714         clear $Uni02Fat;
108100100714         clear $Uni02Spe;
108200100714         clear $UniPos;
108300100714         clear $UniPosF;
108400100714         clear $UniPosS;
108500100714         clear $UniNeg;
108600100714         clear $UniNegF;
108700100714         clear $UniNegS;
108800100714         clear $Uni03Tr;
108900100714         clear $Uni03Fat;
109000100714         clear $Uni03Spe;
109100100714         clear $UniCaldo;
109200100714         clear $UniCaldoF;
109300100714         clear $UniCaldoS;
109400110112         clear $Uni01Tre;
109500110112         clear $Uni01Fate;
109600110112         clear $Uni01Spee;
109700110112         clear $Uni02Tre;
109800110112         clear $Uni02Fate;
109900110112         clear $Uni02Spee;
110000110112         clear $UniPose;
110100110112         clear $UniPosFe;
110200110112         clear $UniPosSe;
110300110112         clear $UniNege;
110400110112         clear $UniNegFe;
110500110112         clear $UniNegSe;
110600110112         clear $Uni03Tre;
110700110112         clear $Uni03Fate;
110800110112         clear $Uni03Spee;
110900110112         clear $UniCaldoe;
111000110112         clear $UniCaldoFe;
111100110112         clear $UniCaldoSe;
111200100714
111300100714       ENDSR;
111400100714
111500100714       //--------------------------------------------------------------
111600100714       //?Stampo i totali generali commerciali INTERNI.
111700100714       //--------------------------------------------------------------
111800100714       BEGSR sr_StpTotGenI;
111900100714
112000100714       //?Stampo la testata
112100100714         *in01 = *off;
112200100714         *in02 = *off;
112300100714         *in03 = *off;
112400100714         write TA98PAGINA;
112500100714         wrkriga = '*' + wTrat + '*';
112600100714         write TA98SEPARA;
112700100714         write TA98TESTAG;
112800100714         write TA98SEPARA;
112900100714
113000100714       //?Trattative AVVIATE
113100100715         IF  ITA98dtavi > 0;
113200100715           clear TA98TOTA;
113300100715           ptDtavI = pDtavI;
113400100715           ptDtavF = pDtavF;
113500100715           w60 = %xfoot($TotI01Tr);
113600100715           IF  w60 > 99900;
113700100715             pTotTr = *all'9';
113800100715           ELSE;
113900100715             pTotTr = w60;
114000100715           ENDIF;
114100110113           w60 = %xfoot($TotI01Tre);
114200110113           IF  w60 > 99900;
114300110113             pTotTre = *all'9';
114400110113           ELSE;
114500110113             pTotTre = w60;
114600110113           ENDIF;
114700100715           w120 = %xfoot($TotI01Fat);
114800100715           IF  w120 > 999999900;
114900100715             pTotFat = *all'9';
115000100715           ELSE;
115100100715             pTotFat = w120;
115200100715           ENDIF;
115300110113           w120 = %xfoot($TotI01Fate);
115400110113           IF  w120 > 999999900;
115500110113             pTotFate = *all'9';
115600110113           ELSE;
115700110113             pTotFate = w120;
115800110113           ENDIF;
115900100715           pTotSpe = %xfoot($TotI01Spe);
116000110113           pTotSpee = %xfoot($TotI01Spee);
116100100715           write TA98TOTA;
116200100714
116300100715         //?Di cui per tipo trattativa
116400100715           clear TA98TOTAD;
116500100715           ww = 1;
116600100715           FOR ww by 1 to %elem($Ttr);
116700100715             IF  $Ttr(ww) <> *blanks;
116800100715               pTtr     = %subst($Ttr(ww):3:1);
116900100715               pDesTtr  = %subst($Ttr(ww):4:25);
117000100908               w60      = $TotI01Tr(ww);
117100100908               IF  w60 > 99900;
117200100908                 pTotTrd = *all'9';
117300100908               ELSE;
117400100908                 pTotTrd = w60;
117500100908               ENDIF;
117600110113               w60      = $TotI01Tre(ww);
117700110113               IF  w60 > 99900;
117800110113                 pTotTrde = *all'9';
117900110113               ELSE;
118000110113                 pTotTrde = w60;
118100110113               ENDIF;
118200100908               w120     = $TotI01Fat(ww);
118300100908               IF  w120 > 999999900;
118400100908                 pTotFatd = *all'9';
118500100908               ELSE;
118600100908                 pTotFatd = w120;
118700100908               ENDIF;
118800110113               w120     = $TotI01Fate(ww);
118900110113               IF  w120 > 999999900;
119000110113                 pTotFatde = *all'9';
119100110113               ELSE;
119200110113                 pTotFatde = w120;
119300110113               ENDIF;
119400100715               pTotSped = $TotI01Spe(ww);
119500110113               pTotSpede = $TotI01Spee(ww);
119600100715               write TA98TOTAD;
119700100715               clear TA98TOTAD;
119800100715             ENDIF;
119900100715           ENDFOR;
120000100715         ENDIF;
120100100714
120200100714       //?Trattative CHIUSE
120300100715         IF  ITA98dtchi > 0;
120400110113         //?totale + Di cui POSITIVE
120500100715           clear TA98TOTC;
120600100715           ptDtchI = pDtchI;
120700100715           ptDtchF = pDtchF;
120800100715           w60 = %xfoot($TotI02Tr);
120900100715           IF  w60 > 99900;
121000100715             pTotTr = *all'9';
121100100715           ELSE;
121200100715             pTotTr = w60;
121300100715           ENDIF;
121400110113           w60 = %xfoot($TotI02Tre);
121500110113           IF  w60 > 99900;
121600110113             pTotTre = *all'9';
121700110113           ELSE;
121800110113             pTotTre = w60;
121900110113           ENDIF;
122000100715           w120 = %xfoot($TotI02Fat);
122100100715           IF  w120 > 999999900;
122200100715             pTotFat = *all'9';
122300100715           ELSE;
122400100715             pTotFat = w120;
122500100715           ENDIF;
122600110113           w120 = %xfoot($TotI02Fate);
122700110113           IF  w120 > 999999900;
122800110113             pTotFate = *all'9';
122900110113           ELSE;
123000110113             pTotFate = w120;
123100110113           ENDIF;
123200100715           pTotSpe  = %Xfoot($TotI02Spe);
123300110113           pTotSpee = %Xfoot($TotI02Spee);
123400100715           w60 = %xfoot($TotIPos);
123500100715           IF  w60 > 99900;
123600100715             pTotPos = *all'9';
123700100715           ELSE;
123800100715             pTotPos = w60;
123900100715           ENDIF;
124000110113           w60 = %xfoot($TotIPose);
124100110113           IF  w60 > 99900;
124200110113             pTotPose = *all'9';
124300110113           ELSE;
124400110113             pTotPose = w60;
124500110113           ENDIF;
124600100715           w120 = %xfoot($TotIPosF);
124700100715           IF  w120 > 999999900;
124800100715             pTotPFat = *all'9';
124900100715           ELSE;
125000100715             pTotPFat = w120;
125100100715           ENDIF;
125200110113           w120 = %xfoot($TotIPosFe);
125300110113           IF  w120 > 999999900;
125400110113             pTotPFate = *all'9';
125500110113           ELSE;
125600110113             pTotPFate = w120;
125700110113           ENDIF;
125800100715           pTotPSpe = %xfoot($TotIPosS);
125900110113           pTotPSpee = %xfoot($TotIPosSe);
126000100715           write TA98TOTC;
126100110113
126200110113         //?Di cui per tipo trattativa
126300110113           clear TA98TOTCD;
126400110113           ww = 1;
126500110113           FOR ww by 1 to %elem($Ttr);
126600110113             IF  $Ttr(ww) <> *blanks;
126700110113               pTtr      = %subst($Ttr(ww):3:1);
126800110113               pDesTtr   = %subst($Ttr(ww):4:25);
126900110113               w60       = $TotI02Tr(ww);
127000110113               IF  w60 > 99900;
127100110113                 pTotTrd = *all'9';
127200110113               ELSE;
127300110113                 pTotTrd = w60;
127400110113               ENDIF;
127500110113               w60       = $TotI02Tre(ww);
127600110113               IF  w60 > 99900;
127700110113                 pTotTrde = *all'9';
127800110113               ELSE;
127900110113                 pTotTrde = w60;
128000110113               ENDIF;
128100110113               w120      = $TotI02Fat(ww);
128200110113               IF  w120 > 999999900;
128300110113                 pTotFatd = *all'9';
128400110113               ELSE;
128500110113                 pTotFatd = w120;
128600110113               ENDIF;
128700110113               w120      = $TotI02Fate(ww);
128800110113               IF  w120 > 999999900;
128900110113                 pTotFatde = *all'9';
129000110113               ELSE;
129100110113                 pTotFatde = w120;
129200110113               ENDIF;
129300110113               pTotSped  = $TotI02Spe(ww);
129400110113               pTotSpede = $TotI02Spee(ww);
129500110113               w60       = $TotIPos(ww);
129600110113               IF  w60 > 99900;
129700110113                 pTotPosd = *all'9';
129800110113               ELSE;
129900110113                 pTotPosd = w60;
130000110113               ENDIF;
130100110113               w60       = $TotIPose(ww);
130200110113               IF  w60 > 99900;
130300110113                 pTotPosde = *all'9';
130400110113               ELSE;
130500110113                 pTotPosde = w60;
130600110113               ENDIF;
130700110113               w120      = $TotIPosF(ww);
130800110113               IF  w120 > 999999900;
130900110113                 pTotPFatd = *all'9';
131000110113               ELSE;
131100110113                 pTotPFatd = w120;
131200110113               ENDIF;
131300110113               w120      = $TotIPosFe(ww);
131400110113               IF  w120 > 999999900;
131500110113                 pTotPFatde = *all'9';
131600110113               ELSE;
131700110113                 pTotPFatde = w120;
131800110113               ENDIF;
131900110113               pTotPSped = $TotIPosS(ww);
132000110113               pTotPSpede = $TotIPosSe(ww);
132100110113               write TA98TOTCD;
132200110113               clear TA98TOTCD;
132300110113             ENDIF;
132400110113           ENDFOR;
132500110113
132600110113         //?di cui NEGATIVE
132700110113           clear TA98TOTC1;
132800110113           w60 = %xfoot($TotINeg);
132900110113           IF  w60 > 99900;
133000110113             pTotNeg = *all'9';
133100110113           ELSE;
133200110113             pTotNeg = w60;
133300110113           ENDIF;
133400110113           w60 = %xfoot($TotINege);
133500110113           IF  w60 > 99900;
133600110113             pTotNege = *all'9';
133700110113           ELSE;
133800110113             pTotNege = w60;
133900110113           ENDIF;
134000110113           w120 = %xfoot($TotINegF);
134100110113           IF  w120 > 999999900;
134200110113             pTotNFat = *all'9';
134300110113           ELSE;
134400110113             pTotNFat = w120;
134500110113           ENDIF;
134600110113           w120 = %xfoot($TotINegFe);
134700110113           IF  w120 > 999999900;
134800110113             pTotNFate = *all'9';
134900110113           ELSE;
135000110113             pTotNFate = w120;
135100110113           ENDIF;
135200110113           pTotNSpe = %xfoot($TotINegS);
135300110113           pTotNSpee = %xfoot($TotINegSe);
135400110113           write TA98TOTC1;
135500100714
135600100715         //?Di cui per tipo trattativa
135700110113           clear TA98TOTCD1;
135800100715           ww = 1;
135900100715           FOR ww by 1 to %elem($Ttr);
136000100715             IF  $Ttr(ww) <> *blanks;
136100110117               pTtr      = %subst($Ttr(ww):3:1);
136200110117               pDesTtr   = %subst($Ttr(ww):4:25);
136300100908               w60       = $TotINeg(ww);
136400100908               IF  w60 > 99900;
136500100908                 pTotNegd = *all'9';
136600100908               ELSE;
136700100908                 pTotNegd = w60;
136800100908               ENDIF;
136900110113               w60       = $TotINege(ww);
137000110113               IF  w60 > 99900;
137100110113                 pTotNegde = *all'9';
137200110113               ELSE;
137300110113                 pTotNegde = w60;
137400110113               ENDIF;
137500100908               w120      = $TotINegF(ww);
137600100908               IF  w120 > 999999900;
137700100908                 pTotNFatd = *all'9';
137800100908               ELSE;
137900100908                 pTotNFatd = w120;
138000100908               ENDIF;
138100110113               w120      = $TotINegFe(ww);
138200110113               IF  w120 > 999999900;
138300110113                 pTotNFatde = *all'9';
138400110113               ELSE;
138500110113                 pTotNFatde = w120;
138600110113               ENDIF;
138700100715               pTotNSped = $TotINegS(ww);
138800110113               pTotNSpede = $TotINegSe(ww);
138900110113               write TA98TOTCD1;
139000110113               clear TA98TOTCD1;
139100100715             ENDIF;
139200100715           ENDFOR;
139300100715         ENDIF;
139400100714
139500100714       //?Trattative IN CORSO
139600100715         IF  ITA98dtica > 0;
139700100715           clear TA98TOTX;
139800100715           ptDtica = pDticA;
139900100715           w60 = %xfoot($TotI03Tr);
140000100715           IF  w60 > 99900;
140100100715             pTotTr = *all'9';
140200100715           ELSE;
140300100715             pTotTr = w60;
140400100715           ENDIF;
140500110113           w60 = %xfoot($TotI03Tre);
140600110113           IF  w60 > 99900;
140700110113             pTotTre = *all'9';
140800110113           ELSE;
140900110113             pTotTre = w60;
141000110113           ENDIF;
141100100715           w120 = %xfoot($TotI03Fat);
141200100715           IF  w120 > 999999900;
141300100715             pTotFat = *all'9';
141400100715           ELSE;
141500100715             pTotFat = w120;
141600100715           ENDIF;
141700110113           w120 = %xfoot($TotI03Fate);
141800110113           IF  w120 > 999999900;
141900110113             pTotFate = *all'9';
142000110113           ELSE;
142100110113             pTotFate = w120;
142200110113           ENDIF;
142300100715           pTotSpe = %xfoot($TotI03Spe);
142400110113           pTotSpee = %xfoot($TotI03Spee);
142500100715           w60 = %xfoot($TotICaldo);
142600100715           IF  w60 > 99900;
142700100715             pTotCaldo = *all'9';
142800100715           ELSE;
142900100715             pTotCaldo = w60;
143000100715           ENDIF;
143100110113           w60 = %xfoot($TotICaldoe);
143200110113           IF  w60 > 99900;
143300110113             pTotCaldoe = *all'9';
143400110113           ELSE;
143500110113             pTotCaldoe = w60;
143600110113           ENDIF;
143700100715           w120 = %xfoot($TotICaldoF);
143800100715           IF  w120 > 999999900;
143900100715             pTotCalF = *all'9';
144000100715           ELSE;
144100100715             pTotCalF = w120;
144200100715           ENDIF;
144300110113           w120 = %xfoot($TotICaldoFe);
144400110113           IF  w120 > 999999900;
144500110113             pTotCalFe = *all'9';
144600110113           ELSE;
144700110113             pTotCalFe = w120;
144800110113           ENDIF;
144900100715           pTotCalS = %xfoot($TotICaldoS);
145000110113           pTotCalSe = %xfoot($TotICaldoSe);
145100100715           write TA98TOTX;
145200100714
145300100715         //?Di cui per tipo trattativa
145400100715           clear TA98TOTXD;
145500100715           ww = 1;
145600100715           FOR ww by 1 to %elem($Ttr);
145700100715             IF  $Ttr(ww) <> *blanks;
145800100715               pTtr       = %subst($Ttr(ww):3:1);
145900100715               pDesTtr    = %subst($Ttr(ww):4:25);
146000100908               w60        = $TotI03Tr(ww);
146100100908               IF  w60 > 99900;
146200100908                 pTotTrd = *all'9';
146300100908               ELSE;
146400100908                 pTotTrd = w60;
146500100908               ENDIF;
146600110113               w60        = $TotI03Tre(ww);
146700110113               IF  w60 > 99900;
146800110113                 pTotTrde = *all'9';
146900110113               ELSE;
147000110113                 pTotTrde = w60;
147100110113               ENDIF;
147200100908               w120       = $TotI03Fat(ww);
147300100908               IF  w120 > 999999900;
147400100908                 pTotFatd = *all'9';
147500100908               ELSE;
147600100908                 pTotFatd = w120;
147700100908               ENDIF;
147800110113               w120       = $TotI03Fate(ww);
147900110113               IF  w120 > 999999900;
148000110113                 pTotFatde = *all'9';
148100110113               ELSE;
148200110113                 pTotFatde = w120;
148300110113               ENDIF;
148400100715               pTotSped   = $TotI03Spe(ww);
148500110113               pTotSpede  = $TotI03Spee(ww);
148600100908               w60        = $TotICaldo(ww);
148700100908               IF  w60 > 99900;
148800100908                 pTotCaldod = *all'9';
148900100908               ELSE;
149000100908                 pTotCaldod = w60;
149100100908               ENDIF;
149200110113               w60        = $TotICaldoe(ww);
149300110113               IF  w60 > 99900;
149400110113                 pToCaldode = *all'9';
149500110113               ELSE;
149600110113                 pToCaldode = w60;
149700110113               ENDIF;
149800100908               w120       = $TotICaldoF(ww);
149900100908               IF  w120 > 999999900;
150000100908                 pTotCalFd = *all'9';
150100100908               ELSE;
150200100908                 pTotCalFd = w120;
150300100908               ENDIF;
150400110113               w120       = $TotICaldoFe(ww);
150500110113               IF  w120 > 999999900;
150600110113                 pTotCalFde = *all'9';
150700110113               ELSE;
150800110113                 pTotCalFde = w120;
150900110113               ENDIF;
151000100715               pTotCalSd  = $TotICaldoS(ww);
151100110113               pTotCalSde = $TotICaldoSe(ww);
151200100715               write TA98TOTXD;
151300100715               clear TA98TOTXD;
151400100715             ENDIF;
151500100715           ENDFOR;
151600100715         ENDIF;
151700100714
151800100714       ENDSR;
151900100714
152000100714       //--------------------------------------------------------------
152100100714       //?Stampo i totali generali commerciali ESTERNI.
152200100714       //--------------------------------------------------------------
152300100714       BEGSR sr_StpTotGenE;
152400100714
152500100714       //?Stampo la testata
152600100714         *in01 = *off;
152700100714         *in02 = *off;
152800100714         *in03 = *off;
152900100714         write TA98PAGINA;
153000100714         wrkriga = '*' + wTrat + '*';
153100100714         write TA98SEPARA;
153200100714         write TA98TESTAG;
153300100714         write TA98SEPARA;
153400100714
153500100714       //?Trattative AVVIATE
153600100715         IF  ITA98dtavi > 0;
153700100715           clear TA98TOTA;
153800100715           ptDtavI = pDtavI;
153900100715           ptDtavF = pDtavF;
154000100715           w60 = %xfoot($TotE01Tr);
154100100715           IF  w60 > 99900;
154200100715             pTotTr = *all'9';
154300100715           ELSE;
154400100715             pTotTr = w60;
154500100715           ENDIF;
154600110113           w60 = %xfoot($TotE01Tre);
154700110113           IF  w60 > 99900;
154800110113             pTotTre = *all'9';
154900110113           ELSE;
155000110113             pTotTre = w60;
155100110113           ENDIF;
155200100715           w120 = %xfoot($TotE01Fat);
155300100715           IF  w120 > 999999900;
155400100715             pTotFat = *all'9';
155500100715           ELSE;
155600100715             pTotFat = w120;
155700100715           ENDIF;
155800110113           w120 = %xfoot($TotE01Fate);
155900110113           IF  w120 > 999999900;
156000110113             pTotFate = *all'9';
156100110113           ELSE;
156200110113             pTotFate = w120;
156300110113           ENDIF;
156400100715           pTotSpe = %xfoot($TotE01Spe);
156500110113           pTotSpee = %xfoot($TotE01Spee);
156600100715           write TA98TOTA;
156700100714
156800100715         //?Di cui per tipo trattativa
156900100715           clear TA98TOTAD;
157000100715           ww = 1;
157100100715           FOR ww by 1 to %elem($Ttr);
157200100715             IF  $Ttr(ww) <> *blanks;
157300100715               pTtr     = %subst($Ttr(ww):3:1);
157400100715               pDesTtr  = %subst($Ttr(ww):4:25);
157500100908               w60      = $TotE01Tr(ww);
157600100908               IF  w60 > 99900;
157700100908                 pTotTrd = *all'9';
157800100908               ELSE;
157900100908                 pTotTrd = w60;
158000100908               ENDIF;
158100110113               w60      = $TotE01Tre(ww);
158200110113               IF  w60 > 99900;
158300110113                 pTotTrde = *all'9';
158400110113               ELSE;
158500110113                 pTotTrde = w60;
158600110113               ENDIF;
158700100908               w120     = $TotE01Fat(ww);
158800100908               IF  w120 > 999999900;
158900100908                 pTotFatd = *all'9';
159000100908               ELSE;
159100100908                 pTotFatd = w120;
159200100908               ENDIF;
159300110113               w120     = $TotE01Fate(ww);
159400110113               IF  w120 > 999999900;
159500110113                 pTotFatde = *all'9';
159600110113               ELSE;
159700110113                 pTotFatde = w120;
159800110113               ENDIF;
159900100715               pTotSped = $TotE01Spe(ww);
160000110113               pTotSpede = $TotE01Spee(ww);
160100100715               write TA98TOTAD;
160200100715               clear TA98TOTAD;
160300100715             ENDIF;
160400100715           ENDFOR;
160500100715         ENDIF;
160600100714
160700100714       //?Trattative CHIUSE
160800100715         IF  ITA98dtchi > 0;
160900110113         //?totale + Di cui POSITIVE
161000100715           clear TA98TOTC;
161100100715           ptDtchI = pDtchI;
161200100715           ptDtchF = pDtchF;
161300100715           w60 = %xfoot($TotE02Tr);
161400100715           IF  w60 > 99900;
161500100715             pTotTr = *all'9';
161600100715           ELSE;
161700100715             pTotTr = w60;
161800100715           ENDIF;
161900110113           w60 = %xfoot($TotE02Tre);
162000110113           IF  w60 > 99900;
162100110113             pTotTre = *all'9';
162200110113           ELSE;
162300110113             pTotTre = w60;
162400110113           ENDIF;
162500100715           w120 = %xfoot($TotE02Fat);
162600100715           IF  w120 > 999999900;
162700100715             pTotFat = *all'9';
162800100715           ELSE;
162900100715             pTotFat = w120;
163000100715           ENDIF;
163100110113           w120 = %xfoot($TotE02Fate);
163200110113           IF  w120 > 999999900;
163300110113             pTotFate = *all'9';
163400110113           ELSE;
163500110113             pTotFate = w120;
163600110113           ENDIF;
163700100715           pTotSpe = %Xfoot($TotE02Spe);
163800110113           pTotSpee = %Xfoot($TotE02Spee);
163900100715           w60 = %xfoot($TotEPos);
164000100715           IF  w60 > 99900;
164100100715             pTotPos = *all'9';
164200100715           ELSE;
164300100715             pTotPos = w60;
164400100715           ENDIF;
164500110113           w60 = %xfoot($TotEPose);
164600110113           IF  w60 > 99900;
164700110113             pTotPose = *all'9';
164800110113           ELSE;
164900110113             pTotPose = w60;
165000110113           ENDIF;
165100100715           w120 = %xfoot($TotEPosF);
165200100715           IF  w120 > 999999900;
165300100715             pTotPFat = *all'9';
165400100715           ELSE;
165500100715             pTotPFat = w120;
165600100715           ENDIF;
165700110113           w120 = %xfoot($TotEPosFe);
165800110113           IF  w120 > 999999900;
165900110113             pTotPFate = *all'9';
166000110113           ELSE;
166100110113             pTotPFate = w120;
166200110113           ENDIF;
166300100715           pTotPSpe = %xfoot($TotEPosS);
166400110113           pTotPSpee = %xfoot($TotEPosSe);
166500110113           write TA98TOTC;
166600110113
166700110113         //?Di cui per tipo trattativa
166800110113           clear TA98TOTCD;
166900110113           ww = 1;
167000110113           FOR ww by 1 to %elem($Ttr);
167100110113             IF  $Ttr(ww) <> *blanks;
167200110113               pTtr      = %subst($Ttr(ww):3:1);
167300110113               pDesTtr   = %subst($Ttr(ww):4:25);
167400110113               w60       = $TotE02Tr(ww);
167500110113               IF  w60 > 99900;
167600110113                 pTotTrd = *all'9';
167700110113               ELSE;
167800110113                 pTotTrd = w60;
167900110113               ENDIF;
168000110113               w60       = $TotE02Tre(ww);
168100110113               IF  w60 > 99900;
168200110113                 pTotTrde = *all'9';
168300110113               ELSE;
168400110113                 pTotTrde = w60;
168500110113               ENDIF;
168600110113               w120      = $TotE02Fat(ww);
168700110113               IF  w120 > 999999900;
168800110113                 pTotFatd = *all'9';
168900110113               ELSE;
169000110113                 pTotFatd = w120;
169100110113               ENDIF;
169200110113               w120      = $TotE02Fate(ww);
169300110113               IF  w120 > 999999900;
169400110113                 pTotFatde = *all'9';
169500110113               ELSE;
169600110113                 pTotFatde = w120;
169700110113               ENDIF;
169800110113               pTotSped  = $TotE02Spe(ww);
169900110113               pTotSpede = $TotE02Spee(ww);
170000110113               w60       = $TotEPos(ww);
170100110113               IF  w60 > 99900;
170200110113                 pTotPosd = *all'9';
170300110113               ELSE;
170400110113                 pTotPosd = w60;
170500110113               ENDIF;
170600110113               w60       = $TotEPose(ww);
170700110113               IF  w60 > 99900;
170800110113                 pTotPosde = *all'9';
170900110113               ELSE;
171000110113                 pTotPosde = w60;
171100110113               ENDIF;
171200110113               w120      = $TotEPosF(ww);
171300110113               IF  w120 > 999999900;
171400110113                 pTotPFatd = *all'9';
171500110113               ELSE;
171600110113                 pTotPFatd = w120;
171700110113               ENDIF;
171800110113               w120      = $TotEPosFe(ww);
171900110113               IF  w120 > 999999900;
172000110113                 pTotPFatde = *all'9';
172100110113               ELSE;
172200110113                 pTotPFatde = w120;
172300110113               ENDIF;
172400110113               pTotPSped = $TotEPosS(ww);
172500110113               pTotPSpede = $TotEPosSe(ww);
172600110113               write TA98TOTCD;
172700110113               clear TA98TOTCD;
172800110113             ENDIF;
172900110113           ENDFOR;
173000110113
173100110113         //?di cui NEGATIVE
173200110113           clear TA98TOTC1;
173300100715           w60 = %xfoot($TotENeg);
173400100715           IF  w60 > 99900;
173500100715             pTotNeg = *all'9';
173600100715           ELSE;
173700100715             pTotNeg = w60;
173800100715           ENDIF;
173900110113           w60 = %xfoot($TotENege);
174000110113           IF  w60 > 99900;
174100110113             pTotNege = *all'9';
174200110113           ELSE;
174300110113             pTotNege = w60;
174400110113           ENDIF;
174500100715           w120 = %xfoot($TotENegF);
174600100715           IF  w120 > 999999900;
174700100715             pTotNFat = *all'9';
174800100715           ELSE;
174900100715             pTotNFat = w120;
175000100715           ENDIF;
175100110113           w120 = %xfoot($TotENegFe);
175200110113           IF  w120 > 999999900;
175300110113             pTotNFate = *all'9';
175400110113           ELSE;
175500110113             pTotNFate = w120;
175600110113           ENDIF;
175700100715           pTotNSpe = %xfoot($TotENegS);
175800110113           pTotNSpee = %xfoot($TotENegSe);
175900110113           write TA98TOTC1;
176000100714
176100100715         //?Di cui per tipo trattativa
176200110113           clear TA98TOTCD1;
176300100715           ww = 1;
176400100715           FOR ww by 1 to %elem($Ttr);
176500100715             IF  $Ttr(ww) <> *blanks;
176600110117               pTtr      = %subst($Ttr(ww):3:1);
176700110117               pDesTtr   = %subst($Ttr(ww):4:25);
176800100908               w60       = $TotENeg(ww);
176900100908               IF  w60 > 99900;
177000100908                 pTotNegd = *all'9';
177100100908               ELSE;
177200100908                 pTotNegd = w60;
177300100908               ENDIF;
177400110113               w60       = $TotENege(ww);
177500110113               IF  w60 > 99900;
177600110113                 pTotNegde = *all'9';
177700110113               ELSE;
177800110113                 pTotNegde = w60;
177900110113               ENDIF;
178000100908               w120      = $TotENegF(ww);
178100100908               IF  w120 > 999999900;
178200100908                 pTotNFatd = *all'9';
178300100908               ELSE;
178400100908                 pTotNFatd = w120;
178500100908               ENDIF;
178600110113               w120      = $TotENegFe(ww);
178700110113               IF  w120 > 999999900;
178800110113                 pTotNFatde = *all'9';
178900110113               ELSE;
179000110113                 pTotNFatde = w120;
179100110113               ENDIF;
179200100715               pTotNSped = $TotENegS(ww);
179300110113               pTotNSpede = $TotENegSe(ww);
179400110113               write TA98TOTCD1;
179500110113               clear TA98TOTCD1;
179600100715             ENDIF;
179700100715           ENDFOR;
179800100715         ENDIF;
179900100714
180000100714       //?Trattative IN CORSO
180100100715         IF  ITA98dtica > 0;
180200100715           clear TA98TOTX;
180300100715           ptDtica = pDticA;
180400100715           w60 = %xfoot($TotE03Tr);
180500100715           IF  w60 > 99900;
180600100715             pTotTr = *all'9';
180700100715           ELSE;
180800100715             pTotTr = w60;
180900100715           ENDIF;
181000110113           w60 = %xfoot($TotE03Tre);
181100110113           IF  w60 > 99900;
181200110113             pTotTre = *all'9';
181300110113           ELSE;
181400110113             pTotTre = w60;
181500110113           ENDIF;
181600100715           w120 = %xfoot($TotE03Fat);
181700100715           IF  w120 > 999999900;
181800100715             pTotFat = *all'9';
181900100715           ELSE;
182000100715             pTotFat = w120;
182100100715           ENDIF;
182200110113           w120 = %xfoot($TotE03Fate);
182300110113           IF  w120 > 999999900;
182400110113             pTotFate = *all'9';
182500110113           ELSE;
182600110113             pTotFate = w120;
182700110113           ENDIF;
182800100715           pTotSpe = %xfoot($TotE03Spe);
182900110113           pTotSpee = %xfoot($TotE03Spee);
183000100715           w60 = %xfoot($TotECaldo);
183100100715           IF  w60 > 99900;
183200100715             pTotCaldo = *all'9';
183300100715           ELSE;
183400100715             pTotCaldo = w60;
183500100715           ENDIF;
183600110113           w60 = %xfoot($TotECaldoe);
183700110113           IF  w60 > 99900;
183800110113             pTotCaldoe = *all'9';
183900110113           ELSE;
184000110113             pTotCaldoe = w60;
184100110113           ENDIF;
184200100715           w120 = %xfoot($TotECaldoF);
184300100715           IF  w120 > 999999900;
184400100715             pTotCalF = *all'9';
184500100715           ELSE;
184600100715             pTotCalF = w120;
184700100715           ENDIF;
184800110113           w120 = %xfoot($TotECaldoFe);
184900110113           IF  w120 > 999999900;
185000110113             pTotCalFe = *all'9';
185100110113           ELSE;
185200110113             pTotCalFe = w120;
185300110113           ENDIF;
185400100715           pTotCalS = %xfoot($TotECaldoS);
185500110113           pTotCalSe = %xfoot($TotECaldoSe);
185600100715           write TA98TOTX;
185700100714
185800100715         //?Di cui per tipo trattativa
185900100715           clear TA98TOTXD;
186000100715           ww = 1;
186100100715           FOR ww by 1 to %elem($Ttr);
186200100715             IF  $Ttr(ww) <> *blanks;
186300100715               pTtr       = %subst($Ttr(ww):3:1);
186400100715               pDesTtr    = %subst($Ttr(ww):4:25);
186500100908               w60        = $TotE03Tr(ww);
186600100908               IF  w60 > 99900;
186700100908                 pTotTrd = *all'9';
186800100908               ELSE;
186900100908                 pTotTrd = w60;
187000100908               ENDIF;
187100110113               w60        = $TotE03Tre(ww);
187200110113               IF  w60 > 99900;
187300110113                 pTotTrde = *all'9';
187400110113               ELSE;
187500110113                 pTotTrde = w60;
187600110113               ENDIF;
187700100908               w120       = $TotE03Fat(ww);
187800100908               IF  w120 > 999999900;
187900100908                 pTotFatd = *all'9';
188000100908               ELSE;
188100100908                 pTotFatd = w120;
188200100908               ENDIF;
188300110113               w120       = $TotE03Fate(ww);
188400110113               IF  w120 > 999999900;
188500110113                 pTotFatde = *all'9';
188600110113               ELSE;
188700110113                 pTotFatde = w120;
188800110113               ENDIF;
188900100715               pTotSped   = $TotE03Spe(ww);
189000110113               pTotSpede  = $TotE03Spee(ww);
189100100908               w60        = $TotECaldo(ww);
189200100908               IF  w60 > 99900;
189300100908                 pTotCaldod = *all'9';
189400100908               ELSE;
189500100908                 pTotCaldod = w60;
189600100908               ENDIF;
189700110113               w60        = $TotECaldoe(ww);
189800110113               IF  w60 > 99900;
189900110113                 pToCaldode = *all'9';
190000110113               ELSE;
190100110113                 pToCaldode = w60;
190200110113               ENDIF;
190300100908               w120       = $TotECaldoF(ww);
190400100908               IF  w120 > 999999900;
190500100908                 pTotCalFd = *all'9';
190600100908               ELSE;
190700100908                 pTotCalFd = w120;
190800100908               ENDIF;
190900110113               w120       = $TotECaldoFe(ww);
191000110113               IF  w120 > 999999900;
191100110113                 pTotCalFde = *all'9';
191200110113               ELSE;
191300110113                 pTotCalFde = w120;
191400110113               ENDIF;
191500100715               pTotCalSd  = $TotECaldoS(ww);
191600110113               pTotCalSde = $TotECaldoSe(ww);
191700100715               write TA98TOTXD;
191800100715               clear TA98TOTXD;
191900100715             ENDIF;
192000100715           ENDFOR;
192100100715         ENDIF;
192200100714
192300100714       ENDSR;
192400100716
192500100716       //--------------------------------------------------------------
192600100716       //?Stampo i totali generali di TUTTI i commerciali.
192700100716       //--------------------------------------------------------------
192800100716       BEGSR sr_StpTotGen;
192900100716
193000100716       //?Stampo la testata
193100100716         *in01 = *off;
193200100716         *in02 = *off;
193300100716         *in03 = *off;
193400100716         write TA98PAGINA;
193500100716         wrkriga = '*' + wTrat + '*';
193600100716         write TA98SEPARA;
193700100716         write TA98TESTAG;
193800100716         write TA98SEPARA;
193900100716
194000100716       //?Trattative AVVIATE
194100100716         IF  ITA98dtavi > 0;
194200100716           clear TA98TOTA;
194300100716           ptDtavI = pDtavI;
194400100716           ptDtavF = pDtavF;
194500100716           w60 = %xfoot($TotI01Tr) + %xfoot($TotE01Tr);
194600100716           IF  w60 > 99900;
194700100716             pTotTr = *all'9';
194800100716           ELSE;
194900100716             pTotTr = w60;
195000100716           ENDIF;
195100110113           w60 = %xfoot($TotI01Tre) + %xfoot($TotE01Tre);
195200110113           IF  w60 > 99900;
195300110113             pTotTre = *all'9';
195400110113           ELSE;
195500110113             pTotTre = w60;
195600110113           ENDIF;
195700100716           w120 = %xfoot($TotI01Fat) + %xfoot($TotE01Fat);
195800100716           IF  w120 > 999999900;
195900100716             pTotFat = *all'9';
196000100716           ELSE;
196100100716             pTotFat = w120;
196200100716           ENDIF;
196300110113           w120 = %xfoot($TotI01Fate) + %xfoot($TotE01Fate);
196400110113           IF  w120 > 999999900;
196500110113             pTotFate = *all'9';
196600110113           ELSE;
196700110113             pTotFate = w120;
196800110113           ENDIF;
196900100716           pTotSpe = %xfoot($TotI01Spe) + %xfoot($TotE01Spe);
197000110113           pTotSpee = %xfoot($TotI01Spee) + %xfoot($TotE01Spee);
197100100716           write TA98TOTA;
197200100716
197300100716         //?Di cui per tipo trattativa
197400100716           clear TA98TOTAD;
197500100716           ww = 1;
197600100716           FOR ww by 1 to %elem($Ttr);
197700100716             IF  $Ttr(ww) <> *blanks;
197800100716               pTtr     = %subst($Ttr(ww):3:1);
197900100716               pDesTtr  = %subst($Ttr(ww):4:25);
198000100908               w60      = $TotI01Tr(ww) + $TotE01Tr(ww);
198100100908               IF  w60 > 99900;
198200100908                 pTotTrd = *all'9';
198300100908               ELSE;
198400100908                 pTotTrd = w60;
198500100908               ENDIF;
198600110113               w60      = $TotI01Tre(ww) + $TotE01Tre(ww);
198700110113               IF  w60 > 99900;
198800110113                 pTotTrde = *all'9';
198900110113               ELSE;
199000110113                 pTotTrde = w60;
199100110113               ENDIF;
199200100908               w120     = $TotI01Fat(ww) + $TotE01Fat(ww);
199300100908               IF  w120 > 999999900;
199400100908                 pTotFatd = *all'9';
199500100908               ELSE;
199600100908                 pTotFatd = w120;
199700100908               ENDIF;
199800110113               w120     = $TotI01Fate(ww) + $TotE01Fate(ww);
199900110113               IF  w120 > 999999900;
200000110113                 pTotFatde = *all'9';
200100110113               ELSE;
200200110113                 pTotFatde = w120;
200300110113               ENDIF;
200400100716               pTotSped = $TotI01Spe(ww) + $TotE01Spe(ww);
200500110113               pTotSpede = $TotI01Spee(ww) + $TotE01Spee(ww);
200600100716               write TA98TOTAD;
200700100716               clear TA98TOTAD;
200800100716             ENDIF;
200900100716           ENDFOR;
201000100716         ENDIF;
201100100716
201200100716       //?Trattative CHIUSE
201300100716         IF  ITA98dtchi > 0;
201400110113         //?totale + Di cui POSITIVE
201500100716           clear TA98TOTC;
201600100716           ptDtchI = pDtchI;
201700100716           ptDtchF = pDtchF;
201800100716           w60 = %xfoot($TotI02Tr) + %xfoot($TotE02Tr);
201900100716           IF  w60 > 99900;
202000100716             pTotTr = *all'9';
202100100716           ELSE;
202200100716             pTotTr = w60;
202300100716           ENDIF;
202400110113           w60 = %xfoot($TotI02Tre) + %xfoot($TotE02Tre);
202500110113           IF  w60 > 99900;
202600110113             pTotTre = *all'9';
202700110113           ELSE;
202800110113             pTotTre = w60;
202900110113           ENDIF;
203000100716           w120 = %xfoot($TotI02Fat) + %xfoot($TotE02Fat);
203100100716           IF  w120 > 999999900;
203200100716             pTotFat = *all'9';
203300100716           ELSE;
203400100716             pTotFat = w120;
203500100716           ENDIF;
203600110113           w120 = %xfoot($TotI02Fate) + %xfoot($TotE02Fate);
203700110113           IF  w120 > 999999900;
203800110113             pTotFate = *all'9';
203900110113           ELSE;
204000110113             pTotFate = w120;
204100110113           ENDIF;
204200100716           pTotSpe  = %xfoot($TotI02Spe) + %xfoot($TotE02Spe);
204300110113           pTotSpee = %xfoot($TotI02Spee) + %xfoot($TotE02Spee);
204400100716           w60 = %xfoot($TotIPos) + %xfoot($TotEPos);
204500100716           IF  w60 > 99900;
204600100716             pTotPos = *all'9';
204700100716           ELSE;
204800100716             pTotPos = w60;
204900100716           ENDIF;
205000110113           w60 = %xfoot($TotIPose) + %xfoot($TotEPose);
205100110113           IF  w60 > 99900;
205200110113             pTotPose = *all'9';
205300110113           ELSE;
205400110113             pTotPose = w60;
205500110113           ENDIF;
205600100716           w120 = %xfoot($TotIPosF) + %xfoot($TotEPosF);
205700100716           IF  w120 > 999999900;
205800100716             pTotPFat = *all'9';
205900100716           ELSE;
206000100716             pTotPFat = w120;
206100100716           ENDIF;
206200110113           w120 = %xfoot($TotIPosFe) + %xfoot($TotEPosFe);
206300110113           IF  w120 > 999999900;
206400110113             pTotPFate = *all'9';
206500110113           ELSE;
206600110113             pTotPFate = w120;
206700110113           ENDIF;
206800100716           pTotPSpe = %xfoot($TotIPosS) + %xfoot($TotEPosS);
206900110113           pTotPSpee = %xfoot($TotIPosSe) + %xfoot($TotEPosSe);
207000110113           write TA98TOTC;
207100110113
207200110113         //?Di cui per tipo trattativa
207300110113           clear TA98TOTCD;
207400110113           ww = 1;
207500110113           FOR ww by 1 to %elem($Ttr);
207600110113             IF  $Ttr(ww) <> *blanks;
207700110113               pTtr      = %subst($Ttr(ww):3:1);
207800110113               pDesTtr   = %subst($Ttr(ww):4:25);
207900110113               w60       = $TotI02Tr(ww)  + $TotE02Tr(ww);
208000110113               IF  w60 > 99900;
208100110113                 pTotTrd = *all'9';
208200110113               ELSE;
208300110113                 pTotTrd = w60;
208400110113               ENDIF;
208500110113               w60       = $TotI02Tre(ww)  + $TotE02Tre(ww);
208600110113               IF  w60 > 99900;
208700110113                 pTotTrde = *all'9';
208800110113               ELSE;
208900110113                 pTotTrde = w60;
209000110113               ENDIF;
209100110113               w120      = $TotI02Fat(ww) + $TotE02Fat(ww);
209200110113               IF  w120 > 999999900;
209300110113                 pTotFatd = *all'9';
209400110113               ELSE;
209500110113                 pTotFatd = w120;
209600110113               ENDIF;
209700110113               w120      = $TotI02Fate(ww) + $TotE02Fate(ww);
209800110113               IF  w120 > 999999900;
209900110113                 pTotFatde = *all'9';
210000110113               ELSE;
210100110113                 pTotFatde = w120;
210200110113               ENDIF;
210300110113               pTotSped  = $TotI02Spe(ww) + $TotE02Spe(ww);
210400110113               pTotSpede = $TotI02Spee(ww) + $TotE02Spee(ww);
210500110113               w60       = $TotIPos(ww)   + $TotEPos(ww);
210600110113               IF  w60 > 99900;
210700110113                 pTotPosd = *all'9';
210800110113               ELSE;
210900110113                 pTotPosd = w60;
211000110113               ENDIF;
211100110113               w60       = $TotIPose(ww)   + $TotEPose(ww);
211200110113               IF  w60 > 99900;
211300110113                 pTotPosde = *all'9';
211400110113               ELSE;
211500110113                 pTotPosde = w60;
211600110113               ENDIF;
211700110113               w120      = $TotIPosF(ww)  + $TotEPosF(ww);
211800110113               IF  w120 > 999999900;
211900110113                 pTotPFatd = *all'9';
212000110113               ELSE;
212100110113                 pTotPFatd = w120;
212200110113               ENDIF;
212300110113               w120      = $TotIPosFe(ww)  + $TotEPosFe(ww);
212400110113               IF  w120 > 999999900;
212500110113                 pTotPFatde = *all'9';
212600110113               ELSE;
212700110113                 pTotPFatde = w120;
212800110113               ENDIF;
212900110113               pTotPSped = $TotIPosS(ww)  + $TotEPosS(ww);
213000110113               pTotPSpede = $TotIPosSe(ww)  + $TotEPosSe(ww);
213100110113               write TA98TOTCD;
213200110113               clear TA98TOTCD;
213300110113             ENDIF;
213400110113           ENDFOR;
213500110113
213600110113         //?di cui NEGATIVE
213700110113           clear TA98TOTC1;
213800100716           w60 = %xfoot($TotINeg) + %xfoot($TotENeg);
213900100716           IF  w60 > 99900;
214000100716             pTotNeg = *all'9';
214100100716           ELSE;
214200100716             pTotNeg = w60;
214300100716           ENDIF;
214400110113           w60 = %xfoot($TotINege) + %xfoot($TotENege);
214500110113           IF  w60 > 99900;
214600110113             pTotNege = *all'9';
214700110113           ELSE;
214800110113             pTotNege = w60;
214900110113           ENDIF;
215000100716           w120 = %xfoot($TotINegF) + %xfoot($TotENegF);
215100100716           IF  w120 > 999999900;
215200100716             pTotNFat = *all'9';
215300100716           ELSE;
215400100716             pTotNFat = w120;
215500100716           ENDIF;
215600110113           w120 = %xfoot($TotINegFe) + %xfoot($TotENegFe);
215700110113           IF  w120 > 999999900;
215800110113             pTotNFate = *all'9';
215900110113           ELSE;
216000110113             pTotNFate = w120;
216100110113           ENDIF;
216200100716           pTotNSpe = %xfoot($TotINegS) + %xfoot($TotENegS);
216300110113           pTotNSpee = %xfoot($TotINegSe) + %xfoot($TotENegSe);
216400110113           write TA98TOTC1;
216500100716
216600100716         //?Di cui per tipo trattativa
216700110113           clear TA98TOTCD1;
216800100716           ww = 1;
216900100716           FOR ww by 1 to %elem($Ttr);
217000100716             IF  $Ttr(ww) <> *blanks;
217100110117               pTtr      = %subst($Ttr(ww):3:1);
217200110117               pDesTtr   = %subst($Ttr(ww):4:25);
217300100908               w60       = $TotINeg(ww)   + $TotENeg(ww);
217400100908               IF  w60 > 99900;
217500100908                 pTotNegd = *all'9';
217600100908               ELSE;
217700100908                 pTotNegd = w60;
217800100908               ENDIF;
217900110113               w60       = $TotINege(ww)   + $TotENege(ww);
218000110113               IF  w60 > 99900;
218100110113                 pTotNegde = *all'9';
218200110113               ELSE;
218300110113                 pTotNegde = w60;
218400110113               ENDIF;
218500100908               w120      = $TotINegF(ww)  + $TotENegF(ww);
218600100908               IF  w120 > 999999900;
218700100908                 pTotNFatd = *all'9';
218800100908               ELSE;
218900100908                 pTotNFatd = w120;
219000100908               ENDIF;
219100110113               w120      = $TotINegFe(ww)  + $TotENegFe(ww);
219200110113               IF  w120 > 999999900;
219300110113                 pTotNFatde = *all'9';
219400110113               ELSE;
219500110113                 pTotNFatde = w120;
219600110113               ENDIF;
219700100716               pTotNSped = $TotINegS(ww)  + $TotENegS(ww);
219800110113               pTotNSpede = $TotINegSe(ww)  + $TotENegSe(ww);
219900110113               write TA98TOTCD1;
220000110113               clear TA98TOTCD1;
220100100716             ENDIF;
220200100716           ENDFOR;
220300100716         ENDIF;
220400100716
220500100716       //?Trattative IN CORSO
220600100716         IF  ITA98dtica > 0;
220700100716           clear TA98TOTX;
220800100716           ptDtica = pDticA;
220900100716           w60 = %xfoot($TotI03Tr) + %xfoot($TotE03Tr);
221000100716           IF  w60 > 99900;
221100100716             pTotTr = *all'9';
221200100716           ELSE;
221300100716             pTotTr = w60;
221400100716           ENDIF;
221500110113           w60 = %xfoot($TotI03Tre) + %xfoot($TotE03Tre);
221600110113           IF  w60 > 99900;
221700110113             pTotTre = *all'9';
221800110113           ELSE;
221900110113             pTotTre = w60;
222000110113           ENDIF;
222100100716           w120 = %xfoot($TotI03Fat) + %xfoot($TotE03Fat);
222200100716           IF  w120 > 999999900;
222300100716             pTotFat = *all'9';
222400100716           ELSE;
222500100716             pTotFat = w120;
222600100716           ENDIF;
222700110113           w120 = %xfoot($TotI03Fate) + %xfoot($TotE03Fate);
222800110113           IF  w120 > 999999900;
222900110113             pTotFate = *all'9';
223000110113           ELSE;
223100110113             pTotFate = w120;
223200110113           ENDIF;
223300100716           pTotSpe = %xfoot($TotI03Spe) + %xfoot($TotE03Spe);
223400110113           pTotSpee = %xfoot($TotI03Spee) + %xfoot($TotE03Spee);
223500100716           w60 = %xfoot($TotICaldo) + %xfoot($TotECaldo);
223600100716           IF  w60 > 99900;
223700100716             pTotCaldo = *all'9';
223800100716           ELSE;
223900100716             pTotCaldo = w60;
224000100716           ENDIF;
224100110113           w60 = %xfoot($TotICaldoe) + %xfoot($TotECaldoe);
224200110113           IF  w60 > 99900;
224300110113             pTotCaldoe = *all'9';
224400110113           ELSE;
224500110113             pTotCaldoe = w60;
224600110113           ENDIF;
224700100716           w120 = %xfoot($TotICaldoF) + %xfoot($TotECaldoF);
224800100716           IF  w120 > 999999900;
224900100716             pTotCalF = *all'9';
225000100716           ELSE;
225100100716             pTotCalF = w120;
225200100716           ENDIF;
225300110113           w120 = %xfoot($TotICaldoFe) + %xfoot($TotECaldoFe);
225400110113           IF  w120 > 999999900;
225500110113             pTotCalFe = *all'9';
225600110113           ELSE;
225700110113             pTotCalFe = w120;
225800110113           ENDIF;
225900100716           pTotCalS = %xfoot($TotICaldoS) + %xfoot($TotECaldoS);
226000110113           pTotCalSe = %xfoot($TotICaldoSe) + %xfoot($TotECaldoSe);
226100100716           write TA98TOTX;
226200100716
226300100716         //?Di cui per tipo trattativa
226400100716           clear TA98TOTXD;
226500100716           ww = 1;
226600100716           FOR ww by 1 to %elem($Ttr);
226700100716             IF  $Ttr(ww) <> *blanks;
226800100716               pTtr       = %subst($Ttr(ww):3:1);
226900100716               pDesTtr    = %subst($Ttr(ww):4:25);
227000100908               w60        = $TotI03Tr(ww)   + $TotE03Tr(ww);
227100100908               IF  w60 > 99900;
227200100908                 pTotTrd = *all'9';
227300100908               ELSE;
227400100908                 pTotTrd = w60;
227500100908               ENDIF;
227600110113               w60        = $TotI03Tre(ww)   + $TotE03Tre(ww);
227700110113               IF  w60 > 99900;
227800110113                 pTotTrde = *all'9';
227900110113               ELSE;
228000110113                 pTotTrde = w60;
228100110113               ENDIF;
228200100908               w120       = $TotI03Fat(ww)  + $TotE03Fat(ww);
228300100908               IF  w120 > 999999900;
228400100908                 pTotFatd = *all'9';
228500100908               ELSE;
228600100908                 pTotFatd = w120;
228700100908               ENDIF;
228800110113               w120       = $TotI03Fate(ww)  + $TotE03Fate(ww);
228900110113               IF  w120 > 999999900;
229000110113                 pTotFatde = *all'9';
229100110113               ELSE;
229200110113                 pTotFatde = w120;
229300110113               ENDIF;
229400100716               pTotSped   = $TotI03Spe(ww)  + $TotE03Spe(ww);
229500110113               pTotSpede  = $TotI03Spee(ww)  + $TotE03Spee(ww);
229600100908               w60        = $TotICaldo(ww)  + $TotECaldo(ww);
229700100908               IF  w60 > 99900;
229800100908                 pTotCaldod = *all'9';
229900100908               ELSE;
230000100908                 pTotCaldod = w60;
230100100908               ENDIF;
230200110113               w60        = $TotICaldoe(ww)  + $TotECaldoe(ww);
230300110113               IF  w60 > 99900;
230400110113                 pToCaldode = *all'9';
230500110113               ELSE;
230600110113                 pToCaldode = w60;
230700110113               ENDIF;
230800100908               w120       = $TotICaldoF(ww) + $TotECaldoF(ww);
230900100908               IF  w120 > 999999900;
231000100908                 pTotCalFd = *all'9';
231100100908               ELSE;
231200100908                 pTotCalFd = w120;
231300100908               ENDIF;
231400110113               w120       = $TotICaldoFe(ww) + $TotECaldoFe(ww);
231500110113               IF  w120 > 999999900;
231600110113                 pTotCalFde = *all'9';
231700110113               ELSE;
231800110113                 pTotCalFde = w120;
231900110113               ENDIF;
232000100716               pTotCalSd  = $TotICaldoS(ww) + $TotECaldoS(ww);
232100110113               pTotCalSde = $TotICaldoSe(ww) + $TotECaldoSe(ww);
232200100716               write TA98TOTXD;
232300100716               clear TA98TOTXD;
232400100716             ENDIF;
232500100716           ENDFOR;
232600100716         ENDIF;
232700100716
232800100716       ENDSR;
232900100714
233000100714       //--------------------------------------------------------------
233100100714       //?Operazioni finali.
233200100714       //--------------------------------------------------------------
233300100714       BEGSR RoutEnd;
233400100714
233500100714         *inLR = *on;
233600100714         return;
233700100714
233800100714       ENDSR;
233900100714
234000100714       //--------------------------------------------------------------
234100100714       //?Elaboro le trattative.
234200100714       //--------------------------------------------------------------
234300100714       BEGSR sr_Elabora;
234400100714
234500100714       //?Stampo le trattative in ordine come da tabella TTR
234600100714         ww = 1;
234700100714         FOR ww by 1 to %elem($Ttr);
234800100714           IF  $Ttr(ww) <> *blanks;
234900100714             *in12 = *off;
235000110118             *in15 = *off;
235100100810             clear pCodTtr;
235200100810             $Ttrok = *off;
235300110118           //?Se NO info trattative non deve stampare importi
235400110118             IF %subst($Ttr(ww):29:1) = 'N';
235500100714               *in12 = *on;
235600100714             ENDIF;
235700110118           //?Se Tipo trattativa 'A' non deve stampare le spedizoni
235800110118             IF %subst($Ttr(ww):3:1) = 'A';
235900110118               *in15 = *on;
236000110118             ENDIF;
236100100714             exsr sr_StpTratta;
236200100714           //?Stampo i totali per tipo trattativa
236300100714           //?solo se richiesto il dettaglio
236400100714             IF  pTtrTr > 0 and ITA98det = 'S' and $TtrOk and $DetCmm;
236500100714
236600100714             //?Salto pagina
236700100714               IF  CurLine >= 61;
236800100714                 exsr sr_StpSalto;
236900100714                 *in99 = *off;
237000100714               ENDIF;
237100100714               wrkriga = '*' + wTrat + '*';
237200100714               write TA98SEPARA;
237300100714               write TA98TTR;
237400110222              // IF  pTtrTre > 0;
237500110112                 write TA98TTRE;
237600110222              // ENDIF;
237700100714               wrkriga = '*' + wTrat + '*';
237800100714               write TA98SEPARA;
237900100921             ENDIF;
238000100714
238100100908             //?Memorizzo i totali in sk x unificante
238200100921             SELECT;
238300100921               WHEN  *in01;
238400100921                 $Uni01Tr(ww)  += pTtrTr;
238500100921                 $Uni01Fat(ww) += pTtrFat;
238600100921                 $Uni01Spe(ww) += pTtrSpe;
238700110113                 $Uni01Tre(ww)  += pTtrTre;
238800110113                 $Uni01Fate(ww) += pTtrFate;
238900110113                 $Uni01Spee(ww) += pTtrSpee;
239000100921               WHEN  *in02;
239100100921                 $Uni02Tr(ww)  += pTtrTr;
239200100921                 $Uni02Fat(ww) += pTtrFat;
239300100921                 $Uni02Spe(ww) += pTtrSpe;
239400110113                 $Uni02Tre(ww)  += pTtrTre;
239500110113                 $Uni02Fate(ww) += pTtrFate;
239600110113                 $Uni02Spee(ww) += pTtrSpee;
239700100921               WHEN  *in03;
239800100921                 $Uni03Tr(ww)  += pTtrTr;
239900100921                 $Uni03Fat(ww) += pTtrFat;
240000100921                 $Uni03Spe(ww) += pTtrSpe;
240100110113                 $Uni03Tre(ww)  += pTtrTre;
240200110113                 $Uni03Fate(ww) += pTtrFate;
240300110113                 $Uni03Spee(ww) += pTtrSpee;
240400100921             ENDSL;
240500100714
240600100714             clear pTtrTr;
240700100714             clear pTtrFat;
240800100714             clear pTtrSpe;
240900110112             clear pTtrTre;
241000110112             clear pTtrFate;
241100110112             clear pTtrSpee;
241200100714           ENDIF;
241300100714         ENDFOR;
241400100714
241500100714       ENDSR;
241600100714
241700100714       //--------------------------------------------------------------
241800100714       //?Stampo le trattative.
241900100714       //--------------------------------------------------------------
242000100714       BEGSR sr_StpTratta;
242100100714
242200100714         $End = *off;
242300100715         clear wConta;
242400100714
242500100714         //?Imposto la stringa per SQL
242600100714         exsr  sr_PrepSQL;
242700100714
242800100714         //?Dichiarazione cursore
242900100714         exec sql
243000100714           prepare S1   from :wSQL;
243100100714         exec sql
243200100714           declare WRK  cursor for S1;
243300100714
243400100714         //?Apertura del cursore
243500100714         exec sql
243600100714           open WRK;
243700100714
243800100714         IF sqlcode < 0;
243900100714           $End = *on;
244000100714         ENDIF;
244100100714
244200100714         DOW not $End;
244300100714           exec sql
244400100714             fetch next from WRK into :wfrctds;
244500100714           IF sqlcod = 100 or sqlcod < 0;
244600100714             $End = *on;
244700100714             leave;
244800100714           ENDIF;
244900100714
245000100714         //?Controllo se commerciale da stampare
245100100714           exsr sr_CtrCmmOk;
245200100714
245300100714         //?Controllo se tipo trattativa da stampare
245400100714           exsr sr_CtrTtrOk;
245500100714
245600100714        //?Solo se richiesto il dettaglio
245700100714           IF  ITA98det = 'S' and $TtrOk and $DetCmm;
245800100714         //?Se mai stampata o è salto pagina
245900100714         //?faccio il salto pagina
246000100714             IF  wConta = 0 or CurLine >= 65;
246100100714             //?Stampo salto pagina
246200100714               exsr sr_StpSalto;
246300100714               wConta += 1;
246400100714               *in99 = *off;
246500100714             ENDIF;
246600100714           //?A cambio tipo trattativa
246700100715             IF  %subst($Ttr(ww):3:1) <> pCodTtr;
246800100714             //?Stampo separazione per tipo trattativa
246900100715               exsr sr_StpTipoTtr;
247000100715             ENDIF;
247100100714           ENDIF;
247200100714
247300100714         //?Stampo la trattative
247400100714           exsr sr_StpDettaglio;
247500100714         //?Stampo le INFO
247600100714           exsr sr_StpINFO;
247700100915         //?Stampo i concorrenti  +  codice cliente + vecchio convalidato
247800100714           exsr sr_StpConc;
247900100714         //?Stampo se cliente caldo
248000100714           exsr sr_StpCaldo;
248100100714         //?Stampo le attività scadute e la prossima da fare
248200100714           exsr sr_StpAttivita;
248300100714
248400100714         //?Sommo il nr. trattative
248500100714           PTtrTr += 1;
248600110112         //?Sommo il nr. trattative con traffico estero per 'N' e 'I'
248700110112           IF  (RCTttr = 'I' or RCTttr = 'N') and
248800110112                RCTfate > 0;
248900110112             PTtrTre += 1;
249000110112           ENDIF;
249100100714
249200100714         ENDDO;
249300100714
249400100714         exec sql
249500100714           close WRK;
249600100714
249700100714       ENDSR;
249800100714
249900100714       //--------------------------------------------------------------
250000100714       //?Preparazione stringa SQL.
250100100714       //--------------------------------------------------------------
250200100714       BEGSR sr_PrepSQL;
250300100714
250400100714         wSQL = 'select * from WFRCT00F +
250500100714                 where rctrgf = ' +
250600100714                 %editc($AgeU(zz):'X');
250700100714
250800100714       //?trattative avviate-chiuse-in corso
250900100714         IF  *in01;
251000100714           wSQL += ' and rcttrc = ''A''';
251100100714         ENDIF;
251200100714         IF  *in02;
251300100714           wSQL += ' and rcttrc = ''C''';
251400100714         ENDIF;
251500100714         IF  *in03;
251600100714           wSQL += ' and rcttrc = ''I''';
251700100714         ENDIF;
251800100714
251900100714       //?Solo il tipo trattativa che sto leggendo
252000100714         wSQL += ' and rctttr =  ''' + %subst($Ttr(ww):3:1) + '''';
252100100714
252200100714       //?ordinamento richiesto a video
252300100714         SELECT;
252400100714       //? ragione sociale
252500100714           WHEN  ITA98ord = 'R';
252600100715             wSQL += ' order by RCTrag, RCTnrv';
252700100714       //? data avvio
252800100714           WHEN  ITA98ord = 'D';
252900100715             wSQL += ' order by RCTdavt, RCTrag';
253000100714       //? valore traffico totale
253100100714           WHEN  ITA98ord = 'V';
253200100716             wSQL += ' order by RCTfatt desc, RCTrag';
253300100714         ENDSL;
253400100714
253500100714         wSQL += ' for fetch only';
253600100714
253700100714       ENDSR;
253800100714
253900100714       //--------------------------------------------------------------
254000100714       //?Controllo se commerciale da stampare.
254100100714       //--------------------------------------------------------------
254200100714       BEGSR sr_CtrCmmOk;
254300100714
254400100714         $DetCmm = *on;
254500100714
254600100714       //?Stampa solo dei commerciali Interni
254700101006         IF  ITA98detc = 'I' and RCTfun <> 'COMIN' and
254800101006             RCTfun <> 'ASC' and RCTfun <> 'SA';
254900100714           $DetCmm = *off;
255000100714         ENDIF;
255100100714
255200100714       //?Stampa solo dei commerciali Esterni
255300101006         IF  ITA98detc = 'E' and (RCTfun = 'COMIN' or
255400101006             RCTfun = 'ASC' or RCTfun = 'SA');
255500100714           $DetCmm = *off;
255600100714         ENDIF;
255700100715
255800100715       //?Salvo la funzione del commerciale
255900100715         wFun = RCTfun;
256000100714
256100100714       ENDSR;
256200100714
256300100714       //--------------------------------------------------------------
256400100714       //?Controllo se tipo trattativa da stampare.
256500100714       //--------------------------------------------------------------
256600100714       BEGSR Sr_CtrTtrOk;
256700100714
256800100714         $TtrOk = *on;
256900100714
257000100714         IF  ITA98ttrio = 'I' and
257100100714            RCTttr <> ITA98ttr1 and
257200100714            RCTttr <> ITA98ttr2 and
257300100714            RCTttr <> ITA98ttr3 and
257400110112            RCTttr <> ITA98ttr4 and
257500110112            RCTttr <> ITA98ttr5 and
257600110112            RCTttr <> ITA98ttr6;
257700100714           $TtrOk = *off;
257800100714         ENDIF;
257900100714
258000100714         IF  ITA98ttrio = 'O' and
258100100714            (RCTttr = ITA98ttr1 or
258200100714             RCTttr = ITA98ttr2 or
258300100714             RCTttr = ITA98ttr3 or
258400110112             RCTttr = ITA98ttr4 or
258500110112             RCTttr = ITA98ttr5 or
258600110112             RCTttr = ITA98ttr6);
258700100714           $TtrOk = *off;
258800100714         ENDIF;
258900100714
259000100714       ENDSR;
259100100714
259200100714       //--------------------------------------------------------------
259300100714       //?Stampo il salto pagina.
259400100714       //--------------------------------------------------------------
259500100714       BEGSR sr_StpSalto;
259600100714
259700100714         pCodUni = RCTrgf;
259800100714         pDesUni = RCTdrg;
259900100714
260000100714         //?Stampo la testata
260100100714         write TA98PAGINA;
260200100714         wrkriga = '*' + wTrat + '*';
260300100714         write TA98SEPARA;
260400100714         write TA98TESTA;
260500100714         write TA98SEPARA;
260600100714         //?Stampo le intestazioni delle colonne
260700100714         write TA98INTCOL;
260800100714
260900100714       ENDSR;
261000100714
261100100714       //--------------------------------------------------------------
261200100714       //?Stampo la separazione per tipo trattativa.
261300100714       //--------------------------------------------------------------
261400100714       BEGSR sr_StpTipoTtr;
261500100714
261600100714         pCodTtr = %subst($Ttr(ww):3:1);
261700100714         pDesTtr = %subst($Ttr(ww):4:25);
261800100714
261900100714       //?Salto pagina
262000100714         IF  CurLine >= 63;
262100100714           exsr  sr_StpSalto;
262200100714           *in99 = *off;
262300100714         ENDIF;
262400100714
262500100714         clear wrkriga;
262600100714         write TA98SEPARA;
262700100714         write TA98TESTTR;
262800100714         write TA98SEPARA;
262900100714
263000100714       ENDSR;
263100100714
263200100714       //--------------------------------------------------------------
263300100714       //?Stampo le trattative.
263400100714       //--------------------------------------------------------------
263500100714       BEGSR sr_StpDettaglio;
263600100714
263700100714         clear pKsc;
263800100714         clear pOfferta;
263900100714         clear pClv;
264000100714         clear pVisDat;
264100100714         clear pDesCmmi;
264200100714         clear pVisDch;
264300100714         clear pDesEsi;
264400120919         clear pNrapp;
264500120919         clear pAtcDco;
264600100714
264700100714         //?CHIODO 'A' e 'M' come tipo trattativa
264800110530         //?Se trattativa di tipo Aumento/Sconto, Mantenimento,
264900110530         //?Incremento o Restyling
265000100714         //?stampo il codice cliente
265100110530         IF  RCTttr = 'A' or RCTttr = 'M' or
265200110530             RCTttr = 'I' or RCTttr = 'R';
265300100714           pKsc = RCTksc;
265400100714         ENDIF;
265500100714
265600100714         //?stampo se c'è offerta
265700100714         IF  RCToff = 'S';
265800100714           pOfferta = 'OFF';
265900100714         ENDIF;
266000100714
266100100714         //?Importanza cliente/potenziale
266200100714         pClv = RCTtic;
266300100714
266400100714         //?Data apertura trattativa
266500100714         wDate_ISO = %date(RCTdavt:*iso);
266600100714         wDate_EUR = wDate_ISO;
266700100714         pVisDat   = %dec(wDate_EUR);
266800100714
266900100714         //?Commerciale interno
267000100714         pDesCmmi = RCTdcmmi;
267100100714
267200100714         //?Filiale gestione trattativa
267300100714         pFilCmm = RCTfgs;
267400100714
267500100714         //?Conto gli appuntamenti
267600100714         IF  RCtnrapp > 0;
267700100714           wDate_ISO = %date(RCTduapp:*iso);
267800100714           wDate_EUR = wDate_ISO;
267900100714           pAtcDco   = %dec(wDate_EUR);
268000100714           pNrApp    = RCTnrapp;
268100100714         ENDIF;
268200100714
268300100714         //?Data chiusura trattativa
268400100714         IF  RCTdch > 0;
268500100714           wDate_ISO = %date(RCTdch:*iso);
268600100714           wDate_EUR = wDate_ISO;
268700100714           pVisDch   = %dec(wDate_EUR);
268800100714         ENDIF;
268900100714
269000100714         //?Esito trattativa
269100100714         IF  RCTepn = 'P ';
269200100714           pDesEsi = 'POSITIVO';
269300100714         //?Se sto elaborando le trattative CHIUSE
269400100714         //?conto direttamente in sk per Unificante
269500100714           IF  *in02;
269600100714             $UniPos(ww) += 1;
269700100714             $UniPosF(ww) += RCTfatt;
269800100714             $UniPosS(ww) += RCTnspt;
269900110607           //?di cui estero
270000110607             IF  RCTfate > 0;
270100110607               $UniPose(ww) += 1;
270200110607               $UniPosFe(ww) += RCTfate;
270300110607               $UniPosSe(ww) += RCTnspe;
270400110607             ENDIF;
270500100714           ENDIF;
270600100714         ENDIF;
270700100714         IF  RCTepn = 'N ';
270800100714           pDesEsi = 'NEGATIVO';
270900100714           clear TIBS02ds;
271000100714           clear dCCO;
271100100714           T02mod = 'C';
271200100714           T02cod = 'CCO';
271300100714           T02ke1 = RCTesi;
271400100714           T02sif = KNSIF;
271500100714           TNTBE_RicercaControllo (kpjba : tibs02ds);
271600100714           IF  T02err = *blanks;
271700100714             dCCO = T02uni;
271800100714           ENDIF;
271900100714           pDesEsi = §CCOdes;
272000100714         //?Se sto elaborando le trattative CHIUSE
272100100714         //?conto direttamente in sk per Unificante
272200100714           IF  *in02;
272300100714             $UniNeg(ww) += 1;
272400100714             $UniNegF(ww) += RCTfatt;
272500100714             $UniNegS(ww) += RCTnspt;
272600110607           //?di cui estero
272700110607             IF  RCTfate > 0;
272800110607               $UniNege(ww) += 1;
272900110607               $UniNegFe(ww) += RCTfate;
273000110607               $UniNegSe(ww) += RCTnspe;
273100110607             ENDIF;
273200100714           ENDIF;
273300100714         ENDIF;
273400100714
273500100714         //?Se richiesto il dettaglio
273600100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
273700100714           //?Salto pagina
273800100716           IF  CurLine >= 61;
273900100714             exsr  sr_StpSalto;
274000100714             exsr  sr_StpTipoTtr;
274100100714             *in99 = *off;
274200100714           ENDIF;
274300100714
274400100714           write TA98RIGA;
274500100714         ENDIF;
274600100714
274700100714       ENDSR;
274800100714
274900100714       //--------------------------------------------------------------
275000100714       //?Stampo le INFO.
275100100714       //--------------------------------------------------------------
275200100714       BEGSR sr_StpINFO;
275300100714
275400100714         clear pFatTot;
275500100714         clear pFatEst;
275600100714         clear pSpeTot;
275700100714         clear pSpeEst;
275800110114         clear pAuSc;
275900100714         clear pDelta;
276000100714         clear pPkg;
276100100714
276200100714         //?Dati traffico/spedizioni totale
276300110113         IF  RCTfatt <> 0;
276400180220           PFatTot = 'Valore Tratt.: ' + %trim(%editc(RCTfatt:'K'));
276500180220           pSpeTot = 'Sped. Tratt.: ' +
276600180220                       %trim(%editc(RCTnspt:'2'));
276700100715           pTtrFat += RCTfatt;
276800100715           PTtrSpe += RCTnspt;
276900100715         ENDIF;
277000100714
277100100714         //?Dati traffico/spedizioni estere
277200100715         IF  RCTfate > 0;
277300110113           PFatEst = 'Di cui estero: ' + %trim(%editc(RCTfate:'2'));
277400180220           pSpeEst = 'Di cui estero: ' + %trim(%editc(RCTnspe:'2'));
277500110112           //?Se trattativa di tipo 'N' o 'I' sommo anche il traffico estero
277600110112           IF  RCTttr = 'N' or RCTttr = 'I';
277700110112             pTtrFate += RCTfate;
277800110112             PTtrSpee += RCTnspe;
277900110112           ENDIF;
278000100715         ENDIF;
278100110114
278200110114         //?Dati Aumento/Sconto
278300110114         IF RCTausc <> 0;
278400110114           pAuSc = 'Aum./Sc.: ' + %trim(%editc(RCTausc:'K')) + ' %';
278500110114         ENDIF;
278600100714
278700100715         //?Dati delta
278800100715         IF RCTdel <> *blanks;
278900100715           pDelta = 'Delta Previsto:';
279000110114           pDelta = %trim(pDelta) + ' ' + %trim(RCTdel) +
279100110114                    RCTdels + ' %';
279200100715         ENDIF;
279300100714
279400100714         //?Dati peso
279500100715         IF  RCTpkg > 0;
279600110113           pPkg = 'Peso medio Sped.: ' + %trim(%editc(RCTpkg:'Z')) +
279700100715                    ' Kg.';
279800100715         ENDIF;
279900100714
280000100714         //?Se richiesto il dettaglio
280100100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
280200100714           //?Salto pagina
280300100714           IF  CurLine >= 65;
280400100714             exsr sr_StpSalto;
280500100714             exsr sr_StpTipoTtr;
280600100714             *in99 = *off;
280700100714           ENDIF;
280800100714
280900100714           IF  pFatTot <> *blanks;
281000100714             write TA98INFO;
281100100714           ENDIF;
281200100714         ENDIF;
281300100714
281400100714       ENDSR;
281500100714
281600100714       //--------------------------------------------------------------
281700100714       //?Stampo i Concorrenti.
281800100714       //--------------------------------------------------------------
281900100714       BEGSR sr_StpConc;
282000100714
282100100714         clear pConc;
282200100714         clear wConc;
282300100714
282400100714         IF  RCTconc1 <> *blanks;
282500100714           pConc = 'Concorrenti:';
282600100714           wConc = RCTconc1;
282700100714           exsr sr_DecConc;
282800100714           pConc = %trim(pConc) + ' ' + §ITSdes;
282900100714         ENDIF;
283000100714
283100100714         IF  RCTconc2 <> *blanks;
283200100714           wConc = RCTconc2;
283300100714           exsr sr_DecConc;
283400100915         //?Aggiungo concorrente fino al massimo di 158 byte
283500100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
283600100714             Pconc = %trim(Pconc) + ', Altri...';
283700100714           ELSE;
283800100714             pConc = %trim(pConc) + ', ' + §ITSdes;
283900100714           ENDIF;
284000100714         ENDIF;
284100100714
284200100714         IF  RCTconc3 <> *blanks;
284300100714           wConc = RCTconc3;
284400100714           exsr sr_DecConc;
284500100915         //?Aggiungo concorrente fino al massimo di 158 byte
284600100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
284700100714             Pconc = %trim(Pconc) + ', Altri...';
284800100714           ELSE;
284900100714             pConc = %trim(pConc) + ', ' + §ITSdes;
285000100714           ENDIF;
285100100714         ENDIF;
285200100714
285300100714         IF  RCTconc4 <> *blanks;
285400100714           wConc = RCTconc4;
285500100714           exsr sr_DecConc;
285600100915         //?Aggiungo concorrente fino al massimo di 158 byte
285700100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
285800100714             Pconc = %trim(Pconc) + ', Altri...';
285900100714           ELSE;
286000100714             pConc = %trim(pConc) + ', ' + §ITSdes;
286100100714           ENDIF;
286200100714         ENDIF;
286300100714
286400100714         IF  RCTconc5 <> *blanks;
286500100714           wConc = RCTconc5;
286600100714           exsr sr_DecConc;
286700100915         //?Aggiungo concorrente fino al massimo di 158 byte
286800100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
286900100714             Pconc = %trim(Pconc) + ', Altri...';
287000100714           ELSE;
287100100714             pConc = %trim(pConc) + ', ' + §ITSdes;
287200100714           ENDIF;
287300100714         ENDIF;
287400100714
287500100714         IF  RCTconc6 <> *blanks;
287600100714           wConc = RCTconc6;
287700100714           exsr sr_DecConc;
287800100915         //?Aggiungo concorrente fino al massimo di 158 byte
287900100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
288000100714             Pconc = %trim(Pconc) + ', Altri...';
288100100714           ELSE;
288200100714             pConc = %trim(pConc) + ', ' + §ITSdes;
288300100714           ENDIF;
288400100714         ENDIF;
288500100714
288600100714         IF  RCTconc7 <> *blanks;
288700100714           wConc = RCTconc7;
288800100714           exsr sr_DecConc;
288900100915         //?Aggiungo concorrente fino al massimo di 158 byte
289000100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
289100100714             Pconc = %trim(Pconc) + ', Altri...';
289200100714           ELSE;
289300100714             pConc = %trim(pConc) + ', ' + §ITSdes;
289400100714           ENDIF;
289500100714         ENDIF;
289600100714
289700100714         IF  RCTconc8 <> *blanks;
289800100714           wConc = RCTconc8;
289900100714           exsr sr_DecConc;
290000100915         //?Aggiungo concorrente fino al massimo di 158 byte
290100100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
290200100714             Pconc = %trim(Pconc) + ', Altri...';
290300100714           ELSE;
290400100714             pConc = %trim(pConc) + ', ' + §ITSdes;
290500100714           ENDIF;
290600100714         ENDIF;
290700100714
290800100714         IF  RCTconc9 <> *blanks;
290900100714           wConc = RCTconc9;
291000100714           exsr sr_DecConc;
291100100915         //?Aggiungo concorrente fino al massimo di 158 byte
291200100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
291300100714             Pconc = %trim(Pconc) + ', Altri...';
291400100714           ELSE;
291500100714             pConc = %trim(pConc) + ', ' + §ITSdes;
291600100714           ENDIF;
291700100714         ENDIF;
291800100714
291900100714         IF  RCTconc10 <> *blanks;
292000100714           wConc = RCTconc10;
292100100714           exsr sr_DecConc;
292200100915         //?Aggiungo concorrente fino al massimo di 158 byte
292300100915           IF  (%len(%trim(pConc)) + %len(%trim(§ITSdes)) + 2) > 157;
292400100714             Pconc = %trim(Pconc) + ', Altri...';
292500100714           ELSE;
292600100714             pConc = %trim(pConc) + ', ' + §ITSdes;
292700100714           ENDIF;
292800100714         ENDIF;
292900100915
293000100915         //?In caso di stampa trattative CHIUSA - NUOVA stampo
293100100915         //?il codice cliente + vecchio convalidato
293200100915         clear pKscNew;
293300100915         IF  *in02 and RCTttr = 'N' and RCTksc > 0;
293400100915           pKscNew = 'Codificato: ' +
293500100915                      %editc(RCTksc:'Z');
293600100915         ENDIF;
293700100714
293800100714         //?Se richiesto il dettaglio
293900100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
294000100714           //?Salto pagina
294100100714           IF  CurLine >= 65;
294200100714             exsr sr_StpSalto;
294300100714             exsr sr_StpTipoTtr;
294400100714             *in99 = *off;
294500100714           ENDIF;
294600100714
294700100714           IF  pConc <> *blanks;
294800100714             write TA98CONC;
294900100714           ENDIF;
295000100714         ENDIF;
295100100714
295200100714       ENDSR;
295300100714
295400100714       //--------------------------------------------------------------
295500100714       //?Stampo se cliente caldo.
295600100714       //--------------------------------------------------------------
295700100714       BEGSR sr_StpCaldo;
295800100714
295900100714       //?Se esiste offerta accettata imposto cliente caldo
296000100714         IF  RCTcaldo = 'S';
296100100714           wDate_ISO = %date(RCTdpaff:*iso);
296200100714           wDate_EUR = wDate_ISO;
296300100714           pVOFdpa   = %dec(wDate_EUR);
296400100714
296500100714         //?Se richiesto il dettaglio
296600100810         IF  ITA98det = 'S' and $TtrOk and $DetCmm;
296700100714             //?Salto pagina
296800100714             IF  CurLine >= 65;
296900100714              exsr sr_StpSalto;
297000100714              exsr sr_StpTipoTtr;
297100100714               *in99 = *off;
297200100714             ENDIF;
297300100714
297400100714             write TA98CALDO;
297500100714           ENDIF;
297600100714
297700100714         //?Se sto elaborando le trattative IN CORSO
297800100714           IF  *in03;
297900100714         //?conto se caldo direttamente in sk per Unificante
298000100714             $UniCaldo(ww) += 1;
298100100714             $UniCaldoF(ww) += RCTfatt;
298200100714             $UniCaldoS(ww) += RCTnspt;
298300100714           ENDIF;
298400100714
298500100714         ENDIF;
298600100714
298700100714       ENDSR;
298800100714
298900100714       //--------------------------------------------------------------
299000100714       //?Stampo le attività scadute e la prossima da fare.
299100100714       //--------------------------------------------------------------
299200100714       BEGSR sr_StpAttivita;
299300100714
299400100714         *in10 = *off;
299500100714         *in11 = *off;
299600100714
299700100714       //?Attività scadute
299800100714         IF  RCTnrscad > 0;
299900100714           *in10 = *on;
300000100715           pNrAttSc = RCTnrscad;
300100100714         ELSE;
300200100714       //?Attività future
300300100714           IF  RCTtat <> *blanks;
300400100714             *in11 = *on;
300500100714             pTatNew = RCTtat;
300600100714             wDate_ISO = %date(RCTdad:*iso);
300700100714             wDate_EUR = wDate_ISO;
300800100714             pDadNew   = %dec(wDate_EUR);
300900100714             pHdaNew   = %dec(%subst(%editc(RCThda:'X'):1:4):4:0);
301000100714             pCauNew   = RCTcad;
301100100714         //?Decodifico la causale attività
301200100714             clear TIBS02ds;
301300100714             clear dCCO;
301400100714             T02mod = 'C';
301500100714             T02cod = 'CCO';
301600100714             T02ke1 = pCauNew;
301700100714             T02sif = KNSIF;
301800100714             TNTBE_RicercaControllo (kpjba : tibs02ds);
301900100714             IF  T02err = *blanks;
302000100714               dCCO = T02uni;
302100100714             ENDIF;
302200100714             pDCauNew = §CCOdes;
302300100714           ENDIF;
302400100714         ENDIF;
302500100714
302600100715       //?Se richiesto il dettaglio
302700100715       //?Stampo riga attività scadute e futura
302800100810         IF  ITA98det = 'S' and  (*in10 or *in11) and
302900100810             $TtrOk and $DetCmm;
303000100715         //?Salto pagina
303100100715           IF  CurLine >= 65;
303200100715             exsr sr_StpSalto;
303300100715             exsr sr_StpTipoTtr;
303400100715             *in99 = *off;
303500100715           ENDIF;
303600100715           write TA98ATTIV;
303700100714         ENDIF;
303800100714
303900100714       ENDSR;
304000100714
304100100714       //--------------------------------------------------------------
304200100714       //?Decodifico i concorrenti.
304300100714       //--------------------------------------------------------------
304400100714       BEGSR Sr_DecConc;
304500100714
304600100714         clear TIBS02ds;
304700100714         clear dITS;
304800100714         T02mod = 'C';
304900100714         T02cod = 'ITS';
305000100714         T02ke1 = '10';
305100100714         T02ke2 = wConc;
305200100714         T02sif = KNSIF;
305300100714         TNTBE_RicercaControllo (kpjba : tibs02ds);
305400100714         IF  T02err = *blanks;
305500100714           dITS = T02uni;
305600100714         ENDIF;
305700100714
305800100714       ENDSR;
305900100714
306000100714       //--------------------------------------------------------------
306100100714       //?Reperimento Dati del job (Utente/Operativi).
306200100714       //--------------------------------------------------------------
306300100714       BEGSR DatiJob;
306400100714
306500100714         in(E) §AzUte;
306600100714         if NOT %error;
306700100714           in(E) §DatiUte;
306800100714         endif;
306900100714         if %error or RSut = *blanks;
307000100714           clear TIBS34ds;
307100100714           tibs34r(tibs34ds);
307200100714           in §AzUte;
307300100714           in §DatiUte;
307400100714         endif;
307500100714
307600100714       //?Richiamo pgm di controllo autorizzazioni utente così mi torna
307700100714       //?il codice autorizzazione relativo alle tariffe clienti
307800100714       //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
307900100714         reset TNTAA1DS;
308000100714         ITAA1caut = '§utegtc';
308100100714         kpjbu     = TNTAA1DS;
308200100714         tntaa1c (kpjba);
308300100714         TNTAA1DS = kpjbu;
308400100714
308500150910       //?Se utente abilitato come 'AZ' mi carico
308600150910       //?la sk delle filiali del distretto
308700110922         IF  OTAA1cabi = 'AZ'and DUTpou <> 046;
308800150910           OTAA1cabi = 'DI';
308900100714         ENDIF;
309000100714
309100100714       //?Carico filiali abilitate all'utente
309200100714       //?Non controllo se caricate o no le filiali
309300100714       //?tanto se non ho caricato nessuna filiale e la richiesta
309400100714       //?è per tutti i commerciali, non stampo niente
309500100714         clear TRUL31DS;
309600110119         clear TRUL31DS2;
309700110119         *in09 = *off;
309800100714         I31abi = OTAA1cabi;
309900100714         I31cdi = DUTdis;
310000100714         I31car = DUTare;
310100100714         I31cpo = DUTpou;
310200110119         trul31r (kpjba : trul31ds : trul31ds2);
310300110119         IF  car(2) > *zeros;
310400110119           *in09 = *on;
310500110119         ENDIF;
310600100714
310700100714       ENDSR;
310800100714
310900100714       //--------------------------------------------------------------
311000100714       //?Carico sk agenti unificanti.
311100100714       //--------------------------------------------------------------
311200100714       BEGSR sr_CarAgeUni;
311300100714
311400100714       //?Carico gli agenti unificanti delle filiali gestite dall'utente
311500100714         clear xx;
311600100714         clear $AGEU;
311700100714         $EndAge = *off;
311800100714
311900100714       //?Imposto la stringa per SQL
312000130905         wSQL = 'select CMMcod +
312100130905                   from AZCMM00F +
312200130905                  where substr(digits(CMMuni), 1, 3) in (';
312300100714
312400100714         yy = 0;
312500100714         xx = 1;
312600100714         FOR xx by 1 to %elem(pog);
312700100714           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
312800100714             IF yy > 0;
312900100714               wSQL += ', ';
313000100714             ELSE;
313100100714               yy = 1;
313200100714             ENDIF;
313300100714             wSQL += '''';
313400100714             wSQL += pog(xx);
313500100714             wSQL += '''';
313600100714           ENDIF;
313700100714         ENDFOR;
313800100714
313900130905         wSQL += ') and CMMcod = CMMuni +
314000130905                    and CMMpar = '' ''';
314100100714
314200100714       //?Controllo funzione commerciale da elaborare
314300100714         IF  ITA98elcmm = 'I';
314400130905           wSQL += ' and CMMfun in (''COMIN'', +
314500130905                                    ''ASC  '', +
314600130905                                    ''SA   '')';
314700100714         ENDIF;
314800100714         IF  ITA98elcmm = 'E';
314900130905           wSQL += ' and CMMfun NOT in (''COMIN'', +
315000130905                                        ''ASC  '', +
315100130905                                        ''SA   '')';
315200100714         ENDIF;
315300100714
315400100714       //?Ordino per codice commerciale
315500130905         wSQL += ' order by CMMcod +
315600130905                     for fetch only';
315700100714
315800100714       //?Dichiarazione cursore
315900100714         exec sql
316000100714           prepare A1   from :wSQL;
316100100714         exec sql
316200100714           declare AGEU cursor for A1;
316300100714
316400100714       //?Apertura del cursore
316500100714         exec sql
316600100714           open AGEU;
316700100714
316800100714         IF sqlcode < 0;
316900100714           $EndAge = *on;
317000100714         ENDIF;
317100100714
317200100714       //?Carico la schiera degli agenti unificanti
317300100714         yy = 0;
317400100714         DOW not $EndAge;
317500100714           exec sql
317600130905             fetch next from AGEU into :CMMcod;
317700100714           IF sqlcod = 100 or sqlcod < 0;
317800100714             $EndAge = *on;
317900100714             leave;
318000100714           ENDIF;
318100100714
318200130905           yy += 1;
318300130905           $AGEU(yy) = CMMcod;
318400100714
318500100714         ENDDO;
318600100714
318700100714         exec sql
318800100714           close AGEU;
318900100714
319000100714       ENDSR;
319100100714
319200100714      /end-free
