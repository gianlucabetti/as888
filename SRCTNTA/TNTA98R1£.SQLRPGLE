000100080206      //--------------------------------------------------------------
000200100708      //?TNTA98R1 - Elenco trattative - crea file work
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
001400100308     fTIVII01L  if   e           k disk
001500100308     fTIVOF01L  if   e           k disk
001600100308     fTNCPO01L  if   e           k disk
001700100527     fWFRCT00F  o    e             disk
001800100310
001900080206      //---------------------------------------------------------------
002000090406      //?Definizione costanti.
002100080206      //---------------------------------------------------------------
002200080206
002300080206      //---------------------------------------------------------------
002400080206      //?Definizione schiere.
002500080206      //---------------------------------------------------------------
002600100325
002700100325      // - Agenti
002800100325     d $Age            s              7  0 dim(3500)
002900100325
003000100325      // - Agenti Unificanti
003100100325     d $AgeU           s              7  0 dim(3500)
003200080206
003300080206      //---------------------------------------------------------------
003400080206      //?Definizione aree dati.
003500080206      //---------------------------------------------------------------
003600080206
003700080206      // - Dati utente
003800080206     d §AzUte        e ds                  extname(AZUTE00F)
003900080206     d                                     dtaara
004000080206     d §DatiUte      e ds                  extname(dDatiUte)
004100080206     d                                     dtaara
004200080206
004300080206      //---------------------------------------------------------------
004400080206      //?Definizione strutture dati.
004500080206      //---------------------------------------------------------------
004600080206
004700080206      // - Parametri ricevuti
004800080206     d KPJBA         e ds
004900100524     d TNTA98DS      e ds
005000130905     d   ITA98conc            90    109
005100130905     d   $Conc                90    109    dim(5)
005200130905     d   ITA98tpt            124    129
005300130905     d   $Tpt                124    129    dim(6)
005400130905     d   ITA98tic            130    134
005500130905     d   $Tic                130    134    dim(5)
005600080206
005700100308      // - Ricerca/Controllo tabelle
005800100308     d TIBS02ds      e ds                  inz
005900100308
006000080206      // - Reperimento dati utente
006100100305     d TIBS34DS      e ds
006200100308
006300100308       // - Reperimento dati anagrafici
006400100308     d TIBS69ds      e ds
006500100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
006600100308     d DS_cnind      e ds                  inz extname(CNIND00F)
006700100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
006800100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
006900100326
007000100326      // - controllo abilitazioni
007100100326     d TNTAA1DS      e ds                  inz
007200100325
007300100325      // - Carico Filiali abilitate all'utente
007400100325     d TRUL31DS      e ds
007500130905     d   POG                  10    759    DIM(250)
007600110119      // - Carico Aree abilitate all'utente
007700110119     d TRUL31DS2     e ds
007800100325
007900100325      // - Tabella CCO = Causale contatto
008000100325     d dCCO          e ds                  inz
008100100308
008200100324      // - Tabella ITS = Sottotipi INFO trattativa
008300130905     d*// dITS          e ds                  inz
008400100526
008500100527      // - Tabella 05 = Codici Area
008600100527     d ds05          e ds                  inz
008700100526
008800100526      // - Tabella 17 = Direzioni commerciali
008900100526     d ds17          e ds                  inz
009000101015
009100101015      // - VISFLO file TIVIS00F
009200101015     d dVIS01        e ds
009300090807
009400090807      // File attività
009500100805     d TIATCDS       e ds                  extname(TIATC00F)
009600100305
009700100305      // File trattativa
009800100324     d TIVISDS       e ds                  extname(TIVIS00F)
009900130905
010000130905      // File anagrafica commerciali
010100130905     d AZCMMds       e ds                  extname(AZCMM00F)  inz
010200090629
010300080206      //---------------------------------------------------------------
010400080206      //?Definizione variabili globali.
010500080206      //---------------------------------------------------------------
010600080206
010700080206      // - Flags booleani
010800100305     d $End            s               n   inz(*off)
010900100325     d $EndAge         s               n   inz(*off)
011000100326     d $EndAtt         s               n   inz(*off)
011100100305     d $RcdOk          s               n   inz(*off)
011200100325
011300100325      // - Indici di schiera
011400100326     d xx              s              4  0 inz
011500100326     d yy              s              4  0 inz
011600100326     d zz              s              4  0 inz
011700080206
011800090806       // - Stringa SQL da eseguire
011900090806     d wSQL            s           2048    Varying        inz
012000100305
012100100305      // - Campi di comodo
012200110505     d wausi           s              5  2 inz
012300110505     d wausf           s              5  2 inz
012400100915     d wdata           s              8  0 inz
012500100715     d wdel            s              5  0 inz
012600100715     d wdeli           s              5  0 inz
012700100715     d wdelf           s              5  0 inz
012800100329     d wOggi           s              8  0 inz
012900130905     d w1$AgeU         s              7  0 inz
013000100325
013100090508      //---------------------------------------------------------------
013200090807      //?Definizione procedure esterne.
013300090508      //---------------------------------------------------------------
013400100120
013500110119      // - Filiali e aree abilitate
013600110119     d trul31r         pr                  extpgm('TRUL31R')
013700110119     d  kpjba                              likeds(KPJBA)
013800110119     d  trul31ds                           likeds(trul31ds)
013900110119     d  trul31ds2                          likeds(trul31ds2)
014000100120
014100100120      //---------------------------------------------------------------
014200100120      //?prototipi
014300100120      //---------------------------------------------------------------
014400090807
014500080626      /copy gaitrasrc/srcprotopr,tibs02r
014600080626      /copy gaitrasrc/srcprotopr,tibs34r
014700100308      /copy gaitrasrc/srcprotopr,tibs69r
014800100326      /copy gaitrasrc/srcprotopr,tntaa1c
014900090807
015000080206      //---------------------------------------------------------------
015100080206      //?Definizione key-list.
015200080206      //---------------------------------------------------------------
015300100325
015400100325       // - File TABEL00F
015500100325     d k03tabel      e ds                  extname(TABEL00F:*key)
015600100325     d                                     prefix(k_)
015700100325     d                                     inz
015800080206
015900080206      //---------------------------------------------------------------
016000080206      //?Riepilogo indicatori.
016100080206      //---------------------------------------------------------------
016200100709
016300100709      // 01    : Condizione stampa trattative AVVIATE
016400100709      // 02    : Condizione stampa trattative CHIUSE
016500100709      // 03    : Condizione stampa trattative IN CORSO
016600090807
016700080206      //---------------------------------------------------------------
016800080206
016900080206      //---------------------------------------------------------------
017000080206      //?M A I N - L I N E
017100080206      //---------------------------------------------------------------
017200080206
017300080206     c     *Entry        plist
017400080206     c                   parm                    KPJBA
017500080206
017600080206      /free
017700080206
017800090807       //?Operazioni iniziali
017900080206       exsr RoutInz;
018000100524
018100100709       //?Ciclo per sk unificanti
018200100524       zz = 1;
018300100524       FOR zz by 1 to %elem($AgeU);
018400100524         IF  $AgeU(zz) <> *zeros;
018500100524
018600100524         //?Carico sk agenti dell'unificante
018700100524           exsr sr_CarAge;
018800100708
018900100709         //?Elaboro AVVIATE
019000100709           IF  ITA98dtavi <> 0 and ITA98dtavf <> 0;
019100100709             exsr sr_Avviate;
019200100709           ENDIF;
019300100709
019400100709         //?Elaboro CHIUSE
019500100712           IF  ITA98dtchi <> 0 and ITA98dtchf <> 0;
019600100712             exsr sr_Chiuse;
019700100712           ENDIF;
019800100709
019900100709         //?Elaboro IN CORSO
020000100712           IF  ITA98dtica <> 0;
020100100712             exsr sr_InCorso;
020200100712           ENDIF;
020300100527
020400100524         ENDIF;
020500100524       ENDFOR;
020600080206
020700090807       //?Operazioni finali
020800080206       exsr RoutEnd;
020900080206
021000080206       //--------------------------------------------------------------
021100080206       //?Operazioni iniziali.
021200080206       //--------------------------------------------------------------
021300080206       BEGSR RoutInz;
021400100329
021500100709       //?Imposto la ds con i dati della KPJBU
021600100524         TNTA98ds = kpjbu;
021700090807
021800100709       //?Reperimento dati job
021900090807         exsr DatiJob;
022000090807
022100090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022200100527
022300100709       //?Imposto dati fissi
022400100527         k_tblkut = 1;
022500100305
022600100712       //?Imposto data elaborazione
022700100712         wOggi = %dec(%date());
022800100325
022900100325         clear $AgeU;
023000100325
023100100709       //?Carico in sk l'unificante richiesto
023200100524         IF  ITA98cmm > 0;
023300100524           $AgeU(01) = ITA98cmm;
023400100325         ENDIF;
023500100325
023600100709       //?Carico gli unificanti delle filiali gestite dall'utente
023700110119       //?o dell'area se richiesta
023800100524         IF  ITA98cmm = 0;
023900100325           exsr sr_CarAgeUni;
024000100325           sorta $AgeU;
024100100325         ENDIF;
024200090714
024300080206       ENDSR;
024400100524
024500100326       //--------------------------------------------------------------
024600100326       //?Carico Sk Agenti.
024700100326       //--------------------------------------------------------------
024800100326       BEGSR sr_CarAge;
024900100326
025000100326         clear xx;
025100100326         clear $Age;
025200100326         $EndAge = *off;
025300130905         w1$AgeU = $AgeU(zz);
025400100326
025500100326         exec sql
025600100326          declare AGE cursor for
025700130905          select *
025800130905            from AZCMM00F
025900130905           where CMMuni = :w1$AgeU
026000130905           order by CMMcod;
026100100326
026200100326         exec sql
026300100326           open AGE;
026400100326           IF sqlcode < 0;
026500100326             $EndAge = *on;
026600100326           ENDIF;
026700100326
026800100326         DOW not $EndAge;
026900100326           exec sql
027000130905             fetch next from AGE into :AZCMMds;
027100100326             IF sqlcod = 100 or sqlcod < 0;
027200100326               $EndAge = *on;
027300100326               leave;
027400100326             ENDIF;
027500130905             IF  CMMuni = $AgeU(zz);
027600100326               xx += 1;
027700130905               $Age(xx) = CMMcod;
027800100326             ENDIF;
027900100326         ENDDO;
028000100326
028100100326         exec sql close AGE;
028200100326
028300130905       //?Se non ho caricato niente memorizzo almeno il codice richiesto
028400100326         IF $Age(01) = 0;
028500100326           $Age(01) = $AgeU(zz);
028600100326         ENDIF;
028700100326
028800100326       ENDSR;
028900100709
029000100709       //--------------------------------------------------------------
029100100709       //?Elaboro le trattative avviate.
029200100709       //--------------------------------------------------------------
029300100709       BEGSR sr_Avviate;
029400100709
029500100709       //?Imposto flag per identificare che sto elaborando le
029600100709       //?trattative AVVIATE
029700100709         *in01 = *on;
029800100709         *in02 = *off;
029900100709         *in03 = *off;
030000100709
030100100709       //?Elabora le trattative
030200100709         exsr sr_Elabora;
030300100709
030400100709       ENDSR;
030500100525
030600100709       //--------------------------------------------------------------
030700100709       //?Elaboro le trattative chiuse.
030800100709       //--------------------------------------------------------------
030900100709       BEGSR sr_Chiuse;
031000100709
031100100709       //?Imposto flag per identificare che sto elaborando le
031200100709       //?trattative CHIUSE
031300100709         *in01 = *off;
031400100709         *in02 = *on;
031500100709         *in03 = *off;
031600100709
031700100709       //?Elabora le trattative
031800100709         exsr sr_Elabora;
031900100709
032000100709       ENDSR;
032100100709
032200100709       //--------------------------------------------------------------
032300100709       //?Elaboro le trattative in corso.
032400100709       //--------------------------------------------------------------
032500100709       BEGSR sr_InCorso;
032600100709
032700100709       //?Imposto flag per identificare che sto elaborando le
032800100709       //?trattative IN CORSO
032900100709         *in01 = *off;
033000100709         *in02 = *off;
033100100709         *in03 = *on;
033200100709
033300100709       //?Elabora le trattative
033400100709         exsr sr_Elabora;
033500100709
033600100709       ENDSR;
033700100709
033800100525       //--------------------------------------------------------------
033900100525       //?Elaboro le trattative.
034000100525       //--------------------------------------------------------------
034100100525       BEGSR sr_Elabora;
034200100526
034300100526         $End = *off;
034400100526
034500100709       //?Imposto la stringa per SQL
034600100708         exsr  sr_PrepSQL;
034700100708
034800100709       //?Dichiarazione cursore
034900100708         exec sql
035000100708           prepare S1   from :wSQL;
035100100708         exec sql
035200100708           declare VIS  cursor for S1;
035300100526
035400100709       //?Apertura del cursore
035500100526         exec sql
035600100526           open VIS;
035700100526
035800100526         IF sqlcode < 0;
035900100526           $End = *on;
036000100526         ENDIF;
036100100526
036200100526         DOW not $End;
036300100526           exec sql
036400100526             fetch next from VIS into :tivisds;
036500100526           IF sqlcod = 100 or sqlcod < 0;
036600100526             $End = *on;
036700100526             leave;
036800100526           ENDIF;
036900100709
037000100709       //?Carico i dati nel file di work
037100100709           exsr sr_CarDati;
037200100526
037300100526         ENDDO;
037400100526
037500100526         exec sql
037600100526           close VIS;
037700100526
037800100526       ENDSR;
037900100525
038000100525       //--------------------------------------------------------------
038100100525       //?Preparazione stringa SQL.
038200100525       //--------------------------------------------------------------
038300100525       BEGSR sr_PrepSQL;
038400100525
038500100709       //?Seleziono trattative NON fittizie del commerciale che sto elaborando
038600100525         wSQL = 'select * from TIVIS00F +
038700100709                 where visffz = '' '' and viscmm in(';
038800100525         yy = 0;
038900100525         xx = 1;
039000100525         FOR xx by 1 to %elem($Age);
039100100525           IF $Age(xx) <> *zeros;
039200100525             IF yy > 0;
039300100525               wSQL += ', ';
039400100525             ELSE;
039500100525               yy = 1;
039600100525             ENDIF;
039700100525             wSQL += %editc($Age(xx):'X');
039800100525           ENDIF;
039900100525         ENDFOR;
040000100709
040100100709         //?Trattative avviate
040200100709         IF  *in01;
040300100709           wSQL += ') and visdat between ' + %editc(ITA98dtavi:'X') +
040400100709                   ' and ' + %editc(ITA98dtavf:'X');
040500100709         ENDIF;
040600100709
040700100709         //?Trattative chiuse
040800100709         IF  *in02;
040900100709           wSQL += ') and visdch between ' + %editc(ITA98dtchi:'X') +
041000100709                   ' and ' + %editc(ITA98dtchf:'X');
041100100709         ENDIF;
041200100709
041300100709         //?Trattative in corso
041400100709         IF  *in03;
041500100709           wSQL += ') and visdat <= ' + %editc(ITA98dtica:'X') +
041600100714                   ' and (visdch = 0 or visdch > ' +
041700100714                     %editc(ITA98dtica:'X') + ')';
041800100709         ENDIF;
041900100709
042000100709       //?Seleziono i tipi trattativa richiesti
042100100716         IF  ITA98tpt <> *blanks;
042200100709           wSQL += ' and vistpv in(';
042300100709           yy = 0;
042400100709           xx = 1;
042500100709           FOR xx by 1 to %elem($Tpt);
042600100709             IF $Tpt(xx) <> *blanks;
042700100709               IF yy > 0;
042800100709                 wSQL += ', ';
042900100709               ELSE;
043000100709                 yy = 1;
043100100709               ENDIF;
043200100716               wSQL += '''' + $Tpt(xx) + '''';
043300100709             ENDIF;
043400100709           ENDFOR;
043500100716           wSQL += ')';
043600100709         ENDIF;
043700100525
043800100525       ENDSR;
043900100709
044000100709       //--------------------------------------------------------------
044100100709       //?Carico i dati legati alla trattativa.
044200100709       //--------------------------------------------------------------
044300100709       BEGSR sr_CarDati;
044400100709
044500100709         clear WFRCT000;
044600100915         wdata = *all'9';
044700101015         dVIS01 = VISflo;
044800100709
044900100709       //?Verifico il tipo cliente
045000100709       //?Vale per tutte e tre le elaborazioni
045100100709         SELECT;
045200100709         //?Da anagrafica cliente
045300100709           WHEN  VISksc > 0;
045400100709             clear tibs69ds;
045500100709             I69kcp = VISksc;
045600100709             tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
045700100709             IF  O69err = *blanks;
045800100709               RCTtic = CLPclv;
045900100709             ENDIF;
046000100709         //?Da anagrafica potenziale
046100100709           WHEN  VIScpo <> 0;
046200100709             chain VIScpo TNCPO01L;
046300100709             IF  %found(TNCPO01L);
046400100709               RCTtic = CPOftr;
046500100709             ENDIF;
046600100709         ENDSL;
046700100715       //?Se tipo cliente non previsto in schiera NON scrivo file
046800100716         IF  ITA98tic <> *blanks and
046900100716             %lookup(RCTtic : $Tic) = 0;
047000100716           leavesr;
047100100709         ENDIF;
047200100716         IF  ITA98tic <> *blanks and RCTtic = *blanks;
047300100716           leavesr;
047400100716         ENDIF;
047500100709
047600100709       //?Verifico INFO trattativa
047700100709       //?Vale per tutte e tre le elaborazioni
047800100709         exsr sr_INFO;
047900100709         IF  not $RcdOk;
048000100709           leavesr;
048100100709         ENDIF;
048200100709
048300100709       //?Verifico se CALDA (offerta accettata)
048400100709       //?Vale solo per elaborazione trattative IN CORSO
048500110113       //?devo considerare anche il CALDO senza offerte (999)
048600100709         IF  *in03;
048700100709           setll VISnrv TIVOF01L;
048800100709           reade VISnrv TIVOF01L;
048900100709           DOW not %eof(TIVOF01L);
049000100709           //?Offerta accettata
049100100709             IF  VOFeso = 'A';
049200100709               RCTcaldo = 'S';
049300100709               RCTdpaff = VOFdpa;
049400100709               leave;
049500100709             ENDIF;
049600110113             IF  VOFctr = 999 and VOFdpa > 0;
049700110113               RCTcaldo = 'S';
049800110113               RCTdpaff = VOFdpa;
049900110113               leave;
050000110113             ENDIF;
050100100709             reade VISnrv TIVOF01L;
050200100709           ENDDO;
050300100713         //?Tengo se trattativa calda
050400100709           IF  ITA98calde = 'S' and RCTcaldo = *blanks;
050500100709             leavesr;
050600100709           ENDIF;
050700100709         //?Tengo se calda e data presunto affidamento entro data richiesta
050800100709           IF  ITA98dtaff > 0 and RCTdpaff > ITA98dtaff;
050900100709             leavesr;
051000100709           ENDIF;
051100100709         ENDIF;
051200100709
051300100709       //?Verifico esito trattativa
051400100709       //?Vale solo per elaborazione trattative CHIUSE
051500100709         IF  *in02;
051600100709         //?Positivo/Negativo
051700101015           IF  VISesi = 'P ' or §VISesito = 'P';
051800100709             RCTepn = 'P';
051900100709           ELSE;
052000100709             RCTepn = 'N';
052100100709           ENDIF;
052200100709         //?Tengo solo se esito richiesto
052300100709           IF  ITA98esi = 'N' and RCTepn = 'P';
052400100709             leavesr;
052500100709           ENDIF;
052600100709           IF  ITA98esi = 'P' and RCTepn = 'N';
052700100709             leavesr;
052800100709           ENDIF;
052900100709         //?Causale esito
053000100709           RCTesi = VISesi;
053100100709         //?Decodifico esito
053200100709           clear TIBS02ds;
053300100709           clear dCCO;
053400100709           T02mod = 'C';
053500100709           T02cod = 'CCO';
053600100709           T02ke1 = VISesi;
053700100709           T02sif = KNSIF;
053800100709           TNTBE_RicercaControllo (kpjba : tibs02ds);
053900100709           IF  T02err = *blanks;
054000100709             dCCO = T02uni;
054100100709           ENDIF;
054200100709           RCTdesi = §CCOdes;
054300100709         ENDIF;
054400100709
054500100709       //?Imposto dati comuni
054600100709         exsr sr_DatiComuni;
054700100709
054800100709       //?Imposto dati relativi alla trattativa
054900100709         RCTnrv = VISnrv;
055000100709         RCTrag = VISrag;
055100100709         RCTksc = VISksc;
055200100709         RCTttr = VIStpv;
055300100709         RCTdavt = VISdat;
055400100709         RCTcmmi = VIScmmi;
055500100709       //?Decodifico commerciale interno
055600130905         chain  (VIScmmi)  AZCMM000;
055700130905         IF  %found( AZCMM01L );
055800130905           RCTdcmmi = CMMdes;
055900100709         ENDIF;
056000100709         RCTfgs  = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0);
056100100709         RCTdch = VISdch;
056200100709
056300100709       //?Imposto se c'è offerta
056400100915       //?Recupero il cliente più vecchio convalidato su trattative NUOVE
056500100915       //?e CHIUSE
056600100709         SETLL VISnrv TIVOF01L;
056700100709         READE VISnrv TIVOF01L;
056800100712         DOW  not %Eof(TIVOF01L);
056900110117           IF  VOFeso <> '*' and RCToff = *blanks and VOFctr <> 999;
057000100709             RCToff = 'S';
057100100709           ENDIF;
057200100915           //?Offerta convalidata
057300100915           IF  *in02 and VIStpv = 'N';
057400100915             IF  VOFeso = 'C' and VOFdco > 0 and VOFdco < wdata;
057500100915               RCTksc = VOFksc;
057600100915               wdata  = VOFdco;
057700100915             ENDIF;
057800100915           ENDIF;
057900100709           READE VISnrv TIVOF01L;
058000100709         ENDDO;
058100100805
058200100805       //?Controllo le attività
058300100805         exsr sr_Attivita;
058400100712
058500100712         write WFRCT000;
058600100709
058700100709       ENDSR;
058800100709
058900100709       //--------------------------------------------------------------
059000100709       //?INFO Trattativa.
059100100709       //--------------------------------------------------------------
059200100709       BEGSR sr_INFO;
059300100709
059400100709         $RcdOk = *on;
059500100715
059600100715       //?Se richiesto anche 1 sola selezione relativa alle INFO e
059700100715       //?la trattativa non ha le info non faccio nessun altro controllo
059800110114         IF  (ITA98vltti <> 0 or ITA98valte > 0 or
059900100715              ITA98deli <> *blanks or ITA98pkg > 0 or
060000110114              ITA98ausi <> 0 or
060100100715              ITA98conc1 <> *blanks or ITA98conc2 <> *blanks or
060200100715              ITA98conc3 <> *blanks or ITA98conc4 <> *blanks or
060300100715              ITA98conc5 <> *blanks) and VISinfot = *blanks;
060400100715           $RcdOk = *off;
060500100715           leavesr;
060600100715         ENDIF;
060700100709
060800100709       //?Dati traffico/spedizioni totali
060900100709         chain (VISnrv:'TRT') TIVII01L;
061000100709         IF  %found(TIVII01L);
061100110114           RCTfatt = VIIpft;
061200100713           RCTnspt = VIIsna;
061300100709         ENDIF;
061400100709       //?Controllo con selezioni
061500110114         IF  ITA98vltti <> 0;
061600101228           IF  RCTfatt < ITA98vltti;
061700101228             $RcdOk = *off;
061800101228             leavesr;
061900101228           ENDIF;
062000100709         ENDIF;
062100110114         IF  ITA98vlttf <> 0;
062200101228           IF  RCTfatt > ITA98vlttf;
062300101228             $RcdOk = *off;
062400101228             leavesr;
062500101228           ENDIF;
062600101228         ENDIF;
062700100709
062800100709       //?Dati traffico/spedizioni estere
062900100709         chain (VISnrv:'TRE') TIVII01L;
063000100709         IF  %found(TIVII01L);
063100100709           RCTfate = VIIpft;
063200100713           RCTnspe = VIIsna;
063300100709         ENDIF;
063400100709       //?Controllo con selezioni
063500100709         IF  ITA98valte > 0 and RCTfate <= ITA98valte;
063600100709           $RcdOk = *off;
063700100709           leavesr;
063800100709         ENDIF;
063900100709
064000100709       //?Dati delta
064100100709         chain (VISnrv:'DEL') TIVII01L;
064200100709         IF  %found(TIVII01L);
064300100715           wdel = %abs(VIIval);
064400100715           RCTdel = %editc(wdel:'4');
064500100715           IF  wdel = 0;
064600100715             RCTdel = '    0';
064700100715           ENDIF;
064800100709           IF  VIIval >= 0;
064900100709             RCTdels = '+';
065000100709           ELSE;
065100100709             RCTdels = '-';
065200100709           ENDIF;
065300100709         ENDIF;
065400100709       //?Controllo con selezioni
065500100715         IF  ITA98deli <> *blanks;
065600100714           wdeli = %dec(ITA98deli:3:0);
065700100715           IF  ITA98delsi = '-';
065800100715             wdeli = wdeli * -1;
065900100715           ENDIF;
066000100715           IF  VIIval < wdeli;
066100100714             $RcdOk = *off;
066200100714             leavesr;
066300100714           ENDIF;
066400100709         ENDIF;
066500100715         IF  ITA98delf <> *blanks;
066600100714           wdelf = %dec(ITA98delf:3:0);
066700100715           IF  ITA98delsf = '-';
066800100715             wdelf = wdelf * -1;
066900100715           ENDIF;
067000100715           IF  VIIval > wdelf;
067100100714             $RcdOk = *off;
067200100714             leavesr;
067300100714           ENDIF;
067400100709         ENDIF;
067500101228
067600101228       //?Dati aumento/sconto
067700101228         chain (VISnrv:'A/S') TIVII01L;
067800101228         IF  %found(TIVII01L);
067900110428           RCTausc = VIIvald;
068000101228         ENDIF;
068100101228       //?Controllo con selezioni
068200101228         IF  ITA98ausi <> 0;
068300110114           IF  not %found(TIVII01L);
068400110114             $RcdOk = *off;
068500110114             leavesr;
068600110114           ENDIF;
068700110505           wausi = ITA98ausi;
068800110505           IF  VIIvald < wausi;
068900101228             $RcdOk = *off;
069000101228             leavesr;
069100101228           ENDIF;
069200101228         ENDIF;
069300101228         IF  ITA98ausf <> 0;
069400110505           wausf = ITA98ausf;
069500110505           IF  VIIvald > wausf;
069600101228             $RcdOk = *off;
069700101228             leavesr;
069800101228           ENDIF;
069900101228         ENDIF;
070000100709
070100100709       //?Dati peso
070200100709         chain (VISnrv:'KMS') TIVII01L;
070300100709         IF  %found(TIVII01L);
070400100709           RCTpkg = VIIval;
070500100709         ENDIF;
070600100709       //?Controllo con selezioni
070700100709         IF  ITA98pkg > 0 and RCTpkg <= ITA98pkg;
070800100709           $RcdOk = *off;
070900100709           leavesr;
071000100709         ENDIF;
071100100709
071200100709       //?Concorrenti
071300100709         setll (VISnrv:'10') TIVII01L;
071400100709         reade (VISnrv:'10') TIVII01L;
071500100709         DOW not %eof(TIVII01L);
071600100709           IF  VIIfsn = 'S';
071700130905             clear TIBS02ds;
071800130905             //clear dITS;
071900130905             T02mod = 'C';
072000130905             T02cod = 'ITS';
072100130905             T02ke1 = VIItpf;
072200130905             T02ke2 = VIIspf;
072300130905             T02sif = KNSIF;
072400130905             //TNTBE_RicercaControllo (kpjba : tibs02ds);
072500130905             //IF  T02err = *blanks;
072600130905             //  dITS = T02uni;
072700130905             //ENDIF;
072800100716         //?Carico sempre tutti i concorrenti nei campi del file
072900100716         //?dopo controllo se scrivere o no il rcd della trattativa
073000100709         //?Imposto concorrente
073100100716             SELECT;
073200100716               WHEN  RCTconc1 = *blanks;
073300100716                 RCTconc1 = %subst(T02ke2:1:4);
073400100716               WHEN  RCTconc2 = *blanks;
073500100716                 RCTconc2 = %subst(T02ke2:1:4);
073600100716               WHEN  RCTconc3 = *blanks;
073700100716                 RCTconc3 = %subst(T02ke2:1:4);
073800100716               WHEN  RCTconc4 = *blanks;
073900100716                 RCTconc4 = %subst(T02ke2:1:4);
074000100716               WHEN  RCTconc5 = *blanks;
074100100716                 RCTconc5 = %subst(T02ke2:1:4);
074200100716               WHEN  RCTconc6 = *blanks;
074300100716                 RCTconc6 = %subst(T02ke2:1:4);
074400100716               WHEN  RCTconc7 = *blanks;
074500100716                 RCTconc7 = %subst(T02ke2:1:4);
074600100716               WHEN  RCTconc8 = *blanks;
074700100716                 RCTconc8 = %subst(T02ke2:1:4);
074800100716               WHEN  RCTconc9 = *blanks;
074900100716                 RCTconc9 = %subst(T02ke2:1:4);
075000100716               WHEN  RCTconc10= *blanks;
075100100716                 RCTconc10= %subst(T02ke2:1:4);
075200100716             ENDSL;
075300100712           ENDIF;
075400100712           reade (VISnrv:'10') TIVII01L;
075500100712         ENDDO;
075600100716       //?Se concorrente non previsto in schiera non carico nel file
075700100716         IF  ITA98conc <> *blanks and
075800100716            (RCTconc1 <> *blanks and %lookup(RCTconc1 : $Conc) > 0 or
075900100716             RCTconc2 <> *blanks and %lookup(RCTconc2 : $Conc) > 0 or
076000100716             RCTconc3 <> *blanks and %lookup(RCTconc3 : $Conc) > 0 or
076100100716             RCTconc4 <> *blanks and %lookup(RCTconc4 : $Conc) > 0 or
076200100716             RCTconc5 <> *blanks and %lookup(RCTconc5 : $Conc) > 0 or
076300100716             RCTconc6 <> *blanks and %lookup(RCTconc6 : $Conc) > 0 or
076400100716             RCTconc7 <> *blanks and %lookup(RCTconc7 : $Conc) > 0 or
076500100716             RCTconc8 <> *blanks and %lookup(RCTconc8 : $Conc) > 0 or
076600100716             RCTconc9 <> *blanks and %lookup(RCTconc9 : $Conc) > 0 or
076700100716             RCTconc10<> *blanks and %lookup(RCTconc10: $Conc) > 0);
076800100716           leavesr;
076900100716         ENDIF;
077000100716       //?Se non è stato richiesto nessun concorrente tutto ok
077100100716         IF  ITA98conc = *blanks;
077200100716           leavesr;
077300100716         ENDIF;
077400100716       //?Se arrivo qua i concorrenti memorizzati non sono stati
077500100716       //?richiesti
077600100716         $RcdOk = *off;
077700100716         leavesr;
077800100709
077900100709       ENDSR;
078000100709
078100100709       //--------------------------------------------------------------
078200100709       //?Imposto dati comuni sul file.
078300100709       //--------------------------------------------------------------
078400100709       BEGSR sr_DatiComuni;
078500100709
078600100709       //?Tipo record
078700100709         SELECT;
078800100709           WHEN *in01;
078900100709             RCTtrc = 'A';
079000100709           WHEN *in02;
079100100709             RCTtrc = 'C';
079200100709           WHEN *in03;
079300100709             RCTtrc = 'I';
079400100709         ENDSL;
079500100709
079600100709       //?Data/Utente lancio
079700100709         RCTdat = wOggi;
079800100709         RCTpru = knmus;
079900100709
080000100709       //?Data avvio
080100100709         RCTavdal = ITA98dtavi;
080200100709         RCTaval  = ITA98dtavf;
080300100709       //?Data chiusura
080400100709         RCTchdal = ITA98dtchi;
080500100709         RCTchal  = ITA98dtchf;
080600100709       //?Data in corso
080700100709         RCTical  = ITA98dtica;
080800100709
080900100709       //?Dati unificante
081000100709         RCTrgf = $AgeU(zz);
081100130905         chain  ($AgeU(zz))  AZCMM000;
081200130905         IF  %found( AZCMM01L );
081300130905           RCTdrg = CMMdes;
081400130905           RCTfun = CMMfun;
081500100709         ENDIF;
081600100709
081700100709       //?Filiale
081800100709         RCTfil = %dec(%subst(%editc($AgeU(zz):'X'):1:3):3:0);
081900100709         chain  RCTfil AZORG01L;
082000100709         IF  %found(AZORG01L);
082100100709           RCTfid = ORGdes;
082200100709           RCTdiv = ORGfl3;
082300100709           RCTare = ORGcar;
082400100709         ENDIF;
082500100709
082600100709       //?Descrizione area
082700100709         clear ds05;
082800100709         k_tblcod = '05';
082900100709         k_tblkey = %editc(RCTare:'X');
083000100709         chain  %kds(K03tabel) TABEL00F;
083100100709         IF  %found( TABEL00F );
083200100709           ds05 = TBLuni;
083300100709         ENDIF;
083400100709         RCTard = §05des;
083500100709
083600100709       //?Descrizione direzione commerciale
083700100709         clear ds17;
083800100709         k_tblcod = '17';
083900100709         k_tblkey = RCTdiv;
084000100709         chain  %kds(K03tabel) TABEL00F;
084100100709         IF  %found( TABEL00F );
084200100709           ds17 = TBLuni;
084300100709         ENDIF;
084400100709         RCTdid = §17des;
084500100709
084600100709       ENDSR;
084700100805
084800100805       //--------------------------------------------------------------
084900100805       //?Controllo le attività legate alla trattativa.
085000100805       //--------------------------------------------------------------
085100100805       BEGSR Sr_Attivita;
085200100805
085300100805         $EndAtt = *off;
085400100805
085500100805         exec sql
085600100805          declare ATT cursor for
085700100805          select * from tiatc00f
085800100805          where atcnrv = :visnrv
085900100805          order by atcdad, atchda;
086000100805
086100100805         exec sql
086200100805           open ATT;
086300100805
086400100805         IF sqlcode < 0;
086500100805           $EndAtt = *on;
086600100805         ENDIF;
086700100805
086800100805         DOW not $EndAtt;
086900100805           exec sql
087000100805             fetch next from ATT into :tiatcds;
087100100805           IF sqlcod = 100 or sqlcod < 0;
087200100805             $EndAtt = *on;
087300100805             leave;
087400100805           ENDIF;
087500100805
087600100805         //?Conto gli appuntamenti eseguiti
087700100915         //?CHIODO '91'
087800100805           IF  ATCcad = '91' and ATCdco > 0 and ATCest = 'S';
087900100805             RCTnrapp += 1;
088000100805           //?mi salvo la data più alta
088100100805             IF ATCdco > RCTduapp;
088200100805               RCTduapp = ATCdco;
088300100805             ENDIF;
088400100805           ENDIF;
088500100805
088600100805         //?Conto quante attività scadute non eseguite
088700101007           IF ATCdad < woggi and ATCdco = 0;
088800100805             RCTnrscad += 1;
088900100805           ENDIF;
089000100805
089100100805         //?Memorizzo i dati della prossima attività da eseguire
089200101019           IF ATCdad >= woggi and ATCdco = 0 and RCTtat = *blanks;
089300100805             RCTtat  = ATCtat;
089400100805             RCTcad  = ATCcad;
089500100805             RCTdad  = ATCdad;
089600100805             RCThda  = ATChda;
089700100805           ENDIF;
089800100805
089900100805         ENDDO;
090000100805
090100100805         exec sql
090200100805           close ATT;
090300100712
090400100526       ENDSR;
090500080206
090600080206       //--------------------------------------------------------------
090700080206       //?Reperimento Dati del job (Utente/Operativi).
090800080206       //--------------------------------------------------------------
090900080206       BEGSR DatiJob;
091000080206
091100080206         in(E) §AzUte;
091200080206         if NOT %error;
091300080206           in(E) §DatiUte;
091400080206         endif;
091500080206         if %error or RSut = *blanks;
091600080206           clear TIBS34ds;
091700080206           tibs34r(tibs34ds);
091800080206           in §AzUte;
091900080206           in §DatiUte;
092000080206         endif;
092100100325
092200100709       //?Richiamo pgm di controllo autorizzazioni utente così mi torna
092300100709       //?il codice autorizzazione relativo alle tariffe clienti
092400100709       //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
092500100325         reset TNTAA1DS;
092600100325         ITAA1caut = '§utegtc';
092700100325         kpjbu     = TNTAA1DS;
092800100325         tntaa1c (kpjba);
092900100325         TNTAA1DS = kpjbu;
093000100325
093100100709       //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
093200100325         IF  OTAA1cabi = 'AZ';
093300100325           OTAA1cabi = 'RA';
093400100325         ENDIF;
093500100325
093600100709       //?Carico filiali abilitate all'utente
093700100709       //?Non controllo se caricate o no le filiali
093800100709       //?tanto se non ho caricato nessuna filiale e la richiesta
093900100709       //?è per tutti i commerciali, non stampo niente
094000110119       //?Se richiesta un'area carico SOLO le filiali di quell'area
094100110119         IF  ITA98car > 0;
094200110119           clear TRUL31DS;
094300110119           clear TRUL31DS2;
094400110119           I31abi = 'AO';
094500110119           I31car = ITA98car;
094600110119           trul31r (kpjba : trul31ds : trul31ds2);
094700110119         ELSE;
094800110922          clear TRUL31DS;
094900110922          clear TRUL31DS2;
095000110922          if Dutpou = 046 ;
095100110922           I31abi = 'AZ'     ;
095200110922          else ;
095300110119           I31abi = OTAA1cabi;
095400110119           I31cdi = DUTdis;
095500110119           I31car = DUTare;
095600110119           I31cpo = DUTpou;
095700110922          Endif;
095800110119           trul31r (kpjba : trul31ds : trul31ds2);
095900110119         ENDIF;
096000080206
096100080206       ENDSR;
096200100325
096300100325       //--------------------------------------------------------------
096400100325       //?Carico sk agenti unificanti.
096500100325       //--------------------------------------------------------------
096600100326       BEGSR sr_CarAgeUni;
096700100325
096800100709       //?Carico gli agenti unificanti delle filiali gestite dall'utente
096900110119       //?o dell'area se richiesti
097000100326         clear xx;
097100100326         clear $AGEU;
097200100326         $EndAge = *off;
097300100325
097400100709       //?Imposto la stringa per SQL
097500130905         wSQL = 'select CMMcod +
097600130905                   from AZCMM00F +
097700130905                  where substr(digits(CMMuni), 1, 3) in (';
097800100325
097900100325         yy = 0;
098000100325         xx = 1;
098100100325         FOR xx by 1 to %elem(pog);
098200100325           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
098300100325             IF yy > 0;
098400100325               wSQL += ', ';
098500100325             ELSE;
098600100325               yy = 1;
098700100325             ENDIF;
098800100325             wSQL += '''';
098900100325             wSQL += pog(xx);
099000100325             wSQL += '''';
099100100325           ENDIF;
099200100325         ENDFOR;
099300100325
099400130905         wSQL += ') and CMMcod = CMMuni +
099500130905                    and CMMpar =  '' ''';
099600100709
099700100709       //?Controllo funzione commerciale da elaborare
099800100709         IF  ITA98elcmm = 'I';
099900130905           wSQL += ' and CMMfun in (''COMIN'', +
100000130905                                    ''ASC  '', +
100100130905                                    ''SA   '')';
100200100709         ENDIF;
100300100709         IF  ITA98elcmm = 'E';
100400130905           wSQL += ' and CMMfun NOT in (''COMIN'', +
100500130905                                        ''ASC  '', +
100600130905                                        ''SA   '')';
100700100709         ENDIF;
100800100325
100900100709       //?Ordino per codice commerciale
101000130905         wSQL += ' order by CMMcod +
101100100325                   for fetch only';
101200100325
101300100709       //?Dichiarazione cursore
101400100325         exec sql
101500100325           prepare A1   from :wSQL;
101600100325         exec sql
101700100325           declare AGEU cursor for A1;
101800100325
101900100709       //?Apertura del cursore
102000100325         exec sql
102100100325           open AGEU;
102200100325
102300100325         IF sqlcode < 0;
102400100325           $EndAge = *on;
102500100325         ENDIF;
102600100325
102700100709       //?Carico la schiera degli agenti unificanti
102800100325         yy = 0;
102900100325         DOW not $EndAge;
103000100325           exec sql
103100130905             fetch next from AGEU into :CMMcod;
103200100325           IF sqlcod = 100 or sqlcod < 0;
103300100325             $EndAge = *on;
103400100325             leave;
103500100325           ENDIF;
103600100325
103700100325         yy += 1;
103800130905         $AGEU(yy) = CMMcod;
103900100325
104000100325         ENDDO;
104100100325
104200100325         exec sql
104300100325           close AGEU;
104400100325
104500100325       ENDSR;
104600100326
104700080206       //--------------------------------------------------------------
104800080206       //?Operazioni finali.
104900080206       //--------------------------------------------------------------
105000080206       BEGSR RoutEnd;
105100100714
105200100714       //?Reimposto la kpjbu con la ds dei dati passati
105300100714         kpjbu = TNTA98ds;
105400090521
105500080206         *inLR = *on;
105600080206         return;
105700080206
105800080206       ENDSR;
105900080206
106000080206      /end-free
