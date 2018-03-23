000100080206      //--------------------------------------------------------------
000200100708      //?TNTA98R1 - Elenco trattative - crea file work
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600080206
000700080206      //---------------------------------------------------------------
000800080206      //?Dichiarazione file.
000900080206      //---------------------------------------------------------------
001000100527     fAZORG01L  if   e           k disk
001100100325     fTABEL00F  if   e           k disk
001200100308     fTIVII01L  if   e           k disk
001300100308     fTIVOF01L  if   e           k disk
001400100308     fTNCPO01L  if   e           k disk
001500100527     fWFRCT00F  o    e             disk
001600100310
001700080206      //---------------------------------------------------------------
001800090406      //?Definizione costanti.
001900080206      //---------------------------------------------------------------
002000080206
002100080206      //---------------------------------------------------------------
002200080206      //?Definizione schiere.
002300080206      //---------------------------------------------------------------
002400100325
002500100325      // - Agenti
002600100325     d $Age            s              7  0 dim(3500)
002700100325
002800100325      // - Agenti Unificanti
002900100325     d $AgeU           s              7  0 dim(3500)
003000080206
003100080206      //---------------------------------------------------------------
003200080206      //?Definizione aree dati.
003300080206      //---------------------------------------------------------------
003400080206
003500080206      // - Dati utente
003600080206     d §AzUte        e ds                  extname(AZUTE00F)
003700080206     d                                     dtaara
003800080206     d §DatiUte      e ds                  extname(dDatiUte)
003900080206     d                                     dtaara
004000080206
004100080206      //---------------------------------------------------------------
004200080206      //?Definizione strutture dati.
004300080206      //---------------------------------------------------------------
004400080206
004500080206      // - Parametri ricevuti
004600080206     d KPJBA         e ds
004700100524     d TNTA98DS      e ds
004800110119     d ITA98conc              90    109
004900110119     d $Conc                  90    109    dim(5)
005000110119     d ITA98tpt              124    129
005100110119     d $Tpt                  124    129    dim(6)
005200110119     d ITA98tic              130    134
005300110119     d $Tic                  130    134    dim(5)
005400080206
005500100308      // - Ricerca/Controllo tabelle
005600100308     d TIBS02ds      e ds                  inz
005700100308
005800080206      // - Reperimento dati utente
005900100305     d TIBS34DS      e ds
006000100308
006100100308       // - Reperimento dati anagrafici
006200100308     d TIBS69ds      e ds
006300100308     d DS_cnaco      e ds                  inz extname(CNACO00F)
006400100308     d DS_cnind      e ds                  inz extname(CNIND00F)
006500100308     d DS_cnclp      e ds                  inz extname(CNCLP00F)
006600100308     d DS_fncls      e ds                  inz extname(FNCLS00F)
006700100326
006800100326      // - controllo abilitazioni
006900100326     d TNTAA1DS      e ds                  inz
007000100325
007100100325      // - Carico Filiali abilitate all'utente
007200100325     d TRUL31DS      e ds
007300100325     d POG                    10    759    DIM(250)
007400110119      // - Carico Aree abilitate all'utente
007500110119     d TRUL31DS2     e ds
007600110119     d CAR                    22    171    DIM(50)
007700100325
007800100325      // - Tabella CCO = Causale contatto
007900100325     d dCCO          e ds                  inz
008000100308
008100100324      // - Tabella ITS = Sottotipi INFO trattativa
008200100324     d dITS          e ds                  inz
008300100325
008400100325      // - Tabella 01 = Codici Commerciali
008500100325     d ds01          e ds                  inz
008600100526
008700100527      // - Tabella 05 = Codici Area
008800100527     d ds05          e ds                  inz
008900100526
009000100526      // - Tabella 17 = Direzioni commerciali
009100100526     d ds17          e ds                  inz
009200101015
009300101015      // - VISFLO file TIVIS00F
009400101015     d dVIS01        e ds
009500090807
009600090807      // File attività
009700100805     d TIATCDS       e ds                  extname(TIATC00F)
009800100305
009900100305      // File trattativa
010000100324     d TIVISDS       e ds                  extname(TIVIS00F)
010100090629
010200080206      //---------------------------------------------------------------
010300080206      //?Definizione variabili globali.
010400080206      //---------------------------------------------------------------
010500080206
010600080206      // - Flags booleani
010700100305     d $End            s               n   inz(*off)
010800100325     d $EndAge         s               n   inz(*off)
010900100326     d $EndAtt         s               n   inz(*off)
011000100305     d $RcdOk          s               n   inz(*off)
011100100325
011200100325      // - Indici di schiera
011300100326     d xx              s              4  0 inz
011400100326     d yy              s              4  0 inz
011500100326     d zz              s              4  0 inz
011600080206
011700090806       // - Stringa SQL da eseguire
011800090806     d wSQL            s           2048    Varying        inz
011900080206
012000090401      // - Campi di comodo data
012100100305     d wDate_EUR       s               d   datfmt(*eur)
012200100305     d wDate_ISO       s               d   datfmt(*iso)
012300100305
012400100305      // - Campi di comodo
012500110505     d wausi           s              5  2 inz
012600110505     d wausf           s              5  2 inz
012700100915     d wdata           s              8  0 inz
012800100715     d wdel            s              5  0 inz
012900100715     d wdeli           s              5  0 inz
013000100715     d wdelf           s              5  0 inz
013100100329     d wOggi           s              8  0 inz
013200100325
013300090508      //---------------------------------------------------------------
013400090807      //?Definizione procedure esterne.
013500090508      //---------------------------------------------------------------
013600100120
013700110119      // - Filiali e aree abilitate
013800110119     d trul31r         pr                  extpgm('TRUL31R')
013900110119     d  kpjba                              likeds(KPJBA)
014000110119     d  trul31ds                           likeds(trul31ds)
014100110119     d  trul31ds2                          likeds(trul31ds2)
014200100120
014300100120      //---------------------------------------------------------------
014400100120      //?prototipi
014500100120      //---------------------------------------------------------------
014600090807
014700080626      /copy gaitrasrc/srcprotopr,tibs02r
014800080626      /copy gaitrasrc/srcprotopr,tibs34r
014900100308      /copy gaitrasrc/srcprotopr,tibs69r
015000100326      /copy gaitrasrc/srcprotopr,tntaa1c
015100090807
015200080206      //---------------------------------------------------------------
015300080206      //?Definizione key-list.
015400080206      //---------------------------------------------------------------
015500100325
015600100325       // - File TABEL00F
015700100325     d k03tabel      e ds                  extname(TABEL00F:*key)
015800100325     d                                     prefix(k_)
015900100325     d                                     inz
016000080206
016100080206      //---------------------------------------------------------------
016200080206      //?Riepilogo indicatori.
016300080206      //---------------------------------------------------------------
016400100709
016500100709      // 01    : Condizione stampa trattative AVVIATE
016600100709      // 02    : Condizione stampa trattative CHIUSE
016700100709      // 03    : Condizione stampa trattative IN CORSO
016800090807
016900080206      //---------------------------------------------------------------
017000080206
017100080206      //---------------------------------------------------------------
017200080206      //?M A I N - L I N E
017300080206      //---------------------------------------------------------------
017400080206
017500080206     c     *Entry        plist
017600080206     c                   parm                    KPJBA
017700080206
017800080206      /free
017900080206
018000090807       //?Operazioni iniziali
018100080206       exsr RoutInz;
018200100524
018300100709       //?Ciclo per sk unificanti
018400100524       zz = 1;
018500100524       FOR zz by 1 to %elem($AgeU);
018600100524         IF  $AgeU(zz) <> *zeros;
018700100524
018800100524         //?Carico sk agenti dell'unificante
018900100524           exsr sr_CarAge;
019000100708
019100100709         //?Elaboro AVVIATE
019200100709           IF  ITA98dtavi <> 0 and ITA98dtavf <> 0;
019300100709             exsr sr_Avviate;
019400100709           ENDIF;
019500100709
019600100709         //?Elaboro CHIUSE
019700100712           IF  ITA98dtchi <> 0 and ITA98dtchf <> 0;
019800100712             exsr sr_Chiuse;
019900100712           ENDIF;
020000100709
020100100709         //?Elaboro IN CORSO
020200100712           IF  ITA98dtica <> 0;
020300100712             exsr sr_InCorso;
020400100712           ENDIF;
020500100527
020600100524         ENDIF;
020700100524       ENDFOR;
020800080206
020900090807       //?Operazioni finali
021000080206       exsr RoutEnd;
021100080206
021200080206       //--------------------------------------------------------------
021300080206       //?Operazioni iniziali.
021400080206       //--------------------------------------------------------------
021500080206       BEGSR RoutInz;
021600100329
021700100709       //?Imposto la ds con i dati della KPJBU
021800100524         TNTA98ds = kpjbu;
021900090807
022000100709       //?Reperimento dati job
022100090807         exsr DatiJob;
022200090807
022300090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022400100527
022500100709       //?Imposto dati fissi
022600100527         k_tblkut = 1;
022700100305
022800100712       //?Imposto data elaborazione
022900100712         wOggi = %dec(%date());
023000100325
023100100325         clear $AgeU;
023200100325
023300100709       //?Carico in sk l'unificante richiesto
023400100524         IF  ITA98cmm > 0;
023500100524           $AgeU(01) = ITA98cmm;
023600100325         ENDIF;
023700100325
023800100709       //?Carico gli unificanti delle filiali gestite dall'utente
023900110119       //?o dell'area se richiesta
024000100524         IF  ITA98cmm = 0;
024100100325           exsr sr_CarAgeUni;
024200100325           sorta $AgeU;
024300100325         ENDIF;
024400090714
024500080206       ENDSR;
024600100524
024700100326       //--------------------------------------------------------------
024800100326       //?Carico Sk Agenti.
024900100326       //--------------------------------------------------------------
025000100326       BEGSR sr_CarAge;
025100100326
025200100326         clear xx;
025300100326         clear $Age;
025400100326         $EndAge = *off;
025500100326
025600100326         exec sql
025700100326          declare AGE cursor for
025800100326          select tblkey, tbluni
025900100326          from tabel00f where tblkut = '1' and tblcod = '01' and
026000100326          tblflg = ' ' order by tblkut, tblcod, tblkey;
026100100326
026200100326         exec sql
026300100326           open AGE;
026400100326           IF sqlcode < 0;
026500100326             $EndAge = *on;
026600100326           ENDIF;
026700100326
026800100326         DOW not $EndAge;
026900100326           exec sql
027000100326             fetch next from AGE into :tblkey, :tbluni;
027100100326             IF sqlcod = 100 or sqlcod < 0;
027200100326               $EndAge = *on;
027300100326               leave;
027400100326             ENDIF;
027500100326             ds01 = Tbluni;
027600100326             IF  §01rgf = $AgeU(zz);
027700100326               xx += 1;
027800100326               $Age(xx) = %dec(%subst(tblkey:1:7):7:0);
027900100326             ENDIF;
028000100326         ENDDO;
028100100326
028200100326         exec sql close AGE;
028300100326
028400100709       //?Se non ho cricato niente memorizzo almeno il codice richiesto
028500100326         IF $Age(01) = 0;
028600100326           $Age(01) = $AgeU(zz);
028700100326         ENDIF;
028800100326
028900100326       ENDSR;
029000100709
029100100709       //--------------------------------------------------------------
029200100709       //?Elaboro le trattative avviate.
029300100709       //--------------------------------------------------------------
029400100709       BEGSR sr_Avviate;
029500100709
029600100709       //?Imposto flag per identificare che sto elaborando le
029700100709       //?trattative AVVIATE
029800100709         *in01 = *on;
029900100709         *in02 = *off;
030000100709         *in03 = *off;
030100100709
030200100709       //?Elabora le trattative
030300100709         exsr sr_Elabora;
030400100709
030500100709       ENDSR;
030600100525
030700100709       //--------------------------------------------------------------
030800100709       //?Elaboro le trattative chiuse.
030900100709       //--------------------------------------------------------------
031000100709       BEGSR sr_Chiuse;
031100100709
031200100709       //?Imposto flag per identificare che sto elaborando le
031300100709       //?trattative CHIUSE
031400100709         *in01 = *off;
031500100709         *in02 = *on;
031600100709         *in03 = *off;
031700100709
031800100709       //?Elabora le trattative
031900100709         exsr sr_Elabora;
032000100709
032100100709       ENDSR;
032200100709
032300100709       //--------------------------------------------------------------
032400100709       //?Elaboro le trattative in corso.
032500100709       //--------------------------------------------------------------
032600100709       BEGSR sr_InCorso;
032700100709
032800100709       //?Imposto flag per identificare che sto elaborando le
032900100709       //?trattative IN CORSO
033000100709         *in01 = *off;
033100100709         *in02 = *off;
033200100709         *in03 = *on;
033300100709
033400100709       //?Elabora le trattative
033500100709         exsr sr_Elabora;
033600100709
033700100709       ENDSR;
033800100709
033900100525       //--------------------------------------------------------------
034000100525       //?Elaboro le trattative.
034100100525       //--------------------------------------------------------------
034200100525       BEGSR sr_Elabora;
034300100526
034400100526         $End = *off;
034500100526
034600100709       //?Imposto la stringa per SQL
034700100708         exsr  sr_PrepSQL;
034800100708
034900100709       //?Dichiarazione cursore
035000100708         exec sql
035100100708           prepare S1   from :wSQL;
035200100708         exec sql
035300100708           declare VIS  cursor for S1;
035400100526
035500100709       //?Apertura del cursore
035600100526         exec sql
035700100526           open VIS;
035800100526
035900100526         IF sqlcode < 0;
036000100526           $End = *on;
036100100526         ENDIF;
036200100526
036300100526         DOW not $End;
036400100526           exec sql
036500100526             fetch next from VIS into :tivisds;
036600100526           IF sqlcod = 100 or sqlcod < 0;
036700100526             $End = *on;
036800100526             leave;
036900100526           ENDIF;
037000100709
037100100709       //?Carico i dati nel file di work
037200100709           exsr sr_CarDati;
037300100526
037400100526         ENDDO;
037500100526
037600100526         exec sql
037700100526           close VIS;
037800100526
037900100526       ENDSR;
038000100525
038100100525       //--------------------------------------------------------------
038200100525       //?Preparazione stringa SQL.
038300100525       //--------------------------------------------------------------
038400100525       BEGSR sr_PrepSQL;
038500100525
038600100709       //?Seleziono trattative NON fittizie del commerciale che sto elaborando
038700100525         wSQL = 'select * from TIVIS00F +
038800100709                 where visffz = '' '' and viscmm in(';
038900100525         yy = 0;
039000100525         xx = 1;
039100100525         FOR xx by 1 to %elem($Age);
039200100525           IF $Age(xx) <> *zeros;
039300100525             IF yy > 0;
039400100525               wSQL += ', ';
039500100525             ELSE;
039600100525               yy = 1;
039700100525             ENDIF;
039800100525             wSQL += %editc($Age(xx):'X');
039900100525           ENDIF;
040000100525         ENDFOR;
040100100709
040200100709         //?Trattative avviate
040300100709         IF  *in01;
040400100709           wSQL += ') and visdat between ' + %editc(ITA98dtavi:'X') +
040500100709                   ' and ' + %editc(ITA98dtavf:'X');
040600100709         ENDIF;
040700100709
040800100709         //?Trattative chiuse
040900100709         IF  *in02;
041000100709           wSQL += ') and visdch between ' + %editc(ITA98dtchi:'X') +
041100100709                   ' and ' + %editc(ITA98dtchf:'X');
041200100709         ENDIF;
041300100709
041400100709         //?Trattative in corso
041500100709         IF  *in03;
041600100709           wSQL += ') and visdat <= ' + %editc(ITA98dtica:'X') +
041700100714                   ' and (visdch = 0 or visdch > ' +
041800100714                     %editc(ITA98dtica:'X') + ')';
041900100709         ENDIF;
042000100709
042100100709       //?Seleziono i tipi trattativa richiesti
042200100716         IF  ITA98tpt <> *blanks;
042300100709           wSQL += ' and vistpv in(';
042400100709           yy = 0;
042500100709           xx = 1;
042600100709           FOR xx by 1 to %elem($Tpt);
042700100709             IF $Tpt(xx) <> *blanks;
042800100709               IF yy > 0;
042900100709                 wSQL += ', ';
043000100709               ELSE;
043100100709                 yy = 1;
043200100709               ENDIF;
043300100716               wSQL += '''' + $Tpt(xx) + '''';
043400100709             ENDIF;
043500100709           ENDFOR;
043600100716           wSQL += ')';
043700100709         ENDIF;
043800100525
043900100525       ENDSR;
044000100709
044100100709       //--------------------------------------------------------------
044200100709       //?Carico i dati legati alla trattativa.
044300100709       //--------------------------------------------------------------
044400100709       BEGSR sr_CarDati;
044500100709
044600100709         clear WFRCT000;
044700100915         wdata = *all'9';
044800101015         dVIS01 = VISflo;
044900100709
045000100709       //?Verifico il tipo cliente
045100100709       //?Vale per tutte e tre le elaborazioni
045200100709         SELECT;
045300100709         //?Da anagrafica cliente
045400100709           WHEN  VISksc > 0;
045500100709             clear tibs69ds;
045600100709             I69kcp = VISksc;
045700100709             tibs69r (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
045800100709             IF  O69err = *blanks;
045900100709               RCTtic = CLPclv;
046000100709             ENDIF;
046100100709         //?Da anagrafica potenziale
046200100709           WHEN  VIScpo <> 0;
046300100709             chain VIScpo TNCPO01L;
046400100709             IF  %found(TNCPO01L);
046500100709               RCTtic = CPOftr;
046600100709             ENDIF;
046700100709         ENDSL;
046800100715       //?Se tipo cliente non previsto in schiera NON scrivo file
046900100716         IF  ITA98tic <> *blanks and
047000100716             %lookup(RCTtic : $Tic) = 0;
047100100716           leavesr;
047200100709         ENDIF;
047300100716         IF  ITA98tic <> *blanks and RCTtic = *blanks;
047400100716           leavesr;
047500100716         ENDIF;
047600100709
047700100709       //?Verifico INFO trattativa
047800100709       //?Vale per tutte e tre le elaborazioni
047900100709         exsr sr_INFO;
048000100709         IF  not $RcdOk;
048100100709           leavesr;
048200100709         ENDIF;
048300100709
048400100709       //?Verifico se CALDA (offerta accettata)
048500100709       //?Vale solo per elaborazione trattative IN CORSO
048600110113       //?devo considerare anche il CALDO senza offerte (999)
048700100709         IF  *in03;
048800100709           setll VISnrv TIVOF01L;
048900100709           reade VISnrv TIVOF01L;
049000100709           DOW not %eof(TIVOF01L);
049100100709           //?Offerta accettata
049200100709             IF  VOFeso = 'A';
049300100709               RCTcaldo = 'S';
049400100709               RCTdpaff = VOFdpa;
049500100709               leave;
049600100709             ENDIF;
049700110113             IF  VOFctr = 999 and VOFdpa > 0;
049800110113               RCTcaldo = 'S';
049900110113               RCTdpaff = VOFdpa;
050000110113               leave;
050100110113             ENDIF;
050200100709             reade VISnrv TIVOF01L;
050300100709           ENDDO;
050400100713         //?Tengo se trattativa calda
050500100709           IF  ITA98calde = 'S' and RCTcaldo = *blanks;
050600100709             leavesr;
050700100709           ENDIF;
050800100709         //?Tengo se calda e data presunto affidamento entro data richiesta
050900100709           IF  ITA98dtaff > 0 and RCTdpaff > ITA98dtaff;
051000100709             leavesr;
051100100709           ENDIF;
051200100709         ENDIF;
051300100709
051400100709       //?Verifico esito trattativa
051500100709       //?Vale solo per elaborazione trattative CHIUSE
051600100709         IF  *in02;
051700100709         //?Positivo/Negativo
051800101015           IF  VISesi = 'P ' or §VISesito = 'P';
051900100709             RCTepn = 'P';
052000100709           ELSE;
052100100709             RCTepn = 'N';
052200100709           ENDIF;
052300100709         //?Tengo solo se esito richiesto
052400100709           IF  ITA98esi = 'N' and RCTepn = 'P';
052500100709             leavesr;
052600100709           ENDIF;
052700100709           IF  ITA98esi = 'P' and RCTepn = 'N';
052800100709             leavesr;
052900100709           ENDIF;
053000100709         //?Causale esito
053100100709           RCTesi = VISesi;
053200100709         //?Decodifico esito
053300100709           clear TIBS02ds;
053400100709           clear dCCO;
053500100709           T02mod = 'C';
053600100709           T02cod = 'CCO';
053700100709           T02ke1 = VISesi;
053800100709           T02sif = KNSIF;
053900100709           TNTBE_RicercaControllo (kpjba : tibs02ds);
054000100709           IF  T02err = *blanks;
054100100709             dCCO = T02uni;
054200100709           ENDIF;
054300100709           RCTdesi = §CCOdes;
054400100709         ENDIF;
054500100709
054600100709       //?Imposto dati comuni
054700100709         exsr sr_DatiComuni;
054800100709
054900100709       //?Imposto dati relativi alla trattativa
055000100709         RCTnrv = VISnrv;
055100100709         RCTrag = VISrag;
055200100709         RCTksc = VISksc;
055300100709         RCTttr = VIStpv;
055400100709         RCTdavt = VISdat;
055500100709         RCTcmmi = VIScmmi;
055600100709       //?Decodifico commerciale interno
055700100709         clear ds01;
055800100709         k_tblcod = '01';
055900100709         k_tblkey = %editc(VIScmmi:'X');
056000100709         chain  %kds(K03tabel) TABEL00F;
056100100709         IF  %found( TABEL00F );
056200100709           ds01 = TBLuni;
056300100709         ENDIF;
056400100709         RCTdcmmi = §01age;
056500100709         RCTfgs  = %dec(%subst(%editc(VIScmm:'X'):1:3):3:0);
056600100709         RCTdch = VISdch;
056700100709
056800100709       //?Imposto se c'è offerta
056900100915       //?Recupero il cliente più vecchio convalidato su trattative NUOVE
057000100915       //?e CHIUSE
057100100709         SETLL VISnrv TIVOF01L;
057200100709         READE VISnrv TIVOF01L;
057300100712         DOW  not %Eof(TIVOF01L);
057400110117           IF  VOFeso <> '*' and RCToff = *blanks and VOFctr <> 999;
057500100709             RCToff = 'S';
057600100709           ENDIF;
057700100915           //?Offerta convalidata
057800100915           IF  *in02 and VIStpv = 'N';
057900100915             IF  VOFeso = 'C' and VOFdco > 0 and VOFdco < wdata;
058000100915               RCTksc = VOFksc;
058100100915               wdata  = VOFdco;
058200100915             ENDIF;
058300100915           ENDIF;
058400100709           READE VISnrv TIVOF01L;
058500100709         ENDDO;
058600100805
058700100805       //?Controllo le attività
058800100805         exsr sr_Attivita;
058900100712
059000100712         write WFRCT000;
059100100709
059200100709       ENDSR;
059300100709
059400100709       //--------------------------------------------------------------
059500100709       //?INFO Trattativa.
059600100709       //--------------------------------------------------------------
059700100709       BEGSR sr_INFO;
059800100709
059900100709         $RcdOk = *on;
060000100715
060100100715       //?Se richiesto anche 1 sola selezione relativa alle INFO e
060200100715       //?la trattativa non ha le info non faccio nessun altro controllo
060300110114         IF  (ITA98vltti <> 0 or ITA98valte > 0 or
060400100715              ITA98deli <> *blanks or ITA98pkg > 0 or
060500110114              ITA98ausi <> 0 or
060600100715              ITA98conc1 <> *blanks or ITA98conc2 <> *blanks or
060700100715              ITA98conc3 <> *blanks or ITA98conc4 <> *blanks or
060800100715              ITA98conc5 <> *blanks) and VISinfot = *blanks;
060900100715           $RcdOk = *off;
061000100715           leavesr;
061100100715         ENDIF;
061200100709
061300100709       //?Dati traffico/spedizioni totali
061400100709         chain (VISnrv:'TRT') TIVII01L;
061500100709         IF  %found(TIVII01L);
061600110114           RCTfatt = VIIpft;
061700100713           RCTnspt = VIIsna;
061800100709         ENDIF;
061900100709       //?Controllo con selezioni
062000110114         IF  ITA98vltti <> 0;
062100101228           IF  RCTfatt < ITA98vltti;
062200101228             $RcdOk = *off;
062300101228             leavesr;
062400101228           ENDIF;
062500100709         ENDIF;
062600110114         IF  ITA98vlttf <> 0;
062700101228           IF  RCTfatt > ITA98vlttf;
062800101228             $RcdOk = *off;
062900101228             leavesr;
063000101228           ENDIF;
063100101228         ENDIF;
063200100709
063300100709       //?Dati traffico/spedizioni estere
063400100709         chain (VISnrv:'TRE') TIVII01L;
063500100709         IF  %found(TIVII01L);
063600100709           RCTfate = VIIpft;
063700100713           RCTnspe = VIIsna;
063800100709         ENDIF;
063900100709       //?Controllo con selezioni
064000100709         IF  ITA98valte > 0 and RCTfate <= ITA98valte;
064100100709           $RcdOk = *off;
064200100709           leavesr;
064300100709         ENDIF;
064400100709
064500100709       //?Dati delta
064600100709         chain (VISnrv:'DEL') TIVII01L;
064700100709         IF  %found(TIVII01L);
064800100715           wdel = %abs(VIIval);
064900100715           RCTdel = %editc(wdel:'4');
065000100715           IF  wdel = 0;
065100100715             RCTdel = '    0';
065200100715           ENDIF;
065300100709           IF  VIIval >= 0;
065400100709             RCTdels = '+';
065500100709           ELSE;
065600100709             RCTdels = '-';
065700100709           ENDIF;
065800100709         ENDIF;
065900100709       //?Controllo con selezioni
066000100715         IF  ITA98deli <> *blanks;
066100100714           wdeli = %dec(ITA98deli:3:0);
066200100715           IF  ITA98delsi = '-';
066300100715             wdeli = wdeli * -1;
066400100715           ENDIF;
066500100715           IF  VIIval < wdeli;
066600100714             $RcdOk = *off;
066700100714             leavesr;
066800100714           ENDIF;
066900100709         ENDIF;
067000100715         IF  ITA98delf <> *blanks;
067100100714           wdelf = %dec(ITA98delf:3:0);
067200100715           IF  ITA98delsf = '-';
067300100715             wdelf = wdelf * -1;
067400100715           ENDIF;
067500100715           IF  VIIval > wdelf;
067600100714             $RcdOk = *off;
067700100714             leavesr;
067800100714           ENDIF;
067900100709         ENDIF;
068000101228
068100101228       //?Dati aumento/sconto
068200101228         chain (VISnrv:'A/S') TIVII01L;
068300101228         IF  %found(TIVII01L);
068400110428           RCTausc = VIIvald;
068500101228         ENDIF;
068600101228       //?Controllo con selezioni
068700101228         IF  ITA98ausi <> 0;
068800110114           IF  not %found(TIVII01L);
068900110114             $RcdOk = *off;
069000110114             leavesr;
069100110114           ENDIF;
069200110505           wausi = ITA98ausi;
069300110505           IF  VIIvald < wausi;
069400101228             $RcdOk = *off;
069500101228             leavesr;
069600101228           ENDIF;
069700101228         ENDIF;
069800101228         IF  ITA98ausf <> 0;
069900110505           wausf = ITA98ausf;
070000110505           IF  VIIvald > wausf;
070100101228             $RcdOk = *off;
070200101228             leavesr;
070300101228           ENDIF;
070400101228         ENDIF;
070500100709
070600100709       //?Dati peso
070700100709         chain (VISnrv:'KMS') TIVII01L;
070800100709         IF  %found(TIVII01L);
070900100709           RCTpkg = VIIval;
071000100709         ENDIF;
071100100709       //?Controllo con selezioni
071200100709         IF  ITA98pkg > 0 and RCTpkg <= ITA98pkg;
071300100709           $RcdOk = *off;
071400100709           leavesr;
071500100709         ENDIF;
071600100709
071700100709       //?Concorrenti
071800100709         setll (VISnrv:'10') TIVII01L;
071900100709         reade (VISnrv:'10') TIVII01L;
072000100709         DOW not %eof(TIVII01L);
072100100709           IF  VIIfsn = 'S';
072200100709             clear TIBS02ds;
072300100709             clear dITS;
072400100709             T02mod = 'C';
072500100709             T02cod = 'ITS';
072600100709             T02ke1 = VIItpf;
072700100709             T02ke2 = VIIspf;
072800100709             T02sif = KNSIF;
072900100709             TNTBE_RicercaControllo (kpjba : tibs02ds);
073000100709             IF  T02err = *blanks;
073100100709               dITS = T02uni;
073200100709             ENDIF;
073300100716         //?Carico sempre tutti i concorrenti nei campi del file
073400100716         //?dopo controllo se scrivere o no il rcd della trattativa
073500100709         //?Imposto concorrente
073600100716             SELECT;
073700100716               WHEN  RCTconc1 = *blanks;
073800100716                 RCTconc1 = %subst(T02ke2:1:4);
073900100716               WHEN  RCTconc2 = *blanks;
074000100716                 RCTconc2 = %subst(T02ke2:1:4);
074100100716               WHEN  RCTconc3 = *blanks;
074200100716                 RCTconc3 = %subst(T02ke2:1:4);
074300100716               WHEN  RCTconc4 = *blanks;
074400100716                 RCTconc4 = %subst(T02ke2:1:4);
074500100716               WHEN  RCTconc5 = *blanks;
074600100716                 RCTconc5 = %subst(T02ke2:1:4);
074700100716               WHEN  RCTconc6 = *blanks;
074800100716                 RCTconc6 = %subst(T02ke2:1:4);
074900100716               WHEN  RCTconc7 = *blanks;
075000100716                 RCTconc7 = %subst(T02ke2:1:4);
075100100716               WHEN  RCTconc8 = *blanks;
075200100716                 RCTconc8 = %subst(T02ke2:1:4);
075300100716               WHEN  RCTconc9 = *blanks;
075400100716                 RCTconc9 = %subst(T02ke2:1:4);
075500100716               WHEN  RCTconc10= *blanks;
075600100716                 RCTconc10= %subst(T02ke2:1:4);
075700100716             ENDSL;
075800100712           ENDIF;
075900100712           reade (VISnrv:'10') TIVII01L;
076000100712         ENDDO;
076100100716       //?Se concorrente non previsto in schiera non carico nel file
076200100716         IF  ITA98conc <> *blanks and
076300100716            (RCTconc1 <> *blanks and %lookup(RCTconc1 : $Conc) > 0 or
076400100716             RCTconc2 <> *blanks and %lookup(RCTconc2 : $Conc) > 0 or
076500100716             RCTconc3 <> *blanks and %lookup(RCTconc3 : $Conc) > 0 or
076600100716             RCTconc4 <> *blanks and %lookup(RCTconc4 : $Conc) > 0 or
076700100716             RCTconc5 <> *blanks and %lookup(RCTconc5 : $Conc) > 0 or
076800100716             RCTconc6 <> *blanks and %lookup(RCTconc6 : $Conc) > 0 or
076900100716             RCTconc7 <> *blanks and %lookup(RCTconc7 : $Conc) > 0 or
077000100716             RCTconc8 <> *blanks and %lookup(RCTconc8 : $Conc) > 0 or
077100100716             RCTconc9 <> *blanks and %lookup(RCTconc9 : $Conc) > 0 or
077200100716             RCTconc10<> *blanks and %lookup(RCTconc10: $Conc) > 0);
077300100716           leavesr;
077400100716         ENDIF;
077500100716       //?Se non è stato richiesto nessun concorrente tutto ok
077600100716         IF  ITA98conc = *blanks;
077700100716           leavesr;
077800100716         ENDIF;
077900100716       //?Se arrivo qua i concorrenti memorizzati non sono stati
078000100716       //?richiesti
078100100716         $RcdOk = *off;
078200100716         leavesr;
078300100709
078400100709       ENDSR;
078500100709
078600100709       //--------------------------------------------------------------
078700100709       //?Imposto dati comuni sul file.
078800100709       //--------------------------------------------------------------
078900100709       BEGSR sr_DatiComuni;
079000100709
079100100709       //?Tipo record
079200100709         SELECT;
079300100709           WHEN *in01;
079400100709             RCTtrc = 'A';
079500100709           WHEN *in02;
079600100709             RCTtrc = 'C';
079700100709           WHEN *in03;
079800100709             RCTtrc = 'I';
079900100709         ENDSL;
080000100709
080100100709       //?Data/Utente lancio
080200100709         RCTdat = wOggi;
080300100709         RCTpru = knmus;
080400100709
080500100709       //?Data avvio
080600100709         RCTavdal = ITA98dtavi;
080700100709         RCTaval  = ITA98dtavf;
080800100709       //?Data chiusura
080900100709         RCTchdal = ITA98dtchi;
081000100709         RCTchal  = ITA98dtchf;
081100100709       //?Data in corso
081200100709         RCTical  = ITA98dtica;
081300100709
081400100709       //?Dati unificante
081500100709         RCTrgf = $AgeU(zz);
081600100709         clear ds01;
081700100709         k_tblcod = '01';
081800100709         k_tblkey = %editc($AgeU(zz):'X');
081900100709         chain  %kds(K03tabel) TABEL00F;
082000100709         IF  %found( TABEL00F );
082100100709           ds01 = TBLuni;
082200100709         ENDIF;
082300100709         RCTdrg = §01age;
082400100709         RCTfun = §01fun;
082500100709
082600100709       //?Filiale
082700100709         RCTfil = %dec(%subst(%editc($AgeU(zz):'X'):1:3):3:0);
082800100709         chain  RCTfil AZORG01L;
082900100709         IF  %found(AZORG01L);
083000100709           RCTfid = ORGdes;
083100100709           RCTdiv = ORGfl3;
083200100709           RCTare = ORGcar;
083300100709         ENDIF;
083400100709
083500100709       //?Descrizione area
083600100709         clear ds05;
083700100709         k_tblcod = '05';
083800100709         k_tblkey = %editc(RCTare:'X');
083900100709         chain  %kds(K03tabel) TABEL00F;
084000100709         IF  %found( TABEL00F );
084100100709           ds05 = TBLuni;
084200100709         ENDIF;
084300100709         RCTard = §05des;
084400100709
084500100709       //?Descrizione direzione commerciale
084600100709         clear ds17;
084700100709         k_tblcod = '17';
084800100709         k_tblkey = RCTdiv;
084900100709         chain  %kds(K03tabel) TABEL00F;
085000100709         IF  %found( TABEL00F );
085100100709           ds17 = TBLuni;
085200100709         ENDIF;
085300100709         RCTdid = §17des;
085400100709
085500100709       ENDSR;
085600100805
085700100805       //--------------------------------------------------------------
085800100805       //?Controllo le attività legate alla trattativa.
085900100805       //--------------------------------------------------------------
086000100805       BEGSR Sr_Attivita;
086100100805
086200100805         $EndAtt = *off;
086300100805
086400100805         exec sql
086500100805          declare ATT cursor for
086600100805          select * from tiatc00f
086700100805          where atcnrv = :visnrv
086800100805          order by atcdad, atchda;
086900100805
087000100805         exec sql
087100100805           open ATT;
087200100805
087300100805         IF sqlcode < 0;
087400100805           $EndAtt = *on;
087500100805         ENDIF;
087600100805
087700100805         DOW not $EndAtt;
087800100805           exec sql
087900100805             fetch next from ATT into :tiatcds;
088000100805           IF sqlcod = 100 or sqlcod < 0;
088100100805             $EndAtt = *on;
088200100805             leave;
088300100805           ENDIF;
088400100805
088500100805         //?Conto gli appuntamenti eseguiti
088600100915         //?CHIODO '91'
088700100805           IF  ATCcad = '91' and ATCdco > 0 and ATCest = 'S';
088800100805             RCTnrapp += 1;
088900100805           //?mi salvo la data più alta
089000100805             IF ATCdco > RCTduapp;
089100100805               RCTduapp = ATCdco;
089200100805             ENDIF;
089300100805           ENDIF;
089400100805
089500100805         //?Conto quante attività scadute non eseguite
089600101007           IF ATCdad < woggi and ATCdco = 0;
089700100805             RCTnrscad += 1;
089800100805           ENDIF;
089900100805
090000100805         //?Memorizzo i dati della prossima attività da eseguire
090100101019           IF ATCdad >= woggi and ATCdco = 0 and RCTtat = *blanks;
090200100805             RCTtat  = ATCtat;
090300100805             RCTcad  = ATCcad;
090400100805             RCTdad  = ATCdad;
090500100805             RCThda  = ATChda;
090600100805           ENDIF;
090700100805
090800100805         ENDDO;
090900100805
091000100805         exec sql
091100100805           close ATT;
091200100712
091300100526       ENDSR;
091400080206
091500080206       //--------------------------------------------------------------
091600080206       //?Reperimento Dati del job (Utente/Operativi).
091700080206       //--------------------------------------------------------------
091800080206       BEGSR DatiJob;
091900080206
092000080206         in(E) §AzUte;
092100080206         if NOT %error;
092200080206           in(E) §DatiUte;
092300080206         endif;
092400080206         if %error or RSut = *blanks;
092500080206           clear TIBS34ds;
092600080206           tibs34r(tibs34ds);
092700080206           in §AzUte;
092800080206           in §DatiUte;
092900080206         endif;
093000100325
093100100709       //?Richiamo pgm di controllo autorizzazioni utente così mi torna
093200100709       //?il codice autorizzazione relativo alle tariffe clienti
093300100709       //?Non controllo se abilitato o meno, lo ha già fatto il chiamante
093400100325         reset TNTAA1DS;
093500100325         ITAA1caut = '§utegtc';
093600100325         kpjbu     = TNTAA1DS;
093700100325         tntaa1c (kpjba);
093800100325         TNTAA1DS = kpjbu;
093900100325
094000100709       //?Se utente abilitato come 'AZ' mi carico la sk delle filiali di area
094100100325         IF  OTAA1cabi = 'AZ';
094200100325           OTAA1cabi = 'RA';
094300100325         ENDIF;
094400100325
094500100709       //?Carico filiali abilitate all'utente
094600100709       //?Non controllo se caricate o no le filiali
094700100709       //?tanto se non ho caricato nessuna filiale e la richiesta
094800100709       //?è per tutti i commerciali, non stampo niente
094900110119       //?Se richiesta un'area carico SOLO le filiali di quell'area
095000110119         IF  ITA98car > 0;
095100110119           clear TRUL31DS;
095200110119           clear TRUL31DS2;
095300110119           I31abi = 'AO';
095400110119           I31car = ITA98car;
095500110119           trul31r (kpjba : trul31ds : trul31ds2);
095600110119         ELSE;
095700110922          clear TRUL31DS;
095800110922          clear TRUL31DS2;
095900110922          if Dutpou = 046 ;
096000110922           I31abi = 'AZ'     ;
096100110922          else ;
096200110119           I31abi = OTAA1cabi;
096300110119           I31cdi = DUTdis;
096400110119           I31car = DUTare;
096500110119           I31cpo = DUTpou;
096600110922          Endif;
096700110119           trul31r (kpjba : trul31ds : trul31ds2);
096800110119         ENDIF;
096900080206
097000080206       ENDSR;
097100100325
097200100325       //--------------------------------------------------------------
097300100325       //?Carico sk agenti unificanti.
097400100325       //--------------------------------------------------------------
097500100326       BEGSR sr_CarAgeUni;
097600100325
097700100709       //?Carico gli agenti unificanti delle filiali gestite dall'utente
097800110119       //?o dell'area se richiesti
097900100326         clear xx;
098000100326         clear $AGEU;
098100100326         $EndAge = *off;
098200100325
098300100709       //?Imposto la stringa per SQL
098400100325         wSQL = 'select TBLKEY +
098500100325                 from TABEL00F +
098600100325                 where TBLCOD = ''01'' and TBLFLG = '' '' and +
098700100325                       substr(TBLUNI, 26, 3) in (';
098800100325
098900100325         yy = 0;
099000100325         xx = 1;
099100100325         FOR xx by 1 to %elem(pog);
099200100325           IF pog(xx) <> *zeros and pog(xx) <> *blanks;
099300100325             IF yy > 0;
099400100325               wSQL += ', ';
099500100325             ELSE;
099600100325               yy = 1;
099700100325             ENDIF;
099800100325             wSQL += '''';
099900100325             wSQL += pog(xx);
100000100325             wSQL += '''';
100100100325           ENDIF;
100200100325         ENDFOR;
100300100325
100400100325         wSQL += ') and TBLKEY = substr(TBLUNI, 26, 7) and +
100500110114                    substr(TBLUNI, 78, 1) =  '' ''';
100600100709
100700100709       //?Controllo funzione commerciale da elaborare
100800100709         IF  ITA98elcmm = 'I';
100900101006           wSQL += ' and (substr(TBLUNI, 73, 5) = ''COMIN''' +
101000101006                   ' or substr(TBLUNI, 73, 5) = ''ASC  ''' +
101100101006                   ' or substr(TBLUNI, 73, 5) = ''SA   '')';
101200100709         ENDIF;
101300100709         IF  ITA98elcmm = 'E';
101400101006           wSQL += ' and substr(TBLUNI, 73, 5) <> ''COMIN''' +
101500101006                   ' and substr(TBLUNI, 73, 5) <> ''ASC  ''' +
101600101006                   ' and substr(TBLUNI, 73, 5) <> ''SA   ''';
101700100709         ENDIF;
101800100325
101900100709       //?Ordino per codice commerciale
102000100325         wSQL += ' order by TBLKEY +
102100100325                   for fetch only';
102200100325
102300100709       //?Dichiarazione cursore
102400100325         exec sql
102500100325           prepare A1   from :wSQL;
102600100325         exec sql
102700100325           declare AGEU cursor for A1;
102800100325
102900100709       //?Apertura del cursore
103000100325         exec sql
103100100325           open AGEU;
103200100325
103300100325         IF sqlcode < 0;
103400100325           $EndAge = *on;
103500100325         ENDIF;
103600100325
103700100709       //?Carico la schiera degli agenti unificanti
103800100325         yy = 0;
103900100325         DOW not $EndAge;
104000100325           exec sql
104100100325             fetch next from AGEU into :TBLKEY;
104200100325           IF sqlcod = 100 or sqlcod < 0;
104300100325             $EndAge = *on;
104400100325             leave;
104500100325           ENDIF;
104600100325
104700100325         yy += 1;
104800100326         $AGEU(yy) = %dec(%subst(TBLKEY:1:7):7:0);
104900100325
105000100325         ENDDO;
105100100325
105200100325         exec sql
105300100325           close AGEU;
105400100325
105500100325       ENDSR;
105600100326
105700080206       //--------------------------------------------------------------
105800080206       //?Operazioni finali.
105900080206       //--------------------------------------------------------------
106000080206       BEGSR RoutEnd;
106100100714
106200100714       //?Reimposto la kpjbu con la ds dei dati passati
106300100714         kpjbu = TNTA98ds;
106400090521
106500080206         *inLR = *on;
106600080206         return;
106700080206
106800080206       ENDSR;
106900080206
107000080206      /end-free
