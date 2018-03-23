000100080206      //--------------------------------------------------------------
000200090806      //?TRMK83R - Interrogazione attività commerciale
000300080206      //--------------------------------------------------------------
000400080206
000500090407     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100090807     ftncpo01l  If   e           k disk
001200090807     ftabel00f  if   e           k disk
001300100520     fticpn05l  if   e           k disk
001400090806     fTRMK83D   cf   e             workstn
001500090806     f                                     sfile(MK83S01 : S01nrr)
001600080208     f                                     indds(IndDspF)
001700080206     f                                     infds(InfDspF)
001800090601     f                                     usropn
001900080206
002000080206      //---------------------------------------------------------------
002100090406      //?Definizione costanti.
002200080206      //---------------------------------------------------------------
002300080207
002400080207      // - Tasti funzionali a video
002500080207     d c_F01           c                   const(x'31')
002600080207     d c_F02           c                   const(x'32')
002700080207     d c_F03           c                   const(x'33')
002800090406     d c_F04           c                   const(x'34')
002900080207     d c_F05           c                   const(x'35')
003000080207     d c_F06           c                   const(x'36')
003100080207     d c_F07           c                   const(x'37')
003200080207     d c_F08           c                   const(x'38')
003300080207     d c_F09           c                   const(x'39')
003400080207     d c_F10           c                   const(x'3A')
003500090303     d c_F11           c                   const(x'3B')
003600080207     d c_F12           c                   const(x'3C')
003700080207     d c_F13           c                   const(x'B1')
003800080207     d c_F14           c                   const(x'B2')
003900080207     d c_F15           c                   const(x'B3')
004000080207     d c_F16           c                   const(x'B4')
004100080207     d c_F17           c                   const(x'B5')
004200080207     d c_F18           c                   const(x'B6')
004300080207     d c_F19           c                   const(x'B7')
004400080207     d c_F20           c                   const(x'B8')
004500080207     d c_F21           c                   const(x'B9')
004600080207     d c_F22           c                   const(x'BA')
004700080207     d c_F23           c                   const(x'BB')
004800080207     d c_F24           c                   const(x'BC')
004900080207     d c_Enter         c                   const(x'F1')
005000080207     d c_RollDown      c                   const(x'F4')
005100080207     d c_RollUp        c                   const(x'F5')
005200080206
005300080206      //---------------------------------------------------------------
005400080206      //?Definizione schiere.
005500080206      //---------------------------------------------------------------
005600080206
005700080206      // - Messaggi di errore
005800090609     d $Msg            s             78    dim(10) ctdata perrcd(1)
005900090806
006000090806      // - Agenti
006100100416     d $Age            s              7  0 dim(200)
006200080206
006300080206      //---------------------------------------------------------------
006400080206      //?Definizione aree dati.
006500080206      //---------------------------------------------------------------
006600080206
006700080206      // - Dati utente
006800080206     d §AzUte        e ds                  extname(AZUTE00F)
006900080206     d                                     dtaara
007000080206     d §DatiUte      e ds                  extname(dDatiUte)
007100080206     d                                     dtaara
007200080206
007300080206      //---------------------------------------------------------------
007400080206      //?Definizione strutture dati.
007500080206      //---------------------------------------------------------------
007600080206
007700080206      // - Status
007800080206     d Psds           sds
007900080206     d   SDSpgm          *proc
008000080206
008100080206      // - InfDS
008200080206     d InfDspF         ds
008300080207     d  dsp_aid              369    369a                                        AID byte
008400080206
008500080206      // - Indicatori su DspF
008600080208     d IndDspF         ds
008700080206        // - Indicatori di gestione del subfile
008800080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
008900080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009000080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009100100611        // - Indicatori di visualizzazione dati
009200100611     d  VisImpegno                    1n   overlay(IndDspF : 40)
009300100914     d  RigaS01Red                    1n   overlay(IndDspF : 41)
009400090529        // - Indicatori di errore subfile
009500090529     d  PosCurOpz                     1n   overlay(IndDspF : 50)
009600090422        // - Indicatori di errore generico
009700090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009800090807     d  ErrMessage                    1n   overlay(IndDspF : 28)
009900090407
010000090407     d WindDspF        ds                  inz
010100090407     d   WindDspFa             1     49    inz(*zero)
010200090407     d   WindDspFb            50     99    inz(*zero)
010300080206
010400080206      // - Parametri ricevuti
010500080206     d KPJBA         e ds
010600090806     d Trmk83ds      e ds
010700080206
010800080206      // - Reperimento dati utente
010900080206     d TIBS34ds      e ds
011000080206
011100080206      // - Ricerca/Controllo tabelle
011200080206     d TIBS02ds      e ds                  inz
011300090529
011400090402      // - Tabella CCO = Causale contatto
011500090402     d dcco          e ds                  inz
011600100108
011700100108      // - Tabella TTA = Tipo attività
011800100108     d dTTA          e ds                  inz
011900090402
012000090318      // - Tabella 01 = Codici Commerciali
012100090318     d ds01          e ds                  inz
012200090806
012300090609      // - Gestione contatti
012400090609     d TRMK19ds      e ds                  inz
012500100415     d TRMK01ds      e ds                  inz
012600090807
012700090807      // File attività
012800090807     d tiatc00f      e ds                  extname(TIATC00F)
012900090629
013000080206      //---------------------------------------------------------------
013100080206      //?Definizione variabili globali.
013200080206      //---------------------------------------------------------------
013300080206
013400080206      // - Flags booleani
013500080208     d $Fine           s               n   inz(*off)
013600080208     d $InzS01         s               n   inz(*on)
013700080207     d $EoF            s               n   inz(*off)
013800090806     d $EndAge         s               n   inz(*off)
013900080206
014000080206      // - Indici di schiera
014100080208     d xx              s              3  0 inz
014200080206
014300080207      // - Campi associati al video
014400090806     d $Video          s              2    inz('S1')
014500080208     d S01nrr          s              4  0 inz
014600090601
014700090601     d dsp_mod         s             10I 0
014800090806
014900090806       // - Stringa SQL da eseguire
015000090806     d wSQL            s           2048    Varying        inz
015100080206
015200090401      // - Campi di comodo data
015300100428     d data_dmy        s               d   Datfmt(*dmy)
015400100428     d wData_ISO       s               d   datfmt(*iso)
015500100428     d wData_EUR       s               d   datfmt(*eur)
015600090508
015700090508      //---------------------------------------------------------------
015800090807      //?Definizione procedure esterne.
015900090508      //---------------------------------------------------------------
016000100120
016100100120      // - Inserimento attività
016200100120     d trmk19c         pr                  extpgm('TRMK19C')
016300100120     d  kpjba                              likeds(KPJBA)
016400100215     d  trmk19ds                           likeds(TRMK19ds)
016500100120
016600100120      //---------------------------------------------------------------
016700100120      //?prototipi
016800100120      //---------------------------------------------------------------
016900090807
017000080626      /copy gaitrasrc/srcprotopr,tibs02r
017100080626      /copy gaitrasrc/srcprotopr,tibs34r
017200090609      /copy gaitrasrc/srcprotopr,trmk19r
017300090601      /copy gaitrasrc/srcprotopr,QsnQryModS
017400090807
017500080206      //---------------------------------------------------------------
017600080206      //?Definizione key-list.
017700080206      //---------------------------------------------------------------
017800090807
017900090807       // - File TABEL00F
018000090807     d k03tabel      e ds                  extname(TABEL00F:*key)
018100090807     d                                     prefix(k_)
018200090807     d                                     inz
018300080206
018400080206      //---------------------------------------------------------------
018500080206      //?Riepilogo indicatori.
018600080206      //---------------------------------------------------------------
018700090807
018800080207      // 28    : Emissione messaggio di errore a video
018900090807      // 30    : Condiziona SFLDSP
019000090807      // 31    : Condiziona SFLDSPCTL
019100080207      // 33    : Condiziona SFLEND
019200100914      // 40    : Visualizza impegno nel subfile
019300100914      // 41    : Riga subfile in RED se appuntamento
019400090518      // 50    : Errore: Posizione cursore
019500080207      // 99    : Generico di Errore
019600090807
019700080206      //---------------------------------------------------------------
019800080206
019900080206      //---------------------------------------------------------------
020000080206      //?M A I N - L I N E
020100080206      //---------------------------------------------------------------
020200080206
020300080206     c     *Entry        plist
020400080206     c                   parm                    KPJBA
020500091111     c                   parm                    TRMK83ds
020600080206
020700080206      /free
020800080206
020900090807       //?Operazioni iniziali
021000080206       exsr RoutInz;
021100090601
021200090807       //?Operazioni iniziali controllo se video accetta 132 caratteri
021300090806       if not %open(trmk83d);
021400090601           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
021500090609           if dsp_mod = 1 ;
021600090806              open trmk83d;
021700090609           else ;
021800090807       //?Vado a fine in quanto il video non supporta i 132
021900090609              eval $fine = *on ;
022000090609           endif ;
022100090601       endif;
022200080206
022300090807       //?Gestione video
022400080206       DOW $Fine = *off;
022500090807
022600080206         select;
022700080206           when $Video = 'S1';
022800080206             exsr GesS01;
022900090428           other;
023000080206             $Fine = *on;
023100080206         endsl;
023200090807
023300080206       ENDDO;
023400080206
023500090807       //?Operazioni finali
023600080206       exsr RoutEnd;
023700080206
023800080206       //--------------------------------------------------------------
023900080206       //?Operazioni iniziali.
024000080206       //--------------------------------------------------------------
024100080206       BEGSR RoutInz;
024200080206
024300091111         $video = 'S1';
024400091111         $inzS01 = *on;
024500090807
024600090807         //?Reperimento dati job
024700090807         exsr DatiJob;
024800090807
024900090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
025000090806
025100090807         //?Impostazione campi "fissi"
025200080208         T01pgm = SDSpgm;
025300080208         TBLkut = 1;
025400090806
025500090806         clear xx;
025600090806         clear $Age;
025700090806         exec sql
025800090806          declare AGE cursor for
025900090806          select tblkey, tbluni
026000090807          from tabel00f where tblkut = '1' and tblcod = '01' and
026100090807          tblflg = ' ' order by tblkut, tblcod, tblkey;
026200090806
026300090806         exec sql
026400090806           open AGE;
026500090806           IF sqlcode < 0;
026600090806             $EndAge = *on;
026700090806           ENDIF;
026800090806
026900090806         DOW not $EndAge;
027000090806           exec sql
027100090806             fetch next from AGE into :tblkey, :tbluni;
027200090806             IF sqlcod = 100 or sqlcod < 0;
027300090806               $EndAge = *on;
027400090806               leave;
027500090806             ENDIF;
027600090806             ds01 = Tbluni;
027700090806             IF  §01rgf = imk83cmm;
027800090806               xx += 1;
027900090806               $Age(xx) = %dec(%subst(tblkey:1:7):7:0);
028000090806             ENDIF;
028100090806         ENDDO;
028200090806
028300090806         exec sql close AGE;
028400090714
028500080206       ENDSR;
028600080206
028700080206       //--------------------------------------------------------------
028800080206       //?Reperimento Dati del job (Utente/Operativi).
028900080206       //--------------------------------------------------------------
029000080206       BEGSR DatiJob;
029100080206
029200080206         in(E) §AzUte;
029300080206         if NOT %error;
029400080206           in(E) §DatiUte;
029500080206         endif;
029600080206         if %error or RSut = *blanks;
029700080206           clear TIBS34ds;
029800080206           tibs34r(tibs34ds);
029900080206           in §AzUte;
030000080206           in §DatiUte;
030100080206         endif;
030200080206
030300080206       ENDSR;
030400080206
030500090421       //--------------------------------------------------------------
030600080206       //?Gestione videata S01
030700080206       //--------------------------------------------------------------
030800080206       BEGSR GesS01;
030900080207
031000090807         //?Inizializzazione videata
031100080207         if  $InzS01 = *on;
031200080207            exsr InzS01;
031300080207            $InzS01  = *off;
031400080207         endif;
031500100121
031600100121         $EoF = *off;
031700100121         IF  S01nrr > C01rcd;
031800100121           C01rcd = 1;
031900100121         ENDIF;
032000080207
032100090807         //?Emissione Testata e Piede con tasti funzionali abilitati
032200090806           write  MK83T01;
032300090806           write  MK83P01;
032400080207
032500090807         //?Emissione videata
032600090806         exfmt  MK83C01;
032700080207         reset errMessage;
032800080207         reset errGenerico;
032900080207         clear V1Dmsg;
033000080207
033100080207         SELECT;
033200100428
033300100428         //?- F4=Giorno precedente
033400100428           WHEN  dsp_aid = c_F04;
033500100428             exsr F04S01;
033600100428
033700100428         //?- F5=Giorno sucessivo
033800100428           WHEN  dsp_aid = c_F05;
033900100428             exsr F05S01;
034000091111
034100091111         //?- F12=Ritorno
034200091111           WHEN  dsp_aid = c_F12;
034300091111             exsr F12S01;
034400080207
034500090807         //?Invio
034600080207           OTHER;
034700080207             exsr OpzS01;
034800080207
034900080207         ENDSL;
035000080207
035100080207       ENDSR;
035200080207
035300080207       //--------------------------------------------------------------
035400080207       //?Inizializzazione videata S01
035500080207       //--------------------------------------------------------------
035600080207       BEGSR InzS01;
035700080207
035800090807         //?Pulizia subfile
035900090806         exsr pulsf01;
036000081017
036100090807         //?Descrizione agente
036200090807         clear ds01;
036300090807         k_tblkut = 1;
036400090807         k_tblcod = '01';
036500090807         k_tblkey = %editc(imk83cmm:'X');
036600090807         chain  %kds(K03tabel) TABEL00F;
036700090807         IF  %found( TABEL00F ) and TBLflg = *blanks;
036800090807         ds01 = TBLuni;
036900090807         ENDIF;
037000090807
037100090807         V1dAGE = §01age;
037200100428
037300100428         //?Data visualizzata
037400100428         //?per ora il programma è richiamato per visualizzare il dettaglio
037500100428         //?di un giorno solo
037600100428         wData_ISO = %date(IMK83dati:*iso);
037700100428         wData_EUR = wData_ISO;
037800100428         V1data = %dec(wData_EUR);
037900100608         //?Recupero il nome del giorno
038000100608         clear V1giorno;
038100100608         exec sql
038200100608           set :V1giorno = dayname(:wData_ISO);
038300090807
038400090807         //?Caricamento dei dati da presentare nel sfl
038500080207         $EoF = *off;
038600090806         clear  s01nrr;
038700090806
038800090806         //?Imposto la stringa per SQL
038900090806         exsr  sr_PrepSQL;
039000090806
039100090806         //?Dichiarazione cursore
039200090806         exec sql
039300090806           prepare S1   from :wSQL;
039400090806         exec sql
039500090806           declare ATT  cursor for S1;
039600090323
039700090806         //?Apertura del cursore
039800090806         exec sql
039900090806           open ATT ;
040000090806
040100090806         IF sqlcode < 0;
040200090806           $Eof = *on;
040300090806         ENDIF;
040400090806
040500090806         dow  not $EoF;
040600090806           exec sql
040700090806             fetch next from ATT into :tiatc00f;
040800090806
040900090806             IF sqlcod = 100 or sqlcod < 0;
041000090806               $EoF = *on;
041100090806               leave;
041200090806             ENDIF;
041300090806
041400090806             exsr ries01;
041500100122         //?Quando raggiungo i 9000 rcd caricati nel subfile esco
041600100122             IF  S01nrr >= 9000;
041700100122               ErrMessage  = *on;
041800100122               ErrGenerico = *on;
041900100122               V1Dmsg = 'Superate 9000 attività caricate';
042000100122               $EoF = *on;
042100100122               leave;
042200100122             ENDIF;
042300080207         enddo;
042400080207
042500090806         exec sql
042600090806           close ATT;
042700090806
042800090807         //?Visualizzazione del SFL (se ci sono dati)
042900090806         SflDsp_N = (S01nrr <= *zeros);
043000090806
043100090807         //?Attivazione (eventuale) del SFLEND
043200090806         SflEnd  =  *on;
043300090625
043400090807         //?Impaginazione subfile
043500090807         //?-> forza l'impaginazione sul 1° rec. del subfile
043600080207         if S01nrr > *zero;
043700080207           C01rcd  = 1;
043800080207         else;
043900080207           clear C01rcd;
044000080207         endif;
044100090508
044200080207       ENDSR;
044300080207
044400090508       //--------------------------------------------------------------
044500090508       //?Pulizia SFL01
044600090508       //--------------------------------------------------------------
044700090806       BEGSR Pulsf01;
044800090508
044900090807         //?Pulizia subfile
045000090508         SflDsp_N    = *on;
045100090508         SflDspCtl_N = *on;
045200090806         write  MK83C01;
045300090508         SflDspCtl_N = *off;
045400090508         SflEnd      = *off;
045500090508
045600090508         clear C01rcd;
045700090508         clear S01nrr;
045800090508         clear V1Dmsg;
045900090508         errMessage  = *off;
046000090508         errGenerico = *off;
046100090508
046200090508       ENDSR ;
046300090806
046400090806       //--------------------------------------------------------------
046500090806       //?Preparazione stringa SQL.
046600090806       //--------------------------------------------------------------
046700090806       BEGSR sr_PrepSQL;
046800090806
046900090806         wSQL = 'select * from +
047000100120                  TIATC00F +
047100090806                  where atccmm in(';
047200090806
047300090806         xx = 1;
047400090806         FOR xx by 1 to %elem($Age);
047500090806           IF $Age(xx) <> *zeros;
047600090806             wSQL += ' ' + %editc($Age(xx):'X') + ',';
047700090806           ENDIF;
047800090806         ENDFOR;
047900090806
048000090806         wSQL += ') and atcdad between ' + %editc(imk83dati:'X') +
048100090806                 ' and ' + %editc(imk83datf:'X');
048200090806
048300100122         // wSQL += ' order by atcdad, atcoii, atcofi +
048400100122         wSQL += ' order by atcdad, atchda +
048500090806                   for fetch only';
048600090806
048700090806       ENDSR;
048800080207
048900080207       //--------------------------------------------------------------
049000080207       //?Caricamento singolo record nel SubFile S01
049100080207       //--------------------------------------------------------------
049200080208       BEGSR RieS01;
049300080207
049400090806         clear  MK83S01;
049500100914         RigaS01Red = *off;
049600100525
049700100525         //?CHIODO
049800100525         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
049900100525         //?non devo caricare
050000100525         IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
050100100525           leavesr;
050200100525         ENDIF;
050300100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
050400100525         //?non devo caricare in agenda
050500100525         IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
050600100525           leavesr;
050700100525         ENDIF;
050800100525         //?Se attvità qualsiasi chiusa con attività 'AA'
050900100525         //?non devo caricare in agenda
051000100525         IF  ATCcac = 'AA';
051100100525           leavesr;
051200100525         ENDIF;
051300100914
051400100916         //?Se attività di tipo 'A' --> APPUNTAMENTO la riga del subfile la visualizzo tutta rossa
051500100916         IF  ATCtat = 'A';
051600100914           RigaS01Red = *on;
051700100914         ENDIF;
051800080211
051900090807         //?Reperimento dati del potenziale
052000090515         s01cpo = atccpo ;
052100090807
052200090807         //?Ragione sociale
052300090611         chain     atccpo tncpo01l;
052400090323         If   %found(tncpo01l);
052500090323              s01rag = cporag ;
052600100331              s01ind = cpovia ;
052700090527              s01loc = cpocit ;
052800100331              s01cap = cpocap ;
052900090527              s01prv = cpoprv ;
053000090323         endif ;
053100080207
053200100331         //s01ccm = atccmm;
053300090807         data_dmy = %date(atcdad:*iso) ;
053400100122         s01dad = %dec(data_dmy) ;
053500100122         s01hda = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
053600100122         s01cad = atccad ;
053700100611
053800100611         //?se previsto impegno lo visualizzo
053900100611         //?non considero le Ferie Totali perchè le ore impegno sono solo indicative
054000100611         VisImpegno = *off;
054100100611         IF  atcoii > 0 and atccad <> 'FT';
054200100611           VisImpegno = *on;
054300100611           s01oii = atcoii;
054400100611           s01ofi = atcofi;
054500100611         ENDIF;
054600100122
054700100122         //?memorizzo la data chiusura attività per richiamare in modo corretto
054800100122         //?il pgm di interrogazione attità
054900100122         sh1dco = atcdco;
055000090323
055100090807         //?decodifica della causale
055200090807         clear  dCCO;
055300090807         clear  TIBS02ds;
055400090807         T02mod = 'C';
055500090807         T02cod = 'CCO';
055600100122         T02ke1 = s01cad;
055700090807         T02sif = KNSIF;
055800090807         TNTBE_RicercaControllo  (kpjba : tibs02ds);
055900090807         if  T02err = *blanks;
056000090807           dcco = t02uni ;
056100100122           s01cadd = §ccodes ;
056200090807         endif ;
056300080207
056400100122         sh1atn = atcatn ;
056500100122         sh1atnp= atcatnp;
056600090428
056700090807         //?tipo attività
056800100122         s01tat = atctat;
056900090910
057000090910         //?note
057100100120         cpncpo  = atccpo;
057200100120         cpnatn  = atcatn;
057300100120         cpnatnp = atcatnp - 1;
057400100120         cpnpno  = 1;
057500100520         chain (CPNcpo:CPNatn:CPNatnp:CPNpno) TICPN05L;
057600100520         IF  %found(TICPN05L);
057700100120           s01note = cpnnot;
057800100120         ELSE;
057900100120           clear s01note;
058000100120         ENDIF;
058100100914
058200100914         //?Indicatori di visualizzazione
058300100914         SH1in40 = VisImpegno;
058400100914         SH1in41 = RigaS01Red;
058500090629
058600090807         S01nrr += 1;
058700090807         write MK83S01;
058800090601
058900080207       ENDSR;
059000100428
059100100428       //--------------------------------------------------------------
059200100428       //?Gestione tasto funzionale F4 da videata S01
059300100428       //?F4=Giorno precedente
059400100428       //--------------------------------------------------------------
059500100428       BEGSR F04S01;
059600100428
059700100428       //?Alla data passata tolgo 1 gg e salvo in data inizio e data fine
059800100428       //?per ora il programma è richiamato per visualizzare il dettaglio
059900100428       //?di un giorno solo
060000100428         wData_ISO = %date(IMK83dati:*iso);
060100100428         wData_ISO = wData_ISO - %days(1);
060200100428         IMK83dati = %dec(wData_ISO);
060300100428         IMK83datf = IMK83dati;
060400100428
060500100428       //?devo ricaricare il subfile
060600100428         $video = 'S1';
060700100428         $inzS01 = *on;
060800100428
060900100428       ENDSR;
061000100428
061100100428       //--------------------------------------------------------------
061200100428       //?Gestione tasto funzionale F5 da videata S01
061300100428       //?F5=Giorno sucessivo
061400100428       //--------------------------------------------------------------
061500100428       BEGSR F05S01;
061600100428
061700100428       //?Alla data passata aggiungo 1 gg e salvo in data inizio e data fine
061800100428       //?per ora il programma è richiamato per visualizzare il dettaglio
061900100428       //?di un giorno solo
062000100428         wData_ISO = %date(IMK83dati:*iso);
062100100428         wData_ISO = wData_ISO + %days(1);
062200100428         IMK83dati = %dec(wData_ISO);
062300100428         IMK83datf = IMK83dati;
062400100428
062500100428       //?devo ricaricare il subfile
062600100428         $video = 'S1';
062700100428         $inzS01 = *on;
062800100428
062900100428       ENDSR;
063000091111
063100091111       //--------------------------------------------------------------
063200091111       //?Gestione tasto funzionale F12 da videata S01
063300091111       //?F12=Ritorno
063400091111       //--------------------------------------------------------------
063500091111       BEGSR F12S01;
063600091111
063700091111         //?Chiusura del programma
063800091111         $Fine = *on;
063900091111
064000091111       ENDSR;
064100090807
064200080207       //--------------------------------------------------------------
064300080207       //?Gestione opzioni subfile
064400080207       //--------------------------------------------------------------
064500080207       BEGSR OpzS01;
064600100121
064700100121         WindDspF  = IndDspF;
064800100121         reset WindDspFb;
064900100121         IndDspF   = WindDspF;
065000100121
065100100121         clear S01Nrr;
065200080207
065300100121         DOW  not $EoF;
065400100120           S01nrr += 1;
065500100120           chain S01nrr MK83S01;
065600100120           IF  not %found;
065700100121             $EoF = *on;
065800100120             leave;
065900100120           ENDIF;
066000100914
066100100914           VisImpegno = SH1in40;
066200100914           RigaS01Red = SH1in41;
066300080207
066400080207           SELECT;
066500080207
066600090807             //?- Nessuna opzione
066700080208             WHEN  S01opz  = *blank;
066800110916
066900110916             //?- 2 = Modifica  in sede da errore
067000110916             WHEN  S01opz  = '2' and dutpou = 046 ;
067100110916               SH1in40 = VisImpegno;
067200110916               SH1in41 = RigaS01Red;
067300110916               errMessage  = *on;
067400110916               errGenerico = *on;
067500110916               PosCurOpz   = *on;
067600110916               V1Dmsg = $Msg(02);
067700110916               C01rcd = S01nrr;
067800110916               update MK83S01;
067900110916               $EoF = *on;
068000110916               leave;
068100090514
068200100108             //?- 2 = Modifica
068300100108             WHEN  S01opz  = '2';
068400100108                exsr CtrOpz;
068500100121                IF  ErrMessage;
068600100914                  SH1in40 = VisImpegno;
068700100914                  SH1in41 = RigaS01Red;
068800100121                  C01rcd = S01nrr;
068900100121                  update MK83S01;
069000100121                  $EoF = *on;
069100100121                  leave;
069200100121                ENDIF;
069300100121                exsr Calltrmk19  ;
069400100121                $InzS01 = *on;
069500100108
069600090807             //?- 5 = Visualizzazione
069700090514             WHEN  S01opz  = '5';
069800100120                exsr CtrOpz;
069900100121                IF  ErrMessage;
070000100914                  SH1in40 = VisImpegno;
070100100914                  SH1in41 = RigaS01Red;
070200100121                  C01rcd = S01nrr;
070300100121                  update MK83S01;
070400100121                  $EoF = *on;
070500100121                  leave;
070600100121                ENDIF;
070700100121                exsr Calltrmk19  ;
070800100121                $InzS01 = *on;
070900090514
071000090807             //?- ? = Opzione NON valida
071100080207             OTHER;
071200100914               SH1in40 = VisImpegno;
071300100914               SH1in41 = RigaS01Red;
071400080207               errMessage  = *on;
071500080207               errGenerico = *on;
071600080207               PosCurOpz   = *on;
071700090806               V1Dmsg = $Msg(01);
071800100121               C01rcd = S01nrr;
071900100121               update MK83S01;
072000100121               $EoF = *on;
072100100121               leave;
072200080207
072300080207           ENDSL;
072400080207
072500090807           //?Aggiornamento sfl
072600100914           SH1in40 = VisImpegno;
072700100914           SH1in41 = RigaS01Red;
072800100121           C01Rcd = S01Nrr;
072900090806           update MK83S01;
073000080207         ENDDO;
073100080207
073200080207       ENDSR;
073300100108
073400100108       //--------------------------------------------------------------
073500100108       //?Controllo se posso manutenzionare l'attività scelta
073600100108       //--------------------------------------------------------------
073700100108       BEGSR CtrOpz;
073800100108
073900100108           clear TIBS02ds;
074000100108           clear dTTA;
074100100108           T02mod = 'C';
074200100108           T02cod = 'TTA';
074300100122           T02ke1 = S01tat;
074400100108           T02sif = KNSIF;
074500100108           TNTBE_RicercaControllo (kpjba : tibs02ds);
074600100108           IF  T02err = *blanks;
074700100108             dTTA = T02uni;
074800100108           ENDIF;
074900100120           // IF  §TTAleg = 'S' and IMK83rich <> 'A' and S01opz = '2';
075000100120           IF  §TTAleg = 'S' and S01opz = '2';
075100100108             errMessage  = *on;
075200100108             errGenerico = *on;
075300100108             PosCurOpz   = *on;
075400100108             V1Dmsg = $Msg(01);
075500100108             V1Dmsg = %trim(V1Dmsg) + '. Attività legata a cliente';
075600100120             leavesr;
075700100108           ENDIF;
075800100930          // IF  IMK83rich = 'A';
075900100930          //  errMessage  = *on;
076000100930          //  errGenerico = *on;
076100100930          //  PosCurOpz   = *on;
076200100930          //  V1Dmsg = $Msg(01);
076300100930          //  V1Dmsg = %trim(V1Dmsg) + '. Gestione attività già aperta';
076400100930          //  leavesr;
076500100930          // ENDIF;
076600100108
076700100108       endsr;
076800080207
076900090611       //--------------------------------------------------------------
077000090611       //?Richiamo trmk19r Gestione dettaglio contatti
077100090611       //--------------------------------------------------------------
077200090611       BEGSR Calltrmk19 ;
077300090611
077400090807         clear trmk19ds ;
077500090807         imk19fle = s01opz ;
077600100122         imk19tco = s01tat ;
077700090807         imk19cpo = s01cpo ;
077800100122         imk19tat = s01tat ;
077900100122         imk19atn = sh1atn ;
078000100122         imk19atnp= sh1atnp;
078100091105         imk19fpr = 'A';
078200100122         IF  sh1dco <> 0;
078300100122           IMK19fatt = 'S';
078400100122         ENDIF;
078500100120
078600100504       //?se posso modificare vuol dire che vengo da menù perchè la modifica
078700100504       //?delle attività è possibile solo quando il programma è da menù e posso
078800100504       //?modificare solo le attivitù ufficio
078900100930        // IF  S01opz = '2';
079000100120           trmk19C (kpjba : trmk19ds) ;
079100100930        // ELSE;
079200100930        //   trmk19r (kpjba : trmk19ds) ;
079300100930        //  ENDIF;
079400090611
079500090611       endsr;
079600090807
079700080206       //--------------------------------------------------------------
079800080206       //?Operazioni finali.
079900080206       //--------------------------------------------------------------
080000080206       BEGSR RoutEnd;
080100090521
080200080206         *inLR = *on;
080300080206         return;
080400080206
080500080206       ENDSR;
080600080206
080700080206      /end-free
080800090807
080900080206       //--------------------------------------------------------------
081000080206       //?Schiere a tempo di compilazione.
081100080206       //--------------------------------------------------------------
081200080206
081300080206** - $MSG -------------------------------------------------------------------*
081400090806Opzione errata                                                                 01
081500110916Opzione errata. Si può solo visualizzare                                       02
