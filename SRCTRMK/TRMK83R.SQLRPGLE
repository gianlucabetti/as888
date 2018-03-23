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
001200100520     fticpn05l  if   e           k disk
001300130917     fAZCMM01L  if   e           k disk
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
011900090806
012000090609      // - Gestione contatti
012100090609     d TRMK19ds      e ds                  inz
012200100415     d TRMK01ds      e ds                  inz
012300090807
012400090807      // File attività
012500090807     d tiatc00f      e ds                  extname(TIATC00F)
012600090629
012700080206      //---------------------------------------------------------------
012800080206      //?Definizione variabili globali.
012900080206      //---------------------------------------------------------------
013000080206
013100080206      // - Flags booleani
013200080208     d $Fine           s               n   inz(*off)
013300080208     d $InzS01         s               n   inz(*on)
013400080207     d $EoF            s               n   inz(*off)
013500090806     d $EndAge         s               n   inz(*off)
013600080206
013700080206      // - Indici di schiera
013800080208     d xx              s              3  0 inz
013900080206
014000080207      // - Campi associati al video
014100090806     d $Video          s              2    inz('S1')
014200080208     d S01nrr          s              4  0 inz
014300090601
014400090601     d dsp_mod         s             10I 0
014500090806
014600090806       // - Stringa SQL da eseguire
014700090806     d wSQL            s           2048    Varying        inz
014800080206
014900090401      // - Campi di comodo data
015000100428     d data_dmy        s               d   Datfmt(*dmy)
015100100428     d wData_ISO       s               d   datfmt(*iso)
015200100428     d wData_EUR       s               d   datfmt(*eur)
015300090508
015400090508      //---------------------------------------------------------------
015500090807      //?Definizione procedure esterne.
015600090508      //---------------------------------------------------------------
015700100120
015800100120      // - Inserimento attività
015900100120     d trmk19c         pr                  extpgm('TRMK19C')
016000100120     d  kpjba                              likeds(KPJBA)
016100100215     d  trmk19ds                           likeds(TRMK19ds)
016200100120
016300100120      //---------------------------------------------------------------
016400100120      //?prototipi
016500100120      //---------------------------------------------------------------
016600090807
016700080626      /copy gaitrasrc/srcprotopr,tibs02r
016800080626      /copy gaitrasrc/srcprotopr,tibs34r
016900090609      /copy gaitrasrc/srcprotopr,trmk19r
017000090601      /copy gaitrasrc/srcprotopr,QsnQryModS
017100090807
017200080206      //---------------------------------------------------------------
017300080206      //?Definizione key-list.
017400080206      //---------------------------------------------------------------
017500090807
017600080206
017700080206      //---------------------------------------------------------------
017800080206      //?Riepilogo indicatori.
017900080206      //---------------------------------------------------------------
018000090807
018100080207      // 28    : Emissione messaggio di errore a video
018200090807      // 30    : Condiziona SFLDSP
018300090807      // 31    : Condiziona SFLDSPCTL
018400080207      // 33    : Condiziona SFLEND
018500100914      // 40    : Visualizza impegno nel subfile
018600100914      // 41    : Riga subfile in RED se appuntamento
018700090518      // 50    : Errore: Posizione cursore
018800080207      // 99    : Generico di Errore
018900090807
019000080206      //---------------------------------------------------------------
019100080206
019200080206      //---------------------------------------------------------------
019300080206      //?M A I N - L I N E
019400080206      //---------------------------------------------------------------
019500080206
019600080206     c     *Entry        plist
019700080206     c                   parm                    KPJBA
019800091111     c                   parm                    TRMK83ds
019900080206
020000080206      /free
020100080206
020200090807       //?Operazioni iniziali
020300080206       exsr RoutInz;
020400090601
020500090807       //?Operazioni iniziali controllo se video accetta 132 caratteri
020600090806       if not %open(trmk83d);
020700090601           dsp_mod = QueryDisplayModeSupport('4':*omit :*omit :*omit);
020800090609           if dsp_mod = 1 ;
020900090806              open trmk83d;
021000090609           else ;
021100090807       //?Vado a fine in quanto il video non supporta i 132
021200090609              eval $fine = *on ;
021300090609           endif ;
021400090601       endif;
021500080206
021600090807       //?Gestione video
021700080206       DOW $Fine = *off;
021800090807
021900080206         select;
022000080206           when $Video = 'S1';
022100080206             exsr GesS01;
022200090428           other;
022300080206             $Fine = *on;
022400080206         endsl;
022500090807
022600080206       ENDDO;
022700080206
022800090807       //?Operazioni finali
022900080206       exsr RoutEnd;
023000080206
023100080206       //--------------------------------------------------------------
023200080206       //?Operazioni iniziali.
023300080206       //--------------------------------------------------------------
023400080206       BEGSR RoutInz;
023500080206
023600091111         $video = 'S1';
023700091111         $inzS01 = *on;
023800090807
023900090807         //?Reperimento dati job
024000090807         exsr DatiJob;
024100090807
024200090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
024300090806
024400090807         //?Impostazione campi "fissi"
024500080208         T01pgm = SDSpgm;
024600090806
024700090806         clear xx;
024800090806         clear $Age;
024900090806         exec sql
025000090806          declare AGE cursor for
025100130917          select CMMcod, CMMuni
025200130917          from AZCMM00F
025300130917          order by CMMcod;
025400090806
025500090806         exec sql
025600090806           open AGE;
025700090806           IF sqlcode < 0;
025800090806             $EndAge = *on;
025900090806           ENDIF;
026000090806
026100090806         DOW not $EndAge;
026200090806           exec sql
026300130917             fetch next from AGE into :CMMcod, :CMMuni;
026400090806             IF sqlcod = 100 or sqlcod < 0;
026500090806               $EndAge = *on;
026600090806               leave;
026700090806             ENDIF;
026800130917             IF  CMMuni = imk83cmm;
026900090806               xx += 1;
027000130917               $Age(xx) = CMMcod;
027100090806             ENDIF;
027200090806         ENDDO;
027300090806
027400090806         exec sql close AGE;
027500090714
027600080206       ENDSR;
027700080206
027800080206       //--------------------------------------------------------------
027900080206       //?Reperimento Dati del job (Utente/Operativi).
028000080206       //--------------------------------------------------------------
028100080206       BEGSR DatiJob;
028200080206
028300080206         in(E) §AzUte;
028400080206         if NOT %error;
028500080206           in(E) §DatiUte;
028600080206         endif;
028700080206         if %error or RSut = *blanks;
028800080206           clear TIBS34ds;
028900080206           tibs34r(tibs34ds);
029000080206           in §AzUte;
029100080206           in §DatiUte;
029200080206         endif;
029300080206
029400080206       ENDSR;
029500080206
029600090421       //--------------------------------------------------------------
029700080206       //?Gestione videata S01
029800080206       //--------------------------------------------------------------
029900080206       BEGSR GesS01;
030000080207
030100090807         //?Inizializzazione videata
030200080207         if  $InzS01 = *on;
030300080207            exsr InzS01;
030400080207            $InzS01  = *off;
030500080207         endif;
030600100121
030700100121         $EoF = *off;
030800100121         IF  S01nrr > C01rcd;
030900100121           C01rcd = 1;
031000100121         ENDIF;
031100080207
031200090807         //?Emissione Testata e Piede con tasti funzionali abilitati
031300090806           write  MK83T01;
031400090806           write  MK83P01;
031500080207
031600090807         //?Emissione videata
031700090806         exfmt  MK83C01;
031800080207         reset errMessage;
031900080207         reset errGenerico;
032000080207         clear V1Dmsg;
032100080207
032200080207         SELECT;
032300100428
032400100428         //?- F4=Giorno precedente
032500100428           WHEN  dsp_aid = c_F04;
032600100428             exsr F04S01;
032700100428
032800100428         //?- F5=Giorno sucessivo
032900100428           WHEN  dsp_aid = c_F05;
033000100428             exsr F05S01;
033100091111
033200091111         //?- F12=Ritorno
033300091111           WHEN  dsp_aid = c_F12;
033400091111             exsr F12S01;
033500080207
033600090807         //?Invio
033700080207           OTHER;
033800080207             exsr OpzS01;
033900080207
034000080207         ENDSL;
034100080207
034200080207       ENDSR;
034300080207
034400080207       //--------------------------------------------------------------
034500080207       //?Inizializzazione videata S01
034600080207       //--------------------------------------------------------------
034700080207       BEGSR InzS01;
034800080207
034900090807         //?Pulizia subfile
035000090806         exsr pulsf01;
035100081017
035200090807         //?Descrizione agente
035300130917         chain  (iMK83cmm)  AZCMM000;
035400130917         IF  NOT  %found( AZCMM01L );
035500130917           CMMdes = %editc( iMK83cmm : 'X' ) + ' ? ? ? ? ?';
035600090807         ENDIF;
035700090807
035800130917         V1dAGE = CMMdes;
035900100428
036000100428         //?Data visualizzata
036100100428         //?per ora il programma è richiamato per visualizzare il dettaglio
036200100428         //?di un giorno solo
036300100428         wData_ISO = %date(IMK83dati:*iso);
036400100428         wData_EUR = wData_ISO;
036500100428         V1data = %dec(wData_EUR);
036600100608         //?Recupero il nome del giorno
036700100608         clear V1giorno;
036800100608         exec sql
036900100608           set :V1giorno = dayname(:wData_ISO);
037000090807
037100090807         //?Caricamento dei dati da presentare nel sfl
037200080207         $EoF = *off;
037300090806         clear  s01nrr;
037400090806
037500090806         //?Imposto la stringa per SQL
037600090806         exsr  sr_PrepSQL;
037700090806
037800090806         //?Dichiarazione cursore
037900090806         exec sql
038000090806           prepare S1   from :wSQL;
038100090806         exec sql
038200090806           declare ATT  cursor for S1;
038300090323
038400090806         //?Apertura del cursore
038500090806         exec sql
038600090806           open ATT ;
038700090806
038800090806         IF sqlcode < 0;
038900090806           $Eof = *on;
039000090806         ENDIF;
039100090806
039200090806         dow  not $EoF;
039300090806           exec sql
039400090806             fetch next from ATT into :tiatc00f;
039500090806
039600090806             IF sqlcod = 100 or sqlcod < 0;
039700090806               $EoF = *on;
039800090806               leave;
039900090806             ENDIF;
040000090806
040100090806             exsr ries01;
040200100122         //?Quando raggiungo i 9000 rcd caricati nel subfile esco
040300100122             IF  S01nrr >= 9000;
040400100122               ErrMessage  = *on;
040500100122               ErrGenerico = *on;
040600100122               V1Dmsg = 'Superate 9000 attività caricate';
040700100122               $EoF = *on;
040800100122               leave;
040900100122             ENDIF;
041000080207         enddo;
041100080207
041200090806         exec sql
041300090806           close ATT;
041400090806
041500090807         //?Visualizzazione del SFL (se ci sono dati)
041600090806         SflDsp_N = (S01nrr <= *zeros);
041700090806
041800090807         //?Attivazione (eventuale) del SFLEND
041900090806         SflEnd  =  *on;
042000090625
042100090807         //?Impaginazione subfile
042200090807         //?-> forza l'impaginazione sul 1° rec. del subfile
042300080207         if S01nrr > *zero;
042400080207           C01rcd  = 1;
042500080207         else;
042600080207           clear C01rcd;
042700080207         endif;
042800090508
042900080207       ENDSR;
043000080207
043100090508       //--------------------------------------------------------------
043200090508       //?Pulizia SFL01
043300090508       //--------------------------------------------------------------
043400090806       BEGSR Pulsf01;
043500090508
043600090807         //?Pulizia subfile
043700090508         SflDsp_N    = *on;
043800090508         SflDspCtl_N = *on;
043900090806         write  MK83C01;
044000090508         SflDspCtl_N = *off;
044100090508         SflEnd      = *off;
044200090508
044300090508         clear C01rcd;
044400090508         clear S01nrr;
044500090508         clear V1Dmsg;
044600090508         errMessage  = *off;
044700090508         errGenerico = *off;
044800090508
044900090508       ENDSR ;
045000090806
045100090806       //--------------------------------------------------------------
045200090806       //?Preparazione stringa SQL.
045300090806       //--------------------------------------------------------------
045400090806       BEGSR sr_PrepSQL;
045500090806
045600090806         wSQL = 'select * from +
045700100120                  TIATC00F +
045800090806                  where atccmm in(';
045900090806
046000090806         xx = 1;
046100090806         FOR xx by 1 to %elem($Age);
046200090806           IF $Age(xx) <> *zeros;
046300090806             wSQL += ' ' + %editc($Age(xx):'X') + ',';
046400090806           ENDIF;
046500090806         ENDFOR;
046600090806
046700090806         wSQL += ') and atcdad between ' + %editc(imk83dati:'X') +
046800090806                 ' and ' + %editc(imk83datf:'X');
046900090806
047000100122         // wSQL += ' order by atcdad, atcoii, atcofi +
047100100122         wSQL += ' order by atcdad, atchda +
047200090806                   for fetch only';
047300090806
047400090806       ENDSR;
047500080207
047600080207       //--------------------------------------------------------------
047700080207       //?Caricamento singolo record nel SubFile S01
047800080207       //--------------------------------------------------------------
047900080208       BEGSR RieS01;
048000080207
048100090806         clear  MK83S01;
048200100914         RigaS01Red = *off;
048300100525
048400100525         //?CHIODO
048500100525         //?Se appuntamento '91' con causale chiusura attività '24' e esito 'N'
048600100525         //?non devo caricare
048700100525         IF  ATCcad = '91' and ATCcac = '24' and ATCest = 'N';
048800100525           leavesr;
048900100525         ENDIF;
049000100525         //?Se affiancamento '92' con causale chiusura attività 'NO' e esito 'N'
049100100525         //?non devo caricare in agenda
049200100525         IF  ATCcad = '92' and ATCcac = 'NO' and ATCest = 'N';
049300100525           leavesr;
049400100525         ENDIF;
049500100525         //?Se attvità qualsiasi chiusa con attività 'AA'
049600100525         //?non devo caricare in agenda
049700100525         IF  ATCcac = 'AA';
049800100525           leavesr;
049900100525         ENDIF;
050000100914
050100100916         //?Se attività di tipo 'A' --> APPUNTAMENTO la riga del subfile la visualizzo tutta rossa
050200100916         IF  ATCtat = 'A';
050300100914           RigaS01Red = *on;
050400100914         ENDIF;
050500080211
050600090807         //?Reperimento dati del potenziale
050700090515         s01cpo = atccpo ;
050800090807
050900090807         //?Ragione sociale
051000090611         chain     atccpo tncpo01l;
051100090323         If   %found(tncpo01l);
051200090323              s01rag = cporag ;
051300100331              s01ind = cpovia ;
051400090527              s01loc = cpocit ;
051500100331              s01cap = cpocap ;
051600090527              s01prv = cpoprv ;
051700090323         endif ;
051800080207
051900100331         //s01ccm = atccmm;
052000090807         data_dmy = %date(atcdad:*iso) ;
052100100122         s01dad = %dec(data_dmy) ;
052200100122         s01hda = %dec(%subst(%EDITC(atchda:'X'):1:4):4:0);
052300100122         s01cad = atccad ;
052400100611
052500100611         //?se previsto impegno lo visualizzo
052600100611         //?non considero le Ferie Totali perchè le ore impegno sono solo indicative
052700100611         VisImpegno = *off;
052800100611         IF  atcoii > 0 and atccad <> 'FT';
052900100611           VisImpegno = *on;
053000100611           s01oii = atcoii;
053100100611           s01ofi = atcofi;
053200100611         ENDIF;
053300100122
053400100122         //?memorizzo la data chiusura attività per richiamare in modo corretto
053500100122         //?il pgm di interrogazione attità
053600100122         sh1dco = atcdco;
053700090323
053800090807         //?decodifica della causale
053900090807         clear  dCCO;
054000090807         clear  TIBS02ds;
054100090807         T02mod = 'C';
054200090807         T02cod = 'CCO';
054300100122         T02ke1 = s01cad;
054400090807         T02sif = KNSIF;
054500090807         TNTBE_RicercaControllo  (kpjba : tibs02ds);
054600090807         if  T02err = *blanks;
054700090807           dcco = t02uni ;
054800100122           s01cadd = §ccodes ;
054900090807         endif ;
055000080207
055100100122         sh1atn = atcatn ;
055200100122         sh1atnp= atcatnp;
055300090428
055400090807         //?tipo attività
055500100122         s01tat = atctat;
055600090910
055700090910         //?note
055800100120         cpncpo  = atccpo;
055900100120         cpnatn  = atcatn;
056000100120         cpnatnp = atcatnp - 1;
056100100120         cpnpno  = 1;
056200100520         chain (CPNcpo:CPNatn:CPNatnp:CPNpno) TICPN05L;
056300100520         IF  %found(TICPN05L);
056400100120           s01note = cpnnot;
056500100120         ELSE;
056600100120           clear s01note;
056700100120         ENDIF;
056800100914
056900100914         //?Indicatori di visualizzazione
057000100914         SH1in40 = VisImpegno;
057100100914         SH1in41 = RigaS01Red;
057200090629
057300090807         S01nrr += 1;
057400090807         write MK83S01;
057500090601
057600080207       ENDSR;
057700100428
057800100428       //--------------------------------------------------------------
057900100428       //?Gestione tasto funzionale F4 da videata S01
058000100428       //?F4=Giorno precedente
058100100428       //--------------------------------------------------------------
058200100428       BEGSR F04S01;
058300100428
058400100428       //?Alla data passata tolgo 1 gg e salvo in data inizio e data fine
058500100428       //?per ora il programma è richiamato per visualizzare il dettaglio
058600100428       //?di un giorno solo
058700100428         wData_ISO = %date(IMK83dati:*iso);
058800100428         wData_ISO = wData_ISO - %days(1);
058900100428         IMK83dati = %dec(wData_ISO);
059000100428         IMK83datf = IMK83dati;
059100100428
059200100428       //?devo ricaricare il subfile
059300100428         $video = 'S1';
059400100428         $inzS01 = *on;
059500100428
059600100428       ENDSR;
059700100428
059800100428       //--------------------------------------------------------------
059900100428       //?Gestione tasto funzionale F5 da videata S01
060000100428       //?F5=Giorno sucessivo
060100100428       //--------------------------------------------------------------
060200100428       BEGSR F05S01;
060300100428
060400100428       //?Alla data passata aggiungo 1 gg e salvo in data inizio e data fine
060500100428       //?per ora il programma è richiamato per visualizzare il dettaglio
060600100428       //?di un giorno solo
060700100428         wData_ISO = %date(IMK83dati:*iso);
060800100428         wData_ISO = wData_ISO + %days(1);
060900100428         IMK83dati = %dec(wData_ISO);
061000100428         IMK83datf = IMK83dati;
061100100428
061200100428       //?devo ricaricare il subfile
061300100428         $video = 'S1';
061400100428         $inzS01 = *on;
061500100428
061600100428       ENDSR;
061700091111
061800091111       //--------------------------------------------------------------
061900091111       //?Gestione tasto funzionale F12 da videata S01
062000091111       //?F12=Ritorno
062100091111       //--------------------------------------------------------------
062200091111       BEGSR F12S01;
062300091111
062400091111         //?Chiusura del programma
062500091111         $Fine = *on;
062600091111
062700091111       ENDSR;
062800090807
062900080207       //--------------------------------------------------------------
063000080207       //?Gestione opzioni subfile
063100080207       //--------------------------------------------------------------
063200080207       BEGSR OpzS01;
063300100121
063400100121         WindDspF  = IndDspF;
063500100121         reset WindDspFb;
063600100121         IndDspF   = WindDspF;
063700100121
063800100121         clear S01Nrr;
063900080207
064000100121         DOW  not $EoF;
064100100120           S01nrr += 1;
064200100120           chain S01nrr MK83S01;
064300100120           IF  not %found;
064400100121             $EoF = *on;
064500100120             leave;
064600100120           ENDIF;
064700100914
064800100914           VisImpegno = SH1in40;
064900100914           RigaS01Red = SH1in41;
065000080207
065100080207           SELECT;
065200080207
065300090807             //?- Nessuna opzione
065400080208             WHEN  S01opz  = *blank;
065500110916
065600110916             //?- 2 = Modifica  in sede da errore
065700110916             WHEN  S01opz  = '2' and dutpou = 046 ;
065800110916               SH1in40 = VisImpegno;
065900110916               SH1in41 = RigaS01Red;
066000110916               errMessage  = *on;
066100110916               errGenerico = *on;
066200110916               PosCurOpz   = *on;
066300110916               V1Dmsg = $Msg(02);
066400110916               C01rcd = S01nrr;
066500110916               update MK83S01;
066600110916               $EoF = *on;
066700110916               leave;
066800090514
066900100108             //?- 2 = Modifica
067000100108             WHEN  S01opz  = '2';
067100100108                exsr CtrOpz;
067200100121                IF  ErrMessage;
067300100914                  SH1in40 = VisImpegno;
067400100914                  SH1in41 = RigaS01Red;
067500100121                  C01rcd = S01nrr;
067600100121                  update MK83S01;
067700100121                  $EoF = *on;
067800100121                  leave;
067900100121                ENDIF;
068000100121                exsr Calltrmk19  ;
068100100121                $InzS01 = *on;
068200100108
068300090807             //?- 5 = Visualizzazione
068400090514             WHEN  S01opz  = '5';
068500100120                exsr CtrOpz;
068600100121                IF  ErrMessage;
068700100914                  SH1in40 = VisImpegno;
068800100914                  SH1in41 = RigaS01Red;
068900100121                  C01rcd = S01nrr;
069000100121                  update MK83S01;
069100100121                  $EoF = *on;
069200100121                  leave;
069300100121                ENDIF;
069400100121                exsr Calltrmk19  ;
069500100121                $InzS01 = *on;
069600090514
069700090807             //?- ? = Opzione NON valida
069800080207             OTHER;
069900100914               SH1in40 = VisImpegno;
070000100914               SH1in41 = RigaS01Red;
070100080207               errMessage  = *on;
070200080207               errGenerico = *on;
070300080207               PosCurOpz   = *on;
070400090806               V1Dmsg = $Msg(01);
070500100121               C01rcd = S01nrr;
070600100121               update MK83S01;
070700100121               $EoF = *on;
070800100121               leave;
070900080207
071000080207           ENDSL;
071100080207
071200090807           //?Aggiornamento sfl
071300100914           SH1in40 = VisImpegno;
071400100914           SH1in41 = RigaS01Red;
071500100121           C01Rcd = S01Nrr;
071600090806           update MK83S01;
071700080207         ENDDO;
071800080207
071900080207       ENDSR;
072000100108
072100100108       //--------------------------------------------------------------
072200100108       //?Controllo se posso manutenzionare l'attività scelta
072300100108       //--------------------------------------------------------------
072400100108       BEGSR CtrOpz;
072500100108
072600100108           clear TIBS02ds;
072700100108           clear dTTA;
072800100108           T02mod = 'C';
072900100108           T02cod = 'TTA';
073000100122           T02ke1 = S01tat;
073100100108           T02sif = KNSIF;
073200100108           TNTBE_RicercaControllo (kpjba : tibs02ds);
073300100108           IF  T02err = *blanks;
073400100108             dTTA = T02uni;
073500100108           ENDIF;
073600100120           // IF  §TTAleg = 'S' and IMK83rich <> 'A' and S01opz = '2';
073700100120           IF  §TTAleg = 'S' and S01opz = '2';
073800100108             errMessage  = *on;
073900100108             errGenerico = *on;
074000100108             PosCurOpz   = *on;
074100100108             V1Dmsg = $Msg(01);
074200100108             V1Dmsg = %trim(V1Dmsg) + '. Attività legata a cliente';
074300100120             leavesr;
074400100108           ENDIF;
074500100930          // IF  IMK83rich = 'A';
074600100930          //  errMessage  = *on;
074700100930          //  errGenerico = *on;
074800100930          //  PosCurOpz   = *on;
074900100930          //  V1Dmsg = $Msg(01);
075000100930          //  V1Dmsg = %trim(V1Dmsg) + '. Gestione attività già aperta';
075100100930          //  leavesr;
075200100930          // ENDIF;
075300100108
075400100108       endsr;
075500080207
075600090611       //--------------------------------------------------------------
075700090611       //?Richiamo trmk19r Gestione dettaglio contatti
075800090611       //--------------------------------------------------------------
075900090611       BEGSR Calltrmk19 ;
076000090611
076100090807         clear trmk19ds ;
076200090807         imk19fle = s01opz ;
076300100122         imk19tco = s01tat ;
076400090807         imk19cpo = s01cpo ;
076500100122         imk19tat = s01tat ;
076600100122         imk19atn = sh1atn ;
076700100122         imk19atnp= sh1atnp;
076800091105         imk19fpr = 'A';
076900100122         IF  sh1dco <> 0;
077000100122           IMK19fatt = 'S';
077100100122         ENDIF;
077200100120
077300100504       //?se posso modificare vuol dire che vengo da menù perchè la modifica
077400100504       //?delle attività è possibile solo quando il programma è da menù e posso
077500100504       //?modificare solo le attivitù ufficio
077600100930        // IF  S01opz = '2';
077700100120           trmk19C (kpjba : trmk19ds) ;
077800100930        // ELSE;
077900100930        //   trmk19r (kpjba : trmk19ds) ;
078000100930        //  ENDIF;
078100090611
078200090611       endsr;
078300090807
078400080206       //--------------------------------------------------------------
078500080206       //?Operazioni finali.
078600080206       //--------------------------------------------------------------
078700080206       BEGSR RoutEnd;
078800090521
078900080206         *inLR = *on;
079000080206         return;
079100080206
079200080206       ENDSR;
079300080206
079400080206      /end-free
079500090807
079600080206       //--------------------------------------------------------------
079700080206       //?Schiere a tempo di compilazione.
079800080206       //--------------------------------------------------------------
079900080206
080000080206** - $MSG -------------------------------------------------------------------*
080100090806Opzione errata                                                                 01
080200110916Opzione errata. Si può solo visualizzare                                       02
