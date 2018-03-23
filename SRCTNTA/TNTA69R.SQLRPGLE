000100080206      //--------------------------------------------------------------
000200091111      //?TNTA69R - Interrogazione clienti per potenziale
000300080206      //--------------------------------------------------------------
000400080206
000500091124     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600090807     h dftactgrp(*no) actgrp(*caller)
000700080206
000800080206      //---------------------------------------------------------------
000900080206      //?Dichiarazione file.
001000080206      //---------------------------------------------------------------
001100091111     ftncpo01l  if   e           k disk
001200130904     fAZCMM01L  if   e           k disk
001300091111     fTNTA69D   cf   e             workstn
001400091111     f                                     sfile(TA69S01 : VS1nrr)
001500080208     f                                     indds(IndDspF)
001600080206     f                                     infds(InfDspF)
001700080206
001800080206      //---------------------------------------------------------------
001900090406      //?Definizione costanti.
002000080206      //---------------------------------------------------------------
002100080207
002200080207      // - Tasti funzionali a video
002300080207     d c_F01           c                   const(x'31')
002400080207     d c_F02           c                   const(x'32')
002500080207     d c_F03           c                   const(x'33')
002600090406     d c_F04           c                   const(x'34')
002700080207     d c_F05           c                   const(x'35')
002800080207     d c_F06           c                   const(x'36')
002900080207     d c_F07           c                   const(x'37')
003000080207     d c_F08           c                   const(x'38')
003100080207     d c_F09           c                   const(x'39')
003200080207     d c_F10           c                   const(x'3A')
003300090303     d c_F11           c                   const(x'3B')
003400080207     d c_F12           c                   const(x'3C')
003500080207     d c_F13           c                   const(x'B1')
003600080207     d c_F14           c                   const(x'B2')
003700080207     d c_F15           c                   const(x'B3')
003800080207     d c_F16           c                   const(x'B4')
003900080207     d c_F17           c                   const(x'B5')
004000080207     d c_F18           c                   const(x'B6')
004100080207     d c_F19           c                   const(x'B7')
004200080207     d c_F20           c                   const(x'B8')
004300080207     d c_F21           c                   const(x'B9')
004400080207     d c_F22           c                   const(x'BA')
004500080207     d c_F23           c                   const(x'BB')
004600080207     d c_F24           c                   const(x'BC')
004700080207     d c_Enter         c                   const(x'F1')
004800080207     d c_RollDown      c                   const(x'F4')
004900080207     d c_RollUp        c                   const(x'F5')
005000091124
005100091124      // - Opzioni a video
005200110506     d c_opz           c                   const('                        ')
005300091124     d c_opz1          c                   const('1=Scelta')
005400100319     d c_opz2          c                   const('2=Modifica  5=Visualizza')
005500091124     d c_opz5          c                   const('5=Visualizza')
005600080206
005700080206      //---------------------------------------------------------------
005800080206      //?Definizione schiere.
005900080206      //---------------------------------------------------------------
006000080206
006100080206      // - Messaggi di errore
006200090609     d $Msg            s             78    dim(10) ctdata perrcd(1)
006300080206
006400080206      //---------------------------------------------------------------
006500080206      //?Definizione aree dati.
006600080206      //---------------------------------------------------------------
006700080206
006800080206      // - Dati utente
006900080206     d §AzUte        e ds                  extname(AZUTE00F)
007000080206     d                                     dtaara
007100080206     d §DatiUte      e ds                  extname(dDatiUte)
007200080206     d                                     dtaara
007300080206
007400080206      //---------------------------------------------------------------
007500080206      //?Definizione strutture dati.
007600080206      //---------------------------------------------------------------
007700080206
007800080206      // - Status
007900080206     d Psds           sds
008000080206     d   SDSpgm          *proc
008100080206
008200080206      // - InfDS
008300080206     d InfDspF         ds
008400080207     d  dsp_aid              369    369a
008500080206
008600080206      // - Indicatori su DspF
008700080208     d IndDspF         ds
008800080206        // - Indicatori di gestione del subfile
008900080206     d  SflDsp_N                      1n   overlay(IndDspF : 30)
009000080208     d  SflDspCtl_N                   1n   overlay(IndDspF : 31)
009100080206     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009200080206     d  SflEnd                        1n   overlay(IndDspF : 33)
009300090529        // - Indicatori di errore subfile
009400090529     d  PosCurOpz                     1n   overlay(IndDspF : 50)
009500090422        // - Indicatori di errore generico
009600090807     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009700090807     d  ErrMessage                    1n   overlay(IndDspF : 28)
009800090407
009900090407     d WindDspF        ds                  inz
010000090407     d   WindDspFa             1     49    inz(*zero)
010100090407     d   WindDspFb            50     99    inz(*zero)
010200091124
010300091124      // - Data
010400091124     d                 ds
010500091124     d wdata                   1      6  0
010600091124     d wgg                     1      2  0
010700091124     d wmm                     3      4  0
010800091124     d waa                     5      6  0
010900110506
011000110506      // - Parametri per ricerca Programmi chiamanti
011100110506     d dsStack         ds
011200110506     d  skStack                      10    Dim(100)
011300080206
011400080206      // - Parametri ricevuti
011500080206     d KPJBA         e ds
011600091111     d Tnta69ds      e ds
011700091111
011800091111      // - Parametri per Reperimento legami
011900091111     d tibs10ds      e ds                  inz
012000091111     d  skfigli               21   5520  0 dim(500)
012100080206
012200080206      // - Reperimento dati utente
012300080206     d TIBS34ds      e ds
012400100209
012500100209      // - controllo abilitazioni
012600100209     d TNTAA1DS      e ds                  inz
012700091124
012800091124      // - Gestione anagrafica clienti
012900091124     d tnta60ds      e ds                  inz
013000090807
013100091111      // File clienti
013200091111     d cnaco00f      e ds                  extname(CNACO00F)
013300091111     d cnind00f      e ds                  extname(CNIND00F)
013400091111     d cnclp00f      e ds                  extname(CNCLP00F)
013500090629
013600080206      //---------------------------------------------------------------
013700080206      //?Definizione variabili globali.
013800080206      //---------------------------------------------------------------
013900080206
014000080206      // - Flags booleani
014100080208     d $Fine           s               n   inz(*off)
014200080208     d $InzS01         s               n   inz(*on)
014300080207     d $EoF            s               n   inz(*off)
014400100205     d $ErrGrave       s               n   inz(*off)
014500110506     d $GiaChiamato    s               n   inz(*off)
014600080206
014700080207      // - Campi associati al video
014800090806     d $Video          s              2    inz('S1')
014900091111     d VS1nrr          s              4  0 inz
015000090601     d dsp_mod         s             10I 0
015100090806
015200090806       // - Stringa SQL da eseguire
015300090806     d wSQL            s           2048    Varying        inz
015400091111
015500091111      // - Campi di comodo
015600091111     d wpadre          s                   like(acoksc)
015700080206
015800090401      // - Campi di comodo data
015900090401     d  data_dmy       s               d   Datfmt(*dmy)
016000110506
016100110506      // - Campi per procedura esterna
016200110506     d  pStack         s           1000
016300110506     d  pStartPoint    s              3  0
016400110506     d  pEntries       s              3  0
016500090508
016600090508      //---------------------------------------------------------------
016700090807      //?Definizione procedure esterne.
016800090508      //---------------------------------------------------------------
016900090807
017000091111       // - Reperimento legami
017100091111      /copy gaitrasrc/srcprotopr,tibs10r
017200091111
017300100209       // - Controlla abilitazioni utente
017400100209      /copy gaitrasrc/srcprotopr,tntaa1c
017500100209
017600091124       // - Interrogazione anagrafica clienti
017700091124      /copy gaitrasrc/srcprotopr,tnta60r
017800091124
017900100209       // - Reperimento dati utente
018000080626      /copy gaitrasrc/srcprotopr,tibs34r
018100110506
018200110506       // - Pgm per ricerca Programmi chiamati
018300110506     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
018400110506     d  pStack                     1000
018500110506     d  pStartPoint                   3  0
018600110506     d  pEntries                      3  0
018700090807
018800080206      //---------------------------------------------------------------
018900080206      //?Definizione key-list.
019000080206      //---------------------------------------------------------------
019100090807
019200080206
019300080206      //---------------------------------------------------------------
019400080206      //?Riepilogo indicatori.
019500080206      //---------------------------------------------------------------
019600090807
019700080207      // 28    : Emissione messaggio di errore a video
019800090807      // 30    : Condiziona SFLDSP
019900090807      // 31    : Condiziona SFLDSPCTL
020000090807      // 32    : Condiziona SFLNXTCHG
020100080207      // 33    : Condiziona SFLEND
020200090518      // 50    : Errore: Posizione cursore
020300080207      // 99    : Generico di Errore
020400090807
020500080206      //---------------------------------------------------------------
020600080206
020700080206      //---------------------------------------------------------------
020800080206      //?M A I N - L I N E
020900080206      //---------------------------------------------------------------
021000080206
021100080206     c     *Entry        plist
021200080206     c                   parm                    KPJBA
021300091111     c                   parm                    TNTA69ds
021400080206
021500080206      /free
021600080206
021700090807       //?Operazioni iniziali
021800080206       exsr RoutInz;
021900080206
022000090807       //?Gestione video
022100080206       DOW $Fine = *off;
022200090807
022300080206         select;
022400080206           when $Video = 'S1';
022500080206             exsr GesS01;
022600090428           other;
022700080206             $Fine = *on;
022800080206         endsl;
022900090807
023000080206       ENDDO;
023100080206
023200090807       //?Operazioni finali
023300080206       exsr RoutEnd;
023400080206
023500080206       //--------------------------------------------------------------
023600080206       //?Operazioni iniziali.
023700080206       //--------------------------------------------------------------
023800080206       BEGSR RoutInz;
023900080206
024000091111         $video = 'S1';
024100091111         $inzS01 = *on;
024200090807
024300090807         //?Reperimento dati job
024400090807         exsr DatiJob;
024500100209
024600100209         //?Controllo se utente abilitato alla funzione
024700100209         reset TNTAA1DS;
024800100209         ITAA1caut = '§utecli';
024900100209         kpjbu     = TNTAA1DS;
025000100209         tntaa1c (kpjba);
025100100209         TNTAA1DS = kpjbu;
025200100209
025300100209         IF  OTAA1fabi = 'N';
025400100209           $ErrGrave   = *on;
025500100209           ErrMessage  = *on;
025600100209           ErrGenerico = *on;
025700100209           VC1msg      = $Msg(02);
025800100209           leavesr;
025900100209         ENDIF;
026000090807
026100090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
026200090806
026300090807         //?Impostazione campi "fissi"
026400080208         T01pgm = SDSpgm;
026500110506
026600110506         //?Verifico se anagrafica clienti già richiamata
026700110506         //?in questo caso non abilito le opzioni nel subfile
026800110506         //?per evitare la chiamata ricorsiva
026900110506         $GiaChiamato = *off;
027000110506         XRTVSTKR (pStack:pStartPoint:pEntries);
027100110506         dsStack = pStack;
027200110506         IF  %lookup('TNTA60R   ':skStack) > 0;
027300110506           $giachiamato = *on;
027400110506         ENDIF;
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
030600080207
030700090807         //?Posizionamento cursore
030800091111         if  VC1csr  > *zero;
030900091111           VC1rcd = VC1csr;
031000080207         else;
031100090807         //?Se non è stato caricato nulla
031200091111           VC1rcd = 1;
031300080207         endif;
031400091124
031500091124         //?Imposto opzioni di scelta
031600091124         SELECT;
031700110506           WHEN $GiaChiamato;
031800110506             VC1opz = c_opz;
031900091124           WHEN ITA69fle = '1';
032000091124             VC1opz = c_opz1;
032100110906           WHEN ITA69fle = '2' and DUTpou <> 046;
032200091124             VC1opz = c_opz2;
032300110906           WHEN ITA69fle = '2' and DUTpou = 046;
032400110906             VC1opz = c_opz5;
032500091124           WHEN ITA69fle = '5';
032600091124             VC1opz = c_opz5;
032700091124         ENDSL;
032800080207
032900090807         //?Emissione Testata e Piede con tasti funzionali abilitati
033000091111           write  TA69T01;
033100091111           write  TA69P01;
033200080207
033300090807         //?Emissione videata
033400091111         exfmt  TA69C01;
033500080207         reset errMessage;
033600080207         reset errGenerico;
033700091111         clear VC1msg;
033800080207
033900080207         SELECT;
034000100205
034100100205         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
034200100205           WHEN  $ErrGrave;
034300100205             $Fine  = *on;
034400091111
034500091111         //?- F12=Ritorno
034600091111           WHEN  dsp_aid = c_F12;
034700091111             exsr F12S01;
034800080207
034900090807         //?Invio
035000080207           OTHER;
035100090807         //?verifico se esistono campi di parzializzazione
035200090527             if  errGenerico = *on;
035300090527               leavesr;
035400090527             endif;
035500080207             exsr OpzS01;
035600080207             if  errGenerico = *on;
035700080207               leavesr;
035800080207             endif;
035900080207
036000080207         ENDSL;
036100080207
036200080207       ENDSR;
036300080207
036400080207       //--------------------------------------------------------------
036500080207       //?Inizializzazione videata S01
036600080207       //--------------------------------------------------------------
036700080207       BEGSR InzS01;
036800080207
036900090807         //?Pulizia subfile
037000090806         exsr pulsf01;
037100091111
037200091111         //?Potenziale
037300091111         VC1cpo1 = %dec(%subst(%editc(ITA69cpo:'X'):1:3):3:0);
037400091111         VC1cpo2 = %dec(%subst(%editc(ITA69cpo:'X'):4:8):8:0);
037500091111
037600091111         //?Descrizione potenziale
037700130904         chain ITA69cpo TNCPO000;
037800091111         IF  %found (TNCPO01L);
037900091111              VC1dcpo = cporag;
038000091111         ENDIF;
038100091111
038200091111         exec sql
038300091111           declare CPO cursor for
038400091111           select acoksc, acorag, acoabl, indcit, indprv, indsta,
038500091111           clpdus, clpage
038600091111           from cnaco00f, cnind00f, cnclp00f
038700091111           where acokut = 1 and acokcc = 151 and acolib = :ita69cpo
038800091111           and acokut = indkut and acokcc = indkcc and acoksc = indksc
038900091111           and acokut = clpkut and acokcc = clpkcc and acoksc = clpksc
039000091111           order by acoksc, acorag;
039100090323
039200090806         //?Apertura del cursore
039300090806         exec sql
039400091111           open CPO;
039500090806
039600090806         IF sqlcode < 0;
039700090806           $Eof = *on;
039800090806         ENDIF;
039900090806
040000090806         dow  not $EoF;
040100090806           exec sql
040200091111             fetch next from CPO into :acoksc, :acorag, :acoabl,
040300091111                                      :indcit, :indprv, :indsta,
040400091111                                      :clpdus, :clpage;
040500090806
040600090806             IF sqlcod = 100 or sqlcod < 0;
040700090806               $EoF = *on;
040800090806               leave;
040900090806             ENDIF;
041000090806
041100090806             exsr ries01;
041200090806
041300080207         enddo;
041400080207
041500090806         exec sql
041600091111           close CPO;
041700090806
041800090807         //?Visualizzazione del SFL (se ci sono dati)
041900091111         SflDsp_N = (VS1nrr <= *zeros);
042000090806
042100090807         //?Attivazione (eventuale) del SFLEND
042200090806         SflEnd  =  *on;
042300090625
042400090807         //?Impaginazione subfile
042500090807         //?-> forza l'impaginazione sul 1° rec. del subfile
042600091111         if VS1nrr > *zero;
042700091111           VC1rcd  = 1;
042800091111           VC1csr  = 1;
042900080207         else;
043000091111           clear VC1rcd;
043100080207         endif;
043200090508
043300080207       ENDSR;
043400080207
043500090508       //--------------------------------------------------------------
043600090508       //?Pulizia SFL01
043700090508       //--------------------------------------------------------------
043800090806       BEGSR Pulsf01;
043900090508
044000090807         //?Pulizia subfile
044100090508         SflDsp_N    = *on;
044200090508         SflDspCtl_N = *on;
044300091111         write  TA69C01;
044400090508         SflDspCtl_N = *off;
044500090508         SflEnd      = *off;
044600090508
044700091111         clear VC1rcd;
044800091111         clear VC1csr;
044900091111         clear VS1nrr;
045000091111         clear VC1msg;
045100091111
045200090508         errMessage  = *off;
045300090508         errGenerico = *off;
045400091111
045500090508         WindDspF    = IndDspF;
045600090508         reset WindDspFb;
045700090508         IndDspF     = WindDspF;
045800090508
045900090508       ENDSR ;
046000080207
046100080207       //--------------------------------------------------------------
046200080207       //?Caricamento singolo record nel SubFile S01
046300080207       //--------------------------------------------------------------
046400080208       BEGSR RieS01;
046500091111
046600091111         clear  TA69S01;
046700091111
046800091111         VS1ksc = acoksc;
046900091111         VS1rag = acorag;
047000091111         VS1abl = acoabl;
047100130321
047200130321         //?Visualizzo sempre blocco "8" anche se sul file c'è
047300130321         //?blocco "7" (blocco automatico)
047400130321         IF  VS1abl = '7';
047500130321           VS1abl = '8';
047600130321         ENDIF;
047700091111
047800091111         //?Ricerco il codice padre del cliente
047900091124         clear VS1uni;
048000091111         clear tibs10ds;
048100091111         clear wpadre;
048200091111         d10drf = *date;
048300091111         d10tle = 'ST';
048400091111         d10paf = 'P';
048500091111         d10cod = acoksc;
048600091111         tibs10r(tibs10ds);
048700091111
048800091111         //?Trovo il padre
048900091111         IF  d10err = *blanks and d10cop > 0;
049000091111           wpadre = %dec(%subst(%editc(d10cop:'X'):5:7):7:0);
049100091124         ELSE;
049200091124           wpadre = acoksc;
049300091124         ENDIF;
049400091111         //?controllo se uguale
049500091124         IF  wpadre = acoksc;
049600091124           VS1uni = 'U';
049700091124         ENDIF;
049800091111
049900091111         //?Data ultima spedizione fattura
050000091124         wgg = %dec(%subst(%editc(clpdus:'X'):5:2):2:0);
050100091124         wmm = %dec(%subst(%editc(clpdus:'X'):3:2):2:0);
050200091124         waa = %dec(%subst(%editc(clpdus:'X'):1:2):2:0);
050300091124         vs1dus = wdata;
050400090323
050500090807         //?decodifica del commerciale
050600130904         chain  (CLPage)  AZCMM000;
050700130904         if   %found(AZCMM01L);
050800130904           vs1age = CMMdes;
050900090807         endif;
051000091124
051100091124         //?Località e provincia
051200091124         vs1cit = indcit;
051300091124         vs1prv = indprv;
051400090629
051500091124         VS1nrr += 1;
051600091111         write TA69S01;
051700090601
051800080207       ENDSR;
051900091111
052000091111       //--------------------------------------------------------------
052100091111       //?Gestione tasto funzionale F12 da videata S01
052200091111       //?F12=Ritorno
052300091111       //--------------------------------------------------------------
052400091111       BEGSR F12S01;
052500091111
052600091111         //?Chiusura del programma
052700091111         $Fine = *on;
052800091111
052900091111       ENDSR;
053000090807
053100080207       //--------------------------------------------------------------
053200080207       //?Gestione opzioni subfile
053300080207       //--------------------------------------------------------------
053400080207       BEGSR OpzS01;
053500080207
053600091111         readc TA69S01;
053700080207
053800091111         DOW  NOT  %eof(TNTA69D);
053900080207
054000080207           SflNxtChg = *off;
054100080207           WindDspF  = IndDspF;
054200080207           reset WindDspFb;
054300080207           IndDspF   = WindDspF;
054400091124           VC1rcd    = VS1nrr;
054500080207
054600080207           SELECT;
054700080207
054800090807             //?- Nessuna opzione
054900091124             WHEN  VS1opz  = *blank;
055000090514
055100091124             //?- 1 = Scelta
055200091125             WHEN  VS1opz  = '1' and ITA69fle = '1';
055300091124               OTA69ksc = VS1ksc;
055400091124               exsr F12S01;
055500110906
055600110906             //?- 2 = Modifica  non possibile per SEDE
055700110906             WHEN  ITA69fle = '2' and DUTpou <> 046 and
055800110906                  (VS1opz  = '2' or VS1opz = '5');
055900110906               exsr sr_Anagra;
056000091124
056100110906             //?- 5 = Visualizza
056200110920             WHEN  VS1opz = '5' and  DUTpou = 046 and
056300110920                   not $GiaChiamato;
056400091124               exsr sr_Anagra;
056500091124
056600091124             //?- 5 = Visualizza
056700110506             //WHEN  VS1opz  = '5' and ITA69fle = '5';
056800110506             WHEN  VS1opz  = '5' and ITA69fle = '5' and
056900110506                   not $GiaChiamato;
057000091124               exsr sr_Anagra;
057100090514
057200090807             //?- ? = Opzione NON valida
057300080207             OTHER;
057400080207               errMessage  = *on;
057500080207               errGenerico = *on;
057600080207               PosCurOpz   = *on;
057700091124               VC1msg = $Msg(01);
057800080207
057900080207           ENDSL;
058000080207
058100090807           //?Aggiornamento sfl
058200080207           select;
058300080207             when  errMessage  = *on;
058400080207               SflNxtChg = *on;
058500091124               VC1csr    = VC1rcd;
058600080207             when  errGenerico = *on;
058700091124               VC1csr    = VC1rcd;
058800091124               clear  VS1opz;
058900080207             other;
059000091124               VC1csr    = VC1rcd;
059100091124               clear  VS1opz;
059200080207           endsl;
059300080207
059400091111           update TA69S01;
059500080207
059600080208           if  errMessage = *on  or  errGenerico = *on;
059700080207             leave;
059800080207           endif;
059900080207
060000091111           readc TA69S01;
060100080207
060200080207         ENDDO;
060300080207
060400080207       ENDSR;
060500091124
060600091124       //--------------------------------------------------------------
060700091124       //?Richiamo anagrafica cliente.
060800091124       //--------------------------------------------------------------
060900091124       BEGSR sr_Anagra;
061000091124
061100091124         clear Tnta60ds ;
061200091124
061300091124           //?Modifica
061400091124         IF  VS1opz = '2';
061500100205           //?Se utente non abilitato vado in visualizzazione
061600100209           clear TNTAA1DS;
061700100209           ITAA1caut = '§utecli';
061800100209           ITAA1ksc  = VS1ksc;
061900100209           kpjbu     = tntaa1ds;
062000100209           tntaa1c (kpjba);
062100100209           TNTAA1DS = kpjbu;
062200100209           IF  OTAA1fabi = 'N';
062300100209             ta60flg = 'I';
062400100209             ta60nrv = VS1ksc ;
062500100209             ta60int = 'S';
062600100209           ELSE;
062700100205             ta60flg = 'M';
062800100205             ta60cli = VS1ksc ;
062900100205           ENDIF;
063000091124         ENDIF;
063100091124
063200091124           //?Interrogazione imposto il codice cliente nel campo
063300091124           //?visita perchè il tnta60r è impostato in modo da ricevere
063400091124           //?il codice da interrogare nel campo visita
063500091124         IF  VS1opz = '5';
063600091124           ta60flg = 'I';
063700091124           ta60nrv = VS1ksc ;
063800091124           ta60int = 'S';
063900091124         ENDIF;
064000091124
064100110329         //?blocco l'emissione del tasto F20-Potenziali da TA60
064200110329         //?siamo già in gestione/interrogazione potenziale
064300110329         TA60pot = '1';
064400110329
064500091124         tnta60r (kpjba:tnta60ds) ;
064600091124
064700091124         IF  ta60err <> *blanks;
064800091124           errMessage  = *on;
064900091124           errGenerico = *on;
065000091124           PosCurOpz   = *on;
065100091124           VC1msg = ta60msg;
065200091124         ENDIF;
065300091124
065400091124       ENDSR;
065500090807
065600080206       //--------------------------------------------------------------
065700080206       //?Operazioni finali.
065800080206       //--------------------------------------------------------------
065900080206       BEGSR RoutEnd;
066000090521
066100080206         *inLR = *on;
066200080206         return;
066300080206
066400080206       ENDSR;
066500080206
066600080206      /end-free
066700090807
066800080206       //--------------------------------------------------------------
066900080206       //?Schiere a tempo di compilazione.
067000080206       //--------------------------------------------------------------
067100080206
067200080206** - $MSG -------------------------------------------------------------------*
067300100205Opzione errata                                                                 1
067400100205Utente non abilitato alla funzione                                             2
