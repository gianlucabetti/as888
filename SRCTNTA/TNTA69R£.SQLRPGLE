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
001100091111     ftabel00f  if   e           k disk
001200091111     ftncpo01l  if   e           k disk
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
013000090402
013100090318      // - Tabella 01 = Codici Commerciali
013200090318     d ds01          e ds                  inz
013300090807
013400091111      // File clienti
013500091111     d cnaco00f      e ds                  extname(CNACO00F)
013600091111     d cnind00f      e ds                  extname(CNIND00F)
013700091111     d cnclp00f      e ds                  extname(CNCLP00F)
013800090629
013900080206      //---------------------------------------------------------------
014000080206      //?Definizione variabili globali.
014100080206      //---------------------------------------------------------------
014200080206
014300080206      // - Flags booleani
014400080208     d $Fine           s               n   inz(*off)
014500080208     d $InzS01         s               n   inz(*on)
014600080207     d $EoF            s               n   inz(*off)
014700100205     d $ErrGrave       s               n   inz(*off)
014800110506     d $GiaChiamato    s               n   inz(*off)
014900080206
015000080207      // - Campi associati al video
015100090806     d $Video          s              2    inz('S1')
015200091111     d VS1nrr          s              4  0 inz
015300090601     d dsp_mod         s             10I 0
015400090806
015500090806       // - Stringa SQL da eseguire
015600090806     d wSQL            s           2048    Varying        inz
015700091111
015800091111      // - Campi di comodo
015900091111     d wpadre          s                   like(acoksc)
016000080206
016100090401      // - Campi di comodo data
016200090401     d  data_dmy       s               d   Datfmt(*dmy)
016300110506
016400110506      // - Campi per procedura esterna
016500110506     d  pStack         s           1000
016600110506     d  pStartPoint    s              3  0
016700110506     d  pEntries       s              3  0
016800090508
016900090508      //---------------------------------------------------------------
017000090807      //?Definizione procedure esterne.
017100090508      //---------------------------------------------------------------
017200090807
017300091111       // - Reperimento legami
017400091111      /copy gaitrasrc/srcprotopr,tibs10r
017500091111
017600100209       // - Controlla abilitazioni utente
017700100209      /copy gaitrasrc/srcprotopr,tntaa1c
017800100209
017900091124       // - Interrogazione anagrafica clienti
018000091124      /copy gaitrasrc/srcprotopr,tnta60r
018100091124
018200100209       // - Reperimento dati utente
018300080626      /copy gaitrasrc/srcprotopr,tibs34r
018400110506
018500110506       // - Pgm per ricerca Programmi chiamati
018600110506     d XRTVSTKR        pr                  extpgm('XRTVSTKR')
018700110506     d  pStack                     1000
018800110506     d  pStartPoint                   3  0
018900110506     d  pEntries                      3  0
019000090807
019100080206      //---------------------------------------------------------------
019200080206      //?Definizione key-list.
019300080206      //---------------------------------------------------------------
019400090807
019500090807       // - File TABEL00F
019600090807     d k03tabel      e ds                  extname(TABEL00F:*key)
019700090807     d                                     prefix(k_)
019800090807     d                                     inz
019900080206
020000080206      //---------------------------------------------------------------
020100080206      //?Riepilogo indicatori.
020200080206      //---------------------------------------------------------------
020300090807
020400080207      // 28    : Emissione messaggio di errore a video
020500090807      // 30    : Condiziona SFLDSP
020600090807      // 31    : Condiziona SFLDSPCTL
020700090807      // 32    : Condiziona SFLNXTCHG
020800080207      // 33    : Condiziona SFLEND
020900090518      // 50    : Errore: Posizione cursore
021000080207      // 99    : Generico di Errore
021100090807
021200080206      //---------------------------------------------------------------
021300080206
021400080206      //---------------------------------------------------------------
021500080206      //?M A I N - L I N E
021600080206      //---------------------------------------------------------------
021700080206
021800080206     c     *Entry        plist
021900080206     c                   parm                    KPJBA
022000091111     c                   parm                    TNTA69ds
022100080206
022200080206      /free
022300080206
022400090807       //?Operazioni iniziali
022500080206       exsr RoutInz;
022600080206
022700090807       //?Gestione video
022800080206       DOW $Fine = *off;
022900090807
023000080206         select;
023100080206           when $Video = 'S1';
023200080206             exsr GesS01;
023300090428           other;
023400080206             $Fine = *on;
023500080206         endsl;
023600090807
023700080206       ENDDO;
023800080206
023900090807       //?Operazioni finali
024000080206       exsr RoutEnd;
024100080206
024200080206       //--------------------------------------------------------------
024300080206       //?Operazioni iniziali.
024400080206       //--------------------------------------------------------------
024500080206       BEGSR RoutInz;
024600080206
024700091111         $video = 'S1';
024800091111         $inzS01 = *on;
024900090807
025000090807         //?Reperimento dati job
025100090807         exsr DatiJob;
025200100209
025300100209         //?Controllo se utente abilitato alla funzione
025400100209         reset TNTAA1DS;
025500100209         ITAA1caut = '§utecli';
025600100209         kpjbu     = TNTAA1DS;
025700100209         tntaa1c (kpjba);
025800100209         TNTAA1DS = kpjbu;
025900100209
026000100209         IF  OTAA1fabi = 'N';
026100100209           $ErrGrave   = *on;
026200100209           ErrMessage  = *on;
026300100209           ErrGenerico = *on;
026400100209           VC1msg      = $Msg(02);
026500100209           leavesr;
026600100209         ENDIF;
026700090807
026800090807         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
026900090806
027000090807         //?Impostazione campi "fissi"
027100080208         T01pgm = SDSpgm;
027200080208         TBLkut = 1;
027300110506
027400110506         //?Verifico se anagrafica clienti già richiamata
027500110506         //?in questo caso non abilito le opzioni nel subfile
027600110506         //?per evitare la chiamata ricorsiva
027700110506         $GiaChiamato = *off;
027800110506         XRTVSTKR (pStack:pStartPoint:pEntries);
027900110506         dsStack = pStack;
028000110506         IF  %lookup('TNTA60R   ':skStack) > 0;
028100110506           $giachiamato = *on;
028200110506         ENDIF;
028300090714
028400080206       ENDSR;
028500080206
028600080206       //--------------------------------------------------------------
028700080206       //?Reperimento Dati del job (Utente/Operativi).
028800080206       //--------------------------------------------------------------
028900080206       BEGSR DatiJob;
029000080206
029100080206         in(E) §AzUte;
029200080206         if NOT %error;
029300080206           in(E) §DatiUte;
029400080206         endif;
029500080206         if %error or RSut = *blanks;
029600080206           clear TIBS34ds;
029700080206           tibs34r(tibs34ds);
029800080206           in §AzUte;
029900080206           in §DatiUte;
030000080206         endif;
030100080206
030200080206       ENDSR;
030300080206
030400090421       //--------------------------------------------------------------
030500080206       //?Gestione videata S01
030600080206       //--------------------------------------------------------------
030700080206       BEGSR GesS01;
030800080207
030900090807         //?Inizializzazione videata
031000080207         if  $InzS01 = *on;
031100080207            exsr InzS01;
031200080207            $InzS01  = *off;
031300080207         endif;
031400080207
031500090807         //?Posizionamento cursore
031600091111         if  VC1csr  > *zero;
031700091111           VC1rcd = VC1csr;
031800080207         else;
031900090807         //?Se non è stato caricato nulla
032000091111           VC1rcd = 1;
032100080207         endif;
032200091124
032300091124         //?Imposto opzioni di scelta
032400091124         SELECT;
032500110506           WHEN $GiaChiamato;
032600110506             VC1opz = c_opz;
032700091124           WHEN ITA69fle = '1';
032800091124             VC1opz = c_opz1;
032900110906           WHEN ITA69fle = '2' and DUTpou <> 046;
033000091124             VC1opz = c_opz2;
033100110906           WHEN ITA69fle = '2' and DUTpou = 046;
033200110906             VC1opz = c_opz5;
033300091124           WHEN ITA69fle = '5';
033400091124             VC1opz = c_opz5;
033500091124         ENDSL;
033600080207
033700090807         //?Emissione Testata e Piede con tasti funzionali abilitati
033800091111           write  TA69T01;
033900091111           write  TA69P01;
034000080207
034100090807         //?Emissione videata
034200091111         exfmt  TA69C01;
034300080207         reset errMessage;
034400080207         reset errGenerico;
034500091111         clear VC1msg;
034600080207
034700080207         SELECT;
034800100205
034900100205         //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
035000100205           WHEN  $ErrGrave;
035100100205             $Fine  = *on;
035200091111
035300091111         //?- F12=Ritorno
035400091111           WHEN  dsp_aid = c_F12;
035500091111             exsr F12S01;
035600080207
035700090807         //?Invio
035800080207           OTHER;
035900090807         //?verifico se esistono campi di parzializzazione
036000090527             if  errGenerico = *on;
036100090527               leavesr;
036200090527             endif;
036300080207             exsr OpzS01;
036400080207             if  errGenerico = *on;
036500080207               leavesr;
036600080207             endif;
036700080207
036800080207         ENDSL;
036900080207
037000080207       ENDSR;
037100080207
037200080207       //--------------------------------------------------------------
037300080207       //?Inizializzazione videata S01
037400080207       //--------------------------------------------------------------
037500080207       BEGSR InzS01;
037600080207
037700090807         //?Pulizia subfile
037800090806         exsr pulsf01;
037900091111
038000091111         //?Potenziale
038100091111         VC1cpo1 = %dec(%subst(%editc(ITA69cpo:'X'):1:3):3:0);
038200091111         VC1cpo2 = %dec(%subst(%editc(ITA69cpo:'X'):4:8):8:0);
038300091111
038400091111         //?Descrizione potenziale
038500091111         chain ITA69cpo TNCPO01L;
038600091111         IF  %found (TNCPO01L);
038700091111              VC1dcpo = cporag;
038800091111         ENDIF;
038900091111
039000091111         exec sql
039100091111           declare CPO cursor for
039200091111           select acoksc, acorag, acoabl, indcit, indprv, indsta,
039300091111           clpdus, clpage
039400091111           from cnaco00f, cnind00f, cnclp00f
039500091111           where acokut = 1 and acokcc = 151 and acolib = :ita69cpo
039600091111           and acokut = indkut and acokcc = indkcc and acoksc = indksc
039700091111           and acokut = clpkut and acokcc = clpkcc and acoksc = clpksc
039800091111           order by acoksc, acorag;
039900090323
040000090806         //?Apertura del cursore
040100090806         exec sql
040200091111           open CPO;
040300090806
040400090806         IF sqlcode < 0;
040500090806           $Eof = *on;
040600090806         ENDIF;
040700090806
040800090806         dow  not $EoF;
040900090806           exec sql
041000091111             fetch next from CPO into :acoksc, :acorag, :acoabl,
041100091111                                      :indcit, :indprv, :indsta,
041200091111                                      :clpdus, :clpage;
041300090806
041400090806             IF sqlcod = 100 or sqlcod < 0;
041500090806               $EoF = *on;
041600090806               leave;
041700090806             ENDIF;
041800090806
041900090806             exsr ries01;
042000090806
042100080207         enddo;
042200080207
042300090806         exec sql
042400091111           close CPO;
042500090806
042600090807         //?Visualizzazione del SFL (se ci sono dati)
042700091111         SflDsp_N = (VS1nrr <= *zeros);
042800090806
042900090807         //?Attivazione (eventuale) del SFLEND
043000090806         SflEnd  =  *on;
043100090625
043200090807         //?Impaginazione subfile
043300090807         //?-> forza l'impaginazione sul 1° rec. del subfile
043400091111         if VS1nrr > *zero;
043500091111           VC1rcd  = 1;
043600091111           VC1csr  = 1;
043700080207         else;
043800091111           clear VC1rcd;
043900080207         endif;
044000090508
044100080207       ENDSR;
044200080207
044300090508       //--------------------------------------------------------------
044400090508       //?Pulizia SFL01
044500090508       //--------------------------------------------------------------
044600090806       BEGSR Pulsf01;
044700090508
044800090807         //?Pulizia subfile
044900090508         SflDsp_N    = *on;
045000090508         SflDspCtl_N = *on;
045100091111         write  TA69C01;
045200090508         SflDspCtl_N = *off;
045300090508         SflEnd      = *off;
045400090508
045500091111         clear VC1rcd;
045600091111         clear VC1csr;
045700091111         clear VS1nrr;
045800091111         clear VC1msg;
045900091111
046000090508         errMessage  = *off;
046100090508         errGenerico = *off;
046200091111
046300090508         WindDspF    = IndDspF;
046400090508         reset WindDspFb;
046500090508         IndDspF     = WindDspF;
046600090508
046700090508       ENDSR ;
046800080207
046900080207       //--------------------------------------------------------------
047000080207       //?Caricamento singolo record nel SubFile S01
047100080207       //--------------------------------------------------------------
047200080208       BEGSR RieS01;
047300091111
047400091111         clear  TA69S01;
047500091111
047600091111         VS1ksc = acoksc;
047700091111         VS1rag = acorag;
047800091111         VS1abl = acoabl;
047900130321
048000130321         //?Visualizzo sempre blocco "8" anche se sul file c'è
048100130321         //?blocco "7" (blocco automatico)
048200130321         IF  VS1abl = '7';
048300130321           VS1abl = '8';
048400130321         ENDIF;
048500091111
048600091111         //?Ricerco il codice padre del cliente
048700091124         clear VS1uni;
048800091111         clear tibs10ds;
048900091111         clear wpadre;
049000091111         d10drf = *date;
049100091111         d10tle = 'ST';
049200091111         d10paf = 'P';
049300091111         d10cod = acoksc;
049400091111         tibs10r(tibs10ds);
049500091111
049600091111         //?Trovo il padre
049700091111         IF  d10err = *blanks and d10cop > 0;
049800091111           wpadre = %dec(%subst(%editc(d10cop:'X'):5:7):7:0);
049900091124         ELSE;
050000091124           wpadre = acoksc;
050100091124         ENDIF;
050200091111         //?controllo se uguale
050300091124         IF  wpadre = acoksc;
050400091124           VS1uni = 'U';
050500091124         ENDIF;
050600091111
050700091111         //?Data ultima spedizione fattura
050800091124         wgg = %dec(%subst(%editc(clpdus:'X'):5:2):2:0);
050900091124         wmm = %dec(%subst(%editc(clpdus:'X'):3:2):2:0);
051000091124         waa = %dec(%subst(%editc(clpdus:'X'):1:2):2:0);
051100091124         vs1dus = wdata;
051200090323
051300090807         //?decodifica del commerciale
051400090807         clear  ds01;
051500090807         k_tblkut = 1;
051600090807         k_tblcod = '01';
051700091124         k_tblkey = %editc(clpage:'X');
051800090807         chain  %kds(K03tabel) TABEL00F;
051900090807         if   %found(TABEL00F) or tblflg <> *blanks;
052000091124           ds01 = TBLuni;
052100090807         endif;
052200091124         vs1age = §01age;
052300091124
052400091124         //?Località e provincia
052500091124         vs1cit = indcit;
052600091124         vs1prv = indprv;
052700090629
052800091124         VS1nrr += 1;
052900091111         write TA69S01;
053000090601
053100080207       ENDSR;
053200091111
053300091111       //--------------------------------------------------------------
053400091111       //?Gestione tasto funzionale F12 da videata S01
053500091111       //?F12=Ritorno
053600091111       //--------------------------------------------------------------
053700091111       BEGSR F12S01;
053800091111
053900091111         //?Chiusura del programma
054000091111         $Fine = *on;
054100091111
054200091111       ENDSR;
054300090807
054400080207       //--------------------------------------------------------------
054500080207       //?Gestione opzioni subfile
054600080207       //--------------------------------------------------------------
054700080207       BEGSR OpzS01;
054800080207
054900091111         readc TA69S01;
055000080207
055100091111         DOW  NOT  %eof(TNTA69D);
055200080207
055300080207           SflNxtChg = *off;
055400080207           WindDspF  = IndDspF;
055500080207           reset WindDspFb;
055600080207           IndDspF   = WindDspF;
055700091124           VC1rcd    = VS1nrr;
055800080207
055900080207           SELECT;
056000080207
056100090807             //?- Nessuna opzione
056200091124             WHEN  VS1opz  = *blank;
056300090514
056400091124             //?- 1 = Scelta
056500091125             WHEN  VS1opz  = '1' and ITA69fle = '1';
056600091124               OTA69ksc = VS1ksc;
056700091124               exsr F12S01;
056800110906
056900110906             //?- 2 = Modifica  non possibile per SEDE
057000110906             WHEN  ITA69fle = '2' and DUTpou <> 046 and
057100110906                  (VS1opz  = '2' or VS1opz = '5');
057200110906               exsr sr_Anagra;
057300091124
057400110906             //?- 5 = Visualizza
057500110920             WHEN  VS1opz = '5' and  DUTpou = 046 and
057600110920                   not $GiaChiamato;
057700091124               exsr sr_Anagra;
057800091124
057900091124             //?- 5 = Visualizza
058000110506             //WHEN  VS1opz  = '5' and ITA69fle = '5';
058100110506             WHEN  VS1opz  = '5' and ITA69fle = '5' and
058200110506                   not $GiaChiamato;
058300091124               exsr sr_Anagra;
058400090514
058500090807             //?- ? = Opzione NON valida
058600080207             OTHER;
058700080207               errMessage  = *on;
058800080207               errGenerico = *on;
058900080207               PosCurOpz   = *on;
059000091124               VC1msg = $Msg(01);
059100080207
059200080207           ENDSL;
059300080207
059400090807           //?Aggiornamento sfl
059500080207           select;
059600080207             when  errMessage  = *on;
059700080207               SflNxtChg = *on;
059800091124               VC1csr    = VC1rcd;
059900080207             when  errGenerico = *on;
060000091124               VC1csr    = VC1rcd;
060100091124               clear  VS1opz;
060200080207             other;
060300091124               VC1csr    = VC1rcd;
060400091124               clear  VS1opz;
060500080207           endsl;
060600080207
060700091111           update TA69S01;
060800080207
060900080208           if  errMessage = *on  or  errGenerico = *on;
061000080207             leave;
061100080207           endif;
061200080207
061300091111           readc TA69S01;
061400080207
061500080207         ENDDO;
061600080207
061700080207       ENDSR;
061800091124
061900091124       //--------------------------------------------------------------
062000091124       //?Richiamo anagrafica cliente.
062100091124       //--------------------------------------------------------------
062200091124       BEGSR sr_Anagra;
062300091124
062400091124         clear Tnta60ds ;
062500091124
062600091124           //?Modifica
062700091124         IF  VS1opz = '2';
062800100205           //?Se utente non abilitato vado in visualizzazione
062900100209           clear TNTAA1DS;
063000100209           ITAA1caut = '§utecli';
063100100209           ITAA1ksc  = VS1ksc;
063200100209           kpjbu     = tntaa1ds;
063300100209           tntaa1c (kpjba);
063400100209           TNTAA1DS = kpjbu;
063500100209           IF  OTAA1fabi = 'N';
063600100209             ta60flg = 'I';
063700100209             ta60nrv = VS1ksc ;
063800100209             ta60int = 'S';
063900100209           ELSE;
064000100205             ta60flg = 'M';
064100100205             ta60cli = VS1ksc ;
064200100205           ENDIF;
064300091124         ENDIF;
064400091124
064500091124           //?Interrogazione imposto il codice cliente nel campo
064600091124           //?visita perchè il tnta60r è impostato in modo da ricevere
064700091124           //?il codice da interrogare nel campo visita
064800091124         IF  VS1opz = '5';
064900091124           ta60flg = 'I';
065000091124           ta60nrv = VS1ksc ;
065100091124           ta60int = 'S';
065200091124         ENDIF;
065300091124
065400110329         //?blocco l'emissione del tasto F20-Potenziali da TA60
065500110329         //?siamo già in gestione/interrogazione potenziale
065600110329         TA60pot = '1';
065700110329
065800091124         tnta60r (kpjba:tnta60ds) ;
065900091124
066000091124         IF  ta60err <> *blanks;
066100091124           errMessage  = *on;
066200091124           errGenerico = *on;
066300091124           PosCurOpz   = *on;
066400091124           VC1msg = ta60msg;
066500091124         ENDIF;
066600091124
066700091124       ENDSR;
066800090807
066900080206       //--------------------------------------------------------------
067000080206       //?Operazioni finali.
067100080206       //--------------------------------------------------------------
067200080206       BEGSR RoutEnd;
067300090521
067400080206         *inLR = *on;
067500080206         return;
067600080206
067700080206       ENDSR;
067800080206
067900080206      /end-free
068000090807
068100080206       //--------------------------------------------------------------
068200080206       //?Schiere a tempo di compilazione.
068300080206       //--------------------------------------------------------------
068400080206
068500080206** - $MSG -------------------------------------------------------------------*
068600100205Opzione errata                                                                 1
068700100205Utente non abilitato alla funzione                                             2
