000100091029      //---------------------------------------------------------------
000200091106      //?FNLV44R - Richiesta e-mail
000300091029      //---------------------------------------------------------------
000400091029
000500091029     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600091029     h dftactgrp(*no) actgrp(*caller)
000700091029
000800091029      //---------------------------------------------------------------
000900091029      //?Dichiarazione file.
001000091029      //---------------------------------------------------------------
001100100506
001200130731      // -?Anagrafica Commerciali?
001300130731     fAZCMM01L  if   e           k disk
001400100506
001500091106     ffnlv44d   cf   e             workstn
001600091029     f                                     infds(InfDspF)
001700091106     f                                     indds(IndDspF)
001800091029
001900091029      //---------------------------------------------------------------
002000091029      //?Definizione costanti.
002100091029      //---------------------------------------------------------------
002200091029
002300091029      // - Tasti funzionali a video
002400091029     d c_F01           c                   const(x'31')
002500091029     d c_F02           c                   const(x'32')
002600091029     d c_F03           c                   const(x'33')
002700091029     d c_F04           c                   const(x'34')
002800091029     d c_F05           c                   const(x'35')
002900091029     d c_F06           c                   const(x'36')
003000091029     d c_F07           c                   const(x'37')
003100091029     d c_F08           c                   const(x'38')
003200091029     d c_F09           c                   const(x'39')
003300091029     d c_F10           c                   const(x'3A')
003400091029     d c_F11           c                   const(x'3B')
003500091029     d c_F12           c                   const(x'3C')
003600091029     d c_F13           c                   const(x'B1')
003700091029     d c_F14           c                   const(x'B2')
003800091029     d c_F15           c                   const(x'B3')
003900091029     d c_F16           c                   const(x'B4')
004000091029     d c_F17           c                   const(x'B5')
004100091029     d c_F18           c                   const(x'B6')
004200091029     d c_F19           c                   const(x'B7')
004300091029     d c_F20           c                   const(x'B8')
004400091029     d c_F21           c                   const(x'B9')
004500091029     d c_F22           c                   const(x'BA')
004600091029     d c_F23           c                   const(x'BB')
004700091029     d c_F24           c                   const(x'BC')
004800091029     d c_Enter         c                   const(x'F1')
004900091029     d c_RollDown      c                   const(x'F4')
005000091029     d c_RollUp        c                   const(x'F5')
005100091029
005200091029     d Digits          c                   const('0123456789')
005300100506
005400100506     d $Msg            c                   const('Commerciale errato o non in -
005500100506     d                                     gestione all''utente')
005600091029
005700091029      //---------------------------------------------------------------
005800091029      //?Definizione schiere.
005900091029      //---------------------------------------------------------------
006000091029
006100091029      //---------------------------------------------------------------
006200091029      //?Definizione aree dati.
006300091029      //---------------------------------------------------------------
006400100506
006500100506      // - Dati utente
006600100506     d §AzUte        e ds                  extname(AZUTE00F)
006700100506     d                                     dtaara
006800100506     d §DatiUte      e ds                  extname(dDatiUte)
006900100506     d                                     dtaara
007000091029
007100091029      //---------------------------------------------------------------
007200091029      //?Definizione strutture dati.
007300091029      //---------------------------------------------------------------
007400091029
007500091029      // - InfDS
007600091029     d InfDspF         ds
007700091029     d  dsp_aid              369    369a
007800091106
007900091106      // - Indicatori su DspF
008000091106     d IndDspF         ds
008100100507        // - Abilitazione tasti funzionali
008200100507     d  F12Attivo                     1n   overlay(IndDspF : 12)
008300100511        // - Indicatore di protezione campi
008400100511     d  ProteggiCMM                    n   overlay(IndDspF : 25)
008500091106        // - Indicatori di errore subfile
008600091106     d  PosCurMAIL                    1n   overlay(IndDspF : 50)
008700100506     d  PosCurCMM                     1n   overlay(IndDspF : 51)
008800091106        // - Indicatori di errore generico
008900091106     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009000091106     d  ErrMessage                    1n   overlay(IndDspF : 28)
009100100507        // - Indicatori per l'utilizzo del file in formato *DS4
009200100507     d  FmtDspF_DS4                    n   overlay(IndDspF : 04)
009300091106
009400091106     d WindDspF        ds                  inz
009500091106     d   WindDspFa             1     49    inz(*zero)
009600091106     d   WindDspFb            50     99    inz(*zero)
009700091029
009800091029      // - Parametri ricevuti
009900091029     d KPJBA         e ds
010000091106     d FNLV44ds      e ds
010100100506
010200100506      // - Reperimento dati utente
010300100506     d TIBS34ds      e ds
010400100506     d dUte01        e ds
010500100506
010600100506      // - controllo abilitazioni
010700100506     d TNTAA1DS      e ds                  inz
010800091109
010900091109      // - Parametri per controllo e-mail
011000091109     d dsEmail       e ds
011100091029
011200091029      //---------------------------------------------------------------
011300091029      //?Definizione variabili globali.
011400091029      //---------------------------------------------------------------
011500091029
011600091029      // - Flags booleani
011700091029     d $Fine           s               n   inz(*off)
011800091029     d $InzW01         s               n   inz(*on)
011900091029
012000091029      // - Campi associati al video
012100091029     d $Video          s              2    inz('W1')
012200091029
012300091029      //---------------------------------------------------------------
012400091029      //?Definizione procedure esterne.
012500091029      //---------------------------------------------------------------
012600100506      /copy gaitrasrc/srcprotopr,tibs34r
012700100506      /copy gaitrasrc/srcprotopr,tntaa1c
012800130731      /copy gaitrasrc/srcprotoPI,TRMK43R_1
012900130731      /copy gaitrasrc/srcprotoPR,TRMK43R
013000100506      /COPY GAITRASRC/SRCPROTOPR,XEMAIL
013100091029
013200091029      //---------------------------------------------------------------
013300091029      //?Definizione key-list.
013400091029      //---------------------------------------------------------------
013500100506
013600130731       // - File AZCMM01L
013700130731     d k_azcmm01     e ds                  extname(AZCMM01L : *key)
013800100506     d                                     prefix(k_)
013900091029
014000091029      //---------------------------------------------------------------
014100091029      //?Riepilogo indicatori.
014200091029      //---------------------------------------------------------------
014300091106
014400100507      // 12    : Abilita il tasto funzionale F12
014500091106      // 28    : Emissione messaggio di errore a video
014600091106      // 50    : Errore: Posizione cursore
014700100506      // 51    : Errore: Posizione commerciale
014800100506      // 99    : Generico di Errore
014900091029
015000091029      //---------------------------------------------------------------
015100091029      //?M A I N - L I N E
015200091029      //---------------------------------------------------------------
015300091029
015400091029     c     *Entry        plist
015500100519     c                   parm                    kpjba
015600091106     c                   parm                    FNLV44ds
015700091029
015800091029      /free
015900091029
016000091029       //?Operazioni iniziali
016100091029       exsr sr_RoutInz;
016200091029
016300091029       //?Gestione video
016400091029       DOW  $Fine = *off;
016500091029
016600091029         SELECT;
016700091029           WHEN  $Video = 'W1';
016800091029             exsr sr_GesW01;
016900091029           OTHER;
017000091029             $Fine = *on;
017100091029         ENDSL;
017200091029
017300091029       ENDDO;
017400091029
017500091029       //?Operazioni finali
017600091029       exsr sr_RoutEnd;
017700091029
017800091029       //--------------------------------------------------------------
017900091029       //?Operazioni iniziali.
018000091029       //--------------------------------------------------------------
018100091029       BEGSR sr_RoutInz;
018200100506
018300100506         // Reperimento dati job
018400100506         exsr DatiJob;
018500100507
018600100507         //?Verifica se visualizzare la finestra a 80 o 132 colonne:
018700100507         //?  solo se ricevuto il carattere "4" nella ds TRMK22 campo
018800100507         //?  del tipo formato
018900100507         FmtDspF_DS4 = (LV44dsp = '4');
019000091029
019100091029       ENDSR;
019200100506
019300100506       //--------------------------------------------------------------
019400100506       //?Reperimento Dati del job (Utente/Operativi).
019500100506       //--------------------------------------------------------------
019600100506       BEGSR DatiJob;
019700100506
019800100506         in(E) §AzUte;
019900100506         if NOT %error;
020000100506           in(E) §DatiUte;
020100100506         endif;
020200100506         if %error or RSut = *blanks;
020300100506           clear TIBS34ds;
020400100506           tibs34r(tibs34ds);
020500100506           in §AzUte;
020600100506           in §DatiUte;
020700100506         endif;
020800100506
020900100506       ENDSR;
021000091029
021100091029       //--------------------------------------------------------------
021200091029       //?Gestione videata W01.
021300091029       //--------------------------------------------------------------
021400091029       BEGSR sr_GesW01;
021500091029
021600091029         //?Inizializzazione videata
021700091029         IF  $InzW01 = *on;
021800091029           exsr sr_InzW01;
021900091029           $InzW01 = *off;
022000091029         ENDIF;
022100091029
022200091029         //?Emissione videata
022300091106         exfmt LV44W01;
022400091106         ErrMessage = *off;
022500091106         ErrGenerico = *off;
022600091106         clear w01dmsg;
022700091106
022800091106         SELECT;
022900091029
023000091029           //?F12=Ritorno
023100091106           WHEN  dsp_aid = c_F12;
023200091106             exsr sr_F12W01;
023300091106
023400091106           //?F06=Conferma
023500091106           WHEN  dsp_aid = c_F06;
023600091106             exsr sr_F06W01;
023700091106             IF  ErrGenerico;
023800091106               leavesr;
023900091106             ENDIF;
024000091106
024100091106         //?Invio
024200091106           OTHER;
024300091106         //?Controllo i dati della videata
024400091106             exsr sr_CTRW01;
024500091106             IF  ErrGenerico;
024600091106               leavesr;
024700091106             endif;
024800091106
024900091106         ENDSL;
025000091029
025100091029       ENDSR;
025200091029
025300091029       //--------------------------------------------------------------
025400091029       //?Inizializzazione videata D01.
025500091029       //--------------------------------------------------------------
025600091029       BEGSR sr_InzW01;
025700091029
025800091106         clear LV44W01;
025900100511
026000100511       //?proteggo il commerciale se richiesto
026100100511         ProteggiCMM = (LV44pcmm = 'S');
026200100507
026300100507         //?se non passato decodifico commerciale
026400100507         IF  LV44dcmm = *blanks;
026500130731           k_CMMcod = LV44ccmm;
026600130731           chain  %kds(K_AZCMM01) AZCMM000;
026700130731           IF  %found(AZCMM01L);
026800130731             LV44dcmm = CMMdes;
026900100507           ENDIF;
027000100507         ENDIF;
027100100507
027200091029         //?imposto i campi
027300100506         w01ccmm = %editc(LV44ccmm:'X');
027400091106         w01dcmm = LV44dcmm;
027500091106         w01mail = LV44mail;
027600100507
027700100507         //?Abilito F12-Ritorno
027800100507         F12Attivo = (LV44F12 = *blanks);
027900091029
028000091029       ENDSR;
028100091106
028200091106       //--------------------------------------------------------------
028300091106       //?Controllo videata W01.
028400091106       //--------------------------------------------------------------
028500091106       BEGSR sr_CTRW01;
028600091106
028700091106         WindDspF = IndDspF;
028800091106         reset WindDspFb;
028900091106         IndDspF  = WindDspF;
029000100506
029100100506       //?Commerciale  -  Ricerca
029200100506         IF  %scan('?' : W01ccmm) > *zero;
029300100506           ErrGenerico = *on;
029400100506           PosCurCMM   = *on;
029500130731           clear  TRMK43ds;
029600130731           iMK43fil = DUTpou;
029700130731           TRMK43R (kpjba : TRMK43ds);
029800130731           if  oMK43err = *off  and  oMK43fxx = *blank;
029900130731             w01ccmm = %editc( oMK43cmm : 'X' );
030000130731             w01dcmm = oMK43des;
030100130731           endif;
030200100506           leavesr;
030300100506         ENDIF;
030400100506
030500100506       //?Commerciale  -  Obbligatorio
030600100506         IF  W01ccmm = *blanks  or  W01ccmm = *zero;
030700100506           ErrMessage  = *on;
030800100506           ErrGenerico = *on;
030900100506           PosCurCMM   = *on;
031000100506           W01dmsg     = $Msg;
031100100506           leavesr;
031200100506         ENDIF;
031300100506
031400100506         IF  %check(digits:W01ccmm) > 0;
031500100506           ErrMessage  = *on;
031600100506           ErrGenerico = *on;
031700100506           PosCurCMM   = *on;
031800100506           W01dmsg     = $Msg;
031900100506           leavesr;
032000100506         ENDIF;
032100100506
032200100506       //?Commerciale  -  Controllo
032300130731         k_CMMcod = %int(W01ccmm);
032400130731         chain  %kds(K_AZCMM01) AZCMM000;
032500130731         IF  NOT %found(AZCMM01L);
032600100506           ErrMessage  = *on;
032700100506           ErrGenerico = *on;
032800100506           PosCurCMM   = *on;
032900100506           W01dmsg     = $Msg;
033000100506           leavesr;
033100100506         ENDIF;
033200100506
033300130731         W01dcmm = CMMdes;
033400100506
033500100506         //?commerciale valido no vari o clienti inattivi
033600100506         IF  %subst(W01ccmm:4:4) = '0000' or
033700100506             %subst(W01ccmm:4:4) = '0999';
033800100506           ErrMessage  = *on;
033900100506           ErrGenerico = *on;
034000100506           PosCurCMM   = *on;
034100100506           W01dmsg     = $Msg;
034200100506           leavesr;
034300100506         ENDIF;
034400100506
034500100506         //?utente abilitato al commerciale
034600110926         //?solo se non è utente di sede
034700110926         IF  DUTpou <> 46;
034800100506         clear TNTAA1DS;
034900120810         ITAA1tipo = 'M';
035000120810         ITAA1caut = '§utegtc';
035100100506         ITAA1cmm  = %dec(W01ccmm:7:0);
035200100519         ITAA1abi = 'RA';
035300100506         kpjbu     = tntaa1ds;
035400100506         tntaa1c (kpjba);
035500100506         TNTAA1DS = kpjbu;
035600100506         IF  OTAA1fabi = 'N';
035700100506           ErrMessage  = *on;
035800100506           ErrGenerico = *on;
035900100506           PosCurCMM   = *on;
036000100506           W01dmsg     = $Msg;
036100100506           leavesr;
036200100506         ENDIF;
036300110926         ENDIF;
036400091106
036500091106         //?Controllo indirizzo mail
036600091106         IF  w01mail = *blanks;
036700091106           ErrMessage   = *on;
036800091106           ErrGenerico  = *on;
036900091106           PosCurMAIL   = *on;
037000091106           w01dmsg = 'Inserire indirizzo mail';
037100091106           leavesr;
037200091106         ENDIF;
037300091109
037400091109         clear dsEmail;
037500091109         §emlIndI = w01mail;
037600091109         xEmail( dsEmail );
037700091109         IF §emlErrO = *on;
037800091109           ErrMessage   = *on;
037900091109           ErrGenerico  = *on;
038000091109           PosCurMAIL   = *on;
038100091109           w01dmsg = §emlMsgO;
038200091109           leavesr;
038300091109         ENDIF;
038400091109
038500091109         w01mail = §emlIndO;
038600091106
038700091106       ENDSR;
038800091106
038900091106       //--------------------------------------------------------------
039000091106       //?Gestione tasto funzionale F06 da videata W01.
039100091106       //?F06=Conferma
039200091106       //--------------------------------------------------------------
039300091106       BEGSR sr_F06W01;
039400091106
039500091106         //?controllo i dati della videata
039600091106             exsr sr_CTRW01;
039700091106             IF  ErrGenerico;
039800091106               leavesr;
039900091106             endif;
040000091106
040100091106         //?Ritorno indirizzo e-mail immesso a video
040200091106         LV44mail = W01mail;
040300100506         //?Ritorno codice commerciale imesso a video
040400100506         LV44ccmm = %dec(W01ccmm:7:0);
040500091106
040600091106         //?Chiusura del programma
040700091106         $Fine = *on;
040800091106
040900091106       ENDSR;
041000091029
041100091029       //--------------------------------------------------------------
041200091029       //?Gestione tasto funzionale F12 da videata W01.
041300091029       //?F12=Ritorno
041400091029       //--------------------------------------------------------------
041500091029       BEGSR sr_F12W01;
041600091029
041700091029         //?Chiusura del programma
041800091109         lv44f12 = '1';
041900091029         $Fine = *on;
042000091029
042100091029       ENDSR;
042200091029
042300091029       //--------------------------------------------------------------
042400091029       //?Operazioni finali.
042500091029       //--------------------------------------------------------------
042600091029       BEGSR sr_RoutEnd;
042700091029
042800091029         *inLR = *on;
042900091029         return;
043000091029
043100091029       ENDSR;
043200091029
043300091029      /end-free
