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
001200100506      // - Tabelle
001300100506     fTABEL00F  if   e           k disk
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
011100100506
011200100506      // - Tabella 01 = Codici Commerciali
011300100506     d ds01          e ds                  inz
011400091029
011500091029      //---------------------------------------------------------------
011600091029      //?Definizione variabili globali.
011700091029      //---------------------------------------------------------------
011800091029
011900091029      // - Flags booleani
012000091029     d $End            s               n   inz(*off)
012100091029     d $Fine           s               n   inz(*off)
012200091029     d $InzW01         s               n   inz(*on)
012300091029
012400091029      // - Campi associati al video
012500091029     d $Video          s              2    inz('W1')
012600100506
012700100506     d tb85Kfil        s              3
012800100506     d tb85Key         s              8
012900100506     d tb85Des1        s             25
013000100506     d tb85Uni         s              1    inz('S')
013100091029
013200091029      //---------------------------------------------------------------
013300091029      //?Definizione procedure esterne.
013400091029      //---------------------------------------------------------------
013500100506      /copy gaitrasrc/srcprotopr,tibs34r
013600100506      /copy gaitrasrc/srcprotopr,tntaa1c
013700100506      /copy gaitrasrc/srcprotopr,trtb85r
013800100506      /COPY GAITRASRC/SRCPROTOPR,XEMAIL
013900091029
014000091029      //---------------------------------------------------------------
014100091029      //?Definizione key-list.
014200091029      //---------------------------------------------------------------
014300100506
014400100506       // - File TABEL00F
014500100506     d k03tabel      e ds                  extname(TABEL00F:*key)
014600100506     d                                     prefix(k_)
014700091029
014800091029      //---------------------------------------------------------------
014900091029      //?Riepilogo indicatori.
015000091029      //---------------------------------------------------------------
015100091106
015200100507      // 12    : Abilita il tasto funzionale F12
015300091106      // 28    : Emissione messaggio di errore a video
015400091106      // 50    : Errore: Posizione cursore
015500100506      // 51    : Errore: Posizione commerciale
015600100506      // 99    : Generico di Errore
015700091029
015800091029      //---------------------------------------------------------------
015900091029      //?M A I N - L I N E
016000091029      //---------------------------------------------------------------
016100091029
016200091029     c     *Entry        plist
016300100519     c                   parm                    kpjba
016400091106     c                   parm                    FNLV44ds
016500091029
016600091029      /free
016700091029
016800091029       //?Operazioni iniziali
016900091029       exsr sr_RoutInz;
017000091029
017100091029       //?Gestione video
017200091029       DOW  $Fine = *off;
017300091029
017400091029         SELECT;
017500091029           WHEN  $Video = 'W1';
017600091029             exsr sr_GesW01;
017700091029           OTHER;
017800091029             $Fine = *on;
017900091029         ENDSL;
018000091029
018100091029       ENDDO;
018200091029
018300091029       //?Operazioni finali
018400091029       exsr sr_RoutEnd;
018500091029
018600091029       //--------------------------------------------------------------
018700091029       //?Operazioni iniziali.
018800091029       //--------------------------------------------------------------
018900091029       BEGSR sr_RoutInz;
019000100506
019100100506         // Impostazione campi "fissi"
019200100506         k_TBLkut = 1;
019300100506         k_TBLcod = '01';
019400100506
019500100506         // Reperimento dati job
019600100506         exsr DatiJob;
019700100507
019800100507         //?Verifica se visualizzare la finestra a 80 o 132 colonne:
019900100507         //?  solo se ricevuto il carattere "4" nella ds TRMK22 campo
020000100507         //?  del tipo formato
020100100507         FmtDspF_DS4 = (LV44dsp = '4');
020200091029
020300091029       ENDSR;
020400100506
020500100506       //--------------------------------------------------------------
020600100506       //?Reperimento Dati del job (Utente/Operativi).
020700100506       //--------------------------------------------------------------
020800100506       BEGSR DatiJob;
020900100506
021000100506         in(E) §AzUte;
021100100506         if NOT %error;
021200100506           in(E) §DatiUte;
021300100506         endif;
021400100506         if %error or RSut = *blanks;
021500100506           clear TIBS34ds;
021600100506           tibs34r(tibs34ds);
021700100506           in §AzUte;
021800100506           in §DatiUte;
021900100506         endif;
022000100506
022100100506       ENDSR;
022200091029
022300091029       //--------------------------------------------------------------
022400091029       //?Gestione videata W01.
022500091029       //--------------------------------------------------------------
022600091029       BEGSR sr_GesW01;
022700091029
022800091029         //?Inizializzazione videata
022900091029         IF  $InzW01 = *on;
023000091029           exsr sr_InzW01;
023100091029           $InzW01 = *off;
023200091029         ENDIF;
023300091029
023400091029         //?Emissione videata
023500091106         exfmt LV44W01;
023600091106         ErrMessage = *off;
023700091106         ErrGenerico = *off;
023800091106         clear w01dmsg;
023900091106
024000091106         SELECT;
024100091029
024200091029           //?F12=Ritorno
024300091106           WHEN  dsp_aid = c_F12;
024400091106             exsr sr_F12W01;
024500091106
024600091106           //?F06=Conferma
024700091106           WHEN  dsp_aid = c_F06;
024800091106             exsr sr_F06W01;
024900091106             IF  ErrGenerico;
025000091106               leavesr;
025100091106             ENDIF;
025200091106
025300091106         //?Invio
025400091106           OTHER;
025500091106         //?Controllo i dati della videata
025600091106             exsr sr_CTRW01;
025700091106             IF  ErrGenerico;
025800091106               leavesr;
025900091106             endif;
026000091106
026100091106         ENDSL;
026200091029
026300091029       ENDSR;
026400091029
026500091029       //--------------------------------------------------------------
026600091029       //?Inizializzazione videata D01.
026700091029       //--------------------------------------------------------------
026800091029       BEGSR sr_InzW01;
026900091029
027000091106         clear LV44W01;
027100100511
027200100511       //?proteggo il commerciale se richiesto
027300100511         ProteggiCMM = (LV44pcmm = 'S');
027400100507
027500100507         //?se non passato decodifico commerciale
027600100507         IF  LV44dcmm = *blanks;
027700100507           clear ds01;
027800100507           k_TBLkey = %editc(LV44ccmm:'X');
027900100507           chain  %kds(K03tabel) TABEL00F;
028000100507           IF  %found(TABEL00F);
028100100507             ds01 = TBLuni;
028200100507             LV44dcmm = §01age;
028300100507           ENDIF;
028400100507         ENDIF;
028500100507
028600091029         //?imposto i campi
028700100506         w01ccmm = %editc(LV44ccmm:'X');
028800091106         w01dcmm = LV44dcmm;
028900091106         w01mail = LV44mail;
029000100507
029100100507         //?Abilito F12-Ritorno
029200100507         F12Attivo = (LV44F12 = *blanks);
029300091029
029400091029       ENDSR;
029500091106
029600091106       //--------------------------------------------------------------
029700091106       //?Controllo videata W01.
029800091106       //--------------------------------------------------------------
029900091106       BEGSR sr_CTRW01;
030000091106
030100091106         WindDspF = IndDspF;
030200091106         reset WindDspFb;
030300091106         IndDspF  = WindDspF;
030400100506
030500100506       //?Commerciale  -  Ricerca
030600100506         IF  %scan('?' : W01ccmm) > *zero;
030700100506           ErrGenerico = *on;
030800100506           PosCurCMM   = *on;
030900100506           tb85Kfil = %editc(DUTpou:'X');
031000100506           clear tb85Key ;
031100100506           clear tb85Des1;
031200100506           trtb85r (kpjba : tb85kfil : tb85key : tb85des1);
031300100506           w01ccmm  = tb85Key;
031400100506           w01dcmm = tb85Des1;
031500100506           leavesr;
031600100506         ENDIF;
031700100506
031800100506       //?Commerciale  -  Obbligatorio
031900100506         IF  W01ccmm = *blanks  or  W01ccmm = *zero;
032000100506           ErrMessage  = *on;
032100100506           ErrGenerico = *on;
032200100506           PosCurCMM   = *on;
032300100506           W01dmsg     = $Msg;
032400100506           leavesr;
032500100506         ENDIF;
032600100506
032700100506         IF  %check(digits:W01ccmm) > 0;
032800100506           ErrMessage  = *on;
032900100506           ErrGenerico = *on;
033000100506           PosCurCMM   = *on;
033100100506           W01dmsg     = $Msg;
033200100506           leavesr;
033300100506         ENDIF;
033400100506
033500100506       //?Commerciale  -  Controllo
033600100506         clear ds01;
033700100506         k_TBLkey = W01ccmm;
033800100506         chain  %kds(K03tabel) TABEL00F;
033900100506         IF  NOT %found(TABEL00F)
034000100506             or  TBLflg <> *blank;
034100100506           ErrMessage  = *on;
034200100506           ErrGenerico = *on;
034300100506           PosCurCMM   = *on;
034400100506           W01dmsg     = $Msg;
034500100506           leavesr;
034600100506         ENDIF;
034700100506
034800100506         ds01 = TBLuni;
034900100506         W01dcmm = §01age;
035000100506
035100100506         //?commerciale valido no vari o clienti inattivi
035200100506         IF  %subst(W01ccmm:4:4) = '0000' or
035300100506             %subst(W01ccmm:4:4) = '0999';
035400100506           ErrMessage  = *on;
035500100506           ErrGenerico = *on;
035600100506           PosCurCMM   = *on;
035700100506           W01dmsg     = $Msg;
035800100506           leavesr;
035900100506         ENDIF;
036000100506
036100100506         //?utente abilitato al commerciale
036200110926         //?solo se non è utente di sede
036300110926         IF  DUTpou <> 46;
036400100506         clear TNTAA1DS;
036500120810         ITAA1tipo = 'M';
036600120810         ITAA1caut = '§utegtc';
036700100506         ITAA1cmm  = %dec(W01ccmm:7:0);
036800100519         ITAA1abi = 'RA';
036900100506         kpjbu     = tntaa1ds;
037000100506         tntaa1c (kpjba);
037100100506         TNTAA1DS = kpjbu;
037200100506         IF  OTAA1fabi = 'N';
037300100506           ErrMessage  = *on;
037400100506           ErrGenerico = *on;
037500100506           PosCurCMM   = *on;
037600100506           W01dmsg     = $Msg;
037700100506           leavesr;
037800100506         ENDIF;
037900110926         ENDIF;
038000091106
038100091106         //?Controllo indirizzo mail
038200091106         IF  w01mail = *blanks;
038300091106           ErrMessage   = *on;
038400091106           ErrGenerico  = *on;
038500091106           PosCurMAIL   = *on;
038600091106           w01dmsg = 'Inserire indirizzo mail';
038700091106           leavesr;
038800091106         ENDIF;
038900091109
039000091109         clear dsEmail;
039100091109         §emlIndI = w01mail;
039200091109         xEmail( dsEmail );
039300091109         IF §emlErrO = *on;
039400091109           ErrMessage   = *on;
039500091109           ErrGenerico  = *on;
039600091109           PosCurMAIL   = *on;
039700091109           w01dmsg = §emlMsgO;
039800091109           leavesr;
039900091109         ENDIF;
040000091109
040100091109         w01mail = §emlIndO;
040200091106
040300091106       ENDSR;
040400091106
040500091106       //--------------------------------------------------------------
040600091106       //?Gestione tasto funzionale F06 da videata W01.
040700091106       //?F06=Conferma
040800091106       //--------------------------------------------------------------
040900091106       BEGSR sr_F06W01;
041000091106
041100091106         //?controllo i dati della videata
041200091106             exsr sr_CTRW01;
041300091106             IF  ErrGenerico;
041400091106               leavesr;
041500091106             endif;
041600091106
041700091106         //?Ritorno indirizzo e-mail immesso a video
041800091106         LV44mail = W01mail;
041900100506         //?Ritorno codice commerciale imesso a video
042000100506         LV44ccmm = %dec(W01ccmm:7:0);
042100091106
042200091106         //?Chiusura del programma
042300091106         $Fine = *on;
042400091106
042500091106       ENDSR;
042600091029
042700091029       //--------------------------------------------------------------
042800091029       //?Gestione tasto funzionale F12 da videata W01.
042900091029       //?F12=Ritorno
043000091029       //--------------------------------------------------------------
043100091029       BEGSR sr_F12W01;
043200091029
043300091029         //?Chiusura del programma
043400091109         lv44f12 = '1';
043500091029         $Fine = *on;
043600091029
043700091029       ENDSR;
043800091029
043900091029       //--------------------------------------------------------------
044000091029       //?Operazioni finali.
044100091029       //--------------------------------------------------------------
044200091029       BEGSR sr_RoutEnd;
044300091029
044400091029         *inLR = *on;
044500091029         return;
044600091029
044700091029       ENDSR;
044800091029
044900091029      /end-free
