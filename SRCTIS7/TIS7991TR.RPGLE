000100090923      *PARMS dftactgrp(*no) actgrp('QILE')
000200090923       //--------------------------------------------------------------
000300090923       //?TEST - CREAZIONE FILE ESTRATTO BOLLE -TIVTA10-               ?
000400090923       //--------------------------------------------------------------
000500090923
000600090923     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700090923     h dftactgrp(*no) actgrp('QILE')
000800090923
000900090923       //--------------------------------------------------------------
001000090923       //?Dichiarazione file.                                          ?
001100090923       //--------------------------------------------------------------
001200090923
001300090923       // - Video
001400090923     fTIS7991TD cf   e             workstn
001500090923     f                                     indds(IndDspF)
001600090923     f                                     infds(InfDspF)
001700090923
001800090923       //--------------------------------------------------------------
001900090923       //?Definizione costanti.                                        ?
002000090923       //--------------------------------------------------------------
002100090923
002200090923       // - Tasti funzionali a video
002300090923     d c_F01           c                   const(x'31')
002400090923     d c_F02           c                   const(x'32')
002500090923     d c_F03           c                   const(x'33')
002600090923     d c_F04           c                   const(x'34')
002700090923     d c_F05           c                   const(x'35')
002800090923     d c_F06           c                   const(x'36')
002900090923     d c_F07           c                   const(x'37')
003000090923     d c_F08           c                   const(x'38')
003100090923     d c_F09           c                   const(x'39')
003200090923     d c_F10           c                   const(x'3A')
003300090923     d c_F11           c                   const(x'3B')
003400090923     d c_F12           c                   const(x'3C')
003500090923     d c_F13           c                   const(x'B1')
003600090923     d c_F14           c                   const(x'B2')
003700090923     d c_F15           c                   const(x'B3')
003800090923     d c_F16           c                   const(x'B4')
003900090923     d c_F17           c                   const(x'B5')
004000090923     d c_F18           c                   const(x'B6')
004100090923     d c_F19           c                   const(x'B7')
004200090923     d c_F20           c                   const(x'B8')
004300090923     d c_F21           c                   const(x'B9')
004400090923     d c_F22           c                   const(x'BA')
004500090923     d c_F23           c                   const(x'BB')
004600090923     d c_F24           c                   const(x'BC')
004700090923     d c_Enter         c                   const(x'F1')
004800090923     d c_RollDown      c                   const(x'F4')
004900090923     d c_RollUp        c                   const(x'F5')
005000090923
005100090923       //--------------------------------------------------------------
005200090923       //?Definizione schiere.                                         ?
005300090923       //--------------------------------------------------------------
005400090923
005500090923
005600090923       //--------------------------------------------------------------
005700090923       //?Definizione aree dati.                                       ?
005800090923       //--------------------------------------------------------------
005900090923
006000090923       // - Dati utente
006100090923     d �AzUte        e ds                  extname(AZUTE00F)
006200090923     d                                     dtaara
006300090923     d �DatiUte      e ds                  extname(dDatiUte)
006400090923     d                                     dtaara
006500090923
006600090923       //--------------------------------------------------------------
006700090923       //?Definizione strutture dati.                                  ?
006800090923       //--------------------------------------------------------------
006900090923
007000090923       // - Status ds
007100090923     d Status         sds
007200090923     d  SDSpgm           *proc
007300090923     d  SDSprm           *parms
007400090923     d  SDSusr               254    263
007500090923
007600090923       // - InfDS
007700090923     d InfDspF         ds
007800090923     d  dsp_aid              369    369a
007900090923
008000090923       // - Indicatori su DspF
008100090923     d IndDspF         ds
008200090923        // - Abilitazione tasti funzionali
008300090923     d  AbilitF06                      n   overlay(IndDspF : 06)
008400090923     d  AbilitF12                      n   overlay(IndDspF : 12)
008500090923        // - Indicatori di errore
008600090923     d  ErrMessage                     n   overlay(IndDspF : 28)
008700090923     d  ErrMsgPGM                      n   overlay(IndDspF : 50)
008800090923     d  ErrGenerico                    n   overlay(IndDspF : 99)
008900090923
009000090923       // - Parametri ricevuti
009100090923     d KPJBA         e ds
009200090923
009300090923       // - Parametri x Controllo profilo utenti
009400090923     d TIBS34ds      e ds
009500090923
009600090923       // - Parametri x programmi chiamati (sia TIS799R che TIS7991R)
009700090923     d TIS799dsI     e ds                  inz
009800091109     d TIVTA10LF     e ds                  inz prefix(l_)
009900090923
010000090923       //--------------------------------------------------------------
010100090923       //?Definizione variabili globali.                               ?
010200090923       //--------------------------------------------------------------
010300090923
010400090923       // - Flags booleani
010500090923     d $Fine           s               n   inz(*off)
010600090923     d $InzD01         s               n   inz(*on)
010700090923     d $InzD02         s               n   inz(*on)
010800090923
010900090923       // - Campi per la gestione del video
011000090923     d $Video          s              2a   inz('D1')
011100090923
011200090923       //--------------------------------------------------------------
011300090923       //?Definizione prototipi procedure usate.                       ?
011400090923       //--------------------------------------------------------------
011500090923
011600090923       // - Reperimento dati utente
011700090923     d tibs34r         pr                  extpgm('TIBS34R')
011800090923     d   tibs34ds                          likeds(TIBS34ds)
011900090923
012000090923       // - T&T: creazione file estratto bolle - Esegue
012100090923     d tis799r         pr                  extpgm('TIS799R')
012200090923     d   tis799dsI                         likeds(TIS799dsI)
012300090923     d tis7991r        pr                  extpgm('TIS7991R')
012400090923     d   tis799dsI                         likeds(TIS799dsI)
012500091109     d   rwrinew                           like(l_rwrinew)
012600091109     d   rdelnew                           like(l_rdelnew)
012700090923
012800090923       //--------------------------------------------------------------
012900090923       //?Definizione key-list.                                        ?
013000090923       //--------------------------------------------------------------
013100090923
013200090923
013300090923       //--------------------------------------------------------------
013400090923       //?Riepilogo indicatori.                                        ?
013500090923       //--------------------------------------------------------------
013600090923
013700090923
013800090923       //--------------------------------------------------------------
013900090923       //?M A I N - L I N E                                            ?
014000090923       //--------------------------------------------------------------
014100090923
014200090923     c     *Entry        plist
014300090923     c                   parm                    KPJBA
014400090923
014500090923      /free
014600090923
014700090923       // - Start
014800090923       *inLR = *on;
014900090923
015000090923       // - Elab
015100090923       dow  $Fine = *off;
015200090923         select;
015300090923           when  $Video = 'D1';
015400090923             exsr sr_GesD01;
015500090923           when  $Video = 'D2';
015600090923             exsr sr_GesD02;
015700090923           other;
015800090923             $Fine = *on;
015900090923         endsl;
016000090923       enddo;
016100090923
016200090923       // - End
016300090923       return;
016400090923
016500090923       //--------------------------------------------------------------
016600090923       //?Operazioni iniziali                                          ?
016700090923       //--------------------------------------------------------------
016800090923       BEGSR  *InzSR;
016900090923
017000090923         // - Reperimento dati utente
017100090923         exsr sr_DatiJob;
017200090923
017300090923         // - Impostazione nome programma in testata a video
017400090923         V1Tpgm = SDSpgm;
017500090923
017600090923       ENDSR;
017700090923
017800090923       //--------------------------------------------------------------
017900090923       //?Reperimento Dati del job (Utente/Operativi).                 ?
018000090923       //--------------------------------------------------------------
018100090923       BEGSR  sr_DatiJob;
018200090923
018300090923         in(E) �AzUte;
018400090923         if NOT %error;
018500090923           in(E) �DatiUte;
018600090923         endif;
018700090923         if %error or RSut = *blanks;
018800090923           clear TIBS34ds;
018900090923           tibs34r(tibs34ds);
019000090923           in �AzUte;
019100090923           in �DatiUte;
019200090923         endif;
019300090923
019400090923       ENDSR;
019500090923
019600090923       //--------------------------------------------------------------
019700090923       //?Gestione videata D01                                         ?
019800090923       //--------------------------------------------------------------
019900090923       BEGSR  sr_GesD01;
020000090923
020100090923         // - Inizializzazione videata
020200090923         if  $InzD01 = *on;
020300090923           clear S7991d1;
020400091207           V1Cpgm  = 'TIS799R ';
020500090923           $InzD01 = *off;
020600090923         endif;
020700090923         AbilitF06 = *on;
020800090923         AbilitF12 = *off;
020900090923
021000090923         // - Emissione videata
021100090923         if  ErrMessage = *off;
021200090923           write S7991t1;
021300090923           write S7991z1;
021400090923         endif;
021500090923
021600090923         exfmt S7991d1;
021700090923
021800090923         reset ErrMessage;
021900090923         reset ErrGenerico;
022000090923         clear V1Dmsg;
022100090923
022200090923         SELECT;
022300090923
022400090923           // - F3=Fine
022500090923           WHEN  dsp_aid = c_F03;
022600090923             $Fine = *on;
022700090923
022800090923           // - Invio o F6=Conferma
022900090923           OTHER;
023000090923             exsr sr_CtrD01;
023100090923             select;
023200090923               // - rilevato errore
023300090923               when  ErrGenerico = *on;
023400090923               // - non premuto F6
023500090923               when  dsp_aid <> c_F06;
023600090923               // - premuto F6
023700090923               other;
023800090923                 $Video = 'D2';
023900090923                 reset  $InzD02;
024000090923             endsl;
024100090923
024200090923         ENDSL;
024300090923
024400090923       ENDSR;
024500090923
024600090923       //--------------------------------------------------------------
024700090923       //?Controllo dati in videata D01                                ?
024800090923       //--------------------------------------------------------------
024900090923       BEGSR  sr_CtrD01;
025000090923
025100090923         %subst(IndDspF : 50) = *off;
025200090923
025300090923       ENDSR;
025400090923
025500090923       //--------------------------------------------------------------
025600090923       //?Gestione videata D02                                         ?
025700090923       //--------------------------------------------------------------
025800090923       BEGSR  sr_GesD02;
025900090923
026000090923         // - Inizializzazione videata
026100090923         if  $InzD02 = *on;
026200090923           clear  S7991d2;
026300090923           exsr  Call_TIS799xx;
026400090923           $InzD02 = *off;
026500090923           AbilitF06 = *off;
026600090923           AbilitF12 = *on;
026700090923           if  $Video <> 'D2';
026800090923             leavesr;
026900090923           endif;
027000090923         endif;
027100090923
027200090923         // - Emissione videata
027300090923         if  ErrMessage = *off;
027400090923           write S7991t1;
027500090923           write S7991z1;
027600090923           write S7991d1;
027700090923           write Protect;
027800090923         endif;
027900090923
028000090923         exfmt S7991d2;
028100090923
028200090923         reset ErrMessage;
028300090923         reset ErrGenerico;
028400090923         clear V1Dmsg;
028500090923
028600090923         SELECT;
028700090923
028800090923           // - F3=Fine
028900090923           WHEN  dsp_aid = c_F03;
029000090923             $Fine = *on;
029100090923
029200090923           // - F12=Ritorno
029300090923           WHEN  dsp_aid = c_F12;
029400090923             $Video = 'D1';
029500090923             //reset $InzD01;
029600090923             reset $InzD02;
029700090923
029800090923           // - Invio
029900090923           OTHER;
030000090923
030100090923         ENDSL;
030200090923
030300090923       ENDSR;
030400090923
030500090923       //--------------------------------------------------------------
030600090923       //?Richiamo Service Program TIS799R/TIS7991R                    ?
030700090923       //--------------------------------------------------------------
030800090923       BEGSR  Call_TIS799xx;
030900090923
031000090923         select;
031100090923           when V1Cpgm = 'TIS799R  ';
031200090923             tis799r ( tis799dsI );
031300090923           when V1Cpgm = 'TIS7991R ';
031400091109             tis7991r ( tis799dsI : l_rwrinew : l_rdelnew );
031500090923           other;
031600090923             $Video = 'D1';
031700090923             V1Dmsg = 'Programma NON previsto in chiamata.';
031800090923             ErrMsgPGM = *on;
031900090923             ErrMessage  = *on;
032000090923             ErrGenerico = *on;
032100090923         endsl;
032200090923
032300090923       ENDSR;
032400090923
032500090923      /end-free
