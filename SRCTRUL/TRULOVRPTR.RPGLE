000100080819      *PARMS DFTACTGRP(*NO) ACTGRP(QILE) OPTION(*NOXREF)
000200080819       //--------------------------------------------------------------
000300090922       //?TEST - PREPARAZIONE/ESECUZIONE *CMD OVRPRTF O DLTOVR
000400080819       //--------------------------------------------------------------
000500080819
000600080819     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700080819     h dftactgrp(*no) actgrp('QILE')
000800080819
000900080819       //--------------------------------------------------------------
001000080819       //?Dichiarazione file.
001100080819       //--------------------------------------------------------------
001200080819
001300090129       // - Video
001400090922     fTRULOVRPTDcf   e             workstn
001500080819     f                                     indds(IndDspF)
001600080819     f                                     infds(InfDspF)
001700080819
001800080819       //--------------------------------------------------------------
001900080819       //?Definizione costanti.
002000080819       //--------------------------------------------------------------
002100080819
002200090129       // - Tasti funzionali a video
002300080819     d c_F01           c                   const(x'31')
002400080819     d c_F02           c                   const(x'32')
002500080819     d c_F03           c                   const(x'33')
002600080819     d c_F04           c                   const(x'34')
002700080819     d c_F05           c                   const(x'35')
002800080819     d c_F06           c                   const(x'36')
002900080819     d c_F07           c                   const(x'37')
003000080819     d c_F08           c                   const(x'38')
003100080819     d c_F09           c                   const(x'39')
003200080819     d c_F10           c                   const(x'3A')
003300080819     d c_F11           c                   const(x'3B')
003400080819     d c_F12           c                   const(x'3C')
003500080819     d c_F13           c                   const(x'B1')
003600080819     d c_F14           c                   const(x'B2')
003700080819     d c_F15           c                   const(x'B3')
003800080819     d c_F16           c                   const(x'B4')
003900080819     d c_F17           c                   const(x'B5')
004000080819     d c_F18           c                   const(x'B6')
004100080819     d c_F19           c                   const(x'B7')
004200080819     d c_F20           c                   const(x'B8')
004300080819     d c_F21           c                   const(x'B9')
004400080819     d c_F22           c                   const(x'BA')
004500080819     d c_F23           c                   const(x'BB')
004600080819     d c_F24           c                   const(x'BC')
004700080819     d c_Enter         c                   const(x'F1')
004800080819     d c_RollDown      c                   const(x'F4')
004900080819     d c_RollUp        c                   const(x'F5')
005000080819
005100080819       //--------------------------------------------------------------
005200080819       //?Definizione schiere.
005300080819       //--------------------------------------------------------------
005400080819
005500090129       // - Messaggi di errore
005600090922     d $Msg            s             78    dim( 2) ctdata perrcd(1)
005700080819
005800080819       //--------------------------------------------------------------
005900080819       //?Definizione aree dati.
006000080819       //--------------------------------------------------------------
006100080819
006200080819       // - Dati utente
006300080819     d §AzUte        e ds                  extname(AZUTE00F)
006400080819     d                                     dtaara
006500080819     d §DatiUte      e ds                  extname(dDatiUte)
006600080819     d                                     dtaara
006700080819
006800080819       //--------------------------------------------------------------
006900080819       //?Definizione strutture dati.
007000080819       //--------------------------------------------------------------
007100080819
007200080819       // - Status ds
007300080819     d Status         sds
007400080819     d  SDSpgm           *proc
007500080819     d  SDSprm           *parms
007600080819     d  SDSusr               254    263
007700080819
007800090129       // - InfDS
007900080819     d InfDspF         ds
008000080819     d  dsp_aid              369    369a
008100080819
008200090129       // - Indicatori su DspF
008300080819     d IndDspF         ds
008400090922        // - Abilitazione tasti funzionali
008500090922     d  AbilitF06                     1n   overlay(IndDspF : 06)
008600090922     d  AbilitF12                     1n   overlay(IndDspF : 12)
008700090129         // - Indicatori di errore
008800080819     d  ErrMessage                    1n   overlay(IndDspF : 28)
008900080819     d  PosCurToDo                    1n   overlay(IndDspF : 50)
009000090922     d  PosCurPrtF                    1n   overlay(IndDspF : 51)
009100080819     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009200080819
009300090129       // - Parametri ricevuti
009400090922     d p_KPJBA         s                   like(KPJBA)
009500090922     d KPJBA         e ds                  inz
009600080819
009700080819       // - Parametri x Controllo profilo utenti
009800080819     d TIBS34ds      e ds
009900080819
010000080819       // - Parametri x override
010100080819     d trulOvrPds    e ds                  inz
010200080819
010300080819       //--------------------------------------------------------------
010400080819       //?Definizione variabili globali.
010500080819       //--------------------------------------------------------------
010600080819
010700080819       // - Flags booleani
010800080819     d $Fine           s               n   inz(*off)
010900080819     d $InzD01         s               n   inz(*on)
011000090922     d $InzD02         s               n   inz(*on)
011100090922
011200090922       // - Campi per la gestione del video
011300090922     d $Video          s              2a   inz('D1')
011400080819
011500080819       //--------------------------------------------------------------
011600080819       //?Definizione procedure usate.
011700080819       //--------------------------------------------------------------
011800080819
011900080819       // - Reperimento dati utente
012000080819     d tibs34r         pr                  extpgm('TIBS34R')
012100080819     d   tibs34ds                          likeds(TIBS34ds)
012200080819
012300080819       // - Override allo spool di stampa
012400080819     d trulOvrPr       pr                  extpgm('TRULOVRPR')
012500080819     d   trulOvrPds...
012600080819     d                                     likeds(trulOVRPds)
012700080819
012800080819       //--------------------------------------------------------------
012900080819       //?Definizione key-list.
013000080819       //--------------------------------------------------------------
013100080819
013200080819
013300080819       //--------------------------------------------------------------
013400080819       //?Riepilogo indicatori.
013500080819       //--------------------------------------------------------------
013600080819
013700090922     c     *Entry        plist
013800090922     c                   parm                    p_KPJBA
013900080819
014000080819      /free
014100080819
014200080819       // - Start
014300080819       *inLR = *on;
014400090922       if  %parms() > *zero;
014500090922         KPJBA = p_KPJBA;
014600090922       endif;
014700080819
014800080819       // - Elab
014900080819       dow  $Fine = *off;
015000090922         select;
015100090922           when  $Video = 'D1';
015200090922             exsr sr_GesD01;
015300090922           when  $Video = 'D2';
015400090922             exsr sr_GesD02;
015500090922           other;
015600090922             $Fine = *on;
015700090922         endsl;
015800080819       enddo;
015900080819
016000080819       // - End
016100080819       return;
016200080819
016300080819       //--------------------------------------------------------------
016400080819       //?Operazioni iniziali
016500080819       //--------------------------------------------------------------
016600080819       BEGSR *InzSR;
016700080819
016800090922         // - Reperimento dati utente
016900080819         exsr sr_DatiJob;
017000080819
017100090922         // - Impostazione nome programma in testata a video
017200090922         V1Tpgm = SDSpgm;
017300080819
017400080819       ENDSR;
017500080819
017600080819       //--------------------------------------------------------------
017700080819       //?Reperimento Dati del job (Utente/Operativi).
017800080819       //--------------------------------------------------------------
017900080819       BEGSR sr_DatiJob;
018000080819
018100080819         in(E) §AzUte;
018200080819         if NOT %error;
018300080819           in(E) §DatiUte;
018400080819         endif;
018500080819         if %error or RSut = *blanks;
018600080819           clear TIBS34ds;
018700080819           tibs34r(tibs34ds);
018800080819           in §AzUte;
018900080819           in §DatiUte;
019000080819         endif;
019100080819
019200080819       ENDSR;
019300080819
019400080819       //--------------------------------------------------------------
019500080819       //?Gestione videata D01
019600080819       //--------------------------------------------------------------
019700090922       BEGSR  sr_GesD01;
019800080819
019900090922         // - Inizializzazione videata
020000080819         if  $InzD01 = *on;
020100080819           clear ulOVRPd1;
020200080819           $InzD01  = *off;
020300080819         endif;
020400090922         AbilitF06  = *on;
020500090922         AbilitF12  = *off;
020600080819
020700090922         // - Emissione videata
020800080819         if  ErrMessage = *off;
020900080819           write ulOVRPt1;
021000080819           write ulOVRPz1;
021100080819         endif;
021200080819
021300080819         exfmt ulOVRPd1;
021400080819
021500080819         reset ErrMessage;
021600080819         reset ErrGenerico;
021700080819         clear V1Dmsg;
021800080819
021900080819         SELECT;
022000080819
022100090922           // - F3=Fine
022200080819           WHEN  dsp_aid = c_F03;
022300080819             $Fine = *on;
022400080819
022500090922           // - Invio o F6=Conferma
022600080819           OTHER;
022700080819             exsr sr_CtrD01;
022800080819             select;
022900090922               // - rilevato errore
023000080819               when  ErrGenerico = *on;
023100090922               // - non premuto F6
023200080819               when  dsp_aid <> c_F06;
023300090922               // - premuto F6
023400080819               other;
023500090922                 $Video = 'D2';
023600090922                 reset  $InzD02;
023700080819             endsl;
023800080819
023900080819         ENDSL;
024000080819
024100080819       ENDSR;
024200090922
024300090922       //--------------------------------------------------------------
024400090922       //?Controllo dati in videata D01
024500090922       //--------------------------------------------------------------
024600090922       BEGSR sr_CtrD01;
024700090922
024800090922         %subst(IndDspF : 28) = *off;
024900090922
025000090922         SELECT;
025100090922
025200090922           // - Nessun tipo di comando
025300090922           WHEN  uopToDo = *blank;
025400090922             ErrMessage  = *on;
025500090922             ErrGenerico = *on;
025600090922             PosCurToDo  = *on;
025700090922             V1Dmsg = $Msg(01);
025800090922             leavesr;
025900090922
026000090922           // - Nessun file di stampa inserito
026100090922           WHEN  uopPrtF = *blank;
026200090922             ErrMessage  = *on;
026300090922             ErrGenerico = *on;
026400090922             PosCurPrtF  = *on;
026500090922             V1Dmsg = $Msg(02);
026600090922             leavesr;
026700090922
026800090922         ENDSL;
026900090922
027000090922       ENDSR;
027100090922
027200090922       //--------------------------------------------------------------
027300090922       //?Gestione videata D02                                         ?
027400090922       //--------------------------------------------------------------
027500090922       BEGSR  sr_GesD02;
027600090922
027700090922         // Inizializzazione videata
027800090922         if  $InzD02 = *on;
027900090922           clear ulOVRPd2;
028000090922           exsr  Call_trulOvrP;
028100090922           $InzD02  = *off;
028200090922           AbilitF06  = *off;
028300090922           AbilitF12  = *on;
028400090922         endif;
028500090922
028600090922         // Emissione videata
028700090922         if  ErrMessage = *off;
028800090922           write ulOVRPt1;
028900090922           write ulOVRPz1;
029000090922           write ulOVRPd1;
029100090922           write Protect;
029200090922         endif;
029300090922
029400090922         exfmt ulOVRPd2;
029500090922
029600090922         reset ErrMessage;
029700090922         reset ErrGenerico;
029800090922         clear V1Dmsg;
029900090922
030000090922         SELECT;
030100090922
030200090922         // - F3=Fine
030300090922           WHEN  dsp_aid = c_F03;
030400090922             $Fine = *on;
030500090922
030600090922         // - F12=Ritorno
030700090922           WHEN  dsp_aid = c_F12;
030800090922             $Video = 'D1';
030900090922             //reset $InzD01;
031000090922             reset $InzD02;
031100090922
031200090922         // - Invio
031300090922           OTHER;
031400090922
031500090922         ENDSL;
031600090922
031700090922       ENDSR;
031800080819
031900080819       //--------------------------------------------------------------
032000080819       //?Richiamo stored procedure per gestione override
032100080819       //--------------------------------------------------------------
032200080819       BEGSR Call_trulOvrP;
032300080819
032400080819         trulOVRPr (trulOVRPds);
032500090922
032600090922         uopCMDx = uopCMD;
032700080819
032800080819       ENDSR;
032900080819
033000080819      /end-free
033100080819
033200080819** - $Msg
033300080819Tipo di comando obbligatorio
033400090922Nome del file di stampa obbligatorio
