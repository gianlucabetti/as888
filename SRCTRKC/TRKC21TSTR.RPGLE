000100141106       //==============================================================
000200150113       //?TRKC21TSTR - TEST richiamo TRKC21R (Interrog.Cli.in Campagna)?
000300141106       //==============================================================
000400141106
000500141106       //--------------------------------------------------------------
000600141106       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700141106       //--------------------------------------------------------------
000800141106
000900141106     /*PRM dbgview(*source)
001000141106     /*END
001100141106
001200141106       //--------------------------------------------------------------
001300141106       //?Specifiche di controllo.                                     ?
001400141106       //--------------------------------------------------------------
001500141106
001600141106     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700141106     h dftactgrp(*no)
001800141106
001900141106       //--------------------------------------------------------------
002000141106       //?Dichiarazione file.                                          ?
002100141106       //--------------------------------------------------------------
002200141106
002300141106       // -?Video?
002400150113     fTRKC21TSTDcf   e             workstn
002500141106     f                                     indds(IndDspF)
002600141106     f                                     infds(InfDspF)
002700141106
002800141106       //--------------------------------------------------------------
002900141106       //?Definizione costanti.                                        ?
003000141106       //--------------------------------------------------------------
003100141106
003200141106       // -?Tasti funzionali a video?
003300141106     d c_F01           c                   const(x'31')
003400141106     d c_F02           c                   const(x'32')
003500141106     d c_F03           c                   const(x'33')
003600141106     d c_F04           c                   const(x'34')
003700141106     d c_F05           c                   const(x'35')
003800141106     d c_F06           c                   const(x'36')
003900141106     d c_F07           c                   const(x'37')
004000141106     d c_F08           c                   const(x'38')
004100141106     d c_F09           c                   const(x'39')
004200141106     d c_F10           c                   const(x'3A')
004300141106     d c_F11           c                   const(x'3B')
004400141106     d c_F12           c                   const(x'3C')
004500141106     d c_F13           c                   const(x'B1')
004600141106     d c_F14           c                   const(x'B2')
004700141106     d c_F15           c                   const(x'B3')
004800141106     d c_F16           c                   const(x'B4')
004900141106     d c_F17           c                   const(x'B5')
005000141106     d c_F18           c                   const(x'B6')
005100141106     d c_F19           c                   const(x'B7')
005200141106     d c_F20           c                   const(x'B8')
005300141106     d c_F21           c                   const(x'B9')
005400141106     d c_F22           c                   const(x'BA')
005500141106     d c_F23           c                   const(x'BB')
005600141106     d c_F24           c                   const(x'BC')
005700141106     d c_Enter         c                   const(x'F1')
005800141106     d c_RollDown      c                   const(x'F4')
005900141106     d c_RollUp        c                   const(x'F5')
006000141106
006100141106       //--------------------------------------------------------------
006200141106       //?Definizione schiere.                                         ?
006300141106       //--------------------------------------------------------------
006400141106
006500141106
006600141106       //--------------------------------------------------------------
006700141106       //?Definizione aree dati.                                       ?
006800141106       //--------------------------------------------------------------
006900141106
007000141106       // -?Dati utente?
007100141106     d �AzUte        e ds                  extname(AZUTE00F)
007200141106     d                                     dtaara
007300141106     d �DatiUte      e ds                  extname(dDatiUte)
007400141106     d                                     dtaara
007500141106
007600141106       //--------------------------------------------------------------
007700141106       //?Definizione strutture dati.                                  ?
007800141106       //--------------------------------------------------------------
007900141106
008000141106       // -?Status ds?
008100141106     d Status         sds
008200141106     d   SDSpgm          *proc
008300141106     d*//SDSprm          *parms
008400141106     d*//SDSusr              254    263
008500141106
008600141106       // -?InfDS?
008700141106     d InfDspF         ds
008800141106     d   dsp_aid             369    369a
008900141106     d*//sfl_rrn             376    377i 0
009000141106     d*//min_nrr             378    379i 0
009100141106     d*//num_rcds            380    381i 0
009200141106
009300141106       // -?Indicatori su DspF?
009400141106     d IndDspF         ds                  inz
009500141106         // -?Abilitazione tasti funzionali?
009600141106     d   F3Attivo                      n   overlay(IndDspF : 03)
009700141106     d   F6Attivo                      n   overlay(IndDspF : 06)
009800141106     d   F12Attivo                     n   overlay(IndDspF : 12)
009900141106         // -?Emissione messaggio di errore?
010000141106     d   ErrMessage                    n   overlay(IndDspF : 28)
010100141106         // -?Indicatori di gestione del subfile?
010200141106     d   SflDsp_N                      n   overlay(IndDspF : 30)
010300141106     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
010400141106     d   SflNxtChg                     n   overlay(IndDspF : 32)
010500141106     d   SflEnd                        n   overlay(IndDspF : 33)
010600141106         // -?Riemissione videata?
010700141106     d   ErrGenerico                   n   overlay(IndDspF : 99)
010800141106
010900141106       // -?Parametri ricevuti?
011000141106     d KPJBA         e ds
011100141106
011200141106       //--------------------------------------------------------------
011300141106       //?Definizione variabili globali.                               ?
011400141106       //--------------------------------------------------------------
011500141106
011600141106       // -?Flags Booleani?
011700141106     d $Fine           s               n   inz
011800141106     d $InzD01         s               n   inz
011900141106
012000141106       // -?Campi di comodo?
012100141106     d wDate_ISO       s               d   datfmt(*iso)  inz
012200141106     d wDate           s              8  0 inz
012300141106
012400141106       //--------------------------------------------------------------
012500141106       //?Definizione prototipi procedure usate.                       ?
012600141106       //--------------------------------------------------------------
012700141106
012800141106       // -?Reperimento dati utente?
012900141106     d TIBS34ds      e ds                  inz
013000141106      /copy gaitrasrc/srcProtoPR,TIBS34R
013100141106
013200141106       // -?Reperimento dati utente?
013300150113     d TRKC21ds      e ds                  inz
013400150113     d trkc21r         pr                  extpgm('TRKC21R')
013500141106     d   kpjba                             likeds(KPJBA)
013600150113     d   trkc21ds                          likeds(TRKC21ds)
013700141106
013800141106       //--------------------------------------------------------------
013900141106       //?Definizione key-list.                                        ?
014000141106       //--------------------------------------------------------------
014100141106
014200141106
014300141106       //--------------------------------------------------------------
014400141106       //?Riepilogo indicatori utilizzati.                             ?
014500141106       //--------------------------------------------------------------
014600141106       //--------------------------------------------------------------
014700141106
014800141106       //--------------------------------------------------------------
014900141106       //?M A I N - L I N E                                            ?
015000141106       //--------------------------------------------------------------
015100141106
015200141106     c     *Entry        plist
015300141106     c                   parm                    KPJBA
015400141106
015500141106      /free
015600141106
015700141106       // -?Operazioni iniziali?
015800141106       exsr sr_RoutInz;
015900141106
016000141106       // -?Richiamo *pgm in test?
016100141106       DoW  $Fine = *off;
016200141106         exsr  sr_GesD01;
016300141106       EndDo;
016400141106
016500141106       // -?Operazioni finali?
016600141106       exsr sr_RoutEnd;
016700141106
016800141106       //--------------------------------------------------------------
016900141106       //?Operazioni iniziali.                                         ?
017000141106       //--------------------------------------------------------------
017100141106       BEGSR  sr_RoutInz;
017200141106
017300141106         // -?Impostazione chiusura?
017400141106         *inLR = *on;
017500141106
017600141106         // -?Reperimento dati job?
017700141106         exsr  sr_DatiJob;
017800141106
017900141106         // -?Reperimento data odierna?
018000141106         wDate_Iso = %date();
018100141106         wDate = %dec( %date() );
018200141106
018300141106       ENDSR;
018400141106
018500141106       //--------------------------------------------------------------
018600141106       //?Reperimento Dati del job (Utente/Operativi).                 ?
018700141106       //--------------------------------------------------------------
018800141106       BEGSR  sr_DatiJob;
018900141106
019000141106         in(e) �AzUte;
019100141106         if NOT %error;
019200141106           in(e) �DatiUte;
019300141106         endif;
019400141106         if %error or RSut = *blank;
019500141106           tibs34r ( tibs34ds );
019600141106           in �AzUte;
019700141106           in �DatiUte;
019800141106         endif;
019900141106
020000141106       ENDSR;
020100141106
020200141106       //--------------------------------------------------------------
020300141106       //?Gestione videata D01                                         ?
020400141106       //--------------------------------------------------------------
020500141106       BEGSR  sr_GesD01;
020600141106
020700141106         // -?Inizializzazione videata?
020800141106         if  $InzD01 = *on;
020900150113           clear  KC21D01;
021000141106           $InzD01 = *off;
021100141106         endif;
021200141106
021300141106         F6Attivo  = *on;
021400141106         F12Attivo = *off;
021500141106
021600141106         // -?Emissione videata?
021700141106         if  ErrMessage = *off;
021800150113           write  KC21T01;
021900150113           write  KC21P01;
022000141106         endif;
022100150113         exfmt  KC21D01;
022200141106
022300141106         clear  V1Dmsg;
022400141106         reset  ErrMessage;
022500141106         reset  ErrGenerico;
022600141106
022700141106         SELECT;
022800141106
022900141106           // -?F3=Fine?
023000141106           WHEN  dsp_aid = c_F03;
023100141106             $Fine = *on;
023200141106
023300141106           // -?Invio o F6=Conferma?
023400141106           OTHER;
023500141106             exsr  sr_CtrD01;
023600141106             select;
023700141106               // -?rilevato errore?
023800141106               when  ErrGenerico;
023900141106                 leavesr;
024000141106               // -?non premuto F6?
024100141106               when  dsp_aid <> c_F06;
024200141106               // -?richiamo da eseguire?
024300141106               other;
024400150113                 exsr  sr_CallTRKC21R;
024500141106             endsl;
024600141106
024700141106         ENDSL;
024800141106
024900141106       ENDSR;
025000141106
025100141106       //--------------------------------------------------------------
025200150113       //?Richiamo *pgm TRKC21R                                        ?
025300141106       //--------------------------------------------------------------
025400150113       BEGSR  sr_CallTRKC21R;
025500141106
025600150113         // -?Richiamo *pgm TRKC21R?
025700150113         TRKC21R ( KPJBA : TRKC21ds );
025800141106
025900150113         *in28  = ( oKC21err <> *blank );
026000150113         V1Dmsg = oKC21msg;
026100141106
026200141106         F6Attivo  = *off;
026300141106         F12Attivo = *on;
026400141106
026500150113         write  KC21T01;
026600150113         write  KC21P01;
026700150113         write  KC21D01;
026800141106         write  Protect;
026900141106
027000141106         // -?Emissione 2� videata (con il risultato)?
027100141106         DoU  dsp_aid = c_F03  or  dsp_aid = c_F12;
027200141106
027300150113           exfmt  KC21D02;
027400141106
027500141106           // -?F3=Fine?
027600141106           if  dsp_aid = c_F03;
027700141106             $Fine = *on;
027800141106           endif;
027900141106
028000141106         EndDo;
028100141106
028200141106         //$InzD01 = *on;
028300141106
028400141106       ENDSR;
028500141106
028600141106       //--------------------------------------------------------------
028700141106       //?Controllo dati in videata D01                                ?
028800141106       //--------------------------------------------------------------
028900141106       BEGSR  sr_CtrD01;
029000141106
029100141106         %subst(IndDspF : 28) = *off;
029200141106
029300141106       ENDSR;
029400141106
029500141106       //--------------------------------------------------------------
029600141106       //?Operazioni finali.                                         ?
029700141106       //--------------------------------------------------------------
029800141106       BEGSR  sr_RoutEnd;
029900141106
030000141106         // -?Chiusura *pgm?
030100141106         return;
030200141106
030300141106       ENDSR;
030400141106
030500141106      /end-free
