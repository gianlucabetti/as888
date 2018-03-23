000100151029       //==============================================================
000200151103       //?TNSS10R - Filtro Statistica spedizioni fino a 30 Kg          ?
000300151030       //?          (per Clienti Codificati e NON).                    ?
000400151029       //==============================================================
000500151029
000600151029       //--------------------------------------------------------------
000700151029       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800151029       //--------------------------------------------------------------
000900151029
001000151029     /*PRM dbgview(*source)
001100151029     /*END
001200151029
001300151029       //--------------------------------------------------------------
001400151029       //?Specifiche di controllo.                                     ?
001500151029       //--------------------------------------------------------------
001600151029
001700151029     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800151029     h dftactgrp(*no)
001900151029
002000151029       //--------------------------------------------------------------
002100151029       //?Dichiarazione file.                                          ?
002200151029       //--------------------------------------------------------------
002300151029
002400151029       // -?Video?
002500151029     fTNSS10D   cf   e             workstn
002600151029     f                                     indds(IndDspF)
002700151029     f                                     infds(InfDspF)
002800151029
002900151029       //--------------------------------------------------------------
003000151029       //?Definizione costanti.                                        ?
003100151029       //--------------------------------------------------------------
003200151029
003300151029       // -?Tasti funzionali a video?
003400151029     d c_F01           c                   const(x'31')
003500151029     d c_F02           c                   const(x'32')
003600151029     d c_F03           c                   const(x'33')
003700151029     d c_F04           c                   const(x'34')
003800151029     d c_F05           c                   const(x'35')
003900151029     d c_F06           c                   const(x'36')
004000151029     d c_F07           c                   const(x'37')
004100151029     d c_F08           c                   const(x'38')
004200151029     d c_F09           c                   const(x'39')
004300151029     d c_F10           c                   const(x'3A')
004400151029     d c_F11           c                   const(x'3B')
004500151029     d c_F12           c                   const(x'3C')
004600151029     d c_F13           c                   const(x'B1')
004700151029     d c_F14           c                   const(x'B2')
004800151029     d c_F15           c                   const(x'B3')
004900151029     d c_F16           c                   const(x'B4')
005000151029     d c_F17           c                   const(x'B5')
005100151029     d c_F18           c                   const(x'B6')
005200151029     d c_F19           c                   const(x'B7')
005300151029     d c_F20           c                   const(x'B8')
005400151029     d c_F21           c                   const(x'B9')
005500151029     d c_F22           c                   const(x'BA')
005600151029     d c_F23           c                   const(x'BB')
005700151029     d c_F24           c                   const(x'BC')
005800151029     d c_Enter         c                   const(x'F1')
005900151029     d c_RollDown      c                   const(x'F4')
006000151029     d c_RollUp        c                   const(x'F5')
006100151029
006200151029       //--------------------------------------------------------------
006300151029       //?Definizione schiere.                                         ?
006400151029       //--------------------------------------------------------------
006500151029
006600151029       // -?Messaggi di errore?
006700151029     d sk_Msg          s             78    dim( 5)  ctdata  perrcd( 1)
006800151029
006900151029       //--------------------------------------------------------------
007000151029       //?Definizione aree dati.                                       ?
007100151029       //--------------------------------------------------------------
007200151029
007300151029       // -?Dati utente?
007400151029     d §AzUte        e ds                  extname(AZUTE00F)
007500151029     d                                     dtaara
007600151029     d §DatiUte      e ds                  extname(dDatiUte)
007700151029     d                                     dtaara
007800151029
007900151029       //--------------------------------------------------------------
008000151029       //?Definizione strutture dati.                                  ?
008100151029       //--------------------------------------------------------------
008200151029
008300151029       // -?InfDS?
008400151029     d InfDspF         ds
008500151029     d   dsp_aid             369    369a
008600151029     d*//sfl_rrn             376    377i 0
008700151029     d*//min_nrr             378    379i 0
008800151029     d*//num_rcds            380    381i 0
008900151029
009000151029       // -?Indicatori su DspF?
009100151029     d IndDspF         ds                  inz
009200151029         // -?Abilitazione tasti funzionali?
009300151029     d   F9Attivo                      n   overlay( IndDspF : 09 )
009400151029         // -?Emissione messaggio di errore?
009500151029     d   ErrMessage                    n   overlay( IndDspF : 28 )
009600151029         // -?Posizionamento cursore & segnalazione errore?
009700151029     d   PosCurDSI                     n   overlay( IndDspF : 51 )
009800151029     d   PosCurDSF                     n   overlay( IndDspF : 52 )
009900151029         //   -?Riemissione videata?
010000151029     d   ErrGenerico                   n   overlay( IndDspF : 99 )
010100151029
010200151029       // -?Status ds?
010300151029     d Status         sds
010400151029     d   SDSpgmName      *proc
010500151029
010600151029       // -?Parametri ricevuti?
010700151029     d KPJBA         e ds
010800151029
010900151029       //--------------------------------------------------------------
011000151029       //?Definizione variabili globali.                               ?
011100151029       //--------------------------------------------------------------
011200151029
011300151029       // -?Flags booleani?
011400151029     d $Fine           s               n   inz(*off)
011500151029     d $InzD01         s               n   inz(*on)
011600151029
011700151029       // -?Variabili per la gestione del video?
011800151029     d $Video          s              2    inz('D1')
011900151029
012000151029       //--------------------------------------------------------------
012100151029       //?Definizione prototipi procedure.                             ?
012200151029       //--------------------------------------------------------------
012300151029
012400151029       // -?Reperimento dati utente?
012500151029     d TIBS34ds      e ds                  inz
012600151029      /copy gaitrasrc/srcProtoPR,TIBS34R
012700151029
012800151029       // -?Controllo ed inversione date?
012900151029     d WLBdat          ds                  inz
013000151029     d   G08dat                       8  0 inz
013100151029     d   G08inv                       8  0 inz
013200151029     d   G08err                       1    inz('3')
013300151029     d   G08tgi                       5  0 inz
013400151029      /copy gaitrasrc/srcProtoPr,XSRDA8
013500151029
013600151029       // -?Richiamo diretto lavoro batch?
013700151029     d TNSS11ds      e ds                  inz
013800151029     d   SS11dsi     e                     inz(*loval)
013900151029     d   SS11dsf     e                     inz(*hival)
014000151029     d tnss11r         pr                  extpgm('TNSS11R')
014100151029     d   kpjba                             likeds(KPJBA)
014200151029
014300151029       // -?Personalizzazione lavoro batch?
014400151029     d Bch09           pr                  extpgm('BCH09')
014500151029     d   kpjba                             likeds(KPJBA)
014600151029
014700151029       // -?Sottomissione lavoro batch?
014800151029      /copy gaitrasrc/srcProtoPR,BCH10
014900151029
015000151029       //--------------------------------------------------------------
015100151029       //?Definizione key-list.                                        ?
015200151029       //--------------------------------------------------------------
015300151029
015400151029
015500151029       //--------------------------------------------------------------
015600151029       //?Riepilogo indicatori utilizzati.                             ?
015700151029       //--------------------------------------------------------------
015800151029
015900151029
016000151029       //--------------------------------------------------------------
016100151029       //?M A I N - L I N E                                            ?
016200151029       //--------------------------------------------------------------
016300151029
016400151029     c     *Entry        plist
016500151029     c                   parm                    KPJBA
016600151029
016700151029      /free
016800151029
016900151029       // -?Operazioni iniziali?
017000151029       exsr  sr_RoutInz;
017100151029
017200151029       // -?Ciclo di gestione del file video?
017300151029       DoW  $Fine = *off;
017400151029
017500151029         select;
017600151029
017700151029           // -?Gestione filtro di lancio?
017800151029           when $Video = 'D1';
017900151029             exsr  sr_GesD01;
018000151029
018100151029           // -?? ? ? ? ??
018200151029           other;
018300151029             $Fine = *on;
018400151029
018500151029         endsl;
018600151029
018700151029       EndDo;
018800151029
018900151029       // -?Operazioni finali?
019000151029       exsr sr_RoutEnd;
019100151029
019200151029       //--------------------------------------------------------------
019300151029       //?Operazioni iniziali.                                         ?
019400151029       //--------------------------------------------------------------
019500151029       BEGSR  sr_RoutInz;
019600151029
019700151029         // -?Impostazione chiusura?
019800151029         *inLR = *on;
019900151029
020000151029         // -?Reperimento dati job?
020100151029         exsr  sr_DatiJob;
020200151029
020300151029         // -?Impostazione nome programma a video?
020400151029         V1Tpgm = SDSpgmName;
020500151029
020600151029       ENDSR;
020700151029
020800151029       //--------------------------------------------------------------
020900151029       //?Reperimento Dati del job (Utente/Operativi).                 ?
021000151029       //--------------------------------------------------------------
021100151029       BEGSR  sr_DatiJob;
021200151029
021300151029         in(e) §AzUte;
021400151029         if NOT %error;
021500151029           in(e) §DatiUte;
021600151029         endif;
021700151029         if %error or RSut = *blank;
021800151029           tibs34r ( tibs34ds );
021900151029           in §AzUte;
022000151029           in §DatiUte;
022100151029         endif;
022200151029
022300151029       ENDSR;
022400151029
022500151029       //--------------------------------------------------------------
022600151029       //?Gestione videata D01.                                        ?
022700151029       //--------------------------------------------------------------
022800151029       BEGSR  sr_GesD01;
022900151029
023000151029         // -?Inizializzazione videata?
023100151029         if  $InzD01 = *on;
023200151029           exsr  sr_InzD01;
023300151029           $InzD01 = *off;
023400151029         endif;
023500151029
023600151029         // -?Emissione videata?
023700151029         write  SS10T01;
023800151029         exfmt  SS10D01;
023900151029
024000151029         clear  V1Dmsg;
024100151029         reset  ErrMessage;
024200151029         reset  ErrGenerico;
024300151029
024400151029         SELECT;
024500151029
024600151029           // -?Sistema Informativo errato => FINE?
024700151029           WHEN  %subst( KNSIF : 1 : 3 ) = 'FIL';
024800151029             $Fine = *on;
024900151029
025000151029           // -?F3=Fine?
025100151029           WHEN  dsp_aid = c_F03;
025200151029             $Fine = *on;
025300151029
025400151029           //// -?F9=Personalizzazione Batch?
025500151030           WHEN  dsp_aid = c_F09;
025600151030             Kcoaz = 'SS11';
025700151030             Bch09 ( kpjba );
025800151029
025900151029           // -?Invio / F6=Conferma?
026000151029           OTHER;
026100151029             exsr  sr_CtrD01;
026200151029             // -?F6 = Lancio batch (se NO errori)?
026300151029             if  ErrGenerico = *off  and  dsp_aid = c_F06;
026400151029               exsr  sr_F06D01;
026500151029               $Fine = *on;
026600151029             endif;
026700151029
026800151029         ENDSL;
026900151029
027000151029       ENDSR;
027100151029
027200151029       //--------------------------------------------------------------
027300151029       //?Inizializzazione videata D01.                                ?
027400151029       //--------------------------------------------------------------
027500151029       BEGSR  sr_InzD01;
027600151029
027700151029         clear  SS10D01;
027800151029
027900151029         // -?Ctrl se utente di Sede o Filiale?
028000151029         if  DUTlpo <> 'S';
028100151029           ErrGenerico = *on;
028200151029           ErrMessage  = *on;
028300151029           V1Dmsg = sk_Msg(01);
028400151029         endif;
028500151029
028600151029         // -?(Dis)Abilitazione tasti funzionali?
028700151029         F9Attivo = ( DUTute = 'EDPSM     ' );
028800151029
028900151029       ENDSR;
029000151029
029100151029       //--------------------------------------------------------------
029200151029       //?Controllo dati nella videata D01.                            ?
029300151029       //--------------------------------------------------------------
029400151029       BEGSR  sr_CtrD01;
029500151029
029600151029         // -?Spegnimento indicatori di posizionamento cursore?
029700151029         %subst( IndDspF : 50 ) = *off;
029800151029
029900151029         clear  SS11dsi;
030000151029         clear  SS11dsf;
030100151029
030200151029         // -?Date spedizioni?
030300151029         if  V1Cdsi = *zero  and  V1Cdsf = *zero;
030400151029           PosCurDSI   = *on;
030500151029           ErrMessage  = *on;
030600151029           ErrGenerico = *on;
030700151029           V1Dmsg = sk_Msg(02);
030800151029           leavesr;
030900151029         endif;
031000151029
031100151029         // -?Data spedizione iniziale?
031200151029         clear  WLBdat;
031300151029         if  V1Cdsi = *zero;
031400151029           PosCurDSI   = *on;
031500151029           ErrMessage  = *on;
031600151029           ErrGenerico = *on;
031700151029           V1Dmsg = sk_Msg(03);
031800151029           leavesr;
031900151029         endif;
032000151029         G08dat = V1Cdsi;
032100151029         xsrda8 ( WLBdat );
032200151029         if  G08err =  *on;
032300151029           PosCurDSI   = *on;
032400151029           ErrMessage  = *on;
032500151029           ErrGenerico = *on;
032600151029           V1Dmsg = sk_Msg(04);
032700151029           leavesr;
032800151029         endif;
032900151029         V1Cdsi  = G08dat;
033000151029         SS11dsi = G08inv;
033100151029
033200151029         // -?Data spedizione finale?
033300151029         clear  WLBdat;
033400151029         if  V1Cdsf = *zero;
033500151029           PosCurDSF   = *on;
033600151029           ErrMessage  = *on;
033700151029           ErrGenerico = *on;
033800151029           V1Dmsg = sk_Msg(03);
033900151029           leavesr;
034000151029         endif;
034100151029         G08dat = V1Cdsf;
034200151029         xsrda8 ( WLBdat );
034300151029         if  G08err =  *on;
034400151029           PosCurDSF   = *on;
034500151029           ErrMessage  = *on;
034600151029           ErrGenerico = *on;
034700151029           V1Dmsg = sk_Msg(04);
034800151029           leavesr;
034900151029         endif;
035000151029         V1Cdsf  = G08dat;
035100151029         SS11dsf = G08inv;
035200151029
035300151029         // -?Range date spedizioni?
035400151029         if  SS11dsi > SS11dsf;
035500151029           PosCurDSI   = *on;
035600151029           ErrMessage  = *on;
035700151029           ErrGenerico = *on;
035800151029           V1Dmsg = sk_Msg(05);
035900151029           leavesr;
036000151029         endif;
036100151029
036200151029       ENDSR;
036300151029
036400151029       //--------------------------------------------------------------
036500151029       //?Gestione tasto funzionale F6 da videata D01:                 ?
036600151029       //?F6=Conferma (Sottomissione lavoro batch)                     ?
036700151029       //--------------------------------------------------------------
036800151029       BEGSR  sr_F06D01;
036900151029
037000151029         // -?Impostazione dei parametri restanti?
037100151029         kCoAz = 'SS11';
037200151029         kpjbu = TNSS11ds;
037300151029
037400151029         // -?Lancio?
037500151029         if  KNMUS = *all'1';
037600151029           // -?Richiamo interattivo?
037700151029           tnss11r ( kpjba );
037800151029         else;
037900151029           // -?Sottomissione batch?
038000151029           bch10 ( kpjba );
038100151029         endif;
038200151029
038300151029       ENDSR;
038400151029
038500151029       //--------------------------------------------------------------
038600151029       //?Operazioni finali.                                           ?
038700151029       //--------------------------------------------------------------
038800151029       BEGSR  sr_RoutEnd;
038900151029
039000151029         return;
039100151029
039200151029       ENDSR;
039300151029
039400151029      /end-free
039500151029
039600151029       //--------------------------------------------------------------
039700151029       //?Schiere a tempo di compilazione.
039800151029       //--------------------------------------------------------------
039900151029
040000151029** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
040100151029Utente non abilitato alla funzione                                             1
040200151029Impostare il periodo da elaborare                                              2
040300151029Data obbligatoria                                                              3
040400151029Data formalmente errata                                                        4
040500151029Data iniziale incongruente con data finale                                     5
