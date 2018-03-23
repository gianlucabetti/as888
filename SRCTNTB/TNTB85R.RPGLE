000100110112      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200110112       //==============================================================
000300110112       //?Gestione tabella "TTR" (Tipi Trattativa)                     ?
000400110112       //==============================================================
000500110112
000600110112       //--------------------------------------------------------------
000700110112       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800110112       //--------------------------------------------------------------
000900110112
001000110112     /*PRM dftactgrp(*no) actgrp(*caller) dbgview(*source)
001100110112     /*END
001200110112
001300110112       //--------------------------------------------------------------
001400110112       //?Specifiche di controllo.                                     ?
001500110112       //--------------------------------------------------------------
001600110112
001700110112     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800110112     h dftactgrp(*no)
001900110112
002000110112       //--------------------------------------------------------------
002100110112       //?Dichiarazione file.                                          ?
002200110112       //--------------------------------------------------------------
002300110112
002400110112       // -?Tabelle?
002500110112     fTNTBE01L  Uf A e           k disk
002600110112
002700110112       // -?Video?
002800110112     fTNTB85D   cf   e             workstn
002900110112     f                                     indds(IndDspF)
003000110112     f                                     infds(InfDspF)
003100110112
003200110112       //--------------------------------------------------------------
003300110112       //?Definizione costanti.                                        ?
003400110112       //--------------------------------------------------------------
003500110112
003600110112       // -?Codice tabella in gestione?
003700110112     d c_Tab           c                   const('TTR')
003800110112
003900110112       // -?Tasti funzionali a video?
004000110112     d c_F01           c                   const(x'31')
004100110112     d c_F02           c                   const(x'32')
004200110112     d c_F03           c                   const(x'33')
004300110112     d c_F04           c                   const(x'34')
004400110112     d c_F05           c                   const(x'35')
004500110112     d c_F06           c                   const(x'36')
004600110112     d c_F07           c                   const(x'37')
004700110112     d c_F08           c                   const(x'38')
004800110112     d c_F09           c                   const(x'39')
004900110112     d c_F10           c                   const(x'3A')
005000110112     d c_F11           c                   const(x'3B')
005100110112     d c_F12           c                   const(x'3C')
005200110112     d c_F13           c                   const(x'B1')
005300110112     d c_F14           c                   const(x'B2')
005400110112     d c_F15           c                   const(x'B3')
005500110112     d c_F16           c                   const(x'B4')
005600110112     d c_F17           c                   const(x'B5')
005700110112     d c_F18           c                   const(x'B6')
005800110112     d c_F19           c                   const(x'B7')
005900110112     d c_F20           c                   const(x'B8')
006000110112     d c_F21           c                   const(x'B9')
006100110112     d c_F22           c                   const(x'BA')
006200110112     d c_F23           c                   const(x'BB')
006300110112     d c_F24           c                   const(x'BC')
006400110112     d c_Enter         c                   const(x'F1')
006500110112     d c_RollDown      c                   const(x'F4')
006600110112     d c_RollUp        c                   const(x'F5')
006700110112
006800110112       //--------------------------------------------------------------
006900110112       //?Definizione schiere.                                         ?
007000110112       //--------------------------------------------------------------
007100110112
007200110112       // -?Messaggi di errore?
007300110112     d $Msg            s             78    dim(12)  ctdata  perrcd( 1)
007400110112
007500110112       //--------------------------------------------------------------
007600110112       //?Definizione aree dati.                                       ?
007700110112       //--------------------------------------------------------------
007800110112
007900110112       // -?Dati utente?
008000110112     d §AzUte        e ds                  extname(AZUTE00F)
008100110112     d                                     dtaara
008200110112     d §DatiUte      e ds                  extname(dDatiUte)
008300110112     d                                     dtaara
008400110112
008500110112       //--------------------------------------------------------------
008600110112       //?Definizione strutture dati.                                  ?
008700110112       //--------------------------------------------------------------
008800110112
008900110112       // -?Status ds?
009000110112     d Status         sds
009100110112     d  SDSpgm           *proc
009200110112
009300110112       // -?InfDS?
009400110112     d InfDspF         ds
009500110112     d   dsp_aid             369    369a
009600110112
009700110112       // -?Indicatori su DspF?
009800110112     d IndDspF         ds                  inz
009900110112         // -?Abilitazione tasti funzionali?
010000110112     d   F5Attivo                      n   overlay(IndDspF : 05)
010100110112     d   F6Attivo                      n   overlay(IndDspF : 06)
010200110112     d   F10Attivo                     n   overlay(IndDspF : 10)
010300110112     d   F16Attivo                     n   overlay(IndDspF : 16)
010400110112         // -?Emissione messaggio di errore?
010500110112     d   ErrMessage                    n   overlay(IndDspF : 28)
010600110112         // -?Posizionamento cursore & segnalazione errore?
010700110112     d   PosCurTTR                     n   overlay(IndDspF : 50)
010800110112     d   PosCurDES                     n   overlay(IndDspF : 51)
010900110112     d   PosCurVCN                     n   overlay(IndDspF : 52)
011000110112     d   PosCurOFF                     n   overlay(IndDspF : 53)
011100110112     d   PosCurORD                     n   overlay(IndDspF : 54)
011200110112     d   PosCurINFO                    n   overlay(IndDspF : 55)
011300110112     d   PosCurACC                     n   overlay(IndDspF : 56)
011400110112         // -?Emissione messaggio di errore in window W1?
011500110112     d   ErrMessageW1                  n   overlay(IndDspF : 90)
011600110112         // -?Riemissione videata?
011700110112     d   ErrGenerico                   n   overlay(IndDspF : 99)
011800110112
011900110112       // -?Parametri ricevuti?
012000110112     d KPJBA         e ds
012100110112
012200110112       // -?Dati record principale della tabella?
012300110112     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012400110112     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012500110112     d                                          prefix(TBX:3)
012600110112
012700110112       // -?Tabelle usate:?
012800110112       // ·?"TTR" = Tipi Trattativa?
012900110112     d dTTR          e ds                  inz  qualified
013000110112
013100110112       //--------------------------------------------------------------
013200110112       //?Definizione variabili globali.                               ?
013300110112       //--------------------------------------------------------------
013400110112
013500110112       // -?Flags booleani?
013600110112     d $Fine           s               n   inz
013700110112     d $InzD01         s               n   inz(*on)
013800110112     d $InzD02         s               n   inz(*on)
013900110112     d $InzW01         s               n   inz(*on)
014000110112     d $Annullamento   s               n   inz
014100110112     d $Immissione     s               n   inz
014200110112     d $Ripristino     s               n   inz
014300110112     d $Protect        s               n   inz
014400110112
014500110112       // -?Variabili per la gestione del video?
014600110112     d $Video          s              2    inz('D1')
014700110112
014800110112       // -?Campi di comodo?
014900110112     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015000110112
015100110112       //--------------------------------------------------------------
015200110112       //?Definizione prototipi procedure.                             ?
015300110112       //--------------------------------------------------------------
015400110112
015500110112       // -?Reperimento dati utente?
015600110112     d TIBS34ds      e ds
015700110112      /copy gaitrasrc/srcProtoPR,TIBS34R
015800110112
015900110112       // -?Ricerca e controllo nuove tabelle?
016000110112     d TIBS02ds      e ds                  inz
016100110112     d   T02mod      e                     inz('R')
016200110112     d   T02cod      e                     inz(c_Tab)
016300110112      /copy gaitrasrc/srcProtoPR,TIBS02R
016400110112
016500110112       //--------------------------------------------------------------
016600110112       //?Definizione key-list.                                        ?
016700110112       //--------------------------------------------------------------
016800110112
016900110112       // -?File TNTBE01L?
017000110112     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017100110112     d                                     prefix(k_)   inz
017200110112
017300110112       //--------------------------------------------------------------
017400110112       //?Riepilogo indicatori utilizzati.                             ?
017500110112       //--------------------------------------------------------------
017600110112       //--------------------------------------------------------------
017700110112
017800110112       //--------------------------------------------------------------
017900110112       //?M A I N - L I N E                                            ?
018000110112       //--------------------------------------------------------------
018100110112
018200110112     c     *Entry        plist
018300110112     c                   parm                    KPJBA
018400110112
018500110112      /free
018600110112
018700110112       // -?Operazioni iniziali?
018800110112       exsr  sr_RoutInz;
018900110112
019000110112       // -?Ciclo di gestione del file video?
019100110112       DOW  $Fine = *off;
019200110112
019300110112         select;
019400110112
019500110112           // -?Richiesta chiave di accesso?
019600110112           when $Video = 'D1';
019700110112             exsr  sr_GesD01;
019800110112
019900110112           // -?Manutenzione dati della singola tabella?
020000110112           when $Video = 'D2';
020100110112             exsr  sr_GesD02;
020200110112
020300110112           // -?Richiesta dati per trasmissione record?
020400110112           when $Video = 'W1';
020500110112             exsr  sr_GesW01;
020600110112
020700110112           // -?? ? ??
020800110112           other;
020900110112             $Fine = *on;
021000110112
021100110112         endsl;
021200110112
021300110112       ENDDO;
021400110112
021500110112       // -?Operazioni finali?
021600110112       exsr  sr_RoutEnd;
021700110112
021800110112       //--------------------------------------------------------------
021900110112       //?Operazioni iniziali.                                         ?
022000110112       //--------------------------------------------------------------
022100110112       BEGSR  sr_RoutInz;
022200110112
022300110112         *inLR = *on;
022400110112
022500110112         // -?Reperimento dati job?
022600110112         exsr  sr_DatiJob;
022700110112
022800110112         // -?Impostazione nome programma a video?
022900110112         V1Tpgm = SDSpgm;
023000110112
023100110112         // -?Aggancio dati generali della tabella in esame?
023200110112         clear  k_TBEcod;
023300110112         k_TBEke1 = *zero;
023400110112         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
023500110112                = c_Tab;
023600110112         clear  k_TBEke2;
023700110112         clear  k_TBElin;
023800110112         k_TBEsif = KNSIF;
023900110112         chain(n)  %kds(k05tntbe01)  TNTBE000;
024000110112         if  not  %found(TNTBE01L);
024100110112           clear  k_TBEsif;
024200110112           chain(n)  %kds(k05tntbe01)  TNTBE000;
024300110112         endif;
024400110112         if  %found(TNTBE01L);
024500110112           xTNTBEds = TNTBEds;
024600110112         else;
024700110112           clear  xTNTBEds;
024800110112         endif;
024900110112
025000110112         // -?Impostazione iniziale / pulizia dei campi chiave?
025100110112         k_TBEcod = c_Tab;
025200110112         clear  k_TBEke1;
025300110112         clear  k_TBEke2;
025400110112         clear  k_TBElin;
025500110112         clear  k_TBEsif;
025600110112
025700110112       ENDSR;
025800110112
025900110112       //--------------------------------------------------------------
026000110112       //?Reperimento Dati del job (Utente/Operativi).                 ?
026100110112       //--------------------------------------------------------------
026200110112       BEGSR  sr_DatiJob;
026300110112
026400110112         in(E) §AzUte;
026500110112         if NOT %error;
026600110112           in(E) §DatiUte;
026700110112         endif;
026800110112         if %error or RSut = *blanks;
026900110112           clear TIBS34ds;
027000110112           tibs34r ( tibs34ds );
027100110112           in §AzUte;
027200110112           in §DatiUte;
027300110112         endif;
027400110112
027500110112       ENDSR;
027600110112
027700110112       //--------------------------------------------------------------
027800110112       //?Gestione videata D01.                                        ?
027900110112       //--------------------------------------------------------------
028000110112       BEGSR  sr_GesD01;
028100110112
028200110112         // -?Inizializzazione videata?
028300110112         if  $InzD01 = *on;
028400110112           exsr  sr_InzD01;
028500110112           $InzD01 = *off;
028600110112         endif;
028700110112         clear  V1Topz;
028800110112
028900110112         // -?Emissione videata?
029000110112         write  TB85T01;
029100110112         write  TB85P01;
029200110112         exfmt  TB85D01;
029300110112
029400110112         clear  V1Dmsg;
029500110112         reset  ErrMessage;
029600110112         reset  ErrGenerico;
029700110112
029800110112         SELECT;
029900110112
030000110112           // -?F3=Fine?
030100110112           WHEN  dsp_aid = c_F03;
030200110112             $Fine = *on;
030300110112
030400110112           // -?Invio?
030500110112           OTHER;
030600110112             exsr  sr_CtrD01;
030700110112             if  ErrGenerico = *off;
030800110112               $Video    = 'D2';
030900110112               $InzD02   = *on;
031000110112             endif;
031100110112
031200110112         ENDSL;
031300110112
031400110112       ENDSR;
031500110112
031600110112       //--------------------------------------------------------------
031700110112       //?Inizializzazione videata D01.                                ?
031800110112       //--------------------------------------------------------------
031900110112       BEGSR  sr_InzD01;
032000110112
032100110112         clear  TB85D01;
032200110112
032300110112         V1Cttr = '?';
032400110112
032500110112       ENDSR;
032600110112
032700110112       //--------------------------------------------------------------
032800110112       //?Controllo dati immessi nella videata D01.                    ?
032900110112       //--------------------------------------------------------------
033000110112       BEGSR  sr_CtrD01;
033100110112
033200110112         // -?Spegnimento indicatori di posizionamento cursore?
033300110112         %subst(IndDspF : 28) = *off;
033400110112
033500110112         // -?Codice Tipi Trattativa?
033600110112         Select;
033700110112
033800110112           // -?Ricerca?
033900110112           When  %scan( '?' : V1Cttr ) > *zero;
034000110112             reset  TIBS02ds;
034100110112             T02cod = 'TTR';
034200110112             T02sif = KNSIF;
034300110112             TNTBE_RicercaControllo  (kpjba : tibs02ds);
034400110112             if  T02err = *blank;
034500110112               V1Cttr = T02ke1;
034600110112             else;
034700110112               V1Dmsg = T02msg;
034800110112               errMessage = *on;
034900110112             endif;
035000110112             ErrGenerico = *on;
035100110112             leavesr;
035200110112
035300110112           // -?Controllo immissione?
035400110112           When  V1Cttr = *blank;
035500110112             V1Dmsg = $Msg(01);
035600110112             PosCurTTR   = *on;
035700110112             ErrMessage  = *on;
035800110112             ErrGenerico = *on;
035900110112             leavesr;
036000110112
036100110112         EndSl;
036200110112
036300110112       ENDSR;
036400110112
036500110112       //--------------------------------------------------------------
036600110112       //?Gestione videata D02.                                        ?
036700110112       //--------------------------------------------------------------
036800110112       BEGSR  sr_GesD02;
036900110112
037000110112         // -?Inizializzazione videata?
037100110112         if  $InzD02 = *on;
037200110112           exsr  sr_InzD02;
037300110112           $InzD02 = *off;
037400110112         endif;
037500110112
037600110112         // -?Emissione Testata e Piede con tasti funzionali abilitati?
037700110112         if  NOT ErrMessage;
037800110112           write  TB85T01;
037900110112           write  TB85D01;
038000110112         endif;
038100110112         write  Protect;
038200110112         write  TB85P02;
038300110112
038400110112         // -?Emissione videata?
038500110112         if  Not $Protect;
038600110112           exfmt  TB85D02;
038700110112         else;
038800110112           write  TB85D02;
038900110112           exfmt  Protect;
039000110112         endif;
039100110112
039200110112         clear  V1Dmsg;
039300110112         reset  ErrMessage;
039400110112         reset  ErrGenerico;
039500110112
039600110112         SELECT;
039700110112
039800110112           // -?F3=Fine?
039900110112           WHEN  dsp_aid = c_F03;
040000110112             unlock TNTBE01L;
040100110112             $Fine = *on;
040200110112
040300110112           // -?F12=Ritorno?
040400110112           WHEN  dsp_aid = c_F12;
040500110112             unlock TNTBE01L;
040600110112             reset  $Video;
040700110112             clear  V1Topz;
040800110112
040900110112           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
041000110112           OTHER;
041100110112             $Ripristino   = (dsp_aid = c_F05);
041200110112             $Annullamento = (dsp_aid = c_F16);
041300110112             // -?Controlli eseguiti NON se F16=Annullamento?
041400110112             if  Not $Annullamento;
041500110112               exsr  sr_CtrD02;
041600110112             // -?Controlli eseguiti SOLO se F16=Annullamento?
041700110112             //else;
041800110112             //  exsr  sr_CtrANN;
041900110112             endif;
042000110112             if  ErrGenerico;
042100110112               leavesr;
042200110112             endif;
042300110112             // -?Aggiornamento?
042400110112             if  dsp_aid = c_F05   or
042500110112                 dsp_aid = c_F06   or
042600110112                 dsp_aid = c_F16;
042700110112               $Video = 'W1';
042800110112               reset  $InzW01;
042900110112             endif;
043000110112
043100110112         ENDSL;
043200110112
043300110112       ENDSR;
043400110112
043500110112       //--------------------------------------------------------------
043600110112       //?Inizializzazione videata D02.                                ?
043700110112       //--------------------------------------------------------------
043800110112       BEGSR  sr_InzD02;
043900110112
044000110112         reset  $Immissione;
044100110112         reset  $Ripristino;
044200110112         reset  $Protect;
044300110112
044400110112         // -?Spegnimento degli indicatori di errore?
044500110112         %subst(IndDspF : 50) = *off;
044600110112
044700110112         // -?Pulizia videata?
044800110112         clear  TB85D02;
044900110112
045000110112         // -?Reperimento dati del Tipo Trattativa?
045100110112         k_TBEke1 = V1Cttr;
045200110112         clear  k_TBEke2;
045300110112         clear  k_TBElin;
045400110112         clear  k_TBEsif;
045500110112         chain  %kds(k05tntbe01)  TNTBE000;
045600110112
045700110112         // -?Caricamento dati del Tipo Trattativa?
045800110112         select;
045900110112           when  Not %found(TNTBE01L);
046000110112             $Immissione = *on;
046100110112           when  TBEatb <> *blank;
046200110112             $Ripristino = *on;
046300110112         endsl;
046400110112         if  %found(TNTBE01L);
046500110112           exsr  sr_RieD02;
046600110112         endif;
046700110112
046800110112         // -?Impostazione testata?
046900110112         //clear  V1Topz;              (già fatto in sr_GesD01)
047000110112         select;
047100110112           when  Not %found(TNTBE01L);
047200110112             V1Topz = '  INSERIMENTO  ';
047300110112           when  TBEatb <> *blank;
047400110112             V1Topz = '  RIPRISTINO   ';
047500110112           other;
047600110112             V1Topz = '   MODIFICA    ';
047700110112         endsl;
047800110112
047900110112         // -?Impostazione indicatori per abilitazione tasti funzionali?
048000110112         exsr  sr_AbilitFxxD02;
048100110112
048200110112       ENDSR;
048300110112
048400110112       //--------------------------------------------------------------
048500110112       //?Caricamento dati del singolo record della tab. "3EW"         ?
048600110112       //?nella videata D02.                                           ?
048700110112       //--------------------------------------------------------------
048800110112       BEGSR  sr_RieD02;
048900110112
049000110112         dTTR = TBEuni;
049100110112
049200110112         V1Cdes  = dTTR.§TTRdes;
049300110112         V1Cvcn  = dTTR.§TTRvcn;
049400110112         V1Coff  = dTTR.§TTRoff;
049500110112         V1Cord  = dTTR.§TTRord;
049600110112         V1Cinfo = dTTR.§TTRinfo;
049700110112         V1Cacc  = dTTR.§TTRacc;
049800110112
049900110112       ENDSR;
050000110112
050100110112       //--------------------------------------------------------------
050200110112       //?Settaggio indicatori nella videata D02 per abilitazione      ?
050300110112       //?  tasti funzionali.                                          ?
050400110112       //--------------------------------------------------------------
050500110112       BEGSR  sr_AbilitFxxD02;
050600110112
050700110112         // -?F5=Ripristino?
050800110112         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
050900110112                                         and   Not $Protect);
051000110112
051100110112         // -?F6=Conferma?
051200110112         F6Attivo  = (NOT F5Attivo);
051300110112
051400110112         // -?F16=Annullamento?
051500110112         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
051600110112                                         and   Not $Protect);
051700110112
051800110112       ENDSR;
051900110112
052000110112       //--------------------------------------------------------------
052100110112       //?Controllo dati videata D02.                                  ?
052200110112       //--------------------------------------------------------------
052300110112       BEGSR  sr_CtrD02;
052400110112
052500110112         // -?Spegnimento degli indicatori di errore?
052600110112         %subst(IndDspF : 50) = *off;
052700110112
052800110112         select;
052900110112
053000110112           // -?Descrizione obbligatoria?
053100110112           when  V1Cdes = *blank;
053200110112             V1Dmsg = $Msg(02);
053300110112             PosCurDES   = *on;
053400110112             ErrMessage  = *on;
053500110112             ErrGenerico = *on;
053600110112             leavesr;
053700110112
053800110113           // -?Flag "Trattativa per tipo cliente" obbligatorio?
053900110112           when  V1Cvcn = *blank;
054000110112             V1Dmsg = $Msg(03);
054100110112             PosCurVCN   = *on;
054200110112             ErrMessage  = *on;
054300110112             ErrGenerico = *on;
054400110112             leavesr;
054500110113           // -?Flag "Trattativa per tipo cliente" errato?
054600110112           when  V1Cvcn <> 'N'  and  V1Cvcn <> 'O';
054700110112             V1Dmsg = $Msg(04);
054800110112             PosCurVCN   = *on;
054900110112             ErrMessage  = *on;
055000110112             ErrGenerico = *on;
055100110112             leavesr;
055200110112
055300110113           // -?Flag "Trattativa con offerte" obbligatorio?
055400110112           when  V1Coff = *blank;
055500110112             V1Dmsg = $Msg(05);
055600110112             PosCurOFF   = *on;
055700110112             ErrMessage  = *on;
055800110112             ErrGenerico = *on;
055900110112             leavesr;
056000110113           // -?Flag "Trattativa con offerte" errato?
056100110112           when  V1Coff <> 'N'  and  V1Coff <> 'S';
056200110112             V1Dmsg = $Msg(06);
056300110112             PosCurOFF   = *on;
056400110112             ErrMessage  = *on;
056500110112             ErrGenerico = *on;
056600110112             leavesr;
056700110112
056800110112           // -?Ordinamento per elenco obbligatorio?
056900110112           when  V1Cord = *zero;
057000110112             V1Dmsg = $Msg(07);
057100110112             PosCurORD   = *on;
057200110112             ErrMessage  = *on;
057300110112             ErrGenerico = *on;
057400110112             leavesr;
057500110112           // -?Ordinamento per elenco errato?
057600110112           when  V1Cord < *zero;
057700110112             V1Dmsg = $Msg(08);
057800110112             PosCurORD   = *on;
057900110112             ErrMessage  = *on;
058000110112             ErrGenerico = *on;
058100110112             leavesr;
058200110112
058300110113           // -?Flag "Info trattativa" obbligatorio?
058400110112           when  V1Cinfo = *blank;
058500110112             V1Dmsg = $Msg(09);
058600110112             PosCurINFO  = *on;
058700110112             ErrMessage  = *on;
058800110112             ErrGenerico = *on;
058900110112             leavesr;
059000110113           // -?Flag "Info trattativa" errato?
059100110112           when  V1Cinfo <> 'N'  and  V1Cinfo <> 'S';
059200110112             V1Dmsg = $Msg(10);
059300110112             PosCurINFO  = *on;
059400110112             ErrMessage  = *on;
059500110112             ErrGenerico = *on;
059600110112             leavesr;
059700110112
059800110113           // -?Flag "Accettazione offerte" obbligatorio?
059900110112           when  V1Cacc = *blank;
060000110112             V1Dmsg = $Msg(11);
060100110112             PosCurACC   = *on;
060200110112             ErrMessage  = *on;
060300110112             ErrGenerico = *on;
060400110112             leavesr;
060500110113           // -?Flag "Accettazione offerte" errato?
060600110112           when  V1Cacc <> 'N'  and  V1Cacc <> 'S';
060700110112             V1Dmsg = $Msg(12);
060800110112             PosCurACC   = *on;
060900110112             ErrMessage  = *on;
061000110112             ErrGenerico = *on;
061100110112             leavesr;
061200110112
061300110112         endsl;
061400110112
061500110112       ENDSR;
061600110112
061700110112       //--------------------------------------------------------------
061800110112       //?Gestione trasmissione aggiornamento                          ?
061900110112       //--------------------------------------------------------------
062000110112       BEGSR  sr_GesW01;
062100110112
062200110112         // -?Inizializzazione videata?
062300110112         if $InzW01 = *on;
062400110112           exsr  sr_InzW01;
062500110112           $InzW01 = *off;
062600110112         endif;
062700110112
062800110112         // -?Emissione window?
062900110112         if  TBXftt = 'S';
063000110112           exfmt  TB85W01;
063100110112           ErrMessage  = *off;
063200110112           ErrGenerico = *off;
063300110112           clear  V1Dmsg;
063400110112         else;
063500110112           dsp_aid = c_F06;
063600110112         endif;
063700110112
063800110112         select;
063900110112
064000110112           // -?F12=Ritorno (gestito solo se emesso W01)?
064100110112           when  dsp_aid = c_F12;
064200110112             $Video = 'D2';
064300110112
064400110112           // -?F6=Conferma?
064500110112           when  dsp_aid = c_F06;
064600110112             exsr  sr_UpdTNTBE;
064700110112             // -?Ritorno alla videata iniziale?
064800110112             if  NOT  ErrGenerico;
064900110112               clear  V1Topz;
065000110112               $Video  = 'D1';
065100110112               $InzD01 = *on;
065200110112             endif;
065300110112         endsl;
065400110112
065500110112       ENDSR;
065600110112
065700110112       //--------------------------------------------------------------
065800110112       //?Inizializzazione videata W01                                 ?
065900110112       //--------------------------------------------------------------
066000110112       BEGSR  sr_InzW01;
066100110112
066200110112         clear TB85W01;
066300110112
066400110112         if  $Immissione;
066500110112
066600110112           // -?Se immissione?
066700110112           W1ftt  = TBXftt;
066800110112
066900110112         else;
067000110112
067100110112           // -?Se NON immissione:?
067200110112           //  ?visualizza i dati relativi all'ultima trasmissione?
067300110112           W1ftt  = TBEftt;
067400110112           W1flt  = TBEflt;
067500110112           W1ftr  = TBEftr;
067600110112           if TBEdtr <> *zero;
067700110112             wDate_EUR = %date(TBEdtr : *iso);
067800110112             W1dtr     = %dec(wDate_EUR);
067900110112           endif;
068000110112
068100110112         endif;
068200110112
068300110112       ENDSR;
068400110112
068500110112       //--------------------------------------------------------------
068600110112       //?Aggiornamento records TNTBE00F (tab. "TTR")                  ?
068700110112       //--------------------------------------------------------------
068800110112       BEGSR  sr_UpdTNTBE;
068900110112
069000110112         // -?Aggancio record di TNTBE00F?
069100110112         k_TBEcod  = c_Tab;
069200110112         k_TBEke1  = V1Cttr;
069300110112         clear  k_TBEke2;
069400110112         clear  k_TBElin;
069500110112         clear  k_TBEsif;
069600110112         //k_TBElin  = TBXlin;
069700110112         //k_TBEsif  = TBXsif;
069800110112
069900110112         chain  %kds(K05tntbe01) TNTBE000;
070000110112
070100110112         exsr  sr_RieTTR;
070200110112         TBEuni = dTTR;
070300110112
070400110112         // -?Aggiornamento tab. "TTR"?
070500110112         SELECT;
070600110112
070700110112           // -?F5=Ripristino?
070800110112           WHEN  $Ripristino;
070900110112             clear  TBEatb;
071000110112             TBEftt = W1ftt;
071100110112             clear  TBEftr;
071200110112             //_______________
071300110112             UPDATE  TNTBE000;
071400110112             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
071500110112
071600110112           // -?F16=Annullamento?
071700110112           WHEN  $Annullamento;
071800110112             TBEatb = 'A';
071900110112             TBEftt = W1ftt;
072000110112             clear  TBEftr;
072100110112             //_______________
072200110112             UPDATE  TNTBE000;
072300110112             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
072400110112
072500110112           // -?Immissione o Modifica?
072600110112           OTHER;
072700110112             if  NOT %found(TNTBE01L);
072800110112               clear TNTBE000;
072900110112               TBEcod  = k_TBEcod;
073000110112               TBEke1  = k_TBEke1;
073100110112               TBEke2  = k_TBEke2;
073200110112               TBElin  = k_TBElin;
073300110112               TBEsif  = k_TBEsif;
073400110112               TBEuni  = dTTR;
073500110112             endif;
073600110112             TBEapl = TBXapl;
073700110112             clear  TBEatb;
073800110112             TBEftt = W1ftt;
073900110112             clear  TBEflt;
074000110112             clear  TBEftr;
074100110112             //clear TBEdtr;
074200110112             // -?Aggiornamento record?
074300110112             if  %found(TNTBE01L);
074400110112               //_____________
074500110112               UPDATE TNTBE000;
074600110112               //¯¯¯¯¯¯¯¯¯¯¯¯¯
074700110112             else;
074800110112               //_____________
074900110112               WRITE  TNTBE000;
075000110112               //¯¯¯¯¯¯¯¯¯¯¯¯¯
075100110112             endif;
075200110112
075300110112         ENDSL;
075400110112
075500110112       ENDSR;
075600110112
075700110112       //--------------------------------------------------------------
075800110112       //?Caricamento dati in tab. "TTR"                               ?
075900110112       //--------------------------------------------------------------
076000110112       BEGSR  sr_RieTTR;
076100110112
076200110112         clear  dTTR;
076300110112
076400110112         dTTR.§TTRdes  = V1Cdes;
076500110112         dTTR.§TTRvcn  = V1Cvcn;
076600110112         dTTR.§TTRoff  = V1Coff;
076700110112         dTTR.§TTRord  = V1Cord;
076800110112         dTTR.§TTRinfo = V1Cinfo;
076900110112         dTTR.§TTRacc  = V1Cacc;
077000110112
077100110112       ENDSR;
077200110112
077300110112       //--------------------------------------------------------------
077400110112       //?Operazioni finali.                                           ?
077500110112       //--------------------------------------------------------------
077600110112       BEGSR  sr_RoutEnd;
077700110112
077800110112         // -?Chiusura funzioni precedentemente aperte?
077900110112         clear  TIBS02ds;
078000110112         T02tla = 'C';
078100110112         TNTBE_RicercaControllo ( kpjba : tibs02ds );
078200110112
078300110112         //  ?Uscita?
078400110112         return;
078500110112
078600110112       ENDSR;
078700110112
078800110112      /end-free
078900110112
079000110112       //--------------------------------------------------------------
079100110112       //?Definizione schiere a tempo di compilazione.                 ?
079200110112       //--------------------------------------------------------------
079300110112
079400110112** - $Msg:?Messaggi di Errore?-----------------------------------------------*
079500110112Immettere la Causale Contatto Marketing                                         1
079600110112Descrizione obbligatoria                                                        2
079700110113Flag "Trattativa per tipo cliente" obbligatorio                                 3
079800110113Flag "Trattativa per tipo cliente" errato                                       4
079900110113Flag "Trattativa con offerte" obbligatorio                                      5
080000110113Flag "Trattativa con offerte" errato                                            6
080100110112Ordinamento per Elanco obbligatorio                                             7
080200110112Ordinamento per Elanco errato                                                   8
080300110113Flag "Info Trattativa" obbligatorio                                             9
080400110113Flag "Info Trattativa" errato                                                  10
080500110113Flag "Accettazione offerte" obbligatorio                                       11
080600110113Flag "Accettazione offerte" errato                                             12
