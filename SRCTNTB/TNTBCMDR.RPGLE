000100140109       //==============================================================
000200140109       //?Gestione tabella "CMD" (Comandi Telefonata)                  ?
000300140109       //==============================================================
000400140109
000500140109       //--------------------------------------------------------------
000600140109       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700140109       //--------------------------------------------------------------
000800140109
000900140109     /*PRM  dbgview(*source)
001000140109     /*END
001100140109
001200140109       //--------------------------------------------------------------
001300140109       //?Specifiche di controllo.                                     ?
001400140109       //--------------------------------------------------------------
001500140109
001600140109     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700140109     h dftactgrp(*no)
001800140109
001900140109       //--------------------------------------------------------------
002000140109       //?Dichiarazione file.                                          ?
002100140109       //--------------------------------------------------------------
002200140109
002300140109       // -?Tabelle
002400140109     fTNTBE01L  Uf A e           k disk
002500140109
002600140109       // -?Video?
002700140109     fTNTBCMDD  cf   e             workstn
002800140109     f                                     indds(IndDspF)
002900140109     f                                     infds(InfDspF)
003000140109
003100140109       //--------------------------------------------------------------
003200140109       //?Definizione costanti.                                        ?
003300140109       //--------------------------------------------------------------
003400140109
003500140109       // -?Codice tabella in gestione?
003600140109     d c_Tab           c                   const('CMD')
003700140109
003800140109       // -?Tasti funzionali a video?
003900140109     d c_F01           c                   const(x'31')
004000140109     d c_F02           c                   const(x'32')
004100140109     d c_F03           c                   const(x'33')
004200140109     d c_F04           c                   const(x'34')
004300140109     d c_F05           c                   const(x'35')
004400140109     d c_F06           c                   const(x'36')
004500140109     d c_F07           c                   const(x'37')
004600140109     d c_F08           c                   const(x'38')
004700140109     d c_F09           c                   const(x'39')
004800140109     d c_F10           c                   const(x'3A')
004900140109     d c_F11           c                   const(x'3B')
005000140109     d c_F12           c                   const(x'3C')
005100140109     d c_F13           c                   const(x'B1')
005200140109     d c_F14           c                   const(x'B2')
005300140109     d c_F15           c                   const(x'B3')
005400140109     d c_F16           c                   const(x'B4')
005500140109     d c_F17           c                   const(x'B5')
005600140109     d c_F18           c                   const(x'B6')
005700140109     d c_F19           c                   const(x'B7')
005800140109     d c_F20           c                   const(x'B8')
005900140109     d c_F21           c                   const(x'B9')
006000140109     d c_F22           c                   const(x'BA')
006100140109     d c_F23           c                   const(x'BB')
006200140109     d c_F24           c                   const(x'BC')
006300140109     d c_Enter         c                   const(x'F1')
006400140109     d c_RollDown      c                   const(x'F4')
006500140109     d c_RollUp        c                   const(x'F5')
006600140109
006700140109       //--------------------------------------------------------------
006800140109       //?Definizione schiere.                                         ?
006900140109       //--------------------------------------------------------------
007000140109
007100140109       // -?Messaggi di errore?
007200140109     d sk_Msg          s             78    dim( 4)  ctdata  perrcd( 1)
007300140109
007400140109       //--------------------------------------------------------------
007500140109       //?Definizione aree dati.                                       ?
007600140109       //--------------------------------------------------------------
007700140109
007800140109       // -?Dati utente?
007900140109     d §AzUte        e ds                  extname(AZUTE00F)
008000140109     d                                     dtaara
008100140109     d §DatiUte      e ds                  extname(dDatiUte)
008200140109     d                                     dtaara
008300140109
008400140109       //--------------------------------------------------------------
008500140109       //?Definizione strutture dati.                                  ?
008600140109       //--------------------------------------------------------------
008700140109
008800140109       // -?Status ds?
008900140109     d Status         sds
009000140109     d   SDSpgm          *proc
009100140109
009200140109       // -?InfDS?
009300140109     d InfDspF         ds
009400140109     d   dsp_aid             369    369a
009500140109
009600140109       // -?Indicatori su DspF?
009700140109     d IndDspF         ds                  inz
009800140109         // -?Abilitazione tasti funzionali?
009900140109     d   F5Attivo                      n   overlay(IndDspF : 05)
010000140109     d   F6Attivo                      n   overlay(IndDspF : 06)
010100140109     d   F16Attivo                     n   overlay(IndDspF : 16)
010200140109         // -?Emissione messaggio di errore?
010300140109     d   ErrMessage                    n   overlay(IndDspF : 28)
010400140109         // -?Posizionamento cursore & segnalazione errore?
010500140109     d   PosCurCMD                     n   overlay(IndDspF : 50)
010600140109     d   PosCurDES                     n   overlay(IndDspF : 51)
010700140109     d   PosCurTIP                     n   overlay(IndDspF : 52)
010800140109     d   PosCurUTUT                    n   overlay(IndDspF : 53)
010900140109         // -?Emissione messaggio di errore in window W1?
011000140109     d   ErrMessageW1                  n   overlay(IndDspF : 90)
011100140109         // -?Riemissione videata?
011200140109     d   ErrGenerico                   n   overlay(IndDspF : 99)
011300140109
011400140109       // -?Parametri ricevuti?
011500140109     d KPJBA         e ds
011600140109
011700140109       // -?Dati record principale della tabella?
011800140109     d TNTBEds       e ds                  inz  extname(TNTBE00F)
011900140109     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012000140109     d                                          prefix(TBX:3)
012100140109
012200140109       // -?Tabelle usate:?
012300140109       // ·?"CMD" = Comandi Telefonate?
012400140109     d dCMD          e ds                  inz  qualified
012500140109
012600140109       //--------------------------------------------------------------
012700140109       //?Definizione variabili globali.                               ?
012800140109       //--------------------------------------------------------------
012900140109
013000140109       // -?Flags booleani?
013100140109     d $Fine           s               n   inz
013200140109     d $InzD01         s               n   inz(*on)
013300140109     d $InzD02         s               n   inz(*on)
013400140109     d $InzW01         s               n   inz(*on)
013500140109     d $Annullamento   s               n   inz
013600140109     d $Immissione     s               n   inz
013700140109     d $Ripristino     s               n   inz
013800140109     d $Protect        s               n   inz
013900140109
014000140109       // -?Variabili per la gestione del video?
014100140109     d $Video          s              2    inz('D1')
014200140109
014300140109       // -?Campi di comodo?
014400140109     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
014500140109
014600140109       //--------------------------------------------------------------
014700140109       //?Definizione prototipi procedure.                             ?
014800140109       //--------------------------------------------------------------
014900140109
015000140109       // -?Reperimento dati utente?
015100140109     d TIBS34ds      e ds
015200140109      /copy gaitrasrc/srcProtoPR,TIBS34R
015300140109
015400140109       // -?Ricerca e controllo nuove tabelle?
015500140109     d TIBS02ds      e ds                  inz
015600140109     d   T02mod      e                     inz('C')
015700140109      /copy gaitrasrc/srcProtoPR,TIBS02R
015800140109
015900140109       // -?Ricerca tab. "CMD" = Comandi Telefonate?
016000140109     d tntbCMDds1    e ds                  inz
016100140109     d   iCMDutUt    e                     inz('E')
016200140109      /copy gaitrasrc/srcProtoPR,TNTBCMDR1
016300140109
016400140109       //--------------------------------------------------------------
016500140109       //?Definizione key-list.                                        ?
016600140109       //--------------------------------------------------------------
016700140109
016800140109       // -?File TNTBE01L?
016900140109     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
017000140109     d                                     prefix(k_)   inz
017100140109
017200140109       //--------------------------------------------------------------
017300140109       //?Riepilogo indicatori utilizzati.                             ?
017400140109       //--------------------------------------------------------------
017500140109       //--------------------------------------------------------------
017600140109
017700140109       //--------------------------------------------------------------
017800140109       //?M A I N - L I N E                                            ?
017900140109       //--------------------------------------------------------------
018000140109
018100140109     c     *Entry        plist
018200140109     c                   parm                    KPJBA
018300140109
018400140109      /free
018500140109
018600140109       // -?Operazioni iniziali?
018700140109       exsr  sr_RoutInz;
018800140109
018900140109       // -?Ciclo di gestione del file video?
019000140109       DOW  $Fine = *off;
019100140109
019200140109         select;
019300140109
019400140109           // -?Richiesta chiave di accesso?
019500140109           when $Video = 'D1';
019600140109             exsr  sr_GesD01;
019700140109
019800140109           // -?Manutenzione dati della singola tabella?
019900140109           when $Video = 'D2';
020000140109             exsr  sr_GesD02;
020100140109
020200140109           // -?Richiesta dati per trasmissione record?
020300140109           when $Video = 'W1';
020400140109             exsr  sr_GesW01;
020500140109
020600140109           // -?? ? ??
020700140109           other;
020800140109             $Fine = *on;
020900140109
021000140109         endsl;
021100140109
021200140109       ENDDO;
021300140109
021400140109       // -?Operazioni finali?
021500140109       exsr  sr_RoutEnd;
021600140109
021700140109       //--------------------------------------------------------------
021800140109       //?Operazioni iniziali.                                         ?
021900140109       //--------------------------------------------------------------
022000140109       BEGSR  sr_RoutInz;
022100140109
022200140109         *inLR = *on;
022300140109
022400140109         // -?Reperimento dati job?
022500140109         exsr  sr_DatiJob;
022600140109
022700140109         // -?Impostazione nome programma a video?
022800140109         V1Tpgm = SDSpgm;
022900140109
023000140109         // -?Aggancio dati generali della tabella in esame?
023100140109         clear  keyTNTBE01;
023200140109         k_TBEke1 = *zero;
023300140109         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
023400140109                = c_Tab;
023500140109         k_TBEsif = KNSIF;
023600140109         chain(n)  %kds(keyTNTBE01)  TNTBE000;
023700140109         if  not  %found(TNTBE01L);
023800140109           clear  k_TBEsif;
023900140109           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
024000140109         endif;
024100140109         if  %found(TNTBE01L);
024200140109           xTNTBEds = TNTBEds;
024300140109         else;
024400140109           clear  xTNTBEds;
024500140109         endif;
024600140109
024700140109         // -?Impostazione iniziale / pulizia dei campi chiave?
024800140109         clear  keyTNTBE01;
024900140109         k_TBEcod = c_Tab;
025000140109
025100140109       ENDSR;
025200140109
025300140109       //--------------------------------------------------------------
025400140109       //?Reperimento Dati del job (Utente/Operativi).                 ?
025500140109       //--------------------------------------------------------------
025600140109       BEGSR  sr_DatiJob;
025700140109
025800140109         in(E) §AzUte;
025900140109         if NOT %error;
026000140109           in(E) §DatiUte;
026100140109         endif;
026200140109         if %error or RSut = *blanks;
026300140109           clear TIBS34ds;
026400140109           tibs34r ( tibs34ds );
026500140109           in §AzUte;
026600140109           in §DatiUte;
026700140109         endif;
026800140109
026900140109       ENDSR;
027000140109
027100140109       //--------------------------------------------------------------
027200140109       //?Gestione videata D01.                                        ?
027300140109       //--------------------------------------------------------------
027400140109       BEGSR  sr_GesD01;
027500140109
027600140109         // -?Inizializzazione videata?
027700140109         if  $InzD01 = *on;
027800140109           exsr  sr_InzD01;
027900140109           $InzD01 = *off;
028000140109         endif;
028100140109         clear  V1Topz;
028200140109
028300140109         // -?Emissione videata?
028400140109         write  TBCMDT01;
028500140109         write  TBCMDP01;
028600140109         exfmt  TBCMDD01;
028700140109
028800140109         clear  V1Dmsg;
028900140109         reset  ErrMessage;
029000140109         reset  ErrGenerico;
029100140109
029200140109         SELECT;
029300140109
029400140109           // -?F3=Fine?
029500140109           WHEN  dsp_aid = c_F03;
029600140109             $Fine = *on;
029700140109
029800140109           // -?Invio?
029900140109           OTHER;
030000140109             exsr  sr_CtrD01;
030100140109             if  ErrGenerico = *off;
030200140109               $Video    = 'D2';
030300140109               $InzD02   = *on;
030400140109             endif;
030500140109
030600140109         ENDSL;
030700140109
030800140109       ENDSR;
030900140109
031000140109       //--------------------------------------------------------------
031100140109       //?Inizializzazione videata D01.                                ?
031200140109       //--------------------------------------------------------------
031300140109       BEGSR  sr_InzD01;
031400140109
031500140109         clear  TBCMDD01;
031600140109
031700140109         V1Ccmd = '?';
031800140109
031900140109       ENDSR;
032000140109
032100140109       //--------------------------------------------------------------
032200140109       //?Controllo dati immessi nella videata D01.                    ?
032300140109       //--------------------------------------------------------------
032400140109       BEGSR  sr_CtrD01;
032500140109
032600140109         // -?Spegnimento indicatori di posizionamento cursore?
032700140109         %subst(IndDspF : 28) = *off;
032800140109
032900140109         // -?Motivi Apertura R.A.?
033000140109         Select;
033100140109
033200140109           // -?Ricerca?
033300140109           When  %scan( '?' : V1Ccmd ) > *zero;
033400140109             reset  tntbCMDds1;
033500140110             //iCMDtip  = *blank;           //?(già così)?
033600140109             //iCMDutUt = 'E';              //?(già così)?
033700140109             kpjbu = tntbCMDds1;
033800140109             tntbCMDr1 ( kpjba );
033900140109             tntbCMDds1 = kpjbu;
034000140109             select;
034100140109               when  oCMDfxx <> *blank;
034200140109               when  oCMDerr =  *on;
034300140109                 V1Dmsg = oCMDmsg;
034400140109                 PosCurCMD   = *on;
034500140109                 errMessage  = *on;
034600140109               other;
034700140109                 V1Ccmd = oCMDcmd;
034800140109             endsl;
034900140109             ErrGenerico = *on;
035000140109             leavesr;
035100140109
035200140109           // -?Controllo immissione?
035300140109           When  V1Ccmd = *blank;
035400140109             V1Dmsg = sk_Msg(01);
035500140109             PosCurCMD   = *on;
035600140109             ErrMessage  = *on;
035700140109             ErrGenerico = *on;
035800140109             leavesr;
035900140109
036000140109         EndSl;
036100140109
036200140109       ENDSR;
036300140109
036400140109       //--------------------------------------------------------------
036500140109       //?Gestione videata D02.                                        ?
036600140109       //--------------------------------------------------------------
036700140109       BEGSR  sr_GesD02;
036800140109
036900140109         // -?Inizializzazione videata?
037000140109         if  $InzD02 = *on;
037100140109           exsr  sr_InzD02;
037200140109           $InzD02 = *off;
037300140109         endif;
037400140109
037500140109         // -?Emissione Testata e Piede con tasti funzionali abilitati?
037600140109         if  NOT ErrMessage;
037700140109           write  TBCMDT01;
037800140109           write  TBCMDD01;
037900140109         endif;
038000140109         write  Protect;
038100140109         write  TBCMDP02;
038200140109
038300140109         // -?Emissione videata?
038400140109         if  Not $Protect;
038500140109           exfmt  TBCMDD02;
038600140109         else;
038700140109           write  TBCMDD02;
038800140109           exfmt  Protect;
038900140109         endif;
039000140109
039100140109         clear  V1Dmsg;
039200140109         reset  ErrMessage;
039300140109         reset  ErrGenerico;
039400140109
039500140109         SELECT;
039600140109
039700140109           // -?F3=Fine?
039800140109           WHEN  dsp_aid = c_F03;
039900140109             unlock TNTBE01L;
040000140109             $Fine = *on;
040100140109
040200140109           // -?F12=Ritorno?
040300140109           WHEN  dsp_aid = c_F12;
040400140109             unlock TNTBE01L;
040500140109             reset  $Video;
040600140109             clear  V1Topz;
040700140109
040800140109           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
040900140109           OTHER;
041000140109             $Ripristino   = (dsp_aid = c_F05);
041100140109             $Annullamento = (dsp_aid = c_F16);
041200140109             // -?Controlli eseguiti NON se F16=Annullamento?
041300140109             if  Not $Annullamento;
041400140109               exsr  sr_CtrD02;
041500140109             // -?Controlli eseguiti SOLO se F16=Annullamento?
041600140109             //else;
041700140109             //  exsr  sr_CtrANN;
041800140109             endif;
041900140109             if  ErrGenerico;
042000140109               leavesr;
042100140109             endif;
042200140109             // -?Aggiornamento?
042300140109             if  dsp_aid = c_F05   or
042400140109                 dsp_aid = c_F06   or
042500140109                 dsp_aid = c_F16;
042600140109               $Video = 'W1';
042700140109               reset  $InzW01;
042800140109             endif;
042900140109
043000140109         ENDSL;
043100140109
043200140109       ENDSR;
043300140109
043400140109       //--------------------------------------------------------------
043500140109       //?Inizializzazione videata D02.                                ?
043600140109       //--------------------------------------------------------------
043700140109       BEGSR  sr_InzD02;
043800140109
043900140109         reset  $Immissione;
044000140109         reset  $Ripristino;
044100140109         reset  $Protect;
044200140109
044300140109         // -?Spegnimento degli indicatori di errore?
044400140109         %subst(IndDspF : 50) = *off;
044500140109
044600140109         // -?Pulizia videata?
044700140109         clear  TBCMDD02;
044800140109
044900140109         // -?Reperimento dati del Comando Telefonata?
045000140109         k_TBEke1 = V1Ccmd;
045100140109         chain  %kds(KeyTNTBE01)  TNTBE000;
045200140109
045300140109         // -?Impostazione testata?
045400140109         Select;
045500140109           When  Not %found(TNTBE01L);
045600140109             V1Topz = '  INSERIMENTO  ';
045700140109             $Immissione = *on;
045800140109           When  TBEatb <> *blank;
045900140109             V1Topz = '  RIPRISTINO   ';
046000140109             $Ripristino = *on;
046100140109           Other;
046200140109             V1Topz = '   MODIFICA    ';
046300140109         EndSl;
046400140109
046500140109         // -?Caricamento dati del Codice?
046600140109         if  %found(TNTBE01L);
046700140109           exsr  sr_RieD02;
046800140109         endif;
046900140109
047000140109         // -?Impostazione indicatori per abilitazione tasti funzionali?
047100140109         exsr  sr_AbilitFxxD02;
047200140109
047300140109       ENDSR;
047400140109
047500140109       //--------------------------------------------------------------
047600140109       //?Caricamento dati del singolo record della tab. "CMD"         ?
047700140109       //?nella videata D02.                                           ?
047800140109       //--------------------------------------------------------------
047900140109       BEGSR  sr_RieD02;
048000140109
048100140109         dCMD = TBEuni;
048200140109
048300140109         V1Dcmd  = dCMD.§CMDdes;
048400140109         V1Ctip  = dCMD.§CMDtip;
048500140109         V1CutUt = dCMD.§CMDutUt;
048600140109
048700140109       ENDSR;
048800140109
048900140109       //--------------------------------------------------------------
049000140109       //?Settaggio indicatori nella videata D02 per abilitazione      ?
049100140109       //?  tasti funzionali.                                          ?
049200140109       //--------------------------------------------------------------
049300140109       BEGSR  sr_AbilitFxxD02;
049400140109
049500140109         // -?F5=Ripristino?
049600140109         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
049700140109                                         and   Not $Protect);
049800140109
049900140109         // -?F6=Conferma?
050000140109         F6Attivo  = (NOT F5Attivo);
050100140109
050200140109         // -?F16=Annullamento?
050300140109         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
050400140109                                         and   Not $Protect);
050500140109
050600140109       ENDSR;
050700140109
050800140109       //--------------------------------------------------------------
050900140109       //?Controllo dati videata D02.                                  ?
051000140109       //--------------------------------------------------------------
051100140109       BEGSR  sr_CtrD02;
051200140109
051300140109         // -?Spegnimento degli indicatori di errore?
051400140109         %subst(IndDspF : 50) = *off;
051500140109
051600140109         select;
051700140109
051800140109           // -?Descrizione obbligatoria?
051900140109           when  V1Dcmd = *blank;
052000140109             V1Dmsg = sk_Msg(02);
052100140109             PosCurDES   = *on;
052200140109             ErrMessage  = *on;
052300140109             ErrGenerico = *on;
052400140109             leavesr;
052500140109
052600140109           // -?Tipologia Comando?
052700140109           when  V1Ctip <> 'C'  and
052800140109                 V1Ctip <> 'R'  and
052900140109                 V1Ctip <> 'E';
053000140109             V1Dmsg = sk_Msg(03);
053100140109             PosCurTIP   = *on;
053200140109             ErrMessage  = *on;
053300140109             ErrGenerico = *on;
053400140109             leavesr;
053500140109
053600140109         endsl;
053700140109
053800140109         // -?Comando uilizzabile dall'utente?
053900140109         if  V1CutUt <> *blank  and  V1CutUt <> 'N';
054000140109           V1Dmsg = sk_Msg(04);
054100140109           PosCurUTUT  = *on;
054200140109           ErrMessage  = *on;
054300140109           ErrGenerico = *on;
054400140109           leavesr;
054500140109         endif;
054600140109
054700140109       ENDSR;
054800140109
054900140109       //--------------------------------------------------------------
055000140109       //?Gestione trasmissione aggiornamento                          ?
055100140109       //--------------------------------------------------------------
055200140109       BEGSR  sr_GesW01;
055300140109
055400140109         // -?Inizializzazione videata?
055500140109         if $InzW01 = *on;
055600140109           exsr  sr_InzW01;
055700140109           $InzW01 = *off;
055800140109         endif;
055900140109
056000140109         // -?Emissione window?
056100140109         if  TBXftt = 'S';
056200140109           exfmt  TBCMDW01;
056300140109           ErrMessage  = *off;
056400140109           ErrGenerico = *off;
056500140109           clear  V1Dmsg;
056600140109         else;
056700140109           dsp_aid = c_F06;
056800140109         endif;
056900140109
057000140109         select;
057100140109
057200140109           // -?F12=Ritorno (gestito solo se emesso W01)?
057300140109           when  dsp_aid = c_F12;
057400140109             $Video = 'D2';
057500140109
057600140109           // -?F6=Conferma?
057700140109           when  dsp_aid = c_F06;
057800140109             exsr  sr_UpdTNTBE;
057900140109             // -?Ritorno alla videata iniziale?
058000140109             if  NOT  ErrGenerico;
058100140109               clear  V1Topz;
058200140109               $Video  = 'D1';
058300140109               $InzD01 = *on;
058400140109             endif;
058500140109         endsl;
058600140109
058700140109       ENDSR;
058800140109
058900140109       //--------------------------------------------------------------
059000140109       //?Inizializzazione videata W01                                 ?
059100140109       //--------------------------------------------------------------
059200140109       BEGSR  sr_InzW01;
059300140109
059400140109         clear TBCMDW01;
059500140109
059600140109         if  $Immissione;
059700140109
059800140109           // -?Se immissione?
059900140109           W1ftt  = TBXftt;
060000140109
060100140109         else;
060200140109
060300140109           // -?Se NON immissione:?
060400140109           //  ?visualizza i dati relativi all'ultima trasmissione?
060500140109           W1ftt  = TBEftt;
060600140109           W1flt  = TBEflt;
060700140109           W1ftr  = TBEftr;
060800140109           if TBEdtr <> *zero;
060900140109             wDate_EUR = %date(TBEdtr : *iso);
061000140109             W1dtr     = %dec(wDate_EUR);
061100140109           endif;
061200140109
061300140109         endif;
061400140109
061500140109       ENDSR;
061600140109
061700140109       //--------------------------------------------------------------
061800140109       //?Aggiornamento records TNTBE00F (tab. "CMD")                  ?
061900140109       //--------------------------------------------------------------
062000140109       BEGSR  sr_UpdTNTBE;
062100140109
062200140109         // -?Aggancio record di TNTBE00F?
062300140109         k_TBEcod  = c_Tab;
062400140109         k_TBEke1  = V1Ccmd;
062500140109         clear  k_TBEke2;
062600140109         clear  k_TBElin;
062700140109         clear  k_TBEsif;
062800140109
062900140109         chain  %kds(KeyTNTBE01) TNTBE000;
063000140109
063100140109         exsr  sr_RieCMD;
063200140109         TBEuni = dCMD;
063300140109
063400140109         // -?Aggiornamento tab. "CMD"?
063500140109         SELECT;
063600140109
063700140109           // -?F5=Ripristino?
063800140109           WHEN  $Ripristino;
063900140109             clear  TBEatb;
064000140109             TBEftt = W1ftt;
064100140109             clear  TBEftr;
064200140109             //_______________
064300140109             UPDATE  TNTBE000;
064400140109             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
064500140109
064600140109           // -?F16=Annullamento?
064700140109           WHEN  $Annullamento;
064800140109             TBEatb = 'A';
064900140109             TBEftt = W1ftt;
065000140109             clear  TBEftr;
065100140109             //_______________
065200140109             UPDATE  TNTBE000;
065300140109             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
065400140109
065500140109           // -?Immissione o Modifica?
065600140109           OTHER;
065700140109             if  NOT %found(TNTBE01L);
065800140109               clear TNTBE000;
065900140109               TBEcod  = k_TBEcod;
066000140109               TBEke1  = k_TBEke1;
066100140109               TBEke2  = k_TBEke2;
066200140109               TBElin  = k_TBElin;
066300140109               TBEsif  = k_TBEsif;
066400140109               TBEuni  = dCMD;
066500140109             endif;
066600140109             TBEapl = TBXapl;
066700140109             clear  TBEatb;
066800140109             TBEftt = W1ftt;
066900140109             clear  TBEflt;
067000140109             clear  TBEftr;
067100140109             //clear TBEdtr;
067200140109             // -?Aggiornamento record?
067300140109             if  %found(TNTBE01L);
067400140109               //_____________
067500140109               UPDATE TNTBE000;
067600140109               //¯¯¯¯¯¯¯¯¯¯¯¯¯
067700140109             else;
067800140109               //_____________
067900140109               WRITE  TNTBE000;
068000140109               //¯¯¯¯¯¯¯¯¯¯¯¯¯
068100140109             endif;
068200140109
068300140109         ENDSL;
068400140109
068500140109       ENDSR;
068600140109
068700140109       //--------------------------------------------------------------
068800140109       //?Caricamento dati in tab. "CMD"                               ?
068900140109       //--------------------------------------------------------------
069000140109       BEGSR  sr_RieCMD;
069100140109
069200140109         clear  dCMD;
069300140109
069400140109         dCMD.§CMDdes  = V1Dcmd;
069500140109         dCMD.§CMDtip  = V1Ctip;
069600140109         dCMD.§CMDutUt = V1CutUt;
069700140109
069800140109       ENDSR;
069900140109
070000140109       //--------------------------------------------------------------
070100140109       //?Operazioni finali.                                           ?
070200140109       //--------------------------------------------------------------
070300140109       BEGSR  sr_RoutEnd;
070400140109
070500140109         // -?Chiusura funzioni precedentemente aperte?
070600140109         clear  TIBS02ds;
070700140109         T02tla = 'C';
070800140109         TNTBE_RicercaControllo ( kpjba : tibs02ds );
070900140109
071000140109         //  ?Uscita?
071100140109         return;
071200140109
071300140109       ENDSR;
071400140109
071500140109      /end-free
071600140109
071700140109       //--------------------------------------------------------------
071800140109       //?Definizione schiere a tempo di compilazione.                 ?
071900140109       //--------------------------------------------------------------
072000140109
072100140109** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
072200140109Immettere il Comando Telefonata                                                 1
072300140109Descrizione obbligatoria                                                        2
072400140109Tipologia Comando errata                                                        3
072500140109Flag "Comando utilizzabile dell'utente" errato                                  4
