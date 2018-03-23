000100110112       //==============================================================
000200120919       //?Gestione tabella "MAR" (Motivi Apertura R.A.)                ?
000300110112       //==============================================================
000400130513
000500130513       //--------------------------------------------------------------
000600130513       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130513       //--------------------------------------------------------------
000800130513
000900130513     /*PRM  dbgview(*source)
001000130513     /*END
001100110112
001200110112       //--------------------------------------------------------------
001300110112       //?Specifiche di controllo.                                     ?
001400110112       //--------------------------------------------------------------
001500110112
001600110112     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700110112     h dftactgrp(*no)
001800110112
001900110112       //--------------------------------------------------------------
002000110112       //?Dichiarazione file.                                          ?
002100110112       //--------------------------------------------------------------
002200110112
002300110112       // -?Tabelle?
002400110112     fTNTBE01L  Uf A e           k disk
002500110112
002600110112       // -?Video?
002700120919     fTNTBMARD  cf   e             workstn
002800110112     f                                     indds(IndDspF)
002900110112     f                                     infds(InfDspF)
003000110112
003100110112       //--------------------------------------------------------------
003200110112       //?Definizione costanti.                                        ?
003300110112       //--------------------------------------------------------------
003400110112
003500110112       // -?Codice tabella in gestione?
003600120919     d c_Tab           c                   const('MAR')
003700110112
003800110112       // -?Tasti funzionali a video?
003900110112     d c_F01           c                   const(x'31')
004000110112     d c_F02           c                   const(x'32')
004100110112     d c_F03           c                   const(x'33')
004200110112     d c_F04           c                   const(x'34')
004300110112     d c_F05           c                   const(x'35')
004400110112     d c_F06           c                   const(x'36')
004500110112     d c_F07           c                   const(x'37')
004600110112     d c_F08           c                   const(x'38')
004700110112     d c_F09           c                   const(x'39')
004800110112     d c_F10           c                   const(x'3A')
004900110112     d c_F11           c                   const(x'3B')
005000110112     d c_F12           c                   const(x'3C')
005100110112     d c_F13           c                   const(x'B1')
005200110112     d c_F14           c                   const(x'B2')
005300110112     d c_F15           c                   const(x'B3')
005400110112     d c_F16           c                   const(x'B4')
005500110112     d c_F17           c                   const(x'B5')
005600110112     d c_F18           c                   const(x'B6')
005700110112     d c_F19           c                   const(x'B7')
005800110112     d c_F20           c                   const(x'B8')
005900110112     d c_F21           c                   const(x'B9')
006000110112     d c_F22           c                   const(x'BA')
006100110112     d c_F23           c                   const(x'BB')
006200110112     d c_F24           c                   const(x'BC')
006300110112     d c_Enter         c                   const(x'F1')
006400110112     d c_RollDown      c                   const(x'F4')
006500110112     d c_RollUp        c                   const(x'F5')
006600110112
006700110112       //--------------------------------------------------------------
006800110112       //?Definizione schiere.                                         ?
006900110112       //--------------------------------------------------------------
007000110112
007100110112       // -?Messaggi di errore?
007200140416     d $Msg            s             78    dim( 9)  ctdata  perrcd( 1)
007300110112
007400110112       //--------------------------------------------------------------
007500110112       //?Definizione aree dati.                                       ?
007600110112       //--------------------------------------------------------------
007700110112
007800110112       // -?Dati utente?
007900110112     d §AzUte        e ds                  extname(AZUTE00F)
008000110112     d                                     dtaara
008100110112     d §DatiUte      e ds                  extname(dDatiUte)
008200110112     d                                     dtaara
008300110112
008400110112       //--------------------------------------------------------------
008500110112       //?Definizione strutture dati.                                  ?
008600110112       //--------------------------------------------------------------
008700110112
008800110112       // -?Status ds?
008900110112     d Status         sds
009000140415     d   SDSpgm          *proc
009100110112
009200110112       // -?InfDS?
009300110112     d InfDspF         ds
009400110112     d   dsp_aid             369    369a
009500110112
009600110112       // -?Indicatori su DspF?
009700110112     d IndDspF         ds                  inz
009800110112         // -?Abilitazione tasti funzionali?
009900110112     d   F5Attivo                      n   overlay(IndDspF : 05)
010000110112     d   F6Attivo                      n   overlay(IndDspF : 06)
010100110112     d   F10Attivo                     n   overlay(IndDspF : 10)
010200110112     d   F16Attivo                     n   overlay(IndDspF : 16)
010300110112         // -?Emissione messaggio di errore?
010400110112     d   ErrMessage                    n   overlay(IndDspF : 28)
010500110112         // -?Posizionamento cursore & segnalazione errore?
010600120919     d   PosCurMAR                     n   overlay(IndDspF : 50)
010700110112     d   PosCurDES                     n   overlay(IndDspF : 51)
010800120919     d   PosCurMOTCH                   n   overlay(IndDspF : 52)
010900130513     d   PosCurTOR                     n   overlay(IndDspF : 53)
011000130513     d   PosCurFFR                     n   overlay(IndDspF : 54)
011100131015     d   PosCurNOTLDV                  n   overlay(IndDspF : 55)
011200140416     d   PosCurUTAP                    n   overlay(IndDspF : 56)
011300150114     d   PosCurCAUCHP                  n   overlay(IndDspF : 57)
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
012800130513       // ·?"MAR" = Motivi apertura R.A.?
012900120919     d dMAR          e ds                  inz  qualified
013000130513       // ·?"CHR" = Codice chiusura Reclami?
013100120919     d dCHR          e ds                  inz  qualified
013200130513       // ·?"OGR" = Oggetto Richiesta?
013300130513     d dOGR          e ds                  inz  qualified
013400110112
013500110112       //--------------------------------------------------------------
013600110112       //?Definizione variabili globali.                               ?
013700110112       //--------------------------------------------------------------
013800110112
013900110112       // -?Flags booleani?
014000110112     d $Fine           s               n   inz
014100110112     d $InzD01         s               n   inz(*on)
014200110112     d $InzD02         s               n   inz(*on)
014300110112     d $InzW01         s               n   inz(*on)
014400110112     d $Annullamento   s               n   inz
014500110112     d $Immissione     s               n   inz
014600110112     d $Ripristino     s               n   inz
014700110112     d $Protect        s               n   inz
014800110112
014900110112       // -?Variabili per la gestione del video?
015000110112     d $Video          s              2    inz('D1')
015100110112
015200110112       // -?Campi di comodo?
015300110112     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015400110112
015500110112       //--------------------------------------------------------------
015600110112       //?Definizione prototipi procedure.                             ?
015700110112       //--------------------------------------------------------------
015800110112
015900110112       // -?Reperimento dati utente?
016000110112     d TIBS34ds      e ds
016100110112      /copy gaitrasrc/srcProtoPR,TIBS34R
016200110112
016300110112       // -?Ricerca e controllo nuove tabelle?
016400110112     d TIBS02ds      e ds                  inz
016500131018     d   T02mod      e                     inz('C')
016600110112      /copy gaitrasrc/srcProtoPR,TIBS02R
016700131018
016800131018       // -?Ricerca tab. "MAR" = Motivo Apertura Richiesta Assistenza?
016900131018     d tntbMARds1    e ds                  inz
017000131018      /copy gaitrasrc/srcProtoPR,TNTBMARR1
017100110112
017200110112       //--------------------------------------------------------------
017300110112       //?Definizione key-list.                                        ?
017400110112       //--------------------------------------------------------------
017500110112
017600110112       // -?File TNTBE01L?
017700110112     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017800110112     d                                     prefix(k_)   inz
017900110112
018000110112       //--------------------------------------------------------------
018100110112       //?Riepilogo indicatori utilizzati.                             ?
018200110112       //--------------------------------------------------------------
018300110112       //--------------------------------------------------------------
018400110112
018500110112       //--------------------------------------------------------------
018600110112       //?M A I N - L I N E                                            ?
018700110112       //--------------------------------------------------------------
018800110112
018900110112     c     *Entry        plist
019000110112     c                   parm                    KPJBA
019100110112
019200110112      /free
019300110112
019400110112       // -?Operazioni iniziali?
019500110112       exsr  sr_RoutInz;
019600110112
019700110112       // -?Ciclo di gestione del file video?
019800110112       DOW  $Fine = *off;
019900110112
020000110112         select;
020100110112
020200110112           // -?Richiesta chiave di accesso?
020300110112           when $Video = 'D1';
020400110112             exsr  sr_GesD01;
020500110112
020600110112           // -?Manutenzione dati della singola tabella?
020700110112           when $Video = 'D2';
020800110112             exsr  sr_GesD02;
020900110112
021000110112           // -?Richiesta dati per trasmissione record?
021100110112           when $Video = 'W1';
021200110112             exsr  sr_GesW01;
021300110112
021400110112           // -?? ? ??
021500110112           other;
021600110112             $Fine = *on;
021700110112
021800110112         endsl;
021900110112
022000110112       ENDDO;
022100110112
022200110112       // -?Operazioni finali?
022300110112       exsr  sr_RoutEnd;
022400110112
022500110112       //--------------------------------------------------------------
022600110112       //?Operazioni iniziali.                                         ?
022700110112       //--------------------------------------------------------------
022800110112       BEGSR  sr_RoutInz;
022900110112
023000110112         *inLR = *on;
023100110112
023200110112         // -?Reperimento dati job?
023300110112         exsr  sr_DatiJob;
023400110112
023500110112         // -?Impostazione nome programma a video?
023600110112         V1Tpgm = SDSpgm;
023700110112
023800110112         // -?Aggancio dati generali della tabella in esame?
023900110112         clear  k_TBEcod;
024000110112         k_TBEke1 = *zero;
024100110112         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
024200110112                = c_Tab;
024300110112         clear  k_TBEke2;
024400110112         clear  k_TBElin;
024500110112         k_TBEsif = KNSIF;
024600110112         chain(n)  %kds(k05tntbe01)  TNTBE000;
024700110112         if  not  %found(TNTBE01L);
024800110112           clear  k_TBEsif;
024900110112           chain(n)  %kds(k05tntbe01)  TNTBE000;
025000110112         endif;
025100110112         if  %found(TNTBE01L);
025200110112           xTNTBEds = TNTBEds;
025300110112         else;
025400110112           clear  xTNTBEds;
025500110112         endif;
025600110112
025700110112         // -?Impostazione iniziale / pulizia dei campi chiave?
025800110112         k_TBEcod = c_Tab;
025900110112         clear  k_TBEke1;
026000110112         clear  k_TBEke2;
026100110112         clear  k_TBElin;
026200110112         clear  k_TBEsif;
026300110112
026400110112       ENDSR;
026500110112
026600110112       //--------------------------------------------------------------
026700110112       //?Reperimento Dati del job (Utente/Operativi).                 ?
026800110112       //--------------------------------------------------------------
026900110112       BEGSR  sr_DatiJob;
027000110112
027100110112         in(E) §AzUte;
027200110112         if NOT %error;
027300110112           in(E) §DatiUte;
027400110112         endif;
027500110112         if %error or RSut = *blanks;
027600110112           clear TIBS34ds;
027700110112           tibs34r ( tibs34ds );
027800110112           in §AzUte;
027900110112           in §DatiUte;
028000110112         endif;
028100110112
028200110112       ENDSR;
028300110112
028400110112       //--------------------------------------------------------------
028500110112       //?Gestione videata D01.                                        ?
028600110112       //--------------------------------------------------------------
028700110112       BEGSR  sr_GesD01;
028800110112
028900110112         // -?Inizializzazione videata?
029000110112         if  $InzD01 = *on;
029100110112           exsr  sr_InzD01;
029200110112           $InzD01 = *off;
029300110112         endif;
029400110112         clear  V1Topz;
029500110112
029600110112         // -?Emissione videata?
029700120919         write  TBMART01;
029800120919         write  TBMARP01;
029900120919         exfmt  TBMARD01;
030000110112
030100110112         clear  V1Dmsg;
030200110112         reset  ErrMessage;
030300110112         reset  ErrGenerico;
030400110112
030500110112         SELECT;
030600110112
030700110112           // -?F3=Fine?
030800110112           WHEN  dsp_aid = c_F03;
030900110112             $Fine = *on;
031000110112
031100110112           // -?Invio?
031200110112           OTHER;
031300110112             exsr  sr_CtrD01;
031400110112             if  ErrGenerico = *off;
031500110112               $Video    = 'D2';
031600110112               $InzD02   = *on;
031700110112             endif;
031800110112
031900110112         ENDSL;
032000110112
032100110112       ENDSR;
032200110112
032300110112       //--------------------------------------------------------------
032400110112       //?Inizializzazione videata D01.                                ?
032500110112       //--------------------------------------------------------------
032600110112       BEGSR  sr_InzD01;
032700110112
032800120919         clear  TBMARD01;
032900110112
033000120919         V1Cmar = '?';
033100110112
033200110112       ENDSR;
033300110112
033400110112       //--------------------------------------------------------------
033500110112       //?Controllo dati immessi nella videata D01.                    ?
033600110112       //--------------------------------------------------------------
033700110112       BEGSR  sr_CtrD01;
033800110112
033900110112         // -?Spegnimento indicatori di posizionamento cursore?
034000110112         %subst(IndDspF : 28) = *off;
034100110112
034200120919         // -?Motivi Apertura R.A.?
034300110112         Select;
034400110112
034500110112           // -?Ricerca?
034600120919           When  %scan( '?' : V1Cmar ) > *zero;
034700131018             clear  tntbMARds1;
034800131018             //iMARutUt = 'I';              //?(da visualizzare TUTTE)?
034900140416             //iMARutGe = 'I';              //?(da visualizzare TUTTE)?
035000131018             kpjbu = tntbMARds1;
035100131018             tntbMARr1 ( kpjba );
035200131018             tntbMARds1 = kpjbu;
035300131018             select;
035400131018               when  oMARfxx <> *blank;
035500131018               when  oMARerr =  *on;
035600131018                 V1Dmsg = oMARmsg;
035700131018                 PosCurMAR   = *on;
035800131018                 errMessage  = *on;
035900131018               other;
036000131018                 V1Cmar = oMARcod;
036100131018             endsl;
036200110112             ErrGenerico = *on;
036300110112             leavesr;
036400110112
036500110112           // -?Controllo immissione?
036600120919           When  V1Cmar = *blank;
036700110112             V1Dmsg = $Msg(01);
036800120919             PosCurMAR   = *on;
036900110112             ErrMessage  = *on;
037000110112             ErrGenerico = *on;
037100110112             leavesr;
037200110112
037300110112         EndSl;
037400110112
037500110112       ENDSR;
037600110112
037700110112       //--------------------------------------------------------------
037800110112       //?Gestione videata D02.                                        ?
037900110112       //--------------------------------------------------------------
038000110112       BEGSR  sr_GesD02;
038100110112
038200110112         // -?Inizializzazione videata?
038300110112         if  $InzD02 = *on;
038400110112           exsr  sr_InzD02;
038500110112           $InzD02 = *off;
038600110112         endif;
038700110112
038800110112         // -?Emissione Testata e Piede con tasti funzionali abilitati?
038900110112         if  NOT ErrMessage;
039000120919           write  TBMART01;
039100120919           write  TBMARD01;
039200110112         endif;
039300110112         write  Protect;
039400120919         write  TBMARP02;
039500110112
039600110112         // -?Emissione videata?
039700110112         if  Not $Protect;
039800120919           exfmt  TBMARD02;
039900110112         else;
040000120919           write  TBMARD02;
040100110112           exfmt  Protect;
040200110112         endif;
040300110112
040400110112         clear  V1Dmsg;
040500110112         reset  ErrMessage;
040600110112         reset  ErrGenerico;
040700110112
040800110112         SELECT;
040900110112
041000110112           // -?F3=Fine?
041100110112           WHEN  dsp_aid = c_F03;
041200110112             unlock TNTBE01L;
041300110112             $Fine = *on;
041400110112
041500110112           // -?F12=Ritorno?
041600110112           WHEN  dsp_aid = c_F12;
041700110112             unlock TNTBE01L;
041800110112             reset  $Video;
041900110112             clear  V1Topz;
042000110112
042100110112           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
042200110112           OTHER;
042300110112             $Ripristino   = (dsp_aid = c_F05);
042400110112             $Annullamento = (dsp_aid = c_F16);
042500110112             // -?Controlli eseguiti NON se F16=Annullamento?
042600110112             if  Not $Annullamento;
042700110112               exsr  sr_CtrD02;
042800110112             // -?Controlli eseguiti SOLO se F16=Annullamento?
042900110112             //else;
043000110112             //  exsr  sr_CtrANN;
043100110112             endif;
043200110112             if  ErrGenerico;
043300110112               leavesr;
043400110112             endif;
043500110112             // -?Aggiornamento?
043600110112             if  dsp_aid = c_F05   or
043700110112                 dsp_aid = c_F06   or
043800110112                 dsp_aid = c_F16;
043900110112               $Video = 'W1';
044000110112               reset  $InzW01;
044100110112             endif;
044200110112
044300110112         ENDSL;
044400110112
044500110112       ENDSR;
044600110112
044700110112       //--------------------------------------------------------------
044800110112       //?Inizializzazione videata D02.                                ?
044900110112       //--------------------------------------------------------------
045000110112       BEGSR  sr_InzD02;
045100110112
045200110112         reset  $Immissione;
045300110112         reset  $Ripristino;
045400110112         reset  $Protect;
045500110112
045600110112         // -?Spegnimento degli indicatori di errore?
045700110112         %subst(IndDspF : 50) = *off;
045800110112
045900110112         // -?Pulizia videata?
046000120919         clear  TBMARD02;
046100110112
046200120919         // -?Reperimento dati del Motivo Apertura R.A.?
046300120919         k_TBEke1 = V1Cmar;
046400110112         clear  k_TBEke2;
046500110112         clear  k_TBElin;
046600110112         clear  k_TBEsif;
046700110112         chain  %kds(k05tntbe01)  TNTBE000;
046800110112
046900120919         // -?Caricamento dati del Codice?
047000110112         select;
047100110112           when  Not %found(TNTBE01L);
047200110112             $Immissione = *on;
047300110112           when  TBEatb <> *blank;
047400110112             $Ripristino = *on;
047500110112         endsl;
047600110112         if  %found(TNTBE01L);
047700110112           exsr  sr_RieD02;
047800110112         endif;
047900110112
048000110112         // -?Impostazione testata?
048100110112         select;
048200110112           when  Not %found(TNTBE01L);
048300110112             V1Topz = '  INSERIMENTO  ';
048400110112           when  TBEatb <> *blank;
048500110112             V1Topz = '  RIPRISTINO   ';
048600110112           other;
048700110112             V1Topz = '   MODIFICA    ';
048800110112         endsl;
048900110112
049000110112         // -?Impostazione indicatori per abilitazione tasti funzionali?
049100110112         exsr  sr_AbilitFxxD02;
049200110112
049300110112       ENDSR;
049400110112
049500110112       //--------------------------------------------------------------
049600120919       //?Caricamento dati del singolo record della tab. "MAR"         ?
049700110112       //?nella videata D02.                                           ?
049800110112       //--------------------------------------------------------------
049900110112       BEGSR  sr_RieD02;
050000110112
050100120919         dMAR = TBEuni;
050200110112
050300131015         V1Cdes    = dMAR.§MARdesc;
050400131015         V1Crdco   = dMAR.§MARrdco;
050500131015         V1Cprot   = dMAR.§MARprot;
050600131015         V1Cmotch  = dMAR.§MARmotch;
050700150114         V1CcauChP = dMAR.§MARcauChP;
050800131015         V1Cnote   = dMAR.§MARnote;
050900131015         V1Cksc    = dMAR.§MARksc;
051000131015         V1Ctor    = dMAR.§MARtor;
051100131015         V1Cffr    = dMAR.§MARffr;
051200131015         V1CnotLdV = dMAR.§MARnotLdV;
051300140416         V1CutAp   = dMAR.§MARutAp;
051400131017         V1CutUt   = dMAR.§MARutUt;
051500140210         V1Cest    = dMAR.§MARest;
051600140210         V1Cect    = dMAR.§MARect;
051700141209         V1Ccanpa  = dMAR.§MARcanPA;
051800120919
051900130513         // -?Descrizione motivo chiusura R.A. tab. CHR?
052000130513         clear  V1Dmotch;
052100130513         clear  dCHR;
052200131018         reset  TIBS02ds;
052300131018         //T02mod = 'C';           ?(già così)?
052400120919         T02cod = 'CHR';
052500120919         T02sif = KNSIF;
052600120919         T02ke1 = V1Cmotch;
052700120919         TNTBE_RicercaControllo  (kpjba : tibs02ds);
052800120919         if  T02err = *blank;
052900120919           dCHR = T02uni;
053000120919         endif;
053100150114         V1Dmotch = dCHR.§CHRdesc;
053200150114
053300150114         // -?Descrizione causale chiusura R.A. proposta tab. CHR?
053400150114         clear  V1DcauChP;
053500150114         clear  dCHR;
053600150114         reset  TIBS02ds;
053700150114         //T02mod = 'C';           ?(già così)?
053800150114         T02cod = 'CHR';
053900150114         T02sif = KNSIF;
054000150114         T02ke1 = V1CcauChP;
054100150114         TNTBE_RicercaControllo  (kpjba : tibs02ds);
054200150114         if  T02err = *blank;
054300150114           dCHR = T02uni;
054400150114         endif;
054500150114         V1DcauChP = dCHR.§CHRdesc;
054600130513
054700130513         // -?Descrizione tipo oggetto obbligatorio. tab. OGR?
054800130513         clear  V1Dtor;
054900130513         clear  dOGR;
055000131018         reset  TIBS02ds;
055100131018         //T02mod = 'C';           ?(già così)?
055200130513         T02cod = 'OGR';
055300130513         T02sif = KNSIF;
055400130513         T02ke1 = V1Ctor;
055500130513         TNTBE_RicercaControllo  (kpjba : tibs02ds);
055600130513         if  T02err = *blank;
055700130513           dOGR = T02uni;
055800130513         endif;
055900130513         V1Dtor = dOGR.§OGRdesc;
056000110112
056100110112       ENDSR;
056200110112
056300110112       //--------------------------------------------------------------
056400110112       //?Settaggio indicatori nella videata D02 per abilitazione      ?
056500110112       //?  tasti funzionali.                                          ?
056600110112       //--------------------------------------------------------------
056700110112       BEGSR  sr_AbilitFxxD02;
056800110112
056900110112         // -?F5=Ripristino?
057000110112         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
057100110112                                         and   Not $Protect);
057200110112
057300110112         // -?F6=Conferma?
057400110112         F6Attivo  = (NOT F5Attivo);
057500110112
057600110112         // -?F16=Annullamento?
057700110112         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
057800110112                                         and   Not $Protect);
057900110112
058000110112       ENDSR;
058100110112
058200110112       //--------------------------------------------------------------
058300110112       //?Controllo dati videata D02.                                  ?
058400110112       //--------------------------------------------------------------
058500110112       BEGSR  sr_CtrD02;
058600110112
058700110112         // -?Spegnimento degli indicatori di errore?
058800110112         %subst(IndDspF : 50) = *off;
058900120919
059000150114         // -?Pulisco campi descrizione CHR?
059100120919         clear V1Dmotch;
059200150114         clear V1DcauChP;
059300130513         // -?Pulisco campo descrizione OGR?
059400130513         clear V1Dtor;
059500110112
059600110112         select;
059700110112
059800110112           // -?Descrizione obbligatoria?
059900110112           when  V1Cdes = *blank;
060000110112             V1Dmsg = $Msg(02);
060100110112             PosCurDES   = *on;
060200110112             ErrMessage  = *on;
060300110112             ErrGenerico = *on;
060400110112             leavesr;
060500110112
060600120919           // -?Motivo chiusura R.A. valido?
060700120919           when  V1Cmotch <> *blank;
060800120919             clear dCHR;
060900120919             reset  TIBS02ds;
061000131018             //T02mod = 'C';           ?(già così)?
061100120919             T02cod = 'CHR';
061200120919             T02sif = KNSIF;
061300120919             T02ke1 = V1Cmotch;
061400120919             TNTBE_RicercaControllo  (kpjba : tibs02ds);
061500120919             if  T02err <> *blank;
061600120919               V1Dmsg = $Msg(03);
061700120919               PosCurMOTCH = *on;
061800120919               ErrMessage  = *on;
061900120919               ErrGenerico = *on;
062000120919               leavesr;
062100120919             endif;
062200120919             dCHR = T02uni;
062300150114             V1Dmotch = dCHR.§CHRdesc;
062400110112
062500110112         endsl;
062600150114
062700150114           // -?Causale Chiusura R.A. Proposta valido?
062800150114         If  V1CcauChP <> *blank;
062900150114           clear dCHR;
063000150114           reset  TIBS02ds;
063100150114           //T02mod = 'C';           ?(già così)?
063200150114           T02cod = 'CHR';
063300150114           T02sif = KNSIF;
063400150114           T02ke1 = V1CcauChP;
063500150114           TNTBE_RicercaControllo  (kpjba : tibs02ds);
063600150114           if  T02err <> *blank;
063700150114             V1Dmsg = $Msg(03);
063800150114             PosCurCAUCHP = *on;
063900150114             ErrMessage   = *on;
064000150114             ErrGenerico  = *on;
064100150114             leavesr;
064200150114           endif;
064300150114           dCHR = T02uni;
064400150114           V1DcauChP = dCHR.§CHRdesc;
064500150114         EndIf;
064600130513
064700130513         // -?Tipo oggetto richiesta?
064800130513         select;
064900130513           // ·?Ricerca?
065000130513           when  %scan('?' : V1Ctor) > *zero;
065100130513             clear  TIBS02ds;
065200130513             T02mod = 'R';
065300130513             T02cod = 'OGR';
065400130513             T02sif = KNSIF;
065500130513             TNTBE_RicercaControllo (kpjba : TIBS02ds);
065600130513             if  T02err = *blank;
065700130513               V1Ctor = T02ke1;
065800130513               dOGR   = T02uni;
065900130513               V1Dtor = dOGR.§OGRdesc;
066000130513             else;
066100130513               V1Dmsg = T02msg;
066200130513               ErrMessage = *on;
066300130513             endif;
066400130513             PosCurTOR   = *on;
066500130513             errGenerico = *on;
066600130513             leavesr;
066700130513           // ·?Controllo?
066800130513           when  V1Ctor <> *blank;
066900130513             clear dOGR;
067000130513             reset  TIBS02ds;
067100131018             //T02mod = 'C';           ?(già così)?
067200130513             T02cod = 'OGR';
067300130513             T02sif = KNSIF;
067400130513             T02ke1 = V1Ctor;
067500130513             TNTBE_RicercaControllo (kpjba : TIBS02ds);
067600130513             if  T02err <> *blank;
067700130513               V1Dmsg = $Msg(04);
067800130513               PosCurTOR   = *on;
067900130513               ErrMessage  = *on;
068000130513               ErrGenerico = *on;
068100130513               leavesr;
068200130513             endif;
068300130513             dOGR = T02uni;
068400130513             V1Dtor = dOGR.§OGRdesc;
068500130513
068600130513         endsl;
068700130513
068800130513         // -?Flag Filiale Responsabile?
068900130513         select;
069000130513           when  V1Cffr <> *blank  and  V1Ctor <> 'S';
069100130513             V1Dmsg = $Msg(05);
069200130513             PosCurFFR   = *on;
069300130513             ErrMessage  = *on;
069400130513             ErrGenerico = *on;
069500130513             leavesr;
069600130513           when  V1Cffr <> *blank  and  V1Cffr <> 'A'
069700130513                                   and  V1Cffr <> 'P';
069800130513             V1Dmsg = $Msg(06);
069900130513             PosCurFFR   = *on;
070000130513             ErrMessage  = *on;
070100130513             ErrGenerico = *on;
070200130513             leavesr;
070300130513         endsl;
070400131015
070500131015         // -?Flag Note LdV obbligatorie (solo se ogg. Spedizioni)?
070600131015         select;
070700131015           when  V1CnotLdV = 'S'  and  V1Ctor <> 'S';
070800131015             V1Dmsg = $Msg(07);
070900131015             PosCurNOTLDV = *on;
071000131015             ErrMessage   = *on;
071100131015             ErrGenerico  = *on;
071200131015             leavesr;
071300131015           when  V1CnotLdV <> *blank  and  V1CnotLdV <> 'S';
071400131015             V1Dmsg = $Msg(08);
071500131015             PosCurNOTLDV = *on;
071600131015             ErrMessage   = *on;
071700131015             ErrGenerico  = *on;
071800131015             leavesr;
071900131015         endsl;
072000140416
072100140416         // -?Flag Utilizzabile dall'utente in Apertura?
072200140416         if  V1Cprot <> 'S'  and  V1CutAp = 'N';
072300140416           V1Dmsg = $Msg(09);
072400140416           PosCurutAp   = *on;
072500140416           ErrMessage   = *on;
072600140416           ErrGenerico  = *on;
072700140416           leavesr;
072800140416         endif;
072900110112
073000110112       ENDSR;
073100110112
073200110112       //--------------------------------------------------------------
073300110112       //?Gestione trasmissione aggiornamento                          ?
073400110112       //--------------------------------------------------------------
073500110112       BEGSR  sr_GesW01;
073600110112
073700110112         // -?Inizializzazione videata?
073800110112         if $InzW01 = *on;
073900110112           exsr  sr_InzW01;
074000110112           $InzW01 = *off;
074100110112         endif;
074200110112
074300110112         // -?Emissione window?
074400110112         if  TBXftt = 'S';
074500120919           exfmt  TBMARW01;
074600110112           ErrMessage  = *off;
074700110112           ErrGenerico = *off;
074800110112           clear  V1Dmsg;
074900110112         else;
075000110112           dsp_aid = c_F06;
075100110112         endif;
075200110112
075300110112         select;
075400110112
075500110112           // -?F12=Ritorno (gestito solo se emesso W01)?
075600110112           when  dsp_aid = c_F12;
075700110112             $Video = 'D2';
075800110112
075900110112           // -?F6=Conferma?
076000110112           when  dsp_aid = c_F06;
076100110112             exsr  sr_UpdTNTBE;
076200110112             // -?Ritorno alla videata iniziale?
076300110112             if  NOT  ErrGenerico;
076400110112               clear  V1Topz;
076500110112               $Video  = 'D1';
076600110112               $InzD01 = *on;
076700110112             endif;
076800110112         endsl;
076900110112
077000110112       ENDSR;
077100110112
077200110112       //--------------------------------------------------------------
077300110112       //?Inizializzazione videata W01                                 ?
077400110112       //--------------------------------------------------------------
077500110112       BEGSR  sr_InzW01;
077600110112
077700120919         clear TBMARW01;
077800110112
077900110112         if  $Immissione;
078000110112
078100110112           // -?Se immissione?
078200110112           W1ftt  = TBXftt;
078300110112
078400110112         else;
078500110112
078600110112           // -?Se NON immissione:?
078700110112           //  ?visualizza i dati relativi all'ultima trasmissione?
078800110112           W1ftt  = TBEftt;
078900110112           W1flt  = TBEflt;
079000110112           W1ftr  = TBEftr;
079100110112           if TBEdtr <> *zero;
079200110112             wDate_EUR = %date(TBEdtr : *iso);
079300110112             W1dtr     = %dec(wDate_EUR);
079400110112           endif;
079500110112
079600110112         endif;
079700110112
079800110112       ENDSR;
079900110112
080000110112       //--------------------------------------------------------------
080100120919       //?Aggiornamento records TNTBE00F (tab. "MAR")                  ?
080200110112       //--------------------------------------------------------------
080300110112       BEGSR  sr_UpdTNTBE;
080400110112
080500110112         // -?Aggancio record di TNTBE00F?
080600110112         k_TBEcod  = c_Tab;
080700120919         k_TBEke1  = V1Cmar;
080800110112         clear  k_TBEke2;
080900110112         clear  k_TBElin;
081000110112         clear  k_TBEsif;
081100110112
081200110112         chain  %kds(K05tntbe01) TNTBE000;
081300110112
081400120919         exsr  sr_RieMAR;
081500120919         TBEuni = dMAR;
081600110112
081700120919         // -?Aggiornamento tab. "MAR"?
081800110112         SELECT;
081900110112
082000110112           // -?F5=Ripristino?
082100110112           WHEN  $Ripristino;
082200110112             clear  TBEatb;
082300110112             TBEftt = W1ftt;
082400110112             clear  TBEftr;
082500110112             //_______________
082600110112             UPDATE  TNTBE000;
082700110112             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
082800110112
082900110112           // -?F16=Annullamento?
083000110112           WHEN  $Annullamento;
083100110112             TBEatb = 'A';
083200110112             TBEftt = W1ftt;
083300110112             clear  TBEftr;
083400110112             //_______________
083500110112             UPDATE  TNTBE000;
083600110112             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
083700110112
083800110112           // -?Immissione o Modifica?
083900110112           OTHER;
084000110112             if  NOT %found(TNTBE01L);
084100110112               clear TNTBE000;
084200110112               TBEcod  = k_TBEcod;
084300110112               TBEke1  = k_TBEke1;
084400110112               TBEke2  = k_TBEke2;
084500110112               TBElin  = k_TBElin;
084600110112               TBEsif  = k_TBEsif;
084700120919               TBEuni  = dMAR;
084800110112             endif;
084900110112             TBEapl = TBXapl;
085000110112             clear  TBEatb;
085100110112             TBEftt = W1ftt;
085200110112             clear  TBEflt;
085300110112             clear  TBEftr;
085400110112             //clear TBEdtr;
085500110112             // -?Aggiornamento record?
085600110112             if  %found(TNTBE01L);
085700110112               //_____________
085800110112               UPDATE TNTBE000;
085900110112               //¯¯¯¯¯¯¯¯¯¯¯¯¯
086000110112             else;
086100110112               //_____________
086200110112               WRITE  TNTBE000;
086300110112               //¯¯¯¯¯¯¯¯¯¯¯¯¯
086400110112             endif;
086500110112
086600110112         ENDSL;
086700110112
086800110112       ENDSR;
086900110112
087000110112       //--------------------------------------------------------------
087100120919       //?Caricamento dati in tab. "MAR"                               ?
087200110112       //--------------------------------------------------------------
087300120919       BEGSR  sr_RieMAR;
087400110112
087500120919         clear  dMAR;
087600110112
087700131015         dMAR.§MARdesc   = V1Cdes;
087800131015         dMAR.§MARrdco   = V1Crdco;
087900131015         dMAR.§MARprot   = V1Cprot;
088000131015         dMAR.§MARmotch  = V1Cmotch;
088100131015         dMAR.§MARnote   = V1Cnote;
088200131015         dMAR.§MARksc    = V1Cksc;
088300131015         dMAR.§MARtor    = V1Ctor;
088400131015         dMAR.§MARffr    = V1Cffr;
088500131015         dMAR.§MARnotLdV = V1CnotLdV;
088600140416         dMAR.§MARutAp   = V1CutAp;
088700131017         dMAR.§MARutUt   = V1CutUt;
088800140210         dMAR.§MARest    = V1Cest;
088900140210         dMAR.§MARect    = V1Cect;
089000141209         dMAR.§MARcanPA  = V1CCanPA;
089100150114         dMAR.§MARcauChP = V1CcauChP;
089200110112
089300110112       ENDSR;
089400110112
089500110112       //--------------------------------------------------------------
089600110112       //?Operazioni finali.                                           ?
089700110112       //--------------------------------------------------------------
089800110112       BEGSR  sr_RoutEnd;
089900110112
090000110112         // -?Chiusura funzioni precedentemente aperte?
090100110112         clear  TIBS02ds;
090200110112         T02tla = 'C';
090300110112         TNTBE_RicercaControllo ( kpjba : tibs02ds );
090400110112
090500110112         //  ?Uscita?
090600110112         return;
090700110112
090800110112       ENDSR;
090900110112
091000110112      /end-free
091100110112
091200110112       //--------------------------------------------------------------
091300110112       //?Definizione schiere a tempo di compilazione.                 ?
091400110112       //--------------------------------------------------------------
091500110112
091600130513** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
091700120919Immettere il Motivo Apertura R.A.                                               1
091800110112Descrizione obbligatoria                                                        2
091900120919Motivo chiusura R.A. errato                                                     3
092000130513Tipo Oggetto Richiesta errato                                                   4
092100130514Filiale Responsabile  inseribile SOLO SE  Tipo Oggetto Reclamo obblig. = "S"    5
092200130513Filiale Responsabile errata                                                     6
092300131015Note LDV obbligatorie  inseribile SOLO SE  Tipo Oggetto Reclamo obblig. = "S"   7
092400131015Flag "Note LDV obbligatorie" errato                                             8
092500140416Questa impostazione richiede "Motivo protetto in modifica" = "S"                9
