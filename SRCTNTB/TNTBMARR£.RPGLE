000100130513      *PARMS dfactgrp(*no) actgrp(*caller) dbgview(*source)
000200110112       //==============================================================
000300120919       //?Gestione tabella "MAR" (Motivi Apertura R.A.)                ?
000400110112       //==============================================================
000500130513
000600130513       //--------------------------------------------------------------
000700130513       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800130513       //--------------------------------------------------------------
000900130513
001000130513     /*PRM  dbgview(*source)
001100130513     /*END
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
002800120919     fTNTBMARD  cf   e             workstn
002900110112     f                                     indds(IndDspF)
003000110112     f                                     infds(InfDspF)
003100110112
003200110112       //--------------------------------------------------------------
003300110112       //?Definizione costanti.                                        ?
003400110112       //--------------------------------------------------------------
003500110112
003600110112       // -?Codice tabella in gestione?
003700120919     d c_Tab           c                   const('MAR')
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
007300140416     d $Msg            s             78    dim( 9)  ctdata  perrcd( 1)
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
009100140415     d   SDSpgm          *proc
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
010700120919     d   PosCurMAR                     n   overlay(IndDspF : 50)
010800110112     d   PosCurDES                     n   overlay(IndDspF : 51)
010900120919     d   PosCurMOTCH                   n   overlay(IndDspF : 52)
011000130513     d   PosCurTOR                     n   overlay(IndDspF : 53)
011100130513     d   PosCurFFR                     n   overlay(IndDspF : 54)
011200131015     d   PosCurNOTLDV                  n   overlay(IndDspF : 55)
011300140416     d   PosCurUTAP                    n   overlay(IndDspF : 56)
011400150114     d   PosCurCAUCHP                  n   overlay(IndDspF : 57)
011500110112         // -?Emissione messaggio di errore in window W1?
011600110112     d   ErrMessageW1                  n   overlay(IndDspF : 90)
011700110112         // -?Riemissione videata?
011800110112     d   ErrGenerico                   n   overlay(IndDspF : 99)
011900110112
012000110112       // -?Parametri ricevuti?
012100110112     d KPJBA         e ds
012200110112
012300110112       // -?Dati record principale della tabella?
012400110112     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012500110112     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012600110112     d                                          prefix(TBX:3)
012700110112
012800110112       // -?Tabelle usate:?
012900130513       // ·?"MAR" = Motivi apertura R.A.?
013000120919     d dMAR          e ds                  inz  qualified
013100130513       // ·?"CHR" = Codice chiusura Reclami?
013200120919     d dCHR          e ds                  inz  qualified
013300130513       // ·?"OGR" = Oggetto Richiesta?
013400130513     d dOGR          e ds                  inz  qualified
013500110112
013600110112       //--------------------------------------------------------------
013700110112       //?Definizione variabili globali.                               ?
013800110112       //--------------------------------------------------------------
013900110112
014000110112       // -?Flags booleani?
014100110112     d $Fine           s               n   inz
014200110112     d $InzD01         s               n   inz(*on)
014300110112     d $InzD02         s               n   inz(*on)
014400110112     d $InzW01         s               n   inz(*on)
014500110112     d $Annullamento   s               n   inz
014600110112     d $Immissione     s               n   inz
014700110112     d $Ripristino     s               n   inz
014800110112     d $Protect        s               n   inz
014900110112
015000110112       // -?Variabili per la gestione del video?
015100110112     d $Video          s              2    inz('D1')
015200110112
015300110112       // -?Campi di comodo?
015400110112     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015500110112
015600110112       //--------------------------------------------------------------
015700110112       //?Definizione prototipi procedure.                             ?
015800110112       //--------------------------------------------------------------
015900110112
016000110112       // -?Reperimento dati utente?
016100110112     d TIBS34ds      e ds
016200110112      /copy gaitrasrc/srcProtoPR,TIBS34R
016300110112
016400110112       // -?Ricerca e controllo nuove tabelle?
016500110112     d TIBS02ds      e ds                  inz
016600131018     d   T02mod      e                     inz('C')
016700110112      /copy gaitrasrc/srcProtoPR,TIBS02R
016800131018
016900131018       // -?Ricerca tab. "MAR" = Motivo Apertura Richiesta Assistenza?
017000131018     d tntbMARds1    e ds                  inz
017100131018      /copy gaitrasrc/srcProtoPR,TNTBMARR1
017200110112
017300110112       //--------------------------------------------------------------
017400110112       //?Definizione key-list.                                        ?
017500110112       //--------------------------------------------------------------
017600110112
017700110112       // -?File TNTBE01L?
017800110112     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
017900110112     d                                     prefix(k_)   inz
018000110112
018100110112       //--------------------------------------------------------------
018200110112       //?Riepilogo indicatori utilizzati.                             ?
018300110112       //--------------------------------------------------------------
018400110112       //--------------------------------------------------------------
018500110112
018600110112       //--------------------------------------------------------------
018700110112       //?M A I N - L I N E                                            ?
018800110112       //--------------------------------------------------------------
018900110112
019000110112     c     *Entry        plist
019100110112     c                   parm                    KPJBA
019200110112
019300110112      /free
019400110112
019500110112       // -?Operazioni iniziali?
019600110112       exsr  sr_RoutInz;
019700110112
019800110112       // -?Ciclo di gestione del file video?
019900110112       DOW  $Fine = *off;
020000110112
020100110112         select;
020200110112
020300110112           // -?Richiesta chiave di accesso?
020400110112           when $Video = 'D1';
020500110112             exsr  sr_GesD01;
020600110112
020700110112           // -?Manutenzione dati della singola tabella?
020800110112           when $Video = 'D2';
020900110112             exsr  sr_GesD02;
021000110112
021100110112           // -?Richiesta dati per trasmissione record?
021200110112           when $Video = 'W1';
021300110112             exsr  sr_GesW01;
021400110112
021500110112           // -?? ? ??
021600110112           other;
021700110112             $Fine = *on;
021800110112
021900110112         endsl;
022000110112
022100110112       ENDDO;
022200110112
022300110112       // -?Operazioni finali?
022400110112       exsr  sr_RoutEnd;
022500110112
022600110112       //--------------------------------------------------------------
022700110112       //?Operazioni iniziali.                                         ?
022800110112       //--------------------------------------------------------------
022900110112       BEGSR  sr_RoutInz;
023000110112
023100110112         *inLR = *on;
023200110112
023300110112         // -?Reperimento dati job?
023400110112         exsr  sr_DatiJob;
023500110112
023600110112         // -?Impostazione nome programma a video?
023700110112         V1Tpgm = SDSpgm;
023800110112
023900110112         // -?Aggancio dati generali della tabella in esame?
024000110112         clear  k_TBEcod;
024100110112         k_TBEke1 = *zero;
024200110112         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
024300110112                = c_Tab;
024400110112         clear  k_TBEke2;
024500110112         clear  k_TBElin;
024600110112         k_TBEsif = KNSIF;
024700110112         chain(n)  %kds(k05tntbe01)  TNTBE000;
024800110112         if  not  %found(TNTBE01L);
024900110112           clear  k_TBEsif;
025000110112           chain(n)  %kds(k05tntbe01)  TNTBE000;
025100110112         endif;
025200110112         if  %found(TNTBE01L);
025300110112           xTNTBEds = TNTBEds;
025400110112         else;
025500110112           clear  xTNTBEds;
025600110112         endif;
025700110112
025800110112         // -?Impostazione iniziale / pulizia dei campi chiave?
025900110112         k_TBEcod = c_Tab;
026000110112         clear  k_TBEke1;
026100110112         clear  k_TBEke2;
026200110112         clear  k_TBElin;
026300110112         clear  k_TBEsif;
026400110112
026500110112       ENDSR;
026600110112
026700110112       //--------------------------------------------------------------
026800110112       //?Reperimento Dati del job (Utente/Operativi).                 ?
026900110112       //--------------------------------------------------------------
027000110112       BEGSR  sr_DatiJob;
027100110112
027200110112         in(E) §AzUte;
027300110112         if NOT %error;
027400110112           in(E) §DatiUte;
027500110112         endif;
027600110112         if %error or RSut = *blanks;
027700110112           clear TIBS34ds;
027800110112           tibs34r ( tibs34ds );
027900110112           in §AzUte;
028000110112           in §DatiUte;
028100110112         endif;
028200110112
028300110112       ENDSR;
028400110112
028500110112       //--------------------------------------------------------------
028600110112       //?Gestione videata D01.                                        ?
028700110112       //--------------------------------------------------------------
028800110112       BEGSR  sr_GesD01;
028900110112
029000110112         // -?Inizializzazione videata?
029100110112         if  $InzD01 = *on;
029200110112           exsr  sr_InzD01;
029300110112           $InzD01 = *off;
029400110112         endif;
029500110112         clear  V1Topz;
029600110112
029700110112         // -?Emissione videata?
029800120919         write  TBMART01;
029900120919         write  TBMARP01;
030000120919         exfmt  TBMARD01;
030100110112
030200110112         clear  V1Dmsg;
030300110112         reset  ErrMessage;
030400110112         reset  ErrGenerico;
030500110112
030600110112         SELECT;
030700110112
030800110112           // -?F3=Fine?
030900110112           WHEN  dsp_aid = c_F03;
031000110112             $Fine = *on;
031100110112
031200110112           // -?Invio?
031300110112           OTHER;
031400110112             exsr  sr_CtrD01;
031500110112             if  ErrGenerico = *off;
031600110112               $Video    = 'D2';
031700110112               $InzD02   = *on;
031800110112             endif;
031900110112
032000110112         ENDSL;
032100110112
032200110112       ENDSR;
032300110112
032400110112       //--------------------------------------------------------------
032500110112       //?Inizializzazione videata D01.                                ?
032600110112       //--------------------------------------------------------------
032700110112       BEGSR  sr_InzD01;
032800110112
032900120919         clear  TBMARD01;
033000110112
033100120919         V1Cmar = '?';
033200110112
033300110112       ENDSR;
033400110112
033500110112       //--------------------------------------------------------------
033600110112       //?Controllo dati immessi nella videata D01.                    ?
033700110112       //--------------------------------------------------------------
033800110112       BEGSR  sr_CtrD01;
033900110112
034000110112         // -?Spegnimento indicatori di posizionamento cursore?
034100110112         %subst(IndDspF : 28) = *off;
034200110112
034300120919         // -?Motivi Apertura R.A.?
034400110112         Select;
034500110112
034600110112           // -?Ricerca?
034700120919           When  %scan( '?' : V1Cmar ) > *zero;
034800131018             clear  tntbMARds1;
034900131018             //iMARutUt = 'I';              //?(da visualizzare TUTTE)?
035000140416             //iMARutGe = 'I';              //?(da visualizzare TUTTE)?
035100131018             kpjbu = tntbMARds1;
035200131018             tntbMARr1 ( kpjba );
035300131018             tntbMARds1 = kpjbu;
035400131018             select;
035500131018               when  oMARfxx <> *blank;
035600131018               when  oMARerr =  *on;
035700131018                 V1Dmsg = oMARmsg;
035800131018                 PosCurMAR   = *on;
035900131018                 errMessage  = *on;
036000131018               other;
036100131018                 V1Cmar = oMARcod;
036200131018             endsl;
036300110112             ErrGenerico = *on;
036400110112             leavesr;
036500110112
036600110112           // -?Controllo immissione?
036700120919           When  V1Cmar = *blank;
036800110112             V1Dmsg = $Msg(01);
036900120919             PosCurMAR   = *on;
037000110112             ErrMessage  = *on;
037100110112             ErrGenerico = *on;
037200110112             leavesr;
037300110112
037400110112         EndSl;
037500110112
037600110112       ENDSR;
037700110112
037800110112       //--------------------------------------------------------------
037900110112       //?Gestione videata D02.                                        ?
038000110112       //--------------------------------------------------------------
038100110112       BEGSR  sr_GesD02;
038200110112
038300110112         // -?Inizializzazione videata?
038400110112         if  $InzD02 = *on;
038500110112           exsr  sr_InzD02;
038600110112           $InzD02 = *off;
038700110112         endif;
038800110112
038900110112         // -?Emissione Testata e Piede con tasti funzionali abilitati?
039000110112         if  NOT ErrMessage;
039100120919           write  TBMART01;
039200120919           write  TBMARD01;
039300110112         endif;
039400110112         write  Protect;
039500120919         write  TBMARP02;
039600110112
039700110112         // -?Emissione videata?
039800110112         if  Not $Protect;
039900120919           exfmt  TBMARD02;
040000110112         else;
040100120919           write  TBMARD02;
040200110112           exfmt  Protect;
040300110112         endif;
040400110112
040500110112         clear  V1Dmsg;
040600110112         reset  ErrMessage;
040700110112         reset  ErrGenerico;
040800110112
040900110112         SELECT;
041000110112
041100110112           // -?F3=Fine?
041200110112           WHEN  dsp_aid = c_F03;
041300110112             unlock TNTBE01L;
041400110112             $Fine = *on;
041500110112
041600110112           // -?F12=Ritorno?
041700110112           WHEN  dsp_aid = c_F12;
041800110112             unlock TNTBE01L;
041900110112             reset  $Video;
042000110112             clear  V1Topz;
042100110112
042200110112           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
042300110112           OTHER;
042400110112             $Ripristino   = (dsp_aid = c_F05);
042500110112             $Annullamento = (dsp_aid = c_F16);
042600110112             // -?Controlli eseguiti NON se F16=Annullamento?
042700110112             if  Not $Annullamento;
042800110112               exsr  sr_CtrD02;
042900110112             // -?Controlli eseguiti SOLO se F16=Annullamento?
043000110112             //else;
043100110112             //  exsr  sr_CtrANN;
043200110112             endif;
043300110112             if  ErrGenerico;
043400110112               leavesr;
043500110112             endif;
043600110112             // -?Aggiornamento?
043700110112             if  dsp_aid = c_F05   or
043800110112                 dsp_aid = c_F06   or
043900110112                 dsp_aid = c_F16;
044000110112               $Video = 'W1';
044100110112               reset  $InzW01;
044200110112             endif;
044300110112
044400110112         ENDSL;
044500110112
044600110112       ENDSR;
044700110112
044800110112       //--------------------------------------------------------------
044900110112       //?Inizializzazione videata D02.                                ?
045000110112       //--------------------------------------------------------------
045100110112       BEGSR  sr_InzD02;
045200110112
045300110112         reset  $Immissione;
045400110112         reset  $Ripristino;
045500110112         reset  $Protect;
045600110112
045700110112         // -?Spegnimento degli indicatori di errore?
045800110112         %subst(IndDspF : 50) = *off;
045900110112
046000110112         // -?Pulizia videata?
046100120919         clear  TBMARD02;
046200110112
046300120919         // -?Reperimento dati del Motivo Apertura R.A.?
046400120919         k_TBEke1 = V1Cmar;
046500110112         clear  k_TBEke2;
046600110112         clear  k_TBElin;
046700110112         clear  k_TBEsif;
046800110112         chain  %kds(k05tntbe01)  TNTBE000;
046900110112
047000120919         // -?Caricamento dati del Codice?
047100110112         select;
047200110112           when  Not %found(TNTBE01L);
047300110112             $Immissione = *on;
047400110112           when  TBEatb <> *blank;
047500110112             $Ripristino = *on;
047600110112         endsl;
047700110112         if  %found(TNTBE01L);
047800110112           exsr  sr_RieD02;
047900110112         endif;
048000110112
048100110112         // -?Impostazione testata?
048200110112         select;
048300110112           when  Not %found(TNTBE01L);
048400110112             V1Topz = '  INSERIMENTO  ';
048500110112           when  TBEatb <> *blank;
048600110112             V1Topz = '  RIPRISTINO   ';
048700110112           other;
048800110112             V1Topz = '   MODIFICA    ';
048900110112         endsl;
049000110112
049100110112         // -?Impostazione indicatori per abilitazione tasti funzionali?
049200110112         exsr  sr_AbilitFxxD02;
049300110112
049400110112       ENDSR;
049500110112
049600110112       //--------------------------------------------------------------
049700120919       //?Caricamento dati del singolo record della tab. "MAR"         ?
049800110112       //?nella videata D02.                                           ?
049900110112       //--------------------------------------------------------------
050000110112       BEGSR  sr_RieD02;
050100110112
050200120919         dMAR = TBEuni;
050300110112
050400131015         V1Cdes    = dMAR.§MARdesc;
050500131015         V1Crdco   = dMAR.§MARrdco;
050600131015         V1Cprot   = dMAR.§MARprot;
050700131015         V1Cmotch  = dMAR.§MARmotch;
050800150114         V1CcauChP = dMAR.§MARcauChP;
050900131015         V1Cnote   = dMAR.§MARnote;
051000131015         V1Cksc    = dMAR.§MARksc;
051100131015         V1Ctor    = dMAR.§MARtor;
051200131015         V1Cffr    = dMAR.§MARffr;
051300131015         V1CnotLdV = dMAR.§MARnotLdV;
051400140416         V1CutAp   = dMAR.§MARutAp;
051500131017         V1CutUt   = dMAR.§MARutUt;
051600140210         V1Cest    = dMAR.§MARest;
051700140210         V1Cect    = dMAR.§MARect;
051800141209         V1Ccanpa  = dMAR.§MARcanPA;
051900120919
052000130513         // -?Descrizione motivo chiusura R.A. tab. CHR?
052100130513         clear  V1Dmotch;
052200130513         clear  dCHR;
052300131018         reset  TIBS02ds;
052400131018         //T02mod = 'C';           ?(già così)?
052500120919         T02cod = 'CHR';
052600120919         T02sif = KNSIF;
052700120919         T02ke1 = V1Cmotch;
052800120919         TNTBE_RicercaControllo  (kpjba : tibs02ds);
052900120919         if  T02err = *blank;
053000120919           dCHR = T02uni;
053100120919         endif;
053200150114         V1Dmotch = dCHR.§CHRdesc;
053300150114
053400150114         // -?Descrizione causale chiusura R.A. proposta tab. CHR?
053500150114         clear  V1DcauChP;
053600150114         clear  dCHR;
053700150114         reset  TIBS02ds;
053800150114         //T02mod = 'C';           ?(già così)?
053900150114         T02cod = 'CHR';
054000150114         T02sif = KNSIF;
054100150114         T02ke1 = V1CcauChP;
054200150114         TNTBE_RicercaControllo  (kpjba : tibs02ds);
054300150114         if  T02err = *blank;
054400150114           dCHR = T02uni;
054500150114         endif;
054600150114         V1DcauChP = dCHR.§CHRdesc;
054700130513
054800130513         // -?Descrizione tipo oggetto obbligatorio. tab. OGR?
054900130513         clear  V1Dtor;
055000130513         clear  dOGR;
055100131018         reset  TIBS02ds;
055200131018         //T02mod = 'C';           ?(già così)?
055300130513         T02cod = 'OGR';
055400130513         T02sif = KNSIF;
055500130513         T02ke1 = V1Ctor;
055600130513         TNTBE_RicercaControllo  (kpjba : tibs02ds);
055700130513         if  T02err = *blank;
055800130513           dOGR = T02uni;
055900130513         endif;
056000130513         V1Dtor = dOGR.§OGRdesc;
056100110112
056200110112       ENDSR;
056300110112
056400110112       //--------------------------------------------------------------
056500110112       //?Settaggio indicatori nella videata D02 per abilitazione      ?
056600110112       //?  tasti funzionali.                                          ?
056700110112       //--------------------------------------------------------------
056800110112       BEGSR  sr_AbilitFxxD02;
056900110112
057000110112         // -?F5=Ripristino?
057100110112         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
057200110112                                         and   Not $Protect);
057300110112
057400110112         // -?F6=Conferma?
057500110112         F6Attivo  = (NOT F5Attivo);
057600110112
057700110112         // -?F16=Annullamento?
057800110112         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
057900110112                                         and   Not $Protect);
058000110112
058100110112       ENDSR;
058200110112
058300110112       //--------------------------------------------------------------
058400110112       //?Controllo dati videata D02.                                  ?
058500110112       //--------------------------------------------------------------
058600110112       BEGSR  sr_CtrD02;
058700110112
058800110112         // -?Spegnimento degli indicatori di errore?
058900110112         %subst(IndDspF : 50) = *off;
059000120919
059100150114         // -?Pulisco campi descrizione CHR?
059200120919         clear V1Dmotch;
059300150114         clear V1DcauChP;
059400130513         // -?Pulisco campo descrizione OGR?
059500130513         clear V1Dtor;
059600110112
059700110112         select;
059800110112
059900110112           // -?Descrizione obbligatoria?
060000110112           when  V1Cdes = *blank;
060100110112             V1Dmsg = $Msg(02);
060200110112             PosCurDES   = *on;
060300110112             ErrMessage  = *on;
060400110112             ErrGenerico = *on;
060500110112             leavesr;
060600110112
060700120919           // -?Motivo chiusura R.A. valido?
060800120919           when  V1Cmotch <> *blank;
060900120919             clear dCHR;
061000120919             reset  TIBS02ds;
061100131018             //T02mod = 'C';           ?(già così)?
061200120919             T02cod = 'CHR';
061300120919             T02sif = KNSIF;
061400120919             T02ke1 = V1Cmotch;
061500120919             TNTBE_RicercaControllo  (kpjba : tibs02ds);
061600120919             if  T02err <> *blank;
061700120919               V1Dmsg = $Msg(03);
061800120919               PosCurMOTCH = *on;
061900120919               ErrMessage  = *on;
062000120919               ErrGenerico = *on;
062100120919               leavesr;
062200120919             endif;
062300120919             dCHR = T02uni;
062400150114             V1Dmotch = dCHR.§CHRdesc;
062500110112
062600110112         endsl;
062700150114
062800150114           // -?Causale Chiusura R.A. Proposta valido?
062900150114         If  V1CcauChP <> *blank;
063000150114           clear dCHR;
063100150114           reset  TIBS02ds;
063200150114           //T02mod = 'C';           ?(già così)?
063300150114           T02cod = 'CHR';
063400150114           T02sif = KNSIF;
063500150114           T02ke1 = V1CcauChP;
063600150114           TNTBE_RicercaControllo  (kpjba : tibs02ds);
063700150114           if  T02err <> *blank;
063800150114             V1Dmsg = $Msg(03);
063900150114             PosCurCAUCHP = *on;
064000150114             ErrMessage   = *on;
064100150114             ErrGenerico  = *on;
064200150114             leavesr;
064300150114           endif;
064400150114           dCHR = T02uni;
064500150114           V1DcauChP = dCHR.§CHRdesc;
064600150114         EndIf;
064700130513
064800130513         // -?Tipo oggetto richiesta?
064900130513         select;
065000130513           // ·?Ricerca?
065100130513           when  %scan('?' : V1Ctor) > *zero;
065200130513             clear  TIBS02ds;
065300130513             T02mod = 'R';
065400130513             T02cod = 'OGR';
065500130513             T02sif = KNSIF;
065600130513             TNTBE_RicercaControllo (kpjba : TIBS02ds);
065700130513             if  T02err = *blank;
065800130513               V1Ctor = T02ke1;
065900130513               dOGR   = T02uni;
066000130513               V1Dtor = dOGR.§OGRdesc;
066100130513             else;
066200130513               V1Dmsg = T02msg;
066300130513               ErrMessage = *on;
066400130513             endif;
066500130513             PosCurTOR   = *on;
066600130513             errGenerico = *on;
066700130513             leavesr;
066800130513           // ·?Controllo?
066900130513           when  V1Ctor <> *blank;
067000130513             clear dOGR;
067100130513             reset  TIBS02ds;
067200131018             //T02mod = 'C';           ?(già così)?
067300130513             T02cod = 'OGR';
067400130513             T02sif = KNSIF;
067500130513             T02ke1 = V1Ctor;
067600130513             TNTBE_RicercaControllo (kpjba : TIBS02ds);
067700130513             if  T02err <> *blank;
067800130513               V1Dmsg = $Msg(04);
067900130513               PosCurTOR   = *on;
068000130513               ErrMessage  = *on;
068100130513               ErrGenerico = *on;
068200130513               leavesr;
068300130513             endif;
068400130513             dOGR = T02uni;
068500130513             V1Dtor = dOGR.§OGRdesc;
068600130513
068700130513         endsl;
068800130513
068900130513         // -?Flag Filiale Responsabile?
069000130513         select;
069100130513           when  V1Cffr <> *blank  and  V1Ctor <> 'S';
069200130513             V1Dmsg = $Msg(05);
069300130513             PosCurFFR   = *on;
069400130513             ErrMessage  = *on;
069500130513             ErrGenerico = *on;
069600130513             leavesr;
069700130513           when  V1Cffr <> *blank  and  V1Cffr <> 'A'
069800130513                                   and  V1Cffr <> 'P';
069900130513             V1Dmsg = $Msg(06);
070000130513             PosCurFFR   = *on;
070100130513             ErrMessage  = *on;
070200130513             ErrGenerico = *on;
070300130513             leavesr;
070400130513         endsl;
070500131015
070600131015         // -?Flag Note LdV obbligatorie (solo se ogg. Spedizioni)?
070700131015         select;
070800131015           when  V1CnotLdV = 'S'  and  V1Ctor <> 'S';
070900131015             V1Dmsg = $Msg(07);
071000131015             PosCurNOTLDV = *on;
071100131015             ErrMessage   = *on;
071200131015             ErrGenerico  = *on;
071300131015             leavesr;
071400131015           when  V1CnotLdV <> *blank  and  V1CnotLdV <> 'S';
071500131015             V1Dmsg = $Msg(08);
071600131015             PosCurNOTLDV = *on;
071700131015             ErrMessage   = *on;
071800131015             ErrGenerico  = *on;
071900131015             leavesr;
072000131015         endsl;
072100140416
072200140416         // -?Flag Utilizzabile dall'utente in Apertura?
072300140416         if  V1Cprot <> 'S'  and  V1CutAp = 'N';
072400140416           V1Dmsg = $Msg(09);
072500140416           PosCurutAp   = *on;
072600140416           ErrMessage   = *on;
072700140416           ErrGenerico  = *on;
072800140416           leavesr;
072900140416         endif;
073000110112
073100110112       ENDSR;
073200110112
073300110112       //--------------------------------------------------------------
073400110112       //?Gestione trasmissione aggiornamento                          ?
073500110112       //--------------------------------------------------------------
073600110112       BEGSR  sr_GesW01;
073700110112
073800110112         // -?Inizializzazione videata?
073900110112         if $InzW01 = *on;
074000110112           exsr  sr_InzW01;
074100110112           $InzW01 = *off;
074200110112         endif;
074300110112
074400110112         // -?Emissione window?
074500110112         if  TBXftt = 'S';
074600120919           exfmt  TBMARW01;
074700110112           ErrMessage  = *off;
074800110112           ErrGenerico = *off;
074900110112           clear  V1Dmsg;
075000110112         else;
075100110112           dsp_aid = c_F06;
075200110112         endif;
075300110112
075400110112         select;
075500110112
075600110112           // -?F12=Ritorno (gestito solo se emesso W01)?
075700110112           when  dsp_aid = c_F12;
075800110112             $Video = 'D2';
075900110112
076000110112           // -?F6=Conferma?
076100110112           when  dsp_aid = c_F06;
076200110112             exsr  sr_UpdTNTBE;
076300110112             // -?Ritorno alla videata iniziale?
076400110112             if  NOT  ErrGenerico;
076500110112               clear  V1Topz;
076600110112               $Video  = 'D1';
076700110112               $InzD01 = *on;
076800110112             endif;
076900110112         endsl;
077000110112
077100110112       ENDSR;
077200110112
077300110112       //--------------------------------------------------------------
077400110112       //?Inizializzazione videata W01                                 ?
077500110112       //--------------------------------------------------------------
077600110112       BEGSR  sr_InzW01;
077700110112
077800120919         clear TBMARW01;
077900110112
078000110112         if  $Immissione;
078100110112
078200110112           // -?Se immissione?
078300110112           W1ftt  = TBXftt;
078400110112
078500110112         else;
078600110112
078700110112           // -?Se NON immissione:?
078800110112           //  ?visualizza i dati relativi all'ultima trasmissione?
078900110112           W1ftt  = TBEftt;
079000110112           W1flt  = TBEflt;
079100110112           W1ftr  = TBEftr;
079200110112           if TBEdtr <> *zero;
079300110112             wDate_EUR = %date(TBEdtr : *iso);
079400110112             W1dtr     = %dec(wDate_EUR);
079500110112           endif;
079600110112
079700110112         endif;
079800110112
079900110112       ENDSR;
080000110112
080100110112       //--------------------------------------------------------------
080200120919       //?Aggiornamento records TNTBE00F (tab. "MAR")                  ?
080300110112       //--------------------------------------------------------------
080400110112       BEGSR  sr_UpdTNTBE;
080500110112
080600110112         // -?Aggancio record di TNTBE00F?
080700110112         k_TBEcod  = c_Tab;
080800120919         k_TBEke1  = V1Cmar;
080900110112         clear  k_TBEke2;
081000110112         clear  k_TBElin;
081100110112         clear  k_TBEsif;
081200110112
081300110112         chain  %kds(K05tntbe01) TNTBE000;
081400110112
081500120919         exsr  sr_RieMAR;
081600120919         TBEuni = dMAR;
081700110112
081800120919         // -?Aggiornamento tab. "MAR"?
081900110112         SELECT;
082000110112
082100110112           // -?F5=Ripristino?
082200110112           WHEN  $Ripristino;
082300110112             clear  TBEatb;
082400110112             TBEftt = W1ftt;
082500110112             clear  TBEftr;
082600110112             //_______________
082700110112             UPDATE  TNTBE000;
082800110112             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
082900110112
083000110112           // -?F16=Annullamento?
083100110112           WHEN  $Annullamento;
083200110112             TBEatb = 'A';
083300110112             TBEftt = W1ftt;
083400110112             clear  TBEftr;
083500110112             //_______________
083600110112             UPDATE  TNTBE000;
083700110112             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
083800110112
083900110112           // -?Immissione o Modifica?
084000110112           OTHER;
084100110112             if  NOT %found(TNTBE01L);
084200110112               clear TNTBE000;
084300110112               TBEcod  = k_TBEcod;
084400110112               TBEke1  = k_TBEke1;
084500110112               TBEke2  = k_TBEke2;
084600110112               TBElin  = k_TBElin;
084700110112               TBEsif  = k_TBEsif;
084800120919               TBEuni  = dMAR;
084900110112             endif;
085000110112             TBEapl = TBXapl;
085100110112             clear  TBEatb;
085200110112             TBEftt = W1ftt;
085300110112             clear  TBEflt;
085400110112             clear  TBEftr;
085500110112             //clear TBEdtr;
085600110112             // -?Aggiornamento record?
085700110112             if  %found(TNTBE01L);
085800110112               //_____________
085900110112               UPDATE TNTBE000;
086000110112               //¯¯¯¯¯¯¯¯¯¯¯¯¯
086100110112             else;
086200110112               //_____________
086300110112               WRITE  TNTBE000;
086400110112               //¯¯¯¯¯¯¯¯¯¯¯¯¯
086500110112             endif;
086600110112
086700110112         ENDSL;
086800110112
086900110112       ENDSR;
087000110112
087100110112       //--------------------------------------------------------------
087200120919       //?Caricamento dati in tab. "MAR"                               ?
087300110112       //--------------------------------------------------------------
087400120919       BEGSR  sr_RieMAR;
087500110112
087600120919         clear  dMAR;
087700110112
087800131015         dMAR.§MARdesc   = V1Cdes;
087900131015         dMAR.§MARrdco   = V1Crdco;
088000131015         dMAR.§MARprot   = V1Cprot;
088100131015         dMAR.§MARmotch  = V1Cmotch;
088200131015         dMAR.§MARnote   = V1Cnote;
088300131015         dMAR.§MARksc    = V1Cksc;
088400131015         dMAR.§MARtor    = V1Ctor;
088500131015         dMAR.§MARffr    = V1Cffr;
088600131015         dMAR.§MARnotLdV = V1CnotLdV;
088700140416         dMAR.§MARutAp   = V1CutAp;
088800131017         dMAR.§MARutUt   = V1CutUt;
088900140210         dMAR.§MARest    = V1Cest;
089000140210         dMAR.§MARect    = V1Cect;
089100141209         dMAR.§MARcanPA  = V1CCanPA;
089200150114         dMAR.§MARcauChP = V1CcauChP;
089300110112
089400110112       ENDSR;
089500110112
089600110112       //--------------------------------------------------------------
089700110112       //?Operazioni finali.                                           ?
089800110112       //--------------------------------------------------------------
089900110112       BEGSR  sr_RoutEnd;
090000110112
090100110112         // -?Chiusura funzioni precedentemente aperte?
090200110112         clear  TIBS02ds;
090300110112         T02tla = 'C';
090400110112         TNTBE_RicercaControllo ( kpjba : tibs02ds );
090500110112
090600110112         //  ?Uscita?
090700110112         return;
090800110112
090900110112       ENDSR;
091000110112
091100110112      /end-free
091200110112
091300110112       //--------------------------------------------------------------
091400110112       //?Definizione schiere a tempo di compilazione.                 ?
091500110112       //--------------------------------------------------------------
091600110112
091700130513** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
091800120919Immettere il Motivo Apertura R.A.                                               1
091900110112Descrizione obbligatoria                                                        2
092000120919Motivo chiusura R.A. errato                                                     3
092100130513Tipo Oggetto Richiesta errato                                                   4
092200130514Filiale Responsabile  inseribile SOLO SE  Tipo Oggetto Reclamo obblig. = "S"    5
092300130513Filiale Responsabile errata                                                     6
092400131015Note LDV obbligatorie  inseribile SOLO SE  Tipo Oggetto Reclamo obblig. = "S"   7
092500131015Flag "Note LDV obbligatorie" errato                                             8
092600140416Questa impostazione richiede "Motivo protetto in modifica" = "S"                9
