000100091120     /*END
000200091029       //--------------------------------------------------------------
000300091029       //?Gestione tabella "5PS" (Tipi supporto dati da cliente a BART)?
000400091029       //--------------------------------------------------------------
000500091029
000600091029     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700091029     h dftactgrp(*no) actgrp(*caller)
000800091029
000900091029       //--------------------------------------------------------------
001000091029       //?Dichiarazione file.                                          ?
001100091029       //--------------------------------------------------------------
001200091029
001300091029       // - Tabelle
001400091029     fTNTBE01L  Uf A e           k disk
001500091029
001600091029       // - Video Gestione
001700091029     fTNTB78D   cf   e             workstn
001800091029     f                                     indds(IndDspF)
001900091029     f                                     infds(InfDspF)
002000091029
002100091029       //--------------------------------------------------------------
002200091029       //?Definizione costanti.                                        ?
002300091029       //--------------------------------------------------------------
002400091029
002500091029       // - Codice tabella in gestione
002600091029     d c_Tab           c                   const('3CS')
002700091029
002800091029       // - Tasti funzionali a video
002900091029     d c_F01           c                   const(x'31')
003000091029     d c_F02           c                   const(x'32')
003100091029     d c_F03           c                   const(x'33')
003200091029     d c_F04           c                   const(x'34')
003300091029     d c_F05           c                   const(x'35')
003400091029     d c_F06           c                   const(x'36')
003500091029     d c_F07           c                   const(x'37')
003600091029     d c_F08           c                   const(x'38')
003700091029     d c_F09           c                   const(x'39')
003800091029     d c_F10           c                   const(x'3A')
003900091029     d c_F11           c                   const(x'3B')
004000091029     d c_F12           c                   const(x'3C')
004100091029     d c_F13           c                   const(x'B1')
004200091029     d c_F14           c                   const(x'B2')
004300091029     d c_F15           c                   const(x'B3')
004400091029     d c_F16           c                   const(x'B4')
004500091029     d c_F17           c                   const(x'B5')
004600091029     d c_F18           c                   const(x'B6')
004700091029     d c_F19           c                   const(x'B7')
004800091029     d c_F20           c                   const(x'B8')
004900091029     d c_F21           c                   const(x'B9')
005000091029     d c_F22           c                   const(x'BA')
005100091029     d c_F23           c                   const(x'BB')
005200091029     d c_F24           c                   const(x'BC')
005300091029     d c_Enter         c                   const(x'F1')
005400091029     d c_RollDown      c                   const(x'F4')
005500091029     d c_RollUp        c                   const(x'F5')
005600091029
005700091029       //--------------------------------------------------------------
005800091029       //?Definizione schiere.                                         ?
005900091029       //--------------------------------------------------------------
006000091029
006100091029       // - Messaggi di errore
006200091029     d $Msg            s             78    dim( 2)  ctdata  perrcd( 1)
006300091029
006400091029       //--------------------------------------------------------------
006500091029       //?Definizione aree dati.                                       ?
006600091029       //--------------------------------------------------------------
006700091029
006800091029       // - Dati utente
006900091029     d §AzUte        e ds                  extname(AZUTE00F)
007000091029     d                                     dtaara
007100091029     d §DatiUte      e ds                  extname(dDatiUte)
007200091029     d                                     dtaara
007300091029
007400091029       //--------------------------------------------------------------
007500091029       //?Definizione strutture dati.                                  ?
007600091029       //--------------------------------------------------------------
007700091029
007800091029       // - Status ds
007900091029     d Status         sds
008000091029     d  SDSpgm           *proc
008100091029
008200091029       // - InfDS
008300091029     d InfDspF         ds
008400091029     d   dsp_aid             369    369a
008500091029
008600091029       // - Indicatori su DspF
008700091029     d IndDspF         ds                  inz
008800091029         // - Abilitazione tasti funzionali
008900091029     d   F5Attivo                      n   overlay(IndDspF : 05)
009000091029     d   F6Attivo                      n   overlay(IndDspF : 06)
009100091029     d   F12Attivo                     n   overlay(IndDspF : 12)
009200091029     d   F16Attivo                     n   overlay(IndDspF : 16)
009300091029         // - Emissione messaggio di errore
009400091029     d   ErrMessage                    n   overlay(IndDspF : 28)
009500091029         // - Posizionamento cursore & segnalazione errore
009600091029     d   PosCurKE1                     n   overlay(IndDspF : 50)
009700091029     d   PosCurDES                     n   overlay(IndDspF : 51)
009800091029         //   - Riemissione videata
009900091029     d   ErrGenerico                   n   overlay(IndDspF : 99)
010000091029
010100091029       // - Parametri ricevuti
010200091029     d KPJBA         e ds
010300091029
010400091029       // - Parametri per Reperimento dati utente
010500091029     d TIBS34ds      e ds
010600091029
010700091029       // - Parametri per Ricerca/Controllo tabelle
010800091029     d TIBS02ds      e ds                  inz
010900091029     d   T02mod      e                     inz('R')
011000091029     d   T02cod      e                     inz(c_Tab)
011100091029
011200091029       // - Tab. 3CS = Tipi supporto dati da cliente a Bartolini
011300091029     d*// d3CP          e ds                  inz
011400091029
011500091029       // - Dati record principale della tabella
011600091029     d TNTBEds       e ds                  inz  extname(TNTBE00F)
011700091029     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
011800091029     d                                          prefix(TBX:3)
011900091029
012000091029       //--------------------------------------------------------------
012100091029       //?Definizione variabili globali.                               ?
012200091029       //--------------------------------------------------------------
012300091029
012400091029       // - Flags booleani
012500091029     d $Fine           s               n   inz
012600091029     d $InzD01         s               n   inz(*on)
012700091029     d $InzD02         s               n   inz(*on)
012800091029     d $InzW01         s               n   inz(*on)
012900091029     d $Annullamento   s               n   inz
013000091029     d $Immissione     s               n   inz
013100091029     d $Ripristino     s               n   inz
013200091029
013300091029       // - Indici di schiera
013400091029     d xx              s              3  0 inz
013500091029
013600091029       // - Variabili per la gestione del video
013700091029     d $Video          s              2    inz('D1')
013800091029
013900091029       // - Campi di comodo
014000091029     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
014100091029
014200091029       //--------------------------------------------------------------
014300091029       //?Definizione prototipi procedure.                             ?
014400091029       //--------------------------------------------------------------
014500091029
014600091029       // - Reperimento dati utente
014700091029      /copy gaitrasrc/srcProtoPR,TIBS34R
014800091029
014900091029       // - Ricerca e controllo nuove tabelle
015000091029      /copy gaitrasrc/srcProtoPR,TIBS02R
015100091029
015200091029       //--------------------------------------------------------------
015300091029       //?Definizione key-list.                                        ?
015400091029       //--------------------------------------------------------------
015500091029
015600091029       // - File TNTBE01L
015700091029     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
015800091029     d                                     prefix(k_)   inz
015900091029
016000091029       //--------------------------------------------------------------
016100091029       //?M A I N - L I N E                                            ?
016200091029       //--------------------------------------------------------------
016300091029
016400091029     c     *Entry        plist
016500091029     c                   parm                    KPJBA
016600091029
016700091029      /free
016800091029
016900091029       // -?Operazioni iniziali?
017000091029       exsr sr_RoutInz;
017100091029
017200091029       // -?Gestione video?
017300091029       DOW  $Fine = *off;
017400091029
017500091029         select;
017600091029
017700091029           // -?Richiesta chiave di accesso?
017800091029           when $Video = 'D1';
017900091029             exsr sr_GesD01;
018000091029
018100091029           // -?Manutenzione dati della singola tabella?
018200091029           when $Video = 'D2';
018300091029             exsr sr_GesD02;
018400091029
018500091029           // -?Videata per trasmissione dati?
018600091029           when $Video = 'W1';
018700091029             exsr sr_GesW01;
018800091029
018900091029           // -?? ? ??
019000091029           other;
019100091029             $Fine = *on;
019200091029
019300091029         endsl;
019400091029
019500091029       ENDDO;
019600091029
019700091029       // -?Operazioni finali?
019800091029       exsr sr_RoutEnd;
019900091029
020000091029       //--------------------------------------------------------------
020100091029       //?Operazioni iniziali.                                         ?
020200091029       //--------------------------------------------------------------
020300091029       BEGSR  sr_RoutInz;
020400091029
020500091029         *inLR = *on;
020600091029
020700091029         // -?Reperimento dati job?
020800091029         exsr  sr_DatiJob;
020900091029
021000091029         // -?Impostazione nome programma a video?
021100091029         V1Tpgm = SDSpgm;
021200091029
021300091029         // -?Aggancio dati generali della tabella in esame?
021400091029         clear  k_TBEcod;
021500091029         k_TBEke1 = *zero;
021600091029         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
021700091029                = c_Tab;
021800091029         clear  k_TBEke2;
021900091029         clear  k_TBElin;
022000091029         k_TBEsif = KNSIF;
022100091029         chain(n)  %kds(k05tntbe01)  TNTBE000;
022200091029         if  not  %found(TNTBE01L);
022300091029           clear  k_TBEsif;
022400091029           chain(n)  %kds(k05tntbe01)  TNTBE000;
022500091029         endif;
022600091029         if  %found(TNTBE01L);
022700091029           xTNTBEds = TNTBEds;
022800091029         else;
022900091029           clear  xTNTBEds;
023000091029         endif;
023100091029
023200091029         // -?Impostazione iniziale / pulizia dei campi chiave?
023300091029         k_TBEcod = c_Tab;
023400091029         clear  k_TBEke1;
023500091029         clear  k_TBEke2;
023600091029         clear  k_TBElin;
023700091029         clear  k_TBEsif;
023800091029
023900091029         // -?1ª inizializzazione della videata D01?
024000091029         exsr  sr_InzD01;
024100091029         V1Cke1  = '?';
024200091029         $InzD01 = *off;
024300091029
024400091029       ENDSR;
024500091029
024600091029       //--------------------------------------------------------------
024700091029       //?Reperimento Dati del job (Utente/Operativi).                 ?
024800091029       //--------------------------------------------------------------
024900091029       BEGSR  sr_DatiJob;
025000091029
025100091029         in(E) §AzUte;
025200091029         if NOT %error;
025300091029           in(E) §DatiUte;
025400091029         endif;
025500091029         if %error or RSut = *blanks;
025600091029           clear TIBS34ds;
025700091029           tibs34r ( tibs34ds );
025800091029           in §AzUte;
025900091029           in §DatiUte;
026000091029         endif;
026100091029
026200091029       ENDSR;
026300091029
026400091029       //--------------------------------------------------------------
026500091029       //?Gestione videata D01                                         ?
026600091029       //--------------------------------------------------------------
026700091029       BEGSR  sr_GesD01;
026800091029
026900091029         // -?Inizializzazione videata?
027000091029         if  $InzD01 = *on;
027100091029           exsr  sr_InzD01;
027200091029           $InzD01 = *off;
027300091029         endif;
027400091029
027500091029         // -?(Dis)Abilitazione tasti funzionali?
027600091029         F5Attivo  = *off;
027700091029         F6Attivo  = *off;
027800091029         F12Attivo = *off;
027900091029         F16Attivo = *off;
028000091029
028100091029         // -?Emissione videata?
028200091029         write  TB78P01;
028300091029         write  TB78T01;
028400091029         exfmt  TB78D01;
028500091029
028600091029         clear  V1Dmsg;
028700091029         reset  ErrMessage;
028800091029         reset  ErrGenerico;
028900091029
029000091029         SELECT;
029100091029
029200091029           // -?F3=Fine?
029300091029           WHEN  dsp_aid = c_F03;
029400091029             $Fine = *on;
029500091029
029600091029           // -?Invio?
029700091029           OTHER;
029800091029             exsr sr_CtrD01;
029900091029             if  ErrGenerico = *off;
030000091029               $Video    = 'D2';
030100091029               $InzD02   = *on;
030200091029             endif;
030300091029
030400091029         ENDSL;
030500091029
030600091029       ENDSR;
030700091029
030800091029       //--------------------------------------------------------------
030900091029       //?Inizializzazione videata D01                                 ?
031000091029       //--------------------------------------------------------------
031100091029       BEGSR  sr_InzD01;
031200091029
031300091029         clear  TB78D01;
031400091029
031500091029       ENDSR;
031600091029
031700091029       //--------------------------------------------------------------
031800091029       //?Controllo dati immessi nella videata D01                     ?
031900091029       //--------------------------------------------------------------
032000091029       BEGSR  sr_CtrD01;
032100091029
032200091029         %subst(IndDspF : 28) = *off;
032300091029
032400091029         SELECT;
032500091029
032600091029           // -?Ricerca codice Tipo Supporto Dati DA Cliente A Bartolini?
032700091029           WHEN  %scan('?' : V1Cke1) > *zero;
032800091029             clear  V1Cke1;
032900091029             clear  TIBS02ds;
033000091029             T02mod = 'R';
033100091029             T02cod = '3CS';
033200091029             T02sif = knsif;
033300091029             TNTBE_RicercaControllo (kpjba : TIBS02ds);
033400091120             if  T02ke1 <> *blank;
033500091029               V1Cke1 = T02ke1;
033600091029               //d3CS = T02uni ;
033700091029             endif;
033800091029             errGenerico = *on;
033900091029             PosCurKE1   = *on;
034000091029             leavesr;
034100091029
034200091029           // -?Controllo immissione codice causale?
034300091029           WHEN  V1Cke1 = *blank;
034400091029             ErrMessage  = *on;
034500091029             ErrGenerico = *on;
034600091029             PosCurKE1   = *on;
034700091029             V1Dmsg = $Msg(01);
034800091029             leavesr;
034900091029
035000091029         ENDSL;
035100091029
035200091029       ENDSR;
035300091029
035400091029       //--------------------------------------------------------------
035500091029       //?Gestione videata D02                                         ?
035600091029       //--------------------------------------------------------------
035700091029       BEGSR  sr_GesD02;
035800091029
035900091029         // -?Inizializzazione videata?
036000091029         if  $InzD02 = *on;
036100091029           exsr sr_InzD02;
036200091029           $InzD02 = *off;
036300091029         endif;
036400091029
036500091029         // -?Emissione Testata e Piede con tasti funzionali abilitati?
036600091029         if  errGenerico = *off;
036700091029           write  TB78P01;
036800091029           write  TB78T01;
036900091029           write  TB78D01;
037000091029           write  Protect;
037100091029         endif;
037200091029
037300091029         // -?Emissione videata?
037400091029         exfmt  TB78D02;
037500091029
037600091029         clear  V1Dmsg;
037700091029         reset  ErrMessage;
037800091029         reset  ErrGenerico;
037900091029
038000091029         reset  $Annullamento;
038100091029
038200091029         SELECT;
038300091029
038400091029           // -?F3=Fine?
038500091029           WHEN  dsp_aid = c_F03;
038600091029             unlock TNTBE01L;
038700091029             $Fine = *on;
038800091029
038900091029           // -?F12=Ritorno?
039000091029           WHEN  dsp_aid = c_F12;
039100091029             unlock TNTBE01L;
039200091029             reset  $Video;
039300091029             clear  V1Topz;
039400091029
039500091029           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
039600091029           OTHER;
039700091029             $Ripristino   = (dsp_aid = c_F05);
039800091029             $Annullamento = (dsp_aid = c_F16);
039900091029             if  Not $Annullamento;
040000091029               exsr  sr_CtrD02;
040100091029               if  ErrGenerico;
040200091029                 leavesr;
040300091029               endif;
040400091029             endif;
040500091029             if  dsp_aid = c_F05  or  dsp_aid = c_F06  or
040600091029                 dsp_aid = c_F16;
040700091029               $Video  = 'W1';
040800091029               reset  $InzW01;
040900091029             endif;
041000091029
041100091029         ENDSL;
041200091029
041300091029       ENDSR;
041400091029
041500091029       //--------------------------------------------------------------
041600091029       //?Inizializzazione videata D02                                 ?
041700091029       //--------------------------------------------------------------
041800091029       BEGSR  sr_InzD02;
041900091029
042000091029         // -?Spegnimento degli indicatori di errore?
042100091029         %subst(IndDspF : 50) = *off;
042200091029
042300091029         //?Pulizia videata?
042400091029         clear  TB78D02;
042500091029
042600091029         // -?Reperimento dati del supporto dati da clienti a Bartolini?
042700091029         reset  $Immissione;
042800091029         reset  $Ripristino;
042900091029         k_TBEke1 = V1Cke1;
043000091029         chain  %kds(k05tntbe01)  TNTBE000;
043100091029         select;
043200091029           when  Not %found(TNTBE01L);
043300091029             $Immissione = *on;
043400091029           when  TBEatb <> *blank;
043500091029             $Ripristino = *on;
043600091029         endsl;
043700091029
043800091029         // -?Caricamento dati della causale contatto marketing?
043900091029         if  %found(TNTBE01L);
044000091029           exsr  sr_RieD02;
044100091029         endif;
044200091029
044300091029         // -?Impostazione tipo manutenzione?
044400091029         select;
044500091029           when  $Immissione;
044600091029             V1Topz = 'IMMISSIONE';
044700091029           when  $Ripristino;
044800091029             V1Topz = 'RIPRISTINO';
044900091029           other;
045000091029             V1Topz = ' MODIFICA ';
045100091029         endsl;
045200091029
045300091029         // -?Impostazione indicatori per abilitazione tasti funzionali?
045400091029         exsr  sr_SetIndD02;
045500091029
045600091029       ENDSR;
045700091029
045800091029       //--------------------------------------------------------------
045900091029       //?Caricamento singola riga di testo nella videata D02          ?
046000091029       //--------------------------------------------------------------
046100091029       BEGSR  sr_RieD02;
046200091029
046300091029         clear  TB78D02;
046400091029
046500091029         //d3CS = TBEuni;
046600091029
046700091029         V2Cdes   = %subst( TBEuni : 1 : %len(V2Cdes) );
046800091029         V2Cdes2  = %subst( TBEuni : %len(V2Cdes) + 1 : %len(V2Cdes2) );
046900091029
047000091029       ENDSR;
047100091029
047200091029       //--------------------------------------------------------------
047300091029       //?Settaggio indicatori nella videata D02 per abilitazione      ?
047400091029       //?  tasti funzionali                                           ?
047500091029       //--------------------------------------------------------------
047600091029       BEGSR  sr_SetIndD02;
047700091029
047800091029         // -?F5=Ripristino?
047900091030         F5Attivo = *off;
048000091030         //F5Attivo  = (NOT $Immissione  and  $Ripristino);
048100091029
048200091029         // -?F6=Conferma?
048300091029         F6Attivo  = (NOT F5Attivo);
048400091029
048500091029         // -?F12=Ritorno (se NON richiamato da altro *pgm)?
048600091029         F12Attivo = *on;
048700091029
048800091029         // -?F16=Annullamento?
048900091030         F16Attivo = *off;
049000091030         //F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
049100091029
049200091029       ENDSR;
049300091029
049400091029       //--------------------------------------------------------------
049500091029       //?Controllo dati videata D02                                   ?
049600091029       //--------------------------------------------------------------
049700091029       BEGSR  sr_CtrD02;
049800091029
049900091029         %subst(IndDspF : 50) = *off;
050000091029
050100091029         select;
050200091029
050300091029           // -?Descrizione obbligatoria?
050400091029           when  V2Cdes = *blank  and  V2Cdes2 = *blank;
050500091029             ErrMessage  = *on;
050600091029             ErrGenerico = *on;
050700091029             PosCurDES   = *on;
050800091029             V1Dmsg = $Msg(02);
050900091029             leavesr;
051000091029
051100091029         endsl;
051200091029
051300091029       ENDSR;
051400091029
051500091029       //--------------------------------------------------------------
051600091029       //?Gestione trasmissione aggiornamento                          ?
051700091029       //--------------------------------------------------------------
051800091029       BEGSR  sr_GesW01;
051900091029
052000091029         // - Inizializzazione videata
052100091029         if $InzW01 = *on;
052200091029           exsr  sr_InzW01;
052300091029           $InzW01 = *off;
052400091029         endif;
052500091029
052600091029         // - Emissione window
052700091029         if  TBXftt = 'S';
052800091029           exfmt  TB78W01;
052900091029           ErrMessage  = *off;
053000091029           ErrGenerico = *off;
053100091029           clear  V1Dmsg;
053200091029         else;
053300091029           dsp_aid = c_F06;
053400091029         endif;
053500091029
053600091029         select;
053700091029
053800091029           // - F12=Ritorno (gestito solo se emesso W01)
053900091029           when  dsp_aid = c_F12;
054000091029             $Video = 'D2';
054100091029
054200091029           // - F6=Conferma
054300091029           when  dsp_aid = c_F06;
054400091029             exsr  sr_UpdTNTBE;
054500091029             // - Ritorno alla videata iniziale
054600091029             if  NOT  ErrGenerico;
054700091029               clear  V1Topz;
054800091029               $Video  = 'D1';
054900091029               $InzD01 = *on;
055000091029             endif;
055100091029         endsl;
055200091029
055300091029       ENDSR;
055400091029
055500091029       //--------------------------------------------------------------
055600091029       //?Inizializzazione videata W01                                 ?
055700091029       //--------------------------------------------------------------
055800091029       BEGSR  sr_InzW01;
055900091029
056000091029         clear TB78W01;
056100091029
056200091029         if  $Immissione;
056300091029
056400091029           // - Se immissione
056500091029           W1ftt  = TBXftt;
056600091029
056700091029         else;
056800091029
056900091029           // - Se NON immissione:
057000091029           //   visualizza i dati relativi all'ultima trasmissione
057100091029           W1ftt  = TBEftt;
057200091029           W1flt  = TBEflt;
057300091029           W1ftr  = TBEftr;
057400091029           if TBEdtr <> *zero;
057500091029             wDate_EUR = %date(TBEdtr : *iso);
057600091029             W1dtr     = %dec(wDate_EUR);
057700091029           endif;
057800091029
057900091029         endif;
058000091029
058100091029       ENDSR;
058200091029
058300091029       //--------------------------------------------------------------
058400091029       //?Aggiornamento records TNTBE00F (tab. "3CS")                  ?
058500091029       //--------------------------------------------------------------
058600091029       BEGSR  sr_UpdTNTBE;
058700091029
058800091029         // -?Aggancio record di TNTBE00F?
058900091029         k_TBEcod = c_Tab;
059000091029         k_TBEke1 = V1Cke1;
059100091029         clear  k_TBEke2;
059200091029         clear  k_TBElin;
059300091029         clear  k_TBEsif;
059400091029         //k_TBElin = TBXlin;
059500091029         //k_TBEsif = TBXsif;
059600091029
059700091029         chain  %kds(K05tntbe01) TNTBE000;
059800091029
059900091029         // -?Impostazione campi chiave se inserimento?
060000091029         if  NOT %found(TNTBE01L);
060100091029           clear TNTBE000;
060200091029           TBEcod = k_TBEcod;
060300091029           TBEke1 = k_TBEke1;
060400091029           TBEke2 = k_TBEke2;
060500091029           TBElin = k_TBElin;
060600091029           TBEsif = k_TBEsif;
060700091029           TBEapl = TBXapl;
060800091029         endif;
060900091029
061000091029         // -?Impostazione dati tabella da aggiornare?
061100091029         //exsr  sr_Rie5CS;
061200091029         //TBEuni = d5CS;
061300091029         TBEuni = V2Cdes + V2Cdes2;
061400091029
061500091029         // -?Aggiornamento tab. "3CS"?
061600091029         SELECT;
061700091029
061800091029           // -?F5=Ripristino?
061900091029           WHEN  $Ripristino;
062000091029             clear  TBEatb;
062100091029             TBEftt = W1ftt;
062200091029             clear  TBEftr;
062300091029             //_______________
062400091029             UPDATE  TNTBE000;
062500091029             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
062600091029
062700091029           // -?F16=Annullamento?
062800091029           WHEN  $Annullamento;
062900091029             TBEatb = 'A';
063000091029             TBEftt = W1ftt;
063100091029             clear  TBEftr;
063200091029             //_______________
063300091029             UPDATE  TNTBE000;
063400091029             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
063500091029
063600091029           // -?Immissione o Modifica?
063700091029           OTHER;
063800091029
063900091029             TBEapl = TBXapl;
064000091029             clear  TBEatb;
064100091029             TBEftt = W1ftt;
064200091029             clear  TBEflt;
064300091029             clear  TBEftr;
064400091029             //clear TBEdtr;
064500091029
064600091029             // - Aggiornamento record
064700091029             if  %found(TNTBE01L);
064800091029               //_____________
064900091029               UPDATE TNTBE000;
065000091029               //¯¯¯¯¯¯¯¯¯¯¯¯¯
065100091029             else;
065200091029               //_____________
065300091029               WRITE  TNTBE000;
065400091029               //¯¯¯¯¯¯¯¯¯¯¯¯¯
065500091029             endif;
065600091029
065700091029         ENDSL;
065800091029
065900091029       ENDSR;
066000091029
066100091029       //--------------------------------------------------------------
066200091029       //?Caricamento dati in tab. "3CS"                               ?
066300091029       //--------------------------------------------------------------
066400091029       //BEGSR  sr_Rie3CS;
066500091029       //
066600091029       //  clear  d3CS;
066700091029       //
066800091029       //  ...;
066900091029       //
067000091029       //ENDSR;
067100091029
067200091029       //--------------------------------------------------------------
067300091029       //?Operazioni finali.                                           ?
067400091029       //--------------------------------------------------------------
067500091029       BEGSR  sr_RoutEnd;
067600091029
067700091029         //?Chiusura funzioni precedentemente aperte?
067800091029         clear  TIBS02ds;
067900091029         T02tla = 'C';
068000091029         TNTBE_RicercaControllo ( kpjba : tibs02ds );
068100091029
068200091029         //?Chiusura pgm?
068300091029         return;
068400091029
068500091029       ENDSR;
068600091029
068700091029      /end-free
068800091029
068900091029       //--------------------------------------------------------------
069000091029       //?Definizione schiere a tempo di compilazione                  ?
069100091029       //--------------------------------------------------------------
069200091029
069300091029** - $Msg -------------------------------------------------------------------*
069400110512Immettere il codice "tipo supporto dati da cliente a BRT"                       1
069500091029Descrizione obbligatoria                                                        2
