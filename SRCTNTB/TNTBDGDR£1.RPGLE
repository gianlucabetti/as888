000100121107       //==============================================================
000200121107       //?Gestione tabella "DGD":                                      ?
000300121107       //?Periodi per Distinte Consegna & per Prepagati                ?
000400121107       //==============================================================
000500121107
000600121107       //--------------------------------------------------------------
000700121107       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800121107       //--------------------------------------------------------------
000900121107
001000121107     /*PRM  dbgview(*source)
001100121107     /*END
001200121107
001300121107       //--------------------------------------------------------------
001400121107       //?Specifiche di controllo.                                     ?
001500121107       //--------------------------------------------------------------
001600121107
001700121107     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800121107     h dftactgrp(*no)
001900121107
002000121107       //--------------------------------------------------------------
002100121107       //?Dichiarazione file.                                          ?
002200121107       //--------------------------------------------------------------
002300121107
002400121107       // -?Tabella "DGD"?
002500121107     fTNTBE01L  Uf A e           k disk
002600121107
002700121107       // -?Video Gestione?
002800121107     fTNTBDGDD  cf   e             workstn
002900121107     f                                     indds(IndDspF)
003000121107     f                                     infds(InfDspF)
003100121107
003200121107       //--------------------------------------------------------------
003300121107       //?Definizione costanti.                                        ?
003400121107       //--------------------------------------------------------------
003500121107
003600121107       // -?Codice tabella in gestione?
003700121107     d c_Tab           c                   const('DGD')
003800121107
003900121107       // -?Tasti funzionali a video?
004000121107     d c_F01           c                   const(x'31')
004100121107     d c_F02           c                   const(x'32')
004200121107     d c_F03           c                   const(x'33')
004300121107     d c_F04           c                   const(x'34')
004400121107     d c_F05           c                   const(x'35')
004500121107     d c_F06           c                   const(x'36')
004600121107     d c_F07           c                   const(x'37')
004700121107     d c_F08           c                   const(x'38')
004800121107     d c_F09           c                   const(x'39')
004900121107     d c_F10           c                   const(x'3A')
005000121107     d c_F11           c                   const(x'3B')
005100121107     d c_F12           c                   const(x'3C')
005200121107     d c_F13           c                   const(x'B1')
005300121107     d c_F14           c                   const(x'B2')
005400121107     d c_F15           c                   const(x'B3')
005500121107     d c_F16           c                   const(x'B4')
005600121107     d c_F17           c                   const(x'B5')
005700121107     d c_F18           c                   const(x'B6')
005800121107     d c_F19           c                   const(x'B7')
005900121107     d c_F20           c                   const(x'B8')
006000121107     d c_F21           c                   const(x'B9')
006100121107     d c_F22           c                   const(x'BA')
006200121107     d c_F23           c                   const(x'BB')
006300121107     d c_F24           c                   const(x'BC')
006400121107     d c_Enter         c                   const(x'F1')
006500121107     d c_RollDown      c                   const(x'F4')
006600121107     d c_RollUp        c                   const(x'F5')
006700121107
006800121107       // -?Costante per controllo "caratteri solo numerici"?
006900121107     d c_Digits        c                   const('0123456789')
007000121107
007100121107       //--------------------------------------------------------------
007200121107       //?Definizione schiere.                                         ?
007300121107       //--------------------------------------------------------------
007400121107
007500121107       // -?Messaggi di errore?
007600121108     d $Msg            s             78    dim( 6)  ctdata  perrcd( 1)
007700121107
007800121107       //--------------------------------------------------------------
007900121107       //?Definizione aree dati.                                       ?
008000121107       //--------------------------------------------------------------
008100121107
008200121107       // -?Dati utente?
008300121107     d §AzUte        e ds                  extname(AZUTE00F)
008400121107     d                                     dtaara
008500121107     d §DatiUte      e ds                  extname(dDatiUte)
008600121107     d                                     dtaara
008700121107
008800121107       //--------------------------------------------------------------
008900121107       //?Definizione strutture dati.                                  ?
009000121107       //--------------------------------------------------------------
009100121107
009200121107       // -?Status ds?
009300121107     d Status         sds
009400121107     d  SDSpgm           *proc
009500121107
009600121107       // -?InfDS?
009700121107     d InfDspF         ds
009800121107     d   dsp_aid             369    369a
009900121107
010000121107       // -?Indicatori su DspF?
010100121107     d IndDspF         ds                  inz
010200121107         // -?Abilitazione tasti funzionali?
010300121107     d   F3Attivo                      n   overlay(IndDspF : 03)
010400121107     d   F5Attivo                      n   overlay(IndDspF : 05)
010500121107     d   F6Attivo                      n   overlay(IndDspF : 06)
010600121107     d   F12Attivo                     n   overlay(IndDspF : 12)
010700121107     d   F16Attivo                     n   overlay(IndDspF : 16)
010800121107         // -?Emissione messaggio di errore?
010900121107     d   ErrMessage                    n   overlay(IndDspF : 28)
011000121107         // -?Posizionamento cursore & segnalazione errore?
011100121107     d   PosCurKE1                     n   overlay(IndDspF : 50)
011200121107     d   PosCurDGG                     n   overlay(IndDspF : 51)
011300121107     d   PosCurDEI                     n   overlay(IndDspF : 52)
011400121107     d   PosCurDEF                     n   overlay(IndDspF : 53)
011500121107     d   PosCurDGE                     n   overlay(IndDspF : 54)
011600121107     d   PosCurDII                     n   overlay(IndDspF : 55)
011700121107     d   PosCurDIF                     n   overlay(IndDspF : 56)
011800121107     d   PosCurDGI                     n   overlay(IndDspF : 57)
011900121107     d   PosCurDGBAR                   n   overlay(IndDspF : 58)
012000121108     d   PosCurDGPT                    n   overlay(IndDspF : 59)
012100121108     d   PosCurDDC                     n   overlay(IndDspF : 60)
012200121108     d   PosCurDTADA                   n   overlay(IndDspF : 61)
012300121108     d   PosCurDTAA                    n   overlay(IndDspF : 62)
012400121108     d   PosCurDBPD                    n   overlay(IndDspF : 63)
012500121108     d   PosCurDBPA                    n   overlay(IndDspF : 64)
012600121108     d   PosCurDCPD                    n   overlay(IndDspF : 65)
012700121108     d   PosCurDCPA                    n   overlay(IndDspF : 66)
012800121107         //   -?Riemissione videata?
012900121107     d   ErrGenerico                   n   overlay(IndDspF : 99)
013000121107
013100121107       // -?Parametri ricevuti?
013200121107     d KPJBA         e ds
013300121107
013400121107       // -?Dati record principale della tabella?
013500121107     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013600121107     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013700121107     d                                          prefix(TBX:3)
013800121107
013900121107       // -?Tabelle usate:?
014000121107       // ·?"DGD" = Periodi per Distinta Consegna e per Prepagato?
014100121107     d dDGD          e ds                  inz   qualified
014200121107
014300121107       //--------------------------------------------------------------
014400121107       //?Definizione variabili globali.                               ?
014500121107       //--------------------------------------------------------------
014600121107
014700121107       // -?Flags booleani?
014800121107     d $Fine           s               n   inz
014900121107     d $Err            s               n   inz
015000121107     d $InzD01         s               n   inz(*on)
015100121107     d $InzD02         s               n   inz(*on)
015200121107     d $InzW01         s               n   inz(*on)
015300121107     d $Immissione     s               n   inz
015400121107     d $Ripristino     s               n   inz
015500121107     d $Annullamento   s               n   inz
015600121107
015700121107       // -?Indici di schiera?
015800121107     d xx              s              3  0 inz
015900121107
016000121107       // -?Variabili per la gestione del video?
016100121107     d $Video          s              2    inz('D1')
016200121107
016300121107       // -?Campi di comodo?
016400121107     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
016500121107     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
016600121107     d*// wDate           s              8  0 inz
016700121107     d wDateAlfa       s              8    inz
016800121107
016900121107       //--------------------------------------------------------------
017000121107       //?Definizione prototipi procedure.                             ?
017100121107       //--------------------------------------------------------------
017200121107
017300121107       // -?Reperimento dati utente?
017400121107     d TIBS34ds      e ds                  inz
017500121107      /copy gaitrasrc/srcProtoPR,TIBS34R
017600121107
017700121107       // -?Ricerca e Controllo nuove tabelle (TNTBE)?
017800121107     d TIBS02ds      e ds                  inz
017900121107     d   T02mod      e                     inz('R')
018000121107     d   T02cod      e                     inz('DGD')
018100121107      /copy gaitrasrc/srcProtoPR,TIBS02R
018200121107
018300121107       // -?Ricerca filiale in organigramma?
018400121107      /copy gaitrasrc/srcProtoPI,TNSD24R
018500121107      /copy gaitrasrc/srcProtoPR,TNSD24R
018600121107
018700121107       // -?Controllo/Inversione data?
018800121107     d WLBdat          ds                  inz
018900121107     d  G08dat                        8  0 inz
019000121107     d  G08inv                        8  0 inz
019100121107     d  G08err                        1    inz('3')
019200121107     d  G08tgi                        5  0 inz
019300121107      /copy gaitrasrc/srcProtoPR,XSRDA8
019400121107
019500121107       //--------------------------------------------------------------
019600121107       //?Definizione key-list.                                        ?
019700121107       //--------------------------------------------------------------
019800121107
019900121107       // -?File TNTBE01L?
020000121107     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020100121107     d                                     prefix(k_)   inz
020200121107
020300121107       //--------------------------------------------------------------
020400121107       //?Riepilogo indicatori utilizzati.                             ?
020500121107       //--------------------------------------------------------------
020600121107       //--------------------------------------------------------------
020700121107
020800121107       //--------------------------------------------------------------
020900121107       //?M A I N - L I N E                                            ?
021000121107       //--------------------------------------------------------------
021100121107
021200121107     c     *Entry        plist
021300121107     c                   parm                    KPJBA
021400121107
021500121107      /free
021600121107
021700121107       // -?Operazioni iniziali?
021800121107       exsr  sr_RoutInz;
021900121107
022000121107       // -?Gestione videate?
022100121107       DOW  $Fine = *off;
022200121107
022300121107         select;
022400121107
022500121107           // -?Gestione videata D1 (Manutenzione dati singolo record)?
022600121107           when  $Video = 'D2';
022700121107             exsr  sr_GesD02;
022800121107
022900121107           // -?Gestione videata D1 (Richiesta codice filiale)?
023000121107           when  $Video = 'D1';
023100121107             exsr  sr_GesD01;
023200121107
023300121107           // -?Gestione videata W1 (Richiesta dati per trasmissione record)?
023400121107           when  $Video = 'W1';
023500121107             exsr  sr_GesW01;
023600121107
023700121107           // -?? ? ? ? ??
023800121107           other;
023900121107             $Fine = *on;
024000121107
024100121107         endsl;
024200121107
024300121107       ENDDO;
024400121107
024500121107       // -?Operazioni finali?
024600121107       exsr  sr_RoutEnd;
024700121107
024800121107       //--------------------------------------------------------------
024900121107       //?Operazioni iniziali.                                         ?
025000121107       //--------------------------------------------------------------
025100121107       BEGSR  sr_RoutInz;
025200121107
025300121107         *inLR = *on;
025400121107
025500121107         // -?Reperimento dati job?
025600121107         exsr  sr_DatiJob;
025700121107
025800121107         // -?Impostazione nome programma a video?
025900121107         V1Tpgm = SDSpgm;
026000121107
026100121107         // -?Reperimento data odierna?
026200121107         //wDate = %int( %subst( %char( %dec( %timestamp() ) )
026300121107         //                      : 1 : 8 ) );
026400121107
026500121107         // -?Aggancio dati generali della tabella in esame?
026600121107         clear  xTNTBEds;
026700121107         clear  k05tntbe01;
026800121107         k_TBEke1 = *zero;
026900121107         %subst( k_TBEke1 : %len(k_TBEke1) - %len(c_Tab) + 1 )
027000121107                  = c_Tab;
027100121107         k_TBEsif = KNSIF;
027200121107         chain(n)  %kds(k05tntbe01)  TNTBE000;
027300121107         if  not  %found(TNTBE01L);
027400121107           clear  k_TBEsif;
027500121107           chain(n)  %kds(k05tntbe01)  TNTBE000;
027600121107         endif;
027700121107         if  %found(TNTBE01L);
027800121107           xTNTBEds = TNTBEds;
027900121107         else;
028000121107           clear  xTNTBEds;
028100121107         endif;
028200121107
028300121107         // -?Impostazione iniziale / pulizia dei campi chiave?
028400121107         clear  k05tntbe01;
028500121107         k_TBEcod = c_Tab;
028600121109
028700121109         // -?Forzature per scartare la 1ª videata (richiesta codice)?
028800121109         //  ?- FINCHÈ C'È SOLO IL CODICE "1 "... -                  ?
028900121109         exsr  sr_InzD01;
029000121109         V1Cke1 = '1 ';
029100121109         exsr  sr_CtrD01;
029200121109         if  Not ErrGenerico;
029300121109           $Video = 'D2';
029400121109           reset  $InzD02;
029500121109         endif;
029600121107
029700121107       ENDSR;
029800121107
029900121107       //--------------------------------------------------------------
030000121107       //?Reperimento Dati del job (Utente/Operativi).                 ?
030100121107       //--------------------------------------------------------------
030200121107       BEGSR  sr_DatiJob;
030300121107
030400121107         in(E) §AzUte;
030500121107         if NOT %error;
030600121107           in(E) §DatiUte;
030700121107         endif;
030800121107         if %error or RSut = *blanks;
030900121107           clear TIBS34ds;
031000121107           tibs34r ( tibs34ds );
031100121107           in §AzUte;
031200121107           in §DatiUte;
031300121107         endif;
031400121107
031500121107       ENDSR;
031600121107
031700121107       //--------------------------------------------------------------
031800121107       //?Gestione videata D01.                                        ?
031900121107       //--------------------------------------------------------------
032000121107       BEGSR  sr_GesD01;
032100121107
032200121107         // -?Inizializzazione videata?
032300121107         if  $InzD01 = *on;
032400121107           exsr  sr_InzD01;
032500121107           $InzD01 = *off;
032600121107         endif;
032700121107         clear  V1Topz;
032800121107
032900121107         // -?(Dis)Abilitazione tasti funzionali?
033000121107         F3Attivo  = *on;
033100121107         F5Attivo  = *off;
033200121107         F6Attivo  = *off;
033300121107         F12Attivo = *off;
033400121107         F16Attivo = *off;
033500121107
033600121107         // -?Emissione videata?
033700121107         write  tbDGDt01;
033800121107         write  tbDGDp01;
033900121107         exfmt  tbDGDd01;
034000121107
034100121107         clear  VIDmsg;
034200121107         reset  ErrMessage;
034300121107         reset  ErrGenerico;
034400121107
034500121107         SELECT;
034600121107
034700121107           // -?F3=Fine?
034800121107           WHEN  dsp_aid = c_F03;
034900121107             $Fine = *on;
035000121107
035100121107           // -?Invio?
035200121107           OTHER;
035300121107             exsr  sr_CtrD01;
035400121107             if  ErrGenerico = *off;
035500121107               $Video    = 'D2';
035600121107               $InzD02   = *on;
035700121107             endif;
035800121107
035900121107         ENDSL;
036000121107
036100121107       ENDSR;
036200121107
036300121107       //--------------------------------------------------------------
036400121107       //?Inizializzazione videata D01.                                ?
036500121107       //--------------------------------------------------------------
036600121107       BEGSR  sr_InzD01;
036700121107
036800121107         clear  tbDGDd01;
036900121107
037000121107         V1Cke1 = '?';
037100121107
037200121107       ENDSR;
037300121107
037400121107       //--------------------------------------------------------------
037500121107       //?Controllo dati immessi nella videata D01.                    ?
037600121107       //--------------------------------------------------------------
037700121107       BEGSR  sr_CtrD01;
037800121107
037900121107         // -?Spegnimento indicatori di posizionamento cursore?
038000121107         %subst(IndDspF : 28) = *off;
038100121107
038200121107         SELECT;
038300121107
038400121107           // -?Ricerca dati già inseriti?
038500121107           WHEN  %scan('?' : V1Cke1) > *zero;
038600121107             clear  V1Cke1;
038700121107             reset  TIBS02ds;
038800121107             //T02mod = 'R';                //?(già così)?
038900121107             //T02cod = 'DGD';              //?(già così)?
039000121107             T02sif = knsif;
039100121107             TNTBE_RicercaControllo (kpjba : TIBS02ds);
039200121107             if  T02err = *blank;
039300121107               V1Cke1 = T02ke1;
039400121107               //dDGD   = T02uni;
039500121107             endif;
039600121107             ErrGenerico = *on;
039700121107             PosCurKE1   = *on;
039800121107             leavesr;
039900121107
040000121107           // -?Controllo immissione Codice?
040100121107           WHEN  V1Cke1 = *blank;
040200121107             ErrGenerico = *on;
040300121107             ErrMessage  = *on;
040400121107             PosCurKE1   = *on;
040500121107             VIDmsg = $Msg(01);
040600121107             leavesr;
040700121107
040800121107         ENDSL;
040900121107
041000121107       ENDSR;
041100121107
041200121107       //--------------------------------------------------------------
041300121107       //?Gestione videata D02                                         ?
041400121107       //--------------------------------------------------------------
041500121107       BEGSR  sr_GesD02;
041600121107
041700121107         // -?Inizializzazione videata?
041800121107         if  $InzD02 = *on;
041900121107           exsr sr_InzD02;
042000121107           $InzD02 = *off;
042100121107         endif;
042200121107
042300121107         // -?Emissione Testata e Piede con tasti funzionali abilitati?
042400121107         if  errGenerico = *off;
042500121107           write  tbDGDt01;
042600121107           write  tbDGDd01;
042700121107           write  Protect;
042800121107           write  tbDGDp01;
042900121107         endif;
043000121107
043100121107         // -?Emissione videata?
043200121107         exfmt  tbDGDd02;
043300121107
043400121107         clear  VIDmsg;
043500121107         reset  ErrMessage;
043600121107         reset  ErrGenerico;
043700121107
043800121107         SELECT;
043900121107
044000121107           // -?F3=Fine?
044100121107           WHEN  dsp_aid = c_F03;
044200121107             unlock TNTBE01L;
044300121107             $Fine = *on;
044400121107
044500121107           // -?F12=Ritorno?
044600121107           WHEN  dsp_aid = c_F12;
044700121107             unlock TNTBE01L;
044800121107             reset  $Video;
044900121107             clear  V1Topz;
045000121107
045100121107           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
045200121107           OTHER;
045300121107             $Ripristino   = (dsp_aid = c_F05);
045400121107             $Annullamento = (dsp_aid = c_F16);
045500121107             // -?Controlli eseguiti NON se F16=Annullamento?
045600121107             if  Not $Annullamento;
045700121107               exsr  sr_CtrD02;
045800121107             endif;
045900121107             select;
046000121107               // -?Rilevato errore?
046100121107               when  ErrGenerico;
046200121107                 leavesr;
046300121107               // -?Richiesta trasmissione record?
046400121107               when  dsp_aid = c_F05  or
046500121107                     dsp_aid = c_F06  or
046600121107                     dsp_aid = c_F16;
046700121107                 $Video  = 'W1';
046800121107                 reset  $InzW01;
046900121107             endsl;
047000121107
047100121107         ENDSL;
047200121107
047300121107       ENDSR;
047400121107
047500121107       //--------------------------------------------------------------
047600121107       //?Inizializzazione videata D02.                                ?
047700121107       //--------------------------------------------------------------
047800121107       BEGSR  sr_InzD02;
047900121107
048000121107         // -?Spegnimento degli indicatori di errore?
048100121107         %subst(IndDspF : 50) = *off;
048200121107
048300121107         // -?Pulizia videata?
048400121107         clear  tbDGDd02;
048500121107
048600121107         // -?Reperimento dati della filiale di emissione?
048700121107         reset  $Immissione;
048800121107         reset  $Ripristino;
048900121107         reset  $Annullamento;
049000121107         k_TBEke1 = V1Cke1;
049100121107         chain  %kds(k05tntbe01)  TNTBE000;
049200121107         select;
049300121107           when  Not %found(TNTBE01L);
049400121107             $Immissione = *on;
049500121107           when  TBEatb <> *blank;
049600121107             $Ripristino = *on;
049700121107         endsl;
049800121107
049900121107         // -?Caricamento dati della tabella a video?
050000121107         if  %found(TNTBE01L);
050100121107           exsr  sr_RieD02;
050200121107         endif;
050300121107
050400121107         // -?Impostazione tipo manutenzione?
050500121107         select;
050600121107           when  $Immissione;
050700121107             V1Topz = 'IMMISSIONE';
050800121107           when  $Ripristino;
050900121107             V1Topz = 'RIPRISTINO';
051000121107           other;
051100121107             V1Topz = ' MODIFICA ';
051200121107         endsl;
051300121107
051400121107         // -?Impostazione indicatori per abilitazione tasti funzionali?
051500121107         exsr  sr_AbilitFxxD02;
051600121107
051700121107       ENDSR;
051800121107
051900121107       //--------------------------------------------------------------
052000121107       //?Caricamento dati del singolo record della tab. "DGD"         ?
052100121107       //?nella videata D02.                                           ?
052200121107       //--------------------------------------------------------------
052300121107       BEGSR  sr_RieD02;
052400121107
052500121107         dDGD = TBEuni;
052600121107
052700121107         // -?Campi hidden?
052800121107
052900121107         // -?Campi i/o?
053000121107         V1dgg   = dDGD.§DGDgg;
053100121107         if  dDGD.§DGDei > *zero;
053200121107           wDate_EUR = %date( ( *year * 10000 ) + dDGD.§DGDei : *iso );
053300121107           V1dei   = ( ( %dec(wDate_EUR) - *year ) / 10000 );
053400121107         endif;
053500121107         if  dDGD.§DGDef > *zero;
053600121107           wDate_EUR = %date( ( *year * 10000 ) + dDGD.§DGDef : *iso );
053700121107           V1def   = ( ( %dec(wDate_EUR) - *year ) / 10000 );
053800121107         endif;
053900121107         V1dge   = dDGD.§DGDge;
054000121107         if  dDGD.§DGDii > *zero;
054100121107           wDate_EUR = %date( dDGD.§DGDii : *iso );
054200121107           V1dii   = %dec(wDate_EUR);
054300121107         endif;
054400121107         if  dDGD.§DGDif > *zero;
054500121107           wDate_EUR = %date( dDGD.§DGDif : *iso );
054600121107           V1dif   = %dec(wDate_EUR);
054700121107         endif;
054800121107         V1dgi   = dDGD.§DGDgi;
054900121107         V1dgBar = dDGD.§DGDgBar;
055000121108         V1dgpt  = dDGD.§DGDgpt;
055100121107         V1ddc   = dDGD.§DGDdc;
055200121107         if  dDGD.§DGDtaDa > *zero;
055300121107           wDate_EUR = %date( dDGD.§DGDtaDa : *iso );
055400121107           V1dtaDa = %dec(wDate_EUR);
055500121107         endif;
055600121107         if  dDGD.§DGDtaA > *zero;
055700121107           wDate_EUR = %date( dDGD.§DGDtaA : *iso );
055800121107           V1dtaA  = %dec(wDate_EUR);
055900121107         endif;
056000121107         if  dDGD.§DGDbpd > *zero;
056100121107           wDate_EUR = %date( dDGD.§DGDbpd : *iso );
056200121107           V1dbpd  = %dec(wDate_EUR);
056300121107         endif;
056400121107         if  dDGD.§DGDbpa > *zero;
056500121107           wDate_EUR = %date( dDGD.§DGDbpa : *iso );
056600121107           V1dbpa  = %dec(wDate_EUR);
056700121107         endif;
056800121107         if  dDGD.§DGDcpd > *zero;
056900121107           wDate_EUR = %date( dDGD.§DGDcpd : *iso );
057000121107           V1dcpd  = %dec(wDate_EUR);
057100121107         endif;
057200121107         if  dDGD.§DGDcpa > *zero;
057300121107           wDate_EUR = %date( dDGD.§DGDcpa : *iso );
057400121107           V1dcpa  = %dec(wDate_EUR);
057500121107         endif;
057600121107
057700121107       ENDSR;
057800121107
057900121107       //--------------------------------------------------------------
058000121107       //?Settaggio indicatori nella videata D02 per abilitazione      ?
058100121107       //?  tasti funzionali.                                          ?
058200121107       //--------------------------------------------------------------
058300121107       BEGSR  sr_AbilitFxxD02;
058400121107
058500121107         // -?F3=Fine?
058600121107         F3Attivo  = *on;
058700121107
058800121107         // -?F5=Ripristino?
058900121107         F5Attivo  = (NOT $Immissione  and  $Ripristino);
059000121107
059100121107         // -?F6=Conferma?
059200121107         F6Attivo  = (NOT F5Attivo);
059300121107
059400121107         // -?F12=Ritorno?
059500121109         //  ?(disabilitato finchè esiste solo il cod. "1")?
059600121109         //F12Attivo = *on;
059700121109         F12Attivo = *off;
059800121107
059900121107         // -?F16=Annullamento?
060000121107         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
060100121107
060200121107       ENDSR;
060300121107
060400121107       //--------------------------------------------------------------
060500121107       //?Controllo dati videata D02.                                  ?
060600121107       //--------------------------------------------------------------
060700121107       BEGSR  sr_CtrD02;
060800121108
060900121108         clear  dDGD;
061000121107
061100121107         // -?Spegnimento degli indicatori di errore?
061200121107         %subst(IndDspF : 50) = *off;
061300121107
061400121107         // -?Ctrl giorni limite di manutenzionabilità Distinta Consegna?
061500121107         select;
061600121107           when  V1dgg   = *zero;
061700121107             ErrGenerico = *on;
061800121107             ErrMessage  = *on;
061900121107             PosCurDGG   = *on;
062000121107             VIDmsg = $Msg(02);
062100121107             leavesr;
062200121107           when  V1dgg   < *zero;
062300121107             ErrGenerico = *on;
062400121107             ErrMessage  = *on;
062500121107             PosCurDGG   = *on;
062600121107             VIDmsg = $Msg(03);
062700121107             leavesr;
062800121107         endsl;
062900121107
063000121107         // -?Ctrl date del periodo di trazioni ridotte estive?
063100121107         select;
063200121107           when  V1dei   = *zero;
063300121107             ErrGenerico = *on;
063400121107             ErrMessage  = *on;
063500121107             PosCurDEI   = *on;
063600121107             VIDmsg = $Msg(04);
063700121107             leavesr;
063800121107           when  V1dei   < *zero;
063900121107             ErrGenerico = *on;
064000121107             ErrMessage  = *on;
064100121107             PosCurDEI   = *on;
064200121107             VIDmsg = $Msg(05);
064300121107             leavesr;
064400121108           other;
064500121107             clear WLBdat;
064600121107             G08dat = ( V1dei * 10000 ) + *year;
064700121107             XSRDA8(WLBdat);
064800121107             if  G08err = *on;
064900121107               ErrGenerico = *on;
065000121107               ErrMessage  = *on;
065100121107               PosCurDEI   = *on;
065200121107               VIDmsg = $Msg(05);
065300121107               leavesr;
065400121107             //else;
065500121107             //  V1dei  = ( G08dat - *year ) / 10000;
065600121107             endif;
065700121107         endsl;
065800121108         dDGD.§DGDei = G08inv - (*year * 10000);
065900121107         select;
066000121107           when  V1def   = *zero;
066100121107             ErrGenerico = *on;
066200121107             ErrMessage  = *on;
066300121107             PosCurDEF   = *on;
066400121107             VIDmsg = $Msg(04);
066500121107             leavesr;
066600121107           when  V1def   < *zero;
066700121107             ErrGenerico = *on;
066800121107             ErrMessage  = *on;
066900121107             PosCurDEF   = *on;
067000121107             VIDmsg = $Msg(05);
067100121107             leavesr;
067200121108           other;
067300121107             clear WLBdat;
067400121107             G08dat = ( V1def * 10000 ) + *year;
067500121107             XSRDA8(WLBdat);
067600121107             if  G08err = *on;
067700121107               ErrGenerico = *on;
067800121107               ErrMessage  = *on;
067900121107               PosCurDEF   = *on;
068000121107               VIDmsg = $Msg(05);
068100121107               leavesr;
068200121107             //else;
068300121107             //  V1def  = ( G08dat - *year ) / 10000;
068400121107             endif;
068500121107         endsl;
068600121108         dDGD.§DGDef = G08inv - (*year * 10000);
068700121108         if  dDGD.§DGDei > dDGD.§DGDef;
068800121108           ErrGenerico = *on;
068900121108           ErrMessage  = *on;
069000121108           PosCurDEF   = *on;
069100121108           VIDmsg = $Msg(06);
069200121108           leavesr;
069300121108         endif;
069400121108
069500121108         // -?Ctrl giorni di controllo nel periodo estivo?
069600121107         select;
069700121107           when  V1dge   = *zero;
069800121107             ErrGenerico = *on;
069900121107             ErrMessage  = *on;
070000121107             PosCurDGE   = *on;
070100121107             VIDmsg = $Msg(02);
070200121107             leavesr;
070300121107           when  V1dge   < *zero;
070400121107             ErrGenerico = *on;
070500121107             ErrMessage  = *on;
070600121107             PosCurDGE   = *on;
070700121107             VIDmsg = $Msg(03);
070800121107             leavesr;
070900121107         endsl;
071000121107
071100121107         // -?Ctrl date del periodo di trazioni ridotte invernali?
071200121107         select;
071300121107           when  V1dii   = *zero;
071400121107             ErrGenerico = *on;
071500121107             ErrMessage  = *on;
071600121107             PosCurDII   = *on;
071700121107             VIDmsg = $Msg(04);
071800121107             leavesr;
071900121107           when  V1dii   < *zero;
072000121107             ErrGenerico = *on;
072100121107             ErrMessage  = *on;
072200121107             PosCurDII   = *on;
072300121107             VIDmsg = $Msg(05);
072400121107             leavesr;
072500121108           other;
072600121107             clear WLBdat;
072700121107             G08dat = V1dii;
072800121107             XSRDA8(WLBdat);
072900121107             if  G08err = *on;
073000121107               ErrGenerico = *on;
073100121107               ErrMessage  = *on;
073200121107               PosCurDII   = *on;
073300121107               VIDmsg = $Msg(05);
073400121107               leavesr;
073500121107             else;
073600121107               V1dii  = G08dat;
073700121107             endif;
073800121107         endsl;
073900121108         dDGD.§DGDii = G08inv;
074000121107         select;
074100121107           when  V1dif   = *zero;
074200121107             ErrGenerico = *on;
074300121107             ErrMessage  = *on;
074400121107             PosCurDIF   = *on;
074500121107             VIDmsg = $Msg(04);
074600121107             leavesr;
074700121107           when  V1dif   < *zero;
074800121107             ErrGenerico = *on;
074900121107             ErrMessage  = *on;
075000121107             PosCurDIF   = *on;
075100121107             VIDmsg = $Msg(05);
075200121107             leavesr;
075300121108           other;
075400121107             clear WLBdat;
075500121107             G08dat = V1dif;
075600121107             XSRDA8(WLBdat);
075700121107             if  G08err = *on;
075800121107               ErrGenerico = *on;
075900121107               ErrMessage  = *on;
076000121107               PosCurDIF   = *on;
076100121107               VIDmsg = $Msg(05);
076200121107               leavesr;
076300121107             else;
076400121107               V1dif  = G08dat;
076500121107             endif;
076600121107         endsl;
076700121108         dDGD.§DGDif = G08inv;
076800121108         if  dDGD.§DGDii > dDGD.§DGDif;
076900121108           ErrGenerico = *on;
077000121108           ErrMessage  = *on;
077100121108           PosCurDIF   = *on;
077200121108           VIDmsg = $Msg(06);
077300121108           leavesr;
077400121108         endif;
077500121108
077600121108         // -?Ctrl giorni di controllo nel periodo invernale?
077700121107         select;
077800121107           when  V1dgi   = *zero;
077900121107             ErrGenerico = *on;
078000121107             ErrMessage  = *on;
078100121107             PosCurDGI   = *on;
078200121107             VIDmsg = $Msg(02);
078300121107             leavesr;
078400121107           when  V1dgi   < *zero;
078500121107             ErrGenerico = *on;
078600121107             ErrMessage  = *on;
078700121107             PosCurDGI   = *on;
078800121107             VIDmsg = $Msg(03);
078900121107             leavesr;
079000121107         endsl;
079100121107
079200121107         // -?Ctrl giorni limite per retrodatazione della data consegna?
079300121107         //  ?(BRT)?
079400121107         select;
079500121107           when  V1dgBar = *zero;
079600121107             ErrGenerico = *on;
079700121107             ErrMessage  = *on;
079800121107             PosCurDGBAR = *on;
079900121107             VIDmsg = $Msg(02);
080000121107             leavesr;
080100121107           when  V1dgBar < *zero;
080200121107             ErrGenerico = *on;
080300121107             ErrMessage  = *on;
080400121107             PosCurDGBAR = *on;
080500121107             VIDmsg = $Msg(03);
080600121107             leavesr;
080700121107         endsl;
080800121107
080900121107         // -?Ctrl giorni limite per retrodatazione della data consegna?
081000121107         //  ?(POSTE ITALIANE)?
081100121108         select;
081200121108           when  V1dgPt  = *zero;
081300121108             ErrGenerico = *on;
081400121108             ErrMessage  = *on;
081500121108             PosCurDGPT  = *on;
081600121108             VIDmsg = $Msg(02);
081700121108             leavesr;
081800121108           when  V1dgPt  < *zero;
081900121108             ErrGenerico = *on;
082000121108             ErrMessage  = *on;
082100121108             PosCurDGPT  = *on;
082200121108             VIDmsg = $Msg(03);
082300121108             leavesr;
082400121108         endsl;
082500121107
082600121107         // -?Ctrl giorni limite per la data di Fogli Vari e Distinte?
082700121107         select;
082800121107           when  V1ddc   = *zero;
082900121107             ErrGenerico = *on;
083000121107             ErrMessage  = *on;
083100121107             PosCurDDC   = *on;
083200121107             VIDmsg = $Msg(02);
083300121107             leavesr;
083400121107           when  V1ddc   < *zero;
083500121107             ErrGenerico = *on;
083600121107             ErrMessage  = *on;
083700121107             PosCurDDC   = *on;
083800121107             VIDmsg = $Msg(03);
083900121107             leavesr;
084000121107         endsl;
084100121107
084200121107         // -?Ctrl date del periodo nel quale NON controllare le?
084300121107         //  ?partite aperte durante le quadrature giornaliere?
084400121107         select;
084500121107           when  V1dtaDa = *zero;
084600121107             ErrGenerico = *on;
084700121107             ErrMessage  = *on;
084800121107             PosCurDTADA = *on;
084900121107             VIDmsg = $Msg(04);
085000121107             leavesr;
085100121107           when  V1dtaDa < *zero;
085200121107             ErrGenerico = *on;
085300121107             ErrMessage  = *on;
085400121107             PosCurDTADA = *on;
085500121107             VIDmsg = $Msg(05);
085600121107             leavesr;
085700121108           other;
085800121107             clear WLBdat;
085900121107             G08dat = V1dtaDa;
086000121107             XSRDA8(WLBdat);
086100121107             if  G08err = *on;
086200121107               ErrGenerico = *on;
086300121107               ErrMessage  = *on;
086400121107               PosCurDTADA = *on;
086500121107               VIDmsg = $Msg(05);
086600121107               leavesr;
086700121107             else;
086800121107               V1dtaDa = G08dat;
086900121107             endif;
087000121107         endsl;
087100121108         dDGD.§DGDtaDa = G08inv;
087200121108         select;
087300121108           when  V1dtaA  = *zero;
087400121108             ErrGenerico = *on;
087500121108             ErrMessage  = *on;
087600121108             PosCurDTAA  = *on;
087700121108             VIDmsg = $Msg(04);
087800121108             leavesr;
087900121108           when  V1dtaA  < *zero;
088000121108             ErrGenerico = *on;
088100121108             ErrMessage  = *on;
088200121108             PosCurDTAA  = *on;
088300121108             VIDmsg = $Msg(05);
088400121108             leavesr;
088500121108           other;
088600121108             clear WLBdat;
088700121108             G08dat = V1dtaA;
088800121108             XSRDA8(WLBdat);
088900121108             if  G08err = *on;
089000121108               ErrGenerico = *on;
089100121108               ErrMessage  = *on;
089200121108               PosCurDTAA  = *on;
089300121108               VIDmsg = $Msg(05);
089400121108               leavesr;
089500121108             else;
089600121108               V1dtaA  = G08dat;
089700121108             endif;
089800121108         endsl;
089900121108         dDGD.§DGDtaA  = G08inv;
090000121108         if  dDGD.§DGDtaDa > dDGD.§DGDtaA;
090100121108           ErrGenerico = *on;
090200121108           ErrMessage  = *on;
090300121108           PosCurDTAA  = *on;
090400121108           VIDmsg = $Msg(06);
090500121108           leavesr;
090600121108         endif;
090700121107
090800121107         // -?Ctrl date del periodo per blocco immissione prepagati?
090900121107         select;
091000121107           when  V1dbpd  = *zero;
091100121107             ErrGenerico = *on;
091200121107             ErrMessage  = *on;
091300121107             PosCurDBPD  = *on;
091400121107             VIDmsg = $Msg(04);
091500121107             leavesr;
091600121107           when  V1dbpd  < *zero;
091700121107             ErrGenerico = *on;
091800121107             ErrMessage  = *on;
091900121107             PosCurDBPD  = *on;
092000121107             VIDmsg = $Msg(05);
092100121107             leavesr;
092200121108           other;
092300121107             clear WLBdat;
092400121107             G08dat = V1dbpd;
092500121107             XSRDA8(WLBdat);
092600121107             if  G08err = *on;
092700121107               ErrGenerico = *on;
092800121107               ErrMessage  = *on;
092900121107               PosCurDBPD  = *on;
093000121107               VIDmsg = $Msg(05);
093100121107               leavesr;
093200121107             else;
093300121107               V1dbpd = G08dat;
093400121107             endif;
093500121107         endsl;
093600121108         dDGD.§DGDbpd  = G08inv;
093700121107         select;
093800121107           when  V1dbpa  = *zero;
093900121107             ErrGenerico = *on;
094000121107             ErrMessage  = *on;
094100121107             PosCurDBPA  = *on;
094200121107             VIDmsg = $Msg(04);
094300121107             leavesr;
094400121107           when  V1dbpa  < *zero;
094500121107             ErrGenerico = *on;
094600121107             ErrMessage  = *on;
094700121107             PosCurDBPA  = *on;
094800121107             VIDmsg = $Msg(05);
094900121107             leavesr;
095000121108           other;
095100121107             clear WLBdat;
095200121107             G08dat = V1dbpa;
095300121107             XSRDA8(WLBdat);
095400121107             if  G08err = *on;
095500121107               ErrGenerico = *on;
095600121107               ErrMessage  = *on;
095700121107               PosCurDBPA  = *on;
095800121107               VIDmsg = $Msg(05);
095900121107               leavesr;
096000121107             else;
096100121107               V1dbpa = G08dat;
096200121107             endif;
096300121107         endsl;
096400121108         dDGD.§DGDbpa  = G08inv;
096500121108         if  dDGD.§DGDbpd > dDGD.§DGDbpa;
096600121108           ErrGenerico = *on;
096700121108           ErrMessage  = *on;
096800121108           PosCurDBPA  = *on;
096900121108           VIDmsg = $Msg(06);
097000121108           leavesr;
097100121108         endif;
097200121108
097300121108         // -?Ctrl date del periodo per conferma automatica prepagati?
097400121108         select;
097500121108           when  V1dcpd  = *zero;
097600121108             ErrGenerico = *on;
097700121108             ErrMessage  = *on;
097800121108             PosCurDCPD  = *on;
097900121108             VIDmsg = $Msg(04);
098000121108             leavesr;
098100121108           when  V1dcpd  < *zero;
098200121108             ErrGenerico = *on;
098300121108             ErrMessage  = *on;
098400121108             PosCurDCPD  = *on;
098500121108             VIDmsg = $Msg(05);
098600121108             leavesr;
098700121108           other;
098800121108             clear WLBdat;
098900121108             G08dat = V1dcpd;
099000121108             XSRDA8(WLBdat);
099100121108             if  G08err = *on;
099200121108               ErrGenerico = *on;
099300121108               ErrMessage  = *on;
099400121108               PosCurDCPD  = *on;
099500121108               VIDmsg = $Msg(05);
099600121108               leavesr;
099700121108             else;
099800121108               V1dcpd = G08dat;
099900121108             endif;
100000121108         endsl;
100100121108         dDGD.§DGDcpd  = G08inv;
100200121108         select;
100300121108           when  V1dcpa  = *zero;
100400121108             ErrGenerico = *on;
100500121108             ErrMessage  = *on;
100600121108             PosCurDCPA  = *on;
100700121108             VIDmsg = $Msg(04);
100800121108             leavesr;
100900121108           when  V1dcpa  < *zero;
101000121108             ErrGenerico = *on;
101100121108             ErrMessage  = *on;
101200121108             PosCurDCPA  = *on;
101300121108             VIDmsg = $Msg(05);
101400121108             leavesr;
101500121108           other;
101600121108             clear WLBdat;
101700121108             G08dat = V1dcpa;
101800121108             XSRDA8(WLBdat);
101900121108             if  G08err = *on;
102000121108               ErrGenerico = *on;
102100121108               ErrMessage  = *on;
102200121108               PosCurDCPA  = *on;
102300121108               VIDmsg = $Msg(05);
102400121108               leavesr;
102500121108             else;
102600121108               V1dcpa = G08dat;
102700121108             endif;
102800121108         endsl;
102900121108         dDGD.§DGDcpa  = G08inv;
103000121108         if  dDGD.§DGDcpd > dDGD.§DGDcpa;
103100121108           ErrGenerico = *on;
103200121108           ErrMessage  = *on;
103300121108           PosCurDCPA  = *on;
103400121108           VIDmsg = $Msg(06);
103500121108           leavesr;
103600121108         endif;
103700121107
103800121107       ENDSR;
103900121107
104000121107       //--------------------------------------------------------------
104100121107       //?Gestione trasmissione aggiornamento.                         ?
104200121107       //--------------------------------------------------------------
104300121107       BEGSR  sr_GesW01;
104400121107
104500121107         // -?Inizializzazione videata?
104600121107         if $InzW01 = *on;
104700121107           exsr  sr_InzW01;
104800121107           $InzW01 = *off;
104900121107         endif;
105000121107
105100121107         // -?Emissione window?
105200121107         if  TBXftt = 'S';
105300121107           exfmt  tbDGDw01;
105400121107           ErrMessage  = *off;
105500121107           ErrGenerico = *off;
105600121107           clear  VIDmsg;
105700121107         else;
105800121107           dsp_aid = c_F06;
105900121107         endif;
106000121107
106100121107         select;
106200121107
106300121107           // -?F12=Ritorno (gestito solo se emesso W01)?
106400121107           when  dsp_aid = c_F12;
106500121107             $Video = 'D2';
106600121107
106700121107           // -?F6=Conferma?
106800121107           when  dsp_aid = c_F06;
106900121107             exsr  sr_UpdTNTBE;
107000121107             // -?Ritorno alla videata iniziale?
107100121109             //  ?(disabilitata finchè esiste solo il cod. "1")?
107200121109             //if  NOT  ErrGenerico;
107300121109             //  clear  V1Topz;
107400121109             //  $Video  = 'D1';
107500121109             //  $InzD01 = *on;
107600121109             //endif;
107700121109             // -?Forzatura per scartare la 1ª videata (richiesta codice)?
107800121109             //  ?- FINCHÈ C'È SOLO IL CODICE "1 "... -                  ?
107900121109             if  NOT  ErrGenerico;
108000121109               $Fine = *on;
108100121109             endif;
108200121107         endsl;
108300121107
108400121107       ENDSR;
108500121107
108600121107       //--------------------------------------------------------------
108700121107       //?Inizializzazione videata W01.                                ?
108800121107       //--------------------------------------------------------------
108900121107       BEGSR  sr_InzW01;
109000121107
109100121107         clear  tbDGDw01;
109200121107
109300121107         if  $Immissione;
109400121107
109500121107           // -?Se immissione?
109600121107           W1ftt  = TBXftt;
109700121107
109800121107         else;
109900121107
110000121107           // -?Se NON immissione:?
110100121107           //  ?visualizza i dati relativi all'ultima trasmissione?
110200121107           W1ftt  = TBEftt;
110300121107           W1flt  = TBEflt;
110400121107           W1ftr  = TBEftr;
110500121107           if TBEdtr <> *zero;
110600121107             wDate_EUR = %date(TBEdtr : *iso);
110700121107             W1dtr     = %dec(wDate_EUR);
110800121107           endif;
110900121107
111000121107         endif;
111100121107
111200121107       ENDSR;
111300121107
111400121107       //--------------------------------------------------------------
111500121107       //?Aggiornamento records TNTBE00F (tab. "DGD").                 ?
111600121107       //--------------------------------------------------------------
111700121107       BEGSR  sr_UpdTNTBE;
111800121107
111900121107         // -?Aggancio record di TNTBE00F?
112000121107         clear  k05tntbe01;
112100121107         k_TBEcod  = c_Tab;
112200121107         k_TBEke1  = V1Cke1;
112300121107         //k_TBElin  = TBXlin;
112400121107         //k_TBEsif  = TBXsif;
112500121107
112600121107         chain  %kds(k05tntbe01)  TNTBE000;
112700121107
112800121107         exsr  sr_RieDGD;
112900121107         TBEuni = dDGD;
113000121107
113100121107         // -?Aggiornamento tab. "DGD"?
113200121107         SELECT;
113300121107
113400121107           // -?F5=Ripristino?
113500121107           WHEN  $Ripristino;
113600121107             clear  TBEatb;
113700121107             TBEftt = W1ftt;
113800121107             clear  TBEftr;
113900121107             //_______________
114000121107             UPDATE  TNTBE000;
114100121107             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
114200121107
114300121107           // -?F16=Annullamento?
114400121107           WHEN  $Annullamento;
114500121107             TBEatb = 'A';
114600121107             TBEftt = W1ftt;
114700121107             clear  TBEftr;
114800121107             //_______________
114900121107             UPDATE  TNTBE000;
115000121107             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
115100121107
115200121107           // -?Immissione o Modifica?
115300121107           OTHER;
115400121107             if  NOT %found(TNTBE01L);
115500121107               clear TNTBE000;
115600121107               TBEcod  = k_TBEcod;
115700121107               TBEke1  = k_TBEke1;
115800121107               TBEke2  = k_TBEke2;
115900121107               TBElin  = k_TBElin;
116000121107               TBEsif  = k_TBEsif;
116100121107               TBEuni  = dDGD;
116200121107             endif;
116300121107             TBEapl = TBXapl;
116400121107             clear  TBEatb;
116500121107             TBEftt = W1ftt;
116600121107             clear  TBEflt;
116700121107             clear  TBEftr;
116800121107             //clear TBEdtr;
116900121107             if  %found(TNTBE01L);
117000121107               //_____________
117100121107               UPDATE TNTBE000;
117200121107               //¯¯¯¯¯¯¯¯¯¯¯¯¯
117300121107             else;
117400121107               //_____________
117500121107               WRITE  TNTBE000;
117600121107               //¯¯¯¯¯¯¯¯¯¯¯¯¯
117700121107             endif;
117800121107
117900121107         ENDSL;
118000121107
118100121107       ENDSR;
118200121107
118300121107       //--------------------------------------------------------------
118400121107       //?Caricamento dati in tab. "DGD".                              ?
118500121107       //--------------------------------------------------------------
118600121107       BEGSR  sr_RieDGD;
118700121107
118800121107         clear  dDGD;
118900121107
119000121107         dDGD.§DGDgg   = V1dgg;
119100121107         if  V1dei > *zero;
119200121107           wDate_ISO = %date( ( V1dei * 10000 ) + *year : *eur );
119300121107           dDGD.§DGDei = ( %dec( wDate_ISO ) - ( *year * 10000 ) );
119400121107         endif;
119500121107         if  V1def > *zero;
119600121107           wDate_ISO = %date( ( V1def * 10000 ) + *year : *eur );
119700121107           dDGD.§DGDef = ( %dec( wDate_ISO ) - ( *year * 10000 ) );
119800121107         endif;
119900121107         dDGD.§DGDge   = V1dge;
120000121107         if  V1dii > *zero;
120100121107           wDate_ISO = %date( V1dii : *eur );
120200121107           dDGD.§DGDii = %dec( wDate_ISO );
120300121107         endif;
120400121107         if  V1dif > *zero;
120500121107           wDate_ISO = %date( V1dif : *eur );
120600121107           dDGD.§DGDif = %dec( wDate_ISO );
120700121107         endif;
120800121107         dDGD.§DGDgi   = V1dgi;
120900121107         dDGD.§DGDgBar = V1dgBar;
121000121107         dDGD.§DGDgPt  = V1dgPt;
121100121107         dDGD.§DGDdc   = V1ddc;
121200121107         if  V1dtaDa > *zero;
121300121107           wDate_ISO = %date( V1dtaDa : *eur );
121400121107           dDGD.§DGDtada = %dec( wDate_ISO );
121500121107         endif;
121600121107         if  V1dtaA  > *zero;
121700121107           wDate_ISO = %date( V1dtaA : *eur );
121800121107           dDGD.§DGDtaa  = %dec( wDate_ISO );
121900121107         endif;
122000121107         if  V1dbpd  > *zero;
122100121107           wDate_ISO = %date( V1dbpd : *eur );
122200121107           dDGD.§DGDbpd  = %dec( wDate_ISO );
122300121107         endif;
122400121107         if  V1dbpa  > *zero;
122500121107           wDate_ISO = %date( V1dbpa : *eur );
122600121107           dDGD.§DGDbpa  = %dec( wDate_ISO );
122700121107         endif;
122800121107         if  V1dcpd  > *zero;
122900121107           wDate_ISO = %date( V1dcpd : *eur );
123000121107           dDGD.§DGDcpd  = %dec( wDate_ISO );
123100121107         endif;
123200121107         if  V1dcpa  > *zero;
123300121107           wDate_ISO = %date( V1dcpa : *eur );
123400121107           dDGD.§DGDcpa  = %dec( wDate_ISO );
123500121107         endif;
123600121107
123700121107       ENDSR;
123800121107
123900121107       //--------------------------------------------------------------
124000121107       //?Operazioni finali.                                           ?
124100121107       //--------------------------------------------------------------
124200121107       BEGSR  sr_RoutEnd;
124300121107
124400121107         // -?Chiusura funzioni precedentemente aperte?
124500121107         clear  TIBS02ds;
124600121107         T02tla = 'C';
124700121107         TNTBE_RicercaControllo ( kpjba : tibs02ds );
124800121107
124900121107         // -?Uscita?
125000121107         return;
125100121107
125200121107       ENDSR;
125300121107
125400121107      /end-free
125500121107
125600121107       //--------------------------------------------------------------
125700121107       //?Definizione schiere a tempo di compilazione                  ?
125800121107       //--------------------------------------------------------------
125900121107
126000121107** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
126100121107Chiave di accesso obbligatoria                                                  1
126200121107Numero giorni obbligatorio                                                      2
126300121107Numero giorni errato                                                            3
126400121107Data obbligatoria                                                               4
126500121107Data formalmente errata                                                         5
126600121108Data iniziale successiva a quella finale                                        6
