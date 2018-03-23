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
012000121108     d   PosCurDDC                     n   overlay(IndDspF : 60)
012100121108     d   PosCurDTADA                   n   overlay(IndDspF : 61)
012200121108     d   PosCurDTAA                    n   overlay(IndDspF : 62)
012300121108     d   PosCurDBPD                    n   overlay(IndDspF : 63)
012400121108     d   PosCurDBPA                    n   overlay(IndDspF : 64)
012500121108     d   PosCurDCPD                    n   overlay(IndDspF : 65)
012600121108     d   PosCurDCPA                    n   overlay(IndDspF : 66)
012700121107         //   -?Riemissione videata?
012800121107     d   ErrGenerico                   n   overlay(IndDspF : 99)
012900121107
013000121107       // -?Parametri ricevuti?
013100121107     d KPJBA         e ds
013200121107
013300121107       // -?Dati record principale della tabella?
013400121107     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013500121107     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013600121107     d                                          prefix(TBX:3)
013700121107
013800121107       // -?Tabelle usate:?
013900121107       // ·?"DGD" = Periodi per Distinta Consegna e per Prepagato?
014000121107     d dDGD          e ds                  inz   qualified
014100121107
014200121107       //--------------------------------------------------------------
014300121107       //?Definizione variabili globali.                               ?
014400121107       //--------------------------------------------------------------
014500121107
014600121107       // -?Flags booleani?
014700121107     d $Fine           s               n   inz
014800121107     d $Err            s               n   inz
014900121107     d $InzD01         s               n   inz(*on)
015000121107     d $InzD02         s               n   inz(*on)
015100121107     d $InzW01         s               n   inz(*on)
015200121107     d $Immissione     s               n   inz
015300121107     d $Ripristino     s               n   inz
015400121107     d $Annullamento   s               n   inz
015500121107
015600121107       // -?Indici di schiera?
015700121107     d xx              s              3  0 inz
015800121107
015900121107       // -?Variabili per la gestione del video?
016000121107     d $Video          s              2    inz('D1')
016100121107
016200121107       // -?Campi di comodo?
016300121107     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
016400121107     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
016500121107     d*// wDate           s              8  0 inz
016600121107     d wDateAlfa       s              8    inz
016700121107
016800121107       //--------------------------------------------------------------
016900121107       //?Definizione prototipi procedure.                             ?
017000121107       //--------------------------------------------------------------
017100121107
017200121107       // -?Reperimento dati utente?
017300121107     d TIBS34ds      e ds                  inz
017400121107      /copy gaitrasrc/srcProtoPR,TIBS34R
017500121107
017600121107       // -?Ricerca e Controllo nuove tabelle (TNTBE)?
017700121107     d TIBS02ds      e ds                  inz
017800121107     d   T02mod      e                     inz('R')
017900121107     d   T02cod      e                     inz('DGD')
018000121107      /copy gaitrasrc/srcProtoPR,TIBS02R
018100121107
018200121107       // -?Ricerca filiale in organigramma?
018300121107      /copy gaitrasrc/srcProtoPI,TNSD24R
018400121107      /copy gaitrasrc/srcProtoPR,TNSD24R
018500121107
018600121107       // -?Controllo/Inversione data?
018700121107     d WLBdat          ds                  inz
018800121107     d  G08dat                        8  0 inz
018900121107     d  G08inv                        8  0 inz
019000121107     d  G08err                        1    inz('3')
019100121107     d  G08tgi                        5  0 inz
019200121107      /copy gaitrasrc/srcProtoPR,XSRDA8
019300121107
019400121107       //--------------------------------------------------------------
019500121107       //?Definizione key-list.                                        ?
019600121107       //--------------------------------------------------------------
019700121107
019800121107       // -?File TNTBE01L?
019900121107     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020000121107     d                                     prefix(k_)   inz
020100121107
020200121107       //--------------------------------------------------------------
020300121107       //?Riepilogo indicatori utilizzati.                             ?
020400121107       //--------------------------------------------------------------
020500121107       //--------------------------------------------------------------
020600121107
020700121107       //--------------------------------------------------------------
020800121107       //?M A I N - L I N E                                            ?
020900121107       //--------------------------------------------------------------
021000121107
021100121107     c     *Entry        plist
021200121107     c                   parm                    KPJBA
021300121107
021400121107      /free
021500121107
021600121107       // -?Operazioni iniziali?
021700121107       exsr  sr_RoutInz;
021800121107
021900121107       // -?Gestione videate?
022000121107       DOW  $Fine = *off;
022100121107
022200121107         select;
022300121107
022400121107           // -?Gestione videata D1 (Manutenzione dati singolo record)?
022500121107           when  $Video = 'D2';
022600121107             exsr  sr_GesD02;
022700121107
022800121107           // -?Gestione videata D1 (Richiesta codice filiale)?
022900121107           when  $Video = 'D1';
023000121107             exsr  sr_GesD01;
023100121107
023200121107           // -?Gestione videata W1 (Richiesta dati per trasmissione record)?
023300121107           when  $Video = 'W1';
023400121107             exsr  sr_GesW01;
023500121107
023600121107           // -?? ? ? ? ??
023700121107           other;
023800121107             $Fine = *on;
023900121107
024000121107         endsl;
024100121107
024200121107       ENDDO;
024300121107
024400121107       // -?Operazioni finali?
024500121107       exsr  sr_RoutEnd;
024600121107
024700121107       //--------------------------------------------------------------
024800121107       //?Operazioni iniziali.                                         ?
024900121107       //--------------------------------------------------------------
025000121107       BEGSR  sr_RoutInz;
025100121107
025200121107         *inLR = *on;
025300121107
025400121107         // -?Reperimento dati job?
025500121107         exsr  sr_DatiJob;
025600121107
025700121107         // -?Impostazione nome programma a video?
025800121107         V1Tpgm = SDSpgm;
025900121107
026000121107         // -?Reperimento data odierna?
026100121107         //wDate = %int( %subst( %char( %dec( %timestamp() ) )
026200121107         //                      : 1 : 8 ) );
026300121107
026400121107         // -?Aggancio dati generali della tabella in esame?
026500121107         clear  xTNTBEds;
026600121107         clear  k05tntbe01;
026700121107         k_TBEke1 = *zero;
026800121107         %subst( k_TBEke1 : %len(k_TBEke1) - %len(c_Tab) + 1 )
026900121107                  = c_Tab;
027000121107         k_TBEsif = KNSIF;
027100121107         chain(n)  %kds(k05tntbe01)  TNTBE000;
027200121107         if  not  %found(TNTBE01L);
027300121107           clear  k_TBEsif;
027400121107           chain(n)  %kds(k05tntbe01)  TNTBE000;
027500121107         endif;
027600121107         if  %found(TNTBE01L);
027700121107           xTNTBEds = TNTBEds;
027800121107         else;
027900121107           clear  xTNTBEds;
028000121107         endif;
028100121107
028200121107         // -?Impostazione iniziale / pulizia dei campi chiave?
028300121107         clear  k05tntbe01;
028400121107         k_TBEcod = c_Tab;
028500121109
028600121109         // -?Forzature per scartare la 1ª videata (richiesta codice)?
028700121109         //  ?- FINCHÈ C'È SOLO IL CODICE "1 "... -                  ?
028800121109         exsr  sr_InzD01;
028900121109         V1Cke1 = '1 ';
029000121109         exsr  sr_CtrD01;
029100121109         if  Not ErrGenerico;
029200121109           $Video = 'D2';
029300121109           reset  $InzD02;
029400121109         endif;
029500121107
029600121107       ENDSR;
029700121107
029800121107       //--------------------------------------------------------------
029900121107       //?Reperimento Dati del job (Utente/Operativi).                 ?
030000121107       //--------------------------------------------------------------
030100121107       BEGSR  sr_DatiJob;
030200121107
030300121107         in(E) §AzUte;
030400121107         if NOT %error;
030500121107           in(E) §DatiUte;
030600121107         endif;
030700121107         if %error or RSut = *blanks;
030800121107           clear TIBS34ds;
030900121107           tibs34r ( tibs34ds );
031000121107           in §AzUte;
031100121107           in §DatiUte;
031200121107         endif;
031300121107
031400121107       ENDSR;
031500121107
031600121107       //--------------------------------------------------------------
031700121107       //?Gestione videata D01.                                        ?
031800121107       //--------------------------------------------------------------
031900121107       BEGSR  sr_GesD01;
032000121107
032100121107         // -?Inizializzazione videata?
032200121107         if  $InzD01 = *on;
032300121107           exsr  sr_InzD01;
032400121107           $InzD01 = *off;
032500121107         endif;
032600121107         clear  V1Topz;
032700121107
032800121107         // -?(Dis)Abilitazione tasti funzionali?
032900121107         F3Attivo  = *on;
033000121107         F5Attivo  = *off;
033100121107         F6Attivo  = *off;
033200121107         F12Attivo = *off;
033300121107         F16Attivo = *off;
033400121107
033500121107         // -?Emissione videata?
033600121107         write  tbDGDt01;
033700121107         write  tbDGDp01;
033800121107         exfmt  tbDGDd01;
033900121107
034000121107         clear  VIDmsg;
034100121107         reset  ErrMessage;
034200121107         reset  ErrGenerico;
034300121107
034400121107         SELECT;
034500121107
034600121107           // -?F3=Fine?
034700121107           WHEN  dsp_aid = c_F03;
034800121107             $Fine = *on;
034900121107
035000121107           // -?Invio?
035100121107           OTHER;
035200121107             exsr  sr_CtrD01;
035300121107             if  ErrGenerico = *off;
035400121107               $Video    = 'D2';
035500121107               $InzD02   = *on;
035600121107             endif;
035700121107
035800121107         ENDSL;
035900121107
036000121107       ENDSR;
036100121107
036200121107       //--------------------------------------------------------------
036300121107       //?Inizializzazione videata D01.                                ?
036400121107       //--------------------------------------------------------------
036500121107       BEGSR  sr_InzD01;
036600121107
036700121107         clear  tbDGDd01;
036800121107
036900121107         V1Cke1 = '?';
037000121107
037100121107       ENDSR;
037200121107
037300121107       //--------------------------------------------------------------
037400121107       //?Controllo dati immessi nella videata D01.                    ?
037500121107       //--------------------------------------------------------------
037600121107       BEGSR  sr_CtrD01;
037700121107
037800121107         // -?Spegnimento indicatori di posizionamento cursore?
037900121107         %subst(IndDspF : 28) = *off;
038000121107
038100121107         SELECT;
038200121107
038300121107           // -?Ricerca dati già inseriti?
038400121107           WHEN  %scan('?' : V1Cke1) > *zero;
038500121107             clear  V1Cke1;
038600121107             reset  TIBS02ds;
038700121107             //T02mod = 'R';                //?(già così)?
038800121107             //T02cod = 'DGD';              //?(già così)?
038900121107             T02sif = knsif;
039000121107             TNTBE_RicercaControllo (kpjba : TIBS02ds);
039100121107             if  T02err = *blank;
039200121107               V1Cke1 = T02ke1;
039300121107               //dDGD   = T02uni;
039400121107             endif;
039500121107             ErrGenerico = *on;
039600121107             PosCurKE1   = *on;
039700121107             leavesr;
039800121107
039900121107           // -?Controllo immissione Codice?
040000121107           WHEN  V1Cke1 = *blank;
040100121107             ErrGenerico = *on;
040200121107             ErrMessage  = *on;
040300121107             PosCurKE1   = *on;
040400121107             VIDmsg = $Msg(01);
040500121107             leavesr;
040600121107
040700121107         ENDSL;
040800121107
040900121107       ENDSR;
041000121107
041100121107       //--------------------------------------------------------------
041200121107       //?Gestione videata D02                                         ?
041300121107       //--------------------------------------------------------------
041400121107       BEGSR  sr_GesD02;
041500121107
041600121107         // -?Inizializzazione videata?
041700121107         if  $InzD02 = *on;
041800121107           exsr sr_InzD02;
041900121107           $InzD02 = *off;
042000121107         endif;
042100121107
042200121107         // -?Emissione Testata e Piede con tasti funzionali abilitati?
042300121107         if  errGenerico = *off;
042400121107           write  tbDGDt01;
042500121107           write  tbDGDd01;
042600121107           write  Protect;
042700121107           write  tbDGDp01;
042800121107         endif;
042900121107
043000121107         // -?Emissione videata?
043100121107         exfmt  tbDGDd02;
043200121107
043300121107         clear  VIDmsg;
043400121107         reset  ErrMessage;
043500121107         reset  ErrGenerico;
043600121107
043700121107         SELECT;
043800121107
043900121107           // -?F3=Fine?
044000121107           WHEN  dsp_aid = c_F03;
044100121107             unlock TNTBE01L;
044200121107             $Fine = *on;
044300121107
044400121107           // -?F12=Ritorno?
044500121107           WHEN  dsp_aid = c_F12;
044600121107             unlock TNTBE01L;
044700121107             reset  $Video;
044800121107             clear  V1Topz;
044900121107
045000121107           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
045100121107           OTHER;
045200121107             $Ripristino   = (dsp_aid = c_F05);
045300121107             $Annullamento = (dsp_aid = c_F16);
045400121107             // -?Controlli eseguiti NON se F16=Annullamento?
045500121107             if  Not $Annullamento;
045600121107               exsr  sr_CtrD02;
045700121107             endif;
045800121107             select;
045900121107               // -?Rilevato errore?
046000121107               when  ErrGenerico;
046100121107                 leavesr;
046200121107               // -?Richiesta trasmissione record?
046300121107               when  dsp_aid = c_F05  or
046400121107                     dsp_aid = c_F06  or
046500121107                     dsp_aid = c_F16;
046600121107                 $Video  = 'W1';
046700121107                 reset  $InzW01;
046800121107             endsl;
046900121107
047000121107         ENDSL;
047100121107
047200121107       ENDSR;
047300121107
047400121107       //--------------------------------------------------------------
047500121107       //?Inizializzazione videata D02.                                ?
047600121107       //--------------------------------------------------------------
047700121107       BEGSR  sr_InzD02;
047800121107
047900121107         // -?Spegnimento degli indicatori di errore?
048000121107         %subst(IndDspF : 50) = *off;
048100121107
048200121107         // -?Pulizia videata?
048300121107         clear  tbDGDd02;
048400121107
048500121107         // -?Reperimento dati della filiale di emissione?
048600121107         reset  $Immissione;
048700121107         reset  $Ripristino;
048800121107         reset  $Annullamento;
048900121107         k_TBEke1 = V1Cke1;
049000121107         chain  %kds(k05tntbe01)  TNTBE000;
049100121107         select;
049200121107           when  Not %found(TNTBE01L);
049300121107             $Immissione = *on;
049400121107           when  TBEatb <> *blank;
049500121107             $Ripristino = *on;
049600121107         endsl;
049700121107
049800121107         // -?Caricamento dati della tabella a video?
049900121107         if  %found(TNTBE01L);
050000121107           exsr  sr_RieD02;
050100121107         endif;
050200121107
050300121107         // -?Impostazione tipo manutenzione?
050400121107         select;
050500121107           when  $Immissione;
050600121107             V1Topz = 'IMMISSIONE';
050700121107           when  $Ripristino;
050800121107             V1Topz = 'RIPRISTINO';
050900121107           other;
051000121107             V1Topz = ' MODIFICA ';
051100121107         endsl;
051200121107
051300121107         // -?Impostazione indicatori per abilitazione tasti funzionali?
051400121107         exsr  sr_AbilitFxxD02;
051500121107
051600121107       ENDSR;
051700121107
051800121107       //--------------------------------------------------------------
051900121107       //?Caricamento dati del singolo record della tab. "DGD"         ?
052000121107       //?nella videata D02.                                           ?
052100121107       //--------------------------------------------------------------
052200121107       BEGSR  sr_RieD02;
052300121107
052400121107         dDGD = TBEuni;
052500121107
052600121107         // -?Campi hidden?
052700121113         //V1dgpt  = dDGD.§DGDgpt;
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
055000121107         V1ddc   = dDGD.§DGDdc;
055100121107         if  dDGD.§DGDtaDa > *zero;
055200121107           wDate_EUR = %date( dDGD.§DGDtaDa : *iso );
055300121107           V1dtaDa = %dec(wDate_EUR);
055400121107         endif;
055500121107         if  dDGD.§DGDtaA > *zero;
055600121107           wDate_EUR = %date( dDGD.§DGDtaA : *iso );
055700121107           V1dtaA  = %dec(wDate_EUR);
055800121107         endif;
055900121107         if  dDGD.§DGDbpd > *zero;
056000121107           wDate_EUR = %date( dDGD.§DGDbpd : *iso );
056100121107           V1dbpd  = %dec(wDate_EUR);
056200121107         endif;
056300121107         if  dDGD.§DGDbpa > *zero;
056400121107           wDate_EUR = %date( dDGD.§DGDbpa : *iso );
056500121107           V1dbpa  = %dec(wDate_EUR);
056600121107         endif;
056700121107         if  dDGD.§DGDcpd > *zero;
056800121107           wDate_EUR = %date( dDGD.§DGDcpd : *iso );
056900121107           V1dcpd  = %dec(wDate_EUR);
057000121107         endif;
057100121107         if  dDGD.§DGDcpa > *zero;
057200121107           wDate_EUR = %date( dDGD.§DGDcpa : *iso );
057300121107           V1dcpa  = %dec(wDate_EUR);
057400121107         endif;
057500121107
057600121107       ENDSR;
057700121107
057800121107       //--------------------------------------------------------------
057900121107       //?Settaggio indicatori nella videata D02 per abilitazione      ?
058000121107       //?  tasti funzionali.                                          ?
058100121107       //--------------------------------------------------------------
058200121107       BEGSR  sr_AbilitFxxD02;
058300121107
058400121107         // -?F3=Fine?
058500121107         F3Attivo  = *on;
058600121107
058700121107         // -?F5=Ripristino?
058800121107         F5Attivo  = (NOT $Immissione  and  $Ripristino);
058900121107
059000121107         // -?F6=Conferma?
059100121107         F6Attivo  = (NOT F5Attivo);
059200121107
059300121107         // -?F12=Ritorno?
059400121109         //  ?(disabilitato finchè esiste solo il cod. "1")?
059500121109         //F12Attivo = *on;
059600121109         F12Attivo = *off;
059700121107
059800121107         // -?F16=Annullamento?
059900121107         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
060000121107
060100121107       ENDSR;
060200121107
060300121107       //--------------------------------------------------------------
060400121107       //?Controllo dati videata D02.                                  ?
060500121107       //--------------------------------------------------------------
060600121107       BEGSR  sr_CtrD02;
060700121108
060800121108         clear  dDGD;
060900121107
061000121107         // -?Spegnimento degli indicatori di errore?
061100121107         %subst(IndDspF : 50) = *off;
061200121107
061300121107         // -?Ctrl giorni limite di manutenzionabilità Distinta Consegna?
061400121107         select;
061500121107           when  V1dgg   = *zero;
061600121107             ErrGenerico = *on;
061700121107             ErrMessage  = *on;
061800121107             PosCurDGG   = *on;
061900121107             VIDmsg = $Msg(02);
062000121107             leavesr;
062100121107           when  V1dgg   < *zero;
062200121107             ErrGenerico = *on;
062300121107             ErrMessage  = *on;
062400121107             PosCurDGG   = *on;
062500121107             VIDmsg = $Msg(03);
062600121107             leavesr;
062700121107         endsl;
062800121107
062900121107         // -?Ctrl date del periodo di trazioni ridotte estive?
063000121107         select;
063100121107           when  V1dei   = *zero;
063200121107             ErrGenerico = *on;
063300121107             ErrMessage  = *on;
063400121107             PosCurDEI   = *on;
063500121107             VIDmsg = $Msg(04);
063600121107             leavesr;
063700121107           when  V1dei   < *zero;
063800121107             ErrGenerico = *on;
063900121107             ErrMessage  = *on;
064000121107             PosCurDEI   = *on;
064100121107             VIDmsg = $Msg(05);
064200121107             leavesr;
064300121108           other;
064400121107             clear WLBdat;
064500121107             G08dat = ( V1dei * 10000 ) + *year;
064600121107             XSRDA8(WLBdat);
064700121107             if  G08err = *on;
064800121107               ErrGenerico = *on;
064900121107               ErrMessage  = *on;
065000121107               PosCurDEI   = *on;
065100121107               VIDmsg = $Msg(05);
065200121107               leavesr;
065300121107             //else;
065400121107             //  V1dei  = ( G08dat - *year ) / 10000;
065500121107             endif;
065600121107         endsl;
065700121108         dDGD.§DGDei = G08inv - (*year * 10000);
065800121107         select;
065900121107           when  V1def   = *zero;
066000121107             ErrGenerico = *on;
066100121107             ErrMessage  = *on;
066200121107             PosCurDEF   = *on;
066300121107             VIDmsg = $Msg(04);
066400121107             leavesr;
066500121107           when  V1def   < *zero;
066600121107             ErrGenerico = *on;
066700121107             ErrMessage  = *on;
066800121107             PosCurDEF   = *on;
066900121107             VIDmsg = $Msg(05);
067000121107             leavesr;
067100121108           other;
067200121107             clear WLBdat;
067300121107             G08dat = ( V1def * 10000 ) + *year;
067400121107             XSRDA8(WLBdat);
067500121107             if  G08err = *on;
067600121107               ErrGenerico = *on;
067700121107               ErrMessage  = *on;
067800121107               PosCurDEF   = *on;
067900121107               VIDmsg = $Msg(05);
068000121107               leavesr;
068100121107             //else;
068200121107             //  V1def  = ( G08dat - *year ) / 10000;
068300121107             endif;
068400121107         endsl;
068500121108         dDGD.§DGDef = G08inv - (*year * 10000);
068600121108         if  dDGD.§DGDei > dDGD.§DGDef;
068700121108           ErrGenerico = *on;
068800121108           ErrMessage  = *on;
068900121108           PosCurDEF   = *on;
069000121108           VIDmsg = $Msg(06);
069100121108           leavesr;
069200121108         endif;
069300121108
069400121108         // -?Ctrl giorni di controllo nel periodo estivo?
069500121107         select;
069600121107           when  V1dge   = *zero;
069700121107             ErrGenerico = *on;
069800121107             ErrMessage  = *on;
069900121107             PosCurDGE   = *on;
070000121107             VIDmsg = $Msg(02);
070100121107             leavesr;
070200121107           when  V1dge   < *zero;
070300121107             ErrGenerico = *on;
070400121107             ErrMessage  = *on;
070500121107             PosCurDGE   = *on;
070600121107             VIDmsg = $Msg(03);
070700121107             leavesr;
070800121107         endsl;
070900121107
071000121107         // -?Ctrl date del periodo di trazioni ridotte invernali?
071100121107         select;
071200121107           when  V1dii   = *zero;
071300121107             ErrGenerico = *on;
071400121107             ErrMessage  = *on;
071500121107             PosCurDII   = *on;
071600121107             VIDmsg = $Msg(04);
071700121107             leavesr;
071800121107           when  V1dii   < *zero;
071900121107             ErrGenerico = *on;
072000121107             ErrMessage  = *on;
072100121107             PosCurDII   = *on;
072200121107             VIDmsg = $Msg(05);
072300121107             leavesr;
072400121108           other;
072500121107             clear WLBdat;
072600121107             G08dat = V1dii;
072700121107             XSRDA8(WLBdat);
072800121107             if  G08err = *on;
072900121107               ErrGenerico = *on;
073000121107               ErrMessage  = *on;
073100121107               PosCurDII   = *on;
073200121107               VIDmsg = $Msg(05);
073300121107               leavesr;
073400121107             else;
073500121107               V1dii  = G08dat;
073600121107             endif;
073700121107         endsl;
073800121108         dDGD.§DGDii = G08inv;
073900121107         select;
074000121107           when  V1dif   = *zero;
074100121107             ErrGenerico = *on;
074200121107             ErrMessage  = *on;
074300121107             PosCurDIF   = *on;
074400121107             VIDmsg = $Msg(04);
074500121107             leavesr;
074600121107           when  V1dif   < *zero;
074700121107             ErrGenerico = *on;
074800121107             ErrMessage  = *on;
074900121107             PosCurDIF   = *on;
075000121107             VIDmsg = $Msg(05);
075100121107             leavesr;
075200121108           other;
075300121107             clear WLBdat;
075400121107             G08dat = V1dif;
075500121107             XSRDA8(WLBdat);
075600121107             if  G08err = *on;
075700121107               ErrGenerico = *on;
075800121107               ErrMessage  = *on;
075900121107               PosCurDIF   = *on;
076000121107               VIDmsg = $Msg(05);
076100121107               leavesr;
076200121107             else;
076300121107               V1dif  = G08dat;
076400121107             endif;
076500121107         endsl;
076600121108         dDGD.§DGDif = G08inv;
076700121108         if  dDGD.§DGDii > dDGD.§DGDif;
076800121108           ErrGenerico = *on;
076900121108           ErrMessage  = *on;
077000121108           PosCurDIF   = *on;
077100121108           VIDmsg = $Msg(06);
077200121108           leavesr;
077300121108         endif;
077400121108
077500121108         // -?Ctrl giorni di controllo nel periodo invernale?
077600121107         select;
077700121107           when  V1dgi   = *zero;
077800121107             ErrGenerico = *on;
077900121107             ErrMessage  = *on;
078000121107             PosCurDGI   = *on;
078100121107             VIDmsg = $Msg(02);
078200121107             leavesr;
078300121107           when  V1dgi   < *zero;
078400121107             ErrGenerico = *on;
078500121107             ErrMessage  = *on;
078600121107             PosCurDGI   = *on;
078700121107             VIDmsg = $Msg(03);
078800121107             leavesr;
078900121107         endsl;
079000121107
079100121107         // -?Ctrl giorni limite per retrodatazione della data consegna?
079200121107         //  ?(BRT)?
079300121107         select;
079400121107           when  V1dgBar = *zero;
079500121107             ErrGenerico = *on;
079600121107             ErrMessage  = *on;
079700121107             PosCurDGBAR = *on;
079800121107             VIDmsg = $Msg(02);
079900121107             leavesr;
080000121107           when  V1dgBar < *zero;
080100121107             ErrGenerico = *on;
080200121107             ErrMessage  = *on;
080300121107             PosCurDGBAR = *on;
080400121107             VIDmsg = $Msg(03);
080500121107             leavesr;
080600121107         endsl;
080700121107
080800121107         // -?Ctrl giorni limite per la data di Fogli Vari e Distinte?
080900121107         select;
081000121107           when  V1ddc   = *zero;
081100121107             ErrGenerico = *on;
081200121107             ErrMessage  = *on;
081300121107             PosCurDDC   = *on;
081400121107             VIDmsg = $Msg(02);
081500121107             leavesr;
081600121107           when  V1ddc   < *zero;
081700121107             ErrGenerico = *on;
081800121107             ErrMessage  = *on;
081900121107             PosCurDDC   = *on;
082000121107             VIDmsg = $Msg(03);
082100121107             leavesr;
082200121107         endsl;
082300121107
082400121107         // -?Ctrl date del periodo nel quale NON controllare le?
082500121107         //  ?partite aperte durante le quadrature giornaliere?
082600121107         select;
082700121107           when  V1dtaDa = *zero;
082800121107             ErrGenerico = *on;
082900121107             ErrMessage  = *on;
083000121107             PosCurDTADA = *on;
083100121107             VIDmsg = $Msg(04);
083200121107             leavesr;
083300121107           when  V1dtaDa < *zero;
083400121107             ErrGenerico = *on;
083500121107             ErrMessage  = *on;
083600121107             PosCurDTADA = *on;
083700121107             VIDmsg = $Msg(05);
083800121107             leavesr;
083900121108           other;
084000121107             clear WLBdat;
084100121107             G08dat = V1dtaDa;
084200121107             XSRDA8(WLBdat);
084300121107             if  G08err = *on;
084400121107               ErrGenerico = *on;
084500121107               ErrMessage  = *on;
084600121107               PosCurDTADA = *on;
084700121107               VIDmsg = $Msg(05);
084800121107               leavesr;
084900121107             else;
085000121107               V1dtaDa = G08dat;
085100121107             endif;
085200121107         endsl;
085300121108         dDGD.§DGDtaDa = G08inv;
085400121108         select;
085500121108           when  V1dtaA  = *zero;
085600121108             ErrGenerico = *on;
085700121108             ErrMessage  = *on;
085800121108             PosCurDTAA  = *on;
085900121108             VIDmsg = $Msg(04);
086000121108             leavesr;
086100121108           when  V1dtaA  < *zero;
086200121108             ErrGenerico = *on;
086300121108             ErrMessage  = *on;
086400121108             PosCurDTAA  = *on;
086500121108             VIDmsg = $Msg(05);
086600121108             leavesr;
086700121108           other;
086800121108             clear WLBdat;
086900121108             G08dat = V1dtaA;
087000121108             XSRDA8(WLBdat);
087100121108             if  G08err = *on;
087200121108               ErrGenerico = *on;
087300121108               ErrMessage  = *on;
087400121108               PosCurDTAA  = *on;
087500121108               VIDmsg = $Msg(05);
087600121108               leavesr;
087700121108             else;
087800121108               V1dtaA  = G08dat;
087900121108             endif;
088000121108         endsl;
088100121108         dDGD.§DGDtaA  = G08inv;
088200121108         if  dDGD.§DGDtaDa > dDGD.§DGDtaA;
088300121108           ErrGenerico = *on;
088400121108           ErrMessage  = *on;
088500121108           PosCurDTAA  = *on;
088600121108           VIDmsg = $Msg(06);
088700121108           leavesr;
088800121108         endif;
088900121107
089000121107         // -?Ctrl date del periodo per blocco immissione prepagati?
089100121107         select;
089200121107           when  V1dbpd  = *zero;
089300121107             ErrGenerico = *on;
089400121107             ErrMessage  = *on;
089500121107             PosCurDBPD  = *on;
089600121107             VIDmsg = $Msg(04);
089700121107             leavesr;
089800121107           when  V1dbpd  < *zero;
089900121107             ErrGenerico = *on;
090000121107             ErrMessage  = *on;
090100121107             PosCurDBPD  = *on;
090200121107             VIDmsg = $Msg(05);
090300121107             leavesr;
090400121108           other;
090500121107             clear WLBdat;
090600121107             G08dat = V1dbpd;
090700121107             XSRDA8(WLBdat);
090800121107             if  G08err = *on;
090900121107               ErrGenerico = *on;
091000121107               ErrMessage  = *on;
091100121107               PosCurDBPD  = *on;
091200121107               VIDmsg = $Msg(05);
091300121107               leavesr;
091400121107             else;
091500121107               V1dbpd = G08dat;
091600121107             endif;
091700121107         endsl;
091800121108         dDGD.§DGDbpd  = G08inv;
091900121107         select;
092000121107           when  V1dbpa  = *zero;
092100121107             ErrGenerico = *on;
092200121107             ErrMessage  = *on;
092300121107             PosCurDBPA  = *on;
092400121107             VIDmsg = $Msg(04);
092500121107             leavesr;
092600121107           when  V1dbpa  < *zero;
092700121107             ErrGenerico = *on;
092800121107             ErrMessage  = *on;
092900121107             PosCurDBPA  = *on;
093000121107             VIDmsg = $Msg(05);
093100121107             leavesr;
093200121108           other;
093300121107             clear WLBdat;
093400121107             G08dat = V1dbpa;
093500121107             XSRDA8(WLBdat);
093600121107             if  G08err = *on;
093700121107               ErrGenerico = *on;
093800121107               ErrMessage  = *on;
093900121107               PosCurDBPA  = *on;
094000121107               VIDmsg = $Msg(05);
094100121107               leavesr;
094200121107             else;
094300121107               V1dbpa = G08dat;
094400121107             endif;
094500121107         endsl;
094600121108         dDGD.§DGDbpa  = G08inv;
094700121108         if  dDGD.§DGDbpd > dDGD.§DGDbpa;
094800121108           ErrGenerico = *on;
094900121108           ErrMessage  = *on;
095000121108           PosCurDBPA  = *on;
095100121108           VIDmsg = $Msg(06);
095200121108           leavesr;
095300121108         endif;
095400121108
095500121108         // -?Ctrl date del periodo per conferma automatica prepagati?
095600121108         select;
095700121108           when  V1dcpd  = *zero;
095800121108             ErrGenerico = *on;
095900121108             ErrMessage  = *on;
096000121108             PosCurDCPD  = *on;
096100121108             VIDmsg = $Msg(04);
096200121108             leavesr;
096300121108           when  V1dcpd  < *zero;
096400121108             ErrGenerico = *on;
096500121108             ErrMessage  = *on;
096600121108             PosCurDCPD  = *on;
096700121108             VIDmsg = $Msg(05);
096800121108             leavesr;
096900121108           other;
097000121108             clear WLBdat;
097100121108             G08dat = V1dcpd;
097200121108             XSRDA8(WLBdat);
097300121108             if  G08err = *on;
097400121108               ErrGenerico = *on;
097500121108               ErrMessage  = *on;
097600121108               PosCurDCPD  = *on;
097700121108               VIDmsg = $Msg(05);
097800121108               leavesr;
097900121108             else;
098000121108               V1dcpd = G08dat;
098100121108             endif;
098200121108         endsl;
098300121108         dDGD.§DGDcpd  = G08inv;
098400121108         select;
098500121108           when  V1dcpa  = *zero;
098600121108             ErrGenerico = *on;
098700121108             ErrMessage  = *on;
098800121108             PosCurDCPA  = *on;
098900121108             VIDmsg = $Msg(04);
099000121108             leavesr;
099100121108           when  V1dcpa  < *zero;
099200121108             ErrGenerico = *on;
099300121108             ErrMessage  = *on;
099400121108             PosCurDCPA  = *on;
099500121108             VIDmsg = $Msg(05);
099600121108             leavesr;
099700121108           other;
099800121108             clear WLBdat;
099900121108             G08dat = V1dcpa;
100000121108             XSRDA8(WLBdat);
100100121108             if  G08err = *on;
100200121108               ErrGenerico = *on;
100300121108               ErrMessage  = *on;
100400121108               PosCurDCPA  = *on;
100500121108               VIDmsg = $Msg(05);
100600121108               leavesr;
100700121108             else;
100800121108               V1dcpa = G08dat;
100900121108             endif;
101000121108         endsl;
101100121108         dDGD.§DGDcpa  = G08inv;
101200121108         if  dDGD.§DGDcpd > dDGD.§DGDcpa;
101300121108           ErrGenerico = *on;
101400121108           ErrMessage  = *on;
101500121108           PosCurDCPA  = *on;
101600121108           VIDmsg = $Msg(06);
101700121108           leavesr;
101800121108         endif;
101900121107
102000121107       ENDSR;
102100121107
102200121107       //--------------------------------------------------------------
102300121107       //?Gestione trasmissione aggiornamento.                         ?
102400121107       //--------------------------------------------------------------
102500121107       BEGSR  sr_GesW01;
102600121107
102700121107         // -?Inizializzazione videata?
102800121107         if $InzW01 = *on;
102900121107           exsr  sr_InzW01;
103000121107           $InzW01 = *off;
103100121107         endif;
103200121107
103300121107         // -?Emissione window?
103400121107         if  TBXftt = 'S';
103500121107           exfmt  tbDGDw01;
103600121107           ErrMessage  = *off;
103700121107           ErrGenerico = *off;
103800121107           clear  VIDmsg;
103900121107         else;
104000121107           dsp_aid = c_F06;
104100121107         endif;
104200121107
104300121107         select;
104400121107
104500121107           // -?F12=Ritorno (gestito solo se emesso W01)?
104600121107           when  dsp_aid = c_F12;
104700121107             $Video = 'D2';
104800121107
104900121107           // -?F6=Conferma?
105000121107           when  dsp_aid = c_F06;
105100121107             exsr  sr_UpdTNTBE;
105200121107             // -?Ritorno alla videata iniziale?
105300121109             //  ?(disabilitata finchè esiste solo il cod. "1")?
105400121109             //if  NOT  ErrGenerico;
105500121109             //  clear  V1Topz;
105600121109             //  $Video  = 'D1';
105700121109             //  $InzD01 = *on;
105800121109             //endif;
105900121109             // -?Forzatura per scartare la 1ª videata (richiesta codice)?
106000121109             //  ?- FINCHÈ C'È SOLO IL CODICE "1 "... -                  ?
106100121109             if  NOT  ErrGenerico;
106200121109               $Fine = *on;
106300121109             endif;
106400121107         endsl;
106500121107
106600121107       ENDSR;
106700121107
106800121107       //--------------------------------------------------------------
106900121107       //?Inizializzazione videata W01.                                ?
107000121107       //--------------------------------------------------------------
107100121107       BEGSR  sr_InzW01;
107200121107
107300121107         clear  tbDGDw01;
107400121107
107500121107         if  $Immissione;
107600121107
107700121107           // -?Se immissione?
107800121107           W1ftt  = TBXftt;
107900121107
108000121107         else;
108100121107
108200121107           // -?Se NON immissione:?
108300121107           //  ?visualizza i dati relativi all'ultima trasmissione?
108400121107           W1ftt  = TBEftt;
108500121107           W1flt  = TBEflt;
108600121107           W1ftr  = TBEftr;
108700121107           if TBEdtr <> *zero;
108800121107             wDate_EUR = %date(TBEdtr : *iso);
108900121107             W1dtr     = %dec(wDate_EUR);
109000121107           endif;
109100121107
109200121107         endif;
109300121107
109400121107       ENDSR;
109500121107
109600121107       //--------------------------------------------------------------
109700121107       //?Aggiornamento records TNTBE00F (tab. "DGD").                 ?
109800121107       //--------------------------------------------------------------
109900121107       BEGSR  sr_UpdTNTBE;
110000121107
110100121107         // -?Aggancio record di TNTBE00F?
110200121107         clear  k05tntbe01;
110300121107         k_TBEcod  = c_Tab;
110400121107         k_TBEke1  = V1Cke1;
110500121107         //k_TBElin  = TBXlin;
110600121107         //k_TBEsif  = TBXsif;
110700121107
110800121107         chain  %kds(k05tntbe01)  TNTBE000;
110900121107
111000121107         exsr  sr_RieDGD;
111100121107         TBEuni = dDGD;
111200121107
111300121107         // -?Aggiornamento tab. "DGD"?
111400121107         SELECT;
111500121107
111600121107           // -?F5=Ripristino?
111700121107           WHEN  $Ripristino;
111800121107             clear  TBEatb;
111900121107             TBEftt = W1ftt;
112000121107             clear  TBEftr;
112100121107             //_______________
112200121107             UPDATE  TNTBE000;
112300121107             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
112400121107
112500121107           // -?F16=Annullamento?
112600121107           WHEN  $Annullamento;
112700121107             TBEatb = 'A';
112800121107             TBEftt = W1ftt;
112900121107             clear  TBEftr;
113000121107             //_______________
113100121107             UPDATE  TNTBE000;
113200121107             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
113300121107
113400121107           // -?Immissione o Modifica?
113500121107           OTHER;
113600121107             if  NOT %found(TNTBE01L);
113700121107               clear TNTBE000;
113800121107               TBEcod  = k_TBEcod;
113900121107               TBEke1  = k_TBEke1;
114000121107               TBEke2  = k_TBEke2;
114100121107               TBElin  = k_TBElin;
114200121107               TBEsif  = k_TBEsif;
114300121107               TBEuni  = dDGD;
114400121107             endif;
114500121107             TBEapl = TBXapl;
114600121107             clear  TBEatb;
114700121107             TBEftt = W1ftt;
114800121107             clear  TBEflt;
114900121107             clear  TBEftr;
115000121107             //clear TBEdtr;
115100121107             if  %found(TNTBE01L);
115200121107               //_____________
115300121107               UPDATE TNTBE000;
115400121107               //¯¯¯¯¯¯¯¯¯¯¯¯¯
115500121107             else;
115600121107               //_____________
115700121107               WRITE  TNTBE000;
115800121107               //¯¯¯¯¯¯¯¯¯¯¯¯¯
115900121107             endif;
116000121107
116100121107         ENDSL;
116200121107
116300121107       ENDSR;
116400121107
116500121107       //--------------------------------------------------------------
116600121107       //?Caricamento dati in tab. "DGD".                              ?
116700121107       //--------------------------------------------------------------
116800121107       BEGSR  sr_RieDGD;
116900121107
117000121107         clear  dDGD;
117100121107
117200121107         dDGD.§DGDgg   = V1dgg;
117300121107         if  V1dei > *zero;
117400121107           wDate_ISO = %date( ( V1dei * 10000 ) + *year : *eur );
117500121107           dDGD.§DGDei = ( %dec( wDate_ISO ) - ( *year * 10000 ) );
117600121107         endif;
117700121107         if  V1def > *zero;
117800121107           wDate_ISO = %date( ( V1def * 10000 ) + *year : *eur );
117900121107           dDGD.§DGDef = ( %dec( wDate_ISO ) - ( *year * 10000 ) );
118000121107         endif;
118100121107         dDGD.§DGDge   = V1dge;
118200121107         if  V1dii > *zero;
118300121107           wDate_ISO = %date( V1dii : *eur );
118400121107           dDGD.§DGDii = %dec( wDate_ISO );
118500121107         endif;
118600121107         if  V1dif > *zero;
118700121107           wDate_ISO = %date( V1dif : *eur );
118800121107           dDGD.§DGDif = %dec( wDate_ISO );
118900121107         endif;
119000121107         dDGD.§DGDgi   = V1dgi;
119100121107         dDGD.§DGDgBar = V1dgBar;
119200121107         dDGD.§DGDdc   = V1ddc;
119300121107         if  V1dtaDa > *zero;
119400121107           wDate_ISO = %date( V1dtaDa : *eur );
119500121107           dDGD.§DGDtada = %dec( wDate_ISO );
119600121107         endif;
119700121107         if  V1dtaA  > *zero;
119800121107           wDate_ISO = %date( V1dtaA : *eur );
119900121107           dDGD.§DGDtaa  = %dec( wDate_ISO );
120000121107         endif;
120100121107         if  V1dbpd  > *zero;
120200121107           wDate_ISO = %date( V1dbpd : *eur );
120300121107           dDGD.§DGDbpd  = %dec( wDate_ISO );
120400121107         endif;
120500121107         if  V1dbpa  > *zero;
120600121107           wDate_ISO = %date( V1dbpa : *eur );
120700121107           dDGD.§DGDbpa  = %dec( wDate_ISO );
120800121107         endif;
120900121107         if  V1dcpd  > *zero;
121000121107           wDate_ISO = %date( V1dcpd : *eur );
121100121107           dDGD.§DGDcpd  = %dec( wDate_ISO );
121200121107         endif;
121300121107         if  V1dcpa  > *zero;
121400121107           wDate_ISO = %date( V1dcpa : *eur );
121500121107           dDGD.§DGDcpa  = %dec( wDate_ISO );
121600121107         endif;
121700121107
121800121107       ENDSR;
121900121107
122000121107       //--------------------------------------------------------------
122100121107       //?Operazioni finali.                                           ?
122200121107       //--------------------------------------------------------------
122300121107       BEGSR  sr_RoutEnd;
122400121107
122500121107         // -?Chiusura funzioni precedentemente aperte?
122600121107         clear  TIBS02ds;
122700121107         T02tla = 'C';
122800121107         TNTBE_RicercaControllo ( kpjba : tibs02ds );
122900121107
123000121107         // -?Uscita?
123100121107         return;
123200121107
123300121107       ENDSR;
123400121107
123500121107      /end-free
123600121107
123700121107       //--------------------------------------------------------------
123800121107       //?Definizione schiere a tempo di compilazione                  ?
123900121107       //--------------------------------------------------------------
124000121107
124100121107** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
124200121107Chiave di accesso obbligatoria                                                  1
124300121107Numero giorni obbligatorio                                                      2
124400121107Numero giorni errato                                                            3
124500121107Data obbligatoria                                                               4
124600121107Data formalmente errata                                                         5
124700121108Data iniziale successiva a quella finale                                        6
