000100120919       //==============================================================
000200120919       //?Gestione tabella "FFC":                                      ?
000300120919       //?Forzatura Filiale di Consegna per Destinatari Esteri         ?
000400120919       //==============================================================
000500120919
000600120919       //--------------------------------------------------------------
000700120919       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800120919       //--------------------------------------------------------------
000900120919
001000120919     /*PRM  dbgview(*source)
001100120919     /*END
001200120919
001300120919       //--------------------------------------------------------------
001400120919       //?Specifiche di controllo.                                     ?
001500120919       //--------------------------------------------------------------
001600120919
001700120919     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800120919     h dftactgrp(*no)
001900120919
002000120919       //--------------------------------------------------------------
002100120919       //?Dichiarazione file.                                          ?
002200120919       //--------------------------------------------------------------
002300120919
002400120919       // -?Organigramma filiale/aziendale?
002500120919     fAZORG01L  if   e           k disk
002600120919
002700120919       // -?Tabella "FFC"?
002800120919     fTNTBE01L  Uf A e           k disk
002900120919
003000120919       // -?Video Gestione?
003100120919     fTNTBFFCD  cf   e             workstn
003200120919     f                                     indds(IndDspF)
003300120919     f                                     infds(InfDspF)
003400120919
003500120919       //--------------------------------------------------------------
003600120919       //?Definizione costanti.                                        ?
003700120919       //--------------------------------------------------------------
003800120919
003900120919       // -?Codice tabella in gestione?
004000120919     d c_Tab           c                   const('FFC')
004100120919
004200120919       // -?Tasti funzionali a video?
004300120919     d c_F01           c                   const(x'31')
004400120919     d c_F02           c                   const(x'32')
004500120919     d c_F03           c                   const(x'33')
004600120919     d c_F04           c                   const(x'34')
004700120919     d c_F05           c                   const(x'35')
004800120919     d c_F06           c                   const(x'36')
004900120919     d c_F07           c                   const(x'37')
005000120919     d c_F08           c                   const(x'38')
005100120919     d c_F09           c                   const(x'39')
005200120919     d c_F10           c                   const(x'3A')
005300120919     d c_F11           c                   const(x'3B')
005400120919     d c_F12           c                   const(x'3C')
005500120919     d c_F13           c                   const(x'B1')
005600120919     d c_F14           c                   const(x'B2')
005700120919     d c_F15           c                   const(x'B3')
005800120919     d c_F16           c                   const(x'B4')
005900120919     d c_F17           c                   const(x'B5')
006000120919     d c_F18           c                   const(x'B6')
006100120919     d c_F19           c                   const(x'B7')
006200120919     d c_F20           c                   const(x'B8')
006300120919     d c_F21           c                   const(x'B9')
006400120919     d c_F22           c                   const(x'BA')
006500120919     d c_F23           c                   const(x'BB')
006600120919     d c_F24           c                   const(x'BC')
006700120919     d c_Enter         c                   const(x'F1')
006800120919     d c_RollDown      c                   const(x'F4')
006900120919     d c_RollUp        c                   const(x'F5')
007000120919
007100120919       // -?Costante per controllo "caratteri solo numerici"?
007200120919     d c_Digits        c                   const('0123456789')
007300120919
007400120919       //--------------------------------------------------------------
007500120919       //?Definizione schiere.                                         ?
007600120919       //--------------------------------------------------------------
007700120919
007800120919       // -?Messaggi di errore?
007900120919     d $Msg            s             78    dim( 7)  ctdata  perrcd( 1)
008000120919
008100120919       //--------------------------------------------------------------
008200120919       //?Definizione aree dati.                                       ?
008300120919       //--------------------------------------------------------------
008400120919
008500120919       // -?Dati utente?
008600120919     d §AzUte        e ds                  extname(AZUTE00F)
008700120919     d                                     dtaara
008800120919     d §DatiUte      e ds                  extname(dDatiUte)
008900120919     d                                     dtaara
009000120919
009100120919       //--------------------------------------------------------------
009200120919       //?Definizione strutture dati.                                  ?
009300120919       //--------------------------------------------------------------
009400120919
009500120919       // -?Status ds?
009600120919     d Status         sds
009700120919     d  SDSpgm           *proc
009800120919
009900120919       // -?InfDS?
010000120919     d InfDspF         ds
010100120919     d   dsp_aid             369    369a
010200120919
010300120919       // -?Indicatori su DspF?
010400120919     d IndDspF         ds                  inz
010500120919         // -?Abilitazione tasti funzionali?
010600120919     d   F3Attivo                      n   overlay(IndDspF : 03)
010700120919     d   F5Attivo                      n   overlay(IndDspF : 05)
010800120919     d   F6Attivo                      n   overlay(IndDspF : 06)
010900120919     d   F12Attivo                     n   overlay(IndDspF : 12)
011000120919     d   F16Attivo                     n   overlay(IndDspF : 16)
011100120919         // -?Emissione messaggio di errore?
011200120919     d   ErrMessage                    n   overlay(IndDspF : 28)
011300120919         // -?Posizionamento cursore & segnalazione errore?
011400120919     d   PosCurPOE                     n   overlay(IndDspF : 51)
011500120919     d   PosCurPOC                     n   overlay(IndDspF : 52)
011600120919     d   PosCurDTI                     n   overlay(IndDspF : 53)
011700120919     d   PosCurDTF                     n   overlay(IndDspF : 54)
011800120919         //   -?Riemissione videata?
011900120919     d   ErrGenerico                   n   overlay(IndDspF : 99)
012000120919
012100120919       // -?Parametri ricevuti?
012200120919     d KPJBA         e ds
012300120919
012400120919       // -?Dati record principale della tabella?
012500120919     d TNTBEds       e ds                  inz  extname(TNTBE00F)
012600120919     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
012700120919     d                                          prefix(TBX:3)
012800120919
012900120919       // -?Tabelle usate:?
013000120919       // ·?"FFC" = Forzatura Filiale di Consegna x Destinatari Esteri?
013100120919     d dFFC          e ds                  inz   qualified
013200120919
013300120919       //--------------------------------------------------------------
013400120919       //?Definizione variabili globali.                               ?
013500120919       //--------------------------------------------------------------
013600120919
013700120919       // -?Flags booleani?
013800120919     d $Fine           s               n   inz
013900120919     d $Err            s               n   inz
014000120919     d $InzD01         s               n   inz(*on)
014100120919     d $InzD02         s               n   inz(*on)
014200120919     d $InzW01         s               n   inz(*on)
014300120919     d $Called         s               n   inz
014400120919     d $Immissione     s               n   inz
014500120919     d $Ripristino     s               n   inz
014600120919     d $Annullamento   s               n   inz
014700120919
014800120919       // -?Indici di schiera?
014900120919     d xx              s              3  0 inz
015000120919
015100120919       // -?Variabili per la gestione del video?
015200120919     d $Video          s              2    inz('D1')
015300120919
015400120919       // -?Campi di comodo?
015500120919     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
015600120919     d wDate_ISO       s               d   datfmt(*iso)   inz(*loval)
015700120919     d*// wDate           s              8  0 inz
015800120919     d wDateAlfa       s              8    inz
015900120919
016000120919       //--------------------------------------------------------------
016100120919       //?Definizione prototipi procedure.                             ?
016200120919       //--------------------------------------------------------------
016300120919
016400120919       // -?Reperimento dati utente?
016500120919     d TIBS34ds      e ds                  inz
016600120919      /copy gaitrasrc/srcProtoPR,TIBS34R
016700120919
016800120919       // -?Ricerca e Controllo nuove tabelle (TNTBE)?
016900120919     d TIBS02ds      e ds                  inz
017000120919     d   T02mod      e                     inz('R')
017100120919     d   T02cod      e                     inz('FFC')
017200120919      /copy gaitrasrc/srcProtoPR,TIBS02R
017300120919
017400120919       // -?Ricerca filiale in organigramma?
017500120919      /copy gaitrasrc/srcProtoPI,TNSD24R
017600120919      /copy gaitrasrc/srcProtoPR,TNSD24R
017700120920
017800120920       // -?Controllo/Inversione data?
017900120920     d WLBdat          ds                  inz
018000120920     d  G08dat                        8  0 inz
018100120920     d  G08inv                        8  0 inz
018200120920     d  G08err                        1    inz('3')
018300120920     d  G08tgi                        5  0 inz
018400120920      /copy gaitrasrc/srcProtoPR,XSRDA8
018500120919
018600120919       //--------------------------------------------------------------
018700120919       //?Definizione key-list.                                        ?
018800120919       //--------------------------------------------------------------
018900120919
019000120919       // -?File TNTBE01L?
019100120919     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
019200120919     d                                     prefix(k_)   inz
019300120919
019400120919       //--------------------------------------------------------------
019500120919       //?Riepilogo indicatori utilizzati.                             ?
019600120919       //--------------------------------------------------------------
019700120919       //--------------------------------------------------------------
019800120919
019900120919       //--------------------------------------------------------------
020000120919       //?M A I N - L I N E                                            ?
020100120919       //--------------------------------------------------------------
020200120919
020300120919     c     *Entry        plist
020400120919     c                   parm                    KPJBA
020500120919
020600120919      /free
020700120919
020800120919       // -?Operazioni iniziali?
020900120919       exsr  sr_RoutInz;
021000120919
021100120919       // -?Gestione videate?
021200120919       DOW  $Fine = *off;
021300120919
021400120919         select;
021500120919
021600120919           // -?Gestione videata D1 (Manutenzione dati singolo record)?
021700120919           when  $Video = 'D2';
021800120919             exsr  sr_GesD02;
021900120919
022000120919           // -?Gestione videata D1 (Richiesta codice filiale)?
022100120919           when  $Video = 'D1';
022200120919             exsr  sr_GesD01;
022300120919
022400120919           // -?Gestione videata W1 (Richiesta dati per trasmissione record)?
022500120919           when  $Video = 'W1';
022600120919             exsr  sr_GesW01;
022700120919
022800120919           // -?? ? ? ? ??
022900120919           other;
023000120919             $Fine = *on;
023100120919
023200120919         endsl;
023300120919
023400120919       ENDDO;
023500120919
023600120919       // -?Operazioni finali?
023700120919       exsr  sr_RoutEnd;
023800120919
023900120919       //--------------------------------------------------------------
024000120919       //?Operazioni iniziali.                                         ?
024100120919       //--------------------------------------------------------------
024200120919       BEGSR  sr_RoutInz;
024300120919
024400120919         *inLR = *on;
024500120919
024600120919         // -?Reperimento dati job?
024700120919         exsr  sr_DatiJob;
024800120919
024900120919         // -?Impostazione nome programma a video?
025000120919         V1Tpgm = SDSpgm;
025100120919
025200120919         // -?Reperimento data odierna?
025300120919         //wDate = %int( %subst( %char( %dec( %timestamp() ) )
025400120919         //                      : 1 : 8 ) );
025500120919
025600120919         // -?Aggancio dati generali della tabella in esame?
025700120919         clear  xTNTBEds;
025800120919         clear  k05tntbe01;
025900120919         k_TBEke1 = *zero;
026000120919         %subst( k_TBEke1 : %len(k_TBEke1) - %len(c_Tab) + 1 )
026100120919                  = c_Tab;
026200120919         k_TBEsif = KNSIF;
026300120919         chain(n)  %kds(k05tntbe01)  TNTBE000;
026400120919         if  not  %found(TNTBE01L);
026500120919           clear  k_TBEsif;
026600120919           chain(n)  %kds(k05tntbe01)  TNTBE000;
026700120919         endif;
026800120919         if  %found(TNTBE01L);
026900120919           xTNTBEds = TNTBEds;
027000120919         else;
027100120919           clear  xTNTBEds;
027200120919         endif;
027300120919
027400120919         // -?Impostazione iniziale / pulizia dei campi chiave?
027500120919         clear  k05tntbe01;
027600120919         k_TBEcod = c_Tab;
027700120919
027800120919         // -?Verifica se *pgm richiamato:?
027900120919         $Called = (%subst( KPJBU : 1 : %len(V1Cpoe) ) <> *blank);
028000120919         if  $Called;
028100120919           exsr  sr_InzD01;
028200120919           exsr  sr_CtrD01;
028300120919           if  Not ErrGenerico;
028400120919             $Video  = 'D2';
028500120919             $InzD02 = *on;
028600120919           endif;
028700120919         endif;
028800120919
028900120919       ENDSR;
029000120919
029100120919       //--------------------------------------------------------------
029200120919       //?Reperimento Dati del job (Utente/Operativi).                 ?
029300120919       //--------------------------------------------------------------
029400120919       BEGSR  sr_DatiJob;
029500120919
029600120919         in(E) §AzUte;
029700120919         if NOT %error;
029800120919           in(E) §DatiUte;
029900120919         endif;
030000120919         if %error or RSut = *blanks;
030100120919           clear TIBS34ds;
030200120919           tibs34r ( tibs34ds );
030300120919           in §AzUte;
030400120919           in §DatiUte;
030500120919         endif;
030600120919
030700120919       ENDSR;
030800120919
030900120919       //--------------------------------------------------------------
031000120919       //?Gestione videata D01.                                        ?
031100120919       //--------------------------------------------------------------
031200120919       BEGSR  sr_GesD01;
031300120919
031400120919         // -?Inizializzazione videata?
031500120919         if  $InzD01 = *on;
031600120919           exsr  sr_InzD01;
031700120919           $InzD01 = *off;
031800120919         endif;
031900120919         clear  V1Topz;
032000120919
032100120919         // -?(Dis)Abilitazione tasti funzionali?
032200120919         F5Attivo  = *off;
032300120919         F6Attivo  = *off;
032400120919         F12Attivo = *off;
032500120919         F16Attivo = *off;
032600120919
032700120919         // -?Emissione videata?
032800120919         write  tbFFCt1;
032900120919         write  tbFFCp1;
033000120919         exfmt  tbFFCd1;
033100120919
033200120919         clear  VIDmsg;
033300120919         reset  ErrMessage;
033400120919         reset  ErrGenerico;
033500120919
033600120919         SELECT;
033700120919
033800120919           // -?F3=Fine?
033900120919           WHEN  dsp_aid = c_F03;
034000120919             $Fine = *on;
034100120919
034200120919           // -?Invio?
034300120919           OTHER;
034400120919             exsr  sr_CtrD01;
034500120919             if  ErrGenerico = *off;
034600120919               $Video    = 'D2';
034700120919               $InzD02   = *on;
034800120919             endif;
034900120919
035000120919         ENDSL;
035100120919
035200120919       ENDSR;
035300120919
035400120919       //--------------------------------------------------------------
035500120919       //?Inizializzazione videata D01.                                ?
035600120919       //--------------------------------------------------------------
035700120919       BEGSR  sr_InzD01;
035800120919
035900120919         clear  tbFFCd1;
036000120919
036100120919         if  $Called  and
036200120919             %check( c_Digits : %subst( KPJBU : 1 : %len(V1Cpoe) ) )
036300120919                      =  *zero;
036400120919           V1Cpoe = %subst( KPJBU : 1 : %len(V1Cpoe) );
036500120919         else;
036600120919           V1Cpoe = '?  ';
036700120919         endif;
036800120919
036900120919       ENDSR;
037000120919
037100120919       //--------------------------------------------------------------
037200120919       //?Controllo dati immessi nella videata D01.                    ?
037300120919       //--------------------------------------------------------------
037400120919       BEGSR  sr_CtrD01;
037500120919
037600120919         // -?Spegnimento indicatori di posizionamento cursore?
037700120919         %subst(IndDspF : 28) = *off;
037800120919
037900120919         clear  V1Dpoe;
038000120919
038100120919         SELECT;
038200120919
038300120919           // -?Ricerca dati già inseriti?
038400120919           WHEN  %scan('?' : V1Cpoe) > *zero;
038500120919             clear  V1Cpoe;
038600120919             reset  TIBS02ds;
038700120919             //T02mod = 'R';                //?(già così)?
038800120919             //T02cod = 'FFC';              //?(già così)?
038900120919             T02sif = knsif;
039000120919             TNTBE_RicercaControllo (kpjba : TIBS02ds);
039100120919             if  T02err = *blank;
039200120919               V1Cpoe = T02ke1;
039300120919               dFFC   = T02uni;
039400120919               V1Dpoe = %subst( T02uni : 1 : %len(V1Dpoe) );
039500120919               chain  (%int(V1Cpoe))  AZORG;
039600120919               if  Not %found(AZORG01L)  or  ORGfva <> *blank  or
039700120919                   (ORGfag <> 'A'       and  ORGfag <> 'F');
039800120919                 V1Dpoe = *all'? ';
039900120919               else;
040000120919                 V1Dpoe = ORGdes;
040100120919               endif;
040200120919             endif;
040300120919             ErrGenerico = *on;
040400120919             PosCurPOE   = *on;
040500120919             leavesr;
040600120919
040700120919           // -?Controllo immissione codice Filiale di Emissione?
040800120919           WHEN  V1Cpoe = *blank  or  V1Cpoe = *zero;
040900120919             ErrGenerico = *on;
041000120919             ErrMessage  = *on;
041100120919             PosCurPOE   = *on;
041200120919             VIDmsg = $Msg(01);
041300120919             leavesr;
041400120919
041500120919           // -?Controllo immisione solo caratteri numerici?
041600120919           WHEN  %check( c_Digits : V1Cpoe ) > *zero;
041700120919             ErrGenerico = *on;
041800120919             ErrMessage  = *on;
041900120919             PosCurPOE   = *on;
042000120919             VIDmsg = $Msg(02);
042100120919             leavesr;
042200120919
042300120919           // -?Controllo validità codice Filiale di Emissione?
042400120919           OTHER;
042500120919             chain  (%int(V1Cpoe))  AZORG;
042600120919             if  Not %found(AZORG01L)  or  ORGfva <> *blank  or
042700120919                 (ORGfag <> 'A'       and  ORGfag <> 'F');
042800120919               ErrGenerico = *on;
042900120919               ErrMessage  = *on;
043000120919               PosCurPOE   = *on;
043100120919               VIDmsg = $Msg(02);
043200120919               leavesr;
043300120919             endif;
043400120919             V1Dpoe = ORGdes;
043500120919
043600120919         ENDSL;
043700120919
043800120919       ENDSR;
043900120919
044000120919       //--------------------------------------------------------------
044100120919       //?Gestione videata D02                                         ?
044200120919       //--------------------------------------------------------------
044300120919       BEGSR  sr_GesD02;
044400120919
044500120919         // -?Inizializzazione videata?
044600120919         if  $InzD02 = *on;
044700120919           exsr sr_InzD02;
044800120919           $InzD02 = *off;
044900120919         endif;
045000120919
045100120919         // -?Emissione Testata e Piede con tasti funzionali abilitati?
045200120919         if  errGenerico = *off;
045300120919           write  tbFFCt1;
045400120919           write  tbFFCd1;
045500120919           write  Protect;
045600120919           write  tbFFCp2;
045700120919         endif;
045800120919
045900120919         // -?Emissione videata?
046000120919         exfmt  tbFFCd2;
046100120919
046200120919         clear  VIDmsg;
046300120919         reset  ErrMessage;
046400120919         reset  ErrGenerico;
046500120919
046600120919         SELECT;
046700120919
046800120919           // -?F3=Fine?
046900120919           WHEN  dsp_aid = c_F03;
047000120919             unlock TNTBE01L;
047100120919             $Fine = *on;
047200120919
047300120919           // -?F12=Ritorno?
047400120919           WHEN  dsp_aid = c_F12;
047500120919             unlock TNTBE01L;
047600120919             reset  $Video;
047700120919             clear  V1Topz;
047800120919             if  $Called;
047900120919               $Fine = *on;
048000120919             endif;
048100120919
048200120919           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
048300120919           OTHER;
048400120919             $Ripristino   = (dsp_aid = c_F05);
048500120919             $Annullamento = (dsp_aid = c_F16);
048600120919             // -?Controlli eseguiti NON se F16=Annullamento?
048700120919             if  Not $Annullamento;
048800120919               exsr  sr_CtrD02;
048900120919             endif;
049000120919             select;
049100120919               // -?Rilevato errore?
049200120919               when  ErrGenerico;
049300120919                 leavesr;
049400120919               // -?Richiesta trasmissione record?
049500120919               when  dsp_aid = c_F05  or
049600120919                     dsp_aid = c_F06  or
049700120919                     dsp_aid = c_F16;
049800120919                 $Video  = 'W1';
049900120919                 reset  $InzW01;
050000120919             endsl;
050100120919
050200120919         ENDSL;
050300120919
050400120919       ENDSR;
050500120919
050600120919       //--------------------------------------------------------------
050700120919       //?Inizializzazione videata D02.                                ?
050800120919       //--------------------------------------------------------------
050900120919       BEGSR  sr_InzD02;
051000120919
051100120919         // -?Spegnimento degli indicatori di errore?
051200120919         %subst(IndDspF : 50) = *off;
051300120919
051400120919         // -?Pulizia videata?
051500120919         clear  tbFFCd2;
051600120919
051700120919         // -?Reperimento dati della filiale di emissione?
051800120919         reset  $Immissione;
051900120919         reset  $Ripristino;
052000120919         reset  $Annullamento;
052100120919         k_TBEke1 = V1Cpoe;
052200120919         chain  %kds(k05tntbe01)  TNTBE000;
052300120919         select;
052400120919           when  Not %found(TNTBE01L);
052500120919             $Immissione = *on;
052600120919           when  TBEatb <> *blank;
052700120919             $Ripristino = *on;
052800120919         endsl;
052900120919
053000120919         // -?Caricamento dati della Filiale di Emissione?
053100120919         if  %found(TNTBE01L);
053200120919           exsr  sr_RieD02;
053300120919         endif;
053400120919
053500120919         // -?Impostazione tipo manutenzione?
053600120919         select;
053700120919           when  $Immissione;
053800120919             V1Topz = 'IMMISSIONE';
053900120919           when  $Ripristino;
054000120919             V1Topz = 'RIPRISTINO';
054100120919           other;
054200120919             V1Topz = ' MODIFICA ';
054300120919         endsl;
054400120919
054500120919         // -?Impostazione indicatori per abilitazione tasti funzionali?
054600120919         exsr  sr_AbilitFxxD02;
054700120919
054800120919       ENDSR;
054900120919
055000120919       //--------------------------------------------------------------
055100120919       //?Caricamento dati del singolo record della tab. "FFC"         ?
055200120919       //?nella videata D02.                                           ?
055300120919       //--------------------------------------------------------------
055400120919       BEGSR  sr_RieD02;
055500120920
055600120920         V2Cpoe = %int( V1Cpoe );
055700120920         V2Dpoe = V1Dpoe;
055800120919
055900120919         dFFC = TBEuni;
056000120919
056100120919         V2Cpoc   = %editc( dFFC.§FFCpoc : 'X' );
056200120919         if  dFFC.§FFCdti > *zero;
056300120919           wDate_EUR = %date( %int(dFFC.§FFCdti) : *iso );
056400120919           V2Cdti = %dec(wDate_EUR);
056500120919         endif;
056600120919         if  dFFC.§FFCdtf > *zero;
056700120919           wDate_EUR = %date( %int(dFFC.§FFCdtf) : *iso );
056800120919           V2Cdtf = %dec(wDate_EUR);
056900120919         endif;
057000120919
057100120919         // -?Decodifiche?
057200120919         exsr  sr_CtrD02;
057300120919         clear  VIDmsg;
057400120919         clear  ErrMessage;
057500120919         clear  ErrGenerico;
057600120919         %subst(IndDspF : 50) = *off;
057700120919
057800120919       ENDSR;
057900120919
058000120919       //--------------------------------------------------------------
058100120919       //?Settaggio indicatori nella videata D02 per abilitazione      ?
058200120919       //?  tasti funzionali.                                          ?
058300120919       //--------------------------------------------------------------
058400120919       BEGSR  sr_AbilitFxxD02;
058500120919
058600120919         // -?F3=Fine (se NON richiamato da altro *pgm)?
058700120919         F3Attivo  = (Not $Called);
058800120919
058900120919         // -?F5=Ripristino?
059000120919         F5Attivo  = (NOT $Immissione  and  $Ripristino);
059100120919
059200120919         // -?F6=Conferma?
059300120919         F6Attivo  = (NOT F5Attivo);
059400120919
059500120919         // -?F12=Ritorno?
059600120919         F12Attivo = *on;
059700120919
059800120919         // -?F16=Annullamento?
059900120919         F16Attivo = (NOT $Immissione  and  NOT $Ripristino);
060000120919
060100120919       ENDSR;
060200120919
060300120919       //--------------------------------------------------------------
060400120919       //?Controllo dati videata D02.                                  ?
060500120919       //--------------------------------------------------------------
060600120919       BEGSR  sr_CtrD02;
060700120919
060800120919         // -?Spegnimento degli indicatori di errore?
060900120919         %subst(IndDspF : 50) = *off;
061000120919
061100120919         // -?Filiale di Consegna?
061200120919         clear  V2Dpoc;
061300120919         Select;
061400120919
061500120919           // ·?Obbligatoria?
061600120919           When  V2Cpoc = *blank  or  V2Cpoc = *zero;
061700120919             ErrGenerico = *on;
061800120919             ErrMessage  = *on;
061900120919             PosCurPOC   = *on;
062000120919             VIDmsg = $Msg(03);
062100120919             leavesr;
062200120919
062300120919           // ·?Richiesta ricerca?
062400120919           When  %scan( '?' : V2Cpoc ) > *zero;
062500120919             tnsd24r ( p_FIL : p_FAG : p_DES : p_DIT );
062600120919             V2Cpoc = p_FIL;
062700120919             V2Dpoc = p_DES;
062800120919             ErrGenerico = *on;
062900120919             PosCurPOC   = *on;
063000120919             leavesr;
063100120919
063200120919           // ·?Controllo immisione solo caratteri numerici?
063300120919           When  %check( c_Digits : V2Cpoc ) > *zero;
063400120919             ErrGenerico = *on;
063500120919             ErrMessage  = *on;
063600120919             PosCurPOC   = *on;
063700120919             VIDmsg = $Msg(04);
063800120919             leavesr;
063900120919
064000120919           // ·?Controllo?
064100120919           Other;
064200120919             chain (%int( V2Cpoc ))  AZORG;
064300120919             if  %found(AZORG01L)  and  ORGfva = *blank;
064400120919               V2Dpoc = ORGdes;
064500120919             endif;
064600120919             if  Not %found(AZORG01L)  or  ORGfva <> *blank  or
064700120919                 (ORGfag <> 'A'       and  ORGfag <> 'F');
064800120919               ErrGenerico = *on;
064900120919               ErrMessage  = *on;
065000120919               PosCurPOC   = *on;
065100120919               VIDmsg = $Msg(04);
065200120919               leavesr;
065300120919             endif;
065400120919
065500120919         EndSl;
065600120919
065700120919         // -?Controllo Data Inizio Forzatura?
065800120919         Select;
065900120919           when  V2Cdti = *zero;
066000120919             ErrGenerico = *on;
066100120919             ErrMessage  = *on;
066200120919             PosCurDTI   = *on;
066300120919             VIDmsg = $Msg(05);
066400120919             leavesr;
066500120919           when  V2Cdti < *zero;
066600120919             ErrGenerico = *on;
066700120919             ErrMessage  = *on;
066800120919             PosCurDTI   = *on;
066900120919             VIDmsg = $Msg(06);
067000120919             leavesr;
067100120919           when  V2Cdti > *zero;
067200120920             //wDateAlfa = %editc( V2Cdti : 'X' );
067300120920             //test(de)  *eur0  wDateAlfa;
067400120920             //if  %error;
067500120920             //  ErrGenerico = *on;
067600120920             //  ErrMessage  = *on;
067700120920             //  PosCurDTI   = *on;
067800120920             //  VIDmsg = $Msg(06);
067900120920             //  leavesr;
068000120920             //endif;
068100120920             clear WLBdat;
068200120920             G08dat = V2Cdti;
068300120920             XSRDA8(WLBdat);
068400120920             if  G08err = *on;
068500120920               ErrGenerico = *on;
068600120920               ErrMessage  = *on;
068700120920               PosCurDTI   = *on;
068800120920               VIDmsg = $Msg(06);
068900120920               leavesr;
069000120920             else;
069100120920               V2Cdti = G08dat;
069200120920             endif;
069300120919         endsl;
069400120919
069500120919         // -?Controllo Data Fine Forzatura?
069600120919         Select;
069700120919           when  V2Cdtf = *zero;
069800120919             ErrGenerico = *on;
069900120919             ErrMessage  = *on;
070000120919             PosCurDTF   = *on;
070100120919             VIDmsg = $Msg(05);
070200120919             leavesr;
070300120919           when  V2Cdtf < *zero;
070400120919             ErrGenerico = *on;
070500120919             ErrMessage  = *on;
070600120919             PosCurDTF   = *on;
070700120919             VIDmsg = $Msg(06);
070800120919             leavesr;
070900120919           when  V2Cdtf > *zero;
071000120920             //wDateAlfa = %editc( V2Cdtf : 'X' );
071100120920             //test(de)  *eur0  wDateAlfa;
071200120920             //if  %error;
071300120920             //  ErrGenerico = *on;
071400120920             //  ErrMessage  = *on;
071500120920             //  PosCurDTF   = *on;
071600120920             //  VIDmsg = $Msg(06);
071700120920             //  leavesr;
071800120920             //endif;
071900120920             clear WLBdat;
072000120920             G08dat = V2Cdtf;
072100120920             XSRDA8(WLBdat);
072200120920             if  G08err = *on;
072300120920               ErrGenerico = *on;
072400120920               ErrMessage  = *on;
072500120920               PosCurDTF   = *on;
072600120920               VIDmsg = $Msg(06);
072700120920               leavesr;
072800120920             else;
072900120920               V2Cdtf = G08dat;
073000120920             endif;
073100120919         endsl;
073200120919
073300120919         // -?Controllo sequenza date Inizio/Fine forzatura?
073400120919         if  %date( V2Cdti : *eur ) >
073500120919             %date( V2Cdtf : *eur );
073600120919           ErrGenerico = *on;
073700120919           ErrMessage  = *on;
073800120919           PosCurDTI   = *on;
073900120919           VIDmsg = $Msg(07);
074000120919           leavesr;
074100120919         endif;
074200120919
074300120919       ENDSR;
074400120919
074500120919       //--------------------------------------------------------------
074600120919       //?Gestione trasmissione aggiornamento.                         ?
074700120919       //--------------------------------------------------------------
074800120919       BEGSR  sr_GesW01;
074900120919
075000120919         // -?Inizializzazione videata?
075100120919         if $InzW01 = *on;
075200120919           exsr  sr_InzW01;
075300120919           $InzW01 = *off;
075400120919         endif;
075500120919
075600120919         // -?Emissione window?
075700120919         if  TBXftt = 'S';
075800120919           exfmt  tbFFCw1;
075900120919           ErrMessage  = *off;
076000120919           ErrGenerico = *off;
076100120919           clear  VIDmsg;
076200120919         else;
076300120919           dsp_aid = c_F06;
076400120919         endif;
076500120919
076600120919         select;
076700120919
076800120919           // -?F12=Ritorno (gestito solo se emesso W01)?
076900120919           when  dsp_aid = c_F12;
077000120919             $Video = 'D2';
077100120919
077200120919           // -?F6=Conferma?
077300120919           when  dsp_aid = c_F06;
077400120919             exsr  sr_UpdTNTBE;
077500120919             // -?Ritorno alla videata iniziale?
077600120919             if  NOT  ErrGenerico;
077700120919               clear  V1Topz;
077800120919               $Video  = 'D1';
077900120919               $InzD01 = *on;
078000120919             endif;
078100120919         endsl;
078200120919
078300120919       ENDSR;
078400120919
078500120919       //--------------------------------------------------------------
078600120919       //?Inizializzazione videata W01.                                ?
078700120919       //--------------------------------------------------------------
078800120919       BEGSR  sr_InzW01;
078900120919
079000120919         clear  tbFFCw1;
079100120919
079200120919         if  $Immissione;
079300120919
079400120919           // -?Se immissione?
079500120919           W1ftt  = TBXftt;
079600120919
079700120919         else;
079800120919
079900120919           // -?Se NON immissione:?
080000120919           //  ?visualizza i dati relativi all'ultima trasmissione?
080100120919           W1ftt  = TBEftt;
080200120919           W1flt  = TBEflt;
080300120919           W1ftr  = TBEftr;
080400120919           if TBEdtr <> *zero;
080500120919             wDate_EUR = %date(TBEdtr : *iso);
080600120919             W1dtr     = %dec(wDate_EUR);
080700120919           endif;
080800120919
080900120919         endif;
081000120919
081100120919       ENDSR;
081200120919
081300120919       //--------------------------------------------------------------
081400120919       //?Aggiornamento records TNTBE00F (tab. "FFC").                 ?
081500120919       //--------------------------------------------------------------
081600120919       BEGSR  sr_UpdTNTBE;
081700120919
081800120919         // -?Aggancio record di TNTBE00F?
081900120919         clear  k05tntbe01;
082000120919         k_TBEcod  = c_Tab;
082100120919         k_TBEke1  = V1Cpoe;
082200120919         //k_TBElin  = TBXlin;
082300120919         //k_TBEsif  = TBXsif;
082400120919
082500120919         chain  %kds(k05tntbe01)  TNTBE000;
082600120919
082700120919         exsr  sr_RieFFC;
082800120919         TBEuni = dFFC;
082900120919
083000120919         // -?Aggiornamento tab. "FFC"?
083100120919         SELECT;
083200120919
083300120919           // -?F5=Ripristino?
083400120919           WHEN  $Ripristino;
083500120919             clear  TBEatb;
083600120919             TBEftt = W1ftt;
083700120919             clear  TBEftr;
083800120919             //_______________
083900120919             UPDATE  TNTBE000;
084000120919             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
084100120919
084200120919           // -?F16=Annullamento?
084300120919           WHEN  $Annullamento;
084400120919             TBEatb = 'A';
084500120919             TBEftt = W1ftt;
084600120919             clear  TBEftr;
084700120919             //_______________
084800120919             UPDATE  TNTBE000;
084900120919             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
085000120919
085100120919           // -?Immissione o Modifica?
085200120919           OTHER;
085300120919             if  NOT %found(TNTBE01L);
085400120919               clear TNTBE000;
085500120919               TBEcod  = k_TBEcod;
085600120919               TBEke1  = k_TBEke1;
085700120919               TBEke2  = k_TBEke2;
085800120919               TBElin  = k_TBElin;
085900120919               TBEsif  = k_TBEsif;
086000120919               TBEuni  = dFFC;
086100120919             endif;
086200120919             TBEapl = TBXapl;
086300120919             clear  TBEatb;
086400120919             TBEftt = W1ftt;
086500120919             clear  TBEflt;
086600120919             clear  TBEftr;
086700120919             //clear TBEdtr;
086800120919             if  %found(TNTBE01L);
086900120919               //_____________
087000120919               UPDATE TNTBE000;
087100120919               //¯¯¯¯¯¯¯¯¯¯¯¯¯
087200120919             else;
087300120919               //_____________
087400120919               WRITE  TNTBE000;
087500120919               //¯¯¯¯¯¯¯¯¯¯¯¯¯
087600120919             endif;
087700120919
087800120919         ENDSL;
087900120919
088000120919       ENDSR;
088100120919
088200120919       //--------------------------------------------------------------
088300120919       //?Caricamento dati in tab. "FFC".                              ?
088400120919       //--------------------------------------------------------------
088500120919       BEGSR  sr_RieFFC;
088600120919
088700120919         clear  dFFC;
088800120919
088900120919         dFFC.§FFCpoc = %int( V2Cpoc );
089000120919         if  V2Cdti > *zero;
089100120919           wDate_ISO = %date( V2Cdti : *eur );
089200120919           dFFC.§FFCdti = %dec( wDate_ISO );
089300120919         endif;
089400120919         if  V2Cdtf > *zero;
089500120919           wDate_ISO = %date( V2Cdtf : *eur );
089600120919           dFFC.§FFCdtf = %dec( wDate_ISO );
089700120919         endif;
089800120919
089900120919       ENDSR;
090000120919
090100120919       //--------------------------------------------------------------
090200120919       //?Operazioni finali.                                           ?
090300120919       //--------------------------------------------------------------
090400120919       BEGSR  sr_RoutEnd;
090500120919
090600120919         // -?Chiusura funzioni precedentemente aperte?
090700120919         clear  TIBS02ds;
090800120919         T02tla = 'C';
090900120919         TNTBE_RicercaControllo ( kpjba : tibs02ds );
091000120919
091100120919         // -?Uscita?
091200120919         return;
091300120919
091400120919       ENDSR;
091500120919
091600120919      /end-free
091700120919
091800120919       //--------------------------------------------------------------
091900120919       //?Definizione schiere a tempo di compilazione                  ?
092000120919       //--------------------------------------------------------------
092100120919
092200120919** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
092300120919Filiale di Emissione obbligatoria                                               1
092400120919Filiale di Emissione errata                                                     2
092500120919Filiale di Consegna obbligatoria                                                3
092600120919Filiale di Consegna errata                                                      4
092700120919Data obbligatoria                                                               5
092800120919Data formalmente errata                                                         6
092900120919La data fine forzatura NON può essere precedente la data inizio forzatura       7
