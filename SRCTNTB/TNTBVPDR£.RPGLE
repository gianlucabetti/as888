000100150930       //==============================================================
000200150930       //?Gestione tabella "VPD" (Variabili Calcolo Peso Desunto).     ?
000300150930       //==============================================================
000400150930
000500150930       //--------------------------------------------------------------
000600150930       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150930       //--------------------------------------------------------------
000800150930
000900150930     /*PRM  dbgview(*source)
001000150930     /*END
001100150930
001200150930       //--------------------------------------------------------------
001300150930       //?Specifiche di controllo.                                     ?
001400150930       //--------------------------------------------------------------
001500150930
001600150930     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150930     h dftactgrp(*no)
001800150930
001900150930       //--------------------------------------------------------------
002000150930       //?Dichiarazione file.                                          ?
002100150930       //--------------------------------------------------------------
002200150930
002300150930       // -?Tabelle?
002400150930     fTNTBE01L  Uf A e           k disk
002500150930
002600150930       // -?Video?
002700150930     fTNTBVPDD  cf   e             workstn
002800150930     f                                     indds(IndDspF)
002900150930     f                                     infds(InfDspF)
003000150930
003100150930       //--------------------------------------------------------------
003200150930       //?Definizione costanti.                                        ?
003300150930       //--------------------------------------------------------------
003400150930
003500150930       // -?Codice tabella in gestione?
003600150930     d c_Tab           c                   const('VPD')
003700150930
003800150930       // -?Tasti funzionali a video?
003900150930     d c_F01           c                   const(x'31')
004000150930     d c_F02           c                   const(x'32')
004100150930     d c_F03           c                   const(x'33')
004200150930     d c_F04           c                   const(x'34')
004300150930     d c_F05           c                   const(x'35')
004400150930     d c_F06           c                   const(x'36')
004500150930     d c_F07           c                   const(x'37')
004600150930     d c_F08           c                   const(x'38')
004700150930     d c_F09           c                   const(x'39')
004800150930     d c_F10           c                   const(x'3A')
004900150930     d c_F11           c                   const(x'3B')
005000150930     d c_F12           c                   const(x'3C')
005100150930     d c_F13           c                   const(x'B1')
005200150930     d c_F14           c                   const(x'B2')
005300150930     d c_F15           c                   const(x'B3')
005400150930     d c_F16           c                   const(x'B4')
005500150930     d c_F17           c                   const(x'B5')
005600150930     d c_F18           c                   const(x'B6')
005700150930     d c_F19           c                   const(x'B7')
005800150930     d c_F20           c                   const(x'B8')
005900150930     d c_F21           c                   const(x'B9')
006000150930     d c_F22           c                   const(x'BA')
006100150930     d c_F23           c                   const(x'BB')
006200150930     d c_F24           c                   const(x'BC')
006300150930     d c_Enter         c                   const(x'F1')
006400150930     d c_RollDown      c                   const(x'F4')
006500150930     d c_RollUp        c                   const(x'F5')
006600150930
006700150930       // -?Costante per controllo "caratteri solo numerici"?
006800150930     d c_Digits        c                   const('0123456789')
006900150930
007000150930       //--------------------------------------------------------------
007100150930       //?Definizione schiere.                                         ?
007200150930       //--------------------------------------------------------------
007300150930
007400150930       // -?Messaggi di errore?
007500151001     d sk_Msg          s             78    dim( 7)  ctdata  perrcd( 1)
007600150930
007700150930       //--------------------------------------------------------------
007800150930       //?Definizione aree dati.                                       ?
007900150930       //--------------------------------------------------------------
008000150930
008100150930       // -?Dati utente?
008200150930     d §AzUte        e ds                  extname(AZUTE00F)
008300150930     d                                     dtaara
008400150930     d §DatiUte      e ds                  extname(dDatiUte)
008500150930     d                                     dtaara
008600150930
008700150930       //--------------------------------------------------------------
008800150930       //?Definizione strutture dati.                                  ?
008900150930       //--------------------------------------------------------------
009000150930
009100150930       // -?Status ds?
009200150930     d Status         sds
009300150930     d   SDSpgm          *proc
009400150930
009500150930       // -?InfDS?
009600150930     d InfDspF         ds
009700150930     d   dsp_aid             369    369a
009800150930     d*//sfl_rrn             376    377i 0
009900150930     d*//min_nrr             378    379i 0
010000150930     d*//num_rcds            380    381i 0
010100150930
010200150930       // -?Indicatori su DspF?
010300150930     d IndDspF         ds                  inz
010400150930         // -?Abilitazione tasti funzionali?
010500151001     d   $F3Attivo                     n   overlay( IndDspF : 03 )
010600151001     d   $F5Attivo                     n   overlay( IndDspF : 05 )
010700151001     d   $F6Attivo                     n   overlay( IndDspF : 06 )
010800151001     d   $F12Attivo                    n   overlay( IndDspF : 12 )
010900151001     d   $F16Attivo                    n   overlay( IndDspF : 16 )
011000150930         // -?Emissione messaggio di errore?
011100151001     d   $ErrMessage                   n   overlay( IndDspF : 28 )
011200150930         // -?Posizionamento cursore & segnalazione errore?
011300151001     d   $PosCurKE1                    n   overlay( IndDspF : 50 )
011400151001     d   $PosCurMESI                   n   overlay( IndDspF : 51 )
011500151001     d   $PosCurNSPED                  n   overlay( IndDspF : 52 )
011600151001     d   $PosCurCOLLI                  n   overlay( IndDspF : 53 )
011700151001     d   $PosCurPKDA1                  n   overlay( IndDspF : 54 )
011800151001     d   $PosCurPKAL1                  n   overlay( IndDspF : 55 )
011900151001     d   $PosCurSCOD1                  n   overlay( IndDspF : 56 )
012000151001     d   $PosCurSCOA1                  n   overlay( IndDspF : 57 )
012100151001     d   $PosCurPKDA2                  n   overlay( IndDspF : 58 )
012200151001     d   $PosCurPKAL2                  n   overlay( IndDspF : 59 )
012300151001     d   $PosCurSCOD2                  n   overlay( IndDspF : 60 )
012400151001     d   $PosCurSCOA2                  n   overlay( IndDspF : 61 )
012500150930         // -?Emissione messaggio di errore in window W1?
012600150930     d   $ErrMessageW1...
012700151001     d                                 n   overlay( IndDspF : 90 )
012800150930         //   -?Riemissione videata?
012900151001     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
013000150930
013100150930       // -?Parametri ricevuti?
013200150930     d KPJBA         e ds
013300150930
013400150930       // -?Dati record principale della tabella?
013500150930     d TNTBEds       e ds                  inz  extname(TNTBE00F)
013600150930     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
013700150930     d                                          prefix(TBX:3)
013800150930
013900150930       // -?Tabelle usate:?
014000150930       // ·?"VPD" = Variabili Calcolo Peso Desunto?
014100150930     d dVPD          e ds                  inz
014200150930
014300150930       //--------------------------------------------------------------
014400150930       //?Definizione variabili globali.                               ?
014500150930       //--------------------------------------------------------------
014600150930
014700150930       // -?Flags booleani?
014800150930     d $Fine           s               n   inz
014900150930     d $InzD01         s               n   inz(*on)
015000150930     d $InzD02         s               n   inz(*on)
015100150930     d $InzW01         s               n   inz(*on)
015200150930     d $Annullamento   s               n   inz
015300150930     d $Immissione     s               n   inz
015400150930     d $Ripristino     s               n   inz
015500150930     d $Protect        s               n   inz
015600150930
015700150930       // -?Variabili per la gestione del video?
015800150930     d $Video          s              2    inz('D1')
015900151006
016000151006       // -?Data inizio validità (numerica in fmt aaaammgg)?
016100151006     d W1Cke1          s              8  0 inz
016200150930
016300150930       // -?Campi di comodo?
016400150930     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
016500150930     d wDate           s              8  0 inz
016600150930
016700150930       //--------------------------------------------------------------
016800150930       //?Definizione prototipi procedure.                             ?
016900150930       //--------------------------------------------------------------
017000150930
017100150930       // -?Reperimento dati utente?
017200150930     d TIBS34ds      e ds                  inz
017300150930      /copy gaitrasrc/srcProtoPR,TIBS34R
017400150930
017500151001       // -?Ricerca/Controllo tabelle (TNTBE)?
017600150930     d TIBS02ds      e ds                  inz
017700151001     d   T02mod      e                     inz('R')
017800151001     d   T02cod      e                     inz(c_Tab)
017900151001      /copy gaitrasrc/srcProtoPR,TIBS02R
018000150930
018100150930       // -?Controllo/Inversione data?
018200150930     d WLBdat          ds                  inz
018300150930     d   G08dat                       8  0 inz
018400150930     d   G08inv                       8  0 inz
018500150930     d   G08err                       1    inz('3')
018600150930     d   G08tgi                       5  0 inz
018700150930      /copy gaitrasrc/srcProtoPR,XSRDA8
018800150930
018900150930       //--------------------------------------------------------------
019000150930       //?Definizione key-list.                                        ?
019100150930       //--------------------------------------------------------------
019200150930
019300150930       // -?File TNTBE01L?
019400150930     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
019500150930     d                                     prefix( k_ )   inz
019600150930
019700150930       //--------------------------------------------------------------
019800150930       //?M A I N - L I N E                                            ?
019900150930       //--------------------------------------------------------------
020000150930
020100150930     c     *Entry        plist
020200150930     c                   parm                    KPJBA
020300150930
020400150930      /free
020500150930
020600150930       // -?Operazioni iniziali?
020700150930       exsr sr_RoutInz;
020800150930
020900150930       // -?Ciclo di gestione del file video?
021000150930       DoW  $Fine = *off;
021100150930
021200150930         Select;
021300150930
021400150930           // -?Gestione videata D1: richiesta chiave di accesso?
021500150930           When $Video = 'D1';
021600150930             exsr sr_GesD01;
021700150930
021800150930           // -?Gestione videata D2: manutenzione dati della singola tabella?
021900150930           When $Video = 'D2';
022000150930             exsr sr_GesD02;
022100150930
022200150930           // -?Richiesta dati per trasmissione record?
022300150930           when $Video = 'W1';
022400150930             exsr  sr_GesW01;
022500150930
022600150930           // -?? ? ? ? ??
022700150930           Other;
022800150930             $Fine = *on;
022900150930
023000150930         EndSl;
023100150930
023200150930       EndDo;
023300150930
023400150930       // -?Operazioni finali?
023500150930       exsr sr_RoutEnd;
023600150930
023700150930       //--------------------------------------------------------------
023800150930       //?Operazioni iniziali.                                         ?
023900150930       //--------------------------------------------------------------
024000150930       BEGSR  sr_RoutInz;
024100150930
024200150930         // -?Impostazione chiusura?
024300150930         *inLR = *on;
024400150930
024500150930         // -?Reperimento dati job?
024600150930         exsr  sr_DatiJob;
024700150930
024800150930         // -?Reperimento data odierna?
024900150930         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025000150930                               : 1 : 8 ) );
025100150930
025200150930         // -?Impostazione nome programma a video?
025300150930         V1Tpgm = SDSpgm;
025400150930
025500150930         // -?Aggancio dati generali della tabella in esame?
025600150930         clear  keyTNTBE01;
025700150930         k_TBEke1 = *zero;
025800150930         %subst( k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab) )
025900150930                = c_Tab;
026000150930         k_TBEsif = KNSIF;
026100150930         chain(n)  %kds(keyTNTBE01)  TNTBE000;
026200150930         if  not  %found(TNTBE01L);
026300150930           clear  k_TBEsif;
026400150930           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
026500150930         endif;
026600150930         if  %found(TNTBE01L);
026700150930           xTNTBEds = TNTBEds;
026800150930         else;
026900150930           clear  xTNTBEds;
027000150930         endif;
027100150930
027200150930         // -?Impostazione iniziale / pulizia dei campi chiave?
027300150930         clear  keyTNTBE01;
027400150930         k_TBEcod = c_Tab;
027500150930
027600150930       ENDSR;
027700150930
027800150930       //--------------------------------------------------------------
027900150930       //?Reperimento Dati del job (Utente/Operativi).                 ?
028000150930       //--------------------------------------------------------------
028100150930       BEGSR  sr_DatiJob;
028200150930
028300150930         in(E)  §AzUte;
028400150930         if  NOT %error;
028500150930           in(E)  §DatiUte;
028600150930         endif;
028700150930         if  %error  or  RSut = *blanks;
028800150930           clear  TIBS34ds;
028900150930           tibs34r ( tibs34ds );
029000150930           in  §AzUte;
029100150930           in  §DatiUte;
029200150930         endif;
029300150930
029400150930       ENDSR;
029500150930
029600150930       //--------------------------------------------------------------
029700150930       //?Gestione videata D01.                                        ?
029800150930       //--------------------------------------------------------------
029900150930       BEGSR  sr_GesD01;
030000150930
030100150930         // -?Inizializzazione videata?
030200150930         if  $InzD01 = *on;
030300150930           exsr  sr_InzD01;
030400150930           $InzD01 = *off;
030500150930         endif;
030600150930         clear  V1Topz;
030700150930
030800150930         // -?(Dis)Abilitazione Tasti Funzionali?
030900150930         // -?F3=Fine?
031000151001         $F3Attivo  = *on;
031100150930         // -?F5=Ripristino?
031200151001         $F5Attivo  = *off;
031300150930         // -?F6=Conferma?
031400151001         $F6Attivo  = *off;
031500150930         // -?F12=Ritorno?
031600151001         $F12Attivo = *off;
031700150930         // -?F16=Annullamento?
031800151001         $F16Attivo = *off;
031900150930
032000150930         // -?Emissione videata?
032100151001         write  tbVPDt01;
032200151001         write  tbVPDp01;
032300151001         exfmt  tbVPDd01;
032400150930
032500150930         clear  V1Dmsg;
032600151001         reset  $ErrMessage;
032700151001         reset  $ErrGenerico;
032800150930
032900150930         SELECT;
033000150930
033100150930           // -?Utente NON di Sede (errore già visualizzato)?
033200150930           WHEN  DUTlpo <> 'S';
033300150930             $Fine = *on;
033400150930
033500150930           // -?F3=Fine?
033600150930           WHEN  dsp_aid = c_F03;
033700150930             $Fine = *on;
033800150930
033900150930           // -?Invio?
034000150930           OTHER;
034100150930             exsr  sr_CtrD01;
034200151001             if  $ErrGenerico = *off;
034300150930               $Video    = 'D2';
034400150930               $InzD02   = *on;
034500150930             endif;
034600150930
034700150930         ENDSL;
034800150930
034900150930       ENDSR;
035000150930
035100150930       //--------------------------------------------------------------
035200150930       //?Inizializzazione videata D01.                                ?
035300150930       //--------------------------------------------------------------
035400150930       BEGSR  sr_InzD01;
035500150930
035600151001         clear  tbVPDd01;
035700150930
035800151001         V1Cke1 = '?';
035900150930
036000150930         // -?Eseguibile SOLO da Sede?
036100150930         if  DUTlpo <> 'S';
036200150930           V1Dmsg = sk_Msg(01);
036300151001           $ErrMessage  = *on;
036400151001           $ErrGenerico = *on;
036500150930           leavesr;
036600150930         endif;
036700150930
036800150930       ENDSR;
036900150930
037000150930       //--------------------------------------------------------------
037100150930       //?Controllo dati immessi nella videata D01.                    ?
037200150930       //--------------------------------------------------------------
037300150930       BEGSR  sr_CtrD01;
037400150930
037500150930         // -?Spegnimento indicatori di posizionamento cursore?
037600151001         %subst( IndDspF : 28 ) = *off;
037700150930
037800151006         // -?Data inizio validità (chiave)?
037900151006         clear  W1Cke1;
038000150930         Select;
038100150930
038200150930           // -?Ricerca?
038300151001           When  %scan( '?' : V1Cke1 ) > *zero;
038400151001             clear  V1Cke1;
038500151001             reset  TIBS02ds;
038600151001             //T02mod = 'R';                //?(già così)?
038700151001             //T02cod = c_Tab;              //?(già così)?
038800151001             TNTBE_RicercaControllo ( KPJBA : TIBS02ds );
038900151001             if  T02err = *blank;
039000151006               //V1Cke1 = T02ke1;
039100151006               reset  WLBdat;
039200151006               G08inv = %int( %trim( T02ke1 ) );
039300151006               XSRDA8 ( WLBdat );
039400151006               if  G08err = *on;
039500151006                 V1Dmsg = sk_Msg(03);
039600151006                 $PosCurKE1   = *on;
039700151006                 $ErrMessage  = *on;
039800151006                 $ErrGenerico = *on;
039900151006                 leavesr;
040000151006               else;
040100151006                 V1Cke1 = %editc( G08dat : 'X' );
040200151006                 W1Cke1 = G08inv;
040300151006               endif;
040400151001             else;
040500151001               $ErrMessage = *on;
040600151001               V1Dmsg = T02msg;
040700151001             endif;
040800151001             $PosCurKE1   = *on;
040900151001             $ErrGenerico = *on;
041000150930             leavesr;
041100150930
041200150930           // -?Controllo immissione?
041300151001           When  V1Cke1 = *blank  or  %trim( V1Cke1 ) = *zero;
041400150930             V1Dmsg = sk_Msg(02);
041500151001             $PosCurKE1   = *on;
041600151001             $ErrMessage  = *on;
041700151001             $ErrGenerico = *on;
041800150930             leavesr;
041900151006
042000150930           // -?Controllo immissione solo caratteri numerici?
042100151006           When  %check( c_Digits : %trim( V1Cke1 ) ) > *zero;
042200150930             V1Dmsg = sk_Msg(03);
042300151001             $PosCurKE1   = *on;
042400151001             $ErrMessage  = *on;
042500151001             $ErrGenerico = *on;
042600150930             leavesr;
042700151006
042800151006           // -?Controllo correttezza formale data inserita?
042900151006           Other;
043000151006             clear  WLBdat;
043100151006             G08dat = %int( %trim( V1Cke1 ) );
043200151006             XSRDA8 ( WLBdat );
043300151006             if  G08err = *on;
043400151006               V1Dmsg = sk_Msg(03);
043500151006               $PosCurKE1   = *on;
043600151006               $ErrMessage  = *on;
043700151006               $ErrGenerico = *on;
043800151006               leavesr;
043900151006             else;
044000151006               W1Cke1 = G08inv;
044100151006             endif;
044200150930
044300150930         EndSl;
044400150930
044500150930       ENDSR;
044600150930
044700150930       //--------------------------------------------------------------
044800150930       //?Gestione videata D02.                                        ?
044900150930       //--------------------------------------------------------------
045000150930       BEGSR  sr_GesD02;
045100150930
045200150930         // -?Inizializzazione videata?
045300150930         if  $InzD02 = *on;
045400150930           exsr  sr_InzD02;
045500150930           $InzD02 = *off;
045600150930         endif;
045700150930
045800150930         // -?Emissione Testata e Piede con tasti funzionali abilitati?
045900151001         if  NOT $ErrMessage;
046000151001           write  tbVPDt01;
046100151006           //write  tbVPDd01;
046200150930         endif;
046300151001         write  tbVPDp01;
046400150930
046500150930         // -?Emissione videata?
046600150930         if  Not $Protect;
046700150930           write  Protect;
046800151001           exfmt  tbVPDd02;
046900150930         else;
047000151001           write  tbVPDd02;
047100150930           exfmt  Protect;
047200150930         endif;
047300150930
047400150930         clear  V1Dmsg;
047500150930         reset  $ErrMessage;
047600150930         reset  $ErrGenerico;
047700150930
047800150930         SELECT;
047900150930
048000150930           // -?F3=Fine?
048100150930           WHEN  dsp_aid = c_F03;
048200150930             unlock  TNTBE01L;
048300150930             $Fine = *on;
048400150930
048500150930           // -?F12=Ritorno?
048600150930           WHEN  dsp_aid = c_F12;
048700150930             unlock  TNTBE01L;
048800150930             reset  $Video;
048900150930             clear  V1Topz;
049000150930
049100150930           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
049200150930           OTHER;
049300150930             $Ripristino   = (dsp_aid = c_F05);
049400150930             $Annullamento = (dsp_aid = c_F16);
049500150930             // -?Controlli eseguiti NON se F16=Annullamento?
049600150930             if  Not $Annullamento;
049700150930               exsr  sr_CtrD02;
049800150930             // -?Controlli eseguiti SOLO se F16=Annullamento?
049900150930             //else;
050000150930             //  exsr  sr_CtrANN;
050100150930             endif;
050200150930             if  $ErrGenerico;
050300150930               leavesr;
050400150930             endif;
050500150930             // -?Aggiornamento?
050600150930             if  dsp_aid = c_F05   or
050700150930                 dsp_aid = c_F06   or
050800150930                 dsp_aid = c_F16;
050900150930               $Video = 'W1';
051000150930               reset  $InzW01;
051100150930               exsr  sr_GesW01;
051200150930             endif;
051300150930
051400150930         ENDSL;
051500150930
051600150930       ENDSR;
051700150930
051800150930       //--------------------------------------------------------------
051900150930       //?Inizializzazione videata D02.                                ?
052000150930       //--------------------------------------------------------------
052100150930       BEGSR  sr_InzD02;
052200150930
052300150930         reset  $Immissione;
052400150930         reset  $Ripristino;
052500150930         reset  $Protect;
052600150930
052700150930         // -?Spegnimento degli indicatori di errore?
052800151001         %subst( IndDspF : 50 ) = *off;
052900150930
053000150930         // -?Pulizia videata?
053100151001         clear  tbVPDd02;
053200150930
053300150930         // -?Verifica esistenza del record in TNTBE?
053400150930         //  ?e Reperimento dati?
053500151006         k_TBEke1 = %editc( W1Cke1 : 'X' );
053600150930         chain  %kds( keyTNTBE01 )  TNTBE000;
053700150930
053800150930         // -?Impostazione testata?
053900150930         Select;
054000150930           When  Not %found(TNTBE01L);
054100150930             V1Topz = '  INSERIMENTO  ';
054200150930             $Immissione = *on;
054300150930           When  TBEatb <> *blank;
054400150930             V1Topz = '  RIPRISTINO   ';
054500150930             $Ripristino = *on;
054600150930           Other;
054700150930             V1Topz = '   MODIFICA    ';
054800150930         EndSl;
054900150930
055000150930         // -?Caricamento dati?
055100151006         V2Cke1 = G08dat;
055200150930         clear  dVPD;
055300150930         If  %found(TNTBE01L);
055400150930
055500150930           dVPD = TBEuni;
055600150930
055700150930           V1mesi   = §VPDmesi;
055800150930           V1nSped  = §VPDnSped;
055900150930           V1colli  = §VPDcolli;
056000150930           V1pKda1  = §VPDpKda1;
056100150930           V1pKal1  = §VPDpKal1;
056200150930           V1scoDa1 = §VPDscoDa1;
056300150930           V1scoA1  = §VPDscoA1;
056400150930           V1pKda2  = §VPDpKda2;
056500150930           V1pKal2  = §VPDpKal2;
056600150930           V1scoDa2 = §VPDscoDa2;
056700150930           V1scoA2  = §VPDscoA2;
056800150930
056900150930         EndIf;
057000150930
057100150930         // -?Impostazione indicatori per abilitazione tasti funzionali?
057200150930         exsr  sr_AbilitFxxD02;
057300150930
057400150930       ENDSR;
057500150930
057600150930       //--------------------------------------------------------------
057700150930       //?Settaggio indicatori nella videata D02 per abilitazione      ?
057800150930       //?  tasti funzionali.                                          ?
057900150930       //--------------------------------------------------------------
058000150930       BEGSR  sr_AbilitFxxD02;
058100150930
058200150930         // -?F3=Fine?
058300151001         $F3Attivo  = *on;
058400150930
058500150930         // -?F5=Ripristino?
058600151001         $F5Attivo  = ( %found(TNTBE01L)  and  TBEatb <> *blank
058700151001                                          and  Not $Protect );
058800150930
058900150930         // -?F6=Conferma?
059000151001         $F6Attivo  = ( NOT $F5Attivo );
059100150930
059200150930         // -?F12=Ritorno?
059300151001         $F12Attivo = *on;
059400150930
059500150930         // -?F16=Annullamento?
059600151001         $F16Attivo = ( %found(TNTBE01L)  and  TBEatb = *blank
059700151001                                          and  Not $Protect );
059800150930
059900150930       ENDSR;
060000150930
060100150930       //--------------------------------------------------------------
060200150930       //?Controllo dati videata D02.                                  ?
060300150930       //--------------------------------------------------------------
060400150930       BEGSR  sr_CtrD02;
060500150930
060600150930         // -?Spegnimento degli indicatori di errore?
060700151001         %subst( IndDspF : 50 ) = *off;
060800150930
060900151001         // -?Nr max Mesi indietro per ricerca Statistica Peso?
061000151001         select;
061100151001           when  V1mesi   = *zero;
061200151001             $ErrGenerico = *on;
061300151001             $ErrMessage  = *on;
061400151001             $PosCurMESI  = *on;
061500151001             V1Dmsg = sk_Msg(04);
061600151001             leavesr;
061700151001           when  V1mesi   < *zero;
061800151001             $ErrGenerico = *on;
061900151001             $ErrMessage  = *on;
062000151001             $PosCurMESI  = *on;
062100151001             V1Dmsg = sk_Msg(05);
062200151001             leavesr;
062300151001         endsl;
062400150930
062500151001         // -?Nr min Spedizioni analizzate in Statistica Pesi?
062600151001         select;
062700151001           when  V1nSped  = *zero;
062800151001             $ErrGenerico = *on;
062900151001             $ErrMessage  = *on;
063000151001             $PosCurNSPED = *on;
063100151001             V1Dmsg = sk_Msg(04);
063200151001             leavesr;
063300151001           when  V1nSped  < *zero;
063400151001             $ErrGenerico = *on;
063500151001             $ErrMessage  = *on;
063600151001             $PosCurNSPED = *on;
063700151001             V1Dmsg = sk_Msg(05);
063800151001             leavesr;
063900151001         endsl;
064000151001
064100151001         // -?Percentuale Colli VdL rilevati in Statistica Pesi?
064200151001         select;
064300151001           when  V1colli  = *zero;
064400151001             $ErrGenerico = *on;
064500151001             $ErrMessage  = *on;
064600151001             $PosCurCOLLI = *on;
064700151001             V1Dmsg = sk_Msg(04);
064800151001             leavesr;
064900151001           when  V1colli  < *zero;
065000151001             $ErrGenerico = *on;
065100151001             $ErrMessage  = *on;
065200151001             $PosCurCOLLI = *on;
065300151001             V1Dmsg = sk_Msg(05);
065400151001             leavesr;
065500151001         endsl;
065600151001
065700151001         // -?1° scaglione di Peso x verifica % Scostamento?
065800151001
065900151001         // -?Peso KG (1° scaglione di Peso)?
066000151001         select;
066100151001           when  V1pKda1  = *zero;
066200151001             $ErrGenerico = *on;
066300151001             $ErrMessage  = *on;
066400151001             $PosCurPKDA1 = *on;
066500151001             V1Dmsg = sk_Msg(04);
066600151001             leavesr;
066700151001           when  V1pKda1  < *zero;
066800151001             $ErrGenerico = *on;
066900151001             $ErrMessage  = *on;
067000151001             $PosCurPKDA1 = *on;
067100151001             V1Dmsg = sk_Msg(05);
067200151001             leavesr;
067300151001           when  V1pKal1  = *zero;
067400151001             $ErrGenerico = *on;
067500151001             $ErrMessage  = *on;
067600151001             $PosCurPKAL1 = *on;
067700151001             V1Dmsg = sk_Msg(04);
067800151001             leavesr;
067900151001           when  V1pKal1  < *zero;
068000151001             $ErrGenerico = *on;
068100151001             $ErrMessage  = *on;
068200151001             $PosCurPKAL1 = *on;
068300151001             V1Dmsg = sk_Msg(05);
068400151001             leavesr;
068500151001         endsl;
068600151001
068700151001         // -?Controllo Range di Peso KG (1° scaglione di Peso)?
068800151001         if  V1pKda1 > V1pKal1;
068900151001           $ErrGenerico = *on;
069000151001           $ErrMessage  = *on;
069100151001           $PosCurPKDA1 = *on;
069200151001           V1Dmsg = sk_Msg(06);
069300151001           leavesr;
069400151001         endif;
069500151001
069600151001         // -?% Scostamento Negativo (1° scaglione di Peso )?
069700151001         V1scoDa1 = %abs( V1scoDa1 );
069800151001         if  V1scoDa1  <= *zero;
069900151001           $ErrGenerico = *on;
070000151001           $ErrMessage  = *on;
070100151001           $PosCurSCOD1 = *on;
070200151001           V1Dmsg = sk_Msg(04);
070300151001           leavesr;
070400151001         endif;
070500151001         V1scoA1  = %abs( V1scoA1 );
070600151001         if  V1scoA1   <= *zero;
070700151001           $ErrGenerico = *on;
070800151001           $ErrMessage  = *on;
070900151001           $PosCurSCOA1 = *on;
071000151001           V1Dmsg = sk_Msg(04);
071100151001           leavesr;
071200151001         endif;
071300151001
071400151001         // -?Controllo Range della % Scostamento (1° scaglione di Peso)?
071500151001         if  V1scoDa1 < V1scoA1;
071600151001           $ErrGenerico = *on;
071700151001           $ErrMessage  = *on;
071800151001           $PosCurSCOD1 = *on;
071900151001           V1Dmsg = sk_Msg(06);
072000151001           leavesr;
072100151001         endif;
072200151001
072300151001         // -?2° scaglione di Peso x verifica % Scostamento?
072400151001
072500151001         // -?Peso KG (2° scaglione di Peso)?
072600151001         select;
072700151001           //when  V1pKda2  = *zero;
072800151001           //  $ErrGenerico = *on;
072900151001           //  $ErrMessage  = *on;
073000151001           //  $PosCurPKDA2 = *on;
073100151001           //  V1Dmsg = sk_Msg(04);
073200151001           //  leavesr;
073300151001           when  V1pKda2 <= V1pKal1;
073400151001             $ErrGenerico = *on;
073500151001             $ErrMessage  = *on;
073600151001             $PosCurPKDA2 = *on;
073700151001             V1Dmsg = sk_Msg(07);
073800151001             leavesr;
073900151001           when  V1pKda2  < *zero;
074000151001             $ErrGenerico = *on;
074100151001             $ErrMessage  = *on;
074200151001             $PosCurPKDA2 = *on;
074300151001             V1Dmsg = sk_Msg(05);
074400151001             leavesr;
074500151001           when  V1pKal2  = *zero;
074600151001             $ErrGenerico = *on;
074700151001             $ErrMessage  = *on;
074800151001             $PosCurPKAL2 = *on;
074900151001             V1Dmsg = sk_Msg(04);
075000151001             leavesr;
075100151001           when  V1pKal2  < *zero;
075200151001             $ErrGenerico = *on;
075300151001             $ErrMessage  = *on;
075400151001             $PosCurPKAL2 = *on;
075500151001             V1Dmsg = sk_Msg(05);
075600151001             leavesr;
075700151001         endsl;
075800151001
075900151001         // -?Controllo Range di Peso KG (2° scaglione di Peso)?
076000151001         if  V1pKda2 > V1pKal2;
076100151001           $ErrGenerico = *on;
076200151001           $ErrMessage  = *on;
076300151001           $PosCurPKDA2 = *on;
076400151001           V1Dmsg = sk_Msg(06);
076500151001           leavesr;
076600151001         endif;
076700151001
076800151001         // -?% Scostamento Negativo (2° scaglione di Peso)?
076900151001         V1scoDa2 = %abs( V1scoDa2 );
077000151001         if  V1scoDa2 <= *zero;
077100151001           $ErrGenerico = *on;
077200151001           $ErrMessage  = *on;
077300151001           $PosCurSCOD2 = *on;
077400151001           V1Dmsg = sk_Msg(04);
077500151001           leavesr;
077600151001         endif;
077700151001         V1scoA2  = %abs( V1scoA2 );
077800151001         if  V1scoA2  <= *zero;
077900151001           $ErrGenerico = *on;
078000151001           $ErrMessage  = *on;
078100151001           $PosCurSCOA2 = *on;
078200151001           V1Dmsg = sk_Msg(04);
078300151001           leavesr;
078400151001         endif;
078500151001
078600151001         // -?Controllo Range della % Scostamento (2° scaglione di Peso)?
078700151001         if  V1scoDa2 < V1scoA2;
078800151001           $ErrGenerico = *on;
078900151001           $ErrMessage  = *on;
079000151001           $PosCurSCOD2 = *on;
079100151001           V1Dmsg = sk_Msg(06);
079200151001           leavesr;
079300151001         endif;
079400150930
079500150930       ENDSR;
079600150930
079700150930       //--------------------------------------------------------------
079800150930       //?Gestione trasmissione aggiornamento                          ?
079900150930       //--------------------------------------------------------------
080000150930       BEGSR  sr_GesW01;
080100150930
080200150930         // -?Inizializzazione videata?
080300150930         if $InzW01 = *on;
080400150930           exsr  sr_InzW01;
080500150930           $InzW01 = *off;
080600150930         endif;
080700150930
080800150930         // -?Emissione window?
080900150930         if  TBXftt = 'S';
081000151001           exfmt  tbVPDw01;
081100150930           $ErrMessage  = *off;
081200150930           $ErrGenerico = *off;
081300150930           clear  V1Dmsg;
081400150930         else;
081500150930           dsp_aid = c_F06;
081600150930         endif;
081700150930
081800150930         select;
081900150930
082000150930           // -?F12=Ritorno (gestito solo se emesso W01)?
082100150930           when  dsp_aid = c_F12;
082200150930             $Video = 'D2';
082300150930
082400150930           // -?F6=Conferma?
082500150930           when  dsp_aid = c_F06;
082600150930             exsr  sr_UpdTNTBE;
082700150930             // -?Ritorno alla videata iniziale?
082800150930             if  NOT  $ErrGenerico;
082900151001               $Video  = 'D1';
083000151001               $InzD01 = *on;
083100150930             endif;
083200150930
083300150930         endsl;
083400150930
083500150930       ENDSR;
083600150930
083700150930       //--------------------------------------------------------------
083800150930       //?Inizializzazione videata W01                                 ?
083900150930       //--------------------------------------------------------------
084000150930       BEGSR  sr_InzW01;
084100150930
084200151001         clear tbVPDw01;
084300150930
084400150930         if  $Immissione;
084500150930
084600150930           // -?Se immissione?
084700150930           W1ftt  = TBXftt;
084800150930
084900150930         else;
085000150930
085100150930           // -?Se NON immissione:?
085200150930           //  ?visualizza i dati relativi all'ultima trasmissione?
085300150930           W1ftt  = TBEftt;
085400150930           W1flt  = TBEflt;
085500150930           W1ftr  = TBEftr;
085600150930           if TBEdtr <> *zero;
085700150930             wDate_EUR = %date(TBEdtr : *iso);
085800150930             W1dtr     = %dec(wDate_EUR);
085900150930           endif;
086000150930
086100150930         endif;
086200150930
086300150930       ENDSR;
086400150930
086500150930       //--------------------------------------------------------------
086600150930       //?Aggiornamento records TNTBE00F (tab. "VPD")                  ?
086700150930       //--------------------------------------------------------------
086800150930       BEGSR  sr_UpdTNTBE;
086900150930
087000150930         // -?Aggancio record di TNTBE00F?
087100150930         clear  keyTNTBE01;
087200150930         k_TBEcod  = c_Tab;
087300151006         k_TBEke1  = %editc( W1Cke1 : 'X' );
087400150930         chain  %kds( keyTNTBE01 )  TNTBE000;
087500150930
087600150930
087700150930         clear  dVPD;
087800150930         §VPDmesi   = V1mesi;
087900150930         §VPDnSped  = V1nSped;
088000150930         §VPDcolli  = V1colli;
088100150930         §VPDpKda1  = V1pKda1;
088200150930         §VPDpKal1  = V1pKal1;
088300150930         §VPDscoDa1 = V1scoDa1;
088400150930         §VPDscoA1  = V1scoA1;
088500150930         §VPDpKda2  = V1pKda2;
088600150930         §VPDpKal2  = V1pKal2;
088700150930         §VPDscoDa2 = V1scoDa2;
088800150930         §VPDscoA2  = V1scoA2;
088900151006
089000151006         if  NOT %found(TNTBE01L);
089100151006           clear  TNTBE000;
089200151006           TBEcod  = k_TBEcod;
089300151006           TBEke1  = k_TBEke1;
089400151006           TBEke2  = k_TBEke2;
089500151006           TBElin  = k_TBElin;
089600151006           TBEsif  = k_TBEsif;
089700151006         endif;
089800150930
089900150930         TBEuni = dVPD;
090000150930
090100150930         TBEapl = TBXapl;
090200150930         TBEftt = W1ftt;
090300150930         TBEflt = W1flt;
090400150930         clear  TBEftr;
090500150930         //clear  TBEdtr;
090600150930
090700150930         // -?Aggiornamento tab. "VPD"?
090800150930         SELECT;
090900150930
091000150930           // -?F5=Ripristino?
091100150930           WHEN  $Ripristino;
091200150930             select;
091300150930               when  %found(TNTBE01L)  and  TBEatb <> *blank;
091400150930                 clear  TBEatb;
091500150930                 //______________
091600150930                 UPDATE  TNTBE000;
091700150930                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
091800150930               // -?Record appena cancellato fisicamente?
091900150930               when  NOT %found(TNTBE01L);
092000150930                 //______________
092100150930                 WRITE   TNTBE000;
092200150930                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
092300150930             endsl;
092400150930
092500150930           // -?F16=Annullamento?
092600150930           WHEN  $Annullamento;
092700150930             if  %found(TNTBE01L)  and  TBEatb <> 'A';
092800150930               TBEatb = 'A';
092900150930               //______________
093000150930               UPDATE  TNTBE000;
093100150930               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
093200150930             endif;
093300150930
093400150930           // -?Immissione o Modifica?
093500150930           OTHER;
093600150930             if  NOT %found(TNTBE01L);
093700150930               //_____________
093800150930               WRITE  TNTBE000;
093900150930               //¯¯¯¯¯¯¯¯¯¯¯¯¯
094000150930             else;
094100150930               clear  TBEatb;
094200150930               //_____________
094300150930               UPDATE TNTBE000;
094400150930               //¯¯¯¯¯¯¯¯¯¯¯¯¯
094500150930             endif;
094600150930
094700150930         ENDSL;
094800150930
094900150930       ENDSR;
095000150930
095100150930       //--------------------------------------------------------------
095200150930       //?Operazioni finali.                                           ?
095300150930       //--------------------------------------------------------------
095400150930       BEGSR  sr_RoutEnd;
095500150930
095600150930         // -?Chiusura funzioni precedentemente aperte?
095700150930         clear  TIBS02ds;
095800150930         T02tla = 'C';
095900150930         TNTBE_RicercaControllo ( kpjba : tibs02ds );
096000150930
096100150930         //  ?Uscita?
096200150930         return;
096300150930
096400150930       ENDSR;
096500150930
096600150930      /end-free
096700150930
096800150930       //--------------------------------------------------------------
096900150930       //?Definizione schiere a tempo di compilazione.                 ?
097000150930       //--------------------------------------------------------------
097100150930
097200150930** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
097300150930Tabella gestibile SOLO da SEDE                                                  1
097400150930Data obbligatoria                                                               2
097500150930Data formalmente errata                                                         3
097600151001Campo obbligatorio                                                              4
097700151001Valori negativi NON ammessi                                                     5
097800151001Valore iniziale maggiore di quello finale                                       6
097900151001Peso in KG iniziale 2° scaglione  NON superiore  a quello finale 1° scaglione   7
