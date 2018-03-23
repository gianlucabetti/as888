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
017500151008       // -?Ricerca tabella "VPD"?
017600151008     d tntbVPD1r       pr                  extpgm('TNTBVPD1R')
017700151008     d   kpjba                             likeds(KPJBA)
017800150930
017900150930       // -?Controllo/Inversione data?
018000150930     d WLBdat          ds                  inz
018100150930     d   G08dat                       8  0 inz
018200150930     d   G08inv                       8  0 inz
018300150930     d   G08err                       1    inz('3')
018400150930     d   G08tgi                       5  0 inz
018500150930      /copy gaitrasrc/srcProtoPR,XSRDA8
018600150930
018700150930       //--------------------------------------------------------------
018800150930       //?Definizione key-list.                                        ?
018900150930       //--------------------------------------------------------------
019000150930
019100150930       // -?File TNTBE01L?
019200150930     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
019300150930     d                                     prefix( k_ )   inz
019400150930
019500150930       //--------------------------------------------------------------
019600150930       //?M A I N - L I N E                                            ?
019700150930       //--------------------------------------------------------------
019800150930
019900150930     c     *Entry        plist
020000150930     c                   parm                    KPJBA
020100150930
020200150930      /free
020300150930
020400150930       // -?Operazioni iniziali?
020500150930       exsr sr_RoutInz;
020600150930
020700150930       // -?Ciclo di gestione del file video?
020800150930       DoW  $Fine = *off;
020900150930
021000150930         Select;
021100150930
021200150930           // -?Gestione videata D1: richiesta chiave di accesso?
021300150930           When $Video = 'D1';
021400150930             exsr sr_GesD01;
021500150930
021600150930           // -?Gestione videata D2: manutenzione dati della singola tabella?
021700150930           When $Video = 'D2';
021800150930             exsr sr_GesD02;
021900150930
022000150930           // -?Richiesta dati per trasmissione record?
022100150930           when $Video = 'W1';
022200150930             exsr  sr_GesW01;
022300150930
022400150930           // -?? ? ? ? ??
022500150930           Other;
022600150930             $Fine = *on;
022700150930
022800150930         EndSl;
022900150930
023000150930       EndDo;
023100150930
023200150930       // -?Operazioni finali?
023300150930       exsr sr_RoutEnd;
023400150930
023500150930       //--------------------------------------------------------------
023600150930       //?Operazioni iniziali.                                         ?
023700150930       //--------------------------------------------------------------
023800150930       BEGSR  sr_RoutInz;
023900150930
024000150930         // -?Impostazione chiusura?
024100150930         *inLR = *on;
024200150930
024300150930         // -?Reperimento dati job?
024400150930         exsr  sr_DatiJob;
024500150930
024600150930         // -?Reperimento data odierna?
024700150930         wDate = %int( %subst( %char( %dec( %timestamp() ) )
024800150930                               : 1 : 8 ) );
024900150930
025000150930         // -?Impostazione nome programma a video?
025100150930         V1Tpgm = SDSpgm;
025200150930
025300150930         // -?Aggancio dati generali della tabella in esame?
025400150930         clear  keyTNTBE01;
025500150930         k_TBEke1 = *zero;
025600150930         %subst( k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab) )
025700150930                = c_Tab;
025800150930         k_TBEsif = KNSIF;
025900150930         chain(n)  %kds(keyTNTBE01)  TNTBE000;
026000150930         if  not  %found(TNTBE01L);
026100150930           clear  k_TBEsif;
026200150930           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
026300150930         endif;
026400150930         if  %found(TNTBE01L);
026500150930           xTNTBEds = TNTBEds;
026600150930         else;
026700150930           clear  xTNTBEds;
026800150930         endif;
026900150930
027000150930         // -?Impostazione iniziale / pulizia dei campi chiave?
027100150930         clear  keyTNTBE01;
027200150930         k_TBEcod = c_Tab;
027300150930
027400150930       ENDSR;
027500150930
027600150930       //--------------------------------------------------------------
027700150930       //?Reperimento Dati del job (Utente/Operativi).                 ?
027800150930       //--------------------------------------------------------------
027900150930       BEGSR  sr_DatiJob;
028000150930
028100150930         in(E)  §AzUte;
028200150930         if  NOT %error;
028300150930           in(E)  §DatiUte;
028400150930         endif;
028500150930         if  %error  or  RSut = *blanks;
028600150930           clear  TIBS34ds;
028700150930           tibs34r ( tibs34ds );
028800150930           in  §AzUte;
028900150930           in  §DatiUte;
029000150930         endif;
029100150930
029200150930       ENDSR;
029300150930
029400150930       //--------------------------------------------------------------
029500150930       //?Gestione videata D01.                                        ?
029600150930       //--------------------------------------------------------------
029700150930       BEGSR  sr_GesD01;
029800150930
029900150930         // -?Inizializzazione videata?
030000150930         if  $InzD01 = *on;
030100150930           exsr  sr_InzD01;
030200150930           $InzD01 = *off;
030300150930         endif;
030400150930         clear  V1Topz;
030500150930
030600150930         // -?(Dis)Abilitazione Tasti Funzionali?
030700150930         // -?F3=Fine?
030800151001         $F3Attivo  = *on;
030900150930         // -?F5=Ripristino?
031000151001         $F5Attivo  = *off;
031100150930         // -?F6=Conferma?
031200151001         $F6Attivo  = *off;
031300150930         // -?F12=Ritorno?
031400151001         $F12Attivo = *off;
031500150930         // -?F16=Annullamento?
031600151001         $F16Attivo = *off;
031700150930
031800150930         // -?Emissione videata?
031900151001         write  tbVPDt01;
032000151001         write  tbVPDp01;
032100151001         exfmt  tbVPDd01;
032200150930
032300150930         clear  V1Dmsg;
032400151001         reset  $ErrMessage;
032500151001         reset  $ErrGenerico;
032600150930
032700150930         SELECT;
032800150930
032900150930           // -?Utente NON di Sede (errore già visualizzato)?
033000150930           WHEN  DUTlpo <> 'S';
033100150930             $Fine = *on;
033200150930
033300150930           // -?F3=Fine?
033400150930           WHEN  dsp_aid = c_F03;
033500150930             $Fine = *on;
033600150930
033700150930           // -?Invio?
033800150930           OTHER;
033900150930             exsr  sr_CtrD01;
034000151001             if  $ErrGenerico = *off;
034100150930               $Video    = 'D2';
034200150930               $InzD02   = *on;
034300150930             endif;
034400150930
034500150930         ENDSL;
034600150930
034700150930       ENDSR;
034800150930
034900150930       //--------------------------------------------------------------
035000150930       //?Inizializzazione videata D01.                                ?
035100150930       //--------------------------------------------------------------
035200150930       BEGSR  sr_InzD01;
035300150930
035400151001         clear  tbVPDd01;
035500150930
035600151001         V1Cke1 = '?';
035700150930
035800150930         // -?Eseguibile SOLO da Sede?
035900150930         if  DUTlpo <> 'S';
036000150930           V1Dmsg = sk_Msg(01);
036100151001           $ErrMessage  = *on;
036200151001           $ErrGenerico = *on;
036300150930           leavesr;
036400150930         endif;
036500150930
036600150930       ENDSR;
036700150930
036800150930       //--------------------------------------------------------------
036900150930       //?Controllo dati immessi nella videata D01.                    ?
037000150930       //--------------------------------------------------------------
037100150930       BEGSR  sr_CtrD01;
037200150930
037300150930         // -?Spegnimento indicatori di posizionamento cursore?
037400151001         %subst( IndDspF : 28 ) = *off;
037500150930
037600151006         // -?Data inizio validità (chiave)?
037700151006         clear  W1Cke1;
037800150930         Select;
037900150930
038000150930           // -?Ricerca?
038100151001           When  %scan( '?' : V1Cke1 ) > *zero;
038200151001             clear  V1Cke1;
038300151008             clear  KPJBU;
038400151008             tntbVPD1r ( KPJBA );
038500151008             if  KPJBU <> *blank;
038600151008               //V1Cke1 = KPJBU;
038700151006               reset  WLBdat;
038800151008               G08inv = %int( %subst( KPJBU : 1 : %len(G08inv) ) );
038900151006               XSRDA8 ( WLBdat );
039000151006               if  G08err = *on;
039100151006                 V1Dmsg = sk_Msg(03);
039200151006                 $PosCurKE1   = *on;
039300151006                 $ErrMessage  = *on;
039400151006                 $ErrGenerico = *on;
039500151006                 leavesr;
039600151006               else;
039700151006                 V1Cke1 = %editc( G08dat : 'X' );
039800151006                 W1Cke1 = G08inv;
039900151006               endif;
040000151001             endif;
040100151001             $PosCurKE1   = *on;
040200151001             $ErrGenerico = *on;
040300150930             leavesr;
040400150930
040500150930           // -?Controllo immissione?
040600151001           When  V1Cke1 = *blank  or  %trim( V1Cke1 ) = *zero;
040700150930             V1Dmsg = sk_Msg(02);
040800151001             $PosCurKE1   = *on;
040900151001             $ErrMessage  = *on;
041000151001             $ErrGenerico = *on;
041100150930             leavesr;
041200151006
041300150930           // -?Controllo immissione solo caratteri numerici?
041400151006           When  %check( c_Digits : %trim( V1Cke1 ) ) > *zero;
041500150930             V1Dmsg = sk_Msg(03);
041600151001             $PosCurKE1   = *on;
041700151001             $ErrMessage  = *on;
041800151001             $ErrGenerico = *on;
041900150930             leavesr;
042000151006
042100151006           // -?Controllo correttezza formale data inserita?
042200151006           Other;
042300151006             clear  WLBdat;
042400151006             G08dat = %int( %trim( V1Cke1 ) );
042500151006             XSRDA8 ( WLBdat );
042600151006             if  G08err = *on;
042700151006               V1Dmsg = sk_Msg(03);
042800151006               $PosCurKE1   = *on;
042900151006               $ErrMessage  = *on;
043000151006               $ErrGenerico = *on;
043100151006               leavesr;
043200151006             else;
043300151006               W1Cke1 = G08inv;
043400151006             endif;
043500150930
043600150930         EndSl;
043700150930
043800150930       ENDSR;
043900150930
044000150930       //--------------------------------------------------------------
044100150930       //?Gestione videata D02.                                        ?
044200150930       //--------------------------------------------------------------
044300150930       BEGSR  sr_GesD02;
044400150930
044500150930         // -?Inizializzazione videata?
044600150930         if  $InzD02 = *on;
044700150930           exsr  sr_InzD02;
044800150930           $InzD02 = *off;
044900150930         endif;
045000150930
045100150930         // -?Emissione Testata e Piede con tasti funzionali abilitati?
045200151001         if  NOT $ErrMessage;
045300151001           write  tbVPDt01;
045400151006           //write  tbVPDd01;
045500150930         endif;
045600151001         write  tbVPDp01;
045700150930
045800150930         // -?Emissione videata?
045900150930         if  Not $Protect;
046000150930           write  Protect;
046100151001           exfmt  tbVPDd02;
046200150930         else;
046300151001           write  tbVPDd02;
046400150930           exfmt  Protect;
046500150930         endif;
046600150930
046700150930         clear  V1Dmsg;
046800150930         reset  $ErrMessage;
046900150930         reset  $ErrGenerico;
047000150930
047100150930         SELECT;
047200150930
047300150930           // -?F3=Fine?
047400150930           WHEN  dsp_aid = c_F03;
047500150930             unlock  TNTBE01L;
047600150930             $Fine = *on;
047700150930
047800150930           // -?F12=Ritorno?
047900150930           WHEN  dsp_aid = c_F12;
048000150930             unlock  TNTBE01L;
048100150930             reset  $Video;
048200150930             clear  V1Topz;
048300150930
048400150930           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
048500150930           OTHER;
048600150930             $Ripristino   = (dsp_aid = c_F05);
048700150930             $Annullamento = (dsp_aid = c_F16);
048800150930             // -?Controlli eseguiti NON se F16=Annullamento?
048900150930             if  Not $Annullamento;
049000150930               exsr  sr_CtrD02;
049100150930             // -?Controlli eseguiti SOLO se F16=Annullamento?
049200150930             //else;
049300150930             //  exsr  sr_CtrANN;
049400150930             endif;
049500150930             if  $ErrGenerico;
049600150930               leavesr;
049700150930             endif;
049800150930             // -?Aggiornamento?
049900150930             if  dsp_aid = c_F05   or
050000150930                 dsp_aid = c_F06   or
050100150930                 dsp_aid = c_F16;
050200150930               $Video = 'W1';
050300150930               reset  $InzW01;
050400150930               exsr  sr_GesW01;
050500150930             endif;
050600150930
050700150930         ENDSL;
050800150930
050900150930       ENDSR;
051000150930
051100150930       //--------------------------------------------------------------
051200150930       //?Inizializzazione videata D02.                                ?
051300150930       //--------------------------------------------------------------
051400150930       BEGSR  sr_InzD02;
051500150930
051600150930         reset  $Immissione;
051700150930         reset  $Ripristino;
051800150930         reset  $Protect;
051900150930
052000150930         // -?Spegnimento degli indicatori di errore?
052100151001         %subst( IndDspF : 50 ) = *off;
052200150930
052300150930         // -?Pulizia videata?
052400151001         clear  tbVPDd02;
052500150930
052600150930         // -?Verifica esistenza del record in TNTBE?
052700150930         //  ?e Reperimento dati?
052800151006         k_TBEke1 = %editc( W1Cke1 : 'X' );
052900150930         chain  %kds( keyTNTBE01 )  TNTBE000;
053000150930
053100150930         // -?Impostazione testata?
053200150930         Select;
053300150930           When  Not %found(TNTBE01L);
053400150930             V1Topz = '  INSERIMENTO  ';
053500150930             $Immissione = *on;
053600150930           When  TBEatb <> *blank;
053700150930             V1Topz = '  RIPRISTINO   ';
053800150930             $Ripristino = *on;
053900150930           Other;
054000150930             V1Topz = '   MODIFICA    ';
054100150930         EndSl;
054200150930
054300150930         // -?Caricamento dati?
054400151006         V2Cke1 = G08dat;
054500150930         clear  dVPD;
054600150930         If  %found(TNTBE01L);
054700150930
054800150930           dVPD = TBEuni;
054900150930
055000150930           V1mesi   = §VPDmesi;
055100150930           V1nSped  = §VPDnSped;
055200150930           V1colli  = §VPDcolli;
055300150930           V1pKda1  = §VPDpKda1;
055400150930           V1pKal1  = §VPDpKal1;
055500150930           V1scoDa1 = §VPDscoDa1;
055600150930           V1scoA1  = §VPDscoA1;
055700150930           V1pKda2  = §VPDpKda2;
055800150930           V1pKal2  = §VPDpKal2;
055900150930           V1scoDa2 = §VPDscoDa2;
056000150930           V1scoA2  = §VPDscoA2;
056100150930
056200150930         EndIf;
056300150930
056400150930         // -?Impostazione indicatori per abilitazione tasti funzionali?
056500150930         exsr  sr_AbilitFxxD02;
056600150930
056700150930       ENDSR;
056800150930
056900150930       //--------------------------------------------------------------
057000150930       //?Settaggio indicatori nella videata D02 per abilitazione      ?
057100150930       //?  tasti funzionali.                                          ?
057200150930       //--------------------------------------------------------------
057300150930       BEGSR  sr_AbilitFxxD02;
057400150930
057500150930         // -?F3=Fine?
057600151001         $F3Attivo  = *on;
057700150930
057800150930         // -?F5=Ripristino?
057900151001         $F5Attivo  = ( %found(TNTBE01L)  and  TBEatb <> *blank
058000151001                                          and  Not $Protect );
058100150930
058200150930         // -?F6=Conferma?
058300151001         $F6Attivo  = ( NOT $F5Attivo );
058400150930
058500150930         // -?F12=Ritorno?
058600151001         $F12Attivo = *on;
058700150930
058800150930         // -?F16=Annullamento?
058900151001         $F16Attivo = ( %found(TNTBE01L)  and  TBEatb = *blank
059000151001                                          and  Not $Protect );
059100150930
059200150930       ENDSR;
059300150930
059400150930       //--------------------------------------------------------------
059500150930       //?Controllo dati videata D02.                                  ?
059600150930       //--------------------------------------------------------------
059700150930       BEGSR  sr_CtrD02;
059800150930
059900150930         // -?Spegnimento degli indicatori di errore?
060000151001         %subst( IndDspF : 50 ) = *off;
060100150930
060200151001         // -?Nr max Mesi indietro per ricerca Statistica Peso?
060300151001         select;
060400151001           when  V1mesi   = *zero;
060500151001             $ErrGenerico = *on;
060600151001             $ErrMessage  = *on;
060700151001             $PosCurMESI  = *on;
060800151001             V1Dmsg = sk_Msg(04);
060900151001             leavesr;
061000151001           when  V1mesi   < *zero;
061100151001             $ErrGenerico = *on;
061200151001             $ErrMessage  = *on;
061300151001             $PosCurMESI  = *on;
061400151001             V1Dmsg = sk_Msg(05);
061500151001             leavesr;
061600151001         endsl;
061700150930
061800151001         // -?Nr min Spedizioni analizzate in Statistica Pesi?
061900151001         select;
062000151001           when  V1nSped  = *zero;
062100151001             $ErrGenerico = *on;
062200151001             $ErrMessage  = *on;
062300151001             $PosCurNSPED = *on;
062400151001             V1Dmsg = sk_Msg(04);
062500151001             leavesr;
062600151001           when  V1nSped  < *zero;
062700151001             $ErrGenerico = *on;
062800151001             $ErrMessage  = *on;
062900151001             $PosCurNSPED = *on;
063000151001             V1Dmsg = sk_Msg(05);
063100151001             leavesr;
063200151001         endsl;
063300151001
063400151001         // -?Percentuale Colli VdL rilevati in Statistica Pesi?
063500151001         select;
063600151001           when  V1colli  = *zero;
063700151001             $ErrGenerico = *on;
063800151001             $ErrMessage  = *on;
063900151001             $PosCurCOLLI = *on;
064000151001             V1Dmsg = sk_Msg(04);
064100151001             leavesr;
064200151001           when  V1colli  < *zero;
064300151001             $ErrGenerico = *on;
064400151001             $ErrMessage  = *on;
064500151001             $PosCurCOLLI = *on;
064600151001             V1Dmsg = sk_Msg(05);
064700151001             leavesr;
064800151001         endsl;
064900151001
065000151001         // -?1° scaglione di Peso x verifica % Scostamento?
065100151001
065200151001         // -?Peso KG (1° scaglione di Peso)?
065300151001         select;
065400151001           when  V1pKda1  = *zero;
065500151001             $ErrGenerico = *on;
065600151001             $ErrMessage  = *on;
065700151001             $PosCurPKDA1 = *on;
065800151001             V1Dmsg = sk_Msg(04);
065900151001             leavesr;
066000151001           when  V1pKda1  < *zero;
066100151001             $ErrGenerico = *on;
066200151001             $ErrMessage  = *on;
066300151001             $PosCurPKDA1 = *on;
066400151001             V1Dmsg = sk_Msg(05);
066500151001             leavesr;
066600151001           when  V1pKal1  = *zero;
066700151001             $ErrGenerico = *on;
066800151001             $ErrMessage  = *on;
066900151001             $PosCurPKAL1 = *on;
067000151001             V1Dmsg = sk_Msg(04);
067100151001             leavesr;
067200151001           when  V1pKal1  < *zero;
067300151001             $ErrGenerico = *on;
067400151001             $ErrMessage  = *on;
067500151001             $PosCurPKAL1 = *on;
067600151001             V1Dmsg = sk_Msg(05);
067700151001             leavesr;
067800151001         endsl;
067900151001
068000151001         // -?Controllo Range di Peso KG (1° scaglione di Peso)?
068100151001         if  V1pKda1 > V1pKal1;
068200151001           $ErrGenerico = *on;
068300151001           $ErrMessage  = *on;
068400151001           $PosCurPKDA1 = *on;
068500151001           V1Dmsg = sk_Msg(06);
068600151001           leavesr;
068700151001         endif;
068800151001
068900151001         // -?% Scostamento Negativo (1° scaglione di Peso )?
069000151001         V1scoDa1 = %abs( V1scoDa1 );
069100151001         V1scoA1  = %abs( V1scoA1 );
069200151008         //if  V1scoDa1  <= *zero;
069300151008         //  $ErrGenerico = *on;
069400151008         //  $ErrMessage  = *on;
069500151008         //  $PosCurSCOD1 = *on;
069600151008         //  V1Dmsg = sk_Msg(04);
069700151008         //  leavesr;
069800151008         //endif;
069900151008         //if  V1scoA1   <= *zero;
070000151008         //  $ErrGenerico = *on;
070100151008         //  $ErrMessage  = *on;
070200151008         //  $PosCurSCOA1 = *on;
070300151008         //  V1Dmsg = sk_Msg(04);
070400151008         //  leavesr;
070500151008         //endif;
070600151001
070700151001         // -?Controllo Range della % Scostamento (1° scaglione di Peso)?
070800151001         if  V1scoDa1 < V1scoA1;
070900151001           $ErrGenerico = *on;
071000151001           $ErrMessage  = *on;
071100151001           $PosCurSCOD1 = *on;
071200151001           V1Dmsg = sk_Msg(06);
071300151001           leavesr;
071400151001         endif;
071500151001
071600151001         // -?2° scaglione di Peso x verifica % Scostamento?
071700151001
071800151001         // -?Peso KG (2° scaglione di Peso)?
071900151001         select;
072000151001           //when  V1pKda2  = *zero;
072100151001           //  $ErrGenerico = *on;
072200151001           //  $ErrMessage  = *on;
072300151001           //  $PosCurPKDA2 = *on;
072400151001           //  V1Dmsg = sk_Msg(04);
072500151001           //  leavesr;
072600151001           when  V1pKda2 <= V1pKal1;
072700151001             $ErrGenerico = *on;
072800151001             $ErrMessage  = *on;
072900151001             $PosCurPKDA2 = *on;
073000151001             V1Dmsg = sk_Msg(07);
073100151001             leavesr;
073200151001           when  V1pKda2  < *zero;
073300151001             $ErrGenerico = *on;
073400151001             $ErrMessage  = *on;
073500151001             $PosCurPKDA2 = *on;
073600151001             V1Dmsg = sk_Msg(05);
073700151001             leavesr;
073800151001           when  V1pKal2  = *zero;
073900151001             $ErrGenerico = *on;
074000151001             $ErrMessage  = *on;
074100151001             $PosCurPKAL2 = *on;
074200151001             V1Dmsg = sk_Msg(04);
074300151001             leavesr;
074400151001           when  V1pKal2  < *zero;
074500151001             $ErrGenerico = *on;
074600151001             $ErrMessage  = *on;
074700151001             $PosCurPKAL2 = *on;
074800151001             V1Dmsg = sk_Msg(05);
074900151001             leavesr;
075000151001         endsl;
075100151001
075200151001         // -?Controllo Range di Peso KG (2° scaglione di Peso)?
075300151001         if  V1pKda2 > V1pKal2;
075400151001           $ErrGenerico = *on;
075500151001           $ErrMessage  = *on;
075600151001           $PosCurPKDA2 = *on;
075700151001           V1Dmsg = sk_Msg(06);
075800151001           leavesr;
075900151001         endif;
076000151001
076100151001         // -?% Scostamento Negativo (2° scaglione di Peso)?
076200151001         V1scoDa2 = %abs( V1scoDa2 );
076300151001         V1scoA2  = %abs( V1scoA2 );
076400151008         //if  V1scoDa2 <= *zero;
076500151008         //  $ErrGenerico = *on;
076600151008         //  $ErrMessage  = *on;
076700151008         //  $PosCurSCOD2 = *on;
076800151008         //  V1Dmsg = sk_Msg(04);
076900151008         //  leavesr;
077000151008         //endif;
077100151008         //if  V1scoA2  <= *zero;
077200151008         //  $ErrGenerico = *on;
077300151008         //  $ErrMessage  = *on;
077400151008         //  $PosCurSCOA2 = *on;
077500151008         //  V1Dmsg = sk_Msg(04);
077600151008         //  leavesr;
077700151008         //endif;
077800151001
077900151001         // -?Controllo Range della % Scostamento (2° scaglione di Peso)?
078000151001         if  V1scoDa2 < V1scoA2;
078100151001           $ErrGenerico = *on;
078200151001           $ErrMessage  = *on;
078300151001           $PosCurSCOD2 = *on;
078400151001           V1Dmsg = sk_Msg(06);
078500151001           leavesr;
078600151001         endif;
078700150930
078800150930       ENDSR;
078900150930
079000150930       //--------------------------------------------------------------
079100150930       //?Gestione trasmissione aggiornamento                          ?
079200150930       //--------------------------------------------------------------
079300150930       BEGSR  sr_GesW01;
079400150930
079500150930         // -?Inizializzazione videata?
079600150930         if $InzW01 = *on;
079700150930           exsr  sr_InzW01;
079800150930           $InzW01 = *off;
079900150930         endif;
080000150930
080100150930         // -?Emissione window?
080200150930         if  TBXftt = 'S';
080300151001           exfmt  tbVPDw01;
080400150930           $ErrMessage  = *off;
080500150930           $ErrGenerico = *off;
080600150930           clear  V1Dmsg;
080700150930         else;
080800150930           dsp_aid = c_F06;
080900150930         endif;
081000150930
081100150930         select;
081200150930
081300150930           // -?F12=Ritorno (gestito solo se emesso W01)?
081400150930           when  dsp_aid = c_F12;
081500150930             $Video = 'D2';
081600150930
081700150930           // -?F6=Conferma?
081800150930           when  dsp_aid = c_F06;
081900150930             exsr  sr_UpdTNTBE;
082000150930             // -?Ritorno alla videata iniziale?
082100150930             if  NOT  $ErrGenerico;
082200151001               $Video  = 'D1';
082300151001               $InzD01 = *on;
082400150930             endif;
082500150930
082600150930         endsl;
082700150930
082800150930       ENDSR;
082900150930
083000150930       //--------------------------------------------------------------
083100150930       //?Inizializzazione videata W01                                 ?
083200150930       //--------------------------------------------------------------
083300150930       BEGSR  sr_InzW01;
083400150930
083500151001         clear tbVPDw01;
083600150930
083700150930         if  $Immissione;
083800150930
083900150930           // -?Se immissione?
084000150930           W1ftt  = TBXftt;
084100150930
084200150930         else;
084300150930
084400150930           // -?Se NON immissione:?
084500150930           //  ?visualizza i dati relativi all'ultima trasmissione?
084600150930           W1ftt  = TBEftt;
084700150930           W1flt  = TBEflt;
084800150930           W1ftr  = TBEftr;
084900150930           if TBEdtr <> *zero;
085000151008             wDate_EUR = %date( TBEdtr : *iso );
085100151008             W1dtr     = %dec( wDate_EUR );
085200150930           endif;
085300150930
085400150930         endif;
085500150930
085600150930       ENDSR;
085700150930
085800150930       //--------------------------------------------------------------
085900150930       //?Aggiornamento records TNTBE00F (tab. "VPD")                  ?
086000150930       //--------------------------------------------------------------
086100150930       BEGSR  sr_UpdTNTBE;
086200150930
086300150930         // -?Aggancio record di TNTBE00F?
086400150930         clear  keyTNTBE01;
086500150930         k_TBEcod  = c_Tab;
086600151006         k_TBEke1  = %editc( W1Cke1 : 'X' );
086700150930         chain  %kds( keyTNTBE01 )  TNTBE000;
086800150930
086900150930
087000150930         clear  dVPD;
087100150930         §VPDmesi   = V1mesi;
087200150930         §VPDnSped  = V1nSped;
087300150930         §VPDcolli  = V1colli;
087400150930         §VPDpKda1  = V1pKda1;
087500150930         §VPDpKal1  = V1pKal1;
087600150930         §VPDscoDa1 = V1scoDa1;
087700150930         §VPDscoA1  = V1scoA1;
087800150930         §VPDpKda2  = V1pKda2;
087900150930         §VPDpKal2  = V1pKal2;
088000150930         §VPDscoDa2 = V1scoDa2;
088100150930         §VPDscoA2  = V1scoA2;
088200151006
088300151006         if  NOT %found(TNTBE01L);
088400151006           clear  TNTBE000;
088500151006           TBEcod  = k_TBEcod;
088600151006           TBEke1  = k_TBEke1;
088700151006           TBEke2  = k_TBEke2;
088800151006           TBElin  = k_TBElin;
088900151006           TBEsif  = k_TBEsif;
089000151006         endif;
089100150930
089200150930         TBEuni = dVPD;
089300150930
089400150930         TBEapl = TBXapl;
089500150930         TBEftt = W1ftt;
089600150930         TBEflt = W1flt;
089700150930         clear  TBEftr;
089800150930         //clear  TBEdtr;
089900150930
090000150930         // -?Aggiornamento tab. "VPD"?
090100150930         SELECT;
090200150930
090300150930           // -?F5=Ripristino?
090400150930           WHEN  $Ripristino;
090500150930             select;
090600150930               when  %found(TNTBE01L)  and  TBEatb <> *blank;
090700150930                 clear  TBEatb;
090800150930                 //______________
090900150930                 UPDATE  TNTBE000;
091000150930                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
091100150930               // -?Record appena cancellato fisicamente?
091200150930               when  NOT %found(TNTBE01L);
091300150930                 //______________
091400150930                 WRITE   TNTBE000;
091500150930                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
091600150930             endsl;
091700150930
091800150930           // -?F16=Annullamento?
091900150930           WHEN  $Annullamento;
092000150930             if  %found(TNTBE01L)  and  TBEatb <> 'A';
092100150930               TBEatb = 'A';
092200150930               //______________
092300150930               UPDATE  TNTBE000;
092400150930               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
092500150930             endif;
092600150930
092700150930           // -?Immissione o Modifica?
092800150930           OTHER;
092900150930             if  NOT %found(TNTBE01L);
093000150930               //_____________
093100150930               WRITE  TNTBE000;
093200150930               //¯¯¯¯¯¯¯¯¯¯¯¯¯
093300150930             else;
093400150930               clear  TBEatb;
093500150930               //_____________
093600150930               UPDATE TNTBE000;
093700150930               //¯¯¯¯¯¯¯¯¯¯¯¯¯
093800150930             endif;
093900150930
094000150930         ENDSL;
094100150930
094200150930       ENDSR;
094300150930
094400150930       //--------------------------------------------------------------
094500150930       //?Operazioni finali.                                           ?
094600150930       //--------------------------------------------------------------
094700150930       BEGSR  sr_RoutEnd;
094800150930
094900150930         //  ?Uscita?
095000150930         return;
095100150930
095200150930       ENDSR;
095300150930
095400150930      /end-free
095500150930
095600150930       //--------------------------------------------------------------
095700150930       //?Definizione schiere a tempo di compilazione.                 ?
095800150930       //--------------------------------------------------------------
095900150930
096000150930** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
096100150930Tabella gestibile SOLO da SEDE                                                  1
096200150930Data obbligatoria                                                               2
096300150930Data formalmente errata                                                         3
096400151001Campo obbligatorio                                                              4
096500151001Valori negativi NON ammessi                                                     5
096600151001Valore iniziale maggiore di quello finale                                       6
096700151001Peso in KG iniziale 2° scaglione  NON superiore  a quello finale 1° scaglione   7
