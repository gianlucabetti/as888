000100150219       //==============================================================
000200150219       //?Gestione tabella "VMA" (V.M.A. Negato)                       ?
000300150219       //==============================================================
000400150219
000500150219       //--------------------------------------------------------------
000600150219       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700150219       //--------------------------------------------------------------
000800150219
000900150219     /*PRM  dbgview(*source)
001000150219     /*END
001100150219
001200150219       //--------------------------------------------------------------
001300150219       //?Specifiche di controllo.                                     ?
001400150219       //--------------------------------------------------------------
001500150219
001600150219     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700150219     h dftactgrp(*no)
001800150224     h bnddir('UBRTVNETA')
001900150219
002000150219       //--------------------------------------------------------------
002100150219       //?Dichiarazione file.                                          ?
002200150219       //--------------------------------------------------------------
002300150219
002400150219       // -?Tabelle?
002500150224     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
002600150224     f                                     usropn
002700150219
002800150219       // -?Video?
002900150219     fTNTBVMAD  cf   e             workstn
003000150219     f                                     indds(IndDspF)
003100150219     f                                     infds(InfDspF)
003200150219
003300150219       //--------------------------------------------------------------
003400150219       //?Definizione costanti.                                        ?
003500150219       //--------------------------------------------------------------
003600150219
003700150219       // -?Codice tabella in gestione?
003800150219     d c_Tab           c                   const('VMA')
003900150224
004000150224       // -?Costanti per la definizione delle schiere con i nomi?
004100150224       //  ?degli iSeries da elaborare e delle relative librerie?
004200150224     d c_NrSyst        c                   const(2)
004300150224     d c_NrLibr        c                   const(2)
004400150219
004500150219       // -?Tasti funzionali a video?
004600150219     d c_F01           c                   const(x'31')
004700150219     d c_F02           c                   const(x'32')
004800150219     d c_F03           c                   const(x'33')
004900150219     d c_F04           c                   const(x'34')
005000150219     d c_F05           c                   const(x'35')
005100150219     d c_F06           c                   const(x'36')
005200150219     d c_F07           c                   const(x'37')
005300150219     d c_F08           c                   const(x'38')
005400150219     d c_F09           c                   const(x'39')
005500150219     d c_F10           c                   const(x'3A')
005600150219     d c_F11           c                   const(x'3B')
005700150219     d c_F12           c                   const(x'3C')
005800150219     d c_F13           c                   const(x'B1')
005900150219     d c_F14           c                   const(x'B2')
006000150219     d c_F15           c                   const(x'B3')
006100150219     d c_F16           c                   const(x'B4')
006200150219     d c_F17           c                   const(x'B5')
006300150219     d c_F18           c                   const(x'B6')
006400150219     d c_F19           c                   const(x'B7')
006500150219     d c_F20           c                   const(x'B8')
006600150219     d c_F21           c                   const(x'B9')
006700150219     d c_F22           c                   const(x'BA')
006800150219     d c_F23           c                   const(x'BB')
006900150219     d c_F24           c                   const(x'BC')
007000150219     d c_Enter         c                   const(x'F1')
007100150219     d c_RollDown      c                   const(x'F4')
007200150219     d c_RollUp        c                   const(x'F5')
007300150219
007400150219       // -?Costante per controllo "caratteri solo numerici"?
007500150219     d c_Digits        c                   const('0123456789')
007600150219
007700150219       //--------------------------------------------------------------
007800150219       //?Definizione schiere.                                         ?
007900150219       //--------------------------------------------------------------
008000150224
008100150224       // -?iSeries  &  Librerie con entrambi i file tabelle?
008200150224     d sk_iSystem      s                   like(currSysNetA)
008300150224     d                                     dim(c_NrSyst)
008400150224     d                                     ctdata   perrcd( 1)
008500150224     d sk_Libraries    s                   like(ds_Libr)
008600150224     d                                     dim(c_NrSyst)
008700150224     d                                     alt(sk_iSystem)
008800150219
008900150219       // -?Messaggi di errore?
009000150219     d sk_Msg          s             78    dim( 7)  ctdata  perrcd( 1)
009100150219
009200150219       //--------------------------------------------------------------
009300150219       //?Definizione aree dati.                                       ?
009400150219       //--------------------------------------------------------------
009500150219
009600150219       // -?Dati utente?
009700150219     d §AzUte        e ds                  extname(AZUTE00F)
009800150219     d                                     dtaara
009900150219     d §DatiUte      e ds                  extname(dDatiUte)
010000150219     d                                     dtaara
010100150219
010200150219       //--------------------------------------------------------------
010300150219       //?Definizione strutture dati.                                  ?
010400150219       //--------------------------------------------------------------
010500150219
010600150219       // -?Status ds?
010700150219     d Status         sds
010800150219     d   SDSpgm          *proc
010900150219
011000150219       // -?InfDS?
011100150219     d InfDspF         ds
011200150219     d   dsp_aid             369    369a
011300150219
011400150219       // -?Indicatori su DspF?
011500150219     d IndDspF         ds                  inz
011600150219         // -?Abilitazione tasti funzionali?
011700150219     d   F3Attivo                      n   overlay(IndDspF : 03)
011800150219     d   F5Attivo                      n   overlay(IndDspF : 05)
011900150219     d   F6Attivo                      n   overlay(IndDspF : 06)
012000150219     d   F12Attivo                     n   overlay(IndDspF : 12)
012100150219     d   F16Attivo                     n   overlay(IndDspF : 16)
012200150219         // -?Emissione messaggio di errore?
012300150219     d   ErrMessage                    n   overlay(IndDspF : 28)
012400150219         // -?Posizionamento cursore & segnalazione errore?
012500150219     d   PosCurKSC                     n   overlay(IndDspF : 50)
012600150219     d   PosCurDDT                     n   overlay(IndDspF : 51)
012700150219     d   PosCurDST                     n   overlay(IndDspF : 52)
012800150219         // -?Emissione messaggio di errore in window W1?
012900150219     d   ErrMessageW1                  n   overlay(IndDspF : 90)
013000150219         //   -?Riemissione videata?
013100150219     d   ErrGenerico                   n   overlay(IndDspF : 99)
013200150224
013300150224       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
013400150224     d ds_Libr         ds                  inz
013500150224     d   sk_Libr                     10    dim(c_NrLibr) inz
013600150219
013700150219       // -?Parametri ricevuti?
013800150219     d KPJBA         e ds
013900150219
014000150219       // -?Dati record principale della tabella?
014100150219     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014200150219     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014300150219     d                                          prefix(TBX:3)
014400150219
014500150219       // -?Tabelle usate:?
014600150219       // ·?"VMA" = Dati V.M.A. Negato?
014700150219     d dVMA          e ds                  inz
014800150219
014900150219       //--------------------------------------------------------------
015000150219       //?Definizione variabili globali.                               ?
015100150219       //--------------------------------------------------------------
015200150219
015300150219       // -?Flags booleani?
015400150219     d $Fine           s               n   inz
015500150219     d $InzD01         s               n   inz(*on)
015600150219     d $InzD02         s               n   inz(*on)
015700150219     d $Annullamento   s               n   inz
015800150219     d $Immissione     s               n   inz
015900150219     d $Ripristino     s               n   inz
016000150219     d $Protect        s               n   inz
016100150219
016200150219       // -?Variabili per la gestione del video?
016300150219     d $Video          s              2    inz('D1')
016400150224
016500150224       // -?Nome esteso Libreria/File del file tabelle?
016600150224     d wLibFile        s             21a   inz
016700150219
016800150219       // -?Campi di comodo?
016900150224     d w_iSystem       s              1  0 inz
017000150224     d w_SisInf        s              3  0 inz
017100150224     d wDate           s              8  0 inz
017200150224     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
017300150219
017400150219       //--------------------------------------------------------------
017500150219       //?Definizione prototipi procedure.                             ?
017600150219       //--------------------------------------------------------------
017700150224
017800150224       // -?Reperimento NETA sistema AS/400 corrente?
017900150224     d currSysNeta     s              8a   inz
018000150224      /copy gaitrasrc/srcProtoPR,UBRTVNETA
018100150219
018200150219       // -?Reperimento dati utente?
018300150219     d TIBS34ds      e ds                  inz
018400150219      /copy gaitrasrc/srcProtoPR,TIBS34R
018500150219
018600150219       // -?Ricerca e controllo nuove tabelle?
018700150219     d TIBS02ds      e ds                  inz
018800150219     d   T02mod      e                     inz('R')
018900150219     d   T02cod      e                     inz('VMA')
019000150219      /copy gaitrasrc/srcProtoPR,TIBS02R
019100150219
019200150219       // -?Controllo/Decodifica cliente?
019300150219      /copy gaitrasrc/srcProtoPI,TIBS69R
019400150219      /copy gaitrasrc/srcProtoPR,TIBS69R
019500150219
019600150219      *// // -?Interrogazione sottoconti?
019700150219      *///copy gaitrasrc/srcProtoPR,XALFA3BRDS
019800150219      *///copy gaitrasrc/srcProtoPR,XALFA3BR
019900150219
020000150219       // -?Controllo/Inversione data?
020100150219     d WLBdat          ds                  inz
020200150219     d   G08dat                       8  0 inz
020300150219     d   G08inv                       8  0 inz
020400150219     d   G08err                       1    inz('3')
020500150219     d   G08tgi                       5  0 inz
020600150219      /copy gaitrasrc/srcProtoPR,XSRDA8
020700150219
020800150219       //--------------------------------------------------------------
020900150219       //?Definizione key-list.                                        ?
021000150219       //--------------------------------------------------------------
021100150219
021200150219       // -?File TNTBE01L?
021300150219     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
021400150219     d                                     prefix( k_ )   inz
021500150219
021600150219       //--------------------------------------------------------------
021700150219       //?M A I N - L I N E                                            ?
021800150219       //--------------------------------------------------------------
021900150219
022000150219     c     *Entry        plist
022100150219     c                   parm                    KPJBA
022200150219
022300150219      /free
022400150219
022500150219       // -?Operazioni iniziali?
022600150219       exsr sr_RoutInz;
022700150219
022800150219       // -?Ciclo di gestione del file video?
022900150219       DoW  $Fine = *off;
023000150219
023100150224         Select;
023200150219
023300150219           // -?Gestione videata D1: richiesta chiave di accesso?
023400150224           When $Video = 'D1';
023500150219             exsr sr_GesD01;
023600150219
023700150219           // -?Gestione videata D2: manutenzione dati della singola tabella?
023800150224           When $Video = 'D2';
023900150219             exsr sr_GesD02;
024000150219
024100150219           // -?? ? ? ? ??
024200150224           Other;
024300150219             $Fine = *on;
024400150219
024500150224         EndSl;
024600150219
024700150224       EndDo;
024800150219
024900150219       // -?Operazioni finali?
025000150219       exsr sr_RoutEnd;
025100150219
025200150219       //--------------------------------------------------------------
025300150219       //?Operazioni iniziali.                                         ?
025400150219       //--------------------------------------------------------------
025500150219       BEGSR  sr_RoutInz;
025600150219
025700150219         *inLR = *on;
025800150219
025900150219         // -?Reperimento dati job?
026000150219         exsr  sr_DatiJob;
026100150224
026200150224         // -?Reperimento data odierna?
026300150224         wDate = %int( %subst( %char( %dec( %timestamp() ) )
026400150224                               : 1 : 8 ) );
026500150219
026600150219         // -?Impostazione nome programma a video?
026700150219         V1Tpgm = SDSpgm;
026800150224
026900150224         // -?Verifica del sistema AS/400 corrente?
027000150224         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
027100150224           $Fine = *on;
027200150224           leavesr;
027300150224         endif;
027400150224
027500150224         // -?Impostazione elenco librerie in cui gestire le tabelle?
027600150224         //  ?(a seconda del sistema in cui si stà lavorando)?
027700150224         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
027800150224         if  w_iSystem = *zero;
027900150224           $Fine = *on;
028000150224           leavesr;
028100150224         endif;
028200150224
028300150224         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
028400150224         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
028500150224           w_SisInf = 1;
028600150224           exsr  sr_OpenFileTab;
028700150224         endif;
028800150219
028900150219         // -?Aggancio dati generali della tabella in esame?
029000150219         clear  keyTNTBE01;
029100150219         k_TBEke1 = *zero;
029200150219         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
029300150219                = c_Tab;
029400150219         k_TBEsif = KNSIF;
029500150219         chain(n)  %kds(keyTNTBE01)  TNTBE000;
029600150219         if  not  %found(TNTBE01L);
029700150219           clear  k_TBEsif;
029800150219           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
029900150219         endif;
030000150219         if  %found(TNTBE01L);
030100150219           xTNTBEds = TNTBEds;
030200150219         else;
030300150219           clear  xTNTBEds;
030400150219         endif;
030500150219
030600150219         // -?Impostazione iniziale / pulizia dei campi chiave?
030700150219         clear  keyTNTBE01;
030800150219         k_TBEcod = c_Tab;
030900150219
031000150219       ENDSR;
031100150219
031200150219       //--------------------------------------------------------------
031300150219       //?Reperimento Dati del job (Utente/Operativi).                 ?
031400150219       //--------------------------------------------------------------
031500150219       BEGSR  sr_DatiJob;
031600150219
031700150219         in(E)  §AzUte;
031800150219         if  NOT %error;
031900150219           in(E)  §DatiUte;
032000150219         endif;
032100150219         if  %error  or  RSut = *blanks;
032200150219           clear  TIBS34ds;
032300150219           tibs34r ( tibs34ds );
032400150219           in  §AzUte;
032500150219           in  §DatiUte;
032600150219         endif;
032700150219
032800150219       ENDSR;
032900150219
033000150219       //--------------------------------------------------------------
033100150219       //?Gestione videata D01.                                        ?
033200150219       //--------------------------------------------------------------
033300150219       BEGSR  sr_GesD01;
033400150219
033500150219         // -?Inizializzazione videata?
033600150219         if  $InzD01 = *on;
033700150219           exsr  sr_InzD01;
033800150219           $InzD01 = *off;
033900150219         endif;
034000150219         clear  V1Topz;
034100150320
034200150320         // -?(Dis)Abilitazione Tasti Funzionali?
034300150320         // -?F3=Fine?
034400150320         F3Attivo  = *on;
034500150320         // -?F5=Ripristino?
034600150320         F5Attivo  = *off;
034700150320         // -?F6=Conferma?
034800150320         F6Attivo  = *off;
034900150320         // -?F12=Ritorno?
035000150320         F12Attivo = *off;
035100150320         // -?F16=Annullamento?
035200150320         F16Attivo = *off;
035300150219
035400150219         // -?Emissione videata?
035500150219         write  TBVMAT01;
035600150219         write  TBVMAP01;
035700150219         exfmt  TBVMAD01;
035800150219
035900150219         clear  V1Dmsg;
036000150219         reset  ErrMessage;
036100150219         reset  ErrGenerico;
036200150219
036300150219         SELECT;
036400150219
036500150219           // -?Utente NON di Sede (errore già visualizzato)?
036600150219           WHEN  DUTlpo <> 'S';
036700150219             $Fine = *on;
036800150219
036900150219           // -?F3=Fine?
037000150219           WHEN  dsp_aid = c_F03;
037100150219             $Fine = *on;
037200150219
037300150219           // -?Invio?
037400150219           OTHER;
037500150219             exsr  sr_CtrD01;
037600150219             if  ErrGenerico = *off;
037700150219               $Video    = 'D2';
037800150219               $InzD02   = *on;
037900150219             endif;
038000150219
038100150219         ENDSL;
038200150219
038300150219       ENDSR;
038400150219
038500150219       //--------------------------------------------------------------
038600150219       //?Inizializzazione videata D01.                                ?
038700150219       //--------------------------------------------------------------
038800150219       BEGSR  sr_InzD01;
038900150219
039000150219         clear  TBVMAD01;
039100150219
039200150219         V1Cksc = '?';
039300150219
039400150320         // -?Eseguibile SOLO da Sede?
039500150219         if  DUTlpo <> 'S';
039600150219           V1Dmsg = sk_Msg(01);
039700150219           ErrMessage  = *on;
039800150219           ErrGenerico = *on;
039900150219           leavesr;
040000150219         endif;
040100150219
040200150219       ENDSR;
040300150219
040400150219       //--------------------------------------------------------------
040500150219       //?Controllo dati immessi nella videata D01.                    ?
040600150219       //--------------------------------------------------------------
040700150219       BEGSR  sr_CtrD01;
040800150219
040900150219         // -?Spegnimento indicatori di posizionamento cursore?
041000150219         %subst(IndDspF : 28) = *off;
041100150219
041200150219         // -?Codice Cliente?
041300150219         clear  V1Dksc;
041400150219         Select;
041500150219
041600150219           // -?Ricerca?
041700150219           When  %scan( '?' : V1Cksc ) > *zero;
041800150219             clear  V1Cksc;
041900150219             reset  TIBS02ds;
042000150219             T02sif = knsif;
042100150219             TNTBE_RicercaControllo (kpjba : TIBS02ds);
042200150219             if  T02err = *blank;
042300150219               dVMA   = T02uni;
042400150219               V1Cksc = T02ke1;
042500150219             endif;
042600150219             ErrGenerico = *on;
042700150219             leavesr;
042800150219
042900150219           // -?Controllo immissione?
043000150219           When  V1Cksc = *blank  or  V1Cksc = *zero;
043100150219             V1Dmsg = sk_Msg(02);
043200150219             PosCurKSC   = *on;
043300150219             ErrMessage  = *on;
043400150219             ErrGenerico = *on;
043500150219             leavesr;
043600150219
043700150219           // -?Controllo immissione solo caratteri numerici?
043800150219           When  %check( c_Digits : V1Cksc ) > *zero;
043900150219             V1Dmsg = sk_Msg(03);
044000150219             PosCurKSC   = *on;
044100150219             ErrMessage  = *on;
044200150219             ErrGenerico = *on;
044300150219             leavesr;
044400150219
044500150219         EndSl;
044600150219
044700150219         // -?Controllo / decodifica cliente?
044800150219         clear  TIBS69ds;
044900150219         I69sif = knsif;
045000150219         I69kcc = DUTkci;
045100150219         I69kac = %int( V1Cksc );
045200150219         tibs69r( TIBS69ds :
045300150219                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
045400150219         if  O69err = *on;
045500150219           V1Dmsg = sk_Msg(04);
045600150219           PosCurKSC   = *on;
045700150219           ErrMessage  = *on;
045800150219           ErrGenerico = *on;
045900150219           leavesr;
046000150219         endif;
046100150219         V1Dksc = ACOrag;
046200150219
046300150219       ENDSR;
046400150219
046500150219       //--------------------------------------------------------------
046600150219       //?Gestione videata D02.                                        ?
046700150219       //--------------------------------------------------------------
046800150219       BEGSR  sr_GesD02;
046900150219
047000150219         // -?Inizializzazione videata?
047100150219         if  $InzD02 = *on;
047200150219           exsr  sr_InzD02;
047300150219           $InzD02 = *off;
047400150219         endif;
047500150219
047600150219         // -?Emissione Testata e Piede con tasti funzionali abilitati?
047700150219         if  NOT ErrMessage;
047800150219           write  TBVMAT01;
047900150219           write  TBVMAD01;
048000150219         endif;
048100150219         write  TBVMAP01;
048200150219
048300150219         // -?Emissione videata?
048400150219         if  Not $Protect;
048500150219           write  Protect;
048600150219           exfmt  TBVMAD02;
048700150219         else;
048800150219           write  TBVMAD02;
048900150219           exfmt  Protect;
049000150219         endif;
049100150219
049200150219         clear  V1Dmsg;
049300150219         reset  ErrMessage;
049400150219         reset  ErrGenerico;
049500150219
049600150219         SELECT;
049700150219
049800150219           // -?F3=Fine?
049900150219           WHEN  dsp_aid = c_F03;
050000150219             unlock  TNTBE01L;
050100150219             $Fine = *on;
050200150219
050300150219           // -?F12=Ritorno?
050400150219           WHEN  dsp_aid = c_F12;
050500150219             unlock  TNTBE01L;
050600150219             reset  $Video;
050700150219             clear  V1Topz;
050800150219
050900150219           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
051000150219           OTHER;
051100150219             $Ripristino   = (dsp_aid = c_F05);
051200150219             $Annullamento = (dsp_aid = c_F16);
051300150219             // -?Controlli eseguiti NON se F16=Annullamento?
051400150219             if  Not $Annullamento;
051500150219               exsr  sr_CtrD02;
051600150219             // -?Controlli eseguiti SOLO se F16=Annullamento?
051700150219             //else;
051800150219             //  exsr  sr_CtrANN;
051900150219             endif;
052000150219             if  ErrGenerico;
052100150219               leavesr;
052200150219             endif;
052300150219             // -?Aggiornamento?
052400150219             if  dsp_aid = c_F05   or
052500150219                 dsp_aid = c_F06   or
052600150219                 dsp_aid = c_F16;
052700150224               exsr  sr_UpdateAll;
052800150224               // -?Rilevato errore in fase di aggiornamento?
052900150224               if  ErrGenerico;
053000150224                 leavesr;
053100150224               endif;
053200150224               // -?Ritorno alla videata iniziale altrimenti?
053300150224               $Video  = 'D1';
053400150224               $InzD01 = *on;
053500150219             endif;
053600150219
053700150219         ENDSL;
053800150219
053900150219       ENDSR;
054000150219
054100150219       //--------------------------------------------------------------
054200150219       //?Inizializzazione videata D02.                                ?
054300150219       //--------------------------------------------------------------
054400150219       BEGSR  sr_InzD02;
054500150219
054600150219         reset  $Immissione;
054700150219         reset  $Ripristino;
054800150219         reset  $Protect;
054900150219
055000150219         // -?Spegnimento degli indicatori di errore?
055100150219         %subst(IndDspF : 50) = *off;
055200150224
055300150224         // -?Apertura file TABEL00F del 1° S.I. (sede)?
055400150224         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
055500150224           w_SisInf = 1;
055600150224           exsr  sr_OpenFileTab;
055700150224         endif;
055800150219
055900150219         // -?Pulizia videata?
056000150219         clear  TBVMAD02;
056100150219
056200150224         // -?Verifica esistenza del record in TNTBE?
056300150224         //  ?e Reperimento dati del Cliente?
056400150219         k_TBEke1 = V1Cksc;
056500150219         chain  %kds( keyTNTBE01 )  TNTBE000;
056600150219
056700150219         // -?Impostazione testata?
056800150219         Select;
056900150219           When  Not %found(TNTBE01L);
057000150219             V1Topz = '  INSERIMENTO  ';
057100150219             $Immissione = *on;
057200150219           When  TBEatb <> *blank;
057300150219             V1Topz = '  RIPRISTINO   ';
057400150219             $Ripristino = *on;
057500150219           Other;
057600150219             V1Topz = '   MODIFICA    ';
057700150219         EndSl;
057800150219
057900150219         // -?Caricamento dati del Codice?
058000150224         clear  dVMA;
058100150224         If  %found(TNTBE01L);
058200150224           dVMA = TBEuni;
058300150219           if  §VMAddt > *zero;
058400150219             wDate_EUR = %date( §VMAddt : *iso );
058500150219             V1Cddt    = %dec( wDate_EUR );
058600150219           endif;
058700150219           if  §VMAdst > *zero;
058800150219             wDate_EUR = %date( §VMAdst : *iso );
058900150219             V1Cdst    = %dec( wDate_EUR );
059000150219           endif;
059100150224         EndIf;
059200150219
059300150219         // -?Impostazione indicatori per abilitazione tasti funzionali?
059400150219         exsr  sr_AbilitFxxD02;
059500150219
059600150219       ENDSR;
059700150219
059800150219       //--------------------------------------------------------------
059900150219       //?Settaggio indicatori nella videata D02 per abilitazione      ?
060000150219       //?  tasti funzionali.                                          ?
060100150219       //--------------------------------------------------------------
060200150219       BEGSR  sr_AbilitFxxD02;
060300150219
060400150219         // -?F3=Fine?
060500150219         F3Attivo  = *on;
060600150219
060700150219         // -?F5=Ripristino?
060800150219         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
060900150219                                         and   Not $Protect);
061000150219
061100150219         // -?F6=Conferma?
061200150219         F6Attivo  = (NOT F5Attivo);
061300150219
061400150219         // -?F12=Ritorno?
061500150219         F12Attivo = *on;
061600150219
061700150219         // -?F16=Annullamento?
061800150219         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
061900150219                                         and   Not $Protect);
062000150219
062100150219       ENDSR;
062200150219
062300150219       //--------------------------------------------------------------
062400150219       //?Controllo dati videata D02.                                  ?
062500150219       //--------------------------------------------------------------
062600150219       BEGSR  sr_CtrD02;
062700150219
062800150219         // -?Spegnimento degli indicatori di errore?
062900150219         %subst(IndDspF : 50) = *off;
063000150219
063100150219         clear  dVMA;
063200150219
063300150219         // -?Data Decorrenza NON applicazione V.M.A.?
063400150219         if  V1Cddt = *zero;
063500150219           ErrGenerico = *on;
063600150219           ErrMessage  = *on;
063700150219           PosCurDDT   = *on;
063800150219           V1Dmsg = sk_Msg(05);
063900150219           leavesr;
064000150219         endif;
064100150219
064200150219         clear WLBdat;
064300150219         G08dat = V1Cddt;
064400150219         XSRDA8(WLBdat);
064500150219         if  G08err = *on;
064600150219           ErrGenerico = *on;
064700150219           ErrMessage  = *on;
064800150219           PosCurDDT   = *on;
064900150219           V1Dmsg = sk_Msg(06);
065000150219           leavesr;
065100150219         endif;
065200150219         V1Cddt  = G08dat;
065300150219         §VMAddt = G08inv;
065400150219
065500150219         // -?Data Scadenza NON applicazione V.M.A.?
065600150219         if  V1Cdst = *zero;
065700150219           ErrGenerico = *on;
065800150219           ErrMessage  = *on;
065900150219           PosCurDST   = *on;
066000150219           V1Dmsg = sk_Msg(05);
066100150219           leavesr;
066200150219         endif;
066300150219
066400150219         clear WLBdat;
066500150219         G08dat = V1Cdst;
066600150219         XSRDA8(WLBdat);
066700150219         if  G08err = *on;
066800150219           ErrGenerico = *on;
066900150219           ErrMessage  = *on;
067000150219           PosCurDST   = *on;
067100150219           V1Dmsg = sk_Msg(06);
067200150219           leavesr;
067300150219         endif;
067400150219         V1Cdst  = G08dat;
067500150219         §VMAdst = G08inv;
067600150219
067700150219         // -?Controllo Range di Date Decorrenza/Scadenza?
067800150219         if  §VMAddt > §VMAdst;
067900150219           ErrGenerico = *on;
068000150219           ErrMessage  = *on;
068100150219           PosCurDST   = *on;
068200150219           V1Dmsg = sk_Msg(07);
068300150219           leavesr;
068400150219         endif;
068500150219
068600150219       ENDSR;
068700150224
068800150224       //--------------------------------------------------------------
068900150224       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
069000150224       //--------------------------------------------------------------
069100150224       BEGSR  sr_UpdateAll;
069200150224
069300150224         // -?Ciclo di elaborazione per ogni sistema informativo?
069400150224         For  w_SisInf = 1  To  %elem(sk_Libr);
069500150224
069600150224           // -?Apertura degli archivi?
069700150224           if  w_SisInf > 1;
069800150224             exsr  sr_OpenFileTab;
069900150224           endif;
070000150224
070100150224           // -?Aggiornamento tab. "VMA"?
070200150224           exsr  sr_UpdTNTBE;
070300150224
070400150224         EndFor;
070500150224
070600150224       ENDSR;
070700150224
070800150224       //--------------------------------------------------------------
070900150224       //?Apertura dei files tabelle nel sistema informativo impostato.?
071000150224       //--------------------------------------------------------------
071100150224       BEGSR  sr_OpenFileTab;
071200150224
071300150224         // -?Chiusura (eventuale) file tabelle?
071400150224         if  %open(TNTBE01L);
071500150224           close  TNTBE01L;
071600150224         endif;
071700150224
071800150224         // -?Apertura file tabelle?
071900150224         ds_Libr  = sk_Libraries(w_iSystem);
072000150224         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
072100150224         open  TNTBE01L;
072200150224
072300150224       ENDSR;
072400150219
072500150219       //--------------------------------------------------------------
072600150219       //?Aggiornamento records TNTBE00F (tab. "VMA")                  ?
072700150219       //--------------------------------------------------------------
072800150219       BEGSR  sr_UpdTNTBE;
072900150224
073000150224         //? N.B.                                                      ?
073100150224         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
073200150224         //   trasmissione (vedi flag TBEFTR).                        ?
073300150224         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
073400150224         //   subito nello stesso file di entrambi i S.I. - in due    ?
073500150224         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
073600150224
073700150224         // -?RI-aggancio record da aggiornare?
073800150224         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
073900150219
074000150219         // -?Aggancio record di TNTBE00F?
074100150224         //if  w_SisInf > 1;
074200150224           clear  keyTNTBE01;
074300150224           k_TBEcod  = c_Tab;
074400150224           k_TBEke1  = V1Cksc;
074500150224           chain  %kds( keyTNTBE01 )  TNTBE000;
074600150224         //endif;
074700150219
074800150219         //§VMAddt = ......;       ?(già impostata)?
074900150219         //§VMAdst = ......;       ?(già impostata)?
075000150219         TBEuni = dVMA;
075100150219
075200150219         // -?Aggiornamento tab. "VMA"?
075300150224         SELECT;
075400150219
075500150219           // -?F5=Ripristino?
075600150224           WHEN  $Ripristino;
075700150224             TBEapl = TBXapl;
075800150224             //TBEftt = W1ftt;
075900150224             TBEftt = 'S';
076000150224             clear  TBEflt;
076100150224             if  w_SisInf = 1;
076200150224               TBEftr = 'T';
076300150224             else;
076400150224               TBEftr = 'R';
076500150224             endif;
076600150224             TBEdtr = wDate;
076700150224             select;
076800150224               when  %found(TNTBE01L)  and  TBEatb <> *blank;
076900150224                 clear  TBEatb;
077000150224                 //______________
077100150224                 UPDATE  TNTBE000;
077200150224                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077300150224               // -?Record appena cancellato fisicamente?
077400150224               when  NOT %found(TNTBE01L);
077500150224                 clear  TBEatb;
077600150224                 //______________
077700150224                 WRITE   TNTBE000;
077800150224                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077900150224             endsl;
078000150219
078100150219           // -?F16=Annullamento?
078200150219           WHEN  $Annullamento;
078300150224             if  %found(TNTBE01L)  and  TBEatb <> 'A';
078400150224               TBEatb = 'A';
078500150224               //TBEftt = W1ftt;
078600150224               TBEftt = 'S';
078700150224               clear  TBEftr;
078800150224               //______________
078900150224               UPDATE  TNTBE000;
079000150224               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079100150224             endif;
079200150219
079300150219           // -?Immissione o Modifica?
079400150219           OTHER;
079500150219             if  NOT %found(TNTBE01L);
079600150219               clear TNTBE000;
079700150219               TBEcod  = k_TBEcod;
079800150219               TBEke1  = k_TBEke1;
079900150219               TBEke2  = k_TBEke2;
080000150219               TBElin  = k_TBElin;
080100150219               TBEsif  = k_TBEsif;
080200150219               TBEuni  = dVMA;
080300150219             endif;
080400150224             clear  TBEatb;
080500150219             TBEapl = TBXapl;
080600150224             //TBEftt = W1ftt;
080700150224             TBEftt = 'S';
080800150219             clear  TBEflt;
080900150224             if  w_SisInf = 1;
081000150224               TBEftr = 'T';
081100150224             else;
081200150224               TBEftr = 'R';
081300150224             endif;
081400150224             TBEdtr = wDate;
081500150219             // -?Aggiornamento record?
081600150219             if  %found(TNTBE01L);
081700150219               //_____________
081800150219               UPDATE TNTBE000;
081900150219               //¯¯¯¯¯¯¯¯¯¯¯¯¯
082000150219             else;
082100150219               //_____________
082200150219               WRITE  TNTBE000;
082300150219               //¯¯¯¯¯¯¯¯¯¯¯¯¯
082400150219             endif;
082500150219
082600150219         ENDSL;
082700150219
082800150219       ENDSR;
082900150219
083000150219       //--------------------------------------------------------------
083100150219       //?Operazioni finali.                                           ?
083200150219       //--------------------------------------------------------------
083300150219       BEGSR  sr_RoutEnd;
083400150219
083500150219         // -?Chiusura funzioni precedentemente aperte?
083600150219         clear  TIBS02ds;
083700150219         T02tla = 'C';
083800150219         TNTBE_RicercaControllo ( kpjba : TIBS02ds );
083900150219
084000150219         clear  TIBS69ds;
084100150219         I69tla = 'C';
084200150219         tibs69r( tibs69ds :
084300150219                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
084400150219
084500150219         //  ?Uscita?
084600150219         return;
084700150219
084800150219       ENDSR;
084900150219
085000150219      /end-free
085100150219
085200150219       //--------------------------------------------------------------
085300150219       //?Definizione schiere a tempo di compilazione.                 ?
085400150219       //--------------------------------------------------------------
085500150219
085600150224** -?sk_iSystem / sk_Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
085700150224SETRAS  GAITRAGRU FILTRAGRU
085800150224AS888   GAITRAGRPSFILTRAGRPF
085900150219** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
086000150219Tabella gestibile SOLO da SEDE                                                  1
086100150219Codice cliente obbligatorio                                                     2
086200150219Ammessi SOLO caratteri numerici                                                 3
086300150219Cliente NON presente in anagrafica                                              4
086400150219Data obbligatoria                                                               5
086500150219Data formalmente errata                                                         6
086600150219Data iniziale successiva a quella finale                                        7
