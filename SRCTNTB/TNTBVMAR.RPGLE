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
018700150327     d tntbVMAds1    e ds                  inz
018800150327     d tntbVMAr1       pr                  extpgm('TNTBVMAR1')
018900150327     d   kpjba                             likeds(KPJBA)
019000150219
019100150219       // -?Controllo/Decodifica cliente?
019200150219      /copy gaitrasrc/srcProtoPI,TIBS69R
019300150219      /copy gaitrasrc/srcProtoPR,TIBS69R
019400150219
019500150219      *// // -?Interrogazione sottoconti?
019600150219      *///copy gaitrasrc/srcProtoPR,XALFA3BRDS
019700150219      *///copy gaitrasrc/srcProtoPR,XALFA3BR
019800150219
019900150219       // -?Controllo/Inversione data?
020000150219     d WLBdat          ds                  inz
020100150219     d   G08dat                       8  0 inz
020200150219     d   G08inv                       8  0 inz
020300150219     d   G08err                       1    inz('3')
020400150219     d   G08tgi                       5  0 inz
020500150219      /copy gaitrasrc/srcProtoPR,XSRDA8
020600150219
020700150219       //--------------------------------------------------------------
020800150219       //?Definizione key-list.                                        ?
020900150219       //--------------------------------------------------------------
021000150219
021100150219       // -?File TNTBE01L?
021200150219     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
021300150219     d                                     prefix( k_ )   inz
021400150219
021500150219       //--------------------------------------------------------------
021600150219       //?M A I N - L I N E                                            ?
021700150219       //--------------------------------------------------------------
021800150219
021900150219     c     *Entry        plist
022000150219     c                   parm                    KPJBA
022100150219
022200150219      /free
022300150219
022400150219       // -?Operazioni iniziali?
022500150219       exsr sr_RoutInz;
022600150219
022700150219       // -?Ciclo di gestione del file video?
022800150219       DoW  $Fine = *off;
022900150219
023000150224         Select;
023100150219
023200150219           // -?Gestione videata D1: richiesta chiave di accesso?
023300150224           When $Video = 'D1';
023400150219             exsr sr_GesD01;
023500150219
023600150219           // -?Gestione videata D2: manutenzione dati della singola tabella?
023700150224           When $Video = 'D2';
023800150219             exsr sr_GesD02;
023900150219
024000150219           // -?? ? ? ? ??
024100150224           Other;
024200150219             $Fine = *on;
024300150219
024400150224         EndSl;
024500150219
024600150224       EndDo;
024700150219
024800150219       // -?Operazioni finali?
024900150219       exsr sr_RoutEnd;
025000150219
025100150219       //--------------------------------------------------------------
025200150219       //?Operazioni iniziali.                                         ?
025300150219       //--------------------------------------------------------------
025400150219       BEGSR  sr_RoutInz;
025500150219
025600150219         *inLR = *on;
025700150219
025800150219         // -?Reperimento dati job?
025900150219         exsr  sr_DatiJob;
026000150224
026100150224         // -?Reperimento data odierna?
026200150224         wDate = %int( %subst( %char( %dec( %timestamp() ) )
026300150224                               : 1 : 8 ) );
026400150219
026500150219         // -?Impostazione nome programma a video?
026600150219         V1Tpgm = SDSpgm;
026700150224
026800150224         // -?Verifica del sistema AS/400 corrente?
026900150224         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
027000150224           $Fine = *on;
027100150224           leavesr;
027200150224         endif;
027300150224
027400150224         // -?Impostazione elenco librerie in cui gestire le tabelle?
027500150224         //  ?(a seconda del sistema in cui si stà lavorando)?
027600150224         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
027700150224         if  w_iSystem = *zero;
027800150224           $Fine = *on;
027900150224           leavesr;
028000150224         endif;
028100150224
028200150224         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
028300150224         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
028400150224           w_SisInf = 1;
028500150224           exsr  sr_OpenFileTab;
028600150224         endif;
028700150219
028800150219         // -?Aggancio dati generali della tabella in esame?
028900150219         clear  keyTNTBE01;
029000150219         k_TBEke1 = *zero;
029100150219         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
029200150219                = c_Tab;
029300150219         k_TBEsif = KNSIF;
029400150219         chain(n)  %kds(keyTNTBE01)  TNTBE000;
029500150219         if  not  %found(TNTBE01L);
029600150219           clear  k_TBEsif;
029700150219           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
029800150219         endif;
029900150219         if  %found(TNTBE01L);
030000150219           xTNTBEds = TNTBEds;
030100150219         else;
030200150219           clear  xTNTBEds;
030300150219         endif;
030400150219
030500150219         // -?Impostazione iniziale / pulizia dei campi chiave?
030600150219         clear  keyTNTBE01;
030700150219         k_TBEcod = c_Tab;
030800150219
030900150219       ENDSR;
031000150219
031100150219       //--------------------------------------------------------------
031200150219       //?Reperimento Dati del job (Utente/Operativi).                 ?
031300150219       //--------------------------------------------------------------
031400150219       BEGSR  sr_DatiJob;
031500150219
031600150219         in(E)  §AzUte;
031700150219         if  NOT %error;
031800150219           in(E)  §DatiUte;
031900150219         endif;
032000150219         if  %error  or  RSut = *blanks;
032100150219           clear  TIBS34ds;
032200150219           tibs34r ( tibs34ds );
032300150219           in  §AzUte;
032400150219           in  §DatiUte;
032500150219         endif;
032600150219
032700150219       ENDSR;
032800150219
032900150219       //--------------------------------------------------------------
033000150219       //?Gestione videata D01.                                        ?
033100150219       //--------------------------------------------------------------
033200150219       BEGSR  sr_GesD01;
033300150219
033400150219         // -?Inizializzazione videata?
033500150219         if  $InzD01 = *on;
033600150219           exsr  sr_InzD01;
033700150219           $InzD01 = *off;
033800150219         endif;
033900150219         clear  V1Topz;
034000150320
034100150320         // -?(Dis)Abilitazione Tasti Funzionali?
034200150320         // -?F3=Fine?
034300150320         F3Attivo  = *on;
034400150320         // -?F5=Ripristino?
034500150320         F5Attivo  = *off;
034600150320         // -?F6=Conferma?
034700150320         F6Attivo  = *off;
034800150320         // -?F12=Ritorno?
034900150320         F12Attivo = *off;
035000150320         // -?F16=Annullamento?
035100150320         F16Attivo = *off;
035200150219
035300150219         // -?Emissione videata?
035400150219         write  TBVMAT01;
035500150219         write  TBVMAP01;
035600150219         exfmt  TBVMAD01;
035700150219
035800150219         clear  V1Dmsg;
035900150219         reset  ErrMessage;
036000150219         reset  ErrGenerico;
036100150219
036200150219         SELECT;
036300150219
036400150219           // -?Utente NON di Sede (errore già visualizzato)?
036500150219           WHEN  DUTlpo <> 'S';
036600150219             $Fine = *on;
036700150219
036800150219           // -?F3=Fine?
036900150219           WHEN  dsp_aid = c_F03;
037000150219             $Fine = *on;
037100150219
037200150219           // -?Invio?
037300150219           OTHER;
037400150219             exsr  sr_CtrD01;
037500150219             if  ErrGenerico = *off;
037600150219               $Video    = 'D2';
037700150219               $InzD02   = *on;
037800150219             endif;
037900150219
038000150219         ENDSL;
038100150219
038200150219       ENDSR;
038300150219
038400150219       //--------------------------------------------------------------
038500150219       //?Inizializzazione videata D01.                                ?
038600150219       //--------------------------------------------------------------
038700150219       BEGSR  sr_InzD01;
038800150219
038900150219         clear  TBVMAD01;
039000150219
039100150219         V1Cksc = '?';
039200150219
039300150320         // -?Eseguibile SOLO da Sede?
039400150219         if  DUTlpo <> 'S';
039500150219           V1Dmsg = sk_Msg(01);
039600150219           ErrMessage  = *on;
039700150219           ErrGenerico = *on;
039800150219           leavesr;
039900150219         endif;
040000150219
040100150219       ENDSR;
040200150219
040300150219       //--------------------------------------------------------------
040400150219       //?Controllo dati immessi nella videata D01.                    ?
040500150219       //--------------------------------------------------------------
040600150219       BEGSR  sr_CtrD01;
040700150219
040800150219         // -?Spegnimento indicatori di posizionamento cursore?
040900150219         %subst(IndDspF : 28) = *off;
041000150219
041100150219         // -?Codice Cliente?
041200150219         clear  V1Dksc;
041300150219         Select;
041400150219
041500150219           // -?Ricerca?
041600150219           When  %scan( '?' : V1Cksc ) > *zero;
041700150219             clear  V1Cksc;
041800150327             clear  tntbVMAds1;
041900150327             kpjbu = tntbVMAds1;
042000150327             tntbVMAr1 ( kpjba );
042100150327             tntbVMAds1 = kpjbu;
042200150327             select;
042300150327               when  oVMAfxx = *blank  and  oVMAerr = *off;
042400150327                 V1Cksc = %editc( oVMAcli : 'X' );
042500150327               when  oVMAerr = *on;
042600150327                 ErrMessage = *on;
042700150327                 V1Dmsg = oVMAmsg;
042800150327             endsl;
042900150219             ErrGenerico = *on;
043000150219             leavesr;
043100150219
043200150219           // -?Controllo immissione?
043300150219           When  V1Cksc = *blank  or  V1Cksc = *zero;
043400150219             V1Dmsg = sk_Msg(02);
043500150219             PosCurKSC   = *on;
043600150219             ErrMessage  = *on;
043700150219             ErrGenerico = *on;
043800150219             leavesr;
043900150219
044000150219           // -?Controllo immissione solo caratteri numerici?
044100150219           When  %check( c_Digits : V1Cksc ) > *zero;
044200150219             V1Dmsg = sk_Msg(03);
044300150219             PosCurKSC   = *on;
044400150219             ErrMessage  = *on;
044500150219             ErrGenerico = *on;
044600150219             leavesr;
044700150219
044800150219         EndSl;
044900150219
045000150219         // -?Controllo / decodifica cliente?
045100150219         clear  TIBS69ds;
045200150219         I69sif = knsif;
045300150219         I69kcc = DUTkci;
045400150219         I69kac = %int( V1Cksc );
045500150219         tibs69r( TIBS69ds :
045600150219                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
045700150219         if  O69err = *on;
045800150219           V1Dmsg = sk_Msg(04);
045900150219           PosCurKSC   = *on;
046000150219           ErrMessage  = *on;
046100150219           ErrGenerico = *on;
046200150219           leavesr;
046300150219         endif;
046400150219         V1Dksc = ACOrag;
046500150219
046600150219       ENDSR;
046700150219
046800150219       //--------------------------------------------------------------
046900150219       //?Gestione videata D02.                                        ?
047000150219       //--------------------------------------------------------------
047100150219       BEGSR  sr_GesD02;
047200150219
047300150219         // -?Inizializzazione videata?
047400150219         if  $InzD02 = *on;
047500150219           exsr  sr_InzD02;
047600150219           $InzD02 = *off;
047700150219         endif;
047800150219
047900150219         // -?Emissione Testata e Piede con tasti funzionali abilitati?
048000150219         if  NOT ErrMessage;
048100150219           write  TBVMAT01;
048200150219           write  TBVMAD01;
048300150219         endif;
048400150219         write  TBVMAP01;
048500150219
048600150219         // -?Emissione videata?
048700150219         if  Not $Protect;
048800150219           write  Protect;
048900150219           exfmt  TBVMAD02;
049000150219         else;
049100150219           write  TBVMAD02;
049200150219           exfmt  Protect;
049300150219         endif;
049400150219
049500150219         clear  V1Dmsg;
049600150219         reset  ErrMessage;
049700150219         reset  ErrGenerico;
049800150219
049900150219         SELECT;
050000150219
050100150219           // -?F3=Fine?
050200150219           WHEN  dsp_aid = c_F03;
050300150219             unlock  TNTBE01L;
050400150219             $Fine = *on;
050500150219
050600150219           // -?F12=Ritorno?
050700150219           WHEN  dsp_aid = c_F12;
050800150219             unlock  TNTBE01L;
050900150219             reset  $Video;
051000150219             clear  V1Topz;
051100150219
051200150219           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
051300150219           OTHER;
051400150219             $Ripristino   = (dsp_aid = c_F05);
051500150219             $Annullamento = (dsp_aid = c_F16);
051600150219             // -?Controlli eseguiti NON se F16=Annullamento?
051700150219             if  Not $Annullamento;
051800150219               exsr  sr_CtrD02;
051900150219             // -?Controlli eseguiti SOLO se F16=Annullamento?
052000150219             //else;
052100150219             //  exsr  sr_CtrANN;
052200150219             endif;
052300150219             if  ErrGenerico;
052400150219               leavesr;
052500150219             endif;
052600150219             // -?Aggiornamento?
052700150219             if  dsp_aid = c_F05   or
052800150219                 dsp_aid = c_F06   or
052900150219                 dsp_aid = c_F16;
053000150224               exsr  sr_UpdateAll;
053100150224               // -?Rilevato errore in fase di aggiornamento?
053200150224               if  ErrGenerico;
053300150224                 leavesr;
053400150224               endif;
053500150224               // -?Ritorno alla videata iniziale altrimenti?
053600150224               $Video  = 'D1';
053700150224               $InzD01 = *on;
053800150219             endif;
053900150219
054000150219         ENDSL;
054100150219
054200150219       ENDSR;
054300150219
054400150219       //--------------------------------------------------------------
054500150219       //?Inizializzazione videata D02.                                ?
054600150219       //--------------------------------------------------------------
054700150219       BEGSR  sr_InzD02;
054800150219
054900150219         reset  $Immissione;
055000150219         reset  $Ripristino;
055100150219         reset  $Protect;
055200150219
055300150219         // -?Spegnimento degli indicatori di errore?
055400150219         %subst(IndDspF : 50) = *off;
055500150224
055600150224         // -?Apertura file TABEL00F del 1° S.I. (sede)?
055700150224         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
055800150224           w_SisInf = 1;
055900150224           exsr  sr_OpenFileTab;
056000150224         endif;
056100150219
056200150219         // -?Pulizia videata?
056300150219         clear  TBVMAD02;
056400150219
056500150224         // -?Verifica esistenza del record in TNTBE?
056600150224         //  ?e Reperimento dati del Cliente?
056700150219         k_TBEke1 = V1Cksc;
056800150219         chain  %kds( keyTNTBE01 )  TNTBE000;
056900150219
057000150219         // -?Impostazione testata?
057100150219         Select;
057200150219           When  Not %found(TNTBE01L);
057300150219             V1Topz = '  INSERIMENTO  ';
057400150219             $Immissione = *on;
057500150219           When  TBEatb <> *blank;
057600150219             V1Topz = '  RIPRISTINO   ';
057700150219             $Ripristino = *on;
057800150219           Other;
057900150219             V1Topz = '   MODIFICA    ';
058000150219         EndSl;
058100150219
058200150219         // -?Caricamento dati del Codice?
058300150224         clear  dVMA;
058400150224         If  %found(TNTBE01L);
058500150224           dVMA = TBEuni;
058600150219           if  §VMAddt > *zero;
058700150219             wDate_EUR = %date( §VMAddt : *iso );
058800150219             V1Cddt    = %dec( wDate_EUR );
058900150219           endif;
059000150219           if  §VMAdst > *zero;
059100150219             wDate_EUR = %date( §VMAdst : *iso );
059200150219             V1Cdst    = %dec( wDate_EUR );
059300150219           endif;
059400150224         EndIf;
059500150219
059600150219         // -?Impostazione indicatori per abilitazione tasti funzionali?
059700150219         exsr  sr_AbilitFxxD02;
059800150219
059900150219       ENDSR;
060000150219
060100150219       //--------------------------------------------------------------
060200150219       //?Settaggio indicatori nella videata D02 per abilitazione      ?
060300150219       //?  tasti funzionali.                                          ?
060400150219       //--------------------------------------------------------------
060500150219       BEGSR  sr_AbilitFxxD02;
060600150219
060700150219         // -?F3=Fine?
060800150219         F3Attivo  = *on;
060900150219
061000150219         // -?F5=Ripristino?
061100150219         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
061200150219                                         and   Not $Protect);
061300150219
061400150219         // -?F6=Conferma?
061500150219         F6Attivo  = (NOT F5Attivo);
061600150219
061700150219         // -?F12=Ritorno?
061800150219         F12Attivo = *on;
061900150219
062000150219         // -?F16=Annullamento?
062100150219         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
062200150219                                         and   Not $Protect);
062300150219
062400150219       ENDSR;
062500150219
062600150219       //--------------------------------------------------------------
062700150219       //?Controllo dati videata D02.                                  ?
062800150219       //--------------------------------------------------------------
062900150219       BEGSR  sr_CtrD02;
063000150219
063100150219         // -?Spegnimento degli indicatori di errore?
063200150219         %subst(IndDspF : 50) = *off;
063300150219
063400150219         clear  dVMA;
063500150219
063600150219         // -?Data Decorrenza NON applicazione V.M.A.?
063700150219         if  V1Cddt = *zero;
063800150219           ErrGenerico = *on;
063900150219           ErrMessage  = *on;
064000150219           PosCurDDT   = *on;
064100150219           V1Dmsg = sk_Msg(05);
064200150219           leavesr;
064300150219         endif;
064400150219
064500150219         clear WLBdat;
064600150219         G08dat = V1Cddt;
064700150219         XSRDA8(WLBdat);
064800150219         if  G08err = *on;
064900150219           ErrGenerico = *on;
065000150219           ErrMessage  = *on;
065100150219           PosCurDDT   = *on;
065200150219           V1Dmsg = sk_Msg(06);
065300150219           leavesr;
065400150219         endif;
065500150219         V1Cddt  = G08dat;
065600150219         §VMAddt = G08inv;
065700150219
065800150219         // -?Data Scadenza NON applicazione V.M.A.?
065900150219         if  V1Cdst = *zero;
066000150219           ErrGenerico = *on;
066100150219           ErrMessage  = *on;
066200150219           PosCurDST   = *on;
066300150219           V1Dmsg = sk_Msg(05);
066400150219           leavesr;
066500150219         endif;
066600150219
066700150219         clear WLBdat;
066800150219         G08dat = V1Cdst;
066900150219         XSRDA8(WLBdat);
067000150219         if  G08err = *on;
067100150219           ErrGenerico = *on;
067200150219           ErrMessage  = *on;
067300150219           PosCurDST   = *on;
067400150219           V1Dmsg = sk_Msg(06);
067500150219           leavesr;
067600150219         endif;
067700150219         V1Cdst  = G08dat;
067800150219         §VMAdst = G08inv;
067900150219
068000150219         // -?Controllo Range di Date Decorrenza/Scadenza?
068100150219         if  §VMAddt > §VMAdst;
068200150219           ErrGenerico = *on;
068300150219           ErrMessage  = *on;
068400150219           PosCurDST   = *on;
068500150219           V1Dmsg = sk_Msg(07);
068600150219           leavesr;
068700150219         endif;
068800150219
068900150219       ENDSR;
069000150224
069100150224       //--------------------------------------------------------------
069200150224       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
069300150224       //--------------------------------------------------------------
069400150224       BEGSR  sr_UpdateAll;
069500150224
069600150224         // -?Ciclo di elaborazione per ogni sistema informativo?
069700150224         For  w_SisInf = 1  To  %elem(sk_Libr);
069800150224
069900150224           // -?Apertura degli archivi?
070000150224           if  w_SisInf > 1;
070100150224             exsr  sr_OpenFileTab;
070200150224           endif;
070300150224
070400150224           // -?Aggiornamento tab. "VMA"?
070500150224           exsr  sr_UpdTNTBE;
070600150224
070700150224         EndFor;
070800150224
070900150224       ENDSR;
071000150224
071100150224       //--------------------------------------------------------------
071200150224       //?Apertura dei files tabelle nel sistema informativo impostato.?
071300150224       //--------------------------------------------------------------
071400150224       BEGSR  sr_OpenFileTab;
071500150224
071600150224         // -?Chiusura (eventuale) file tabelle?
071700150224         if  %open(TNTBE01L);
071800150224           close  TNTBE01L;
071900150224         endif;
072000150224
072100150224         // -?Apertura file tabelle?
072200150224         ds_Libr  = sk_Libraries(w_iSystem);
072300150224         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
072400150224         open  TNTBE01L;
072500150224
072600150224       ENDSR;
072700150219
072800150219       //--------------------------------------------------------------
072900150219       //?Aggiornamento records TNTBE00F (tab. "VMA")                  ?
073000150219       //--------------------------------------------------------------
073100150219       BEGSR  sr_UpdTNTBE;
073200150224
073300150224         //? N.B.                                                      ?
073400150224         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
073500150224         //   trasmissione (vedi flag TBEFTR).                        ?
073600150224         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
073700150224         //   subito nello stesso file di entrambi i S.I. - in due    ?
073800150224         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
073900150224
074000150224         // -?RI-aggancio record da aggiornare?
074100150224         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
074200150219
074300150219         // -?Aggancio record di TNTBE00F?
074400150224         //if  w_SisInf > 1;
074500150224           clear  keyTNTBE01;
074600150224           k_TBEcod  = c_Tab;
074700150224           k_TBEke1  = V1Cksc;
074800150224           chain  %kds( keyTNTBE01 )  TNTBE000;
074900150224         //endif;
075000150219
075100150219         //§VMAddt = ......;       ?(già impostata)?
075200150219         //§VMAdst = ......;       ?(già impostata)?
075300150219         TBEuni = dVMA;
075400150219
075500150219         // -?Aggiornamento tab. "VMA"?
075600150224         SELECT;
075700150219
075800150219           // -?F5=Ripristino?
075900150224           WHEN  $Ripristino;
076000150224             TBEapl = TBXapl;
076100150224             //TBEftt = W1ftt;
076200150224             TBEftt = 'S';
076300150224             clear  TBEflt;
076400150224             if  w_SisInf = 1;
076500150224               TBEftr = 'T';
076600150224             else;
076700150224               TBEftr = 'R';
076800150224             endif;
076900150224             TBEdtr = wDate;
077000150224             select;
077100150224               when  %found(TNTBE01L)  and  TBEatb <> *blank;
077200150224                 clear  TBEatb;
077300150224                 //______________
077400150224                 UPDATE  TNTBE000;
077500150224                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077600150224               // -?Record appena cancellato fisicamente?
077700150224               when  NOT %found(TNTBE01L);
077800150224                 clear  TBEatb;
077900150224                 //______________
078000150224                 WRITE   TNTBE000;
078100150224                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078200150224             endsl;
078300150219
078400150219           // -?F16=Annullamento?
078500150219           WHEN  $Annullamento;
078600150224             if  %found(TNTBE01L)  and  TBEatb <> 'A';
078700150224               TBEatb = 'A';
078800150224               //TBEftt = W1ftt;
078900150224               TBEftt = 'S';
079000150224               clear  TBEftr;
079100150224               //______________
079200150224               UPDATE  TNTBE000;
079300150224               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079400150224             endif;
079500150219
079600150219           // -?Immissione o Modifica?
079700150219           OTHER;
079800150219             if  NOT %found(TNTBE01L);
079900150219               clear TNTBE000;
080000150219               TBEcod  = k_TBEcod;
080100150219               TBEke1  = k_TBEke1;
080200150219               TBEke2  = k_TBEke2;
080300150219               TBElin  = k_TBElin;
080400150219               TBEsif  = k_TBEsif;
080500150219               TBEuni  = dVMA;
080600150219             endif;
080700150224             clear  TBEatb;
080800150219             TBEapl = TBXapl;
080900150224             //TBEftt = W1ftt;
081000150224             TBEftt = 'S';
081100150219             clear  TBEflt;
081200150224             if  w_SisInf = 1;
081300150224               TBEftr = 'T';
081400150224             else;
081500150224               TBEftr = 'R';
081600150224             endif;
081700150224             TBEdtr = wDate;
081800150219             // -?Aggiornamento record?
081900150219             if  %found(TNTBE01L);
082000150219               //_____________
082100150219               UPDATE TNTBE000;
082200150219               //¯¯¯¯¯¯¯¯¯¯¯¯¯
082300150219             else;
082400150219               //_____________
082500150219               WRITE  TNTBE000;
082600150219               //¯¯¯¯¯¯¯¯¯¯¯¯¯
082700150219             endif;
082800150219
082900150219         ENDSL;
083000150219
083100150219       ENDSR;
083200150219
083300150219       //--------------------------------------------------------------
083400150219       //?Operazioni finali.                                           ?
083500150219       //--------------------------------------------------------------
083600150219       BEGSR  sr_RoutEnd;
083700150219
083800150219         // -?Chiusura funzioni precedentemente aperte?
083900150219         clear  TIBS69ds;
084000150219         I69tla = 'C';
084100150219         tibs69r( tibs69ds :
084200150219                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
084300150219
084400150219         //  ?Uscita?
084500150219         return;
084600150219
084700150219       ENDSR;
084800150219
084900150219      /end-free
085000150219
085100150219       //--------------------------------------------------------------
085200150219       //?Definizione schiere a tempo di compilazione.                 ?
085300150219       //--------------------------------------------------------------
085400150219
085500150224** -?sk_iSystem / sk_Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
085600150224SETRAS  GAITRAGRU FILTRAGRU
085700150224AS888   GAITRAGRPSFILTRAGRPF
085800150219** -?sk_Msg:?Messaggi di Errore?---------------------------------------------*
085900150219Tabella gestibile SOLO da SEDE                                                  1
086000150219Codice cliente obbligatorio                                                     2
086100150219Ammessi SOLO caratteri numerici                                                 3
086200150219Cliente NON presente in anagrafica                                              4
086300150219Data obbligatoria                                                               5
086400150219Data formalmente errata                                                         6
086500150219Data iniziale successiva a quella finale                                        7
