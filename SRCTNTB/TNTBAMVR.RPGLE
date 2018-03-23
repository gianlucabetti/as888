000100160701       //==============================================================
000200160701       //?Gestione tabella "AMV" (Autorizzazione MAI VanDerLande).     ?
000300160701       //==============================================================
000400160701
000500160701       //--------------------------------------------------------------
000600160701       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160701       //--------------------------------------------------------------
000800160701
000900160701     /*PRM  dbgview(*source)
001000160701     /*END
001100160701
001200160701       //--------------------------------------------------------------
001300160701       //?Specifiche di controllo.                                     ?
001400160701       //--------------------------------------------------------------
001500160701
001600160701     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160701     h dftactgrp(*no)
001800160701     h bnddir('UBRTVNETA')
001900160701
002000160701       //--------------------------------------------------------------
002100160701       //?Dichiarazione file.                                          ?
002200160701       //--------------------------------------------------------------
002300160701
002400160701       // -?Tabelle?
002500160701     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
002600160701     f                                     usropn
002700160701
002800160701       // -?Video?
002900160701     fTNTBAMVD  cf   e             workstn
003000160701     f                                     indds(IndDspF)
003100160701     f                                     infds(InfDspF)
003200160701
003300160701       //--------------------------------------------------------------
003400160701       //?Definizione costanti.                                        ?
003500160701       //--------------------------------------------------------------
003600160701
003700160701       // -?Codice tabella in gestione?
003800160701     d c_Tab           c                   const('AMV')
003900160701
004000160701       // -?Costanti per la definizione delle schiere con i nomi?
004100160701       //  ?degli iSeries da elaborare e delle relative librerie?
004200160701     d c_NrSyst        c                   const(2)
004300160701     d c_NrLibr        c                   const(2)
004400160701
004500160701       // -?Tasti funzionali a video?
004600160701     d c_F01           c                   const(x'31')
004700160701     d c_F02           c                   const(x'32')
004800160701     d c_F03           c                   const(x'33')
004900160701     d c_F04           c                   const(x'34')
005000160701     d c_F05           c                   const(x'35')
005100160701     d c_F06           c                   const(x'36')
005200160701     d c_F07           c                   const(x'37')
005300160701     d c_F08           c                   const(x'38')
005400160701     d c_F09           c                   const(x'39')
005500160701     d c_F10           c                   const(x'3A')
005600160701     d c_F11           c                   const(x'3B')
005700160701     d c_F12           c                   const(x'3C')
005800160701     d c_F13           c                   const(x'B1')
005900160701     d c_F14           c                   const(x'B2')
006000160701     d c_F15           c                   const(x'B3')
006100160701     d c_F16           c                   const(x'B4')
006200160701     d c_F17           c                   const(x'B5')
006300160701     d c_F18           c                   const(x'B6')
006400160701     d c_F19           c                   const(x'B7')
006500160701     d c_F20           c                   const(x'B8')
006600160701     d c_F21           c                   const(x'B9')
006700160701     d c_F22           c                   const(x'BA')
006800160701     d c_F23           c                   const(x'BB')
006900160701     d c_F24           c                   const(x'BC')
007000160701     d c_Enter         c                   const(x'F1')
007100160701     d c_RollDown      c                   const(x'F4')
007200160701     d c_RollUp        c                   const(x'F5')
007300160701
007400160701       // -?Costante per controllo "caratteri solo numerici"?
007500160701     d c_Digits        c                   const('0123456789')
007600160701
007700160701       //--------------------------------------------------------------
007800160701       //?Definizione schiere.                                         ?
007900160701       //--------------------------------------------------------------
008000160701
008100160701       // -?iSeries  &  Librerie con entrambi i file tabelle?
008200160701     d sk_iSystem      s                   like(currSysNetA)
008300160701     d                                     dim(c_NrSyst)
008400160701     d                                     ctdata   perrcd( 1)
008500160701     d sk_Libraries    s                   like(ds_Libr)
008600160701     d                                     dim(c_NrSyst)
008700160701     d                                     alt(sk_iSystem)
008800160701
008900160701       // -?Messaggi di errore?
009000160701     d sk_Msg          s             78    dim( 7)  ctdata  perrcd( 1)
009100160701
009200160701       //--------------------------------------------------------------
009300160701       //?Definizione aree dati.                                       ?
009400160701       //--------------------------------------------------------------
009500160701
009600160701       // -?Dati utente?
009700160701     d §AzUte        e ds                  extname(AZUTE00F)
009800160701     d                                     dtaara
009900160701     d §DatiUte      e ds                  extname(dDatiUte)
010000160701     d                                     dtaara
010100160701
010200160701       //--------------------------------------------------------------
010300160701       //?Definizione strutture dati.                                  ?
010400160701       //--------------------------------------------------------------
010500160701
010600160701       // -?Status ds?
010700160701     d Status         sds
010800160701     d   SDSpgm          *proc
010900160701
011000160701       // -?InfDS?
011100160701     d InfDspF         ds
011200160701     d   dsp_aid             369    369a
011300160701
011400160701       // -?Indicatori su DspF?
011500160701     d IndDspF         ds                  inz
011600160701         // -?Abilitazione tasti funzionali?
011700160701     d   F3Attivo                      n   overlay(IndDspF : 03)
011800160701     d   F5Attivo                      n   overlay(IndDspF : 05)
011900160701     d   F6Attivo                      n   overlay(IndDspF : 06)
012000160701     d   F12Attivo                     n   overlay(IndDspF : 12)
012100160701     d   F16Attivo                     n   overlay(IndDspF : 16)
012200160701         // -?Emissione messaggio di errore?
012300160701     d   ErrMessage                    n   overlay(IndDspF : 28)
012400160701         // -?Posizionamento cursore & segnalazione errore?
012500160701     d   PosCurKSC                     n   overlay(IndDspF : 50)
012600160701     d   PosCurDTI                     n   overlay(IndDspF : 51)
012700160701     d   PosCurDTF                     n   overlay(IndDspF : 52)
012800160701         // -?Emissione messaggio di errore in window W1?
012900160701     d   ErrMessageW1                  n   overlay(IndDspF : 90)
013000160701         //   -?Riemissione videata?
013100160701     d   ErrGenerico                   n   overlay(IndDspF : 99)
013200160701
013300160701       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
013400160701     d ds_Libr         ds                  inz
013500160701     d   sk_Libr                     10    dim(c_NrLibr) inz
013600160701
013700160701       // -?Parametri ricevuti?
013800160701     d KPJBA         e ds
013900160701
014000160701       // -?Dati record principale della tabella?
014100160701     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014200160701     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014300160701     d                                          prefix(TBX:3)
014400160701
014500160701       // -?Tabelle usate:?
014600160701       // ·?"AMV" = Autorizzazioni MAI VanDerLande?
014700160701     d dAMV          e ds                  inz
014800160701
014900160701       //--------------------------------------------------------------
015000160701       //?Definizione variabili globali.                               ?
015100160701       //--------------------------------------------------------------
015200160701
015300160701       // -?Flags booleani?
015400160701     d $Fine           s               n   inz
015500160701     d $InzD01         s               n   inz(*on)
015600160701     d $InzD02         s               n   inz(*on)
015700160701     d $Annullamento   s               n   inz
015800160701     d $Immissione     s               n   inz
015900160701     d $Ripristino     s               n   inz
016000160701     d $Protect        s               n   inz
016100160701
016200160701       // -?Variabili per la gestione del video?
016300160701     d $Video          s              2    inz('D1')
016400160701
016500160701       // -?Nome esteso Libreria/File del file tabelle?
016600160701     d wLibFile        s             21a   inz
016700160701
016800160701       // -?Campi di comodo?
016900160701     d w_iSystem       s              1  0 inz
017000160701     d w_SisInf        s              3  0 inz
017100160701     d wDate           s              8  0 inz
017200160701     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
017300160701
017400160701       //--------------------------------------------------------------
017500160701       //?Definizione prototipi procedure.                             ?
017600160701       //--------------------------------------------------------------
017700160701
017800160701       // -?Reperimento NETA sistema AS/400 corrente?
017900160701     d currSysNeta     s              8a   inz
018000160701      /copy gaitrasrc/srcProtoPR,UBRTVNETA
018100160701
018200160701       // -?Reperimento dati utente?
018300160701     d TIBS34ds      e ds                  inz
018400160701      /copy gaitrasrc/srcProtoPR,TIBS34R
018500160701
018600160701       // -?Ricerca e controllo nuove tabelle?
018700160701     d tntbAMVds1    e ds                  inz
018800160701     d tntbAMVr1       pr                  extpgm('TNTBAMVR1')
018900160701     d   kpjba                             likeds(KPJBA)
019000160701
019100160701       // -?Controllo/Decodifica cliente?
019200160701      /copy gaitrasrc/srcProtoPI,TIBS69R
019300160701      /copy gaitrasrc/srcProtoPR,TIBS69R
019400160701
019500160701      *// // -?Interrogazione sottoconti?
019600160701      *///copy gaitrasrc/srcProtoPR,XALFA3BRDS
019700160701      *///copy gaitrasrc/srcProtoPR,XALFA3BR
019800160701
019900160701       // -?Controllo/Inversione data?
020000160701     d WLBdat          ds                  inz
020100160701     d   G08dat                       8  0 inz
020200160701     d   G08inv                       8  0 inz
020300160701     d   G08err                       1    inz('3')
020400160701     d   G08tgi                       5  0 inz
020500160701      /copy gaitrasrc/srcProtoPR,XSRDA8
020600160701
020700160701       //--------------------------------------------------------------
020800160701       //?Definizione key-list.                                        ?
020900160701       //--------------------------------------------------------------
021000160701
021100160701       // -?File TNTBE01L?
021200160701     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
021300160701     d                                     prefix( k_ )   inz
021400160701
021500160701       //--------------------------------------------------------------
021600160701       //?M A I N - L I N E                                            ?
021700160701       //--------------------------------------------------------------
021800160701
021900160701     c     *Entry        plist
022000160701     c                   parm                    KPJBA
022100160701
022200160701      /free
022300160701
022400160701       // -?Operazioni iniziali?
022500160701       exsr sr_RoutInz;
022600160701
022700160701       // -?Ciclo di gestione del file video?
022800160701       DoW  $Fine = *off;
022900160701
023000160701         Select;
023100160701
023200160701           // -?Gestione videata D1: richiesta chiave di accesso?
023300160701           When $Video = 'D1';
023400160701             exsr sr_GesD01;
023500160701
023600160701           // -?Gestione videata D2: manutenzione dati della singola tabella?
023700160701           When $Video = 'D2';
023800160701             exsr sr_GesD02;
023900160701
024000160701           // -?? ? ? ? ??
024100160701           Other;
024200160701             $Fine = *on;
024300160701
024400160701         EndSl;
024500160701
024600160701       EndDo;
024700160701
024800160701       // -?Operazioni finali?
024900160701       exsr sr_RoutEnd;
025000160701
025100160701       //--------------------------------------------------------------
025200160701       //?Operazioni iniziali.                                         ?
025300160701       //--------------------------------------------------------------
025400160701       BEGSR  sr_RoutInz;
025500160701
025600160701         *inLR = *on;
025700160701
025800160701         // -?Reperimento dati job?
025900160701         exsr  sr_DatiJob;
026000160701
026100160701         // -?Reperimento data odierna?
026200160701         wDate = %int( %subst( %char( %dec( %timestamp() ) )
026300160701                               : 1 : 8 ) );
026400160701
026500160701         // -?Impostazione nome programma a video?
026600160701         V1Tpgm = SDSpgm;
026700160701
026800160701         // -?Verifica del sistema AS/400 corrente?
026900160701         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
027000160701           $Fine = *on;
027100160701           leavesr;
027200160701         endif;
027300160701
027400160701         // -?Impostazione elenco librerie in cui gestire le tabelle?
027500160701         //  ?(a seconda del sistema in cui si stà lavorando)?
027600160701         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
027700160701         if  w_iSystem = *zero;
027800160701           $Fine = *on;
027900160701           leavesr;
028000160701         endif;
028100160701
028200160701         // -?1ª apertura file TABEL00F del 1° S.I. (sede)?
028300160701         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
028400160701           w_SisInf = 1;
028500160701           exsr  sr_OpenFileTab;
028600160701         endif;
028700160701
028800160701         // -?Aggancio dati generali della tabella in esame?
028900160701         clear  keyTNTBE01;
029000160701         k_TBEke1 = *zero;
029100160701         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
029200160701                = c_Tab;
029300160701         k_TBEsif = KNSIF;
029400160701         chain(n)  %kds(keyTNTBE01)  TNTBE000;
029500160701         if  not  %found(TNTBE01L);
029600160701           clear  k_TBEsif;
029700160701           chain(n)  %kds(KeyTNTBE01)  TNTBE000;
029800160701         endif;
029900160701         if  %found(TNTBE01L);
030000160701           xTNTBEds = TNTBEds;
030100160701         else;
030200160701           clear  xTNTBEds;
030300160701         endif;
030400160701
030500160701         // -?Impostazione iniziale / pulizia dei campi chiave?
030600160701         clear  keyTNTBE01;
030700160701         k_TBEcod = c_Tab;
030800160701
030900160701       ENDSR;
031000160701
031100160701       //--------------------------------------------------------------
031200160701       //?Reperimento Dati del job (Utente/Operativi).                 ?
031300160701       //--------------------------------------------------------------
031400160701       BEGSR  sr_DatiJob;
031500160701
031600160701         in(E)  §AzUte;
031700160701         if  NOT %error;
031800160701           in(E)  §DatiUte;
031900160701         endif;
032000160701         if  %error  or  RSut = *blanks;
032100160701           clear  TIBS34ds;
032200160701           tibs34r ( tibs34ds );
032300160701           in  §AzUte;
032400160701           in  §DatiUte;
032500160701         endif;
032600160701
032700160701       ENDSR;
032800160701
032900160701       //--------------------------------------------------------------
033000160701       //?Gestione videata D01.                                        ?
033100160701       //--------------------------------------------------------------
033200160701       BEGSR  sr_GesD01;
033300160701
033400160701         // -?Inizializzazione videata?
033500160701         if  $InzD01 = *on;
033600160701           exsr  sr_InzD01;
033700160701           $InzD01 = *off;
033800160701         endif;
033900160701         clear  V1Topz;
034000160701
034100160701         // -?(Dis)Abilitazione Tasti Funzionali?
034200160701         // -?F3=Fine?
034300160701         F3Attivo  = *on;
034400160701         // -?F5=Ripristino?
034500160701         F5Attivo  = *off;
034600160701         // -?F6=Conferma?
034700160701         F6Attivo  = *off;
034800160701         // -?F12=Ritorno?
034900160701         F12Attivo = *off;
035000160701         // -?F16=Annullamento?
035100160701         F16Attivo = *off;
035200160701
035300160701         // -?Emissione videata?
035400160701         write  TBAMVT01;
035500160701         write  TBAMVP01;
035600160701         exfmt  TBAMVD01;
035700160701
035800160701         clear  V1Dmsg;
035900160701         reset  ErrMessage;
036000160701         reset  ErrGenerico;
036100160701
036200160701         SELECT;
036300160701
036400160701           // -?Utente NON di Sede (errore già visualizzato)?
036500160701           WHEN  DUTlpo <> 'S';
036600160701             $Fine = *on;
036700160701
036800160701           // -?F3=Fine?
036900160701           WHEN  dsp_aid = c_F03;
037000160701             $Fine = *on;
037100160701
037200160701           // -?Invio?
037300160701           OTHER;
037400160701             exsr  sr_CtrD01;
037500160701             if  ErrGenerico = *off;
037600160701               $Video    = 'D2';
037700160701               $InzD02   = *on;
037800160701             endif;
037900160701
038000160701         ENDSL;
038100160701
038200160701       ENDSR;
038300160701
038400160701       //--------------------------------------------------------------
038500160701       //?Inizializzazione videata D01.                                ?
038600160701       //--------------------------------------------------------------
038700160701       BEGSR  sr_InzD01;
038800160701
038900160701         clear  TBAMVD01;
039000160701
039100160701         V1Cksc = '?';
039200160701
039300160701         // -?Eseguibile SOLO da Sede?
039400160701         if  DUTlpo <> 'S';
039500160701           V1Dmsg = sk_Msg(01);
039600160701           ErrMessage  = *on;
039700160701           ErrGenerico = *on;
039800160701           leavesr;
039900160701         endif;
040000160701
040100160701       ENDSR;
040200160701
040300160701       //--------------------------------------------------------------
040400160701       //?Controllo dati immessi nella videata D01.                    ?
040500160701       //--------------------------------------------------------------
040600160701       BEGSR  sr_CtrD01;
040700160701
040800160701         // -?Spegnimento indicatori di posizionamento cursore?
040900160701         %subst(IndDspF : 28) = *off;
041000160701
041100160701         // -?Codice Cliente?
041200160701         clear  V1Dksc;
041300160701         Select;
041400160701
041500160701           // -?Ricerca?
041600160701           When  %scan( '?' : V1Cksc ) > *zero;
041700160701             clear  V1Cksc;
041800160701             clear  tntbAMVds1;
041900160701             kpjbu = tntbAMVds1;
042000160701             tntbAMVr1 ( kpjba );
042100160701             tntbAMVds1 = kpjbu;
042200160701             select;
042300160701               when  oAMVfxx = *blank  and  oAMVerr = *off;
042400160701                 V1Cksc = %editc( oAMVcli : 'X' );
042500160701               when  oAMVerr = *on;
042600160701                 ErrMessage = *on;
042700160701                 V1Dmsg = oAMVmsg;
042800160701             endsl;
042900160701             ErrGenerico = *on;
043000160701             leavesr;
043100160701
043200160701           // -?Controllo immissione?
043300160701           When  V1Cksc = *blank  or  V1Cksc = *zero;
043400160701             V1Dmsg = sk_Msg(02);
043500160701             PosCurKSC   = *on;
043600160701             ErrMessage  = *on;
043700160701             ErrGenerico = *on;
043800160701             leavesr;
043900160701
044000160701           // -?Controllo immissione solo caratteri numerici?
044100160701           When  %check( c_Digits : V1Cksc ) > *zero;
044200160701             V1Dmsg = sk_Msg(03);
044300160701             PosCurKSC   = *on;
044400160701             ErrMessage  = *on;
044500160701             ErrGenerico = *on;
044600160701             leavesr;
044700160701
044800160701         EndSl;
044900160701
045000160701         // -?Controllo / decodifica cliente?
045100160701         clear  TIBS69ds;
045200160701         I69sif = knsif;
045300160701         I69kcc = DUTkci;
045400160701         I69kac = %int( V1Cksc );
045500160701         tibs69r( TIBS69ds :
045600160701                  ds_CNACO : ds_CNIND : ds_CNCLP : ds_FNCLS );
045700160701         if  O69err = *on;
045800160701           V1Dmsg = sk_Msg(04);
045900160701           PosCurKSC   = *on;
046000160701           ErrMessage  = *on;
046100160701           ErrGenerico = *on;
046200160701           leavesr;
046300160701         endif;
046400160701         V1Dksc = ACOrag;
046500160701
046600160701       ENDSR;
046700160701
046800160701       //--------------------------------------------------------------
046900160701       //?Gestione videata D02.                                        ?
047000160701       //--------------------------------------------------------------
047100160701       BEGSR  sr_GesD02;
047200160701
047300160701         // -?Inizializzazione videata?
047400160701         if  $InzD02 = *on;
047500160701           exsr  sr_InzD02;
047600160701           $InzD02 = *off;
047700160701         endif;
047800160701
047900160701         // -?Emissione Testata e Piede con tasti funzionali abilitati?
048000160701         if  NOT ErrMessage;
048100160701           write  TBAMVT01;
048200160701           write  TBAMVD01;
048300160701         endif;
048400160701         write  TBAMVP01;
048500160701
048600160701         // -?Emissione videata?
048700160701         if  Not $Protect;
048800160701           write  Protect;
048900160701           exfmt  TBAMVD02;
049000160701         else;
049100160701           write  TBAMVD02;
049200160701           exfmt  Protect;
049300160701         endif;
049400160701
049500160701         clear  V1Dmsg;
049600160701         reset  ErrMessage;
049700160701         reset  ErrGenerico;
049800160701
049900160701         SELECT;
050000160701
050100160701           // -?F3=Fine?
050200160701           WHEN  dsp_aid = c_F03;
050300160701             unlock  TNTBE01L;
050400160701             $Fine = *on;
050500160701
050600160701           // -?F12=Ritorno?
050700160701           WHEN  dsp_aid = c_F12;
050800160701             unlock  TNTBE01L;
050900160701             reset  $Video;
051000160701             clear  V1Topz;
051100160701
051200160701           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
051300160701           OTHER;
051400160701             $Ripristino   = (dsp_aid = c_F05);
051500160701             $Annullamento = (dsp_aid = c_F16);
051600160701             // -?Controlli eseguiti NON se F16=Annullamento?
051700160701             if  Not $Annullamento;
051800160701               exsr  sr_CtrD02;
051900160701             // -?Controlli eseguiti SOLO se F16=Annullamento?
052000160701             //else;
052100160701             //  exsr  sr_CtrANN;
052200160701             endif;
052300160701             if  ErrGenerico;
052400160701               leavesr;
052500160701             endif;
052600160701             // -?Aggiornamento?
052700160701             if  dsp_aid = c_F05   or
052800160701                 dsp_aid = c_F06   or
052900160701                 dsp_aid = c_F16;
053000160701               exsr  sr_UpdateAll;
053100160701               // -?Rilevato errore in fase di aggiornamento?
053200160701               if  ErrGenerico;
053300160701                 leavesr;
053400160701               endif;
053500160701               // -?Ritorno alla videata iniziale altrimenti?
053600160701               $Video  = 'D1';
053700160701               $InzD01 = *on;
053800160701             endif;
053900160701
054000160701         ENDSL;
054100160701
054200160701       ENDSR;
054300160701
054400160701       //--------------------------------------------------------------
054500160701       //?Inizializzazione videata D02.                                ?
054600160701       //--------------------------------------------------------------
054700160701       BEGSR  sr_InzD02;
054800160701
054900160701         reset  $Immissione;
055000160701         reset  $Ripristino;
055100160701         reset  $Protect;
055200160701
055300160701         // -?Spegnimento degli indicatori di errore?
055400160701         %subst(IndDspF : 50) = *off;
055500160701
055600160701         // -?Apertura file TABEL00F del 1° S.I. (sede)?
055700160701         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
055800160701           w_SisInf = 1;
055900160701           exsr  sr_OpenFileTab;
056000160701         endif;
056100160701
056200160701         // -?Pulizia videata?
056300160701         clear  TBAMVD02;
056400160701
056500160701         // -?Verifica esistenza del record in TNTBE?
056600160701         //  ?e Reperimento dati del Cliente?
056700160701         k_TBEke1 = V1Cksc;
056800160701         chain  %kds( keyTNTBE01 )  TNTBE000;
056900160701
057000160701         // -?Impostazione testata?
057100160701         Select;
057200160701           When  Not %found(TNTBE01L);
057300160701             V1Topz = '  INSERIMENTO  ';
057400160701             $Immissione = *on;
057500160701           When  TBEatb <> *blank;
057600160701             V1Topz = '  RIPRISTINO   ';
057700160701             $Ripristino = *on;
057800160701           Other;
057900160701             V1Topz = '   MODIFICA    ';
058000160701         EndSl;
058100160701
058200160701         // -?Caricamento dati del Codice?
058300160701         clear  dAMV;
058400160701         If  %found(TNTBE01L);
058500160701           dAMV = TBEuni;
058600160701           if  §AMVdti > *zero;
058700160701             wDate_EUR = %date( §AMVdti : *iso );
058800160701             V1Cdti    = %dec( wDate_EUR );
058900160701           endif;
059000160701           if  §AMVdtf > *zero;
059100160701             wDate_EUR = %date( §AMVdtf : *iso );
059200160701             V1Cdtf    = %dec( wDate_EUR );
059300160701           endif;
059400160701         EndIf;
059500160701
059600160701         // -?Impostazione indicatori per abilitazione tasti funzionali?
059700160701         exsr  sr_AbilitFxxD02;
059800160701
059900160701       ENDSR;
060000160701
060100160701       //--------------------------------------------------------------
060200160701       //?Settaggio indicatori nella videata D02 per abilitazione      ?
060300160701       //?  tasti funzionali.                                          ?
060400160701       //--------------------------------------------------------------
060500160701       BEGSR  sr_AbilitFxxD02;
060600160701
060700160701         // -?F3=Fine?
060800160701         F3Attivo  = *on;
060900160701
061000160701         // -?F5=Ripristino?
061100160701         F5Attivo  = (%found(TNTBE01L)   and   TBEatb <> *blank
061200160701                                         and   Not $Protect);
061300160701
061400160701         // -?F6=Conferma?
061500160701         F6Attivo  = (NOT F5Attivo);
061600160701
061700160701         // -?F12=Ritorno?
061800160701         F12Attivo = *on;
061900160701
062000160701         // -?F16=Annullamento?
062100160701         F16Attivo = (%found(TNTBE01L)   and   TBEatb = *blank
062200160701                                         and   Not $Protect);
062300160701
062400160701       ENDSR;
062500160701
062600160701       //--------------------------------------------------------------
062700160701       //?Controllo dati videata D02.                                  ?
062800160701       //--------------------------------------------------------------
062900160701       BEGSR  sr_CtrD02;
063000160701
063100160701         // -?Spegnimento degli indicatori di errore?
063200160701         %subst(IndDspF : 50) = *off;
063300160701
063400160701         clear  dAMV;
063500160701
063600160701         // -?Data Decorrenza NON applicazione V.M.A.?
063700160701         if  V1Cdti = *zero;
063800160701           ErrGenerico = *on;
063900160701           ErrMessage  = *on;
064000160701           PosCurDTI   = *on;
064100160701           V1Dmsg = sk_Msg(05);
064200160701           leavesr;
064300160701         endif;
064400160701
064500160701         clear WLBdat;
064600160701         G08dat = V1Cdti;
064700160701         XSRDA8(WLBdat);
064800160701         if  G08err = *on;
064900160701           ErrGenerico = *on;
065000160701           ErrMessage  = *on;
065100160701           PosCurDTI   = *on;
065200160701           V1Dmsg = sk_Msg(06);
065300160701           leavesr;
065400160701         endif;
065500160701         V1Cdti  = G08dat;
065600160701         §AMVdti = G08inv;
065700160701
065800160701         // -?Data Scadenza NON applicazione V.M.A.?
065900160701         if  V1Cdtf = *zero;
066000160701           ErrGenerico = *on;
066100160701           ErrMessage  = *on;
066200160701           PosCurDTF   = *on;
066300160701           V1Dmsg = sk_Msg(05);
066400160701           leavesr;
066500160701         endif;
066600160701
066700160701         clear WLBdat;
066800160701         G08dat = V1Cdtf;
066900160701         XSRDA8(WLBdat);
067000160701         if  G08err = *on;
067100160701           ErrGenerico = *on;
067200160701           ErrMessage  = *on;
067300160701           PosCurDTF   = *on;
067400160701           V1Dmsg = sk_Msg(06);
067500160701           leavesr;
067600160701         endif;
067700160701         V1Cdtf  = G08dat;
067800160701         §AMVdtf = G08inv;
067900160701
068000160701         // -?Controllo Range di Date Decorrenza/Scadenza?
068100160701         if  §AMVdti > §AMVdtf;
068200160701           ErrGenerico = *on;
068300160701           ErrMessage  = *on;
068400160701           PosCurDTF   = *on;
068500160701           V1Dmsg = sk_Msg(07);
068600160701           leavesr;
068700160701         endif;
068800160701
068900160701       ENDSR;
069000160701
069100160701       //--------------------------------------------------------------
069200160701       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
069300160701       //--------------------------------------------------------------
069400160701       BEGSR  sr_UpdateAll;
069500160701
069600160701         // -?Ciclo di elaborazione per ogni sistema informativo?
069700160701         For  w_SisInf = 1  To  %elem(sk_Libr);
069800160701
069900160701           // -?Apertura degli archivi?
070000160701           if  w_SisInf > 1;
070100160701             exsr  sr_OpenFileTab;
070200160701           endif;
070300160701
070400160701           // -?Aggiornamento tab. "AMV"?
070500160701           exsr  sr_UpdTNTBE;
070600160701
070700160701         EndFor;
070800160701
070900160701       ENDSR;
071000160701
071100160701       //--------------------------------------------------------------
071200160701       //?Apertura dei files tabelle nel sistema informativo impostato.?
071300160701       //--------------------------------------------------------------
071400160701       BEGSR  sr_OpenFileTab;
071500160701
071600160701         // -?Chiusura (eventuale) file tabelle?
071700160701         if  %open(TNTBE01L);
071800160701           close  TNTBE01L;
071900160701         endif;
072000160701
072100160701         // -?Apertura file tabelle?
072200160701         ds_Libr  = sk_Libraries(w_iSystem);
072300160701         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
072400160701         open  TNTBE01L;
072500160701
072600160701       ENDSR;
072700160701
072800160701       //--------------------------------------------------------------
072900160701       //?Aggiornamento records TNTBE00F (tab. "AMV")                  ?
073000160701       //--------------------------------------------------------------
073100160701       BEGSR  sr_UpdTNTBE;
073200160701
073300160701         //? N.B.                                                      ?
073400160701         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
073500160701         //   trasmissione (vedi flag TBEFTR).                        ?
073600160701         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
073700160701         //   subito nello stesso file di entrambi i S.I. - in due    ?
073800160701         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
073900160701
074000160701         // -?RI-aggancio record da aggiornare?
074100160701         //  ?(dopo aver cambiato la libreria del file da aggiornare)?
074200160701
074300160701         // -?Aggancio record di TNTBE00F?
074400160701         //if  w_SisInf > 1;
074500160701           clear  keyTNTBE01;
074600160701           k_TBEcod  = c_Tab;
074700160701           k_TBEke1  = V1Cksc;
074800160701           chain  %kds( keyTNTBE01 )  TNTBE000;
074900160701         //endif;
075000160701
075100160701         //§AMVdti = ......;       ?(già impostata)?
075200160701         //§AMVdtf = ......;       ?(già impostata)?
075300160701         TBEuni = dAMV;
075400160701
075500160701         // -?Aggiornamento tab. "AMV"?
075600160701         SELECT;
075700160701
075800160701           // -?F5=Ripristino?
075900160701           WHEN  $Ripristino;
076000160701             TBEapl = TBXapl;
076100160701             //TBEftt = W1ftt;
076200160701             TBEftt = 'S';
076300160701             clear  TBEflt;
076400160701             if  w_SisInf = 1;
076500160701               TBEftr = 'T';
076600160701             else;
076700160701               TBEftr = 'R';
076800160701             endif;
076900160701             TBEdtr = wDate;
077000160701             select;
077100160701               when  %found(TNTBE01L)  and  TBEatb <> *blank;
077200160701                 clear  TBEatb;
077300160701                 //______________
077400160701                 UPDATE  TNTBE000;
077500160701                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077600160701               // -?Record appena cancellato fisicamente?
077700160701               when  NOT %found(TNTBE01L);
077800160701                 clear  TBEatb;
077900160701                 //______________
078000160701                 WRITE   TNTBE000;
078100160701                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
078200160701             endsl;
078300160701
078400160701           // -?F16=Annullamento?
078500160701           WHEN  $Annullamento;
078600160701             if  %found(TNTBE01L)  and  TBEatb <> 'A';
078700160701               TBEatb = 'A';
078800160701               //TBEftt = W1ftt;
078900160701               TBEftt = 'S';
079000160701               clear  TBEftr;
079100160701               //______________
079200160701               UPDATE  TNTBE000;
079300160701               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079400160701             endif;
079500160701
079600160701           // -?Immissione o Modifica?
079700160701           OTHER;
079800160701             if  NOT %found(TNTBE01L);
079900160701               clear TNTBE000;
080000160701               TBEcod  = k_TBEcod;
080100160701               TBEke1  = k_TBEke1;
080200160701               TBEke2  = k_TBEke2;
080300160701               TBElin  = k_TBElin;
080400160701               TBEsif  = k_TBEsif;
080500160701               TBEuni  = dAMV;
080600160701             endif;
080700160701             clear  TBEatb;
080800160701             TBEapl = TBXapl;
080900160701             //TBEftt = W1ftt;
081000160701             TBEftt = 'S';
081100160701             clear  TBEflt;
081200160701             if  w_SisInf = 1;
081300160701               TBEftr = 'T';
081400160701             else;
081500160701               TBEftr = 'R';
081600160701             endif;
081700160701             TBEdtr = wDate;
081800160701             // -?Aggiornamento record?
081900160701             if  %found(TNTBE01L);
082000160701               //_____________
082100160701               UPDATE TNTBE000;
082200160701               //¯¯¯¯¯¯¯¯¯¯¯¯¯
082300160701             else;
082400160701               //_____________
082500160701               WRITE  TNTBE000;
082600160701               //¯¯¯¯¯¯¯¯¯¯¯¯¯
082700160701             endif;
082800160701
082900160701         ENDSL;
083000160701
083100160701       ENDSR;
083200160701
083300160701       //--------------------------------------------------------------
083400160701       //?Operazioni finali.                                           ?
083500160701       //--------------------------------------------------------------
083600160701       BEGSR  sr_RoutEnd;
083700160701
083800160701         // -?Chiusura funzioni precedentemente aperte?
083900160701         clear  TIBS69ds;
084000160701         I69tla = 'C';
084100160701         tibs69r( tibs69ds :
084200160701                  ds_cnaco : ds_cnind : ds_cnclp : ds_fncls );
084300160701
084400160701         //  ?Uscita?
084500160701         return;
084600160701
084700160701       ENDSR;
084800160701
084900160701      /end-free
085000160701
085100160701       //--------------------------------------------------------------
085200160701       //?Definizione schiere a tempo di compilazione.                 ?
085300160701       //--------------------------------------------------------------
085400160701
085500160701** --?sk_iSystem / sk_Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
085600160701SETRAS  GAITRAGRU FILTRAGRU
085700160701AS888   GAITRAGRPSFILTRAGRPF
085800160701** --?sk_Msg:?Messaggi di Errore?---------------------------------------------*
085900160701Tabella gestibile SOLO da SEDE                                                  1
086000160701Codice cliente obbligatorio                                                     2
086100160701Ammessi SOLO caratteri numerici                                                 3
086200160701Cliente NON presente in anagrafica                                              4
086300160701Data obbligatoria                                                               5
086400160701Data formalmente errata                                                         6
086500160701Data iniziale successiva a quella finale                                        7
