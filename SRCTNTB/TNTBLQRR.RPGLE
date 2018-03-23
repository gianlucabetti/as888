000100160922       //==============================================================
000200160922       //?Gestione tabella "LQR" (Limiti Quantità Ritiri)              ?
000300160922       //==============================================================
000400160922     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000500160922
000600160922       //--------------------------------------------------------------
000700160922       //?Dichiarazione file.                                          ?
000800160922       //--------------------------------------------------------------
000900160922       // -?Tabelle
001000160922     fTNTBE01L  Uf A e           k disk
001100160922
001200160922       // -?Video?
001300160922     fTNTBLQRD  cf   e             workstn
001400160922     f                                     indds(IndDspF)
001500160922     f                                     infds(InfDspF)
001600160922
001700160922       //--------------------------------------------------------------
001800160922       //?Definizione costanti.                                        ?
001900160922       //--------------------------------------------------------------
002000160922       // -?Codice tabella in gestione?
002100160922     d c_Tab           c                   const('LQR')
002200160922
002300160922       // -?Costante per controllo "caratteri solo numerici"?
002400160922     d c_Digits        c                   const('0123456789')
002500160922
002600160922       // -?Tasti funzionali a video?
002700160922     d c_F01           c                   const(x'31')
002800160922     d c_F02           c                   const(x'32')
002900160922     d c_F03           c                   const(x'33')
003000160922     d c_F04           c                   const(x'34')
003100160922     d c_F05           c                   const(x'35')
003200160922     d c_F06           c                   const(x'36')
003300160922     d c_F07           c                   const(x'37')
003400160922     d c_F08           c                   const(x'38')
003500160922     d c_F09           c                   const(x'39')
003600160922     d c_F10           c                   const(x'3A')
003700160922     d c_F11           c                   const(x'3B')
003800160922     d c_F12           c                   const(x'3C')
003900160922     d c_F13           c                   const(x'B1')
004000160922     d c_F14           c                   const(x'B2')
004100160922     d c_F15           c                   const(x'B3')
004200160922     d c_F16           c                   const(x'B4')
004300160922     d c_F17           c                   const(x'B5')
004400160922     d c_F18           c                   const(x'B6')
004500160922     d c_F19           c                   const(x'B7')
004600160922     d c_F20           c                   const(x'B8')
004700160922     d c_F21           c                   const(x'B9')
004800160922     d c_F22           c                   const(x'BA')
004900160922     d c_F23           c                   const(x'BB')
005000160922     d c_F24           c                   const(x'BC')
005100160922     d c_Enter         c                   const(x'F1')
005200160922     d c_RollDown      c                   const(x'F4')
005300160922     d c_RollUp        c                   const(x'F5')
005400160922
005500160922       //--------------------------------------------------------------
005600160922       //?Definizione schiere.                                         ?
005700160922       //--------------------------------------------------------------
005800160922       // -?Messaggi di errore?
005900160922     d Msg             s             78    dim( 8)  ctdata  perrcd( 1)
006000160922
006100160922       //--------------------------------------------------------------
006200160922       //?Definizione aree dati.                                       ?
006300160922       //--------------------------------------------------------------
006400160922       // -?Dati utente?
006500160922     d §AzUte        e ds                  extname(AZUTE00F)
006600160922     d                                     dtaara
006700160922     d §DatiUte      e ds                  extname(dDatiUte)
006800160922     d                                     dtaara
006900160922
007000160922       //--------------------------------------------------------------
007100160922       //?Definizione strutture dati.                                  ?
007200160922       //--------------------------------------------------------------
007300160922       // -?Status ds?
007400160922     d Psds           sds
007500160922     d   PGMname         *proc
007600160922
007700160922       // -?InfDS?
007800160922     d InfDspF         ds
007900160922     d   dsp_aid             369    369a
008000160922     d   sfl_rrn             376    377i 0
008100160922     d   min_nrr             378    379i 0
008200160922     d   num_rcds            380    381i 0
008300160922
008400160922       // -?Indicatori su DspF?
008500160922     d IndDspF         ds                  inz
008600160922         // -?Emissione messaggio di errore?
008700160922     d   ErrMessage                    n   overlay(IndDspF : 28)
008800160922         // -?Posizionamento cursore & segnalazione errore?
008900160922     d   PosCurBNCM                    n   overlay(IndDspF : 50)
009000160922     d   PosCurBLCM                    n   overlay(IndDspF : 51)
009100160922     d   PosCurATTM                    n   overlay(IndDspF : 52)
009200160922     d   PosCurMTCM                    n   overlay(IndDspF : 53)
009300160922         //   -?Riemissione videata?
009400160922     d   ErrGenerico                   n   overlay(IndDspF : 99)
009500160922
009600160922     d WindDspF        ds                  inz
009700160922     d  WindDspFa              1     49    inz(*zero)
009800160922     d  WindDspFb             50     99    inz(*zero)
009900160922
010000160922       // -?Parametri ricevuti?
010100160922     d KPJBA         e ds
010200160922
010300160922       // -?Tabella LQR
010400160922     d dLQR          e ds
010500160922
010600160922       // -?Dati record principale della tabella?
010700160922     d TNTBEds       e ds                  inz  extname(TNTBE00F)
010800160922     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
010900160922     d                                          prefix(TBX:3)
011000160922
011100160922       //--------------------------------------------------------------
011200160922       //?Definizione variabili globali.                               ?
011300160922       //--------------------------------------------------------------
011400160922       // -?Flags booleani?
011500160922     d Fine            s               n   inz
011600160922     d Immissione      s               n   inz
011700160922     d wInzD01         s               n   inz(*on)
011800160922     d wInzW01         s               n   inz(*on)
011900160922
012000160922       // -?Indici di schiera?
012100160922     d xx              s              3  0 inz
012200160922
012300160922       // -?Variabili per la gestione del video?
012400160922     d Video           s              2    inz('D1')
012500160922
012600160922       // -?Variabili di comodo?
012700160922     d wData_EUR       s               d   datfmt(*eur)   inz(*loval)
012800160922
012900160922       //--------------------------------------------------------------
013000160922       //?Definizione prototipi procedure.                             ?
013100160922       //--------------------------------------------------------------
013200160922       // -?Reperimento dati utente?
013300160922     d TIBS34ds      e ds                  inz
013400160922      /copy gaitrasrc/srcProtoPR,TIBS34R
013500160922
013600160922       //--------------------------------------------------------------
013700160922       //?Definizione key-list.                                        ?
013800160922       //--------------------------------------------------------------
013900160922       // -?File TNTBE01L?
014000160922     d keyTNTBE01    e ds                  extname(TNTBE01L : *key)
014100160922     d                                     prefix(k_)   inz
014200160922
014300160922       //--------------------------------------------------------------
014400160922       //?Riepilogo indicatori utilizzati.                             ?
014500160922       //--------------------------------------------------------------
014600160922
014700160922       //--------------------------------------------------------------
014800160922       //?M A I N - L I N E                                            ?
014900160922       //--------------------------------------------------------------
015000160922     c     *Entry        plist
015100160922     c                   parm                    KPJBA
015200160922
015300160922      /free
015400160922
015500160922       //?Operazioni iniziali?
015600160922       exsr RoutInz;
015700160922
015800160922       //?Ciclo di gestione del file video?
015900160922       DOW  not Fine;
016000160922         SELECT;
016100160922
016200160922         //?Gestione videata D1?
016300160922         WHEN  Video = 'D1';
016400160922           exsr GesD01;
016500160922
016600160922         //?Gestione videata W1?
016700160922         WHEN  Video = 'W1';
016800160922           exsr GesW01;
016900160922
017000160922         OTHER;
017100160922           Fine = *on;
017200160922
017300160922         ENDSL;
017400160922       ENDDO;
017500160922
017600160922       //?Operazioni finali?
017700160922       exsr RoutEnd;
017800160922
017900160922       //--------------------------------------------------------------
018000160922       //?Operazioni iniziali.                                         ?
018100160922       //--------------------------------------------------------------
018200160922       BEGSR RoutInz;
018300160922
018400160922         //?Impostazione chiusura?
018500160922         *inLR = *on;
018600160922
018700160922         //?Reperimento dati job?
018800160922         exsr DatiJob;
018900160922
019000160922         //?Aggancio dati generali della tabella in esame?
019100160922         clear k_TBEcod;
019200160922         k_TBEke1 = *zero;
019300160922         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
019400160922                = c_Tab;
019500160922         clear k_TBEke2;
019600160922         clear k_TBElin;
019700160922         k_TBEsif = KNSIF;
019800160922         chain %kds(keytntbe01) TNTBE01L;
019900160922         IF  not %found(TNTBE01L);
020000160922           clear k_TBEsif;
020100160922           chain %kds(keytntbe01) TNTBE01L;
020200160922         ENDIF;
020300160922         IF  %found(TNTBE01L);
020400160922           xTNTBEds = TNTBEds;
020500160922         ELSE;
020600160922           clear xTNTBEds;
020700160922         ENDIF;
020800160922
020900160922         //?Pulizia / impostazione iniziale dei campi chiave?
021000160922         clear keytntbe01;
021100160922         k_TBEcod = c_Tab;
021200160922
021300160922       ENDSR;
021400160922
021500160922       //--------------------------------------------------------------
021600160922       //?Reperimento Dati del job (Utente/Operativi).                 ?
021700160922       //--------------------------------------------------------------
021800160922       BEGSR DatiJob;
021900160922
022000160922         in(e) §AzUte;
022100160922         IF  NOT %error;
022200160922           in(e) §DatiUte;
022300160922         ENDIF;
022400160922         IF  %error or RSut = *blank;
022500160922           tibs34r ( tibs34ds );
022600160922           in §AzUte;
022700160922           in §DatiUte;
022800160922         ENDIF;
022900160922
023000160922       ENDSR;
023100160922
023200160922       //--------------------------------------------------------------
023300160922       //?Gestione videata D01.                                        ?
023400160922       //--------------------------------------------------------------
023500160922       BEGSR GesD01;
023600160922
023700160922       //?Inizializzazione videata?
023800160922         IF  wInzD01;
023900160922           exsr InzD01;
024000160922           wInzD01 = *off;
024100160922         ENDIF;
024200160922
024300160922       //?Emissione Videata?
024400160922         EXFMT TBLQRD1;
024500160922         reset ErrGenerico;
024600160922         reset ErrMessage;
024700160922         clear V01msg;
024800160922
024900160922         SELECT;
025000160922
025100160922       //?F3=Fine?
025200160922         WHEN  dsp_aid = c_F03;
025300160922           Fine = *on;
025400160922
025500160922       //?F6=Conferma?
025600160922         WHEN  dsp_aid = c_F06;
025700160922           exsr ContrD01;
025800160922           IF  ErrGenerico;
025900160922             leavesr;
026000160922           ENDIF;
026100160922           Video = 'W1';
026200160922           reset wInzW01;
026300160922
026400160922       //?Invio?
026500160922         OTHER;
026600160922         //?Controlli
026700160922           exsr ContrD01;
026800160922           IF  ErrGenerico;
026900160922             leavesr;
027000160922           ENDIF;
027100160922
027200160922         ENDSL;
027300160922
027400160922       ENDSR;
027500160922
027600160922       //--------------------------------------------------------------
027700160922       //?Inizializzazione videata D01.                                ?
027800160922       //--------------------------------------------------------------
027900160922       BEGSR InzD01;
028000160922
028100160922         reset Immissione;
028200160922
028300160922       //?Spegnimento degli indicatori di errore?
028400160922         %subst(IndDspF : 50) = *off;
028500160922
028600160922       //?Pulizia videata?
028700160922         clear V01bncm;
028800160922         clear V01bncf;
028900160922         clear V01blcm;
029000160922         clear V01blcf;
029100160922         clear V01attm;
029200160922         clear V01attf;
029300160922         clear V01mtcm;
029400160922         clear V01mtcf;
029500160922
029600160922       //?Reperimento dati della tabella?
029700160922         k_TBEke1 = '1';
029800160922         chain %kds(KeyTNTBE01) TNTBE01L;
029900160922
030000160922       //?Impostazione testata?
030100160922         IF  not %found(TNTBE01L);
030200160922           V01opz = '  INSERIMENTO  ';
030300160922           Immissione = *on;
030400160922         ELSE;
030500160922           V01opz = '   MODIFICA    ';
030600160922         ENDIF;
030700160922
030800160922       //?Caricamento dati Codice?
030900160922         IF  %found(TNTBE01L);
031000160922           exsr RieD01;
031100160922         ENDIF;
031200160922
031300160922       ENDSR;
031400160922
031500160922       //--------------------------------------------------------------
031600160922       //?Caricamento dati della tabella.
031700160922       //--------------------------------------------------------------
031800160922       BEGSR RieD01;
031900160922
032000160922         dLQR = TBEuni;
032100160922
032200160922         V01bncm = §LQRbncm;
032300160922         V01bncf = §LQRbncf;
032400160922         V01blcm = §LQRblcm;
032500160922         V01blcf = §LQRblcf;
032600160922         V01attm = §LQRattm;
032700160922         V01attf = §LQRattf;
032800160922         V01mtcm = §LQRmtcm;
032900160922         V01mtcf = §LQRmtcf;
033000160922
033100160922       ENDSR;
033200160922
033300160922       //--------------------------------------------------------------
033400160922       //?Controllo dati nella videata D01.
033500160922       //--------------------------------------------------------------
033600160922       BEGSR ContrD01;
033700160922
033800160922         WindDspF = IndDspF;
033900160922         reset WindDspFb;
034000160922         IndDspF  = WindDspF;
034100160922
034200160922         IF  V01bncm = 0 or V01bncf = 0;
034300160922           ErrGenerico = *on;
034400160922           ErrMessage  = *on;
034500160922           PosCurBNCM  = *on;
034600160922           V01msg = Msg(01);
034700160922           leavesr;
034800160922         ENDIF;
034900160922
035000160922         IF  V01blcm = 0 or V01blcf = 0;
035100160922           ErrGenerico = *on;
035200160922           ErrMessage  = *on;
035300160922           PosCurBLCM  = *on;
035400160922           V01msg = Msg(01);
035500160922           leavesr;
035600160922         ENDIF;
035700160922
035800160922         IF  V01attm = 0 or V01attf = 0;
035900160922           ErrGenerico = *on;
036000160922           ErrMessage  = *on;
036100160922           PosCurATTM  = *on;
036200160922           V01msg = Msg(01);
036300160922           leavesr;
036400160922         ENDIF;
036500160922
036600160922         IF  V01mtcm = 0 or V01mtcf = 0;
036700160922           ErrGenerico = *on;
036800160922           ErrMessage  = *on;
036900160922           PosCurMTCM  = *on;
037000160922           V01msg = Msg(01);
037100160922           leavesr;
037200160922         ENDIF;
037300160922
037400160922       //?Il valore forzabile non può essere maggiore del valore massimo?
037500160922         IF  V01bncm < V01bncf;
037600160922           ErrGenerico = *on;
037700160922           ErrMessage  = *on;
037800160922           PosCurBNCM  = *on;
037900160922           V01msg = Msg(02);
038000160922           leavesr;
038100160922         ENDIF;
038200160922
038300160922         IF  V01blcm < V01blcf;
038400160922           ErrGenerico = *on;
038500160922           ErrMessage  = *on;
038600160922           PosCurBLCM  = *on;
038700160922           V01msg = Msg(02);
038800160922           leavesr;
038900160922         ENDIF;
039000160922
039100160922         IF  V01attm < V01attf;
039200160922           ErrGenerico = *on;
039300160922           ErrMessage  = *on;
039400160922           PosCurATTM  = *on;
039500160922           V01msg = Msg(02);
039600160922           leavesr;
039700160922         ENDIF;
039800160922
039900160922         IF  V01mtcm < V01mtcf;
040000160922           ErrGenerico = *on;
040100160922           ErrMessage  = *on;
040200160922           PosCurMTCM  = *on;
040300160922           V01msg = Msg(02);
040400160922           leavesr;
040500160922         ENDIF;
040600160922
040700160922       ENDSR;
040800160922
040900160922       //--------------------------------------------------------------
041000160922       //?Gestione videata W01.
041100160922       //--------------------------------------------------------------
041200160922       BEGSR GesW01;
041300160922
041400160922       //?Inizializzazione videata?
041500160922         IF  wInzW01;
041600160922           exsr InzW01;
041700160922           wInzW01 = *off;
041800160922         ENDIF;
041900160922
042000160922       //?Emissione window?
042100160922         IF  TBXftt = 'S';
042200160922           exfmt TBLQRW1;
042300160922           reset ErrGenerico;
042400160922           reset ErrMessage;
042500160922           clear V01msg;
042600160922         ELSE;
042700160922           dsp_aid = c_F06;
042800160922         ENDIF;
042900160922
043000160922         SELECT;
043100160922
043200160922         //?F12=Ritorno?
043300160922         WHEN  dsp_aid = c_F12;
043400160922           Video = 'D1';
043500160922           leavesr;
043600160922
043700160922         //?F6=Aggiornamento TNTBE00F?
043800160922         WHEN  dsp_aid = c_F06;
043900160922           exsr UpdTNTBE;
044000160922           //?Esco dal pgm?
044100160922            IF  not ErrGenerico;
044200160922              exsr Routend;
044300160922            ENDIF;
044400160922
044500160922          ENDSL;
044600160922
044700160922       ENDSR;
044800160922
044900160922       //--------------------------------------------------------------
045000160922       //?Inizializzazione videata W01                                 ?
045100160922       //--------------------------------------------------------------
045200160922       BEGSR InzW01;
045300160922
045400160922         clear TBLQRW1;
045500160922
045600160922       //?Immissione?
045700160922         IF  Immissione;
045800160922           W1ftt = TBXftt;
045900160922         ENDIF;
046000160922
046100160922       //?Modifica?
046200160922         IF  not Immissione;
046300160922           W1ftt = TBEftt;
046400160922           W1flt  = TBEflt;
046500160922           W1ftr  = TBEftr;
046600160922           IF  TBEdtr <> *zero;
046700160922             wData_Eur = %date( TBEdtr : *iso );
046800160922             W1dtr     = %dec( wData_Eur );
046900160922           ENDIF;
047000160922         ENDIF;
047100160922
047200160922       ENDSR;
047300160922
047400160922       //--------------------------------------------------------------
047500160922       //?Aggiornamento record TNTBE00F.
047600160922       //--------------------------------------------------------------
047700160922       BEGSR UpdTNTBE;
047800160922
047900160922       //?Aggancio record di TNTBE00F?
048000160922         k_TBEcod  = c_Tab;
048100160922         k_TBEke1  = '1';
048200160922         clear k_TBEke2;
048300160922         clear k_TBElin;
048400160922         clear k_TBEsif;
048500160922
048600160922         chain %kds(KeyTNTBE01) TNTBE01L;
048700160922
048800160922         exsr  RieLQR;
048900160922
049000160922         TBEuni = dLQR;
049100160922
049200160922       //?Aggiornamento tabella?
049300160922         IF  not %found(TNTBE01L);
049400160922           clear TNTBE000;
049500160922           TBEcod  = k_TBEcod;
049600160922           TBEke1  = k_TBEke1;
049700160922           TBEke2  = k_TBEke2;
049800160922           TBElin  = k_TBElin;
049900160922           TBEsif  = k_TBEsif;
050000160922           TBEuni  = dLQR;
050100160922         ENDIF;
050200160922
050300160922         TBEapl = TBXapl;
050400160922         clear  TBEatb;
050500160922         TBEftt = W1ftt;
050600160922         clear  TBEflt;
050700160922         clear  TBEftr;
050800160922
050900160922         IF  %found(TNTBE01L);
051000160922           //_____________
051100160922           UPDATE TNTBE000;
051200160922           //¯¯¯¯¯¯¯¯¯¯¯¯¯
051300160922         ELSE;
051400160922           //_____________
051500160922           WRITE  TNTBE000;
051600160922           //¯¯¯¯¯¯¯¯¯¯¯¯¯
051700160922         ENDIF;
051800160922
051900160922       ENDSR;
052000160922
052100160922       //--------------------------------------------------------------
052200160922       //?Caricamento dati in tabella.                                 ?
052300160922       //--------------------------------------------------------------
052400160922       BEGSR RieLQR;
052500160922
052600160922         clear dLQR;
052700160922
052800160922         §LQRbncm = V01bncm;
052900160922         §LQRbncf = V01bncf;
053000160922         §LQRblcm = V01blcm;
053100160922         §LQRblcf = V01blcf;
053200160922         §LQRattm = V01attm;
053300160922         §LQRattf = V01attf;
053400160922         §LQRmtcm = V01mtcm;
053500160922         §LQRmtcf = V01mtcf;
053600160922
053700160922       ENDSR;
053800160922
053900160922       //--------------------------------------------------------------
054000160922       //?Operazioni finali.                                           ?
054100160922       //--------------------------------------------------------------
054200160922       BEGSR RoutEnd;
054300160922
054400160922         return;
054500160922
054600160922       ENDSR;
054700160922
054800160922      /end-free
054900160922
055000160922** -?Msg:?Messaggi di Errore?---------------------------------------------------*
055100160922Immettere i tutti i valori.                                                     1
055200160922Il valore massimo non può essere inferiore al valore forzabile                  2
