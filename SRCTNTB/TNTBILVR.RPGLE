000100130206       //==============================================================
000200130206       //?Gestione tabella "ILV" (RootFolder per immagini LdV)         ?
000300130206       //==============================================================
000400130206
000500130206       //--------------------------------------------------------------
000600130206       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130206       //--------------------------------------------------------------
000800130206
000900130206     /*PRM  dbgview(*source)
001000130206     /*END
001100130206
001200130206       //--------------------------------------------------------------
001300130206       //?Specifiche di controllo.                                     ?
001400130206       //--------------------------------------------------------------
001500130206
001600130206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130206     h dftactgrp(*no)
001800130206     h bnddir('TRUL')
001900130206
002000130206       //--------------------------------------------------------------
002100130206       //?Dichiarazione file.                                          ?
002200130206       //--------------------------------------------------------------
002300130206
002400130206       // -?Tabelle?
002500130206     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
002600130206     f                                     usropn
002700130206
002800130206       // -?Video?
002900130206     fTNTBILVD  cf   e             workstn
003000130206     f                                     indds(IndDspF)
003100130206     f                                     infds(InfDspF)
003200130206
003300130206       //--------------------------------------------------------------
003400130206       //?Definizione costanti.                                        ?
003500130206       //--------------------------------------------------------------
003600130206
003700130206       // -?Codice tabella in gestione?
003800130206     d c_Tab           c                   const('ILV')
003900130206
004000130206       // -?TBEke1 previsti?
004100130206     d c_Setras        c                   const('SETRAS')
004200130206     d c_As888         c                   const('AS888')
004300130206       // -?TBEke2 previsti?
004400130206     d c_Linea         c                   const('LINEA')
004500130206     d c_BackUp        c                   const('BACKUP')
004600130206
004700130206       // -?Tasti funzionali a video?
004800130206     d c_F01           c                   const(x'31')
004900130206     d c_F02           c                   const(x'32')
005000130206     d c_F03           c                   const(x'33')
005100130206     d c_F04           c                   const(x'34')
005200130206     d c_F05           c                   const(x'35')
005300130206     d c_F06           c                   const(x'36')
005400130206     d c_F07           c                   const(x'37')
005500130206     d c_F08           c                   const(x'38')
005600130206     d c_F09           c                   const(x'39')
005700130206     d c_F10           c                   const(x'3A')
005800130206     d c_F11           c                   const(x'3B')
005900130206     d c_F12           c                   const(x'3C')
006000130206     d c_F13           c                   const(x'B1')
006100130206     d c_F14           c                   const(x'B2')
006200130206     d c_F15           c                   const(x'B3')
006300130206     d c_F16           c                   const(x'B4')
006400130206     d c_F17           c                   const(x'B5')
006500130206     d c_F18           c                   const(x'B6')
006600130206     d c_F19           c                   const(x'B7')
006700130206     d c_F20           c                   const(x'B8')
006800130206     d c_F21           c                   const(x'B9')
006900130206     d c_F22           c                   const(x'BA')
007000130206     d c_F23           c                   const(x'BB')
007100130206     d c_F24           c                   const(x'BC')
007200130206     d c_Enter         c                   const(x'F1')
007300130206     d c_RollDown      c                   const(x'F4')
007400130206     d c_RollUp        c                   const(x'F5')
007500130206
007600130206       // -?Costanti per la definizione delle schiere con i nomi?
007700130206       //  ?degli iSeries da elaborare e delle relative librerie?
007800130206     d c_NrSyst        c                   const(2)
007900130206     d c_NrLibr        c                   const(2)
008000130206
008100130206       //--------------------------------------------------------------
008200130206       //?Definizione schiere.                                         ?
008300130206       //--------------------------------------------------------------
008400130206
008500130206       // -?iSeries  &  Librerie con entrambi i file tabelle?
008600130206     d $iSystem        s                   like(currSysNetA)
008700130206     d                                     dim(c_NrSyst)
008800130206     d                                     ctdata   perrcd( 1)
008900130206     d $Libraries      s                   like(ds_Libr)
009000130206     d                                     dim(c_NrSyst)
009100130206     d                                     alt($iSystem)
009200130206
009300130206       // -?Messaggi di errore?
009400130206     d $Msg            s             78    dim( 5)  ctdata  perrcd( 1)
009500130206
009600130206       //--------------------------------------------------------------
009700130206       //?Definizione aree dati.                                       ?
009800130206       //--------------------------------------------------------------
009900130206
010000130206       // -?Dati utente?
010100130206     d §AzUte        e ds                  extname(AZUTE00F)
010200130206     d                                     dtaara
010300130206     d §DatiUte      e ds                  extname(dDatiUte)
010400130206     d                                     dtaara
010500130206
010600130206       //--------------------------------------------------------------
010700130206       //?Definizione strutture dati.                                  ?
010800130206       //--------------------------------------------------------------
010900130206
011000130206       // -?Status ds?
011100130206     d Status         sds
011200130206     d   SDSpgm          *proc
011300130206
011400130206       // -?InfDS?
011500130206     d InfDspF         ds
011600130206     d   dsp_aid             369    369a
011700130206
011800130206       // -?Indicatori su DspF?
011900130206     d IndDspF         ds                  inz
012000130206         // -?Abilitazione tasti funzionali?
012100130206     d   F5Attivo                      n   overlay(IndDspF : 05)
012200130206     d   F6Attivo                      n   overlay(IndDspF : 06)
012300130206     d   F11Attivo                     n   overlay(IndDspF : 11)
012400130206     d*//F16Attivo                     n   overlay(IndDspF : 16)
012500130206         // -?Emissione messaggio di errore?
012600130206     d   ErrMessage                    n   overlay(IndDspF : 28)
012700130206         // -?Posizionamento cursore & segnalazione errore?
012800130206     d   PosCurSeLnOk                  n   overlay(IndDspF : 51)
012900130206     d   PosCurSeLnSc                  n   overlay(IndDspF : 52)
013000130206     d   PosCurSeBkOk                  n   overlay(IndDspF : 53)
013100130206     d   PosCurSeBkSc                  n   overlay(IndDspF : 54)
013200130206     d   PosCurA8LnOk                  n   overlay(IndDspF : 55)
013300130206     d   PosCurA8LnSc                  n   overlay(IndDspF : 56)
013400130206         //   -?Riemissione videata?
013500130206     d   ErrGenerico                   n   overlay(IndDspF : 99)
013600130206
013700130206       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
013800130206     d ds_Libr         ds                  inz
013900130206     d   $Libr                       10    dim(c_NrLibr) inz
014000130206
014100130206       // -?Parametri ricevuti?
014200130206     d KPJBA         e ds
014300130206
014400130206       // -?Dati record principale della tabella?
014500130206     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014600130206     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014700130206     d                                          prefix(TBX:3)
014800130206
014900130206       // -?Tabelle usate:?
015000130206       // ·?"ILV" = RootFolders per immagini LdV?
015100130206     d dILVsetrasLin e ds                  extname(dILV)  qualified
015200130206     d                                     inz
015300130206     d dILVsetrasBkUpe ds                  extname(dILV)  qualified
015400130206     d                                     inz
015500130206     d dILVas888Lin  e ds                  extname(dILV)  qualified
015600130206     d                                     inz
015700130206
015800130206       //--------------------------------------------------------------
015900130206       //?Definizione variabili globali.                               ?
016000130206       //--------------------------------------------------------------
016100130206
016200130206       // -?Flags booleani?
016300130206     d $Fine           s               n   inz
016400130206     d $InzD02         s               n   inz(*on)
016500130206
016600130206       // -?Indici di schiera?
016700130206     d xx              s              3  0 inz
016800130206
016900130206       // -?Variabili per la gestione del video?
017000130206     d $Video          s              2    inz('D2')
017100130206
017200130206       // -?Nome del sistema?
017300130206     d currSysNeta     s              8a   inz
017400130206
017500130206       // -?Nome esteso Libreria/File dei 2 file tabella?
017600130206     d wLibFile        s             21a   inz
017700130206
017800130206       // -?Flag di Annullamento dei 4 record?
017900130206     d wTBEatbSetLin   s                   like(TBEatb)   inz
018000130206     d wTBEatbSetBkUp  s                   like(TBEatb)   inz
018100130206     d wTBEatbAS8Lin   s                   like(TBEatb)   inz
018200130206
018300130206       // -?Campi di comodo?
018400130206     d w_iSystem       s              1  0 inz
018500130206     d w_SisInf        s              3  0 inz
018600130206     d wDate           s              8  0 inz
018700130206     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
018800130206
018900130206       //--------------------------------------------------------------
019000130206       //?Definizione prototipi procedure.                             ?
019100130206       //--------------------------------------------------------------
019200130206
019300130206       // -?Reperimento NETA sistema AS/400 corrente?
019400130206      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019500130206
019600130206       // -?Reperimento dati utente?
019700130206     d TIBS34ds      e ds
019800130206      /copy gaitrasrc/srcProtoPR,TIBS34R
019900130206
020000130206       //--------------------------------------------------------------
020100130206       //?Definizione key-list.                                        ?
020200130206       //--------------------------------------------------------------
020300130206
020400130206       // -?File TNTBE01L?
020500130206     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020600130206     d                                     prefix(k_)   inz
020700130206
020800130206       //--------------------------------------------------------------
020900130206       //?M A I N - L I N E                                            ?
021000130206       //--------------------------------------------------------------
021100130206
021200130206     c     *Entry        plist
021300130206     c                   parm                    KPJBA
021400130206
021500130206      /free
021600130206
021700130206       // -?Operazioni iniziali?
021800130206       exsr  sr_RoutInz;
021900130206
022000130206       // -?Ciclo di gestione del file video?
022100130206       DOW  $Fine = *off;
022200130206
022300130206         select;
022400130206           // -?Gestione RootFolder per immagini LdV?
022500130206           when $Video = 'D2';
022600130206             exsr  sr_GesD02;
022700130206           // -?? ? ??
022800130206           other;
022900130206             $Fine = *on;
023000130206         endsl;
023100130206
023200130206       ENDDO;
023300130206
023400130206       // -?Operazioni finali?
023500130206       exsr  sr_RoutEnd;
023600130206
023700130206       //--------------------------------------------------------------
023800130206       //?Operazioni iniziali.                                         ?
023900130206       //--------------------------------------------------------------
024000130206       BEGSR  *InzSr;
024100130206
024200130206         // -?Reperimento dati job?
024300130206         exsr  sr_DatiJob;
024400130206
024500130206         // -?Impostazione nome programma a video?
024600130206         V1Tpgm = SDSpgm;
024700130206
024800130206         // -?Reperimento data odierna?
024900130206         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025000130206                               : 1 : 8 ) );
025100130206
025200130206       ENDSR;
025300130206
025400130206       //--------------------------------------------------------------
025500130206       //?Operazioni iniziali.                                         ?
025600130206       //--------------------------------------------------------------
025700130206       BEGSR  sr_RoutInz;
025800130206
025900130206         // -?Impostazione chiusura?
026000130206         *inLR = *on;
026100130206
026200130206         // -?Verifica del sistema AS/400 corrente?
026300130206         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
026400130206           $Fine = *on;
026500130206           leavesr;
026600130206         endif;
026700130206
026800130206         // -?Impostazione elenco librerie in cui gestire le tabelle?
026900130206         //  ?(a seconda del sistema in cui si stà lavorando)?
027000130206         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
027100130206         if  w_iSystem = *zero;
027200130206           $Fine = *on;
027300130206           leavesr;
027400130206         endif;
027500130206
027600130206         // -?1ª apertura file TNTBE01L del 1° S.I. (sede)?
027700130206         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
027800130206           w_SisInf = 1;
027900130206           exsr  sr_OpenFile;
028000130206         endif;
028100130206
028200130206         // -?Aggancio dati generali della tabella in esame?
028300130206         clear  k_TBEcod;
028400130206         k_TBEke1 = *zero;
028500130206         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
028600130206                = c_Tab;
028700130206         clear  k_TBEke2;
028800130206         clear  k_TBElin;
028900130206         k_TBEsif = KNSIF;
029000130206         chain(n)  %kds(k05tntbe01)  TNTBE000;
029100130206         if  not  %found(TNTBE01L);
029200130206           clear  k_TBEsif;
029300130206           chain(n)  %kds(k05tntbe01)  TNTBE000;
029400130206         endif;
029500130206         if  %found(TNTBE01L);
029600130206           xTNTBEds = TNTBEds;
029700130206         else;
029800130206           clear  xTNTBEds;
029900130206         endif;
030000130206
030100130206         // -?Impostazione iniziale / pulizia dei campi chiave?
030200130206         clear  k05tntbe01;
030300130206         k_TBEcod = c_Tab;
030400130206
030500130206         // -?Reperimento dati (a chiave "fissa")?
030600130206         clear  dILVsetrasLin;
030700130206         clear  dILVsetrasBkUp;
030800130206         clear  dILVas888Lin;
030900130206         clear  wTBEatbSetLin;
031000130206         clear  wTBEatbSetBkUp;
031100130206         clear  wTBEatbAs8Lin;
031200130206
031300130206         k_TBEke1 = c_Setras;
031400130206         k_TBEke2 = c_Linea;
031500130206         chain  %kds(k05tntbe01)  TNTBE000;
031600130206         if  %found(TNTBE01L);
031700130206           wTBEatbSetLin = TBEatb;
031800130206           if  TBEatb = *blank;
031900130206             dILVsetrasLin  = TBEuni;
032000130206           endif;
032100130206         endif;
032200130206
032300130206         k_TBEke1 = c_Setras;
032400130206         k_TBEke2 = c_BackUp;
032500130206         chain  %kds(k05tntbe01)  TNTBE000;
032600130206         if  %found(TNTBE01L);
032700130206           wTBEatbSetBkUp = TBEatb;
032800130206           if  TBEatb = *blank;
032900130206             dILVsetrasBkUp = TBEuni;
033000130206           endif;
033100130206         endif;
033200130206
033300130206         k_TBEke1 = c_As888;
033400130206         k_TBEke2 = c_Linea;
033500130206         chain  %kds(k05tntbe01)  TNTBE000;
033600130206         if  %found(TNTBE01L);
033700130206           wTBEatbAs8Lin = TBEatb;
033800130206           if  TBEatb = *blank;
033900130206             dILVas888Lin   = TBEuni;
034000130206           endif;
034100130206         endif;
034200130206
034300130206       ENDSR;
034400130206
034500130206       //--------------------------------------------------------------
034600130206       //?Reperimento Dati del job (Utente/Operativi).                 ?
034700130206       //--------------------------------------------------------------
034800130206       BEGSR  sr_DatiJob;
034900130206
035000130206         in(E) §AzUte;
035100130206         if NOT %error;
035200130206           in(E) §DatiUte;
035300130206         endif;
035400130206         if %error or RSut = *blanks;
035500130206           clear TIBS34ds;
035600130206           tibs34r ( tibs34ds );
035700130206           in §AzUte;
035800130206           in §DatiUte;
035900130206         endif;
036000130206
036100130206       ENDSR;
036200130206
036300130206       //--------------------------------------------------------------
036400130206       //?Apertura dei files tabelle nel sistema informativo impostato.?
036500130206       //--------------------------------------------------------------
036600130206       BEGSR  sr_OpenFile;
036700130206
036800130206         // -?Chiusura (eventuale) file tabelle?
036900130206         if  %open(TNTBE01L);
037000130206           close  TNTBE01L;
037100130206         endif;
037200130206
037300130206         // -?Apertura file tabelle?
037400130206         ds_Libr  = $Libraries(w_iSystem);
037500130206         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
037600130206         open  TNTBE01L;
037700130206
037800130206       ENDSR;
037900130206
038000130206       //--------------------------------------------------------------
038100130206       //?Gestione videata D02 (unica).                                ?
038200130206       //--------------------------------------------------------------
038300130206       BEGSR  sr_GesD02;
038400130206
038500130206         // -?Inizializzazione videata?
038600130206         if  $InzD02 = *on;
038700130206           exsr  sr_InzD02;
038800130206           $InzD02 = *off;
038900130206         endif;
039000130206
039100130206         // -?Emissione Testata?
039200130206         write  TBILVT01;
039300130206
039400130206         // -?Emissione videata?
039500130206         exfmt  TBILVD02;
039600130206
039700130206         clear  V1Dmsg;
039800130206         reset  ErrMessage;
039900130206         reset  ErrGenerico;
040000130206
040100130206         SELECT;
040200130206
040300130206           // -?F3=Fine?
040400130206           WHEN  dsp_aid = c_F03;
040500130206             unlock  TNTBE01L;
040600130206             $Fine = *on;
040700130206
040800130206           // -?F11=Inversione Linea-BackUp?
040900130206           WHEN  dsp_aid = c_F11;
041000130206             exsr  sr_Inversione;
041100130206
041200130206           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
041300130206           OTHER;
041400130206             // -?Controlli eseguiti NON se F16=Annullamento?
041500130206             if  dsp_aid <> c_F16;
041600130206               exsr  sr_CtrD02;
041700130206             endif;
041800130206             if  ErrGenerico;
041900130206               leavesr;
042000130206             endif;
042100130206             // -?Aggiornamento?
042200130206             if  dsp_aid = c_F05   or
042300130206                 dsp_aid = c_F06   or
042400130206                 dsp_aid = c_F16;
042500130206               exsr  sr_UpdateTBE;
042600130206               select;
042700130206                 // -?Rilevato errore in fase di aggiornamento?
042800130206                 when  ErrGenerico = *on;
042900130206                   leavesr;
043000130206                 // -?Uscita altrimenti?
043100130206                 other;
043200130206                   $Fine = *on;
043300130206               endsl;
043400130206             endif;
043500130206
043600130206         ENDSL;
043700130206
043800130206       ENDSR;
043900130206
044000130206       //--------------------------------------------------------------
044100130206       //?Inizializzazione videata D02.                                ?
044200130206       //--------------------------------------------------------------
044300130206       BEGSR  sr_InzD02;
044400130206
044500130206         F5Attivo  = *off;
044600130206         F6Attivo  = *off;
044700130206         F11Attivo = *off;
044800130206
044900130206         // -?Spegnimento degli indicatori di errore?
045000130206         %subst(IndDspF : 50) = *off;
045100130206
045200130206         // -?Pulizia videata?
045300130206         clear  TBILVD02;
045400130206
045500130206         // -?Caricamento dati a video?
045600130206         if  wTBEatbSetLin  = *blank  and  dILVsetrasLin  <> *blank;
045700130206           V1CSeLnOk = dILVsetrasLin.§ILVimgOk;
045800130206           V1CSeLnSc = dILVsetrasLin.§ILVimgSca;
045900130206         endif;
046000130206         if  wTBEatbSetBkUp = *blank  and  dILVsetrasBkUp <> *blank;
046100130206           V1CSeBkOk = dILVsetrasBkUp.§ILVimgOk;
046200130206           V1CSeBkSc = dILVsetrasBkUp.§ILVimgSca;
046300130206         endif;
046400130206         if  wTBEatbAs8Lin  = *blank  and  dILVas888Lin   <> *blank;
046500130206           V1CA8LnOk = dILVas888Lin.§ILVimgOk;
046600130206           V1CA8LnSc = dILVas888Lin.§ILVimgSca;
046700130206         endif;
046800130206
046900130206         // -?Impostazione indicatori per abilitazione tasti funzionali?
047000130206         exsr  sr_AbilitFxxD02;
047100130206
047200130206       ENDSR;
047300130206
047400130206       //--------------------------------------------------------------
047500130206       //?Settaggio indicatori nella videata D02 per abilitazione      ?
047600130206       //?  tasti funzionali.                                          ?
047700130206       //--------------------------------------------------------------
047800130206       BEGSR  sr_AbilitFxxD02;
047900130206
048000130206         // -?F3=Fine (sempre abilitato)?
048100130206         //F3Attivo  = *on;
048200130206
048300130206         // -?F5=Ripristino?
048400130206         F5Attivo  = (wTBEatbSetLin  <> *blank  and
048500130206                      wTBEatbSetBkUp <> *blank  and
048600130206                      wTBEatbAs8Lin  <> *blank);
048700130206
048800130206         // -?F6=Conferma?
048900130206         F6Attivo  = (NOT F5Attivo);
049000130206
049100130206         // -?F11 = Inversione Linea-BackUp?
049200130206         F11Attivo = (V1CSeLnOk <> *blank  or  V1CSeLnSc <> *blank  or
049300130206                      V1CSeBkOk <> *blank  or  V1CSeBkSc <> *blank);
049400130206
049500130206         // -?F16=Annullamento?
049600130206         //F16Attivo = ( dILVsetrasLin  <> *blank  or
049700130206         //              dILVsetrasBkUp <> *blank);
049800130206
049900130206       ENDSR;
050000130206
050100130206       //--------------------------------------------------------------
050200130206       //?Controllo dati videata D02.                                  ?
050300130206       //--------------------------------------------------------------
050400130206       BEGSR  sr_CtrD02;
050500130206
050600130206         // -?Spegnimento degli indicatori di errore?
050700130206         %subst(IndDspF : 50) = *off;
050800130206
050900130206         // -?Eliminazione *blanks a sinistra?
051000130206         if  V1CSeLnOk <> *blank;
051100130206           V1CSeLnOk = %triml( V1CSeLnOk );
051200130206         endif;
051300130206         if  V1CSeLnSc <> *blank;
051400130206           V1CSeLnSc = %triml( V1CSeLnSc );
051500130206         endif;
051600130206         if  V1CSeBkOk <> *blank;
051700130206           V1CSeBkOk = %triml( V1CSeBkOk );
051800130206         endif;
051900130206         if  V1CSeBkSc <> *blank;
052000130206           V1CSeBkSc = %triml( V1CSeBkSc );
052100130206         endif;
052200130206         if  V1CA8LnOk <> *blank;
052300130206           V1CA8LnOk = %triml( V1CA8LnOk );
052400130206         endif;
052500130206         if  V1CA8LnSc <> *blank;
052600130206           V1CA8LnSc = %triml( V1CA8LnSc );
052700130206         endif;
052800130206
052900130206         // -?RootFolders obbligatori/errati?
053000130206         Select;
053100130206
053200130206           // -?SETRAS - Immagini in Linea - Ok?
053300130206           When  V1CSeLnOk = *blank;
053400130206             V1Dmsg = $Msg(01);
053500130206             PosCurSeLnOk = *on;
053600130206             ErrMessage   = *on;
053700130206             ErrGenerico  = *on;
053800130206             leavesr;
053900130206           When  %subst( %triml(V1CSeLnOk) : 1 : 2 ) <> '\\';
054000130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
054100130206             PosCurSeLnOk = *on;
054200130206             ErrMessage   = *on;
054300130206             ErrGenerico  = *on;
054400130206             leavesr;
054500130206           When  %subst( V1CSeLnOk : %len( %trimr(V1CSeLnOk) ) : 2 )
054600130206                 <> '\ ';
054700130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
054800130206             PosCurSeLnOk = *on;
054900130206             ErrMessage   = *on;
055000130206             ErrGenerico  = *on;
055100130206             leavesr;
055200130206           When  %checkr( '\ ' : V1CSeLnOk ) <= 3  or
055300130206                 %subst( V1CSeLnOk : 3 : %checkr( '\ ' : V1CSeLnOk ) )
055400130206                                             = *blank;
055500130206             V1Dmsg = $Msg(02);
055600130206             PosCurSeLnOk = *on;
055700130206             ErrMessage   = *on;
055800130206             ErrGenerico  = *on;
055900130206             leavesr;
056000130206
056100130206           // -?SETRAS - Immagini in Linea - A Scarto?
056200130206           When  V1CSeLnSc = *blank;
056300130206             V1Dmsg = $Msg(01);
056400130206             PosCurSeLnSc = *on;
056500130206             ErrMessage   = *on;
056600130206             ErrGenerico  = *on;
056700130206             leavesr;
056800130206           When  %subst( %triml(V1CSeLnSc) : 1 : 2 ) <> '\\';
056900130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
057000130206             PosCurSeLnSc = *on;
057100130206             ErrMessage   = *on;
057200130206             ErrGenerico  = *on;
057300130206             leavesr;
057400130206           When  %subst( V1CSeLnSc : %len( %trimr(V1CSeLnSc) ) : 2 )
057500130206                 <> '\ ';
057600130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
057700130206             PosCurSeLnSc = *on;
057800130206             ErrMessage   = *on;
057900130206             ErrGenerico  = *on;
058000130206             leavesr;
058100130206           When  %checkr( '\ ' : V1CSeLnSc ) <= 3  or
058200130206                 %subst( V1CSeLnSc : 3 : %checkr( '\ ' : V1CSeLnSc ) )
058300130206                                             = *blank;
058400130206             V1Dmsg = $Msg(02);
058500130206             PosCurSeLnSc = *on;
058600130206             ErrMessage   = *on;
058700130206             ErrGenerico  = *on;
058800130206             leavesr;
058900130206
059000130206           // -?SETRAS - Immagini di BackUp - Ok?
059100130206           When  V1CSeBkOk = *blank;
059200130206             V1Dmsg = $Msg(01);
059300130206             PosCurSeBkOk = *on;
059400130206             ErrMessage   = *on;
059500130206             ErrGenerico  = *on;
059600130206             leavesr;
059700130206           When  %subst( %triml(V1CSeBkOk) : 1 : 2 ) <> '\\';
059800130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
059900130206             PosCurSeBkOk = *on;
060000130206             ErrMessage   = *on;
060100130206             ErrGenerico  = *on;
060200130206             leavesr;
060300130206           When  %subst( V1CSeBkOk : %len( %trimr(V1CSeBkOk) ) : 2 )
060400130206                 <> '\ ';
060500130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
060600130206             PosCurSeBkOk = *on;
060700130206             ErrMessage   = *on;
060800130206             ErrGenerico  = *on;
060900130206             leavesr;
061000130206           When  %checkr( '\ ' : V1CSeBkOk ) <= 3  or
061100130206                 %subst( V1CSeBkOk : 3 : %checkr( '\ ' : V1CSeBkOk ) )
061200130206                                             = *blank;
061300130206             V1Dmsg = $Msg(02);
061400130206             PosCurSeBkOk = *on;
061500130206             ErrMessage   = *on;
061600130206             ErrGenerico  = *on;
061700130206             leavesr;
061800130206
061900130206           // -?SETRAS - Immagini di BackUp - A Scarto?
062000130206           When  V1CSeBkSc = *blank;
062100130206             V1Dmsg = $Msg(01);
062200130206             PosCurSeBkSc = *on;
062300130206             ErrMessage   = *on;
062400130206             ErrGenerico  = *on;
062500130206             leavesr;
062600130206           When  %subst( %triml(V1CSeBkSc) : 1 : 2 ) <> '\\';
062700130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
062800130206             PosCurSeBkSc = *on;
062900130206             ErrMessage   = *on;
063000130206             ErrGenerico  = *on;
063100130206             leavesr;
063200130206           When  %subst( V1CSeBkSc : %len( %trimr(V1CSeBkSc) ) : 2 )
063300130206                                             <> '\ ';
063400130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
063500130206             PosCurSeBkSc = *on;
063600130206             ErrMessage   = *on;
063700130206             ErrGenerico  = *on;
063800130206             leavesr;
063900130206           When  %checkr( '\ ' : V1CSeBkSc ) <= 3  or
064000130206                 %subst( V1CSeBkSc : 3 : %checkr( '\ ' : V1CSeBkSc ) )
064100130206                                             = *blank;
064200130206             V1Dmsg = $Msg(02);
064300130206             PosCurSeBkSc = *on;
064400130206             ErrMessage   = *on;
064500130206             ErrGenerico  = *on;
064600130206             leavesr;
064700130206
064800130206           // -?AS888 - Immagini in Linea - Ok?
064900130206           When  V1CA8LnOk = *blank;
065000130206             V1Dmsg = $Msg(01);
065100130206             PosCurA8LnOk = *on;
065200130206             ErrMessage   = *on;
065300130206             ErrGenerico  = *on;
065400130206             leavesr;
065500130206           When  %subst( %triml(V1CA8LnOk) : 1 : 2 ) <> '\\';
065600130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
065700130206             PosCurA8LnOk = *on;
065800130206             ErrMessage   = *on;
065900130206             ErrGenerico  = *on;
066000130206             leavesr;
066100130206           When  %subst( V1CA8LnOk : %len( %trimr(V1CA8LnOk) ) : 2 )
066200130206                 <> '\ ';
066300130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
066400130206             PosCurA8LnOk = *on;
066500130206             ErrMessage   = *on;
066600130206             ErrGenerico  = *on;
066700130206             leavesr;
066800130206           When  %checkr( '\ ' : V1CA8LnOk ) <= 3  or
066900130206                 %subst( V1CA8LnOk : 3 : %checkr( '\ ' : V1CA8LnOk ) )
067000130206                                             = *blank;
067100130206             V1Dmsg = $Msg(02);
067200130206             PosCurA8LnOk = *on;
067300130206             ErrMessage   = *on;
067400130206             ErrGenerico  = *on;
067500130206             leavesr;
067600130206
067700130206           // -?AS888 - Immagini in Linea - A Scarto?
067800130206           When  V1CA8LnSc = *blank;
067900130206             V1Dmsg = $Msg(01);
068000130206             PosCurA8LnSc = *on;
068100130206             ErrMessage   = *on;
068200130206             ErrGenerico  = *on;
068300130206             leavesr;
068400130206           When  %subst( %triml(V1CA8LnSc) : 1 : 2 ) <> '\\';
068500130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
068600130206             PosCurA8LnSc = *on;
068700130206             ErrMessage   = *on;
068800130206             ErrGenerico  = *on;
068900130206             leavesr;
069000130206           When  %subst( V1CA8LnSc : %len( %trimr(V1CA8LnSc) ) : 2 )
069100130206                 <> '\ ';
069200130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
069300130206             PosCurA8LnSc = *on;
069400130206             ErrMessage   = *on;
069500130206             ErrGenerico  = *on;
069600130206             leavesr;
069700130206           When  %checkr( '\ ' : V1CA8LnSc ) <= 3  or
069800130206                 %subst( V1CA8LnSc : 3 : %checkr( '\ ' : V1CA8LnSc ) )
069900130206                                             = *blank;
070000130206             V1Dmsg = $Msg(02);
070100130206             PosCurA8LnSc = *on;
070200130206             ErrMessage   = *on;
070300130206             ErrGenerico  = *on;
070400130206             leavesr;
070500130206
070600130206         EndSl;
070700130206
070800130206         // ->?F11 = Inversione Linea-BackUp?
070900130206         F11Attivo = (V1CSeLnOk <> *blank  or  V1CSeLnSc <> *blank  or
071000130206                      V1CSeBkOk <> *blank  or  V1CSeBkSc <> *blank);
071100130206
071200130206       ENDSR;
071300130206
071400130206       //--------------------------------------------------------------
071500130206       //?Gestione tasto funzionale F11=Inversione Linea-BackUp.       ?
071600130206       //--------------------------------------------------------------
071700130206       BEGSR  sr_Inversione;
071800130206
071900130206         clear  dILVAs888Lin;
072000130206
072100130206         dILVas888Lin.§ILVimgOk  = V1CSeLnOk;
072200130206         dILVas888Lin.§ILVimgSca = V1CSeLnSc;
072300130206
072400130206         V1CSeLnOk = V1CSeBkOk;
072500130206         V1CSeLnSc = V1CSeBkSc;
072600130206
072700130206         V1CSeBkOk = dILVas888Lin.§ILVimgOk;
072800130206         V1CSeBkSc = dILVas888Lin.§ILVimgSca;
072900130206
073000130206       ENDSR;
073100130206
073200130206       //--------------------------------------------------------------
073300130206       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
073400130206       //--------------------------------------------------------------
073500130206       BEGSR  sr_UpdateTBE;
073600130206
073700130206         // -?Ciclo di elaborazione per ogni sistema informativo?
073800130206         For  w_SisInf = 1  To  %elem($Libr);
073900130206
074000130206           // -?Apertura degli archivi?
074100130206           if  w_SisInf > 1;
074200130206             exsr  sr_OpenFile;
074300130206           endif;
074400130206
074500130206           // -?Aggiornamento tab. "ILV"?
074600130206           For  xx = 1  To  3;
074700130206             exsr  sr_UpdTNTBE;
074800130206           EndFor;
074900130206
075000130206         EndFor;
075100130206
075200130206       ENDSR;
075300130206
075400130206       //--------------------------------------------------------------
075500130206       //?Aggiornamento records TNTBE00F (tab. "ILV").                 ?
075600130206       //--------------------------------------------------------------
075700130206       BEGSR  sr_UpdTNTBE;
075800130206
075900130206         //? N.B.                                                      ?
076000130206         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
076100130206         //   trasmissione (vedi flag TBEFTR).                        ?
076200130206         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
076300130206         //   subito nello stesso file di entrambi i S.I. - in due    ?
076400130206         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
076500130206
076600130206         // -?RI-aggancio record da aggiornare?
076700130206         clear  k05tntbe01;
076800130206         k_TBEcod = c_Tab;
076900130206         select;
077000130206           when  xx = 1;
077100130206             k_TBEke1 = c_Setras;
077200130206             k_TBEke2 = c_Linea;
077300130206           when  xx = 2;
077400130206             k_TBEke1 = c_Setras;
077500130206             k_TBEke2 = c_BackUp;
077600130206           when  xx = 3;
077700130206             k_TBEke1 = c_As888;
077800130206             k_TBEke2 = c_Linea;
077900130206         endsl;
078000130206         chain  %kds(K05tntbe01)  TNTBE000;
078100130206
078200130206         // -?Aggiornamento singolo record della tab. "ILV"?
078300130206         select;
078400130206
078500130206           // -?F5=Ripristino?
078600130206           when  dsp_aid = c_F05;
078700130206             exsr  sr_RieRecTBE;
078800130206             select;
078900130206               when  %found(TNTBE01L)   and   TBEatb <> *blank;
079000130206                 clear  TBEatb;
079100130206                 //_______________
079200130206                 UPDATE  TNTBE000;
079300130206                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079400130206               // -?Record appena cancellato fisicamente?
079500130206               when  NOT %found(TNTBE01L);
079600130206                 clear  TBEatb;
079700130206                 //_______________
079800130206                 WRITE   TNTBE000;
079900130206                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080000130206             endsl;
080100130206
080200130206           // -?F6 = Aggiornamento / Inserimento?
080300130206           when  dsp_aid = c_F06;
080400130206             exsr  sr_RieRecTBE;
080500130206             clear  TBEatb;
080600130206             if  %found(TNTBE01L);
080700130206               //_____________
080800130206               UPDATE TNTBE000;
080900130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯
081000130206             else;
081100130206               //_____________
081200130206               WRITE  TNTBE000;
081300130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯
081400130206             endif;
081500130206
081600130206           // -?F16=Annullamento?
081700130206           when  dsp_aid = c_F16;
081800130206             if  %found(TNTBE01L)   and   TBEatb <> 'A';
081900130206               TBEatb = 'A';
082000130206               //TBEftt = W1ftt;
082100130206               TBEftt = 'S';
082200130206               clear  TBEftr;
082300130206               //_______________
082400130206               UPDATE  TNTBE000;
082500130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
082600130206             endif;
082700130206
082800130206         endsl;
082900130206
083000130206       ENDSR;
083100130206
083200130206       //--------------------------------------------------------------
083300130206       //?Caricamento dati in tab. "3EW".                              ?
083400130206       //--------------------------------------------------------------
083500130206       BEGSR  sr_RieRecTBE;
083600130206
083700130206         // -?Impostazione campi chiave se inserimento?
083800130206         if  NOT %found(TNTBE01L);
083900130206           clear TNTBE000;
084000130206           TBEcod = k_TBEcod;
084100130206           TBEke1 = k_TBEke1;
084200130206           TBEke2 = k_TBEke2;
084300130206           TBElin = k_TBElin;
084400130206           TBEsif = k_TBEsif;
084500130206         endif;
084600130206
084700130206         // -?Impostazione dati tabella da aggiornare?
084800130206         select;
084900130206           when  xx = 1;
085000130206             clear  dILVsetrasLin;
085100130206             dILVsetrasLin.§ILVimgOk  = V1CSeLnOk;
085200130206             dILVsetrasLin.§ILVimgSca = V1CSeLnSc;
085300130206             TBEuni = dILVsetrasLin;
085400130206           when  xx = 2;
085500130206             clear  dILVsetrasBkUp;
085600130206             dILVsetrasBkUp.§ILVimgOk  = V1CSeBkOk;
085700130206             dILVsetrasBkUp.§ILVimgSca = V1CSeBkSc;
085800130206             TBEuni = dILVsetrasBkUp;
085900130206           when  xx = 3;
086000130206             clear  dILVAs888Lin;
086100130206             dILVas888Lin.§ILVimgOk  = V1CA8LnOk;
086200130206             dILVas888Lin.§ILVimgSca = V1CA8LnSc;
086300130206             TBEuni = dILVas888Lin;
086400130206         endsl;
086500130206
086600130206         // -?Impostazione altri campi del record?
086700130206         TBEapl = TBXapl;
086800130206         //clear  TBEatb;       //Sarà impostato caso per caso...?
086900130206         //TBEftt = W1ftt;
087000130206         TBEftt = 'S';
087100130206         clear  TBEflt;
087200130206         if  w_SisInf = 1;
087300130206           TBEftr = 'T';
087400130206         else;
087500130206           TBEftr = 'R';
087600130206         endif;
087700130206         TBEdtr = wDate;
087800130206
087900130206       ENDSR;
088000130206
088100130206       //--------------------------------------------------------------
088200130206       //?Operazioni finali.                                           ?
088300130206       //--------------------------------------------------------------
088400130206       BEGSR  sr_RoutEnd;
088500130206
088600130206         // -?Chiusura pgm?
088700130206         return;
088800130206
088900130206       ENDSR;
089000130206
089100130206      /end-free
089200130206
089300130206       //--------------------------------------------------------------
089400130206       //?Definizione schiere a tempo di compilazione                  ?
089500130206       //--------------------------------------------------------------
089600130206
089700130206** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
089800130206SETRAS  GAITRAGRU FILTRAGRU
089900130206AS888   GAITRAGRPSFILTRAGRPF
090000130206** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
090100130206RootFolder obbligatorio                                                         1
090200130206Nome RootFolder con caratteri mancanti                                          2
