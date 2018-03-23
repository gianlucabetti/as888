000100130206       //==============================================================
000200170707       //?Gestione tabella "ILG" (RootFolder x immagini LdV - GULLIVER)?
000300170707       //?- copia della tabella "ILV" -                                ?
000400130206       //==============================================================
000500130206
000600130206       //--------------------------------------------------------------
000700130206       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000800130206       //--------------------------------------------------------------
000900130206
001000130206     /*PRM  dbgview(*source)
001100130206     /*END
001200130206
001300130206       //--------------------------------------------------------------
001400130206       //?Specifiche di controllo.                                     ?
001500130206       //--------------------------------------------------------------
001600130206
001700130206     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001800130206     h dftactgrp(*no)
001900130206     h bnddir('TRUL')
002000130206
002100130206       //--------------------------------------------------------------
002200130206       //?Dichiarazione file.                                          ?
002300130206       //--------------------------------------------------------------
002400130206
002500130206       // -?Tabelle?
002600130206     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
002700130206     f                                     usropn
002800130206
002900130206       // -?Video?
003000170707     fTNTBILGD  cf   e             workstn
003100130206     f                                     indds(IndDspF)
003200130206     f                                     infds(InfDspF)
003300130206
003400130206       //--------------------------------------------------------------
003500130206       //?Definizione costanti.                                        ?
003600130206       //--------------------------------------------------------------
003700130206
003800130206       // -?Codice tabella in gestione?
003900170707     d c_Tab           c                   const('ILG')
004000130206
004100130206       // -?TBEke1 previsti?
004200130206     d c_Setras        c                   const('SETRAS')
004300130206     d c_As888         c                   const('AS888')
004400130206       // -?TBEke2 previsti?
004500130206     d c_Linea         c                   const('LINEA')
004600130206     d c_BackUp        c                   const('BACKUP')
004700130206
004800130206       // -?Tasti funzionali a video?
004900130206     d c_F01           c                   const(x'31')
005000130206     d c_F02           c                   const(x'32')
005100130206     d c_F03           c                   const(x'33')
005200130206     d c_F04           c                   const(x'34')
005300130206     d c_F05           c                   const(x'35')
005400130206     d c_F06           c                   const(x'36')
005500130206     d c_F07           c                   const(x'37')
005600130206     d c_F08           c                   const(x'38')
005700130206     d c_F09           c                   const(x'39')
005800130206     d c_F10           c                   const(x'3A')
005900130206     d c_F11           c                   const(x'3B')
006000130206     d c_F12           c                   const(x'3C')
006100130206     d c_F13           c                   const(x'B1')
006200130206     d c_F14           c                   const(x'B2')
006300130206     d c_F15           c                   const(x'B3')
006400130206     d c_F16           c                   const(x'B4')
006500130206     d c_F17           c                   const(x'B5')
006600130206     d c_F18           c                   const(x'B6')
006700130206     d c_F19           c                   const(x'B7')
006800130206     d c_F20           c                   const(x'B8')
006900130206     d c_F21           c                   const(x'B9')
007000130206     d c_F22           c                   const(x'BA')
007100130206     d c_F23           c                   const(x'BB')
007200130206     d c_F24           c                   const(x'BC')
007300130206     d c_Enter         c                   const(x'F1')
007400130206     d c_RollDown      c                   const(x'F4')
007500130206     d c_RollUp        c                   const(x'F5')
007600130206
007700130206       // -?Costanti per la definizione delle schiere con i nomi?
007800130206       //  ?degli iSeries da elaborare e delle relative librerie?
007900130206     d c_NrSyst        c                   const(2)
008000130206     d c_NrLibr        c                   const(2)
008100130206
008200130206       //--------------------------------------------------------------
008300130206       //?Definizione schiere.                                         ?
008400130206       //--------------------------------------------------------------
008500130206
008600130206       // -?iSeries  &  Librerie con entrambi i file tabelle?
008700130206     d $iSystem        s                   like(currSysNetA)
008800130206     d                                     dim(c_NrSyst)
008900130206     d                                     ctdata   perrcd( 1)
009000130206     d $Libraries      s                   like(ds_Libr)
009100130206     d                                     dim(c_NrSyst)
009200130206     d                                     alt($iSystem)
009300130206
009400130206       // -?Messaggi di errore?
009500130206     d $Msg            s             78    dim( 5)  ctdata  perrcd( 1)
009600130206
009700130206       //--------------------------------------------------------------
009800130206       //?Definizione aree dati.                                       ?
009900130206       //--------------------------------------------------------------
010000130206
010100130206       // -?Dati utente?
010200130206     d §AzUte        e ds                  extname(AZUTE00F)
010300130206     d                                     dtaara
010400130206     d §DatiUte      e ds                  extname(dDatiUte)
010500130206     d                                     dtaara
010600130206
010700130206       //--------------------------------------------------------------
010800130206       //?Definizione strutture dati.                                  ?
010900130206       //--------------------------------------------------------------
011000130206
011100130206       // -?Status ds?
011200130206     d Status         sds
011300130206     d   SDSpgm          *proc
011400130206
011500130206       // -?InfDS?
011600130206     d InfDspF         ds
011700130206     d   dsp_aid             369    369a
011800130206
011900130206       // -?Indicatori su DspF?
012000130206     d IndDspF         ds                  inz
012100130206         // -?Abilitazione tasti funzionali?
012200130206     d   F5Attivo                      n   overlay(IndDspF : 05)
012300130206     d   F6Attivo                      n   overlay(IndDspF : 06)
012400130206     d   F11Attivo                     n   overlay(IndDspF : 11)
012500130206     d*//F16Attivo                     n   overlay(IndDspF : 16)
012600130206         // -?Emissione messaggio di errore?
012700130206     d   ErrMessage                    n   overlay(IndDspF : 28)
012800130206         // -?Posizionamento cursore & segnalazione errore?
012900130206     d   PosCurSeLnOk                  n   overlay(IndDspF : 51)
013000130206     d   PosCurSeLnSc                  n   overlay(IndDspF : 52)
013100130206     d   PosCurSeBkOk                  n   overlay(IndDspF : 53)
013200130206     d   PosCurSeBkSc                  n   overlay(IndDspF : 54)
013300130206     d   PosCurA8LnOk                  n   overlay(IndDspF : 55)
013400130206     d   PosCurA8LnSc                  n   overlay(IndDspF : 56)
013500130206         //   -?Riemissione videata?
013600130206     d   ErrGenerico                   n   overlay(IndDspF : 99)
013700130206
013800130206       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
013900130206     d ds_Libr         ds                  inz
014000130206     d   $Libr                       10    dim(c_NrLibr) inz
014100130206
014200130206       // -?Parametri ricevuti?
014300130206     d KPJBA         e ds
014400130206
014500130206       // -?Dati record principale della tabella?
014600130206     d TNTBEds       e ds                  inz  extname(TNTBE00F)
014700130206     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
014800130206     d                                          prefix(TBX:3)
014900130206
015000130206       // -?Tabelle usate:?
015100130206       // ·?"ILV" = RootFolders per immagini LdV?
015200130206     d dILVsetrasLin e ds                  extname(dILV)  qualified
015300130206     d                                     inz
015400130206     d dILVsetrasBkUpe ds                  extname(dILV)  qualified
015500130206     d                                     inz
015600130206     d dILVas888Lin  e ds                  extname(dILV)  qualified
015700130206     d                                     inz
015800130206
015900130206       //--------------------------------------------------------------
016000130206       //?Definizione variabili globali.                               ?
016100130206       //--------------------------------------------------------------
016200130206
016300130206       // -?Flags booleani?
016400130206     d $Fine           s               n   inz
016500130206     d $InzD02         s               n   inz(*on)
016600130206
016700130206       // -?Indici di schiera?
016800130206     d xx              s              3  0 inz
016900130206
017000130206       // -?Variabili per la gestione del video?
017100130206     d $Video          s              2    inz('D2')
017200130206
017300130206       // -?Nome del sistema?
017400130206     d currSysNeta     s              8a   inz
017500130206
017600130206       // -?Nome esteso Libreria/File dei 2 file tabella?
017700130206     d wLibFile        s             21a   inz
017800130206
017900130206       // -?Flag di Annullamento dei 4 record?
018000130206     d wTBEatbSetLin   s                   like(TBEatb)   inz
018100130206     d wTBEatbSetBkUp  s                   like(TBEatb)   inz
018200130206     d wTBEatbAS8Lin   s                   like(TBEatb)   inz
018300130206
018400130206       // -?Campi di comodo?
018500130206     d w_iSystem       s              1  0 inz
018600130206     d w_SisInf        s              3  0 inz
018700130206     d wDate           s              8  0 inz
018800130206     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
018900130206
019000130206       //--------------------------------------------------------------
019100130206       //?Definizione prototipi procedure.                             ?
019200130206       //--------------------------------------------------------------
019300130206
019400130206       // -?Reperimento NETA sistema AS/400 corrente?
019500130206      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019600130206
019700130206       // -?Reperimento dati utente?
019800130206     d TIBS34ds      e ds
019900130206      /copy gaitrasrc/srcProtoPR,TIBS34R
020000130206
020100130206       //--------------------------------------------------------------
020200130206       //?Definizione key-list.                                        ?
020300130206       //--------------------------------------------------------------
020400130206
020500130206       // -?File TNTBE01L?
020600130206     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020700130206     d                                     prefix(k_)   inz
020800130206
020900130206       //--------------------------------------------------------------
021000130206       //?M A I N - L I N E                                            ?
021100130206       //--------------------------------------------------------------
021200130206
021300130206     c     *Entry        plist
021400130206     c                   parm                    KPJBA
021500130206
021600130206      /free
021700130206
021800130206       // -?Operazioni iniziali?
021900130206       exsr  sr_RoutInz;
022000130206
022100130206       // -?Ciclo di gestione del file video?
022200130206       DOW  $Fine = *off;
022300130206
022400130206         select;
022500130206           // -?Gestione RootFolder per immagini LdV?
022600130206           when $Video = 'D2';
022700130206             exsr  sr_GesD02;
022800130206           // -?? ? ??
022900130206           other;
023000130206             $Fine = *on;
023100130206         endsl;
023200130206
023300130206       ENDDO;
023400130206
023500130206       // -?Operazioni finali?
023600130206       exsr  sr_RoutEnd;
023700130206
023800130206       //--------------------------------------------------------------
023900130206       //?Operazioni iniziali.                                         ?
024000130206       //--------------------------------------------------------------
024100130206       BEGSR  *InzSr;
024200130206
024300130206         // -?Reperimento dati job?
024400130206         exsr  sr_DatiJob;
024500130206
024600130206         // -?Impostazione nome programma a video?
024700130206         V1Tpgm = SDSpgm;
024800130206
024900130206         // -?Reperimento data odierna?
025000130206         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025100130206                               : 1 : 8 ) );
025200130206
025300130206       ENDSR;
025400130206
025500130206       //--------------------------------------------------------------
025600130206       //?Operazioni iniziali.                                         ?
025700130206       //--------------------------------------------------------------
025800130206       BEGSR  sr_RoutInz;
025900130206
026000130206         // -?Impostazione chiusura?
026100130206         *inLR = *on;
026200130206
026300130206         // -?Verifica del sistema AS/400 corrente?
026400130206         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
026500130206           $Fine = *on;
026600130206           leavesr;
026700130206         endif;
026800130206
026900130206         // -?Impostazione elenco librerie in cui gestire le tabelle?
027000130206         //  ?(a seconda del sistema in cui si stà lavorando)?
027100130206         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
027200130206         if  w_iSystem = *zero;
027300130206           $Fine = *on;
027400130206           leavesr;
027500130206         endif;
027600130206
027700130206         // -?1ª apertura file TNTBE01L del 1° S.I. (sede)?
027800130206         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
027900130206           w_SisInf = 1;
028000130206           exsr  sr_OpenFile;
028100130206         endif;
028200130206
028300130206         // -?Aggancio dati generali della tabella in esame?
028400130206         clear  k_TBEcod;
028500130206         k_TBEke1 = *zero;
028600130206         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
028700130206                = c_Tab;
028800130206         clear  k_TBEke2;
028900130206         clear  k_TBElin;
029000130206         k_TBEsif = KNSIF;
029100130206         chain(n)  %kds(k05tntbe01)  TNTBE000;
029200130206         if  not  %found(TNTBE01L);
029300130206           clear  k_TBEsif;
029400130206           chain(n)  %kds(k05tntbe01)  TNTBE000;
029500130206         endif;
029600130206         if  %found(TNTBE01L);
029700130206           xTNTBEds = TNTBEds;
029800130206         else;
029900130206           clear  xTNTBEds;
030000130206         endif;
030100130206
030200130206         // -?Impostazione iniziale / pulizia dei campi chiave?
030300130206         clear  k05tntbe01;
030400130206         k_TBEcod = c_Tab;
030500130206
030600130206         // -?Reperimento dati (a chiave "fissa")?
030700130206         clear  dILVsetrasLin;
030800130206         clear  dILVsetrasBkUp;
030900130206         clear  dILVas888Lin;
031000130206         clear  wTBEatbSetLin;
031100130206         clear  wTBEatbSetBkUp;
031200130206         clear  wTBEatbAs8Lin;
031300130206
031400130206         k_TBEke1 = c_Setras;
031500130206         k_TBEke2 = c_Linea;
031600130206         chain  %kds(k05tntbe01)  TNTBE000;
031700130206         if  %found(TNTBE01L);
031800130206           wTBEatbSetLin = TBEatb;
031900130206           if  TBEatb = *blank;
032000130206             dILVsetrasLin  = TBEuni;
032100130206           endif;
032200130206         endif;
032300130206
032400130206         k_TBEke1 = c_Setras;
032500130206         k_TBEke2 = c_BackUp;
032600130206         chain  %kds(k05tntbe01)  TNTBE000;
032700130206         if  %found(TNTBE01L);
032800130206           wTBEatbSetBkUp = TBEatb;
032900130206           if  TBEatb = *blank;
033000130206             dILVsetrasBkUp = TBEuni;
033100130206           endif;
033200130206         endif;
033300130206
033400130206         k_TBEke1 = c_As888;
033500130206         k_TBEke2 = c_Linea;
033600130206         chain  %kds(k05tntbe01)  TNTBE000;
033700130206         if  %found(TNTBE01L);
033800130206           wTBEatbAs8Lin = TBEatb;
033900130206           if  TBEatb = *blank;
034000130206             dILVas888Lin   = TBEuni;
034100130206           endif;
034200130206         endif;
034300130206
034400130206       ENDSR;
034500130206
034600130206       //--------------------------------------------------------------
034700130206       //?Reperimento Dati del job (Utente/Operativi).                 ?
034800130206       //--------------------------------------------------------------
034900130206       BEGSR  sr_DatiJob;
035000130206
035100130206         in(E) §AzUte;
035200130206         if NOT %error;
035300130206           in(E) §DatiUte;
035400130206         endif;
035500130206         if %error or RSut = *blanks;
035600130206           clear TIBS34ds;
035700130206           tibs34r ( tibs34ds );
035800130206           in §AzUte;
035900130206           in §DatiUte;
036000130206         endif;
036100130206
036200130206       ENDSR;
036300130206
036400130206       //--------------------------------------------------------------
036500130206       //?Apertura dei files tabelle nel sistema informativo impostato.?
036600130206       //--------------------------------------------------------------
036700130206       BEGSR  sr_OpenFile;
036800130206
036900130206         // -?Chiusura (eventuale) file tabelle?
037000130206         if  %open(TNTBE01L);
037100130206           close  TNTBE01L;
037200130206         endif;
037300130206
037400130206         // -?Apertura file tabelle?
037500130206         ds_Libr  = $Libraries(w_iSystem);
037600130206         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
037700130206         open  TNTBE01L;
037800130206
037900130206       ENDSR;
038000130206
038100130206       //--------------------------------------------------------------
038200130206       //?Gestione videata D02 (unica).                                ?
038300130206       //--------------------------------------------------------------
038400130206       BEGSR  sr_GesD02;
038500130206
038600130206         // -?Inizializzazione videata?
038700130206         if  $InzD02 = *on;
038800130206           exsr  sr_InzD02;
038900130206           $InzD02 = *off;
039000130206         endif;
039100130206
039200130206         // -?Emissione Testata?
039300130206         write  TBILVT01;
039400130206
039500130206         // -?Emissione videata?
039600130206         exfmt  TBILVD02;
039700130206
039800130206         clear  V1Dmsg;
039900130206         reset  ErrMessage;
040000130206         reset  ErrGenerico;
040100130206
040200130206         SELECT;
040300130206
040400130206           // -?F3=Fine?
040500130206           WHEN  dsp_aid = c_F03;
040600130206             unlock  TNTBE01L;
040700130206             $Fine = *on;
040800130206
040900130206           // -?F11=Inversione Linea-BackUp?
041000130206           WHEN  dsp_aid = c_F11;
041100130206             exsr  sr_Inversione;
041200130206
041300130206           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
041400130206           OTHER;
041500130206             // -?Controlli eseguiti NON se F16=Annullamento?
041600130206             if  dsp_aid <> c_F16;
041700130206               exsr  sr_CtrD02;
041800130206             endif;
041900130206             if  ErrGenerico;
042000130206               leavesr;
042100130206             endif;
042200130206             // -?Aggiornamento?
042300130206             if  dsp_aid = c_F05   or
042400130206                 dsp_aid = c_F06   or
042500130206                 dsp_aid = c_F16;
042600130206               exsr  sr_UpdateTBE;
042700130206               select;
042800130206                 // -?Rilevato errore in fase di aggiornamento?
042900130206                 when  ErrGenerico = *on;
043000130206                   leavesr;
043100130206                 // -?Uscita altrimenti?
043200130206                 other;
043300130206                   $Fine = *on;
043400130206               endsl;
043500130206             endif;
043600130206
043700130206         ENDSL;
043800130206
043900130206       ENDSR;
044000130206
044100130206       //--------------------------------------------------------------
044200130206       //?Inizializzazione videata D02.                                ?
044300130206       //--------------------------------------------------------------
044400130206       BEGSR  sr_InzD02;
044500130206
044600130206         F5Attivo  = *off;
044700130206         F6Attivo  = *off;
044800130206         F11Attivo = *off;
044900130206
045000130206         // -?Spegnimento degli indicatori di errore?
045100130206         %subst(IndDspF : 50) = *off;
045200130206
045300130206         // -?Pulizia videata?
045400130206         clear  TBILVD02;
045500130206
045600130206         // -?Caricamento dati a video?
045700130206         if  wTBEatbSetLin  = *blank  and  dILVsetrasLin  <> *blank;
045800130206           V1CSeLnOk = dILVsetrasLin.§ILVimgOk;
045900130206           V1CSeLnSc = dILVsetrasLin.§ILVimgSca;
046000130206         endif;
046100130206         if  wTBEatbSetBkUp = *blank  and  dILVsetrasBkUp <> *blank;
046200130206           V1CSeBkOk = dILVsetrasBkUp.§ILVimgOk;
046300130206           V1CSeBkSc = dILVsetrasBkUp.§ILVimgSca;
046400130206         endif;
046500130206         if  wTBEatbAs8Lin  = *blank  and  dILVas888Lin   <> *blank;
046600130206           V1CA8LnOk = dILVas888Lin.§ILVimgOk;
046700130206           V1CA8LnSc = dILVas888Lin.§ILVimgSca;
046800130206         endif;
046900130206
047000130206         // -?Impostazione indicatori per abilitazione tasti funzionali?
047100130206         exsr  sr_AbilitFxxD02;
047200130206
047300130206       ENDSR;
047400130206
047500130206       //--------------------------------------------------------------
047600130206       //?Settaggio indicatori nella videata D02 per abilitazione      ?
047700130206       //?  tasti funzionali.                                          ?
047800130206       //--------------------------------------------------------------
047900130206       BEGSR  sr_AbilitFxxD02;
048000130206
048100130206         // -?F3=Fine (sempre abilitato)?
048200130206         //F3Attivo  = *on;
048300130206
048400130206         // -?F5=Ripristino?
048500130206         F5Attivo  = (wTBEatbSetLin  <> *blank  and
048600130206                      wTBEatbSetBkUp <> *blank  and
048700130206                      wTBEatbAs8Lin  <> *blank);
048800130206
048900130206         // -?F6=Conferma?
049000130206         F6Attivo  = (NOT F5Attivo);
049100130206
049200130206         // -?F11 = Inversione Linea-BackUp?
049300130206         F11Attivo = (V1CSeLnOk <> *blank  or  V1CSeLnSc <> *blank  or
049400130206                      V1CSeBkOk <> *blank  or  V1CSeBkSc <> *blank);
049500130206
049600130206         // -?F16=Annullamento?
049700130206         //F16Attivo = ( dILVsetrasLin  <> *blank  or
049800130206         //              dILVsetrasBkUp <> *blank);
049900130206
050000130206       ENDSR;
050100130206
050200130206       //--------------------------------------------------------------
050300130206       //?Controllo dati videata D02.                                  ?
050400130206       //--------------------------------------------------------------
050500130206       BEGSR  sr_CtrD02;
050600130206
050700130206         // -?Spegnimento degli indicatori di errore?
050800130206         %subst(IndDspF : 50) = *off;
050900130206
051000130206         // -?Eliminazione *blanks a sinistra?
051100130206         if  V1CSeLnOk <> *blank;
051200130206           V1CSeLnOk = %triml( V1CSeLnOk );
051300130206         endif;
051400130206         if  V1CSeLnSc <> *blank;
051500130206           V1CSeLnSc = %triml( V1CSeLnSc );
051600130206         endif;
051700130206         if  V1CSeBkOk <> *blank;
051800130206           V1CSeBkOk = %triml( V1CSeBkOk );
051900130206         endif;
052000130206         if  V1CSeBkSc <> *blank;
052100130206           V1CSeBkSc = %triml( V1CSeBkSc );
052200130206         endif;
052300130206         if  V1CA8LnOk <> *blank;
052400130206           V1CA8LnOk = %triml( V1CA8LnOk );
052500130206         endif;
052600130206         if  V1CA8LnSc <> *blank;
052700130206           V1CA8LnSc = %triml( V1CA8LnSc );
052800130206         endif;
052900130206
053000130206         // -?RootFolders obbligatori/errati?
053100130206         Select;
053200130206
053300130206           // -?SETRAS - Immagini in Linea - Ok?
053400130206           When  V1CSeLnOk = *blank;
053500130206             V1Dmsg = $Msg(01);
053600130206             PosCurSeLnOk = *on;
053700130206             ErrMessage   = *on;
053800130206             ErrGenerico  = *on;
053900130206             leavesr;
054000130206           When  %subst( %triml(V1CSeLnOk) : 1 : 2 ) <> '\\';
054100130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
054200130206             PosCurSeLnOk = *on;
054300130206             ErrMessage   = *on;
054400130206             ErrGenerico  = *on;
054500130206             leavesr;
054600130206           When  %subst( V1CSeLnOk : %len( %trimr(V1CSeLnOk) ) : 2 )
054700130206                 <> '\ ';
054800130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
054900130206             PosCurSeLnOk = *on;
055000130206             ErrMessage   = *on;
055100130206             ErrGenerico  = *on;
055200130206             leavesr;
055300130206           When  %checkr( '\ ' : V1CSeLnOk ) <= 3  or
055400130206                 %subst( V1CSeLnOk : 3 : %checkr( '\ ' : V1CSeLnOk ) )
055500130206                                             = *blank;
055600130206             V1Dmsg = $Msg(02);
055700130206             PosCurSeLnOk = *on;
055800130206             ErrMessage   = *on;
055900130206             ErrGenerico  = *on;
056000130206             leavesr;
056100130206
056200130206           // -?SETRAS - Immagini in Linea - A Scarto?
056300130206           When  V1CSeLnSc = *blank;
056400130206             V1Dmsg = $Msg(01);
056500130206             PosCurSeLnSc = *on;
056600130206             ErrMessage   = *on;
056700130206             ErrGenerico  = *on;
056800130206             leavesr;
056900130206           When  %subst( %triml(V1CSeLnSc) : 1 : 2 ) <> '\\';
057000130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
057100130206             PosCurSeLnSc = *on;
057200130206             ErrMessage   = *on;
057300130206             ErrGenerico  = *on;
057400130206             leavesr;
057500130206           When  %subst( V1CSeLnSc : %len( %trimr(V1CSeLnSc) ) : 2 )
057600130206                 <> '\ ';
057700130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
057800130206             PosCurSeLnSc = *on;
057900130206             ErrMessage   = *on;
058000130206             ErrGenerico  = *on;
058100130206             leavesr;
058200130206           When  %checkr( '\ ' : V1CSeLnSc ) <= 3  or
058300130206                 %subst( V1CSeLnSc : 3 : %checkr( '\ ' : V1CSeLnSc ) )
058400130206                                             = *blank;
058500130206             V1Dmsg = $Msg(02);
058600130206             PosCurSeLnSc = *on;
058700130206             ErrMessage   = *on;
058800130206             ErrGenerico  = *on;
058900130206             leavesr;
059000130206
059100130206           // -?SETRAS - Immagini di BackUp - Ok?
059200130206           When  V1CSeBkOk = *blank;
059300130206             V1Dmsg = $Msg(01);
059400130206             PosCurSeBkOk = *on;
059500130206             ErrMessage   = *on;
059600130206             ErrGenerico  = *on;
059700130206             leavesr;
059800130206           When  %subst( %triml(V1CSeBkOk) : 1 : 2 ) <> '\\';
059900130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
060000130206             PosCurSeBkOk = *on;
060100130206             ErrMessage   = *on;
060200130206             ErrGenerico  = *on;
060300130206             leavesr;
060400130206           When  %subst( V1CSeBkOk : %len( %trimr(V1CSeBkOk) ) : 2 )
060500130206                 <> '\ ';
060600130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
060700130206             PosCurSeBkOk = *on;
060800130206             ErrMessage   = *on;
060900130206             ErrGenerico  = *on;
061000130206             leavesr;
061100130206           When  %checkr( '\ ' : V1CSeBkOk ) <= 3  or
061200130206                 %subst( V1CSeBkOk : 3 : %checkr( '\ ' : V1CSeBkOk ) )
061300130206                                             = *blank;
061400130206             V1Dmsg = $Msg(02);
061500130206             PosCurSeBkOk = *on;
061600130206             ErrMessage   = *on;
061700130206             ErrGenerico  = *on;
061800130206             leavesr;
061900130206
062000130206           // -?SETRAS - Immagini di BackUp - A Scarto?
062100130206           When  V1CSeBkSc = *blank;
062200130206             V1Dmsg = $Msg(01);
062300130206             PosCurSeBkSc = *on;
062400130206             ErrMessage   = *on;
062500130206             ErrGenerico  = *on;
062600130206             leavesr;
062700130206           When  %subst( %triml(V1CSeBkSc) : 1 : 2 ) <> '\\';
062800130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
062900130206             PosCurSeBkSc = *on;
063000130206             ErrMessage   = *on;
063100130206             ErrGenerico  = *on;
063200130206             leavesr;
063300130206           When  %subst( V1CSeBkSc : %len( %trimr(V1CSeBkSc) ) : 2 )
063400130206                                             <> '\ ';
063500130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
063600130206             PosCurSeBkSc = *on;
063700130206             ErrMessage   = *on;
063800130206             ErrGenerico  = *on;
063900130206             leavesr;
064000130206           When  %checkr( '\ ' : V1CSeBkSc ) <= 3  or
064100130206                 %subst( V1CSeBkSc : 3 : %checkr( '\ ' : V1CSeBkSc ) )
064200130206                                             = *blank;
064300130206             V1Dmsg = $Msg(02);
064400130206             PosCurSeBkSc = *on;
064500130206             ErrMessage   = *on;
064600130206             ErrGenerico  = *on;
064700130206             leavesr;
064800130206
064900130206           // -?AS888 - Immagini in Linea - Ok?
065000130206           When  V1CA8LnOk = *blank;
065100130206             V1Dmsg = $Msg(01);
065200130206             PosCurA8LnOk = *on;
065300130206             ErrMessage   = *on;
065400130206             ErrGenerico  = *on;
065500130206             leavesr;
065600130206           When  %subst( %triml(V1CA8LnOk) : 1 : 2 ) <> '\\';
065700130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
065800130206             PosCurA8LnOk = *on;
065900130206             ErrMessage   = *on;
066000130206             ErrGenerico  = *on;
066100130206             leavesr;
066200130206           When  %subst( V1CA8LnOk : %len( %trimr(V1CA8LnOk) ) : 2 )
066300130206                 <> '\ ';
066400130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
066500130206             PosCurA8LnOk = *on;
066600130206             ErrMessage   = *on;
066700130206             ErrGenerico  = *on;
066800130206             leavesr;
066900130206           When  %checkr( '\ ' : V1CA8LnOk ) <= 3  or
067000130206                 %subst( V1CA8LnOk : 3 : %checkr( '\ ' : V1CA8LnOk ) )
067100130206                                             = *blank;
067200130206             V1Dmsg = $Msg(02);
067300130206             PosCurA8LnOk = *on;
067400130206             ErrMessage   = *on;
067500130206             ErrGenerico  = *on;
067600130206             leavesr;
067700130206
067800130206           // -?AS888 - Immagini in Linea - A Scarto?
067900130206           When  V1CA8LnSc = *blank;
068000130206             V1Dmsg = $Msg(01);
068100130206             PosCurA8LnSc = *on;
068200130206             ErrMessage   = *on;
068300130206             ErrGenerico  = *on;
068400130206             leavesr;
068500130206           When  %subst( %triml(V1CA8LnSc) : 1 : 2 ) <> '\\';
068600130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
068700130206             PosCurA8LnSc = *on;
068800130206             ErrMessage   = *on;
068900130206             ErrGenerico  = *on;
069000130206             leavesr;
069100130206           When  %subst( V1CA8LnSc : %len( %trimr(V1CA8LnSc) ) : 2 )
069200130206                 <> '\ ';
069300130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
069400130206             PosCurA8LnSc = *on;
069500130206             ErrMessage   = *on;
069600130206             ErrGenerico  = *on;
069700130206             leavesr;
069800130206           When  %checkr( '\ ' : V1CA8LnSc ) <= 3  or
069900130206                 %subst( V1CA8LnSc : 3 : %checkr( '\ ' : V1CA8LnSc ) )
070000130206                                             = *blank;
070100130206             V1Dmsg = $Msg(02);
070200130206             PosCurA8LnSc = *on;
070300130206             ErrMessage   = *on;
070400130206             ErrGenerico  = *on;
070500130206             leavesr;
070600130206
070700130206         EndSl;
070800130206
070900130206         // ->?F11 = Inversione Linea-BackUp?
071000130206         F11Attivo = (V1CSeLnOk <> *blank  or  V1CSeLnSc <> *blank  or
071100130206                      V1CSeBkOk <> *blank  or  V1CSeBkSc <> *blank);
071200130206
071300130206       ENDSR;
071400130206
071500130206       //--------------------------------------------------------------
071600130206       //?Gestione tasto funzionale F11=Inversione Linea-BackUp.       ?
071700130206       //--------------------------------------------------------------
071800130206       BEGSR  sr_Inversione;
071900130206
072000130206         clear  dILVAs888Lin;
072100130206
072200130206         dILVas888Lin.§ILVimgOk  = V1CSeLnOk;
072300130206         dILVas888Lin.§ILVimgSca = V1CSeLnSc;
072400130206
072500130206         V1CSeLnOk = V1CSeBkOk;
072600130206         V1CSeLnSc = V1CSeBkSc;
072700130206
072800130206         V1CSeBkOk = dILVas888Lin.§ILVimgOk;
072900130206         V1CSeBkSc = dILVas888Lin.§ILVimgSca;
073000130206
073100130206       ENDSR;
073200130206
073300130206       //--------------------------------------------------------------
073400130206       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
073500130206       //--------------------------------------------------------------
073600130206       BEGSR  sr_UpdateTBE;
073700130206
073800130206         // -?Ciclo di elaborazione per ogni sistema informativo?
073900130206         For  w_SisInf = 1  To  %elem($Libr);
074000130206
074100130206           // -?Apertura degli archivi?
074200130206           if  w_SisInf > 1;
074300130206             exsr  sr_OpenFile;
074400130206           endif;
074500130206
074600170707           // -?Aggiornamento tab. "ILG"?
074700130206           For  xx = 1  To  3;
074800130206             exsr  sr_UpdTNTBE;
074900130206           EndFor;
075000130206
075100130206         EndFor;
075200130206
075300130206       ENDSR;
075400130206
075500130206       //--------------------------------------------------------------
075600170707       //?Aggiornamento records TNTBE00F (tab. "ILG").                 ?
075700130206       //--------------------------------------------------------------
075800130206       BEGSR  sr_UpdTNTBE;
075900130206
076000130206         //? N.B.                                                      ?
076100130206         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
076200130206         //   trasmissione (vedi flag TBEFTR).                        ?
076300130206         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
076400130206         //   subito nello stesso file di entrambi i S.I. - in due    ?
076500130206         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
076600130206
076700130206         // -?RI-aggancio record da aggiornare?
076800130206         clear  k05tntbe01;
076900130206         k_TBEcod = c_Tab;
077000130206         select;
077100130206           when  xx = 1;
077200130206             k_TBEke1 = c_Setras;
077300130206             k_TBEke2 = c_Linea;
077400130206           when  xx = 2;
077500130206             k_TBEke1 = c_Setras;
077600130206             k_TBEke2 = c_BackUp;
077700130206           when  xx = 3;
077800130206             k_TBEke1 = c_As888;
077900130206             k_TBEke2 = c_Linea;
078000130206         endsl;
078100130206         chain  %kds(K05tntbe01)  TNTBE000;
078200130206
078300170707         // -?Aggiornamento singolo record della tab. "ILG"?
078400130206         select;
078500130206
078600130206           // -?F5=Ripristino?
078700130206           when  dsp_aid = c_F05;
078800130206             exsr  sr_RieRecTBE;
078900130206             select;
079000130206               when  %found(TNTBE01L)   and   TBEatb <> *blank;
079100130206                 clear  TBEatb;
079200130206                 //_______________
079300130206                 UPDATE  TNTBE000;
079400130206                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
079500130206               // -?Record appena cancellato fisicamente?
079600130206               when  NOT %found(TNTBE01L);
079700130206                 clear  TBEatb;
079800130206                 //_______________
079900130206                 WRITE   TNTBE000;
080000130206                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
080100130206             endsl;
080200130206
080300130206           // -?F6 = Aggiornamento / Inserimento?
080400130206           when  dsp_aid = c_F06;
080500130206             exsr  sr_RieRecTBE;
080600130206             clear  TBEatb;
080700130206             if  %found(TNTBE01L);
080800130206               //_____________
080900130206               UPDATE TNTBE000;
081000130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯
081100130206             else;
081200130206               //_____________
081300130206               WRITE  TNTBE000;
081400130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯
081500130206             endif;
081600130206
081700130206           // -?F16=Annullamento?
081800130206           when  dsp_aid = c_F16;
081900130206             if  %found(TNTBE01L)   and   TBEatb <> 'A';
082000130206               TBEatb = 'A';
082100130206               //TBEftt = W1ftt;
082200130206               TBEftt = 'S';
082300130206               clear  TBEftr;
082400130206               //_______________
082500130206               UPDATE  TNTBE000;
082600130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
082700130206             endif;
082800130206
082900130206         endsl;
083000130206
083100130206       ENDSR;
083200130206
083300130206       //--------------------------------------------------------------
083400130206       //?Caricamento dati in tab. "3EW".                              ?
083500130206       //--------------------------------------------------------------
083600130206       BEGSR  sr_RieRecTBE;
083700130206
083800130206         // -?Impostazione campi chiave se inserimento?
083900130206         if  NOT %found(TNTBE01L);
084000130206           clear TNTBE000;
084100130206           TBEcod = k_TBEcod;
084200130206           TBEke1 = k_TBEke1;
084300130206           TBEke2 = k_TBEke2;
084400130206           TBElin = k_TBElin;
084500130206           TBEsif = k_TBEsif;
084600130206         endif;
084700130206
084800130206         // -?Impostazione dati tabella da aggiornare?
084900130206         select;
085000130206           when  xx = 1;
085100130206             clear  dILVsetrasLin;
085200130206             dILVsetrasLin.§ILVimgOk  = V1CSeLnOk;
085300130206             dILVsetrasLin.§ILVimgSca = V1CSeLnSc;
085400130206             TBEuni = dILVsetrasLin;
085500130206           when  xx = 2;
085600130206             clear  dILVsetrasBkUp;
085700130206             dILVsetrasBkUp.§ILVimgOk  = V1CSeBkOk;
085800130206             dILVsetrasBkUp.§ILVimgSca = V1CSeBkSc;
085900130206             TBEuni = dILVsetrasBkUp;
086000130206           when  xx = 3;
086100130206             clear  dILVAs888Lin;
086200130206             dILVas888Lin.§ILVimgOk  = V1CA8LnOk;
086300130206             dILVas888Lin.§ILVimgSca = V1CA8LnSc;
086400130206             TBEuni = dILVas888Lin;
086500130206         endsl;
086600130206
086700130206         // -?Impostazione altri campi del record?
086800130206         TBEapl = TBXapl;
086900130206         //clear  TBEatb;       //Sarà impostato caso per caso...?
087000130206         //TBEftt = W1ftt;
087100130206         TBEftt = 'S';
087200130206         clear  TBEflt;
087300130206         if  w_SisInf = 1;
087400130206           TBEftr = 'T';
087500130206         else;
087600130206           TBEftr = 'R';
087700130206         endif;
087800130206         TBEdtr = wDate;
087900130206
088000130206       ENDSR;
088100130206
088200130206       //--------------------------------------------------------------
088300130206       //?Operazioni finali.                                           ?
088400130206       //--------------------------------------------------------------
088500130206       BEGSR  sr_RoutEnd;
088600130206
088700130206         // -?Chiusura pgm?
088800130206         return;
088900130206
089000130206       ENDSR;
089100130206
089200130206      /end-free
089300130206
089400130206       //--------------------------------------------------------------
089500130206       //?Definizione schiere a tempo di compilazione                  ?
089600130206       //--------------------------------------------------------------
089700130206
089800130206** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
089900130206SETRAS  GAITRAGRU FILTRAGRU
090000130206AS888   GAITRAGRPSFILTRAGRPF
090100130206** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
090200130206RootFolder obbligatorio                                                         1
090300130206Nome RootFolder con caratteri mancanti                                          2
