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
004400130206     d c_Ok            c                   const('OK')
004500130206     d c_Scarto        c                   const('SCARTO')
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
012800130206     d   PosCurSeOkLn                  n   overlay(IndDspF : 51)
012900130206     d   PosCurSeOkBk                  n   overlay(IndDspF : 52)
013000130206     d   PosCurSeScLn                  n   overlay(IndDspF : 53)
013100130206     d   PosCurSeScBk                  n   overlay(IndDspF : 54)
013200130206     d   PosCurA8OkLn                  n   overlay(IndDspF : 55)
013300130206     d   PosCurA8ScLn                  n   overlay(IndDspF : 56)
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
015100130206     d dILVsetrasOk  e ds                  extname(dILV)  qualified
015200130206     d                                     inz
015300130206     d dILVsetrasScare ds                  extname(dILV)  qualified
015400130206     d                                     inz
015500130206     d dILVas888Ok   e ds                  extname(dILV)  qualified
015600130206     d                                     inz
015700130206     d dILVas888Scar e ds                  extname(dILV)  qualified
015800130206     d                                     inz
015900130206
016000130206       //--------------------------------------------------------------
016100130206       //?Definizione variabili globali.                               ?
016200130206       //--------------------------------------------------------------
016300130206
016400130206       // -?Flags booleani?
016500130206     d $Fine           s               n   inz
016600130206     d $InzD02         s               n   inz(*on)
016700130206
016800130206       // -?Indici di schiera?
016900130206     d xx              s              3  0 inz
017000130206
017100130206       // -?Variabili per la gestione del video?
017200130206     d $Video          s              2    inz('D2')
017300130206
017400130206       // -?Nome del sistema?
017500130206     d currSysNeta     s              8a   inz
017600130206
017700130206       // -?Nome esteso Libreria/File dei 2 file tabella?
017800130206     d wLibFile        s             21a   inz
017900130206
018000130206       // -?Flag di Annullamento dei 4 record?
018100130206     d wTBEatbSETok    s                   like(TBEatb)   inz
018200130206     d wTBEatbSETscar  s                   like(TBEatb)   inz
018300130206     d wTBEatbAS8ok    s                   like(TBEatb)   inz
018400130206     d wTBEatbAS8scar  s                   like(TBEatb)   inz
018500130206
018600130206       // -?Campi di comodo?
018700130206     d w_iSystem       s              1  0 inz
018800130206     d w_SisInf        s              3  0 inz
018900130206     d wDate           s              8  0 inz
019000130206     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
019100130206
019200130206       //--------------------------------------------------------------
019300130206       //?Definizione prototipi procedure.                             ?
019400130206       //--------------------------------------------------------------
019500130206
019600130206       // -?Reperimento NETA sistema AS/400 corrente?
019700130206      /copy gaitrasrc/srcProtoPR,UBRTVNETA
019800130206
019900130206       // -?Reperimento dati utente?
020000130206     d TIBS34ds      e ds
020100130206      /copy gaitrasrc/srcProtoPR,TIBS34R
020200130206
020300130206       //--------------------------------------------------------------
020400130206       //?Definizione key-list.                                        ?
020500130206       //--------------------------------------------------------------
020600130206
020700130206       // -?File TNTBE01L?
020800130206     d k05tntbe01    e ds                  extname(TNTBE01L : *key)
020900130206     d                                     prefix(k_)   inz
021000130206
021100130206       //--------------------------------------------------------------
021200130206       //?M A I N - L I N E                                            ?
021300130206       //--------------------------------------------------------------
021400130206
021500130206     c     *Entry        plist
021600130206     c                   parm                    KPJBA
021700130206
021800130206      /free
021900130206
022000130206       // -?Operazioni iniziali?
022100130206       exsr  sr_RoutInz;
022200130206
022300130206       // -?Ciclo di gestione del file video?
022400130206       DOW  $Fine = *off;
022500130206
022600130206         select;
022700130206           // -?Gestione RootFolder per immagini LdV?
022800130206           when $Video = 'D2';
022900130206             exsr  sr_GesD02;
023000130206           // -?? ? ??
023100130206           other;
023200130206             $Fine = *on;
023300130206         endsl;
023400130206
023500130206       ENDDO;
023600130206
023700130206       // -?Operazioni finali?
023800130206       exsr  sr_RoutEnd;
023900130206
024000130206       //--------------------------------------------------------------
024100130206       //?Operazioni iniziali.                                         ?
024200130206       //--------------------------------------------------------------
024300130206       BEGSR  *InzSr;
024400130206
024500130206         // -?Reperimento dati job?
024600130206         exsr  sr_DatiJob;
024700130206
024800130206         // -?Impostazione nome programma a video?
024900130206         V1Tpgm = SDSpgm;
025000130206
025100130206         // -?Reperimento data odierna?
025200130206         wDate = %int( %subst( %char( %dec( %timestamp() ) )
025300130206                               : 1 : 8 ) );
025400130206
025500130206       ENDSR;
025600130206
025700130206       //--------------------------------------------------------------
025800130206       //?Operazioni iniziali.                                         ?
025900130206       //--------------------------------------------------------------
026000130206       BEGSR  sr_RoutInz;
026100130206
026200130206         // -?Impostazione chiusura?
026300130206         *inLR = *on;
026400130206
026500130206         // -?Verifica del sistema AS/400 corrente?
026600130206         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
026700130206           $Fine = *on;
026800130206           leavesr;
026900130206         endif;
027000130206
027100130206         // -?Impostazione elenco librerie in cui gestire le tabelle?
027200130206         //  ?(a seconda del sistema in cui si stà lavorando)?
027300130206         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : $iSystem );
027400130206         if  w_iSystem = *zero;
027500130206           $Fine = *on;
027600130206           leavesr;
027700130206         endif;
027800130206
027900130206         // -?1ª apertura file TNTBE01L del 1° S.I. (sede)?
028000130206         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
028100130206           w_SisInf = 1;
028200130206           exsr  sr_OpenFile;
028300130206         endif;
028400130206
028500130206         // -?Aggancio dati generali della tabella in esame?
028600130206         clear  k_TBEcod;
028700130206         k_TBEke1 = *zero;
028800130206         %subst(k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab))
028900130206                = c_Tab;
029000130206         clear  k_TBEke2;
029100130206         clear  k_TBElin;
029200130206         k_TBEsif = KNSIF;
029300130206         chain(n)  %kds(k05tntbe01)  TNTBE000;
029400130206         if  not  %found(TNTBE01L);
029500130206           clear  k_TBEsif;
029600130206           chain(n)  %kds(k05tntbe01)  TNTBE000;
029700130206         endif;
029800130206         if  %found(TNTBE01L);
029900130206           xTNTBEds = TNTBEds;
030000130206         else;
030100130206           clear  xTNTBEds;
030200130206         endif;
030300130206
030400130206         // -?Impostazione iniziale / pulizia dei campi chiave?
030500130206         clear  k05tntbe01;
030600130206         k_TBEcod = c_Tab;
030700130206
030800130206         // -?Reperimento dati (a chiave "fissa")?
030900130206         clear  dILVsetrasOk;
031000130206         clear  dILVsetrasScar;
031100130206         clear  dILVas888Ok;
031200130206         clear  dILVas888Scar;
031300130206         clear  wTBEatbSETok;
031400130206         clear  wTBEatbSETscar;
031500130206         clear  wTBEatbAS8ok;
031600130206         clear  wTBEatbAS8scar;
031700130206
031800130206         k_TBEke1 = c_Setras;
031900130206         k_TBEke2 = c_Ok;
032000130206         chain  %kds(k05tntbe01)  TNTBE000;
032100130206         if  %found(TNTBE01L);
032200130206           wTBEatbSETok = TBEatb;
032300130206           if  TBEatb = *blank;
032400130206             dILVsetrasOk   = TBEuni;
032500130206           endif;
032600130206         endif;
032700130206
032800130206         k_TBEke1 = c_Setras;
032900130206         k_TBEke2 = c_Scarto;
033000130206         chain  %kds(k05tntbe01)  TNTBE000;
033100130206         if  %found(TNTBE01L);
033200130206           wTBEatbSETscar = TBEatb;
033300130206           if  TBEatb = *blank;
033400130206             dILVsetrasScar = TBEuni;
033500130206           endif;
033600130206         endif;
033700130206
033800130206         k_TBEke1 = c_As888;
033900130206         k_TBEke2 = c_Ok;
034000130206         chain  %kds(k05tntbe01)  TNTBE000;
034100130206         if  %found(TNTBE01L);
034200130206           wTBEatbAS8ok = TBEatb;
034300130206           if  TBEatb = *blank;
034400130206             dILVas888Ok    = TBEuni;
034500130206           endif;
034600130206         endif;
034700130206
034800130206         k_TBEke1 = c_As888;
034900130206         k_TBEke2 = c_Scarto;
035000130206         chain  %kds(k05tntbe01)  TNTBE000;
035100130206         if  %found(TNTBE01L);
035200130206           wTBEatbAS8scar = TBEatb;
035300130206           if  TBEatb = *blank;
035400130206             dILVas888Scar  = TBEuni;
035500130206           endif;
035600130206         endif;
035700130206
035800130206       ENDSR;
035900130206
036000130206       //--------------------------------------------------------------
036100130206       //?Reperimento Dati del job (Utente/Operativi).                 ?
036200130206       //--------------------------------------------------------------
036300130206       BEGSR  sr_DatiJob;
036400130206
036500130206         in(E) §AzUte;
036600130206         if NOT %error;
036700130206           in(E) §DatiUte;
036800130206         endif;
036900130206         if %error or RSut = *blanks;
037000130206           clear TIBS34ds;
037100130206           tibs34r ( tibs34ds );
037200130206           in §AzUte;
037300130206           in §DatiUte;
037400130206         endif;
037500130206
037600130206       ENDSR;
037700130206
037800130206       //--------------------------------------------------------------
037900130206       //?Apertura dei files tabelle nel sistema informativo impostato.?
038000130206       //--------------------------------------------------------------
038100130206       BEGSR  sr_OpenFile;
038200130206
038300130206         // -?Chiusura (eventuale) file tabelle?
038400130206         if  %open(TNTBE01L);
038500130206           close  TNTBE01L;
038600130206         endif;
038700130206
038800130206         // -?Apertura file tabelle?
038900130206         ds_Libr  = $Libraries(w_iSystem);
039000130206         wLibFile = %trimr( $Libr(w_SisInf) ) + '/' + 'TNTBE01L';
039100130206         open  TNTBE01L;
039200130206
039300130206       ENDSR;
039400130206
039500130206       //--------------------------------------------------------------
039600130206       //?Gestione videata D02 (unica).                                ?
039700130206       //--------------------------------------------------------------
039800130206       BEGSR  sr_GesD02;
039900130206
040000130206         // -?Inizializzazione videata?
040100130206         if  $InzD02 = *on;
040200130206           exsr  sr_InzD02;
040300130206           $InzD02 = *off;
040400130206         endif;
040500130206
040600130206         // -?Emissione Testata?
040700130206         write  TBILVT01;
040800130206
040900130206         // -?Emissione videata?
041000130206         exfmt  TBILVD02;
041100130206
041200130206         clear  V1Dmsg;
041300130206         reset  ErrMessage;
041400130206         reset  ErrGenerico;
041500130206
041600130206         SELECT;
041700130206
041800130206           // -?F3=Fine?
041900130206           WHEN  dsp_aid = c_F03;
042000130206             unlock  TNTBE01L;
042100130206             $Fine = *on;
042200130206
042300130206           // -?F11=Inversione Linea-BackUp?
042400130206           WHEN  dsp_aid = c_F11;
042500130206             exsr  sr_Inversione;
042600130206
042700130206           // -?Enter; F5=Ripristino; F6=Conferma; F16=Annullamento?
042800130206           OTHER;
042900130206             // -?Controlli eseguiti NON se F16=Annullamento?
043000130206             if  dsp_aid <> c_F16;
043100130206               exsr  sr_CtrD02;
043200130206             endif;
043300130206             if  ErrGenerico;
043400130206               leavesr;
043500130206             endif;
043600130206             // -?Aggiornamento?
043700130206             if  dsp_aid = c_F05   or
043800130206                 dsp_aid = c_F06   or
043900130206                 dsp_aid = c_F16;
044000130206               exsr  sr_UpdateTBE;
044100130206               select;
044200130206                 // -?Rilevato errore in fase di aggiornamento?
044300130206                 when  ErrGenerico = *on;
044400130206                   leavesr;
044500130206                 // -?Uscita altrimenti?
044600130206                 other;
044700130206                   $Fine = *on;
044800130206               endsl;
044900130206             endif;
045000130206
045100130206         ENDSL;
045200130206
045300130206       ENDSR;
045400130206
045500130206       //--------------------------------------------------------------
045600130206       //?Inizializzazione videata D02.                                ?
045700130206       //--------------------------------------------------------------
045800130206       BEGSR  sr_InzD02;
045900130206
046000130206         F5Attivo  = *off;
046100130206         F6Attivo  = *off;
046200130206         F11Attivo = *off;
046300130206
046400130206         // -?Spegnimento degli indicatori di errore?
046500130206         %subst(IndDspF : 50) = *off;
046600130206
046700130206         // -?Pulizia videata?
046800130206         clear  TBILVD02;
046900130206
047000130206         // -?Caricamento dati a video?
047100130206         if  wTBEatbSETok   = *blank  and  dILVsetrasOk   <> *blank;
047200130206           V1CSeOkLn = dILVsetrasOk.§ILVlinea;
047300130206           V1CSeOkBk = dILVsetrasOk.§ILVbckUp;
047400130206         endif;
047500130206         if  wTBEatbSETscar = *blank  and  dILVsetrasScar <> *blank;
047600130206           V1CSeScLn = dILVsetrasScar.§ILVlinea;
047700130206           V1CSeScBk = dILVsetrasScar.§ILVbckUp;
047800130206         endif;
047900130206         if  wTBEatbAS8ok   = *blank  and  dILVas888Ok    <> *blank;
048000130206           V1CA8OkLn = dILVas888Ok.§ILVlinea;
048100130206         endif;
048200130206         if  wTBEatbAS8scar = *blank  and  dILVas888Scar  <> *blank;
048300130206           V1CA8ScLn = dILVas888Scar.§ILVlinea;
048400130206         endif;
048500130206
048600130206         // -?Impostazione indicatori per abilitazione tasti funzionali?
048700130206         exsr  sr_AbilitFxxD02;
048800130206
048900130206       ENDSR;
049000130206
049100130206       //--------------------------------------------------------------
049200130206       //?Settaggio indicatori nella videata D02 per abilitazione      ?
049300130206       //?  tasti funzionali.                                          ?
049400130206       //--------------------------------------------------------------
049500130206       BEGSR  sr_AbilitFxxD02;
049600130206
049700130206         // -?F3=Fine (sempre abilitato)?
049800130206         //F3Attivo  = *on;
049900130206
050000130206         // -?F5=Ripristino?
050100130206         F5Attivo  = (wTBEatbSETok   <> *blank  and
050200130206                      wTBEatbSETscar <> *blank  and
050300130206                      wTBEatbAS8ok   <> *blank  and
050400130206                      wTBEatbAS8scar <> *blank);
050500130206
050600130206         // -?F6=Conferma?
050700130206         F6Attivo  = (NOT F5Attivo);
050800130206
050900130206         // ->?F11 = Inversione Linea-BackUp?
051000130206         F11Attivo = (V1CSeOkLn <> *blank  or  V1CSeScLn <> *blank  or
051100130206                      V1CSeOkBk <> *blank  or  V1CSeScBk <> *blank);
051200130206
051300130206         // -?F16=Annullamento?
051400130206         //F16Attivo = ( dILVsetrasOK   <> *blank  or
051500130206         //              dILVsetrasScar <> *blank);
051600130206
051700130206       ENDSR;
051800130206
051900130206       //--------------------------------------------------------------
052000130206       //?Controllo dati videata D02.                                  ?
052100130206       //--------------------------------------------------------------
052200130206       BEGSR  sr_CtrD02;
052300130206
052400130206         // -?Spegnimento degli indicatori di errore?
052500130206         %subst(IndDspF : 50) = *off;
052600130206
052700130206         // -?RootFolders obbligatori/errati?
052800130206         Select;
052900130206
053000130206           // -?SETRAS - Immagini OK - in Linea?
053100130206           When  V1CSeOkLn = *blank;
053200130206             V1Dmsg = $Msg(01);
053300130206             PosCurSeOkLn = *on;
053400130206             ErrMessage   = *on;
053500130206             ErrGenerico  = *on;
053600130206             leavesr;
053700130206           When  %subst( V1CSeOkLn : 1 : 2 ) <> '\\';
053800130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
053900130206             PosCurSeOkLn = *on;
054000130206             ErrMessage   = *on;
054100130206             ErrGenerico  = *on;
054200130206             leavesr;
054300130206           When  %subst( V1CSeOkLn : %len( %trimr(V1CSeOkLn) ) : 2 )
054400130206                 <> '\ ';
054500130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
054600130206             PosCurSeOkLn = *on;
054700130206             ErrMessage   = *on;
054800130206             ErrGenerico  = *on;
054900130206             leavesr;
055000130206
055100130206           // -?SETRAS - Immagini OK - BackUp?
055200130206           When  V1CSeOkBk = *blank;
055300130206             V1Dmsg = $Msg(01);
055400130206             PosCurSeOkBk = *on;
055500130206             ErrMessage   = *on;
055600130206             ErrGenerico  = *on;
055700130206             leavesr;
055800130206           When  %subst( V1CSeOkBk : 1 : 2 ) <> '\\';
055900130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
056000130206             PosCurSeOkBk = *on;
056100130206             ErrMessage   = *on;
056200130206             ErrGenerico  = *on;
056300130206             leavesr;
056400130206           When  %subst( V1CSeOkBk : %len( %trimr(V1CSeOkBk) ) : 2 )
056500130206                 <> '\ ';
056600130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
056700130206             PosCurSeOkBk = *on;
056800130206             ErrMessage   = *on;
056900130206             ErrGenerico  = *on;
057000130206             leavesr;
057100130206
057200130206           // -?SETRAS - Immagini a Scarto - in Linea?
057300130206           When  V1CSeScLn = *blank;
057400130206             V1Dmsg = $Msg(01);
057500130206             PosCurSeScLn = *on;
057600130206             ErrMessage   = *on;
057700130206             ErrGenerico  = *on;
057800130206             leavesr;
057900130206           When  %subst( V1CSeScLn : 1 : 2 ) <> '\\';
058000130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
058100130206             PosCurSeScLn = *on;
058200130206             ErrMessage   = *on;
058300130206             ErrGenerico  = *on;
058400130206             leavesr;
058500130206           When  %subst( V1CSeScLn : %len( %trimr(V1CSeScLn) ) : 2 )
058600130206                 <> '\ ';
058700130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
058800130206             PosCurSeScLn = *on;
058900130206             ErrMessage   = *on;
059000130206             ErrGenerico  = *on;
059100130206             leavesr;
059200130206
059300130206           // -?SETRAS - Immagini a Scarto - BackUp?
059400130206           When  V1CSeScBk = *blank;
059500130206             V1Dmsg = $Msg(01);
059600130206             PosCurSeScBk = *on;
059700130206             ErrMessage   = *on;
059800130206             ErrGenerico  = *on;
059900130206             leavesr;
060000130206           When  %subst( V1CSeScBk : 1 : 2 ) <> '\\';
060100130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
060200130206             PosCurSeScBk = *on;
060300130206             ErrMessage   = *on;
060400130206             ErrGenerico  = *on;
060500130206             leavesr;
060600130206           When  %subst( V1CSeScBk : %len( %trimr(V1CSeScBk) ) : 2 )
060700130206                                             <> '\ ';
060800130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
060900130206             PosCurSeScBk = *on;
061000130206             ErrMessage   = *on;
061100130206             ErrGenerico  = *on;
061200130206             leavesr;
061300130206
061400130206           // -?AS888 - Immagini OK - in Linea?
061500130206           When  V1CA8OkLn = *blank;
061600130206             V1Dmsg = $Msg(01);
061700130206             PosCurA8OkLn = *on;
061800130206             ErrMessage   = *on;
061900130206             ErrGenerico  = *on;
062000130206             leavesr;
062100130206           When  %subst( V1CA8OkLn : 1 : 2 ) <> '\\';
062200130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
062300130206             PosCurA8OkLn = *on;
062400130206             ErrMessage   = *on;
062500130206             ErrGenerico  = *on;
062600130206             leavesr;
062700130206           When  %subst( V1CA8OkLn : %len( %trimr(V1CA8OkLn) ) : 2 )
062800130206                 <> '\ ';
062900130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
063000130206             PosCurA8OkLn = *on;
063100130206             ErrMessage   = *on;
063200130206             ErrGenerico  = *on;
063300130206             leavesr;
063400130206
063500130206           // -?AS888 - Immagini a Scarto - in Linea?
063600130206           When  V1CA8ScLn = *blank;
063700130206             V1Dmsg = $Msg(01);
063800130206             PosCurA8ScLn = *on;
063900130206             ErrMessage   = *on;
064000130206             ErrGenerico  = *on;
064100130206             leavesr;
064200130206           When  %subst( V1CA8ScLn : 1 : 2 ) <> '\\';
064300130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve cominciare con "\\"';
064400130206             PosCurA8ScLn = *on;
064500130206             ErrMessage   = *on;
064600130206             ErrGenerico  = *on;
064700130206             leavesr;
064800130206           When  %subst( V1CA8ScLn : %len( %trimr(V1CA8ScLn) ) : 2 )
064900130206                 <> '\ ';
065000130206             V1Dmsg = %trimr( $Msg(02) ) + ': deve terminare con "\ "';
065100130206             PosCurA8ScLn = *on;
065200130206             ErrMessage   = *on;
065300130206             ErrGenerico  = *on;
065400130206             leavesr;
065500130206         EndSl;
065600130206
065700130206         // ->?F11 = Inversione Linea-BackUp?
065800130206         F11Attivo = (V1CSeOkLn <> *blank  or  V1CSeScLn <> *blank  or
065900130206                      V1CSeOkBk <> *blank  or  V1CSeScBk <> *blank);
066000130206
066100130206       ENDSR;
066200130206
066300130206       //--------------------------------------------------------------
066400130206       //?Gestione tasto funzionale F11=Inversione Linea-BackUp.       ?
066500130206       //--------------------------------------------------------------
066600130206       BEGSR  sr_Inversione;
066700130206
066800130206         clear  dILVAs888OK;
066900130206
067000130206         dILVas888OK.§ILVlinea = V1CSeOkLn;
067100130206         dILVas888OK.§ILVbckup = V1CSeScLn;
067200130206
067300130206         V1CSeOkLn = V1CSeOkBk;
067400130206         V1CSeScLn = V1CSeScBk;
067500130206
067600130206         V1CSeOkBk = dILVas888OK.§ILVlinea;
067700130206         V1CSeScBk = dILVas888OK.§ILVbckup;
067800130206
067900130206       ENDSR;
068000130206
068100130206       //--------------------------------------------------------------
068200130206       //?Aggiornam. tab. "3EW" nei file di tutti i sistemi informativi?
068300130206       //--------------------------------------------------------------
068400130206       BEGSR  sr_UpdateTBE;
068500130206
068600130206         // -?Ciclo di elaborazione per ogni sistema informativo?
068700130206         For  w_SisInf = 1  To  %elem($Libr);
068800130206
068900130206           // -?Apertura degli archivi?
069000130206           if  w_SisInf > 1;
069100130206             exsr  sr_OpenFile;
069200130206           endif;
069300130206
069400130206           // -?Aggiornamento tab. "ILV"?
069500130206           For  xx = 1  To  4;
069600130206             exsr  sr_UpdTNTBE;
069700130206           EndFor;
069800130206
069900130206         EndFor;
070000130206
070100130206       ENDSR;
070200130206
070300130206       //--------------------------------------------------------------
070400130206       //?Aggiornamento records TNTBE00F (tab. "ILV").                 ?
070500130206       //--------------------------------------------------------------
070600130206       BEGSR  sr_UpdTNTBE;
070700130206
070800130206         //? N.B.                                                      ?
070900130206         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
071000130206         //   trasmissione (vedi flag TBEFTR).                        ?
071100130206         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
071200130206         //   subito nello stesso file di entrambi i S.I. - in due    ?
071300130206         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
071400130206
071500130206         // -?RI-aggancio record da aggiornare?
071600130206         clear  k05tntbe01;
071700130206         k_TBEcod = c_Tab;
071800130206         select;
071900130206           when  xx = 1;
072000130206             k_TBEke1 = c_Setras;
072100130206             k_TBEke2 = c_Ok;
072200130206           when  xx = 2;
072300130206             k_TBEke1 = c_Setras;
072400130206             k_TBEke2 = c_Scarto;
072500130206           when  xx = 3;
072600130206             k_TBEke1 = c_As888;
072700130206             k_TBEke2 = c_Ok;
072800130206           when  xx = 4;
072900130206             k_TBEke1 = c_As888;
073000130206             k_TBEke2 = c_Scarto;
073100130206         endsl;
073200130206         chain  %kds(K05tntbe01)  TNTBE000;
073300130206
073400130206         // -?Aggiornamento singolo record della tab. "ILV"?
073500130206         select;
073600130206
073700130206           // -?F5=Ripristino?
073800130206           when  dsp_aid = c_F05;
073900130206             exsr  sr_RieRecTBE;
074000130206             select;
074100130206               when  %found(TNTBE01L)   and   TBEatb <> *blank;
074200130206                 clear  TBEatb;
074300130206                 //_______________
074400130206                 UPDATE  TNTBE000;
074500130206                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
074600130206               // -?Record appena cancellato fisicamente?
074700130206               when  NOT %found(TNTBE01L);
074800130206                 clear  TBEatb;
074900130206                 //_______________
075000130206                 WRITE   TNTBE000;
075100130206                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
075200130206             endsl;
075300130206
075400130206           // -?F6 = Aggiornamento / Inserimento?
075500130206           when  dsp_aid = c_F06;
075600130206             exsr  sr_RieRecTBE;
075700130206             clear  TBEatb;
075800130206             if  %found(TNTBE01L);
075900130206               //_____________
076000130206               UPDATE TNTBE000;
076100130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯
076200130206             else;
076300130206               //_____________
076400130206               WRITE  TNTBE000;
076500130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯
076600130206             endif;
076700130206
076800130206           // -?F16=Annullamento?
076900130206           when  dsp_aid = c_F16;
077000130206             if  %found(TNTBE01L)   and   TBEatb <> 'A';
077100130206               TBEatb = 'A';
077200130206               //TBEftt = W1ftt;
077300130206               TBEftt = 'S';
077400130206               clear  TBEftr;
077500130206               //_______________
077600130206               UPDATE  TNTBE000;
077700130206               //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
077800130206             endif;
077900130206
078000130206         endsl;
078100130206
078200130206       ENDSR;
078300130206
078400130206       //--------------------------------------------------------------
078500130206       //?Caricamento dati in tab. "3EW".                              ?
078600130206       //--------------------------------------------------------------
078700130206       BEGSR  sr_RieRecTBE;
078800130206
078900130206         // -?Impostazione campi chiave se inserimento?
079000130206         if  NOT %found(TNTBE01L);
079100130206           clear TNTBE000;
079200130206           TBEcod = k_TBEcod;
079300130206           TBEke1 = k_TBEke1;
079400130206           TBEke2 = k_TBEke2;
079500130206           TBElin = k_TBElin;
079600130206           TBEsif = k_TBEsif;
079700130206         endif;
079800130206
079900130206         // -?Impostazione dati tabella da aggiornare?
080000130206         select;
080100130206           when  xx = 1;
080200130206             clear  dILVsetrasOK;
080300130206             dILVsetrasOK.§ILVlinea = V1CSeOkLn;
080400130206             dILVsetrasOK.§ILVbckup = V1CSeOkBk;
080500130206             TBEuni = dILVsetrasOk;
080600130206           when  xx = 2;
080700130206             clear  dILVsetrasScar;
080800130206             dILVsetrasScar.§ILVlinea = V1CSeScLn;
080900130206             dILVsetrasScar.§ILVbckup = V1CSeScBk;
081000130206             TBEuni = dILVsetrasScar;
081100130206           when  xx = 3;
081200130206             clear  dILVAs888OK;
081300130206             dILVas888OK.§ILVlinea = V1CA8OkLn;
081400130206             TBEuni = dILVas888Ok;
081500130206           when  xx = 4;
081600130206             clear  dILVAs888Scar;
081700130206             dILVas888Scar.§ILVlinea = V1CA8ScLn;
081800130206             TBEuni = dILVas888Scar;
081900130206         endsl;
082000130206
082100130206         // -?Impostazione altri campi del record?
082200130206         TBEapl = TBXapl;
082300130206         //clear  TBEatb;       //Sarà impostato caso per caso...?
082400130206         //TBEftt = W1ftt;
082500130206         TBEftt = 'S';
082600130206         clear  TBEflt;
082700130206         if  w_SisInf = 1;
082800130206           TBEftr = 'T';
082900130206         else;
083000130206           TBEftr = 'R';
083100130206         endif;
083200130206         TBEdtr = wDate;
083300130206
083400130206       ENDSR;
083500130206
083600130206       //--------------------------------------------------------------
083700130206       //?Operazioni finali.                                           ?
083800130206       //--------------------------------------------------------------
083900130206       BEGSR  sr_RoutEnd;
084000130206
084100130206         // -?Chiusura pgm?
084200130206         return;
084300130206
084400130206       ENDSR;
084500130206
084600130206      /end-free
084700130206
084800130206       //--------------------------------------------------------------
084900130206       //?Definizione schiere a tempo di compilazione                  ?
085000130206       //--------------------------------------------------------------
085100130206
085200130206** -?$iSystem / $Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
085300130206SETRAS  GAITRAGRU FILTRAGRU
085400130206AS888   GAITRAGRPSFILTRAGRPF
085500130206** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
085600130206RootFolder obbligatorio                                                         1
085700130206Nome RootFolder con caratteri errati                                            2
