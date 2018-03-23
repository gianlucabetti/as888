000100160614       //==============================================================
000200160614       //?Gestione tabella "DRM" (Dati per Report SIM Magazzino).      ?
000300160614       //==============================================================
000400160614
000500160614       //--------------------------------------------------------------
000600160614       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700160614       //--------------------------------------------------------------
000800160614
000900160614     /*PRM  dbgview(*source)
001000160614     /*END
001100160614
001200160614       //--------------------------------------------------------------
001300160614       //?Specifiche di controllo.                                     ?
001400160614       //--------------------------------------------------------------
001500160614
001600160614     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700160614     h dftactgrp(*no)
001800160902     h bnddir('UBBNDDIR')
001900160614
002000160614       //--------------------------------------------------------------
002100160614       //?Dichiarazione file.                                          ?
002200160614       //--------------------------------------------------------------
002300160614
002400160614       // -?Organigramma?
002500160614     fAZORG01L  if   e           k disk
002600160614
002700160614       // -?Tabelle?
002800160902     fTNTBE01L  Uf A e           k disk    extfile(wLibFile)
002900160902     f                                     usropn
003000160614
003100160614       // -?Video?
003200160614     fTNTBDRMD  cf   e             workstn
003300160614     f                                     sfile(tbDRMs01 : S01nrr)
003400160614     f                                     indds(IndDspF)
003500160614     f                                     infds(InfDspF)
003600160614
003700160614       //--------------------------------------------------------------
003800160614       //?Definizione costanti.                                        ?
003900160614       //--------------------------------------------------------------
004000160614
004100160614       // -?Codice tabella in gestione?
004200160614     d c_Tab           c                   const('DRM')
004300160614
004400160614       // -?Numero di record in ciascuna videata di subfile?
004500160614     d c_SflPag        c                   const(15)
004600160614
004700160614       // -?Numero massimo di record gestibili?
004800160614     d c_MaxRec        c                   const(9999)
004900160729
005000160729       // -?Attributi di Visualizzazione?
005100160729     d c_DspAtrNorm    c                   const(x'A0')
005200160729     d c_DspAtrRI      c                   const(x'A1')
005300160729     d c_DspAtrHI      c                   const(x'A2')
005400160729     d c_DspAtrND      c                   const(x'A7')
005500160729     d c_DspAtrRED     c                   const(x'A8')
005600160729     d c_DspAtrRI_RED  c                   const(x'A9')
005700160729     d c_DspAtrBL_RED  c                   const(x'AA')
005800160614
005900160614       // -?Tasti funzionali a video?
006000160614     d c_F01           c                   const(x'31')
006100160614     d c_F02           c                   const(x'32')
006200160614     d c_F03           c                   const(x'33')
006300160614     d c_F04           c                   const(x'34')
006400160614     d c_F05           c                   const(x'35')
006500160614     d c_F06           c                   const(x'36')
006600160614     d c_F07           c                   const(x'37')
006700160614     d c_F08           c                   const(x'38')
006800160614     d c_F09           c                   const(x'39')
006900160614     d c_F10           c                   const(x'3A')
007000160614     d c_F11           c                   const(x'3B')
007100160614     d c_F12           c                   const(x'3C')
007200160614     d c_F13           c                   const(x'B1')
007300160614     d c_F14           c                   const(x'B2')
007400160614     d c_F15           c                   const(x'B3')
007500160614     d c_F16           c                   const(x'B4')
007600160614     d c_F17           c                   const(x'B5')
007700160614     d c_F18           c                   const(x'B6')
007800160614     d c_F19           c                   const(x'B7')
007900160614     d c_F20           c                   const(x'B8')
008000160614     d c_F21           c                   const(x'B9')
008100160614     d c_F22           c                   const(x'BA')
008200160614     d c_F23           c                   const(x'BB')
008300160614     d c_F24           c                   const(x'BC')
008400160614     d c_Enter         c                   const(x'F1')
008500160614     d c_RollDown      c                   const(x'F4')
008600160614     d c_RollUp        c                   const(x'F5')
008700160902
008800160902       // -?Costanti per la definizione delle schiere con i nomi?
008900160902       //  ?degli iSeries da elaborare e delle relative librerie?
009000160902     d c_NrSyst        c                   const(2)
009100160902     d c_NrLibr        c                   const(2)
009200160614
009300160614       //--------------------------------------------------------------
009400160614       //?Definizione schiere.                                         ?
009500160614       //--------------------------------------------------------------
009600160902
009700160902       // -?iSeries  &  Librerie con entrambi i file tabelle?
009800160902     d sk_iSystem      s                   like(currSysNetA)
009900160902     d                                     dim(c_NrSyst)
010000160902     d                                     ctdata   perrcd( 1)
010100160902     d sk_Libraries    s                   like(ds_Libr)
010200160902     d                                     dim(c_NrSyst)
010300160902     d                                     alt(sk_iSystem)
010400160614
010500160614       // -?Messaggi di errore?
010600160614     d sk_Msg          s             78    dim( 8)  ctdata  perrcd( 1)
010700160614
010800160614       //--------------------------------------------------------------
010900160614       //?Definizione aree dati.                                       ?
011000160614       //--------------------------------------------------------------
011100160614
011200160614       // -?Dati utente?
011300160614     d §AzUte        e ds                  extname(AZUTE00F)
011400160614     d                                     dtaara
011500160614     d §DatiUte      e ds                  extname(dDatiUte)
011600160614     d                                     dtaara
011700160614
011800160614       //--------------------------------------------------------------
011900160614       //?Definizione strutture dati.                                  ?
012000160614       //--------------------------------------------------------------
012100160614
012200160614       // -?Status ds?
012300160614     d Status         sds
012400160614     d   SDSpgm          *proc
012500160614
012600160614       // -?InfDS?
012700160614     d InfDspF         ds
012800160614     d   dsp_aid             369    369a
012900160614     d*//sfl_rrn             376    377i 0
013000160614     d*//min_nrr             378    379i 0
013100160614     d*//num_rcds            380    381i 0
013200160614
013300160614       // -?Indicatori su DspF?
013400160614     d IndDspF         ds                  inz
013500160614         // -?Abilitazione tasti funzionali?
013600160614     d   $F3Attivo                     n   overlay( IndDspF : 03 )
013700160614     d   $F5Attivo                     n   overlay( IndDspF : 05 )
013800160614     d   $F6Attivo                     n   overlay( IndDspF : 06 )
013900160614     d   $F10Attivo                    n   overlay( IndDspF : 10 )
014000160614     d   $F12Attivo                    n   overlay( IndDspF : 12 )
014100160614     d   $F16Attivo                    n   overlay( IndDspF : 16 )
014200160614         // -?Emissione messaggio di errore?
014300160614     d   $ErrMessage                   n   overlay( IndDspF : 28 )
014400160614         // -?Indicatori di gestione del subfile?
014500160614     d   $SflDsp_N                     n   overlay( IndDspF : 30 )
014600160614     d   $SflDspCtl_N                  n   overlay( IndDspF : 31 )
014700160614     d   $SflNxtChg                    n   overlay( IndDspF : 32 )
014800160614     d   $SflEnd                       n   overlay( IndDspF : 33 )
014900160614         // -?Indicatori per Attributi di visualizzazione?
015000160614     d*//$DisabilOPZ                   n   overlay( IndDspF : 40 )
015100160614     d   $ProtectKEY                   n   overlay( IndDspF : 41 )
015200160614         // -?Posizionamento cursore & segnalazione errore?
015300160614     d   $PosCurOPZ                    n   overlay( IndDspF : 50 )
015400160614     d   $PosCurFIL                    n   overlay( IndDspF : 51 )
015500160614     d   $PosCurAPL                    n   overlay( IndDspF : 52 )
015600160614     d   $PosCurRPVP                   n   overlay( IndDspF : 53 )
015700160614     d   $PosCurFISSI                  n   overlay( IndDspF : 54 )
015800160614         // -?Riemissione videata?
015900160614     d   $ErrGenerico                  n   overlay( IndDspF : 99 )
016000160902
016100160902       // -?Ridefinizione elenco librerie in cui elaborare le tabelle?
016200160902     d ds_Libr         ds                  inz
016300160902     d   sk_Libr                     10    dim(c_NrLibr) inz
016400160614
016500160614       // -?Parametri ricevuti?
016600160614     d KPJBA         e ds
016700160711     d TNTBDRMds     e ds                  inz  qualified
016800160614
016900160614       // -?Dati record principale della tabella?
017000160614     d TNTBEds       e ds                  inz  extname(TNTBE00F)
017100160614     d xTNTBEds      e ds                  inz  extname(TNTBE00F)
017200160614     d                                          prefix(TBX:3)
017300160614
017400160614       // -?Tabelle utilizzate:?
017500160614       // ·?"DRM" = Filiali - Dati per Report Magazzino?
017600160614     d dDRM          e ds                  inz  qualified
017700160614       // ·?"MFP" = Filiali - Applicazioni Installate?
017800160614     d dMFP          e ds                  inz  qualified
017900160614       // ·?"MTP" = Macchinone (VdL) - Tipi Applicazione?
018000160614     d dMTP          e ds                  inz  qualified
018100160614
018200160614       //--------------------------------------------------------------
018300160614       //?Definizione variabili globali.                               ?
018400160614       //--------------------------------------------------------------
018500160614
018600160614       // -?Flags booleani?
018700160614     d $Called         s               n   inz
018800160614     d $Fine           s               n   inz
018900160614     d $InzS01         s               n   inz(*on)
019000160614     d $InzD02         s               n   inz(*on)
019100160614     d $InzW01         s               n   inz(*on)
019200160614     d $Annullamento   s               n   inz
019300160614     d $Copia          s               n   inz
019400160614     d $Immissione     s               n   inz
019500160614     d $Ripristino     s               n   inz
019600160614     d $Protect        s               n   inz
019700160614
019800160614       // -?Variabili per la gestione del video?
019900160614     d $Video          s              2    inz('S1')
020000160614     d S01nrr          s                   like(C1RcdNbr) inz
020100160614     d SavS01csr       s                   like(C1RcdNbr) inz
020200160614
020300160614       // -?Variabili per la gestione del posizionamento nel subfile?
020400160614     d SavC1Ccod       s                   like(C1Ccod)   inz
020500160614
020600160614       // -?Conteggio selezioni (opz. 1) inserite?
020700160614     d wMemoSel        s                   like(S1Copz)   inz
020800160902
020900160902       // -?Nome esteso Libreria/File dei 2 file tabella?
021000160902     d wLibFile        s             21a   inz
021100160614
021200160614       // -?Campi di comodo?
021300160902     d w_iSystem       s              1  0 inz
021400160902     d w_SisInf        s              3  0 inz
021500160614     d wDate_EUR       s               d   datfmt(*eur)   inz(*loval)
021600160614     d wDate           s              8s 0 inz
021700160614
021800160614       //--------------------------------------------------------------
021900160614       //?Definizione prototipi procedure.                             ?
022000160614       //--------------------------------------------------------------
022100160902
022200160902       // -?Reperimento NETA sistema AS/400 corrente?
022300160902     d currSysNeta     s              8a   inz
022400160902      /copy gaitrasrc/srcProtoPR,UBRTVNETA
022500160614
022600160614       // -?Reperimento dati utente?
022700160614     d TIBS34ds      e ds
022800160614      /copy gaitrasrc/srcProtoPR,TIBS34R
022900160614
023000160614       // -?Ricerca e controllo nuove tabelle?
023100160614     d TIBS02ds      e ds                  inz
023200160614     d   T02mod      e                     inz('C')
023300160614      /copy gaitrasrc/srcProtoPR,TIBS02R
023400160614
023500160614       //--------------------------------------------------------------
023600160614       //?Definizione key-list.                                        ?
023700160614       //--------------------------------------------------------------
023800160614
023900160614       // -?File TNTBE01L?
024000160614     d keyTNTBE01    e ds                  extname( TNTBE01L : *key )
024100160614     d                                     prefix( k_ )   inz
024200160614
024300160614       //--------------------------------------------------------------
024400160614       //?Riepilogo indicatori utilizzati.                             ?
024500160614       //--------------------------------------------------------------
024600160614       //--------------------------------------------------------------
024700160614
024800160614       //--------------------------------------------------------------
024900160614       //?M A I N - L I N E                                            ?
025000160614       //--------------------------------------------------------------
025100160614
025200160614     c     *Entry        plist
025300160614     c                   parm                    KPJBA
025400160614
025500160614      /free
025600160614
025700160614       // -?Operazioni iniziali?
025800160614       exsr  sr_RoutInz;
025900160614
026000160614       // -?Ciclo di gestione del file video?
026100160614       DOW  $Fine = *off;
026200160614
026300160614         select;
026400160614
026500160614           // -?Gestione videata S1 (subfile)?
026600160614           when $Video = 'S1';
026700160614             exsr sr_GesS01;
026800160614
026900160614           // -?Manutenzione dati della singola tabella?
027000160614           when $Video = 'D2';
027100160614             exsr  sr_GesD02;
027200160614
027300160902           //*·// -?Richiesta dati per trasmissione record?
027400160902           //*·when $Video = 'W1';
027500160902           //*·  exsr  sr_GesW01;
027600160614
027700160614           // -?? ? ??
027800160614           other;
027900160614             $Fine = *on;
028000160614
028100160614         endsl;
028200160614
028300160614       ENDDO;
028400160614
028500160614       // -?Operazioni finali?
028600160614       exsr  sr_RoutEnd;
028700160614
028800160614       //--------------------------------------------------------------
028900160614       //?Operazioni iniziali.                                         ?
029000160614       //--------------------------------------------------------------
029100160614       BEGSR  sr_RoutInz;
029200160614
029300160614         // -?Impostazione chiusura?
029400160614         *inLR = *on;
029500160902
029600160902         // -?Verifica del sistema AS/400 corrente?
029700160902         if  UBRTVNETA_Rtv(currSysNeta) <> *zero;
029800160902           $Fine = *on;
029900160902           leavesr;
030000160902         endif;
030100160614
030200160614         // -?Reperimento dati job?
030300160614         exsr  sr_DatiJob;
030400160614
030500160614         // -?Reperimento data odierna?
030600160902         //*·wDate = %dec( %date() );
030700160902         wDate = %int( %subst( %char( %dec( %timestamp() ) )
030800160902                               : 1 : 8 ) );
030900160614
031000160614         // -?Impostazione nome programma a video?
031100160614         V1Tpgm = SDSpgm;
031200160902
031300160902         // -?Impostazione elenco librerie in cui gestire le tabelle?
031400160902         //  ?(a seconda del sistema in cui si stà lavorando)?
031500160902         w_iSystem = %lookup ( %subst ( currSysNeta : 1 : 6 ) : sk_iSystem );
031600160902         if  w_iSystem = *zero;
031700160902           $Fine = *on;
031800160902           leavesr;
031900160902         endif;
032000160902
032100160902         // -?1ª apertura file TNTBE00F del 1° S.I. (sede)?
032200160902         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
032300160902           w_SisInf = 1;
032400160902           exsr  sr_OpenFileTab;
032500160902         endif;
032600160614
032700160614         // -?Aggancio dati generali della tabella in esame?
032800160614         clear  keyTNTBE01;
032900160614         k_TBEke1 = *zero;
033000160614         %subst( k_TBEke1 : %len(k_TBEke1)-%len(c_Tab)+1 : %len(c_Tab) )
033100160614                  = c_Tab;
033200160614         k_TBEsif = KNSIF;
033300160614         chain(n)  %kds( keyTNTBE01 )  TNTBE000;
033400160614         if  not  %found(TNTBE01L);
033500160614           clear  k_TBEsif;
033600160614           chain(n)  %kds( keyTNTBE01 )  TNTBE000;
033700160614         endif;
033800160614         if  %found(TNTBE01L);
033900160614           xTNTBEds = TNTBEds;
034000160614         else;
034100160614           clear  xTNTBEds;
034200160614         endif;
034300160614
034400160614         // -?Verifica parametri ricevuti?
034500160614         $Called = ( KPJBU <> *blank );
034600160711         if  $Called;
034700160711           tntbDRMds = kpjbu;
034800160711         else;
034900160711           clear  tntbDRMds;
035000160711         endif;
035100160614
035200160614         // -?Pulizia parametri di output?
035300160711         clear  tntbDRMds.oDRMfil;
035400160711         clear  tntbDRMds.oDRMfxx;
035500160711         tntbDRMds.oDRMerr = *off;
035600160711         clear  tntbDRMds.oDRMmsg;
035700160614
035800160614         // -?Impostazione videata iniziale?
035900160711         if  $Called  and  ( tntbDRMds.iDRMopz = '2'
036000160711                       or    tntbDRMds.iDRMopz = '5' );
036100160711           $Video = 'D2';
036200160711           S1Copz = tntbDRMds.iDRMopz;
036300160711           S1Cfil = tntbDRMds.iDRMfil;
036400160711           $ProtectKEY   = ( tntbDRMds.iDRMopz = '2' );
036500160711           $Protect      = ( tntbDRMds.iDRMopz = '5' );
036600160711           $Annullamento = *off;
036700160711           $Copia        = *off;
036800160711           $Immissione   = *off;
036900160711           $Ripristino   = *off;
037000160711         else;
037100160711           $Video = 'S1';
037200160711         endif;
037300160614         reset  $InzS01;
037400160711         reset  $InzD02;
037500160614         //*$DisabilOpz = *off;
037600160614
037700160614         // -?Impostazione iniziale / pulizia dei campi chiave?
037800160614         clear  keyTNTBE01;
037900160614         k_TBEcod = c_Tab;
038000160614
038100160614       ENDSR;
038200160614
038300160614       //--------------------------------------------------------------
038400160614       //?Reperimento Dati del job (Utente/Operativi).                 ?
038500160614       //--------------------------------------------------------------
038600160614       BEGSR  sr_DatiJob;
038700160614
038800160614         in(E) §AzUte;
038900160614         if NOT %error;
039000160614           in(E) §DatiUte;
039100160614         endif;
039200160614         if %error or RSut = *blanks;
039300160614           clear TIBS34ds;
039400160614           tibs34r ( tibs34ds );
039500160614           in §AzUte;
039600160614           in §DatiUte;
039700160614         endif;
039800160614
039900160614       ENDSR;
040000160614
040100160614       //--------------------------------------------------------------
040200160614       //?Gestione subfile S01                                         ?
040300160614       //--------------------------------------------------------------
040400160614       BEGSR  sr_GesS01;
040500160614
040600160614         // -?Inizializzazione videata?
040700160614         if  $InzS01 = *on;
040800160614           exsr  sr_InzS01;
040900160614           $InzS01 = *off;
041000160614         endif;
041100160614
041200160614         // -?(Dis)Abilitazione tasti funzionali?
041300160614         exsr  sr_AbilitFxxC01;
041400160614
041500160614         // -?Emissione Testata e Piede con tasti funzionali abilitati?
041600160706         clear  V1Topz;
041700160614         write  tbDRMt01;
041800160614         write  tbDRMp01;
041900160614
042000160614         // -?Posizionamento cursore?
042100160614         //  ?C1CsrRrn contiene il numero di riga del subfile su cui?
042200160614         //  ?era posizionato il cursore; impostandolo in C1RcdNbr?
042300160614         //  ?si visualizza la pagina che vedeva l'utente quando ha?
042400160614         //  ?premuto l'ultimo tasto.?
042500160614         if  C1CsrRrn > *zero;
042600160614           C1RcdNbr = C1CsrRrn;
042700160614         else;
042800160614           C1RcdNbr = 1;
042900160614           write  tbDRMs00;       //?(no rec.)?
043000160614         endif;
043100160614
043200160614         // -?Emissione videata (testata compresa) in window?
043300160614         exfmt  tbDRMc01;
043400160614
043500160614         clear  V1Dmsg;
043600160614         reset  $ErrMessage;
043700160614         reset  $ErrGenerico;
043800160614
043900160614         reset  $Copia;
044000160614         reset  $Immissione;
044100160614         reset  $Ripristino;
044200160614
044300160614         SELECT;
044400160614
044500160614           // -?F3=Fine?
044600160614           WHEN  dsp_aid = c_F03;
044700160711             tntbDRMds.oDRMfxx = '03';
044800160614             $Fine   = *on;
044900160614
045000160614           // -?F10=Inserimento Filiale?
045100160614           WHEN  dsp_aid = c_F10;
045200160614             $Immissione = *on;
045300160614             $Video  = 'D2';
045400160614             $InzD02 = *on;
045500160614
045600160614           // -?F12=Ritorno?
045700160614           WHEN  dsp_aid = c_F12;
045800160711             tntbDRMds.oDRMfxx = '12';
045900160614             $Fine   = *on;
046000160614
046100160614           // -?Subfile vuoto?
046200160614           WHEN  S01nrr = *zero;
046300160614             if  C1Ccod  <> SavC1Ccod;
046400160614               $InzS01 = *on;
046500160614             endif;
046600160614
046700160614           // -?Invio?
046800160614           OTHER;
046900160614             // -?Gestione opzioni?
047000160614             exsr  sr_OpzS01;
047100160614             select;
047200160614               when  $ErrGenerico;
047300160614                 leavesr;
047400160614               when  wMemoSel > *zero;
047500160614                 $Fine = *on;
047600160614                 leavesr;
047700160614             endsl;
047800160614             // -?Richiesto Posizionamento?
047900160614             if  C1Ccod <> SavC1Ccod;
048000160614               $InzS01 = *on;
048100160614               SavC1Ccod = C1Ccod;
048200160614             endif;
048300160614
048400160614         ENDSL;
048500160614
048600160614       ENDSR;
048700160614
048800160614       //--------------------------------------------------------------
048900160614       //?Inizializzazione subfile S01                                 ?
049000160614       //--------------------------------------------------------------
049100160614       BEGSR  sr_InzS01;
049200160614
049300160614         // -?Spegnimento degli indicatori di errore?
049400160614         %subst( IndDspF : 50 ) = *off;
049500160614
049600160614         // -?Pulizia subfile?
049700160614         $SflDsp_N    = *on;
049800160614         $SflDspCtl_N = *on;
049900160614         write  tbDRMc01;
050000160614         $SflDspCtl_N = *off;
050100160614         $SflEnd      = *off;
050200160614
050300160614         clear  C1RcdNbr;
050400160614         clear  C1CsrRrn;
050500160614         clear  S01nrr;
050600160614         clear  V1Dmsg;
050700160614         $ErrMessage  = *off;
050800160614         $ErrGenerico = *off;
050900160614
051000160614         $SflNxtChg = *off;
051100160902
051200160902         // -?Apertura file TNTBE00F del 1° S.I. (sede)?
051300160902         if  w_SisInf <> 1   or   Not %open(TNTBE01L);
051400160902           w_SisInf = 1;
051500160902           exsr  sr_OpenFileTab;
051600160902         endif;
051700160614
051800160614         // -?Caricamento completo dei dati nel subfile (per codice)?
051900160614         //  ?(occorre caricarlo al COMPLETO per riuscire a gestirne?
052000160614         //  ?l'eventuale ordinamento)?
052100160902         clear  keyTNTBE01;
052200160902         k_TBEcod = c_Tab;
052300160614         k_TBEke1 = %editc( C1Ccod : 'X' );
052400160614         setll  %kds( keyTNTBE01 )  TNTBE000;
052500160614         reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
052600160614
052700160614         DoW  Not %eof(TNTBE01L)  and  S01nrr < c_MaxRec;
052800160614           //if  TBEatb = *blank;
052900160614             exsr  sr_WriteS01;
053000160614           //endif;
053100160614           reade(N)  %kds( keyTNTBE01 : 1 )  TNTBE000;
053200160614         EndDo;
053300160614
053400160614         // -?Visualizzazione del SFL (se ci sono dati)?
053500160614         $SflDsp_N = ( S01nrr <= *zero );
053600160614
053700160614         // -?Attivazione del SFLEND?
053800160614         $SflEnd   = %eof(TNTBE01L);
053900160614
054000160614         // -?Posizionamento cursore al 1° rcd della 1ª pagina?
054100160614         if  S01nrr > *zero;
054200160614           C1RcdNbr = 1;
054300160614         else;
054400160614           clear  C1RcdNbr;
054500160614         endif;
054600160614
054700160614         // -?Impostazione Opzioni?
054800160711         if  $Called;
054900160711           C1Dopz = '1=Selezione, 5=Visualizzazione';
055000160711         else;
055100160711           C1Dopz = '2=Modifica, 3=Copia, 5=Visualizzazione';
055200160711         endif;
055300160614
055400160614         // -?Emissione messaggio di segnalazione raggiungimento limite?
055500160614         if  S01nrr >= c_MaxRec   and   Not %eof(TNTBE01L);
055600160614           $ErrGenerico = *on;
055700160614           $ErrMessage  = *on;
055800160614           $PosCurOPZ   = *on;
055900160614           V1Dmsg = sk_Msg(01);
056000160614           leavesr;
056100160614         endif;
056200160614
056300160614       ENDSR;
056400160614
056500160614       //--------------------------------------------------------------
056600160614       //?Registrazione del singolo rec. nel subfile S01               ?
056700160614       //--------------------------------------------------------------
056800160614       BEGSR  sr_WriteS01;
056900160614
057000160614         // -?Eventuale posizionamento richiesto?
057100160614         if  TBEke1 < %editc( C1Ccod : 'X' );
057200160614           leavesr;
057300160614         endif;
057400160614
057500160614         dDRM = TBEuni;
057600160614
057700160614         // -?Impostazione campi nel record del subfile?
057800160614         clear  tbDRMs01;
057900160614         S1Catb   = TBEatb;
058000160614         S1Cfil   = %int( %trimR( TBEke1 ) );
058100160614         S1Capl   = dDRM.§DRMaPri;
058200160614         S1Crpvp  = dDRM.§DRMaRPVP;
058300160614         S1Cfisse = dDRM.§DRMaFiss;
058400160614
058500160614         chain  ( S1Cfil )  AZORG;
058600160614         if  %found(AZORG01L);
058700160614           S1Dfil = ORGdes;
058800160614         else;
058900160614           S1Dfil = *all'? ';
059000160614         endif;
059100160614
059200160614         // -?Registrazione singolo record nel subfile?
059300160614         S01nrr += 1;
059400160614         write  tbDRMs01;
059500160614
059600160614       ENDSR;
059700160614
059800160614       //--------------------------------------------------------------
059900160614       //?Settaggio indicatori nella videata C01/S01 per abilitazione  ?
060000160614       //?  tasti funzionali.                                          ?
060100160614       //--------------------------------------------------------------
060200160614       BEGSR  sr_AbilitFxxC01;
060300160614
060400160614         // ·?F3 = Fine?
060500160614         $F3Attivo  = ( Not $Called );
060600160614
060700160614         // ·?F10 = Immissione?
060800160614         $F10Attivo = ( Not $Called );
060900160614
061000160614         // ·?F12 = Ritorno?
061100160614         $F12Attivo = ( $Called );
061200160614
061300160614       ENDSR;
061400160614
061500160614       //--------------------------------------------------------------
061600160614       //?Gestione opzioni del subfile S01                             ?
061700160614       //--------------------------------------------------------------
061800160614       BEGSR  sr_OpzS01;
061900160614
062000160614         // -?Spegnimento degli indicatori di errore?
062100160614         %subst(IndDspF : 50) = *off;
062200160614
062300160614         clear  SavS01csr;
062400160614         clear  wMemoSel;
062500160614
062600160614         // -?Ciclo di lettura subfile?
062700160614         readc  tbDRMs01;
062800160614
062900160614         DoW  Not %eof(TNTBDRMD);
063000160614
063100160614           $SflNxtChg = *off;
063200160614           %subst( IndDspF : 50 ) = *off;
063300160614           C1RcdNbr   = S01nrr;
063400160614
063500160614           Select;
063600160614
063700160614             // -?Nessuna opzione?
063800160614             When  S1Copz = *blank;
063900160614
064000160614             // -?Opz. 1=Selezione filiale?
064100160711             When  $Called  and  tntbDRMds.iDRMopz = '1'
064200160711                            and  S1Copz = '1';
064300160614               // -?Già selezionata una filiale?
064400160614               if  wMemoSel > *zero;
064500160614                 $ErrGenerico = *on;
064600160614                 $ErrMessage  = *on;
064700160614                 $PosCurOPZ   = *on;
064800160614                 V1Dmsg   = sk_Msg(03);
064900160614               else;
065000160614                 wMemoSel = S1Copz;
065100160614               endif;
065200160614
065300160614             // -?Opz. 2=Modifica/Annullamento/Ripristino?
065400160614             When  Not $Called  and  S1Copz = '2';
065500160614               $ProtectKEY = *on;
065600160614               $InzD02     = *on;
065700160614               $Video      = 'D2';
065800160614               DoW  $Video = 'D2';
065900160614                 exsr  sr_GesD02;
066000160614               EndDo;
066100160614               clear  $ProtectKEY;
066200160614
066300160614             // -?Opz. 3=Copia?
066400160614             When  Not $Called  and  S1Copz = '3';
066500160614               $Copia      = *on;
066600160614               $ProtectKEY = *off;
066700160614               $InzD02     = *on;
066800160614               $Video      = 'D2';
066900160614               DoW  $Video = 'D2';
067000160614                 exsr  sr_GesD02;
067100160614               EndDo;
067200160614               clear  $ProtectKEY;
067300160614
067400160614             // -?Opz. 5=Visualizzazione?
067500160711             When  S1Copz = '5';
067600160614               $Protect    = *on;
067700160614               $InzD02     = *on;
067800160614               $Video      = 'D2';
067900160614               DoW  $Video = 'D2';
068000160614                 exsr  sr_GesD02;
068100160614               EndDo;
068200160614               clear  $Protect;
068300160614
068400160614             // -?Opzione errata?
068500160614             Other;
068600160614               $ErrGenerico = *on;
068700160614               $ErrMessage  = *on;
068800160614               $PosCurOPZ   = *on;
068900160614               V1Dmsg = sk_Msg(02);
069000160614
069100160614           EndSl;
069200160614
069300160614           // -?Memorizzazione nrr del sfl con la 1ª opzione per?
069400160614           //  ?riposizionarvici il cursore dopo il tasto "Invio"?
069500160614           if  S1Copz   <> *blank  and
069600160614              (SavS01csr = *zero   or  SavS01csr < C1RcdNbr);
069700160614             SavS01csr   = C1RcdNbr;
069800160614           endif;
069900160614
070000160614           // -?Aggiornamento sfl?
070100160614           select;
070200160614             when  $ErrMessage  or  S1Copz = '1';
070300160614               $SflNxtChg = *on;
070400160614               C1CsrRrn   = C1RcdNbr;
070500160614             when  $ErrGenerico;
070600160614               C1CsrRrn   = C1RcdNbr;
070700160614               if  S1Copz <> '1';
070800160614                 clear  S1Copz;          //?(se selezionato più di uno?)?
070900160614               endif;
071000160614             other;
071100160614               if  S1Copz <> '1';
071200160614                 clear  S1Copz;          //?(se selezionato più di uno?)?
071300160614               endif;
071400160614           endsl;
071500160614
071600160614           UPDATE  tbDRMs01;
071700160614
071800160614           if  $ErrGenerico  or  $ErrMessage;
071900160614             leave;
072000160614           endif;
072100160614
072200160614           readc  tbDRMs01;
072300160614
072400160614         ENDDO;
072500160614
072600160614         // -?Riposizionam. cursore sul record contenente la prima opz.?
072700160614         //  ?(se non sono stati rilevati errori)?
072800160614         if  NOT $ErrMessage  and  NOT $ErrGenerico   and
072900160614             SavS01csr > *zero;
073000160614           C1CsrRrn = SavS01csr;
073100160614         endif;
073200160614
073300160614       ENDSR;
073400160614
073500160614       //--------------------------------------------------------------
073600160614       //?Gestione videata D02.                                        ?
073700160614       //--------------------------------------------------------------
073800160614       BEGSR  sr_GesD02;
073900160614
074000160614         // -?Inizializzazione videata?
074100160614         if  $InzD02 = *on;
074200160614           exsr  sr_InzD02;
074300160614           $InzD02 = *off;
074400160614         endif;
074500160614
074600160614         // -?Emissione Testata e Piede con tasti funzionali abilitati?
074700160614         write  tbDRMt01;
074800160614         write  tbDRMp02;
074900160614
075000160614         // -?Emissione videata?
075100160614         if  Not $Protect;
075200160614           exfmt  tbDRMd02;
075300160614         else;
075400160614           write  tbDRMd02;
075500160614           exfmt  Protect;
075600160614         endif;
075700160614
075800160614         clear  V1Dmsg;
075900160614         reset  $ErrMessage;
076000160614         reset  $ErrGenerico;
076100160614
076200160614         SELECT;
076300160614
076400160614           // -?F3=Fine?
076500160614           WHEN  dsp_aid = c_F03;
076600160614             unlock TNTBE01L;
076700160711             if  $Called;
076800160711               tntbDRMds.oDRMfxx = '03';
076900160711               $Fine = *on;
077000160711             else;
077100160711               $Video = 'S1';
077200160711               //$Fine  = *on;
077300160711               $ErrGenerico = *on;
077400160711             endif;
077500160614
077600160614           // -?F12=Ritorno?
077700160614           WHEN  dsp_aid = c_F12;
077800160614             unlock TNTBE01L;
077900160711             if  $Called  and  tntbDRMds.iDRMopz <> '1';
078000160711               tntbDRMds.oDRMfxx = '12';
078100160711               $Fine = *on;
078200160711             else;
078300160711               $Video = 'S1';
078400160711             endif;
078500160614
078600160614           // -?Enter / F5=Ripristino / F6=Conferma / F16=Annullamento?
078700160614           OTHER;
078800160614             $Ripristino   = (dsp_aid = c_F05);
078900160614             $Annullamento = (dsp_aid = c_F16);
079000160614             // -?Controlli eseguiti NON se F16=Annullamento?
079100160614             if  Not $Annullamento;
079200160614               exsr  sr_CtrD02;
079300160902             //*·// -?Controlli eseguiti SOLO se F16=Annullamento?
079400160902             //*·else;
079500160902             //*·  exsr  sr_CtrANN;
079600160614             endif;
079700160614             if  $ErrGenerico;
079800160614               leavesr;
079900160614             endif;
080000160614             // -?Aggiornamento?
080100160902             If  dsp_aid = c_F05   or
080200160614                 dsp_aid = c_F06   or
080300160614                 dsp_aid = c_F16;
080400160902               //*·$Video = 'W1';
080500160902               //*·reset  $InzW01;
080600160902               //*·exsr  sr_GesW01;
080700160902               exsr  sr_updTNTBE;
080800160902               // -?Rilevato errore in fase di aggiornamento?
080900160902               if  $ErrGenerico = *on;
081000160902                 leavesr;
081100160902               endif;
081200160902               // -?SubFile iniziale altrimenti?
081300160902               $Video  = 'S1';
081400160902               $InzS01 = *on;
081500160902             EndIf;
081600160614
081700160614         ENDSL;
081800160614
081900160614       ENDSR;
082000160614
082100160614       //--------------------------------------------------------------
082200160614       //?Inizializzazione videata D02.                                ?
082300160614       //--------------------------------------------------------------
082400160614       BEGSR  sr_InzD02;
082500160614
082600160614         // -?Spegnimento degli indicatori di errore?
082700160614         %subst(IndDspF : 50) = *off;
082800160614
082900160614         // -?Pulizia videata?
083000160614         clear  tbDRMd02;
083100160614
083200160614         // -?Impostazione testata?
083300160614         clear  V1Topz;
083400160614         Select;
083500160614           When  $Protect;
083600160614             V1Topz = 'VISUALIZZAZIONE';
083700160614           When  $Copia;
083800160614             V1Topz = '     COPIA     ';
083900160614           When  $Immissione;
084000160614             V1Topz = '  INSERIMENTO  ';
084100160614           When  TBEatb <> *blank;
084200160614             V1Topz = '  RIPRISTINO   ';
084300160614             $Ripristino = *on;
084400160614           Other;
084500160614             V1Topz = '   MODIFICA    ';
084600160614         EndSl;
084700160614
084800160614         // -?Reperimento dati della Filiale per il Report SIM Magazzino?
084900160614         //  ?(SE NON Immissione)?
085000160614         If  Not $Immissione;
085100160614
085200160614           // -?Reperimento dati?
085300160614           k_TBEke1 = %editc( S1Cfil : 'X' );
085400160614           if  $Protect  or  $Copia;
085500160614             chain(N)  %kds( keyTNTBE01 )  TNTBE000;
085600160614           else;
085700160614             chain  %kds( keyTNTBE01 )  TNTBE000;
085800160614           endif;
085900160711
086000160711           if  NOT %found(TNTBE01L)  and  $Called;
086100160711             tntbDRMds.oDRMerr = *on;
086200160711             //*oDRMmsg = ...............;
086300160711             exsr  sr_RoutEnd;
086400160711           endif;
086500160614
086600160614           // -?Caricamento dati del Codice?
086700160614           if  %found(TNTBE01L);
086800160614             exsr  sr_RieD02;
086900160614           endif;
087000160614
087100160614         EndiF;
087200160614
087300160614         // -?Impostazione indicatori per abilitazione tasti funzionali?
087400160614         exsr  sr_AbilitFxxD02;
087500160614
087600160614       ENDSR;
087700160614
087800160614       //--------------------------------------------------------------
087900160614       //?Caricamento dati del singolo record della tab. "DRM"         ?
088000160614       //?nella videata D02.                                           ?
088100160614       //--------------------------------------------------------------
088200160614       BEGSR  sr_RieD02;
088300160614
088400160614         if  Not $Copia;
088500160614           V2Cfil = %int( TBEke1 );
088600160614         endif;
088700160706
088800160706         if  Not $Copia       and
088900160706             TBEatb <> *blank;
089000160706           V2Dtxt = 'F I L I A L E   A N N U L L A T A';
089100160706         endif;
089200160614
089300160614         clear  dDRM;
089400160614         if  %found(TNTBE01L);
089500160614           dDRM   = TBEuni;
089600160614         endif;
089700160614
089800160614         V2Capl   = dDRM.§DRMaPri;
089900160614         V2Crpvp  = dDRM.§DRMaRPVP;
090000160614         V2Cfissi = dDRM.§DRMaFiss;
090100160614
090200160614         // -?Decodifica Filiale?
090300160614         clear  V2Dfil;
090400160614         if  V2Cfil > *zero;
090500160614           chain  ( V2Cfil )  AZORG;
090600160614           if  %found(AZORG01L);
090700160614             V2Dfil = ORGdes;
090800160614           else;
090900160614             V2Dfil = *all'? ';
091000160614           endif;
091100160614         endif;
091200160614
091300160614         // -?Decodifica Tipo Applicazione Principale?
091400160614         clear  dMTP;
091500160614         reset  TIBS02ds;
091600160614         //*T02mod = 'C';     ?(già così)?
091700160614         T02cod = 'MTP';
091800160614         T02ke1 = V2Capl;
091900160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
092000160614         if  T02err = *blanks;
092100160614           dMTP = T02uni;
092200160614         endif;
092300160614         V2Dapl   = dMTP.§MTPdes;
092400160614
092500160614         // -?Decodifica Tipo Applicazione "Fissi RPVP"?
092600160614         clear  dMTP;
092700160614         reset  TIBS02ds;
092800160614         //*T02mod = 'C';     ?(già così)?
092900160614         T02cod = 'MTP';
093000160614         T02ke1 = V2Crpvp;
093100160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
093200160614         if  T02err = *blanks;
093300160614           dMTP = T02uni;
093400160614         endif;
093500160614         V2Drpvp  = dMTP.§MTPdes;
093600160614
093700160614         // -?Decodifica Tipo Applicazione "Fissi"?
093800160614         clear  dMTP;
093900160614         reset  TIBS02ds;
094000160614         //*T02mod = 'C';     ?(già così)?
094100160614         T02cod = 'MTP';
094200160614         T02ke1 = V2Cfissi;
094300160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
094400160614         if  T02err = *blanks;
094500160614           dMTP = T02uni;
094600160614         endif;
094700160614         V2Dfissi = dMTP.§MTPdes;
094800160729
094900160729         // -?Note:?
095000160729         V2DspAtr1 = c_DspAtrNorm;
095100160729         V2DspAtr2 = V2DspAtr1;
095200160729         V2DspAtr3 = V2DspAtr1;
095300160729         // -?Nota per la filiale 006 - Padova Interporto?
095400160729         if  V2Cfil = 006;
095500160729           //*          *...+....1....+....2....+....3....+....4
095600160729           //*          ....+....5....+....6....+....7....+...
095700160729           V2Dtxt1   = 'ATTENZIONE:';
095800160729           V2Dtxt2   = 'Per questa filiale vengono comunque elab+
095900160729                        orate letture di Picking su Rulliera  ';
096000160729           V2Dtxt3   = '("K") per i dati relativi al "Movimento"+
096100160729                        !';
096200160729           V2DspAtr1 = c_DspAtrRED;
096300160729           V2DspAtr2 = V2DspAtr1;
096400160729           V2DspAtr3 = V2DspAtr1;
096500160729         endif;
096600160729
096700160614       ENDSR;
096800160614
096900160614       //--------------------------------------------------------------
097000160614       //?Settaggio indicatori nella videata D02 per abilitazione      ?
097100160614       //?  tasti funzionali.                                          ?
097200160614       //--------------------------------------------------------------
097300160614       BEGSR  sr_AbilitFxxD02;
097400160614
097500160614         // -?F3 = Fine?
097600160711         $F3Attivo  = ( Not $Immissione   and  Not $Called );
097700160614
097800160614         // -?F5 = Ripristino?
097900160614         $F5Attivo  = ( Not $Immissione   and
098000160614                        %found(TNTBE01L)  and  TBEatb <> *blank
098100160614                                          and  Not $Protect );
098200160614
098300160614         // -?F6 = Conferma?
098400160614         $F6Attivo  = ( NOT $F5Attivo  and  NOT $Protect );
098500160614
098600160614         // -?F12 = Ritorno?
098700160614         $F12Attivo = *on;
098800160614
098900160614         // -?F16 = Annullamento?
099000160614         $F16Attivo = ( Not $Immissione   and
099100160614                        %found(TNTBE01L)  and  TBEatb = *blank
099200160614                                          and  Not $Protect );
099300160614
099400160614       ENDSR;
099500160614
099600160614       //--------------------------------------------------------------
099700160614       //?Controllo dati videata D02.                                  ?
099800160614       //--------------------------------------------------------------
099900160614       BEGSR  sr_CtrD02;
100000160614
100100160614         // -?Spegnimento degli indicatori di errore?
100200160614         %subst(IndDspF : 50) = *off;
100300160614
100400160614         // -?Controllo immissione codice Filiale?
100500160614         clear  V2Dfil;
100600160614         If  V2Cfil = *zero;
100700160614           V1Dmsg = sk_Msg(04);
100800160614           $PosCurFIL   = *on;
100900160614           $ErrMessage  = *on;
101000160614           $ErrGenerico = *on;
101100160614           leavesr;
101200160614         EndIf;
101300160614
101400160614         // -?Verifica esistenza in AZORG / Decodifica Filiale?
101500160614         chain  ( V2Cfil )  AZORG;
101600160614         if  NOT %found(TNTBE01L);
101700160614           V1Dmsg = sk_Msg(06);
101800160614           $PosCurFIL   = *on;
101900160614           $ErrMessage  = *on;
102000160614           $ErrGenerico = *on;
102100160614           leavesr;
102200160614         endif;
102300160614         V2Dfil   = ORGdes;
102400160614
102500160614         // -?Verifica esistenza in TNTBE?
102600160706         If  $Immissione  or  $Copia;
102700160614           k_TBEke1 = %editc( V2Cfil : 'X' );
102800160614           setll  %kds( KeyTNTBE01 )  TNTBE000;
102900160614           if  %equal(TNTBE01L);
103000160614             V1Dmsg = sk_Msg(05);
103100160614             $PosCurFIL   = *on;
103200160614             $ErrMessage  = *on;
103300160614             $ErrGenerico = *on;
103400160614             leavesr;
103500160614           endif;
103600160614         EndIf;
103700160614
103800160614         // -?Tipo Applicazione Principale obbligatorio?
103900160614         clear  V2Dapl;
104000160614         If  V2Capl   = *blank;
104100160614           V1Dmsg = sk_Msg(07);
104200160614           $PosCurAPL   = *on;
104300160614           $ErrMessage  = *on;
104400160614           $ErrGenerico = *on;
104500160614           leavesr;
104600160614         EndIf;
104700160614
104800160614         // -?Ricerca Tipo Applicazione Principale?
104900160614         if  %scan( '?' : V2Capl ) > *zero;
105000160614           clear  V2Capl;
105100160614           reset  TIBS02ds;
105200160614           T02mod = 'R';
105300160614           T02cod = 'MTP';
105400160614           TNTBE_RicercaControllo ( kpjba : tibs02ds );
105500160614           if  T02err = *blanks;
105600160614             V2Capl   = T02ke1;
105700160614             dMTP     = T02uni;
105800160614             V2Dapl   = dMTP.§MTPdes;
105900160614           endIf;
106000160614           $PosCurAPL   = *on;
106100160614           $ErrGenerico = *on;
106200160614           leavesr;
106300160614         EndIf;
106400160614
106500160614         // -?Tipo Applicazione Principale errato?
106600160614         clear  dMTP;
106700160614         reset  TIBS02ds;
106800160614         //*T02mod = 'C';     ?(già così)?
106900160614         T02cod = 'MTP';
107000160614         T02ke1 = V2Capl;
107100160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
107200160614         If  T02err = *blanks;
107300160614           dMTP = T02uni;
107400160614         Else;
107500160614           V1Dmsg = sk_Msg(08);
107600160614           $PosCurAPL   = *on;
107700160614           $ErrMessage  = *on;
107800160614           $ErrGenerico = *on;
107900160614           leavesr;
108000160614         EndIf;
108100160614         V2Dapl   = dMTP.§MTPdes;
108200160614
108300160614         // -?Tipo Applicazione "Fissi RPVP" obbligatorio?
108400160614         clear  V2Drpvp;
108500160614         If  V2Crpvp  = *blank;
108600160614           V1Dmsg = sk_Msg(07);
108700160614           $PosCurRPVP  = *on;
108800160614           $ErrMessage  = *on;
108900160614           $ErrGenerico = *on;
109000160614           leavesr;
109100160614         EndIf;
109200160614
109300160614         // -?Ricerca Tipo Applicazione "Fissi RPVP"?
109400160614         if  %scan( '?' : V2Crpvp ) > *zero;
109500160614           clear  V2Crpvp;
109600160614           reset  TIBS02ds;
109700160614           T02mod = 'R';
109800160614           T02cod = 'MTP';
109900160614           TNTBE_RicercaControllo ( kpjba : tibs02ds );
110000160614           if  T02err = *blanks;
110100160614             V2Crpvp  = T02ke1;
110200160614             dMTP     = T02uni;
110300160614             V2Drpvp  = dMTP.§MTPdes;
110400160614           endIf;
110500160614           $PosCurRPVP  = *on;
110600160614           $ErrGenerico = *on;
110700160614           leavesr;
110800160614         EndIf;
110900160614
111000160614         // -?Tipo Applicazione "Fissi RPVP" errato?
111100160614         clear  dMTP;
111200160614         reset  TIBS02ds;
111300160614         //*T02mod = 'C';     ?(già così)?
111400160614         T02cod = 'MTP';
111500160614         T02ke1 = V2Crpvp;
111600160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
111700160614         If  T02err = *blanks;
111800160614           dMTP = T02uni;
111900160614         Else;
112000160614           V1Dmsg = sk_Msg(08);
112100160614           $PosCurRPVP  = *on;
112200160614           $ErrMessage  = *on;
112300160614           $ErrGenerico = *on;
112400160614           leavesr;
112500160614         EndIf;
112600160614         V2Drpvp  = dMTP.§MTPdes;
112700160614
112800160614         // -?Tipo Applicazione "Fissi" obbligatorio?
112900160614         If  V2Crpvp  = *blank;
113000160614           V1Dmsg = sk_Msg(07);
113100160614           $PosCurRPVP  = *on;
113200160614           $ErrMessage  = *on;
113300160614           $ErrGenerico = *on;
113400160614           leavesr;
113500160614         EndIf;
113600160614
113700160614         // -?Ricerca Tipo Applicazione "Fissi"?
113800160614         if  %scan( '?' : V2Crpvp ) > *zero;
113900160614           clear  V2Cfissi;
114000160614           reset  TIBS02ds;
114100160614           T02mod = 'R';
114200160614           T02cod = 'MTP';
114300160614           TNTBE_RicercaControllo ( kpjba : tibs02ds );
114400160614           if  T02err = *blanks;
114500160614             V2Cfissi = T02ke1;
114600160614             dMTP     = T02uni;
114700160614             V2Dfissi = dMTP.§MTPdes;
114800160614           endIf;
114900160614           $PosCurFISSI = *on;
115000160614           $ErrGenerico = *on;
115100160614           leavesr;
115200160614         EndIf;
115300160614
115400160614         // -?Tipo Applicazione "Fissi" errato?
115500160614         clear  dMTP;
115600160614         reset  TIBS02ds;
115700160614         //*T02mod = 'C';     ?(già così)?
115800160614         T02cod = 'MTP';
115900160614         T02ke1 = V2Cfissi;
116000160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
116100160614         If  T02err = *blanks;
116200160614           dMTP = T02uni;
116300160614         Else;
116400160614           V1Dmsg = sk_Msg(08);
116500160614           $PosCurFISSI = *on;
116600160614           $ErrMessage  = *on;
116700160614           $ErrGenerico = *on;
116800160614           leavesr;
116900160614         EndIf;
117000160614         V2Dfissi = dMTP.§MTPdes;
117100160614
117200160614       ENDSR;
117300160614
117400160902       //*·//--------------------------------------------------------------
117500160902       //*·//?Gestione trasmissione aggiornamento                          ?
117600160902       //*·//--------------------------------------------------------------
117700160902       //*·BEGSR  sr_GesW01;
117800160902       //*·
117900160902       //*·  // -?Inizializzazione videata?
118000160902       //*·  if $InzW01 = *on;
118100160902       //*·    exsr  sr_InzW01;
118200160902       //*·    $InzW01 = *off;
118300160902       //*·  endif;
118400160902       //*·
118500160902       //*·  // -?Emissione window?
118600160902       //*·  if  TBXftt = 'S';
118700160902       //*·    exfmt  tbDRMw01;
118800160902       //*·    $ErrMessage  = *off;
118900160902       //*·    $ErrGenerico = *off;
119000160902       //*·    clear  V1Dmsg;
119100160902       //*·  else;
119200160902       //*·    dsp_aid = c_F06;
119300160902       //*·  endif;
119400160902       //*·
119500160902       //*·  select;
119600160902       //*·
119700160902       //*·    // -?F12=Ritorno (gestito solo se emesso W01)?
119800160902       //*·    when  dsp_aid = c_F12;
119900160902       //*·      $Video = 'D2';
120000160902       //*·
120100160902       //*·    // -?F6=Conferma?
120200160902       //*·    when  dsp_aid = c_F06;
120300160902       //*·      exsr  sr_UpdTNTBE;
120400160902       //*·      // -?Ritorno alla videata iniziale?
120500160902       //*·      if  NOT  $ErrGenerico;
120600160902       //*·        if  $Called;
120700160902       //*·          tntbDRMds.oDRMfxx = '06';
120800160902       //*·          $Fine = *on;
120900160902       //*·        else;
121000160902       //*·          $Video  = 'S1';
121100160902       //*·          $InzS01 = *on;
121200160902       //*·        endif;
121300160902       //*·      endif;
121400160902       //*·
121500160902       //*·  endsl;
121600160902       //*·
121700160902       //*·ENDSR;
121800160902       //*·
121900160902       //*·//--------------------------------------------------------------
122000160902       //*·//?Inizializzazione videata W01                                 ?
122100160902       //*·//--------------------------------------------------------------
122200160902       //*·BEGSR  sr_InzW01;
122300160902       //*·
122400160902       //*·  clear tbDRMw01;
122500160902       //*·
122600160902       //*·  if  $Immissione;
122700160902       //*·
122800160902       //*·    // -?Se immissione?
122900160902       //*·    W1ftt  = TBXftt;
123000160902       //*·
123100160902       //*·  else;
123200160902       //*·
123300160902       //*·    // -?Se NON immissione:?
123400160902       //*·    //  ?visualizza i dati relativi all'ultima trasmissione?
123500160902       //*·    W1ftt  = TBEftt;
123600160902       //*·    W1flt  = TBEflt;
123700160902       //*·    W1ftr  = TBEftr;
123800160902       //*·    if TBEdtr <> *zero;
123900160902       //*·      wDate_EUR = %date(TBEdtr : *iso);
124000160902       //*·      W1dtr     = %dec(wDate_EUR);
124100160902       //*·    endif;
124200160902       //*·
124300160902       //*·  endif;
124400160902       //*·
124500160902       //*·ENDSR;
124600160614
124700160614       //--------------------------------------------------------------
124800160614       //?Aggiornamento records TNTBE00F (tab. "DRM")                  ?
124900160902       //?nel *file di entrambi i sistemi informativi.                 ?
125000160614       //--------------------------------------------------------------
125100160614       BEGSR  sr_UpdTNTBE;
125200160902
125300160902         //? N.B.                                                      ?
125400160902         // L'annullamento  ed  il ripristino  vanno lasciati in      ?
125500160902         //   trasmissione (vedi flag TBEFTR).                        ?
125600160902         // L'aggiornamento  e  l'inserimento  NO: vanno registrati   ?
125700160902         //   subito nello stesso file di entrambi i S.I. - in due    ?
125800160902         //   cicli diversi - ma NON vanno messi in trasmissione.     ?
125900160614
126000160614         // -?Aggancio record di TNTBE00F?
126100160614         clear  keyTNTBE01;
126200160614         k_TBEcod  = c_Tab;
126300160614         k_TBEke1  = %editc( V2Cfil : 'X' );
126400160902
126500160902         // -?Ciclo di elaborazione per ogni sistema informativo?
126600160902         For  w_SisInf = 1  To  %elem(sk_Libr);
126700160902
126800160902           // -?Apertura dell'archivio da aggiornare?
126900160902           if  w_SisInf > 1;
127000160902             exsr  sr_OpenFileTab;
127100160902           endif;
127200160614
127300160902           chain  %kds( keyTNTBE01 )  TNTBE000;
127400160902           exsr  sr_UpdTNTBE_1;
127500160902
127600160902         EndFor;
127700160614
127800160614       ENDSR;
127900160902
128000160902       //--------------------------------------------------------------
128100160902       //?Aggiornamento SINGOLO records TNTBE00F (tab. "DRM")          ?
128200160902       //?nel *file di UN sistema informativo.                         ?
128300160902       //--------------------------------------------------------------
128400160902       BEGSR  sr_UpdTNTBE_1;
128500160902
128600160902         // -?Impostazione dati?
128700160902         clear  dDRM;
128800160902         dDRM.§DRMaPri  = V2Capl;
128900160902         dDRM.§DRMaRPVP = V2Crpvp;
129000160902         dDRM.§DRMaFiss = V2Cfissi;
129100160902
129200160902         TBEuni = dDRM;
129300160902
129400160902         // -?Aggiornamento tab. "DRM"?
129500160902         SELECT;
129600160902
129700160902           // -?F5=Ripristino?
129800160902           WHEN  $Ripristino;
129900160902             clear  TBEatb;
130000160902             //*·TBEftt = W1ftt;
130100160902             TBEftt = 'S';
130200160902             clear  TBEftr;
130300160902             select;
130400160902               when  %found(TNTBE01L)  and  TBEatb <> *blank;
130500160902                 //_______________
130600160902                 UPDATE  TNTBE000;
130700160902                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
130800160902               // -?Record appena cancellato fisicamente?
130900160902               when  NOT %found(TNTBE01L);
131000160902                 //_______________
131100160902                 WRITE   TNTBE000;
131200160902                 //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
131300160902             endsl;
131400160902
131500160902           // -?F16=Annullamento?
131600160902           WHEN  $Annullamento;
131700160902             TBEatb = 'A';
131800160902             //*·TBEftt = W1ftt;
131900160902             TBEftt = 'S';
132000160902             clear  TBEftr;
132100160902             //_______________
132200160902             UPDATE  TNTBE000;
132300160902             //¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
132400160902
132500160902           // -?Immissione o Copia o Modifica?
132600160902           OTHER;
132700160902             if  NOT %found(TNTBE01L);
132800160902               clear TNTBE000;
132900160902               TBEcod  = k_TBEcod;
133000160902               TBEke1  = k_TBEke1;
133100160902               TBEke2  = k_TBEke2;
133200160902               TBElin  = k_TBElin;
133300160902               TBEsif  = k_TBEsif;
133400160902               TBEuni  = dDRM;
133500160902             endif;
133600160902             TBEapl = TBXapl;
133700160902             clear  TBEatb;
133800160902             //*·TBEftt = W1ftt;
133900160902             TBEftt = 'S';
134000160902             clear  TBEflt;
134100160902             //*·clear  TBEftr;
134200160902             if  w_SisInf = 1;
134300160902               TBEftr = 'T';
134400160902             else;
134500160902               TBEftr = 'R';
134600160902             endif;
134700160902             //*·clear TBEdtr;
134800160902             TBEdtr = wDate;
134900160902             // -?Aggiornamento record?
135000160902             if  %found(TNTBE01L);
135100160902               //_____________
135200160902               UPDATE TNTBE000;
135300160902               //¯¯¯¯¯¯¯¯¯¯¯¯¯
135400160902             else;
135500160902               //_____________
135600160902               WRITE  TNTBE000;
135700160902               //¯¯¯¯¯¯¯¯¯¯¯¯¯
135800160902             endif;
135900160902
136000160902         ENDSL;
136100160902
136200160902       ENDSR;
136300160902
136400160902       //--------------------------------------------------------------
136500160902       //?Apertura del file tabelle nel sistema informativo impostato. ?
136600160902       //--------------------------------------------------------------
136700160902       BEGSR  sr_OpenFileTab;
136800160902
136900160902         // -?Chiusura (eventuale) file tabelle?
137000160902         if  %open(TNTBE01L);
137100160902           close  TNTBE01L;
137200160902         endif;
137300160902
137400160902         // -?Apertura file tabelle?
137500160902         ds_Libr  = sk_Libraries(w_iSystem);
137600160902         wLibFile = %trimr( sk_Libr(w_SisInf) ) + '/' + 'TNTBE01L';
137700160902         open  TNTBE01L;
137800160902
137900160902       ENDSR;
138000160614
138100160614       //--------------------------------------------------------------
138200160614       //?Operazioni finali.                                           ?
138300160614       //--------------------------------------------------------------
138400160614       BEGSR  sr_RoutEnd;
138500160614
138600160614         // -?Chiusura funzioni precedentemente aperte?
138700160614         clear  TIBS02ds;
138800160614         T02tla = 'C';
138900160614         TNTBE_RicercaControllo ( kpjba : tibs02ds );
139000160614
139100160614         // -?Restituzione parametri?
139200160614         //  ?(se selezionato una Filiale)?
139300160711         If  wMemoSel > *zero;
139400160711           chain  SavS01csr  tbDRMs01;
139500160711           tntbDRMds.oDRMfil  = S1Cfil;
139600160711         EndIf;
139700160614
139800160711         kpjbu = tntbDRMds;
139900160614
140000160614         //  ?Uscita?
140100160614         return;
140200160614
140300160614       ENDSR;
140400160614
140500160614      /end-free
140600160614
140700160614       //--------------------------------------------------------------
140800160614       //?Definizione schiere a tempo di compilazione.                 ?
140900160614       //--------------------------------------------------------------
141000160614
141100160902** --?sk_iSystem / sk_Libraries:?Sistemi AS/400 & Librerie con entrambi i file?
141200160902SETRAS  GAITRAGRU FILTRAGRU
141300160902AS888   GAITRAGRPSFILTRAGRPF
141400160902** --?sk_Msg:?Messaggi di Errore?--------------------------------------------*
141500160614Raggiunto il limite massimo dei record caricabili a video                      1
141600160614Opzione errata                                                                 2
141700160614Selezionate più di una Filiale                                                 3
141800160614Immettere la Filiale                                                           4
141900160614Filiale già esistente in tabella                                               5
142000160614Filiale errata                                                                 6
142100160614Tipo Applicazione obbligatoria                                                 7
142200160614Tipo Applicazione errata                                                       8
