000100121114       //==============================================================
000200121114       //?TRTBBIR - Ricerca tabella "BI" (Causale Blocco Cliente) ?
000300121114       //==============================================================
000400121114
000500121114       //--------------------------------------------------------------
000600121114       //?Specifiche di controllo.                                     ?
000700121114       //--------------------------------------------------------------
000800121114
000900121114     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001000121114
001100121114       //--------------------------------------------------------------
001200121114       //?Dichiarazione file.                                          ?
001300121114       //--------------------------------------------------------------
001400121114
001500121114       // -?Tabelle?
001600121114     fTABEL00F  if   e           k disk
001700121114
001800121114       // -?Video?
001900121114     fTRTBBID   cf   e             workstn
002000121114     f                                     indds(IndDspF)
002100121114     f                                     infds(InfDspF)
002200121114     f                                     sfile(TBBIS01:S01nrr)
002300121114
002400121114       //--------------------------------------------------------------
002500121114       //?Definizione costanti.                                        ?
002600121114       //--------------------------------------------------------------
002700121114
002800121114       // -?Codice tabella
002900121114     d c_Tab           c                   const('BI')
003000121114
003100121114       // -?Codice utente?
003200121114     d c_Kut           c                   const(1)
003300121114
003400121114       // -?Numero di record in ciascuna videata di subfile?
003500121114     d c_SflPag        c                   const(10)
003600121114
003700121114       // -?Numero massimo di record gestibili?
003800121114     d c_MaxRec        c                   const(0999)
003900121114
004000121114       // -?Tasti funzionali a video?
004100121114     d c_F01           c                   const(x'31')
004200121114     d c_F02           c                   const(x'32')
004300121114     d c_F03           c                   const(x'33')
004400121114     d c_F04           c                   const(x'34')
004500121114     d c_F05           c                   const(x'35')
004600121114     d c_F06           c                   const(x'36')
004700121114     d c_F07           c                   const(x'37')
004800121114     d c_F08           c                   const(x'38')
004900121114     d c_F09           c                   const(x'39')
005000121114     d c_F10           c                   const(x'3A')
005100121114     d c_F11           c                   const(x'3B')
005200121114     d c_F12           c                   const(x'3C')
005300121114     d c_F13           c                   const(x'B1')
005400121114     d c_F14           c                   const(x'B2')
005500121114     d c_F15           c                   const(x'B3')
005600121114     d c_F16           c                   const(x'B4')
005700121114     d c_F17           c                   const(x'B5')
005800121114     d c_F18           c                   const(x'B6')
005900121114     d c_F19           c                   const(x'B7')
006000121114     d c_F20           c                   const(x'B8')
006100121114     d c_F21           c                   const(x'B9')
006200121114     d c_F22           c                   const(x'BA')
006300121114     d c_F23           c                   const(x'BB')
006400121114     d c_F24           c                   const(x'BC')
006500121114     d c_Enter         c                   const(x'F1')
006600121114     d c_RollDown      c                   const(x'F4')
006700121114     d c_RollUp        c                   const(x'F5')
006800121114
006900121114       //--------------------------------------------------------------
007000121114       //?Definizione schiere.                                         ?
007100121114       //--------------------------------------------------------------
007200121114
007300121114       // -?Messaggi di errore?
007400121114     d wMsg            s             78    dim(10)  ctdata  perrcd(1)
007500121114
007600121114       // -?Tabella BI x ordinamento
007700121114     d skBI            s              3    dim(999) inz
007800121114     d skBIds          s             89    dim(999) inz
007900121114     d skBIord         s              6    dim(999) inz
008000121114
008100121114       //--------------------------------------------------------------
008200121114       //?Definizione aree dati.                                       ?
008300121114       //--------------------------------------------------------------
008400121114
008500121114       // -?Dati utente?
008600121114     d §AzUte        e ds                  extname(AZUTE00F)
008700121114     d                                     dtaara
008800121114     d §DatiUte      e ds                  extname(dDatiUte)
008900121114     d                                     dtaara
009000121114
009100121114       //--------------------------------------------------------------
009200121114       //?Definizione strutture dati.                                  ?
009300121114       //--------------------------------------------------------------
009400121114
009500121114       // -?Status ds?
009600121114     d Status         sds
009700121114     d   SDSpgm          *proc
009800121114
009900121114       // -?InfDS?
010000121114     d InfDspF         ds
010100121114     d   dsp_aid             369    369a
010200121114     d   sfl_rrn             376    377i 0
010300121114     d   min_nrr             378    379i 0
010400121114     d   num_rcds            380    381i 0
010500121114
010600121114       // -?Indicatori su DspF?
010700121114     d IndDspF         ds                  inz
010800121114         // -?Emissione messaggio di errore?
010900121114     d   ErrMessage                    n   overlay(IndDspF : 28)
011000121114         // -?Indicatori di gestione del subfile?
011100121114     d   SflDsp_N                      n   overlay(IndDspF : 30)
011200121114     d   SflDspCtl_N                   n   overlay(IndDspF : 31)
011300121114     d   SflNxtChg                     n   overlay(IndDspF : 32)
011400121114     d   SflEnd                        n   overlay(IndDspF : 33)
011500121114         // -?Posizionamento cursore & Segnalazione errore?
011600121114     d   PosCurOPZ                     n   overlay(IndDspF : 60)
011700121114         // -?Riemissione videata?
011800121114     d   ErrGenerico                   n   overlay(IndDspF : 99)
011900121114
012000121114       // -?Parametri ricevuti?
012100121114     d KPJBA         e ds
012200121114
012300121114       // -?Tab. "BI" = Causali di Blocco?
012400121114     d dsBI          e ds                  inz
012500121114
012600121114       //--------------------------------------------------------------
012700121114       //?Definizione variabili globali.                               ?
012800121114       //--------------------------------------------------------------
012900121114
013000121114       // -?Flags booleani?
013100121114     d wFine           s               n   inz
013200121114     d wEoF            s               n   inz
013300121114     d wInzS01         s               n   inz(*on)
013400121114
013500121114       // -?Variabili per la gestione del video?
013600121114     d wVideo          s              2a   inz('S1')
013700121114     d S01nrr          s                   like(C1RcdNbr) inz
013800121114     d S01nrrMax       s                   like(S01nrr)   inz
013900121114     d w_SflPag        s              3s 0 inz
014000121114     d wPag            s              4s 0 inz
014100121114     d wRig            s              3s 0 inz
014200121114
014300121114       // -?Campi di comodo?
014400121114     d wBI             s              3a   inz
014500121114     d wblanks         s             45a   inz
014600121114     d wcom45          s             45a   inz
014700121114     d wDate_DMY       s               d   datfmt(*dmy)  inz
014800121114     d wlen            s              2s 0
014900121114
015000121114       // -?Indici di schiera
015100121114     d xx              s              4s 0 inz
015200121114     d yy              s              4s 0 inz
015300121114
015400121114       //--------------------------------------------------------------
015500121114       //?Definizione prototipi procedure.                             ?
015600121114       //--------------------------------------------------------------
015700121114
015800121114       // -?Reperimento dati utente?
015900121114     d TIBS34ds      e ds                  inz
016000121114      /copy gaitrasrc/srcProtoPR,TIBS34R
016100121114
016200121114       //--------------------------------------------------------------
016300121114       //?Definizione key-list.                                        ?
016400121114       //--------------------------------------------------------------
016500121114
016600121114       // -?File TABEL00F?
016700121114     d k03tabel      e ds                  extname(TABEL00F : *key)
016800121114     d                                     prefix(k_)   inz
016900121114
017000121114       //--------------------------------------------------------------
017100121114       //?Riepilogo indicatori utilizzati.                             ?
017200121114       //--------------------------------------------------------------
017300121114       //--------------------------------------------------------------
017400121114
017500121114       //--------------------------------------------------------------
017600121114       //?M A I N - L I N E                                            ?
017700121114       //--------------------------------------------------------------
017800121114
017900121114     c     *Entry        plist
018000121114     c                   parm                    KPJBA
018100121114
018200121114      /free
018300121114
018400121114       //?Operazioni iniziali?
018500121114       exsr  RoutInz;
018600121114
018700121114       //?Ciclo di gestione del file video?
018800121114       DOW  Not wFine;
018900121114         SELECT;
019000121114           //?Gestione videata S1 (subfile Cod.Tassazione)?
019100121114           WHEN  wVideo = 'S1';
019200121114             exsr  GesS01;
019300121114           OTHER;
019400121114             wFine = *on;
019500121114         ENDSL;
019600121114       ENDDO;
019700121114
019800121114       //?Operazioni finali?
019900121114       exsr  RoutEnd;
020000121114
020100121114       //--------------------------------------------------------------
020200121114       //?Operazioni iniziali.                                         ?
020300121114       //--------------------------------------------------------------
020400121114       BEGSR  RoutInz;
020500121114
020600121114       //?Impostazione chiusura?
020700121114         *inLR = *on;
020800121114
020900121114       //?Reperimento dati job?
021000121114         exsr  DatiJob;
021100121114
021200121114       //?Impostazione iniziale / pulizia dei campi chiave?
021300121114         clear  k03tabel;
021400121114         k_TBLkut = c_Kut;
021500121114         clear k_TBLcod;
021600121114
021700121114       //?Impostazione intestazione tabella
021800121114         k_TBLkey = *all'0';
021900121114         %subst(k_TBLkey:7:2) = c_TAB;
022000121114         chain  %kds(K03tabel) TABEL00F;
022100121114         IF  %found(TABEL00F);
022200121114           wcom45 = TBLuni;
022300121114           wlen = (%len(wcom45) - %len(%trimr(wcom45))) / 2;
022400121114           vtipr = %subst(wblanks:1:wlen) + wcom45;
022500121114           %subst(vtipr:43:3) = ' ' + c_TAB;
022600121114         ENDIF;
022700121114
022800121114       //?Carica la tabella in SK
022900121114         k_TBLcod = c_TAB;
023000121114         clear xx;
023100121114         setll  %kds(k03tabel : 2)  TABEL00F;
023200121114         reade  %kds(k03tabel : 2)  TABEL00F;
023300121114         DOW  Not %eof(TABEL00F);
023400121114           IF  TBLflg = *blanks;
023500121114             dsBI = TBLuni;
023600121114             IF  §BIuso = *blanks;
023700121114               xx += 1;
023800121114               skBI(xx)    = %subst(TBLkey:1:3);
023900121114               skBIds(xx)  = dsBI;
024000121114               skBIord(xx) = §BIord + %subst(TBLkey:1:3);
024100121114             ENDIF;
024200121114           ENDIF;
024300121114           reade  %kds(k03tabel : 2)  TABEL00F;
024400121114         ENDDO;
024500121114
024600121114       //?Ordino la sk delle INFO
024700121114         sorta skBIord;
024800121114
024900121114       ENDSR;
025000121114
025100121114       //--------------------------------------------------------------
025200121114       //?Reperimento Dati del job (Utente/Operativi).                 ?
025300121114       //--------------------------------------------------------------
025400121114       BEGSR  DatiJob;
025500121114
025600121114         in(e) §AzUte;
025700121114         IF  NOT %error;
025800121114           in(e) §DatiUte;
025900121114         ENDIF;
026000121114         IF  %error or RSut = *blanks;
026100121114           tibs34r ( tibs34ds );
026200121114           in §AzUte;
026300121114           in §DatiUte;
026400121114         ENDIF;
026500121114
026600121114       ENDSR;
026700121114
026800121114       //--------------------------------------------------------------
026900121114       //?Gestione subfile S01.                                        ?
027000121114       //--------------------------------------------------------------
027100121114       BEGSR  GesS01;
027200121114
027300121114       //?Inizializzazione videata?
027400121114         IF  wInzS01;
027500121114           exsr  InzS01;
027600121114           wInzS01 = *off;
027700121114         ENDIF;
027800121114
027900121114       //?Emissione Testata & Piede?
028000121114         write  TBBIT01;
028100121114         write  TBBIP01;
028200121114
028300121114       //?Posizionamento cursore?
028400121114         IF  C1CsrRrn > *zeros;
028500121114           C1RcdNbr = C1CsrRrn;
028600121114         ELSE;
028700121114           C1RcdNbr = 1;
028800121114         ENDIF;
028900121114
029000121114       //?Emissione videata?
029100121114         exfmt  TBBIC01;
029200121114
029300121114         reset  ErrGenerico;
029400121114         reset  ErrMessage;
029500121114         clear  V1Dmsg;
029600121114
029700121114         SELECT;
029800121114
029900121114         //?F12=Ritorno?
030000121114           WHEN  dsp_aid = c_F12;
030100121114             wFine = *on;
030200121114
030300121114           //?Invio?
030400121114           OTHER;
030500121114             //?Gestione opzioni?
030600121114             IF  S01nrr > *zero;
030700121114               exsr  OpzS01;
030800121114               IF  ErrGenerico;
030900121114                 leavesr;
031000121114               ENDIF;
031100121114             ENDIF;
031200121114
031300121114         ENDSL;
031400121114
031500121114       ENDSR;
031600121114
031700121114       //--------------------------------------------------------------
031800121114       //?Inizializzazione subfile S01.                                ?
031900121114       //--------------------------------------------------------------
032000121114       BEGSR  InzS01;
032100121114
032200121114       //?Spegnimento degli indicatori di errore?
032300121114         %subst(IndDspF : 60) = *off;
032400121114
032500121114       //?Pulizia subfile?
032600121114         SflDsp_N    = *on;
032700121114         SflDspCtl_N = *on;
032800121114         write  TBBIC01;
032900121114         SflDspCtl_N = *off;
033000121114         SflEnd      = *off;
033100121114
033200121114         clear  C1RcdNbr;
033300121114         clear  C1CsrRrn;
033400121114         clear  S01nrr;
033500121114         clear  V1Dmsg;
033600121114         ErrGenerico = *off;
033700121114         ErrMessage  = *off;
033800121114         wEoF = *off;
033900121114
034000121114       //?Caricamento
034100121114         exsr CarS01;
034200121114
034300121114       //?Impaginazione subfile?
034400121114       //?-> forza l'impaginazione sul 1° rec. del subfile?
034500121114         IF  S01nrr > *zero;
034600121114           C1RcdNbr  = 1;
034700121114           C1CsrRrn  = 1;
034800121114         ELSE;
034900121114           clear  C1RcdNbr;
035000121114         ENDIF;
035100121114
035200121114       ENDSR;
035300121114
035400121114       //--------------------------------------------------------------
035500121114       //?Caricamento completo del subfile S01                         ?
035600121114       //--------------------------------------------------------------
035700121114       BEGSR  CarS01;
035800121114
035900121114         clear  S01nrr;
036000121114         SflNxtChg = *off;
036100121114
036200121114       //?Ciclo di caricamento dati da tab. "BI"?
036300121114         xx = 1;
036400121114         FOR xx by 1 to %elem(skBIord);
036500121114
036600121114           IF  skBIord(xx) = *blanks;
036700121114             iter;
036800121114           ENDIF;
036900121114
037000121114           wBI = %subst(skBIord(xx):4:3);
037100121114
037200121114         //?Recupero la ds
037300121114           yy = %lookup(wBI:skBI);
037400121114           IF yy > 0;
037500121114             dsBI = skBIds(yy);
037600121114           ENDIF;
037700121114
037800121114           S01cod = wBI;
037900121114           S01des = §BIdes;
038000121114
038100121114         //?Scrittura record nel subfile?
038200121114           S01nrr += 1;
038300121114           write  TBBIS01;
038400121114
038500121114         ENDFOR;
038600121114
038700121114       //?Visualizzazione del SFL (se ci sono dati)?
038800121114         SflDsp_N = (S01nrr <= *zeros);
038900121114
039000121114       //?Attivazione del SFLEND?
039100121114         SflEnd = *on;
039200121114
039300121114       //?Posizionamento cursore al 1° rcd della 1ª pagina?
039400121114         IF  S01nrr > *zero;
039500121114           C1RcdNbr = 1;
039600121114         ELSE;
039700121114           clear  C1RcdNbr;
039800121114         ENDIF;
039900121114
040000121114         C1CsrRrn = C1RcdNbr;
040100121114
040200121114       ENDSR;
040300121114
040400121114       //--------------------------------------------------------------
040500121114       //?Gestione opzioni del subfile S01                             ?
040600121114       //--------------------------------------------------------------
040700121114       BEGSR  OpzS01;
040800121114
040900121114       //?Ciclo di lettura subfile?
041000121114         readc  TBBIS01;
041100121114
041200121114         SflNxtChg = *off;
041300121114         %subst(IndDspF : 60) = *off;
041400121114         C1RcdNbr = S01nrr;
041500121114
041600121114         SELECT;
041700121114
041800121114           //?Nessuna opzione?
041900121114           WHEN  S01opz = *blanks;
042000121114
042100121114           //?Opzione NON abilitata?
042200121114           WHEN  S01opz <> '1';
042300121114             ErrGenerico = *on;
042400121114             ErrMessage  = *on;
042500121114             PosCurOPZ   = *on;
042600121114             V1Dmsg = wMsg(01);
042700121114
042800121114           //?Opz. 1 = Selezione.?
042900121114           OTHER;
043000121114             wFine = *on;
043100121114             kpjbu = S01cod;
043200121114
043300121114         ENDSL;
043400121114
043500121114       ENDSR;
043600121114
043700121114       //--------------------------------------------------------------
043800121114       //?Operazioni finali.                                           ?
043900121114       //--------------------------------------------------------------
044000121114       BEGSR  RoutEnd;
044100121114
044200121114       //?Uscita dal *pgm?
044300121114         return;
044400121114
044500121114       ENDSR;
044600121114
044700121114      /end-free
044800121114
044900121114       //--------------------------------------------------------------
045000121114       //?Definizione schiere a tempo di compilazione                  ?
045100121114       //--------------------------------------------------------------
045200121114
045300121114** -?$Msg:?Messaggi di Errore?-----------------------------------------------*
045400121114Opzione errata                                                                 1
