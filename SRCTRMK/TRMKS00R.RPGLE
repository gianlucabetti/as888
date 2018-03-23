000100121025       //==============================================================
000200121024       //?TRMKS00R * Lancio Pulizia CRM.
000300121025       //==============================================================
000400121025
000500121025       //--------------------------------------------------------------
000600121025       //?Specifiche di controllo.                                     ?
000700121025       //--------------------------------------------------------------
000800121025
000900121025     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000121025
001100121025       //--------------------------------------------------------------
001200121025       //?Dichiarazione file.                                          ?
001300121025       //--------------------------------------------------------------
001400121025
001500121025       // -?File Tabella
001600121025     fTNTBE01L  uf   e           k disk
001700121025
001800121025       // -?File video
001900121024     fTRMKS00D  cf   e             workstn
002000121107     f                                     indds(IndDspF)
002100121107     f                                     infds(InfDspF)
002200121025
002300121025       //--------------------------------------------------------------
002400121025       //?Definizione costanti.                                        ?
002500121025       //--------------------------------------------------------------
002600121025
002700121024      // - Tasti funzionali a video
002800121024     d c_F01           c                   const(x'31')
002900121024     d c_F02           c                   const(x'32')
003000121024     d c_F03           c                   const(x'33')
003100121024     d c_F04           c                   const(x'34')
003200121024     d c_F05           c                   const(x'35')
003300121024     d c_F06           c                   const(x'36')
003400121024     d c_F07           c                   const(x'37')
003500121024     d c_F08           c                   const(x'38')
003600121024     d c_F09           c                   const(x'39')
003700121024     d c_F10           c                   const(x'3A')
003800121024     d c_F11           c                   const(x'3B')
003900121024     d c_F12           c                   const(x'3C')
004000121024     d c_F13           c                   const(x'B1')
004100121024     d c_F14           c                   const(x'B2')
004200121024     d c_F15           c                   const(x'B3')
004300121024     d c_F16           c                   const(x'B4')
004400121024     d c_F17           c                   const(x'B5')
004500121024     d c_F18           c                   const(x'B6')
004600121024     d c_F19           c                   const(x'B7')
004700121024     d c_F20           c                   const(x'B8')
004800121024     d c_F21           c                   const(x'B9')
004900121024     d c_F22           c                   const(x'BA')
005000121024     d c_F23           c                   const(x'BB')
005100121024     d c_F24           c                   const(x'BC')
005200121024     d c_Enter         c                   const(x'F1')
005300121024     d c_RollDown      c                   const(x'F4')
005400121024     d c_RollUp        c                   const(x'F5')
005500121024
005600121024     d Digits          c                   const('0123456789')
005700121025
005800121025       //--------------------------------------------------------------
005900121025       //?Definizione schiere.                                         ?
006000121025       //--------------------------------------------------------------
006100121025
006200121024      // -?Messaggi di errore
006300121024     d skMsg           s             78    dim(31) ctdata perrcd(1)
006400121025
006500121025       //--------------------------------------------------------------
006600121025       //?Definizione aree dati.                                       ?
006700121025       //--------------------------------------------------------------
006800121025
006900121025       // -?Dati utente?
007000121025     d §AzUte        e ds                  extname(AZUTE00F)
007100121025     d                                     dtaara
007200121025     d §DatiUte      e ds                  extname(dDatiUte)
007300121025     d                                     dtaara
007400121025
007500121025       //--------------------------------------------------------------
007600121025       //?Definizione strutture dati.                                  ?
007700121025       //--------------------------------------------------------------
007800121025
007900121025       // -?Status ds?
008000121025     d Status         sds
008100121025     d  SDSpgm           *proc
008200121025     d  SDSusr               254    263
008300121024
008400121024      // -?InfDS
008500121024     d InfDspF         ds
008600121024     d  dsp_aid              369    369a
008700121024
008800121024      // -?Indicatori su DspF
008900121024     d IndDspF         ds
009000121024      // -?Indicatori di Errore
009100121024     d  ErrMessage                    1n   overlay(IndDspF : 28)
009200121205     d  PosCurDataC                   1n   overlay(IndDspF : 50)
009300121205     d  PosCurDataP                   1n   overlay(IndDspF : 51)
009400121024      // -?Indicatori di errore generico
009500121024     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009600121024
009700121024      // -?DS indicatori Dspf
009800121024     d WindDspF        ds                  inz
009900121024     d   WindDspFa             1     49    inz(*zero)
010000121024     d   WindDspFb            50     99    inz(*zero)
010100121024
010200121024      // -?DS controllo data
010300121024     d wlbdat          ds                  inz
010400121024     d  g02dat                 1      8  0
010500121024     d  g02inv                 9     16  0
010600121024     d  g02err                17     17
010700121024     d  g02tgi                18     22  0
010800121025
010900121025       // -?Parametri ricevuti
011000121025     d KPJBA         e ds
011100100507
011200121025       // -?Parametri per Reperimento dati utente?
011300121025     d TIBS34ds      e ds
011400121025
011500121025      // -?Ricerca/Controllo tabelle
011600121025     d TIBS02ds      e ds                  inz
011700121205
011800121205      // -?Tabella date pulizia - CRM
011900121211     d dPULCRM       e ds                  inz
012000121025
012100121025       //--------------------------------------------------------------
012200121025       //?Definizione variabili globali.                               ?
012300121025       //--------------------------------------------------------------
012400121025
012500121025       // -?Flags booleani
012600121024     d wEnd            s               n   inz(*off)
012700121024     d wErrGrave       s               n   inz(*off)
012800121024     d wFine           s               n   inz(*off)
012900121024     d wInzW01         s               n   inz(*on)
013000121024
013100121024       // -?Campi Data
013200121025     d wDataCRM        s              8s 0 inz
013300121205     d wDataPOT        s              8s 0 inz
013400121205     d wDataPULC       s              8s 0 inz
013500121205     d wDataPULP       s              8s 0 inz
013600121024     d wOggi           s              8s 0 inz
013700121025
013800121025       //--------------------------------------------------------------
013900121025       //?Definizione procedure esterne.                     ?
014000121025       //--------------------------------------------------------------
014100121025
014200121025       // -?Sottomissione lavoro batch
014300121025     d bch10           pr                  extpgm('BCH10')
014400121025     d  kpjba                              likeds(KPJBA)
014500121025
014600121205       // -?CL Pulizia attività/trattative
014700121025     d TRMKS01C        pr                  extpgm('TRMKS01C')
014800121025     d  kpjba                              likeds(kpjba)
014900121205
015000121205       // -?CL Pulizia potenziali eliminabili
015100121205     d TRMKS06C        pr                  extpgm('TRMKS06C')
015200121205     d  kpjba                              likeds(kpjba)
015300121025
015400121024       //--------------------------------------------------------------
015500121024       //?Definizione prototipi.
015600121024       //--------------------------------------------------------------
015700121024
015800121024      /copy gaitrasrc/srcprotopr,tibs02r
015900121025      /copy gaitrasrc/srcprotopr,tibs34r
016000121024      /copy gaitrasrc/srcprotopr,xsrda8
016100121025
016200121025       //--------------------------------------------------------------
016300121025       //?Definizione key-list.                                        ?
016400121025       //--------------------------------------------------------------
016500121025
016600121025       //--------------------------------------------------------------
016700121025       //?M A I N - L I N E                                            ?
016800121025       //--------------------------------------------------------------
016900121025
017000121025     c     *Entry        plist
017100121025     c                   parm                    KPJBA
017200121025
017300121025      /free
017400121025
017500121024       //?Operazioni iniziali?
017600121025       exsr RoutInz;
017700121025
017800121024       //?Gestione vidata?
017900121024       DOW wFine = *off;
018000121025         exsr GesW01;
018100121025       ENDDO;
018200121025
018300121024       //?Operazioni finali?
018400121025       exsr RoutEnd;
018500121025
018600121025       //--------------------------------------------------------------
018700121025       //?Operazioni iniziali.                                         ?
018800121025       //--------------------------------------------------------------
018900121024       BEGSR  RoutInz;
019000121025
019100121024       //?Reperimento dati job?
019200121024         exsr  DatiJob;
019300121025
019400121025       //?Reperimento della data corrente (in formato aaaa/mm/gg)?
019500121024         wOggi = %dec(%date());
019600121024
019700121205       //?Reperimento delle date ultima pulizia CRM?
019800121211         clear dPULCRM;
019900121025         clear wDataCRM;
020000121205         clear wDataPOT;
020100121024         clear TIBS02ds;
020200121024         T02mod = 'C';
020300121025         T02cod = 'PUL';
020400121025         T02ke1 = 'CRM';
020500121024         T02sif = KNSIF;
020600121024         TNTBE_RicercaControllo (kpjba : tibs02ds);
020700121024       //?Errore msg e poi esco dal PGM?
020800121024         IF  T02err <> *blanks;
020900121024           wErrGrave   = *on;
021000121024           ErrMessage  = *on;
021100121024           ErrGenerico = *on;
021200121024           W01msg = skMsg(01);
021300121024           leavesr;
021400121024         ENDIF;
021500121205
021600121211         dPULCRM = T02uni;
021700121024
021800121205       //?Se ci sono le date le imposto?
021900121205         IF  §CRMdatac <> *blanks;
022000121205           wDataCRM = %dec(§CRMdatac:8:0);
022100121024         ENDIF;
022200121205         IF  §CRMdatap <> *blanks;
022300121205           wDataPOT = %dec(§CRMdatap:8:0);
022400121205         ENDIF;
022500121025
022600121025       ENDSR;
022700121025
022800121025       //--------------------------------------------------------------
022900121025       //?Reperimento Dati del job (Utente/Operativi).                 ?
023000121025       //--------------------------------------------------------------
023100121024       BEGSR  DatiJob;
023200121025
023300121025         in(E) §AzUte;
023400121025         IF  NOT %error;
023500121025           in(E) §DatiUte;
023600121025         ENDIF;
023700121025         IF  %error or RSut = *blanks;
023800121025           clear TIBS34DS;
023900121025           tibs34r ( TIBS34DS );
024000121025           in §AzUte;
024100121025           in §DatiUte;
024200121025         ENDIF;
024300121025
024400121025       ENDSR;
024500121024
024600121024       //--------------------------------------------------------------
024700121024       //?Gesdione videata.
024800121024       //--------------------------------------------------------------
024900121024       BEGSR  GesW01;
025000121024
025100121024       //?Inizializzazione videata
025200121024         IF  wInzW01;
025300121024           exsr InzW01;
025400121024           wInzW01 = *off;
025500121024         ENDIF;
025600121024
025700121024       //?Emissione videata
025800121024         exfmt MKS00W01;
025900121024         ErrMessage   = *off;
026000121024         ErrGenerico  = *off;
026100121024         clear W01msg;
026200121024
026300121024         SELECT;
026400121024
026500121024       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
026600121025           WHEN  wErrGrave;
026700121025             wFine = *on;
026800121024
026900121024       //?F3=Fine
027000121024           WHEN  dsp_aid = c_F03;
027100121025             exsr F03W01;
027200121024
027300121024       //?F6=Conferma
027400121024           WHEN  dsp_aid = c_F06;
027500121024             exsr CtrW01;
027600121024             IF  ErrGenerico;
027700121024               leavesr;
027800121024             ENDIF;
027900121025             exsr F06W01;
028000121024
028100121024       //?Enter
028200121024           OTHER;
028300121024             exsr CtrW01;
028400121024             IF  ErrGenerico;
028500121024               leavesr;
028600121024             ENDIF;
028700121024
028800121024         ENDSL;
028900121024       ENDSR;
029000121024
029100121024       //--------------------------------------------------------------
029200121024       //?Inizializzazione videata.
029300121024       //--------------------------------------------------------------
029400121024       BEGSR InzW01;
029500121024
029600121024         clear MKS00W01;
029700121205
029800121205       //?Imposto 'NO' per tutte e 2 le pulizie
029900121205         W01crm = 'NO';
030000121205         W01pot = 'NO';
030100121025
030200121025         clear wlbdat;
030300121205
030400121205       //?Se non ho la data pulizia attività/trattative
030500121205       //?metto 31/12/2010 Prima pulizia?
030600121025         IF  wDataCRM = 0;
030700121025           G02inv = 20101231;
030800121025         ELSE;
030900121025           G02inv = wDataCRM;
031000121025         ENDIF;
031100121107         G02err = '3';
031200121024         xsrda8(wlbdat);
031300121024         IF  G02err = '1';
031400121024           leavesr;
031500121025         ENDIF;
031600121205         W01datac = G02dat;
031700121205
031800121205       //?Se non ho la data pulizia potenziali eliminabili
031900121205       //?metto 31/12/2010 Prima pulizia?
032000121205         IF  wDataPOT = 0;
032100121205           G02inv = 20101231;
032200121205         ELSE;
032300121205           G02inv = wDataPOT;
032400121205         ENDIF;
032500121205         G02err = '3';
032600121205         xsrda8(wlbdat);
032700121205         IF  G02err = '1';
032800121205           leavesr;
032900121205         ENDIF;
033000121205         W01datap = G02dat;
033100121024
033200121024       ENDSR;
033300121024
033400121024       //--------------------------------------------------------------
033500121024       //?Gestione tasto funzionale F3.
033600121024       //?F3=Fine
033700121024       //--------------------------------------------------------------
033800121024       BEGSR F03W01;
033900121024
034000121024       //?Chiusura del programma
034100121024         wFine = *on;
034200121024
034300121024       ENDSR;
034400121024
034500121024       //--------------------------------------------------------------
034600121024       //?Gestione tasto funzionale F6 da videata D01
034700121024       //?F6=Conferma
034800121024       //--------------------------------------------------------------
034900121025       BEGSR F06W01;
035000121024
035100121205       //?Lancia la pulizia attività/trattative
035200121205         IF  W01crm = 'SI';
035300121205           kpjbu = %editc(wDataPULc:'X');
035400121205           kcoaz = 'MKS1';
035500121205           kbuff = *blanks;
035600121025
035700121205         //?Richiamo interattivo
035800121205           IF  knmus = *all'1';
035900121205             TRMKS01C(kpjba);
036000121205         //?Richiamo batch
036100121205           ELSE;
036200121205             BCH10(kpjba);
036300121205           ENDIF;
036400121205         ENDIF;
036500121205
036600121205       //?Lancia la pulizia potenziali eliminabili
036700121205         IF  W01pot = 'SI';
036800121205           kpjbu = %editc(wDataPULp:'X');
036900121205           kcoaz = 'MKS6';
037000121205           kbuff = *blanks;
037100121205
037200121205         //?Richiamo interattivo
037300121205           IF  knmus = *all'1';
037400121205             TRMKS06C(kpjba);
037500121205         //?Richiamo batch
037600121205           ELSE;
037700121205             BCH10(kpjba);
037800121205           ENDIF;
037900121205         ENDIF;
038000121024
038100121024       //?Aggiorno la data ultima pulzia sulla tabella CRM
038200121205         IF  W01crm = 'SI';
038300121205           §CRMdatac = %editc(wDataPULc:'X');
038400121205         ENDIF;
038500121205         IF  W01pot = 'SI';
038600121205           §CRMdatap = %editc(wDataPULp:'X');
038700121205         ENDIF;
038800121205         IF  W01crm ='SI' or W01pot = 'SI';
038900121205           TBEcod = 'PUL';
039000121205           TBEke1 = 'CRM';
039100121205           chain (TBEcod:TBEke1) TNTBE01L;
039200121205           IF  %found(TNTBE01L);
039300121211             TBEuni = dPULCRM;
039400121205             UPDATE  TNTBE000;
039500121205           ENDIF;
039600121205         ENDIF;
039700121025
039800121025       //?Chiusura del programma
039900121025         wFine = *on;
040000121024
040100121024       ENDSR;
040200121024
040300121024       //--------------------------------------------------------------
040400121024       //?Controllo videata.
040500121024       //--------------------------------------------------------------
040600121024       BEGSR CtrW01;
040700121024
040800121024         WindDspF = IndDspF;
040900121024         reset WindDspFb;
041000121024         IndDspF  = WindDspF;
041100121024
041200121205       //?Se pulizia attività/trattative
041300121205         IF  W01crm = 'SI';
041400121205
041500121205         //?Data pulizia attività/trattative
041600121205           IF  W01datac  = 0;
041700121205             ErrMessage  = *on;
041800121205             ErrGenerico = *on;
041900121205             PosCurDatac = *on;
042000121205             W01msg      = skMsg(02);
042100121205             leavesr;
042200121205           ENDIF;
042300121025
042400121205           clear wlbdat;
042500121205           G02dat = W01datac;
042600121205           xsrda8(wlbdat);
042700121205           IF  G02err = '1';
042800121205             ErrMessage  = *on;
042900121205             ErrGenerico = *on;
043000121205             PosCurDatac = *on;
043100121205             W01msg      = skMsg(02);
043200121205             leavesr;
043300121205           ENDIF;
043400121025
043500121205           IF  G02inv = wDataCRM;
043600121205             ErrMessage  = *on;
043700121205             ErrGenerico = *on;
043800121205             PosCurDatac = *on;
043900121205             W01msg      = skMsg(02);
044000121205             W01msg      = %trim(W01msg) + ' Data già elaborata.';
044100121205             leavesr;
044200121205           ENDIF;
044300121025
044400121205           wDataPULc = G02inv;
044500121205         ENDIF;
044600121205
044700121205       //?Se pulizia potenziali eliminabili
044800121205         IF  W01pot = 'SI';
044900121205
045000121205         //?Data pulizia potenziali eliminabili
045100121205           IF  W01datap  = 0;
045200121205             ErrMessage  = *on;
045300121205             ErrGenerico = *on;
045400121205             PosCurDatap = *on;
045500121205             W01msg      = skMsg(02);
045600121205             leavesr;
045700121205           ENDIF;
045800121205
045900121205           clear wlbdat;
046000121205           G02dat = W01datap;
046100121205           xsrda8(wlbdat);
046200121205           IF  G02err = '1';
046300121205             ErrMessage  = *on;
046400121205             ErrGenerico = *on;
046500121205             PosCurDatap = *on;
046600121205             W01msg      = skMsg(02);
046700121205             leavesr;
046800121205           ENDIF;
046900121205
047000121205           IF  G02inv = wDataPOT;
047100121205             ErrMessage  = *on;
047200121205             ErrGenerico = *on;
047300121205             PosCurDatap = *on;
047400121205             W01msg      = skMsg(02);
047500121205             W01msg      = %trim(W01msg) + ' Data già elaborata.';
047600121205             leavesr;
047700121205           ENDIF;
047800121205
047900121211           wDataPULp = G02inv;
048000121205         ENDIF;
048100121024
048200121024       ENDSR;
048300121025
048400121025       //--------------------------------------------------------------
048500121025       //?Operazioni finali.                                           ?
048600121025       //--------------------------------------------------------------
048700121024       BEGSR  RoutEnd;
048800121025
048900121025         *inLR = *on;
049000100507
049100121025       //?Uscita dal pgm
049200121025         return;
049300121025
049400121025       ENDSR;
049500121025
049600121025      /end-free
049700121024
049800121024       //--------------------------------------------------------------
049900121024       //?Schiere a tempo di compilazione.
050000121024       //--------------------------------------------------------------
050100121024
050200121024** - $MSG -------------------------------------------------------------------*
050300121024Non trovata tabella CRM.                                                      01
050400121024Data errata.                                                                  02
