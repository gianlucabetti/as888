000100100507       //==============================================================
000200110506       //?FIDG50R * Crea WrkF per controllo giri da PDA
000300100507       //==============================================================
000400100507
000500100507       //--------------------------------------------------------------
000600100507       //?Specifiche di controllo.                                     ?
000700100507       //--------------------------------------------------------------
000800100507
000900100510     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000100507
001100100507       //--------------------------------------------------------------
001200100507       //?Dichiarazione file.                                          ?
001300100507       //--------------------------------------------------------------
001400101011
001500101011       // -?Organigramma?
001600101011     fAZORG01L  if   e           k disk
001700110506
001800110506       // -?Bolle arrivi
001900110509     fFNARB01L  if   e           k disk    usropn
002000110509     f                                     extfile(wFLibArb)
002100110509     fFIAR401L  if   e           k disk    usropn
002200110509     f                                     extfile(wFLibAr4)
002300110510     fFIAR404L  if   e           k disk    usropn
002400110510     f                                     extfile(wFLibAr44)
002500110510     f                                     rename (FIAR4000:FIAR404)
002600110510     fFIAR601L  if   e           k disk    usropn
002700110510     f                                     extfile(wFLibAr6)
002800110510     fFIAR901L  if   e           k disk    usropn
002900110510     f                                     extfile(wFLibAr9)
003000110510     fFIARN11L  if   e           k disk    usropn
003100110510     f                                     extfile(wFLibArn)
003200110506
003300110506       // -?ORM
003400110506     fFNORM01L  if   e           k disk
003500110509     fFNORG01L  if   e           k disk
003600100507
003700100507       // -?Tabelle?
003800101011     fTABEL00F  if   e           k disk
003900110506     fTNTBE01L  if   e           k disk
004000100507
004100110506       // -?Elenco esiti da PDA
004200110506     fWFPDA00F  o    e             disk    usropn
004300101011     f                                     extfile(wFLib)
004400101019
004500101019       // -?File video
004600110506     fFIDG50D   cf   e             workstn
004700110506     f                                     indds(IndDspF)
004800110506     f                                     infds(InfDspF)
004900100507
005000100507       //--------------------------------------------------------------
005100100507       //?Definizione costanti.                                        ?
005200100507       //--------------------------------------------------------------
005300110506
005400110506      // - Tasti funzionali a video
005500110506     d c_F01           c                   const(x'31')
005600110506     d c_F02           c                   const(x'32')
005700110506     d c_F03           c                   const(x'33')
005800110506     d c_F04           c                   const(x'34')
005900110506     d c_F05           c                   const(x'35')
006000110506     d c_F06           c                   const(x'36')
006100110506     d c_F07           c                   const(x'37')
006200110506     d c_F08           c                   const(x'38')
006300110506     d c_F09           c                   const(x'39')
006400110506     d c_F10           c                   const(x'3A')
006500110506     d c_F11           c                   const(x'3B')
006600110506     d c_F12           c                   const(x'3C')
006700110506     d c_F13           c                   const(x'B1')
006800110506     d c_F14           c                   const(x'B2')
006900110506     d c_F15           c                   const(x'B3')
007000110506     d c_F16           c                   const(x'B4')
007100110506     d c_F17           c                   const(x'B5')
007200110506     d c_F18           c                   const(x'B6')
007300110506     d c_F19           c                   const(x'B7')
007400110506     d c_F20           c                   const(x'B8')
007500110506     d c_F21           c                   const(x'B9')
007600110506     d c_F22           c                   const(x'BA')
007700110506     d c_F23           c                   const(x'BB')
007800110506     d c_F24           c                   const(x'BC')
007900110506     d c_Enter         c                   const(x'F1')
008000110506     d c_RollDown      c                   const(x'F4')
008100110506     d c_RollUp        c                   const(x'F5')
008200100507
008300100507       //--------------------------------------------------------------
008400100507       //?Definizione schiere.                                         ?
008500100507       //--------------------------------------------------------------
008600110506
008700110506      // - Messaggi di errore
008800110506     d $Msg            s             50    dim(30) ctdata perrcd(1)
008900100507
009000100507       //--------------------------------------------------------------
009100100507       //?Definizione aree dati.                                       ?
009200100507       //--------------------------------------------------------------
009300100507
009400100507       // -?Dati utente?
009500100507     d §AzUte        e ds                  extname(AZUTE00F)
009600100507     d                                     dtaara
009700100507     d §DatiUte      e ds                  extname(dDatiUte)
009800100507     d                                     dtaara
009900100507
010000100507       //--------------------------------------------------------------
010100100507       //?Definizione strutture dati.                                  ?
010200100507       //--------------------------------------------------------------
010300100507
010400100507       // -?Status ds?
010500110506     d Psds           sds
010600100507     d  SDSpgm           *proc
010700100507     d  SDSusr               254    263
010800110506
010900110506      // - InfDS
011000110506     d InfDspF         ds
011100110506     d  dsp_aid              369    369a
011200110506
011300110506      // - Indicatori su DspF
011400110506     d IndDspF         ds
011500110506        // - Indicatori di errore
011600110506     d  ErrMessage                    1n   overlay(IndDspF : 28)
011700110506     d  PosCurFgs                     1n   overlay(IndDspF : 50)
011800110506     d  PosCurData                    1n   overlay(IndDspF : 51)
011900110506        // - Indicatori di errore generico
012000110506     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012100110506
012200110506     d WindDspF        ds                  inz
012300110506     d   WindDspFa             1     49    inz(*zero)
012400110506     d   WindDspFb            50     99    inz(*zero)
012500110506
012600110506     d wlbdat          ds                  inz
012700110506     d  g02dat                 1      8  0
012800110506     d  g02inv                 9     16  0
012900110506     d  g02err                17     17
013000110506     d  g02tgi                18     22  0
013100110510
013200110510     d wkeyorm         ds                  inz
013300110510     d  ORMpoe
013400110510     d  ORMnsr
013500110510     d  ORMnor
013600110510     d  ORMnrv
013700101011
013800101011       // -?Parametri ricevuti
013900101011     d KPJBA         e ds
014000100507
014100100507       // -?Parametri per Reperimento dati utente?
014200100507     d TIBS34ds      e ds
014300100507
014400110506       // -?Tabella "2A" = Esiti consegne
014500110506     d ds2A          e ds
014600110510
014700110510       // -?Tabella "3A" = Tipo bolla
014800110510     d ds3A          e ds
014900100507
015000110509       // -?Tabella "CMR" = Esiti ritiri
015100110509     d dCMR          e ds
015200110509
015300110509       // -?Descrizione campo ORGDE8
015400110509     d OG148         e ds
015500101019
015600110506       // -?File LOG PDA
015700110506     d FIPLG00F      e ds                  extname(FIPLG00F)
015800110506
015900110506       // -?DS per dati da LOG PDA
016000110506     d FIPRCresds    e ds
016100110506     d FIPRDcetds    e ds
016200100507
016300100507       //--------------------------------------------------------------
016400100507       //?Definizione variabili globali.                               ?
016500100507       //--------------------------------------------------------------
016600101011
016700101011       // -?Flags booleani
016800110506     d $End            s               n   inz(*off)
016900110509     d $ErrGrave       s               n   inz(*off)
017000101019     d $Fine           s               n   inz(*off)
017100110506     d $InzW01         s               n   inz(*on)
017200101011
017300110506       // -?Nome libreria del work-file WFPDA00F?
017400101011     d wFLib           s             21    inz
017500110509     d wFLibArb        s             21    inz
017600110510     d wFLibArn        s             21    inz
017700110509     d wFLibAr4        s             21    inz
017800110510     d wFLibAr44       s             21    inz
017900110510     d wFLibAr6        s             21    inz
018000110510     d wFLibAr9        s             21    inz
018100101011
018200101011       // -?Campi per richiamo QCMDEXC
018300101011     d Qcmd            s            128    inz
018400101019
018500110506       // -?Campi di comodo per date
018600101019     d wDate           s              8s 0 inz
018700110506     d wDate_EUR       s               d   datfmt(*eur)
018800110506     d wDate_ISO       s               d   datfmt(*iso)
018900110509
019000110510       // -?Campi di comodo
019100110509     d sav_DtOrin      s                   like(§RESdtorin)
019200110509     d sav_IdDoc       s                   like(PLGiddoc)
019300100507
019400100507       //--------------------------------------------------------------
019500100507       //?Definizione prototipi procedure e relativi parametri.        ?
019600100507       //--------------------------------------------------------------
019700101011
019800101011       // -?Esecuzione comando di sistema
019900101011      /copy gaitrasrc/srcProtoPR,QCMDEXC
020000100507
020100100507       // -?Reperimento dati utente?
020200100507      /copy gaitrasrc/srcProtoPR,TIBS34R
020300110506
020400110506       // -?Controllo data         ?
020500110506      /copy gaitrasrc/srcprotopr,xsrda8
020600100507
020700100507       //--------------------------------------------------------------
020800100507       //?Definizione key-list.                                        ?
020900100507       //--------------------------------------------------------------
021000100507
021100100507       // -?File TABEL00F?
021200100507     d k03tabel00    e ds                  extname(TABEL00F : *key)
021300100507     d                                     inz  prefix(k_)
021400110509
021500110509       // -?File FNORM00F?
021600110509     d k04fnorm00    e ds                  extname(FNORM01L : *key)
021700110509     d                                     inz  prefix(k_)
021800110506
021900110506      //---------------------------------------------------------------
022000110506      //?Riepilogo indicatori.
022100110506      //---------------------------------------------------------------
022200110506      // 28    : Emissione messaggio di errore a video
022300110506      // 50-51 : Errore Video
022400110506      // 99    : Generico di Errore
022500110506      //---------------------------------------------------------------
022600100507
022700100507       //--------------------------------------------------------------
022800100507       //?M A I N - L I N E                                            ?
022900100507       //--------------------------------------------------------------
023000101011
023100101011     c     *Entry        plist
023200101011     c                   parm                    KPJBA
023300100507
023400100507      /free
023500100507
023600100507       // -?Operazioni iniziali?
023700110506       exsr RoutInz;
023800101019
023900110506       // -?Gestione Video?
024000101019       DOW $Fine = *off;
024100110506         exsr GesW01;
024200110506       ENDDO;
024300100507
024400100507       // -?Operazioni finali?
024500110506       exsr RoutEnd;
024600100507
024700100507       //--------------------------------------------------------------
024800100507       //?Operazioni iniziali.                                         ?
024900100507       //--------------------------------------------------------------
025000110506       BEGSR  RoutInz;
025100100507
025200100507         // -?Reperimento dati job?
025300110506         exsr  DatiJob;
025400100507
025500100507         // -?Reperimento della data corrente (in formato aaaa/mm/gg)?
025600101019         wDate = %dec(%date());
025700110506         wDate_ISO = %date(wDate:*iso);
025800110506         wDate_EUR = wDate_ISO;
025900100507
026000100507         // -?Impostazione campi fissi?
026100100507         k_TBLkut = 1;
026200110509
026300110510         // -?Apro i file delle bolle?
026400110509         IF  %subst(knsif : 7 : 1) = 'P';
026500110509           wFLibArb = 'FILTRAPRD/FNARB01L';
026600110510           wFLibAr4 = 'FILTRAPRD/FIAR401L';
026700110510           wFLibAr6 = 'FILTRAPRD/FIAR601L';
026800110510           wFLibAr9 = 'FILTRAPRD/FIAR901L';
026900110510           wFLibArn = 'FILTRAPRD/FIARN11L';
027000110510           wFLibAr44 = 'FILTRAPRD/FIAR404L';
027100110509         ELSE;
027200110509           wFLibArb = 'FILTRA201/FNARB01L';
027300110510           wFLibAr4 = 'FILTRA201/FIAR401L';
027400110510           wFLibAr6 = 'FILTRA201/FIAR601L';
027500110510           wFLibAr9 = 'FILTRA201/FIAR901L';
027600110510           wFLibArn = 'FILTRA201/FIARN11L';
027700110510           wFLibAr44 = 'FILTRA201/FIAR404L';
027800110509         ENDIF;
027900110509         open  FNARB01L;
028000110509         open  FIAR401L;
028100110510         open  FIAR404L;
028200110510         open  FIAR601L;
028300110510         open  FIAR901L;
028400110510         open  FIARN11L;
028500101011
028600101011         exec sql set option dynusrprf = *owner,
028700101011                             closqlcsr = *endmod;
028800100507
028900100507       ENDSR;
029000100507
029100100507       //--------------------------------------------------------------
029200100507       //?Reperimento Dati del job (Utente/Operativi).                 ?
029300100507       //--------------------------------------------------------------
029400110506       BEGSR  DatiJob;
029500100507
029600100507         in(E) §AzUte;
029700100507         if NOT %error;
029800100507           in(E) §DatiUte;
029900100507         endif;
030000100507         if %error or RSut = *blanks;
030100100507           clear TIBS34ds;
030200100507           tibs34r ( tibs34ds );
030300100507           in §AzUte;
030400100507           in §DatiUte;
030500100507         endif;
030600100507
030700100507       ENDSR;
030800110506
030900110506       //--------------------------------------------------------------
031000110506       //?Gestione videata.
031100110506       //--------------------------------------------------------------
031200110506       BEGSR GesW01;
031300110506
031400110506       //?Inizializzazione videata
031500110506         IF  $InzW01;
031600110506           exsr InzW01;
031700110506           $InzW01 = *off;
031800110506         ENDIF;
031900110506
032000110506       //?Emissione videata
032100110506         exfmt DG50W01;
032200110506         ErrMessage   = *off;
032300110506         ErrGenerico  = *off;
032400110506         clear W01msg;
032500110506
032600110506         SELECT;
032700110506
032800110506       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
032900110506           WHEN  $ErrGrave;
033000110506             $Fine = *on;
033100110506
033200110506       //?F3=Fine
033300110506           WHEN  dsp_aid = c_F03;
033400110506             exsr F03W01;
033500110506
033600110506       //?F6=Conferma
033700110506           WHEN  dsp_aid = c_F06;
033800110506             exsr CtrW01;
033900110506             IF  ErrGenerico;
034000110506               leavesr;
034100110506             ENDIF;
034200110506             exsr F06W01;
034300110506
034400110506       //?F8=Visualizza
034500110506           WHEN  dsp_aid = c_F08;
034600110506             exsr F08W01;
034700110506
034800110506       //?Enter
034900110506           OTHER;
035000110506             exsr CtrW01;
035100110506             IF  ErrGenerico;
035200110506               leavesr;
035300110506             ENDIF;
035400110506         ENDSL;
035500110506
035600110506       ENDSR;
035700110506
035800110506       //--------------------------------------------------------------
035900110506       //?Inizializzazione videata.
036000110506       //--------------------------------------------------------------
036100110506       BEGSR InzW01;
036200110506
036300110506         clear DG50W01;
036400110506
036500110506         W01data = %dec(wDate_EUR);
036600110506
036700110506       ENDSR;
036800110506
036900110506       //--------------------------------------------------------------
037000110506       //?Gestione tasto funzionale F3.
037100110506       //?F3=Fine
037200110506       //--------------------------------------------------------------
037300110506       BEGSR F03W01;
037400110506
037500110506       //?Chiusura del programma
037600110506         $Fine = *on;
037700110506
037800110506       ENDSR;
037900110506
038000110506       //--------------------------------------------------------------
038100110506       //?Gestione tasto funzionale F6.
038200110506       //?F6=Conferma
038300110506       //--------------------------------------------------------------
038400110506       BEGSR F06W01;
038500101025
038600101025       // -?Pulizia ed apertura WrkF?
038700101025         Qcmd = 'CLRPFM FILE(';
038800101025         IF  %subst(knsif : 7 : 1) = 'P';
038900101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
039000110506           wFLib = 'GAITRAAZP/WFPDA00F';
039100101025         ELSE;
039200101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
039300110506           wFLib = 'GAITRAAZM/WFPDA00F';
039400101025         ENDIF;
039500110506         Qcmd = %trimr(Qcmd) + '/WFPDA00F)';
039600101025         ExecuteCommand ( Qcmd : %size(Qcmd));
039700110506         open  WFPDA00F;
039800101019
039900110506       // -?Elaboro i dati?
040000110506         exsr Elabora;
040100101019
040200101019         $fine = *on;
040300101019
040400101019       ENDSR;
040500110506
040600110506       //--------------------------------------------------------------
040700110506       //?Gestione tasto funzionale F8.
040800110506       //?F8=Visualizza
040900110506       //--------------------------------------------------------------
041000110506       BEGSR F08W01;
041100101025
041200101025         // -?Visualizzo il file?
041300101025         Qcmd = 'DSPDBF FILE(';
041400101025         IF  %subst(knsif : 7 : 1) = 'P';
041500101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
041600101025         ELSE;
041700101025           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
041800101025         ENDIF;
041900110506         Qcmd = %trimr(Qcmd) + '/WFPDA00F) mbr(*first) Output(*)';
042000101025         callp(e) ExecuteCommand ( Qcmd : %size(Qcmd));
042100101025
042200101025       ENDSR;
042300110506
042400110506       //--------------------------------------------------------------
042500110506       //?Controllo videata.
042600110506       //--------------------------------------------------------------
042700110509       BEGSR CtrW01;
042800110506
042900110506         clear W01dfgs;
043000110506
043100110506         WindDspF = IndDspF;
043200110506         reset WindDspFb;
043300110506         IndDspF  = WindDspF;
043400110506
043500110506       //?Filiale
043600110506         IF  W01fgs = 0;
043700110506           ErrMessage  = *on;
043800110506           ErrGenerico = *on;
043900110506           PosCurFgs   = *on;
044000110506           W01msg      = $Msg(01);
044100110506           leavesr;
044200110506         ENDIF;
044300110506
044400110506         chain (W01fgs) AZORG01L;
044500110506         IF  %found(AZORG01L);
044600110506           W01dfgs = ORGdes;
044700110509           og148   = ORGde8;
044800110506         ENDIF;
044900110506         IF  not %found(AZORG01L) or ORGfva <> *blanks or
045000110506                                    (ORGfag <> 'A' and ORGfag <> 'F');
045100110506           ErrMessage  = *on;
045200110506           ErrGenerico = *on;
045300110506           PosCurFgs   = *on;
045400110506           W01msg      = $Msg(01);
045500110506           leavesr;
045600110506         ENDIF;
045700110509         //?Filiale senza procedura PDA attiva
045800110509         IF  §OGpdaorm = *blanks and §OGpdacon = *blanks;
045900110509           ErrMessage  = *on;
046000110509           ErrGenerico = *on;
046100110509           PosCurFgs   = *on;
046200110509           W01msg      = $Msg(03);
046300110509           leavesr;
046400110509         ENDIF;
046500110506
046600110506       //?Data
046700110506         IF  W01data = 0;
046800110506           ErrMessage  = *on;
046900110506           ErrGenerico = *on;
047000110506           PosCurData  = *on;
047100110506           W01msg      = $Msg(02);
047200110506           leavesr;
047300110506         ENDIF;
047400110506
047500110506         clear wlbdat;
047600110506         G02dat = W01data;
047700110506         xsrda8(wlbdat);
047800110506         IF  G02err = '1';
047900110506           ErrMessage  = *on;
048000110506           ErrGenerico = *on;
048100110506           PosCurData  = *on;
048200110506           W01msg      = $Msg(02);
048300110506           leavesr;
048400110506         ENDIF;
048500110506
048600110506         W01data = G02dat;
048700110506         wDate   = G02inv;
048800110506
048900110506       ENDSR;
049000101019
049100101019       //--------------------------------------------------------------
049200110506       //?Elaborazione.
049300101019       //--------------------------------------------------------------
049400110506       BEGSR  Elabora;
049500101019
049600110506         // -?Cerco i dati dal log PDA?
049700101019         exec sql
049800110506          declare LOGPDA cursor for
049900110506          select * from FIPLG00F
050000110506          where PLGfgs = :W01fgs
050100110506            and substr(PLGdatora, 1, 8) = :wDate
050200110509            and PLGtiporcd in('RES', 'CET')
050300110509          order by PLGfgs, PLGndc, PLGiddoc;
050400101019
050500101019         exec sql
050600110506          open LOGPDA;
050700101019          IF sqlcode < 0;
050800110506            $End = *on;
050900101019          ENDIF;
051000101019
051100110506         DOW not $End;
051200101019          exec sql
051300110506           fetch next from LOGPDA into :FIPLG00F;
051400110506
051500101019           IF sqlcod = 100 or sqlcod < 0;
051600110506             $End = *on;
051700101019             leave;
051800101019           ENDIF;
051900110509
052000110509         // -?Scrivo rcd WrkF?
052100110509           IF  PLGiddoc <> sav_IdDoc and sav_IdDoc <> *blanks;
052200110509             write  WFPDA000;
052300110509           ENDIF;
052400110509
052500110509         // -?Memorizzo l'evento per il WrkF in caso di IdDoc uguale?
052600110509           IF  PLGiddoc = sav_IdDoc and sav_IdDoc <> *blanks;
052700110509             exsr MemEsito;
052800110509           ENDIF;
052900110509
053000110509         // -?Memorizzo i dati per il WrkF in caso di IdDoc diverso?
053100110509           IF  PLGiddoc <> sav_IdDoc;
053200110509             exsr MemDati;
053300110509             sav_IdDoc = PLGiddoc;
053400110509           ENDIF;
053500101019
053600101019         ENDDO;
053700101019
053800110506         exec sql close LOGPDA;
053900101019
054000101019       ENDSR;
054100100507
054200101019       //--------------------------------------------------------------
054300110509       //?Memorizzo i dati.
054400101019       //--------------------------------------------------------------
054500110509       BEGSR  MemDati;
054600101019
054700110506         clear  WFPDA000;
054800110506
054900110506         WFiddoc = PLGiddoc;
055000110506         WFfgs   = PLGfgs;
055100110506         WFndc   = PLGndc;
055200110506         WFpdr   = PLGcodaut;
055300110506
055400110506         SELECT;
055500110509       // -?BOLLA?
055600110506           WHEN  PLGtipappl = 'C';
055700110506             WFtipappl = '1';
055800110506             FIPRDcetds = PLGdati;
055900110509             WFdata  = §CETdata;
056000110509             WFora   = §CETora;
056100110509             WFesito = §CETcmc;
056200110509           // -?Decodifico l'esito?
056300110509             IF  WFesito <> *blanks;
056400110509               clear ds2A;
056500110509               k_TBLcod = '2A';
056600110509               k_TBLkey = WFesito;
056700110509               chain %kds(k03tabel00) TABEL00F;
056800110509               IF  %Found(TABEL00F);
056900110509                 ds2A = TBLuni;
057000110509               ENDIF;
057100110509               WFdeses = §2Ades;
057200110509             ENDIF;
057300110509           // -?Aggancio la bolla?
057400110509             chain (§CETaas:§CETlnp:§CETnrs:§CETnsp) FNARB01L;
057500110509             IF  %Found(FNARB01L);
057600110510             // -?Colli?
057700110509               WFncl = ARBncl;
057800110509             // -?Calcolo il volume?
057900110509               SELECT;
058000110509                 WHEN  ARBfvf = 'T';
058100110509                   WFvlm = ARBvlf;
058200110509                 WHEN  ARBncr = ARBncl;
058300110509                   WFvlm = ARBvlc;
058400110509                 WHEN  ARBvlc >= ARBvlf;
058500110509                   WFvlm = ARBvlc;
058600110509                 OTHER;
058700110509                   WFvlm = ARBvlf;
058800110509               ENDSL;
058900110509             // -?Calcolo il peso?
059000110509               SELECT;
059100110509                 WHEN  ARBncp = ARBncl;
059200110509                   WFpkg = ARBpkc;
059300110509                 WHEN  ARBpkc >= ARBpkf;
059400110509                   WFpkg = ARBpkc;
059500110509                 OTHER;
059600110509                   WFpkg = ARBpkf;
059700110509               ENDSL;
059800110510             // -?Tipo Servizio?
059900110510               WFtsp = ARBtsp;
060000110510             // -?Consegne Particolari?
060100110510               WFtc1 = ARBtc1;
060200110510               WFtc2 = ARBtc2;
060300110510             // -?Chiusure per Turno?
060400110510               WFgc1 = ARBgc1;
060500110510               WFgc2 = ARBgc2;
060600110509             // -?Note bolla?
060700110509               chain (§CETaas:§CETlnp:§CETnrs:§CETnsp:'8') FIAR401L;
060800110509               IF  %Found(FIAR401L);
060900110509                 WFnote1 = AR4not;
061000110509               ENDIF;
061100110509               chain (§CETaas:§CETlnp:§CETnrs:§CETnsp:'9') FIAR401L;
061200110509               IF  %Found(FIAR401L);
061300110509                 WFnote2 = AR4not;
061400110509               ENDIF;
061500110510             // -?Note LDV?
061600110510               setll (§CETaas:§CETlnp:§CETnrs:§CETnsp:'LDV':'S') FIARN11L;
061700110510               reade (§CETaas:§CETlnp:§CETnrs:§CETnsp:'LDV':'S') FIARN11L;
061800110510               DOW  not %eof(FIARN11L);
061900110510                 IF  ARNnob <> *blanks;
062000110510                   SELECT;
062100110510                     WHEN WFnldv1 = *blanks;
062200110510                       WFnldv1 = ARNnob;
062300110510                     WHEN WFnldv2 = *blanks;
062400110510                       WFnldv2 = ARNnob;
062500110510                     WHEN WFnldv3 = *blanks;
062600110510                       WFnldv3 = ARNnob;
062700110510                     WHEN WFnldv4 = *blanks;
062800110510                       WFnldv4 = ARNnob;
062900110510                   ENDSL;
063000110510                 ENDIF;
063100110510                 reade (§CETaas:§CETlnp:§CETnrs:§CETnsp:'LDV':'S') FIARN11L;
063200110510               ENDDO;
063300110510
063400110510             // -?Aggancio la tabella del tipo bolla?
063500110510               clear ds3A;
063600110510               k_TBLcod = '3A';
063700110510               k_TBLkey = ARBcbo;
063800110510               chain %kds(k03tabel00) TABEL00F;
063900110510               IF  %Found(TABEL00F);
064000110510                 ds3A = TBLuni;
064100110510               ENDIF;
064200110510
064300110510             // -?Importo fattura?
064400110510               chain (§CETaas:§CETlnp:§CETnrs:§CETnsp:'1') FIAR601L;
064500110510               IF  %Found(FIAR601L);
064600110510               // -?Non c'è la seconda bolla?
064700110510                 IF  §3Atb2 = *blanks;
064800110510                   WFift = AR6ift;
064900110510                 ELSE;
065000110510                   chain (§CETaas:§CETlnp:§CETnrs:§CETnsp:'2') FIAR601L;
065100110510                   IF  %Found(FIAR601L);
065200110510                     WFift = AR6ift;
065300110510                   ENDIF;
065400110510                 ENDIF;
065500110510               ENDIF;
065600110510
065700110510             // -?Importo e tipo contrassegno?
065800110510               chain (§CETaas:§CETlnp:§CETnrs:§CETnsp) FIAR901L;
065900110510               IF  %Found(FIAR901L);
066000110510                 WFtic = AR9tic;
066100110510                 WFcas = AR9cas;
066200110510               ENDIF;
066300110509
066400110509             // -?Dati destinatario?
066500110509               WFrag = ARBrsd;
066600110509               WFind = ARBind;
066700110509               WFcap = ARBcad;
066800110509               WFloc = ARBlod;
066900110509               WFprv = ARBprd;
067000110509             ENDIF;
067100110509
067200110509             sav_DtOrin = %editc(§CETdtorin:'X');
067300110506
067400110509       // -?RITIRO?
067500110506           WHEN  PLGtipappl = 'R';
067600110506             WFtipappl = '2';
067700110509           // -?ORM Manuale?
067800110509             IF  %subst(PLGiddoc:1:8) = *all'0';
067900110509               WFdocman = 'M';
068000110509             ENDIF;
068100110506             FIPRCresds = PLGdati;
068200110509             WFdata  = %dec(%subst(§RESdatora:1:8):8:0);
068300110509             WFora   = %dec(%subst(§RESdatora:9:6):6:0);
068400110509
068500110509           // -?Se ORM da AS?
068600110509             IF  WFdocman = *blanks;
068700110509               WFesito = §REScmr;
068800110509             // -?Decodifico l'esito?
068900110509               IF  WFesito <> *blanks;
069000110509                 clear dCMR;
069100110509                 chain ('CMR':WFesito) TNTBE01L;
069200110509                 IF  %Found(TNTBE01L);
069300110509                   dCMR = TBEuni;
069400110509                 ENDIF;
069500110509                 WFdeses = d§CMRdes;
069600110509               ENDIF;
069700110509             // -?Aggancio l'ORM?
069800110509               k_ORMpoe = %dec(§RESpoe:3:0);
069900110509               k_ORMnsr = %dec(§RESnsr:2:0);
070000110509               k_ORMnor = %dec(§RESnor:7:0);
070100110509               k_ORMnrv = %dec(§RESnrv:2:0);
070200110509               chain %kds(k04fnorm00) FNORM01L;
070300110509               IF  %Found(FNORM01L);
070400110509                 WFncl   = ORMncl;
070500110509                 WFbnc   = ORMbnc;
070600110509                 WFspi   = ORMspi;
070700110509                 WFnote1 = ORMno1;
070800110509                 WFnote2 = ORMno2;
070900110509                 WFrag   = ORMrsr;
071000110509                 WFind   = ORMinr;
071100110509                 WFcap   = ORMcar;
071200110509                 WFloc   = ORMlor;
071300110509                 WFprv   = ORMprr;
071400110509               ENDIF;
071500110510
071600110510             // -?Dati da ORG per peso e volume?
071700110509               chain (ORMpoe:ORMnsr:ORMnor:ORMnrv) FNORG01L;
071800110509               IF  %Found(FNORG01L);
071900110509                 WFpkg = ORGpkg;
072000110509                 WFvlm = ORGvlm;
072100110509               ENDIF;
072200110510
072300110510             // -?Se prepagato cerco il totale fattura?
072400110510               IF  ORMtor = 'P';
072500110510               // -?Cerco il n. bolla?
072600110510                 chain ('M':wkeyorm) FIAR404L;
072700110510                 IF  %found(FIAR404L);
072800110510                   chain (AR4aas:AR4lnp:AR4nrs:AR4nsp:'1') FIAR601L;
072900110510                   IF  %Found(FIAR601L);
073000110510                     WFift = AR6ift;
073100110510                   ENDIF;
073200110510                 ENDIF;
073300110510               ENDIF;
073400110509             ENDIF;
073500110509
073600110509           // -?Se ORM Manuale?
073700110509             IF  WFdocman = 'M';
073800110509               WFncl   = %dec(§RESncl:5:0);
073900110509               WFbnc   = %dec(§RESbnc:5:0);
074000110509               WFnote1 = §RESnote;
074100110509               WFrag   = §RESrsr;
074200110509               WFind   = §RESinr;
074300110509             ENDIF;
074400110509
074500110509           sav_DtOrin = §RESdtorin;
074600110506
074700110506         ENDSL;
074800101019
074900101019       ENDSR;
075000110509
075100110509       //--------------------------------------------------------------
075200110509       //?Memorizzo l'ultimo esito.
075300110509       //--------------------------------------------------------------
075400110509       BEGSR  MemEsito;
075500110509
075600110509         SELECT;
075700110509       // -?BOLLA?
075800110509           WHEN  PLGtipappl = 'C';
075900110509             FIPRDcetds = PLGdati;
076000110509             IF  sav_DtOrin < %editc(§CETdtorin:'X');
076100110509               WFesito = §CETcmc;
076200110509               clear WFdeses;
076300110509             // -?Decodifico l'esito?
076400110509               IF  WFesito <> *blanks;
076500110509                 clear ds2a;
076600110509                 k_TBLcod = '2A';
076700110509                 k_TBLkey = WFesito;
076800110509                 chain %kds(k03tabel00) TABEL00F;
076900110509                 IF  %Found(TABEL00F);
077000110509                   ds2a = TBLuni;
077100110509                 ENDIF;
077200110509                 WFdeses = §2Ades;
077300110509               ENDIF;
077400110509
077500110509               sav_DtOrin = %editc(§CETdtorin:'X');
077600110509             ENDIF;
077700110509
077800110509       // -?RITIRO?
077900110509           WHEN  PLGtipappl = 'R';
078000110509             FIPRCresds = PLGdati;
078100110509             IF  sav_DtOrin < §RESdtorin;
078200110509           // -?Ritiro Manuale memorizzo i dati del manuale?
078300110509               IF  %subst(PLGiddoc:1:8) = *all'0';
078400110509                 WFncl   = %dec(§RESncl:5:0);
078500110509                 WFbnc   = %dec(§RESbnc:5:0);
078600110509                 WFnote1 = §RESnote;
078700110509                 WFrag   = §RESrsr;
078800110509                 WFind   = §RESinr;
078900110509               ENDIF;
079000110509           // -?Ritiro da AS400?
079100110509               IF  %subst(PLGiddoc:1:8) <> *all'0';
079200110509                 WFesito = §REScmr;
079300110509                 clear WFdeses;
079400110509             // -?Decodifico l'esito?
079500110509                 IF  WFesito <> *blanks;
079600110509                   clear dCMR;
079700110509                   chain ('CMR':WFesito) TNTBE01L;
079800110509                   IF  %Found(TNTBE01L);
079900110509                     dCMR = TBEuni;
080000110509                   ENDIF;
080100110509                   WFdeses = d§CMRdes;
080200110509                 ENDIF;
080300110509               ENDIF;
080400110509
080500110509               sav_DtOrin = §RESdtorin;
080600110509             ENDIF;
080700110509
080800110509           ENDSL;
080900110509
081000110509       ENDSR;
081100100507
081200100507       //--------------------------------------------------------------
081300100507       //?Operazioni finali.                                           ?
081400100507       //--------------------------------------------------------------
081500110509       BEGSR  RoutEnd;
081600101011
081700101011         *inLR = *on;
081800100507
081900100510         // -?Uscita dal *pgm?
082000100507         return;
082100100507
082200100507       ENDSR;
082300100507
082400100507      /end-free
082500110506
082600110506       //--------------------------------------------------------------
082700110506       //?Schiere a tempo di compilazione.
082800110506       //--------------------------------------------------------------
082900110506
083000110506** - $MSG -------------------------------------------------------------------*
083100110506Filiale errata                                                                01
083200110509Data errata                                                                   02
083300110509PDA non attivo per la filiale in gestione                                     03
