000100180307      //--------------------------------------------------------------
000200180307      //?FIDNE0R - ANALISI RA PER OBIETTIVI
000300180307      //--------------------------------------------------------------
000400180307     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500180307     h dftactgrp(*no) actgrp(*caller)
000600180307
000700180307      //---------------------------------------------------------------
000800180307      //?Dichiarazione file.
000900180307      //---------------------------------------------------------------
001000180307      // - Organigramma
001100180307     fAZORG01L  if   e           k disk
001200180308
001300180308      // - Tabelle
001400180308     fTABEL00F  if   e           k disk
001500180308
001600180307      // - Video Gestione Campagne
001700180307     fFIDNE0D   cf   e             workstn
001800180307     f                                     indds(IndDspF)
001900180307     f                                     infds(InfDspF)
002000180307
002100180307      //---------------------------------------------------------------
002200180307      //?Definizione costanti.
002300180307      //---------------------------------------------------------------
002400180307      // - Tasti funzionali a video
002500180307     d c_F01           c                   const(x'31')
002600180307     d c_F02           c                   const(x'32')
002700180307     d c_F03           c                   const(x'33')
002800180307     d c_F04           c                   const(x'34')
002900180307     d c_F05           c                   const(x'35')
003000180307     d c_F06           c                   const(x'36')
003100180307     d c_F07           c                   const(x'37')
003200180307     d c_F08           c                   const(x'38')
003300180307     d c_F09           c                   const(x'39')
003400180307     d c_F10           c                   const(x'3A')
003500180307     d c_F11           c                   const(x'3B')
003600180307     d c_F12           c                   const(x'3C')
003700180307     d c_F13           c                   const(x'B1')
003800180307     d c_F14           c                   const(x'B2')
003900180307     d c_F15           c                   const(x'B3')
004000180307     d c_F16           c                   const(x'B4')
004100180307     d c_F17           c                   const(x'B5')
004200180307     d c_F18           c                   const(x'B6')
004300180307     d c_F19           c                   const(x'B7')
004400180307     d c_F20           c                   const(x'B8')
004500180307     d c_F21           c                   const(x'B9')
004600180307     d c_F22           c                   const(x'BA')
004700180307     d c_F23           c                   const(x'BB')
004800180307     d c_F24           c                   const(x'BC')
004900180307     d c_Enter         c                   const(x'F1')
005000180307     d c_RollDown      c                   const(x'F4')
005100180307     d c_RollUp        c                   const(x'F5')
005200180307
005300180307     d c_Titolo        c                   const('* Analisi RA *')
005400180307
005500180307     d Digits          c                   const('0123456789')
005600180307
005700180307      //---------------------------------------------------------------
005800180307      //?Definizione schiere.
005900180307      //---------------------------------------------------------------
006000180307      // - Messaggi di errore
006100180307     d Msg             s             78    dim(10) ctdata perrcd(1)
006200180307
006300180307      //---------------------------------------------------------------
006400180307      //?Definizione aree dati.
006500180307      //---------------------------------------------------------------
006600180307      // - Dati utente
006700180307     d §AzUte        e ds                  extname(AZUTE00F)
006800180307     d                                     dtaara
006900180307     d §DatiUte      e ds                  extname(dDatiUte)
007000180307     d                                     dtaara
007100180307
007200180307      //---------------------------------------------------------------
007300180307      //?Definizione strutture dati.
007400180307      //---------------------------------------------------------------
007500180307      // - Status
007600180307     d Psds           sds
007700180307     d   SDSpgm          *proc
007800180307
007900180307      // - InfDS
008000180307     d InfDspF         ds
008100180307     d  dsp_aid              369    369a
008200180307     d  sfl_rrn              376    377i 0
008300180307     d  min_nrr              378    379i 0
008400180307     d  num_rcds             380    381i 0
008500180307
008600180307      // - Indicatori su DspF
008700180307     d IndDspF         ds
008800180307        // - Indicatori di errore in videata
008900180307     d  ErrMessage                    1n   overlay(IndDspF : 28)
009000180307        // - Indicatori di gestione del subfile
009100180307     d  SflDsp                        1n   overlay(IndDspF : 30)
009200180307     d  SflDspCtl                     1n   overlay(IndDspF : 31)
009300180307     d  SflNxtChg                     1n   overlay(IndDspF : 32)
009400180307     d  SflEnd                        1n   overlay(IndDspF : 33)
009500180307        // - Indicatori di errore
009600180307     d  PosCurDTINI                   1n   overlay(IndDspF : 50)
009700180307     d  PosCurDTFIN                   1n   overlay(IndDspF : 51)
009800180307     d  PosCurAREA                    1n   overlay(IndDspF : 52)
009900180307     d  PosCurFIL                     1n   overlay(IndDspF : 53)
010000180307        // - Indicatori di errore generico
010100180307     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010200180307
010300180307     d WindDspF        ds                  inz
010400180307     d  WindDspFa              1     49    inz(*zero)
010500180307     d  WindDspFb             50     99    inz(*zero)
010600180308
010700180308     d Wlbdat          ds                  inz
010800180308     d  G08dat                 1      8  0
010900180308     d  G08inv                 9     16  0
011000180308     d  G08err                17     17
011100180308     d  G08tgi                18     22  0
011200180307
011300180307      // - Parametri ricevuti
011400180307     d KPJBA         e ds
011500180308
011600180308      // - Passaggio dati a pgm batch
011700180308     d FIDNE1DS      e ds                  inz
011800180307
011900180307      // - Reperimento dati utente
012000180307     d TIBS34DS      e ds
012100180307
012200180308      // - Tabella 05 - Aree
012300180308     d ds05          e ds
012400180308
012500180307      //---------------------------------------------------------------
012600180307      //?Definizione variabili globali.
012700180307      //---------------------------------------------------------------
012800180307      // - Flags booleani
012900180307     d ErrGrave        s               n   inz(*off)
013000180307     d Fine            s               n   inz(*off)
013100180307     d wInzD02         s               n   inz(*off)
013200180307
013300180307      // - Campi associati al video
013400180307     d Video           s              2a   inz
013500180307
013600180307      // - Campi di comodo data
013700180307     d Data_EUR        s               d   datfmt(*eur)
013800180307     d Data_ISO        s               d   datfmt(*iso)
013900180307
014000180307      // - Campi di comodo
014100180308     d InvDtIni        s              8s 0 inz
014200180308     d InvDtFin        s              8s 0 inz
014300180307     d Oggi            s              8s 0 inz
014400180307     d w0030           s              3s 0 inz
014500180307
014600180307      //---------------------------------------------------------------
014700180307      //?Definizione procedure usate.
014800180307      //---------------------------------------------------------------
014900180308      // - Sottomissione lavoro batch
015000180308     d bch10           pr                  extpgm('BCH10')
015100180308     d  kpjba                              likeds(KPJBA)
015200180308
015300180308       // - Analisi RA per Obiettivi
015400180308     d FIDNE1R         pr                  extpgm('FIDNE1R')
015500180308     d  kpjba                              likeds(kpjba)
015600180308
015700180307      //---------------------------------------------------------------
015800180307      //?Definizione Prototipi.
015900180307      //---------------------------------------------------------------
016000180307      /copy gaitrasrc/srcprotopr,TIBS34R
016100180308      /copy gaitrasrc/srcprotopr,XSRDA8
016200180308
016300180307      //---------------------------------------------------------------
016400180307      //?Definizione key-list.
016500180307      //---------------------------------------------------------------
016600180307      // - File TABEL00F
016700180307     d k03tabel      e ds                  extname(TABEL00F:*key)
016800180307     d                                     prefix(k_)
016900180307
017000180307      //---------------------------------------------------------------
017100180307      //?M A I N - L I N E
017200180307      //---------------------------------------------------------------
017300180307     c     *Entry        plist
017400180307     c                   parm                    kpjba
017500180307
017600180307      //?Operazioni iniziali
017700180307       exsr RoutInz;
017800180307
017900180307      //?Gestione video
018000180307       DOW  Fine = *off;
018100180307         SELECT;
018200180307         WHEN  Video = 'D2';
018300180307           exsr GesD02;
018400180307         OTHER;
018500180307           Fine = *on;
018600180307         ENDSL;
018700180307       ENDDO;
018800180307
018900180307      //?Operazioni finali
019000180307       exsr RoutEnd;
019100180307
019200180307      //--------------------------------------------------------------
019300180307      //?Operazioni iniziali.
019400180307      //--------------------------------------------------------------
019500180307       BEGSR RoutInz;
019600180307
019700180307      //?Reperimento dati job
019800180307        exsr DatiJob;
019900180307
020000180307      //?Impostazione campi "fissi"
020100180307        V01pgm = SDSpgm;
020200180307        Video = 'D2';
020300180307        wInzD02 = *on;
020400180308        k_TBLkut = 1;
020500180307
020600180307      //?Imposto oggi
020700180307        Oggi = %dec(%date());
020800180307
020900180307      //?Imposto il titolo
021000180307        V01tit = c_Titolo;
021100180307
021200180307       ENDSR;
021300180307
021400180307      //--------------------------------------------------------------
021500180307      //?Reperimento Dati del job (Utente/Operativi).
021600180307      //--------------------------------------------------------------
021700180307       BEGSR DatiJob;
021800180307
021900180307        in(E) §AzUte;
022000180307        IF NOT %error;
022100180307          in(E) §DatiUte;
022200180307        ENDIF;
022300180307        IF %error or RSut = *blanks;
022400180307          clear TIBS34ds;
022500180307          tibs34r(tibs34ds);
022600180307          in §AzUte;
022700180307          in §DatiUte;
022800180307        ENDIF;
022900180307
023000180307       ENDSR;
023100180307
023200180307      //--------------------------------------------------------------
023300180307      //?Gestione videata D02.
023400180307      //--------------------------------------------------------------
023500180307       BEGSR GesD02;
023600180307
023700180307      //?Inizializzazione videata
023800180307        IF wInzD02;
023900180307          exsr InzD02;
024000180307          wInzD02 = *off;
024100180307        ENDIF;
024200180307
024300180307      //?Emissione Testata
024400180307        write DNE0T01;
024500180307
024600180307      //?Emissione videata
024700180307        exfmt DNE0D02;
024800180307        reset ErrMessage;
024900180307        reset ErrGenerico;
025000180307        clear V02msg;
025100180307
025200180307        SELECT;
025300180307
025400180307      //?- Errore Grave esco
025500180307        WHEN ErrGrave;
025600180307          Fine = *on;
025700180307
025800180307      //?- F03=Fine
025900180307        WHEN dsp_aid = c_F03;
026000180307          exsr F03D02;
026100180307
026200180307      //?- F06=Elabora
026300180307        WHEN dsp_aid = c_F06;
026400180308          exsr ContrD02;
026500180308          IF ErrGenerico;
026600180308            leavesr;
026700180308          ENDIF;
026800180307          exsr F06D02;
026900180307
027000180307      //?Invio
027100180307        OTHER;
027200180307          exsr ContrD02;
027300180307          IF ErrGenerico;
027400180307            leavesr;
027500180307          ENDIF;
027600180307
027700180307        ENDSL;
027800180307
027900180307       ENDSR;
028000180307
028100180307      //--------------------------------------------------------------
028200180307      //?Inizializzazione Videata D02.
028300180307      //--------------------------------------------------------------
028400180307       BEGSR InzD02;
028500180307
028600180307      //?Pulizia videata
028700180307        clear DNE0D02;
028800180307
028900180307       ENDSR;
029000180307
029100180307      //--------------------------------------------------------------
029200180307      //?Gestione tasto funzionale F03 da videata D02
029300180307      //?F03=Fine
029400180307      //--------------------------------------------------------------
029500180307       BEGSR F03D02;
029600180307
029700180307      //?Chiusura del programma
029800180307        Fine = *on;
029900180307
030000180307       ENDSR;
030100180308
030200180308      //--------------------------------------------------------------
030300180308      //?Gestione tasto funzionale F06 da videata D02
030400180308      //?F06=Elabora
030500180308      //--------------------------------------------------------------
030600180308       BEGSR F06D02;
030700180308
030800180308      //?Lancio il report
030900180308        clear FIDNE1DS;
031000180308        DNE1dtini = invdtini;
031100180308        DNE1dtfin = invdtfin;
031200180308        DNE1area  = V02area;
031300180308        DNE1fil   = V02fil;
031400180308
031500180308        kcoaz = 'DNE1';
031600180308        kpjbu = FIDNE1DS;
031700180308
031800180308      //?Richiamo interattivo
031900180308        IF knmus = *all'1';
032000180308          FIDNE1R(kpjba);
032100180308      //?Richiamo batch
032200180308        ELSE;
032300180308          BCH10(kpjba);
032400180308        ENDIF;
032500180308
032600180308      //?Chiusura del programma
032700180308        Fine = *on;
032800180308
032900180308       ENDSR;
033000180307
033100180307      //--------------------------------------------------------------
033200180307      //?Controlla Videata D02.
033300180307      //--------------------------------------------------------------
033400180307       BEGSR ContrD02;
033500180307
033600180307        WindDspF = IndDspF;
033700180307        reset WindDspFb;
033800180307        IndDspF  = WindDspF;
033900180308
034000180308      //?Data obbligatoria
034100180308        IF V02dtini = *zeros;
034200180308          ErrMessage = *on;
034300180308          ErrGenerico = *on;
034400180308          PosCurDTINI = *on;
034500180308          V02msg = Msg(01);
034600180308          leavesr;
034700180308        ENDIF;
034800180308
034900180308      //?Data valida
035000180308        clear Wlbdat;
035100180308        G08dat = V02dtini;
035200180308        xsrda8(Wlbdat);
035300180308        IF G08err = '1';
035400180308          ErrMessage = *on;
035500180308          ErrGenerico = *on;
035600180308          PosCurDTINI = *on;
035700180308          V02msg = Msg(01);
035800180308          leavesr;
035900180308        ENDIF;
036000180308        V02dtini = G08dat;
036100180308        invdtini = G08inv;
036200180308
036300180308      //?Data obbligatoria
036400180308        IF V02dtfin = *zeros;
036500180308          ErrMessage = *on;
036600180308          ErrGenerico = *on;
036700180308          PosCurDTFIN = *on;
036800180308          V02msg = Msg(01);
036900180308          leavesr;
037000180308        ENDIF;
037100180308
037200180308      //?Data valida
037300180308        clear Wlbdat;
037400180308        G08dat = V02dtfin;
037500180308        xsrda8(Wlbdat);
037600180308        IF G08err = '1';
037700180308          ErrMessage = *on;
037800180308          ErrGenerico = *on;
037900180308          PosCurDTFIN = *on;
038000180308          V02msg = Msg(01);
038100180308          leavesr;
038200180308        ENDIF;
038300180308        V02dtfin = G08dat;
038400180308        invdtfin = G08inv;
038500180308
038600180308      //Congruenza date
038700180308        IF V02dtini > V02dtfin;
038800180308          ErrMessage = *on;
038900180308          ErrGenerico = *on;
039000180308          PosCurDTINI = *on;
039100180308          V02msg = Msg(02);
039200180308          leavesr;
039300180308        ENDIF;
039400180307
039500180308      //Area valida se immessa
039600180308        clear ds05;
039700180308        IF V02area > *zeros;
039800180308          k_TBLcod = '05';
039900180308          k_TBLkey = %editc(V02area:'X');
040000180308          chain %kds(K03tabel) TABEL00F;
040100180308          IF not %found(TABEL00F) or TBLflg <> *blanks;
040200180308            ErrMessage = *on;
040300180308            ErrGenerico = *on;
040400180308            PosCurAREA = *on;
040500180308            V02msg = Msg(03);
040600180308            leavesr;
040700180308          ENDIF;
040800180308          ds05 = TBLuni;
040900180308          V02aread = §05des;
041000180308        ENDIF;
041100180308
041200180308      //Filiale valida se immessa
041300180308        IF V02fil > *zeros;
041400180308          chain V02fil AZORG01L;
041500180308          IF not %found(AZORG01L) or ORGfva <> *blanks;
041600180308            ErrMessage  = *on;
041700180308            ErrGenerico = *on;
041800180308            PosCurFIL = *on;
041900180308            V02msg = Msg(04);
042000180308            leavesr;
042100180308          ENDIF;
042200180308          V02fild = ORGdes;
042300180307        ENDIF;
042400180308
042500180308      //Se immessa anche l'area la Filiale deve essere dell'area indicata
042600180308        IF V02area > *zeros and V02fil > *zeros and ORGcar <> V02area;
042700180308          ErrMessage = *on;
042800180308          ErrGenerico = *on;
042900180308          PosCurFIL = *on;
043000180308          V02msg = Msg(05);
043100180308          leavesr;
043200180308        ENDIF;
043300180307
043400180307       ENDSR;
043500180307
043600180307      //--------------------------------------------------------------
043700180307      //?Operazioni finali.
043800180307      //--------------------------------------------------------------
043900180307       BEGSR RoutEnd;
044000180307
044100180307        *inLR = *on;
044200180307        return;
044300180307
044400180307       ENDSR;
044500180307
044600180307       //--------------------------------------------------------------
044700180307       //?Schiere a tempo di compilazione.
044800180307       //--------------------------------------------------------------
044900180307
045000180307** -- MSG -------------------------------------------------------------------*
045100180307Data Errata                                                                    1
045200180308Date Incongruenti                                                              2
045300180308Area Errata                                                                    3
045400180308Filiale Errata                                                                 4
045500180308La filiale immessa non appartiene all'area richiesta                           5
