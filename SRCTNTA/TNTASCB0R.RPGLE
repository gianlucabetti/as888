000100170125      //--------------------------------------------------------------
000200170125      //?TNTASCB0R - Conteggio Clienti x Supporto da TITAS - Lancio
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125     h dftactgrp(*no) actgrp(*caller)
000600170125
000700170125      //---------------------------------------------------------------
000800170125      //?Dichiarazione file.
000900170125      //---------------------------------------------------------------
001000170125      // - Organigramma
001100170125     fAZORG01L  if   e           k disk
001200170125
001300170125     fTNTASCB0D cf   e             workstn
001400170125     f                                     indds(IndDspF)
001500170125     f                                     infds(InfDspF)
001600170125
001700170125      //---------------------------------------------------------------
001800170125      //?Definizione costanti.
001900170125      //---------------------------------------------------------------
002000170125      // - Tasti funzionali a video
002100170125     d c_F01           c                   const(x'31')
002200170125     d c_F02           c                   const(x'32')
002300170125     d c_F03           c                   const(x'33')
002400170125     d c_F04           c                   const(x'34')
002500170125     d c_F05           c                   const(x'35')
002600170125     d c_F06           c                   const(x'36')
002700170125     d c_F07           c                   const(x'37')
002800170125     d c_F08           c                   const(x'38')
002900170125     d c_F09           c                   const(x'39')
003000170125     d c_F10           c                   const(x'3A')
003100170125     d c_F11           c                   const(x'3B')
003200170125     d c_F12           c                   const(x'3C')
003300170125     d c_F13           c                   const(x'B1')
003400170125     d c_F14           c                   const(x'B2')
003500170125     d c_F15           c                   const(x'B3')
003600170125     d c_F16           c                   const(x'B4')
003700170125     d c_F17           c                   const(x'B5')
003800170125     d c_F18           c                   const(x'B6')
003900170125     d c_F19           c                   const(x'B7')
004000170125     d c_F20           c                   const(x'B8')
004100170125     d c_F21           c                   const(x'B9')
004200170125     d c_F22           c                   const(x'BA')
004300170125     d c_F23           c                   const(x'BB')
004400170125     d c_F24           c                   const(x'BC')
004500170125     d c_Enter         c                   const(x'F1')
004600170125     d c_RollDown      c                   const(x'F4')
004700170125     d c_RollUp        c                   const(x'F5')
004800170125
004900170125     d Digits          c                   const('0123456789')
005000170125
005100170125      //---------------------------------------------------------------
005200170125      //?Definizione schiere.
005300170125      //---------------------------------------------------------------
005400170125      // - Messaggi di errore
005500170125     d Msg             s             78    dim(10) ctdata perrcd(1)
005600170125
005700170125      //---------------------------------------------------------------
005800170125      //?Definizione aree dati.
005900170125      //---------------------------------------------------------------
006000170125      // - Dati utente
006100170125     d §AzUte        e ds                  extname(AZUTE00F)
006200170125     d                                     dtaara
006300170125     d §DatiUte      e ds                  extname(dDatiUte)
006400170125     d                                     dtaara
006500170125
006600170125      //---------------------------------------------------------------
006700170125      //?Definizione strutture dati.
006800170125      //---------------------------------------------------------------
006900170125      // - Status
007000170125     d Psds           sds
007100170125     d   SDSpgm          *proc
007200170125
007300170125      // - InfDS
007400170125     d InfDspF         ds
007500170125     d  dsp_aid              369    369a
007600170125
007700170125      // - Indicatori su DspF
007800170125     d IndDspF         ds
007900170125        // - Indicatori di errore
008000170125     d  ErrMessage                    1n   overlay(IndDspF : 28)
008100170125     d  PosCurDataI                   1n   overlay(IndDspF : 50)
008200170125     d  PosCurDataF                   1n   overlay(IndDspF : 51)
008300170125     d  PosCurLNA                     1n   overlay(IndDspF : 52)
008400170125        // - Indicatori di errore generico
008500170125     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008600170125
008700170125     d WindDspF        ds                  inz
008800170125     d  WindDspFa              1     49    inz(*zero)
008900170125     d  WindDspFb             50     99    inz(*zero)
009000170125
009100170125     d wlbdat          ds                  inz
009200170125     d  g02dat                 1      8  0
009300170125     d  g02inv                 9     16  0
009400170125     d  g02err                17     17
009500170125     d  g02tgi                18     22  0
009600170125
009700170125      // - Parametri ricevuti
009800170125     d KPJBA         e ds
009900170125
010000170125      // - Reperimento dati utente
010100170125     d TIBS34DS      e ds
010200170125
010300170125      // - Passaggio parametri a TNTASBC1R
010400170125     d TNTASCBDS     e ds
010500170125
010600170125      //---------------------------------------------------------------
010700170125      //?Definizione variabili globali.
010800170125      //---------------------------------------------------------------
010900170125      // - Flags booleani
011000170125     d ErrGrave        s               n   inz(*off)
011100170125     d Fine            s               n   inz(*off)
011200170125     d wEoF            s               n   inz
011300170125     d wInzD01         s               n   inz(*on)
011400170125
011500170125      // - Campi per QCMDEXC
011600170125     d cmd             s            512a   varying
011700170125
011800170125      // - Campi associati al video
011900170125     d Video           s              2    inz('D1')
012000170125
012100170125      // - Campi di comodo data
012200170125     d Data_eur        s               d   Datfmt(*eur)
012300170125     d Data_iso        s               d   Datfmt(*iso)
012400170125     d Oggi            s              8  0 inz
012500170125
012600170125      // - Campi di comodo
012700170125     d wDataI          s              8  0 inz
012800170125     d wDataF          s              8  0 inz
012900170125
013000170125      //---------------------------------------------------------------
013100170125      //?Definizione Procedure usate.
013200170125      //---------------------------------------------------------------
013300170125      // - Sottomissione lavoro batch
013400170125     d bch10           pr                  extpgm('BCH10')
013500170125     d  kpjba                              likeds(KPJBA)
013600170125
013700170125      // - Richiamo diretto lavoro batch
013800170125     d TNTASCB1R       pr                  extpgm('TNTASCB1R')
013900170125     d  kpjba                              likeds(KPJBA)
014000170125
014100170125      //---------------------------------------------------------------
014200170125      //?Definizione Prototipi.
014300170125      //---------------------------------------------------------------
014400170125      /copy gaitrasrc/srcprotopr,SYSTEM
014500170125      /copy gaitrasrc/srcprotopr,TIBS34R
014600170125      /copy gaitrasrc/srcprotopr,XSRDA8
014700170125
014800170125      //---------------------------------------------------------------
014900170125      //?Definizione key-list.
015000170125      //---------------------------------------------------------------
015100170125
015200170125      //---------------------------------------------------------------
015300170125      //?M A I N - L I N E
015400170125      //---------------------------------------------------------------
015500170125     c     *Entry        plist
015600170125     c                   parm                    kpjba
015700170125
015800170125      /free
015900170125
016000170125       //?Operazioni iniziali
016100170125       exsr RoutInz;
016200170125
016300170125       //?Gestione video
016400170125       DOW  Fine = *off;
016500170125         SELECT;
016600170125         WHEN Video = 'D1';
016700170125           exsr GesD01;
016800170125         OTHER;
016900170125           Fine = *on;
017000170125         ENDSL;
017100170125       ENDDO;
017200170125
017300170125       //?Operazioni finali
017400170125       exsr RoutEnd;
017500170125
017600170125       //--------------------------------------------------------------
017700170125       //?Operazioni iniziali.
017800170125       //--------------------------------------------------------------
017900170125       BEGSR RoutInz;
018000170125
018100170125       //?Reperimento dati job
018200170125         exsr DatiJob;
018300170125
018400170125       //?Calcolo oggi
018500170125         Oggi = %dec(%date());
018600170125
018700170125       ENDSR;
018800170125
018900170125       //--------------------------------------------------------------
019000170125       //?Reperimento Dati del job (Utente/Operativi).
019100170125       //--------------------------------------------------------------
019200170125       BEGSR DatiJob;
019300170125
019400170125         in(E) §AzUte;
019500170125         IF  not %error;
019600170125           in(E) §DatiUte;
019700170125         ENDIF;
019800170125         IF  %error or RSut = *blanks;
019900170125           clear TIBS34ds;
020000170125           tibs34r(tibs34ds);
020100170125           in §AzUte;
020200170125           in §DatiUte;
020300170125         ENDIF;
020400170125
020500170125       ENDSR;
020600170125
020700170125       //--------------------------------------------------------------
020800170125       //?Gestione videata D01
020900170125       //--------------------------------------------------------------
021000170125       BEGSR GesD01;
021100170125
021200170125       //?Inizializzazione videata
021300170125         IF  wInzD01;
021400170125           exsr InzD01;
021500170125           wInzD01 = *off;
021600170125         ENDIF;
021700170125
021800170125       //?Emissione videata
021900170125         exfmt TASCB01;
022000170125         ErrMessage   = *off;
022100170125         ErrGenerico  = *off;
022200170125         clear V01msg;
022300170125
022400170125         SELECT;
022500170125
022600170125       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
022700170125         WHEN  ErrGrave;
022800170125           Fine = *on;
022900170125
023000170125       //?F3=Fine
023100170125         WHEN  dsp_aid = c_F03;
023200170125           exsr F03D01;
023300170125
023400170125       //?F6=Conferma
023500170125         WHEN  dsp_aid = c_F06;
023600170125           exsr CtrD01;
023700170125           IF  ErrGenerico;
023800170125             leavesr;
023900170125           ENDIF;
024000170125           exsr F06D01;
024100170125
024200170125       //?F8=Visualizza File WFCBA00F
024300170125         WHEN  dsp_aid = c_F08;
024400170125           exsr F08D01;
024500170125
024600170125       //?Enter
024700170125         OTHER;
024800170125           exsr CtrD01;
024900170125           IF  ErrGenerico;
025000170125             leavesr;
025100170125           ENDIF;
025200170125         ENDSL;
025300170125
025400170125       ENDSR;
025500170125
025600170125       //--------------------------------------------------------------
025700170125       //?Inizializzazione videata D01
025800170125       //--------------------------------------------------------------
025900170125       BEGSR InzD01;
026000170125
026100170125         clear TASCB01;
026200170125
026300170125         V01lna = 999;
026400170125         V01dlna = '999 = Tutte';
026500170125
026600170125       ENDSR;
026700170125
026800170125       //--------------------------------------------------------------
026900170125       //?Gestione tasto funzionale F3 da videata D01
027000170125       //?F3=Fine
027100170125       //--------------------------------------------------------------
027200170125       BEGSR F03D01;
027300170125
027400170125       //?Chiusura del programma
027500170125         Fine = *on;
027600170125
027700170125       ENDSR;
027800170125
027900170125       //--------------------------------------------------------------
028000170125       //?Gestione tasto funzionale F6 da videata D01
028100170125       //?F6=Conferma
028200170125       //--------------------------------------------------------------
028300170125       BEGSR F06D01;
028400170125
028500170125         clear kpjbu;
028600170125         clear tntascbds;
028700170125         ISCBdatai = wDataI;
028800170125         ISCBdataf = wDataF;
028900170125         ISCBlna = V01lna;
029000170125
029100170125         kcoaz = 'TAS1';
029200170125         kpjbu = TNTASCBDS;
029300170125         kbuff = *blanks;
029400170125
029500170125       //?Richiamo interattivo
029600170125         IF  knmus = *all'1';
029700170125           TNTASCB1R (kpjba);
029800170125       //?Richiamo batch
029900170125         ELSE;
030000170125           BCH10(kpjba);
030100170125         ENDIF;
030200170125
030300170125       //?Chiusura del programma
030400170125         Fine = *on;
030500170125
030600170125       ENDSR;
030700170125
030800170125       //--------------------------------------------------------------
030900170125       //?Gestione tasto funzionale F8 da videata D01
031000170125       //?F8=Visualizza File
031100170125       //--------------------------------------------------------------
031200170125       BEGSR F08D01;
031300170125
031400170125         cmd = 'DSPDBF FILE(';
031500170125         IF  %subst(knsif : 7 : 1) = 'P';
031600170125           cmd = %trimr(cmd) + 'GAITRAAZP';
031700170125         ELSE;
031800170125           cmd = %trimr(cmd) + 'GAITRAAZM';
031900170125         ENDIF;
032000170125         cmd = %trimr(cmd) + '/WFSCB00F) mbr(*first) Output(*)';
032100170125
032200170125         ExecuteCommand (cmd);
032300170125
032400170125       ENDSR;
032500170125
032600170125       //--------------------------------------------------------------
032700170125       //?Controllo videata D01
032800170125       //--------------------------------------------------------------
032900170125       BEGSR CtrD01;
033000170125
033100170125         clear V01dlna;
033200170125
033300170125         clear wDataF;
033400170125         clear wDataI;
033500170125
033600170125         WindDspF = IndDspF;
033700170125         reset WindDspFb;
033800170125         IndDspF  = WindDspF;
033900170125
034000170125       //?Periodo elaborazione
034100170125         IF  V01datai = 0 and V01dataf = 0;
034200170125           ErrMessage  = *on;
034300170125           ErrGenerico = *on;
034400170125           PosCurDataI = *on;
034500170125           V01msg      = Msg(01);
034600170125           leavesr;
034700170125         ENDIF;
034800170125
034900170125       //?Periodo "dal"
035000170125         IF  V01datai <> 0;
035100170125           clear wlbdat;
035200170125           G02dat = V01datai;
035300170125           xsrda8(wlbdat);
035400170125           IF  G02err = '1';
035500170125             ErrMessage  = *on;
035600170125             ErrGenerico = *on;
035700170125             PosCurDataI = *on;
035800170125             V01msg      = Msg(01);
035900170125             leavesr;
036000170125           ENDIF;
036100170125
036200170125           V01datai = G02dat;
036300170125           wDataI   = G02inv;
036400170125         ENDIF;
036500170125
036600170125       //?Periodo "al"
036700170125         IF  V01dataf <> 0;
036800170125           clear wlbdat;
036900170125           G02dat = V01dataf;
037000170125           xsrda8(wlbdat);
037100170125           IF  G02err = '1';
037200170125             ErrMessage  = *on;
037300170125             ErrGenerico = *on;
037400170125             PosCurDataF = *on;
037500170125             V01msg      = Msg(01);
037600170125             leavesr;
037700170125           ENDIF;
037800170125
037900170125           V01dataf = G02dat;
038000170125           wDataF   = G02inv;
038100170125         ENDIF;
038200170125
038300170125       //?"DAL" congruente con "AL"
038400170125         IF  wDataI > 0 and wDataF = 0;
038500170125           ErrMessage  = *on;
038600170125           ErrGenerico = *on;
038700170125           PosCurDataI = *on;
038800170125           V01msg      = Msg(02);
038900170125           leavesr;
039000170125         ENDIF;
039100170125         IF  wDataI > 0 and wDataF > 0 and wDataI > wDataF;
039200170125           ErrMessage  = *on;
039300170125           ErrGenerico = *on;
039400170125           PosCurDataF = *on;
039500170125           V01msg      = Msg(02);
039600170125           leavesr;
039700170125         ENDIF;
039800170125
039900170125       //?Linea di arrivo
040000170125         IF  V01lna = 0;
040100170125           V01dlna = '999 = Tutte';
040200170125           ErrMessage  = *on;
040300170125           ErrGenerico = *on;
040400170125           PosCurLNA   = *on;
040500170125           V01msg      = Msg(03);
040600170125           leavesr;
040700170125         ENDIF;
040800170125
040900170125         IF  V01lna = 999;
041000170125           V01dlna = '999 = Tutte';
041100170125           leavesr;
041200170125         ENDIF;
041300170125
041400170125         chain V01lna AZORG01L;
041500170125         IF  not %found(AZORG01L) or
041600170125             ORGfva <> *blanks or
041700170125             (ORGfag <> 'F' and ORGfag <> 'A');
041800170125           ErrMessage  = *on;
041900170125           ErrGenerico = *on;
042000170125           PosCurLNA   = *on;
042100170125           V01msg      = Msg(03);
042200170125           leavesr;
042300170125         ENDIF;
042400170125         V01dlna = ORGdes;
042500170125
042600170125       ENDSR;
042700170125
042800170125       //--------------------------------------------------------------
042900170125       //?Operazioni finali.
043000170125       //--------------------------------------------------------------
043100170125       BEGSR RoutEnd;
043200170125
043300170125         *inLR = *on;
043400170125         return;
043500170125
043600170125       ENDSR;
043700170125
043800170125      /end-free
043900170125
044000170125** - MSG  ---------------------------------------------*
044100170125Data Errata                                             01
044200170125Data "DAL" incongruente con data "AL"                   02
044300170125Data Linea arrivo errata                                03
