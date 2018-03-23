000100170125      //--------------------------------------------------------------
000200170125      //?TNTACBA0R - Conteggio Clienti x Supporto da TITAS - Lancio
000300170125      //--------------------------------------------------------------
000400170125     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500170125
000600170125      //---------------------------------------------------------------
000700170125      //?Dichiarazione file.
000800170125      //---------------------------------------------------------------
000900170125     fTNTACBA0D cf   e             workstn
001000170125
001100170125      //---------------------------------------------------------------
001200170125      //?Definizione costanti.
001300170125      //---------------------------------------------------------------
001400170125      // - Tasti funzionali a video
001500170125     d c_F01           c                   const(x'31')
001600170125     d c_F02           c                   const(x'32')
001700170125     d c_F03           c                   const(x'33')
001800170125     d c_F04           c                   const(x'34')
001900170125     d c_F05           c                   const(x'35')
002000170125     d c_F06           c                   const(x'36')
002100170125     d c_F07           c                   const(x'37')
002200170125     d c_F08           c                   const(x'38')
002300170125     d c_F09           c                   const(x'39')
002400170125     d c_F10           c                   const(x'3A')
002500170125     d c_F11           c                   const(x'3B')
002600170125     d c_F12           c                   const(x'3C')
002700170125     d c_F13           c                   const(x'B1')
002800170125     d c_F14           c                   const(x'B2')
002900170125     d c_F15           c                   const(x'B3')
003000170125     d c_F16           c                   const(x'B4')
003100170125     d c_F17           c                   const(x'B5')
003200170125     d c_F18           c                   const(x'B6')
003300170125     d c_F19           c                   const(x'B7')
003400170125     d c_F20           c                   const(x'B8')
003500170125     d c_F21           c                   const(x'B9')
003600170125     d c_F22           c                   const(x'BA')
003700170125     d c_F23           c                   const(x'BB')
003800170125     d c_F24           c                   const(x'BC')
003900170125     d c_Enter         c                   const(x'F1')
004000170125     d c_RollDown      c                   const(x'F4')
004100170125     d c_RollUp        c                   const(x'F5')
004200170125
004300170125     d Digits          c                   const('0123456789')
004400170125
004500170125      //---------------------------------------------------------------
004600170125      //?Definizione schiere.
004700170125      //---------------------------------------------------------------
004800170125      // - Messaggi di errore
004900170125     d Msg             s             78    dim(10) ctdata perrcd(1)
005000170125
005100170125      //---------------------------------------------------------------
005200170125      //?Definizione aree dati.
005300170125      //---------------------------------------------------------------
005400170125      // - Dati utente
005500170125     d §AzUte        e ds                  extname(AZUTE00F)
005600170125     d                                     dtaara
005700170125     d §DatiUte      e ds                  extname(dDatiUte)
005800170125     d                                     dtaara
005900170125
006000170125      //---------------------------------------------------------------
006100170125      //?Definizione strutture dati.
006200170125      //---------------------------------------------------------------
006300170125      // - Status
006400170125     d Psds           sds
006500170125     d   SDSpgm          *proc
006600170125
006700170125      // - InfDS
006800170125     d InfDspF         ds
006900170125     d  dsp_aid              369    369a
007000170125
007100170125      // - Indicatori su DspF
007200170125     d IndDspF         ds
007300170125        // - Indicatori di errore
007400170125     d  ErrMessage                    1n   overlay(IndDspF : 28)
007500170125     d  PosCurDataI                   1n   overlay(IndDspF : 50)
007600170125     d  PosCurDataF                   1n   overlay(IndDspF : 51)
007700170125     d  PosCurLNA                     1n   overlay(IndDspF : 52)
007800170125        // - Indicatori di errore generico
007900170125     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008000170125
008100170125     d WindDspF        ds                  inz
008200170125     d  WindDspFa              1     49    inz(*zero)
008300170125     d  WindDspFb             50     99    inz(*zero)
008400170125
008500170125     d wlbdat          ds                  inz
008600170125     d  g02dat                 1      8  0
008700170125     d  g02inv                 9     16  0
008800170125     d  g02err                17     17
008900170125     d  g02tgi                18     22  0
009000170125
009100170125      // - Parametri ricevuti
009200170125     d KPJBA         e ds
009300170125
009400170125      // - Reperimento dati utente
009500170125     d TIBS34DS      e ds
009600170125
009700170125      //---------------------------------------------------------------
009800170125      //?Definizione variabili globali.
009900170125      //---------------------------------------------------------------
010000170125      // - Flags booleani
010100170125     d ErrGrave        s               n   inz(*off)
010200170125     d Fine            s               n   inz(*off)
010300170125     d wEoF            s               n   inz
010400170125     d wInzD01         s               n   inz(*on)
010500170125
010600170125      // - Campi per QCMDEXC
010700170125     d cmd             s            512a   varying
010800170125
010900170125      // - Campi associati al video
011000170125     d Video           s              2    inz('D1')
011100170125
011200170125      // - Campi di comodo data
011300170125     d Data_eur        s               d   Datfmt(*eur)
011400170125     d Data_iso        s               d   Datfmt(*iso)
011500170125     d Oggi            s              8  0 inz
011600170125
011700170125      // - Campi di comodo
011800170125     d wDataI          s              8  0 inz
011900170125     d wDataF          s              8  0 inz
012000170125
012100170125      //---------------------------------------------------------------
012200170125      //?Definizione Procedure usate.
012300170125      //---------------------------------------------------------------
012400170125      // - Sottomissione lavoro batch
012500170125     d bch10           pr                  extpgm('BCH10')
012600170125     d  kpjba                              likeds(KPJBA)
012700170125
012800170125      // - Richiamo diretto lavoro batch
012900170125     d TNTACBA1R       pr                  extpgm('TNTACBA1R')
013000170125     d  kpjba                              likeds(KPJBA)
013100170125
013200170125      //---------------------------------------------------------------
013300170125      //?Definizione Prototipi.
013400170125      //---------------------------------------------------------------
013500170125      /copy gaitrasrc/srcprotopr,SYSTEM
013600170125      /copy gaitrasrc/srcprotopr,TIBS34R
013700170125      /copy gaitrasrc/srcprotopr,XSRDA8
013800170125
013900170125      //---------------------------------------------------------------
014000170125      //?Definizione key-list.
014100170125      //---------------------------------------------------------------
014200170125
014300170125      //---------------------------------------------------------------
014400170125      //?M A I N - L I N E
014500170125      //---------------------------------------------------------------
014600170125     c     *Entry        plist
014700170125     c                   parm                    kpjba
014800170125
014900170125      /free
015000170125
015100170125       //?Operazioni iniziali
015200170125       exsr RoutInz;
015300170125
015400170125       //?Gestione video
015500170125       DOW  Fine = *off;
015600170125         SELECT;
015700170125         WHEN Video = 'D1';
015800170125           exsr GesD01;
015900170125         OTHER;
016000170125           Fine = *on;
016100170125         ENDSL;
016200170125       ENDDO;
016300170125
016400170125       //?Operazioni finali
016500170125       exsr RoutEnd;
016600170125
016700170125       //--------------------------------------------------------------
016800170125       //?Operazioni iniziali.
016900170125       //--------------------------------------------------------------
017000170125       BEGSR RoutInz;
017100170125
017200170125       //?Reperimento dati job
017300170125         exsr DatiJob;
017400170125
017500170125       //?Calcolo oggi
017600170125         Oggi = %dec(%date());
017700170125
017800170125       ENDSR;
017900170125
018000170125       //--------------------------------------------------------------
018100170125       //?Reperimento Dati del job (Utente/Operativi).
018200170125       //--------------------------------------------------------------
018300170125       BEGSR DatiJob;
018400170125
018500170125         in(E) §AzUte;
018600170125         IF  not %error;
018700170125           in(E) §DatiUte;
018800170125         ENDIF;
018900170125         IF  %error or RSut = *blanks;
019000170125           clear TIBS34ds;
019100170125           tibs34r(tibs34ds);
019200170125           in §AzUte;
019300170125           in §DatiUte;
019400170125         ENDIF;
019500170125
019600170125       ENDSR;
019700170125
019800170125       //--------------------------------------------------------------
019900170125       //?Gestione videata D01
020000170125       //--------------------------------------------------------------
020100170125       BEGSR GesD01;
020200170125
020300170125       //?Inizializzazione videata
020400170125         IF  wInzD01;
020500170125           exsr InzD01;
020600170125           wInzD01 = *off;
020700170125         ENDIF;
020800170125
020900170125       //?Emissione videata
021000170125         exfmt TACBA01;
021100170125         ErrMessage   = *off;
021200170125         ErrGenerico  = *off;
021300170125         clear V01msg;
021400170125
021500170125         SELECT;
021600170125
021700170125       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
021800170125         WHEN  ErrGrave;
021900170125           Fine = *on;
022000170125
022100170125       //?F3=Fine
022200170125         WHEN  dsp_aid = c_F03;
022300170125           exsr F03D01;
022400170125
022500170125       //?F6=Conferma
022600170125         WHEN  dsp_aid = c_F06;
022700170125           exsr CtrD01;
022800170125           IF  ErrGenerico;
022900170125             leavesr;
023000170125           ENDIF;
023100170125           exsr F06D01;
023200170125
023300170125       //?F8=Visualizza File WFCBA00F
023400170125         WHEN  dsp_aid = c_F08;
023500170125           exsr F08D01;
023600170125
023700170125       //?Enter
023800170125         OTHER;
023900170125           exsr CtrD01;
024000170125           IF  ErrGenerico;
024100170125             leavesr;
024200170125           ENDIF;
024300170125         ENDSL;
024400170125
024500170125       ENDSR;
024600170125
024700170125       //--------------------------------------------------------------
024800170125       //?Inizializzazione videata D01
024900170125       //--------------------------------------------------------------
025000170125       BEGSR InzD01;
025100170125
025200170125         clear TACBA01;
025300170125
025400170125       ENDSR;
025500170125
025600170125       //--------------------------------------------------------------
025700170125       //?Gestione tasto funzionale F3 da videata D01
025800170125       //?F3=Fine
025900170125       //--------------------------------------------------------------
026000170125       BEGSR F03D01;
026100170125
026200170125       //?Chiusura del programma
026300170125         Fine = *on;
026400170125
026500170125       ENDSR;
026600170125
026700170125       //--------------------------------------------------------------
026800170125       //?Gestione tasto funzionale F6 da videata D01
026900170125       //?F6=Conferma
027000170125       //--------------------------------------------------------------
027100170125       BEGSR F06D01;
027200170125
027300170125         clear kpjbu;
027400170125         clear tntacbads;
027500170125           ITA98car   = %dec(V01car:3:0);
027600170125         ENDIF;
027700170125         IF  V01cmm > *zeros;
027800170125           ITA98cmm   = %dec(V01cmm:7:0);
027900170125         ENDIF;
028000170125         ITA98elcmm = V01elcmm;
028100170125         ITA98dtavi = wdtavi;
028200170125         ITA98dtavf = wdtavf;
028300170125         ITA98dtchi = wdtchi;
028400170125         ITA98dtchf = wdtchf;
028500170125         ITA98dtica = wdtica;
028600170125       //?Imposto il valore traffico totale con il segno immesso a video
028700170125         IF  V01valttsi = '-';
028800170125           ITA98vltti = V01valtti * -1;
028900170125         ELSE;
029000170125           ITA98vltti = V01valtti;
029100170125         ENDIF;
029200170125         IF  V01valttsf = '-';
029300170125           ITA98vlttf = V01valttf * -1;
029400170125         ELSE;
029500170125           ITA98vlttf = V01valttf;
029600170125         ENDIF;
029700170125         ITA98valte = V01valte;
029800170125         ITA98pkg   = V01pkg;
029900170125         ITA98conc1 = V01conc1;
030000170125         ITA98conc2 = V01conc2;
030100170125         ITA98conc3 = V01conc3;
030200170125         ITA98conc4 = V01conc4;
030300170125         ITA98conc5 = V01conc5;
030400170125         ITA98delsi = V01delsi;
030500170125         ITA98deli  = V01deli;
030600170125         ITA98delsf = V01delsf;
030700170125         ITA98delf  = V01delf;
030800170125       //?Imposto il valore aumento/sconto con il segno immesso a video
030900170125         IF  V01auscsi = '-';
031000170125           ITA98ausi = V01ausci * -1;
031100170125         ELSE;
031200170125           ITA98ausi = V01ausci;
031300170125         ENDIF;
031400170125         IF  V01auscsf = '-';
031500170125           ITA98ausf = V01auscf * -1;
031600170125         ELSE;
031700170125           ITA98ausf = V01auscf;
031800170125         ENDIF;
031900170125         ITA98tpt1  = V01ttr1;
032000170125         ITA98tpt2  = V01ttr2;
032100170125         ITA98tpt3  = V01ttr3;
032200170125         ITA98tpt4  = V01ttr4;
032300170125         ITA98tpt5  = V01ttr5;
032400170125         ITA98tpt6  = V01ttr6;
032500170125         ITA98tic1  = V01tic1;
032600170125         ITA98tic2  = V01tic2;
032700170125         ITA98tic3  = V01tic3;
032800170125         ITA98tic4  = V01tic4;
032900170125         ITA98tic5  = V01tic5;
033000170125         ITA98calde = V01calde;
033100170125         ITA98dtaff = wdtaff;
033200170125         ITA98esi   = V01esi;
033300170125
033400170125         ITA98det   = V02det;
033500170125         ITA98detc  = V02detc;
033600170125         ITA98ttrio = V02ttrio;
033700170125         ITA98ttr1  = V02ttr1;
033800170125         ITA98ttr2  = V02ttr2;
033900170125         ITA98ttr3  = V02ttr3;
034000170125         ITA98ttr4  = V02ttr4;
034100170125         ITA98ttr5  = V02ttr5;
034200170125         ITA98ttr6  = V02ttr6;
034300170125         ITA98ord   = V02ord;
034400170125         ITA98cmmt  = V02cmmt;
034500170125         ITA98gent  = V02gent;
034600170125         kcoaz = 'TA98';
034700170125         kpjbu = TNTA98ds;
034800170125         kbuff = *blanks;
034900170125
035000170125       //?Richiamo interattivo
035100170125         IF  knmus = *all'1';
035200170125           TNTA98C(kpjba);
035300170125       //?Richiamo batch
035400170125         ELSE;
035500170125           BCH10(kpjba);
035600170125         ENDIF;
035700170125
035800170125       //?Chiusura del programma
035900170125         $Fine = *on;
036000170125
036100170125       ENDSR;
036200170125
036300170125       //--------------------------------------------------------------
036400170125       //?Gestione tasto funzionale F8 da videata D01
036500170125       //?F8=Visualizza File
036600170125       //--------------------------------------------------------------
036700170125       BEGSR F08D01;
036800170125           cmd = CmdOvrPrtF
036900170125               + ' outq(' + %trim(§MRAdoutqi) + ')'
037000170125               + ' usrdfndta(''' + TRTCM1ds + ''')';
037100170125         ELSE;
037200170125           cmd = CmdOvrPrtF;
037300170125         ENDIF;
037400170125
037500170125         ExecuteCommand (cmd);
037600170125
037700170125         // -?Visualizzo il file?
037800170125         Qcmd = 'DSPDBF FILE(';
037900170125         IF  %subst(knsif : 7 : 1) = 'P';
038000170125           Qcmd = %trimr(Qcmd) + 'GAITRAAZP';
038100170125         ELSE;
038200170125           Qcmd = %trimr(Qcmd) + 'GAITRAAZM';
038300170125         ENDIF;
038400170125         Qcmd = %trimr(Qcmd) + '/WFATT00F) mbr(*first) Output(*)';
038500170125         callp(e) ExecuteCommand ( Qcmd : %size(Qcmd));
038600170125
038700170125       ENDSR;
038800170125
038900170125       //--------------------------------------------------------------
039000170125       //?Controllo videata D01
039100170125       //--------------------------------------------------------------
039200170125       BEGSR CtrD01;
039300170125
039400170125         clear V01dlna;
039500170125
039600170125         clear wDataF;
039700170125         clear wDataI;
039800170125
039900170125         WindDspF = IndDspF;
040000170125         reset WindDspFb;
040100170125         IndDspF  = WindDspF;
040200170125
040300170125       //?Periodo elaborazione
040400170125         IF  V01datai = 0 and V01dtavf = 0;
040500170125           ErrMessage  = *on;
040600170125           ErrGenerico = *on;
040700170125           PosCurDataI = *on;
040800170125           V01msg      = Msg(01);
040900170125           leavesr;
041000170125         ENDIF;
041100170125
041200170125       //?Periodo "dal"
041300170125         IF  V01datai <> 0;
041400170125           clear wlbdat;
041500170125           G02dat = V01datai;
041600170125           xsrda8(wlbdat);
041700170125           IF  G02err = '1';
041800170125             ErrMessage  = *on;
041900170125             ErrGenerico = *on;
042000170125             PosCurDataI = *on;
042100170125             V01msg      = Msg(01);
042200170125             leavesr;
042300170125           ENDIF;
042400170125
042500170125           V01datai = G02dat;
042600170125           wDataI   = G02inv;
042700170125         ENDIF;
042800170125
042900170125       //?Periodo "al"
043000170125         IF  V01dataf <> 0;
043100170125           clear wlbdat;
043200170125           G02dat = V01dataf;
043300170125           xsrda8(wlbdat);
043400170125           IF  G02err = '1';
043500170125             ErrMessage  = *on;
043600170125             ErrGenerico = *on;
043700170125             PosCurDataF = *on;
043800170125             V01msg      = Msg(01);
043900170125             leavesr;
044000170125           ENDIF;
044100170125
044200170125           V01dataf = G02dat;
044300170125           wDataF   = G02inv;
044400170125         ENDIF;
044500170125
044600170125       //?"DAL" congruente con "AL"
044700170125         IF  wDataI > 0 and wDataF = 0;
044800170125           ErrMessage  = *on;
044900170125           ErrGenerico = *on;
045000170125           PosCurDtavi = *on;
045100170125           V01msg      = Msg(02);
045200170125           leavesr;
045300170125         ENDIF;
045400170125         IF  wDataI > 0 and wDataF > 0 and wDataI > wDataF;
045500170125           ErrMessage  = *on;
045600170125           ErrGenerico = *on;
045700170125           PosCurDataF = *on;
045800170125           V01msg      = Msg(02);
045900170125           leavesr;
046000170125         ENDIF;
046100170125
046200170125       //?Linea di arrivo
046300170125         IF  V01lna = 0;
046400170125           ErrMessage  = *on;
046500170125           ErrGenerico = *on;
046600170125           PosCurLNA   = *on;
046700170125           V01msg      = Msg(03);
046800170125           leavesr;
046900170125         ENDIF;
047000170125
047100170125         chain V01lna AZORG01L;
047200170125         IF  not %found(AZORG01L) or
047300170125             ORGfva <> *blanks or
047400170125             (ORGfag <> 'F' and ORGfag <> 'A');
047500170125           ErrMessage  = *on;
047600170125           ErrGenerico = *on;
047700170125           PosCurLNA   = *on;
047800170125           V01msg      = Msg(03);
047900170125           leavesr;
048000170125         ENDIF;
048100170125         V01dlna = ORGdes;
048200170125
048300170125       ENDSR;
048400170125
048500170125       //--------------------------------------------------------------
048600170125       //?Operazioni finali.
048700170125       //--------------------------------------------------------------
048800170125       BEGSR RoutEnd;
048900170125
049000170125         *inLR = *on;
049100170125         return;
049200170125
049300170125       ENDSR;
049400170125
049500170125      /end-free
049600170125
049700170125** - MSG  ---------------------------------------------*
049800170125Data Errata                                             01
049900170125Data "DAL" incongruente con data "AL"                   02
050000170125Data Linea arrivo errata                                03
