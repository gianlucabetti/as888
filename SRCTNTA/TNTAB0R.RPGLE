000100160915      //--------------------------------------------------------------
000200160915      //?TNTAB0R - Duplica tariffe DPD
000300160915      //--------------------------------------------------------------
000400160915     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000500160915
000600160915      //---------------------------------------------------------------
000700160915      //?Dichiarazione file.
000800160915      //---------------------------------------------------------------
000900160915      // - Anagrafica Cliente
001000160915     fCNACO00F  if   e           k disk
001100160915
001200160915      // - Tabelle
001300160915     fTABEL00F  if   e           k disk
001400160915
001500160915      // - Tariffe
001600160915     fTNTAM01L  if   e           k disk
001700160915
001800160915      // - Video
001900160915     fTNTAB0D   cf   e             workstn
002000160915     f                                     indds(IndDspF)
002100160915     f                                     infds(InfDspF)
002200160915
002300160915      //---------------------------------------------------------------
002400160915      //?Definizione costanti.
002500160915      //---------------------------------------------------------------
002600160915      // - Tasti funzionali a video
002700160915     d c_F01           c                   const(x'31')
002800160915     d c_F02           c                   const(x'32')
002900160915     d c_F03           c                   const(x'33')
003000160915     d c_F04           c                   const(x'34')
003100160915     d c_F05           c                   const(x'35')
003200160915     d c_F06           c                   const(x'36')
003300160915     d c_F07           c                   const(x'37')
003400160915     d c_F08           c                   const(x'38')
003500160915     d c_F09           c                   const(x'39')
003600160915     d c_F10           c                   const(x'3A')
003700160915     d c_F11           c                   const(x'3B')
003800160915     d c_F12           c                   const(x'3C')
003900160915     d c_F13           c                   const(x'B1')
004000160915     d c_F14           c                   const(x'B2')
004100160915     d c_F15           c                   const(x'B3')
004200160915     d c_F16           c                   const(x'B4')
004300160915     d c_F17           c                   const(x'B5')
004400160915     d c_F18           c                   const(x'B6')
004500160915     d c_F19           c                   const(x'B7')
004600160915     d c_F20           c                   const(x'B8')
004700160915     d c_F21           c                   const(x'B9')
004800160915     d c_F22           c                   const(x'BA')
004900160915     d c_F23           c                   const(x'BB')
005000160915     d c_F24           c                   const(x'BC')
005100160915     d c_Enter         c                   const(x'F1')
005200160915     d c_RollDown      c                   const(x'F4')
005300160915     d c_RollUp        c                   const(x'F5')
005400160915
005500160915     d Digits          c                   const('0123456789')
005600160915
005700160915      //---------------------------------------------------------------
005800160915      //?Definizione schiere.
005900160915      //---------------------------------------------------------------
006000160915      // - Messaggi di errore
006100160915     d Msg             s             78    dim(30) ctdata perrcd(1)
006200160915
006300160915      //---------------------------------------------------------------
006400160915      //?Definizione aree dati.
006500160915      //---------------------------------------------------------------
006600160915      // - Dati utente
006700160915     d §AzUte        e ds                  extname(AZUTE00F)
006800160915     d                                     dtaara
006900160915     d §DatiUte      e ds                  extname(dDatiUte)
007000160915     d                                     dtaara
007100160915
007200160915      //---------------------------------------------------------------
007300160915      //?Definizione strutture dati.
007400160915      //---------------------------------------------------------------
007500160915      // - Status
007600160915     d Psds           sds
007700160915     d   SDSpgm          *proc
007800160915
007900160915      // - InfDS
008000160915     d InfDspF         ds
008100160915     d  dsp_aid              369    369a
008200160915
008300160915      // - Indicatori su DspF
008400160915     d IndDspF         ds
008500160915        // - Indicatori di errore
008600160915     d  ErrMessage                    1n   overlay(IndDspF : 28)
008700160915     d  PosCurKsc                     1n   overlay(IndDspF : 50)
008800160915     d  PosCurCtr                     1n   overlay(IndDspF : 51)
008900160915     d  PosCurPrg                     1n   overlay(IndDspF : 52)
009000160916     d  PosCurKsc1                    1n   overlay(IndDspF : 53)
009100160916     d  PosCurKsc2                    1n   overlay(IndDspF : 54)
009200160916     d  PosCurKsc3                    1n   overlay(IndDspF : 55)
009300160916     d  PosCurKsc4                    1n   overlay(IndDspF : 56)
009400160916     d  PosCurKsc5                    1n   overlay(IndDspF : 57)
009500160916     d  PosCurKsc6                    1n   overlay(IndDspF : 58)
009600160916     d  PosCurKsc7                    1n   overlay(IndDspF : 59)
009700160916     d  PosCurKsc8                    1n   overlay(IndDspF : 60)
009800160916     d  PosCurKsc9                    1n   overlay(IndDspF : 61)
009900160916     d  PosCurKsc10                   1n   overlay(IndDspF : 62)
010000160915        // - Indicatori di errore generico
010100160915     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010200160915
010300160915     d WindDspF        ds                  inz
010400160915     d   WindDspFa             1     49    inz(*zero)
010500160915     d   WindDspFb            50     99    inz(*zero)
010600160915
010700160915      // - Parametri ricevuti
010800160915     d KPJBA         e ds
010900160915
011000160915      // - Reperimento dati utente
011100160915     d TIBS34ds      e ds
011200160915
011300160915      // - Passaggio dati a pgm stampa
011400160915     d TNTAB0DS      e ds
011500160915
011600160915      // - Tabella TR = Tipo Tariffa
011700160915     d dsTR          e ds                  inz
011800160915
011900160915      // - Campo TAMflo
012000160915     d dsTA01        e ds                  inz
012100160915
012200160915      //---------------------------------------------------------------
012300160915      //?Definizione variabili globali.
012400160915      //---------------------------------------------------------------
012500160915      // - Flags booleani
012600160915     d ErrGrave        s               n   inz(*off)
012700160915     d Fine            s               n   inz(*off)
012800160915     d wInzD01         s               n   inz(*on)
012900160915
013000160915      // - Campi associati al video
013100160915     d wVideo          s              2    inz('D1')
013200160915
013300160915      // - Indici di schiera
013400160915     d xx              s              4  0 inz
013500160915
013600160915      // - Campi di comodo data
013700160916     d Data_ISO        s               d   datfmt(*iso)
013800160916     d Oggi            s              8s 0 inz
013900160915
014000160915      // - Campi di comodo
014100160916     d Annoincorso     s              4s 0 inz
014200160916     d Annotariffa     s              4s 0 inz
014300160916     d Meseincorso     s              2s 0 inz
014400160916     d Mesetariffa     s              2s 0 inz
014500160916     d wksc            s                   like(ACOksc)
014600160915
014700160915      //---------------------------------------------------------------
014800160915      //?Definizione procedure usate.
014900160915      //---------------------------------------------------------------
015000160915      // - Sottomissione lavoro batch
015100160915     d bch10           pr                  extpgm('BCH10')
015200160915     d  kpjba                              likeds(KPJBA)
015300160915
015400160915      // - Richiamo diretto lavoro batch
015500160915     d tntab1r         pr                  extpgm('TNTAB1R')
015600160915     d  kpjba                              likeds(KPJBA)
015700160915
015800160915      //---------------------------------------------------------------
015900160915      //?Definizione prototipi usati.
016000160915      //---------------------------------------------------------------
016100160915      /copy gaitrasrc/srcprotopr,tibs34r
016200160915
016300160915      //---------------------------------------------------------------
016400160915      //?Definizione key-list.
016500160915      //---------------------------------------------------------------
016600160916      // - File TABEL00F
016700160915     d k03tabel      e ds                  extname(TABEL00F:*key)
016800160915     d                                     prefix(k_)
016900160915
017000160915      //---------------------------------------------------------------
017100160915      //?Riepilogo indicatori.
017200160915      //---------------------------------------------------------------
017300160915
017400160915      //---------------------------------------------------------------
017500160915      //?M A I N - L I N E
017600160915      //---------------------------------------------------------------
017700160915     c     *Entry        plist
017800160915     c                   parm                    KPJBA
017900160915
018000160915      /free
018100160915
018200160915       //?Operazioni iniziali
018300160915       exsr RoutInz;
018400160915
018500160915       //?Gestione video
018600160915       DOW  Fine = *off;
018700160915         SELECT;
018800160915           WHEN wVideo = 'D1';
018900160915             exsr GesD01;
019000160915           OTHER;
019100160915             Fine = *on;
019200160915         ENDSL;
019300160915       ENDDO;
019400160915
019500160915       //?Operazioni finali
019600160915       exsr RoutEnd;
019700160915
019800160915       //--------------------------------------------------------------
019900160915       //?Operazioni iniziali.
020000160915       //--------------------------------------------------------------
020100160915       BEGSR RoutInz;
020200160915
020300160915       //?Reperimento dati job
020400160915         exsr DatiJob;
020500160915
020600160915       //?Calcolo oggi
020700160916         Oggi = %dec(%date());
020800160916         Data_ISO = %date(Oggi);
020900160916         AnnoInCorso = %subdt(Data_ISO:*years);
021000160916         MeseInCorso = %subdt(Data_ISO:*months);
021100160915
021200160915       ENDSR;
021300160915
021400160915       //--------------------------------------------------------------
021500160915       //?Reperimento Dati del job (Utente/Operativi).
021600160915       //--------------------------------------------------------------
021700160915       BEGSR DatiJob;
021800160915
021900160915         in(E) §AzUte;
022000160915         IF  NOT %error;
022100160915           in(E) §DatiUte;
022200160915         ENDIF;
022300160915         IF  %error or Rsut = *blanks;
022400160915           clear TIBS34ds;
022500160915           tibs34r(tibs34ds);
022600160915           in §AzUte;
022700160915           in §DatiUte;
022800160915         ENDIF;
022900160915
023000160915       ENDSR;
023100160915
023200160915       //--------------------------------------------------------------
023300160915       //?Gestione videata D01
023400160915       //--------------------------------------------------------------
023500160915       BEGSR GesD01;
023600160915
023700160915       //?Inizializzazione videata
023800160915         IF  wInzD01;
023900160915           exsr InzD01;
024000160915           wInzD01 = *off;
024100160915         ENDIF;
024200160915
024300160915       //?Emissione videata
024400160915         exfmt TAB0D01;
024500160915         ErrMessage  = *off;
024600160915         ErrGenerico = *off;
024700160915         clear V01msg;
024800160915
024900160915         SELECT;
025000160915
025100160915       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
025200160915           WHEN  ErrGrave;
025300160915             Fine = *on;
025400160915
025500160915       //?F3=Fine
025600160915           WHEN  dsp_aid = c_F03;
025700160915             exsr F03D01;
025800160915
025900160915       //?F6=Conferma
026000160915           WHEN  dsp_aid = c_F06;
026100160915             exsr CtrD01;
026200160915             IF  ErrGenerico;
026300160915               leavesr;
026400160915             ENDIF;
026500160915             exsr F06D01;
026600160915
026700160915       //?Enter
026800160915           OTHER;
026900160915             exsr CtrD01;
027000160915             IF  ErrGenerico;
027100160915               leavesr;
027200160915             ENDIF;
027300160915         ENDSL;
027400160915
027500160915       ENDSR;
027600160915
027700160915       //--------------------------------------------------------------
027800160915       //?Inizializzazione videata D01
027900160915       //--------------------------------------------------------------
028000160915       BEGSR InzD01;
028100160915
028200160915         clear TAB0D01;
028300160915         V01pgm = SDSpgm;
028400160915         V01sut = rsut;
028500160915         V01sif = knsif;
028600160915         V01mus = knmus;
028700160915
028800160915       ENDSR;
028900160915
029000160915       //--------------------------------------------------------------
029100160915       //?Gestione tasto funzionale F3 da videata D01
029200160915       //?F3=Fine
029300160915       //--------------------------------------------------------------
029400160915       BEGSR F03D01;
029500160915
029600160915       //?Chiusura del programma
029700160915         Fine = *on;
029800160915
029900160915       ENDSR;
030000160915
030100160915       //--------------------------------------------------------------
030200160915       //?Gestione tasto funzionale F6 da videata D01
030300160915       //?F6=Conferma
030400160915       //--------------------------------------------------------------
030500160915       BEGSR F06D01;
030600160915
030700160915         clear TNTAB0DS;
030800160916         TAB0ksc = V01ksc;
030900160916         TAB0ctr = %dec(V01ctr:3:0);
031000160916         TAB0prg = %dec(V01prg:3:0);
031100160916         TAB0new = V01new;
031200160916         TAB0ksc1 = V01ksc1;
031300160916         TAB0ksc2 = V01ksc2;
031400160916         TAB0ksc3 = V01ksc3;
031500160916         TAB0ksc4 = V01ksc4;
031600160916         TAB0ksc5 = V01ksc5;
031700160916         TAB0ksc6 = V01ksc6;
031800160916         TAB0ksc7 = V01ksc7;
031900160916         TAB0ksc8 = V01ksc8;
032000160916         TAB0ksc9 = V01ksc9;
032100160916         TAB0ksc10 = V01ksc10;
032200160916         TAB0tamddt = TAMddt;
032300160916         Data_ISO = %date(TAMddt);
032400160916         Data_ISO = Data_ISO - %days(1);
032500160916         TAB0tamdst = %dec(Data_ISO);
032600160915         kcoaz = 'TAB1';
032700160915         kpjbu = TNTAB0DS;
032800160915         kbuff = *blanks;
032900160915
033000160915       //?Richiamo interattivo
033100160915         IF  knmus = *all'1';
033200160916           TNTAB1R(kpjba);
033300160915       //?Richiamo batch
033400160915         ELSE;
033500160915           BCH10(kpjba);
033600160915         ENDIF;
033700160915
033800160915       //?Chiusura del programma
033900160915         Fine = *on;
034000160915
034100160915       ENDSR;
034200160915
034300160915       //--------------------------------------------------------------
034400160915       //?Controllo videata D01
034500160915       //--------------------------------------------------------------
034600160915       BEGSR CtrD01;
034700160915
034800160915         clear V01dksc;
034900160915
035000160915         WindDspF = IndDspF;
035100160915         reset WindDspFb;
035200160915         IndDspF  = WindDspF;
035300160915
035400160915       //?Cliente
035500160915         IF  V01ksc = 0;
035600160915           ErrMessage  = *on;
035700160915           ErrGenerico = *on;
035800160915           PosCurKsc   = *on;
035900160915           V01msg = Msg(01);
036000160915           leavesr;
036100160915         ENDIF;
036200160916         wksc = V01ksc;
036300160916         exsr CtrKSC;
036400160916         IF  ErrMessage;
036500160916           PosCurKsc = *on;
036600160915           leavesr;
036700160915         ENDIF;
036800160915         V01dksc = ACOrag;
036900160915
037000160915       //?Tariffa
037100160915         IF  V01ctr = *blanks;
037200160915           ErrMessage  = *on;
037300160915           ErrGenerico = *on;
037400160915           PosCurCtr   = *on;
037500160915           V01msg = Msg(02);
037600160915           leavesr;
037700160915         ENDIF;
037800160915         IF  %check(Digits:V01ctr) > *zeros;
037900160915           ErrMessage  = *on;
038000160915           ErrGenerico = *on;
038100160915           PosCurCtr   = *on;
038200160915           V01msg = Msg(02);
038300160915           leavesr;
038400160915         ENDIF;
038500160916         k_TBLkut = 1;
038600160915         k_TBLcod = 'TR';
038700160915         k_TBLkey = %subst(V01ctr:1:1);
038800160915         chain  %kds(K03tabel) TABEL00F;
038900160915         IF  NOT %found(TABEL00F) or
039000160915             TBLflg <> *blank;
039100160915           ErrMessage  = *on;
039200160915           ErrGenerico = *on;
039300160915           PosCurCtr   = *on;
039400160915           V01msg = Msg(02);
039500160915           leavesr;
039600160915         ENDIF;
039700160915         dsTR = TBLuni;
039800160915         IF  §TRutc <> 'S';
039900160915           ErrMessage  = *on;
040000160915           ErrGenerico = *on;
040100160915           PosCurCtr   = *on;
040200160915           V01msg = Msg(02);
040300160915           leavesr;
040400160915         ENDIF;
040500160915
040600160915       //?Progressivo
040700160915         IF  V01prg = *blanks;
040800160915           ErrMessage  = *on;
040900160915           ErrGenerico = *on;
041000160915           PosCurPrg   = *on;
041100160915           V01msg = Msg(03);
041200160915           leavesr;
041300160915         ENDIF;
041400160915         IF  %check(Digits:V01prg) > *zeros;
041500160915           ErrMessage  = *on;
041600160915           ErrGenerico = *on;
041700160915           PosCurPrg   = *on;
041800160915           V01msg = Msg(03);
041900160915           leavesr;
042000160915         ENDIF;
042100160915
042200160915       //?Deve esistere la tariffa
042300160915         TAMksc = V01ksc;
042400160915         TAMctr = %dec(V01ctr:3:0);
042500160915         TAMprg = %dec(V01prg:3:0);
042600160915         chain (TAMksc:TAMctr:TAMprg) TNTAM01L;
042700160915         IF  not %found(TNTAM01L) or TAMatb <> *blanks;
042800160915           ErrMessage  = *on;
042900160915           ErrGenerico = *on;
043000160915           PosCurKsc   = *on;
043100160915           V01msg = Msg(04);
043200160915           leavesr;
043300160915         ENDIF;
043400160915         dsTA01 = TAMflo;
043500160916       //?Deve essere una tariffa DPD
043600160915         IF  §TAdpd <> 'S';
043700160915           ErrMessage  = *on;
043800160915           ErrGenerico = *on;
043900160915           PosCurKsc   = *on;
044000160915           V01msg = Msg(04);
044100160915           leavesr;
044200160915         ENDIF;
044300160916       //?La data decorrenza della tariffa deve essere del mese in corso
044400160916         Data_ISO = %date(TAMddt);
044500160916         AnnoTariffa = %subdt(Data_ISO:*years);
044600160916         MeseTariffa = %subdt(Data_ISO:*months);
044700160916         IF  AnnoTariffa < AnnoInCorso;
044800160916           ErrMessage  = *on;
044900160916           ErrGenerico = *on;
045000160916           PosCurKsc   = *on;
045100160916           V01msg = Msg(05);
045200160916           leavesr;
045300160916         ENDIF;
045400160916         IF  AnnoTariffa = AnnoInCorso and MeseTariffa < MeseInCorso;
045500160916           ErrMessage  = *on;
045600160916           ErrGenerico = *on;
045700160916           PosCurKsc   = *on;
045800160916           V01msg = Msg(05);
045900160916           leavesr;
046000160916         ENDIF;
046100160916
046200160916       //?Controllo i clienti da escludere
046300160916         IF  V01ksc1 <> *zeros;
046400160916           wksc = V01ksc1;
046500160916           exsr CtrKSC;
046600160916           IF  ErrMessage;
046700160916             PosCurKsc1 = *on;
046800160916             leavesr;
046900160916           ENDIF;
047000160916         ENDIF;
047100160916         IF  V01ksc2 <> *zeros;
047200160916           wksc = V01ksc2;
047300160916           exsr CtrKSC;
047400160916           IF  ErrMessage;
047500160916             PosCurKsc2 = *on;
047600160916             leavesr;
047700160916           ENDIF;
047800160916         ENDIF;
047900160916         IF  V01ksc3 <> *zeros;
048000160916           wksc = V01ksc3;
048100160916           exsr CtrKSC;
048200160916           IF  ErrMessage;
048300160916             PosCurKsc3 = *on;
048400160916             leavesr;
048500160916           ENDIF;
048600160916         ENDIF;
048700160916         IF  V01ksc4 <> *zeros;
048800160916           wksc = V01ksc4;
048900160916           exsr CtrKSC;
049000160916           IF  ErrMessage;
049100160916             PosCurKsc4 = *on;
049200160916             leavesr;
049300160916           ENDIF;
049400160916         ENDIF;
049500160916         IF  V01ksc5 <> *zeros;
049600160916           wksc = V01ksc5;
049700160916           exsr CtrKSC;
049800160916           IF  ErrMessage;
049900160916             PosCurKsc5 = *on;
050000160916             leavesr;
050100160916           ENDIF;
050200160916         ENDIF;
050300160916         IF  V01ksc6 <> *zeros;
050400160916           wksc = V01ksc6;
050500160916           exsr CtrKSC;
050600160916           IF  ErrMessage;
050700160916             PosCurKsc6 = *on;
050800160916             leavesr;
050900160916           ENDIF;
051000160916         ENDIF;
051100160916         IF  V01ksc7 <> *zeros;
051200160916           wksc = V01ksc7;
051300160916           exsr CtrKSC;
051400160916           IF  ErrMessage;
051500160916             PosCurKsc7 = *on;
051600160916             leavesr;
051700160916           ENDIF;
051800160916         ENDIF;
051900160916         IF  V01ksc8 <> *zeros;
052000160916           wksc = V01ksc8;
052100160916           exsr CtrKSC;
052200160916           IF  ErrMessage;
052300160916             PosCurKsc8 = *on;
052400160916             leavesr;
052500160916           ENDIF;
052600160916         ENDIF;
052700160916         IF  V01ksc9 <> *zeros;
052800160916           wksc = V01ksc9;
052900160916           exsr CtrKSC;
053000160916           IF  ErrMessage;
053100160916             PosCurKsc9 = *on;
053200160916             leavesr;
053300160916           ENDIF;
053400160916         ENDIF;
053500160916         IF  V01ksc10 <> *zeros;
053600160916           wksc = V01ksc10;
053700160916           exsr CtrKSC;
053800160916           IF  ErrMessage;
053900160916             PosCurKsc10 = *on;
054000160916             leavesr;
054100160916           ENDIF;
054200160916         ENDIF;
054300160916
054400160916       ENDSR;
054500160916
054600160916       //--------------------------------------------------------------
054700160916       //?Aggancio CNACO.
054800160916       //--------------------------------------------------------------
054900160916       BEGSR CtrKSC;
055000160916
055100160916         chain (1:DUTkci:wksc) CNACO00F;
055200160916         IF  not %found(CNACO00F);
055300160916           ErrMessage  = *on;
055400160916           ErrGenerico = *on;
055500160916           V01msg = Msg(01);
055600160916         ENDIF;
055700160915
055800160915       ENDSR;
055900160915
056000160915       //--------------------------------------------------------------
056100160915       //?Operazioni finali.
056200160915       //--------------------------------------------------------------
056300160915       BEGSR RoutEnd;
056400160915
056500160915         *inLR = *on;
056600160915         return;
056700160915
056800160915       ENDSR;
056900160915
057000160915      /end-free
057100160915
057200160915       //--------------------------------------------------------------
057300160915       //?Schiere a tempo di compilazione.
057400160915       //--------------------------------------------------------------
057500160915** - MSG --------------------------------------------------------------------*
057600160915Codice cliente errato                                                         01
057700160915Codice tariffa errato                                                         02
057800160915Progressivo errato                                                            03
057900160915Tariffe errata o no DPD                                                       04
058000160916La tariffa non deve avere la decorrenza inferiore al mese in corso            05
