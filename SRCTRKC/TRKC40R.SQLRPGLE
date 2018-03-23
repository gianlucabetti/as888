000100141223      //--------------------------------------------------------------
000200150112      //?TRKC40R - REPORT CAMPAGNE COMMERCIALI
000300141223      //--------------------------------------------------------------
000400141223
000500141223     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600141223     h dftactgrp(*no) actgrp(*caller)
000700141223
000800141223      //---------------------------------------------------------------
000900141223      //?Dichiarazione file.
001000141223      //---------------------------------------------------------------
001100141223
001200141223      // - Organigramma
001300150123     fAZORG02L  if   e           k disk
001400141223
001500141223      // - Anagrafica Commerciali
001600141223     fAZCMM01L  if   e           k disk
001700141223
001800141223      // - Tabelle
001900141223     fTABEL00F  if   e           k disk
002000141223
002100141223      // - File anagrafiche Campagne
002200141223     fTICMP01L  if   e           k disk
002300141223
002400141223      // - Video Gestione Campagne
002500150112     fTRKC40D   cf   e             workstn
002600141223     f                                     indds(IndDspF)
002700141223     f                                     infds(InfDspF)
002800141223
002900141223      //---------------------------------------------------------------
003000141223      //?Definizione costanti.
003100141223      //---------------------------------------------------------------
003200141223
003300141223      // - Tasti funzionali a video
003400141223     d c_F01           c                   const(x'31')
003500141223     d c_F02           c                   const(x'32')
003600141223     d c_F03           c                   const(x'33')
003700141223     d c_F04           c                   const(x'34')
003800141223     d c_F05           c                   const(x'35')
003900141223     d c_F06           c                   const(x'36')
004000141223     d c_F07           c                   const(x'37')
004100141223     d c_F08           c                   const(x'38')
004200141223     d c_F09           c                   const(x'39')
004300141223     d c_F10           c                   const(x'3A')
004400141223     d c_F11           c                   const(x'3B')
004500141223     d c_F12           c                   const(x'3C')
004600141223     d c_F13           c                   const(x'B1')
004700141223     d c_F14           c                   const(x'B2')
004800141223     d c_F15           c                   const(x'B3')
004900141223     d c_F16           c                   const(x'B4')
005000141223     d c_F17           c                   const(x'B5')
005100141223     d c_F18           c                   const(x'B6')
005200141223     d c_F19           c                   const(x'B7')
005300141223     d c_F20           c                   const(x'B8')
005400141223     d c_F21           c                   const(x'B9')
005500141223     d c_F22           c                   const(x'BA')
005600141223     d c_F23           c                   const(x'BB')
005700141223     d c_F24           c                   const(x'BC')
005800141223     d c_Enter         c                   const(x'F1')
005900141223     d c_RollDown      c                   const(x'F4')
006000141223     d c_RollUp        c                   const(x'F5')
006100141223
006200141223     d Digits          c                   const('0123456789')
006300150123
006400150123     d TotAzienda      c                   const('Azienda')
006500150123     d TotDistretto    c                   const('Distretto')
006600150123     d TotArea         c                   const('Area')
006700150123     d TotCommerciale  c                   const('Commerciale')
006800141223
006900141223      //---------------------------------------------------------------
007000141223      //?Definizione schiere.
007100141223      //---------------------------------------------------------------
007200141223
007300141223      // - Sk Aree
007400141223     d skAree          s              3a   dim(999) inz
007500141223     d skDesArea       s                   like(ORGdes) dim(999) inz
007600141223
007700141223      // - Sk Distretti
007800141223     d skDistretti     s                   like(ORGfl3) dim(99) inz
007900141223     d skDesDist       s                   like(§17des) dim(99) inz
008000141223
008100141223      // - Sk Cod.Importanza clienti da tabella IC
008200141223     d skIC            s              1    dim(10)
008300141223     d skIC_ord        s              1  0 dim(10)
008400141223
008500141223      // - Messaggi di errore
008600141223     d Msg             s             78    dim(30) ctdata perrcd(1)
008700141223
008800141223      //---------------------------------------------------------------
008900141223      //?Definizione aree dati.
009000141223      //---------------------------------------------------------------
009100141223
009200141223      // - Dati utente
009300141223     d §AzUte        e ds                  extname(AZUTE00F)
009400141223     d                                     dtaara
009500141223     d §DatiUte      e ds                  extname(dDatiUte)
009600141223     d                                     dtaara
009700141223
009800141223      //---------------------------------------------------------------
009900141223      //?Definizione strutture dati.
010000141223      //---------------------------------------------------------------
010100141223
010200141223      // - Status
010300141223     d Psds           sds
010400141223     d   SDSpgm          *proc
010500141223
010600141223      // - InfDS
010700141223     d InfDspF         ds
010800141223     d  dsp_aid              369    369a
010900141223     d  sfl_rrn              376    377i 0
011000141223     d  min_nrr              378    379i 0
011100141223     d  num_rcds             380    381i 0
011200141223
011300141223      // - Indicatori su DspF
011400141223     d IndDspF         ds
011500141223        // - Indicatori di Visualizzazione Campi
011600150223     d  VisFile                       1n   overlay(IndDspF : 25)
011700150123     d  VisArea                       1n   overlay(IndDspF : 26)
011800150123     d  VisDistretto                  1n   overlay(IndDspF : 27)
011900141223        // - Indicatori di errore in videata
012000141223     d  ErrMessage                    1n   overlay(IndDspF : 28)
012100150123        // - Indicatori di Visualizzazione Campi
012200150123     d  VisTOT1                       1n   overlay(IndDspF : 40)
012300150123     d  VisTOT2                       1n   overlay(IndDspF : 41)
012400150123     d  VisTOT3                       1n   overlay(IndDspF : 42)
012500150123     d  VisTOT4                       1n   overlay(IndDspF : 43)
012600141223        // - Indicatori di errore
012700141223     d  PosCurNCM                     1n   overlay(IndDspF : 50)
012800141223     d  PosCurCDI                     1n   overlay(IndDspF : 51)
012900141223     d  PosCurCAR                     1n   overlay(IndDspF : 52)
013000150123     d  PosCurCMM                     1n   overlay(IndDspF : 53)
013100150123     d  PosCurCLV1                    1n   overlay(IndDspF : 54)
013200150123     d  PosCurCLV2                    1n   overlay(IndDspF : 55)
013300150123     d  PosCurCLV3                    1n   overlay(IndDspF : 56)
013400150123     d  PosCurCLV4                    1n   overlay(IndDspF : 57)
013500150123     d  PosCurCLV5                    1n   overlay(IndDspF : 58)
013600150123     d  PosCurTOT1                    1n   overlay(IndDspF : 59)
013700150608     d  PosCurTOT2                    1n   overlay(IndDspF : 60)
013800150608     d  PosCurTOT3                    1n   overlay(IndDspF : 61)
013900141223        // - Indicatori di errore generico
014000141223     d  ErrGenerico                   1n   overlay(IndDspF : 99)
014100141223
014200141223     d WindDspF        ds                  inz
014300141223     d  WindDspFa              1     49    inz(*zero)
014400141223     d  WindDspFb             50     99    inz(*zero)
014500141223
014600141223      // - Parametri ricevuti
014700141223     d KPJBA         e ds
014800141223
014900150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
015000150112     d TRKC02DS      e ds
015100141223
015200150112      // - Richiamo pgm TRKC41C - Stampa Report
015300150112     d TRKC41DS      e ds
015400141223
015500141223      // - Reperimento dati utente
015600141223     d TIBS34DS      e ds
015700141223
015800141223      // - Controllo abilitazioni
015900141223     d TNTAA1DS      e ds                  inz
016000141223
016100141223      // - Reperimento filiali gestite dall'utente
016200141223     d TRUL31DS      e ds
016300141223     d  POG                   10    759    DIM(250)
016400141223     d TRUL31DS2     e ds
016500141223     d  DIS                    1     20    DIM(20)
016600141223     d  CAR                   22    171    DIM(50)
016700141223
016800141223      // - Tabella 05 - Aree
016900141223     d ds05          e ds
017000141223
017100141223      // - Tabella 17 - Distretti
017200141223     d ds17          e ds
017300141223
017400141223      // - Tabella IC - Importanza Clienti
017500141223     d dsIC          e ds
017600141223
017700141223      //---------------------------------------------------------------
017800141223      //?Definizione variabili globali.
017900141223      //---------------------------------------------------------------
018000141223
018100141223      // - Flags booleani
018200141223     d ErrGrave        s               n   inz(*off)
018300141223     d Fine            s               n   inz(*off)
018400141029     d wInzD02         s               n   inz(*off)
018500141223
018600141223      // - Indici di schiera
018700141223     d xx              s              4s 0 inz
018800141223
018900141223      // - Campi associati al video
019000141223     d Video           s              2a   inz('D2')
019100141223
019200141223      // - Campi di comodo
019300150505     d*// Oggi            s              8s 0 inz
019400141223     d wabi            s                   like(OTAA1cabi) inz
019500141223     d wmsg            s             78a   inz
019600141223     d w001a           s              1a   inz
019700141223     d w003a           s              3a   inz
019800141223     d w0030           s              3s 0 inz
019900141223     d w007a           s              7a   inz
020000141223     d w0070           s              7s 0 inz
020100141223     d w050a           s             50a   inz
020200141223
020300141223      // - Campi x ricerca tabelle TABEL
020400141223     d idTabella       s              2
020500141223     d Ordinamento     s              1
020600141223     d idElemento      s              8
020700141223     d TastoUscita     s              1
020800141223
020900141223      // - Parametri per ricerca Commerciale Unificante
021000141223      /copy gaitrasrc/srcprotopi,TRMK43R_1
021100141223
021200141223      //---------------------------------------------------------------
021300141223      //?Definizione procedure usate.
021400141223      //---------------------------------------------------------------
021500141223
021600141223      // - Sottomissione lavoro batch
021700141223     d bch10           pr                  extpgm('BCH10')
021800141223     d  kpjba                              likeds(KPJBA)
021900141223
022000141223       // - Interrogazione anagrafica campagne
022100150112     d TRKC02R         pr                  extpgm('TRKC02R')
022200141223     d  kpjba                              likeds(kpjba)
022300150112     d  trkc02ds                           likeds(TRKC02DS)
022400141223
022500141223       // - Stampa Report
022600150112     d TRKC41C         pr                  extpgm('TRKC41C')
022700141223     d  kpjba                              likeds(kpjba)
022800141223
022900141223       // - Caricamento Filiali in gestione
023000141223     d TRUL31R         pr                  extpgm('TRUL31R')
023100141223     d  kpjba                              likeds(kpjba)
023200141223     d  trul31ds                           likeds(trul31ds)
023300141223     d  trul31ds2                          likeds(trul31ds2)
023400141223
023500141223      //---------------------------------------------------------------
023600141223      //?Definizione Prototipi.
023700141223      //---------------------------------------------------------------
023800141223
023900141223      /copy gaitrasrc/srcprotopr,TIBS34R
024000141223      /copy gaitrasrc/srcprotopr,TNTAA1C
024100141223      /copy gaitrasrc/srcprotopr,TRMK43R
024200141223      /copy gaitrasrc/srcprotopr,TRUL19R
024300141223
024400141223      //---------------------------------------------------------------
024500141223      //?Definizione key-list.
024600141223      //---------------------------------------------------------------
024700141223       // - File TABEL00F
024800141223     d k03tabel      e ds                  extname(TABEL00F:*key)
024900141223     d                                     prefix(k_)
025000141223
025100141223      //---------------------------------------------------------------
025200141223      //?M A I N - L I N E
025300141223      //---------------------------------------------------------------
025400141223
025500141223     c     *Entry        plist
025600141223     c                   parm                    kpjba
025700141223
025800141223      /free
025900141223
026000141223       //?Operazioni iniziali
026100141223       exsr RoutInz;
026200141223
026300141223       //?Gestione video
026400141223       DOW  Fine = *off;
026500141223         SELECT;
026600141223           WHEN  Video = 'D2';
026700141223             exsr GesD02;
026800141223           OTHER;
026900141223             Fine = *on;
027000141223         ENDSL;
027100141223       ENDDO;
027200141223
027300141223       //?Operazioni finali
027400141223       exsr RoutEnd;
027500141223
027600141223       //--------------------------------------------------------------
027700141223       //?Operazioni iniziali.
027800141223       //--------------------------------------------------------------
027900141223       BEGSR RoutInz;
028000141223
028100141223         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028200141223
028300141223       //?Impostazione campi "fissi"
028400141223         V01pgm = SDSpgm;
028500141223         k_TBLkut = 1;
028600141223         Video = 'D2';
028700141223         wInzD02 = *on;
028800141223
028900150505       ////?Imposto oggi
029000150505         //Oggi = %dec(%date());
029100141223
029200141223       //?Reperimento dati job
029300141223         exsr DatiJob;
029400141223
029500141223       //?Controllo abilitazione utente
029600141223         reset TNTAA1DS;
029700141223         ITAA1caut = '§utegtc';
029800141223         clear kpjbu;
029900141223         kpjbu = TNTAA1DS;
030000141223         tntaa1c (kpjba);
030100141223         TNTAA1DS = kpjbu;
030200141223
030300141223         IF  OTAA1fabi = 'N';
030400141223           ErrGrave = *on;
030500141223           leavesr;
030600141223         ENDIF;
030700141223         wabi = OTAA1cabi;
030800141223
030900141223       //?Carico le filiali abilitate all'utente
031000141223         clear TRUL31DS;
031100141223         clear TRUL31DS2;
031200141223         I31abi = wabi;
031300141223         I31cdi = DUTdis;
031400141223         I31car = DUTare;
031500141223         I31cpo = DUTpou;
031600141223         TRUL31R (kpjba:trul31ds:trul31ds2);
031700141223         IF O31pog <= *zeros;
031800141223           ErrGrave = *on;
031900141223           leavesr;
032000141223         ENDIF;
032100141223
032200141223       //?Carico sk Distretti - 17
032300141223         clear xx;
032400141223         k_TBLcod = '17';
032500141223         clear k_TBLkey;
032600141223         setll %kds(K03tabel) TABEL00F;
032700141223         reade %kds(K03tabel : 2) TABEL00F;
032800141223         DOW  not %eof(TABEL00F);
032900141223           IF  TBLflg = *blanks;
033000141223             ds17 = TBLuni;
033100141223             xx += 1;
033200141223             skDistretti(xx) = %subst(TBLkey:1:1);
033300141223             skDesDist(xx) = §17des;
033400141223           ENDIF;
033500141223           reade %kds(K03tabel : 2) TABEL00F;
033600141223         ENDDO;
033700141223
033800141223       //?Carico sk Aree - 05
033900141223         clear xx;
034000141223         k_TBLcod = '05';
034100141223         clear k_TBLkey;
034200141223         setll %kds(K03tabel) TABEL00F;
034300141223         reade %kds(K03tabel : 2) TABEL00F;
034400141223         DOW  not %eof(TABEL00F);
034500141223           IF  TBLflg = *blanks;
034600141223             ds05 = TBLuni;
034700141223             xx += 1;
034800141223             skAree(xx) = %subst(TBLkey:1:3);
034900141223             skDesArea(xx) = §05des;
035000141223           ENDIF;
035100141223           reade %kds(K03tabel : 2) TABEL00F;
035200141223         ENDDO;
035300141223
035400141223       //?Carico sk Importanza Clienti
035500141223         clear xx;
035600141223         k_TBLcod = 'IC';
035700141223         clear k_TBLkey;
035800141223         setll %kds(K03tabel) TABEL00F;
035900141223         reade %kds(K03tabel : 2) TABEL00F;
036000141223         DOW  not %eof(TABEL00F);
036100141223           IF  TBLflg = *blanks;
036200141223             xx += 1;
036300141223             skIC(xx) = %subst(TBLkey:1:1);
036400141223             dsIC = TBLuni;
036500141223             skIC_ord(xx) = §SICor;
036600141223           ENDIF;
036700141223           reade %kds(K03tabel : 2) TABEL00F;
036800141223         ENDDO;
036900141223
037000141223       ENDSR;
037100141223
037200141223       //--------------------------------------------------------------
037300141223       //?Reperimento Dati del job (Utente/Operativi).
037400141223       //--------------------------------------------------------------
037500141223       BEGSR DatiJob;
037600141223
037700141223         in(E) §AzUte;
037800141223         IF  NOT %error;
037900141223           in(E) §DatiUte;
038000141223         ENDIF;
038100141223         IF  %error or RSut = *blanks;
038200141223           clear TIBS34ds;
038300141223           tibs34r(tibs34ds);
038400141223           in §AzUte;
038500141223           in §DatiUte;
038600141223         ENDIF;
038700141223
038800141223       ENDSR;
038900141223
039000141223       //--------------------------------------------------------------
039100141223       //?Gestione videata D02.
039200141223       //--------------------------------------------------------------
039300141223       BEGSR GesD02;
039400141223
039500141223       //?Inizializzazione videata
039600141223         IF  wInzD02;
039700141223           exsr InzD02;
039800141223           wInzD02 = *off;
039900141223         ENDIF;
040000141223
040100141223       //?Se errore grave emetto messaggio poi esco
040200141223         IF  ErrGrave;
040300141223           ErrMessage  = *on;
040400141223           ErrGenerico = *on;
040500141223           V02msg = Msg(01);
040600141223         ENDIF;
040700141223
040800141223       //?Emissione Testata
040900150112         write  KC40T01;
041000141223
041100141223       //?Emissione videata
041200150112         exfmt  KC40D02;
041300141223         reset ErrMessage;
041400141223         reset ErrGenerico;
041500141223         clear V02msg;
041600141223
041700141223         SELECT;
041800141223
041900141223       //?- Errore Grave esco
042000141223           WHEN  ErrGrave;
042100141223             Fine = *on;
042200141223
042300141223       //?- F03=Fine
042400141223           WHEN  dsp_aid = c_F03;
042500141223             exsr F03D02;
042600141223
042700141223       //?- F06=Conferma
042800141223           WHEN  dsp_aid = c_F06;
042900141223             exsr ContrD02;
043000141223             IF  ErrGenerico;
043100141223               leavesr;
043200141223             ENDIF;
043300141223             exsr F06D02;
043400141223
043500141223       //?Invio
043600141223           OTHER;
043700141223             exsr ContrD02;
043800141223             IF  ErrGenerico;
043900141223               leavesr;
044000141223             ENDIF;
044100141223
044200141223         ENDSL;
044300141223
044400141223       ENDSR;
044500141223
044600141223       //--------------------------------------------------------------
044700141223       //?Inizializzazione Videata D02.
044800141223       //--------------------------------------------------------------
044900141223       BEGSR InzD02;
045000141223
045100150223         VisFile      = *off;
045200141223         VisDistretto = *off;
045300141223         VisArea      = *off;
045400150123         VisTOT1      = *off;
045500150123         VisTOT2      = *off;
045600150123         VisTOT3      = *off;
045700150123         VisTOT4      = *off;
045800141024
045900141223       //?Pulizia videata
046000150112         clear KC40D02;
046100141223
046200141223       //?Se errore grave non imposto più niente
046300141223         IF  ErrGrave;
046400141223           leavesr;
046500141223         ENDIF;
046600141223
046700150123       //?Controllo Abilitazioni
046800141223         SELECT;
046900150123       //?AZ-Azienda
047000141223       //?vedo tutte le selezioni
047100150605         WHEN  wabi = 'AZ';
047200141223           VisDistretto = *on;
047300141223           VisArea      = *on;
047400150123       //?DI-Distretto
047500141223       //?NON vedo la selezione per Distretto
047600150605         WHEN  wabi = 'DI' or car(2) > *zeros;
047700141229           VisArea  = *on;
047800141223         ENDSL;
047900150123
048000150123       //?Imposto la richiesta dei totali in base all' autorizzazione utente
048100150123         SELECT;
048200150123         WHEN  wabi = 'AZ';
048300150123           visTOT1 = *on;
048400150123           visTOT2 = *on;
048500150123           visTOT3 = *on;
048600150123           visTOT4 = *on;
048700150123           V02dtot1 = TotAzienda;
048800150123           V02dtot2 = TotDistretto;
048900150123           V02dtot3 = TotArea;
049000150123           V02dtot4 = TotCommerciale;
049100150605         WHEN  wabi = 'DI';
049200150123           visTOT1 = *on;
049300150123           visTOT2 = *on;
049400150123           visTOT3 = *on;
049500150123           visTOT4 = *off;
049600150123           V02dtot1 = TotDistretto;
049700150123           V02dtot2 = TotArea;
049800150123           V02dtot3 = TotCommerciale;
049900150123           clear V02dtot4;
050000150126         WHEN  wabi = 'RA';
050100150123           visTOT1 = *on;
050200150123           visTOT2 = *on;
050300150123           visTOT3 = *off;
050400150123           visTOT4 = *off;
050500150123           V02dtot1 = TotArea;
050600150123           V02dtot2 = TotCommerciale;
050700150123           clear V02dtot3;
050800150123           clear V02dtot4;
050900150123         OTHER;
051000150123           visTOT1 = *on;
051100150123           visTOT2 = *off;
051200150123           visTOT3 = *off;
051300150123           visTOT4 = *off;
051400150123           V02dtot1 = TotCommerciale;
051500150123           clear V02dtot2;
051600150123           clear V02dtot3;
051700150123           clear V02dtot4;
051800150123           V02tot1 = 'S';
051900150123         ENDSL;
052000150223
052100150223       //?Imposto la richiesta del file se utente di sede
052200150310         IF  DUTpou = 046;
052300150223           visFile = *on;
052400150223         ENDIF;
052500141223
052600141223       ENDSR;
052700141223
052800141223       //--------------------------------------------------------------
052900141223       //?Gestione tasto funzionale F03 da videata D02
053000141223       //?F03=Fine
053100141223       //--------------------------------------------------------------
053200141223       BEGSR F03D02;
053300141223
053400141223       //?Chiusura del programma
053500141223         Fine = *on;
053600141223
053700141223       ENDSR;
053800141223
053900141223       //--------------------------------------------------------------
054000141223       //?Gestione tasto funzionale F06 da videata D02
054100141223       //?F06=Conferma
054200141223       //--------------------------------------------------------------
054300141223       BEGSR F06D02;
054400141223
054500141223       //?Lancio il report
054600150112         clear TRKC41DS;
054700150107
054800150112         IKC41abi = wabi;
054900141230         IF  V02ncm <> *blanks;
055000150112           IKC41ncm = %dec(V02ncm:7:0);
055100141223         ENDIF;
055200150112         IKC41cdi = V02cdi;
055300141230         IF  V02car <> *blanks;
055400150112           IKC41car = %dec(V02car:3:0);
055500141223         ENDIF;
055600141230         IF  V02cmm <> *blanks;
055700150112           IKC41cmm = %dec(V02cmm:7:0);
055800141223         ENDIF;
055900150112         IKC41clv1 = V02clv1;
056000150112         IKC41clv2 = V02clv2;
056100150112         IKC41clv3 = V02clv3;
056200150112         IKC41clv4 = V02clv4;
056300150112         IKC41clv5 = V02clv5;
056400150216         IKC41objp = V02objp;
056500150216         IKC41objf = V02objf;
056600150123
056700150123       //?Totale 1
056800150123         IF  V02tot1 <> *blanks;
056900150123           IF  V02dtot1 = TotAzienda;
057000150123             IKC41totaz = 'S';
057100150123           ENDIF;
057200150123           IF  V02dtot1 = TotDistretto;
057300150123             IKC41totdi = 'S';
057400150123           ENDIF;
057500150123           IF  V02dtot1 = TotArea;
057600150123             IKC41totra = 'S';
057700150123           ENDIF;
057800150123           IF  V02dtot1 = TotCommerciale;
057900150123             IKC41totcm = 'S';
058000150123           ENDIF;
058100150123         ENDIF;
058200150123       //?Totale 2
058300150123         IF  V02tot2 <> *blanks;
058400150123           IF  V02dtot2 = TotAzienda;
058500150123             IKC41totaz = 'S';
058600150123           ENDIF;
058700150123           IF  V02dtot2 = TotDistretto;
058800150123             IKC41totdi = 'S';
058900150123           ENDIF;
059000150123           IF  V02dtot2 = TotArea;
059100150123             IKC41totra = 'S';
059200150123           ENDIF;
059300150123           IF  V02dtot2 = TotCommerciale;
059400150123             IKC41totcm = 'S';
059500150123           ENDIF;
059600150123         ENDIF;
059700150123       //?Totale 3
059800150123         IF  V02tot3 <> *blanks;
059900150123           IF  V02dtot3 = TotAzienda;
060000150123             IKC41totaz = 'S';
060100150123           ENDIF;
060200150123           IF  V02dtot3 = TotDistretto;
060300150123             IKC41totdi = 'S';
060400150123           ENDIF;
060500150123           IF  V02dtot3 = TotArea;
060600150123             IKC41totra = 'S';
060700150123           ENDIF;
060800150123           IF  V02dtot3 = TotCommerciale;
060900150123             IKC41totcm = 'S';
061000150123           ENDIF;
061100150123         ENDIF;
061200150123       //?Totale 4
061300150123         IF  V02tot4 <> *blanks;
061400150123           IF  V02dtot4 = TotAzienda;
061500150123             IKC41totaz = 'S';
061600150123           ENDIF;
061700150123           IF  V02dtot4 = TotDistretto;
061800150123             IKC41totdi = 'S';
061900150123           ENDIF;
062000150123           IF  V02dtot4 = TotArea;
062100150123             IKC41totra = 'S';
062200150123           ENDIF;
062300150123           IF  V02dtot4 = TotCommerciale;
062400150123             IKC41totcm = 'S';
062500150123           ENDIF;
062600150123         ENDIF;
062700150223
062800150223       //?File
062900150223         IKC41file = V02file;
063000141223
063100150112         kcoaz = 'KC41';
063200150112         kpjbu = TRKC41ds;
063300141223         kbuff = *blanks;
063400141223
063500141223       //?Richiamo interattivo
063600141223         IF  knmus = *all'1';
063700150112           TRKC41C(kpjba);
063800141223       //?Richiamo batch
063900141223         ELSE;
064000141223           BCH10(kpjba);
064100141223         ENDIF;
064200141223
064300141223       //?Chiusura del programma
064400141223         Fine = *on;
064500141223
064600141223       ENDSR;
064700141223
064800141223       //--------------------------------------------------------------
064900141223       //?Controlla Videata D02.
065000141223       //--------------------------------------------------------------
065100141223       BEGSR ContrD02;
065200141223
065300141223         WindDspF = IndDspF;
065400141223         reset WindDspFb;
065500141223         IndDspF  = WindDspF;
065600141223
065700141223       //?Campagna Commerciale Obbligatoria
065800141223         clear V02des;
065900141223         IF  V02ncm = *blanks;
066000141223           ErrMessage  = *on;
066100141223           ErrGenerico = *on;
066200141223           PosCurNCM   = *on;
066300141223           V02msg = Msg(02);
066400141223           leavesr;
066500141223         ENDIF;
066600141223       //?Ricerca Campagna Commerciale
066700141223         IF  %scan('?':V02ncm) > 0;
066800150112           clear TRKC02DS;
066900150112           IKC02ric = 'R';
067000150112           trkc02r (kpjba:trkc02ds);
067100141223           ErrGenerico = *on;
067200141223           PosCurNCM   = *on;
067300150112           V02ncm = %editc(OKC02ncm:'X');
067400141223         ENDIF;
067500141223       //?Accetto solo dati numerici
067600141223         xx = 1;
067700141223         FOR xx by 1 to %len(V02ncm);
067800141223           IF  %subst(V02ncm:xx:1) <> *blanks and
067900141223               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
068000141223             ErrMessage  = *on;
068100141223             ErrGenerico = *on;
068200141223             PosCurNCM   = *on;
068300141223             V02msg = Msg(02);
068400141223             leavesr;
068500141223           ENDIF;
068600141223         ENDFOR;
068700141223       //?Deve essere una Campagna Commerciale valida
068800141223         CMPncm = %dec(V02ncm:7:0);
068900150112         chain CMPncm TICMP01L;
069000141223         IF  not %found(TICMP01L);
069100141223           ErrMessage  = *on;
069200141223           ErrGenerico = *on;
069300141223           PosCurNCM   = *on;
069400141223           V02msg = Msg(02);
069500141223           leavesr;
069600141223         ENDIF;
069700141223       //?Decodifico Campagna Commerciale
069800141223         V02des = CMPdes;
069900150505       ////?Campagna Commerciale già finita
070000150505         //IF  CMPdfc < Oggi;
070100150505         //  ErrMessage  = *on;
070200150505         //  ErrGenerico = *on;
070300150505         //  PosCurNCM   = *on;
070400150505         //  V02msg = %trim(Msg(02)) + '. Già chiusa';
070500150505         //  leavesr;
070600150505         //ENDIF;
070700141223       //?In carico all'Utente
070800141223         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
070900141223           ErrMessage  = *on;
071000141223           ErrGenerico = *on;
071100141223           PosCurNCM   = *on;
071200141223           V02msg = %trim(Msg(02)) + '. Non in gestione';
071300141223           leavesr;
071400141223         ENDIF;
071500141223
071600141223       //?Distretto
071700141223         w001a = V02cdi;
071800141223         clear w050a;
071900141223         exsr Distretto;
072000141223         V02cdi = w001a;
072100141223         V02cdid = w050a;
072200141223         V02msg = wmsg;
072300141223         IF  ErrGenerico;
072400141223           leavesr;
072500141223         ENDIF;
072600141223
072700141223       //?Area
072800141223         w003a = V02car;
072900141223         clear w050a;
073000141223         exsr Area;
073100141223         V02car = w003a;
073200141223         V02card = w050a;
073300141223         V02msg = wmsg;
073400141223         IF  ErrGenerico;
073500141223           leavesr;
073600141223         ENDIF;
073700141223       //?Se presente anche il distretto devono essere congruenti
073800141223         IF  V02cdi <> *blanks and V02car <> *blanks;
073900141223           chain (V02cdi:w0030) AZORG02L;
074000141223           IF  not %found(AZORG02L) or ORGfva <> *blanks;
074100141223             ErrGenerico = *on;
074200141223             ErrMessage  = *on;
074300141223             PosCurCAR   = *on;
074400141223             V02msg = %trim(Msg(04)) + '. Non congruente con il distretto';
074500141223             leavesr;
074600141223           ENDIF;
074700141223         ENDIF;
074800141223
074900141223       //?Commerciale Unificante
075000141223         w007a = V02cmm;
075100141223         clear w050a;
075200141223         exsr Commerciale;
075300141223         V02cmm = w007a;
075400141223         V02cmmd = w050a;
075500141223         V02msg = wmsg;
075600141223         IF  ErrGenerico;
075700141223           leavesr;
075800141223         ENDIF;
075900150605
076000150605       //?Se utente NON di sede a abilitato a più di un'area e no
076100150605       //?abilitazione 'AZ' o 'DI'
076200150605       //?richiedo obbligatoria l'area
076300150605         IF  DUTpou <> 046 and wabi <> 'AZ' and wabi <> 'DI' and
076400150605             V02car = *blanks and V02cmm = *blanks and
076500150605             car(2) > *zeros;
076600150605           ErrMessage  = *on;
076700150605           ErrGenerico = *on;
076800150605           PosCurCAR   = *on;
076900150605           V02msg = Msg(08);
077000150605           leavesr;
077100150605         ENDIF;
077200141223
077300141223       //?Importanza Cliente
077400141223         w001a = V02clv1;
077500141223         clear w050a;
077600141223         exsr ImpCliente;
077700141223         V02clv1 = w001a;
077800141223         V02msg = wmsg;
077900141223         IF  ErrGenerico;
078000141223           PosCurCLV1 = *on;
078100141223           leavesr;
078200141223         ENDIF;
078300141223         w001a = V02clv2;
078400141223         clear w050a;
078500141223         exsr ImpCliente;
078600141223         V02clv2 = w001a;
078700141223         V02msg = wmsg;
078800141223         IF  ErrGenerico;
078900141223           PosCurCLV2 = *on;
079000141223           leavesr;
079100141223         ENDIF;
079200141223         w001a = V02clv3;
079300141223         clear w050a;
079400141223         exsr ImpCliente;
079500141223         V02clv3 = w001a;
079600141223         V02msg = wmsg;
079700141223         IF  ErrGenerico;
079800141223           PosCurCLV3 = *on;
079900141223           leavesr;
080000141223         ENDIF;
080100141223         w001a = V02clv4;
080200141223         clear w050a;
080300141223         exsr ImpCliente;
080400141223         V02clv4 = w001a;
080500141223         V02msg = wmsg;
080600141223         IF  ErrGenerico;
080700141223           PosCurCLV4 = *on;
080800141223           leavesr;
080900141223         ENDIF;
081000141223         w001a = V02clv5;
081100141223         clear w050a;
081200141223         exsr ImpCliente;
081300141223         V02clv5 = w001a;
081400141223         V02msg = wmsg;
081500141223         IF  ErrGenerico;
081600141223           PosCurCLV5 = *on;
081700141223           leavesr;
081800141223         ENDIF;
081900150123
082000150123       //?Stampa totali
082100150123         IF  V02tot1 = *blanks and V02tot2 = *blanks and
082200150123             V02tot3 = *blanks and V02tot4 = *blanks;
082300150123           ErrGenerico = *on;
082400150123           ErrMessage  = *on;
082500150123           PosCurTOT1  = *on;
082600150123           V02msg = Msg(07);
082700150123           leavesr;
082800150123         ENDIF;
082900150608       //?Non posso richiedere un totale superiore alla selezione fatta
083000150608         SELECT;
083100150608         //?Posso richiedere tutti e 4 i totali
083200150608         //?Azienda - Distretto - Area - Commerciale
083300150608         WHEN  VisTOT4 and
083400150608               V02dtot1 = TotAzienda and V02tot1 <> *blanks and
083500150608               (V02cdi <> *blanks or V02car <> *blanks or V02cmm <> *blanks);
083600150608           ErrMessage  = *on;
083700150608           ErrGenerico = *on;
083800150608           PosCurTOT1  = *on;
083900150608           V02msg = 'Totale per ' + %trim(V02dtot1) + ' non ammesso';
084000150608           leavesr;
084100150608         WHEN  VisTOT4 and
084200150608               V02dtot2 = TotDistretto and V02tot2 <> *blanks and
084300150608               (V02car <> *blanks or V02cmm <> *blanks);
084400150608           ErrMessage  = *on;
084500150608           ErrGenerico = *on;
084600150608           PosCurTOT2  = *on;
084700150608           V02msg = 'Totale per ' + %trim(V02dtot2) + ' non ammesso';
084800150608           leavesr;
084900150608         WHEN  VisTOT4 and
085000150608               V02dtot3 = TotArea and V02tot3 <> *blanks and
085100150608               V02cmm <> *blanks;
085200150608           ErrMessage  = *on;
085300150608           ErrGenerico = *on;
085400150608           PosCurTOT3  = *on;
085500150608           V02msg = 'Totale per ' + %trim(V02dtot3) + ' non ammesso';
085600150608           leavesr;
085700150608         //?Posso richiedere 3 totali
085800150608         //?Distretto - Area - Commerciale
085900150608         WHEN  VisTOT3 and
086000150608               V02dtot1 = TotDistretto and V02tot1 <> *blanks and
086100150608               (V02car <> *blanks or V02cmm <> *blanks);
086200150608           ErrMessage  = *on;
086300150608           ErrGenerico = *on;
086400150608           PosCurTOT1  = *on;
086500150608           V02msg = 'Totale per ' + %trim(V02dtot1) + ' non ammesso';
086600150608           leavesr;
086700150608         WHEN  VisTOT3 and
086800150608               V02dtot2 = TotArea and V02tot2 <> *blanks and
086900150608               V02cmm <> *blanks;
087000150608           ErrMessage  = *on;
087100150608           ErrGenerico = *on;
087200150608           PosCurTOT2  = *on;
087300150608           V02msg = 'Totale per ' + %trim(V02dtot2) + ' non ammesso';
087400150608           leavesr;
087500150608         //?Posso richiedere 2 totali
087600150608         //?Area - Commerciale
087700150608         WHEN  VisTOT2 and
087800150608               V02dtot1 = TotArea and V02tot1 <> *blanks and
087900150608               V02cmm <> *blanks;
088000150608           ErrMessage  = *on;
088100150608           ErrGenerico = *on;
088200150608           PosCurTOT1  = *on;
088300150608           V02msg = 'Totale per ' + %trim(V02dtot1) + ' non ammesso';
088400150608           leavesr;
088500150608         ENDSL;
088600141223
088700141223       ENDSR;
088800141223
088900141223       //--------------------------------------------------------------
089000141223       //?Controllo Distretto.
089100141223       //--------------------------------------------------------------
089200141223       BEGSR Distretto;
089300141223
089400141223         IF  w001a = *blanks;
089500141223           leavesr;
089600141223         ENDIF;
089700141223
089800141223       //?Ricerca Distretto
089900141223         IF  %scan('?' : w001a) > 0;
090000141223           ErrGenerico = *on;
090100141223           PosCurCDI   = *on;
090200141223           idTabella = '17';
090300141223           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
090400141223                            tastoUscita);
090500141223           w001a = idElemento;
090600141223         ENDIF;
090700141223       //?Deve essere un Distretto valido
090800141223         clear xx;
090900141223         xx = %lookup(w001a : skDistretti);
091000141223         IF  xx = 0;
091100141223           ErrMessage  = *on;
091200141223           ErrGenerico = *on;
091300141223           PosCurCDI   = *on;
091400141223           wmsg = Msg(03);
091500141223           leavesr;
091600141223         ENDIF;
091700141223       //?Decodifico distretto
091800141223         w050a = skDesDist(xx);
091900141223       //?In carico all'Utente
092000141223         IF  %lookup (w001a:DIS) = 0;
092100141223           ErrMessage  = *on;
092200141223           ErrGenerico = *on;
092300141223           PosCurCDI   = *on;
092400141223           wmsg = %trim(Msg(03)) + '. Non in gestione';
092500141223           leavesr;
092600141223         ENDIF;
092700141223
092800141223       ENDSR;
092900141223
093000141223       //--------------------------------------------------------------
093100141223       //?Controllo Area.
093200141223       //--------------------------------------------------------------
093300141223       BEGSR Area;
093400141223
093500141223         IF  w003a = *blanks;
093600141223           leavesr;
093700141223         ENDIF;
093800141223
093900141223       //?Ricerca Area
094000141223         IF  %scan('?' : w003a) > 0;
094100141223           ErrGenerico = *on;
094200141223           PosCurCAR   = *on;
094300141223           idTabella = '05';
094400141223           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
094500141223                          tastoUscita);
094600141223           w003a = idElemento;
094700141223         ENDIF;
094800141223         IF  w003a = *blanks;
094900141223           leavesr;
095000141223         ENDIF;
095100141223       //?Accetto solo dati numerici
095200141223         xx = 1;
095300141223         FOR xx by 1 to %len(w003a);
095400141223           IF  %subst(w003a:xx:1) <> *blanks and
095500141223               %check(Digits:%subst(w003a:xx:1)) > *zeros;
095600141223             ErrMessage  = *on;
095700141223             ErrGenerico = *on;
095800141223             PosCurCAR   = *on;
095900141223             wmsg = Msg(04);
096000141223             leavesr;
096100141223           ENDIF;
096200141223         ENDFOR;
096300141223       //?Deve essere un'Area valida
096400141223         clear xx;
096500141223         w0030 = %dec(w003a:3:0);
096600141223         xx = %lookup(%editc(w0030:'X'):skAree);
096700141223         IF  xx = 0;
096800141223           ErrMessage  = *on;
096900141223           ErrGenerico = *on;
097000141223           PosCurCAR   = *on;
097100141223           wmsg = Msg(04);
097200141223           leavesr;
097300141223         ENDIF;
097400141223       //?Decodifico area
097500141223         w050a = skDesArea(xx);
097600141223       //?Deve essere un'area in carico all'utente
097700141223         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
097800141223           ErrGenerico = *on;
097900141223           ErrMessage  = *on;
098000141223           PosCurCAR   = *on;
098100141223           wmsg = %trim(Msg(04)) + '. Non in gestione';
098200141223           leavesr;
098300141223         ENDIF;
098400141223
098500141223       ENDSR;
098600141223
098700141223       //--------------------------------------------------------------
098800141223       //?Controllo Commerciale.
098900141223       //--------------------------------------------------------------
099000141223       BEGSR Commerciale;
099100141223
099200141223         IF  w007a = *blanks;
099300141223           leavesr;
099400141223         ENDIF;
099500141223
099600141223       //?Ricerca Commerciale
099700141223         IF  %scan('?' : w007a) > 0;
099800141223           clear TRMK43DS;
099900141223           IMK43solu = 'S';
100000141223           IMK43fil = DUTpou;
100100141223           trmk43r (kpjba : TRMK43DS);
100200141223           IF  OMK43err = *off and OMK43fxx = *blanks;
100300141223             w007a = %editc(OMK43cmm:'X');
100400141223             w050a = OMK43des;
100500141223           ENDIF;
100600141223           ErrGenerico = *on;
100700141223           PosCurCMM   = *on;
100800141223         ENDIF;
100900141223       //?Accetto solo dati numerici
101000141223         xx = 1;
101100141223         FOR xx by 1 to %len(w007a);
101200141223           IF  %subst(w007a:xx:1) <> *blanks and
101300141223               %check(Digits:%subst(w007a:xx:1)) > *zeros;
101400141223             ErrMessage  = *on;
101500141223             ErrGenerico = *on;
101600141223             PosCurCMM   = *on;
101700150123             wmsg = Msg(05);
101800141223             leavesr;
101900141223           ENDIF;
102000141223         ENDFOR;
102100141223         w0070 = %dec(w007a:7:0);
102200141223       //?Deve esistere il Commerciale
102300141223         chain (w0070) AZCMM01L;
102400141223         IF  not %found(AZCMM01L);
102500141223           ErrMessage  = *on;
102600141223           ErrGenerico = *on;
102700141223           PosCurCMM   = *on;
102800150123           wmsg = Msg(05);
102900141223           leavesr;
103000141223         ENDIF;
103100141223       //?Decodifico Commerciale
103200141223         w050a = CMMdes;
103300141223       //?Deve essere un Commerciale in carico all'utente
103400141223         clear TNTAA1DS;
103500141223         ITAA1caut = '§utegtc';
103600141223         ITAA1cmm  = w0070;
103700141223         kpjbu = TNTAA1DS;
103800141223         tntaa1c (kpjba);
103900141223         TNTAA1DS = kpjba;
104000141223         IF  OTAA1fabi = 'N';
104100141223           ErrGenerico = *on;
104200141223           ErrMessage  = *on;
104300141223           PosCurCMM   = *on;
104400150123           wmsg = %trim(Msg(05)) + '. Non in gestione';
104500141223           leavesr;
104600141223         ENDIF;
104700141223       //?Deve essere un Commerciale Unificante
104800141223         IF  w0070 <> CMMuni;
104900141223           ErrGenerico = *on;
105000141223           ErrMessage  = *on;
105100141223           PosCurCMM   = *on;
105200150123           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
105300141223           leavesr;
105400141223         ENDIF;
105500141223
105600141223       ENDSR;
105700141223
105800141223       //--------------------------------------------------------------
105900141223       //?Controllo Importanza Cliente.
106000141223       //--------------------------------------------------------------
106100141223       BEGSR ImpCliente;
106200141223
106300141223         IF  w001a = *blanks;
106400141223           leavesr;
106500141223         ENDIF;
106600141223
106700141223         IF  %scan('?' : w001a) > 0;
106800141223           ErrGenerico = *on;
106900141223           idTabella = 'IC';
107000141223           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
107100141223                          tastoUscita);
107200141223           w001a = idElemento;
107300141223         ENDIF;
107400141223         IF  %lookup(w001a : skIC) = 0;
107500141223           ErrMessage  = *on;
107600141223           ErrGenerico = *on;
107700141223           PosCurCLV1  = *on;
107800150123           wmsg = Msg(06);
107900141223           leavesr;
108000141223         ENDIF;
108100141223
108200141223       ENDSR;
108300141223
108400141223       //--------------------------------------------------------------
108500141223       //?Operazioni finali.
108600141223       //--------------------------------------------------------------
108700141223       BEGSR RoutEnd;
108800141223
108900141223         *inLR = *on;
109000141223         return;
109100141223
109200141223       ENDSR;
109300141223
109400141223      /end-free
109500141223
109600141223       //--------------------------------------------------------------
109700141223       //?Schiere a tempo di compilazione.
109800141223       //--------------------------------------------------------------
109900141223
110000141223** -- MSG -------------------------------------------------------------------*
110100141223Utente non abilitato alla Funzione.                                            1
110200141223Campagna Commerciale errata                                                    2
110300141223Distretto Errato                                                               3
110400141223Area Errata                                                                    4
110500150126Commerciale Unificante Errato                                                  5
110600150126Codice Importanza Clienti Errato                                               6
110700150123Impostare il totale da stampare                                                7
110800150605Impostare l'aera da elaborare                                                  8
