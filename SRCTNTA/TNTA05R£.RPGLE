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
013700141223        // - Indicatori di errore generico
013800141223     d  ErrGenerico                   1n   overlay(IndDspF : 99)
013900141223
014000141223     d WindDspF        ds                  inz
014100141223     d  WindDspFa              1     49    inz(*zero)
014200141223     d  WindDspFb             50     99    inz(*zero)
014300141223
014400141223      // - Parametri ricevuti
014500141223     d KPJBA         e ds
014600141223
014700150112      // - Richiamo pgm TRKC02R - Interrogazione campagne
014800150112     d TRKC02DS      e ds
014900141223
015000150112      // - Richiamo pgm TRKC41C - Stampa Report
015100150112     d TRKC41DS      e ds
015200141223
015300141223      // - Reperimento dati utente
015400141223     d TIBS34DS      e ds
015500141223
015600141223      // - Controllo abilitazioni
015700141223     d TNTAA1DS      e ds                  inz
015800141223
015900141223      // - Reperimento filiali gestite dall'utente
016000141223     d TRUL31DS      e ds
016100141223     d  POG                   10    759    DIM(250)
016200141223     d TRUL31DS2     e ds
016300141223     d  DIS                    1     20    DIM(20)
016400141223     d  CAR                   22    171    DIM(50)
016500141223
016600141223      // - Tabella 05 - Aree
016700141223     d ds05          e ds
016800141223
016900141223      // - Tabella 17 - Distretti
017000141223     d ds17          e ds
017100141223
017200141223      // - Tabella IC - Importanza Clienti
017300141223     d dsIC          e ds
017400141223
017500141223      //---------------------------------------------------------------
017600141223      //?Definizione variabili globali.
017700141223      //---------------------------------------------------------------
017800141223
017900141223      // - Flags booleani
018000141223     d ErrGrave        s               n   inz(*off)
018100141223     d Fine            s               n   inz(*off)
018200141029     d wInzD02         s               n   inz(*off)
018300141223
018400141223      // - Indici di schiera
018500141223     d xx              s              4s 0 inz
018600141223
018700141223      // - Campi associati al video
018800141223     d Video           s              2a   inz('D2')
018900141223
019000141223      // - Campi di comodo
019100141223     d Oggi            s              8s 0 inz
019200141223     d wabi            s                   like(OTAA1cabi) inz
019300141223     d wmsg            s             78a   inz
019400141223     d w001a           s              1a   inz
019500141223     d w003a           s              3a   inz
019600141223     d w0030           s              3s 0 inz
019700141223     d w007a           s              7a   inz
019800141223     d w0070           s              7s 0 inz
019900141223     d w050a           s             50a   inz
020000141223
020100141223      // - Campi x ricerca tabelle TABEL
020200141223     d idTabella       s              2
020300141223     d Ordinamento     s              1
020400141223     d idElemento      s              8
020500141223     d TastoUscita     s              1
020600141223
020700141223      // - Parametri per ricerca Commerciale Unificante
020800141223      /copy gaitrasrc/srcprotopi,TRMK43R_1
020900141223
021000141223      //---------------------------------------------------------------
021100141223      //?Definizione procedure usate.
021200141223      //---------------------------------------------------------------
021300141223
021400141223      // - Sottomissione lavoro batch
021500141223     d bch10           pr                  extpgm('BCH10')
021600141223     d  kpjba                              likeds(KPJBA)
021700141223
021800141223       // - Interrogazione anagrafica campagne
021900150112     d TRKC02R         pr                  extpgm('TRKC02R')
022000141223     d  kpjba                              likeds(kpjba)
022100150112     d  trkc02ds                           likeds(TRKC02DS)
022200141223
022300141223       // - Stampa Report
022400150112     d TRKC41C         pr                  extpgm('TRKC41C')
022500141223     d  kpjba                              likeds(kpjba)
022600141223
022700141223       // - Caricamento Filiali in gestione
022800141223     d TRUL31R         pr                  extpgm('TRUL31R')
022900141223     d  kpjba                              likeds(kpjba)
023000141223     d  trul31ds                           likeds(trul31ds)
023100141223     d  trul31ds2                          likeds(trul31ds2)
023200141223
023300141223      //---------------------------------------------------------------
023400141223      //?Definizione Prototipi.
023500141223      //---------------------------------------------------------------
023600141223
023700141223      /copy gaitrasrc/srcprotopr,TIBS34R
023800141223      /copy gaitrasrc/srcprotopr,TNTAA1C
023900141223      /copy gaitrasrc/srcprotopr,TRMK43R
024000141223      /copy gaitrasrc/srcprotopr,TRUL19R
024100141223
024200141223      //---------------------------------------------------------------
024300141223      //?Definizione key-list.
024400141223      //---------------------------------------------------------------
024500141223       // - File TABEL00F
024600141223     d k03tabel      e ds                  extname(TABEL00F:*key)
024700141223     d                                     prefix(k_)
024800141223
024900141223      //---------------------------------------------------------------
025000141223      //?M A I N - L I N E
025100141223      //---------------------------------------------------------------
025200141223
025300141223     c     *Entry        plist
025400141223     c                   parm                    kpjba
025500141223
025600141223      /free
025700141223
025800141223       //?Operazioni iniziali
025900141223       exsr RoutInz;
026000141223
026100141223       //?Gestione video
026200141223       DOW  Fine = *off;
026300141223         SELECT;
026400141223           WHEN  Video = 'D2';
026500141223             exsr GesD02;
026600141223           OTHER;
026700141223             Fine = *on;
026800141223         ENDSL;
026900141223       ENDDO;
027000141223
027100141223       //?Operazioni finali
027200141223       exsr RoutEnd;
027300141223
027400141223       //--------------------------------------------------------------
027500141223       //?Operazioni iniziali.
027600141223       //--------------------------------------------------------------
027700141223       BEGSR RoutInz;
027800141223
027900141223         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028000141223
028100141223       //?Impostazione campi "fissi"
028200141223         V01pgm = SDSpgm;
028300141223         k_TBLkut = 1;
028400141223         Video = 'D2';
028500141223         wInzD02 = *on;
028600141223
028700141223       //?Imposto oggi
028800141223         Oggi = %dec(%date());
028900141223
029000141223       //?Reperimento dati job
029100141223         exsr DatiJob;
029200141223
029300141223       //?Controllo abilitazione utente
029400141223         reset TNTAA1DS;
029500141223         ITAA1caut = '§utegtc';
029600141223         clear kpjbu;
029700141223         kpjbu = TNTAA1DS;
029800141223         tntaa1c (kpjba);
029900141223         TNTAA1DS = kpjbu;
030000141223
030100141223         IF  OTAA1fabi = 'N';
030200141223           ErrGrave = *on;
030300141223           leavesr;
030400141223         ENDIF;
030500141223         wabi = OTAA1cabi;
030600141223
030700141223       //?Carico le filiali abilitate all'utente
030800141223         clear TRUL31DS;
030900141223         clear TRUL31DS2;
031000141223         I31abi = wabi;
031100141223         I31cdi = DUTdis;
031200141223         I31car = DUTare;
031300141223         I31cpo = DUTpou;
031400141223         TRUL31R (kpjba:trul31ds:trul31ds2);
031500141223         IF O31pog <= *zeros;
031600141223           ErrGrave = *on;
031700141223           leavesr;
031800141223         ENDIF;
031900141223
032000141223       //?Carico sk Distretti - 17
032100141223         clear xx;
032200141223         k_TBLcod = '17';
032300141223         clear k_TBLkey;
032400141223         setll %kds(K03tabel) TABEL00F;
032500141223         reade %kds(K03tabel : 2) TABEL00F;
032600141223         DOW  not %eof(TABEL00F);
032700141223           IF  TBLflg = *blanks;
032800141223             ds17 = TBLuni;
032900141223             xx += 1;
033000141223             skDistretti(xx) = %subst(TBLkey:1:1);
033100141223             skDesDist(xx) = §17des;
033200141223           ENDIF;
033300141223           reade %kds(K03tabel : 2) TABEL00F;
033400141223         ENDDO;
033500141223
033600141223       //?Carico sk Aree - 05
033700141223         clear xx;
033800141223         k_TBLcod = '05';
033900141223         clear k_TBLkey;
034000141223         setll %kds(K03tabel) TABEL00F;
034100141223         reade %kds(K03tabel : 2) TABEL00F;
034200141223         DOW  not %eof(TABEL00F);
034300141223           IF  TBLflg = *blanks;
034400141223             ds05 = TBLuni;
034500141223             xx += 1;
034600141223             skAree(xx) = %subst(TBLkey:1:3);
034700141223             skDesArea(xx) = §05des;
034800141223           ENDIF;
034900141223           reade %kds(K03tabel : 2) TABEL00F;
035000141223         ENDDO;
035100141223
035200141223       //?Carico sk Importanza Clienti
035300141223         clear xx;
035400141223         k_TBLcod = 'IC';
035500141223         clear k_TBLkey;
035600141223         setll %kds(K03tabel) TABEL00F;
035700141223         reade %kds(K03tabel : 2) TABEL00F;
035800141223         DOW  not %eof(TABEL00F);
035900141223           IF  TBLflg = *blanks;
036000141223             xx += 1;
036100141223             skIC(xx) = %subst(TBLkey:1:1);
036200141223             dsIC = TBLuni;
036300141223             skIC_ord(xx) = §SICor;
036400141223           ENDIF;
036500141223           reade %kds(K03tabel : 2) TABEL00F;
036600141223         ENDDO;
036700141223
036800141223       ENDSR;
036900141223
037000141223       //--------------------------------------------------------------
037100141223       //?Reperimento Dati del job (Utente/Operativi).
037200141223       //--------------------------------------------------------------
037300141223       BEGSR DatiJob;
037400141223
037500141223         in(E) §AzUte;
037600141223         IF  NOT %error;
037700141223           in(E) §DatiUte;
037800141223         ENDIF;
037900141223         IF  %error or RSut = *blanks;
038000141223           clear TIBS34ds;
038100141223           tibs34r(tibs34ds);
038200141223           in §AzUte;
038300141223           in §DatiUte;
038400141223         ENDIF;
038500141223
038600141223       ENDSR;
038700141223
038800141223       //--------------------------------------------------------------
038900141223       //?Gestione videata D02.
039000141223       //--------------------------------------------------------------
039100141223       BEGSR GesD02;
039200141223
039300141223       //?Inizializzazione videata
039400141223         IF  wInzD02;
039500141223           exsr InzD02;
039600141223           wInzD02 = *off;
039700141223         ENDIF;
039800141223
039900141223       //?Se errore grave emetto messaggio poi esco
040000141223         IF  ErrGrave;
040100141223           ErrMessage  = *on;
040200141223           ErrGenerico = *on;
040300141223           V02msg = Msg(01);
040400141223         ENDIF;
040500141223
040600141223       //?Emissione Testata
040700150112         write  KC40T01;
040800141223
040900141223       //?Emissione videata
041000150112         exfmt  KC40D02;
041100141223         reset ErrMessage;
041200141223         reset ErrGenerico;
041300141223         clear V02msg;
041400141223
041500141223         SELECT;
041600141223
041700141223       //?- Errore Grave esco
041800141223           WHEN  ErrGrave;
041900141223             Fine = *on;
042000141223
042100141223       //?- F03=Fine
042200141223           WHEN  dsp_aid = c_F03;
042300141223             exsr F03D02;
042400141223
042500141223       //?- F06=Conferma
042600141223           WHEN  dsp_aid = c_F06;
042700141223             exsr ContrD02;
042800141223             IF  ErrGenerico;
042900141223               leavesr;
043000141223             ENDIF;
043100141223             exsr F06D02;
043200141223
043300141223       //?Invio
043400141223           OTHER;
043500141223             exsr ContrD02;
043600141223             IF  ErrGenerico;
043700141223               leavesr;
043800141223             ENDIF;
043900141223
044000141223         ENDSL;
044100141223
044200141223       ENDSR;
044300141223
044400141223       //--------------------------------------------------------------
044500141223       //?Inizializzazione Videata D02.
044600141223       //--------------------------------------------------------------
044700141223       BEGSR InzD02;
044800141223
044900150223         VisFile      = *off;
045000141223         VisDistretto = *off;
045100141223         VisArea      = *off;
045200150123         VisTOT1      = *off;
045300150123         VisTOT2      = *off;
045400150123         VisTOT3      = *off;
045500150123         VisTOT4      = *off;
045600141024
045700141223       //?Pulizia videata
045800150112         clear KC40D02;
045900141223
046000141223       //?Se errore grave non imposto più niente
046100141223         IF  ErrGrave;
046200141223           leavesr;
046300141223         ENDIF;
046400141223
046500150123       //?Controllo Abilitazioni
046600141223         SELECT;
046700150123       //?AZ-Azienda
046800141223       //?vedo tutte le selezioni
046900141223         WHEN  wabi = 'AZ';
047000141223           VisDistretto = *on;
047100141223           VisArea      = *on;
047200150123       //?DI-Distretto
047300141223       //?NON vedo la selezione per Distretto
047400141223         WHEN  wabi = 'DI';
047500141229           VisArea  = *on;
047600141223         ENDSL;
047700150123
047800150123       //?Imposto la richiesta dei totali in base all' autorizzazione utente
047900150123         SELECT;
048000150123         WHEN  wabi = 'AZ';
048100150123           visTOT1 = *on;
048200150123           visTOT2 = *on;
048300150123           visTOT3 = *on;
048400150123           visTOT4 = *on;
048500150123           V02dtot1 = TotAzienda;
048600150123           V02dtot2 = TotDistretto;
048700150123           V02dtot3 = TotArea;
048800150123           V02dtot4 = TotCommerciale;
048900150123         WHEN  wabi = 'DI';
049000150123           visTOT1 = *on;
049100150123           visTOT2 = *on;
049200150123           visTOT3 = *on;
049300150123           visTOT4 = *off;
049400150123           V02dtot1 = TotDistretto;
049500150123           V02dtot2 = TotArea;
049600150123           V02dtot3 = TotCommerciale;
049700150123           clear V02dtot4;
049800150126         WHEN  wabi = 'RA';
049900150123           visTOT1 = *on;
050000150123           visTOT2 = *on;
050100150123           visTOT3 = *off;
050200150123           visTOT4 = *off;
050300150123           V02dtot1 = TotArea;
050400150123           V02dtot2 = TotCommerciale;
050500150123           clear V02dtot3;
050600150123           clear V02dtot4;
050700150123         OTHER;
050800150123           visTOT1 = *on;
050900150123           visTOT2 = *off;
051000150123           visTOT3 = *off;
051100150123           visTOT4 = *off;
051200150123           V02dtot1 = TotCommerciale;
051300150123           clear V02dtot2;
051400150123           clear V02dtot3;
051500150123           clear V02dtot4;
051600150123           V02tot1 = 'S';
051700150123         ENDSL;
051800150223
051900150223       //?Imposto la richiesta del file se utente di sede
052000150310         IF  DUTpou = 046;
052100150310         //IF  %subst(KNMUS:1:3) = 'EDP';
052200150223           visFile = *on;
052300150223         ENDIF;
052400141223
052500141223       ENDSR;
052600141223
052700141223       //--------------------------------------------------------------
052800141223       //?Gestione tasto funzionale F03 da videata D02
052900141223       //?F03=Fine
053000141223       //--------------------------------------------------------------
053100141223       BEGSR F03D02;
053200141223
053300141223       //?Chiusura del programma
053400141223         Fine = *on;
053500141223
053600141223       ENDSR;
053700141223
053800141223       //--------------------------------------------------------------
053900141223       //?Gestione tasto funzionale F06 da videata D02
054000141223       //?F06=Conferma
054100141223       //--------------------------------------------------------------
054200141223       BEGSR F06D02;
054300141223
054400141223       //?Lancio il report
054500150112         clear TRKC41DS;
054600150107
054700150112         IKC41abi = wabi;
054800141230         IF  V02ncm <> *blanks;
054900150112           IKC41ncm = %dec(V02ncm:7:0);
055000141223         ENDIF;
055100150112         IKC41cdi = V02cdi;
055200141230         IF  V02car <> *blanks;
055300150112           IKC41car = %dec(V02car:3:0);
055400141223         ENDIF;
055500141230         IF  V02cmm <> *blanks;
055600150112           IKC41cmm = %dec(V02cmm:7:0);
055700141223         ENDIF;
055800150112         IKC41clv1 = V02clv1;
055900150112         IKC41clv2 = V02clv2;
056000150112         IKC41clv3 = V02clv3;
056100150112         IKC41clv4 = V02clv4;
056200150112         IKC41clv5 = V02clv5;
056300150216         IKC41objp = V02objp;
056400150216         IKC41objf = V02objf;
056500150123
056600150123       //?Totale 1
056700150123         IF  V02tot1 <> *blanks;
056800150123           IF  V02dtot1 = TotAzienda;
056900150123             IKC41totaz = 'S';
057000150123           ENDIF;
057100150123           IF  V02dtot1 = TotDistretto;
057200150123             IKC41totdi = 'S';
057300150123           ENDIF;
057400150123           IF  V02dtot1 = TotArea;
057500150123             IKC41totra = 'S';
057600150123           ENDIF;
057700150123           IF  V02dtot1 = TotCommerciale;
057800150123             IKC41totcm = 'S';
057900150123           ENDIF;
058000150123         ENDIF;
058100150123       //?Totale 2
058200150123         IF  V02tot2 <> *blanks;
058300150123           IF  V02dtot2 = TotAzienda;
058400150123             IKC41totaz = 'S';
058500150123           ENDIF;
058600150123           IF  V02dtot2 = TotDistretto;
058700150123             IKC41totdi = 'S';
058800150123           ENDIF;
058900150123           IF  V02dtot2 = TotArea;
059000150123             IKC41totra = 'S';
059100150123           ENDIF;
059200150123           IF  V02dtot2 = TotCommerciale;
059300150123             IKC41totcm = 'S';
059400150123           ENDIF;
059500150123         ENDIF;
059600150123       //?Totale 3
059700150123         IF  V02tot3 <> *blanks;
059800150123           IF  V02dtot3 = TotAzienda;
059900150123             IKC41totaz = 'S';
060000150123           ENDIF;
060100150123           IF  V02dtot3 = TotDistretto;
060200150123             IKC41totdi = 'S';
060300150123           ENDIF;
060400150123           IF  V02dtot3 = TotArea;
060500150123             IKC41totra = 'S';
060600150123           ENDIF;
060700150123           IF  V02dtot3 = TotCommerciale;
060800150123             IKC41totcm = 'S';
060900150123           ENDIF;
061000150123         ENDIF;
061100150123       //?Totale 4
061200150123         IF  V02tot4 <> *blanks;
061300150123           IF  V02dtot4 = TotAzienda;
061400150123             IKC41totaz = 'S';
061500150123           ENDIF;
061600150123           IF  V02dtot4 = TotDistretto;
061700150123             IKC41totdi = 'S';
061800150123           ENDIF;
061900150123           IF  V02dtot4 = TotArea;
062000150123             IKC41totra = 'S';
062100150123           ENDIF;
062200150123           IF  V02dtot4 = TotCommerciale;
062300150123             IKC41totcm = 'S';
062400150123           ENDIF;
062500150123         ENDIF;
062600150223
062700150223       //?File
062800150223         IKC41file = V02file;
062900141223
063000150112         kcoaz = 'KC41';
063100150112         kpjbu = TRKC41ds;
063200141223         kbuff = *blanks;
063300141223
063400141223       //?Richiamo interattivo
063500141223         IF  knmus = *all'1';
063600150112           TRKC41C(kpjba);
063700141223       //?Richiamo batch
063800141223         ELSE;
063900141223           BCH10(kpjba);
064000141223         ENDIF;
064100141223
064200141223       //?Chiusura del programma
064300141223         Fine = *on;
064400141223
064500141223       ENDSR;
064600141223
064700141223       //--------------------------------------------------------------
064800141223       //?Controlla Videata D02.
064900141223       //--------------------------------------------------------------
065000141223       BEGSR ContrD02;
065100141223
065200141223         WindDspF = IndDspF;
065300141223         reset WindDspFb;
065400141223         IndDspF  = WindDspF;
065500141223
065600141223       //?Campagna Commerciale Obbligatoria
065700141223         clear V02des;
065800141223         IF  V02ncm = *blanks;
065900141223           ErrMessage  = *on;
066000141223           ErrGenerico = *on;
066100141223           PosCurNCM   = *on;
066200141223           V02msg = Msg(02);
066300141223           leavesr;
066400141223         ENDIF;
066500141223       //?Ricerca Campagna Commerciale
066600141223         IF  %scan('?':V02ncm) > 0;
066700150112           clear TRKC02DS;
066800150112           IKC02ric = 'R';
066900150112           trkc02r (kpjba:trkc02ds);
067000141223           ErrGenerico = *on;
067100141223           PosCurNCM   = *on;
067200150112           V02ncm = %editc(OKC02ncm:'X');
067300141223         ENDIF;
067400141223       //?Accetto solo dati numerici
067500141223         xx = 1;
067600141223         FOR xx by 1 to %len(V02ncm);
067700141223           IF  %subst(V02ncm:xx:1) <> *blanks and
067800141223               %check(Digits:%subst(V02ncm:xx:1)) > *zeros;
067900141223             ErrMessage  = *on;
068000141223             ErrGenerico = *on;
068100141223             PosCurNCM   = *on;
068200141223             V02msg = Msg(02);
068300141223             leavesr;
068400141223           ENDIF;
068500141223         ENDFOR;
068600141223       //?Deve essere una Campagna Commerciale valida
068700141223         CMPncm = %dec(V02ncm:7:0);
068800150112         chain CMPncm TICMP01L;
068900141223         IF  not %found(TICMP01L);
069000141223           ErrMessage  = *on;
069100141223           ErrGenerico = *on;
069200141223           PosCurNCM   = *on;
069300141223           V02msg = Msg(02);
069400141223           leavesr;
069500141223         ENDIF;
069600141223       //?Decodifico Campagna Commerciale
069700141223         V02des = CMPdes;
069800141223       //?Campagna Commerciale già finita
069900141223         IF  CMPdfc < Oggi;
070000141223           ErrMessage  = *on;
070100141223           ErrGenerico = *on;
070200141223           PosCurNCM   = *on;
070300141223           V02msg = %trim(Msg(02)) + '. Già chiusa';
070400141223           leavesr;
070500141223         ENDIF;
070600141223       //?In carico all'Utente
070700141223         IF  CMPfil <> 046 and %lookup(%editc(CMPfil:'X'):POG) = 0;
070800141223           ErrMessage  = *on;
070900141223           ErrGenerico = *on;
071000141223           PosCurNCM   = *on;
071100141223           V02msg = %trim(Msg(02)) + '. Non in gestione';
071200141223           leavesr;
071300141223         ENDIF;
071400141223
071500141223       //?Distretto
071600141223         w001a = V02cdi;
071700141223         clear w050a;
071800141223         exsr Distretto;
071900141223         V02cdi = w001a;
072000141223         V02cdid = w050a;
072100141223         V02msg = wmsg;
072200141223         IF  ErrGenerico;
072300141223           leavesr;
072400141223         ENDIF;
072500141223
072600141223       //?Area
072700141223         w003a = V02car;
072800141223         clear w050a;
072900141223         exsr Area;
073000141223         V02car = w003a;
073100141223         V02card = w050a;
073200141223         V02msg = wmsg;
073300141223         IF  ErrGenerico;
073400141223           leavesr;
073500141223         ENDIF;
073600141223       //?Se presente anche il distretto devono essere congruenti
073700141223         IF  V02cdi <> *blanks and V02car <> *blanks;
073800141223           chain (V02cdi:w0030) AZORG02L;
073900141223           IF  not %found(AZORG02L) or ORGfva <> *blanks;
074000141223             ErrGenerico = *on;
074100141223             ErrMessage  = *on;
074200141223             PosCurCAR   = *on;
074300141223             V02msg = %trim(Msg(04)) + '. Non congruente con il distretto';
074400141223             leavesr;
074500141223           ENDIF;
074600141223         ENDIF;
074700141223
074800141223       //?Commerciale Unificante
074900141223         w007a = V02cmm;
075000141223         clear w050a;
075100141223         exsr Commerciale;
075200141223         V02cmm = w007a;
075300141223         V02cmmd = w050a;
075400141223         V02msg = wmsg;
075500141223         IF  ErrGenerico;
075600141223           leavesr;
075700141223         ENDIF;
075800141223
075900141223       //?Importanza Cliente
076000141223         w001a = V02clv1;
076100141223         clear w050a;
076200141223         exsr ImpCliente;
076300141223         V02clv1 = w001a;
076400141223         V02msg = wmsg;
076500141223         IF  ErrGenerico;
076600141223           PosCurCLV1 = *on;
076700141223           leavesr;
076800141223         ENDIF;
076900141223         w001a = V02clv2;
077000141223         clear w050a;
077100141223         exsr ImpCliente;
077200141223         V02clv2 = w001a;
077300141223         V02msg = wmsg;
077400141223         IF  ErrGenerico;
077500141223           PosCurCLV2 = *on;
077600141223           leavesr;
077700141223         ENDIF;
077800141223         w001a = V02clv3;
077900141223         clear w050a;
078000141223         exsr ImpCliente;
078100141223         V02clv3 = w001a;
078200141223         V02msg = wmsg;
078300141223         IF  ErrGenerico;
078400141223           PosCurCLV3 = *on;
078500141223           leavesr;
078600141223         ENDIF;
078700141223         w001a = V02clv4;
078800141223         clear w050a;
078900141223         exsr ImpCliente;
079000141223         V02clv4 = w001a;
079100141223         V02msg = wmsg;
079200141223         IF  ErrGenerico;
079300141223           PosCurCLV4 = *on;
079400141223           leavesr;
079500141223         ENDIF;
079600141223         w001a = V02clv5;
079700141223         clear w050a;
079800141223         exsr ImpCliente;
079900141223         V02clv5 = w001a;
080000141223         V02msg = wmsg;
080100141223         IF  ErrGenerico;
080200141223           PosCurCLV5 = *on;
080300141223           leavesr;
080400141223         ENDIF;
080500150123
080600150123       //?Stampa totali
080700150123         IF  V02tot1 = *blanks and V02tot2 = *blanks and
080800150123             V02tot3 = *blanks and V02tot4 = *blanks;
080900150123           ErrGenerico = *on;
081000150123           ErrMessage  = *on;
081100150123           PosCurTOT1  = *on;
081200150123           V02msg = Msg(07);
081300150123           leavesr;
081400150123         ENDIF;
081500141223
081600141223       ENDSR;
081700141223
081800141223       //--------------------------------------------------------------
081900141223       //?Controllo Distretto.
082000141223       //--------------------------------------------------------------
082100141223       BEGSR Distretto;
082200141223
082300141223         IF  w001a = *blanks;
082400141223           leavesr;
082500141223         ENDIF;
082600141223
082700141223       //?Ricerca Distretto
082800141223         IF  %scan('?' : w001a) > 0;
082900141223           ErrGenerico = *on;
083000141223           PosCurCDI   = *on;
083100141223           idTabella = '17';
083200141223           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
083300141223                            tastoUscita);
083400141223           w001a = idElemento;
083500141223         ENDIF;
083600141223       //?Deve essere un Distretto valido
083700141223         clear xx;
083800141223         xx = %lookup(w001a : skDistretti);
083900141223         IF  xx = 0;
084000141223           ErrMessage  = *on;
084100141223           ErrGenerico = *on;
084200141223           PosCurCDI   = *on;
084300141223           wmsg = Msg(03);
084400141223           leavesr;
084500141223         ENDIF;
084600141223       //?Decodifico distretto
084700141223         w050a = skDesDist(xx);
084800141223       //?In carico all'Utente
084900141223         IF  %lookup (w001a:DIS) = 0;
085000141223           ErrMessage  = *on;
085100141223           ErrGenerico = *on;
085200141223           PosCurCDI   = *on;
085300141223           wmsg = %trim(Msg(03)) + '. Non in gestione';
085400141223           leavesr;
085500141223         ENDIF;
085600141223
085700141223       ENDSR;
085800141223
085900141223       //--------------------------------------------------------------
086000141223       //?Controllo Area.
086100141223       //--------------------------------------------------------------
086200141223       BEGSR Area;
086300141223
086400141223         IF  w003a = *blanks;
086500141223           leavesr;
086600141223         ENDIF;
086700141223
086800141223       //?Ricerca Area
086900141223         IF  %scan('?' : w003a) > 0;
087000141223           ErrGenerico = *on;
087100141223           PosCurCAR   = *on;
087200141223           idTabella = '05';
087300141223           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
087400141223                          tastoUscita);
087500141223           w003a = idElemento;
087600141223         ENDIF;
087700141223         IF  w003a = *blanks;
087800141223           leavesr;
087900141223         ENDIF;
088000141223       //?Accetto solo dati numerici
088100141223         xx = 1;
088200141223         FOR xx by 1 to %len(w003a);
088300141223           IF  %subst(w003a:xx:1) <> *blanks and
088400141223               %check(Digits:%subst(w003a:xx:1)) > *zeros;
088500141223             ErrMessage  = *on;
088600141223             ErrGenerico = *on;
088700141223             PosCurCAR   = *on;
088800141223             wmsg = Msg(04);
088900141223             leavesr;
089000141223           ENDIF;
089100141223         ENDFOR;
089200141223       //?Deve essere un'Area valida
089300141223         clear xx;
089400141223         w0030 = %dec(w003a:3:0);
089500141223         xx = %lookup(%editc(w0030:'X'):skAree);
089600141223         IF  xx = 0;
089700141223           ErrMessage  = *on;
089800141223           ErrGenerico = *on;
089900141223           PosCurCAR   = *on;
090000141223           wmsg = Msg(04);
090100141223           leavesr;
090200141223         ENDIF;
090300141223       //?Decodifico area
090400141223         w050a = skDesArea(xx);
090500141223       //?Deve essere un'area in carico all'utente
090600141223         IF  %lookup(%editc(w0030:'X'):CAR) = 0;
090700141223           ErrGenerico = *on;
090800141223           ErrMessage  = *on;
090900141223           PosCurCAR   = *on;
091000141223           wmsg = %trim(Msg(04)) + '. Non in gestione';
091100141223           leavesr;
091200141223         ENDIF;
091300141223
091400141223       ENDSR;
091500141223
091600141223       //--------------------------------------------------------------
091700141223       //?Controllo Commerciale.
091800141223       //--------------------------------------------------------------
091900141223       BEGSR Commerciale;
092000141223
092100141223         IF  w007a = *blanks;
092200141223           leavesr;
092300141223         ENDIF;
092400141223
092500141223       //?Ricerca Commerciale
092600141223         IF  %scan('?' : w007a) > 0;
092700141223           clear TRMK43DS;
092800141223           IMK43solu = 'S';
092900141223           IMK43fil = DUTpou;
093000141223           trmk43r (kpjba : TRMK43DS);
093100141223           IF  OMK43err = *off and OMK43fxx = *blanks;
093200141223             w007a = %editc(OMK43cmm:'X');
093300141223             w050a = OMK43des;
093400141223           ENDIF;
093500141223           ErrGenerico = *on;
093600141223           PosCurCMM   = *on;
093700141223         ENDIF;
093800141223       //?Accetto solo dati numerici
093900141223         xx = 1;
094000141223         FOR xx by 1 to %len(w007a);
094100141223           IF  %subst(w007a:xx:1) <> *blanks and
094200141223               %check(Digits:%subst(w007a:xx:1)) > *zeros;
094300141223             ErrMessage  = *on;
094400141223             ErrGenerico = *on;
094500141223             PosCurCMM   = *on;
094600150123             wmsg = Msg(05);
094700141223             leavesr;
094800141223           ENDIF;
094900141223         ENDFOR;
095000141223         w0070 = %dec(w007a:7:0);
095100141223       //?Deve esistere il Commerciale
095200141223         chain (w0070) AZCMM01L;
095300141223         IF  not %found(AZCMM01L);
095400141223           ErrMessage  = *on;
095500141223           ErrGenerico = *on;
095600141223           PosCurCMM   = *on;
095700150123           wmsg = Msg(05);
095800141223           leavesr;
095900141223         ENDIF;
096000141223       //?Decodifico Commerciale
096100141223         w050a = CMMdes;
096200141223       //?Deve essere un Commerciale in carico all'utente
096300141223         clear TNTAA1DS;
096400141223         ITAA1caut = '§utegtc';
096500141223         ITAA1cmm  = w0070;
096600141223         kpjbu = TNTAA1DS;
096700141223         tntaa1c (kpjba);
096800141223         TNTAA1DS = kpjba;
096900141223         IF  OTAA1fabi = 'N';
097000141223           ErrGenerico = *on;
097100141223           ErrMessage  = *on;
097200141223           PosCurCMM   = *on;
097300150123           wmsg = %trim(Msg(05)) + '. Non in gestione';
097400141223           leavesr;
097500141223         ENDIF;
097600141223       //?Deve essere un Commerciale Unificante
097700141223         IF  w0070 <> CMMuni;
097800141223           ErrGenerico = *on;
097900141223           ErrMessage  = *on;
098000141223           PosCurCMM   = *on;
098100150123           wmsg = %trim(Msg(05)) + '. Non è un Commerciale Unificante';
098200141223           leavesr;
098300141223         ENDIF;
098400141223
098500141223       ENDSR;
098600141223
098700141223       //--------------------------------------------------------------
098800141223       //?Controllo Importanza Cliente.
098900141223       //--------------------------------------------------------------
099000141223       BEGSR ImpCliente;
099100141223
099200141223         IF  w001a = *blanks;
099300141223           leavesr;
099400141223         ENDIF;
099500141223
099600141223         IF  %scan('?' : w001a) > 0;
099700141223           ErrGenerico = *on;
099800141223           idTabella = 'IC';
099900141223           Tabel_Ricerca (idTabella : Ordinamento : idElemento :
100000141223                          tastoUscita);
100100141223           w001a = idElemento;
100200141223         ENDIF;
100300141223         IF  %lookup(w001a : skIC) = 0;
100400141223           ErrMessage  = *on;
100500141223           ErrGenerico = *on;
100600141223           PosCurCLV1  = *on;
100700150123           wmsg = Msg(06);
100800141223           leavesr;
100900141223         ENDIF;
101000141223
101100141223       ENDSR;
101200141223
101300141223       //--------------------------------------------------------------
101400141223       //?Operazioni finali.
101500141223       //--------------------------------------------------------------
101600141223       BEGSR RoutEnd;
101700141223
101800141223         *inLR = *on;
101900141223         return;
102000141223
102100141223       ENDSR;
102200141223
102300141223      /end-free
102400141223
102500141223       //--------------------------------------------------------------
102600141223       //?Schiere a tempo di compilazione.
102700141223       //--------------------------------------------------------------
102800141223
102900141223** -- MSG -------------------------------------------------------------------*
103000141223Utente non abilitato alla Funzione.                                            1
103100141223Campagna Commerciale errata                                                    2
103200141223Distretto Errato                                                               3
103300141223Area Errata                                                                    4
103400150126Commerciale Unificante Errato                                                  5
103500150126Codice Importanza Clienti Errato                                               6
103600150123Impostare il totale da stampare                                                7
