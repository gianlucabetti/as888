000100150714      //--------------------------------------------------------------
000200150714      //?FIOR040R - CONFERMA ORM COMMISSIONATI
000300150714      //--------------------------------------------------------------
000400150714
000500150714     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600150717     h dftactgrp(*no) actgrp(*caller)
000700150714
000800150714      //---------------------------------------------------------------
000900150714      //?Dichiarazione file.
001000150714      //---------------------------------------------------------------
001100150714      // - Organigramma
001200150714     fAZORG01L  if   e           k disk
001300150714
001400150714      // - ORM
001500150716     fFNORM01L  if   e           k disk
001600150716     fFNORM13L  if   e           k disk    rename(FNORM000:FNORM13)
001700150716     fFNORP01L  if   e           k disk
001800150714
001900150714      // - Estensione ORM
002000150714     fFNORE01L  if   e           k disk
002100150714
002200150714      // - Video Gestione Campagne
002300150714     fFIOR040D  cf   e             workstn
002400150716     f                                     sfile(OR040S03: S03nrr)
002500150714     f                                     indds(IndDspF)
002600150714     f                                     infds(InfDspF)
002700150714
002800150714      //---------------------------------------------------------------
002900150714      //?Definizione costanti.
003000150714      //---------------------------------------------------------------
003100150714      // - Tasti funzionali a video
003200150714     d c_F01           c                   const(x'31')
003300150714     d c_F02           c                   const(x'32')
003400150714     d c_F03           c                   const(x'33')
003500150714     d c_F04           c                   const(x'34')
003600150714     d c_F05           c                   const(x'35')
003700150714     d c_F06           c                   const(x'36')
003800150714     d c_F07           c                   const(x'37')
003900150714     d c_F08           c                   const(x'38')
004000150714     d c_F09           c                   const(x'39')
004100150714     d c_F10           c                   const(x'3A')
004200150714     d c_F11           c                   const(x'3B')
004300150714     d c_F12           c                   const(x'3C')
004400150714     d c_F13           c                   const(x'B1')
004500150714     d c_F14           c                   const(x'B2')
004600150714     d c_F15           c                   const(x'B3')
004700150714     d c_F16           c                   const(x'B4')
004800150714     d c_F17           c                   const(x'B5')
004900150714     d c_F18           c                   const(x'B6')
005000150714     d c_F19           c                   const(x'B7')
005100150714     d c_F20           c                   const(x'B8')
005200150714     d c_F21           c                   const(x'B9')
005300150714     d c_F22           c                   const(x'BA')
005400150714     d c_F23           c                   const(x'BB')
005500150714     d c_F24           c                   const(x'BC')
005600150714     d c_Enter         c                   const(x'F1')
005700150714     d c_RollDown      c                   const(x'F4')
005800150714     d c_RollUp        c                   const(x'F5')
005900150714
006000150714     d Digits          c                   const('0123456789')
006100150714
006200150714     d c_Conferma      c                   const('*** CONFERMA ORM COMMISSIONAT-
006300150714     d                                     I ***')
006400150714
006500150714      //---------------------------------------------------------------
006600150714      //?Definizione schiere.
006700150714      //---------------------------------------------------------------
006800150714      // - Messaggi di errore
006900150714     d Msg             s             78    dim(10) ctdata perrcd(1)
007000150714
007100150714      //---------------------------------------------------------------
007200150714      //?Definizione aree dati.
007300150714      //---------------------------------------------------------------
007400150714      // - Dati utente
007500150714     d §AzUte        e ds                  extname(AZUTE00F)
007600150714     d                                     dtaara
007700150714     d §DatiUte      e ds                  extname(dDatiUte)
007800150714     d                                     dtaara
007900150714
008000150714      //---------------------------------------------------------------
008100150714      //?Definizione strutture dati.
008200150714      //---------------------------------------------------------------
008300150714      // - Status
008400150714     d Psds           sds
008500150714     d   SDSpgm          *proc
008600150714
008700150714      // - InfDS
008800150714     d InfDspF         ds
008900150714     d  dsp_aid              369    369a
009000150714     d  sfl_rrn              376    377i 0
009100150714     d  min_nrr              378    379i 0
009200150714     d  num_rcds             380    381i 0
009300150714
009400150714      // - Indicatori su DspF
009500150714     d IndDspF         ds
009600150714        // - Indicatori di errore in videata
009700150714     d  ErrMessage                    1n   overlay(IndDspF : 28)
009800150714        // - Indicatori di gestione del subfile
009900150714     d  SflDsp                        1n   overlay(IndDspF : 30)
010000150714     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010100150714     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010200150714     d  SflEnd                        1n   overlay(IndDspF : 33)
010300150714        // - Indicatori di errore
010400150714     d  PosCurPOR                     1n   overlay(IndDspF : 50)
010500150716     d  PosCurOPZ                     1n   overlay(IndDspF : 51)
010600150714        // - Indicatori di errore generico
010700150714     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010800150714
010900150714     d WindDspF        ds                  inz
011000150714     d  WindDspFa              1     49    inz(*zero)
011100150714     d  WindDspFb             50     99    inz(*zero)
011200150714
011300150714      // - Parametri ricevuti
011400150714     d KPJBA         e ds
011500150714
011600150716      // - Dati per richiamo FIOR05R
011700150716     d FIOR05DS      e ds
011800150714
011900150714      // - Ricerca/Controllo tabelle
012000150714     d TIBS02DS      e ds                  inz
012100150714
012200150714      // - Reperimento dati utente
012300150714     d TIBS34DS      e ds
012400150714
012500150714       // - Caricamento Filiali in £6
012600150714     d TRUL06DS      e ds
012700150714     d  L6                     1     90    dim(30)
012800150714
012900150714      // - Reperimento filiali gestite dall'utente
013000150714     d TRUL31DS      e ds
013100150714     d  POG                   10    759    dim(250)
013200150717
013300150717      // - Richiamo pgm scelta stampanti
013400150717     d TRUL90DS      e ds
013500150717
013600150717      // - Dati per immissiona bolla
013700150717     d dBLP          e ds
013800150717     d dTASV         e ds
013900150714
014000150714      // - Tabella LAT - Autorizzazioni
014100150714     d dLAT          e ds
014200150714
014300150714      // ds per rcd 'G' FNORE - generale
014400150714     d dOREgen       e ds
014500150714
014600150714      // - Campo UTEFAF - AZUTE00F
014700150714     d dUTE01        e ds
014800150714
014900150714      // - Parametri da passare a FIOR05R/FIOR07R in kpjbu
015000150714     d FIORDS          DS
015100150714     d  ppoe                          3  0
015200150714     d  pnor                          7  0
015300150714     d  pnsr                          2  0
015400150714     d  pnrv                          2  0
015500150714     d  psce                          1
015600150714     d  pfgs                          3  0
015700150714     d  ppor                          3  0
015800150714     d  pdtndc                        8  0
015900150714     d  pmdo                         10
016000150714     d  ppro                         10
016100150714     d  pdtrist                       8  0
016200150714     d  prmp                          1
016300150714     d  pbrc                          1
016400150714     d  pref                          2
016500150714     d  pmio                          1
016600150714     d  pgas                          1
016700150714     d  pndc                          7  0
016800150714     d  ptpela                        1
016900150714
017000150714      //---------------------------------------------------------------
017100150714      //?Definizione variabili globali.
017200150714      //---------------------------------------------------------------
017300150714      // - Flags booleani
017400150714     d ErrGrave        s               n   inz(*off)
017500150714     d Fine            s               n   inz(*off)
017600150717     d OkBolla         s               n   inz(*off)
017700150716     d Proposte        s               n   inz(*off)
017800150716     d wAuto           s               n   inz(*off)
017900150714     d wEnd            s               n   inz(*off)
018000150716     d wEndP           s               n   inz(*off)
018100150714     d wInzD02         s               n   inz(*off)
018200150716     d wInzS03         s               n   inz(*off)
018300150717     d wPrtf           s               n   inz(*off)
018400150714
018500150714      // - Indici di schiera
018600150714     d xx              s              4s 0 inz
018700150714
018800150714      // - Campi associati al video
018900150714     d Video           s              2a   inz
019000150716     d S03nrr          s              4s 0 inz
019100150717     d sav_S03nrr      s              4s 0 inz
019200150714
019300150714      // - Campi di comodo data
019400150714     d Data_EUR        s               d   datfmt(*eur)
019500150714     d Data_ISO        s               d   datfmt(*iso)
019600150714
019700150714      // - Campi di comodo
019800150717     d cmd             s            512a   varying
019900150714     d DataRitA        s              8s 0 inz
020000150714     d DataRitDa       s              8s 0 inz
020100150714     d Oggi            s              8s 0 inz
020200150714     d wabi            s              2a   inz
020300150714     d wpor            s              3s 0 inz
020400150714     d w0030           s              3s 0 inz
020500150714
020600150714       // - Parametri per ricerca Filiale
020700150714     d pinFIL          s              3
020800150714     d pinFAG          s              1
020900150714     d pinDES          s             25
021000150714     d pinDIT          s              3
021100150714
021200150714      //---------------------------------------------------------------
021300150714      //?Definizione procedure usate.
021400150714      //---------------------------------------------------------------
021500150714       // - Gestione ORM
021600150714     d FIOR05R         pr                  extpgm('FIOR05R')
021700150714     d  kpjba                              likeds(kpjba)
021800150714     d  fior05ds                           likeds(FIOR05DS)
021900150717
022000150717       // - Immissione bolla
022100150717     d FNLS01R         pr                  extpgm('FNLS01R')
022200150717     d  kpjba                              likeds(kpjba)
022300150717     d  dblp                               likeds(dBLP)
022400150717     d  dtasv                              likeds(dTASV)
022500150717     d  trul90ds                           likeds(trul90ds)
022600150714
022700150714       // - Caricamento Filiali in £6
022800150714     d TRUL06R         pr                  extpgm('TRUL06R')
022900150714     d  kpjba                              likeds(kpjba)
023000150714
023100150714       // - Caricamento Filiali in gestione
023200150714     d TRUL31R         pr                  extpgm('TRUL31R')
023300150714     d  kpjba                              likeds(kpjba)
023400150714     d  trul31ds                           likeds(trul31ds)
023500150717
023600150717       // - Pgm scelta stampanti
023700150717     d TRUL90R         pr                  extpgm('TRUL90R')
023800150717     d  kpjba                              likeds(kpjba)
023900150717     d  trul90ds                           likeds(trul90ds)
024000150714
024100150714      //---------------------------------------------------------------
024200150714      //?Definizione Prototipi.
024300150714      //---------------------------------------------------------------
024400150714      /copy gaitrasrc/srcprotopr,TIBS02R
024500150714      /copy gaitrasrc/srcprotopr,TIBS34R
024600150714      /copy gaitrasrc/srcprotopr,TNSD24R
024700150717      /copy gaitrasrc/srcprotopr,SYSTEM
024800150714
024900150714      //---------------------------------------------------------------
025000150714      //?Definizione key-list.
025100150714      //---------------------------------------------------------------
025200150714       // - File TABEL00F
025300150714     d k03tabel      e ds                  extname(TABEL00F:*key)
025400150714     d                                     prefix(k_)
025500150714
025600150714      //---------------------------------------------------------------
025700150714      //?M A I N - L I N E
025800150714      //---------------------------------------------------------------
025900150714
026000150714     c     *Entry        plist
026100150714     c                   parm                    kpjba
026200150714
026300150714      /free
026400150714
026500150714       //?Operazioni iniziali
026600150714       exsr RoutInz;
026700150714
026800150714       //?Gestione video
026900150714       DOW  Fine = *off;
027000150714         SELECT;
027100150714         WHEN  Video = 'D2';
027200150714           exsr GesD02;
027300150716         WHEN  Video = 'S3';
027400150716           exsr GesS03;
027500150714         OTHER;
027600150714           Fine = *on;
027700150714         ENDSL;
027800150714       ENDDO;
027900150714
028000150714       //?Operazioni finali
028100150714       exsr RoutEnd;
028200150714
028300150714       //--------------------------------------------------------------
028400150714       //?Operazioni iniziali.
028500150714       //--------------------------------------------------------------
028600150714       BEGSR RoutInz;
028700150714
028800150714       //?Reperimento dati job
028900150714         exsr DatiJob;
029000150714
029100150714       //?Impostazione campi "fissi"
029200150714         V01pgm = SDSpgm;
029300150714         Video = 'D2';
029400150714         wInzD02 = *on;
029500150714
029600150714       //?Imposto oggi
029700150714         Oggi = %dec(%date());
029800150714
029900150714       //?Imposto oggi + 2 gg
030000150714         Data_ISO = %date(Oggi);
030100150717         Data_ISO = Data_ISO + %days(10);
030200150714         DataRitA = %dec(Data_ISO);
030300150714
030400150717       //?Imposto oggi - 4 mese
030500150714         Data_ISO = %date(Oggi);
030600150717         Data_ISO = Data_ISO - %months(4);
030700150714         DataRitDa = %dec(Data_ISO);
030800150714
030900150714       //?Imposto il titolo
031000150714         V01tit = c_Conferma;
031100150714
031200150714       //?Se ho già un errore grave esco
031300150714         IF  ErrGrave;
031400150714           leavesr;
031500150714         ENDIF;
031600150714
031700150714       //?Carico le filiali abilitate all'utente
031800150714         clear TRUL31DS;
031900150714         I31abi = wabi;
032000150714         I31cdi = DUTdis;
032100150714         I31car = DUTare;
032200150714         I31cpo = DUTpou;
032300150714         TRUL31R (kpjba:trul31ds);
032400150714         IF O31pog <= *zeros;
032500150714           ErrGrave = *on;
032600150714           leavesr;
032700150714         ENDIF;
032800150714
032900150714       ENDSR;
033000150714
033100150714       //--------------------------------------------------------------
033200150714       //?Reperimento Dati del job (Utente/Operativi).
033300150714       //--------------------------------------------------------------
033400150714       BEGSR DatiJob;
033500150714
033600150714         in(E) §AzUte;
033700150714         IF  NOT %error;
033800150714           in(E) §DatiUte;
033900150714         ENDIF;
034000150714         IF  %error or RSut = *blanks;
034100150714           clear TIBS34ds;
034200150714           tibs34r(tibs34ds);
034300150714           in §AzUte;
034400150714           in §DatiUte;
034500150714         ENDIF;
034600150714
034700150714         clear wabi;
034800150714         clear dLAT;
034900150714         dUTE01 = UTEfaf;
035000150714
035100150714       //?Controllo autorizzazioni profilo
035200150714         SELECT;
035300150714         WHEN  DUTerr = 'E';
035400150714           ErrGrave = *on;
035500150714           leavesr;
035600150714         WHEN  §UTEorm <> *blanks;
035700150714           wabi = §UTEorm;
035800150714         WHEN  UTEaut <> *blanks;
035900150714           wabi = UTEaut;
036000150714         OTHER;
036100150714           IF  DUTlpo = '1';
036200150714             wabi = 'TP';
036300150714           ENDIF;
036400150714           IF  DUTlpo = '2';
036500150714             wabi = 'PO';
036600150714           ENDIF;
036700150714         ENDSL;
036800150714
036900150714       //?Controllo se l'autorizzazione è valida
037000150714         clear TIBS02DS;
037100150714         T02mod = 'C';
037200150714         T02sif = knsif;
037300150714         T02cod = 'LAT';
037400150714         T02ke1 = wabi;
037500150714         TNTBE_RicercaControllo (kpjba:tibs02ds);
037600150714         IF  T02err <> *blanks or §LATabi = 'S';
037700150714           ErrGrave = *on;
037800150714           leavesr;
037900150714         ENDIF;
038000150714         dLAT = T02uni;
038100150714
038200150714       ENDSR;
038300150714
038400150714       //--------------------------------------------------------------
038500150714       //?Gestione videata D02.
038600150714       //--------------------------------------------------------------
038700150714       BEGSR GesD02;
038800150714
038900150714       //?Inizializzazione videata
039000150714         IF  wInzD02;
039100150714           exsr InzD02;
039200150714           wInzD02 = *off;
039300150714         ENDIF;
039400150714
039500150714       //?Se errore grave emetto messaggio poi esco
039600150714         IF  ErrGrave;
039700150714           ErrMessage  = *on;
039800150714           ErrGenerico = *on;
039900150714           V02msg = Msg(01);
040000150714         ENDIF;
040100150714
040200150714       //?Emissione Testata
040300150714         write  OR040T01;
040400150714
040500150714       //?Emissione videata
040600150714         exfmt  OR040D02;
040700150714         reset ErrMessage;
040800150714         reset ErrGenerico;
040900150714         clear V02msg;
041000150714
041100150714         SELECT;
041200150714
041300150714       //?- Errore Grave esco
041400150714         WHEN  ErrGrave;
041500150714           Fine = *on;
041600150714
041700150714       //?- F03=Fine
041800150714         WHEN  dsp_aid = c_F03;
041900150714           exsr F03D02;
042000150714
042100150714       //?Invio
042200150714         OTHER;
042300150714           exsr ContrD02;
042400150714           IF  ErrGenerico;
042500150714             leavesr;
042600150714           ENDIF;
042700150714         //?Aggiorno gli ORM
042800150714           exsr Aggiorna;
042900150714
043000150714         ENDSL;
043100150714
043200150714       ENDSR;
043300150714
043400150714       //--------------------------------------------------------------
043500150714       //?Inizializzazione Videata D02.
043600150714       //--------------------------------------------------------------
043700150714       BEGSR InzD02;
043800150714
043900150714       //?Pulizia videata
044000150714         clear OR040D02;
044100150714
044200150714       //?Imposto la filiale utente come Filiale Ritiro
044300150714         V02por = %editc(DUTpou:'X');
044400150714         clear V02pord;
044500150714
044600150714       //?Decodifico filiale
044700150714         wpor = %dec(V02por:3:0);
044800150714         chain wpor AZORG01L;
044900150714         IF  %found(AZORG01L);
045000150714           V02pord = ORGdes;
045100150714         ENDIF;
045200150714
045300150714       ENDSR;
045400150714
045500150714       //--------------------------------------------------------------
045600150714       //?Gestione tasto funzionale F03 da videata D02
045700150714       //?F03=Fine
045800150714       //--------------------------------------------------------------
045900150714       BEGSR F03D02;
046000150714
046100150714       //?Chiusura del programma
046200150714         Fine = *on;
046300150714
046400150714       ENDSR;
046500150714
046600150714       //--------------------------------------------------------------
046700150714       //?Controlla Videata D02.
046800150714       //--------------------------------------------------------------
046900150714       BEGSR ContrD02;
047000150714
047100150714         WindDspF = IndDspF;
047200150714         reset WindDspFb;
047300150714         IndDspF  = WindDspF;
047400150714
047500150714         IF  V02por = *blanks;
047600150714           ErrMessage  = *on;
047700150714           ErrGenerico = *on;
047800150714           PosCurPOR = *on;
047900150714           V02msg = Msg(02);
048000150714           leavesr;
048100150714         ENDIF;
048200150714
048300150714       //?Ricerca Filale
048400150714         IF  %scan('?' : V02por) > 0;
048500150714           clear pinFIL;
048600150714           clear pinFAG;
048700150714           clear pinDES;
048800150714           clear pinDIT;
048900150714           tnsd24r (pinFIL:pinFAG:pinDES:pinDIT);
049000150714           IF pinFIL > *zeros;
049100150714             V02por = pinFIL;
049200150714             V02pord = pinDES;
049300150714           ENDIF;
049400150714           ErrGenerico = *on;
049500150714           PosCurPOR = *on;
049600150714         ENDIF;
049700150714         IF  V02por = *blanks;
049800150714           leavesr;
049900150714         ENDIF;
050000150714       //?Accetto solo dati numerici
050100150714         xx = 1;
050200150714         FOR xx by 1 to %len(V02por);
050300150714           IF  %subst(V02por:xx:1) <> *blanks and
050400150714               %check(Digits:%subst(V02por:xx:1)) > *zeros;
050500150714             ErrMessage  = *on;
050600150714             ErrGenerico = *on;
050700150714             PosCurPOR = *on;
050800150714             V02msg = Msg(02);
050900150714             leavesr;
051000150714           ENDIF;
051100150714           IF  %subst(V02por:xx:1) = *blanks;
051200150714             %subst(V02por:xx:1) = '0';
051300150714           ENDIF;
051400150714         ENDFOR;
051500150714
051600150714       //?Deve esistere la Filiale
051700150714         wpor = %dec(V02por:3:0);
051800150714         chain wpor AZORG01L;
051900150714         IF  not %found(AZORG01L) or ORGfva <> *blanks;
052000150714           ErrMessage  = *on;
052100150714           ErrGenerico = *on;
052200150714           PosCurPOR = *on;
052300150714           V02msg = Msg(02);
052400150714           leavesr;
052500150714         ENDIF;
052600150714       //?Decodifico filiale
052700150714         V02pord = ORGdes;
052800150714
052900150714       //?Utente abilitato alla filiale
053000150714         IF  %lookup(V02por:pog) = 0;
053100150714           ErrMessage  = *on;
053200150714           ErrGenerico = *on;
053300150714           PosCurPOR = *on;
053400150714           V02msg = Msg(03);
053500150714           leavesr;
053600150714         ENDIF;
053700150714
053800150714       ENDSR;
053900150714
054000150714       //--------------------------------------------------------------
054100150714       //?Aggiorno gli ORM.
054200150714       //--------------------------------------------------------------
054300150714       BEGSR Aggiorna;
054400150714
054500150714       //?Per prima cosa devo confermare gli ORM con alert
054600150714       //?e lo faccio in maniera automatica
054700150714         exsr Leggi_ORM;
054800150716
054900150716       //?Non devo più fare la conferma automatica
055000150716         IF  not wAuto;
055100150716           wAuto = *on;
055200150716         ENDIF;
055300150716
055400150716       //?Carico il subfile
055500150716         Video = 'S3';
055600150716         wInzS03 = *on;
055700151006         clear sav_s03nrr;
055800150714
055900150714       ENDSR;
056000150714
056100150714       //--------------------------------------------------------------
056200150714       //?Leggo gli ORM in fase 50.
056300150714       //--------------------------------------------------------------
056400150714       BEGSR Leggi_ORM;
056500150714
056600150714         wEnd = *off;
056700150714
056800150714       //?Leggo gli ORM in £6 delle filiale ritiro richiesta
056900150714         clear TRUL06DS;
057000150714         D06cod = '£6';
057100150714         D06key = V02por;
057200150714         kpjbu = TRUL06DS;
057300150714         trul06r (kpjba);
057400150714         TRUL06DS = kpjbu;
057500150714
057600150714       //?Ciclo la lettura per £6 e data ritiro "da"
057700150714         xx = 1;
057800150714         FOR xx by 1 to %elem(L6);
057900150714           IF  L6(xx) = *blanks or L6(xx) = *zeros;
058000150714             iter;
058100150714           ENDIF;
058200150714           w0030 = %dec(L6(xx):3:0);
058300150714           setll (w0030:DataRitDa) FNORM13L;
058400150714           reade w0030 FNORM13L;
058500150714           DOW  not wEnd;
058600150714             IF  %eof(FNORM13L);
058700150714               wEnd = *on;
058800150714               leavesr;
058900150714             ENDIF;
059000150714             IF  ORMdar > DataRitA;
059100150714               wEnd = *on;
059200150714               leavesr;
059300150714             ENDIF;
059400150716
059500150716           //?Solo gli ORM in fase 50
059600150716           //?e no RC
059700150716             IF  ORMfao = 50;
059800150714
059900150716             //?Se devo ancora fare la conferma automatica e
060000150716             //?se hanno alert li confermo in modo automatico
060100150716             //?dato che non si deve telefonare
060200150716               IF  not wAuto;
060300150716                 exsr  Conf_Alert;
060400150716               ENDIF;
060500150716
060600150716             //?Se ho già fatto la conferma automatica
060700150716             //?carico il subfile con i rimanenti per poi confermarli a mano
060800150716             //?dato che si deve telefonare
060900150716               IF  wAuto;
061000150716                 exsr  CarS03;
061100151109                 IF  wEnd;
061200151109                   leavesr;
061300151109                 ENDIF;
061400150716               ENDIF;
061500150716
061600150716             ENDIF;
061700150714
061800150714             reade w0030 FNORM13L;
061900150714           ENDDO;
062000150714
062100150714         ENDFOR;
062200150714
062300150714       ENDSR;
062400150714
062500150714       //--------------------------------------------------------------
062600150714       //?Conferma ORM Commissionato con Alert.
062700150714       //--------------------------------------------------------------
062800150714       BEGSR Conf_Alert;
062900150714
063000150714       //?Aggancio rcd G di FNORE00F
063100150714         OREtrc = 'G';
063200150714         clear dOREgen;
063300150714         chain (ORMpoe:ORMnsr:ORMnor:ORMnrv:OREtrc) FNORE01L;
063400150714         IF  %found(FNORE01L);
063500150714           dOREgen = OREdati;
063600150714         ENDIF;
063700150714
063800150714       //?Se c'è Alert richiamo FIOR05R nascosto
063900150714         IF  §ORefimo = 'S' or §OREfiso = 'S';
064000150714           clear FIOR05DS;
064100150714           clear FIORDS;
064200150714           §RMfpr = 'O';
064300150714           §RMtla = '0';
064400150714           ppoe = ORMpoe;
064500150714           pnsr = ORMnsr;
064600150714           pnor = ORMnor;
064700150714           pnrv = ORMnrv;
064800150714           psce = 'C';
064900150714           kpjbu = FIORDS;
065000150714           fior05r (kpjba:FIOR05DS);
065100150714         ENDIF;
065200150714
065300150714       ENDSR;
065400150716
065500150716       //--------------------------------------------------------------
065600150716       //?Gestione videata S03.
065700150716       //--------------------------------------------------------------
065800150716       BEGSR GesS03;
065900150716
066000150716       //?Inizializzazione videata
066100150716         IF  wInzS03;
066200150716           exsr InzS03;
066300150716           wInzS03 = *off;
066400150716         ENDIF;
066500150716
066600150716       //?Visualizzazione del SFL (se ci sono dati)
066700150716         SflDsp = (S03nrr <= *zeros);
066800150716
066900150716       //?Posizionamento cursore
067000150716         V03rcd = V03csr;
067100150716
067200150716       //?Emissione Testata e Piede con tasti funzionali abilitati
067300150716         write  OR040T01;
067400150716         write  OR040P03;
067500150716
067600150716       //?Emissione videata
067700150716         exfmt  OR040C03;
067800150716         reset ErrMessage;
067900150716         reset ErrGenerico;
068000150716         clear V03msg;
068100150716
068200150716         SELECT;
068300150716
068400150716       //?- F03=Fine
068500150716         WHEN  dsp_aid = c_F03;
068600150716           exsr F03D02;
068700150716
068800150716       //?- F12=Ritorno
068900150716         WHEN  dsp_aid = c_F12;
069000150716           exsr F12S03;
069100150716
069200150716       //?Invio
069300150716         OTHER;
069400150716           IF  V03csr > *zero;
069500150716             exsr OpzS03;
069600150716             IF  ErrGenerico;
069700150716               leavesr;
069800150716             ENDIF;
069900150716           ENDIF;
070000150716
070100150716         ENDSL;
070200150716
070300150716       ENDSR;
070400150716
070500150716       //--------------------------------------------------------------
070600150716       //?Inizializzazione videata S03.
070700150716       //--------------------------------------------------------------
070800150716       BEGSR InzS03;
070900150716
071000150716       //?Pulizia subfile
071100150716         exsr PulS03;
071200150716
071300150716       //?Caricamento subfile
071400150716         exsr Leggi_ORM;
071500150716
071600150716         SflEnd = *on;
071700150716
071800150716       //?Imposto il posizionamento al primo rcd
071900150716         IF  S03nrr > 0;
072000150716           V03rcd = 1;
072100150716         ELSE;
072200150716           clear V03rcd;
072300150716         ENDIF;
072400150717
072500150717         V03csr = V03rcd;
072600150717
072700150717         IF  sav_S03nrr > 0;
072800150921           IF  sav_s03nrr <= S03nrr;
072900150921             V03csr = sav_S03nrr;
073000150921             clear sav_S03nrr;
073100150921           ELSE;
073200150921             V03csr = S03nrr;
073300150921           ENDIF;
073400150717         ENDIF;
073500150716
073600150716       ENDSR;
073700150716
073800150716       //--------------------------------------------------------------
073900150716       //?Pulizia Subfile S03.
074000150716       //--------------------------------------------------------------
074100150716       BEGSR PulS03;
074200150716
074300150716       //?Pulizia subfile
074400150716         SflDsp    = *on;
074500150716         SflDspCtl = *on;
074600150716         write OR040C03;
074700150716         SflDspCtl = *off;
074800150716         SflEnd    = *off;
074900150716
075000150716         clear V03rcd;
075100150716         clear V03csr;
075200150716         clear S03nrr;
075300150716         clear V03msg;
075400150716
075500150716         ErrMessage  = *off;
075600150716         ErrGenerico = *off;
075700150716
075800150716         WindDspF = IndDspF;
075900150716         reset WindDspFb;
076000150716         IndDspF  = WindDspF;
076100150716
076200150716         V03por = wpor;
076300150716         V03pord = V02pord;
076400150716
076500150716       ENDSR;
076600150716
076700150716       //--------------------------------------------------------------
076800150716       //?Carico i dati nel Subfile S03.
076900150716       //--------------------------------------------------------------
077000150716       BEGSR CarS03;
077100150716
077200150716         clear  OR040S03;
077300150716         PosCurOPZ = *off;
077400150716
077500150716       //?ORM
077600150716         V03poenor = ORMpoe;
077700150716         V03nsr = ORMnsr;
077800150716         V03nor = ORMnor;
077900150716         V03nrv = ORMnrv;
078000150716       //?Mittente
078100150716         V03rsr = ORMrsr;
078200150716         V03lor = ORMlor;
078300150716         V03prr = ORMprr;
078400150716       //?Filiale Emissione/Ritiro
078500150716         V03poe = ORMpoe;
078600150716       //?Data Ritiro
078700150716         V03dar = %subst(%editc(ORMdar:'X'):7:2) + '/' +
078800150716                  %subst(%editc(ORMdar:'X'):5:2) + '/' +
078900150716                  %subst(%editc(ORMdar:'X'):3:2);
079000150716       //?Voce e Qtà
079100150716         SELECT;
079200150716         WHEN  ORMncl <> 0;
079300150716           V03voc = 'COL';
079400150716           V03qta = ORMncl;
079500150716         WHEN  ORMpkg <> 0;
079600150716           V03voc = 'PKG';
079700150716           V03qta = ORMpkg;
079800150716         WHEN  ORMvlm <> 0;
079900150716           V03voc = 'VLM';
080000150716           V03qta = ORMvlm;
080100150716         WHEN  ORMbnc <> 0;
080200150716           V03voc = 'BNC';
080300150716           V03qta = ORMbnc;
080400150716         WHEN  ORMblc <> 0;
080500150716           V03voc = 'BLC';
080600150716           V03qta = ORMblc;
080700150716         WHEN  ORMatt <> 0;
080800150716           V03voc = 'AUT';
080900150716           V03qta = ORMatt;
081000150716         WHEN  ORMmtc <> 0;
081100150716           V03voc = 'MOT';
081200150716           V03qta = ORMmtc;
081300150716         OTHER;
081400150716           clear V03voc;
081500150716           clear V03qta;
081600150716         ENDSL;
081700150717       //?Tipo ORM
081800150717         V03tor = ORMtor;
081900150716
082000150716         S03nrr += 1;
082100150716
082200151109         monitor;
082300150716         write  OR040S03;
082400151109         on-error;
082500151109         dump(A);
082600151109         wEnd = *on;
082700151109         endmon;
082800150716
082900150716       ENDSR;
083000150716
083100150716       //--------------------------------------------------------------
083200150716       //?Gestione tasto funzionale F12 da videata S03
083300150716       //?F12=Ritorno
083400150716       //--------------------------------------------------------------
083500150716       BEGSR F12S03;
083600150716
083700150717         clear sav_s03nrr;
083800150716
083900150716       //?Ritorno alle selezioni
084000150716         Video = 'D2';
084100151111
084200151111       //?Pulizia subfile
084300151111         exsr PulS03;
084400150716
084500150716       ENDSR;
084600150716
084700150716       //--------------------------------------------------------------
084800150716       //?Gestione opzioni Subfile S03.
084900150716       //--------------------------------------------------------------
085000150716       BEGSR OpzS03;
085100150716
085200150716         readc OR040S03;
085300150716
085400150716         DOW  NOT  %eof(FIOR040D);
085500150716
085600150716           SflNxtChg = *off;
085700150716           WindDspF  = IndDspF;
085800150716           reset WindDspFb;
085900150716           IndDspF   = WindDspF;
086000150716           V03rcd    = S03nrr;
086100150716
086200150716           PosCurOPZ = *off;
086300150717           sav_S03nrr = S03nrr;
086400150716
086500150716         //?- Se ok l'opzione
086600150716           IF  not ErrMessage;
086700150716
086800150716             SELECT;
086900150716           //?- Nessuna opzione
087000150716             WHEN  V03opz = *blank;
087100150716
087200150716           //?- 1 = Scelta
087300150716             WHEN  V03opz = '1';
087400150716               exsr Opzione1;
087500150716
087600150716             OTHER;
087700150716               ErrMessage  = *on;
087800150716               ErrGenerico = *on;
087900150716               PosCurOPZ   = *on;
088000150716               V03msg      = Msg(04);
088100150716             ENDSL;
088200150716           ENDIF;
088300150716
088400150716           //?Aggiornamento sfl
088500150716           SELECT;
088600150716           WHEN  ErrMessage;
088700150716             SflNxtChg = *on;
088800150716             V03csr    = V03rcd;
088900150716           WHEN  ErrGenerico;
089000150716             V03csr = V03rcd;
089100150716             clear V03opz;
089200150716           OTHER;
089300150716             V03csr = V03rcd;
089400150716             clear V03opz;
089500150716           ENDSL;
089600150716
089700150716           update OR040S03;
089800150716
089900150716           IF  ErrMessage or ErrGenerico;
090000150716             leave;
090100150716           ENDIF;
090200150716
090300150716           readc OR040S03;
090400150716
090500150716         ENDDO;
090600150717
090700150717         IF  ErrMessage or ErrGenerico;
090800150717           leavesr;
090900150717         ENDIF;
091000150717
091100150717         wInzS03 = *on;
091200150716
091300150716       ENDSR;
091400150716
091500150716       //--------------------------------------------------------------
091600150716       //?Opzione "1" videata S03.
091700150716       //--------------------------------------------------------------
091800150716       BEGSR Opzione1;
091900150717
092000150717       //?Prima vanno confermate le proposte di variazione
092100150717         exsr ContrProposte;
092200150717         IF  Proposte;
092300150717           ErrGenerico = *on;
092400150717           ErrMessage  = *on;
092500150717           PosCurOPZ   = *on;
092600150717           V03msg      = Msg(07);
092700150717           V03opz      = 'E';
092800150717           leavesr;
092900150717         ENDIF;
093000150717
093100150717       //?Se è un prepagato e NON è un orm locale
093200150717       //?prima devo emettere la bolla prepagato
093300150717         IF  %lookup(%editc(V03poe:'X'):L6) = 0 and V03tor = 'P';
093400150717           exsr ImmettiPrepagato;
093500150717           IF  not OkBolla;
093600150717             ErrGenerico = *on;
093700150717             ErrMessage  = *on;
093800150717             PosCurOPZ   = *on;
093900150717             V03msg      = Msg(08);
094000150717             V03opz      = 'E';
094100150717             leavesr;
094200150717           ENDIF;
094300150717         ENDIF;
094400150716
094500150716       //?Deve essere ancora in fase 50
094600150717       //?e non allocato
094700150717         chain(e) (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
094800150717         IF  %error;
094900150717           ErrGenerico = *on;
095000150717           ErrMessage  = *on;
095100150717           PosCurOPZ   = *on;
095200150717           V03msg      = Msg(09);
095300150717           V03opz      = 'E';
095400150717           leavesr;
095500150717         ENDIF;
095600150717         IF  %found(FNORM01L) and ORMfao <> 50;
095700150717           ErrGenerico = *on;
095800150717           ErrMessage  = *on;
095900150717           PosCurOPZ   = *on;
096000150717           V03msg      = Msg(06);
096100150717           V03opz      = 'E';
096200150717           leavesr;
096300150717         ENDIF;
096400150716
096500150716         clear FIOR05DS;
096600150716         clear FIORDS;
096700150717         §RMfpr = 'O';
096800150717         §RMtla = '2';
096900150716         ppoe = V03poe;
097000150716         pnsr = V03nsr;
097100150716         pnor = V03nor;
097200150716         pnrv = V03nrv;
097300150716         psce = '2';
097400150716         kpjbu = FIORDS;
097500150716         FIOR05R (kpjba:FIOR05DS);
097600150716         FIORDS = kpjbu;
097700150716
097800150716       ENDSR;
097900150716
098000150716       //--------------------------------------------------------------
098100150716       //?Controllo se ci sono proposte variazione ORM.
098200150716       //--------------------------------------------------------------
098300150716       BEGSR ContrProposte;
098400150716
098500150716         wEndP = *off;
098600150716         Proposte = *off;
098700150716         setll (V03poe:V03nsr:V03nor:V03nrv) FNORP01L;
098800150716         reade (V03poe:V03nsr:V03nor:V03nrv) FNORP01L;
098900150716         DOW  not wEndP;
099000150716           IF  %eof(FNORP01L);
099100150716             leave;
099200150716           ENDIF;
099300150716           IF  ORPfev = *blanks;
099400150716             Proposte = *on;
099500150716             wEndP = *on;
099600150716           ENDIF;
099700150716           reade (V03poe:V03nsr:V03nor:V03nrv) FNORP01L;
099800150716         ENDDO;
099900150716
100000150716       ENDSR;
100100150717
100200150717       //--------------------------------------------------------------
100300150717       //?Immissione Bolla Prepagato.
100400150717       //--------------------------------------------------------------
100500150717       BEGSR ImmettiPrepagato;
100600150717
100700150717         clear kpjbu;
100800150717
100900150717       //?Richiamo il programma se mai richiamato prima
101000150717         IF  not wPrtf;
101100150717           clear TRUL90DS;
101200150717           D90rse = 'S';
101300150717           D90rsb = 'S';
101400150717           trul90r (kpjba:TRUL90DS);
101500150717
101600150717           IF  D90f3 = '1';
101700150717             leavesr;
101800150717           ENDIF;
101900150717
102000150717       //?Ovrprtf segnacolli
102100150717           cmd = 'OVRPRTF file(FNLV22P) outq('+
102200150717                 %trim(D90pre) + ') formtype('''+
102300150717                 %trim(D90mde) + ''') share(*yes)';
102400150717           IF  ExecuteCommand (cmd) = 0;
102500150717           ELSE;
102600150717             leavesr;
102700150717           ENDIF;
102800150717
102900150717       //?Ovrprtf Bolle A4
103000150717           cmd = 'OVRPRTF file(FNLSB5PA4) outq('+
103100150717                 %trim(D90prb4) + ') formtype('''+
103200150717                 %trim(D90mdb4) + ''') share(*yes) +
103300150717                 usrdta(''Prepag_A4'')';
103400150717           IF  ExecuteCommand (cmd) = 0;
103500150717           ELSE;
103600150717             leavesr;
103700150717           ENDIF;
103800150717
103900150717       //?Ovrprtf Bolle A5
104000150717           cmd = 'OVRPRTF file(FNLSB5PA5) outq('+
104100150717                 %trim(D90prb5) + ') formtype('''+
104200150717                 %trim(D90mdb5) + ''') share(*yes) +
104300150717                 usrdta(''Prepag_A5'')';
104400150717           IF  ExecuteCommand (cmd) = 0;
104500150717           ELSE;
104600150717             leavesr;
104700150717           ENDIF;
104800150717
104900150717           wPrtf = *on;
105000150717         ENDIF;
105100150717
105200150717       //?Richiamo immissione prepagati
105300150717         clear kpjbu;
105400150717         clear dTASV;
105500150717         clear dBLP;
105600150717         chain (V03poe:V03nsr:V03nor:V03nrv) FNORM01L;
105700150717         IF  %found(FNORM01L);
105800150717           §LPfpr = 'O';
105900150717           §LPpoe = ORMpoe;
106000150717           §LPnsr = ORMnsr;
106100150717           §LPnor = ORMnor;
106200150717           §LPnrv = ORMnrv;
106300150717         ENDIF;
106400150717
106500150717         fnls01r (kpjba:dBLP:dTASV:TRUL90DS);
106600150717
106700150717         OkBolla = *on;
106800150717
106900150717         IF  §LPfpr = '4' or §LPfpr = '3' or §LPfpr = '5';
107000150717           OkBolla = *off;
107100150717         ENDIF;
107200150717
107300150717       ENDSR;
107400150714
107500150714       //--------------------------------------------------------------
107600150714       //?Operazioni finali.
107700150714       //--------------------------------------------------------------
107800150714       BEGSR RoutEnd;
107900150714
108000150714         *inLR = *on;
108100150714         return;
108200150714
108300150714       ENDSR;
108400150714
108500150714      /end-free
108600150714
108700150714       //--------------------------------------------------------------
108800150714       //?Schiere a tempo di compilazione.
108900150714       //--------------------------------------------------------------
109000150714
109100150714** -- MSG -------------------------------------------------------------------*
109200150714Utente non abilitato alla Funzione.                                            1
109300150714Filiale errata.                                                                2
109400150714Utente non abilitato alla Filiale.                                             3
109500150716Opzione non valida                                                             4
109600150716ORM non in gestione alla Filiale                                               5
109700150716ORM già confermato                                                             6
109800150716Prima confermare le proposte di variazione.                                    7
109900150717Non è stato creato il prepagato                                                8
110000150717ORM in uso da altro utente riprovare più tardi                                 9
