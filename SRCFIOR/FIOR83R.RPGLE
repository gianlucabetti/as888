000100140514      //--------------------------------------------------------------
000200140514      //?FIOR83R - Elenco Clienti con ORM commissionati
000300140514      //--------------------------------------------------------------
000400140514
000500140514     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600140514     h dftactgrp(*no) actgrp('FIOR83') bnddir('TRUL')
000700140514
000800140514      //---------------------------------------------------------------
000900140514      //?Dichiarazione file.
001000140514      //---------------------------------------------------------------
001100140514      // - Organigramma
001200140514     fAZORG01L  if   e           k disk
001300140514
001400140514      // - Video
001500140514     fFIOR83D   cf   e             workstn
001600140514     f                                     indds(IndDspF)
001700140514     f                                     infds(InfDspF)
001800140514
001900140514      //---------------------------------------------------------------
002000140514      //?Definizione costanti.
002100140514      //---------------------------------------------------------------
002200140514      // - Tasti funzionali a video
002300140514     d c_F01           c                   const(x'31')
002400140514     d c_F02           c                   const(x'32')
002500140514     d c_F03           c                   const(x'33')
002600140514     d c_F04           c                   const(x'34')
002700140514     d c_F05           c                   const(x'35')
002800140514     d c_F06           c                   const(x'36')
002900140514     d c_F07           c                   const(x'37')
003000140514     d c_F08           c                   const(x'38')
003100140514     d c_F09           c                   const(x'39')
003200140514     d c_F10           c                   const(x'3A')
003300140514     d c_F11           c                   const(x'3B')
003400140514     d c_F12           c                   const(x'3C')
003500140514     d c_F13           c                   const(x'B1')
003600140514     d c_F14           c                   const(x'B2')
003700140514     d c_F15           c                   const(x'B3')
003800140514     d c_F16           c                   const(x'B4')
003900140514     d c_F17           c                   const(x'B5')
004000140514     d c_F18           c                   const(x'B6')
004100140514     d c_F19           c                   const(x'B7')
004200140514     d c_F20           c                   const(x'B8')
004300140514     d c_F21           c                   const(x'B9')
004400140514     d c_F22           c                   const(x'BA')
004500140514     d c_F23           c                   const(x'BB')
004600140514     d c_F24           c                   const(x'BC')
004700140514     d c_Enter         c                   const(x'F1')
004800140514     d c_RollDown      c                   const(x'F4')
004900140514     d c_RollUp        c                   const(x'F5')
005000140514
005100140514     d Digits          c                   const('0123456789')
005200140514
005300140514      //---------------------------------------------------------------
005400140514      //?Definizione schiere.
005500140514      //---------------------------------------------------------------
005600140514      // - Messaggi di errore
005700140514     d ErrMsg          s             78    dim(10) ctdata perrcd(1)
005800140514
005900140514      //---------------------------------------------------------------
006000140514      //?Definizione aree dati.
006100140514      //---------------------------------------------------------------
006200140514      // - Dati utente
006300140514     d §AzUte        e ds                  extname(AZUTE00F)
006400140514     d                                     dtaara
006500140514     d §DatiUte      e ds                  extname(dDatiUte)
006600140514     d                                     dtaara
006700140514
006800140514      //---------------------------------------------------------------
006900140514      //?Definizione strutture dati.
007000140514      //---------------------------------------------------------------
007100140514      // - Status
007200140514     d Psds           sds
007300140514     d   SDSpgm          *proc
007400140514
007500140514      // - InfDS
007600140514     d InfDspF         ds
007700140514     d  dsp_aid              369    369a
007800140514
007900140514      // - Indicatori su DspF
008000140514     d IndDspF         ds
008100140514      // - Indicatori di errore
008200140514     d  ErrMessage                    1n   overlay(IndDspF : 28)
008300140514      // - Indicatori di posizionamento cursore
008400140514     d  PosCurPoe                     1n   overlay(IndDspF : 50)
008500140514     d  PosCurDtad                    1n   overlay(IndDspF : 51)
008600140514     d  PosCurDtaa                    1n   overlay(IndDspF : 52)
008700140514      // - Indicatori di errore generico
008800140514     d  ErrGenerico                   1n   overlay(IndDspF : 99)
008900140514
009000140514     d WindDspF        ds                  inz
009100140514     d  WindDspFa              1     49    inz(*zero)
009200140514     d  WindDspFb             50     99    inz(*zero)
009300140514
009400140514     d wlbdat          ds                  inz
009500140514     d  g02dat                 1      8  0
009600140514     d  g02inv                 9     16  0
009700140514     d  g02err                17     17
009800140514     d  g02tgi                18     22  0
009900140514
010000140514      // - Parametri ricevuti
010100140514     d KPJBA         e ds
010200140514
010300140514      // - Passaggio dati a pgm stampa
010400140514     d FIOR84DS      e ds                  inz qualified
010500140514
010600140514      // - Ricerca/Controllo tabelle
010700140514     d TIBS02DS      e ds                  inz qualified
010800140514
010900140514      // - Reperimento dati utente
011000140514     d TIBS34DS      e ds                  inz qualified
011100140514
011200140514      // - Carico Filiali abilitate all'utente
011300140514     d TRUL31DS      e ds                  inz qualified
011400140514     d POG                    10    759    dim(250)
011500140514
011600140514      // - Tabella Abilitazioni
011700140514     d dLAT          e ds                  inz qualified
011800140514
011900140514      // - Campo Abilitazioni Utente
012000140514     d dUTE01        e ds                  inz qualified
012100140514
012200140514      // - Tabella 5A SEDE1
012300140514     d ds5AS1        e ds                  inz qualified
012400140514
012500140514      //---------------------------------------------------------------
012600140514      //?Definizione variabili globali.
012700140514      //---------------------------------------------------------------
012800140514      // - Flags booleani
012900140514     d UteSede         s               n   inz(*off)
013000140514     d wEnd            s               n   inz(*off)
013100140514     d wErrGrave       s               n   inz(*off)
013200140514     d wFine           s               n   inz(*off)
013300140514     d wInzD01         s               n   inz(*on)
013400140514
013500140514      // - Campi associati al video
013600140514     d wVideo          s              2    inz('D1')
013700140514
013800140514      // - Campi di comodo data
013900140514     d data_EUR        s               d   datfmt(*eur)
014000140514     d data_ISO        s               d   datfmt(*iso)
014100140514     d wDataPul        s              8  0 inz
014200140514     d wOggi           s              8  0 inz
014300140514
014400140514      // - Campi di comodo
014500140514     d keytabel        s              8a   inz
014600140514     d wabi            s                   inz like(UTEaut)
014700140514     d wdtad           s              8s 0 inz
014800140514     d wdtaa           s              8s 0 inz
014900140514
015000140514      //---------------------------------------------------------------
015100140514      //?Definizione procedure usate.
015200140514      //---------------------------------------------------------------
015300140514      // - Sottomissione lavoro batch
015400140514     d bch10           pr                  extpgm('BCH10')
015500140514     d  kpjba                              likeds(KPJBA)
015600140514
015700140514      // - Richiamo diretto lavoro batch
015800140514     d fior84c         pr                  extpgm('FIOR84C')
015900140514     d  kpjba                              likeds(KPJBA)
016000140514
016100140514      // - Filiali e aree abilitate
016200140514     d trul31r         pr                  extpgm('TRUL31R')
016300140514     d  kpjba                              likeds(KPJBA)
016400140514     d  trul31ds                           likeds(trul31ds)
016500140514
016600140514      //---------------------------------------------------------------
016700140514      //?Prototipi.
016800140514      //---------------------------------------------------------------
016900140514      /copy gaitrasrc/srcprotopr,TIBS02R
017000140514      /copy gaitrasrc/srcprotopr,TIBS34R
017100140514      /copy gaitrasrc/srcprotopi,TRULTAB
017200140514      /copy gaitrasrc/srcprotopr,TRULTAB
017300140514      /copy gaitrasrc/srcprotopr,XSRDA8
017400140514
017500140514      //---------------------------------------------------------------
017600140514      //?Definizione key-list.
017700140514      //---------------------------------------------------------------
017800140514
017900140514      //---------------------------------------------------------------
018000140514      //?M A I N - L I N E
018100140514      //---------------------------------------------------------------
018200140514
018300140514     c     *entry        plist
018400140514     c                   parm                    KPJBA
018500140514
018600140514      /free
018700140514
018800140514       //?Operazioni iniziali
018900140514       exsr RoutInz;
019000140514
019100140514       //?Gestione video
019200140514       DOW wFine = *off;
019300140514         SELECT;
019400140514           WHEN wVideo = 'D1';
019500140514             exsr GesD01;
019600140514           OTHER;
019700140514             wFine = *on;
019800140514         ENDSL;
019900140514       ENDDO;
020000140514
020100140514       //?Operazioni finali
020200140514       exsr RoutEnd;
020300140514
020400140514       //--------------------------------------------------------------
020500140514       //?Operazioni iniziali.
020600140514       //--------------------------------------------------------------
020700140514       BEGSR RoutInz;
020800080206
020900100326       //?Reperimento dati job
021000080206         exsr DatiJob;
021100140514
021200140514       //?Reperimento abilitazioni utente
021300140514         exsr AbiUte;
021400140514         IF  wErrGrave;
021500140514           ErrMessage  = *on;
021600140514           ErrGenerico = *on;
021700140514           V01msg = ErrMsg(01);
021800140514         ENDIF;
021900140514
022000140514       //?Reperimento filiali abilitate
022100140514         clear TRUL31DS;
022200140514         trul31ds.I31abi = wabi;
022300140514         trul31ds.I31cdi = DUTdis;
022400140514         trul31ds.I31car = DUTare;
022500140514         trul31ds.I31cpo = DUTpou;
022600140514         trul31r (kpjba:trul31ds);
022700140514         IF  trul31ds.O31pog <= *zeros;
022800140514           wErrGrave   = *on;
022900140514           ErrMessage  = *on;
023000140514           ErrGenerico = *on;
023100140514           V01msg = ErrMsg(01);
023200140514           leavesr;
023300140514         ENDIF;
023400140514
023500140514       //?Utente di sede
023600140514         UteSede = (simfel = 0);
023700140514
023800140514       //?Cerco giorni pulizia ORM in tabella 5A SEDE1
023900140514         clear dS5AS1;
024000140514         keytabel = 'SEDE1';
024100140514         IF  getTabella (c_tabel:'5A':keytabel:
024200140514                           *blanks:*blanks:*blanks:
024300140514                           *omit:*omit:*omit:*omit:*omit:
024400140514                           *omit:*omit:*omit:*omit:*omit:
024500140514                           ds_TNTBE.TBEatb:ds_TNTBE.TBEuni)
024600140514               = *zeros and ds_TNTBE.TBEatb = *blanks;
024700140514           ds5AS1 = ds_TNTBE.TBEuni;
024800140514         ENDIF;
024900140514
025000140514       //?Calcolo la data pulizia da oggi - nr. gg pulizia
025100140514         wOggi = %dec(%date());
025200140514         data_ISO = %date(wOggi);
025300140514         data_ISO = data_ISO - %days(ds5as1.§5Aors);
025400140514         wDataPul = %dec(data_ISO);
025500140514
025600140514       ENDSR;
025700140514
025800140514       //--------------------------------------------------------------
025900140514       //?Reperimento Dati del job.
026000140514       //--------------------------------------------------------------
026100140514       BEGSR DatiJob;
026200140514
026300140514         in(E) §AzUte;
026400140514         IF  not %error;
026500140514           in(E) §DatiUte;
026600140514         ENDIF;
026700140514         IF  %error or rsut = *blanks;
026800140514           clear TIBS34ds;
026900140514           tibs34r(tibs34ds);
027000140514           in §AzUte;
027100140514           in §DatiUte;
027200140514         ENDIF;
027300140514
027400140514       ENDSR;
027500140514
027600140514       //--------------------------------------------------------------
027700140514       //?Reperimento Abilitazioni utente.
027800140514       //--------------------------------------------------------------
027900140514       BEGSR AbiUte;
028000140514
028100140514         clear wabi;
028200140514
028300140514         dUTE01 = UTEfaf;
028400140514
028500140514       //?Imposto l'abilitazione dell'utente
028600140514         SELECT;
028700140514         WHEN  DUTerr = 'E';
028800140514           wErrGrave = *on;
028900140514           leavesr;
029000140514         WHEN  dute01.§UTEorm <> *blanks;
029100140514           wabi = dute01.§UTEorm;
029200140514         WHEN  UTEaut <> *blanks;
029300140514           wabi = UTEaut;
029400140514         OTHER;
029500140514           IF  DUTlpo = '1';
029600140514             wabi = 'TP';
029700140514           ENDIF;
029800140514           IF  DUTlpo = '2';
029900140514             wabi = 'PO';
030000140514           ENDIF;
030100140514         ENDSL;
030200140514
030300140514       //?Controllo se abilitazione OK
030400140514         clear TIBS02DS;
030500140514         clear dLAT;
030600140514         tibs02ds.T02mod = 'C';
030700140514         tibs02ds.T02cod = 'LAT';
030800140514         tibs02ds.T02ke1 = wabi;
030900140514         tibs02ds.T02sif = knsif;
031000140514         TNTBE_RicercaControllo (kpjba:tibs02ds);
031100140514         dLAT = tibs02ds.T02uni;
031200140514         IF  tibs02ds.T02err <> *blanks or dlat.§LATabi = 'S';
031300140514           wErrGrave = *on;
031400140514           leavesr;
031500140514         ENDIF;
031600140514
031700140514       ENDSR;
031800140514
031900140514       //--------------------------------------------------------------
032000140514       //?Gestione videata D01.
032100140514       //--------------------------------------------------------------
032200140514       BEGSR GesD01;
032300140514
032400140514       //?Inizializzazione videata
032500140514         IF  wInzD01;
032600140514           exsr InzD01;
032700140514           wInzD01 = *off;
032800140514         ENDIF;
032900140514
033000140514       //?Emissione videata
033100140514         exfmt FIOR83D01;
033200140514         ErrMessage   = *off;
033300140514         ErrGenerico  = *off;
033400140514         clear V01msg;
033500140514
033600140514         SELECT;
033700100326
033800100326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
033900140514         WHEN  wErrGrave;
034000140514           wFine = *on;
034100140514
034200140514       //?F3=Fine
034300140514         WHEN  dsp_aid = c_F03;
034400140514           exsr F03D01;
034500140514
034600140514       //?F6=Conferma
034700140514         WHEN  dsp_aid = c_F06;
034800140514           exsr CtrD01;
034900140514           IF  ErrGenerico;
035000140514             leavesr;
035100140514           ENDIF;
035200140514           exsr F06D01;
035300140514
035400140514       //?Enter
035500140514         OTHER;
035600140514           exsr CtrD01;
035700140514           IF  ErrGenerico;
035800140514             leavesr;
035900140514           ENDIF;
036000140514         ENDSL;
036100140514
036200140514       ENDSR;
036300140514
036400140514       //--------------------------------------------------------------
036500140514       //?Inizializzazione videata D01
036600140514       //--------------------------------------------------------------
036700140514       BEGSR InzD01;
036800140514
036900140514         clear FIOR83D01;
037000140514         V01pgm = SDSpgm;
037100140514         V01rsut = rsut;
037200140514         V01sif = knsif;
037300140514         V01ute = knmus;
037400140514
037500140514       //?Imposta filiale da elaborare
037600140514       //?e Abilita F18 se utente di filiale primo livello
037700140514         IF  not UteSede;
037800140514           V01poe = DUTpou;
037900140514         ENDIF;
038000140514
038100140514       ENDSR;
038200140514
038300140514       //--------------------------------------------------------------
038400140514       //?Gestione tasto funzionale F3 da videata D01
038500140514       //?F3=Fine.
038600140514       //--------------------------------------------------------------
038700140514       BEGSR F03D01;
038800140514
038900140514       //?Chiusura del programma
039000140514         wFine = *on;
039100140514
039200140514       ENDSR;
039300140514
039400140514       //--------------------------------------------------------------
039500140514       //?Gestione tasto funzionale F6 da videata D01
039600140514       //?F6=Conferma.
039700140514       //--------------------------------------------------------------
039800140514       BEGSR F06D01;
039900140514
040000140514       //?Lancio stampa
040100140514         clear FIOR84DS;
040200140514         fior84ds.OR84poe   = V01poe;
040300140514         fior84ds.OR84dtad  = wdtad;
040400140514         fior84ds.OR84dtaa  = wdtaa;
040500140514         kcoaz = 'OR84';
040600140514         kpjbu = FIOR84DS;
040700140514         kbuff = *blanks;
040800140514
040900140514       //?Richiamo interattivo
041000140520         IF  knmus = *all'1';
041100140514           FIOR84C(kpjba);
041200140514       //?Richiamo batch
041300140520         ELSE;
041400140520           BCH10(kpjba);
041500140520         ENDIF;
041600140514
041700140514       //?Chiusura del programma
041800140514         wFine = *on;
041900140514
042000140514       ENDSR;
042100140514
042200140514       //--------------------------------------------------------------
042300140514       //?Controllo videata D01.
042400140514       //--------------------------------------------------------------
042500140514       BEGSR CtrD01;
042600100326
042700140514         WindDspF = IndDspF;
042800140514         reset WindDspFb;
042900140514         IndDspF  = WindDspF;
043000140514
043100140514       //?Filiale emissione
043200140514         clear V01dpoe;
043300140515
043400140515       //?Obbligatoria
043500140515         IF  V01poe = 0;
043600140515           ErrMessage  = *on;
043700140515           ErrGenerico = *on;
043800140515           PosCurPoe   = *on;
043900140515           V01msg      = ErrMsg(02);
044000140515           leavesr;
044100140515         ENDIF;
044200140514
044300140515       //?999 tutte
044400140515         IF  V01poe = 999;
044500140515           V01dpoe = 'TUTTE';
044600140515         ELSE;
044700140515         //?Valida
044800140515           chain (V01poe) AZORG01L;
044900140515           IF  not %found(AZORG01L) or ORGfva <> *blanks;
045000140515             ErrMessage  = *on;
045100140515             ErrGenerico = *on;
045200140515             PosCurPoe   = *on;
045300140515             V01msg      = ErrMsg(02);
045400140515             leavesr;
045500140515           ENDIF;
045600140514
045700140515           V01dpoe = ORGdes;
045800140514
045900140515         //?In gestione all'utente
046000140515           IF  %lookup(%editc(V01poe:'X'):trul31ds.pog) = 0;
046100140515             ErrMessage  = *on;
046200140515             ErrGenerico = *on;
046300140515             PosCurPoe   = *on;
046400140515             V01msg      = ErrMsg(02);
046500140515             leavesr;
046600140515           ENDIF;
046700140514         ENDIF;
046800100706
046900140514       //?DATA Immissione ORM
047000140514         clear wdtad;
047100140514         clear wdtaa;
047200140514         IF  V01dtad = *zeros and V01dtaa = *zeros;
047300140514           ErrMessage  = *on;
047400140514           ErrGenerico = *on;
047500140514           PosCurDtad  = *on;
047600140514           V01msg      = ErrMsg(03);
047700140514           leavesr;
047800140514         ENDIF;
047900140908
048000140908         IF  V01dtad = *zeros;
048100140908           ErrMessage  = *on;
048200140908           ErrGenerico = *on;
048300140908           PosCurDtad  = *on;
048400140908           V01msg      = ErrMsg(03);
048500140908           leavesr;
048600140908         ENDIF;
048700140514
048800140514         IF  V01dtad > *zeros;
048900140514           clear wlbdat;
049000140514           G02dat = V01dtad;
049100140514           xsrda8(wlbdat);
049200140514           IF  G02err = '1';
049300140514             ErrMessage  = *on;
049400140514             ErrGenerico = *on;
049500140514             PosCurDtad  = *on;
049600140514             V01msg      = ErrMsg(03);
049700140514             leavesr;
049800140514           ENDIF;
049900140514
050000140514           V01dtad = G02dat;
050100140514           wdtad   = G02inv;
050200121029
050300140514         //?La data dal non può essere inferiore alla data pulizia ORM
050400140514           IF  wdtad < wDataPul;
050500140514             ErrMessage  = *on;
050600140514             ErrGenerico = *on;
050700140514             PosCurDtad  = *on;
050800140514             V01msg      = ErrMsg(03);
050900140514             V01msg = %trim(V01msg) +
051000140515             '. I dati sono stati puliti al ' +
051100140515             %subst(%editc(wDataPul:'X'):7:2) + '/' +
051200140515             %subst(%editc(wDataPul:'X'):5:2) + '/' +
051300140515             %subst(%editc(wDataPul:'X'):1:4);
051400140514             leavesr;
051500140514           ENDIF;
051600140514         ENDIF;
051700100708
051800140514         IF  V01dtaa <> 0;
051900140514           clear wlbdat;
052000140514           G02dat = V01dtaa;
052100140514           xsrda8(wlbdat);
052200140514           IF  G02err = '1';
052300140514             ErrMessage  = *on;
052400140514             ErrGenerico = *on;
052500140514             PosCurDtaa  = *on;
052600140514             V01msg      = ErrMsg(03);
052700140514             leavesr;
052800140514           ENDIF;
052900100706
053000140514           V01dtaa = G02dat;
053100140514           wdtaa   = G02inv;
053200140514         ENDIF;
053300140514
053400140514       //?Se impostata data "dal" e non "al"
053500140514       //?imposto "al" = data del giorno
053600140514         IF  V01dtad > 0 and V01dtaa = 0;
053700140514           wdtaa    = %dec(%date());
053800140514           data_eur = %date(wdtaa:*iso);
053900140514           V01dtaa  = %dec(data_eur);
054000140514         ENDIF;
054100140514
054200140514       //?"dal" congruente con "al"
054300140514         IF  wdtad > wdtaa;
054400140514           ErrMessage  = *on;
054500140514           ErrGenerico = *on;
054600140514           PosCurDtaa  = *on;
054700140514           V01msg      = ErrMsg(04);
054800140514           leavesr;
054900140514         ENDIF;
055000140514
055100140514       ENDSR;
055200140514
055300140514       //--------------------------------------------------------------
055400140514       //?Operazioni finali.
055500140514       //--------------------------------------------------------------
055600140514       BEGSR RoutEnd;
055700140514
055800140514         *inLR = *on;
055900140514         return;
056000140514
056100140514       ENDSR;
056200140514
056300140514      /end-free
056400140514
056500140514       //--------------------------------------------------------------
056600140514       //?Schiere a tempo di compilazione.
056700140514       //--------------------------------------------------------------
056800140514
056900140514** - ErrMsg -----------------------------------------------------------------*
057000140514Utente non abilitato alla funzione                                            01
057100140514Filiale emissione Errata o NON in gestione all'Utente                         02
057200140514Data errata                                                                   03
057300140514Data "DAL" incongruente con data "AL"                                         04
