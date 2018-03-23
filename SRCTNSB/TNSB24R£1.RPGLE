000100120411      //--------------------------------------------------------------
000200120411      //?TNSB24R - Rigenerazione Immagini per n. bolla
000300120411      //--------------------------------------------------------------
000400120411
000500120411     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120411
000700120411      //---------------------------------------------------------------
000800120411      //?Dichiarazione file.
000900120411      //---------------------------------------------------------------
001000040721
001100120411       // -?File bolle di sede
001200120411     fTITAS30C  if   e           k disk
001300120411
001400120411       // -?File immagini
001500121022     fTILAC00F  o    e             disk
001600120411
001700120411       // -?File video
001800120412     fTNSB24D   cf   e             workstn indds(IndDspF)
001900120412     f                                     infds(InfDspF)
002000120411
002100120411      //---------------------------------------------------------------
002200120411      //?Definizione costanti.
002300120411      //---------------------------------------------------------------
002400120411
002500120411       // -?Tasti funzionali a video
002600120411     d c_F01           c                   const(x'31')
002700120411     d c_F02           c                   const(x'32')
002800120411     d c_F03           c                   const(x'33')
002900120411     d c_F04           c                   const(x'34')
003000120411     d c_F05           c                   const(x'35')
003100120411     d c_F06           c                   const(x'36')
003200120411     d c_F07           c                   const(x'37')
003300120411     d c_F08           c                   const(x'38')
003400120411     d c_F09           c                   const(x'39')
003500120411     d c_F10           c                   const(x'3A')
003600120411     d c_F11           c                   const(x'3B')
003700120411     d c_F12           c                   const(x'3C')
003800120411     d c_F13           c                   const(x'B1')
003900120411     d c_F14           c                   const(x'B2')
004000120411     d c_F15           c                   const(x'B3')
004100120411     d c_F16           c                   const(x'B4')
004200120411     d c_F17           c                   const(x'B5')
004300120411     d c_F18           c                   const(x'B6')
004400120411     d c_F19           c                   const(x'B7')
004500120411     d c_F20           c                   const(x'B8')
004600120411     d c_F21           c                   const(x'B9')
004700120411     d c_F22           c                   const(x'BA')
004800120411     d c_F23           c                   const(x'BB')
004900120411     d c_F24           c                   const(x'BC')
005000120411     d c_Enter         c                   const(x'F1')
005100120411     d c_RollDown      c                   const(x'F4')
005200120411     d c_RollUp        c                   const(x'F5')
005300120411
005400120411       // -?Costante per controllo "caratteri solo numerici"?
005500120411     d c_Digits        c                   const('0123456789')
005600120411
005700120411      //---------------------------------------------------------------
005800120411      //?Definizione schiere.
005900120411      //---------------------------------------------------------------
006000040721
006100120411       // -?Messaggi di errore
006200120411     d skMsg           s             78    dim(20) ctdata perrcd(1)
006300120411
006400120411      //---------------------------------------------------------------
006500120411      //?Definizione aree dati.
006600120411      //---------------------------------------------------------------
006700120411
006800120411       // -?Dati utente
006900120411     d §AzUte        e ds                  extname(AZUTE00F)
007000120411     d                                     dtaara
007100120411     d §DatiUte      e ds                  extname(dDatiUte)
007200120411     d                                     dtaara
007300120411
007400120411      //---------------------------------------------------------------
007500120411      //?Definizione strutture dati.
007600120411      //---------------------------------------------------------------
007700120411
007800120411       // -?Status
007900120411     d Psds           sds
008000120411     d   SDSpgm          *proc
008100120411
008200120411       // -?InfDS
008300120411     d InfDspF         ds
008400120411     d  dsp_aid              369    369a
008500120411     d  sfl_rrn              376    377i 0
008600120411     d  min_nrr              378    379i 0
008700120411     d  num_rcds             380    381i 0
008800120411
008900120411       // -?Indicatori su DspF
009000120411     d IndDspF         ds
009100120411       // -?Indicatori di errore
009200120411     d  ErrMessage                    1n   overlay(IndDspF : 28)
009300120411       // -?Altri Indicatori
009400120411     d  PosCurAAS                     1n   overlay(IndDspF : 50)
009500120411     d  PosCurTPI                     1n   overlay(IndDspF : 52)
009600120412     d  PosCurNIM                     1n   overlay(IndDspF : 53)
009700120411     d  PosCurDIR                     1n   overlay(IndDspF : 54)
009800121022     d  PosCurKsc                     1n   overlay(IndDspF : 55)
009900120411     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010000120411
010100120411     d WindDspF        ds                  inz
010200120411     d   WindDspFa             1     49    inz(*zero)
010300120411     d   WindDspFb            50     99    inz(*zero)
010400120411
010500120411       // -?Paremetri ricevuti
010600120411     d KPJBA         e ds
010700121022
010800121023       // -?Struttura per passaggio dati ad interrogazione tabella
010900121022     d Param01         ds                  inz
011000121022     d  P01cod                        7  0 inz
011100121022     d  P01ord                        1    inz
011200121022     d  P01ksu                        7  0 inz
011300121022     d  P01ke1                        7    inz
011400121022     d  P01ke2                       15    inz
011500121022     d  P01rit                        1    inz
011600040721
011700120411      // -?Ricerca/Controllo tabelle
011800120411     d TIBS02ds      e ds                  inz
011900120411
012000120411      // -?Reperimento dati utente
012100120411     d TIBS34ds      e ds
012200121022
012300121023      // -?Interrogazione tabella LAC
012400121022     d TNTB46ds      e ds                  inz
012500121022     d   B46opz      e                     inz('5')
012600121022     d   B46err      e                     inz(*off)
012700120411
012800120411      // -?Parametri x TRUL55R
012900120411     d TRUL55ds      e ds                  inz
013000120411     d   I55tla      e                     inz('C')
013100121022
013200121023      // -?Tabella "LAC" = Clienti per ritorno immagini
013300121022     d dLAC          e ds                  inz
013400120411
013500120411       // -?DS campo TASFLO
013600120411     d dTASflo       e ds
013700120411
013800120411      //---------------------------------------------------------------
013900120411      //?Definizione variabili globali.
014000120411      //---------------------------------------------------------------
014100120411
014200120411       // -?Flags booleani
014300120411     d wEoF            s               n   inz(*off)
014400120411     d wErrGrave       s               n   inz(*off)
014500120411     d wFine           s               n   inz(*off)
014600120411     d wInzD01         s               n   inz(*on)
014700120411
014800120411       // -?Campi associati al video
014900120411     d wVideo          s              2    inz('D1')
015000120411     d dsp_mod         s             10I 0
015100120412
015200120412       // -?Campi di comodo
015300120412     d wTimeStamp      s             14
015400121023     d sav_V01ksc      s                   like(V01ksc)
015500120411
015600120411      //---------------------------------------------------------------
015700120411      //?Definizione procedure esterne.
015800120411      //---------------------------------------------------------------
015900121022
016000121022      // - Interrogazione tabella LAC
016100121022     d tntb461r        pr                  extpgm('TNTB461R')
016200121022     d  kpjba                              likeds(KPJBA)
016300120411
016400120411      // -?Determinazione nome dell'immagine per TILAC
016500120411     d trul55r         pr                  extpgm('TRUL55R')
016600120412     d   trul55ds                          likeds(trul55ds)
016700120411
016800120411      //---------------------------------------------------------------
016900120411      //?Definizione prototipi.
017000120411      //---------------------------------------------------------------
017100120411
017200120411       // -?Ricerc/Controllo tabelle
017300120411      /copy gaitrasrc/srcprotopr,tibs02r
017400120411
017500120411       // -?Reperimento dati utente
017600120411      /copy gaitrasrc/srcprotopr,tibs34r
017700120411
017800120411      //---------------------------------------------------------------
017900120411      //?Definizione key-list.
018000120411      //---------------------------------------------------------------
018100120411
018200120411      //---------------------------------------------------------------
018300120411      //?Riepilogo indicatori.
018400120411      //---------------------------------------------------------------
018500120411
018600120411      //---------------------------------------------------------------
018700120411
018800120411      //---------------------------------------------------------------
018900120411      //?M A I N - L I N E
019000120411      //---------------------------------------------------------------
019100120411
019200120411     c     *Entry        plist
019300120411     c                   parm                    KPJBA
019400120411
019500120411      /free
019600120411
019700120411       //?Operazioni iniziali
019800120411       exsr RoutInz;
019900120411
020000120411       //?Gestione video
020100120411       DOW  wFine = *off;
020200120411
020300120411         select;
020400120411           when wVideo = 'D1';
020500120411             exsr GesD01;
020600120411           other;
020700120411             wFine = *on;
020800120411         endsl;
020900120411
021000120411       ENDDO;
021100120411
021200120411       //?Operazioni finali
021300120411       exsr RoutEnd;
021400120411
021500120411       //--------------------------------------------------------------
021600120411       //?Operazioni iniziali.
021700120411       //--------------------------------------------------------------
021800120411       BEGSR RoutInz;
021900120411
022000120411         wVideo = 'D1';
022100120411         wInzD01 = *on;
022200120412
022300120412         wTimeStamp = %char(%timestamp:*iso0);
022400120411
022500120411       //?Reperimento dati job
022600120411         exsr DatiJob;
022700120411
022800120411       //?Impostazione campi "fissi"
022900120411         V1Tpgm = SDSpgm;
023000120411
023100120411       ENDSR;
023200120411
023300120411       //--------------------------------------------------------------
023400120411       //?Reperimento Dati del job (Utente/Operativi).
023500120411       //--------------------------------------------------------------
023600120411       BEGSR DatiJob;
023700120411
023800120411         in(E) §AzUte;
023900120411         IF NOT %error;
024000120411           in(E) §DatiUte;
024100120411         ENDIF;
024200120411         IF %error or RSut = *blanks;
024300120411           clear TIBS34ds;
024400120411           tibs34r(tibs34ds);
024500120411           in §AzUte;
024600120411           in §DatiUte;
024700120411         ENDIF;
024800120411
024900120411       ENDSR;
025000120411
025100120411       //--------------------------------------------------------------
025200120411       //?Gestione videata D01
025300120411       //--------------------------------------------------------------
025400120411       BEGSR GesD01;
025500120411
025600120411       //?Inizializzazione videata
025700120411         IF  wInzD01 = *on;
025800120411            exsr InzD01;
025900120411            wInzD01  = *off;
026000120411         ENDIF;
026100120411
026200120411       //?Emissione Testata
026300120411         write  SB24T01;
026400120411
026500120411       //?Emissione videata
026600120411         exfmt  SB24D01;
026700120411         reset ErrMessage;
026800120411         reset ErrGenerico;
026900120411         clear V01msg;
027000120411
027100120411         SELECT;
027200120411
027300120411         //?- Errore grave esco dal pgm
027400120411           WHEN  wErrGrave;
027500120411             wFine = *on;
027600120411
027700120411         //?- F03=Fine
027800120411           WHEN  dsp_aid = c_F03;
027900120411             exsr F03D01;
028000120411
028100120411         //?- F06=Elabora
028200120411           WHEN  dsp_aid = c_F06;
028300120411             exsr CtrD01;
028400120411             IF  ErrGenerico;
028500120411               leavesr;
028600120411             ENDIF;
028700120411             exsr F06D01;
028800120412             wFine = *on;
028900120411
029000120411         //?Invio
029100120411           OTHER;
029200120411         //?Eseguo i controlli
029300120411             exsr CtrD01;
029400120411             IF  ErrGenerico;
029500120411               leavesr;
029600120411             ENDIF;
029700120411
029800120411         ENDSL;
029900120411
030000120411       ENDSR;
030100120411
030200120411       //--------------------------------------------------------------
030300120411       //?Inizializzazione videata D01.
030400120411       //--------------------------------------------------------------
030500120411       BEGSR InzD01;
030600120411
030700120411         clear V01aas;
030800120411         clear V01lnp;
030900120411         clear V01nrs;
031000120411         clear V01nsp;
031100120411         clear V01tpi;
031200120411         clear V01nim;
031300120411         clear V01dnim;
031400120411         clear V01dir;
031500121023         clear V01ksc;
031600121023         clear sav_V01ksc;
031700120411
031800120411       ENDSR;
031900120411
032000120411       //--------------------------------------------------------------
032100120411       //?Gestione tasto funzionale F03 da videata D01.
032200120411       //?F3=Fine
032300120411       //--------------------------------------------------------------
032400120411       BEGSR F03D01;
032500120411
032600120411       //?Chiusura del programma
032700120411         wFine = *on;
032800120411
032900120411       ENDSR;
033000120411
033100120411       //--------------------------------------------------------------
033200120411       //?Gestione tasto funzionale F06 da videata D01.
033300120411       //?F6=Conferma
033400120411       //--------------------------------------------------------------
033500120411       BEGSR F06D01;
033600120412
033700120412         clear TILAC000;
033800120412
033900120412         LACaas = %subst(%editc(TASaas:'X'):3:2);
034000120412         LAClnp = %editc(TASlnp:'X');
034100120412         LACnrs = %editc(TASnrs:'X');
034200120412         LACnsp = %editc(TASnsp:'X');
034300120412         LACdir = V01dir;
034400120412         LACtpi = V01tpi;
034500120412       //?Se il flag nome immagine è "S" vuol dire che il nome va impostato con
034600120412       //?il n.spedizione e questo lo fa Jdoc quindi lasciamo il campo vuoto
034700120412         clear  LACnim;
034800120412         IF  V01nim <> 'S ';
034900120412           clear  TRUL55ds;
035000120412           I55fmi = V01nim;
035100120412           I55lnp = TASlnp;
035200120412           I55nrs = TASnrs;
035300120412           I55nsp = TASnsp;
035400120412           I55aas = TASaas;
035500120412           I55mgs = TASmgs;
035600120412           I55rmn = TASrmn;
035700120412           trul55r (TRUL55DS);
035800120412           IF  O55err = *off;
035900120412             LACnim = O55nim;
036000120412           ENDIF;
036100120412         ENDIF;
036200120412         LACela = '00';
036300120412
036400121022         LACtim = wTimeStamp;
036500121022         LACcop = 'CLI';
036600121022         LACidl = *blanks;
036700121022         LACtela = 'N';
036800121022         LACksc = TASksc;
036900121022         LACksu = TASksc;
037000121022       //?nessuna tassazione, sono immagini spot per nostro uso
037100121022         LACtad = 'N';
037200121022         LACtadu = 'J';
037300121022         LACidJob = wTimeStamp;
037400121022         write TILAC000;
037500120411
037600120411       ENDSR;
037700120411
037800120411       //--------------------------------------------------------------
037900120411       //?Controllo dati videata D01.
038000120411       //--------------------------------------------------------------
038100120411       BEGSR CtrD01;
038200120411
038300120411         WindDspF  = IndDspF;
038400120411         reset WindDspFb;
038500120411         IndDspF   = WindDspF;
038600121026
038700121026       //?La spedizione deve essere valida
038800121026         chain (V01aas:V01lnp:V01nrs:V01nsp) TITAS30C;
038900121026         IF  not %found(TITAS30C);
039000121026           ErrMessage  = *on;
039100121026           ErrGenerico = *on;
039200121026           PosCurAAS   = *on;
039300121026           V01msg      = skMsg(02);
039400121026           leavesr;
039500121026         ENDIF;
039600121026
039700121026       //?La spedizione deve avere l'immagine
039800121026         dTASflo = TASflo;
039900121026         IF  §FLOiml <> 'I' and §FLOiml <> 'S';
040000121026           ErrMessage  = *on;
040100121026           ErrGenerico = *on;
040200121026           PosCurAAS   = *on;
040300121026           V01msg      = skMsg(06);
040400121026           leavesr;
040500121026         ENDIF;
040600121026
040700121026       //?Ricerca sul cliente
040800121026         IF  %scan('?' : V01ksc) > 0;
040900121026           clear  Param01;
041000121026           P01ord = '0';
041100121026           KPJBU  = Param01;
041200121026           tntb461r (KPJBA);
041300121026           Param01 = KPJBU;
041400121026           IF  P01ke1 <> *zero;
041500121026             V01ksc = P01ke1;
041600121026           ENDIF;
041700121026         ENDIF;
041800121026
041900121026       //?Controllo il cliente
042000121026         IF  V01ksc <> *blanks;
042100121026         //?Controllo immissione solo caratteri numerici
042200121026           IF  %check(c_Digits : V01ksc) > *zeros;
042300121026             ErrMessage  = *on;
042400121026             ErrGenerico = *on;
042500121026             PosCurksc   = *on;
042600121026             V01msg = 'Immettere solo caratteri numerici';
042700121026             leavesr;
042800121026           ENDIF;
042900121026         //?Verifica esistenza codice cliente in tabella "LAC"
043000121026           clear dLAC;
043100121026           clear TIBS02ds;
043200121026           T02mod = 'C';
043300121026           T02cod = 'LAC';
043400121026           T02sif = KNSIF;
043500121026           T02ke1 = V01ksc;
043600121026           TNTBE_RicercaControllo(kpjba : tibs02ds);
043700121026           IF T02err <> *blanks;
043800121026             ErrGenerico = *on;
043900121026             ErrMessage  = *on;
044000121026             PosCurKsc   = *on;
044100121026             V01msg = T02msg;
044200121026             leavesr;
044300121026           ENDIF;
044400121026           dLAC   = T02uni;
044500121026         ENDIF;
044600121026
044700121026       //?Se cliente variato imposto i dati a video
044800121026         IF  V01ksc > *zeros and V01ksc <> sav_V01ksc;
044900121026           V01tpi = §LACtpi;
045000121026           V01nim = §LACfmi;
045100121026           V01dir = §LACdir;
045200121026           sav_V01ksc = V01ksc;
045300121026         ENDIF;
045400120411
045500121026       //?Tipo immagine
045600121026         IF  V01tpi = *blanks;
045700121026           ErrMessage  = *on;
045800121026           ErrGenerico = *on;
045900121026           PosCurTPI   = *on;
046000121026           V01msg      = skMsg(03);
046100121026           leavesr;
046200121026         ENDIF;
046300120411
046400121026       //?Nome immagine
046500121026         IF  V01nim = *blanks;
046600121026           ErrMessage  = *on;
046700121026           ErrGenerico = *on;
046800121026           PosCurNIM   = *on;
046900121026           V01msg      = skMsg(04);
047000121026           leavesr;
047100121026         ENDIF;
047200121026
047300121026       //?Ricerca del nome immagine
047400121026         IF  %scan('?' : V01nim) > 0;
047500121026           ErrGenerico = *on;
047600121026           PosCurNIM   = *on;
047700121026           clear TIBS02DS;
047800121026           T02mod = 'R';
047900121026           T02cod = 'NIM';
048000121026           T02sif = knsif;
048100121026           TNTBE_RicercaControllo (kpjba : tibs02ds);
048200121026           IF  T02err = *blanks;
048300121026             V01nim = T02ke1;
048400121026           ENDIF;
048500121026         ENDIF;
048600121026
048700121026       //?Il nome immagine deve esistere in tabella NIM
048800121026         clear TIBS02DS;
048900121026         T02mod = 'C';
049000121026         T02cod = 'NIM';
049100121026         T02ke1 = V01nim;
049200121026         T02sif = knsif;
049300121026         TNTBE_RicercaControllo (kpjba : tibs02ds);
049400121026         IF  T02err <> *blanks;
049500121026           ErrMessage  = *on;
049600121026           ErrGenerico = *on;
049700121026           PosCurNIM   = *on;
049800121026           V01msg      = skMsg(02);
049900121026           leavesr;
050000121026         ENDIF;
050100121026         V01dnim = T02uni;
050200120411
050300121026       //?Directory
050400121026         IF  V01dir = *blanks;
050500121026           ErrMessage  = *on;
050600121026           ErrGenerico = *on;
050700121026           PosCurDIR   = *on;
050800121026           V01msg      = skMsg(05);
050900121026           leavesr;
051000121026         ENDIF;
051100120411
051200120411       ENDSR;
051300120411
051400120411       //--------------------------------------------------------------
051500120411       //?Operazioni finali.
051600120411       //--------------------------------------------------------------
051700120411       BEGSR RoutEnd;
051800120411
051900120411         *inLR = *on;
052000120411         return;
052100120411
052200120411       ENDSR;
052300120411
052400120411      /end-free
052500120411
052600120411       //--------------------------------------------------------------
052700120411       //?Schiere a tempo di compilazione.
052800120411       //--------------------------------------------------------------
052900120411
053000120411** - skMSG ------------------------------------------------------------------*
053100120411Immettere l'anno                                                              01
053200120411Spedizione errata                                                             02
053300120411Immettere il tipo immagine                                                    03
053400120411Nome immagine errato                                                          04
053500120411Immettere la directory                                                        05
053600120411Immagine non presente per la spedizione richiesta                             06
