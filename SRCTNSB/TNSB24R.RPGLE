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
012200120411
012300120411      // -?Parametri x TRUL55R
012400120411     d TRUL55ds      e ds                  inz
012500120411     d   I55tla      e                     inz('C')
012600121022
012700121023      // -?Tabella "LAC" = Clienti per ritorno immagini
012800121022     d dLAC          e ds                  inz
012900120411
013000120411       // -?DS campo TASFLO
013100120411     d dTASflo       e ds
013200120411
013300120411      //---------------------------------------------------------------
013400120411      //?Definizione variabili globali.
013500120411      //---------------------------------------------------------------
013600120411
013700120411       // -?Flags booleani
013800120411     d wEoF            s               n   inz(*off)
013900120411     d wErrGrave       s               n   inz(*off)
014000120411     d wFine           s               n   inz(*off)
014100120411     d wInzD01         s               n   inz(*on)
014200120411
014300120411       // -?Campi associati al video
014400120411     d wVideo          s              2    inz('D1')
014500120411     d dsp_mod         s             10I 0
014600120412
014700120412       // -?Campi di comodo
014800120412     d wTimeStamp      s             14
014900121023     d sav_V01ksc      s                   like(V01ksc)
015000141201     d P01opz3         s              1    inz('R')
015100120411
015200120411      //---------------------------------------------------------------
015300120411      //?Definizione procedure esterne.
015400120411      //---------------------------------------------------------------
015500121022
015600121022      // - Interrogazione tabella LAC
015700121022     d tntb461r        pr                  extpgm('TNTB461R')
015800121022     d  kpjba                              likeds(KPJBA)
015900141201     d  P01opz3                       1a
016000120411
016100120411      // -?Determinazione nome dell'immagine per TILAC
016200120411     d trul55r         pr                  extpgm('TRUL55R')
016300120412     d   trul55ds                          likeds(trul55ds)
016400120411
016500120411      //---------------------------------------------------------------
016600120411      //?Definizione prototipi.
016700120411      //---------------------------------------------------------------
016800120411
016900120411       // -?Ricerc/Controllo tabelle
017000120411      /copy gaitrasrc/srcprotopr,tibs02r
017100120411
017200120411       // -?Reperimento dati utente
017300120411      /copy gaitrasrc/srcprotopr,tibs34r
017400120411
017500120411      //---------------------------------------------------------------
017600120411      //?Definizione key-list.
017700120411      //---------------------------------------------------------------
017800120411
017900120411      //---------------------------------------------------------------
018000120411      //?Riepilogo indicatori.
018100120411      //---------------------------------------------------------------
018200120411
018300120411      //---------------------------------------------------------------
018400120411
018500120411      //---------------------------------------------------------------
018600120411      //?M A I N - L I N E
018700120411      //---------------------------------------------------------------
018800120411
018900120411     c     *Entry        plist
019000120411     c                   parm                    KPJBA
019100120411
019200120411      /free
019300120411
019400120411       //?Operazioni iniziali
019500120411       exsr RoutInz;
019600120411
019700120411       //?Gestione video
019800120411       DOW  wFine = *off;
019900120411
020000120411         select;
020100120411           when wVideo = 'D1';
020200120411             exsr GesD01;
020300120411           other;
020400120411             wFine = *on;
020500120411         endsl;
020600120411
020700120411       ENDDO;
020800120411
020900120411       //?Operazioni finali
021000120411       exsr RoutEnd;
021100120411
021200120411       //--------------------------------------------------------------
021300120411       //?Operazioni iniziali.
021400120411       //--------------------------------------------------------------
021500120411       BEGSR RoutInz;
021600120411
021700120411         wVideo = 'D1';
021800120411         wInzD01 = *on;
021900120412
022000120412         wTimeStamp = %char(%timestamp:*iso0);
022100120411
022200120411       //?Reperimento dati job
022300120411         exsr DatiJob;
022400120411
022500120411       //?Impostazione campi "fissi"
022600120411         V1Tpgm = SDSpgm;
022700120411
022800120411       ENDSR;
022900120411
023000120411       //--------------------------------------------------------------
023100120411       //?Reperimento Dati del job (Utente/Operativi).
023200120411       //--------------------------------------------------------------
023300120411       BEGSR DatiJob;
023400120411
023500120411         in(E) §AzUte;
023600120411         IF NOT %error;
023700120411           in(E) §DatiUte;
023800120411         ENDIF;
023900120411         IF %error or RSut = *blanks;
024000120411           clear TIBS34ds;
024100120411           tibs34r(tibs34ds);
024200120411           in §AzUte;
024300120411           in §DatiUte;
024400120411         ENDIF;
024500120411
024600120411       ENDSR;
024700120411
024800120411       //--------------------------------------------------------------
024900120411       //?Gestione videata D01
025000120411       //--------------------------------------------------------------
025100120411       BEGSR GesD01;
025200120411
025300120411       //?Inizializzazione videata
025400120411         IF  wInzD01 = *on;
025500120411            exsr InzD01;
025600120411            wInzD01  = *off;
025700120411         ENDIF;
025800120411
025900120411       //?Emissione Testata
026000120411         write  SB24T01;
026100120411
026200120411       //?Emissione videata
026300120411         exfmt  SB24D01;
026400120411         reset ErrMessage;
026500120411         reset ErrGenerico;
026600120411         clear V01msg;
026700120411
026800120411         SELECT;
026900120411
027000120411         //?- Errore grave esco dal pgm
027100120411           WHEN  wErrGrave;
027200120411             wFine = *on;
027300120411
027400120411         //?- F03=Fine
027500120411           WHEN  dsp_aid = c_F03;
027600120411             exsr F03D01;
027700120411
027800120411         //?- F06=Elabora
027900120411           WHEN  dsp_aid = c_F06;
028000120411             exsr CtrD01;
028100120411             IF  ErrGenerico;
028200120411               leavesr;
028300120411             ENDIF;
028400120411             exsr F06D01;
028500120412             wFine = *on;
028600120411
028700120411         //?Invio
028800120411           OTHER;
028900120411         //?Eseguo i controlli
029000120411             exsr CtrD01;
029100120411             IF  ErrGenerico;
029200120411               leavesr;
029300120411             ENDIF;
029400120411
029500120411         ENDSL;
029600120411
029700120411       ENDSR;
029800120411
029900120411       //--------------------------------------------------------------
030000120411       //?Inizializzazione videata D01.
030100120411       //--------------------------------------------------------------
030200120411       BEGSR InzD01;
030300120411
030400120411         clear V01aas;
030500120411         clear V01lnp;
030600120411         clear V01nrs;
030700120411         clear V01nsp;
030800120411         clear V01tpi;
030900120411         clear V01nim;
031000120411         clear V01dnim;
031100120411         clear V01dir;
031200121023         clear V01ksc;
031300121023         clear sav_V01ksc;
031400120411
031500120411       ENDSR;
031600120411
031700120411       //--------------------------------------------------------------
031800120411       //?Gestione tasto funzionale F03 da videata D01.
031900120411       //?F3=Fine
032000120411       //--------------------------------------------------------------
032100120411       BEGSR F03D01;
032200120411
032300120411       //?Chiusura del programma
032400120411         wFine = *on;
032500120411
032600120411       ENDSR;
032700120411
032800120411       //--------------------------------------------------------------
032900120411       //?Gestione tasto funzionale F06 da videata D01.
033000120411       //?F6=Conferma
033100120411       //--------------------------------------------------------------
033200120411       BEGSR F06D01;
033300120412
033400120412         clear TILAC000;
033500120412
033600120412         LACaas = %subst(%editc(TASaas:'X'):3:2);
033700120412         LAClnp = %editc(TASlnp:'X');
033800120412         LACnrs = %editc(TASnrs:'X');
033900120412         LACnsp = %editc(TASnsp:'X');
034000120412         LACdir = V01dir;
034100120412         LACtpi = V01tpi;
034200120412       //?Se il flag nome immagine è "S" vuol dire che il nome va impostato con
034300120412       //?il n.spedizione e questo lo fa Jdoc quindi lasciamo il campo vuoto
034400120412         clear  LACnim;
034500120412         IF  V01nim <> 'S ';
034600120412           clear  TRUL55ds;
034700120412           I55fmi = V01nim;
034800120412           I55lnp = TASlnp;
034900120412           I55nrs = TASnrs;
035000120412           I55nsp = TASnsp;
035100120412           I55aas = TASaas;
035200120412           I55mgs = TASmgs;
035300120412           I55rmn = TASrmn;
035400130411           I55fld = 'L';
035500120412           trul55r (TRUL55DS);
035600120412           IF  O55err = *off;
035700130411             LACnim = O55niml;
035800120412           ENDIF;
035900120412         ENDIF;
036000120412         LACela = '00';
036100120412
036200121022         LACtim = wTimeStamp;
036300121022         LACcop = 'CLI';
036400121022         LACidl = *blanks;
036500121022         LACtela = 'N';
036600121022         LACksc = TASksc;
036700121022         LACksu = TASksc;
036800121022       //?nessuna tassazione, sono immagini spot per nostro uso
036900121022         LACtad = 'N';
037000121022         LACtadu = 'J';
037100121022         LACidJob = wTimeStamp;
037200121022         write TILAC000;
037300120411
037400120411       ENDSR;
037500120411
037600120411       //--------------------------------------------------------------
037700120411       //?Controllo dati videata D01.
037800120411       //--------------------------------------------------------------
037900120411       BEGSR CtrD01;
038000120411
038100120411         WindDspF  = IndDspF;
038200120411         reset WindDspFb;
038300120411         IndDspF   = WindDspF;
038400121026
038500121026       //?La spedizione deve essere valida
038600121026         chain (V01aas:V01lnp:V01nrs:V01nsp) TITAS30C;
038700121026         IF  not %found(TITAS30C);
038800121026           ErrMessage  = *on;
038900121026           ErrGenerico = *on;
039000121026           PosCurAAS   = *on;
039100121026           V01msg      = skMsg(02);
039200121026           leavesr;
039300121026         ENDIF;
039400121026
039500121026       //?La spedizione deve avere l'immagine
039600121026         dTASflo = TASflo;
039700121026         IF  §FLOiml <> 'I' and §FLOiml <> 'S';
039800121026           ErrMessage  = *on;
039900121026           ErrGenerico = *on;
040000121026           PosCurAAS   = *on;
040100121026           V01msg      = skMsg(06);
040200121026           leavesr;
040300121026         ENDIF;
040400121026
040500121026       //?Ricerca sul cliente
040600121026         IF  %scan('?' : V01ksc) > 0;
040700121026           clear  Param01;
040800121026           P01ord = '0';
040900121026           KPJBU  = Param01;
041000141201           tntb461r (kpjba:P01opz3);
041100121026           Param01 = KPJBU;
041200121026           IF  P01ke1 <> *zero;
041300121026             V01ksc = P01ke1;
041400121026           ENDIF;
041500121026         ENDIF;
041600121026
041700121026       //?Controllo il cliente
041800121026         IF  V01ksc <> *blanks;
041900121026         //?Controllo immissione solo caratteri numerici
042000121026           IF  %check(c_Digits : V01ksc) > *zeros;
042100121026             ErrMessage  = *on;
042200121026             ErrGenerico = *on;
042300121026             PosCurksc   = *on;
042400121026             V01msg = 'Immettere solo caratteri numerici';
042500121026             leavesr;
042600121026           ENDIF;
042700121026         //?Verifica esistenza codice cliente in tabella "LAC"
042800121026           clear dLAC;
042900121026           clear TIBS02ds;
043000121026           T02mod = 'C';
043100121026           T02cod = 'LAC';
043200121026           T02sif = KNSIF;
043300121026           T02ke1 = V01ksc;
043400121026           TNTBE_RicercaControllo(kpjba : tibs02ds);
043500121026           IF T02err <> *blanks;
043600121026             ErrGenerico = *on;
043700121026             ErrMessage  = *on;
043800121026             PosCurKsc   = *on;
043900121026             V01msg = T02msg;
044000121026             leavesr;
044100121026           ENDIF;
044200121026           dLAC   = T02uni;
044300121026         ENDIF;
044400121026
044500121026       //?Se cliente variato imposto i dati a video
044600121026         IF  V01ksc > *zeros and V01ksc <> sav_V01ksc;
044700121026           V01tpi = §LACtpi;
044800121026           V01nim = §LACfmi;
044900121026           V01dir = §LACdir;
045000121026           sav_V01ksc = V01ksc;
045100121026         ENDIF;
045200120411
045300121026       //?Tipo immagine
045400121026         IF  V01tpi = *blanks;
045500121026           ErrMessage  = *on;
045600121026           ErrGenerico = *on;
045700121026           PosCurTPI   = *on;
045800121026           V01msg      = skMsg(03);
045900121026           leavesr;
046000121026         ENDIF;
046100120411
046200121026       //?Nome immagine
046300121026         IF  V01nim = *blanks;
046400121026           ErrMessage  = *on;
046500121026           ErrGenerico = *on;
046600121026           PosCurNIM   = *on;
046700121026           V01msg      = skMsg(04);
046800121026           leavesr;
046900121026         ENDIF;
047000121026
047100121026       //?Ricerca del nome immagine
047200121026         IF  %scan('?' : V01nim) > 0;
047300121026           ErrGenerico = *on;
047400121026           PosCurNIM   = *on;
047500121026           clear TIBS02DS;
047600121026           T02mod = 'R';
047700121026           T02cod = 'NIM';
047800121026           T02sif = knsif;
047900121026           TNTBE_RicercaControllo (kpjba : tibs02ds);
048000121026           IF  T02err = *blanks;
048100130411             V01nim  = T02ke1;
048200130411             V01dnim = T02uni;
048300130411             leavesr;
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
