000100120411      //--------------------------------------------------------------
000200121023      //?TNSB74R - Rigenerazione Documenti per n. bolla MERGE
000300120411      //--------------------------------------------------------------
000400120411
000500120411     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120411
000700120411      //---------------------------------------------------------------
000800120411      //?Dichiarazione file.
000900120411      //---------------------------------------------------------------
001000121026
001100120411       // -?File bolle di sede
001200120606     fFIAR531C  if   e           k disk
001300120606     fTITAS30C  if   e           k disk
001400120411
001500120606       // -?File documenti
001600121023     fTIJDC00F  o    e             disk    prefix(JDC:3)
001700121023     fTILAC00F  o    e             disk
001800120411
001900120411       // -?File video
002000121023     fTNSB74D   cf   e             workstn indds(IndDspF)
002100120412     f                                     infds(InfDspF)
002200120411
002300120411      //---------------------------------------------------------------
002400120411      //?Definizione costanti.
002500120411      //---------------------------------------------------------------
002600120411
002700120411       // -?Tasti funzionali a video
002800120411     d c_F01           c                   const(x'31')
002900120411     d c_F02           c                   const(x'32')
003000120411     d c_F03           c                   const(x'33')
003100120411     d c_F04           c                   const(x'34')
003200120411     d c_F05           c                   const(x'35')
003300120411     d c_F06           c                   const(x'36')
003400120411     d c_F07           c                   const(x'37')
003500120411     d c_F08           c                   const(x'38')
003600120411     d c_F09           c                   const(x'39')
003700120411     d c_F10           c                   const(x'3A')
003800120411     d c_F11           c                   const(x'3B')
003900120411     d c_F12           c                   const(x'3C')
004000120411     d c_F13           c                   const(x'B1')
004100120411     d c_F14           c                   const(x'B2')
004200120411     d c_F15           c                   const(x'B3')
004300120411     d c_F16           c                   const(x'B4')
004400120411     d c_F17           c                   const(x'B5')
004500120411     d c_F18           c                   const(x'B6')
004600120411     d c_F19           c                   const(x'B7')
004700120411     d c_F20           c                   const(x'B8')
004800120411     d c_F21           c                   const(x'B9')
004900120411     d c_F22           c                   const(x'BA')
005000120411     d c_F23           c                   const(x'BB')
005100120411     d c_F24           c                   const(x'BC')
005200120411     d c_Enter         c                   const(x'F1')
005300120411     d c_RollDown      c                   const(x'F4')
005400120411     d c_RollUp        c                   const(x'F5')
005500120411
005600120411       // -?Costante per controllo "caratteri solo numerici"?
005700120411     d c_Digits        c                   const('0123456789')
005800120411
005900120411      //---------------------------------------------------------------
006000120411      //?Definizione schiere.
006100120411      //---------------------------------------------------------------
006200040721
006300120411       // -?Messaggi di errore
006400120411     d skMsg           s             78    dim(20) ctdata perrcd(1)
006500120411
006600120411      //---------------------------------------------------------------
006700120411      //?Definizione aree dati.
006800120411      //---------------------------------------------------------------
006900120411
007000120411       // -?Dati utente
007100120411     d §AzUte        e ds                  extname(AZUTE00F)
007200120411     d                                     dtaara
007300120411     d §DatiUte      e ds                  extname(dDatiUte)
007400120411     d                                     dtaara
007500120411
007600120411      //---------------------------------------------------------------
007700120411      //?Definizione strutture dati.
007800120411      //---------------------------------------------------------------
007900120411
008000120411       // -?Status
008100120411     d Psds           sds
008200120411     d   SDSpgm          *proc
008300120411
008400120411       // -?InfDS
008500120411     d InfDspF         ds
008600120411     d  dsp_aid              369    369a
008700120411     d  sfl_rrn              376    377i 0
008800120411     d  min_nrr              378    379i 0
008900120411     d  num_rcds             380    381i 0
009000120411
009100120411       // -?Indicatori su DspF
009200120411     d IndDspF         ds
009300120411       // -?Indicatori di errore
009400120411     d  ErrMessage                    1n   overlay(IndDspF : 28)
009500120411       // -?Altri Indicatori
009600120411     d  PosCurAAS                     1n   overlay(IndDspF : 50)
009700121026     d  PosCurKSC                     1n   overlay(IndDspF : 52)
009800120412     d  PosCurNIM                     1n   overlay(IndDspF : 53)
009900121026     d  PosCurDIRD                    1n   overlay(IndDspF : 54)
010000121026     d  PosCurDIRP                    1n   overlay(IndDspF : 55)
010100121026     d  PosCurPAG                     1n   overlay(IndDspF : 56)
010200120411     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010300120411
010400120411     d WindDspF        ds                  inz
010500120411     d   WindDspFa             1     49    inz(*zero)
010600120411     d   WindDspFb            50     99    inz(*zero)
010700120411
010800120411       // -?Paremetri ricevuti
010900120411     d KPJBA         e ds
011000121023
011100121023       // -?Struttura per passaggio dati ad interrogazione tabella
011200121023     d Param01         ds                  inz
011300121023     d  P01cod                        7  0 inz
011400121023     d  P01ord                        1    inz
011500121023     d  P01ksu                        7  0 inz
011600121023     d  P01ke1                        7    inz
011700121023     d  P01ke2                       15    inz
011800121023     d  P01rit                        1    inz
011900040721
012000120411      // -?Ricerca/Controllo tabelle
012100120411     d TIBS02ds      e ds                  inz
012200120411
012300120411      // -?Reperimento dati utente
012400120411     d TIBS34ds      e ds
012500121023
012600121023      // -?Interrogazione tabella JDC
012700121023     d TNTBJDCds     e ds                  inz
012800121023     d   BJDCopz     e                     inz('5')
012900121023     d   BJDCerr     e                     inz(*off)
013000120411
013100120606      // -?Parametri x TRUL54R
013200120606     d TRUL54ds      e ds                  inz
013300120606     d   I54tla      e                     inz('C')
013400120411
013500120606       // -?DS campo AR5 rcd DOC
013600120606     d dAR5doc       e ds
013700121023
013800121023      // -?Tabella "JDC" = Clienti per ritorno immagini
013900121023     d dJDC          e ds                  inz
014000121023
014100121023      // -?Tabella "LAC" = Clienti per ritorno immagini
014200121023     d dLAC          e ds                  inz
014300121023
014400121023       // -?DS campo TASflo
014500121023     d dTASflo       e ds
014600120411
014700120411      //---------------------------------------------------------------
014800120411      //?Definizione variabili globali.
014900120411      //---------------------------------------------------------------
015000120411
015100120411       // -?Flags booleani
015200120411     d wEoF            s               n   inz(*off)
015300120411     d wErrGrave       s               n   inz(*off)
015400120411     d wFine           s               n   inz(*off)
015500120411     d wInzD01         s               n   inz(*on)
015600120411
015700120411       // -?Campi associati al video
015800120411     d wVideo          s              2    inz('D1')
015900120411     d dsp_mod         s             10I 0
016000120412
016100120412       // -?Campi di comodo
016200120412     d wTimeStamp      s             14
016300120606     d kAR5trd         s                   like(AR5trd) inz('DOC')
016400121023     d sav_V01ksc      s                   like(V01ksc)
016500120411
016600120411      //---------------------------------------------------------------
016700120411      //?Definizione procedure esterne.
016800120411      //---------------------------------------------------------------
016900121023
017000121023      // -?Interroga tabella JDC
017100121023     d tntbjdc1r       pr                  extpgm('TNTBJDC1R')
017200121023     d  kpjba                              likeds(KPJBA)
017300120411
017400120606      // -?Determinazione nome dell'immagine per TIJDC
017500120606     d trul54r         pr                  extpgm('TRUL54R')
017600120606     d   trul54ds                          likeds(trul54ds)
017700120411
017800120411      //---------------------------------------------------------------
017900120411      //?Definizione prototipi.
018000120411      //---------------------------------------------------------------
018100120411
018200120411       // -?Ricerc/Controllo tabelle
018300120411      /copy gaitrasrc/srcprotopr,tibs02r
018400120411
018500120411       // -?Reperimento dati utente
018600120411      /copy gaitrasrc/srcprotopr,tibs34r
018700120411
018800120411      //---------------------------------------------------------------
018900120411      //?Definizione key-list.
019000120411      //---------------------------------------------------------------
019100120411
019200120411      //---------------------------------------------------------------
019300120411      //?Riepilogo indicatori.
019400120411      //---------------------------------------------------------------
019500120411
019600120411      //---------------------------------------------------------------
019700120411
019800120411      //---------------------------------------------------------------
019900120411      //?M A I N - L I N E
020000120411      //---------------------------------------------------------------
020100120411
020200120411     c     *Entry        plist
020300120411     c                   parm                    KPJBA
020400120411
020500120411      /free
020600120411
020700120411       //?Operazioni iniziali
020800120411       exsr RoutInz;
020900120411
021000120411       //?Gestione video
021100120411       DOW  wFine = *off;
021200120411
021300120411         select;
021400120411           when wVideo = 'D1';
021500120411             exsr GesD01;
021600120411           other;
021700120411             wFine = *on;
021800120411         endsl;
021900120411
022000120411       ENDDO;
022100120411
022200120411       //?Operazioni finali
022300120411       exsr RoutEnd;
022400120411
022500120411       //--------------------------------------------------------------
022600120411       //?Operazioni iniziali.
022700120411       //--------------------------------------------------------------
022800120411       BEGSR RoutInz;
022900120411
023000120411         wVideo = 'D1';
023100120411         wInzD01 = *on;
023200120412
023300120412         wTimeStamp = %char(%timestamp:*iso0);
023400120411
023500120411       //?Reperimento dati job
023600120411         exsr DatiJob;
023700120411
023800120411       //?Impostazione campi "fissi"
023900120411         V1Tpgm = SDSpgm;
024000120411
024100120411       ENDSR;
024200120411
024300120411       //--------------------------------------------------------------
024400120411       //?Reperimento Dati del job (Utente/Operativi).
024500120411       //--------------------------------------------------------------
024600120411       BEGSR DatiJob;
024700120411
024800120411         in(E) §AzUte;
024900120411         IF NOT %error;
025000120411           in(E) §DatiUte;
025100120411         ENDIF;
025200120411         IF %error or RSut = *blanks;
025300120411           clear TIBS34ds;
025400120411           tibs34r(tibs34ds);
025500120411           in §AzUte;
025600120411           in §DatiUte;
025700120411         ENDIF;
025800120411
025900120411       ENDSR;
026000120411
026100120411       //--------------------------------------------------------------
026200120411       //?Gestione videata D01
026300120411       //--------------------------------------------------------------
026400120411       BEGSR GesD01;
026500120411
026600120411       //?Inizializzazione videata
026700120411         IF  wInzD01 = *on;
026800120411            exsr InzD01;
026900120411            wInzD01  = *off;
027000120411         ENDIF;
027100120411
027200120411       //?Emissione Testata
027300121024         write  SB74T01;
027400120411
027500120411       //?Emissione videata
027600121024         exfmt  SB74D01;
027700120411         reset ErrMessage;
027800120411         reset ErrGenerico;
027900120411         clear V01msg;
028000120411
028100120411         SELECT;
028200120411
028300120411         //?- Errore grave esco dal pgm
028400120411           WHEN  wErrGrave;
028500120411             wFine = *on;
028600120411
028700120411         //?- F03=Fine
028800120411           WHEN  dsp_aid = c_F03;
028900120411             exsr F03D01;
029000120411
029100120411         //?- F06=Elabora
029200120411           WHEN  dsp_aid = c_F06;
029300120411             exsr CtrD01;
029400120411             IF  ErrGenerico;
029500120411               leavesr;
029600120411             ENDIF;
029700120411             exsr F06D01;
029800120412             wFine = *on;
029900120411
030000120411         //?Invio
030100120411           OTHER;
030200120411         //?Eseguo i controlli
030300120411             exsr CtrD01;
030400120411             IF  ErrGenerico;
030500120411               leavesr;
030600120411             ENDIF;
030700120411
030800120411         ENDSL;
030900120411
031000120411       ENDSR;
031100120411
031200120411       //--------------------------------------------------------------
031300120411       //?Inizializzazione videata D01.
031400120411       //--------------------------------------------------------------
031500120411       BEGSR InzD01;
031600120411
031700120411         clear V01aas;
031800120411         clear V01lnp;
031900120411         clear V01nrs;
032000120411         clear V01nsp;
032100121026         V01tpi = 'PDF';
032200120411         clear V01nim;
032300120411         clear V01dnim;
032400121026         clear V01dirD;
032500121026         clear V01dirP;
032600121023         clear V01ksc;
032700121024         clear V01pag;
032800121023         clear sav_V01ksc;
032900120411
033000120411       ENDSR;
033100120411
033200120411       //--------------------------------------------------------------
033300120411       //?Gestione tasto funzionale F03 da videata D01.
033400120411       //?F3=Fine
033500120411       //--------------------------------------------------------------
033600120411       BEGSR F03D01;
033700120411
033800120411       //?Chiusura del programma
033900120411         wFine = *on;
034000120411
034100120411       ENDSR;
034200120411
034300120411       //--------------------------------------------------------------
034400120411       //?Gestione tasto funzionale F06 da videata D01.
034500120411       //?F6=Conferma
034600120411       //--------------------------------------------------------------
034700120411       BEGSR F06D01;
034800121024
034900121024         clear  TRUL54ds;
035000121024         I54fmi = V01nim;
035100121024         I54lnp = TASlnp;
035200121024         I54nrs = TASnrs;
035300121024         I54nsp = TASnsp;
035400121024         I54aas = TASaas;
035500121024         I54mgs = TASmgs;
035600121024         I54rmn = TASrmn;
035700121024         trul54r (TRUL54DS);
035800121024
035900121024       //?Scrivo TILAC
036000121024         clear TILAC000;
036100121024         LACaas = %subst(%editc(TASaas:'X'):3:2);
036200121024         LAClnp = %editc(TASlnp:'X');
036300121024         LACnrs = %editc(TASnrs:'X');
036400121024         LACnsp = %editc(TASnsp:'X');
036500121026         LACdir = V01dirp;
036600121024         LACtpi = V01tpi;
036700121024       //?Se il flag nome immagine è "S" vuol dire che il nome va impostato con
036800121024       //?il n.spedizione e questo lo fa Jdoc quindi lasciamo il campo vuoto
036900121024         clear LACnim;
037000121024         IF  V01nim <> 'S ' and O54nim <> *blanks;
037100121024           LACnim = O54nim;
037200121024         ENDIF;
037300121024         LACela = '00';
037400121024         LACtim = wTimeStamp;
037500121024         LACcop = 'CLI';
037600121024         LACidl = *blanks;
037700121024         LACtela = 'N';
037800121024         LACksc = TASksc;
037900121024         LACksu = TASksc;
038000121024       //?nessuna tassazione, sono immagini spot per nostro uso
038100121024         LACtad = 'N';
038200121024         LACtadu = 'J';
038300121024         LACidJob = wTimeStamp;
038400121024         write TILAC000;
038500121024
038600121024       //?Scrivo TIJDC
038700121024         clear TIJDC000;
038800121024         JDCaas = LACaas;
038900121024         JDClnp = LAClnp;
039000121026         JDCnrs = LACnrs;
039100121026         JDCnsp = LACnsp;
039200121026         JDCcop = V01pag;
039300121026         JDCdir = V01dird;
039400121024         JDCtpi = V01tpi;
039500121024         JDCnim = LACnim;
039600121026         JDCela = '00';
039700121026         JDCtim = LACtim;
039800121026         JDCcop = V01pag;
039900121026         JDCidl = *blanks;
040000121024         JDCtela = 'N';
040100121024         JDCksc = LACksc;
040200121024         JDCksu = LACksu;
040300121024       //?nessuna tassazione, sono immagini spot per nostro uso
040400121024         JDCtad = 'N';
040500121024         JDCtadu = 'J';
040600121024         JDCidJob = LACidjob;
040700121024         write TIJDC000;
040800120411
040900120411       ENDSR;
041000120411
041100120411       //--------------------------------------------------------------
041200120411       //?Controllo dati videata D01.
041300120411       //--------------------------------------------------------------
041400120411       BEGSR CtrD01;
041500120411
041600120411         WindDspF  = IndDspF;
041700120411         reset WindDspFb;
041800120411         IndDspF   = WindDspF;
041900121026
042000121026       //?La spedizione deve essere valida
042100121026         chain (V01aas:V01lnp:V01nrs:V01nsp) TITAS30C;
042200121026         IF  not %found(TITAS30C);
042300121026           ErrMessage  = *on;
042400121026           ErrGenerico = *on;
042500121026           PosCurAAS   = *on;
042600121026           V01msg      = skMsg(02);
042700121026           leavesr;
042800121026         ENDIF;
042900121026
043000121026       //?La spedizione deve avere l'immagine LDV
043100121026         dTASflo = TASflo;
043200121026         IF  §FLOiml <> 'I' and §FLOiml <> 'S';
043300121026           ErrMessage  = *on;
043400121026           ErrGenerico = *on;
043500121026           PosCurAAS   = *on;
043600121026           V01msg      = skMsg(06);
043700121026           leavesr;
043800121026         ENDIF;
043900121026
044000121026       //?La spedizione deve avere l'immagine DOC
044100121026         chain (TASaas:TASlnp:TASnrs:TASnsp:kAR5trd) FIAR531C;
044200121026         dAR5doc = AR5uni;
044300121026         IF  §AR5jfid <> 'I' and §AR5jfid <> 'S';
044400121026           ErrMessage  = *on;
044500121026           ErrGenerico = *on;
044600121026           PosCurAAS   = *on;
044700121026           V01msg      = skMsg(06);
044800121026           leavesr;
044900121026         ENDIF;
045000121026
045100121026       //?Controllo il cliente
045200121026         IF  V01ksc <> *blanks;
045300121026           exsr CtrKSC;
045400121026         ENDIF;
045500121026         IF  ErrMessage;
045600121026           leavesr;
045700121026         ENDIF;
045800121026
045900121026       //?Ricerca del nome immagine
046000121026         IF  %scan('?' : V01nim) > 0;
046100121026           ErrGenerico = *on;
046200121026           PosCurNIM   = *on;
046300121026           clear TIBS02DS;
046400121026           T02mod = 'R';
046500121026           T02cod = 'NIM';
046600121026           T02sif = knsif;
046700121026           TNTBE_RicercaControllo (kpjba : tibs02ds);
046800121026           IF  T02err = *blanks;
046900121026             V01nim = T02ke1;
047000121026           ENDIF;
047100121026         ENDIF;
047200121026
047300121026       //?Il nome immagine deve esistere in tabella NIM
047400121026         clear TIBS02DS;
047500121026         T02mod = 'C';
047600121026         T02cod = 'NIM';
047700121026         T02ke1 = V01nim;
047800121026         T02sif = knsif;
047900121026         TNTBE_RicercaControllo (kpjba : tibs02ds);
048000121026         IF  T02err <> *blanks;
048100121026           ErrMessage  = *on;
048200121026           ErrGenerico = *on;
048300121026           PosCurNIM   = *on;
048400121029           V01msg      = skMsg(04);
048500121026           leavesr;
048600121026         ENDIF;
048700121026         V01dnim = T02uni;
048800121026
048900121026       //?Tipo invio
049000121026         IF  V01pag  <> 'CLI' and V01pag <> 'CL1';
049100121026           ErrMessage  = *on;
049200121026           ErrGenerico = *on;
049300121026           PosCurPAG   = *on;
049400121026           V01msg      = skMsg(07);
049500121026           leavesr;
049600121026         ENDIF;
049700120411
049800121026       //?Directory DOC
049900121026         IF  V01dird = *blanks;
050000121026           ErrMessage  = *on;
050100121026           ErrGenerico = *on;
050200121026           PosCurDIRd  = *on;
050300121026           V01msg      = skMsg(05);
050400121026           leavesr;
050500121026         ENDIF;
050600121026
050700121026       //?Directory POD
050800121026         IF  V01dirp = *blanks;
050900121026           ErrMessage  = *on;
051000121026           ErrGenerico = *on;
051100121026           PosCurDIRp  = *on;
051200121026           V01msg      = skMsg(05);
051300121026           leavesr;
051400121026         ENDIF;
051500120411
051600120411       ENDSR;
051700121024
051800121024       //--------------------------------------------------------------
051900121024       //?Controllo Cliente.
052000121024       //--------------------------------------------------------------
052100121024       BEGSR CtrKSC;
052200121024
052300121024
052400121024       //?Ricerca del cliente
052500121024         IF  %scan('?' : V01ksc) > 0;
052600121026           clear  Param01;
052700121026           P01ord = '0';
052800121026           KPJBU  = Param01;
052900121026           tntbjdc1r (KPJBA);
053000121026           Param01 = KPJBU;
053100121026           IF  P01ke1 = *zeros;
053200121026             leavesr;
053300121026           ENDIF;
053400121026           V01ksc = P01ke1;
053500121024         ENDIF;
053600121024
053700121024         IF  V01ksc = *blanks;
053800121024           leavesr;
053900121024         ENDIF;
054000121024
054100121024       //?Controllo immissione solo caratteri numerici
054200121024         IF  %check(c_Digits : V01ksc) > *zeros;
054300121024           ErrMessage  = *on;
054400121024           ErrGenerico = *on;
054500121024           PosCurksc   = *on;
054600121026           V01msg      = skMsg(08);
054700121024           leavesr;
054800121024         ENDIF;
054900121024
055000121024       //?Controllo il cliente
055100121024       //?su tabella LAC
055200121026         clear dLAC;
055300121026         clear TIBS02ds;
055400121026         T02mod = 'C';
055500121026         T02cod = 'LAC';
055600121026         T02sif = KNSIF;
055700121026         T02ke1 = V01ksc;
055800121026         TNTBE_RicercaControllo(kpjba : tibs02ds);
055900121026         IF T02err <> *blanks;
056000121026           ErrGenerico = *on;
056100121026           ErrMessage  = *on;
056200121026           PosCurKsc   = *on;
056300121026           V01msg = T02msg;
056400121026           V01msg = %trim(V01msg) + ' Tabella LAC';
056500121026           leavesr;
056600121026         ENDIF;
056700121026         dLAC = T02uni;
056800121024       //?su tabella JDC
056900121026         clear dJDC;
057000121026         clear TIBS02ds;
057100121026         T02sif = KNSIF;
057200121026         T02mod = 'C';
057300121026         T02cod = 'JDC';
057400121026         T02ke1 = V01ksc;
057500121026         TNTBE_RicercaControllo(kpjba : tibs02ds);
057600121026         IF T02err <> *blanks;
057700121026           ErrMessage  = *on;
057800121026           ErrGenerico = *on;
057900121026           PosCurKsc   = *on;
058000121026           V01msg = T02msg;
058100121026           V01msg = %trim(V01msg) + ' Tabella LAC';
058200121026           leavesr;
058300121026         ENDIF;
058400121026         dJDC = T02uni;
058500121024
058600121024       //?deve avere la richiesta di immagine con Merge
058700121026         IF  §LACmerge = *blanks or §JDCmerge = *blanks;
058800121024           ErrMessage  = *on;
058900121024           ErrGenerico = *on;
059000121026           PosCurKSC   = *on;
059100121026           V01msg      = skMSG(09);
059200121024           leavesr;
059300121024         ENDIF;
059400121024
059500121024       //?salvo i dati della tabella
059600121024         IF  V01ksc > *zeros and V01ksc <> sav_V01ksc;
059700121026           V01nim  = §LACfmi;
059800121026           V01dirp = §LACdir;
059900121026           V01dird = §JDCdir;
060000121026           V01pag  = §JDCpag;
060100121024           sav_V01ksc = V01ksc;
060200121024         ENDIF;
060300121024
060400121024       ENDSR;
060500120411
060600120411       //--------------------------------------------------------------
060700120411       //?Operazioni finali.
060800120411       //--------------------------------------------------------------
060900120411       BEGSR RoutEnd;
061000120411
061100120411         *inLR = *on;
061200120411         return;
061300120411
061400120411       ENDSR;
061500120411
061600120411      /end-free
061700120411
061800120411       //--------------------------------------------------------------
061900120411       //?Schiere a tempo di compilazione.
062000120411       //--------------------------------------------------------------
062100120411
062200120411** - skMSG ------------------------------------------------------------------*
062300120411Immettere l'anno                                                              01
062400120411Spedizione errata                                                             02
062500120606Immettere il tipo documento                                                   03
062600120606Nome documento errato                                                         04
062700120411Immettere la directory                                                        05
062800120606Documento non presente per la spedizione richiesta                            06
062900120607Tipo invio errato                                                             07
063000121026Immettere solo caratteri numerici                                             08
063100121026In tabella non è prevista l'immagine con Merge                                09
