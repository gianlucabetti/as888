000100120411      //--------------------------------------------------------------
000200120606      //?TNSB72R - Rigenerazione Documenti per n. bolla
000300120411      //--------------------------------------------------------------
000400120411
000500120411     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600120411
000700120411      //---------------------------------------------------------------
000800120411      //?Dichiarazione file.
000900120411      //---------------------------------------------------------------
001000040721
001100120411       // -?File bolle di sede
001200120606     fFIAR531C  if   e           k disk
001300120606     fTITAS30C  if   e           k disk
001400120411
001500120606       // -?File documenti
001600121022     fTIJDC00F  o    e             disk
001700120411
001800120411       // -?File video
001900120606     fTNSB72D   cf   e             workstn indds(IndDspF)
002000120412     f                                     infds(InfDspF)
002100120411
002200120411      //---------------------------------------------------------------
002300120411      //?Definizione costanti.
002400120411      //---------------------------------------------------------------
002500120411
002600120411       // -?Tasti funzionali a video
002700120411     d c_F01           c                   const(x'31')
002800120411     d c_F02           c                   const(x'32')
002900120411     d c_F03           c                   const(x'33')
003000120411     d c_F04           c                   const(x'34')
003100120411     d c_F05           c                   const(x'35')
003200120411     d c_F06           c                   const(x'36')
003300120411     d c_F07           c                   const(x'37')
003400120411     d c_F08           c                   const(x'38')
003500120411     d c_F09           c                   const(x'39')
003600120411     d c_F10           c                   const(x'3A')
003700120411     d c_F11           c                   const(x'3B')
003800120411     d c_F12           c                   const(x'3C')
003900120411     d c_F13           c                   const(x'B1')
004000120411     d c_F14           c                   const(x'B2')
004100120411     d c_F15           c                   const(x'B3')
004200120411     d c_F16           c                   const(x'B4')
004300120411     d c_F17           c                   const(x'B5')
004400120411     d c_F18           c                   const(x'B6')
004500120411     d c_F19           c                   const(x'B7')
004600120411     d c_F20           c                   const(x'B8')
004700120411     d c_F21           c                   const(x'B9')
004800120411     d c_F22           c                   const(x'BA')
004900120411     d c_F23           c                   const(x'BB')
005000120411     d c_F24           c                   const(x'BC')
005100120411     d c_Enter         c                   const(x'F1')
005200120411     d c_RollDown      c                   const(x'F4')
005300120411     d c_RollUp        c                   const(x'F5')
005400120411
005500120411       // -?Costante per controllo "caratteri solo numerici"?
005600120411     d c_Digits        c                   const('0123456789')
005700120411
005800120411      //---------------------------------------------------------------
005900120411      //?Definizione schiere.
006000120411      //---------------------------------------------------------------
006100040721
006200120411       // -?Messaggi di errore
006300120411     d skMsg           s             78    dim(20) ctdata perrcd(1)
006400120411
006500120411      //---------------------------------------------------------------
006600120411      //?Definizione aree dati.
006700120411      //---------------------------------------------------------------
006800120411
006900120411       // -?Dati utente
007000120411     d §AzUte        e ds                  extname(AZUTE00F)
007100120411     d                                     dtaara
007200120411     d §DatiUte      e ds                  extname(dDatiUte)
007300120411     d                                     dtaara
007400120411
007500120411      //---------------------------------------------------------------
007600120411      //?Definizione strutture dati.
007700120411      //---------------------------------------------------------------
007800120411
007900120411       // -?Status
008000120411     d Psds           sds
008100120411     d   SDSpgm          *proc
008200120411
008300120411       // -?InfDS
008400120411     d InfDspF         ds
008500120411     d  dsp_aid              369    369a
008600120411     d  sfl_rrn              376    377i 0
008700120411     d  min_nrr              378    379i 0
008800120411     d  num_rcds             380    381i 0
008900120411
009000120411       // -?Indicatori su DspF
009100120411     d IndDspF         ds
009200120411       // -?Indicatori di errore
009300120411     d  ErrMessage                    1n   overlay(IndDspF : 28)
009400120411       // -?Altri Indicatori
009500120411     d  PosCurAAS                     1n   overlay(IndDspF : 50)
009600120411     d  PosCurTPI                     1n   overlay(IndDspF : 52)
009700120412     d  PosCurNIM                     1n   overlay(IndDspF : 53)
009800120411     d  PosCurDIR                     1n   overlay(IndDspF : 54)
009900120607     d  PosCurPAG                     1n   overlay(IndDspF : 55)
010000121023     d  PosCurKsc                     1n   overlay(IndDspF : 56)
010100120411     d  ErrGenerico                   1n   overlay(IndDspF : 99)
010200120411
010300120411     d WindDspF        ds                  inz
010400120411     d   WindDspFa             1     49    inz(*zero)
010500120411     d   WindDspFb            50     99    inz(*zero)
010600120411
010700120411       // -?Paremetri ricevuti
010800120411     d KPJBA         e ds
010900121023
011000121023       // -?Struttura per passaggio dati ad interrogazione tabella
011100121023     d Param01         ds                  inz
011200121023     d  P01cod                        7  0 inz
011300121023     d  P01ord                        1    inz
011400121023     d  P01ksu                        7  0 inz
011500121023     d  P01ke1                        7    inz
011600121023     d  P01ke2                       15    inz
011700121023     d  P01rit                        1    inz
011800040721
011900120411      // -?Ricerca/Controllo tabelle
012000120411     d TIBS02ds      e ds                  inz
012100120411
012200120411      // -?Reperimento dati utente
012300120411     d TIBS34ds      e ds
012400121023
012500121023      // -?Interrogazione tabella JDC
012600121023     d TNTBJDCds     e ds                  inz
012700121023     d   BJDCopz     e                     inz('5')
012800121023     d   BJDCerr     e                     inz(*off)
012900120411
013000120606      // -?Parametri x TRUL54R
013100120606     d TRUL54ds      e ds                  inz
013200120606     d   I54tla      e                     inz('C')
013300120411
013400120606       // -?DS campo AR5 rcd DOC
013500120606     d dAR5doc       e ds
013600121023
013700121023      // -?Tabella "JDC" = Clienti per ritorno immagini
013800121023     d dJDC          e ds                  inz
013900120411
014000120411      //---------------------------------------------------------------
014100120411      //?Definizione variabili globali.
014200120411      //---------------------------------------------------------------
014300120411
014400120411       // -?Flags booleani
014500120411     d wEoF            s               n   inz(*off)
014600120411     d wErrGrave       s               n   inz(*off)
014700120411     d wFine           s               n   inz(*off)
014800120411     d wInzD01         s               n   inz(*on)
014900120411
015000120411       // -?Campi associati al video
015100120411     d wVideo          s              2    inz('D1')
015200120411     d dsp_mod         s             10I 0
015300120412
015400120412       // -?Campi di comodo
015500120412     d wTimeStamp      s             14
015600120606     d kAR5trd         s                   like(AR5trd) inz('DOC')
015700121023     d sav_V01ksc      s                   like(V01ksc)
015800120411
015900120411      //---------------------------------------------------------------
016000120411      //?Definizione procedure esterne.
016100120411      //---------------------------------------------------------------
016200121023
016300121023      // -?Interroga tabella JDC
016400121023     d tntbjdc1r       pr                  extpgm('TNTBJDC1R')
016500121023     d  kpjba                              likeds(KPJBA)
016600120411
016700120606      // -?Determinazione nome dell'immagine per TIJDC
016800120606     d trul54r         pr                  extpgm('TRUL54R')
016900120606     d   trul54ds                          likeds(trul54ds)
017000120411
017100120411      //---------------------------------------------------------------
017200120411      //?Definizione prototipi.
017300120411      //---------------------------------------------------------------
017400120411
017500120411       // -?Ricerc/Controllo tabelle
017600120411      /copy gaitrasrc/srcprotopr,tibs02r
017700120411
017800120411       // -?Reperimento dati utente
017900120411      /copy gaitrasrc/srcprotopr,tibs34r
018000120411
018100120411      //---------------------------------------------------------------
018200120411      //?Definizione key-list.
018300120411      //---------------------------------------------------------------
018400120411
018500120411      //---------------------------------------------------------------
018600120411      //?Riepilogo indicatori.
018700120411      //---------------------------------------------------------------
018800120411
018900120411      //---------------------------------------------------------------
019000120411
019100120411      //---------------------------------------------------------------
019200120411      //?M A I N - L I N E
019300120411      //---------------------------------------------------------------
019400120411
019500120411     c     *Entry        plist
019600120411     c                   parm                    KPJBA
019700120411
019800120411      /free
019900120411
020000120411       //?Operazioni iniziali
020100120411       exsr RoutInz;
020200120411
020300120411       //?Gestione video
020400120411       DOW  wFine = *off;
020500120411
020600120411         select;
020700120411           when wVideo = 'D1';
020800120411             exsr GesD01;
020900120411           other;
021000120411             wFine = *on;
021100120411         endsl;
021200120411
021300120411       ENDDO;
021400120411
021500120411       //?Operazioni finali
021600120411       exsr RoutEnd;
021700120411
021800120411       //--------------------------------------------------------------
021900120411       //?Operazioni iniziali.
022000120411       //--------------------------------------------------------------
022100120411       BEGSR RoutInz;
022200120411
022300120411         wVideo = 'D1';
022400120411         wInzD01 = *on;
022500120412
022600120412         wTimeStamp = %char(%timestamp:*iso0);
022700120411
022800120411       //?Reperimento dati job
022900120411         exsr DatiJob;
023000120411
023100120411       //?Impostazione campi "fissi"
023200120411         V1Tpgm = SDSpgm;
023300120411
023400120411       ENDSR;
023500120411
023600120411       //--------------------------------------------------------------
023700120411       //?Reperimento Dati del job (Utente/Operativi).
023800120411       //--------------------------------------------------------------
023900120411       BEGSR DatiJob;
024000120411
024100120411         in(E) §AzUte;
024200120411         IF NOT %error;
024300120411           in(E) §DatiUte;
024400120411         ENDIF;
024500120411         IF %error or RSut = *blanks;
024600120411           clear TIBS34ds;
024700120411           tibs34r(tibs34ds);
024800120411           in §AzUte;
024900120411           in §DatiUte;
025000120411         ENDIF;
025100120411
025200120411       ENDSR;
025300120411
025400120411       //--------------------------------------------------------------
025500120411       //?Gestione videata D01
025600120411       //--------------------------------------------------------------
025700120411       BEGSR GesD01;
025800120411
025900120411       //?Inizializzazione videata
026000120411         IF  wInzD01 = *on;
026100120411            exsr InzD01;
026200120411            wInzD01  = *off;
026300120411         ENDIF;
026400120411
026500120411       //?Emissione Testata
026600120606         write  SB72T01;
026700120411
026800120411       //?Emissione videata
026900120606         exfmt  SB72D01;
027000120411         reset ErrMessage;
027100120411         reset ErrGenerico;
027200120411         clear V01msg;
027300120411
027400120411         SELECT;
027500120411
027600120411         //?- Errore grave esco dal pgm
027700120411           WHEN  wErrGrave;
027800120411             wFine = *on;
027900120411
028000120411         //?- F03=Fine
028100120411           WHEN  dsp_aid = c_F03;
028200120411             exsr F03D01;
028300120411
028400120411         //?- F06=Elabora
028500120411           WHEN  dsp_aid = c_F06;
028600120411             exsr CtrD01;
028700120411             IF  ErrGenerico;
028800120411               leavesr;
028900120411             ENDIF;
029000120411             exsr F06D01;
029100120412             wFine = *on;
029200120411
029300120411         //?Invio
029400120411           OTHER;
029500120411         //?Eseguo i controlli
029600120411             exsr CtrD01;
029700120411             IF  ErrGenerico;
029800120411               leavesr;
029900120411             ENDIF;
030000120411
030100120411         ENDSL;
030200120411
030300120411       ENDSR;
030400120411
030500120411       //--------------------------------------------------------------
030600120411       //?Inizializzazione videata D01.
030700120411       //--------------------------------------------------------------
030800120411       BEGSR InzD01;
030900120411
031000120411         clear V01aas;
031100120411         clear V01lnp;
031200120411         clear V01nrs;
031300120411         clear V01nsp;
031400120411         clear V01tpi;
031500120411         clear V01nim;
031600120411         clear V01dnim;
031700120411         clear V01dir;
031800121023         clear V01ksc;
031900121023         clear sav_V01ksc;
032000120411
032100120411       ENDSR;
032200120411
032300120411       //--------------------------------------------------------------
032400120411       //?Gestione tasto funzionale F03 da videata D01.
032500120411       //?F3=Fine
032600120411       //--------------------------------------------------------------
032700120411       BEGSR F03D01;
032800120411
032900120411       //?Chiusura del programma
033000120411         wFine = *on;
033100120411
033200120411       ENDSR;
033300120411
033400120411       //--------------------------------------------------------------
033500120411       //?Gestione tasto funzionale F06 da videata D01.
033600120411       //?F6=Conferma
033700120411       //--------------------------------------------------------------
033800120411       BEGSR F06D01;
033900120412
034000120606         clear TIJDC000;
034100120412
034200120606         LACaas = %subst(%editc(AR5aas:'X'):3:2);
034300120606         LAClnp = %editc(AR5lnp:'X');
034400120606         LACnrs = %editc(AR5nrs:'X');
034500120606         LACnsp = %editc(AR5nsp:'X');
034600120607         LACcop = V01pag;
034700120412         LACdir = V01dir;
034800120412         LACtpi = V01tpi;
034900120412       //?Se il flag nome immagine è "S" vuol dire che il nome va impostato con
035000120412       //?il n.spedizione e questo lo fa Jdoc quindi lasciamo il campo vuoto
035100120412         clear  LACnim;
035200120412         IF  V01nim <> 'S ';
035300120606           clear  TRUL54ds;
035400120606           I54fmi = V01nim;
035500120606           I54lnp = TASlnp;
035600120606           I54nrs = TASnrs;
035700120606           I54nsp = TASnsp;
035800120606           I54aas = TASaas;
035900120606           I54mgs = TASmgs;
036000120606           I54rmn = TASrmn;
036100120606           trul54r (TRUL54DS);
036200120606           IF  O54err = *off;
036300120606             LACnim = O54nim;
036400120412           ENDIF;
036500120412         ENDIF;
036600120412         LACela = '00';
036700120412
036800121022         LACtim = wTimeStamp;
036900121022         LACcop = V01pag;
037000121022         LACidl = *blanks;
037100121022         LACtela = 'N';
037200121022         LACksc = TASksc;
037300121022         LACksu = TASksc;
037400121022       //?nessuna tassazione, sono immagini spot per nostro uso
037500121022         LACtadu = 'J';
037600121022         LACidJob = wTimeStamp;
037700121022         write TIJDC000;
037800120411
037900120411       ENDSR;
038000120411
038100120411       //--------------------------------------------------------------
038200120411       //?Controllo dati videata D01.
038300120411       //--------------------------------------------------------------
038400120411       BEGSR CtrD01;
038500120411
038600120411         WindDspF  = IndDspF;
038700120411         reset WindDspFb;
038800120411         IndDspF   = WindDspF;
038900121026
039000121026       //?La spedizione deve essere valida
039100121026         chain (V01aas:V01lnp:V01nrs:V01nsp) TITAS30C;
039200121026         IF  not %found(TITAS30C);
039300121026           ErrMessage  = *on;
039400121026           ErrGenerico = *on;
039500121026           PosCurAAS   = *on;
039600121026           V01msg      = skMsg(02);
039700121026           leavesr;
039800121026         ENDIF;
039900121026
040000121026       //?La spedizione deve avere l'immagine
040100121026         chain (TASaas:TASlnp:TASnrs:TASnsp:kAR5trd) FIAR531C;
040200121026         dAR5doc = AR5uni;
040300121026         IF  §AR5jfid <> 'I' and §AR5jfid <> 'S';
040400121026           ErrMessage  = *on;
040500121026           ErrGenerico = *on;
040600121026           PosCurAAS   = *on;
040700121026           V01msg      = skMsg(06);
040800121026           leavesr;
040900121026         ENDIF;
041000121026
041100121026       //?Ricerca sul cliente
041200121026         IF  %scan('?' : V01ksc) > 0;
041300121026           clear  Param01;
041400121026           P01ord = '0';
041500121026           KPJBU  = Param01;
041600121026           tntbJDC1r (KPJBA);
041700121026           Param01 = KPJBU;
041800121026           IF  P01ke1 <> *zero;
041900121026             V01ksc = P01ke1;
042000121026           ENDIF;
042100121026         ENDIF;
042200121026
042300121026       //?Controllo il cliente
042400121026         IF  V01ksc <> *blanks;
042500121026         //?Controllo immissione solo caratteri numerici
042600121026           IF  %check(c_Digits : V01ksc) > *zeros;
042700121026             ErrMessage  = *on;
042800121026             ErrGenerico = *on;
042900121026             PosCurksc   = *on;
043000121026             V01msg = 'Immettere solo caratteri numerici';
043100121026             leavesr;
043200121026           ENDIF;
043300121026         //?Verifica esistenza codice cliente in tabella "JDC"
043400121026           clear dJDC;
043500121026           clear TIBS02ds;
043600121026           T02mod = 'C';
043700121026           T02cod = 'JDC';
043800121026           T02sif = KNSIF;
043900121026           T02ke1 = V01ksc;
044000121026           TNTBE_RicercaControllo(kpjba : tibs02ds);
044100121026           IF T02err <> *blanks;
044200121026             ErrGenerico = *on;
044300121026             ErrMessage  = *on;
044400121026             PosCurKsc   = *on;
044500121026             V01msg = T02msg;
044600121026             leavesr;
044700121026           ENDIF;
044800121026           dJDC   = T02uni;
044900121026         ENDIF;
045000121026
045100121026       //?Se cliente variato imposto i dati a video
045200121026         IF  V01ksc > *zeros and V01ksc <> sav_V01ksc;
045300121026           V01tpi = §JDCtpi;
045400121026           V01nim = §JDCfmi;
045500121026           V01pag = §JDCpag;
045600121026           V01dir = §JDCdir;
045700121026           sav_V01ksc = V01ksc;
045800121026         ENDIF;
045900120411
046000121026       //?Tipo immagine
046100121026         IF  V01tpi = *blanks;
046200121026           ErrMessage  = *on;
046300121026           ErrGenerico = *on;
046400121026           PosCurTPI   = *on;
046500121026           V01msg      = skMsg(03);
046600121026           leavesr;
046700121026         ENDIF;
046800120411
046900121026       //?Nome immagine
047000121026         IF  V01nim = *blanks;
047100121026           ErrMessage  = *on;
047200121026           ErrGenerico = *on;
047300121026           PosCurNIM   = *on;
047400121026           V01msg      = skMsg(04);
047500121026           leavesr;
047600121026         ENDIF;
047700121026
047800121026       //?Ricerca del nome immagine
047900121026         IF  %scan('?' : V01nim) > 0;
048000121026           ErrGenerico = *on;
048100121026           PosCurNIM   = *on;
048200121026           clear TIBS02DS;
048300121026           T02mod = 'R';
048400121026           T02cod = 'NIM';
048500121026           T02sif = knsif;
048600121026           TNTBE_RicercaControllo (kpjba : tibs02ds);
048700121026           IF  T02err = *blanks;
048800121026             V01nim = T02ke1;
048900121026           ENDIF;
049000121026         ENDIF;
049100121026
049200121026       //?Il nome immagine deve esistere in tabella NIM
049300121026         clear TIBS02DS;
049400121026         T02mod = 'C';
049500121026         T02cod = 'NIM';
049600121026         T02ke1 = V01nim;
049700121026         T02sif = knsif;
049800121026         TNTBE_RicercaControllo (kpjba : tibs02ds);
049900121026         IF  T02err <> *blanks;
050000121026           ErrMessage  = *on;
050100121026           ErrGenerico = *on;
050200121026           PosCurNIM   = *on;
050300121026           V01msg      = skMsg(02);
050400121026           leavesr;
050500121026         ENDIF;
050600121026         V01dnim = T02uni;
050700120607
050800121026       //?Tipo invio
050900121026         IF  V01pag  <> 'CLI' and V01pag <> 'CL1';
051000121026           ErrMessage  = *on;
051100121026           ErrGenerico = *on;
051200121026           PosCurPAG   = *on;
051300121026           V01msg      = skMsg(07);
051400121026           leavesr;
051500121026         ENDIF;
051600120411
051700121026       //?Directory
051800121026         IF  V01dir = *blanks;
051900121026           ErrMessage  = *on;
052000121026           ErrGenerico = *on;
052100121026           PosCurDIR   = *on;
052200121026           V01msg      = skMsg(05);
052300121026           leavesr;
052400121026         ENDIF;
052500120411
052600120411       ENDSR;
052700120411
052800120411       //--------------------------------------------------------------
052900120411       //?Operazioni finali.
053000120411       //--------------------------------------------------------------
053100120411       BEGSR RoutEnd;
053200120411
053300120411         *inLR = *on;
053400120411         return;
053500120411
053600120411       ENDSR;
053700120411
053800120411      /end-free
053900120411
054000120411       //--------------------------------------------------------------
054100120411       //?Schiere a tempo di compilazione.
054200120411       //--------------------------------------------------------------
054300120411
054400120411** - skMSG ------------------------------------------------------------------*
054500120411Immettere l'anno                                                              01
054600120411Spedizione errata                                                             02
054700120606Immettere il tipo documento                                                   03
054800120606Nome documento errato                                                         04
054900120411Immettere la directory                                                        05
055000120606Documento non presente per la spedizione richiesta                            06
055100120607Tipo invio errato                                                             07
