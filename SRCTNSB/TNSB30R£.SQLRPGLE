000100130327       //==============================================================
000200130802       //?TNSB30R - Rigenerazione immagini LDV per AUTISTA
000300130327       //==============================================================
000400130326
000500130326       //--------------------------------------------------------------
000600130326       //?Specifiche di controllo.                                     ?
000700130326       //--------------------------------------------------------------
000800130326
000900130326     h decedit('0,') datedit(*ymd/) option(*nodebugio)
001000130326
001100130326       //--------------------------------------------------------------
001200130326       //?Dichiarazione file.                                          ?
001300130326       //--------------------------------------------------------------
001400130709
001500130709       // -?File Organigramma
001600130709     fAZORG01L  if   e           k disk
001700130805
001800130805       // -?File Anagrafica AUT
001900130805     fFIAPD01L  if   e           k disk
002000130710
002100130326       // -?File video
002200130802     fTNSB30D   cf   e             workstn
002300130802     f                                     sfile(SB3001S:S01nrr)
002400130326     f                                     indds(IndDspF)
002500130326     f                                     infds(InfDspF)
002600130326
002700130326       //--------------------------------------------------------------
002800130326       //?Definizione costanti.                                        ?
002900130326       //--------------------------------------------------------------
003000130326
003100130326       // -?Tasti funzionali a video
003200130326     d c_F01           c                   const(x'31')
003300130326     d c_F02           c                   const(x'32')
003400130326     d c_F03           c                   const(x'33')
003500130326     d c_F04           c                   const(x'34')
003600130326     d c_F05           c                   const(x'35')
003700130326     d c_F06           c                   const(x'36')
003800130326     d c_F07           c                   const(x'37')
003900130326     d c_F08           c                   const(x'38')
004000130326     d c_F09           c                   const(x'39')
004100130326     d c_F10           c                   const(x'3A')
004200130326     d c_F11           c                   const(x'3B')
004300130326     d c_F12           c                   const(x'3C')
004400130326     d c_F13           c                   const(x'B1')
004500130326     d c_F14           c                   const(x'B2')
004600130326     d c_F15           c                   const(x'B3')
004700130326     d c_F16           c                   const(x'B4')
004800130326     d c_F17           c                   const(x'B5')
004900130326     d c_F18           c                   const(x'B6')
005000130326     d c_F19           c                   const(x'B7')
005100130326     d c_F20           c                   const(x'B8')
005200130326     d c_F21           c                   const(x'B9')
005300130326     d c_F22           c                   const(x'BA')
005400130326     d c_F23           c                   const(x'BB')
005500130326     d c_F24           c                   const(x'BC')
005600130326     d c_Enter         c                   const(x'F1')
005700130326     d c_RollDown      c                   const(x'F4')
005800130326     d c_RollUp        c                   const(x'F5')
005900130326
006000130326     d Digits          c                   const('0123456789')
006100130326
006200130326       //--------------------------------------------------------------
006300130326       //?Definizione schiere.                                         ?
006400130326       //--------------------------------------------------------------
006500130326
006600130326      // -?Messaggi di errore
006700130328     d skMSG           s             78    dim(50) ctdata perrcd(1)
006800130326
006900130326       //--------------------------------------------------------------
007000130326       //?Definizione aree dati.                                       ?
007100130326       //--------------------------------------------------------------
007200130326
007300130326       // -?Dati utente?
007400130326     d §AzUte        e ds                  extname(AZUTE00F)
007500130326     d                                     dtaara
007600130326     d §DatiUte      e ds                  extname(dDatiUte)
007700130326     d                                     dtaara
007800130326
007900130326       //--------------------------------------------------------------
008000130326       //?Definizione strutture dati.                                  ?
008100130326       //--------------------------------------------------------------
008200130326
008300130326       // -?Status ds?
008400130326     d Status         sds
008500130326     d  SDSpgm           *proc
008600130326     d  SDSusr               254    263
008700130326
008800130326       // -?InfDS
008900130326     d InfDspF         ds
009000130326     d  dsp_aid              369    369a
009100130327     d  sfl_rrn              376    377i 0
009200130327     d  min_nrr              378    379i 0
009300130327     d  num_rcds             380    381i 0
009400130326
009500130326       // -?Indicatori su DspF
009600130326     d IndDspF         ds
009700130327       // -?Indicatori di visualizzazione Errore
009800130326     d  ErrMessage                    1n   overlay(IndDspF : 28)
009900130328       // -?Indicatori di gestione del subfile
010000130709     d  SflDsp                        1n   overlay(IndDspF : 30)
010100130709     d  SflDspCtl                     1n   overlay(IndDspF : 31)
010200130709     d  SflNxtChg                     1n   overlay(IndDspF : 32)
010300130709     d  SflEnd                        1n   overlay(IndDspF : 33)
010400130327       // -?Indicatori di Errore
010500130802     d  PosCurDTD                     1n   overlay(IndDspF : 50)
010600130802     d  PosCurDTA                     1n   overlay(IndDspF : 51)
010700130802     d  PosCurPDR1                    1n   overlay(IndDspF : 52)
010800130802     d  PosCurPDR2                    1n   overlay(IndDspF : 53)
010900130326       // -?Indicatori di errore generico
011000130326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
011100130326
011200130326       // -?DS indicatori Dspf
011300130326     d WindDspF        ds                  inz
011400130326     d   WindDspFa             1     49    inz(*zero)
011500130326     d   WindDspFb            50     99    inz(*zero)
011600130805
011700130805       // -?DS controllo data
011800130805     d wlbdat          ds                  inz
011900130805     d  g02dat                 1      8  0
012000130805     d  g02inv                 9     16  0
012100130805     d  g02err                17     17
012200130805     d  g02tgi                18     22  0
012300130805
012400130805       // -?DS codice AUT
012500130805     d                 ds
012600130805     d  V01pdr1                1      3  0
012700130805     d  V01pdr2                4      7
012800130805     d  V01pdr                 1      7
012900130326
013000130326       // -?Parametri ricevuti
013100130326     d KPJBA         e ds
013200130805
013300130805       // -?Parametri per Ricerca codice AUT
013400130805     d FNLV24DS      e ds
013500130326
013600130326       // -?Parametri per Reperimento dati utente?
013700130326     d TIBS34DS      e ds
013800130805
013900130805       // -?Parametri per Elaborazione immagini  ?
014000130805     d TNSB30DS      e ds
014100130710
014200130326       //--------------------------------------------------------------
014300130326       //?Definizione variabili globali.                               ?
014400130326       //--------------------------------------------------------------
014500130326
014600130326       // -?Flags booleani
014700130328     d wEnd            s               n   inz(*off)
014800130326     d wErrGrave       s               n   inz(*off)
014900130326     d wFine           s               n   inz(*off)
015000130802     d wInzS01         s               n   inz(*on)
015100130328
015200130328       // -?Indici di schiera
015300130328     d xx              s              4  0 inz
015400130326
015500130326       // -?Campi di Comodo
015600130802     d S01nrr          s              4  0 inz
015700130805     d wdtd            s              8  0 inz
015800130805     d wdta            s              8  0 inz
015900130802     d wMaxNrr         s              2  0 inz(10)
016000130805     d wOggi           s              8  0 inz
016100130805     d wProg           s              2  0 inz
016200130802     d wVideo          s              2    inz('S1')
016300130326
016400130326       //--------------------------------------------------------------
016500130326       //?Definizione procedure esterne.                     ?
016600130326       //--------------------------------------------------------------
016700130805
016800130805       // -?Sottomissione lavoro batch
016900130805     d bch10           pr                  extpgm('BCH10')
017000130805     d  kpjba                              likeds(KPJBA)
017100130710
017200130802       // -?Interrogazione AUTISTI
017300130802     d fnlv24r         pr                  extpgm('FNLV24R')
017400130710     d  kpjba                              likeds(KPJBA)
017500130805
017600130805       // -?Richiamao diretto lavoro batch
017700130805     d tnsb31r         pr                  extpgm('TNSB31R')
017800130805     d  kpjba                              likeds(KPJBA)
017900130326
018000130326       //--------------------------------------------------------------
018100130326       //?Definizione prototipi.
018200130326       //--------------------------------------------------------------
018300130328
018400130328       // -?Reperimento dati utente?
018500130326      /copy gaitrasrc/srcprotopr,tibs34r
018600130805
018700130805       // -?Controllo data
018800130805      /copy gaitrasrc/srcprotopr,xsrda8
018900130326
019000130326       //--------------------------------------------------------------
019100130326       //?Definizione key-list.                                        ?
019200130326       //--------------------------------------------------------------
019300130326
019400130326       //--------------------------------------------------------------
019500130326       //?Indicatori.
019600130326       //--------------------------------------------------------------
019700130327       // IN   =
019800130326
019900130326       //--------------------------------------------------------------
020000130326       //?M A I N - L I N E                                            ?
020100130326       //--------------------------------------------------------------
020200130326
020300130326     c     *Entry        plist
020400130326     c                   parm                    KPJBA
020500130326
020600130326      /free
020700130326
020800130328       //?Operazioni iniziali?
020900130326       exsr RoutInz;
021000130326
021100130328       //?Gestione vidate?
021200130326       DOW not wFine;
021300130326         SELECT;
021400130802           WHEN  wVideo = 'S1';
021500130802             exsr GesS01;
021600130326           OTHER;
021700130326             wFine = *on;
021800130326         ENDSL;
021900130326       ENDDO;
022000130326
022100130328       //?Operazioni finali?
022200130326       exsr RoutEnd;
022300130326
022400130326       //--------------------------------------------------------------
022500130326       //?Operazioni iniziali.                                         ?
022600130326       //--------------------------------------------------------------
022700130326       BEGSR  RoutInz;
022800130326
022900130326       //?Reperimento dati job?
023000130326         exsr  DatiJob;
023100130328
023200130328         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
023300130327
023400130327       //?Imposto nome pgm a video
023500130327         V00pgm = SDSpgm;
023600130805
023700130805       //?Data del giorno
023800130805         wOggi = %dec( %date() );
023900130405
024000130326       ENDSR;
024100130326
024200130326       //--------------------------------------------------------------
024300130326       //?Reperimento Dati del job (Utente/Operativi).                 ?
024400130326       //--------------------------------------------------------------
024500130326       BEGSR  DatiJob;
024600130326
024700130326         in(E) §AzUte;
024800130326         IF  NOT %error;
024900130326           in(E) §DatiUte;
025000130326         ENDIF;
025100130326         IF  %error or RSut = *blanks;
025200130326           clear TIBS34DS;
025300130326           tibs34r ( TIBS34DS );
025400130326           in §AzUte;
025500130326           in §DatiUte;
025600130326         ENDIF;
025700130326
025800130326       ENDSR;
025900130328
026000130802       //--------------------------------------------------------------
026100130802       //?Gestione videata S01.
026200130802       //--------------------------------------------------------------
026300130802       BEGSR  GesS01;
026400130802
026500130802       //?Inizializzazione videata
026600130802         IF  wInzS01;
026700130805           exsr InzS01;
026800130805           PosCurDTD   = *on;
026900130802           wInzS01 = *off;
027000130802         //?Per errore durante l'inizializzazione delle videta
027100130802         //?esco dal programma
027200130802           IF  ErrGenerico;
027300130802             wFine = *on;
027400130802             leavesr;
027500130802           ENDIF;
027600130802         ENDIF;
027700130802
027800130802       //?Emissione videata
027900130805         write SB3000T;
028000130802         write SB3001Z;
028100130802         exfmt SB3001C;
028200130802         ErrMessage  = *off;
028300130802         ErrGenerico = *off;
028400130802         clear V01msg;
028500130802
028600130802         SELECT;
028700130802
028800130802       //?F3=Fine
028900130802           WHEN  dsp_aid = c_F03;
029000130802             exsr F03S01;
029100130802
029200130802       //?F6=Conferma
029300130802           WHEN  dsp_aid = c_F06;
029400130802           //?Eseguo i controlli control
029500130802             exsr CtrC01 ;
029600130802             IF  ErrGenerico;
029700130802               leavesr;
029800130802             ENDIF;
029900130802           //?Eseguo i controlli subfile
030000130802             exsr CtrS01 ;
030100130802             IF  ErrGenerico;
030200130802               leavesr;
030300130802             ENDIF;
030400130802             exsr F06S01;
030500130802             exsr F03S01;
030600130802
030700130802       //?Enter
030800130802           OTHER;
030900130802           //?Eseguo i controlli control
031000130802             exsr CtrC01 ;
031100130802             IF  ErrGenerico;
031200130802               leavesr;
031300130802             ENDIF;
031400130802         //?Eseguo i controlli subfile
031500130802             exsr CtrS01;
031600130802             IF  ErrGenerico;
031700130802               leavesr;
031800130802             ENDIF;
031900130802
032000130802         ENDSL;
032100130802
032200130802       ENDSR;
032300130802
032400130802       //--------------------------------------------------------------
032500130802       //?Inizializzazione videata S01.
032600130802       //--------------------------------------------------------------
032700130802       BEGSR InzS01;
032800130802
032900130802       //?Pulizia del Subfile
033000130802         exsr PulS01;
033100130802
033200130802       //?Carico 10 righe vuote
033300130802         FOR xx by 1 to wMaxNrr;
033400130802           exsr CarS01;
033500130802         ENDFOR;
033600130802
033700130802       //?Visualizzazione del Subfile
033800130802         SflDsp = (S01nrr <= *zeros);
033900130802
034000130802       //?Attivazione SFLEND
034100130802         SflEnd = *on;
034200130802
034300130802       //?Impaginazione Subfile
034400130802       //?Forzo sempre il primo rcd
034500130802         IF  S01nrr > *zero;
034600130802           V01rcd = 1;
034700130802           V01csr = 1;
034800130802         ELSE;
034900130802           clear V01rcd;
035000130802         ENDIF;
035100130802
035200130802         V01csr = V01rcd;
035300130802
035400130802       ENDSR;
035500130802
035600130802       //--------------------------------------------------------------
035700130802       //?Pulizia Subfile S01.
035800130802       //--------------------------------------------------------------
035900130802       BEGSR PulS01;
036000130802
036100130802         SflDsp    = *on;
036200130802         SflDspCtl = *on;
036300130802         write  SB3001C;
036400130802         SflDspCtl = *off;
036500130802         SflEnd    = *off;
036600130802
036700130802         clear V01rcd;
036800130802         clear V01csr;
036900130802         clear S01nrr;
037000130802         clear V01msg;
037100130802         ErrMessage  = *off;
037200130802         ErrGenerico = *off;
037300130802
037400130802         WindDspF = IndDspF;
037500130802         reset WindDspFb;
037600130802         IndDspF  = WindDspF;
037700130802
037800130802       ENDSR;
037900130802
038000130802       //--------------------------------------------------------------
038100130802       //?Carico i dati nel SUBFILE.
038200130802       //--------------------------------------------------------------
038300130802       BEGSR CarS01;
038400130802
038500130802         clear V01pdr1;
038600130802         clear V01pdr2;
038700130802         clear V01pdrd;
038800130802         S01nrr += 1;
038900130802         write SB3001S;
039000130802
039100130802       ENDSR;
039200130805
039300130805       //--------------------------------------------------------------
039400130805       //?Gestione tasto funzionale F3.
039500130805       //?F3=Fine
039600130805       //--------------------------------------------------------------
039700130805       BEGSR F03S01;
039800130805
039900130805       //?Chiusura del programma
040000130805         wFine = *on;
040100130805
040200130805       ENDSR;
040300130802
040400130802       //--------------------------------------------------------------
040500130802       //?Gestione tasto funzionale F6.
040600130802       //?F6=Conferma.
040700130802       //--------------------------------------------------------------
040800130802       BEGSR F06S01;
040900130802
041000130805       //?Lancio l'estrazione delle immagini per ogni AUT immesso
041100130802         S01nrr = 1;
041200130802         FOR S01nrr by 1 to wMaxNrr;
041300130802       //?Aggiorno i dati in base a quanto inserito nel subfile
041400130802           chain S01nrr SB3001S;
041500130802           IF  %found;
041600130802
041700130805           //?Schedulo elaborazione
041800130802             IF  V01pdr1 > *zeros;
041900130805               wProg = S01nrr;
042000130802               exsr Elabora;
042100130802             ENDIF;
042200130802
042300130802             V01csr = S01nrr;
042400130802             V01rcd = S01nrr;
042500130802             UPDATE SB3001S;
042600130802           ENDIF;
042700130802         ENDFOR;
042800130802
042900130802       ENDSR;
043000130805
043100130805       //--------------------------------------------------------------
043200130805       //?Controllo videata C01.
043300130805       //--------------------------------------------------------------
043400130805       BEGSR CtrC01;
043500130805
043600130805         WindDspF = IndDspF;
043700130805         reset WindDspFb;
043800130805         IndDspF  = WindDspF;
043900130805
044000130805       //?Controllo cosa è stato inserito nel control
044100130805
044200130805         IF  V01dtd = *zeros;
044300130805           ErrMessage  = *on;
044400130805           ErrGenerico = *on;
044500130805           PosCurDTD   = *on;
044600130805           V01msg      = skMSG(01);
044700130805           leavesr;
044800130805         ENDIF;
044900130805
045000130805         clear wlbdat;
045100130805         G02dat = V01dtd;
045200130805         xsrda8(wlbdat);
045300130805         IF  G02err = '1';
045400130805           ErrMessage  = *on;
045500130805           ErrGenerico = *on;
045600130805           PosCurDTD   = *on;
045700130805           V01msg      = skMSG(01);
045800130805           leavesr;
045900130805         ENDIF;
046000130805
046100130805         V01dtd = G02dat;
046200130805         wdtd   = G02inv;
046300130805
046400130805         IF  V01dta = *zeros;
046500130805           ErrMessage  = *on;
046600130805           ErrGenerico = *on;
046700130805           PosCurDTA   = *on;
046800130805           V01msg      = skMSG(01);
046900130805           leavesr;
047000130805         ENDIF;
047100130805
047200130805         clear wlbdat;
047300130805         G02dat = V01dta;
047400130805         xsrda8(wlbdat);
047500130805         IF  G02err = '1';
047600130805           ErrMessage  = *on;
047700130805           ErrGenerico = *on;
047800130805           PosCurDTA   = *on;
047900130805           V01msg      = skMSG(01);
048000130805           leavesr;
048100130805         ENDIF;
048200130805
048300130805         V01dta = G02dat;
048400130805         wdta   = G02inv;
048500130805
048600130805         IF  wdtd > wdta;
048700130805           ErrMessage  = *on;
048800130805           ErrGenerico = *on;
048900130805           PosCurDTA   = *on;
049000130805           V01msg      = skMSG(02);
049100130805           leavesr;
049200130805         ENDIF;
049300130805
049400130805       ENDSR;
049500130327
049600130327       //--------------------------------------------------------------
049700130802       //?Controllo videata S01.
049800130327       //--------------------------------------------------------------
049900130802       BEGSR CtrS01;
050000130327
050100130328         WindDspF = IndDspF;
050200130328         reset WindDspFb;
050300130328         IndDspF  = WindDspF;
050400130328
050500130328       //?Controllo cosa è stato inserito nel subfile
050600130802         readc SB3001S;
050700130328
050800130802         DOW  not %eof(TNSB30D);
050900130802           %subst(IndDspF : 52) = *off;
051000130802           %subst(IndDspF : 53) = *off;
051100130709           SflNxtChg = *off;
051200130802           V01rcd = S01nrr;
051300130328
051400130328       //?Controllo se opzione valida
051500130328           SELECT;
051600130802             //?Nessun AUTISTA
051700130805             WHEN  V01pdr1 = *zeros and V01pdr2 = *blanks;
051800130802             //?Tutti e 2 i campi dell'autista
051900130802             WHEN  V01pdr1 = *zeros and V01pdr2 <> *blanks;
052000130710                 ErrMessage  = *on;
052100130710                 ErrGenerico = *on;
052200130802                 PosCurPDR1  = *on;
052300130805                 V01msg = skMSG(03);
052400130802             WHEN  V01pdr1 > *zeros and V01pdr2 = *blanks;
052500130802                 ErrMessage  = *on;
052600130802                 ErrGenerico = *on;
052700130802                 PosCurPDR2  = *on;
052800130805                 V01msg = skMSG(03);
052900130802             OTHER;
053000130802             //?Controllo se OK la Filiale AUT
053100130805               exsr ctrFIL;
053200130802             //?Controllo AUT
053300130805               IF  not ErrGenerico;
053400130805                 exsr ctrAUT;
053500130805               ENDIF;
053600130328           ENDSL;
053700130328
053800130328       //?Aggiorno il subfile
053900130328           SELECT;
054000130328             WHEN  ErrMessage;
054100130709               SflNxtChg = *on;
054200130802               V01csr    = V01rcd;
054300130328             WHEN  ErrGenerico;
054400130802               V01csr = V01rcd;
054500130328             OTHER;
054600130802               V01csr = V01rcd;
054700130328           ENDSL;
054800130328
054900130802           UPDATE SB3001S;
055000130328
055100130328       //?Al primo errore esco dalla lettura del subfile
055200130328           IF  ErrMessage or ErrGenerico;
055300130328             leave;
055400130328           ENDIF;
055500130328
055600130802           readc SB3001S;
055700130328
055800130328         ENDDO;
055900130327
056000130327       ENDSR;
056100130805
056200130805       //--------------------------------------------------------------
056300130805       //?Controllo la filiale AUTISTA.
056400130805       //--------------------------------------------------------------
056500130805       BEGSR ctrFIL;
056600130805
056700130805         IF  V01pdr1 = 0;
056800130805           leavesr;
056900130805         ENDIF;
057000130805
057100130805         chain (V01pdr1) AZORG01L;
057200130805         IF  not %found(AZORG01L) or ORGfva <> *blanks or
057300130805            (ORGfag <> 'A' and ORGfag <> 'F');
057400130805           ErrMessage  = *on;
057500130805           ErrGenerico = *on;
057600130805           PosCurPDR1  = *on;
057700130805           V01msg = skMSG(04);
057800130805           leavesr;
057900130805         ENDIF;
058000130805
058100130805       ENDSR;
058200130329
058300130805       //--------------------------------------------------------------
058400130805       //?Controllo il codice AUTISTA.
058500130805       //--------------------------------------------------------------
058600130805       BEGSR ctrAUT;
058700130805
058800130805         IF  V01pdr2 = *blanks;
058900130805           leavesr;
059000130805         ENDIF;
059100130805
059200130805         clear FNLV24DS;
059300130805         IF  %scan('?' : V01pdr2) > *zero;
059400130805           D24fil = V01pdr1;
059500130805           D24flg = 'R';
059600130805           D24tip = 'A';
059700130805           kpjbu = FNLV24DS;
059800130805           fnlv24r (KPJBA);
059900130805           FNLV24DS = kpjbu;
060000130805           V01pdr2 = %subst(%editc(D24pdr:'X'):4:4);
060100130805           V01pdrd = D24rsc;
060200130805           leavesr;
060300130805         ENDIF;
060400130805
060500130805         APDtip = 'A';
060600130805         APDpdr = %dec(V01pdr:7:0);
060700130805         chain (APDtip:APDpdr) FIAPD01L;
060800131004         IF  not %found(FIAPD01L) ;
060900130805           ErrMessage  = *on;
061000130805           ErrGenerico = *on;
061100130805           PosCurPDR1  = *on;
061200130805           V01msg = skMSG(04);
061300130805           leavesr;
061400130805         ENDIF;
061500130805         V01pdrd = APDrsf;
061600130805
061700130805       ENDSR;
061800130805
061900130805       //--------------------------------------------------------------
062000130805       //?Sottomette elaborazione.
062100130805       //--------------------------------------------------------------
062200130805       BEGSR Elabora;
062300130805
062400130805         clear TNSB30DS;
062500130805         ISB30dtd = wdtd;
062600130805         ISB30dta = wdta;
062700130805         ISB30pdr = %dec(V01pdr:7:0);
062800130805         ISB30idjob = %editc(wOggi:'X') + %editc(wProg:'X');
062900130805
063000130805         kcoaz = 'SB31';
063100130805         kpjbu = TNSB30DS;
063200130805         if  knmus = *all'1';
063300130805           tnsb31r(kpjba);
063400130805         else;
063500130805           BCH10(kpjba);
063600130805         endif;
063700130805
063800130805       ENDSR;
063900130805
064000130327       //--------------------------------------------------------------
064100130327       //?Operazioni finali.
064200130327       //--------------------------------------------------------------
064300130327       BEGSR RoutEnd;
064400130327
064500130327         *inLR = *on;
064600130327         return;
064700130327
064800130327       ENDSR;
064900130327
065000130327      /end-free
065100130326
065200130326       //--------------------------------------------------------------
065300130326       //?Schiere a tempo di compilazione.
065400130326       //--------------------------------------------------------------
065500130326
065600130326** - skMSG ------------------------------------------------------------------*
065700130805Data errata                                                                    01
065800130805Data "DAL" incongruente con data "AL"                                          02
065900130805Codice Autista incompleto                                                      03
066000130805Codice errato                                                                  04
