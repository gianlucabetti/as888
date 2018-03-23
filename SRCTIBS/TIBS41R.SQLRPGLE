000100130327       //==============================================================
000200130709       //?TIBS41R - Creazione nuovi utenti da profilo STD
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
001700130326
001800130709       // -?File Utenti
001900130709     fAZUTE00F  if a e             disk
002000130709     fAZUTE01L  if   e           k disk    rename(AZUTE000:AZUTE01)
002100130709     fAZUTE02L  if   e           k disk    rename(AZUTE000:AZUTE02)
002200130709     f                                     prefix(x:1)
002300130710
002400130710       // -?Modulo Base
002500130710     fKPPRF01L  if a e           k disk
002600130326
002700130326       // -?File video
002800130709     fTIBS41D   cf   e             workstn
002900130709     f                                     sfile(BS4102S:S02nrr)
003000130326     f                                     indds(IndDspF)
003100130326     f                                     infds(InfDspF)
003200130326
003300130326       //--------------------------------------------------------------
003400130326       //?Definizione costanti.                                        ?
003500130326       //--------------------------------------------------------------
003600130326
003700130326       // -?Tasti funzionali a video
003800130326     d c_F01           c                   const(x'31')
003900130326     d c_F02           c                   const(x'32')
004000130326     d c_F03           c                   const(x'33')
004100130326     d c_F04           c                   const(x'34')
004200130326     d c_F05           c                   const(x'35')
004300130326     d c_F06           c                   const(x'36')
004400130326     d c_F07           c                   const(x'37')
004500130326     d c_F08           c                   const(x'38')
004600130326     d c_F09           c                   const(x'39')
004700130326     d c_F10           c                   const(x'3A')
004800130326     d c_F11           c                   const(x'3B')
004900130326     d c_F12           c                   const(x'3C')
005000130326     d c_F13           c                   const(x'B1')
005100130326     d c_F14           c                   const(x'B2')
005200130326     d c_F15           c                   const(x'B3')
005300130326     d c_F16           c                   const(x'B4')
005400130326     d c_F17           c                   const(x'B5')
005500130326     d c_F18           c                   const(x'B6')
005600130326     d c_F19           c                   const(x'B7')
005700130326     d c_F20           c                   const(x'B8')
005800130326     d c_F21           c                   const(x'B9')
005900130326     d c_F22           c                   const(x'BA')
006000130326     d c_F23           c                   const(x'BB')
006100130326     d c_F24           c                   const(x'BC')
006200130326     d c_Enter         c                   const(x'F1')
006300130326     d c_RollDown      c                   const(x'F4')
006400130326     d c_RollUp        c                   const(x'F5')
006500130326
006600130326     d Digits          c                   const('0123456789')
006700130326
006800130326       //--------------------------------------------------------------
006900130326       //?Definizione schiere.                                         ?
007000130326       //--------------------------------------------------------------
007100130328
007200130328       // -?Schiere per controllo codici già presenti sul file
007300130327
007400130327     d ksa             s              4    dim(30)
007500130326
007600130326      // -?Messaggi di errore
007700130328     d skMSG           s             78    dim(50) ctdata perrcd(1)
007800130326
007900130326       //--------------------------------------------------------------
008000130326       //?Definizione aree dati.                                       ?
008100130326       //--------------------------------------------------------------
008200130326
008300130326       // -?Dati utente?
008400130326     d §AzUte        e ds                  extname(AZUTE00F)
008500130326     d                                     dtaara
008600130326     d §DatiUte      e ds                  extname(dDatiUte)
008700130326     d                                     dtaara
008800130326
008900130326       //--------------------------------------------------------------
009000130326       //?Definizione strutture dati.                                  ?
009100130326       //--------------------------------------------------------------
009200130326
009300130326       // -?Status ds?
009400130326     d Status         sds
009500130326     d  SDSpgm           *proc
009600130326     d  SDSusr               254    263
009700130326
009800130326       // -?InfDS
009900130326     d InfDspF         ds
010000130326     d  dsp_aid              369    369a
010100130327     d  sfl_rrn              376    377i 0
010200130327     d  min_nrr              378    379i 0
010300130327     d  num_rcds             380    381i 0
010400130326
010500130326       // -?Indicatori su DspF
010600130326     d IndDspF         ds
010700130327       // -?Indicatori di visualizzazione Errore
010800130326     d  ErrMessage                    1n   overlay(IndDspF : 28)
010900130328       // -?Indicatori di gestione del subfile
011000130709     d  SflDsp                        1n   overlay(IndDspF : 30)
011100130709     d  SflDspCtl                     1n   overlay(IndDspF : 31)
011200130709     d  SflNxtChg                     1n   overlay(IndDspF : 32)
011300130709     d  SflEnd                        1n   overlay(IndDspF : 33)
011400130327       // -?Indicatori di Errore
011500130709     d  PosCurFIL                     1n   overlay(IndDspF : 50)
011600130709     d  PosCurOPZ                     1n   overlay(IndDspF : 51)
011700130710     d  PosCurNRUTE                   1n   overlay(IndDspF : 52)
011800130326       // -?Indicatori di errore generico
011900130326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012000130326
012100130326       // -?DS indicatori Dspf
012200130326     d WindDspF        ds                  inz
012300130326     d   WindDspFa             1     49    inz(*zero)
012400130326     d   WindDspFb            50     99    inz(*zero)
012500130326
012600130326       // -?Parametri ricevuti
012700130326     d KPJBA         e ds
012800130326
012900130326       // -?Parametri per Reperimento dati utente?
013000130326     d TIBS34DS      e ds
013100130710
013200130710       // -?Recupero liberire CURLIB da tabella CUR
013300130710     d TIBS40DS      e ds
013400130710     d  wfil                   9   1808  0 dim(600)
013500130710     d  wlib                1809   7808    dim(600)
013600130326
013700130710       // -?Campo UTEfa1 AZUTE00F
013800130710     d dUTE02        e ds                  inz
013900130710
014000130326       //--------------------------------------------------------------
014100130326       //?Definizione variabili globali.                               ?
014200130326       //--------------------------------------------------------------
014300130326
014400130326       // -?Flags booleani
014500130328     d wEnd            s               n   inz(*off)
014600130326     d wErrGrave       s               n   inz(*off)
014700130326     d wFine           s               n   inz(*off)
014800130326     d wInzD01         s               n   inz(*on)
014900130327     d wInzS02         s               n   inz(*off)
015000130328
015100130328       // -?Indici di schiera
015200130328     d xx              s              4  0 inz
015300130326
015400130326       // -?Campi di Comodo
015500130329     d sav_S02nrr      s                   like(S02nrr)
015600130327     d S02nrr          s              4  0 inz
015700130710     d wCdu            s                   like(UTEcdu)
015800130710     d wKcdu           s                   like(UTEcdu)
015900130709     d wProgr          s              2  0 inz
016000130709     d wUte            s             10    inz
016100130328     d wVideo          s              2    inz('D1')
016200130710
016300130710       // -?Campi per TIBS32C
016400130710     d w32tela         s              3a   inz('CRT')
016500130710     d w32moba         s              1a   inz('S')
016600130710     d w32ute          s             10a
016700130710     d w32text         s             50a
016800130710     d w32curlib       s             10a
016900130710     d w32atnpgm       s             10a   inz('TNSY87C')
017000130710     d w32lib          s             10a   inz('GAITRAOBJ')
017100130710     d w32pswexp       s              4a   inz('*YES')
017200130710     d w32psw          s             10a
017300130710     d w32status       s              9a   inz('*ENABLED')
017400130710     d w32err          s              1a   inz
017500130326
017600130326       //--------------------------------------------------------------
017700130326       //?Definizione procedure esterne.                     ?
017800130326       //--------------------------------------------------------------
017900130710
018000130710       // -?Gestione profili
018100130710     d tibs30r         pr                  extpgm('TIBS30R')
018200130710     d  kpjba                              likeds(KPJBA)
018300130326
018400130710       // -?Modulo base AS400
018500130710     d tibs32c         pr                  extpgm('TIBS32C')
018600130710     d  w32tela                       3a
018700130710     d  w32moba                       1a
018800130710     d  w32ute                       10a
018900130710     d  w32text                      50a
019000130710     d  w32curlib                    10a
019100130710     d  w32atnpgm                    10a
019200130710     d  w32lib                       10a
019300130710     d  w32pswexp                     4a
019400130710     d  w32psw                       10a
019500130710     d  w32status                     9a
019600130710     d  w32err                        1a
019700130710
019800130710       // -?Curlib da tabella CUR
019900130710     d tibs40r         pr                  extpgm('TIBS40R')
020000130710     d  kpjba                              likeds(KPJBA)
020100130710     d  tibs40ds                           likeds(TIBS40DS)
020200130326
020300130326       //--------------------------------------------------------------
020400130326       //?Definizione prototipi.
020500130326       //--------------------------------------------------------------
020600130328
020700130328       // -?Reperimento dati utente?
020800130326      /copy gaitrasrc/srcprotopr,tibs34r
020900130326
021000130326       //--------------------------------------------------------------
021100130326       //?Definizione key-list.                                        ?
021200130326       //--------------------------------------------------------------
021300130326
021400130326       //--------------------------------------------------------------
021500130326       //?Indicatori.
021600130326       //--------------------------------------------------------------
021700130327       // IN   =
021800130326
021900130326       //--------------------------------------------------------------
022000130326       //?M A I N - L I N E                                            ?
022100130326       //--------------------------------------------------------------
022200130326
022300130326     c     *Entry        plist
022400130326     c                   parm                    KPJBA
022500130326
022600130326      /free
022700130326
022800130328       //?Operazioni iniziali?
022900130326       exsr RoutInz;
023000130326
023100130328       //?Gestione vidate?
023200130326       DOW not wFine;
023300130326         SELECT;
023400130326           WHEN  wVideo = 'D1';
023500130326             exsr GesD01;
023600130327           WHEN  wVideo = 'S2';
023700130327             exsr GesS02;
023800130326           OTHER;
023900130326             wFine = *on;
024000130326         ENDSL;
024100130326       ENDDO;
024200130326
024300130328       //?Operazioni finali?
024400130326       exsr RoutEnd;
024500130326
024600130326       //--------------------------------------------------------------
024700130326       //?Operazioni iniziali.                                         ?
024800130326       //--------------------------------------------------------------
024900130326       BEGSR  RoutInz;
025000130326
025100130326       //?Reperimento dati job?
025200130326         exsr  DatiJob;
025300130328
025400130328         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
025500130327
025600130327       //?Imposto nome pgm a video
025700130327         V00pgm = SDSpgm;
025800130710
025900130710       //?Recupero le CURLIB da tabella CUR
026000130710         clear TIBS40DS;
026100130710         tibs40r (kpjba:TIBS40DS);
026200130405
026300130326       ENDSR;
026400130326
026500130326       //--------------------------------------------------------------
026600130326       //?Reperimento Dati del job (Utente/Operativi).                 ?
026700130326       //--------------------------------------------------------------
026800130326       BEGSR  DatiJob;
026900130326
027000130326         in(E) §AzUte;
027100130326         IF  NOT %error;
027200130326           in(E) §DatiUte;
027300130326         ENDIF;
027400130326         IF  %error or RSut = *blanks;
027500130326           clear TIBS34DS;
027600130326           tibs34r ( TIBS34DS );
027700130326           in §AzUte;
027800130326           in §DatiUte;
027900130326         ENDIF;
028000130326
028100130326       ENDSR;
028200130328
028300130326       //--------------------------------------------------------------
028400130327       //?Gestione videata D01.
028500130326       //--------------------------------------------------------------
028600130326       BEGSR  GesD01;
028700130326
028800130326       //?Inizializzazione videata
028900130326         IF  wInzD01;
029000130326           exsr Inzd01;
029100130326           wInzD01 = *off;
029200130326         ENDIF;
029300130326
029400130328       //?Per errore grave messaggio all'utente
029500130326         IF  wErrGrave;
029600130328           V01msg = skMSG(01);
029700130326           ErrMessage  = *on;
029800130326           Errgenerico = *on;
029900130326         ENDIF;
030000130326
030100130326       //?Emissione videata
030200130709         write BS4100T;
030300130709         exfmt BS4101D;
030400130326         ErrMessage  = *off;
030500130326         ErrGenerico = *off;
030600130326         clear V01msg;
030700130326
030800130326         SELECT;
030900130326
031000130326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
031100130326           WHEN  wErrGrave;
031200130326             wFine = *on;
031300130326
031400130326       //?F3=Fine
031500130326           WHEN  dsp_aid = c_F03;
031600130326             exsr F03D01;
031700130710
031800130326       //?Enter
031900130326           OTHER;
032000130326             exsr CtrD01;
032100130326             IF  ErrGenerico;
032200130326               leavesr;
032300130326             ENDIF;
032400170803             exsr F06D01;
032500130326
032600130326         ENDSL;
032700130326
032800130326       ENDSR;
032900130326
033000130326       //--------------------------------------------------------------
033100130326       //?Inizializzazione videata.
033200130326       //--------------------------------------------------------------
033300130326       BEGSR InzD01;
033400130327
033500130709         clear BS4101D;
033600130326
033700130326       ENDSR;
033800130326
033900130326       //--------------------------------------------------------------
034000130326       //?Gestione tasto funzionale F3.
034100130326       //?F3=Fine
034200130326       //--------------------------------------------------------------
034300130326       BEGSR F03D01;
034400130326
034500130326       //?Chiusura del programma
034600130326         wFine = *on;
034700130326
034800130326       ENDSR;
034900130710
035000130710       //--------------------------------------------------------------
035100130710       //?Gestione tasto funzionale F6.
035200130710       //?F6=Conferma
035300130710       //--------------------------------------------------------------
035400130710       BEGSR F06D01;
035500130710
035600130710       //?Passo al subfile
035700130710           wVideo  = 'S2';
035800130710           wInzS02 = *on;
035900130710
036000130710       //?Cerco la CURLIB
036100130710         clear xx;
036200130710         xx = %lookup(V01fil:wfil);
036300130710         IF  xx > *zeros;
036400130710           w32curlib = wlib(xx);
036500130710         ELSE;
036600130710           w32curlib = '*CRTDFT';
036700130710         ENDIF;
036800130710
036900130710       ENDSR;
037000130326
037100130326       //--------------------------------------------------------------
037200130326       //?Controllo videata.
037300130326       //--------------------------------------------------------------
037400130326       BEGSR CtrD01;
037500130326
037600130326         WindDspF = IndDspF;
037700130326         reset WindDspFb;
037800130326         IndDspF  = WindDspF;
037900130326
038000130709       //?Filiale
038100130327       //?obbligatorio
038200130710         IF  V01fil = *zeros;
038300130326           ErrMessage  = *on;
038400130326           ErrGenerico = *on;
038500130709           PosCurFIL   = *on;
038600130709           V01msg      = skMSG(01);
038700130326           leavesr;
038800130326         ENDIF;
038900130327
039000130709       //?filiale valida
039100130709         clear  V01fild;
039200130709         chain V01fil AZORG01L;
039300130709         IF  not %found(AZORG01L) or ORGfva <> *blanks or
039400130709             (ORGfag <> 'F' and ORGfag <> 'A');
039500130327           ErrMessage  = *on;
039600130327           ErrGenerico = *on;
039700130709           PosCurFIL   = *on;
039800130709           V01msg      = skMSG(01);
039900130327           leavesr;
040000130327         ENDIF;
040100130709         V01fild = ORGdes;
040200130327
040300130326       ENDSR;
040400130327
040500130327       //--------------------------------------------------------------
040600130327       //?Gestione videata S02.
040700130327       //--------------------------------------------------------------
040800130327       BEGSR  GesS02;
040900130327
041000130327       //?Inizializzazione videata
041100130327         IF  wInzS02;
041200130327           exsr InzS02;
041300130327           wInzS02 = *off;
041400130402         //?Per errore durante l'inizializzazione delle videta
041500130402         //?torno alla prima con errore impostato
041600130402           IF  ErrGenerico;
041700130402             wVideo  = 'D1';
041800130402             leavesr;
041900130402           ENDIF;
042000130402         ENDIF;
042100130327
042200130327       //?Emissione videata
042300130709         write BS4102Z;
042400130709         exfmt BS4102C;
042500130327         ErrMessage  = *off;
042600130327         ErrGenerico = *off;
042700130328         clear V02msg;
042800130327
042900130327         SELECT;
043000130328
043100130328       //?F3=Fine
043200130328           WHEN  dsp_aid = c_F03;
043300130328             exsr F03D01;
043400130327
043500130327       //?F6=Conferma
043600130327           WHEN  dsp_aid = c_F06;
043700130328           //?Eseguo i controlli
043800130328             exsr CtrS02 ;
043900130328             IF  ErrGenerico;
044000130328               leavesr;
044100130328             ENDIF;
044200130327             exsr F06S02;
044300130329             exsr F12S02;
044400130327
044500130327       //?F12=Ritorno
044600130327           WHEN  dsp_aid = c_F12;
044700130327             exsr F12S02;
044800130327
044900130327       //?Enter
045000130327           OTHER;
045100130328         //?Eseguo i controlli
045200130327             exsr CtrS02;
045300130327             IF  ErrGenerico;
045400130327               leavesr;
045500130327             ENDIF;
045600130327
045700130327         ENDSL;
045800130327
045900130327       ENDSR;
046000130327
046100130327       //--------------------------------------------------------------
046200130327       //?Inizializzazione videata S02.
046300130327       //--------------------------------------------------------------
046400130327       BEGSR InzS02;
046500130328
046600130328       //?Pulizia del Subfile
046700130328         exsr PulS02;
046800130327
046900130709       //?carico gli utenti STANDARD
047000130709         wEnd = *off;
047100130709         exec sql
047200130709         DECLARE  AZUTE cursor for
047300130709         SELECT UTEute, UTEfa1, PRFtxt from AZUTE00F, KPPRF00F
047400170803         WHERE substr(UTEfa1, 1, 1) = 'S' and UTEute = PRFnmu
047500170803         ORDER BY UTEute;
047600130709
047700130709       //?Apertura del cursore
047800130709         exec sql
047900130709         OPEN AZUTE;
048000130709
048100130709         IF  sqlcode < 0;
048200130709           wEnd = *on;
048300130709         ENDIF;
048400130709
048500130709         DOW  not wEnd;
048600130709           exec sql
048700130709           FETCH next from AZUTE into :UTEute, :UTEfa1,
048800130709                                      :PRFtxt;
048900130709           IF  sqlcod = 100 or sqlcod < 0;
049000130709             wEnd = *on;
049100130709             leave;
049200130709           ENDIF;
049300130709
049400130709           exsr CarS02;
049500130709         ENDDO;
049600130709
049700130709       //?Chiusura del cursore
049800130709         exec sql
049900130709         CLOSE AZUTE;
050000130328
050100130328       //?Visualizzazione del Subfile
050200130709         SflDsp = (S02nrr <= *zeros);
050300130329         sav_S02nrr = S02nrr;
050400130328
050500130328       //?Attivazione SFLEND
050600130709         SflEnd = *on;
050700130328
050800130328       //?Impaginazione Subfile
050900130328       //?Forzo sempre il primo rcd
051000130328         IF  S02nrr > *zero;
051100130328           V02rcd = 1;
051200130328           V02csr = 1;
051300130328         ELSE;
051400130328           clear V02rcd;
051500130328         ENDIF;
051600130328
051700130328         V02csr = V02rcd;
051800130327
051900130327       ENDSR;
052000130328
052100130328       //--------------------------------------------------------------
052200130329       //?Pulizia Subfile S02.
052300130328       //--------------------------------------------------------------
052400130328       BEGSR PulS02;
052500130328
052600130709         SflDsp    = *on;
052700130709         SflDspCtl = *on;
052800130709         write  BS4102C;
052900130709         SflDspCtl = *off;
053000130709         SflEnd    = *off;
053100130328
053200130328         clear V02rcd;
053300130328         clear V02csr;
053400130328         clear S02nrr;
053500130328         clear V02msg;
053600130328         ErrMessage  = *off;
053700130328         ErrGenerico = *off;
053800130328
053900130328         WindDspF = IndDspF;
054000130328         reset WindDspFb;
054100130328         IndDspF  = WindDspF;
054200130328
054300130328       ENDSR;
054400130328
054500130328       //--------------------------------------------------------------
054600130709       //?Carico i dati nel SUBFILE.
054700130328       //--------------------------------------------------------------
054800130328       BEGSR CarS02;
054900130328
055000130709         clear V02opz;
055100130709         V02std = UTEute;
055200130709         V02stdd = PRFtxt;
055300130709         clear V02nrute;
055400130709         S02nrr += 1;
055500130709         write BS4102S;
055600130328
055700130328       ENDSR;
055800130327
055900130327       //--------------------------------------------------------------
056000130327       //?Gestione tasto funzionale F6.
056100130327       //?F6=Conferma.
056200130327       //--------------------------------------------------------------
056300130327       BEGSR F06S02;
056400130327
056500130709       //?Scrivo i nuovi utenti
056600130329         S02nrr = 1;
056700130329         FOR S02nrr by 1 to sav_S02nrr;
056800130329       //?Aggiorno i dati in base a quanto inserito nel subfile
056900130709           chain S02nrr BS4102S;
057000130329           IF  %found;
057100130329
057200130709           //?Scrivo
057300130709             IF  V02opz = '1';
057400130709               exsr NEWutenti;
057500130709             ENDIF;
057600130329
057700130329             V02csr = S02nrr;
057800130329             V02rcd = S02nrr;
057900130709             UPDATE BS4102S;
058000130329           ENDIF;
058100130329         ENDFOR;
058200130710
058300130710       //?Dopo aver scritto tutti gli utenti richiesti
058400130710       //?vado al pgm di Gestione Profili
058500130710         %subst(kpjbu:1:3) = %editc(V01fil:'X');
058600130710         tibs30r (kpjba);
058700130327
058800130327       ENDSR;
058900130327
059000130327       //--------------------------------------------------------------
059100130327       //?Gestione tasto funzionale F12.
059200130327       //?F12=Ritorno.
059300130327       //--------------------------------------------------------------
059400130327       BEGSR F12S02;
059500130327
059600130328         wVideo = 'D1';
059700130328         wInzD01 = *on;
059800130327
059900130327       ENDSR;
060000130327
060100130327       //--------------------------------------------------------------
060200130327       //?Controllo videata S02.
060300130327       //--------------------------------------------------------------
060400130327       BEGSR CtrS02;
060500130327
060600130328         WindDspF = IndDspF;
060700130328         reset WindDspFb;
060800130328         IndDspF  = WindDspF;
060900130328
061000130328       //?Controllo cosa è stato inserito nel subfile
061100130709         readc BS4102S;
061200130328
061300130709         DOW  not %eof(TIBS41D);
061400130709           %subst(IndDspF : 51) = *off;
061500130709           SflNxtChg = *off;
061600130328           V02rcd = S02nrr;
061700130328
061800130328       //?Controllo se opzione valida
061900130328           SELECT;
062000130328             //?Nessuna opzione
062100130329             WHEN  V02opz  = *blank;
062200130709             //?Opzione '1' di scelta
062300130709             WHEN  V02opz = '1';
062400130710               IF V02nrute = 0;
062500130710                 ErrMessage  = *on;
062600130710                 ErrGenerico = *on;
062700130710                 PosCurNRUTE = *on;
062800130710                 V02msg = skMSG(03);
062900130710               ENDIF;
063000130709             //?Altra opzione errore
063100130709           OTHER;
063200130328               ErrMessage  = *on;
063300130328               ErrGenerico = *on;
063400130709               PosCurOPZ   = *on;
063500130709               V02msg = skMSG(02);
063600130328           ENDSL;
063700130328
063800130328       //?Aggiorno il subfile
063900130328           SELECT;
064000130328             WHEN  ErrMessage;
064100130709               SflNxtChg = *on;
064200130709               V02csr    = V02rcd;
064300130328             WHEN  ErrGenerico;
064400130328               V02csr = V02rcd;
064500130329               clear  V02opz;
064600130328             OTHER;
064700130328               V02csr = V02rcd;
064800130328           ENDSL;
064900130328
065000130709           UPDATE BS4102S;
065100130328
065200130328       //?Al primo errore esco dalla lettura del subfile
065300130328           IF  ErrMessage or ErrGenerico;
065400130328             leave;
065500130328           ENDIF;
065600130328
065700130709           readc BS4102S;
065800130328
065900130328         ENDDO;
066000130327
066100130327       ENDSR;
066200130709
066300130709       //--------------------------------------------------------------
066400130709       //?Scrivo i nuovi utenti.
066500130709       //--------------------------------------------------------------
066600130709       BEGSR NEWutenti;
066700130709
066800130709         xx = 1;
066900130709         FOR xx by 1 to V02nrute;
067000130709         //?Cerco il primo nr. disponibile per Profilo/Filiale
067100130709           exsr fndNEWprg;
067200130710         //?Aggancio il profilo STD
067300130710           chain v02std AZUTE01L;
067400130710           IF  not %found(AZUTE01L);
067500130710             iter;
067600130710           ENDIF;
067700130710           UTEute = wUte;
067800130710           §UT2mopr = 'R';
067900130710           UTEfil = V01fil;
068000130710           UTEdsc = 20391231;
068100130710           exsr NewUTEcod;
068200130710           UTEcdu = wCdu;
068300130710           UTEfa1 = dUTE02;
068400130710         //?Scrivo il nuovo profilo
068500130710           write AZUTE000;
068600130710         //?Modulo base
068700130710           chain v02std KPPRF01L;
068800130710           PRFnmu = UTEute;
068900130710           write KPPRF000;
069000130710         //?Modulo base AS400
069100130710           w32ute = UTEute;
069200130710           w32text =  PRFtxt;
069300130710           w32psw = UTEute;
069400130710           tibs32c (w32tela:w32moba:w32ute:w32text:w32curlib:
069500130710                    w32atnpgm:w32lib:w32pswexp:w32psw:
069600130710                    w32status:w32err);
069700130709         ENDFOR;
069800130709
069900130709       ENDSR;
070000130709
070100130709       //--------------------------------------------------------------
070200130709       //?Cerco il primo numero progressivo libero.
070300130709       //--------------------------------------------------------------
070400130709       BEGSR fndNEWprg;
070500130709
070600140523         SELECT;
070700130709       //?Se utene STD COCOM l'utente va creato COM
070800140523         WHEN  V02std = 'COCOM';
070900130710           wUte = 'COM' + %editc(V01fil:'X');
071000140710       //?Se utene STD RDRED l'utente va creato RED
071100140710         WHEN  V02std = 'RDRED';
071200140710           wUte = 'RED' + %editc(V01fil:'X');
071300140710       //?Se utene STD RAREA l'utente va creato REA
071400140710         WHEN  V02std = 'RAREA';
071500140710           wUte = 'REA' + %editc(V01fil:'X');
071600140523       //?Se utene STD XXXM l'utente va creato XXX
071700140523           WHEN %subst(V02std:4:1) = 'M';
071800140523           wUte = %subst(V02std:1:3) + %editc(V01fil:'X');
071900140523         OTHER;
072000130710           wUte = %subst(V02std:1:3) +
072100130710                  %editc(V01fil:'X');
072200140523         ENDSL;
072300130709
072400130709         wProgr = 1;
072500130709
072600130709         setll wUte AZUTE01L;
072700130709         read  AZUTE01L;
072800130709
072900130709         DOW  not %eof(AZUTE01L);
073000130801           IF  %subst(UTEute:7:2) <> *blanks;
073100130801             IF   %subst(UTEute:1:6) <> %subst(wUte:1:6);
073200130801               leave;
073300130801             ENDIF;
073400130801               IF  wProgr <> %int(%subst(UTEute:7:2));
073500130801                 leave;
073600130801               ENDIF;
073700130801               IF  wProgr = *hival;
073800130801                   clear  wProgr;
073900130801                 leave;
074000130801               ENDIF;
074100130709
074200130801             wProgr += 1;
074300130801           ENDIF;
074400130801
074500130709           read AZUTE01L;
074600130709         ENDDO;
074700130709
074800130709         wUte = %subst(wUte:1:6) + %editc(wProgr:'X');
074900130709
075000130709       ENDSR;
075100130709
075200130709       //--------------------------------------------------------------
075300130709       //?Cerco il codice utente.
075400130709       //--------------------------------------------------------------
075500130709       BEGSR newUTEcod;
075600130709
075700130710         wKcdu = 999;
075800130710         setgt (V01fil:wKcdu) AZUTE02L;
075900130709         readpe V01fil AZUTE02L;
076000130709       //?Se trovo sommo 1
076100130709         IF  %found(AZUTE02L);
076200130801           IF  XTEcdu = 999;
076300130801             wCdu = 0;
076400130801           ELSE;
076500130801             wCdu = XTEcdu + 1;
076600130801           ENDIF;
076700130709         ENDIF;
076800130709       //?Se non trovo metto 1
076900130709         IF  not %found(AZUTE02L);
077000130709           wCdu = 1;
077100130709         ENDIF;
077200130710
077300130710       //?Se codice = 0 cerco il primo numero vuoto
077400130710         IF  wCdu = 0;
077500130710           wKcdu = 1;
077600130710           chain (V01fil:wKcdu) AZUTE02L;
077700130710           DOW  %found(AZUTE02L);
077800130710             wKcdu += 1;
077900130710             chain (V01fil:wKcdu) AZUTE02L;
078000130710           ENDDO;
078100130710           wCdu = wKcdu;
078200130710         ENDIF;
078300130709
078400130709       ENDSR;
078500130329
078600130327       //--------------------------------------------------------------
078700130327       //?Operazioni finali.
078800130327       //--------------------------------------------------------------
078900130327       BEGSR RoutEnd;
079000130327
079100130327         *inLR = *on;
079200130327         return;
079300130327
079400130327       ENDSR;
079500130327
079600130327      /end-free
079700130326
079800130326       //--------------------------------------------------------------
079900130326       //?Schiere a tempo di compilazione.
080000130326       //--------------------------------------------------------------
080100130326
080200130326** - skMSG ------------------------------------------------------------------*
080300130709Filiale errata                                                                 01
080400130709Opzione errata                                                                 02
080500130710Immetter il Nr. dei profili da creare                                          03
