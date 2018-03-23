000100130327       //==============================================================
000200130327       //?FIDNK0R - Gestione Clienti con Chiusura Forzata dopo fase 100
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
001400130326
001500130327       // -?File Legami
001600130329     fFNDKL00F  if a e             disk
001700130326
001800130326       // -?File video
001900130327     fFIDNK0D   cf   e             workstn
002000130327     f                                     sfile(DNK002S:S02nrr)
002100130328     f                                     sfile(DNK003S:S03nrr)
002200130328     f                                     sfile(DNK004S:S04nrr)
002300130326     f                                     indds(IndDspF)
002400130326     f                                     infds(InfDspF)
002500130326
002600130326       //--------------------------------------------------------------
002700130326       //?Definizione costanti.                                        ?
002800130326       //--------------------------------------------------------------
002900130326
003000130326       // -?Tasti funzionali a video
003100130326     d c_F01           c                   const(x'31')
003200130326     d c_F02           c                   const(x'32')
003300130326     d c_F03           c                   const(x'33')
003400130326     d c_F04           c                   const(x'34')
003500130326     d c_F05           c                   const(x'35')
003600130326     d c_F06           c                   const(x'36')
003700130326     d c_F07           c                   const(x'37')
003800130326     d c_F08           c                   const(x'38')
003900130326     d c_F09           c                   const(x'39')
004000130326     d c_F10           c                   const(x'3A')
004100130326     d c_F11           c                   const(x'3B')
004200130326     d c_F12           c                   const(x'3C')
004300130326     d c_F13           c                   const(x'B1')
004400130326     d c_F14           c                   const(x'B2')
004500130326     d c_F15           c                   const(x'B3')
004600130326     d c_F16           c                   const(x'B4')
004700130326     d c_F17           c                   const(x'B5')
004800130326     d c_F18           c                   const(x'B6')
004900130326     d c_F19           c                   const(x'B7')
005000130326     d c_F20           c                   const(x'B8')
005100130326     d c_F21           c                   const(x'B9')
005200130326     d c_F22           c                   const(x'BA')
005300130326     d c_F23           c                   const(x'BB')
005400130326     d c_F24           c                   const(x'BC')
005500130326     d c_Enter         c                   const(x'F1')
005600130326     d c_RollDown      c                   const(x'F4')
005700130326     d c_RollUp        c                   const(x'F5')
005800130326
005900130326     d Digits          c                   const('0123456789')
006000130326
006100130326       //--------------------------------------------------------------
006200130326       //?Definizione schiere.                                         ?
006300130326       //--------------------------------------------------------------
006400130328
006500130328       // -?Schiere per controllo codici già presenti sul file
006600130329     d skCOD           s              7  0 dim(999)
006700130329     d skKSC           s              7  0 dim(999)
006800130329     d skKUN           s              7  0 dim(999)
006900130327
007000130327       // -?Schiere per gestione codici cliente di ritorno da XCLIR
007100130327     d ksa             s              4    dim(30)
007200130327     d ksc             s              7  0 dim(30)
007300130326
007400130326      // -?Messaggi di errore
007500130328     d skMSG           s             78    dim(50) ctdata perrcd(1)
007600130326
007700130326       //--------------------------------------------------------------
007800130326       //?Definizione aree dati.                                       ?
007900130326       //--------------------------------------------------------------
008000130326
008100130326       // -?Dati utente?
008200130326     d §AzUte        e ds                  extname(AZUTE00F)
008300130326     d                                     dtaara
008400130326     d §DatiUte      e ds                  extname(dDatiUte)
008500130326     d                                     dtaara
008600130326
008700130326       //--------------------------------------------------------------
008800130326       //?Definizione strutture dati.                                  ?
008900130326       //--------------------------------------------------------------
009000130326
009100130326       // -?Status ds?
009200130326     d Status         sds
009300130326     d  SDSpgm           *proc
009400130326     d  SDSusr               254    263
009500130326
009600130326       // -?InfDS
009700130326     d InfDspF         ds
009800130326     d  dsp_aid              369    369a
009900130327     d  sfl_rrn              376    377i 0
010000130327     d  min_nrr              378    379i 0
010100130327     d  num_rcds             380    381i 0
010200130326
010300130326       // -?Indicatori su DspF
010400130326     d IndDspF         ds
010500130327       // -?Indicatori di visualizzazione Errore
010600130326     d  ErrMessage                    1n   overlay(IndDspF : 28)
010700130328       // -?Indicatori di gestione del subfile
010800130328     d  SflDsp_02                     1n   overlay(IndDspF : 30)
010900130328     d  SflDspCtl_02                  1n   overlay(IndDspF : 31)
011000130328     d  SflNxtChg_02                  1n   overlay(IndDspF : 32)
011100130328     d  SflEnd_02                     1n   overlay(IndDspF : 33)
011200130328     d  SflDsp_03                     1n   overlay(IndDspF : 34)
011300130328     d  SflDspCtl_03                  1n   overlay(IndDspF : 35)
011400130328     d  SflNxtChg_03                  1n   overlay(IndDspF : 36)
011500130328     d  SflEnd_03                     1n   overlay(IndDspF : 37)
011600130328     d  SflDsp_04                     1n   overlay(IndDspF : 38)
011700130328     d  SflDspCtl_04                  1n   overlay(IndDspF : 39)
011800130328     d  SflNxtChg_04                  1n   overlay(IndDspF : 40)
011900130328     d  SflEnd_04                     1n   overlay(IndDspF : 41)
012000130329       // -?Indicatori di Protezioni campi
012100130329     d  ProtS04                       1n   overlay(IndDspF : 49)
012200130327       // -?Indicatori di Errore
012300130329     d  PosCurCOD                     1n   overlay(IndDspF : 50)
012400130328     d  PosCurOPZ2                    1n   overlay(IndDspF : 51)
012500130328     d  PosCurOPZ3                    1n   overlay(IndDspF : 52)
012600130329     d  PosCurKSC                     1n   overlay(IndDspF : 53)
012700130326       // -?Indicatori di errore generico
012800130326     d  ErrGenerico                   1n   overlay(IndDspF : 99)
012900130326
013000130326       // -?DS indicatori Dspf
013100130326     d WindDspF        ds                  inz
013200130326     d   WindDspFa             1     49    inz(*zero)
013300130326     d   WindDspFb            50     99    inz(*zero)
013400130327
013500130327       // -?DS per gestione codici cliente di ritorno da XCLIR
013600130327     d                 ds
013700130327     d dsksn                   1      4p 0
013800130327     d dsksa                   1      4
013900130326
014000130326       // -?Parametri ricevuti
014100130326     d KPJBA         e ds
014200130327
014300130327       // -?Ricerca/Controllo tabelle
014400130327     d TIBS02DS      e ds                  inz
014500130326
014600130326       // -?Parametri per Reperimento dati utente?
014700130326     d TIBS34DS      e ds
014800130329
014900130329       // -?Parametri per Reperimento ultimo numero C.A. chiuse
015000130329     d TRUL33DS      e ds
015100130327
015200130327       // -?Ricerca clienti
015300130327     d xCLIrds       e ds
015400130326
015500130327       // -?Tabella DKL - Clienti abilitati alla chiusura automatica C.A.
015600130327     d dDKL          e ds
015700130326
015800130328       // -?File Clienti abilitati alla chiusura automatica C.A.
015900130328     d FNDKLDS       e ds                  extname(FNDKL00F)
016000130326
016100130326       //--------------------------------------------------------------
016200130326       //?Definizione variabili globali.                               ?
016300130326       //--------------------------------------------------------------
016400130326
016500130326       // -?Flags booleani
016600130329     d wCarica         s               n   inz(*off)
016700130328     d wEnd            s               n   inz(*off)
016800130326     d wErrGrave       s               n   inz(*off)
016900130326     d wFine           s               n   inz(*off)
017000130326     d wInzD01         s               n   inz(*on)
017100130327     d wInzS02         s               n   inz(*off)
017200130328     d wInzS03         s               n   inz(*off)
017300130328     d wInzS04         s               n   inz(*off)
017400130328     d wInzW05         s               n   inz(*off)
017500130328
017600130328       // -?Indici di schiera
017700130328     d xx              s              4  0 inz
017800130328     d yy              s              4  0 inz
017900130328     d zz              s              4  0 inz
018000130326
018100130326       // -?Campi di Comodo
018200130329     d sav_skKUN       s                   like(skKUN)
018300130328     d sav_S02nrr      s                   like(S02nrr)
018400130329     d sav_S03nrr      s                   like(S03nrr)
018500130329     d sav_S04nrr      s                   like(S04nrr)
018600130327     d S02nrr          s              4  0 inz
018700130328     d S03nrr          s              4  0 inz
018800130328     d S04nrr          s              4  0 inz
018900130327     d wPag            s              4  0 inz
019000130327     d wRig            s              3  0 inz
019100130328     d wVideo          s              2    inz('D1')
019200130326
019300130326       //--------------------------------------------------------------
019400130326       //?Definizione procedure esterne.                     ?
019500130326       //--------------------------------------------------------------
019600130326
019700130326
019800130326       //--------------------------------------------------------------
019900130326       //?Definizione prototipi.
020000130326       //--------------------------------------------------------------
020100130326
020200130328       // -?Reperimento dati tabelle
020300130326      /copy gaitrasrc/srcprotopr,tibs02r
020400130328
020500130328       // -?Reperimento dati utente?
020600130326      /copy gaitrasrc/srcprotopr,tibs34r
020700130328
020800130328       // -?Recupero codici clienti ed unificanti "TIBS10R"?
020900130328      /copy gaitrasrc/srcProtoPI,TIBS10R
021000130328      /copy gaitrasrc/srcProtoPR,TIBS10R
021100130328
021200130328       // -?Controllo/Decodifica cliente?
021300130327      /copy gaitrasrc/srcprotopi,tibs69r
021400130327      /copy gaitrasrc/srcprotopr,tibs69r
021500130329
021600130329       // -?Reperimento ultimo numero C.A. chiusa
021700130329      /copy gaitrasrc/srcprotopr,trul33r
021800130329
021900130329       // -?Ricerca cliente per ragione sociale
022000130329      /copy gaitrasrc/srcprotopr,xalfa3brds
022100130329      /copy gaitrasrc/srcprotopr,xalfa3br
022200130329
022300130329       // -?Ricerca cliente
022400130329      /copy gaitrasrc/srcprotopr,xclir
022500130326
022600130326       //--------------------------------------------------------------
022700130326       //?Definizione key-list.                                        ?
022800130326       //--------------------------------------------------------------
022900130326
023000130326       //--------------------------------------------------------------
023100130326       //?Indicatori.
023200130326       //--------------------------------------------------------------
023300130327       // IN   =
023400130326
023500130326       //--------------------------------------------------------------
023600130326       //?M A I N - L I N E                                            ?
023700130326       //--------------------------------------------------------------
023800130326
023900130326     c     *Entry        plist
024000130326     c                   parm                    KPJBA
024100130326
024200130326      /free
024300130326
024400130328       //?Operazioni iniziali?
024500130326       exsr RoutInz;
024600130326
024700130328       //?Gestione vidate?
024800130326       DOW not wFine;
024900130326         SELECT;
025000130326           WHEN  wVideo = 'D1';
025100130326             exsr GesD01;
025200130327           WHEN  wVideo = 'S2';
025300130327             exsr GesS02;
025400130328           WHEN  wVideo = 'S3';
025500130328             exsr GesS03;
025600130328           WHEN  wVideo = 'S4';
025700130328             exsr GesS04;
025800130329           WHEN  wVideo = 'W5';
025900130329             exsr GesW05;
026000130326           OTHER;
026100130326             wFine = *on;
026200130326         ENDSL;
026300130326       ENDDO;
026400130326
026500130328       //?Operazioni finali?
026600130326       exsr RoutEnd;
026700130326
026800130326       //--------------------------------------------------------------
026900130326       //?Operazioni iniziali.                                         ?
027000130326       //--------------------------------------------------------------
027100130326       BEGSR  RoutInz;
027200130326
027300130326       //?Reperimento dati job?
027400130326         exsr  DatiJob;
027500130328
027600130328         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
027700130327
027800130327       //?Imposto nome pgm a video
027900130327         V00pgm = SDSpgm;
028000130328
028100130328       //?Carico i dati del file in schiera
028200130328         exsr CarSK;
028300130326
028400130326       ENDSR;
028500130326
028600130326       //--------------------------------------------------------------
028700130326       //?Reperimento Dati del job (Utente/Operativi).                 ?
028800130326       //--------------------------------------------------------------
028900130326       BEGSR  DatiJob;
029000130326
029100130326         in(E) §AzUte;
029200130326         IF  NOT %error;
029300130326           in(E) §DatiUte;
029400130326         ENDIF;
029500130326         IF  %error or RSut = *blanks;
029600130326           clear TIBS34DS;
029700130326           tibs34r ( TIBS34DS );
029800130326           in §AzUte;
029900130326           in §DatiUte;
030000130326         ENDIF;
030100130326
030200130326       ENDSR;
030300130328
030400130328       //--------------------------------------------------------------
030500130328       //?Carico le schiere.
030600130328       //--------------------------------------------------------------
030700130328       BEGSR  CarSK;
030800130328
030900130329         wEnd = *off;
031000130328         clear xx;
031100130328         clear skCOD;
031200130328         clear skKUN;
031300130328         clear skKSC;
031400130328
031500130328       //?Carico i codici in schiera
031600130328         exec sql
031700130328         DECLARE  FNDKL cursor for
031800130328         SELECT * from FNDKL00F
031900130328         ORDER BY DKLcod, DKLkun, DKLksc;
032000130328
032100130328       //?Apertura del cursore
032200130328         exec sql
032300130328         OPEN FNDKL;
032400130328
032500130328         IF  sqlcode < 0;
032600130328           wEnd = *on;
032700130328         ENDIF;
032800130328
032900130328         DOW  not wEnd;
033000130328           exec sql
033100130328           FETCH next from FNDKL into :fndklds;
033200130328           IF  sqlcod = 100 or sqlcod < 0;
033300130328             wEnd = *on;
033400130328             leave;
033500130328           ENDIF;
033600130328
033700130328       //?Non posso avere 2 COD uguali
033800130328       //?Non posso avere KUN uguale su + COD diversi
033900130328       //?Non posso avere KSC uguale su + KUN diversi
034000130328           xx += 1;
034100130328           skKUN(xx) = DKLkun;
034200130328           skCOD(xx) = DKLcod;
034300130328           skKSC(xx) = DKLksc;
034400130328
034500130328       //?Messaggio di avvertimento se supero il n. di elementi previsti
034600130328          IF  xx >= %elem(skKUN);
034700130328           wErrGrave = *on;
034800130328           leave;
034900130328          ENDIF;
035000130328
035100130328         ENDDO;
035200130328
035300130328       //?Chiusura del cursore
035400130328         exec sql
035500130328         CLOSE FNDKL;
035600130328
035700130328       ENDSR;
035800130328
035900130326       //--------------------------------------------------------------
036000130327       //?Gestione videata D01.
036100130326       //--------------------------------------------------------------
036200130326       BEGSR  GesD01;
036300130326
036400130326       //?Inizializzazione videata
036500130326         IF  wInzD01;
036600130326           exsr Inzd01;
036700130326           wInzD01 = *off;
036800130326         ENDIF;
036900130326
037000130328       //?Per errore grave messaggio all'utente
037100130326         IF  wErrGrave;
037200130328           V01msg = skMSG(01);
037300130326           ErrMessage  = *on;
037400130326           Errgenerico = *on;
037500130326         ENDIF;
037600130326
037700130326       //?Emissione videata
037800130327         write DNK000T;
037900130327         exfmt DNK001D;
038000130326         ErrMessage  = *off;
038100130326         ErrGenerico = *off;
038200130326         clear V01msg;
038300130326
038400130326         SELECT;
038500130326
038600130326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
038700130326           WHEN  wErrGrave;
038800130326             wFine = *on;
038900130327
039000130327       //?F2=Verifica Unificanti
039100130327           WHEN  dsp_aid = c_F02;
039200130327             exsr F02D01;
039300130326
039400130326       //?F3=Fine
039500130326           WHEN  dsp_aid = c_F03;
039600130326             exsr F03D01;
039700130326
039800130326       //?Enter
039900130326           OTHER;
040000130326             exsr CtrD01;
040100130326             IF  ErrGenerico;
040200130326               leavesr;
040300130326             ENDIF;
040400130327         //?I controlli sono OK passo al subfile
040500130328             wVideo  = 'S3';
040600130328             wInzS03 = *on;
040700130326
040800130326         ENDSL;
040900130326
041000130326       ENDSR;
041100130326
041200130326       //--------------------------------------------------------------
041300130326       //?Inizializzazione videata.
041400130326       //--------------------------------------------------------------
041500130326       BEGSR InzD01;
041600130327
041700130327         clear DNK001D;
041800130326
041900130326       ENDSR;
042000130327
042100130327       //--------------------------------------------------------------
042200130327       //?Gestione tasto funzionale F2.
042300130327       //?F2=Verifica Unificanti
042400130327       //--------------------------------------------------------------
042500130327       BEGSR F02D01;
042600130327
042700130327       //?Gestione del subfile Verifica unificanti
042800130327         wVideo  = 'S2';
042900130327         wInzS02 = *on;
043000130327
043100130327       ENDSR;
043200130326
043300130326       //--------------------------------------------------------------
043400130326       //?Gestione tasto funzionale F3.
043500130326       //?F3=Fine
043600130326       //--------------------------------------------------------------
043700130326       BEGSR F03D01;
043800130326
043900130326       //?Chiusura del programma
044000130326         wFine = *on;
044100130326
044200130326       ENDSR;
044300130326
044400130326       //--------------------------------------------------------------
044500130326       //?Controllo videata.
044600130326       //--------------------------------------------------------------
044700130326       BEGSR CtrD01;
044800130326
044900130326         WindDspF = IndDspF;
045000130326         reset WindDspFb;
045100130326         IndDspF  = WindDspF;
045200130326
045300130327       //?Cliente di riferimento
045400130327       //?obbligatorio
045500130329         IF  V01cod = *blanks;
045600130326           ErrMessage  = *on;
045700130326           ErrGenerico = *on;
045800130329           PosCurCOD   = *on;
045900130328           V01msg      = skMSG(02);
046000130326           leavesr;
046100130326         ENDIF;
046200130327
046300130327       //?ricerca
046400130329         IF  %scan('?' : V01cod) > 0;
046500130327           clear dDKL;
046600130327           ErrGenerico = *on;
046700130329           PosCurCOD   = *on;
046800130327           clear TIBS02ds;
046900130327           T02mod = 'R';
047000130327           T02cod = 'DKL';
047100130327           T02sif = KNSIF;
047200130327           TNTBE_RicercaControllo  (kpjba : tibs02ds);
047300130327           IF  T02err = *blanks;
047400130329             V01cod  = T02ke1;
047500130327             dDKL    = T02uni;
047600130329             V01codd = §DKLrag;
047700130327             leavesr;
047800130329           ELSE;
047900130329             clear V01cod;
048000130329             V01msg = T02msg;
048100130329             ErrMessage = *on;
048200130329             leavesr;
048300130327           ENDIF;
048400130327         ENDIF;
048500130327
048600130327       //?solo caratteri numerici
048700130329         IF  %check(digits:V01cod) > 0;
048800130327           ErrMessage  = *on;
048900130327           ErrGenerico = *on;
049000130329           PosCurCOD   = *on;
049100130328           V01msg      = skMSG(02);
049200130327           leavesr;
049300130327         ENDIF;
049400130327
049500130327       //?codice valido in anagrafica
049600130327         clear  tibs69ds;
049700130329         I69kac = %int(V01cod);
049800130327         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
049900130327         IF  O69err <> *blanks;
050000130327           ErrMessage  = *on;
050100130327           ErrGenerico = *on;
050200130329           PosCurCOD   = *on;
050300130328           V01msg      = skMSG(02);
050400130327           leavesr;
050500130327         ENDIF;
050600130327
050700130327       //?codice abilitato in tabella DKL
050800130327         clear dDKL;
050900130327         clear TIBS02ds;
051000130327         T02mod = 'C';
051100130327         T02cod = 'DKL';
051200130327         T02sif = KNSIF;
051300130329         T02ke1 = V01cod;
051400130327         TNTBE_RicercaControllo  (kpjba : tibs02ds);
051500130327         IF  T02err <> *blanks;
051600130327           ErrMessage  = *on;
051700130327           ErrGenerico = *on;
051800130329           PosCurCOD   = *on;
051900130328           V01msg      = skMSG(02);
052000130327           leavesr;
052100130327         ENDIF;
052200130327         dDKL = T02uni;
052300130329         V01codd = §DKLrag;
052400130327
052500130326       ENDSR;
052600130327
052700130327       //--------------------------------------------------------------
052800130327       //?Gestione videata S02.
052900130327       //--------------------------------------------------------------
053000130327       BEGSR  GesS02;
053100130327
053200130327       //?Inizializzazione videata
053300130327         IF  wInzS02;
053400130327           exsr InzS02;
053500130327           wInzS02 = *off;
053600130327         ENDIF;
053700130328
053800130328       //?Per errore durante l'inizializzazione delle videta
053900130328       //?torno alla prima con errore impostato
054000130328         IF  ErrGenerico;
054100130328           wVideo  = 'D1';
054200130328           leavesr;
054300130328         ENDIF;
054400130327
054500130327       //?Emissione videata
054600130329         //write DNK000T;
054700130328         write DNK002Z;
054800130328         exfmt DNK002C;
054900130327         ErrMessage  = *off;
055000130327         ErrGenerico = *off;
055100130328         clear V02msg;
055200130327
055300130327         SELECT;
055400130328
055500130328       //?F3=Fine
055600130328           WHEN  dsp_aid = c_F03;
055700130328             exsr F03D01;
055800130327
055900130327       //?F6=Conferma
056000130327           WHEN  dsp_aid = c_F06;
056100130328           //?Eseguo i controlli
056200130328             exsr CtrS02 ;
056300130328             IF  ErrGenerico;
056400130328               leavesr;
056500130328             ENDIF;
056600130327             exsr F06S02;
056700130327
056800130327       //?F12=Ritorno
056900130327           WHEN  dsp_aid = c_F12;
057000130327             exsr F12S02;
057100130327
057200130327       //?Enter
057300130327           OTHER;
057400130328         //?Eseguo i controlli
057500130327             exsr CtrS02;
057600130327             IF  ErrGenerico;
057700130327               leavesr;
057800130327             ENDIF;
057900130327
058000130327         ENDSL;
058100130327
058200130327       ENDSR;
058300130327
058400130327       //--------------------------------------------------------------
058500130327       //?Inizializzazione videata S02.
058600130327       //--------------------------------------------------------------
058700130327       BEGSR InzS02;
058800130328
058900130328       //?Pulizia del Subfile
059000130328         exsr PulS02;
059100130327
059200130328       //?Leggo la schiera dei codici Padre
059300130328         xx = 1;
059400130328         clear sav_skKUN;
059500130328         FOR xx by 1 to %elem(skKUN);
059600130328         //?Per ogni KUN richiamo TIBS10R per cercare legami ST
059700130328           IF  skKUN(xx) <> *zeros and skKUN(xx) <> sav_skKUN;
059800130328             clear tibs10ds;
059900130328             TIBS10ds.D10drf = %dec(%date());
060000130328             TIBS10ds.D10tle = 'ST';
060100130328             TIBS10ds.D10paf = 'F';
060200130328             TIBS10ds.D10cod = skKUN(xx);
060300130328             tibs10r (TIBS10DS);
060400130328           //?Se errore vuol dire che non è un padre e non va bene
060500130328             IF  TIBS10ds.D10err <> *blanks;
060600130328               ErrMessage  = *on;
060700130328               ErrGenerico = *on;
060800130328               V01msg      = skMsg(03);
060900130328               %subst(V01msg:8:7) = %editc(skKUN(xx):'X');
061000130328               leavesr;
061100130328             ENDIF;
061200130328             sav_skKUN = skKUN(xx);
061300130328           ENDIF;
061400130328       //?Carico il subfile per ogni figlio Mancante (nuovo legame ST)
061500130328       //?                           oppure Eliminato (non più legame ST)
061600130328           exsr CarS02;
061700130328         ENDFOR;
061800130328
061900130328       //?Visualizzazione del Subfile
062000130328         SflDsp_02 = (S02nrr <= *zeros);
062100130328         sav_S02nrr = S02nrr;
062200130328
062300130328       //?Attivazione SFLEND
062400130328         SflEnd_02 = *on;
062500130328
062600130328       //?Impaginazione Subfile
062700130328       //?Forzo sempre il primo rcd
062800130328         IF  S02nrr > *zero;
062900130328           V02rcd = 1;
063000130328           V02csr = 1;
063100130328         ELSE;
063200130328           clear V02rcd;
063300130328         ENDIF;
063400130328
063500130328         V02csr = V02rcd;
063600130327
063700130327       ENDSR;
063800130328
063900130328       //--------------------------------------------------------------
064000130329       //?Pulizia Subfile S02.
064100130328       //--------------------------------------------------------------
064200130328       BEGSR PulS02;
064300130328
064400130328         SflDsp_02    = *on;
064500130328         SflDspCtl_02 = *on;
064600130328         write  DNK002C;
064700130328         SflDspCtl_02 = *off;
064800130328         SflEnd_02    = *off;
064900130328
065000130328         clear V02rcd;
065100130328         clear V02csr;
065200130328         clear S02nrr;
065300130328         clear V02msg;
065400130328         ErrMessage  = *off;
065500130328         ErrGenerico = *off;
065600130328
065700130328         WindDspF = IndDspF;
065800130328         reset WindDspFb;
065900130328         IndDspF  = WindDspF;
066000130328
066100130328       ENDSR;
066200130328
066300130328       //--------------------------------------------------------------
066400130328       //?Controllo i figli con i dati del file.
066500130328       //--------------------------------------------------------------
066600130328       BEGSR CarS02;
066700130328
066800130328       //?Figlio da TIBS10R non presente su FILE
066900130328         yy = 1;
067000130329         FOR yy by 1 to %elem(TIBS10DS.D10skcary);
067100130328         //?Non trovo sul file
067200130329           IF  %lookup (TIBS10DS.D10skcary(yy):skKSC) = 0;
067300130328             V02mod = 'MANCANTE';
067400130329             V02ksc = %dec(%subst(%editc(TIBS10DS.D10skcary(yy):'X'):5:7):7:0);
067500130328             V02kun = skKun(xx);
067600130328             clear V02kunnew;
067700130328             exsr RieS02;
067800130328             iter;
067900130328           ENDIF;
068000130328         //?Trovo sul file
068100130328           clear zz;
068200130329           zz =  %lookup (TIBS10DS.D10skcary(yy):skKSC);
068300130328         //?Ma padri diversi
068400130328           IF  zz > 0 and skKUN(zz) <> skKUN(xx);
068500130328             V02mod = 'UNIF.VAR.';
068600130328             V02kun = skKUN(zz);
068700130328             V02ksc = skKSC(zz);
068800130328             V02kunnew = skKUN(xx);
068900130328             exsr RieS02;
069000130328             iter;
069100130328           ENDIF;
069200130328         ENDFOR;
069300130328
069400130328       //?Figlio da FILE non presente su TIBS10R
069500130329         IF  %lookup (skKSC(xx):TIBS10DS.D10skcary) = 0;
069600130328           V02mod = 'ELIMINATO';
069700130328           V02ksc = skKSC(xx);
069800130328           V02kun = skKUN(xx);
069900130328           clear V02kunnew;
070000130328           exsr RieS02;
070100130328         ENDIF;
070200130328
070300130328       ENDSR;
070400130328
070500130328       //--------------------------------------------------------------
070600130329       //?Scrivo il rcd del subfile S02.
070700130328       //--------------------------------------------------------------
070800130328       BEGSR RieS02;
070900130328
071000130328         clear V02opz;
071100130328         V02cod = skCOD(xx);
071200130328         clear TIBS69ds;
071300130328         I69kac = V02cod;
071400130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
071500130328         V02codd = ACOrag;
071600130328         clear TIBS69ds;
071700130328         I69kac = V02kun;
071800130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
071900130328         V02kund = ACOrag;
072000130328         clear TIBS69ds;
072100130328         I69kac = V02ksc;
072200130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
072300130328         V02kscd = ACOrag;
072400130328         S02nrr += 1;
072500130328         write DNK002S;
072600130328
072700130328       ENDSR;
072800130327
072900130327       //--------------------------------------------------------------
073000130327       //?Gestione tasto funzionale F6.
073100130327       //?F6=Conferma.
073200130327       //--------------------------------------------------------------
073300130327       BEGSR F06S02;
073400130327
073500130328         S02nrr = 1;
073600130328         FOR S02nrr by 1 to sav_S02nrr;
073700130328       //?Aggiorno i dati in base a quanto inserito nel subfile
073800130328           chain S02nrr DNK002S;
073900130328           IF  %found;
074000130328         //?Aggiorno
074100130328             SELECT;
074200130328               //?Opzione "E"  ELIMINA
074300130329               WHEN  V02opz = 'E';
074400130328                 exec sql
074500130328                 DELETE from FNDKL00F
074600130328                 WHERE DKLcod = :V02cod and DKLkun = :V02kun and
074700130328                       DKLksc = :V02ksc;
074800130328               //?Opzione "I"  INSERISCI
074900130329               WHEN  V02opz = 'I';
075000130328                 clear FNDKL000;
075100130328                 DKLcod = V02cod;
075200130328                 DKLkun = V02kun;
075300130328                 DKLksc = V02ksc;
075400130328                 write  FNDKL000;
075500130328               //?Opzione "M"  UNIFICANTE VARIATO
075600130329               WHEN  V02opz = 'M';
075700130328                 exec sql
075800130328                 UPDATE from FNDKL00F set DKLkun = :V02kunnew
075900130328                 WHERE DKLcod = :V02cod and DKLkun = :V02kun and
076000130328                       DKLksc = :V02ksc;
076100130328             ENDSL;
076200130329
076300130329             V02csr = S02nrr;
076400130329             V02rcd = S02nrr;
076500130329             UPDATE DNK002S;
076600130329           ENDIF;
076700130328         ENDFOR;
076800130328
076900130328       //?Ricarico il tutto
077000130329         wVideo = 'S3';
077100130329         wInzS03 = *on;
077200130327
077300130327       ENDSR;
077400130327
077500130327       //--------------------------------------------------------------
077600130327       //?Gestione tasto funzionale F12.
077700130327       //?F12=Ritorno.
077800130327       //--------------------------------------------------------------
077900130327       BEGSR F12S02;
078000130327
078100130328         wVideo = 'D1';
078200130328         wInzD01 = *on;
078300130327
078400130327       ENDSR;
078500130327
078600130327       //--------------------------------------------------------------
078700130327       //?Controllo videata S02.
078800130327       //--------------------------------------------------------------
078900130327       BEGSR CtrS02;
079000130327
079100130328         WindDspF = IndDspF;
079200130328         reset WindDspFb;
079300130328         IndDspF  = WindDspF;
079400130328
079500130328       //?Controllo cosa è stato inserito nel subfile
079600130328         readc DNK002S;
079700130328
079800130328         DOW  not %eof(FIDNK0D);
079900130328           %subst(IndDspF : 51) = *off;
080000130328           SflNxtChg_02 = *off;
080100130328           V02rcd = S02nrr;
080200130328
080300130328       //?Controllo se opzione valida
080400130328           SELECT;
080500130328             //?Nessuna opzione
080600130329             WHEN  V02opz  = *blank;
080700130328
080800130328             //?Opzione "E" ma non è ELIMINATO
080900130329             WHEN  V02opz = 'E' and %subst(V02mod:1:1) <> 'E';
081000130328               ErrMessage  = *on;
081100130328               ErrGenerico = *on;
081200130329               PosCurOPZ2  = *on;
081300130328               V02msg = skMSG(04);
081400130328
081500130328             //?Opzione "I" ma non è MANCANTE
081600130329             WHEN  V02opz = 'I' and %subst(V02mod:1:1) <> 'M';
081700130328               ErrMessage  = *on;
081800130328               ErrGenerico = *on;
081900130329               PosCurOPZ2  = *on;
082000130328               V02msg = skMSG(04);
082100130328
082200130328             //?Opzione "M" ma non è UNIFICANTE VARIATO
082300130329             WHEN  V02opz = 'M' and %subst(V02mod:1:1) <> 'U';
082400130328               ErrMessage  = *on;
082500130328               ErrGenerico = *on;
082600130329               PosCurOPZ2  = *on;
082700130328               V02msg = skMSG(04);
082800130328
082900130328             //?Opzione non prevista
083000130329             WHEN  V02opz <> *blanks and V02opz <> 'E' and
083100130329                   V02opz <> 'I' and V02opz <> 'M';
083200130328               ErrMessage  = *on;
083300130328               ErrGenerico = *on;
083400130329               PosCurOPZ2  = *on;
083500130328               V02msg = skMSG(04);
083600130328           ENDSL;
083700130328
083800130328       //?Aggiorno il subfile
083900130328           SELECT;
084000130328             WHEN  ErrMessage;
084100130328               SflNxtChg_02 = *on;
084200130328               V02csr       = V02rcd;
084300130328             WHEN  ErrGenerico;
084400130328               V02csr = V02rcd;
084500130329               clear  V02opz;
084600130328             OTHER;
084700130328               V02csr = V02rcd;
084800130328               clear  V02opz;
084900130328           ENDSL;
085000130328
085100130328           UPDATE DNK002S;
085200130328
085300130328       //?Al primo errore esco dalla lettura del subfile
085400130328           IF  ErrMessage or ErrGenerico;
085500130328             leave;
085600130328           ENDIF;
085700130328
085800130328           readc DNK002S;
085900130328
086000130328         ENDDO;
086100130327
086200130327       ENDSR;
086300130328
086400130328       //--------------------------------------------------------------
086500130328       //?Gestione videata S03.
086600130328       //--------------------------------------------------------------
086700130328       BEGSR  GesS03;
086800130328
086900130328       //?Inizializzazione videata
087000130328         IF  wInzS03;
087100130328           exsr InzS03;
087200130328           wInzS03 = *off;
087300130328         ENDIF;
087400130328
087500130328       //?Emissione videata
087600130329         //write DNK000T;
087700130328         write DNK003Z;
087800130328         exfmt DNK003C;
087900130328         ErrMessage  = *off;
088000130328         ErrGenerico = *off;
088100130328         clear V03msg;
088200130328
088300130328         SELECT;
088400130328
088500130328       //?F6=Conferma
088600130328           WHEN  dsp_aid = c_F06;
088700130328           //?Eseguo i controlli
088800130328             exsr CtrS03 ;
088900130328             IF  ErrGenerico;
089000130328               leavesr;
089100130328             ENDIF;
089200130328             exsr F06S03;
089300130328
089400130328       //?F7=Inserimento per Unificante
089500130328           WHEN  dsp_aid = c_F07;
089600130328             exsr F07S03;
089700130328
089800130328       //?F10=Immissione
089900130328           WHEN  dsp_aid = c_F10;
090000130328             exsr F10S03;
090100130328
090200130328       //?F12=Ritorno
090300130328           WHEN  dsp_aid = c_F12;
090400130328             exsr F12S02;
090500130328
090600130328       //?Enter
090700130328           OTHER;
090800130328         //?Eseguo i controlli
090900130328             exsr CtrS03;
091000130328             IF  ErrGenerico;
091100130328               leavesr;
091200130328             ENDIF;
091300130328
091400130328         ENDSL;
091500130328
091600130328       ENDSR;
091700130328
091800130328       //--------------------------------------------------------------
091900130328       //?Inizializzazione videata S03.
092000130328       //--------------------------------------------------------------
092100130328       BEGSR InzS03;
092200130328
092300130328       //?Pulizia del Subfile
092400130328         exsr PulS03;
092500130329
092600130329       //?Carico i dati nella testata del subfile
092700130329         exsr CarC03;
092800130328
092900130328       //?Carico il subfile
093000130328         exsr CarS03;
093100130328
093200130328       //?Visualizzazione del Subfile
093300130328         SflDsp_03 = (S03nrr <= *zeros);
093400130328         sav_S03nrr = S03nrr;
093500130328
093600130328       //?Attivazione SFLEND
093700130328         SflEnd_03 = *on;
093800130328
093900130328       //?Impaginazione Subfile
094000130328       //?Forzo sempre il primo rcd
094100130328         IF  S03nrr > *zero;
094200130328           V03rcd = 1;
094300130328           V03csr = 1;
094400130328         ELSE;
094500130328           clear V03rcd;
094600130328         ENDIF;
094700130328
094800130328         V03csr = V03rcd;
094900130328
095000130328       ENDSR;
095100130328
095200130328       //--------------------------------------------------------------
095300130329       //?Pulizia Subfile S03.
095400130328       //--------------------------------------------------------------
095500130328       BEGSR PulS03;
095600130328
095700130328         SflDsp_03    = *on;
095800130328         SflDspCtl_03 = *on;
095900130328         write  DNK003C;
096000130328         SflDspCtl_03 = *off;
096100130328         SflEnd_03    = *off;
096200130328
096300130328         clear V03rcd;
096400130328         clear V03csr;
096500130328         clear S03nrr;
096600130328         clear V03msg;
096700130328         ErrMessage  = *off;
096800130328         ErrGenerico = *off;
096900130328
097000130328         WindDspF = IndDspF;
097100130328         reset WindDspFb;
097200130328         IndDspF  = WindDspF;
097300130328
097400130328       ENDSR;
097500130329
097600130329       //--------------------------------------------------------------
097700130329       //?Carico la testata del subfile S03.
097800130329       //--------------------------------------------------------------
097900130329       BEGSR CarC03;
098000130329
098100130329       //?Codice di Riferimento dalla prima videata
098200130329         V03cod  = %int(V01cod);
098300130329         V03codd = V01codd;
098400130329
098500130329       //?Nr. C.A. da chiudere - Numeratore
098600130329         V03nca = §DKLnca;
098700130329         V03cnu = §DKLcnu;
098800130329
098900130329       //?Ultimo numero prelevato dal Numeratore
099000130329         clear TRUL33DS;
099100130329         I33cnu = 120;
099200130329         I33ope = 1;
099300130329         kpjbu = TRUL33DS;
099400130329         trul33r (KPJBA);
099500130329         TRUL33DS = kpjbu;
099600130329         V03unp = I33unp;
099700130329
099800130329       ENDSR;
099900130328
100000130328       //--------------------------------------------------------------
100100130329       //?Carico il subfile S03.
100200130328       //--------------------------------------------------------------
100300130328       BEGSR CarS03;
100400130328
100500130329         wEnd = *off;
100600130329
100700130329       //?Carico tutti i codici legati al codice di riferimento
100800130329         exec sql
100900130329         DECLARE FNDKL1 cursor for
101000130329         SELECT * from FNDKL00F
101100130329         WHERE DKLcod = :V03cod
101200130329         ORDER BY DKLcod, DKLkun, DKLksc;
101300130329
101400130329       //?Apertura del cursore
101500130329         exec sql
101600130329         OPEN FNDKL1;
101700130329
101800130329         IF  sqlcode < 0;
101900130329           wEnd = *on;
102000130329         ENDIF;
102100130329
102200130329         DOW  not wEnd;
102300130329           exec sql
102400130329           FETCH next from FNDKL1 into :fndklds;
102500130329           IF  sqlcod = 100 or sqlcod < 0;
102600130329             wEnd = *on;
102700130329             leave;
102800130329           ENDIF;
102900130329
103000130329           exsr RieS03;
103100130329
103200130329         ENDDO;
103300130329
103400130329       //?Chiusura del cursore
103500130329         exec sql
103600130329         CLOSE FNDKL1;
103700130328
103800130328       ENDSR;
103900130328
104000130328       //--------------------------------------------------------------
104100130329       //?Scrivo il rcd del subfile S03.
104200130328       //--------------------------------------------------------------
104300130328       BEGSR RieS03;
104400130328
104500130329         clear DNK003S;
104600130329
104700130329         V03ksc = DKLksc;
104800130328         clear TIBS69ds;
104900130329         I69kac = V03ksc;
105000130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
105100130329         V03kscd = ACOrag;
105200130329         V03kun = DKLkun;
105300130329         clear TIBS69ds;
105400130329         I69kac = V03kun;
105500130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
105600130329         V03kund = ACOrag;
105700130329
105800130328         S03nrr += 1;
105900130328         write DNK003S;
106000130328
106100130328       ENDSR;
106200130328
106300130328       //--------------------------------------------------------------
106400130328       //?Gestione tasto funzionale F6.
106500130328       //?F6=Conferma.
106600130328       //--------------------------------------------------------------
106700130328       BEGSR F06S03;
106800130328
106900130328         S03nrr = 1;
107000130328         FOR S03nrr by 1 to sav_S03nrr;
107100130328       //?Aggiorno i dati in base a quanto inserito nel subfile
107200130328           chain S03nrr DNK003S;
107300130328           IF  %found;
107400130329         //?Elimino
107500130329             IF  V03opz = 'E';
107600130329               exec sql
107700130329               DELETE from FNDKL00F
107800130329               WHERE DKLcod = :V03cod and DKLkun = :V03kun and
107900130329                     DKLksc = :V03ksc;
108000130329             ENDIF;
108100130329
108200130329             V03csr = S03nrr;
108300130329             V03rcd = S03nrr;
108400130329             UPDATE DNK003S;
108500130329           ENDIF;
108600130329
108700130328         ENDFOR;
108800130328
108900130328       //?Vado in prima videata
109000130328         wVideo = 'D1';
109100130328         wInzD01 = *on;
109200130328
109300130328       ENDSR;
109400130328
109500130328       //--------------------------------------------------------------
109600130328       //?Gestione tasto funzionale F7.
109700130328       //?F7=Inserimento per Unificante.
109800130328       //--------------------------------------------------------------
109900130328       BEGSR F07S03;
110000130328
110100130329       //?Ricerca cliente
110200130329         clear XCLIRDS;
110300130329         DXCcap = DUTkci;
110400130329         DXCaut = 'AZ';
110500130329         xclir (XCLIRDS);
110600130329         IF  DXCf03 <> *blanks or DXCerr <> *blanks;
110700130329           leavesr;
110800130329         ENDIF;
110900130329
111000130329         ksa = DXCksc;
111100130329         dsksa = ksa(1);
111200130329         ksc(1) = dsksn;
111300130329
111400130328         wVideo = 'W5';
111500130328         wInzW05 = *on;
111600130328
111700130328       ENDSR;
111800130328
111900130328       //--------------------------------------------------------------
112000130328       //?Gestione tasto funzionale F10.
112100130328       //?F10=Immissione.
112200130328       //--------------------------------------------------------------
112300130328       BEGSR F10S03;
112400130328
112500130329         wCarica = *off;
112600130328         wVideo = 'S4';
112700130328         wInzS04 = *on;
112800130328
112900130328       ENDSR;
113000130328
113100130328       //--------------------------------------------------------------
113200130328       //?Controllo videata S03.
113300130328       //--------------------------------------------------------------
113400130328       BEGSR CtrS03;
113500130328
113600130328         WindDspF = IndDspF;
113700130328         reset WindDspFb;
113800130328         IndDspF  = WindDspF;
113900130328
114000130329       //?Controllo cosa è stato inserito nel subfile
114100130329         S03nrr = 1;
114200130329         FOR S03nrr by 1 to sav_S03nrr;
114300130329           chain S03nrr DNK003S;
114400130329           IF  %found;
114500130328
114600130329             %subst(IndDspF : 52) = *off;
114700130329             SflNxtChg_03 = *off;
114800130329             V03rcd = S03nrr;
114900130328
115000130329         //?Controllo se opzione valida
115100130329             SELECT;
115200130329               //?Nessuna opzione
115300130329               WHEN  V03opz  = *blank;
115400130329               WHEN  V03opz  <> 'E';
115500130329                 ErrMessage  = *on;
115600130329                 ErrGenerico = *on;
115700130329                 PosCurOPZ3  = *on;
115800130329                 V03msg = skMSG(04);
115900130329             ENDSL;
116000130328
116100130329         //?Aggiorno il subfile
116200130329             SELECT;
116300130329               WHEN  ErrMessage;
116400130329                 SflNxtChg_03 = *on;
116500130329                 V03csr       = V03rcd;
116600130329               WHEN  ErrGenerico;
116700130329                 V03csr = V03rcd;
116800130329                 clear  V03opz;
116900130329               OTHER;
117000130329                 V03csr = V03rcd;
117100130329                 clear  V03opz;
117200130329             ENDSL;
117300130328
117400130329             UPDATE DNK003S;
117500130329           ENDIF;
117600130328
117700130329       //?Al primo errore esco dalla lettura del subfile
117800130329           IF  ErrMessage or ErrGenerico;
117900130329             leave;
118000130329           ENDIF;
118100130328
118200130329           readc DNK003S;
118300130328
118400130329         ENDFOR;
118500130328
118600130328       ENDSR;
118700130329
118800130329       //--------------------------------------------------------------
118900130329       //?Gestione videata S04.
119000130329       //--------------------------------------------------------------
119100130329       BEGSR  GesS04;
119200130329
119300130329       //?Inizializzazione videata
119400130329         IF  wInzS04;
119500130329           exsr InzS04;
119600130329           wInzS04 = *off;
119700130329         ENDIF;
119800130329
119900130329       //?Emissione videata
120000130329         //write DNK000T;
120100130329         write DNK004Z;
120200130329         exfmt DNK004C;
120300130329         ErrMessage  = *off;
120400130329         ErrGenerico = *off;
120500130329         clear V04msg;
120600130329
120700130329         SELECT;
120800130329
120900130329       //?F6=Conferma
121000130329           WHEN  dsp_aid = c_F06;
121100130329           //?Eseguo i controlli
121200130329             exsr CtrS04 ;
121300130329             IF  ErrGenerico;
121400130329               leavesr;
121500130329             ENDIF;
121600130329             exsr F06S04;
121700130329
121800130329       //?F12=Ritorno
121900130329           WHEN  dsp_aid = c_F12;
122000130329             exsr F12S04;
122100130329
122200130329       //?Enter
122300130329           OTHER;
122400130329         //?Eseguo i controlli
122500130329             exsr CtrS04;
122600130329             IF  ErrGenerico;
122700130329               leavesr;
122800130329             ENDIF;
122900130329
123000130329         ENDSL;
123100130329
123200130329       ENDSR;
123300130329
123400130329       //--------------------------------------------------------------
123500130329       //?Inizializzazione videata S04.
123600130329       //--------------------------------------------------------------
123700130329       BEGSR InzS04;
123800130329
123900130329       //?Pulizia del Subfile
124000130329         exsr PulS04;
124100130329
124200130329       //?Carico i dati nella testata del subfile
124300130329         exsr CarC04;
124400130329
124500130329       //?Carico il subfile
124600130329         exsr CarS04;
124700130329
124800130329       //?Visualizzazione del Subfile
124900130329         SflDsp_04 = (S04nrr <= *zeros);
125000130329         sav_S04nrr = S04nrr;
125100130329
125200130329       //?Attivazione SFLEND
125300130329         SflEnd_04 = *on;
125400130329
125500130329       //?Impaginazione Subfile
125600130329       //?Forzo sempre il primo rcd
125700130329         IF  S04nrr > *zero;
125800130329           V04rcd = 1;
125900130329           V04csr = 1;
126000130329         ELSE;
126100130329           clear V04rcd;
126200130329         ENDIF;
126300130329
126400130329         V04csr = V04rcd;
126500130329
126600130329       ENDSR;
126700130329
126800130329       //--------------------------------------------------------------
126900130329       //?Pulizia Subfile S04.
127000130329       //--------------------------------------------------------------
127100130329       BEGSR PulS04;
127200130329
127300130329         SflDsp_04    = *on;
127400130329         SflDspCtl_04 = *on;
127500130329         write  DNK004C;
127600130329         SflDspCtl_04 = *off;
127700130329         SflEnd_04    = *off;
127800130329
127900130329         clear V04rcd;
128000130329         clear V04csr;
128100130329         clear S04nrr;
128200130329         clear V04msg;
128300130329         ErrMessage  = *off;
128400130329         ErrGenerico = *off;
128500130329
128600130329         WindDspF = IndDspF;
128700130329         reset WindDspFb;
128800130329         IndDspF  = WindDspF;
128900130329
129000130329       ENDSR;
129100130329
129200130329       //--------------------------------------------------------------
129300130329       //?Carico la testata del subfile S04.
129400130329       //--------------------------------------------------------------
129500130329       BEGSR CarC04;
129600130329
129700130329       //?Codice di Riferimento dalla prima videata
129800130329         V04cod  = %int(V01cod);
129900130329         V04codd = V01codd;
130000130329
130100130329       //?Nr. C.A. da chiudere - Numeratore
130200130329         V04nca = §DKLnca;
130300130329         V04cnu = §DKLcnu;
130400130329
130500130329       //?Ultimo numero prelevato dal Numeratore
130600130329         V04unp = V03unp;
130700130329
130800130329       ENDSR;
130900130329
131000130329       //--------------------------------------------------------------
131100130329       //?Carico il subfile S04.
131200130329       //--------------------------------------------------------------
131300130329       BEGSR CarS04;
131400130329
131500130329         xx = 1;
131600130329       //?Carico 99 righe vuote nel subfile
131700130329       //?se non provengo da caricamento totale
131800130329         IF  not wCarica;
131900130329           FOR xx by 1 to 99;
132000130329             exsr RieS04;
132100130329           ENDFOR;
132200130329       //?se carica totale leggo SK TIBS10
132300130329         ELSE;
132400130329           DOW TIBS10DS.D10skcary(xx) > *zeros;
132500130329             exsr RieS04;
132600130329             xx += 1;
132700130329           ENDDO;
132800130329         ENDIF;
132900130329
133000130329       ENDSR;
133100130329
133200130329       //--------------------------------------------------------------
133300130329       //?Scrivo il rcd del subfile S04.
133400130329       //--------------------------------------------------------------
133500130329       BEGSR RieS04;
133600130329
133700130329         clear DNK004S;
133800130329         ProtS04 = *off;
133900130329
134000130329         IF  wCarica;
134100130329           V04ksc = %subst(%editc(TIBS10DS.D10skcary(xx):'X'):5:7);
134200130329           I69kac = %int(V04ksc);
134300130329           V04kscd = ACOrag;
134400130329           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
134500130329           V04kun = TIBS10DS.D10cop;
134600130329           V04kund = ACOrag;
134700130329           ProtS04 = *on;
134800130329         ENDIF;
134900130329
135000130329         S04nrr += 1;
135100130329         write DNK004S;
135200130329
135300130329       ENDSR;
135400130329
135500130329       //--------------------------------------------------------------
135600130329       //?Gestione tasto funzionale F6.
135700130329       //?F6=Conferma.
135800130329       //--------------------------------------------------------------
135900130329       BEGSR F06S04;
136000130329
136100130329         readc  DNK004S;
136200130329
136300130329         DOW  not %eof(FIDNK0D);
136400130329           %subst(IndDspF : 53) = *off;
136500130329           SflNxtChg_04 = *off;
136600130329           V04rcd = S04nrr;
136700130329         S04nrr = 1;
136800130329         FOR S04nrr by 1 to sav_S04nrr;
136900130329       //?Aggiorno i dati in base a quanto inserito nel subfile
137000130329           chain S04nrr DNK004S;
137100130329           IF  %found;
137200130329         //?Scrivo
137300130329             clear FNDKL000;
137400130329             DKLcod = V04cod;
137500130329             DKLkun = V04kun;
137600130329             DKLksc = %int(V04ksc);
137700130329             write  FNDKL000;
137800130329
137900130329             V04csr = S04nrr;
138000130329             V04rcd = S04nrr;
138100130329             UPDATE DNK004S;
138200130329           ENDIF;
138300130329
138400130329         ENDFOR;
138500130329
138600130329       //?Vado al subfile con tutti i codici caricati
138700130329         wVideo = 'S3';
138800130329         wInzS03 = *on;
138900130329
139000130329       ENDSR;
139100130329
139200130329       //--------------------------------------------------------------
139300130329       //?Gestione tasto funzionale F12.
139400130329       //?F12=Ritorno.
139500130329       //--------------------------------------------------------------
139600130329       BEGSR F12S04;
139700130329
139800130329         wVideo = 'S3';
139900130329         wInzS03 = *on;
140000130329
140100130329       ENDSR;
140200130329
140300130329       //--------------------------------------------------------------
140400130329       //?Controllo videata S04.
140500130329       //--------------------------------------------------------------
140600130329       BEGSR CtrS04;
140700130329
140800130329         WindDspF = IndDspF;
140900130329         reset WindDspFb;
141000130329         IndDspF  = WindDspF;
141100130329
141200130329       //?Controllo cosa è stato inserito nel subfile
141300130329         readc  DNK004S;
141400130329
141500130329         DOW  not %eof(FIDNK0D);
141600130329           %subst(IndDspF : 53) = *off;
141700130329           SflNxtChg_04 = *off;
141800130329           V04rcd = S04nrr;
141900130329
142000130329       //?Controllo il cliente
142100130329           IF  V04ksc <> *blanks and V04ksc <> *zeros;
142200130329             exsr ctrKSC;
142300130329           ELSE;
142400130329             clear V04ksc;
142500130329             clear V04kscd;
142600130329             clear V04kun;
142700130329             clear V04kund;
142800130329           ENDIF;
142900130329
143000130329       //?Aggiorno il subfile
143100130329           SELECT;
143200130329             WHEN  ErrMessage;
143300130329               SflNxtChg_04 = *on;
143400130329               V04csr       = V04rcd;
143500130329             WHEN  ErrGenerico;
143600130329               V04csr = V04rcd;
143700130329             OTHER;
143800130329               V04csr = V04rcd;
143900130329           ENDSL;
144000130329
144100130329           UPDATE DNK004S;
144200130329
144300130329       //?Al primo errore esco dalla lettura del subfile
144400130329           IF  ErrMessage or ErrGenerico;
144500130329             leave;
144600130329           ENDIF;
144700130329
144800130329           readc DNK004S;
144900130329
145000130329         ENDDO;
145100130329
145200130329       ENDSR;
145300130329
145400130329       //--------------------------------------------------------------
145500130329       //?Controllo cliente.
145600130329       //--------------------------------------------------------------
145700130329       BEGSR CtrKSC;
145800130329
145900130329       //?ricerca
146000130329         IF  %scan('?' : V04ksc) > 0;
146100130329           xpardut = RSut;
146200130329           xparkut = 1;
146300130329           xparrag = V04kscd;
146400130329           xparkcc = DUTkci;
146500130329           xparsta = 9;          // esclusione annullati
146600130329           clear xparflr;
146700130329           xparnum = 1;
146800130329           clear xparkcm;
146900130329           clear xparksm;
147000130329           clear xparkdm;
147100130329           clear xparesci;
147200130329           clear xparerr;
147300130329           clear xpariva;
147400130329           clear xparcdf;
147500130329           XALFA3BR (xpardut : xparkut : xparrag :
147600130329                     xparkcc : xparsta : xparflr :
147700130329                     xpardit : xparnum : xparkcm :
147800130329                     xparksm : xparkdm : xparesci :
147900130329                     xparerr : xpariva : xparcdf);
148000130329           IF  xparsta <> -1;
148100130329             V04ksc  = %subst(xparksm:1:7);
148200130329             V04kscd = xparrag;
148300130329           ENDIF;
148400130329           ErrGenerico = *on;
148500130329           PosCurKSC   = *on;
148600130329         ENDIF;
148700130329
148800130329       //?Solo caratteri numerici
148900130329         IF  %check(digits:V04ksc) > 0;
149000130329           ErrMessage  = *on;
149100130329           ErrGenerico = *on;
149200130329           PosCurKSC   = *on;
149300130329           V04msg      = skMSG(02);
149400130329           leavesr;
149500130329         ENDIF;
149600130329
149700130329       //?Controllo che sia un codice valido
149800130329         clear  tibs69ds;
149900130329         I69kac = %int(V04ksc);
150000130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
150100130329         IF  O69err <> *blanks;
150200130329           ErrMessage  = *on;
150300130329           ErrGenerico = *on;
150400130329           PosCurKSC   = *on;
150500130329           V04msg      = skMSG(02);
150600130329           leavesr;
150700130329         ENDIF;
150800130329         V04kscd = ACOrag;
150900130329
151000130329       //?Se codice cliente inserito ricerco il padre
151100130329         clear tibs10ds;
151200130329         TIBS10ds.D10drf = %dec(%date());
151300130329         TIBS10ds.D10tle = 'ST';
151400130329         TIBS10ds.D10paf = 'F';
151500130329         TIBS10ds.D10cod = %int(V04ksc);
151600130329         tibs10r (TIBS10DS);
151700130329       //?Se non trovo come padre lo cerco come figlio
151800130329         IF  TIBS10ds.D10err <> *blanks;
151900130329           TIBS10ds.D10paf = 'P';
152000130329           tibs10r (TIBS10DS);
152100130329         //?Se non trovo imposto padre = figlio
152200130329           IF  TIBS10ds.D10err <> *blanks;
152300130329             TIBS10DS.D10cop = %int(V04ksc);
152400130329           ENDIF;
152500130329         ENDIF;
152600130329
152700130329         V04kun = TIBS10DS.D10cop;
152800130329       //?Decodifico il padre
152900130329         clear  tibs69ds;
153000130329         I69kac = %int(V04kun);
153100130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
153200130329         V04kund = ACOrag;
153300130329
153400130329       ENDSR;
153500130329
153600130329       //--------------------------------------------------------------
153700130329       //?Gestione videata W05.
153800130329       //--------------------------------------------------------------
153900130329       BEGSR  GesW05;
154000130329
154100130329       //?Inizializzazione videata
154200130329         IF  wInzW05;
154300130329           exsr InzW05;
154400130329           wInzW05 = *off;
154500130329         ENDIF;
154600130329
154700130329       //?Emissione videata
154800130329         exfmt DNK005W;
154900130329         ErrMessage  = *off;
155000130329         ErrGenerico = *off;
155100130329
155200130329         SELECT;
155300130329
155400130329       //?F12=Ritorno
155500130329           WHEN  dsp_aid = c_F12;
155600130329             exsr F12W05;
155700130329
155800130329       //?Enter
155900130329           OTHER;
156000130329         //?Se immesso SI devo caricari i codici figli + padre
156100130329             IF  W05conf = 'SI';
156200130329               exsr Carica;
156300130329               wVideo = 'S4';
156400130329               wInzS04 = *on;
156500130329               leavesr;
156600130329             ENDIF;
156700130329
156800130329         ENDSL;
156900130329
157000130329       ENDSR;
157100130329
157200130329       //--------------------------------------------------------------
157300130329       //?Inizializzazione videata W05.
157400130329       //--------------------------------------------------------------
157500130329       BEGSR InzW05;
157600130329
157700130329       //?Imposto il cliente selezionato dalla ricerca
157800130329         W05ksc = ksc(1);
157900130329
158000130329       //?Decodifico
158100130329         clear TIBS69ds;
158200130329         I69kac = W05ksc;
158300130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
158400130329         W05kscd = ACOrag;
158500130329
158600130329       //?SI di conferma
158700130329         clear  W05conf;
158800130329
158900130329       ENDSR;
159000130329
159100130329       //--------------------------------------------------------------
159200130329       //?Gestione tasto funzionale F12.
159300130329       //?F12=Ritorno.
159400130329       //--------------------------------------------------------------
159500130329       BEGSR F12W05;
159600130329
159700130329         wVideo = 'S3';
159800130329         wInzS03 = *on;
159900130329
160000130329       ENDSR;
160100130327
160200130329       //--------------------------------------------------------------
160300130329       //?Carico tutti i codici.
160400130329       //--------------------------------------------------------------
160500130329       BEGSR Carica;
160600130329
160700130329         wCarica = *on;
160800130329         clear tibs10ds;
160900130329         TIBS10ds.D10drf = %dec(%date());
161000130329         TIBS10ds.D10tle = 'ST';
161100130329         TIBS10ds.D10paf = 'F';
161200130329         TIBS10ds.D10cod = W05ksc;
161300130329         tibs10r (TIBS10DS);
161400130329       //?Se non trovo come padre lo cerco come figlio
161500130329         IF  TIBS10ds.D10err <> *blanks;
161600130329           TIBS10ds.D10paf = 'P';
161700130329           tibs10r (TIBS10DS);
161800130329         //?Se non trovo imposto padre = figlio
161900130329           IF  TIBS10ds.D10err <> *blanks;
162000130329             TIBS10DS.D10cop = W05ksc;
162100130329             TIBS10DS.D10skcary(1) = W05ksc;
162200130329           ENDIF;
162300130329         ENDIF;
162400130329
162500130329       ENDSR;
162600130329
162700130327       //--------------------------------------------------------------
162800130327       //?Operazioni finali.
162900130327       //--------------------------------------------------------------
163000130327       BEGSR RoutEnd;
163100130327
163200130327         *inLR = *on;
163300130327         return;
163400130327
163500130327       ENDSR;
163600130327
163700130327      /end-free
163800130326
163900130326       //--------------------------------------------------------------
164000130326       //?Schiere a tempo di compilazione.
164100130326       //--------------------------------------------------------------
164200130326
164300130326** - skMSG ------------------------------------------------------------------*
164400130328Superato nr. MAX di Clienti consentiti. AVVISARE ICT!                          01
164500130328Cliente errato o non abilitato alla chiusura forzata                           02
164600130328Codice xxxxxxx non trovato come UNIFICANTE nei legati "ST" . AVVISARE ICT!     03
164700130328Opzione errata                                                                 04
