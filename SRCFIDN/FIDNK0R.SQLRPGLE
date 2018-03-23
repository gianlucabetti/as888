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
018300130329     d sav_S02nrr      s                   like(S02nrr)
018400130329     d sav_S03nrr      s                   like(S03nrr)
018500130329     d sav_S04nrr      s                   like(S04nrr)
018600130327     d S02nrr          s              4  0 inz
018700130328     d S03nrr          s              4  0 inz
018800130328     d S04nrr          s              4  0 inz
018900130405     d wANNO           s              4  0 inz
019000130402     d wConta          s             10i 0 inz
019100130405     d wDataISO        s               d   datfmt(*ISO)
019200130402     d wKSC            s              7  0 inz
019300130327     d wPag            s              4  0 inz
019400130327     d wRig            s              3  0 inz
019500130328     d wVideo          s              2    inz('D1')
019600130326
019700130326       //--------------------------------------------------------------
019800130326       //?Definizione procedure esterne.                     ?
019900130326       //--------------------------------------------------------------
020000130326
020100130326
020200130326       //--------------------------------------------------------------
020300130326       //?Definizione prototipi.
020400130326       //--------------------------------------------------------------
020500130326
020600130328       // -?Reperimento dati tabelle
020700130326      /copy gaitrasrc/srcprotopr,tibs02r
020800130328
020900130328       // -?Reperimento dati utente?
021000130326      /copy gaitrasrc/srcprotopr,tibs34r
021100130328
021200130328       // -?Recupero codici clienti ed unificanti "TIBS10R"?
021300130328      /copy gaitrasrc/srcProtoPI,TIBS10R
021400130328      /copy gaitrasrc/srcProtoPR,TIBS10R
021500130328
021600130328       // -?Controllo/Decodifica cliente?
021700130327      /copy gaitrasrc/srcprotopi,tibs69r
021800130327      /copy gaitrasrc/srcprotopr,tibs69r
021900130329
022000130329       // -?Reperimento ultimo numero C.A. chiusa
022100130329      /copy gaitrasrc/srcprotopr,trul33r
022200130329
022300130329       // -?Ricerca cliente per ragione sociale
022400130329      /copy gaitrasrc/srcprotopr,xalfa3brds
022500130329      /copy gaitrasrc/srcprotopr,xalfa3br
022600130329
022700130329       // -?Ricerca cliente
022800130329      /copy gaitrasrc/srcprotopr,xclir
022900130326
023000130326       //--------------------------------------------------------------
023100130326       //?Definizione key-list.                                        ?
023200130326       //--------------------------------------------------------------
023300130326
023400130326       //--------------------------------------------------------------
023500130326       //?Indicatori.
023600130326       //--------------------------------------------------------------
023700130327       // IN   =
023800130326
023900130326       //--------------------------------------------------------------
024000130326       //?M A I N - L I N E                                            ?
024100130326       //--------------------------------------------------------------
024200130326
024300130326     c     *Entry        plist
024400130326     c                   parm                    KPJBA
024500130326
024600130326      /free
024700130326
024800130328       //?Operazioni iniziali?
024900130326       exsr RoutInz;
025000130326
025100130328       //?Gestione vidate?
025200130326       DOW not wFine;
025300130326         SELECT;
025400130326           WHEN  wVideo = 'D1';
025500130326             exsr GesD01;
025600130327           WHEN  wVideo = 'S2';
025700130327             exsr GesS02;
025800130328           WHEN  wVideo = 'S3';
025900130328             exsr GesS03;
026000130328           WHEN  wVideo = 'S4';
026100130328             exsr GesS04;
026200130329           WHEN  wVideo = 'W5';
026300130329             exsr GesW05;
026400130326           OTHER;
026500130326             wFine = *on;
026600130326         ENDSL;
026700130326       ENDDO;
026800130326
026900130328       //?Operazioni finali?
027000130326       exsr RoutEnd;
027100130326
027200130326       //--------------------------------------------------------------
027300130326       //?Operazioni iniziali.                                         ?
027400130326       //--------------------------------------------------------------
027500130326       BEGSR  RoutInz;
027600130326
027700130326       //?Reperimento dati job?
027800130326         exsr  DatiJob;
027900130328
028000130328         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
028100130327
028200130327       //?Imposto nome pgm a video
028300130327         V00pgm = SDSpgm;
028400130405
028500130405       //?Imposto anno corrente
028600130405         wDataISO = %date();
028700130405         wANNO    = %subdt(wDataISO:*years);
028800130405
028900130326       ENDSR;
029000130326
029100130326       //--------------------------------------------------------------
029200130326       //?Reperimento Dati del job (Utente/Operativi).                 ?
029300130326       //--------------------------------------------------------------
029400130326       BEGSR  DatiJob;
029500130326
029600130326         in(E) §AzUte;
029700130326         IF  NOT %error;
029800130326           in(E) §DatiUte;
029900130326         ENDIF;
030000130326         IF  %error or RSut = *blanks;
030100130326           clear TIBS34DS;
030200130326           tibs34r ( TIBS34DS );
030300130326           in §AzUte;
030400130326           in §DatiUte;
030500130326         ENDIF;
030600130326
030700130326       ENDSR;
030800130328
030900130328       //--------------------------------------------------------------
031000130328       //?Carico le schiere.
031100130328       //--------------------------------------------------------------
031200130328       BEGSR  CarSK;
031300130328
031400130329         wEnd = *off;
031500130328         clear xx;
031600130328         clear skCOD;
031700130328         clear skKUN;
031800130328         clear skKSC;
031900130328
032000130328       //?Carico i codici in schiera
032100130328         exec sql
032200130328         DECLARE  FNDKL cursor for
032300130328         SELECT * from FNDKL00F
032400130328         ORDER BY DKLcod, DKLkun, DKLksc;
032500130328
032600130328       //?Apertura del cursore
032700130328         exec sql
032800130328         OPEN FNDKL;
032900130328
033000130328         IF  sqlcode < 0;
033100130328           wEnd = *on;
033200130328         ENDIF;
033300130328
033400130328         DOW  not wEnd;
033500130328           exec sql
033600130328           FETCH next from FNDKL into :fndklds;
033700130328           IF  sqlcod = 100 or sqlcod < 0;
033800130328             wEnd = *on;
033900130328             leave;
034000130328           ENDIF;
034100130328
034200130328       //?Non posso avere 2 COD uguali
034300130328       //?Non posso avere KUN uguale su + COD diversi
034400130328       //?Non posso avere KSC uguale su + KUN diversi
034500130328           xx += 1;
034600130328           skKUN(xx) = DKLkun;
034700130328           skCOD(xx) = DKLcod;
034800130328           skKSC(xx) = DKLksc;
034900130328
035000130328       //?Messaggio di avvertimento se supero il n. di elementi previsti
035100130328          IF  xx >= %elem(skKUN);
035200130328           wErrGrave = *on;
035300130328           leave;
035400130328          ENDIF;
035500130328
035600130328         ENDDO;
035700130328
035800130328       //?Chiusura del cursore
035900130328         exec sql
036000130328         CLOSE FNDKL;
036100130328
036200130328       ENDSR;
036300130328
036400130326       //--------------------------------------------------------------
036500130327       //?Gestione videata D01.
036600130326       //--------------------------------------------------------------
036700130326       BEGSR  GesD01;
036800130326
036900130326       //?Inizializzazione videata
037000130326         IF  wInzD01;
037100130326           exsr Inzd01;
037200130326           wInzD01 = *off;
037300130326         ENDIF;
037400130326
037500130328       //?Per errore grave messaggio all'utente
037600130326         IF  wErrGrave;
037700130328           V01msg = skMSG(01);
037800130326           ErrMessage  = *on;
037900130326           Errgenerico = *on;
038000130326         ENDIF;
038100130326
038200130326       //?Emissione videata
038300130327         write DNK000T;
038400130327         exfmt DNK001D;
038500130326         ErrMessage  = *off;
038600130326         ErrGenerico = *off;
038700130326         clear V01msg;
038800130326
038900130326         SELECT;
039000130326
039100130326       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
039200130326           WHEN  wErrGrave;
039300130326             wFine = *on;
039400130327
039500130327       //?F2=Verifica Unificanti
039600130327           WHEN  dsp_aid = c_F02;
039700130327             exsr F02D01;
039800130326
039900130326       //?F3=Fine
040000130326           WHEN  dsp_aid = c_F03;
040100130326             exsr F03D01;
040200130326
040300130326       //?Enter
040400130326           OTHER;
040500130326             exsr CtrD01;
040600130326             IF  ErrGenerico;
040700130326               leavesr;
040800130326             ENDIF;
040900130327         //?I controlli sono OK passo al subfile
041000130328             wVideo  = 'S3';
041100130328             wInzS03 = *on;
041200130326
041300130326         ENDSL;
041400130326
041500130326       ENDSR;
041600130326
041700130326       //--------------------------------------------------------------
041800130326       //?Inizializzazione videata.
041900130326       //--------------------------------------------------------------
042000130326       BEGSR InzD01;
042100130327
042200130327         clear DNK001D;
042300130329
042400130329       //?Carico i dati del file in schiera
042500130329         exsr CarSK;
042600130326
042700130326       ENDSR;
042800130327
042900130327       //--------------------------------------------------------------
043000130327       //?Gestione tasto funzionale F2.
043100130327       //?F2=Verifica Unificanti
043200130327       //--------------------------------------------------------------
043300130327       BEGSR F02D01;
043400130327
043500130327       //?Gestione del subfile Verifica unificanti
043600130327         wVideo  = 'S2';
043700130327         wInzS02 = *on;
043800130327
043900130327       ENDSR;
044000130326
044100130326       //--------------------------------------------------------------
044200130326       //?Gestione tasto funzionale F3.
044300130326       //?F3=Fine
044400130326       //--------------------------------------------------------------
044500130326       BEGSR F03D01;
044600130326
044700130326       //?Chiusura del programma
044800130326         wFine = *on;
044900130326
045000130326       ENDSR;
045100130326
045200130326       //--------------------------------------------------------------
045300130326       //?Controllo videata.
045400130326       //--------------------------------------------------------------
045500130326       BEGSR CtrD01;
045600130326
045700130326         WindDspF = IndDspF;
045800130326         reset WindDspFb;
045900130326         IndDspF  = WindDspF;
046000130326
046100130327       //?Cliente di riferimento
046200130327       //?obbligatorio
046300130329         IF  V01cod = *blanks;
046400130326           ErrMessage  = *on;
046500130326           ErrGenerico = *on;
046600130329           PosCurCOD   = *on;
046700130328           V01msg      = skMSG(02);
046800130326           leavesr;
046900130326         ENDIF;
047000130327
047100130327       //?ricerca
047200130329         IF  %scan('?' : V01cod) > 0;
047300130327           clear dDKL;
047400130327           ErrGenerico = *on;
047500130329           PosCurCOD   = *on;
047600130327           clear TIBS02ds;
047700130327           T02mod = 'R';
047800130327           T02cod = 'DKL';
047900130327           T02sif = KNSIF;
048000130327           TNTBE_RicercaControllo  (kpjba : tibs02ds);
048100130327           IF  T02err = *blanks;
048200130329             V01cod  = T02ke1;
048300130327             dDKL    = T02uni;
048400130329             V01codd = §DKLrag;
048500130327             leavesr;
048600130329           ELSE;
048700130329             clear V01cod;
048800130329             V01msg = T02msg;
048900130329             ErrMessage = *on;
049000130329             leavesr;
049100130327           ENDIF;
049200130327         ENDIF;
049300130327
049400130327       //?solo caratteri numerici
049500130329         IF  %check(digits:V01cod) > 0;
049600130327           ErrMessage  = *on;
049700130327           ErrGenerico = *on;
049800130329           PosCurCOD   = *on;
049900130328           V01msg      = skMSG(02);
050000130327           leavesr;
050100130327         ENDIF;
050200130327
050300130327       //?codice valido in anagrafica
050400130327         clear  tibs69ds;
050500130329         I69kac = %int(V01cod);
050600130327         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
050700130327         IF  O69err <> *blanks;
050800130327           ErrMessage  = *on;
050900130327           ErrGenerico = *on;
051000130329           PosCurCOD   = *on;
051100130328           V01msg      = skMSG(02);
051200130327           leavesr;
051300130327         ENDIF;
051400130327
051500130327       //?codice abilitato in tabella DKL
051600130327         clear dDKL;
051700130327         clear TIBS02ds;
051800130327         T02mod = 'C';
051900130327         T02cod = 'DKL';
052000130327         T02sif = KNSIF;
052100130329         T02ke1 = V01cod;
052200130327         TNTBE_RicercaControllo  (kpjba : tibs02ds);
052300130327         IF  T02err <> *blanks;
052400130327           ErrMessage  = *on;
052500130327           ErrGenerico = *on;
052600130329           PosCurCOD   = *on;
052700130328           V01msg      = skMSG(02);
052800130327           leavesr;
052900130327         ENDIF;
053000130327         dDKL = T02uni;
053100130329         V01codd = §DKLrag;
053200130327
053300130326       ENDSR;
053400130327
053500130327       //--------------------------------------------------------------
053600130327       //?Gestione videata S02.
053700130327       //--------------------------------------------------------------
053800130327       BEGSR  GesS02;
053900130327
054000130327       //?Inizializzazione videata
054100130327         IF  wInzS02;
054200130327           exsr InzS02;
054300130327           wInzS02 = *off;
054400130402         //?Per errore durante l'inizializzazione delle videta
054500130402         //?torno alla prima con errore impostato
054600130402           IF  ErrGenerico;
054700130402             wVideo  = 'D1';
054800130402             leavesr;
054900130402           ENDIF;
055000130402         ENDIF;
055100130327
055200130327       //?Emissione videata
055300130328         write DNK002Z;
055400130328         exfmt DNK002C;
055500130327         ErrMessage  = *off;
055600130327         ErrGenerico = *off;
055700130328         clear V02msg;
055800130327
055900130327         SELECT;
056000130328
056100130328       //?F3=Fine
056200130328           WHEN  dsp_aid = c_F03;
056300130328             exsr F03D01;
056400130327
056500130327       //?F6=Conferma
056600130327           WHEN  dsp_aid = c_F06;
056700130328           //?Eseguo i controlli
056800130328             exsr CtrS02 ;
056900130328             IF  ErrGenerico;
057000130328               leavesr;
057100130328             ENDIF;
057200130327             exsr F06S02;
057300130329             exsr F12S02;
057400130327
057500130327       //?F12=Ritorno
057600130327           WHEN  dsp_aid = c_F12;
057700130327             exsr F12S02;
057800130327
057900130327       //?Enter
058000130327           OTHER;
058100130328         //?Eseguo i controlli
058200130327             exsr CtrS02;
058300130327             IF  ErrGenerico;
058400130327               leavesr;
058500130327             ENDIF;
058600130327
058700130327         ENDSL;
058800130327
058900130327       ENDSR;
059000130327
059100130327       //--------------------------------------------------------------
059200130327       //?Inizializzazione videata S02.
059300130327       //--------------------------------------------------------------
059400130327       BEGSR InzS02;
059500130328
059600130328       //?Pulizia del Subfile
059700130328         exsr PulS02;
059800130327
059900130328       //?Leggo la schiera dei codici Padre
060000130328         xx = 1;
060100130328         clear sav_skKUN;
060200130328         FOR xx by 1 to %elem(skKUN);
060300130328         //?Per ogni KUN richiamo TIBS10R per cercare legami ST
060400130328           IF  skKUN(xx) <> *zeros and skKUN(xx) <> sav_skKUN;
060500130328             clear tibs10ds;
060600130402             TIBS10DS.D10drf = %dec(%date());
060700130402             TIBS10DS.D10tle = 'ST';
060800130402             TIBS10DS.D10paf = 'F';
060900130402             TIBS10DS.D10cod = skKUN(xx);
061000130328             tibs10r (TIBS10DS);
061100130402           //?Se errore vuol dire che non è un padre
061200130402             IF  TIBS10DS.D10err <> *blanks;
061300130402             //?Verifico se figlio
061400130402               TIBS10DS.D10paf = 'P';
061500130402               tibs10r (TIBS10DS);
061600130402             //?Se è un figlio non deve essere memorizzato come KUN
061700130402               IF  TIBS10DS.D10err = *blanks;
061800130402                 ErrMessage  = *on;
061900130402                 ErrGenerico = *on;
062000130402                 V01msg      = skMsg(03);
062100130402                 %subst(V01msg:8:7) = %editc(skKUN(xx):'X');
062200130402                 leavesr;
062300130402               ENDIF;
062400130402             //?Se non trovo neanche come figlio è unico
062500130402               TIBS10DS.D10skcary(1) = skKUN(xx);
062600130328             ENDIF;
062700130402           //?Carico il subfile per ogni figlio Mancante (nuovo legame ST)
062800130402           //?                           oppure Eliminato (non più legame ST)
062900130402             exsr CarS02;
063000130402             sav_skKUN = skKUN(xx);
063100130402           ENDIF;
063200130402         //?Figlio da FILE non presente su TIBS10R
063300130402           IF  skKSC(xx) > *zeros and
063400130402               %lookup (skKSC(xx):TIBS10DS.D10skcary) = 0;
063500130402             V02mod = 'ELIMINATO';
063600130402             V02ksc = skKSC(xx);
063700130402             V02kun = skKUN(xx);
063800130402             clear V02kunnew;
063900130402             exsr RieS02;
064000130402           ENDIF;
064100130328         ENDFOR;
064200130328
064300130328       //?Visualizzazione del Subfile
064400130328         SflDsp_02 = (S02nrr <= *zeros);
064500130329         sav_S02nrr = S02nrr;
064600130328
064700130328       //?Attivazione SFLEND
064800130328         SflEnd_02 = *on;
064900130328
065000130328       //?Impaginazione Subfile
065100130328       //?Forzo sempre il primo rcd
065200130328         IF  S02nrr > *zero;
065300130328           V02rcd = 1;
065400130328           V02csr = 1;
065500130328         ELSE;
065600130328           clear V02rcd;
065700130328         ENDIF;
065800130328
065900130328         V02csr = V02rcd;
066000130327
066100130327       ENDSR;
066200130328
066300130328       //--------------------------------------------------------------
066400130329       //?Pulizia Subfile S02.
066500130328       //--------------------------------------------------------------
066600130328       BEGSR PulS02;
066700130328
066800130328         SflDsp_02    = *on;
066900130328         SflDspCtl_02 = *on;
067000130328         write  DNK002C;
067100130328         SflDspCtl_02 = *off;
067200130328         SflEnd_02    = *off;
067300130328
067400130328         clear V02rcd;
067500130328         clear V02csr;
067600130328         clear S02nrr;
067700130328         clear V02msg;
067800130328         ErrMessage  = *off;
067900130328         ErrGenerico = *off;
068000130328
068100130328         WindDspF = IndDspF;
068200130328         reset WindDspFb;
068300130328         IndDspF  = WindDspF;
068400130328
068500130328       ENDSR;
068600130328
068700130328       //--------------------------------------------------------------
068800130328       //?Controllo i figli con i dati del file.
068900130328       //--------------------------------------------------------------
069000130328       BEGSR CarS02;
069100130328
069200130328       //?Figlio da TIBS10R non presente su FILE
069300130328         yy = 1;
069400130329         FOR yy by 1 to %elem(TIBS10DS.D10skcary);
069500130402           IF  TIBS10DS.D10skcary(yy) = *zeros;
069600130402             iter;
069700130402           ENDIF;
069800130328         //?Non trovo sul file
069900130329           IF  %lookup (TIBS10DS.D10skcary(yy):skKSC) = 0;
070000130328             V02mod = 'MANCANTE';
070100130329             V02ksc = %dec(%subst(%editc(TIBS10DS.D10skcary(yy):'X'):5:7):7:0);
070200130328             V02kun = skKun(xx);
070300130328             clear V02kunnew;
070400130328             exsr RieS02;
070500130328             iter;
070600130328           ENDIF;
070700130328         //?Trovo sul file
070800130328           clear zz;
070900130329           zz =  %lookup (TIBS10DS.D10skcary(yy):skKSC);
071000130328         //?Ma padri diversi
071100130328           IF  zz > 0 and skKUN(zz) <> skKUN(xx);
071200130328             V02mod = 'UNIF.VAR.';
071300130328             V02kun = skKUN(zz);
071400130328             V02ksc = skKSC(zz);
071500130328             V02kunnew = skKUN(xx);
071600130328             exsr RieS02;
071700130328             iter;
071800130328           ENDIF;
071900130328         ENDFOR;
072000130328
072100130328       ENDSR;
072200130328
072300130328       //--------------------------------------------------------------
072400130329       //?Scrivo il rcd del subfile S02.
072500130328       //--------------------------------------------------------------
072600130328       BEGSR RieS02;
072700130328
072800130328         clear V02opz;
072900130328         V02cod = skCOD(xx);
073000130328         clear TIBS69ds;
073100130328         I69kac = V02cod;
073200130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
073300130328         V02codd = ACOrag;
073400130328         clear TIBS69ds;
073500130328         I69kac = V02kun;
073600130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
073700130328         V02kund = ACOrag;
073800130328         clear TIBS69ds;
073900130328         I69kac = V02ksc;
074000130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
074100130328         V02kscd = ACOrag;
074200130328         S02nrr += 1;
074300130328         write DNK002S;
074400130328
074500130328       ENDSR;
074600130327
074700130327       //--------------------------------------------------------------
074800130327       //?Gestione tasto funzionale F6.
074900130327       //?F6=Conferma.
075000130327       //--------------------------------------------------------------
075100130327       BEGSR F06S02;
075200130327
075300130328       //?Aggiorno i dati in base a quanto inserito nel subfile
075400130329         S02nrr = 1;
075500130329         FOR S02nrr by 1 to sav_S02nrr;
075600130329       //?Aggiorno i dati in base a quanto inserito nel subfile
075700130329           chain S02nrr DNK002S;
075800130329           IF  %found;
075900130329
076000130329           //?Aggiorno
076100130329             SELECT;
076200130329               //?Opzione "E"  ELIMINA
076300130329               WHEN  V02opz = 'E';
076400130329                 exec sql
076500130329                 DELETE from FNDKL00F
076600130329                 WHERE DKLcod = :V02cod and DKLkun = :V02kun and
076700130329                       DKLksc = :V02ksc;
076800130329               //?Opzione "I"  INSERISCI
076900130329               WHEN  V02opz = 'I';
077000130329                 clear FNDKL000;
077100130329                 DKLcod = V02cod;
077200130329                 DKLkun = V02kun;
077300130329                 DKLksc = V02ksc;
077400130329                 write  FNDKL000;
077500130329                 //?Opzione "M"  UNIFICANTE VARIATO
077600130329                 WHEN  V02opz = 'M';
077700130329                   exec sql
077800130408                   UPDATE FNDKL00F set DKLkun = :V02kunnew
077900130329                   WHERE DKLcod = :V02cod and DKLkun = :V02kun and
078000130329                         DKLksc = :V02ksc;
078100130329             ENDSL;
078200130329
078300130329             V02csr = S02nrr;
078400130329             V02rcd = S02nrr;
078500130329             UPDATE DNK002S;
078600130329           ENDIF;
078700130329         ENDFOR;
078800130327
078900130327       ENDSR;
079000130327
079100130327       //--------------------------------------------------------------
079200130327       //?Gestione tasto funzionale F12.
079300130327       //?F12=Ritorno.
079400130327       //--------------------------------------------------------------
079500130327       BEGSR F12S02;
079600130327
079700130328         wVideo = 'D1';
079800130328         wInzD01 = *on;
079900130327
080000130327       ENDSR;
080100130327
080200130327       //--------------------------------------------------------------
080300130327       //?Controllo videata S02.
080400130327       //--------------------------------------------------------------
080500130327       BEGSR CtrS02;
080600130327
080700130328         WindDspF = IndDspF;
080800130328         reset WindDspFb;
080900130328         IndDspF  = WindDspF;
081000130328
081100130328       //?Controllo cosa è stato inserito nel subfile
081200130328         readc DNK002S;
081300130328
081400130328         DOW  not %eof(FIDNK0D);
081500130328           %subst(IndDspF : 51) = *off;
081600130328           SflNxtChg_02 = *off;
081700130328           V02rcd = S02nrr;
081800130328
081900130328       //?Controllo se opzione valida
082000130328           SELECT;
082100130328             //?Nessuna opzione
082200130329             WHEN  V02opz  = *blank;
082300130328
082400130328             //?Opzione "E" ma non è ELIMINATO
082500130329             WHEN  V02opz = 'E' and %subst(V02mod:1:1) <> 'E';
082600130328               ErrMessage  = *on;
082700130328               ErrGenerico = *on;
082800130329               PosCurOPZ2  = *on;
082900130328               V02msg = skMSG(04);
083000130328
083100130328             //?Opzione "I" ma non è MANCANTE
083200130329             WHEN  V02opz = 'I' and %subst(V02mod:1:1) <> 'M';
083300130328               ErrMessage  = *on;
083400130328               ErrGenerico = *on;
083500130329               PosCurOPZ2  = *on;
083600130328               V02msg = skMSG(04);
083700130328
083800130328             //?Opzione "M" ma non è UNIFICANTE VARIATO
083900130329             WHEN  V02opz = 'M' and %subst(V02mod:1:1) <> 'U';
084000130328               ErrMessage  = *on;
084100130328               ErrGenerico = *on;
084200130329               PosCurOPZ2  = *on;
084300130328               V02msg = skMSG(04);
084400130328
084500130328             //?Opzione non prevista
084600130329             WHEN  V02opz <> *blanks and V02opz <> 'E' and
084700130329                   V02opz <> 'I' and V02opz <> 'M';
084800130328               ErrMessage  = *on;
084900130328               ErrGenerico = *on;
085000130329               PosCurOPZ2  = *on;
085100130328               V02msg = skMSG(04);
085200130328           ENDSL;
085300130328
085400130328       //?Aggiorno il subfile
085500130328           SELECT;
085600130328             WHEN  ErrMessage;
085700130328               SflNxtChg_02 = *on;
085800130328               V02csr       = V02rcd;
085900130328             WHEN  ErrGenerico;
086000130328               V02csr = V02rcd;
086100130329               clear  V02opz;
086200130328             OTHER;
086300130328               V02csr = V02rcd;
086400130328           ENDSL;
086500130328
086600130328           UPDATE DNK002S;
086700130328
086800130328       //?Al primo errore esco dalla lettura del subfile
086900130328           IF  ErrMessage or ErrGenerico;
087000130328             leave;
087100130328           ENDIF;
087200130328
087300130328           readc DNK002S;
087400130328
087500130328         ENDDO;
087600130327
087700130327       ENDSR;
087800130328
087900130328       //--------------------------------------------------------------
088000130328       //?Gestione videata S03.
088100130328       //--------------------------------------------------------------
088200130328       BEGSR  GesS03;
088300130328
088400130328       //?Inizializzazione videata
088500130328         IF  wInzS03;
088600130328           exsr InzS03;
088700130328           wInzS03 = *off;
088800130328         ENDIF;
088900130328
089000130328       //?Emissione videata
089100130328         write DNK003Z;
089200130328         exfmt DNK003C;
089300130328         ErrMessage  = *off;
089400130328         ErrGenerico = *off;
089500130328         clear V03msg;
089600130328
089700130328         SELECT;
089800130328
089900130328       //?F6=Conferma
090000130328           WHEN  dsp_aid = c_F06;
090100130328           //?Eseguo i controlli
090200130328             exsr CtrS03 ;
090300130328             IF  ErrGenerico;
090400130328               leavesr;
090500130328             ENDIF;
090600130328             exsr F06S03;
090700130328
090800130328       //?F7=Inserimento per Unificante
090900130328           WHEN  dsp_aid = c_F07;
091000130328             exsr F07S03;
091100130328
091200130328       //?F10=Immissione
091300130328           WHEN  dsp_aid = c_F10;
091400130328             exsr F10S03;
091500130328
091600130328       //?F12=Ritorno
091700130328           WHEN  dsp_aid = c_F12;
091800130328             exsr F12S02;
091900130328
092000130328       //?Enter
092100130328           OTHER;
092200130328         //?Eseguo i controlli
092300130328             exsr CtrS03;
092400130328             IF  ErrGenerico;
092500130328               leavesr;
092600130328             ENDIF;
092700130328
092800130328         ENDSL;
092900130328
093000130328       ENDSR;
093100130328
093200130328       //--------------------------------------------------------------
093300130328       //?Inizializzazione videata S03.
093400130328       //--------------------------------------------------------------
093500130328       BEGSR InzS03;
093600130328
093700130328       //?Pulizia del Subfile
093800130328         exsr PulS03;
093900130329
094000130329       //?Carico i dati nella testata del subfile
094100130329         exsr CarC03;
094200130328
094300130328       //?Carico il subfile
094400130328         exsr CarS03;
094500130328
094600130328       //?Visualizzazione del Subfile
094700130328         SflDsp_03 = (S03nrr <= *zeros);
094800130329         sav_S03nrr = S03nrr;
094900130328
095000130328       //?Attivazione SFLEND
095100130328         SflEnd_03 = *on;
095200130328
095300130328       //?Impaginazione Subfile
095400130328       //?Forzo sempre il primo rcd
095500130328         IF  S03nrr > *zero;
095600130328           V03rcd = 1;
095700130328           V03csr = 1;
095800130328         ELSE;
095900130328           clear V03rcd;
096000130328         ENDIF;
096100130328
096200130328         V03csr = V03rcd;
096300130328
096400130328       ENDSR;
096500130328
096600130328       //--------------------------------------------------------------
096700130329       //?Pulizia Subfile S03.
096800130328       //--------------------------------------------------------------
096900130328       BEGSR PulS03;
097000130328
097100130328         SflDsp_03    = *on;
097200130328         SflDspCtl_03 = *on;
097300130328         write  DNK003C;
097400130328         SflDspCtl_03 = *off;
097500130328         SflEnd_03    = *off;
097600130328
097700130328         clear V03rcd;
097800130328         clear V03csr;
097900130328         clear S03nrr;
098000130328         clear V03msg;
098100130328         ErrMessage  = *off;
098200130328         ErrGenerico = *off;
098300130328
098400130328         WindDspF = IndDspF;
098500130328         reset WindDspFb;
098600130328         IndDspF  = WindDspF;
098700130328
098800130328       ENDSR;
098900130329
099000130329       //--------------------------------------------------------------
099100130329       //?Carico la testata del subfile S03.
099200130329       //--------------------------------------------------------------
099300130329       BEGSR CarC03;
099400130329
099500130329       //?Codice di Riferimento dalla prima videata
099600130329         V03cod  = %int(V01cod);
099700130329         V03codd = V01codd;
099800130329
099900130329       //?Nr. C.A. da chiudere - Numeratore
100000130329         V03nca = §DKLnca;
100100130329         V03cnu = §DKLcnu;
100200130404         V03cau = §DKLcau;
100300130329
100400130329       //?Ultimo numero prelevato dal Numeratore
100500130329         clear TRUL33DS;
100600130404         I33cnu = §DKLcnu;
100700130329         I33ope = 1;
100800130405         I33aaa = wANNO;
100900130329         kpjbu = TRUL33DS;
101000130329         trul33r (KPJBA);
101100130329         TRUL33DS = kpjbu;
101200130405         V03unp = O33nrf;
101300130329
101400130329       ENDSR;
101500130328
101600130328       //--------------------------------------------------------------
101700130329       //?Carico il subfile S03.
101800130328       //--------------------------------------------------------------
101900130328       BEGSR CarS03;
102000130328
102100130329         wEnd = *off;
102200130329
102300130329       //?Carico tutti i codici legati al codice di riferimento
102400130329         exec sql
102500130329         DECLARE FNDKL1 cursor for
102600130329         SELECT * from FNDKL00F
102700130329         WHERE DKLcod = :V03cod
102800130329         ORDER BY DKLcod, DKLkun, DKLksc;
102900130329
103000130329       //?Apertura del cursore
103100130329         exec sql
103200130329         OPEN FNDKL1;
103300130329
103400130329         IF  sqlcode < 0;
103500130329           wEnd = *on;
103600130329         ENDIF;
103700130329
103800130329         DOW  not wEnd;
103900130329           exec sql
104000130329           FETCH next from FNDKL1 into :fndklds;
104100130329           IF  sqlcod = 100 or sqlcod < 0;
104200130329             wEnd = *on;
104300130329             leave;
104400130329           ENDIF;
104500130329
104600130329           exsr RieS03;
104700130329
104800130329         ENDDO;
104900130329
105000130329       //?Chiusura del cursore
105100130329         exec sql
105200130329         CLOSE FNDKL1;
105300130328
105400130328       ENDSR;
105500130328
105600130328       //--------------------------------------------------------------
105700130329       //?Scrivo il rcd del subfile S03.
105800130328       //--------------------------------------------------------------
105900130328       BEGSR RieS03;
106000130328
106100130329         clear DNK003S;
106200130329
106300130329         V03ksc = DKLksc;
106400130328         clear TIBS69ds;
106500130329         I69kac = V03ksc;
106600130328         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
106700130329         V03kscd = ACOrag;
106800130329         V03kun = DKLkun;
106900130329         clear TIBS69ds;
107000130329         I69kac = V03kun;
107100130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
107200130329         V03kund = ACOrag;
107300130329
107400130328         S03nrr += 1;
107500130328         write DNK003S;
107600130328
107700130328       ENDSR;
107800130328
107900130328       //--------------------------------------------------------------
108000130328       //?Gestione tasto funzionale F6.
108100130328       //?F6=Conferma.
108200130328       //--------------------------------------------------------------
108300130328       BEGSR F06S03;
108400130328
108500130329       //?Aggiorno i dati in base a quanto inserito nel subfile
108600130329         S03nrr = 1;
108700130329         FOR S03nrr by 1 to sav_S03nrr;
108800130329           chain S03nrr DNK003S;
108900130329           IF  %found;
109000130329
109100130329           //?Elimino
109200130329             IF  V03opz = 'E';
109300130329               exec sql
109400130329               DELETE from FNDKL00F
109500130329               WHERE DKLcod = :V03cod and DKLkun = :V03kun and
109600130329                     DKLksc = :V03ksc;
109700130329             ENDIF;
109800130329
109900130329             V03csr = S03nrr;
110000130329             V03rcd = S03nrr;
110100130329             UPDATE DNK003S;
110200130329           ENDIF;
110300130329
110400130329         ENDFOR;
110500130328
110600130328       //?Vado in prima videata
110700130328         wVideo = 'D1';
110800130328         wInzD01 = *on;
110900130328
111000130328       ENDSR;
111100130328
111200130328       //--------------------------------------------------------------
111300130328       //?Gestione tasto funzionale F7.
111400130328       //?F7=Inserimento per Unificante.
111500130328       //--------------------------------------------------------------
111600130328       BEGSR F07S03;
111700130328
111800130329       //?Ricerca cliente
111900130329         clear XCLIRDS;
112000130329         DXCcap = DUTkci;
112100130329         DXCaut = 'AZ';
112200130329         xclir (XCLIRDS);
112300130408         IF  DXCf03 <> *blanks or DXCerr <> *blanks;
112400130329           leavesr;
112500130329         ENDIF;
112600130329
112700130329         ksa = DXCksc;
112800130329         dsksa = ksa(1);
112900130329         ksc(1) = dsksn;
113000130402         wKSC = ksc(1);
113100130408         IF  wKSC = *zeros;
113200130408           leavesr;
113300130408         ENDIF;
113400130402
113500130402       //?Controllo che non sia già presente nel file come Padre
113600130402         exec sql
113700130402         SELECT count(*), DKLcod into :wConta, :DKLcod
113800130402         FROM FNDKL00F
113900130402         WHERE DKLkun = :wKSC
114000130402         GROUP BY DKLcod;
114100130402         IF sqlcod <> 0;
114200130402           clear wConta;
114300130402         ENDIF;
114400130402         IF  wConta > 0;
114500130402           ErrMessage  = *on;
114600130402           ErrGenerico = *on;
114700130402           V03msg      = skMSG(07);
114800130402           %subst(V03msg:64:7) = %editc(DKLcod:'X');
114900130402           wVideo = 'S3';
115000130402           leavesr;
115100130402         ENDIF;
115200130402
115300130402       //?Controllo che non sia già presente nel file come Figlio
115400130402         exec sql
115500130402         SELECT count(*), DKLcod into :wConta, :DKLcod
115600130402         FROM FNDKL00F
115700130402         WHERE DKLksc = :wKSC
115800130402         GROUP BY DKLcod;
115900130402         IF sqlcod <> 0;
116000130402           clear wConta;
116100130402         ENDIF;
116200130402         IF  wConta > 0;
116300130402           ErrMessage  = *on;
116400130402           ErrGenerico = *on;
116500130402           V03msg      = skMSG(08);
116600130402           %subst(V03msg:65:7) = %editc(DKLcod:'X');
116700130402           wVideo = 'S3';
116800130402           leavesr;
116900130402         ENDIF;
117000130329
117100130328         wVideo = 'W5';
117200130328         wInzW05 = *on;
117300130328
117400130328       ENDSR;
117500130328
117600130328       //--------------------------------------------------------------
117700130328       //?Gestione tasto funzionale F10.
117800130328       //?F10=Immissione.
117900130328       //--------------------------------------------------------------
118000130328       BEGSR F10S03;
118100130328
118200130329         wCarica = *off;
118300130328         wVideo = 'S4';
118400130328         wInzS04 = *on;
118500130328
118600130328       ENDSR;
118700130328
118800130328       //--------------------------------------------------------------
118900130328       //?Controllo videata S03.
119000130328       //--------------------------------------------------------------
119100130328       BEGSR CtrS03;
119200130328
119300130328         WindDspF = IndDspF;
119400130328         reset WindDspFb;
119500130328         IndDspF  = WindDspF;
119600130328
119700130329       //?Controllo cosa è stato inserito nel subfile
119800130329         readc  DNK003S;
119900130329
120000130329         DOW  not %eof(FIDNK0D);
120100130329           %subst(IndDspF : 52) = *off;
120200130329           SflNxtChg_03 = *off;
120300130329           V03rcd = S03nrr;
120400130328
120500130329         //?Controllo se opzione valida
120600130329           SELECT;
120700130329             //?Nessuna opzione
120800130329             WHEN  V03opz  = *blank;
120900130329             WHEN  V03opz  <> 'E';
121000130329               ErrMessage  = *on;
121100130329               ErrGenerico = *on;
121200130329               PosCurOPZ3  = *on;
121300130329               V03msg = skMSG(04);
121400130329           ENDSL;
121500130328
121600130329         //?Aggiorno il subfile
121700130329           SELECT;
121800130329             WHEN  ErrMessage;
121900130329               SflNxtChg_03 = *on;
122000130329               V03csr       = V03rcd;
122100130329             WHEN  ErrGenerico;
122200130329               V03csr = V03rcd;
122300130329               clear  V03opz;
122400130329             OTHER;
122500130329               V03csr = V03rcd;
122600130329           ENDSL;
122700130328
122800130329           UPDATE DNK003S;
122900130328
123000130329       //?Al primo errore esco dalla lettura del subfile
123100130329           IF  ErrMessage or ErrGenerico;
123200130329             leave;
123300130329           ENDIF;
123400130328
123500130329           readc DNK003S;
123600130328
123700130329         ENDDO;
123800130328
123900130328       ENDSR;
124000130329
124100130329       //--------------------------------------------------------------
124200130329       //?Gestione videata S04.
124300130329       //--------------------------------------------------------------
124400130329       BEGSR  GesS04;
124500130329
124600130329       //?Inizializzazione videata
124700130329         IF  wInzS04;
124800130329           exsr InzS04;
124900130329           wInzS04 = *off;
125000130329         ENDIF;
125100130329
125200130329       //?Emissione videata
125300130329         write DNK004Z;
125400130329         exfmt DNK004C;
125500130329         ErrMessage  = *off;
125600130329         ErrGenerico = *off;
125700130329         clear V04msg;
125800130329
125900130329         SELECT;
126000130329
126100130329       //?F6=Conferma
126200130329           WHEN  dsp_aid = c_F06;
126300130329           //?Eseguo i controlli
126400130329             exsr CtrS04 ;
126500130329             IF  ErrGenerico;
126600130329               leavesr;
126700130329             ENDIF;
126800130329             exsr F06S04;
126900130329
127000130329       //?F12=Ritorno
127100130329           WHEN  dsp_aid = c_F12;
127200130329             exsr F12S04;
127300130329
127400130329       //?Enter
127500130329           OTHER;
127600130329         //?Eseguo i controlli
127700130329             exsr CtrS04;
127800130329             IF  ErrGenerico;
127900130329               leavesr;
128000130329             ENDIF;
128100130329
128200130329         ENDSL;
128300130329
128400130329       ENDSR;
128500130329
128600130329       //--------------------------------------------------------------
128700130329       //?Inizializzazione videata S04.
128800130329       //--------------------------------------------------------------
128900130329       BEGSR InzS04;
129000130329
129100130329       //?Pulizia del Subfile
129200130329         exsr PulS04;
129300130329
129400130329       //?Carico i dati nella testata del subfile
129500130329         exsr CarC04;
129600130329
129700130329       //?Carico il subfile
129800130329         exsr CarS04;
129900130329
130000130329       //?Visualizzazione del Subfile
130100130329         SflDsp_04 = (S04nrr <= *zeros);
130200130329         sav_S04nrr = S04nrr;
130300130329
130400130329       //?Attivazione SFLEND
130500130329         SflEnd_04 = *on;
130600130329
130700130329       //?Impaginazione Subfile
130800130329       //?Forzo sempre il primo rcd
130900130329         IF  S04nrr > *zero;
131000130329           V04rcd = 1;
131100130329           V04csr = 1;
131200130329         ELSE;
131300130329           clear V04rcd;
131400130329         ENDIF;
131500130329
131600130329         V04csr = V04rcd;
131700130329
131800130329       ENDSR;
131900130329
132000130329       //--------------------------------------------------------------
132100130329       //?Pulizia Subfile S04.
132200130329       //--------------------------------------------------------------
132300130329       BEGSR PulS04;
132400130329
132500130329         SflDsp_04    = *on;
132600130329         SflDspCtl_04 = *on;
132700130329         write  DNK004C;
132800130329         SflDspCtl_04 = *off;
132900130329         SflEnd_04    = *off;
133000130329
133100130329         clear V04rcd;
133200130329         clear V04csr;
133300130329         clear S04nrr;
133400130329         clear V04msg;
133500130329         ErrMessage  = *off;
133600130329         ErrGenerico = *off;
133700130329
133800130329         WindDspF = IndDspF;
133900130329         reset WindDspFb;
134000130329         IndDspF  = WindDspF;
134100130329
134200130329       ENDSR;
134300130329
134400130329       //--------------------------------------------------------------
134500130329       //?Carico la testata del subfile S04.
134600130329       //--------------------------------------------------------------
134700130329       BEGSR CarC04;
134800130329
134900130329       //?Codice di Riferimento dalla prima videata
135000130329         V04cod  = %int(V01cod);
135100130329         V04codd = V01codd;
135200130329
135300130329       //?Nr. C.A. da chiudere - Numeratore
135400130329         V04nca = §DKLnca;
135500130329         V04cnu = §DKLcnu;
135600130404         V04cau = §DKLcau;
135700130329
135800130329       //?Ultimo numero prelevato dal Numeratore
135900130329         V04unp = V03unp;
136000130329
136100130329       ENDSR;
136200130329
136300130329       //--------------------------------------------------------------
136400130329       //?Carico il subfile S04.
136500130329       //--------------------------------------------------------------
136600130329       BEGSR CarS04;
136700130329
136800130329         xx = 1;
136900130329       //?Carico 99 righe vuote nel subfile
137000130329       //?se non provengo da caricamento totale
137100130329         IF  not wCarica;
137200130329           FOR xx by 1 to 99;
137300130329             exsr RieS04;
137400130329           ENDFOR;
137500130329       //?se carica totale leggo SK TIBS10
137600130329         ELSE;
137700130329           DOW TIBS10DS.D10skcary(xx) > *zeros;
137800130329             exsr RieS04;
137900130329             xx += 1;
138000130329           ENDDO;
138100130329         ENDIF;
138200130329
138300130329       ENDSR;
138400130329
138500130329       //--------------------------------------------------------------
138600130329       //?Scrivo il rcd del subfile S04.
138700130329       //--------------------------------------------------------------
138800130329       BEGSR RieS04;
138900130329
139000130329         clear DNK004S;
139100130329         ProtS04 = *off;
139200130329
139300130329         IF  wCarica;
139400130329           V04ksc = %subst(%editc(TIBS10DS.D10skcary(xx):'X'):5:7);
139500130329           I69kac = %int(V04ksc);
139600130409           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
139700130329           V04kscd = ACOrag;
139800130409           V04kun = TIBS10DS.D10cop;
139900130409           I69kac = %int(V04kun);
140000130329           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
140100130329           V04kund = ACOrag;
140200130329           ProtS04 = *on;
140300130329         ENDIF;
140400130329
140500130329         S04nrr += 1;
140600130329         write DNK004S;
140700130329
140800130329       ENDSR;
140900130329
141000130329       //--------------------------------------------------------------
141100130329       //?Gestione tasto funzionale F6.
141200130329       //?F6=Conferma.
141300130329       //--------------------------------------------------------------
141400130329       BEGSR F06S04;
141500130329
141600130329       //?Aggiorno i dati in base a quanto inserito nel subfile
141700130329         S04nrr = 1;
141800130329         FOR S04nrr by 1 to sav_S04nrr;
141900130329       //?Aggiorno i dati in base a quanto inserito nel subfile
142000130329           chain S04nrr DNK004S;
142100130329           IF  %found;
142200130329         //?Scrivo
142300130329             IF  V04ksc <> *blanks and V04ksc <> *zeros;
142400130329               clear FNDKL000;
142500130329               DKLcod = V04cod;
142600130329               DKLkun = V04kun;
142700130329               DKLksc = %int(V04ksc);
142800130329               write  FNDKL000;
142900130329             ENDIF;
143000130329
143100130329             V04csr = S04nrr;
143200130329             V04rcd = S04nrr;
143300130329             UPDATE DNK004S;
143400130329           ENDIF;
143500130329
143600130329         ENDFOR;
143700130329
143800130329       //?Vado al subfile con tutti i codici caricati
143900130329         wVideo = 'S3';
144000130329         wInzS03 = *on;
144100130329
144200130329       ENDSR;
144300130329
144400130329       //--------------------------------------------------------------
144500130329       //?Gestione tasto funzionale F12.
144600130329       //?F12=Ritorno.
144700130329       //--------------------------------------------------------------
144800130329       BEGSR F12S04;
144900130329
145000130329         wVideo = 'S3';
145100130329         wInzS03 = *on;
145200130329
145300130329       ENDSR;
145400130329
145500130329       //--------------------------------------------------------------
145600130329       //?Controllo videata S04.
145700130329       //--------------------------------------------------------------
145800130329       BEGSR CtrS04;
145900130329
146000130329         WindDspF = IndDspF;
146100130329         reset WindDspFb;
146200130329         IndDspF  = WindDspF;
146300130329
146400130329       //?Controllo cosa è stato inserito nel subfile
146500130329         readc  DNK004S;
146600130329
146700130329         DOW  not %eof(FIDNK0D);
146800130329           %subst(IndDspF : 53) = *off;
146900130329           SflNxtChg_04 = *off;
147000130329           V04rcd = S04nrr;
147100130329
147200130329       //?Controllo il cliente
147300130329           IF  V04ksc <> *blanks and V04ksc <> *zeros;
147400130329             exsr ctrKSC;
147500130329           ELSE;
147600130329             clear V04ksc;
147700130329             clear V04kscd;
147800130329             clear V04kun;
147900130329             clear V04kund;
148000130329           ENDIF;
148100130329
148200130329       //?Aggiorno il subfile
148300130329           SELECT;
148400130329             WHEN  ErrMessage;
148500130329               SflNxtChg_04 = *on;
148600130329               V04csr       = V04rcd;
148700130329             WHEN  ErrGenerico;
148800130329               V04csr = V04rcd;
148900130329             OTHER;
149000130329               V04csr = V04rcd;
149100130329           ENDSL;
149200130329
149300130329           UPDATE DNK004S;
149400130329
149500130329       //?Al primo errore esco dalla lettura del subfile
149600130329           IF  ErrMessage or ErrGenerico;
149700130329             leave;
149800130329           ENDIF;
149900130329
150000130329           readc DNK004S;
150100130329
150200130329         ENDDO;
150300130329
150400130329       ENDSR;
150500130329
150600130329       //--------------------------------------------------------------
150700130329       //?Controllo cliente.
150800130329       //--------------------------------------------------------------
150900130329       BEGSR CtrKSC;
151000130329
151100130329       //?ricerca
151200130329         IF  %scan('?' : V04ksc) > 0;
151300130329           xpardut = RSut;
151400130329           xparkut = 1;
151500130329           xparrag = V04kscd;
151600130329           xparkcc = DUTkci;
151700130329           xparsta = 9;          // esclusione annullati
151800130329           clear xparflr;
151900130329           xparnum = 1;
152000130329           clear xparkcm;
152100130329           clear xparksm;
152200130329           clear xparkdm;
152300130329           clear xparesci;
152400130329           clear xparerr;
152500130329           clear xpariva;
152600130329           clear xparcdf;
152700130329           XALFA3BR (xpardut : xparkut : xparrag :
152800130329                     xparkcc : xparsta : xparflr :
152900130329                     xpardit : xparnum : xparkcm :
153000130329                     xparksm : xparkdm : xparesci :
153100130329                     xparerr : xpariva : xparcdf);
153200130329           IF  xparsta <> -1;
153300130329             V04ksc  = %subst(xparksm:1:7);
153400130329             V04kscd = xparrag;
153500130329           ENDIF;
153600130329           ErrGenerico = *on;
153700130329           PosCurKSC   = *on;
153800130329         ENDIF;
153900130329
154000130329       //?Solo caratteri numerici
154100130329         IF  %check(digits:V04ksc) > 0;
154200130329           ErrMessage  = *on;
154300130329           ErrGenerico = *on;
154400130329           PosCurKSC   = *on;
154500130329           V04msg      = skMSG(02);
154600130329           leavesr;
154700130329         ENDIF;
154800130329
154900130329       //?Controllo che sia un codice valido
155000130329         clear  tibs69ds;
155100130329         I69kac = %int(V04ksc);
155200130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
155300130329         IF  O69err <> *blanks;
155400130329           ErrMessage  = *on;
155500130329           ErrGenerico = *on;
155600130329           PosCurKSC   = *on;
155700130329           V04msg      = skMSG(02);
155800130329           leavesr;
155900130329         ENDIF;
156000130329         V04kscd = ACOrag;
156100130402
156200130402       //?Controllo che non sia già presente nel file come Padre
156300130402         exec sql
156400130402         SELECT count(*), DKLcod into :wConta, :DKLcod
156500130402         FROM FNDKL00F
156600130402         WHERE DKLkun = :ACOksc
156700130402         GROUP BY DKLcod;
156800130402         IF sqlcod <> 0;
156900130402           clear wConta;
157000130402         ENDIF;
157100130402         IF  wConta > 0;
157200130402           ErrMessage  = *on;
157300130402           ErrGenerico = *on;
157400130402           PosCurKSC   = *on;
157500130402           V04msg      = skMSG(05);
157600130402           %subst(V04msg:57:7) = %editc(DKLcod:'X');
157700130402           leavesr;
157800130402         ENDIF;
157900130402
158000130402       //?Controllo che non sia già presente nel file come Figlio
158100130402         exec sql
158200130402         SELECT count(*), DKLcod into :wConta, :DKLcod
158300130402         FROM FNDKL00F
158400130402         WHERE DKLksc = :ACOksc
158500130402         GROUP BY DKLcod;
158600130402         IF sqlcod <> 0;
158700130402           clear wConta;
158800130402         ENDIF;
158900130402         IF  wConta > 0;
159000130402           ErrMessage  = *on;
159100130402           ErrGenerico = *on;
159200130402           PosCurKSC   = *on;
159300130402           V04msg      = skMSG(06);
159400130402           %subst(V04msg:58:7) = %editc(DKLcod:'X');
159500130402           leavesr;
159600130402         ENDIF;
159700130329
159800130329       //?Se codice cliente inserito ricerco il padre
159900130329         clear tibs10ds;
160000130329         TIBS10ds.D10drf = %dec(%date());
160100130329         TIBS10ds.D10tle = 'ST';
160200130329         TIBS10ds.D10paf = 'F';
160300130329         TIBS10ds.D10cod = %int(V04ksc);
160400130329         tibs10r (TIBS10DS);
160500130329       //?Se non trovo come padre lo cerco come figlio
160600130329         IF  TIBS10ds.D10err <> *blanks;
160700130329           TIBS10ds.D10paf = 'P';
160800130329           tibs10r (TIBS10DS);
160900130329         //?Se non trovo imposto padre = figlio
161000130329           IF  TIBS10ds.D10err <> *blanks;
161100130329             TIBS10DS.D10cop = %int(V04ksc);
161200130329           ENDIF;
161300130329         ENDIF;
161400130329
161500130329         V04kun = TIBS10DS.D10cop;
161600130329       //?Decodifico il padre
161700130329         clear  tibs69ds;
161800130329         I69kac = %int(V04kun);
161900130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
162000130329         V04kund = ACOrag;
162100130329
162200130329       ENDSR;
162300130329
162400130329       //--------------------------------------------------------------
162500130329       //?Gestione videata W05.
162600130329       //--------------------------------------------------------------
162700130329       BEGSR  GesW05;
162800130329
162900130329       //?Inizializzazione videata
163000130329         IF  wInzW05;
163100130329           exsr InzW05;
163200130329           wInzW05 = *off;
163300130329         ENDIF;
163400130329
163500130329       //?Emissione videata
163600130329         exfmt DNK005W;
163700130329         ErrMessage  = *off;
163800130329         ErrGenerico = *off;
163900130329
164000130329         SELECT;
164100130329
164200130329       //?F12=Ritorno
164300130329           WHEN  dsp_aid = c_F12;
164400130329             exsr F12W05;
164500130329
164600130329       //?Enter
164700130329           OTHER;
164800130329         //?Se immesso SI devo caricari i codici figli + padre
164900130329             IF  W05conf = 'SI';
165000130329               exsr Carica;
165100130329               wVideo = 'S4';
165200130329               wInzS04 = *on;
165300130329               leavesr;
165400130329             ENDIF;
165500130329
165600130329         ENDSL;
165700130329
165800130329       ENDSR;
165900130329
166000130329       //--------------------------------------------------------------
166100130329       //?Inizializzazione videata W05.
166200130329       //--------------------------------------------------------------
166300130329       BEGSR InzW05;
166400130329
166500130329       //?Imposto il cliente selezionato dalla ricerca
166600130329         W05ksc = ksc(1);
166700130329
166800130329       //?Decodifico
166900130329         clear TIBS69ds;
167000130329         I69kac = W05ksc;
167100130329         TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
167200130329         W05kscd = ACOrag;
167300130329
167400130329       //?SI di conferma
167500130329         clear  W05conf;
167600130329
167700130329       ENDSR;
167800130329
167900130329       //--------------------------------------------------------------
168000130329       //?Gestione tasto funzionale F12.
168100130329       //?F12=Ritorno.
168200130329       //--------------------------------------------------------------
168300130329       BEGSR F12W05;
168400130329
168500130329         wVideo = 'S3';
168600130329         wInzS03 = *on;
168700130329
168800130329       ENDSR;
168900130327
169000130329       //--------------------------------------------------------------
169100130329       //?Carico tutti i codici.
169200130329       //--------------------------------------------------------------
169300130329       BEGSR Carica;
169400130329
169500130329         wCarica = *on;
169600130329         clear tibs10ds;
169700130329         TIBS10ds.D10drf = %dec(%date());
169800130329         TIBS10ds.D10tle = 'ST';
169900130329         TIBS10ds.D10paf = 'F';
170000130329         TIBS10ds.D10cod = W05ksc;
170100130329         tibs10r (TIBS10DS);
170200130329       //?Se non trovo come padre lo cerco come figlio
170300130329         IF  TIBS10ds.D10err <> *blanks;
170400130329           TIBS10ds.D10paf = 'P';
170500130329           tibs10r (TIBS10DS);
170600130329         //?Se non trovo imposto padre = figlio
170700130329           IF  TIBS10ds.D10err <> *blanks;
170800130329             TIBS10DS.D10cop = W05ksc;
170900130329             TIBS10DS.D10skcary(1) = W05ksc;
171000130329           ENDIF;
171100130329         ENDIF;
171200130329
171300130329       ENDSR;
171400130329
171500130327       //--------------------------------------------------------------
171600130327       //?Operazioni finali.
171700130327       //--------------------------------------------------------------
171800130327       BEGSR RoutEnd;
171900130327
172000130327         *inLR = *on;
172100130327         return;
172200130327
172300130327       ENDSR;
172400130327
172500130327      /end-free
172600130326
172700130326       //--------------------------------------------------------------
172800130326       //?Schiere a tempo di compilazione.
172900130326       //--------------------------------------------------------------
173000130326
173100130326** - skMSG ------------------------------------------------------------------*
173200130328Superato nr. MAX di Clienti consentiti. AVVISARE ICT!                          01
173300130328Cliente errato o non abilitato alla chiusura forzata                           02
173400130402Codice xxxxxxx memorizzato come PADRE, nei legami "ST" è FIGLIO. AVVISARE ICT! 03
173500130328Opzione errata                                                                 04
173600130402Il codice è già presente come PADRE sul cod.Riferimento xxxxxxx                05
173700130402Il codice è già presente come FIGLIO sul cod.Riferimento xxxxxxx               06
173800130402Il codice scelto è già presente come PADRE sul cod.Riferimento xxxxxxx         07
173900130402Il codice scelto è già presente come FIGLIO sul cod.Riferimento xxxxxxx        08
