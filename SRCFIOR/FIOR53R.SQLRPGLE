000100130423      //---------------------------------------------------------------
000200130423      //?FIOR53R - STAMPA ORM EXPORT
000300130423      //---------------------------------------------------------------
000400130423
000500130423     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000600130423
000700130423      //---------------------------------------------------------------
000800130423      //?Dichiarazione file.
000900130423      //---------------------------------------------------------------
001000130424     fAZORG01L  if   e           k disk
001100130424     fTABEL00F  if   e           k disk
001200130423     fFIOR53D   cf   e             workstn
001300130424     f                                     sfile(OR53S02 : S02nrr)
001400130423     f                                     indds(IndDspF)
001500130423     f                                     infds(InfDspF)
001600130423
001700130423      //---------------------------------------------------------------
001800130423      //?Definizione costanti.
001900130423      //---------------------------------------------------------------
002000130423
002100130423      // - Tasti funzionali a video
002200130423     d c_F01           c                   const(x'31')
002300130423     d c_F02           c                   const(x'32')
002400130423     d c_F03           c                   const(x'33')
002500130423     d c_F04           c                   const(x'34')
002600130423     d c_F05           c                   const(x'35')
002700130423     d c_F06           c                   const(x'36')
002800130423     d c_F07           c                   const(x'37')
002900130423     d c_F08           c                   const(x'38')
003000130423     d c_F09           c                   const(x'39')
003100130423     d c_F10           c                   const(x'3A')
003200130423     d c_F11           c                   const(x'3B')
003300130423     d c_F12           c                   const(x'3C')
003400130423     d c_F13           c                   const(x'B1')
003500130423     d c_F14           c                   const(x'B2')
003600130423     d c_F15           c                   const(x'B3')
003700130423     d c_F16           c                   const(x'B4')
003800130423     d c_F17           c                   const(x'B5')
003900130423     d c_F18           c                   const(x'B6')
004000130423     d c_F19           c                   const(x'B7')
004100130423     d c_F20           c                   const(x'B8')
004200130423     d c_F21           c                   const(x'B9')
004300130423     d c_F22           c                   const(x'BA')
004400130423     d c_F23           c                   const(x'BB')
004500130423     d c_F24           c                   const(x'BC')
004600130423     d c_Enter         c                   const(x'F1')
004700130423
004800130423     d Digits          c                   const('0123456789')
004900130423
005000130423      //---------------------------------------------------------------
005100130423      //?Definizione schiere.
005200130423      //---------------------------------------------------------------
005300130424      // - Sk linee estere
005400130424     d wSKpor          s              3  0 dim(30)
005500130510      // - Sk linee con errore in stampa
005600130510     d wSKporE         s              3  0 dim(30)
005700130423      // - Messaggi di errore
005800130423     d wMsg            s             78    dim(30) ctdata perrcd(1)
005900130423
006000130423      //---------------------------------------------------------------
006100130423      //?Definizione aree dati.
006200130423      //---------------------------------------------------------------
006300130423      // - Dati utente
006400130423     d §AzUte        e ds                  extname(AZUTE00F)
006500130423     d                                     dtaara
006600130423     d §DatiUte      e ds                  extname(dDatiUte)
006700130423     d                                     dtaara
006800130423
006900130423      //---------------------------------------------------------------
007000130423      //?Definizione strutture dati.
007100130423      //---------------------------------------------------------------
007200130423      // - Status
007300130423     d Psds           sds
007400130423     d   SDSpgm          *proc
007500130423
007600130423      // - InfDS
007700130423     d InfDspF         ds
007800130423     d  dsp_aid              369    369a
007900130423     d  dsp_rig              370    370
008000130423     d  dsp_col              371    371
008100130423
008200130423      // - Indicatori su DspF
008300130423     d IndDspF         ds
008400130423        // - Indicatori di gestione del subfile
008500130424     d  SflDsp                        1n   overlay(IndDspF : 30)
008600130424     d  SflDspCtl                     1n   overlay(IndDspF : 31)
008700130423     d  SflEnd                        1n   overlay(IndDspF : 32)
008800130424     d  SflNxtChg                     1n   overlay(IndDspF : 33)
008900130423        // - Indicatori di errore
009000130423     d  ErrMessage                    1n   overlay(IndDspF : 28)
009100130423        // - Indicatore di pozionamento cursore
009200130508     d  PosCurOPZ                     1n   overlay(IndDspF : 50)
009300130510     d  ProtCurOPZ                    1n   overlay(IndDspF : 51)
009400130423        // - Indicatori di errore generico
009500130423     d  ErrGenerico                   1n   overlay(IndDspF : 99)
009600130424
009700130423      // Campi di comodo per gestione indicatori a video
009800130423     d WindDspF        ds                  inz
009900130423     d   WindDspFa             1     49    inz(*zero)
010000130423     d   WindDspFb            50     99    inz(*zero)
010100130423
010200130423      // - Controllo data
010300130423     d wlbdat          ds                  inz
010400130423     d  g02dat                 1      8  0
010500130423     d  g02inv                 9     16  0
010600130423     d  g02err                17     17
010700130423     d  g02tgi                18     22  0
010800130423
010900130423      // - Parametri ricevuti
011000130423     d KPJBA         e ds
011100130424
011200130424      // - DS passaggio dati a pgm di stampa
011300130424     d FIOR16DS      e ds
011400130423
011500130423      // - Reperimento dati utente
011600130424     d TIBS34DS      e ds
011700130424
011800130424      // - Filiali gestibili dall'utente
011900130424     d TRUL06DS      e ds
012000130424     d  l1                     1     90  0 dim(30)
012100130424
012200130424      // - DS per TRUL90R - Richiesta stampanti
012300130424     d TRUL90DS      e ds
012400130424
012500130424      // - Tabella 5P
012600130424     d ds5P          e ds                  inz
012700130424     d   §5Ppgm      e                     inz('FIOR18R   ')
012800130424
012900130424      // - Organigramma
013000130424     d og143         e ds
013100130424
013200130424      // - File ORM
013300130424     d FNORM00F      e ds
013400130423
013500130423      //---------------------------------------------------------------
013600130423      //?Definizione variabili globali.
013700130423      //---------------------------------------------------------------
013800130423
013900130423      // - Flags booleani
014000130423     d wFine           s               n   inz(*off)
014100130423     d wEoF            s               n   inz(*off)
014200130423     d wErrGrave       s               n   inz(*off)
014300130510     d wErrStp         s               n   inz(*off)
014400130508     d wInzS02         s               n   inz(*on)
014500130424     d wNoestero       s               n   inz(*off)
014600130423
014700130423      // - Indici di schiera
014800130423     d xx              s              3  0 inz
014900130424     d yy              s              3  0 inz
015000130423
015100130423      // - Campi associati al video
015200130508     d wVideo          s              2    inz('S2')
015300130424     d S02nrr          s              4  0 inz
015400130424
015500130424       // - Stringa SQL da eseguire
015600130424     d wSQL            s           2048    Varying        inz
015700130423
015800130423      // - Campi di comodo data
015900130423     d wDataEUR        s               d   datfmt(*eur)
016000130423     d wDataISO        s               d   datfmt(*iso)
016100130423     d wData           s              8  0 inz
016200130423     d wOggi           s              8  0 inz
016300130423
016400130423       // - Stringa SQL da eseguire
016500130423     d StringaSQL      s           2048    varying        inz
016600130423
016700130423      // - Campi di comodo
016800130424     d codut           s              1s 0 inz(1)
016900130424     d sav_S02nrr      s                   like(S02nrr)
017000130423     d wconta          s             10i 0
017100130423
017200130423      //---------------------------------------------------------------
017300130423      //?Definizione procedure usate.
017400130423      //---------------------------------------------------------------
017500130424      // - Recupero filiali gestite dall'utente
017600130424     d trul06r         pr                  extpgm('TRUL06R')
017700130424     d  kpjba                              likeds(KPJBA)
017800130424
017900130424      // - Richiesta stampante
018000130424     d trul90r         pr                  extpgm('TRUL90R')
018100130424     d  kpjba                              likeds(KPJBA)
018200130424     d  trul90ds                           likeds(TRUL90DS)
018300130423
018400130423      //---------------------------------------------------------------
018500130423      //?Definizione prototipi.
018600130423      //---------------------------------------------------------------
018700130423      /copy gaitrasrc/srcprotopr,tibs34r
018800130423      /copy gaitrasrc/srcprotopr,xsrda8
018900130423
019000130423      //---------------------------------------------------------------
019100130423      //?Definizione key-list.
019200130423      //---------------------------------------------------------------
019300130423
019400130423      //---------------------------------------------------------------
019500130423      //?Riepilogo indicatori.
019600130423      //---------------------------------------------------------------
019700130423
019800130423
019900130423      //---------------------------------------------------------------
020000130423      //?M A I N - L I N E
020100130423      //---------------------------------------------------------------
020200130423
020300130423     c     *Entry        plist
020400130423     c                   parm                    KPJBA
020500130423
020600130423      /free
020700130423
020800130423       //?Operazioni iniziali
020900130423       exsr RoutInz;
021000130423
021100130423       exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
021200130423
021300130423       //?Gestione video
021400130423       DOW not wFine;
021500130423         SELECT;
021600130423           WHEN wVideo = 'S2';
021700130423             exsr GesS02;
021800130423           OTHER;
021900130423             wFine = *on;
022000130423         ENDSL;
022100130423       ENDDO;
022200130423
022300130423       //?Operazioni finali
022400130423       exsr RoutEnd;
022500130423
022600130423       //--------------------------------------------------------------
022700130423       //?Operazioni iniziali.
022800130423       //--------------------------------------------------------------
022900130423       BEGSR RoutInz;
023000130423
023100130423       //?Reperimento dati job
023200130423         exsr DatiJob;
023300130424
023400130424       //?Carico la £1
023500130424         clear TRUL06DS;
023600130424         D06cod = '£1';
023700130424         D06key = %editc(DUTpou:'X');
023800130424         kpjbu = TRUL06DS;
023900130424         trul06r (kpjba);
024000130429         TRUL06DS = kpjbu;
024100130424
024200130424       //?Se trovo anche una sola filiale estera OK altrimenti esco dalla procedura
024300130424         wNoestero = *off;
024400130424         xx = 1;
024500130424         clear yy;
024600130424         FOR xx by 1 to %elem(l1);
024700130424           IF  l1(xx) > 0;
024800130424             clear OG143;
024900130424             chain l1(xx) AZORG01L;
025000130424             IF  %found(AZORG01L);
025100130424               OG143 = ORGde3;
025200130424             ENDIF;
025300130424             IF  §OGntw = 'FED' or §OGntw = 'DPD' or
025400130424                 §OGntw = 'EEX';
025500130424               yy += 1;
025600130424               wSKpor(yy) = l1(xx);
025700130424             ENDIF;
025800130424           ENDIF;
025900130424         ENDFOR;
026000130429
026100130429         IF  wSKpor(1) = *zeros;
026200130429           wNoEstero = *on;
026300130429         ENDIF;
026400130424
026500130424         IF  wNoestero;
026600130424           leavesr;
026700130424         ENDIF;
026800130424
026900130424       //?Richiesta stampante
027000130424         clear FIOR16DS;
027100130424         clear TRUL90DS;
027200130424         D90rso = 'S';
027300130424         trul90r (kpjba : TRUL90DS);
027400130424         IF  D90f3 = '1';
027500130424           wFine = *on;
027600130424           leavesr;
027700130424         ENDIF;
027800130424
027900130424       //?Dati da tabella 5P
028000130424         reset ds5P;
028100130424         IF  %subst(knmus : 1 : 3) = 'EDP';
028200130424           TBLkey = 'O' + 'EDP';
028300130424         ELSE;
028400130424           TBLkey = 'O' + %editc( DUTpou : 'X' );
028500130424         ENDIF;
028600130424         chain  ( Codut : '5P' : TBLkey )  TABEL;
028700130424         IF  %found( TABEL00F );
028800130424           ds5P = TBLuni;
028900130424         ENDIF;
029000130424
029100130424         R16mdo = D90mdo;
029200130424         R16oqo = D90pro;
029300130423
029400130423       //?Imposto la data del giorno
029500130423         wOggi = %dec(%date());
029600130423         wDataISO = %date(wOggi:*iso);
029700130423         wDataEUR = wDataISO;
029800130423         wData = %dec(wDataEUR);
029900130423
030000130423       ENDSR;
030100130423
030200130423       //--------------------------------------------------------------
030300130423       //?Reperimento Dati del job (Utente/Operativi).
030400130423       //--------------------------------------------------------------
030500130423       BEGSR DatiJob;
030600130423
030700130423         in(E) §AzUte;
030800130423         IF not %error;
030900130423           in(E) §DatiUte;
031000130423         ENDIF;
031100130423         IF %error or RSut = *blanks;
031200130423           clear TIBS34ds;
031300130423           tibs34r(tibs34ds);
031400130423           in §AzUte;
031500130423           in §DatiUte;
031600130423         ENDIF;
031700130423
031800130423       ENDSR;
031900130423
032000130423       //--------------------------------------------------------------
032100130423       //?Gestione videata S02.
032200130423       //--------------------------------------------------------------
032300130423       BEGSR GesS02;
032400130423
032500130423       //?Inizializzazione videata
032600130423         IF  wInzS02;
032700130423           exsr InzS02;
032800130423           wInzS02 = *off;
032900130423         ENDIF;
033000130423
033100130423       //?Visualizzazione del SFL (se ci sono dati)
033200130424         SflDsp = (S02nrr <= *zeros);
033300130423
033400130423       //?Posizionamento cursore al 1° rcd della pagina
033500130423         IF  V02csr > *zero;
033600130423           V02rcd = V02csr;
033700130423         ELSE;
033800130423           V02rcd = 1;
033900130423         ENDIF;
034000130423
034100130423         V02csr = V02rcd;
034200130423
034300130423       //?Emissione Testata e Piede con tasti funzionali abilitati
034400130424         write  OR53T01;
034500130424         write  OR53Z02;
034600130423
034700130423       //?Emissione videata
034800130424         exfmt  OR53C02;
034900130423         reset ErrMessage;
035000130423         reset ErrGenerico;
035100130423         clear V02msg;
035200130423
035300130423         SELECT;
035400130508
035500130508       //?Rilevato errore grave: qualsiasi tasto venga premuto, esce dal pgm
035600130508           WHEN  wErrGrave;
035700130508             wFine = *on;
035800130423
035900130423       //?- F03=Fine
036000130423           WHEN  dsp_aid = c_F03;
036100130508             exsr F03S02;
036200130424
036300130424       //?- F06=Conferma
036400130424           WHEN  dsp_aid = c_F06;
036500130424             exsr CtrS02 ;
036600130424             IF  ErrGenerico;
036700130424               leavesr;
036800130424             ENDIF;
036900130424             exsr F06S02;
037000130423
037100130423       //?Invio
037200130423           OTHER;
037300130424         //?Eseguo i controlli
037400130424             exsr CtrS02;
037500130424             IF  ErrGenerico;
037600130424               leavesr;
037700130424             ENDIF;
037800130423
037900130423         ENDSL;
038000130423
038100130423       ENDSR;
038200130423
038300130423       //--------------------------------------------------------------
038400130423       //?Inizializzazione videata S02.
038500130423       //--------------------------------------------------------------
038600130423       BEGSR InzS02;
038700130423
038800130423       //?Pulisco il subfile
038900130424         SflDsp    = *on;
039000130424         SflDspCtl = *on;
039100130424         write OR53C02;
039200130424         SflDspCtl = *off;
039300130424         SflEnd    = *off;
039400130423
039500130423         clear V02rcd;
039600130423         clear V02csr;
039700130423         clear S02nrr;
039800130423         clear V02msg;
039900130423         ErrMessage  = *off;
040000130423         ErrGenerico = *off;
040100130423
040200130423         WindDspF    = IndDspF;
040300130423         reset WindDspFb;
040400130423         IndDspF     = WindDspF;
040500130423
040600130423         wEoF = *off;
040700130508
040800130508       //?Non c'è estero non sto neanche a caricare subito msg
040900130508         IF  wNoestero;
041000130508           wErrgrave = *on;
041100130508           ErrMessage  = *on;
041200130508           ErrGenerico = *on;
041300130508           V02msg      = wMsg(02);
041400130508           leavesr;
041500130508         ENDIF;
041600130424
041700130424       //?Preparo la stringa SQL
041800130424         exsr PrepSQL;
041900130424
042000130424       //?Dichiarazione cursore
042100130424         exec sql
042200130424           prepare S1   from :wSQL;
042300130424         exec sql
042400130424           declare ORM  cursor for S1;
042500130423
042600130423       //?Apertura del cursore
042700130423         exec sql
042800130424           open ORM;
042900130423
043000130423         IF sqlcode < 0;
043100130423           wEoF = *on;
043200130423         ENDIF;
043300130423
043400130423         DOW not wEoF;
043500130423           exec sql
043600130424             FETCH next from ORM into :ORMpor, :wconta;
043700130423
043800130423           IF sqlcod = 100 or sqlcod < 0;
043900130423             wEof = *on;
044000130423             leave;
044100130423           ENDIF;
044200130423
044300130423       //?Carico il subfile
044400130510           ProtCurOpz = *off;
044500130510           V02opz = '1';
044600130510           IF  wErrStp;
044700130510             V02opz = 'E';
044800130510             ProtCurOpz = *on;
044900130510           ENDIF;
045000130424           clear V02dpor;
045100130424           V02por  = ORMpor;
045200130424           V02norm = wconta;
045300130424           chain V02por AZORG01L;
045400130424           IF  %found(AZORG01L);
045500130424             V02dpor = ORGdes;
045600130424           ENDIF;
045700130423           S02nrr += 1;
045800130424           write OR53S02;
045900130423
046000130423         ENDDO;
046100130423
046200130423         exec sql
046300130424           close ORM;
046400130510
046500130510       //?Se c'è stato errore emetto il msg
046600130510         IF  wErrStp;
046700130510           ErrMessage  = *on;
046800130510           ErrGenerico = *on;
046900130510           V02msg      = wMsg(03);
047000130510         ENDIF;
047100130423
047200130423         SflEnd = *on;
047300130429         sav_S02nrr = S02nrr;
047400130423
047500130423       ENDSR;
047600130508
047700130508       //--------------------------------------------------------------
047800130508       //?Gestione tasto funzionale F3 da videata S02.
047900130508       //?F3=Fine
048000130508       //--------------------------------------------------------------
048100130508       BEGSR F03S02;
048200130508
048300130508         //?Chiusura del programma
048400130508         wFine = *on;
048500130508
048600130508       ENDSR;
048700130424
048800130424       //--------------------------------------------------------------
048900130424       //?Gestione tasto funzionale F06 da videata S02.
049000130424       //?F06=Conferma
049100130424       //--------------------------------------------------------------
049200130424       BEGSR F06S02;
049300130510
049400130510         wErrStp = *off;
049500130424
049600130424       //?Leggo il subfile e lancio la stampa
049700130424         S02nrr = 1;
049800130424         FOR S02nrr by 1 to sav_S02nrr;
049900130424           chain S02nrr OR53S02;
050000130424           IF  %found;
050100130424           //?Stampoo
050200130424             IF  V02opz = '1';
050300130424               exsr Stampa;
050400130424             ENDIF;
050500130424             V02csr = S02nrr;
050600130424             V02rcd = S02nrr;
050700130424             UPDATE OR53S02;
050800130424           ENDIF;
050900130424         ENDFOR;
051000130510
051100130510       //?Se ho qualche linea caricata sulla sk degli errori devo ricaricare il SBF con
051200130510       //?quelle linee
051300130510         IF  wErrStp;
051400130510           clear wSKpor;
051500130510           wSKpor = wSKporE;
051600130510           wInzS02 = *on;
051700130510         ENDIF;
051800130429
051900130508       //?Esco dal pgm
052000130510         IF  not wErrStp;
052100130510           exsr F03S02;
052200130510         ENDIF;
052300130424
052400130424       ENDSR;
052500130423
052600130424       //--------------------------------------------------------------
052700130424       //?Preparo la stringa SQL.
052800130424       //--------------------------------------------------------------
052900130424       BEGSR PrepSQL;
053000130424
053100130424       //?Seleziono ORM delle filiali estere in gestione all'utente
053200130424         wSQL = 'select ORMpor, count(*) from FNORM00F +
053300130424                 where ORMpor in(';
053400130424         yy = 0;
053500130424         xx = 1;
053600130424         FOR xx by 1 to %elem(wSKpor);
053700130424           IF wSKpor(xx) <> *zeros;
053800130424             IF yy > 0;
053900130424               wSQL += ', ';
054000130424             ELSE;
054100130424               yy = 1;
054200130424             ENDIF;
054300130424             wSQL += %editc(wSKpor(xx):'X');
054400130424           ENDIF;
054500130424         ENDFOR;
054600130508         wSQL += ') and ORMdst = 0 and ORMfao = 100';
054700130424
054800130429         wSQL += ' group by ORMpor order by ORMPOR';
054900130424
055000130424       ENDSR;
055100130424
055200130424       //--------------------------------------------------------------
055300130424       //?Controllo videata S03.
055400130424       //--------------------------------------------------------------
055500130424       BEGSR CtrS02;
055600130424
055700130424         WindDspF = IndDspF;
055800130424         reset WindDspFb;
055900130424         IndDspF  = WindDspF;
056000130424
056100130424       //?Controllo le scelte immesse nel subfile
056200130424         readc  OR53S02;
056300130424
056400130424         DOW  not %eof(FIOR53D);
056500130424           %subst(IndDspF : 52) = *off;
056600130424           SflNxtChg = *off;
056700130424           V02rcd = S02nrr;
056800130424
056900130424         //?Controllo se opzione valida
057000130424           SELECT;
057100130424             //?Nessuna opzione
057200130424             WHEN  V02opz  = *blank;
057300130510             //?Errore
057400130510             WHEN  V02opz  = 'E';
057500130510             //?Non è '1' di scelta
057600130424             WHEN  V02opz  <> '1';
057700130424               ErrMessage  = *on;
057800130424               ErrGenerico = *on;
057900130424               PosCurOPZ   = *on;
058000130508               V02msg = wMSG(01);
058100130424           ENDSL;
058200130424
058300130424         //?Aggiorno il subfile
058400130424           SELECT;
058500130424             WHEN  ErrMessage;
058600130424               SflNxtChg = *on;
058700130424               V02csr    = V02rcd;
058800130424             WHEN  ErrGenerico;
058900130424               V02csr = V02rcd;
059000130424               clear  V02opz;
059100130424             OTHER;
059200130424               V02csr = V02rcd;
059300130424           ENDSL;
059400130424
059500130424           UPDATE OR53S02;
059600130424
059700130424       //?Al primo errore esco dalla lettura del subfile
059800130424           IF  ErrMessage or ErrGenerico;
059900130424             leave;
060000130424           ENDIF;
060100130424
060200130424           readc OR53S02;
060300130424
060400130424         ENDDO;
060500130424
060600130424       ENDSR;
060700130424
060800130424       //--------------------------------------------------------------
060900130424       //?Stampa.
061000130424       //--------------------------------------------------------------
061100130424       BEGSR Stampa;
061200130424
061300130424         R16tla = 'L';
061400130424         R16por = V02por;
061500130508         R16ris = 'S';
061600130424         kpjbu = FIOR16DS;
061700130424
061800130424      /end-free
061900130424     c                   call      §5Ppgm
062000130424     c                   parm                    KPJBA
062100130424      /free
062200130510       //?Se torna un errore dal pgm di stampa è perchè non ho inviato tutto al partner
062300130510       //?per indicare l'errore uso il campo R16ass che per l'estero non serve
062400130521           IF  %subst(kpjbu:53:1) = 'E';
062500130521             wSkporE(s02nrr) = V02por;
062600130521             wErrStp = *on;
062700130521           ENDIF;
062800130424
062900130424       ENDSR;
063000130423
063100130423       //--------------------------------------------------------------
063200130423       //?Operazioni finali.
063300130423       //--------------------------------------------------------------
063400130423       BEGSR RoutEnd;
063500130423
063600130423         *inLR = *on;
063700130423         return;
063800130423
063900130423       ENDSR;
064000130423
064100130423      /end-free
064200130423       //--------------------------------------------------------------
064300130423       //?Schiere a tempo di compilazione.
064400130423       //--------------------------------------------------------------
064500130423
064600130423** - wMSG -------------------------------------------------------------------*
064700130508Opzione errata, immettere 1 per effettuare la scelta                          01
064800130508Utente non abilitato alla funzione.                                           02
064900130510Errori in trasmissione ORM. Avvisare il CED!!!                                03
