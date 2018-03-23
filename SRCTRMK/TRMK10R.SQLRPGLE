000100120703       //--------------------------------------------------------------
000200120703       //?TRMK10R * Interrogazione Storico Attività                    ?
000300120703       //--------------------------------------------------------------
000400120703
000500120703     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600120703     h dftactgrp(*no) actgrp(*caller)
000700120703     h alwnull(*inputonly)
000800120703
000900120703       //--------------------------------------------------------------
001000120703       //?Dichiarazione file.
001100120703       //--------------------------------------------------------------
001200120704
001300120704       // - Anagrafico potenziali
001400120704     fTNCPO01L  if   e           k disk
001500120703
001600120703       // - Video Gestione
001700120703     fTRMK10D   cf   e             workstn
001800120703     f                                     sfile(MK10S01 : S01nrr)
001900120703     f                                     indds(IndDspF)
002000120703     f                                     infds(InfDspF)
002100120703
002200120703       //--------------------------------------------------------------
002300120703       //?Definizione costanti.
002400120703       //--------------------------------------------------------------
002500120703
002600120703      // - Numero di record in ciascuna videata di subfile
002700120703     d c_SflPag        c                   const(15)
002800120703
002900120703      // - Numero massimo di record gestibili
003000120703     d c_MaxRec        c                   const(9999)
003100120703
003200120703       // - Tasti funzionali a video
003300120703     d c_F01           c                   const(x'31')
003400120703     d c_F02           c                   const(x'32')
003500120703     d c_F03           c                   const(x'33')
003600120703     d c_F05           c                   const(x'35')
003700120703     d c_F06           c                   const(x'36')
003800120703     d c_F07           c                   const(x'37')
003900120703     d c_F08           c                   const(x'38')
004000120703     d c_F09           c                   const(x'39')
004100120703     d c_F10           c                   const(x'3A')
004200120703     d c_F12           c                   const(x'3C')
004300120703     d c_F13           c                   const(x'B1')
004400120703     d c_F14           c                   const(x'B2')
004500120703     d c_F15           c                   const(x'B3')
004600120703     d c_F16           c                   const(x'B4')
004700120703     d c_F17           c                   const(x'B5')
004800120703     d c_F18           c                   const(x'B6')
004900120703     d c_F19           c                   const(x'B7')
005000120703     d c_F20           c                   const(x'B8')
005100120703     d c_F21           c                   const(x'B9')
005200120703     d c_F22           c                   const(x'BA')
005300120703     d c_F23           c                   const(x'BB')
005400120703     d c_F24           c                   const(x'BC')
005500120703     d c_Enter         c                   const(x'F1')
005600120703     d c_RollDown      c                   const(x'F4')
005700120703     d c_RollUp        c                   const(x'F5')
005800120703
005900120703       //--------------------------------------------------------------
006000120703       //?Definizione schiere.                                         ?
006100120703       //--------------------------------------------------------------
006200120703
006300120703       // - Messaggi di errore
006400120703     d wMsg            s             78    dim(10) ctdata perrcd(1)
006500120703
006600120703       //--------------------------------------------------------------
006700120703       //?Definizione aree dati.                                       ?
006800120703       //--------------------------------------------------------------
006900120703
007000120703       // - Dati utente
007100120703     d §AzUte        e ds                  extname(AZUTE00F)
007200120703     d                                     dtaara
007300120703     d §DatiUte      e ds                  extname(dDatiUte)
007400120703     d                                     dtaara
007500120703
007600120703       //--------------------------------------------------------------
007700120703       //?Definizione strutture dati.                                  ?
007800120703       //--------------------------------------------------------------
007900120703
008000120703       // - Status ds
008100120703     d Status         sds
008200120703     d  SDSpgm           *proc
008300120703     d  SDSusr               254    263
008400120703
008500120703       // - InfDS
008600120703     d InfDspF         ds
008700120703     d   dsp_aid             369    369a
008800120703     d   sfl_rrn             376    377i 0
008900120703     d   min_nrr             378    379i 0
009000120703     d   num_rcds            380    381i 0
009100120703
009200120703       // - Indicatori su DspF
009300120703     d IndDspF         ds                  inz
009400120703         // - Emissione messaggio di errore
009500120703     d   ErrMessage                    n   overlay(IndDspF : 28)
009600120703         // - Indicatori di gestione del subfile
009700120703     d   SflDsp                        n   overlay(IndDspF : 30)
009800120703     d   SflDspCtl                     n   overlay(IndDspF : 31)
009900120703     d   SflNxtChg                     n   overlay(IndDspF : 32)
010000120703     d   SflEnd                        n   overlay(IndDspF : 33)
010100120703         // - Posizionamento cursore & segnalazione errore
010200120703     d   PosCurOPZ                     n   overlay(IndDspF : 50)
010300120703         //   - Riemissione videata
010400120703     d   ErrGenerico                   n   overlay(IndDspF : 99)
010500120703
010600120703       // - Parametri ricevuti
010700120703     d KPJBA         e ds
010800120704     d TRMK10ds      e ds
010900120703
011000120703       // - Dati estratti via SQL
011100120703     d TICRMds       e ds                  extname(TICRM00F)  inz
011200120703
011300120704       // - Ricerca/Controllo tabelle
011400120704     d TIBS02ds      e ds                  inz
011500120704     d   T02cod      e                     inz('TTR')
011600120704     d   T02Mod      e                     inz('C')
011700120704
011800120703       // - Parametri per Reperimento dati utente
011900120703     d TIBS34ds      e ds                  inz
012000120703
012100120703       // - Gestione note clienti/potenziali/trattative
012200120703     d TRMK20ds      e ds                  inz
012300120703     d   IMK20tla    e                     inz('L')
012400120926     d   IMK20flm    e                     inz('S')
012500120704
012600120704       // - Tabella "TTR" = Tipi Trattativa
012700120704     d dTTR          e ds                  inz
012800120703
012900120703       //--------------------------------------------------------------
013000120703       //?Definizione variabili globali.                               ?
013100120703       //--------------------------------------------------------------
013200120703
013300120703       // - Flags booleani
013400120703     d wEoF            s               n   inz
013500120703     d wErr            s               n   inz
013600120704     d wFine           s               n   inz
013700120704     d wF12            s               n   inz
013800120703     d wInzS01         s               n   inz(*on)
013900120703
014000120703       // - Variabili per la gestione del video
014100120703     d wVideo          s              2    inz('S1')
014200120703     d wSflPag         s              3  0 inz
014300120703     d wPag            s              4  0 inz
014400120703     d wRig            s              3  0 inz
014500120703     d S01nrr          s                   like(C1RcdNbr) inz
014600120703     d SavS01csr       s                   like(C1RcdNbr) inz
014700120703
014800120703       // - Stringa SQL da eseguire
014900120703     d wSQL            s           2048    Varying        inz
015000120703
015100120703       // - Parametri per API QCAPCMD (Process Commands)
015200120703      /copy qsysinc/qrpglesrc,QCAPCMD
015300120703     d Qcmd            s            128    Varying        inz
015400120703
015500120703       // - Parametri gestione errori API.
015600120703      /copy qsysinc/qrpglesrc,QUSEC
015700120703
015800120703       // - Campi di comodo
015900120704     d wAlfa6          s              6    inz
016000120704     d wAlfa8          s              8    inz
016100120703
016200120703       //--------------------------------------------------------------
016300120703       //?Definizione Programmi.                             ?
016400120703       //--------------------------------------------------------------
016500120703
016600120703       //--------------------------------------------------------------
016700120703       //?Definizione Prototipi.                             ?
016800120703       //--------------------------------------------------------------
016900120704
017000120704       // - Ricerca/Controllo tabelle
017100120704      /copy gaitrasrc/srcProtoPR,TIBS02R
017200120703
017300120703       // - Reperimento dati utente
017400120703      /copy gaitrasrc/srcProtoPR,TIBS34R
017500120704
017600120704       // - Reperimento dati anagrafici cliente
017700120704      /copy gaitrasrc/srcProtoPI,tibs69r
017800120704      /copy gaitrasrc/srcProtoPR,tibs69r
017900120703
018000120703       // - Gestione note clienti/potenziali/trattative
018100120703      /copy gaitrasrc/srcProtoPR,TRMK20R
018200120703
018300120703       // - Api QCAPCMD (Process Commands)
018400120703      /copy gaitrasrc/srcProtoPR,QCAPCMD
018500120703
018600120703       //--------------------------------------------------------------
018700120703       //?Definizione key-list.                                        ?
018800120703       //--------------------------------------------------------------
018900120703
019000120703
019100120703       //--------------------------------------------------------------
019200120703       //?M A I N - L I N E                                            ?
019300120703       //--------------------------------------------------------------
019400120703
019500120703     c     *Entry        plist
019600120703     c                   parm                    KPJBA
019700120703     c                   parm                    TRMK10DS
019800120703
019900120703      /free
020000120703
020100120703       //?Operazioni iniziali?
020200120703       exsr RoutInz;
020300120703
020400120703       //?Gestione video   ?
020500120703       DOW  not wFine;
020600120703         SELECT;
020700120703         //?Subfile
020800120703           WHEN  wVideo = 'S1';
020900120703             exsr GesS01;
021000120703           OTHER;
021100120703             wFine = *on;
021200120703         ENDSL;
021300120703       ENDDO;
021400120703
021500120703       //?Operazioni finali?
021600120703       exsr RoutEnd;
021700120703
021800120703       //--------------------------------------------------------------
021900120703       //?Operazioni iniziali.                                         ?
022000120703       //--------------------------------------------------------------
022100120703       BEGSR  RoutInz;
022200120703
022300120703         *inLR = *on;
022400120703
022500120703       //?Impostazione opzioni per SQL?
022600120703         exec sql   set  option  DynUsrPrf = *Owner,
022700120703                                 CloSqlCsr = *EndMod;
022800120703
022900120703       //?Reperimento dati job
023000120703         exsr  DatiJob;
023100120703
023200120703       //?Impostazione nome programma a video?
023300120703         V1Tpgm = SDSpgm;
023400120703
023500120703       //?Controllo se OK il potenziale passato
023600120704         IF  IMK10cpo = 0;
023700120704           OMK10err = '1';
023800120704           OMK10err = wMsg(02);
023900120703           wFine = *on;
024000120703           leavesr;
024100120703         ENDIF;
024200120704         chain  IMK10cpo  TNCPO01L;
024300120703         IF  not %found(TNCPO01L);
024400120704           OMK10err = '1';
024500120704           OMK10err = wMsg(03);
024600120703           wFine = *on;
024700120703           leavesr;
024800120703         ENDIF;
024900120703
025000120703       ENDSR;
025100120703
025200120703       //--------------------------------------------------------------
025300120703       //?Reperimento Dati del job (Utente/Operativi).                 ?
025400120703       //--------------------------------------------------------------
025500120703       BEGSR  DatiJob;
025600120703
025700120703         in(e) §AzUte;
025800120703         IF  not %error;
025900120703           in(e) §DatiUte;
026000120703         ENDIF;
026100120703         IF  %error or RSut = *blank;
026200120703           tibs34r ( tibs34ds );
026300120703           in §AzUte;
026400120703           in §DatiUte;
026500120703         ENDIF;
026600120703
026700120703       ENDSR;
026800120703
026900120703       //--------------------------------------------------------------
027000120703       //?Gestione subfile S01                                         ?
027100120703       //--------------------------------------------------------------
027200120703       BEGSR  GesS01;
027300120703
027400120703       //?Inizializzazione subfile?
027500120703         IF  wInzS01;
027600120703           exsr  InzS01;
027700120703           wInzS01 = *off;
027800120703         ENDIF;
027900120703
028000120703       //?Emissione Testata e Piede con tasti funzionali abilitati?
028100120703         write  MK10T01;
028200120703         write  MK10P01;
028300120703
028400120703       //?Posizionamento cursore?
028500120703         IF  C1CsrRrn > *zero;
028600120703           C1RcdNbr = C1CsrRrn;
028700120703         ELSE;
028800120703           C1RcdNbr = 1;
028900120703           write  MK10S00;             // (no rec.)
029000120703         ENDIF;
029100120703
029200120703       //?Emissione videata?
029300120703         exfmt  MK10C01;
029400120703
029500120703         reset  ErrMessage;
029600120703         reset  ErrGenerico;
029700120703         clear  V1Dmsg;
029800120703
029900120703         SELECT;
030000120703
030100120703       //?F12=Ritorno?
030200120703           WHEN  dsp_aid = c_F12;
030300120703             wFine = *on;
030400120703
030500120703       //?Roll-Up?
030600120703           WHEN  dsp_aid = c_RollUp;
030700120703             exsr RollUpS01;
030800120703
030900120703       //?SubFile vuoto?
031000120703           WHEN  S01nrr = *zero;
031100120703
031200120703       //?Invio?
031300120703           OTHER;
031400120703         //?Gestione opzioni?
031500120703             exsr  OpzS01;
031600120703             IF  ErrGenerico;
031700120703               leavesr;
031800120703             ENDIF;
031900120703
032000120703         ENDSL;
032100120703
032200120703       ENDSR;
032300120703
032400120703       //--------------------------------------------------------------
032500120703       //?Inizializzazione subfile S01                                 ?
032600120703       //--------------------------------------------------------------
032700120703       BEGSR  InzS01;
032800120703
032900120703       //?Spegnimento degli indicatori di errore?
033000120703         %subst(IndDspF : 50) = *off;
033100120703
033200120703       //?Pulizia subfile?
033300120703         SflDsp    = *on;
033400120703         SflDspCtl = *on;
033500120703         write  MK10C01;
033600120703         SflDspCtl = *off;
033700120703         SflEnd    = *off;
033800120703
033900120703         clear  wSflPag ;
034000120703         clear  C1RcdNbr;
034100120703         clear  C1CsrRrn;
034200120703         clear  S01nrr;
034300120703         clear  V1Dmsg;
034400120703         ErrMessage  = *off;
034500120703         ErrGenerico = *off;
034600120703
034700120703       //?Impostazione campi di "testata"?
034800120704         C01cpo = IMK10cpo;
034900120704         C01rag = IMK10rag;
035000120703
035100120703       //?Caricamento della 1ª pagina di dati del subfile?
035200120703         exsr  OpenCursor;
035300120703         exsr  ReadCursor;
035400120703         exsr  RollUpS01;
035500120703
035600120703       //?Impaginazione subfile?
035700120703       //?-> forza l'impaginazione sul 1° rec. del subfile
035800120704         IF  S01nrr > *zero;
035900120703           C1RcdNbr  = 1;
036000120703           C1CsrRrn  = 1;
036100120704         ELSE;
036200120703           clear C1RcdNbr;
036300120704         ENDIF;
036400120703
036500120703       ENDSR;
036600120703
036700120703       //--------------------------------------------------------------
036800120703       //?Apertura cursore.                                            ?
036900120703       //--------------------------------------------------------------
037000120703       BEGSR  OpenCursor;
037100120703
037200120703       //?Dichiarazione cursore?
037300120703         exec sql
037400120703           DECLARE CUR1 cursor for
037500120703           SELECT * from TICRM00F
037600120704           WHERE  CRMcpo = :IMK10CPO
037700120703           ORDER BY CRMdaf desc
037800120703           FOR fetch only;
037900120703
038000120703       //?Apertura del cursore?
038100120704         exec sql
038200120704           open CUR1;
038300120703
038400120704         IF  SQLcode < *zero;
038500120703           wErr = *on;
038600120703           wEoF = *on;
038700120703           exsr  PrintErrSQL;
038800120704         ENDIF;
038900120703
039000120703       ENDSR;
039100120703
039200120703       //--------------------------------------------------------------
039300120703       //?Lettura cursore.                                             ?
039400120703       //--------------------------------------------------------------
039500120703       BEGSR  ReadCursor;
039600120703
039700120703         clear  TICRMds;
039800120703
039900120703         exec sql
040000120703           FETCH next   from CUR1 into :TICRMds;
040100120703
040200120703         SELECT;
040300120703           WHEN  SQLcode = 100;
040400120703             wEoF = *on;
040500120703           WHEN  SQLcode < *zero;
040600120703             wErr = *on;
040700120703             wEoF = *on;
040800120703             exsr  PrintErrSQL;
040900120703         ENDSL;
041000120703
041100120703       ENDSR;
041200120703
041300120703       //--------------------------------------------------------------
041400120703       //?Caricamento singola pagina nel subfile S01                   ?
041500120703       //--------------------------------------------------------------
041600120703       BEGSR  RollUpS01;
041700120703
041800120703         S01nrr    = (wSflPag  * c_SflPag);
041900120703         wSflPag  += 1;
042000120703         SflNxtChg = *off;
042100120703
042200120703       //?Ciclo di caricamento dati da TIATC00F?
042300120703         DOW  S01nrr    < (wSflPag  * c_SflPag)   and
042400120703              S01nrr    < c_MaxRec                and
042500120703             (Not wEoF   and   Not wErr);
042600120703
042700120703           clear  MK10S01;
042800120703
042900120703       //?Caricamento del singolo record nel subfile S01?
043000120704           SH1trc = CRMtrc;
043100120704           SH1mmd = CRMmmd;
043200120704           SH1dca = CRMdca;
043300120704           SH1dai = CRMdai;
043400120704           SH1daf = CRMdaf;
043500120704           SH1poi = CRMpoi;
043600120704           SH1coi = CRMcoi;
043700120704           SH1poe = CRMpoe;
043800120704           SH1coe = CRMcoe;
043900120705           SH1esi = CRMesi;
044000120706           SH1nrv = CRMnrv;
044100120706           SH1tpv = CRMtpv;
044200121008           SH1app = CRMapp;
044300120704
044400120704           SELECT;
044500120704             WHEN  CRMtrc = 'A';
044600120926               S01dca = 'TELEF - ' + CRMdca;
044700120704             WHEN  CRMtrc = 'T';
044800120926               exsr dec_TTR;
044900120926               S01dca = %trim(§TTRdes) + ' - ' + CRMdca;
045000120704             WHEN  CRMtrc = 'S';
045100121119               S01dca = 'A2 - ' + CRMdca;
045200120704           ENDSL;
045300120703
045400120704           clear wAlfa6;
045500120704           IF  CRMdaf <> *zeros;
045600120704             wAlfa6 = %subst(%editc(CRMdaf:'X'):7:2) +
045700120704                      %subst(%editc(CRMdaf:'X'):5:2) +
045800120704                      %subst(%editc(CRMdaf:'X'):3:2);
045900120704             S01daf = %dec(wAlfa6:6:0);
046000120704           ELSE;
046100120704             clear S01daf;
046200120704           ENDIF;
046300120704
046400120704           S01mmd = CRMmmd;
046500120926           //S01dca = CRMdca;
046600120704           S01ksc = CRMksc;
046700120703
046800120704       //?Scrittura record nel subfile
046900120704           S01nrr += 1;
047000120704           write  MK10S01;
047100120703
047200120704       //?Leggo record successivo
047300120704           exsr  ReadCursor;
047400120703
047500120704         ENDDO;
047600120703
047700120704       //?Visualizzazione del SFL (se ci sono dati)?
047800120703         SflDsp = (S01nrr <= *zero);
047900120703
048000120704       //?Attivazione del SFLEND?
048100120703         SflEnd   = (S01nrr >= c_MaxRec   or   wEoF   or   wErr);
048200120703
048300120704       //?Posizionamento cursore al 1° rcd della pagina?
048400120704         IF  S01nrr > *zero;
048500120703           wPag     = S01nrr / c_SflPag;
048600120703           wRig     = S01nrr - (c_SflPag * wPag);
048700120703           C1RcdNbr = wPag * c_SflPag;
048800120704           IF  wRig > *zero;
048900120703             C1RcdNbr += 1;
049000120704           ELSE;
049100120703             C1RcdNbr = C1RcdNbr - c_SflPag + 1;
049200120704           ENDIF;
049300120704         ELSE;
049400120703           clear  C1RcdNbr;
049500120704         ENDIF;
049600120703
049700120703         C1CsrRrn = C1RcdNbr;
049800120703
049900120703       ENDSR;
050000120703
050100120703       //--------------------------------------------------------------
050200120703       //?Gestione opzioni del subfile S01                             ?
050300120703       //--------------------------------------------------------------
050400120703       BEGSR  OpzS01;
050500120703
050600120703         clear  SavS01csr;
050700120703
050800120704       //?Ciclo di lettura subfile?
050900120704         readc  MK10S01;
051000120703
051100120704         DOW  not %eof(TRMK10D);
051200120703
051300120703           SflNxtChg = *off;
051400120703           %subst(IndDspF : 50) = *off;
051500120703           C1RcdNbr = S01nrr;
051600120703
051700120704           SELECT;
051800120703
051900120704           //?1-Dettaglio
052000120704             WHEN  S01opz = '1';
052100120704               exsr Dettaglio;
052200120703
052300120704           //?Nessuna Opzione
052400120704             WHEN  S01opz = *blank;
052500120703
052600120704           //?Opzione errata?
052700120704             OTHER;
052800120703               ErrMessage  = *on;
052900120703               ErrGenerico = *on;
053000120703               PosCurOPZ   = *on;
053100120703               V1Dmsg = wMsg(01);
053200120703
053300120704           ENDSL;
053400120703
053500120704           //?Memorizzazione nrr del sfl con la 1ª opzione per?
053600120704           //?riposizionarvici il cursore dopo il tasto "Invio"?
053700120704           IF  S01opz  <> *blank   and
053800120703              (SavS01csr = *zeros   or   SavS01csr < C1RcdNbr);
053900120703             SavS01csr   = C1RcdNbr;
054000120704           ENDIF;
054100120703
054200120704           //?Aggiornamento sfl?
054300120704           SELECT;
054400120704             WHEN  ErrMessage;
054500120703               SflNxtChg = *on;
054600120703               C1CsrRrn  = C1RcdNbr;
054700120704             WHEN  ErrGenerico;
054800120703               C1CsrRrn  = C1RcdNbr;
054900120704               clear  S01opz;
055000120704             OTHER;
055100120704               clear  S01opz;
055200120704           ENDSL;
055300120704           UPDATE  MK10S01;
055400120704           IF  ErrGenerico   or   ErrMessage;
055500120703             leave;
055600120704           ENDIF;
055700120703
055800120704           readc  MK10S01;
055900120703
056000120703         ENDDO;
056100120703
056200120704         //?Riposiziono il cursore sul record contenente la prima opz.?
056300120704         //?(se non sono stati rilevati errori)?
056400120704         IF  not ErrMessage   and   not ErrGenerico   and
056500120703             SavS01csr > *zero;
056600120703           C1CsrRrn = SavS01csr;
056700120704         ENDIF;
056800120703
056900120703       ENDSR;
057000120703
057100120703       //--------------------------------------------------------------
057200120704       //?Visualizza dettaglio storico attività.
057300120703       //--------------------------------------------------------------
057400120704       BEGSR  Dettaglio;
057500120703
057600120704       //?Visualizzo window diversa in base al tipo storico
057700120704         SELECT;
057800120704         //?Dettaglio Attività o Dettaglio Storico
057900120704           WHEN  SH1trc = 'A' or SH1trc = 'S';
058000120704             exsr GesW01;
058100120704         //?Dettaglio Trattativa
058200120704           WHEN  SH1trc = 'T';
058300120704             exsr GesW02;
058400120704         ENDSL;
058500120703
058600120703       ENDSR;
058700120704
058800120704       //--------------------------------------------------------------
058900120704       //?Gestione videata W01 - dettaglio Attività o Stato.              ?
059000120704       //--------------------------------------------------------------
059100120704       BEGSR  GesW01;
059200120704
059300120704         wF12 = *off;
059400120704
059500120704       //?Attività
059600120704         IF  SH1trc = 'A';
059700120704           W01trc  = 'ATTIVITÀ';
059800120705           W01ddai = 'Pianificata per il';
059900120704         ENDIF;
060000120704
060100120704       //?Stato
060200120704         IF  SH1trc = 'S';
060300120704           W01trc  = 'STATO';
060400120704           W01ddai = 'Immesso il';
060500120704         ENDIF;
060600120704
060700120704         W01dca = SH1dca;
060800120704
060900120704         clear wAlfa8;
061000120704         IF  SH1daf <> *zeros;
061100120704           wAlfa8 = %subst(%editc(SH1daf:'X'):7:2) +
061200120704                    %subst(%editc(SH1daf:'X'):5:2) +
061300120704                    %subst(%editc(SH1daf:'X'):1:4);
061400120704           W01daf = %dec(wAlfa8:8:0);
061500120704         ELSE;
061600120704           clear W01daf;
061700120704         ENDIF;
061800120704
061900120704         W01dca = SH1dca;
062000120704
062100120704         W01mmd = SH1mmd;
062200120704
062300120704         W01ksc = S01ksc;
062400120705         IF  W01ksc > 0;
062500120705           clear tibs69ds;
062600120705           I69kac = W01ksc;
062700120705           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
062800120705           W01rag = ACOrag;
062900120705         ELSE;
063000120705           clear W01rag;
063100120705         ENDIF;
063200120704
063300120704         clear wAlfa8;
063400120704         IF  SH1dai <> *zeros;
063500120704           wAlfa8 = %subst(%editc(SH1dai:'X'):7:2) +
063600120704                    %subst(%editc(SH1dai:'X'):5:2) +
063700120704                    %subst(%editc(SH1dai:'X'):1:4);
063800120704           W01dai = %dec(wAlfa8:8:0);
063900120704         ELSE;
064000120704           clear W01dai;
064100120704         ENDIF;
064200120704
064300120704       //?Emissione window
064400120704         DOW not wF12;
064500120704
064600120704           exfmt  MK10W01;
064700120704
064800120704         //?Esco dalla window
064900120704           IF  dsp_aid = c_F12;
065000120704             wF12 = *on;
065100120704             leavesr;
065200120704           ENDIF;
065300120704
065400120704         ENDDO;
065500120704
065600120704       ENDSR;
065700120704
065800120704       //--------------------------------------------------------------
065900120704       //?Gestione videata W02 - dettaglio Trattativa.                    ?
066000120704       //--------------------------------------------------------------
066100120704       BEGSR  GesW02;
066200120704
066300120704         wF12 = *off;
066400120704
066500120706         W02nrv = SH1nrv;
066600120704
066700120704         clear wAlfa8;
066800120704         IF  SH1dai <> *zeros;
066900120704           wAlfa8 = %subst(%editc(SH1dai:'X'):7:2) +
067000120704                    %subst(%editc(SH1dai:'X'):5:2) +
067100120704                    %subst(%editc(SH1dai:'X'):1:4);
067200120704           W02dai = %dec(wAlfa8:8:0);
067300120704         ELSE;
067400120704           clear W02dai;
067500120704         ENDIF;
067600120704
067700120926         exsr dec_TTR;
067800120704         W02tpv = §TTRdes;
067900120704
068000120704         clear wAlfa8;
068100120704         IF  SH1daf <> *zeros;
068200120704           wAlfa8 = %subst(%editc(SH1daf:'X'):7:2) +
068300120704                    %subst(%editc(SH1daf:'X'):5:2) +
068400120704                    %subst(%editc(SH1daf:'X'):1:4);
068500120704           W02daf = %dec(wAlfa8:8:0);
068600120704         ELSE;
068700120704           clear W02daf;
068800120704         ENDIF;
068900120704
069000120704         W02dca = SH1dca;
069100120705         IF  SH1esi = 'N';
069200120705           W02dca = %trim(W02dca) + ' - Negativo';
069300120705         ENDIF;
069400120705         IF  SH1esi = 'P';
069500120705           W02dca = %trim(W02dca) + ' - Positivo';
069600120705         ENDIF;
069700120704
069800120704         W02mmd = SH1mmd;
069900120704
070000120704         W02ksc = S01ksc;
070100120705         IF  W02ksc > 0;
070200120705           clear tibs69ds;
070300120705           I69kac = W02ksc;
070400120705           TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
070500120705           W02rag = ACOrag;
070600120705         ELSE;
070700120705           clear W02rag;
070800120705         ENDIF;
070900120704
071000120704         IF  SH1poi = 'S';
071100120704           W02poi = 'SI';
071200120704         ELSE;
071300120704           W02poi = 'NO';
071400120704         ENDIF;
071500120704         IF  SH1coi = 'S';
071600120704           W02coi = 'SI';
071700120704         ELSE;
071800120704           W02Coi = 'NO';
071900120704         ENDIF;
072000120704
072100120704         IF  SH1poe = 'S';
072200120704           W02poe = 'SI';
072300120704         ELSE;
072400120704           W02poe = 'NO';
072500120704         ENDIF;
072600120704         IF  SH1coe = 'S';
072700120704           W02coe = 'SI';
072800120704         ELSE;
072900120704           W02Coe = 'NO';
073000120704         ENDIF;
073100121008
073200121008         W02app = SH1app;
073300120704
073400120704       //?Emissionewindow
073500120704         DOW not wF12;
073600120704
073700120704           exfmt  MK10W02;
073800120704
073900120704         //?Interrogazione note
074000120704           IF  dsp_aid = c_F08;
074100120704             exsr callTRMK20;
074200120704           ENDIF;
074300120704
074400120704         //?Esco dalla window
074500120704           IF  dsp_aid = c_F12;
074600120704             wF12 = *on;
074700120704             leavesr;
074800120704           ENDIF;
074900120704
075000120704         ENDDO;
075100120704
075200120704       ENDSR;
075300120926
075400120926       //--------------------------------------------------------------
075500120926       //?Richiamo pgm interrogazione note
075600120926       //--------------------------------------------------------------
075700120926       BEGSR  dec_TTR;
075800120926
075900120926         clear dTTR;
076000120926         reset TIBS02ds;
076100120926         T02ke1 = SH1tpv;
076200120926         T02sif = knsif;
076300120926         TNTBE_RicercaControllo ( kpjba : tibs02ds );
076400120926         IF  T02err = *blank;
076500120926           dTTR = T02uni;
076600120926         ELSE;
076700120926           §TTRdes = *all'? ';
076800120926         ENDIF;
076900120926
077000120926       ENDSR;
077100120704
077200120704       //--------------------------------------------------------------
077300120704       //?Richiamo pgm interrogazione note
077400120704       //--------------------------------------------------------------
077500120704       BEGSR  CallTRMK20;
077600120704
077700120704         reset  TRMK20ds;
077800120704         IMK20cpo = C01cpo;
077900120704         IMK20nrv = W02nrv;
078000120704
078100120704         trmk20r ( KPJBA : TRMK20ds );
078200120704
078300120704       ENDSR;
078400120703
078500120703       //--------------------------------------------------------------
078600120703       //?Stampa segnalazione dell'errore rilevato via SQL             ?
078700120703       //--------------------------------------------------------------
078800120703       BEGSR  PrintErrSQL;
078900120703
079000120703       //?Stampa del Dump?
079100120703         Dump(A);
079200120703
079300120703       //?Stampa del Job-Log?
079400120703         Qcmd = 'DSPJOBLOG job(*) output(*print)';
079500120703         exsr  ExecCmd;
079600120703
079700120703       //?Chiusura del programma?
079800120703         exsr  RoutEnd;
079900120703
080000120703       ENDSR;
080100120703
080200120703       //--------------------------------------------------------------
080300120703       //?Esecuzione del comando (già impostato).                      ?
080400120703       //--------------------------------------------------------------
080500120703       BEGSR  ExecCmd;
080600120703
080700120703         clear Qcap0100;
080800120703         Qcabcsdh = *off;
080900120703         Qcapa    = *off;
081000120703         Qcacmdss = *off;
081100120703         Qcaerved = *allX'00';
081200120703
081300120703         clear Qusec;
081400120703         Qusbprv  = %size(Qusec);
081500120703
081600120703         ProcessCommands ( Qcmd : %size(Qcmd) : Qcap0100 :
081700120703                           %size(Qcap0100) : 'CPOP0100' : *OMIT :
081800120703                           0 : 0 : Qusec);
081900120703
082000120703         IF  Qusei <> *blank;
082100120703           exsr  MsgErr;
082200120703         ENDIF;
082300120703
082400120703       ENDSR;
082500120703
082600120703       //--------------------------------------------------------------
082700120703       //?Invio messaggio per segnalazione errori.                     ?
082800120703       //--------------------------------------------------------------
082900120703       BEGSR  MsgErr;
083000120703
083100120703
083200120703       ENDSR;
083300120703
083400120703       //--------------------------------------------------------------
083500120703       //?Operazioni finali.                                           ?
083600120703       //--------------------------------------------------------------
083700120703       BEGSR  RoutEnd;
083800120703
083900120703       //?Chiusura cursore SQL?
084000120704         exec sql
084100120704           close CUR1;
084200120703
084300120703       //?Chiusura pgm?
084400120703         kpjbu = TRMK10ds;
084500120703         return;
084600120703
084700120703       ENDSR;
084800120703
084900120703      /end-free
085000120703
085100120703       //--------------------------------------------------------------
085200120703       //?Schiere a tempo di compilazione.                             ?
085300120703       //--------------------------------------------------------------
085400120703
085500120703** - wMsg -------------------------------------------------------------------*
085600120703Opzione errata                                                                 1
085700120703Codice potenziale non passato                                                  2
085800120703Codice potenziale errato                                                       3
