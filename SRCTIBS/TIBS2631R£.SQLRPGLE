000100060510      //********************************************************************************************
000200060330      //
000300060428      // Questo programma gestisce la testata del modello.
000400060330      //
000500060330      //********************************************************************************************
000600060509     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700060414     H BNDDIR('PJXBND':'QC2LE':'PJXBND')
000800060207     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000900060127
001000060330      //********************************************************************************************
001100060330      //
001200060330      // Definizione files.
001300060330      //
001400060330      //********************************************************************************************
001500060428     Fycow0431d CF   E             WORKSTN
001600060320     F                                     USROPN
001700060130     F                                     INFDS(infDspF)
001800060127     F                                     INDDS(indDspF)
001900060428     Fynmrt00f  UF A E             DISK
002000060428     F                                     USROPN
002100060428     F                                     COMMIT
002200060505     F                                     INFDS(infYnmrt)
002300060127
002400060330      //********************************************************************************************
002500060330      //
002600060330      // Definizione costanti.
002700060330      //
002800060330      //********************************************************************************************
002900060127     D F3              C                   X'33'
003000060428     D F4              C                   X'34'
003100060127     D F5              C                   X'35'
003200060428     D F6              C                   X'36'
003300060428     D F8              C                   X'38'
003400060427     D F10             C                   X'3A'
003500060127     D F12             C                   X'3C'
003600060127     D Enter           C                   X'F1'
003700060428     D RollDown        C                   X'F4'
003800060428     D PageUp          C                   X'F4'
003900060428     D RollUp          C                   X'F5'
004000060428     D PageDown        C                   X'F5'
004100060403     D Normal          C                   X'20'
004200060202     D ReverseImage    C                   X'21'
004300060404     D HighIntensity   C                   X'22'
004400060314     D Blink           C                   X'28'
004500060428     D ProtectNormal   C                   X'A0'
004600060428     D RecordAlreadyLocked...
004700060428     D                 C                   01218
004800060510     D Civilistica     C                   'CG'
004900060510     D Definitiva      C                   'D'
005000060404
005100060330      //********************************************************************************************
005200060330      //
005300060330      // Definizione delle procedure esterne usate.
005400060330      //
005500060330      //********************************************************************************************
005600060222     D xSoc            PR
005700060222     D                                     EXTPROC('XSOC')
005800060222     D  tipXsc                        6A
005900060222     D                                     CONST
006000060222     D  socXsc
006100060222     D                                     LIKE(pjzSocieta)
006200060222     D  cdsXsc                        9P 0
006300060222     D                                     CONST
006400060222     D  modXsc                        3A
006500060222     D                                     CONST
006600060222     D  rtnXsc                        1A
006700060222     D  xSoc001ds
006800060222     D                                     LIKEDS(xSoc001ds)
006900060222     D  kpjba
007000060222     D                                     LIKEDS(kpjba)
007100060331     D tibs34r         PR
007200060131     D                                     EXTPGM('TIBS34R')
007300060131     D  tibs34ds
007400060131     D                                     LIKEDS(tibs34ds)
007500060426     D atoi            PR            10I 0
007600060222     D                                     EXTPROC('atoi')
007700060222     D  char                           *
007800060222     D                                     VALUE
007900060222     D                                     OPTIONS(*STRING)
008000060427     D ycow0432r       PR
008100060427     D                                     EXTPGM('YCOW0432R')
008200060427     D  kpjba
008300060427     D                                     LIKEDS(kpjba)
008400060505     D DisplayLongText...
008500060505     D                 PR
008600060505     D                                     EXTPGM('QUILNGTX')
008700060505     D  TextString...
008800060505     D                              256A
008900060505     D                                     CONST
009000060505     D                                     OPTIONS(*VARSIZE)
009100060505     D  LengthOfTextString...
009200060505     D                               10I 0
009300060505     D                                     CONST
009400060505     D  MessageID...
009500060505     D                                7A
009600060505     D                                     CONST
009700060505     D  QualifiedMessageFileName...
009800060505     D                               20A
009900060505     D                                     CONST
010000060505     D  ErrorCode...
010100060505     D                                     LIKE(qusec)
010200060505     D                                     OPTIONS(*VARSIZE)
010300060505
010400060330      //********************************************************************************************
010500060330      //
010600060330      // Definizione strutture dati.
010700060330      //
010800060330      //********************************************************************************************
010900060330     D psds           SDS
011000060131     D  pgmName          *PROC
011100060130     D infDspF         DS
011200060222     D  dsp_aid              369    369A                                        AID byte
011300060206     D indDspF         DS
011400060428     D  F4Attivo               4      4N
011500060428     D  F6Attivo               6      6N
011600060428     D  F8Attivo               8      8N
011700060428     D  F10Attivo             10     10N
011800060428     D  tuttoProtetto         25     25N
011900060510     D  errMsgModello         30     30N
012000060427     D  errGenerico           99     99N
012100060505     D infYnmrt        DS
012200060505     D  db_rrn               397    400I 0                                      Relative-rcd-num
012300060404     D kpjba         E DS
012400060414     D xsoc001ds     E DS
012500060414     D                                     INZ
012600060414     D cndizion      E DS
012700060130     D                                     BASED(nullPtr)
012800060130     D                                     PREFIX(diz)
012900060222     D andiz00f      E DS
013000060222     D                                     BASED(nullPtr)
013100060222     D                                     PREFIX(pjz)
013200060427     D ycow0431s     E DS
013300060427     D                                     INZ
013400060427     D                                     QUALIFIED
013500060505     D ycow0432s     E DS
013600060505     D                                     INZ
013700060505     D                                     QUALIFIED
013800060414     D tibs34ds      E DS
013900060414     D                                     INZ
014000060131
014100060330      //********************************************************************************************
014200060330      //
014300060330      // Definizione variabili.
014400060330      //
014500060330      //********************************************************************************************
014600060505     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
014700060505     D/COPY QSYSINC/QRPGLESRC,QUSEC
014800060505     D nullPtr         S               *
014900060130     D opCode          S             10
015000060510     D                                     INZ('INZR0')
015100060414     D rtnXsc          S              1A
015200060428     D p               S              5I 0
015300060428     D l               S              5I 0
015400060228     D esito           S             10I 0
015500060510     D count           S             10I 0
015600060512     D tcmKcm          S              9P 0
015700060427     D fxx             S
015800060427     D                                     LIKE(dsp_aid)
015900060427     D                                     DIM(4)
016000060427     D                                     ASCEND
016100060505     D  TextString...
016200060505     D                 S            256A
016300060505     D  LengthOfTextString...
016400060505     D                 S             10I 0
016500060505     D                                     INZ(%SIZE(textString))
016600060505     D  MessageID...
016700060505     D                 S              7A
016800060505     D                                     INZ('BAR0019')
016900060505     D  QualifiedMessageFileName...
017000060505     D                 S             20A
017100060505     D                                     INZ('YBARMSG   *LIBL')
017200060505
017300060330      //********************************************************************************************
017400060330      //
017500060330      // Definizione aree dati.
017600060330      //
017700060330      //********************************************************************************************
017800060201     D §azute        E DS
017900060201     D                                     EXTNAME(azute00f)
018000060201     D                                     DTAARA
018100060201     D §DatiUte      E DS
018200060201     D                                     EXTNAME(dDatiUte)
018300060201     D                                     DTAARA
018400060403
018500060330      //********************************************************************************************
018600060330      //
018700060330      // Definizione parametri procedura.
018800060330      //
018900060330      //********************************************************************************************
019000060131     C     *ENTRY        PLIST
019100060131     C                   PARM                    kpjba
019200060127
019300060127      /FREE
019400060130
019500060131       EXSR inzProc;
019600060130
019700060130       DOU opCode = 'END';
019800060130         SELECT;
019900060510           // Inizio transazione.
020000060510           WHEN opCode = 'INZR0';
020100060510             opCode = 'INZT01';
020200060510             EXSR inzR0;
020300060428           WHEN opCode = 'INZT01';
020400060510             opCode = 'INZF01';
020500060428             EXSR inzT01;
020600060510           WHEN opCode = 'INZF01';
020700060510             opCode = 'SETR01';
020800060510             EXSR inzF01;
020900060510           // Finestra 1 di 2.
021000060510           WHEN opCode = 'SETR01';
021100060510             opCode = 'SETT01R01';
021200060510             EXSR setR0;
021300060510           WHEN opCode = 'SETT01R01';
021400060510             opCode = 'SETF01R01';
021500060428             EXSR setT01;
021600060510           WHEN opCode = 'SETF01R01';
021700060510             opCode = 'PUTT01R01';
021800060510             EXSR setF01R01;
021900060510           WHEN opCode = 'PUTT01R01';
022000060510             opCode = 'PUTF01R01';
022100060510             EXSR putT01;
022200060510           WHEN opCode = 'PUTF01R01';
022300060510             opCode = 'PUTR01';
022400060510             EXSR putF01;
022500060510           WHEN opCode = 'PUTR01';
022600060510             EXSR putR01;
022700060510           WHEN opCode = 'CHKR01';
022800060510             opCode = 'PUTR01';
022900060510             EXSR chkR0;
023000060510           WHEN opCode = 'RIGHE1';
023100060510             opCode = 'SETR01';
023200060510             EXSR aggiornaTestata;
023300060510             EXSR righeModello;
023400060510           WHEN opCode = 'RICHIESTA1';
023500060510             opCode = 'PUTR01';
023600060510             EXSR richiesta;
023700060510           // Finestra 2 di 2.
023800060510           WHEN opCode = 'SETR02';
023900060510             opCode = 'SETT01R02';
024000060510             EXSR setR0;
024100060510           WHEN opCode = 'SETT01R02';
024200060510             opCode = 'SETF01R02';
024300060510             EXSR setT01;
024400060510           WHEN opCode = 'SETF01R02';
024500060510             opCode = 'PUTT01R02';
024600060510             EXSR setF01R02;
024700060510           WHEN opCode = 'PUTT01R02';
024800060510             opCode = 'PUTF01R02';
024900060510             EXSR putT01;
025000060510           WHEN opCode = 'PUTF01R02';
025100060510             opCode = 'PUTR02';
025200060510             EXSR putF01;
025300060510           WHEN opCode = 'PUTR02';
025400060510             EXSR putR02;
025500060505           WHEN opCode = 'CHKR02';
025600060505             opCode = 'PUTR02';
025700060505             EXSR chkR0;
025800060505           WHEN opCode = 'RIGHE2';
025900060510             opCode = 'SETR02';
026000060505             EXSR aggiornaTestata;
026100060505             EXSR righeModello;
026200060505           WHEN opCode = 'RICHIESTA2';
026300060505             opCode = 'PUTR02';
026400060505             EXSR richiesta;
026500060510           // Fine transazione.
026600060505           WHEN opCode = 'UPDDB';
026700060505             opCode = 'COMMIT';
026800060505             EXSR aggiornaTestata;
026900060505           WHEN opCode = 'ERRORE';
027000060505             opCode = 'ROLLBACK';
027100060505             EXSR errore;
027200060505           WHEN opCode = 'COMMIT';
027300060505             opCode = 'END';
027400060505             COMMIT;
027500060505           WHEN opCode = 'ROLLBACK';
027600060505             opCode = 'END';
027700060505             ROLBK;
027800060505           WHEN opCode = 'END';
027900060505             LEAVE;
028000060130           OTHER;
028100060505             TextString = 'Codice operativo ' + %TRIMR(opCode)
028200060505             + ' sconosciuto.';
028300060505             opCode = 'ERRORE';
028400060130         ENDSL;
028500060130       ENDDO;
028600060130
028700060130       EXSR uscita;
028800060323
028900060323       //*******************************************************************************************
029000060323       //
029100060323       // Operazioni iniziali da eseguire una tantum.
029200060323       //
029300060323       //*******************************************************************************************
029400060323       BEGSR *INZSR;
029500060330
029600060509      /END-FREE
029700060509     C/EXEC SQL
029800060509     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER, COMMIT = *CHG
029900060509     C/END-EXEC
030000060509      /FREE
030100060509
030200060323         IN(E) §azute;
030300060323         IF NOT %ERROR;
030400060323           IN(E) §DatiUte;
030500060323         ENDIF;
030600060323         IF %ERROR OR rsut = ' ';
030700060323           tibs34r(tibs34ds);
030800060323           IN §azute;
030900060323           IN §DatiUte;
031000060323         ENDIF;
031100060407
031200060407       ENDSR;
031300060131
031400060131       //*******************************************************************************************
031500060131       //
031600060428       // Operazioni iniziali da eseguire ad ogni chiamata.
031700060131       //
031800060131       //*******************************************************************************************
031900060131       BEGSR inzProc;
032000060330
032100060330         RESET opCode;
032200060131
032300060131         EXSR chkParm;
032400060222
032500060222         CLEAR kpjbu;
032600060405         CLEAR xSoc001ds;
032700060405         xSoc('SOC001':xscSoc:0:*BLANK:rtnXsc:xsoc001ds:kpjba);
032800060222         IF rtnXsc = *ON;
032900060428           ycow0431s.rtnCode = 'ERRXSOC';
033000060330           opCode = 'ERRORE';
033100060330           LEAVESR;
033200060222         ENDIF;
033300060428
033400060428         IF NOT %OPEN(ycow0431d);
033500060428           OPEN ycow0431d;
033600060428         ENDIF;
033700060428
033800060428         IF NOT %OPEN(ynmrt00f);
033900060428           OPEN ynmrt00f;
034000060428         ENDIF;
034100060428
034200060131       ENDSR;
034300060131
034400060131       //*******************************************************************************************
034500060131       //
034600060131       // Controllo dei parametri ricevuti.
034700060131       //
034800060131       //*******************************************************************************************
034900060131       BEGSR chkParm;
035000060414
035100060428         ycow0431s = kpjbu;
035200060428         CLEAR ycow0431s.rtnCmd;
035300060428         CLEAR ycow0431s.rtnCode;
035400060428         CLEAR ycow0431s.rtnEsito;
035500060428         IF ycow0431s.opCode = 'NUOVO';
035600060428           CLEAR ycow0431s.modello;
035700060428           CLEAR ycow0431s.rrnYnmrt;
035800060428         ENDIF;
035900060426
036000060131       ENDSR;
036100060428
036200060428       //*******************************************************************************************
036300060428       //
036400060428       // Reperisco testata modello.
036500060428       //
036600060428       //*******************************************************************************************
036700060428       BEGSR getTestataModello;
036800060428
036900060428         SELECT;
037000060428           WHEN ycow0431s.opCode = 'COPIA' OR ycow0431s.opCode = 'VISUALIZZA';
037100060428             CHAIN(EN) ycow0431s.rrnYnmrt ynmrt00f;
037200060428           WHEN ycow0431s.opCode = 'NUOVO';
037300060428             CLEAR ynmrt000;
037400060428           OTHER;
037500060428             CHAIN(E) ycow0431s.rrnYnmrt ynmrt00f;
037600060428             IF %ERROR;
037700060428               ycow0431s.rtnCode = 'YNMRT00F';
037800060428               ycow0431s.rtnEsito = %STATUS;
037900060428               SELECT;
038000060428                 WHEN %STATUS = RecordAlreadyLocked;
038100060428                   opCode = 'END';
038200060428                 OTHER;
038300060428                   opCode = 'ERRORE';
038400060428               ENDSL;
038500060428             ENDIF;
038600060428         ENDSL;
038700060428
038800060428       ENDSR;
038900060428
039000060428       //*******************************************************************************************
039100060428       //
039200060428       // Inizializzo testata.
039300060428       //
039400060428       //*******************************************************************************************
039500060428       BEGSR inzT01;
039600060428
039700060510         RESET t01PgmName;
039800060510         RESET t01SottoTi;
039900060428
040000060428       ENDSR;
040100060508
040200060508       //*******************************************************************************************
040300060508       //
040400060508       // Inizializzo piede.
040500060508       //
040600060508       //*******************************************************************************************
040700060508       BEGSR inzF01;
040800060508
040900060510         RESET f01;
041000060508
041100060508       ENDSR;
041200060508
041300060508       //*******************************************************************************************
041400060508       //
041500060508       // Inizializzo 1a e 2a finestra testata modello.
041600060508       //
041700060508       //*******************************************************************************************
041800060508       BEGSR inzR0;
041900060508
042000060508         IF ycow0431s.opCode = 'NUOVO';
042100060510           RESET r01;
042200060510           RESET r02;
042300060508         ENDIF;
042400060508
042500060508       ENDSR;
042600060428
042700060428       //*******************************************************************************************
042800060428       //
042900060428       // Carico dati testata.
043000060428       //
043100060428       //*******************************************************************************************
043200060428       BEGSR setT01;
043300060428
043400060428         t01PgmName = pgmName;
043500060428
043600060428         // Importo il sottotitolo a seconda del codice operativo ricevuto.
043700060428         SELECT;
043800060428           WHEN ycow0431s.opCode = 'NUOVO';
043900060509             t01SottoTi = 'Immissione modello';
044000060428           WHEN ycow0431s.opCode = 'MODIFICA';
044100060509             t01SottoTi = 'Modifica modello';
044200060428           WHEN ycow0431s.opCode = 'COPIA';
044300060509             t01SottoTi = 'Copia modello';
044400060428           WHEN ycow0431s.opCode = 'ANNULLA';
044500060428             IF mrtAnnulla = *ON;
044600060509               t01SottoTi = 'Ripristino modello';
044700060428             ELSE;
044800060509               t01SottoTi = 'Annulla modello';
044900060428             ENDIF;
045000060428           WHEN ycow0431s.opCode = 'VISUALIZZA';
045100060509             t01SottoTi = 'Visualizzazione modello';
045200060511           WHEN ycow0431s.opCode = 'CANCELLA';
045300060511             t01SottoTi = 'Cancella modello';
045400060428           OTHER;
045500060428             t01SottoTi = ycow0431s.opCode;
045600060428         ENDSL;
045700060428
045800060428         // Centro il sottotitolo.
045900060428         l = %LEN(%TRIMR(t01SottoTi));
046000060428         p = ((%SIZE(t01SottoTi) - l) / 2) + 1;
046100060428         %SUBST(t01SottoTi:p) = %TRIMR(t01SottoTi);
046200060428         %SUBST(t01SottoTi:1:p-1) = *BLANK;
046300060428
046400060428       ENDSR;
046500060508
046600060508       //*******************************************************************************************
046700060508       //
046800060510       // Carico dati piede per finestra 1.
046900060508       //
047000060508       //*******************************************************************************************
047100060510       BEGSR setF01R01;
047200060508
047300060508         // Abilito i tasti funzionali.
047400060508         SELECT;
047500060508           WHEN ycow0431s.opCode = 'NUOVO';
047600060510             f01Tasti = 'F3=Fine  F4=Richiesta  F5=Ricarica  +
047700060508             F12=Ritorno';
047800060508             F4Attivo = *ON;
047900060510             F6Attivo = *OFF;
048000060508             F8Attivo = *OFF;
048100060508             F10Attivo = *OFF;
048200060508           WHEN ycow0431s.opCode = 'MODIFICA';
048300060508             f01Tasti = 'F3=Fine  F4=Richiesta  F5=Ricarica  F6=Conferma  +
048400060508             F8=Successivo  F10=Righe';
048500060508             F4Attivo = *ON;
048600060508             F6Attivo = *ON;
048700060508             F8Attivo = *ON;
048800060508             F10Attivo = *ON;
048900060508           WHEN ycow0431s.opCode = 'COPIA';
049000060508             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
049100060508             F8=Successivo  F12=Ritorno';
049200060508             F4Attivo = *OFF;
049300060508             F6Attivo = *ON;
049400060508             F8Attivo = *ON;
049500060508             F10Attivo = *OFF;
049600060511           WHEN ycow0431s.opCode = 'ANNULLA' OR ycow0431s.opCode = 'CANCELLA';
049700060508             f01Tasti = 'F3=Fine  F6=Conferma  F8=Successivo  +
049800060508             F12=Ritorno';
049900060508             F4Attivo = *OFF;
050000060508             F6Attivo = *ON;
050100060508             F8Attivo = *ON;
050200060508             F10Attivo = *OFF;
050300060508           WHEN ycow0431s.opCode = 'VISUALIZZA';
050400060508             f01Tasti = 'F3=Fine  F8=Successivo  F10=Righe  +
050500060508             F12=Ritorno';
050600060508             F4Attivo = *OFF;
050700060508             F6Attivo = *OFF;
050800060508             F8Attivo = *ON;
050900060508             F10Attivo = *ON;
051000060508           OTHER;
051100060508             F4Attivo = *OFF;
051200060508             F6Attivo = *OFF;
051300060508             F8Attivo = *OFF;
051400060508             F10Attivo = *OFF;
051500060508         ENDSL;
051600060508
051700060508       ENDSR;
051800060510
051900060510       //*******************************************************************************************
052000060510       //
052100060510       // Carico dati piede per finestra 2.
052200060510       //
052300060510       //*******************************************************************************************
052400060510       BEGSR setF01R02;
052500060510
052600060510         // Abilito i tasti funzionali.
052700060510         SELECT;
052800060510           WHEN ycow0431s.opCode = 'NUOVO';
052900060510             f01Tasti = 'F3=Fine  F4=Richiesta  F5=Ricarica  F6=Conferma  +
053000060510             F12=Ritorno';
053100060510             F4Attivo = *ON;
053200060510             F6Attivo = *ON;
053300060510             F8Attivo = *OFF;
053400060510             F10Attivo = *OFF;
053500060510           WHEN ycow0431s.opCode = 'MODIFICA';
053600060510             f01Tasti = 'F3=Fine  F4=Richiesta  F5=Ricarica  F6=Conferma  +
053700060510             F8=Successivo  F10=Righe';
053800060510             F4Attivo = *ON;
053900060510             F6Attivo = *ON;
054000060510             F8Attivo = *ON;
054100060510             F10Attivo = *ON;
054200060510           WHEN ycow0431s.opCode = 'COPIA';
054300060510             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
054400060510             F8=Successivo  F12=Ritorno';
054500060510             F4Attivo = *OFF;
054600060510             F6Attivo = *ON;
054700060510             F8Attivo = *ON;
054800060510             F10Attivo = *OFF;
054900060511           WHEN ycow0431s.opCode = 'ANNULLA' OR ycow0431s.opCode = 'CANCELLA';
055000060510             f01Tasti = 'F3=Fine  F6=Conferma  F8=Successivo  +
055100060510             F12=Ritorno';
055200060510             F4Attivo = *OFF;
055300060510             F6Attivo = *ON;
055400060510             F8Attivo = *ON;
055500060510             F10Attivo = *OFF;
055600060510           WHEN ycow0431s.opCode = 'VISUALIZZA';
055700060510             f01Tasti = 'F3=Fine  F8=Successivo  F10=Righe  +
055800060510             F12=Ritorno';
055900060510             F4Attivo = *OFF;
056000060510             F6Attivo = *OFF;
056100060510             F8Attivo = *ON;
056200060510             F10Attivo = *ON;
056300060510           OTHER;
056400060510             F4Attivo = *OFF;
056500060510             F6Attivo = *OFF;
056600060510             F8Attivo = *OFF;
056700060510             F10Attivo = *OFF;
056800060510         ENDSL;
056900060510
057000060510       ENDSR;
057100060508
057200060508       //*******************************************************************************************
057300060508       //
057400060508       // Imposto dati 1a finestra testata modello.
057500060508       //
057600060508       //*******************************************************************************************
057700060508       BEGSR setR0;
057800060508
057900060508         EXSR getTestataModello;
058000060508
058100060508         // Imposto attributi campi.
058200060508         SELECT;
058300060508           WHEN ycow0431s.opCode = 'NUOVO';
058400060508             tuttoProtetto = *OFF;
058500060509             RESET p01Modello;
058600060509             RESET p01Descriz;
058700060509             RESET p01DtInVal;
058800060509             RESET p01DtFiVal;
058900060508           WHEN ycow0431s.opCode = 'MODIFICA';
059000060508             tuttoProtetto = *OFF;
059100060509             p01Modello = ProtectNormal;
059200060509             RESET p01Descriz;
059300060509             RESET p01DtInVal;
059400060509             RESET p01DtFiVal;
059500060508           WHEN ycow0431s.opCode = 'COPIA';
059600060508             tuttoProtetto = *ON;
059700060509             RESET p01Modello;
059800060509             RESET p01Descriz;
059900060508             p01DtInVal = ProtectNormal;
060000060508             p01DtFiVal = ProtectNormal;
060100060511           WHEN ycow0431s.opCode = 'ANNULLA' OR ycow0431s.opCode = 'CANCELLA';
060200060508             tuttoProtetto = *ON;
060300060508             p01Modello = ProtectNormal;
060400060509             p01Descriz = ProtectNormal;
060500060508             p01DtInVal = ProtectNormal;
060600060508             p01DtFiVal = ProtectNormal;
060700060508           WHEN ycow0431s.opCode = 'VISUALIZZA';
060800060508             tuttoProtetto = *ON;
060900060508             p01Modello = ProtectNormal;
061000060509             p01Descriz = ProtectNormal;
061100060508             p01DtInVal = ProtectNormal;
061200060508             p01DtFiVal = ProtectNormal;
061300060508           OTHER;
061400060508             tuttoProtetto = *ON;
061500060508             p01Modello = ProtectNormal;
061600060509             p01Descriz = ProtectNormal;
061700060508             p01DtInVal = ProtectNormal;
061800060508             p01DtFiVal = ProtectNormal;
061900060508         ENDSL;
062000060508
062100060508         SELECT;
062200060508           WHEN ycow0431s.opCode = 'NUOVO';
062300060508             r01DtInVal = D'1940-01-01';
062400060508             r01DtFiVal = D'2039-12-31';
062500060510             mrtCtb = Civilistica;
062600060510             mrtTpRegZ = Definitiva;
062700060508           OTHER;
062800060508             r01DtInVal = mrtDtInVal;
062900060508             r01DtFiVal = mrtDtFiVal;
063000060508             r02MsgFine = mrtMsgFine;
063100060508         ENDSL;
063200060508
063300060508       ENDSR;
063400060428
063500060428       //*******************************************************************************************
063600060428       //
063700060428       // Emissione testata.
063800060428       //
063900060428       //*******************************************************************************************
064000060428       BEGSR putT01;
064100060428
064200060428         WRITE t01;
064300060428
064400060428       ENDSR;
064500060428
064600060428       //*******************************************************************************************
064700060428       //
064800060510       // Emissione piede.
064900060428       //
065000060428       //*******************************************************************************************
065100060510       BEGSR putF01;
065200060428
065300060428         WRITE f01;
065400060428
065500060428       ENDSR;
065600060428
065700060428       //*******************************************************************************************
065800060428       //
065900060428       // Emissione 1a finestra testata modello.
066000060428       //
066100060428       //*******************************************************************************************
066200060428       BEGSR putR01;
066300060428
066400060510         IF errGenerico;
066500060510           errGenerico = *OFF;
066600060510           WRITE r01;
066700060510           errGenerico = *ON;
066800060510         ENDIF;
066900060510
067000060428         EXFMT r01;
067100060428         ycow0431s.rtnCmd = dsp_aid;
067200060428
067300060428         SELECT;
067400060428           WHEN dsp_aid = F6 OR dsp_aid = Enter;
067500060505             opCode = 'CHKR01';
067600060428           WHEN dsp_aid = PageDown;
067700060510             opCode = 'SETF01R02';
067800060428           WHEN dsp_aid = F4;
067900060505             opCode = 'RICHIESTA1';
068000060428           WHEN dsp_aid = F5;
068100060510             RESET opCode;
068200060428           WHEN dsp_aid = F10;
068300060505             opCode = 'RIGHE1';
068400060428           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
068500060508             opCode = 'ROLLBACK';
068600060428         ENDSL;
068700060428
068800060428       ENDSR;
068900060428
069000060428       //*******************************************************************************************
069100060428       //
069200060428       // Emissione 2a finestra testata modello.
069300060428       //
069400060428       //*******************************************************************************************
069500060428       BEGSR putR02;
069600060510
069700060510         IF errGenerico;
069800060510           errGenerico = *OFF;
069900060510           WRITE r02;
070000060510           errGenerico = *ON;
070100060510         ENDIF;
070200060428
070300060428         EXFMT r02;
070400060428         ycow0431s.rtnCmd = dsp_aid;
070500060428
070600060428         SELECT;
070700060428           WHEN dsp_aid = F6 OR dsp_aid = Enter;
070800060505             opCode = 'CHKR02';
070900060428           WHEN dsp_aid = PageUp;
071000060510             opCode = 'SETF01R01';
071100060428           WHEN dsp_aid = F4;
071200060505             opCode = 'RICHIESTA2';
071300060428           WHEN dsp_aid = F5;
071400060510             RESET opCode;
071500060428           WHEN dsp_aid = F10;
071600060505             opCode = 'RIGHE2';
071700060428           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
071800060508             opCode = 'ROLLBACK';
071900060428         ENDSL;
072000060428
072100060428       ENDSR;
072200060130
072300060131       //*******************************************************************************************
072400060131       //
072500060428       // Controllo dati 1a e 2a finestra testata modello.
072600060131       //
072700060131       //*******************************************************************************************
072800060428       BEGSR chkR0;
072900060130
073000060428         RESET errGenerico;
073100060505
073200060510         // Controllo modello già esistente.
073300060510         IF ycow0431s.opCode = 'NUOVO' OR ycow0431s.opCode = 'COPIA';
073400060510      /END-FREE
073500060510     C/EXEC SQL
073600060510     C+ SELECT COUNT(*) INTO :count
073700060510     C+ FROM YNMRT00F
073800060510     C+ WHERE MRTMODELLO = :mrtModello
073900060510     C/END-EXEC
074000060510      /FREE
074100060510           IF count > 0;
074200060510             errGenerico = *ON;
074300060510             errMsgModello = *ON;
074400060510             p01MsgId30 = 'Il modello ' + %TRIMR(mrtModello) + ' esiste già. +
074500060510             Cambiare il nome del modello.';
074600060510             opCode = 'PUTR01';
074700060510           ENDIF;
074800060510         ENDIF;
074900060505
075000060505         // Non ci sono errori e l'utente ha premuto F6:
075100060505         // se sono in immissione o copia di un nuovo modello proseguo con l'immissione
075200060505         // delle righe del modello, altrimenti aggiorno la testata del modello.
075300060505         IF NOT errGenerico AND dsp_aid = F6;
075400060505           SELECT;
075500060509             WHEN ycow0431s.opCode = 'NUOVO' OR NOT %FOUND(ynmrt00f);
075600060505               opCode = 'RIGHE1';
075700060505             OTHER;
075800060505               opCode = 'UPDDB';
075900060505           ENDSL;
076000060131         ENDIF;
076100060505
076200060130
076300060131       ENDSR;
076400060428
076500060428       //*******************************************************************************************
076600060428       //
076700060428       // Gestione richiesta.
076800060428       //
076900060428       //*******************************************************************************************
077000060428       BEGSR richiesta;
077100060428       ENDSR;
077200060428
077300060428       //*******************************************************************************************
077400060428       //
077500060505       // Aggiornamento testata modello.
077600060428       //
077700060428       //*******************************************************************************************
077800060505       BEGSR aggiornaTestata;
077900060505
078000060508         IF ycow0431s.opCode = 'VISUALIZZA';
078100060508           LEAVESR;
078200060508         ENDIF;
078300060511
078400060511         // Cancellazione del modello.
078500060511         IF ycow0431s.opCode = 'CANCELLA';
078600060512
078700060511      /END-FREE
078800060511     C/EXEC SQL
078900060511     C+ DELETE FROM YNMRR00F
079000060511     C+ WHERE MRRMODELLO = :ycow0431s.modello
079100060511     C/END-EXEC
079200060511      /FREE
079300060511           IF sqlCod < 0;
079400060511             opCode = 'ERRORE';
079500060511             ycow0431s.rtnCode = 'ERRORE';
079600060511             ycow0431s.rtnEsito = sqlCod;
079700060511             LEAVESR;
079800060511           ENDIF;
079900060512
080000060511      /END-FREE
080100060511     C/EXEC SQL
080200060511     C+ DELETE FROM YNMRT00F
080300060511     C+ WHERE MRTMODELLO = :ycow0431s.modello
080400060511     C/END-EXEC
080500060511      /FREE
080600060511           IF sqlCod < 0;
080700060511             opCode = 'ERRORE';
080800060511             ycow0431s.rtnCode = 'ERRORE';
080900060511             ycow0431s.rtnEsito = sqlCod;
081000060511             LEAVESR;
081100060511           ENDIF;
081200060512
081300060512         // Cancellazione dei commenti legati al modello.
081400060512      /END-FREE
081500060512     C/EXEC SQL
081600060512     C+ SELECT TCMKCM
081700060512     C+ INTO :tcmKcm
081800060512     C+ FROM ANTCM00F
081900060512     C+ WHERE TCMSOCIETA = :xscSoc AND TCMTPA = 'YMP'
082000060512     C+       AND TCMKY1 = :ycow0431s.modello
082100060512     C/END-EXEC
082200060512      /FREE
082300060512           IF sqlCod = 0 AND tcmKcm > 0;
082400060512      /END-FREE
082500060512     C/EXEC SQL
082600060512     C+ DELETE
082700060512     C+ FROM ANRCM00F
082800060512     C+ WHERE RCMKCM = :tcmKcm
082900060512     C/END-EXEC
083000060512      /FREE
083100060512             IF sqlCod < 0;
083200060512               opCode = 'ERRORE';
083300060512               ycow0431s.rtnCode = 'ERRORE';
083400060512               ycow0431s.rtnEsito = sqlCod;
083500060512               LEAVESR;
083600060512             ENDIF;
083700060512      /END-FREE
083800060512     C/EXEC SQL
083900060512     C+ DELETE
084000060512     C+ FROM ANTCM00F
084100060512     C+ WHERE TCMSOCIETA = :xscSoc AND TCMTPA = 'YMP'
084200060512     C+       AND TCMKY1 = :ycow0431s.modello
084300060512     C/END-EXEC
084400060512      /FREE
084500060512             IF sqlCod < 0;
084600060512               opCode = 'ERRORE';
084700060512               ycow0431s.rtnCode = 'ERRORE';
084800060512               ycow0431s.rtnEsito = sqlCod;
084900060512               LEAVESR;
085000060512             ENDIF;
085100060512           ENDIF;
085200060512
085300060511           LEAVESR;
085400060512
085500060511         ENDIF;
085600060508
085700060505         SELECT;
085800060505           WHEN ycow0431s.opCode = 'ANNULLA' AND mrtAnnulla = *ON;
085900060505             mrtAnnulla = *OFF;  // Ripristino il modello.
086000060505           WHEN ycow0431s.opCode = 'ANNULLA' AND mrtAnnulla <> *ON;
086100060505             mrtAnnulla = *ON;   // Annullo il modello.
086200060505           OTHER;
086300060505             mrtAnnulla = *OFF;
086400060505         ENDSL;
086500060505
086600060505         mrtDtInVal = r01DtInVal;
086700060505         mrtDtFiVal = r01DtFiVal;
086800060505         mrtMsgFine = %TRIMR(r02MsgFine);
086900060505
087000060505         IF NOT %FOUND(ynmrt00f) OR ycow0431s.opCode = 'COPIA';
087100060505           WRITE ynmrt000;
087200060505         ELSE;
087300060505           UPDATE ynmrt000;
087400060505         ENDIF;
087500060509
087600060509         // Copia delle righe del modello.
087700060509         IF ycow0431s.opCode = 'COPIA';
087800060509      /END-FREE
087900060509     C/EXEC SQL
088000060509     C+ INSERT INTO YNMRR00F
088100060509     C+ SELECT MRRANNULLA, :mrtModello,
088200060509     C+ MRRPRGRIGA, MRRSQZORD, MRRDTINVAL, MRRDTFIVAL, MRROBGIMM, MRRKCC,
088300060509     C+ MRRKSC, MRRCNTSET, MRRCNTRGLR, MRRCNTRGLC, MRRSEGNO, MRRCAUSRIG,
088400060509     C+ MRRPTTSETD, MRRPTTSETN, MRRPTTSETS, MRRPTTRGLE, MRRPTTAMBC,
088500060509     C+ MRRPTTRGLR, MRRPTTRGLS, MRRCASSA, MRRRGRIPVA
088600060509     C+ FROM YNMRR00F
088700060509     C+ WHERE MRRANNULLA = '0' AND MRRMODELLO = :ycow0431s.modello
088800060509     C/END-EXEC
088900060509      /FREE
089000060509         ENDIF;
089100060428
089200060428       ENDSR;
089300060505
089400060505       //*******************************************************************************************
089500060505       //
089600060505       // Gestione righe modello.
089700060505       //
089800060505       //*******************************************************************************************
089900060505       BEGSR righeModello;
090000060505
090100060505         RESET ycow0432s;
090200060505         ycow0432s.opCode = ycow0431s.opCode;
090300060505         ycow0432s.setOnLR = ycow0431s.setOnLR;
090400060505         ycow0432s.modello = mrtModello;
090500060505         ycow0432s.rrnYnmrt = db_rrn;
090600060505         kpjbu = ycow0432s;
090700060508         ycow0432r(kpjba);
090800060505         ycow0432s = kpjbu;
090900060505
091000060505         SELECT;
091100060505           WHEN ycow0432s.rtnCode = 'DONE' AND ycow0432s.rtnCmd = F6;
091200060505             opCode = 'COMMIT';
091300060508             ycow0431s.rtnCmd = ycow0432s.rtnCmd;
091400060508           WHEN ycow0432s.rtnCmd = F3 OR ycow0432s.rtnCmd = F8;
091500060505             opCode = 'ROLLBACK';
091600060508             ycow0431s.rtnCmd = ycow0432s.rtnCmd;
091700060505           OTHER;
091800060505         ENDSL;
091900060505
092000060505       ENDSR;
092100060505
092200060505       //*******************************************************************************************
092300060505       //
092400060505       // Gestione errori.
092500060505       //
092600060505       //*******************************************************************************************
092700060505       BEGSR errore;
092800060505
092900060505         CLEAR qusec;
093000060505         qusbprv = %SIZE(qusec);
093100060505         DisplayLongText(TextString:LengthOfTextString:MessageID
093200060505         :QualifiedMessageFileName:qusec);
093300060505
093400060505       ENDSR;
093500060407
093600060131       //*******************************************************************************************
093700060131       //
093800060131       // Uscita.
093900060131       //
094000060131       //*******************************************************************************************
094100060131       BEGSR uscita;
094200060131
094300060320         IF dsp_aid = F12;
094400060320           WRITE frcDta;
094500060320         ELSE;
094600060428           CLOSE(E) ycow0431d;
094700060320         ENDIF;
094800060426
094900060428         IF ycow0431s.rtnCode = ' ';
095000060428           ycow0431s.rtnCode = 'DONE';
095100060131         ENDIF;
095200060428
095300060428         kpjbu = ycow0431s;
095400060428         *INLR = (ycow0431s.setOnLR = *ON);
095500060131         RETURN;
095600060131
095700060131       ENDSR;
095800060131
095900060131      /END-FREE
