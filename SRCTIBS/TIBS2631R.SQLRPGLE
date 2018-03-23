000100060510      //********************************************************************************************
000200060330      //
000300060915      // Questo programma gestisce la regola di un numeratore.
000400060330      //
000500060330      //********************************************************************************************
000600060920     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700060915     H BNDDIR('PJXBND':'QC2LE':'TIBS')
000800060207     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000900060127
001000060330      //********************************************************************************************
001100060330      //
001200060330      // Definizione files.
001300060330      //
001400060330      //********************************************************************************************
001500060915     Ftibs2631d CF   E             WORKSTN
001600060320     F                                     USROPN
001700060130     F                                     INFDS(infDspF)
001800060127     F                                     INDDS(indDspF)
001900060915     Faznmrr00f UF A E             DISK
002000060428     F                                     USROPN
002100060428     F                                     COMMIT
002200060915     F                                     INFDS(infAznmrr)
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
003400060127     D F12             C                   X'3C'
003500060127     D Enter           C                   X'F1'
003600060428     D RollDown        C                   X'F4'
003700060428     D PageUp          C                   X'F4'
003800060428     D RollUp          C                   X'F5'
003900060428     D PageDown        C                   X'F5'
004000060403     D Normal          C                   X'20'
004100060202     D ReverseImage    C                   X'21'
004200060404     D HighIntensity   C                   X'22'
004300060314     D Blink           C                   X'28'
004400060428     D ProtectNormal   C                   X'A0'
004500060428     D RecordAlreadyLocked...
004600060428     D                 C                   01218
004700060920     D Giorno          C                   'G'
004800060920     D Mese            C                   'M'
004900060920     D Anno            C                   'A'
005000060920     D Esercizio       C                   'E'
005100060920     D Progressivo     C                   'P'
005200060920     D Assoluto        C                   'T'
005300060920     D DiGruppo        C                   'G'
005400060920     D PerSocieta      C                   'S'
005500060921     D POTutti         C                   '0'
005600060921     D POLivello1      C                   '1'
005700060921     D POLivello2      C                   '2'
005800060921     D NessunaRegolaAreaPO...
005900060921     D                 C                   '0'
006000060921     D Numerico        C                   'N'
006100060921     D Carattere       C                   'C'
006200060921     D SoloNumeri      C                   'N'
006300060921     D SoloLettere     C                   'L'
006400060921     D NumeriELettere  C                   'T'
006500060404
006600060330      //********************************************************************************************
006700060330      //
006800060330      // Definizione delle procedure esterne usate.
006900060330      //
007000060330      //********************************************************************************************
007100060222     D xSoc            PR
007200060222     D                                     EXTPROC('XSOC')
007300060222     D  tipXsc                        6A
007400060222     D                                     CONST
007500060222     D  socXsc
007600060222     D                                     LIKE(pjzSocieta)
007700060222     D  cdsXsc                        9P 0
007800060222     D                                     CONST
007900060222     D  modXsc                        3A
008000060222     D                                     CONST
008100060222     D  rtnXsc                        1A
008200060222     D  xSoc001ds
008300060222     D                                     LIKEDS(xSoc001ds)
008400060222     D  kpjba
008500060222     D                                     LIKEDS(kpjba)
008600060921     D xChkAut         PR
008700060921     D                                     EXTPROC('XCHKAUT')
008800060921     D  xChkAutDS
008900060921     D                                     LIKEDS(xChkAutDS)
009000060921     D tibs34r         PR
009100060131     D                                     EXTPGM('TIBS34R')
009200060131     D  tibs34ds
009300060131     D                                     LIKEDS(tibs34ds)
009400060426     D atoi            PR            10I 0
009500060222     D                                     EXTPROC('atoi')
009600060222     D  char                           *
009700060222     D                                     VALUE
009800060222     D                                     OPTIONS(*STRING)
009900060505     D DisplayLongText...
010000060505     D                 PR
010100060505     D                                     EXTPGM('QUILNGTX')
010200060505     D  TextString...
010300060505     D                              256A
010400060505     D                                     CONST
010500060505     D                                     OPTIONS(*VARSIZE)
010600060505     D  LengthOfTextString...
010700060505     D                               10I 0
010800060505     D                                     CONST
010900060505     D  MessageID...
011000060505     D                                7A
011100060505     D                                     CONST
011200060505     D  QualifiedMessageFileName...
011300060505     D                               20A
011400060505     D                                     CONST
011500060505     D  ErrorCode...
011600060505     D                                     LIKE(qusec)
011700060505     D                                     OPTIONS(*VARSIZE)
011800060505
011900060330      //********************************************************************************************
012000060330      //
012100060330      // Definizione strutture dati.
012200060330      //
012300060330      //********************************************************************************************
012400060330     D psds           SDS
012500060131     D  pgmName          *PROC
012600060130     D infDspF         DS
012700060222     D  dsp_aid              369    369A                                        AID byte
012800060206     D indDspF         DS
012900060428     D  F6Attivo               6      6N
013000060428     D  F8Attivo               8      8N
013100060428     D  tuttoProtetto         25     25N
013200060918     D  errMsgCodice          30     30N
013300060925     D  errMsgValNIni         31     31N
013400060925     D  errMsgValNFin         32     32N
013500060925     D  errMsgSogliaN         33     33N
013600060427     D  errGenerico           99     99N
013700060915     D infAznmrr       DS
013800060505     D  db_rrn               397    400I 0                                      Relative-rcd-num
013900060404     D kpjba         E DS
014000060414     D xsoc001ds     E DS
014100060414     D                                     INZ
014200060921     D xChkAutDS     E DS
014300060921     D                                     INZ
014400060921     D  xcaFnc       E
014500060921     D                                     INZ('YAZNMR')
014600060921     D  xcaVfu       E
014700060926     D                                     INZ(*ZERO)
014800060921     D  xcaTck       E
014900060921     D                                     INZ('CK')
015000060921     D  xcaNau       E
015100060921     D                                     INZ('1')
015200060921     D cndizion      E DS
015300060130     D                                     BASED(nullPtr)
015400060130     D                                     PREFIX(diz)
015500060222     D andiz00f      E DS
015600060222     D                                     BASED(nullPtr)
015700060222     D                                     PREFIX(pjz)
015800060915     D tibs2631s     E DS
015900060427     D                                     INZ
016000060427     D                                     QUALIFIED
016100060414     D tibs34ds      E DS
016200060414     D                                     INZ
016300060131
016400060330      //********************************************************************************************
016500060330      //
016600060330      // Definizione variabili.
016700060330      //
016800060330      //********************************************************************************************
016900060505     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
017000060505     D/COPY QSYSINC/QRPGLESRC,QUSEC
017100060505     D nullPtr         S               *
017200060130     D opCode          S             10
017300060918     D                                     INZ('INZR01')
017400060414     D rtnXsc          S              1A
017500060428     D p               S              5I 0
017600060428     D l               S              5I 0
017700060228     D esito           S             10I 0
017800060510     D count           S             10I 0
017900060512     D tcmKcm          S              9P 0
018000060427     D fxx             S
018100060427     D                                     LIKE(dsp_aid)
018200060427     D                                     DIM(4)
018300060427     D                                     ASCEND
018400060505     D  TextString...
018500060505     D                 S            256A
018600060505     D  LengthOfTextString...
018700060505     D                 S             10I 0
018800060505     D                                     INZ(%SIZE(textString))
018900060505     D  MessageID...
019000060505     D                 S              7A
019100060505     D                                     INZ('BAR0019')
019200060505     D  QualifiedMessageFileName...
019300060505     D                 S             20A
019400060505     D                                     INZ('YBARMSG   *LIBL')
019500060505
019600060330      //********************************************************************************************
019700060330      //
019800060330      // Definizione aree dati.
019900060330      //
020000060330      //********************************************************************************************
020100060201     D §azute        E DS
020200060201     D                                     EXTNAME(azute00f)
020300060201     D                                     DTAARA
020400060201     D §DatiUte      E DS
020500060201     D                                     EXTNAME(dDatiUte)
020600060201     D                                     DTAARA
020700060403
020800060330      //********************************************************************************************
020900060330      //
021000060330      // Definizione parametri procedura.
021100060330      //
021200060330      //********************************************************************************************
021300060131     C     *ENTRY        PLIST
021400060131     C                   PARM                    kpjba
021500060127
021600060127      /FREE
021700060130
021800060131       EXSR inzProc;
021900060130
022000060130       DOU opCode = 'END';
022100060130         SELECT;
022200060510           // Inizio transazione.
022300060915           WHEN opCode = 'INZR01';
022400060510             opCode = 'INZT01';
022500060918             EXSR inzR01;
022600060428           WHEN opCode = 'INZT01';
022700060510             opCode = 'INZF01';
022800060428             EXSR inzT01;
022900060510           WHEN opCode = 'INZF01';
023000060510             opCode = 'SETR01';
023100060510             EXSR inzF01;
023200060510           // Finestra 1 di 2.
023300060510           WHEN opCode = 'SETR01';
023400060510             opCode = 'SETT01R01';
023500060918             EXSR setR01;
023600060510           WHEN opCode = 'SETT01R01';
023700060510             opCode = 'SETF01R01';
023800060428             EXSR setT01;
023900060510           WHEN opCode = 'SETF01R01';
024000060510             opCode = 'PUTT01R01';
024100060510             EXSR setF01R01;
024200060510           WHEN opCode = 'PUTT01R01';
024300060510             opCode = 'PUTF01R01';
024400060510             EXSR putT01;
024500060510           WHEN opCode = 'PUTF01R01';
024600060510             opCode = 'PUTR01';
024700060510             EXSR putF01;
024800060510           WHEN opCode = 'PUTR01';
024900060510             EXSR putR01;
025000060510           WHEN opCode = 'CHKR01';
025100060510             opCode = 'PUTR01';
025200060918             EXSR chkR01;
025300060918           WHEN opCode = 'RICHIESTA';
025400060510             opCode = 'PUTR01';
025500060510             EXSR richiesta;
025600060510           // Fine transazione.
025700060505           WHEN opCode = 'UPDDB';
025800060505             opCode = 'COMMIT';
025900060915             EXSR aggiornaRegola;
026000060505           WHEN opCode = 'ERRORE';
026100060505             opCode = 'ROLLBACK';
026200060505             EXSR errore;
026300060505           WHEN opCode = 'COMMIT';
026400060505             opCode = 'END';
026500060505             COMMIT;
026600060505           WHEN opCode = 'ROLLBACK';
026700060505             opCode = 'END';
026800060505             ROLBK;
026900060505           WHEN opCode = 'END';
027000060505             LEAVE;
027100060130           OTHER;
027200060505             TextString = 'Codice operativo ' + %TRIMR(opCode)
027300060505             + ' sconosciuto.';
027400060505             opCode = 'ERRORE';
027500060130         ENDSL;
027600060130       ENDDO;
027700060130
027800060130       EXSR uscita;
027900060323
028000060323       //*******************************************************************************************
028100060323       //
028200060323       // Operazioni iniziali da eseguire una tantum.
028300060323       //
028400060323       //*******************************************************************************************
028500060323       BEGSR *INZSR;
028600060330
028700060509      /END-FREE
028800060509     C/EXEC SQL
028900060928     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER, COMMIT = *CHG,
029000060928     C+            DATFMT = *ISO, TIMFMT = *ISO
029100060509     C/END-EXEC
029200060509      /FREE
029300060509
029400060323         IN(E) §azute;
029500060323         IF NOT %ERROR;
029600060323           IN(E) §DatiUte;
029700060323         ENDIF;
029800060323         IF %ERROR OR rsut = ' ';
029900060323           tibs34r(tibs34ds);
030000060323           IN §azute;
030100060323           IN §DatiUte;
030200060323         ENDIF;
030300060407
030400060407       ENDSR;
030500060131
030600060131       //*******************************************************************************************
030700060131       //
030800060428       // Operazioni iniziali da eseguire ad ogni chiamata.
030900060131       //
031000060131       //*******************************************************************************************
031100060131       BEGSR inzProc;
031200060330
031300060330         RESET opCode;
031400060131
031500060131         EXSR chkParm;
031600060222
031700060222         CLEAR kpjbu;
031800060405         CLEAR xSoc001ds;
031900060405         xSoc('SOC001':xscSoc:0:*BLANK:rtnXsc:xsoc001ds:kpjba);
032000060222         IF rtnXsc = *ON;
032100060928           tibs2631s.rtnCode = 'XSOC';
032200060330           opCode = 'ERRORE';
032300060330           LEAVESR;
032400060222         ENDIF;
032500060428
032600060926         // Controllo autorizzazione generale alla gestione dei numeratori.
032700060921         RESET xChkAutDS;
032800060921         xcaSoc = xscSoc;
032900060921         xcaGrp = xscGrp;
033000060921         xcaPrf = kNmUs;
033100060921         xChkAut(xChkAutDS);
033200060921         IF xcaRtn <> 0;
033300060921           opCode = 'END';
033400060921           LEAVESR;
033500060921         ENDIF;
033600060926
033700060926         // Controllo autorizzazione specifica alla gestione delle regole.
033800060926         RESET xChkAutDS;
033900060926         xcaSoc = xscSoc;
034000060926         xcaGrp = xscGrp;
034100060926         xcaPrf = kNmUs;
034200060926         SELECT;
034300060928           WHEN tibs2631s.opCode = 'NUOVO';
034400060926             xcaVfu = 'F10';
034500060928           WHEN tibs2631s.opCode = 'MODIFICA';
034600060926             xcaVfu = 'O02';
034700060928           WHEN tibs2631s.opCode = 'COPIA';
034800060926             xcaVfu = 'O03';
034900060928           WHEN tibs2631s.opCode = 'CANCELLA';
035000060926             xcaVfu = 'O44';
035100060926         ENDSL;
035200060926         xChkAut(xChkAutDS);
035300060926         IF xcaRtn <> 0;
035400060926           opCode = 'END';
035500060926           LEAVESR;
035600060926         ENDIF;
035700060926
035800060915         IF NOT %OPEN(tibs2631d);
035900060915           OPEN tibs2631d;
036000060428         ENDIF;
036100060428
036200060915         IF NOT %OPEN(aznmrr00f);
036300060915           OPEN aznmrr00f;
036400060428         ENDIF;
036500060428
036600060131       ENDSR;
036700060131
036800060131       //*******************************************************************************************
036900060131       //
037000060131       // Controllo dei parametri ricevuti.
037100060131       //
037200060131       //*******************************************************************************************
037300060131       BEGSR chkParm;
037400060414
037500060928         tibs2631s = kpjbu;
037600060928         CLEAR tibs2631s.rtnCmd;
037700060928         CLEAR tibs2631s.rtnCode;
037800060928         CLEAR tibs2631s.rtnEsito;
037900060928         IF tibs2631s.opCode = 'NUOVO';
038000060915           CLEAR tibs2631s.codice;
038100060915           CLEAR tibs2631s.rrnAznmrr;
038200060428         ENDIF;
038300060426
038400060131       ENDSR;
038500060428
038600060428       //*******************************************************************************************
038700060428       //
038800060915       // Reperisco testata regola.
038900060428       //
039000060428       //*******************************************************************************************
039100060915       BEGSR getRegolaNumeratore;
039200060428
039300060428         SELECT;
039400060928           WHEN tibs2631s.opCode = 'COPIA' OR tibs2631s.opCode = 'VISUALIZZA';
039500060915             CHAIN(EN) tibs2631s.rrnAznmrr aznmrr00f;
039600060928           WHEN tibs2631s.opCode = 'NUOVO';
039700060915             CLEAR aznmrr000;
039800060428           OTHER;
039900060915             CHAIN(E) tibs2631s.rrnAznmrr aznmrr00f;
040000060428             IF %ERROR;
040100060928               tibs2631s.rtnCode = 'AZNMRR00F';
040200060928               tibs2631s.rtnEsito = %STATUS;
040300060428               SELECT;
040400060428                 WHEN %STATUS = RecordAlreadyLocked;
040500060428                   opCode = 'END';
040600060428                 OTHER;
040700060428                   opCode = 'ERRORE';
040800060428               ENDSL;
040900060428             ENDIF;
041000060428         ENDSL;
041100060428
041200060428       ENDSR;
041300060428
041400060428       //*******************************************************************************************
041500060428       //
041600060428       // Inizializzo testata.
041700060428       //
041800060428       //*******************************************************************************************
041900060428       BEGSR inzT01;
042000060428
042100060510         RESET t01PgmName;
042200060510         RESET t01SottoTi;
042300060428
042400060428       ENDSR;
042500060508
042600060508       //*******************************************************************************************
042700060508       //
042800060508       // Inizializzo piede.
042900060508       //
043000060508       //*******************************************************************************************
043100060508       BEGSR inzF01;
043200060508
043300060510         RESET f01;
043400060508
043500060508       ENDSR;
043600060508
043700060508       //*******************************************************************************************
043800060508       //
043900060918       // Inizializzo finestra regola.
044000060508       //
044100060508       //*******************************************************************************************
044200060915       BEGSR inzR01;
044300060508
044400060928         IF tibs2631s.opCode = 'NUOVO';
044500060510           RESET r01;
044600060508         ENDIF;
044700060508
044800060508       ENDSR;
044900060428
045000060428       //*******************************************************************************************
045100060428       //
045200060428       // Carico dati testata.
045300060428       //
045400060428       //*******************************************************************************************
045500060428       BEGSR setT01;
045600060428
045700060428         t01PgmName = pgmName;
045800060428
045900060428         // Importo il sottotitolo a seconda del codice operativo ricevuto.
046000060428         SELECT;
046100060928           WHEN tibs2631s.opCode = 'NUOVO';
046200060915             t01SottoTi = 'Immissione regola';
046300060928           WHEN tibs2631s.opCode = 'MODIFICA';
046400060915             t01SottoTi = 'Modifica regola';
046500060928           WHEN tibs2631s.opCode = 'COPIA';
046600060915             t01SottoTi = 'Copia regola';
046700060928           WHEN tibs2631s.opCode = 'VISUALIZZA';
046800060915             t01SottoTi = 'Visualizzazione regola';
046900060928           WHEN tibs2631s.opCode = 'CANCELLA';
047000060915             t01SottoTi = 'Cancella regola e numeratori';
047100060428           OTHER;
047200060928             t01SottoTi = tibs2631s.opCode;
047300060428         ENDSL;
047400060428
047500060428         // Centro il sottotitolo.
047600060428         l = %LEN(%TRIMR(t01SottoTi));
047700060428         p = ((%SIZE(t01SottoTi) - l) / 2) + 1;
047800060428         %SUBST(t01SottoTi:p) = %TRIMR(t01SottoTi);
047900060428         %SUBST(t01SottoTi:1:p-1) = *BLANK;
048000060428
048100060428       ENDSR;
048200060508
048300060508       //*******************************************************************************************
048400060508       //
048500060510       // Carico dati piede per finestra 1.
048600060508       //
048700060508       //*******************************************************************************************
048800060510       BEGSR setF01R01;
048900060508
049000060508         // Abilito i tasti funzionali.
049100060508         SELECT;
049200060928           WHEN tibs2631s.opCode = 'NUOVO';
049300060921             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
049400060921             F12=Ritorno';
049500060921             F6Attivo = *ON;
049600060508             F8Attivo = *OFF;
049700060928           WHEN tibs2631s.opCode = 'MODIFICA';
049800060921             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
049900060918             F8=Successivo';
050000060508             F6Attivo = *ON;
050100060508             F8Attivo = *ON;
050200060928           WHEN tibs2631s.opCode = 'COPIA';
050300060508             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
050400060508             F8=Successivo  F12=Ritorno';
050500060508             F6Attivo = *ON;
050600060508             F8Attivo = *ON;
050700060928           WHEN tibs2631s.opCode = 'CANCELLA';
050800060508             f01Tasti = 'F3=Fine  F6=Conferma  F8=Successivo  +
050900060508             F12=Ritorno';
051000060508             F6Attivo = *ON;
051100060508             F8Attivo = *ON;
051200060928           WHEN tibs2631s.opCode = 'VISUALIZZA';
051300060918             f01Tasti = 'F3=Fine  F8=Successivo  +
051400060508             F12=Ritorno';
051500060508             F6Attivo = *OFF;
051600060508             F8Attivo = *ON;
051700060508           OTHER;
051800060508             F6Attivo = *OFF;
051900060508             F8Attivo = *OFF;
052000060508         ENDSL;
052100060508
052200060508       ENDSR;
052300060508
052400060508       //*******************************************************************************************
052500060508       //
052600060918       // Imposto dati finestra regola.
052700060508       //
052800060508       //*******************************************************************************************
052900060915       BEGSR setR01;
053000060508
053100060915         EXSR getRegolaNumeratore;
053200060508
053300060508         // Imposto attributi campi.
053400060508         SELECT;
053500060928           WHEN tibs2631s.opCode = 'NUOVO';
053600060508             tuttoProtetto = *OFF;
053700060918             RESET p01Codice;
053800060509             RESET p01Descriz;
053900060928           WHEN tibs2631s.opCode = 'MODIFICA';
054000060508             tuttoProtetto = *OFF;
054100060918             p01Codice = ProtectNormal;
054200060509             RESET p01Descriz;
054300060928           WHEN tibs2631s.opCode = 'COPIA';
054400060921             tuttoProtetto = *OFF;
054500060918             RESET p01Codice;
054600060509             RESET p01Descriz;
054700060928           WHEN tibs2631s.opCode = 'CANCELLA';
054800060508             tuttoProtetto = *ON;
054900060918             p01Codice = ProtectNormal;
055000060509             p01Descriz = ProtectNormal;
055100060928           WHEN tibs2631s.opCode = 'VISUALIZZA';
055200060508             tuttoProtetto = *ON;
055300060918             p01Codice = ProtectNormal;
055400060509             p01Descriz = ProtectNormal;
055500060508           OTHER;
055600060508             tuttoProtetto = *ON;
055700060918             p01Codice = ProtectNormal;
055800060509             p01Descriz = ProtectNormal;
055900060508         ENDSL;
056000060508
056100060508         SELECT;
056200060928           WHEN tibs2631s.opCode = 'NUOVO';
056300060918             valNIncrem = 1;
056400060508           OTHER;
056500060921             IF generAutom = *ON;
056600060921               generAutom = xscOn;
056700060921             ELSE;
056800060921               generAutom = xscOff;
056900060921             ENDIF;
057000060921             IF rtnAutIniz = *ON;
057100060921               rtnAutIniz = xscOn;
057200060921             ELSE;
057300060921               rtnAutIniz = xscOff;
057400060921             ENDIF;
057500060921             IF conData = *ON;
057600060921               conData = xscOn;
057700060921             ELSE;
057800060921               conData = xscOff;
057900060921             ENDIF;
058000060921             IF dataSequen = *ON;
058100060921               dataSequen = xscOn;
058200060921             ELSE;
058300060921               dataSequen = xscOff;
058400060921             ENDIF;
058500060921             IF stopCapien = *ON;
058600060921               stopCapien = xscOn;
058700060921             ELSE;
058800060921               stopCapien = xscOff;
058900060921             ENDIF;
059000060928           EXSR chkR01;
059100060508         ENDSL;
059200060508
059300060508       ENDSR;
059400060428
059500060428       //*******************************************************************************************
059600060428       //
059700060428       // Emissione testata.
059800060428       //
059900060428       //*******************************************************************************************
060000060428       BEGSR putT01;
060100060428
060200060428         WRITE t01;
060300060428
060400060428       ENDSR;
060500060428
060600060428       //*******************************************************************************************
060700060428       //
060800060510       // Emissione piede.
060900060428       //
061000060428       //*******************************************************************************************
061100060510       BEGSR putF01;
061200060428
061300060428         WRITE f01;
061400060428
061500060428       ENDSR;
061600060428
061700060428       //*******************************************************************************************
061800060428       //
061900060915       // Emissione 1a finestra testata regola.
062000060428       //
062100060428       //*******************************************************************************************
062200060428       BEGSR putR01;
062300060428
062400060510         IF errGenerico;
062500060510           errGenerico = *OFF;
062600060510           WRITE r01;
062700060510           errGenerico = *ON;
062800060510         ENDIF;
062900060510
063000060428         EXFMT r01;
063100060928         tibs2631s.rtnCmd = dsp_aid;
063200060428
063300060428         SELECT;
063400060428           WHEN dsp_aid = F6 OR dsp_aid = Enter;
063500060505             opCode = 'CHKR01';
063600060428           WHEN dsp_aid = F4;
063700060915             opCode = 'RICHIESTA';
063800060428           WHEN dsp_aid = F5;
063900060510             RESET opCode;
064000060428           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
064100060508             opCode = 'ROLLBACK';
064200060428         ENDSL;
064300060428
064400060428       ENDSR;
064500060130
064600060131       //*******************************************************************************************
064700060131       //
064800060915       // Controllo dati 1a e 2a finestra testata regola.
064900060131       //
065000060131       //*******************************************************************************************
065100060915       BEGSR chkR01;
065200060130
065300060428         RESET errGenerico;
065400060921         livelloPO = POTutti;
065500060921         regolaArea = NessunaRegolaAreaPO;
065600060921         tipoNumero = Numerico;
065700060921         composizio = SoloNumeri;
065800060505
065900060915         // Controllo regola già esistente.
066000060928         IF tibs2631s.opCode = 'NUOVO' OR tibs2631s.opCode = 'COPIA';
066100060510      /END-FREE
066200060510     C/EXEC SQL
066300060510     C+ SELECT COUNT(*) INTO :count
066400060915     C+ FROM AZNMRR00F
066500060915     C+ WHERE CODICE = :codice
066600060510     C/END-EXEC
066700060510      /FREE
066800060510           IF count > 0;
066900060510             errGenerico = *ON;
067000060918             errMsgCodice = *ON;
067100060915             p01MsgId30 = 'La regola ' + %TRIMR(codice) + ' esiste già. +
067200060918             Cambiare il codice della regola.';
067300060510           ENDIF;
067400060510         ENDIF;
067500060505
067600060920         // Decodifica temporizzatore (il controllo di validità è fatto dal DSPF).
067700060920         SELECT;
067800060920           WHEN temporizza = Anno;
067900060920             desTempor = 'Anno';
068000060920           WHEN temporizza = Mese;
068100060920             desTempor = 'Mese';
068200060920           WHEN temporizza = Giorno;
068300060920             desTempor = 'Giorno';
068400060920           WHEN temporizza = Esercizio;
068500060920             desTempor = 'Esercizio CG Proj';
068600060920           WHEN temporizza = Progressivo;
068700060920             desTempor = 'Progressivo';
068800060920           WHEN temporizza = Assoluto;
068900060920             desTempor = 'Assoluto';
069000060920           OTHER;
069100060920             desTempor = *ALL'?';
069200060921             errGenerico = *ON;
069300060920         ENDSL;
069400060920
069500060920         // Quantità tempo.
069600060920         IF qtaTempo = 0;
069700060920           IF temporizza = Anno OR temporizza = Mese OR temporizza = Giorno OR
069800060920           temporizza = Esercizio;
069900060920             qtaTempo = 1;
070000060920           ENDIF;
070100060920         ELSE;
070200060920           IF temporizza = Progressivo OR temporizza = Assoluto;
070300060920             CLEAR qtaTempo;
070400060920           ENDIF;
070500060920         ENDIF;
070600060920
070700060920         // Decodifica gruppo o società (il controllo di validità è fatto dal DSPF).
070800060920         SELECT;
070900060920           WHEN gruOSoc = DiGruppo;
071000060920             desGruOSoc = 'Di gruppo';
071100060920           WHEN gruOSoc = PerSocieta;
071200060920             desGruOSoc = 'Per società';
071300060920           OTHER;
071400060920             desGruOSoc = *ALL'?';
071500060921             errGenerico = *ON;
071600060920         ENDSL;
071700060921
071800060921         IF tipoNumero = Numerico; // Il numeratore è numerico.
071900060921           IF lunghezza < 0 OR lunghezza > 15; // Lunghezza ammessa.
072000060921             errGenerico = *ON;
072100060921           ENDIF;
072200060921           IF valNInizio = 0; // Impostazione automatica del valore iniziale.
072300060921             valNInizio = 1;
072400060925           ELSE;
072500060925             IF %LEN(%CHAR(valNInizio)) > lunghezza;
072600060925               errGenerico = *ON;
072700060925               errMsgValNIni = *ON;
072800060925               p01ValNIni = 'Immettere un valore iniziale congruente con la +
072900060925               lunghezza del numero.';
073000060925             ENDIF;
073100060921           ENDIF;
073200060921           IF valNFine = 0; // Impostazione automatica del valore finale.
073300060921             SELECT;
073400060921               WHEN lunghezza = 1;
073500060921                 valNFine = 9;
073600060921               WHEN lunghezza = 2;
073700060921                 valNFine = 99;
073800060921               WHEN lunghezza = 3;
073900060921                 valNFine = 999;
074000060921               WHEN lunghezza = 4;
074100060921                 valNFine = 9999;
074200060921               WHEN lunghezza = 5;
074300060921                 valNFine = 99999;
074400060921               WHEN lunghezza = 6;
074500060921                 valNFine = 999999;
074600060921               WHEN lunghezza = 7;
074700060921                 valNFine = 9999999;
074800060921               WHEN lunghezza = 8;
074900060921                 valNFine = 99999999;
075000060921               WHEN lunghezza = 9;
075100060921                 valNFine = 999999999;
075200060921               WHEN lunghezza = 10;
075300060921                 valNFine = 9999999999;
075400060921               WHEN lunghezza = 11;
075500060921                 valNFine = 99999999999;
075600060921               WHEN lunghezza = 12;
075700060921                 valNFine = 999999999999;
075800060921               WHEN lunghezza = 13;
075900060921                 valNFine = 9999999999999;
076000060921               WHEN lunghezza = 14;
076100060921                 valNFine = 99999999999999;
076200060921               WHEN lunghezza = 15;
076300060921                 valNFine = 999999999999999;
076400060921             ENDSL;
076500060925           ELSE;
076600060925             IF %LEN(%CHAR(valNFine)) > lunghezza;
076700060925               errGenerico = *ON;
076800060925               errMsgValNFin = *ON;
076900060925               p01ValNFin = 'Immettere un valore finale congruente con la +
077000060925               lunghezza del numero.';
077100060925             ENDIF;
077200060921           ENDIF;
077300060925           IF valNInizio > valNFine;
077400060925             errGenerico = *ON;
077500060925             errMsgValNIni = *ON;
077600060925             p01ValNIni = 'Immettere un valore iniziale minore del valore +
077700060925             finale.';
077800060925             errMsgValNFin = *ON;
077900060925             p01ValNFin = 'Immettere un valore finale maggiore del valore +
078000060925             iniziale.';
078100060925           ENDIF;
078200060925           IF sogliaNRis > 0 AND sogliaNRis > valNFine;
078300060925             errGenerico = *ON;
078400060925             errMsgSogliaN = *ON;
078500060925             p01SogliaN = 'Immettere una soglia di rischio minore +
078600060925             del valore finale.';
078700060925           ENDIF;
078800060921         ENDIF;
078900060920
079000060915         // Non ci sono errori e l'utente ha premuto F6: aggiorno regola.
079100060505         IF NOT errGenerico AND dsp_aid = F6;
079200060915           opCode = 'UPDDB';
079300060131         ENDIF;
079400060505
079500060130
079600060131       ENDSR;
079700060428
079800060428       //*******************************************************************************************
079900060428       //
080000060428       // Gestione richiesta.
080100060428       //
080200060428       //*******************************************************************************************
080300060428       BEGSR richiesta;
080400060428       ENDSR;
080500060428
080600060428       //*******************************************************************************************
080700060428       //
080800060915       // Aggiornamento testata regola.
080900060428       //
081000060428       //*******************************************************************************************
081100060915       BEGSR aggiornaRegola;
081200060505
081300060928         IF tibs2631s.opCode = 'VISUALIZZA';
081400060508           LEAVESR;
081500060508         ENDIF;
081600060511
081700060918         // Cancellazione della regola, di tutti i suoi numeratori e dei commenti.
081800060928         IF tibs2631s.opCode = 'CANCELLA';
081900060512
082000060511      /END-FREE
082100060511     C/EXEC SQL
082200060915     C+ DELETE FROM AZNMRN00F
082300060915     C+ WHERE CODICE = :tibs2631s.codice
082400060511     C/END-EXEC
082500060511      /FREE
082600060928           IF sqlCode < 0;
082700060511             opCode = 'ERRORE';
082800060928             tibs2631s.rtnCode = 'SQLCODE';
082900060928             tibs2631s.rtnEsito = sqlCode;
083000060511           ENDIF;
083100060928
083200060511      /END-FREE
083300060511     C/EXEC SQL
083400060915     C+ DELETE FROM AZNMRR00F
083500060915     C+ WHERE CODICE = :tibs2631s.codice
083600060511     C/END-EXEC
083700060511      /FREE
083800060928           IF sqlCode < 0;
083900060511             opCode = 'ERRORE';
084000060928             tibs2631s.rtnCode = 'SQLCODE';
084100060928             tibs2631s.rtnEsito = sqlCode;
084200060511           ENDIF;
084300060512
084400060915         // Cancellazione dei commenti legati al regola.
084500060512      /END-FREE
084600060512     C/EXEC SQL
084700060512     C+ SELECT TCMKCM
084800060512     C+ INTO :tcmKcm
084900060512     C+ FROM ANTCM00F
085000060915     C+ WHERE TCMSOCIETA = :xscCpa AND TCMTPA = 'YNU'
085100060915     C+       AND TCMKY1 = :tibs2631s.codice
085200060512     C/END-EXEC
085300060512      /FREE
085400060928           IF sqlCode = 0 AND tcmKcm > 0;
085500060512      /END-FREE
085600060512     C/EXEC SQL
085700060512     C+ DELETE
085800060512     C+ FROM ANRCM00F
085900060512     C+ WHERE RCMKCM = :tcmKcm
086000060512     C/END-EXEC
086100060512      /FREE
086200060928             IF sqlCode < 0;
086300060512               opCode = 'ERRORE';
086400060928               tibs2631s.rtnCode = 'SQLCODE';
086500060928               tibs2631s.rtnEsito = sqlCode;
086600060512             ENDIF;
086700060512      /END-FREE
086800060512     C/EXEC SQL
086900060512     C+ DELETE
087000060512     C+ FROM ANTCM00F
087100060915     C+ WHERE TCMSOCIETA = :xscCpa AND TCMTPA = 'YNU'
087200060915     C+       AND TCMKY1 = :tibs2631s.codice
087300060512     C/END-EXEC
087400060512      /FREE
087500060928             IF sqlCode < 0;
087600060512               opCode = 'ERRORE';
087700060928               tibs2631s.rtnCode = 'SQLCODE';
087800060928               tibs2631s.rtnEsito = sqlCode;
087900060512             ENDIF;
088000060512           ENDIF;
088100060928
088200060928           IF tibs2631s.rtnCode = 'SQLCODE' AND tibs2631s.rtnEsito < 0;
088300060928      /END-FREE
088400060928     C/EXEC SQL
088500060928     C+ ROLLBACK
088600060928     C/END-EXEC
088700060928      /FREE
088800060928           ELSE;
088900060928      /END-FREE
089000060928     C/EXEC SQL
089100060928     C+ COMMIT
089200060928     C/END-EXEC
089300060928      /FREE
089400060928           ENDIF;
089500060512
089600060511           LEAVESR;
089700060512
089800060511         ENDIF;
089900060505
090000060921         IF generAutom = xscOn;
090100060921           generAutom = *ON;
090200060921         ELSE;
090300060921           generAutom = *OFF;
090400060921         ENDIF;
090500060921         IF rtnAutIniz = xscOn;
090600060921           rtnAutIniz = *ON;
090700060921         ELSE;
090800060921           rtnAutIniz = *OFF;
090900060921         ENDIF;
091000060921         IF conData = xscOn;
091100060921           conData = *ON;
091200060921         ELSE;
091300060921           conData = *OFF;
091400060921         ENDIF;
091500060921         IF dataSequen = xscOn;
091600060921           dataSequen = *ON;
091700060921         ELSE;
091800060921           dataSequen = *OFF;
091900060921         ENDIF;
092000060921         IF stopCapien = xscOn;
092100060921           stopCapien = *ON;
092200060921         ELSE;
092300060921           stopCapien = *OFF;
092400060921         ENDIF;
092500060505
092600060928         IF tibs2631s.opCode = 'NUOVO' OR tibs2631s.opCode = 'COPIA';
092700060915           WRITE aznmrr000;
092800060505         ELSE;
092900060915           UPDATE aznmrr000;
093000060505         ENDIF;
093100060428
093200060428       ENDSR;
093300060505
093400060505       //*******************************************************************************************
093500060505       //
093600060505       // Gestione errori.
093700060505       //
093800060505       //*******************************************************************************************
093900060505       BEGSR errore;
094000060505
094100060505         CLEAR qusec;
094200060505         qusbprv = %SIZE(qusec);
094300060505         DisplayLongText(TextString:LengthOfTextString:MessageID
094400060505         :QualifiedMessageFileName:qusec);
094500060505
094600060505       ENDSR;
094700060407
094800060131       //*******************************************************************************************
094900060131       //
095000060131       // Uscita.
095100060131       //
095200060131       //*******************************************************************************************
095300060131       BEGSR uscita;
095400060131
095500070612         IF %OPEN(tibs2631d);
095600070612           WRITE frcDta;
095700070612         ENDIF;
095800060426
095900060928         IF tibs2631s.rtnCode = ' ';
096000060928           tibs2631s.rtnCode = 'DONE';
096100060131         ENDIF;
096200060428
096300060928         kpjbu = tibs2631s;
096400060928         *INLR = (tibs2631s.setOnLR = *ON);
096500060131         RETURN;
096600060131
096700060131       ENDSR;
096800060131
096900060131      /END-FREE
