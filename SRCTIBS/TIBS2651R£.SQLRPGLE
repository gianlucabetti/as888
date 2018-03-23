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
015800060927     D tibs2600s     E DS
015900060927     D                                     INZ
016000060927     D                                     QUALIFIED
016100060915     D tibs2631s     E DS
016200060427     D                                     INZ
016300060427     D                                     QUALIFIED
016400060414     D tibs34ds      E DS
016500060414     D                                     INZ
016600060131
016700060330      //********************************************************************************************
016800060330      //
016900060330      // Definizione variabili.
017000060330      //
017100060330      //********************************************************************************************
017200060505     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
017300060505     D/COPY QSYSINC/QRPGLESRC,QUSEC
017400060505     D nullPtr         S               *
017500060130     D opCode          S             10
017600060918     D                                     INZ('INZR01')
017700060414     D rtnXsc          S              1A
017800060428     D p               S              5I 0
017900060428     D l               S              5I 0
018000060228     D esito           S             10I 0
018100060510     D count           S             10I 0
018200060512     D tcmKcm          S              9P 0
018300060427     D fxx             S
018400060427     D                                     LIKE(dsp_aid)
018500060427     D                                     DIM(4)
018600060427     D                                     ASCEND
018700060505     D  TextString...
018800060505     D                 S            256A
018900060505     D  LengthOfTextString...
019000060505     D                 S             10I 0
019100060505     D                                     INZ(%SIZE(textString))
019200060505     D  MessageID...
019300060505     D                 S              7A
019400060505     D                                     INZ('BAR0019')
019500060505     D  QualifiedMessageFileName...
019600060505     D                 S             20A
019700060505     D                                     INZ('YBARMSG   *LIBL')
019800060505
019900060330      //********************************************************************************************
020000060330      //
020100060330      // Definizione aree dati.
020200060330      //
020300060330      //********************************************************************************************
020400060201     D §azute        E DS
020500060201     D                                     EXTNAME(azute00f)
020600060201     D                                     DTAARA
020700060201     D §DatiUte      E DS
020800060201     D                                     EXTNAME(dDatiUte)
020900060201     D                                     DTAARA
021000060403
021100060330      //********************************************************************************************
021200060330      //
021300060330      // Definizione parametri procedura.
021400060330      //
021500060330      //********************************************************************************************
021600060131     C     *ENTRY        PLIST
021700060131     C                   PARM                    kpjba
021800060127
021900060127      /FREE
022000060130
022100060131       EXSR inzProc;
022200060130
022300060130       DOU opCode = 'END';
022400060130         SELECT;
022500060510           // Inizio transazione.
022600060915           WHEN opCode = 'INZR01';
022700060510             opCode = 'INZT01';
022800060918             EXSR inzR01;
022900060428           WHEN opCode = 'INZT01';
023000060510             opCode = 'INZF01';
023100060428             EXSR inzT01;
023200060510           WHEN opCode = 'INZF01';
023300060510             opCode = 'SETR01';
023400060510             EXSR inzF01;
023500060510           // Finestra 1 di 2.
023600060510           WHEN opCode = 'SETR01';
023700060510             opCode = 'SETT01R01';
023800060918             EXSR setR01;
023900060510           WHEN opCode = 'SETT01R01';
024000060510             opCode = 'SETF01R01';
024100060428             EXSR setT01;
024200060510           WHEN opCode = 'SETF01R01';
024300060510             opCode = 'PUTT01R01';
024400060510             EXSR setF01R01;
024500060510           WHEN opCode = 'PUTT01R01';
024600060510             opCode = 'PUTF01R01';
024700060510             EXSR putT01;
024800060510           WHEN opCode = 'PUTF01R01';
024900060510             opCode = 'PUTR01';
025000060510             EXSR putF01;
025100060510           WHEN opCode = 'PUTR01';
025200060510             EXSR putR01;
025300060510           WHEN opCode = 'CHKR01';
025400060510             opCode = 'PUTR01';
025500060918             EXSR chkR01;
025600060918           WHEN opCode = 'RICHIESTA';
025700060510             opCode = 'PUTR01';
025800060510             EXSR richiesta;
025900060510           // Fine transazione.
026000060505           WHEN opCode = 'UPDDB';
026100060505             opCode = 'COMMIT';
026200060915             EXSR aggiornaRegola;
026300060505           WHEN opCode = 'ERRORE';
026400060505             opCode = 'ROLLBACK';
026500060505             EXSR errore;
026600060505           WHEN opCode = 'COMMIT';
026700060505             opCode = 'END';
026800060505             COMMIT;
026900060505           WHEN opCode = 'ROLLBACK';
027000060505             opCode = 'END';
027100060505             ROLBK;
027200060505           WHEN opCode = 'END';
027300060505             LEAVE;
027400060130           OTHER;
027500060505             TextString = 'Codice operativo ' + %TRIMR(opCode)
027600060505             + ' sconosciuto.';
027700060505             opCode = 'ERRORE';
027800060130         ENDSL;
027900060130       ENDDO;
028000060130
028100060130       EXSR uscita;
028200060323
028300060323       //*******************************************************************************************
028400060323       //
028500060323       // Operazioni iniziali da eseguire una tantum.
028600060323       //
028700060323       //*******************************************************************************************
028800060323       BEGSR *INZSR;
028900060330
029000060509      /END-FREE
029100060509     C/EXEC SQL
029200060509     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER, COMMIT = *CHG
029300060509     C/END-EXEC
029400060509      /FREE
029500060509
029600060323         IN(E) §azute;
029700060323         IF NOT %ERROR;
029800060323           IN(E) §DatiUte;
029900060323         ENDIF;
030000060323         IF %ERROR OR rsut = ' ';
030100060323           tibs34r(tibs34ds);
030200060323           IN §azute;
030300060323           IN §DatiUte;
030400060323         ENDIF;
030500060407
030600060407       ENDSR;
030700060131
030800060131       //*******************************************************************************************
030900060131       //
031000060428       // Operazioni iniziali da eseguire ad ogni chiamata.
031100060131       //
031200060131       //*******************************************************************************************
031300060131       BEGSR inzProc;
031400060330
031500060330         RESET opCode;
031600060131
031700060131         EXSR chkParm;
031800060222
031900060222         CLEAR kpjbu;
032000060405         CLEAR xSoc001ds;
032100060405         xSoc('SOC001':xscSoc:0:*BLANK:rtnXsc:xsoc001ds:kpjba);
032200060222         IF rtnXsc = *ON;
032300060927           tibs2600s.rtnCode = 'ERRXSOC';
032400060330           opCode = 'ERRORE';
032500060330           LEAVESR;
032600060222         ENDIF;
032700060428
032800060926         // Controllo autorizzazione generale alla gestione dei numeratori.
032900060921         RESET xChkAutDS;
033000060921         xcaSoc = xscSoc;
033100060921         xcaGrp = xscGrp;
033200060921         xcaPrf = kNmUs;
033300060921         xChkAut(xChkAutDS);
033400060921         IF xcaRtn <> 0;
033500060921           opCode = 'END';
033600060921           LEAVESR;
033700060921         ENDIF;
033800060926
033900060926         // Controllo autorizzazione specifica alla gestione delle regole.
034000060926         RESET xChkAutDS;
034100060926         xcaSoc = xscSoc;
034200060926         xcaGrp = xscGrp;
034300060926         xcaPrf = kNmUs;
034400060926         SELECT;
034500060927           WHEN tibs2600s.opCode = 'NUOVO';
034600060926             xcaVfu = 'F10';
034700060927           WHEN tibs2600s.opCode = 'MODIFICA';
034800060926             xcaVfu = 'O02';
034900060927           WHEN tibs2600s.opCode = 'COPIA';
035000060926             xcaVfu = 'O03';
035100060927           WHEN tibs2600s.opCode = 'CANCELLA';
035200060926             xcaVfu = 'O44';
035300060926         ENDSL;
035400060926         xChkAut(xChkAutDS);
035500060926         IF xcaRtn <> 0;
035600060926           opCode = 'END';
035700060926           LEAVESR;
035800060926         ENDIF;
035900060926
036000060915         IF NOT %OPEN(tibs2631d);
036100060915           OPEN tibs2631d;
036200060428         ENDIF;
036300060428
036400060915         IF NOT %OPEN(aznmrr00f);
036500060915           OPEN aznmrr00f;
036600060428         ENDIF;
036700060428
036800060131       ENDSR;
036900060131
037000060131       //*******************************************************************************************
037100060131       //
037200060131       // Controllo dei parametri ricevuti.
037300060131       //
037400060131       //*******************************************************************************************
037500060131       BEGSR chkParm;
037600060414
037700060927         tibs2600s = kpjbu;
037800060927         tibs2631s = %SUBST(kpjbu:%SIZE(tibs2600s)+1);
037900060927         CLEAR tibs2600s.rtnCmd;
038000060927         CLEAR tibs2600s.rtnCode;
038100060927         CLEAR tibs2600s.rtnEsito;
038200060927         IF tibs2600s.opCode = 'NUOVO';
038300060915           CLEAR tibs2631s.codice;
038400060915           CLEAR tibs2631s.rrnAznmrr;
038500060428         ENDIF;
038600060426
038700060131       ENDSR;
038800060428
038900060428       //*******************************************************************************************
039000060428       //
039100060915       // Reperisco testata regola.
039200060428       //
039300060428       //*******************************************************************************************
039400060915       BEGSR getRegolaNumeratore;
039500060428
039600060428         SELECT;
039700060927           WHEN tibs2600s.opCode = 'COPIA' OR tibs2600s.opCode = 'VISUALIZZA';
039800060915             CHAIN(EN) tibs2631s.rrnAznmrr aznmrr00f;
039900060927           WHEN tibs2600s.opCode = 'NUOVO';
040000060915             CLEAR aznmrr000;
040100060428           OTHER;
040200060915             CHAIN(E) tibs2631s.rrnAznmrr aznmrr00f;
040300060428             IF %ERROR;
040400060927               tibs2600s.rtnCode = 'AZNMRR00F';
040500060927               tibs2600s.rtnEsito = %STATUS;
040600060428               SELECT;
040700060428                 WHEN %STATUS = RecordAlreadyLocked;
040800060428                   opCode = 'END';
040900060428                 OTHER;
041000060428                   opCode = 'ERRORE';
041100060428               ENDSL;
041200060428             ENDIF;
041300060428         ENDSL;
041400060428
041500060428       ENDSR;
041600060428
041700060428       //*******************************************************************************************
041800060428       //
041900060428       // Inizializzo testata.
042000060428       //
042100060428       //*******************************************************************************************
042200060428       BEGSR inzT01;
042300060428
042400060510         RESET t01PgmName;
042500060510         RESET t01SottoTi;
042600060428
042700060428       ENDSR;
042800060508
042900060508       //*******************************************************************************************
043000060508       //
043100060508       // Inizializzo piede.
043200060508       //
043300060508       //*******************************************************************************************
043400060508       BEGSR inzF01;
043500060508
043600060510         RESET f01;
043700060508
043800060508       ENDSR;
043900060508
044000060508       //*******************************************************************************************
044100060508       //
044200060918       // Inizializzo finestra regola.
044300060508       //
044400060508       //*******************************************************************************************
044500060915       BEGSR inzR01;
044600060508
044700060927         IF tibs2600s.opCode = 'NUOVO';
044800060510           RESET r01;
044900060508         ENDIF;
045000060508
045100060508       ENDSR;
045200060428
045300060428       //*******************************************************************************************
045400060428       //
045500060428       // Carico dati testata.
045600060428       //
045700060428       //*******************************************************************************************
045800060428       BEGSR setT01;
045900060428
046000060428         t01PgmName = pgmName;
046100060428
046200060428         // Importo il sottotitolo a seconda del codice operativo ricevuto.
046300060428         SELECT;
046400060927           WHEN tibs2600s.opCode = 'NUOVO';
046500060915             t01SottoTi = 'Immissione regola';
046600060927           WHEN tibs2600s.opCode = 'MODIFICA';
046700060915             t01SottoTi = 'Modifica regola';
046800060927           WHEN tibs2600s.opCode = 'COPIA';
046900060915             t01SottoTi = 'Copia regola';
047000060927           WHEN tibs2600s.opCode = 'VISUALIZZA';
047100060915             t01SottoTi = 'Visualizzazione regola';
047200060927           WHEN tibs2600s.opCode = 'CANCELLA';
047300060915             t01SottoTi = 'Cancella regola e numeratori';
047400060428           OTHER;
047500060927             t01SottoTi = tibs2600s.opCode;
047600060428         ENDSL;
047700060428
047800060428         // Centro il sottotitolo.
047900060428         l = %LEN(%TRIMR(t01SottoTi));
048000060428         p = ((%SIZE(t01SottoTi) - l) / 2) + 1;
048100060428         %SUBST(t01SottoTi:p) = %TRIMR(t01SottoTi);
048200060428         %SUBST(t01SottoTi:1:p-1) = *BLANK;
048300060428
048400060428       ENDSR;
048500060508
048600060508       //*******************************************************************************************
048700060508       //
048800060510       // Carico dati piede per finestra 1.
048900060508       //
049000060508       //*******************************************************************************************
049100060510       BEGSR setF01R01;
049200060508
049300060508         // Abilito i tasti funzionali.
049400060508         SELECT;
049500060927           WHEN tibs2600s.opCode = 'NUOVO';
049600060921             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
049700060921             F12=Ritorno';
049800060921             F6Attivo = *ON;
049900060508             F8Attivo = *OFF;
050000060927           WHEN tibs2600s.opCode = 'MODIFICA';
050100060921             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
050200060918             F8=Successivo';
050300060508             F6Attivo = *ON;
050400060508             F8Attivo = *ON;
050500060927           WHEN tibs2600s.opCode = 'COPIA';
050600060508             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
050700060508             F8=Successivo  F12=Ritorno';
050800060508             F6Attivo = *ON;
050900060508             F8Attivo = *ON;
051000060927           WHEN tibs2600s.opCode = 'CANCELLA';
051100060508             f01Tasti = 'F3=Fine  F6=Conferma  F8=Successivo  +
051200060508             F12=Ritorno';
051300060508             F6Attivo = *ON;
051400060508             F8Attivo = *ON;
051500060927           WHEN tibs2600s.opCode = 'VISUALIZZA';
051600060918             f01Tasti = 'F3=Fine  F8=Successivo  +
051700060508             F12=Ritorno';
051800060508             F6Attivo = *OFF;
051900060508             F8Attivo = *ON;
052000060508           OTHER;
052100060508             F6Attivo = *OFF;
052200060508             F8Attivo = *OFF;
052300060508         ENDSL;
052400060508
052500060508       ENDSR;
052600060508
052700060508       //*******************************************************************************************
052800060508       //
052900060918       // Imposto dati finestra regola.
053000060508       //
053100060508       //*******************************************************************************************
053200060915       BEGSR setR01;
053300060508
053400060915         EXSR getRegolaNumeratore;
053500060508
053600060508         // Imposto attributi campi.
053700060508         SELECT;
053800060927           WHEN tibs2600s.opCode = 'NUOVO';
053900060508             tuttoProtetto = *OFF;
054000060918             RESET p01Codice;
054100060509             RESET p01Descriz;
054200060927           WHEN tibs2600s.opCode = 'MODIFICA';
054300060508             tuttoProtetto = *OFF;
054400060918             p01Codice = ProtectNormal;
054500060509             RESET p01Descriz;
054600060927           WHEN tibs2600s.opCode = 'COPIA';
054700060921             tuttoProtetto = *OFF;
054800060918             RESET p01Codice;
054900060509             RESET p01Descriz;
055000060927           WHEN tibs2600s.opCode = 'CANCELLA';
055100060508             tuttoProtetto = *ON;
055200060918             p01Codice = ProtectNormal;
055300060509             p01Descriz = ProtectNormal;
055400060927           WHEN tibs2600s.opCode = 'VISUALIZZA';
055500060508             tuttoProtetto = *ON;
055600060918             p01Codice = ProtectNormal;
055700060509             p01Descriz = ProtectNormal;
055800060508           OTHER;
055900060508             tuttoProtetto = *ON;
056000060918             p01Codice = ProtectNormal;
056100060509             p01Descriz = ProtectNormal;
056200060508         ENDSL;
056300060508
056400060508         SELECT;
056500060927           WHEN tibs2600s.opCode = 'NUOVO';
056600060918             valNIncrem = 1;
056700060508           OTHER;
056800060921             IF generAutom = *ON;
056900060921               generAutom = xscOn;
057000060921             ELSE;
057100060921               generAutom = xscOff;
057200060921             ENDIF;
057300060921             IF rtnAutIniz = *ON;
057400060921               rtnAutIniz = xscOn;
057500060921             ELSE;
057600060921               rtnAutIniz = xscOff;
057700060921             ENDIF;
057800060921             IF conData = *ON;
057900060921               conData = xscOn;
058000060921             ELSE;
058100060921               conData = xscOff;
058200060921             ENDIF;
058300060921             IF dataSequen = *ON;
058400060921               dataSequen = xscOn;
058500060921             ELSE;
058600060921               dataSequen = xscOff;
058700060921             ENDIF;
058800060921             IF stopCapien = *ON;
058900060921               stopCapien = xscOn;
059000060921             ELSE;
059100060921               stopCapien = xscOff;
059200060921             ENDIF;
059300060508         ENDSL;
059400060508
059500060508       ENDSR;
059600060428
059700060428       //*******************************************************************************************
059800060428       //
059900060428       // Emissione testata.
060000060428       //
060100060428       //*******************************************************************************************
060200060428       BEGSR putT01;
060300060428
060400060428         WRITE t01;
060500060428
060600060428       ENDSR;
060700060428
060800060428       //*******************************************************************************************
060900060428       //
061000060510       // Emissione piede.
061100060428       //
061200060428       //*******************************************************************************************
061300060510       BEGSR putF01;
061400060428
061500060428         WRITE f01;
061600060428
061700060428       ENDSR;
061800060428
061900060428       //*******************************************************************************************
062000060428       //
062100060915       // Emissione 1a finestra testata regola.
062200060428       //
062300060428       //*******************************************************************************************
062400060428       BEGSR putR01;
062500060428
062600060510         IF errGenerico;
062700060510           errGenerico = *OFF;
062800060510           WRITE r01;
062900060510           errGenerico = *ON;
063000060510         ENDIF;
063100060510
063200060428         EXFMT r01;
063300060927         tibs2600s.rtnCmd = dsp_aid;
063400060428
063500060428         SELECT;
063600060428           WHEN dsp_aid = F6 OR dsp_aid = Enter;
063700060505             opCode = 'CHKR01';
063800060428           WHEN dsp_aid = F4;
063900060915             opCode = 'RICHIESTA';
064000060428           WHEN dsp_aid = F5;
064100060510             RESET opCode;
064200060428           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
064300060508             opCode = 'ROLLBACK';
064400060428         ENDSL;
064500060428
064600060428       ENDSR;
064700060130
064800060131       //*******************************************************************************************
064900060131       //
065000060915       // Controllo dati 1a e 2a finestra testata regola.
065100060131       //
065200060131       //*******************************************************************************************
065300060915       BEGSR chkR01;
065400060130
065500060428         RESET errGenerico;
065600060921         livelloPO = POTutti;
065700060921         regolaArea = NessunaRegolaAreaPO;
065800060921         tipoNumero = Numerico;
065900060921         composizio = SoloNumeri;
066000060505
066100060915         // Controllo regola già esistente.
066200060927         IF tibs2600s.opCode = 'NUOVO' OR tibs2600s.opCode = 'COPIA';
066300060510      /END-FREE
066400060510     C/EXEC SQL
066500060510     C+ SELECT COUNT(*) INTO :count
066600060915     C+ FROM AZNMRR00F
066700060915     C+ WHERE CODICE = :codice
066800060510     C/END-EXEC
066900060510      /FREE
067000060510           IF count > 0;
067100060510             errGenerico = *ON;
067200060918             errMsgCodice = *ON;
067300060915             p01MsgId30 = 'La regola ' + %TRIMR(codice) + ' esiste già. +
067400060918             Cambiare il codice della regola.';
067500060510             opCode = 'PUTR01';
067600060510           ENDIF;
067700060510         ENDIF;
067800060505
067900060920         // Decodifica temporizzatore (il controllo di validità è fatto dal DSPF).
068000060920         SELECT;
068100060920           WHEN temporizza = Anno;
068200060920             desTempor = 'Anno';
068300060920           WHEN temporizza = Mese;
068400060920             desTempor = 'Mese';
068500060920           WHEN temporizza = Giorno;
068600060920             desTempor = 'Giorno';
068700060920           WHEN temporizza = Esercizio;
068800060920             desTempor = 'Esercizio CG Proj';
068900060920           WHEN temporizza = Progressivo;
069000060920             desTempor = 'Progressivo';
069100060920           WHEN temporizza = Assoluto;
069200060920             desTempor = 'Assoluto';
069300060920           OTHER;
069400060920             desTempor = *ALL'?';
069500060921             errGenerico = *ON;
069600060920         ENDSL;
069700060920
069800060920         // Quantità tempo.
069900060920         IF qtaTempo = 0;
070000060920           IF temporizza = Anno OR temporizza = Mese OR temporizza = Giorno OR
070100060920           temporizza = Esercizio;
070200060920             qtaTempo = 1;
070300060920           ENDIF;
070400060920         ELSE;
070500060920           IF temporizza = Progressivo OR temporizza = Assoluto;
070600060920             CLEAR qtaTempo;
070700060920           ENDIF;
070800060920         ENDIF;
070900060920
071000060920         // Decodifica gruppo o società (il controllo di validità è fatto dal DSPF).
071100060920         SELECT;
071200060920           WHEN gruOSoc = DiGruppo;
071300060920             desGruOSoc = 'Di gruppo';
071400060920           WHEN gruOSoc = PerSocieta;
071500060920             desGruOSoc = 'Per società';
071600060920           OTHER;
071700060920             desGruOSoc = *ALL'?';
071800060921             errGenerico = *ON;
071900060920         ENDSL;
072000060921
072100060921         IF tipoNumero = Numerico; // Il numeratore è numerico.
072200060921           IF lunghezza < 0 OR lunghezza > 15; // Lunghezza ammessa.
072300060921             errGenerico = *ON;
072400060921           ENDIF;
072500060921           IF valNInizio = 0; // Impostazione automatica del valore iniziale.
072600060921             valNInizio = 1;
072700060925           ELSE;
072800060925             IF %LEN(%CHAR(valNInizio)) > lunghezza;
072900060925               errGenerico = *ON;
073000060925               errMsgValNIni = *ON;
073100060925               p01ValNIni = 'Immettere un valore iniziale congruente con la +
073200060925               lunghezza del numero.';
073300060925             ENDIF;
073400060921           ENDIF;
073500060921           IF valNFine = 0; // Impostazione automatica del valore finale.
073600060921             SELECT;
073700060921               WHEN lunghezza = 1;
073800060921                 valNFine = 9;
073900060921               WHEN lunghezza = 2;
074000060921                 valNFine = 99;
074100060921               WHEN lunghezza = 3;
074200060921                 valNFine = 999;
074300060921               WHEN lunghezza = 4;
074400060921                 valNFine = 9999;
074500060921               WHEN lunghezza = 5;
074600060921                 valNFine = 99999;
074700060921               WHEN lunghezza = 6;
074800060921                 valNFine = 999999;
074900060921               WHEN lunghezza = 7;
075000060921                 valNFine = 9999999;
075100060921               WHEN lunghezza = 8;
075200060921                 valNFine = 99999999;
075300060921               WHEN lunghezza = 9;
075400060921                 valNFine = 999999999;
075500060921               WHEN lunghezza = 10;
075600060921                 valNFine = 9999999999;
075700060921               WHEN lunghezza = 11;
075800060921                 valNFine = 99999999999;
075900060921               WHEN lunghezza = 12;
076000060921                 valNFine = 999999999999;
076100060921               WHEN lunghezza = 13;
076200060921                 valNFine = 9999999999999;
076300060921               WHEN lunghezza = 14;
076400060921                 valNFine = 99999999999999;
076500060921               WHEN lunghezza = 15;
076600060921                 valNFine = 999999999999999;
076700060921             ENDSL;
076800060925           ELSE;
076900060925             IF %LEN(%CHAR(valNFine)) > lunghezza;
077000060925               errGenerico = *ON;
077100060925               errMsgValNFin = *ON;
077200060925               p01ValNFin = 'Immettere un valore finale congruente con la +
077300060925               lunghezza del numero.';
077400060925             ENDIF;
077500060921           ENDIF;
077600060925           IF valNInizio > valNFine;
077700060925             errGenerico = *ON;
077800060925             errMsgValNIni = *ON;
077900060925             p01ValNIni = 'Immettere un valore iniziale minore del valore +
078000060925             finale.';
078100060925             errMsgValNFin = *ON;
078200060925             p01ValNFin = 'Immettere un valore finale maggiore del valore +
078300060925             iniziale.';
078400060925           ENDIF;
078500060925           IF sogliaNRis > 0 AND sogliaNRis > valNFine;
078600060925             errGenerico = *ON;
078700060925             errMsgSogliaN = *ON;
078800060925             p01SogliaN = 'Immettere una soglia di rischio minore +
078900060925             del valore finale.';
079000060925           ENDIF;
079100060921         ENDIF;
079200060920
079300060915         // Non ci sono errori e l'utente ha premuto F6: aggiorno regola.
079400060505         IF NOT errGenerico AND dsp_aid = F6;
079500060915           opCode = 'UPDDB';
079600060131         ENDIF;
079700060505
079800060130
079900060131       ENDSR;
080000060428
080100060428       //*******************************************************************************************
080200060428       //
080300060428       // Gestione richiesta.
080400060428       //
080500060428       //*******************************************************************************************
080600060428       BEGSR richiesta;
080700060428       ENDSR;
080800060428
080900060428       //*******************************************************************************************
081000060428       //
081100060915       // Aggiornamento testata regola.
081200060428       //
081300060428       //*******************************************************************************************
081400060915       BEGSR aggiornaRegola;
081500060505
081600060927         IF tibs2600s.opCode = 'VISUALIZZA';
081700060508           LEAVESR;
081800060508         ENDIF;
081900060511
082000060918         // Cancellazione della regola, di tutti i suoi numeratori e dei commenti.
082100060927         IF tibs2600s.opCode = 'CANCELLA';
082200060512
082300060511      /END-FREE
082400060511     C/EXEC SQL
082500060915     C+ DELETE FROM AZNMRN00F
082600060915     C+ WHERE CODICE = :tibs2631s.codice
082700060511     C/END-EXEC
082800060511      /FREE
082900060511           IF sqlCod < 0;
083000060511             opCode = 'ERRORE';
083100060927             tibs2600s.rtnCode = 'ERRORE';
083200060927             tibs2600s.rtnEsito = sqlCod;
083300060511             LEAVESR;
083400060511           ENDIF;
083500060512
083600060511      /END-FREE
083700060511     C/EXEC SQL
083800060915     C+ DELETE FROM AZNMRR00F
083900060915     C+ WHERE CODICE = :tibs2631s.codice
084000060511     C/END-EXEC
084100060511      /FREE
084200060511           IF sqlCod < 0;
084300060511             opCode = 'ERRORE';
084400060927             tibs2600s.rtnCode = 'ERRORE';
084500060927             tibs2600s.rtnEsito = sqlCod;
084600060511             LEAVESR;
084700060511           ENDIF;
084800060512
084900060915         // Cancellazione dei commenti legati al regola.
085000060512      /END-FREE
085100060512     C/EXEC SQL
085200060512     C+ SELECT TCMKCM
085300060512     C+ INTO :tcmKcm
085400060512     C+ FROM ANTCM00F
085500060915     C+ WHERE TCMSOCIETA = :xscCpa AND TCMTPA = 'YNU'
085600060915     C+       AND TCMKY1 = :tibs2631s.codice
085700060512     C/END-EXEC
085800060512      /FREE
085900060512           IF sqlCod = 0 AND tcmKcm > 0;
086000060512      /END-FREE
086100060512     C/EXEC SQL
086200060512     C+ DELETE
086300060512     C+ FROM ANRCM00F
086400060512     C+ WHERE RCMKCM = :tcmKcm
086500060512     C/END-EXEC
086600060512      /FREE
086700060512             IF sqlCod < 0;
086800060512               opCode = 'ERRORE';
086900060927               tibs2600s.rtnCode = 'ERRORE';
087000060927               tibs2600s.rtnEsito = sqlCod;
087100060512               LEAVESR;
087200060512             ENDIF;
087300060512      /END-FREE
087400060512     C/EXEC SQL
087500060512     C+ DELETE
087600060512     C+ FROM ANTCM00F
087700060915     C+ WHERE TCMSOCIETA = :xscCpa AND TCMTPA = 'YNU'
087800060915     C+       AND TCMKY1 = :tibs2631s.codice
087900060512     C/END-EXEC
088000060512      /FREE
088100060512             IF sqlCod < 0;
088200060512               opCode = 'ERRORE';
088300060927               tibs2600s.rtnCode = 'ERRORE';
088400060927               tibs2600s.rtnEsito = sqlCod;
088500060512               LEAVESR;
088600060512             ENDIF;
088700060512           ENDIF;
088800060512
088900060511           LEAVESR;
089000060512
089100060511         ENDIF;
089200060505
089300060921         IF generAutom = xscOn;
089400060921           generAutom = *ON;
089500060921         ELSE;
089600060921           generAutom = *OFF;
089700060921         ENDIF;
089800060921         IF rtnAutIniz = xscOn;
089900060921           rtnAutIniz = *ON;
090000060921         ELSE;
090100060921           rtnAutIniz = *OFF;
090200060921         ENDIF;
090300060921         IF conData = xscOn;
090400060921           conData = *ON;
090500060921         ELSE;
090600060921           conData = *OFF;
090700060921         ENDIF;
090800060921         IF dataSequen = xscOn;
090900060921           dataSequen = *ON;
091000060921         ELSE;
091100060921           dataSequen = *OFF;
091200060921         ENDIF;
091300060921         IF stopCapien = xscOn;
091400060921           stopCapien = *ON;
091500060921         ELSE;
091600060921           stopCapien = *OFF;
091700060921         ENDIF;
091800060505
091900060927         IF tibs2600s.opCode = 'NUOVO' OR tibs2600s.opCode = 'COPIA';
092000060915           WRITE aznmrr000;
092100060505         ELSE;
092200060915           UPDATE aznmrr000;
092300060505         ENDIF;
092400060428
092500060428       ENDSR;
092600060505
092700060505       //*******************************************************************************************
092800060505       //
092900060505       // Gestione errori.
093000060505       //
093100060505       //*******************************************************************************************
093200060505       BEGSR errore;
093300060505
093400060505         CLEAR qusec;
093500060505         qusbprv = %SIZE(qusec);
093600060505         DisplayLongText(TextString:LengthOfTextString:MessageID
093700060505         :QualifiedMessageFileName:qusec);
093800060505
093900060505       ENDSR;
094000060407
094100060131       //*******************************************************************************************
094200060131       //
094300060131       // Uscita.
094400060131       //
094500060131       //*******************************************************************************************
094600060131       BEGSR uscita;
094700060131
094800060320         IF dsp_aid = F12;
094900060320           WRITE frcDta;
095000060320         ELSE;
095100060915           CLOSE(E) tibs2631d;
095200060320         ENDIF;
095300060426
095400060927         IF tibs2600s.rtnCode = ' ';
095500060927           tibs2600s.rtnCode = 'DONE';
095600060131         ENDIF;
095700060428
095800060927         kpjbu = tibs2600s + tibs2631s;
095900060927         *INLR = (tibs2600s.setOnLR = *ON);
096000060131         RETURN;
096100060131
096200060131       ENDSR;
096300060131
096400060131      /END-FREE
