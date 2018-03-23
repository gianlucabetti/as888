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
001500060927     Ftibs2651d CF   E             WORKSTN
001600060320     F                                     USROPN
001700060130     F                                     INFDS(infDspF)
001800060127     F                                     INDDS(indDspF)
001900060927     Faznmrn00f UF A E             DISK
002000060428     F                                     USROPN
002100060428     F                                     COMMIT
002200060915     F                                     INFDS(infAznmrr)
002300060927     Faznmrr01l IF   E           K DISK
002400060928     F                                     USROPN
002500060928
002600060330      //********************************************************************************************
002700060330      //
002800060330      // Definizione costanti.
002900060330      //
003000060330      //********************************************************************************************
003100060127     D F3              C                   X'33'
003200060428     D F4              C                   X'34'
003300060127     D F5              C                   X'35'
003400060428     D F6              C                   X'36'
003500060428     D F8              C                   X'38'
003600060127     D F12             C                   X'3C'
003700060127     D Enter           C                   X'F1'
003800060428     D RollDown        C                   X'F4'
003900060428     D PageUp          C                   X'F4'
004000060428     D RollUp          C                   X'F5'
004100060428     D PageDown        C                   X'F5'
004200060403     D Normal          C                   X'20'
004300060927     D NormalProtect   C                   X'A0'
004400060928     D Nondisplay      C                   X'27'
004500060927     D NondisplayProtect...
004600060927     D                 C                   X'A7'
004700060202     D ReverseImage    C                   X'21'
004800060404     D HighIntensity   C                   X'22'
004900060314     D Blink           C                   X'28'
005000060428     D RecordAlreadyLocked...
005100060428     D                 C                   01218
005200060920     D Giorno          C                   'G'
005300060920     D Mese            C                   'M'
005400060920     D Anno            C                   'A'
005500060920     D Esercizio       C                   'E'
005600060920     D Progressivo     C                   'P'
005700060920     D Assoluto        C                   'T'
005800060920     D DiGruppo        C                   'G'
005900060920     D PerSocieta      C                   'S'
006000060921     D POTutti         C                   '0'
006100060921     D POLivello1      C                   '1'
006200060921     D POLivello2      C                   '2'
006300060921     D NessunaRegolaAreaPO...
006400060921     D                 C                   '0'
006500060921     D Numerico        C                   'N'
006600060921     D Carattere       C                   'C'
006700060921     D SoloNumeri      C                   'N'
006800060921     D SoloLettere     C                   'L'
006900060921     D NumeriELettere  C                   'T'
007000060404
007100060330      //********************************************************************************************
007200060330      //
007300060330      // Definizione delle procedure esterne usate.
007400060330      //
007500060330      //********************************************************************************************
007600060222     D xSoc            PR
007700060222     D                                     EXTPROC('XSOC')
007800060222     D  tipXsc                        6A
007900060222     D                                     CONST
008000060222     D  socXsc
008100060222     D                                     LIKE(pjzSocieta)
008200060222     D  cdsXsc                        9P 0
008300060222     D                                     CONST
008400060222     D  modXsc                        3A
008500060222     D                                     CONST
008600060222     D  rtnXsc                        1A
008700060222     D  xSoc001ds
008800060222     D                                     LIKEDS(xSoc001ds)
008900060222     D  kpjba
009000060222     D                                     LIKEDS(kpjba)
009100060921     D xChkAut         PR
009200060921     D                                     EXTPROC('XCHKAUT')
009300060921     D  xChkAutDS
009400060921     D                                     LIKEDS(xChkAutDS)
009500060921     D tibs34r         PR
009600060131     D                                     EXTPGM('TIBS34R')
009700060131     D  tibs34ds
009800060131     D                                     LIKEDS(tibs34ds)
009900060426     D atoi            PR            10I 0
010000060222     D                                     EXTPROC('atoi')
010100060222     D  char                           *
010200060222     D                                     VALUE
010300060222     D                                     OPTIONS(*STRING)
010400060505     D DisplayLongText...
010500060505     D                 PR
010600060505     D                                     EXTPGM('QUILNGTX')
010700060505     D  TextString...
010800060505     D                              256A
010900060505     D                                     CONST
011000060505     D                                     OPTIONS(*VARSIZE)
011100060505     D  LengthOfTextString...
011200060505     D                               10I 0
011300060505     D                                     CONST
011400060505     D  MessageID...
011500060505     D                                7A
011600060505     D                                     CONST
011700060505     D  QualifiedMessageFileName...
011800060505     D                               20A
011900060505     D                                     CONST
012000060505     D  ErrorCode...
012100060505     D                                     LIKE(qusec)
012200060505     D                                     OPTIONS(*VARSIZE)
012300060505
012400060330      //********************************************************************************************
012500060330      //
012600060330      // Definizione strutture dati.
012700060330      //
012800060330      //********************************************************************************************
012900060330     D psds           SDS
013000060131     D  pgmName          *PROC
013100060130     D infDspF         DS
013200060222     D  dsp_aid              369    369A                                        AID byte
013300060206     D indDspF         DS
013400060428     D  F6Attivo               6      6N
013500060428     D  F8Attivo               8      8N
013600060428     D  tuttoProtetto         25     25N
013700060928     D  errMsgSocieta         30     30N
013800060925     D  errMsgValNIni         31     31N
013900060925     D  errMsgValNFin         32     32N
014000060925     D  errMsgSogliaN         33     33N
014100060928     D  errMsgUnita           34     34N
014200060928     D  errMsgProgressivo...
014300060928     D                        35     35N
014400060928     D  errMsgKeyGest...
014500060928     D                        36     36N
014600060928     D  errMsgNumeratoreEsiste...
014700060928     D                        37     37N
014800061013     D  errMsgNumero...
014900061013     D                        38     38N
015000060427     D  errGenerico           99     99N
015100060915     D infAznmrr       DS
015200060505     D  db_rrn               397    400I 0                                      Relative-rcd-num
015300060404     D kpjba         E DS
015400060414     D xsoc001ds     E DS
015500060414     D                                     INZ
015600060921     D xChkAutDS     E DS
015700060921     D                                     INZ
015800060921     D  xcaFnc       E
015900060921     D                                     INZ('YAZNMR')
016000060921     D  xcaVfu       E
016100060926     D                                     INZ(*ZERO)
016200060921     D  xcaTck       E
016300060921     D                                     INZ('CK')
016400060921     D  xcaNau       E
016500060921     D                                     INZ('1')
016600060921     D cndizion      E DS
016700060130     D                                     BASED(nullPtr)
016800060130     D                                     PREFIX(diz)
016900060222     D andiz00f      E DS
017000060222     D                                     BASED(nullPtr)
017100060222     D                                     PREFIX(pjz)
017200060927     D tibs2651s     E DS
017300060427     D                                     INZ
017400060427     D                                     QUALIFIED
017500060414     D tibs34ds      E DS
017600060414     D                                     INZ
017700060131
017800060330      //********************************************************************************************
017900060330      //
018000060330      // Definizione variabili.
018100060330      //
018200060330      //********************************************************************************************
018300060505     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
018400060505     D/COPY QSYSINC/QRPGLESRC,QUSEC
018500060505     D nullPtr         S               *
018600060130     D opCode          S             10
018700060918     D                                     INZ('INZR01')
018800060414     D rtnXsc          S              1A
018900060428     D p               S              5I 0
019000060428     D l               S              5I 0
019100060228     D esito           S             10I 0
019200060510     D count           S             10I 0
019300060512     D tcmKcm          S              9P 0
019400060427     D fxx             S
019500060427     D                                     LIKE(dsp_aid)
019600060427     D                                     DIM(4)
019700060427     D                                     ASCEND
019800060505     D  TextString...
019900060505     D                 S            256A
020000060505     D  LengthOfTextString...
020100060505     D                 S             10I 0
020200060505     D                                     INZ(%SIZE(textString))
020300060505     D  MessageID...
020400060505     D                 S              7A
020500060505     D                                     INZ('BAR0019')
020600060505     D  QualifiedMessageFileName...
020700060505     D                 S             20A
020800060505     D                                     INZ('YBARMSG   *LIBL')
020900060928     D sifDesBrev      S             20A
021000060928     D uniDesBrev      S             20A
021100060505
021200060330      //********************************************************************************************
021300060330      //
021400060330      // Definizione aree dati.
021500060330      //
021600060330      //********************************************************************************************
021700060201     D §azute        E DS
021800060201     D                                     EXTNAME(azute00f)
021900060201     D                                     DTAARA
022000060201     D §DatiUte      E DS
022100060201     D                                     EXTNAME(dDatiUte)
022200060201     D                                     DTAARA
022300060928
022400060928      //********************************************************************************************
022500060928      //
022600060928      // Cambio nome campi.
022700060928      //
022800060928      //********************************************************************************************
022900060928     Iaznmrr000
023000060928     I              valNInizio                  rValNInizio
023100060928     I              valNFine                    rValNFine
023200060928     I              sogliaNRis                  rSogliaNRis
023300060403
023400060330      //********************************************************************************************
023500060330      //
023600060330      // Definizione parametri procedura.
023700060330      //
023800060330      //********************************************************************************************
023900060131     C     *ENTRY        PLIST
024000060131     C                   PARM                    kpjba
024100060127
024200060127      /FREE
024300060130
024400060131       EXSR inzProc;
024500060130
024600060130       DOU opCode = 'END';
024700060130         SELECT;
024800060510           // Inizio transazione.
024900060915           WHEN opCode = 'INZR01';
025000060928             opCode = 'INZW01';
025100060918             EXSR inzR01;
025200060928           WHEN opCode = 'INZW01';
025300060510             opCode = 'INZF01';
025400060928             EXSR inzW01;
025500060510           WHEN opCode = 'INZF01';
025600060510             opCode = 'SETR01';
025700060510             EXSR inzF01;
025800060510           // Finestra 1 di 2.
025900060510           WHEN opCode = 'SETR01';
026000060928             opCode = 'SETW01R01';
026100060918             EXSR setR01;
026200060928           WHEN opCode = 'SETW01R01';
026300060510             opCode = 'SETF01R01';
026400060928             EXSR setW01;
026500060510           WHEN opCode = 'SETF01R01';
026600060928             opCode = 'PUTW01R01';
026700060510             EXSR setF01R01;
026800060928           WHEN opCode = 'PUTW01R01';
026900060510             opCode = 'PUTF01R01';
027000060928             EXSR putW01;
027100060510           WHEN opCode = 'PUTF01R01';
027200060510             opCode = 'PUTR01';
027300060510             EXSR putF01;
027400060510           WHEN opCode = 'PUTR01';
027500060510             EXSR putR01;
027600060510           WHEN opCode = 'CHKR01';
027700060510             opCode = 'PUTR01';
027800060918             EXSR chkR01;
027900060918           WHEN opCode = 'RICHIESTA';
028000060510             opCode = 'PUTR01';
028100060510             EXSR richiesta;
028200060510           // Fine transazione.
028300060505           WHEN opCode = 'UPDDB';
028400060505             opCode = 'COMMIT';
028500060927             EXSR aggiornaNumeratore;
028600060505           WHEN opCode = 'ERRORE';
028700060505             opCode = 'ROLLBACK';
028800060505             EXSR errore;
028900060505           WHEN opCode = 'COMMIT';
029000060505             opCode = 'END';
029100060505             COMMIT;
029200060505           WHEN opCode = 'ROLLBACK';
029300060505             opCode = 'END';
029400060505             ROLBK;
029500060505           WHEN opCode = 'END';
029600060505             LEAVE;
029700060130           OTHER;
029800060505             TextString = 'Codice operativo ' + %TRIMR(opCode)
029900060505             + ' sconosciuto.';
030000060505             opCode = 'ERRORE';
030100060130         ENDSL;
030200060130       ENDDO;
030300060130
030400060130       EXSR uscita;
030500060323
030600060323       //*******************************************************************************************
030700060323       //
030800060323       // Operazioni iniziali da eseguire una tantum.
030900060323       //
031000060323       //*******************************************************************************************
031100060323       BEGSR *INZSR;
031200060509
031300060323         IN(E) §azute;
031400060323         IF NOT %ERROR;
031500060323           IN(E) §DatiUte;
031600060323         ENDIF;
031700060323         IF %ERROR OR rsut = ' ';
031800060323           tibs34r(tibs34ds);
031900060323           IN §azute;
032000060323           IN §DatiUte;
032100060323         ENDIF;
032200060928
032300060928         p01SocCos = HighIntensity;
032400060928         p01UniCos = HighIntensity;
032500060928         p01PrdCos = HighIntensity;
032600060928         p01PrgCos = HighIntensity;
032700060928
032800060407       ENDSR;
032900060131
033000060131       //*******************************************************************************************
033100060131       //
033200060428       // Operazioni iniziali da eseguire ad ogni chiamata.
033300060131       //
033400060131       //*******************************************************************************************
033500060131       BEGSR inzProc;
033600060330
033700060330         RESET opCode;
033800060131
033900060131         EXSR chkParm;
034000060222
034100060222         CLEAR kpjbu;
034200060405         CLEAR xSoc001ds;
034300060405         xSoc('SOC001':xscSoc:0:*BLANK:rtnXsc:xsoc001ds:kpjba);
034400060222         IF rtnXsc = *ON;
034500060928           tibs2651s.rtnCode = 'XSOC';
034600060330           opCode = 'ERRORE';
034700060330           LEAVESR;
034800060222         ENDIF;
034900060428
035000060926         // Controllo autorizzazione generale alla gestione dei numeratori.
035100060921         RESET xChkAutDS;
035200060921         xcaSoc = xscSoc;
035300060921         xcaGrp = xscGrp;
035400060921         xcaPrf = kNmUs;
035500060921         xChkAut(xChkAutDS);
035600060921         IF xcaRtn <> 0;
035700060921           opCode = 'END';
035800060921           LEAVESR;
035900060921         ENDIF;
036000060926
036100070612         // Controllo autorizzazione specifica alla gestione del numeratore.
036200060926         RESET xChkAutDS;
036300060926         xcaSoc = xscSoc;
036400060926         xcaGrp = xscGrp;
036500060926         xcaPrf = kNmUs;
036600070612         xcaNau = *BLANK;
036700060926         SELECT;
036800060928           WHEN tibs2651s.opCode = 'NUOVO';
036900070612             xcaVfu = 'F10';
037000060928           WHEN tibs2651s.opCode = 'MODIFICA';
037100070612             xcaVfu = 'O02';
037200060928           WHEN tibs2651s.opCode = 'COPIA';
037300070612             xcaVfu = 'O03';
037400060928           WHEN tibs2651s.opCode = 'CANCELLA';
037500070612             xcaVfu = 'O04';
037600060926         ENDSL;
037700060926         xChkAut(xChkAutDS);
037800060926         IF xcaRtn <> 0;
037900070612           RESET xChkAutDS;
038000070612           xcaSoc = xscSoc;
038100070612           xcaGrp = xscGrp;
038200070612           xcaPrf = kNmUs;
038300070612           SELECT;
038400070612             WHEN tibs2651s.opCode = 'NUOVO';
038500070612               xcaVfu = 'F10_' + tibs2651s.codice;
038600070612             WHEN tibs2651s.opCode = 'MODIFICA';
038700070612               xcaVfu = 'O02_' + tibs2651s.codice;
038800070612             WHEN tibs2651s.opCode = 'COPIA';
038900070612               xcaVfu = 'O03_' + tibs2651s.codice;
039000070612             WHEN tibs2651s.opCode = 'CANCELLA';
039100070612               xcaVfu = 'O04_' + tibs2651s.codice;
039200070612           ENDSL;
039300070612           xChkAut(xChkAutDS);
039400070612           IF xcaRtn <> 0;
039500070612             opCode = 'END';
039600070612             LEAVESR;
039700070612           ENDIF;
039800060926         ENDIF;
039900060926
040000060927         IF NOT %OPEN(tibs2651d);
040100060927           OPEN tibs2651d;
040200060428         ENDIF;
040300060428
040400060927         IF NOT %OPEN(aznmrn00f);
040500060927           OPEN aznmrn00f;
040600060428         ENDIF;
040700060927
040800060927         IF NOT %OPEN(aznmrr01l);
040900060927           OPEN aznmrr01l;
041000060927         ENDIF;
041100060428
041200060929         CLEAR dsp_aid;
041300060929
041400060131       ENDSR;
041500060131
041600060131       //*******************************************************************************************
041700060131       //
041800060131       // Controllo dei parametri ricevuti.
041900060131       //
042000060131       //*******************************************************************************************
042100060131       BEGSR chkParm;
042200060414
042300060928         tibs2651s = kpjbu;
042400060928         CLEAR tibs2651s.rtnCmd;
042500060928         CLEAR tibs2651s.rtnCode;
042600060928         CLEAR tibs2651s.rtnEsito;
042700060928         IF tibs2651s.opCode = 'NUOVO';
042800060927           CLEAR tibs2651s.rrnAznmrn;
042900060428         ENDIF;
043000060426
043100060131       ENDSR;
043200060428
043300060428       //*******************************************************************************************
043400060428       //
043500060927       // Reperisco numeratore.
043600060428       //
043700060428       //*******************************************************************************************
043800060927       BEGSR getNumeratore;
043900060428
044000060927         CHAIN tibs2651s.codice aznmrr01l;
044100060927
044200060428         SELECT;
044300061019           WHEN tibs2651s.opCode = 'COPIA';
044400060927             CHAIN(EN) tibs2651s.rrnAznmrn aznmrn00f;
044500060929             CLEAR numero;
044600060929             CLEAR dataUltimo;
044700060929             IF temporizza = Progressivo;
044800060929               progressiv += 1;
044900060929             ENDIF;
045000060928           WHEN tibs2651s.opCode = 'NUOVO';
045100060928             CLEAR societa;
045200060928             CLEAR unita;
045300060928             CLEAR keyGest;
045400060928             CLEAR dataPeriod;
045500060928             CLEAR progressiv;
045600060928             CLEAR numero;
045700060928             CLEAR dataUltimo;
045800061019           WHEN tibs2651s.opCode = 'VISUALIZZA';
045900061019             CHAIN(N) tibs2651s.rrnAznmrn aznmrn00f;
046000060428           OTHER;
046100060927             CHAIN(E) tibs2651s.rrnAznmrn aznmrn00f;
046200060428             IF %ERROR;
046300060928               tibs2651s.rtnCode = 'AZNMRN00F';
046400060928               tibs2651s.rtnEsito = %STATUS;
046500060428               SELECT;
046600060428                 WHEN %STATUS = RecordAlreadyLocked;
046700060428                   opCode = 'END';
046800060428                 OTHER;
046900060428                   opCode = 'ERRORE';
047000060428               ENDSL;
047100060428             ENDIF;
047200060428         ENDSL;
047300060428
047400060428       ENDSR;
047500060428
047600060428       //*******************************************************************************************
047700060428       //
047800060428       // Inizializzo testata.
047900060428       //
048000060428       //*******************************************************************************************
048100060928       BEGSR inzW01;
048200060428       ENDSR;
048300060508
048400060508       //*******************************************************************************************
048500060508       //
048600060508       // Inizializzo piede.
048700060508       //
048800060508       //*******************************************************************************************
048900060508       BEGSR inzF01;
049000060508       ENDSR;
049100060508
049200060508       //*******************************************************************************************
049300060508       //
049400060918       // Inizializzo finestra regola.
049500060508       //
049600060508       //*******************************************************************************************
049700060915       BEGSR inzR01;
049800060508
049900060928         IF tibs2651s.opCode = 'NUOVO';
050000060510           RESET r01;
050100060928         ELSE;
050200060928           CLEAR r01Descr1;
050300060928           CLEAR r01Descr2;
050400060928           CLEAR sifDesBrev;
050500060928           CLEAR uniDesBrev;
050600060508         ENDIF;
050700060928
050800060508       ENDSR;
050900060428
051000060428       //*******************************************************************************************
051100060428       //
051200060428       // Carico dati testata.
051300060428       //
051400060428       //*******************************************************************************************
051500060928       BEGSR setW01;
051600060428
051700060428         // Importo il sottotitolo a seconda del codice operativo ricevuto.
051800060428         SELECT;
051900060928           WHEN tibs2651s.opCode = 'NUOVO';
052000060928             p01Titolo = 'Immissione numeratore';
052100060928           WHEN tibs2651s.opCode = 'MODIFICA';
052200060928             p01Titolo = 'Modifica numeratore';
052300060928           WHEN tibs2651s.opCode = 'COPIA';
052400060928             p01Titolo = 'Copia numeratore';
052500060928           WHEN tibs2651s.opCode = 'VISUALIZZA';
052600060928             p01Titolo = 'Visualizzazione numeratore';
052700060928           WHEN tibs2651s.opCode = 'CANCELLA';
052800060928             p01Titolo = 'Cancella numeratore';
052900060428           OTHER;
053000060928             p01Titolo = tibs2651s.opCode;
053100060428         ENDSL;
053200060428
053300060428       ENDSR;
053400060508
053500060508       //*******************************************************************************************
053600060508       //
053700060927       // Carico dati piede.
053800060508       //
053900060508       //*******************************************************************************************
054000060510       BEGSR setF01R01;
054100060508
054200060508         // Abilito i tasti funzionali.
054300060508         SELECT;
054400060928           WHEN tibs2651s.opCode = 'NUOVO';
054500060921             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
054600060921             F12=Ritorno';
054700060921             F6Attivo = *ON;
054800060508             F8Attivo = *OFF;
054900060928           WHEN tibs2651s.opCode = 'MODIFICA';
055000060921             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
055100060918             F8=Successivo';
055200060508             F6Attivo = *ON;
055300060508             F8Attivo = *ON;
055400060928           WHEN tibs2651s.opCode = 'COPIA';
055500060508             f01Tasti = 'F3=Fine  F5=Ricarica  F6=Conferma  +
055600060508             F8=Successivo  F12=Ritorno';
055700060508             F6Attivo = *ON;
055800060508             F8Attivo = *ON;
055900060928           WHEN tibs2651s.opCode = 'CANCELLA';
056000060508             f01Tasti = 'F3=Fine  F6=Conferma  F8=Successivo  +
056100060508             F12=Ritorno';
056200060508             F6Attivo = *ON;
056300060508             F8Attivo = *ON;
056400060928           WHEN tibs2651s.opCode = 'VISUALIZZA';
056500060918             f01Tasti = 'F3=Fine  F8=Successivo  +
056600060508             F12=Ritorno';
056700060508             F6Attivo = *OFF;
056800060508             F8Attivo = *ON;
056900060508           OTHER;
057000060508             F6Attivo = *OFF;
057100060508             F8Attivo = *OFF;
057200060508         ENDSL;
057300060508
057400060508       ENDSR;
057500060508
057600060508       //*******************************************************************************************
057700060508       //
057800060928       // Imposto dati finestra numeratore.
057900060508       //
058000060508       //*******************************************************************************************
058100060915       BEGSR setR01;
058200060508
058300060927         EXSR getNumeratore;
058400060508
058500060927         RESET p01Societa;
058600060928         RESET p01SocCos;
058700060927         RESET p01Unita;
058800060928         RESET p01UniCos;
058900060927         RESET p01KeyGest;
059000060927         RESET p01DataPer;
059100060928         RESET p01PrdCos;
059200060927         RESET p01Progres;
059300060928         RESET p01PrgCos;
059400060928         RESET p01DtUltim;
059500060928
059600060928         SELECT;
059700060928           WHEN tibs2651s.opCode = 'NUOVO';
059800060928             codice = tibs2651s.codice;
059900060928           OTHER;
060000060928         ENDSL;
060100060927
060200060508         // Imposto attributi campi.
060300060508         SELECT;
060400060928           WHEN tibs2651s.opCode = 'NUOVO';
060500060508             tuttoProtetto = *OFF;
060600061018             valNInizio = rValNInizio;
060700061018             valNFine = rValNFine;
060800061018             sogliaNRis = rSogliaNRis;
060900060928           WHEN tibs2651s.opCode = 'MODIFICA';
061000060508             tuttoProtetto = *OFF;
061100060927             p01Societa = NormalProtect;
061200060927             p01Unita = NormalProtect;
061300060927             p01KeyGest = NormalProtect;
061400060927             p01DataPer = NormalProtect;
061500060927             p01Progres = NormalProtect;
061600060928           WHEN tibs2651s.opCode = 'COPIA';
061700060921             tuttoProtetto = *OFF;
061800060928           WHEN tibs2651s.opCode = 'CANCELLA';
061900060508             tuttoProtetto = *ON;
062000060928           WHEN tibs2651s.opCode = 'VISUALIZZA';
062100060508             tuttoProtetto = *ON;
062200060508           OTHER;
062300060508             tuttoProtetto = *ON;
062400060508         ENDSL;
062500060927
062600060927         IF gruOSoc = DiGruppo;
062700060927           p01Societa = NondisplayProtect;
062800060928           p01SocCos = Nondisplay;
062900060927         ENDIF;
063000060928
063100060927         IF livelloPO = POTutti;
063200060927           p01Unita = NondisplayProtect;
063300060928           p01UniCos = Nondisplay;
063400060927         ENDIF;
063500060928
063600060927         IF desKeyGest = *BLANK;
063700060927           p01KeyGest = NondisplayProtect;
063800060928           CLEAR r01DesKeyG;
063900060928         ELSE;
064000060928           EVALR r01DesKeyG = %TRIMR(desKeyGest) + ':';
064100060927         ENDIF;
064200060928
064300060927         IF temporizza = Progressivo OR temporizza = Assoluto;
064400060927           p01DataPer = NondisplayProtect;
064500060928           p01PrdCos = Nondisplay;
064600060927         ENDIF;
064700060928
064800060928         IF temporizza <> Progressivo;
064900060928           p01Progres = NondisplayProtect;
065000060928           p01PrgCos = Nondisplay;
065100060928         ENDIF;
065200060928
065300060928         // Permetto di modificare la data ultimo prelievo solo se la regola del
065400060928         // numeratore prevedete il controllo di data in sequenza.
065500060928         IF dataSequen = *OFF;
065600060928           p01DtUltim = NormalProtect;
065700060928         ENDIF;
065800060508
065900060928         r01Descr1 = descrizion;
066000060928         r01Descr2 = %SUBST(descrizion:%SIZE(r01Descr1)+1);
066100060508
066200060928         EXSR chkR01;
066300060928
066400060508       ENDSR;
066500060428
066600060428       //*******************************************************************************************
066700060428       //
066800060428       // Emissione testata.
066900060428       //
067000060428       //*******************************************************************************************
067100060928       BEGSR putW01;
067200060428       ENDSR;
067300060428
067400060428       //*******************************************************************************************
067500060428       //
067600060510       // Emissione piede.
067700060428       //
067800060428       //*******************************************************************************************
067900060510       BEGSR putF01;
068000060428       ENDSR;
068100060428
068200060428       //*******************************************************************************************
068300060428       //
068400060915       // Emissione 1a finestra testata regola.
068500060428       //
068600060428       //*******************************************************************************************
068700060428       BEGSR putR01;
068800060428
068900060510         IF errGenerico;
069000060510           errGenerico = *OFF;
069100060510           WRITE r01;
069200060510           errGenerico = *ON;
069300060510         ENDIF;
069400060510
069500060428         EXFMT r01;
069600060928         tibs2651s.rtnCmd = dsp_aid;
069700060428
069800060428         SELECT;
069900060428           WHEN dsp_aid = F6 OR dsp_aid = Enter;
070000060505             opCode = 'CHKR01';
070100060428           WHEN dsp_aid = F4;
070200060915             opCode = 'RICHIESTA';
070300060428           WHEN dsp_aid = F5;
070400060510             RESET opCode;
070500060428           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
070600060508             opCode = 'ROLLBACK';
070700060428         ENDSL;
070800060428
070900060428       ENDSR;
071000060130
071100060131       //*******************************************************************************************
071200060131       //
071300060915       // Controllo dati 1a e 2a finestra testata regola.
071400060131       //
071500060131       //*******************************************************************************************
071600060915       BEGSR chkR01;
071700060130
071800060428         RESET errGenerico;
071900060505
072000060927         // Controllo numeratore già esistente.
072100060928         IF tibs2651s.opCode = 'NUOVO' OR tibs2651s.opCode = 'COPIA';
072200060510      /END-FREE
072300060510     C/EXEC SQL
072400060510     C+ SELECT COUNT(*) INTO :count
072500060927     C+ FROM AZNMRN00F
072600060927     C+ WHERE CODICE = :codice AND SOCIETA = :societa AND UNITA = :unita AND
072700060927     C+       KEYGEST = :KeyGest AND DATAPERIOD = :dataPeriod AND
072800060927     C+       PROGRESSIV = :progressiv
072900060510     C/END-EXEC
073000060510      /FREE
073100060510           IF count > 0;
073200060510             errGenerico = *ON;
073300060928             errMsgNumeratoreEsiste = *ON;
073400060928             p01MsgId37 = 'Il numeratore esiste già. +
073500060928             Cambiare la chiave del numeratore.';
073600060510           ENDIF;
073700060510         ENDIF;
073800060928
073900060928         IF gruOSoc = PerSocieta;
074000060928      /END-FREE
074100060928     C/EXEC SQL
074200060928     C+ SELECT SIFDESBREV INTO :sifDesBrev
074300060928     C+ FROM ANSIF00F
074400060928     C+ WHERE SIFSOCIETA = :societa
074500060928     C/END-EXEC
074600060928      /FREE
074700060928           IF sqlCode = 100;
074800060928             errGenerico = *ON;
074900060928             errMsgSocieta = *ON;
075000060928           ENDIF;
075100060928         ENDIF;
075200060928
075300060928         IF livelloPO <> POTutti;
075400060928      /END-FREE
075500060928     C/EXEC SQL
075600060928     C+ SELECT UNIDESBREV INTO :uniDesBrev
075700060928     C+ FROM ANUNI00F
075800060928     C+ WHERE UNISOCIETA = :societa AND UNIUNITA = :unita
075900060928     C/END-EXEC
076000060928      /FREE
076100060928           IF sqlCode = 100;
076200060928             errGenerico = *ON;
076300060928             errMsgUnita = *ON;
076400060928           ENDIF;
076500060928         ENDIF;
076600060928
076700060928         IF temporizza = Progressivo AND tibs2651s.opCode = 'NUOVO' AND
076800060928         progressiv < 0;
076900060928           errGenerico = *ON;
077000060928           errMsgProgressivo = *ON;
077100060928           p01MsgId35 = 'Il progressivo non può essere negativo.';
077200060928         ENDIF;
077300060928
077400060928         IF desKeyGest <> *BLANK AND keyGest = *BLANK;
077500060928           errGenerico = *ON;
077600060928           errMsgKeyGest = *ON;
077700060928           p01MsgId36 = 'Immettere ' + %TRIMR(desKeyGest) + '.';
077800060928         ENDIF;
077900061013
078000060927         IF valNInizio = 0; // Impostazione automatica del valore iniziale.
078100060928           valNInizio = rValNInizio;
078200060927         ELSE;
078300060927           IF %LEN(%CHAR(valNInizio)) > lunghezza;
078400060927             errGenerico = *ON;
078500060927             errMsgValNIni = *ON;
078600060927             p01ValNIni = 'Immettere un valore iniziale congruente con la +
078700060927             lunghezza del numero.';
078800060927           ENDIF;
078900061013           IF valNInizio < rValNInizio;
079000061013             errGenerico = *ON;
079100061013             errMsgValNIni = *ON;
079200061013             p01ValNIni = 'Il valore iniziale non può essere minore di ' +
079300061013             %CHAR(rValNInizio);
079400061013           ENDIF;
079500060927         ENDIF;
079600060928
079700060927         IF valNFine = 0; // Impostazione automatica del valore finale.
079800060928           valNFine = rValNFine;
079900060927         ELSE;
080000060927           IF %LEN(%CHAR(valNFine)) > lunghezza;
080100060927             errGenerico = *ON;
080200060927             errMsgValNFin = *ON;
080300060927             p01ValNFin = 'Immettere un valore finale congruente con la +
080400060927             lunghezza del numero.';
080500060927           ENDIF;
080600061013           IF valNFine > rValNFine;
080700061013             errGenerico = *ON;
080800061013             errMsgValNFin = *ON;
080900061013             p01ValNFin = 'Il valore finale non può essere maggiore di ' +
081000061013             %CHAR(rValNFine);
081100061013           ENDIF;
081200060927         ENDIF;
081300060928
081400060927         IF valNInizio > valNFine;
081500060927           errGenerico = *ON;
081600060927           errMsgValNIni = *ON;
081700060927           p01ValNIni = 'Immettere un valore iniziale minore del valore +
081800060927           finale.';
081900060927           errMsgValNFin = *ON;
082000060927           p01ValNFin = 'Immettere un valore finale maggiore del valore +
082100060927           iniziale.';
082200060927         ENDIF;
082300060928
082400060928         IF sogliaNRis = 0;
082500060928           sogliaNRis = rSogliaNRis;
082600060928         ENDIF;
082700060927         IF sogliaNRis > 0 AND sogliaNRis > valNFine;
082800060927           errGenerico = *ON;
082900060927           errMsgSogliaN = *ON;
083000060927           p01SogliaN = 'Immettere una soglia di rischio minore +
083100060927           del valore finale.';
083200060927         ENDIF;
083300061013
083400061013         IF numero > 0;
083500061013           SELECT;
083600061013             WHEN numero < valNInizio;
083700061013               errGenerico = *ON;
083800061013               errMsgNumero = *ON;
083900061013               p01Numero = 'Il numero non può essere inferiore a ' +
084000061013               %CHAR(valNInizio);
084100061013             WHEN numero > valNFine;
084200061013               errGenerico = *ON;
084300061013               errMsgNumero = *ON;
084400061013               p01Numero = 'Il numero non può essere superiore a ' +
084500061013               %CHAR(valNFine);
084600061013           ENDSL;
084700061013         ENDIF;
084800060920
084900060915         // Non ci sono errori e l'utente ha premuto F6: aggiorno regola.
085000060505         IF NOT errGenerico AND dsp_aid = F6;
085100060915           opCode = 'UPDDB';
085200060131         ENDIF;
085300060130
085400060131       ENDSR;
085500060428
085600060428       //*******************************************************************************************
085700060428       //
085800060428       // Gestione richiesta.
085900060428       //
086000060428       //*******************************************************************************************
086100060428       BEGSR richiesta;
086200060428       ENDSR;
086300060428
086400060428       //*******************************************************************************************
086500060428       //
086600060927       // Aggiornamento numeratore.
086700060428       //
086800060428       //*******************************************************************************************
086900060927       BEGSR aggiornaNumeratore;
087000060505
087100060928         IF tibs2651s.opCode = 'VISUALIZZA';
087200060508           LEAVESR;
087300060508         ENDIF;
087400060511
087500060918         // Cancellazione della regola, di tutti i suoi numeratori e dei commenti.
087600060928         IF tibs2651s.opCode = 'CANCELLA';
087700060927           DELETE aznmrn000;
087800060511           LEAVESR;
087900060511         ENDIF;
088000060505
088100060928         IF tibs2651s.opCode = 'NUOVO' OR tibs2651s.opCode = 'COPIA';
088200060927           WRITE aznmrn000;
088300060505         ELSE;
088400060927           UPDATE aznmrn000;
088500060505         ENDIF;
088600060428
088700060428       ENDSR;
088800060505
088900060505       //*******************************************************************************************
089000060505       //
089100060505       // Gestione errori.
089200060505       //
089300060505       //*******************************************************************************************
089400060505       BEGSR errore;
089500060505
089600060505         CLEAR qusec;
089700060505         qusbprv = %SIZE(qusec);
089800060505         DisplayLongText(TextString:LengthOfTextString:MessageID
089900060505         :QualifiedMessageFileName:qusec);
090000060505
090100060505       ENDSR;
090200060407
090300060131       //*******************************************************************************************
090400060131       //
090500060131       // Uscita.
090600060131       //
090700060131       //*******************************************************************************************
090800060131       BEGSR uscita;
090900060131
091000070612         IF %OPEN(tibs2651d);
091100070612           WRITE frcDta;
091200070612         ENDIF;
091300060426
091400060928         IF tibs2651s.rtnCode = ' ';
091500060928           tibs2651s.rtnCode = 'DONE';
091600060131         ENDIF;
091700060428
091800060928         kpjbu = tibs2651s;
091900060928         *INLR = (tibs2651s.setOnLR = *ON);
092000060131         RETURN;
092100060131
092200060131       ENDSR;
092300060131
092400060131      /END-FREE
