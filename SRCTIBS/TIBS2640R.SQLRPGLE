000100060926      //********************************************************************************************
000200060330      //
000300060926      // Questo programma visualizza l'elenco dei numeratori AZNMRN00F.
000400060330      //
000500060330      //********************************************************************************************
000600060926     H DFTACTGRP(*NO) ACTGRP('NUMERATORI')
000700060915     H BNDDIR('PJXBND':'QC2LE':'TIBS')
000800060207     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000900060127
001000060330      //********************************************************************************************
001100060330      //
001200060330      // Definizione files.
001300060330      //
001400060330      //********************************************************************************************
001500060926     Ftibs2640d CF   E             WORKSTN
001600060320     F                                     USROPN
001700060130     F                                     INFDS(infDspF)
001800060127     F                                     INDDS(indDspF)
001900060130     F                                     SFILE(s01:s01Rrn)
002000060926     Faznmrr01l IF   E           K DISK
002100060926     F                                     USROPN
002200060926     F                                     PREFIX(r)
002300060127
002400060330      //********************************************************************************************
002500060330      //
002600060330      // Definizione costanti.
002700060330      //
002800060330      //********************************************************************************************
002900060127     D F3              C                   X'33'
003000060127     D F5              C                   X'35'
003100060927     D F6              C                   X'36'
003200060927     D F8              C                   X'38'
003300060427     D F10             C                   X'3A'
003400060127     D F12             C                   X'3C'
003500060127     D Enter           C                   X'F1'
003600060403     D Normal          C                   X'20'
003700060927     D Underscore      C                   X'24'
003800060927     D UnderscoreReverseImage...
003900060927     D                 C                   X'25'
004000060404     D HighIntensity   C                   X'22'
004100060314     D Blink           C                   X'28'
004200060414     D RighePagina     C                   15
004300060927     D OpzScelta...
004400060927     D                 C                   1
004500060414     D OpzModifica...
004600060414     D                 C                   2
004700060427     D OpzCopia...
004800060403     D                 C                   3
004900060511     D OpzCancella...
005000060926     D                 C                   4
005100060927     D OpzVisualizza...
005200060927     D                 C                   5
005300060926     D Giorno          C                   'G'
005400060926     D Mese            C                   'M'
005500060926     D Anno            C                   'A'
005600060926     D Esercizio       C                   'E'
005700060926     D Progressivo     C                   'P'
005800060926     D Assoluto        C                   'T'
005900060926     D DiGruppo        C                   'G'
006000060926     D PerSocieta      C                   'S'
006100060926     D POTutti         C                   '0'
006200060926     D POLivello1      C                   '1'
006300060926     D POLivello2      C                   '2'
006400060926     D NessunaRegolaAreaPO...
006500060926     D                 C                   '0'
006600060926     D Numerico        C                   'N'
006700060926     D Carattere       C                   'C'
006800060926     D SoloNumeri      C                   'N'
006900060926     D SoloLettere     C                   'L'
007000060926     D NumeriELettere  C                   'T'
007100060926
007200060330      //********************************************************************************************
007300060330      //
007400060330      // Definizione delle procedure esterne usate.
007500060330      //
007600060330      //********************************************************************************************
007700060222     D xSoc            PR
007800060222     D                                     EXTPROC('XSOC')
007900060222     D  tipXsc                        6A
008000060222     D                                     CONST
008100060222     D  socXsc
008200060222     D                                     LIKE(pjzSocieta)
008300060222     D  cdsXsc                        9P 0
008400060222     D                                     CONST
008500060222     D  modXsc                        3A
008600060222     D                                     CONST
008700060222     D  rtnXsc                        1A
008800060921     D  xSoc001DS
008900060921     D                                     LIKEDS(xSoc001DS)
009000060222     D  kpjba
009100060222     D                                     LIKEDS(kpjba)
009200060921     D xChkAut         PR
009300060921     D                                     EXTPROC('XCHKAUT')
009400060921     D  xChkAutDS
009500060921     D                                     LIKEDS(xChkAutDS)
009600060331     D tibs34r         PR
009700060131     D                                     EXTPGM('TIBS34R')
009800060131     D  tibs34ds
009900060131     D                                     LIKEDS(tibs34ds)
010000060426     D atoi            PR            10I 0
010100060222     D                                     EXTPROC('atoi')
010200060222     D  char                           *
010300060222     D                                     VALUE
010400060222     D                                     OPTIONS(*STRING)
010500060509     D ana725r         PR
010600060509     D                                     EXTPGM('ANA725R')
010700060509     D  kpjba
010800060509     D                                     LIKEDS(kpjba)
010900060512     D displayLongText...
011000060512     D                 PR
011100060512     D                                     EXTPGM('QUILNGTX')
011200060512     D  textString...
011300060512     D                              256A
011400060512     D                                     CONST
011500060512     D                                     OPTIONS(*VARSIZE)
011600060512     D  lengthOfTextString...
011700060512     D                               10I 0
011800060512     D                                     CONST
011900060512     D  messageID...
012000060512     D                                7A
012100060512     D                                     CONST
012200060512     D  qualifiedMessageFileName...
012300060512     D                               20A
012400060512     D                                     CONST
012500060512     D  errorCode...
012600060512     D                                     LIKE(qusec)
012700060512     D                                     OPTIONS(*VARSIZE)
012800060927     D tibs2651r       PR
012900060927     D                                     EXTPGM('TIBS2651R')
013000060927     D  kpjba
013100060927     D                                     LIKEDS(kpjba)
013200060927
013300060330      //********************************************************************************************
013400060330      //
013500060330      // Definizione strutture dati.
013600060330      //
013700060330      //********************************************************************************************
013800060330     D psds           SDS
013900060131     D  pgmName          *PROC
014000060130     D infDspF         DS
014100060222     D  dsp_aid              369    369A                                        AID byte
014200060206     D  sf_rrn               376    377I 0                                      Subfile rrn
014300060206     D indDspF         DS
014400060130     D  sflDsp                 1      1N
014500060130     D  sflClr                 2      2N
014600060222     D  sflEnd                 3      3N
014700060926     D  errMsgOpzione         30     30N
014800060427     D  errGenerico           99     99N
014900060404     D kpjba         E DS
015000060921     D xSoc001DS     E DS
015100060414     D                                     INZ
015200060921     D xChkAutDS     E DS
015300060921     D                                     INZ
015400060921     D  xcaFnc       E
015500060921     D                                     INZ('YAZNMR')
015600060921     D  xcaVfu       E
015700060926     D                                     INZ(*ZERO)
015800060921     D  xcaTck       E
015900060921     D                                     INZ('CK')
016000060921     D  xcaNau       E
016100060921     D                                     INZ('1')
016200060414     D cndizion      E DS
016300060130     D                                     BASED(nullPtr)
016400060130     D                                     PREFIX(diz)
016500060222     D andiz00f      E DS
016600060222     D                                     BASED(nullPtr)
016700060222     D                                     PREFIX(pjz)
016800060926     D tibs2640s     E DS
016900060914     D                                     INZ
017000060914     D                                     QUALIFIED
017100060927     D tibs2651s     E DS
017200060927     D                                     INZ
017300060927     D                                     QUALIFIED
017400060414     D tibs34ds      E DS
017500060414     D                                     INZ
017600060926     D aznmrn00f     E DS
017700060427     D                                     INZ
017800060926     D                                     PREFIX(n)
017900060509     D ana725ds      E DS
018000060509     D                                     INZ
018100060509     D  tpa725       E
018200060921     D                                     INZ('YNU')
018300060509     D  cmt725       E
018400060509     D                                     INZ(*OFF)
018500060131
018600060330      //********************************************************************************************
018700060330      //
018800060330      // Definizione variabili.
018900060330      //
019000060330      //********************************************************************************************
019100060512     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
019200060512     D/COPY QSYSINC/QRPGLESRC,QUSEC
019300060512     D nullPtr         S               *
019400060130     D opCode          S             10
019500060221     D                                     INZ('INZC01')
019600060414     D rtnXsc          S              1A
019700060414     D s01Rrn          S              5I 0
019800060223     D s01RrnLast      S              5I 0
019900060403     D wrkRrn          S              5I 0
020000060317     D i               S              5I 0
020100060222     D p               S              5I 0
020200060228     D esito           S             10I 0
020300060228     D elementi        S             10I 0
020400060202     D wrkErrS01       S               N
020500060303     D stop            S               N
020600060207     D wrkCsrRrn       S              5I 0
020700060207     D                                     INZ(1)
020800060427     D oxx             S
020900060427     D                                     LIKE(s01Opz)
021000060511     D                                     DIM(6)
021100061018     D                                     DESCEND
021200060427     D fxx             S
021300060427     D                                     LIKE(dsp_aid)
021400060927     D                                     DIM(5)
021500061018     D                                     DESCEND
021600060512     D textString...
021700060512     D                 S            256A
021800060512     D lengthOfTextString...
021900060512     D                 S             10I 0
022000060512     D                                     INZ(%SIZE(textString))
022100060512     D messageID...
022200060512     D                 S              7A
022300060512     D                                     INZ('BAR0019')
022400060512     D qualifiedMessageFileName...
022500060512     D                 S             20A
022600060512     D                                     INZ('YBARMSG   *LIBL')
022700060926     D wrkDati         S             75A
022800060926     D                                     VARYING
022900060512
023000060330      //********************************************************************************************
023100060330      //
023200060330      // Definizione aree dati.
023300060330      //
023400060330      //********************************************************************************************
023500060201     D §azute        E DS
023600060201     D                                     EXTNAME(azute00f)
023700060201     D                                     DTAARA
023800060201     D §DatiUte      E DS
023900060201     D                                     EXTNAME(dDatiUte)
024000060201     D                                     DTAARA
024100060403
024200060330      //********************************************************************************************
024300060330      //
024400060330      // Definizione parametri procedura.
024500060330      //
024600060330      //********************************************************************************************
024700060131     C     *ENTRY        PLIST
024800060131     C                   PARM                    kpjba
024900060127
025000060127      /FREE
025100060130
025200060131       EXSR inzProc;
025300060130
025400060130       DOU opCode = 'END';
025500060130         SELECT;
025600060221           WHEN opCode = 'INZC01';
025700060221             EXSR inzC01;
025800060130           WHEN opCode = 'LODC01';
025900060131             EXSR lodC01;
026000060130           WHEN opCode = 'PUTC01';
026100060131             EXSR putC01;
026200060131           WHEN opCode = 'CHKC01';
026300060427             EXSR chkC01;
026400060427           WHEN opCode = 'RUNC01';
026500060427             EXSR runC01;
026600060926           WHEN opCode = 'NUOVONUMER';
026700060926             EXSR nuovoNumeratore;
026800060330           WHEN opCode = 'ERRORE';
026900060512             EXSR errore;
027000060512             opCode = 'END';
027100060512           WHEN opCode = 'END';
027200060512             LEAVE;
027300060130           OTHER;
027400060512             textString = 'Codice operativo ' + %TRIMR(opCode)
027500060512             + ' sconosciuto.';
027600060512             opCode = 'ERRORE';
027700060512         ENDSL;
027800060130       ENDDO;
027900060130
028000060130       EXSR uscita;
028100060323
028200060323       //*******************************************************************************************
028300060323       //
028400060323       // Operazioni iniziali da eseguire una tantum.
028500060323       //
028600060323       //*******************************************************************************************
028700060323       BEGSR *INZSR;
028800060323
028900060330      /END-FREE
029000060330     C/EXEC SQL
029100060926     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER, DATFMT = *ISO,
029200060926     C+            TIMFMT = *ISO, COMMIT = *NONE
029300060330     C/END-EXEC
029400060330      /FREE
029500060330
029600060323         IN(E) §azute;
029700060323         IF NOT %ERROR;
029800060323           IN(E) §DatiUte;
029900060323         ENDIF;
030000060323         IF %ERROR OR rsut = ' ';
030100060323           tibs34r(tibs34ds);
030200060323           IN §azute;
030300060323           IN §DatiUte;
030400060323         ENDIF;
030500060427
030600060927         p01Opz = Underscore;
030700060927
030800060407       ENDSR;
030900060131
031000060131       //*******************************************************************************************
031100060131       //
031200060131       // Operazioni iniziali.
031300060131       //
031400060131       //*******************************************************************************************
031500060131       BEGSR inzProc;
031600060330
031700060330         RESET opCode;
031800060131
031900060131         EXSR chkParm;
032000060222
032100060222         CLEAR kpjbu;
032200060921         CLEAR xSoc001DS;
032300060921         xSoc('SOC001':xscSoc:0:*BLANK:rtnXsc:xSoc001DS:kpjba);
032400060222         IF rtnXsc = *ON;
032500060928           tibs2640s.rtnCode = 'XSOC';
032600060926           opCode = 'END';
032700060330           LEAVESR;
032800060222         ENDIF;
032900060308
033000060927         // Controllo autorizzazione generale alla gestione dei numeratori.
033100060921         RESET xChkAutDS;
033200060921         xcaSoc = xscSoc;
033300060921         xcaGrp = xscGrp;
033400060921         xcaPrf = kNmUs;
033500060921         xChkAut(xChkAutDS);
033600060921         IF xcaRtn <> 0;
033700060928           tibs2640s.rtnCode = 'XCHKAUT';
033800060921           opCode = 'END';
033900060921           LEAVESR;
034000060921         ENDIF;
034100060921
034200060927         // Controllo autorizzazione specifica alla gestione del numeratore.
034300060927         RESET xChkAutDS;
034400060927         xcaSoc = xscSoc;
034500060927         xcaGrp = xscGrp;
034600060927         xcaPrf = kNmUs;
034700060927         CLEAR xcaNau;
034800060927         xcaVfu = 'O11';
034900060927         xChkAut(xChkAutDS);
035000060927         IF xcaRtn <> 0;
035100060927           RESET xChkAutDS;
035200060927           xcaSoc = xscSoc;
035300060927           xcaGrp = xscGrp;
035400060927           xcaPrf = kNmUs;
035500060927           xcaVfu = 'O11_' + tibs2640s.codice;
035600060927           xChkAut(xChkAutDS);
035700060927           IF xcaRtn <> 0;
035800060927             opCode = 'END';
035900060927             LEAVESR;
036000060927           ENDIF;
036100060927         ENDIF;
036200060927
036300060926         OPEN(E) aznmrr01l;
036400060926         CHAIN tibs2640s.codice aznmrr01l;
036500060926         CLOSE(E) aznmrr01l;
036600060927
036700060131       ENDSR;
036800060131
036900060131       //*******************************************************************************************
037000060131       //
037100060131       // Controllo dei parametri ricevuti.
037200060131       //
037300060131       //*******************************************************************************************
037400060131       BEGSR chkParm;
037500060414
037600060928         tibs2640s = kpjbu;
037700060928         CLEAR tibs2640s.rtnCmd;
037800060928         CLEAR tibs2640s.rtnCode;
037900060928         CLEAR tibs2640s.rtnEsito;
038000060426
038100060426         SELECT;
038200060928           WHEN tibs2640s.opCode = 'GESTIONE';
038300060928           WHEN tibs2640s.opCode = 'RICERCA';
038400060426         ENDSL;
038500060414
038600060131       ENDSR;
038700060221
038800060221       //*******************************************************************************************
038900060221       //
039000060330       // Inizializzo subfile movimenti.
039100060221       //
039200060221       //*******************************************************************************************
039300060221       BEGSR inzC01;
039400060320
039500060330         opCode = 'LODC01';
039600060927
039700060926         IF NOT %OPEN(tibs2640d);
039800060926           OPEN tibs2640d;
039900060320         ENDIF;
040000060927
040100060927         w01Titolo = 'Numeratori ' + tibs2640s.codice;
040200060927         WRITE w01;
040300060927
040400060221         sflDsp = *OFF;
040500060221         sflClr = *ON;
040600060221         WRITE c01;
040700060221         sflClr = *OFF;
040800060221         CLEAR s01Rrn;
040900060223         CLEAR s01RrnLast;
041000060221         RESET wrkCsrRrn;
041100060331
041200060926         CLEAR wrkDati;
041300060926
041400060926         IF rGruOSoc = PerSocieta;
041500060926           wrkDati += 'Soc ';
041600060926         ENDIF;
041700060926
041800060926         IF rLivelloPO <> POTutti;
041900060926           wrkDati += ' PO ';
042000060926         ENDIF;
042100060926
042200060926         IF rDesKeyGest <> *BLANK;
042300060926           wrkDati += 'Chiave     ';
042400060926         ENDIF;
042500060926
042600060926         SELECT;
042700060926           WHEN rTemporizza = Progressivo;
042800060926             wrkDati += 'Prg  ';
042900060926           WHEN rTemporizza = Assoluto;
043000060926           OTHER;
043100060926             wrkDati += 'Periodo    ';
043200060926         ENDSL;
043300060926
043400060926         wrkDati += '         Numero';
043500060926         c01ColHdg = wrkDati;
043600060926
043700060221       ENDSR;
043800060201
043900060201       //*******************************************************************************************
044000060201       //
044100060414       // Carico elenco modelli.
044200060201       //
044300060201       //*******************************************************************************************
044400060201       BEGSR lodC01;
044500060201
044600060330         opCode = 'PUTC01';
044700060426
044800060426      /END-FREE
044900060426     C/EXEC SQL
045000060926     C+ DECLARE NUMERATORI CURSOR FOR
045100060926     C+ SELECT RRN(AZNMRN00F), AZNMRN00F.*
045200060926     C+ FROM AZNMRN00F
045300060929     C+ WHERE CODICE = :tibs2640s.codice AND PROGRESSIV >= 0
045400060927     C+ ORDER BY CODICE, SOCIETA, UNITA, KEYGEST, DATAPERIOD DESC, PROGRESSIV
045500060926     C+ FOR READ ONLY
045600060926     C/END-EXEC
045700060426     C
045800060426     C/EXEC SQL
045900060926     C+ OPEN NUMERATORI
046000060426     C/END-EXEC
046100060426      /FREE
046200060330
046300060921         IF sqlCode < 0;
046400060928           tibs2640s.rtnCode = 'SQLCODE';
046500060928           tibs2640s.rtnEsito = sqlCode;
046600060928           textString = 'Codice ritorno ' + %TRIMR(tibs2640s.rtnCode)
046700060928           + ' esito ' + %CHAR(tibs2640s.rtnEsito) +
046800060512           '. Premere invio per continuare.';
046900060330           opCode = 'ERRORE';
047000060330           LEAVESR;
047100060330         ENDIF;
047200060330
047300060414         EXSR inzS01;
047400060414
047500060921         DOU sqlCode = 100;
047600060330      /END-FREE
047700060330     C/EXEC SQL
047800060926     C+ FETCH NUMERATORI
047900060926     C+ INTO :h01RrnNmrN, :nCodice, :nSocieta, :nUnita, :nKeyGest,
048000060926     C+      :nDataPeriod, :nProgressiv, :nNumero, :nDataUltimo, :nValNInizio,
048100060926     C+      :nValNFine, :nSogliaNRis
048200060330     C/END-EXEC
048300060330      /FREE
048400060330           SELECT;
048500060921             WHEN sqlCode = 100;
048600060330               sflEnd = *ON;
048700060330               LEAVE;
048800060921             WHEN sqlCode < 0;
048900060928               tibs2640s.rtnCode = 'SQLCODE';
049000060928               tibs2640s.rtnEsito = sqlCode;
049100060330               opCode = 'ERRORE';
049200060330               LEAVE;
049300060330             OTHER;
049400060330               EXSR setS01;
049500060331               EXSR wrtS01;
049600060330           ENDSL;
049700060330         ENDDO;
049800060330
049900060330      /END-FREE
050000060330     C/EXEC SQL
050100060926     C+ CLOSE NUMERATORI
050200060330     C/END-EXEC
050300060330      /FREE
050400060330
050500060330         s01RrnLast = s01Rrn;
050600060330
050700060201       ENDSR;
050800060330
050900060330       //*******************************************************************************************
051000060330       //
051100060330       // Inizializzo riga subfile movimenti.
051200060330       //
051300060330       //*******************************************************************************************
051400060330       BEGSR inzS01;
051500060330         CLEAR s01;
051600060330       ENDSR;
051700060330
051800060330       //*******************************************************************************************
051900060330       //
052000060405       // Imposto riga subfile movimenti come definito nel modello.
052100060330       //
052200060330       //*******************************************************************************************
052300060330       BEGSR setS01;
052400060927
052500060926         CLEAR wrkDati;
052600060926
052700060926         IF rGruOSoc = PerSocieta;
052800060926           wrkDati += nSocieta + ' ';
052900060926         ENDIF;
053000060926
053100060926         IF rLivelloPO <> POTutti;
053200060926           wrkDati += %SUBST(nUnita:1:3) + ' ';
053300060926         ENDIF;
053400060926
053500060926         IF rDesKeyGest <> *BLANK;
053600060926           wrkDati += nKeyGest + ' ';
053700060926         ENDIF;
053800060926
053900060926         SELECT;
054000060926           WHEN rTemporizza = Progressivo;
054100060926             wrkDati += %EDITC(nProgressiv:'L') + ' ';
054200060926           WHEN rTemporizza = Assoluto;
054300060926           OTHER;
054400060926             wrkDati += %CHAR(nDataPeriod) + ' ';
054500060926         ENDSL;
054600060926
054700060926         wrkDati += %EDITC(nNumero:'3');
054800060926         s01Dati = wrkDati;
054900060926
055000060330       ENDSR;
055100060331
055200060331       //*******************************************************************************************
055300060331       //
055400060331       // Scrittura riga subfile movimenti.
055500060331       //
055600060331       //*******************************************************************************************
055700060331       BEGSR wrtS01;
055800060331         s01Rrn += 1;
055900060331         WRITE s01;
056000060331       ENDSR;
056100060130
056200060131       //*******************************************************************************************
056300060131       //
056400060131       // Emissione subfile spedizioni.
056500060131       //
056600060131       //*******************************************************************************************
056700060131       BEGSR putC01;
056800060130
056900060403         opCode = 'PUTC01';
057000060928         WRITE w01;
057100060427         RESET oxx;
057200060427         RESET fxx;
057300060427
057400060427         SELECT;
057500060928           WHEN tibs2640s.opCode = 'GESTIONE';
057600060927             c01Opzion1 = '2=Modifica  3=Copia  4=Cancella';
057700060927             c01Opzion2 = '5=Visualizza';
057800061018             oxx(4) = OpzModifica;
057900061018             oxx(3) = OpzCopia;
058000061018             oxx(2) = OpzCancella;
058100061018             oxx(1) = OpzVisualizza;
058200060927             f01Tasti1 = 'F3=Fine  F5=Ricarica  F8=Successivo';
058300060927             f01Tasti2 = 'F10=Nuovo numeratore  F12=Ritorno';
058400061018             fxx(5) = F3;
058500061018             fxx(4) = F5;
058600061018             fxx(3) = F8;
058700061018             fxx(2) = F10;
058800061018             fxx(1) = F12;
058900060928           WHEN tibs2640s.opCode = 'RICERCA';
059000060427         ENDSL;
059100060427
059200060131         WRITE f01;
059300060404
059400060207         SELECT;
059500060207           WHEN wrkCsrRrn > 0;
059600060207             c01RcdNbr = wrkCsrRrn;
059700060207             CLEAR wrkCsrRrn;
059800060207           WHEN c01CsrRrn > 0;
059900060207             c01RcdNbr = c01CsrRrn;
060000060207           OTHER;
060100060207             c01RcdNbr = 1;
060200060207         ENDSL;
060300060131
060400060427         sflDsp = (s01RrnLast > 0);
060500060131         EXFMT c01;
060600060130
060700060928         tibs2640s.rtnCmd = dsp_aid;
060800060130
060900060131         SELECT;
061000060927           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
061100060414             opCode = 'END';
061200060131           WHEN dsp_aid = F5;
061300060221             opCode = 'INZC01';
061400060427           WHEN dsp_aid = Enter;
061500060131             opCode = 'CHKC01';
061600060427           WHEN dsp_aid = F10;
061700060926             opCode = 'NUOVONUMER';
061800060206         ENDSL;
061900060130
062000060131       ENDSR;
062100060130
062200060131       //*******************************************************************************************
062300060131       //
062400060427       // Lettura subfile per controlli.
062500060131       //
062600060131       //*******************************************************************************************
062700060427       BEGSR chkC01;
062800060130
062900060403         opCode = 'PUTC01';
063000060427         RESET errGenerico;
063100060207         CLEAR wrkCsrRrn;
063200060303         RESET stop;
063300060130
063400060427         EXSR chkC01Fxx;
063500060427         IF errGenerico;
063600060427           LEAVESR;
063700060427         ENDIF;
063800060427
063900060403         FOR wrkRrn = 1 TO s01RrnLast;
064000060403           CHAIN wrkRrn s01;
064100060403           IF NOT %FOUND;
064200060221             LEAVE;
064300060221           ENDIF;
064400060427           EXSR chkS01;
064500060427           EXSR updS01;
064600060427           IF wrkErrS01;
064700060427             errGenerico = *ON;
064800060427             IF wrkCsrRrn = 0;
064900060427               wrkCsrRrn = s01Rrn;
065000060427             ENDIF;
065100060427           ENDIF;
065200060427           IF stop;
065300060427             LEAVE;
065400060404           ENDIF;
065500060403         ENDFOR;
065600060130
065700060427         IF NOT errGenerico AND dsp_aid = Enter;
065800060427           opCode = 'RUNC01';
065900060131         ENDIF;
066000060130
066100060131       ENDSR;
066200060427
066300060427       //*******************************************************************************************
066400060427       //
066500060427       // Controllo tasto funzionale premuto.
066600060427       //
066700060427       //*******************************************************************************************
066800060427       BEGSR chkC01Fxx;
066900060427
067000060427         IF dsp_aid <> Enter;
067100061018           IF %LOOKUP(dsp_aid:fxx:1) = 0;
067200060427             errGenerico = *ON;
067300060427           ENDIF;
067400060427         ENDIF;
067500060427
067600060427       ENDSR;
067700060427
067800060427       //*******************************************************************************************
067900060427       //
068000060427       // Controllo riga subfile.
068100060427       //
068200060427       //*******************************************************************************************
068300060427       BEGSR chkS01;
068400060427
068500060427         RESET wrkErrS01;
068600060427         EXSR chkS01Oxx;
068700060427
068800060427       ENDSR;
068900060403
069000060403       //*******************************************************************************************
069100060403       //
069200060403       // Controllo opzione immessa su riga subfile.
069300060403       //
069400060403       //*******************************************************************************************
069500060427       BEGSR chkS01Oxx;
069600060404
069700060511         RESET p01Opz;
069800060404         IF s01Opz <> 0;
069900061018           IF %LOOKUP(s01Opz:oxx:1) = 0;
070000060427             wrkErrS01 = *ON;
070100060926             errMsgOpzione = *ON;
070200060926             errGenerico = *ON;
070300060927             p01Opz = UnderscoreReverseImage;
070400060427           ENDIF;
070500060404         ENDIF;
070600060403
070700060403       ENDSR;
070800060427
070900060427       //*******************************************************************************************
071000060427       //
071100060427       // Lettura subfile per elaborazione delle opzioni immesse.
071200060427       //
071300060427       //*******************************************************************************************
071400060427       BEGSR runC01;
071500060427
071600060427         opCode = 'PUTC01';
071700060427         RESET stop;
071800060427
071900060427         FOR wrkRrn = 1 TO s01RrnLast;
072000060427           CHAIN wrkRrn s01;
072100060427           IF NOT %FOUND;
072200060427             LEAVE;
072300060427           ENDIF;
072400060427           EXSR runS01Opz;
072500060427           EXSR updS01;
072600060427           IF stop;
072700060427             LEAVE;
072800060427           ENDIF;
072900060427         ENDFOR;
073000060427
073100060427       ENDSR;
073200060928
073300060928       //*******************************************************************************************
073400060928       //
073500060928       // Aggiornamento riga subfile.
073600060928       //
073700060928       //*******************************************************************************************
073800060928       BEGSR updS01;
073900060928
074000060928         UPDATE s01;
074100060928
074200060928       ENDSR;
074300060427
074400060427       //*******************************************************************************************
074500060427       //
074600060928       // Nuovo numeratore.
074700060427       //
074800060427       //*******************************************************************************************
074900060926       BEGSR nuovoNumeratore;
075000060427
075100060928         RESET tibs2651s;
075200060928         tibs2651s.opCode = 'NUOVO';
075300060928         tibs2651s.codice = tibs2640s.codice;
075400060927         EXSR gestioneNumeratore;
075500060427
075600060427       ENDSR;
075700060427
075800060427       //*******************************************************************************************
075900060427       //
076000060427       // Elaborazione opzione immessa su riga subfile.
076100060427       //
076200060427       //*******************************************************************************************
076300060427       BEGSR runS01Opz;
076400060926
076500060927         IF s01Opz <> 0;
076600060927           RESET tibs2651s;
076700060927           tibs2651s.codice = tibs2640s.codice;
076800060927           tibs2651s.rrnAznmrn = h01RrnNmrN;
076900060927           SELECT;
077000060927             WHEN s01Opz = OpzScelta;
077100060928               tibs2640s.rrnAznmrn = h01RrnNmrN;
077200060927               stop = *ON;
077300060927               opCode = 'END';
077400060927             WHEN s01Opz = OpzModifica;
077500060928               tibs2651s.opCode = 'MODIFICA';
077600060927               EXSR gestioneNumeratore;
077700060927             WHEN s01Opz = OpzCopia;
077800060928               tibs2651s.opCode = 'COPIA';
077900060927               EXSR gestioneNumeratore;
078000060927             WHEN s01Opz = OpzCancella;
078100060928               tibs2651s.opCode = 'CANCELLA';
078200060927               EXSR gestioneNumeratore;
078300060927             WHEN s01Opz = OpzVisualizza;
078400060928               tibs2651s.opCode = 'VISUALIZZA';
078500060927               EXSR gestioneNumeratore;
078600060927             OTHER;
078700060927           ENDSL;
078800060927           CLEAR s01Opz;
078900060927         ENDIF;
079000060427
079100060427       ENDSR;
079200060427
079300060427       //*******************************************************************************************
079400060427       //
079500060928       // Gestione numeratore.
079600060427       //
079700060427       //*******************************************************************************************
079800060927       BEGSR gestioneNumeratore;
079900060926
080000060928         kpjbu = tibs2651s;
080100060927         tibs2651r(kpjba);
080200060928         tibs2651s = kpjbu;
080300060927
080400060928         IF tibs2651s.rtnCode = 'DONE';
080500060927           SELECT;
080600060928             WHEN tibs2651s.rtnCmd = F3;
080700060928               tibs2640s.rtnCode = tibs2651s.rtnCode;
080800060928               tibs2640s.rtnCmd = tibs2651s.rtnCmd;
080900060928               stop = *ON;
081000060927               opCode = 'END';
081100060928             WHEN tibs2651s.rtnCmd = F6;
081200060928               IF tibs2651s.opCode <> 'NUOVO';
081300060927                 opCode = 'INZC01';
081400060927               ENDIF;
081500060928             WHEN tibs2651s.rtnCmd = F12;
081600060928               stop = *ON;
081700060928               opCode = 'PUTC01';
081800060927             OTHER;
081900060927               opCode = 'PUTC01';
082000060927           ENDSL;
082100060927         ELSE;
082200060928           textString = 'Codice ritorno ' + %TRIMR(tibs2651s.rtnCode)
082300060928           + ' esito ' + %CHAR(tibs2651s.rtnEsito) +
082400060927           '. Premere invio per continuare.';
082500060927           opCode = 'ERRORE';
082600060927         ENDIF;
082700060427
082800060427       ENDSR;
082900060509
083000060509       //*******************************************************************************************
083100060509       //
083200060509       // Gestione commenti.
083300060509       //
083400060509       //*******************************************************************************************
083500060509       BEGSR gestioneCommenti;
083600060509
083700060509         RESET ana725ds;
083800060928         IF tibs2640s.opCode = 'GESTIONE';
083900060509           opz725 = '02';
084000060509         ELSE;
084100060509           opz725 = '05';
084200060509         ENDIF;
084300060927         ky1725 = tibs2640s.codice;
084400060509         kpjbu = ana725ds;
084500060509         ana725r(kpjba);
084600060509         ana725ds = kpjbu;
084700060509
084800060509       ENDSR;
084900060512
085000060512       //*******************************************************************************************
085100060512       //
085200060512       // Gestione errori.
085300060512       //
085400060512       //*******************************************************************************************
085500060512       BEGSR errore;
085600060512
085700060512         CLEAR qusec;
085800060512         qusbprv = %SIZE(qusec);
085900060512         displayLongText(textString:lengthOfTextString:messageID
086000060512         :qualifiedMessageFileName:qusec);
086100060512
086200060512       ENDSR;
086300060407
086400060131       //*******************************************************************************************
086500060131       //
086600060131       // Uscita.
086700060131       //
086800060131       //*******************************************************************************************
086900060131       BEGSR uscita;
087000060131
087100070612         IF %OPEN(tibs2640d);
087200070612           WRITE frcDta;
087300070612         ENDIF;
087400060426
087500060928         IF tibs2640s.rtnCode = ' ';
087600060928           tibs2640s.rtnCode = 'DONE';
087700060131         ENDIF;
087800060426
087900060426         SELECT;
088000060928           WHEN tibs2640s.opCode = 'GESTIONE';
088100060928             kpjbu = tibs2640s + tibs2640s;
088200060928           WHEN tibs2640s.opCode = 'RICERCA';
088300060928             kpjbu = tibs2640s;
088400060426         ENDSL;
088500060426
088600060928         *INLR = (tibs2640s.setOnLR = *ON);
088700060131         RETURN;
088800060131
088900060131       ENDSR;
089000060131
089100060131      /END-FREE
