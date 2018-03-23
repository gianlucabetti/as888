000100060331      //********************************************************************************************
000200060330      //
000300060414      // Questo programma visualizza l'elenco dei modelli di prima nota filiale.
000400060330      //
000500060330      //********************************************************************************************
000600060511     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700060414     H BNDDIR('PJXBND':'QC2LE':'PJXBND')
000800060207     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000900060127
001000060330      //********************************************************************************************
001100060330      //
001200060330      // Definizione files.
001300060330      //
001400060330      //********************************************************************************************
001500060414     Fycow0420d CF   E             WORKSTN
001600060320     F                                     USROPN
001700060130     F                                     INFDS(infDspF)
001800060127     F                                     INDDS(indDspF)
001900060130     F                                     SFILE(s01:s01Rrn)
002000060127
002100060330      //********************************************************************************************
002200060330      //
002300060330      // Definizione costanti.
002400060330      //
002500060330      //********************************************************************************************
002600060127     D F3              C                   X'33'
002700060127     D F5              C                   X'35'
002800060511     D F6              C                   X'36'
002900060427     D F10             C                   X'3A'
003000060127     D F12             C                   X'3C'
003100060127     D Enter           C                   X'F1'
003200060403     D Normal          C                   X'20'
003300060202     D ReverseImage    C                   X'21'
003400060404     D HighIntensity   C                   X'22'
003500060314     D Blink           C                   X'28'
003600060414     D RighePagina     C                   15
003700060427     D POSede...
003800060427     D                 C                   46
003900060427     D Sede...
004000060427     D                 C                   'S'
004100060427     D Livello1...
004200060427     D                 C                   '1'
004300060427     D Livello2...
004400060427     D                 C                   '2'
004500060414     D OpzScelta...
004600060414     D                 C                   1
004700060414     D OpzModifica...
004800060414     D                 C                   2
004900060427     D OpzCopia...
005000060403     D                 C                   3
005100060427     D OpzAnnulla...
005200060403     D                 C                   4
005300060427     D OpzVisualizza...
005400060427     D                 C                   5
005500060509     D OpzCommenti...
005600060509     D                 C                   14
005700060511     D OpzCancella...
005800060511     D                 C                   44
005900060404
006000060330      //********************************************************************************************
006100060330      //
006200060330      // Definizione delle procedure esterne usate.
006300060330      //
006400060330      //********************************************************************************************
006500060222     D xSoc            PR
006600060222     D                                     EXTPROC('XSOC')
006700060222     D  tipXsc                        6A
006800060222     D                                     CONST
006900060222     D  socXsc
007000060222     D                                     LIKE(pjzSocieta)
007100060222     D  cdsXsc                        9P 0
007200060222     D                                     CONST
007300060222     D  modXsc                        3A
007400060222     D                                     CONST
007500060222     D  rtnXsc                        1A
007600060222     D  xSoc001ds
007700060222     D                                     LIKEDS(xSoc001ds)
007800060222     D  kpjba
007900060222     D                                     LIKEDS(kpjba)
008000060331     D tibs34r         PR
008100060131     D                                     EXTPGM('TIBS34R')
008200060131     D  tibs34ds
008300060131     D                                     LIKEDS(tibs34ds)
008400060426     D fnlv55r         PR
008500060426     D                                     EXTPGM('FNLV55R')
008600060426     D  fnlv55ds
008700060426     D                                     LIKEDS(fnlv55ds)
008800060426     D atoi            PR            10I 0
008900060222     D                                     EXTPROC('atoi')
009000060222     D  char                           *
009100060222     D                                     VALUE
009200060222     D                                     OPTIONS(*STRING)
009300060427     D ycow0431r       PR
009400060427     D                                     EXTPGM('YCOW0431R')
009500060427     D  kpjba
009600060427     D                                     LIKEDS(kpjba)
009700060427     D ycow0432r       PR
009800060427     D                                     EXTPGM('YCOW0432R')
009900060427     D  kpjba
010000060427     D                                     LIKEDS(kpjba)
010100060509     D ana725r         PR
010200060509     D                                     EXTPGM('ANA725R')
010300060509     D  kpjba
010400060509     D                                     LIKEDS(kpjba)
010500060512     D displayLongText...
010600060512     D                 PR
010700060512     D                                     EXTPGM('QUILNGTX')
010800060512     D  textString...
010900060512     D                              256A
011000060512     D                                     CONST
011100060512     D                                     OPTIONS(*VARSIZE)
011200060512     D  lengthOfTextString...
011300060512     D                               10I 0
011400060512     D                                     CONST
011500060512     D  messageID...
011600060512     D                                7A
011700060512     D                                     CONST
011800060512     D  qualifiedMessageFileName...
011900060512     D                               20A
012000060512     D                                     CONST
012100060512     D  errorCode...
012200060512     D                                     LIKE(qusec)
012300060512     D                                     OPTIONS(*VARSIZE)
012400060512
012500060330      //********************************************************************************************
012600060330      //
012700060330      // Definizione strutture dati.
012800060330      //
012900060330      //********************************************************************************************
013000060330     D psds           SDS
013100060131     D  pgmName          *PROC
013200060130     D infDspF         DS
013300060222     D  dsp_aid              369    369A                                        AID byte
013400060206     D  sf_rrn               376    377I 0                                      Subfile rrn
013500060206     D indDspF         DS
013600060130     D  sflDsp                 1      1N
013700060130     D  sflClr                 2      2N
013800060222     D  sflEnd                 3      3N
013900060403     D  errMsgOpzione         25     25N
014000060427     D  errGenerico           99     99N
014100060404     D kpjba         E DS
014200060414     D xsoc001ds     E DS
014300060414     D                                     INZ
014400060414     D cndizion      E DS
014500060130     D                                     BASED(nullPtr)
014600060130     D                                     PREFIX(diz)
014700060222     D andiz00f      E DS
014800060222     D                                     BASED(nullPtr)
014900060222     D                                     PREFIX(pjz)
015000060427     D ycow0400s     E DS
015100060426     D                                     INZ
015200060426     D                                     QUALIFIED
015300060426     D ycow0410s     E DS
015400060426     D                                     INZ
015500060426     D                                     QUALIFIED
015600060414     D ycow0420s     E DS
015700060414     D                                     INZ
015800060426     D                                     QUALIFIED
015900060427     D ycow0431s     E DS
016000060427     D                                     INZ
016100060427     D                                     QUALIFIED
016200060414     D tibs34ds      E DS
016300060414     D                                     INZ
016400060426     D fnlv55ds      E DS
016500060426     D                                     INZ
016600060427     D ynmrt00f      E DS
016700060427     D                                     INZ
016800060509     D ana725ds      E DS
016900060509     D                                     INZ
017000060509     D  tpa725       E
017100060509     D                                     INZ('YMP')
017200060509     D  cmt725       E
017300060509     D                                     INZ(*OFF)
017400060131
017500060330      //********************************************************************************************
017600060330      //
017700060330      // Definizione variabili.
017800060330      //
017900060330      //********************************************************************************************
018000060512     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
018100060512     D/COPY QSYSINC/QRPGLESRC,QUSEC
018200060512     D nullPtr         S               *
018300060130     D opCode          S             10
018400060221     D                                     INZ('INZC01')
018500060414     D rtnXsc          S              1A
018600060414     D s01Rrn          S              5I 0
018700060223     D s01RrnLast      S              5I 0
018800060403     D wrkRrn          S              5I 0
018900060317     D i               S              5I 0
019000060222     D p               S              5I 0
019100060228     D esito           S             10I 0
019200060228     D elementi        S             10I 0
019300060202     D wrkErrS01       S               N
019400060303     D stop            S               N
019500060207     D wrkCsrRrn       S              5I 0
019600060207     D                                     INZ(1)
019700060427     D wrkSqlStm       S            512A
019800060427     D                                     VARYING
019900060427     D wrkLivelloFgs   S
020000060427     D                                     LIKE(dutLpo)
020100060427     D                                     INZ(Sede)
020200060427     D oxx             S
020300060427     D                                     LIKE(s01Opz)
020400060511     D                                     DIM(6)
020500060427     D                                     ASCEND
020600060427     D fxx             S
020700060427     D                                     LIKE(dsp_aid)
020800060427     D                                     DIM(4)
020900060427     D                                     ASCEND
021000060512     D textString...
021100060512     D                 S            256A
021200060512     D lengthOfTextString...
021300060512     D                 S             10I 0
021400060512     D                                     INZ(%SIZE(textString))
021500060512     D messageID...
021600060512     D                 S              7A
021700060512     D                                     INZ('BAR0019')
021800060512     D qualifiedMessageFileName...
021900060512     D                 S             20A
022000060512     D                                     INZ('YBARMSG   *LIBL')
022100060512
022200060330      //********************************************************************************************
022300060330      //
022400060330      // Definizione aree dati.
022500060330      //
022600060330      //********************************************************************************************
022700060201     D §azute        E DS
022800060201     D                                     EXTNAME(azute00f)
022900060201     D                                     DTAARA
023000060201     D §DatiUte      E DS
023100060201     D                                     EXTNAME(dDatiUte)
023200060201     D                                     DTAARA
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
024800060221           WHEN opCode = 'INZC01';
024900060221             EXSR inzC01;
025000060130           WHEN opCode = 'LODC01';
025100060131             EXSR lodC01;
025200060130           WHEN opCode = 'PUTC01';
025300060131             EXSR putC01;
025400060131           WHEN opCode = 'CHKC01';
025500060427             EXSR chkC01;
025600060427           WHEN opCode = 'RUNC01';
025700060427             EXSR runC01;
025800060427           WHEN opCode = 'NUOVOMODEL';
025900060427             EXSR nuovoModello;
026000060330           WHEN opCode = 'ERRORE';
026100060512             EXSR errore;
026200060512             opCode = 'END';
026300060512           WHEN opCode = 'END';
026400060512             LEAVE;
026500060130           OTHER;
026600060512             textString = 'Codice operativo ' + %TRIMR(opCode)
026700060512             + ' sconosciuto.';
026800060512             opCode = 'ERRORE';
026900060512         ENDSL;
027000060130       ENDDO;
027100060130
027200060130       EXSR uscita;
027300060323
027400060323       //*******************************************************************************************
027500060323       //
027600060323       // Operazioni iniziali da eseguire una tantum.
027700060323       //
027800060323       //*******************************************************************************************
027900060323       BEGSR *INZSR;
028000060323
028100060330      /END-FREE
028200060330     C/EXEC SQL
028300060330     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
028400060330     C/END-EXEC
028500060330      /FREE
028600060330
028700060323         IN(E) §azute;
028800060323         IF NOT %ERROR;
028900060323           IN(E) §DatiUte;
029000060323         ENDIF;
029100060323         IF %ERROR OR rsut = ' ';
029200060323           tibs34r(tibs34ds);
029300060323           IN §azute;
029400060323           IN §DatiUte;
029500060323         ENDIF;
029600060330
029700060427         IF dutLpo = ' ';
029800060427           dutLpo = Sede;
029900060427         ENDIF;
030000060427
030100060323         t01PgmName = pgmName;
030200060407
030300060407       ENDSR;
030400060131
030500060131       //*******************************************************************************************
030600060131       //
030700060131       // Operazioni iniziali.
030800060131       //
030900060131       //*******************************************************************************************
031000060131       BEGSR inzProc;
031100060330
031200060330         RESET opCode;
031300060131
031400060131         EXSR chkParm;
031500060222
031600060222         CLEAR kpjbu;
031700060405         CLEAR xSoc001ds;
031800060405         xSoc('SOC001':xscSoc:0:*BLANK:rtnXsc:xsoc001ds:kpjba);
031900060222         IF rtnXsc = *ON;
032000060426           ycow0400s.rtnCode = 'ERRXSOC';
032100060330           opCode = 'ERRORE';
032200060330           LEAVESR;
032300060222         ENDIF;
032400060308
032500060131       ENDSR;
032600060131
032700060131       //*******************************************************************************************
032800060131       //
032900060131       // Controllo dei parametri ricevuti.
033000060427       // All'inizio di KPJBU c'è YCOW0400S.
033100060427       // A seguire ci può essere YCOW0410S oppure YCOW0420S, a seconda che
033200060427       // la chiamata sia fatta per gestire i modelli o per cercare e scegliere un modello.
033300060131       //
033400060131       //*******************************************************************************************
033500060131       BEGSR chkParm;
033600060414
033700060426         ycow0400s = kpjbu;
033800060426         CLEAR ycow0400s.rtnCmd;
033900060426         CLEAR ycow0400s.rtnCode;
034000060426         CLEAR ycow0400s.rtnEsito;
034100060426         CLEAR ycow0410s;
034200060426         CLEAR ycow0420s;
034300060426
034400060426         SELECT;
034500060426           WHEN ycow0400s.opCode = 'GESTIONE';
034600060426             ycow0410s = %SUBST(kpjbu:%SIZE(ycow0400s)+1);
034700060426           WHEN ycow0400s.opCode = 'RICERCA';
034800060426             ycow0420s = %SUBST(kpjbu:%SIZE(ycow0400s)+1);
034900060426             CLEAR ycow0420s.modello;
035000060426             CLEAR ycow0420s.rrnYnmrt;
035100060426         ENDSL;
035200060414
035300060131       ENDSR;
035400060221
035500060221       //*******************************************************************************************
035600060221       //
035700060330       // Inizializzo subfile movimenti.
035800060221       //
035900060221       //*******************************************************************************************
036000060221       BEGSR inzC01;
036100060320
036200060330         opCode = 'LODC01';
036300060414         IF NOT %OPEN(ycow0420d);
036400060414           OPEN ycow0420d;
036500060320         ENDIF;
036600060221         sflDsp = *OFF;
036700060221         sflClr = *ON;
036800060221         WRITE c01;
036900060221         sflClr = *OFF;
037000060221         CLEAR s01Rrn;
037100060223         CLEAR s01RrnLast;
037200060221         RESET wrkCsrRrn;
037300060331
037400060221       ENDSR;
037500060201
037600060201       //*******************************************************************************************
037700060201       //
037800060414       // Carico elenco modelli.
037900060201       //
038000060201       //*******************************************************************************************
038100060201       BEGSR lodC01;
038200060201
038300060330         opCode = 'PUTC01';
038400060330
038500060427         wrkSqlStm = 'SELECT RRN(YNMRT00F), MRTMODELLO, MRTDESCRIZ, +
038600060427         MRTANNULLA, MRTABLUTES, MRTABLUTE1, MRTABLUTE2, MRTABLFGSS, +
038700060427         MRTABLFGS1, MRTABLFGS2';
038800060427         wrkSqlStm = wrkSqlStm + ' FROM YNMRT00F';
038900060426
039000060426         SELECT;
039100060426           WHEN ycow0400s.opCode = 'GESTIONE';
039200060511             IF ycow0410s.dtValid = *LOVAL;
039300060511               wrkSqlStm = wrkSqlStm + ' WHERE MRTDTINVAL = MRTDTINVAL';
039400060511             ELSE;
039500060511               wrkSqlStm = wrkSqlStm + ' WHERE ' + '''' +
039600060511               %CHAR(ycow0410s.dtValid) + '''' +
039700060511               ' BETWEEN MRTDTINVAL AND MRTDTFIVAL';
039800060511             ENDIF;
039900060427             SELECT;
040000060427               WHEN ycow0410s.annullate = *ON;
040100060427                 wrkSqlStm = wrkSqlStm + ' AND MRTANNULLA = ' + '''1''';
040200060427               WHEN ycow0410s.annullate = *OFF;
040300060427                 wrkSqlStm = wrkSqlStm + ' AND MRTANNULLA = ' + '''0''';
040400060427             ENDSL;
040500060427             SELECT;
040600060427               WHEN ycow0410s.ablUteS = *ON;
040700060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTES = ' + '''1''';
040800060427               WHEN ycow0410s.ablUteS = *OFF;
040900060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTES = ' + '''0''';
041000060427             ENDSL;
041100060427             SELECT;
041200060427               WHEN ycow0410s.ablUte1 = *ON;
041300060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTE1 = ' + '''1''';
041400060427               WHEN ycow0410s.ablUteS = *OFF;
041500060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTE1 = ' + '''0''';
041600060427             ENDSL;
041700060427             SELECT;
041800060427               WHEN ycow0410s.ablUte2 = *ON;
041900060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTE2 = ' + '''1''';
042000060427               WHEN ycow0410s.ablUteS = *OFF;
042100060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTE2 = ' + '''0''';
042200060427             ENDSL;
042300060427             SELECT;
042400060427               WHEN ycow0410s.ablFgsS = *ON;
042500060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGSS = ' + '''1''';
042600060427               WHEN ycow0410s.ablFgsS = *OFF;
042700060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGSS = ' + '''0''';
042800060427             ENDSL;
042900060427             SELECT;
043000060427               WHEN ycow0410s.ablFgs1 = *ON;
043100060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGS1 = ' + '''1''';
043200060427               WHEN ycow0410s.ablFgs1 = *OFF;
043300060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGS1 = ' + '''0''';
043400060427             ENDSL;
043500060427             SELECT;
043600060427               WHEN ycow0410s.ablFgs2 = *ON;
043700060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGS2 = ' + '''1''';
043800060427               WHEN ycow0410s.ablFgs2 = *OFF;
043900060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGS2 = ' + '''0''';
044000060427             ENDSL;
044100060426           WHEN ycow0400s.opCode = 'RICERCA';
044200060427             wrkSqlStm = wrkSqlStm + ' WHERE ' + '''' +
044300060427             %CHAR(ycow0420s.dtValid) + '''' +
044400060427             ' BETWEEN MRTDTINVAL AND MRTDTFIVAL';
044500060427             wrkSqlStm = wrkSqlStm + ' AND MRTANNULLA = ' + '''0''';
044600060427             SELECT;
044700060427               WHEN dutLpo = Sede;
044800060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTES = ' + '''1''';
044900060427               WHEN dutLpo = Livello1;
045000060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTE1 = ' + '''1''';
045100060427               WHEN dutLpo = Livello2;
045200060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLUTE2 = ' + '''1''';
045300060427             ENDSL;
045400060427             EXSR getLivelloFGS;
045500060427             SELECT;
045600060427               WHEN wrkLivelloFgs = Sede;
045700060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGSS = ' + '''1''';
045800060427               WHEN wrkLivelloFgs = Livello1;
045900060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGS1 = ' + '''1''';
046000060427               WHEN wrkLivelloFgs = Livello2;
046100060427                 wrkSqlStm = wrkSqlStm + ' AND MRTABLFGS2 = ' + '''1''';
046200060427             ENDSL;
046300060426         ENDSL;
046400060426
046500060427         wrkSqlStm = wrkSqlStm + ' ORDER BY MRTMODELLO';
046600060427         wrkSqlStm = wrkSqlStm + ' FOR READ ONLY';
046700060426
046800060426      /END-FREE
046900060426     C/EXEC SQL
047000060426     C+ PREPARE MODELLI FROM :wrkSqlStm
047100060426     C/END-EXEC
047200060426     C
047300060426     C/EXEC SQL
047400060426     C+ DECLARE MODELLI CURSOR FOR MODELLI
047500060426     C/END-EXEC
047600060426     C
047700060426     C/EXEC SQL
047800060426     C+ OPEN MODELLI
047900060426     C/END-EXEC
048000060426      /FREE
048100060330
048200060330         IF sqlCod < 0;
048300060426           ycow0400s.rtnCode = 'ERRSQL';
048400060426           ycow0400s.rtnEsito = sqlCod;
048500060512           textString = 'Codice ritorno ' + %TRIMR(ycow0400s.rtnCode)
048600060512           + ' esito ' + %CHAR(ycow0400s.rtnEsito) +
048700060512           '. Premere invio per continuare.';
048800060330           opCode = 'ERRORE';
048900060330           LEAVESR;
049000060330         ENDIF;
049100060330
049200060414         EXSR inzS01;
049300060414
049400060330         DOU sqlCod = 100;
049500060330      /END-FREE
049600060330     C/EXEC SQL
049700060414     C+ FETCH MODELLI
049800060427     C+ INTO :h01RrnMrt, :mrtModello, :mrtDescriz, :mrtAnnulla, :mrtAblUteS,
049900060427     C+      :mrtAblUte1, :mrtAblUte2, :mrtAblFgsS, :mrtAblFgs1, :mrtAblFgs2
050000060330     C/END-EXEC
050100060330      /FREE
050200060330           SELECT;
050300060330             WHEN sqlCod = 100;
050400060330               sflEnd = *ON;
050500060330               LEAVE;
050600060330             WHEN sqlCod < 0;
050700060426               ycow0400s.rtnCode = 'ERRSQL';
050800060426               ycow0400s.rtnEsito = sqlCod;
050900060330               opCode = 'ERRORE';
051000060330               LEAVE;
051100060330             OTHER;
051200060330               EXSR setS01;
051300060331               EXSR wrtS01;
051400060330           ENDSL;
051500060330         ENDDO;
051600060330
051700060330      /END-FREE
051800060330     C/EXEC SQL
051900060414     C+ CLOSE MODELLI
052000060330     C/END-EXEC
052100060330      /FREE
052200060330
052300060330         s01RrnLast = s01Rrn;
052400060330
052500060201       ENDSR;
052600060330
052700060330       //*******************************************************************************************
052800060330       //
052900060330       // Inizializzo riga subfile movimenti.
053000060330       //
053100060330       //*******************************************************************************************
053200060330       BEGSR inzS01;
053300060330
053400060330         CLEAR s01;
053500060330
053600060330       ENDSR;
053700060330
053800060330       //*******************************************************************************************
053900060330       //
054000060405       // Imposto riga subfile movimenti come definito nel modello.
054100060330       //
054200060330       //*******************************************************************************************
054300060330       BEGSR setS01;
054400060427
054500060427         SELECT;
054600060427           WHEN ycow0400s.opCode = 'GESTIONE';
054700060427             s01Var = mrtAblUteS + ' ' + mrtAblUte1 + ' ' + mrtAblUte2 + ' ' +
054800060427             mrtAblFgsS + ' ' + mrtAblFgs1 + ' ' + mrtAblFgs2 + ' ' +
054900060427             mrtAnnulla;
055000060427           WHEN ycow0400s.opCode = 'RICERCA';
055100060427             CLEAR s01Var;
055200060427         ENDSL;
055300060427
055400060330       ENDSR;
055500060331
055600060331       //*******************************************************************************************
055700060331       //
055800060331       // Scrittura riga subfile movimenti.
055900060331       //
056000060331       //*******************************************************************************************
056100060331       BEGSR wrtS01;
056200060331
056300060331         s01Rrn += 1;
056400060331         WRITE s01;
056500060331
056600060331       ENDSR;
056700060130
056800060131       //*******************************************************************************************
056900060131       //
057000060131       // Emissione subfile spedizioni.
057100060131       //
057200060131       //*******************************************************************************************
057300060131       BEGSR putC01;
057400060130
057500060403         opCode = 'PUTC01';
057600060427         RESET oxx;
057700060427         RESET fxx;
057800060427
057900060427         SELECT;
058000060427           WHEN ycow0400s.opCode = 'GESTIONE';
058100060427             c01Opzioni = '2=Modifica  3=Copia  4=Annulla/Ripristina  +
058200060511             5=Visualizza  14=Commenti  44=Cancella';
058300060427             oxx(1) = OpzModifica;
058400060427             oxx(2) = OpzCopia;
058500060427             oxx(3) = OpzAnnulla;
058600060427             oxx(4) = OpzVisualizza;
058700060509             oxx(5) = OpzCommenti;
058800060511             oxx(6) = OpzCancella;
058900060427             f01Tasti = 'F3=Fine  F5=Ricarica  F10=Nuovo modello  F12=Ritorno';
059000060427             fxx(1) = F3;
059100060427             fxx(2) = F5;
059200060427             fxx(3) = F10;
059300060427             fxx(4) = F12;
059400060427             c01VarA = 'U U U G G G A';
059500060427             c01VarB = 'S 1 2 S 1 2 n';
059600060427           WHEN ycow0400s.opCode = 'RICERCA';
059700060509             c01Opzioni = '1=Scelta  14=Commenti';
059800060511             oxx(5) = OpzScelta;
059900060511             oxx(6) = OpzCommenti;
060000060427             f01Tasti = 'F3=Fine  F5=Ricarica  F12=Ritorno';
060100060511             fxx(2) = F3;
060200060511             fxx(3) = F5;
060300060511             fxx(4) = F12;
060400060427             CLEAR c01VarA;
060500060427             CLEAR c01VarB;
060600060427         ENDSL;
060700060427
060800060131         WRITE t01;
060900060131         WRITE f01;
061000060404
061100060207         SELECT;
061200060207           WHEN wrkCsrRrn > 0;
061300060207             c01RcdNbr = wrkCsrRrn;
061400060207             CLEAR wrkCsrRrn;
061500060207           WHEN c01CsrRrn > 0;
061600060207             c01RcdNbr = c01CsrRrn;
061700060207           OTHER;
061800060207             c01RcdNbr = 1;
061900060207         ENDSL;
062000060131
062100060427         sflDsp = (s01RrnLast > 0);
062200060131         EXFMT c01;
062300060130
062400060426         ycow0400s.rtnCmd = dsp_aid;
062500060130
062600060131         SELECT;
062700060131           WHEN dsp_aid = F3 OR dsp_aid = F12;
062800060414             opCode = 'END';
062900060131           WHEN dsp_aid = F5;
063000060221             opCode = 'INZC01';
063100060427           WHEN dsp_aid = Enter;
063200060131             opCode = 'CHKC01';
063300060427           WHEN dsp_aid = F10;
063400060427             opCode = 'NUOVOMODEL';
063500060206         ENDSL;
063600060130
063700060131       ENDSR;
063800060130
063900060131       //*******************************************************************************************
064000060131       //
064100060427       // Lettura subfile per controlli.
064200060131       //
064300060131       //*******************************************************************************************
064400060427       BEGSR chkC01;
064500060130
064600060403         opCode = 'PUTC01';
064700060427         RESET errGenerico;
064800060207         CLEAR wrkCsrRrn;
064900060303         RESET stop;
065000060130
065100060427         EXSR chkC01Fxx;
065200060427         IF errGenerico;
065300060427           LEAVESR;
065400060427         ENDIF;
065500060427
065600060403         FOR wrkRrn = 1 TO s01RrnLast;
065700060403           CHAIN wrkRrn s01;
065800060403           IF NOT %FOUND;
065900060221             LEAVE;
066000060221           ENDIF;
066100060427           EXSR chkS01;
066200060427           EXSR updS01;
066300060427           IF wrkErrS01;
066400060427             errGenerico = *ON;
066500060427             IF wrkCsrRrn = 0;
066600060427               wrkCsrRrn = s01Rrn;
066700060427             ENDIF;
066800060427           ENDIF;
066900060427           IF stop;
067000060427             LEAVE;
067100060404           ENDIF;
067200060403         ENDFOR;
067300060130
067400060427         IF NOT errGenerico AND dsp_aid = Enter;
067500060427           opCode = 'RUNC01';
067600060131         ENDIF;
067700060130
067800060131       ENDSR;
067900060427
068000060427       //*******************************************************************************************
068100060427       //
068200060427       // Controllo tasto funzionale premuto.
068300060427       //
068400060427       //*******************************************************************************************
068500060427       BEGSR chkC01Fxx;
068600060427
068700060427         IF dsp_aid <> Enter;
068800060427           IF %LOOKUP(dsp_aid:fxx) = 0;
068900060427             errGenerico = *ON;
069000060427           ENDIF;
069100060427         ENDIF;
069200060427
069300060427       ENDSR;
069400060427
069500060427       //*******************************************************************************************
069600060427       //
069700060427       // Controllo riga subfile.
069800060427       //
069900060427       //*******************************************************************************************
070000060427       BEGSR chkS01;
070100060427
070200060427         RESET wrkErrS01;
070300060427         EXSR chkS01Oxx;
070400060427
070500060427       ENDSR;
070600060403
070700060403       //*******************************************************************************************
070800060403       //
070900060403       // Controllo opzione immessa su riga subfile.
071000060403       //
071100060403       //*******************************************************************************************
071200060427       BEGSR chkS01Oxx;
071300060404
071400060511         RESET p01Opz;
071500060404         IF s01Opz <> 0;
071600060427           IF %LOOKUP(s01Opz:oxx) = 0;
071700060427             wrkErrS01 = *ON;
071800060427             p01Opz = ReverseImage;
071900060427           ENDIF;
072000060404         ENDIF;
072100060403
072200060403       ENDSR;
072300060427
072400060427       //*******************************************************************************************
072500060427       //
072600060427       // Lettura subfile per elaborazione delle opzioni immesse.
072700060427       //
072800060427       //*******************************************************************************************
072900060427       BEGSR runC01;
073000060427
073100060427         opCode = 'PUTC01';
073200060427         RESET stop;
073300060427
073400060427         FOR wrkRrn = 1 TO s01RrnLast;
073500060427           CHAIN wrkRrn s01;
073600060427           IF NOT %FOUND;
073700060427             LEAVE;
073800060427           ENDIF;
073900060427           EXSR runS01Opz;
074000060427           EXSR updS01;
074100060427           IF stop;
074200060427             LEAVE;
074300060427           ENDIF;
074400060427         ENDFOR;
074500060427
074600060427       ENDSR;
074700060427
074800060427       //*******************************************************************************************
074900060427       //
075000060427       // Nuovo modello.
075100060427       //
075200060427       //*******************************************************************************************
075300060427       BEGSR nuovoModello;
075400060427
075500060427         opCode = 'PUTC01';
075600060427         RESET ycow0431s;
075700060427         ycow0431s.opCode = 'NUOVO';
075800060427         EXSR gestioneModello;
075900060427
076000060427       ENDSR;
076100060427
076200060427       //*******************************************************************************************
076300060427       //
076400060427       // Elaborazione opzione immessa su riga subfile.
076500060427       //
076600060427       //*******************************************************************************************
076700060427       BEGSR runS01Opz;
076800060427
076900060427         IF s01Opz <> 0;
077000060427           RESET ycow0431s;
077100060427           ycow0431s.modello = mrtModello;
077200060427           ycow0431s.rrnYnmrt = h01RrnMrt;
077300060427           SELECT;
077400060427             WHEN s01Opz = OpzScelta;
077500060427               ycow0420s.modello = mrtModello;
077600060427               ycow0420s.rrnYnmrt = h01RrnMrt;
077700060427               stop = *ON;
077800060427               opCode = 'END';
077900060427             WHEN s01Opz = OpzModifica;
078000060427               ycow0431s.opCode = 'MODIFICA';
078100060427               EXSR gestioneModello;
078200060427             WHEN s01Opz = OpzCopia;
078300060427               ycow0431s.opCode = 'COPIA';
078400060427               EXSR gestioneModello;
078500060427             WHEN s01Opz = OpzAnnulla;
078600060427               ycow0431s.opCode = 'ANNULLA';
078700060427               EXSR gestioneModello;
078800060511             WHEN s01Opz = OpzCancella;
078900060511               ycow0431s.opCode = 'CANCELLA';
079000060511               EXSR gestioneModello;
079100060427             WHEN s01Opz = OpzVisualizza;
079200060427               ycow0431s.opCode = 'VISUALIZZA';
079300060427               EXSR gestioneModello;
079400060509             WHEN s01Opz = OpzCommenti;
079500060509               EXSR gestioneCommenti;
079600060427             OTHER;
079700060427           ENDSL;
079800060427           CLEAR s01Opz;
079900060427         ENDIF;
080000060427
080100060427       ENDSR;
080200060427
080300060427       //*******************************************************************************************
080400060427       //
080500060427       // Aggiornamento riga subfile.
080600060427       //
080700060427       //*******************************************************************************************
080800060427       BEGSR updS01;
080900060427
081000060427         UPDATE s01;
081100060427
081200060427       ENDSR;
081300060427
081400060427       //*******************************************************************************************
081500060427       //
081600060427       // Gestione modello.
081700060427       //
081800060427       //*******************************************************************************************
081900060427       BEGSR gestioneModello;
082000060427
082100060427         kpjbu = ycow0431s;
082200060508         ycow0431r(kpjba);
082300060427         ycow0431s = kpjbu;
082400060511
082500060511         IF ycow0431s.rtnCode = 'DONE';
082600060511           SELECT;
082700060511             WHEN ycow0431s.rtnCmd = F3;
082800060511               opCode = 'END';
082900060511             WHEN ycow0431s.rtnCmd = F6;
083000060511               opCode = 'INZC01';
083100060511           ENDSL;
083200060512         ELSE;
083300060512           textString = 'Codice ritorno ' + %TRIMR(ycow0431s.rtnCode)
083400060512           + ' esito ' + %CHAR(ycow0431s.rtnEsito) +
083500060512           '. Premere invio per continuare.';
083600060512           opCode = 'ERRORE';
083700060512         ENDIF;
083800060427
083900060427       ENDSR;
084000060509
084100060509       //*******************************************************************************************
084200060509       //
084300060509       // Gestione commenti.
084400060509       //
084500060509       //*******************************************************************************************
084600060509       BEGSR gestioneCommenti;
084700060509
084800060509         RESET ana725ds;
084900060509         IF ycow0400s.opCode = 'GESTIONE';
085000060509           opz725 = '02';
085100060509         ELSE;
085200060509           opz725 = '05';
085300060509         ENDIF;
085400060509         ky1725 = mrtModello;
085500060509         kpjbu = ana725ds;
085600060509         ana725r(kpjba);
085700060509         ana725ds = kpjbu;
085800060509
085900060509       ENDSR;
086000060427
086100060427       //*******************************************************************************************
086200060427       //
086300060427       // Reperisco livello PO gestione.
086400060427       //
086500060427       //*******************************************************************************************
086600060427       BEGSR getLivelloFGS;
086700060427
086800060427         RESET wrkLivelloFgs;
086900060427
087000060427         IF ycow0420s.fgs = POSede;
087100060427           wrkLivelloFgs = Sede;
087200060427           LEAVESR;
087300060427         ENDIF;
087400060427
087500060427         IF ycow0420s.fgs <> d55Lin;
087600060427           RESET fnlv55ds;
087700060427           d55Lin = ycow0420s.fgs;
087800060427           d55Drf = (%SUBDT(ycow0420s.dtValid:*YEARS) * 10000)
087900060427           + (%SUBDT(ycow0420s.dtValid:*MONTHS) * 100)
088000060427           + %SUBDT(ycow0420s.dtValid:*DAYS);
088100060427           fnlv55r(fnlv55ds);
088200060427           IF d55Err <> ' ';
088300060427           ENDIF;
088400060427         ENDIF;
088500060427
088600060427         SELECT;
088700060427           WHEN ycow0420s.fgs = d55Tfp;
088800060427             wrkLivelloFgs = Livello1;
088900060427           WHEN ycow0420s.fgs <> d55Tfp;
089000060427             wrkLivelloFgs = Livello2;
089100060427         ENDSL;
089200060427
089300060427       ENDSR;
089400060512
089500060512       //*******************************************************************************************
089600060512       //
089700060512       // Gestione errori.
089800060512       //
089900060512       //*******************************************************************************************
090000060512       BEGSR errore;
090100060512
090200060512         CLEAR qusec;
090300060512         qusbprv = %SIZE(qusec);
090400060512         displayLongText(textString:lengthOfTextString:messageID
090500060512         :qualifiedMessageFileName:qusec);
090600060512
090700060512       ENDSR;
090800060407
090900060131       //*******************************************************************************************
091000060131       //
091100060131       // Uscita.
091200060131       //
091300060131       //*******************************************************************************************
091400060131       BEGSR uscita;
091500060131
091600060320         IF dsp_aid = F12;
091700060320           WRITE frcDta;
091800060320         ELSE;
091900060414           CLOSE(E) ycow0420d;
092000060320         ENDIF;
092100060426
092200060426         IF ycow0400s.rtnCode = ' ';
092300060426           ycow0400s.rtnCode = 'DONE';
092400060131         ENDIF;
092500060426
092600060426         SELECT;
092700060426           WHEN ycow0400s.opCode = 'GESTIONE';
092800060426             kpjbu = ycow0400s + ycow0410s;
092900060426           WHEN ycow0400s.opCode = 'RICERCA';
093000060426             kpjbu = ycow0400s + ycow0420s;
093100060426         ENDSL;
093200060426
093300060426         *INLR = (ycow0400s.setOnLR = *ON);
093400060131         RETURN;
093500060131
093600060131       ENDSR;
093700060131
093800060131      /END-FREE
