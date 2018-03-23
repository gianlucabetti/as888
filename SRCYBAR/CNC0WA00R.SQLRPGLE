000100060321      //**********************************************************************************
000200060321      //
000300060321      // Questo programma immette il dettaglio del contante incassato.
000400060321      //
000500060321      //**********************************************************************************
000600060320     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000700060320     H DATEDIT(*YMD) DECEDIT(*JOBRUN)
000800060320
000900060321      //**********************************************************************************
001000060321      //
001100060321      // Definizione files.
001200060321      //
001300060321      //**********************************************************************************
001400060320     Fcnc0wa00d CF   E             WORKSTN
001500060320     F                                     USROPN
001600060320     F                                     INFDS(infDspF)
001700060320     F                                     INDDS(indDspF)
001800060320     F                                     SFILE(s01:s01Rrn)
001900060320
002000060321      //**********************************************************************************
002100060321      //
002200060321      // Definizione costanti.
002300060321      //
002400060321      //**********************************************************************************
002500060320     D F3              C                   X'33'
002600060320     D F5              C                   X'35'
002700060320     D F6              C                   X'36'
002800060320     D F8              C                   X'38'
002900060320     D F12             C                   X'3C'
003000060322     D F21             C                   X'B9'
003100060320     D Enter           C                   X'F1'
003200060320     D ReverseImage    C                   X'21'
003300060320     D HighIntensity   C                   X'22'
003400060320     D Blink           C                   X'28'
003500060321     D IgnoreDBCSdata...
003600060321     D                 C                   '0'
003700060321     D NeverPromptTheCommand...
003800060321     D                 C                   '0'
003900060321     D CommandRunning...
004000060321     D                 C                   '0'
004100060320
004200060321      //**********************************************************************************
004300060321      //
004400060321      // Definizione delle procedure esterne usate.
004500060321      //
004600060321      //**********************************************************************************
004700060322     D tibs34r         PR
004800060322     D                                     EXTPGM('TIBS34R')
004900060322     D  tibs34ds
005000060322     D                                     LIKEDS(tibs34ds)
005100060322     D trul29c         PR
005200060320     D                                     EXTPGM('TRUL29C')
005300060320     D  obj                          10A
005400060320     D                                     CONST
005500060320     D  objType                       8A
005600060320     D                                     CONST
005700060320     D  rtnLib                       10A
005800060321     D ProcessCommands...
005900060321     D                 PR
006000060320     D                                     EXTPGM('QCAPCMD')
006100060320     D  sourceCommandString...
006200060321     D                            32767A
006300060321     D                                     CONST
006400060321     D                                     OPTIONS(*VARSIZE)
006500060320     D  sourceCommandStringLength...
006600060320     D                               10I 0
006700060320     D                                     CONST
006800060320     D  optionsControlBlock...
006900060321     D                                     LIKE(QCAP0100)
007000060321     D                                     CONST
007100060321     D                                     OPTIONS(*VARSIZE)
007200060320     D  optionsControlBlockLength...
007300060320     D                               10I 0
007400060320     D                                     CONST
007500060320     D  optionsControlBlockFormat...
007600060320     D                                8A
007700060320     D                                     CONST
007800060320     D  changedCommandString...
007900060321     D                            32767A
008000060321     D                                     OPTIONS(*VARSIZE:*OMIT)
008100060320     D  lengthAvailableForChangedCommandString...
008200060320     D                               10I 0
008300060320     D                                     CONST
008400060320     D  lengthOfChangedCommandStringAvailableToReturn...
008500060320     D                               10I 0
008600060320     D  errorCode...
008700060321     D                                     LIKE(qusec)
008800060321     D                                     OPTIONS(*VARSIZE)
008900060321     D DisplayLongText...
009000060321     D                 PR
009100060321     D                                     EXTPGM('QUILNGTX')
009200060321     D  TextString...
009300060321     D                              256A
009400060321     D                                     CONST
009500060321     D                                     OPTIONS(*VARSIZE)
009600060321     D  LengthOfTextString...
009700060321     D                               10I 0
009800060321     D                                     CONST
009900060321     D  MessageID...
010000060321     D                                7A
010100060321     D                                     CONST
010200060321     D  QualifiedMessageFileName...
010300060321     D                               20A
010400060321     D                                     CONST
010500060321     D  ErrorCode...
010600060321     D                                     LIKE(qusec)
010700060321     D                                     OPTIONS(*VARSIZE)
010800060323     D cnc0wa01r       PR
010900060323     D                                     EXTPGM('CNC0WA01R')
011000060323     D  kpjba
011100060323     D                                     LIKEDS(kpjba)
011200060323
011300060321      //**********************************************************************************
011400060321      //
011500060321      // Definizione strutture dati.
011600060321      //
011700060321      //**********************************************************************************
011800060320     D infDspF         DS
011900060320     D  dsp_aid              369    369A                                        AID byte
012000060320     D  sf_rrn               376    377I 0                                      Subfile rr
012100060320     D indDspF         DS
012200060320     D  sflDspCtl             25     25N
012300060320     D  sflDsp                26     26N
012400060320     D  sflClr                27     27N
012500060320     D  sflEnd                28     28N
012600060321     D  sflNxtChg             29     29N
012700060321     D  errQuadratura         30     30N
012800060321     D  errGenerico           99     99N
012900060320     D cndizion      E DS
013000060320     D                                     BASED(nullPtr)
013100060320     D                                     PREFIX(diz)
013200060320     D andiz00f      E DS
013300060320     D                                     BASED(nullPtr)
013400060320     D                                     PREFIX(pjz)
013500060320     D kpjba         E DS
013600060320     D cnc0wa00s     E DS
013700060320     D                                     INZ
013800060321     D                                     PREFIX(prm)
013900060322     D tibs34ds      E DS
014000060322     D                                     INZ
014100060322
014200060321      //**********************************************************************************
014300060321      //
014400060321      // Definizione variabili.
014500060321      //
014600060321      //**********************************************************************************
014700060322     D/COPY QSYSINC/QRPGLESRC,QCAPCMD
014800060322     D/COPY QSYSINC/QRPGLESRC,QUSEC
014900060320     D nullPtr         S               *
015000060320     D rtnLib          S             10A
015100060320     D opCode          S             10A
015200060320     D                                     INZ('INZC01')
015300060320     D s01Rrn          S              5I 0
015400060320     D esito           S             10I 0
015500060320     D comando         S            256A
015600060321     D testo           S            256A
015700060323     D stampare        S              1N
015800060321
015900060322      //**********************************************************************************
016000060322      //
016100060322      // Definizione aree dati.
016200060322      //
016300060322      //**********************************************************************************
016400060322     D §azute        E DS
016500060322     D                                     EXTNAME(azute00f)
016600060322     D                                     DTAARA
016700060322     D §DatiUte      E DS
016800060322     D                                     EXTNAME(dDatiUte)
016900060322     D                                     DTAARA
017000060322
017100060321      //**********************************************************************************
017200060321      //
017300060321      // Definizione parametri procedura.
017400060321      //
017500060321      //**********************************************************************************
017600060320     C     *ENTRY        PLIST
017700060320     C                   PARM                    kpjba
017800060320
017900060320      /FREE
018000060320
018100060320       EXSR inzProc;
018200060320
018300060320       DOU opCode = 'END';
018400060320         SELECT;
018500060320           WHEN opCode = 'END';
018600060320             LEAVE;
018700060320           WHEN opCode = 'INZC01';
018800060320             EXSR inzC01;
018900060320           WHEN opCode = 'PUTC01';
019000060320             EXSR putC01;
019100060320           WHEN opCode = 'CHKC01';
019200060320             EXSR getC01Chk;
019300060323           WHEN opCode = 'PRTDATA';
019400060323             EXSR prtData;
019500060321           WHEN opCode = 'UPDDB';
019600060321             EXSR getC01UpdDb;
019700060321           WHEN opCode = 'ERRORE';
019800060321             EXSR errore;
019900060320           OTHER;
020000060321             prmOpCode = opCode;
020100060320             LEAVE;
020200060320         ENDSL;
020300060320       ENDDO;
020400060320
020500060320       EXSR uscita;
020600060320
020700060320       //*********************************************************************************
020800060320       //
020900060320       // Operazioni iniziali da eseguire una tantum.
021000060320       //
021100060320       //*********************************************************************************
021200060320       BEGSR *INZSR;
021300060426
021400060321      /END-FREE
021500060321     C/EXEC SQL
021600060321     C+ SET OPTION CLOSQLCSR = *ENDMOD, DYNUSRPRF = *OWNER
021700060321     C/END-EXEC
021800060321      /FREE
021900060322
022000060322         IN(E) §azute;
022100060322         IF NOT %ERROR;
022200060322           IN(E) §DatiUte;
022300060322         ENDIF;
022400060322         IF %ERROR OR rsut = ' ';
022500060322           tibs34r(tibs34ds);
022600060322           IN §azute;
022700060322           IN §DatiUte;
022800060322         ENDIF;
022900060322
023000060322       ENDSR;
023100060320
023200060320       //*********************************************************************************
023300060320       //
023400060320       // Operazioni iniziali.
023500060320       //
023600060320       //*********************************************************************************
023700060320       BEGSR inzProc;
023800060320
023900060321         RESET opCode;
024000060323         RESET stampare;
024100060320         EXSR chkParm;
024200060320
024300060320         // Duplico FIDCI00F in QTEMP.
024400060320         trul29c('FIDCI00F':'*FILE':rtnLib);
024500060321         SELECT;
024600060321           WHEN rtnLib = ' ';
024700060321             prmRtnCode = 'ERRFIDCI';
024800060321             esito = 1;
024900060321             opCode = 'ERRORE';
025000060321           WHEN rtnLib <> 'QTEMP';
025100060321             CLEAR qcap0100;
025200060321             qcabcsdh = IgnoreDBCSdata;
025300060321             qcapa = NeverPromptTheCommand;
025400060321             qcacmdss = CommandRunning;
025500060321             qcaerved = X'000000000000000000';
025600060321             comando = 'CRTDUPOBJ OBJ(FIDCI*) FROMLIB(' + %TRIMR(rtnLib)
025700060321             + ') OBJTYPE(*FILE) TOLIB(QTEMP)';
025800060321             CLEAR qusec;
025900060321             qusbprv = %SIZE(qusec);
026000060321             ProcessCommands(comando:%SIZE(comando):qcap0100:%SIZE(qcap0100)
026100060321             :'CPOP0100':*OMIT:%INT(0):esito:qusec);
026200060321             IF qusei <> ' ';
026300060321               prmRtnCode = qusei;
026400060321               esito = 2;
026500060321               opCode = 'ERRORE';
026600060321             ENDIF;
026700060321         ENDSL;
026800060320
026900060320       ENDSR;
027000060320
027100060320       //*********************************************************************************
027200060320       //
027300060320       // Controllo dei parametri ricevuti.
027400060320       //
027500060320       //*********************************************************************************
027600060320       BEGSR chkParm;
027700060320
027800060320         cnc0wa00s = kpjbu;
027900060320         CLEAR prmRtnCmd;
028000060320         CLEAR prmRtnCode;
028100060321         CLEAR prmImpImmesso;
028101160229         CLEAR p01MsgId30;
028200060321         IF prmDivisa = ' ';
028300060321           prmRtnCode = 'ERRPARM';
028400060321           esito = -1;
028500060321           opCode = 'ERRORE';
028600060321         ENDIF;
028700060320
028800060320       ENDSR;
028900060320
029000060320       //*********************************************************************************
029100060320       //
029200060321       // Inizializzo subfile contante.
029300060320       //
029400060320       //*********************************************************************************
029500060320       BEGSR inzC01;
029600060320
029700060321         opCode = 'PUTC01';
029800060321
029900060320         IF NOT %OPEN(cnc0wa00d);
030000060320           OPEN cnc0wa00d;
030100060320         ENDIF;
030200060320
030300060321         sflDspCtl = *OFF;
030400060320         sflDsp = *OFF;
030500060320         sflEnd = *OFF;
030600060320         sflClr = *ON;
030700060320         WRITE c01;
030800060321         sflDspCtl = *ON;
030900060320         sflDsp = *ON;
031000060320         sflClr = *OFF;
031100060320         sflNxtChg = *OFF;
031200060320         CLEAR s01Rrn;
031300060320         EXSR azzeraTotali;
031400060320
031500060321      /END-FREE
031600060321     C/EXEC SQL
031700060321     C+ DECLARE CONTANTE CURSOR FOR
031800060321     C+ SELECT TGDDESCR, TGDVALORE, IFNULL(DCIQTA, 0)
031900060321     C+ FROM YNTGD00F LEFT OUTER JOIN QTEMP/FIDCI00F
032000060321     C+ ON :knraz = DCIKNRAZ AND TGDVALORE = DCIVALORE
032100060321     C+ WHERE TGDDIVISA = :prmDivisa AND TGDANN = '0'
032200060321     C+       AND TGDVALORE >= :prmMinimo
032300060321     C+ ORDER BY TGDDIVISA, TGDSEQUENZ
032400060321     C+ FOR READ ONLY
032500060321     C/END-EXEC
032600060321     C
032700060321     C/EXEC SQL
032800060321     C+ OPEN CONTANTE
032900060321     C/END-EXEC
033000060321      /FREE
033100060321
033200060321         IF sqlCod < 0;
033300060321           prmRtnCode = 'ERRSQLOPN';
033400060321           esito = sqlCod;
033500060321           opCode = 'ERRORE';
033600060321           LEAVESR;
033700060321         ENDIF;
033800060321
033900060321         DOU sqlCod = 100;
034000060321      /END-FREE
034100060321     C/EXEC SQL
034200060323     C+ FETCH CONTANTE INTO :tgdDescr, :tgdValore, :s01Qta
034300060321     C/END-EXEC
034400060321      /FREE
034500060321           SELECT;
034600060321             WHEN sqlCod = 100;
034700060321               sflEnd = *ON;
034800060321               LEAVE;
034900060321             WHEN sqlCod < 0;
035000060321               prmRtnCode = 'ERRSQLFET';
035100060321               esito = sqlCod;
035200060321               opCode = 'ERRORE';
035300060321               LEAVE;
035400060321             OTHER;
035500060321               EXSR setS01;
035600060323               sflNxtChg = (s01Qta > 0);
035700060321               s01Rrn += 1;
035800060321               WRITE s01;
035900060321               EXSR calcolaTotali;
036000060321           ENDSL;
036100060321         ENDDO;
036200060321
036300060321      /END-FREE
036400060321     C/EXEC SQL
036500060321     C+ CLOSE CONTANTE
036600060321     C/END-EXEC
036700060321      /FREE
036800060320
036900060320       ENDSR;
037000060321
037100060321       //*********************************************************************************
037200060321       //
037300060321       // Impostazione riga contante.
037400060321       //
037500060321       //*********************************************************************************
037600060321       BEGSR setS01;
037700060321
037800060323         s01Importo = s01Qta * tgdValore;
037900060321
038000060321       ENDSR;
038100060320
038200060320       //*********************************************************************************
038300060320       //
038400060321       // Emissione subfile contante.
038500060320       //
038600060320       //*********************************************************************************
038700060320       BEGSR putC01;
038800060320
038900060321         opCode = 'PUTC01';
039000060320
039100060321         c01Div = prmDivisa;
039200060426         c01Importo = prmImpTotale;
039300060426         c01ImpImm = prmImpImmesso;
039400060321
039500060320         WRITE f01;
039600060320         EXFMT c01;
039700060320
039800060320         prmRtnCmd = dsp_aid;
039900060320
040000060320         SELECT;
040100060320           WHEN dsp_aid = F3 OR dsp_aid = F8 OR dsp_aid = F12;
040200060320             opCode = 'END';
040300060320           WHEN dsp_aid = F5;
040400060320             opCode = 'INZC01';
040500060320           WHEN dsp_aid = F6 OR dsp_aid = Enter;
040600060320             opCode = 'CHKC01';
040700060322           WHEN dsp_aid = F21;
040800060323             stampare = *ON;
040900060320         ENDSL;
041000060320
041100060320       ENDSR;
041200060320
041300060320       //*********************************************************************************
041400060320       //
041500060320       // Lettura subfile partite incassate per controlli.
041600060320       //
041700060320       //*********************************************************************************
041800060320       BEGSR getC01Chk;
041900060320
042000060321         opCode = 'PUTC01';
042100060320         *IN99 = *OFF;
042200060321         CLEAR p01Importo;
042300060321         CLEAR p01MsgId30;
042400060320         EXSR azzeraTotali;
042500060321         sflNxtChg = *ON;
042600060320
042700060320         DOU %EOF;
042800060320           READC s01;
042900060320           IF %EOF;
043000060320             LEAVE;
043100060320           ENDIF;
043200060321           EXSR setS01;
043300060320           EXSR calcolaTotali;
043400060320           UPDATE s01;
043500060320         ENDDO;
043600060320
043700060320         sflNxtChg = *OFF;
043800060320
043900060321         IF prmImpTotale > 0 AND prmImpTotale <> prmImpImmesso;
044000060426           IF dsp_aid <> F6;
044100060426             *IN99 = *ON;
044200060426           ENDIF;
044300060321           p01Importo = ReverseImage;
044400060321           errQuadratura = *ON;
044500060426           SELECT;
044600060426             WHEN prmImpImmesso > prmImpTotale;
044700060426               p01MsgId30 = 'Resto ' +
044800060426               %TRIML(%EDITC(prmImpImmesso-prmImpTotale:'2')) + ' ' + prmDivisa
044900060426               + ' (F6=Conferma).';
045000060426             WHEN prmImpImmesso < prmImpTotale;
045100060426               p01MsgId30 = 'Mancano ' +
045200060426               %TRIML(%EDITC(prmImpTotale-prmImpImmesso:'2')) + ' ' + prmDivisa
045300060426               + ' (F6=Conferma).';
045400060426           ENDSL;
045500060321         ENDIF;
045600060320
045700060320         IF NOT *IN99 AND dsp_aid = F6;
045800060321           opCode = 'UPDDB';
045900060320         ENDIF;
046000060320
046100060320       ENDSR;
046200060320
046300060320       //*********************************************************************************
046400060320       //
046500060321       // Lettura subfile contante per scrittura database.
046600060320       //
046700060320       //*********************************************************************************
046800060321       BEGSR getC01UpdDb;
046900060320
047000060321         opCode = 'END';
047100060320         CLEAR esito;
047200060321
047300060321      /END-FREE
047400060321     C/EXEC SQL
047500060321     C+ DELETE FROM QTEMP/FIDCI00F
047600060321     C+ WHERE DCIKNRAZ = :knraz
047700060321     C/END-EXEC
047800060321      /FREE
047900060320
048000060321         IF sqlCod < 0;
048100060321           opCode = 'ERRORE';
048200060321           prmRtnCode = 'ERRSQLDEL';
048300060321           esito = sqlCod;
048400060321           LEAVESR;
048500060321         ENDIF;
048600060321
048700060321         DOU %EOF;
048800060321           READC s01;
048900060321           IF %EOF;
049000060321             LEAVE;
049100060321           ENDIF;
049200060321           EXSR wrtFidci00f;
049300060321           IF esito <> 0;
049400060321             LEAVE;
049500060321           ENDIF;
049600060321         ENDDO;
049700060323
049800060323         IF stampare;
049900060323           opCode = 'PRTDATA';
050000060323         ENDIF;
050100060320
050200060320       ENDSR;
050300060320
050400060320       //*********************************************************************************
050500060320       //
050600060321       // Scrittura FIDCI00F.
050700060320       //
050800060320       //*********************************************************************************
050900060321       BEGSR wrtFidci00f;
051000060320
051100060320         CLEAR esito;
051200060320
051300060323         IF s01Qta > 0;
051400060321      /END-FREE
051500060321     C/EXEC SQL
051600060321     C+ INSERT INTO QTEMP/FIDCI00F (DCIKNRAZ, DCIVALORE, DCIQTA)
051700060323     C+ VALUES(:knraz, :tgdValore, :s01Qta)
051800060321     C/END-EXEC
051900060321      /FREE
052000060321           IF sqlCod < 0;
052100060321             opCode = 'ERRORE';
052200060321             prmRtnCode = 'ERRSQLINS';
052300060321             esito = sqlCod;
052400060321             LEAVESR;
052500060321           ENDIF;
052600060321         ENDIF;
052700060323
052800060320       ENDSR;
052900060320
053000060320       //*********************************************************************************
053100060320       //
053200060320       // Azzera totali.
053300060320       //
053400060320       //*********************************************************************************
053500060320       BEGSR azzeraTotali;
053600060320
053700060321         c01Importo = prmImpTotale;
053800060321         CLEAR prmImpImmesso;
053900060426         CLEAR c01ImpImm;
054000060320
054100060320       ENDSR;
054200060320
054300060320       //*********************************************************************************
054400060320       //
054500060320       // Calcolo totali.
054600060320       //
054700060320       //*********************************************************************************
054800060320       BEGSR calcolaTotali;
054900060320
055000060321         prmImpImmesso += s01Importo;
055100060320
055200060320       ENDSR;
055300060322
055400060322       //*********************************************************************************
055500060322       //
055600060322       // Stampa dati immessi.
055700060322       //
055800060322       //*********************************************************************************
055900060323       BEGSR prtData;
056000060322
056100060323         opCode = 'END';
056200060323         CLEAR esito;
056300060323         kpjbu = cnc0wa00s;
056400060323         cnc0wa01r(kpjba);
056500060323         cnc0wa00s = kpjbu;
056600060322
056700060322       ENDSR;
056800060321
056900060321       //*********************************************************************************
057000060321       //
057100060321       // Errore.
057200060321       //
057300060321       //*********************************************************************************
057400060321       BEGSR errore;
057500060321
057600060321         opCode = 'END';
057700060321         testo = 'Si è verificato l''errore ' + %TRIMR(prmRtnCode)
057800060321         + ' esito ' + %TRIML(%CHAR(esito))
057900060321         + '. Premere invio per continuare.';
058000060321         CLEAR qusec;
058100060321         qusbprv = %SIZE(qusec);
058200060321         DisplayLongText(testo:%SIZE(testo):'BAR0019':'YBARMSG   *LIBL'
058300060321         :qusec);
058400060321
058500060321       ENDSR;
058600060320
058700060320       //*********************************************************************************
058800060320       //
058900060320       // Uscita.
059000060320       //
059100060320       //*********************************************************************************
059200060320       BEGSR uscita;
059300060320
059400060330         WRITE frcDta;
059500060321         CLOSE(E) cnc0wa00d;
059600060321         IF prmRtnCode = ' ';
059700060321           prmRtnCode = 'DONE';
059800060321         ENDIF;
059900060320         kpjbu = cnc0wa00s;
060000060320         RETURN;
060100060320
060200060320       ENDSR;
060300060320
060400060320      /END-FREE
