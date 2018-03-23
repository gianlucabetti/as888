000100060113      //************************************************************************
000200060113      //
000300060113      // Questo programma conta le registrazioni immesse dai PO in un periodo.
000400060113      //
000500060113      //************************************************************************
000600060116
000700060117     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('PJXBND')
000800060116
000900060113     Ftntud01l  UF A E           K DISK
001000060116     F                                     USROPN
001100060116     F                                     COMMIT
001200060113     FqSysPrt   O    F  132        PRINTER
001300060116     F                                     USROPN
001400060116     F                                     OFLIND(*INOF)
001500060112
001600060113      //************************************************************************
001700060113      //
001800060116      // Definizione delle procedure usate.
001900060113      //
002000060113      //************************************************************************
002100060116     D xStrCmt         PR
002200060116     D                                     EXTPROC('XSTRCMT')
002300060116     D  lckLvl                        1
002400060116     D                                     CONST
002500060116     D  cmtScope                      1
002600060116     D                                     CONST
002700060123     D tntudtar        PR
002800060123     D                                     EXTPGM('TNTUDTAR')
002900060123     D  tntudtads
003000060123     D                                     LIKEDS(tntudtads)
003100060116
003200060116      //************************************************************************
003300060116      //
003400060116      // Definizione delle costanti.
003500060116      //
003600060116      //************************************************************************
003700060116     D LckLvlChg       C                   '1'
003800060116     D CmtScopeActGrp  C                   '1'
003900060117     D PrimaNotaGenerica...
004000060117     D                 C                   '400'
004100060117     D IncassoBonifico...
004200060117     D                 C                   '410'
004300060112
004400060113      //************************************************************************
004500060113      //
004600060113      // Definizione delle strutture dati.
004700060113      //
004800060113      //************************************************************************
004900060116     D kpjba         E DS
005000060113     D tntu05ds      E DS
005100060116     D kdsTntud01l   E DS
005200060113     D                                     EXTNAME(tntud01l:*KEY)
005300060113     D                                     INZ
005400060113     D andiz00f      E DS
005500060113     D                                     BASED(nullPtr)
005600060113     D                                     PREFIX(pjz)
005700060113     D cndizion      E DS
005800060113     D                                     BASED(nullPtr)
005900060113     D                                     PREFIX(diz)
006000060123     D tntudtads     E DS
006100060123     D                                     INZ
006200060113
006300060113      //************************************************************************
006400060113      //
006500060113      // Definizione delle variabili
006600060113      //
006700060113      //************************************************************************
006800060113     D nullPtr         S               *
006900060113     D istruzioneSQL   S           4096
007000051214     D                                     VARYING
007100060113     D comDti          S               D
007200060113     D comDtf          S               D
007300060113     D conta           S             10I 0
007400060116
007500060113      //************************************************************************
007600060113      //
007700060113      // Definizione parametri procedura.
007800060113      //
007900060113      //************************************************************************
008000060116     C     *ENTRY        PLIST
008100060116     C                   PARM                    kpjba
008200060113
008300060113      //************************************************************************
008400060113      //
008500060113      // Main.
008600060113      //
008700060113      //************************************************************************
008800030115
008900060112     C/EXEC SQL
009000060112     C+ SET OPTION DYNUSRPRF = *OWNER
009100060112     C/END-EXEC
009200060112
009300060113      /FREE
009400060113
009500060113       // Conto le registrazioni contabili di prima nota generica.
009600060117       IF t05mult = ' ' OR t05mult = PrimaNotaGenerica;
009700060113         EXSR conta400;
009800060113       ENDIF;
009900060116
010000060117       // Conto le registrazioni contabili di incasso con bonifico.
010100060117       IF t05mult = ' ' OR t05mult = IncassoBonifico;
010200060116         EXSR conta410;
010300060116       ENDIF;
010400060123
010500060123       §dtaAzn = kCoAz;
010600060123       §dtaDta = t05dtf;
010700060123       tntudtar(tntudtads);
010800060113
010900060117       EXSR uscita;
011000060116
011100060116       //***********************************************************************
011200060116       //
011300060116       // Operazioni iniziali.
011400060116       //
011500060116       //***********************************************************************
011600060116       BEGSR *INZSR;
011700060116
011800060116         tntu05ds = kpjbu;
011900060116         comDti = %DATE(t05dti);
012000060116         comDtf = %DATE(t05dtf);
012100060116         *INOF = *ON;
012200060116         xStrCmt(LckLvlChg:CmtScopeActGrp);
012300060116         OPEN tntud01l;
012400060116
012500060116       ENDSR;
012600060113
012700060113       //***********************************************************************
012800060113       //
012900060113       // Conteggio delle registrazioni contabili di prima nota generica.
013000060113       //
013100060113       //***********************************************************************
013200060113       BEGSR conta400;
013300060113
013400060117         tudMult = PrimaNotaGenerica;
013500060113
013600060117         istruzioneSQL = 'SELECT UTEFIL, REGDTIMM, COUNT(*) +
013700060113         FROM NDREG00F JOIN ANUTN00F ON REGUTEIMM = UTNCUT +
013800060113         JOIN AZUTE00F ON UTNPRF = UTEUTE +
013900060113         WHERE REGCTB = ''CG'' AND REGDOCIVA = ''0'' AND UTEFIL <> 46 +
014000060113         AND REGDTIMM BETWEEN ''' + %CHAR(comDti)
014100060113         + '''' + ' AND ''' + %CHAR(comDtf) + '''';
014200060113
014300060113         // Aggiungo il filtro sul PO.
014400060113         IF t05fil <> 0;
014500060117           istruzioneSQL += ' AND UTEFIL = ' + %CHAR(t05fil);
014600060113         ENDIF;
014700060113
014800060116         // Raggruppamento.
014900060117         istruzioneSQL += ' GROUP BY UTEFIL, REGDTIMM';
015000060113
015100060113         // Solo lettura.
015200060113         istruzioneSQL += ' FOR READ ONLY';
015300060113
015400060113      /END-FREE
015500060113     C/EXEC SQL
015600060113     C+ PREPARE sPrimaNota FROM :istruzioneSQL
015700060113     C/END-EXEC
015800060117     C
015900060113     C/EXEC SQL
016000060113     C+ DECLARE cPrimaNota CURSOR FOR sPrimaNota
016100060113     C/END-EXEC
016200060117     C
016300060113     C/EXEC SQL
016400060113     C+ OPEN cPrimaNota
016500060113     C/END-EXEC
016600060113      /FREE
016700060113
016800060116         DOU sqlCod = 100;
016900060113
017000060113      /END-FREE
017100060113     C/EXEC SQL
017200060117     C+ FETCH NEXT FROM cPrimaNota INTO :tudFil, :tudDta, :conta
017300060113     C/END-EXEC
017400060113      /FREE
017500060113
017600060113           SELECT;
017700060116             WHEN sqlCod = 100;
017800060116               COMMIT;
017900060116               LEAVE;
018000060116             WHEN sqlCod < 0;
018100060116               ROLBK;
018200060116               *INH1 = *ON;
018300060117               DUMP;
018400060116               EXSR uscita;
018500060116             OTHER;
018600060116               EXSR wrtTntud;
018700060113           ENDSL;
018800060113
018900060113         ENDDO;
019000060113
019100060113      /END-FREE
019200060113     C/EXEC SQL
019300060113     C+ CLOSE cPrimaNota
019400060113     C/END-EXEC
019500060113      /FREE
019600060113
019700060113       ENDSR;
019800060116
019900060116       //***********************************************************************
020000060116       //
020100060117       // Conteggio delle registrazioni contabili di incasso con bonifico.
020200060116       // Il risultato lo sottraggo dal conteggio delle registrazioni immesse.
020300060116       //
020400060116       //***********************************************************************
020500060116       BEGSR conta410;
020600060116
020700060117         istruzioneSQL = 'SELECT UTEFIL, REGDTREG, COUNT(*) +
020800060116         FROM NDREG00F JOIN ANUTN00F ON REGUTEIMM = UTNCUT +
020900060116         JOIN AZUTE00F ON UTNPRF = UTEUTE +
021000060117         JOIN ANOPE00F ON REGSOCIETA = OPESOCIETA AND REGCAUSTES = OPECAUSALE +
021100060116         WHERE REGCTB = ''CG'' AND REGDOCIVA = ''0'' AND UTEFIL <> 46 +
021200060117         AND REGSOCIETA = ''201'' +
021300060117         AND OPECAUBAN <> '' '' AND REGFUNGEN = ''S'' +
021400060117         AND REGCLASSE = ''IC'' AND REGDTREG BETWEEN ''' + %CHAR(comDti)
021500060116         + '''' + ' AND ''' + %CHAR(comDtf) + '''';
021600060117
021700060117         // Aggiungo il filtro sul PO.
021800060117         IF t05fil <> 0;
021900060117           istruzioneSQL += ' AND UTEFIL = ' + %CHAR(t05fil);
022000060117         ENDIF;
022100060117
022200060117         // Raggruppamento.
022300060117         istruzioneSQL += ' GROUP BY UTEFIL, REGDTREG';
022400060117
022500060117         // Solo lettura.
022600060117         istruzioneSQL += ' FOR READ ONLY';
022700060116
022800060116      /END-FREE
022900060116     C/EXEC SQL
023000060116     C+ PREPARE sIncassi FROM :istruzioneSQL
023100060116     C/END-EXEC
023200060117     C
023300060116     C/EXEC SQL
023400060116     C+ DECLARE cIncassi CURSOR FOR sIncassi
023500060116     C/END-EXEC
023600060117     C
023700060116     C/EXEC SQL
023800060116     C+ OPEN cIncassi
023900060116     C/END-EXEC
024000060116      /FREE
024100060116
024200060116         DOU sqlCod = 100;
024300060116
024400060116      /END-FREE
024500060116     C/EXEC SQL
024600060117     C+ FETCH NEXT FROM cIncassi INTO :tudFil, :tudDta, :conta
024700060116     C/END-EXEC
024800060116      /FREE
024900060116
025000060116           SELECT;
025100060116             WHEN sqlCod = 100;
025200060116               COMMIT;
025300060116               LEAVE;
025400060116             WHEN sqlCod < 0;
025500060116               ROLBK;
025600060117               *INH2 = *ON;
025700060117               DUMP;
025800060116               EXSR uscita;
025900060116             OTHER;
026000060117               tudMult = IncassoBonifico;
026100060116               EXSR wrtTntud;
026200060117               tudMult = PrimaNotaGenerica;
026300060116               EXSR subFrom;
026400060116           ENDSL;
026500060116
026600060116         ENDDO;
026700060116
026800060116      /END-FREE
026900060116     C/EXEC SQL
027000060116     C+ CLOSE cIncassi
027100060116     C/END-EXEC
027200060116      /FREE
027300060116
027400060116       ENDSR;
027500060112
027600060113       //***********************************************************************
027700060113       //
027800060113       // Scrittura TNTUD00F.
027900060113       //
028000060113       //***********************************************************************
028100060113       BEGSR wrtTntud;
028200060113
028300060113         CLEAR tudCli;
028400060116         CHAIN %KDS(kdsTntud01l) tntud01l;
028500060113         tudQta = conta;
028600060116
028700060113         IF %FOUND(tntud01l);
028800060113           EXSR prtLog;
028900060113           UPDATE tntud000 %FIELDS(tudQta);
029000060113         ELSE;
029100060113           WRITE tntud000;
029200060113         ENDIF;
029300060113
029400060113       ENDSR;
029500060113
029600060113       //***********************************************************************
029700060113       //
029800060116       // Sottraggo quantità da voce XXX.
029900060113       //
030000060113       //***********************************************************************
030100060116       BEGSR subFrom;
030200060113
030300060116         CHAIN %KDS(kdsTntud01l) tntud01l;
030400060113         IF %FOUND(tntud01l);
030500060113           tudQta -= conta;
030600060113           UPDATE tntud000 %FIELDS(tudQta);
030700060113         ENDIF;
030800060113
030900060113       ENDSR;
031000060112
031100060113       //***********************************************************************
031200060113       //
031300060113       // Stampo anomalia.
031400060113       //
031500060113       //***********************************************************************
031600060113       BEGSR prtLog;
031700060113
031800060116         IF NOT %OPEN(qSysPrt);
031900060116           OPEN qSysPrt;
032000060116         ENDIF;
032100060116
032200060113         IF *INOF;
032300060113           EXCEPT testa;
032400060113           RESET *INOF;
032500060113         ENDIF;
032600060113
032700060113         EXCEPT anomalia;
032800060113
032900060113       ENDSR;
033000060113
033100060113       //***********************************************************************
033200060113       //
033300060113       // Uscita.
033400060113       //
033500060113       //***********************************************************************
033600060113       BEGSR uscita;
033700060113
033800060116         *INLR = *ON;
033900060116         RETURN;
034000060113
034100060113       ENDSR;
034200060112
034300060113      /END-FREE
034400060113
034500060113      //************************************************************************
034600060113      //
034700060113      // Stampa log.
034800060113      //
034900060113      //************************************************************************
035000060116     OqSysPrt   E            testa            02
035100051229     O                                           41 'Anomalie nel pgm TNTU18R'
035200051229     O                       PAGE          Z    132
035300051229     O          E            anomalia    1
035400051229     O                                           +0 'Si è eseguito un UPDATE pe-
035500051229     O                                              r il moltiplicatore'
035600060116     O                       tudMult             +1
035700051229     O                                           +1 'P.O.'
035800060116     O                       tudFil              +1
035900051229     O                                           +1 'Data'
036000060116     O                       tudDta              +1
