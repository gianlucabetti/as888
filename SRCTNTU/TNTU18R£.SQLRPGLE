000100060113      //************************************************************************
000200060113      //
000300060113      // Questo programma conta le registrazioni immesse dai PO in un periodo.
000400060113      //
000500060113      //************************************************************************
000600060116
000700060116     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE':'PJXBND')
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
002100060112     D atoi            PR            10I 0
002200060112     D                                     EXTPROC('atoi')
002300060112     D  char                           *
002400060112     D                                     VALUE
002500060112     D                                     OPTIONS(*STRING)
002600060116     D xStrCmt         PR
002700060116     D                                     EXTPROC('XSTRCMT')
002800060116     D  lckLvl                        1
002900060116     D                                     CONST
003000060116     D  cmtScope                      1
003100060116     D                                     CONST
003200060116
003300060116      //************************************************************************
003400060116      //
003500060116      // Definizione delle costanti.
003600060116      //
003700060116      //************************************************************************
003800060116     D LckLvlChg       C                   '1'
003900060116     D CmtScopeActGrp  C                   '1'
004000060112
004100060113      //************************************************************************
004200060113      //
004300060113      // Definizione delle strutture dati.
004400060113      //
004500060113      //************************************************************************
004600060116     D kpjba         E DS
004700060113     D tntu05ds      E DS
004800060116     D kdsTntud01l   E DS
004900060113     D                                     EXTNAME(tntud01l:*KEY)
005000060113     D                                     INZ
005100060113     D andiz00f      E DS
005200060113     D                                     BASED(nullPtr)
005300060113     D                                     PREFIX(pjz)
005400060113     D cndizion      E DS
005500060113     D                                     BASED(nullPtr)
005600060113     D                                     PREFIX(diz)
005700060113
005800060113      //************************************************************************
005900060113      //
006000060113      // Definizione delle variabili
006100060113      //
006200060113      //************************************************************************
006300060113     D nullPtr         S               *
006400060113     D istruzioneSQL   S           4096
006500051214     D                                     VARYING
006600060113     D comDti          S               D
006700060113     D comDtf          S               D
006800060113     D unita           S                   LIKE(pjzUnita)
006900060113     D conta           S             10I 0
007000060116
007100060113      //************************************************************************
007200060113      //
007300060113      // Definizione parametri procedura.
007400060113      //
007500060113      //************************************************************************
007600060116     C     *ENTRY        PLIST
007700060116     C                   PARM                    kpjba
007800060113
007900060113      //************************************************************************
008000060113      //
008100060113      // Main.
008200060113      //
008300060113      //************************************************************************
008400030115
008500060112     C/EXEC SQL
008600060112     C+ SET OPTION DYNUSRPRF = *OWNER
008700060112     C/END-EXEC
008800060112
008900060113      /FREE
009000060113
009100060113       // Conto le registrazioni contabili di prima nota generica.
009200060113       IF t05mult = ' ' OR t05mult = '400';
009300060113         EXSR conta400;
009400060113       ENDIF;
009500060116
009600060116       // Conto le registrazioni contabili di incasso.
009700060116       IF t05mult = ' ' OR t05mult = '410';
009800060116         EXSR conta410;
009900060116       ENDIF;
010000060113
010100060113       // Conto le registrazioni contabili di incasso fattura pre pagata.
010200060113       IF t05mult = ' ' OR t05mult = '280';
010300060113         EXSR conta280;
010400060113       ENDIF;
010500060113
010600060113       EXSR uscita;
010700060116
010800060116       //***********************************************************************
010900060116       //
011000060116       // Operazioni iniziali.
011100060116       //
011200060116       //***********************************************************************
011300060116       BEGSR *INZSR;
011400060116
011500060116         tntu05ds = kpjbu;
011600060116         comDti = %DATE(t05dti);
011700060116         comDtf = %DATE(t05dtf);
011800060116         *INOF = *ON;
011900060116         xStrCmt(LckLvlChg:CmtScopeActGrp);
012000060116         OPEN tntud01l;
012100060116
012200060116       ENDSR;
012300060113
012400060113       //***********************************************************************
012500060113       //
012600060113       // Conteggio delle registrazioni contabili di prima nota generica.
012700060113       //
012800060113       //***********************************************************************
012900060113       BEGSR conta400;
013000060113
013100060113         tudMult = '400';
013200060113
013300060113         istruzioneSQL = 'SELECT REGUNITA, REGDTIMM, COUNT(*) +
013400060113         FROM NDREG00F JOIN ANUTN00F ON REGUTEIMM = UTNCUT +
013500060113         JOIN AZUTE00F ON UTNPRF = UTEUTE +
013600060113         WHERE REGCTB = ''CG'' AND REGDOCIVA = ''0'' AND UTEFIL <> 46 +
013700060113         AND REGDTIMM BETWEEN ''' + %CHAR(comDti)
013800060113         + '''' + ' AND ''' + %CHAR(comDtf) + '''';
013900060113
014000060113         // Aggiungo il filtro sul PO.
014100060113         IF t05fil <> 0;
014200060113           istruzioneSQL += ' AND REGUNITA = ''' + %EDITC(t05fil:'X') + '''';
014300060113         ENDIF;
014400060113
014500060116         // Raggruppamento.
014600060113         istruzioneSQL += ' GROUP BY REGUNITA, REGDTIMM';
014700060113
014800060113         // Solo lettura.
014900060113         istruzioneSQL += ' FOR READ ONLY';
015000060113
015100060113      /END-FREE
015200060113     C/EXEC SQL
015300060113     C+ PREPARE sPrimaNota FROM :istruzioneSQL
015400060113     C/END-EXEC
015500060113     C/EXEC SQL
015600060113     C+ DECLARE cPrimaNota CURSOR FOR sPrimaNota
015700060113     C/END-EXEC
015800060113     C/EXEC SQL
015900060113     C+ OPEN cPrimaNota
016000060113     C/END-EXEC
016100060113      /FREE
016200060113
016300060116         DOU sqlCod = 100;
016400060113
016500060113      /END-FREE
016600060113     C/EXEC SQL
016700060113     C+ FETCH NEXT FROM cPrimaNota INTO :unita, :tudDta, :conta
016800060113     C/END-EXEC
016900060113      /FREE
017000060113
017100060113           SELECT;
017200060116             WHEN sqlCod = 100;
017300060116               COMMIT;
017400060116               LEAVE;
017500060116             WHEN sqlCod < 0;
017600060116               ROLBK;
017700060116               *INH1 = *ON;
017800060116               EXSR uscita;
017900060116             OTHER;
018000060116               EXSR wrtTntud;
018100060113           ENDSL;
018200060113
018300060113         ENDDO;
018400060113
018500060113      /END-FREE
018600060113     C/EXEC SQL
018700060113     C+ CLOSE cPrimaNota
018800060113     C/END-EXEC
018900060113      /FREE
019000060113
019100060113       ENDSR;
019200060116
019300060116       //***********************************************************************
019400060116       //
019500060116       // Conteggio delle registrazioni contabili di incasso.
019600060116       // Il risultato lo sottraggo dal conteggio delle registrazioni immesse.
019700060116       //
019800060116       //***********************************************************************
019900060116       BEGSR conta410;
020000060116
020100060116         istruzioneSQL = 'SELECT REGUNITA, REGDTIMM, COUNT(*) +
020200060116         FROM NDREG00F JOIN ANUTN00F ON REGUTEIMM = UTNCUT +
020300060116         JOIN AZUTE00F ON UTNPRF = UTEUTE +
020400060116         WHERE REGCTB = ''CG'' AND REGDOCIVA = ''0'' AND UTEFIL <> 46 +
020500060116         AND REGCLASSE = ''IC'' AND REGDTIMM BETWEEN ''' + %CHAR(comDti)
020600060116         + '''' + ' AND ''' + %CHAR(comDtf) + '''';
020700060116
020800060116         // Aggiungo il filtro sul PO.
020900060116         IF t05fil <> 0;
021000060116           istruzioneSQL += ' AND REGUNITA = ''' + %EDITC(t05fil:'X') + '''';
021100060116         ENDIF;
021200060116
021300060116         // Raggruppamento.
021400060116         istruzioneSQL += ' GROUP BY REGUNITA, REGDTIMM';
021500060116
021600060116         // Solo lettura.
021700060116         istruzioneSQL += ' FOR READ ONLY';
021800060116
021900060116      /END-FREE
022000060116     C/EXEC SQL
022100060116     C+ PREPARE sIncassi FROM :istruzioneSQL
022200060116     C/END-EXEC
022300060116     C/EXEC SQL
022400060116     C+ DECLARE cIncassi CURSOR FOR sIncassi
022500060116     C/END-EXEC
022600060116     C/EXEC SQL
022700060116     C+ OPEN cIncassi
022800060116     C/END-EXEC
022900060116      /FREE
023000060116
023100060116         DOU sqlCod = 100;
023200060116
023300060116      /END-FREE
023400060116     C/EXEC SQL
023500060116     C+ FETCH NEXT FROM cIncassi INTO :unita, :tudDta, :conta
023600060116     C/END-EXEC
023700060116      /FREE
023800060116
023900060116           SELECT;
024000060116             WHEN sqlCod = 100;
024100060116               COMMIT;
024200060116               LEAVE;
024300060116             WHEN sqlCod < 0;
024400060116               ROLBK;
024500060116               *INH1 = *ON;
024600060116               EXSR uscita;
024700060116             OTHER;
024800060116               tudMult = '410';
024900060116               EXSR wrtTntud;
025000060116               tudMult = '400';
025100060116               EXSR subFrom;
025200060116           ENDSL;
025300060116
025400060116         ENDDO;
025500060116
025600060116      /END-FREE
025700060116     C/EXEC SQL
025800060116     C+ CLOSE cIncassi
025900060116     C/END-EXEC
026000060116      /FREE
026100060116
026200060116       ENDSR;
026300060112
026400060113       //***********************************************************************
026500060113       //
026600060113       // Conteggio delle registrazioni contabili di incasso fatture pre pagate.
026700060116       // Il risultato lo sottraggo dal conteggio degli incassi.
026800060116       // Per rendere il tutto più veloce cerco le registrazioni solo nella
026900060116       // società Bartolini e uso la data di registrazione anzichè la data di
027000060116       // immissione, che tanto sono sempre uguali.
027100060113       //
027200060113       //***********************************************************************
027300060113       BEGSR conta280;
027400060113
027500060116         istruzioneSQL = 'SELECT REGUNITA, REGDTREG, COUNT(*) +
027600060116         FROM NDREG00F JOIN ANUTN00F ON REGUTEIMM = UTNCUT +
027700060116         JOIN AZUTE00F ON UTNPRF = UTEUTE +
027800060113         WHERE REGSOCIETA = ''201'' AND REGCTB = ''CG'' AND REGDOCIVA = ''0'' +
027900060116         AND UTEFIL <> 46 +
028000060113         AND REGKCCCPA = ''000151'' AND SUBSTR(REGKSCCPA, 5, 4) = ''8888'' +
028100060113         AND REGCLASSE = ''IC'' AND REGDTREG BETWEEN ''' + %CHAR(comDti)
028200060113         + '''' + ' AND ''' + %CHAR(comDtf) + '''';
028300060113
028400060113         // Aggiungo il filtro sul PO.
028500060113         IF t05fil <> 0;
028600060113           istruzioneSQL += ' AND REGUNITA = ''' + %EDITC(t05fil:'X') + '''';
028700060113         ENDIF;
028800060113
028900060116         // Raggruppamento.
029000060113         istruzioneSQL += ' GROUP BY REGUNITA, REGDTREG';
029100060113
029200060113         // Solo lettura.
029300060113         istruzioneSQL += ' FOR READ ONLY';
029400060113
029500060113      /END-FREE
029600060113     C/EXEC SQL
029700060113     C+ PREPARE sIncassiPrePag FROM :istruzioneSQL
029800060113     C/END-EXEC
029900060113     C/EXEC SQL
030000060113     C+ DECLARE cIncassiPrePag CURSOR FOR sIncassiPrePag
030100060113     C/END-EXEC
030200060113     C/EXEC SQL
030300060113     C+ OPEN cIncassiPrePag
030400060113     C/END-EXEC
030500060113      /FREE
030600060113
030700060116         DOU sqlCod = 100;
030800051222
030900060113      /END-FREE
031000060113     C/EXEC SQL
031100060113     C+ FETCH NEXT FROM cIncassiPrePag INTO :unita, :tudDta, :conta
031200060113     C/END-EXEC
031300060113      /FREE
031400060113
031500060113           SELECT;
031600060116             WHEN sqlCod = 100;
031700060116               COMMIT;
031800060116               LEAVE;
031900060116             WHEN sqlCod < 0;
032000060116               ROLBK;
032100060116               *INH1 = *ON;
032200060116               EXSR uscita;
032300060116             OTHER;
032400060116               tudMult = '280';
032500060116               EXSR wrtTntud;
032600060116               tudMult = '410';
032700060116               EXSR subFrom;
032800060113           ENDSL;
032900060113
033000060113         ENDDO;
033100060116
033200060113      /END-FREE
033300051222     C/EXEC SQL
033400060112     C+ CLOSE cIncassiPrePag
033500051222     C/END-EXEC
033600060113      /FREE
033700060113
033800060113       ENDSR;
033900060112
034000060113       //***********************************************************************
034100060113       //
034200060113       // Scrittura TNTUD00F.
034300060113       //
034400060113       //***********************************************************************
034500060113       BEGSR wrtTntud;
034600060113
034700060113         tudFil = atoi(unita);
034800060113         CLEAR tudCli;
034900060116         CHAIN %KDS(kdsTntud01l) tntud01l;
035000060113         tudQta = conta;
035100060116
035200060113         IF %FOUND(tntud01l);
035300060113           EXSR prtLog;
035400060113           UPDATE tntud000 %FIELDS(tudQta);
035500060113         ELSE;
035600060113           WRITE tntud000;
035700060113         ENDIF;
035800060113
035900060113       ENDSR;
036000060113
036100060113       //***********************************************************************
036200060113       //
036300060116       // Sottraggo quantità da voce XXX.
036400060113       //
036500060113       //***********************************************************************
036600060116       BEGSR subFrom;
036700060113
036800060116         CHAIN %KDS(kdsTntud01l) tntud01l;
036900060113         IF %FOUND(tntud01l);
037000060113           tudQta -= conta;
037100060113           UPDATE tntud000 %FIELDS(tudQta);
037200060113         ENDIF;
037300060113
037400060113       ENDSR;
037500060112
037600060113       //***********************************************************************
037700060113       //
037800060113       // Stampo anomalia.
037900060113       //
038000060113       //***********************************************************************
038100060113       BEGSR prtLog;
038200060113
038300060116         IF NOT %OPEN(qSysPrt);
038400060116           OPEN qSysPrt;
038500060116         ENDIF;
038600060116
038700060113         IF *INOF;
038800060113           EXCEPT testa;
038900060113           RESET *INOF;
039000060113         ENDIF;
039100060113
039200060113         EXCEPT anomalia;
039300060113
039400060113       ENDSR;
039500060113
039600060113       //***********************************************************************
039700060113       //
039800060113       // Uscita.
039900060113       //
040000060113       //***********************************************************************
040100060113       BEGSR uscita;
040200060113
040300060116         *INLR = *ON;
040400060116         RETURN;
040500060113
040600060113       ENDSR;
040700060112
040800060113      /END-FREE
040900060113
041000060113      //************************************************************************
041100060113      //
041200060113      // Stampa log.
041300060113      //
041400060113      //************************************************************************
041500060116     OqSysPrt   E            testa            02
041600051229     O                                           41 'Anomalie nel pgm TNTU18R'
041700051229     O                       PAGE          Z    132
041800051229     O          E            anomalia    1
041900051229     O                                           +0 'Si è eseguito un UPDATE pe-
042000051229     O                                              r il moltiplicatore'
042100060116     O                       tudMult             +1
042200051229     O                                           +1 'P.O.'
042300060116     O                       tudFil              +1
042400051229     O                                           +1 'Data'
042500060116     O                       tudDta              +1
