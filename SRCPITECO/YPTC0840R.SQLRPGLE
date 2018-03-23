000100121114      //************************************************************************
000200121114      //
000300121114      // Tester di YPTCMSEQ.
000400121114      //
000500121114      //************************************************************************
000600121129     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('PJXBND':'QC2LE':'YPTC')
000700121116
000800121116      //************************************************************************
000900121116      //
001000121116      // File.
001100121116      //
001200121116      //************************************************************************
001300121116     Fy084log   O    F  132        PRINTER EXTFILE('QSYSPRT')
001400121116
001500121114      //************************************************************************
001600121114      //
001700121114      // Strutture dati.
001800121114      //
001900121114      //************************************************************************
002000121114     D kpjba         E DS                  QUALIFIED
002100121114     D ndbmv00f      E DS                  QUALIFIED INZ
002200121114     D this            DS                  QUALIFIED INZ
002300121114     D  segno                         1A
002400121114     D  rifIntern                    10A
002500121114     D  usrPrf                       10A
002600121114     D  societa                       3A
002700121116     D  descrizione                 250A
002800121114     D  nrTeso                        9P 0
002900121114     D  autMan                        1A
003000121114     D  dataOperazioneMinima...
003100121114     D                                 D
003200121114     D  esito                        10I 0
003300121114     D  esitoProj                     1A
003400121114     D  newFileDone                    N
003500121116     D log             DS           132
003600121114
003700121114      //************************************************************************
003800121114      //
003900121114      // Costanti.
004000121114      //
004100121114      //************************************************************************
004200121114      /COPY GAITRASRC/SRCCONST,YPTCMSEQ
004300121114     D PROJ_ESITO_OK...
004400121114     D                 C                   '0'
004500121114
004600121114     D*--------------------------------------------------
004700121114     D* Procedure name: UpdNdbmv00f
004800121114     D* Purpose:        Aggiorna NDBMV00F.
004900121114     D* Returns:        Esito.
005000121114     D*--------------------------------------------------
005100121114     D UpdNdbmv00f     PR            10I 0
005200121129
005300121129     D*--------------------------------------------------
005400121129     D* Procedure name: Main
005500121129     D* Purpose:
005600121129     D* Returns:        Esito.
005700121129     D*--------------------------------------------------
005800121129     D Main            PR            10I 0
005900121129
006000121129
006100121114
006200121114      //************************************************************************
006300121114      //
006400121114      // Prototipi.
006500121114      //
006600121114      //************************************************************************
006700121114      /COPY GAITRASRC/SRCPROTOPR,YPTCMSEQ
006800121114      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
006900121129      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
007000121114
007100121114      //************************************************************************
007200121114      //
007300121114      // Parametri.
007400121114      //
007500121114      //************************************************************************
007600121114     C     *ENTRY        PLIST
007700121114     C                   PARM                    kpjba
007800121114
007900121114      //************************************************************************
008000121114      //
008100121114      // Main.
008200121114      //
008300121114      //************************************************************************
008400121114
008500121114      /FREE
008600121114
008700121114       *INLR = *ON;
008800121114
008900121129       this.esito = Main();
009000121129
009100121129       IF this.esito < YPTCMSEQ_ESITO_OK;
009200121129         log = 'ERRORE ' + %CHAR(this.esito);
009300121129         WRITE y084log log;
009400121129         ExecuteCommand('DSPJOBLOG OUTPUT(*PRINT)');
009500121129       ENDIF;
009600121114
009700121114      /END-FREE
009800121114
009900121114     P*--------------------------------------------------
010000121114     P* Procedure name: UpdNdbmv00f
010100121114     P* Purpose:        Aggiorna NDBMV00F.
010200121114     P* Returns:        Esito.
010300121114     P*--------------------------------------------------
010400121114     P UpdNdbmv00f     B
010500121114     D UpdNdbmv00f     PI            10I 0
010600121114
010700121114     D retField        S             10I 0 STATIC
010800121114
010900121114      /FREE
011000121114
011100121114       RESET retField;
011200121114
011300121114       EXEC SQL
011400121114         UPDATE ndbmv00f
011500121114            SET ndbmv00f.bmvNrTeso = :this.nrTeso
011600121114              , ndbmv00f.bmvStatus = '1'
011700121114              , ndbmv00f.bmvDest = 'Y'
011800121114          WHERE CURRENT OF ndbmv00f
011900121114       ;
012000121114
012100121114       SELECT;
012200121114         WHEN sqlCode < *ZERO;
012300121114           DUMP(A);
012400121114           RETURN sqlCode;
012500121114       ENDSL;
012600121114
012700121114       RETURN retField;
012800121114
012900121114      /END-FREE
013000121114     P UpdNdbmv00f     E
013100121114
013200121129
013300121129     P*--------------------------------------------------
013400121129     P* Procedure name: Main
013500121129     P* Purpose:
013600121129     P* Returns:        Esito.
013700121129     P*--------------------------------------------------
013800121129     P Main            B
013900121129     D Main            PI            10I 0
014000121129
014100121129     D retField        S             10I 0
014200121129
014300121129      /FREE
014400121129
014500121129       EXEC SQL
014600121129         SELECT LEFT(tntbe00f.tbeUni, 10)
014700121129           INTO :this.dataOperazioneMinima
014800121129           FROM tntbe00f
014900121129           WHERE tntbe00f.tbeCod = '011' AND tntbe00f.tbeKe1 = '*DATA'
015000121129             AND tntbe00f.tbeKe2 = '*OPERAZIONE_MIN' AND tntbe00f.tbeLin = ''
015100121129             AND tntbe00f.tbeSif = '' AND tntbe00f.tbeAtb = ''
015200121129       ;
015300121129
015400121129       SELECT;
015500121129         WHEN sqlCode < *ZERO;
015600121129           DUMP(A);
015700121129           RETURN sqlCode;
015800121129       ENDSL;
015900121129
016000170607       EXEC SQL
016100170607         DECLARE ndbmv00f NO SCROLL CURSOR FOR
016200170607          SELECT ndbmv00f.bmvSys, ndbmv00f.bmvNrAsReg
016300170607               , ndbmv00f.bmvNrRigaM, ndbmv00f.bmvSocieta
016400170607               , ndbmv00f.bmvUnita, ndbmv00f.bmvDtReg
016500170607               , ndbmv00f.bmvDtOpe, ndbmv00f.bmvDtVal, ndbmv00f.bmvKcc
016600170607               , ndbmv00f.bmvKsc, ndbmv00f.bmvCauBan
016700170607               , CASE ndbmv00f.bmvDare WHEN 1 THEN 'D' ELSE 'A' END
016800170607               , ndbmv00f.bmvImporto, ndbmv00f.bmvImportD
016900170607               , ndbmv00f.bmvDivisa
017000170607               , ndbmv00f.bmvBanca
017100170607               , ndbmv00f.bmvLinea
017200170607            FROM ndbmv00f
017300170607           WHERE ndbmv00f.bmvCtb = 'CG'
017400170607             AND ndbmv00f.bmvKcc = '000201'
017401170607             AND ndbmv00f.bmvTpRegz = 'D'
017500170607             AND ndbmv00f.bmvStatus = ''
017600170607             AND ndbmv00f.bmvCauBan NOT IN ('PTCA','PTCD')
017700170607             AND ndbmv00f.bmvDtOpe >= :this.dataOperazioneMinima
017701170607             AND ndbmv00f.bmvSocieta IN (SELECT societa FROM ysocgrp0v)
017800170607        ORDER BY ndbmv00f.bmvSocieta
017900170607             FOR UPDATE OF bmvNrTeso, bmvStatus, bmvDest
018000170607            WITH RS USE AND KEEP EXCLUSIVE LOCKS
018100170607            SKIP LOCKED DATA;
018300121129
018400121129       EXEC SQL OPEN ndbmv00f;
018500121129
018600121129       SELECT;
018700121129         WHEN sqlCode < *ZERO;
018800121129           DUMP(A);
018900121129           RETURN sqlCode;
019000121129       ENDSL;
019100121129
019200121129       retField = YPTCMSEQ_Init( kpjba );
019300121129
019400121129       IF retField < YPTCMSEQ_ESITO_OK;
019500121129         RETURN retField;
019600121129       ENDIF;
019700121129
019800121129       DOU sqlCode < *ZERO;
019900121129
020000121129         EXEC SQL
020100121129           FETCH NEXT FROM ndbmv00f
020200121129             INTO :ndbmv00f.bmvSys, :ndbmv00f.bmvNrAsReg, :ndbmv00f.bmvNrRigaM
020300121129                , :ndbmv00f.bmvSocieta, :ndbmv00f.bmvUnita, :ndbmv00f.bmvDtReg
020400121129                , :ndbmv00f.bmvDtOpe, :ndbmv00f.bmvDtVal, :ndbmv00f.bmvKcc
020500121129                , :ndbmv00f.bmvKsc, :ndbmv00f.bmvCauBan, :this.segno
020600121129                , :ndbmv00f.bmvImporto, :ndbmv00f.bmvImportD
020700130206                , :ndbmv00f.bmvDivisa, :ndbmv00f.bmvBanca, :ndbmv00f.bmvLinea
020800121129         ;
020900121129
021000121129         SELECT;
021100121129           WHEN sqlCode = 100;
021200121129             IF this.newFileDone;
021300121129               retField = YPTCMSEQ_EndFile( *ON );
021400121129             ENDIF;
021500121129             LEAVE;
021600121129           WHEN sqlCode < *ZERO;
021700121129             DUMP(A);
021800121129             LEAVE;
021900121129         ENDSL;
022000121129
022100121129         IF NOT this.newFileDone;
022200121129           retField = YPTCMSEQ_NewFile();
022300121129           IF retField < YPTCMSEQ_ESITO_OK;
022400121129             LEAVE;
022500121129           ENDIF;
022600121129           this.newFileDone = *ON;
022700121129         ENDIF;
022800121129
022900121129         EXEC SQL
023000121129           SELECT anutn00f.utnPrf
023100121129             INTO :this.usrPrf
023200121129             FROM ndreg00f
023300121129             JOIN anutn00f
023400121129               ON ndreg00f.regUteImm = anutn00f.utnCut
023500121129            WHERE ndreg00f.regSys = :ndbmv00f.bmvSys
023600121129              AND ndreg00f.regNrAsReg = :ndbmv00f.bmvNrAsReg
023700121129         ;
023800121129
023900121129         SELECT;
024000121129           WHEN sqlCode < *ZERO;
024100121129             DUMP(A);
024200121129             LEAVE;
024300121129         ENDSL;
024400121129
024500121129         Proj_Numeratore( '3' : *BLANK : *BLANK : *BLANK : *BLANK : 'NRHB' : *ON
024600121129                        : %DATE() : *OFF : *OFF : this.esitoProj : this.nrTeso
024700121129                        : this.autMan );
024800121129
024900121129         IF this.esitoProj <> PROJ_ESITO_OK;
025000121129           LEAVE;
025100121129         ENDIF;
025200121129
025300121129         retField = YPTCMSEQ_NewRegistrazione( ndbmv00f.bmvSys
025400121129                                            : ndbmv00f.bmvNrAsReg
025500121129                                            : ndbmv00f.bmvNrRigaM
025600121129                                            : ndbmv00f.bmvSocieta
025700121129                                            : ndbmv00f.bmvDtReg
025800121129                                            : this.nrTeso
025900121129                                            : this.usrPrf
026000121129                                            : ndbmv00f.bmvKcc
026100121129                                            : ndbmv00f.bmvKsc
026200121129                                            : this.segno
026300121129                                            : ndbmv00f.bmvDivisa
026400121129                                            : %DEC(ndbmv00f.bmvImportD : 13 : 2)
026500121129                                            : %DEC(ndbmv00f.bmvImporto : 13 : 2)
026600121129                                            : ndbmv00f.bmvCauBan
026700121129                                            : ndbmv00f.bmvDtOpe
026800121129                                            : ndbmv00f.bmvDtVal
026900121129                                            : ndbmv00f.bmvUnita
027000121129                                            : this.descrizione
027100130206                                            : ndbmv00f.bmvBanca
027200130206                                            : ndbmv00f.bmvLinea
027300121129                                            );
027400121129
027500130208         SELECT;
027600130208           WHEN retField = YPTCMSEQ_ESITO_CAUSALE_OMESSA;
027700140409             YPTCMSEQ_EndRegistrazione( *OFF );
027800130208           WHEN retField < YPTCMSEQ_ESITO_OK;
027900140409             YPTCMSEQ_EndRegistrazione( *OFF );
028000130208             LEAVE;
028100130208           OTHER;
028200130208             retField = YPTCMSEQ_EndRegistrazione( *ON );
028300130208             IF retField < YPTCMSEQ_ESITO_OK;
028400130208               LEAVE;
028500130208             ELSE;
028600130208               retField = UpdNdbmv00f();
028700130208               IF retField < YPTCMSEQ_ESITO_OK;
028800130208                 LEAVE;
028900130208               ENDIF;
029000130208             ENDIF;
029100130208         ENDSL;
029200121129
029300121129       ENDDO;
029400121129
029500121129       EXEC SQL CLOSE ndbmv00f;
029600121129
029700121129       IF retField < YPTCMSEQ_ESITO_OK;
029800121129         EXEC SQL ROLLBACK;
029900121129         log = 'ERRORE ' + %CHAR(retField)
030000121129             + ' NDBMV00F ' + %EDITC(ndbmv00f.bmvNrAsReg : 'X')
030100121129             + ' ' + %EDITC(ndbmv00f.bmvNrRigaM : 'X');
030200121129         WRITE y084log log;
030300121129         retField = YPTCMSEQ_EndFile( *OFF );
030400121129       ELSE;
030500121129         retField = YPTCMSEQ_EndFile( *ON );
030600121129         IF retField < YPTCMSEQ_ESITO_OK;
030700121129           EXEC SQL ROLLBACK;
030800121129           log = 'ERRORE ' + %CHAR(retField);
030900121129           WRITE y084log log;
031000121129         ELSE;
031100121129           EXEC SQL COMMIT;
031200121129         ENDIF;
031300121129       ENDIF;
031400121129
031500121129       YPTCMSEQ_Finalize();
031600121129
031700121129       RETURN retField;
031800121129
031900121129      /END-FREE
032000121129     P Main            E
032100121129
