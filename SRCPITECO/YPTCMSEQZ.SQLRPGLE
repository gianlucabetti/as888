000100121112      //************************************************************************
000200121112      //
000300121112      // Tester di YPTCMSEQ.
000400121112      //
000500121112      //************************************************************************
000600121112     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('YPTC')
000700121112
000800121112      //************************************************************************
000900121112      //
001000121112      // Strutture dati.
001100121112      //
001200121112      //************************************************************************
001300121112     D kpjba         E DS                  QUALIFIED
001400121112     D ndbmv00f      E DS                  QUALIFIED INZ
001500121112     D ndmov00f      E DS                  QUALIFIED INZ
001600121112     D ndreg00f      E DS                  QUALIFIED INZ
001700121112     D this            DS                  QUALIFIED INZ
001800121112     D  segno                         1A
001900121112     D  rifIntern                    10A
002000121112     D  usrPrf                       10A
002100121113     D  societa                       3A
002200121112
002300121112      //************************************************************************
002400121112      //
002500121112      // Costanti.
002600121112      //
002700121112      //************************************************************************
002800121112      /COPY GAITRASRC/SRCCONST,YPTCMSEQ
002900121112
003000121112      //************************************************************************
003100121112      //
003200121112      // Campi.
003300121112      //
003400121112      //************************************************************************
003500121112     D dataOperazioneMinima...
003600121112     D                 S               D
003700121112     D esito           S             10I 0
003800121112
003900121112      //************************************************************************
004000121112      //
004100121112      // Prototipi.
004200121112      //
004300121112      //************************************************************************
004400121112      /COPY GAITRASRC/SRCPROTOPR,YPTCMSEQ
004500121112
004600121112      //************************************************************************
004700121112      //
004800121112      // Parametri.
004900121112      //
005000121112      //************************************************************************
005100121112     C     *ENTRY        PLIST
005200121112     C                   PARM                    kpjba
005300121112
005400121112      //************************************************************************
005500121112      //
005600121112      // Main.
005700121112      //
005800121112      //************************************************************************
005900121112
006000121112      /FREE
006100121112
006200121112       *INLR = *ON;
006300121112
006400121112       EXEC SQL
006500121112         SELECT LEFT(tntbe00f.tbeUni, 10)
006600121112           INTO :dataOperazioneMinima
006700121112           FROM tntbe00f
006800121112           WHERE tntbe00f.tbeCod = '011' AND tntbe00f.tbeKe1 = '*DATA'
006900121112             AND tntbe00f.tbeKe2 = '*OPERAZIONE_MIN' AND tntbe00f.tbeLin = ''
007000121112             AND tntbe00f.tbeSif = '' AND tntbe00f.tbeAtb = ''
007100121112       ;
007200121112
007300121112       SELECT;
007400121112         WHEN sqlCode < *ZERO;
007500121112           DUMP(A);
007600121112           RETURN;
007700121112       ENDSL;
007800121112
007900121112       EXEC SQL
008000121112         DECLARE ndbmv00f NO SCROLL CURSOR FOR
008100121113           WITH m ( sys, nrAsReg, nrRigaM, societa, unita, dtReg, dtOpe, dtVal
008200121113                  , kcc, ksc, cauBan, segno, importo, importD, divisa ) AS
008300121113                  ( SELECT ndbmv00f.bmvSys, ndbmv00f.bmvNrAsReg
008400121113                         , ndbmv00f.bmvNrRigaM, ndbmv00f.bmvSocieta
008500121113                         , ndbmv00f.bmvUnita, ndbmv00f.bmvDtReg
008600121113                         , ndbmv00f.bmvDtOpe, ndbmv00f.bmvDtVal, ndbmv00f.bmvKcc
008700121113                         , ndbmv00f.bmvKsc, ndbmv00f.bmvCauBan
008800121113                         , CASE ndbmv00f.bmvDare WHEN 1 THEN 'D' ELSE 'A' END
008900121113                         , ndbmv00f.bmvImporto, ndbmv00f.bmvImportD
009000121113                         , ndbmv00f.bmvDivisa
009100121113                    FROM ndbmv00f
009200121113                    WHERE ndbmv00f.bmvCtb = 'CG'
009300121113                      AND ndbmv00f.bmvTpRegz = 'D'
009400121113                      AND ndbmv00f.bmvStatus = ''
009500121113                      AND ndbmv00f.bmvKcc = '000201'
009600121113                      AND ndbmv00f.bmvCauBan NOT IN ('PTCA','PTCD')
009700121113                      AND ndbmv00f.bmvDtOpe >= :dataOperazioneMinima )
009800121113           SELECT m.*, ndreg00f.regNrReg, anutn00f.utnPrf
009900121113           FROM m
010000121112           JOIN ndreg00f
010100121113             ON m.sys = ndreg00f.regSys
010200121113            AND m.nrAsReg = ndreg00f.regNrAsReg
010300121112           JOIN anutn00f
010400121112             ON ndreg00f.regUteImm = anutn00f.utnCut
010500121113           ORDER BY m.societa, m.dtOpe
010600121112           FOR READ ONLY
010700121112       ;
010800121112
010900121112       EXEC SQL OPEN ndbmv00f;
011000121112
011100121112       SELECT;
011200121112         WHEN sqlCode < *ZERO;
011300121112           DUMP(A);
011400121112           RETURN;
011500121112       ENDSL;
011600121112
011700121112       esito = YPTCMSEQ_Init( kpjba );
011800121112
011900121112       IF esito < YPTCMSEQ_ESITO_OK;
012000121112         RETURN;
012100121112       ENDIF;
012200121112
012300121112       DOU esito < YPTCMSEQ_ESITO_OK;
012400121112
012500121112         EXEC SQL
012600121112           FETCH NEXT FROM ndbmv00f
012700121112             INTO :ndbmv00f.bmvSys, :ndbmv00f.bmvNrAsReg, :ndbmv00f.bmvNrRigaM
012800121112                , :ndbmv00f.bmvSocieta, :ndbmv00f.bmvUnita, :ndbmv00f.bmvDtReg
012900121112                , :ndbmv00f.bmvDtOpe, :ndbmv00f.bmvDtVal, :ndbmv00f.bmvKcc
013000121112                , :ndbmv00f.bmvKsc, :ndbmv00f.bmvCauBan, :this.segno
013100121112                , :ndbmv00f.bmvImporto, :ndbmv00f.bmvImportD
013200121113                , :ndbmv00f.bmvDivisa, :ndreg00f.regNrReg, :this.usrPrf
013300121112         ;
013400121112
013500121112         SELECT;
013600121112           WHEN sqlCode = 100;
013700121113             IF this.societa <> *BLANK;
013800121113               esito = YPTCMSEQ_EndFile( *ON );
013900121113             ENDIF;
014000121112             LEAVE;
014100121112           WHEN sqlCode < *ZERO;
014200121112             DUMP(A);
014300121112             LEAVE;
014400121112         ENDSL;
014500121113
014600121113         IF ndbmv00f.bmvSocieta <> this.societa;
014700121113           IF this.societa <> *BLANK;
014800121113             esito = YPTCMSEQ_EndFile( *ON );
014900121113             IF esito < YPTCMSEQ_ESITO_OK;
015000121113               LEAVE;
015100121113             ENDIF;
015200121113           ENDIF;
015300121113           esito = YPTCMSEQ_NewFile();
015400121113           IF esito < YPTCMSEQ_ESITO_OK;
015500121113             LEAVE;
015600121113           ENDIF;
015700121113           this.societa = ndbmv00f.bmvSocieta;
015800121113         ENDIF;
015900121112
016000121113         esito = YPTCMSEQ_NewRegistrazione( ndbmv00f.bmvSys
016100121113                                          : ndbmv00f.bmvNrAsReg
016200121113                                          : ndbmv00f.bmvNrRigaM
016300121113                                          : ndbmv00f.bmvSocieta
016400121112                                          : ndbmv00f.bmvDtReg
016500121112                                          : ndreg00f.regNrReg
016600121112                                          : this.usrPrf
016700121112                                          : ndbmv00f.bmvKcc
016800121112                                          : ndbmv00f.bmvKsc
016900121112                                          : this.segno
017000121112                                          : ndbmv00f.bmvDivisa
017100121112                                          : %DEC(ndbmv00f.bmvImportD : 13 : 2)
017200121112                                          : %DEC(ndbmv00f.bmvImporto : 13 : 2)
017300121112                                          : ndbmv00f.bmvCauBan
017400121112                                          : ndbmv00f.bmvDtOpe
017500121112                                          : ndbmv00f.bmvDtVal
017600121112                                          : ndbmv00f.bmvUnita
017700121112                                          : ndmov00f.movDesBrev
017800121112                                          );
017900121112
018000121112         IF esito < YPTCMSEQ_ESITO_OK;
018100121112           esito = YPTCMSEQ_EndRegistrazione( *OFF );
018200121112           LEAVE;
018300121112         ELSE;
018400121112           esito = YPTCMSEQ_EndRegistrazione( *ON );
018500121112         ENDIF;
018600121112
018700121112       ENDDO;
018800121112
018900121112       esito = YPTCMSEQ_Finalize();
019000121112       EXEC SQL CLOSE ndbmv00f;
019100121112
019200121112      /END-FREE
