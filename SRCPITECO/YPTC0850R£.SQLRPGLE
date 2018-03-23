000100131202      //************************************************************************
000200121116      //
000600140911      // Questo programma
000601140911      //
000602140911      // - Corregge e integra la prima nota batch inserita da Piteco.
000603140911      // - Sottomette la prima nota batch di Proj.
000604140911      // - Aggiusta i movimenti banca di Proj.
000605140911      //
000700121116      //************************************************************************
000701140911
000800121116     H DFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('PJXBND')
000900121116
001000121116      //************************************************************************
001100121116      //
001200121116      // Strutture dati.
001300121116      //
001400121116      //************************************************************************
001500121116     D kpjba         E DS                  QUALIFIED
001600121116     D kpjba_sbmJob    DS                  LIKEDS(kpjba)
001700121116     D ndc071Ds      E DS                  QUALIFIED INZ
001800121116     D  rifInt071    E                     INZ('PITECO')
001900121116     D  dtRegA071    E                     INZ(*HIVAL)
002000121116     D  subNumA071   E                     INZ(*HIVAL)
002100121116     D  solCont071   E                     INZ('N')
002200121116     D  interat071   E                     INZ('N')
002300121116     D this            DS                  QUALIFIED INZ
002400121116     D  esitoProj                     1A
002401140723     D  curTimeStamp                   Z   INZ(*SYS)
002500121116     D xSoc001Ds     E DS                  QUALIFIED INZ
002600121116
002700121116      //************************************************************************
002800121116      //
002900121116      // Costanti.
003000121116      //
003100121116      //************************************************************************
003101140127     D DAPITECO...
003102140127     D                 C                   '*DAPITECO'
003200121116     D PROJ_ESITO_OK...
003300121116     D                 C                   '0'
003400131127     D OFF             S              1A   INZ(*OFF)
003401131127     D ON              S              1A   INZ(*ON)
003402131127     D PITECO          S              9A   INZ('PITECO')
003403140910     D PRIORITA_LAVORO_4...
003404140910     D                 C                   'P4'
003405140910     D PRIORITA_LAVORO_9...
003406140910     D                 C                   'P9'
003407140207     D SEDE            S              3A   INZ('046')
003408140910     D SOCIETA_BRT...
003409140910     D                 C                   '201'
003500121116
003600121116      //************************************************************************
003700121116      //
003800121116      // Prototipi.
003900121116      //
004000121116      //************************************************************************
004100121116      /COPY GAITRASRC/SRCPROTOPR,ABP
004200121116      /COPY GAITRASRC/SRCPROTOPR,PROJ_ILE
004300121116      /COPY GAITRASRC/SRCPROTOPR,XSOC
004301140120      /COPY GAITRASRC/SRCPROTOPR,SYSTEM
004400121116
004500121116      //************************************************************************
004600121116      //
004700121116      // Parametri.
004800121116      //
004900121116      //************************************************************************
005000121116     C     *ENTRY        PLIST
005100121116     C                   PARM                    kpjba
005200121116
005300121116      //************************************************************************
005400121116      //
005500121116      // Main.
005600121116      //
005700121116      //************************************************************************
005800121116
005900121116      /FREE
006000121116
006100131127       *INLR = *ON;
006200121116
006201140120       // Sono stato chiamato da Piteco (stored procedure) tolgo la libreria
006202140120       // corrente perché dentro ci sono degli oggetti che non piacciono alla
006203140120       // prima nota batch di Proj.
006204140120
006205140127       IF %SUBST(kpjba.kpjbu : 1 : 9) = DAPITECO;
006206140725         ExecuteCommand('CHGCURLIB CURLIB(*CRTDFT)');
006207140120       ENDIF;
006208140120
006233131127       // Filtro le righe di NDBHM00F inserite da Piteco e su quelle dei conti
006234131127       // di tesoreria aggiungo un finto movimento di tesoreria con
006235131127       // operazione bancaria PTCA o PTCD.
006236131127
006300121116       EXEC SQL
006400121116         UPDATE ndbhm00f
006500130208            SET ndbhm00f.bhmCausRig = CONCAT('PTC', ndbhm00f.bhmSegno)
006600130208              , ndbhm00f.bhmCauBan = CONCAT('PTC', ndbhm00f.bhmSegno)
006700121116              , ndbhm00f.bhmDtOpe = ndbhm00f.bhmDtReg
006800121116              , ndbhm00f.bhmDtVal = ndbhm00f.bhmDtReg
006900121116          WHERE ndbhm00f.bhmRifInt = :PITECO
007000121116            AND EXISTS ( SELECT *
007100130208                           FROM anrco00f
007200130208                          WHERE anrco00f.rcoSocieta = ndbhm00f.bhmSocieta
007300130208                            AND anrco00f.rcoKcc = ndbhm00f.bhmKcc
007400130208                            AND anrco00f.rcoKsc = ndbhm00f.bhmKsc
007500131127                            AND anrco00f.rcoRappTes = :ON )
007600121116           SKIP LOCKED DATA
007700121116       ;
007800121116
007900121116       SELECT;
008000121116         WHEN sqlCode < *ZERO;
008100121116           DUMP(A);
008200121116           RETURN;
008300121116       ENDSL;
008301131202
008302131202       // Imposto il numero azione.
008305131202
008306131202       EXEC SQL
008307131202         UPDATE ndbhm00f
008308131202            SET ndbhm00f.bhmKnraz = :kpjba.knraz
008313131202          WHERE ndbhm00f.bhmRifInt = :PITECO
008314131202            AND ndbhm00f.bhmKnraz = 0
008320131202           SKIP LOCKED DATA
008321140911           WITH NC
008322131202       ;
008323131202
008324131202       SELECT;
008325131202         WHEN sqlCode < *ZERO;
008326131202           DUMP(A);
008328131202       ENDSL;
008329140207
008330140207       // Metto l'unità della riga 2 sulla riga 1.
008331140207
008332140207       EXEC SQL
008333140207         UPDATE ndbhm00f AS a
008334140207            SET a.bhmUnita = ( SELECT b.bhmUnita FROM ndbhm00f AS b WHERE
008335140207                a.bhmSocieta = b.bhmSocieta AND a.bhmRifInt = b.bhmRifInt AND
008336140207                a.bhmSubNum = b.bhmSubNum AND 2 = b.bhmSubRig )
008337140207          WHERE a.bhmRifInt = :PITECO
008338140207            AND a.bhmSubRig = 1
008339140207            AND a.bhmUnita = ''
008340140207           SKIP LOCKED DATA
008341140911           WITH NC
008342140207       ;
008343140207
008344140207       SELECT;
008345140207         WHEN sqlCode < *ZERO;
008346140207           DUMP(A);
008348140207       ENDSL;
008349140207
008350140207       // Metto l'unità 046 dove manca.
008351140207
008352140207       EXEC SQL
008353140207         UPDATE ndbhm00f
008354140207            SET bhmUnita = :SEDE
008355140207          WHERE bhmRifInt = :PITECO
008357140207            AND bhmUnita = ''
008358140207           SKIP LOCKED DATA
008359140911           WITH NC
008360140207       ;
008361140207
008362140207       SELECT;
008363140207         WHEN sqlCode < *ZERO;
008364140207           DUMP(A);
008366140207       ENDSL;
008367131127
008368131127       // Cambio il valore del campo "uso civilistico" perché voglio che le
008369131127       // descrizioni aggiuntive non siano stampate nel giornale e nelle schede
008370131127       // contabili.
008371131127
008372131127       EXEC SQL
008373131127         UPDATE ndbhd00f
008374131127            SET ndbhd00f.bhdUsoCiv = :OFF
008375131127          WHERE ndbhd00f.bhdRifInt = :PITECO
008376131127            AND ndbhd00f.bhdUsoCiv = :ON
008377131127           SKIP LOCKED DATA
008378140911           WITH NC
008379131127       ;
008380131127
008381131127       SELECT;
008382131127         WHEN sqlCode < *ZERO;
008383131127           DUMP(A);
008385131127       ENDSL;
008457140723
008458140723       //***********************************************************************
008459140723       //
008460140723       // Sottometto una prima nota batch per ogni società del lotto 'PITECO'.
008461140723       //
008462140723       //***********************************************************************
008463131127
008500121116       EXEC SQL
008600121116         DECLARE societaPiteco NO SCROLL CURSOR FOR
008700121116          SELECT DISTINCT ndbhm00f.bhmSocieta
008800121116            FROM ndbhm00f
008900121116           WHERE ndbhm00f.bhmRifInt = :PITECO
009000130208            SKIP LOCKED DATA
009100121116       ;
009200121116
009300121116       EXEC SQL OPEN societaPiteco;
009400121116
009500121116       SELECT;
009600121116         WHEN sqlCode < *ZERO;
009700121116           DUMP(A);
009800121116           RETURN;
009900121116       ENDSL;
010000121116
010100121116       kpjba_sbmJob = kpjba;
010101140204       kpjba_sbmJob.knmus = 'ESTPITECO';
010200121116       kpjba_sbmJob.kcoaz = 'C071';
010201140911       kpjba_sbmJob.kqebi = 'KGIORNO3';
010202131203       CLEAR kpjba_sbmJob.kbuff; // Pulisco data e ora schedulazione.
010300121116
010400121116       DOU sqlCode < *ZERO;
010500121116
010600121116         EXEC SQL FETCH NEXT FROM societaPiteco INTO :ndc071Ds.societa071;
010700121116
010800121116         SELECT;
010900121116           WHEN sqlCode = 100;
011000121116             LEAVE;
011100121116           WHEN sqlCode < *ZERO;
011200121116             DUMP(A);
011300121116             LEAVE;
011400121116         ENDSL;
011500121116
011600121116         Proj_Societa( 'SOC002' : ndc071Ds.societa071 : *ZERO : *BLANK
011700121116                     : this.esitoProj : xSoc001Ds : kpjba );
011800121116
011900121116         IF this.esitoProj <> PROJ_ESITO_OK;
012000121116           DUMP(A);
012100121116           LEAVE;
012200121116         ENDIF;
012300121116
012301140910         IF ndc071Ds.societa071 = SOCIETA_BRT;
012302140910           kpjba_sbmJob.kcojb = PRIORITA_LAVORO_4;
012303140910         ELSE;
012304140910           kpjba_sbmJob.kcojb = PRIORITA_LAVORO_9;
012305140910         ENDIF;
012306140910
012400121116         kpjba_sbmJob.knsif = xSoc001Ds.xscNsi;
012500121116         kpjba_sbmJob.kpjbu = ndc071Ds;
012600121116
012700121116         ABP_LancioAzioneBatch( kpjba_sbmJob );
012800121116
012900121116       ENDDO;
013000121116
013100121116       EXEC SQL CLOSE societaPiteco;
013101140911
013102140911       //***********************************************************************
013103140911       //
013104140911       // In tutte le righe di tesoreria inserite da Piteco imposto il flag
013105140911       // di movimento spuntato.
013106140911       //
013107140911       //***********************************************************************
013108140911
013109140911       EXEC SQL
013110140911         UPDATE ndbmv00f
013111140911            SET bmvStatus = '1'
013112140911          WHERE bmvCertif = '1'
013113140911            AND bmvNrTeso > 0
013114140911            AND bmvStatus = ''
013115140911            AND bmvSocieta IN (SELECT sifSocieta FROM ansif99v)
013116140911           SKIP LOCKED DATA
013117140911           WITH NC
013118140911       ;
013119140911
013120140911       SELECT;
013121140911         WHEN sqlCode < *ZERO;
013122140911           DUMP(A);
013123140911       ENDSL;
013124140911
013125140911       //***********************************************************************
013126140911       //
013127140911       // Cambio il numero di tesoreria per gestire le spunte "n a n".
013128140911       // I numeri di tesoreria da cambiare sono nella table YPTCNRTESO.
013129140911       // Le righe sono inserite da Piteco durante la scrittura di NDBHM00F e
013130140911       // nascono con la colonna ORAELABOR nulla.
013131140911       //
013132140911       //***********************************************************************
013133141201
013134141201       EXEC SQL
013135141201         DELETE FROM yptcnrteso
013137141201          WHERE yptcnrteso.oldNrTeso = yptcnrteso.newNrTeso
013138141201           SKIP LOCKED DATA
013140141201       ;
013141141201
013142141201       SELECT;
013143141201         WHEN sqlCode < *ZERO;
013144141201           DUMP(A);
013147141201       ENDSL;
013148140911
013149140911       EXEC SQL SET :this.curTimeStamp = CURRENT_TIMESTAMP;
013150140911
013151140911       EXEC SQL
013152140911         UPDATE yptcnrteso
013153140911            SET yptcnrteso.oraElabor = :this.curTimeStamp
013154140911          WHERE yptcnrteso.oraElabor IS NULL
013155140911           SKIP LOCKED DATA
013156140911           WITH UR
013157140911       ;
013158140911
013159140911       SELECT;
013160140911         WHEN sqlCode < *ZERO;
013161140911           DUMP(A);
013162140911           EXEC SQL ROLLBACK;
013163140911           RETURN;
013164140911       ENDSL;
013165140911
013166140911       EXEC SQL
013167140911         UPDATE ndbmv00f
013168140911            SET ndbmv00f.bmvNrTeso =
013169140911              ( SELECT MAX(yptcnrteso.newNrTeso)
013170140911                  FROM yptcnrteso
013171140911                 WHERE yptcnrteso.oldNrTeso = ndbmv00f.bmvNrTeso
013172140911                   AND yptcnrteso.oraElabor = :this.curTimeStamp )
013173140911          WHERE ndbmv00f.bmvDest = 'Y'
013174140911            AND ndbmv00f.bmvNrTeso IN
013175140911              ( SELECT DISTINCT yptcnrteso.oldNrTeso
013176140911                  FROM yptcnrteso
013177140916                 WHERE yptcnrteso.oraElabor = :this.curTimeStamp
013178140916                   AND yptcnrteso.oldNrTeso <> yptcnrteso.newNrTeso )
013179140911           SKIP LOCKED DATA
013180140911           WITH UR
013181140911       ;
013182140911
013183140911       SELECT;
013184140911         WHEN sqlCode < *ZERO;
013185140911           DUMP(A);
013186140911           EXEC SQL ROLLBACK;
013187140911           RETURN;
013188140911       ENDSL;
013189140911
013190140911       EXEC SQL COMMIT;
013200121116
013300121116      /END-FREE
