000100030115      *PARMS DFTACTGRP(*NO) ACTGRP(*CALLER)
000200030115      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000300030115      *PARMS DYNUSRPRF(*OWNER) CLOSQLCSR(*ENDMOD)
000400030115
000500030115      /TITLE Driver lettura YNFEA00F.
000600030115
000700030115     D Errore          C                   '1'
000800030115     D Eseguito        C                   '0'
000900030115
001000030115     D PrmTpFor        S
001100030115     D                                     LIKE(FeaTpFor)
001200030115     D PrmContra       S
001300030115     D                                     LIKE(FeaContra)
001400030115     D PrmDtVal        S               D
001500030115     D PrmEsito        S              1
001600030115     D PrmYnFea        S
001700030115     D                                     LIKE(YnFea00F)
001800030115     D YnFea00F      E DS
001900030115     D                                     INZ
002000030115
002100030115     C     *ENTRY        PLIST
002200030115     ** Parametri di input
002300030115     C                   PARM                    PrmTpFor
002400030115     C                   PARM                    PrmContra
002500030115     C                   PARM                    PrmDtVal
002600030115     ** Parametri di output
002700030115     C                   PARM                    PrmEsito
002800030115     C                   PARM      YnFea00F      PrmYnFea
002900030115
003000030115     C                   EXSR      ChkPrm
003100030115     C                   EXSR      RtvYnFea
003200030115     C                   EXSR      Uscita
003300030115
003400030115     ***********************************************************************
003500030115     **
003600030115     ** Reperisco YNFEA00F.
003700030115     **
003800030115     ***********************************************************************
003900030115     C     RtvYnFea      BEGSR
004000030115
004100030115     C/EXEC SQL
004200030115     C+ DECLARE YNFEA CURSOR FOR SELECT * FROM YNFEA00F WHERE FEATPFOR =
004300030115     C+ :PrmTpFor AND FEACONTRA = :PrmContra AND :PrmDtVal BETWEEN
004400030116     C+ FEADTINCO AND FEADTFICO AND FEADTDLT = '0001-01-01'
004500030115     C/END-EXEC
004600030115
004700030115     C/EXEC SQL
004800030115     C+ OPEN YNFEA
004900030115     C/END-EXEC
005000030115
005100030115     C/EXEC SQL
005200030115     C+ FETCH NEXT FROM YNFEA INTO :YnFea00F
005300030115     C/END-EXEC
005400030115
005500030115     C                   SELECT
005600030115     C                   WHEN      SQLCod = 100
005700030115     C                   EVAL      PrmEsito = Errore
005800030115     C                   WHEN      SQLCod < 0
005900030115     C                   EVAL      *INH1 = *ON
006000030115     C                   ENDSL
006100030115
006200030115     C/EXEC SQL
006300030115     C+ CLOSE YNFEA
006400030115     C/END-EXEC
006500030115
006600030115     C                   ENDSR
006700030115
006800030115     ***********************************************************************
006900030115     **
007000030115     ** Controllo parametri ricevuti.
007100030115     **
007200030115     ***********************************************************************
007300030115     C     ChkPrm        BEGSR
007400030115
007500030115     C                   CLEAR                   PrmEsito
007600030115     C                   CLEAR                   YnFea00F
007700030115
007800030115     C                   IF        PrmTpFor = *BLANK
007900030115     C                             OR
008000030115     C                             PrmContra = *BLANK
008100030115     C                   EVAL      PrmEsito = Errore
008200030115     C                   EXSR      Uscita
008300030115     C                   ENDIF
008400030115
008500030115     C                   ENDSR
008600030115
008700030115     ***********************************************************************
008800030115     **
008900030115     ** Uscita.
009000030115     **
009100030115     ***********************************************************************
009200030115     C     Uscita        BEGSR
009300030115
009400030115     C                   IF        PrmEsito = *BLANK
009500030115     C                   EVAL      PrmEsito = Eseguito
009600030115     C                   ENDIF
009700030115
009800030115     C                   RETURN
009900030115
010000030115     C                   ENDSR
