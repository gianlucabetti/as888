000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300030721     H* Allineameno: Azioni/Menu/KPJBU via SQL  -Filiale-
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700030721     D*------------------------------------------------------------------------*
000800030721     D* VARIABILI DI WRK
000900030721     D*------------------------------------------------------------------------*
001000030721     D StrSQL          S            150    INZ(*blanks)
001100970526     C*------------------------------------------------------------------------*
001200991214     C* MAIN LINES
001300970526     C*------------------------------------------------------------------------*
001400970526     C*
001500030728     C* Esegue cancellazione contenuto file GAITRAOBJ/KFAZN10F e suo riempimento da file EDPWKSE/KFAZN10F
001600030721     C                   EXSR      EXE_KFAZN10F
001700030721     C*
001800030728     C* Esegue cancellazione contenuto file GAITRAOBJ/KFKPJ00F e suo riempimento da file EDPWKSE/KFKPJ00F
001900030721     C                   EXSR      EXE_KFKPJ00F
002000030721     C*
002100030728     C* Esegue cancellazione contenuto file GAITRAOBJ/KPMNT00F e suo riempimento da file EDPWKSE/KPMNT00F
002200030721     C                   EXSR      EXE_KPMNT00F
002300030721     C*
002400030728     C* Esegue cancellazione contenuto file GAITRAOBJ/KPMNU00F e suo riempimento da file EDPWKSE/KPMNU00F
002500030721     C                   EXSR      EXE_KPMNU00F
002600000306     C*
002700970526     C                   SETON                                        LR
002800970526     C*------------------------------------------------------------------------*
002900030721     C* EXE_KFAZN10F - OPERAZINI DI DELETE E INSERT
003000970526     C*------------------------------------------------------------------------*
003100030721     C     EXE_KFAZN10F  BEGSR
003200030721     C*
003300030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
003400030728     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KFAZN10F '  +
003500030728     C                                      'WHERE SUBSTR(KCAZN, 1, 1) '       +
003600030728     C                                      'NOT IN (''G'', ''£'')'
003700000929     C*
003800000929     C/EXEC SQL
003900030721     C+ EXECUTE IMMEDIATE :StrSQL
004000000929     C/END-EXEC
004100030721     C*
004200030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
004300030721     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KFAZN10F '+
004400030728     C                                      'SELECT * FROM EDPWKSE/KFAZN10F '  +
004500030728     C                                      'WHERE SUBSTR(KCAZN, 1, 1) '       +
004600030728     C                                      'NOT IN (''G'', ''£'')'
004700030721     C*
004800030721     C/EXEC SQL
004900030721     C+ EXECUTE IMMEDIATE :StrSQL
005000030721     C/END-EXEC
005100000929     C*
005200000929     C                   ENDSR
005300030721     C*------------------------------------------------------------------------*
005400030721     C* EXE_KFKPJ00F - OPERAZINI DI DELETE E INSERT
005500030721     C*------------------------------------------------------------------------*
005600030721     C     EXE_KFKPJ00F  BEGSR
005700030721     C*
005800030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
005900030728     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KFKPJ00F '  +
006000030728     C                                      'WHERE SUBSTR(KCAZN, 1, 1) '       +
006100030728     C                                      'NOT IN (''G'', ''£'')'
006200030721     C*
006300030721     C/EXEC SQL
006400030721     C+ EXECUTE IMMEDIATE :StrSQL
006500030721     C/END-EXEC
006600030721     C*
006700030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
006800030721     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KFKPJ00F '+
006900030728     C                                      'SELECT * FROM EDPWKSE/KFKPJ00F '  +
007000030728     C                                      'WHERE SUBSTR(KCAZN, 1, 1) '       +
007100030728     C                                      'NOT IN (''G'', ''£'')'
007200030721     C*
007300030721     C/EXEC SQL
007400030721     C+ EXECUTE IMMEDIATE :StrSQL
007500030721     C/END-EXEC
007600030721     C*
007700030721     C                   ENDSR
007800030721     C*------------------------------------------------------------------------*
007900030721     C* EXE_KPMNT00F - OPERAZINI DI DELETE E INSERT
008000030721     C*------------------------------------------------------------------------*
008100030721     C     EXE_KPMNT00F  BEGSR
008200030721     C*
008300030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
008400030728     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KPMNT00F '  +
008500030728     C                                      'WHERE SUBSTR(MNTCOD, 1, 1) '      +
008600030728     C                                      'NOT IN (''G'', ''£'')'
008700030721     C*
008800030721     C/EXEC SQL
008900030721     C+ EXECUTE IMMEDIATE :StrSQL
009000030721     C/END-EXEC
009100030721     C*
009200030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
009300030728     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KPMNT00F '+
009400030728     C                                      'SELECT * FROM EDPWKSE/KPMNT00F '  +
009500030728     C                                      'WHERE SUBSTR(MNTCOD, 1, 1) '      +
009600030728     C                                      'NOT IN (''G'', ''£'')'
009700030721     C*
009800030721     C/EXEC SQL
009900030721     C+ EXECUTE IMMEDIATE :StrSQL
010000030721     C/END-EXEC
010100030721     C*
010200030721     C                   ENDSR
010300030721     C*------------------------------------------------------------------------*
010400030721     C* EXE_KPMNU00F - OPERAZINI DI DELETE E INSERT
010500030721     C*------------------------------------------------------------------------*
010600030721     C     EXE_KPMNU00F  BEGSR
010700030721     C*
010800030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
010900030728     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KPMNU00F '  +
011000030728     C                                      'WHERE SUBSTR(MNUCOD, 1, 1) '      +
011100030728     C                                      'NOT IN (''G'', ''£'')'
011200030721     C*
011300030721     C/EXEC SQL
011400030721     C+ EXECUTE IMMEDIATE :StrSQL
011500030721     C/END-EXEC
011600030721     C*
011700030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
011800030728     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KPMNU00F '+
011900030728     C                                      'SELECT * FROM EDPWKSE/KPMNU00F '  +
012000030728     C                                      'WHERE SUBSTR(MNUCOD, 1, 1) '      +
012100030728     C                                      'NOT IN (''G'', ''£'')'
012200030721     C*
012300030721     C/EXEC SQL
012400030721     C+ EXECUTE IMMEDIATE :StrSQL
012500030721     C/END-EXEC
012600030721     C*
012700030721     C                   ENDSR
012800970526     C*------------------------------------------------------------------------*
012900970526     C* *INZSR - OPERAZIONI INIZIALI
013000970526     C*------------------------------------------------------------------------*
013100970526     C     *INZSR        BEGSR
013200991214     C*
013300030721     C*
013400970526     C                   ENDSR
