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
001500030721     C* Esegue cancellazione contenuto file GAITRAOBJ/KFAZN10F e suo riempimento da file EDPWK/KFAZN10F
001600030721     C                   EXSR      EXE_KFAZN10F
001700030721     C*
001800030721     C* Esegue cancellazione contenuto file GAITRAOBJ/KFKPJ00F e suo riempimento da file EDPWK/KFKPJ00F
001900030721     C                   EXSR      EXE_KFKPJ00F
002000030721     C*
002100030721     C* Esegue cancellazione contenuto file GAITRAOBJ/KPMNT00F e suo riempimento da file EDPWK/KPMNT00F
002200030721     C                   EXSR      EXE_KPMNT00F
002300030721     C*
002400030721     C* Esegue cancellazione contenuto file GAITRAOBJ/KPMNU00F e suo riempimento da file EDPWK/KPMNU00F
002500030721     C                   EXSR      EXE_KPMNU00F
002600000306     C*
002700970526     C                   SETON                                        LR
002800970526     C*------------------------------------------------------------------------*
002900030721     C* EXE_KFAZN10F - OPERAZINI DI DELETE E INSERT
003000970526     C*------------------------------------------------------------------------*
003100030721     C     EXE_KFAZN10F  BEGSR
003200030721     C*
003300030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
003400030721     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KFAZN10F'
003500000929     C*
003600000929     C/EXEC SQL
003700030721     C+ EXECUTE IMMEDIATE :StrSQL
003800000929     C/END-EXEC
003900030721     C*
004000030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
004100030721     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KFAZN10F '+
004200030721     C                                      'SELECT * FROM EDPWK/KFAZN10F'
004300030721     C*
004400030721     C/EXEC SQL
004500030721     C+ EXECUTE IMMEDIATE :StrSQL
004600030721     C/END-EXEC
004700000929     C*
004800000929     C                   ENDSR
004900030721     C*------------------------------------------------------------------------*
005000030721     C* EXE_KFKPJ00F - OPERAZINI DI DELETE E INSERT
005100030721     C*------------------------------------------------------------------------*
005200030721     C     EXE_KFKPJ00F  BEGSR
005300030721     C*
005400030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
005500030721     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KFKPJ00F'
005600030721     C*
005700030721     C/EXEC SQL
005800030721     C+ EXECUTE IMMEDIATE :StrSQL
005900030721     C/END-EXEC
006000030721     C*
006100030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
006200030721     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KFKPJ00F '+
006300030721     C                                      'SELECT * FROM EDPWK/KFKPJ00F'
006400030721     C*
006500030721     C/EXEC SQL
006600030721     C+ EXECUTE IMMEDIATE :StrSQL
006700030721     C/END-EXEC
006800030721     C*
006900030721     C                   ENDSR
007000030721     C*------------------------------------------------------------------------*
007100030721     C* EXE_KPMNT00F - OPERAZINI DI DELETE E INSERT
007200030721     C*------------------------------------------------------------------------*
007300030721     C     EXE_KPMNT00F  BEGSR
007400030721     C*
007500030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
007600030721     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KPMNT00F'
007700030721     C*
007800030721     C/EXEC SQL
007900030721     C+ EXECUTE IMMEDIATE :StrSQL
008000030721     C/END-EXEC
008100030721     C*
008200030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
008300030721     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KPMNT00F '+
008400030721     C                                      'SELECT * FROM EDPWK/KPMNT00F'
008500030721     C*
008600030721     C/EXEC SQL
008700030721     C+ EXECUTE IMMEDIATE :StrSQL
008800030721     C/END-EXEC
008900030721     C*
009000030721     C                   ENDSR
009100030721     C*------------------------------------------------------------------------*
009200030721     C* EXE_KPMNU00F - OPERAZINI DI DELETE E INSERT
009300030721     C*------------------------------------------------------------------------*
009400030721     C     EXE_KPMNU00F  BEGSR
009500030721     C*
009600030721     C* Preparo la 1° stringa SQL da eseguire x lo svuotamento
009700030721     C                   EVAL      StrSQL = 'DELETE FROM GAITRAOBJ/KPMNU00F'
009800030721     C*
009900030721     C/EXEC SQL
010000030721     C+ EXECUTE IMMEDIATE :StrSQL
010100030721     C/END-EXEC
010200030721     C*
010300030721     C* Preparo la 2° stringa SQL da eseguire x il riempimento
010400030721     C                   EVAL      StrSQL = 'INSERT   INTO GAITRAOBJ/KPMNU00F '+
010500030721     C                                      'SELECT * FROM EDPWK/KPMNU00F'
010600030721     C*
010700030721     C/EXEC SQL
010800030721     C+ EXECUTE IMMEDIATE :StrSQL
010900030721     C/END-EXEC
011000030721     C*
011100030721     C                   ENDSR
011200970526     C*------------------------------------------------------------------------*
011300970526     C* *INZSR - OPERAZIONI INIZIALI
011400970526     C*------------------------------------------------------------------------*
011500970526     C     *INZSR        BEGSR
011600991214     C*
011700030721     C*
011800970526     C                   ENDSR
