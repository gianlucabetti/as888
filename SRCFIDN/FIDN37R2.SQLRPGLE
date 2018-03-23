000100981118     H DECEDIT('0,') DATEDIT(*DMY.)
000200981118
000300981119     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F)
000400060629     D FIDN37DS      E DS
000500981118     D KPJBA         E DS
000600981119      *
000700060629     d Param           ds
000800060629     d  poges                  1      3  0
000900060629     d  dtaris                 4     11  0
001000981118
001100981118      *---------------------------------------------------------------*
001200981118      *--------  ISTRUZIONI SQL PER APERTURA FILE  --------------------
001300981118      *
001400981118     C/EXEC SQL
001500981118     C+ DECLARE C1 CURSOR FOR
001600981118     C+   SELECT * FROM FNDCT00F
001700060629     C+     WHERE DCTDIC = :dtaris and DCTPOR = :poges
001800060629     C+       FOR READ ONLY
001900981118     C/END-EXEC
002000981118      *
002100981118     C/EXEC SQL
002200981118     C+ OPEN C1
002300981118     C/END-EXEC
002400981118
002500981118      *----------------------------------------------------------------
002600981118     C     SQLCOD        DOUNE     0
002700981118      *----------------
002800981118     C/EXEC SQL
002900981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
003000981118     C/END-EXEC
003100981118      *----------------
003200981118     C     SQLCOD        CABNE     *ZEROS        ERROR
003300981118      *
003400060619      *
003500060629      *  Elaboro C.A. se stampata autodichiararzione
003600060619      *
003700981119      *  Stampo lettera
003800981119     c                   EXSR      PRTLET
003900981119      *
004000981118     C                   ENDDO
004100981118     C     ERROR         TAG
004200981118      *
004300060629     C/EXEC SQL
004400060629     C+ CLOSE C1
004500060629     C/END-EXEC
004600060629      *
004700981118     C                   eval      *INLR = *ON
004800981119      *****************************************************************
004900981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
005000981119      *****************************************************************
005100981119     C     PRTLET        BEGSR
005200981119      *
005300060629     c                   clear                   FIDN37DS
005400060629      *
005500060629     c                   z-add     DCTaac        I37aac
005600060629     c                   z-add     DCTfil        I37fil
005700060629     c                   z-add     DCTnca        I37nca
005800060629     c                   z-add     poges         I37fgs
005900060629      *
006000060629     c                   Select
006100060629
006200060629      * verifico il tipo denuncia da stampare in base al tipo anomalia
006300060629      * prevista autodenuncia per ammanchi prima della consegna
006400060629     c                   when      Dcttad = '02'
006500060629     c                   eval      I37tde = '1'
006600060629      * prevista autodenuncia per ammanchi dopo la consegna
006700060629     c                   when      Dcttad = '22'
006800060629     c                   eval      I37tde = '2'
006900060629      * prevista autodenuncia per mancanze
007000060629     c                   when      Dcttad = '01' or Dcttad = '21'
007100060629     c                   eval      I37tde = '3'
007200060629     c                   endsl
007300060629      *
007400060629     c                   movel(P)  FIDN37DS      KPJBU
007500060629      *
007600060629     c                   CALL      'FIDN37R3'
007700060629     c                   PARM                    KPJBA
007800060629      *
007900981119     c                   ENDSR
008000981118      *****************************************************************
008100981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
008200981118      *****************************************************************
008300981118     C     *INZSR        BEGSR
008400981118      *
008500981118     C     *ENTRY        PLIST
008600981118     C                   PARM                    KPJBA
008700981119      *
008800060629     c                   movel     kpjbu         param
008900981119      *
009000060629     C*
009100060629     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009200060629     C
009300060629     C/EXEC SQL
009400060629     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009500060629     C/END-EXEC
009600060629     C*
009700981118     C                   ENDSR
009800981119      *-----------------------------------------------------------------
