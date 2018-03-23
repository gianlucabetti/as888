000100981118     H DECEDIT('0,') DATEDIT(*DMY.)
000200981118
000300981119     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F)
000400981119     D FIDN26DS      E DS
000500981118     D KPJBA         E DS
000600981119      *
000700981119     D WI26dic         S                   LIKE(I26dic)
000800981118
000900981118      *---------------------------------------------------------------*
001000981118      *--------  ISTRUZIONI SQL PER APERTURA FILE  --------------------
001100981118      *
001200981118     C/EXEC SQL
001300981118     C+ DECLARE C1 CURSOR FOR
001400981118     C+   SELECT * FROM FNDCT00F
001500060629     C+     WHERE DCTDIC = :WI26dic  and DCTPOR = 046
001600981119     C+       FOR UPDATE OF DCTRCD
001700981118     C/END-EXEC
001800981118      *
001900981118     C/EXEC SQL
002000981118     C+ OPEN C1
002100981118     C/END-EXEC
002200981118
002300981118      *----------------------------------------------------------------
002400981118     C     SQLCOD        DOUNE     0
002500981118      *----------------
002600981118     C/EXEC SQL
002700981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
002800981118     C/END-EXEC
002900981118      *----------------
003000981118     C     SQLCOD        CABNE     *ZEROS        ERROR
003100981118      *
003200060619      *
003300060619      *  Elaboro C.A. se stampata denuncia e assegnato numero pratica e legata ad evento
003400060619     c                   IF        DCTnde > *zeros  AND  DCTprn > *zeros
003500060619     c                             and  DCTnev > *zeros
003600060619      *
003700981119      *  Stampo lettera
003800981119     c                   EXSR      PRTLET
003900981119      *
004000981119      *  Aggiorno archivi
004100981118     C/EXEC SQL
004200981119     C+ UPDATE FNDCT00F SET DCTRCD = ' '
004300981118     C+     WHERE CURRENT OF C1
004400981118     C/END-EXEC
004500981118      *
004600060619     c                   endif
004700981118     C                   ENDDO
004800981118     C     ERROR         TAG
004900981118      *
005000060629      *
005100060629     C/EXEC SQL
005200060629     C+ CLOSE C1
005300060629     C/END-EXEC
005400060629      *
005500981118     C                   eval      *INLR = *ON
005600981119      *****************************************************************
005700981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
005800981119      *****************************************************************
005900981119     C     PRTLET        BEGSR
006000981119      *
006100981119     c                   clear                   FIDN26DS
006200981119      *
006300981119     c                   z-add     DCTaac        I26aac
006400981119     c                   z-add     DCTfil        I26fil
006500981119     c                   z-add     DCTnca        I26nca
006600981119     c                   z-add     DCTaas        I26aas
006700981119     c                   z-add     DCTlnp        I26lnp
006800981119     c                   z-add     DCTnrs        I26nrs
006900981119     c                   z-add     DCTnsp        I26nsp
007000981119     c                   z-add     DCTaad        I26aad
007100981119     c                   z-add     DCTpod        I26pod
007200981119     c                   z-add     DCTnde        I26nde
007300981119     c                   movel     DCTdit        I26dit
007400981119     c                   z-add     DCTpra        I26pra
007500981119     c                   z-add     DCTprn        I26prn
007600981119     c                   movel     DCTptr        I26ptr
007700981119     c                   z-add     DCTkcc        I26kcc
007800981119     c                   z-add     DCTksc        I26ksc
007900981119     c                   z-add     DCTdic        I26dic
008000981119      *
008100981119     c                   movel(P)  FIDN26DS      KPJBU
008200981119      *
008300981216     c                   CALL      'FIDN26R3'
008400981119     c                   PARM                    KPJBA
008500981119      *
008600981119     c                   ENDSR
008700981118      *****************************************************************
008800981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
008900981118      *****************************************************************
009000981118     C     *INZSR        BEGSR
009100981118      *
009200981118     C     *ENTRY        PLIST
009300981118     C                   PARM                    KPJBA
009400981119      *
009500981119     c                   movel     KPJBU         WI26dic
009600060629     C*
009700060629     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009800060629     C
009900060629     C/EXEC SQL
010000060629     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
010100060629     C/END-EXEC
010200060629     C*
010300981119      *
010400981118     C                   ENDSR
010500981119      *-----------------------------------------------------------------
