000100981118     H DECEDIT('0,') DATEDIT(*DMY.)
000200981118
000300981119     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F)
000400981119     D FIDN26DS      E DS
000500981118     D KPJBA         E DS
000600981119      *
000700981119     D WI26dic         S                   LIKE(I26dic)
000800981118
000900981118      *---------------------------------------------------------------*
001000081222     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
001100081222     C
001200081222     C/EXEC SQL
001300081222     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
001400081222     C/END-EXEC
001500081222     C*
001600981118      *--------  ISTRUZIONI SQL PER APERTURA FILE  --------------------
001700981118      *
001800981118     C/EXEC SQL
001900981118     C+ DECLARE C1 CURSOR FOR
002000981118     C+   SELECT * FROM FNDCT00F
002100060629     C+     WHERE DCTDIC = :WI26dic  and DCTPOR = 046
002200981119     C+       FOR UPDATE OF DCTRCD
002300981118     C/END-EXEC
002400981118      *
002500981118     C/EXEC SQL
002600981118     C+ OPEN C1
002700981118     C/END-EXEC
002800981118
002900981118      *----------------------------------------------------------------
003000981118     C     SQLCOD        DOUNE     0
003100981118      *----------------
003200981118     C/EXEC SQL
003300981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
003400981118     C/END-EXEC
003500981118      *----------------
003600981118     C     SQLCOD        CABNE     *ZEROS        ERROR
003700981118      *
003800060619      *
003900060619      *  Elaboro C.A. se stampata denuncia e assegnato numero pratica e legata ad evento
004000060619     c                   IF        DCTnde > *zeros  AND  DCTprn > *zeros
004100060619     c                             and  DCTnev > *zeros
004200060619      *
004300981119      *  Stampo lettera
004400981119     c                   EXSR      PRTLET
004500981119      *
004600981119      *  Aggiorno archivi
004700981118     C/EXEC SQL
004800981119     C+ UPDATE FNDCT00F SET DCTRCD = ' '
004900981118     C+     WHERE CURRENT OF C1
005000981118     C/END-EXEC
005100981118      *
005200060619     c                   endif
005300981118     C                   ENDDO
005400981118     C     ERROR         TAG
005500981118      *
005600060629      *
005700060629     C/EXEC SQL
005800060629     C+ CLOSE C1
005900060629     C/END-EXEC
006000060629      *
006100981118     C                   eval      *INLR = *ON
006200981119      *****************************************************************
006300981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
006400981119      *****************************************************************
006500981119     C     PRTLET        BEGSR
006600981119      *
006700981119     c                   clear                   FIDN26DS
006800981119      *
006900981119     c                   z-add     DCTaac        I26aac
007000981119     c                   z-add     DCTfil        I26fil
007100981119     c                   z-add     DCTnca        I26nca
007200981119     c                   z-add     DCTaas        I26aas
007300981119     c                   z-add     DCTlnp        I26lnp
007400981119     c                   z-add     DCTnrs        I26nrs
007500981119     c                   z-add     DCTnsp        I26nsp
007600981119     c                   z-add     DCTaad        I26aad
007700981119     c                   z-add     DCTpod        I26pod
007800981119     c                   z-add     DCTnde        I26nde
007900981119     c                   movel     DCTdit        I26dit
008000981119     c                   z-add     DCTpra        I26pra
008100981119     c                   z-add     DCTprn        I26prn
008200981119     c                   movel     DCTptr        I26ptr
008300981119     c                   z-add     DCTkcc        I26kcc
008400981119     c                   z-add     DCTksc        I26ksc
008500981119     c                   z-add     DCTdic        I26dic
008600081222     c                   move      DCTflo        I26flo
008700981119      *
008800981119     c                   movel(P)  FIDN26DS      KPJBU
008900981119      *
009000981216     c                   CALL      'FIDN26R3'
009100981119     c                   PARM                    KPJBA
009200981119      *
009300981119     c                   ENDSR
009400981118      *****************************************************************
009500981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
009600981118      *****************************************************************
009700981118     C     *INZSR        BEGSR
009800981118      *
009900981118     C     *ENTRY        PLIST
010000981118     C                   PARM                    KPJBA
010100981119      *
010200981119     c                   movel     KPJBU         WI26dic
010300060629     C*
010400981119      *
010500981118     C                   ENDSR
010600981119      *-----------------------------------------------------------------
