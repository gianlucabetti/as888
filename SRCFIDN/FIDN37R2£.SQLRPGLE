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
001100981118     C/EXEC SQL
001200981118     C+ WHENEVER SQLERROR GOTO ERROR
001300981118     C/END-EXEC
001400981118      *
001500981118     C/EXEC SQL
001600981118     C+ DECLARE C1 CURSOR FOR
001700981118     C+   SELECT * FROM FNDCT00F
001800981119     C+     WHERE DCTDIC = :WI26dic
001900981119     C+       FOR UPDATE OF DCTRCD
002000981118     C/END-EXEC
002100981118      *
002200981118     C/EXEC SQL
002300981118     C+ OPEN C1
002400981118     C/END-EXEC
002500981118
002600981118      *----------------------------------------------------------------
002700981118     C     SQLCOD        DOUNE     0
002800981118      *----------------
002900981118     C/EXEC SQL
003000981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
003100981118     C/END-EXEC
003200981118      *----------------
003300981118     C     SQLCOD        CABNE     *ZEROS        ERROR
003400981118      *
003500060619      *
003600060619      *  Elaboro C.A. se stampata denuncia e assegnato numero pratica e legata ad evento
003700060619     c                   IF        DCTnde > *zeros  AND  DCTprn > *zeros
003800060619     c                             and  DCTnev > *zeros
003900060619      *
004000981119      *  Stampo lettera
004100981119     c                   EXSR      PRTLET
004200981119      *
004300981119      *  Aggiorno archivi
004400981118     C/EXEC SQL
004500981119     C+ UPDATE FNDCT00F SET DCTRCD = ' '
004600981118     C+     WHERE CURRENT OF C1
004700981118     C/END-EXEC
004800981118      *
004900060619     c                   endif
005000981118     C                   ENDDO
005100981118     C     ERROR         TAG
005200981118      *
005300981118     C                   eval      *INLR = *ON
005400981119      *****************************************************************
005500981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
005600981119      *****************************************************************
005700981119     C     PRTLET        BEGSR
005800981119      *
005900981119     c                   clear                   FIDN26DS
006000981119      *
006100981119     c                   z-add     DCTaac        I26aac
006200981119     c                   z-add     DCTfil        I26fil
006300981119     c                   z-add     DCTnca        I26nca
006400981119     c                   z-add     DCTaas        I26aas
006500981119     c                   z-add     DCTlnp        I26lnp
006600981119     c                   z-add     DCTnrs        I26nrs
006700981119     c                   z-add     DCTnsp        I26nsp
006800981119     c                   z-add     DCTaad        I26aad
006900981119     c                   z-add     DCTpod        I26pod
007000981119     c                   z-add     DCTnde        I26nde
007100981119     c                   movel     DCTdit        I26dit
007200981119     c                   z-add     DCTpra        I26pra
007300981119     c                   z-add     DCTprn        I26prn
007400981119     c                   movel     DCTptr        I26ptr
007500981119     c                   z-add     DCTkcc        I26kcc
007600981119     c                   z-add     DCTksc        I26ksc
007700981119     c                   z-add     DCTdic        I26dic
007800981119      *
007900981119     c                   movel(P)  FIDN26DS      KPJBU
008000981119      *
008100981216     c                   CALL      'FIDN26R3'
008200981119     c                   PARM                    KPJBA
008300981119      *
008400981119     c                   ENDSR
008500981118      *****************************************************************
008600981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
008700981118      *****************************************************************
008800981118     C     *INZSR        BEGSR
008900981118      *
009000981118     C     *ENTRY        PLIST
009100981118     C                   PARM                    KPJBA
009200981119      *
009300981119     c                   movel     KPJBU         WI26dic
009400981119      *
009500981118     C                   ENDSR
009600981119      *-----------------------------------------------------------------
