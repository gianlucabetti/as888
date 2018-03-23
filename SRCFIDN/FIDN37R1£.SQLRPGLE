000100981118     H DECEDIT('0,') DATEDIT(*DMY.)
000200981118
000300981119     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F)
000400981119     D FIDN26DS      E DS
000500981118     D KPJBA         E DS
000600981119      *
000700981119     D WLBDAT          DS                  INZ
000800981119     D  G02DAT                 1      8  0
000900981119     D  G02INV                 9     16  0
001000981119     D  G02ERR                17     17
001100981119     D  G02TGI                18     22  0
001200981119      *
001300981119     D WI26dic         S                   LIKE(I26dic)
001400981119     D W0140           S             14  0
001500981119     D Wdtgio          S              8  0
001600981119     D dateu           S              8  0
001700981118
001800981118      *---------------------------------------------------------------*
001900981118      *--------  ISTRUZIONI SQL PER APERTURA FILE  --------------------
002000981118     C/EXEC SQL
002100981118     C+ WHENEVER SQLERROR GOTO ERROR
002200981118     C/END-EXEC
002300981118      *
002400981118     C/EXEC SQL
002500981118     C+ DECLARE C1 CURSOR FOR
002600981118     C+   SELECT * FROM FNDCT00F
002700981118     C+     WHERE DCTRCD = 'S'
002800981118     C+       FOR UPDATE OF DCTRCD, DCTDIC
002900981118     C/END-EXEC
003000981118      *
003100981118     C/EXEC SQL
003200981118     C+ OPEN C1
003300981118     C/END-EXEC
003400981118
003500981118      *----------------------------------------------------------------
003600981118     C     SQLCOD        DOUNE     0
003700981118      *----------------
003800981118     C/EXEC SQL
003900981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
004000981118     C/END-EXEC
004100981118      *----------------
004200981118     C     SQLCOD        CABNE     *ZEROS        ERROR
004300981118      *
004400060619      *  Elaboro C.A. se stampata denuncia e assegnato numero pratica e legata ad evento
004500981119     c                   IF        DCTnde > *zeros  AND  DCTprn > *zeros
004600060619     c                             and  DCTnev > *zeros
004700981119      *
004800981119      *  Stampo lettera
004900981119     c                   EXSR      PRTLET
005000981119      *
005100981119      *  Aggiorno archivi
005200981118     C/EXEC SQL
005300981119     C+ UPDATE FNDCT00F SET DCTRCD = ' ', DCTDIC = :WI26dic
005400981118     C+     WHERE CURRENT OF C1
005500981118     C/END-EXEC
005600981118      *
005700981119     C                   ENDIF
005800981119     C                   ENDDO
005900981118     C     ERROR         TAG
006000981118      *
006100981118     C                   eval      *INLR = *ON
006200981119      *****************************************************************
006300981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
006400981119      *****************************************************************
006500981119     C     PRTLET        BEGSR
006600981119      *
006700981119     c                   clear                   FIDN26DS
006800981119     c                   clear                   WI26dic
006900981119      *
007000981119     c                   z-add     DCTaac        I26aac
007100981119     c                   z-add     DCTfil        I26fil
007200981119     c                   z-add     DCTnca        I26nca
007300981119     c                   z-add     DCTaas        I26aas
007400981119     c                   z-add     DCTlnp        I26lnp
007500981119     c                   z-add     DCTnrs        I26nrs
007600981119     c                   z-add     DCTnsp        I26nsp
007700981119     c                   z-add     DCTaad        I26aad
007800981119     c                   z-add     DCTpod        I26pod
007900981119     c                   z-add     DCTnde        I26nde
008000981119     c                   movel     DCTdit        I26dit
008100981119     c                   z-add     DCTpra        I26pra
008200981119     c                   z-add     DCTprn        I26prn
008300981119     c                   movel     DCTptr        I26ptr
008400981119     c                   z-add     DCTkcc        I26kcc
008500981119     c                   z-add     DCTksc        I26ksc
008600981119      *
008700981119     C                   IF        DCTdic > *zeros
008800981216     c                   z-add     DCTdic        WI26dic
008900981216     c                   z-add     DCTdic        I26dic
009000981119     c                   ELSE
009100981216     c                   z-add     dateu         WI26dic
009200981216     c                   z-add     dateu         I26dic
009300981119     c                   ENDIF
009400981119      *
009500981119     c                   movel(P)  FIDN26DS      KPJBU
009600981119      *
009700981216     c                   CALL      'FIDN26R3'
009800981119     c                   PARM                    KPJBA
009900981119      *
010000981119     c                   ENDSR
010100981118      *****************************************************************
010200981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
010300981118      *****************************************************************
010400981118     C     *INZSR        BEGSR
010500981118      *
010600981118     C     *ENTRY        PLIST
010700981118     C                   PARM                    KPJBA
010800981119      *
010900981119     C                   TIME                    W0140
011000981119      * UDATE IN GGMMAAAA
011100981119     C                   MOVE      W0140         WDTGIO
011200981119      *
011300981119     C                   Z-ADD     WDTGIO        G02DAT
011400981119     C                   MOVEL     *BLANK        G02ERR
011500981119     C                   CALL      'XSRDA8'
011600981119     C                   PARM                    WLBDAT
011700981119      * UDATE IN AAAAMMGG
011800981119     C                   MOVEL     G02INV        DATEU
011900981119      *
012000981118     C                   ENDSR
012100981119      *-----------------------------------------------------------------
