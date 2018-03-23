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
002000981118      *
002100981118     C/EXEC SQL
002200981118     C+ DECLARE C1 CURSOR FOR
002300981118     C+   SELECT * FROM FNDCT00F
002400981118     C+     WHERE DCTRCD = 'S'
002500060630     C+       FOR UPDATE OF DCTRCD, DCTDIC, DCTPOR
002600981118     C/END-EXEC
002700981118      *
002800981118     C/EXEC SQL
002900981118     C+ OPEN C1
003000981118     C/END-EXEC
003100981118
003200981118      *----------------------------------------------------------------
003300981118     C     SQLCOD        DOUNE     0
003400981118      *----------------
003500981118     C/EXEC SQL
003600981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
003700981118     C/END-EXEC
003800981118      *----------------
003900981118     C     SQLCOD        CABNE     *ZEROS        ERROR
004000981118      *
004100060619      *  Elaboro C.A. se stampata denuncia e assegnato numero pratica e legata ad evento
004200981119     c                   IF        DCTnde > *zeros  AND  DCTprn > *zeros
004300060619     c                             and  DCTnev > *zeros
004400981119      *
004500981119      *  Stampo lettera
004600981119     c                   EXSR      PRTLET
004700981119      *
004800981119      *  Aggiorno archivi
004900981118     C/EXEC SQL
005000060629     C+ UPDATE FNDCT00F SET DCTRCD = ' ', DCTDIC = :WI26dic , DCTPOR = 046
005100981118     C+     WHERE CURRENT OF C1
005200981118     C/END-EXEC
005300981118      *
005400981119     C                   ENDIF
005500981119     C                   ENDDO
005600981118     C     ERROR         TAG
005700981118      *
005800060629     C/EXEC SQL
005900060629     C+ CLOSE C1
006000060629     C/END-EXEC
006100060629      *
006200981118     C                   eval      *INLR = *ON
006300981119      *****************************************************************
006400981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
006500981119      *****************************************************************
006600981119     C     PRTLET        BEGSR
006700981119      *
006800981119     c                   clear                   FIDN26DS
006900981119     c                   clear                   WI26dic
007000981119      *
007100981119     c                   z-add     DCTaac        I26aac
007200981119     c                   z-add     DCTfil        I26fil
007300981119     c                   z-add     DCTnca        I26nca
007400981119     c                   z-add     DCTaas        I26aas
007500981119     c                   z-add     DCTlnp        I26lnp
007600981119     c                   z-add     DCTnrs        I26nrs
007700981119     c                   z-add     DCTnsp        I26nsp
007800981119     c                   z-add     DCTaad        I26aad
007900981119     c                   z-add     DCTpod        I26pod
008000981119     c                   z-add     DCTnde        I26nde
008100981119     c                   movel     DCTdit        I26dit
008200981119     c                   z-add     DCTpra        I26pra
008300981119     c                   z-add     DCTprn        I26prn
008400981119     c                   movel     DCTptr        I26ptr
008500981119     c                   z-add     DCTkcc        I26kcc
008600981119     c                   z-add     DCTksc        I26ksc
008700981119      *
008800981119     C                   IF        DCTdic > *zeros
008900981216     c                   z-add     DCTdic        WI26dic
009000981216     c                   z-add     DCTdic        I26dic
009100981119     c                   ELSE
009200981216     c                   z-add     dateu         WI26dic
009300981216     c                   z-add     dateu         I26dic
009400981119     c                   ENDIF
009500981119      *
009600981119     c                   movel(P)  FIDN26DS      KPJBU
009700981119      *
009800981216     c                   CALL      'FIDN26R3'
009900981119     c                   PARM                    KPJBA
010000981119      *
010100981119     c                   ENDSR
010200981118      *****************************************************************
010300981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
010400981118      *****************************************************************
010500981118     C     *INZSR        BEGSR
010600981118      *
010700981118     C     *ENTRY        PLIST
010800981118     C                   PARM                    KPJBA
010900981119      *
011000981119     C                   TIME                    W0140
011100981119      * UDATE IN GGMMAAAA
011200981119     C                   MOVE      W0140         WDTGIO
011300981119      *
011400981119     C                   Z-ADD     WDTGIO        G02DAT
011500981119     C                   MOVEL     *BLANK        G02ERR
011600981119     C                   CALL      'XSRDA8'
011700981119     C                   PARM                    WLBDAT
011800981119      * UDATE IN AAAAMMGG
011900981119     C                   MOVEL     G02INV        DATEU
012000981119      *
012100060629     C*
012200060629     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
012300060629     C
012400060629     C/EXEC SQL
012500060629     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
012600060629     C/END-EXEC
012700060629     C*
012800981118     C                   ENDSR
012900981119      *-----------------------------------------------------------------
