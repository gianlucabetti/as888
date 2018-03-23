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
001900081222     C*
002000081222     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
002100081222     C
002200081222     C/EXEC SQL
002300081222     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
002400081222     C/END-EXEC
002500081222     C*
002600981118      *
002700081222      *--------  ISTRUZIONI SQL PER APERTURA FILE  --------------------
002800981118     C/EXEC SQL
002900981118     C+ DECLARE C1 CURSOR FOR
003000981118     C+   SELECT * FROM FNDCT00F
003100981118     C+     WHERE DCTRCD = 'S'
003200060630     C+       FOR UPDATE OF DCTRCD, DCTDIC, DCTPOR
003300981118     C/END-EXEC
003400981118      *
003500981118     C/EXEC SQL
003600981118     C+ OPEN C1
003700981118     C/END-EXEC
003800981118
003900981118      *----------------------------------------------------------------
004000981118     C     SQLCOD        DOUNE     0
004100981118      *----------------
004200981118     C/EXEC SQL
004300981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
004400981118     C/END-EXEC
004500981118      *----------------
004600981118     C     SQLCOD        CABNE     *ZEROS        ERROR
004700981118      *
004800060619      *  Elaboro C.A. se stampata denuncia e assegnato numero pratica e legata ad evento
004900981119     c                   IF        DCTnde > *zeros  AND  DCTprn > *zeros
005000060619     c                             and  DCTnev > *zeros
005100981119      *
005200981119      *  Stampo lettera
005300981119     c                   EXSR      PRTLET
005400981119      *
005500981119      *  Aggiorno archivi
005600981118     C/EXEC SQL
005700060629     C+ UPDATE FNDCT00F SET DCTRCD = ' ', DCTDIC = :WI26dic , DCTPOR = 046
005800981118     C+     WHERE CURRENT OF C1
005900981118     C/END-EXEC
006000981118      *
006100981119     C                   ENDIF
006200981119     C                   ENDDO
006300981118     C     ERROR         TAG
006400981118      *
006500060629     C/EXEC SQL
006600060629     C+ CLOSE C1
006700060629     C/END-EXEC
006800060629      *
006900981118     C                   eval      *INLR = *ON
007000981119      *****************************************************************
007100981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
007200981119      *****************************************************************
007300981119     C     PRTLET        BEGSR
007400981119      *
007500981119     c                   clear                   FIDN26DS
007600981119     c                   clear                   WI26dic
007700981119      *
007800981119     c                   z-add     DCTaac        I26aac
007900981119     c                   z-add     DCTfil        I26fil
008000981119     c                   z-add     DCTnca        I26nca
008100981119     c                   z-add     DCTaas        I26aas
008200981119     c                   z-add     DCTlnp        I26lnp
008300981119     c                   z-add     DCTnrs        I26nrs
008400981119     c                   z-add     DCTnsp        I26nsp
008500981119     c                   z-add     DCTaad        I26aad
008600981119     c                   z-add     DCTpod        I26pod
008700981119     c                   z-add     DCTnde        I26nde
008800981119     c                   movel     DCTdit        I26dit
008900981119     c                   z-add     DCTpra        I26pra
009000981119     c                   z-add     DCTprn        I26prn
009100981119     c                   movel     DCTptr        I26ptr
009200981119     c                   z-add     DCTkcc        I26kcc
009300981119     c                   z-add     DCTksc        I26ksc
009400081222     c                   move      DCTflo        I26flo
009500981119      *
009600981119     C                   IF        DCTdic > *zeros
009700981216     c                   z-add     DCTdic        WI26dic
009800981216     c                   z-add     DCTdic        I26dic
009900981119     c                   ELSE
010000981216     c                   z-add     dateu         WI26dic
010100981216     c                   z-add     dateu         I26dic
010200981119     c                   ENDIF
010300981119      *
010400981119     c                   movel(P)  FIDN26DS      KPJBU
010500981119      *
010600981216     c                   CALL      'FIDN26R3'
010700981119     c                   PARM                    KPJBA
010800981119      *
010900981119     c                   ENDSR
011000981118      *****************************************************************
011100981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
011200981118      *****************************************************************
011300981118     C     *INZSR        BEGSR
011400981118      *
011500981118     C     *ENTRY        PLIST
011600981118     C                   PARM                    KPJBA
011700981119      *
011800981119     C                   TIME                    W0140
011900981119      * UDATE IN GGMMAAAA
012000981119     C                   MOVE      W0140         WDTGIO
012100981119      *
012200981119     C                   Z-ADD     WDTGIO        G02DAT
012300981119     C                   MOVEL     *BLANK        G02ERR
012400981119     C                   CALL      'XSRDA8'
012500981119     C                   PARM                    WLBDAT
012600981119      * UDATE IN AAAAMMGG
012700981119     C                   MOVEL     G02INV        DATEU
012800981119      *
012900981118     C                   ENDSR
013000981119      *-----------------------------------------------------------------
