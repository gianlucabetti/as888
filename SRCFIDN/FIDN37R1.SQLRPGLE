000100981118     H DECEDIT('0,') DATEDIT(*DMY.)
000200981118
000300981119     D DS_FNDCT      E DS                  EXTNAME(FNDCT00F)
000400060628     D FIDN37DS      E DS
000500981118     D KPJBA         E DS
000600060628     D DCCH          E DS
000700060628     D TIBS02DS      E DS
000800060628
000900060628      *
001000060628     d Param           ds
001100060628     d  poges                  1      3  0
001200060628     d  dtaris                 4     11  0
001300060628      *
001400981119     D WLBDAT          DS                  INZ
001500981119     D  G02DAT                 1      8  0
001600981119     D  G02INV                 9     16  0
001700981119     D  G02ERR                17     17
001800981119     D  G02TGI                18     22  0
001900060628      *
002000060628      *   S C H I E R E
002100060628     D CCH             S              2    DIM(30)                              causali chiusura
002200060628     D IMA             S              1    DIM(30)                              ima 'S'
002300981119      *
002400981119     D W0140           S             14  0
002500981119     D Wdtgio          S              8  0
002600981119     D dateu           S              8  0
002700060628     D e               s              3  0                                      indice
002800060628     D x               s              3  0                                      indice
002900060628     d Wfpad           S                   like(DCTfca) inz(400)
003000060628     d Wfchi           S                   like(DCTfca) inz(900)
003100981118
003200981118      *---------------------------------------------------------------*
003300981118      *--------  ISTRUZIONI SQL PER APERTURA FILE  --------------------
003400981118      *
003500981118     C/EXEC SQL
003600981118     C+ DECLARE C1 CURSOR FOR
003700981118     C+   SELECT * FROM FNDCT00F
003800981118     C+     WHERE DCTRCD = 'S'
003900060628     C+       FOR READ ONLY
004000981118     C/END-EXEC
004100981118      *
004200981118     C/EXEC SQL
004300981118     C+ OPEN C1
004400981118     C/END-EXEC
004500981118
004600981118      *----------------------------------------------------------------
004700981118     C     SQLCOD        DOUNE     0
004800981118      *----------------
004900981118     C/EXEC SQL
005000981118     C+ FETCH NEXT FROM C1 INTO :DS_FNDCT
005100981118     C/END-EXEC
005200981118      *----------------
005300981118     C     SQLCOD        CABNE     *ZEROS        ERROR
005400981118      *
005500060628      *  Elaboro C.A. se :
005600060628      *     Pratica assicurativa
005700060628     c                   IF        DCTfpr <> 'P'
005800060628     c                   iter
005900060628     c                   endif
006000060628      *     Fase che non sia minore della 400
006100060628     c                   If        DCTfca < wfpad
006200060628     c                   iter
006300060628     c                   endif
006400060628      *     Chiusa controllo CA di chiusura diversa da pareggio / errata apertrura ecc
006500060628     c                   If        DCTfca = wfchi
006600060628     c                   z-add     1             x
006700060628     c     dctcch        lookup    cch(x)                                 29
006800060628
006900060628     c                   If        not %found
007000060628     c                   exsr      Chcch
007100060628     c                   endif
007200060628
007300060628     c                   if        ima(x) = 'S'
007400060628     c                   iter
007500060628     c                   endif
007600060628
007700060628     c                   endif
007800060628      *     CA senza evento
007900060628     c                   If        DCTnev > 0
008000060628     c                   iter
008100060628     c                   endif
008200060628      *     tipo anomalia diverso da avaria o mancanze particolari non elaboro
008300060628     c                   If        DCTtad = '03' or DCTtad = '23' or
008400060628     c                             DCTtad = '24' or DCTtad = '25'
008500060628     c                   iter
008600060628     c                   endif
008700060628      * Rimando al programma di stampa Autodichiarazione il controllo del Beneficiario
008800060628      * appartenente al P.O. gestione oppure no in quanto farei un lavoro doppio
008900981119      *
009000981119      *  Stampo lettera
009100981119     c                   EXSR      PRTLET
009200981119      *
009300981119     C                   ENDDO
009400981118     C     ERROR         TAG
009500060629     C
009600060629     C/EXEC SQL
009700060629     C+ CLOSE C1
009800060629     C/END-EXEC
009900981118      *
010000981118     C                   eval      *INLR = *ON
010100981119      *****************************************************************
010200981119      * RICHIAMO PGM PER STAMPA SINGOLA LETTERA
010300981119      *****************************************************************
010400981119     C     PRTLET        BEGSR
010500981119      *
010600060628     c                   clear                   FIDN37DS
010700981119      *
010800060628     c                   eval      I37mod = 'G'
010900060628     c                   z-add     DCTaac        I37aac
011000060628     c                   z-add     DCTfil        I37fil
011100060628     c                   z-add     DCTnca        I37nca
011200060628     c                   z-add     poges         I37fgs
011300981119      *
011400060628     c                   Select
011500060628
011600060628      * verifico il tipo denuncia da stampare in base al tipo anomalia
011700060628      * prevista autodenuncia per ammanchi prima della consegna
011800060628     c                   when      Dcttad = '02'
011900060628     c                   eval      I37tde = '1'
012000060628      * prevista autodenuncia per ammanchi dopo la consegna
012100060628     c                   when      Dcttad = '22'
012200060628     c                   eval      I37tde = '2'
012300060628      * prevista autodenuncia per mancanze
012400060628     c                   when      Dcttad = '01' or Dcttad = '21'
012500060628     c                   eval      I37tde = '3'
012600060628     c                   endsl
012700060628      *
012800060628     c                   movel(P)  FIDN37DS      KPJBU
012900981119      *
013000060628     c                   CALL      'FIDN37R3'
013100981119     c                   PARM                    KPJBA
013200981119      *
013300981119     c                   ENDSR
013400060628      *---------------------------------------------------------------*
013500060628      * Controllo causale di chiusura
013600060628      *---------------------------------------------------------------*
013700060628     c     chcch         Begsr
013800060628
013900060628      *
014000060628     c                   clear                   DCCH
014100060628      *
014200060628     c                   clear                   TIBS02DS
014300060628     c                   MOVEL     'C'           t02mod
014400060628     c                   MOVEL     knsif         t02sif
014500060628     c                   MOVEL     'CCH'         t02cod
014600060628     c                   MOVEL(P)  dctcch        t02ke1
014700060628      *
014800060628     c                   CALL      'TIBS02R'
014900060628     c                   PARM                    KPJBA
015000060628     c                   PARM                    TIBS02DS
015100060628      *
015200060628     c                   if        t02err = *BLANKS
015300060628     c                   MOVEL     T02UNI        DCCH
015400060628     c                   endif
015500060628      *
015600060628     c* carico la causale di chiusura in schiera e il suo flag IMA
015700060628     c                   add       1             e
015800060628     c                   movea     dctcch        cch(e)
015900060628     c                   movea     §cchima       ima(e)
016000060628     c                   eval      x=e
016100060628      *
016200060628     c                   endsr
016300981118      *****************************************************************
016400981118      **  DEFINIZIONE PARAMETRI E CHIAVI INIZIALI                    **
016500981118      *****************************************************************
016600981118     C     *INZSR        BEGSR
016700981118      *
016800981118     C     *ENTRY        PLIST
016900981118     C                   PARM                    KPJBA
017000060628     c                   movel     kpjbu         param
017100060628
017200981119     C                   TIME                    W0140
017300981119      * UDATE IN GGMMAAAA
017400981119     C                   MOVE      W0140         WDTGIO
017500981119      *
017600981119     C                   Z-ADD     WDTGIO        G02DAT
017700981119     C                   MOVEL     *BLANK        G02ERR
017800981119     C                   CALL      'XSRDA8'
017900981119     C                   PARM                    WLBDAT
018000981119      * UDATE IN AAAAMMGG
018100981119     C                   MOVEL     G02INV        DATEU
018200981119      *
018300060629     C*
018400060629     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
018500060629     C
018600060629     C/EXEC SQL
018700060629     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
018800060629     C/END-EXEC
018900060629     C*
019000981118     C                   ENDSR
019100981119      *-----------------------------------------------------------------
