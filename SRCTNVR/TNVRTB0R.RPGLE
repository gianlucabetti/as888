000100020722     H/TITLE  tarscodifica tabella TABEL00F a tabella TNTBE00F
000200020415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300951009?     *--------------------------------------------------------------*
000400020722      * tnvrtb0r                                                     *
000500951009      *     *----------------------------------------------*         *
000600020722      *         trascodifica tabella TABEL00F a tabella TNTBE00F     *
000700951009      *     *---------------------------------------------*          *
000800020723      *                                                              *
000900020723      * Il pgm copia una tabella di TABEL in una di TNTBE.           *
001000020723      * se si lavora in ambiente di sede si può usare il multilancio *
001100020723      * se si vuole allineare l'ambiente di prova delle filiali biso-*
001200020723      * gna lanciarlo singolarmente impostando il S.I. corretto.
001300951009      *                                                              *
001400951009?     *--------------------------------------------------------------*
001500951009      * KC - FINE LAVORO                                             *
001600951009      * KF - CONFERMA                                                *
001700951009?     *--------------------------------------------------------------*
001800020722     Ftnvrtb0d  CF   E             WORKSTN
001900020722     Ftabel00f  IF   E           K DISK
002000020411     D* Reperimento nome PGM
002100020411     D STATUS         SDS           333
002200020411     D  nompgm           *PROC
002300020411     D*---------------------------------------------------------------*
002400020411     D* CAMPI INTERNI
002500020411     D*---------------------------------------------------------------*
002600951009     D KPJBA         E DS
002700020723     d prova                  98    100
002800020722     D tnvrtb1ds     e ds                  inz
002900951009?     *--------------------------------------------------------------*
003000951009?     *  CICLO PRINCIPALE                                            *
003100951009?     *--------------------------------------------------------------*
003200951009      *  Loop gestione videata
003300951009     C     WFINE         DOUEQ     'S'
003400000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003500951012     C                   END
003600020418     C                   END
003700980312     C*
003800020529     C     FINE          TAG
003900951009     C                   SETON                                        LR
004000020405?     *--------------------------------------------------------------*
004100020405?     *  GESS01: Gestione prima videata                              *
004200020405?     *--------------------------------------------------------------*
004300020405     C     GESS01        BEGSR
004400020405      *
004500020419     c                   if        *in28
004600020419     c                   setoff                                       28
004700020419     C                   write     y350d01
004800020419     c                   seton                                        28
004900020419     C                   END
005000020415     C                   EXFMT     y350d01
005100020415      *  Fine Lavoro
005200020415     C     *INKC         IFEQ      '1'
005300020415     C                   MOVEL     'S'           WFINE
005400020415     c                   goto      finvd1
005500020415     C                   END
005600951009      *  Controlli
005700951009     C                   EXSR      CTR01
005800020405     C   28              GOTO      FINVD1
005900020408      *  CONFERMA
006000951013     C     *INKF         IFEQ      '1'
006100020722     c                   clear                   tnvrtb1ds
006200020722     c                   move      xscsoc        tnvsoc
006300020722     c                   movel     tabelv        tnvda
006400020722     c                   move      tntbev        tnva
006500020722     C                   MOVEL(p)  tnvrtb1ds     KPJBU
006600020722     c                   call      'TNVRTB1R'
006700020419     c                   parm                    kpjba
006800020423     c                   movel     'S'           wfine
006900980911     C                   END                                                    V3CTOT = y35FAT
007000951009      *
007100951009     C     FINVD1        ENDSR
007200951009?     *--------------------------------------------------------------*
007300951009?     *  CTR01: Controlli prima videata                              *
007400951009?     *--------------------------------------------------------------*
007500951009     C     CTR01         BEGSR
007600951009      *
007700020419     c                   setoff                                       28
007800020415     c* data da
007900020722     c                   if        tabelv <> ' '
008000020722     c     ktab          chain     tabel00f                           45
008100020723     c   45              seton                                        28
008200020415     c                   else
008300020723     c                   seton                                        4528
008400020415     c                   end
008500951010      *
008600020419     C                   ENDSR
008700951011?     *--------------------------------------------------------------*
008800951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
008900951011?     *--------------------------------------------------------------*
009000951011     C     *INZSR        BEGSR
009100951011      *
009200951011     C     *ENTRY        PLIST
009300951011     C                   PARM                    KPJBA
009400020418      *  Inizializzo variabili
009500020418     C                   MOVEL     '1'           WTPVID            1
009600020418     C                   MOVEL     'N'           WFINE             1
009700020722     c* se il sistema informativo è di prova filiale (GAITRAPFP o FILTRAPFP)
009800020722     c* non chiamo XSOC e imposto la società = 201
009900020723     c                   clear                   xscsoc            3
010000020722     c                   if        prova = 'PFP'
010100020723     c                   move      '201'         xscsoc
010200020722     c                   else
010300020415     C*---------- RICERCA DITTA :
010400020723     c                   clear                   ok                1
010500020723     c                   call      'TNVRTB2R'
010600020723     c                   parm                    xscsoc
010700020723     c                   parm                    ok
010800020723     c                   if        ok = '1'
010900020529     c                   goto      fine
011000980911     c                   end
011100020722     c                   end
011200020722     C     Ktab          KLIST
011300020722     C                   KFLD                    tblkut
011400020722     C                   KFLD                    tabelv
011500020722     c                   z-add     1             tblkut
011600020725     c                   movel     '4O'          tabelv
011700020725     c                   movel(p)  'Y4O'         tntbev
011800951012     C                   ENDSR
