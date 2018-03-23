000100100628     H/TITLE  Lancio estrazione rettifiche ai nteggi aut./coop.
000200100628     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000300951009?     *--------------------------------------------------------------*
000400100628      * ficn49R                                                      *
000500100628     Fficn49D   CF   E             WORKSTN
000600020411     D* Reperimento nome PGM
000700020411     D STATUS         SDS           333
000800020411     D  nompgm           *PROC
000900020415     D* posizione cursore
001000020415     D CURSOR          DS
001100020415     D  RIRI                   1      2B 0 INZ
001200020415     D  R$$                    2      2
001300020415     D  COCO                   3      4B 0 INZ
001400020415     D  C$$                    4      4
001500020415     D DSFMT           DS
001600020415     D  $TASTO               369    369
001700020415     D  NRG                  370    370
001800020415     D  NCL                  371    371
001900020411     D*---------------------------------------------------------------*
002000020411     D* CAMPI INTERNI
002100020411     D*---------------------------------------------------------------*
002200020411     D dataiso         s               d   datfmt(*iso)
002300020411     D dataeur         s               d   datfmt(*eur)
002400951009     D KPJBA         E DS
002500100628     D ficn49ds      e ds                  inz
002600951009?     *--------------------------------------------------------------*
002700951009?     *  CICLO PRINCIPALE                                            *
002800951009?     *--------------------------------------------------------------*
002900951009      *  Loop gestione videata
003000951009     C     WFINE         DOUEQ     'S'
003100000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003200951012     C                   END
003300020418     C                   END
003400980312     C*
003500020507      *  Fine Lavoro
003600020507     C     FINE          TAG
003700951009     C                   SETON                                        LR
003800020405?     *--------------------------------------------------------------*
003900020405?     *  GESS01: Gestione prima videata                              *
004000020405?     *--------------------------------------------------------------*
004100020405     C     GESS01        BEGSR
004200020405      *
004300020419     c                   if        *in28
004400020419     c                   setoff                                       28
004500020419     C                   write     y350d01
004600020419     c                   seton                                        28
004700020419     C                   END
004800020415     C                   EXFMT     y350d01
004900020415      *  Fine Lavoro
005000020415     C     *INKC         IFEQ      '1'
005100020415     C                   MOVEL     'S'           WFINE
005200020415     c                   goto      finvd1
005300020415     C                   END
005400951009      *  Controlli
005500951009     C                   EXSR      CTR01
005600020405     C   28              GOTO      FINVD1
005700020408      *  CONFERMA
005800951013     C     *INKF         IFEQ      '1'
005900100628     c                   clear                   ficn49ds
006000100628     c                   move      wdatda        f49dti
006100100628     c                   move      wdataa        f49dtf
006200100628     C                   MOVEL(p)  ficn49ds      KPJBU
006300020419     c     knmus         ifeq      *all'1'
006400100628     c                   call      'FICN48R'
006500031124     c                   parm                    kpjba
006600020419     c                   else
006700100628     C                   MOVEL     'FN48'        KCOAZ
006800030214     C                   CALL      'BCH10'
006900030214     C                   PARM                    KPJBA
007000020419     c                   end
007100100628     c                   eval      wfine = 'S'
007200020423     c                   exsr      inz01
007300980911     C                   END                                                    V3CTOT = y35FAT
007400951009      *
007500951009     C     FINVD1        ENDSR
007600951009?     *--------------------------------------------------------------*
007700951009?     *  CTR01: Controlli prima videata                              *
007800951009?     *--------------------------------------------------------------*
007900951009     C     CTR01         BEGSR
008000951009      *
008100020419     c                   setoff                                       28
008200020415     c* data da
008300020415     c                   if        v1cdti <> 0
008400020415     c     *eur          test(d)                 v1cdti                 41
008500020415     c                   if        *in41
008600020415     c     *dmy          test(d)                 v1cdti                 41
008700020415     c  n41*dmy          move      v1cdti        dataeur
008800020415     c  n41              move      dataeur       v1cdti
008900020415     c                   end
009000020415     c                   else
009100020415     c                   seton                                        41
009200020415     c                   end
009300020415     c* data al
009400020415     c                   if        v1cdtf <> 0
009500020415     c     *eur          test(d)                 v1cdtf                 45
009600020415     c                   if        *in45
009700020415     c     *dmy          test(d)                 v1cdtf                 45
009800020415     c  n45*dmy          move      v1cdtf        dataeur
009900020415     c  n45              move      dataeur       v1cdtf
010000020415     c                   end
010100020415     c                   else
010200020423     c                   if        not *in41
010300020423     c                   move      v1cdti        v1cdtf
010400020423     c                   else
010500020415     c                   seton                                          2845
010600020415     c                   end
010700020423     c                   end
010800020415     c                   if        *in41 or *in45
010900020415     c                   seton                                          28
011000020415     c                   else
011100020415     c                   movel     v1cdti        dataeur
011200020415     c                   movel     dataeur       dataiso
011300020415     c                   movel     dataiso       wdatda            8 0
011400020415     c                   movel     v1cdtf        dataeur
011500020415     c                   movel     dataeur       dataiso
011600020415     c                   movel     dataiso       wdataa            8 0
011700020415      *  Controllo limiti due date
011800020415     C     WDATDA        IFGT      WDATAA
011900020415     C                   SETON                                        414528
012000020415     C                   END
012100020415     c                   end
012200951010      *
012300020419     C                   ENDSR
012400020423?     *--------------------------------------------------------------*
012500020423?     *  inz01  inizializza video                                    *
012600020423?     *--------------------------------------------------------------*
012700020423     C     inz01         BEGSR
012800020423      *
012900020423     c                   setoff                                       28
013000100628     c                   movel     *date         dataiso
013100100628     c                   movel     dataiso       dtacom            8 0
013200100628     c                   move      01            dtacom
013300100628     c                   movel     dtacom        dataiso
013400100628     c     dataiso       subdur    1:*d          dataiso
013500100628     c                   movel     dataiso       dataeur
013600100628     c                   movel     dataeur       v1cdtf
013700100628     c                   movel     dataeur       v1cdti
013800100628     c                   movel     01            v1cdti
013900020423      *
014000020423     C                   ENDSR
014100951011?     *--------------------------------------------------------------*
014200951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
014300951011?     *--------------------------------------------------------------*
014400951011     C     *INZSR        BEGSR
014500951011      *
014600951011     C     *ENTRY        PLIST
014700951011     C                   PARM                    KPJBA
014800020418      *  Inizializzo variabili
014900020418     C                   MOVEL     '1'           WTPVID            1
015000020418     C                   MOVEL     'N'           WFINE             1
015100020418     C                   Z-ADD     0             CURR              2 0
015200020418     C                   Z-ADD     0             CURC              2 0
015300100628     c                   movel     *date         dataiso
015400100628     c                   movel     dataiso       dtacom            8 0
015500100628     c                   move      01            dtacom
015600100628     c                   movel     dtacom        dataiso
015700100628     c     dataiso       subdur    1:*d          dataiso
015800100628     c                   movel     dataiso       dataeur
015900100628     c                   movel     dataeur       v1cdtf
016000100628     c                   movel     dataeur       v1cdti
016100100628     c                   movel     01            v1cdti
016200951012     C                   ENDSR
