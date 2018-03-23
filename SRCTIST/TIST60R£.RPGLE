000100020415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020415     H*PARMS ACTGRP(QILE)
000300030219     H/TITLE  Lancio statistica spedizioni in distinta da sede
000400030219     H DEBUG DECEDIT('0,') DATEDIT(*ymd.)
000500951009?     *--------------------------------------------------------------*
000600030219      * tist60R                                                      *
000700030219      *     *---------------------------------------------*          *
000800030219      *         statistica spedizioni in distinta da sede            *
000900951009      *     *---------------------------------------------*          *
001000951009      *                                                              *
001100951009?     *--------------------------------------------------------------*
001200951009      * KC - FINE LAVORO                                             *
001300951009      * KF - CONFERMA                                                *
001400951009?     *--------------------------------------------------------------*
001500030219     Ftist60D   CF   E             WORKSTN
001800020411     D* Reperimento nome PGM
001900020411     D STATUS         SDS           333
002000020411     D  nompgm           *PROC
002100020411     D*---------------------------------------------------------------*
002200020411     D* CAMPI INTERNI
002300020411     D*---------------------------------------------------------------*
002400030310     D datacom         s               d   datfmt(*iso)
002401030310     D dataiso         s               d   datfmt(*iso)
002500020411     D dataeur         s               d   datfmt(*eur)
002600951009     D KPJBA         E DS
002900030219     D tist60ds      e ds                  inz
003000951009?     *--------------------------------------------------------------*
003100951009?     *  CICLO PRINCIPALE                                            *
003200951009?     *--------------------------------------------------------------*
003300951009      *  Loop gestione videata
003400951009     C     WFINE         DOUEQ     'S'
003500000426     C     WTPVID        CASEQ     '1'           GESS01                         sfl pieno
003600951012     C                   END
003700020418     C                   END
003800980312     C*
003900020529     C     FINE          TAG
004000951009     C                   SETON                                        LR
004100020405?     *--------------------------------------------------------------*
004200020405?     *  GESS01: Gestione prima videata                              *
004300020405?     *--------------------------------------------------------------*
004400020405     C     GESS01        BEGSR
004500020405      *
004600020419     c                   if        *in28
004700020419     c                   setoff                                       28
004800020419     C                   write     y350d01
004900020419     c                   seton                                        28
005000020419     C                   END
005100020415     C                   EXFMT     y350d01
005200020415      *  Fine Lavoro
005300020415     C     *INKC         IFEQ      '1'
005400020415     C                   MOVEL     'S'           WFINE
005500020415     c                   goto      finvd1
005600020415     C                   END
005700951009      *  Controlli
005800951009     C                   EXSR      CTR01
005900020405     C   28              GOTO      FINVD1
006000020408      *  CONFERMA
006100951013     C     *INKF         IFEQ      '1'
006200030219     c                   clear                   tist60ds
006400030219     c                   eval      t60sta = stativ
006401030319     c                   eval      t60det = detv
006402030219     c                   move      wdatda        t60dti
006500030219     c                   move      wdataa        t60dtf
006600030219     C                   MOVEL(p)  tist60ds      KPJBU
006700020419     c     knmus         ifeq      *all'1'
006800030219     c                   call      'TIST61C'
006900020419     c                   parm                    kpjba
007000020419     c                   else
007100030219     C                   MOVEL     'ST61'        KCOAZ
007200020419     C                   CALL      'BCH10'
007300020419     C                   PARM                    KPJBA
007400020419     c                   end
007500020423     c                   movel     'S'           wfine
007600980911     C                   END                                                    V3CTOT = y35FAT
007700951009      *
007800951009     C     FINVD1        ENDSR
007900951009?     *--------------------------------------------------------------*
008000951009?     *  CTR01: Controlli prima videata                              *
008100951009?     *--------------------------------------------------------------*
008200951009     C     CTR01         BEGSR
008300951009      *
008400020419     c                   setoff                                       28
008500020415     c* data da
008600020415     c                   if        v1cdti <> 0
008700020415     c     *eur          test(d)                 v1cdti                 41
008800020415     c                   if        *in41
008900020415     c     *dmy          test(d)                 v1cdti                 41
009000020415     c  n41*dmy          move      v1cdti        dataeur
009100020415     c  n41              move      dataeur       v1cdti
009200020415     c                   end
009300020415     c                   else
009400020415     c                   seton                                        41
009500020415     c                   end
009501030310     c* se statistica = 1 la data non deve essere inferiore di 30 gg
009502030219     c                   if        not *in41 and stativ = '1'
009503030310     c                   move      dataeur       dataiso
009505030310     c     dataiso       comp      datacom                              41
009506030219     c                   end
009600020415     c* data al
009700020415     c                   if        v1cdtf <> 0
009800020415     c     *eur          test(d)                 v1cdtf                 45
009900020415     c                   if        *in45
010000020415     c     *dmy          test(d)                 v1cdtf                 45
010100020415     c  n45*dmy          move      v1cdtf        dataeur
010200020415     c  n45              move      dataeur       v1cdtf
010300020415     c                   end
010400020415     c                   else
010500020423     c                   if        not *in41
010600020423     c                   move      v1cdti        v1cdtf
010700020423     c                   else
010800020415     c                   seton                                          2845
010900020415     c                   end
011000020423     c                   end
011002030219     c
011100020415     c                   if        *in41 or *in45
011200020415     c                   seton                                          28
011300020415     c                   else
011400020415     c                   movel     v1cdti        dataeur
011500020415     c                   movel     dataeur       dataiso
011600020415     c                   movel     dataiso       wdatda            8 0
011700020415     c                   movel     v1cdtf        dataeur
011800020415     c                   movel     dataeur       dataiso
011900020415     c                   movel     dataiso       wdataa            8 0
012000020415      *  Controllo limiti due date
012100020415     C     WDATDA        IFGT      WDATAA
012200020415     C                   SETON                                        414528
012300020415     C                   END
012400020415     c                   end
013300951010      *
013400020419     C                   ENDSR
015000951011?     *--------------------------------------------------------------*
015100951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
015200951011?     *--------------------------------------------------------------*
015300951011     C     *INZSR        BEGSR
015400951011      *
015500951011     C     *ENTRY        PLIST
015600951011     C                   PARM                    KPJBA
015700020418      *  Inizializzo variabili
015800020418     C                   MOVEL     '1'           WTPVID            1
015900020418     C                   MOVEL     'N'           WFINE             1
015901030219     c                   eval      stativ = '1'
015902030319     c                   eval      detv = 'N'
015903030310     c                   move      udate         datacom
015904030310     c                   subdur    30:*d         datacom
019100951012     C                   ENDSR
