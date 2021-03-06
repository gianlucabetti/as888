000100100730     H/TITLE  Lancio estrazione conteggio autotr. con distinte aperte
000200100730     H DEBUG DECEDIT('0,') DATEDIT(*dmy.)
000300951009?     *--------------------------------------------------------------*
000400100730     Fficn480d  CF   E             WORKSTN
000500020411     D* Reperimento nome PGM
000600020411     D STATUS         SDS           333
000700020411     D  nompgm           *PROC
000800020415     D* posizione cursore
000900020415     D CURSOR          DS
001000020415     D  RIRI                   1      2B 0 INZ
001100020415     D  R$$                    2      2
001200020415     D  COCO                   3      4B 0 INZ
001300020415     D  C$$                    4      4
001400020415     D DSFMT           DS
001500020415     D  $TASTO               369    369
001600020415     D  NRG                  370    370
001700020415     D  NCL                  371    371
001800100730     d dataiso                         d   datfmt(*iso)
001900100730     d dataeur                         d   datfmt(*eur)
002000020411     D*---------------------------------------------------------------*
002100020411     D* CAMPI INTERNI
002200020411     D*---------------------------------------------------------------*
002300951009     D KPJBA         E DS
002400100730     D ficn480ds     e ds                  inz
002500951009?     *--------------------------------------------------------------*
002600951009?     *  CICLO PRINCIPALE                                            *
002700951009?     *--------------------------------------------------------------*
002800020405      *
002900100730     c                   do        *hival
003000020419     c                   if        *in28
003100020419     c                   setoff                                       28
003200020419     C                   write     y350d01
003300020419     c                   seton                                        28
003400020419     C                   END
003500020415     C                   EXFMT     y350d01
003600020415      *  Fine Lavoro
003700100730     C   kc              leave
003800951009      *  Controlli
003900951009     C                   EXSR      CTR01
004000100730     C   28              iter
004100020408      *  CONFERMA
004200951013     C     *INKF         IFEQ      '1'
004300100730     c                   clear                   ficn480ds
004400100730     c                   movel     v1cdti        dataeur
004500100730     c                   movel     dataeur       dataiso
004600100730     c                   movel     dataiso       f480dti
004700100730     C                   MOVEL(p)  ficn480ds     KPJBU
004800100730     c                   call      'FICN481R'
004900031124     c                   parm                    kpjba
005000100730     c                   leave
005100100730     C                   end                                                    V3CTOT = y35FAT
005200100730     C                   ENDDO                                                  V3CTOT = y35FAT
005300951009      *
005400100730     C                   SETON                                        LR
005500951009?     *--------------------------------------------------------------*
005600951009?     *  CTR01: Controlli prima videata                              *
005700951009?     *--------------------------------------------------------------*
005800951009     C     CTR01         BEGSR
005900951009      *
006000020419     c                   setoff                                       28
006100100730     c* data
006200020415     c                   if        v1cdti <> 0
006300020415     c     *eur          test(d)                 v1cdti                 41
006400020415     c                   if        *in41
006500020415     c     *dmy          test(d)                 v1cdti                 41
006600020415     c  n41*dmy          move      v1cdti        dataeur
006700020415     c  n41              move      dataeur       v1cdti
006800020415     c                   end
006900020415     c                   else
007000020415     c                   seton                                        41
007100020415     c                   end
007200100730     c   41              seton                                        28
007300951010      *
007400020419     C                   ENDSR
007500020423?     *--------------------------------------------------------------*
007600020423?     *  inz01  inizializza video                                    *
007700020423?     *--------------------------------------------------------------*
007800020423     C     inz01         BEGSR
007900020423      *
008000020423     c                   setoff                                       28
008100100730     c                   movel     *date         v1cdti
008200020423      *
008300020423     C                   ENDSR
008400951011?     *--------------------------------------------------------------*
008500951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
008600951011?     *--------------------------------------------------------------*
008700951011     C     *INZSR        BEGSR
008800951011      *
008900951011     C     *ENTRY        PLIST
009000951011     C                   PARM                    KPJBA
009100100730     c                   exsr      inz01
009200951012     C                   ENDSR
