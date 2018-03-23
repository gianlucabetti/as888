000100010213     H DECEDIT('0,') DATEDIT(*YMD.)
000200010213
000300010213      **************************************************************************
000400010213      *                                                                        *
000500011212      *          Pulizia TFNTC00F dopo la pulizia dei potenziali               *
000600010213      *                                                                        *
000700010213      *                        E S E C U Z I O N E                             *
000800010213      *                                                                        *
000900010213      **************************************************************************
001000010213
001100010213     FTFNTC01L  UF   E           K DISK
001200010216     FTFNTCDLT  O    E             DISK    RENAME(TFNTC:TFNTCD)
001300011212     FTNCPO01L  IF   E           K DISK
001400010213
001500010213     D KPJBA         E DS
001600010213     D CNCR80        E DS
001700010213     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001800010214     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001900010214     D  LIN                    1     90  0
002000010214     D                                     DIM(30)
002100010219     D DS1T          E DS
002200010216     D PARAM           DS
002300010302      * S=Simulazione
002400010302     D  VTIPOE                        1                                         S=Simulazione
002500010302      * Stampa: S=Si  N=No
002600010302     D  VSTAMP                        1                                         Stampa: S=Si  N=No
002700010213
002800011212     D Kapl            s                   like(ntcapl) inz('P')
002900011212     D wapl            s                   like(ntcapl)
003000011212     D wnk1            s                   like(ntcnk1)
003100011212     D wnk2            s                   like(ntcnk2)
003200011212     D wtnt            s                   like(ntctnt)
003300010213
003400010213      *------------------------------------------------------------------------*
003500010213      *                                                                        *
003600010213      *    Significato campo "wdelet" (causale annullamento record)            *
003700010213      *                                                                        *
003800010517      *    A = Nota annullata                                                  *
003900010302      * p  P = Potenziale non esistente in anagrafica (TNCPO)                  *
004000010213      *                                                                        *
004100010213      *------------------------------------------------------------------------*
004200010213
004300010213      *------------------------------------------------------------------------*
004400010213      *                                                                        *
004500010213      *    OPERAZIONI INIZIALI                                                 *
004600010213      *                                                                        *
004700010213      *------------------------------------------------------------------------*
004800010213
004900010213     C     *ENTRY        PLIST
005000010213     C                   PARM                    KPJBA
005100010216     C                   movel     KPJBU         PARAM
005200011212
005300010213
005400010213      *------------------------------------------------------------------------*
005500010213      *                                                                        *
005600010213      *    ROUTINE PRINCIPALE                                                  *
005700010213      *                                                                        *
005800010213      *------------------------------------------------------------------------*
005900010213
006000010213      *    Lettura File Note
006100010213      *    =================
006200010213
006300011212      * leggo le sole note relative ai potenziali
006400011212
006500011212
006600011212     C     kapl          setll     TFNTC01L
006700010213     C                   do        *hival
006800011212     C     kapl          reade     TFNTC01L
006900010213     C                   if        %eof(tfntc01l)
007000010213     C                   leave
007100010213     C                   endif
007200010213
007300010213      *    Sflaggo la causale di annullamento record
007400010213
007500010213     C                   clear                   wdelet            1
007600010213
007700010214
007800010214      *    Controlli
007900010214      *    =========
008000010213
008100010517      * note annullate le deleto
008200010517     C                   if        ntcflt = 'A'
008300010517     C                   eval      wdelet = 'A'                                 pulisco le note annu
008400010517     C                   endif
008500010219
008600011212      *    Controllo su potenziali
008700010214
008800010214     C     wdelet        ifeq      ' '
008900010214     C     ntcapl        andeq     'P'
009000010214     C                   move      ntcnk1        wnum11           11 0
009100010214     C     wnum11        chain     tncpo01l
009200010214     C                   if        not %found(tncpo01l)
009300010214     C                   eval      wdelet = 'P'                                 Non esiste TNCPO
009400011212     C                   endif
009500010214     C                   endif
009600010214
009700010214      *    Aggiornamento Note
009800010214      *    ==================
009900010214
010000010216     C     vtipoe        ifne      'S'
010100011212      *    ....... se è da tenere sflaggo
010200011212      *    ....... per la trasmissione
010300010214     C     wdelet        ifeq      ' '
010400010606      *
010500010214     C                   eval      ntcflt = ' '
010600010214     C                   clear                   ntcntr
010700010216     C                   update    tfntc                                        Da Trasmettere
010800010216      *                  ----------------
010900010606      *
011000010219      *    ....... se è da cancellare: cancello e scrivo dlt
011100010214     C                   else
011200010517      * solo se non è una nota annullata
011300010517     c     wdelet        ifne      'A'
011400010216     C                   write     tfntcd                                       Scrivo DLT
011500010517     c                   endif
011600010216      *                  ----------------
011700010216     C                   delete    tfntc                                        Cancello
011800010216      *                  ---------------
011900010214     C                   endif
012000010214     C                   endif
012100010213
012200010213     C                   enddo
012300010213
012400010213      *    Fine Programma
012500010606      *    ==============
012600010213
012700010214
012800010213     C                   eval      *inlr = *on
012900010219
