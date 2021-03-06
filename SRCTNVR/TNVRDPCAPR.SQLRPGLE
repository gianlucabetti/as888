000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200040820     H**********************************************************************
000300180108     FTABEL00F  IF   E           K DISK
000400180129     FfiPND01L  IF   E           K DISK
000500180129     Fwfdpcnf0f o  a E             DISK
000600180108     H**********************************************************************
000700180108      * Codici ISO nazioni x E.D.I.
000800180108     D C15             S              3    DIM(1000)
000900180108     D ISO             S              2    DIM(1000)
001000180109     D DES             S             25    DIM(1000)
001100180108     D DS15          E DS
001200040820     D*
001300180129     D tas00F        e DS                  extname(titas00F)
001400180108     D tisid4ds      e DS
001500180129     D tisie3ds      e DS
001600040820     D*
001700040820     D WLBDAT          DS
001800040820     D  G02DAT                 1      8  0
001900040820     D  G02INV                 9     16  0
002000040820     D  G02ERR                17     17
002100040820     D  G02TGI                18     22  0
002200040820      *
002300180227     D DATArif         S              8  0 inz(20180401)
002301180227      **
002400180129      *
002500180215     D CONTA           S              9  0
002600180215     D savCONTA        S              9  0
002700180215      *
002800180129     D WrkSqlCmd       S           1024
002900040820     D  data_Oggi      s                   like(G02INV) INZ(0)
003000040820      *
003100040820     D digits          C                   CONST('0123456789')
003200040820      *****************************************************************
003300180108     C/EXEC SQL
003400180129     C+ DELETE FROM WFDPCNF0F
003500180108     C/END-EXEC
003600180108      *****************************************************************
003700011210     C/EXEC SQL
003800011210     C+ PREPARE S1 FROM :WrkSqlCmd
003900011210     C/END-EXEC
004000011210
004100011210     C/EXEC SQL
004200011210     C+ DECLARE A1 CURSOR FOR S1
004300011210     C/END-EXEC
004400011210
004500011210     C/EXEC SQL
004600011210     C+ OPEN A1
004700011210     C/END-EXEC
004800040820
004900040820     C                   DOU       SqlCod <> 0
005000040823     c*
005100011205     C/EXEC SQL
005200180129     C+ FETCH NEXT FROM A1 INTO :TAS00F
005300011205     C/END-EXEC
005400011210     C                   SELECT
005500041007     c*
005600041007     c* a fine file Totali x rotture
005700041007     C                   WHEN      SqlCod = 100
005800041007     c                   leave
005900041007     **
006000041007     C                   WHEN      SqlCod < 0
006100041007     C                   seton                                        H1
006200041007     c                   goto      fine
006300040820     c*
006400041007     C                   OTHER
006500180215     c*
006600180215     c                   eval      conta = conta + 1
006700180214      *
006800180129     c     kPND          chain     fiPND01L
006900180129     c                   z-add     101           servcode          3 0
007000180129     c                   if        %Found(fiPND01L)
007100180129     c                   move      pndISC        servcode
007200180129     c                   end
007300180215     c*
007400040820     c* DETTAGLIO
007500180108     c                   clear                   tisid4ds
007600180109     c                   clear                   desNAZ
007700180108     c*
007800180129     C                   Z-ADD     DATArif       ISID4DRI
007900180129     C                   Z-ADD     tasLNP        ISID4LNP
008000180108     C                   Z-ADD     1             YY                3 0
008100180129     C     tasNZD        LOOKUP    C15(YY)                                32
008200180129     C  n32              movel     tasNZD        ISID4NZD
008300180108     C   32              movel     C15(YY)       ISID4NZD
008400180109     C   32              movel     DES(YY)       desNAZ           25
008500180129     C                   movel     tasCAD        ISID4CAD
008600180129     C                   Z-ADD     ServCode      ISID4SORC
008700180108      *
008800180108     c                   call      'TISID4R'
008900180108     C                   PARM                    TISID4DS
009000180214      *
009100180108     C                   clear                   WFDPD0
009200180129      *--
009300180108     c                   movel     OSID4DSTR     OUTDEP4
009400180215      *
009500180221     c******             goto      DOPO_FORZA
009600180129      *
009700180129     c                   clear                   tisie3ds
009800180129     C                   Z-ADD     DATArif       ISIE3DRI
009900180129     C                   Z-ADD     DATArif       ISIE3DSP
010000180129     C                   Z-ADD     080000        ISIE3HSP
010100180129     C  n32              movel     tasNZD        ISIE3NZD
010200180129     C   32              movel     C15(YY)       ISIE3NZD
010300180129     C                   movel     tasCAD        ISIE3CAD
010400180129     C                   Z-ADD     ServCode      ISIE3SRV
010500180129     C                   Z-ADD     tasLNP        ISIE3LNP
010600180129      *
010700180129     c                   call      'TISIE3R'
010800180129     C                   PARM                    TISIE3DS
010900180129      *
011000180129     C                   movel     OSIE3DDEP     OUT3DDEP
011100180215      *
011200180221     c**** DOPO_FORZA    tag
011300180221     c*****              goto      salto
011400180108      *
011500180129      *   vecchio cappario instradamento
011600180129      *     solo se diverso il depot trovato fra le 2 versioni
011700180129     c                   if        out3ddep <> OUTDEP4
011800180129     c                   z-add     ServCode      INPSOCODE
011900180129     c                   movel     tasCAD        INPPOSTAL
012000180129     c                   movel     tasNZD        INPDCOUN
012100180129     c                   movel     desNAZ        INPDESCC
012200180129     c                   eval        CMPDEPSTR = %subst(pndeTIU:2:4)
012300180129     c                   movel     OSID4DDPT     OUTDEP7
012400180129     c                   movel     OSID4ERR      OUTERR
012500180129     c                   movel     OSID4ERRL     OUTERRL
012600180129     c                   movel     OSID4ERRD     OUTERRD
012700180129     c                   movel     OSID4SSORT    OUTSSORT
012800180129     c                   movel     OSID4OSORT    OUTOSORT
012900180129     c                   movel     OSID4CSORT    OUTCSORT
013000180129     c                   movel     OSID4DSORT    OUTDSORT
013100180129     c                   z-add     tasAAS        INPAAS
013200180129     c                   z-add     tasLNP        INPLNP
013300180129     c                   z-add     tasNRS        INPNRS
013400180129     c                   z-add     tasNSP        INPNSP
013500180130     c                   movel     tasTBL        INPTBL
013600180130     c                   movel     tasMGS        INPMGS
013700180129     c                   movel     OSIE3OSRT     OUT3OSORT
013800180129     c                   movel     OSIE3DSRT     OUT3DSORT
013900180129     C                   movel     OSIE3ERR      OUTE3ERR
014000180129     C                   movel     OSIE3ERRL     OUTE3ERRL
014100180129     C                   movel     OSIE3ERRD     OUTE3ERRD
014200180108     C                   write     WFDPD0
014300180129     c                   end
014400180221     c**** salto         tag
014500180108      *
014600011210     C                   ENDSL
014700040820
014800011205     C                   ENDDO
014900011205     C/EXEC SQL
015000011205     C+ CLOSE A1
015100011205     C/END-EXEC
015200180215     c                   eval      savconta = conta
015300040823      *
015400041007     c     fine          tag
015500020326     C                   SETON                                        LR
015600040820     C**************************************************************************
015700040820     C* INIZIALIZZAZIONE
015800040820     C**************************************************************************
015900040820     C     *inzsr        BEGSR
016000180108      **
016100180108     C     KTAB          KLIST
016200180108     C                   KFLD                    KKUT
016300180108     C                   KFLD                    KCOD
016400180129      **
016500180129     C     KPND          KLIST
016600180129     C                   KFLD                    tasAAS
016700180129     C                   KFLD                    tasLNP
016800180129     C                   KFLD                    tasNRS
016900180129     C                   KFLD                    tasNSP
017000180108      **
017100180108     C     *LIKE         DEFINE    TBLKUT        KKUT
017200180108     C     *LIKE         DEFINE    TBLCOD        KCOD
017300040820      **
017400040820      *  imposta UDATE di elaborazione
017500040820     C                   move      *date         G02DAT
017600040820     C                   MOVE      *ZEROS        G02INV
017700040820     C                   MOVE      *BLANKS       G02ERR
017800040820     C                   CALL      'XSRDA8'
017900040820     C                   PARM                    WLBDAT
018000040820     C                   Z-ADD     G02INV        data_oggi
018100040820     C                   TIME                    W0140            14 0
018200040820     c*
018300180108     C* Caricamento Tabella codici nazioni
018400180108     C                   Z-ADD     0             X                 3 0
018500180108     C                   MOVEL     '15'          KCOD
018600180108     C                   Z-ADD     1             KKUT
018700180108     C     KTAB          CHAIN     TABEL00F                           30
018800180108     C     *IN30         DOWEQ     '0'
018900180108     C     TBLFLG        IFEQ      *BLANKS
019000180108     C                   ADD       1             X
019100180108     C                   MOVEL     TBLKEY        C15(X)
019200180108     C                   MOVEL     TBLUNI        DS15
019300180108     C                   MOVEL     �15COD        ISO(X)
019400180109     C                   MOVEL     �15DES        DES(X)
019500180108     C                   END
019600180108     C     KTAB          READE     TABEL00F                               30
019700180108     C                   END
019800180108     c*
019900040820     c* compongo istruzione sql
020000040820     c                   exsr      istruz
020100040820     c*
020200040820     C                   ENDSR
020300040820     C**************************************************************************
020400040820     C* mi compongo l'istruzione sql
020500040820     C**************************************************************************
020600040820     C     istruz        BEGSR                                                  *
020700040820      **
020800180108     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
020900180129     C                   EVAL      WrkSqlCmd =
021000180214     C                             'SELECT * FROM TITAS10F WHERE tasaas=2018  -
021100180214     C                              and  tasmgs between 0101 and 0131   and   -
021200180214     C                              TASLNA in (190, 191, 192, 193, 194, 195,  -
021300180214     c                               196, 197, 291, 292, 293, 294)      and   -
021400180214     c                               TASLNP not in (190, 191, 192, 193, 194,  -
021500180214     c                               195, 196, 197, 291, 292, 293, 294)'
021600040820      **
021700040820     C                   ENDSR
021800020326     C**************************************************************************
