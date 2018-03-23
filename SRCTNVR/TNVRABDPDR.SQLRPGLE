000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200040820     H**********************************************************************
000300180108     FTABEL00F  IF   E           K DISK
000400180109     Fwfdpcap0f o  a E             DISK
000500180108     H**********************************************************************
000600180108      * Codici ISO nazioni x E.D.I.
000700180108     D C15             S              3    DIM(1000)
000800180108     D ISO             S              2    DIM(1000)
000900180109     D DES             S             25    DIM(1000)
001000180108     D DS15          E DS
001100040820     D*
001200180109     D DPDCAPTST     e DS
001300180108     D tisid4ds      e DS
001400040820     D*
001500040820     D WLBDAT          DS
001600040820     D  G02DAT                 1      8  0
001700040820     D  G02INV                 9     16  0
001800040820     D  G02ERR                17     17
001900040820     D  G02TGI                18     22  0
002000040820      *
002100040820     D WrkSqlCmd       S           1024
002200040820     D  data_Oggi      s                   like(G02INV) INZ(0)
002300040820      *
002400040820     D digits          C                   CONST('0123456789')
002500040820      *****************************************************************
002600180108     C/EXEC SQL
002700180109     C+ DELETE FROM WFDPCAP0F
002800180108     C/END-EXEC
002900180108      *****************************************************************
003000011210     C/EXEC SQL
003100011210     C+ PREPARE S1 FROM :WrkSqlCmd
003200011210     C/END-EXEC
003300011210
003400011210     C/EXEC SQL
003500011210     C+ DECLARE A1 CURSOR FOR S1
003600011210     C/END-EXEC
003700011210
003800011210     C/EXEC SQL
003900011210     C+ OPEN A1
004000011210     C/END-EXEC
004100040820
004200040820     C                   DOU       SqlCod <> 0
004300040823     c*
004400011205     C/EXEC SQL
004500180109     C+ FETCH NEXT FROM A1 INTO :DPDCAPTST
004600011205     C/END-EXEC
004700011210     C                   SELECT
004800041007     c*
004900041007     c* a fine file Totali x rotture
005000041007     C                   WHEN      SqlCod = 100
005100041007     c                   leave
005200041007     **
005300041007     C                   WHEN      SqlCod < 0
005400041007     C                   seton                                        H1
005500041007     c                   goto      fine
005600040820     c*
005700041007     C                   OTHER
005800040820     c* DETTAGLIO
005900180108     c                   clear                   tisid4ds
006000180109     c                   clear                   desNAZ
006100180108     c*
006200180108     C                   Z-ADD     20180201      ISID4DRI
006300180108     C                   Z-ADD     001           ISID4LNP
006400180108     C                   Z-ADD     1             YY                3 0
006500180108     C     DCOUNTRY      LOOKUP    ISO(YY)                                32
006600180111     C  n32              movel     Dcountry      ISID4NZD
006700180108     C   32              movel     C15(YY)       ISID4NZD
006800180109     C   32              movel     DES(YY)       desNAZ           25
006900180108     C                   movel     POSTALCODE    ISID4CAD
007000180108     C                   Z-ADD     SOCODE        ISID4SORC
007100180108      *
007200180108     c                   call      'TISID4R'
007300180108     C                   PARM                    TISID4DS
007400180108      *
007500180108     C                   clear                   WFDPD0
007600180108     c                   z-add     NAME          INPNAME
007700180108     c                   movel     ODEPOT        INPDEPOT
007800180108     c                   z-add     SOCODE        INPSOCODE
007900180108     c                   movel     POSTALCODE    INPPOSTAL
008000180108     c                   movel     DCOUNTRY      INPDCOUN
008100180109     c                   movel     desNAZ        INPDESCC
008200180109     c                   movel     FAILURECD     INPFAILUR
008300180109     c                   movel     DDEPOTCC      INPDEPCC
008400180109     c                   movel     SSORT         INPSSORT
008500180109     c                   movel     OSORT         INPOSORT
008600180109     c                   movel     CSORT         INPCSORT
008700180109     c                   movel     DSORT         INPDSORT
008800180108     c                   movel     DDEPOT        CMPDEPOT
008900180108     c                   movel     DEPOTSTR      CMPDEPSTR
009000180108     c                   movel     OSID4DDPT     OUTDEP7
009100180108     c                   movel     OSID4DSTR     OUTDEP4
009200180108     c                   movel     OSID4ERR      OUTERR
009300180108     c                   movel     OSID4ERRL     OUTERRL
009400180108     c                   movel     OSID4ERRD     OUTERRD
009500180109     c                   movel     OSID4SSORT    OUTSSORT
009600180109     c                   movel     OSID4OSORT    OUTOSORT
009700180109     c                   movel     OSID4CSORT    OUTCSORT
009800180109     c                   movel     OSID4DSORT    OUTDSORT
009900180108     c                   if        CMPDEPOT  = OUTDEP7 and
010000180110     c                             CMPDEPSTR = OUTDEP4 and
010100180110     c                             OUTDEP7 <> ' ' and
010200180110     c                             OUTERR  = *blank
010300180108     c                   movel     'OK'          OUTESITO
010400180108     c                   end
010500180108      *
010600180108     C                   write     WFDPD0
010700180108      *
010800011210     C                   ENDSL
010900040820
011000011205     C                   ENDDO
011100011205     C/EXEC SQL
011200011205     C+ CLOSE A1
011300011205     C/END-EXEC
011400040823      *
011500041007     c     fine          tag
011600020326     C                   SETON                                        LR
011700040820     C**************************************************************************
011800040820     C* INIZIALIZZAZIONE
011900040820     C**************************************************************************
012000040820     C     *inzsr        BEGSR
012100180108      **
012200180108     C     KTAB          KLIST
012300180108     C                   KFLD                    KKUT
012400180108     C                   KFLD                    KCOD
012500180108      **
012600180108     C     *LIKE         DEFINE    TBLKUT        KKUT
012700180108     C     *LIKE         DEFINE    TBLCOD        KCOD
012800040820      **
012900040820      *  imposta UDATE di elaborazione
013000040820     C                   move      *date         G02DAT
013100040820     C                   MOVE      *ZEROS        G02INV
013200040820     C                   MOVE      *BLANKS       G02ERR
013300040820     C                   CALL      'XSRDA8'
013400040820     C                   PARM                    WLBDAT
013500040820     C                   Z-ADD     G02INV        data_oggi
013600040820     C                   TIME                    W0140            14 0
013700040820     c*
013800180108     C* Caricamento Tabella codici nazioni
013900180108     C                   Z-ADD     0             X                 3 0
014000180108     C                   MOVEL     '15'          KCOD
014100180108     C                   Z-ADD     1             KKUT
014200180108     C     KTAB          CHAIN     TABEL00F                           30
014300180108     C     *IN30         DOWEQ     '0'
014400180108     C     TBLFLG        IFEQ      *BLANKS
014500180108     C                   ADD       1             X
014600180108     C                   MOVEL     TBLKEY        C15(X)
014700180108     C                   MOVEL     TBLUNI        DS15
014800180108     C                   MOVEL     §15COD        ISO(X)
014900180109     C                   MOVEL     §15DES        DES(X)
015000180108     C                   END
015100180108     C     KTAB          READE     TABEL00F                               30
015200180108     C                   END
015300180108     c*
015400040820     c* compongo istruzione sql
015500040820     c                   exsr      istruz
015600040820     c*
015700040820     C                   ENDSR
015800040820     C**************************************************************************
015900040820     C* mi compongo l'istruzione sql
016000040820     C**************************************************************************
016100040820     C     istruz        BEGSR                                                  *
016200040820      **
016300180108     c* MI COMPONGO L'ISTRUZIONE SQL che legge il file
016400040820     C                   EVAL      WrkSqlCmd
016500180110     C                             =  'SELECT * FROM DPDCAPTST'
016600180110     c*******************            ORDER BY DCOUNTRY, SOCODE, POSTALCODE'
016700040820      **
016800040820     C                   ENDSR
016900020326     C**************************************************************************
