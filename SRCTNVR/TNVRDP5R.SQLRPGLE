000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200040820     H**********************************************************************
000300180108     FTABEL00F  IF   E           K DISK
000400180129     FfiPND01L  IF   E           K DISK
000500180222     FWFDPCNF1F o  a E             DISK
000600180108     H**********************************************************************
000700180108      * Codici ISO nazioni x E.D.I.
000800180108     D C15             S              3    DIM(1000)
000900180108     D ISO             S              2    DIM(1000)
001000180109     D DES             S             25    DIM(1000)
001100180108     D DS15          E DS
001200040820     D*
001300180129     D tas00F        e DS                  extname(titas00F)
001500180129     D tisie3ds      e DS
001600040820     D*
001700040820     D WLBDAT          DS
001800040820     D  G02DAT                 1      8  0
001900040820     D  G02INV                 9     16  0
002000040820     D  G02ERR                17     17
002100040820     D  G02TGI                18     22  0
002200040820      *
002300180129     D DATArif         S              8  0 inz(20180201)
002301180222     D DATAfut         S              8  0 inz(20180401)
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
003400180222     C+ DELETE FROM WFDPCNF1F
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
006601180222     c                   clear                   WFDPD1CNF
006700180214      *
006800180129     c     kPND          chain     fiPND01L
006900180129     c                   z-add     101           servcode          3 0
007000180129     c                   if        %Found(fiPND01L)
007100180129     c                   move      pndISC        servcode
007200180129     c                   end
007300180215     c*
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
010901180222     c                   movel     OSIE3DDEP     OUTDEP4A
010902180222     c                   movel     OSIE3DDEP     OUTDEP7A
010903180222      *
010904180222     C                   Z-ADD     DATAfut       ISIE3DRI
010905180222     C                   Z-ADD     DATAfut       ISIE3DSP
010906180222      *
010907180222     c                   call      'TISIE3R'
010908180222     C                   PARM                    TISIE3DS
010909180222      *
010910180222     c                   movel     OSIE3DDEP     OUTDEP4B
010911180222     c                   movel     OSIE3DDEP     OUTDEP7B
010912180222      *
011100180215      *
011500180129      *   vecchio cappario instradamento
011600180129      *     solo se diverso il depot trovato fra le 2 versioni
011700180222     c                   if        OUTDEP4A <> OUTDEP4A
011800180129     c                   z-add     ServCode      INPSOCODE
011900180129     c                   movel     tasCAD        INPPOSTAL
012000180129     c                   movel     tasNZD        INPDCOUN
012001180222     C                   Z-ADD     1             YY                3 0
012002180222     c                   clear                   desNAZ
012003180222     C     tasNZD        LOOKUP    C15(YY)                                32
012004180222     C  n32              movel     tasNZD        ISIe3NZD
012005180222     C   32              movel     C15(YY)       ISIe3NZD
012006180222     C   32              movel     DES(YY)       desNAZ           25
012201180222     c                   movel     desNAZ        INPDESCC
012400180222     c                   movel     OSIE3ERR      OUTERR
012500180222     c                   movel     OSIE3ERRL     OUTERRL
012600180222     c                   movel     OSIE3ERRD     OUTERRD
013100180129     c                   z-add     tasAAS        INPAAS
013200180129     c                   z-add     tasLNP        INPLNP
013300180129     c                   z-add     tasNRS        INPNRS
013400180129     c                   z-add     tasNSP        INPNSP
013500180130     c                   movel     tasTBL        INPTBL
013600180130     c                   movel     tasMGS        INPMGS
014200180222     C                   write     WFDPD1CNF
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
019300180108     C                   MOVEL     §15COD        ISO(X)
019400180109     C                   MOVEL     §15DES        DES(X)
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
021000180222     C                             'SELECT * FROM TITAS10F WHERE tasaas=2017  -
021100180214     C                              and  tasmgs between 0101 and 0131   and   -
021200180214     C                              TASLNA in (190, 191, 192, 193, 194, 195,  -
021300180214     c                               196, 197, 291, 292, 293, 294)      and   -
021400180214     c                               TASLNP not in (190, 191, 192, 193, 194,  -
021500180214     c                               195, 196, 197, 291, 292, 293, 294)'
021600040820      **
021700040820     C                   ENDSR
021800020326     C**************************************************************************
