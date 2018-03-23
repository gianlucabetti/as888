000100000000     H DATEDIT(*YMD)
000200000000      **************************************************************************
000300000000      *
000400941230      *  Nome programma:  FNLRE4R
000500000000      *  Descrizione   :  Pulizia archivi statistiche arrivi
000600941230      *  Data creazione:  01 FEB 1994  - 30/12/94
000700000000      *
000800000000      **************************************************************************
000900040924      *  24/09/04: Non si riceve più la data di pulizia ma viene calco-
001000040924      *  lata direttamenta da questo pgm
001100000000      **************************************************************************
001200090929     FFISAR01L  UF   E           K DISK    USROPN
001300970314     FFNSCO01L  UF   E           K DISK    USROPN
001400040924     FTABEL00F  IF   E           K DISK
001500970314      *
001600040924     d §KUT            s                   like(TBLKUT) inz(1)
001700040924     d §cod            s                   like(TBLcod)
001800040924     d §Key            s                   like(TBLKey)
001900040924
002000000000     D KPJBA         E DS
002100040924     D DS5AS1        E DS
002200040924     D WLBDAT          DS
002300040924     D  G02DAT                 1      8  0
002400040924     D  G02INV                 9     16  0
002500040924     D  G02ERR                17     17
002600040924     D  G02TGI                18     22  0
002700040924     D WGIDAT          DS
002800040924     D  GIODAT                 1      8  0
002900040924     D  GIOINV                 9     16  0
003000040924     D  GIOTGI                17     21  0
003100000000      *
003200090929      ***********************************************************************
003300000000     C     *ENTRY        PLIST
003400000000     C                   PARM                    KPJBA
003500970314     C* IMPOSTO CAMPI CHIAVE
003600970314     C     KSCO          KLIST
003700970317     C                   KFLD                    SCOFLE
003800970317     C                   KFLD                    SCOLNA
003900040924     C* TABEL00F
004000040924     C     KTAB          KLIST
004100040924     C                   KFLD                    §KUT
004200040924     C                   KFLD                    §COD
004300040924     C                   KFLD                    §KEY
004400040924     C*
004500040924     C     *LIKE         DEFINE    GIOINV        COMDPC
004600040924     c*
004700040924     C                   TIME                    W0140            14 0
004800040924     C                   MOVE      W0140         UDATE8            8 0
004900040924     C* CON XSRDA8 TRASFORMO LA DATA ODIERNA IN NR.GIORNI (G02TGI)
005000040924     C                   MOVE      UDATE8        G02DAT
005100040924     C                   MOVEL     *BLANK        G02ERR
005200040924     C                   CALL      'XSRDA8'
005300040924     C                   PARM                    WLBDAT
005400040924     c* calcolo data di pulizia
005500040924     C                   MOVEL     '5A'          §COD
005600040924     C                   MOVEL(P)  'SEDE1'       §KEY
005700040924     C     KTAB          CHAIN     TABEL00F                           30
005800040924     C  N30              MOVEL     TBLUNI        DS5AS1
005900040924     C   30              MOVEL     *ZEROS        DS5AS1
006000040924     C     G02TGI        SUB       §5ASTS        GIOTGI
006100040924     C*
006200040924     C                   CALL      'XSRGI8'
006300040924     C                   PARM                    WGIDAT
006400040924     C                   Z-ADD     GIOINV        COMDPC
006500970317     C*
006600000000      **************************************************************************
006700000000      *             M A I N      L I N E
006800000000      **************************************************************************
006900000000      *
007000000000      * Leggo sequenzialmente il file statistiche
007100090929     C                   OPEN      FiSAR01L
007200090929     C     *LOVAL        SETLL     FiSAR01L
007300000000     C                   DO        *HIVAL
007400000000      *
007500090929     C                   READ      FiSAR01L                               31
007600990507     C  N31SARDRE        IFLE      COMDPC
007700000000      *
007800000000      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
007900090929     C                   DELETE    FiSAR000
008000000000      *
008100000000     C                   ENDIF
008200000000     C  N31              ENDDO
008300090929     C                   CLOSE     FiSAR01L
008400970314     C*
008500970314     C* PULIZIA ARCHIVIO STATISTICA CONSEGNE
008600970314     C                   OPEN      FNSCO01L
008700970317     C     *LOVAL        SETLL     FNSCO01L
008800970317     C                   READ      FNSCO01L                               31
008900970314     C     *IN31         DOWEQ     *OFF
009000970317     C     SCODCM        IFLE      COMDPC
009100970314     C                   DELETE    FNSCO000
009200970317     C                   ELSE
009300970317     C     KSCO          SETGT     FNSCO01L
009400970317     C                   END
009500970317     C                   READ      FNSCO01L                               31
009600970314     C                   ENDDO
009700970314     C                   CLOSE     FNSCO01L
009800000000      *
009900000000     C                   SETON                                        LR
