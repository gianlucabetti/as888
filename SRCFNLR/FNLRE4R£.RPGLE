000100000000     H DATEDIT(*YMD)
000200000000      **************************************************************************
000300000000      *
000400941230      *  Nome programma:  FNLRE4R
000500000000      *  Descrizione   :  Pulizia archivi statistiche arrivi
000600000000      *  Autore        :  STEFANO (SC)
000700941230      *  Data creazione:  01 FEB 1994  - 30/12/94
000800000000      *
000900000000      **************************************************************************
001000000000      *  Come parametro di ingresso vi e' solo la data limite per la pulizia
001100940628      *  dell'archivio; tale data viene impostata dal pgm di lancio FLFR82R
001200040924      *  24/09/04: Non si riceve più la data di pulizia ma viene calco-
001300040924      *  lata direttamenta da questo pgm
001400000000      **************************************************************************
001500990507     FFNSAR01L  UF   E           K DISK    USROPN
001600970314     FFNSCO01L  UF   E           K DISK    USROPN
001700040924     FTABEL00F  IF   E           K DISK
001800970314      *
001900040924     d §KUT            s                   like(TBLKUT) inz(1)
002000040924     d §cod            s                   like(TBLcod)
002100040924     d §Key            s                   like(TBLKey)
002200040924
002300000000     D KPJBA         E DS
002400040924     D DS5AS1        E DS
002500040924     D WLBDAT          DS
002600040924     D  G02DAT                 1      8  0
002700040924     D  G02INV                 9     16  0
002800040924     D  G02ERR                17     17
002900040924     D  G02TGI                18     22  0
003000040924     D WGIDAT          DS
003100040924     D  GIODAT                 1      8  0
003200040924     D  GIOINV                 9     16  0
003300040924     D  GIOTGI                17     21  0
003400040924     I***PARAM       DS
003500040924     I***                                    60  670COMDPC
003600000000      *
003700000000     C     *ENTRY        PLIST
003800000000     C                   PARM                    KPJBA
003900040924     C***                  MOVELKPJBU     PARAM
004000970314     C* IMPOSTO CAMPI CHIAVE
004100970314     C     KSCO          KLIST
004200970317     C                   KFLD                    SCOFLE
004300970317     C                   KFLD                    SCOLNA
004400040924     C* TABEL00F
004500040924     C     KTAB          KLIST
004600040924     C                   KFLD                    §KUT
004700040924     C                   KFLD                    §COD
004800040924     C                   KFLD                    §KEY
004900040924     C*
005000040924     C     *LIKE         DEFINE    GIOINV        COMDPC
005100040924     c*
005200040924     C                   TIME                    W0140            14 0
005300040924     C                   MOVE      W0140         UDATE8            8 0
005400040924     C* CON XSRDA8 TRASFORMO LA DATA ODIERNA IN NR.GIORNI (G02TGI)
005500040924     C                   MOVE      UDATE8        G02DAT
005600040924     C                   MOVEL     *BLANK        G02ERR
005700040924     C                   CALL      'XSRDA8'
005800040924     C                   PARM                    WLBDAT
005900040924     c* calcolo data di pulizia
006000040924     C                   MOVEL     '5A'          §COD
006100040924     C                   MOVEL(P)  'SEDE1'       §KEY
006200040924     C     KTAB          CHAIN     TABEL00F                           30
006300040924     C  N30              MOVEL     TBLUNI        DS5AS1
006400040924     C   30              MOVEL     *ZEROS        DS5AS1
006500040924     C     G02TGI        SUB       §5ASTS        GIOTGI
006600040924     C*
006700040924     C                   CALL      'XSRGI8'
006800040924     C                   PARM                    WGIDAT
006900040924     C                   Z-ADD     GIOINV        COMDPC
007000970317     C*
007100000000      **************************************************************************
007200000000      *             M A I N      L I N E
007300000000      **************************************************************************
007400000000      *
007500000000      * Leggo sequenzialmente il file statistiche
007600990507     C                   OPEN      FNSAR01L
007700990507     C     *LOVAL        SETLL     FNSAR01L
007800000000     C                   DO        *HIVAL
007900000000      *
008000990507     C                   READ      FNSAR01L                               31
008100990507     C  N31SARDRE        IFLE      COMDPC
008200000000      *
008300000000      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
008400000000      * Ma solo se il record e' gia' stato trasmesso
008500040924     C****       SARFTR    IFNE ' '
008600990507     C                   DELETE    FNSAR000
008700040924     C****                 ENDIF
008800000000      *
008900000000     C                   ENDIF
009000000000     C  N31              ENDDO
009100990507     C                   CLOSE     FNSAR01L
009200970314     C*
009300970314     C* PULIZIA ARCHIVIO STATISTICA CONSEGNE
009400970314     C                   OPEN      FNSCO01L
009500970317     C     *LOVAL        SETLL     FNSCO01L
009600970317     C                   READ      FNSCO01L                               31
009700970314     C     *IN31         DOWEQ     *OFF
009800970317     C     SCODCM        IFLE      COMDPC
009900040924     C****       SCOFTR    IFNE ' '
010000970314     C                   DELETE    FNSCO000
010100040924     C****                 END
010200970317     C                   ELSE
010300970317     C     KSCO          SETGT     FNSCO01L
010400970317     C                   END
010500970317     C                   READ      FNSCO01L                               31
010600970314     C                   ENDDO
010700970314     C                   CLOSE     FNSCO01L
010800000000      *
010900000000     C                   SETON                                        LR
