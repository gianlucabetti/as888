000100000000     H DATEDIT(*YMD)
000200000000      **************************************************************************
000300950118      *  Nome programma:  FNLSC7R
000400000000      *  Descrizione   :  Pulizia archivi statistiche partenze (batch)
000500000000      *  Data creazione:  01 FEB 1994
000600000000      **************************************************************************
000700090911     FFiSBO01L  UF   E           K DISK    USROPN
000800950118     FFNSCE01L  UF   E           K DISK    USROPN
000900970314     F*
001000090915     Ffispa01L  UF   E           K DISK    USROPN
001100090916     Ffisat04L  UF   E           K DISK    USROPN
001200970318     FFNANT01L  UF   E           K DISK    USROPN
001300040927     FTABEL00F  IF   E           K DISK
001400000000      *
001500000000     D KPJBA         E DS
001600040927     D DS5AS1        E DS
001700040927     D WLBDAT          DS
001800040927     D  G02DAT                 1      8  0
001900040927     D  G02INV                 9     16  0
002000040927     D  G02ERR                17     17
002100040927     D  G02TGI                18     22  0
002200040927     D WGIDAT          DS
002300040927     D  GIODAT                 1      8  0
002400040927     D  GIOINV                 9     16  0
002500040927     D  GIOTGI                17     21  0
002600000000      *
002700000000      **************************************************************************
002800000000      *             M A I N      L I N E
002900000000      **************************************************************************
003000000000      *
003100040927      * CALCOLO DATA PULIZIA STATISTICHE PARTENZA
003200040927     C* Ricavo il nr. giorni dalla data odierna
003300040927     C                   TIME                    WTIME            14 0
003400040927     C                   MOVE      WTIME         WDATE             8 0
003500040927     C                   MOVE      WDATE         G02DAT
003600040927     C                   MOVEL     *BLANK        G02ERR
003700040927     C                   CALL      'XSRDA8'
003800040927     C                   PARM                    WLBDAT
003900040927     C                   Z-ADD     0             CO1DPT            8 0
004000040927     C     G02TGI        SUB       §5ASTS        GIOTGI
004100040927      *
004200040927     C                   CALL      'XSRGI8'
004300040927     C                   PARM                    WGIDAT
004400040928     C                   Z-ADD     GIOinv        CO1DPT
004500040927     c*
004600000000      *
004700000000      * Leggo sequenzialmente il file statistiche sul bollettato
004800090911     C                   OPEN      FiSBO01L
004900090911     C     *LOVAL        SETLL     FiSBO01L
005000000000     C                   DO        *HIVAL
005100000000      *
005200090911     C                   READ      FiSBO01L                               31
005300990507     C  N31SBODRE        IFLE      CO1DPT
005400000000      *
005500000000      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
005600090911     C                   DELETE    FiSBO000
005700000000      *
005800000000     C                   ENDIF
005900000000     C  N31              ENDDO
006000090911     C                   CLOSE     FiSBO01L
006100000000      *-------------------------------------------------------------------------
006200000000      *
006300000000      * Leggo sequenzialmente il file statistiche controllo entrate
006400950118     C                   OPEN      FNSCE01L
006500950118     C     *LOVAL        SETLL     FNSCE01L
006600000000     C                   DO        *HIVAL
006700000000      *
006800950118     C                   READ      FNSCE01L                               31
006900950118     C  N31SCEDRE        IFLE      CO1DPT
007000040927     C***  SCEFTR        ANDNE     ' '
007100000000      *
007200000000      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
007300950118     C                   DELETE    FNSCE000
007400000000      *
007500000000     C                   ENDIF
007600000000     C  N31              ENDDO
007700950118     C                   CLOSE     FNSCE01L
007800000000      *-------------------------------------------------------------------------
007900970317      * A TERRA
008000090916     C                   OPEN      fisat04L
008100090916     C     *LOVAL        SETLL     fisat04L
008200090916     C                   READ      fisat04L                               31
008300970317    1C     *IN31         DOWEQ     *OFF
008400970317      * Se la data e' inferiore (o uguale) a quella di pulizia cancello record
008500970317    2C     SATDRE        IFLE      CO1DPT
008600090916     C                   DELETE    fisat000
008700970317   X2C                   ELSE
008800090916     C     KSAT          SETGT     fisat04L
008900970317    2C                   END
009000090916     C                   READ      fisat04L                               31
009100970317    1C                   ENDDO
009200090916     C                   CLOSE     fisat04L
009300970317     C*
009400970317     C* PARTITO
009500090915     C                   OPEN      fispa01L
009600090915     C     *LOVAL        SETLL     fispa01L
009700090915     C                   READ      fispa01L                               31
009800970317    1C     *IN31         DOWEQ     *OFF
009900090915     c
010000970317    2C     SPADRE        IFLE      CO1DPT
010100090915     C                   DELETE    fispa000
010200970317   X2C                   ELSE
010300090915     C     KSPA          SETGT     fispa01L
010400970317    2C                   END
010500090915     c
010600090915     C                   READ      fispa01L                               31
010700970317    1C                   ENDDO
010800090915     C                   CLOSE     fispa01L
010900970318     C* TOTALI ANOMALIE
011000970318     C                   OPEN      FNANT01L
011100970318     C     *LOVAL        SETLL     FNANT01L
011200970318     C                   READ      FNANT01L                               31
011300970318    1C     *IN31         DOWEQ     *OFF
011400970318    2C     ANTDAO        IFLE      CO1DPT
011500040927     C***  ANTFT1        IFNE      *BLANKS
011600040927     C***  ANTFT2        ANDNE     *BLANKS
011700970318     C                   DELETE    FNANT000
011800040927     C***                END
011900970318   X2C                   ELSE
012000970318     C     ANTLNP        SETGT     FNANT01L
012100970318    2C                   END
012200970318     C                   READ      FNANT01L                               31
012300970318    1C                   ENDDO
012400970318     C                   CLOSE     FNANT01L
012500970314     C*
012600000000     C                   SETON                                        LR
012700970314     C****************  ROUTINE INIZIALE  *****************************
012800970314     C     *INZSR        BEGSR
012900970314     C*
013000970314     C     *ENTRY        PLIST
013100970314     C                   PARM                    KPJBA
013200040927     C****               MOVEL     KPJBU         PARAM
013300040927     C                   Z-ADD     1             CODUT
013400970314     C*
013500970314     C* IMPOSTO CAMPI CHIAVE
013600970317     C     KSAT          KLIST
013700970317     C                   KFLD                    SATFLE
013800970317     C                   KFLD                    SATLNP
013900970317     C*
014000970317     C     KSPA          KLIST
014100970317     C                   KFLD                    SPAFLE
014200970317     C                   KFLD                    SPALNP
014300040927     C* TABEL00F
014400040927     C     KTAB          KLIST
014500040927     C                   KFLD                    CODUT             1 0
014600040927     C                   KFLD                    COD
014700040927     C                   KFLD                    KEY
014800040927     c*
014900040927     C     *LIKE         DEFINE    TBLCOD        COD
015000040927     C     *LIKE         DEFINE    TBLKEY        KEY
015100040927     C* LETTURA GIORNI DI PULIZIA
015200040927     C                   MOVEL     '5A'          COD
015300040927     C                   MOVEL     'SEDE1   '    KEY
015400040927     C     KTAB          CHAIN     TABEL00F                           30
015500040927     C  N30              MOVEL     TBLUNI        DS5AS1
015600040927     C   30              CLEAR                   DS5AS1
015700970314     C*
015800970314     C                   ENDSR
