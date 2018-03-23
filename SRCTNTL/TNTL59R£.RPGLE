000100940912     H DECEDIT('0,') DATEDIT(*DMY.)
000200960913     H* TNTL59R  *----------------------------------------------------*
000300960913     H*                 PULIZIA FV PARTENZA    - BATCH -              **
000400951110     H*---------------------------------------------------------------*
000500960916     FTNFVS03L  UF   E           K DISK
000600990415     FTNFVS01L  IF   E           K DISK
000700990415     F                                     RENAME(TNFVS000:TNFVS001)
000800960916     FFNFVD01L  UF   E           K DISK
000900960916     FFNFV201L  UF   E           K DISK
001000960916     FFNFV401L  UF   E           K DISK
001100960916     FTNTSR04L  IF   E           K DISK
001200960919     FTABEL00F  IF   E           K DISK
001300951110     D*
001400960912     D* DS PER TNTL57R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
001500960912     D DSTL57        E DS                  EXTNAME(TNTL57DS)
001600960229     D*
001700960919     D WLBDAT          DS
001800960919     D  G02DAT                 1      8  0
001900960919     D  G02INV                 9     16  0
002000960919     D  G02ERR                17     17
002100960919     D  G02TGI                18     22  0
002200960919     D WGIDAT          DS
002300960919     D  GIODAT                 1      8  0
002400960919     D  GIOINV                 9     16  0
002500960919     D  GIOTGI                17     21  0
002600960919     D*
002700970718     D DS5A2         E DS
002800951110     D KPJBA         E DS
002900951110     I/SPACE 3
003000951110     C*****************************************************************
003100951110     C* RIEPILOGO INDICATORI
003200951110     C*****************************************************************
003300960916     C* 30/33 - DI COMODO
003400951110     C*****************************************************************
003500951110     C     *ENTRY        PLIST
003600951110     C                   PARM                    KPJBA
003700960912     C                   MOVEL     KPJBU         DSTL57
003800951113     C*
003900951110     C*---------------------------------------------------------------*
004000960919     C* SE RICEVO LA DATA ELABORAZIONE A 0 --> LA RICALCOLO
004100960919    1C     D57DEA        IFEQ      0
004200970718     C* DATA ELABORAZIONE AL: REPERISCO GIORNI DA DS5A
004300970718     C                   MOVEL     '5A'          COD
004400970718     C                   MOVEL(P)  '2'           KEY
004500960919     C     KTAB          CHAIN     TABEL                              30
004600960919     C     *IN30         IFEQ      *OFF
004700960919     C     TBLFLG        ANDEQ     ' '
004800970718     C                   MOVEL     TBLUNI        DS5A2
004900960919     C                   ELSE
005000970718     C                   CLEAR                   DS5A2
005100960919     C                   ENDIF
005200960919     C*
005300960919     C* PRENDO LA DATA DEL GIORNO DA TIME
005400960919     C                   TIME                    W0140            14 0
005500960919     C                   MOVE      W0140         UDATE8            8 0
005600960919     C*
005700960919     C                   MOVE      UDATE8        G02DAT
005800960919     C                   MOVEL     *BLANK        G02ERR
005900960919     C                   CALL      'XSRDA8'
006000960919     C                   PARM                    WLBDAT
006100960919     C*
006200970718     C* SOTTRAGGO §5AFVS AI GIORNI
006300970718     C     G02TGI        SUB       §5AFVS        GIOTGI
006400960919     C*
006500960919     C                   CALL      'XSRGI8'
006600960919     C                   PARM                    WGIDAT
006700960919     C                   Z-ADD     GIOINV        D57DEA
006800960919    1C                   ENDIF
006900960919     C*
007000990415     C                   READ      TNFVS01L                               30
007100960919     C*****
007200960919     C** E L A B O R A Z I O N E
007300960919     C*****
007400960916     C                   CLEAR                   WLNP
007500960916     C                   SETOFF                                       30
007600960916     C*
007700960916    1C     *IN30         DOWEQ     *OFF
007800960916     C     WLNP          SETGT     TNFVS03L
007900960916     C                   READ      TNFVS03L                               30
008000960916     C*
008100960916    2C     *IN30         IFEQ      *OFF
008200960916     C                   Z-ADD     FVSLNP        WLNP
008300960916     C*
008400960916     C     WLNP          SETLL     TNFVS03L
008500960916     C     WLNP          READE     TNFVS03L                               31
008600960916     C*
008700960916    3C     *IN31         DOWEQ     *OFF
008800960916     C*
008900960916     C* SE DATA FOGLIO VIAGGIO > DATA PULIZIA LEGGO ALTRA LINEA
009000960916    4C     FVSDFV        IFGT      D57DEA
009100960916     C                   SETON                                        31
009200960916   X4C                   ELSE
009300960916     C*
009400960916     C* VERIFICO CHE IL FOGLIO SIA STATO CONTROLLATO PER L'ABBINAMENTO
009500960916    5C     FVSABN        IFEQ      'S'
009600960916     C*
009700960916     C* SE FV ANNULLATO ---> DELETO SENZA CONTROLLARE IL TRAINO
009800960916    6C     FVSATB        IFNE      ' '
009900960918     C                   EXSR      PULIZ
010000960916     C*
010100960916   X6C                   ELSE
010200960916     C* SE RIESCO VERIFICO L'ESISTENZA DEL TRAINO ALTRIMENTI CANCELLO
010300960916     C*   IL FOGLIO
010400960916    7C     FVSTRS        IFEQ      0
010500960916     C     FVSTRS        OREQ      9999998
010600960916     C     FVSTRS        OREQ      9999999
010700960918     C                   EXSR      PULIZ
010800960916     C*
010900960916   X7C                   ELSE
011000960916     C* CONTROLLO L'ESISTENZA DEL TRAINO
011100960916     C     KFVS          SETLL     TNTSR04L                               32
011200960916     C  N32              EXSR      PULIZ
011300960916    7C                   ENDIF
011400960916    6C                   ENDIF
011500960916    5C                   ENDIF
011600960916     C*
011700960916     C*
011800960916     C     WLNP          READE     TNFVS03L                               31
011900960916    4C                   ENDIF
012000960916    3C                   ENDDO
012100960916     C*
012200960916    2C                   ENDIF
012300960916    1C                   ENDDO
012400990415     C*
012500990415     C* LEGGO DI FNFVD00F TUTTO CIO' CHE NON HA PIU' IL FOGLIO VIAGG
012600990415     C     *LOVAL        SETLL     FNFVD01L
012700990415     C                   READ      FNFVD01L                               30
012800990415     C     *IN30         DOWEQ     *OFF
012900990415     C     KFVD          SETLL     TNFVS01L                               35
013000990415     C     *IN35         IFEQ      *OFF
013100990415     C                   DELETE    FNFVD000
013200990415     C                   ENDIF
013300990415     C*
013400990415     C                   READ      FNFVD01L                               30
013500990415     C                   ENDDO
013600990415     C*
013700990415     C* LEGGO DI FNFV400F TUTTO CIO' CHE NON HA PIU' IL FOGLIO VIAGG
013800990415     C     *LOVAL        SETLL     FNFV401L
013900990415     C                   READ      FNFV401L                               30
014000990415     C     *IN30         DOWEQ     *OFF
014100990415     C     KFV4          SETLL     TNFVS01L                               35
014200990415     C     *IN35         IFEQ      *OFF
014300990415     C                   DELETE    FNFV4000
014400990415     C                   ENDIF
014500990415     C*
014600990415     C                   READ      FNFV401L                               30
014700990415     C                   ENDDO
014800990415     C*
014900990415     C* LEGGO DI FNFV400F TUTTO CIO' CHE NON HA PIU' IL FOGLIO VIAGG
015000990415     C     *LOVAL        SETLL     FNFV201L
015100990415     C                   READ      FNFV201L                               30
015200990415     C     *IN30         DOWEQ     *OFF
015300990415     C     KFV2          SETLL     TNFVS01L                               35
015400990415     C     *IN35         IFEQ      *OFF
015500990415     C                   DELETE    FNFV2000
015600990415     C                   ENDIF
015700990415     C*
015800990415     C                   READ      FNFV201L                               30
015900990415     C                   ENDDO
016000960916     C*
016100960916     C                   SETON                                        LR
016200960912     C*
016300960912     C*--- ROUTINE DI PULIZIA ----------------------------------------*
016400960912     C     PULIZ         BEGSR
016500960912     C*
016600960916     C****  FNFV400F ---> ESTENSIONE DESCR. FV PARTENZA  ****
016700960916     C                   SETOFF                                       33
016800960916     C     KFVS          SETLL     FNFV401L
016900960916    1C     *IN33         DOWEQ     *OFF
017000960916     C     KFVS          READE     FNFV401L                               33
017100960916     C  N33              DELETE    FNFV4000
017200960916    1C                   ENDDO
017300960916     C*
017400960916     C****  FNFV200F ---> DATA/ORA PART/ARR FV PARTENZA  ****
017500960916     C                   SETOFF                                       33
017600960916     C     KFVS          SETLL     FNFV201L
017700960916    1C     *IN33         DOWEQ     *OFF
017800960916     C     KFVS          READE     FNFV201L                               33
017900960916     C  N33              DELETE    FNFV2000
018000960916    1C                   ENDDO
018100960916     C*
018200960916     C****  FNFVD00F ---> DETTAGLIO TOTALI  FV PARTENZA  ****
018300960916     C                   SETOFF                                       33
018400960916     C     KFVS          SETLL     FNFVD01L
018500960916    1C     *IN33         DOWEQ     *OFF
018600960916     C     KFVS          READE     FNFVD01L                               33
018700960916     C  N33              DELETE    FNFVD000
018800960916    1C                   ENDDO
018900960916     C*
019000960916     C****  TNFVS00F ---> FV PARTENZA  ****
019100960916     C                   DELETE    TNFVS000
019200960912     C*
019300960912     C                   ENDSR
019400960912     C*
019500960226     C*--- SR INIZIALE -----------------------------------------------*
019600951110     C     *INZSR        BEGSR
019700960912     C****
019800960912     C** KLIST
019900960912     C****
020000960916     C     KFVS          KLIST
020100960916     C                   KFLD                    FVSLNP
020200960916     C                   KFLD                    FVSNFV
020300990415     C     KFVD          KLIST
020400990415     C                   KFLD                    FVDLNP
020500990415     C                   KFLD                    FVDNFV
020600990415     C     KFV4          KLIST
020700990415     C                   KFLD                    FV4LNP
020800990415     C                   KFLD                    FV4NFV
020900990415     C     KFV2          KLIST
021000990415     C                   KFLD                    FV2LNP
021100990415     C                   KFLD                    FV2NFV
021200960919     C* ACCESSO TABEL00F
021300960919     C     KTAB          KLIST
021400960919     C                   KFLD                    CODUT
021500960919     C                   KFLD                    COD
021600960919     C                   KFLD                    KEY
021700960916     C****
021800960916     C** DEFINIZIONE CAMPI
021900960916     C****
022000960916     C     *LIKE         DEFINE    FVSLNP        WLNP
022100960919     C     *LIKE         DEFINE    TBLCOD        COD
022200960919     C     *LIKE         DEFINE    TBLKEY        KEY
022300960912     C*
022400960919     C* IMPOSTAZIONE CAMPI FISSI
022500960919     C                   Z-ADD     1             CODUT             1 0
022600960229     C                   ENDSR
