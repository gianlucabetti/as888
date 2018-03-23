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
001100130211     FTNfgs01L  UF   E           K DISK
001200130211     FTNFWS01L  UF   E           K DISK
001300960916     FTNTSR04L  IF   E           K DISK
001400960919     FTABEL00F  IF   E           K DISK
001500951110     D*
001600960912     D* DS PER TNTL57R - DS DI PASSAGGIO PARAMETRI PER PGM CHIAMANTI
001700960912     D DSTL57        E DS                  EXTNAME(TNTL57DS)
001800960229     D*
001900960919     D WLBDAT          DS
002000960919     D  G02DAT                 1      8  0
002100960919     D  G02INV                 9     16  0
002200960919     D  G02ERR                17     17
002300960919     D  G02TGI                18     22  0
002400960919     D WGIDAT          DS
002500960919     D  GIODAT                 1      8  0
002600960919     D  GIOINV                 9     16  0
002700960919     D  GIOTGI                17     21  0
002800960919     D*
002900970718     D DS5A2         E DS
003000951110     D KPJBA         E DS
003100951110     I/SPACE 3
003200951110     C*****************************************************************
003300951110     C* RIEPILOGO INDICATORI
003400951110     C*****************************************************************
003500960916     C* 30/33 - DI COMODO
003600951110     C*****************************************************************
003700951110     C     *ENTRY        PLIST
003800951110     C                   PARM                    KPJBA
003900960912     C                   MOVEL     KPJBU         DSTL57
004000951113     C*
004100951110     C*---------------------------------------------------------------*
004200960919     C* SE RICEVO LA DATA ELABORAZIONE A 0 --> LA RICALCOLO
004300960919    1C     D57DEA        IFEQ      0
004400970718     C* DATA ELABORAZIONE AL: REPERISCO GIORNI DA DS5A
004500970718     C                   MOVEL     '5A'          COD
004600970718     C                   MOVEL(P)  '2'           KEY
004700960919     C     KTAB          CHAIN     TABEL                              30
004800960919     C     *IN30         IFEQ      *OFF
004900960919     C     TBLFLG        ANDEQ     ' '
005000970718     C                   MOVEL     TBLUNI        DS5A2
005100960919     C                   ELSE
005200970718     C                   CLEAR                   DS5A2
005300960919     C                   ENDIF
005400960919     C*
005500960919     C* PRENDO LA DATA DEL GIORNO DA TIME
005600960919     C                   TIME                    W0140            14 0
005700960919     C                   MOVE      W0140         UDATE8            8 0
005800960919     C*
005900960919     C                   MOVE      UDATE8        G02DAT
006000960919     C                   MOVEL     *BLANK        G02ERR
006100960919     C                   CALL      'XSRDA8'
006200960919     C                   PARM                    WLBDAT
006300960919     C*
006400970718     C* SOTTRAGGO §5AFVS AI GIORNI
006500970718     C     G02TGI        SUB       §5AFVS        GIOTGI
006600960919     C*
006700960919     C                   CALL      'XSRGI8'
006800960919     C                   PARM                    WGIDAT
006900960919     C                   Z-ADD     GIOINV        D57DEA
007000960919    1C                   ENDIF
007100960919     C*
007200990415     C                   READ      TNFVS01L                               30
007300960919     C*****
007400960919     C** E L A B O R A Z I O N E
007500960919     C*****
007600960916     C                   CLEAR                   WLNP
007700960916     C                   SETOFF                                       30
007800960916     C*
007900960916    1C     *IN30         DOWEQ     *OFF
008000960916     C     WLNP          SETGT     TNFVS03L
008100960916     C                   READ      TNFVS03L                               30
008200960916     C*
008300960916    2C     *IN30         IFEQ      *OFF
008400960916     C                   Z-ADD     FVSLNP        WLNP
008500960916     C*
008600960916     C     WLNP          SETLL     TNFVS03L
008700960916     C     WLNP          READE     TNFVS03L                               31
008800960916     C*
008900960916    3C     *IN31         DOWEQ     *OFF
009000960916     C*
009100960916     C* SE DATA FOGLIO VIAGGIO > DATA PULIZIA LEGGO ALTRA LINEA
009200960916    4C     FVSDFV        IFGT      D57DEA
009300960916     C                   SETON                                        31
009400960916   X4C                   ELSE
009500960916     C*
009600960916     C* VERIFICO CHE IL FOGLIO SIA STATO CONTROLLATO PER L'ABBINAMENTO
009700960916    5C     FVSABN        IFEQ      'S'
009800960916     C*
009900960916     C* SE FV ANNULLATO ---> DELETO SENZA CONTROLLARE IL TRAINO
010000960916    6C     FVSATB        IFNE      ' '
010100960918     C                   EXSR      PULIZ
010200960916     C*
010300960916   X6C                   ELSE
010400960916     C* SE RIESCO VERIFICO L'ESISTENZA DEL TRAINO ALTRIMENTI CANCELLO
010500960916     C*   IL FOGLIO
010600960916    7C     FVSTRS        IFEQ      0
010700960916     C     FVSTRS        OREQ      9999998
010800960916     C     FVSTRS        OREQ      9999999
010900960918     C                   EXSR      PULIZ
011000960916     C*
011100960916   X7C                   ELSE
011200960916     C* CONTROLLO L'ESISTENZA DEL TRAINO
011300960916     C     KFVS          SETLL     TNTSR04L                               32
011400960916     C  N32              EXSR      PULIZ
011500960916    7C                   ENDIF
011600960916    6C                   ENDIF
011700960916    5C                   ENDIF
011800960916     C*
011900960916     C*
012000960916     C     WLNP          READE     TNFVS03L                               31
012100960916    4C                   ENDIF
012200960916    3C                   ENDDO
012300960916     C*
012400960916    2C                   ENDIF
012500960916    1C                   ENDDO
012600990415     C*
012700990415     C* LEGGO DI FNFVD00F TUTTO CIO' CHE NON HA PIU' IL FOGLIO VIAGG
012800990415     C     *LOVAL        SETLL     FNFVD01L
012900990415     C                   READ      FNFVD01L                               30
013000990415     C     *IN30         DOWEQ     *OFF
013100990415     C     KFVD          SETLL     TNFVS01L                               35
013200990415     C     *IN35         IFEQ      *OFF
013300990415     C                   DELETE    FNFVD000
013400990415     C                   ENDIF
013500990415     C*
013600990415     C                   READ      FNFVD01L                               30
013700990415     C                   ENDDO
013800990415     C*
013900990415     C* LEGGO DI FNFV400F TUTTO CIO' CHE NON HA PIU' IL FOGLIO VIAGG
014000990415     C     *LOVAL        SETLL     FNFV401L
014100990415     C                   READ      FNFV401L                               30
014200990415     C     *IN30         DOWEQ     *OFF
014300990415     C     KFV4          SETLL     TNFVS01L                               35
014400990415     C     *IN35         IFEQ      *OFF
014500990415     C                   DELETE    FNFV4000
014600990415     C                   ENDIF
014700990415     C*
014800990415     C                   READ      FNFV401L                               30
014900990415     C                   ENDDO
015000990415     C*
015100990415     C* LEGGO DI FNFV400F TUTTO CIO' CHE NON HA PIU' IL FOGLIO VIAGG
015200990415     C     *LOVAL        SETLL     FNFV201L
015300990415     C                   READ      FNFV201L                               30
015400990415     C     *IN30         DOWEQ     *OFF
015500990415     C     KFV2          SETLL     TNFVS01L                               35
015600990415     C     *IN35         IFEQ      *OFF
015700990415     C                   DELETE    FNFV2000
015800990415     C                   ENDIF
015900990415     C*
016000990415     C                   READ      FNFV201L                               30
016100990415     C                   ENDDO
016200130211     C*
016300130211     C* LEGGO DI TNFWS00F TUTTO CIO' CHE NON HA PIU' IL FOGLIO TNFVS
016400130211     C     *LOVAL        SETLL     tNFWS01L
016500130211     C                   READ      tNFWS01L                               30
016600130211     C     *IN30         DOWEQ     *OFF
016700130211     C     KFws          SETLL     TNFVS01L                               35
016800130211     C     *IN35         IFEQ      *OFF
016900130212     C                   DELETE    tNFws000
017000130211     C                   ENDIF
017100130211     C*
017200130211     C                   READ      tNFws01L                               30
017300130211     C                   ENDDO
017400960916     C*
017500960916     C                   SETON                                        LR
017600960912     C*
017700960912     C*--- ROUTINE DI PULIZIA ----------------------------------------*
017800960912     C     PULIZ         BEGSR
017900960912     C*
018000960916     C****  FNFV400F ---> ESTENSIONE DESCR. FV PARTENZA  ****
018100960916     C                   SETOFF                                       33
018200960916     C     KFVS          SETLL     FNFV401L
018300960916    1C     *IN33         DOWEQ     *OFF
018400960916     C     KFVS          READE     FNFV401L                               33
018500960916     C  N33              DELETE    FNFV4000
018600960916    1C                   ENDDO
018700960916     C*
018800960916     C****  FNFV200F ---> DATA/ORA PART/ARR FV PARTENZA  ****
018900960916     C                   SETOFF                                       33
019000960916     C     KFVS          SETLL     FNFV201L
019100960916    1C     *IN33         DOWEQ     *OFF
019200960916     C     KFVS          READE     FNFV201L                               33
019300960916     C  N33              DELETE    FNFV2000
019400960916    1C                   ENDDO
019500960916     C*
019600960916     C****  FNFVD00F ---> DETTAGLIO TOTALI  FV PARTENZA  ****
019700960916     C                   SETOFF                                       33
019800960916     C     KFVS          SETLL     FNFVD01L
019900960916    1C     *IN33         DOWEQ     *OFF
020000960916     C     KFVS          READE     FNFVD01L                               33
020100960916     C  N33              DELETE    FNFVD000
020200960916    1C                   ENDDO
020300130211     C****  TNFGS00F ---> punto dis cambio
020400130211     C                   SETOFF                                       33
020500130211     C     KFVS          SETLL     tnfgs01L
020600130211    1C     *IN33         DOWEQ     *OFF
020700130211     C     KFVS          READE     tnfgs01L                               33
020800130211     C  N33              DELETE    fnfgs000
020900130211    1C                   ENDDO
021000960916     C*
021100130211     C****  TNFWS00F ---> FV PARTENZA  estensione
021200130211     C     KFVS          delete    tnfWS01L
021300130211     c
021400960916     C****  TNFVS00F ---> FV PARTENZA  ****
021500960916     C                   DELETE    TNFVS000
021600960912     C*
021700960912     C                   ENDSR
021800960912     C*
021900960226     C*--- SR INIZIALE -----------------------------------------------*
022000951110     C     *INZSR        BEGSR
022100960912     C****
022200960912     C** KLIST
022300960912     C****
022400960916     C     KFVS          KLIST
022500960916     C                   KFLD                    FVSLNP
022600960916     C                   KFLD                    FVSNFV
022700990415     C     KFVD          KLIST
022800990415     C                   KFLD                    FVDLNP
022900990415     C                   KFLD                    FVDNFV
023000990415     C     KFV4          KLIST
023100990415     C                   KFLD                    FV4LNP
023200990415     C                   KFLD                    FV4NFV
023300990415     C     KFV2          KLIST
023400990415     C                   KFLD                    FV2LNP
023500990415     C                   KFLD                    FV2NFV
023600130211     C     KFWS          KLIST
023700130211     C                   KFLD                    FwSLNP
023800130211     C                   KFLD                    FwSNFV
023900960919     C* ACCESSO TABEL00F
024000960919     C     KTAB          KLIST
024100960919     C                   KFLD                    CODUT
024200960919     C                   KFLD                    COD
024300960919     C                   KFLD                    KEY
024400960916     C****
024500960916     C** DEFINIZIONE CAMPI
024600960916     C****
024700960916     C     *LIKE         DEFINE    FVSLNP        WLNP
024800960919     C     *LIKE         DEFINE    TBLCOD        COD
024900960919     C     *LIKE         DEFINE    TBLKEY        KEY
025000960912     C*
025100960919     C* IMPOSTAZIONE CAMPI FISSI
025200960919     C                   Z-ADD     1             CODUT             1 0
025300960229     C                   ENDSR
