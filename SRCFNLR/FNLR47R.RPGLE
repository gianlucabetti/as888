000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941227      * FNLR47R *----------------------------------------------------*
000300911206      *            - RISTAMPA FATTURE ASSEGNATI -    BATCH           *
000400911205      *--------------------------------------------------------------*
000500990924     FFIAR635L  IF   E           K DISK
000600991229     FFNARB01L  IF   E           K DISK
000700991229     FTABEL00F  IF   E           K DISK
000800911205      *
000900900523     D PARAM           DS
001000941227     D  VIDDFD                 1      8  0
001100941227     D  VIDDFA                 7     16  0
001200941227     D  COMDFD                17     24  0
001300941227     D  COMDFA                25     32  0
001400941227     D  VIDNFD                33     38  0
001500941227     D  VIDNFA                39     44  0
001600941227     D  VIDFIV                45     47  0
001700030619     d* - parametri da TRUL90R:
001800030619     d* · pgm.   per stampa LDV
001900030619     d  wD90psl              187    196
002000030619      * DS per pgm di STAMPA BOLLE
002100030619     d FNLSB5DS      e ds                  inz
002200030619      *
002300991229     D DS3A          E DS
002400991229     D DSTB          E DS
002500911211      *
002600940216     D KPJBA         E DS
002700911206      *
002800911206      ****************************************************************
002900911206      *  RIEPILOGO INDICATORI
003000911210      ****************************************************************
003100911206      * 05    - IMMESSA DATA DAL..AL..
003200911216      * 06    - IMMESSO NUMERO FATTURA DAL..AL..O SOLO NUMERO AL..
003300911216      * 07    - IMMESSO SOLO NUMERO FATTURA DAL..
003400911211      * 31    - DI COMODO,RIUTILIZZABILE
003500911206      *****************************************************************
003600000000     C     *ENTRY        PLIST
003700000000     C                   PARM                    KPJBA
003800911211     C                   MOVEL     KPJBU         PARAM
003900911211      *---------------------------------------------------------------*
004000990924      * CHIAVE X POSIZIONAMENTO SU FIAR635L
004100911211      *
004200911211     C     KARB          KLIST                                                  *
004300911211     C                   KFLD                    FIV               3 0          *
004400941227     C                   KFLD                    DFT               8 0          *
004500911211     C                   KFLD                    NFT               6 0          *
004600991229     C     KAR6          KLIST                                                  *
004700991229     C                   KFLD                    AR6AAS
004800991229     C                   KFLD                    AR6LNP
004900991229     C                   KFLD                    AR6NRS
005000991229     C                   KFLD                    AR6NSP
005100991229     C     KTAB          KLIST                                                  *
005200991229     C                   KFLD                    CODUT             1 0
005300991229     C                   KFLD                    COD
005400991229     C                   KFLD                    KEY
005500991229     C**
005600991229     C     *LIKE         DEFINE    TBLCOD        COD
005700991229     C     *LIKE         DEFINE    TBLKEY        KEY
005800991229     C                   Z-ADD     1             CODUT
005900911212      *
006000911216      * CONTROLLO SE IMMESSA DATA FATTURA DAL...AL...
006100911216     C     COMDFA        IFNE      0
006200911206     C                   SETON                                          05
006300911206     C                   END
006400911206      *
006500911216      * CONTROLLO SE IMMESSO N°FATTURA DAL...AL.../SOLO N°FATTURA AL...
006600911216     C     VIDNFA        IFNE      0
006700911206     C                   SETON                                          06
006800911206     C                   END
006900911206      *
007000911212      * CONTROLLO SE IMMESSO SOLO NUMERO FATTURA DAL...
007100911212     C     VIDNFD        IFNE      0
007200911212     C     VIDNFA        ANDEQ     0
007300911216     C                   SETON                                          07
007400911212     C                   END
007500911212      *
007600911206     C                   Z-ADD     VIDFIV        FIV
007700911209     C                   Z-ADD     COMDFD        DFT
007800911206     C                   Z-ADD     VIDNFD        NFT
007900911206      *
008000990924     C     KARB          SETLL     FIAR635L
008100990924     C     FIV           READE     FIAR635L                               31
008200030619    1C     *IN31         DOWEQ     *OFF
008300911210      *
008400911210      * CONTROLLO DATA FATTURA
008500990924     C     AR6DFT        IFNE      COMDFD
008600990924     C     *IN05         ANDEQ     *OFF
008700911206     C                   SETON                                            31
008800911206     C                   END
008900990924     C     AR6DFT        IFGT      COMDFA
009000990924     C     *IN05         ANDEQ     *ON
009100911206     C                   SETON                                            31
009200911206     C                   END
009300911206      *
009400911216      * CONTROLLO NUMERO FATTURA DAL...AL...
009500990924     C   06AR6NFT        IFLT      VIDNFD
009600941227     C     AR6NFT        ORGT      VIDNFA
009700911212     C                   SETON                                            31
009800911212     C                   END
009900911206      *
010000911212      * CONTROLLO NUMERO FATTURA DAL...
010100990924     C   07AR6NFT        IFNE      VIDNFD
010200911213     C                   SETON                                            31
010300911213     C                   END
010400911212      *
010500991229      *
010600991229    2C  N31              DO
010700991229     C     KAR6          CHAIN     FNARB01L                           30
010800991229    3C     *IN30         IFEQ      *OFF
010900991229     C                   MOVEL     '3A'          COD
011000991229     C                   MOVEL(P)  ARBCBO        KEY
011100991229     C     KTAB          CHAIN     TABEL                              33
011200991229    4C     *IN33         IFEQ      *OFF
011300991229     C                   MOVEL     TBLUNI        DS3A
011400991229     C                   MOVEL     §3ATB1        FLGTB1            1
011500991229     C*
011600991229    5C     §3ATB2        IFNE      *BLANKS
011700991229     C     FLGTB1        OREQ      'A'
011800991229     C**
011900991229     C* ESCLUDO SE TIPO BOLLA NON DA CONTABILIZZARE
012000991229     C     §3ATB2        IFNE      *BLANKS
012100991229     C                   MOVEL(P)  §3ATB2        KEY
012200991229     C                   ELSE
012300991229     C                   MOVEL(P)  §3ATB1        KEY
012400991229     C                   ENDIF
012500991229     C                   MOVEL     'TB'          COD
012600991229     C     KTAB          CHAIN     TABEL                              33
012700991229     C**
012800991229    6C     *IN33         IFEQ      *OFF
012900991229     C                   MOVEL     TBLUNI        DSTB
013000991229    7C     §TBFCB        IFEQ      '1'
013100911216      *
013200030619     c                   clear                   FNLSB5DS
013300941227     C                   Z-ADD     AR6AAS        DB0AAS
013400941227     C                   Z-ADD     AR6LNP        DB0LNP
013500941227     C                   Z-ADD     AR6NRS        DB0NRS
013600941227     C                   Z-ADD     AR6NSP        DB0NSP
013700990924      *
013800941227     C                   MOVE      'T'           DB0TBO
013900941227     C                   MOVE      'R'           DB0RIS
014000911210      *
014100030619     C                   call      wD90psl
014200030619     c                   parm                    FNLSB5DS
014300911216      *
014400030619    7C                   ENDIF
014500030619    6C                   ENDIF
014600030619    5C                   ENDIF
014700030619    4C                   ENDIF
014800030619    3C                   ENDIF
014900030619    2C                   ENDDO
015000911211      *
015100990924     C  N31FIV           READE     FIAR635L                               31
015200030619    1C                   ENDDO
015300911206      *
015400911206     C     FINE          TAG
015500911206     C                   SETON                                        LR
