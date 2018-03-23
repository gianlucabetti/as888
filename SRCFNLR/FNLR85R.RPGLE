000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941212     H* FNLR85R *----------------------------------------------------*
000300920324     H*        - PULIZIA BOLLE TRANSITO
000400000000     H*--------------------------------------------------------------*
000500031128     FFNBTP15L  UF   E           K DISK
000600031128     FFNBTT11L  UF   E           K DISK
000700050105     FFNBLP01L  IF   E           K DISK
000800920325     D*
000900920325     D PARAM           DS
001000941212     D  COMDPT                25     32  0
001100920324     D                 DS
001200941212     D  BTPAAS                 1      4  0
001300941212     D  BTPMGS                 5      8  0
001400941212     D  BTPDSP                 1      8  0
001500031128     d
001600031128     d wflp            s                   Like(BTPFLP)
001700031128     d
001800000000     D KPJBA         E DS
001900000000     I/SPACE 3
002000000000     C*---------------------------------------------------------------*
002100000000     C     *ENTRY        PLIST
002200000000     C                   PARM                    KPJBA
002300900409     C                   MOVEL     KPJBU         PARAM
002400920205     C*---------------------------------------------------------------*
002500031128     C* ACCESSO FNBTP15L
002600031128     C     KBTP1         KLIST
002700031128     C                   KFLD                    Wflp
002800031128     C                   KFLD                    WDUT
002900031128     C     KBTP2         KLIST
003000031128     C                   KFLD                    Wflp
003100031128     C                   KFLD                    WDUT
003200031128     C                   KFLD                    WDET
003300050105     C     KBLP          KLIST
003400050105     C                   KFLD                    blpaas
003500050105     C                   KFLD                    blplnp
003600050105     C                   KFLD                    blpnrs
003700050105     C                   KFLD                    blpnsp
003800941212     C* ACCESSO FNBTT01L
003900920324     C     KBTT          KLIST
004000031128     C                   KFLD                    BTPFLP
004100031128     C                   KFLD                    BTPAAS
004200941212     C                   KFLD                    BTPLNP
004300941212     C                   KFLD                    BTPNRS
004400941212     C                   KFLD                    BTPNSP
004500941212     C*
004600941212     C* DEFINIZIONE CAMPI
004700941212     C     *LIKE         DEFINE    BTPDUT        WDUT
004800941212     C     *LIKE         DEFINE    BTPDET        WDET
004900941212     C*---------------------------------------------------------------*
005000031128     c* Per ogni p.o. di transito elaboro i 2 cicli
005100031128     c                   z-add     0             wflp
005200031128     c     wflp          setgt     fnbtp000
005300031128     c                   read      fnbtp000                               32
005400031128    0c                   dow       not *in32
005500031128     c                   z-add     btpflp        wflp
005600941212     C***
005700941212     C** 1°  C I C L O : ELABORO SOLO I RECORD CON BTPDUT > 0 E DELETO
005800941212     C**                 QUELLI CHE HANNO LA DATA USCITA DAL TRANSITO
005900941212     C**                 MINORE O UGUALE ALLA DATA PULIZIA
006000941212     C***
006100941212     C                   Z-ADD     1             WDUT
006200031128     C     kbtp1         SETGT     FNBTP000
006300031128     C     wflp          READE     FNBTP000                               30
006400920306     C*
006500941212    1C     *IN30         DOWEQ     *OFF
006600941212    2C     BTPDUT        IFLE      COMDPT
006700920324     C                   EXSR      PULIZ
006800941213     C                   ELSE
006900941213     C* SE DATA USCITA DAL TRANSITO > DATA PULIZIA ESCO
007000941213     C                   SETON                                        30
007100941212    2C                   ENDIF
007200941212     C*
007300031128     C  N30wflp          READE     FNBTP000                               30
007400941212    1C                   ENDDO
007500941212     C***
007600941212     C** 2°  C I C L O : ELABORO SOLO I RECORD CON BTPDUT E BTPDET = 0
007700941212     C**                 E DELETO QUELLI CHE HANNO LA DATA SPEDIZIONE
007800941212     C**                 MINORE O UGUALE ALLA DATA PULIZIA
007900941212     C***
008000941212     C                   CLEAR                   WDUT
008100941212     C                   CLEAR                   WDET
008200031128     C     KBTP2         SETLL     FNBTP000
008300031128     C     KBTP2         READE     FNBTP000                               30
008400920325     C*
008500941212    1C     *IN30         DOWEQ     *OFF
008600941212    2C     BTPDSP        IFLE      COMDPT
008700920325     C                   EXSR      PULIZ
008800941212    2C                   ENDIF
008900920325     C*
009000031128     C     KBTP2         READE     FNBTP000                               30
009100941212    1C                   ENDDO
009200050105     C***
009300050105     C** 3°  C I C L O : ELABORO SOLO I RECORD CON BTPDUT = 0 E btpdet>0
009400050105     C**                 se la  relativa  bolla partenza risulta consegnata
009500050105     C***
009600050105     C                   Z-ADD     0             WDUT
009700050105     C                   z-add     1             WDET
009800050105     C     kbtp2         SETll     FNBTP000
009900050105     C     kbtp1         READE     FNBTP000                               30
010000050105     C*
010100050105    1C     *IN30         DOWEQ     *OFF
010200050105    2C     BTPDsp        IFLE      COMDPT
010300050105     c     btpdet        andgt     0
010400050105     c*
010500050105     c     kblp          chain     fnblp01l
010600050105     c                   if        %found(fnblp01l) and blpdcm>0
010700050105     C                   EXSR      PULIZ
010800050105    2C                   ENDIF
010900050105    2C                   ENDIF
011000050105     C*
011100050105     C     kbtp1         READE     FNBTP000                               30
011200050105    1C                   ENDDO
011300031128     c
011400031128     c* Leggo nuovo p.o. transito
011500031128     c     wflp          setgt     fnbtp000
011600031128     c                   read      fnbtp000                               32
011700031128    0C                   ENDDO
011800920205     C*
011900920205     C                   SETON                                        LR
012000920205     C*
012100941212     C*--- PULIZIA FILE TRANSITI E FILE COLLEGATI --------------------*
012200920324     C     PULIZ         BEGSR
012300941212     C**
012400941212     C* DETTAGLIO COLLI
012500941212     C**
012600941212     C     KBTT          SETLL     FNBTT000
012700941212     C     KBTT          READE     FNBTT000                               31
012800920324     C*
012900941212    1C     *IN31         DOWEQ     *OFF
013000941212     C                   DELETE    FNBTT000
013100941212     C     KBTT          READE     FNBTT000                               31
013200941212    1C                   ENDDO
013300920324     C**
013400941212     C* CANCELLO LA BOLLA TRANSITO
013500920324     C**
013600941212     C                   DELETE    FNBTP000
013700920324     C*
013800920306     C                   ENDSR
