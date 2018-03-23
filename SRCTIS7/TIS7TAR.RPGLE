000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000125     H*
000400090325     Ftivgdtmp  uF   E             DISK
000500090309     FTIVAFwwt  O    E             DISK    commit
000600000125     F*
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000090325     D tivafds       e ds                  extname(tivaf00t)
001100941221     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200000125     D*  8+3
001300000125     D                 DS
001400000125     D  WS13                   1      1
001500000125     D  WI13                   2      9
001600000125     D  WV13                  10     10    inz(',')
001700000125     D  WD13                  11     13
001800000125     D  WASC13                 1     13
001900941221     D* 10+3
002000941221     D                 DS
002100941221     D  WS15                   1      1
002200941221     D  WI15                   2     11
002300991125     D  WV15                  12     12    inz(',')
002400941221     D  WD15                  13     15
002500941221     D  WASC15                 1     15
002600941221     D*  6+1
002700941221     D                 DS
002800941221     D  WS09                   1      1
002900941221     D  WI09                   2      7
003000991125     D  WV09                   8      8    inz(',')
003100941221     D  WD09                   9      9
003200941221     D  WASC09                 1      9
003300941221     D*  2+3
003400941221     D                 DS
003500941221     D  WS07                   1      1
003600941221     D  WI07                   2      3
003700991125     D  WV07                   4      4    inz(',')
003800941221     D  WD07                   5      7
003900941221     D  WASC07                 1      7
004000000904
004100000904     C*
004200991027     C                   EXSR      traduci
004300921023     C                   SETON                                        LR
004400000125     C*
004500991027     C     traduci       BEGSR
004600991027     C*
004700090325     C                   READ      tivgdtmp
004800991027     C                   dow       not %eof
004900090325     c                   movel     vgddta        tivafds
005000000904     C*
005100000125     C                   MOVE      VAFAAS        VHFAAS
005200000125     C                   MOVE      VAFLNP        VHFLNP
005300000125     C                   MOVE      VAFNRS        VHFNRS
005400000125     C                   MOVE      VAFNSP        VHFNSP
005500000125     C                   MOVE      VAFTBL        VHFTBL
005600000125     C                   MOVE      VAFMGS        VHFMGS
005700000125     C                   MOVE      VAFKSC        VHFKSC
005800000125     C                   MOVE      VAFLNA        VHFLNA
005900000125     C                   MOVE      VAFNCL        VHFNCL
006000000125     C*
006100000125     C                   MOVE      VAFPKB        WD09
006200000125     C                   MOVEL     VAFPKB        WI09
006300000125     C                   MOVE      WASC09        VHFPKB
006400000125     C*
006500000125     C                   MOVE      VAFVLF        WD07
006600000125     C                   MOVEL     VAFVLF        WI07
006700000125     C                   MOVE      WASC07        VHFVLF
006800000125     C*
006900000125     C                   MOVE      VAFQFT        WD15
007000000125     C                   MOVEL     VAFQFT        WI15
007100000125     C                   MOVE      WASC15        VHFQFT
007200000125     C*
007300000125     C                   MOVE      VAFPOR        WD13
007400000125     C                   MOVEL     VAFPOR        WI13
007500000125     C                   MOVE      WASC13        VHFPOR
007600000125     C*
007700000125     C                   MOVE      VAFSV1        VHFSV1
007800000125     C                   MOVE      VAFVA1        WD13
007900000125     C                   MOVEL     VAFVA1        WI13
008000000125     C                   MOVE      WASC13        VHFVA1
008100000125     C*
008200000125     C                   MOVE      VAFSV2        VHFSV2
008300000125     C                   MOVE      VAFVA2        WD13
008400000125     C                   MOVEL     VAFVA2        WI13
008500000125     C                   MOVE      WASC13        VHFVA2
008600000125     C*
008700000125     C                   MOVE      VAFSV3        VHFSV3
008800000125     C                   MOVE      VAFVA3        WD13
008900000125     C                   MOVEL     VAFVA3        WI13
009000000125     C                   MOVE      WASC13        VHFVA3
009100000125     C*
009200000125     C                   MOVE      VAFSV4        VHFSV4
009300000125     C                   MOVE      VAFVA4        WD13
009400000125     C                   MOVEL     VAFVA4        WI13
009500000125     C                   MOVE      WASC13        VHFVA4
009600000125     C*
009700000125     C                   MOVE      VAFSV5        VHFSV5
009800000125     C                   MOVE      VAFVA5        WD13
009900000125     C                   MOVEL     VAFVA5        WI13
010000000125     C                   MOVE      WASC13        VHFVA5
010100000125     C*
010200000125     C                   MOVE      VAFSV6        VHFSV6
010300000125     C                   MOVE      VAFVA6        WD13
010400000125     C                   MOVEL     VAFVA6        WI13
010500000125     C                   MOVE      WASC13        VHFVA6
010600000125     C*
010700000125     C                   MOVE      VAFSV7        VHFSV7
010800000125     C                   MOVE      VAFVA7        WD13
010900000125     C                   MOVEL     VAFVA7        WI13
011000000125     C                   MOVE      WASC13        VHFVA7
011100000125     C*
011200000125     C                   MOVE      VAFSV8        VHFSV8
011300000125     C                   MOVE      VAFVA8        WD13
011400000125     C                   MOVEL     VAFVA8        WI13
011500000125     C                   MOVE      WASC13        VHFVA8
011600000125     C*
011700000125     C                   MOVE      VAFVAX        WD13
011800000125     C                   MOVEL     VAFVAX        WI13
011900000125     C                   MOVE      WASC13        VHFVAX
012000000125     C*
012100000125     C                   MOVE      VAFIMV        WD15
012200000125     C                   MOVEL     VAFIMV        WI15
012300000125     C                   MOVE      WASC15        VHFIMV
012400000125     C*
012500000125     C                   MOVE      VAFDFT        VHFDFT
012600000125     C                   MOVE      VAFNFT        VHFNFT
012700000125     C                   MOVE      VAFFIV        VHFFIV
012800000125     C                   MOVE      VAFDIV        VHFDIV
012900000125     C                   MOVE      VAFDRT        VHFDRT
013000000125     C                   MOVE      VAFRMN        VHFRMN
013100000125     C                   MOVE      VAFRMA        VHFRMA
013200000125     C                   MOVE      VAFRMX        VHFRMX
013300000125     C                   MOVE      VAFFTC        VHFFTC
013400000125     C                   MOVE      VAFTC2        VHFTC2
013500000125     C                   MOVE      VAFTSP        VHFTSP
013600000125     C                   MOVE      VAFFAP        VHFFAP
013700000125     C                   MOVE      VAFFIN        VHFFIN
013800000125     C                   MOVE      VAFCTR        VHFCTR
013900000125     C                   MOVE      VAFNAS        VHFNAS
014000000125     C                   MOVE      VAFCTS        VHFCTS
014100000125     C                   MOVE      VAFCAD        VHFCAD
014200000125     C                   MOVE      VAFLOD        VHFLOD
014300000125     C                   MOVE      VAFPRD        VHFPRD
014400000125     C                   MOVE      VAFNZD        VHFNZD
014500000125     C                   MOVE      VAFVAS        VHFVAS
014600000125     C*
014700000125     C                   MOVE      VAFIAS        WD15
014800000125     C                   MOVEL     VAFIAS        WI15
014900000125     C                   MOVE      WASC15        VHFIAS
015000000125     C*
015100000125     C                   MOVE      VAFFPC        VHFFPC
015200000125     C*
015300000125     C                   MOVE      VAFPKC        WD09
015400000125     C                   MOVEL     VAFPKC        WI09
015500000125     C                   MOVE      WASC09        VHFPKC
015600000125     C*
015700000125     C                   MOVE      VAFFVC        VHFFVC
015800000125     C*
015900000125     C                   MOVE      VAFVLC        WD07
016000000125     C                   MOVEL     VAFVLC        WI07
016100000125     C                   MOVE      WASC07        VHFVLC
016200930409     C*
016300000125     C                   WRITE     TIVAFaa
016400090325     C                   delete    tivgd000
016500000904     C*
016600090325     C                   READ      tivgdtmp
016700991027     C                   ENDdo
016800991027     C*
016900991027     C                   eval      wrkesito = '0'
017000991027     C*
017100910830     C                   ENDSR
017200000125     C*
017300991027      /TITLE Operazioni iniziali.
017400991027     C     *inzsr        BEGSR
017500991027     C*
017600991027     C     *ENTRY        PLIST
017700991027     C                   parm                    prmppt
017800991027     C     wrkesito      parm      wrkesito      prmesito
017900991027     C*
018000991027     C                   ENDSR
