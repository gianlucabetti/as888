000100890921     H DECEDIT('0,') DATEDIT(*DMY.)
000200090323     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000300000125     H*
000400090325     Ftivgdtmp  uF   E             DISK
000500090309     FTIVAF1Wt  O    E             DISK    commit
000600000125     F*
000700991027     D prmppt          s             50
000800991027     D prmesito        s              1
000900991027     D wrkesito        s                   like(prmesito)
001000090325     D tivafds       e ds                  extname(tivaf00t)
001100000904
001200000904     C*
001300991027     C                   EXSR      traduci
001400921023     C                   SETON                                        LR
001500000125     C*
001600020611
001700991027     C     traduci       BEGSR
001800991027     C*
001900090325     C                   READ      tivgdtmp
002000020611     C                   DOW       not %eof
002100090325     c                   movel     vgddta        tivafds
002200000904     C*
002300000125     C                   MOVE      VAFAAS        VHFAAS
002400000125     C                   MOVE      VAFLNP        VHFLNP
002500000125     C                   MOVE      VAFNRS        VHFNRS
002600000125     C                   MOVE      VAFNSP        VHFNSP
002700000125     C                   MOVE      VAFTBL        VHFTBL
002800000125     C                   MOVE      VAFMGS        VHFMGS
002900000125     C                   MOVE      VAFKSC        VHFKSC
003000000125     C                   MOVE      VAFLNA        VHFLNA
003100000125     C                   MOVE      VAFNCL        VHFNCL
003200020611     C                   MOVE      VAFPKB        VHFPKB
003300020611     C                   MOVE      VAFVLF        VHFVLF
003400020611     C                   MOVE      VAFQFT        VHFQFT
003500020611     C                   MOVE      VAFPOR        VHFPOR
003600000125     C                   MOVE      VAFSV1        VHFSV1
003700020611     C                   MOVE      VAFVA1        VHFVA1
003800000125     C                   MOVE      VAFSV2        VHFSV2
003900020611     C                   MOVE      VAFVA2        VHFVA2
004000000125     C                   MOVE      VAFSV3        VHFSV3
004100020611     C                   MOVE      VAFVA3        VHFVA3
004200000125     C                   MOVE      VAFSV4        VHFSV4
004300020611     C                   MOVE      VAFVA4        VHFVA4
004400000125     C                   MOVE      VAFSV5        VHFSV5
004500020611     C                   MOVE      VAFVA5        VHFVA5
004600000125     C                   MOVE      VAFSV6        VHFSV6
004700020611     C                   MOVE      VAFVA6        VHFVA6
004800000125     C                   MOVE      VAFSV7        VHFSV7
004900020611     C                   MOVE      VAFVA7        VHFVA7
005000000125     C                   MOVE      VAFSV8        VHFSV8
005100020611     C                   MOVE      VAFVA8        VHFVA8
005200020611     C                   MOVE      VAFVAX        VHFVAX
005300020611     C                   MOVE      VAFIMV        VHFIMV
005400000125     C                   MOVE      VAFDFT        VHFDFT
005500000125     C                   MOVE      VAFNFT        VHFNFT
005600000125     C                   MOVE      VAFFIV        VHFFIV
005700000125     C                   MOVE      VAFDIV        VHFDIV
005800000125     C                   MOVE      VAFDRT        VHFDRT
005900000125     C                   MOVE      VAFRMN        VHFRMN
006000000125     C                   MOVE      VAFRMA        VHFRMA
006100000125     C                   MOVE      VAFRMX        VHFRMX
006200000125     C                   MOVE      VAFFTC        VHFFTC
006300000125     C                   MOVE      VAFTC2        VHFTC2
006400000125     C                   MOVE      VAFTSP        VHFTSP
006500000125     C                   MOVE      VAFFAP        VHFFAP
006600000125     C                   MOVE      VAFFIN        VHFFIN
006700000125     C                   MOVE      VAFCTR        VHFCTR
006800000125     C                   MOVE      VAFNAS        VHFNAS
006900000125     C                   MOVE      VAFCTS        VHFCTS
007000000125     C                   MOVE      VAFCAD        VHFCAD
007100000125     C                   MOVE      VAFLOD        VHFLOD
007200000125     C                   MOVE      VAFPRD        VHFPRD
007300000125     C                   MOVE      VAFNZD        VHFNZD
007400000125     C                   MOVE      VAFVAS        VHFVAS
007500020611     C                   MOVE      VAFIAS        VHFIAS
007600000125     C                   MOVE      VAFFPC        VHFFPC
007700020611     C                   MOVE      VAFPKC        VHFPKC
007800000125     C                   MOVE      VAFFVC        VHFFVC
007900020611     C                   MOVE      VAFVLC        VHFVLC
008000930409     C*
008100000125     C                   WRITE     TIVAFaa
008200090325     C                   delete    tivgd000
008300000904     C*
008400090325     C                   READ      tivgdtmp
008500020611     C                   ENDDO
008600991027     C*
008700020611     C                   EVAL      wrkesito = '0'
008800991027     C*
008900910830     C                   ENDSR
009000000125     C*
009100991027      /TITLE Operazioni iniziali.
009200991027     C     *inzsr        BEGSR
009300991027     C*
009400991027     C     *ENTRY        PLIST
009500991027     C                   parm                    prmppt
009600991027     C     wrkesito      parm      wrkesito      prmesito
009700991027     C*
009800991027     C                   ENDSR
