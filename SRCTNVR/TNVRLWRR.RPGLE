000100020618     H DECEDIT('0,') DATEDIT(*DMY.)
000200020618     F*------------------------------------------------------------------------*
000300020618     F* DATA BASE
000400020618     F*------------------------------------------------------------------------*
000500020821     Ftitas39C  UF   E           k DISK    PREFIX(O_)
000600020821     F
000700020821     Ftitas30C  UF   E           k DISK    RENAME(TITAS000:TAS30000)
000800020821     F                                     RENAME(TITAS010:TAS30010)
000900020821     F                                     RENAME(TITASP00:TAS30P00)
001000020618     ITITAS000      01
001100020618     ITITAS010      02
001200020618     ITITASP00      03
001300020821     ITAS30000      11
001400020821     ITAS30010      12
001500020821     ITAS30P00      13
001600020618     C*------------------------------------------------------------------------*
001700020618     C* MAIN LINE
001800020618     C*------------------------------------------------------------------------*
001900020618     C     *LIKE         DEFINE    TasAas        kasAas
002000020618     C     *LIKE         DEFINE    TasLnp        kasLnp
002100020821     C     *LIKE         DEFINE    TasMgs        kasMgs
002200020821     C     *LIKE         DEFINE    TasNrs        kasNrs
002300020821     C     *LIKE         DEFINE    TasNsp        kasNsp
002400020618     c*
002500020618     C     keytas        KLIST
002600020821     C                   KFLD                    kasLnp
002700020618     C                   KFLD                    kasAas
002800020821     C                   KFLD                    kasMgs
002900020821     C     ke2tas        KLIST
003000020821     C                   KFLD                    kasAas
003100020821     C                   KFLD                    kasLnp
003200020821     C                   KFLD                    kasNrs
003300020821     C                   KFLD                    kasNsp
003400020618     C*
003500020821     c                   Z-ADD     340           kaslnp
003600020821     c                   Z-ADD     2002          kasaas
003700020821     c                   Z-ADD     0201          kasmgs
003800020821     c                   SETOFF                                       010203
003900020821     C     keytas        SETLL     titas39c
004000020821     C                   READ      titas39c                               99
004100020821do  1C                   DOW       not *in99
004200020821if  2C                   IF        o_tasmgs>0300
004300020821     c                   LEAVE
004400020821e   2c                   ENDIF
004500020821if  2c                   IF        o_tasnsp>=9001070 AND
004600020821     C                             o_tasnsp<=9001230 AND
004700020821     C                             o_tasdcm>=20020801
004800020821     C                   Z-ADD     o_tasAas      kasAas
004900020821     C                   Z-ADD     o_tasLnp      kasLnp
005000020821     C                   Z-ADD     o_tasNrs      kasNrs
005100020821     C                   EVAL      kasNsp = o_tasnsp
005200020821     C                   MOVEL     0             kasnsp
005300020821     c                   SETOFF                                       111213
005400020821     c     ke2tas        CHAIN     titas30c                           98
005500020821if  3c     *in98         IFEQ      *off
005600020821     c                   Z-ADD     o_TASPDC      TASPDC
005700020821     c                   Z-ADD     o_TASNDC      TASNDC
005800020821     c                   Z-ADD     o_TASDDC      TASDDC
005900020821     c                   Z-ADD     o_TASDCE      TASDCE
006000020821     c                   Z-ADD     o_TASHCE      TASHCE
006100020821     c                   Z-ADD     o_TASDCP      TASDCP
006200020821     c                   Z-ADD     o_TASDCM      TASDCM
006300020821     c                   Z-ADD     o_TASHMC      TASHMC
006400020821     c                   MOVEL     o_TASCCA      TASCCA
006500020821     c                   Z-ADD     o_TASNPI      TASNPI
006600020821     c                   Z-ADD     o_TASNCI      TASNCI
006700020821     c                   Z-ADD     o_TASNRC      TASNRC
006800020821     c                   Z-ADD     O_TASSTP      TASSTP
006900020821     c                   Z-ADD     o_TASDTI      TASDTI
007000020821     c                   Z-ADD     o_TASHTI      TASHTI
007100020821     c   11              UPDATE    TAS30000
007200020821     c   12              UPDATE    TAS30010
007300020821     c   13              UPDATE    TAS30P00
007400020821e   3c                   ENDIF
007500020821     c                   Z-ADD     999           o_TASPDC
007600020821     c                   MOVEL     o_taslna      o_TASPDC
007700020821     c                   MOVEL     *all'9'       o_TASNDC
007800020821     c                   MOVEL     O_TASAAS      o_TASDDC
007900020821     c                   MOVE      O_TASMGS      o_TASDDC
008000020821     c                   Z-ADD     o_TASDDC      O_TASDCE
008100020821     c                   Z-ADD     2359          O_TASHCE
008200020821     c                   Z-ADD     *ZEROS        O_TASDCP
008300020821     c                   Z-ADD     o_TASDDC      O_TASDCM
008400020821     c                   Z-ADD     2359          O_TASHMC
008500020821     c                   MOVEL     *blanks       o_TASCCA
008600020821     c                   Z-ADD     *zeros        o_TASNPI
008700020821     c                   Z-ADD     *zeros        o_TASNCI
008800020821     c                   Z-ADD     *zeros        o_TASNRC
008900020821     c                   Z-ADD     1             O_TASSTP
009000020821     c                   Z-ADD     o_TASDDC      o_TASDTI
009100020821     c                   Z-ADD     2359          o_TASHTI
009200020821     c   01              UPDATE    TITAS000
009300020821     c   02              UPDATE    TITAS010
009400020821     c   03              UPDATE    TITASP00
009500020821e   2c                   ENDIF
009600020618     c*
009700020821     c                   SETOFF                                       010203
009800020821     C                   READ      titas39c                               99
009900020618E   1C                   ENDDO
010000020618     C*
010100020618     c                   SETON                                        lr
