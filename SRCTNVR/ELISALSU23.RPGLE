000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300061009     D kpjba         E DS
000400160819
000500160819     D  FNLSU2R3DS     DS
000600160819     D  sa2tla                        1
000700160819     D  sa2trc                        1
000800160819     D  sa2dts                        8  0
000900160819     D  sa2lna                        3  0
001000160819     D  sa2ncl                        5  0
001100160819     D  sa2tfp                        3  0
001200160819     c                   clear                   fnlsu2r3ds
001300160819     c                   eval      sa2trc='E'
001400160819     c                   eval      sa2dts=20160307
001500160819     c                   eval      sa2ncl=7
001600160819     c                   eval      sa2tfp=001
001700160819     c                   call      'FNLSU2R3'
001800160819     c                   parm                    fnlsu2r3ds
001900160819     c
002000160819     c                   clear                   fnlsu2r3ds
002100160819     c                   eval      sa2trc='P'
002200160819     c                   eval      sa2dts=20160307
002300160819     c                   eval      sa2ncl=2
002400160819     c                   eval      sa2tfp=015
002500160819     c                   call      'FNLSU2R3'
002600160819     c                   parm                    fnlsu2r3ds
002700160819     c
002800160819     c                   clear                   fnlsu2r3ds
002900160819     c                   eval      sa2trc='C'
003000160819     c                   eval      sa2dts=20160307
003100160819     c                   eval      sa2ncl=0
003200160819     c                   eval      sa2tfp=015
003300160819     c                   eval      sa2lna=014
003400160819     c                   call      'FNLSU2R3'
003500160819     c                   parm                    fnlsu2r3ds
003600160819     c
003700160819     c                   clear                   fnlsu2r3ds
003800160819     c                   eval      sa2trc='A'
003900160819     c                   eval      sa2dts=20160307
004000160819     c                   eval      sa2ncl=0
004100160819     c                   eval      sa2tfp=015
004200160819     c                   call      'FNLSU2R3'
004300160819     c                   parm                    fnlsu2r3ds
004400061006     C*
004500160819     c                   clear                   fnlsu2r3ds
004600160819     c                   eval      sa2tla='C'
004700160819     c                   call      'FNLSU2R3'
004800160819     c                   parm                    fnlsu2r3ds
004900020502     C                   SETON                                        LR
