000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     D param           DS
000600030109     D  data                          8s 0
000700030109     D  poges                         3s 0
000800030109     D  tipopre                       1
000900030109     d  error                         1
001000030109
001100030109
001200030114     d trovate         s              5  0 inz
001300030109     d dalcod          s              7  0 inz
001400030109     d alcod           s              7  0 inz(9999999)
001500020624     D*
001600011026
001700000000     C*---------------------------------------------------------------*
001800020624     c     *entry        plist
001900030109     c                   parm                    param
002000030109     c                   movel     poges         dalcod
002100030109     c                   movel     poges         alcod
002200030109
002300020624
002400020313     C/EXEC SQL
002500030114     C+ DECLARE A1 CURSOR FOR SELECT count(*) FROM fifgt00F WHERE fgttsr
002600030114     C+ = :tipopre and fgtpdr between :dalcod and :alcod and fgtddt <=
002700030121     C+ :data and fgtdst >= :data and fgtatb = ' '
002800020313     C/END-EXEC
002900030109      *
003000030109     C/EXEC SQL
003100030109     C+ OPEN A1
003200030109     C/END-EXEC
003300030109      *
003400030109     C/EXEC SQL
003500030114     C+ FETCH  next FROM A1 INTO :trovate
003600030109     C/END-EXEC
003700030114     c                   if        trovate <> 0
003800030109     c                   move      '1'           error
003900030109     c                   else
004000030114     c                   move      ' '           error
004100030109     c                   endif
004200030109     c
004300020624      *
004400030109     C/EXEC SQL
004500030109     C+ CLOSE A1
004600030109     C/END-EXEC
004700030109     c                   seton                                        lr
004800011129     C*--------------------------------------------------------------------
