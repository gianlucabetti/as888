000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     D param           DS
000600030109     D  data                          8s 0
000700030109     D  poges                         3s 0
000800030109     D  tipopre                       1
000900030109     d  dsfgtout                           like(fgtds)
001000030109     d  error                         1
001100030109
001200030109     d fgtds         e ds                  extname(fifgt00f)
001300030109
001400030109     d dalcod          s              7  0 inz
001500030109     d alcod           s              7  0 inz(9999999)
001600020624     D*
001700011026
001800000000     C*---------------------------------------------------------------*
001900020624     c     *entry        plist
002000030109     c                   parm                    param
002100030109     c                   clear                   dsfgtout
002200030109     c                   movel     poges         dalcod
002300030109     c                   movel     poges         alcod
002400030109
002500020624
002600020313     C/EXEC SQL
002700030121     C+ DECLARE A1 CURSOR FOR SELECT * FROM fifgt00F WHERE fgttsr =
002800030121     C+ :tipopre and fgtpdr between :dalcod and :alcod and fgtddt <=
002900030121     C+ :data and fgtdst >= :data and fgtatb = ' '
003000020313     C/END-EXEC
003100030109      *
003200030109     C/EXEC SQL
003300030109     C+ OPEN A1
003400030109     C/END-EXEC
003500030109      *
003600030109     C/EXEC SQL
003700030109     C+ FETCH  next FROM A1 INTO :fgtds
003800030109     C/END-EXEC
003900030109     c                   if        sqlcod < 0
004000030109     c                   clear                   fgtds
004100030109     c                   move      '1'           error
004200030109     c                   else
004300030109     c                   movel     fgtds         dsfgtout
004400030109     c                   endif
004500030129     c                   if        dsfgtout = *blank
004600030129     c                   move      '1'           error
004700030129     c                   endif
004800030109     c
004900020624      *
005000030109     C/EXEC SQL
005100030109     C+ CLOSE A1
005200030109     C/END-EXEC
005300030109     c                   seton                                        lr
005400011129     C*--------------------------------------------------------------------
