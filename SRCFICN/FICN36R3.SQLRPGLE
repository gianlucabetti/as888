000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     D param           DS
000600030109     D  data                          8s 0
000700030206     D  codice                        7s 0
000800030109     D  tipopre                       1
000900030109     d  dsfgtout                           like(fgtds)
001000030109     d  error                         1
001100030109
001200030109     d fgtds         e ds                  extname(fifgt00f)
001300030109
001400020624     D*
001500011026
001600000000     C*---------------------------------------------------------------*
001700020624     c     *entry        plist
001800030109     c                   parm                    param
001900030109     c                   clear                   dsfgtout
002000030109
002100020624
002200020313     C/EXEC SQL
002300030121     C+ DECLARE A1 CURSOR FOR SELECT * FROM fifgt00F WHERE fgttsr =
002400030521     C+ :tipopre and fgtpdr = :codice and fgtddt <= :data and fgtdst >=
002500030521     C+ :data and fgtatb = ' ' and FGTDCN <> 0
002600020313     C/END-EXEC
002700030109      *
002800030109     C/EXEC SQL
002900030109     C+ OPEN A1
003000030109     C/END-EXEC
003100030109      *
003200030109     C/EXEC SQL
003300030109     C+ FETCH  next FROM A1 INTO :fgtds
003400030109     C/END-EXEC
003500030109     c                   if        sqlcod < 0
003600030109     c                   clear                   fgtds
003700030109     c                   move      '1'           error
003800030109     c                   else
003900030109     c                   movel     fgtds         dsfgtout
004000030109     c                   endif
004100030129     c                   if        dsfgtout = *blank
004200030129     c                   move      '1'           error
004300030129     c                   endif
004400030109     c
004500020624      *
004600030109     C/EXEC SQL
004700030109     C+ CLOSE A1
004800030109     C/END-EXEC
004900030109     c                   seton                                        lr
005000011129     C*--------------------------------------------------------------------
