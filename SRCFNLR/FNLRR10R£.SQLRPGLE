000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200051011     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000300000000     H*--------------------------------------------------------------*
000400051011     ftabel00f  if   e           k disk
000500020624      *
000600020624     d kpjba         e ds
000700051011     d ds5a2         e ds
000800020624     D param           DS
000900051011     D dataiso         S               d   datfmt(*iso)
001000051011     D data            s              8s 0
001100020624     D*
001200011026
001300000000     C*---------------------------------------------------------------*
001400020624     c     *entry        plist
001500020624     c                   parm                    kpjba
001600020624     c                   movel     kpjbu         param
001700051011
001800051011     c     ktab          klist
001900051011     c                   kfld                    tblkut
002000051011     c                   kfld                    tblcod
002100051011     c                   kfld                    tblkey
002200051011     c                   z-add     1             tblkut
002300051011     c                   move      '5A'          tblcod
002400051011     c                   movel(p)  '2'           tblkey
002500051011
002600051011     c                   clear                   ds5a2
002700051011     c     ktab          chain     tabel00f
002800051011     c                   if        %found(tabel00f)
002900051011     c                   movel     tbluni        ds5a2
003000051011     c                   endif
003100051011     C                   move      *date         dataiso
003200051011     c     dataiso       subdur    §5arie:*d     dataiso
003300051011     c                   move      dataiso       data
003400020624
003500051107     C/EXEC SQL
003600051107     C+ delete from FIqdt00F where qdtddc = 99999999
003700051107     C/END-EXEC
003800051107      *
003900020313     C/EXEC SQL
004000051011     C+ delete from FIqdt00F where qdtddc < :data
004100020313     C/END-EXEC
004200020515      *
004300051011     C/EXEC SQL
004400051011     C+ delete from FIqdc00F where qdcddc < :data
004500051011     C/END-EXEC
004600051011      *
004700051011     C/EXEC SQL
004800051011     C+ delete from FIqdo00F where qdoddc < :data
004900051011     C/END-EXEC
005000051011      *
005100051011     C/EXEC SQL
005200051011     C+ delete from FIqbt00F where qbtddc < :data
005300051011     C/END-EXEC
005400051011      *
005500020624     c                   seton                                        lr
005600011129     C*--------------------------------------------------------------------
