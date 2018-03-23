000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200051011     H DEBUG DECEDIT('0,') DATEDIT(*YMD.)
000300000000     H*--------------------------------------------------------------*
000400051011     ftabel00f  if   e           k disk
000500020624      *
000600020624     d kpjba         e ds
000700150804     d ds5a4         e ds
000800051011     D dataiso         S               d   datfmt(*iso)
000900051011     D data            s              8s 0
001000020624     D*
001100011026
001200000000     C*---------------------------------------------------------------*
001300020624     c     *entry        plist
001400020624     c                   parm                    kpjba
001500051011
001600051011     c     ktab          klist
001700051011     c                   kfld                    tblkut
001800051011     c                   kfld                    tblcod
001900051011     c                   kfld                    tblkey
002000051011     c                   z-add     1             tblkut
002100051011     c                   move      '5A'          tblcod
002200150804     c                   movel(p)  '4'           tblkey
002300051011
002400150804     c                   clear                   ds5a4
002500051011     c     ktab          chain     tabel00f
002600051011     c                   if        %found(tabel00f)
002700150804     c                   movel     tbluni        ds5a4
002800051011     c                   endif
002900051011     C                   move      *date         dataiso
003000051011     c     dataiso       subdur    §5arie:*d     dataiso
003100051011     c                   move      dataiso       data
003200020624
003300051107     C/EXEC SQL
003400051107     C+ delete from FIqdt00F where qdtddc = 99999999
003500051107     C/END-EXEC
003600051107      *
003700020313     C/EXEC SQL
003800051011     C+ delete from FIqdt00F where qdtddc < :data
003900020313     C/END-EXEC
004000020515      *
004100051011     C/EXEC SQL
004200051011     C+ delete from FIqdc00F where qdcddc < :data
004300051011     C/END-EXEC
004400051011      *
004500051011     C/EXEC SQL
004600051011     C+ delete from FIqdo00F where qdoddc < :data
004700051011     C/END-EXEC
004800051011      *
004900051011     C/EXEC SQL
005000051011     C+ delete from FIqbt00F where qbtddc < :data
005100051011     C/END-EXEC
005200051011      *
005300020624     c                   seton                                        lr
005400011129     C*--------------------------------------------------------------------
