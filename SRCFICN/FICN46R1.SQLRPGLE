000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300020624      *
000400020624     D param           DS
000500020624     D  datadal                       8s 0
000600020624     D  dataal                        8s 0
000700020624     D*
000800011026
000900000000     C*---------------------------------------------------------------*
001000020624     c     *entry        plist
001100030113     c                   parm                    param
001200020624
001300020313     C/EXEC SQL
001400030519     C+ delete from FIFsA00F where fsaddc between :datadal and :dataal
001500020313     C/END-EXEC
001600020624      *
001700020624     c                   seton                                        lr
001800011129     C*--------------------------------------------------------------------
