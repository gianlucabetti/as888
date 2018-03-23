000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020624      *
000500020624     D param           DS
000600020624     D  datadal                       8s 0
000700020624     D  dataal                        8s 0
000800020624     D*
000900011026
001000000000     C*---------------------------------------------------------------*
001100020624     c     *entry        plist
001200030113     c                   parm                    param
001300020624
001400020313     C/EXEC SQL
001500030113     C+ delete from FIFsc00F where fscddc between :datadal and :dataal
001600020313     C/END-EXEC
001700020624      *
001800020624     c                   seton                                        lr
001900011129     C*--------------------------------------------------------------------
