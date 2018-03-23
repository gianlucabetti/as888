000100020326     H DECEDIT('0,') DATEDIT(*DMY.)
000200020603     D cmd             s             70    ctdata dim(4)                        FIL. COMODO
000300020326     D WrkSqlCmd       S           1024
000400020603     c                   EVAL      WRKSQLCMD = CMD(1)+CMD(2)+CMD(3)+cmd(4)
000500020405     C/EXEC SQL
000600020405     C+ EXECUTE IMMEDIATE :WrkSqlCmd
000700020405     C/END-EXEC
000800020603     c*
000900020326     C                   SETON                                        LR
001000020326**
001100020603update fnar400f
001200020603set   ar4not = 'Y00000000000000 00000000   00000000'
001300020603where ar4not = 'Y                                  '
001400020603and ar4trc = 'A'
