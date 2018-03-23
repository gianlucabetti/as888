000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000101080307
006802060906     C/EXEC SQL
006804080307     C+ UPDATE FILTRA201/FIDST00F SET DSTPDA = 'N' WHERE (dstpda <> 'N'
006805080307     C+ and dstfgs <> 1) or (dstpda not in ('O', 'N') and dstfgs = 1)
006812060906     C/END-EXEC
006813060906     C/EXEC SQL
006814080307     C+ UPDATE UNITRAGRU/FIAPD00F SET APDFLR = substr(apdflr, 1, 7)
006815080307     C+ concat 'N' WHERE apdtip = 'A' and ((substr(apdflr, 8, 1) <> 'N'
006816080307     C+ and apdpdr > 0019999) or (substr(apdflr, 8, 1) not in ('N', 'O')
006817080307     C+ and apdpdr <= 0019999))
006818060906     C/END-EXEC
006819060905     c                   seton                                        lr
