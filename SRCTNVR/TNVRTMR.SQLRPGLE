000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
006801080424     c*
006802060906     C/EXEC SQL
006804080424     C+ UPDATE GAITRAGRU/TABEL00F SET TBLUNI = Substr(TBLUNI, 1, 50)
006805080424     C+ concat 'N' concat substr(TBLUNI, 52), tblftr = ' '
006806080424     C+ WHERE TBLCOD = 'TM' AND
006807080424     C+ Substr(TBLUNI, 51, 1) = ' '
006809060906     C/END-EXEC
006810080424     C/EXEC SQL
006811080424     C+ UPDATE filtragru/TABEL00F SET TBLUNI = Substr(TBLUNI, 1, 50)
006812080424     C+ concat 'N' concat substr(TBLUNI, 52) WHERE TBLCOD = 'TM' AND
006813080424     C+ Substr(TBLUNI, 51, 1) = ' '
006815080424     C/END-EXEC
006816060905     c                   seton                                        lr
