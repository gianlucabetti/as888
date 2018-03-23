013800101011       // -?Parametri ricevuti
013900101011     d KPJBA         e ds
013901160212     D TIBS55        E DS                  EXTNAME(TIBS55DS)
013904160216     d fNLVP53DS     e ds
014000100507
023100101011     c     *Entry        plist
023200101011     c                   parm                    KPJBA
023300160216     c                   movel     kpjbu         fNLVP53DS
023400160212     C/EXEC SQL
023401160216     C+ DELETE FROM GAITRAAZM/WFNLVP52F
023402160212     C/END-EXEC
023403160212
023404160212     c                   if        f53po = 999
023405160212     C/EXEC SQL
023500160216     C+ insert into gaitraazm/WfNLVP52F
023501160216     C+ (wfgs, wdes, wddc, wpda, wnogeo, wsigeo )
023503160216     C+ SELECT plgfgs, orgdes, data , pda, SUM(A) nogeo, SUM(B) sigeo FROM
023504160216     C+ (SELECT plgfgs, substr(plgdatora, 1, 8) data , CASE WHEN
023505160216     C+ substr(plgiddisp, 1, 1) < '0' then 'D' else 'M' end pda, CASE WHEN
023506160216     C+ substr(plgdati, 220, 16) in ( '                ',
023507160216     C+ '0000000000000000') THEN 1 ELSE 0 END AS A , CASE WHEN
023508160216     C+ substr(plgdati, 220, 16) not in ( '                ',
023509160216     C+ '0000000000000000') THEN 1 ELSE 0 END AS B FROM fiplg00f where
023510160216     C+ plgtiporcd = 'CET' and substr(plgdatora, 1, 8) between :f53dti
023511160216     C+ and :f53dtf) AS TABELLA join azorg00f on plgfgs=orgfil GROUP BY
023512160216     C+ plgfgs, orgdes, data, pda order by plgfgs, orgdes, data, pda
023900160212     C/END-EXEC
023901160212     c                   else
023902160216     C/EXEC SQL
023903160216     C+ insert into gaitraazm/WfNLVP52F
023904160216     C+ (wfgs, wdes, wddc, wpda, wnogeo, wsigeo )
023905160216     C+ SELECT plgfgs, orgdes, data , pda, SUM(A) nogeo, SUM(B) sigeo FROM
023906160216     C+ (SELECT plgfgs, substr(plgdatora, 1, 8) data , CASE WHEN
023907160216     C+ substr(plgiddisp, 1, 1) < '0' then 'D' else 'M' end pda, CASE WHEN
023908160216     C+ substr(plgdati, 220, 16) in ( '                ',
023909160216     C+ '0000000000000000') THEN 1 ELSE 0 END AS A , CASE WHEN
023910160216     C+ substr(plgdati, 220, 16) not in ( '                ',
023911160216     C+ '0000000000000000') THEN 1 ELSE 0 END AS B FROM fiplg00f where
023912160216     C+ plgtiporcd = 'CET' and substr(plgdatora, 1, 8) between :f53dti
023913160216     C+ and :f53dtf and plgfgs = :f53po)
023914160216     C+ AS TABELLA join azorg00f on plgfgs=orgfil GROUP BY
023915160216     C+ plgfgs, orgdes, data, pda order by plgfgs, orgdes, data, pda
023916160216     C/END-EXEC
023917160212     c                   end
023918160212     c                   seton                                        lr
