013800101011       // -?Parametri ricevuti
013900101011     d KPJBA         e ds
013901160212     D TIBS55        E DS                  EXTNAME(TIBS55DS)
013902160212     D OVRTAS          S              1    DIM(39) CTDATA PERRCD(39)
013903160212     D Ctas            C                   CONST('/FIARG00F)')
013904160212     d fidg53ds      e ds
014000100507
023100101011     c     *Entry        plist
023200101011     c                   parm                    KPJBA
023300160212     c                   movel     kpjbu         fidg53ds
023301160212     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
023302160212     C* da/a cui spedire
023303160212     C                   CLEAR                   TIBS55
023304160212     C                   MOVEL     'L'           I50TLA
023306160212     C                   MOVEL     '001'         I50PPO
023307160212     C                   MOVEL     '001'         I50APO
023308160212     C                   CALL      'TIBS55R'
023309160212     C                   PARM                    TIBS55
023310160212     c                   movea     o50ala        ovrtas(30)
023311160212     C                   SETOFF                                       01
023312160212     C                   CLEAR                   COMMAN           80
023313160212     C                   MOVEA     ovrtas        COMMAN
023314160212     c                   eval      comman=%trim(comman)+ctas
023315160212     C                   CALL      'QCMDEXC'
023316160212     C                   PARM                    COMMAN
023317160212     C                   PARM      50            LUNG             15 5
023400160212     C/EXEC SQL
023401160212     C+ DELETE FROM GAITRAAZM/WFIDG52F
023402160212     C/END-EXEC
023403160212
023404160212     c                   if        f53po = 999
023405160212     C/EXEC SQL
023500160212     C+ insert into gaitraazm/wfidg52f
023501160212     C+ (wfgs, wdes, wddc, wman, waut )
023503160212     C+ SELECT argfgs, orgdes, argddc, SUM(A) manuale , SUM(B) automatic
023504160212     C+ FROM (SELECT argfgs, argddc, case when argnftl=99999 THEN 1 ELSE 0
023505160212     C+ END AS A, CASE WHEN argnftl between 1 and 99998 THEN 1 ELSE 0 END
023506160212     C+ AS B FROM fiarg00f where argtgi<> ' ' and argndc <> 0 and
023507160212     C+ argddc between :f53dti and :f53dtf) as tabella join azorg00f on
023508160212     C+ argfgs = orgfil group by argfgs, orgdes, argddc
023900160212     C/END-EXEC
023901160212     c                   else
023902160212     C/EXEC SQL
023903160212     C+ insert into gaitraazm/wfidg52f
023904160212     C+ (wfgs, wdes, wddc, wman, waut )
023905160212     C+ SELECT argfgs, orgdes, argddc, SUM(A) manuale , SUM(B) automatic
023906160212     C+ FROM (SELECT argfgs, argddc, case when argnftl=99999 THEN 1 ELSE 0
023907160212     C+ END AS A, CASE WHEN argnftl between 1 and 99998 THEN 1 ELSE 0 END
023908160212     C+ AS B FROM fiarg00f where argtgi<> ' ' and argndc <> 0 and
023909160212     C+ argfgs= :f53po and
023910160212     C+ argddc between :f53dti and :f53dtf) as tabella join azorg00f on
023911160212     C+ argfgs = orgfil group by argfgs, orgdes, argddc
023912160212     C/END-EXEC
023913160212     c                   end
023914160212     c                   seton                                        lr
024000160212**         OVRtas
024100160212OVRDBF FILE(FIARG00F) TOFILE(XXXXXXXXXX
