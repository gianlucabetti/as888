000800071008      * copia i valori del da assegnare delle consegne x i RITIRI (FIAPD20F)
000801071008      * e duplica i giri della pianificazione consegne x i RITIRI (FIAPD50F)
000802071009     d date            s               d   inz(*sys)
000803071009     d oggi            s              8  0
000900070329     c     *entry        plist
001000071009     c                   parm                    fgs               3
001001071009     c     *iso          move      date          oggi
001002070412     c                   clear                   compdri
001003071009     c                   move      fgs           comfgs            3 0
001004071009     c                   movel     fgs           compdri           7 0
001005070412     c                   eval      compdrf = 9999999
001006070412     c                   movel     fgs           compdrf           7 0
001100070329
001200070326     C/EXEC SQL
001300071009     C+ UPDATE FIAPD20F SET APD2RPKL = apd2pkl, APD2RVLU =
001301071008     C+ apd2vlu, APD2RNFTL = apd2nftl, APD2RNBL = apd2nbl, APD2RTOL =
001302071009     C+ apd2tol, APD2RINC = apd2inc, APD2RSPI = 'N' WHERE apd2tipa='A' and
001400071009     C+ apd2pdr between :compdri and :compdrf and apd2atb = ' '
001401071009     C+ and APD2DSCAD > :oggi and apd2rpkl = 0 and apd2rvlu = 0 and
001402071009     C+ APD2rnftl = 0 and apd2rnbl = 0 and
001403071018     C+ apd2rtol = 0
001600070326     C/END-EXEC
001700070326
001701071009     C/EXEC SQL
001702071009     C+ INSERT INTO  FIAPD50F SELECT * FROM fiapd30f WHERE
001704071009     C+ apd3tipa='A' and apd3pdr between :compdri and :compdrf and
001705071009     C+ apd3dscad > :oggi and
001706071009     C+ apd3atb = ' ' and apd3cgi in ( SELECT GRGCGI FROM figrg00f WHERE
001707071009     C+ substr(GRGFLO, 4, 1) <> 'C' and GRGFGS = :comfgs and grgatb = ' '
001708071009     C+ and grgdscad >:oggi)
001709071009     C/END-EXEC
001800070326      **
001900070315     c                   seton                                        LR
