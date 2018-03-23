000100070315      * --------------------------------------­¬
000200070329     C*? Aggiorna FIAPD nel campo:             |
000300070315      *                                        |
000400070329      * ? APDFLR                               |
000500070326      **
000600070326      **   Aggiorna le Anagrafiche impostando il campo filler sul FIAPD APDFLR...  ?
000700070326      **?  Se esiste il FIAPD30F con i Giri occorre allineare FIAPD00F: §APDFCGI ="S"
000800070329      *                                        |
000900070329     c     *entry        plist
001000070329     c                   parm                    fgs               3
001001070412     c                   clear                   compdri
001002070412     c                   movel     fgs           compdri           7 0
001003070412     c                   eval      compdrf = 9999999
001004070412     c                   movel     fgs           compdrf           7 0
001100070329
001200070326     C/EXEC SQL
001300070329     C+ UPDATE FIAPD00F SET APDFLR = substr(apdflr, 1, 6)
001400070326     C+ concat 'S' WHERE apdtip='A' and substr(apdflr, 7, 1)<>'S' and
001401070412     C+ apdpdr between :compdri and :compdrf and apdatb = ' ' and
001500070412     C+ apdpdr in (SELECT apd3pdr FROM fiapd30f WHERE APD3TIPA ='A' and
001501070412     C+ apd3atb = ' ')
001600070326     C/END-EXEC
001700070326
001800070326      **
001900070315     c                   seton                                        LR
