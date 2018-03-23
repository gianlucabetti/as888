000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200020409     h*****************************************************************
000300040528     h* Il pgm pulisce i progressivi obsoleti di FIAR500F tipo record TRS
000400020409     h*****************************************************************
000500040218     c*
000600031124     C/EXEC SQL
000700040528     C+ DELETE FROM fiar500f a WHERE a.ar5trd = 'TRS' and exists
000800040528     C+ (SELECT b.ar5aas, b.ar5lnp, b.ar5nrs, b.ar5nsp, count(*),
000900040528     C+ max(b.ar5prg) FROM fiar500f b WHERE b.ar5trd = 'TRS' GROUP BY
001000040528     C+ b.ar5aas, b.ar5lnp, b.ar5nrs, b.ar5nsp HAVING count(*) > 1
001100040528     C+ and a.ar5aas=b.ar5aas and a.ar5lnp=b.ar5lnp and a.ar5nrs=b.ar5nrs
001200040528     C+ and a.ar5nsp=b.ar5nsp and a.ar5prg < max(b.ar5prg))
001300040528     C/END-EXEC
001400040218     c*
001500030213     C                   seton                                        lr
