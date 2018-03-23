000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200150504      *-----------------------------------------------------------
000300150504     d KPJBA         e ds
000400150504     d fieu84ds      e ds
000500150504      *-----------------------------------------------------------
000600150504     c     *entry        PLIST
000700150504     c                   parm                    kpjba
000800150504      *
000900150504     c                   eval       fieu84ds = kpjbu
001000150504      *
001100150504      *  creazione del file
001200150504     c                   if        e84FPC = 'S'
001300130722     C/EXEC SQL
001400150504     c+   delete from WFIEU85F where F85UTE = :Knmus
001500130722     C/END-EXEC
001600151105      *
001700151105     C/EXEC SQL
001800151105     c+   delete from WFNODPDF where N85UTE = :Knmus
001900151105     C/END-EXEC
002000151105      *
002100151105     C/EXEC SQL
002200151105     C+ INSERT INTO GAITRAAZM/WFNODPDF SELECT F85NTW, F85DES, F85RIF,
002300151105     C+ F85SPB, F85AAS, F85LNP, F85NRS, F85NSP, F85DTS, F85LNA, F85TPB,
002400151105     C+ F85FBR, F85IMC, F85IMR, F85TDE, F85UTE, F85AL, CURDATE() FROM
002500151105     C+ wfieu85f WHERE F85NTW = 'DPD' and F85UTE = :KNMUS and F85GRP =
002600151105     C+ '001' and (F85DES like '(5)%' or F85DES like '(8)%') ORDER BY
002700151105     C+ f85des, f85rif
002800151105     C/END-EXEC
002900150504     c                   end
003000150504      *
003100150504     c                   seton                                        LR
003200150504      *-----------------------------------------------------------
