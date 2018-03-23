000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200150504      *-----------------------------------------------------------
000300150504     d KPJBA         e ds
000400150504     d fieu84ds      e ds
000500150504      *-----------------------------------------------------------
000600150504     c     *entry        PLIST
000700150504     c                   parm                    kpjba
000800150504      *
000900150504     c                   eval       fieu84ds = kpjbu
001000160627     C/EXEC SQL
001100160627     c+   delete from WFIEU85NT where N85UTE = :Knmus
001200160627     C/END-EXEC
001300160627      *
001400150504      *  creazione del file
001500150504     c                   if        e84FPC = 'S'
001600130722     C/EXEC SQL
001700150504     c+   delete from WFIEU85F where F85UTE = :Knmus
001800130722     C/END-EXEC
001900151105      *
002000151105     C/EXEC SQL
002100151105     c+   delete from WFNODPDF where N85UTE = :Knmus
002200151105     C/END-EXEC
002300151105      *
002400150504     c                   end
002500150504      *
002600150504     c                   seton                                        LR
002700150504      *-----------------------------------------------------------
