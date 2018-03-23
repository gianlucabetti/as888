000100090402     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200090402
000300090402      * -------------------------------------------------------------*
000400100215      * Crea file stati TICPS
000500090402      * -------------------------------------------------------------*
000600090402
000700100215     fTncps01l  if   e           k Disk
000800100215     fTicps00f  o  a e             Disk
000900090402     d Kpjba         e ds
001000090402
001100090402     d data_ymd        s               d   Datfmt(*ymd)
001200090402     d data_iso        s               d   Datfmt(*iso)
001300090402
001400090402     c     Kntc01        Klist
001500090402     c                   Kfld                    Kapl
001600090402     c                   eval      kapl = 'P'
001700090402
001800090402     c     kntc01        setll     tfntc01l
001900090402     c                   Do        *hival
002000090402      * Leggo il file
002100090402     c     kntc01        Reade     tfntc01l
002200090402      * Fine file esco
002300090402     c                   If        %Eof(Tfntc01l)
002400090402     c                   Leave
002500090402     c                   EndIf
002600090402      * Leggo solo le note di tipo IN
002700090402     c                   If        ntctnt <> 'IN'
002800090402     c                   Iter
002900090402     c                   EndIf
003000090402
003100090416      * verifico se sono all'interno dello stesso potenziale / data inserimento
003200090416     c                   If        savnk1 <> ntcnk1 or savntr <> ntcntr
003300090618     c****               if        cpnnot<>*blanks
003400090618     c****               eval      CPNPNO = cpnpno + 1
003500090618     c****               write     ticpn000
003600090618     c****               endif
003700090402     c                   clear                   ticpn000
003800090618     c****               clear                   wmemo
003900090416     c                   eval      savnk1 = ntcnk1
004000090416     c                   eval      savntr = ntcntr
004100090416     c                   endif
004200090416
004300090402     c                   movel     ntCNK1        cpncpo                          Potenziale
004400090402     c     *ymd          move      ntcntr        data_iso
004500090402     c                   move      data_iso      CPNDIM
004600090417     c                   eval      CPNHIM = 235800
004700090402     c                   eval      CPNSNS = 'S'
004800090618     c****               select
004900090618     c****               when      cpnnot=*blanks
005000090618     c                   eval      CPNNOT = ntcrnt
005100090618     c****               eval      wmemo=30
005200090618     c****               other
005300090618     c****               if        wmemo=30
005400090618     c****               eval      %subst(cpnnot:61:30)=%subst(ntcrnt:1:30)
005500090618     c                   eval      CPNPNO = cpnpno + 1
005600090618     c                   write     ticpn000
005700090618     c****               eval      cpnnot=%subst(ntcrnt:31:30)
005800090618     c****               clear                   wmemo
005900090618     c****               else
006000090618     c****               move      ntcrnt        cpnnot
006100090618     c****               eval      CPNPNO = cpnpno + 1
006200090618     c****               write     ticpn000
006300090618     c****               clear                   cpnnot
006400090618     c****               endif
006500090618     c****               endsl
006600090402     c                   EndDo
006700090618      *
006800090618     c* write di fine ciclo
006900090618     c***                if        cpnnot<>*blanks
007000090618     c***                eval      CPNPNO = cpnpno + 1
007100090618     c***                write     ticpn000
007200090618     c***                endif
007300090402
007400090402     c                   Eval      *InLr = *On
