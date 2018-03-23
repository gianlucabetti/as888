000100090402     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200090402
000300090402      * -------------------------------------------------------------*
000400090402      * Crea file note TICPN
000500090402      * -------------------------------------------------------------*
000600090402
000900090402     fTfntc01l  if   e           k Disk
001000090402     fTicpn00f  o  a e             Disk
001100090402     d Kpjba         e ds
001104090402
001105090402     d Kapl            s                   like(ntcapl)
001106090416     d savnk1          s                   like(ntcnk1)
001107090416     d savntr          s                   like(ntcntr)
001108090402     d data_ymd        s               d   Datfmt(*ymd)
001109090402     d data_iso        s               d   Datfmt(*iso)
001110090618     d wmemo           s              2  0
001111090402
001112090402     c     Kntc01        Klist
001113090402     c                   Kfld                    Kapl
001114090402     c                   eval      kapl = 'P'
001115090402
001116090402     c     kntc01        setll     tfntc01l
001200090402     c                   Do        *hival
001300090402      * Leggo il file
001400090402     c     kntc01        Reade     tfntc01l
001500090402      * Fine file esco
001600090402     c                   If        %Eof(Tfntc01l)
001700090402     c                   Leave
001800090402     c                   EndIf
001900090402      * Leggo solo le note di tipo IN
002000090402     c                   If        ntctnt <> 'IN'
002100090402     c                   Iter
002200090402     c                   EndIf
002300090402
002301090416      * verifico se sono all'interno dello stesso potenziale / data inserimento
002302090416     c                   If        savnk1 <> ntcnk1 or savntr <> ntcntr
002303090618     c****               if        cpnnot<>*blanks
002304090618     c****               eval      CPNPNO = cpnpno + 1
002305090618     c****               write     ticpn000
002306090618     c****               endif
002307090402     c                   clear                   ticpn000
002308090618     c****               clear                   wmemo
002309090416     c                   eval      savnk1 = ntcnk1
002310090416     c                   eval      savntr = ntcntr
002311090416     c                   endif
002312090416
002313090402     c                   movel     ntCNK1        cpncpo                          Potenziale
002314090402     c     *ymd          move      ntcntr        data_iso
002401090402     c                   move      data_iso      CPNDIM
002403090417     c                   eval      CPNHIM = 235800
002407090402     c                   eval      CPNSNS = 'S'
002427090618     c****               select
002428090618     c****               when      cpnnot=*blanks
002430090618     c                   eval      CPNNOT = ntcrnt
002431090618     c****               eval      wmemo=30
002438090618     c****               other
002439090618     c****               if        wmemo=30
002441090618     c****               eval      %subst(cpnnot:61:30)=%subst(ntcrnt:1:30)
002442090618     c                   eval      CPNPNO = cpnpno + 1
002443090618     c                   write     ticpn000
002444090618     c****               eval      cpnnot=%subst(ntcrnt:31:30)
002445090618     c****               clear                   wmemo
002446090618     c****               else
002448090618     c****               move      ntcrnt        cpnnot
002449090618     c****               eval      CPNPNO = cpnpno + 1
002450090618     c****               write     ticpn000
002451090618     c****               clear                   cpnnot
002452090618     c****               endif
002453090618     c****               endsl
005700090402     c                   EndDo
005701090618      *
005702090618     c* write di fine ciclo
005703090618     c***                if        cpnnot<>*blanks
005704090618     c***                eval      CPNPNO = cpnpno + 1
005705090618     c***                write     ticpn000
005706090618     c***                endif
005800090402
005900090402     c                   Eval      *InLr = *On
