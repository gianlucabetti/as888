000100090402     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200090402
000300090402      * -------------------------------------------------------------*
000400090402      * Crea file note TICPN x i clienti
000500090402      * -------------------------------------------------------------*
000600090402
000900090402     fTfntc01l  if   e           k Disk
001000090402     fTicpn00f  o  a e             Disk
001100090402     d Kpjba         e ds
001101090402
001102090402     d Kapl            s                   like(ntcapl)
001103090416     d savnk1          s                   like(ntcnk1)
001104090416     d savntr          s                   like(ntcntr)
001105090402     d data_ymd        s               d   Datfmt(*ymd)
001106090402     d data_iso        s               d   Datfmt(*iso)
001107090618     d wmemo           s              2  0
001108090402
001109090402
001110090402     D TIBS69DS      E DS
001111090402     D ds_cnaco      E DS                  extname(CNACO00F)
001112090402     D ds_cnind      E DS                  extname(CNIND00F)
001113090402     D ds_cnclp      E DS                  extname(CNCLP00F)
001114090402     D ds_fncls      E DS                  extname(FNCLS00F)
001115090402
001116090402     c     Kntc01        Klist
001117090402     c                   Kfld                    Kapl
001118090402     c                   eval      kapl = 'C'
001119090402
001120090402     c     kntc01        setll     tfntc01l
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
002301090416
002302090416      * verifico se sono all'interno dello stesso potenziale / data inserimento
002303090416     c                   If        savnk1 <> ntcnk1 or savntr <> ntcntr
002304090618     c****               if        cpnnot<>*blanks
002305090618     c****               eval      CPNPNO = cpnpno + 1
002306090618     c****               write     ticpn000
002307090618     c****               endif
002308090416     c                   clear                   ticpn000
002309090618     c****               clear                   wmemo
002310090416     c                   eval      savnk1 = ntcnk1
002311090416     c                   eval      savntr = ntcntr
002312090416     c                   endif
002313090416      * cerchiamo il cliente quando diverso
002314090402     C                   clear                   TIBS69DS
002315090402     C                   move      ntcnk1        I69kac
002316090402     C                   call      'TIBS69R'
002317090402     C                   parm                    tibs69DS
002318090402     C                   parm                    ds_cnaco
002319090402     C                   parm                    ds_cnind
002320090402     C                   parm                    ds_cnclp
002321090402     C                   parm                    ds_fncls
002322090402     C                   IF        O69ERR <> ' '
002323090402     C                   ITER
002324090402     C                   ENDIF
002325090402     c                   z-add     acolib        cpncpo                          Potenziale
002326090402     c                   z-add     acoksc        cpnksc                          Cliente
002327090402     c     *ymd          move      ntcntr        data_iso
002401090402     c                   move      data_iso      CPNDIM
002403090402     c                   eval      CPNHIM = 235900
002407090402     c                   eval      CPNSNS = 'S'
002408090618     c****               select
002409090618     c****               when      cpnnot=*blanks
002410090618     c                   eval      CPNNOT = ntcrnt
002411090618     c****               eval      wmemo=30
002412090618     c****               other
002413090618     c****               if        wmemo=30
002414090618     c****               eval      %subst(cpnnot:61:30)=%subst(ntcrnt:1:30)
002415090618     c                   eval      CPNPNO = cpnpno + 1
002416090618     c                   write     ticpn000
002417090618     c***                eval      cpnnot=%subst(ntcrnt:31:30)
002418090618     c****               clear                   wmemo
002419090618     c****               else
002420090618     c****               move      ntcrnt        cpnnot
002421090618     c****               eval      CPNPNO = cpnpno + 1
002422090618     c****               write     ticpn000
002423090618     c****               clear                   cpnnot
002424090618     c****               endif
002425090618     c****               endsl
005700090402     c                   EndDo
005701090618      *
005702090618     c* write di fine ciclo
005703090618     c****               if        cpnnot<>*blanks
005704090618     c****               eval      CPNPNO = cpnpno + 1
005705090618     c****               write     ticpn000
005706090618     c****               endif
005800090402
005900090402     c                   Eval      *InLr = *On
