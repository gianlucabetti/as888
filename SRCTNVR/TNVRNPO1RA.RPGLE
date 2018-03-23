000100090402     h Decedit('0,') Datedit(*dmy/) Option(*nodebugio)
000200090402
000300090402      * -------------------------------------------------------------*
000400090402      * Crea file note TICPN
000500090402      * -------------------------------------------------------------*
000600090402
000900090402     fTfntc01l  if   e           k Disk
001000090402     fTicpn00f  o  a e             Disk
001100090402     d Kpjba         e ds
001101090402
001102090402     d Kapl            s                   like(ntcapl)
001103090402     d data_ymd        s               d   Datfmt(*ymd)
001104090402     d data_eur        s               d   Datfmt(*eur)
001105090402
001106090402     c     Kntc01        Klist
001107090402     c                   Kfld                    Kapl
001108090402     c                   eval      kapl = 'P'
001109090402
001110090402     c     kntc01        setll     tfntc01l
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
002301090402     c                   clear                   ticpn000
002302090402     c                   eval      CPNCPO = ntCNK1                              Potenziale
002303090402     c     *ymd          move      ntcntr        data_eur
002401090402     c                   move      data_eur      CPNDIM
002403090402     c                   eval      CPNHIM = 235900
002404090402     c                   eval      CPNPNO = 1
002406090402     c                   eval      CPNNOT = %subst(ntcrnt: 1: 35)
002407090402     c                   eval      CPNSNS = 'S'
002408090402     c                   eval      CPNPOS = 046
002409090402     c                   eval      CPNPRU = 'EDPSEDE'
002411090402     c                   write     ticpn000
002412090402     c                   if        %subst(ntcrnt: 36)  <> *blanks
002413090402     c                   eval      CPNPNO = 2
002414090402     c                   eval      CPNNOT = %subst(ntcrnt: 36)
002415090402     c                   write     ticpn000
002416090402     c                   endif
005700090402     c                   EndDo
005800090402
005900090402     c                   Eval      *InLr = *On
