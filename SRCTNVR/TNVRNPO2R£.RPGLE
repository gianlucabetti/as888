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
001107090402
001108090402
001109090402     D TIBS69DS      E DS
001110090402     D ds_cnaco      E DS                  extname(CNACO00F)
001111090402     D ds_cnind      E DS                  extname(CNIND00F)
001112090402     D ds_cnclp      E DS                  extname(CNCLP00F)
001113090402     D ds_fncls      E DS                  extname(FNCLS00F)
001114090402
001115090402     c     Kntc01        Klist
001116090402     c                   Kfld                    Kapl
001117090402     c                   eval      kapl = 'C'
001118090402
001119090402     c     kntc01        setll     tfntc01l
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
002304090416     c                   clear                   ticpn000
002305090416     c                   eval      savnk1 = ntcnk1
002306090416     c                   eval      savntr = ntcntr
002307090416     c                   endif
002309090416      * cerchiamo il cliente quando diverso
002311090402     C                   clear                   TIBS69DS
002312090402     C                   move      ntcnk1        I69kac
002313090402     C                   call      'TIBS69R'
002314090402     C                   parm                    tibs69DS
002315090402     C                   parm                    ds_cnaco
002316090402     C                   parm                    ds_cnind
002317090402     C                   parm                    ds_cnclp
002318090402     C                   parm                    ds_fncls
002319090402     C                   IF        O69ERR <> ' '
002320090402     C                   ITER
002321090402     C                   ENDIF
002322090402     c                   z-add     acolib        cpncpo                          Potenziale
002323090402     c                   z-add     acoksc        cpnksc                          Cliente
002324090402     c     *ymd          move      ntcntr        data_iso
002401090402     c                   move      data_iso      CPNDIM
002403090402     c                   eval      CPNHIM = 235900
002404090416     c                   eval      CPNPNO = CPNPNO +1
002406090402     c                   eval      CPNNOT = %subst(ntcrnt: 1: 30)
002407090402     c                   eval      CPNSNS = 'S'
002411090402     c                   write     ticpn000
002412090402     c                   if        %subst(ntcrnt: 31)  <> *blanks
002413090416     c                   eval      CPNPNO = CPNPNO +1
002414090402     c                   eval      CPNNOT = %subst(ntcrnt: 31)
002415090402     c                   write     ticpn000
002416090402     c                   endif
005700090402     c                   EndDo
005800090402
005900090402     c                   Eval      *InLr = *On
