000100080923     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000200020318     ftntbe01l  if   e           k disk
000300080922     ffiapd01l  if   e           k disk
000400080919     D ficn74ds      e ds
000500080922     D dapdflr       e ds
000600080922     D dgpi          e ds
000700080922     D dgpm          e ds
000800020318     d kcod            s                   like(tbecod)
000900020318     d key1            s                   like(tbeke1)
001000020318     d key2            s                   like(tbeke2)
001100040309     D* DS PER TRUL06R - CARICAMENTO £1
001200040309     D kpjba         E DS
001300040309     D savkpjbu        s                   like(kpjbu)
001400080919     d dataiso         s               d   datfmt(*iso)
001500000607      *--------------------------------------------------------------------
001600000607     c
001700000607     c     *entry        plist
001800080919     c                   parm                    ficn74ds
001900080922     c                   eval      *inlr = *on
002000020318
002100020318     c     ktbe          klist
002200020318     c                   kfld                    kcod
002300020318     c                   kfld                    key1
002400020318     c                   kfld                    key2
002500080919     c                   eval      kcod = 'GPI'
002600080922      *
002700080922     c     kapd          klist
002800080922     c                   kfld                    I74TIP
002900080922     c                   kfld                    I74PDR
003000080922      * reperimento prezzi di riferimento
003100080922     c                   exsr      repprz
003200080922      * reperimento massa automezzo
003300080922     c                   exsr      repmassa
003400080922
003500080922      * calcolo eventuale percentuale di aumento
003600080922     c                   exsr      calcperc
003700080922
003800080922      * calcolo importi aumento
003900080922     c                   exsr      calcimpor
004000080922
004100080922     c                   return
004200080922     c
004300080922      *_________________________________________________________________
004400080922     c     repprz        begsr
004500080922      *_________________________________________________________________
004600080919      *cerco prezzo di ingresso carburante
004700080919     c                   eval      key1 = '1'
004800080919     c                   eval      key2 = *blank
004900020318     c     ktbe          chain     tntbe01l                           99
005000020318     c                   if        not *in99
005100080919     c                   movel     tbeuni        dgpi
005200080922     c                   eval      o74gpi = D§GPIGPI
005300080919     c                   endif
005400080922      *prelevo prezzo riferimento mese precedente
005500080919     c                   move      i74dtpre      dataiso
005600080919     c     dataiso       subdur    1:*m          dataiso
005700080922     c                   movel     dataiso       data8             8 0
005800080922     c                   movel     data8         data6             6 0
005900080919     c                   eval      kcod = 'GPM'
006000080922     c                   movel     data6         key1
006100080922     c                   eval      key2 = *blank
006200080919     c     ktbe          chain     tntbe01l                           99
006300080919     c                   if        not *in99
006400080919     c                   movel     tbeuni        dgpm
006500080922     c                   eval      o74gpm = D§GPmGPm
006600080919     c                   endif
006700080922     c                   endsr
006800080922      *_________________________________________________________________
006900080922     c     repmassa      begsr
007000080922      *_________________________________________________________________
007100080922      *cerco massa e scaglione percentuale appartenenza x calcolo (10-20-30%)
007200080922     c*per traini forza il massimo
007300080922     c                   select
007400080922     c                   when      i74tip = 'T'
007500080922     c                   z-add     30            o74perc
007600080922     c                   z-add     20000         o74mcp
007700080922     c                   other
007800080922     c* ricerco su anagrafica i dati della massa se non sono già stati passati
007900080922      * se li ho già cerco solo lo scaglione di appartenenza
008000080922     c                   if        i74mcp = 0
008100080922     c     kapd          chain     fiapd01l
008200080922     c                   if        %found(fiapd01l)
008300080922     c                   movel     apdflr        dapdflr
008400080922      *non trovata massa su anagrafica autista
008500080922     c                   if        §APDMCP = 0
008600080922     c                   eval      o74err =  'E'
008700080922     c                   eval      o74msg = 'Non trovata massa su anagrafica -
008800080922     c                             Autista'
008900080922     c                   return
009000080922     c                   else
009100080922     c                   eval      o74mcp = §APDMCP
009200080922     c                   endif
009300080922      *non trovata anagrafica
009400080922     c                   else
009500080922     c                   eval      o74err =  'E'
009600080922     c                   eval      o74msg = 'Non trovata anagrafica -
009700080922     c                             Autista'
009800080922     c                   return
009900080922     c                   endif
010000080922     c                   else
010100080922      *parametro già ricevuto in input
010200080922     c                   eval      o74mcp = i74mcp
010300080922     c                   endif
010400080922     c* ricavo scaglione percentuale aumento (i numeri si riferisco a Kg)
010401081003     c                   select
010500081003     c                   when      o74mcp >= 20000
010501081003     c                   eval      o74perc = 30
010502081003     c                   when      o74mcp >= 3500 and o74mcp < 20000
010600080922     c                   eval      o74perc = 20
010700081003     c                   other
010800080922     c                   eval      o74perc = 10
010900081003     c                   endsl
011000080922
011100080922     c                   endsl
011200080922     c                   endsr
011300080922      *_________________________________________________________________
011400080922     c     calcperc      begsr
011500080922      *_________________________________________________________________
011600080922     c                   if        o74gpm <> 0 and o74gpi <> 0
011700081002     c                   eval(h)   o74percau= (((o74gpm - o74gpi) /o74gpi)* 100)
011800080922     c                   eval      o74percad= ((o74percau * o74perc) / 100)
011900080922     c                   else
012000080922     c                   eval      o74err =  'E'
012100080922     c                   eval      o74msg = 'Manca valore prezzo medio o prezzo-
012200080922     c                              di ingresso'
012300080922     c                   return
012400080922     c                   endif
012500080922     c                   endsr
012600080922      *_________________________________________________________________
012700080922     c     calcimpor     begsr
012800080922      *_________________________________________________________________
012900080922
013000080922      * calcolo importo su cui calcolare l'aumento
013100080922     c                   eval      O74IMPCO = ((i74imp * o74perc) / 100)
013200080922      * calcolo l'ammontare dell'aumento solo se la percentuale è superiore al 2%
013300080922     c                   if        o74percau > 2
013400080922     c                   eval      o74impa = ((o74impco * o74percau) / 100)
013500080922     c                   endif
013600080922     c                   endsr
