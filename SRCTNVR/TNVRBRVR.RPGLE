000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300170303     Ffnbrv01l  iF   E           k DISK
000400170303     Ftigcp01l  iF   E           k DISK
000500170303     Ffnfvv01l  iF   E           k DISK
000600170303     Ffnart27l  iF   E           k DISK
000700170303     FFNarb01l  iF   E           K DISK
000800091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021214      *--------------------------------------------------
001000170303     c     *entry        plist
001100170303     c                   parm                    pnfv              6
001200170303     c                   parm                    pfgs              3
001300170303
001400150714     c     kbrv          klist
001500150714     c                   kfld                    knpg              1 0
001600170303     c                   kfld                    knfv
001700170303     c                   kfld                    kfgs
001800170303     c     kbrv2         klist
001900170303     c                   kfld                    brvlnp
002000170303     c                   kfld                    brvlna
002100170303     c                   kfld                    brvnrs
002200170303     c                   kfld                    brvnsc
002300170303     c     kfvv          klist
002400170303     c                   kfld                    knpg              1 0
002500170303     c                   kfld                    knfv5             5 0
002600170303     c                   kfld                    kfgs
002700150714     c     kblp          klist
002800170303     c                   kfld                    artaas
002900170303     c                   kfld                    artlnp
003000170303     c                   kfld                    artnrs
003100170303     c                   kfld                    artnsp
003200170303     c     kgcp          klist
003300170303     c                   kfld                    arbaas
003400170303     c                   kfld                    arblnp
003500170303     c                   kfld                    arbnrs
003600170303     c                   kfld                    arbnsp
003700170303     c
003800130124     c
003900150714     c                   clear                   conta
004000070913     c
004100170303     c                   eval      knpg=3
004200170303     c                   movel     pnfv          knfv              6 0
004300170303     c                   move      pnfv          knfv5             5 0
004400170303     c                   movel     pfgs          kfgs              3 0
004500170303
004600170303     c     kfvv          chain     fnfvv01l
004700170303    0c                   if        %found(fnfvv01l)
004800170303     c     kbrv          setll     fnbrv01l
004900170303     c     kbrv          reade     fnbrv01l
005000051110
005100170303    1c                   DOW       not %EOF(fnbrv01l)
005200170303     c     kbrv2         chain     fnart27l
005300170303    2c                   if        %found(fnart27l)
005400170303     c     kblp          chain     fnarb01l
005500170303    3c                   if        %found(fnarb01l)
005600170303     c                   clear                   giacok            1
005700170303     c* verifico se ha giacenza
005800170303     c     kgcp          setll     tigcp01l
005900170303     c     kgcp          reade     tigcp01l
006000170303    4c                   dow       not %eof(tigcp01l)
006100170303    5c                   if        (gcpagc*10000)+gcpmgc<=fvvdfv
006200170303     c                             and (gcpdcg=0 or gcpdcg>=fvvdfv)
006300170303     c                   eval      giacok='S'
006400170303     c                   leave
006500170303    5c                   endif
006600170303     c     kgcp          reade     tigcp01l
006700170303    4c                   enddo
006800170303     c
006900170303    4c                   if        giacok='S' and GCPRIAP=' '
007000170303     c* se la data a magazzino non c'è oppure è maggiore della data foglio
007100170303    5c                   if        gcpdlma=0 or gcpdlma>fvvdfv
007200170303     c                   add       1             conta             7 0
007300170303    5c                   endif
007400170303    4c                   endif
007500170303     c
007600170303    3c                   endif
007700170303    2c                   endif
007800150714     c
007900170303     c     kbrv          reade     fnbrv01l
008000170303    1c                   enddo
008100021216      *
008200130124     c                   except    totale
008300170303     c                   endif
008400130124     c
008500021214     C                   eval       *inlr = *on
008600130124      *
008700150714     OQSYSPRT   E            totale      1
008800170303     O                                              'Totale colli in IMA'
008900170303     O                                        +   1 ' di giacenza.Fil'
009000170303     O                       pfgs             +   1
009100170303     O                                        +   1 ' data'
009200170303     O                       fvvdfv           +   1 '    /  /  '
009300170303     O                       conta         2  +   1
