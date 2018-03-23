000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300080320     fFNarb01l  UF   E           K DISK    prefix(blp:3)
000400100315     ftitas30c  uF   E           K DISK
000500100315     ffnblp01l  uF   E           K DISK
000600080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700100315     d
000800100702     d kpkf            s                   like(blppkf)
000900080320     ititas000      01
001000980611      * ---------------------------------------------------------
001100100702     c     *entry        plist
001200100702     c                   parm                    aggiorna          1
001300080212     C     Kblp          KLIST
001400080212     C                   KFLD                    blpAAS
001500080212     C                   KFLD                    blpLNP
001600080212     C                   KFLD                    blpNRS
001700080212     C                   KFLD                    blpNSP
001800100315     C     Kblp2         KLIST
001900100315     C                   KFLD                    kaAS
002000100315     C                   KFLD                    klnp              3 0
002100100702     C                   KFLD                    knrs              2 0
002200041222     C
002300080212     c                   clear                   kaas              4 0
002400100315     c                   clear                   conta             4 0
002500080212
002600100315     C                   EVAL      KAAS=2010
002700100702     C                   EVAL      Klnp=116
002800100702     C                   EVAL      Knrs=01
002900100315     c                   setoff                                       90
003000041222     C
003100100315     c     kblp2         SETLL     fnarb000                           30
003200100702     c     kblp2         READe(N)  fnarb01l
003300041222     C
003400100702    1C                   DOW       NOT %EOF(fnarb01l)
003500080320     c
003600100702    2c                   if        blpmgs=0701
003700100702     c                                          and blpksc=1160940
003800100315     c
003900100702     c                   eval(h)   kpkf=blppkb/10
004000100702     c                   except    stampa
004100100702     c
004200100702    3c                   if        aggiorna='S'
004300100315     c
004400100315     C     Kblp          chain     fnarb01l
004500100315     c
004600100702     c                   eval      blppkb=kpkf
004700100702     c                   eval      blppkf=kpkf
004800100315     c                   update    fnarb000
004900100315     c
005000100315     c                   eval      conta=conta+1
005100100315
005200100315     C     Kblp          chain     fnblp01l
005300100702     c                   eval      blppkb=kpkf
005400100702     c                   eval      blppkf=kpkf
005500100315     c                   update    fnblp000
005600100315     c
005700100315     c                   setoff                                       01
005800100315     C     Kblp          chain     titas30c
005900100315    4c                   if        %found(titas30c) and *in01
006000100702     c                   eval      taspkb=kpkf
006100100702     c                   eval      taspkf=kpkf
006200100315     c                   update    titas000
006300100315    4c                   endif
006400100315
006500100702    3c                   endif
006600100702    2c                   endif
006700080320     c
006800100702     c     kblp2         READe(N)  fnarb01l
006900080320    1c                   enddo
007000080320     c
007100100315     c
007200100315     c                   except    fine
007300100315     c
007400080212     c                   seton                                        lr
007500100315     OQSYSPRT   E            fine        1
007600100315     O                                              'Totale record aggiornati'
007700100315     O                       conta         2  +   1
007800100315     OQSYSPRT   E            STAMPA      1
007900100702     O                                              'Bolla corretta:'
008000100315     O                       blpaas        z  +   1
008100100315     O                       blplnp           +   1
008200100315     O                       blpnrs        z  +   1
008300100315     O                       blpnsp        2  +   1
008400100315     O                       blpmgs           +   1 '  /  '
008500100315     O                       blpCBO           +   2
008600100702     o                                        +   2 'Peso'
008700100702     O                       blppkb        2  +   1
008800100702     O                       blppkf        2  +   1
008900100702     o                                        +   2 'peso corretto'
009000100702     O                       kpkf          2  +   1
