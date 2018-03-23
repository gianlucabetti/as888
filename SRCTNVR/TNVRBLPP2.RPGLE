000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300080320     fFNarb01l  UF   E           K DISK    prefix(blp:3)
000400080212     ftitas30c  IF   E           K DISK
000500080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600080320     dtfp              s              3  0 dim(100)
000700080320     dpeso             s              5  0 dim(100)
000800080320     dvolume           s              5  0 dim(100)
000900080320     d
001000080320     ititas000      01
001100980611      * ---------------------------------------------------------
001200080212     C     Kblp          KLIST
001300080212     C                   KFLD                    blpAAS
001400080212     C                   KFLD                    blpLNP
001500080212     C                   KFLD                    blpNRS
001600080212     C                   KFLD                    blpNSP
001700041222     C
001800080212     c                   clear                   kaas              4 0
001900080320     c                   clear                   x                 4 0
002000080320     c                   clear                   y                 4 0
002100080212
002200080212     C                   EVAL      KAAS=2008
002300041222     C
002400080320     c     kaas          SETLL     fnarb000                           30
002500080320     c     kaas          READe(N)  fnarb01l
002600041222     C
002700080320    1C                   DOW       NOT %EOF(fnarb01l)
002800080320    2c                   if        blpmgs>=0301 and blppkc>0 and blpncp<>99999
002900080212     c                   setoff                                       01
003000080212     C     Kblp          chain     titas30c
003100080320    3c                   if        %found(titas30c) and *in01
003200080320    4c                   if        *in01 and (taspkc=0 or tasncp=99999 or
003300080320     c                             taspkc<blppkc)
003400080320     C     Kblp          chain     fnarb01l
003500080320     c                   if        blpfbs<>' '
003600080320     c                   eval      blpfbs='E'
003700080320     c                   else
003800080320     c                   eval      blpfbs='P'
003900080320     c                   endif
004000080320     c                   update    fnarb000
004100080320
004200080320     c                   z-add     1             x
004300080320     c     blptfa        lookup    tfp(x)                                 80
004400080320    5c                   if        not  *in80
004500080320     c                   add       1             y
004600080320     c                   eval      tfp(y) =blptfa
004700080320     c                   z-add     1             peso(y)
004800080320     c                   else
004900080320     c                   add       1             peso(x)
005000080320    5c                   endif
005100080320     c
005200080320    4c                   endif
005300080320    3c                   endif
005400080320    2c                   endif
005500080320     c
005600080320    2c                   if        blpmgs>=0301 and blpvlc>0 and blpncr<>99999
005700080320     c                   setoff                                       01
005800080320     C     Kblp          chain     titas30c
005900080320    3c                   if        %found(titas30c) and *in01
006000080320    4c                   if        *in01 and (tasvlc=0 or tasncr=99999 or
006100080320     c                             tasvlf<blpvlf)
006200080320     C     Kblp          chain     fnarb01l
006300080320     c                   if        blpfbs<>' '
006400080320     c                   eval      blpfbs='E'
006500080320     c                   else
006600080320     c                   eval      blpfbs='V'
006700080320     c                   endif
006800080320     c                   update    fnarb000
006900080320     c                   z-add     1             x
007000080320     c     blptfa        lookup    tfp(x)                                 80
007100080320    5c                   if        not  *in80
007200080320     c                   add       1             y
007300080320     c                   eval      tfp(y) =blptfa
007400080320     c                   z-add     1             volume(y)
007500080320     c                   else
007600080320     c                   add       1             volume(x)
007700080320    5c                   endif
007800080320    4c                   endif
007900080320    3c                   endif
008000080320    2c                   endif
008100080320     c
008200080320     c     kaas          READe(N)  fnarb01l
008300080320    1c                   enddo
008400080320     c
008500080320     c                   z-add     1             x
008600080320     c                   dow       tfp(X)>0
008700080320     c                   except    fine
008800080320     c                   add       1             x
008900080320     c                   enddo
009000080212     c                   seton                                        lr
009100080320     OQSYSPRT   E            fine        1
009200080320     O                                              'Totali terminal arr.'
009300080320     O                       tfp(x)           +   1
009400080320     O                                        +   1 'peso aggiornati'
009500080320     O                       peso(x)       2  +   1
009600080320     O                                        +   1 'vol. aggiornati'
009700080320     O                       volume(x)     2  +   1
