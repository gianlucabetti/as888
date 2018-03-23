000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300080401     fFNblp01l  iF   E           K DISK
000400080401     fFNarb01l  iF   E           K DISK
000500080212     ftitas30c  IF   E           K DISK
000600080320     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700080320     dtfp              s              3  0 dim(100)
000800080320     dpeso             s              5  0 dim(100)
000900080320     dvolume           s              5  0 dim(100)
001000080320     d
001100080320     ititas000      01
001200980611      * ---------------------------------------------------------
001300080212     C     Kblp          KLIST
001400080212     C                   KFLD                    blpAAS
001500080212     C                   KFLD                    blpLNP
001600080212     C                   KFLD                    blpNRS
001700080212     C                   KFLD                    blpNSP
001800041222     C
001900080212     c                   clear                   kaas              4 0
002000080320     c                   clear                   x                 4 0
002100080320     c                   clear                   y                 4 0
002200080212
002300080212     C                   EVAL      KAAS=2008
002400041222     C
002500080212     c     kaas          SETLL     fnblp000                           30
002600080212     c     kaas          READe(N)  fnblp01l
002700041222     C
002800080212    1C                   DOW       NOT %EOF(fnblp01l)
002900080320    2c                   if        blpmgs>=0301 and blppkc>0 and blpncp<>99999
003000080212     c                   setoff                                       01
003100080212     C     Kblp          chain     titas30c
003200080320    3c                   if        %found(titas30c) and *in01
003300080320    4c                   if        *in01 and (taspkc=0 or tasncp=99999 or
003400080320     c                             taspkc<>blppkc)
003500080212     C     Kblp          chain     fnblp01l
003600080212     c                   eval      blpncp=99999
003700080320     c                   z-add     1             x
003800080320     c     blptfp        lookup    tfp(x)                                 80
003900080320    5c                   if        not  *in80
004000080320     c                   add       1             y
004100080320     c                   eval      tfp(y) =blptfp
004200080320     c                   z-add     1             peso(y)
004300080320     c                   else
004400080320     c                   add       1             peso(x)
004500080320    5c                   endif
004600080320     c
004700080320     c*****              update    fnblp000
004800080320    4c                   endif
004900080320    3c                   endif
005000080320    2c                   endif
005100080320     c
005200080320    2c                   if        blpmgs>=0301 and blpvlc>0 and blpncr<>99999
005300080320     c                   setoff                                       01
005400080320     C     Kblp          chain     titas30c
005500080320    3c                   if        %found(titas30c) and *in01
005600080320    4c                   if        *in01 and (tasvlc=0 or tasncr=99999 or
005700080320     c                             tasvlf<>blpvlf)
005800080320     C     Kblp          chain     fnblp01l
005900080320     c                   eval      blpncr=99999
006000080320     c********           update    fnblp000
006100080320     c                   z-add     1             x
006200080320     c     blptfp        lookup    tfp(x)                                 80
006300080320    5c                   if        not  *in80
006400080320     c                   add       1             y
006500080320     c                   eval      tfp(y) =blptfp
006600080320     c                   z-add     1             volume(y)
006700080320     c                   else
006800080320     c                   add       1             volume(x)
006900080320    5c                   endif
007000080320    4c                   endif
007100080320    3c                   endif
007200080320    2c                   endif
007300080320     c
007400080212     c     kaas          READe(N)  fnblp01l
007500080320    1c                   enddo
007600080320     c
007700080320     c                   z-add     1             x
007800080320     c                   dow       tfp(X)>0
007900080320     c                   except    fine
008000080320     c                   add       1             x
008100080320     c                   enddo
008200080212     c                   seton                                        lr
008300080320     OQSYSPRT   E            fine        1
008400080320     O                                              'Totali terminal part'
008500080320     O                       tfp(x)           +   1
008600080320     O                                        +   1 'peso aggiornati'
008700080320     O                       peso(x)       2  +   1
008800080320     O                                        +   1 'vol. aggiornati'
008900080320     O                       volume(x)     2  +   1
