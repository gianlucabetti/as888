000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400111108     ffnblp31l  if   e           k Disk
000500111110     fTitas30c  if   e           k Disk    ignore(titasp00)
000600111110     ftncsb03l  if   e           k Disk
000700111108     fFiar501l  if   e           k Disk
000800040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900111107      *
001000111108     D* DS per TRUL06R - Caricamento £X
001100111108     D TRUL06DS      E DS
001200111108     D  LIN                    1     90  0
001300111108     D                                     DIM(30)
001400111108     D L1              S              3  0 DIM(30)
001500111108     d
001600111108     d kpjba         e ds
001700111110     d dar5gen       e ds
001800040302
001900040415     iTitas000      01
002000040415     iTitas010      02
002100040225
002200001006      *------------------------------------------------------------------------*
002300111108     C     Kar5          KLIST
002400111108     C                   KFLD                    blpaas
002500111108     C                   KFLD                    blplnp
002600111108     C                   KFLD                    blpnrs
002700111108     C                   KFLD                    blpnsp
002800111108     C                   KFLD                    ktrd              3
002900111110     c                   eval      ktrd='GEN'
003000111108     C     Ktas          KLIST
003100111108     C                   KFLD                    blpaas
003200111108     C                   KFLD                    blplnp
003300111108     C                   KFLD                    blpnrs
003400111108     C                   KFLD                    blpnsp
003500111108     C
003600111108     C     Kblp          KLIST
003700111108     C                   KFLD                    klnp              3 0
003800111108     C                   KFLD                    kaas              4 0
003900111108     C                   KFLD                    kmgs              4 0
004000111108     C     Kblp2         KLIST
004100111108     C                   KFLD                    klnp              3 0
004200111108     C                   KFLD                    kaas              4 0
004300111108     c
004400111108     c                   z-add     2011          kaas
004500111110     c                   z-add     1031          kmgs
004600111107     c
004700111108     C* CARICO TABELLA FILIALI GESTITE £1
004800111108     C                   CLEAR                   TRUL06DS
004900111108     C                   MOVE      '£1'          D06COD
005000111108     C                   MOVEL     'L'           D06TLA
005100111108     C                   MOVEL     006           D06KEY
005200111108     C                   MOVE      'S'           D06KEY
005300111108     c                   movel     20111101      d06drf
005400111108     C                   MOVEL     TRUL06DS      KPJBU
005500111108     C*
005600111108     C                   CALL      'TRUL06R'
005700111108     C                   PARM                    KPJBA
005800111108     C                   MOVEL     KPJBU         TRUL06DS
005900111108     C                   MOVEA     LIN           L1
006000111108     c
006100040225
006200111108     c                   z-add     1             xx                3 0
006300111108    1c                   dow       l1(xx)>0
006400111108     c                   z-add     l1(xx)        klnp
006500111108     c
006600111108     c     kblp          setll     fnblp31l
006700111108     c     kblp2         reade     fnblp31l
006800111108     c
006900111108    2c                   dow       not %eof(fnblp31l)
007000111108     c
007100111108    3c                   if        blpmgs>1104
007200111108     c                   leave
007300111108    3c                   endif
007400111108     c
007500111108     c     kar5          chain     Fiar501l
007600111108    3c                   if        %found(fiar501l)
007700111110     c                   eval      dar5gen=ar5uni
007800111110     c* Se presete CDI stampo
007900111110    4c                   if        §ar5cdi>*zeros
008000111110     c     ktas          chain     tncsb03l
008100111110     c
008200111110     c
008300111110     c                   except    errore
008400111110     c                   add       1             totcdi            9 0
008500111110    4c                   endif
008600111110    3c                   endif
008700111107     c
008800111108     c     kblp2         reade     fnblp31l
008900111110    2c                   EndDo
009000111108     c
009100111108     c                   add       1             xx
009200111108    1c                   EndDo
009300111108     c
009400111107     c                   except    totale
009500010202
009600111107     c
009700040225     c                   Eval      *Inlr = *On
009800111110     c*--------------------------------------------
009900111110     OQSYSPRT   E            errore      1
010000111110     O                                         +  1 'Presente CDI      '
010100111110     O                       ar5aas        z   +  1
010200111110     O                       ar5lnp        z   +  1
010300111110     O                       ar5nrs        z   +  1
010400111110     O                       ar5nsp        2   +  1
010500111110     O                       ar5trd            +  1
010600111110     O                       §ar5cdi           +  1
010700111110     o                                         +  1 'Int asssegno'
010800111110     O                       csbcdi            +  1
010900111110     o                                         +  1 'Data inc'
011000111110     O                       csbdri            +  1 '    /  /  '
011100111110     o                                         +  1 'Data pag'
011200111110     O                       csbddp            +  1 '    /  /  '
011300111110     o
011400111110     o          e            totale      1
011500111110     o                       totcdi        2   +  1
