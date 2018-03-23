000100040225     h Decedit('0,') Datedit(*YMD.) Option(*nodebugio)
000200001006
000300040225      *------------------------------------------------------------------------*
000400040415      * Dati errati in bolla  - DUT
000500040225      *------------------------------------------------------------------------*
000600111108     ffnblp31l  if   e           k Disk
000700111108     fTitas30c  uf   e           k Disk    ignore(titasp00)
000800111108     fFiar501l  if   e           k Disk
000900040415     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001000111107      *
001100111108     D* DS per TRUL06R - Caricamento £X
001200111108     D TRUL06DS      E DS
001300111108     D  LIN                    1     90  0
001400111108     D                                     DIM(30)
001500111108     D L1              S              3  0 DIM(30)
001600111108     D TOTVo           S              9  0 DIM(30)
001700111108     D TOTVn           S              9  0 DIM(30)
001800111108     d
001900111107     d dtasflo       e ds
002000111108     d kpjba         e ds
002100040302
002200040415     iTitas000      01
002300040415     iTitas010      02
002400040225
002500001006      *------------------------------------------------------------------------*
002600111108     C     Kar5          KLIST
002700111108     C                   KFLD                    blpaas
002800111108     C                   KFLD                    blplnp
002900111108     C                   KFLD                    blpnrs
003000111108     C                   KFLD                    blpnsp
003100111108     C                   KFLD                    ktrd              3
003200111108     c                   eval      ktrd='EMD'
003300111108     C     Ktas          KLIST
003400111108     C                   KFLD                    blpaas
003500111108     C                   KFLD                    blplnp
003600111108     C                   KFLD                    blpnrs
003700111108     C                   KFLD                    blpnsp
003800111108     C
003900111108     C     Kblp          KLIST
004000111108     C                   KFLD                    klnp              3 0
004100111108     C                   KFLD                    kaas              4 0
004200111108     C                   KFLD                    kmgs              4 0
004300111108     C     Kblp2         KLIST
004400111108     C                   KFLD                    klnp              3 0
004500111108     C                   KFLD                    kaas              4 0
004600111108     c
004700111108     c                   z-add     2011          kaas
004800111108     c                   z-add     1101          kmgs
004900111107     c
005000111108     C* CARICO TABELLA FILIALI GESTITE £1
005100111108     C                   CLEAR                   TRUL06DS
005200111108     C                   MOVE      '£1'          D06COD
005300111108     C                   MOVEL     'L'           D06TLA
005400111108     C                   MOVEL     006           D06KEY
005500111108     C                   MOVE      'S'           D06KEY
005600111108     c                   movel     20111101      d06drf
005700111108     C                   MOVEL     TRUL06DS      KPJBU
005800111108     C*
005900111108     C                   CALL      'TRUL06R'
006000111108     C                   PARM                    KPJBA
006100111108     C                   MOVEL     KPJBU         TRUL06DS
006200111108     C                   MOVEA     LIN           L1
006300111108     c
006400040225
006500111108     c                   z-add     1             xx                3 0
006600111108    1c                   dow       l1(xx)>0
006700111108     c                   z-add     l1(xx)        klnp
006800111108     c
006900111108     c     kblp          setll     fnblp31l
007000111108     c     kblp2         reade     fnblp31l
007100111108     c
007200111108    2c                   dow       not %eof(fnblp31l)
007300111108     c
007400111108    3c                   if        blpmgs>1104
007500111108     c                   leave
007600111108    3c                   endif
007700111108     c
007800111108     c     kar5          chain     Fiar501l
007900111108    3c                   if        %found(fiar501l)
008000111108
008100111108     c                   setoff                                       0102
008200111108     c     ktas          chain     titas30c                           30
008300111108
008400111108     c                   if        not *in30
008500111108    4c                   if        *in01  and tasnft=0
008600111108     c                   Eval      dtasflo = TasFlo
008700111108     c                   Eval      §Floemd = 'S'
008800111108     c                   Eval      TasFlo = dtasflo
008900111108     c                   update    titas000
009000111108     c
009100111108    5c                   if        blpmgs=1031
009200111108     c                   add       1             totVo(XX)
009300111108     c                   else
009400111108     c                   add       1             totVN(XX)
009500111108    5c                   endif
009600111108    4c                   endif
009700111108     c
009800111108   x3c                   else
009900111108     c                   except    erroreN
010000111108    3c                   endif
010100111108
010200111108    2c                   endif
010300111107     c
010400111108     c     kblp2         reade     fnblp31l
010500040415    1c                   EndDo
010600111108     c
010700111108     c                   add       1             xx
010800111108    1c                   EndDo
010900111108     c
011000111108     c                   xfoot     totvo         totO             11 0
011100111108     c                   xfoot     totvn         totn             11 0
011200111107     c                   except    totale
011300010202
011400111107     c
011500040225     c                   Eval      *Inlr = *On
011600111107     c*--------------------------------------------
011700111107
011800111107     OQSYSPRT   E            erroreN     1
011900111108     O                                         +  1 'Bolla non trovata'
012000111107     O                       ar5aas        z   +  1
012100111107     O                       ar5lnp        z   +  1
012200111107     O                       ar5nrs        z   +  1
012300111107     O                       ar5nsp        2   +  1
012400111107     O                       ar5trd            +  1
012500111107     o          e            totale      1
012600111108     O                                              'totali OTT:'
012700111108     o                       l1(1)         2   +  1
012800111108     o                       totvo(1)      2   +  1
012900111108     o                       l1(2)         2   +  1
013000111108     o                       totvo(2)      2   +  1
013100111108     o                       l1(3)         2   +  1
013200111108     o                       totvo(3)      2   +  1
013300111108     o                       l1(4)         2   +  1
013400111108     o                       totvo(4)      2   +  1
013500111108     o                       l1(5)         2   +  1
013600111108     o                       totvo(5)      2   +  1
013700111108     o                       l1(6)         2   +  1
013800111108     o                       totvo(6)      2   +  1
013900111108     o          e            totale      1
014000111108     o                       l1(7)         2   +  1
014100111108     o                       totvo(7)      2   +  1
014200111108     o                       l1(8)         2   +  1
014300111108     o                       totvo(8)      2   +  1
014400111108     o                       l1(9)         2   +  1
014500111108     o                       totvo(9)      2   +  1
014600111108     o                       l1(10)        2   +  1
014700111108     o                       totvo(10)     2   +  1
014800111108     o                       l1(11)        2   +  1
014900111108     o                       totvo(11)     2   +  1
015000111108     o                       l1(12)        2   +  1
015100111108     o                       totvo(12)     2   +  1
015200111108     o          e            totale      1
015300111108     O                                              'totali NOV:'
015400111108     o                       l1(1)         2   +  1
015500111108     o                       totvn(1)      2   +  1
015600111108     o                       l1(2)         2   +  1
015700111108     o                       totvn(2)      2   +  1
015800111108     o                       l1(3)         2   +  1
015900111108     o                       totvn(3)      2   +  1
016000111108     o                       l1(4)         2   +  1
016100111108     o                       totvn(4)      2   +  1
016200111108     o                       l1(5)         2   +  1
016300111108     o                       totvn(5)      2   +  1
016400111108     o                       l1(6)         2   +  1
016500111108     o                       totvn(6)      2   +  1
016600111108     o          e            totale      1
016700111108     o                       l1(7)         2   +  1
016800111108     o                       totvn(7)      2   +  1
016900111108     o                       l1(8)         2   +  1
017000111108     o                       totvn(8)      2   +  1
017100111108     o                       l1(9)         2   +  1
017200111108     o                       totvn(9)      2   +  1
017300111108     o                       l1(10)        2   +  1
017400111108     o                       totvn(10)     2   +  1
017500111108     o                       l1(11)        2   +  1
017600111108     o                       totvn(11)     2   +  1
017700111108     o                       l1(12)        2   +  1
017800111108     o                       totvn(12)     2   +  1
017900111108     o          e            totale      1
018000111108     O                                              'Generale OTT'
018100111108     o                       totO          2   +  1
018200111108     O                                         +  1 'Generale NOV'
018300111108     o                       totN          2   +  1
