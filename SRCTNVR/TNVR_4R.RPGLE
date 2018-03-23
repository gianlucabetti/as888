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
002000111107     d dar5gen       e ds
002100111108     d kpjba         e ds
002200040302
002300040415     iTitas000      01
002400040415     iTitas010      02
002500040225
002600001006      *------------------------------------------------------------------------*
002700111108     C     Kar5          KLIST
002800111108     C                   KFLD                    blpaas
002900111108     C                   KFLD                    blplnp
003000111108     C                   KFLD                    blpnrs
003100111108     C                   KFLD                    blpnsp
003200111108     C                   KFLD                    ktrd              3
003300111108     c                   eval      ktrd='GEN'
003400111108     C     Ktas          KLIST
003500111108     C                   KFLD                    blpaas
003600111108     C                   KFLD                    blplnp
003700111108     C                   KFLD                    blpnrs
003800111108     C                   KFLD                    blpnsp
003900111108     C
004000111108     C     Kblp          KLIST
004100111108     C                   KFLD                    klnp              3 0
004200111108     C                   KFLD                    kaas              4 0
004300111108     C                   KFLD                    kmgs              4 0
004400111108     C     Kblp2         KLIST
004500111108     C                   KFLD                    klnp              3 0
004600111108     C                   KFLD                    kaas              4 0
004700111108     c
004800111108     c                   z-add     2011          kaas
004900111108     c                   z-add     1031          kmgs
005000111107     c
005100111108     C* CARICO TABELLA FILIALI GESTITE £1
005200111108     C                   CLEAR                   TRUL06DS
005300111108     C                   MOVE      '£1'          D06COD
005400111108     C                   MOVEL     'L'           D06TLA
005500111108     C                   MOVEL     006           D06KEY
005600111108     C                   MOVE      'S'           D06KEY
005700111108     c                   movel     20111031      d06drf
005800111108     C                   MOVEL     TRUL06DS      KPJBU
005900111108     C*
006000111108     C                   CALL      'TRUL06R'
006100111108     C                   PARM                    KPJBA
006200111108     C                   MOVEL     KPJBU         TRUL06DS
006300111108     C                   MOVEA     LIN           L1
006400111108     c
006500040225
006600111108     c                   z-add     1             xx                3 0
006700111108     c                   dow       l1(xx)>0
006800111108     c                   z-add     l1(xx)        klnp
006900111108     c
007000111108     c     kblp          setll     fnblp31l
007100111108     c     kblp2         reade     fnblp31l
007200111108     c
007300111108     c                   dow       not %eof(fnblp31l)
007400111108     c
007500111108     c                   if        blpmgs>1104
007600111108     c                   leave
007700111108     c                   endif
007800111108     c
007900111108     c     kar5          chain     Fiar501l
008000111108     c                   if        %found(fiar501l)
008100111107     c                   eval      dar5gen=ar5uni
008200111107     c* solo colli di valore
008300111107     c                   if        §ar5bva='V'
008400111107     c                   exsr      aggtitas
008500040420    4c                   endif
008600111107     c                   endif
008700111107     c
008800111108     c     kblp2         reade     fnblp31l
008900040415    1c                   EndDo
009000111108     c
009100111108     c                   add       1             xx
009200111108    1c                   EndDo
009300111108     c
009400111108     c                   xfoot     totvo         totO             11 0
009500111108     c                   xfoot     totvn         totn             11 0
009600111107     c                   except    totale
009700010202
009800111107     c
009900040225     c                   Eval      *Inlr = *On
010000111107     c*--------------------------------------------
010100111107     c     AggTITAS      BEGSR
010200111107     c
010300111107     c                   setoff                                       0102
010400111107     c     ktas          chain     titas30c
010500111107     c                   if        %found(titas30c)
010600111107     c                   Eval      dtasflo = TasFlo
010700111108     c
010800111108     c                   if        §flotba=' '
010900111107     c                   Eval      §FloTba = §Ar5Bva
011000111107     c                   Eval      TasFlo = dtasflo
011100111108     c   01              update    titas000
011200111108     c   02              update    titas010
011300111108     c
011400111108     c                   if        blpmgs=1031
011500111108     c                   add       1             totVo(XX)
011600111108     c                   else
011700111108     c                   add       1             totVN(XX)
011800111108     c                   endif
011900111108     c                   endif
012000111108     c
012100111107     c                   else
012200111107     c                   except    erroreN
012300111107     c                   endif
012400111108     C
012500111107     c                   ENDSR
012600111107
012700111107     OQSYSPRT   E            erroreN     1
012800111108     O                                         +  1 'AGGIORNO BOLLA VALORE'
012900111107     O                       ar5aas        z   +  1
013000111107     O                       ar5lnp        z   +  1
013100111107     O                       ar5nrs        z   +  1
013200111107     O                       ar5nsp        2   +  1
013300111107     O                       ar5trd            +  1
013400111107     o          e            totale      1
013500111108     O                                              'totali OTT:'
013600111108     o                       l1(1)         2   +  1
013700111108     o                       totvo(1)      2   +  1
013800111108     o                       l1(2)         2   +  1
013900111108     o                       totvo(2)      2   +  1
014000111108     o                       l1(3)         2   +  1
014100111108     o                       totvo(3)      2   +  1
014200111108     o                       l1(4)         2   +  1
014300111108     o                       totvo(4)      2   +  1
014400111108     o                       l1(5)         2   +  1
014500111108     o                       totvo(5)      2   +  1
014600111108     o                       l1(6)         2   +  1
014700111108     o                       totvo(6)      2   +  1
014800111108     o          e            totale      1
014900111108     o                       l1(7)         2   +  1
015000111108     o                       totvo(7)      2   +  1
015100111108     o                       l1(8)         2   +  1
015200111108     o                       totvo(8)      2   +  1
015300111108     o                       l1(9)         2   +  1
015400111108     o                       totvo(9)      2   +  1
015500111108     o                       l1(10)        2   +  1
015600111108     o                       totvo(10)     2   +  1
015700111108     o                       l1(11)        2   +  1
015800111108     o                       totvo(11)     2   +  1
015900111108     o                       l1(12)        2   +  1
016000111108     o                       totvo(12)     2   +  1
016100111108     o          e            totale      1
016200111108     O                                              'totali NOV:'
016300111108     o                       l1(1)         2   +  1
016400111108     o                       totvn(1)      2   +  1
016500111108     o                       l1(2)         2   +  1
016600111108     o                       totvn(2)      2   +  1
016700111108     o                       l1(3)         2   +  1
016800111108     o                       totvn(3)      2   +  1
016900111108     o                       l1(4)         2   +  1
017000111108     o                       totvn(4)      2   +  1
017100111108     o                       l1(5)         2   +  1
017200111108     o                       totvn(5)      2   +  1
017300111108     o                       l1(6)         2   +  1
017400111108     o                       totvn(6)      2   +  1
017500111108     o          e            totale      1
017600111108     o                       l1(7)         2   +  1
017700111108     o                       totvn(7)      2   +  1
017800111108     o                       l1(8)         2   +  1
017900111108     o                       totvn(8)      2   +  1
018000111108     o                       l1(9)         2   +  1
018100111108     o                       totvn(9)      2   +  1
018200111108     o                       l1(10)        2   +  1
018300111108     o                       totvn(10)     2   +  1
018400111108     o                       l1(11)        2   +  1
018500111108     o                       totvn(11)     2   +  1
018600111108     o                       l1(12)        2   +  1
018700111108     o                       totvn(12)     2   +  1
018800111108     o          e            totale      1
018900111108     O                                              'Generale OTT'
019000111108     o                       totO          2   +  1
019100111108     O                                         +  1 'Generale NOV'
019200111108     o                       totN          2   +  1
