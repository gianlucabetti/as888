000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400030723     Ffiatt05l  if   e           k disk
000500021203     Ffiapd01l  if   e           k disk
000600030725     Fanrco98j  if   e           k disk
000700030723     Fazorg01l  if   e           k disk
000800030723     Fprtf198   O    F  198        PRINTER prtctl(prtds)
000900020304     dkpjba          e ds
001000030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001100030505     D CNCR80        E DS
001200030723     D PARAM           DS
001300030723     d  data1                         8  0
001400030723     d  data2                         8  0
001500030723     d  aut1                          7  0
001600030723     d  aut2                          7  0
001700030723     d  prosp                         1
001800031114     d  cdf1                          8
001900031114     d  note                          1
002000020404     d prtds           ds
002100020404     d   spab                         3  0
002200020404     d   spaa                         3  0
002300020404     d   skab                         3  0
002400020404     d   skaa                         3  0
002500020404     d   line                         3  0
002600030724     d exattcdf        s                   like(attcdf)
002700130506     d exattsoc        s                   like(attsoc)
002800030724     d exattpdr        s                   like(attpdr)
002900030724     d primo           s              1
003000020305     d dataiso         s               d   datfmt(*iso)
003100020305     d dataeur         s               d   datfmt(*eur)
003200000000     C*---------------------------------------------------------------*
003300020304      *
003400030723     c                   exsr      fiatt
003500020308     c                   seton                                        lr
003600990113     c*-------------------------------------------------
003700030723     c     fiatt         begsr
003800990113     c*-------------------------------------------------
003900990113     c                   setoff                                           99
004000030723     c     terminal      setll     fiatt05l
004100981029     c                   do        *hival
004200030723     c     terminal      reade     fiatt05l                               99
004300981029     c   99              leave
004400031114     c                   if        cdf1 <> *blank and attcdf <> cdf1
004500031114     c                   iter
004600031114     c                   endif
004700030724      * elimino i record che non rientrano nei parametri richiesti
004800030723     c                   if        attddc >= data1 and attddc <= data2 and
004900030723     c                             attpdr >= aut1  and attpdr <= aut2
005000030724     c                   if        primo = *blank
005100030724     c                   move      attpdr        exattpdr
005200030724     c                   move      attcdf        exattcdf
005300130506     c                   move      attsoc        exattsoc
005400030724     c                   move      'X'           primo
005500030724     c                   endif
005600030724
005700030724      * per prospetto riepilogativo verifico rottura autotrasp.
005800030724     c                   if        attpdr <> exattpdr and prosp = '2'
005900030724     c                   if        line > 60
006000030724     c                   except    testata
006100030724     c                   endif
006200030724     c                   except    detta2
006300030724     c                   move      attpdr        exattpdr
006400030724     c                   clear                   totimp           11 3
006500030724     c                   endif
006600030724      * per prospetto riepilogativo verifico rottura FORNITORE
006700030724     c                   if        attcdf <> exattcdf and prosp = '2'
006800030724     c                   if        line > 60
006900030724     c                   except    testata
007000030724     c                   endif
007100030724     c                   except    detta2t
007200030724     c                   move      attcdf        exattcdf
007300130506     c                   move      attsoc        exattsoc
007400030724     c                   clear                   totimpf          11 3
007500030724     c                   endif
007600030724     c
007700021203     c     kapdf         chain     fiapd01l                           97
007800030724      *se l'anagrafica non corrisponde non stampo la decodifica
007900030724     c*  97              iter
008000030723     c   97              clear                   despdr           25
008100090507     c  n97              movel     apdrsf        despdr
008200020404
008300030725      * anagrafica del fornitore
008400030725     c                   move      *all'0'       kksc
008500160607     c                   move      attcdf        kksc
008600030725     c                   clear                   descdf           25
008700030725     c     krco          chain     anrco98j
008800030725     c                   if        %Found(anrco98j)
008900030725     c                   movel     sogDES        descdf           25
009000030725     c                   endif
009100030723      * decodifica p.o.
009200030723     c     attfgp        chain     azorg01l                           95
009300030723     c  n95              movel     orgdes        desfgp           20
009400030723     c   95              clear                   desfgp
009500030723     c     attfga        chain     azorg01l                           95
009600030723     c  n95              movel     orgdes        desfga           20
009700030723     c   95              clear                   desfga
009800030723
009900020404     c                   if        line > 60
010000020404     c                   except    testata
010100020404     c                   endif
010200030723     c                   if        prosp = '1'
010300030723     c                   move      attddc        dataiso
010400030723     c                   move      dataiso       dataeur
010500030723     c                   move      dataeur       attddcs           8 0
010600030723     c                   except    detta1
010700031114     c                   if        note = 'S'
010800031114     c     attnot        comp      *blank                             8080
010900031114     c   80              except    commenti
011000031114     c                   endif
011100030723     c                   else
011200031003     c                   add       attimpp       totimp           11 3
011300031003     c                   add       attimpp       totimpf          11 3
011400030723     c                   endif
011500020404
011600020114     c                   end
011700020404
011800020304     c                   enddo
011900030724      *stampo ultima rottura se tipo prospetto 2
012000030724      * per prospetto riepilogativo verifico rottura autotrasp.
012100030724     c                   if        prosp = '2'
012200030724     c                   if        line > 60
012300030724     c                   except    testata
012400030724     c                   endif
012500030724     c                   except    detta2
012600030724     c                   except    detta2t
012700030724     c                   endif
012800020304
012900020304     c                   endsr
013000981029      *
013100011026     c*-------------------------------------------------
013200011026     c     *inzsr        begsr
013300011026     c*-------------------------------------------------
013400020304     c     *entry        plist
013500020304     c                   parm                    kpjba
013600030723     c                   movel     kpjbu         param
013700030724     c                   if        prosp ='1'
013800030724     c                   seton                                        90
013900030724     c                   endif
014000031114     c                   if        cdf1 <> *blank
014100031114     c                   move      '0000'        aut1
014200031114     c                   move      '9999'        aut2
014300031114     c                   endif
014400031114
014500030723     c                   movel     aut1          terminal          3 0
014600030723     c                   move      data1         dataiso
014700030723     c                   move      dataiso       dataeur
014800030723     c                   move      dataeur       datadal           8 0
014900030723     c                   move      data2         dataiso
015000030723     c                   move      dataiso       dataeur
015100030723     c                   move      dataeur       dataal            8 0
015200030723     c     terminal      chain     azorg01l
015300030723     c                   movel     orgdes        desterm          20
015400011026
015500020218     C                   time                    w0120            14 0
015600020218     C                   move      w0120         wdat              8 0
015700020404     c                   z-add     99            line
015800021203     c     kapdf         klist
015900030723     c                   kfld                    tipo              1
016000030723     c                   kfld                    attpdr
016100030723     c                   move      'D'           tipo
016200030725     c     krco          klist
016300160607     c                   kfld                    attsoc
016400030725     c                   kfld                    knatura           1
016500030725     c                   kfld                    kksc              8
016600030725     c                   movel     'F'           knatura
016700030725
016800030505     C                   Z-ADD     1             CODUT
016900030505     C                   CALL      'X§PARUT'
017000030505     C                   PARM                    UT§DSE
017100030505     C                   MOVEL     RAGUT         RSUT             20
017200030505     C                   MOVEL     REC80         CNCR80
017300011026     c                   endsr
017400020404     C*--------------------------------------------------------------------
017500030723     Oprtf198   E            testata        1 02
017600030724     o                                           10 'FICN81R'
017700020404     o                       knmus               22
017800030723     o                                        +   3 'Lista viaggi confermati -
017900030723     o                                              dal terminal'
018000030723     o                       terminal         +   2
018100030723     o                       desterm          +   1
018200020404     o                       udate         y  +   2
018300020404     O                       page          z  +   1
018400030723     O          E            testata     1
018500030723     o                                           20 'Periodo dal'
018600030723     o                       datadal          +   2 '  /  /    '
018700030723     o                                        +   2 'al'
018800030723     o                       dataal           +   2 '  /  /    '
018900030723     o                                        +   2 'Dal codice'
019000030723     o                       aut1             +   1
019100030723     o                                        +   2 'al codice'
019200030723     o                       aut2             +   1
019300030723
019400030724     O          E            testata     2
019500030723     o                                           10 'Fornitore'
019600030723     o                                           55 'Autotrasportatore'
019700030724     o               90                          82 'Data viaggio'
019800030724     o               90                          95 'Partenza'
019900030724     o               90                         118 'Arrivo'
020000030724     o               90                         145 'Nr.Viaggio'
020100030724     o               90                         158 'Importo'
020200030724     o              n90                          85 'Importo'
020300030723     O          E            detta1      1
020400130506     o                       attsoc               5
020500130506     o                       attcdf           +   1
020600030723     o                       descdf           +   1
020700030723     o                       attpdr           +   2
020800030723     o                       despdr           +   1
020900030723     o                       attddcs          +   1 '  /  /    '
021000030723     o                       attfgp           +   2
021100030723     o                       desfgp           +   1
021200030723     o                       attfga           +   2
021300030723     o                       desfga           +   1
021400030728     o                       attprg        z  +   2
021500031003     o                       attimpp       2  +   2
021600031114     O          E            commenti    1
021700031114     o                                            6 'Note'
021800031114     o                       attnot              60
021900030723     O          E            detta2      1
022000130506     o                       exattsoc             5
022100130506     o                       exattcdf         +   1
022200030723     o                       descdf           +   1
022300030724     o                       exattpdr         +   2
022400030723     o                       despdr           +   1
022500030724     o                       totimp        2  +   2
022600030723     O          E            detta2t     1
022700030723     o                                           50 'Totale delle -
022800030723     o                                              prestazioni effettuate'
022900030724     o                       totimpf       2  +   2
023000030723     O          E            detta2t     0
023100030723     o                                           50 'Totale delle -
023200030723     o                                              prestazioni effettuate'
023300030724     o                       totimpf       2  +   2
023400030723     O          E            detta2t     0
023500030723     o                                           50 'Totale delle -
023600030723     o                                              prestazioni effettuate'
023700030724     o                       totimpf       2  +   2
