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
002700030724     d exattpdr        s                   like(attpdr)
002800030724     d primo           s              1
002900020305     d dataiso         s               d   datfmt(*iso)
003000020305     d dataeur         s               d   datfmt(*eur)
003100000000     C*---------------------------------------------------------------*
003200020304      *
003300030723     c                   exsr      fiatt
003400020308     c                   seton                                        lr
003500990113     c*-------------------------------------------------
003600030723     c     fiatt         begsr
003700990113     c*-------------------------------------------------
003800990113     c                   setoff                                           99
003900030723     c     terminal      setll     fiatt05l
004000981029     c                   do        *hival
004100030723     c     terminal      reade     fiatt05l                               99
004200981029     c   99              leave
004300031114     c                   if        cdf1 <> *blank and attcdf <> cdf1
004400031114     c                   iter
004500031114     c                   endif
004600030724      * elimino i record che non rientrano nei parametri richiesti
004700030723     c                   if        attddc >= data1 and attddc <= data2 and
004800030723     c                             attpdr >= aut1  and attpdr <= aut2
004900030724     c                   if        primo = *blank
005000030724     c                   move      attpdr        exattpdr
005100030724     c                   move      attcdf        exattcdf
005200030724     c                   move      'X'           primo
005300030724     c                   endif
005400030724
005500030724      * per prospetto riepilogativo verifico rottura autotrasp.
005600030724     c                   if        attpdr <> exattpdr and prosp = '2'
005700030724     c                   if        line > 60
005800030724     c                   except    testata
005900030724     c                   endif
006000030724     c                   except    detta2
006100030724     c                   move      attpdr        exattpdr
006200030724     c                   clear                   totimp           11 3
006300030724     c                   endif
006400030724      * per prospetto riepilogativo verifico rottura FORNITORE
006500030724     c                   if        attcdf <> exattcdf and prosp = '2'
006600030724     c                   if        line > 60
006700030724     c                   except    testata
006800030724     c                   endif
006900030724     c                   except    detta2t
007000030724     c                   move      attcdf        exattcdf
007100030724     c                   clear                   totimpf          11 3
007200030724     c                   endif
007300030724     c
007400021203     c     kapdf         chain     fiapd01l                           97
007500030724      *se l'anagrafica non corrisponde non stampo la decodifica
007600030724     c*  97              iter
007700030723     c   97              clear                   despdr           25
007800030723     c  n97              movel     apdrsc        despdr
007900020404
008000030725      * anagrafica del fornitore
008100030725     c                   move      *all'0'       kksc
008200030725     c                   move      apdksc        kksc
008300030725     c                   clear                   descdf           25
008400030725     c     krco          chain     anrco98j
008500030725     c                   if        %Found(anrco98j)
008600030725     c                   movel     sogDES        descdf           25
008700030725     c                   endif
008800030723      * decodifica p.o.
008900030723     c     attfgp        chain     azorg01l                           95
009000030723     c  n95              movel     orgdes        desfgp           20
009100030723     c   95              clear                   desfgp
009200030723     c     attfga        chain     azorg01l                           95
009300030723     c  n95              movel     orgdes        desfga           20
009400030723     c   95              clear                   desfga
009500030723
009600020404     c                   if        line > 60
009700020404     c                   except    testata
009800020404     c                   endif
009900030723     c                   if        prosp = '1'
010000030723     c                   move      attddc        dataiso
010100030723     c                   move      dataiso       dataeur
010200030723     c                   move      dataeur       attddcs           8 0
010300030723     c                   except    detta1
010400031114     c                   if        note = 'S'
010500031114     c     attnot        comp      *blank                             8080
010600031114     c   80              except    commenti
010700031114     c                   endif
010800030723     c                   else
010900031003     c                   add       attimpp       totimp           11 3
011000031003     c                   add       attimpp       totimpf          11 3
011100030723     c                   endif
011200020404
011300020114     c                   end
011400020404
011500020304     c                   enddo
011600030724      *stampo ultima rottura se tipo prospetto 2
011700030724      * per prospetto riepilogativo verifico rottura autotrasp.
011800030724     c                   if        prosp = '2'
011900030724     c                   if        line > 60
012000030724     c                   except    testata
012100030724     c                   endif
012200030724     c                   except    detta2
012300030724     c                   except    detta2t
012400030724     c                   endif
012500020304
012600020304     c                   endsr
012700981029      *
012800011026     c*-------------------------------------------------
012900011026     c     *inzsr        begsr
013000011026     c*-------------------------------------------------
013100020304     c     *entry        plist
013200020304     c                   parm                    kpjba
013300030723     c                   movel     kpjbu         param
013400030724     c                   if        prosp ='1'
013500030724     c                   seton                                        90
013600030724     c                   endif
013700031114     c                   if        cdf1 <> *blank
013800031114     c                   move      '0000'        aut1
013900031114     c                   move      '9999'        aut2
014000031114     c                   endif
014100031114
014200030723     c                   movel     aut1          terminal          3 0
014300030723     c                   move      data1         dataiso
014400030723     c                   move      dataiso       dataeur
014500030723     c                   move      dataeur       datadal           8 0
014600030723     c                   move      data2         dataiso
014700030723     c                   move      dataiso       dataeur
014800030723     c                   move      dataeur       dataal            8 0
014900030723     c     terminal      chain     azorg01l
015000030723     c                   movel     orgdes        desterm          20
015100011026
015200020218     C                   time                    w0120            14 0
015300020218     C                   move      w0120         wdat              8 0
015400020404     c                   z-add     99            line
015500021203     c     kapdf         klist
015600030723     c                   kfld                    tipo              1
015700030723     c                   kfld                    attpdr
015800030723     c                   move      'D'           tipo
015900030725     c     krco          klist
016000030725     c                   kfld                    apdcsf
016100030725     c                   kfld                    knatura           1
016200030725     c                   kfld                    kksc              8
016300030725     c                   movel     'F'           knatura
016400030725
016500030505     C                   Z-ADD     1             CODUT
016600030505     C                   CALL      'X§PARUT'
016700030505     C                   PARM                    UT§DSE
016800030505     C                   MOVEL     RAGUT         RSUT             20
016900030505     C                   MOVEL     REC80         CNCR80
017000011026     c                   endsr
017100020404     C*--------------------------------------------------------------------
017200030723     Oprtf198   E            testata        1 02
017300030724     o                                           10 'FICN81R'
017400020404     o                       knmus               22
017500030723     o                                        +   3 'Lista viaggi confermati -
017600030723     o                                              dal terminal'
017700030723     o                       terminal         +   2
017800030723     o                       desterm          +   1
017900020404     o                       udate         y  +   2
018000020404     O                       page          z  +   1
018100030723     O          E            testata     1
018200030723     o                                           20 'Periodo dal'
018300030723     o                       datadal          +   2 '  /  /    '
018400030723     o                                        +   2 'al'
018500030723     o                       dataal           +   2 '  /  /    '
018600030723     o                                        +   2 'Dal codice'
018700030723     o                       aut1             +   1
018800030723     o                                        +   2 'al codice'
018900030723     o                       aut2             +   1
019000030723
019100030724     O          E            testata     2
019200030723     o                                           10 'Fornitore'
019300030723     o                                           55 'Autotrasportatore'
019400030724     o               90                          82 'Data viaggio'
019500030724     o               90                          95 'Partenza'
019600030724     o               90                         118 'Arrivo'
019700030724     o               90                         145 'Nr.Viaggio'
019800030724     o               90                         158 'Importo'
019900030724     o              n90                          85 'Importo'
020000030723     O          E            detta1      1
020100030723     o                       attcdf              10
020200030723     o                       descdf           +   1
020300030723     o                       attpdr           +   2
020400030723     o                       despdr           +   1
020500030723     o                       attddcs          +   1 '  /  /    '
020600030723     o                       attfgp           +   2
020700030723     o                       desfgp           +   1
020800030723     o                       attfga           +   2
020900030723     o                       desfga           +   1
021000030728     o                       attprg        z  +   2
021100031003     o                       attimpp       2  +   2
021200031114     O          E            commenti    1
021300031114     o                                            6 'Note'
021400031114     o                       attnot              60
021500030723     O          E            detta2      1
021600030724     o                       exattcdf            10
021700030723     o                       descdf           +   1
021800030724     o                       exattpdr         +   2
021900030723     o                       despdr           +   1
022000030724     o                       totimp        2  +   2
022100030723     O          E            detta2t     1
022200030723     o                                           50 'Totale delle -
022300030723     o                                              prestazioni effettuate'
022400030724     o                       totimpf       2  +   2
022500030723     O          E            detta2t     0
022600030723     o                                           50 'Totale delle -
022700030723     o                                              prestazioni effettuate'
022800030724     o                       totimpf       2  +   2
022900030723     O          E            detta2t     0
023000030723     o                                           50 'Totale delle -
023100030723     o                                              prestazioni effettuate'
023200030724     o                       totimpf       2  +   2
