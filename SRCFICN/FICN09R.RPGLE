000100981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300090424     Ffitgt01l  if   e           k disk
000400021203     Ffiapd01l  if   e           k disk
000500020404     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
000600020304     dkpjba          e ds
000700030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000800030505     D CNCR80        E DS
000900020404     d prtds           ds
001000020404     d   spab                         3  0
001100020404     d   spaa                         3  0
001200020404     d   skab                         3  0
001300020404     d   skaa                         3  0
001400020404     d   line                         3  0
001500020305     d dataiso         s               d   datfmt(*iso)
001600020305     d dataeur         s               d   datfmt(*eur)
001700000000     C*---------------------------------------------------------------*
001800020304      *
001900090424     c                   exsr      fitgt
002000020404     c                   if        una = *blank
002100020404     c                   except    testata
002200020404     c                   except    noncene
002300020404     c                   endif
002400020308     c                   seton                                        lr
002500990113     c*-------------------------------------------------
002600090424     c     fitgt         begsr
002700990113     c*-------------------------------------------------
002800990113     c                   setoff                                           99
002900020404     c                   clear                   una               1
003000090424     c     *loval        setll     fitgt01l
003100981029     c                   do        *hival
003200090424     c                   read      fitgt01l                               99
003300981029     c   99              leave
003400090424     c*
003500090424     c                   if        tgtsml <> ' ' or tgtatb <> ' '
003600090424     c                   iter
003700090424     c                   end
003800020404      * stampo le tariffe con data stampa impostata e data convalida a 0
003900090424     c                   if        tgtdrc = 0 and tgtdcn <> 0
004000021203     c     kapdf         chain     fiapd01l                           97
004100030204      *se l'anagrafica non corrisponde non stampo la tariffa
004200030204     c   97              iter
004300020404
004400020404     c                   if        line > 60
004500020404     c                   except    testata
004600020404     c                   endif
004700020404     c                   move      'X'           una
004800090424     c                   if        tgtdcn <> 0
004900090424     c                   move      tgtdcn        dataiso
005000020404     c                   move      dataiso       dataeur
005100090424     c                   move      dataeur       tgtdcns           8 0
005200090424     c                   else
005300090424     c                   clear                   tgtdcns
005400090424     c                   end
005500090424     c                   if        tgtdst <> 0
005600090424     c                   move      tgtdst        dataiso
005700020404     c                   move      dataiso       dataeur
005800090424     c                   move      dataeur       tgtdsts           8 0
005900090424     c                   else
006000090424     c                   clear                   tgtdsts
006100090424     c                   end
006200090424     c                   if        tgtddt <> 0
006300090424     c                   move      tgtddt        dataiso
006400020404     c                   move      dataiso       dataeur
006500090424     c                   move      dataeur       tgtddts           8 0
006600020404     c                   except    detta
006700090424     c                   else
006800090424     c                   clear                   tgtddts
006900090424     c                   end
007000020404
007100020114     c                   end
007200020404
007300020304     c                   enddo
007400020304
007500020304     c                   endsr
007600981029      *
007700011026     c*-------------------------------------------------
007800011026     c     *inzsr        begsr
007900011026     c*-------------------------------------------------
008000020304     c     *entry        plist
008100020304     c                   parm                    kpjba
008200011026
008300020218     C                   time                    w0120            14 0
008400020218     C                   move      w0120         wdat              8 0
008500020404     c                   z-add     99            line
008600021203     c     kapdf         klist
008700090424     c                   kfld                    apdtip
008800090424     c                   kfld                    tgtpdr
008900090424     c                   movel     'A'           apdtip
009000070307     c* reperisco i po legati contabilmente
009100070307     C                   Z-ADD     1             CODUT
009200070307     C                   CALL      'X§PARUT'
009300070307     C                   PARM                    UT§DSE
009400070307     C                   MOVEL     RAGUT         RSUT             20
009500070307     C                   MOVEL     REC80         CNCR80
009600070307
009700070307     c                   endsr
009800020404     C*--------------------------------------------------------------------
009900020404     Oqsysprt   E            testata        1 02
010000090507     o                                           10 'FICN09R'
010100020404     o                       knmus               22
010200021106     o                                        +   3 'Lista tariffe firmate  -
010300021106     o                                              non ancora pervenute'
010400020404     o                       udate         y  +   2
010500020404     O                       page          z  +   1
010600020404     O          E            testata     1  1
010700090424     o                                           20 'Autotrasportatore'
010800020404     o                                           60 'Prog'
010900020404     o                                           74 'Data Stampa'
011000020404     o                                           87 'Data Decor.'
011100020404     o                                          100 'Data Scad. '
011200021106     o                                          112 'Escl.da Fat'
011300020404     O          E            detta       1
011400090424     o                       tgtpdr               9
011500100223     o                       apdrsf           +   2
011600090424     o                       tgtprg        z     59
011700090424     o                       tgtdcns             74 '  /  /    '
011800090424     o                       tgtddts             86 '  /  /    '
011900090424     o                       tgtdsts             98 '  /  /    '
012000021106     o                       apdpdd             106
012100020404     O          E            noncene     3
012200020404     o                                           80 'N O N   E S I S T O N O   -
012300090424     o                                              T A R I F F E   D A   R I C-
012400090424     o                                               E V E R E'
