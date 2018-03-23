000100100223     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400090424     Ffitgt01l  if   e           k disk
000500021203     Ffiapd01l  if   e           k disk
000600020404     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
000700020304     dkpjba          e ds
000900030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001000030505     D CNCR80        E DS
001500020404     d prtds           ds
001600020404     d   spab                         3  0
001700020404     d   spaa                         3  0
001800020404     d   skab                         3  0
001900020404     d   skaa                         3  0
002000020404     d   line                         3  0
002100020305     d dataiso         s               d   datfmt(*iso)
002200020305     d dataeur         s               d   datfmt(*eur)
002300000000     C*---------------------------------------------------------------*
002400020304      *
002500090424     c                   exsr      fitgt
002600020404     c                   if        una = *blank
002700020404     c                   except    testata
002800020404     c                   except    noncene
002900020404     c                   endif
003000020308     c                   seton                                        lr
003100990113     c*-------------------------------------------------
003200090424     c     fitgt         begsr
003300990113     c*-------------------------------------------------
003400990113     c                   setoff                                           99
003500020404     c                   clear                   una               1
003600090424     c     *loval        setll     fitgt01l
003700981029     c                   do        *hival
003800090424     c                   read      fitgt01l                               99
003900981029     c   99              leave
003901090424     c*
003902090424     c                   if        tgtsml <> ' ' or tgtatb <> ' '
003903090424     c                   iter
003904090424     c                   end
005800020404      * stampo le tariffe con data stampa impostata e data convalida a 0
005900090424     c                   if        tgtdrc = 0 and tgtdcn <> 0
006000021203     c     kapdf         chain     fiapd01l                           97
006100030204      *se l'anagrafica non corrisponde non stampo la tariffa
006200030204     c   97              iter
006300020404
006400020404     c                   if        line > 60
006500020404     c                   except    testata
006600020404     c                   endif
006700020404     c                   move      'X'           una
006701090424     c                   if        tgtdcn <> 0
006800090424     c                   move      tgtdcn        dataiso
006900020404     c                   move      dataiso       dataeur
007000090424     c                   move      dataeur       tgtdcns           8 0
007001090424     c                   else
007002090424     c                   clear                   tgtdcns
007003090424     c                   end
007004090424     c                   if        tgtdst <> 0
007100090424     c                   move      tgtdst        dataiso
007200020404     c                   move      dataiso       dataeur
007300090424     c                   move      dataeur       tgtdsts           8 0
007301090424     c                   else
007302090424     c                   clear                   tgtdsts
007303090424     c                   end
007304090424     c                   if        tgtddt <> 0
007400090424     c                   move      tgtddt        dataiso
007500020404     c                   move      dataiso       dataeur
007600090424     c                   move      dataeur       tgtddts           8 0
007700020404     c                   except    detta
007701090424     c                   else
007702090424     c                   clear                   tgtddts
007703090424     c                   end
007800020404
007900020114     c                   end
008000020404
008100020304     c                   enddo
008200020304
008300020304     c                   endsr
008400981029      *
008500011026     c*-------------------------------------------------
008600011026     c     *inzsr        begsr
008700011026     c*-------------------------------------------------
008800020304     c     *entry        plist
008900020304     c                   parm                    kpjba
009000011026
009100020218     C                   time                    w0120            14 0
009200020218     C                   move      w0120         wdat              8 0
009300020404     c                   z-add     99            line
009400021203     c     kapdf         klist
009500090424     c                   kfld                    apdtip
009600090424     c                   kfld                    tgtpdr
009601090424     c                   movel     'A'           apdtip
009800070307     c* reperisco i po legati contabilmente
009900070307     C                   Z-ADD     1             CODUT
010000070307     C                   CALL      'X§PARUT'
010100070307     C                   PARM                    UT§DSE
010200070307     C                   MOVEL     RAGUT         RSUT             20
010300070307     C                   MOVEL     REC80         CNCR80
011100070307
011200070307     c                   endsr
011300020404     C*--------------------------------------------------------------------
011400020404     Oqsysprt   E            testata        1 02
011500090507     o                                           10 'FICN09R'
011600020404     o                       knmus               22
011700021106     o                                        +   3 'Lista tariffe firmate  -
011800021106     o                                              non ancora pervenute'
011900020404     o                       udate         y  +   2
012000020404     O                       page          z  +   1
012100020404     O          E            testata     1  1
012200090424     o                                           20 'Autotrasportatore'
012500020404     o                                           60 'Prog'
012600020404     o                                           74 'Data Stampa'
012700020404     o                                           87 'Data Decor.'
012800020404     o                                          100 'Data Scad. '
012900021106     o                                          112 'Escl.da Fat'
013000020404     O          E            detta       1
013100090424     o                       tgtpdr               9
013200100223     o                       apdrsf           +   2
013500090424     o                       tgtprg        z     59
013600090424     o                       tgtdcns             74 '  /  /    '
013700090424     o                       tgtddts             86 '  /  /    '
013800090424     o                       tgtdsts             98 '  /  /    '
013900021106     o                       apdpdd             106
014000020404     O          E            noncene     3
014100020404     o                                           80 'N O N   E S I S T O N O   -
014200090424     o                                              T A R I F F E   D A   R I C-
014300090424     o                                               E V E R E'
