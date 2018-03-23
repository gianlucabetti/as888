000100141127     H*
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020404     Ffifgt03l  if   e           k disk
000500021203     Ffiapd01l  if   e           k disk
000600020404     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
000700020304     dkpjba          e ds
000800030203     d tipo            s              1
000900030505     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001000030505     D CNCR80        E DS
001100140117      ****-?
001200020404     d prtds           ds
001300020404     d   spab                         3  0
001400020404     d   spaa                         3  0
001500020404     d   skab                         3  0
001600020404     d   skaa                         3  0
001700020404     d   line                         3  0
001800020305     d dataiso         s               d   datfmt(*iso)
001900020305     d dataeur         s               d   datfmt(*eur)
002000000000     C*---------------------------------------------------------------*
002100020304      *
002200020404     c                   exsr      fifgt
002300020404     c                   if        una = *blank
002400020404     c                   except    testata
002500020404     c                   except    noncene
002600020404     c                   endif
002700020308     c                   seton                                        lr
002800990113     c*-------------------------------------------------
002900011026     c     fifgt         begsr
003000990113     c*-------------------------------------------------
003100990113     c                   setoff                                           99
003200020404     c                   clear                   una               1
003300020404     c     *loval        setll     fifgt03l
003400981029     c                   do        *hival
003500020404     c                   read      fifgt03l                               99
003600981029     c   99              leave
003700090424     c* rimane per il RAM solo per le tariffe senza supertestata
003800090424     c                   if        fgtprg >= 200 and tipo = 'A'
003900090424     c                   iter
004000090424     c                   end
004100060322      * verifica se tariffa autisti
004200060324     c                   if        tipo = 'A'
004300060322     c                   if        fgttsr = 'X' or fgttsr = 'T' or
004400060322     c                             fgttsr = 'P'
004500060322     c                   iter
004600060322     c                   endif
004700060324     c                   else
004800060324     c                   if        fgttsr = 'C' or fgttsr = 'R' or
004900060324     c                             fgttsr = 'G'
005000060324     c                   iter
005100060324     c                   endif
005200060324     c                   endif
005300020404      * stampo le tariffe con data stampa impostata e data convalida a 0
005400020528     c                   if        fgtdts > 0 and fgtdcn = 0 and fgtsml = *blank
005500021203     c     kapdf         chain     fiapd01l                           97
005600030204      *se l'anagrafica non corrisponde non stampo la tariffa
005700030204     c   97              iter
005800020404
005900020404     c                   if        line > 60
006000020404     c                   except    testata
006100020404     c                   endif
006200020404     c                   move      'X'           una
006300020404     c                   move      fgtdts        dataiso
006400020404     c                   move      dataiso       dataeur
006500020404     c                   move      dataeur       fgtdtss           8 0
006600020404     c                   move      fgtdst        dataiso
006700020404     c                   move      dataiso       dataeur
006800020404     c                   move      dataeur       fgtdsts           8 0
006900020404     c                   move      fgtddt        dataiso
007000020404     c                   move      dataiso       dataeur
007100020404     c                   move      dataeur       fgtddts           8 0
007200020404     c                   except    detta
007300020404
007400020114     c                   end
007500020404
007600020304     c                   enddo
007700020304
007800020304     c                   endsr
007900981029      *
008000011026     c*-------------------------------------------------
008100011026     c     *inzsr        begsr
008200011026     c*-------------------------------------------------
008300020304     c     *entry        plist
008400020304     c                   parm                    kpjba
008500011026
008600020218     C                   time                    w0120            14 0
008700020218     C                   move      w0120         wdat              8 0
008800020404     c                   z-add     99            line
008900021203     c     kapdf         klist
009000030203     c                   kfld                    tipo
009100021203     c                   kfld                    fgtpdr
009200030203     c                   movel     kpjbu         tipo
009300070307     c* reperisco i po legati contabilmente
009400070307     C                   Z-ADD     1             CODUT
009500070307     C                   CALL      'X§PARUT'
009600070307     C                   PARM                    UT§DSE
009700070307     C                   MOVEL     RAGUT         RSUT             20
009800070307     C                   MOVEL     REC80         CNCR80
009900090424     c                   if        simpou = 046 and tipo = 'A'
010000090424     c                   seton                                        lr
010100090424     c                   return
010200090424     c                   end
010300140117     C***
010400140117     c***                clear                   trul72ds
010500140117     c***                eval      i72tla = 'L'
010600140117     c***                eval      I72POU = simpou
010700140117     c***                eval      kpjbu = trul72ds
010800140117     C***                CALL      'TRUL72R'
010900140117     C***                PARM                    KPJBA
011000140117     c***                movel     kpjbu         trul72ds
011100140117     C***
011200070307
011300070307     c                   endsr
011400020404     C*--------------------------------------------------------------------
011500020404     Oqsysprt   E            testata        1 02
011600020404     o                                           10 'FICN18R'
011700020404     o                       knmus               22
011800021106     o                                        +   3 'Lista tariffe firmate  -
011900021106     o                                              non ancora pervenute'
012000020404     o                       udate         y  +   2
012100020404     O                       page          z  +   1
012200020404     O          E            testata     1  1
012300030204     o                                           20 'Autotrasp./Coop'
012400020404     o                                           48 'T.S.'
012500020404     o                                           55 'C.tar'
012600020404     o                                           60 'Prog'
012700020404     o                                           74 'Data Stampa'
012800020404     o                                           87 'Data Decor.'
012900020404     o                                          100 'Data Scad. '
013000021106     o                                          112 'Escl.da Fat'
013100020404     O          E            detta       1
013200020404     o                       fgtpdr               9
013300100223     o                       apdrsf           +   2
013400020404     o                       fgttsr              47
013500020404     o                       fgtctr              54
013600020404     o                       fgtprg        z     59
013700020404     o                       fgtdtss             74 '  /  /    '
013800021106     o                       fgtddts             86 '  /  /    '
013900021106     o                       fgtdsts             98 '  /  /    '
014000021106     o                       apdpdd             106
014100020404     O          E            noncene     3
014200020404     o                                           80 'N O N   E S I S T O N O   -
014300020404     o                                              T A R I F F E   D A  C O N -
014400020404     o                                              V A L I D A R E'
