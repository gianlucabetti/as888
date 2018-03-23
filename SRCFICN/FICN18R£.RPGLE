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
001200140117     D**** TRUL72DS      E DS
001300140117     D****  L72                           3s 0 DESCEND
001400140117     D****                                     DIM(60)
001500140117     D****                                     OVERLAY(O72Lia)
001600140117      ****-?Caricamento filiali contabili
001700140117     d TRUL31ds      e ds                  inz
001800140117     d $POg            s              3    inz   dim(250)
001900140117      ****-?
002000020404     d prtds           ds
002100020404     d   spab                         3  0
002200020404     d   spaa                         3  0
002300020404     d   skab                         3  0
002400020404     d   skaa                         3  0
002500020404     d   line                         3  0
002600020305     d dataiso         s               d   datfmt(*iso)
002700020305     d dataeur         s               d   datfmt(*eur)
002800000000     C*---------------------------------------------------------------*
002900020304      *
003000020404     c                   exsr      fifgt
003100020404     c                   if        una = *blank
003200020404     c                   except    testata
003300020404     c                   except    noncene
003400020404     c                   endif
003500020308     c                   seton                                        lr
003600990113     c*-------------------------------------------------
003700011026     c     fifgt         begsr
003800990113     c*-------------------------------------------------
003900990113     c                   setoff                                           99
004000020404     c                   clear                   una               1
004100020404     c     *loval        setll     fifgt03l
004200981029     c                   do        *hival
004300020404     c                   read      fifgt03l                               99
004400981029     c   99              leave
004500090424     c* rimane per il RAM solo per le tariffe senza supertestata
004600090424     c                   if        fgtprg >= 200 and tipo = 'A'
004700090424     c                   iter
004800090424     c                   end
004900030522      *effettua il controllo solo se non è un profilo di sede
005000030522     c                   if        simpou <> 046
005100030505     c                   movel     fgtpdr        filpdr            3 0
005200140117     c**** filpdr        lookup    l72                                    98
005300140117     C                   move      filpdr        Fil03A            3
005400140117     C     Fil03A        LOOKUP    $POg                                   98
005500030505     c  n98              iter
005600030522     c                   endif
005700060322      * verifica se tariffa autisti
005800060324     c                   if        tipo = 'A'
005900060322     c                   if        fgttsr = 'X' or fgttsr = 'T' or
006000060322     c                             fgttsr = 'P'
006100060322     c                   iter
006200060322     c                   endif
006300060324     c                   else
006400060324     c                   if        fgttsr = 'C' or fgttsr = 'R' or
006500060324     c                             fgttsr = 'G'
006600060324     c                   iter
006700060324     c                   endif
006800060324     c                   endif
006900020404      * stampo le tariffe con data stampa impostata e data convalida a 0
007000020528     c                   if        fgtdts > 0 and fgtdcn = 0 and fgtsml = *blank
007100021203     c     kapdf         chain     fiapd01l                           97
007200030204      *se l'anagrafica non corrisponde non stampo la tariffa
007300030204     c   97              iter
007400020404
007500020404     c                   if        line > 60
007600020404     c                   except    testata
007700020404     c                   endif
007800020404     c                   move      'X'           una
007900020404     c                   move      fgtdts        dataiso
008000020404     c                   move      dataiso       dataeur
008100020404     c                   move      dataeur       fgtdtss           8 0
008200020404     c                   move      fgtdst        dataiso
008300020404     c                   move      dataiso       dataeur
008400020404     c                   move      dataeur       fgtdsts           8 0
008500020404     c                   move      fgtddt        dataiso
008600020404     c                   move      dataiso       dataeur
008700020404     c                   move      dataeur       fgtddts           8 0
008800020404     c                   except    detta
008900020404
009000020114     c                   end
009100020404
009200020304     c                   enddo
009300020304
009400020304     c                   endsr
009500981029      *
009600011026     c*-------------------------------------------------
009700011026     c     *inzsr        begsr
009800011026     c*-------------------------------------------------
009900020304     c     *entry        plist
010000020304     c                   parm                    kpjba
010100011026
010200020218     C                   time                    w0120            14 0
010300020218     C                   move      w0120         wdat              8 0
010400020404     c                   z-add     99            line
010500021203     c     kapdf         klist
010600030203     c                   kfld                    tipo
010700021203     c                   kfld                    fgtpdr
010800030203     c                   movel     kpjbu         tipo
010900070307     c* reperisco i po legati contabilmente
011000070307     C                   Z-ADD     1             CODUT
011100070307     C                   CALL      'X§PARUT'
011200070307     C                   PARM                    UT§DSE
011300070307     C                   MOVEL     RAGUT         RSUT             20
011400070307     C                   MOVEL     REC80         CNCR80
011500090424     c                   if        simpou = 046 and tipo = 'A'
011600090424     c                   seton                                        lr
011700090424     c                   return
011800090424     c                   end
011900140117     C***
012000140117     c***                clear                   trul72ds
012100140117     c***                eval      i72tla = 'L'
012200140117     c***                eval      I72POU = simpou
012300140117     c***                eval      kpjbu = trul72ds
012400140117     C***                CALL      'TRUL72R'
012500140117     C***                PARM                    KPJBA
012600140117     c***                movel     kpjbu         trul72ds
012700140117     C***
012800140117      * Reperimento dei P.O. di CONTABILITA' gestibili dalla filiale
012900140117     c                   clear                   TRUL31DS
013000140117     c                   eval      I31abi = 'AC'
013100140117     c                   eval      I31cpo = simpou
013200140117     c                   call      'TRUL31R'
013300140117     c                   parm                    KPJBA
013400140117     c                   parm                    TRUL31DS
013500140117     c                   if        o31pog > *zeros
013600140117     c                   movea     o31pog        $pog
013700140117     c                   endif
013800140117
013900070307
014000070307     c                   endsr
014100020404     C*--------------------------------------------------------------------
014200020404     Oqsysprt   E            testata        1 02
014300020404     o                                           10 'FICN18R'
014400020404     o                       knmus               22
014500021106     o                                        +   3 'Lista tariffe firmate  -
014600021106     o                                              non ancora pervenute'
014700020404     o                       udate         y  +   2
014800020404     O                       page          z  +   1
014900020404     O          E            testata     1  1
015000030204     o                                           20 'Autotrasp./Coop'
015100020404     o                                           48 'T.S.'
015200020404     o                                           55 'C.tar'
015300020404     o                                           60 'Prog'
015400020404     o                                           74 'Data Stampa'
015500020404     o                                           87 'Data Decor.'
015600020404     o                                          100 'Data Scad. '
015700021106     o                                          112 'Escl.da Fat'
015800020404     O          E            detta       1
015900020404     o                       fgtpdr               9
016000100223     o                       apdrsf           +   2
016100020404     o                       fgttsr              47
016200020404     o                       fgtctr              54
016300020404     o                       fgtprg        z     59
016400020404     o                       fgtdtss             74 '  /  /    '
016500021106     o                       fgtddts             86 '  /  /    '
016600021106     o                       fgtdsts             98 '  /  /    '
016700021106     o                       apdpdd             106
016800020404     O          E            noncene     3
016900020404     o                                           80 'N O N   E S I S T O N O   -
017000020404     o                                              T A R I F F E   D A  C O N -
017100020404     o                                              V A L I D A R E'
