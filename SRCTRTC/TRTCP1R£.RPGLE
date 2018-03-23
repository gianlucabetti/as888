000100080423     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200080423     H*   ATTENZIONE
000300080423     H*   COMPILARE IN INTERATTIVO DOPO AVER CREATO IL FILE IN QTEMP
000400080423     H*
000500080423     H* ?    CRTDUPOBJ  OBJ(FIBAR00F) FROMLIB(*LIBL) OBJTYPE(*FILE)
000600080423     H* ?                 TOLIB(QTEMP) NEWOBJ(FIBARCOF)
000700080423     H*
000800000000     H*--------------------------------------------------------------*
000900080624     Ffidst01l  if   e           k disk
001000080423     FFIPCD03L  IF   E           k DISK
001100080520     FFIBARCOF  O    E             DISK    usropn
001200070214     d
001300080423     dfipcdcedds     e ds
001400080624     d ddstflr       e ds
001500080624     d cat4            s              1  0 inz(4)
001600080423     d
001700000606     D WLBDAT          DS                  INZ
001800000606     D  G02DAT                 1      8  0
001900000606     D  G02INV                 9     16  0
002000000606     D  G02ERR                17     17
002100000606     D  G02TGI                18     22  0
002200070214     c*---------------------------------------------------------------------
002300070214     c     *entry        plist
002400080423     c                   parm                    parfgs            3
002500080423     c                   parm                    pardis            6
002600080423     c                   parm                    parspc            2
002700080423
002800080423     c                   move      parfgs        fgs               3 0
002900080423     c                   move      pardis        ndc               6 0
003000080423     c                   move      'CED'         trd               3
003100080624     c     kdst          klist
003200080624     c                   kfld                    cat4
003300080624     c                   kfld                    ndc
003400080624     c                   kfld                    fgs
003500080624
003600080423
003700080423     c     kpcd          klist
003800080423     c                   kfld                    FGS
003900080423     c                   kfld                    NDC
004000080423     c                   kfld                    TRD
004100080520     c                   open      fibarcof
004200080624      * se fase di test esco senza fare nulla
004300080624     c                   clear                   ddstflr
004400080624     c     kdst          chain     fidst01l
004500080624     c                   if        %found(fidst01l)
004600080624     c                   movel     dstflr        ddstflr
004700080624     c                   endif
004800080721     c                   if        §DSTTSTPDA <> *blank
004900080624     c                   return
005000080624     c                   endif
005100921221     C*
005200080423     C     kpcd          setll     FIpcd03l
005300080423     c                   do        *hival
005400080423     c     kpcd          reade     fipcd03l
005500080423     c                   if        %eof(fipcd03l)
005600080423     c                   leave
005700080423     c                   endif
005800080423     c                   movel     pcddati       fipcdcedds
005900080423     c                   if        §pcdspuspc <> 'S'
006000080423     c                   iter
006100080423     c                   endif
006200080520     c                   clear                   barco
006300080714     c                   move      pcdfls        BARLNP
006400080423     c                   move      pcdLNA        BARLNA
006500080423     c                   move      pcdNRS        BARNRS
006600080423     c                   move      pcdNSC        BARNSC
006700080423     c                   move      BARZNC        BARZNC
006800080423     c                   move      pcdndc        BARNFV
006900080423     c                   move      47            BARNPS
007000080423     c                   move      8             BARNPG
007100080423     c                   movel     pcddtorin     BARDFS
007200080423     c                   move      pcddtorin     BARHMS
007300080423     c                   eval      BARPRU = 'SPC' + parspc
007400080423     c                   move      'P'           BARTAP
007500080520     c                   write     barco
007600080423     c                   enddo
007700080520     c                   close     fibarcof
007800070214     c
007900080423     C                   SETON                                            rt
