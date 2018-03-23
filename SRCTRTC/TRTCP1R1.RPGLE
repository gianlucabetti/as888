000100080423     H DECEDIT('0,') DATEDIT(*YMD.) option(*nodebugio)
000200080423     H*
000300000000     H*--------------------------------------------------------------*
000400080512     ffidsf02l  o    e             disk
000500080624     ffidst01l  if   e           k disk
000600080512     ffiqdt01l  uf   e           k disk
000700080423     d
000800080423     d fiprdds       e ds                  extname(fiprd00r)
000900080423     d fiprdCRIds    e ds
001000080728     d fidsfCRIds    e ds
001100080624     d ddstflr       e ds
001200080423     d cat4            s              1  0 inz(4)
001300080423     d tmp             s             14
001400080728     d dpsp          e ds
001500080728     d tibs02ds      e ds
001600080728     d kpjba         e ds
001700081211     d codspc          s              2    dim(22)
001800081211     d desspc          s             15    dim(22)
001900070214     c*---------------------------------------------------------------------
002000070214     c     *entry        plist
002100080423     c                   parm                    fiprdds
002200080423
002300080423     c                   movel     prddati       fiprdCRIds
002400080423
002500080423     c     kqdt          klist
002600080423     c                   kfld                    §criFGS
002700080423     c                   kfld                    §criNDC
002701170714     c                   kfld                    dstdfv
002800080624     C*
002900080624     c     kdst          klist
003000080624     c                   kfld                    cat4
003100080624     c                   kfld                    §criNDC
003200080624     c                   kfld                    §criFGS
003300080624
003400080423      *scrittura fase
003500080423     c                   clear                   fidsf000
003600080728     c                   clear                   fidsfcrids
003700080423     c                   eval      DSFfgs    =   §criFGS
003800080423     c                   eval      DSFnpg    =   cat4
003900080423     c                   eval      DSFnfv    =   §criNDC
004000080424     c                   eval      DSFtrd    =   prdtiporcd
004100080728     c                   eval      §DSFNRBANR=   §CRINRBANR
004200081112     c                   eval      §DSFNRcolR=   §CRINRcolR
004300080728     c                   eval      §DSFSPCCOD=   §CRISPCCOD
004400080728     c                   exsr      decspc
004500080728      *
004600080728     c                   z-add     1             i                 2 0
004700080728     c     §CriSPCCOD    lookup    codspc(i)                              66
004800080728     c   66              eval      §DSFSPCDES=   desspc(i)
004900080728     c                   movel     FIDSFCriDS    DSFDATI
005000080728      *
005100080423     c                   eval      tmp= %char(%timestamp: *iso0)
005200080423     c                   move      tmp           DSFdtorin
005300080423     c                   eval      DSFPDAAS4 = 'P'
005400080423     c                   write     fidsf000
005500080624      *scrittura bancali per rientri se autista non in fase di test
005600080624     c                   clear                   ddstflr
005700080624     c     kdst          chain     fidst01l
005800080624     c                   if        %found(fidst01l)
005900080624     c                   movel     dstflr        ddstflr
006000080624     c                   endif
006100080624     c                   if        §DSTTSTPDA = *blank
006200080423     c     kqdt          chain     fiqdt01l
006300080423     c                   if        %found(fiqdt01l)
006400080423     c                   add       §CRINRBANR    QDTNBNRI
006500081111     c                   move      'S'           QDTFNBRI
006600080423     c                   z-add     §CRINRCOLR    QDTNCLNC
006700081111     c                   movel     §CRIDTORIN    QDTDAIRI
006800081111     c                   move      §CRIDTORIN    comodo6           6 0
006900081111     c                   movel     comodo6       QDTORARI
007000080423     c                   update    fiqdt000
007100080423     c                   endif
007200080624     c                   endif
007300070214     c
007400080423     C                   SETON                                            rt
007500080728      * ?_______________________________________________________________________
007600080728      * decodifica SPC
007700080728      * ?_______________________________________________________________________
007800080728     c     decspc        begsr
007900080728      * recupero tabella codici SPC (PSP)
008000081211      * chiave 1 per le prime 11 password
008100080728     c                   clear                   tibs02ds
008200080728     c                   clear                   dpsp
008300080728     c                   eval      t02mod = 'C'
008400080728     c                   eval      t02sif = knsif
008500080728     c                   eval      t02cod = 'PSP'
008600080728     c                   movel     §CRiFGS       t02ke1
008700081211     c                   eval      t02ke2 = '1'
008800080728     c                   call      'TIBS02R'
008900080728     c                   parm                    kpjba
009000080728     c                   parm                    tibs02ds
009100080728     c                   if        t02err = *blanks
009200080728     c                   eval      dpsp = t02uni
009300080728     c                   endif
009400080728
009500080728     c                   if        dpsp <> *blanks
009600080728     c                   eval      codspc(01) = §pspcod01
009700080728     c                   eval      codspc(02) = §pspcod02
009800080728     c                   eval      codspc(03) = §pspcod03
009900080728     c                   eval      codspc(04) = §pspcod04
010000080728     c                   eval      codspc(05) = §pspcod05
010100080728     c                   eval      codspc(06) = §pspcod06
010200080728     c                   eval      codspc(07) = §pspcod07
010300080728     c                   eval      codspc(08) = §pspcod08
010400080728     c                   eval      codspc(09) = §pspcod09
010500080728     c                   eval      codspc(10) = §pspcod10
010600081211     c                   eval      codspc(11) = §pspcod11
010700080728     c                   eval      desspc(01) = §pspdes01
010800080728     c                   eval      desspc(02) = §pspdes02
010900080728     c                   eval      desspc(03) = §pspdes03
011000080728     c                   eval      desspc(04) = §pspdes04
011100080728     c                   eval      desspc(05) = §pspdes05
011200080728     c                   eval      desspc(06) = §pspdes06
011300080728     c                   eval      desspc(07) = §pspdes07
011400080728     c                   eval      desspc(08) = §pspdes08
011500080728     c                   eval      desspc(09) = §pspdes09
011600080728     c                   eval      desspc(10) = §pspdes10
011700081211     c                   eval      desspc(11) = §pspdes11
011800080728     c                   endif
011900081211      * chiave 2 per le altre 11 password
012000081211     c                   clear                   dpsp
012100081211     c                   eval      t02ke2 = '2'
012200081211     c                   call      'TIBS02R'
012300081211     c                   parm                    kpjba
012400081211     c                   parm                    tibs02ds
012500081211     c                   if        t02err = *blanks
012600081211     c                   eval      dpsp = t02uni
012700081211     c                   endif
012800081211
012900081211     c                   if        dpsp <> *blanks
013000081211     c                   eval      codspc(12) = §pspcod01
013100081211     c                   eval      desspc(12) = §pspdes01
013200081211     c                   eval      codspc(13) = §pspcod02
013300081211     c                   eval      desspc(13) = §pspdes02
013400081211     c                   eval      codspc(14) = §pspcod03
013500081211     c                   eval      desspc(14) = §pspdes03
013600081211     c                   eval      codspc(15) = §pspcod04
013700081211     c                   eval      desspc(15) = §pspdes04
013800081211     c                   eval      codspc(16) = §pspcod05
013900081211     c                   eval      desspc(16) = §pspdes05
014000081211     c                   eval      codspc(17) = §pspcod06
014100081211     c                   eval      desspc(17) = §pspdes06
014200081211     c                   eval      codspc(18) = §pspcod07
014300081211     c                   eval      desspc(18) = §pspdes07
014400081211     c                   eval      codspc(19) = §pspcod08
014500081211     c                   eval      desspc(19) = §pspdes08
014600081211     c                   eval      codspc(20) = §pspcod09
014700081211     c                   eval      desspc(20) = §pspdes09
014800081211     c                   eval      codspc(21) = §pspcod10
014900081211     c                   eval      desspc(21) = §pspdes10
015000081211     c                   eval      codspc(22) = §pspcod11
015100081211     c                   eval      desspc(22) = §pspdes11
015200081211     c                   endif
015300080728     c                   endsr
