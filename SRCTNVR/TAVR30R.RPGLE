000100040329     H DECEDIT('0,') DATEDIT(*DMY.)
000200040329     H*         *----------------------------------------------------*
000300040329     H*       comodo
000400040329     H*--------------------------------------------------------------*
000500041028     FFNCLS01L  uf   e           k disk
000600041028     fcnaco00f  uf   e           k disk
000700041028     ftitgc01l  uf   e           k disk
000800041027      *
000900041027     fprtf198   o    f  132        printer  oflind(*inof)
001000041027      *
001100041028     d sktcm           s              1    dim(30)                              tipo com. mitten.
001200041028     d sktfg           s              1    dim(30)                              tipo fine giacen.
001300041027      *
001400041028     d cm              s              3  0
001500041028     d fg              s              3  0
001600041028     d tot_casoa1      s              9  0
001700041028     d tot_casoa2      s              9  0
001800041028     d tot_casob1      s              9  0
001900041028     d tot_casob2      s              9  0
002000041028     d tot_casoc1      s              9  0
002100041028     d tot_casoc2      s              9  0
002200041028     d tot_pulit1      s              9  0
002300041028     d tot_pulit2      s              9  0
002400041028     d tot_diver1      s              9  0
002500041028     d tot_diver2      s              9  0
002600041028
002700041028     d flg_casoTCM     s              1
002800041028     d flg_casoTFG     s              1
002900041028
003000041028     d keycut          s                   like(acokut) inz(1)
003100041028     d keykcc          s                   like(acokcc) inz(0151)
003200041028     d keyksc          s                   like(acoksc)
003300041027
003400041027     D                 DS
003500041027     D  VIDtfg                 1      1
003600041027     D  VIDtcm                10     10
003700041027     D  FLG                    1     10
003800041027     D                                     DIM(10)
003900041027      *
004000041028     c     keyaco        klist
004100041028     c                   kfld                    keycut
004200041028     c                   kfld                    keykcc
004300041028     c                   kfld                    keyksc
004400041028
004500041027     c                   except    testa
004600041027     c                   do        *hival
004700041027     c                   read      fncls01l
004800041027      *
004900041027     c                   if        %eof(fncls01l)
005000041027     c                   leave
005100041027     c                   endif
005200041027      *
005300041027     c                   movea     clsflo        flg
005400041027      * leggo  tutte le tariffe
005500041027     c     clsksc        setll     titgc01l                               30
005600041027      * se setll non riuscita leggo il cliente successivo
005700041027     c                   if        not *in30
005800041027     c                   iter
005900041027     c                   endif
006000041027      *
006100041028     c                   clear                   sktcm
006200041028     c                   clear                   sktfg
006300041028     c                   clear                   cm
006400041028     c                   clear                   fg
006500041028     c                   clear                   flg_casoTCM
006600041028     c                   clear                   flg_casoTFG
006700041028
006800041027     c                   do        *hival
006900041027      *
007000041027     c     clsksc        reade     titgc01l
007100041027      *
007200041027     c                   if        %eof(titgc01l)
007300041027     c                   exsr      SR_contr
007400041027     c                   leave
007500041027     c                   endif
007600041028      *
007700041028     c     tgctcm        lookup    sktcm                                  30
007800041028     c                   if        not %found
007900041028      *
008000041028     c                   add       1             cm
008100041028     c                   movea     tgctcm        sktcm(cm)
008200041028      *
008300041028     c                   endif
008400041027
008500041028      *
008600041028     c     tgctfg        lookup    sktfg                                  30
008700041028     c                   if        not %found
008800041028      *
008900041028     c                   add       1             fg
009000041028     c                   movea     tgctfg        sktfg(fg)
009100041028      *
009200041028     c                   endif
009300041028
009400041027      *
009500041027     c                   enddo
009600041027      *
009700041027     c                   enddo
009800041027      *
009900041028      * stampo i totali
010000041028      *
010100041028     c                   except    totali
010200041028
010300041027     c                   seton                                        lr
010400041027      *
010500041027      *-----------------------------------------------------------------------------
010600041027      * controllo le casistiche per elaborare
010700041027      *-----------------------------------------------------------------------------
010800041027     c     Sr_contr      begsr
010900041027      *
011000041028      *
011100041028     c                   if        *inof
011200041027     c                   except    testa
011300041027     c                   setoff                                       of
011400041027     c                   endif
011500041027
011600041028      * tipo comunicazione
011700041028      * tariffa è a blank
011800041028     c                   If        cm = 0
011900041028     c                   except    sta_pul1
012000041028     c                   add       1             tot_pulit1
012100041028     c                   move      'P'           flg_CASOTCM
012200041028     c                   endif
012300041028      *
012400041028      * tariffa non è a blanks ma  è uguale all'anagrafica
012500041028     c                   if        cm = 1 and vidtcm = sktcm(cm)
012600041028     c                   except    sta_casoa1
012700041028     c                   add       1             tot_casoa1
012800041028     c                   move      'A'           flg_casoTCM
012900041028     c                   endif
013000041028      *                            è diverso da angrafica
013100041028     c                   if        cm = 1 and vidtcm <> sktcm(cm)
013200041028      * se anagrafica non è invio mail devo aggiornare ANA con i valori della tariffa
013300041028     c                   if        vidtcm <> 'W' and vidtcm <> 'V'
013400041028     c                   except    sta_casob1
013500041028     c                   add       1             tot_casob1
013600041028     c                   move      'B'           flg_casoTCM
013700041028      * se anagrafica è via E-mail e tariffa uguale fax aggiorno altrimenti non faccio nulla
013800041028     c                   else
013900041028      *
014000041028     c                   if        sktcm(cm)='E' or sktcm(cm)='F'
014100041028     c                   except    sta_casoc1
014200041028     c                   add       1             tot_casoc1
014300041028     c                   move      'C'           flg_casoTCM
014400041028     c                   else
014500041028     c                   except    sta_div1
014600041028     c                   add       1             tot_diver1
014700041028     c                   move      'D'           flg_CASOTCM
014800041028     c                   endif
014900041028     c                   endif
015000041028     c                   endif
015100041028      * se diversi tipi comuniazione stampo che sono diversi e basta
015200041028     c                   if        cm > 1
015300041028     c                   except    sta_div1
015400041028     c                   add       1             tot_diver1
015500041028     c                   move      'D'           flg_casoTCM
015600041028     c                   endif
015700041028      * tipo invio comunicazione fine giacenza
015800041028      * tariffa è a blank
015900041028     c                   If        fg = 0
016000041028     c                   except    sta_pul2
016100041028     c                   add       1             tot_pulit2
016200041028     c                   move      'P'           flg_casoTFG
016300041028     c                   endif
016400041028      *
016500041028      * tariffa non è a blanks ma  è uguale all'anagrafica
016600041028     c                   if        fg = 1 and vidtfg = sktfg(fg)
016700041028     c                   except    sta_casoa2
016800041028     c                   add       1             tot_casoa2
016900041028     c                   move      'A'           flg_casoTFG
017000041028     c                   endif
017100041028      *                            è diverso da angrafica
017200041028     c                   if        fg = 1 and vidtfg <> sktfg(fg)
017300041028      * se anagrafica non è invio mail devo aggiornare ANA con i valori della tariffa
017400041028     c                   if        vidtfg <> 'M' and vidtfg <> 'V'
017500041028     c                   except    sta_casob2
017600041028     c                   add       1             tot_casob2
017700041028     c                   move      'B'           flg_casoTFG
017800041028      * se anagrafica è via E-mail e tariffa uguale fax aggiorno altrimenti non faccio nulla
017900041028     c                   else
018000041028      *
018100041028     c                   if        sktfg(fg)='X' or sktfg(fg)='F'
018200041028     c                   except    sta_casoc2
018300041028     c                   add       1             tot_casoc2
018400041028     c                   move      'C'           flg_casoTFG
018500041028     c                   else
018600041028     c                   except    sta_div2
018700041028     c                   add       1             tot_diver2
018800041028     c                   move      'D'           flg_casoTFG
018900041028     c                   endif
019000041028     c                   endif
019100041028     c                   endif
019200041028      * se diversi tipi comuniazione stampo che sono diversi e basta
019300041028     c                   if        fg > 1
019400041028     c                   except    sta_div2
019500041028     c                   add       1             tot_diver2
019600041028     c                   move      'D'           flg_CasoTFG
019700041028     c                   endif
019800041028      *
019900041028      *              elaboro
020000041028     c                   SELECT
020100041028
020200041028     c                   When      (flg_casoTCM = 'P' or flg_casoTCM = 'D') and
020300041028     c                             (flg_casoTFG = 'P' or flg_casoTFG = 'D')
020400041028
020500041028     c                   other
020600041028      *
020700041028      * leggo  tutte le tariffe
020800041028     c     clsksc        setll     titgc01l                               30
020900041028     c                   do        *hival
021000041028     c     clsksc        reade     titgc01l
021100041028
021200041028     c                   if        %eof(titgc01l)
021300041028     c                   leave
021400041028     c                   endif
021500041028      * TCM
021600041029     c                   if        FLG_casoTCM = 'C'
021700041028     c                   clear                   tgctcm
021800041028     c                   endif
021900041028     c                   update    titgc000
022000041028
022100041028     c                   enddo
022200041028
022300041028     c                   endsl
022400041028      *
022500041027     c                   endsr
022600041027      *---------------------------------------------------------------------------------------------
022700041027     OPRTF198   E            testa            01
022800041027     O                                        +   1 'Casi differenziati in base'
022900041028     O                                              ' ai flag di giacenza'
023000041028     O          E            Sta_pul1    1
023100041027     O                                        +   1 'Cliente'
023200041027     O                       clsksc           +   1
023300041028     O                                        +  10 'Ha TCM in tariffa pulite  '
023400041028     O          E            Sta_casoc1  1
023500041027     O                                        +   1 'Cliente'
023600041027     O                       clsksc           +   1
023700041028     O                                        +  10 'Caso_C TCM Tar=fax TCM Ana'
023800041028     o                                        +   1 '=Mail'
023900041028     O                                        +   1 'TARTCM'
024000041028     O                       SKTCM(CM)        +   1
024100041028     O                                        +   1 'CLSTCM'
024200041028     O                       VIDTCM           +   1
024300041028     O          E            Sta_casob1  1
024400041027     O                                        +   1 'Cliente'
024500041027     O                       clsksc           +   1
024600041028     O                                        +  10 'Caso_B TCM Tar <> TCM Ana '
024700041028     O                                        +   1 'TARTCM'
024800041028     O                       SKTCM(CM)        +   1
024900041028     O                                        +   1 'CLSTCM'
025000041028     O                       VIDTCM           +   1
025100041028     O          E            Sta_casoa1  1
025200041027     O                                        +   1 'Cliente'
025300041027     O                       clsksc           +   1
025400041028     O                                        +  10 'Caso_A TCM Tar. = TCM Ana '
025500041028     O          E            Sta_div1    1
025600041028     O                                        +   1 'Cliente'
025700041028     O                       clsksc           +   1
025800041028     O                                        +  10 'Ha TCM tutti diversi :'
025900041028     O                       cm            z  +   1
026000041028     O          E            Sta_pul2    0
026100041028     O                                        +  80 'Ha TFG in tariffa pulite  '
026200041028     O          E            Sta_casoc2  0
026300041028     O                                        +  80 'Caso_C TFG Tar=fax TFG Ana'
026400041028     o                                        +   1 '=Mail'
026500041028     O                                        +   1 'TARTFG'
026600041028     O                       SKTFG(FG)        +   1
026700041028     O                                        +   1 'CLSTFG'
026800041028     O                       VIDTFG           +   1
026900041028     O          E            Sta_casob2  0
027000041028     O                                        +  80 'Caso_B TFG Tar <> TFG Ana '
027100041028     O                                        +   1 'TARTFG'
027200041028     O                       SKTFG(FG)        +   1
027300041028     O                                        +   1 'CLSTFG'
027400041028     O                       VIDTFG           +   1
027500041028     O          E            Sta_casoa2  0
027600041028     O                                        +  80 'Caso_A TFG Tar. = TFG Ana '
027700041028     O          E            Sta_div2    0
027800041028     O                                        +  80 'Ha TFG tutti diversi :'
027900041028     O                       FG            z  +   1
028000041028     O          e            totali      2
028100041028     O                                        +   1 'T O T A L I'
028200041028     O                                        +   1 'TCM'
028300041028     O                                        +   1 'PULITI'
028400041028     O                       tot_pulit1    z  +   0
028500041028     O                                        +   2 'DIVERSI'
028600041028     O                       tot_diver1    z  +   0
028700041028     O                                        +   2 'ANA = TAR'
028800041028     O                       tot_casoa1    z  +   0
028900041028     O                                        +   2 'ANA <> TAR vince TAR'
029000041028     O                       tot_casob1    z  +   0
029100041028     O                                        +   2 'ANA <> TAR vince ANA'
029200041028     O                       tot_casoc1    z  +   0
029300041028     O          e            totali      2
029400041028     O                                        +   1 'T O T A L I'
029500041028     O                                        +   1 'TFG'
029600041028     O                                        +   1 'PULITI'
029700041028     O                       tot_pulit2    z  +   0
029800041028     O                                        +   2 'DIVERSI'
029900041028     O                       tot_diver2    z  +   0
030000041028     O                                        +   2 'ANA = TAR'
030100041028     O                       tot_casoa2    z  +   0
030200041028     O                                        +   2 'ANA <> TAR vince TAR'
030300041028     O                       tot_casob2    z  +   0
030400041028     O                                        +   2 'ANA <> TAR vince ANA'
030500041028     O                       tot_casoc2    z  +   0
