000100040329     H DECEDIT('0,') DATEDIT(*DMY.)
000200040329     H*         *----------------------------------------------------*
000300040329     H*       comodo
000400040329     H*--------------------------------------------------------------*
000500041028     FFNCLS01L  if   e           k disk
000600041028     fcnaco00f  if   e           k disk
000700041028     ftitgc01l  if   e           k disk
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
002700041028     d flg_casoa1      s              1
002800041028     d flg_casoa2      s              1
002900041028     d flg_casob1      s              1
003000041028     d flg_casob2      s              1
003100041028     d flg_casoc1      s              1
003200041028     d flg_casoc2      s              1
003300041028     d flg_pulit1      s              1
003400041028     d flg_pulit2      s              1
003500041028     d flg_diver1      s              1
003600041028     d flg_diver2      s              1
003700041027
003800041027     D                 DS
003900041027     D  VIDtfg                 1      1
004000041027     D  VIDtcm                10     10
004100041027     D  FLG                    1     10
004200041027     D                                     DIM(10)
004300041027      *
004400041027     c                   except    testa
004500041027     c                   do        *hival
004600041027     c                   read      fncls01l
004700041027      *
004800041027     c                   if        %eof(fncls01l)
004900041027     c                   leave
005000041027     c                   endif
005100041027      *
005200041027     c                   movea     clsflo        flg
005300041027      * leggo  tutte le tariffe
005400041027     c     clsksc        setll     titgc01l                               30
005500041027      * se setll non riuscita leggo il cliente successivo
005600041027     c                   if        not *in30
005700041027     c                   iter
005800041027     c                   endif
005900041027      *
006000041028     c                   clear                   sktcm
006100041028     c                   clear                   sktfg
006200041028     c                   clear                   cm
006300041028     c                   clear                   fg
006400041028     c                   clear                   flg_pulit1
006500041028     c                   clear                   flg_diver1
006600041028     c                   clear                   flg_casoa1
006700041028     c                   clear                   flg_casob1
006800041028     c                   clear                   flg_casoc1
006900041028
007000041028     c                   clear                   flg_pulit2
007100041028     c                   clear                   flg_diver2
007200041028     c                   clear                   flg_casoa2
007300041028     c                   clear                   flg_casob2
007400041028     c                   clear                   flg_casoc2
007500041028
007600041027     c                   do        *hival
007700041027      *
007800041027     c     clsksc        reade     titgc01l
007900041027      *
008000041027     c                   if        %eof(titgc01l)
008100041027     c                   exsr      SR_contr
008200041027     c                   leave
008300041027     c                   endif
008400041028      *
008500041028     c     tgctcm        lookup    sktcm                                  30
008600041028     c                   if        not %found
008700041028      *
008800041028     c                   add       1             cm
008900041028     c                   movea     tgctcm        sktcm(cm)
009000041028      *
009100041028     c                   endif
009200041027
009300041028      *
009400041028     c     tgctfg        lookup    sktfg                                  30
009500041028     c                   if        not %found
009600041028      *
009700041028     c                   add       1             fg
009800041028     c                   movea     tgctfg        sktfg(fg)
009900041028      *
010000041028     c                   endif
010100041028
010200041027      *
010300041027     c                   enddo
010400041027      *
010500041027     c                   enddo
010600041027      *
010700041028      * stampo i totali
010800041028      *
010900041028     c                   except    totali
011000041028
011100041027     c                   seton                                        lr
011200041027      *
011300041027      *-----------------------------------------------------------------------------
011400041027      * controllo le casistiche per elaborare
011500041027      *-----------------------------------------------------------------------------
011600041027     c     Sr_contr      begsr
011700041027      *
011800041028      *
011900041028     c                   if        *inof
012000041027     c                   except    testa
012100041027     c                   setoff                                       of
012200041027     c                   endif
012300041027
012400041028      * tipo comunicazione
012500041028      * tariffa è a blank
012600041028     c                   If        cm = 0
012700041028     c                   except    sta_pul1
012800041028     c                   add       1             tot_pulit1
012900041028     c                   move      'Y'           flg_pulit1
013000041028     c                   endif
013100041028      *
013200041028      * tariffa non è a blanks ma  è uguale all'anagrafica
013300041028     c                   if        cm = 1 and vidtcm = sktcm(cm)
013400041028     c                   except    sta_casoa1
013500041028     c                   add       1             tot_casoa1
013600041028     c                   move      'Y'           flg_casoa1
013700041028     c                   endif
013800041028      *                            è diverso da angrafica
013900041028     c                   if        cm = 1 and vidtcm <> sktcm(cm)
014000041028      * se anagrafica non è invio mail devo aggiornare ANA con i valori della tariffa
014100041028     c                   if        vidtcm <> 'W' and vidtcm <> 'V'
014200041028     c                   except    sta_casob1
014300041028     c                   add       1             tot_casob1
014400041028     c                   move      'Y'           flg_casob1
014500041028      * se anagrafica è via E-mail e tariffa uguale fax aggiorno altrimenti non faccio nulla
014600041028     c                   else
014700041028      *
014800041028     c                   if        sktcm(cm)='E' or sktcm(cm)='F'
014900041028     c                   except    sta_casoc1
015000041028     c                   add       1             tot_casoc1
015100041028     c                   move      'Y'           flg_casoc1
015200041028     c                   else
015300041028     c                   except    sta_div1
015400041028     c                   add       1             tot_diver1
015500041028     c                   move      'Y'           flg_diver1
015600041028     c                   endif
015700041028     c                   endif
015800041028     c                   endif
015900041028      * se diversi tipi comuniazione stampo che sono diversi e basta
016000041028     c                   if        cm > 1
016100041028     c                   except    sta_div1
016200041028     c                   add       1             tot_diver1
016300041028     c                   move      'Y'           flg_diver1
016400041028     c                   endif
016500041028      * tipo invio comunicazione fine giacenza
016600041028      * tariffa è a blank
016700041028     c                   If        fg = 0
016800041028     c                   except    sta_pul2
016900041028     c                   add       1             tot_pulit2
017000041028     c                   move      'Y'           flg_pulit2
017100041028     c                   endif
017200041028      *
017300041028      * tariffa non è a blanks ma  è uguale all'anagrafica
017400041028     c                   if        fg = 1 and vidtfg = sktfg(fg)
017500041028     c                   except    sta_casoa2
017600041028     c                   add       1             tot_casoa2
017700041028     c                   move      'Y'           flg_casoa2
017800041028     c                   endif
017900041028      *                            è diverso da angrafica
018000041028     c                   if        fg = 1 and vidtfg <> sktfg(fg)
018100041028      * se anagrafica non è invio mail devo aggiornare ANA con i valori della tariffa
018200041028     c                   if        vidtfg <> 'M' and vidtfg <> 'V'
018300041028     c                   except    sta_casob2
018400041028     c                   add       1             tot_casob2
018500041028     c                   move      'Y'           flg_casob2
018600041028      * se anagrafica è via E-mail e tariffa uguale fax aggiorno altrimenti non faccio nulla
018700041028     c                   else
018800041028      *
018900041028     c                   if        sktfg(fg)='X' or sktfg(fg)='F'
019000041028     c                   except    sta_casoc2
019100041028     c                   add       1             tot_casoc2
019200041028     c                   move      'Y'           flg_casoc2
019300041028     c                   else
019400041028     c                   except    sta_div2
019500041028     c                   add       1             tot_diver2
019600041028     c                   move      'Y'           flg_diver2
019700041028     c                   endif
019800041028     c                   endif
019900041028     c                   endif
020000041028      * se diversi tipi comuniazione stampo che sono diversi e basta
020100041028     c                   if        fg > 1
020200041028     c                   except    sta_div2
020300041028     c                   add       1             tot_diver2
020400041028     c                   move      'Y'           flg_diver2
020500041028     c                   endif
020600041027
020700041028      *              elaboro
020800041028
020900041028     c
021000041027     c                   endsr
021100041027      *---------------------------------------------------------------------------------------------
021200041027     OPRTF198   E            testa            01
021300041027     O                                        +   1 'Casi differenziati in base'
021400041028     O                                              ' ai flag di giacenza'
021500041028     O          E            Sta_pul1    1
021600041027     O                                        +   1 'Cliente'
021700041027     O                       clsksc           +   1
021800041028     O                                        +  10 'Ha TCM in tariffa pulite  '
021900041028     O          E            Sta_casoc1  1
022000041027     O                                        +   1 'Cliente'
022100041027     O                       clsksc           +   1
022200041028     O                                        +  10 'Caso_C TCM Tar=fax TCM Ana'
022300041028     o                                        +   1 '=Mail'
022400041028     O          E            Sta_casob1  1
022500041027     O                                        +   1 'Cliente'
022600041027     O                       clsksc           +   1
022700041028     O                                        +  10 'Caso_B TCM Tar <> TCM Ana '
022800041028     O          E            Sta_casoa1  1
022900041027     O                                        +   1 'Cliente'
023000041027     O                       clsksc           +   1
023100041028     O                                        +  10 'Caso_A TCM Tar. = TCM Ana '
023200041028     O          E            Sta_div1    1
023300041028     O                                        +   1 'Cliente'
023400041028     O                       clsksc           +   1
023500041028     O                                        +  10 'Ha TCM tutti diversi :'
023600041028     O                       cm            z  +   1
023700041028     O          E            Sta_pul2    0
023800041028     O                                        +  70 'Ha TFG in tariffa pulite  '
023900041028     O          E            Sta_casoc2  0
024000041028     O                                        +  70 'Caso_C TFG Tar=fax TFG Ana'
024100041028     o                                        +   1 '=Mail'
024200041028     O          E            Sta_casob2  0
024300041028     O                                        +  70 'Caso_B TFG Tar <> TFG Ana '
024400041028     O          E            Sta_casoa2  0
024500041028     O                                        +  70 'Caso_A TFG Tar. = TFG Ana '
024600041028     O          E            Sta_div2    0
024700041028     O                                        +  70 'Ha TFG tutti diversi :'
024800041028     O                       FG            z  +   1
024900041028     O          e            totali      2
025000041028     O                                        +   1 'T O T A L I'
025100041028     O                                        +   1 'TCM'
025200041028     O                                        +   1 'PULITI'
025300041028     O                       tot_pulit1    z  +   0
025400041028     O                                        +   2 'DIVERSI'
025500041028     O                       tot_diver1    z  +   0
025600041028     O                                        +   2 'ANA = TAR'
025700041028     O                       tot_casoa1    z  +   0
025800041028     O                                        +   2 'ANA <> TAR vince TAR'
025900041028     O                       tot_casob1    z  +   0
026000041028     O                                        +   2 'ANA <> TAR vince ANA'
026100041028     O                       tot_casoc1    z  +   0
026200041028     O          e            totali      2
026300041028     O                                        +   1 'T O T A L I'
026400041028     O                                        +   1 'TFG'
026500041028     O                                        +   1 'PULITI'
026600041028     O                       tot_pulit2    z  +   0
026700041028     O                                        +   2 'DIVERSI'
026800041028     O                       tot_diver2    z  +   0
026900041028     O                                        +   2 'ANA = TAR'
027000041028     O                       tot_casoa2    z  +   0
027100041028     O                                        +   2 'ANA <> TAR vince TAR'
027200041028     O                       tot_casob2    z  +   0
027300041028     O                                        +   2 'ANA <> TAR vince ANA'
027400041028     O                       tot_casoc2    z  +   0
