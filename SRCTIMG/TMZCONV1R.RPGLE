000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400011206     Ftmclp00f  uf   e             disk
000500011206     Ftmfop00f  uf   e             disk
000600011206     Ftmlmp00f  uf   e             disk
000700011206     Ftmmmp00f  uf   e             disk
000800011206     Ftmsmp00f  uf   e             disk
000900011206     Ftmmoc00f  uf   e             disk
001000011206     Ftmsoc00f  uf   e             disk
001100011206     Ftmmof00f  uf   e             disk
001200011206     Ftmsof00f  uf   e             disk
001300011206
001400011206     D YEURCODS      E DS
001500011206     Dwimpor           s             14  4
001600011206
001700000000     C*---------------------------------------------------------------*
001800011206      *conversioni
001900990113     c                   exsr      tmclp
002000990114     c                   exsr      tmfop
002100990113     c                   exsr      tmlmp
002200990113     c                   exsr      tmmmp
002300990114     c                   exsr      tmsmp
002400990114     c                   exsr      tmmoc
002500990114     c                   exsr      tmsoc
002600990114     c                   exsr      tmmof
002700990114     c                   exsr      tmsof
002800990113     c                   seton                                        lr
002900990113     c*-------------------------------------------------
003000990113     c     tmclp         begsr
003100990113     c*-------------------------------------------------
003200990113     c                   setoff                                           99
003300011206     c     *start        setll     tmclp00f
003400981029     c                   do        *hival
003500011206     c                   read      tmclp00f                               99
003600981029     c   99              leave
003700011206
003800011206     c                   if        clpval = 'ITL ' or clpval = *blank
003900011206     c                   movel     'EUR '        clpval
004000011206     c                   else
004100011206     c                   iter
004200011206     c                   endif
004300011206
004400011206     c                   update    tmclp000
004500981029     c                   enddo
004600990113     c                   endsr
004700990113     c*-------------------------------------------------
004800990114     c     tmfop         begsr
004900990113     c*-------------------------------------------------
005000990113     c                   setoff                                           99
005100011206     c     *start        setll     tmfop00f
005200990113     c                   do        *hival
005300011206     c                   read      tmfop00f                               99
005400990113     c   99              leave
005500011206
005600011206     c                   if        fopval = 'ITL ' or fopval = *blank
005700011206     c                   movel     'EUR '        fopval
005800011206     c                   else
005900011206     c                   iter
006000011206     c                   endif
006100011206
006200011206     c                   update    tmfop000
006300990113     c                   enddo
006400990113     c                   endsr
006500990113     c*-------------------------------------------------
006600990113     c     tmlmp         begsr
006700990113     c*-------------------------------------------------
006800990113     c                   setoff                                           99
006900011206     c     *start        setll     tmlmp00f
007000990113     c                   do        *hival
007100011206     c                   read      tmlmp00f                               99
007200990113     c   99              leave
007300011206     c                   if        lmpval = 'ITL ' or lmpval = *blank
007400011206
007500011206     c                   if        lmpvp1 > 0
007600011206     c                   z-add     lmpvp1        wimpor
007700011206     c                   exsr      cambio
007800011206     c                   z-add     wimpor        lmpvp1
007900011206     c                   endif
008000011206     c
008100011206     c                   if        lmpvp2 > 0
008200011206     c                   z-add     lmpvp2        wimpor
008300011206     c                   exsr      cambio
008400011206     c                   z-add     wimpor        lmpvp2
008500011206     c                   endif
008600011206     c
008700011206     c                   if        lmpvp3 > 0
008800011206     c                   z-add     lmpvp3        wimpor
008900011206     c                   exsr      cambio
009000011206     c                   z-add     wimpor        lmpvp3
009100011206     c                   endif
009200011206     c
009300011206     c                   if        lmpvp4 > 0
009400011206     c                   z-add     lmpvp4        wimpor
009500011206     c                   exsr      cambio
009600011206     c                   z-add     wimpor        lmpvp4
009700011206     c                   endif
009800011206     c
009900011206     c                   if        lmpvp5 > 0
010000011206     c                   z-add     lmpvp5        wimpor
010100011206     c                   exsr      cambio
010200011206     c                   z-add     wimpor        lmpvp5
010300011206     c                   endif
010400011206     c
010500011206     c                   movel     'EUR '        lmpval
010600011206
010700011206     c                   else
010800011206     c                   iter
010900011206     c                   endif
011000011206
011100011206     c                   update    tmlmp000
011200990113     c                   enddo
011300990113     c                   endsr
011400990113     c*-------------------------------------------------
011500990113     c     tmmmp         begsr
011600990113     c*-------------------------------------------------
011700990113     c                   setoff                                           99
011800011206     c     *start        setll     tmmmp00f
011900990113     c                   do        *hival
012000011206     c                   read      tmmmp00f                               99
012100990113     c   99              leave
012200011206
012300011206     c                   if        mmpval = 'ITL ' or mmpval = *blank
012400011206
012500011206     c                   if        mmpprz > 0
012600011206     c                   z-add     mmpprz        wimpor
012700011206     c                   exsr      cambio
012800011206     c                   z-add     wimpor        mmpprz
012900011206     c                   z-add     1             mmpcmb
013000011206     c                   endif
013100011206     c                   movel     'EUR '        mmpval
013200011206
013300011206     c                   else
013400011206     c                   iter
013500011206     c                   endif
013600011206
013700011206     c                   update    tmmmp000
013800990113     c                   enddo
013900990113     c                   endsr
014000990114     c*-------------------------------------------------
014100990114     c     tmsmp         begsr
014200990114     c*-------------------------------------------------
014300990114     c                   setoff                                           99
014400011206     c     *start        setll     tmsmp00f
014500990114     c                   do        *hival
014600011206     c                   read      tmsmp00f                               99
014700990114     c   99              leave
014800011206     c                   if        smpval = 'ITL ' or smpval = *blank
014900011206
015000011206     c                   if        smpplf > 0
015100011206     c                   z-add     smpplf        wimpor
015200011206     c                   exsr      cambio
015300011206     c                   z-add     wimpor        smpplf
015400011206     c                   endif
015500011206     c
015600011206     c                   if        smpplv > 0
015700011206     c                   z-add     smpplv        wimpor
015800011206     c                   exsr      cambio
015900011206     c                   z-add     wimpor        smpplv
016000011206     c                   endif
016100011206     c
016200011206     c                   if        smppmc > 0
016300011206     c                   z-add     smppmc        wimpor
016400011206     c                   exsr      cambio
016500011206     c                   z-add     wimpor        smppmc
016600011206     c                   endif
016700011206     c
016800011206     c                   z-add     1             smpcmb
016900011206     c                   movel     'EUR '        smpval
017000011206
017100011206     c                   else
017200011206     c                   iter
017300011206     c                   endif
017400011206
017500011206     c                   update    tmsmp000
017600990114     c                   enddo
017700990114     c                   endsr
017800990114     c*-------------------------------------------------
017900990114     c     tmmoc         begsr
018000990114     c*-------------------------------------------------
018100990114     c                   setoff                                           99
018200011206     c     *start        setll     tmmoc00f
018300990114     c                   do        *hival
018400011206     c                   read      tmmoc00f                               99
018500990114     c   99              leave
018600011206
018700011206     c                   if        mocval = 'ITL ' or mocval = *blank
018800011206
018900011206     c                   if        mocprz > 0
019000011206     c                   z-add     mocprz        wimpor
019100011206     c                   exsr      cambio
019200011206     c                   z-add     wimpor        mocprz
019300011206     c                   z-add     1             moccmb
019400011206     c                   endif
019500011206     c                   movel     'EUR '        mocval
019600011206
019700011206     c                   else
019800011206     c                   iter
019900011206     c                   endif
020000011206
020100011206     c                   update    tmmoc000
020200990114     c                   enddo
020300990114     c                   endsr
020400990114     c*-------------------------------------------------
020500990114     c     tmsoc         begsr
020600990114     c*-------------------------------------------------
020700990114     c                   setoff                                           99
020800011206     c     *start        setll     tmsoc00f
020900990114     c                   do        *hival
021000011206     c                   read      tmsoc00f                               99
021100990114     c   99              leave
021200011206
021300011206     c                   if        mocval = 'ITL ' or mocval = *blank
021400011206
021500011206     c                   if        mocprz > 0
021600011206     c                   z-add     mocprz        wimpor
021700011206     c                   exsr      cambio
021800011206     c                   z-add     wimpor        mocprz
021900011206     c                   z-add     1             moccmb
022000011206     c                   endif
022100011206     c                   movel     'EUR '        mocval
022200011206
022300011206     c                   else
022400011206     c                   iter
022500011206     c                   endif
022600011206
022700011206     c                   update    tmsoc000
022800990114     c                   enddo
022900990114     c                   endsr
023000990114     c*-------------------------------------------------
023100990114     c     tmmof         begsr
023200990114     c*-------------------------------------------------
023300990114     c                   setoff                                           99
023400011206     c     *start        setll     tmmof00f
023500990114     c                   do        *hival
023600011206     c                   read      tmmof00f                               99
023700990114     c   99              leave
023800011206
023900011206     c                   if        mofval = 'ITL ' or mofval = *blank
024000011206
024100011206     c                   if        mofprz > 0
024200011206     c                   z-add     mofprz        wimpor
024300011206     c                   exsr      cambio
024400011206     c                   z-add     wimpor        mofprz
024500011206     c                   z-add     1             mofcmb
024600011206     c                   endif
024700011206     c                   movel     'EUR '        mofval
024800011206
024900011206     c                   else
025000011206     c                   iter
025100011206     c                   endif
025200011206
025300011206     c                   update    tmmof000
025400990114     c                   enddo
025500990114     c                   endsr
025600990114     c*-------------------------------------------------
025700990114     c     tmsof         begsr
025800990114     c*-------------------------------------------------
025900990114     c                   setoff                                           99
026000011206     c     *start        setll     tmsof00f
026100990114     c                   do        *hival
026200011206     c                   read      tmsof00f                               99
026300990114     c   99              leave
026400011206
026500011206     c                   if        mofval = 'ITL ' or mofval = *blank
026600011206
026700011206     c                   if        mofprz > 0
026800011206     c                   z-add     mofprz        wimpor
026900011206     c                   exsr      cambio
027000011206     c                   z-add     wimpor        mofprz
027100011206     c                   z-add     1             mofcmb
027200011206     c                   endif
027300011206     c                   movel     'EUR '        mofval
027400011206
027500011206     c                   else
027600011206     c                   iter
027700011206     c                   endif
027800011206
027900011206     c                   update    tmsof000
028000990114     c                   enddo
028100990114     c                   endsr
028200981029      *
028300011206     c*-------------------------------------------------
028400011206     c     cambio        begsr
028500011206     c*-------------------------------------------------
028600011206      *
028700011206     C                   CLEAR                   YEURCODS
028800011206     C                   MOVEL     'ITL'         YECDVI
028900011206     C                   Z-ADD     wimpor        YECIMI
029000011206     C                   MOVEL     'EUR'         YECDVO
029100011206     C                   MOVEL     'H'           YECTAR
029200011206     C                   MOVEL     '3'           YECdco
029300011206     C                   CALL      'YEURCO'
029400011206     C                   PARM                    YEURCODS
029500011206     C                   Z-ADD     YECIMO        wimpor
029600011206      *
029700011206     c                   endsr
