000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300161110      * Il pgm estrae i record di log degli esiti palmare e gli orari
000400060125      *
000500060125      *****************************************************************
000600161110     Fbitdi00f  IF   E             DISK
000700161110     ffiplg02l  if   e           k disk
000800161110     ffiplg01l  if   e           k disk    rename(fiplg000:fiplg1) prefix(x)
000900161110     ftabel00f  if   e           k disk
001000161110     ftntbe01l  if   e           k disk
001100161110     Fbidep00f  o    E             DISK
001200030429     F*---------------------------------------------------------------------------------
001300000607     D kpjba         e ds
001400161110     D ds2a          e ds
001500161110     D dcmr          e ds
001600161110     D fiprdcetds    e ds
001700161110     D fiprdcords    e ds
001800161110     D fiprcresds    e ds
001900161110     D fiprcrords    e ds
002000060125     d dataoggi        s               d   datfmt(*eur)
002100060125     d data_gg1        s               d   datfmt(*iso)
002200161110     d tmp             s             17
002300020205     c**********************************************************************
002400030429      *
002500161110     c     *start        setll     bitdi00f
002600161026     c                   do        *hival
002700161110     c                   read      bitdi00f
002800161110     c                   if        %eof(bitdi00f)
002900161026     c                   leave
003000161026     c                   endif
003100161110      *reperimento esiti consegne
003200161110     c                   exsr      RekCet
003300161110      *reperimento esiti ritiri
003400161110     c                   exsr      RekRes
003500161110     c                   enddo
003600161110     c                   seton                                        lr
003700161110     C**-------------------------------------------------------------**
003800161110     c     RekCet        begsr
003900161110     C**-------------------------------------------------------------**
004000161110     c                   clear                   bidep000
004100161110     c                   move      'CET'         trek
004200161110     c     kplg          setll     fiplg02l
004300161110     c                   do        *hival
004400161110     c     kplg          reade     fiplg02l
004500161110     c                   if        %eof(fiplg02l)
004600161110     c                   leave
004700161110     c                   endif
004800170306     c                   if        plgerr <> *blank
004900170306     c                   iter
005000170306     c                   endif
005100161110     c                   movel     plgdati       fiprdcetds
005200161110      * reperisce orario
005300161110     c                   move      'COR'         trek1
005400161110     c                   move      'C'           tsrk1
005500161110     c                   move      §cetdtorin    dtorink
005600161110     c                   clear                   fiprdcords
005700161110     c                   exsr      RekCorRor
005800161110      * IMPOSTA DATI PER SCRITTURA
005900161110     c                   eval      DEPFGS    =   tdiFGS
006000161110     c                   eval      DEPDFV    =   tdiDFV
006100161110     c                   eval      DEPNFV    =   tdiNFV
006200161110     c                   eval      DEPPDR    =   tdiPDR
006300161110     c                   eval      DEPTSR    =   'C'
006400161110     c                   eval      DEPTSRD   =   'CONSEGNE'
006500161110     c                   eval      DEPCMC    =   §CETCMC
006600170119     c                   if        §cetflgspc <> *blank
006700170119     c                   eval      DEPspc    =   §CETflgspc
006800170119     c                   else
006900170119     c                   eval      DEPspc    =   'N'
007000170119     c                   endif
007100161110     c                   clear                   depCMCD
007200161110     c                   if        depcmc <> *blank
007300161110     c                   move      '2A'          tblcod
007400161110     c                   movel     depcmc        tblkey
007500161110     c     ktab2         chain     tabel00f
007600161110     c                   if        %found(tabel00f)
007700161110     c                   movel     tbluni        ds2a
007800161110     c                   movel     §2ades        depcmcd
007900161110     c                   endif
008000161110     c                   endif
008100161110     c                   eval      DEPAAS    =   §CETAAS
008200161110     c                   eval      DEPLNP    =   §CETLNP
008300161110     c                   eval      DEPNRS    =   §CETNRS
008400161110     c                   eval      DEPNSP    =   §CETNSP
008500170306     c                   MOVEL     §CETDTORIN    DEPDCM
008600170306     c                   MOVE      §CETDTORIN    HCOMO6            6 0
008700170306     c                   MOVEL     HCOMO6        HCOMO4            4 0
008800170306     c                   Z-ADD     HCOMO4        depHmc
008900170306     c                   Z-ADD     HCOMO4        DEPESIORA
009000170306     c                   eval      depfora = *blank
009100170306      *errore data forza data e ora ricezione evento
009200170306     c                   if        plgmsg = 'DATA < o > di 10 GG'
009300170306     c                   MOVEL     plgdatora     DEPDCM
009400170306     c                   MOVE      plgdatora     HCOMO6
009500170306     c                   MOVEL     HCOMO6        HCOMO4
009600170306     c                   Z-ADD     HCOMO4        depHmc
009700170306     c                   Z-ADD     HCOMO4        DEPESIORA
009800170306     c                   eval      depfora = 'F'
009900170306     c                   endif
010000170306      *errore ora forza orario ricezione evento
010100170306     c                   if        hcomo4 > 2200    or
010200170306     c                             hcomo4 < 0600
010300170306     c                   MOVE      plgdatora     HCOMO6
010400170306     c                   MOVEL     HCOMO6        HCOMO4
010500170306     c                   Z-ADD     HCOMO4        depHmc
010600170306     c                   Z-ADD     HCOMO4        DEPESIORA
010700170306     c                   eval      depfora = 'F'
010800170306     c                   endif
010900170306      *
011000161110     c                   eval      DEPHSTI   =   §CORORASTI
011100161110     c                   clear                   DEPXLO
011200161110     c                   if        §cetxlo <> *blank
011300161111     c                   MOVE      §cetxlo       COMO86
011400161111     c                   z-add     como86        DEPXLO
011500161110     c                   endif
011600161110     c                   clear                   DEPylo
011700161110     c                   if        §cetyla <> *blank
011800161111     c                   MOVE      §cetyla       COMO86
011900161111     c                   z-add     como86        DEPylo
012000161110     c                   endif
012100161110     c                   eval      tmp= %char(%timestamp: *iso0)
012200161110     c                   eval      DEPDATORA =   TMP
012300161110     c                   write     bidep000
012400161026     c                   enddo
012500161110     c                   endsr
012600161110     C**-------------------------------------------------------------**
012700161110     c     RekRes        begsr
012800161110     C**-------------------------------------------------------------**
012900161110     c                   clear                   bidep000
013000161110     c                   move      'RES'         trek
013100161110     c     kplg          setll     fiplg02l
013200161110     c                   do        *hival
013300161110     c     kplg          reade     fiplg02l
013400161110     c                   if        %eof(fiplg02l)
013500161110     c                   leave
013600161110     c                   endif
013700161110     c                   movel     plgdati       fiprcresds
013800161110      * reperisce orario
013900161118     c                   move      'ROR'         trek1
014000161110     c                   move      'R'           tsrk1
014100161110     c                   move      §resdtorin    dtorink
014200161111     c                   clear                   fiprcrords
014300161110     c                   exsr      RekCorRor
014400161110      * IMPOSTA DATI PER SCRITTURA
014500161110     c                   eval      DEPFGS    =   tdiFGS
014600161110     c                   eval      DEPDFV    =   tdiDFV
014700161110     c                   eval      DEPNFV    =   tdiNFV
014800161110     c                   eval      DEPPDR    =   tdiPDR
014900161110     c                   eval      DEPTSR    =   'R'
015000161110     c                   eval      DEPTSRD   =   'RITIRI'
015100161110     c                   eval      DEPCMC    =   §rescmr
015200170119     c                   if        §RESFLGSPC <> *blank
015300170119     c                   eval      DEPspc    =   §RESFLGSPC
015400170119     c                   else
015500170119     c                   eval      DEPspc    =   'N'
015600170119     c                   endif
015700161110     c                   clear                   depCMCD
015800161110     c                   if        depcmc <> *blank
015900161110     c                   move      'CMR'         tbecod
016000161110     c                   movel     depcmc        tbeke1
016100161110     c     ktbe          chain     tntbe01l
016200161110     c                   if        %found(tntbe01l)
016300161110     c                   movel     tbeuni        dcmr
016400161110     c                   movel     d§cmrdes      depcmcd
016500161110     c                   endif
016600161110     c                   endif
016700161110     c                   movel     §respoe       DEPPOE
016800161110     c                   movel     §resNSR       depnsr
016900161110     c                   movel     §resNOR       depNOR
017000161110     c                   movel     §resNRV       depNRV
017100161110     c                   MOVEL     §resDTORIN    DEPDCM
017200161111     c                   MOVE      §resDTORIN    HCOMO6
017300161111     c                   MOVEL     HCOMO6        HCOMO4
017400161110     c                   Z-ADD     HCOMO4        DEPHmc
017500161110     c                   Z-ADD     HCOMO4        DEPESIORA
017600170306      *errore data forza data e ora ricezione evento
017700170306     c                   if        plgmsg = 'DATA < o > di 10 GG'
017800170306     c                   MOVEL     plgdatora     DEPDCM
017900170306     c                   MOVE      plgdatora     HCOMO6
018000170306     c                   MOVEL     HCOMO6        HCOMO4
018100170306     c                   Z-ADD     HCOMO4        depHmc
018200170306     c                   Z-ADD     HCOMO4        DEPESIORA
018300170306     c                   eval      depfora = 'F'
018400170306     c                   endif
018500170306      *errore ora forza orario ricezione evento
018600170306     c                   if        hcomo4 > 2200    or
018700170306     c                             hcomo4 < 0600
018800170306     c                   MOVE      plgdatora     HCOMO6
018900170306     c                   MOVEL     HCOMO6        HCOMO4
019000170306     c                   Z-ADD     HCOMO4        depHmc
019100170306     c                   Z-ADD     HCOMO4        DEPESIORA
019200170306     c                   eval      depfora = 'F'
019300170306     c                   endif
019400161110     c                   move      §rororasti    DEPHSTI
019500161110     c                   clear                   DEPXLO
019600161110     c                   if        §resxlongi <> *blank
019700161111     c                   MOVE      §resxlongi    COMO86            8 6
019800161111     c                   z-add     como86        DEPXLO
019900161110     c                   endif
020000161110     c                   clear                   DEPylo
020100161110     c                   if        §resylati <> *blank
020200161111     c                   MOVE      §resylati     COMO86
020300161111     c                   z-add     como86        DEPylo
020400161110     c                   endif
020500161110     c                   eval      tmp= %char(%timestamp: *iso0)
020600161110     c                   eval      DEPDATORA =   TMP
020700161110     c                   write     bidep000
020800161110     c                   enddo
020900161110     c                   endsr
021000161110     C**-------------------------------------------------------------**
021100161110     c     RekCorRor     begsr
021200161110     C**-------------------------------------------------------------**
021300161110     c     kplg1         setgt     fiplg01l
021400161110     c     kplg1b        readpe    fiplg01l
021500161110     c                   if        not%eof(fiplg01l)
021600161110     c                   if        tsrk1 = 'C'
021700161110     c                   movel     xplgdati      fiprdCords
021800161110     c                   else
021900161110     c                   movel     xplgdati      fiprcRords
022000161110     c                   endif
022100161110     c                   endif
022200161110     c                   endsr
022300020205     C**-------------------------------------------------------------**
022400000607     c     *inzsr        begsr
022500060125     C**-------------------------------------------------------------**
022600000607     c     *entry        plist
022700020205     c                   parm                    kpjba
022800030429      *----------*
022900030429     C* GIRO DATA ODIERNA
023000030429     C                   TIME                    W0140            14 0
023100030429     C                   MOVE      W0140         UDATE8            8 0
023200160126     C                   MOVEl     W0140         oracor            6 0
023300060125     C     *eur          movel     UDATE8        dataoggi
023400160201     c                   move      dataoggi      data_gg1
023500160201     c                   move      data_gg1      oggi              8 0
023600060125     C     dataoggi      subdur    1:*d          data_gg1
023700160201     c                   move      data_gg1      ieri              8 0
023800161025      *
023900161110     c     kplg          klist
024000161110     c                   kFLD                    trek              3
024100161110     c                   kFLD                    tdifgs
024200161110     c                   kFLD                    tdinfv
024300161110     c                   kFLD                    tdipdr
024400161110      *
024500161110     c     kplg1         klist
024600161110     c                   kFLD                    trek1             3
024700161110     c                   kFLD                    tdifgs
024800161110     c                   kFLD                    tdinfv
024900161110     c                   kFLD                    tsrk1             1
025000161110     c                   kFLD                    PLGIDDOC
025100161110     c                   kFLD                    dtorink          14
025200161110      *
025300161110     c     kplg1b        klist
025400161110     c                   kFLD                    trek1
025500161110     c                   kFLD                    tdifgs
025600161110     c                   kFLD                    tdinfv
025700161110     c                   kFLD                    tsrk1
025800161110     c                   kFLD                    PLGIDDOC
025900161110     c*
026000161110     c     ktbe          klist
026100161110     c                   kfld                    tbecod
026200161110     c                   kfld                    tbeke1
026300020402     c*
026400161110     c     ktab2         klist
026500161110     c                   kfld                    tblkut
026600161110     c                   kfld                    tblcod
026700161110     c                   kfld                    tblkey
026800161110     c                   z-add     1             tblkut
026900020402     c                   endsr
