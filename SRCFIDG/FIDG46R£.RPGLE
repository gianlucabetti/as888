000100070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200080328     h dftactgrp(*no) actgrp(*caller)
000300070209
000400070209      *------------------------------------------------------------------------*
000500080725      *               Record CET file fipct00f
000600070209      *
000700070209      *------------------------------------------------------------------------*
000800080730     ffipct01l  if   e           k disk    usropn
000900080730     ffipct02l  if   e           k disk    usropn rename(fipct000:fipct002)
001000080805     ffiapd01l  if   e           k disk
001100101130     ffnarb01l  if   e           k disk
001200080805     ffipci01l  if   e           k disk
001300080725     ftabel00f  if   e           k disk
001400080725     ffidg46d   cf   e             workstn
001500070209
001600070216      * ? V A R I A B I L I ?
001700070531     d dataeur         s               d   datfmt(*eur)
001800080328     d dataiso         s               d
001900080725     d kpjba         e ds
002000080725     d ds2a          e ds
002100080725     d ds1p          e ds
002200080725     d fipctcetds    e ds
002300080725     d fidg46ds      e ds
002400080730     D tibs34ds      E DS                  inz
002500080730     D ddatiute      e ds
002600080730     D azuteds       e ds                  extname(AZUTE00F)
002700070209      *------------------------------------------------------------------------*
002800080327     c     *entry        plist
002900080328     c                   parm                    kpjba
003000080725     c                   movel     kpjbu         fidg46ds
003100080730     c                   eval      VTCPGM = 'FIDG46R'
003200110201      *
003300110201      * per condizionare campi da visualizzare oppure no
003400110201     c                   move      'N'           solo_se_EDP       1
003500110201     c                   if        %subst(Knmus:1:3) = 'EDP'
003600110201     c                   move      'S'           solo_se_EDP
003700110201     c                   end
003800110201      *
003900080730     C* Decodifica Elaboratore
004000080730     c     *dtaara       define    §azute        azuteds
004100080730     c     *dtaara       define    §datiute      ddatiute
004200080730     C                   in(E)     *dtaara
004300080730     C                   IF        %Error  or  RSUT = *blanks
004400080730     C                   call      'TIBS34R'
004500080730     C                   parm                    Tibs34Ds
004600080730     C                   in        *dtaara
004700080730     c                   ENDIF
004800080725     c     ktab          klist
004900080725     c                   kfld                    TBLKUT
005000080725     c                   kfld                    TBLCOD
005100080725     c                   kfld                    TBLKEY
005200080725     c                   z-add     1             tblkut
005300080730     c     kpct1         klist
005400080730     c                   kfld                    dg46aasi
005500080730     c                   kfld                    dg46lnpi
005600080730     c                   kfld                    dg46nrsi
005700080730     c                   kfld                    dg46nspi
005800080730     c                   kfld                    pcttrd
005900080729     c     kpct2         klist
006000080729     c                   kfld                    dg46fgsi
006100080729     c                   kfld                    dg46ndci
006200080805     c                   kfld                    dg46codaut
006300080729     c                   kfld                    pcttrd
006400080725     c                   kfld                    dg46aasi
006500080725     c                   kfld                    dg46lnpi
006600080725     c                   kfld                    dg46nrsi
006700080725     c                   kfld                    dg46nspi
006800080729     c                   eval      pcttrd ='CET'
006900080725     c     kspe1         klist
007000080725     c                   kfld                    dg46aasi
007100080725     c                   kfld                    dg46lnpi
007200080725     c                   kfld                    dg46nrsi
007300080725     c                   kfld                    dg46nspi
007400080805     c     kapd          klist
007500080805     c                   kfld                    apdtip
007600080805     c                   kfld                    v3ccodaut
007700080805     c                   eval      apdtip = 'A'
007800101130     c*
007900101130     c     karb1         klist
008000101130     c                   kfld                    pctaas
008100101130     c                   kfld                    pctlnp
008200101130     c                   kfld                    pctnrs
008300101130     c                   kfld                    pctnsp
008400080725     c*
008500080730     c                   if        dg46ndci<> 0
008600080730     c                   open      fipct02l
008700080729     c     kpct2         chain     fipct02l
008800080729     c                   if        not %found(fipct02l)
008900080725     c                   eval      dg46erro = 'E'
009000080730     c                   exsr      srfine
009100080725     c                   end
009200080730     c                   else
009300080730     c                   open      fipct01l
009400080805     c     kpct1         setgt     fipct01l
009500080805     c     kpct1         readpe    fipct01l
009600080805     c                   if        not %eof(fipct01l)
009700080730     c                   eval      dg46erro = 'E'
009800080730     c                   exsr      srfine
009900080730     c                   end
010000080730     c                   end
010100080725     c                   eval      fipctcetds= pctdati
010200080327     c                   exsr      sr_pulvid
010300080805      * distinta
010400080725     c                   move      pctfgs        v3fgs
010500080725     c                   move      pctndc        v3ndc
010600080805      * autotrasp.
010700080805     c                   eval      v3ccodaut = pctcodaut
010800080805     c     kapd          chain     fiapd01l
010900080805     c                   if        %found(fiapd01l)
011000080805     c                   eval      v3dcodaut = apdrsc
011100080805     c                   else
011200080805     c                   eval      v3dcodaut = *all'?'
011300080805     c                   end
011400070514      * data/ora evento
011500080725     c                   movel     PCTDTORIN     v3data
011600080328     c                   move      v3data        dataiso
011700080328     c                   move      dataiso       dataeur
011800080328     c                   move      dataeur       v3data
011900080725     c                   move      pctdtorin     v3ora
012000080327      * spedizione
012100080725     c                   move      pctaas        v3aas
012200080725     c                   move      pctlnp        v3lnp
012300080725     c                   move      pctnrs        v3nrs
012400080725     c                   move      pctnsp        v3nsp
012500101130     c                   clear                   v3rgsd
012600101130     c     karb1         chain     fnarb01l
012700101130     c                   if        %Found(fnarb01l)
012800101130     c                   eval      v3rgsd = arbrsd
012900101130     c                   end
013000080327      * codice mancata consegna
013100080725     c                   movel     §pctcmc       v3cmc
013200101129     c                   clear                   v3dcodd
013300101129     c                   eval       v3ddata = 'Consegna.....'
013400080725     c                   if        §pctcmc <> ' '
013500080725     c                   movel(p)  §pctcmc       tblkey
013600080725     c                   eval      tblcod='2A'
013700080725     c     ktab          chain     tabel00f
013800080725     c                   if        %found(tabel00f)
013900080725     c                   movel     tbluni        ds2a
014000080725     c                   movel     §2ades        v3dcmc
014100101129     c                   eval       v3dcodd = 'Cod.Manc.Cons....'
014200101129     c                   eval       v3ddata = 'Manc.Consegna....'
014300080725     c                   end
014400080725     c                   end
014500101111      * codice mancata consegna precedente
014600101111     c                   if        §pctcmcp <> ' '
014700101111     c                   seton                                        44
014800101111     c                   movel     §pctcmcp      v3cmcp
014900101111     c                   movel(p)  §pctcmcp      tblkey
015000101111     c                   eval      tblcod='2A'
015100101111     c     ktab          chain     tabel00f
015200101111     c                   if        %found(tabel00f)
015300101111     c                   movel     tbluni        ds2a
015400101111     c                   movel     §2ades        v3dcmcp
015500101111     c                   end
015600101111     c                   end
015700080327      * data/ora mancata consegna
015800080725     c                   movel     §pctDATA      v3datacon
015900080328     c                   move      v3datacon     dataiso
016000080328     c                   move      dataiso       dataeur
016100080328     c                   move      dataeur       v3datacon
016200080725     c                   move      §pctORA       v3oracon
016300080327      * data/ora consegna richiesta
016400080725     c                   if        §pctdcr <> 0
016500080725     c                   movel     §pctdcr       v3dcr
016600080328     c                   move      v3dcr         dataiso
016700080328     c                   move      dataiso       dataeur
016800080328     c                   move      dataeur       v3dcr
016900080725     c                   move      §pcthcr       v3hcr
017000080725     c                   end
017100080327     c* note
017200080725     c                   movel     §pctNOTAUT    v3notaut
017300080327     c* incassi
017400080728     c                   clear                   v3inccas
017500080728     c                   select
017600080728     c                   when      §pctINCCAS = 'A'
017700080728     c                   eval      v3inccas = 'ASSEGNO'
017800080728     c                   when      §pctINCCAS = 'C'
017900080728     c                   eval      v3inccas = 'CONTANTI'
018000080728     c                   when      §pctINCCAS = 'N'
018100080728     c                   eval      v3inccas = 'NO INCASSO'
018200080728     c                   endsl
018300080728     c                   clear                   v3incfat
018400080728     c                   select
018500080728     c                   when      §pctINCfat = 'A'
018600080728     c                   eval      v3incfat = 'ASSEGNO'
018700080728     c                   when      §pctINCfat = 'C'
018800080728     c                   eval      v3incfat = 'CONTANTI'
018900080728     c                   when      §pctINCfat = 'N'
019000080728     c                   eval      v3incfat = 'NO INCASSO'
019100080728     c                   endsl
019200080327     c* cod. fiscale e part. iva
019300080725     c     kspe1         chain     fipci01l
019400080725     c                   if        %found(fipci01l)
019500080725     c                   movel     pcicpi        v3codpiv
019600080725     c                   movel     pciiso        v3codiso
019700080725     c                   movel     pcicdf        v3codfis
019800080725     c                   end
019900080327     c* firma
020000080725     c                   movel     §pctNOMFIR    v3nomfir
020100080327     c* riserva
020200080725     c                   movel     §pctRISERV    v3riserv
020300080725     c                   if        §pctRISERV <> ' '
020400080725     c                   movel(p)  §pctRISERV    tblkey
020500080725     c                   eval      tblcod='2A'
020600080725     c     ktab          chain     tabel00f
020700080725     c                   if        %found(tabel00f)
020800080725     c                   movel     tbluni        ds2a
020900080725     c                   movel     §2ades        v3desris
021000080725     c                   end
021100080725     c                   end
021200080327     c* consegna particolare
021300080725     c                   movel     §pctTC1       v3tc1
021400080725     c                   if        §pcttc1 <> ' '
021500080725     c                   movel(p)  §pcttc1       tblkey
021600080725     c                   eval      tblcod='1P'
021700080725     c     ktab          chain     tabel00f
021800080725     c                   if        %found(tabel00f)
021900080725     c                   movel     tbluni        ds1p
022000080725     c                   movel     §1pdes        v3dtc1
022100080725     c                   end
022200080725     c                   end
022300080327     c* bancali resi
022400080725     c                   move      §pctNRBANR    v3nrbanr
022500080725     c                   movel     §pctFLGSPC    v3flgspc
022600101102     c                   movel     §pctLDVfir    v3flgfir
022700080725     c* emetto video
022800080725     c                   do        *hival
022900110201     c                   setoff                                       23
023000110201     c                   if        solo_se_EDP ='S'
023100110201     c                   seton                                        23
023200110201     c                   end
023300110201      *
023400080725     c                   write     dg46t01
023500080725     c                   exfmt     dg46d01
023600070508
023700070508      * f3=fine
023800070508     c                   if        *inkc
023900080725     c                   eval      dg46erro='1'
024000080730     c                   exsr      srfine
024100070508     c                   endif
024200070508
024300070508      * f12=ritorno
024400070508     c                   if        *inkl
024500080725     c                   eval      dg46erro='2'
024600080730     c                   exsr      srfine
024700070508     c                   endif
024800080328      * f7=sfl dettaglio segnacolli
024900080328     c                   if        *inkg
025000080725     c                   movel(p)  fidg46ds      kpjbu
025100080728     c                   call      'FIDG48R'
025200080328     c                   parm                    kpjba
025300080725     c                   movel     kpjbu         fidg46ds
025400080328      * f3=fine
025500080730     c                   if        dg46erro =  '1'
025600080730     c                   exsr      srfine
025700080328     c                   endif
025800080328
025900080328     c                   endif
026000080725     c                   enddo
026100080327      *------------------------------------------------------------------------*
026200080730      *? fine
026300080327      *------------------------------------------------------------------------*
026400080730     c     srfine        begsr
026500080730     c                   movel(p)  fidg46ds      kpjbu
026600080730     c                   seton                                        lr
026700080730     c                   return
026800080730     c                   endsr
026900080730      *------------------------------------------------------------------------*
027000080730      *? PULIZIA DELLA VIDEATA  ?
027100080730      *------------------------------------------------------------------------*
027200080730     c     sr_pulvid     begsr
027300080327
027400080327     c                   clear                   v3fgs
027500080327     c                   clear                   v3ndc
027600080327     c                   clear                   v3data
027700080327     c                   clear                   v3ora
027800080327
027900080327     c                   clear                   v3aas
028000080327     c                   clear                   v3lnp
028100080327     c                   clear                   v3nrs
028200080327     c                   clear                   v3nsp
028300080327     c                   clear                   v3data
028400080327     c                   clear                   v3datacon
028500080327     c                   clear                   v3oracon
028600080327     c                   clear                   v3dcr
028700080327     c                   clear                   v3hcr
028800080327     c                   clear                   v3notaut
028900080327     c                   clear                   v3inccas
029000080327     c                   clear                   v3incfat
029100080327     c                   clear                   v3codpiv
029200080327     c                   clear                   v3codiso
029300080327     c                   clear                   v3codfis
029400080327     c                   clear                   v3nomfir
029500080327     c                   clear                   v3riserv
029600080725     c                   clear                   v3desris
029700080327     c                   clear                   v3nrbanr
029800080327     c                   clear                   v3tc1
029900080725     c                   clear                   v3dtc1
030000080725     c                   clear                   v3flgspc
030100101027     c                   clear                   v3flgfir
030200080725     c                   clear                   v3dcmc
030300080805     c                   clear                   v3ccodaut
030400080805     c                   clear                   v3dcodaut
030500101111     c                   clear                   v3dcmcp
030600101111     c                   clear                   v3cmcp
030700101111     c                   setoff                                       44
030800080327
030900080327     c                   endsr
031000080327
