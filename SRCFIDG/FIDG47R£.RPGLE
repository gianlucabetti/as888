000100070209     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200080328     h dftactgrp(*no) actgrp(*caller)
000300070209
000400070209      *------------------------------------------------------------------------*
000500080728      *               Record CED file fipcd00f
000600070209      *
000700070209      *------------------------------------------------------------------------*
000800080729     ffipcd03l  if   e           k disk
000900080805     ffiapd01l  if   e           k disk
001000080725     ftabel00f  if   e           k disk
001100080728     ffidg47d   cf   e             workstn
001200070209
001300070216      * ? V A R I A B I L I ?
001400070531     d dataeur         s               d   datfmt(*eur)
001500080328     d dataiso         s               d
001600080725     d kpjba         e ds
001700080728     d ds3e          e ds
001800080728     d fipcdcedds    e ds
001900080728     d fidg47ds      e ds
002000080730     D tibs34ds      E DS                  inz
002100080730     D ddatiute      e ds
002200080730     D azuteds       e ds                  extname(AZUTE00F)
002300070209      *------------------------------------------------------------------------*
002400080327     c     *entry        plist
002500080328     c                   parm                    kpjba
002600080728     c                   movel     kpjbu         fidg47ds
002700080730     c                   eval      VTCPGM = 'FIDG47R'
002800080730     C* Decodifica Elaboratore
002900080730     c     *dtaara       define    §azute        azuteds
003000080730     c     *dtaara       define    §datiute      ddatiute
003100080730     C                   in(E)     *dtaara
003200080730     C                   IF        %Error  or  RSUT = *blanks
003300080730     C                   call      'TIBS34R'
003400080730     C                   parm                    Tibs34Ds
003500080730     C                   in        *dtaara
003600080730     c                   ENDIF
003700080805     c     kapd          klist
003800080805     c                   kfld                    apdtip
003900080805     c                   kfld                    v3ccodaut
004000080805     c                   eval      apdtip = 'A'
004100080725     c     ktab          klist
004200080725     c                   kfld                    TBLKUT
004300080725     c                   kfld                    TBLCOD
004400080725     c                   kfld                    TBLKEY
004500080725     c                   z-add     1             tblkut
004600080725     c     kspe          klist
004700080730     c                   kfld                    dg47fgsi
004800080730     c                   kfld                    dg47ndci
004900080805     c                   kfld                    dg47codaut
005000080730     c                   kfld                    pcdtrd
005100080728     c                   kfld                    dg47aasi
005200080728     c                   kfld                    dg47lnpi
005300080728     c                   kfld                    dg47nrsi
005400080728     c                   kfld                    dg47nspi
005500080728     c                   kfld                    dg47nsci
005600080728     c                   eval      pcdtrd ='CED'
005700080725     c*
005800080730     c     kspe          chain     fipcd03l
005900080730     c                   if        not %found(fipcd03l)
006000080728     c                   eval      dg47erro = 'E'
006100080725     c                   seton                                        lr
006200080725     c                   return
006300080725     c                   end
006400080728     c                   eval      fipcdcedds= pcddati
006500080327     c                   exsr      sr_pulvid
006600080805      * distinta
006700080728     c                   move      pcdfgs        v3fgs
006800080728     c                   move      pcdndc        v3ndc
006900080805     c* cod aut.
007000080805     c                   eval      v3ccodaut = pcdcodaut
007100080805     c     kapd          chain     fiapd01l
007200080805     c                   if        %found(fiapd01l)
007300080805     c                   eval      v3dcodaut = apdrsc
007400080805     c                   else
007500080805     c                   eval      v3dcodaut = *all'?'
007600080805     c                   end
007700070514      * data/ora evento
007800080728     c                   movel     PCdDTORIN     v3data
007900080328     c                   move      v3data        dataiso
008000080328     c                   move      dataiso       dataeur
008100080328     c                   move      dataeur       v3data
008200080728     c                   move      pcddtorin     v3ora
008300080728      * segnacollo
008400080728     c                   move      pcdfls        v3cfls
008500080728     c                   move      pcdlna        v3clna
008600080728     c                   move      pcdnrser      v3cnrser
008700080728     c                   move      pcdnsc        v3cnsc
008800080728      * spedizione
008900080728     c                   move      pcdaas        v3aas
009000080728     c                   move      pcdlnp        v3lnp
009100080728     c                   move      pcdnrs        v3nrs
009200080728     c                   move      pcdnsp        v3nsp
009300080728     c* spunte
009400080730     c                   select
009500080730     c                   when      §pcdspucon ='S'
009600080728     c                   eval      v3cspucon ='SI'
009700080730     c                   when      §pcdspucon ='A'
009800080730     c                   eval      v3cspucon ='Annullato'
009900080730     c                   other
010000080728     c                   eval      v3cspucon ='NO'
010100080728     c                   end
010200080728     c*
010300080728     c                   if        §pcdspuspc ='S'
010400080728     c                   eval      v3cspuspc ='SI'
010500080728     c                   else
010600080728     c                   eval      v3cspuspc ='NO'
010700080728     c                   end
010800080730     c                   if        §pcdflgspc ='S'
010900080730     c                   eval      v3cflgspc ='SI'
011000080730     c                   else
011100080730     c                   eval      v3cflgspc ='NO'
011200080730     c                   end
011300080728     c* anomalia
011400080728     c                   movel     §pcdcancon    v3ccancon
011500080728     c                   if        V3CCANCON <> ' '
011600080728     c                   movel(p)  V3CCANCON     tblkey
011700080728     c                   eval      tblcod='3E'
011800080725     c     ktab          chain     tabel00f
011900080725     c                   if        %found(tabel00f)
012000080728     c                   movel     tbluni        ds3E
012100120806     c                   movel     §3ede12       v3dcancon
012200080725     c                   end
012300080725     c                   end
012400080725     c* emetto video
012500080725     c                   do        *hival
012600080725     c                   write     dg46t01
012700080725     c                   exfmt     dg46d01
012800070508
012900070508      * f3=fine
013000070508     c                   if        *inkc
013100080728     c                   eval      dg47erro='1'
013200080730     c                   exsr      srfine
013300070508     c                   endif
013400070508
013500070508      * f12=ritorno
013600070508     c                   if        *inkl
013700080728     c                   eval      dg47erro='2'
013800080730     c                   exsr      srfine
013900070508     c                   endif
014000080328      * f3=fine
014100080728     c                   if        dg47erro <> ' '
014200080730     c                   exsr      srfine
014300080328     c                   endif
014400080328
014500080725     c                   enddo
014600080730      *------------------------------------------------------------------------*
014700080730      *? fine
014800080730      *------------------------------------------------------------------------*
014900080730     c     srfine        begsr
015000080730     c                   movel(p)  fidg47ds      kpjbu
015100080730     c                   seton                                        lr
015200080730     c                   return
015300080730     c                   endsr
015400080327      *------------------------------------------------------------------------*
015500080327      *? PULIZIA DELLA VIDEATA  ?
015600080327      *------------------------------------------------------------------------*
015700080327     c     sr_pulvid     begsr
015800080327
015900080327     c                   clear                   v3fgs
016000080327     c                   clear                   v3ndc
016100080327     c                   clear                   v3data
016200080327     c                   clear                   v3ora
016300080327
016400080327     c                   clear                   v3aas
016500080327     c                   clear                   v3lnp
016600080327     c                   clear                   v3nrs
016700080327     c                   clear                   v3nsp
016800080728     c                   clear                   v3cfls
016900080728     c                   clear                   v3clna
017000080728     c                   clear                   v3cnrser
017100080728     c                   clear                   v3cnsc
017200080730     c                   clear                   v3cflgspc
017300080728     c                   clear                   v3ccancon
017400080728     c                   clear                   v3dcancon
017500080728     c                   clear                   v3cspuspc
017600080728     c                   clear                   v3cspucon
017700080327
017800080327     c                   endsr
017900080327
