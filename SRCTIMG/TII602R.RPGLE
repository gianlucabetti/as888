000200110719     H DEBUG DECEDIT('0,') DATEDIT(*ymd/) Option(*nodebugio)
000300110809     ftmmof02l  uf   e           k disk
000301110809     ftmfop01l  uf   e           k disk
000406110809     fxx3ea1p   uf a e             disk
000407110809     fxx3ea2p   uf a e             disk
000500030110
000501110719     d kpjba         e ds
000502110719     D soc001        E DS                  EXTNAME(xsoc001ds)
000503110719     D xsocds          DS          1000
000504110719
000505110719     D trul33ds      E DS
000506110811     d oggiiso         s               d   datfmt(*iso)
001100030108     d dataiso         s               d   datfmt(*iso)
001200030108     d dataeur         s               d   datfmt(*eur)
001201110719     D WLBDAT          DS
001202110719     D  G06DAT                 1      6  0
001203110719     D  G06INV                 7     12  0
001204110719     D  G06ERR                13     13
001205110719     D  G06TGI                14     18  0
001300030414
001400030414     d Wdata8          DS
001500030414     d  dadata                 1      8  0
001600030414     d  adata                  9     16  0
001700030414     d  GioLav                17     21  0
001800030414      *
001801110809     D oflspedi        DS
001802110810     D  numsped                1      7  0
001900030109     d
002000000607      *--------------------------------------------------------------------
002100110811     c                   move      *date         oggiiso
002200110719      * lettura file ordini da trasmettere per prelievi a logistica
002300110809     c     *loval        setll     tmmof02l
002301110719     c                   do        *hival
002302110809     c                   read      tmmof02l
002303110809     c                   if        %eof(tmmof02l)
002304110719     c                   leave
002305110719     c                   endif
002306110817     c                   if        moffas <> 'OR'
002307110817     c                   iter
002308110817     c                   endif
002309110811     c                   if        mofdcn > 0
002310110811     c     *iso          move      mofdcn        dataiso
002311110811     c     dataiso       subdur    oggiiso       diff:*d           5 0
002312110811     c                   if        diff > 7
002313110811     c                   iter
002314110811     c                   endif
002315110811     c                   endif
002316110809     c     moffor        chain     tmfop01l
002317110719      *reperisce numero assoluto riga per legame gestionale con trasmissione a logistica
002318110719     c                   exsr      RepNumRig
002319110719      * reperisce numero progressivo per spedizioni logistica
002320110719      * primo ordine oppure rottura cliente
002321110809     c                   if        exforn <> moffor or
002322110809     c                             exncmd <> mofnor
002323110809     c                   move      moffor        exforn            8
002324110810     c                   z-add     mofnor        exncmd            5 0
002325110809     c                   eval      x3sit  = '04'
002326110809     c                   eval      x3dep  = 'MAGASEDE'
002327110810     c     rilok         tag
002328110810     C     *LOCK         IN        Oflspedi                             46
002329110810     C   46              GOTO      rilok
002330110810     c                   eval      numsped = numsped + 1
002331110810     c                   out       oflspedi
002332110809     c                   movel     numsped       x3next
002333110809     c                   move      'E'           x3tenp
002334110809     c                   move      'EM'          x3cmvt
002335110809     c                   move      mofdor        g06inv
002336110719     c                   move      '3'           g06err
002337110719     c                   call      'XSRDAT'
002338110719     c                   parm                    wlbdat
002339110809     c                   move      g06dat        x3xrec
002340110809     c                   move      g06dat        x3xbon
002341110810     c                   movel     mofnor        x3ndoc
002342110809     c                   move      moffor        x3cfrn
002343110809     c                   movel     foprsi        x3nfrn
002344110809     c                   write     xx3ea1
002345110809     c                   eval      x4sit   = x3sit
002346110809     c                   eval      x4dep   = x3dep
002347110809     c                   eval      x4next  = x3next
002348110809     c                   eval      x4art  = mofmat
002349110809     c                   eval      x4qapp = mofqta
002351110809     c                   movel     mofnpt        x4spec
002352110809     c                   write     xx3ea2
002361110809     c                   update    tmmof000
002362110719     c                   iter
002363110719     c                   else
002367110809     c                   eval      x4sit   = x3sit
002368110809     c                   eval      x4dep   = x3dep
002369110809     c                   eval      x4next  = x3next
002370110809     c                   eval      x4art  = mofmat
002371110809     c                   eval      x4qapp = mofqta
002372110809     c                   movel     mofnpt        x4spec
002373110809     c                   write     xx3ea2
002374110809     c                   update    tmmof000
002403110719     c                   endif
002404030415     c
002405110719     c                   enddo
002500110719      * scrivere ultima riga !!!!!!!
002800020517
002801110719     c     fine          tag
002900020521     c                   seton                                        lr
009700020521      *-------------------------------------------------
009800020521     c     *inzsr        begsr
009900020521      *-------------------------------------------------
010000020626     c     *entry        plist
010100110719     c                   parm                    kpjba
010200030414
010203110809     C     *DTAARA       DEFINE                  Oflspedi                       *
010207110719
010211110719     C*---------- RICERCA DITTA :
010212110810     c                   move      '201'         socpas            3
010213110719     C                   MOVEL     'SOC001'      TIPXSC
010214110719     C                   MOVEL     *blank        SOCXSC
010215110719     C                   EXSR      REPSOC
010216110719     C     RTNXSC        IFNE      '1'
010217110719     C                   MOVEL     XSOCDS        SOC001
010221110719     c                   end
010222110719
014600020521     c                   endsr
014601110719     C*----------------------------------------------------*
014602110719     C* Reperimento dati società
014603110719     C*----------------------------------------------------*
014604110719     C     REPSOC        BEGSR
014605110719     C*
014606110809     C                   CALL      'XSOC'
014607110719     C                   PARM                    TIPXSC            6
014608110719     C                   PARM      socpas        SOCXSC            3
014609110719     C                   PARM                    CDSXSC            9 0
014610110719     C                   PARM                    MODXSC            3
014611110719     C                   PARM      *blanks       RTNXSC            1
014612110719     C                   PARM                    XSOCDS
014613110719     C                   PARM                    KPJBA
014614110719     C*
014615110719     C                   ENDSR
014616110719     c**********************************************************************
014617110719     c* reperimento numero viaggio
014618110719     c**********************************************************************
014619110719     C     RepNumRig     BEGSR
014620110719     c                   clear                   trul33ds
014621110719     c                   move      'L'           i33tla
014622110809     c                   z-add     36            i33cnu
014623110719     c                   z-add     1             i33num
014624110719     c                   movel     trul33ds      kpjbu
014625110719     c                   call      'TRUL33R'
014626110719     c                   PARM                    kpjba
014627110719     c                   movel     kpjbu         trul33ds
014628110809     c                   z-add     o33nrf        mofnpt
014630110719     C                   ENDSR
014700030414      *-------------------------------------------------
