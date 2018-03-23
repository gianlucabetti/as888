000200110719     H DEBUG DECEDIT('0,') DATEDIT(*ymd/) Option(*nodebugio)
000300110719     ftmmoc02l  uf   e           k disk
000403110719     Fancln01l  IF   E           K DISK
000404110719     Fansog01l  IF   E           K DISK
000405110719     Fanind01l  IF   E           K DISK
000406110809     fxx3ol1p   uf a e             disk
000407110809     fxx3ol2p   uf a e             disk
000408110809     fxx3ol3p   uf a e             disk
000500030110
000501110719     d kpjba         e ds
000502110719     D soc001        E DS                  EXTNAME(xsoc001ds)
000503110719     D xsocds          DS          1000
000504110719
000505110719     D trul33ds      E DS
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
001801110719     D oclspedi        DS
001802110809     D  numsped                1      9  0
001900030109     d
002000000607      *--------------------------------------------------------------------
002100000607     c
002200110719      * lettura file ordini da trasmettere per prelievi a logistica
002300110719     c     *loval        setll     tmmoc02l
002301110719     c                   do        *hival
002302110719     c                   read      tmmoc02l
002303110719     c                   if        %eof(tmmoc02l)
002304110719     c                   leave
002305110719     c                   endif
002306110719      *reperisce numero assoluto riga per legame gestionale con trasmissione a logistica
002307110719     c                   exsr      RepNumRig
002308110719      * reperisce numero progressivo per spedizioni logistica
002309110719     C     *LOCK         IN        Oclspedi                             46
002310110719     C   46              GOTO      fine
002312110719      * primo ordine oppure rottura cliente
002313110719     c                   if        exclie <> moccli
002314110809     c                   move      moccli        exclie            8
002315110719     C     KEYcln        CHAIN     ancln01l                           20
002316110719     C     clnsogg       CHAIN     ansog01l                           20
002317110719     C     clnsogg       CHAIN     anind01l                           21
002318110720     c                   eval      x1sit  = '04'
002319110720     c                   eval      x1dep  = 'MAGASEDE'
002320110719     c                   eval      numsped = numsped + 1
002321110719     c                   movel     numsped       x1nliv
002322110809     c                   z-add     mocnor        exncmd            5 0
002323110719     c                   movel     mocnor        x1ncmd
002324110719     c                   move      mocdor        g06inv
002325110719     c                   move      '3'           g06err
002326110719     c                   call      'XSRDAT'
002327110719     c                   parm                    wlbdat
002328110719     c                   move      g06dat        x1dbon
002329110719     c                   move      g06dat        x1dol
002330110719     c                   move      mocgpm        x1tyol
002331110719     c                   move      'DDT'         x1tybl
002332110719     c                   move      'UT'          x1cmvt
002333110720     c                   move      '04'          x1site
002334110719     c                   move      moccli        x1clil
002335110719     c                   move      moccli        x1zonl
002336110719     C                   MOVEL     sogdes        x1rsl1
002337110719     C                   MOVEL     indindriz     x1adl1
002338110719     C                   movel     indcap        x1cpsl
002339110719     C                   MOVEL     INDlocalit    x1vill
002340110719     C                   MOVE      INDPRov       x1vill
002341110719     c                   movel     'BART001'     x1tran
002342110719     c                   move      'UTF'         x1mtra
002343110719     c                   move      'F  '         x1ttra
002344110809     c                   write     xx3ol1
002345110719     c                   eval      x2sit   = x1sit
002346110719     c                   eval      x2dep   = x1dep
002347110719     c                   eval      x2nliv  = x1nliv
002348110719     c                   eval      x2ncmd  = x1ncmd
002349110719     c                   eval      x2dbon  = x1dbon
002350110809     c                   write     xx3ol2
002351110719     c                   eval      x3sit   = x1sit
002352110719     c                   eval      x3dep   = x1dep
002353110719     c                   eval      x3nliv  = x1nliv
002354110719     c                   eval      x3ncmd  = x2ncmd
002355110719     c                   eval      x3nlcm  = 1
002356110719     c                   eval      x3qcnt  = mocqta
002357110809     c                   eval      x3niv   = 5
002358110719     c                   eval      x3art   = mocmat
002359110719     c                   movel     mocnpc        x3spec
002360110809     c                   write     xx3ol3
002361110719     c                   update    tmmoc000
002362110719     c                   iter
002363110719     c                   else
002364110719      *rottura ordine stesso cliente
002365110719     c                   if        exncmd <> mocnor
002366110719     c                   z-add     mocnor        exncmd
002367110719     c                   eval      x2sit   = x1sit
002368110719     c                   eval      x2dep   = x1dep
002369110719     c                   eval      x2nliv  = x1nliv
002370110809     c                   movel     mocnor        x2ncmd
002371110719     c                   move      mocdor        g06inv
002372110719     c                   move      '3'           g06err
002373110719     c                   call      'XSRDAT'
002374110719     c                   parm                    wlbdat
002375110719     c                   move      g06dat        x2dbon
002376110809     c                   write     xx3ol2
002377110719     c                   eval      x3sit   = x1sit
002378110719     c                   eval      x3dep   = x1dep
002379110719     c                   eval      x3nliv  = x1nliv
002380110719     c                   eval      x3ncmd  = x2ncmd
002381110719     c                   eval      x3nlcm  = 1
002382110719     c                   eval      x3qcnt  = mocqta
002383110809     c                   eval      x3niv   = 5
002384110719     c                   eval      x3art   = mocmat
002385110719     c                   movel     mocnpc        x3spec
002386110809     c                   write     xx3ol3
002387110719     c                   update    tmmoc000
002388110719     c                   iter
002389110719      *altra riga stesso ordine
002390110719     c                   else
002391110719     c                   eval      x3sit   = x1sit
002392110719     c                   eval      x3dep   = x1dep
002393110719     c                   eval      x3nliv  = x1nliv
002394110719     c                   eval      x3ncmd  = x2ncmd
002395110719     c                   eval      x3nlcm  = 1
002396110719     c                   eval      x3qcnt  = mocqta
002397110809     c                   eval      x3niv   = 5
002398110719     c                   eval      x3art   = mocmat
002399110719     c                   movel     mocnpc        x3spec
002400110809     c                   write     xx3ol3
002401110719     c                   update    tmmoc000
002402110719     c                   endif
002403110719     c                   endif
002404030415     c
002405110719     c                   enddo
002500110719      * scrivere ultima riga !!!!!!!
002800020517
002801110719     c     fine          tag
002802110719      *area data spedizioni logistica
002803110719     C                   OUT       Oclspedi
002900020521     c                   seton                                        lr
009700020521      *-------------------------------------------------
009800020521     c     *inzsr        begsr
009900020521      *-------------------------------------------------
010000020626     c     *entry        plist
010100110719     c                   parm                    kpjba
010200030414
010203110719     C     *DTAARA       DEFINE                  Oclspedi                       *
010207110719
010211110719     C*---------- RICERCA DITTA :
010212110719     c                   eval      socpas = '201'
010213110719     C                   MOVEL     'SOC001'      TIPXSC
010214110719     C                   MOVEL     *blank        SOCXSC
010215110719     C                   EXSR      REPSOC
010216110719     C     RTNXSC        IFNE      '1'
010217110719     C                   MOVEL     XSOCDS        SOC001
010221110719     c                   end
010222110719
010224110719     C     KEYcln        KLIST                                                  *
010225110809     C                   KFLD                    socpas            3            *
010226110719     C                   KFLD                    moccli                         *
014500030415
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
014622110719     c                   z-add     35            i33cnu
014623110719     c                   z-add     1             i33num
014624110719     c                   movel     trul33ds      kpjbu
014625110719     c                   call      'TRUL33R'
014626110719     c                   PARM                    kpjba
014627110719     c                   movel     kpjbu         trul33ds
014628110719     c                   z-add     o33nrf        mocnpc
014630110719     C                   ENDSR
014700030414      *-------------------------------------------------
