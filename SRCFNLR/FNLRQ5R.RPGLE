000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000101101220     h dftactgrp(*no) actgrp(*caller)
000200950315      *---------*----------------------------------------------------*
000500101220      *         - Sitemazione controllo autotrasportatore -          *
000600000426      *                                                              *
000700110104      *  Il programma prevede il caricamento a video degli errori    *
000800110104      *  sul dettaglio dei colli      memorizzati in file di Work,   *
000900110104      *  in fase di controllo autotrasp.                             *
001100950315      *--------------------------------------------------------------*
001200110104     FFNLRQ5D   CF   E             WORKSTN SFILE(LR53S01:NRR3)
001301101220     fwfnlrq21l if   e           k disk
001302101223     ffiapd01l  if   e           k disk
001303110120     ffnfvv01l  if   e           k disk
001304110120     ffidst01l  if   e           k disk
001305101223     fazorg01l  if   e           k disk
002800950320      *----------------------------------------------------------------*
002900950320      *    s c h i e r e                                               *
003000950320      *----------------------------------------------------------------*
003100980325     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              ERRORI
003200950315     D KPJBA         E DS
003300101220     D fnlr59ds      E DS
003301101223     D fnlr98ds      E DS
003302101223     D fnlrq2ds      E DS
003303101223     D fnlrq3ds      E DS
003304101223     D fnlrq5ds      E DS
003500101220     D fidg311ds     E DS
003501101223     D fidg39ds      E DS
004000080707     d trul06ds      e ds
004001110103     d  lin£                   1     90  0 dim(30)
004002101220     D ds_PARA04       DS
004003101220     D  p04NFV                 1      5  0
004004101220     D  p04LNP                 6      8  0
004005101220     D  p04LNA                 9     11  0
004006101220     D  p04NRS                12     13  0
004007101220     D  p04NCD                14     20  0
004008101220     D  p04NCA                21     27  0
004009101220     D  p04NPS                28     29  0
004010101220     D  p04NPG                30     30  0
004011101220     D  p04FLG                31     31
004012101220     D  p04PGM                32     38
004013101220     D  p04FGS                39     41  0
004014110103     d  p04bolp               42     44  0
004015110103     d  p04bors               45     46  0
004016110103     d  p04bosp               47     53  0
004017110103     d  p04boan               54     57  0
004018110103     D  PA4ERR                58     58
005100080515     D ddatiute      e ds
005200080515     D azuteds       e ds                  extname(AZUTE00F)
005300080515     D tibs34ds      E DS                  inz
005600081117     D                SDS
005700081117     D  NOMPGM                 1     10
005800080701      * DS per richiamo pgm FNLR36R
005900080701     D DSLR36          DS
006000080701     D  P36AAS                14     17  0
006100080701     D  P36LNP                18     20  0
006200080701     D  P36NRS                21     22  0
006300080701     D  P36NSP                23     29  0
006400080701     D  P36F03                30     30
006500080701     D  P36FLG                31     31
006501110117     D savfls          s                   like(v3cfls)
006502110126     D savnrc          s                   like(v3cnrc)
006503110126     D savnsc          s                   like(v3cnsc)
006504110117     D dtaiso          s               d
006505110117     D dtaeur          s               d   datfmt(*eur)
007800950315      *---------------------------------------------------------------*
007900950315      * FLUSSO PRINCIPALE                                             *
008000950315      *---------------------------------------------------------------*
008100080508     C*  Caricamento subfile errori
008200080508     C                   EXSR      INZS01
008300101220     c*  Se il subfile degli errori è vuoto forzo l'uscita dal pgm
008500080526     C     NRR3          IFEQ      0
008501110104     C                   MOVEL     '12'          lrq5cmdo
008600110104     c                   eval      lrq5msgo = 'Non esiste dettaglio colli'
008800101223     c                   exsr      srfine
008900080526     C                   END
009000080701     c*
009100101220     C                   Z-ADD     D59fgs        v3Clna
009200080710     c                   clear                   v3dlna
009300080710     c     v3clna        chain     azorg01l
009400080710     c                   if        %found(azorg01l)
009500080710     c                   eval      v3dlna = orgdes
009600080710     c                   end
009700101220     C                   Z-ADD     D59ndd        v3Cnd1
009701101220     C                   Z-ADD     D59pdr        v3Cpdr
009702110104     C                   Z-ADD     lrq5aasi      v3Caas
009703110104     C                   Z-ADD     lrq5lnpi      v3Clnp
009704110104     C                   Z-ADD     lrq5nrsi      v3Cnrs
009705110104     C                   Z-ADD     lrq5nspi      v3Cnsp
009706110203     C                   Z-ADD     lrq5ncci      v3Cncc
009707110203     C                   Z-ADD     lrq5ncei      v3hnce
009708110203     C                   Z-ADD     lrq5ncii      v3Cnci
009709101220     c                   clear                   v3dpdr
009710101220     c     kapd          chain     fiapd01l
009711101220     c                   if        %found(azorg01l)
009712101220     c                   eval      v3dpdr = apdrsf
009713101220     c                   end
010200080512     C                   Z-ADD     1             NRR3
010300101223      *  gestione subfile degli errori
010400080508     C                   DO        *hival
010600081117     C                   WRITE     LR53Z01
010700080508     C                   EXFMT     LR53C01
010800080508      *  Fine Lavoro
010901101220     c                   If        *inKC  = '1'
011000110104     C                   MOVEL     '03'          lrq5cmdo
011100101222     C                   exsr      srfine
011200080508     C                   END
011300080508      *  Guida
011401101220     c                   If        *inKL  = '1'
011500110104     C                   MOVEL     '12'          lrq5cmdo
011600101222     C                   exsr      srfine
011700080508     C                   END
011757101220      *  Interrogazione spunte
011758101220     c                   If        *inKV  = '1'
011759101220     C                   CLEAR                   ds_para04
011760110104     C                   MOVEL     'FNLRQ5R'     P04PGM
011764110104     c                   eval      P04NPS = 99
011765110104     c                   eval      P04NPG = 9
011766110104     C                   MOVEL     '2'           P04FLG
011767110104     C                   Z-ADD     v3clna        P04FGS
011768110104     c                   eval      P04NRS = v3cNRS
011769110104     c                   eval      p04bolp = V3cLNP
011770110104     c                   eval      p04bors = V3cNRS
011771110104     c                   eval      p04bosp = V3cNSP
011772110104     c                   eval      p04boan = V3cAAS
011773110103     C                   MOVEL     ds_PARA04     KPJBU
011774101220     C                   CALL      'FNLR04R'
011775101220     C                   PARM                    KPJBA
011776101220     c                   end
012900080508      *  Loop di controlli
013000080508     C                   EXSR      CTR01
013100080701     c   28              iter
014000080508     C                   ENDdo
014100080508     c*
014200080508     c                   exsr      srfine
014300950317      *---------------------------------------------------------------*
014400950317      * Gestione videata subfile errori                               *
014500950317      *---------------------------------------------------------------*
014600080508     C     INZS01        BEGSR
014700080508     C                   SETOFF                                       202190
014800080508     C                   WRITE     LR53C01
014900950317     C                   SETON                                        2021
015000080512     C                   Z-ADD     0             NRR3              4 0
018006101223     c*
018007101223     c                   exsr      srsfl
018008101223     c*
019000080523     c                   endsr
057500080508      *---------------------------------------------------------------*
057600101223      *  Loop di errori  per segnacollo                               *
057700080508      *---------------------------------------------------------------*
057800110104     C     srsfl         BEGSR
057804101223     c     kq20          setll     wfnlrq21l
057805101223     c                   do        *hival
057806101223     c     kq20          reade     wfnlrq21l
057807101223     c                   if        %eof(wfnlrq21l)
057808101223     c                   leave
057809101223     c                   end
057810110104     c                   if        WLRQ2fgs <> d59fgs or
057811110104     c                             WLRQ2ndc <> d59ndd or
057812110104     c                             WLRQ2pdr <> d59pdr
057813101223     c                   iter
057814101223     c                   end
057815110117     c                   if        WLRQ2FLs = savfls and
057816110126     c                             WLRQ2NSC = savnsc and
057817110126     c                             WLRQ2NrC = savnrc
057818110117     c                   clear                   v3cfls
057819110117     c                   clear                   v3cnsc
057820110126     c                   clear                   v3cnrc
057821110117     c                   else
057822110104     C                   Z-ADD     WLRQ2FLs      v3cFLS
057823110104     C                   Z-ADD     WLRQ2NSC      v3cnsc
057824110126     C                   Z-ADD     WLRQ2NrC      v3cnrc
057825110117     C                   Z-ADD     WLRQ2FLs      savFLS
057826110117     C                   Z-ADD     WLRQ2NSC      savnsc
057827110126     C                   Z-ADD     WLRQ2NrC      savnrc
057828110117     c                   end
057829110104     C                   Z-ADD     WLRQ2FLv      v3cFLv
057830110104     C                   Z-ADD     WLRQ2NPG      v3cNPG
057831110104     C                   Z-ADD     WLRQ2NFV      v3cNFV
057832110117     c                   clear                   v3cdfv
057833110117     c                   if        wlrq2dfv <> 0
057834110117     C                   move      WLRQ2dFV      dtaiso
057835110117     C                   move      dtaiso        dtaeur
057836110117     C                   move      dtaeur        v3cdfv
057837110117     c                   end
057838110104     C                   z-add     WLRQ2NPS      v3cNPS
057839110104     C                   z-add     WLRQ2HMS      v3cHMS
057840110104     C                   movel(p)  WLRQ2ERR      v3cmsg
057841110120     c* aggancio il foglio per decodificare l'autista o il tipo di foglio
057842110120     c                   clear                   v3ddes
057843110120     c                   if        v3cnfv <> 0
057844110120     c                   if        v3cnpg = 4
057851110120     c                   z-add     v3cnfv        dstnfv
057852110120     c     kdst          chain     fidst01l
057853110120     c                   if        %found(fidst01l)
057854110120     c     kapd1         chain     fiapd01l
057855110120     c                   if        %found(fiapd01l)
057856110120     c                   movel(p)  apdrsf        v3ddes
057857110120     c                   end
057858110120     c                   end
057859110120     c                   else
057861110120     c                   z-add     v3cnfv        fvvnfv
057862110120     c     kfvv          chain     fnfvv01l
057863110120     c                   if        %found(fnfvv01l)
057864110120     c                   movel(p)  fvvdsf        v3ddes
057865110120     c                   end
057866110120     c                   end
057867110120     c                   end
057868110104     C                   ADD       1             NRR3
057869110104     C                   WRITE     LR53S01
057870101223     c                   enddo
057871101223     c                   endsr
057872101223      *---------------------------------------------------------------*
057873101223      *  Loop di errori  subfile errori                               *
057874101223      *---------------------------------------------------------------*
057875101223     C     CTR01         BEGSR
057900080715     c                   setoff                                       2840
058000950315      *
058100080509     C                   READC     LR53S01                                32
058200980325      *
058300950317     C     *IN32         DOWEQ     '0'
058400950320     C     *IN28         ANDEQ     '0'
058600080701     c                   select
061700080728     C                   ENDsl
064200980325      *  Aggiorno subfile
064300950320     C                   SETON                                        22
064400080707     C                   UPDATE    LR53s01
064500950320     C                   SETOFF                                       22
064600080711     C  N28              READC     LR53S01                                32
064700080509     C                   ENDdo
064800080715     c*
064900080715     c                   z-add     1             nrr3
065000950317     C*
065100950317     C                   ENDSR
082600080707      *---------------------------------------------------------------*
082700080707      * fine
082800080707      *---------------------------------------------------------------*
082900080707     C     srfine        BEGSR
083000101222     c                   movel(p)  fnlr59ds      kpjbu
083100080526     C*
083200080526     C                   SETON                                        LR
083300080710     c                   return
083400080707     c*
083500080707     C                   ENDSR
083600080509     C*-----------------------------------------------------*
083700080509     C     *INZSR        BEGSR
083800080509     C*-----------------------------------------------------*
083900080508      *
084000080508     C     *ENTRY        PLIST
084100101220     C                   PARM                    kpjba
084101110104     C                   PARM                    fnlrq5ds
084102101220     c                   movel     kpjbu         fnlr59ds
084103110104     c                   clear                   lrq5cmdo
084104110104     c                   clear                   lrq5msgo
084200080515     C* Decodifica Elaboratore
084300080515     c     *dtaara       define    §azute        azuteds
084400080515     c     *dtaara       define    §datiute      ddatiute
084500080515     C                   in(E)     *dtaara
084600080515     C                   IF        %Error  or  RSUT = *blanks
084700080515     C                   call      'TIBS34R'
084800080515     C                   parm                    Tibs34Ds
084900080515     C                   in        *dtaara
085000080515     c                   ENDIF
085001110120     c     kfvv          klist
085002110120     c                   kfld                    v3cnpg
085003110120     c                   kfld                    fvvnfv
085004110120     c                   kfld                    v3cflv
092800101223     c     kapd          klist
092900101223     c                   kfld                    apdtip
093000101223     c                   kfld                    d59pdr
093001110120     c     kdst          klist
093002110120     c                   kfld                    v3cnpg
093003110120     c                   kfld                    dstnfv
093004110120     c                   kfld                    v3cflv
093005110120     c     kapd1         klist
093006110120     c                   kfld                    apdtip
093007110120     c                   kfld                    dstpdr
093008101223     c     kq20          klist
093009110104     c                   kfld                    LRQ5aasi
093010110104     c                   kfld                    LRQ5lnpi
093011110104     c                   kfld                    LRQ5nrsi
093012110104     c                   kfld                    LRQ5nspi
093013101223     c                   eval      apdtip = 'A'
095200080509     C                   ENDSR
