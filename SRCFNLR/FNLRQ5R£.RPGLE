000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000101101220     h dftactgrp(*no) actgrp(*caller)
000200950315      *---------*----------------------------------------------------*
000500101220      *         - Sitemazione controllo autotrasportatore -          *
000600000426      *                                                              *
000700101220      *  Il programma prevede il caricamento a video degli errori in *
000800101222      *  fase di controllo autotrasp.,memorizzati in file di Work,   *
000900101222      *  per la loro risoluzione                                     *
001000101222      *  1= inserisci, 2= togli, 3= trasferisci ... di una spedizione*
001100950315      *--------------------------------------------------------------*
001200101220     FFNLRQ2D   CF   E             WORKSTN SFILE(LR53S01:NRR3)
001300101220     fwfnlrq11l if   e           k disk
001301101220     fwfnlrq21l if   e           k disk
001302101223     ffiapd01l  if   e           k disk
001303101223     fazorg01l  if   e           k disk
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
007800950315      *---------------------------------------------------------------*
007900950315      * FLUSSO PRINCIPALE                                             *
008000950315      *---------------------------------------------------------------*
008100080508     C*  Caricamento subfile errori
008200080508     C                   EXSR      INZS01
008300101220     c*  Se il subfile degli errori è vuoto forzo l'uscita dal pgm
008500080526     C     NRR3          IFEQ      0
008501110103     C                   MOVEL     '12'          lrq2cmdo
008600110103     c                   eval      lrq2msgo = 'Nessun errore presente'
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
009702101220     c                   clear                   v3dpdr
009703101220     c     kapd          chain     fiapd01l
009704101220     c                   if        %found(azorg01l)
009705101220     c                   eval      v3dpdr = apdrsf
009706101220     c                   end
010200080512     C                   Z-ADD     1             NRR3
010300101223      *  gestione subfile degli errori
010400080508     C                   DO        *hival
010600081117     C                   WRITE     LR53Z01
010700080508     C                   EXFMT     LR53C01
010800080508      *  Fine Lavoro
010901101220     c                   If        *inKC  = '1'
011000110103     C                   MOVEL     '03'          lrq2cmdo
011100101222     C                   exsr      srfine
011200080508     C                   END
011300080508      *  Guida
011401101220     c                   If        *inKL  = '1'
011500110103     C                   MOVEL     '12'          lrq2cmdo
011600101222     C                   exsr      srfine
011700080508     C                   END
011701101220     c* fasi
011702101220     c                   If        *inKH  = '1'
011703101220     c                   clear                   fidg39dS
011704101220     c                   eval      fdg39NPG = 4
011705101220     c                   z-add     v3cnd1        fdg39NFV
011706101220     c                   z-add     v3clna        fdg39FGS
011707101220     c                   move      'N'           fdg39noREC
011708101220     c                   clear                   fdg39VIDno
011710101220     c                   movel(p)  fidg39ds      KPJBU
011711101220     c                   call      'FIDG39R'
011712101220     c                   parm                    kpjba
011720101220     c                   end
011721101220      *  Dettaglio Distinta
011722101220     c                   If        *inKI  = '1'
011725101220     c                   clear                   fidg311dS
011726101220     c                   z-add     v3clna        f31FGS
011727101220     c                   eval      f31Npg = 4
011728101220     c                   z-add     v3cnd1        f31NumDis
011729110103     c*                  z-add     dstDFV        f31DatDis
011746101220     c                   movel(p)  fidg311ds     KPJBU
011747101220     c                   call      'FIDG311R'
011748101220     c                   parm                    kpjba
011749101220     c                   eval      fidg311ds =   KPJBU
011750101220     c                   if        F31Tfunz = '03'
011751110103     c                   move      f31tfunz      lrq2cmdo
011752101222     C                   exsr      srfine
011753101220     c                   end
011756101220     c                   end
011757101220      *  Interrogazione spunte
011758101220     c                   If        *inKV  = '1'
011759101220     C                   CLEAR                   ds_para04
011760101220     C                   Z-ADD     v3cnd1        P04NFV
011761101220     C                   Z-ADD     99            P04NRS
011762101220     C                   Z-ADD     99            P04NPS
011763101220     C                   MOVEL     4             P04NPG
011764101220     C                   MOVEL     '1'           P04FLG
011765101220     C                   MOVEL     'FIDG04R'     P04PGM
011766101220     C                   Z-ADD     v3clna        P04FGS
011768110103     C                   MOVEL     ds_PARA04     KPJBU
011769101220     C                   CALL      'FNLR04R'
011770101220     C                   PARM                    KPJBA
011772101220     c                   end
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
017900101222     c* aggancio la distinta e faccio tutti i controlli
017901101223     c                   z-add     4             lrq3npgi
017902101223     c                   z-add     d59fgs        lrq3fgsi
017903101223     c                   z-add     d59ndd        lrq3ndci
017904101223     c                   eval      lrq3alci = 'S'
017905101223     c                   call      'FNLRQ3R'
017906101223     c                   parm                    fnlrq3ds
018001110103     c                   if        lrq3erro <> ' '
018002110103     c                   eval      lrq2cmdo = '12'
018003110103     c                   eval      lrq2msgo = lrq3msgo
018004101223     c                   exsr      srfine
018005101223     c                   end
018006101223     c*
018007101223     c                   exsr      srsfl
018008101223     c*
019000080523     c                   endsr
019104080523     c**********************************************************************
019200101223     c* memorizzo spedizione errata nel sfl
019300080523     c**********************************************************************
019400101223     c     srsfl         begsr
019401101223     c* se ho solo un'anomalia a livello di spedizione, la carico,
019402101223     c* altrimenti aggancio il dettaglio
019403101223     c     *loval        setll     wfnlrq11l
019404101223     c                   do        *hival
019405101223     c                   read      wfnlrq11l
019406101223     c                   if        %eof(wfnlrq11l)
019407101223     c                   leave
019408101223     c                   end
019409101223     c                   if        WLRQ1fgs <> d59fgs or
019410101223     c                             WLRQ1ndc <> d59ndd or
019411101223     c                             WLRQ1pdr <> d59pdr
019412101223     c                   iter
019413101223     c                   end
021000101223     C                   Z-ADD     wlrq1aas      v3caas
021100101223     C                   Z-ADD     wlrq1lnp      v3clnp
021200101223     C                   Z-ADD     wlrq1nrs      v3cnsp
021300101223     C                   Z-ADD     wlrq1nsp      v3cnrs
021301101223     C                   movel(p)  WLRQ1ERR      v3cmsg
021302101223     C                   movel(p)  WLRQ1sce      v3csce
021303101223     c                   if        v3csce = ' '
021304101223     c                   exsr      srdett
021305101223     c                   end
021400080526     C                   ADD       1             NRR3
021500080526     C                   WRITE     LR53S01
021501101223     c                   enddo
021600080526     c                   endsr
057500080508      *---------------------------------------------------------------*
057600101223      *  Loop di errori  per segnacollo                               *
057700080508      *---------------------------------------------------------------*
057800101223     C     srdett        BEGSR
057801101223     c                   clear                   v3hnpg
057802101223     c                   clear                   v3hfgs
057803101223     c                   clear                   v3hndc
057804101223     c     kq20          setll     wfnlrq21l
057805101223     c                   do        *hival
057806101223     c     kq20          reade     wfnlrq21l
057807101223     c                   if        %eof(wfnlrq21l)
057808101223     c                   leave
057809101223     c                   end
057810101223     c                   if        WLRQ2fgs <> WLRQ1fgs or
057811101223     c                             WLRQ2ndc <> WLRQ1ndc or
057812101223     c                             WLRQ2pdr <> WLRQ1pdr
057813101223     c                   iter
057814101223     c                   end
057816101223     c                   if        WLRQ2sce <> v3csce  and v3csce <> ' '
057817101223     c                   clear                   v3csce
057818101223     c                   clear                   v3hnpg
057819101223     c                   clear                   v3hfgs
057820101223     c                   clear                   v3hndc
057821101223     c                   leave
057822101223     c                   end
057823101223     c                   if        WLRQ2fls <> v3hfgs  and v3hfgs <> 0
057824101223     c                   clear                   v3csce
057825101223     c                   clear                   v3hnpg
057826101223     c                   clear                   v3hfgs
057827101223     c                   clear                   v3hndc
057828101223     c                   leave
057829101223     c                   end
057830101223     c                   if        WLRQ2npg <> v3hnpg  and v3hnpg <> 0
057831101223     c                   clear                   v3csce
057832101223     c                   clear                   v3hnpg
057833101223     c                   clear                   v3hfgs
057834101223     c                   clear                   v3hndc
057835101223     c                   leave
057836101223     c                   end
057837101223     c                   if        WLRQ2nfv <> v3hndc  and v3hndc <> 0
057838101223     c                   clear                   v3csce
057839101223     c                   clear                   v3hnpg
057840101223     c                   clear                   v3hfgs
057841101223     c                   clear                   v3hndc
057842101223     c                   leave
057843101223     c                   end
057844101223     c                   eval      v3csce = wLRQ2sce
057845101223     c                   eval      v3hnpg =WLRQ2npg
057846101223     c                   eval      v3hfgs =WLRQ2fls
057847101223     c                   eval      v3hndc =WLRQ2ndc
057848101223     c                   enddo
057849101223     c                   endsr
057850101223      *---------------------------------------------------------------*
057851101223      *  Loop di errori  subfile errori                               *
057852101223      *---------------------------------------------------------------*
057853101223     C     CTR01         BEGSR
057900080715     c                   setoff                                       2840
058000950315      *
058100080509     C                   READC     LR53S01                                32
058200980325      *
058300950317     C     *IN32         DOWEQ     '0'
058400950320     C     *IN28         ANDEQ     '0'
058500080704     c*
058600080701     c                   select
058700080509      *  Interrogazione bolla
058800080512     C                   when      v3csce = 'V'
058900950317     C                   MOVEL     '1'           P36FLG
059000950317     C                   Z-ADD     V3CAAS        P36AAS
059100950317     C                   Z-ADD     V3CLNP        P36LNP
059200950317     C                   Z-ADD     V3CNSP        P36NSP
059300950317     C                   Z-ADD     V3CNRS        P36NRS
059400080509     C                   MOVEL(P)  DSLR36        KPJBU
059500950317     C                   CALL      'FNLR36R'
059600080701     c                   PARM                    KPJBA
059700080730     c                   MOVEL     ' '           v3CSCE
059701101223     c* opzione gestione spunte
059702101223     c                   when      v3csce = 'S'
059703101223     c                   clear                   ds_para04
059704101223     c                   eval      P04NFV = *zeros
059705101223     c                   eval      P04NRS = v3cNRS
059706101223     c                   eval      P04lnp = v3cLNP
059709110103     c                   eval      P04ncd = v3cND1
059710110103     c                   eval      P04nca = v3cND1
059711101223     c                   eval      P04NPS = 99
059712101223     c                   eval      P04NPG = 9
059713101223     c                   eval      P04FLG = '2'
059715110103     c                   eval      P04FGS = v3clna
059716101223     c                   eval      p04bolp = V3cLNP
059717101223     c                   eval      p04bors = V3cNRS
059718101223     c                   eval      p04bosp = V3cNSP
059719101223     c                   eval      p04boan = V3cAAS
059720110103     c                   eval      Pa4ERR  = *blank
059721101223     c                   eval      kpjbu = ds_para04
059722101223     c                   call      'FNLR04R'
059723101223     c                   parm                    kpjba
059724101223     c* opzione dettaglio colli
059725101223     c                   when      v3csce = 'D'
059726101223     c                   clear                   fnlrq5ds
059727101223     c                   eval      kpjbu = fnlr59ds
059728101223     c                   call      'FNLRQ5R'
059729101223     c                   parm                    kpjba
059730101223     c                   parm                    fnlrq5ds
059731101223     c* opzione inserisci, togli o trasferisci
059733101223     C                   when      v3csce <> v3hsce
059736101223     c                   seton                                        2840
059737101223     C                   eval      $MSG = 'Opzione non ammessa'
059738101223     c* il pgm FNLR98 gira in dftactgrp e committa ogni aggiornamento
059739101223     c*
059741101223     C                   when      (v3csce = '1' or v3csce = '2'
059742101223     c                             or v3csce = '3') and *inkf
059743101223     c                   clear                   fnlr98ds
059744101223     c                   eval      i98aas = v3caas
059745101223     c                   eval      i98lnp = v3clnp
059746101223     c                   eval      i98nrs = v3cnrs
059747101223     c                   eval      i98nsp = v3cnsp
059748101223     c                   eval      I98VIDEO  = 'N'
059749101223     c                   eval      I98ALCDST = 'N'
059750101223     c                   eval      I98CTLDST = 'N'
059751101223     c                   eval      I98npg    = 4
059752101223     c                   eval      I98fgs    = d59fgs
059753101223     c                   eval      I98ndc    = d59ndd
059754101223     c*
059755110103     c                   do        2             x                 2 0
059756101223     c                   if        v3csce = '2' or
059757101223     c                             (v3csce = '3' and x = 2)
059758101223     c                   eval      I98DELINS = 'M'
059759101223     c                   if        (v3csce = '3' and x = 2 and v3hnpg = 4
059760101223     c                              and v3hndc <> d59ndd)
059763101223     c                   eval      I98npg    = v3hnpg
059764101223     c                   eval      I98fgs    = v3hfgs
059765101223     c                   eval      I98ndc    = v3hndc
059768101223     c                   end
059769101223     c                   end
059770101223     c                   clear                   fnlr98ds
059771101223     c                   eval      kpjbu = fnlr98ds
059772101223     c                   call      'FNLR98C'
059773101223     c                   parm                    kpjba
059774101223     C                   if        o98esito <>' '
059775101223     c                   seton                                        2840
059776101223     C                   eval      $MSG = o98msgerr
059777101223     c                   leave
059778101223     c                   end
059779101223     c* solo per il trasferisci deve fare il secondo giro
059780101223     c                   if        v3csce = '1' or v3csce = '2'
059781101223     c                   leave
059782101223     c                   end
059783101223     c                   enddo
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
084101101220     C                   PARM                    fnlrq2ds
084102101220     c                   movel     kpjbu         fnlr59ds
084103101220     c                   clear                   fnlrq2ds
084200080515     C* Decodifica Elaboratore
084300080515     c     *dtaara       define    §azute        azuteds
084400080515     c     *dtaara       define    §datiute      ddatiute
084500080515     C                   in(E)     *dtaara
084600080515     C                   IF        %Error  or  RSUT = *blanks
084700080515     C                   call      'TIBS34R'
084800080515     C                   parm                    Tibs34Ds
084900080515     C                   in        *dtaara
085000080515     c                   ENDIF
092800101223     c     kapd          klist
092900101223     c                   kfld                    apdtip
093000101223     c                   kfld                    d59pdr
093001101223     c     kq20          klist
093002101223     c                   kfld                    WLRQ1aas
093003101223     c                   kfld                    WLRQ1lnp
093004101223     c                   kfld                    WLRQ1nrs
093005101223     c                   kfld                    WLRQ1nsp
093006101223     c                   eval      apdtip = 'A'
095200080509     C                   ENDSR
