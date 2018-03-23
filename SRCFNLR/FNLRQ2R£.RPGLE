000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200101220     h dftactgrp(*no) actgrp(*caller)
000300950315      *---------*----------------------------------------------------*
000400101220      *         - Sitemazione controllo autotrasportatore -          *
000500000426      *                                                              *
000600101220      *  Il programma prevede il caricamento a video degli errori in *
000700101222      *  fase di controllo autotrasp.,memorizzati in file di Work,   *
000800101222      *  per la loro risoluzione                                     *
000900101222      *  1= inserisci, 2= togli, 3= trasferisci ... di una spedizione*
001000950315      *--------------------------------------------------------------*
001100101220     FFNLRQ2D   CF   E             WORKSTN SFILE(LR53S01:NRR3)
001200110119     ffnarb01l  if   e           k disk
001300151006     FFiar501L  IF   E           K DISK
001400110119     ffidst01l  if   e           k disk
001500110119     ffnfvv01l  if   e           k disk
001600110119     ffiapd01l  if   e           k disk
001700110204     fwfnlrq10f if   e             disk
001800101220     fwfnlrq21l if   e           k disk
001900101223     fazorg01l  if   e           k disk
002000950320      *----------------------------------------------------------------*
002100950320      *    s c h i e r e                                               *
002200950320      *----------------------------------------------------------------*
002300980325     D ERR             S             70    DIM(7) CTDATA PERRCD(1)              ERRORI
002400950315     D KPJBA         E DS
002500140130      *
002600140130     d  svkpjbu        s                   like(kpjbu)
002700140130      *
002800140130      *  Utility di scrittura "MIC" -
002900140130     d fnlrq7ds      e ds
003000151006      *
003100151006     D Dar5emd       E DS
003200151006     D inviato_contro_alert...
003300151006     d                 s              1    inz('N')
003400140130     c*
003500101220     D fnlr59ds      E DS
003600101223     D fnlr98ds      E DS
003700101223     D fnlrq2ds      E DS
003800110117     D fnlr76ds      E DS
003900110119     D fnarbds       E DS                  extname(fnarb00f)
004000101223     D fnlrq5ds      E DS
004100101220     D fidg311ds     E DS
004200101223     D fidg39ds      E DS
004300141113     D fnlr98ds2     E DS
004400080707     d trul06ds      e ds
004500110103     d  lin£                   1     90  0 dim(30)
004600110118     D ddatiute      e ds
004700110118     D azuteds       e ds                  extname(AZUTE00F)
004800110118     D tibs34ds      E DS                  inz
004900110118     D wfnlrq2ds     E DS                  extname(wfnlrq20f)
005000110121      * DS per richiamo pgm FNLR36R interrogazione bolle
005100110121     D DSLR36          DS
005200110121     D  P36AAS                14     17  0
005300110121     D  P36LNP                18     20  0
005400110121     D  P36NRS                21     22  0
005500110121     D  P36NSP                23     29  0
005600110121     D  P36F03                30     30
005700110121     D  P36FLG                31     31
005800101220     D ds_PARA04       DS
005900101220     D  p04NFV                 1      5  0
006000101220     D  p04LNP                 6      8  0
006100101220     D  p04LNA                 9     11  0
006200101220     D  p04NRS                12     13  0
006300101220     D  p04NCD                14     20  0
006400101220     D  p04NCA                21     27  0
006500101220     D  p04NPS                28     29  0
006600101220     D  p04NPG                30     30  0
006700101220     D  p04FLG                31     31
006800101220     D  p04PGM                32     38
006900101220     D  p04FGS                39     41  0
007000110103     d  p04bolp               42     44  0
007100110103     d  p04bors               45     46  0
007200110103     d  p04bosp               47     53  0
007300110103     d  p04boan               54     57  0
007400110103     D  PA4ERR                58     58
007500081117     D                SDS
007600081117     D  NOMPGM                 1     10
007700110126     D                 DS
007800110119     D  aasc                          4  0
007900110119     D  lnpc                          3  0
008000110119     D  nrsc                          2  0
008100110119     D  nspc                          7  0
008200110119     D  flsc                          3  0
008300110126     D  nrcc                          2  0
008400110126     D  nscc                          7  0
008500110126     dcollo                    1     28  0
008600110126     D  flvc                          3  0
008700110127     D  npgc                          1  0
008800110126     D  nfvc                          7  0
008900110126     D  dfvc                          8  0
009000110127     dfoglio                  29     47  0
009100110127     dcolloima                 1     47  0
009200110119     D savcollo        s                   like(collo)
009300110126     D savcolloima     s                   like(colloima)
009400110127     D savfoglio       s                   like(foglio)
009500110117     D dtaiso          s               d
009600110117     D dtaeur          s               d   datfmt(*eur)
009700110117     D oggi            S              8  0
009800151006     d MSG_F2          c                   'F2 x Forzare -> presente ALERT di V-
009900151006     d                                     ARIAZIONE data CONSEGNA.'
010000950315      *---------------------------------------------------------------*
010100950315      * FLUSSO PRINCIPALE                                             *
010200950315      *---------------------------------------------------------------*
010300151006     C                   move      '0'           da_forzare        1
010400080508     C*  Caricamento subfile errori
010500080508     C                   EXSR      INZS01
010600101220     c*  Se il subfile degli errori è vuoto forzo l'uscita dal pgm
010700080526     C     NRR3          IFEQ      0
010800110103     C                   MOVEL     '12'          lrq2cmdo
010900110103     c                   eval      lrq2msgo = 'Nessun errore presente'
011000101223     c                   exsr      srfine
011100080526     C                   END
011200080701     c*
011300101220     C                   Z-ADD     D59fgs        v3Clna
011400080710     c                   clear                   v3dlna
011500080710     c     v3clna        chain     azorg01l
011600080710     c                   if        %found(azorg01l)
011700080710     c                   eval      v3dlna = orgdes
011800080710     c                   end
011900101220     C                   Z-ADD     D59ndd        v3Cnd1
012000101220     C                   Z-ADD     D59pdr        v3Cpdr
012100101220     c                   clear                   v3dpdr
012200101220     c     kapd          chain     fiapd01l
012300101220     c                   if        %found(azorg01l)
012400101220     c                   eval      v3dpdr = apdrsf
012500101220     c                   end
012600080512     C                   Z-ADD     1             NRR3
012700101223      *  gestione subfile degli errori
012800080508     C                   DO        *hival
012900151006      *
013000151006      *  spegne il Pro-memoria se premuto il tasto F02
013100151006     c                   move      da_forzare    *in66
013200151006      *
013300081117     C                   WRITE     LR53Z01
013400080508     C                   EXFMT     LR53C01
013500151006     C*
013600080508      *  Fine Lavoro
013700101220     c                   If        *inKC  = '1'
013800110103     C                   MOVEL     '03'          lrq2cmdo
013900101222     C                   exsr      srfine
014000080508     C                   END
014100080508      *  Guida
014200101220     c                   If        *inKL  = '1'
014300110103     C                   MOVEL     '12'          lrq2cmdo
014400101222     C                   exsr      srfine
014500080508     C                   END
014600110117      *  Rivisualizza (no abilitato troppo pesante)
014700110104     c                   If        *inKE  = '1'
014800110104     C                   MOVEL     '05'          lrq2cmdo
014900110104     C                   exsr      srfine
015000110104     C                   END
015100101220     c* fasi
015200101220     c                   If        *inKH  = '1'
015300101220     c                   clear                   fidg39dS
015400101220     c                   eval      fdg39NPG = 4
015500110119     c                   z-add     d59ndd        fdg39NFV
015600110119     c                   z-add     d59fgs        fdg39FGS
015700101220     c                   move      'N'           fdg39noREC
015800101220     c                   clear                   fdg39VIDno
015900101220     c                   movel(p)  fidg39ds      KPJBU
016000101220     c                   call      'FIDG39R'
016100101220     c                   parm                    kpjba
016200101220     c                   end
016300101220      *  Dettaglio Distinta
016400101220     c                   If        *inKI  = '1'
016500101220     c                   clear                   fidg311dS
016600110119     c                   z-add     d59fgs        f31FGS
016700101220     c                   eval      f31Npg = 4
016800110119     c                   z-add     d59ndd        f31NumDis
016900101220     c                   movel(p)  fidg311ds     KPJBU
017000101220     c                   call      'FIDG311R'
017100101220     c                   parm                    kpjba
017200101220     c                   eval      fidg311ds =   KPJBU
017300101220     c                   if        F31Tfunz = '03'
017400110103     c                   move      f31tfunz      lrq2cmdo
017500101222     C                   exsr      srfine
017600101220     c                   end
017700101220     c                   end
017800110121      *  Interrogazione bolle
017900110121     c                   If        *inKJ  = '1'
018000110121     c                   clear                   dslr36
018100110121     C                   MOVEL     '2'           P36FLG
018200110121     C                   MOVEL     DSLR36        KPJBU
018300110121     C                   CALL      'FNLR36R'
018400110121     C                   PARM                    KPJBA
018500110121     c                   end
018600101220      *  Interrogazione spunte
018700101220     c                   If        *inKV  = '1'
018800101220     C                   CLEAR                   ds_para04
018900110119     C                   Z-ADD     d59ndd        P04NFV
019000101220     C                   Z-ADD     99            P04NRS
019100101220     C                   Z-ADD     99            P04NPS
019200101220     C                   MOVEL     4             P04NPG
019300101220     C                   MOVEL     '1'           P04FLG
019400110104     C                   MOVEL     'FNLRQ2R'     P04PGM
019500110119     C                   Z-ADD     d59fgs        P04FGS
019600110103     C                   MOVEL     ds_PARA04     KPJBU
019700101220     C                   CALL      'FNLR04R'
019800101220     C                   PARM                    KPJBA
019900101220     c                   end
020000080508      *  Loop di controlli
020100080508     C                   EXSR      CTR01
020200151006      *---
020300080701     c   28              iter
020400151006      *---
020500110121     c                   if        *inkf or *inkg
020600110127     C                   MOVEL     '06'          lrq2cmdo
020700110117      *  Conferma + stampa
020800110117     c                   If        *inKg  = '1'
020900110127     C                   MOVEL     '07'          lrq2cmdo
021000110117     c                   end
021100110104     c                   leave
021200110104     c                   end
021300080508     C                   ENDdo
021400080508     c*
021500080508     c                   exsr      srfine
021600950317      *---------------------------------------------------------------*
021700950317      * Gestione videata subfile errori                               *
021800950317      *---------------------------------------------------------------*
021900080508     C     INZS01        BEGSR
022000151006     C                   move      '0'           da_forzare        1
022100151006      *
022200080508     C                   SETOFF                                       202190
022300080508     C                   WRITE     LR53C01
022400950317     C                   SETON                                        2021
022500080512     C                   Z-ADD     0             NRR3              4 0
022600101223     c*
022700101223     c                   exsr      srsfl
022800101223     c*
022900080523     c                   endsr
023000080523     c**********************************************************************
023100101223     c* memorizzo spedizione errata nel sfl
023200080523     c**********************************************************************
023300101223     c     srsfl         begsr
023400101223     c* se ho solo un'anomalia a livello di spedizione, la carico,
023500101223     c* altrimenti aggancio il dettaglio
023600110204     c*    *loval        setll     wfnlrq11l
023700101223     c                   do        *hival
023800110204     c                   read      wfnlrq10f
023900110204     c                   if        %eof(wfnlrq10f)
024000101223     c                   leave
024100101223     c                   end
024200101223     c                   if        WLRQ1fgs <> d59fgs or
024300101223     c                             WLRQ1ndc <> d59ndd or
024400101223     c                             WLRQ1pdr <> d59pdr
024500101223     c                   iter
024600101223     c                   end
024700101223     C                   Z-ADD     wlrq1aas      v3caas
024800101223     C                   Z-ADD     wlrq1lnp      v3clnp
024900110104     C                   Z-ADD     wlrq1nrs      v3cnrs
025000110104     C                   Z-ADD     wlrq1nsp      v3cnsp
025100101223     C                   movel(p)  WLRQ1ERR      v3cmsg
025200151006      * Controlla
025300151006     c                   exsr      contro_alert
025400151006     c                   eval       h3Calert = inviato_contro_alert
025500151006      *
025600101223     c                   exsr      srdett
025700080526     C                   ADD       1             NRR3
025800080526     C                   WRITE     LR53S01
025900101223     c                   enddo
026000080526     c                   endsr
026100080508      *---------------------------------------------------------------*
026200101223      *  Loop di errori  per segnacollo                               *
026300080508      *---------------------------------------------------------------*
026400101223     C     srdett        BEGSR
026500110119     c                   clear                   collo
026600110119     c                   clear                   savcollo
026700110126     c                   clear                   colloima
026800110126     c                   clear                   savcolloima
026900110127     c                   clear                   foglio
027000110127     c                   clear                   savfoglio
027100110127     C                   clear                   v3csce
027200110127     C                   clear                   v3hsce
027300110126     c                   clear                   v3cnpg
027400110126     c                   clear                   v3cflv
027500110126     c                   clear                   v3cnfv
027600110126     c                   clear                   v3cdfv
027700110126     c                   clear                   nrcolli           4 0
027800110126     c                   clear                   nrcolliima        4 0
027900110119     c     kq20          chain     fnarb01l
028000110119     c                   if        not %found(fnarb01l)
028100110119     c                   clear                   fnarbds
028200110119     c                   end
028300110117     c* richiamo la funzione utilizzata anche dalla lista sped.da consegnare
028400110117     c* mi controlla la consegnabilità della merce
028500110118     c* non voglio anche la spunta partenza, ma mi serve che i colli siano
028600110118     c* arrivati al terminal, non cerco i legami perchè nel caso dovrei
028700110118     c* avere in distanta già la figlia
028800110118     c                   clear                   fnlr76ds
028900110117     c                   clear                   dssoglia
029000110118     c                   eval      ILR76DCO= oggi
029100110117     c                   eval      ILR76AAS= v3cAAs
029200110117     c                   eval      ILR76LNP= v3cLnp
029300110117     c                   eval      ILR76NRS= v3cNRs
029400110117     c                   eval      ILR76NSP= v3cNSP
029500110117     c                   call      'FNLR76R'
029600110117     c                   parm                    fnlr76ds
029700110119     c                   parm                    fnarbds
029800110117     C                   PARM                    dsSoglia         12 0
029900140130      *
030000110119     c                   eval      v3harbifp = arbifp
030100110119     c                   eval      v3harbndc = arbndc
030200110119     c                   eval      v3harbddc = arbddc
030300110804     c                   eval      v3harbxco = arbxco
030400130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
030500130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
030600130221     c**** arbxco        comp      ' '                                8888
030700130221     c     arbxco        comp      '0'                                88
030800110119     c                   movel     arbrsd        v3crsd
030900110408     c                   movel     arbtsp        v3ctsp
031000110118     c*
031100101223     c     kq20          setll     wfnlrq21l
031200101223     c                   do        *hival
031300101223     c     kq20          reade     wfnlrq21l
031400101223     c                   if        %eof(wfnlrq21l)
031500101223     c                   leave
031600101223     c                   end
031700110126     c*
031800110126     c                   if        WLRQ2fgs <> WLRQ1fgs or
031900110126     c                             WLRQ2ndc <> WLRQ1ndc or
032000110126     c                             WLRQ2pdr <> WLRQ1pdr
032100110126     c                   iter
032200110126     c                   end
032300110126     C*
032400110119     c                   eval      aasc = wlrq2aas
032500110119     c                   eval      lnpc = wlrq2lnp
032600110119     c                   eval      nrsc = wlrq2nrs
032700110119     c                   eval      nspc = wlrq2nsp
032800110119     c                   eval      flsc = wlrq2fls
032900110119     c                   eval      nscc = wlrq2nsc
033000110126     c                   eval      nrcc = wlrq2nrc
033100110126     c                   eval      flvc = wlrq2flv
033200110127     c                   eval      npgc = wlrq2npg
033300110126     c                   eval      nfvc = wlrq2nfv
033400110126     c                   eval      dfvc = wlrq2dfv
033500140130      *
033600110126     c* conto quanti colli hanno l'errore
033700110207     c                   if        collo <> savcollo and nscc <> 0
033800110119     c                   add       1             nrcolli
033900110119     c                   eval      savcollo = collo
034000110119     c                   end
034100140130      *
034200110126     c* conto quanti colli sono in ima
034300110126     c                   if        wlrq2npg = 3 and wlrq2dfv = d59ddt
034400110126     c                             and wlrq2flv = dutpou and
034500110126     c                             colloima <> savcolloima
034600110126     c                   add       1             nrcolliima
034700110126     c                   eval      savcolloima = colloima
034800110126     c                   end
034900140130      *
035000110126     c* memorizzo solo se spunta 3 o 4 esco con data distinta e mio p.o.
035100110126     c                   if        (WLRQ2npg = 3 or
035200110126     c                             WLRQ2npg = 4) and wlrq2dfv = d59ddt
035300110126     c                             and wlrq2flv = dutpou
035400110203     c                   if        savfoglio = 0
035500110127     c                   eval      savfoglio = foglio
035600110127     c                   eval      v3cnpg =WLRQ2npg
035700110117     c                   eval      v3cflv =WLRQ2flv
035800110117     c                   eval      v3cnfv =WLRQ2nfv
035900110117     c                   if        wlrq2dfv <> 0
036000110117     c                   move      WLRQ2dfv      dtaiso
036100110117     c                   move      dtaiso        dtaeur
036200110117     c                   move      dtaeur        v3cdfv
036300110117     c                   else
036400110117     c                   clear                   v3cdfv
036500110117     c                   end
036600110126     c                   end
036700140130      *
036800110127     c* se il foglio che sto leggendo è diverso da quello che ho
036900110127     c* salvato pulisco i campi del sfl
037000110127     c                   if        savfoglio <> 0 and savfoglio <> foglio
037100110128     c                             and foglio <> 0
037200110127     c                   clear                   v3cnpg
037300110127     c                   clear                   v3cflv
037400110127     c                   clear                   v3cnfv
037500110127     c                   clear                   v3cdfv
037600110127     c                   end
037700140130      *
037800110203     c                   end
037900140130      *
038000110126     c                   enddo
038100140130      *
038200110203     c* mi salvo la situazione colli in campi idden sfl
038300110203     c                   clear                   v3ddes
038400110203     c                   eval      v3hncc = OLR76CDACO
038500110203     c                   eval      v3hnce = nrcolli
038600110203     c                   eval      v3hnci = nrcolliima
038700110127     c* mi sono già memorizza un foglio d'usita o IMA nella lettura del
038800110127     c* dettaglio quindi:
038900110804     C*
039000110804     C* prima vcerifica è se ho colli consegnabili
039100110203     c                   if        OLR76CDACO = 0
039200140130      *
039300110804     c                   if        v3harbndc = d59ndd
039400110207     c                   eval      v3hsce = '2'
039500110207     c                   eval      v3csce = '2'
039600110207     c                   end
039700140130      *
039800110203     c                   clear                   v3cnpg
039900110203     c                   clear                   v3cflv
040000110203     c                   clear                   v3cnfv
040100110203     c                   clear                   v3cdfv
040200110203     c                   leavesr
040300110203     c                   end
040400110127     c*
040500110126     c* se i colli in errore non corrispondono ai colli consegnabili
040600110126     c* vuol dire che ho dei colli senza anomalia (spunta 4)
040700110126     c* quindi non posso proporre il foglio ima (opz 2) o il foglio d'uscita
040800110127     c* (opz 3) e pulisco
040900110203     c*
041000110128     c* più colli in errore di quelli consegnabili ????
041100110128     c* per il momento non lo prevedo ...
041200110207     c                   if        nrcolli <  OLR76CDACO and
041300110203     c                             OLR76CDACO <> 0
041400110127     c                   clear                   v3hsce
041500110127     c                   clear                   v3csce
041600110117     c                   clear                   v3cnpg
041700110117     c                   clear                   v3cflv
041800110117     c                   clear                   v3cnfv
041900110117     c                   clear                   v3cdfv
042000110127     c                   leavesr
042100110127     c                   end
042200140130      *
042300110127     c* se arrivo qui vuol dire che tutti i colli consegnabili hanno un
042400110127     c* anomalia quindi o sono tutti in IMA quindi propongo il 2 o sono
042500110127     c* tutti su una distinta quindi propongo 1/3 oppure sono sparsi
042600110127     c* quindi propongo 2
042700110127     c*
042800110203     c* se tutti i colli sono nel foglio IMA  propongo il togli
042900110203     c                   if        (nrcolliima = OLR76CDACO
043000110207     c                             and nrcolliima <> 0) and
043100110207     c                             v3harbndc = d59ndd
043200110131     c                   eval      v3hsce = '2'
043300110131     c                   eval      v3csce = '2'
043400110131     c                   end
043500140130      *
043600110203     c* se non tutti i colli sono nel foglio IMA  pulisco il foglio proposto
043700110126     c                   if        nrcolliima <> OLR76CDACO
043800110126     c                             and nrcolliima <> 0
043900110126     c                   clear                   v3cnpg
044000110126     c                   clear                   v3cflv
044100110126     c                   clear                   v3cnfv
044200110126     c                   clear                   v3cdfv
044300110126     c                   end
044400140130      *
044500110126     c* se ho tutti i colli consegnabili sulla mia distinta ma la spediz.
044600110126     c* non lo è forzo l'opz 1 di inserimento
044700110119     c                   if        v3cnpg = 4 and
044800110119     c                             v3cnfv = d59ndd  and
044900110207     c                             v3cflv = d59fgs  and
045000110207     c                             v3harbndc <> d59ndd
045100110119     c                   eval      v3csce = '1'
045200110119     c                   eval      v3hsce = '1'
045300110117     c                   end
045400140130      *
045500110127     c* se ho tutti i colli consegnabili su un'altra distinta ma la spediz.
045600110127     c* è sulla mia forzo l'opz 3
045700110127     c                   if        v3cnpg = 4 and
045800110127     c                             v3cnfv <>d59ndd  and
045900110207     c                             v3cflv = d59fgs
046000110127     c                   eval      v3csce = '3'
046100110127     c                   eval      v3hsce = '3'
046200110127     c                   end
046300140130      *
046400110119     c* aggancio il foglio per decodificare l'autista o il foglio ima
046500110120     c                   clear                   v3ddes
046600140130      *
046700110119     c                   if        v3cnfv <> 0
046800140130      *
046900110119     c                   select
047000140130      *
047100110119     c                   when      v3cnpg = 3
047200110119     c* se è chiuso non importa ???
047300110119     c                   z-add     v3cnfv        fvvnfv
047400110119     c     kfvv          chain     fnfvv01l
047500110119     c                   if        %found(fnfvv01l)
047600110119     c                   movel(p)  fvvdsf        v3ddes
047700110119     c                   end
047800140130      *
047900110119     c                   when      v3cnpg = 4
048000110119     c* non faccio controlli sulla distinta perchè ci penserà poi il pgm
048100110119     c* fnlr98r
048200110119     c                   z-add     v3cnfv        dstnfv
048300110119     c     kdst          chain     fidst01l
048400110119     c                   if        %found(fidst01l)
048500110119     c     kapd1         chain     fiapd01l
048600110119     c                   if        %found(fiapd01l)
048700110119     c                   movel(p)  apdrsf        v3ddes
048800110119     c                   end
048900110119     c                   end
049000140130      *
049100110119     c                   endsl
049200140130      *
049300110119     c                   end
049400140130      *
049500101223     c                   endsr
049600101223      *---------------------------------------------------------------*
049700101223      *  Loop di errori  subfile errori                               *
049800101223      *---------------------------------------------------------------*
049900101223     C     CTR01         BEGSR
050000110104     c                   setoff                                       284044
050100110804     c                   setoff                                       88
050200950315      *
050300110104     c                   clear                   nr                4 0
050400110104     c                   do        *hival        nr
050500110104     C     nr            chain     LR53S01                            32
050600110104     c                   if        *in32
050700110104     c                   leave
050800110104     c                   end
050900110121     c                   setoff                                       40
051000151006      *--
0511001510063    C     v3csce        IFeq      *blank
051200151006     c                   eval      h3Forza = ' '
0513001510063-   C                   ENDIF
051400151006      *--
051500110104     c* proteggo la scelta se già eseguita
051600110104     c     v3hind        comp      '1'                                    44
051700130221      *** Wurth adesso manda nel campo non solo numeri per identificare altre cose
051800130221      ***  quindi si deve sostituire il test per identificare il Perfect Order
051900130221     c**** v3harbxco     comp      ' '                                8888
052000130221     c     v3harbxco     comp      '0'                                88
052100110104     c* se protetto vado al record successivo
052200110104     c                   if        v3hind = '1'
052300110104     c                   iter
052400110104     c                   end
052500080704     c*
052600080701     c                   select
052700080509      *  Interrogazione bolla
052800110203     C                   when      v3csce = '5'
052900950317     C                   MOVEL     '1'           P36FLG
053000950317     C                   Z-ADD     V3CAAS        P36AAS
053100950317     C                   Z-ADD     V3CLNP        P36LNP
053200950317     C                   Z-ADD     V3CNSP        P36NSP
053300950317     C                   Z-ADD     V3CNRS        P36NRS
053400080509     C                   MOVEL(P)  DSLR36        KPJBU
053500950317     C                   CALL      'FNLR36R'
053600080701     c                   PARM                    KPJBA
053700110117     c                   movel     v3hsce        v3csce
053800101223     c* opzione gestione spunte
053900101223     c                   when      v3csce = 'S'
054000101223     c                   clear                   ds_para04
054100110104     C                   MOVEL     'FNLRQ2R'     P04PGM
054200101223     c                   eval      P04NPS = 99
054300101223     c                   eval      P04NPG = 9
054400110104     c                   eval      P04FLG = '1'
054500110119     c                   eval      P04FGS = d59fgs
054600110104     c                   eval      P04NRS = v3cNRS
054700101223     c                   eval      p04bolp = V3cLNP
054800101223     c                   eval      p04bors = V3cNRS
054900101223     c                   eval      p04bosp = V3cNSP
055000101223     c                   eval      p04boan = V3cAAS
055100101223     c                   eval      kpjbu = ds_para04
055200101223     c                   call      'FNLR04R'
055300101223     c                   parm                    kpjba
055400110117     c                   movel     v3hsce        v3csce
055500101223     c* opzione dettaglio colli
055600101223     c                   when      v3csce = 'D'
055700101223     c                   clear                   fnlrq5ds
055800110104     c                   eval      lrq5aasi = v3caas
055900110104     c                   eval      lrq5lnpi = v3clnp
056000110104     c                   eval      lrq5nrsi = v3cnrs
056100110104     c                   eval      lrq5nspi = v3cnsp
056200110203     c                   eval      lrq5ncci = v3hncc
056300110203     c                   eval      lrq5ncei = v3hnce
056400110203     c                   eval      lrq5ncii = v3hnci
056500101223     c                   eval      kpjbu = fnlr59ds
056600101223     c                   call      'FNLRQ5R'
056700101223     c                   parm                    kpjba
056800101223     c                   parm                    fnlrq5ds
056900110104     c                   if        lrq5msgo <> ' '
057000110104     c                   seton                                        2840
057100110104     C                   eval      $MSG = lrq5msgo
057200110104     c                   leave
057300110104     c                   end
057400110104     c                   if        lrq5cmdo = '03'
057500110104     C                   MOVEL     lrq5cmdo      lrq2cmdo
057600110104     C                   exsr      srfine
057700110104     c                   end
057800110117     c                   movel     v3hsce        v3csce
057900110119     c* se preimpostato inserisci, togli o trasferisci l'unica possibilità
058000110119     c* è abblencare la scelta
058100110119     C                   when      v3csce <> v3hsce and v3hsce <> ' '
058200110119     c                             and v3csce <> ' '
058300101223     c                   seton                                        2840
058400110118     C                   eval      $MSG = 'Opzione non ammessa obbligatoria la -
058500110119     c                             scelta ' + v3hsce
058600110119     c* se non ho preimpostato nulla, l'unica possibilità
058700110119     c* è togliere la spedizione  (opz 2)
058800110119     C                   when      v3csce <> ' ' and v3hsce = ' '
058900110119     c                             and v3csce <> '2'
059000110119     c                   seton                                        2840
059100110119     C                   eval      $MSG = 'Opzione non ammessa è possibile solo-
059200110121     c                              inserire 2=Togli'
059300110103     c* il pgm FNLR98 gira in dftactgrp e committa ad ogni aggiornamento
059400101223     c*
059500101223     C                   when      (v3csce = '1' or v3csce = '2'
059600151006     c                             or v3csce= '3') and (*inkf or *inkg or *inkb)
059700151006      *---
059800151006     C*   attenzione: in assegnazione deve controllare se è stato inviato
059900151006     C*   un contro ALert (di NON CONSEGNA) in tal caso deve Forzare l'immissione
060000151006     C*   in distinta della Spedizione con F2.
060100151006      *
060200151006     c                   IF        V3CSCE = '1' and h3Calert = 'S'
060300151006     c                             and h3Forza =' '
060400151006     C* F2=Forzatura
060500151006     C                   if        *inKB
060600151006     c                   setoff                                       66
060700151006     C                   move      '0'           da_forzare
060800151006     c                   eval      h3Forza = 'F'
060900151006     C*
061000151006      * errore: Occorre forzare la riga se la si vuole proprio selezionare
061100151006      *   si deve forzare con "F2" la riga... deve segnalarlo.
061200151006     c                   else
061300151006     c                   seton                                        662840
061400151006     C                   move      '1'           da_forzare
061500151006     C                   eval      $MSG = MSG_F2
061600151006     c                   end
061700151006     c                   endIf
061800151006      *---
061900151006     c  N66              exsr      sraggio
062000151006     c  N66              clear                   v3csce
062100080728     C                   ENDsl
062200980325      *  Aggiorno subfile
062300080707     C                   UPDATE    LR53s01
062400110127     c   28              leave
062500080509     C                   ENDdo
062600080715     c*
062700080715     c                   z-add     1             nrr3
062800950317     C*
062900950317     C                   ENDSR
063000080707      *---------------------------------------------------------------*
063100080707      * fine
063200080707      *---------------------------------------------------------------*
063300110119     C     sraggio       BEGSR
063400140130      **
063500110119     c                   clear                   fnlr98ds
063600110119     c                   eval      i98aas = v3caas
063700110119     c                   eval      i98lnp = v3clnp
063800110119     c                   eval      i98nrs = v3cnrs
063900110119     c                   eval      i98nsp = v3cnsp
064000110119     c                   eval      I98VIDEO  = 'N'
064100110119     c                   eval      I98npg    = 4
064200110119     c                   eval      I98fgs    = d59fgs
064300110119     c                   eval      I98ndc    = d59ndd
064400140130      **
064500110119     c* se c'è l'opz 1 vuol dire che le spunte d'uscita sono sulla mia
064600110119     c* distinta ma la spedizione può essere libera oppure
064700110124     c* su un'altra distinta, in quest'ultimo caso devo prima toglierla
064800110119     c                   if        v3csce = '1' and v3cnpg = 4 and
064900110119     c                             v3cnfv = d59ndd and v3harbndc <> 0
065000110119     c                             and v3harbndc <> v3cnfv
065100110119     c                   eval      v3csce = '3'
065200110119     c                   eval      I98npg    = 4
065300110119     c                   move      v3harbifp     I98fgs
065400110119     c                   eval      I98ndc    = v3harbndc
065500110119     c                   end
065600110119     c*
065700110119     c                   do        2             x                 2 0
065800110119     c                   if        v3csce = '1' or
065900110119     c                             (v3csce = '3' and x = 2)
066000110119     c                   eval      I98DELINS = 'M'
066100110119     c                   end
066200110119     c                   if        v3csce = '3' and x = 2
066300110119     c                   eval      I98npg    = v3cnpg
066400110119     c                   eval      I98fgs    = v3cflv
066500110119     c                   eval      I98ndc    = v3cnfv
066600110119     c                   end
066700110124     c* se lavoro sulla mia distinta non controllo e non alloco solo
066800110125     c* se metto o tolgo, ma se TRASFERISCO lo devo lasciarlo fare
066900110125     c* al FNLR98C (nche lo STRCMTCTL ) perchè altrimenti
067000110125     c* la spedizione è vincolata dal lavoro
067100110124     c                   if        i98ndc <> d59ndd
067200110119     c                   eval      I98ALCDST = 'S'
067300110119     c                   eval      I98CTLDST = 'S'
067400110124     c                   else
067500110124     c                   eval      I98ALCDST = 'N'
067600110124     c                   eval      I98CTLDST = 'N'
067700110124     c                   end
067800110124     c*
067900141113     c                   clear                   fnlr98ds2
068000141113     c                   if        v3csce = '3' or v3csce = '2'
068100141113     c*check spedizione con disposizioni di dirottamento richiede forzatura se presenti
068200141113     c                   z-add     v3caas        i982aas
068300141113     c                   z-add     v3clnp        i982lnp
068400141113     c                   z-add     v3cnrs        i982nrs
068500141113     c                   z-add     v3cnsp        i982nsp
068600141113     c                   call      'FNLR98R2'
068700141113     c                   parm                    fnlr98ds2
068800141113     c                   endif
068900141113     c*
069000141113     c                   if        o982esito = '1'
069100141113     c                   seton                                        2840
069200141113     C                   eval      $MSG = o982msgerr
069300141113     c                   leave
069400141113     c                   else
069500110124     c                   if        v3csce = '3'
069600110124     c                   eval      I98STRCMT = ' '
069700110124     c                   eval      kpjbu = fnlr98ds
069800110124     c                   call      'FNLR98C'
069900110124     c                   parm                    kpjba
070000110119     c                   else
070100110124     c                   eval      I98STRCMT = 'N'
070200110124     c                   eval      kpjbu = fnlr98ds
070300110124     c                   call      'FNLR98R'
070400110124     c                   parm                    kpjba
070500110119     c                   end
070600141113     c                   endif
070700110124     c                   movel     kpjbu         fnlr98ds
070800110124     C                   if        o98esito = '1'
070900110119     c                   seton                                        2840
071000110119     C                   eval      $MSG = o98msgerr
071100110119     c                   leave
071200110119     c                   else
071300110119     c                   seton                                        44
071400110119     c                   move      *in44         v3hind
071500110119     c                   end
071600140130      **
071700140130      **  Qui deve scrivere il MIC se non è stato ancora scritto dopo il
071800140203      **   Trasferimento o l'immissione
071900140203     c                   if        v3csce = '1' or v3csce = '3'
072000140203     c                   clear                   fnlrq7ds
072100140203     c                   eval      svkpjbu = kpjbu
072200140203      * routine x scrivere il MIC
072300140203     c                   eval      LRQ7CMTI  ='N'
072400140203     c                   eval      LRQ7AASI   = v3caas
072500140203     c                   eval      LRQ7LNPI   = v3clnp
072600140203     c                   eval      LRQ7NRSI   = v3cnrs
072700140203     c                   eval      LRQ7NSPI   = v3cnsp
072800140203     c                   eval      LRQ7FGSI   = v3cflv
072900140203     c                   eval      LRQ7NDCI   = v3cnfv
073000140203     c                   move      v3cdfv        dtaeur
073100140203     c                   move      dtaeur        dtaiso
073200140203     c                   movel     dtaiso        LRQ7DDCI
073300140203     c                   eval      LRQ7PDCI   = v3cpdr
073400140203     c                   eval      LRQ7NPGI   = v3cnpg
073500140204     c                   if        v3csce = '1'
073600140204     c                   eval      LRQ7SP4I   = 'N'
073700140204     c                   elseIf    v3csce = '3'
073800140204     c                   eval      LRQ7SP4I   = 'S'
073900140204     c                   end
074000140203     c                   eval      LRQ7TLAI   = 'L'
074100140203     c                   eval        kpjbu = fnlrq7ds
074200140203     c                   call      'FNLRQ7R'
074300140203     c                   parm                    kpjba
074400140203     c                   eval      fnlrq7ds = kpjbu
074500140203     c                   eval        kpjbu = svkpjbu
074600140203     c                   end
074700140130      **
074800110119     c* solo per il trasferisci deve fare il secondo giro
074900110119     c                   if        v3csce = '1' or v3csce = '2'
075000110119     c                   leave
075100110119     c                   end
075200140130      **
075300110119     c                   enddo
075400140130      **
075500110119     C                   ENDSR
075600110119      *---------------------------------------------------------------*
075700110119      * fine
075800110119      *---------------------------------------------------------------*
075900110119     C     srfine        BEGSR
076000101222     c                   movel(p)  fnlr59ds      kpjbu
076100080526     C*
076200080526     C                   SETON                                        LR
076300080710     c                   return
076400080707     c*
076500080707     C                   ENDSR
076600080509     C*-----------------------------------------------------*
076700080509     C     *INZSR        BEGSR
076800080509     C*-----------------------------------------------------*
076900080508      *
077000080508     C     *ENTRY        PLIST
077100101220     C                   PARM                    kpjba
077200101220     C                   PARM                    fnlrq2ds
077300140130      *
077400101220     c                   movel     kpjbu         fnlr59ds
077500101220     c                   clear                   fnlrq2ds
077600080515     C* Decodifica Elaboratore
077700080515     c     *dtaara       define    §azute        azuteds
077800080515     c     *dtaara       define    §datiute      ddatiute
077900080515     C                   in(E)     *dtaara
078000080515     C                   IF        %Error  or  RSUT = *blanks
078100080515     C                   call      'TIBS34R'
078200080515     C                   parm                    Tibs34Ds
078300080515     C                   in        *dtaara
078400080515     c                   ENDIF
078500110119     c     kfvv          klist
078600110119     c                   kfld                    v3cnpg
078700110119     c                   kfld                    fvvnfv
078800110119     c                   kfld                    v3cflv
078900110119     c     kdst          klist
079000110119     c                   kfld                    v3cnpg
079100110119     c                   kfld                    dstnfv
079200110119     c                   kfld                    v3cflv
079300110119     c     kapd          klist
079400110119     c                   kfld                    apdtip
079500110119     c                   kfld                    d59pdr
079600110119     c     kapd1         klist
079700110119     c                   kfld                    apdtip
079800110119     c                   kfld                    dstpdr
079900101223     c     kq20          klist
080000101223     c                   kfld                    WLRQ1aas
080100101223     c                   kfld                    WLRQ1lnp
080200101223     c                   kfld                    WLRQ1nrs
080300101223     c                   kfld                    WLRQ1nsp
080400151006      *
080500151006     C     Kfiar5        KLIST
080600151006     C                   KFLD                    v3caas
080700151006     C                   KFLD                    v3clnp
080800151006     C                   KFLD                    v3cnrs
080900151006     C                   KFLD                    v3cnsp
081000151006     C                   KFLD                    TRDar5            3
081100151006      *
081200101223     c                   eval      apdtip = 'A'
081300110117     c* gg
081400110117     c                   if        d59ddt = 0
081500110117     c                   move      udate         dtaiso
081600110117     c                   else
081700110117     c                   move      d59ddt        dtaiso
081800110117     c                   end
081900110117     c                   move      dtaiso        oggi
082000080509     C                   ENDSR
082100151006     C*-----------------------------------------------------*
082200151006     C* controllo se mandato un CONTRO Alert al CLIENTE
082300151006     C*-----------------------------------------------------*
082400151006     C     contro_alert  BEGSR
082500151006      *
082600151006      * rec.EMD   invio Alert e contro Alert
082700151006     C                   eval      inviato_contro_alert = 'N'
082800151006     C                   clear                   dar5emd
082900151006     C                   move      'EMD'         TRDar5
083000151006     C     Kfiar5        CHAIN     FiAR501L
083100151006     C                   IF        %Found(FiAR501L)
083200151006     C                   MOVEL     ar5uni        dar5emd
083300151006     c                   if        §AR5ICA ='S'
083400151006     c                   if        §AR5DPC <>*blank and §AR5DPC <>*zeros
083500151006     c                   move      §AR5DPC       data8DPC          8 0
083600151006     c                   if        data8DPC = oggi
083700151006     C                   eval      inviato_contro_alert = 'S'
083800151006     C                   end
083900151006     C                   end
084000151006     C                   end
084100151006     C                   endif
084200151006      *
084300151006     C                   ENDSR
084400151006      * ?-------------------------------------------------------------*?
