000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200101220     h dftactgrp(*no) actgrp(*caller)
000300140113      *--------------------------------------------------------------*
000400140113      *                                                              *
000500140113      *               ========  --------------------  =======        *
000600140113      *    E'CHIAMATA  SEMPRE   DOPO AVER AGGIORNATO   A R B         *
000700140113      *               ========  --------------------  =======        *
000800140113      *                                                              *
000900140113      *      x SCRIVERE   E V B  --> (MIC)                           *
001000140113      *                                                              *
001100140113      *--------------------------------------------------------------*
001200140109      *   - Utility x scrivere il MIC verificando se NON presente    *
001300140109      *   - e se vi è la spunta (4) (solo su richiesta)              *
001400140113      * ================                                             *
001500140113      * GESTIONE COMMIT                                              *
001600140113      * ================                                             *
001700140113      *  Deve essere attivata se nella cascata di altri programmi    *
001800140113      *  è gestito il COMMIT mentre altri potrebbero NON gestirlo.   *
001900950315      *--------------------------------------------------------------*
002000140113     FFNarb01L  iF   E           K DISK
002100140110     FFNlbl01L  IF   E           K DISK
002200140203     FFiDst01L  IF   E           K DISK
002300140110     FFNbrv01L  IF   E           K DISK    usropn
002400140110     FFNart01L  IF   E           K DISK    usropn
002500140110      *--------------------------------------------------------------*
002600140110      * x non allocare legge col logico e aggiorna/scrive col Fisico
002700140110     FFNevb04L  IF   E           K DISK    RENAME(FNEVB000:FNEVB004)
002800140110     F                                     infds(evbds)
002900140113     FFNevb00F  UF A E             DISK    commit(ON) USROPN
003000950320      *----------------------------------------------------------------*
003100140110     D                SDS
003200140110     D  NOMPGM                 1     10
003300140113      *----------------------------------------------------------------*
003400140113     D ON              s              1
003500140110      *----------------------------------------------------------------*
003600140110     d evbds           ds
003700140110     d  evb_nrr              397    400b 0
003800140110      *--------------------------------------------------------------*
003900140203     d dsFIDST       e ds                  extname(FiDst00f)
004000140203     d dEVB01        e ds
004100140113     d dEVB01new       s                   like(dEVB01)
004200140110      *
004300950315     D KPJBA         E DS
004400140110     D fnLRQ7ds      E DS
004500140110      *
004600080515     D ddatiute      e ds
004700080515     D azuteds       e ds                  extname(AZUTE00F)
004800080515     D tibs34ds      E DS                  inz
004900140109      *
005000140110     D BOLLA_di_RESO...
005100140110     D                 s              1
005200140110      **
005300140110     d  KAAS           s                   like(EVBAAS)
005400140110     d  KLNP           s                   like(EVBLNP)
005500140110     d  KNRS           s                   like(EVBNRS)
005600140110     d  KNSP           s                   like(EVBNSP)
005700140110     d  KCEV           s                   like(EVBCEV)
005800140110      **
005900110117     D dtaiso          s               d
006000110117     D dtaeur          s               d   datfmt(*eur)
006100140110      *
006200140110     D WLBDA8          DS
006300140110     D  G02DAT                 1      8  0
006400140110     D  G02INV                 9     16  0
006500140110     D  G02ERR                17     17
006600140110     D  G02TGI                18     22  0
006700140110      *
006800950315      *---------------------------------------------------------------*
006900950315      * FLUSSO PRINCIPALE                                             *
007000950315      *---------------------------------------------------------------*
007100140110     C     *ENTRY        PLIST
007200140110     C                   PARM                    kpjba
007300140110     c                   eval      fnLRQ7ds =  kpjbu
007400140203     C*
007500140203     C* Esito ed eventuale msg di errore
007600140203     c                   eval      LRQ7ESIO =  '0'
007700140203     c                   eval      LRQ7MSGO =  *blank
007800140113     C*
007900140113     C* SE RICHIESTA la gestione sotto COMMIT
008000140113     c                   eval      ON = '0'
008100140113     c                   if        LRQ7CMTI = 'S'
008200140113     c                   eval      ON = '1'
008300140113     c                   end
008400140203     C*
008500140113     c                   open      fnEVB00F                             99
008600140110     C*
008700140110     C*   Se richiesto il controllo sulla spunta (4)
008800140110     C*    apro i relativi file da utilizzare
008900140110     c                   if        LRQ7sp4I = 'S'
009000140110     c                   open      FNbrv01L                             99
009100140110     c                   open      FNart01L                             99
009200140110     c                   end
009300140203      *
009400140203     C* Comunque pulisce i campi prima della CHAIN
009500140203     c                   clear                   DsFIDST
009600140203     c                   move      LRQ7NPGI      DSTNPG
009700140203     c                   move      LRQ7NDCI      DSTNFV
009800140203     c                   move      LRQ7FGSI      DSTFGS
009900140203     C*
010000140203     C* e li Decodifica sempre agganciando la Distinta
010100140203      * Qualora fossero vuoti i campi sono inizializzati dalla clear
010200140203     c     kDST          chain     FiDst01L
010300140203     C*
010400140203     C*                                       SE NON TROVASSE DST!!!!!!!!
010500140203     c                   if        not %Found(FiDst01L)
010600140203     c                   eval      LRQ7ESIO =  '9'
010700140203     c                   eval      LRQ7MSGO =  'Nr.Distinta NON trovata'
010800140203     c                   else
010900140110     C*
011000140110     C* aggancia la bolla in arrivo
011100140110     c     kARB          chain     FNarb01l
011200140113     C*                                       SE NON TROVASSE ARB!!!!!!!!
011300140113     c                   if        not %Found(Fnarb01L)
011400140113     c                   eval      LRQ7ESIO =  '9'
011500140113     c                   eval      LRQ7MSGO =  'Spedizione NON trovata in ARB'
011600140113     c                   else
011700140203      *
011800140203     C* Scrivo evento di messa in consegna se non c'è o solo se il nr.distinta
011900140203     C* sulla bolla è diverso da quello impostato sul MIC precedente
012000140203     c                   clear                   Devb01
012100140203     c                   clear                   dEVB01new
012200140203     C                   move      *zeros        §NOTNDC
012300140203     C                   move      LRQ7ndcI      §NOTNDC
012400140203     C                   move      *zeros        §NOTPDC
012500140203     C                   move      LRQ7pdcI      §NOTPDC
012600140203     c                   eval      dEVB01new = dEVB01
012700140203     C                   eval      BOLLA_di_RESO = ' '
012800140203     C*
012900140203     C* Se la distinta passata NON corrisponde alla distinta su ARB !!!!!!
013000140203      **        ERRORE
013100140203      ** Il parametro distinta passato deve essere identico a quello sul ARBNDC
013200140203     C     LRQ7ndcI      IFNE      ARBNDC
013300140203      **        ERRORE
013400140203     c                   eval      LRQ7ESIO =  '9'
013500140203     c                   eval      LRQ7MSGO =  'Distinta Spediz.incongruente'
013600140203     C*
013700140203     c                   ELSE
013800140203     C*
013900140203     C*  esegue il cuore del programma
014000140203     C                   exsr      PROCEDI
014100140203     C*
014200140110     C*
014300140110     C                   END                                                    ARBNDC<>
014400140109     C*
014500140113     c                   end
014600140110     C*
014700140203     c                   end
014800140203     C*
014900140113     c                   exsr      srfine
015000140109     C*-----------------------------------------------------*
015100140109     C* verifica esistenza spunte per scrittura MIC         *
015200140109     C*-----------------------------------------------------*
015300140203     C     PROCEDI       BEGSR
015400140203      *
015500140203     C* Controllo se ho già scritto evento MIC
015600140203     C     KARB1         CHAIN     FNLBL01L
015700140203      *
015800140203     C                   IF        %Found(FNLBL01L) and ARBlna = LBLlap
015900140203     C*
016000140203     C* Controlla se La bolla precedente è la MADRE di una bolla di RESO -> 87-ON
016100140203     C     KLBL          CHAIN     FNARB01L
016200140203     c                   IF        %Found(FNarb01L) and
016300140203     c                               (ArbCCA ='2' or ArbCCA ='6')
016400140203     C                   eval      BOLLA_di_RESO = 'S'
016500140203     C                   END
016600140203      *
016700140203     C     KARB2         CHAIN     FNARB01L
016800140203     C                   END
016900140203      *
017000140203      *  se le LINEE di ARRIVO coincidono e non si tratta di reso
017100140203     C                   IF        BOLLA_di_RESO <>'S' and arbLNA = lblLAP
017200140203     C                   Z-ADD     LBLAAP        KAAS
017300140203     C                   Z-ADD     LBLLPP        KLNP
017400140203     C                   Z-ADD     LBLNRP        KNRS
017500140203     C                   Z-ADD     LBLNSP        KNSP
017600140203     C                   ELSE
017700140203     C                   Z-ADD     ARBAAS        KAAS
017800140203     C                   Z-ADD     ARBLNP        KLNP
017900140203     C                   Z-ADD     ARBNRS        KNRS
018000140203     C                   Z-ADD     ARBNSP        KNSP
018100140203     C                   END
018200140203      *
018300140203      * verifica se deve scrivere il mic se esiste almeno una spunta lo scrive
018400140203     c                   if        LRQ7sp4I = 'S'
018500140203     c                   exsr      check_spunte
018600140203     c                   else
018700140203     C*  Imposta a OK come se avesse fatto la routine
018800140203     c                   eval      spunta4_OK = 'S'
018900140203     c                   end
019000140203      *
019100140203      *  Controlla sempre se c'è stato un ultimo MIC sulla stessa distinta
019200140203      *   e non fare nulla. altrimenti deve scrivere il MIC
019300140203     c                   if        spunta4_OK = 'S'
019400140203     c                   exsr      check_MIC
019500140203     c                   end
019600140203      *
019700140203     c                   endSR
019800140203     C*-----------------------------------------------------*
019900140203     C* verifica esistenza spunte per scrittura MIC         *
020000140203     C*-----------------------------------------------------*
020100140203     C     check_spunte  BEGSR
020200140203      *
020300140110      * Inizializza il test
020400140110     c                   clear                   spunta4_OK        1
020500140110      *
020600140110     C* x routine del ctrl delle Spunte imposta i campi chiave
020700140110     C                   z-add     LRQ7npgI      brvNPG
020800140110     C                   z-add     LRQ7ndcI      brvNFV
020900140110     C                   z-add     LRQ7fgsI      brvFGS
021000140109     c     kart          setll     fnart01l
021100140110      *
021200140110      *  trova i singoli colli
021300140109     c                   do        *hival
021400140110     c     kart          reade     fnart01l
021500140110     c                   if        %EoF(fnart01l)
021600140110     c                   leave
021700140110     c                   end
021800140110      *
021900140110      *  controlla le spunte
022000140109     c     kbrv          chain     fnbrv01l
022100140109     c                   if        %found(fnbrv01l)
022200140110     c                   move      'S'           spunta4_OK
022300140109     c                   leave
022400140109     c                   endif
022500140110      *
022600140109     c                   enddo
022700140110      *
022800140109     c                   endsr
022900140110     C*-----------------------------------------------------*
023000140110     C* verifica presenza di UN MIC fra gli eventi          *
023100140110     C*-----------------------------------------------------*
023200140110     C     check_MIC     BEGSR
023300140110      *
023400140110     C                   movel     'MIC'         KCEV
023500140110     C     KEVB          SETGT     FNEVB04L
023600140110     C     KEVB          READPE    FNEVB04L                               33
023700140110      *
023800140110     C* Se l'evento di messa in consegna non esiste o esista ma
023900140110     C* ha data diversa dalla distinta di consegna scrivo un
024000140110     C* nuovo evento di messa in consegna
024100140203     C                   IF        %Eof(fnEVB04l) or evbDEV <> dstDFV
024200140110     C                   CLEAR                   FNEVB000
024300140110     C                   MOVEL     'MIC'         EVBCEV
024400140110     C                   Z-ADD     DATEU         EVBDTV
024500140110     C                   TIME                    EVBORV
024600140110     C                   Z-ADD     LRQ7fgsI      EVBFLE
024700140110     C                   Z-ADD     DUTCOU        EVBCDU
024800140110     C                   Z-ADD     KAAS          EVBAAS
024900140110     C                   Z-ADD     KLNP          EVBLNP
025000140110     C                   Z-ADD     KNRS          EVBNRS
025100140110     C                   Z-ADD     KNSP          EVBNSP
025200140203     C                   Z-ADD     dstDFV        EVBDEV
025300140203     c                   if          dstFPP = 'M'
025400140110     C                   MOVEL     0800          EVBHEV
025500140110     C                   ELSE
025600140110     C                   MOVEL     1200          EVBHEV
025700140110     C                   END
025800140113     C                   MOVEL     dEVB01new     EVBNOT
025900140110     C                   MOVEL     *BLANKS       EVBDTC
026000140110     C                   WRITE     FNEVB000
026100140110      **
026200140110     C                   ELSE                                                   No Trv Mic
026300140110      **
026400140110     C* Se l'evento c'è e ha data uguale alla distinta controllo
026500140110     C* se è già stato trasmesso
026600140110     C     evbDTR        IFEQ      0                                            MIC trasm
026700140110      **
026800140110     C     evb_nrr       chain     FNEVB00F
026900140110     c                   If        %Found(FNEVB00F)
027000140110      **
027100140110     C* Se non è stato trasmesso aggiorno la data e l'ora di
027200140110     C* di immissione evento con quella del giorno + altri dati
027300140110     C                   Z-ADD     DATEU         EVBDTV
027400140110     C                   TIME                    EVBORV
027500140110     C                   Z-ADD     LRQ7fgsI      EVBFLE
027600140110     C                   Z-ADD     DUTCOU        EVBCDU
027700140113     C                   MOVEL     dEVB01new     EVBNOT
027800140203     c                   if          DSTFPP = 'M'
027900140110     C                   MOVEL     0800          EVBHEV
028000140110     C                   ELSE
028100140110     C                   MOVEL     1200          EVBHEV
028200140110     C                   END
028300140110     C                   MOVEL     *BLANKS       EVBDTC
028400140110     C                   UPDATE    FNEVB000
028500140110     c                   end
028600140110      **
028700140110     C* Altrimenti scrivo un nuovo evento di messa in consegna
028800140110     C                   ELSE                                                   No Trasm
028900140113      **
029000140113      **    Nel campo NOTE che distinta c'è ?
029100140113     c                   eval      dEVB01 = evbNOT
029200140110      **
029300140113      **  SOLO se la DISTINTA è diversa da quella PASSATA
029400140113      **    altrimenti
029500140113      **  si scriverebbe 2 volte lo stesso MIC.
029600140113     c                   move      LRQ7ndcI      DISTINTA_7A       7
029700140113      **
029800140113     C                   IF        DISTINTA_7A <> §NOTNDC
029900140113      **
030000140110     C                   CLEAR                   FNEVB000
030100140110     C                   MOVEL     'MIC'         EVBCEV
030200140110     C                   Z-ADD     DATEU         EVBDTV
030300140110     C                   TIME                    EVBORV
030400140110     C                   Z-ADD     LRQ7fgsI      EVBFLE
030500140110     C                   Z-ADD     DUTCOU        EVBCDU
030600140110     C                   Z-ADD     KAAS          EVBAAS
030700140110     C                   Z-ADD     KLNP          EVBLNP
030800140110     C                   Z-ADD     KNRS          EVBNRS
030900140110     C                   Z-ADD     KNSP          EVBNSP
031000140203     C                   Z-ADD     DSTDFV        EVBDEV
031100140203     c                   if          DSTFPP = 'M'
031200140110     C                   MOVEL     0800          EVBHEV
031300140110     C                   ELSE
031400140110     C                   MOVEL     1200          EVBHEV
031500140110     C                   END
031600140113     C                   MOVEL     dEVB01new     EVBNOT
031700140110     C                   MOVEL     *BLANKS       EVBDTC
031800140110     C                   WRITE     FNEVB000
031900140113     C                   END                                                    Distinta <>
032000140110      **
032100140110     C                   END                                                    Mic Trasm
032200140110     C*
032300140110     C                   END                                                    Trv MIC
032400140110      *
032500140110     c                   endsr
032600140110      *---------------------------------------------------------------*
032700140110      * fine
032800140110      *---------------------------------------------------------------*
032900140110     C     srfine        BEGSR
033000140110     C*
033100140110     C*
033200140110     C* C* TIPO LANCIO, TLA "C" -           CHIUSO CON LR
033300140110     C* C* TIPO LANCIO, TLA "L" - ELABORO E CHIUDO CON LR
033400140110     C* C* TIPO LANCIO, TLA " " - ELABORO E CHIUDO IN RETRN
033500140110     c                   movel(p)  fnLRQ7ds      kpjbu
033600140110     C*
033700140110     c                   IF        LRQ7tlaI = 'R'
033800140110     C                   SETON                                        RT
033900140110     c                   else
034000140113      * li chiude sempre (anche se non erano stati aperti - tanto c'è l'ind.errore)
034100140113     c                   close     FNbrv01L                             99
034200140113     c                   close     FNart01L                             99
034300140113     c                   close     FNevb00F                             99
034400140110     C                   SETON                                        LR
034500140110     c                   end
034600140110     c                   return
034700140110     c*
034800140110     C                   ENDSR
034900140110     C*-----------------------------------------------------*
035000140110     C     *INZSR        BEGSR
035100140110     C*-----------------------------------------------------*
035200140110     C*
035300140110     C* Inverto la data del giorno
035400140110     C                   TIME                    WHHDAT           14 0
035500140110     C                   MOVE      WHHDAT        G02DAT
035600140110     C                   MOVE      *BLANKS       G02ERR
035700140110     C                   CALL      'XSRDA8'
035800140110     C                   PARM                    WLBDA8
035900140110     C                   Z-ADD     G02INV        DATEU             8 0
036000140110     C*
036100140110     C* Decodifica Elaboratore
036200140110     c     *dtaara       define    §azute        azuteds
036300140110     c     *dtaara       define    §datiute      ddatiute
036400140110     C                   in(E)     *dtaara
036500140110     C                   IF        %Error  or  RSUT = *blanks
036600140110     C                   call      'TIBS34R'
036700140110     C                   parm                    Tibs34Ds
036800140110     C                   in        *dtaara
036900140110     c                   ENDIF
037000140110     C*-----------------------------------------------------*
037100140110     C     KLBL          KLIST
037200140110     C                   KFLD                    LBLAAP
037300140110     C                   KFLD                    LBLLPP
037400140110     C                   KFLD                    LBLNRP
037500140110     C                   KFLD                    LBLNSP
037600140110      *
037700140110     c     kARB          klist
037800140110     c                   kfld                    LRQ7aasI
037900140110     c                   kfld                    LRQ7lnpI
038000140110     c                   kfld                    LRQ7nrsI
038100140110     c                   kfld                    LRQ7nspI
038200140110      *
038300140110     C     KARB1         KLIST
038400140110     C                   KFLD                    ARBAAS
038500140110     C                   KFLD                    ARBLNP
038600140110     C                   KFLD                    ARBNRS
038700140110     C                   KFLD                    ARBNSP
038800140110      *
038900140110     C     KARB2         KLIST
039000140110     C                   KFLD                    LBLAAN
039100140110     C                   KFLD                    LBLLPN
039200140110     C                   KFLD                    LBLNRN
039300140110     C                   KFLD                    LBLNSN
039400140110      *
039500140110     C     Kart          KLIST
039600140110     C                   KFLD                    KAAS
039700140110     C                   KFLD                    KLNP
039800140110     C                   KFLD                    KNRS
039900140110     c                   KFLD                    KNSP
040000140110      *
040100140130     c     kDST          klist
040200140130     c                   kfld                    DSTNPG
040300140130     c                   kfld                    DSTNFV
040400140130     c                   kfld                    DSTFGS
040500140130      *
040600140110     C     Kbrv          KLIST
040700140110     C                   KFLD                    brvNPG
040800140110     C                   KFLD                    brvNFV
040900140110     C                   KFLD                    brvFGS
041000140110     c                   KFLD                    artLNP
041100140110     c                   KFLD                    artLNA
041200140110     c                   KFLD                    artNRS
041300140110     c                   kfld                    artNSC
041400140110      *
041500140110     C     KEVB          KLIST
041600140110     C                   KFLD                    KAAS
041700140110     C                   KFLD                    KLNP
041800140110     C                   KFLD                    KNRS
041900140110     C                   KFLD                    KNSP
042000140110     C                   KFLD                    KCEV
042100140110      *
042200140110     C                   ENDSR
042300140110     C*-----------------------------------------------------*
