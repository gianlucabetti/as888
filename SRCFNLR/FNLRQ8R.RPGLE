000100950315     H DECEDIT('0,') DATEDIT(*DMY.)
000200101220     h dftactgrp(*no) actgrp(*caller)
000300140113      *--------------------------------------------------------------*
000400140113      *                                                              *
000500140707      *      x SCRIVERE   E V B  --> (NIC)  o eliminare (MIC)        *
000600140707      *                                    non ancora trasmesso      *
000700140707      *                                                              *
000800140113      *--------------------------------------------------------------*
000900140707      *   - Utility x scrivere il NIC se presente un MIC già trasmesso
001000140707      *       se non fosse trasmesso elimina il MIC da trasmettere.  *
001100140707      *   - il Controllo presenza del MIC è fatto su richiesta.      *
001200140109      *   - e se vi è la spunta (4) (solo su richiesta)              *
001300140113      * ================                                             *
001400140113      * GESTIONE COMMIT                                              *
001500140113      * ================                                             *
001600140113      *  Deve essere attivata se nella cascata di altri programmi    *
001700140113      *  è gestito il COMMIT mentre altri potrebbero NON gestirlo.   *
001800950315      *--------------------------------------------------------------*
001900140113     FFNarb01L  iF   E           K DISK
002000140110     FFNlbl01L  IF   E           K DISK
002100140110      *--------------------------------------------------------------*
002200140110      * x non allocare legge col logico e aggiorna/scrive col Fisico
002300140110     FFNevb04L  IF   E           K DISK    RENAME(FNEVB000:FNEVB004)
002400140110     F                                     infds(evbds)
002500140113     FFNevb00F  UF A E             DISK    commit(ON) USROPN
002600950320      *----------------------------------------------------------------*
002700140110     D                SDS
002800140110     D  NOMPGM                 1     10
002900140113      *----------------------------------------------------------------*
003000140113     D ON              s              1
003100140110      *----------------------------------------------------------------*
003200140110     d evbds           ds
003300140110     d  evb_nrr              397    400b 0
003400140110      *--------------------------------------------------------------*
003500140203     d dEVB01        e ds
003600140113     d dEVB01new       s                   like(dEVB01)
003700140110      *
003800950315     D KPJBA         E DS
003900140707     D fnLRQ8ds      E DS
004000140110      *
004100080515     D ddatiute      e ds
004200080515     D azuteds       e ds                  extname(AZUTE00F)
004300080515     D tibs34ds      E DS                  inz
004400140109      *
004500140110     D BOLLA_di_RESO...
004600140110     D                 s              1
004700140110      **
004800140110     d  KAAS           s                   like(EVBAAS)
004900140110     d  KLNP           s                   like(EVBLNP)
005000140110     d  KNRS           s                   like(EVBNRS)
005100140110     d  KNSP           s                   like(EVBNSP)
005200140110     d  KCEV           s                   like(EVBCEV)
005300140110      **
005400110117     D dtaiso          s               d
005500110117     D dtaeur          s               d   datfmt(*eur)
005600140110      *
005700140110     D WLBDA8          DS
005800140110     D  G02DAT                 1      8  0
005900140110     D  G02INV                 9     16  0
006000140110     D  G02ERR                17     17
006100140110     D  G02TGI                18     22  0
006200140110      *
006300950315      *---------------------------------------------------------------*
006400950315      * FLUSSO PRINCIPALE                                             *
006500950315      *---------------------------------------------------------------*
006600140110     C     *ENTRY        PLIST
006700140110     C                   PARM                    kpjba
006800140707     c                   eval      fnLRQ8ds =  kpjbu
006900140203     C*
007000140203     C* Esito ed eventuale msg di errore
007100140707     c                   eval      LRQ8ESIO =  '0'
007200140707     c                   eval      LRq8MSGO =  *blank
007300140710     C*
007400140710      **  SOLO se la DISTINTA è uguale a quella PASSATA
007500140710     c                   move      LRq8ndcI      DISTINTA_7A       7
007600140113     C*
007700140113     C* SE RICHIESTA la gestione sotto COMMIT
007800140113     c                   eval      ON = '0'
007900140707     c                   if        LRq8CMTI = 'S'
008000140113     c                   eval      ON = '1'
008100140113     c                   end
008200140113     c                   open      fnEVB00F                             99
008300140110     C*
008400140110     C* aggancia la bolla in arrivo
008500140711     c     Bolla_INP     chain     FNarb01l
008600140113     C*                                       SE NON TROVASSE ARB!!!!!!!!
008700140113     c                   if        not %Found(Fnarb01L)
008800140707     c                   eval      LRq8ESIO =  '9'
008900140710     c                   eval      LRq8MSGO =  'Spedizione NON trovata su ARB'
009000140113     c                   else
009100140710      *
009200140710      ** Il parametro distinta ricevuto è identico a quello sul ARBNDC --> ERRORE
009300140710     C     LRq8ndcI      IFEQ      ARBNDC
009400140203      **        ERRORE
009500140707     c                   eval      LRq8ESIO =  '9'
009600140710     c                   eval      LRq8MSGO = 'Distinta ancora presente su ARB!'
009700140203     C*
009800140203     c                   ELSE
009900140710     C*
010000140203     C*  esegue il cuore del programma
010100140203     C                   exsr      PROCEDI
010200140110     C*
010300140110     C                   END                                                    ARBNDC<>
010400140109     C*
010500140203     c                   end
010600140203     C*
010700140113     c                   exsr      srfine
010800140109     C*-----------------------------------------------------*
010900140109     C* verifica esistenza spunte per scrittura MIC         *
011000140109     C*-----------------------------------------------------*
011100140203     C     PROCEDI       BEGSR
011200140710     C*
011300140710     C* Imposta il campo NOTA da scrivere o aggiornare sugli eventi in base
011400140710     C*  ai parametri passati.
011500140710     c                   clear                   Devb01
011600140710     c                   clear                   dEVB01new
011700140710     C                   move      *zeros        §NOTNDC
011800140710     C                   move      LRq8ndcI      §NOTNDC
011900140710     C                   move      *zeros        §NOTPDC
012000140710     C                   move      LRq8pdcI      §NOTPDC
012100140710     c                   eval      dEVB01new = dEVB01
012200140710     C                   eval      BOLLA_di_RESO = ' '
012300140203      *
012400140203     C* Controllo se ho già scritto evento MIC
012500140710     C     Bolla_ARB     CHAIN     FNLBL01L
012600140203      *
012700140203     C                   IF        %Found(FNLBL01L) and ARBlna = LBLlap
012800140203     C*
012900140711     C* Controlla se La bolla precedente è la MADRE di una bolla di RESO
013000140711     C     Bolla_Prec    CHAIN     FNARB01L
013100140203     c                   IF        %Found(FNarb01L) and
013200140203     c                               (ArbCCA ='2' or ArbCCA ='6')
013300140203     C                   eval      BOLLA_di_RESO = 'S'
013400140203     C                   END
013500140203      *
013600140711      * Reimposta la bolla con cui è entrato
013700140711     C     Bolla_segue   CHAIN     FNARB01L
013800140203     C                   END
013900140203      *
014000140203      *  se le LINEE di ARRIVO coincidono e non si tratta di reso
014100140203     C                   IF        BOLLA_di_RESO <>'S' and arbLNA = lblLAP
014200140203     C                   Z-ADD     LBLAAP        KAAS
014300140203     C                   Z-ADD     LBLLPP        KLNP
014400140203     C                   Z-ADD     LBLNRP        KNRS
014500140203     C                   Z-ADD     LBLNSP        KNSP
014600140203     C                   ELSE
014700140203     C                   Z-ADD     ARBAAS        KAAS
014800140203     C                   Z-ADD     ARBLNP        KLNP
014900140203     C                   Z-ADD     ARBNRS        KNRS
015000140203     C                   Z-ADD     ARBNSP        KNSP
015100140203     C                   END
015200140203      *
015300140203      *  Controlla sempre se c'è stato un ultimo MIC sulla stessa distinta
015400140203      *   e non fare nulla. altrimenti deve scrivere il MIC
015500140203     c                   exsr      check_MIC
015600140203      *
015700140203     c                   endSR
015800140110     C*-----------------------------------------------------*
015900140110     C* verifica presenza di UN MIC fra gli eventi          *
016000140110     C*-----------------------------------------------------*
016100140110     C     check_MIC     BEGSR
016200140110      *
016300140110     C                   movel     'MIC'         KCEV
016400140710     C     Evb_Bolla     SETGT     FNEVB04L
016500140710     C     Evb_Bolla     READPE    FNEVB04L
016600140710      *
016700140710      **    Nel campo NOTE che distinta c'è ?
016800140710     C                   IF        not %Eof(fnEVB04l)
016900140710     c                   eval      dEVB01 = evbNOT
017000140710     c                   end
017100140110      *
017200140710     C* Se l'evento di messa in consegna esiste ed è sulla distinta
0173001407111    C                   IF        not %Eof(fnEVB04l)
017400140710     c                               and
017500140710     C                             evbDEV = LRQ8DDCI
017600140710     c                               and
017700140710     C                             DISTINTA_7A = §NOTNDC
017800140110      **
017900140711     C* se NON è già stato trasmesso
018000140711     C*     lo elimino
0181001407112    C                   IF        evbDTR = 0                                   MIC trasm
018200140110      **
018300140110     C     evb_nrr       chain     FNEVB00F
018400140110     c                   If        %Found(FNEVB00F)
018500140710     C                   DELETE    FNEVB000
018600140110     c                   end
018700140110      **
018800140710     C* Altrimenti scrivo un Evento NIC di TOLTO dalla consegna
0189001407112x   C                   ELSEif    evbDTR > 0                                   No Trasm
019000140710     c                               and
019100140710     C                             DISTINTA_7A = §NOTNDC
019200140710      **
019300140710      **  -------------------------------------------------
019400140710     C                   MOVEL     EVBDEV        WDTMIC           12 0
019500140710     C                   MOVE      EVBHEV        WDTMIC
019600140710      *
019700140710     C* se l'evento di messa in consegna è stato trasmesso
019800140710     C* controllo se esiste già un evento NIC tolto dalla
019900140710     C* consegna
020000140710     C                   MOVEL     'NIC'         KCEV
020100140710     C     Evb_Bolla     SETGT     FNEVB04L
020200140710     C     Evb_Bolla     READPE    FNEVB04L
020300140710      *
020400140710     C* SE HO TROVATO EVENTO NIC
0205001407113    C                   IF        %Found(fnevb04L) and
020600140710     C                             EVBdev = LRQ8DDCI
020700140710     c                               and
020800140710     C                             DISTINTA_7A = §NOTNDC
020900140710      *
021000140710     C* Se non è stato trasmesso aggiorno la data e l'ora di
021100140710     C* di immissione evento con quella del giorno + altri dati
0212001407114    C                   IF          EVBDTR = 0
021300140714      ***
021400140714     C     evb_nrr       chain     FNEVB00F
0215001407143    C                   IF        %Found(fnevb00F)
021600140714      *
021700140710     C                   Z-ADD     DATEU         EVBDTV
021800140710     C                   TIME                    EVBORV
021900140710     C                   Z-ADD     LRq8fgsI      EVBFLE
022000140710     C                   Z-ADD     DUTCOU        EVBCDU
022100140710     C     LRQ8FPPI      IFEQ      'M'
022200140710     C                   MOVEL     0800          EVBHEV
022300140710     C                   ELSE
022400140710     C                   MOVEL     1200          EVBHEV
022500140710     C                   END
022600140710     C                   MOVEL     *BLANKS       EVBDTC
022700140710      *
022800140710     C                   UPDATE    FNEVB000
022900140714     c                   end
023000140710      *  >>>>>>
023100140710     C* Se è stato trasmesso controllo se non ha data più
023200140710     C* alta del MIC
0233001407114x   C                   ELSE
023400140710      *
023500140710     C                   MOVEL     EVBDEV        WDTNIC           12 0
023600140710     C                   MOVE      EVBHEV        WDTNIC
023700140710      *
0238001407115    C     WDTNIC        IFLE      WDTMIC
023900140710      *
024000140710     C* Se la data non è più alta scrivo un nuovo NIC
024100140710     C                   CLEAR                   FNEVB000
024200140710     C                   MOVEL     'NIC'         EVBCEV
024300140710     C                   Z-ADD     DATEU         EVBDTV
024400140710     C                   TIME                    EVBORV
024500140710     C                   Z-ADD     LRq8fgsI      EVBFLE
024600140710     C                   Z-ADD     DUTCOU        EVBCDU
024700140710     C                   Z-ADD     KAAS          EVBAAS
024800140710     C                   Z-ADD     KLNP          EVBLNP
024900140710     C                   Z-ADD     KNRS          EVBNRS
025000140710     C                   Z-ADD     KNSP          EVBNSP
025100140710     C                   Z-ADD     LRQ8DDCI      EVBDEV
025200140710     C     LRQ8FPPI      IFEQ      'M'
025300140710     C                   MOVEL     0800          EVBHEV
025400140710     C                   ELSE
025500140710     C                   MOVEL     1200          EVBHEV
025600140710     C                   END
025700140710     C                   MOVEL(p)  Devb01new     EVBNOT
025800140710     C                   MOVEL     *BLANKS       EVBDTC
025900140710      *
026000140710     C                   WRITE     FNEVB000
026100140710      *
0262001407115e   C                   END
026300140710     C*
0264001407114e   C                   END
026500140710     C*
026600140710     C* SE NON ESISTE EVENTO NIC avente la stessa data evento della
026700140710     C* distinta lo scrivo
0268001407113x   C                   ELSE
026900140710     C*
027000140710     C                   CLEAR                   FNEVB000
027100140710     C                   MOVEL     'NIC'         EVBCEV
027200140710     C                   Z-ADD     DATEU         EVBDTV
027300140710     C                   TIME                    EVBORV
027400140710     C                   Z-ADD     LRq8fgsI      EVBFLE
027500140710     C                   Z-ADD     DUTCOU        EVBCDU
027600140710     C                   Z-ADD     KAAS          EVBAAS
027700140710     C                   Z-ADD     KLNP          EVBLNP
027800140710     C                   Z-ADD     KNRS          EVBNRS
027900140710     C                   Z-ADD     KNSP          EVBNSP
028000140710     C                   Z-ADD     LRQ8DDCI      EVBDEV
028100140710     C     LRQ8FPPI      IFEQ      'M'
028200140710     C                   MOVEL     0800          EVBHEV
028300140710     C                   ELSE
028400140710     C                   MOVEL     1200          EVBHEV
028500140710     C                   END
028600140710     C                   MOVEL(p)  Devb01new     EVBNOT
028700140710     C                   MOVEL     *BLANKS       EVBDTC
028800140710      *
028900140710     C                   WRITE     FNEVB000
029000140710     C*
0291001407113e   C                   END
0292001407112e   C                   END
0293001407111e   C                   END
029400140710      *
029500140110     c                   endsr
029600140110      *---------------------------------------------------------------*
029700140110      * fine
029800140110      *---------------------------------------------------------------*
029900140110     C     srfine        BEGSR
030000140110     C*
030100140110     C* C* TIPO LANCIO, TLA "C" -           CHIUSO CON LR
030200140110     C* C* TIPO LANCIO, TLA "L" - ELABORO E CHIUDO CON LR
030300140110     C* C* TIPO LANCIO, TLA " " - ELABORO E CHIUDO IN RETRN
030400140707     c                   movel(p)  fnLRq8ds      kpjbu
030500140110     C*
030600140707     c                   IF        LRq8tlaI = 'R'
030700140110     C                   SETON                                        RT
030800140110     c                   else
030900140113      * li chiude sempre (anche se non erano stati aperti - tanto c'è l'ind.errore)
031000140113     c                   close     FNevb00F                             99
031100140110     C                   SETON                                        LR
031200140110     c                   end
031300140110     c                   return
031400140110     c*
031500140110     C                   ENDSR
031600140110     C*-----------------------------------------------------*
031700140110     C     *INZSR        BEGSR
031800140110     C*-----------------------------------------------------*
031900140110     C*
032000140110     C* Inverto la data del giorno
032100140110     C                   TIME                    WHHDAT           14 0
032200140110     C                   MOVE      WHHDAT        G02DAT
032300140110     C                   MOVE      *BLANKS       G02ERR
032400140110     C                   CALL      'XSRDA8'
032500140110     C                   PARM                    WLBDA8
032600140110     C                   Z-ADD     G02INV        DATEU             8 0
032700140110     C*
032800140110     C* Decodifica Elaboratore
032900140110     c     *dtaara       define    §azute        azuteds
033000140110     c     *dtaara       define    §datiute      ddatiute
033100140110     C                   in(E)     *dtaara
033200140110     C                   IF        %Error  or  RSUT = *blanks
033300140110     C                   call      'TIBS34R'
033400140110     C                   parm                    Tibs34Ds
033500140110     C                   in        *dtaara
033600140110     c                   ENDIF
033700140110     C*-----------------------------------------------------*
033800140711     C     Bolla_Prec    KLIST
033900140110     C                   KFLD                    LBLAAP
034000140110     C                   KFLD                    LBLLPP
034100140110     C                   KFLD                    LBLNRP
034200140110     C                   KFLD                    LBLNSP
034300140110      *
034400140711     c     Bolla_INP     klist
034500140707     c                   kfld                    LRq8aasI
034600140707     c                   kfld                    LRq8lnpI
034700140707     c                   kfld                    LRq8nrsI
034800140707     c                   kfld                    LRq8nspI
034900140110      *
035000140710     C     Bolla_ARB     KLIST
035100140710     C                   KFLD                    ARBAAS
035200140710     C                   KFLD                    ARBLNP
035300140710     C                   KFLD                    ARBNRS
035400140710     C                   KFLD                    ARBNSP
035500140110      *
035600140711     C     Bolla_segue   KLIST
035700140110     C                   KFLD                    LBLAAN
035800140110     C                   KFLD                    LBLLPN
035900140110     C                   KFLD                    LBLNRN
036000140110     C                   KFLD                    LBLNSN
036100140110      *
036200140710      *
036300140710     C     Evb_Bolla     KLIST
036400140710     C                   KFLD                    KAAS
036500140710     C                   KFLD                    KLNP
036600140710     C                   KFLD                    KNRS
036700140710     C                   KFLD                    KNSP
036800140710     C                   KFLD                    KCEV
036900140110      *
037000140110     C                   ENDSR
037100140110     C*-----------------------------------------------------*
