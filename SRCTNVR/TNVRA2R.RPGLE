000100980127     H DECEDIT('0,') DATEDIT(*YMD.)
000200981106      *---------------------------------------------------------------------*
000300001113      * Esamina le Bolle che in sede non hanno la data di consegna
000400001113      *   e dovrebbero averla.
000500001113      *---------------------------------------------------------------------*
000600001023     FAZORG01L  IF   E           K DISK
000700001023     F*----------
000800001024     FTITAS28C  IF   E           K DISK
000900001023     F*----------
001000001026     FTNCSB03L  IF   E           K DISK
001100001026     F*----------
001200001026     FFNEVB01L  IF   E           K DISK
001300001026     F*----------
001400001026     FFIGCP01L  IF   E           K DISK
001500001110     F*----------
001600001110     FFNLBL02L  IF   E           K DISK
001700001026     F*----------
001800001026     FTABEL00F  IF   E           K DISK
001900001026     F*----------
002000001023     FFNTASC0T  O  A E             DISK    USROPN
002100001024     F*----------
002200001024     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
002300001023      *---------------------------------------------------------------------*
002400001023      * Comandi x QCMDEXC
002500001024     D CMDD            S             46    DIM(6) CTDATA PERRCD(1)              CMD Exec
002600001024     D MSG             S             80    DIM(4) CTDATA PERRCD(1)              MESSAGGI
002700001023      *----------------------------------------------------------------
002800980128      * CAMPI DI COMODO
002900001023      *----------------------------------------------------------------
003000001026     D Data8           s               D   datfmt(*iso)
003100001026     D Data80          s               D   datfmt(*iso)
003200001023     D MaxVolte        S              5  0
003300001023     D UDATE8          S              8  0
003400001023     D UdateAMG        S               D
003500001023     D DATTIM          S             14  0
003600001024     D §Fil            S              3
003700001024     D SNPOSTE         S              9
003800001024     D MESSAG          S             80
003900001024     D VAR             S              5
004000001024     D LUNG            S             15  5 INZ(46)
004100001024     D Comman          S             80
004200001024     D Fld4            S              4
004300001024     D LIB             S             10
004400001024     D MBR             S             10
004500001024     D FILE            S             10
004600001024     D NRRX            S             10  0
004700001024     D NrTas           S              7  0 INZ(0)
004800001024     D W236            S            236
004900001110     D Origine         S              1
005000001113     D ConFiglia       S              1
005100001110     D DatDCE          S              8  0
005200980127      *---------------------------------------------------------------------*
005300980128      * DS
005400980128      *---------------------------------------------------------------------*
005500001026     D DS2A          E DS
005600001026      *-------------------
005700001023     D DS02            DS
005800001024     D   aData                 1      8  0
005900001024     D   aAnno                 1      4  0
006000001024     D   aMeseGG               5      8  0
006100001024     D  FlgPoste               9      9
006200001026      *-------------------
006300001026      * DS "TNSD99R" - CALCOLO DELIVERY
006400001026      *-------------------
006500001024     D DSSD99        E DS                  EXTNAME(TNSD99DS)
006600001023      *---------------------------------------------------------------------*
006700001023     D OG143         E DS
006800001023     D KPJBA         E DS
006900001024     D CNCR80        E DS
007000001023     D TIBS50        E DS                  EXTNAME(TIBS50DS)
007100001024     D DSCMZ         E DS
007200001023      *---------------------------------------------------------------------*
007300980128     D UT§DSE0F      E DS
007400980128     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
007500980128     D  KCU                  698    847P 0 DIM(50)     PACKEVEN                 Capiconto
007600001023      *---------------------------------------------------------------------*
007700980127      * Ds scomposzione tipo capoconti
007800961003     D TCUDS           DS
007900961003     D  F34                    3      4
008000961003     D  F56                    5      6
008100001023      *---------------------------------------------------------------------*
008200001024      *  Ds per inversione data
008300961003     D WLBDA8          DS
008400980127     D  G08DAT                 1      8  0
008500980127     D  G08INV                 9     16  0
008600980127     D  G08ERR                17     17
008700980127     D  G08TGI                18     22  0
008800961003     D                SDS
008900961003     D  NOMPGM                 1     10
009000981106      *---------------------------------------------------------------------*
009100980127      * Ciclo principale
009200980127      *---------------------------------------------------------------------*
009300001023     C                   Read      AzOrg01L
009400001023     C*
009500001023     C                   DOW       not %EOF(AzOrg01L)
009600001024     C*
009700001024     C* Imposta la DS per controllare se Posta ? e la Filiale
009800001024     C                   Movel     OrgDE3        OG143
009900001024     C                   Movel     OrgFIL        §Fil
010000001024     C                   eval      NrTas = 0
010100001023     C*
010200001023     C*    creazione del membro da scrivere e apertura
010300001024     C                   Exsr      ChkTrasm
010400001024     C                   Exsr      OvrTASC
010500001023     C*
010600001024     C     KTAS          Chain     TITAS28C                           21
010700001023     C*
010800001024     C*  Ciclo di lettura TITAS28C
010900001023     C     *IN21         Doweq     *Off
011000001023     C*
011100001023     C*  Se viene suparato il limite superiore deve uscire dal giro
011200001023     C                   If        AAnno = TASAAS and TASMGS > AMeseGG or
011300001023     C                             TASAAS > AAnno
011400001023     C                   Leave
011500001023     C                   EndIf
011600001023     C*
011700001110     C*  Non devono essere questi tipi bolle
011800001110     C                   If        TASTBL = 'F3' or TASTBL='A3' or
011900010222     C                             TASTBL = 'AP' or
012000010222     C                             TASCBO = 'B '
012100001110     C                   Else
012200001024     C* Scrittura dei dati sul file da inviare alla filiale
012300001023     C                   EXSR      Dettaglio
012400001110     C                   EndIf
012500001023     C*
012600001024     C     KTAS          ReadE     TITAS28C                               21
012700001023     C                   EndDo
012800001023     C*
012900001023     C*     Trasmissione del membro alla filiale
013000001024     C                   Exsr      DlcTASC
013100001024     C*
013200001024     C*  Se li membro è pieno lo trasmette altrimenti lo rimuove
013300001024     C                   if        NrTas >0
013400001024     C                   Exsr      SbmTrasm
013500001024     C                   else
013600001024     C                   Exsr      RmvTASC
013700001024     C                   end
013800001023     C*
013900001023     C                   Read      AzOrg01L
014000001023     C                   EndDo
014100961003      * Fine Lavoro
014200961003     C     FINE          TAG
014300001024     C                   exsr      finsr
014400001024     C*
014500980128     C                   EVAL      *INLR = *ON
014600980127      *----------------------------------------------------*
014700980127      *   *INZSR: Operazioni iniziali                      *
014800980127      *----------------------------------------------------*
014900980127     C     *INZSR        BEGSR
015000001113      *
015100001113     C     *like         define    LBLAAn        savAAN
015200001113     C     *like         define    LBLLPn        savLPN
015300001113     C     *like         define    LBLNRn        savNRN
015400001113     C     *like         define    LBLNSn        savNSN
015500980127      *
015600961003     C     *ENTRY        PLIST
015700961003     C                   PARM                    KPJBA
015800001023     C                   MOVEL     KPJBU         DS02
015900980127      *
016000001026     C     KSPE          KLIST
016100001026     C                   KFLD                    TASAAS
016200001026     C                   KFLD                    TASLNP
016300001026     C                   KFLD                    TASNRS
016400001026     C                   KFLD                    TASNSP
016500001026      *
016600001026     C     KGIA          KLIST
016700001026     C                   KFLD                    TASAAS
016800001026     C                   KFLD                    TASLNP
016900001026     C                   KFLD                    TASNRS
017000001026     C                   KFLD                    TASNSP
017100001026     C                   KFLD                    TipZero
017200001026      *
017300001026     C     Ktab          KLIST
017400001026     C                   KFLD                    tblkut
017500001026     C                   KFLD                    tblcod
017600001026     C                   KFLD                    tblkey
017700001110      *
017800001110     C     Kfiglia       KLIST
017900001110     C                   KFLD                    LBLAAn
018000001110     C                   KFLD                    LBLLPn
018100001110     C                   KFLD                    LBLNRn
018200001110     C                   KFLD                    LBLNSn
018300001026      *
018400001026     C                   z-add     0             TipZero           2 0
018500001026      *
018600980127      *  Richiamo XPARUT
018700961003     C                   Z-ADD     1             CODUT
018800961003     C                   CALL      'X§PARUT'
018900980128     C                   PARM                    UT§DSE0F
019000001024     C                   MOVEL     REC80         CNCR80
019100001024     C***                MOVEL     RAGUT         RSUT
019200001024      *
019300001024      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
019400980127      *  Chiavi di accesso
019500001023     C     KTAS          KLIST
019600001024     C                   KFLD                    Data0             8 0
019700001023     C                   KFLD                    ORGFIL
019800001024      *
019900001024      *>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
020000001023      * Reperisco la DATA del Giorno
020100980128     C                   TIME                    DATTIM
020200980128     C                   MOVE      DATTIM        UDATE8
020300001023     C                   Z-add     0             Data0
020400001023      *  Giro UDATE8
020500980127     C                   Z-ADD     UDATE8        G08DAT
020600980127     C                   MOVEL     *BLANKS       G08ERR
020700980127     C                   Z-ADD     *ZEROS        G08INV
020800980127     C                   CALL      'XSRDA8'
020900980127     C                   PARM                    WLBDA8
021000980128      *
021100001024     C                   If        FlgPoste = 'S'
021200001024     C                   eval      SNPOSTE = '  POSTE  '
021300001024     C                   else
021400001024     C                   eval      SNPOSTE = 'NON POSTE'
021500001024     C                   end
021600001024      *
021700001024      *  Accende l'indicatore per emettere la prima volta la testata
021800001024     C                   SETON                                        OF
021900001024      *
022000001024     C                   ENDSR
022100001024     C*------------------------------------------------------------------------
022200001024     ** scrive il file
022300001024     **------------------------------------------------------------------------
022400001024     C     Dettaglio     BEGSR
022500001113      *
022600001113     C                   clear                   FNTASC00
022700001024      *
022800001110      *  Controlla se la bolla ha una figlia e se quest'ultima non ha
022900001110      *   data consegna non deve scrivere il record spia x il controllo
023000001110      *    altrimenti deve impostare direttamente la data di consegna della
023100001110      *     figlia.
023200001110     C                   Exsr      chk_bol_orig
023300001110      *
023400001110      * Se è la bolla Origine e l'ultima figlia ha data (0)
023500001110      *    non deve scrivere la spia
023600001110     C                   If        Origine   = 'S' and
023700001113     C                             ConFiglia = 'S' and
023800001110     C                             DatDCE    =  0
023900001110      *
024000001110     C                   Goto      NoWrite
024100001110      *
024200001110     C                   End
024300001110      *
024400001110      * Forza la data della figlia
024500001110     C                   If        Origine   = 'S' and
024600001113     C                             ConFiglia = 'S' and
024700001110     C                             DatDCE    >  0
024800001110     C                   Eval      TssDCE = DatDCE
024900001110     C                   End
025000001110      *
025100001024     C                   Eval      TssLNP = TasLNP
025200001024     C                   Eval      TssAAS = TasAAS
025300001024     C                   Eval      TssNRS = TasNRS
025400001024     C                   Eval      TssNSP = TasNSP
025500001024     C                   Eval      TssDTI = TasDTI
025600001024     C                   Eval      TssHTI = TasHTI
025700001110      *
025800001110      * se non è già stata impostata prima
025900001110     C                   If        TssDCE = 0
026000001024     C                   Eval      TssDCE = TasDCE
026100001110     C                   EndIF
026200001113      *
026300001024     C                   Eval      TssNPI = TasNPI
026400001024     C                   Eval      TssNCI = TasNCI
026500001024     C                   Eval      TssNRC = TasNRC
026600001024     C                   Eval      TssDUC = TasDUC
026700001024     C                   Eval      TssFLP = TasFLP
026800001024     C                   Eval      TssNDC = TasNDC
026900001024     C                   Eval      TssDDC = TasDDC
027000001024     C                   Eval      TssDCP = TasDCP
027100001024     C                   Eval      TssDCM = TasDCM
027200001024     C                   Eval      TssHMC = TasHMC
027300001024     C                   Eval      TssCCA = TasCCA
027400001024      *
027500001113      *   imposta l'affidabilità se non è già stata impostata.
027600001110     C                   If        TssDCE = 0
027700001024     C                   exsr      Delivery
027800001110     C                   endIF
027900001024      *
028000001113      *  e scrive solo se ha la data impostata.
028100001110     C                   if        TSSDCE > 0
028200001024     C                   add       1             NrTas
028300001024     C                   write     FNTASC00
028400001110     C                   endIf
028500001024      *
028600001110     C     NoWrite       ENDSR
028700001024     C*------------------------------------------------------------------------*
028800001110     C* Controlla la bolla se è origine
028900001024     C*------------------------------------------------------------------------*
029000001110     C     Chk_Bol_Orig  BEGSR
029100001110      *
029200001110     C                   move      'N'           Origine
029300001113     C                   move      'N'           ConFiglia
029400001110      *
029500001110     C     KSPE          chain     FnLBL02l
029600001113      *
029700001113      * Avendola trovata in questo file significa che c'è una figlia
029800001113      * occorre controllare che non ci sia una figlia di una figlia.
029900001110     C                   if        %found(FnLBL02l)
030000001113     C                   move      'S'           ConFiglia
030100001113      *
030200001113      * Se c'è uguaglianza fra Precedente e Origine è una
030300001113      *   Bolla origine.
030400001110     C     LBLLPO        IFeq      LBLLPP
030500001110     C     LBLNRO        Andeq     LBLNRP
030600001110     C     LBLNSO        Andeq     LBLNSP
030700001110     C     LBLAAO        Andeq     LBLAAP
030800001110     C                   move      'S'           Origine
030900001113     C                   end
031000001113      *
031100001113      * Tenta di trovare una seconda o successiva figlia
031200001113     C     Ritenta       tag
031300001113      *   =========
031400001113      *
031500001113      *  salva comunque la chiave della figlia x ctl data consegna
031600001113     C                   z-add     LBLAAn        savAAN
031700001113     C                   z-add     LBLLPn        savLPN
031800001113     C                   z-add     LBLNRn        savNRN
031900001113     C                   z-add     LBLNSn        savNSN
032000001113      *
032100001110     C     Kfiglia       Chain     FnLBL02l
032200001110      *
032300001113      * Ha trovato un'altra figlia
032400001113     C                   IF        %found(FnLBL02l)
032500001110     C                   goto      Ritenta
032600001110      *                           =========
032700001110     C                   else
032800001110      *
032900001113      *  controlla con l'ultima figlia la data di consegna.
033000001110      *         CHIAMA Routine esterna che legge il TITAS....  e
033100001110      *           restituisce la data.
033200001110     C                   z-add     0             DatDCE
033300001116     C                   Call      'TNVRA2R1'
033400001110     C                   parm                    savAAN
033500001110     C                   parm                    savLPN
033600001110     C                   parm                    savNRN
033700001110     C                   parm                    savNSN
033800001110     C                   parm                    DatDCE
033900001113     C                   endIF
034000001113      *
034100001110     C                   end
034200001110      *
034300001110     C                   ENDSR
034400001110     C*------------------------------------------------------------------------*
034500001110     C* DELIVERY - SIMULAZIONE TEMPI DI DELIVERY
034600001110     C*------------------------------------------------------------------------*
034700001110     C     DELIVERY      BEGSR
034800001110     C*
034900001024     C                   clear                   DSSD99
035000001110     C                   movel     tasaas        D98DSP                         *DATA SPEDIZIONE
035100001024     C                   move      tasmgs        D98DSP                         *DATA SPEDIZIONE
035200001110      *
035300001110     C                   movel     tasaas        D98AAS                         *Anno
035400001024     C                   z-add     taslnp        D98LNP                         *LINEA DI PARTENZA
035500020129     C**                 z-add     tasnrs        D98nrs                         *LINEA DI PARTENZA
035600020129     C***                z-add     tasnsp        D98nsp                         *LINEA DI PARTENZA
035700001110      *
035800001024     C                   z-add     tastfp        D98TFP                         *TERMINAL PARTENZA
035900001024     C                   movel     tasnzd        D98NZD                         *NAZIONE DI DESTINO
036000001024     C                   movel     tascad        D98CAD                         *CAP DI DESTINO
036100001024     C                   movel     taslod        D98LOD                         *LOCALITA' DESTINO
036200001024     C                   z-add     taslna        D98LNA                         *LINEA DI ARRIVO
036300001024     C                   z-add     tastfa        D98TFA                         *TERMINAL DI ARRIVO
036400001024     C                   movel     tastsp        D98TSP                         *TIPO SPEDIZIONE
036500001024     C                   movel     tasfin        D98FIN                         *FLAG INOLTRO-CAP IS
036600001024     C                   z-add     1             D98NCD                         *BOLLA CON COLLI
036700001025      *
036800001025     C                   z-add     tasduc        D98DUC
036900001025     C                   movel     tastbl        D98TBL
037000001025     C                   movel     tasgc1        D98gc1
037100001025     C                   movel     tasgc2        D98gc2
037200001025     C                   movel     tasfns        D98fns
037300001025     C                   z-add     tasncd        D98NCD
037400001025     C                   z-add     tasdbr        D98DBR
037500001025     C                   z-add     tasduc        D98DUC
037600001025     C                   z-add     tasdti        D98DTI
037700001025     C                   z-add     tashti        D98HTI
037800001025     C                   movel     tasffd        D98ffd
037900001025     C                   movel     tastcr        D98tcr
038000001025     C                   z-add     tasdcr        D98DCR
038100001025     C                   z-add     tashcr        D98HCR
038200001025      *
038300001025      *  controlla se c'è contrassegno
038400001025     C     TASFCA        ifeq      'S'
038500001026     C     KSPE          chain     tncsb03l
038600001026     C                   if        %Found(tncsb03l)
038700020129     C                   z-add     CSBCAS        D98CAS
038800001026     C                   movel     CSBTPA        D98TIC
038900001026     C                   move      CSBTPI        D98TIC
039000001025     C                   endif
039100001026     C                   endif
039200001025      *
039300001025      *  controlla se c'è giacenza
039400001025     C     TASFGC        ifeq      'S'
039500001026     C     KGIA          chain     FIGCP01l
039600001026     C                   if        %Found(FIGCP01L)
039700001026     C                   movel     GCPAGC        D98DAG
039800001026     C                   move      GCPMGC        D98DAG
039900001026     C                   z-add     GCPDCG        D98DCG
040000001026     C                   endif
040100001025      *
040200001026     C                   else
040300001026      *
040400001026      *  controlla se c'è evento di lasciato avviso oppure
040500001025      *  controlla se non c'è giacenza e se c'è evento di tipo giacenza
040600001026      *  imposta nelle date di apertura/chiusura giacenza la data dell'evento
040700001026      *  incrementata dei giorni indicati sulla DS
040800001026     C     KSPE          chain     fnevb01l                           21
040900001026      *
041000001026     C     *in21         doweq     *off
041100001026     C                   movel     Codut         tblkut
041200001026     C                   movel     '2A'          tblcod
041300001026     C                   movel     evbcev        tblkey
041400001026     C     KTAB          chain     Tabel00f                           22
041500001026     C                   if        *in22 = *Off
041600001026     C                   MOVEL     TBLUNI        ds2a
041700001026     C     §2AFTG        ifeq      'S'
041800001026     C                   z-add     EVBDEV        D98DAG
041900001026     C                   move      EVBDEV        Data8
042000001026     C     data8         ADDdur    §2agla:*d     data8
042100001026     C                   move      Data8         D98DCG
042200001026     C                   endif
042300001026     C                   end
042400001026     C     KSPE          reade     fnevb01l                               21
042500001026     C                   enddo
042600001026      *
042700001026     C                   endif
042800001025      *
042900001026      *  chiama il pgm per calcolare la consegna
043000001024     C                   call      'TNSD99R'
043100001024     C                   parm                    DSSD99
043200001024     C*
043300001026     C*  Dati di ritorno della routine esterna
043400001025     C                   movel     D98DCE        TSSDCE
043500001025     C                   movel     D98NPI        TSSNPI
043600001025     C                   movel     D98NCI        TSSNCI
043700001025     C                   movel     D98NRC        TSSNRC
043800001024     C*
043900020129     C*  Forzatura se non è riuscito a calcolare imposta la data di fine
044000020129     C*  anno stando attento al MGS
044100020129     c                   if        TssDCE =0
044200020129     C                   movel     TssAAS        TssDCE
044300020130     C                   move      1231          TssDCE
044400020129     c                   end
044500020129     C*
044600001024     C                   ENDSR
044700001024     C*------------------------------------------------------------------------*
044800001024     C* FINSR - OPERAZIONI FINALI DI CHIUSURA ARCHIVI
044900001024     C*------------------------------------------------------------------------*
045000001024     C     FINSR         BEGSR
045100001024     C*
045200001024     C                   clear                   DSSD99
045300001024     C                   movel     'C'           D98TLA                         *TIPO LANCIO
045400001024     C                   call      'TNSD99R'
045500001024     C                   parm                    DSSD99
045600001024     C*
045700001024     C                   ENDSR
045800001024     C*------------------------------------------------------------------------*
045900001023     ** Sottometto trasmissioni
046000001023     **------------------------------------------------------------------------
046100001023     C     SBMTrasm      BEGSR
046200001023     C*
046300001023     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
046400001023     C* da/a cui spedire
046500001023     C                   CLEAR                   TIBS50
046600001023     C                   MOVEL     'L'           I50TLA
046700001023     C                   MOVEL     KNSIF         I50PSI
046800001024     C                   MOVEL     '046'         I50PPO
046900001024     C                   MOVEL     §Fil          I50APO
047000001023     C                   CALL      'TIBS50R'
047100001023     C                   PARM                    TIBS50
047200001023     C*
047300001023     C*  prepara i parametri per
047400001024     C                   MOVEL     §Fil          CMZDST                          DEST.:SED
047500001024     C                   MOVEL     'FNTASC0T'    CMZFLD                          DA: >>>
047600001023     C                   MOVEL     'M   '        Fld4
047700001024     C                   MOVE      §Fil          Fld4
047800001023     C                   MOVEL     *BLANK        CMZMBD
047900001023     C                   MOVEL     Fld4          CMZMBD
048000001023     C                   MOVEL     O50PLG        CMZLBD
048100001024     C                   MOVEL     'FNTASC0R'    CMZFLA                          A: >>>
048200001023     C                   MOVEL     *BLANK        CMZMBA
048300001023     C                   MOVEL     Fld4          CMZMBA
048400001023     C                   MOVEL     O50ALG        CMZLBA
048500001023     C                   MOVE      '0000'        CMZORI                          ORA
048600001023     C                   MOVE      '0000000'     CMZDTI                          DATA
048700001023     C                   MOVE      'N'           CMZURG                          NOTTE
048800001023     C                   MOVE      'A'           CMZADD                          IN ADD
048900001023     C                   MOVE      'S'           CMZCLR                          CLEAR
049000001023     C                   MOVE      'S'           CMZFTR                          DA TRANS.
049100001023     C                   MOVE      *BLANK        CMZERR                          1= ERR.
049200001024     C                   MOVE      '????'        CMZAZN                          AZIONE
049300001023     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
049400001024     C                   MOVE      NrTas         CMZNRR                          N.REC.
049500001023     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
049600001023     C*
049700001023     C                   CLEAR                   W236
049800001024     c                   goto      salta
049900001023     C                   CALL      'TIBS52C'
050000001023     C                   PARM                    O50LCL
050100001023     C                   PARM                    CMZDST
050200001023     C                   PARM                    CMZFLD
050300001023     C                   PARM                    CMZMBD
050400001023     C                   PARM                    CMZLBD
050500001023     C                   PARM                    CMZFLA
050600001023     C                   PARM                    CMZMBA
050700001023     C                   PARM                    CMZLBA
050800001023     C                   PARM                    CMZORI
050900001023     C                   PARM                    CMZDTI
051000001023     C                   PARM                    CMZURG
051100001023     C                   PARM                    CMZADD
051200001023     C                   PARM                    CMZCLR
051300001023     C                   PARM                    CMZFTR
051400001023     C                   PARM                    CMZERR
051500001023     C                   PARM                    CMZAZN
051600001023     C                   PARM                    CMZSYF
051700001023     C                   PARM                    CMZNRR
051800001023     C                   PARM                    CMZPRR
051900001023     C                   PARM                    W236
052000001023     C*
052100001024     C     salta         ENDSR
052200001023      *------------------------------------------------------------------*
052300001024      *  CHKTrasm: controllo della precedente trasmissione dati.
052400001023      *------------------------------------------------------------------*
052500001023     C     CHKTrasm      BEGSR
052600001023      *
052700001023     C* -----------
052800001024     C* Controllo se ci sono dati in FNTASC0T per trasmissione
052900001023     C*  precedente incompleta.
053000001023     C                   Z-ADD     0             NRRX
053100001024     C                   MOVEL     'FNTASC0T'    FILE
053200001023     C                   MOVEL     '*LIBL   '    LIB
053300001023     C                   MOVEL     'M   '        Fld4
053400001024     C                   MOVE      §Fil          Fld4
053500001023     C                   MOVEL     Fld4          MBR
053600001023      *  controllo esterno
053700001023     C                   CALL      'TRUL51C'
053800001023     C                   PARM                    LIB
053900001023     C                   PARM                    FILE
054000001023     C                   PARM                    MBR
054100001023     C                   PARM                    NRRX
054200001023      *
054300001024      * se risultano presenti dei records deve segnalare la mancata
054400001024      *   precedente trasmissione.
054500001023     C                   IF        Nrrx <>0 and Nrrx <> *all'9'
054600001024     C                   MOVEL     MSG(1)        MESSAG
054700001024     C                   MOVE      §Fil          MESSAG
054800001024     C   OF              Except    Testa
054900001024     C                   Except    Error
055000001023     C                   END
055100001023      *
055200001023     C                   ENDSR
055300001023     C*----------------------------------------------------*
055400001024     C* Esegue OVRDBF su FNTASC0T
055500001023      *----------------------------------------------------*
055600001024     C     OvrTASC       BEGSR
055700001023      *
055800001023     C                   MOVE      ') '          VAR
055900001024     C                   MOVEL     §Fil          VAR
056000001023      *
056100001024      *  CONTROLLO SE ESISTE IL MEMBRO X FNTASC0T
056200001023     C                   SETOFF                                       20
056300001023     C                   MOVE      VAR           CMDD(1)
056400001023     C                   MOVEL     *BLANKS       COMMAN
056500001023     C                   MOVEL     CMDD(1)       COMMAN
056600001024     C                   CALL      'QCMDEXC'                            20
056700001023     C                   PARM                    COMMAN
056800001023     C                   PARM                    LUNG
056900001023      *
057000001023      *  ESEGUE ADDPFM DEL MEMBRO NUOVO
057100001023     C   20              DO
057200001023     C                   MOVE      VAR           CMDD(2)
057300001023     C                   MOVEL     *BLANKS       COMMAN
057400001023     C                   MOVEL     CMDD(2)       COMMAN
057500001023     C                   CALL      'QCMDEXC'
057600001023     C                   PARM                    COMMAN
057700001023     C                   PARM                    LUNG
057800001023     C                   END
057900001023      *
058000001023      *  ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
058100001023     C                   MOVE      VAR           CMDD(3)
058200001023     C                   MOVEL     *BLANKS       COMMAN
058300001023     C                   MOVEL     CMDD(3)       COMMAN
058400001023     C                   CALL      'QCMDEXC'
058500001023     C                   PARM                    COMMAN
058600001023     C                   PARM                    LUNG
058700001023      *
058800001023      * GUARDO SE è ALLOCATO
058900001023     C                   MOVE      ')'           VAR
059000001023     C                   MOVE      VAR           CMDD(4)
059100001023     C                   MOVEL     CMDD(4)       COMMAN
059200001024     C                   CALL      'QCMDEXC'
059300001023     C                   PARM                    COMMAN
059400001023     C                   PARM                    LUNG
059500001023      *
059600001024      * apertura del file
059700001024     C                   OPEN      FNTASC0T
059800001023      *
059900001023     C                   ENDSR
060000001023     C*-------------------------------------------------------------------------*
060100001024     C* ESEGUO RMVM    FNTASC0T per membri vuoti
060200001023     C*-------------------------------------------------------------------------*
060300001024     C     RmvTASC       BEGSR
060400001024      *
060500001024     C                   MOVE      ') '          VAR
060600001024     C                   MOVEL     §Fil          VAR
060700001024      *
060800001024      *  ESEGUE RMVM   DEL MEMBRO VUOTO
060900001024     C                   MOVE      VAR           CMDD(6)
061000001024     C                   MOVEL     *BLANKS       COMMAN
061100001024     C                   MOVEL     CMDD(6)       COMMAN
061200001024     C                   CALL      'QCMDEXC'
061300001024     C                   PARM                    COMMAN
061400001024     C                   PARM                    LUNG
061500001024      *
061600001024     C                   ENDSR
061700001024     C*-------------------------------------------------------------------------*
061800001024     C* ESEGUO Dlcobj  FNTASC0T
061900001024     C*-------------------------------------------------------------------------*
062000001024     C     DlcTASC       BEGSR
062100001024     C*
062200001024     C* chiusura e disallocazione del file
062300001024     C                   CLOSE     FNTASC0T
062400001023     C*
062500001023     C                   MOVE      VAR           CMDD(5)
062600001023     C                   MOVEA     CMDD(5)       COMMAN
062700001024     C                   CALL      'QCMDEXC'
062800001023     C                   PARM                    COMMAN
062900001023     C                   PARM                    LUNG
063000001023     C*
063100001023     C                   ENDSR
063200001024      *-------------------------------------------------------------------------
063300001024     OQSYSPRT   E            TESTA            01 03
063400001024     O                                         +  3 '*TNVR02R'
063500001024     O                                         + 10 'Errori durante il'
063600001024     O                                         +  1 'Controllo bolle non '
063700001024     O                                         +  1 'CONSEGNATE'
063800001024     O                       SNPOSTE           +  1
063900001024     O                                         +  1 'spedite fino al'
064000001024     O                       AData             +  1 '    /  /  '
064100001024     O                       UDATE         Y    122
064200001024     O                                          128 'Pag.'
064300001024     O                       PAGE          Z    132
064400001024     O          E            ERROR       1
064500001024     O                       MESSAG              90
064600001023**
064700001023CHKOBJ OBJ(FNTASC0T) OBJTYPE(*FILE) MBR(M000)
064800001023ADDPFM FILE(FNTASC0T)               MBR(M000)
064900001023OVRDBF FILE(FNTASC0T)               MBR(M000)
065000001023ALCOBJ OBJ((FNTASC0T *FILE *EXCL        M000))
065100001023DLCOBJ OBJ((FNTASC0T *FILE *EXCL        M000))
065200001024RMVM   FILE(FNTASC0T)               MBR(M000)
065300001024**
065400001024Esiste una precedente trasmissione per il file FNTASC0T della filiale ->
