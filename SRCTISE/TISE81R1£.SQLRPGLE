000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050512     ftabel00f  if   E           K DISK
000400050513     fazorg01l  if   E           K DISK
000500050513      **
000600050512     fwfdcm00f  uf a E           K DISK
000700050512     fTise81P   o    E             PRINTER OFLIND(*in66)
000800050513      **
000900050510      **-----------------------------------------------------------------***
001000040820     d KPJBA         E DS
001100050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200050513     D*------------------
001300050513     D* DS REPERIMENTO DATI UTENTE
001400050513     D*-------------------
001500050513     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
001600050513     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
001700050513     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
001800050513     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
001900050513     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
002000050513     D*------------------
002100050513     D* DS "TIBS10R"
002200050513     D*------------------
002300050513     D BS10DS        E DS                  EXTNAME(TIBS10DS)
002400050513     D*------------------
002500050513      *
002600050512     D DsPassaggio   e DS                  EXTNAME(Tise81ds)
002700050512     D tise800f      e DS
002800050512     D sisdo00f      e DS
002900040820     D*
003000050513     D*------------------
003100040820     D WLBDAT          DS
003200040820     D  G02DAT                 1      8  0
003300040820     D  G02INV                 9     16  0
003400040820     D  G02ERR                17     17
003500040820     D  G02TGI                18     22  0
003600050513     D*------------------
003700050513     D* ORGANIGRAMMA
003800050513     D*------------------
003900050513     D FIL             S              3  0 DIM(500)                             *FILIALI
004000050513     D DFIL            S             20    DIM(500)
004100050513     D ARE             S              3  0 DIM(500)                             *AREE
004200050513     D DARE            S             20    DIM(500)
004300050513     D DIV             S              1    DIM(500)                             *DIVISIONI
004400050513     D DDIV            S             20    DIM(500)
004500040820      *
004600040820     D  data_Oggi      s                   like(G02INV) INZ(0)
004700040826     D  almeno_uno     s              1
004800040820      *
004900050513     D  FLD_AAAAmm     s              6  0
005000050513     D  FLD_Anno       s                   like(sdoann)
005100050513     D  FLD_Mese       s                   like(sdomes)
005200050513     D  FLD_Cliente    s                   like(sdoksc)
005300050513     D  FLD_NumSped    s                   like(sdonsp)
005400050513     D  FLD_ImpRBol    s                   like(sdoirb)
005500050513     D  FLD_ImpRRet    s                   like(sdoirr)
005600050513     D  FLD_ImpNotC    s                   like(sdoinc)
005700050513     D  FLD_ImpRPre    s                   like(sdoirp)
005800050513     D  FLD_ImpRDel    s                   like(sdoird)
005900050513     D  FLD_Fil        s                   like(orgfil)
006000050513      *
006100050513     D  RIC_ReaNetto   s                   like(WrrW)
006200050513     D  VAL_Delta      s                   like(WdpW)
006300050513      *
006400050513     D  TOT_RicRPre    s                   like(WrpW)
006500050513     D  SAV_Cliente    s                   like(FLD_Cliente)
006600050510      *
006700050513     D  WrrW           s             17  5
006800050513     D  WrpW           s             17  5
006900050513     D  WdpW           s              4  1
007000050513     D  Gap            s             17  5
007100050511     D*-------------
007200050511     D* Reperimento nome PGM
007300050511     D STATUS         SDS           333
007400050511     D  DSPGM            *PROC
007500050511     D*-------------
007600050511      *
007700040820     D digits          C                   CONST('0123456789')
007800050511      * ?------------------------------------------------------------------ */
007900050513      * ? Sulle Testate dei Saldi si rilevano i dati di periodo anno/mese:
008000050513      * ? il num.totale delle spedizioni, tot ricavi presunti ed i tot ricavi
008100050512      * ? reali. Il delta è ricavato con la semplice formula:
008200050512      * ? DELTA = (Ric.Reale - Ric.Presunto) x 100/ Ric.Reale
008300050512      * ?
008400050513      * ? i dati vengono filtrati in base al file guida TISE800F in cui sono
008500050513      * ? impostati i codici clienti da prendere in considerazione.
008600050513      * ?
008700050513      * ? A totale si eseguono i totali anno con il relativo delta
008800050513      **  Il File è organizzato x Cliente
008900050511      * ?------------------------------------------------------------------ */
009000050510     C/EXEC SQL
009100050510     C+ Declare A1 Cursor for
009200050513     C+ SELECT decimal((SDOANN*100+SDOMES), 6, 0), SDOANN, SDOMES,
009300050513     C+  SDOKSC, SDONSP, SDOIRB, SDOIRR, SDOINC, SDOIRP, SDOIRD,
009400050513     C+  decimal(SDOKSC/10000, 3, 0)
009500050513     C+ FROM SISDO00F                     WHERE
009600050513     C+  (SDOANN*100+SDOMES) between :DataINI and :DataFIN
009700050513     C+  and SDOKSC in
009800050513     C+ (SELECT SE8COD FROM tise800f)
009900050513     C+ ORDER BY SDOKSC, SDOANN, SDOMES
010000050510     C/END-EXEC
010100050510      *          apertura cursore
010200050510     C/EXEC SQL
010300050510     C+ OPEN A1
010400050510     C/END-EXEC
010500050510     c                   clear                   almeno_uno
010600050510      **
010700050510     C                   DOU       SqlCod <> 0
010800050510      **         lettura cursore
010900050510     C/EXEC SQL
011000050513     C+ FETCH NEXT FROM A1 INTO :FLD_AAAAmm, :FLD_Anno, :FLD_Mese,
011100050513     C+ :FLD_Cliente, :FLD_NumSped, :FLD_ImpRBol, :FLD_ImpRRet,
011200050513     C+ :FLD_ImpNotC, :FLD_ImpRPre, :FLD_ImpRDel, :FLD_Fil
011300050510     C/END-EXEC
011400050510     C                   SELECT
011500050510     **
011600050510     C                   WHEN      SqlCod = 100
011700050510     c* a fine file
011800050510     c                   if        almeno_uno = 'S'
011900050510     c                   else
012000050510     c                   end
012100050510     c                   leave
012200050510     c*
012300050510     C                   WHEN      SqlCod < 0
012400050510     C                   seton                                        H1
012500050510     c                   goto      fine
012600050510     c*
012700050510     C                   OTHER
012800050510     c* DETTAGLIO
012900050510     c* riga   dettaglio
013000050513     c                   exsr      WRI_rig_det
013100050513     c                   move      'S'           almeno_uno
013200050510     C                   ENDSL
013300050513      **
013400050510     C                   ENDDO
013500050510     C/EXEC SQL
013600050510     C+ Close A1
013700050510     C/END-EXEC
013800050513      **
013900050513      ** Totali
014000050510     c                   exsr      totalizza
014100050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
014200041007     c     fine          tag
014300020326     C                   SETON                                        LR
014400050511     ?****************************************************************************
014500050511      *?    Scrive Riga File di Work
014600050511     ?****************************************************************************
014700050510     c     Wri_rig_det   begsr
014800050510      *
014900050513      * ? x rottura di Codice Cliente (Pulizia campi x Totale Cliente)
015000050513     c                   iF        FLD_Cliente <> SAV_Cliente
015100050513     c                   eXSr      NEW_Cliente
015200050513     c                   EvaL      SAV_Cliente = FLD_Cliente
015300050513     c                   EnD
015400050513      *
015500050513      * ? x Codice Cliente
015600050513     c     FLD_Cliente   chain     WFdcm00F
015700050513      *
015800050513     c                   iF        %Found(WFdcm00F)
015900050513      * ? Valori di Periodo
016000050513     c                   exsr      in_Periodo
016100050513      * ? aggiorna
016200050513     c                   update    WFDCM000
016300050513      *
016400050513     c                   eLSe
016500050513      *
016600050513     c                   clear                   WFdcm000
016700050513      * ? Decodifiche Angrafiche
016800050513     c                   exsr      Decod_ANA
016900050513      * ? Valori di Periodo
017000050513     c                   exsr      in_Periodo
017100050513      * ? scrive
017200050513     c                   write     WFDCM000
017300050513      *
017400050513     c                   eNd
017500050513      *
017600050513     c                   endsr
017700050513     ?****************************************************************************
017800050513      *?   A Rottura di Codice Cliente - Operazioni a Totale Cliente Precedente
017900050513     ?****************************************************************************
018000050513     c     NEW_Cliente   begsr
018100050513      *
018200050513      *  Pulisce Totale Ricavo Presunto dell'Anno (poichè non è sul WrkFile)
018300050513      *   deve memorizzarlo per poter ricavare il Delta Totale sui 12 mesi
018400050513      *
018500050513     c                   EvaL      TOT_RicRPre = 0
018600050510      *
018700050513     c                   if        almeno_uno = 'S' and se81LST = 'S'
018800050513      *   Stampo dettaglio Cliente Precedente
018900050513     c                   exsr      PRT_rig_sav
019000050513     c                   endIF
019100050513     c*
019200050513      *
019300050510     c                   endsr
019400050511     ?****************************************************************************
019500050513      *?   Decodifiche ANAGRAFICHE/Tabelle
019600050511     ?****************************************************************************
019700050513     c     Decod_ANA     begsr
019800050513      *
019900050513     c                   eval      dcmANN      = se81Ann
020000050513     c                   eval      dcmMES      = se81Mes
020100050513     c                   eval      dcmFIL      = FLD_Fil
020200050513     c                   eval      dcmKSC      = FLD_Cliente
020300050510      *
020400050513     C                   CLEAR                   BS69DS
020500050513     C                   CLEAR                   ACODS
020600050513     C                   MOVEL     KNSIF         I69SIF
020700050513     C                   MOVEL     dcmKSC        I69KAC
020800050513     C                   MOVEL     dcmKSC        I69KCP
020900050513     C                   CALL      'TIBS69R'
021000050513     C                   PARM                    BS69DS
021100050513     C                   PARM                    ACODS
021200050513     C                   PARM                    INDDS
021300050513     C                   PARM                    CLPDS
021400050513     C                   PARM                    CLSDS
021500050513     c*
021600050513     C     O69ERR        IFne      '1'
021700050513     c                   movel     acorag        dcmCLD
021800050513     c                   movel     clpclv        dcmCLV
021900050513     C                   End
022000050513     c*
022100050513     C                   CLEAR                   BS10DS
022200050513     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
022300050513     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
022400050513     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
022500050513     C                   MOVE      dcmKSC        D10COD
022600050513     C                   CALL      'TIBS10R'
022700050513     C                   PARM                    BS10DS
022800050513     c*
022900050513     C     D10ERR        IFne      *BLANKS
023000050513     C                   CLEAR                   BS10DS
023100050513     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
023200050513     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
023300050513     C                   MOVEL     'F'           D10PAF                         *CERCA PADRE
023400050513     C                   MOVE      dcmKSC        D10COD
023500050513     C                   CALL      'TIBS10R'
023600050513     C                   PARM                    BS10DS
023700050513     c                   enD
023800050513      **
023900050513     C     D10ERR        IFeq      *BLANKS
024000050513     c                   z-add     d10cop        dcmKSU
024100050513     C                   CLEAR                   BS69DS
024200050513     C                   CLEAR                   ACODS
024300050513     C                   MOVEL     KNSIF         I69SIF
024400050513     C                   MOVEL     dcmKSU        I69KAC
024500050513     C                   CALL      'TIBS69R'
024600050513     C                   PARM                    BS69DS
024700050513     C                   PARM                    ACODS
024800050513     C                   PARM                    INDDS
024900050513     C                   PARM                    CLPDS
025000050513     C                   PARM                    CLSDS
025100050513     c* Descrizione Cliente Uificante se c'è......
025200050513     C     O69ERR        IFne      '1'
025300050513     c                   movel     acorag        dcmDKS
025400050513     c                   endIF
025500050513     c                   endIF
025600050513     C*
025700050513     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
025800050513     C                   Z-ADD     1             F
025900050513     C     dcmFIL        LOOKUP    FIL(F)                                 99
026000050513IF  1C     *IN99         IFEQ      *ON
026100050513     C                   movel     dFIL(F)       dcmFID
026200050513     C                   Z-ADD     ARE(F)        dcmARE
026300050513     C                   movel     dARE(F)       dcmARD
026400050513     C                   MOVEL     DIV(F)        dcmDIV
026500050513     C                   movel     dDIV(F)       dcmDID
026600050513E   1C                   ENDIF
026700050513      *
026800050513     c                   endsr
026900050513     ?****************************************************************************
027000050513      *?   Valori attribuiti al periodo Anno-Mese o Anno Precedente
027100050513     ?****************************************************************************
027200050513     c     in_Periodo    begsr
027300050513      *
027400050513      *
027500050513     ?* ***  Ricavo Reale Netto = (Ric.Reale Delta + Note + Rettifiche)  ***
027600050513     c                   eval      RIC_ReaNetto = FLD_ImpRDel + FLD_ImpRRet +
027700050513     c                                            FLD_ImpNotC
027800050513      * ***  Costruisce il Delta
027900050513     C                   eval      WrrW = RIC_ReaNetto                          *RICAVO REALE
028000050513     C                   eval      WrpW = FLD_ImpRPre                           *RICAVO PRESUNTO
028100050513     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
028200050513     c                   exsr      CalDelta
028300050513     C                   eval      VAL_delta = WdpW                             *Delta
028400050513      *
028500050513      *  Anno Precedente .............
028600050513     c                   iF        FLD_Anno = annPRE
028700050513      *  Non prendere in considerazione.......
028800050513      **
028900050513     c                   eLSe
029000050513      *  gen
029100050513     c                   if                FLD_Mese  =  1
029200050513     c                   add       FLD_NumSped   dcmNSP01
029300050513     c                   add       RIC_ReaNetto  dcmRIC01
029400050513     c                   add       VAL_Delta     dcmDEL01
029500050513     c                   end
029600050513      *  feb
029700050513     c                   if                FLD_Mese  =  2
029800050513     c                   add       FLD_NumSped   dcmNSP02
029900050513     c                   add       RIC_ReaNetto  dcmRIC02
030000050513     c                   add       VAL_Delta     dcmDEL02
030100050513     c                   end
030200050513      *  mar
030300050513     c                   if                FLD_Mese  =  3
030400050513     c                   add       FLD_NumSped   dcmNSP03
030500050513     c                   add       RIC_ReaNetto  dcmRIC03
030600050513     c                   add       VAL_Delta     dcmDEL03
030700050513     c                   end
030800050513      *  apr
030900050513     c                   if                FLD_Mese  =  4
031000050513     c                   add       FLD_NumSped   dcmNSP04
031100050513     c                   add       RIC_ReaNetto  dcmRIC04
031200050513     c                   add       VAL_Delta     dcmDEL04
031300050513     c                   end
031400050513      *  mag
031500050513     c                   if                FLD_Mese  =  5
031600050513     c                   add       FLD_NumSped   dcmNSP05
031700050513     c                   add       RIC_ReaNetto  dcmRIC05
031800050513     c                   add       VAL_Delta     dcmDEL05
031900050513     c                   end
032000050513      *  giu
032100050513     c                   if                FLD_Mese  =  6
032200050513     c                   add       FLD_NumSped   dcmNSP06
032300050513     c                   add       RIC_ReaNetto  dcmRIC06
032400050513     c                   add       VAL_Delta     dcmDEL06
032500050513     c                   end
032600050513      *  lug
032700050513     c                   if                FLD_Mese  =  7
032800050513     c                   add       FLD_NumSped   dcmNSP07
032900050513     c                   add       RIC_ReaNetto  dcmRIC07
033000050513     c                   add       VAL_Delta     dcmDEL07
033100050513     c                   end
033200050513      *  ago
033300050513     c                   if                FLD_Mese  =  8
033400050513     c                   add       FLD_NumSped   dcmNSP08
033500050513     c                   add       RIC_ReaNetto  dcmRIC08
033600050513     c                   add       VAL_Delta     dcmDEL08
033700050513     c                   end
033800050513      *  set
033900050513     c                   if                FLD_Mese  =  9
034000050513     c                   add       FLD_NumSped   dcmNSP09
034100050513     c                   add       RIC_ReaNetto  dcmRIC09
034200050513     c                   add       VAL_Delta     dcmDEL09
034300050513     c                   end
034400050513      *  ott
034500050513     c                   if                FLD_Mese  = 10
034600050513     c                   add       FLD_NumSped   dcmNSP10
034700050513     c                   add       RIC_ReaNetto  dcmRIC10
034800050513     c                   add       VAL_Delta     dcmDEL10
034900050513     c                   end
035000050513      *  nov
035100050513     c                   if                FLD_Mese  = 11
035200050513     c                   add       FLD_NumSped   dcmNSP11
035300050513     c                   add       RIC_ReaNetto  dcmRIC11
035400050513     c                   add       VAL_Delta     dcmDEL11
035500050513     c                   end
035600050513      *  dic
035700050513     c                   if                FLD_Mese  = 12
035800050513     c                   add       FLD_NumSped   dcmNSP12
035900050513     c                   add       RIC_ReaNetto  dcmRIC12
036000050513     c                   add       VAL_Delta     dcmDEL12
036100050513     c                   end
036200050513      *  Totale Progressivo
036300050513      *      TOT_RicRPre
036400050513     c                   add       FLD_NumSped   dcmNSPto
036500050513     c                   add       RIC_ReaNetto  dcmRICto
036600050513     c                   add       FLD_ImpRPre   TOT_RicRPre
036700050513      * ***  Costruisce il Delta del Totale
036800050513     C                   eval      WrrW = dcmRICto                              *RICAVO REALE
036900050513     C                   eval      WrpW = TOT_RicRPre                           *RICAVO PRESUNTO
037000050513     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
037100050513     c                   exsr      CalDelta
037200050513     C                   z-add     WdpW          dcmDELto                       *Delta
037300050513      *
037400050513     c                   eNd
037500050513      *
037600050513     c                   endsr
037700050513     ?****************************************************************************
037800050513      *?   CALdelta - CALCOLA IL DELTA
037900050513     C*          INPUT  --> WRRW: RICAVO REALE
038000050513     C*                     WRPW: RICAVO PRESUNTO
038100050513     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
038200050513     ?****************************************************************************
038300050513     C     CALDelta      BEGSR
038400050513     C*--------------------
038500050513     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
038600050513     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
038700050513     C*--------------------
038800050513     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
038900050513     C     WRRW          SUB       WRPW          GAP                            (REALE - PRESUNTO)
039000050513     C     GAP           MULT      100           GAP                            (GAP x 100)
039100050513     C*
039200050513     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
039300050513IF  1C     WRRW          IFEQ      *ZEROS
039400050513     C                   Z-ADD     1             WRRW
039500050513E   1C                   ENDIF
039600050513     C*
039700050513     C     GAP           DIV       WRRW          GAP
039800050513     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
039900050513     C*
040000050513     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
040100050513     C     WDPW          COMP      0                                    28
040200050513     C* POSITIVO
040300050513IF  1C     *IN28         IFEQ      *OFF
040400050513IF  2C     GAP           IFGE      1000
040500050513     C                   Z-ADD     +999,9        WDPW
040600050513E   2C                   ENDIF
040700050513     C* NEGATIVO
040800050513X   1C                   ELSE
040900050513IF  2C     GAP           IFLE      -1000
041000050513     C                   Z-ADD     -999,9        WDPW
041100050513E   2C                   ENDIF
041200050513E   1C                   ENDIF
041300050513     C*
041400050513     C                   ENDSR
041500050513     ?****************************************************************************
041600050513      *?   Totali da  File di Work
041700050513     ?****************************************************************************
041800050513     c     Totalizza     begsr
041900050513      * ?--
042000050513     c                   if        almeno_uno = *blank and se81LST = 'S'
042100050513     c                   exsr      PRT_NO_Dati
042200050513     c                   endIF
042300050513      *
042400050513     c                   if        almeno_uno = 'S' and se81LST = 'S'
042500050513      *   Stampo dettaglio ultimo Cliente Precedente
042600050513     c                   exsr      PRT_rig_sav
042700050513     c                   endIF
042800050513     c*
042900050513      * ?--
043000050510     c                   endsr
043100050511     ?****************************************************************************
043200050513      *?   Stampa      Riga dati di Totale Cliente precedente
043300050511     ?****************************************************************************
043400050513     c     PRT_rig_sav   begsr
043500050511      *
043600050511     c                   if        *in66
043700050511      *                 * ------------------ *
043800050511     c                   write     testa
043900050511      *                 * ------------------ *
044000050511     c                   setoff                                       66
044100050511     c                   end
044200050511      *
044300050513     c                   eval      PDist   = DcmDIV
044400050513     C                   eval      PArea   = DcmARE
044500050513     C                   eval      PPOper  = DcmFIL
044600050513     c                   eval      PCodCli = DcmKSC
044700050513     c                   eval      PDesCli = DcmCLD
044800050513     c                   move      '   '         PDesCli
044900050513     c                   move      DcmCLV        PDesCli
045000050513     c                   eval      PNumSPe = DcmNSPTO
045100050513     c                   eval      PRicNet = DcmRICTO
045200050513     c                   eval      PDelta  = DcmDELTO
045300050511      *
045400050511      *                 * ------------------ *
045500050511     c                   write     dettagli
045600050511      *                 * ------------------ *
045700050511      *
045800050511     c                   endsr
045900050511     ?****************************************************************************
046000050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
046100050511     ?****************************************************************************
046200050511     c     PRT_NO_Dati   begsr
046300050511      *
046400050511     c                   if        *in66
046500050511      *                 * ------------------ *
046600050511     c                   write     testa
046700050511      *                 * ------------------ *
046800050511     c                   setoff                                       66
046900050511     c                   end
047000050511      *
047100050511      *                 * ------------------ *
047200050511     c                   write     nodati
047300050511      *                 * ------------------ *
047400050511      *
047500050511     c                   endsr
047600050511     ?****************************************************************************
047700050511      *?   INIZIALIZZAZIONE
047800050511     ?****************************************************************************
047900040820     C     *inzsr        BEGSR
048000040820      **
048100040820     C     *ENTRY        PLIST
048200040820     C                   PARM                    KPJBA
048300050513     c                   movel     kpjbu         DsPassaggio
048400050511      **
048500050511     C                   Z-ADD     1             CODUT
048600050511     C                   CALL      'X§PARUT'
048700050511     C                   PARM                    UT§DSE
048800050511     C                   MOVEL     RAGUT         RSUT             20
048900041103      *
049000040820      *  imposta UDATE di elaborazione
049100040820     C                   move      *date         G02DAT
049200040820     C                   MOVE      *ZEROS        G02INV
049300040820     C                   MOVE      *BLANKS       G02ERR
049400040820     C                   CALL      'XSRDA8'
049500040820     C                   PARM                    WLBDAT
049600040820     C                   Z-ADD     G02INV        data_oggi
049700050513     C                   Z-ADD     G02INV        DATCOR            8 0          *DATA (8) IN AA/M/G
049800050513     C* IMNPOSTA DATA / ORA CORRENTE
049900050511     C                   move      G02dat        day
050000050513     C                   TIME                    W0140            14 0          *ORA (6)+ DATA (8)
050100050511     C                   MOVEl     W0140         ore
050200040823      **
050300050513     c     se81ANN       sub       1             annPRE            4 0
050400050513     c                   movel     se81ANN       DataINI           6 0
050500050513     c                   move      '01'          DataINI
050600050513     c                   movel     se81ANN       DataFIN           6 0
050700050513     c                   move      se81MES       DataFIN
050800050513     c                   move      DataINI       PDadata
050900050513     c                   move      DataFIN       Padata
051000040823      **
051100050511     c                   seton                                        66
051200040825     c                   clear                   prima_volta       1
051300040826     c                   clear                   almeno_uno
051400050511     C                   MOVEL     DSPGM         NOMPGM
051500050513     C*--------------------
051600050513     C* ORGANIGRAMMA
051700050513     C*--------------------
051800050513     C                   Z-ADD     *ZEROS        F                 3 0
051900050513     C                   Z-ADD     *ZEROS        korFIL            3 0
052000050513     C     keyORG        SETLL     AZorg01L
052100050513     C                   READ      AZorg01L                               99
052200050513DO  1C     *IN99         DOWEQ     '0'
052300050513IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
052400050513IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
052500050513     C                   ADD       1             F
052600050513     C                   Z-ADD     orgFIL        FIL(F)
052700050513     C                   MOVEL     orgDES        DFIL(F)
052800050513     C                   Z-ADD     1             tblKUT
052900050513     C                   MOVEL     '05'          tblCOD
053000050513     C                   MOVEL     *BLANKS       tblKEY
053100050513     C                   MOVEL     orgCAR        tblKEY
053200050513     C     keyTAB        CHAIN     TABEL00F                           99
053300050513IF  4C     *IN99         IFEQ      *OFF
053400050513     C                   Z-ADD     orgCAR        ARE(F)
053500050513     C                   MOVEL     TBLUNI        DARE(F)
053600050513E   4C                   ENDIF
053700050513     C                   Z-ADD     1             tblKUT
053800050513     C                   MOVEL     '17'          tblCOD
053900050513     C                   MOVEL     *BLANKS       tblKEY
054000050513     C                   MOVEL     orgFL3        tblKEY
054100050513     C     KEYTAB        CHAIN     TABEL00F                           99
054200050513IF  4C     *IN99         IFEQ      *OFF
054300050513     C                   MOVEL     orgFL3        DIV(F)
054400050513     C                   MOVEL     TBLUNI        DDIV(F)
054500050513E   4C                   ENDIF
054600050513E   3C                   ENDIF
054700050513E   2C                   ENDIF
054800050513     C                   READ      AZorg01L                               99
054900050513E   1C                   ENDDO
055000050513     C*
055100050513     C* CHIAVE LETTURA AZORG01L - COMPLETA
055200050513     C     keyORG        KLIST
055300050513     C                   KFLD                    korFIL                         *FILIALE
055400050513     C*
055500050513     C* CHIAVE LETTURA TABEL00F - COMPLETA
055600050513     C     keyTAB        KLIST
055700050513     C                   KFLD                    tblKUT                         *CODICE UTENTE
055800050513     C                   KFLD                    tblCOD                         *CODICE TABELLA
055900050513     C                   KFLD                    tblKEY                         *CHIAVE TABELLA
056000050513     C*
056100050513     C                   ENDSR
056200050513     ?****************************************************************************
