000100040820     H DECEDIT('0,') DATEDIT(*dmy.)
000200050510      **********************************************************************
000300050512     ftabel00f  if   E           K DISK
000400050513     fazorg01l  if   E           K DISK
000500050513      **
000600050517     fwftise82f uf a E           K DISK
000700050512     fTise81P   o    E             PRINTER OFLIND(*in66)
000800050513      **
000900050510      **-----------------------------------------------------------------***
001000040820     d KPJBA         E DS
001100050511     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001200110511     D CNCR42        E DS
001300050513     D*------------------
001400050513     D* DS REPERIMENTO DATI UTENTE
001500050513     D*-------------------
001600050513     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
001700050513     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
001800050513     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
001900050513     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
002000050513     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
002100050513     D*------------------
002200050513     D* DS "TIBS10R"
002300050513     D*------------------
002400050513     D BS10DS        E DS                  EXTNAME(TIBS10DS)
002500050517      *------------------
002600050517     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
002700050517     D*------------------
002800050517     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
002900050517     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
003000050517     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
003100050517     D*------------------
003200050517     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
003300050517     D*------------------
003400050517     D DS2W          E DS
003500050517     D*------------------
003600050512     D DsPassaggio   e DS                  EXTNAME(Tise81ds)
003700050512     D tise800f      e DS
003800050517     D sisdc00f      e DS
003900040820     D*
004000050513     D*------------------
004100040820     D WLBDAT          DS
004200040820     D  G02DAT                 1      8  0
004300040820     D  G02INV                 9     16  0
004400040820     D  G02ERR                17     17
004500040820     D  G02TGI                18     22  0
004600050513     D*------------------
004700050513     D* ORGANIGRAMMA
004800050513     D*------------------
004900050513     D FIL             S              3  0 DIM(500)                             *FILIALI
005000050513     D DFIL            S             20    DIM(500)
005100050513     D ARE             S              3  0 DIM(500)                             *AREE
005200050513     D DARE            S             20    DIM(500)
005300050513     D DIV             S              1    DIM(500)                             *DIVISIONI
005400050513     D DDIV            S             20    DIM(500)
005500040820      *
005600040820     D  data_Oggi      s                   like(G02INV) INZ(0)
005700040826     D  almeno_uno     s              1
005800050517     D  reck_OK        s              1
005900040820      *
006000050513     D  FLD_AAAAmm     s              6  0
006100050517     D  FLD_Anno       s                   like(sdcann)
006200050517     D  FLD_Mese       s                   like(sdcmes)
006300050517     D  FLD_Cliente    s                   like(sdcksc)
006400050517     D  FLD_NumSped    s                   like(sdcnsp)
006500050517     D  FLD_ImpRBol    s                   like(sdcirb)
006600050517     D  FLD_ImpRRet    s                   like(sdcirr)
006700050517     D  FLD_ImpNotC    s                   like(sdcinc)
006800050517     D  FLD_ImpRPre    s                   like(sdcirp)
006900050517     D  FLD_ImpRDel    s                   like(sdcird)
007000050517     D  FLD_RBol       s                   like(sdcrbl)
007100050513     D  FLD_Fil        s                   like(orgfil)
007200050517      *
007300050517     D  DEL_ImpRRet    s                   like(sdcirr)
007400050517     D  DEL_ImpNotC    s                   like(sdcinc)
007500050513      *
007600050513     D  RIC_ReaNetto   s                   like(WrrW)
007700050517     D  RIC_ReaPres    s                   like(WrpW)
007800050513     D  VAL_Delta      s                   like(WdpW)
007900050513      *
008000050513     D  TOT_RicRPre    s                   like(WrpW)
008100050513     D  SAV_Cliente    s                   like(FLD_Cliente)
008200050510      *
008300050513     D  WrrW           s             17  5
008400050513     D  WrpW           s             17  5
008500050513     D  WdpW           s              4  1
008600050513     D  Gap            s             17  5
008700050511     D*-------------
008800050511     D* Reperimento nome PGM
008900050511     D STATUS         SDS           333
009000050511     D  DSPGM            *PROC
009100050511     D*-------------
009200050511      *
009300040820     D digits          C                   CONST('0123456789')
009400050511      * ?------------------------------------------------------------------ */
009500050513      * ? Sulle Testate dei Saldi si rilevano i dati di periodo anno/mese:
009600050513      * ? il num.totale delle spedizioni, tot ricavi presunti ed i tot ricavi
009700050512      * ? reali. Il delta è ricavato con la semplice formula:
009800050512      * ? DELTA = (Ric.Reale - Ric.Presunto) x 100/ Ric.Reale
009900050512      * ?
010000050513      * ? i dati vengono filtrati in base al file guida TISE800F in cui sono
010100050513      * ? impostati i codici clienti da prendere in considerazione.
010200050513      * ?
010300050513      * ? A totale si eseguono i totali anno con il relativo delta
010400050513      **  Il File è organizzato x Cliente
010500050511      * ?------------------------------------------------------------------ */
010600050510     C/EXEC SQL
010700050510     C+ Declare A1 Cursor for
010800050517     C+ SELECT decimal((sdcANN*100+sdcMES), 6, 0), sdcANN, sdcMES,
010900050517     C+  sdcKSC, sdcNSP, sdcIRB, sdcIRR, sdcINC, sdcIRP, sdcIRD,
011000050517     C+  decimal(sdcKSC/10000, 3, 0), sdcRBL
011100050517     C+ FROM SIsdc00F                     WHERE
011200050517     C+  (sdcANN*100+sdcMES) between :DataINI and :DataFIN
011300050517     C+  and sdcKSC in
011400050513     C+ (SELECT SE8COD FROM tise800f)
011500050517     C+ ORDER BY sdcKSC, sdcANN, sdcMES
011600050510     C/END-EXEC
011700050510      *          apertura cursore
011800050510     C/EXEC SQL
011900050510     C+ OPEN A1
012000050510     C/END-EXEC
012100050510     c                   clear                   almeno_uno
012200050510      **
012300050510     C                   DOU       SqlCod <> 0
012400050510      **         lettura cursore
012500050510     C/EXEC SQL
012600050513     C+ FETCH NEXT FROM A1 INTO :FLD_AAAAmm, :FLD_Anno, :FLD_Mese,
012700050513     C+ :FLD_Cliente, :FLD_NumSped, :FLD_ImpRBol, :FLD_ImpRRet,
012800050517     C+ :FLD_ImpNotC, :FLD_ImpRPre, :FLD_ImpRDel, :FLD_Fil, :FLD_RBol
012900050510     C/END-EXEC
013000050510     C                   SELECT
013100050510     **
013200050510     C                   WHEN      SqlCod = 100
013300050510     c* a fine file
013400050510     c                   if        almeno_uno = 'S'
013500050510     c                   else
013600050510     c                   end
013700050510     c                   leave
013800050510     c*
013900050510     C                   WHEN      SqlCod < 0
014000050510     C                   seton                                        H1
014100050510     c                   goto      fine
014200050510     c*
014300050510     C                   OTHER
014400050510     c* DETTAGLIO
014500050510     c* riga   dettaglio
014600050517     c                   exsr      Chk_Bolla
014700050517      **
014800050513     c                   exsr      WRI_rig_det
014900050513     c                   move      'S'           almeno_uno
015000050517      **
015100050510     C                   ENDSL
015200050513      **
015300050510     C                   ENDDO
015400050510     C/EXEC SQL
015500050510     C+ Close A1
015600050510     C/END-EXEC
015700050513      **
015800050513      ** Totali
015900050510     c                   exsr      totalizza
016000050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
016100041007     c     fine          tag
016200020326     C                   SETON                                        LR
016300050511     ?****************************************************************************
016400050517      *?    Controlla se deve considerare il dettaglio in esame
016500050511     ?****************************************************************************
016600050517     c     Chk_Bolla     begsr
016700050510      *
016800050517     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
016900050517     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
017000050517     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
017100050517     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
017200050517     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
017300050517     C*
017400050517     C* MI SALVO LE NOTE E LE RETTIFICHE PER il "DELTA" NEL CASO VENGANO AZZ.TE
017500050517     C                   Z-ADD     FLD_ImpRRet   DEL_ImpRRet                    RETTIFICHE
017600050517     C                   Z-ADD     FLD_ImpNotC   DEL_ImpNotC                    NOTE ACCREDITO
017700050517     C*
017800050517     C                   Z-ADD     1             JI                3 0
017900050517     C                   SETOFF                                           28
018000050517     C     FLD_RBol      LOOKUP    FLAB(JI)                               28
018100050517     C*
018200050517     C* Bolla NON VALIDA come NR.Spedizioni
018300050517IF  1C     FSP(JI)       IFNE      'S'
018400050517     C                   Z-ADD     *ZEROS        FLD_NumSped                    *NUMERO SPEDIZIONI
018500050517E   1C                   ENDIF
018600050517     C*
018700050517     C* Bolla NON VALIDA come RICAVO
018800050517IF  1C     FRI(JI)       IFNE      'S'
018900050517     C                   Z-ADD     *ZEROS        FLD_ImpRBol                    RICAVO REALE BOLLE
019000050517     C                   Z-ADD     *ZEROS        FLD_ImpRRet                    RETTIFICHE
019100050517     C                   Z-ADD     *ZEROS        FLD_ImpNotC                    NOTE ACCREDITO
019200050517E   1C                   ENDIF
019300050517     C*
019400050517     C* RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
019500050517     C                   Z-ADD     *ZEROS        RICavo_TOT       16 3
019600050517     C     FLD_ImpRBol   ADD       FLD_ImpRRet   RICavo_TOT
019700050517     C     FLD_ImpNotC   ADD       RICavo_TOT    RICavo_TOT                     *RICAVO TOTALE
019800050517     C*
019900050517     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
020000050517     C                   Z-ADD     *ZEROS        RICavo_Delta     16 3
020100050517     C     FLD_ImpRDel   ADD       DEL_ImpRRet   RICavo_Delta
020200050517     C     DEL_ImpNotC   ADD       RICavo_Delta  RICavo_Delta                   *RICAVO REALE DELTA
020300050517      **
020400050517     c                   endsr
020500050517     ?****************************************************************************
020600050517      *?    Scrive Riga File di Work
020700050517     ?****************************************************************************
020800050517     c     Wri_rig_det   begsr
020900050517      *
021000050513      * ? x rottura di Codice Cliente (Pulizia campi x Totale Cliente)
021100050513     c                   iF        FLD_Cliente <> SAV_Cliente
021200050513     c                   eXSr      NEW_Cliente
021300050513     c                   EvaL      SAV_Cliente = FLD_Cliente
021400050513     c                   EnD
021500050513      *
021600050513      * ? x Codice Cliente
021700050517     c     FLD_Cliente   chain     wftise82f
021800050513      *
021900050517     c                   iF        %Found(wftise82f)
022000050513      * ? Valori di Periodo
022100050513     c                   exsr      in_Periodo
022200050513      * ? aggiorna
022300050513     c                   update    WFDCM000
022400050513      *
022500050513     c                   eLSe
022600050513      *
022700050513     c                   clear                   WFdcm000
022800050513      * ? Decodifiche Angrafiche
022900050513     c                   exsr      Decod_ANA
023000050513      * ? Valori di Periodo
023100050513     c                   exsr      in_Periodo
023200050513      * ? scrive
023300050513     c                   write     WFDCM000
023400050513      *
023500050513     c                   eNd
023600050517      *
023700050517      * ? Campi di stampa
023800050517     c                   eval      PDist   = DcmDIV
023900050517     C                   eval      PArea   = DcmARE
024000050517     C                   eval      PPOper  = DcmFIL
024100050517     c                   eval      PCodCli = DcmKSC
024200050517     c                   eval      PDesCli = DcmCLD
024300050517     c                   move      '   '         PDesCli
024400050517     c                   move      DcmCLV        PDesCli
024500050517     c                   eval      PNumSPe = DcmNSPTO
024600050517     c                   eval      PRicNet = DcmRICTO
024700050517     c                   eval      PDelta  = DcmDELTO
024800050517      *
024900050513     c                   endsr
025000050513     ?****************************************************************************
025100050513      *?   A Rottura di Codice Cliente - Operazioni a Totale Cliente Precedente
025200050513     ?****************************************************************************
025300050513     c     NEW_Cliente   begsr
025400050513      *
025500050513      *  Pulisce Totale Ricavo Presunto dell'Anno (poichè non è sul WrkFile)
025600050513      *   deve memorizzarlo per poter ricavare il Delta Totale sui 12 mesi
025700050513      *
025800050513     c                   EvaL      TOT_RicRPre = 0
025900050510      *
026000050513     c                   if        almeno_uno = 'S' and se81LST = 'S'
026100050513      *   Stampo dettaglio Cliente Precedente
026200050513     c                   exsr      PRT_rig_sav
026300050513     c                   endIF
026400050513     c*
026500050513      *
026600050510     c                   endsr
026700050511     ?****************************************************************************
026800050513      *?   Decodifiche ANAGRAFICHE/Tabelle
026900050511     ?****************************************************************************
027000050513     c     Decod_ANA     begsr
027100050513      *
027200050513     c                   eval      dcmANN      = se81Ann
027300050513     c                   eval      dcmMES      = se81Mes
027400050513     c                   eval      dcmFIL      = FLD_Fil
027500050513     c                   eval      dcmKSC      = FLD_Cliente
027600050510      *
027700050513     C                   CLEAR                   BS69DS
027800050513     C                   CLEAR                   ACODS
027900050513     C                   MOVEL     KNSIF         I69SIF
028000050513     C                   MOVEL     dcmKSC        I69KAC
028100050513     C                   MOVEL     dcmKSC        I69KCP
028200050513     C                   CALL      'TIBS69R'
028300050513     C                   PARM                    BS69DS
028400050513     C                   PARM                    ACODS
028500050513     C                   PARM                    INDDS
028600050513     C                   PARM                    CLPDS
028700050513     C                   PARM                    CLSDS
028800050513     c*
028900050513     C     O69ERR        IFne      '1'
029000050513     c                   movel     acorag        dcmCLD
029100050513     c                   movel     clpclv        dcmCLV
029200050513     C                   End
029300050513     c*
029400050513     C                   CLEAR                   BS10DS
029500050513     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
029600050513     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
029700050513     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
029800050513     C                   MOVE      dcmKSC        D10COD
029900050513     C                   CALL      'TIBS10R'
030000050513     C                   PARM                    BS10DS
030100050513     c*
030200050513     C     D10ERR        IFne      *BLANKS
030300050513     C                   CLEAR                   BS10DS
030400050513     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
030500050513     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
030600050513     C                   MOVEL     'F'           D10PAF                         *CERCA PADRE
030700050513     C                   MOVE      dcmKSC        D10COD
030800050513     C                   CALL      'TIBS10R'
030900050513     C                   PARM                    BS10DS
031000050513     c                   enD
031100050513      **
031200050513     C     D10ERR        IFeq      *BLANKS
031300050513     c                   z-add     d10cop        dcmKSU
031400050513     C                   CLEAR                   BS69DS
031500050513     C                   CLEAR                   ACODS
031600050513     C                   MOVEL     KNSIF         I69SIF
031700050513     C                   MOVEL     dcmKSU        I69KAC
031800050513     C                   CALL      'TIBS69R'
031900050513     C                   PARM                    BS69DS
032000050513     C                   PARM                    ACODS
032100050513     C                   PARM                    INDDS
032200050513     C                   PARM                    CLPDS
032300050513     C                   PARM                    CLSDS
032400050513     c* Descrizione Cliente Uificante se c'è......
032500050513     C     O69ERR        IFne      '1'
032600050513     c                   movel     acorag        dcmDKS
032700050513     c                   endIF
032800050513     c                   endIF
032900050513     C*
033000050513     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
033100050513     C                   Z-ADD     1             F
033200050513     C     dcmFIL        LOOKUP    FIL(F)                                 99
033300050513IF  1C     *IN99         IFEQ      *ON
033400050513     C                   movel     dFIL(F)       dcmFID
033500050513     C                   Z-ADD     ARE(F)        dcmARE
033600050513     C                   movel     dARE(F)       dcmARD
033700050513     C                   MOVEL     DIV(F)        dcmDIV
033800050513     C                   movel     dDIV(F)       dcmDID
033900050513E   1C                   ENDIF
034000050513      *
034100050513     c                   endsr
034200050513     ?****************************************************************************
034300050513      *?   Valori attribuiti al periodo Anno-Mese o Anno Precedente
034400050513     ?****************************************************************************
034500050513     c     in_Periodo    begsr
034600050513      *
034700050513     ?* ***  Ricavo Reale Netto = (Ric.Reale Delta + Note + Rettifiche)  ***
034800050517     c                   eval      RIC_ReaNetto = RICavo_Delta
034900050517     c                   eval      RIC_ReaPres  = FLD_ImpRPre
035000050517     ?* ***  Regola del Delta
035100050517     ?* ***     (RICAVO REALE - RICAVO PRESUNTO) = GAP
035200050517     ?* ***     (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
035300050513      *
035400050513      *  Anno Precedente .............
035500050513     c                   iF        FLD_Anno = annPRE
035600050513      *  Non prendere in considerazione.......
035700050513      **
035800050513     c                   eLSe
035900050513      *  gen
036000050513     c                   if                FLD_Mese  =  1
036100050513     c                   add       FLD_NumSped   dcmNSP01
036200050513     c                   add       RIC_ReaNetto  dcmRIC01
036300050517     c                   add       RIC_ReaPres   dcmRPR01
036400050517     C                   eval      WrrW = dcmRIC01                              *RICAVO REALE
036500050517     C                   eval      WrpW = dcmRPR01                              *RICAVO PRESUNTO
036600050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
036700050517     c                   exsr      CalDelta
036800050517     C                   eval      VAL_delta = WdpW                             *Delta
036900050517     c                   z-add     VAL_Delta     dcmDEL01
037000050513     c                   end
037100050513      *  feb
037200050513     c                   if                FLD_Mese  =  2
037300050513     c                   add       FLD_NumSped   dcmNSP02
037400050513     c                   add       RIC_ReaNetto  dcmRIC02
037500050517     c                   add       RIC_ReaPres   dcmRPR02
037600050517     C                   eval      WrrW = dcmRIC02                              *RICAVO REALE
037700050517     C                   eval      WrpW = dcmRPR02                              *RICAVO PRESUNTO
037800050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
037900050517     c                   exsr      CalDelta
038000050517     C                   eval      VAL_delta = WdpW                             *Delta
038100050517     c                   z-add     VAL_Delta     dcmDEL02
038200050513     c                   end
038300050513      *  mar
038400050513     c                   if                FLD_Mese  =  3
038500050513     c                   add       FLD_NumSped   dcmNSP03
038600050513     c                   add       RIC_ReaNetto  dcmRIC03
038700050517     c                   add       RIC_ReaPres   dcmRPR03
038800050517     C                   eval      WrrW = dcmRIC03                              *RICAVO REALE
038900050517     C                   eval      WrpW = dcmRPR03                              *RICAVO PRESUNTO
039000050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
039100050517     c                   exsr      CalDelta
039200050517     C                   eval      VAL_delta = WdpW                             *Delta
039300050517     c                   z-add     VAL_Delta     dcmDEL03
039400050513     c                   end
039500050513      *  apr
039600050513     c                   if                FLD_Mese  =  4
039700050513     c                   add       FLD_NumSped   dcmNSP04
039800050513     c                   add       RIC_ReaNetto  dcmRIC04
039900050517     c                   add       RIC_ReaPres   dcmRPR04
040000050517     C                   eval      WrrW = dcmRIC04                              *RICAVO REALE
040100050517     C                   eval      WrpW = dcmRPR04                              *RICAVO PRESUNTO
040200050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
040300050517     c                   exsr      CalDelta
040400050517     C                   eval      VAL_delta = WdpW                             *Delta
040500050517     c                   z-add     VAL_Delta     dcmDEL04
040600050513     c                   end
040700050513      *  mag
040800050513     c                   if                FLD_Mese  =  5
040900050513     c                   add       FLD_NumSped   dcmNSP05
041000050513     c                   add       RIC_ReaNetto  dcmRIC05
041100050517     c                   add       RIC_ReaPres   dcmRPR05
041200050517     C                   eval      WrrW = dcmRIC05                              *RICAVO REALE
041300050517     C                   eval      WrpW = dcmRPR05                              *RICAVO PRESUNTO
041400050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
041500050517     c                   exsr      CalDelta
041600050517     C                   eval      VAL_delta = WdpW                             *Delta
041700050517     c                   z-add     VAL_Delta     dcmDEL05
041800050513     c                   end
041900050513      *  giu
042000050513     c                   if                FLD_Mese  =  6
042100050513     c                   add       FLD_NumSped   dcmNSP06
042200050513     c                   add       RIC_ReaNetto  dcmRIC06
042300050517     c                   add       RIC_ReaPres   dcmRPR06
042400050517     C                   eval      WrrW = dcmRIC06                              *RICAVO REALE
042500050517     C                   eval      WrpW = dcmRPR06                              *RICAVO PRESUNTO
042600050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
042700050517     c                   exsr      CalDelta
042800050517     C                   eval      VAL_delta = WdpW                             *Delta
042900050517     c                   z-add     VAL_Delta     dcmDEL06
043000050513     c                   end
043100050513      *  lug
043200050513     c                   if                FLD_Mese  =  7
043300050513     c                   add       FLD_NumSped   dcmNSP07
043400050513     c                   add       RIC_ReaNetto  dcmRIC07
043500050517     c                   add       RIC_ReaPres   dcmRPR07
043600050517     C                   eval      WrrW = dcmRIC07                              *RICAVO REALE
043700050517     C                   eval      WrpW = dcmRPR07                              *RICAVO PRESUNTO
043800050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
043900050517     c                   exsr      CalDelta
044000050517     C                   eval      VAL_delta = WdpW                             *Delta
044100050517     c                   z-add     VAL_Delta     dcmDEL07
044200050513     c                   end
044300050513      *  ago
044400050513     c                   if                FLD_Mese  =  8
044500050513     c                   add       FLD_NumSped   dcmNSP08
044600050513     c                   add       RIC_ReaNetto  dcmRIC08
044700050517     c                   add       RIC_ReaPres   dcmRPR08
044800050517     C                   eval      WrrW = dcmRIC08                              *RICAVO REALE
044900050517     C                   eval      WrpW = dcmRPR08                              *RICAVO PRESUNTO
045000050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
045100050517     c                   exsr      CalDelta
045200050517     C                   eval      VAL_delta = WdpW                             *Delta
045300050517     c                   z-add     VAL_Delta     dcmDEL08
045400050513     c                   end
045500050513      *  set
045600050513     c                   if                FLD_Mese  =  9
045700050513     c                   add       FLD_NumSped   dcmNSP09
045800050513     c                   add       RIC_ReaNetto  dcmRIC09
045900050517     c                   add       RIC_ReaPres   dcmRPR09
046000050517     C                   eval      WrrW = dcmRIC09                              *RICAVO REALE
046100050517     C                   eval      WrpW = dcmRPR09                              *RICAVO PRESUNTO
046200050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
046300050517     c                   exsr      CalDelta
046400050517     C                   eval      VAL_delta = WdpW                             *Delta
046500050517     c                   z-add     VAL_Delta     dcmDEL09
046600050513     c                   end
046700050513      *  ott
046800050513     c                   if                FLD_Mese  = 10
046900050513     c                   add       FLD_NumSped   dcmNSP10
047000050513     c                   add       RIC_ReaNetto  dcmRIC10
047100050517     c                   add       RIC_ReaPres   dcmRPR10
047200050517     C                   eval      WrrW = dcmRIC10                              *RICAVO REALE
047300050517     C                   eval      WrpW = dcmRPR10                              *RICAVO PRESUNTO
047400050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
047500050517     c                   exsr      CalDelta
047600050517     C                   eval      VAL_delta = WdpW                             *Delta
047700050517     c                   z-add     VAL_Delta     dcmDEL10
047800050513     c                   end
047900050513      *  nov
048000050513     c                   if                FLD_Mese  = 11
048100050513     c                   add       FLD_NumSped   dcmNSP11
048200050513     c                   add       RIC_ReaNetto  dcmRIC11
048300050517     c                   add       RIC_ReaPres   dcmRPR11
048400050517     C                   eval      WrrW = dcmRIC11                              *RICAVO REALE
048500050517     C                   eval      WrpW = dcmRPR11                              *RICAVO PRESUNTO
048600050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
048700050517     c                   exsr      CalDelta
048800050517     C                   eval      VAL_delta = WdpW                             *Delta
048900050517     c                   z-add     VAL_Delta     dcmDEL11
049000050513     c                   end
049100050513      *  dic
049200050513     c                   if                FLD_Mese  = 12
049300050513     c                   add       FLD_NumSped   dcmNSP12
049400050513     c                   add       RIC_ReaNetto  dcmRIC12
049500050517     c                   add       RIC_ReaPres   dcmRPR12
049600050517     C                   eval      WrrW = dcmRIC12                              *RICAVO REALE
049700050517     C                   eval      WrpW = dcmRPR12                              *RICAVO PRESUNTO
049800050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
049900050517     c                   exsr      CalDelta
050000050517     C                   eval      VAL_delta = WdpW                             *Delta
050100050517     c                   z-add     VAL_Delta     dcmDEL12
050200050513     c                   end
050300050513      *  Totale Progressivo
050400050513      *      TOT_RicRPre
050500050513     c                   add       FLD_NumSped   dcmNSPto
050600050513     c                   add       RIC_ReaNetto  dcmRICto
050700050517     c                   add       RIC_ReaPres   dcmRPRto
050800050513     c                   add       FLD_ImpRPre   TOT_RicRPre
050900050513      * ***  Costruisce il Delta del Totale
051000050513     C                   eval      WrrW = dcmRICto                              *RICAVO REALE
051100050513     C                   eval      WrpW = TOT_RicRPre                           *RICAVO PRESUNTO
051200050513     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
051300050513     c                   exsr      CalDelta
051400050513     C                   z-add     WdpW          dcmDELto                       *Delta
051500050513      *
051600050513     c                   eNd
051700050513      *
051800050513     c                   endsr
051900050513     ?****************************************************************************
052000050513      *?   CALdelta - CALCOLA IL DELTA
052100050513     C*          INPUT  --> WRRW: RICAVO REALE
052200050513     C*                     WRPW: RICAVO PRESUNTO
052300050513     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
052400050513     ?****************************************************************************
052500050513     C     CALDelta      BEGSR
052600050513     C*--------------------
052700050513     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
052800050513     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
052900050513     C*--------------------
053000050513     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
053100050513     C     WRRW          SUB       WRPW          GAP                            (REALE - PRESUNTO)
053200050513     C     GAP           MULT      100           GAP                            (GAP x 100)
053300050513     C*
053400050513     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
053500050513IF  1C     WRRW          IFEQ      *ZEROS
053600050513     C                   Z-ADD     1             WRRW
053700050513E   1C                   ENDIF
053800050513     C*
053900050513     C     GAP           DIV       WRRW          GAP
054000050513     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
054100050513     C*
054200050513     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
054300050513     C     WDPW          COMP      0                                    28
054400050513     C* POSITIVO
054500050513IF  1C     *IN28         IFEQ      *OFF
054600050513IF  2C     GAP           IFGE      1000
054700050513     C                   Z-ADD     +999,9        WDPW
054800050513E   2C                   ENDIF
054900050513     C* NEGATIVO
055000050513X   1C                   ELSE
055100050513IF  2C     GAP           IFLE      -1000
055200050513     C                   Z-ADD     -999,9        WDPW
055300050513E   2C                   ENDIF
055400050513E   1C                   ENDIF
055500050513     C*
055600050513     C                   ENDSR
055700050513     ?****************************************************************************
055800050513      *?   Totali da  File di Work
055900050513     ?****************************************************************************
056000050513     c     Totalizza     begsr
056100050513      * ?--
056200050513     c                   if        almeno_uno = *blank and se81LST = 'S'
056300050513     c                   exsr      PRT_NO_Dati
056400050513     c                   endIF
056500050513      *
056600050513     c                   if        almeno_uno = 'S' and se81LST = 'S'
056700050513      *   Stampo dettaglio ultimo Cliente Precedente
056800050513     c                   exsr      PRT_rig_sav
056900050513     c                   endIF
057000050513     c*
057100050513      * ?--
057200050510     c                   endsr
057300050511     ?****************************************************************************
057400050513      *?   Stampa      Riga dati di Totale Cliente precedente
057500050511     ?****************************************************************************
057600050513     c     PRT_rig_sav   begsr
057700050511      *
057800050511     c                   if        *in66
057900050511      *                 * ------------------ *
058000050511     c                   write     testa
058100050511      *                 * ------------------ *
058200050511     c                   setoff                                       66
058300050511     c                   end
058400050511      *
058500050511      *                 * ------------------ *
058600050511     c                   write     dettagli
058700050511      *                 * ------------------ *
058800050511      *
058900050511     c                   endsr
059000050511     ?****************************************************************************
059100050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
059200050511     ?****************************************************************************
059300050511     c     PRT_NO_Dati   begsr
059400050511      *
059500050511     c                   if        *in66
059600050511      *                 * ------------------ *
059700050511     c                   write     testa
059800050511      *                 * ------------------ *
059900050511     c                   setoff                                       66
060000050511     c                   end
060100050511      *
060200050511      *                 * ------------------ *
060300050511     c                   write     nodati
060400050511      *                 * ------------------ *
060500050511      *
060600050511     c                   endsr
060700050511     ?****************************************************************************
060800050511      *?   INIZIALIZZAZIONE
060900050511     ?****************************************************************************
061000040820     C     *inzsr        BEGSR
061100040820      **
061200040820     C     *ENTRY        PLIST
061300040820     C                   PARM                    KPJBA
061400050513     c                   movel     kpjbu         DsPassaggio
061500050511      **
061600050511     C                   Z-ADD     1             CODUT
061700050511     C                   CALL      'X§PARUT'
061800050511     C                   PARM                    UT§DSE
061900110511     C*****              MOVEL     RAGUT         RSUT             20
062000110511     C                   MOVEL     REC42         CNCR42
062100110511     C                   MOVEL     simRGS        RSUT             20
062200041103      *
062300040820      *  imposta UDATE di elaborazione
062400040820     C                   move      *date         G02DAT
062500040820     C                   MOVE      *ZEROS        G02INV
062600040820     C                   MOVE      *BLANKS       G02ERR
062700040820     C                   CALL      'XSRDA8'
062800040820     C                   PARM                    WLBDAT
062900040820     C                   Z-ADD     G02INV        data_oggi
063000050513     C                   Z-ADD     G02INV        DATCOR            8 0          *DATA (8) IN AA/M/G
063100050513     C* IMNPOSTA DATA / ORA CORRENTE
063200050511     C                   move      G02dat        day
063300050513     C                   TIME                    W0140            14 0          *ORA (6)+ DATA (8)
063400050511     C                   MOVEl     W0140         ore
063500040823      **
063600050513     c     se81ANN       sub       1             annPRE            4 0
063700050513     c                   movel     se81ANN       DataINI           6 0
063800050513     c                   move      '01'          DataINI
063900050513     c                   movel     se81ANN       DataFIN           6 0
064000050513     c                   move      se81MES       DataFIN
064100050513     c                   move      DataINI       PDadata
064200050513     c                   move      DataFIN       Padata
064300040823      **
064400050511     c                   seton                                        66
064500040825     c                   clear                   prima_volta       1
064600040826     c                   clear                   almeno_uno
064700050511     C                   MOVEL     DSPGM         NOMPGM
064800050513     C*--------------------
064900050513     C* ORGANIGRAMMA
065000050513     C*--------------------
065100050513     C                   Z-ADD     *ZEROS        F                 3 0
065200050513     C                   Z-ADD     *ZEROS        korFIL            3 0
065300050513     C     keyORG        SETLL     AZorg01L
065400050513     C                   READ      AZorg01L                               99
065500050513DO  1C     *IN99         DOWEQ     '0'
065600050513IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
065700050513IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
065800050513     C                   ADD       1             F
065900050513     C                   Z-ADD     orgFIL        FIL(F)
066000050513     C                   MOVEL     orgDES        DFIL(F)
066100050513     C                   Z-ADD     1             tblKUT
066200050513     C                   MOVEL     '05'          tblCOD
066300050513     C                   MOVEL     *BLANKS       tblKEY
066400050513     C                   MOVEL     orgCAR        tblKEY
066500050513     C     keyTAB        CHAIN     TABEL00F                           99
066600050513IF  4C     *IN99         IFEQ      *OFF
066700050513     C                   Z-ADD     orgCAR        ARE(F)
066800050513     C                   MOVEL     TBLUNI        DARE(F)
066900050513E   4C                   ENDIF
067000050513     C                   Z-ADD     1             tblKUT
067100050513     C                   MOVEL     '17'          tblCOD
067200050513     C                   MOVEL     *BLANKS       tblKEY
067300050513     C                   MOVEL     orgFL3        tblKEY
067400050513     C     KEYTAB        CHAIN     TABEL00F                           99
067500050513IF  4C     *IN99         IFEQ      *OFF
067600050513     C                   MOVEL     orgFL3        DIV(F)
067700050513     C                   MOVEL     TBLUNI        DDIV(F)
067800050513E   4C                   ENDIF
067900050513E   3C                   ENDIF
068000050513E   2C                   ENDIF
068100050513     C                   READ      AZorg01L                               99
068200050513E   1C                   ENDDO
068300050517      *
068400050517     C*-------------------------
068500050517     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
068600050517     C*-------------------------
068700050517     C                   MOVEL     '2W'          tblCOD
068800050517     C     KEYTA2        SETLL     TABEL00F
068900050517     C     KEYTA2        READE     TABEL00F                               99
069000050517     C*
069100050517     C                   Z-ADD     *ZEROS        G                 4 0
069200050517DO  1C     *IN99         DOWEQ     *OFF
069300050517IF  2C     TBLFLG        IFEQ      *BLANKS
069400050517     C                   ADD       1             G
069500050517     C                   MOVEL     TBLKEY        FLAB(G)
069600050517     C                   MOVEL     TBLUNI        DS2W
069700050517     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
069800050517     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
069900050517E   2C                   ENDIF
070000050517     C*
070100050517     C     KEYTA2        READE     TABEL00F                               99
070200050517E   1C                   ENDDO
070300050513     C*
070400050513     C* CHIAVE LETTURA AZORG01L - COMPLETA
070500050513     C     keyORG        KLIST
070600050513     C                   KFLD                    korFIL                         *FILIALE
070700050513     C*
070800050513     C* CHIAVE LETTURA TABEL00F - COMPLETA
070900050513     C     keyTAB        KLIST
071000050513     C                   KFLD                    tblKUT                         *CODICE UTENTE
071100050513     C                   KFLD                    tblCOD                         *CODICE TABELLA
071200050513     C                   KFLD                    tblKEY                         *CHIAVE TABELLA
071300050517     C*
071400050517     C* CHIAVE LETTURA TABEL00F - parziale
071500050517     C     keyTA2        KLIST
071600050517     C                   KFLD                    tblKUT                         *CODICE UTENTE
071700050517     C                   KFLD                    tblCOD                         *CODICE TABELLA
071800050513     C*
071900050513     C                   ENDSR
072000050513     ?****************************************************************************
