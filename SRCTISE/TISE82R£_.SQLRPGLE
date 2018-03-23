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
002400050517      *------------------
002500050517     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
002600050517     D*------------------
002700050517     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
002800050517     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
002900050517     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
003000050517     D*------------------
003100050517     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
003200050517     D*------------------
003300050517     D DS2W          E DS
003400050517     D*------------------
003500050512     D DsPassaggio   e DS                  EXTNAME(Tise81ds)
003600050512     D tise800f      e DS
003700050517     D sisdc00f      e DS
003800040820     D*
003900050513     D*------------------
004000040820     D WLBDAT          DS
004100040820     D  G02DAT                 1      8  0
004200040820     D  G02INV                 9     16  0
004300040820     D  G02ERR                17     17
004400040820     D  G02TGI                18     22  0
004500050513     D*------------------
004600050513     D* ORGANIGRAMMA
004700050513     D*------------------
004800050513     D FIL             S              3  0 DIM(500)                             *FILIALI
004900050513     D DFIL            S             20    DIM(500)
005000050513     D ARE             S              3  0 DIM(500)                             *AREE
005100050513     D DARE            S             20    DIM(500)
005200050513     D DIV             S              1    DIM(500)                             *DIVISIONI
005300050513     D DDIV            S             20    DIM(500)
005400040820      *
005500040820     D  data_Oggi      s                   like(G02INV) INZ(0)
005600040826     D  almeno_uno     s              1
005700050517     D  reck_OK        s              1
005800040820      *
005900050513     D  FLD_AAAAmm     s              6  0
006000050517     D  FLD_Anno       s                   like(sdcann)
006100050517     D  FLD_Mese       s                   like(sdcmes)
006200050517     D  FLD_Cliente    s                   like(sdcksc)
006300050517     D  FLD_NumSped    s                   like(sdcnsp)
006400050517     D  FLD_ImpRBol    s                   like(sdcirb)
006500050517     D  FLD_ImpRRet    s                   like(sdcirr)
006600050517     D  FLD_ImpNotC    s                   like(sdcinc)
006700050517     D  FLD_ImpRPre    s                   like(sdcirp)
006800050517     D  FLD_ImpRDel    s                   like(sdcird)
006900050517     D  FLD_RBol       s                   like(sdcrbl)
007000050513     D  FLD_Fil        s                   like(orgfil)
007100050517      *
007200050517     D  DEL_ImpRRet    s                   like(sdcirr)
007300050517     D  DEL_ImpNotC    s                   like(sdcinc)
007400050513      *
007500050513     D  RIC_ReaNetto   s                   like(WrrW)
007600050517     D  RIC_ReaPres    s                   like(WrpW)
007700050513     D  VAL_Delta      s                   like(WdpW)
007800050513      *
007900050513     D  TOT_RicRPre    s                   like(WrpW)
008000050513     D  SAV_Cliente    s                   like(FLD_Cliente)
008100050510      *
008200050513     D  WrrW           s             17  5
008300050513     D  WrpW           s             17  5
008400050513     D  WdpW           s              4  1
008500050513     D  Gap            s             17  5
008600050511     D*-------------
008700050511     D* Reperimento nome PGM
008800050511     D STATUS         SDS           333
008900050511     D  DSPGM            *PROC
009000050511     D*-------------
009100050511      *
009200040820     D digits          C                   CONST('0123456789')
009300050511      * ?------------------------------------------------------------------ */
009400050513      * ? Sulle Testate dei Saldi si rilevano i dati di periodo anno/mese:
009500050513      * ? il num.totale delle spedizioni, tot ricavi presunti ed i tot ricavi
009600050512      * ? reali. Il delta è ricavato con la semplice formula:
009700050512      * ? DELTA = (Ric.Reale - Ric.Presunto) x 100/ Ric.Reale
009800050512      * ?
009900050513      * ? i dati vengono filtrati in base al file guida TISE800F in cui sono
010000050513      * ? impostati i codici clienti da prendere in considerazione.
010100050513      * ?
010200050513      * ? A totale si eseguono i totali anno con il relativo delta
010300050513      **  Il File è organizzato x Cliente
010400050511      * ?------------------------------------------------------------------ */
010500050510     C/EXEC SQL
010600050510     C+ Declare A1 Cursor for
010700050517     C+ SELECT decimal((sdcANN*100+sdcMES), 6, 0), sdcANN, sdcMES,
010800050517     C+  sdcKSC, sdcNSP, sdcIRB, sdcIRR, sdcINC, sdcIRP, sdcIRD,
010900050517     C+  decimal(sdcKSC/10000, 3, 0), sdcRBL
011000050517     C+ FROM SIsdc00F                     WHERE
011100050517     C+  (sdcANN*100+sdcMES) between :DataINI and :DataFIN
011200050517     C+  and sdcKSC in
011300050513     C+ (SELECT SE8COD FROM tise800f)
011400050517     C+ ORDER BY sdcKSC, sdcANN, sdcMES
011500050510     C/END-EXEC
011600050510      *          apertura cursore
011700050510     C/EXEC SQL
011800050510     C+ OPEN A1
011900050510     C/END-EXEC
012000050510     c                   clear                   almeno_uno
012100050510      **
012200050510     C                   DOU       SqlCod <> 0
012300050510      **         lettura cursore
012400050510     C/EXEC SQL
012500050513     C+ FETCH NEXT FROM A1 INTO :FLD_AAAAmm, :FLD_Anno, :FLD_Mese,
012600050513     C+ :FLD_Cliente, :FLD_NumSped, :FLD_ImpRBol, :FLD_ImpRRet,
012700050517     C+ :FLD_ImpNotC, :FLD_ImpRPre, :FLD_ImpRDel, :FLD_Fil, :FLD_RBol
012800050510     C/END-EXEC
012900050510     C                   SELECT
013000050510     **
013100050510     C                   WHEN      SqlCod = 100
013200050510     c* a fine file
013300050510     c                   if        almeno_uno = 'S'
013400050510     c                   else
013500050510     c                   end
013600050510     c                   leave
013700050510     c*
013800050510     C                   WHEN      SqlCod < 0
013900050510     C                   seton                                        H1
014000050510     c                   goto      fine
014100050510     c*
014200050510     C                   OTHER
014300050510     c* DETTAGLIO
014400050510     c* riga   dettaglio
014500050517     c                   move      'S'           Reck_OK
014600050517     c                   exsr      Chk_Bolla
014700050517      **
014800050517     c                   if        reck_ok = 'S'
014900050513     c                   exsr      WRI_rig_det
015000050513     c                   move      'S'           almeno_uno
015100050517     c                   end
015200050517      **
015300050510     C                   ENDSL
015400050513      **
015500050510     C                   ENDDO
015600050510     C/EXEC SQL
015700050510     C+ Close A1
015800050510     C/END-EXEC
015900050513      **
016000050513      ** Totali
016100050510     c                   exsr      totalizza
016200050510     C* - - - - - - - - - - - - - - - - - - - - - - - - - -
016300041007     c     fine          tag
016400020326     C                   SETON                                        LR
016500050511     ?****************************************************************************
016600050517      *?    Controlla se deve considerare il dettaglio in esame
016700050511     ?****************************************************************************
016800050517     c     Chk_Bolla     begsr
016900050510      *
017000050517     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
017100050517     C* PER IL CONTEGGIO DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
017200050517     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
017300050517     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
017400050517     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
017500050517     C*
017600050517     C* MI SALVO LE NOTE E LE RETTIFICHE PER il "DELTA" NEL CASO VENGANO AZZ.TE
017700050517     C                   Z-ADD     FLD_ImpRRet   DEL_ImpRRet                    RETTIFICHE
017800050517     C                   Z-ADD     FLD_ImpNotC   DEL_ImpNotC                    NOTE ACCREDITO
017900050517     C*
018000050517     C                   Z-ADD     1             JI                3 0
018100050517     C                   SETOFF                                           28
018200050517     C     FLD_RBol      LOOKUP    FLAB(JI)                               28
018300050517     C*
018400050517     C* Bolla NON VALIDA come NR.Spedizioni
018500050517IF  1C     FSP(JI)       IFNE      'S'
018600050517     C                   Z-ADD     *ZEROS        FLD_NumSped                    *NUMERO SPEDIZIONI
018700050517E   1C                   ENDIF
018800050517     C*
018900050517     C* Bolla NON VALIDA come RICAVO
019000050517IF  1C     FRI(JI)       IFNE      'S'
019100050517     C                   Z-ADD     *ZEROS        FLD_ImpRBol                    RICAVO REALE BOLLE
019200050517     C                   Z-ADD     *ZEROS        FLD_ImpRRet                    RETTIFICHE
019300050517     C                   Z-ADD     *ZEROS        FLD_ImpNotC                    NOTE ACCREDITO
019400050517E   1C                   ENDIF
019500050517     C*
019600050517     C* RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
019700050517     C                   Z-ADD     *ZEROS        RICavo_TOT       16 3
019800050517     C     FLD_ImpRBol   ADD       FLD_ImpRRet   RICavo_TOT
019900050517     C     FLD_ImpNotC   ADD       RICavo_TOT    RICavo_TOT                     *RICAVO TOTALE
020000050517     C*
020100050517     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
020200050517     C                   Z-ADD     *ZEROS        RICavo_Delta     16 3
020300050517     C     FLD_ImpRDel   ADD       DEL_ImpRRet   RICavo_Delta
020400050517     C     DEL_ImpNotC   ADD       RICavo_Delta  RICavo_Delta                   *RICAVO REALE DELTA
020500050517      **
020600050517     c                   endsr
020700050517     ?****************************************************************************
020800050517      *?    Scrive Riga File di Work
020900050517     ?****************************************************************************
021000050517     c     Wri_rig_det   begsr
021100050517      *
021200050513      * ? x rottura di Codice Cliente (Pulizia campi x Totale Cliente)
021300050513     c                   iF        FLD_Cliente <> SAV_Cliente
021400050513     c                   eXSr      NEW_Cliente
021500050513     c                   EvaL      SAV_Cliente = FLD_Cliente
021600050513     c                   EnD
021700050513      *
021800050513      * ? x Codice Cliente
021900050517     c     FLD_Cliente   chain     wftise82f
022000050513      *
022100050517     c                   iF        %Found(wftise82f)
022200050513      * ? Valori di Periodo
022300050513     c                   exsr      in_Periodo
022400050513      * ? aggiorna
022500050513     c                   update    WFDCM000
022600050513      *
022700050513     c                   eLSe
022800050513      *
022900050513     c                   clear                   WFdcm000
023000050513      * ? Decodifiche Angrafiche
023100050513     c                   exsr      Decod_ANA
023200050513      * ? Valori di Periodo
023300050513     c                   exsr      in_Periodo
023400050513      * ? scrive
023500050513     c                   write     WFDCM000
023600050513      *
023700050513     c                   eNd
023800050513      *
023900050513     c                   endsr
024000050513     ?****************************************************************************
024100050513      *?   A Rottura di Codice Cliente - Operazioni a Totale Cliente Precedente
024200050513     ?****************************************************************************
024300050513     c     NEW_Cliente   begsr
024400050513      *
024500050513      *  Pulisce Totale Ricavo Presunto dell'Anno (poichè non è sul WrkFile)
024600050513      *   deve memorizzarlo per poter ricavare il Delta Totale sui 12 mesi
024700050513      *
024800050513     c                   EvaL      TOT_RicRPre = 0
024900050510      *
025000050513     c                   if        almeno_uno = 'S' and se81LST = 'S'
025100050513      *   Stampo dettaglio Cliente Precedente
025200050513     c                   exsr      PRT_rig_sav
025300050513     c                   endIF
025400050513     c*
025500050513      *
025600050510     c                   endsr
025700050511     ?****************************************************************************
025800050513      *?   Decodifiche ANAGRAFICHE/Tabelle
025900050511     ?****************************************************************************
026000050513     c     Decod_ANA     begsr
026100050513      *
026200050513     c                   eval      dcmANN      = se81Ann
026300050513     c                   eval      dcmMES      = se81Mes
026400050513     c                   eval      dcmFIL      = FLD_Fil
026500050513     c                   eval      dcmKSC      = FLD_Cliente
026600050510      *
026700050513     C                   CLEAR                   BS69DS
026800050513     C                   CLEAR                   ACODS
026900050513     C                   MOVEL     KNSIF         I69SIF
027000050513     C                   MOVEL     dcmKSC        I69KAC
027100050513     C                   MOVEL     dcmKSC        I69KCP
027200050513     C                   CALL      'TIBS69R'
027300050513     C                   PARM                    BS69DS
027400050513     C                   PARM                    ACODS
027500050513     C                   PARM                    INDDS
027600050513     C                   PARM                    CLPDS
027700050513     C                   PARM                    CLSDS
027800050513     c*
027900050513     C     O69ERR        IFne      '1'
028000050513     c                   movel     acorag        dcmCLD
028100050513     c                   movel     clpclv        dcmCLV
028200050513     C                   End
028300050513     c*
028400050513     C                   CLEAR                   BS10DS
028500050513     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
028600050513     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
028700050513     C                   MOVEL     'P'           D10PAF                         *CERCA PADRE
028800050513     C                   MOVE      dcmKSC        D10COD
028900050513     C                   CALL      'TIBS10R'
029000050513     C                   PARM                    BS10DS
029100050513     c*
029200050513     C     D10ERR        IFne      *BLANKS
029300050513     C                   CLEAR                   BS10DS
029400050513     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
029500050513     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
029600050513     C                   MOVEL     'F'           D10PAF                         *CERCA PADRE
029700050513     C                   MOVE      dcmKSC        D10COD
029800050513     C                   CALL      'TIBS10R'
029900050513     C                   PARM                    BS10DS
030000050513     c                   enD
030100050513      **
030200050513     C     D10ERR        IFeq      *BLANKS
030300050513     c                   z-add     d10cop        dcmKSU
030400050513     C                   CLEAR                   BS69DS
030500050513     C                   CLEAR                   ACODS
030600050513     C                   MOVEL     KNSIF         I69SIF
030700050513     C                   MOVEL     dcmKSU        I69KAC
030800050513     C                   CALL      'TIBS69R'
030900050513     C                   PARM                    BS69DS
031000050513     C                   PARM                    ACODS
031100050513     C                   PARM                    INDDS
031200050513     C                   PARM                    CLPDS
031300050513     C                   PARM                    CLSDS
031400050513     c* Descrizione Cliente Uificante se c'è......
031500050513     C     O69ERR        IFne      '1'
031600050513     c                   movel     acorag        dcmDKS
031700050513     c                   endIF
031800050513     c                   endIF
031900050513     C*
032000050513     C* REPERISCO FILIALE/AREA/DIVISIONE DEL CLIENTE
032100050513     C                   Z-ADD     1             F
032200050513     C     dcmFIL        LOOKUP    FIL(F)                                 99
032300050513IF  1C     *IN99         IFEQ      *ON
032400050513     C                   movel     dFIL(F)       dcmFID
032500050513     C                   Z-ADD     ARE(F)        dcmARE
032600050513     C                   movel     dARE(F)       dcmARD
032700050513     C                   MOVEL     DIV(F)        dcmDIV
032800050513     C                   movel     dDIV(F)       dcmDID
032900050513E   1C                   ENDIF
033000050513      *
033100050513     c                   endsr
033200050513     ?****************************************************************************
033300050513      *?   Valori attribuiti al periodo Anno-Mese o Anno Precedente
033400050513     ?****************************************************************************
033500050513     c     in_Periodo    begsr
033600050513      *
033700050513     ?* ***  Ricavo Reale Netto = (Ric.Reale Delta + Note + Rettifiche)  ***
033800050517     c                   eval      RIC_ReaNetto = RICavo_Delta
033900050517     c                   eval      RIC_ReaPres  = FLD_ImpRPre
034000050517     ?* ***  Regola del Delta
034100050517     ?* ***     (RICAVO REALE - RICAVO PRESUNTO) = GAP
034200050517     ?* ***     (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
034300050513      *
034400050513      *  Anno Precedente .............
034500050513     c                   iF        FLD_Anno = annPRE
034600050513      *  Non prendere in considerazione.......
034700050513      **
034800050513     c                   eLSe
034900050513      *  gen
035000050513     c                   if                FLD_Mese  =  1
035100050513     c                   add       FLD_NumSped   dcmNSP01
035200050513     c                   add       RIC_ReaNetto  dcmRIC01
035300050517     c                   add       RIC_ReaPres   dcmRPR01
035400050517     C                   eval      WrrW = dcmRIC01                              *RICAVO REALE
035500050517     C                   eval      WrpW = dcmRPR01                              *RICAVO PRESUNTO
035600050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
035700050517     c                   exsr      CalDelta
035800050517     C                   eval      VAL_delta = WdpW                             *Delta
035900050517     c                   z-add     VAL_Delta     dcmDEL01
036000050513     c                   end
036100050513      *  feb
036200050513     c                   if                FLD_Mese  =  2
036300050513     c                   add       FLD_NumSped   dcmNSP02
036400050513     c                   add       RIC_ReaNetto  dcmRIC02
036500050517     c                   add       RIC_ReaPres   dcmRPR02
036600050517     C                   eval      WrrW = dcmRIC02                              *RICAVO REALE
036700050517     C                   eval      WrpW = dcmRPR02                              *RICAVO PRESUNTO
036800050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
036900050517     c                   exsr      CalDelta
037000050517     C                   eval      VAL_delta = WdpW                             *Delta
037100050517     c                   z-add     VAL_Delta     dcmDEL02
037200050513     c                   end
037300050513      *  mar
037400050513     c                   if                FLD_Mese  =  3
037500050513     c                   add       FLD_NumSped   dcmNSP03
037600050513     c                   add       RIC_ReaNetto  dcmRIC03
037700050517     c                   add       RIC_ReaPres   dcmRPR03
037800050517     C                   eval      WrrW = dcmRIC03                              *RICAVO REALE
037900050517     C                   eval      WrpW = dcmRPR03                              *RICAVO PRESUNTO
038000050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
038100050517     c                   exsr      CalDelta
038200050517     C                   eval      VAL_delta = WdpW                             *Delta
038300050517     c                   z-add     VAL_Delta     dcmDEL03
038400050513     c                   end
038500050513      *  apr
038600050513     c                   if                FLD_Mese  =  4
038700050513     c                   add       FLD_NumSped   dcmNSP04
038800050513     c                   add       RIC_ReaNetto  dcmRIC04
038900050517     c                   add       RIC_ReaPres   dcmRPR04
039000050517     C                   eval      WrrW = dcmRIC04                              *RICAVO REALE
039100050517     C                   eval      WrpW = dcmRPR04                              *RICAVO PRESUNTO
039200050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
039300050517     c                   exsr      CalDelta
039400050517     C                   eval      VAL_delta = WdpW                             *Delta
039500050517     c                   z-add     VAL_Delta     dcmDEL04
039600050513     c                   end
039700050513      *  mag
039800050513     c                   if                FLD_Mese  =  5
039900050513     c                   add       FLD_NumSped   dcmNSP05
040000050513     c                   add       RIC_ReaNetto  dcmRIC05
040100050517     c                   add       RIC_ReaPres   dcmRPR05
040200050517     C                   eval      WrrW = dcmRIC05                              *RICAVO REALE
040300050517     C                   eval      WrpW = dcmRPR05                              *RICAVO PRESUNTO
040400050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
040500050517     c                   exsr      CalDelta
040600050517     C                   eval      VAL_delta = WdpW                             *Delta
040700050517     c                   z-add     VAL_Delta     dcmDEL05
040800050513     c                   end
040900050513      *  giu
041000050513     c                   if                FLD_Mese  =  6
041100050513     c                   add       FLD_NumSped   dcmNSP06
041200050513     c                   add       RIC_ReaNetto  dcmRIC06
041300050517     c                   add       RIC_ReaPres   dcmRPR06
041400050517     C                   eval      WrrW = dcmRIC06                              *RICAVO REALE
041500050517     C                   eval      WrpW = dcmRPR06                              *RICAVO PRESUNTO
041600050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
041700050517     c                   exsr      CalDelta
041800050517     C                   eval      VAL_delta = WdpW                             *Delta
041900050517     c                   z-add     VAL_Delta     dcmDEL06
042000050513     c                   end
042100050513      *  lug
042200050513     c                   if                FLD_Mese  =  7
042300050513     c                   add       FLD_NumSped   dcmNSP07
042400050513     c                   add       RIC_ReaNetto  dcmRIC07
042500050517     c                   add       RIC_ReaPres   dcmRPR07
042600050517     C                   eval      WrrW = dcmRIC07                              *RICAVO REALE
042700050517     C                   eval      WrpW = dcmRPR07                              *RICAVO PRESUNTO
042800050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
042900050517     c                   exsr      CalDelta
043000050517     C                   eval      VAL_delta = WdpW                             *Delta
043100050517     c                   z-add     VAL_Delta     dcmDEL07
043200050513     c                   end
043300050513      *  ago
043400050513     c                   if                FLD_Mese  =  8
043500050513     c                   add       FLD_NumSped   dcmNSP08
043600050513     c                   add       RIC_ReaNetto  dcmRIC08
043700050517     c                   add       RIC_ReaPres   dcmRPR08
043800050517     C                   eval      WrrW = dcmRIC08                              *RICAVO REALE
043900050517     C                   eval      WrpW = dcmRPR08                              *RICAVO PRESUNTO
044000050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
044100050517     c                   exsr      CalDelta
044200050517     C                   eval      VAL_delta = WdpW                             *Delta
044300050517     c                   z-add     VAL_Delta     dcmDEL08
044400050513     c                   end
044500050513      *  set
044600050513     c                   if                FLD_Mese  =  9
044700050513     c                   add       FLD_NumSped   dcmNSP09
044800050513     c                   add       RIC_ReaNetto  dcmRIC09
044900050517     c                   add       RIC_ReaPres   dcmRPR09
045000050517     C                   eval      WrrW = dcmRIC09                              *RICAVO REALE
045100050517     C                   eval      WrpW = dcmRPR09                              *RICAVO PRESUNTO
045200050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
045300050517     c                   exsr      CalDelta
045400050517     C                   eval      VAL_delta = WdpW                             *Delta
045500050517     c                   z-add     VAL_Delta     dcmDEL09
045600050513     c                   end
045700050513      *  ott
045800050513     c                   if                FLD_Mese  = 10
045900050513     c                   add       FLD_NumSped   dcmNSP10
046000050513     c                   add       RIC_ReaNetto  dcmRIC10
046100050517     c                   add       RIC_ReaPres   dcmRPR10
046200050517     C                   eval      WrrW = dcmRIC10                              *RICAVO REALE
046300050517     C                   eval      WrpW = dcmRPR10                              *RICAVO PRESUNTO
046400050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
046500050517     c                   exsr      CalDelta
046600050517     C                   eval      VAL_delta = WdpW                             *Delta
046700050517     c                   z-add     VAL_Delta     dcmDEL10
046800050513     c                   end
046900050513      *  nov
047000050513     c                   if                FLD_Mese  = 11
047100050513     c                   add       FLD_NumSped   dcmNSP11
047200050513     c                   add       RIC_ReaNetto  dcmRIC11
047300050517     c                   add       RIC_ReaPres   dcmRPR11
047400050517     C                   eval      WrrW = dcmRIC11                              *RICAVO REALE
047500050517     C                   eval      WrpW = dcmRPR11                              *RICAVO PRESUNTO
047600050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
047700050517     c                   exsr      CalDelta
047800050517     C                   eval      VAL_delta = WdpW                             *Delta
047900050517     c                   z-add     VAL_Delta     dcmDEL11
048000050513     c                   end
048100050513      *  dic
048200050513     c                   if                FLD_Mese  = 12
048300050513     c                   add       FLD_NumSped   dcmNSP12
048400050513     c                   add       RIC_ReaNetto  dcmRIC12
048500050517     c                   add       RIC_ReaPres   dcmRPR12
048600050517     C                   eval      WrrW = dcmRIC12                              *RICAVO REALE
048700050517     C                   eval      WrpW = dcmRPR12                              *RICAVO PRESUNTO
048800050517     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
048900050517     c                   exsr      CalDelta
049000050517     C                   eval      VAL_delta = WdpW                             *Delta
049100050517     c                   z-add     VAL_Delta     dcmDEL12
049200050513     c                   end
049300050513      *  Totale Progressivo
049400050513      *      TOT_RicRPre
049500050513     c                   add       FLD_NumSped   dcmNSPto
049600050513     c                   add       RIC_ReaNetto  dcmRICto
049700050517     c                   add       RIC_ReaPres   dcmRPRto
049800050513     c                   add       FLD_ImpRPre   TOT_RicRPre
049900050513      * ***  Costruisce il Delta del Totale
050000050513     C                   eval      WrrW = dcmRICto                              *RICAVO REALE
050100050513     C                   eval      WrpW = TOT_RicRPre                           *RICAVO PRESUNTO
050200050513     C                   eval      WdpW = 0                                     *DELTA PERCENTUALE
050300050513     c                   exsr      CalDelta
050400050513     C                   z-add     WdpW          dcmDELto                       *Delta
050500050513      *
050600050513     c                   eNd
050700050513      *
050800050513     c                   endsr
050900050513     ?****************************************************************************
051000050513      *?   CALdelta - CALCOLA IL DELTA
051100050513     C*          INPUT  --> WRRW: RICAVO REALE
051200050513     C*                     WRPW: RICAVO PRESUNTO
051300050513     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
051400050513     ?****************************************************************************
051500050513     C     CALDelta      BEGSR
051600050513     C*--------------------
051700050513     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
051800050513     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
051900050513     C*--------------------
052000050513     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
052100050513     C     WRRW          SUB       WRPW          GAP                            (REALE - PRESUNTO)
052200050513     C     GAP           MULT      100           GAP                            (GAP x 100)
052300050513     C*
052400050513     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
052500050513IF  1C     WRRW          IFEQ      *ZEROS
052600050513     C                   Z-ADD     1             WRRW
052700050513E   1C                   ENDIF
052800050513     C*
052900050513     C     GAP           DIV       WRRW          GAP
053000050513     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
053100050513     C*
053200050513     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
053300050513     C     WDPW          COMP      0                                    28
053400050513     C* POSITIVO
053500050513IF  1C     *IN28         IFEQ      *OFF
053600050513IF  2C     GAP           IFGE      1000
053700050513     C                   Z-ADD     +999,9        WDPW
053800050513E   2C                   ENDIF
053900050513     C* NEGATIVO
054000050513X   1C                   ELSE
054100050513IF  2C     GAP           IFLE      -1000
054200050513     C                   Z-ADD     -999,9        WDPW
054300050513E   2C                   ENDIF
054400050513E   1C                   ENDIF
054500050513     C*
054600050513     C                   ENDSR
054700050513     ?****************************************************************************
054800050513      *?   Totali da  File di Work
054900050513     ?****************************************************************************
055000050513     c     Totalizza     begsr
055100050513      * ?--
055200050513     c                   if        almeno_uno = *blank and se81LST = 'S'
055300050513     c                   exsr      PRT_NO_Dati
055400050513     c                   endIF
055500050513      *
055600050513     c                   if        almeno_uno = 'S' and se81LST = 'S'
055700050513      *   Stampo dettaglio ultimo Cliente Precedente
055800050513     c                   exsr      PRT_rig_sav
055900050513     c                   endIF
056000050513     c*
056100050513      * ?--
056200050510     c                   endsr
056300050511     ?****************************************************************************
056400050513      *?   Stampa      Riga dati di Totale Cliente precedente
056500050511     ?****************************************************************************
056600050513     c     PRT_rig_sav   begsr
056700050511      *
056800050511     c                   if        *in66
056900050511      *                 * ------------------ *
057000050511     c                   write     testa
057100050511      *                 * ------------------ *
057200050511     c                   setoff                                       66
057300050511     c                   end
057400050511      *
057500050513     c                   eval      PDist   = DcmDIV
057600050513     C                   eval      PArea   = DcmARE
057700050513     C                   eval      PPOper  = DcmFIL
057800050513     c                   eval      PCodCli = DcmKSC
057900050513     c                   eval      PDesCli = DcmCLD
058000050513     c                   move      '   '         PDesCli
058100050513     c                   move      DcmCLV        PDesCli
058200050513     c                   eval      PNumSPe = DcmNSPTO
058300050513     c                   eval      PRicNet = DcmRICTO
058400050513     c                   eval      PDelta  = DcmDELTO
058500050511      *
058600050511      *                 * ------------------ *
058700050511     c                   write     dettagli
058800050511      *                 * ------------------ *
058900050511      *
059000050511     c                   endsr
059100050511     ?****************************************************************************
059200050511      *?   Stampa      Avviso NO DATI x la SELEZIONE
059300050511     ?****************************************************************************
059400050511     c     PRT_NO_Dati   begsr
059500050511      *
059600050511     c                   if        *in66
059700050511      *                 * ------------------ *
059800050511     c                   write     testa
059900050511      *                 * ------------------ *
060000050511     c                   setoff                                       66
060100050511     c                   end
060200050511      *
060300050511      *                 * ------------------ *
060400050511     c                   write     nodati
060500050511      *                 * ------------------ *
060600050511      *
060700050511     c                   endsr
060800050511     ?****************************************************************************
060900050511      *?   INIZIALIZZAZIONE
061000050511     ?****************************************************************************
061100040820     C     *inzsr        BEGSR
061200040820      **
061300040820     C     *ENTRY        PLIST
061400040820     C                   PARM                    KPJBA
061500050513     c                   movel     kpjbu         DsPassaggio
061600050511      **
061700050511     C                   Z-ADD     1             CODUT
061800050511     C                   CALL      'X§PARUT'
061900050511     C                   PARM                    UT§DSE
062000050511     C                   MOVEL     RAGUT         RSUT             20
062100041103      *
062200040820      *  imposta UDATE di elaborazione
062300040820     C                   move      *date         G02DAT
062400040820     C                   MOVE      *ZEROS        G02INV
062500040820     C                   MOVE      *BLANKS       G02ERR
062600040820     C                   CALL      'XSRDA8'
062700040820     C                   PARM                    WLBDAT
062800040820     C                   Z-ADD     G02INV        data_oggi
062900050513     C                   Z-ADD     G02INV        DATCOR            8 0          *DATA (8) IN AA/M/G
063000050513     C* IMNPOSTA DATA / ORA CORRENTE
063100050511     C                   move      G02dat        day
063200050513     C                   TIME                    W0140            14 0          *ORA (6)+ DATA (8)
063300050511     C                   MOVEl     W0140         ore
063400040823      **
063500050513     c     se81ANN       sub       1             annPRE            4 0
063600050513     c                   movel     se81ANN       DataINI           6 0
063700050513     c                   move      '01'          DataINI
063800050513     c                   movel     se81ANN       DataFIN           6 0
063900050513     c                   move      se81MES       DataFIN
064000050513     c                   move      DataINI       PDadata
064100050513     c                   move      DataFIN       Padata
064200040823      **
064300050511     c                   seton                                        66
064400040825     c                   clear                   prima_volta       1
064500040826     c                   clear                   almeno_uno
064600050511     C                   MOVEL     DSPGM         NOMPGM
064700050513     C*--------------------
064800050513     C* ORGANIGRAMMA
064900050513     C*--------------------
065000050513     C                   Z-ADD     *ZEROS        F                 3 0
065100050513     C                   Z-ADD     *ZEROS        korFIL            3 0
065200050513     C     keyORG        SETLL     AZorg01L
065300050513     C                   READ      AZorg01L                               99
065400050513DO  1C     *IN99         DOWEQ     '0'
065500050513IF  2C     orgFVA        IFEQ      *BLANKS                                      *NO ANNULLATI
065600050513IF  3C     orgFAG        IFNE      'V'                                          *FILIALE/AGENZIA
065700050513     C                   ADD       1             F
065800050513     C                   Z-ADD     orgFIL        FIL(F)
065900050513     C                   MOVEL     orgDES        DFIL(F)
066000050513     C                   Z-ADD     1             tblKUT
066100050513     C                   MOVEL     '05'          tblCOD
066200050513     C                   MOVEL     *BLANKS       tblKEY
066300050513     C                   MOVEL     orgCAR        tblKEY
066400050513     C     keyTAB        CHAIN     TABEL00F                           99
066500050513IF  4C     *IN99         IFEQ      *OFF
066600050513     C                   Z-ADD     orgCAR        ARE(F)
066700050513     C                   MOVEL     TBLUNI        DARE(F)
066800050513E   4C                   ENDIF
066900050513     C                   Z-ADD     1             tblKUT
067000050513     C                   MOVEL     '17'          tblCOD
067100050513     C                   MOVEL     *BLANKS       tblKEY
067200050513     C                   MOVEL     orgFL3        tblKEY
067300050513     C     KEYTAB        CHAIN     TABEL00F                           99
067400050513IF  4C     *IN99         IFEQ      *OFF
067500050513     C                   MOVEL     orgFL3        DIV(F)
067600050513     C                   MOVEL     TBLUNI        DDIV(F)
067700050513E   4C                   ENDIF
067800050513E   3C                   ENDIF
067900050513E   2C                   ENDIF
068000050513     C                   READ      AZorg01L                               99
068100050513E   1C                   ENDDO
068200050517      *
068300050517     C*-------------------------
068400050517     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
068500050517     C*-------------------------
068600050517     C                   MOVEL     '2W'          tblCOD
068700050517     C     KEYTA2        SETLL     TABEL00F
068800050517     C     KEYTA2        READE     TABEL00F                               99
068900050517     C*
069000050517     C                   Z-ADD     *ZEROS        G                 4 0
069100050517DO  1C     *IN99         DOWEQ     *OFF
069200050517IF  2C     TBLFLG        IFEQ      *BLANKS
069300050517     C                   ADD       1             G
069400050517     C                   MOVEL     TBLKEY        FLAB(G)
069500050517     C                   MOVEL     TBLUNI        DS2W
069600050517     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
069700050517     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
069800050517E   2C                   ENDIF
069900050517     C*
070000050517     C     KEYTA2        READE     TABEL00F                               99
070100050517E   1C                   ENDDO
070200050513     C*
070300050513     C* CHIAVE LETTURA AZORG01L - COMPLETA
070400050513     C     keyORG        KLIST
070500050513     C                   KFLD                    korFIL                         *FILIALE
070600050513     C*
070700050513     C* CHIAVE LETTURA TABEL00F - COMPLETA
070800050513     C     keyTAB        KLIST
070900050513     C                   KFLD                    tblKUT                         *CODICE UTENTE
071000050513     C                   KFLD                    tblCOD                         *CODICE TABELLA
071100050513     C                   KFLD                    tblKEY                         *CHIAVE TABELLA
071200050517     C*
071300050517     C* CHIAVE LETTURA TABEL00F - parziale
071400050517     C     keyTA2        KLIST
071500050517     C                   KFLD                    tblKUT                         *CODICE UTENTE
071600050517     C                   KFLD                    tblCOD                         *CODICE TABELLA
071700050513     C*
071800050513     C                   ENDSR
071900050513     ?****************************************************************************
