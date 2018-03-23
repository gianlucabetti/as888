000100060626     H DFTACTGRP(*NO) BNDDIR('TIS':'TIBS')
000200030620
000300030623     FFnOrm01L  IF   E           K DISK
000400030620     FTiTa432C  IF   E           K DISK
000500030716
000600030620     D Eseguito        C                   '0'
000700030620     D Errore          C                   '1'
000800030620     D Elementi        C                   50
000900030623     D Singola         C                   'S'
001000030623     D Multipla        C                   'M'
001100090216     D ORM_FASE_IMMESSO_E_CONFERMATO...
001200090216     D                 S                   LIKE(orfFar)
001300090216     D                                     INZ(100)
001400060626
001500060626     D GetSpeChkCde    PR            10U 0
001600060626     D  Anno                          4
001700060626     D                                     VALUE
001800060626     D  IdSpedizione                 12
001900060626     D                                     VALUE
002000060626     D  ChkCode                      10U 0
002100060626     D  nEsito                        5I 0
002200060626     D inzLingue       PR            10I 0
002300060626     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
002400060626     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
002500060626     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
002600060626     D cvtLinguaISO2ToTntbe...
002700060626     D                 PR                  LIKE(linTntbe)
002800060626     D  rqsISO2                            LIKE(linISO2)
002900060626     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
003000060626     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
003100060626     D  rpyTntbe                           LIKE(linTntbe)
003200060626     D                                     OPTIONS(*NOPASS:*OMIT)
003300060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
003400060626     D  kpjba                              LIKEDS(kpjba)
003500060626     D  tibs02ds                           LIKEDS(tibs02ds)
003600060626
003700060626     D DCMR          E DS                  INZ
003800060626     D DFAR          E DS                  INZ
003900030620     D FnOrf00F      E DS
004000030620     D                                     BASED(NullPtr)
004100030620     D AzOrg00F      E DS
004200030620     D                                     BASED(NullPtr)
004300060626     D azlin00f      E DS
004400060626     D                                     BASED(NullPtr)
004500060626     D kpjba         E DS
004600060626     D                                     INZ
004700060626     D tibs02ds      E DS                  INZ
004800060626     D  t02Mod       E                     INZ('C')                             Controllo
004900060626
005000030620     D TIO1G3IDS     E DS
005100030620     D                                     PREFIX(Inp)
005200030711     D  InpPoe                        3S 0
005300030711     D                                     OVERLAY(InpOrm:1)
005400030711     D  InpNsr                        2S 0
005500030711     D                                     OVERLAY(InpOrm:4)
005600030711     D  InpNor                        7S 0
005700030714     D                                     OVERLAY(InpOrm:6)
005800030711     D  InpNrv                        2S 0
005900030714     D                                     OVERLAY(InpOrm:13)
006000030620
006100030620     D TIO1G3ODS     E DS
006200030620     D                                     PREFIX(Out)
006300030620     D  FARDES                       35
006400030620     D                                     DIM(Elementi)
006500030620     D                                     OVERLAY(OutFarDes)
006600030620     D  DAE                          10
006700030620     D                                     DIM(Elementi)
006800030620     D                                     OVERLAY(OutDae)
006900030620     D  ORE                           5
007000030620     D                                     DIM(Elementi)
007100030620     D                                     OVERLAY(OutOre)
007200030620     D  POGDES                       20
007300030620     D                                     DIM(Elementi)
007400030620     D                                     OVERLAY(OutPogDes)
007500030716     D  CARDES                       35
007600030716     D                                     DIM(Elementi)
007700030716     D                                     OVERLAY(OutCarDes)
007800030620
007900030620     D WrkRowFasi      DS
008000030620     D                                     OCCURS(Elementi)
008100030620     D  RowPoe
008200030620     D                                     LIKE(OrfPoe)
008300030620     D  RowNsr
008400030620     D                                     LIKE(OrfNsr)
008500030620     D  RowNor
008600030620     D                                     LIKE(OrfNor)
008700030620     D  RowNrv
008800030620     D                                     LIKE(OrfNrv)
008900030620     D  RowDae
009000030620     D                                     LIKE(OrfDae)
009100030620     D  RowOre
009200030620     D                                     LIKE(OrfOre)
009300030716     D  RowCar
009400030716     D                                     LIKE(OrfCar)
009500030620     D  RowPogDes
009600030620     D                                     LIKE(OrgDes)
009700060626     D  RowFar
009800060626     D                                     LIKE(OrfFar)
009900030620
010000060626     D NullPtr         S               *
010100030620     D Esito           S              1
010200030620     D I               S              5I 0
010300030620     D WrkDatEur       S               D
010400030620     D                                     DATFMT(*EUR)
010500030620     D WrkTimEur       S               T
010600030620     D                                     TIMFMT(*EUR)
010700030620     D WrkTimChr       S              8
010800041223     D ChkCode         S             10U 0
010900041223     D nEsito          S              5I 0
011000060626     D langTntbe       S                   LIKE(linTntbe)
011100090216     D esisteFase100   S              1N
011200090218     D fase100Dae      S                   LIKE(orfDae)
011300090216
011400090216     C/EXEC SQL
011500090216     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
011600090216     C/END-EXEC
011700060626
011800030620     ***********************************************************************
011900030620     **
012000030620     ** Main
012100030620     **
012200030620     ***********************************************************************
012300030620     C     *ENTRY        PLIST
012400030709     C                   PARM                    Esito
012500030620     C                   PARM                    TIO1G3IDS
012600030620     C                   PARM                    TIO1G3ODS
012700030620
012800030620     C                   CLEAR                   TIO1G3ODS
012900030620     C                   CLEAR                   Esito
013000060626
013100030623     C                   EXSR      GetOrm
013200030620     C                   EXSR      GetTas
013300030620     C                   EXSR      GetFasi
013400030620     C                   EXSR      Uscita
013500030620
013600030620     ***********************************************************************
013700030620     **
013800030620     ** Reperisco i dati ORM.
013900030620     **
014000030620     ***********************************************************************
014100030623     C     GetOrm        BEGSR
014200030620
014300030623     C                   EVAL      OrmPoe = InpPoe
014400030623     C                   EVAL      OrmNsr = InpNsr
014500030623     C                   EVAL      OrmNor = InpNor
014600030623     C                   EVAL      OrmNrv = InpNrv
014700030620
014800030623     C     K04Orm01      CHAIN     FnOrm01L
014900030620
015000030620     C                   IF        NOT %FOUND
015100030620if  1C                   EVAL      Esito = Errore
015200030620     C                   EXSR      Uscita
015300030620     C                   ENDIF
015400030620
015500030716     C                   EVAL      OutRFA = OrmRFA
015600030623     C                   EVAL      OutRSR = OrmRSR
015700030623     C                   EVAL      OutINR = OrmINR
015800030623     C                   EVAL      OutCAR = OrmCAR
015900030623     C                   EVAL      OutLOR = OrmLOR
016000030623     C                   EVAL      OutPRR = OrmPRR
016100030623     C                   EVAL      OutNAR = OrmNAR
016200030623     C                   EVAL      OutPAG = OrmPAG
016300030620
016400030623     C                   IF        OrmDAR > 0
016500030623     C     *ISO          MOVE      OrmDAR        WrkDatEur
016600030620     C                   MOVE      WrkDatEur     OutDAR
016700030620     C                   ENDIF
016800030620
016900030716     C                   EVAL      OutORR = %EDITW(OrmORR:'  .  ')
017000030623     C                   EVAL      OutNAM = OrmNAM
017100030623     C                   EVAL      OutNCL = OrmNCL
017200030623     C                   EVAL      OutPKG = OrmPKG
017300030623     C                   EVAL      OutVLM = OrmVLM
017400030623     C                   EVAL      OutBNC = OrmBNC
017500030623     C                   EVAL      OutBLC = OrmBLC
017600030623     C                   EVAL      OutATT = OrmATT
017700030623     C                   EVAL      OutMTC = OrmMTC
017800030623     C                   EVAL      OutRER = OrmRER
017900030623     C                   EVAL      OutTER = OrmTER
018000030623     C                   EVAL      OutUPD = OrmTOR
018100030623     C                   EVAL      OutRSC = OrmRSC
018200030623     C                   EVAL      OutINC = OrmINC
018300030623     C                   EVAL      OutCAC = OrmCAC
018400030623     C                   EVAL      OutLOC = OrmLOC
018500030623     C                   EVAL      OutPRC = OrmPRC
018600030623     C                   EVAL      OutNAC = OrmNAC
018700030623     C                   EVAL      OutFFD = OrmFFD
018800030623     C*                  EVAL      OutOSE = '?'
018900030623     C                   EVAL      OutNO1 = OrmNO1
019000030623     C                   EVAL      OutNO2 = OrmNO2
019100030620
019200030620     C                   ENDSR
019300030620
019400030620     ***********************************************************************
019500030620     **
019600030620     ** Reperimento riferimento spedizione.
019700030620     **
019800030620     ***********************************************************************
019900030620     C     GetTas        BEGSR
020000030620
020100030620     C                   EVAL      TA4N14 = %EDITC(InpPoe:'X')
020200030620     C                                    + %EDITC(InpNsr:'X')
020300030620     C                                    + %EDITC(InpNor:'X')
020400030620     C                                    + %EDITC(InpNrv:'X')
020500030620
020600030620     C     K01Ta432      CHAIN     TiTa432C
020700030620
020800030620     ** Restituisco il numero spedizione se ce n'è solo una.
020900030620     C                   IF        %FOUND
021000030620     C                   EVAL      OutAAS = Ta4AAS
021100030620     C                   EVAL      OutLnP = Ta4LnP
021200030620     C                   EVAL      OutNrS = Ta4NrS
021300030620     C                   EVAL      OutNSp = Ta4NSp
021400030623     C                   EVAL      OutSpeSing = Singola
021500030620     C     K01Ta432      READE     TiTa432C
021600030620     C                   IF        NOT %EOF
021700030620     C                   CLEAR                   OutAAS
021800030620     C                   CLEAR                   OutLnP
021900030620     C                   CLEAR                   OutNrS
022000030620     C                   CLEAR                   OutNSp
022100030623     C                   EVAL      OutSpeSing = Multipla
022200030620     C                   ENDIF
022300041223     ** Calcolo il codice di controllo.
022400041223     C                   IF        OutNSp > 0
022500041223     C                   EVAL      OutSpeChkCde = GetSpeChkCde(
022600041223     C                             %EDITC(OutAAS:'X'):%EDITC(OutLnP:'X') +
022700041223     C                             %EDITC(OutNrS:'X') + %EDITC(OutNSp:'X'):
022800041223     C                             ChkCode:nEsito)
022900041223     C                   ENDIF
023000030620     C                   ENDIF
023100030620
023200030620     C                   ENDSR
023300030620
023400030620     ***********************************************************************
023500030620     **
023600030620     ** Reperisco le fasi.
023700030620     **
023800030620     ***********************************************************************
023900030620     C     GetFasi       BEGSR
024000030620
024100060626     C                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(inpLang)
024200090216     C                   RESET                   esisteFase100
024300090218     C                   RESET                   fase100Dae
024400060626
024500030620     C/EXEC SQL
024600060626     C+ DECLARE FASI CURSOR FOR SELECT ORFPOE, ORFNSR, ORFNOR, ORFNRV,
024700060626     C+ ORFDAE, ORFORE, ORFCAR, ORGDES AS PO, ORFFAR
024800060626     C+ FROM (FNORF00F JOIN TNTBE00F ON 'FAR' = TBECOD
024900060626     C+ AND DIGITS(ORFFAR) = TBEKE1 AND ' ' = TBEKE2 AND ' ' = TBELIN
025000060626     C+ AND ' ' = TBESIF) JOIN AZORG00F ON
025100060626     C+ ORFPOG = ORGFIL WHERE ORFPOE = :InpPoe AND ORFNSR = :InpNsr AND
025200060626     C+ ORFNOR = :InpNor AND ORFNRV = :InpNrv AND SUBSTR(TBEUNI, 43, 1) =
025300060626     C+ 'S' ORDER BY ORFPOE, ORFNSR, ORFNOR, ORFNRV, ORFDAE DESC, ORFORE
025400060626     C+ DESC FOR READ ONLY
025500030620     C/END-EXEC
025600030620
025700030620     C/EXEC SQL
025800030620     C+ OPEN FASI
025900030620     C/END-EXEC
026000030620
026100030620     C/EXEC SQL
026200030620     C+ FETCH NEXT FROM FASI FOR 50 ROWS INTO :WrkRowFasi
026300030620     C/END-EXEC
026400030620
026500030620     C                   SELECT
026600030620     C                   WHEN      SQLCod = 100
026700030620     C                   WHEN      SQLCod < 0
026800030620if  1C                   EVAL      Esito = Errore
026900030620     C                   OTHER
027000030620     ** A questo punto nella DS multipla WRKROWFASI ci possono essere
027100030620     ** da 0 a 50 ricorrenze. Il numero è contenuto nella variabile SQLER3.
027200030620     ** Scorro la DS multipla e riempio le schiere.
027300030620     C                   CLEAR                   I
027400030620     C                   DOW       I < SQLEr3
027500090216     C*
027600030620     C                   EVAL      I = I + 1
027700030620     C     I             OCCUR     WrkRowFasi
027800060626     C                   RESET                   tibs02ds
027900060626     C                   EVAL      t02Cod = 'FAR'
028000060626     C                   EVAL      t02Ke1 = %EDITC(rowFar:'X')
028100060626     C                   EXSR      getTntbe00f
028200090216     C
028300060626     C                   IF        d§FarDva <> *BLANK
028400060626     C                   EVAL      FarDes(I) = d§FarDva
028500060626     C                   ELSE
028600060626     C                   EVAL      FarDes(I) = d§FarDes
028700060626     C                   ENDIF
028800090216     C
028900030620     C                   EVAL      PogDes(I) = RowPogDes
029000030620     C     *ISO          MOVE      RowDae        WrkDatEur
029100030620     C                   MOVE      WrkDatEur     Dae(I)
029200030620     C     *ISO          MOVE      RowOre        WrkTimEur
029300030620     C                   MOVE      WrkTimEur     WrkTimChr
029400030620     C                   EVAL      Ore(I) = %SUBST(WrkTimChr:1)
029500090216     C
029600030716     c                   IF        RowCar <> *blanks
029700060626     C                   RESET                   tibs02ds
029800060626     C                   EVAL      t02Cod = 'CMR'
029900060626     C                   EVAL      t02Ke1 = rowCar
030000060626     C                   EXSR      getTntbe00f
030100060626     C                   EVAL      CarDes(I) = D§CMRDVA
030200030716     c                   ENDIF
030300090216     c
030400090216     C                   IF        rowFar = ORM_FASE_IMMESSO_E_CONFERMATO
030500090216     C                   EVAL      esisteFase100 = *ON
030600090216     C                   ENDIF
030700090216     c*
030800030620     C                   ENDDO
030900030620
031000030620     C                   EVAL      OutRigheFasi = SQLEr3
031100030620if  1C                   EVAL      Esito = Eseguito
031200030620
031300030620     C                   ENDSL
031400030620
031500030620     C/EXEC SQL
031600030620     C+ CLOSE FASI
031700030620     C/END-EXEC
031800030620
031900090216     C                   IF        esisteFase100
032000090216     C/EXEC SQL
032100090216     C+ SELECT ORAREF
032200090216     C+      , ORANOTE
032300090218     C+      , ORADAE
032400090218     C+ INTO :outFase100Ref
032500090218     C+    , :outFase100Not
032600090218     C+    , :fase100Dae
032700090216     C+ FROM FNORA00F
032800090216     C+ WHERE ORAPOE = :inpPoe
032900090216     C+   AND ORANSR = :inpNsr
033000090216     C+   AND ORANOR = :inpNor
033100090216     C+   AND ORANRV = :inpNrv
033200090216     C+   AND ORAFAR = :ORM_FASE_IMMESSO_E_CONFERMATO
033300090216     C+ ORDER BY ORAPOE
033400090216     C+        , ORANSR
033500090216     C+        , ORANOR
033600090216     C+        , ORANRV
033700090216     C+        , ORAFAR
033800090216     C+        , ORADAE DESC
033900090216     C+        , ORAORE DESC
034000090216     C+ FETCH FIRST ROW ONLY
034100090216     C/END-EXEC
034200090216     C                   IF        sqlCode < 0
034300090216     C                   DUMP(A)
034400090218     C                   ELSEIF    sqlCode <> 100
034500090218     C                   EVAL      outFase100Dae =
034600090218     C                                           %CHAR(%DATE(fase100Dae) : *EUR)
034700090216     C                   ENDIF
034800090216     C                   ENDIF
034900090216
035000030620     C                   ENDSR
035100030620
035200030620     ***********************************************************************
035300030620     **
035400030620     ** Operazioni iniziali.
035500030620     **
035600030620     ***********************************************************************
035700030620     C     *INZSR        BEGSR
035800030620
035900030623     C     K04Orm01      KLIST
036000030623     C                   KFLD                    OrmPoe
036100030623     C                   KFLD                    OrmNsr
036200030623     C                   KFLD                    OrmNor
036300030623     C                   KFLD                    OrmNrv
036400030620
036500030620     C     K01Ta432      KLIST
036600030620     C                   KFLD                    TA4N14
036700060626
036800060626     C                   CALLP         inzLingue()
036900060626
037000030620     C                   ENDSR
037100030620
037200060626     ***********************************************************************************************
037300060626     **
037400060626     ** Reperisco tabella da TNTBE00F.
037500060626     **
037600060626     ***********************************************************************************************
037700060626     C     getTntbe00f   BEGSR
037800060626     C                   EVAL      t02Lin = langTntbe
037900060626     C                   CALLP     tibs02r(kpjba:tibs02ds)
038000060626     C                   SELECT
038100060626     C                   WHEN      t02Cod = 'FAR'
038200060626     C                   EVAL      dfar = t02Uni
038300060626     C                   WHEN      t02Cod = 'CMR'
038400060626     C                   EVAL      dcmr = t02Uni
038500060626     C                   ENDSL
038600060626     C                   ENDSR
038700060626
038800030620     ***********************************************************************
038900030620     **
039000030620     ** Operazioni finali.
039100030620     **
039200030620     ***********************************************************************
039300030620     C     Uscita        BEGSR
039400030620
039500030620     C                   IF        Esito = *BLANK
039600030620     C                   EVAL      Esito = Eseguito
039700030620     C                   ENDIF
039800030620
039900030620     C                   RETURN
040000030620
040100030620     C                   ENDSR
