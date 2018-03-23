000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600161129     FTIVMS01I  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900161129
001000161129
001100161129     D*-----------
001200161129     D* SCHIERE
001300161129     D*-----------
001400161130     D NumElem         C                   CONST(300)
001500161130     D skCAU_BRT       S                   DIM(NumElem) LIKE(vmsCAUBRT)
001600161130     D skCAU_EVT       S                   DIM(NumElem) LIKE(dsCLI_EVT)
001700161130     D skCAU_RSN       S                   DIM(NumElem) LIKE(dsCLI_RSN)
001800161130     D skCAU_OPZ       S                   DIM(NumElem) LIKE(vmsSTSOPZ)
001900161130     D skCAU_DES       S                   DIM(NumElem) LIKE(vmsDESCRIZ)
002000161130     D jCAU            S              3  0 INZ(*zeros)
002100161130     D j               S              3  0 INZ(*zeros)
002200161129     D*-----------
002300161129     D* DS RIDEFINIZIONE STATUS CLIENTE
002400161129     D*-----------
002500161129     D DS_VMSSTSCLI    DS                  INZ
002600161129     D  dsCLI_EVT                    10
002700161129     D  dsCLI_RSN                     5
002800161129     D*-----------
002900170516
003000170516
003100170516     D*--------------------
003200170516     D* DEFINIZIONI A PROCEDURE ESTERNE
003300170516     D*--------------------
003400170516     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
003500141107
003600141107
003700141107     D*******
003800141107     D* DS CLIENTE
003900141107     D*******
004000141107     D*
004100141107     D* BGM
004200141107     D*
004300141107     D CLI_DS_BGM      DS           100
004400141107     D  CD_BGM_RECTYP          1      3A   inz('BGM')
004500141107     D  CD_BGM_DOCCOD          4      6A   inz('77')
004600141107     D  CD_BGM_DOCNAM          7     16A   inz('SHP_STATUS')
004700141107     D  CD_BGM_DOCREF         17     31A   inz(*all'?')
004800141107     D  CD_BGM_DOCVER         32     40A   inz('BRT_001')
004900141107     D  CD_BGM_DOCFUN         41     43A   inz('9')
005000141107     D  CD_BGM_DOCDAT         44     55A   inz(*all'?')
005100141107     D  CD_BGM_TOTSHP         91     95A   inz(*all'0')
005200141107     D  CD_BGM_TOTSTS         96    100A   inz(*all'0')
005300141107     D*
005400141107     D* NAD
005500141107     D*
005600141107     D CLI_DS_NAD      DS           113
005700141107     D  CD_NAD_RECTYP          1      3A   inz('NAD')
005800141107     D  CD_NAD_MSGMIT          4     38A   inz('BRT Corriere Espresso S.p.A.')
005900171123     D  CD_NAD_MITCOD         39     55A   inz('80052')
006000141107     D  CD_NAD_NAZID          56     58A   inz('IT')
006100141107     D  CD_NAD_MSGRCV         59     93A   inz('Ceva Logistics Italia Srl')
006200141107     D  CD_NAD_RCVCOD         94    110A   inz('1')
006300141107     D*
006400141107     D* CNI
006500141107     D*
006600141107     D CLI_DS_CNI      DS          1025
006700141107     D  CD_CNI_RECTYP          1      3A   inz('CNI')
006800141107     D  CD_CNI_SEQUEN          4      8A   inz(*all'?')
006900141107     D  CD_CNI_SHPRMN          9     43A   inz(*all'?')
007000171123     D  CD_CNI_LNPDES         61    316A   inz('043_BRT_VERONA_VR_37057_IT')
007100141107     D  CD_CNI_STSOWN        318    318A   inz('1')
007200141107     D  CD_CNI_STSCOD        319    321A   inz(*all'?')
007300141107     D  CD_CNI_STSDES        322    356A   inz(*all'?')
007400141107     D  CD_CNI_STSRSN        357    376A   inz(*all'?')
007500141107     D  CD_CNI_SHPRMA        412    428A   inz(*all'?')
007600141107     D  CD_CNI_SHPDSP        429    440A   inz(*all'?')
007700141107     D  CD_CNI_STSDAT        528    544A   inz(*all'?')
007800141107     D  CD_CNI_SHPOK         912    914A   inz(*all'?')
007900141107     D  CD_CNI_SHPRSD        915    949A   inz(*all'?')
008000141107     D  CD_CNI_FIRMA         950    984A   inz(*blanks)
008100141107     D  CD_CNI_SHPPKB        985    996A   inz(*all'0')
008200141107     D  CD_CNI_SHPVLB        997   1008A   inz(*all'0')
008300170516     D  CD_CNI_RGRSHP       1009   1025A   inz(*all'?')
008400141107     D*
008500030924
008600161129
008700161129
008800041015     D*-----------
008900041015     D* VARIABILI D WRK
009000041015     D*-----------
009100031103     D prmppi          S             50
009200030924     D prmesito        S              1
009300030924     D wrkesito        S                   like(prmesito)
009400131004     D campo_alfa      S            512
009500161129     D wStsEvt         S             25
009600161129     D wStsRsn         S             25
009700161130     D wStsOpz         S              5
009800161129     D wStsDes         S             35
009900141107     D Conta_msg       S             10i 0
010000141107     D Conta_segm      S             10i 0
010100141107     D wNum005_0       S              5  0
010200141107     D wNum010_0       S             10  0
010300131004
010400131004
010500131004     D*-----------
010600131004     D* DEFINIZIONE DS ESTERNE
010700131004     D*-----------
010800131004     D trul33ds      e ds                  inz
010900131004     D kpjba         e ds                  inz
011000131004     D DTAD          e ds                  inz
011100170516     D DTA4A         e ds                  inz
011200131004
011300131004
011400031103     D*-----------
011500031103     D* VARIABILI RIFERITE AL DATA-BASE
011600031103     D*-----------
011700080618     D wKstbKSU        S                   like(stbKSU)
011800080618     D wKstbFTR        S                   like(stbFTR)
011900030924
012000110705
012100031103
012200031103     D*-----------
012300031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
012400031103     D*-----------
012500031103     D DSPPI           DS
012600080618     D  w§PPIKSU                      7  0
012700161129     D  w§PPIVMSPROC                       like(vmsPROC) inz
012800161129     D  w§PPIVMSKSU                        like(vmsKSU)  inz
012900131004
013000141118
013100141118
013200141118     D*------------------
013300141118     D* LINKING A DEFINIZIONI ESTERNE
013400141118     D*------------------
013500141118     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
013600141118     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
013700161129     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
013800131004
013900030924
014000030325
014100030924     C* MAIN/
014200161129     C*
014300161129     C* Carica schiere di mappatura
014400161129     C                   SETOFF                                       40
014500161129     C                   EXSR      carMAP
014600161129     C*
014700161129     C* Se caricamento mappatura tutto OK => Procedo
014800161129     C                   IF        not *in40
014900140308     C*
015000140308     C* Spia dati da inviare
015100140308     C                   SETOFF                                       45
015200041015     C*
015300041015     C* Esegue traduzione
015400140308     C                   SETOFF                                       31
015500131004     C  N31              EXSR      chkRECFILMBR
015600131004     C  N31              EXSR      rtvID
015700131004     C  N31              EXSR      traduci
015800141107     C**  N31
015900141107     C**AN 45              EXSR      wriFooter
016000161129     C*
016100161129     C* Diversamente traduzioe completamente fallita
016200161129     C                   ELSE
016300161129     C                   EVAL      wrkesito = '2'
016400161129     C                   ENDIF
016500110705     C*
016600921023     C                   SETON                                        LR
016700131004
016800131004
016900131004
017000131004     C*----------------------------------------------------*
017100131004     C*  STACCO ID UNIVOCO
017200131004     C*----------------------------------------------------*
017300131004     C     RTVID         BEGSR
017400131004     C*
017500131004     C                   z-add     *zeros        currUNIQ_ID      15 0
017600131004     C*
017700131004     C* NSP => Stacco un numeratore da AZNUM
017800131004     C                   clear                   TRUL33DS
017900131004     C                   eval      I33OPE = *zeros
018000131004     C                   eval      I33CNU = 302
018100131004     C                   eval      I33NUM = 1
018200131004     C                   movel     TRUL33DS      KPJBU
018300131004     C                   call      'TRUL33R'
018400131004     C                   parm                    KPJBA
018500131004     C                   movel     KPJBU         TRUL33DS
018600131004     C                   if        O33ERR = *zeros
018700131004     C                   z-add     O33NRF        currUNIQ_ID
018800131004     C                   else
018900131004     C                   seton                                        31
019000131004     C                   endif
019100131004     C*
019200131004     C                   ENDSR
019300131004
019400131004
019500131004
019600131004     C     chkRECFILMBR  BEGSR
019700131004     C*
019800131004     C     *start        setll     TIVAWWWT
019900131004     C                   if        %found(TIVAWWWT)
020000131004     C                   seton                                        31
020100131004     C                   endif
020200131004     C*
020300131004     C                   ENDSR
020400131004
020500131004
020600131004
020700131004     C     wriOUTPUT     BEGSR
020800131004     C*
020900131004     C                   write     TIVAW000
021000131004     C*
021100131004     C                   ENDSR
021200161129
021300161129
021400161129
021500161129     C     carMAP        BEGSR
021600161129     C*
021700161129     C* Reperisco la versione di mappatura stati correntemente valida
021800161129     C                   call      'UBVMSDDER'
021900161129     C                   parm      *blanks       iVMSDDEOpz
022000161129     C                   parm      w§PPIVMSPROC  iVMSDDEProc
022100161129     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
022200161129     C                   parm      datcor8       iVMSDDEDtRif
022300161129     C                   parm                    oVMSDDEDDE
022400161129     C                   parm                    oVMSDDEEsito
022500161129     C*
022600161129     C* Verifico se esito no OK
022700161129     C                   if        oVMSDDEEsito <> '1'
022800161129     C                   seton                                        40
022900161129     C                   else
023000161129     C*
023100161129     C                   move(P)   w§PPIVMSPROC  vmsPROC
023200161129     C                   move(P)   w§PPIVMSKSU   vmsKSU
023300161129     C                   z-add     oVMSDDEDDE    vmsDDE
023400161129     C     KEYvms01P     setll     tivms01i
023500161129     C                   if        %equal(tivms01i)
023600161129     C     KEYvms01P     reade     tivms01i
023700161129     C*
023800161129     C* Inizializzo indice schiera di mapping
023900161129     C                   z-add     1             jCAU
024000161129     C*
024100161129     C* Ciclo x tutti i record della mappatura richiesta
024200161129     C                   dow       not %eof(tivms01i)
024300161129     C*
024400161129     C* Ignoro i record annullati
024500161129     C                   if        vmsATB = *blanks
024600161129     C*
024700161129     C* Carico la mappatura dei codici status personalizzati cliente
024800161129     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
024900161129     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
025000161129     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
025100161129     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
025200161130     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
025300161129     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
025400161129     C                   add       1             jCAU
025500161129     C*
025600161129     C                   endif
025700161129     C*
025800161129     C     KEYvms01P     reade     tivms01i
025900161129     C                   enddo
026000161129     C*
026100161129     C                   endif
026200161129     C*
026300161129     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
026400161129     C* altrimenti errore bloccante per intera traduzione
026500161129     C                   if        jCAU > 1
026600161129     C                   else
026700161129     C                   seton                                        40
026800161129     C                   endif
026900161129     C*
027000161129     C                   endif
027100161129     C*
027200161129     C                   ENDSR
027300131104
027400131004
027500131004
027600131004     C     Char_Speciali BEGSR
027700131111     C*
027800131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
027900131111     C*
028000131111     C/EXEC SQL
028100131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
028200131111     C/END-EXEC
028300131111     C*
028400131111     C/EXEC SQL
028500131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
028600131111     C/END-EXEC
028700131111     C*
028800131111     C/EXEC SQL
028900131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
029000131111     C/END-EXEC
029100131111     C*
029200131111     C/EXEC SQL
029300131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
029400131111     C/END-EXEC
029500131111     C*
029600131111     C/EXEC SQL
029700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
029800131111     C/END-EXEC
029900131004     C*
030000131004     C                   ENDSR
030100131004
030200131004
030300131004
030400131004     C     wriHeader     BEGSR
030500131010     C*
030600141107     C*  Tipo record 'BGM'
030700141107     C                   reset                   CLI_DS_BGM
030800141107     C                   eval      CD_BGM_DOCREF = %editc(currUNIQ_ID:'X')
030900141107     C                   eval      CD_BGM_DOCDAT = %editc(datcor8:'X') +
031000141107     C                                             %editc(oracor4:'X')
031100141107     C                   clear                   vawDTA
031200141107     C                   eval      vawDTA = CLI_DS_BGM
031300131004     C                   exsr      wriOUTPUT
031400141107     C*
031500141107     C*  Tipo record 'NAD'
031600141107     C                   reset                   CLI_DS_NAD
031700180125     C*
031800180125     C* a seconda del cliente in ingresso cambio il valore nel NAD
031900180125     C                   select
032000180125     C                   when      w§PPIKSU = 0430001
032100180125     C                   eval      CD_NAD_MITCOD = '80066'
032200180125     C                   when      w§PPIKSU = 0435707
032300180125     C                   eval      CD_NAD_MITCOD = '80052'
032400180125     C                   when      w§PPIKSU = 0435916
032500180125     C                   eval      CD_NAD_MITCOD = '88020'
032600180125     C                   when      w§PPIKSU = 0437580
032700180125     C                   eval      CD_NAD_MITCOD = '80078'
032800180130     C                   when      w§PPIKSU = 0437796
032900180130     C                   eval      CD_NAD_MITCOD = '80079'
033000180125     C                   endsl
033100180125     C*
033200141107     C                   clear                   vawDTA
033300141107     C                   eval      vawDTA = CLI_DS_NAD
033400141107     C                   exsr      wriOUTPUT
033500131004     C*
033600131004     C                   ENDSR
033700131004
033800131004
033900131004
034000131004     C     wriFooter     BEGSR
034100131004     C*
034200141107     C***                clear                   vawDTA
034300131004     C*
034400141107     C***                exsr      wriOUTPUT
034500131004     C*
034600131004     C                   ENDSR
034700131004
034800030924
034900030924
035000991027     C     traduci       BEGSR
035100131004     C*
035200131004     C* Inizializzo i contatori a livello di transazione/messaggio
035300141107     C                   z-add     *zeros        Conta_msg
035400141110     C*
035500141110     C* Inizializzo i contatori a livello di segmento (in questo caso CNI)
035600141110     C                   z-add     *zeros        Conta_segm
035700031103     C*
035800031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
035900031103     C* di traduzione (trattasi d "post-traduzione")
036000031104     C                   EVAL      wKstbKSU = w§PPIKSU
036100031103     C                   EVAL      wKstbFTR = *blanks
036200110103     C     KEYstb03P     SETLL     tistb03l
036300110103     C                   IF        %equal(tistb03l)
036400110103     C     KEYstb03P     READE     tistb03l
036500991027     C*
036600031103     C* Ciclo x tuttii record del cliente da trasmettere
036700110103     C                   DOW       not %eof(tistb03l)
036800080618     C*
036900130807     C                   SETON                                        50
037000141118     C*
037100141118     C* Solo bolle originali e valide ai fini della spedizione
037200141118     C                   movel     *blanks       wEsito1           1
037300141118     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
037400141118     C                                                stbAAS
037500141118     C                                               :stbLNP
037600141118     C                                               :stbNRS
037700141118     C                                               :stbNSP
037800141118     C                                               :pOutLblTyp
037900141118     C                                               :pOutAnnoBO
038000141118     C                                               :pOutLineaParBO
038100141118     C                                               :pOutSerieBO
038200141118     C                                               :pOutNumSpedBO
038300141118     C                                               :pOutDcmBO
038400141118     C                                               :pOutCcaBO
038500141118     C                                               :pOutRblBO))
038600141118     C*
038700141118     C                   if        pOutLblTyp='O' AND pOutRblBO ='N'
038800031103     C*
038900031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
039000031103     C* deve restituire al cliente nel file tradotto
039100031103     C                   IF        stbFTR = *blanks                             * ridondante
039200110103     C*
039300110103     C* Gestisco solamente gli stati standard
039400130808     C                   IF        (stbTIS= '1' OR
039500130906     C                             (stbTIS= '9' AND
039600151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
039700151117     C                              (stbPRS= '1'                ))) AND
039800130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
039900130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
040000131004     C*
040100131004     C* Per "sicurezza" non invio stati senza data
040200131004     C                   IF        stbDAS > *zeros
040300110103     C*
040400110103     C                   SETOFF                                       50
040500031103     C*
040600031103     C* Innanzitutto chaino la bolla direttamente su TITAS
040700031103     C     KEYtas30P     CHAIN     titas30c
040800031103     C                   IF        %found(titas30c)
040900131010     C*
041000131010     C* Innanzitutto chaino la bolla direttamente su TITAS
041100131216     C                   MOVEL     *zeros        wPO               3 0
041200131213     C*
041300131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
041400131213     C                   if        tasDTI = *zeros
041500131213     C                   eval      wPO = tasLNP
041600131213     C                   else
041700131213     C                   eval      wPO = tasLNA
041800131213     C                   endif
041900131213     C*
042000131213     C     wPO           CHAIN     azorg01l
042100131010     C                   IF        %found(azorg01l)
042200080618     C*
042300110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
042400031103     C                   Z-ADD     1             j
042500161129     C                   MOVEL(P)  stbCOS        wCOS_15          15
042600161129     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
042700141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
042800161129     C                   EVAL      wStsEvt = skCAU_EVT(j)
042900161129     C                   EVAL      wStsRsn = skCAU_RSN(j)
043000161130     C                   EVAL      wStsOpz = skCAU_OPZ(j)
043100161129     C                   EVAL      wStsDes = skCAU_DES(j)
043200141107     C                   SETOFF                                       50
043300031103     C                   ELSE
043400031103     C                   SETON                                        50
043500031103     C                   ENDIF
043600161129     C  N55              EXSR      sndAlert
043700140308     C*
043800140308     C* Al primo dettaglio da inviare => scrivo anche testata
043900140313     C                   IF        not *IN50
044000140313     C  N45              EXSR      wriHeader
044100140308     C                   SETON                                        45
044200140313     C                   ENDIF
044300080618     C*
044400080618     C* Scarico il buffer d output
044500131004     C  N50              EXSR      wriStato
044600110103     C*
044700131010     C                   ENDIF
044800131004     C                   ENDIF
044900110103     C                   ENDIF
045000110103     C                   ENDIF
045100110103     C                   ENDIF
045200141118     C*
045300141118     C                   endif
045400031103     C*
045500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
045600031103     C                   EVAL      stbFTR = 'T'
045700031103     C                   UPDATE    tistb000
045800991027     C*
045900110103     C     KEYstb03P     READE     tistb03l
046000031103     C                   SETOFF                                       50
046100030325     C                   ENDDO
046200031103     C                   ENDIF
046300991027     C*
046400030325     C                   EVAL      wrkesito = '0'
046500991027     C*
046600910830     C                   ENDSR
046700031103
046800031103
046900031103
047000131004     C     wriStato      BEGSR
047100131004     C*
047200131004     C* Incremento il contatore di messaggio (al interno della transazione)
047300131004     C                   eval      Conta_msg = Conta_msg + 1
047400131004     C*
047500131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
047600131104     C                   movel     *blanks       wAlfaRMA        512
047700131024     C                   movel     *blanks       wAlfaRSD        512
047800131004     C                   movel     *blanks       wAlfaLOD        512
047900131004     C                   movel     *blanks       wAlfaIND        512
048000131025     C                   movel     *blanks       wAlfaORGDES     512
048100131004     C* RMA
048200131004     C                   eval      campo_alfa = stbRMA
048300131004     C                   exsr      Char_Speciali
048400131004     C                   eval      wAlfaRMA = campo_alfa
048500131024     C* RSD
048600131024     C                   eval      campo_alfa = tasRSD
048700131024     C                   exsr      Char_Speciali
048800131024     C                   eval      wAlfaRSD = campo_alfa
048900131004     C* LOD
049000131004     C                   eval      campo_alfa = tasLOD
049100131004     C                   exsr      Char_Speciali
049200131004     C                   eval      wAlfaLOD = campo_alfa
049300131004     C* IND
049400131004     C                   eval      campo_alfa = tasIND
049500131004     C                   exsr      Char_Speciali
049600131004     C                   eval      wAlfaIND = campo_alfa
049700131025     C* ORGDES
049800131025     C                   eval      campo_alfa = orgDES
049900131025     C                   exsr      Char_Speciali
050000131025     C                   eval      wAlfaORGDES = campo_alfa
050100131004     C*
050200131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
050300131004     C* (ancora corretto ???)
050400131004     C                   z-add     *zeros        wPeso             7 1
050500131004     C                   if        tasPKB > tasPKC
050600131004     C                   eval      wPeso = tasPKB
050700131004     C                   else
050800131004     C                   eval      wPeso = tasPKC
050900131004     C                   endif
051000131004     C*
051100131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
051200170516     C*
051300170516     C* Reperisco TITA4 tipo record 'A' (per natura merce)
051400170516     C                   MOVEL     'A'           UBTA4ITRC
051500170516     C                   EXSR      RTVTA4
051600141107     C*
051700141107     C*  Tipo record 'CNI'
051800141107     C                   eval      Conta_segm  = Conta_segm  + 1
051900141107     C                   reset                   CLI_DS_CNI
052000141107     C                   z-add     Conta_segm    wNum005_0
052100141107     C                   z-add     stbRMN        wNum010_0
052200141107     C                   eval      CD_CNI_SEQUEN = %editc(wNum005_0:'X')
052300170516     C                   eval      CD_CNI_SHPRMN = *blanks
052400141107     C                   eval      CD_CNI_STSCOD = wStsEvt
052500141107     C                   eval      CD_CNI_STSDES = wStsDes
052600141107     C                   eval      CD_CNI_STSRSN = wStsRsn
052700141107     C                   eval      CD_CNI_SHPRMA = stbRMA
052800141107     C                   eval      CD_CNI_SHPDSP = %editc(stbDSP:'X')
052900141107     C                   eval      CD_CNI_STSDAT = %editc(stbDAS:'X') +
053000141107     C                                             %editc(stbORS:'X') + '+0100'
053100141107     C                   if        %subst(stbCOS:1:6) = 'CONSOK'
053200141107     C                   eval      CD_CNI_SHPOK  = 'ST'
053300141107     C                   eval      CD_CNI_SHPRSD = tasRSD
053400141107     C                   else
053500141107     C                   eval      CD_CNI_SHPOK  = *blanks
053600141107     C                   eval      CD_CNI_SHPRSD = *blanks
053700141107     C                   endif
053800170516     C                   eval      CD_CNI_RGRSHP = §TA4ANAS
053900141107     C                   clear                   vawDTA
054000141107     C                   eval      vawDTA = CLI_DS_CNI
054100141107     C                   exsr      wriOUTPUT
054200031103     C*
054300031103     C                   ENDSR
054400170516
054500170516
054600170516
054700170516     C     RTVTA4        BEGSR
054800170516     C*
054900170516     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
055000170516     C                   CLEAR                   DTA4A
055100170516     C*
055200170516     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
055300170516     C                   CALL      'UBTA400R'
055400170516     C                   PARM      *blanks       UBTA4IOPZ
055500170516     C                   PARM      *blanks       UBTA4ITLA
055600170516     C                   PARM      tasAAS        UBTA4IAAS
055700170516     C                   PARM      tasLNP        UBTA4ILNP
055800170516     C                   PARM      tasNRS        UBTA4INRS
055900170516     C                   PARM      tasNSP        UBTA4INSP
056000170516     C                   PARM                    UBTA4ITRC
056100170516     C                   PARM                    UBTA4OERR
056200170516     C                   PARM                    UBTA4ODS
056300170516     C                   PARM                    UBTA4OLEN
056400170516     C                   PARM                    UBTA4ODATI
056500170516     C*
056600170516     C                   IF        UBTA4OERR = *zeros
056700170516     C                   SELECT
056800170516     C* Gestione output tipo record 'A'
056900170516     C                   WHEN      UBTA4ODS = 'DTA4A'
057000170516     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
057100170516     C                   ENDSL
057200170516     C                   ENDIF
057300170516     C*
057400170516     C                   ENDSR
057500170516     C*--------------------------------------------------------
057600161129
057700161129
057800161129
057900161129     C*--------------------------------------------------------------------------------------------*
058000161129     C* /TITLE Compongo il testo e spedisco una e-mail di alert
058100161129     C*--------------------------------------------------------------------------------------------*
058200161129     C     sndAlert      BEGSR
058300161129     C*
058400161129     C* Inizializzo variabili
058500161129     C                   movel     *blanks       wrkEml          253
058600161129     C                   movel     *blanks       wrkCcEml        253
058700161129     C                   movel     *blanks       wrkEmlMsg      5000
058800161129     C                   movel     *blanks       wrkEmlOgg        44
058900161129     C*
059000161129     C* Valorizzo i campi della e-m@ail
059100161129     C                   eval      wrkEml='cedalert@brt.it'
059200161129     C                   eval      wrkEmlOgg=
059300161129     C                               'BARTVAS - Problema Mappatura TIVMS00F'
059400161129     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
059500161129     C                                                                    ':/N'+
059600161129     C                                                                    ':/N'+
059700161129     C                             'Verificare la mappatura: '           +':/N'+
059800161129     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
059900161129     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
060000161129     C                                                                    ':/N'+
060100161129     C                             'Causale mancante: ' + stbCOS         +':/N'+
060200161129     C                             ' '                                   +':/N'
060300161129     C*
060400161129     C                   call(e)   'TIS701C1'
060500161129     C                   parm                    wrkEml
060600161129     C                   parm                    wrkCcEml
060700161129     C                   parm                    wrkEmlOgg
060800161129     C                   parm                    wrkEmlMsg
060900161129     C                   parm                    wrkEsito          1
061000161129     C*
061100161129     C                   ENDSR
061200031103
061300031103
061400991027
061500991027      /TITLE Operazioni iniziali.
061600991027     C     *inzsr        BEGSR
061700991027     C*
061800991027     C     *ENTRY        PLIST
061900031103     C                   parm                    prmppi
062000991027     C     wrkesito      parm      wrkesito      prmesito
062100031103     C*
062200031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
062300031103     C                   EVAL      DSPPI = prmppi
062400131004     C*
062500131004     C* Reperisco i valori variabili ma a livello di transazione corrente
062600131004     C* (tipicamente data e ora relativ al file generato)
062700141107     C*
062800141107     C* CALCOLA LA DATA CORRENTE
062900141107     C                   time                    wn14             14 0
063000141107     C                   movel     wn14          oracor6           6 0          *ORA 6
063100141107     C                   movel     wn14          oracor4           4 0          *ORA 4
063200141107     C                   z-add     *zeros        datcor8           8 0          *DATA CORRENTE AA/M/
063300141107     C                   eval      datcor8 = %dec(%date() : *ISO)
063400030709     C*
063500030709     C* Definizione chiavi
063600030924     C*
063700110103     C* Chiave su TISTB03L - Parziale
063800110103     C     KEYstb03P     KLIST
063900031104     C                   KFLD                    wKstbKSU
064000031103     C                   KFLD                    wKstbFTR
064100031103     C*
064200031103     C* Chiave su TITAS30C - Parziale
064300031103     C     KEYtas30P     KLIST
064400031103     C                   KFLD                    stbAAS
064500031103     C                   KFLD                    stbLNP
064600031103     C                   KFLD                    stbNRS
064700031103     C                   KFLD                    stbNSP
064800161129     C*
064900161129     C* Chiave su TIVMS01I - Parziale
065000161129     C     KEYvms01P     KLIST
065100161129     C                   KFLD                    vmsPROC
065200161129     C                   KFLD                    vmsKSU
065300161129     C                   KFLD                    vmsDDE
065400991027     C*
065500991027     C                   ENDSR
