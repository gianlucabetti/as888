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
002900141107
003000141107
003100141107     D*******
003200141107     D* DS CLIENTE
003300141107     D*******
003400141107     D*
003500141107     D* BGM
003600141107     D*
003700141107     D CLI_DS_BGM      DS           100
003800141107     D  CD_BGM_RECTYP          1      3A   inz('BGM')
003900141107     D  CD_BGM_DOCCOD          4      6A   inz('77')
004000141107     D  CD_BGM_DOCNAM          7     16A   inz('SHP_STATUS')
004100141107     D  CD_BGM_DOCREF         17     31A   inz(*all'?')
004200141107     D  CD_BGM_DOCVER         32     40A   inz('BRT_001')
004300141107     D  CD_BGM_DOCFUN         41     43A   inz('9')
004400141107     D  CD_BGM_DOCDAT         44     55A   inz(*all'?')
004500141107     D  CD_BGM_TOTSHP         91     95A   inz(*all'0')
004600141107     D  CD_BGM_TOTSTS         96    100A   inz(*all'0')
004700141107     D*
004800141107     D* NAD
004900141107     D*
005000141107     D CLI_DS_NAD      DS           113
005100141107     D  CD_NAD_RECTYP          1      3A   inz('NAD')
005200141107     D  CD_NAD_MSGMIT          4     38A   inz('BRT Corriere Espresso S.p.A.')
005300141113     D  CD_NAD_MITCOD         39     55A   inz('40042')
005400141107     D  CD_NAD_NAZID          56     58A   inz('IT')
005500141107     D  CD_NAD_MSGRCV         59     93A   inz('Ceva Logistics Italia Srl')
005600141107     D  CD_NAD_RCVCOD         94    110A   inz('1')
005700141107     D*
005800141107     D* CNI
005900141107     D*
006000141107     D CLI_DS_CNI      DS          1025
006100141107     D  CD_CNI_RECTYP          1      3A   inz('CNI')
006200141107     D  CD_CNI_SEQUEN          4      8A   inz(*all'?')
006300141107     D  CD_CNI_SHPRMN          9     43A   inz(*all'?')
006400141107     D  CD_CNI_LNPDES         61    316A   inz('BRT_166_LANDRIANO_PV_27015_IT')
006500141107     D  CD_CNI_STSOWN        318    318A   inz('1')
006600141107     D  CD_CNI_STSCOD        319    321A   inz(*all'?')
006700141107     D  CD_CNI_STSDES        322    356A   inz(*all'?')
006800141107     D  CD_CNI_STSRSN        357    376A   inz(*all'?')
006900141107     D  CD_CNI_SHPRMA        412    428A   inz(*all'?')
007000141107     D  CD_CNI_SHPDSP        429    440A   inz(*all'?')
007100141107     D  CD_CNI_STSDAT        528    544A   inz(*all'?')
007200141107     D  CD_CNI_SHPOK         912    914A   inz(*all'?')
007300141107     D  CD_CNI_SHPRSD        915    949A   inz(*all'?')
007400141107     D  CD_CNI_FIRMA         950    984A   inz(*blanks)
007500141107     D  CD_CNI_SHPPKB        985    996A   inz(*all'0')
007600141107     D  CD_CNI_SHPVLB        997   1008A   inz(*all'0')
007700141107     D*
007800030924
007900161129
008000161129
008100041015     D*-----------
008200041015     D* VARIABILI D WRK
008300041015     D*-----------
008400031103     D prmppi          S             50
008500030924     D prmesito        S              1
008600030924     D wrkesito        S                   like(prmesito)
008700131004     D campo_alfa      S            512
008800161129     D wStsEvt         S             25
008900161129     D wStsRsn         S             25
009000161130     D wStsOpz         S              5
009100161129     D wStsDes         S             35
009200141107     D Conta_msg       S             10i 0
009300141107     D Conta_segm      S             10i 0
009400141107     D wNum005_0       S              5  0
009500141107     D wNum010_0       S             10  0
009600131004
009700131004
009800131004     D*-----------
009900131004     D* DEFINIZIONE DS ESTERNE
010000131004     D*-----------
010100131004     D trul33ds      e ds                  inz
010200131004     D kpjba         e ds                  inz
010300131004     D DTAD          e ds                  inz
010400131004
010500131004
010600031103     D*-----------
010700031103     D* VARIABILI RIFERITE AL DATA-BASE
010800031103     D*-----------
010900080618     D wKstbKSU        S                   like(stbKSU)
011000080618     D wKstbFTR        S                   like(stbFTR)
011100030924
011200110705
011300031103
011400031103     D*-----------
011500031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
011600031103     D*-----------
011700031103     D DSPPI           DS
011800080618     D  w§PPIKSU                      7  0
011900161129     D  w§PPIVMSPROC                       like(vmsPROC) inz
012000161129     D  w§PPIVMSKSU                        like(vmsKSU)  inz
012100131004
012200141118
012300141118
012400141118     D*------------------
012500141118     D* LINKING A DEFINIZIONI ESTERNE
012600141118     D*------------------
012700141118     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
012800141118     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
012900161129     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
013000131004
013100030924
013200030325
013300030924     C* MAIN/
013400161129     C*
013500161129     C* Carica schiere di mappatura
013600161129     C                   SETOFF                                       40
013700161129     C                   EXSR      carMAP
013800161129     C*
013900161129     C* Se caricamento mappatura tutto OK => Procedo
014000161129     C                   IF        not *in40
014100140308     C*
014200140308     C* Spia dati da inviare
014300140308     C                   SETOFF                                       45
014400041015     C*
014500041015     C* Esegue traduzione
014600140308     C                   SETOFF                                       31
014700131004     C  N31              EXSR      chkRECFILMBR
014800131004     C  N31              EXSR      rtvID
014900131004     C  N31              EXSR      traduci
015000141107     C**  N31
015100141107     C**AN 45              EXSR      wriFooter
015200161129     C*
015300161129     C* Diversamente traduzioe completamente fallita
015400161129     C                   ELSE
015500161129     C                   EVAL      wrkesito = '2'
015600161129     C                   ENDIF
015700110705     C*
015800921023     C                   SETON                                        LR
015900131004
016000131004
016100131004
016200131004     C*----------------------------------------------------*
016300131004     C*  STACCO ID UNIVOCO
016400131004     C*----------------------------------------------------*
016500131004     C     RTVID         BEGSR
016600131004     C*
016700131004     C                   z-add     *zeros        currUNIQ_ID      15 0
016800131004     C*
016900131004     C* NSP => Stacco un numeratore da AZNUM
017000131004     C                   clear                   TRUL33DS
017100131004     C                   eval      I33OPE = *zeros
017200131004     C                   eval      I33CNU = 302
017300131004     C                   eval      I33NUM = 1
017400131004     C                   movel     TRUL33DS      KPJBU
017500131004     C                   call      'TRUL33R'
017600131004     C                   parm                    KPJBA
017700131004     C                   movel     KPJBU         TRUL33DS
017800131004     C                   if        O33ERR = *zeros
017900131004     C                   z-add     O33NRF        currUNIQ_ID
018000131004     C                   else
018100131004     C                   seton                                        31
018200131004     C                   endif
018300131004     C*
018400131004     C                   ENDSR
018500131004
018600131004
018700131004
018800131004     C     chkRECFILMBR  BEGSR
018900131004     C*
019000131004     C     *start        setll     TIVAWWWT
019100131004     C                   if        %found(TIVAWWWT)
019200131004     C                   seton                                        31
019300131004     C                   endif
019400131004     C*
019500131004     C                   ENDSR
019600131004
019700131004
019800131004
019900131004     C     wriOUTPUT     BEGSR
020000131004     C*
020100131004     C                   write     TIVAW000
020200131004     C*
020300131004     C                   ENDSR
020400161129
020500161129
020600161129
020700161129     C     carMAP        BEGSR
020800161129     C*
020900161129     C* Reperisco la versione di mappatura stati correntemente valida
021000161129     C                   call      'UBVMSDDER'
021100161129     C                   parm      *blanks       iVMSDDEOpz
021200161129     C                   parm      w§PPIVMSPROC  iVMSDDEProc
021300161129     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
021400161129     C                   parm      datcor8       iVMSDDEDtRif
021500161129     C                   parm                    oVMSDDEDDE
021600161129     C                   parm                    oVMSDDEEsito
021700161129     C*
021800161129     C* Verifico se esito no OK
021900161129     C                   if        oVMSDDEEsito <> '1'
022000161129     C                   seton                                        40
022100161129     C                   else
022200161129     C*
022300161129     C                   move(P)   w§PPIVMSPROC  vmsPROC
022400161129     C                   move(P)   w§PPIVMSKSU   vmsKSU
022500161129     C                   z-add     oVMSDDEDDE    vmsDDE
022600161129     C     KEYvms01P     setll     tivms01i
022700161129     C                   if        %equal(tivms01i)
022800161129     C     KEYvms01P     reade     tivms01i
022900161129     C*
023000161129     C* Inizializzo indice schiera di mapping
023100161129     C                   z-add     1             jCAU
023200161129     C*
023300161129     C* Ciclo x tutti i record della mappatura richiesta
023400161129     C                   dow       not %eof(tivms01i)
023500161129     C*
023600161129     C* Ignoro i record annullati
023700161129     C                   if        vmsATB = *blanks
023800161129     C*
023900161129     C* Carico la mappatura dei codici status personalizzati cliente
024000161129     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
024100161129     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
024200161129     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
024300161129     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
024400161130     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
024500161129     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
024600161129     C                   add       1             jCAU
024700161129     C*
024800161129     C                   endif
024900161129     C*
025000161129     C     KEYvms01P     reade     tivms01i
025100161129     C                   enddo
025200161129     C*
025300161129     C                   endif
025400161129     C*
025500161129     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
025600161129     C* altrimenti errore bloccante per intera traduzione
025700161129     C                   if        jCAU > 1
025800161129     C                   else
025900161129     C                   seton                                        40
026000161129     C                   endif
026100161129     C*
026200161129     C                   endif
026300161129     C*
026400161129     C                   ENDSR
026500131104
026600131004
026700131004
026800131004     C     Char_Speciali BEGSR
026900131111     C*
027000131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
027100131111     C*
027200131111     C/EXEC SQL
027300131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
027400131111     C/END-EXEC
027500131111     C*
027600131111     C/EXEC SQL
027700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
027800131111     C/END-EXEC
027900131111     C*
028000131111     C/EXEC SQL
028100131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
028200131111     C/END-EXEC
028300131111     C*
028400131111     C/EXEC SQL
028500131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
028600131111     C/END-EXEC
028700131111     C*
028800131111     C/EXEC SQL
028900131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
029000131111     C/END-EXEC
029100131004     C*
029200131004     C                   ENDSR
029300131004
029400131004
029500131004
029600131004     C     wriHeader     BEGSR
029700131010     C*
029800141107     C*  Tipo record 'BGM'
029900141107     C                   reset                   CLI_DS_BGM
030000141107     C                   eval      CD_BGM_DOCREF = %editc(currUNIQ_ID:'X')
030100141107     C                   eval      CD_BGM_DOCDAT = %editc(datcor8:'X') +
030200141107     C                                             %editc(oracor4:'X')
030300141107     C                   clear                   vawDTA
030400141107     C                   eval      vawDTA = CLI_DS_BGM
030500131004     C                   exsr      wriOUTPUT
030600141107     C*
030700141107     C*  Tipo record 'NAD'
030800141107     C                   reset                   CLI_DS_NAD
030900141107     C                   clear                   vawDTA
031000141107     C                   eval      vawDTA = CLI_DS_NAD
031100141107     C                   exsr      wriOUTPUT
031200131004     C*
031300131004     C                   ENDSR
031400131004
031500131004
031600131004
031700131004     C     wriFooter     BEGSR
031800131004     C*
031900141107     C***                clear                   vawDTA
032000131004     C*
032100141107     C***                exsr      wriOUTPUT
032200131004     C*
032300131004     C                   ENDSR
032400131004
032500030924
032600030924
032700991027     C     traduci       BEGSR
032800131004     C*
032900131004     C* Inizializzo i contatori a livello di transazione/messaggio
033000141107     C                   z-add     *zeros        Conta_msg
033100141110     C*
033200141110     C* Inizializzo i contatori a livello di segmento (in questo caso CNI)
033300141110     C                   z-add     *zeros        Conta_segm
033400031103     C*
033500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
033600031103     C* di traduzione (trattasi d "post-traduzione")
033700031104     C                   EVAL      wKstbKSU = w§PPIKSU
033800031103     C                   EVAL      wKstbFTR = *blanks
033900110103     C     KEYstb03P     SETLL     tistb03l
034000110103     C                   IF        %equal(tistb03l)
034100110103     C     KEYstb03P     READE     tistb03l
034200991027     C*
034300031103     C* Ciclo x tuttii record del cliente da trasmettere
034400110103     C                   DOW       not %eof(tistb03l)
034500080618     C*
034600130807     C                   SETON                                        50
034700141118     C*
034800141118     C* Solo bolle originali e valide ai fini della spedizione
034900141118     C                   movel     *blanks       wEsito1           1
035000141118     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
035100141118     C                                                stbAAS
035200141118     C                                               :stbLNP
035300141118     C                                               :stbNRS
035400141118     C                                               :stbNSP
035500141118     C                                               :pOutLblTyp
035600141118     C                                               :pOutAnnoBO
035700141118     C                                               :pOutLineaParBO
035800141118     C                                               :pOutSerieBO
035900141118     C                                               :pOutNumSpedBO
036000141118     C                                               :pOutDcmBO
036100141118     C                                               :pOutCcaBO
036200141118     C                                               :pOutRblBO))
036300141118     C*
036400141118     C                   if        pOutLblTyp='O' AND pOutRblBO ='N'
036500031103     C*
036600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
036700031103     C* deve restituire al cliente nel file tradotto
036800031103     C                   IF        stbFTR = *blanks                             * ridondante
036900110103     C*
037000110103     C* Gestisco solamente gli stati standard
037100130808     C                   IF        (stbTIS= '1' OR
037200130906     C                             (stbTIS= '9' AND
037300151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
037400151117     C                              (stbPRS= '1'                ))) AND
037500130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
037600130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
037700131004     C*
037800131004     C* Per "sicurezza" non invio stati senza data
037900131004     C                   IF        stbDAS > *zeros
038000110103     C*
038100110103     C                   SETOFF                                       50
038200031103     C*
038300031103     C* Innanzitutto chaino la bolla direttamente su TITAS
038400031103     C     KEYtas30P     CHAIN     titas30c
038500031103     C                   IF        %found(titas30c)
038600131010     C*
038700131010     C* Innanzitutto chaino la bolla direttamente su TITAS
038800131216     C                   MOVEL     *zeros        wPO               3 0
038900131213     C*
039000131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
039100131213     C                   if        tasDTI = *zeros
039200131213     C                   eval      wPO = tasLNP
039300131213     C                   else
039400131213     C                   eval      wPO = tasLNA
039500131213     C                   endif
039600131213     C*
039700131213     C     wPO           CHAIN     azorg01l
039800131010     C                   IF        %found(azorg01l)
039900080618     C*
040000110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
040100031103     C                   Z-ADD     1             j
040200161129     C                   MOVEL(P)  stbCOS        wCOS_15          15
040300161129     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
040400141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
040500161129     C                   EVAL      wStsEvt = skCAU_EVT(j)
040600161129     C                   EVAL      wStsRsn = skCAU_RSN(j)
040700161130     C                   EVAL      wStsOpz = skCAU_OPZ(j)
040800161129     C                   EVAL      wStsDes = skCAU_DES(j)
040900141107     C                   SETOFF                                       50
041000031103     C                   ELSE
041100031103     C                   SETON                                        50
041200031103     C                   ENDIF
041300161129     C  N55              EXSR      sndAlert
041400140308     C*
041500140308     C* Al primo dettaglio da inviare => scrivo anche testata
041600140313     C                   IF        not *IN50
041700140313     C  N45              EXSR      wriHeader
041800140308     C                   SETON                                        45
041900140313     C                   ENDIF
042000080618     C*
042100080618     C* Scarico il buffer d output
042200131004     C  N50              EXSR      wriStato
042300110103     C*
042400131010     C                   ENDIF
042500131004     C                   ENDIF
042600110103     C                   ENDIF
042700110103     C                   ENDIF
042800110103     C                   ENDIF
042900141118     C*
043000141118     C                   endif
043100031103     C*
043200031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
043300031103     C                   EVAL      stbFTR = 'T'
043400031103     C                   UPDATE    tistb000
043500991027     C*
043600110103     C     KEYstb03P     READE     tistb03l
043700031103     C                   SETOFF                                       50
043800030325     C                   ENDDO
043900031103     C                   ENDIF
044000991027     C*
044100030325     C                   EVAL      wrkesito = '0'
044200991027     C*
044300910830     C                   ENDSR
044400031103
044500031103
044600031103
044700131004     C     wriStato      BEGSR
044800131004     C*
044900131004     C* Incremento il contatore di messaggio (al interno della transazione)
045000131004     C                   eval      Conta_msg = Conta_msg + 1
045100131004     C*
045200131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
045300131104     C                   movel     *blanks       wAlfaRMA        512
045400131024     C                   movel     *blanks       wAlfaRSD        512
045500131004     C                   movel     *blanks       wAlfaLOD        512
045600131004     C                   movel     *blanks       wAlfaIND        512
045700131025     C                   movel     *blanks       wAlfaORGDES     512
045800131004     C* RMA
045900131004     C                   eval      campo_alfa = stbRMA
046000131004     C                   exsr      Char_Speciali
046100131004     C                   eval      wAlfaRMA = campo_alfa
046200131024     C* RSD
046300131024     C                   eval      campo_alfa = tasRSD
046400131024     C                   exsr      Char_Speciali
046500131024     C                   eval      wAlfaRSD = campo_alfa
046600131004     C* LOD
046700131004     C                   eval      campo_alfa = tasLOD
046800131004     C                   exsr      Char_Speciali
046900131004     C                   eval      wAlfaLOD = campo_alfa
047000131004     C* IND
047100131004     C                   eval      campo_alfa = tasIND
047200131004     C                   exsr      Char_Speciali
047300131004     C                   eval      wAlfaIND = campo_alfa
047400131025     C* ORGDES
047500131025     C                   eval      campo_alfa = orgDES
047600131025     C                   exsr      Char_Speciali
047700131025     C                   eval      wAlfaORGDES = campo_alfa
047800131004     C*
047900131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
048000131004     C* (ancora corretto ???)
048100131004     C                   z-add     *zeros        wPeso             7 1
048200131004     C                   if        tasPKB > tasPKC
048300131004     C                   eval      wPeso = tasPKB
048400131004     C                   else
048500131004     C                   eval      wPeso = tasPKC
048600131004     C                   endif
048700131004     C*
048800131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
048900141107     C*
049000141107     C*  Tipo record 'CNI'
049100141107     C                   eval      Conta_segm  = Conta_segm  + 1
049200141107     C                   reset                   CLI_DS_CNI
049300141107     C                   z-add     Conta_segm    wNum005_0
049400141107     C                   z-add     stbRMN        wNum010_0
049500141107     C                   eval      CD_CNI_SEQUEN = %editc(wNum005_0:'X')
049600141107     C                   eval      CD_CNI_SHPRMN = %editc(wNum010_0:'X')
049700141107     C                   eval      CD_CNI_STSCOD = wStsEvt
049800141107     C                   eval      CD_CNI_STSDES = wStsDes
049900141107     C                   eval      CD_CNI_STSRSN = wStsRsn
050000141107     C                   eval      CD_CNI_SHPRMA = stbRMA
050100141107     C                   eval      CD_CNI_SHPDSP = %editc(stbDSP:'X')
050200141107     C                   eval      CD_CNI_STSDAT = %editc(stbDAS:'X') +
050300141107     C                                             %editc(stbORS:'X') + '+0100'
050400141107     C                   if        %subst(stbCOS:1:6) = 'CONSOK'
050500141107     C                   eval      CD_CNI_SHPOK  = 'ST'
050600141107     C                   eval      CD_CNI_SHPRSD = tasRSD
050700141107     C                   else
050800141107     C                   eval      CD_CNI_SHPOK  = *blanks
050900141107     C                   eval      CD_CNI_SHPRSD = *blanks
051000141107     C                   endif
051100141107     C                   clear                   vawDTA
051200141107     C                   eval      vawDTA = CLI_DS_CNI
051300141107     C                   exsr      wriOUTPUT
051400031103     C*
051500031103     C                   ENDSR
051600161129
051700161129
051800161129
051900161129     C*--------------------------------------------------------------------------------------------*
052000161129     C* /TITLE Compongo il testo e spedisco una e-mail di alert
052100161129     C*--------------------------------------------------------------------------------------------*
052200161129     C     sndAlert      BEGSR
052300161129     C*
052400161129     C* Inizializzo variabili
052500161129     C                   movel     *blanks       wrkEml          253
052600161129     C                   movel     *blanks       wrkCcEml        253
052700161129     C                   movel     *blanks       wrkEmlMsg      5000
052800161129     C                   movel     *blanks       wrkEmlOgg        44
052900161129     C*
053000161129     C* Valorizzo i campi della e-m@ail
053100161129     C                   eval      wrkEml='cedalert@brt.it'
053200161129     C                   eval      wrkCcEml='fabrizio.gurrieri@brt.it'
053300161129     C                   eval      wrkEmlOgg=
053400161129     C                               'BARTVAS - Problema Mappatura TIVMS00F'
053500161129     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
053600161129     C                                                                    ':/N'+
053700161129     C                                                                    ':/N'+
053800161129     C                             'Verificare la mappatura: '           +':/N'+
053900161129     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
054000161129     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
054100161129     C                                                                    ':/N'+
054200161129     C                             'Causale mancante: ' + stbCOS         +':/N'+
054300161129     C                             ' '                                   +':/N'
054400161129     C*
054500161129     C                   call(e)   'TIS701C1'
054600161129     C                   parm                    wrkEml
054700161129     C                   parm                    wrkCcEml
054800161129     C                   parm                    wrkEmlOgg
054900161129     C                   parm                    wrkEmlMsg
055000161129     C                   parm                    wrkEsito          1
055100161129     C*
055200161129     C                   ENDSR
055300031103
055400031103
055500991027
055600991027      /TITLE Operazioni iniziali.
055700991027     C     *inzsr        BEGSR
055800991027     C*
055900991027     C     *ENTRY        PLIST
056000031103     C                   parm                    prmppi
056100991027     C     wrkesito      parm      wrkesito      prmesito
056200031103     C*
056300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
056400031103     C                   EVAL      DSPPI = prmppi
056500131004     C*
056600131004     C* Reperisco i valori variabili ma a livello di transazione corrente
056700131004     C* (tipicamente data e ora relativ al file generato)
056800141107     C*
056900141107     C* CALCOLA LA DATA CORRENTE
057000141107     C                   time                    wn14             14 0
057100141107     C                   movel     wn14          oracor6           6 0          *ORA 6
057200141107     C                   movel     wn14          oracor4           4 0          *ORA 4
057300141107     C                   z-add     *zeros        datcor8           8 0          *DATA CORRENTE AA/M/
057400141107     C                   eval      datcor8 = %dec(%date() : *ISO)
057500030709     C*
057600030709     C* Definizione chiavi
057700030924     C*
057800110103     C* Chiave su TISTB03L - Parziale
057900110103     C     KEYstb03P     KLIST
058000031104     C                   KFLD                    wKstbKSU
058100031103     C                   KFLD                    wKstbFTR
058200031103     C*
058300031103     C* Chiave su TITAS30C - Parziale
058400031103     C     KEYtas30P     KLIST
058500031103     C                   KFLD                    stbAAS
058600031103     C                   KFLD                    stbLNP
058700031103     C                   KFLD                    stbNRS
058800031103     C                   KFLD                    stbNSP
058900161129     C*
059000161129     C* Chiave su TIVMS01I - Parziale
059100161129     C     KEYvms01P     KLIST
059200161129     C                   KFLD                    vmsPROC
059300161129     C                   KFLD                    vmsKSU
059400161129     C                   KFLD                    vmsDDE
059500991027     C*
059600991027     C                   ENDSR
