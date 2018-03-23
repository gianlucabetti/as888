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
005900171124     D  CD_NAD_MITCOD         39     55A   inz('80066')
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
031700141107     C                   clear                   vawDTA
031800141107     C                   eval      vawDTA = CLI_DS_NAD
031900141107     C                   exsr      wriOUTPUT
032000131004     C*
032100131004     C                   ENDSR
032200131004
032300131004
032400131004
032500131004     C     wriFooter     BEGSR
032600131004     C*
032700141107     C***                clear                   vawDTA
032800131004     C*
032900141107     C***                exsr      wriOUTPUT
033000131004     C*
033100131004     C                   ENDSR
033200131004
033300030924
033400030924
033500991027     C     traduci       BEGSR
033600131004     C*
033700131004     C* Inizializzo i contatori a livello di transazione/messaggio
033800141107     C                   z-add     *zeros        Conta_msg
033900141110     C*
034000141110     C* Inizializzo i contatori a livello di segmento (in questo caso CNI)
034100141110     C                   z-add     *zeros        Conta_segm
034200031103     C*
034300031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
034400031103     C* di traduzione (trattasi d "post-traduzione")
034500031104     C                   EVAL      wKstbKSU = w§PPIKSU
034600031103     C                   EVAL      wKstbFTR = *blanks
034700110103     C     KEYstb03P     SETLL     tistb03l
034800110103     C                   IF        %equal(tistb03l)
034900110103     C     KEYstb03P     READE     tistb03l
035000991027     C*
035100031103     C* Ciclo x tuttii record del cliente da trasmettere
035200110103     C                   DOW       not %eof(tistb03l)
035300080618     C*
035400130807     C                   SETON                                        50
035500141118     C*
035600141118     C* Solo bolle originali e valide ai fini della spedizione
035700141118     C                   movel     *blanks       wEsito1           1
035800141118     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
035900141118     C                                                stbAAS
036000141118     C                                               :stbLNP
036100141118     C                                               :stbNRS
036200141118     C                                               :stbNSP
036300141118     C                                               :pOutLblTyp
036400141118     C                                               :pOutAnnoBO
036500141118     C                                               :pOutLineaParBO
036600141118     C                                               :pOutSerieBO
036700141118     C                                               :pOutNumSpedBO
036800141118     C                                               :pOutDcmBO
036900141118     C                                               :pOutCcaBO
037000141118     C                                               :pOutRblBO))
037100141118     C*
037200141118     C                   if        pOutLblTyp='O' AND pOutRblBO ='N'
037300031103     C*
037400031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
037500031103     C* deve restituire al cliente nel file tradotto
037600031103     C                   IF        stbFTR = *blanks                             * ridondante
037700110103     C*
037800110103     C* Gestisco solamente gli stati standard
037900130808     C                   IF        (stbTIS= '1' OR
038000130906     C                             (stbTIS= '9' AND
038100151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
038200151117     C                              (stbPRS= '1'                ))) AND
038300130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
038400130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
038500131004     C*
038600131004     C* Per "sicurezza" non invio stati senza data
038700131004     C                   IF        stbDAS > *zeros
038800110103     C*
038900110103     C                   SETOFF                                       50
039000031103     C*
039100031103     C* Innanzitutto chaino la bolla direttamente su TITAS
039200031103     C     KEYtas30P     CHAIN     titas30c
039300031103     C                   IF        %found(titas30c)
039400131010     C*
039500131010     C* Innanzitutto chaino la bolla direttamente su TITAS
039600131216     C                   MOVEL     *zeros        wPO               3 0
039700131213     C*
039800131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
039900131213     C                   if        tasDTI = *zeros
040000131213     C                   eval      wPO = tasLNP
040100131213     C                   else
040200131213     C                   eval      wPO = tasLNA
040300131213     C                   endif
040400131213     C*
040500131213     C     wPO           CHAIN     azorg01l
040600131010     C                   IF        %found(azorg01l)
040700080618     C*
040800110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
040900031103     C                   Z-ADD     1             j
041000161129     C                   MOVEL(P)  stbCOS        wCOS_15          15
041100161129     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
041200141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
041300161129     C                   EVAL      wStsEvt = skCAU_EVT(j)
041400161129     C                   EVAL      wStsRsn = skCAU_RSN(j)
041500161130     C                   EVAL      wStsOpz = skCAU_OPZ(j)
041600161129     C                   EVAL      wStsDes = skCAU_DES(j)
041700141107     C                   SETOFF                                       50
041800031103     C                   ELSE
041900031103     C                   SETON                                        50
042000031103     C                   ENDIF
042100161129     C  N55              EXSR      sndAlert
042200140308     C*
042300140308     C* Al primo dettaglio da inviare => scrivo anche testata
042400140313     C                   IF        not *IN50
042500140313     C  N45              EXSR      wriHeader
042600140308     C                   SETON                                        45
042700140313     C                   ENDIF
042800080618     C*
042900080618     C* Scarico il buffer d output
043000131004     C  N50              EXSR      wriStato
043100110103     C*
043200131010     C                   ENDIF
043300131004     C                   ENDIF
043400110103     C                   ENDIF
043500110103     C                   ENDIF
043600110103     C                   ENDIF
043700141118     C*
043800141118     C                   endif
043900031103     C*
044000031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
044100031103     C                   EVAL      stbFTR = 'T'
044200031103     C                   UPDATE    tistb000
044300991027     C*
044400110103     C     KEYstb03P     READE     tistb03l
044500031103     C                   SETOFF                                       50
044600030325     C                   ENDDO
044700031103     C                   ENDIF
044800991027     C*
044900030325     C                   EVAL      wrkesito = '0'
045000991027     C*
045100910830     C                   ENDSR
045200031103
045300031103
045400031103
045500131004     C     wriStato      BEGSR
045600131004     C*
045700131004     C* Incremento il contatore di messaggio (al interno della transazione)
045800131004     C                   eval      Conta_msg = Conta_msg + 1
045900131004     C*
046000131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
046100131104     C                   movel     *blanks       wAlfaRMA        512
046200131024     C                   movel     *blanks       wAlfaRSD        512
046300131004     C                   movel     *blanks       wAlfaLOD        512
046400131004     C                   movel     *blanks       wAlfaIND        512
046500131025     C                   movel     *blanks       wAlfaORGDES     512
046600131004     C* RMA
046700131004     C                   eval      campo_alfa = stbRMA
046800131004     C                   exsr      Char_Speciali
046900131004     C                   eval      wAlfaRMA = campo_alfa
047000131024     C* RSD
047100131024     C                   eval      campo_alfa = tasRSD
047200131024     C                   exsr      Char_Speciali
047300131024     C                   eval      wAlfaRSD = campo_alfa
047400131004     C* LOD
047500131004     C                   eval      campo_alfa = tasLOD
047600131004     C                   exsr      Char_Speciali
047700131004     C                   eval      wAlfaLOD = campo_alfa
047800131004     C* IND
047900131004     C                   eval      campo_alfa = tasIND
048000131004     C                   exsr      Char_Speciali
048100131004     C                   eval      wAlfaIND = campo_alfa
048200131025     C* ORGDES
048300131025     C                   eval      campo_alfa = orgDES
048400131025     C                   exsr      Char_Speciali
048500131025     C                   eval      wAlfaORGDES = campo_alfa
048600131004     C*
048700131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
048800131004     C* (ancora corretto ???)
048900131004     C                   z-add     *zeros        wPeso             7 1
049000131004     C                   if        tasPKB > tasPKC
049100131004     C                   eval      wPeso = tasPKB
049200131004     C                   else
049300131004     C                   eval      wPeso = tasPKC
049400131004     C                   endif
049500131004     C*
049600131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
049700170516     C*
049800170516     C* Reperisco TITA4 tipo record 'A' (per natura merce)
049900170516     C                   MOVEL     'A'           UBTA4ITRC
050000170516     C                   EXSR      RTVTA4
050100141107     C*
050200141107     C*  Tipo record 'CNI'
050300141107     C                   eval      Conta_segm  = Conta_segm  + 1
050400141107     C                   reset                   CLI_DS_CNI
050500141107     C                   z-add     Conta_segm    wNum005_0
050600141107     C                   z-add     stbRMN        wNum010_0
050700141107     C                   eval      CD_CNI_SEQUEN = %editc(wNum005_0:'X')
050800170516     C                   eval      CD_CNI_SHPRMN = *blanks
050900141107     C                   eval      CD_CNI_STSCOD = wStsEvt
051000141107     C                   eval      CD_CNI_STSDES = wStsDes
051100141107     C                   eval      CD_CNI_STSRSN = wStsRsn
051200141107     C                   eval      CD_CNI_SHPRMA = stbRMA
051300141107     C                   eval      CD_CNI_SHPDSP = %editc(stbDSP:'X')
051400141107     C                   eval      CD_CNI_STSDAT = %editc(stbDAS:'X') +
051500141107     C                                             %editc(stbORS:'X') + '+0100'
051600141107     C                   if        %subst(stbCOS:1:6) = 'CONSOK'
051700141107     C                   eval      CD_CNI_SHPOK  = 'ST'
051800141107     C                   eval      CD_CNI_SHPRSD = tasRSD
051900141107     C                   else
052000141107     C                   eval      CD_CNI_SHPOK  = *blanks
052100141107     C                   eval      CD_CNI_SHPRSD = *blanks
052200141107     C                   endif
052300170516     C                   eval      CD_CNI_RGRSHP = §TA4ANAS
052400141107     C                   clear                   vawDTA
052500141107     C                   eval      vawDTA = CLI_DS_CNI
052600141107     C                   exsr      wriOUTPUT
052700031103     C*
052800031103     C                   ENDSR
052900170516
053000170516
053100170516
053200170516     C     RTVTA4        BEGSR
053300170516     C*
053400170516     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
053500170516     C                   CLEAR                   DTA4A
053600170516     C*
053700170516     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
053800170516     C                   CALL      'UBTA400R'
053900170516     C                   PARM      *blanks       UBTA4IOPZ
054000170516     C                   PARM      *blanks       UBTA4ITLA
054100170516     C                   PARM      tasAAS        UBTA4IAAS
054200170516     C                   PARM      tasLNP        UBTA4ILNP
054300170516     C                   PARM      tasNRS        UBTA4INRS
054400170516     C                   PARM      tasNSP        UBTA4INSP
054500170516     C                   PARM                    UBTA4ITRC
054600170516     C                   PARM                    UBTA4OERR
054700170516     C                   PARM                    UBTA4ODS
054800170516     C                   PARM                    UBTA4OLEN
054900170516     C                   PARM                    UBTA4ODATI
055000170516     C*
055100170516     C                   IF        UBTA4OERR = *zeros
055200170516     C                   SELECT
055300170516     C* Gestione output tipo record 'A'
055400170516     C                   WHEN      UBTA4ODS = 'DTA4A'
055500170516     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
055600170516     C                   ENDSL
055700170516     C                   ENDIF
055800170516     C*
055900170516     C                   ENDSR
056000170516     C*--------------------------------------------------------
056100161129
056200161129
056300161129
056400161129     C*--------------------------------------------------------------------------------------------*
056500161129     C* /TITLE Compongo il testo e spedisco una e-mail di alert
056600161129     C*--------------------------------------------------------------------------------------------*
056700161129     C     sndAlert      BEGSR
056800161129     C*
056900161129     C* Inizializzo variabili
057000161129     C                   movel     *blanks       wrkEml          253
057100161129     C                   movel     *blanks       wrkCcEml        253
057200161129     C                   movel     *blanks       wrkEmlMsg      5000
057300161129     C                   movel     *blanks       wrkEmlOgg        44
057400161129     C*
057500161129     C* Valorizzo i campi della e-m@ail
057600161129     C                   eval      wrkEml='cedalert@brt.it'
057700161129     C                   eval      wrkEmlOgg=
057800161129     C                               'BARTVAS - Problema Mappatura TIVMS00F'
057900161129     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
058000161129     C                                                                    ':/N'+
058100161129     C                                                                    ':/N'+
058200161129     C                             'Verificare la mappatura: '           +':/N'+
058300161129     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
058400161129     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
058500161129     C                                                                    ':/N'+
058600161129     C                             'Causale mancante: ' + stbCOS         +':/N'+
058700161129     C                             ' '                                   +':/N'
058800161129     C*
058900161129     C                   call(e)   'TIS701C1'
059000161129     C                   parm                    wrkEml
059100161129     C                   parm                    wrkCcEml
059200161129     C                   parm                    wrkEmlOgg
059300161129     C                   parm                    wrkEmlMsg
059400161129     C                   parm                    wrkEsito          1
059500161129     C*
059600161129     C                   ENDSR
059700031103
059800031103
059900991027
060000991027      /TITLE Operazioni iniziali.
060100991027     C     *inzsr        BEGSR
060200991027     C*
060300991027     C     *ENTRY        PLIST
060400031103     C                   parm                    prmppi
060500991027     C     wrkesito      parm      wrkesito      prmesito
060600031103     C*
060700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
060800031103     C                   EVAL      DSPPI = prmppi
060900131004     C*
061000131004     C* Reperisco i valori variabili ma a livello di transazione corrente
061100131004     C* (tipicamente data e ora relativ al file generato)
061200141107     C*
061300141107     C* CALCOLA LA DATA CORRENTE
061400141107     C                   time                    wn14             14 0
061500141107     C                   movel     wn14          oracor6           6 0          *ORA 6
061600141107     C                   movel     wn14          oracor4           4 0          *ORA 4
061700141107     C                   z-add     *zeros        datcor8           8 0          *DATA CORRENTE AA/M/
061800141107     C                   eval      datcor8 = %dec(%date() : *ISO)
061900030709     C*
062000030709     C* Definizione chiavi
062100030924     C*
062200110103     C* Chiave su TISTB03L - Parziale
062300110103     C     KEYstb03P     KLIST
062400031104     C                   KFLD                    wKstbKSU
062500031103     C                   KFLD                    wKstbFTR
062600031103     C*
062700031103     C* Chiave su TITAS30C - Parziale
062800031103     C     KEYtas30P     KLIST
062900031103     C                   KFLD                    stbAAS
063000031103     C                   KFLD                    stbLNP
063100031103     C                   KFLD                    stbNRS
063200031103     C                   KFLD                    stbNSP
063300161129     C*
063400161129     C* Chiave su TIVMS01I - Parziale
063500161129     C     KEYvms01P     KLIST
063600161129     C                   KFLD                    vmsPROC
063700161129     C                   KFLD                    vmsKSU
063800161129     C                   KFLD                    vmsDDE
063900991027     C*
064000991027     C                   ENDSR
