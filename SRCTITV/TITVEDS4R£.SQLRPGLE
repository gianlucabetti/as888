000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500150113     FTIVIRE1L  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800160914     FTIVMS01I  IF   E           K DISK
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000150113
001100150113
001200150113     D*------------------
001300150113     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
001400150113     D*------------------
001500150113     D InStringa       S          65535A   VARYING                              (stringa input)
001600150113     D InSepara        S             10A                                        (separatore)
001700150113     D InTypeOut       S              1A                                        (tipo output)
001800150113     D wSkParam        S          65535A                                        (output)
001900150113     D OutErrore       S              1A                                        (flag errore)
002000150113     D OutMsg          S             80A                                        (messaggio errore)
002100150113     D SkSplitCSV_6    S            512    DIM(128)
002200150113
002300030924
002400041015     D*-----------
002500041015     D* VARIABILI D WRK
002600041015     D*-----------
002700031103     D prmppi          S             50
002800030924     D prmesito        S              1
002900030924     D wrkesito        S                   like(prmesito)
003000131004     D campo_alfa      S            512
003100160914     D wStsEvt         S             25
003200160914     D wStsRsn         S             25
003300160914     D currDate6       S              6
003400160914     D currDate6inv    S              6
003500160914     D currDate8       S              8
003600160914     D currHHMM        S              4
003700160914     D currHHMMSS      S              6
003800131216     D Conta_msg       S             10i 0
003900131216     D Conta_segm      S             10i 0
004000160914     D*
004100160914     D w§UNB_ID        S            128    inz(*zeros)
004200160914     D w§NAD_SF        S            256    inz(*zeros)
004300131004
004400131004
004500131004     D*-----------
004600131004     D* DEFINIZIONE DS ESTERNE
004700131004     D*-----------
004800160914     D TRUL33DS      e ds                  inz
004900160914     D KPJBA         e ds                  inz
005000150113     D DVIREPE       e ds                  inz
005100031103
005200160914
005300160914     D*-----------
005400160914     D* SCHIERE
005500160914     D*-----------
005600160914     D NumElem         C                   CONST(300)
005700160914     D skCAU_BRT       S             15    DIM(NumElem)
005800160914     D skCAU_EVT       S             10    DIM(NumElem)
005900160914     D skCAU_RSN       S              5    DIM(NumElem)
006000160914     D skCAU_DES       S            200    DIM(NumElem)
006100160914     D jCAU            S              3  0 INZ(*zeros)
006200160914     D j               S              3  0 INZ(*zeros)
006300160914     D*-----------
006400160914     D* DS RIDEFINIZIONE STATUS CLIENTE
006500160914     D*-----------
006600160914     D DS_VMSSTSCLI    DS                  INZ
006700160914     D  dsCLI_EVT                    10
006800160914     D  dsCLI_RSN                     5
006900160914     D*-----------
007000160914     D* PARAMETRI CHIAMATA UBVMSDDER
007100160914     D*-----------
007200160914     D VMSDDE_iOpz     S              1    INZ
007300160914     D VMSDDE_iProc    S                   INZ LIKE(vmsPROC)
007400160914     D VMSDDE_iKSU     S                   INZ LIKE(vmsKSU)
007500160914     D VMSDDE_iDtRif   S              8S 0 INZ
007600160914     D VMSDDE_oDDE     S                   INZ LIKE(vmsDDE)
007700160914     D VMSDDE_oEsito   S              1    INZ
007800160914
007900160914
008000131004     D*------------------
008100131004     D* LINKING A DEFINIZIONI ESTERNE
008200131004     D*------------------
008300131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
008400131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008500131004
008600131004
008700031103     D*-----------
008800031103     D* VARIABILI RIFERITE AL DATA-BASE
008900031103     D*-----------
009000080618     D wKstbKSU        S                   like(stbKSU)
009100080618     D wKstbFTR        S                   like(stbFTR)
009200110705
009300031103
009400031103     D*-----------
009500031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
009600031103     D*-----------
009700150114     D DSPPI           DS
009800150114     D  w§PPIKSU                           like(vireKSC) inz
009900150114     D  w§PPITIP                           like(vireTIP) inz
010000150114     D  w§PPIDTI                      8                  inz
010100160914     D  w§PPIVMSPROC                       like(vmsPROC) inz
010200160914     D  w§PPIVMSKSU                        like(vmsKSU)  inz
010300131004
010400131004
010500131004     D*-----------
010600131004     D* COSTANTI / DEFAULT
010700131004     D*-----------
010800150113     D UNB_sgm         C                   CONST('UNB+UNOA:1+IT-BRT001:ZZ+')
010900131007     D UNH_sgm_1       C                   CONST('UNH+')
011000150527     D UNH_sgm_2       C                   CONST('+IFTSTA:D:96A:UN:EAN002')
011100150527     D BGM_7           C                   CONST('BGM+7+')
011200150527     D DTM_137         C                   CONST('DTM+137:')
011300150527     D NAD_CS          C                   CONST('NAD+CS+IT-BRT001')
011400150113     D NAD_SF          C                   CONST('NAD+SF+')
011500131004     D NAD_ST          C                   CONST('NAD+ST++')
011600131010     D LOC_sgm1        C                   CONST('LOC+175+')
011700131010     D LOC_sgm2        C                   CONST('::3:')
011800131010     D LOC_sgm3        C                   CONST('+IT')
011900131004     D CNI_sgm         C                   CONST('CNI+1')
012000141006     D STS_sgm         C                   CONST('STS+')
012100150527     D RFF_FF          C                   CONST('RFF+FF:')
012200150527     D RFF_CR          C                   CONST('RFF+CR:')
012300150113     D TDT_sgm         C                   CONST('TDT+30+1+++IT-BRT001')
012400150527     D DTM_9           C                   CONST('DTM+9:')
012500131004     D EQD_sgm         C                   CONST('EQD+AA')
012600160215     D GID_sgm1        C                   CONST('GID+1+')
012700160215     D GID_sgm2        C                   CONST(':CTN')
012800131004     D UNT_sgm         C                   CONST('UNT+')
012900131004     D UNZ_sgm         C                   CONST('UNZ+')
013000131004     D piu             C                   CONST('+')
013100131004     D diviso          C                   CONST(':')
013200131004     D apice           C                   CONST('''')
013300141006     D wStsTyp         C                   CONST('')
013400131004
013500030924
013600030325
013700030924     C* MAIN/
013800041015     C*
013900160914     C* Carica schiere di mappatura
014000160914     C                   SETOFF                                       40
014100160914     C                   EXSR      carMAP
014200160914     C*
014300160914     C* Se caricamento mappatura tutto OK => Procedo
014400160914     C                   IF        not *in40
014500160914     C*
014600160914     C* Spia dati da inviare
014700160914     C                   SETOFF                                       45
014800041015     C*
014900041015     C* Esegue traduzione
015000140308     C                   SETOFF                                       31
015100150113     C  N31              EXSR      SPLIT_PARMS
015200131004     C  N31              EXSR      chkRECFILMBR
015300131004     C  N31              EXSR      rtvID
015400150527     C***  N31              EXSR      wriHeader
015500131004     C  N31              EXSR      traduci
015600140308     C  N31
015700140308     CAN 45              EXSR      wriFooter
015800160914     C*
015900160914     C* Diversamente traduzioe completamente fallita
016000160914     C                   ELSE
016100160914     C                   EVAL      wrkesito = '2'
016200160914     C                   ENDIF
016300110705     C*
016400921023     C                   SETON                                        LR
016500131004
016600160914
016700131004
016800131004     C*----------------------------------------------------*
016900131004     C*  STACCO ID UNIVOCO
017000131004     C*----------------------------------------------------*
017100131004     C     RTVID         BEGSR
017200131004     C*
017300131004     C                   z-add     *zeros        currUNIQ_ID      15 0
017400131004     C*
017500131004     C* NSP => Stacco un numeratore da AZNUM
017600131004     C                   clear                   TRUL33DS
017700131004     C                   eval      I33OPE = *zeros
017800131004     C                   eval      I33CNU = 302
017900131004     C                   eval      I33NUM = 1
018000131004     C                   movel     TRUL33DS      KPJBU
018100131004     C                   call      'TRUL33R'
018200131004     C                   parm                    KPJBA
018300131004     C                   movel     KPJBU         TRUL33DS
018400131004     C                   if        O33ERR = *zeros
018500131004     C                   z-add     O33NRF        currUNIQ_ID
018600131004     C                   else
018700131004     C                   seton                                        31
018800131004     C                   endif
018900131004     C*
019000131004     C                   ENDSR
019100131004
019200160914
019300131004
019400131004     C     chkRECFILMBR  BEGSR
019500131004     C*
019600131004     C     *start        setll     TIVAWWWT
019700131004     C                   if        %found(TIVAWWWT)
019800131004     C                   seton                                        31
019900131004     C                   endif
020000131004     C*
020100131004     C                   ENDSR
020200131004
020300160914
020400131004
020500131004     C     wriOUTPUT     BEGSR
020600131004     C*
020700131004     C                   write     TIVAW000
020800131004     C*
020900131004     C                   ENDSR
021000130807
021100160914
021200041015
021300160914     C     carMAP        BEGSR
021400160914     C*
021500160914     C* Reperisco la versione di mappatura stati correntemente valida
021600160914     C                   call      'UBVMSDDER'
021700160914     C                   parm      *blanks       VMSDDE_iOpz
021800160914     C                   parm      w§PPIVMSPROC  VMSDDE_iProc
021900160914     C                   parm      w§PPIVMSKSU   VMSDDE_iKSU
022000160914     C                   parm      datcor        VMSDDE_iDtRif
022100160914     C                   parm                    VMSDDE_oDDE
022200160914     C                   parm                    VMSDDE_oEsito
022300160914     C*
022400160914     C* Verifico se esito no OK
022500160914     C                   if        VMSDDE_oEsito <> '1'
022600160914     C                   seton                                        40
022700160914     C                   else
022800160914     C*
022900160914     C                   move(P)   w§PPIVMSPROC  vmsPROC
023000160914     C                   move(P)   w§PPIVMSKSU   vmsKSU
023100160914     C                   z-add     VMSDDE_oDDE   vmsDDE
023200160914     C     KEYvms01P     setll     tivms01i
023300160914     C                   if        %equal(tivms01i)
023400160914     C     KEYvms01P     reade     tivms01i
023500160914     C*
023600160914     C* Inizializzo indice schiera di mapping
023700160914     C                   z-add     1             jCAU
023800160914     C*
023900160914     C* Ciclo x tutti i record della mappatura richiesta
024000160914     C                   dow       not %eof(tivms01i)
024100160914     C*
024200160914     C* Ignoro i record annullati
024300160914     C                   if        vmsATB = *blanks
024400160914     C*
024500160914     C* Carico la mappatura dei codici status personalizzati cliente
024600160914     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
024700160914     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
024800160914     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
024900160914     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
025000160914     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
025100160914     C                   add       1             jCAU
025200160914     C*
025300160914     C                   endif
025400160914     C*
025500160914     C     KEYvms01P     reade     tivms01i
025600160914     C                   enddo
025700160914     C*
025800160914     C                   endif
025900160914     C*
026000160914     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
026100160914     C* altrimenti errore bloccante per intera traduzione
026200160914     C                   if        jCAU > 1
026300160914     C                   else
026400160914     C                   seton                                        40
026500160914     C                   endif
026600160914     C*
026700160914     C                   endif
026800131104     C*
026900131104     C                   ENDSR
027000131104
027100131004
027200131004
027300131004     C     Char_Speciali BEGSR
027400131111     C*
027500131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
027600131111     C*
027700131111     C/EXEC SQL
027800131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
027900131111     C/END-EXEC
028000131111     C*
028100131111     C/EXEC SQL
028200131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
028300131111     C/END-EXEC
028400131111     C*
028500131111     C/EXEC SQL
028600131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
028700131111     C/END-EXEC
028800131111     C*
028900131111     C/EXEC SQL
029000131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
029100131111     C/END-EXEC
029200131111     C*
029300131111     C/EXEC SQL
029400131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
029500131111     C/END-EXEC
029600131004     C*
029700131004     C                   ENDSR
029800131004
029900131004
030000150113
030100150113     C     wriHeader     BEGSR
030200131004     C*
030300131004     C                   clear                   vawDTA
030400131010     C*
030500131004     C*  UNB -
030600131004     C                   eval      vawDTA = UNB_sgm
030700150113     C                              + %trim(w§UNB_ID)
030800131004     C                              +   piu
030900131004     C                              + %trim(currDate6inv)
031000131004     C                              +   diviso
031100131004     C                              + %trim(currHHMM)
031200131004     C                              +   piu
031300131004     C                              + %trim(
031400131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
031500131004     C                              +   apice
031600131004     C                   exsr      wriOUTPUT
031700131004     C*
031800131004     C                   ENDSR
031900131004
032000131004
032100131004
032200131004     C     wriFooter     BEGSR
032300131004     C*
032400131004     C                   clear                   vawDTA
032500131004     C*
032600131004     C*  UNZ -
032700131004     C                   eval      vawDTA = UNZ_sgm
032800131004     C                              + %trim(%editc(Conta_msg:'4'))
032900131004     C                              +   piu
033000131004     C                              + %trim(
033100131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
033200131004     C                              +   apice
033300131004     C                   exsr      wriOUTPUT
033400131004     C*
033500131004     C                   ENDSR
033600131004
033700030924
033800030924
033900991027     C     traduci       BEGSR
034000131004     C*
034100131004     C* Inizializzo i contatori a livello di transazione/messaggio
034200131004     C                   z-add     *zeros        Conta_msg
034300031103     C*
034400031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
034500031103     C* di traduzione (trattasi d "post-traduzione")
034600150113     C                   MOVE(P)   w§PPIKSU      wKstbKSU
034700031103     C                   EVAL      wKstbFTR = *blanks
034800110103     C     KEYstb03P     SETLL     tistb03l
034900110103     C                   IF        %equal(tistb03l)
035000110103     C     KEYstb03P     READE     tistb03l
035100991027     C*
035200031103     C* Ciclo x tuttii record del cliente da trasmettere
035300110103     C                   DOW       not %eof(tistb03l)
035400131004     C*
035500131004     C* Inizializzo i contatori a livello di segmento
035600131004     C                   z-add     *zeros        Conta_segm
035700080618     C*
035800130807     C                   SETON                                        50
035900031103     C*
036000031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
036100031103     C* deve restituire al cliente nel file tradotto
036200031103     C                   IF        stbFTR = *blanks                             * ridondante
036300110103     C*
036400110103     C* Gestisco solamente gli stati standard
036500130808     C                   IF        (stbTIS= '1' OR
036600130906     C                             (stbTIS= '9' AND
036700151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
036800151117     C                              (stbPRS= '1'                ))) AND
036900130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
037000130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
037100131004     C*
037200131004     C* Per "sicurezza" non invio stati senza data
037300131004     C                   IF        stbDAS > *zeros
037400110103     C*
037500110103     C                   SETOFF                                       50
037600031103     C*
037700031103     C* Innanzitutto chaino la bolla direttamente su TITAS
037800031103     C     KEYtas30P     CHAIN     titas30c
037900031103     C                   IF        %found(titas30c)
038000131010     C*
038100131010     C* Innanzitutto chaino la bolla direttamente su TITAS
038200131216     C                   MOVEL     *zeros        wPO               3 0
038300131213     C*
038400131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
038500131213     C                   if        tasDTI = *zeros
038600131213     C                   eval      wPO = tasLNP
038700131213     C                   else
038800131213     C                   eval      wPO = tasLNA
038900131213     C                   endif
039000131213     C*
039100131213     C     wPO           CHAIN     azorg01l
039200131010     C                   IF        %found(azorg01l)
039300080618     C*
039400110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
039500140606     C                   SETOFF                                       80
039600031103     C                   Z-ADD     1             j
039700160914     C                   MOVEL(P)  stbCOS        wCOS_15          15
039800160914     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
039900141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
040000141006     C                   EVAL      wStsEvt = skCAU_EVT(j)
040100141006     C                   EVAL      wStsRsn = skCAU_RSN(j)
040200140514     C                   SETOFF                                       50
040300140606     C                   SETON                                        80
040400031103     C                   ELSE
040500031103     C                   SETON                                        50
040600160914     C                   ENDIF
040700160914     C  N55              EXSR      sndAlert
040800140308     C*
040900140308     C* Al primo dettaglio da inviare => scrivo anche testata
041000140313     C                   IF        not *IN50
041100140313     C  N45              EXSR      wriHeader
041200140308     C                   SETON                                        45
041300140313     C                   ENDIF
041400080618     C*
041500080618     C* Scarico il buffer d output
041600131004     C  N50              EXSR      wriStato
041700110103     C*
041800131010     C                   ENDIF
041900131004     C                   ENDIF
042000110103     C                   ENDIF
042100110103     C                   ENDIF
042200110103     C                   ENDIF
042300031103     C*
042400031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
042500031103     C                   EVAL      stbFTR = 'T'
042600031103     C                   UPDATE    tistb000
042700991027     C*
042800110103     C     KEYstb03P     READE     tistb03l
042900031103     C                   SETOFF                                       50
043000030325     C                   ENDDO
043100031103     C                   ENDIF
043200991027     C*
043300030325     C                   EVAL      wrkesito = '0'
043400991027     C*
043500910830     C                   ENDSR
043600031103
043700031103
043800031103
043900131004     C     wriStato      BEGSR
044000131004     C*
044100131004     C* Incremento il contatore di messaggio (al interno della transazione)
044200131004     C                   eval      Conta_msg = Conta_msg + 1
044300131004     C*
044400131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
044500131104     C                   movel     *blanks       wAlfaRMA        512
044600131024     C                   movel     *blanks       wAlfaRSD        512
044700131004     C                   movel     *blanks       wAlfaLOD        512
044800131004     C                   movel     *blanks       wAlfaIND        512
044900131025     C                   movel     *blanks       wAlfaORGDES     512
045000131004     C* RMA
045100131004     C                   eval      campo_alfa = stbRMA
045200131004     C                   exsr      Char_Speciali
045300131004     C                   eval      wAlfaRMA = campo_alfa
045400131024     C* RSD
045500131024     C                   eval      campo_alfa = tasRSD
045600131024     C                   exsr      Char_Speciali
045700131024     C                   eval      wAlfaRSD = campo_alfa
045800131004     C* LOD
045900131004     C                   eval      campo_alfa = tasLOD
046000131004     C                   exsr      Char_Speciali
046100131004     C                   eval      wAlfaLOD = campo_alfa
046200131004     C* IND
046300131004     C                   eval      campo_alfa = tasIND
046400131004     C                   exsr      Char_Speciali
046500131004     C                   eval      wAlfaIND = campo_alfa
046600131025     C* ORGDES
046700131025     C                   eval      campo_alfa = orgDES
046800131025     C                   exsr      Char_Speciali
046900131025     C                   eval      wAlfaORGDES = campo_alfa
047000131004     C*
047100131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
047200131004     C* (ancora corretto ???)
047300131004     C                   z-add     *zeros        wPeso             7 1
047400131004     C                   if        tasPKB > tasPKC
047500131004     C                   eval      wPeso = tasPKB
047600131004     C                   else
047700131004     C                   eval      wPeso = tasPKC
047800131004     C                   endif
047900131004     C*
048000131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
048100131004     C*
048200131004     C*  UNH -
048300131004     C                   eval      Conta_segm  = Conta_segm  + 1
048400131007     C                   eval      vawDTA = UNH_sgm_1
048500150527     C                              + %trim(currDate8) + %trim(currHHMMSS)
048600150527     C                              + UNH_sgm_2
048700150527     C                              +   apice
048800131004     C                   exsr      wriOUTPUT
048900131004     C*
049000150527     C*  BGM 7 -
049100131004     C                   eval      Conta_segm  = Conta_segm  + 1
049200150527     C                   eval      vawDTA = BGM_7
049300131004     C                              +   apice
049400131004     C                   exsr      wriOUTPUT
049500150527     C*
049600150527     C*  DTM 137 -
049700150527     C                   eval      Conta_segm  = Conta_segm  + 1
049800150527     C                   eval      vawDTA = DTM_137
049900150527     C                              + %trim(currDate8) + %trim(currHHMMSS)
050000150527     C                              +   diviso
050100150527     C                              + '204'
050200150527     C                              +   apice
050300150527     C                   exsr      wriOUTPUT
050400150527     C*
050500150527     C*  NAD CS -
050600150527     C                   eval      Conta_segm  = Conta_segm  + 1
050700150527     C                   eval      vawDTA = NAD_CS
050800150527     C                              +   apice
050900150527     C                   exsr      wriOUTPUT
051000131004     C*
051100150113     C*  NAD SF -
051200150113     C                   eval      Conta_segm  = Conta_segm  + 1
051300150113     C                   eval      vawDTA = NAD_SF
051400150113     C                              + %trim(w§NAD_SF)
051500150113     C                              +   apice
051600150113     C                   exsr      wriOUTPUT
051700131004     C*
051800131004     C*  NAD ST -
051900131004     C                   eval      Conta_segm  = Conta_segm  + 1
052000131004     C                   eval      vawDTA = NAD_ST
052100131025     C                              + %trim(wAlfaRSD)
052200131004     C                              +   piu
052300131004     C                              +   piu
052400131004     C                              + %trim(wAlfaIND)
052500131004     C                              +   piu
052600131004     C                              + %trim(wAlfaLOD)
052700131004     C                              +   piu
052800131004     C                              + %trim(tasPRD)
052900131004     C                              +   piu
053000131004     C                              + %trim(tasCAD)
053100131004     C                              +   piu
053200131004     C                              + 'IT'
053300131004     C                              +   apice
053400131004     C                   exsr      wriOUTPUT
053500131004     C*
053600131010     C*  LOC -
053700131004     C                   eval      Conta_segm  = Conta_segm  + 1
053800131004     C                   eval      vawDTA = LOC_sgm1
053900131010     C                              + %trim(%editc(orgFIL:'X'))
054000131004     C                              +   LOC_sgm2
054100131025     C                              + %trim(wAlfaORGDES)
054200131010     C                              +   LOC_sgm3
054300131004     C                              +   apice
054400131004     C                   exsr      wriOUTPUT
054500131004     C*
054600150527     C*  CNI -
054700131004     C                   eval      Conta_segm  = Conta_segm  + 1
054800131004     C                   eval      vawDTA = CNI_sgm
054900131004     C                              +   apice
055000131004     C                   exsr      wriOUTPUT
055100131004     C*
055200131004     C*  STS -
055300131004     C                   eval      Conta_segm  = Conta_segm  + 1
055400141006     C                   eval      vawDTA = STS_sgm
055500150113     C                              + %trim(wStsTyp) + '+'
055600150113     C                              + %trim(wStsEvt) + '+'
055700141006     C                              + %trim(wStsRsn)
055800131004     C                              +   apice
055900131004     C                   exsr      wriOUTPUT
056000131004     C*
056100150527     C*  RFF FF -
056200131004     C                   eval      Conta_segm  = Conta_segm  + 1
056300150527     C                   eval      vawDTA = RFF_FF
056400150527     C                              + %trim(%editc(stbRMN:'4'))
056500131004     C                              +   apice
056600131004     C                   exsr      wriOUTPUT
056700150527     C*
056800150527     C*  RFF CR -
056900150527     C                   eval      Conta_segm  = Conta_segm  + 1
057000150527     C                   eval      vawDTA = RFF_CR
057100150527     C                              + %trim(wAlfaRMA)
057200150527     C                              +   apice
057300150527     C                   exsr      wriOUTPUT
057400131004     C*
057500131004     C*  TDT -
057600131004     C                   eval      Conta_segm  = Conta_segm  + 1
057700131004     C                   eval      vawDTA = TDT_sgm
057800131004     C                              +   apice
057900131004     C                   exsr      wriOUTPUT
058000131004     C*
058100150527     C*  DTM 9 -
058200131004     C                   eval      Conta_segm  = Conta_segm  + 1
058300150527     C                   eval      vawDTA = DTM_9
058400131007     C                              + %trim(%editc(stbDAS*10000+stbORS:'4'))
058500131007     C                              + '00'
058600131004     C                              +   diviso
058700131007     C                              + '204'
058800131004     C                              +   apice
058900131004     C                   exsr      wriOUTPUT
059000131004     C*
059100131004     C*  EQD -
059200131004     C                   eval      Conta_segm  = Conta_segm  + 1
059300131004     C                   eval      vawDTA = EQD_sgm
059400131004     C                              +   apice
059500131004     C                   exsr      wriOUTPUT
059600160215     C*
059700160215     C*  GID -
059800160215     C                   eval      Conta_segm  = Conta_segm  + 1
059900160215     C                   eval      vawDTA = GID_sgm1
060000160215     C                              + %trim(%editc(tasNCL:'4'))
060100160215     C                              +   GID_sgm2
060200160215     C                              +   apice
060300160215     C                   exsr      wriOUTPUT
060400131004     C*
060500131004     C*  UNT -
060600131004     C                   eval      Conta_segm  = Conta_segm  + 1
060700131004     C                   eval      vawDTA = UNT_sgm
060800131004     C                              + %trim(%editc(Conta_segm:'4'))
060900131004     C                              +   piu
061000131007     C                              + %trim(%editc(Conta_msg:'4'))
061100131004     C                              +   apice
061200131004     C                   exsr      wriOUTPUT
061300031103     C*
061400031103     C                   ENDSR
061500150113
061600160914
061700150113
061800150113     C*----------------------------------------------------*
061900150113     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
062000150113     C*----------------------------------------------------*
062100150113     C     SPLIT_PARMS   BEGSR
062200150113     C*
062300150113     C                   reset                   w§UNB_ID
062400150113     C                   reset                   w§NAD_SF
062500150113     C*
062600150113     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
062700150113     C                   CLEAR                   DVIREPE
062800150113     C                   EVAL      vireTRC = 'PE'
062900150114     C                   MOVE(P)   w§PPIDTI      vireDTI
063000150113     C     KEYvire1C     CHAIN     tivire1l
063100150113     C                   IF        %found(tivire1l)
063200150113     C                   EVAL      DVIREPE = vireFLO
063300150113     C*
063400150113     C* Se reperiti parametri => effettuo splitting
063500150113     C                   eval      InStringa = %trim(§VIREPEKPJ)
063600150113     C*
063700150113     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
063800150113     C                   CALL      'XSPLIT2'
063900150113     C                   PARM                    InStringa
064000150113     C                   PARM      '§'           InSepara
064100150113     C                   PARM      '6'           InTypeOut
064200150113     C                   PARM      ''            wSkParam
064300150113     C                   PARM                    OutErrore
064400150113     C                   PARM                    OutMsg
064500150113     C                   MOVEA     wSkParam      SkSplitCSV_6
064600150113     C*
064700150113     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
064800150113     C                   z-add     1             i                 4 0
064900150113     C                   dow       i <= %elem(SkSplitCSV_6)
065000150113     C                   if        SkSplitCSV_6(i) = *blanks
065100150113     C                   leave
065200150113     C                   else
065300150113     C                   select
065400150113     C* - parametro UNB
065500150113     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
065600150113     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
065700150113     C* - parametro NAD+SF
065800150113     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
065900150113     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
066000150113     C*
066100150113     C                   endsl
066200150113     C                   endif
066300150113     C*
066400150113     C                   add       1             i
066500150113     C                   enddo
066600150113     C*
066700150113     C                   ENDIF
066800150113     C*
066900150113     C                   ENDSR
067000160914
067100160914
067200160914
067300160914     C*--------------------------------------------------------------------------------------------*
067400160914     C* /TITLE Compongo il testo e spedisco una e-mail di alert
067500160914     C*--------------------------------------------------------------------------------------------*
067600160914     C     sndAlert      BEGSR
067700160914     C*
067800160914     C* Inizializzo variabili
067900160914     C                   movel     *blanks       wrkEml          253
068000160914     C                   movel     *blanks       wrkCcEml        253
068100160914     C                   movel     *blanks       wrkEmlMsg      5000
068200160914     C                   movel     *blanks       wrkEmlOgg        44
068300160914     C*
068400160914     C* Valorizzo i campi della e-m@ail
068500160914     C                   eval      wrkEml='cedalert@brt.it'
068600160914     C                   eval      wrkCcEml='fabrizio.gurrieri@brt.it'
068700160914     C                   eval      wrkEmlOgg=
068800160914     C                               'BARTVAS - Problema Mappatura TIVMS00F'
068900160914     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
069000160914     C                                                                    ':/N'+
069100160914     C                                                                    ':/N'+
069200160914     C                             'Verificare la mappatura: '           +':/N'+
069300160914     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
069400160914     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
069500160914     C                                                                    ':/N'+
069600160914     C                             'Causale mancante: ' + stbCOS         +':/N'+
069700160914     C                             ' '                                   +':/N'
069800160914     C*
069900160914     C                   call(e)   'TIS701C1'
070000160914     C                   parm                    wrkEml
070100160914     C                   parm                    wrkCcEml
070200160914     C                   parm                    wrkEmlOgg
070300160914     C                   parm                    wrkEmlMsg
070400160914     C*
070500160914     C                   ENDSR
070600031103
070700031103
070800991027
070900991027      /TITLE Operazioni iniziali.
071000991027     C     *inzsr        BEGSR
071100991027     C*
071200991027     C     *ENTRY        PLIST
071300150113     C                   parm                    prmppi
071400991027     C     wrkesito      parm      wrkesito      prmesito
071500160914     C*
071600160914     C* Reperisco la data corrente
071700160914     C                   z-add     *zero         datcor            8 0
071800160914     C                   eval      datcor = %dec(%date() : *iso)
071900031103     C*
072000031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
072100150113     C                   EVAL      DSPPI = prmppi
072200131004     C*
072300131004     C* Reperisco i valori variabili ma a livello di transazione corrente
072400131004     C* (tipicamente data e ora relativ al file generato)
072500131004     C                   eval      currDate6    =
072600131004     C                              UBFMTDATE_Convert(
072700131004     C                                  %editc(%dec(%date():*ISO):'X') :
072800131004     C                                  'YYYYMMDD' : 'YYMMDD')
072900131004     C                   eval      currDate6inv =
073000131004     C                              UBFMTDATE_Convert(
073100131004     C                                  %editc(%dec(%date():*ISO):'X') :
073200131004     C                                  'YYYYMMDD' : 'DDMMYY')
073300150527     C                   eval      currDate8    =
073400150527     C                                  %editc(%dec(%date():*ISO):'X')
073500131004     C                   eval      currHHMM =
073600131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
073700150527     C                   eval      currHHMMSS =
073800150527     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:6)
073900030709     C*
074000030709     C* Definizione chiavi
074100030924     C*
074200110103     C* Chiave su TISTB03L - Parziale
074300110103     C     KEYstb03P     KLIST
074400031104     C                   KFLD                    wKstbKSU
074500031103     C                   KFLD                    wKstbFTR
074600031103     C*
074700031103     C* Chiave su TITAS30C - Parziale
074800031103     C     KEYtas30P     KLIST
074900031103     C                   KFLD                    stbAAS
075000031103     C                   KFLD                    stbLNP
075100031103     C                   KFLD                    stbNRS
075200031103     C                   KFLD                    stbNSP
075300150113     C*
075400150113     C* Chiave su TIVIRE1L - Parziale
075500150113     C     KEYvire1C     KLIST
075600150113     C                   KFLD                    w§PPIKSU
075700150113     C                   KFLD                    w§PPITIP
075800150114     C                   KFLD                    vireDTI
075900150113     C                   KFLD                    vireTRC
076000160914     C*
076100160914     C* Chiave su TIVMS01I - Parziale
076200160914     C     KEYvms01P     KLIST
076300160914     C                   KFLD                    vmsPROC
076400160914     C                   KFLD                    vmsKSU
076500160914     C                   KFLD                    vmsDDE
076600991027     C*
076700991027     C                   ENDSR
