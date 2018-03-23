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
007200160914       /copy gaitrasrc/srcProtoPI,UBVMSDDER
007300160914
007400160914
007500131004     D*------------------
007600131004     D* LINKING A DEFINIZIONI ESTERNE
007700131004     D*------------------
007800131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
007900131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
008000131004
008100131004
008200031103     D*-----------
008300031103     D* VARIABILI RIFERITE AL DATA-BASE
008400031103     D*-----------
008500080618     D wKstbKSU        S                   like(stbKSU)
008600080618     D wKstbFTR        S                   like(stbFTR)
008700110705
008800031103
008900031103     D*-----------
009000031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
009100031103     D*-----------
009200150114     D DSPPI           DS
009300150114     D  w§PPIKSU                           like(vireKSC) inz
009400150114     D  w§PPITIP                           like(vireTIP) inz
009500150114     D  w§PPIDTI                      8                  inz
009600160914     D  w§PPIVMSPROC                       like(vmsPROC) inz
009700160914     D  w§PPIVMSKSU                        like(vmsKSU)  inz
009800131004
009900131004
010000131004     D*-----------
010100131004     D* COSTANTI / DEFAULT
010200131004     D*-----------
010300150113     D UNB_sgm         C                   CONST('UNB+UNOA:1+IT-BRT001:ZZ+')
010400131007     D UNH_sgm_1       C                   CONST('UNH+')
010500150527     D UNH_sgm_2       C                   CONST('+IFTSTA:D:96A:UN:EAN002')
010600150527     D BGM_7           C                   CONST('BGM+7+')
010700150527     D DTM_137         C                   CONST('DTM+137:')
010800150527     D NAD_CS          C                   CONST('NAD+CS+IT-BRT001')
010900150113     D NAD_SF          C                   CONST('NAD+SF+')
011000131004     D NAD_ST          C                   CONST('NAD+ST++')
011100131010     D LOC_sgm1        C                   CONST('LOC+175+')
011200131010     D LOC_sgm2        C                   CONST('::3:')
011300131010     D LOC_sgm3        C                   CONST('+IT')
011400131004     D CNI_sgm         C                   CONST('CNI+1')
011500141006     D STS_sgm         C                   CONST('STS+')
011600150527     D RFF_FF          C                   CONST('RFF+FF:')
011700150527     D RFF_CR          C                   CONST('RFF+CR:')
011800170303     D RFF_ACL         C                   CONST('RFF+ACL:')
011900150113     D TDT_sgm         C                   CONST('TDT+30+1+++IT-BRT001')
012000150527     D DTM_9           C                   CONST('DTM+9:')
012100131004     D EQD_sgm         C                   CONST('EQD+AA')
012200160215     D GID_sgm1        C                   CONST('GID+1+')
012300160215     D GID_sgm2        C                   CONST(':CTN')
012400131004     D UNT_sgm         C                   CONST('UNT+')
012500131004     D UNZ_sgm         C                   CONST('UNZ+')
012600131004     D piu             C                   CONST('+')
012700131004     D diviso          C                   CONST(':')
012800131004     D apice           C                   CONST('''')
012900141006     D wStsTyp         C                   CONST('')
013000131004
013100030924
013200030325
013300030924     C* MAIN/
013400170123     C*
013500170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013600170123     C
013700170123     C/EXEC SQL
013800170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013900170123     C/END-EXEC
014000041015     C*
014100160914     C* Carica schiere di mappatura
014200160914     C                   SETOFF                                       40
014300160914     C                   EXSR      carMAP
014400160914     C*
014500160914     C* Se caricamento mappatura tutto OK => Procedo
014600160914     C                   IF        not *in40
014700160914     C*
014800160914     C* Spia dati da inviare
014900160914     C                   SETOFF                                       45
015000041015     C*
015100041015     C* Esegue traduzione
015200140308     C                   SETOFF                                       31
015300150113     C  N31              EXSR      SPLIT_PARMS
015400131004     C  N31              EXSR      chkRECFILMBR
015500131004     C  N31              EXSR      rtvID
015600150527     C***  N31              EXSR      wriHeader
015700131004     C  N31              EXSR      traduci
015800140308     C  N31
015900140308     CAN 45              EXSR      wriFooter
016000160914     C*
016100160914     C* Diversamente traduzioe completamente fallita
016200160914     C                   ELSE
016300160914     C                   EVAL      wrkesito = '2'
016400160914     C                   ENDIF
016500110705     C*
016600921023     C                   SETON                                        LR
016700131004
016800160914
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
019400160914
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
020500160914
020600131004
020700131004     C     wriOUTPUT     BEGSR
020800131004     C*
020900131004     C                   write     TIVAW000
021000131004     C*
021100131004     C                   ENDSR
021200130807
021300160914
021400041015
021500160914     C     carMAP        BEGSR
021600160914     C*
021700160914     C* Reperisco la versione di mappatura stati correntemente valida
021800160914     C                   call      'UBVMSDDER'
021900160924     C                   parm      *blanks       iVMSDDEOpz
022000160924     C                   parm      w§PPIVMSPROC  iVMSDDEProc
022100160924     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
022200160924     C                   parm      datcor        iVMSDDEDtRif
022300160924     C                   parm                    oVMSDDEDDE
022400160924     C                   parm                    oVMSDDEEsito
022500160914     C*
022600160914     C* Verifico se esito no OK
022700160924     C                   if        oVMSDDEEsito <> '1'
022800160914     C                   seton                                        40
022900160914     C                   else
023000160914     C*
023100160914     C                   move(P)   w§PPIVMSPROC  vmsPROC
023200160914     C                   move(P)   w§PPIVMSKSU   vmsKSU
023300160924     C                   z-add     oVMSDDEDDE    vmsDDE
023400160914     C     KEYvms01P     setll     tivms01i
023500160914     C                   if        %equal(tivms01i)
023600160914     C     KEYvms01P     reade     tivms01i
023700160914     C*
023800160914     C* Inizializzo indice schiera di mapping
023900160914     C                   z-add     1             jCAU
024000160914     C*
024100160914     C* Ciclo x tutti i record della mappatura richiesta
024200160914     C                   dow       not %eof(tivms01i)
024300160914     C*
024400160914     C* Ignoro i record annullati
024500160914     C                   if        vmsATB = *blanks
024600160914     C*
024700160914     C* Carico la mappatura dei codici status personalizzati cliente
024800160914     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
024900160914     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
025000160914     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
025100160914     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
025200160914     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
025300160914     C                   add       1             jCAU
025400160914     C*
025500160914     C                   endif
025600160914     C*
025700160914     C     KEYvms01P     reade     tivms01i
025800160914     C                   enddo
025900160914     C*
026000160914     C                   endif
026100160914     C*
026200160914     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
026300160914     C* altrimenti errore bloccante per intera traduzione
026400160914     C                   if        jCAU > 1
026500160914     C                   else
026600160914     C                   seton                                        40
026700160914     C                   endif
026800160914     C*
026900160914     C                   endif
027000131104     C*
027100131104     C                   ENDSR
027200131104
027300131004
027400131004
027500131004     C     Char_Speciali BEGSR
027600131111     C*
027700131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
027800131111     C*
027900131111     C/EXEC SQL
028000131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
028100131111     C/END-EXEC
028200131111     C*
028300131111     C/EXEC SQL
028400131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
028500131111     C/END-EXEC
028600131111     C*
028700131111     C/EXEC SQL
028800131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
028900131111     C/END-EXEC
029000131111     C*
029100131111     C/EXEC SQL
029200131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
029300131111     C/END-EXEC
029400131111     C*
029500131111     C/EXEC SQL
029600131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
029700131111     C/END-EXEC
029800131004     C*
029900131004     C                   ENDSR
030000131004
030100131004
030200150113
030300150113     C     wriHeader     BEGSR
030400131004     C*
030500131004     C                   clear                   vawDTA
030600131010     C*
030700131004     C*  UNB -
030800131004     C                   eval      vawDTA = UNB_sgm
030900150113     C                              + %trim(w§UNB_ID)
031000131004     C                              +   piu
031100131004     C                              + %trim(currDate6inv)
031200131004     C                              +   diviso
031300131004     C                              + %trim(currHHMM)
031400131004     C                              +   piu
031500131004     C                              + %trim(
031600131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
031700131004     C                              +   apice
031800131004     C                   exsr      wriOUTPUT
031900131004     C*
032000131004     C                   ENDSR
032100131004
032200131004
032300131004
032400131004     C     wriFooter     BEGSR
032500131004     C*
032600131004     C                   clear                   vawDTA
032700131004     C*
032800131004     C*  UNZ -
032900131004     C                   eval      vawDTA = UNZ_sgm
033000131004     C                              + %trim(%editc(Conta_msg:'4'))
033100131004     C                              +   piu
033200131004     C                              + %trim(
033300131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
033400131004     C                              +   apice
033500131004     C                   exsr      wriOUTPUT
033600131004     C*
033700131004     C                   ENDSR
033800131004
033900030924
034000030924
034100991027     C     traduci       BEGSR
034200131004     C*
034300131004     C* Inizializzo i contatori a livello di transazione/messaggio
034400131004     C                   z-add     *zeros        Conta_msg
034500031103     C*
034600031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
034700031103     C* di traduzione (trattasi d "post-traduzione")
034800150113     C                   MOVE(P)   w§PPIKSU      wKstbKSU
034900031103     C                   EVAL      wKstbFTR = *blanks
035000110103     C     KEYstb03P     SETLL     tistb03l
035100110103     C                   IF        %equal(tistb03l)
035200110103     C     KEYstb03P     READE     tistb03l
035300991027     C*
035400031103     C* Ciclo x tuttii record del cliente da trasmettere
035500110103     C                   DOW       not %eof(tistb03l)
035600131004     C*
035700131004     C* Inizializzo i contatori a livello di segmento
035800131004     C                   z-add     *zeros        Conta_segm
035900080618     C*
036000130807     C                   SETON                                        50
036100031103     C*
036200031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
036300031103     C* deve restituire al cliente nel file tradotto
036400031103     C                   IF        stbFTR = *blanks                             * ridondante
036500110103     C*
036600110103     C* Gestisco solamente gli stati standard
036700130808     C                   IF        (stbTIS= '1' OR
036800130906     C                             (stbTIS= '9' AND
036900151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
037000151117     C                              (stbPRS= '1'                ))) AND
037100130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
037200130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
037300131004     C*
037400131004     C* Per "sicurezza" non invio stati senza data
037500131004     C                   IF        stbDAS > *zeros
037600110103     C*
037700110103     C                   SETOFF                                       50
037800031103     C*
037900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
038000031103     C     KEYtas30P     CHAIN     titas30c
038100031103     C                   IF        %found(titas30c)
038200131010     C*
038300131010     C* Innanzitutto chaino la bolla direttamente su TITAS
038400131216     C                   MOVEL     *zeros        wPO               3 0
038500131213     C*
038600131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
038700131213     C                   if        tasDTI = *zeros
038800131213     C                   eval      wPO = tasLNP
038900131213     C                   else
039000131213     C                   eval      wPO = tasLNA
039100131213     C                   endif
039200131213     C*
039300131213     C     wPO           CHAIN     azorg01l
039400131010     C                   IF        %found(azorg01l)
039500080618     C*
039600110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
039700140606     C                   SETOFF                                       80
039800031103     C                   Z-ADD     1             j
039900160914     C                   MOVEL(P)  stbCOS        wCOS_15          15
040000160914     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
040100141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
040200141006     C                   EVAL      wStsEvt = skCAU_EVT(j)
040300141006     C                   EVAL      wStsRsn = skCAU_RSN(j)
040400140514     C                   SETOFF                                       50
040500140606     C                   SETON                                        80
040600031103     C                   ELSE
040700031103     C                   SETON                                        50
040800160914     C                   ENDIF
040900160914     C  N55              EXSR      sndAlert
041000140308     C*
041100140308     C* Al primo dettaglio da inviare => scrivo anche testata
041200140313     C                   IF        not *IN50
041300140313     C  N45              EXSR      wriHeader
041400140308     C                   SETON                                        45
041500140313     C                   ENDIF
041600080618     C*
041700080618     C* Scarico il buffer d output
041800131004     C  N50              EXSR      wriStato
041900110103     C*
042000131010     C                   ENDIF
042100131004     C                   ENDIF
042200110103     C                   ENDIF
042300110103     C                   ENDIF
042400110103     C                   ENDIF
042500031103     C*
042600031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
042700031103     C                   EVAL      stbFTR = 'T'
042800031103     C                   UPDATE    tistb000
042900991027     C*
043000110103     C     KEYstb03P     READE     tistb03l
043100031103     C                   SETOFF                                       50
043200030325     C                   ENDDO
043300031103     C                   ENDIF
043400991027     C*
043500030325     C                   EVAL      wrkesito = '0'
043600991027     C*
043700910830     C                   ENDSR
043800031103
043900031103
044000031103
044100131004     C     wriStato      BEGSR
044200131004     C*
044300131004     C* Incremento il contatore di messaggio (al interno della transazione)
044400131004     C                   eval      Conta_msg = Conta_msg + 1
044500131004     C*
044600131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
044700131104     C                   movel     *blanks       wAlfaRMA        512
044800131024     C                   movel     *blanks       wAlfaRSD        512
044900131004     C                   movel     *blanks       wAlfaLOD        512
045000131004     C                   movel     *blanks       wAlfaIND        512
045100131025     C                   movel     *blanks       wAlfaORGDES     512
045200131004     C* RMA
045300131004     C                   eval      campo_alfa = stbRMA
045400131004     C                   exsr      Char_Speciali
045500131004     C                   eval      wAlfaRMA = campo_alfa
045600131024     C* RSD
045700131024     C                   eval      campo_alfa = tasRSD
045800131024     C                   exsr      Char_Speciali
045900131024     C                   eval      wAlfaRSD = campo_alfa
046000131004     C* LOD
046100131004     C                   eval      campo_alfa = tasLOD
046200131004     C                   exsr      Char_Speciali
046300131004     C                   eval      wAlfaLOD = campo_alfa
046400131004     C* IND
046500131004     C                   eval      campo_alfa = tasIND
046600131004     C                   exsr      Char_Speciali
046700131004     C                   eval      wAlfaIND = campo_alfa
046800131025     C* ORGDES
046900131025     C                   eval      campo_alfa = orgDES
047000131025     C                   exsr      Char_Speciali
047100131025     C                   eval      wAlfaORGDES = campo_alfa
047200131004     C*
047300131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
047400131004     C* (ancora corretto ???)
047500131004     C                   z-add     *zeros        wPeso             7 1
047600131004     C                   if        tasPKB > tasPKC
047700131004     C                   eval      wPeso = tasPKB
047800131004     C                   else
047900131004     C                   eval      wPeso = tasPKC
048000131004     C                   endif
048100131004     C*
048200131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
048300131004     C*
048400131004     C*  UNH -
048500131004     C                   eval      Conta_segm  = Conta_segm  + 1
048600131007     C                   eval      vawDTA = UNH_sgm_1
048700150527     C                              + %trim(currDate8) + %trim(currHHMMSS)
048800150527     C                              + UNH_sgm_2
048900150527     C                              +   apice
049000131004     C                   exsr      wriOUTPUT
049100131004     C*
049200150527     C*  BGM 7 -
049300131004     C                   eval      Conta_segm  = Conta_segm  + 1
049400150527     C                   eval      vawDTA = BGM_7
049500131004     C                              +   apice
049600131004     C                   exsr      wriOUTPUT
049700150527     C*
049800150527     C*  DTM 137 -
049900150527     C                   eval      Conta_segm  = Conta_segm  + 1
050000150527     C                   eval      vawDTA = DTM_137
050100150527     C                              + %trim(currDate8) + %trim(currHHMMSS)
050200150527     C                              +   diviso
050300150527     C                              + '204'
050400150527     C                              +   apice
050500150527     C                   exsr      wriOUTPUT
050600150527     C*
050700150527     C*  NAD CS -
050800150527     C                   eval      Conta_segm  = Conta_segm  + 1
050900150527     C                   eval      vawDTA = NAD_CS
051000150527     C                              +   apice
051100150527     C                   exsr      wriOUTPUT
051200131004     C*
051300150113     C*  NAD SF -
051400150113     C                   eval      Conta_segm  = Conta_segm  + 1
051500150113     C                   eval      vawDTA = NAD_SF
051600150113     C                              + %trim(w§NAD_SF)
051700150113     C                              +   apice
051800150113     C                   exsr      wriOUTPUT
051900131004     C*
052000131004     C*  NAD ST -
052100131004     C                   eval      Conta_segm  = Conta_segm  + 1
052200131004     C                   eval      vawDTA = NAD_ST
052300131025     C                              + %trim(wAlfaRSD)
052400131004     C                              +   piu
052500131004     C                              +   piu
052600131004     C                              + %trim(wAlfaIND)
052700131004     C                              +   piu
052800131004     C                              + %trim(wAlfaLOD)
052900131004     C                              +   piu
053000131004     C                              + %trim(tasPRD)
053100131004     C                              +   piu
053200131004     C                              + %trim(tasCAD)
053300131004     C                              +   piu
053400131004     C                              + 'IT'
053500131004     C                              +   apice
053600131004     C                   exsr      wriOUTPUT
053700131004     C*
053800131010     C*  LOC -
053900131004     C                   eval      Conta_segm  = Conta_segm  + 1
054000131004     C                   eval      vawDTA = LOC_sgm1
054100131010     C                              + %trim(%editc(orgFIL:'X'))
054200131004     C                              +   LOC_sgm2
054300131025     C                              + %trim(wAlfaORGDES)
054400131010     C                              +   LOC_sgm3
054500131004     C                              +   apice
054600131004     C                   exsr      wriOUTPUT
054700131004     C*
054800150527     C*  CNI -
054900131004     C                   eval      Conta_segm  = Conta_segm  + 1
055000131004     C                   eval      vawDTA = CNI_sgm
055100131004     C                              +   apice
055200131004     C                   exsr      wriOUTPUT
055300131004     C*
055400131004     C*  STS -
055500131004     C                   eval      Conta_segm  = Conta_segm  + 1
055600141006     C                   eval      vawDTA = STS_sgm
055700150113     C                              + %trim(wStsTyp) + '+'
055800150113     C                              + %trim(wStsEvt) + '+'
055900141006     C                              + %trim(wStsRsn)
056000131004     C                              +   apice
056100131004     C                   exsr      wriOUTPUT
056200131004     C*
056300150527     C*  RFF FF -
056400170303     C***                eval      Conta_segm  = Conta_segm  + 1
056500170303     C***                eval      vawDTA = RFF_FF
056600170303     C***                           + %trim(%editc(stbRMN:'4'))
056700170303     C***                           +   apice
056800170303     C***                exsr      wriOUTPUT
056900170303     C*  RFF ACL -
057000170303     C                   eval      Conta_segm  = Conta_segm  + 1
057100170303     C                   eval      vawDTA = RFF_ACL
057200170303     C                              + %trim(%editc(stbRMN:'4'))
057300170303     C                              +   apice
057400170303     C                   exsr      wriOUTPUT
057500150527     C*
057600150527     C*  RFF CR -
057700150527     C                   eval      Conta_segm  = Conta_segm  + 1
057800150527     C                   eval      vawDTA = RFF_CR
057900150527     C                              + %trim(wAlfaRMA)
058000150527     C                              +   apice
058100150527     C                   exsr      wriOUTPUT
058200131004     C*
058300131004     C*  TDT -
058400131004     C                   eval      Conta_segm  = Conta_segm  + 1
058500131004     C                   eval      vawDTA = TDT_sgm
058600131004     C                              +   apice
058700131004     C                   exsr      wriOUTPUT
058800131004     C*
058900150527     C*  DTM 9 -
059000131004     C                   eval      Conta_segm  = Conta_segm  + 1
059100150527     C                   eval      vawDTA = DTM_9
059200131007     C                              + %trim(%editc(stbDAS*10000+stbORS:'4'))
059300131007     C                              + '00'
059400131004     C                              +   diviso
059500131007     C                              + '204'
059600131004     C                              +   apice
059700131004     C                   exsr      wriOUTPUT
059800131004     C*
059900131004     C*  EQD -
060000131004     C                   eval      Conta_segm  = Conta_segm  + 1
060100131004     C                   eval      vawDTA = EQD_sgm
060200131004     C                              +   apice
060300131004     C                   exsr      wriOUTPUT
060400160215     C*
060500160215     C*  GID -
060600160215     C                   eval      Conta_segm  = Conta_segm  + 1
060700160215     C                   eval      vawDTA = GID_sgm1
060800160215     C                              + %trim(%editc(tasNCL:'4'))
060900160215     C                              +   GID_sgm2
061000160215     C                              +   apice
061100160215     C                   exsr      wriOUTPUT
061200131004     C*
061300131004     C*  UNT -
061400131004     C                   eval      Conta_segm  = Conta_segm  + 1
061500131004     C                   eval      vawDTA = UNT_sgm
061600131004     C                              + %trim(%editc(Conta_segm:'4'))
061700131004     C                              +   piu
061800131007     C                              + %trim(%editc(Conta_msg:'4'))
061900131004     C                              +   apice
062000131004     C                   exsr      wriOUTPUT
062100031103     C*
062200031103     C                   ENDSR
062300150113
062400160914
062500150113
062600150113     C*----------------------------------------------------*
062700150113     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
062800150113     C*----------------------------------------------------*
062900150113     C     SPLIT_PARMS   BEGSR
063000150113     C*
063100150113     C                   reset                   w§UNB_ID
063200150113     C                   reset                   w§NAD_SF
063300150113     C*
063400150113     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
063500150113     C                   CLEAR                   DVIREPE
063600150113     C                   EVAL      vireTRC = 'PE'
063700150114     C                   MOVE(P)   w§PPIDTI      vireDTI
063800150113     C     KEYvire1C     CHAIN     tivire1l
063900150113     C                   IF        %found(tivire1l)
064000150113     C                   EVAL      DVIREPE = vireFLO
064100150113     C*
064200150113     C* Se reperiti parametri => effettuo splitting
064300150113     C                   eval      InStringa = %trim(§VIREPEKPJ)
064400150113     C*
064500150113     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
064600150113     C                   CALL      'XSPLIT2'
064700150113     C                   PARM                    InStringa
064800150113     C                   PARM      '§'           InSepara
064900150113     C                   PARM      '6'           InTypeOut
065000150113     C                   PARM      ''            wSkParam
065100150113     C                   PARM                    OutErrore
065200150113     C                   PARM                    OutMsg
065300150113     C                   MOVEA     wSkParam      SkSplitCSV_6
065400150113     C*
065500150113     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
065600150113     C                   z-add     1             i                 4 0
065700150113     C                   dow       i <= %elem(SkSplitCSV_6)
065800150113     C                   if        SkSplitCSV_6(i) = *blanks
065900150113     C                   leave
066000150113     C                   else
066100150113     C                   select
066200150113     C* - parametro UNB
066300150113     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
066400150113     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
066500150113     C* - parametro NAD+SF
066600150113     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
066700150113     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
066800150113     C*
066900150113     C                   endsl
067000150113     C                   endif
067100150113     C*
067200150113     C                   add       1             i
067300150113     C                   enddo
067400150113     C*
067500150113     C                   ENDIF
067600150113     C*
067700150113     C                   ENDSR
067800160914
067900160914
068000160914
068100160914     C*--------------------------------------------------------------------------------------------*
068200160914     C* /TITLE Compongo il testo e spedisco una e-mail di alert
068300160914     C*--------------------------------------------------------------------------------------------*
068400160914     C     sndAlert      BEGSR
068500160914     C*
068600160914     C* Inizializzo variabili
068700160914     C                   movel     *blanks       wrkEml          253
068800160914     C                   movel     *blanks       wrkCcEml        253
068900160914     C                   movel     *blanks       wrkEmlMsg      5000
069000160914     C                   movel     *blanks       wrkEmlOgg        44
069100160914     C*
069200160914     C* Valorizzo i campi della e-m@ail
069300160914     C                   eval      wrkEml='cedalert@brt.it'
069400160914     C                   eval      wrkEmlOgg=
069500160914     C                               'BARTVAS - Problema Mappatura TIVMS00F'
069600160914     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
069700160914     C                                                                    ':/N'+
069800160914     C                                                                    ':/N'+
069900160914     C                             'Verificare la mappatura: '           +':/N'+
070000160914     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
070100160914     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
070200160914     C                                                                    ':/N'+
070300160914     C                             'Causale mancante: ' + stbCOS         +':/N'+
070400160914     C                             ' '                                   +':/N'
070500160914     C*
070600160914     C                   call(e)   'TIS701C1'
070700160914     C                   parm                    wrkEml
070800160914     C                   parm                    wrkCcEml
070900160914     C                   parm                    wrkEmlOgg
071000160914     C                   parm                    wrkEmlMsg
071100160914     C                   parm                    wrkEsito          1
071200160914     C*
071300160914     C                   ENDSR
071400031103
071500031103
071600991027
071700991027      /TITLE Operazioni iniziali.
071800991027     C     *inzsr        BEGSR
071900991027     C*
072000991027     C     *ENTRY        PLIST
072100150113     C                   parm                    prmppi
072200991027     C     wrkesito      parm      wrkesito      prmesito
072300160914     C*
072400160914     C* Reperisco la data corrente
072500160914     C                   z-add     *zero         datcor            8 0
072600160914     C                   eval      datcor = %dec(%date() : *iso)
072700031103     C*
072800031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
072900150113     C                   EVAL      DSPPI = prmppi
073000131004     C*
073100131004     C* Reperisco i valori variabili ma a livello di transazione corrente
073200131004     C* (tipicamente data e ora relativ al file generato)
073300131004     C                   eval      currDate6    =
073400131004     C                              UBFMTDATE_Convert(
073500131004     C                                  %editc(%dec(%date():*ISO):'X') :
073600131004     C                                  'YYYYMMDD' : 'YYMMDD')
073700131004     C                   eval      currDate6inv =
073800131004     C                              UBFMTDATE_Convert(
073900131004     C                                  %editc(%dec(%date():*ISO):'X') :
074000131004     C                                  'YYYYMMDD' : 'DDMMYY')
074100150527     C                   eval      currDate8    =
074200150527     C                                  %editc(%dec(%date():*ISO):'X')
074300131004     C                   eval      currHHMM =
074400131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
074500150527     C                   eval      currHHMMSS =
074600150527     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:6)
074700030709     C*
074800030709     C* Definizione chiavi
074900030924     C*
075000110103     C* Chiave su TISTB03L - Parziale
075100110103     C     KEYstb03P     KLIST
075200031104     C                   KFLD                    wKstbKSU
075300031103     C                   KFLD                    wKstbFTR
075400031103     C*
075500031103     C* Chiave su TITAS30C - Parziale
075600031103     C     KEYtas30P     KLIST
075700031103     C                   KFLD                    stbAAS
075800031103     C                   KFLD                    stbLNP
075900031103     C                   KFLD                    stbNRS
076000031103     C                   KFLD                    stbNSP
076100150113     C*
076200150113     C* Chiave su TIVIRE1L - Parziale
076300150113     C     KEYvire1C     KLIST
076400150113     C                   KFLD                    w§PPIKSU
076500150113     C                   KFLD                    w§PPITIP
076600150114     C                   KFLD                    vireDTI
076700150113     C                   KFLD                    vireTRC
076800160914     C*
076900160914     C* Chiave su TIVMS01I - Parziale
077000160914     C     KEYvms01P     KLIST
077100160914     C                   KFLD                    vmsPROC
077200160914     C                   KFLD                    vmsKSU
077300160914     C                   KFLD                    vmsDDE
077400991027     C*
077500991027     C                   ENDSR
