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
007000160914
007100160914
007200131004     D*------------------
007300131004     D* LINKING A DEFINIZIONI ESTERNE
007400131004     D*------------------
007500160924      /COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
007600160924      /COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
007700160924      /COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
007800131004
007900131004
008000031103     D*-----------
008100031103     D* VARIABILI RIFERITE AL DATA-BASE
008200031103     D*-----------
008300080618     D wKstbKSU        S                   like(stbKSU)
008400080618     D wKstbFTR        S                   like(stbFTR)
008500110705
008600031103
008700031103     D*-----------
008800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
008900031103     D*-----------
009000150114     D DSPPI           DS
009100150114     D  w§PPIKSU                           like(vireKSC) inz
009200150114     D  w§PPITIP                           like(vireTIP) inz
009300150114     D  w§PPIDTI                      8                  inz
009400160914     D  w§PPIVMSPROC                       like(vmsPROC) inz
009500160914     D  w§PPIVMSKSU                        like(vmsKSU)  inz
009600131004
009700131004
009800131004     D*-----------
009900131004     D* COSTANTI / DEFAULT
010000131004     D*-----------
010100160919     D UNA_sgm         C                   CONST('UNA:+.? ')
010200150113     D UNB_sgm         C                   CONST('UNB+UNOA:1+IT-BRT001:ZZ+')
010300131007     D UNH_sgm_1       C                   CONST('UNH+')
010400150527     D UNH_sgm_2       C                   CONST('+IFTSTA:D:96A:UN:EAN002')
010500150527     D BGM_7           C                   CONST('BGM+7+')
010600150527     D DTM_137         C                   CONST('DTM+137:')
010700150527     D NAD_CS          C                   CONST('NAD+CS+IT-BRT001')
010800150113     D NAD_SF          C                   CONST('NAD+SF+')
010900131004     D NAD_ST          C                   CONST('NAD+ST++')
011000131010     D LOC_sgm1        C                   CONST('LOC+175+')
011100131010     D LOC_sgm2        C                   CONST('::3:')
011200131010     D LOC_sgm3        C                   CONST('+IT')
011300131004     D CNI_sgm         C                   CONST('CNI+1')
011400141006     D STS_sgm         C                   CONST('STS+')
011500150527     D RFF_FF          C                   CONST('RFF+FF:')
011600150527     D RFF_CR          C                   CONST('RFF+CR:')
011700150113     D TDT_sgm         C                   CONST('TDT+30+1+++IT-BRT001')
011800150527     D DTM_9           C                   CONST('DTM+9:')
011900131004     D EQD_sgm         C                   CONST('EQD+AA')
012000160215     D GID_sgm1        C                   CONST('GID+1+')
012100160215     D GID_sgm2        C                   CONST(':CTN')
012200131004     D UNT_sgm         C                   CONST('UNT+')
012300131004     D UNZ_sgm         C                   CONST('UNZ+')
012400131004     D piu             C                   CONST('+')
012500131004     D diviso          C                   CONST(':')
012600131004     D apice           C                   CONST('''')
012700141006     D wStsTyp         C                   CONST('')
012800131004
012900030924
013000030325
013100030924     C* MAIN/
013200170123     C*
013300170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
013400170123     C
013500170123     C/EXEC SQL
013600170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
013700170123     C/END-EXEC
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
021700160924     C                   parm      *blanks       iVMSDDEOpz
021800160924     C                   parm      w§PPIVMSPROC  iVMSDDEProc
021900160924     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
022000160924     C                   parm      datcor        iVMSDDEDtRif
022100160924     C                   parm                    oVMSDDEDDE
022200160924     C                   parm                    oVMSDDEEsito
022300160914     C*
022400160914     C* Verifico se esito no OK
022500160924     C                   if        oVMSDDEEsito <> '1'
022600160914     C                   seton                                        40
022700160914     C                   else
022800160914     C*
022900160914     C                   move(P)   w§PPIVMSPROC  vmsPROC
023000160914     C                   move(P)   w§PPIVMSKSU   vmsKSU
023100160924     C                   z-add     oVMSDDEDDE    vmsDDE
023200160924     C     KEYvms01P     setll     tivms01i
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
030400160919     C*
030500160919     C*  UNA -
030600160919     C                   eval      vawDTA = UNA_sgm
030700160919     C                              +   apice
030800160919     C                   exsr      wriOUTPUT
030900131010     C*
031000131004     C*  UNB -
031100131004     C                   eval      vawDTA = UNB_sgm
031200150113     C                              + %trim(w§UNB_ID)
031300131004     C                              +   piu
031400131004     C                              + %trim(currDate6inv)
031500131004     C                              +   diviso
031600131004     C                              + %trim(currHHMM)
031700131004     C                              +   piu
031800131004     C                              + %trim(
031900131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
032000131004     C                              +   apice
032100131004     C                   exsr      wriOUTPUT
032200131004     C*
032300131004     C                   ENDSR
032400131004
032500131004
032600131004
032700131004     C     wriFooter     BEGSR
032800131004     C*
032900131004     C                   clear                   vawDTA
033000131004     C*
033100131004     C*  UNZ -
033200131004     C                   eval      vawDTA = UNZ_sgm
033300131004     C                              + %trim(%editc(Conta_msg:'4'))
033400131004     C                              +   piu
033500131004     C                              + %trim(
033600131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
033700131004     C                              +   apice
033800131004     C                   exsr      wriOUTPUT
033900131004     C*
034000131004     C                   ENDSR
034100131004
034200030924
034300030924
034400991027     C     traduci       BEGSR
034500131004     C*
034600131004     C* Inizializzo i contatori a livello di transazione/messaggio
034700131004     C                   z-add     *zeros        Conta_msg
034800031103     C*
034900031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
035000031103     C* di traduzione (trattasi d "post-traduzione")
035100150113     C                   MOVE(P)   w§PPIKSU      wKstbKSU
035200031103     C                   EVAL      wKstbFTR = *blanks
035300110103     C     KEYstb03P     SETLL     tistb03l
035400110103     C                   IF        %equal(tistb03l)
035500110103     C     KEYstb03P     READE     tistb03l
035600991027     C*
035700031103     C* Ciclo x tuttii record del cliente da trasmettere
035800110103     C                   DOW       not %eof(tistb03l)
035900131004     C*
036000131004     C* Inizializzo i contatori a livello di segmento
036100131004     C                   z-add     *zeros        Conta_segm
036200080618     C*
036300130807     C                   SETON                                        50
036400031103     C*
036500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
036600031103     C* deve restituire al cliente nel file tradotto
036700031103     C                   IF        stbFTR = *blanks                             * ridondante
036800110103     C*
036900110103     C* Gestisco solamente gli stati standard
037000130808     C                   IF        (stbTIS= '1' OR
037100130906     C                             (stbTIS= '9' AND
037200151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
037300151117     C                              (stbPRS= '1'                ))) AND
037400130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
037500130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
037600131004     C*
037700131004     C* Per "sicurezza" non invio stati senza data
037800131004     C                   IF        stbDAS > *zeros
037900110103     C*
038000110103     C                   SETOFF                                       50
038100031103     C*
038200031103     C* Innanzitutto chaino la bolla direttamente su TITAS
038300031103     C     KEYtas30P     CHAIN     titas30c
038400031103     C                   IF        %found(titas30c)
038500131010     C*
038600131010     C* Innanzitutto chaino la bolla direttamente su TITAS
038700131216     C                   MOVEL     *zeros        wPO               3 0
038800131213     C*
038900131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
039000131213     C                   if        tasDTI = *zeros
039100131213     C                   eval      wPO = tasLNP
039200131213     C                   else
039300131213     C                   eval      wPO = tasLNA
039400131213     C                   endif
039500131213     C*
039600131213     C     wPO           CHAIN     azorg01l
039700131010     C                   IF        %found(azorg01l)
039800080618     C*
039900110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
040000140606     C                   SETOFF                                       80
040100031103     C                   Z-ADD     1             j
040200160914     C                   MOVEL(P)  stbCOS        wCOS_15          15
040300160914     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
040400141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
040500141006     C                   EVAL      wStsEvt = skCAU_EVT(j)
040600141006     C                   EVAL      wStsRsn = skCAU_RSN(j)
040700140514     C                   SETOFF                                       50
040800140606     C                   SETON                                        80
040900031103     C                   ELSE
041000031103     C                   SETON                                        50
041100160914     C                   ENDIF
041200160914     C  N55              EXSR      sndAlert
041300140308     C*
041400140308     C* Al primo dettaglio da inviare => scrivo anche testata
041500140313     C                   IF        not *IN50
041600140313     C  N45              EXSR      wriHeader
041700140308     C                   SETON                                        45
041800140313     C                   ENDIF
041900080618     C*
042000080618     C* Scarico il buffer d output
042100131004     C  N50              EXSR      wriStato
042200110103     C*
042300131010     C                   ENDIF
042400131004     C                   ENDIF
042500110103     C                   ENDIF
042600110103     C                   ENDIF
042700110103     C                   ENDIF
042800031103     C*
042900031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
043000031103     C                   EVAL      stbFTR = 'T'
043100031103     C                   UPDATE    tistb000
043200991027     C*
043300110103     C     KEYstb03P     READE     tistb03l
043400031103     C                   SETOFF                                       50
043500030325     C                   ENDDO
043600031103     C                   ENDIF
043700991027     C*
043800030325     C                   EVAL      wrkesito = '0'
043900991027     C*
044000910830     C                   ENDSR
044100031103
044200031103
044300031103
044400131004     C     wriStato      BEGSR
044500131004     C*
044600131004     C* Incremento il contatore di messaggio (al interno della transazione)
044700131004     C                   eval      Conta_msg = Conta_msg + 1
044800131004     C*
044900131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
045000131104     C                   movel     *blanks       wAlfaRMA        512
045100131024     C                   movel     *blanks       wAlfaRSD        512
045200131004     C                   movel     *blanks       wAlfaLOD        512
045300131004     C                   movel     *blanks       wAlfaIND        512
045400131025     C                   movel     *blanks       wAlfaORGDES     512
045500131004     C* RMA
045600131004     C                   eval      campo_alfa = stbRMA
045700131004     C                   exsr      Char_Speciali
045800131004     C                   eval      wAlfaRMA = campo_alfa
045900131024     C* RSD
046000131024     C                   eval      campo_alfa = tasRSD
046100131024     C                   exsr      Char_Speciali
046200131024     C                   eval      wAlfaRSD = campo_alfa
046300131004     C* LOD
046400131004     C                   eval      campo_alfa = tasLOD
046500131004     C                   exsr      Char_Speciali
046600131004     C                   eval      wAlfaLOD = campo_alfa
046700131004     C* IND
046800131004     C                   eval      campo_alfa = tasIND
046900131004     C                   exsr      Char_Speciali
047000131004     C                   eval      wAlfaIND = campo_alfa
047100131025     C* ORGDES
047200131025     C                   eval      campo_alfa = orgDES
047300131025     C                   exsr      Char_Speciali
047400131025     C                   eval      wAlfaORGDES = campo_alfa
047500131004     C*
047600131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
047700131004     C* (ancora corretto ???)
047800131004     C                   z-add     *zeros        wPeso             7 1
047900131004     C                   if        tasPKB > tasPKC
048000131004     C                   eval      wPeso = tasPKB
048100131004     C                   else
048200131004     C                   eval      wPeso = tasPKC
048300131004     C                   endif
048400131004     C*
048500131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
048600131004     C*
048700131004     C*  UNH -
048800131004     C                   eval      Conta_segm  = Conta_segm  + 1
048900131007     C                   eval      vawDTA = UNH_sgm_1
049000150527     C                              + %trim(currDate8) + %trim(currHHMMSS)
049100150527     C                              + UNH_sgm_2
049200150527     C                              +   apice
049300131004     C                   exsr      wriOUTPUT
049400131004     C*
049500150527     C*  BGM 7 -
049600131004     C                   eval      Conta_segm  = Conta_segm  + 1
049700150527     C                   eval      vawDTA = BGM_7
049800160924     C                              + %trim(wAlfaRMA)
049900131004     C                              +   apice
050000131004     C                   exsr      wriOUTPUT
050100150527     C*
050200150527     C*  DTM 137 -
050300150527     C                   eval      Conta_segm  = Conta_segm  + 1
050400150527     C                   eval      vawDTA = DTM_137
050500150527     C                              + %trim(currDate8) + %trim(currHHMMSS)
050600150527     C                              +   diviso
050700150527     C                              + '204'
050800150527     C                              +   apice
050900150527     C                   exsr      wriOUTPUT
051000150527     C*
051100150527     C*  NAD CS -
051200150527     C                   eval      Conta_segm  = Conta_segm  + 1
051300150527     C                   eval      vawDTA = NAD_CS
051400150527     C                              +   apice
051500150527     C                   exsr      wriOUTPUT
051600131004     C*
051700150113     C*  NAD SF -
051800150113     C                   eval      Conta_segm  = Conta_segm  + 1
051900150113     C                   eval      vawDTA = NAD_SF
052000150113     C                              + %trim(w§NAD_SF)
052100150113     C                              +   apice
052200150113     C                   exsr      wriOUTPUT
052300131004     C*
052400131004     C*  NAD ST -
052500131004     C                   eval      Conta_segm  = Conta_segm  + 1
052600131004     C                   eval      vawDTA = NAD_ST
052700131025     C                              + %trim(wAlfaRSD)
052800131004     C                              +   piu
052900131004     C                              +   piu
053000131004     C                              + %trim(wAlfaIND)
053100131004     C                              +   piu
053200131004     C                              + %trim(wAlfaLOD)
053300131004     C                              +   piu
053400131004     C                              + %trim(tasPRD)
053500131004     C                              +   piu
053600131004     C                              + %trim(tasCAD)
053700131004     C                              +   piu
053800131004     C                              + 'IT'
053900131004     C                              +   apice
054000131004     C                   exsr      wriOUTPUT
054100131004     C*
054200131010     C*  LOC -
054300131004     C                   eval      Conta_segm  = Conta_segm  + 1
054400131004     C                   eval      vawDTA = LOC_sgm1
054500131010     C                              + %trim(%editc(orgFIL:'X'))
054600131004     C                              +   LOC_sgm2
054700131025     C                              + %trim(wAlfaORGDES)
054800131010     C                              +   LOC_sgm3
054900131004     C                              +   apice
055000131004     C                   exsr      wriOUTPUT
055100131004     C*
055200150527     C*  CNI -
055300131004     C                   eval      Conta_segm  = Conta_segm  + 1
055400131004     C                   eval      vawDTA = CNI_sgm
055500131004     C                              +   apice
055600131004     C                   exsr      wriOUTPUT
055700131004     C*
055800131004     C*  STS -
055900131004     C                   eval      Conta_segm  = Conta_segm  + 1
056000141006     C                   eval      vawDTA = STS_sgm
056100150113     C                              + %trim(wStsTyp) + '+'
056200150113     C                              + %trim(wStsEvt) + '+'
056300141006     C                              + %trim(wStsRsn)
056400131004     C                              +   apice
056500131004     C                   exsr      wriOUTPUT
056600131004     C*
056700150527     C*  RFF FF -
056800131004     C                   eval      Conta_segm  = Conta_segm  + 1
056900150527     C                   eval      vawDTA = RFF_FF
057000161212     C                              + %trim(%editc(tasCCM:'X'))
057100131004     C                              +   apice
057200131004     C                   exsr      wriOUTPUT
057300150527     C*
057400150527     C*  RFF CR -
057500150527     C                   eval      Conta_segm  = Conta_segm  + 1
057600150527     C                   eval      vawDTA = RFF_CR
057700150527     C                              + %trim(wAlfaRMA)
057800150527     C                              +   apice
057900150527     C                   exsr      wriOUTPUT
058000131004     C*
058100131004     C*  TDT -
058200131004     C                   eval      Conta_segm  = Conta_segm  + 1
058300131004     C                   eval      vawDTA = TDT_sgm
058400131004     C                              +   apice
058500131004     C                   exsr      wriOUTPUT
058600131004     C*
058700150527     C*  DTM 9 -
058800131004     C                   eval      Conta_segm  = Conta_segm  + 1
058900150527     C                   eval      vawDTA = DTM_9
059000131007     C                              + %trim(%editc(stbDAS*10000+stbORS:'4'))
059100131007     C                              + '00'
059200131004     C                              +   diviso
059300131007     C                              + '204'
059400131004     C                              +   apice
059500131004     C                   exsr      wriOUTPUT
059600131004     C*
059700131004     C*  EQD -
059800131004     C                   eval      Conta_segm  = Conta_segm  + 1
059900131004     C                   eval      vawDTA = EQD_sgm
060000131004     C                              +   apice
060100131004     C                   exsr      wriOUTPUT
060200160215     C*
060300160215     C*  GID -
060400160215     C                   eval      Conta_segm  = Conta_segm  + 1
060500160215     C                   eval      vawDTA = GID_sgm1
060600160215     C                              + %trim(%editc(tasNCL:'4'))
060700160215     C                              +   GID_sgm2
060800160215     C                              +   apice
060900160215     C                   exsr      wriOUTPUT
061000131004     C*
061100131004     C*  UNT -
061200131004     C                   eval      Conta_segm  = Conta_segm  + 1
061300131004     C                   eval      vawDTA = UNT_sgm
061400131004     C                              + %trim(%editc(Conta_segm:'4'))
061500131004     C                              +   piu
061600131007     C                              + %trim(%editc(Conta_msg:'4'))
061700131004     C                              +   apice
061800131004     C                   exsr      wriOUTPUT
061900031103     C*
062000031103     C                   ENDSR
062100150113
062200160914
062300150113
062400150113     C*----------------------------------------------------*
062500150113     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
062600150113     C*----------------------------------------------------*
062700150113     C     SPLIT_PARMS   BEGSR
062800150113     C*
062900150113     C                   reset                   w§UNB_ID
063000150113     C                   reset                   w§NAD_SF
063100150113     C*
063200150113     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
063300150113     C                   CLEAR                   DVIREPE
063400150113     C                   EVAL      vireTRC = 'PE'
063500150114     C                   MOVE(P)   w§PPIDTI      vireDTI
063600150113     C     KEYvire1C     CHAIN     tivire1l
063700150113     C                   IF        %found(tivire1l)
063800150113     C                   EVAL      DVIREPE = vireFLO
063900150113     C*
064000150113     C* Se reperiti parametri => effettuo splitting
064100150113     C                   eval      InStringa = %trim(§VIREPEKPJ)
064200150113     C*
064300150113     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
064400150113     C                   CALL      'XSPLIT2'
064500150113     C                   PARM                    InStringa
064600150113     C                   PARM      '§'           InSepara
064700150113     C                   PARM      '6'           InTypeOut
064800150113     C                   PARM      ''            wSkParam
064900150113     C                   PARM                    OutErrore
065000150113     C                   PARM                    OutMsg
065100150113     C                   MOVEA     wSkParam      SkSplitCSV_6
065200150113     C*
065300150113     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
065400150113     C                   z-add     1             i                 4 0
065500150113     C                   dow       i <= %elem(SkSplitCSV_6)
065600150113     C                   if        SkSplitCSV_6(i) = *blanks
065700150113     C                   leave
065800150113     C                   else
065900150113     C                   select
066000150113     C* - parametro UNB
066100150113     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
066200150113     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
066300150113     C* - parametro NAD+SF
066400150113     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
066500150113     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
066600150113     C*
066700150113     C                   endsl
066800150113     C                   endif
066900150113     C*
067000150113     C                   add       1             i
067100150113     C                   enddo
067200150113     C*
067300150113     C                   ENDIF
067400150113     C*
067500150113     C                   ENDSR
067600160914
067700160914
067800160914
067900160914     C*--------------------------------------------------------------------------------------------*
068000160914     C* /TITLE Compongo il testo e spedisco una e-mail di alert
068100160914     C*--------------------------------------------------------------------------------------------*
068200160914     C     sndAlert      BEGSR
068300160914     C*
068400160914     C* Inizializzo variabili
068500160914     C                   movel     *blanks       wrkEml          253
068600160914     C                   movel     *blanks       wrkCcEml        253
068700160914     C                   movel     *blanks       wrkEmlMsg      5000
068800160914     C                   movel     *blanks       wrkEmlOgg        44
068900160914     C*
069000160914     C* Valorizzo i campi della e-m@ail
069100160914     C                   eval      wrkEml='cedalert@brt.it'
069200160914     C                   eval      wrkEmlOgg=
069300160914     C                               'BARTVAS - Problema Mappatura TIVMS00F'
069400160914     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
069500160914     C                                                                    ':/N'+
069600160914     C                                                                    ':/N'+
069700160914     C                             'Verificare la mappatura: '           +':/N'+
069800160914     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
069900160914     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
070000160914     C                                                                    ':/N'+
070100160914     C                             'Causale mancante: ' + stbCOS         +':/N'+
070200160914     C                             ' '                                   +':/N'
070300160914     C*
070400160914     C                   call(e)   'TIS701C1'
070500160914     C                   parm                    wrkEml
070600160914     C                   parm                    wrkCcEml
070700160914     C                   parm                    wrkEmlOgg
070800160914     C                   parm                    wrkEmlMsg
070900160914     C                   parm                    wrkEsito          1
071000160914     C*
071100160914     C                   ENDSR
071200031103
071300031103
071400991027
071500991027      /TITLE Operazioni iniziali.
071600991027     C     *inzsr        BEGSR
071700991027     C*
071800991027     C     *ENTRY        PLIST
071900150113     C                   parm                    prmppi
072000991027     C     wrkesito      parm      wrkesito      prmesito
072100160914     C*
072200160914     C* Reperisco la data corrente
072300160914     C                   z-add     *zero         datcor            8 0
072400160914     C                   eval      datcor = %dec(%date() : *iso)
072500031103     C*
072600031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
072700150113     C                   EVAL      DSPPI = prmppi
072800131004     C*
072900131004     C* Reperisco i valori variabili ma a livello di transazione corrente
073000131004     C* (tipicamente data e ora relativ al file generato)
073100131004     C                   eval      currDate6    =
073200131004     C                              UBFMTDATE_Convert(
073300131004     C                                  %editc(%dec(%date():*ISO):'X') :
073400131004     C                                  'YYYYMMDD' : 'YYMMDD')
073500131004     C                   eval      currDate6inv =
073600131004     C                              UBFMTDATE_Convert(
073700131004     C                                  %editc(%dec(%date():*ISO):'X') :
073800131004     C                                  'YYYYMMDD' : 'DDMMYY')
073900150527     C                   eval      currDate8    =
074000150527     C                                  %editc(%dec(%date():*ISO):'X')
074100131004     C                   eval      currHHMM =
074200131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
074300150527     C                   eval      currHHMMSS =
074400150527     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:6)
074500030709     C*
074600030709     C* Definizione chiavi
074700030924     C*
074800110103     C* Chiave su TISTB03L - Parziale
074900110103     C     KEYstb03P     KLIST
075000031104     C                   KFLD                    wKstbKSU
075100031103     C                   KFLD                    wKstbFTR
075200031103     C*
075300031103     C* Chiave su TITAS30C - Parziale
075400031103     C     KEYtas30P     KLIST
075500031103     C                   KFLD                    stbAAS
075600031103     C                   KFLD                    stbLNP
075700031103     C                   KFLD                    stbNRS
075800031103     C                   KFLD                    stbNSP
075900150113     C*
076000150113     C* Chiave su TIVIRE1L - Parziale
076100150113     C     KEYvire1C     KLIST
076200150113     C                   KFLD                    w§PPIKSU
076300150113     C                   KFLD                    w§PPITIP
076400150114     C                   KFLD                    vireDTI
076500150113     C                   KFLD                    vireTRC
076600160914     C*
076700160914     C* Chiave su TIVMS01I - Parziale
076800160914     C     KEYvms01P     KLIST
076900160914     C                   KFLD                    vmsPROC
077000160914     C                   KFLD                    vmsKSU
077100160914     C                   KFLD                    vmsDDE
077200991027     C*
077300991027     C                   ENDSR
