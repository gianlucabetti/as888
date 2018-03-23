000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600110103     FTISTB03L  UF   E           K DISK    commit
000700160223     FTISTBD3I  UF   E           K DISK    commit
000800161130     FTIVMS01I  IF   E           K DISK
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000160628     FFNBLP01L  IF   E           K DISK    usropn
001100160628     F                                     extfile(LibFileBLP01)
001200160628     F                                     extdesc('FILTRAPRD/FNBLP01L')
001300170315     FFNARB01L  IF   E           K DISK    usropn
001400170315     F                                     extfile(LibFileARB01)
001500170315     F                                     extdesc('FILTRAPRD/FNARB01L')
001600161130
001700161130
001800161130     D*-----------
001900161130     D* SCHIERE
002000161130     D*-----------
002100161130     D NumElem         C                   CONST(300)
002200161130     D skCAU_BRT       S                   DIM(NumElem) LIKE(vmsCAUBRT)
002300161130     D skCAU_EVT       S                   DIM(NumElem) LIKE(dsCLI_EVT)
002400161130     D skCAU_RSN       S                   DIM(NumElem) LIKE(dsCLI_RSN)
002500161130     D skCAU_OPZ       S                   DIM(NumElem) LIKE(vmsSTSOPZ)
002600161130     D skCAU_DES       S                   DIM(NumElem) LIKE(vmsDESCRIZ)
002700161130     D jCAU            S              3  0 INZ(*zeros)
002800161130     D j               S              3  0 INZ(*zeros)
002900161130     D*-----------
003000161130     D* DS RIDEFINIZIONE STATUS CLIENTE
003100161130     D*-----------
003200161130     D DS_VMSSTSCLI    DS                  INZ
003300161130     D  dsCLI_EVT                    10
003400161130     D  dsCLI_RSN                     5
003500161130     D*-----------
003600161130     D* COSTANTI SPECIFICHE DEL TRADUTTORE
003700161130     D*-----------
003800161130     D  w§PPIVMSPROC   S                   LIKE(vmsPROC) INZ('TITVED01R')
003900161130     D  w§PPIVMSKSU    S                   LIKE(vmsKSU)  INZ
004000161130     D*-----------
004100161130     D* VARIABILI SPECIFICHE MAPPATURA STATI
004200161130     D*-----------
004300161130     D wWrkSts         S                   like(dsCLI_EVT)  inz
004400161130     D wWrkRsn         S                   like(dsCLI_RSN)  inz
004500161130     D wWrkOpz         S                   like(vmsSTSOPZ)  inz
004600161130     D wWrkDes         S                   like(vmsDESCRIZ) inz
004700161130     D*-----------
004800131104
004900131104
005000041015     D*-----------
005100161130     D* SCHIERE DETTAGLI TISTB
005200041015     D*-----------
005300161206     D NumDETT         C                   CONST(300)
005400161130     D skDETT_RIF      S             15    DIM(NumDETT)
005500161130     D skDETT_DAT      S              8  0 DIM(NumDETT)
005600161130     D skDETT_ORA      S              6  0 DIM(NumDETT)
005700161130     D jDETT           S              3  0 INZ(*zeros)
005800030924
005900041015     D*-----------
006000041015     D* VARIABILI D WRK
006100041015     D*-----------
006200161206     D psds           sds
006300161206     D  procname         *PROC
006400031103     D prmppi          S             50
006500030924     D prmesito        S              1
006600030924     D wrkesito        S                   like(prmesito)
006700161206     D campo_alfa      S             70    varying
006800160224     D wDataStato      S              8  0 inz
006900160224     D wOraStato       S              6  0 inz
007000131004     D currDate6       S              6
007100131004     D currDate6inv    S              6
007200131004     D currHHMM        S              4
007300131216     D Conta_msg       S             10i 0
007400131216     D Conta_segm      S             10i 0
007500161130     D Conta_Stati     S             10i 0 inz
007600170915     D Tot_Stati       S             10i 0 inz(900000)
007700161130     D LibFileBLP01    S             21A   inz
007800170315     D LibFileARB01    S             21A   inz
007900161130     D currSysNeta     S              8A   inz('*NULL')
008000131004
008100131004
008200131004     D*-----------
008300131004     D* DEFINIZIONE DS ESTERNE
008400131004     D*-----------
008500161130     D TRUL33DS      E DS                  inz
008600161130     D KPJBA         E DS                  inz
008700161206     D DSTBDNPG5     E DS                  inz
008800170127     D TIS7VASDS     E DS                  inz
008900031103
009000131004
009100131004     D*------------------
009200131004     D* LINKING A DEFINIZIONI ESTERNE
009300131004     D*------------------
009400131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
009500131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
009600160628     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009700160628     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009800161130     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
009900131004
010000131004
010100031103     D*-----------
010200031103     D* VARIABILI RIFERITE AL DATA-BASE
010300031103     D*-----------
010400080618     D wKstbKSU        S                   like(stbKSU)
010500080618     D wKstbFTR        S                   like(stbFTR)
010600030924
010700110705
010800031103
010900031103     D*-----------
011000031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
011100031103     D*-----------
011200031103     D DSPPI           DS
011300080618     D  w§PPIKSU                      7  0
011400131004     D  w§UNB_ID                     25
011500131004
011600131004
011700131004     D*-----------
011800131004     D* COSTANTI / DEFAULT
011900131004     D*-----------
012000131004     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+')
012100131007     D UNH_sgm_1       C                   CONST('UNH+')
012200131007     D UNH_sgm_2       C                   CONST('+IFTSTA:D:01A:UN:EAN008')
012300131004     D BGM_sgm         C                   CONST('BGM+23+')
012400131004     D NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
012500131004     D NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
012600131004     D NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
012700131004     D NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
012800131004     D NAD_SF_5        C                   CONST('NNI++29015+IT')
012900131004     D NAD_ST          C                   CONST('NAD+ST++')
013000131010     D LOC_sgm1        C                   CONST('LOC+175+')
013100131010     D LOC_sgm2        C                   CONST('::3:')
013200131010     D LOC_sgm3        C                   CONST('+IT')
013300131004     D CNI_sgm         C                   CONST('CNI+1')
013400170206     D STS_sgm         C                   CONST('STS+1+')
013500131004     D RFF_sgm         C                   CONST('RFF+POR:')
013600131004     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX')
013700131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
013800131004     D EQD_sgm         C                   CONST('EQD+AA')
013900131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
014000160215     D GID_sgm1        C                   CONST('GID+1+')
014100160215     D GID_sgm2        C                   CONST(':CTN')
014200131004     D UNT_sgm         C                   CONST('UNT+')
014300131004     D UNZ_sgm         C                   CONST('UNZ+')
014400131004     D piu             C                   CONST('+')
014500131004     D diviso          C                   CONST(':')
014600131004     D apice           C                   CONST('''')
014700131004
014800030924
014900030325
015000030924     C* MAIN/
015100160628     C*
015200160628     C* Reperisco il sistema AS/400 corrente
015300160628     C                   callp     UBRTVNETA_Rtv(currSysNeta)
015400160628     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
015500160628     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
015600170315     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
015700160628     C                   else
015800160628     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
015900170315     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
016000160628     C                   endif
016100160628     C*
016200160628     C* Apertura file "overraidati"
016300160628     C                   if        not %open(FNBLP01L)
016400160628     C                   open      FNBLP01L
016500160628     C                   endif
016600170315     C                   if        not %open(FNARB01L)
016700170315     C                   open      FNARB01L
016800170315     C                   endif
016900161130     C*
017000161130     C* Carica schiere di mappatura
017100161130     C                   SETOFF                                       40
017200161130     C                   EXSR      carMAP
017300161130     C*
017400161130     C* Se caricamento mappatura tutto OK => Procedo
017500161130     C                   IF        not *in40
017600140308     C*
017700140308     C* Spia dati da inviare
017800140308     C                   SETOFF                                       45
017900041015     C*
018000041015     C* Esegue traduzione
018100140308     C                   SETOFF                                       31
018200131004     C  N31              EXSR      chkRECFILMBR
018300131004     C  N31              EXSR      rtvID
018400131004     C  N31              EXSR      traduci
018500140308     C  N31
018600140308     CAN 45              EXSR      wriFooter
018700161130     C*
018800161130     C* Diversamente traduzioe completamente fallita
018900161130     C                   ELSE
019000161130     C                   EVAL      wrkesito = '2'
019100161130     C                   ENDIF
019200160628     C*
019300160628     C* Chiusura file "overraidati"
019400160628     C                   if        %open(FNBLP01L)
019500160628     C                   close     FNBLP01L
019600160628     C                   endif
019700170315     C                   if        %open(FNARB01L)
019800170315     C                   close     FNARB01L
019900170315     C                   endif
020000110705     C*
020100921023     C                   SETON                                        LR
020200131004
020300131004
020400131004
020500131004     C*----------------------------------------------------*
020600131004     C*  STACCO ID UNIVOCO
020700131004     C*----------------------------------------------------*
020800131004     C     RTVID         BEGSR
020900131004     C*
021000131004     C                   z-add     *zeros        currUNIQ_ID      15 0
021100131004     C*
021200131004     C* NSP => Stacco un numeratore da AZNUM
021300131004     C                   clear                   TRUL33DS
021400131004     C                   eval      I33OPE = *zeros
021500131004     C                   eval      I33CNU = 302
021600131004     C                   eval      I33NUM = 1
021700131004     C                   movel     TRUL33DS      KPJBU
021800131004     C                   call      'TRUL33R'
021900131004     C                   parm                    KPJBA
022000131004     C                   movel     KPJBU         TRUL33DS
022100131004     C                   if        O33ERR = *zeros
022200131004     C                   z-add     O33NRF        currUNIQ_ID
022300131004     C                   else
022400131004     C                   seton                                        31
022500131004     C                   endif
022600131004     C*
022700131004     C                   ENDSR
022800131004
022900131004
023000131004
023100131004     C     chkRECFILMBR  BEGSR
023200131004     C*
023300131004     C     *start        setll     TIVAWWWT
023400131004     C                   if        %found(TIVAWWWT)
023500131004     C                   seton                                        31
023600131004     C                   endif
023700131004     C*
023800131004     C                   ENDSR
023900131004
024000131004
024100131004
024200131004     C     wriOUTPUT     BEGSR
024300131004     C*
024400131004     C                   write     TIVAW000
024500170207     C                   eval      Conta_Stati = Conta_Stati + 1
024600131004     C*
024700131004     C                   ENDSR
024800131004
024900130807
025000161130
025100161130     C     carMAP        BEGSR
025200161130     C*
025300161130     C* Reperisco la versione di mappatura stati correntemente valida
025400161130     C                   call      'UBVMSDDER'
025500161130     C                   parm      *blanks       iVMSDDEOpz
025600161130     C                   parm      w§PPIVMSPROC  iVMSDDEProc
025700161130     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
025800161130     C                   parm      datcor        iVMSDDEDtRif
025900161130     C                   parm                    oVMSDDEDDE
026000161130     C                   parm                    oVMSDDEEsito
026100161130     C*
026200161130     C* Verifico se esito no OK
026300161130     C                   if        oVMSDDEEsito <> '1'
026400161130     C                   seton                                        40
026500161130     C                   else
026600161130     C*
026700161130     C                   move(P)   w§PPIVMSPROC  vmsPROC
026800161130     C                   move(P)   w§PPIVMSKSU   vmsKSU
026900161130     C                   z-add     oVMSDDEDDE    vmsDDE
027000161130     C     KEYvms01P     setll     tivms01i
027100161130     C                   if        %equal(tivms01i)
027200161130     C     KEYvms01P     reade     tivms01i
027300161130     C*
027400161130     C* Inizializzo indice schiera di mapping
027500161130     C                   z-add     1             jCAU
027600161130     C*
027700161130     C* Ciclo x tutti i record della mappatura richiesta
027800161130     C                   dow       not %eof(tivms01i)
027900161130     C*
028000161130     C* Ignoro i record annullati
028100161130     C                   if        vmsATB = *blanks
028200161130     C*
028300161130     C* Carico la mappatura dei codici status personalizzati cliente
028400161130     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
028500161130     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
028600161130     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
028700161130     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
028800161130     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
028900161130     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
029000161130     C                   add       1             jCAU
029100161130     C*
029200161130     C                   endif
029300161130     C*
029400161130     C     KEYvms01P     reade     tivms01i
029500161130     C                   enddo
029600161130     C*
029700161130     C                   endif
029800161130     C*
029900161130     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
030000161130     C* altrimenti errore bloccante per intera traduzione
030100161130     C                   if        jCAU > 1
030200161130     C                   else
030300161130     C                   seton                                        40
030400161130     C                   endif
030500161130     C*
030600161130     C                   endif
030700161130     C*
030800161130     C                   ENDSR
030900131104
031000131004
031100131004
031200131004     C     Char_Speciali BEGSR
031300131111     C*
031400131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
031500161202     C                   EVAL      campo_alfa = %scanrpl('?':'??':campo_alfa)
031600161202     C                   EVAL      campo_alfa = %scanrpl('+':'?+':campo_alfa)
031700161202     C                   EVAL      campo_alfa = %scanrpl(':':'?:':campo_alfa)
031800161202     C                   EVAL      campo_alfa = %scanrpl(',':'?,':campo_alfa)
031900161202     C                   EVAL      campo_alfa = %scanrpl('''':'?''':campo_alfa)
032000131004     C*
032100131004     C                   ENDSR
032200131004
032300131004
032400131004
032500131004     C     wriHeader     BEGSR
032600131004     C*
032700131004     C                   clear                   vawDTA
032800131010     C*
032900131004     C*  UNB -
033000131004     C                   eval      vawDTA = UNB_sgm
033100131004     C                              + %trim(w§UNB_ID)
033200131004     C                              +   piu
033300131004     C                              + %trim(currDate6inv)
033400131004     C                              +   diviso
033500131004     C                              + %trim(currHHMM)
033600131004     C                              +   piu
033700131004     C                              + %trim(
033800131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
033900131004     C                              +   apice
034000131004     C                   exsr      wriOUTPUT
034100131004     C*
034200131004     C                   ENDSR
034300131004
034400131004
034500131004
034600131004     C     wriFooter     BEGSR
034700131004     C*
034800131004     C                   clear                   vawDTA
034900131004     C*
035000131004     C*  UNZ -
035100131004     C                   eval      vawDTA = UNZ_sgm
035200131004     C                              + %trim(%editc(Conta_msg:'4'))
035300131004     C                              +   piu
035400131004     C                              + %trim(
035500131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
035600131004     C                              +   apice
035700131004     C                   exsr      wriOUTPUT
035800131004     C*
035900131004     C                   ENDSR
036000131004
036100030924
036200030924
036300991027     C     traduci       BEGSR
036400131004     C*
036500131004     C* Inizializzo i contatori a livello di transazione/messaggio
036600131004     C                   z-add     *zeros        Conta_msg
036700031103     C*
036800031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
036900031103     C* di traduzione (trattasi d "post-traduzione")
037000031104     C                   EVAL      wKstbKSU = w§PPIKSU
037100031103     C                   EVAL      wKstbFTR = *blanks
037200110103     C     KEYstb03P     SETLL     tistb03l
037300110103     C                   IF        %equal(tistb03l)
037400110103     C     KEYstb03P     READE     tistb03l
037500991027     C*
037600161206     C* Ciclo x tutti record del cliente da trasmettere o fino a MAX numero stati per singolo file
037700161130     C                   DOW       not %eof(tistb03l)
037800161206     C                             AND Conta_Stati < Tot_Stati
037900131004     C*
038000131004     C* Inizializzo i contatori a livello di segmento
038100131004     C                   z-add     *zeros        Conta_segm
038200080618     C*
038300130807     C                   SETON                                        50
038400031103     C*
038500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
038600031103     C* deve restituire al cliente nel file tradotto
038700031103     C                   IF        stbFTR = *blanks                             * ridondante
038800110103     C*
038900110103     C* Gestisco solamente gli stati standard
039000160203     C                   IF        (
039100160203     C                             (stbTIS='1' AND stbPRS<>'7')      OR
039200160203     C                             (stbTIS='1' AND stbPRS ='7'
039300160203     C                                         AND stbCOS ='CONSOK') OR
039400160223     C                             (stbTIS='1' AND stbPRS ='8'
039500160223     C                                         AND stbCOS ='SPUNTNPG5') OR
039600160203     C                             (stbTIS='9' AND stbPRS ='1')
039700160203     C                             ) AND
039800160203     C                             (%subst(stbFOP:1:1)= 'O'  OR
039900160203     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
040000160203     C                             )
040100131004     C*
040200131004     C* Per "sicurezza" non invio stati senza data
040300131004     C                   IF        stbDAS > *zeros
040400110103     C*
040500110103     C                   SETOFF                                       50
040600160628     C                   SETON                                        22
040700031103     C*
040800031103     C* Innanzitutto chaino la bolla direttamente su TITAS
040900160628     C     KEYtas30P     CHAIN     titas30c
041000031103     C                   IF        %found(titas30c)
041100160628     C                   ELSE
041200160628     C*
041300160628     C* Se non trovato TITAS => bolla ancora solo in partenza (FNBLP)
041400160628     C                   CLEAR                   TITAS000
041500160628     C     KEYtas30P     CHAIN     fnblp01l
041600160628     C                   IF        %found(fnblp01l)
041700160628     C*
041800160628     C* Overrido i campi di TITAS con quelli del FNBLP
041900160628     C                   EVAL      tasLNP = blpLNP
042000160628     C                   EVAL      tasLNA = blpLNA
042100160628     C                   EVAL      tasRSD = blpRSD
042200160628     C                   EVAL      tasLOD = blpLOD
042300160628     C                   EVAL      tasIND = blpIND
042400160628     C                   EVAL      tasPRD = blpPRD
042500160628     C                   EVAL      tasCAD = blpCAD
042600160628     C                   EVAL      tasPKB = blpPKB
042700160628     C                   EVAL      tasPKC = blpPKC
042800160628     C                   EVAL      tasNCL = blpNCL
042900160628     C                   ELSE
043000160628     C*
043100160628     C* Se non trovato neanche in BLP allora => bolla non trovata !!
043200160628     C                   SETOFF                                       22
043300160628     C                   ENDIF
043400160628     C                   ENDIF
043500160628     C*
043600160628     C* Se bolla tutto OK
043700160628     C                   IF        *in22
043800170315     C*
043900170315     C* Aggancio anche la bolla in arrivo per reperire Data Arrivo Collo
044000170315     C                   Z-ADD     *zeros        wDatArrCol        8 0
044100170315     C     KEYtas30P     CHAIN     fnarb01l
044200170315     C                   IF        %found(fnarb01l)
044300170315     C                   EVAL      wDatArrCol = arbDAM
044400170315     C                   ELSE
044500170315     C                   EVAL      wDatArrCol = tasDTI
044600170315     C                   ENDIF
044700131010     C*
044800131010     C* Innanzitutto chaino la bolla direttamente su TITAS
044900131216     C                   MOVEL     *zeros        wPO               3 0
045000131213     C*
045100131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
045200170315     C                   if        wDatArrCol = *zeros
045300131213     C                   eval      wPO = tasLNP
045400131213     C                   else
045500131213     C                   eval      wPO = tasLNA
045600131213     C                   endif
045700131213     C*
045800131213     C     wPO           CHAIN     azorg01l
045900160224     C                   IF        %found(azorg01l)
046000080618     C*
046100110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
046200031103     C                   Z-ADD     1             j
046300161130     C                   MOVEL(P)  stbCOS        wCOS_15          15
046400161130     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
046500161130     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
046600161130     C                   EVAL      wWrkSts = skCAU_EVT(j)
046700161130     C                   EVAL      wWrkRsn = skCAU_RSN(j)
046800160224     C                   EVAL      wWrkOpz = skCAU_OPZ(j)
046900161130     C                   EVAL      wWrkDes = skCAU_DES(j)
047000140514     C                   SETOFF                                       50
047100031103     C                   ELSE
047200161130     C                   CLEAR                   wWrkSts
047300161130     C                   CLEAR                   wWrkRsn
047400161130     C                   CLEAR                   wWrkOpz
047500161130     C                   CLEAR                   wWrkDes
047600031103     C                   SETON                                        50
047700031103     C                   ENDIF
047800161130     C  N55              EXSR      sndAlert
047900140308     C*
048000140308     C* Al primo dettaglio da inviare => scrivo anche testata
048100140313     C                   IF        not *IN50
048200140313     C  N45              EXSR      wriHeader
048300140308     C                   SETON                                        45
048400140313     C                   ENDIF
048500080618     C*
048600160224     C* Se ok per procedere
048700160224     C                   IF        not *IN50
048800160224     C*
048900160224     C* Inizializzo le schiere di work
049000160224     C                   CLEAR                   skDETT_RIF
049100160224     C                   CLEAR                   skDETT_DAT
049200160224     C                   CLEAR                   skDETT_ORA
049300160224     C*
049400160224     C* Verifico il tipo di opzione prevista per il codice stato corrente
049500161130     C                   SELECT
049600161130     C                   WHEN      wWrkOpz = '*DETT'
049700160224     C                   EXSR      RTVDETT
049800161130     C                   ENDSL
049900160224     C*
050000160224     C* Scarico il buffer d output
050100160224     C                   EXSR      wriStato
050200160224     C                   ENDIF
050300110103     C*
050400131010     C                   ENDIF
050500160628 ==> C                   ENDIF                                                  * TITAS30C
050600160628     C*
050700110103     C                   ENDIF
050800110103     C                   ENDIF
050900110103     C                   ENDIF
051000031103     C*
051100031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
051200031103     C                   EVAL      stbFTR = 'T'
051300031103     C                   UPDATE    tistb000
051400991027     C*
051500110103     C     KEYstb03P     READE     tistb03l
051600031103     C                   SETOFF                                       50
051700030325     C                   ENDDO
051800161206     C*
051900161206     C* Se raggiunto MAX numero stati bolla per stesso file => esco ed inserisco record "trigger"
052000161206     C                   IF        Conta_Stati >= Tot_Stati
052100170127     C                   CLEAR                   TIS7VASDS
052200170127     C                   EVAL      i§7VASOPZ = 'TRG'
052300170127     C                   EVAL      i§7VASTIP = 'VC'
052400170127     C                   EVAL      i§7VASKSU = '0'+%editc(w§PPIKSU:'X')
052500170127     C                   EVAL      i§7VASTSC = 'WW'
052600170127     C                   EVAL      i§7VASPGM = procname
052700161206     C                   CALL      'TIS7VASR1'
052800170127     C                   PARM                    TIS7VASDS
052900161206     C                   ENDIF
053000161206     C*
053100031103     C                   ENDIF
053200991027     C*
053300030325     C                   EVAL      wrkesito = '0'
053400991027     C*
053500910830     C                   ENDSR
053600031103
053700031103
053800031103
053900131004     C     wriStato      BEGSR
054000131004     C*
054100131004     C* Incremento il contatore di messaggio (al interno della transazione)
054200131004     C                   eval      Conta_msg = Conta_msg + 1
054300131004     C*
054400131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
054500131104     C                   movel     *blanks       wAlfaRMA        512
054600131024     C                   movel     *blanks       wAlfaRSD        512
054700131004     C                   movel     *blanks       wAlfaLOD        512
054800131004     C                   movel     *blanks       wAlfaIND        512
054900131025     C                   movel     *blanks       wAlfaORGDES     512
055000131004     C* RMA
055100131004     C                   eval      campo_alfa = stbRMA
055200131004     C                   exsr      Char_Speciali
055300131004     C                   eval      wAlfaRMA = campo_alfa
055400131024     C* RSD
055500131024     C                   eval      campo_alfa = tasRSD
055600131024     C                   exsr      Char_Speciali
055700131024     C                   eval      wAlfaRSD = campo_alfa
055800131004     C* LOD
055900131004     C                   eval      campo_alfa = tasLOD
056000131004     C                   exsr      Char_Speciali
056100131004     C                   eval      wAlfaLOD = campo_alfa
056200131004     C* IND
056300131004     C                   eval      campo_alfa = tasIND
056400131004     C                   exsr      Char_Speciali
056500131004     C                   eval      wAlfaIND = campo_alfa
056600131025     C* ORGDES
056700131025     C                   eval      campo_alfa = orgDES
056800131025     C                   exsr      Char_Speciali
056900131025     C                   eval      wAlfaORGDES = campo_alfa
057000131004     C*
057100131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
057200131004     C* (ancora corretto ???)
057300131004     C                   z-add     *zeros        wPeso             7 1
057400131004     C                   if        tasPKB > tasPKC
057500131004     C                   eval      wPeso = tasPKB
057600131004     C                   else
057700131004     C                   eval      wPeso = tasPKC
057800131004     C                   endif
057900160224     C*
058000160224     C* Valorizzo la data e ora dello stato corrente
058100160224     C                   eval      wDataStato = stbDAS
058200160224     C                   eval      wOraStato  = stbORS*100
058300160224     C*
058400160606     C* Verifico se per lo stato corrrente sono presenti info anche a livello di collo
058500160224     C                   MOVEL(p)  stbRMN        wAlfaRMN         15
058600160224     C                   Z-ADD     1             j
058700160224     C     wAlfaRMN      LOOKUP    skDETT_RIF(j)                          55
058800160224     C                   IF        %found
058900160224     C                   eval      wDataStato =  skDETT_DAT(j)
059000160224     C                   eval      wOraStato  =  skDETT_ORA(j)
059100160224     C                   ENDIF
059200160224     C*
059300131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
059400131004     C*
059500131004     C*  UNH -
059600131004     C                   eval      Conta_segm  = Conta_segm  + 1
059700131007     C                   eval      vawDTA = UNH_sgm_1
059800131007     C                              + %trim(%editc(Conta_msg:'4'))
059900131007     C                              + UNH_sgm_2
060000131004     C                              +   apice
060100131004     C                   exsr      wriOUTPUT
060200131004     C*
060300131004     C*  BGM -
060400131004     C                   eval      Conta_segm  = Conta_segm  + 1
060500131004     C                   eval      vawDTA = BGM_sgm
060600131004     C                              + %trim(%editc(stbRMN:'4'))
060700131004     C                              +   apice
060800131004     C                   exsr      wriOUTPUT
060900131004     C*
061000131004     C*  NAD SF -
061100131008     C***                eval      Conta_segm  = Conta_segm  + 1
061200131008     C***                eval      vawDTA = NAD_SF_1
061300131008     C***                           + NAD_SF_2
061400131008     C***                           + NAD_SF_3
061500131008     C***                           + NAD_SF_4
061600131008     C***                           + NAD_SF_5
061700131008     C***                           +   apice
061800131008     C***                exsr      wriOUTPUT
061900131004     C*
062000131004     C*  NAD ST -
062100131004     C                   eval      Conta_segm  = Conta_segm  + 1
062200131004     C                   eval      vawDTA = NAD_ST
062300131025     C                              + %trim(wAlfaRSD)
062400131004     C                              +   piu
062500131004     C                              +   piu
062600131004     C                              + %trim(wAlfaIND)
062700131004     C                              +   piu
062800131004     C                              + %trim(wAlfaLOD)
062900131004     C                              +   piu
063000131004     C                              + %trim(tasPRD)
063100131004     C                              +   piu
063200131004     C                              + %trim(tasCAD)
063300131004     C                              +   piu
063400131004     C                              + 'IT'
063500131004     C                              +   apice
063600131004     C                   exsr      wriOUTPUT
063700131004     C*
063800131010     C*  LOC -
063900131004     C                   eval      Conta_segm  = Conta_segm  + 1
064000131004     C                   eval      vawDTA = LOC_sgm1
064100131010     C                              + %trim(%editc(orgFIL:'X'))
064200131004     C                              +   LOC_sgm2
064300131025     C                              + %trim(wAlfaORGDES)
064400131010     C                              +   LOC_sgm3
064500131004     C                              +   apice
064600131004     C                   exsr      wriOUTPUT
064700131004     C*
064800131004     C*  CNI -
064900131004     C                   eval      Conta_segm  = Conta_segm  + 1
065000131004     C                   eval      vawDTA = CNI_sgm
065100131004     C                              +   apice
065200131004     C                   exsr      wriOUTPUT
065300131004     C*
065400131004     C*  STS -
065500131004     C                   eval      Conta_segm  = Conta_segm  + 1
065600131004     C                   eval      vawDTA = STS_sgm
065700170206     C                              + %trim(wWrkSts)
065800170206     C                              +   piu
065900170206     C                              + %trim(wWrkRsn)
066000131004     C                              +   apice
066100131004     C                   exsr      wriOUTPUT
066200131004     C*
066300131004     C*  RFF -
066400131004     C                   eval      Conta_segm  = Conta_segm  + 1
066500131004     C                   eval      vawDTA = RFF_sgm
066600131004     C                              + %trim(wAlfaRMA)
066700131004     C                              +   apice
066800131004     C                   exsr      wriOUTPUT
066900131004     C*
067000131004     C*  TDT -
067100131004     C                   eval      Conta_segm  = Conta_segm  + 1
067200131004     C                   eval      vawDTA = TDT_sgm
067300131004     C                              +   apice
067400131004     C                   exsr      wriOUTPUT
067500131004     C*
067600131004     C*  DTM -
067700131004     C                   eval      Conta_segm  = Conta_segm  + 1
067800131004     C                   eval      vawDTA = DTM_sgm
067900160224     C                              + %trim(%editc(wDataStato*1000000+
068000160224     C                                             wOraStato:'4'))
068100131004     C                              +   diviso
068200131007     C                              + '204'
068300131004     C                              +   apice
068400131004     C                   exsr      wriOUTPUT
068500131004     C*
068600131004     C*  EQD -
068700131004     C                   eval      Conta_segm  = Conta_segm  + 1
068800131004     C                   eval      vawDTA = EQD_sgm
068900131004     C                              +   apice
069000131004     C                   exsr      wriOUTPUT
069100131004     C*
069200131004     C*  MEA -
069300131004     C                   eval      Conta_segm  = Conta_segm  + 1
069400131004     C                   eval      vawDTA = MEA_sgm
069500131004     C                              + %trim(%editc(wPeso:'3'))
069600131004     C                              + '0'
069700131004     C                              +   apice
069800131004     C                   exsr      wriOUTPUT
069900160215     C*
070000160215     C*  GID -
070100160215     C                   eval      Conta_segm  = Conta_segm  + 1
070200160215     C                   eval      vawDTA = GID_sgm1
070300160215     C                              + %trim(%editc(tasNCL:'4'))
070400160215     C                              +   GID_sgm2
070500160215     C                              +   apice
070600160215     C                   exsr      wriOUTPUT
070700131004     C*
070800131004     C*  UNT -
070900131004     C                   eval      Conta_segm  = Conta_segm  + 1
071000131004     C                   eval      vawDTA = UNT_sgm
071100131004     C                              + %trim(%editc(Conta_segm:'4'))
071200131004     C                              +   piu
071300131007     C                              + %trim(%editc(Conta_msg:'4'))
071400131004     C                              +   apice
071500131004     C                   exsr      wriOUTPUT
071600031103     C*
071700031103     C                   ENDSR
071800160224
071900160224
072000160224
072100160224     C     RTVDETT       BEGSR
072200160224     C*
072300160224     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
072400160224     C                   Z-ADD     *zeros        jDETT
072500160224     C     KEYstbd3P     SETLL     tistbd3i
072600160224     C                   IF        %equal(tistbd3i)
072700160224     C     KEYstbd3P     READE     tistbd3i
072800160224     C                   DOW       not %eof(tistbd3i)
072900160224     C* Filtro unicamente i segnacolli "BRT"
073000160224     C                   IF        stbdTIS = 'BRT'
073100160606     C* Considero solamente se presenti dati di data/ora spunta
073200160606     C                   IF        stbdDAD > *zeros AND
073300160606     C                             stbdORD > *zeros
073400160224     C                   ADD       1             jDETT
073500160224     C                   EVAL      DSTBDNPG5 = STBDDTA
073600160224     C                   EVAL      skDETT_RIF(jDETT) = §STBDNPG51
073700160224     C                   EVAL      skDETT_DAT(jDETT) = stbdDAD
073800160224     C                   EVAL      skDETT_ORA(jDETT) = stbdORD
073900160606     C                   ENDIF
074000160224     C                   ENDIF
074100160224     C     KEYstbd3P     READE     tistbd3i
074200160224     C                   ENDDO
074300160224     C                   ENDIF
074400160224     C*
074500160224     C                   ENDSR
074600161130
074700161130
074800161130
074900161130     C*--------------------------------------------------------------------------------------------*
075000161130     C* /TITLE Compongo il testo e spedisco una e-mail di alert
075100161130     C*--------------------------------------------------------------------------------------------*
075200161130     C     sndAlert      BEGSR
075300161130     C*
075400161130     C* Inizializzo variabili
075500161130     C                   movel     *blanks       wrkEml          253
075600161130     C                   movel     *blanks       wrkCcEml        253
075700161130     C                   movel     *blanks       wrkEmlMsg      5000
075800161130     C                   movel     *blanks       wrkEmlOgg        44
075900161130     C*
076000161130     C* Valorizzo i campi della e-m@ail
076100161130     C                   eval      wrkEml='cedalert@brt.it'
076200161130     C                   eval      wrkEmlOgg=
076300161130     C                               'BARTVAS - Problema Mappatura TIVMS00F'
076400161130     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
076500161130     C                                                                    ':/N'+
076600161130     C                                                                    ':/N'+
076700161130     C                             'Verificare la mappatura: '           +':/N'+
076800161130     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
076900161130     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
077000161130     C                                                                    ':/N'+
077100161130     C                             'Causale mancante: ' + stbCOS         +':/N'+
077200161130     C                             ' '                                   +':/N'
077300161130     C*
077400161130     C                   call(e)   'TIS701C1'
077500161130     C                   parm                    wrkEml
077600161130     C                   parm                    wrkCcEml
077700161130     C                   parm                    wrkEmlOgg
077800161130     C                   parm                    wrkEmlMsg
077900161130     C                   parm                    wrkEsito          1
078000161130     C*
078100161130     C                   ENDSR
078200031103
078300031103
078400991027
078500991027      /TITLE Operazioni iniziali.
078600991027     C     *inzsr        BEGSR
078700991027     C*
078800991027     C     *ENTRY        PLIST
078900031103     C                   parm                    prmppi
079000991027     C     wrkesito      parm      wrkesito      prmesito
079100031103     C*
079200031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
079300031103     C                   EVAL      DSPPI = prmppi
079400131004     C*
079500131004     C* Reperisco i valori variabili ma a livello di transazione corrente
079600131004     C* (tipicamente data e ora relativ al file generato)
079700131004     C                   eval      currDate6    =
079800131004     C                              UBFMTDATE_Convert(
079900131004     C                                  %editc(%dec(%date():*ISO):'X') :
080000131004     C                                  'YYYYMMDD' : 'YYMMDD')
080100131004     C                   eval      currDate6inv =
080200131004     C                              UBFMTDATE_Convert(
080300131004     C                                  %editc(%dec(%date():*ISO):'X') :
080400131004     C                                  'YYYYMMDD' : 'DDMMYY')
080500131004     C                   eval      currHHMM =
080600131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
080700161130     C*
080800161130     C* CALCOLA LA DATA CORRENTE
080900161130     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081000161130     C                   eval      datcor = %dec(%date() : *ISO)
081100030709     C*
081200030709     C* Definizione chiavi
081300030924     C*
081400110103     C* Chiave su TISTB03L - Parziale
081500160224     C     KEYstb03P     KLIST
081600160224     C                   KFLD                    wKstbKSU
081700160224     C                   KFLD                    wKstbFTR
081800160224     C*
081900160224     C* Chiave su TISTBD3I - Parziale
082000160224     C     KEYstbd3P     KLIST
082100160224     C                   KFLD                    wKstbFTR
082200160224     C                   KFLD                    stbKSU
082300160224     C                   KFLD                    stbAAS
082400160224     C                   KFLD                    stbLNP
082500160224     C                   KFLD                    stbNRS
082600160224     C                   KFLD                    stbNSP
082700160224     C                   KFLD                    stbDAS
082800160224     C                   KFLD                    stbORS
082900160224     C                   KFLD                    stbCOS
083000031103     C*
083100031103     C* Chiave su TITAS30C - Parziale
083200031103     C     KEYtas30P     KLIST
083300031103     C                   KFLD                    stbAAS
083400031103     C                   KFLD                    stbLNP
083500031103     C                   KFLD                    stbNRS
083600031103     C                   KFLD                    stbNSP
083700161130     C*
083800161130     C* Chiave su TIVMS01I - Parziale
083900161130     C     KEYvms01P     KLIST
084000161130     C                   KFLD                    vmsPROC
084100161130     C                   KFLD                    vmsKSU
084200161130     C                   KFLD                    vmsDDE
084300991027     C*
084400991027     C                   ENDSR
084500161130
