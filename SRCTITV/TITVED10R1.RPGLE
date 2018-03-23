000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500031103     FTITAS30C  IF   E           K DISK
000600110103     FTISTB03L  UF   E           K DISK    commit
000700160304     FTISTBD3I  UF   E           K DISK    commit
000800150814     FFIRAB01L  IF   E           K DISK
000900170207     FTIVMS01I  IF   E           K DISK
001000090323     FTIVAWWWT  UF A E             DISK    commit
001100160628     FFNBLP01L  IF   E           K DISK    usropn
001200160628     F                                     extfile(LibFileBLP01)
001300160628     F                                     extdesc('FILTRAPRD/FNBLP01L')
001400170315     FFNARB01L  IF   E           K DISK    usropn
001500170315     F                                     extfile(LibFileARB01)
001600170315     F                                     extdesc('FILTRAPRD/FNARB01L')
001700170207
001800170207
001900170207     D*-----------
002000170207     D* SCHIERE
002100170207     D*-----------
002200170207     D NumElem         C                   CONST(300)
002300170207     D skCAU_BRT       S                   DIM(NumElem) LIKE(vmsCAUBRT)
002400170207     D skCAU_EVT       S                   DIM(NumElem) LIKE(dsCLI_EVT)
002500170207     D skCAU_RSN       S                   DIM(NumElem) LIKE(dsCLI_RSN)
002600170207     D skCAU_OPZ       S                   DIM(NumElem) LIKE(vmsSTSOPZ)
002700170207     D skCAU_DES       S                   DIM(NumElem) LIKE(vmsDESCRIZ)
002800170207     D jCAU            S              3  0 INZ(*zeros)
002900170207     D j               S              3  0 INZ(*zeros)
003000170207     D*-----------
003100170207     D* DS RIDEFINIZIONE STATUS CLIENTE
003200170207     D*-----------
003300170207     D DS_VMSSTSCLI    DS                  INZ
003400170207     D  dsCLI_EVT                    10
003500170207     D  dsCLI_RSN                     5
003600170207     D*-----------
003700170207     D* COSTANTI SPECIFICHE DEL TRADUTTORE
003800170207     D*-----------
003900170207     D  w§PPIVMSPROC   S                   LIKE(vmsPROC) INZ('TITVED10R1')
004000170207     D  w§PPIVMSKSU    S                   LIKE(vmsKSU)  INZ
004100170207     D*-----------
004200170207     D* VARIABILI SPECIFICHE MAPPATURA STATI
004300170207     D*-----------
004400170207     D wWrkSts         S                   like(dsCLI_EVT)  inz
004500170207     D wWrkRsn         S                   like(dsCLI_RSN)  inz
004600170207     D wWrkOpz         S                   like(vmsSTSOPZ)  inz
004700170207     D wWrkDes         S                   like(vmsDESCRIZ) inz
004800170207     D*-----------
004900170207
005000170207
005100170207     D*-----------
005200170207     D* SCHIERE DETTAGLI TISTB
005300170207     D*-----------
005400170207     D NumDETT         C                   CONST(300)
005500170207     D skDETT_RIF      S             15    DIM(NumDETT)
005600170207     D skDETT_DAT      S              8  0 DIM(NumDETT)
005700170207     D skDETT_ORA      S              6  0 DIM(NumDETT)
005800170207     D jDETT           S              3  0 INZ(*zeros)
005900131104
006000131104
006100041015     D*-----------
006200170207     D* SCHIERE PER DE-ACCORPAMENTO
006300041015     D*-----------
006400150814     D skCpyAccorpa    S                   LIKE(vawDTA) DIM(5000) INZ
006500150814     D jCpyAcc         S              5  0 INZ(*zeros)
006600150814
006700030924
006800041015     D*-----------
006900041015     D* VARIABILI D WRK
007000041015     D*-----------
007100170207     D psds           sds
007200170207     D  procname         *PROC
007300031103     D prmppi          S             50
007400030924     D prmesito        S              1
007500030924     D wrkesito        S                   like(prmesito)
007600170207     D campo_alfa      S             70    varying
007700160304     D wDataStato      S              8  0 inz
007800160304     D wOraStato       S              6  0 inz
007900131004     D currDate6       S              6
008000131004     D currDate6inv    S              6
008100131004     D currHHMM        S              4
008200131216     D Conta_msg       S             10i 0
008300131216     D Conta_segm      S             10i 0
008400170207     D Conta_Stati     S             10i 0 inz
008500170915     D Tot_Stati       S             10i 0 inz(900000)
008600170207     D wDepKSU         S                   like(stbKSU) inz
008700150814     D wDepAAS         S                   like(stbAAS) inz
008800150814     D wDepLNP         S                   like(stbLNP) inz
008900150814     D wDepNRS         S                   like(stbNRS) inz
009000150814     D wDepNSP         S                   like(stbNSP) inz
009100160107     D wDepRMN         S                   like(stbRMN) inz
009200160107     D wDepRMA         S                   like(stbRMA) inz
009300150814     D vawDTA_Acc      S                   like(vawDTA) inz
009400160628     D LibFileBLP01    s             21A   inz
009500170315     D LibFileARB01    S             21A   inz
009600160628     D currSysNeta     s              8A   inz('*NULL')
009700131004
009800131004
009900131004     D*-----------
010000131004     D* DEFINIZIONE DS ESTERNE
010100131004     D*-----------
010200170207     D TRUL33DS      E DS                  inz
010300170207     D KPJBA         E DS                  inz
010400170207     D DSTBDNPG5     E DS                  inz
010500170207     D TIS7VASDS     E DS                  inz
010600031103
010700131004
010800131004     D*------------------
010900131004     D* LINKING A DEFINIZIONI ESTERNE
011000131004     D*------------------
011100131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
011200131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
011300160628     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011400160628     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
011500170207     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
011600131004
011700131004
011800031103     D*-----------
011900031103     D* VARIABILI RIFERITE AL DATA-BASE
012000031103     D*-----------
012100080618     D wKstbKSU        S                   like(stbKSU)
012200080618     D wKstbFTR        S                   like(stbFTR)
012300110705
012400031103
012500031103     D*-----------
012600031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
012700031103     D*-----------
012800031103     D DSPPI           DS
012900080618     D  w§PPIKSU                      7  0
013000131004     D  w§UNB_ID                     25
013100131004
013200131004
013300131004     D*-----------
013400131004     D* COSTANTI / DEFAULT
013500131004     D*-----------
013600131004     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+')
013700131007     D UNH_sgm_1       C                   CONST('UNH+')
013800131007     D UNH_sgm_2       C                   CONST('+IFTSTA:D:01A:UN:EAN008')
013900131004     D BGM_sgm         C                   CONST('BGM+23+')
014000131004     D NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
014100131004     D NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
014200131004     D NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
014300131004     D NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
014400131004     D NAD_SF_5        C                   CONST('NNI++29015+IT')
014500131004     D NAD_ST          C                   CONST('NAD+ST++')
014600131010     D LOC_sgm1        C                   CONST('LOC+175+')
014700131010     D LOC_sgm2        C                   CONST('::3:')
014800131010     D LOC_sgm3        C                   CONST('+IT')
014900131004     D CNI_sgm         C                   CONST('CNI+1')
015000170207     D STS_sgm         C                   CONST('STS+1+')
015100131004     D RFF_sgm         C                   CONST('RFF+POR:')
015200131004     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX')
015300131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
015400131004     D EQD_sgm         C                   CONST('EQD+AA')
015500131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
015600160215     D GID_sgm1        C                   CONST('GID+1+')
015700160215     D GID_sgm2        C                   CONST(':CTN')
015800131004     D UNT_sgm         C                   CONST('UNT+')
015900131004     D UNZ_sgm         C                   CONST('UNZ+')
016000131004     D piu             C                   CONST('+')
016100131004     D diviso          C                   CONST(':')
016200131004     D apice           C                   CONST('''')
016300131004
016400030924
016500030325
016600030924     C* MAIN/
016700160628     C*
016800160628     C* Reperisco il sistema AS/400 corrente
016900160628     C                   callp     UBRTVNETA_Rtv(currSysNeta)
017000160628     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
017100160628     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
017200170315     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
017300160628     C                   else
017400160628     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
017500170315     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
017600160628     C                   endif
017700160628     C*
017800160628     C* Apertura file "overraidati"
017900160628     C                   if        not %open(FNBLP01L)
018000160628     C                   open      FNBLP01L
018100160628     C                   endif
018200170315     C                   if        not %open(FNARB01L)
018300170315     C                   open      FNARB01L
018400170315     C                   endif
018500170207     C*
018600170207     C* Carica schiere di mappatura
018700170207     C                   SETOFF                                       40
018800170207     C                   EXSR      carMAP
018900170207     C*
019000170207     C* Se caricamento mappatura tutto OK => Procedo
019100170207     C                   IF        not *in40
019200140308     C*
019300140308     C* Spia dati da inviare
019400140308     C                   SETOFF                                       45
019500041015     C*
019600041015     C* Esegue traduzione
019700140308     C                   SETOFF                                       31
019800131004     C  N31              EXSR      chkRECFILMBR
019900131004     C  N31              EXSR      rtvID
020000131004     C  N31              EXSR      traduci
020100140308     C  N31
020200140308     CAN 45              EXSR      wriFooter
020300170207     C*
020400170207     C* Diversamente traduzioe completamente fallita
020500170207     C                   ELSE
020600170207     C                   EVAL      wrkesito = '2'
020700170207     C                   ENDIF
020800160628     C*
020900160628     C* Chiusura file "overraidati"
021000160628     C                   if        %open(FNBLP01L)
021100160628     C                   close     FNBLP01L
021200160628     C                   endif
021300170315     C                   if        %open(FNARB01L)
021400170315     C                   close     FNARB01L
021500170315     C                   endif
021600110705     C*
021700921023     C                   SETON                                        LR
021800131004
021900131004
022000131004
022100131004     C*----------------------------------------------------*
022200131004     C*  STACCO ID UNIVOCO
022300131004     C*----------------------------------------------------*
022400131004     C     RTVID         BEGSR
022500131004     C*
022600131004     C                   z-add     *zeros        currUNIQ_ID      15 0
022700131004     C*
022800131004     C* NSP => Stacco un numeratore da AZNUM
022900131004     C                   clear                   TRUL33DS
023000131004     C                   eval      I33OPE = *zeros
023100131004     C                   eval      I33CNU = 302
023200131004     C                   eval      I33NUM = 1
023300131004     C                   movel     TRUL33DS      KPJBU
023400131004     C                   call      'TRUL33R'
023500131004     C                   parm                    KPJBA
023600131004     C                   movel     KPJBU         TRUL33DS
023700131004     C                   if        O33ERR = *zeros
023800131004     C                   z-add     O33NRF        currUNIQ_ID
023900131004     C                   else
024000131004     C                   seton                                        31
024100131004     C                   endif
024200131004     C*
024300131004     C                   ENDSR
024400131004
024500131004
024600131004
024700131004     C     chkRECFILMBR  BEGSR
024800131004     C*
024900131004     C     *start        setll     TIVAWWWT
025000131004     C                   if        %found(TIVAWWWT)
025100131004     C                   seton                                        31
025200131004     C                   endif
025300131004     C*
025400131004     C                   ENDSR
025500131004
025600131004
025700131004
025800131004     C     wriOUTPUT     BEGSR
025900131004     C*
026000131004     C                   write     TIVAW000
026100170207     C                   eval      Conta_Stati = Conta_Stati + 1
026200131004     C*
026300131004     C                   ENDSR
026400170207
026500170207
026600170207
026700170207     C     carMAP        BEGSR
026800170207     C*
026900170207     C* Reperisco la versione di mappatura stati correntemente valida
027000170207     C                   call      'UBVMSDDER'
027100170207     C                   parm      *blanks       iVMSDDEOpz
027200170207     C                   parm      w§PPIVMSPROC  iVMSDDEProc
027300170207     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
027400170207     C                   parm      datcor        iVMSDDEDtRif
027500170207     C                   parm                    oVMSDDEDDE
027600170207     C                   parm                    oVMSDDEEsito
027700170207     C*
027800170207     C* Verifico se esito no OK
027900170207     C                   if        oVMSDDEEsito <> '1'
028000170207     C                   seton                                        40
028100170207     C                   else
028200170207     C*
028300170207     C                   move(P)   w§PPIVMSPROC  vmsPROC
028400170207     C                   move(P)   w§PPIVMSKSU   vmsKSU
028500170207     C                   z-add     oVMSDDEDDE    vmsDDE
028600170207     C     KEYvms01P     setll     tivms01i
028700170207     C                   if        %equal(tivms01i)
028800170207     C     KEYvms01P     reade     tivms01i
028900170207     C*
029000170207     C* Inizializzo indice schiera di mapping
029100170207     C                   z-add     1             jCAU
029200170207     C*
029300170207     C* Ciclo x tutti i record della mappatura richiesta
029400170207     C                   dow       not %eof(tivms01i)
029500170207     C*
029600170207     C* Ignoro i record annullati
029700170207     C                   if        vmsATB = *blanks
029800170207     C*
029900170207     C* Carico la mappatura dei codici status personalizzati cliente
030000170207     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
030100170207     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
030200170207     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
030300170207     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
030400170207     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
030500170207     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
030600170207     C                   add       1             jCAU
030700170207     C*
030800170207     C                   endif
030900170207     C*
031000170207     C     KEYvms01P     reade     tivms01i
031100170207     C                   enddo
031200170207     C*
031300170207     C                   endif
031400170207     C*
031500170207     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
031600170207     C* altrimenti errore bloccante per intera traduzione
031700170207     C                   if        jCAU > 1
031800170207     C                   else
031900170207     C                   seton                                        40
032000170207     C                   endif
032100170207     C*
032200170207     C                   endif
032300170207     C*
032400170207     C                   ENDSR
032500131104
032600131004
032700131004
032800131004     C     Char_Speciali BEGSR
032900170207     C*
033000170207     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
033100170207     C                   EVAL      campo_alfa = %scanrpl('?':'??':campo_alfa)
033200170207     C                   EVAL      campo_alfa = %scanrpl('+':'?+':campo_alfa)
033300170207     C                   EVAL      campo_alfa = %scanrpl(':':'?:':campo_alfa)
033400170207     C                   EVAL      campo_alfa = %scanrpl(',':'?,':campo_alfa)
033500170207     C                   EVAL      campo_alfa = %scanrpl('''':'?''':campo_alfa)
033600131004     C*
033700131004     C                   ENDSR
033800131004
033900131004
034000131004
034100131004     C     wriHeader     BEGSR
034200131004     C*
034300131004     C                   clear                   vawDTA
034400131010     C*
034500131004     C*  UNB -
034600131004     C                   eval      vawDTA = UNB_sgm
034700131004     C                              + %trim(w§UNB_ID)
034800131004     C                              +   piu
034900131004     C                              + %trim(currDate6inv)
035000131004     C                              +   diviso
035100131004     C                              + %trim(currHHMM)
035200131004     C                              +   piu
035300131004     C                              + %trim(
035400131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
035500131004     C                              +   apice
035600131004     C                   exsr      wriOUTPUT
035700131004     C*
035800131004     C                   ENDSR
035900131004
036000131004
036100131004
036200131004     C     wriFooter     BEGSR
036300131004     C*
036400131004     C                   clear                   vawDTA
036500131004     C*
036600131004     C*  UNZ -
036700131004     C                   eval      vawDTA = UNZ_sgm
036800131004     C                              + %trim(%editc(Conta_msg:'4'))
036900131004     C                              +   piu
037000131004     C                              + %trim(
037100131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
037200131004     C                              +   apice
037300131004     C                   exsr      wriOUTPUT
037400131004     C*
037500131004     C                   ENDSR
037600131004
037700030924
037800030924
037900991027     C     traduci       BEGSR
038000131004     C*
038100131004     C* Inizializzo i contatori a livello di transazione/messaggio
038200131004     C                   z-add     *zeros        Conta_msg
038300031103     C*
038400031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
038500031103     C* di traduzione (trattasi d "post-traduzione")
038600031104     C                   EVAL      wKstbKSU = w§PPIKSU
038700031103     C                   EVAL      wKstbFTR = *blanks
038800110103     C     KEYstb03P     SETLL     tistb03l
038900110103     C                   IF        %equal(tistb03l)
039000110103     C     KEYstb03P     READE     tistb03l
039100170207     C*
039200170207     C* Ciclo x tutti record del cliente da trasmettere o fino a MAX numero stati per singolo file
039300170207     C                   DOW       not %eof(tistb03l)
039400170207     C                             AND Conta_Stati < Tot_Stati
039500131004     C*
039600131004     C* Inizializzo i contatori a livello di segmento
039700131004     C                   z-add     *zeros        Conta_segm
039800080618     C*
039900130807     C                   SETON                                        50
040000031103     C*
040100031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
040200031103     C* deve restituire al cliente nel file tradotto
040300031103     C                   IF        stbFTR = *blanks                             * ridondante
040400110103     C*
040500110103     C* Gestisco solamente gli stati standard
040600160212     C                   IF        (
040700160212     C                             (stbTIS='1' AND stbPRS<>'7')      OR
040800160212     C                             (stbTIS='1' AND stbPRS ='7'
040900160212     C                                         AND stbCOS ='CONSOK') OR
041000160304     C                             (stbTIS='1' AND stbPRS ='8'
041100160304     C                                         AND stbCOS ='SPUNTNPG5') OR
041200160212     C                             (stbTIS='9' AND stbPRS ='1')
041300160212     C                             ) AND
041400160212     C                             (%subst(stbFOP:1:1)= 'O'  OR
041500160212     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
041600160212     C                             )
041700131004     C*
041800131004     C* Per "sicurezza" non invio stati senza data
041900131004     C                   IF        stbDAS > *zeros
042000150814     C*
042100110103     C                   SETOFF                                       50
042200151005     C*
042300151005     C* Gestisco rottura di codice per spedizione
042400151005     C                   IF        stbKSU = wDepKSU AND
042500151005     C                             stbAAS = wDepAAS AND
042600151005     C                             stbLNP = wDepLNP AND
042700151005     C                             stbNRS = wDepNRS AND
042800151005     C                             stbNSP = wDepNSP
042900151005     C                   ELSE
043000151005     C*
043100151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
043200151005     C  N50              EXSR      DeAccorpa
043300151005     C*
043400151005     C* Salvo la nuova chiave di rottura
043500151005     C                   EVAL      wDepKSU = stbKSU
043600151005     C                   EVAL      wDepAAS = stbAAS
043700151005     C                   EVAL      wDepLNP = stbLNP
043800151005     C                   EVAL      wDepNRS = stbNRS
043900151005     C                   EVAL      wDepNSP = stbNSP
044000160107     C                   EVAL      wDepRMN = stbRMN
044100160107     C                   EVAL      wDepRMA = stbRMA
044200151005     C*
044300151005     C* Inizializzazioni per nuova chiave di rottura
044400151005     C                   CLEAR                   skCpyAccorpa
044500151005     C                   CLEAR                   jCpyAcc
044600151005     C*
044700151005     C                   ENDIF
044800160628     C*
044900160628     C                   SETON                                        22
045000031103     C*
045100031103     C* Innanzitutto chaino la bolla direttamente su TITAS
045200031103     C     KEYtas30P     CHAIN     titas30c
045300031103     C                   IF        %found(titas30c)
045400160628     C                   ELSE
045500160628     C*
045600160628     C* Se non trovato TITAS => bolla ancora solo in partenza (FNBLP)
045700160628     C                   CLEAR                   TITAS000
045800160628     C     KEYtas30P     CHAIN     fnblp01l
045900160628     C                   IF        %found(fnblp01l)
046000160628     C*
046100160628     C* Overrido i campi di TITAS con quelli del FNBLP
046200160628     C                   EVAL      tasLNP = blpLNP
046300160628     C                   EVAL      tasLNA = blpLNA
046400160628     C                   EVAL      tasRSD = blpRSD
046500160628     C                   EVAL      tasLOD = blpLOD
046600160628     C                   EVAL      tasIND = blpIND
046700160628     C                   EVAL      tasPRD = blpPRD
046800160628     C                   EVAL      tasCAD = blpCAD
046900160628     C                   EVAL      tasPKB = blpPKB
047000160628     C                   EVAL      tasPKC = blpPKC
047100160628     C                   EVAL      tasNCL = blpNCL
047200160628     C                   ELSE
047300160628     C*
047400160628     C* Se non trovato neanche in BLP allora => bolla non trovata !!
047500160628     C                   SETOFF                                       22
047600160628     C                   ENDIF
047700160628     C                   ENDIF
047800160628     C*
047900160628     C* Se bolla tutto OK
048000160628     C                   IF        *in22
048100170315     C*
048200170315     C* Aggancio anche la bolla in arrivo per reperire Data Arrivo Collo
048300170315     C                   Z-ADD     *zeros        wDatArrCol        8 0
048400170315     C     KEYtas30P     CHAIN     fnarb01l
048500170315     C                   IF        %found(fnarb01l)
048600170315     C                   EVAL      wDatArrCol = arbDAM
048700170315     C                   ELSE
048800170315     C                   EVAL      wDatArrCol = tasDTI
048900170315     C                   ENDIF
049000131010     C*
049100131010     C* Innanzitutto chaino la bolla direttamente su TITAS
049200131216     C                   MOVEL     *zeros        wPO               3 0
049300131213     C*
049400131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
049500170315     C                   if        wDatArrCol = *zeros
049600131213     C                   eval      wPO = tasLNP
049700131213     C                   else
049800131213     C                   eval      wPO = tasLNA
049900131213     C                   endif
050000131213     C*
050100131213     C     wPO           CHAIN     azorg01l
050200131010     C                   IF        %found(azorg01l)
050300170207     C*
050400170207     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
050500170207     C                   Z-ADD     1             j
050600170207     C                   MOVEL(P)  stbCOS        wCOS_15          15
050700170207     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
050800170207     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
050900170207     C                   EVAL      wWrkSts = skCAU_EVT(j)
051000170207     C                   EVAL      wWrkRsn = skCAU_RSN(j)
051100170207     C                   EVAL      wWrkOpz = skCAU_OPZ(j)
051200170207     C                   EVAL      wWrkDes = skCAU_DES(j)
051300170207     C                   SETOFF                                       50
051400170207     C                   ELSE
051500170207     C                   CLEAR                   wWrkSts
051600170207     C                   CLEAR                   wWrkRsn
051700170207     C                   CLEAR                   wWrkOpz
051800170207     C                   CLEAR                   wWrkDes
051900170207     C                   SETON                                        50
052000170207     C                   ENDIF
052100170207     C  N55              EXSR      sndAlert
052200140308     C*
052300140308     C* Al primo dettaglio da inviare => scrivo anche testata
052400140313     C                   IF        not *IN50
052500140313     C  N45              EXSR      wriHeader
052600140308     C                   SETON                                        45
052700140313     C                   ENDIF
052800160304     C*
052900160304     C* Se ok per procedere
053000160304     C                   IF        not *IN50
053100160304     C*
053200160304     C* Inizializzo le schiere di work
053300160304     C                   CLEAR                   skDETT_RIF
053400160304     C                   CLEAR                   skDETT_DAT
053500160304     C                   CLEAR                   skDETT_ORA
053600160304     C*
053700160304     C* Verifico il tipo di opzione prevista per il codice stato corrente
053800170207     C                   SELECT
053900170207     C                   WHEN      wWrkOpz = '*DETT'
054000170207     C                   EXSR      RTVDETT
054100170207     C                   ENDSL
054200160304     C*
054300160304     C* Scarico il buffer d output
054400160304     C                   EXSR      wriStato
054500160304     C                   ENDIF
054600110103     C*
054700131010     C                   ENDIF
054800160628 ==> C                   ENDIF                                                  * TITAS30C
054900160628     C*
055000110103     C                   ENDIF
055100110103     C                   ENDIF
055200110103     C                   ENDIF
055300031103     C*
055400031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
055500031103     C                   EVAL      stbFTR = 'T'
055600031103     C                   UPDATE    tistb000
055700991027     C*
055800110103     C     KEYstb03P     READE     tistb03l
055900031103     C                   SETOFF                                       50
056000030325     C                   ENDDO
056100151005     C*
056200151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
056300151005     C  N50              EXSR      DeAccorpa
056400170207     C*
056500170207     C* Se raggiunto MAX numero stati bolla per stesso file => esco ed inserisco record "trigger"
056600170207     C                   IF        Conta_Stati >= Tot_Stati
056700170207     C                   CLEAR                   TIS7VASDS
056800170207     C                   EVAL      i§7VASOPZ = 'TRG'
056900170207     C                   EVAL      i§7VASTIP = 'VC'
057000170207     C                   EVAL      i§7VASKSU = '0'+%editc(w§PPIKSU:'X')
057100170207     C                   EVAL      i§7VASTSC = 'WW'
057200170207     C                   EVAL      i§7VASPGM = procname
057300170207     C                   CALL      'TIS7VASR1'
057400170207     C                   PARM                    TIS7VASDS
057500170207     C                   ENDIF
057600170207     C*
057700031103     C                   ENDIF
057800991027     C*
057900030325     C                   EVAL      wrkesito = '0'
058000991027     C*
058100910830     C                   ENDSR
058200031103
058300031103
058400031103
058500131004     C     wriStato      BEGSR
058600131004     C*
058700131004     C* Incremento il contatore di messaggio (al interno della transazione)
058800131004     C                   eval      Conta_msg = Conta_msg + 1
058900131004     C*
059000131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
059100131104     C                   movel     *blanks       wAlfaRMA        512
059200131024     C                   movel     *blanks       wAlfaRSD        512
059300131004     C                   movel     *blanks       wAlfaLOD        512
059400131004     C                   movel     *blanks       wAlfaIND        512
059500131025     C                   movel     *blanks       wAlfaORGDES     512
059600131004     C* RMA
059700131004     C                   eval      campo_alfa = stbRMA
059800131004     C                   exsr      Char_Speciali
059900131004     C                   eval      wAlfaRMA = campo_alfa
060000131024     C* RSD
060100131024     C                   eval      campo_alfa = tasRSD
060200131024     C                   exsr      Char_Speciali
060300131024     C                   eval      wAlfaRSD = campo_alfa
060400131004     C* LOD
060500131004     C                   eval      campo_alfa = tasLOD
060600131004     C                   exsr      Char_Speciali
060700131004     C                   eval      wAlfaLOD = campo_alfa
060800131004     C* IND
060900131004     C                   eval      campo_alfa = tasIND
061000131004     C                   exsr      Char_Speciali
061100131004     C                   eval      wAlfaIND = campo_alfa
061200131025     C* ORGDES
061300131025     C                   eval      campo_alfa = orgDES
061400131025     C                   exsr      Char_Speciali
061500131025     C                   eval      wAlfaORGDES = campo_alfa
061600131004     C*
061700131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
061800131004     C* (ancora corretto ???)
061900131004     C                   z-add     *zeros        wPeso             7 1
062000131004     C                   if        tasPKB > tasPKC
062100131004     C                   eval      wPeso = tasPKB
062200131004     C                   else
062300131004     C                   eval      wPeso = tasPKC
062400131004     C                   endif
062500160304     C*
062600160304     C* Valorizzo la data e ora dello stato corrente
062700160304     C                   eval      wDataStato = stbDAS
062800160304     C                   eval      wOraStato  = stbORS*100
062900160304     C*
063000160304     C* Verifico se per lo stato corrrente sono presentei info anche a livello di collo
063100160304     C                   MOVEL(p)  stbRMN        wAlfaRMN         15
063200160304     C                   Z-ADD     1             j
063300160304     C     wAlfaRMN      LOOKUP    skDETT_RIF(j)                          55
063400160304     C                   IF        %found
063500160304     C                   eval      wDataStato =  skDETT_DAT(j)
063600160304     C                   eval      wOraStato  =  skDETT_ORA(j)
063700160304     C                   ENDIF
063800160304     C*
063900131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
064000131004     C*
064100131004     C*  UNH -
064200131004     C                   eval      Conta_segm  = Conta_segm  + 1
064300131007     C                   eval      vawDTA = UNH_sgm_1
064400131007     C                              + %trim(%editc(Conta_msg:'4'))
064500131007     C                              + UNH_sgm_2
064600131004     C                              +   apice
064700131004     C                   exsr      wriOUTPUT
064800150814     C                   eval      vawDTA_Acc = vawDTA
064900150814     C                   exsr      CpyAccorpa
065000131004     C*
065100131004     C*  BGM -
065200131004     C                   eval      Conta_segm  = Conta_segm  + 1
065300131004     C                   eval      vawDTA = BGM_sgm
065400131004     C                              + %trim(%editc(stbRMN:'4'))
065500131004     C                              +   apice
065600131004     C                   exsr      wriOUTPUT
065700150814     C                   eval      vawDTA_Acc = vawDTA
065800150814     C                   exsr      CpyAccorpa
065900131004     C*
066000131004     C*  NAD SF -
066100131008     C***                eval      Conta_segm  = Conta_segm  + 1
066200131008     C***                eval      vawDTA = NAD_SF_1
066300131008     C***                           + NAD_SF_2
066400131008     C***                           + NAD_SF_3
066500131008     C***                           + NAD_SF_4
066600131008     C***                           + NAD_SF_5
066700131008     C***                           +   apice
066800131008     C***                exsr      wriOUTPUT
066900150814     C***                eval      vawDTA_Acc = vawDTA
067000150814     C***                exsr      CpyAccorpa
067100131004     C*
067200131004     C*  NAD ST -
067300131004     C                   eval      Conta_segm  = Conta_segm  + 1
067400131004     C                   eval      vawDTA = NAD_ST
067500131025     C                              + %trim(wAlfaRSD)
067600131004     C                              +   piu
067700131004     C                              +   piu
067800131004     C                              + %trim(wAlfaIND)
067900131004     C                              +   piu
068000131004     C                              + %trim(wAlfaLOD)
068100131004     C                              +   piu
068200131004     C                              + %trim(tasPRD)
068300131004     C                              +   piu
068400131004     C                              + %trim(tasCAD)
068500131004     C                              +   piu
068600131004     C                              + 'IT'
068700131004     C                              +   apice
068800131004     C                   exsr      wriOUTPUT
068900150814     C                   eval      vawDTA_Acc = vawDTA
069000150814     C                   exsr      CpyAccorpa
069100131004     C*
069200131010     C*  LOC -
069300131004     C                   eval      Conta_segm  = Conta_segm  + 1
069400131004     C                   eval      vawDTA = LOC_sgm1
069500131010     C                              + %trim(%editc(orgFIL:'X'))
069600131004     C                              +   LOC_sgm2
069700131025     C                              + %trim(wAlfaORGDES)
069800131010     C                              +   LOC_sgm3
069900131004     C                              +   apice
070000131004     C                   exsr      wriOUTPUT
070100150814     C                   eval      vawDTA_Acc = vawDTA
070200150814     C                   exsr      CpyAccorpa
070300131004     C*
070400131004     C*  CNI -
070500131004     C                   eval      Conta_segm  = Conta_segm  + 1
070600131004     C                   eval      vawDTA = CNI_sgm
070700131004     C                              +   apice
070800131004     C                   exsr      wriOUTPUT
070900150814     C                   eval      vawDTA_Acc = vawDTA
071000150814     C                   exsr      CpyAccorpa
071100131004     C*
071200131004     C*  STS -
071300131004     C                   eval      Conta_segm  = Conta_segm  + 1
071400131004     C                   eval      vawDTA = STS_sgm
071500170207     C                              + %trim(wWrkSts)
071600170207     C                              +   piu
071700170207     C                              + %trim(wWrkRsn)
071800170207     C                              +   apice
071900131004     C                   exsr      wriOUTPUT
072000150814     C                   eval      vawDTA_Acc = vawDTA
072100150814     C                   exsr      CpyAccorpa
072200131004     C*
072300131004     C*  RFF -
072400131004     C                   eval      Conta_segm  = Conta_segm  + 1
072500131004     C                   eval      vawDTA = RFF_sgm
072600131004     C                              + %trim(wAlfaRMA)
072700131004     C                              +   apice
072800131004     C                   exsr      wriOUTPUT
072900150814     C                   eval      vawDTA_Acc = vawDTA
073000150814     C                   exsr      CpyAccorpa
073100131004     C*
073200131004     C*  TDT -
073300131004     C                   eval      Conta_segm  = Conta_segm  + 1
073400131004     C                   eval      vawDTA = TDT_sgm
073500131004     C                              +   apice
073600131004     C                   exsr      wriOUTPUT
073700150814     C                   eval      vawDTA_Acc = vawDTA
073800150814     C                   exsr      CpyAccorpa
073900131004     C*
074000131004     C*  DTM -
074100131004     C                   eval      Conta_segm  = Conta_segm  + 1
074200131004     C                   eval      vawDTA = DTM_sgm
074300160304     C                              + %trim(%editc(wDataStato*1000000+
074400160304     C                                             wOraStato:'4'))
074500131004     C                              +   diviso
074600131007     C                              + '204'
074700131004     C                              +   apice
074800131004     C                   exsr      wriOUTPUT
074900150814     C                   eval      vawDTA_Acc = vawDTA
075000150814     C                   exsr      CpyAccorpa
075100131004     C*
075200131004     C*  EQD -
075300131004     C                   eval      Conta_segm  = Conta_segm  + 1
075400131004     C                   eval      vawDTA = EQD_sgm
075500131004     C                              +   apice
075600131004     C                   exsr      wriOUTPUT
075700150814     C                   eval      vawDTA_Acc = vawDTA
075800150814     C                   exsr      CpyAccorpa
075900131004     C*
076000131004     C*  MEA -
076100131004     C                   eval      Conta_segm  = Conta_segm  + 1
076200131004     C                   eval      vawDTA = MEA_sgm
076300131004     C                              + %trim(%editc(wPeso:'3'))
076400131004     C                              + '0'
076500131004     C                              +   apice
076600131004     C                   exsr      wriOUTPUT
076700150814     C                   eval      vawDTA_Acc = vawDTA
076800150814     C                   exsr      CpyAccorpa
076900160215     C*
077000160215     C*  GID -
077100160215     C                   eval      Conta_segm  = Conta_segm  + 1
077200160215     C                   eval      vawDTA = GID_sgm1
077300160215     C                              + %trim(%editc(tasNCL:'4'))
077400160215     C                              +   GID_sgm2
077500160215     C                              +   apice
077600160215     C                   exsr      wriOUTPUT
077700150814     C                   eval      vawDTA_Acc = vawDTA
077800150814     C                   exsr      CpyAccorpa
077900131004     C*
078000131004     C*  UNT -
078100131004     C                   eval      Conta_segm  = Conta_segm  + 1
078200131004     C                   eval      vawDTA = UNT_sgm
078300131004     C                              + %trim(%editc(Conta_segm:'4'))
078400131004     C                              +   piu
078500131007     C                              + %trim(%editc(Conta_msg:'4'))
078600131004     C                              +   apice
078700131004     C                   exsr      wriOUTPUT
078800150814     C                   eval      vawDTA_Acc = vawDTA
078900150814     C                   exsr      CpyAccorpa
079000031103     C*
079100031103     C                   ENDSR
079200150814
079300150814
079400150814
079500150814     C     CpyAccorpa    BEGSR
079600150814     C*
079700150814     C                   ADD       1             jCpyAcc
079800150814     C                   EVAL      skCpyAccorpa(jCpyAcc) = vawDTA_Acc
079900150814     C*
080000150814     C                   ENDSR
080100150814
080200150814
080300150814
080400170207     C     DeAccorpa     BEGSR
080500150814     C*
080600150814     C* Scorro tutte le spedizioni accorpate sulla spedizone corrente
080700150814     C     KEYrab01P     SETLL     firab01l
080800150814     C                   IF        %equal(firab01l)
080900150814     C     KEYrab01P     READE     firab01l
081000150814     C                   DOW       not %eof(firab01l)
081100150814     C*
081200150814     C* Controllo (ridondante) su riferimenti spedizione accorpante
081300160107     C                   IF        wDepRMN = rabRMN AND
081400160107     C                             wDepRMA = rabRMA
081500150814     C*
081600150814     C* Considero solamente spedizioni ACCORPATE
081700150814     C                   IF        rabRMN = rabRMNR AND
081800150814     C                             rabRMA = rabRMAR
081900150814     C                   ELSE
082000150814     C*
082100150814     C* Per ogni spedizione ACCORPATA "duplico" la spedizione ACCORPANTE
082200150814     C                   Z-ADD     1             jCpyAcc
082300150814     C                   DOW       jCpyAcc <= %elem(skCpyAccorpa) AND
082400150814     C                             skCpyAccorpa(jCpyAcc) <> *blanks
082500150814     C*
082600150814     C* Incremento il contatore dei segmenti
082700150814     C                   eval      Conta_segm  = Conta_segm  + 1
082800150814     C*
082900150814     C* Effettuo sostuituzione di valori rispetto bolla ACCORPANTE e sue ACCORPATE
083000151005     C                   SELECT
083100150814     C*
083200150814     C* UNH (per contatori segmento / messaggio)
083300151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNH_sgm_1
083400150814     C                             )) = UNH_sgm_1
083500150814     C                   eval      Conta_msg = Conta_msg + 1
083600150814     C                   z-add     1             Conta_segm
083700150814     C                   eval      vawDTA = UNH_sgm_1
083800150814     C                              + %trim(%editc(Conta_msg:'4'))
083900150814     C                              + UNH_sgm_2
084000150814     C                              +   apice
084100150814     C*
084200150814     C* BGM (per RMN)
084300151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(BGM_sgm))
084400150814     C                             = BGM_sgm
084500150814     C                   eval      vawDTA = BGM_sgm
084600150814     C                              + %trim(%editc(rabRMNR:'4'))
084700150814     C                              +   apice
084800150814     C*
084900150814     C* RFF (per RMA)
085000151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(RFF_sgm))
085100150814     C                             = RFF_sgm
085200150814     C                   eval      campo_alfa = rabRMAR
085300150814     C                   exsr      Char_Speciali
085400150814     C                   eval      wAlfaRMA = campo_alfa
085500150814     C                   eval      vawDTA = RFF_sgm
085600150814     C                              + %trim(wAlfaRMA)
085700150814     C                              +   apice
085800150814     C*
085900150814     C* UNT (per contatori segmento / messaggio)
086000151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNT_sgm))
086100150814     C                             = UNT_sgm
086200150814     C                   eval      vawDTA = UNT_sgm
086300150814     C                              + %trim(%editc(Conta_segm:'4'))
086400150814     C                              +   piu
086500150814     C                              + %trim(%editc(Conta_msg:'4'))
086600150814     C                              +   apice
086700151005     C                   OTHER
086800151005     C*
086900151005     C* Tutti gli altri segmenti "AS IS"
087000151005     C                   eval      vawDTA = skCpyAccorpa(jCpyAcc)
087100151005     C                   ENDSL
087200150814     C*
087300150814     C* Scarico l'ouput di "duplicazione"
087400170207 xxx C                   exsr      wriOUTPUT
087500150814     C*
087600150814     C                   ADD       1             jCpyAcc
087700150814     C                   ENDDO
087800150814     C*
087900150814     C                   ENDIF
088000150814     C                   ENDIF
088100150814     C*
088200150814     C     KEYrab01P     READE     firab01l
088300150814     C                   ENDDO
088400150814     C                   ENDIF
088500150814     C*
088600150814     C                   ENDSR
088700160304
088800160304
088900160304
089000160304     C     RTVDETT       BEGSR
089100160304     C*
089200160304     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
089300160304     C                   Z-ADD     *zeros        jDETT
089400160304     C     KEYstbd3P     SETLL     tistbd3i
089500160304     C                   IF        %equal(tistbd3i)
089600160304     C     KEYstbd3P     READE     tistbd3i
089700160304     C                   DOW       not %eof(tistbd3i)
089800160304     C* Filtro unicamente i segnacolli "BRT"
089900160304     C                   IF        stbdTIS = 'BRT'
090000160304     C                   ADD       1             jDETT
090100160304     C                   EVAL      DSTBDNPG5 = STBDDTA
090200160304     C                   EVAL      skDETT_RIF(jDETT) = §STBDNPG51
090300160304     C                   EVAL      skDETT_DAT(jDETT) = stbdDAD
090400160304     C                   EVAL      skDETT_ORA(jDETT) = stbdORD
090500160304     C                   ENDIF
090600160304     C     KEYstbd3P     READE     tistbd3i
090700160304     C                   ENDDO
090800160304     C                   ENDIF
090900160304     C*
091000160304     C                   ENDSR
091100170207
091200170207
091300170207
091400170207     C*--------------------------------------------------------------------------------------------*
091500170207     C* /TITLE Compongo il testo e spedisco una e-mail di alert
091600170207     C*--------------------------------------------------------------------------------------------*
091700170207     C     sndAlert      BEGSR
091800170207     C*
091900170207     C* Inizializzo variabili
092000170207     C                   movel     *blanks       wrkEml          253
092100170207     C                   movel     *blanks       wrkCcEml        253
092200170207     C                   movel     *blanks       wrkEmlMsg      5000
092300170207     C                   movel     *blanks       wrkEmlOgg        44
092400170207     C*
092500170207     C* Valorizzo i campi della e-m@ail
092600170207     C                   eval      wrkEml='cedalert@brt.it'
092700170207     C                   eval      wrkEmlOgg=
092800170207     C                               'BARTVAS - Problema Mappatura TIVMS00F'
092900170207     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
093000170207     C                                                                    ':/N'+
093100170207     C                                                                    ':/N'+
093200170207     C                             'Verificare la mappatura: '           +':/N'+
093300170207     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
093400170207     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
093500170207     C                                                                    ':/N'+
093600170207     C                             'Causale mancante: ' + stbCOS         +':/N'+
093700170207     C                             ' '                                   +':/N'
093800170207     C*
093900170207     C                   call(e)   'TIS701C1'
094000170207     C                   parm                    wrkEml
094100170207     C                   parm                    wrkCcEml
094200170207     C                   parm                    wrkEmlOgg
094300170207     C                   parm                    wrkEmlMsg
094400170207     C                   parm                    wrkEsito          1
094500170207     C*
094600170207     C                   ENDSR
094700031103
094800031103
094900991027
095000991027      /TITLE Operazioni iniziali.
095100991027     C     *inzsr        BEGSR
095200991027     C*
095300991027     C     *ENTRY        PLIST
095400031103     C                   parm                    prmppi
095500991027     C     wrkesito      parm      wrkesito      prmesito
095600031103     C*
095700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
095800031103     C                   EVAL      DSPPI = prmppi
095900131004     C*
096000131004     C* Reperisco i valori variabili ma a livello di transazione corrente
096100131004     C* (tipicamente data e ora relativ al file generato)
096200131004     C                   eval      currDate6    =
096300131004     C                              UBFMTDATE_Convert(
096400131004     C                                  %editc(%dec(%date():*ISO):'X') :
096500131004     C                                  'YYYYMMDD' : 'YYMMDD')
096600131004     C                   eval      currDate6inv =
096700131004     C                              UBFMTDATE_Convert(
096800131004     C                                  %editc(%dec(%date():*ISO):'X') :
096900131004     C                                  'YYYYMMDD' : 'DDMMYY')
097000131004     C                   eval      currHHMM =
097100131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
097200170207     C*
097300170207     C* CALCOLA LA DATA CORRENTE
097400170207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
097500170207     C                   eval      datcor = %dec(%date() : *ISO)
097600030709     C*
097700030709     C* Definizione chiavi
097800030924     C*
097900110103     C* Chiave su TISTB03L - Parziale
098000110103     C     KEYstb03P     KLIST
098100031104     C                   KFLD                    wKstbKSU
098200031103     C                   KFLD                    wKstbFTR
098300160304     C*
098400160304     C* Chiave su TISTBD3I - Parziale
098500160304     C     KEYstbd3P     KLIST
098600160304     C                   KFLD                    wKstbFTR
098700160304     C                   KFLD                    stbKSU
098800160304     C                   KFLD                    stbAAS
098900160304     C                   KFLD                    stbLNP
099000160304     C                   KFLD                    stbNRS
099100160304     C                   KFLD                    stbNSP
099200160304     C                   KFLD                    stbDAS
099300160304     C                   KFLD                    stbORS
099400160304     C                   KFLD                    stbCOS
099500031103     C*
099600031103     C* Chiave su TITAS30C - Parziale
099700031103     C     KEYtas30P     KLIST
099800031103     C                   KFLD                    stbAAS
099900031103     C                   KFLD                    stbLNP
100000031103     C                   KFLD                    stbNRS
100100031103     C                   KFLD                    stbNSP
100200150814     C*
100300150814     C* Chiave su FIRAB01L - Parziale
100400150814     C     KEYrab01P     KLIST
100500150814     C                   KFLD                    wDepAAS
100600150814     C                   KFLD                    wDepLNP
100700150814     C                   KFLD                    wDepNRS
100800150814     C                   KFLD                    wDepNSP
100900170207     C*
101000170207     C* Chiave su TIVMS01I - Parziale
101100170207     C     KEYvms01P     KLIST
101200170207     C                   KFLD                    vmsPROC
101300170207     C                   KFLD                    vmsKSU
101400170207     C                   KFLD                    vmsDDE
101500991027     C*
101600991027     C                   ENDSR
101700170207
