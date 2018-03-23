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
001400170207
001500170207
001600170207     D*-----------
001700170207     D* SCHIERE
001800170207     D*-----------
001900170207     D NumElem         C                   CONST(300)
002000170207     D skCAU_BRT       S                   DIM(NumElem) LIKE(vmsCAUBRT)
002100170207     D skCAU_EVT       S                   DIM(NumElem) LIKE(dsCLI_EVT)
002200170207     D skCAU_RSN       S                   DIM(NumElem) LIKE(dsCLI_RSN)
002300170207     D skCAU_OPZ       S                   DIM(NumElem) LIKE(vmsSTSOPZ)
002400170207     D skCAU_DES       S                   DIM(NumElem) LIKE(vmsDESCRIZ)
002500170207     D jCAU            S              3  0 INZ(*zeros)
002600170207     D j               S              3  0 INZ(*zeros)
002700170207     D*-----------
002800170207     D* DS RIDEFINIZIONE STATUS CLIENTE
002900170207     D*-----------
003000170207     D DS_VMSSTSCLI    DS                  INZ
003100170207     D  dsCLI_EVT                    10
003200170207     D  dsCLI_RSN                     5
003300170207     D*-----------
003400170207     D* COSTANTI SPECIFICHE DEL TRADUTTORE
003500170207     D*-----------
003600170207     D  w§PPIVMSPROC   S                   LIKE(vmsPROC) INZ('TITVED10R1')
003700170207     D  w§PPIVMSKSU    S                   LIKE(vmsKSU)  INZ
003800170207     D*-----------
003900170207     D* VARIABILI SPECIFICHE MAPPATURA STATI
004000170207     D*-----------
004100170207     D wWrkSts         S                   like(dsCLI_EVT)  inz
004200170207     D wWrkRsn         S                   like(dsCLI_RSN)  inz
004300170207     D wWrkOpz         S                   like(vmsSTSOPZ)  inz
004400170207     D wWrkDes         S                   like(vmsDESCRIZ) inz
004500170207     D*-----------
004600170207
004700170207
004800170207     D*-----------
004900170207     D* SCHIERE DETTAGLI TISTB
005000170207     D*-----------
005100170207     D NumDETT         C                   CONST(300)
005200170207     D skDETT_RIF      S             15    DIM(NumDETT)
005300170207     D skDETT_DAT      S              8  0 DIM(NumDETT)
005400170207     D skDETT_ORA      S              6  0 DIM(NumDETT)
005500170207     D jDETT           S              3  0 INZ(*zeros)
005600131104
005700131104
005800041015     D*-----------
005900170207     D* SCHIERE PER DE-ACCORPAMENTO
006000041015     D*-----------
006100150814     D skCpyAccorpa    S                   LIKE(vawDTA) DIM(5000) INZ
006200150814     D jCpyAcc         S              5  0 INZ(*zeros)
006300150814
006400030924
006500041015     D*-----------
006600041015     D* VARIABILI D WRK
006700041015     D*-----------
006800170207     D psds           sds
006900170207     D  procname         *PROC
007000031103     D prmppi          S             50
007100030924     D prmesito        S              1
007200030924     D wrkesito        S                   like(prmesito)
007300170207     D campo_alfa      S             70    varying
007400160304     D wDataStato      S              8  0 inz
007500160304     D wOraStato       S              6  0 inz
007600131004     D currDate6       S              6
007700131004     D currDate6inv    S              6
007800131004     D currHHMM        S              4
007900131216     D Conta_msg       S             10i 0
008000131216     D Conta_segm      S             10i 0
008100170207     D Conta_Stati     S             10i 0 inz
008200170207     D Tot_Stati       S             10i 0 inz(60000)
008300170207     D wDepKSU         S                   like(stbKSU) inz
008400150814     D wDepAAS         S                   like(stbAAS) inz
008500150814     D wDepLNP         S                   like(stbLNP) inz
008600150814     D wDepNRS         S                   like(stbNRS) inz
008700150814     D wDepNSP         S                   like(stbNSP) inz
008800160107     D wDepRMN         S                   like(stbRMN) inz
008900160107     D wDepRMA         S                   like(stbRMA) inz
009000150814     D vawDTA_Acc      S                   like(vawDTA) inz
009100160628     D LibFileBLP01    s             21A   inz
009200160628     D currSysNeta     s              8A   inz('*NULL')
009300131004
009400131004
009500131004     D*-----------
009600131004     D* DEFINIZIONE DS ESTERNE
009700131004     D*-----------
009800170207     D TRUL33DS      E DS                  inz
009900170207     D KPJBA         E DS                  inz
010000170207     D DSTBDNPG5     E DS                  inz
010100170207     D TIS7VASDS     E DS                  inz
010200031103
010300131004
010400131004     D*------------------
010500131004     D* LINKING A DEFINIZIONI ESTERNE
010600131004     D*------------------
010700131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
010800131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
010900160628     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011000160628     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
011100170207     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
011200131004
011300131004
011400031103     D*-----------
011500031103     D* VARIABILI RIFERITE AL DATA-BASE
011600031103     D*-----------
011700080618     D wKstbKSU        S                   like(stbKSU)
011800080618     D wKstbFTR        S                   like(stbFTR)
011900110705
012000031103
012100031103     D*-----------
012200031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
012300031103     D*-----------
012400031103     D DSPPI           DS
012500080618     D  w§PPIKSU                      7  0
012600131004     D  w§UNB_ID                     25
012700131004
012800131004
012900131004     D*-----------
013000131004     D* COSTANTI / DEFAULT
013100131004     D*-----------
013200131004     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+')
013300131007     D UNH_sgm_1       C                   CONST('UNH+')
013400131007     D UNH_sgm_2       C                   CONST('+IFTSTA:D:01A:UN:EAN008')
013500131004     D BGM_sgm         C                   CONST('BGM+23+')
013600131004     D NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
013700131004     D NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
013800131004     D NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
013900131004     D NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
014000131004     D NAD_SF_5        C                   CONST('NNI++29015+IT')
014100131004     D NAD_ST          C                   CONST('NAD+ST++')
014200131010     D LOC_sgm1        C                   CONST('LOC+175+')
014300131010     D LOC_sgm2        C                   CONST('::3:')
014400131010     D LOC_sgm3        C                   CONST('+IT')
014500131004     D CNI_sgm         C                   CONST('CNI+1')
014600170207     D STS_sgm         C                   CONST('STS+1+')
014700131004     D RFF_sgm         C                   CONST('RFF+POR:')
014800131004     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX')
014900131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
015000131004     D EQD_sgm         C                   CONST('EQD+AA')
015100131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
015200160215     D GID_sgm1        C                   CONST('GID+1+')
015300160215     D GID_sgm2        C                   CONST(':CTN')
015400131004     D UNT_sgm         C                   CONST('UNT+')
015500131004     D UNZ_sgm         C                   CONST('UNZ+')
015600131004     D piu             C                   CONST('+')
015700131004     D diviso          C                   CONST(':')
015800131004     D apice           C                   CONST('''')
015900131004
016000030924
016100030325
016200030924     C* MAIN/
016300160628     C*
016400160628     C* Reperisco il sistema AS/400 corrente
016500160628     C                   callp     UBRTVNETA_Rtv(currSysNeta)
016600160628     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
016700160628     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
016800160628     C                   else
016900160628     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
017000160628     C                   endif
017100160628     C*
017200160628     C* Apertura file "overraidati"
017300160628     C                   if        not %open(FNBLP01L)
017400160628     C                   open      FNBLP01L
017500160628     C                   endif
017600170207     C*
017700170207     C* Carica schiere di mappatura
017800170207     C                   SETOFF                                       40
017900170207     C                   EXSR      carMAP
018000170207     C*
018100170207     C* Se caricamento mappatura tutto OK => Procedo
018200170207     C                   IF        not *in40
018300140308     C*
018400140308     C* Spia dati da inviare
018500140308     C                   SETOFF                                       45
018600041015     C*
018700041015     C* Esegue traduzione
018800140308     C                   SETOFF                                       31
018900131004     C  N31              EXSR      chkRECFILMBR
019000131004     C  N31              EXSR      rtvID
019100131004     C  N31              EXSR      traduci
019200140308     C  N31
019300140308     CAN 45              EXSR      wriFooter
019400170207     C*
019500170207     C* Diversamente traduzioe completamente fallita
019600170207     C                   ELSE
019700170207     C                   EVAL      wrkesito = '2'
019800170207     C                   ENDIF
019900160628     C*
020000160628     C* Chiusura file "overraidati"
020100160628     C                   if        %open(FNBLP01L)
020200160628     C                   close     FNBLP01L
020300160628     C                   endif
020400110705     C*
020500921023     C                   SETON                                        LR
020600131004
020700131004
020800131004
020900131004     C*----------------------------------------------------*
021000131004     C*  STACCO ID UNIVOCO
021100131004     C*----------------------------------------------------*
021200131004     C     RTVID         BEGSR
021300131004     C*
021400131004     C                   z-add     *zeros        currUNIQ_ID      15 0
021500131004     C*
021600131004     C* NSP => Stacco un numeratore da AZNUM
021700131004     C                   clear                   TRUL33DS
021800131004     C                   eval      I33OPE = *zeros
021900131004     C                   eval      I33CNU = 302
022000131004     C                   eval      I33NUM = 1
022100131004     C                   movel     TRUL33DS      KPJBU
022200131004     C                   call      'TRUL33R'
022300131004     C                   parm                    KPJBA
022400131004     C                   movel     KPJBU         TRUL33DS
022500131004     C                   if        O33ERR = *zeros
022600131004     C                   z-add     O33NRF        currUNIQ_ID
022700131004     C                   else
022800131004     C                   seton                                        31
022900131004     C                   endif
023000131004     C*
023100131004     C                   ENDSR
023200131004
023300131004
023400131004
023500131004     C     chkRECFILMBR  BEGSR
023600131004     C*
023700131004     C     *start        setll     TIVAWWWT
023800131004     C                   if        %found(TIVAWWWT)
023900131004     C                   seton                                        31
024000131004     C                   endif
024100131004     C*
024200131004     C                   ENDSR
024300131004
024400131004
024500131004
024600131004     C     wriOUTPUT     BEGSR
024700131004     C*
024800131004     C                   write     TIVAW000
024900170207     C                   eval      Conta_Stati = Conta_Stati + 1
025000131004     C*
025100131004     C                   ENDSR
025200170207
025300170207
025400170207
025500170207     C     carMAP        BEGSR
025600170207     C*
025700170207     C* Reperisco la versione di mappatura stati correntemente valida
025800170207     C                   call      'UBVMSDDER'
025900170207     C                   parm      *blanks       iVMSDDEOpz
026000170207     C                   parm      w§PPIVMSPROC  iVMSDDEProc
026100170207     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
026200170207     C                   parm      datcor        iVMSDDEDtRif
026300170207     C                   parm                    oVMSDDEDDE
026400170207     C                   parm                    oVMSDDEEsito
026500170207     C*
026600170207     C* Verifico se esito no OK
026700170207     C                   if        oVMSDDEEsito <> '1'
026800170207     C                   seton                                        40
026900170207     C                   else
027000170207     C*
027100170207     C                   move(P)   w§PPIVMSPROC  vmsPROC
027200170207     C                   move(P)   w§PPIVMSKSU   vmsKSU
027300170207     C                   z-add     oVMSDDEDDE    vmsDDE
027400170207     C     KEYvms01P     setll     tivms01i
027500170207     C                   if        %equal(tivms01i)
027600170207     C     KEYvms01P     reade     tivms01i
027700170207     C*
027800170207     C* Inizializzo indice schiera di mapping
027900170207     C                   z-add     1             jCAU
028000170207     C*
028100170207     C* Ciclo x tutti i record della mappatura richiesta
028200170207     C                   dow       not %eof(tivms01i)
028300170207     C*
028400170207     C* Ignoro i record annullati
028500170207     C                   if        vmsATB = *blanks
028600170207     C*
028700170207     C* Carico la mappatura dei codici status personalizzati cliente
028800170207     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
028900170207     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
029000170207     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
029100170207     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
029200170207     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
029300170207     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
029400170207     C                   add       1             jCAU
029500170207     C*
029600170207     C                   endif
029700170207     C*
029800170207     C     KEYvms01P     reade     tivms01i
029900170207     C                   enddo
030000170207     C*
030100170207     C                   endif
030200170207     C*
030300170207     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
030400170207     C* altrimenti errore bloccante per intera traduzione
030500170207     C                   if        jCAU > 1
030600170207     C                   else
030700170207     C                   seton                                        40
030800170207     C                   endif
030900170207     C*
031000170207     C                   endif
031100170207     C*
031200170207     C                   ENDSR
031300131104
031400131004
031500131004
031600131004     C     Char_Speciali BEGSR
031700170207     C*
031800170207     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
031900170207     C                   EVAL      campo_alfa = %scanrpl('?':'??':campo_alfa)
032000170207     C                   EVAL      campo_alfa = %scanrpl('+':'?+':campo_alfa)
032100170207     C                   EVAL      campo_alfa = %scanrpl(':':'?:':campo_alfa)
032200170207     C                   EVAL      campo_alfa = %scanrpl(',':'?,':campo_alfa)
032300170207     C                   EVAL      campo_alfa = %scanrpl('''':'?''':campo_alfa)
032400131004     C*
032500131004     C                   ENDSR
032600131004
032700131004
032800131004
032900131004     C     wriHeader     BEGSR
033000131004     C*
033100131004     C                   clear                   vawDTA
033200131010     C*
033300131004     C*  UNB -
033400131004     C                   eval      vawDTA = UNB_sgm
033500131004     C                              + %trim(w§UNB_ID)
033600131004     C                              +   piu
033700131004     C                              + %trim(currDate6inv)
033800131004     C                              +   diviso
033900131004     C                              + %trim(currHHMM)
034000131004     C                              +   piu
034100131004     C                              + %trim(
034200131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
034300131004     C                              +   apice
034400131004     C                   exsr      wriOUTPUT
034500131004     C*
034600131004     C                   ENDSR
034700131004
034800131004
034900131004
035000131004     C     wriFooter     BEGSR
035100131004     C*
035200131004     C                   clear                   vawDTA
035300131004     C*
035400131004     C*  UNZ -
035500131004     C                   eval      vawDTA = UNZ_sgm
035600131004     C                              + %trim(%editc(Conta_msg:'4'))
035700131004     C                              +   piu
035800131004     C                              + %trim(
035900131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
036000131004     C                              +   apice
036100131004     C                   exsr      wriOUTPUT
036200131004     C*
036300131004     C                   ENDSR
036400131004
036500030924
036600030924
036700991027     C     traduci       BEGSR
036800131004     C*
036900131004     C* Inizializzo i contatori a livello di transazione/messaggio
037000131004     C                   z-add     *zeros        Conta_msg
037100031103     C*
037200031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
037300031103     C* di traduzione (trattasi d "post-traduzione")
037400031104     C                   EVAL      wKstbKSU = w§PPIKSU
037500031103     C                   EVAL      wKstbFTR = *blanks
037600110103     C     KEYstb03P     SETLL     tistb03l
037700110103     C                   IF        %equal(tistb03l)
037800110103     C     KEYstb03P     READE     tistb03l
037900170207     C*
038000170207     C* Ciclo x tutti record del cliente da trasmettere o fino a MAX numero stati per singolo file
038100170207     C                   DOW       not %eof(tistb03l)
038200170207     C                             AND Conta_Stati < Tot_Stati
038300131004     C*
038400131004     C* Inizializzo i contatori a livello di segmento
038500131004     C                   z-add     *zeros        Conta_segm
038600080618     C*
038700130807     C                   SETON                                        50
038800031103     C*
038900031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
039000031103     C* deve restituire al cliente nel file tradotto
039100031103     C                   IF        stbFTR = *blanks                             * ridondante
039200110103     C*
039300110103     C* Gestisco solamente gli stati standard
039400160212     C                   IF        (
039500160212     C                             (stbTIS='1' AND stbPRS<>'7')      OR
039600160212     C                             (stbTIS='1' AND stbPRS ='7'
039700160212     C                                         AND stbCOS ='CONSOK') OR
039800160304     C                             (stbTIS='1' AND stbPRS ='8'
039900160304     C                                         AND stbCOS ='SPUNTNPG5') OR
040000160212     C                             (stbTIS='9' AND stbPRS ='1')
040100160212     C                             ) AND
040200160212     C                             (%subst(stbFOP:1:1)= 'O'  OR
040300160212     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
040400160212     C                             )
040500131004     C*
040600131004     C* Per "sicurezza" non invio stati senza data
040700131004     C                   IF        stbDAS > *zeros
040800150814     C*
040900110103     C                   SETOFF                                       50
041000151005     C*
041100151005     C* Gestisco rottura di codice per spedizione
041200151005     C                   IF        stbKSU = wDepKSU AND
041300151005     C                             stbAAS = wDepAAS AND
041400151005     C                             stbLNP = wDepLNP AND
041500151005     C                             stbNRS = wDepNRS AND
041600151005     C                             stbNSP = wDepNSP
041700151005     C                   ELSE
041800151005     C*
041900151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
042000151005     C  N50              EXSR      DeAccorpa
042100151005     C*
042200151005     C* Salvo la nuova chiave di rottura
042300151005     C                   EVAL      wDepKSU = stbKSU
042400151005     C                   EVAL      wDepAAS = stbAAS
042500151005     C                   EVAL      wDepLNP = stbLNP
042600151005     C                   EVAL      wDepNRS = stbNRS
042700151005     C                   EVAL      wDepNSP = stbNSP
042800160107     C                   EVAL      wDepRMN = stbRMN
042900160107     C                   EVAL      wDepRMA = stbRMA
043000151005     C*
043100151005     C* Inizializzazioni per nuova chiave di rottura
043200151005     C                   CLEAR                   skCpyAccorpa
043300151005     C                   CLEAR                   jCpyAcc
043400151005     C*
043500151005     C                   ENDIF
043600160628     C*
043700160628     C                   SETON                                        22
043800031103     C*
043900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
044000031103     C     KEYtas30P     CHAIN     titas30c
044100031103     C                   IF        %found(titas30c)
044200160628     C                   ELSE
044300160628     C*
044400160628     C* Se non trovato TITAS => bolla ancora solo in partenza (FNBLP)
044500160628     C                   CLEAR                   TITAS000
044600160628     C     KEYtas30P     CHAIN     fnblp01l
044700160628     C                   IF        %found(fnblp01l)
044800160628     C*
044900160628     C* Overrido i campi di TITAS con quelli del FNBLP
045000160628     C                   EVAL      tasLNP = blpLNP
045100160628     C                   EVAL      tasLNA = blpLNA
045200160628     C                   EVAL      tasRSD = blpRSD
045300160628     C                   EVAL      tasLOD = blpLOD
045400160628     C                   EVAL      tasIND = blpIND
045500160628     C                   EVAL      tasPRD = blpPRD
045600160628     C                   EVAL      tasCAD = blpCAD
045700160628     C                   EVAL      tasPKB = blpPKB
045800160628     C                   EVAL      tasPKC = blpPKC
045900160628     C                   EVAL      tasNCL = blpNCL
046000160628     C                   ELSE
046100160628     C*
046200160628     C* Se non trovato neanche in BLP allora => bolla non trovata !!
046300160628     C                   SETOFF                                       22
046400160628     C                   ENDIF
046500160628     C                   ENDIF
046600160628     C*
046700160628     C* Se bolla tutto OK
046800160628     C                   IF        *in22
046900131010     C*
047000131010     C* Innanzitutto chaino la bolla direttamente su TITAS
047100131216     C                   MOVEL     *zeros        wPO               3 0
047200131213     C*
047300131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
047400131213     C                   if        tasDTI = *zeros
047500131213     C                   eval      wPO = tasLNP
047600131213     C                   else
047700131213     C                   eval      wPO = tasLNA
047800131213     C                   endif
047900131213     C*
048000131213     C     wPO           CHAIN     azorg01l
048100131010     C                   IF        %found(azorg01l)
048200170207     C*
048300170207     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
048400170207     C                   Z-ADD     1             j
048500170207     C                   MOVEL(P)  stbCOS        wCOS_15          15
048600170207     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
048700170207     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
048800170207     C                   EVAL      wWrkSts = skCAU_EVT(j)
048900170207     C                   EVAL      wWrkRsn = skCAU_RSN(j)
049000170207     C                   EVAL      wWrkOpz = skCAU_OPZ(j)
049100170207     C                   EVAL      wWrkDes = skCAU_DES(j)
049200170207     C                   SETOFF                                       50
049300170207     C                   ELSE
049400170207     C                   CLEAR                   wWrkSts
049500170207     C                   CLEAR                   wWrkRsn
049600170207     C                   CLEAR                   wWrkOpz
049700170207     C                   CLEAR                   wWrkDes
049800170207     C                   SETON                                        50
049900170207     C                   ENDIF
050000170207     C  N55              EXSR      sndAlert
050100140308     C*
050200140308     C* Al primo dettaglio da inviare => scrivo anche testata
050300140313     C                   IF        not *IN50
050400140313     C  N45              EXSR      wriHeader
050500140308     C                   SETON                                        45
050600140313     C                   ENDIF
050700160304     C*
050800160304     C* Se ok per procedere
050900160304     C                   IF        not *IN50
051000160304     C*
051100160304     C* Inizializzo le schiere di work
051200160304     C                   CLEAR                   skDETT_RIF
051300160304     C                   CLEAR                   skDETT_DAT
051400160304     C                   CLEAR                   skDETT_ORA
051500160304     C*
051600160304     C* Verifico il tipo di opzione prevista per il codice stato corrente
051700170207     C                   SELECT
051800170207     C                   WHEN      wWrkOpz = '*DETT'
051900170207     C                   EXSR      RTVDETT
052000170207     C                   ENDSL
052100160304     C*
052200160304     C* Scarico il buffer d output
052300160304     C                   EXSR      wriStato
052400160304     C                   ENDIF
052500110103     C*
052600131010     C                   ENDIF
052700160628 ==> C                   ENDIF                                                  * TITAS30C
052800160628     C*
052900110103     C                   ENDIF
053000110103     C                   ENDIF
053100110103     C                   ENDIF
053200031103     C*
053300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
053400031103     C                   EVAL      stbFTR = 'T'
053500031103     C                   UPDATE    tistb000
053600991027     C*
053700110103     C     KEYstb03P     READE     tistb03l
053800031103     C                   SETOFF                                       50
053900030325     C                   ENDDO
054000151005     C*
054100151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
054200151005     C  N50              EXSR      DeAccorpa
054300170207     C*
054400170207     C* Se raggiunto MAX numero stati bolla per stesso file => esco ed inserisco record "trigger"
054500170207     C                   IF        Conta_Stati >= Tot_Stati
054600170207     C                   CLEAR                   TIS7VASDS
054700170207     C                   EVAL      i§7VASOPZ = 'TRG'
054800170207     C                   EVAL      i§7VASTIP = 'VC'
054900170207     C                   EVAL      i§7VASKSU = '0'+%editc(w§PPIKSU:'X')
055000170207     C                   EVAL      i§7VASTSC = 'WW'
055100170207     C                   EVAL      i§7VASPGM = procname
055200170207     C                   CALL      'TIS7VASR1'
055300170207     C                   PARM                    TIS7VASDS
055400170207     C                   ENDIF
055500170207     C*
055600031103     C                   ENDIF
055700991027     C*
055800030325     C                   EVAL      wrkesito = '0'
055900991027     C*
056000910830     C                   ENDSR
056100031103
056200031103
056300031103
056400131004     C     wriStato      BEGSR
056500131004     C*
056600131004     C* Incremento il contatore di messaggio (al interno della transazione)
056700131004     C                   eval      Conta_msg = Conta_msg + 1
056800131004     C*
056900131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
057000131104     C                   movel     *blanks       wAlfaRMA        512
057100131024     C                   movel     *blanks       wAlfaRSD        512
057200131004     C                   movel     *blanks       wAlfaLOD        512
057300131004     C                   movel     *blanks       wAlfaIND        512
057400131025     C                   movel     *blanks       wAlfaORGDES     512
057500131004     C* RMA
057600131004     C                   eval      campo_alfa = stbRMA
057700131004     C                   exsr      Char_Speciali
057800131004     C                   eval      wAlfaRMA = campo_alfa
057900131024     C* RSD
058000131024     C                   eval      campo_alfa = tasRSD
058100131024     C                   exsr      Char_Speciali
058200131024     C                   eval      wAlfaRSD = campo_alfa
058300131004     C* LOD
058400131004     C                   eval      campo_alfa = tasLOD
058500131004     C                   exsr      Char_Speciali
058600131004     C                   eval      wAlfaLOD = campo_alfa
058700131004     C* IND
058800131004     C                   eval      campo_alfa = tasIND
058900131004     C                   exsr      Char_Speciali
059000131004     C                   eval      wAlfaIND = campo_alfa
059100131025     C* ORGDES
059200131025     C                   eval      campo_alfa = orgDES
059300131025     C                   exsr      Char_Speciali
059400131025     C                   eval      wAlfaORGDES = campo_alfa
059500131004     C*
059600131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
059700131004     C* (ancora corretto ???)
059800131004     C                   z-add     *zeros        wPeso             7 1
059900131004     C                   if        tasPKB > tasPKC
060000131004     C                   eval      wPeso = tasPKB
060100131004     C                   else
060200131004     C                   eval      wPeso = tasPKC
060300131004     C                   endif
060400160304     C*
060500160304     C* Valorizzo la data e ora dello stato corrente
060600160304     C                   eval      wDataStato = stbDAS
060700160304     C                   eval      wOraStato  = stbORS*100
060800160304     C*
060900160304     C* Verifico se per lo stato corrrente sono presentei info anche a livello di collo
061000160304     C                   MOVEL(p)  stbRMN        wAlfaRMN         15
061100160304     C                   Z-ADD     1             j
061200160304     C     wAlfaRMN      LOOKUP    skDETT_RIF(j)                          55
061300160304     C                   IF        %found
061400160304     C                   eval      wDataStato =  skDETT_DAT(j)
061500160304     C                   eval      wOraStato  =  skDETT_ORA(j)
061600160304     C                   ENDIF
061700160304     C*
061800131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
061900131004     C*
062000131004     C*  UNH -
062100131004     C                   eval      Conta_segm  = Conta_segm  + 1
062200131007     C                   eval      vawDTA = UNH_sgm_1
062300131007     C                              + %trim(%editc(Conta_msg:'4'))
062400131007     C                              + UNH_sgm_2
062500131004     C                              +   apice
062600131004     C                   exsr      wriOUTPUT
062700150814     C                   eval      vawDTA_Acc = vawDTA
062800150814     C                   exsr      CpyAccorpa
062900131004     C*
063000131004     C*  BGM -
063100131004     C                   eval      Conta_segm  = Conta_segm  + 1
063200131004     C                   eval      vawDTA = BGM_sgm
063300131004     C                              + %trim(%editc(stbRMN:'4'))
063400131004     C                              +   apice
063500131004     C                   exsr      wriOUTPUT
063600150814     C                   eval      vawDTA_Acc = vawDTA
063700150814     C                   exsr      CpyAccorpa
063800131004     C*
063900131004     C*  NAD SF -
064000131008     C***                eval      Conta_segm  = Conta_segm  + 1
064100131008     C***                eval      vawDTA = NAD_SF_1
064200131008     C***                           + NAD_SF_2
064300131008     C***                           + NAD_SF_3
064400131008     C***                           + NAD_SF_4
064500131008     C***                           + NAD_SF_5
064600131008     C***                           +   apice
064700131008     C***                exsr      wriOUTPUT
064800150814     C***                eval      vawDTA_Acc = vawDTA
064900150814     C***                exsr      CpyAccorpa
065000131004     C*
065100131004     C*  NAD ST -
065200131004     C                   eval      Conta_segm  = Conta_segm  + 1
065300131004     C                   eval      vawDTA = NAD_ST
065400131025     C                              + %trim(wAlfaRSD)
065500131004     C                              +   piu
065600131004     C                              +   piu
065700131004     C                              + %trim(wAlfaIND)
065800131004     C                              +   piu
065900131004     C                              + %trim(wAlfaLOD)
066000131004     C                              +   piu
066100131004     C                              + %trim(tasPRD)
066200131004     C                              +   piu
066300131004     C                              + %trim(tasCAD)
066400131004     C                              +   piu
066500131004     C                              + 'IT'
066600131004     C                              +   apice
066700131004     C                   exsr      wriOUTPUT
066800150814     C                   eval      vawDTA_Acc = vawDTA
066900150814     C                   exsr      CpyAccorpa
067000131004     C*
067100131010     C*  LOC -
067200131004     C                   eval      Conta_segm  = Conta_segm  + 1
067300131004     C                   eval      vawDTA = LOC_sgm1
067400131010     C                              + %trim(%editc(orgFIL:'X'))
067500131004     C                              +   LOC_sgm2
067600131025     C                              + %trim(wAlfaORGDES)
067700131010     C                              +   LOC_sgm3
067800131004     C                              +   apice
067900131004     C                   exsr      wriOUTPUT
068000150814     C                   eval      vawDTA_Acc = vawDTA
068100150814     C                   exsr      CpyAccorpa
068200131004     C*
068300131004     C*  CNI -
068400131004     C                   eval      Conta_segm  = Conta_segm  + 1
068500131004     C                   eval      vawDTA = CNI_sgm
068600131004     C                              +   apice
068700131004     C                   exsr      wriOUTPUT
068800150814     C                   eval      vawDTA_Acc = vawDTA
068900150814     C                   exsr      CpyAccorpa
069000131004     C*
069100131004     C*  STS -
069200131004     C                   eval      Conta_segm  = Conta_segm  + 1
069300131004     C                   eval      vawDTA = STS_sgm
069400170207     C                              + %trim(wWrkSts)
069500170207     C                              +   piu
069600170207     C                              + %trim(wWrkRsn)
069700170207     C                              +   apice
069800131004     C                   exsr      wriOUTPUT
069900150814     C                   eval      vawDTA_Acc = vawDTA
070000150814     C                   exsr      CpyAccorpa
070100131004     C*
070200131004     C*  RFF -
070300131004     C                   eval      Conta_segm  = Conta_segm  + 1
070400131004     C                   eval      vawDTA = RFF_sgm
070500131004     C                              + %trim(wAlfaRMA)
070600131004     C                              +   apice
070700131004     C                   exsr      wriOUTPUT
070800150814     C                   eval      vawDTA_Acc = vawDTA
070900150814     C                   exsr      CpyAccorpa
071000131004     C*
071100131004     C*  TDT -
071200131004     C                   eval      Conta_segm  = Conta_segm  + 1
071300131004     C                   eval      vawDTA = TDT_sgm
071400131004     C                              +   apice
071500131004     C                   exsr      wriOUTPUT
071600150814     C                   eval      vawDTA_Acc = vawDTA
071700150814     C                   exsr      CpyAccorpa
071800131004     C*
071900131004     C*  DTM -
072000131004     C                   eval      Conta_segm  = Conta_segm  + 1
072100131004     C                   eval      vawDTA = DTM_sgm
072200160304     C                              + %trim(%editc(wDataStato*1000000+
072300160304     C                                             wOraStato:'4'))
072400131004     C                              +   diviso
072500131007     C                              + '204'
072600131004     C                              +   apice
072700131004     C                   exsr      wriOUTPUT
072800150814     C                   eval      vawDTA_Acc = vawDTA
072900150814     C                   exsr      CpyAccorpa
073000131004     C*
073100131004     C*  EQD -
073200131004     C                   eval      Conta_segm  = Conta_segm  + 1
073300131004     C                   eval      vawDTA = EQD_sgm
073400131004     C                              +   apice
073500131004     C                   exsr      wriOUTPUT
073600150814     C                   eval      vawDTA_Acc = vawDTA
073700150814     C                   exsr      CpyAccorpa
073800131004     C*
073900131004     C*  MEA -
074000131004     C                   eval      Conta_segm  = Conta_segm  + 1
074100131004     C                   eval      vawDTA = MEA_sgm
074200131004     C                              + %trim(%editc(wPeso:'3'))
074300131004     C                              + '0'
074400131004     C                              +   apice
074500131004     C                   exsr      wriOUTPUT
074600150814     C                   eval      vawDTA_Acc = vawDTA
074700150814     C                   exsr      CpyAccorpa
074800160215     C*
074900160215     C*  GID -
075000160215     C                   eval      Conta_segm  = Conta_segm  + 1
075100160215     C                   eval      vawDTA = GID_sgm1
075200160215     C                              + %trim(%editc(tasNCL:'4'))
075300160215     C                              +   GID_sgm2
075400160215     C                              +   apice
075500160215     C                   exsr      wriOUTPUT
075600150814     C                   eval      vawDTA_Acc = vawDTA
075700150814     C                   exsr      CpyAccorpa
075800131004     C*
075900131004     C*  UNT -
076000131004     C                   eval      Conta_segm  = Conta_segm  + 1
076100131004     C                   eval      vawDTA = UNT_sgm
076200131004     C                              + %trim(%editc(Conta_segm:'4'))
076300131004     C                              +   piu
076400131007     C                              + %trim(%editc(Conta_msg:'4'))
076500131004     C                              +   apice
076600131004     C                   exsr      wriOUTPUT
076700150814     C                   eval      vawDTA_Acc = vawDTA
076800150814     C                   exsr      CpyAccorpa
076900031103     C*
077000031103     C                   ENDSR
077100150814
077200150814
077300150814
077400150814     C     CpyAccorpa    BEGSR
077500150814     C*
077600150814     C                   ADD       1             jCpyAcc
077700150814     C                   EVAL      skCpyAccorpa(jCpyAcc) = vawDTA_Acc
077800150814     C*
077900150814     C                   ENDSR
078000150814
078100150814
078200150814
078300170207     C     DeAccorpa     BEGSR
078400150814     C*
078500150814     C* Scorro tutte le spedizioni accorpate sulla spedizone corrente
078600150814     C     KEYrab01P     SETLL     firab01l
078700150814     C                   IF        %equal(firab01l)
078800150814     C     KEYrab01P     READE     firab01l
078900150814     C                   DOW       not %eof(firab01l)
079000150814     C*
079100150814     C* Controllo (ridondante) su riferimenti spedizione accorpante
079200160107     C                   IF        wDepRMN = rabRMN AND
079300160107     C                             wDepRMA = rabRMA
079400150814     C*
079500150814     C* Considero solamente spedizioni ACCORPATE
079600150814     C                   IF        rabRMN = rabRMNR AND
079700150814     C                             rabRMA = rabRMAR
079800150814     C                   ELSE
079900150814     C*
080000150814     C* Per ogni spedizione ACCORPATA "duplico" la spedizione ACCORPANTE
080100150814     C                   Z-ADD     1             jCpyAcc
080200150814     C                   DOW       jCpyAcc <= %elem(skCpyAccorpa) AND
080300150814     C                             skCpyAccorpa(jCpyAcc) <> *blanks
080400150814     C*
080500150814     C* Incremento il contatore dei segmenti
080600150814     C                   eval      Conta_segm  = Conta_segm  + 1
080700150814     C*
080800150814     C* Effettuo sostuituzione di valori rispetto bolla ACCORPANTE e sue ACCORPATE
080900151005     C                   SELECT
081000150814     C*
081100150814     C* UNH (per contatori segmento / messaggio)
081200151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNH_sgm_1
081300150814     C                             )) = UNH_sgm_1
081400150814     C                   eval      Conta_msg = Conta_msg + 1
081500150814     C                   z-add     1             Conta_segm
081600150814     C                   eval      vawDTA = UNH_sgm_1
081700150814     C                              + %trim(%editc(Conta_msg:'4'))
081800150814     C                              + UNH_sgm_2
081900150814     C                              +   apice
082000150814     C*
082100150814     C* BGM (per RMN)
082200151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(BGM_sgm))
082300150814     C                             = BGM_sgm
082400150814     C                   eval      vawDTA = BGM_sgm
082500150814     C                              + %trim(%editc(rabRMNR:'4'))
082600150814     C                              +   apice
082700150814     C*
082800150814     C* RFF (per RMA)
082900151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(RFF_sgm))
083000150814     C                             = RFF_sgm
083100150814     C                   eval      campo_alfa = rabRMAR
083200150814     C                   exsr      Char_Speciali
083300150814     C                   eval      wAlfaRMA = campo_alfa
083400150814     C                   eval      vawDTA = RFF_sgm
083500150814     C                              + %trim(wAlfaRMA)
083600150814     C                              +   apice
083700150814     C*
083800150814     C* UNT (per contatori segmento / messaggio)
083900151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNT_sgm))
084000150814     C                             = UNT_sgm
084100150814     C                   eval      vawDTA = UNT_sgm
084200150814     C                              + %trim(%editc(Conta_segm:'4'))
084300150814     C                              +   piu
084400150814     C                              + %trim(%editc(Conta_msg:'4'))
084500150814     C                              +   apice
084600151005     C                   OTHER
084700151005     C*
084800151005     C* Tutti gli altri segmenti "AS IS"
084900151005     C                   eval      vawDTA = skCpyAccorpa(jCpyAcc)
085000151005     C                   ENDSL
085100150814     C*
085200150814     C* Scarico l'ouput di "duplicazione"
085300170207 xxx C                   exsr      wriOUTPUT
085400150814     C*
085500150814     C                   ADD       1             jCpyAcc
085600150814     C                   ENDDO
085700150814     C*
085800150814     C                   ENDIF
085900150814     C                   ENDIF
086000150814     C*
086100150814     C     KEYrab01P     READE     firab01l
086200150814     C                   ENDDO
086300150814     C                   ENDIF
086400150814     C*
086500150814     C                   ENDSR
086600160304
086700160304
086800160304
086900160304     C     RTVDETT       BEGSR
087000160304     C*
087100160304     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
087200160304     C                   Z-ADD     *zeros        jDETT
087300160304     C     KEYstbd3P     SETLL     tistbd3i
087400160304     C                   IF        %equal(tistbd3i)
087500160304     C     KEYstbd3P     READE     tistbd3i
087600160304     C                   DOW       not %eof(tistbd3i)
087700160304     C* Filtro unicamente i segnacolli "BRT"
087800160304     C                   IF        stbdTIS = 'BRT'
087900160304     C                   ADD       1             jDETT
088000160304     C                   EVAL      DSTBDNPG5 = STBDDTA
088100160304     C                   EVAL      skDETT_RIF(jDETT) = §STBDNPG51
088200160304     C                   EVAL      skDETT_DAT(jDETT) = stbdDAD
088300160304     C                   EVAL      skDETT_ORA(jDETT) = stbdORD
088400160304     C                   ENDIF
088500160304     C     KEYstbd3P     READE     tistbd3i
088600160304     C                   ENDDO
088700160304     C                   ENDIF
088800160304     C*
088900160304     C                   ENDSR
089000170207
089100170207
089200170207
089300170207     C*--------------------------------------------------------------------------------------------*
089400170207     C* /TITLE Compongo il testo e spedisco una e-mail di alert
089500170207     C*--------------------------------------------------------------------------------------------*
089600170207     C     sndAlert      BEGSR
089700170207     C*
089800170207     C* Inizializzo variabili
089900170207     C                   movel     *blanks       wrkEml          253
090000170207     C                   movel     *blanks       wrkCcEml        253
090100170207     C                   movel     *blanks       wrkEmlMsg      5000
090200170207     C                   movel     *blanks       wrkEmlOgg        44
090300170207     C*
090400170207     C* Valorizzo i campi della e-m@ail
090500170207     C                   eval      wrkEml='cedalert@brt.it'
090600170207     C                   eval      wrkCcEml='fabrizio.gurrieri@brt.it'
090700170207     C                   eval      wrkEmlOgg=
090800170207     C                               'BARTVAS - Problema Mappatura TIVMS00F'
090900170207     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
091000170207     C                                                                    ':/N'+
091100170207     C                                                                    ':/N'+
091200170207     C                             'Verificare la mappatura: '           +':/N'+
091300170207     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
091400170207     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
091500170207     C                                                                    ':/N'+
091600170207     C                             'Causale mancante: ' + stbCOS         +':/N'+
091700170207     C                             ' '                                   +':/N'
091800170207     C*
091900170207     C                   call(e)   'TIS701C1'
092000170207     C                   parm                    wrkEml
092100170207     C                   parm                    wrkCcEml
092200170207     C                   parm                    wrkEmlOgg
092300170207     C                   parm                    wrkEmlMsg
092400170207     C                   parm                    wrkEsito          1
092500170207     C*
092600170207     C                   ENDSR
092700031103
092800031103
092900991027
093000991027      /TITLE Operazioni iniziali.
093100991027     C     *inzsr        BEGSR
093200991027     C*
093300991027     C     *ENTRY        PLIST
093400031103     C                   parm                    prmppi
093500991027     C     wrkesito      parm      wrkesito      prmesito
093600031103     C*
093700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
093800031103     C                   EVAL      DSPPI = prmppi
093900131004     C*
094000131004     C* Reperisco i valori variabili ma a livello di transazione corrente
094100131004     C* (tipicamente data e ora relativ al file generato)
094200131004     C                   eval      currDate6    =
094300131004     C                              UBFMTDATE_Convert(
094400131004     C                                  %editc(%dec(%date():*ISO):'X') :
094500131004     C                                  'YYYYMMDD' : 'YYMMDD')
094600131004     C                   eval      currDate6inv =
094700131004     C                              UBFMTDATE_Convert(
094800131004     C                                  %editc(%dec(%date():*ISO):'X') :
094900131004     C                                  'YYYYMMDD' : 'DDMMYY')
095000131004     C                   eval      currHHMM =
095100131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
095200170207     C*
095300170207     C* CALCOLA LA DATA CORRENTE
095400170207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
095500170207     C                   eval      datcor = %dec(%date() : *ISO)
095600030709     C*
095700030709     C* Definizione chiavi
095800030924     C*
095900110103     C* Chiave su TISTB03L - Parziale
096000110103     C     KEYstb03P     KLIST
096100031104     C                   KFLD                    wKstbKSU
096200031103     C                   KFLD                    wKstbFTR
096300160304     C*
096400160304     C* Chiave su TISTBD3I - Parziale
096500160304     C     KEYstbd3P     KLIST
096600160304     C                   KFLD                    wKstbFTR
096700160304     C                   KFLD                    stbKSU
096800160304     C                   KFLD                    stbAAS
096900160304     C                   KFLD                    stbLNP
097000160304     C                   KFLD                    stbNRS
097100160304     C                   KFLD                    stbNSP
097200160304     C                   KFLD                    stbDAS
097300160304     C                   KFLD                    stbORS
097400160304     C                   KFLD                    stbCOS
097500031103     C*
097600031103     C* Chiave su TITAS30C - Parziale
097700031103     C     KEYtas30P     KLIST
097800031103     C                   KFLD                    stbAAS
097900031103     C                   KFLD                    stbLNP
098000031103     C                   KFLD                    stbNRS
098100031103     C                   KFLD                    stbNSP
098200150814     C*
098300150814     C* Chiave su FIRAB01L - Parziale
098400150814     C     KEYrab01P     KLIST
098500150814     C                   KFLD                    wDepAAS
098600150814     C                   KFLD                    wDepLNP
098700150814     C                   KFLD                    wDepNRS
098800150814     C                   KFLD                    wDepNSP
098900170207     C*
099000170207     C* Chiave su TIVMS01I - Parziale
099100170207     C     KEYvms01P     KLIST
099200170207     C                   KFLD                    vmsPROC
099300170207     C                   KFLD                    vmsKSU
099400170207     C                   KFLD                    vmsDDE
099500991027     C*
099600991027     C                   ENDSR
099700170207
