000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500170606     FTIVIRE1L  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800160304     FTISTBD3I  UF   E           K DISK    commit
000900150814     FFIRAB01L  IF   E           K DISK
001000170207     FTIVMS01I  IF   E           K DISK
001100090323     FTIVAWWWT  UF A E             DISK    commit
001200160628     FFNBLP01L  IF   E           K DISK    usropn
001300160628     F                                     extfile(LibFileBLP01)
001400160628     F                                     extdesc('FILTRAPRD/FNBLP01L')
001500170315     FFNARB01L  IF   E           K DISK    usropn
001600170315     F                                     extfile(LibFileARB01)
001700170315     F                                     extdesc('FILTRAPRD/FNARB01L')
001800170207
001900170207
002000170207     D*-----------
002100170207     D* SCHIERE
002200170207     D*-----------
002300170207     D NumElem         C                   CONST(300)
002400170207     D skCAU_BRT       S                   DIM(NumElem) LIKE(vmsCAUBRT)
002500170207     D skCAU_EVT       S                   DIM(NumElem) LIKE(dsCLI_EVT)
002600170207     D skCAU_RSN       S                   DIM(NumElem) LIKE(dsCLI_RSN)
002700170207     D skCAU_OPZ       S                   DIM(NumElem) LIKE(vmsSTSOPZ)
002800170207     D skCAU_DES       S                   DIM(NumElem) LIKE(vmsDESCRIZ)
002900170207     D jCAU            S              3  0 INZ(*zeros)
003000170207     D j               S              3  0 INZ(*zeros)
003100170207     D*-----------
003200170207     D* DS RIDEFINIZIONE STATUS CLIENTE
003300170207     D*-----------
003400170207     D DS_VMSSTSCLI    DS                  INZ
003500170207     D  dsCLI_EVT                    10
003600170207     D  dsCLI_RSN                     5
003700170207     D*-----------
003800170207     D* VARIABILI SPECIFICHE MAPPATURA STATI
003900170207     D*-----------
004000170207     D wWrkSts         S                   like(dsCLI_EVT)  inz
004100170207     D wWrkRsn         S                   like(dsCLI_RSN)  inz
004200170207     D wWrkOpz         S                   like(vmsSTSOPZ)  inz
004300170207     D wWrkDes         S                   like(vmsDESCRIZ) inz
004400170207     D*-----------
004500170207
004600170207
004700170207     D*-----------
004800170207     D* SCHIERE DETTAGLI TISTB
004900170207     D*-----------
005000170207     D NumDETT         C                   CONST(300)
005100170207     D skDETT_RIF      S             15    DIM(NumDETT)
005200170207     D skDETT_DAT      S              8  0 DIM(NumDETT)
005300170207     D skDETT_ORA      S              6  0 DIM(NumDETT)
005400170207     D jDETT           S              3  0 INZ(*zeros)
005500131104
005600131104
005700041015     D*-----------
005800170207     D* SCHIERE PER DE-ACCORPAMENTO
005900041015     D*-----------
006000150814     D skCpyAccorpa    S                   LIKE(vawDTA) DIM(5000) INZ
006100150814     D jCpyAcc         S              5  0 INZ(*zeros)
006200170606
006300170606
006400170606     D*------------------
006500170606     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006600170606     D*------------------
006700170606     D InStringa       S          65535A   VARYING                              (stringa input)
006800170606     D InSepara        S             10A                                        (separatore)
006900170606     D InTypeOut       S              1A                                        (tipo output)
007000170606     D wSkParam        S          65535A                                        (output)
007100170606     D OutErrore       S              1A                                        (flag errore)
007200170606     D OutMsg          S             80A                                        (messaggio errore)
007300170606     D SkSplitCSV_6    S            512    DIM(128)
007400150814
007500030924
007600041015     D*-----------
007700041015     D* VARIABILI D WRK
007800041015     D*-----------
007900170207     D psds           sds
008000170207     D  procname         *PROC
008100031103     D prmppi          S             50
008200030924     D prmesito        S              1
008300030924     D wrkesito        S                   like(prmesito)
008400170207     D campo_alfa      S             70    varying
008500160304     D wDataStato      S              8  0 inz
008600160304     D wOraStato       S              6  0 inz
008700131004     D currDate6       S              6
008800131004     D currDate6inv    S              6
008900131004     D currHHMM        S              4
009000131216     D Conta_msg       S             10i 0
009100131216     D Conta_segm      S             10i 0
009200170207     D Conta_Stati     S             10i 0 inz
009300170207     D Tot_Stati       S             10i 0 inz(60000)
009400170207     D wDepKSU         S                   like(stbKSU) inz
009500150814     D wDepAAS         S                   like(stbAAS) inz
009600150814     D wDepLNP         S                   like(stbLNP) inz
009700150814     D wDepNRS         S                   like(stbNRS) inz
009800150814     D wDepNSP         S                   like(stbNSP) inz
009900160107     D wDepRMN         S                   like(stbRMN) inz
010000160107     D wDepRMA         S                   like(stbRMA) inz
010100150814     D vawDTA_Acc      S                   like(vawDTA) inz
010200160628     D LibFileBLP01    s             21A   inz
010300170315     D LibFileARB01    S             21A   inz
010400160628     D currSysNeta     s              8A   inz('*NULL')
010500170606     D*
010600170606     D w§UNB_ID        S            128    inz(*zeros)
010700170606     D w§NAD_SF        S            256    inz(*zeros)
010800131004
010900131004
011000131004     D*-----------
011100131004     D* DEFINIZIONE DS ESTERNE
011200131004     D*-----------
011300170207     D TRUL33DS      E DS                  inz
011400170207     D KPJBA         E DS                  inz
011500170207     D DSTBDNPG5     E DS                  inz
011600170207     D TIS7VASDS     E DS                  inz
011700170606     D DVIREPE       e ds                  inz
011800031103
011900131004
012000131004     D*------------------
012100131004     D* LINKING A DEFINIZIONI ESTERNE
012200131004     D*------------------
012300131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
012400131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
012500160628     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
012600160628     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
012700170207     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
012800131004
012900131004
013000031103     D*-----------
013100031103     D* VARIABILI RIFERITE AL DATA-BASE
013200031103     D*-----------
013300080618     D wKstbKSU        S                   like(stbKSU)
013400080618     D wKstbFTR        S                   like(stbFTR)
013500170606
013600170606
013700170606     D*-----------
013800170606     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
013900170606     D*-----------
014000170606     D DSPPI           DS
014100170606     D  w§PPIKSU                           like(vireKSC) inz
014200170606     D  w§PPITIP                           like(vireTIP) inz
014300170606     D  w§PPIDTI                      8                  inz
014400170606     D  w§PPIVMSPROC                       like(vmsPROC) inz
014500170606     D  w§PPIVMSKSU                        like(vmsKSU)  inz
014600131004
014700131004
014800131004     D*-----------
014900131004     D* COSTANTI / DEFAULT
015000131004     D*-----------
015100170606     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+')
015200170606     D UNH_sgm_1       C                   CONST('UNH+')
015300170606     D UNH_sgm_2       C                   CONST('+IFTSTA:D:01A:UN:EAN008')
015400170606     D BGM_sgm         C                   CONST('BGM+23+')
015500170606     D NAD_CS          C                   CONST('NAD+CS+BRTXX-IT')
015600170606     D NAD_SF          C                   CONST('NAD+SF+')
015700170606     D NAD_ST          C                   CONST('NAD+ST++')
015800131010     D LOC_sgm1        C                   CONST('LOC+175+')
015900131010     D LOC_sgm2        C                   CONST('::3:')
016000131010     D LOC_sgm3        C                   CONST('+IT')
016100131004     D CNI_sgm         C                   CONST('CNI+1')
016200170207     D STS_sgm         C                   CONST('STS+1+')
016300170606     D RFF_sgm         C                   CONST('RFF+POR:')
016400131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
016500170726     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX-IT')
016600131004     D EQD_sgm         C                   CONST('EQD+AA')
016700131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
016800160215     D GID_sgm1        C                   CONST('GID+1+')
016900160215     D GID_sgm2        C                   CONST(':CTN')
017000131004     D UNT_sgm         C                   CONST('UNT+')
017100131004     D UNZ_sgm         C                   CONST('UNZ+')
017200131004     D piu             C                   CONST('+')
017300131004     D diviso          C                   CONST(':')
017400131004     D apice           C                   CONST('''')
017500131004
017600030924
017700030325
017800030924     C* MAIN/
017900160628     C*
018000160628     C* Reperisco il sistema AS/400 corrente
018100160628     C                   callp     UBRTVNETA_Rtv(currSysNeta)
018200160628     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
018300160628     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
018400170315     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
018500160628     C                   else
018600160628     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
018700170315     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
018800160628     C                   endif
018900160628     C*
019000160628     C* Apertura file "overraidati"
019100160628     C                   if        not %open(FNBLP01L)
019200160628     C                   open      FNBLP01L
019300160628     C                   endif
019400170315     C                   if        not %open(FNARB01L)
019500170315     C                   open      FNARB01L
019600170315     C                   endif
019700170207     C*
019800170207     C* Carica schiere di mappatura
019900170207     C                   SETOFF                                       40
020000170207     C                   EXSR      carMAP
020100170207     C*
020200170207     C* Se caricamento mappatura tutto OK => Procedo
020300170207     C                   IF        not *in40
020400140308     C*
020500140308     C* Spia dati da inviare
020600140308     C                   SETOFF                                       45
020700041015     C*
020800041015     C* Esegue traduzione
020900140308     C                   SETOFF                                       31
021000170606     C  N31              EXSR      SPLIT_PARMS
021100131004     C  N31              EXSR      chkRECFILMBR
021200131004     C  N31              EXSR      rtvID
021300131004     C  N31              EXSR      traduci
021400140308     C  N31
021500140308     CAN 45              EXSR      wriFooter
021600170207     C*
021700170207     C* Diversamente traduzioe completamente fallita
021800170207     C                   ELSE
021900170207     C                   EVAL      wrkesito = '2'
022000170207     C                   ENDIF
022100160628     C*
022200160628     C* Chiusura file "overraidati"
022300160628     C                   if        %open(FNBLP01L)
022400160628     C                   close     FNBLP01L
022500160628     C                   endif
022600170315     C                   if        %open(FNARB01L)
022700170315     C                   close     FNARB01L
022800170315     C                   endif
022900110705     C*
023000921023     C                   SETON                                        LR
023100131004
023200131004
023300131004
023400131004     C*----------------------------------------------------*
023500131004     C*  STACCO ID UNIVOCO
023600131004     C*----------------------------------------------------*
023700131004     C     RTVID         BEGSR
023800131004     C*
023900131004     C                   z-add     *zeros        currUNIQ_ID      15 0
024000131004     C*
024100131004     C* NSP => Stacco un numeratore da AZNUM
024200131004     C                   clear                   TRUL33DS
024300131004     C                   eval      I33OPE = *zeros
024400131004     C                   eval      I33CNU = 302
024500131004     C                   eval      I33NUM = 1
024600131004     C                   movel     TRUL33DS      KPJBU
024700131004     C                   call      'TRUL33R'
024800131004     C                   parm                    KPJBA
024900131004     C                   movel     KPJBU         TRUL33DS
025000131004     C                   if        O33ERR = *zeros
025100131004     C                   z-add     O33NRF        currUNIQ_ID
025200131004     C                   else
025300131004     C                   seton                                        31
025400131004     C                   endif
025500131004     C*
025600131004     C                   ENDSR
025700131004
025800131004
025900131004
026000131004     C     chkRECFILMBR  BEGSR
026100131004     C*
026200131004     C     *start        setll     TIVAWWWT
026300131004     C                   if        %found(TIVAWWWT)
026400131004     C                   seton                                        31
026500131004     C                   endif
026600131004     C*
026700131004     C                   ENDSR
026800131004
026900131004
027000131004
027100131004     C     wriOUTPUT     BEGSR
027200131004     C*
027300131004     C                   write     TIVAW000
027400170207     C                   eval      Conta_Stati = Conta_Stati + 1
027500131004     C*
027600131004     C                   ENDSR
027700170207
027800170207
027900170207
028000170207     C     carMAP        BEGSR
028100170207     C*
028200170207     C* Reperisco la versione di mappatura stati correntemente valida
028300170207     C                   call      'UBVMSDDER'
028400170207     C                   parm      *blanks       iVMSDDEOpz
028500170207     C                   parm      w§PPIVMSPROC  iVMSDDEProc
028600170207     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
028700170207     C                   parm      datcor        iVMSDDEDtRif
028800170207     C                   parm                    oVMSDDEDDE
028900170207     C                   parm                    oVMSDDEEsito
029000170207     C*
029100170207     C* Verifico se esito no OK
029200170207     C                   if        oVMSDDEEsito <> '1'
029300170207     C                   seton                                        40
029400170207     C                   else
029500170207     C*
029600170207     C                   move(P)   w§PPIVMSPROC  vmsPROC
029700170207     C                   move(P)   w§PPIVMSKSU   vmsKSU
029800170207     C                   z-add     oVMSDDEDDE    vmsDDE
029900170207     C     KEYvms01P     setll     tivms01i
030000170207     C                   if        %equal(tivms01i)
030100170207     C     KEYvms01P     reade     tivms01i
030200170207     C*
030300170207     C* Inizializzo indice schiera di mapping
030400170207     C                   z-add     1             jCAU
030500170207     C*
030600170207     C* Ciclo x tutti i record della mappatura richiesta
030700170207     C                   dow       not %eof(tivms01i)
030800170207     C*
030900170207     C* Ignoro i record annullati
031000170207     C                   if        vmsATB = *blanks
031100170207     C*
031200170207     C* Carico la mappatura dei codici status personalizzati cliente
031300170207     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
031400170207     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
031500170207     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
031600170207     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
031700170207     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
031800170207     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
031900170207     C                   add       1             jCAU
032000170207     C*
032100170207     C                   endif
032200170207     C*
032300170207     C     KEYvms01P     reade     tivms01i
032400170207     C                   enddo
032500170207     C*
032600170207     C                   endif
032700170207     C*
032800170207     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
032900170207     C* altrimenti errore bloccante per intera traduzione
033000170207     C                   if        jCAU > 1
033100170207     C                   else
033200170207     C                   seton                                        40
033300170207     C                   endif
033400170207     C*
033500170207     C                   endif
033600170207     C*
033700170207     C                   ENDSR
033800131104
033900131004
034000131004
034100131004     C     Char_Speciali BEGSR
034200170207     C*
034300170207     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
034400170207     C                   EVAL      campo_alfa = %scanrpl('?':'??':campo_alfa)
034500170207     C                   EVAL      campo_alfa = %scanrpl('+':'?+':campo_alfa)
034600170207     C                   EVAL      campo_alfa = %scanrpl(':':'?:':campo_alfa)
034700170207     C                   EVAL      campo_alfa = %scanrpl(',':'?,':campo_alfa)
034800170207     C                   EVAL      campo_alfa = %scanrpl('''':'?''':campo_alfa)
034900131004     C*
035000131004     C                   ENDSR
035100131004
035200131004
035300131004
035400131004     C     wriHeader     BEGSR
035500131004     C*
035600131004     C                   clear                   vawDTA
035700131010     C*
035800131004     C*  UNB -
035900131004     C                   eval      vawDTA = UNB_sgm
036000170606     C                              + %trim(w§UNB_ID)
036100131004     C                              +   piu
036200131004     C                              + %trim(currDate6inv)
036300131004     C                              +   diviso
036400131004     C                              + %trim(currHHMM)
036500131004     C                              +   piu
036600131004     C                              + %trim(
036700131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
036800131004     C                              +   apice
036900131004     C                   exsr      wriOUTPUT
037000131004     C*
037100131004     C                   ENDSR
037200131004
037300131004
037400131004
037500131004     C     wriFooter     BEGSR
037600131004     C*
037700131004     C                   clear                   vawDTA
037800131004     C*
037900131004     C*  UNZ -
038000131004     C                   eval      vawDTA = UNZ_sgm
038100131004     C                              + %trim(%editc(Conta_msg:'4'))
038200131004     C                              +   piu
038300131004     C                              + %trim(
038400131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
038500131004     C                              +   apice
038600131004     C                   exsr      wriOUTPUT
038700131004     C*
038800131004     C                   ENDSR
038900131004
039000030924
039100030924
039200991027     C     traduci       BEGSR
039300131004     C*
039400131004     C* Inizializzo i contatori a livello di transazione/messaggio
039500131004     C                   z-add     *zeros        Conta_msg
039600031103     C*
039700031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
039800031103     C* di traduzione (trattasi d "post-traduzione")
039900170606     C                   MOVE(P)   w§PPIKSU      wKstbKSU
040000031103     C                   EVAL      wKstbFTR = *blanks
040100110103     C     KEYstb03P     SETLL     tistb03l
040200110103     C                   IF        %equal(tistb03l)
040300110103     C     KEYstb03P     READE     tistb03l
040400170207     C*
040500170207     C* Ciclo x tutti record del cliente da trasmettere o fino a MAX numero stati per singolo file
040600170207     C                   DOW       not %eof(tistb03l)
040700170207     C                             AND Conta_Stati < Tot_Stati
040800131004     C*
040900131004     C* Inizializzo i contatori a livello di segmento
041000131004     C                   z-add     *zeros        Conta_segm
041100080618     C*
041200130807     C                   SETON                                        50
041300031103     C*
041400031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
041500031103     C* deve restituire al cliente nel file tradotto
041600031103     C                   IF        stbFTR = *blanks                             * ridondante
041700110103     C*
041800110103     C* Gestisco solamente gli stati standard
041900160212     C                   IF        (
042000160212     C                             (stbTIS='1' AND stbPRS<>'7')      OR
042100160212     C                             (stbTIS='1' AND stbPRS ='7'
042200160212     C                                         AND stbCOS ='CONSOK') OR
042300160304     C                             (stbTIS='1' AND stbPRS ='8'
042400160304     C                                         AND stbCOS ='SPUNTNPG5') OR
042500160212     C                             (stbTIS='9' AND stbPRS ='1')
042600160212     C                             ) AND
042700160212     C                             (%subst(stbFOP:1:1)= 'O'  OR
042800160212     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
042900160212     C                             )
043000131004     C*
043100131004     C* Per "sicurezza" non invio stati senza data
043200131004     C                   IF        stbDAS > *zeros
043300150814     C*
043400110103     C                   SETOFF                                       50
043500151005     C*
043600151005     C* Gestisco rottura di codice per spedizione
043700151005     C                   IF        stbKSU = wDepKSU AND
043800151005     C                             stbAAS = wDepAAS AND
043900151005     C                             stbLNP = wDepLNP AND
044000151005     C                             stbNRS = wDepNRS AND
044100151005     C                             stbNSP = wDepNSP
044200151005     C                   ELSE
044300151005     C*
044400151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
044500151005     C  N50              EXSR      DeAccorpa
044600151005     C*
044700151005     C* Salvo la nuova chiave di rottura
044800151005     C                   EVAL      wDepKSU = stbKSU
044900151005     C                   EVAL      wDepAAS = stbAAS
045000151005     C                   EVAL      wDepLNP = stbLNP
045100151005     C                   EVAL      wDepNRS = stbNRS
045200151005     C                   EVAL      wDepNSP = stbNSP
045300160107     C                   EVAL      wDepRMN = stbRMN
045400160107     C                   EVAL      wDepRMA = stbRMA
045500151005     C*
045600151005     C* Inizializzazioni per nuova chiave di rottura
045700151005     C                   CLEAR                   skCpyAccorpa
045800151005     C                   CLEAR                   jCpyAcc
045900151005     C*
046000151005     C                   ENDIF
046100160628     C*
046200160628     C                   SETON                                        22
046300031103     C*
046400031103     C* Innanzitutto chaino la bolla direttamente su TITAS
046500031103     C     KEYtas30P     CHAIN     titas30c
046600031103     C                   IF        %found(titas30c)
046700160628     C                   ELSE
046800160628     C*
046900160628     C* Se non trovato TITAS => bolla ancora solo in partenza (FNBLP)
047000160628     C                   CLEAR                   TITAS000
047100160628     C     KEYtas30P     CHAIN     fnblp01l
047200160628     C                   IF        %found(fnblp01l)
047300160628     C*
047400160628     C* Overrido i campi di TITAS con quelli del FNBLP
047500160628     C                   EVAL      tasLNP = blpLNP
047600160628     C                   EVAL      tasLNA = blpLNA
047700160628     C                   EVAL      tasRSD = blpRSD
047800160628     C                   EVAL      tasLOD = blpLOD
047900160628     C                   EVAL      tasIND = blpIND
048000160628     C                   EVAL      tasPRD = blpPRD
048100160628     C                   EVAL      tasCAD = blpCAD
048200160628     C                   EVAL      tasPKB = blpPKB
048300160628     C                   EVAL      tasPKC = blpPKC
048400160628     C                   EVAL      tasNCL = blpNCL
048500160628     C                   ELSE
048600160628     C*
048700160628     C* Se non trovato neanche in BLP allora => bolla non trovata !!
048800160628     C                   SETOFF                                       22
048900160628     C                   ENDIF
049000160628     C                   ENDIF
049100160628     C*
049200160628     C* Se bolla tutto OK
049300160628     C                   IF        *in22
049400170315     C*
049500170315     C* Aggancio anche la bolla in arrivo per reperire Data Arrivo Collo
049600170315     C                   Z-ADD     *zeros        wDatArrCol        8 0
049700170315     C     KEYtas30P     CHAIN     fnarb01l
049800170315     C                   IF        %found(fnarb01l)
049900170315     C                   EVAL      wDatArrCol = arbDAM
050000170315     C                   ELSE
050100170315     C                   EVAL      wDatArrCol = tasDTI
050200170315     C                   ENDIF
050300131010     C*
050400131010     C* Innanzitutto chaino la bolla direttamente su TITAS
050500131216     C                   MOVEL     *zeros        wPO               3 0
050600131213     C*
050700131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
050800170315     C                   if        wDatArrCol = *zeros
050900131213     C                   eval      wPO = tasLNP
051000131213     C                   else
051100131213     C                   eval      wPO = tasLNA
051200131213     C                   endif
051300131213     C*
051400131213     C     wPO           CHAIN     azorg01l
051500131010     C                   IF        %found(azorg01l)
051600170207     C*
051700170207     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
051800170207     C                   Z-ADD     1             j
051900170207     C                   MOVEL(P)  stbCOS        wCOS_15          15
052000170207     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
052100170207     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
052200170207     C                   EVAL      wWrkSts = skCAU_EVT(j)
052300170207     C                   EVAL      wWrkRsn = skCAU_RSN(j)
052400170207     C                   EVAL      wWrkOpz = skCAU_OPZ(j)
052500170207     C                   EVAL      wWrkDes = skCAU_DES(j)
052600170207     C                   SETOFF                                       50
052700170207     C                   ELSE
052800170207     C                   CLEAR                   wWrkSts
052900170207     C                   CLEAR                   wWrkRsn
053000170207     C                   CLEAR                   wWrkOpz
053100170207     C                   CLEAR                   wWrkDes
053200170207     C                   SETON                                        50
053300170207     C                   ENDIF
053400170207     C  N55              EXSR      sndAlert
053500140308     C*
053600140308     C* Al primo dettaglio da inviare => scrivo anche testata
053700140313     C                   IF        not *IN50
053800140313     C  N45              EXSR      wriHeader
053900140308     C                   SETON                                        45
054000140313     C                   ENDIF
054100160304     C*
054200160304     C* Se ok per procedere
054300160304     C                   IF        not *IN50
054400160304     C*
054500160304     C* Inizializzo le schiere di work
054600160304     C                   CLEAR                   skDETT_RIF
054700160304     C                   CLEAR                   skDETT_DAT
054800160304     C                   CLEAR                   skDETT_ORA
054900160304     C*
055000160304     C* Verifico il tipo di opzione prevista per il codice stato corrente
055100170207     C                   SELECT
055200170207     C                   WHEN      wWrkOpz = '*DETT'
055300170207     C                   EXSR      RTVDETT
055400170207     C                   ENDSL
055500160304     C*
055600160304     C* Scarico il buffer d output
055700160304     C                   EXSR      wriStato
055800160304     C                   ENDIF
055900110103     C*
056000131010     C                   ENDIF
056100160628 ==> C                   ENDIF                                                  * TITAS30C
056200160628     C*
056300110103     C                   ENDIF
056400110103     C                   ENDIF
056500110103     C                   ENDIF
056600031103     C*
056700031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
056800031103     C                   EVAL      stbFTR = 'T'
056900031103     C                   UPDATE    tistb000
057000991027     C*
057100110103     C     KEYstb03P     READE     tistb03l
057200031103     C                   SETOFF                                       50
057300030325     C                   ENDDO
057400151005     C*
057500151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
057600151005     C  N50              EXSR      DeAccorpa
057700170207     C*
057800170207     C* Se raggiunto MAX numero stati bolla per stesso file => esco ed inserisco record "trigger"
057900170207     C                   IF        Conta_Stati >= Tot_Stati
058000170207     C                   CLEAR                   TIS7VASDS
058100170207     C                   EVAL      i§7VASOPZ = 'TRG'
058200170207     C                   EVAL      i§7VASTIP = 'VC'
058300170606     C                   EVAL      i§7VASKSU = w§PPIKSU
058400170207     C                   EVAL      i§7VASTSC = 'WW'
058500170207     C                   EVAL      i§7VASPGM = procname
058600170207     C                   CALL      'TIS7VASR1'
058700170207     C                   PARM                    TIS7VASDS
058800170207     C                   ENDIF
058900170207     C*
059000031103     C                   ENDIF
059100991027     C*
059200030325     C                   EVAL      wrkesito = '0'
059300991027     C*
059400910830     C                   ENDSR
059500031103
059600031103
059700031103
059800131004     C     wriStato      BEGSR
059900131004     C*
060000131004     C* Incremento il contatore di messaggio (al interno della transazione)
060100131004     C                   eval      Conta_msg = Conta_msg + 1
060200131004     C*
060300131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
060400131104     C                   movel     *blanks       wAlfaRMA        512
060500131024     C                   movel     *blanks       wAlfaRSD        512
060600131004     C                   movel     *blanks       wAlfaLOD        512
060700131004     C                   movel     *blanks       wAlfaIND        512
060800131025     C                   movel     *blanks       wAlfaORGDES     512
060900131004     C* RMA
061000131004     C                   eval      campo_alfa = stbRMA
061100131004     C                   exsr      Char_Speciali
061200131004     C                   eval      wAlfaRMA = campo_alfa
061300131024     C* RSD
061400131024     C                   eval      campo_alfa = tasRSD
061500131024     C                   exsr      Char_Speciali
061600131024     C                   eval      wAlfaRSD = campo_alfa
061700131004     C* LOD
061800131004     C                   eval      campo_alfa = tasLOD
061900131004     C                   exsr      Char_Speciali
062000131004     C                   eval      wAlfaLOD = campo_alfa
062100131004     C* IND
062200131004     C                   eval      campo_alfa = tasIND
062300131004     C                   exsr      Char_Speciali
062400131004     C                   eval      wAlfaIND = campo_alfa
062500131025     C* ORGDES
062600131025     C                   eval      campo_alfa = orgDES
062700131025     C                   exsr      Char_Speciali
062800131025     C                   eval      wAlfaORGDES = campo_alfa
062900131004     C*
063000131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
063100131004     C* (ancora corretto ???)
063200131004     C                   z-add     *zeros        wPeso             7 1
063300131004     C                   if        tasPKB > tasPKC
063400131004     C                   eval      wPeso = tasPKB
063500131004     C                   else
063600131004     C                   eval      wPeso = tasPKC
063700131004     C                   endif
063800160304     C*
063900160304     C* Valorizzo la data e ora dello stato corrente
064000160304     C                   eval      wDataStato = stbDAS
064100160304     C                   eval      wOraStato  = stbORS*100
064200160304     C*
064300160304     C* Verifico se per lo stato corrrente sono presentei info anche a livello di collo
064400160304     C                   MOVEL(p)  stbRMN        wAlfaRMN         15
064500160304     C                   Z-ADD     1             j
064600160304     C     wAlfaRMN      LOOKUP    skDETT_RIF(j)                          55
064700160304     C                   IF        %found
064800160304     C                   eval      wDataStato =  skDETT_DAT(j)
064900160304     C                   eval      wOraStato  =  skDETT_ORA(j)
065000160304     C                   ENDIF
065100160304     C*
065200131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
065300131004     C*
065400131004     C*  UNH -
065500131004     C                   eval      Conta_segm  = Conta_segm  + 1
065600131007     C                   eval      vawDTA = UNH_sgm_1
065700131007     C                              + %trim(%editc(Conta_msg:'4'))
065800131007     C                              + UNH_sgm_2
065900131004     C                              +   apice
066000131004     C                   exsr      wriOUTPUT
066100150814     C                   eval      vawDTA_Acc = vawDTA
066200150814     C                   exsr      CpyAccorpa
066300131004     C*
066400170608     C*  BGM -
066500131004     C                   eval      Conta_segm  = Conta_segm  + 1
066600131004     C                   eval      vawDTA = BGM_sgm
066700131004     C                              + %trim(%editc(stbRMN:'4'))
066800131004     C                              +   apice
066900131004     C                   exsr      wriOUTPUT
067000150814     C                   eval      vawDTA_Acc = vawDTA
067100150814     C                   exsr      CpyAccorpa
067200170606     C*
067300170606     C*  NAD SF -
067400170606     C                   eval      Conta_segm  = Conta_segm  + 1
067500170606     C                   eval      vawDTA = NAD_SF
067600170606     C                              + %trim(w§NAD_SF)
067700170606     C                              +   apice
067800170606     C                   exsr      wriOUTPUT
067900170606     C                   eval      vawDTA_Acc = vawDTA
068000170606     C                   exsr      CpyAccorpa
068100131004     C*
068200170606     C*  NAD ST -
068300131004     C                   eval      Conta_segm  = Conta_segm  + 1
068400131004     C                   eval      vawDTA = NAD_ST
068500131025     C                              + %trim(wAlfaRSD)
068600131004     C                              +   piu
068700131004     C                              +   piu
068800131004     C                              + %trim(wAlfaIND)
068900131004     C                              +   piu
069000131004     C                              + %trim(wAlfaLOD)
069100131004     C                              +   piu
069200131004     C                              + %trim(tasPRD)
069300131004     C                              +   piu
069400131004     C                              + %trim(tasCAD)
069500131004     C                              +   piu
069600131004     C                              + 'IT'
069700131004     C                              +   apice
069800131004     C                   exsr      wriOUTPUT
069900150814     C                   eval      vawDTA_Acc = vawDTA
070000150814     C                   exsr      CpyAccorpa
070100170606     C*
070200170606     C*  NAD CS -
070300170606     C                   eval      Conta_segm  = Conta_segm  + 1
070400170606     C                   eval      vawDTA = NAD_CS
070500170606     C                              +   apice
070600170606     C                   exsr      wriOUTPUT
070700170606     C                   eval      vawDTA_Acc = vawDTA
070800170606     C                   exsr      CpyAccorpa
070900131004     C*
071000131010     C*  LOC -
071100131004     C                   eval      Conta_segm  = Conta_segm  + 1
071200131004     C                   eval      vawDTA = LOC_sgm1
071300131010     C                              + %trim(%editc(orgFIL:'X'))
071400131004     C                              +   LOC_sgm2
071500131025     C                              + %trim(wAlfaORGDES)
071600131010     C                              +   LOC_sgm3
071700131004     C                              +   apice
071800131004     C                   exsr      wriOUTPUT
071900150814     C                   eval      vawDTA_Acc = vawDTA
072000150814     C                   exsr      CpyAccorpa
072100131004     C*
072200131004     C*  CNI -
072300131004     C                   eval      Conta_segm  = Conta_segm  + 1
072400131004     C                   eval      vawDTA = CNI_sgm
072500131004     C                              +   apice
072600131004     C                   exsr      wriOUTPUT
072700150814     C                   eval      vawDTA_Acc = vawDTA
072800150814     C                   exsr      CpyAccorpa
072900131004     C*
073000131004     C*  STS -
073100131004     C                   eval      Conta_segm  = Conta_segm  + 1
073200131004     C                   eval      vawDTA = STS_sgm
073300170207     C                              + %trim(wWrkSts)
073400170207     C                              +   piu
073500170207     C                              + %trim(wWrkRsn)
073600170207     C                              +   apice
073700131004     C                   exsr      wriOUTPUT
073800150814     C                   eval      vawDTA_Acc = vawDTA
073900150814     C                   exsr      CpyAccorpa
074000131004     C*
074100170606     C*  RFF -
074200131004     C                   eval      Conta_segm  = Conta_segm  + 1
074300131004     C                   eval      vawDTA = RFF_sgm
074400131004     C                              + %trim(wAlfaRMA)
074500131004     C                              +   apice
074600131004     C                   exsr      wriOUTPUT
074700150814     C                   eval      vawDTA_Acc = vawDTA
074800150814     C                   exsr      CpyAccorpa
074900131004     C*
075000131004     C*  DTM -
075100131004     C                   eval      Conta_segm  = Conta_segm  + 1
075200131004     C                   eval      vawDTA = DTM_sgm
075300160304     C                              + %trim(%editc(wDataStato*1000000+
075400160304     C                                             wOraStato:'4'))
075500131004     C                              +   diviso
075600131007     C                              + '204'
075700131004     C                              +   apice
075800131004     C                   exsr      wriOUTPUT
075900150814     C                   eval      vawDTA_Acc = vawDTA
076000150814     C                   exsr      CpyAccorpa
076100170726     C*
076200170726     C*  TDT -
076300170726     C                   eval      Conta_segm  = Conta_segm  + 1
076400170726     C                   eval      vawDTA = TDT_sgm
076500170726     C                              +   apice
076600170726     C                   exsr      wriOUTPUT
076700170726     C                   eval      vawDTA_Acc = vawDTA
076800170726     C                   exsr      CpyAccorpa
076900131004     C*
077000131004     C*  EQD -
077100131004     C                   eval      Conta_segm  = Conta_segm  + 1
077200131004     C                   eval      vawDTA = EQD_sgm
077300131004     C                              +   apice
077400131004     C                   exsr      wriOUTPUT
077500150814     C                   eval      vawDTA_Acc = vawDTA
077600150814     C                   exsr      CpyAccorpa
077700131004     C*
077800131004     C*  MEA -
077900131004     C                   eval      Conta_segm  = Conta_segm  + 1
078000131004     C                   eval      vawDTA = MEA_sgm
078100131004     C                              + %trim(%editc(wPeso:'3'))
078200131004     C                              + '0'
078300131004     C                              +   apice
078400131004     C                   exsr      wriOUTPUT
078500150814     C                   eval      vawDTA_Acc = vawDTA
078600150814     C                   exsr      CpyAccorpa
078700160215     C*
078800160215     C*  GID -
078900160215     C                   eval      Conta_segm  = Conta_segm  + 1
079000160215     C                   eval      vawDTA = GID_sgm1
079100160215     C                              + %trim(%editc(tasNCL:'4'))
079200160215     C                              +   GID_sgm2
079300160215     C                              +   apice
079400160215     C                   exsr      wriOUTPUT
079500150814     C                   eval      vawDTA_Acc = vawDTA
079600150814     C                   exsr      CpyAccorpa
079700131004     C*
079800131004     C*  UNT -
079900131004     C                   eval      Conta_segm  = Conta_segm  + 1
080000131004     C                   eval      vawDTA = UNT_sgm
080100131004     C                              + %trim(%editc(Conta_segm:'4'))
080200131004     C                              +   piu
080300131007     C                              + %trim(%editc(Conta_msg:'4'))
080400131004     C                              +   apice
080500131004     C                   exsr      wriOUTPUT
080600150814     C                   eval      vawDTA_Acc = vawDTA
080700150814     C                   exsr      CpyAccorpa
080800031103     C*
080900031103     C                   ENDSR
081000150814
081100150814
081200150814
081300150814     C     CpyAccorpa    BEGSR
081400150814     C*
081500150814     C                   ADD       1             jCpyAcc
081600150814     C                   EVAL      skCpyAccorpa(jCpyAcc) = vawDTA_Acc
081700150814     C*
081800150814     C                   ENDSR
081900150814
082000150814
082100150814
082200170207     C     DeAccorpa     BEGSR
082300150814     C*
082400150814     C* Scorro tutte le spedizioni accorpate sulla spedizone corrente
082500150814     C     KEYrab01P     SETLL     firab01l
082600150814     C                   IF        %equal(firab01l)
082700150814     C     KEYrab01P     READE     firab01l
082800150814     C                   DOW       not %eof(firab01l)
082900150814     C*
083000150814     C* Controllo (ridondante) su riferimenti spedizione accorpante
083100160107     C                   IF        wDepRMN = rabRMN AND
083200160107     C                             wDepRMA = rabRMA
083300150814     C*
083400150814     C* Considero solamente spedizioni ACCORPATE
083500150814     C                   IF        rabRMN = rabRMNR AND
083600150814     C                             rabRMA = rabRMAR
083700150814     C                   ELSE
083800150814     C*
083900150814     C* Per ogni spedizione ACCORPATA "duplico" la spedizione ACCORPANTE
084000150814     C                   Z-ADD     1             jCpyAcc
084100150814     C                   DOW       jCpyAcc <= %elem(skCpyAccorpa) AND
084200150814     C                             skCpyAccorpa(jCpyAcc) <> *blanks
084300150814     C*
084400150814     C* Incremento il contatore dei segmenti
084500150814     C                   eval      Conta_segm  = Conta_segm  + 1
084600150814     C*
084700150814     C* Effettuo sostuituzione di valori rispetto bolla ACCORPANTE e sue ACCORPATE
084800151005     C                   SELECT
084900150814     C*
085000150814     C* UNH (per contatori segmento / messaggio)
085100151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNH_sgm_1
085200150814     C                             )) = UNH_sgm_1
085300150814     C                   eval      Conta_msg = Conta_msg + 1
085400150814     C                   z-add     1             Conta_segm
085500150814     C                   eval      vawDTA = UNH_sgm_1
085600150814     C                              + %trim(%editc(Conta_msg:'4'))
085700150814     C                              + UNH_sgm_2
085800150814     C                              +   apice
085900150814     C*
086000150814     C* BGM (per RMN)
086100151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(BGM_sgm))
086200150814     C                             = BGM_sgm
086300150814     C                   eval      vawDTA = BGM_sgm
086400150814     C                              + %trim(%editc(rabRMNR:'4'))
086500150814     C                              +   apice
086600150814     C*
086700150814     C* RFF (per RMA)
086800151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(RFF_sgm))
086900150814     C                             = RFF_sgm
087000150814     C                   eval      campo_alfa = rabRMAR
087100150814     C                   exsr      Char_Speciali
087200150814     C                   eval      wAlfaRMA = campo_alfa
087300150814     C                   eval      vawDTA = RFF_sgm
087400150814     C                              + %trim(wAlfaRMA)
087500150814     C                              +   apice
087600150814     C*
087700150814     C* UNT (per contatori segmento / messaggio)
087800151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNT_sgm))
087900150814     C                             = UNT_sgm
088000150814     C                   eval      vawDTA = UNT_sgm
088100150814     C                              + %trim(%editc(Conta_segm:'4'))
088200150814     C                              +   piu
088300150814     C                              + %trim(%editc(Conta_msg:'4'))
088400150814     C                              +   apice
088500151005     C                   OTHER
088600151005     C*
088700151005     C* Tutti gli altri segmenti "AS IS"
088800151005     C                   eval      vawDTA = skCpyAccorpa(jCpyAcc)
088900151005     C                   ENDSL
089000150814     C*
089100150814     C* Scarico l'ouput di "duplicazione"
089200170606     C                   exsr      wriOUTPUT
089300150814     C*
089400150814     C                   ADD       1             jCpyAcc
089500150814     C                   ENDDO
089600150814     C*
089700150814     C                   ENDIF
089800150814     C                   ENDIF
089900150814     C*
090000150814     C     KEYrab01P     READE     firab01l
090100150814     C                   ENDDO
090200150814     C                   ENDIF
090300150814     C*
090400150814     C                   ENDSR
090500160304
090600160304
090700160304
090800160304     C     RTVDETT       BEGSR
090900160304     C*
091000160304     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
091100160304     C                   Z-ADD     *zeros        jDETT
091200160304     C     KEYstbd3P     SETLL     tistbd3i
091300160304     C                   IF        %equal(tistbd3i)
091400160304     C     KEYstbd3P     READE     tistbd3i
091500160304     C                   DOW       not %eof(tistbd3i)
091600160304     C* Filtro unicamente i segnacolli "BRT"
091700160304     C                   IF        stbdTIS = 'BRT'
091800160304     C                   ADD       1             jDETT
091900160304     C                   EVAL      DSTBDNPG5 = STBDDTA
092000160304     C                   EVAL      skDETT_RIF(jDETT) = §STBDNPG51
092100160304     C                   EVAL      skDETT_DAT(jDETT) = stbdDAD
092200160304     C                   EVAL      skDETT_ORA(jDETT) = stbdORD
092300160304     C                   ENDIF
092400160304     C     KEYstbd3P     READE     tistbd3i
092500160304     C                   ENDDO
092600160304     C                   ENDIF
092700160304     C*
092800160304     C                   ENDSR
092900170606
093000170606
093100170606
093200170606     C*----------------------------------------------------*
093300170606     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
093400170606     C*----------------------------------------------------*
093500170606     C     SPLIT_PARMS   BEGSR
093600170606     C*
093700170606     C                   reset                   w§UNB_ID
093800170606     C                   reset                   w§NAD_SF
093900170606     C*
094000170606     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
094100170606     C                   CLEAR                   DVIREPE
094200170606     C                   EVAL      vireTRC = 'PE'
094300170606     C                   MOVE(P)   w§PPIDTI      vireDTI
094400170606     C     KEYvire1C     CHAIN     tivire1l
094500170606     C                   IF        %found(tivire1l)
094600170606     C                   EVAL      DVIREPE = vireFLO
094700170606     C*
094800170606     C* Se reperiti parametri => effettuo splitting
094900170606     C                   eval      InStringa = %trim(§VIREPEKPJ)
095000170606     C*
095100170606     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
095200170606     C                   CALL      'XSPLIT2'
095300170606     C                   PARM                    InStringa
095400170606     C                   PARM      '§'           InSepara
095500170606     C                   PARM      '6'           InTypeOut
095600170606     C                   PARM      ''            wSkParam
095700170606     C                   PARM                    OutErrore
095800170606     C                   PARM                    OutMsg
095900170606     C                   MOVEA     wSkParam      SkSplitCSV_6
096000170606     C*
096100170606     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
096200170606     C                   z-add     1             i                 4 0
096300170606     C                   dow       i <= %elem(SkSplitCSV_6)
096400170606     C                   if        SkSplitCSV_6(i) = *blanks
096500170606     C                   leave
096600170606     C                   else
096700170606     C                   select
096800170606     C* - parametro UNB
096900170606     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
097000170606     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
097100170606     C* - parametro NAD+SF
097200170606     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
097300170606     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
097400170606     C*
097500170606     C                   endsl
097600170606     C                   endif
097700170606     C*
097800170606     C                   add       1             i
097900170606     C                   enddo
098000170606     C*
098100170606     C                   ENDIF
098200170606     C*
098300170606     C                   ENDSR
098400170207
098500170207
098600170207
098700170207     C*--------------------------------------------------------------------------------------------*
098800170207     C* /TITLE Compongo il testo e spedisco una e-mail di alert
098900170207     C*--------------------------------------------------------------------------------------------*
099000170207     C     sndAlert      BEGSR
099100170207     C*
099200170207     C* Inizializzo variabili
099300170207     C                   movel     *blanks       wrkEml          253
099400170207     C                   movel     *blanks       wrkCcEml        253
099500170207     C                   movel     *blanks       wrkEmlMsg      5000
099600170207     C                   movel     *blanks       wrkEmlOgg        44
099700170207     C*
099800170207     C* Valorizzo i campi della e-m@ail
099900170207     C                   eval      wrkEml='cedalert@brt.it'
100000170207     C                   eval      wrkEmlOgg=
100100170207     C                               'BARTVAS - Problema Mappatura TIVMS00F'
100200170207     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
100300170207     C                                                                    ':/N'+
100400170207     C                                                                    ':/N'+
100500170207     C                             'Verificare la mappatura: '           +':/N'+
100600170207     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
100700170207     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
100800170207     C                                                                    ':/N'+
100900170207     C                             'Causale mancante: ' + stbCOS         +':/N'+
101000170207     C                             ' '                                   +':/N'
101100170207     C*
101200170207     C                   call(e)   'TIS701C1'
101300170207     C                   parm                    wrkEml
101400170207     C                   parm                    wrkCcEml
101500170207     C                   parm                    wrkEmlOgg
101600170207     C                   parm                    wrkEmlMsg
101700170207     C                   parm                    wrkEsito          1
101800170207     C*
101900170207     C                   ENDSR
102000031103
102100031103
102200991027
102300991027      /TITLE Operazioni iniziali.
102400991027     C     *inzsr        BEGSR
102500991027     C*
102600991027     C     *ENTRY        PLIST
102700031103     C                   parm                    prmppi
102800991027     C     wrkesito      parm      wrkesito      prmesito
102900031103     C*
103000031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
103100031103     C                   EVAL      DSPPI = prmppi
103200131004     C*
103300131004     C* Reperisco i valori variabili ma a livello di transazione corrente
103400131004     C* (tipicamente data e ora relativ al file generato)
103500131004     C                   eval      currDate6    =
103600131004     C                              UBFMTDATE_Convert(
103700131004     C                                  %editc(%dec(%date():*ISO):'X') :
103800131004     C                                  'YYYYMMDD' : 'YYMMDD')
103900131004     C                   eval      currDate6inv =
104000131004     C                              UBFMTDATE_Convert(
104100131004     C                                  %editc(%dec(%date():*ISO):'X') :
104200131004     C                                  'YYYYMMDD' : 'DDMMYY')
104300131004     C                   eval      currHHMM =
104400131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
104500170207     C*
104600170207     C* CALCOLA LA DATA CORRENTE
104700170207     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
104800170207     C                   eval      datcor = %dec(%date() : *ISO)
104900030709     C*
105000030709     C* Definizione chiavi
105100030924     C*
105200110103     C* Chiave su TISTB03L - Parziale
105300110103     C     KEYstb03P     KLIST
105400031104     C                   KFLD                    wKstbKSU
105500031103     C                   KFLD                    wKstbFTR
105600160304     C*
105700160304     C* Chiave su TISTBD3I - Parziale
105800160304     C     KEYstbd3P     KLIST
105900160304     C                   KFLD                    wKstbFTR
106000160304     C                   KFLD                    stbKSU
106100160304     C                   KFLD                    stbAAS
106200160304     C                   KFLD                    stbLNP
106300160304     C                   KFLD                    stbNRS
106400160304     C                   KFLD                    stbNSP
106500160304     C                   KFLD                    stbDAS
106600160304     C                   KFLD                    stbORS
106700160304     C                   KFLD                    stbCOS
106800031103     C*
106900031103     C* Chiave su TITAS30C - Parziale
107000031103     C     KEYtas30P     KLIST
107100031103     C                   KFLD                    stbAAS
107200031103     C                   KFLD                    stbLNP
107300031103     C                   KFLD                    stbNRS
107400031103     C                   KFLD                    stbNSP
107500170606     C*
107600170606     C* Chiave su TIVIRE1L - Parziale
107700170606     C     KEYvire1C     KLIST
107800170606     C                   KFLD                    w§PPIKSU
107900170606     C                   KFLD                    w§PPITIP
108000170606     C                   KFLD                    vireDTI
108100170606     C                   KFLD                    vireTRC
108200150814     C*
108300150814     C* Chiave su FIRAB01L - Parziale
108400150814     C     KEYrab01P     KLIST
108500150814     C                   KFLD                    wDepAAS
108600150814     C                   KFLD                    wDepLNP
108700150814     C                   KFLD                    wDepNRS
108800150814     C                   KFLD                    wDepNSP
108900170207     C*
109000170207     C* Chiave su TIVMS01I - Parziale
109100170207     C     KEYvms01P     KLIST
109200170207     C                   KFLD                    vmsPROC
109300170207     C                   KFLD                    vmsKSU
109400170207     C                   KFLD                    vmsDDE
109500991027     C*
109600991027     C                   ENDSR
109700170207
