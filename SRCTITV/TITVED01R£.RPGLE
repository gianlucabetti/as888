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
001300161130
001400161130
001500161130     D*-----------
001600161130     D* SCHIERE
001700161130     D*-----------
001800161130     D NumElem         C                   CONST(300)
001900161130     D skCAU_BRT       S                   DIM(NumElem) LIKE(vmsCAUBRT)
002000161130     D skCAU_EVT       S                   DIM(NumElem) LIKE(dsCLI_EVT)
002100161130     D skCAU_RSN       S                   DIM(NumElem) LIKE(dsCLI_RSN)
002200161130     D skCAU_OPZ       S                   DIM(NumElem) LIKE(vmsSTSOPZ)
002300161130     D skCAU_DES       S                   DIM(NumElem) LIKE(vmsDESCRIZ)
002400161130     D jCAU            S              3  0 INZ(*zeros)
002500161130     D j               S              3  0 INZ(*zeros)
002600161130     D*-----------
002700161130     D* DS RIDEFINIZIONE STATUS CLIENTE
002800161130     D*-----------
002900161130     D DS_VMSSTSCLI    DS                  INZ
003000161130     D  dsCLI_EVT                    10
003100161130     D  dsCLI_RSN                     5
003200161130     D*-----------
003300161130     D* COSTANTI SPECIFICHE DEL TRADUTTORE
003400161130     D*-----------
003500161130     D  w§PPIVMSPROC   S                   LIKE(vmsPROC) INZ('TITVED01R')
003600161130     D  w§PPIVMSKSU    S                   LIKE(vmsKSU)  INZ
003700161130     D*-----------
003800161130     D* VARIABILI SPECIFICHE MAPPATURA STATI
003900161130     D*-----------
004000161130     D wWrkSts         S                   like(dsCLI_EVT)  inz
004100161130     D wWrkRsn         S                   like(dsCLI_RSN)  inz
004200161130     D wWrkOpz         S                   like(vmsSTSOPZ)  inz
004300161130     D wWrkDes         S                   like(vmsDESCRIZ) inz
004400161130     D*-----------
004500131104
004600131104
004700041015     D*-----------
004800161130     D* SCHIERE DETTAGLI TISTB
004900041015     D*-----------
005000161206     D NumDETT         C                   CONST(300)
005100161130     D skDETT_RIF      S             15    DIM(NumDETT)
005200161130     D skDETT_DAT      S              8  0 DIM(NumDETT)
005300161130     D skDETT_ORA      S              6  0 DIM(NumDETT)
005400161130     D jDETT           S              3  0 INZ(*zeros)
005500030924
005600041015     D*-----------
005700041015     D* VARIABILI D WRK
005800041015     D*-----------
005900161206     D psds           sds
006000161206     D  procname         *PROC
006100031103     D prmppi          S             50
006200030924     D prmesito        S              1
006300030924     D wrkesito        S                   like(prmesito)
006400161206     D campo_alfa      S             70    varying
006500160224     D wDataStato      S              8  0 inz
006600160224     D wOraStato       S              6  0 inz
006700131004     D currDate6       S              6
006800131004     D currDate6inv    S              6
006900131004     D currHHMM        S              4
007000131216     D Conta_msg       S             10i 0
007100131216     D Conta_segm      S             10i 0
007200161130     D Conta_Stati     S             10i 0 inz
007300170127     D Tot_Stati       S             10i 0 inz(60000)
007400161130     D LibFileBLP01    S             21A   inz
007500161130     D currSysNeta     S              8A   inz('*NULL')
007600131004
007700131004
007800131004     D*-----------
007900131004     D* DEFINIZIONE DS ESTERNE
008000131004     D*-----------
008100161130     D TRUL33DS      E DS                  inz
008200161130     D KPJBA         E DS                  inz
008300161206     D DSTBDNPG5     E DS                  inz
008400170127     D TIS7VASDS     E DS                  inz
008500031103
008600131004
008700131004     D*------------------
008800131004     D* LINKING A DEFINIZIONI ESTERNE
008900131004     D*------------------
009000131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
009100131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
009200160628     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
009300160628     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
009400161130     D/COPY GAITRASRC/SRCPROTOPI,UBVMSDDER
009500131004
009600131004
009700031103     D*-----------
009800031103     D* VARIABILI RIFERITE AL DATA-BASE
009900031103     D*-----------
010000080618     D wKstbKSU        S                   like(stbKSU)
010100080618     D wKstbFTR        S                   like(stbFTR)
010200030924
010300110705
010400031103
010500031103     D*-----------
010600031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
010700031103     D*-----------
010800031103     D DSPPI           DS
010900080618     D  w§PPIKSU                      7  0
011000131004     D  w§UNB_ID                     25
011100131004
011200131004
011300131004     D*-----------
011400131004     D* COSTANTI / DEFAULT
011500131004     D*-----------
011600131004     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+')
011700131007     D UNH_sgm_1       C                   CONST('UNH+')
011800131007     D UNH_sgm_2       C                   CONST('+IFTSTA:D:01A:UN:EAN008')
011900131004     D BGM_sgm         C                   CONST('BGM+23+')
012000131004     D NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
012100131004     D NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
012200131004     D NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
012300131004     D NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
012400131004     D NAD_SF_5        C                   CONST('NNI++29015+IT')
012500131004     D NAD_ST          C                   CONST('NAD+ST++')
012600131010     D LOC_sgm1        C                   CONST('LOC+175+')
012700131010     D LOC_sgm2        C                   CONST('::3:')
012800131010     D LOC_sgm3        C                   CONST('+IT')
012900131004     D CNI_sgm         C                   CONST('CNI+1')
013000170206     D STS_sgm         C                   CONST('STS+1+')
013100131004     D RFF_sgm         C                   CONST('RFF+POR:')
013200131004     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX')
013300131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
013400131004     D EQD_sgm         C                   CONST('EQD+AA')
013500131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
013600160215     D GID_sgm1        C                   CONST('GID+1+')
013700160215     D GID_sgm2        C                   CONST(':CTN')
013800131004     D UNT_sgm         C                   CONST('UNT+')
013900131004     D UNZ_sgm         C                   CONST('UNZ+')
014000131004     D piu             C                   CONST('+')
014100131004     D diviso          C                   CONST(':')
014200131004     D apice           C                   CONST('''')
014300131004
014400030924
014500030325
014600030924     C* MAIN/
014700160628     C*
014800160628     C* Reperisco il sistema AS/400 corrente
014900160628     C                   callp     UBRTVNETA_Rtv(currSysNeta)
015000160628     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
015100160628     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
015200160628     C                   else
015300160628     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
015400160628     C                   endif
015500160628     C*
015600160628     C* Apertura file "overraidati"
015700160628     C                   if        not %open(FNBLP01L)
015800160628     C                   open      FNBLP01L
015900160628     C                   endif
016000161130     C*
016100161130     C* Carica schiere di mappatura
016200161130     C                   SETOFF                                       40
016300161130     C                   EXSR      carMAP
016400161130     C*
016500161130     C* Se caricamento mappatura tutto OK => Procedo
016600161130     C                   IF        not *in40
016700140308     C*
016800140308     C* Spia dati da inviare
016900140308     C                   SETOFF                                       45
017000041015     C*
017100041015     C* Esegue traduzione
017200140308     C                   SETOFF                                       31
017300131004     C  N31              EXSR      chkRECFILMBR
017400131004     C  N31              EXSR      rtvID
017500131004     C  N31              EXSR      traduci
017600140308     C  N31
017700140308     CAN 45              EXSR      wriFooter
017800161130     C*
017900161130     C* Diversamente traduzioe completamente fallita
018000161130     C                   ELSE
018100161130     C                   EVAL      wrkesito = '2'
018200161130     C                   ENDIF
018300160628     C*
018400160628     C* Chiusura file "overraidati"
018500160628     C                   if        %open(FNBLP01L)
018600160628     C                   close     FNBLP01L
018700160628     C                   endif
018800110705     C*
018900921023     C                   SETON                                        LR
019000131004
019100131004
019200131004
019300131004     C*----------------------------------------------------*
019400131004     C*  STACCO ID UNIVOCO
019500131004     C*----------------------------------------------------*
019600131004     C     RTVID         BEGSR
019700131004     C*
019800131004     C                   z-add     *zeros        currUNIQ_ID      15 0
019900131004     C*
020000131004     C* NSP => Stacco un numeratore da AZNUM
020100131004     C                   clear                   TRUL33DS
020200131004     C                   eval      I33OPE = *zeros
020300131004     C                   eval      I33CNU = 302
020400131004     C                   eval      I33NUM = 1
020500131004     C                   movel     TRUL33DS      KPJBU
020600131004     C                   call      'TRUL33R'
020700131004     C                   parm                    KPJBA
020800131004     C                   movel     KPJBU         TRUL33DS
020900131004     C                   if        O33ERR = *zeros
021000131004     C                   z-add     O33NRF        currUNIQ_ID
021100131004     C                   else
021200131004     C                   seton                                        31
021300131004     C                   endif
021400131004     C*
021500131004     C                   ENDSR
021600131004
021700131004
021800131004
021900131004     C     chkRECFILMBR  BEGSR
022000131004     C*
022100131004     C     *start        setll     TIVAWWWT
022200131004     C                   if        %found(TIVAWWWT)
022300131004     C                   seton                                        31
022400131004     C                   endif
022500131004     C*
022600131004     C                   ENDSR
022700131004
022800131004
022900131004
023000131004     C     wriOUTPUT     BEGSR
023100131004     C*
023200131004     C                   write     TIVAW000
023300131004     C*
023400131004     C                   ENDSR
023500131004
023600130807
023700161130
023800161130     C     carMAP        BEGSR
023900161130     C*
024000161130     C* Reperisco la versione di mappatura stati correntemente valida
024100161130     C                   call      'UBVMSDDER'
024200161130     C                   parm      *blanks       iVMSDDEOpz
024300161130     C                   parm      w§PPIVMSPROC  iVMSDDEProc
024400161130     C                   parm      w§PPIVMSKSU   iVMSDDEKSU
024500161130     C                   parm      datcor        iVMSDDEDtRif
024600161130     C                   parm                    oVMSDDEDDE
024700161130     C                   parm                    oVMSDDEEsito
024800161130     C*
024900161130     C* Verifico se esito no OK
025000161130     C                   if        oVMSDDEEsito <> '1'
025100161130     C                   seton                                        40
025200161130     C                   else
025300161130     C*
025400161130     C                   move(P)   w§PPIVMSPROC  vmsPROC
025500161130     C                   move(P)   w§PPIVMSKSU   vmsKSU
025600161130     C                   z-add     oVMSDDEDDE    vmsDDE
025700161130     C     KEYvms01P     setll     tivms01i
025800161130     C                   if        %equal(tivms01i)
025900161130     C     KEYvms01P     reade     tivms01i
026000161130     C*
026100161130     C* Inizializzo indice schiera di mapping
026200161130     C                   z-add     1             jCAU
026300161130     C*
026400161130     C* Ciclo x tutti i record della mappatura richiesta
026500161130     C                   dow       not %eof(tivms01i)
026600161130     C*
026700161130     C* Ignoro i record annullati
026800161130     C                   if        vmsATB = *blanks
026900161130     C*
027000161130     C* Carico la mappatura dei codici status personalizzati cliente
027100161130     C                   eval      DS_VMSSTSCLI = vmsSTSCLI
027200161130     C                   eval      skCAU_BRT(jCAU) = vmsCAUBRT
027300161130     C                   eval      skCAU_EVT(jCAU) = dsCLI_EVT
027400161130     C                   eval      skCAU_RSN(jCAU) = dsCLI_RSN
027500161130     C                   eval      skCAU_OPZ(jCAU) = vmsSTSOPZ
027600161130     C                   eval      skCAU_DES(jCAU) = vmsDESCIT
027700161130     C                   add       1             jCAU
027800161130     C*
027900161130     C                   endif
028000161130     C*
028100161130     C     KEYvms01P     reade     tivms01i
028200161130     C                   enddo
028300161130     C*
028400161130     C                   endif
028500161130     C*
028600161130     C* Verifico che della mappatura richiesta siano stati effettivamente caricati stati
028700161130     C* altrimenti errore bloccante per intera traduzione
028800161130     C                   if        jCAU > 1
028900161130     C                   else
029000161130     C                   seton                                        40
029100161130     C                   endif
029200161130     C*
029300161130     C                   endif
029400161130     C*
029500161130     C                   ENDSR
029600131104
029700131004
029800131004
029900131004     C     Char_Speciali BEGSR
030000131111     C*
030100131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
030200161202     C                   EVAL      campo_alfa = %scanrpl('?':'??':campo_alfa)
030300161202     C                   EVAL      campo_alfa = %scanrpl('+':'?+':campo_alfa)
030400161202     C                   EVAL      campo_alfa = %scanrpl(':':'?:':campo_alfa)
030500161202     C                   EVAL      campo_alfa = %scanrpl(',':'?,':campo_alfa)
030600161202     C                   EVAL      campo_alfa = %scanrpl('''':'?''':campo_alfa)
030700131004     C*
030800131004     C                   ENDSR
030900131004
031000131004
031100131004
031200131004     C     wriHeader     BEGSR
031300131004     C*
031400131004     C                   clear                   vawDTA
031500131010     C*
031600131004     C*  UNB -
031700131004     C                   eval      vawDTA = UNB_sgm
031800131004     C                              + %trim(w§UNB_ID)
031900131004     C                              +   piu
032000131004     C                              + %trim(currDate6inv)
032100131004     C                              +   diviso
032200131004     C                              + %trim(currHHMM)
032300131004     C                              +   piu
032400131004     C                              + %trim(
032500131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
032600131004     C                              +   apice
032700131004     C                   exsr      wriOUTPUT
032800131004     C*
032900131004     C                   ENDSR
033000131004
033100131004
033200131004
033300131004     C     wriFooter     BEGSR
033400131004     C*
033500131004     C                   clear                   vawDTA
033600131004     C*
033700131004     C*  UNZ -
033800131004     C                   eval      vawDTA = UNZ_sgm
033900131004     C                              + %trim(%editc(Conta_msg:'4'))
034000131004     C                              +   piu
034100131004     C                              + %trim(
034200131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
034300131004     C                              +   apice
034400131004     C                   exsr      wriOUTPUT
034500131004     C*
034600131004     C                   ENDSR
034700131004
034800030924
034900030924
035000991027     C     traduci       BEGSR
035100131004     C*
035200131004     C* Inizializzo i contatori a livello di transazione/messaggio
035300131004     C                   z-add     *zeros        Conta_msg
035400031103     C*
035500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
035600031103     C* di traduzione (trattasi d "post-traduzione")
035700031104     C                   EVAL      wKstbKSU = w§PPIKSU
035800031103     C                   EVAL      wKstbFTR = *blanks
035900110103     C     KEYstb03P     SETLL     tistb03l
036000110103     C                   IF        %equal(tistb03l)
036100110103     C     KEYstb03P     READE     tistb03l
036200161130     C                   EVAL      Conta_Stati = Conta_Stati + 1
036300991027     C*
036400161206     C* Ciclo x tutti record del cliente da trasmettere o fino a MAX numero stati per singolo file
036500161130     C                   DOW       not %eof(tistb03l)
036600161206     C                             AND Conta_Stati < Tot_Stati
036700131004     C*
036800131004     C* Inizializzo i contatori a livello di segmento
036900131004     C                   z-add     *zeros        Conta_segm
037000080618     C*
037100130807     C                   SETON                                        50
037200031103     C*
037300031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
037400031103     C* deve restituire al cliente nel file tradotto
037500031103     C                   IF        stbFTR = *blanks                             * ridondante
037600110103     C*
037700110103     C* Gestisco solamente gli stati standard
037800160203     C                   IF        (
037900160203     C                             (stbTIS='1' AND stbPRS<>'7')      OR
038000160203     C                             (stbTIS='1' AND stbPRS ='7'
038100160203     C                                         AND stbCOS ='CONSOK') OR
038200160223     C                             (stbTIS='1' AND stbPRS ='8'
038300160223     C                                         AND stbCOS ='SPUNTNPG5') OR
038400160203     C                             (stbTIS='9' AND stbPRS ='1')
038500160203     C                             ) AND
038600160203     C                             (%subst(stbFOP:1:1)= 'O'  OR
038700160203     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
038800160203     C                             )
038900131004     C*
039000131004     C* Per "sicurezza" non invio stati senza data
039100131004     C                   IF        stbDAS > *zeros
039200110103     C*
039300110103     C                   SETOFF                                       50
039400160628     C                   SETON                                        22
039500031103     C*
039600031103     C* Innanzitutto chaino la bolla direttamente su TITAS
039700160628     C     KEYtas30P     CHAIN     titas30c
039800031103     C                   IF        %found(titas30c)
039900160628     C                   ELSE
040000160628     C*
040100160628     C* Se non trovato TITAS => bolla ancora solo in partenza (FNBLP)
040200160628     C                   CLEAR                   TITAS000
040300160628     C     KEYtas30P     CHAIN     fnblp01l
040400160628     C                   IF        %found(fnblp01l)
040500160628     C*
040600160628     C* Overrido i campi di TITAS con quelli del FNBLP
040700160628     C                   EVAL      tasLNP = blpLNP
040800160628     C                   EVAL      tasLNA = blpLNA
040900160628     C                   EVAL      tasRSD = blpRSD
041000160628     C                   EVAL      tasLOD = blpLOD
041100160628     C                   EVAL      tasIND = blpIND
041200160628     C                   EVAL      tasPRD = blpPRD
041300160628     C                   EVAL      tasCAD = blpCAD
041400160628     C                   EVAL      tasPKB = blpPKB
041500160628     C                   EVAL      tasPKC = blpPKC
041600160628     C                   EVAL      tasNCL = blpNCL
041700160628     C                   ELSE
041800160628     C*
041900160628     C* Se non trovato neanche in BLP allora => bolla non trovata !!
042000160628     C                   SETOFF                                       22
042100160628     C                   ENDIF
042200160628     C                   ENDIF
042300160628     C*
042400160628     C* Se bolla tutto OK
042500160628     C                   IF        *in22
042600131010     C*
042700131010     C* Innanzitutto chaino la bolla direttamente su TITAS
042800131216     C                   MOVEL     *zeros        wPO               3 0
042900131213     C*
043000131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
043100131213     C                   if        tasDTI = *zeros
043200131213     C                   eval      wPO = tasLNP
043300131213     C                   else
043400131213     C                   eval      wPO = tasLNA
043500131213     C                   endif
043600131213     C*
043700131213     C     wPO           CHAIN     azorg01l
043800160224     C                   IF        %found(azorg01l)
043900080618     C*
044000110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
044100031103     C                   Z-ADD     1             j
044200161130     C                   MOVEL(P)  stbCOS        wCOS_15          15
044300161130     C     wCOS_15       LOOKUP    skCAU_BRT(j)                           55
044400161130     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
044500161130     C                   EVAL      wWrkSts = skCAU_EVT(j)
044600161130     C                   EVAL      wWrkRsn = skCAU_RSN(j)
044700160224     C                   EVAL      wWrkOpz = skCAU_OPZ(j)
044800161130     C                   EVAL      wWrkDes = skCAU_DES(j)
044900140514     C                   SETOFF                                       50
045000031103     C                   ELSE
045100161130     C                   CLEAR                   wWrkSts
045200161130     C                   CLEAR                   wWrkRsn
045300161130     C                   CLEAR                   wWrkOpz
045400161130     C                   CLEAR                   wWrkDes
045500031103     C                   SETON                                        50
045600031103     C                   ENDIF
045700161130     C  N55              EXSR      sndAlert
045800140308     C*
045900140308     C* Al primo dettaglio da inviare => scrivo anche testata
046000140313     C                   IF        not *IN50
046100140313     C  N45              EXSR      wriHeader
046200140308     C                   SETON                                        45
046300140313     C                   ENDIF
046400080618     C*
046500160224     C* Se ok per procedere
046600160224     C                   IF        not *IN50
046700160224     C*
046800160224     C* Inizializzo le schiere di work
046900160224     C                   CLEAR                   skDETT_RIF
047000160224     C                   CLEAR                   skDETT_DAT
047100160224     C                   CLEAR                   skDETT_ORA
047200160224     C*
047300160224     C* Verifico il tipo di opzione prevista per il codice stato corrente
047400161130     C                   SELECT
047500161130     C                   WHEN      wWrkOpz = '*DETT'
047600160224     C                   EXSR      RTVDETT
047700161130     C                   ENDSL
047800160224     C*
047900160224     C* Scarico il buffer d output
048000160224     C                   EXSR      wriStato
048100160224     C                   ENDIF
048200110103     C*
048300131010     C                   ENDIF
048400160628 ==> C                   ENDIF                                                  * TITAS30C
048500160628     C*
048600110103     C                   ENDIF
048700110103     C                   ENDIF
048800110103     C                   ENDIF
048900031103     C*
049000031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
049100031103     C                   EVAL      stbFTR = 'T'
049200031103     C                   UPDATE    tistb000
049300991027     C*
049400110103     C     KEYstb03P     READE     tistb03l
049500161130     C                   EVAL      Conta_Stati = Conta_Stati + 1
049600031103     C                   SETOFF                                       50
049700030325     C                   ENDDO
049800161206     C*
049900161206     C* Se raggiunto MAX numero stati bolla per stesso file => esco ed inserisco record "trigger"
050000161206     C                   IF        Conta_Stati >= Tot_Stati
050100170127     C                   CLEAR                   TIS7VASDS
050200170127     C                   EVAL      i§7VASOPZ = 'TRG'
050300170127     C                   EVAL      i§7VASTIP = 'VC'
050400170127     C                   EVAL      i§7VASKSU = '0'+%editc(w§PPIKSU:'X')
050500170127     C                   EVAL      i§7VASTSC = 'WW'
050600170127     C                   EVAL      i§7VASPGM = procname
050700161206     C                   CALL      'TIS7VASR1'
050800170127     C                   PARM                    TIS7VASDS
050900161206     C                   ENDIF
051000161206     C*
051100031103     C                   ENDIF
051200991027     C*
051300030325     C                   EVAL      wrkesito = '0'
051400991027     C*
051500910830     C                   ENDSR
051600031103
051700031103
051800031103
051900131004     C     wriStato      BEGSR
052000131004     C*
052100131004     C* Incremento il contatore di messaggio (al interno della transazione)
052200131004     C                   eval      Conta_msg = Conta_msg + 1
052300131004     C*
052400131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
052500131104     C                   movel     *blanks       wAlfaRMA        512
052600131024     C                   movel     *blanks       wAlfaRSD        512
052700131004     C                   movel     *blanks       wAlfaLOD        512
052800131004     C                   movel     *blanks       wAlfaIND        512
052900131025     C                   movel     *blanks       wAlfaORGDES     512
053000131004     C* RMA
053100131004     C                   eval      campo_alfa = stbRMA
053200131004     C                   exsr      Char_Speciali
053300131004     C                   eval      wAlfaRMA = campo_alfa
053400131024     C* RSD
053500131024     C                   eval      campo_alfa = tasRSD
053600131024     C                   exsr      Char_Speciali
053700131024     C                   eval      wAlfaRSD = campo_alfa
053800131004     C* LOD
053900131004     C                   eval      campo_alfa = tasLOD
054000131004     C                   exsr      Char_Speciali
054100131004     C                   eval      wAlfaLOD = campo_alfa
054200131004     C* IND
054300131004     C                   eval      campo_alfa = tasIND
054400131004     C                   exsr      Char_Speciali
054500131004     C                   eval      wAlfaIND = campo_alfa
054600131025     C* ORGDES
054700131025     C                   eval      campo_alfa = orgDES
054800131025     C                   exsr      Char_Speciali
054900131025     C                   eval      wAlfaORGDES = campo_alfa
055000131004     C*
055100131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
055200131004     C* (ancora corretto ???)
055300131004     C                   z-add     *zeros        wPeso             7 1
055400131004     C                   if        tasPKB > tasPKC
055500131004     C                   eval      wPeso = tasPKB
055600131004     C                   else
055700131004     C                   eval      wPeso = tasPKC
055800131004     C                   endif
055900160224     C*
056000160224     C* Valorizzo la data e ora dello stato corrente
056100160224     C                   eval      wDataStato = stbDAS
056200160224     C                   eval      wOraStato  = stbORS*100
056300160224     C*
056400160606     C* Verifico se per lo stato corrrente sono presenti info anche a livello di collo
056500160224     C                   MOVEL(p)  stbRMN        wAlfaRMN         15
056600160224     C                   Z-ADD     1             j
056700160224     C     wAlfaRMN      LOOKUP    skDETT_RIF(j)                          55
056800160224     C                   IF        %found
056900160224     C                   eval      wDataStato =  skDETT_DAT(j)
057000160224     C                   eval      wOraStato  =  skDETT_ORA(j)
057100160224     C                   ENDIF
057200160224     C*
057300131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
057400131004     C*
057500131004     C*  UNH -
057600131004     C                   eval      Conta_segm  = Conta_segm  + 1
057700131007     C                   eval      vawDTA = UNH_sgm_1
057800131007     C                              + %trim(%editc(Conta_msg:'4'))
057900131007     C                              + UNH_sgm_2
058000131004     C                              +   apice
058100131004     C                   exsr      wriOUTPUT
058200131004     C*
058300131004     C*  BGM -
058400131004     C                   eval      Conta_segm  = Conta_segm  + 1
058500131004     C                   eval      vawDTA = BGM_sgm
058600131004     C                              + %trim(%editc(stbRMN:'4'))
058700131004     C                              +   apice
058800131004     C                   exsr      wriOUTPUT
058900131004     C*
059000131004     C*  NAD SF -
059100131008     C***                eval      Conta_segm  = Conta_segm  + 1
059200131008     C***                eval      vawDTA = NAD_SF_1
059300131008     C***                           + NAD_SF_2
059400131008     C***                           + NAD_SF_3
059500131008     C***                           + NAD_SF_4
059600131008     C***                           + NAD_SF_5
059700131008     C***                           +   apice
059800131008     C***                exsr      wriOUTPUT
059900131004     C*
060000131004     C*  NAD ST -
060100131004     C                   eval      Conta_segm  = Conta_segm  + 1
060200131004     C                   eval      vawDTA = NAD_ST
060300131025     C                              + %trim(wAlfaRSD)
060400131004     C                              +   piu
060500131004     C                              +   piu
060600131004     C                              + %trim(wAlfaIND)
060700131004     C                              +   piu
060800131004     C                              + %trim(wAlfaLOD)
060900131004     C                              +   piu
061000131004     C                              + %trim(tasPRD)
061100131004     C                              +   piu
061200131004     C                              + %trim(tasCAD)
061300131004     C                              +   piu
061400131004     C                              + 'IT'
061500131004     C                              +   apice
061600131004     C                   exsr      wriOUTPUT
061700131004     C*
061800131010     C*  LOC -
061900131004     C                   eval      Conta_segm  = Conta_segm  + 1
062000131004     C                   eval      vawDTA = LOC_sgm1
062100131010     C                              + %trim(%editc(orgFIL:'X'))
062200131004     C                              +   LOC_sgm2
062300131025     C                              + %trim(wAlfaORGDES)
062400131010     C                              +   LOC_sgm3
062500131004     C                              +   apice
062600131004     C                   exsr      wriOUTPUT
062700131004     C*
062800131004     C*  CNI -
062900131004     C                   eval      Conta_segm  = Conta_segm  + 1
063000131004     C                   eval      vawDTA = CNI_sgm
063100131004     C                              +   apice
063200131004     C                   exsr      wriOUTPUT
063300131004     C*
063400131004     C*  STS -
063500131004     C                   eval      Conta_segm  = Conta_segm  + 1
063600131004     C                   eval      vawDTA = STS_sgm
063700170206     C                              + %trim(wWrkSts)
063800170206     C                              +   piu
063900170206     C                              + %trim(wWrkRsn)
064000131004     C                              +   apice
064100131004     C                   exsr      wriOUTPUT
064200131004     C*
064300131004     C*  RFF -
064400131004     C                   eval      Conta_segm  = Conta_segm  + 1
064500131004     C                   eval      vawDTA = RFF_sgm
064600131004     C                              + %trim(wAlfaRMA)
064700131004     C                              +   apice
064800131004     C                   exsr      wriOUTPUT
064900131004     C*
065000131004     C*  TDT -
065100131004     C                   eval      Conta_segm  = Conta_segm  + 1
065200131004     C                   eval      vawDTA = TDT_sgm
065300131004     C                              +   apice
065400131004     C                   exsr      wriOUTPUT
065500131004     C*
065600131004     C*  DTM -
065700131004     C                   eval      Conta_segm  = Conta_segm  + 1
065800131004     C                   eval      vawDTA = DTM_sgm
065900160224     C                              + %trim(%editc(wDataStato*1000000+
066000160224     C                                             wOraStato:'4'))
066100131004     C                              +   diviso
066200131007     C                              + '204'
066300131004     C                              +   apice
066400131004     C                   exsr      wriOUTPUT
066500131004     C*
066600131004     C*  EQD -
066700131004     C                   eval      Conta_segm  = Conta_segm  + 1
066800131004     C                   eval      vawDTA = EQD_sgm
066900131004     C                              +   apice
067000131004     C                   exsr      wriOUTPUT
067100131004     C*
067200131004     C*  MEA -
067300131004     C                   eval      Conta_segm  = Conta_segm  + 1
067400131004     C                   eval      vawDTA = MEA_sgm
067500131004     C                              + %trim(%editc(wPeso:'3'))
067600131004     C                              + '0'
067700131004     C                              +   apice
067800131004     C                   exsr      wriOUTPUT
067900160215     C*
068000160215     C*  GID -
068100160215     C                   eval      Conta_segm  = Conta_segm  + 1
068200160215     C                   eval      vawDTA = GID_sgm1
068300160215     C                              + %trim(%editc(tasNCL:'4'))
068400160215     C                              +   GID_sgm2
068500160215     C                              +   apice
068600160215     C                   exsr      wriOUTPUT
068700131004     C*
068800131004     C*  UNT -
068900131004     C                   eval      Conta_segm  = Conta_segm  + 1
069000131004     C                   eval      vawDTA = UNT_sgm
069100131004     C                              + %trim(%editc(Conta_segm:'4'))
069200131004     C                              +   piu
069300131007     C                              + %trim(%editc(Conta_msg:'4'))
069400131004     C                              +   apice
069500131004     C                   exsr      wriOUTPUT
069600031103     C*
069700031103     C                   ENDSR
069800160224
069900160224
070000160224
070100160224     C     RTVDETT       BEGSR
070200160224     C*
070300160224     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
070400160224     C                   Z-ADD     *zeros        jDETT
070500160224     C     KEYstbd3P     SETLL     tistbd3i
070600160224     C                   IF        %equal(tistbd3i)
070700160224     C     KEYstbd3P     READE     tistbd3i
070800160224     C                   DOW       not %eof(tistbd3i)
070900160224     C* Filtro unicamente i segnacolli "BRT"
071000160224     C                   IF        stbdTIS = 'BRT'
071100160606     C* Considero solamente se presenti dati di data/ora spunta
071200160606     C                   IF        stbdDAD > *zeros AND
071300160606     C                             stbdORD > *zeros
071400160224     C                   ADD       1             jDETT
071500160224     C                   EVAL      DSTBDNPG5 = STBDDTA
071600160224     C                   EVAL      skDETT_RIF(jDETT) = §STBDNPG51
071700160224     C                   EVAL      skDETT_DAT(jDETT) = stbdDAD
071800160224     C                   EVAL      skDETT_ORA(jDETT) = stbdORD
071900160606     C                   ENDIF
072000160224     C                   ENDIF
072100160224     C     KEYstbd3P     READE     tistbd3i
072200160224     C                   ENDDO
072300160224     C                   ENDIF
072400160224     C*
072500160224     C                   ENDSR
072600161130
072700161130
072800161130
072900161130     C*--------------------------------------------------------------------------------------------*
073000161130     C* /TITLE Compongo il testo e spedisco una e-mail di alert
073100161130     C*--------------------------------------------------------------------------------------------*
073200161130     C     sndAlert      BEGSR
073300161130     C*
073400161130     C* Inizializzo variabili
073500161130     C                   movel     *blanks       wrkEml          253
073600161130     C                   movel     *blanks       wrkCcEml        253
073700161130     C                   movel     *blanks       wrkEmlMsg      5000
073800161130     C                   movel     *blanks       wrkEmlOgg        44
073900161130     C*
074000161130     C* Valorizzo i campi della e-m@ail
074100161130     C                   eval      wrkEml='cedalert@brt.it'
074200161130     C                   eval      wrkCcEml='fabrizio.gurrieri@brt.it'
074300161130     C                   eval      wrkEmlOgg=
074400161130     C                               'BARTVAS - Problema Mappatura TIVMS00F'
074500161130     C                   EVAL      wrkEmlMsg = 'Problema Mappatura TIVMS00F'   +
074600161130     C                                                                    ':/N'+
074700161130     C                                                                    ':/N'+
074800161130     C                             'Verificare la mappatura: '           +':/N'+
074900161130     C                             'PROC: ' + w§PPIVMSPROC               +':/N'+
075000161130     C                             'KSU : ' + w§PPIVMSKSU                +':/N'+
075100161130     C                                                                    ':/N'+
075200161130     C                             'Causale mancante: ' + stbCOS         +':/N'+
075300161130     C                             ' '                                   +':/N'
075400161130     C*
075500161130     C                   call(e)   'TIS701C1'
075600161130     C                   parm                    wrkEml
075700161130     C                   parm                    wrkCcEml
075800161130     C                   parm                    wrkEmlOgg
075900161130     C                   parm                    wrkEmlMsg
076000161130     C                   parm                    wrkEsito          1
076100161130     C*
076200161130     C                   ENDSR
076300031103
076400031103
076500991027
076600991027      /TITLE Operazioni iniziali.
076700991027     C     *inzsr        BEGSR
076800991027     C*
076900991027     C     *ENTRY        PLIST
077000031103     C                   parm                    prmppi
077100991027     C     wrkesito      parm      wrkesito      prmesito
077200031103     C*
077300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
077400031103     C                   EVAL      DSPPI = prmppi
077500131004     C*
077600131004     C* Reperisco i valori variabili ma a livello di transazione corrente
077700131004     C* (tipicamente data e ora relativ al file generato)
077800131004     C                   eval      currDate6    =
077900131004     C                              UBFMTDATE_Convert(
078000131004     C                                  %editc(%dec(%date():*ISO):'X') :
078100131004     C                                  'YYYYMMDD' : 'YYMMDD')
078200131004     C                   eval      currDate6inv =
078300131004     C                              UBFMTDATE_Convert(
078400131004     C                                  %editc(%dec(%date():*ISO):'X') :
078500131004     C                                  'YYYYMMDD' : 'DDMMYY')
078600131004     C                   eval      currHHMM =
078700131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
078800161130     C*
078900161130     C* CALCOLA LA DATA CORRENTE
079000161130     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079100161130     C                   eval      datcor = %dec(%date() : *ISO)
079200030709     C*
079300030709     C* Definizione chiavi
079400030924     C*
079500110103     C* Chiave su TISTB03L - Parziale
079600160224     C     KEYstb03P     KLIST
079700160224     C                   KFLD                    wKstbKSU
079800160224     C                   KFLD                    wKstbFTR
079900160224     C*
080000160224     C* Chiave su TISTBD3I - Parziale
080100160224     C     KEYstbd3P     KLIST
080200160224     C                   KFLD                    wKstbFTR
080300160224     C                   KFLD                    stbKSU
080400160224     C                   KFLD                    stbAAS
080500160224     C                   KFLD                    stbLNP
080600160224     C                   KFLD                    stbNRS
080700160224     C                   KFLD                    stbNSP
080800160224     C                   KFLD                    stbDAS
080900160224     C                   KFLD                    stbORS
081000160224     C                   KFLD                    stbCOS
081100031103     C*
081200031103     C* Chiave su TITAS30C - Parziale
081300031103     C     KEYtas30P     KLIST
081400031103     C                   KFLD                    stbAAS
081500031103     C                   KFLD                    stbLNP
081600031103     C                   KFLD                    stbNRS
081700031103     C                   KFLD                    stbNSP
081800161130     C*
081900161130     C* Chiave su TIVMS01I - Parziale
082000161130     C     KEYvms01P     KLIST
082100161130     C                   KFLD                    vmsPROC
082200161130     C                   KFLD                    vmsKSU
082300161130     C                   KFLD                    vmsDDE
082400991027     C*
082500991027     C                   ENDSR
082600161130
