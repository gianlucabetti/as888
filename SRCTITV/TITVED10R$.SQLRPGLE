000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500041015     FTNTBE01L  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800160304     FTISTBD3I  UF   E           K DISK    commit
000900150814     FFIRAB01L  IF   E           K DISK
001000090323     FTIVAWWWT  UF A E             DISK    commit
001100160628     FFNBLP01L  IF   E           K DISK    usropn
001200160628     F                                     extfile(LibFileBLP01)
001300160628     F                                     extdesc('FILTRAPRD/FNBLP01L')
001400031103
001500031103     D*-----------
001600031103     D* SCHIERE A PROGRAMMA
001700031103     D*-----------
001800131104     D WCAU            S             90    DIM(90) CTDATA PERRCD(1)
001900131104     D WDCT            S             30    DIM(4)  CTDATA PERRCD(1)
002000131104     D WTIS9           S             30    DIM(10) CTDATA PERRCD(1)
002100131104
002200131104
002300131104     D*-----------
002400131104     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
002500131104     D*-----------
002600131104     D DS_WCAU         DS
002700131104     D  dsCAU_BRT                    15
002800160304     D  dsCAU_CLI                    10
002900160304     D  dsCAU_OPZ                     5
003000131104     D  dsCAU_DES                    60
003100131104     D*
003200131104     D DS_WDCT         DS
003300131104     D  dsDCT_BRT                    15
003400131104     D  dsDCT_CLI                    15
003500131104     D*
003600131104     D DS_WTIS9        DS
003700131104     D  dsTIS9_BRT                   15
003800131104     D  dsTIS9_CLI                   15
003900131104
004000131104
004100041015     D*-----------
004200041015     D* SCHIERE
004300041015     D*-----------
004400131104     D skCAU_BRT       S             10    DIM(%elem(WCAU))
004500131104     D skCAU_CLI       S             10    DIM(%elem(WCAU))
004600160304     D skCAU_OPZ       S              5    DIM(%elem(WCAU))
004700131104     D skCAU_DES       S             60    DIM(%elem(WCAU))
004800131104     D jCAU            S              3  0 INZ(*zeros)
004900131104     D skDCT_BRT       S              1    DIM(%elem(WDCT))
005000131104     D skDCT_CLI       S             10    DIM(%elem(WDCT))
005100131104     D jDCT            S              3  0 INZ(*zeros)
005200131104     D skTIS9_BRT      S             10    DIM(%elem(WTIS9))
005300131104     D skTIS9_CLI      S             10    DIM(%elem(WTIS9))
005400131104     D jTIS9           S              3  0 INZ(*zeros)
005500131104     D skTADCod        S              2    DIM(100)
005600131104     D skTADRag        S              1    DIM(100)
005700041015     D jTAD            S              3  0 INZ(*zeros)
005800150814     D skCpyAccorpa    S                   LIKE(vawDTA) DIM(5000) INZ
005900150814     D jCpyAcc         S              5  0 INZ(*zeros)
006000160304     D skDETT_RIF      S             15    DIM(200)
006100160304     D skDETT_DAT      S              8  0 DIM(200)
006200160304     D skDETT_ORA      S              6  0 DIM(200)
006300160304     D jDETT           S              3  0 INZ(*zeros)
006400150814
006500030924
006600041015     D*-----------
006700041015     D* VARIABILI D WRK
006800041015     D*-----------
006900031103     D prmppi          S             50
007000030924     D prmesito        S              1
007100030924     D wrkesito        S                   like(prmesito)
007200031103     D j               S              2  0
007300130807     D wTADCod         S              2
007400131004     D campo_alfa      S            512
007500131004     D wWrkSts         S             25
007600160304     D wWrkOpz         S                   like(dsCAU_OPZ) inz
007700160304     D wDataStato      S              8  0 inz
007800160304     D wOraStato       S              6  0 inz
007900131004     D currDate6       S              6
008000131004     D currDate6inv    S              6
008100131004     D currHHMM        S              4
008200131216     D Conta_msg       S             10i 0
008300131216     D Conta_segm      S             10i 0
008400150814     D wDepKSU         S                   like(stbKSU) inz
008500150814     D wDepAAS         S                   like(stbAAS) inz
008600150814     D wDepLNP         S                   like(stbLNP) inz
008700150814     D wDepNRS         S                   like(stbNRS) inz
008800150814     D wDepNSP         S                   like(stbNSP) inz
008900160107     D wDepRMN         S                   like(stbRMN) inz
009000160107     D wDepRMA         S                   like(stbRMA) inz
009100150814     D vawDTA_Acc      S                   like(vawDTA) inz
009200160628     D LibFileBLP01    s             21A   inz
009300160628     D currSysNeta     s              8A   inz('*NULL')
009400131004
009500131004
009600131004     D*-----------
009700131004     D* DEFINIZIONE DS ESTERNE
009800131004     D*-----------
009900131004     D trul33ds      e ds                  inz
010000131004     D kpjba         e ds                  inz
010100131004     D DTAD          e ds                  inz
010200160304     D DSTBDNPG5     E DS
010300031103
010400131004
010500131004     D*------------------
010600131004     D* LINKING A DEFINIZIONI ESTERNE
010700131004     D*------------------
010800131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
010900131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
011000160628     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011100160628     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
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
012700131004     D  w§UNB_ID                     25
012800131004
012900131004
013000131004     D*-----------
013100131004     D* COSTANTI / DEFAULT
013200131004     D*-----------
013300131004     D UNB_sgm         C                   CONST('UNB+UNOA:1+BRTXX-IT:14+')
013400131007     D UNH_sgm_1       C                   CONST('UNH+')
013500131007     D UNH_sgm_2       C                   CONST('+IFTSTA:D:01A:UN:EAN008')
013600131004     D BGM_sgm         C                   CONST('BGM+23+')
013700131004     D NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
013800131004     D NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
013900131004     D NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
014000131004     D NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
014100131004     D NAD_SF_5        C                   CONST('NNI++29015+IT')
014200131004     D NAD_ST          C                   CONST('NAD+ST++')
014300131010     D LOC_sgm1        C                   CONST('LOC+175+')
014400131010     D LOC_sgm2        C                   CONST('::3:')
014500131010     D LOC_sgm3        C                   CONST('+IT')
014600131004     D CNI_sgm         C                   CONST('CNI+1')
014700131004     D STS_sgm         C                   CONST('STS+1++')
014800131004     D RFF_sgm         C                   CONST('RFF+POR:')
014900131004     D TDT_sgm         C                   CONST('TDT+30+1+++BRTXX')
015000131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
015100131004     D EQD_sgm         C                   CONST('EQD+AA')
015200131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
015300160215     D*GID_sgm         C                   CONST('GID+1+1:CTN')
015400160215     D GID_sgm1        C                   CONST('GID+1+')
015500160215     D GID_sgm2        C                   CONST(':CTN')
015600131004     D UNT_sgm         C                   CONST('UNT+')
015700131004     D UNZ_sgm         C                   CONST('UNZ+')
015800131004     D piu             C                   CONST('+')
015900131004     D diviso          C                   CONST(':')
016000131004     D apice           C                   CONST('''')
016100131004
016200030924
016300030325
016400030924     C* MAIN/
016500170123     C*
016600170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
016700170123     C
016800170123     C/EXEC SQL
016900170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
017000170123     C/END-EXEC
017100160628     C*
017200160628     C* Reperisco il sistema AS/400 corrente
017300160628     C                   callp     UBRTVNETA_Rtv(currSysNeta)
017400160628     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
017500160628     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
017600160628     C                   else
017700160628     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
017800160628     C                   endif
017900160628     C*
018000160628     C* Apertura file "overraidati"
018100160628     C                   if        not %open(FNBLP01L)
018200160628     C                   open      FNBLP01L
018300160628     C                   endif
018400041015     C*
018500041015     C* Carica i dati tabellati
018600041015     C                   EXSR      cartab
018700140308     C*
018800140308     C* Spia dati da inviare
018900140308     C                   SETOFF                                       45
019000041015     C*
019100041015     C* Esegue traduzione
019200140308     C                   SETOFF                                       31
019300131004     C  N31              EXSR      chkRECFILMBR
019400131004     C  N31              EXSR      rtvID
019500140308     C***  N31              EXSR      wriHeader
019600131004     C  N31              EXSR      traduci
019700140308     C  N31
019800140308     CAN 45              EXSR      wriFooter
019900140308 xxx C***   31              EXSR      sndAlert
020000160628     C*
020100160628     C* Chiusura file "overraidati"
020200160628     C                   if        %open(FNBLP01L)
020300160628     C                   close     FNBLP01L
020400160628     C                   endif
020500110705     C*
020600921023     C                   SETON                                        LR
020700131004
020800131004
020900131004
021000131004     C*----------------------------------------------------*
021100131004     C*  STACCO ID UNIVOCO
021200131004     C*----------------------------------------------------*
021300131004     C     RTVID         BEGSR
021400131004     C*
021500131004     C                   z-add     *zeros        currUNIQ_ID      15 0
021600131004     C*
021700131004     C* NSP => Stacco un numeratore da AZNUM
021800131004     C                   clear                   TRUL33DS
021900131004     C                   eval      I33OPE = *zeros
022000131004     C                   eval      I33CNU = 302
022100131004     C                   eval      I33NUM = 1
022200131004     C                   movel     TRUL33DS      KPJBU
022300131004     C                   call      'TRUL33R'
022400131004     C                   parm                    KPJBA
022500131004     C                   movel     KPJBU         TRUL33DS
022600131004     C                   if        O33ERR = *zeros
022700131004     C                   z-add     O33NRF        currUNIQ_ID
022800131004     C                   else
022900131004     C                   seton                                        31
023000131004     C                   endif
023100131004     C*
023200131004     C                   ENDSR
023300131004
023400131004
023500131004
023600131004     C     chkRECFILMBR  BEGSR
023700131004     C*
023800131004     C     *start        setll     TIVAWWWT
023900131004     C                   if        %found(TIVAWWWT)
024000131004     C                   seton                                        31
024100131004     C                   endif
024200131004     C*
024300131004     C                   ENDSR
024400131004
024500131004
024600131004
024700131004     C     wriOUTPUT     BEGSR
024800131004     C*
024900131004     C                   write     TIVAW000
025000131004     C*
025100131004     C                   ENDSR
025200131004
025300130807
025400041015
025500041015     C     cartab        BEGSR
025600041015     C*
025700041015     C* Caricamento tipi anomalia C/A
025800041015     C     'TAD'         SETLL     tntbe01l
025900041015     C                   IF        %equal(tntbe01l)
026000041015     C     'TAD'         READE     tntbe01l
026100041015     C                   DOW       not %eof(tntbe01l)
026200041015     C                   EVAL      DTAD = tbeUNI
026300041015     C                   ADD       1             jTAD
026400041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
026500041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
026600041015     C     'TAD'         READE     tntbe01l
026700041015     C                   ENDDO
026800041015     C                   ENDIF
026900131104     C*
027000131104     C* Carico la mappatura dei codici status personalizzati cliente
027100131104     C                   z-add     1             jCAU
027200131104     C                   dow       jCAU <= %elem(WCAU) and
027300131104     C                                     WCAU(jCAU) <> *blanks
027400131104     C                   eval      DS_WCAU = WCAU(jCAU)
027500131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
027600131104     C                   eval      skCAU_CLI(jCAU) = dsCAU_CLI
027700160304     C                   eval      skCAU_OPZ(jCAU) = dsCAU_OPZ
027800131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
027900131104     C                   add       1             jCAU
028000131104     C                   enddo
028100131104     C*
028200131104     C                   z-add     1             jDCT
028300131104     C                   dow       jDCT <= %elem(WDCT) and
028400131104     C                                     WDCT(jDCT) <> *blanks
028500131104     C                   eval      DS_WDCT = WDCT(jDCT)
028600131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
028700131104     C                   eval      skDCT_CLI(jDCT) = dsDCT_CLI
028800131111     C                   add       1             jDCT
028900131104     C                   enddo
029000131104     C*
029100131104     C                   z-add     1             jTIS9
029200131104     C                   dow       jTIS9 <= %elem(WTIS9) and
029300131104     C                                      WTIS9(jTIS9) <> *blanks
029400131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
029500131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
029600131104     C                   eval      skTIS9_CLI(jTIS9) = dsTIS9_CLI
029700131104     C                   add       1             jTIS9
029800131104     C                   enddo
029900131104     C*
030000131104     C                   ENDSR
030100131104
030200131004
030300131004
030400131004     C     Char_Speciali BEGSR
030500131111     C*
030600131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
030700131111     C*
030800131111     C/EXEC SQL
030900131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
031000131111     C/END-EXEC
031100131111     C*
031200131111     C/EXEC SQL
031300131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
031400131111     C/END-EXEC
031500131111     C*
031600131111     C/EXEC SQL
031700131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
031800131111     C/END-EXEC
031900131111     C*
032000131111     C/EXEC SQL
032100131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
032200131111     C/END-EXEC
032300131111     C*
032400131111     C/EXEC SQL
032500131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
032600131111     C/END-EXEC
032700131004     C*
032800131004     C                   ENDSR
032900131004
033000131004
033100131004
033200131004     C     wriHeader     BEGSR
033300131004     C*
033400131004     C                   clear                   vawDTA
033500131010     C*
033600131004     C*  UNB -
033700131004     C                   eval      vawDTA = UNB_sgm
033800131004     C                              + %trim(w§UNB_ID)
033900131004     C                              +   piu
034000131004     C                              + %trim(currDate6inv)
034100131004     C                              +   diviso
034200131004     C                              + %trim(currHHMM)
034300131004     C                              +   piu
034400131004     C                              + %trim(
034500131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
034600131004     C                              +   apice
034700131004     C                   exsr      wriOUTPUT
034800131004     C*
034900131004     C                   ENDSR
035000131004
035100131004
035200131004
035300131004     C     wriFooter     BEGSR
035400131004     C*
035500131004     C                   clear                   vawDTA
035600131004     C*
035700131004     C*  UNZ -
035800131004     C                   eval      vawDTA = UNZ_sgm
035900131004     C                              + %trim(%editc(Conta_msg:'4'))
036000131004     C                              +   piu
036100131004     C                              + %trim(
036200131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
036300131004     C                              +   apice
036400131004     C                   exsr      wriOUTPUT
036500131004     C*
036600131004     C                   ENDSR
036700131004
036800030924
036900030924
037000991027     C     traduci       BEGSR
037100131004     C*
037200131004     C* Inizializzo i contatori a livello di transazione/messaggio
037300131004     C                   z-add     *zeros        Conta_msg
037400031103     C*
037500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
037600031103     C* di traduzione (trattasi d "post-traduzione")
037700031104     C                   EVAL      wKstbKSU = w§PPIKSU
037800031103     C                   EVAL      wKstbFTR = *blanks
037900110103     C     KEYstb03P     SETLL     tistb03l
038000110103     C                   IF        %equal(tistb03l)
038100110103     C     KEYstb03P     READE     tistb03l
038200991027     C*
038300031103     C* Ciclo x tuttii record del cliente da trasmettere
038400110103     C                   DOW       not %eof(tistb03l)
038500131004     C*
038600131004     C* Inizializzo i contatori a livello di segmento
038700131004     C                   z-add     *zeros        Conta_segm
038800080618     C*
038900130807     C                   SETON                                        50
039000031103     C*
039100031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
039200031103     C* deve restituire al cliente nel file tradotto
039300031103     C                   IF        stbFTR = *blanks                             * ridondante
039400110103     C*
039500110103     C* Gestisco solamente gli stati standard
039600160212     C***                IF        (stbTIS= '1' OR
039700160212     C***                          (stbTIS= '9' AND
039800160212     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
039900160212     C***                           (stbPRS= '1'                ))) AND
040000160212     C***                          (%subst(stbFOP:1:1)= 'O'  OR
040100160212     C***                          (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
040200160212     C                   IF        (
040300160212     C                             (stbTIS='1' AND stbPRS<>'7')      OR
040400160212     C                             (stbTIS='1' AND stbPRS ='7'
040500160212     C                                         AND stbCOS ='CONSOK') OR
040600160304     C                             (stbTIS='1' AND stbPRS ='8'
040700160304     C                                         AND stbCOS ='SPUNTNPG5') OR
040800160212     C                             (stbTIS='9' AND stbPRS ='1')
040900160212     C                             ) AND
041000160212     C                             (%subst(stbFOP:1:1)= 'O'  OR
041100160212     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9')
041200160212     C                             )
041300131004     C*
041400131004     C* Per "sicurezza" non invio stati senza data
041500131004     C                   IF        stbDAS > *zeros
041600110103     C*
041700150814     C*******
041800150814     C*
041900110103     C                   SETOFF                                       50
042000151005     C*
042100151005     C* Gestisco rottura di codice per spedizione
042200151005     C                   IF        stbKSU = wDepKSU AND
042300151005     C                             stbAAS = wDepAAS AND
042400151005     C                             stbLNP = wDepLNP AND
042500151005     C                             stbNRS = wDepNRS AND
042600151005     C                             stbNSP = wDepNSP
042700151005     C                   ELSE
042800151005     C*
042900151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
043000151005     C  N50              EXSR      DeAccorpa
043100151005     C*
043200151005     C* Salvo la nuova chiave di rottura
043300151005     C                   EVAL      wDepKSU = stbKSU
043400151005     C                   EVAL      wDepAAS = stbAAS
043500151005     C                   EVAL      wDepLNP = stbLNP
043600151005     C                   EVAL      wDepNRS = stbNRS
043700151005     C                   EVAL      wDepNSP = stbNSP
043800160107     C                   EVAL      wDepRMN = stbRMN
043900160107     C                   EVAL      wDepRMA = stbRMA
044000151005     C*
044100151005     C* Inizializzazioni per nuova chiave di rottura
044200151005     C                   CLEAR                   skCpyAccorpa
044300151005     C                   CLEAR                   jCpyAcc
044400151005     C*
044500151005     C                   ENDIF
044600160628     C*
044700160628     C                   SETON                                        22
044800031103     C*
044900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
045000031103     C     KEYtas30P     CHAIN     titas30c
045100031103     C                   IF        %found(titas30c)
045200160628     C                   ELSE
045300160628     C*
045400160628     C* Se non trovato TITAS => bolla ancora solo in partenza (FNBLP)
045500160628     C                   CLEAR                   TITAS000
045600160628     C     KEYtas30P     CHAIN     fnblp01l
045700160628     C                   IF        %found(fnblp01l)
045800160628     C*
045900160628     C* Overrido i campi di TITAS con quelli del FNBLP
046000160628     C                   EVAL      tasLNP = blpLNP
046100160628     C                   EVAL      tasLNA = blpLNA
046200160628     C                   EVAL      tasRSD = blpRSD
046300160628     C                   EVAL      tasLOD = blpLOD
046400160628     C                   EVAL      tasIND = blpIND
046500160628     C                   EVAL      tasPRD = blpPRD
046600160628     C                   EVAL      tasCAD = blpCAD
046700160628     C                   EVAL      tasPKB = blpPKB
046800160628     C                   EVAL      tasPKC = blpPKC
046900160628     C                   EVAL      tasNCL = blpNCL
047000160628     C                   ELSE
047100160628     C*
047200160628     C* Se non trovato neanche in BLP allora => bolla non trovata !!
047300160628     C                   SETOFF                                       22
047400160628     C                   ENDIF
047500160628     C                   ENDIF
047600160628     C*
047700160628     C* Se bolla tutto OK
047800160628     C                   IF        *in22
047900131010     C*
048000131010     C* Innanzitutto chaino la bolla direttamente su TITAS
048100131216     C                   MOVEL     *zeros        wPO               3 0
048200131213     C*
048300131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
048400131213     C                   if        tasDTI = *zeros
048500131213     C                   eval      wPO = tasLNP
048600131213     C                   else
048700131213     C                   eval      wPO = tasLNA
048800131213     C                   endif
048900131213     C*
049000131213     C     wPO           CHAIN     azorg01l
049100131010     C                   IF        %found(azorg01l)
049200080618     C*
049300110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
049400140606     C                   SETOFF                                       80
049500110103     C                   IF        stbTIS = '1'
049600031103     C                   Z-ADD     1             j
049700131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
049800131113     C                   IF        %found AND skCAU_CLI(j) <> 'NONINVIARE'
049900131104     C                   EVAL      wWrkSts = skCAU_CLI(j)
050000160304     C                   EVAL      wWrkOpz = skCAU_OPZ(j)
050100140514     C                   SETOFF                                       50
050200140606     C                   SETON                                        80
050300031103     C                   ELSE
050400031103     C                   SETON                                        50
050500031103     C                   ENDIF
050600110103     C                   ENDIF
050700110103     C*
050800110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
050900140606     C                   IF        not *IN80
051000140514     C***                IF        stbTIS <> '1'
051100110103     C                   Z-ADD     1             j
051200131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
051300131113     C                   IF        %found AND skTIS9_CLI(j) <> 'NONINVIARE'
051400131104     C                   EVAL      wWrkSts = skTIS9_CLI(j)
051500140514     C                   SETOFF                                       50
051600140606     C                   SETON                                        80
051700110103     C                   ELSE
051800110103     C                   SETON                                        50
051900110103     C                   ENDIF
052000140514     C***                ENDIF
052100140606     C                   ENDIF
052200041015     C*
052300080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
052400130807     C                   IF        stbPRS =  '5'
052500080618     C*
052600130807     C                   SETOFF                                       50
052700080618     C*
052800080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
052900130807     C                   Z-ADD     1             jTAD
053000130807     C                   MOVEL(P)  stbCOS        wTADCod
053100130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
053200130807     C                   IF        %found
053300130807     C                   Z-ADD     1             j
053400131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
053500130807     C                   IF        %found
053600131113     C                   IF        %trim(skDCT_CLI(j)) <> 'NONINVIARE'
053700131104     C                   EVAL      wWrkSts = skDCT_CLI(j)
053800140514     C                   SETOFF                                       50
053900131004     C                   ELSE
054000131004     C                   SETON                                        50
054100131004     C                   ENDIF
054200130807     C                   ELSE
054300130807     C                   SETON                                        50
054400130807     C                   ENDIF
054500130807     C                   ELSE
054600130807     C                   SETON                                        50
054700130807     C                   ENDIF
054800130807     C                   ENDIF
054900140308     C*
055000140308     C* Al primo dettaglio da inviare => scrivo anche testata
055100140313     C                   IF        not *IN50
055200140313     C  N45              EXSR      wriHeader
055300140308     C                   SETON                                        45
055400140313     C                   ENDIF
055500160304     C*
055600160304     C* Se ok per procedere
055700160304     C                   IF        not *IN50
055800160304     C*
055900160304     C* Inizializzo le schiere di work
056000160304     C                   CLEAR                   skDETT_RIF
056100160304     C                   CLEAR                   skDETT_DAT
056200160304     C                   CLEAR                   skDETT_ORA
056300160304     C*
056400160304     C* Verifico il tipo di opzione prevista per il codice stato corrente
056500160304     C                   IF        wWrkOpz = '*DETT'
056600160304     C                   EXSR      RTVDETT
056700160304     C                   ENDIF
056800160304     C*
056900160304     C* Scarico il buffer d output
057000160304     C                   EXSR      wriStato
057100160304     C                   ENDIF
057200110103     C*
057300131010     C                   ENDIF
057400160628 ==> C                   ENDIF                                                  * TITAS30C
057500160628     C*
057600110103     C                   ENDIF
057700110103     C                   ENDIF
057800110103     C                   ENDIF
057900031103     C*
058000031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
058100031103     C                   EVAL      stbFTR = 'T'
058200031103     C                   UPDATE    tistb000
058300991027     C*
058400110103     C     KEYstb03P     READE     tistb03l
058500031103     C                   SETOFF                                       50
058600030325     C                   ENDDO
058700151005     C*
058800151005     C* Eseguo routine di De-Accorpamento in uscita dal ciclo di principale
058900151005     C  N50              EXSR      DeAccorpa
059000151005     C*
059100031103     C                   ENDIF
059200991027     C*
059300030325     C                   EVAL      wrkesito = '0'
059400991027     C*
059500910830     C                   ENDSR
059600031103
059700031103
059800031103
059900131004     C     wriStato      BEGSR
060000131004     C*
060100131004     C* Incremento il contatore di messaggio (al interno della transazione)
060200131004     C                   eval      Conta_msg = Conta_msg + 1
060300131004     C*
060400131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
060500131104     C                   movel     *blanks       wAlfaRMA        512
060600131024     C                   movel     *blanks       wAlfaRSD        512
060700131004     C                   movel     *blanks       wAlfaLOD        512
060800131004     C                   movel     *blanks       wAlfaIND        512
060900131025     C                   movel     *blanks       wAlfaORGDES     512
061000131004     C* RMA
061100131004     C                   eval      campo_alfa = stbRMA
061200131004     C                   exsr      Char_Speciali
061300131004     C                   eval      wAlfaRMA = campo_alfa
061400131024     C* RSD
061500131024     C                   eval      campo_alfa = tasRSD
061600131024     C                   exsr      Char_Speciali
061700131024     C                   eval      wAlfaRSD = campo_alfa
061800131004     C* LOD
061900131004     C                   eval      campo_alfa = tasLOD
062000131004     C                   exsr      Char_Speciali
062100131004     C                   eval      wAlfaLOD = campo_alfa
062200131004     C* IND
062300131004     C                   eval      campo_alfa = tasIND
062400131004     C                   exsr      Char_Speciali
062500131004     C                   eval      wAlfaIND = campo_alfa
062600131025     C* ORGDES
062700131025     C                   eval      campo_alfa = orgDES
062800131025     C                   exsr      Char_Speciali
062900131025     C                   eval      wAlfaORGDES = campo_alfa
063000131004     C*
063100131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
063200131004     C* (ancora corretto ???)
063300131004     C                   z-add     *zeros        wPeso             7 1
063400131004     C                   if        tasPKB > tasPKC
063500131004     C                   eval      wPeso = tasPKB
063600131004     C                   else
063700131004     C                   eval      wPeso = tasPKC
063800131004     C                   endif
063900160304     C*
064000160304     C* Valorizzo la data e ora dello stato corrente
064100160304     C                   eval      wDataStato = stbDAS
064200160304     C                   eval      wOraStato  = stbORS*100
064300160304     C*
064400160304     C* Verifico se per lo stato corrrente sono presentei info anche a livello di collo
064500160304     C                   MOVEL(p)  stbRMN        wAlfaRMN         15
064600160304     C                   Z-ADD     1             j
064700160304     C     wAlfaRMN      LOOKUP    skDETT_RIF(j)                          55
064800160304     C                   IF        %found
064900160304     C                   eval      wDataStato =  skDETT_DAT(j)
065000160304     C                   eval      wOraStato  =  skDETT_ORA(j)
065100160304     C                   ENDIF
065200160304     C*
065300131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
065400131004     C*
065500131004     C*  UNH -
065600131004     C                   eval      Conta_segm  = Conta_segm  + 1
065700131007     C                   eval      vawDTA = UNH_sgm_1
065800131007     C                              + %trim(%editc(Conta_msg:'4'))
065900131007     C                              + UNH_sgm_2
066000131004     C                              +   apice
066100131004     C                   exsr      wriOUTPUT
066200150814     C                   eval      vawDTA_Acc = vawDTA
066300150814     C                   exsr      CpyAccorpa
066400131004     C*
066500131004     C*  BGM -
066600131004     C                   eval      Conta_segm  = Conta_segm  + 1
066700131004     C                   eval      vawDTA = BGM_sgm
066800131004     C                              + %trim(%editc(stbRMN:'4'))
066900131004     C                              +   apice
067000131004     C                   exsr      wriOUTPUT
067100150814     C                   eval      vawDTA_Acc = vawDTA
067200150814     C                   exsr      CpyAccorpa
067300131004     C*
067400131004     C*  NAD SF -
067500131008     C***                eval      Conta_segm  = Conta_segm  + 1
067600131008     C***                eval      vawDTA = NAD_SF_1
067700131008     C***                           + NAD_SF_2
067800131008     C***                           + NAD_SF_3
067900131008     C***                           + NAD_SF_4
068000131008     C***                           + NAD_SF_5
068100131008     C***                           +   apice
068200131008     C***                exsr      wriOUTPUT
068300150814     C***                eval      vawDTA_Acc = vawDTA
068400150814     C***                exsr      CpyAccorpa
068500131004     C*
068600131004     C*  NAD ST -
068700131004     C                   eval      Conta_segm  = Conta_segm  + 1
068800131004     C                   eval      vawDTA = NAD_ST
068900131025     C                              + %trim(wAlfaRSD)
069000131004     C                              +   piu
069100131004     C                              +   piu
069200131004     C                              + %trim(wAlfaIND)
069300131004     C                              +   piu
069400131004     C                              + %trim(wAlfaLOD)
069500131004     C                              +   piu
069600131004     C                              + %trim(tasPRD)
069700131004     C                              +   piu
069800131004     C                              + %trim(tasCAD)
069900131004     C                              +   piu
070000131004     C                              + 'IT'
070100131004     C                              +   apice
070200131004     C                   exsr      wriOUTPUT
070300150814     C                   eval      vawDTA_Acc = vawDTA
070400150814     C                   exsr      CpyAccorpa
070500131004     C*
070600131010     C*  LOC -
070700131004     C                   eval      Conta_segm  = Conta_segm  + 1
070800131004     C                   eval      vawDTA = LOC_sgm1
070900131010     C                              + %trim(%editc(orgFIL:'X'))
071000131004     C                              +   LOC_sgm2
071100131025     C                              + %trim(wAlfaORGDES)
071200131010     C                              +   LOC_sgm3
071300131004     C                              +   apice
071400131004     C                   exsr      wriOUTPUT
071500150814     C                   eval      vawDTA_Acc = vawDTA
071600150814     C                   exsr      CpyAccorpa
071700131004     C*
071800131004     C*  CNI -
071900131004     C                   eval      Conta_segm  = Conta_segm  + 1
072000131004     C                   eval      vawDTA = CNI_sgm
072100131004     C                              +   apice
072200131004     C                   exsr      wriOUTPUT
072300150814     C                   eval      vawDTA_Acc = vawDTA
072400150814     C                   exsr      CpyAccorpa
072500131004     C*
072600131004     C*  STS -
072700131004     C                   eval      Conta_segm  = Conta_segm  + 1
072800131004     C                   eval      vawDTA = STS_sgm
072900131004     C                              + %trim(wWrkSts)
073000131004     C                              +   apice
073100131004     C                   exsr      wriOUTPUT
073200150814     C                   eval      vawDTA_Acc = vawDTA
073300150814     C                   exsr      CpyAccorpa
073400131004     C*
073500131004     C*  RFF -
073600131004     C                   eval      Conta_segm  = Conta_segm  + 1
073700131004     C                   eval      vawDTA = RFF_sgm
073800131004     C                              + %trim(wAlfaRMA)
073900131004     C                              +   apice
074000131004     C                   exsr      wriOUTPUT
074100150814     C                   eval      vawDTA_Acc = vawDTA
074200150814     C                   exsr      CpyAccorpa
074300131004     C*
074400131004     C*  TDT -
074500131004     C                   eval      Conta_segm  = Conta_segm  + 1
074600131004     C                   eval      vawDTA = TDT_sgm
074700131004     C                              +   apice
074800131004     C                   exsr      wriOUTPUT
074900150814     C                   eval      vawDTA_Acc = vawDTA
075000150814     C                   exsr      CpyAccorpa
075100131004     C*
075200131004     C*  DTM -
075300131004     C                   eval      Conta_segm  = Conta_segm  + 1
075400131004     C                   eval      vawDTA = DTM_sgm
075500160304     C                              + %trim(%editc(wDataStato*1000000+
075600160304     C                                             wOraStato:'4'))
075700131004     C                              +   diviso
075800131007     C                              + '204'
075900131004     C                              +   apice
076000131004     C                   exsr      wriOUTPUT
076100150814     C                   eval      vawDTA_Acc = vawDTA
076200150814     C                   exsr      CpyAccorpa
076300131004     C*
076400131004     C*  EQD -
076500131004     C                   eval      Conta_segm  = Conta_segm  + 1
076600131004     C                   eval      vawDTA = EQD_sgm
076700131004     C                              +   apice
076800131004     C                   exsr      wriOUTPUT
076900150814     C                   eval      vawDTA_Acc = vawDTA
077000150814     C                   exsr      CpyAccorpa
077100131004     C*
077200131004     C*  MEA -
077300131004     C                   eval      Conta_segm  = Conta_segm  + 1
077400131004     C                   eval      vawDTA = MEA_sgm
077500131004     C                              + %trim(%editc(wPeso:'3'))
077600131004     C                              + '0'
077700131004     C                              +   apice
077800131004     C                   exsr      wriOUTPUT
077900150814     C                   eval      vawDTA_Acc = vawDTA
078000150814     C                   exsr      CpyAccorpa
078100160215     C*
078200160215     C*  GID -
078300160215     C                   eval      Conta_segm  = Conta_segm  + 1
078400160215     C                   eval      vawDTA = GID_sgm1
078500160215     C                              + %trim(%editc(tasNCL:'4'))
078600160215     C                              +   GID_sgm2
078700160215     C                              +   apice
078800160215     C                   exsr      wriOUTPUT
078900150814     C                   eval      vawDTA_Acc = vawDTA
079000150814     C                   exsr      CpyAccorpa
079100131004     C*
079200131004     C*  UNT -
079300131004     C                   eval      Conta_segm  = Conta_segm  + 1
079400131004     C                   eval      vawDTA = UNT_sgm
079500131004     C                              + %trim(%editc(Conta_segm:'4'))
079600131004     C                              +   piu
079700131007     C                              + %trim(%editc(Conta_msg:'4'))
079800131004     C                              +   apice
079900131004     C                   exsr      wriOUTPUT
080000150814     C                   eval      vawDTA_Acc = vawDTA
080100150814     C                   exsr      CpyAccorpa
080200031103     C*
080300031103     C                   ENDSR
080400150814
080500150814
080600150814
080700150814     C     CpyAccorpa    BEGSR
080800150814     C*
080900150814     C                   ADD       1             jCpyAcc
081000150814     C                   EVAL      skCpyAccorpa(jCpyAcc) = vawDTA_Acc
081100150814     C*
081200150814     C                   ENDSR
081300150814
081400150814
081500150814
081600150814     C     DeAccorpa     BEGSR
081700150814     C*
081800150814     C* Scorro tutte le spedizioni accorpate sulla spedizone corrente
081900150814     C     KEYrab01P     SETLL     firab01l
082000150814     C                   IF        %equal(firab01l)
082100150814     C     KEYrab01P     READE     firab01l
082200150814     C                   DOW       not %eof(firab01l)
082300150814     C*
082400150814     C* Controllo (ridondante) su riferimenti spedizione accorpante
082500160107     C                   IF        wDepRMN = rabRMN AND
082600160107     C                             wDepRMA = rabRMA
082700150814     C*
082800150814     C* Considero solamente spedizioni ACCORPATE
082900150814     C                   IF        rabRMN = rabRMNR AND
083000150814     C                             rabRMA = rabRMAR
083100150814     C                   ELSE
083200150814     C*
083300150814     C* Per ogni spedizione ACCORPATA "duplico" la spedizione ACCORPANTE
083400150814     C                   Z-ADD     1             jCpyAcc
083500150814     C                   DOW       jCpyAcc <= %elem(skCpyAccorpa) AND
083600150814     C                             skCpyAccorpa(jCpyAcc) <> *blanks
083700150814     C*
083800150814     C* Incremento il contatore dei segmenti
083900150814     C                   eval      Conta_segm  = Conta_segm  + 1
084000150814     C*
084100150814     C* Effettuo sostuituzione di valori rispetto bolla ACCORPANTE e sue ACCORPATE
084200151005     C                   SELECT
084300150814     C*
084400150814     C* UNH (per contatori segmento / messaggio)
084500151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNH_sgm_1
084600150814     C                             )) = UNH_sgm_1
084700150814     C                   eval      Conta_msg = Conta_msg + 1
084800150814     C                   z-add     1             Conta_segm
084900150814     C                   eval      vawDTA = UNH_sgm_1
085000150814     C                              + %trim(%editc(Conta_msg:'4'))
085100150814     C                              + UNH_sgm_2
085200150814     C                              +   apice
085300150814     C*
085400150814     C* BGM (per RMN)
085500151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(BGM_sgm))
085600150814     C                             = BGM_sgm
085700150814     C                   eval      vawDTA = BGM_sgm
085800150814     C                              + %trim(%editc(rabRMNR:'4'))
085900150814     C                              +   apice
086000150814     C*
086100150814     C* RFF (per RMA)
086200151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(RFF_sgm))
086300150814     C                             = RFF_sgm
086400150814     C                   eval      campo_alfa = rabRMAR
086500150814     C                   exsr      Char_Speciali
086600150814     C                   eval      wAlfaRMA = campo_alfa
086700150814     C                   eval      vawDTA = RFF_sgm
086800150814     C                              + %trim(wAlfaRMA)
086900150814     C                              +   apice
087000150814     C*
087100150814     C* UNT (per contatori segmento / messaggio)
087200151005     C                   WHEN      %subst(skCpyAccorpa(jCpyAcc):1:%len(UNT_sgm))
087300150814     C                             = UNT_sgm
087400150814     C                   eval      vawDTA = UNT_sgm
087500150814     C                              + %trim(%editc(Conta_segm:'4'))
087600150814     C                              +   piu
087700150814     C                              + %trim(%editc(Conta_msg:'4'))
087800150814     C                              +   apice
087900151005     C                   OTHER
088000151005     C*
088100151005     C* Tutti gli altri segmenti "AS IS"
088200151005     C                   eval      vawDTA = skCpyAccorpa(jCpyAcc)
088300151005     C                   ENDSL
088400150814     C*
088500150814     C* Scarico l'ouput di "duplicazione"
088600150814     C                   exsr      wriOUTPUT
088700150814     C*
088800150814     C                   ADD       1             jCpyAcc
088900150814     C                   ENDDO
089000150814     C*
089100150814     C                   ENDIF
089200150814     C                   ENDIF
089300150814     C*
089400150814     C     KEYrab01P     READE     firab01l
089500150814     C                   ENDDO
089600150814     C                   ENDIF
089700150814     C*
089800150814     C                   ENDSR
089900160304
090000160304
090100160304
090200160304     C     RTVDETT       BEGSR
090300160304     C*
090400160304     C* Leggo tutti i dettagli ancora da elaborare per la testata di TISTB corrente
090500160304     C                   Z-ADD     *zeros        jDETT
090600160304     C     KEYstbd3P     SETLL     tistbd3i
090700160304     C                   IF        %equal(tistbd3i)
090800160304     C     KEYstbd3P     READE     tistbd3i
090900160304     C                   DOW       not %eof(tistbd3i)
091000160304     C* Filtro unicamente i segnacolli "BRT"
091100160304     C                   IF        stbdTIS = 'BRT'
091200160304     C                   ADD       1             jDETT
091300160304     C                   EVAL      DSTBDNPG5 = STBDDTA
091400160304     C                   EVAL      skDETT_RIF(jDETT) = §STBDNPG51
091500160304     C                   EVAL      skDETT_DAT(jDETT) = stbdDAD
091600160304     C                   EVAL      skDETT_ORA(jDETT) = stbdORD
091700160304     C                   ENDIF
091800160304     C     KEYstbd3P     READE     tistbd3i
091900160304     C                   ENDDO
092000160304     C                   ENDIF
092100160304     C*
092200160304     C                   ENDSR
092300031103
092400031103
092500991027
092600991027      /TITLE Operazioni iniziali.
092700991027     C     *inzsr        BEGSR
092800991027     C*
092900991027     C     *ENTRY        PLIST
093000031103     C                   parm                    prmppi
093100991027     C     wrkesito      parm      wrkesito      prmesito
093200031103     C*
093300031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
093400031103     C                   EVAL      DSPPI = prmppi
093500131004     C*
093600131004     C* Reperisco i valori variabili ma a livello di transazione corrente
093700131004     C* (tipicamente data e ora relativ al file generato)
093800131004     C                   eval      currDate6    =
093900131004     C                              UBFMTDATE_Convert(
094000131004     C                                  %editc(%dec(%date():*ISO):'X') :
094100131004     C                                  'YYYYMMDD' : 'YYMMDD')
094200131004     C                   eval      currDate6inv =
094300131004     C                              UBFMTDATE_Convert(
094400131004     C                                  %editc(%dec(%date():*ISO):'X') :
094500131004     C                                  'YYYYMMDD' : 'DDMMYY')
094600131004     C                   eval      currHHMM =
094700131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
094800030709     C*
094900030709     C* Definizione chiavi
095000030924     C*
095100110103     C* Chiave su TISTB03L - Parziale
095200110103     C     KEYstb03P     KLIST
095300031104     C                   KFLD                    wKstbKSU
095400031103     C                   KFLD                    wKstbFTR
095500160304     C*
095600160304     C* Chiave su TISTBD3I - Parziale
095700160304     C     KEYstbd3P     KLIST
095800160304     C                   KFLD                    wKstbFTR
095900160304     C                   KFLD                    stbKSU
096000160304     C                   KFLD                    stbAAS
096100160304     C                   KFLD                    stbLNP
096200160304     C                   KFLD                    stbNRS
096300160304     C                   KFLD                    stbNSP
096400160304     C                   KFLD                    stbDAS
096500160304     C                   KFLD                    stbORS
096600160304     C                   KFLD                    stbCOS
096700031103     C*
096800031103     C* Chiave su TITAS30C - Parziale
096900031103     C     KEYtas30P     KLIST
097000031103     C                   KFLD                    stbAAS
097100031103     C                   KFLD                    stbLNP
097200031103     C                   KFLD                    stbNRS
097300031103     C                   KFLD                    stbNSP
097400150814     C*
097500150814     C* Chiave su FIRAB01L - Parziale
097600150814     C     KEYrab01P     KLIST
097700150814     C                   KFLD                    wDepAAS
097800150814     C                   KFLD                    wDepLNP
097900150814     C                   KFLD                    wDepNRS
098000150814     C                   KFLD                    wDepNSP
098100991027     C*
098200991027     C                   ENDSR
098300131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
098400131104A              6              L.AVVISO                                                     1
098500131125ARS            6              AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
098600131104AVV            6              LASCIATO AVVISO 1GG                                          3
098700131113AV2            210            LASCIATO AVVISO 2GG                                          4
098800131113AV3            210            LASCIATO AVVISO 3GG                                          5
098900131113AV5            210            LASCIATO AVVISO 5GG                                          6
099000131113AV7            210            LASCIATO AVVISO 7GG                                          7
099100131113AV9            210            LASCIATO AVVISO 9 GIORNI                                     8
099200131125A16            6              L.A. RIMANDA LO SVINCOLO                                     9 ex 32
099300131104A23            7              L.A. CHIUSO FINO AL                                         10
099400131113CPO            NONINVIARE     CAMBIO PORTO                                                11
099500131104DAN            14             COLLO/I DANNEGGIATO/I                                       12
099600131125DIR            30             DIROTTAMENTO                                                13
099700131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
099800131125G              6              GIACENZA GENERICA TEMPORANEA                                15 ex 32
099900131125GEN            6              GIACENZA GENERICA                                           16 ex 32
100000131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
100100131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
100200131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
100300131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
100400131104IDD            14             CHIUSURA SPEDIZIONE CON IDD                                 21
100500131113MAN            33             COLLO/I MANCANTE/I                                          22
100600131104MIC            209            MESSA IN CONSEGNA                                           23
100700131113N              NONINVIARE     NON FATTA                                                   24
100800131104NAV            14             COLLO SCONDIZIONATO                                         25
100900131113NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
101000131104P              96             CONSEGNA PARZIALE                                           27
101100131113PAT            631            FESTIVITA PATRONALE                                         28
101200131104R              215            RISERVA                                                     29
101300131113RDC            NONINVIARE     RIPRISTINO                                                  30
101400131113RFD            49             RIFIUTATO                                                   31
101500131113RFM            49             RIFIUTATO                                                   32
101600131122RIC            6              RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
101700131113RIP            NONINVIARE     RIPRISTINO                                                  34
101800131104RS             215            RISERVA                                                     35
101900131104T              7              CHIUSO PER TURNO                                            36
102000131113WA             33             AMMANCO SENZA EVENTO                                        37
102100131113WAE            33             AMMANCO CON EVENTO                                          38
102200131113WM             33             MANCANZA SENZA EVENTO                                       39
102300131113WME            33             MANCANZA CON EVENTO                                         40
102400131113WV             640            AVARIA SENZA EVENTO                                         41
102500131113WVE            640            AVARIA CON EVENTO                                           42
102600131113ZAL            63             ALLUVIONE                                                   43
102700131113ZBS            31             BLOCCO STRADALE                                             44
102800131113ZFR            631            FESTIVITÀ REGIONALE                                         45
102900131113ZMM            63             MARE MOLTO MOSSO                                            46
103000131113ZMP            31             MANIFESTAZIONE PUBBLICA                                     47
103100131113ZNV            63             NEVICATA ECCEZIONALE                                        48
103200131113ZSC            31             SCIOPERO                                                    49
103300131104001            49             RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
103400131104002            206            MERCE NON ORDINATA O NON CONFORME                           51
103500131104003            205            MERCE SPEDITA IN RITARDO                                    52
103600131104004            206            MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
103700131104005            205            MERCE SPEDITA CON TROPPO ANTICIPO                           54
103800131104006            49             MERCE RESA SENZA AUTORIZZAZIONE                             55
103900131104007            206            DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
104000131104008            204            DESTINATARIO NON PAGA IL C/ASSEGNO                          57
104100131125009            6              CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
104200131104010            204            DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
104300131104012            204            IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
104400131104013            204            DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
104500131113014            204            DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
104600131125016            6              DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
104700131104017            6              DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
104800131125018            6              DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
104900131113019            7              DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
105000131113020            21             DESTINATARIO RISULTA TRASFERITO                             67
105100131125021            30             DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
105200131104022            30             INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
105300131104023            7              IL DESTINATARIO E' CHIUSO FINO AL                           70
105400131104024            631            IL DESTINATARIO E' CHIUSO PER FERIE                         71
105500131104025            7              IL DESTINATARIO E' CHIUSO                                   72
105600131113026            91             IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
105700131104027            206            DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
105800131104028            7              ESERCIZIO NON ANCORA IN ATTIVITA                            75
105900131104029            204            IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
106000131104030            49             IL DESTINATARIO RIFIUTA LA MERCE                            77
106100131104031            49             IL DESTINATARIO NON INTENDE                                 78
106200131125032            6              FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
106300131125033            6              IL DESTINATARIO RICHIEDE                                    80 ex 32
106400131113034            31             SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
106500131104035            37             DOCUMENTI INCOMPLETI O MANCANTI                             82
106600131104036            49             IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
106700131104037            49             RIFIUTA LA CONSEGNA TASSATIVA                               84
106800131104100            49             -                                                           85
106900131104101            49             -                                                           86
107000131113999            NONINVIARE     -                                                           87
107100160304CONSOK         725            CONSEGNA OK                                                 88
107200131104RESO           640            RESO                                                        89
107300160304SPUNTNPG5      302       *DETTSPUNTA ENTRATA (5) DETTAGLI COLLI                           90
107400131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
107500131113A              33             AMMANCO                                                        1
107600131113M              33             MANCANZA                                                       2
107700131104V              14             AVARIA                                                         3
107800131113E              640            EVENTO AVVERSO                                                 4
107900131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
108000160304INCARICO1      602                                                                           1
108100160304INCARICO2      602                                                                           2
108200160304CONSOK1        725                                                                           3
108300160304CONSOK2        725                                                                           4
108400131113GIACNODIS1     NONINVIARE                                                                    5
108500131113GIACNODIS2     NONINVIARE                                                                    6
108600131113GIACSIDIS1     NONINVIARE                                                                    7
108700131113GIACSIDIS2     NONINVIARE                                                                    8
108800140514COLNOTAFF      417                                                                           9
108900131113               NONINVIARE                                                                    10
