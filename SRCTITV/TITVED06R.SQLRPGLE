000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500150113     FTIVIRE1L  IF   E           K DISK
000600041015     FTNTBE01L  IF   E           K DISK
000700031103     FTITAS30C  IF   E           K DISK
000800110103     FTISTB03L  UF   E           K DISK    commit
000900090323     FTIVAWWWT  UF A E             DISK    commit
001000031103
001100031103     D*-----------
001200031103     D* SCHIERE A PROGRAMMA
001300031103     D*-----------
001400131104     D WCAU            S             90    DIM(90) CTDATA PERRCD(1)
001500131104     D WDCT            S             30    DIM(4)  CTDATA PERRCD(1)
001600141110     D WTIS9           S             30    DIM(12) CTDATA PERRCD(1)
001700131104
001800131104
001900131104     D*-----------
002000131104     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
002100131104     D*-----------
002200131104     D DS_WCAU         DS
002300131104     D  dsCAU_BRT                    15
002400141006     D  dsCAU_EVT                    10
002500141006     D  dsCAU_RSN                     5
002600131104     D  dsCAU_DES                    60
002700131104     D*
002800131104     D DS_WDCT         DS
002900131104     D  dsDCT_BRT                    15
003000141007     D  dsDCT_EVT                    10
003100141007     D  dsDCT_RSN                     5
003200131104     D*
003300131104     D DS_WTIS9        DS
003400131104     D  dsTIS9_BRT                   15
003500141007     D  dsTIS9_EVT                   10
003600141007     D  dsTIS9_RSN                    5
003700131104
003800131104
003900041015     D*-----------
004000041015     D* SCHIERE
004100041015     D*-----------
004200131104     D skCAU_BRT       S             10    DIM(%elem(WCAU))
004300141006     D skCAU_EVT       S             10    DIM(%elem(WCAU))
004400141007     D skCAU_RSN       S              5    DIM(%elem(WCAU))
004500131104     D skCAU_DES       S             60    DIM(%elem(WCAU))
004600131104     D jCAU            S              3  0 INZ(*zeros)
004700131104     D skDCT_BRT       S              1    DIM(%elem(WDCT))
004800141007     D skDCT_EVT       S             10    DIM(%elem(WDCT))
004900141007     D skDCT_RSN       S              5    DIM(%elem(WDCT))
005000131104     D jDCT            S              3  0 INZ(*zeros)
005100131104     D skTIS9_BRT      S             10    DIM(%elem(WTIS9))
005200141007     D skTIS9_EVT      S             10    DIM(%elem(WTIS9))
005300141007     D skTIS9_RSN      S              5    DIM(%elem(WTIS9))
005400131104     D jTIS9           S              3  0 INZ(*zeros)
005500131104     D skTADCod        S              2    DIM(100)
005600131104     D skTADRag        S              1    DIM(100)
005700041015     D jTAD            S              3  0 INZ(*zeros)
005800150113     D*
005900150113     D w§UNB_ID        S            128    INZ(*zeros)
006000150113     D w§NAD_SF        S            256    INZ(*zeros)
006100150113
006200150113
006300150113     D*------------------
006400150113     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006500150113     D*------------------
006600150113     D InStringa       S          65535A   VARYING                              (stringa input)
006700150113     D InSepara        S             10A                                        (separatore)
006800150113     D InTypeOut       S              1A                                        (tipo output)
006900150113     D wSkParam        S          65535A                                        (output)
007000150113     D OutErrore       S              1A                                        (flag errore)
007100150113     D OutMsg          S             80A                                        (messaggio errore)
007200150113     D SkSplitCSV_6    S            512    DIM(128)
007300150113
007400030924
007500041015     D*-----------
007600041015     D* VARIABILI D WRK
007700041015     D*-----------
007800031103     D prmppi          S             50
007900030924     D prmesito        S              1
008000030924     D wrkesito        S                   like(prmesito)
008100031103     D j               S              2  0
008200130807     D wTADCod         S              2
008300131004     D campo_alfa      S            512
008400141007     D wStsEvt         S             25
008500141007     D wStsRsn         S             25
008600131004     D currDate6       S              6
008700131004     D currDate6inv    S              6
008800131004     D currHHMM        S              4
008900131216     D Conta_msg       S             10i 0
009000131216     D Conta_segm      S             10i 0
009100131004
009200131004
009300131004     D*-----------
009400131004     D* DEFINIZIONE DS ESTERNE
009500131004     D*-----------
009600131004     D trul33ds      e ds                  inz
009700131004     D kpjba         e ds                  inz
009800131004     D DTAD          e ds                  inz
009900150113     D DVIREPE       e ds                  inz
010000031103
010100131004
010200131004     D*------------------
010300131004     D* LINKING A DEFINIZIONI ESTERNE
010400131004     D*------------------
010500131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
010600131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
010700131004
010800131004
010900031103     D*-----------
011000031103     D* VARIABILI RIFERITE AL DATA-BASE
011100031103     D*-----------
011200080618     D wKstbKSU        S                   like(stbKSU)
011300080618     D wKstbFTR        S                   like(stbFTR)
011400110705
011500031103
011600031103     D*-----------
011700031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
011800031103     D*-----------
011900150114     D DSPPI           DS
012000150114     D  w§PPIKSU                           like(vireKSC) inz
012100150114     D  w§PPITIP                           like(vireTIP) inz
012200150114     D  w§PPIDTI                      8                  inz
012300131004
012400131004
012500131004     D*-----------
012600131004     D* COSTANTI / DEFAULT
012700131004     D*-----------
012800150113     D UNB_sgm         C                   CONST('UNB+UNOA:1+IT-BRT001:ZZ+')
012900131007     D UNH_sgm_1       C                   CONST('UNH+')
013000150615     D UNH_sgm_2       C                   CONST('+IFTSTA:D:94A:UN:EAN008')
013100131004     D BGM_sgm         C                   CONST('BGM+23+')
013200150113     D NAD_SF          C                   CONST('NAD+SF+')
013300150113     D*NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
013400150113     D*NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
013500150113     D*NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
013600150113     D*NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
013700150113     D*NAD_SF_5        C                   CONST('NNI++29015+IT')
013800131004     D NAD_ST          C                   CONST('NAD+ST++')
013900131010     D LOC_sgm1        C                   CONST('LOC+175+')
014000131010     D LOC_sgm2        C                   CONST('::3:')
014100131010     D LOC_sgm3        C                   CONST('+IT')
014200131004     D CNI_sgm         C                   CONST('CNI+1')
014300141006     D STS_sgm         C                   CONST('STS+')
014400131004     D RFF_sgm         C                   CONST('RFF+POR:')
014500150113     D TDT_sgm         C                   CONST('TDT+30+1+++IT-BRT001')
014600131004     D DTM_sgm         C                   CONST('DTM+ZZZ:')
014700131004     D EQD_sgm         C                   CONST('EQD+AA')
014800131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
014900160215     D*GID_sgm         C                   CONST('GID+1+1:CTN')
015000160215     D GID_sgm1        C                   CONST('GID+1+')
015100160215     D GID_sgm2        C                   CONST(':CTN')
015200131004     D UNT_sgm         C                   CONST('UNT+')
015300131004     D UNZ_sgm         C                   CONST('UNZ+')
015400131004     D piu             C                   CONST('+')
015500131004     D diviso          C                   CONST(':')
015600131004     D apice           C                   CONST('''')
015700141006     D wStsTyp         C                   CONST('')
015800131004
015900030924
016000030325
016100030924     C* MAIN/
016200170123     C*
016300170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
016400170123     C
016500170123     C/EXEC SQL
016600170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016700170123     C/END-EXEC
016800041015     C*
016900041015     C* Carica i dati tabellati
017000041015     C                   EXSR      cartab
017100140308     C*
017200140308     C* Spia dati da inviare
017300140308     C                   SETOFF                                       45
017400041015     C*
017500041015     C* Esegue traduzione
017600140308     C                   SETOFF                                       31
017700150113     C  N31              EXSR      SPLIT_PARMS
017800131004     C  N31              EXSR      chkRECFILMBR
017900131004     C  N31              EXSR      rtvID
018000140308     C***  N31              EXSR      wriHeader
018100131004     C  N31              EXSR      traduci
018200140308     C  N31
018300140308     CAN 45              EXSR      wriFooter
018400140308 xxx C***   31              EXSR      sndAlert
018500110705     C*
018600921023     C                   SETON                                        LR
018700131004
018800131004
018900131004
019000131004     C*----------------------------------------------------*
019100131004     C*  STACCO ID UNIVOCO
019200131004     C*----------------------------------------------------*
019300131004     C     RTVID         BEGSR
019400131004     C*
019500131004     C                   z-add     *zeros        currUNIQ_ID      15 0
019600131004     C*
019700131004     C* NSP => Stacco un numeratore da AZNUM
019800131004     C                   clear                   TRUL33DS
019900131004     C                   eval      I33OPE = *zeros
020000131004     C                   eval      I33CNU = 302
020100131004     C                   eval      I33NUM = 1
020200131004     C                   movel     TRUL33DS      KPJBU
020300131004     C                   call      'TRUL33R'
020400131004     C                   parm                    KPJBA
020500131004     C                   movel     KPJBU         TRUL33DS
020600131004     C                   if        O33ERR = *zeros
020700131004     C                   z-add     O33NRF        currUNIQ_ID
020800131004     C                   else
020900131004     C                   seton                                        31
021000131004     C                   endif
021100131004     C*
021200131004     C                   ENDSR
021300131004
021400131004
021500131004
021600131004     C     chkRECFILMBR  BEGSR
021700131004     C*
021800131004     C     *start        setll     TIVAWWWT
021900131004     C                   if        %found(TIVAWWWT)
022000131004     C                   seton                                        31
022100131004     C                   endif
022200131004     C*
022300131004     C                   ENDSR
022400131004
022500131004
022600131004
022700131004     C     wriOUTPUT     BEGSR
022800131004     C*
022900131004     C                   write     TIVAW000
023000131004     C*
023100131004     C                   ENDSR
023200131004
023300130807
023400041015
023500041015     C     cartab        BEGSR
023600041015     C*
023700041015     C* Caricamento tipi anomalia C/A
023800041015     C     'TAD'         SETLL     tntbe01l
023900041015     C                   IF        %equal(tntbe01l)
024000041015     C     'TAD'         READE     tntbe01l
024100041015     C                   DOW       not %eof(tntbe01l)
024200041015     C                   EVAL      DTAD = tbeUNI
024300041015     C                   ADD       1             jTAD
024400041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
024500041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
024600041015     C     'TAD'         READE     tntbe01l
024700041015     C                   ENDDO
024800041015     C                   ENDIF
024900131104     C*
025000131104     C* Carico la mappatura dei codici status personalizzati cliente
025100131104     C                   z-add     1             jCAU
025200131104     C                   dow       jCAU <= %elem(WCAU) and
025300131104     C                                     WCAU(jCAU) <> *blanks
025400131104     C                   eval      DS_WCAU = WCAU(jCAU)
025500131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
025600141006     C                   eval      skCAU_EVT(jCAU) = dsCAU_EVT
025700141006     C                   eval      skCAU_RSN(jCAU) = dsCAU_RSN
025800131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
025900131104     C                   add       1             jCAU
026000131104     C                   enddo
026100131104     C*
026200131104     C                   z-add     1             jDCT
026300131104     C                   dow       jDCT <= %elem(WDCT) and
026400131104     C                                     WDCT(jDCT) <> *blanks
026500131104     C                   eval      DS_WDCT = WDCT(jDCT)
026600131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
026700141007     C                   eval      skDCT_EVT(jDCT) = dsDCT_EVT
026800141007     C                   eval      skDCT_RSN(jDCT) = dsDCT_RSN
026900131111     C                   add       1             jDCT
027000131104     C                   enddo
027100131104     C*
027200131104     C                   z-add     1             jTIS9
027300131104     C                   dow       jTIS9 <= %elem(WTIS9) and
027400131104     C                                      WTIS9(jTIS9) <> *blanks
027500131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
027600131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
027700141007     C                   eval      skTIS9_EVT(jTIS9) = dsTIS9_EVT
027800141007     C                   eval      skTIS9_RSN(jTIS9) = dsTIS9_RSN
027900131104     C                   add       1             jTIS9
028000131104     C                   enddo
028100131104     C*
028200131104     C                   ENDSR
028300131104
028400131004
028500131004
028600131004     C     Char_Speciali BEGSR
028700131111     C*
028800131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
028900131111     C*
029000131111     C/EXEC SQL
029100131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
029200131111     C/END-EXEC
029300131111     C*
029400131111     C/EXEC SQL
029500131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
029600131111     C/END-EXEC
029700131111     C*
029800131111     C/EXEC SQL
029900131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
030000131111     C/END-EXEC
030100131111     C*
030200131111     C/EXEC SQL
030300131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
030400131111     C/END-EXEC
030500131111     C*
030600131111     C/EXEC SQL
030700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
030800131111     C/END-EXEC
030900131004     C*
031000131004     C                   ENDSR
031100131004
031200131004
031300150113
031400150113     C     wriHeader     BEGSR
031500131004     C*
031600131004     C                   clear                   vawDTA
031700131010     C*
031800131004     C*  UNB -
031900131004     C                   eval      vawDTA = UNB_sgm
032000150113     C                              + %trim(w§UNB_ID)
032100131004     C                              +   piu
032200131004     C                              + %trim(currDate6inv)
032300131004     C                              +   diviso
032400131004     C                              + %trim(currHHMM)
032500131004     C                              +   piu
032600131004     C                              + %trim(
032700131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
032800131004     C                              +   apice
032900131004     C                   exsr      wriOUTPUT
033000131004     C*
033100131004     C                   ENDSR
033200131004
033300131004
033400131004
033500131004     C     wriFooter     BEGSR
033600131004     C*
033700131004     C                   clear                   vawDTA
033800131004     C*
033900131004     C*  UNZ -
034000131004     C                   eval      vawDTA = UNZ_sgm
034100131004     C                              + %trim(%editc(Conta_msg:'4'))
034200131004     C                              +   piu
034300131004     C                              + %trim(
034400131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
034500131004     C                              +   apice
034600131004     C                   exsr      wriOUTPUT
034700131004     C*
034800131004     C                   ENDSR
034900131004
035000030924
035100030924
035200991027     C     traduci       BEGSR
035300131004     C*
035400131004     C* Inizializzo i contatori a livello di transazione/messaggio
035500131004     C                   z-add     *zeros        Conta_msg
035600031103     C*
035700031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
035800031103     C* di traduzione (trattasi d "post-traduzione")
035900150113     C                   MOVE(P)   w§PPIKSU      wKstbKSU
036000031103     C                   EVAL      wKstbFTR = *blanks
036100110103     C     KEYstb03P     SETLL     tistb03l
036200110103     C                   IF        %equal(tistb03l)
036300110103     C     KEYstb03P     READE     tistb03l
036400991027     C*
036500031103     C* Ciclo x tuttii record del cliente da trasmettere
036600110103     C                   DOW       not %eof(tistb03l)
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
037800130808     C                   IF        (stbTIS= '1' OR
037900130906     C                             (stbTIS= '9' AND
038000151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
038100151117     C                              (stbPRS= '1'                ))) AND
038200130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
038300130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
038400131004     C*
038500131004     C* Per "sicurezza" non invio stati senza data
038600131004     C                   IF        stbDAS > *zeros
038700110103     C*
038800110103     C                   SETOFF                                       50
038900031103     C*
039000031103     C* Innanzitutto chaino la bolla direttamente su TITAS
039100031103     C     KEYtas30P     CHAIN     titas30c
039200031103     C                   IF        %found(titas30c)
039300131010     C*
039400131010     C* Innanzitutto chaino la bolla direttamente su TITAS
039500131216     C                   MOVEL     *zeros        wPO               3 0
039600131213     C*
039700131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
039800131213     C                   if        tasDTI = *zeros
039900131213     C                   eval      wPO = tasLNP
040000131213     C                   else
040100131213     C                   eval      wPO = tasLNA
040200131213     C                   endif
040300131213     C*
040400131213     C     wPO           CHAIN     azorg01l
040500131010     C                   IF        %found(azorg01l)
040600080618     C*
040700110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
040800140606     C                   SETOFF                                       80
040900110103     C                   IF        stbTIS = '1'
041000031103     C                   Z-ADD     1             j
041100131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
041200141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
041300141006     C                   EVAL      wStsEvt = skCAU_EVT(j)
041400141006     C                   EVAL      wStsRsn = skCAU_RSN(j)
041500140514     C                   SETOFF                                       50
041600140606     C                   SETON                                        80
041700031103     C                   ELSE
041800031103     C                   SETON                                        50
041900031103     C                   ENDIF
042000110103     C                   ENDIF
042100110103     C*
042200110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
042300140606     C                   IF        not *IN80
042400140514     C***                IF        stbTIS <> '1'
042500110103     C                   Z-ADD     1             j
042600131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
042700141007     C                   IF        %found AND skTIS9_EVT(j) <> 'NONINVIARE'
042800141007     C                   EVAL      wStsEvt = skTIS9_EVT(j)
042900141007     C                   EVAL      wStsRsn = skTIS9_RSN(j)
043000140514     C                   SETOFF                                       50
043100140606     C                   SETON                                        80
043200110103     C                   ELSE
043300110103     C                   SETON                                        50
043400110103     C                   ENDIF
043500140514     C***                ENDIF
043600140606     C                   ENDIF
043700041015     C*
043800080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
043900130807     C                   IF        stbPRS =  '5'
044000080618     C*
044100130807     C                   SETOFF                                       50
044200080618     C*
044300080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
044400130807     C                   Z-ADD     1             jTAD
044500130807     C                   MOVEL(P)  stbCOS        wTADCod
044600130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
044700130807     C                   IF        %found
044800130807     C                   Z-ADD     1             j
044900131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
045000130807     C                   IF        %found
045100141007     C                   IF        %trim(skDCT_EVT(j)) <> 'NONINVIARE'
045200141007     C                   EVAL      wStsEvt = skDCT_EVT(j)
045300141007     C                   EVAL      wStsRsn = skDCT_RSN(j)
045400140514     C                   SETOFF                                       50
045500131004     C                   ELSE
045600131004     C                   SETON                                        50
045700131004     C                   ENDIF
045800130807     C                   ELSE
045900130807     C                   SETON                                        50
046000130807     C                   ENDIF
046100130807     C                   ELSE
046200130807     C                   SETON                                        50
046300130807     C                   ENDIF
046400130807     C                   ENDIF
046500140308     C*
046600140308     C* Al primo dettaglio da inviare => scrivo anche testata
046700140313     C                   IF        not *IN50
046800140313     C  N45              EXSR      wriHeader
046900140308     C                   SETON                                        45
047000140313     C                   ENDIF
047100080618     C*
047200080618     C* Scarico il buffer d output
047300131004     C  N50              EXSR      wriStato
047400110103     C*
047500131010     C                   ENDIF
047600131004     C                   ENDIF
047700110103     C                   ENDIF
047800110103     C                   ENDIF
047900110103     C                   ENDIF
048000031103     C*
048100031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
048200031103     C                   EVAL      stbFTR = 'T'
048300031103     C                   UPDATE    tistb000
048400991027     C*
048500110103     C     KEYstb03P     READE     tistb03l
048600031103     C                   SETOFF                                       50
048700030325     C                   ENDDO
048800031103     C                   ENDIF
048900991027     C*
049000030325     C                   EVAL      wrkesito = '0'
049100991027     C*
049200910830     C                   ENDSR
049300031103
049400031103
049500031103
049600131004     C     wriStato      BEGSR
049700131004     C*
049800131004     C* Incremento il contatore di messaggio (al interno della transazione)
049900131004     C                   eval      Conta_msg = Conta_msg + 1
050000131004     C*
050100131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
050200131104     C                   movel     *blanks       wAlfaRMA        512
050300131024     C                   movel     *blanks       wAlfaRSD        512
050400131004     C                   movel     *blanks       wAlfaLOD        512
050500131004     C                   movel     *blanks       wAlfaIND        512
050600131025     C                   movel     *blanks       wAlfaORGDES     512
050700131004     C* RMA
050800131004     C                   eval      campo_alfa = stbRMA
050900131004     C                   exsr      Char_Speciali
051000131004     C                   eval      wAlfaRMA = campo_alfa
051100131024     C* RSD
051200131024     C                   eval      campo_alfa = tasRSD
051300131024     C                   exsr      Char_Speciali
051400131024     C                   eval      wAlfaRSD = campo_alfa
051500131004     C* LOD
051600131004     C                   eval      campo_alfa = tasLOD
051700131004     C                   exsr      Char_Speciali
051800131004     C                   eval      wAlfaLOD = campo_alfa
051900131004     C* IND
052000131004     C                   eval      campo_alfa = tasIND
052100131004     C                   exsr      Char_Speciali
052200131004     C                   eval      wAlfaIND = campo_alfa
052300131025     C* ORGDES
052400131025     C                   eval      campo_alfa = orgDES
052500131025     C                   exsr      Char_Speciali
052600131025     C                   eval      wAlfaORGDES = campo_alfa
052700131004     C*
052800131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
052900131004     C* (ancora corretto ???)
053000131004     C                   z-add     *zeros        wPeso             7 1
053100131004     C                   if        tasPKB > tasPKC
053200131004     C                   eval      wPeso = tasPKB
053300131004     C                   else
053400131004     C                   eval      wPeso = tasPKC
053500131004     C                   endif
053600131004     C*
053700131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
053800131004     C*
053900131004     C*  UNH -
054000131004     C                   eval      Conta_segm  = Conta_segm  + 1
054100131007     C                   eval      vawDTA = UNH_sgm_1
054200131007     C                              + %trim(%editc(Conta_msg:'4'))
054300131007     C                              + UNH_sgm_2
054400131004     C                              +   apice
054500131004     C                   exsr      wriOUTPUT
054600131004     C*
054700131004     C*  BGM -
054800131004     C                   eval      Conta_segm  = Conta_segm  + 1
054900131004     C                   eval      vawDTA = BGM_sgm
055000131004     C                              + %trim(%editc(stbRMN:'4'))
055100131004     C                              +   apice
055200131004     C                   exsr      wriOUTPUT
055300131004     C*
055400150113     C*  NAD SF -
055500150113     C                   eval      Conta_segm  = Conta_segm  + 1
055600150113     C                   eval      vawDTA = NAD_SF
055700150113     C                              + %trim(w§NAD_SF)
055800150113     C***                eval      vawDTA = NAD_SF_1
055900131008     C***                           + NAD_SF_2
056000131008     C***                           + NAD_SF_3
056100131008     C***                           + NAD_SF_4
056200131008     C***                           + NAD_SF_5
056300150113     C                              +   apice
056400150113     C                   exsr      wriOUTPUT
056500131004     C*
056600131004     C*  NAD ST -
056700131004     C                   eval      Conta_segm  = Conta_segm  + 1
056800131004     C                   eval      vawDTA = NAD_ST
056900131025     C                              + %trim(wAlfaRSD)
057000131004     C                              +   piu
057100131004     C                              +   piu
057200131004     C                              + %trim(wAlfaIND)
057300131004     C                              +   piu
057400131004     C                              + %trim(wAlfaLOD)
057500131004     C                              +   piu
057600131004     C                              + %trim(tasPRD)
057700131004     C                              +   piu
057800131004     C                              + %trim(tasCAD)
057900131004     C                              +   piu
058000131004     C                              + 'IT'
058100131004     C                              +   apice
058200131004     C                   exsr      wriOUTPUT
058300131004     C*
058400131010     C*  LOC -
058500131004     C                   eval      Conta_segm  = Conta_segm  + 1
058600131004     C                   eval      vawDTA = LOC_sgm1
058700131010     C                              + %trim(%editc(orgFIL:'X'))
058800131004     C                              +   LOC_sgm2
058900131025     C                              + %trim(wAlfaORGDES)
059000131010     C                              +   LOC_sgm3
059100131004     C                              +   apice
059200131004     C                   exsr      wriOUTPUT
059300131004     C*
059400131004     C*  CNI -
059500131004     C                   eval      Conta_segm  = Conta_segm  + 1
059600131004     C                   eval      vawDTA = CNI_sgm
059700131004     C                              +   apice
059800131004     C                   exsr      wriOUTPUT
059900131004     C*
060000131004     C*  STS -
060100131004     C                   eval      Conta_segm  = Conta_segm  + 1
060200141006     C                   eval      vawDTA = STS_sgm
060300150113     C                              + %trim(wStsTyp) + '+'
060400150113     C                              + %trim(wStsEvt) + '+'
060500141006     C                              + %trim(wStsRsn)
060600131004     C                              +   apice
060700131004     C                   exsr      wriOUTPUT
060800131004     C*
060900131004     C*  RFF -
061000131004     C                   eval      Conta_segm  = Conta_segm  + 1
061100131004     C                   eval      vawDTA = RFF_sgm
061200131004     C                              + %trim(wAlfaRMA)
061300131004     C                              +   apice
061400131004     C                   exsr      wriOUTPUT
061500131004     C*
061600131004     C*  TDT -
061700131004     C                   eval      Conta_segm  = Conta_segm  + 1
061800131004     C                   eval      vawDTA = TDT_sgm
061900131004     C                              +   apice
062000131004     C                   exsr      wriOUTPUT
062100131004     C*
062200131004     C*  DTM -
062300131004     C                   eval      Conta_segm  = Conta_segm  + 1
062400131004     C                   eval      vawDTA = DTM_sgm
062500131007     C                              + %trim(%editc(stbDAS*10000+stbORS:'4'))
062600131007     C                              + '00'
062700131004     C                              +   diviso
062800131007     C                              + '204'
062900131004     C                              +   apice
063000131004     C                   exsr      wriOUTPUT
063100131004     C*
063200131004     C*  EQD -
063300131004     C                   eval      Conta_segm  = Conta_segm  + 1
063400131004     C                   eval      vawDTA = EQD_sgm
063500131004     C                              +   apice
063600131004     C                   exsr      wriOUTPUT
063700131004     C*
063800131004     C*  MEA -
063900131004     C                   eval      Conta_segm  = Conta_segm  + 1
064000131004     C                   eval      vawDTA = MEA_sgm
064100131004     C                              + %trim(%editc(wPeso:'3'))
064200131004     C                              + '0'
064300131004     C                              +   apice
064400131004     C                   exsr      wriOUTPUT
064500160215     C*
064600160215     C*  GID -
064700160215     C                   eval      Conta_segm  = Conta_segm  + 1
064800160215     C                   eval      vawDTA = GID_sgm1
064900160215     C                              + %trim(%editc(tasNCL:'4'))
065000160215     C                              +   GID_sgm2
065100160215     C                              +   apice
065200160215     C                   exsr      wriOUTPUT
065300131004     C*
065400131004     C*  UNT -
065500131004     C                   eval      Conta_segm  = Conta_segm  + 1
065600131004     C                   eval      vawDTA = UNT_sgm
065700131004     C                              + %trim(%editc(Conta_segm:'4'))
065800131004     C                              +   piu
065900131007     C                              + %trim(%editc(Conta_msg:'4'))
066000131004     C                              +   apice
066100131004     C                   exsr      wriOUTPUT
066200031103     C*
066300031103     C                   ENDSR
066400150113
066500150113
066600150113     C*----------------------------------------------------*
066700150113     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
066800150113     C*----------------------------------------------------*
066900150113     C     SPLIT_PARMS   BEGSR
067000150113     C*
067100150113     C                   reset                   w§UNB_ID
067200150113     C                   reset                   w§NAD_SF
067300150113     C*
067400150113     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
067500150113     C                   CLEAR                   DVIREPE
067600150113     C                   EVAL      vireTRC = 'PE'
067700150114     C                   MOVE(P)   w§PPIDTI      vireDTI
067800150113     C     KEYvire1C     CHAIN     tivire1l
067900150113     C                   IF        %found(tivire1l)
068000150113     C                   EVAL      DVIREPE = vireFLO
068100150113     C*
068200150113     C* Se reperiti parametri => effettuo splitting
068300150113     C                   eval      InStringa = %trim(§VIREPEKPJ)
068400150113     C*
068500150113     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
068600150113     C                   CALL      'XSPLIT2'
068700150113     C                   PARM                    InStringa
068800150113     C                   PARM      '§'           InSepara
068900150113     C                   PARM      '6'           InTypeOut
069000150113     C                   PARM      ''            wSkParam
069100150113     C                   PARM                    OutErrore
069200150113     C                   PARM                    OutMsg
069300150113     C                   MOVEA     wSkParam      SkSplitCSV_6
069400150113     C*
069500150113     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
069600150113     C                   z-add     1             i                 4 0
069700150113     C                   dow       i <= %elem(SkSplitCSV_6)
069800150113     C                   if        SkSplitCSV_6(i) = *blanks
069900150113     C                   leave
070000150113     C                   else
070100150113     C                   select
070200150113     C* - parametro UNB
070300150113     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
070400150113     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
070500150113     C* - parametro NAD+SF
070600150113     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
070700150113     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
070800150113     C*
070900150113     C                   endsl
071000150113     C                   endif
071100150113     C*
071200150113     C                   add       1             i
071300150113     C                   enddo
071400150113     C*
071500150113     C                   ENDIF
071600150113     C*
071700150113     C                   ENDSR
071800031103
071900031103
072000991027
072100991027      /TITLE Operazioni iniziali.
072200991027     C     *inzsr        BEGSR
072300991027     C*
072400991027     C     *ENTRY        PLIST
072500150113     C                   parm                    prmppi
072600991027     C     wrkesito      parm      wrkesito      prmesito
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
074100131004     C                   eval      currHHMM =
074200131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
074300030709     C*
074400030709     C* Definizione chiavi
074500030924     C*
074600110103     C* Chiave su TISTB03L - Parziale
074700110103     C     KEYstb03P     KLIST
074800031104     C                   KFLD                    wKstbKSU
074900031103     C                   KFLD                    wKstbFTR
075000031103     C*
075100031103     C* Chiave su TITAS30C - Parziale
075200031103     C     KEYtas30P     KLIST
075300031103     C                   KFLD                    stbAAS
075400031103     C                   KFLD                    stbLNP
075500031103     C                   KFLD                    stbNRS
075600031103     C                   KFLD                    stbNSP
075700150113     C*
075800150113     C* Chiave su TIVIRE1L - Parziale
075900150113     C     KEYvire1C     KLIST
076000150113     C                   KFLD                    w§PPIKSU
076100150113     C                   KFLD                    w§PPITIP
076200150114     C                   KFLD                    vireDTI
076300150113     C                   KFLD                    vireTRC
076400991027     C*
076500991027     C                   ENDSR
076600131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
076700141006A              20        209  L.AVVISO                                                     1
076800141006ARS            56        265  AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
076900141006AVV            20        209  LASCIATO AVVISO 1GG                                          3
077000141006AV2            20        209  LASCIATO AVVISO 2GG                                          4
077100141006AV3            20        209  LASCIATO AVVISO 3GG                                          5
077200141006AV5            20        209  LASCIATO AVVISO 5GG                                          6
077300141006AV7            20        209  LASCIATO AVVISO 7GG                                          7
077400141006AV9            20        209  LASCIATO AVVISO 9 GIORNI                                     8
077500141006A16            56        265  L.A. RIMANDA LO SVINCOLO                                     9 ex 32
077600141006A23            56        212  L.A. CHIUSO FINO AL                                         10
077700131113CPO            NONINVIARE     CAMBIO PORTO                                                11
077800141006DAN            20        18   COLLO/I DANNEGGIATO/I                                       12
077900141007DIR            20        53   DIROTTAMENTO                                                13
078000131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
078100141006G              56        265  GIACENZA GENERICA TEMPORANEA                                15 ex 32
078200141006GEN            56        265  GIACENZA GENERICA                                           16 ex 32
078300131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
078400131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
078500131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
078600131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
078700141006IDD            14        117  CHIUSURA SPEDIZIONE CON IDD                                 21
078800141006MAN            20        117  COLLO/I MANCANTE/I                                          22
078900141006MIC            56        113  MESSA IN CONSEGNA                                           23
079000131113N              NONINVIARE     NON FATTA                                                   24
079100141006NAV            14        117  COLLO SCONDIZIONATO                                         25
079200131113NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
079300141006P              23        117  CONSEGNA PARZIALE                                           27
079400141007PAT            20        58   FESTIVITA PATRONALE                                         28
079500141006R              NONINVIARE???  RISERVA                                                     29
079600131113RDC            NONINVIARE     RIPRISTINO                                                  30
079700141006RFD            56        18   RIFIUTATO                                                   31
079800141006RFM            56        117  RIFIUTATO                                                   32
079900141006RIC            20        209  RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
080000131113RIP            NONINVIARE     RIPRISTINO                                                  34
080100141006RS             NONINVIARE???  RISERVA                                                     35
080200141006T              20        211  CHIUSO PER TURNO                                            36
080300141006WA             NONINVIARE???  AMMANCO SENZA EVENTO                                        37
080400141006WAE            NONINVIARE???  AMMANCO CON EVENTO                                          38
080500141006WM             NONINVIARE???  MANCANZA SENZA EVENTO                                       39
080600141006WME            NONINVIARE???  MANCANZA CON EVENTO                                         40
080700141006WV             NONINVIARE???  AVARIA SENZA EVENTO                                         41
080800141006WVE            NONINVIARE???  AVARIA CON EVENTO                                           42
080900141007ZAL            20        63   ALLUVIONE                                                   43
081000141007ZBS            20        58   BLOCCO STRADALE                                             44
081100141007ZFR            20        58   FESTIVITÀ REGIONALE                                         45
081200141007ZMM            20        63   MARE MOLTO MOSSO                                            46
081300141007ZMP            20        58   MANIFESTAZIONE PUBBLICA                                     47
081400141007ZNV            20        63   NEVICATA ECCEZIONALE                                        48
081500141007ZSC            20        58   SCIOPERO                                                    49
081600141006001            56        265  RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
081700141006002            56        284  MERCE NON ORDINATA O NON CONFORME                           51
081800141006003            56        294  MERCE SPEDITA IN RITARDO                                    52
081900141006004            56        284  MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
082000141006005            56        294  MERCE SPEDITA CON TROPPO ANTICIPO                           54
082100141006006            56        265  MERCE RESA SENZA AUTORIZZAZIONE                             55
082200141006007            NONINVIARE???  DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
082300141006008            56        292  DESTINATARIO NON PAGA IL C/ASSEGNO                          57
082400141006009            56        265  CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
082500141006010            NONINVIARE???  DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
082600141006012            56        297  IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
082700141006013            NONINVIARE???  DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
082800141006014            NONINVIARE???  DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
082900141006016            56        265  DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
083000141006017            20        209  DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
083100141006018            NONINVIARE???  DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
083200141006019            20        234  DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
083300141006020            NONINVIARE???  DESTINATARIO RISULTA TRASFERITO                             67
083400141006021            20        234  DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
083500141006022            20        213  INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
083600141006023            56        212  IL DESTINATARIO E' CHIUSO FINO AL                           70
083700141006024            20        212  IL DESTINATARIO E' CHIUSO PER FERIE                         71
083800141006025            NONINVIARE???  IL DESTINATARIO E' CHIUSO                                   72
083900141006026            56        265  IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
084000141006027            NONINVIARE???  DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
084100141006028            56        212  ESERCIZIO NON ANCORA IN ATTIVITA                            75
084200141006029            NONINVIARE???  IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
084300141006030            NONINVIARE???  IL DESTINATARIO RIFIUTA LA MERCE                            77
084400141006031            NONINVIARE???  IL DESTINATARIO NON INTENDE                                 78
084500141006032            56        265  FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
084600141006033            NONINVIARE???  IL DESTINATARIO RICHIEDE                                    80 ex 32
084700141006034            56        265  SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
084800141006035            56        241  DOCUMENTI INCOMPLETI O MANCANTI                             82
084900141006036            NONINVIARE???  IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
085000141006037            56        265  RIFIUTA LA CONSEGNA TASSATIVA                               84
085100141114100            56        265  CONSEGNA RICHIESTA OLTRE 72 ORE                             85
085200141114101            56        265  A SEGUITO L.AVV.CONCORDATA CONSEGNA OLTRE 72 ORE            86
085300141114999            NONINVIARE     -                                                           87
085400141006CONSOK         21             CONSEGNA OK                                                 88
085500141007RESO           14        279  RESO                                                        89
085600131104                                                                                          90
085700131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
085800141007A              NONINVIARE???  AMMANCO                                                        1
085900141007M              NONINVIARE???  MANCANZA                                                       2
086000141007V              NONINVIARE???  AVARIA                                                         3
086100141007E              NONINVIARE???  EVENTO AVVERSO                                                 4
086200131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
086300141007INCARICO1      1                                                                             1
086400141007INCARICO2      1                                                                             2
086500141007CONSOK1        21                                                                            3
086600141007CONSOK2        21                                                                            4
086700131113GIACNODIS1     NONINVIARE                                                                    5
086800131113GIACNODIS2     NONINVIARE                                                                    6
086900131113GIACSIDIS1     NONINVIARE                                                                    7
087000131113GIACSIDIS2     NONINVIARE                                                                    8
087100141007COLNOTAFF      14        117                                                                 9
087200141110ARRPRICOL      NONINVIARE     ARRIVO PRIMO COLLO TERMINAL DISTRIBUZIONE  (10/001)            10
087300141110               NONINVIARE                                                                    10
087400141110               NONINVIARE                                                                    10
