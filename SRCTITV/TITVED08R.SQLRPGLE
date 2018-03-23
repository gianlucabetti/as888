000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500150113     FTIVIRE1L  IF   E           K DISK
000600041015     FTNTBE01L  IF   E           K DISK
000700031103     FTITAS30C  IF   E           K DISK
000800150803     FTITAH30C  IF   E           K DISK
000900150803     FTISTB03L  UF   E           K DISK    commit
001000090323     FTIVAWWWT  UF A E             DISK    commit
001100031103
001200031103     D*-----------
001300031103     D* SCHIERE A PROGRAMMA
001400031103     D*-----------
001500131104     D WCAU            S             90    DIM(90) CTDATA PERRCD(1)
001600131104     D WDCT            S             30    DIM(4)  CTDATA PERRCD(1)
001700141110     D WTIS9           S             30    DIM(12) CTDATA PERRCD(1)
001800131104
001900131104
002000131104     D*-----------
002100131104     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
002200131104     D*-----------
002300131104     D DS_WCAU         DS
002400131104     D  dsCAU_BRT                    15
002500141006     D  dsCAU_EVT                    10
002600141006     D  dsCAU_RSN                     5
002700131104     D  dsCAU_DES                    60
002800131104     D*
002900131104     D DS_WDCT         DS
003000131104     D  dsDCT_BRT                    15
003100141007     D  dsDCT_EVT                    10
003200141007     D  dsDCT_RSN                     5
003300131104     D*
003400131104     D DS_WTIS9        DS
003500131104     D  dsTIS9_BRT                   15
003600141007     D  dsTIS9_EVT                   10
003700141007     D  dsTIS9_RSN                    5
003800131104
003900131104
004000041015     D*-----------
004100041015     D* SCHIERE
004200041015     D*-----------
004300131104     D skCAU_BRT       S             10    DIM(%elem(WCAU))
004400141006     D skCAU_EVT       S             10    DIM(%elem(WCAU))
004500141007     D skCAU_RSN       S              5    DIM(%elem(WCAU))
004600131104     D skCAU_DES       S             60    DIM(%elem(WCAU))
004700131104     D jCAU            S              3  0 INZ(*zeros)
004800131104     D skDCT_BRT       S              1    DIM(%elem(WDCT))
004900141007     D skDCT_EVT       S             10    DIM(%elem(WDCT))
005000141007     D skDCT_RSN       S              5    DIM(%elem(WDCT))
005100131104     D jDCT            S              3  0 INZ(*zeros)
005200131104     D skTIS9_BRT      S             10    DIM(%elem(WTIS9))
005300141007     D skTIS9_EVT      S             10    DIM(%elem(WTIS9))
005400141007     D skTIS9_RSN      S              5    DIM(%elem(WTIS9))
005500131104     D jTIS9           S              3  0 INZ(*zeros)
005600131104     D skTADCod        S              2    DIM(100)
005700131104     D skTADRag        S              1    DIM(100)
005800041015     D jTAD            S              3  0 INZ(*zeros)
005900150113     D*
006000150113     D w§UNB_ID        S            128    INZ(*zeros)
006100150113     D w§NAD_SF        S            256    INZ(*zeros)
006200150113
006300150113
006400150113     D*------------------
006500150113     D* VARIABILI X LA CHIAMATA AL *PGM UTILITA' "XSPLIT"
006600150113     D*------------------
006700150113     D InStringa       S          65535A   VARYING                              (stringa input)
006800150113     D InSepara        S             10A                                        (separatore)
006900150113     D InTypeOut       S              1A                                        (tipo output)
007000150113     D wSkParam        S          65535A                                        (output)
007100150113     D OutErrore       S              1A                                        (flag errore)
007200150113     D OutMsg          S             80A                                        (messaggio errore)
007300150113     D SkSplitCSV_6    S            512    DIM(128)
007400150113
007500030924
007600041015     D*-----------
007700041015     D* VARIABILI D WRK
007800041015     D*-----------
007900031103     D prmppi          S             50
008000030924     D prmesito        S              1
008100030924     D wrkesito        S                   like(prmesito)
008200031103     D j               S              2  0
008300130807     D wTADCod         S              2
008400131004     D campo_alfa      S            512
008500141007     D wStsEvt         S             25
008600141007     D wStsRsn         S             25
008700131004     D currDate6       S              6
008800131004     D currDate6inv    S              6
008900131004     D currHHMM        S              4
009000131216     D Conta_msg       S             10i 0
009100131216     D Conta_segm      S             10i 0
009200131004
009300131004
009400131004     D*-----------
009500131004     D* DEFINIZIONE DS ESTERNE
009600131004     D*-----------
009700131004     D trul33ds      e ds                  inz
009800131004     D kpjba         e ds                  inz
009900131004     D DTAD          e ds                  inz
010000150113     D DVIREPE       e ds                  inz
010100031103
010200131004
010300131004     D*------------------
010400131004     D* LINKING A DEFINIZIONI ESTERNE
010500131004     D*------------------
010600131004     D/COPY GAITRASRC/SRCPROTOPR,UBFMTDATE
010700131004     D/COPY GAITRASRC/SRCPROTOPI,UBFMTDATE
010800131004
010900131004
011000031103     D*-----------
011100031103     D* VARIABILI RIFERITE AL DATA-BASE
011200031103     D*-----------
011300080618     D wKstbKSU        S                   like(stbKSU)
011400080618     D wKstbFTR        S                   like(stbFTR)
011500110705
011600031103
011700031103     D*-----------
011800031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
011900031103     D*-----------
012000150114     D DSPPI           DS
012100150114     D  w§PPIKSU                           like(vireKSC) inz
012200150114     D  w§PPITIP                           like(vireTIP) inz
012300150114     D  w§PPIDTI                      8                  inz
012400131004
012500131004
012600131004     D*-----------
012700131004     D* COSTANTI / DEFAULT
012800131004     D*-----------
012900150113     D UNB_sgm         C                   CONST('UNB+UNOA:1+IT-BRT001:ZZ+')
013000131007     D UNH_sgm_1       C                   CONST('UNH+')
013100150615     D UNH_sgm_2       C                   CONST('+IFTSTA:D:94A:UN:EAN008')
013200131004     D BGM_sgm         C                   CONST('BGM+23+')
013300150113     D NAD_SF          C                   CONST('NAD+SF+')
013400150113     D*NAD_SF_1        C                   CONST('NAD+SF+5450534002394::9++')
013500150113     D*NAD_SF_2        C                   CONST('AMAZON EU SARL CO+AMAZON ')
013600150113     D*NAD_SF_3        C                   CONST('LOGISTICA SRL:STRADA DOGA')
013700150113     D*NAD_SF_4        C                   CONST('NA PO 2U+CASTEL SAN GIOVA')
013800150113     D*NAD_SF_5        C                   CONST('NNI++29015+IT')
013900131004     D NAD_ST          C                   CONST('NAD+ST++')
014000131010     D LOC_sgm1        C                   CONST('LOC+175+')
014100131010     D LOC_sgm2        C                   CONST('::3:')
014200131010     D LOC_sgm3        C                   CONST('+IT')
014300131004     D CNI_sgm         C                   CONST('CNI+1')
014400141006     D STS_sgm         C                   CONST('STS+')
014500150803     D RFF_AGE_sgm     C                   CONST('RFF+AGE:')
014600150113     D TDT_sgm         C                   CONST('TDT+30+1+++IT-BRT001')
014700150803     D DTM_sgm         C                   CONST('DTM+7:')
014800150803     D PCI_sgm         C                   CONST('PCI+24+')
014900131004     D EQD_sgm         C                   CONST('EQD+AA')
015000131004     D MEA_sgm         C                   CONST('MEA+ABR+AAD+KG:')
015100160215     D*GID_sgm         C                   CONST('GID+1+1:CTN')
015200160215     D GID_sgm1        C                   CONST('GID+1+')
015300160215     D GID_sgm2        C                   CONST(':CTN')
015400131004     D UNT_sgm         C                   CONST('UNT+')
015500131004     D UNZ_sgm         C                   CONST('UNZ+')
015600131004     D piu             C                   CONST('+')
015700131004     D diviso          C                   CONST(':')
015800131004     D apice           C                   CONST('''')
015900141006     D wStsTyp         C                   CONST('')
016000131004
016100030924
016200030325
016300030924     C* MAIN/
016400170123     C*
016500170123     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
016600170123     C
016700170123     C/EXEC SQL
016800170123     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
016900170123     C/END-EXEC
017000041015     C*
017100041015     C* Carica i dati tabellati
017200041015     C                   EXSR      cartab
017300140308     C*
017400140308     C* Spia dati da inviare
017500140308     C                   SETOFF                                       45
017600041015     C*
017700041015     C* Esegue traduzione
017800140308     C                   SETOFF                                       31
017900150113     C  N31              EXSR      SPLIT_PARMS
018000131004     C  N31              EXSR      chkRECFILMBR
018100131004     C  N31              EXSR      rtvID
018200140308     C***  N31              EXSR      wriHeader
018300131004     C  N31              EXSR      traduci
018400140308     C  N31
018500140308     CAN 45              EXSR      wriFooter
018600140308 xxx C***   31              EXSR      sndAlert
018700110705     C*
018800921023     C                   SETON                                        LR
018900131004
019000131004
019100131004
019200131004     C*----------------------------------------------------*
019300131004     C*  STACCO ID UNIVOCO
019400131004     C*----------------------------------------------------*
019500131004     C     RTVID         BEGSR
019600131004     C*
019700131004     C                   z-add     *zeros        currUNIQ_ID      15 0
019800131004     C*
019900131004     C* NSP => Stacco un numeratore da AZNUM
020000131004     C                   clear                   TRUL33DS
020100131004     C                   eval      I33OPE = *zeros
020200131004     C                   eval      I33CNU = 302
020300131004     C                   eval      I33NUM = 1
020400131004     C                   movel     TRUL33DS      KPJBU
020500131004     C                   call      'TRUL33R'
020600131004     C                   parm                    KPJBA
020700131004     C                   movel     KPJBU         TRUL33DS
020800131004     C                   if        O33ERR = *zeros
020900131004     C                   z-add     O33NRF        currUNIQ_ID
021000131004     C                   else
021100131004     C                   seton                                        31
021200131004     C                   endif
021300131004     C*
021400131004     C                   ENDSR
021500131004
021600131004
021700131004
021800131004     C     chkRECFILMBR  BEGSR
021900131004     C*
022000131004     C     *start        setll     TIVAWWWT
022100131004     C                   if        %found(TIVAWWWT)
022200131004     C                   seton                                        31
022300131004     C                   endif
022400131004     C*
022500131004     C                   ENDSR
022600131004
022700131004
022800131004
022900131004     C     wriOUTPUT     BEGSR
023000131004     C*
023100131004     C                   write     TIVAW000
023200131004     C*
023300131004     C                   ENDSR
023400131004
023500130807
023600041015
023700041015     C     cartab        BEGSR
023800041015     C*
023900041015     C* Caricamento tipi anomalia C/A
024000041015     C     'TAD'         SETLL     tntbe01l
024100041015     C                   IF        %equal(tntbe01l)
024200041015     C     'TAD'         READE     tntbe01l
024300041015     C                   DOW       not %eof(tntbe01l)
024400041015     C                   EVAL      DTAD = tbeUNI
024500041015     C                   ADD       1             jTAD
024600041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
024700041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
024800041015     C     'TAD'         READE     tntbe01l
024900041015     C                   ENDDO
025000041015     C                   ENDIF
025100131104     C*
025200131104     C* Carico la mappatura dei codici status personalizzati cliente
025300131104     C                   z-add     1             jCAU
025400131104     C                   dow       jCAU <= %elem(WCAU) and
025500131104     C                                     WCAU(jCAU) <> *blanks
025600131104     C                   eval      DS_WCAU = WCAU(jCAU)
025700131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
025800141006     C                   eval      skCAU_EVT(jCAU) = dsCAU_EVT
025900141006     C                   eval      skCAU_RSN(jCAU) = dsCAU_RSN
026000131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
026100131104     C                   add       1             jCAU
026200131104     C                   enddo
026300131104     C*
026400131104     C                   z-add     1             jDCT
026500131104     C                   dow       jDCT <= %elem(WDCT) and
026600131104     C                                     WDCT(jDCT) <> *blanks
026700131104     C                   eval      DS_WDCT = WDCT(jDCT)
026800131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
026900141007     C                   eval      skDCT_EVT(jDCT) = dsDCT_EVT
027000141007     C                   eval      skDCT_RSN(jDCT) = dsDCT_RSN
027100131111     C                   add       1             jDCT
027200131104     C                   enddo
027300131104     C*
027400131104     C                   z-add     1             jTIS9
027500131104     C                   dow       jTIS9 <= %elem(WTIS9) and
027600131104     C                                      WTIS9(jTIS9) <> *blanks
027700131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
027800131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
027900141007     C                   eval      skTIS9_EVT(jTIS9) = dsTIS9_EVT
028000141007     C                   eval      skTIS9_RSN(jTIS9) = dsTIS9_RSN
028100131104     C                   add       1             jTIS9
028200131104     C                   enddo
028300131104     C*
028400131104     C                   ENDSR
028500131104
028600131004
028700131004
028800131004     C     Char_Speciali BEGSR
028900131111     C*
029000131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
029100131111     C*
029200131111     C/EXEC SQL
029300131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
029400131111     C/END-EXEC
029500131111     C*
029600131111     C/EXEC SQL
029700131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
029800131111     C/END-EXEC
029900131111     C*
030000131111     C/EXEC SQL
030100131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
030200131111     C/END-EXEC
030300131111     C*
030400131111     C/EXEC SQL
030500131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
030600131111     C/END-EXEC
030700131111     C*
030800131111     C/EXEC SQL
030900131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
031000131111     C/END-EXEC
031100131004     C*
031200131004     C                   ENDSR
031300131004
031400131004
031500150113
031600150113     C     wriHeader     BEGSR
031700131004     C*
031800131004     C                   clear                   vawDTA
031900131010     C*
032000131004     C*  UNB -
032100131004     C                   eval      vawDTA = UNB_sgm
032200150113     C                              + %trim(w§UNB_ID)
032300131004     C                              +   piu
032400131004     C                              + %trim(currDate6inv)
032500131004     C                              +   diviso
032600131004     C                              + %trim(currHHMM)
032700131004     C                              +   piu
032800131004     C                              + %trim(
032900131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
033000131004     C                              +   apice
033100131004     C                   exsr      wriOUTPUT
033200131004     C*
033300131004     C                   ENDSR
033400131004
033500131004
033600131004
033700131004     C     wriFooter     BEGSR
033800131004     C*
033900131004     C                   clear                   vawDTA
034000131004     C*
034100131004     C*  UNZ -
034200131004     C                   eval      vawDTA = UNZ_sgm
034300131004     C                              + %trim(%editc(Conta_msg:'4'))
034400131004     C                              +   piu
034500131004     C                              + %trim(
034600131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
034700131004     C                              +   apice
034800131004     C                   exsr      wriOUTPUT
034900131004     C*
035000131004     C                   ENDSR
035100131004
035200030924
035300030924
035400991027     C     traduci       BEGSR
035500131004     C*
035600131004     C* Inizializzo i contatori a livello di transazione/messaggio
035700131004     C                   z-add     *zeros        Conta_msg
035800031103     C*
035900031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
036000031103     C* di traduzione (trattasi d "post-traduzione")
036100150113     C                   MOVE(P)   w§PPIKSU      wKstbKSU
036200031103     C                   EVAL      wKstbFTR = *blanks
036300110103     C     KEYstb03P     SETLL     tistb03l
036400110103     C                   IF        %equal(tistb03l)
036500110103     C     KEYstb03P     READE     tistb03l
036600991027     C*
036700031103     C* Ciclo x tuttii record del cliente da trasmettere
036800110103     C                   DOW       not %eof(tistb03l)
036900131004     C*
037000131004     C* Inizializzo i contatori a livello di segmento
037100131004     C                   z-add     *zeros        Conta_segm
037200080618     C*
037300130807     C                   SETON                                        50
037400031103     C*
037500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
037600031103     C* deve restituire al cliente nel file tradotto
037700031103     C                   IF        stbFTR = *blanks                             * ridondante
037800110103     C*
037900110103     C* Gestisco solamente gli stati standard
038000130808     C                   IF        (stbTIS= '1' OR
038100130906     C                             (stbTIS= '9' AND
038200151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
038300151117     C                              (stbPRS= '1'                ))) AND
038400130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
038500130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
038600131004     C*
038700131004     C* Per "sicurezza" non invio stati senza data
038800131004     C                   IF        stbDAS > *zeros
038900110103     C*
039000110103     C                   SETOFF                                       50
039100031103     C*
039200031103     C* Innanzitutto chaino la bolla direttamente su TITAS
039300031103     C     KEYtas30P     CHAIN     titas30c
039400031103     C                   IF        %found(titas30c)
039500131010     C*
039600131010     C* Innanzitutto chaino la bolla direttamente su TITAS
039700131216     C                   MOVEL     *zeros        wPO               3 0
039800131213     C*
039900131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
040000131213     C                   if        tasDTI = *zeros
040100131213     C                   eval      wPO = tasLNP
040200131213     C                   else
040300131213     C                   eval      wPO = tasLNA
040400131213     C                   endif
040500131213     C*
040600131213     C     wPO           CHAIN     azorg01l
040700131010     C                   IF        %found(azorg01l)
040800080618     C*
040900110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
041000140606     C                   SETOFF                                       80
041100110103     C                   IF        stbTIS = '1'
041200031103     C                   Z-ADD     1             j
041300131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
041400141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
041500141006     C                   EVAL      wStsEvt = skCAU_EVT(j)
041600141006     C                   EVAL      wStsRsn = skCAU_RSN(j)
041700140514     C                   SETOFF                                       50
041800140606     C                   SETON                                        80
041900031103     C                   ELSE
042000031103     C                   SETON                                        50
042100031103     C                   ENDIF
042200110103     C                   ENDIF
042300110103     C*
042400110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
042500140606     C                   IF        not *IN80
042600140514     C***                IF        stbTIS <> '1'
042700110103     C                   Z-ADD     1             j
042800131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
042900141007     C                   IF        %found AND skTIS9_EVT(j) <> 'NONINVIARE'
043000141007     C                   EVAL      wStsEvt = skTIS9_EVT(j)
043100141007     C                   EVAL      wStsRsn = skTIS9_RSN(j)
043200140514     C                   SETOFF                                       50
043300140606     C                   SETON                                        80
043400110103     C                   ELSE
043500110103     C                   SETON                                        50
043600110103     C                   ENDIF
043700140514     C***                ENDIF
043800140606     C                   ENDIF
043900041015     C*
044000080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
044100130807     C                   IF        stbPRS =  '5'
044200080618     C*
044300130807     C                   SETOFF                                       50
044400080618     C*
044500080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
044600130807     C                   Z-ADD     1             jTAD
044700130807     C                   MOVEL(P)  stbCOS        wTADCod
044800130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
044900130807     C                   IF        %found
045000130807     C                   Z-ADD     1             j
045100131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
045200130807     C                   IF        %found
045300141007     C                   IF        %trim(skDCT_EVT(j)) <> 'NONINVIARE'
045400141007     C                   EVAL      wStsEvt = skDCT_EVT(j)
045500141007     C                   EVAL      wStsRsn = skDCT_RSN(j)
045600140514     C                   SETOFF                                       50
045700131004     C                   ELSE
045800131004     C                   SETON                                        50
045900131004     C                   ENDIF
046000130807     C                   ELSE
046100130807     C                   SETON                                        50
046200130807     C                   ENDIF
046300130807     C                   ELSE
046400130807     C                   SETON                                        50
046500130807     C                   ENDIF
046600130807     C                   ENDIF
046700140308     C*
046800140308     C* Al primo dettaglio da inviare => scrivo anche testata
046900140313     C                   IF        not *IN50
047000140313     C  N45              EXSR      wriHeader
047100140308     C                   SETON                                        45
047200140313     C                   ENDIF
047300080618     C*
047400080618     C* Scarico il buffer d output
047500131004     C  N50              EXSR      wriStato
047600110103     C*
047700131010     C                   ENDIF
047800131004     C                   ENDIF
047900110103     C                   ENDIF
048000110103     C                   ENDIF
048100110103     C                   ENDIF
048200031103     C*
048300031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
048400031103     C                   EVAL      stbFTR = 'T'
048500031103     C                   UPDATE    tistb000
048600991027     C*
048700110103     C     KEYstb03P     READE     tistb03l
048800031103     C                   SETOFF                                       50
048900030325     C                   ENDDO
049000031103     C                   ENDIF
049100991027     C*
049200030325     C                   EVAL      wrkesito = '0'
049300991027     C*
049400910830     C                   ENDSR
049500031103
049600031103
049700031103
049800131004     C     wriStato      BEGSR
049900131004     C*
050000131004     C* Incremento il contatore di messaggio (al interno della transazione)
050100131004     C                   eval      Conta_msg = Conta_msg + 1
050200131004     C*
050300131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
050400131104     C                   movel     *blanks       wAlfaRMA        512
050500131024     C                   movel     *blanks       wAlfaRSD        512
050600131004     C                   movel     *blanks       wAlfaLOD        512
050700131004     C                   movel     *blanks       wAlfaIND        512
050800131025     C                   movel     *blanks       wAlfaORGDES     512
050900131004     C* RMA
051000131004     C                   eval      campo_alfa = stbRMA
051100131004     C                   exsr      Char_Speciali
051200131004     C                   eval      wAlfaRMA = campo_alfa
051300131024     C* RSD
051400131024     C                   eval      campo_alfa = tasRSD
051500131024     C                   exsr      Char_Speciali
051600131024     C                   eval      wAlfaRSD = campo_alfa
051700131004     C* LOD
051800131004     C                   eval      campo_alfa = tasLOD
051900131004     C                   exsr      Char_Speciali
052000131004     C                   eval      wAlfaLOD = campo_alfa
052100131004     C* IND
052200131004     C                   eval      campo_alfa = tasIND
052300131004     C                   exsr      Char_Speciali
052400131004     C                   eval      wAlfaIND = campo_alfa
052500131025     C* ORGDES
052600131025     C                   eval      campo_alfa = orgDES
052700131025     C                   exsr      Char_Speciali
052800131025     C                   eval      wAlfaORGDES = campo_alfa
052900131004     C*
053000131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
053100131004     C* (ancora corretto ???)
053200131004     C                   z-add     *zeros        wPeso             7 1
053300131004     C                   if        tasPKB > tasPKC
053400131004     C                   eval      wPeso = tasPKB
053500131004     C                   else
053600131004     C                   eval      wPeso = tasPKC
053700131004     C                   endif
053800131004     C*
053900131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
054000131004     C*
054100131004     C*  UNH -
054200131004     C                   eval      Conta_segm  = Conta_segm  + 1
054300131007     C                   eval      vawDTA = UNH_sgm_1
054400131007     C                              + %trim(%editc(Conta_msg:'4'))
054500131007     C                              + UNH_sgm_2
054600131004     C                              +   apice
054700131004     C                   exsr      wriOUTPUT
054800131004     C*
054900131004     C*  BGM -
055000131004     C                   eval      Conta_segm  = Conta_segm  + 1
055100131004     C                   eval      vawDTA = BGM_sgm
055200131004     C                              + %trim(%editc(stbRMN:'4'))
055300131004     C                              +   apice
055400131004     C                   exsr      wriOUTPUT
055500131004     C*
055600150113     C*  NAD SF -
055700150113     C                   eval      Conta_segm  = Conta_segm  + 1
055800150113     C                   eval      vawDTA = NAD_SF
055900150113     C                              + %trim(w§NAD_SF)
056000150113     C***                eval      vawDTA = NAD_SF_1
056100131008     C***                           + NAD_SF_2
056200131008     C***                           + NAD_SF_3
056300131008     C***                           + NAD_SF_4
056400131008     C***                           + NAD_SF_5
056500150113     C                              +   apice
056600150113     C                   exsr      wriOUTPUT
056700131004     C*
056800131004     C*  NAD ST -
056900131004     C                   eval      Conta_segm  = Conta_segm  + 1
057000131004     C                   eval      vawDTA = NAD_ST
057100131025     C                              + %trim(wAlfaRSD)
057200131004     C                              +   piu
057300131004     C                              +   piu
057400131004     C                              + %trim(wAlfaIND)
057500131004     C                              +   piu
057600131004     C                              + %trim(wAlfaLOD)
057700131004     C                              +   piu
057800131004     C                              + %trim(tasPRD)
057900131004     C                              +   piu
058000131004     C                              + %trim(tasCAD)
058100131004     C                              +   piu
058200131004     C                              + 'IT'
058300131004     C                              +   apice
058400131004     C                   exsr      wriOUTPUT
058500131004     C*
058600131010     C*  LOC -
058700131004     C                   eval      Conta_segm  = Conta_segm  + 1
058800131004     C                   eval      vawDTA = LOC_sgm1
058900131010     C                              + %trim(%editc(orgFIL:'X'))
059000131004     C                              +   LOC_sgm2
059100131025     C                              + %trim(wAlfaORGDES)
059200131010     C                              +   LOC_sgm3
059300131004     C                              +   apice
059400131004     C                   exsr      wriOUTPUT
059500131004     C*
059600131004     C*  CNI -
059700131004     C                   eval      Conta_segm  = Conta_segm  + 1
059800131004     C                   eval      vawDTA = CNI_sgm
059900131004     C                              +   apice
060000131004     C                   exsr      wriOUTPUT
060100131004     C*
060200131004     C*  STS -
060300131004     C                   eval      Conta_segm  = Conta_segm  + 1
060400141006     C                   eval      vawDTA = STS_sgm
060500150113     C                              + %trim(wStsTyp) + '+'
060600150113     C                              + %trim(wStsEvt) + '+'
060700141006     C                              + %trim(wStsRsn)
060800131004     C                              +   apice
060900131004     C                   exsr      wriOUTPUT
061000150803     C*
061100150803     C*  RFF AGE -
061200150803     C                   eval      Conta_segm  = Conta_segm  + 1
061300150803     C                   eval      vawDTA = RFF_AGE_sgm
061400150803     C                              + %trim(wAlfaRMA)
061500150803     C                              +   apice
061600150803     C                   exsr      wriOUTPUT
061700131004     C*
061800131004     C*  TDT -
061900131004     C                   eval      Conta_segm  = Conta_segm  + 1
062000131004     C                   eval      vawDTA = TDT_sgm
062100131004     C                              +   apice
062200131004     C                   exsr      wriOUTPUT
062300131004     C*
062400131004     C*  DTM -
062500131004     C                   eval      Conta_segm  = Conta_segm  + 1
062600131004     C                   eval      vawDTA = DTM_sgm
062700131007     C                              + %trim(%editc(stbDAS*10000+stbORS:'4'))
062800131004     C                              +   diviso
062900150803     C                              + '203'
063000131004     C                              +   apice
063100131004     C                   exsr      wriOUTPUT
063200131004     C*
063300131004     C*  EQD -
063400131004     C                   eval      Conta_segm  = Conta_segm  + 1
063500131004     C                   eval      vawDTA = EQD_sgm
063600131004     C                              +   apice
063700131004     C                   exsr      wriOUTPUT
063800131004     C*
063900131004     C*  MEA -
064000131004     C                   eval      Conta_segm  = Conta_segm  + 1
064100131004     C                   eval      vawDTA = MEA_sgm
064200131004     C                              + %trim(%editc(wPeso:'3'))
064300131004     C                              + '0'
064400131004     C                              +   apice
064500131004     C                   exsr      wriOUTPUT
064600160215     C*
064700160215     C*  GID -
064800160215     C                   eval      Conta_segm  = Conta_segm  + 1
064900160215     C                   eval      vawDTA = GID_sgm1
065000160215     C                              + %trim(%editc(tasNCL:'4'))
065100160215     C                              +   GID_sgm2
065200160215     C                              +   apice
065300160215     C                   exsr      wriOUTPUT
065400150803     C*
065500150803     C*  PCI 24 -
065600150803     C                   eval      Conta_segm  = Conta_segm  + 1
065700150803     C                   eval      vawDTA = PCI_sgm
065800150803     C                   z-add     *zeros        wConta            3 0
065900150803     C*
066000150803     C* Aggancio il dettaglio segnaccolli cliente
066100150803     C                   EVAL      tahTRC = 'C'
066200150803     C     KEYtahC       SETLL     titah30c
066300150803     C                   IF        %equal(titah30c)
066400150803     C     KEYtahC       READE     titah30c
066500150803     C                   DOW       not %eof(titah30c)
066600150803     C                   ADD       1             wConta
066700150803     C                   IF        wConta > 1
066800150803     C                   EVAL      vawDTA = %trim(vawDTA) + diviso
066900150803     C                   ENDIF
067000150803     C                   EVAL      vawDTA = %trim(vawDTA) + %trim(tahNOT)
067100150803     C     KEYtahC       READE     titah30c
067200150803     C                   ENDDO
067300150803     C                   ENDIF
067400150803     C*
067500150803     C                   IF        wConta > *zeros
067600150803     C                   eval      vawDTA = %trim(vawDTA)
067700150803     C                              +   apice
067800150803     C                   exsr      wriOUTPUT
067900150803     C                   ENDIF
068000131004     C*
068100131004     C*  UNT -
068200131004     C                   eval      Conta_segm  = Conta_segm  + 1
068300131004     C                   eval      vawDTA = UNT_sgm
068400131004     C                              + %trim(%editc(Conta_segm:'4'))
068500131004     C                              +   piu
068600131007     C                              + %trim(%editc(Conta_msg:'4'))
068700131004     C                              +   apice
068800131004     C                   exsr      wriOUTPUT
068900031103     C*
069000031103     C                   ENDSR
069100150113
069200150113
069300150113     C*----------------------------------------------------*
069400150113     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
069500150113     C*----------------------------------------------------*
069600150113     C     SPLIT_PARMS   BEGSR
069700150113     C*
069800150113     C                   reset                   w§UNB_ID
069900150113     C                   reset                   w§NAD_SF
070000150113     C*
070100150113     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
070200150113     C                   CLEAR                   DVIREPE
070300150113     C                   EVAL      vireTRC = 'PE'
070400150114     C                   MOVE(P)   w§PPIDTI      vireDTI
070500150113     C     KEYvire1C     CHAIN     tivire1l
070600150113     C                   IF        %found(tivire1l)
070700150113     C                   EVAL      DVIREPE = vireFLO
070800150113     C*
070900150113     C* Se reperiti parametri => effettuo splitting
071000150113     C                   eval      InStringa = %trim(§VIREPEKPJ)
071100150113     C*
071200150113     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
071300150113     C                   CALL      'XSPLIT2'
071400150113     C                   PARM                    InStringa
071500150113     C                   PARM      '§'           InSepara
071600150113     C                   PARM      '6'           InTypeOut
071700150113     C                   PARM      ''            wSkParam
071800150113     C                   PARM                    OutErrore
071900150113     C                   PARM                    OutMsg
072000150113     C                   MOVEA     wSkParam      SkSplitCSV_6
072100150113     C*
072200150113     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
072300150113     C                   z-add     1             i                 4 0
072400150113     C                   dow       i <= %elem(SkSplitCSV_6)
072500150113     C                   if        SkSplitCSV_6(i) = *blanks
072600150113     C                   leave
072700150113     C                   else
072800150113     C                   select
072900150113     C* - parametro UNB
073000150113     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
073100150113     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
073200150113     C* - parametro NAD+SF
073300150113     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
073400150113     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
073500150113     C*
073600150113     C                   endsl
073700150113     C                   endif
073800150113     C*
073900150113     C                   add       1             i
074000150113     C                   enddo
074100150113     C*
074200150113     C                   ENDIF
074300150113     C*
074400150113     C                   ENDSR
074500031103
074600031103
074700991027
074800991027      /TITLE Operazioni iniziali.
074900991027     C     *inzsr        BEGSR
075000991027     C*
075100991027     C     *ENTRY        PLIST
075200150113     C                   parm                    prmppi
075300991027     C     wrkesito      parm      wrkesito      prmesito
075400031103     C*
075500031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
075600150113     C                   EVAL      DSPPI = prmppi
075700131004     C*
075800131004     C* Reperisco i valori variabili ma a livello di transazione corrente
075900131004     C* (tipicamente data e ora relativ al file generato)
076000131004     C                   eval      currDate6    =
076100131004     C                              UBFMTDATE_Convert(
076200131004     C                                  %editc(%dec(%date():*ISO):'X') :
076300131004     C                                  'YYYYMMDD' : 'YYMMDD')
076400131004     C                   eval      currDate6inv =
076500131004     C                              UBFMTDATE_Convert(
076600131004     C                                  %editc(%dec(%date():*ISO):'X') :
076700131004     C                                  'YYYYMMDD' : 'DDMMYY')
076800131004     C                   eval      currHHMM =
076900131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
077000030709     C*
077100030709     C* Definizione chiavi
077200030924     C*
077300110103     C* Chiave su TISTB03L - Parziale
077400110103     C     KEYstb03P     KLIST
077500031104     C                   KFLD                    wKstbKSU
077600031103     C                   KFLD                    wKstbFTR
077700031103     C*
077800031103     C* Chiave su TITAS30C - Parziale
077900031103     C     KEYtas30P     KLIST
078000031103     C                   KFLD                    stbAAS
078100031103     C                   KFLD                    stbLNP
078200031103     C                   KFLD                    stbNRS
078300031103     C                   KFLD                    stbNSP
078400150113     C*
078500150113     C* Chiave su TIVIRE1L - Parziale
078600150113     C     KEYvire1C     KLIST
078700150113     C                   KFLD                    w§PPIKSU
078800150113     C                   KFLD                    w§PPITIP
078900150114     C                   KFLD                    vireDTI
079000150113     C                   KFLD                    vireTRC
079100150803     C*
079200150803     C* Chiave su TITAH30C - completa
079300150803     C     KEYtahC       KLIST
079400150803     C                   KFLD                    stbAAS
079500150803     C                   KFLD                    stbLNP
079600150803     C                   KFLD                    stbNRS
079700150803     C                   KFLD                    stbNSP
079800150803     C                   KFLD                    tahTRC
079900991027     C*
080000991027     C                   ENDSR
080100131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
080200141006A              20        209  L.AVVISO                                                     1
080300141006ARS            56        265  AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
080400151007AVV            20        210  LASCIATO AVVISO 1GG                                          3
080500151007AV2            20        214  LASCIATO AVVISO 2GG                                          4
080600151007AV3            20        215  LASCIATO AVVISO 3GG                                          5
080700151007AV5            20        216  LASCIATO AVVISO 5GG                                          6
080800151007AV7            20        217  LASCIATO AVVISO 7GG                                          7
080900151007AV9            20        218  LASCIATO AVVISO 9 GIORNI                                     8
081000151007A16            56        266  L.A. RIMANDA LO SVINCOLO                                     9 ex 32
081100141006A23            56        212  L.A. CHIUSO FINO AL                                         10
081200131113CPO            NONINVIARE     CAMBIO PORTO                                                11
081300141006DAN            20        18   COLLO/I DANNEGGIATO/I                                       12
081400141007DIR            20        53   DIROTTAMENTO                                                13
081500131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
081600151007G              56        267  GIACENZA GENERICA TEMPORANEA                                15 ex 32
081700151007GEN            56        268  GIACENZA GENERICA                                           16 ex 32
081800131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
081900131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
082000131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
082100131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
082200141006IDD            14        117  CHIUSURA SPEDIZIONE CON IDD                                 21
082300141006MAN            20        117  COLLO/I MANCANTE/I                                          22
082400141006MIC            56        113  MESSA IN CONSEGNA                                           23
082500131113N              NONINVIARE     NON FATTA                                                   24
082600151007NAV            14        118  COLLO SCONDIZIONATO                                         25
082700131113NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
082800141006P              23        117  CONSEGNA PARZIALE                                           27
082900141007PAT            20        58   FESTIVITA PATRONALE                                         28
083000141006R              NONINVIARE???  RISERVA                                                     29
083100131113RDC            NONINVIARE     RIPRISTINO                                                  30
083200141006RFD            56        18   RIFIUTATO                                                   31
083300141006RFM            56        117  RIFIUTATO                                                   32
083400151007RIC            20        219  RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
083500131113RIP            NONINVIARE     RIPRISTINO                                                  34
083600141006RS             NONINVIARE???  RISERVA                                                     35
083700141006T              20        211  CHIUSO PER TURNO                                            36
083800141006WA             NONINVIARE???  AMMANCO SENZA EVENTO                                        37
083900141006WAE            NONINVIARE???  AMMANCO CON EVENTO                                          38
084000141006WM             NONINVIARE???  MANCANZA SENZA EVENTO                                       39
084100141006WME            NONINVIARE???  MANCANZA CON EVENTO                                         40
084200141006WV             NONINVIARE???  AVARIA SENZA EVENTO                                         41
084300141006WVE            NONINVIARE???  AVARIA CON EVENTO                                           42
084400141007ZAL            20        63   ALLUVIONE                                                   43
084500151007ZBS            20        59   BLOCCO STRADALE                                             44
084600151007ZFR            20        60   FESTIVITÀ REGIONALE                                         45
084700151007ZMM            20        64   MARE MOLTO MOSSO                                            46
084800151007ZMP            20        61   MANIFESTAZIONE PUBBLICA                                     47
084900151007ZNV            20        65   NEVICATA ECCEZIONALE                                        48
085000151007ZSC            20        62   SCIOPERO                                                    49
085100151007001            56        269  RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
085200141006002            56        284  MERCE NON ORDINATA O NON CONFORME                           51
085300141006003            56        294  MERCE SPEDITA IN RITARDO                                    52
085400151007004            56        285  MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
085500151007005            56        295  MERCE SPEDITA CON TROPPO ANTICIPO                           54
085600151007006            56        270  MERCE RESA SENZA AUTORIZZAZIONE                             55
085700141006007            NONINVIARE???  DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
085800141006008            56        292  DESTINATARIO NON PAGA IL C/ASSEGNO                          57
085900151007009            56        271  CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
086000141006010            NONINVIARE???  DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
086100141006012            56        297  IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
086200141006013            NONINVIARE???  DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
086300141006014            NONINVIARE???  DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
086400151007016            56        272  DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
086500151007017            20        220  DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
086600141006018            NONINVIARE???  DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
086700141006019            20        234  DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
086800141006020            NONINVIARE???  DESTINATARIO RISULTA TRASFERITO                             67
086900151007021            20        235  DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
087000141006022            20        213  INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
087100151007023            56        213  IL DESTINATARIO E' CHIUSO FINO AL                           70
087200141006024            20        212  IL DESTINATARIO E' CHIUSO PER FERIE                         71
087300141006025            NONINVIARE???  IL DESTINATARIO E' CHIUSO                                   72
087400151007026            56        273  IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
087500141006027            NONINVIARE???  DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
087600151007028            56        214  ESERCIZIO NON ANCORA IN ATTIVITA                            75
087700141006029            NONINVIARE???  IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
087800141006030            NONINVIARE???  IL DESTINATARIO RIFIUTA LA MERCE                            77
087900141006031            NONINVIARE???  IL DESTINATARIO NON INTENDE                                 78
088000151007032            56        274  FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
088100141006033            NONINVIARE???  IL DESTINATARIO RICHIEDE                                    80 ex 32
088200151007034            56        275  SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
088300141006035            56        241  DOCUMENTI INCOMPLETI O MANCANTI                             82
088400141006036            NONINVIARE???  IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
088500151007037            56        276  RIFIUTA LA CONSEGNA TASSATIVA                               84
088600151007100            56        277  CONSEGNA RICHIESTA OLTRE 72 ORE                             85
088700151007101            56        278  A SEGUITO L.AVV.CONCORDATA CONSEGNA OLTRE 72 ORE            86
088800141114999            NONINVIARE     -                                                           87
088900141006CONSOK         21             CONSEGNA OK                                                 88
089000141007RESO           14        279  RESO                                                        89
089100131104                                                                                          90
089200131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
089300141007A              NONINVIARE???  AMMANCO                                                        1
089400141007M              NONINVIARE???  MANCANZA                                                       2
089500141007V              NONINVIARE???  AVARIA                                                         3
089600141007E              NONINVIARE???  EVENTO AVVERSO                                                 4
089700131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
089800141007INCARICO1      1                                                                             1
089900141007INCARICO2      1                                                                             2
090000141007CONSOK1        21                                                                            3
090100141007CONSOK2        21                                                                            4
090200131113GIACNODIS1     NONINVIARE                                                                    5
090300131113GIACNODIS2     NONINVIARE                                                                    6
090400131113GIACSIDIS1     NONINVIARE                                                                    7
090500131113GIACSIDIS2     NONINVIARE                                                                    8
090600151007COLNOTAFF      14        119                                                                 9
090700141110ARRPRICOL      NONINVIARE     ARRIVO PRIMO COLLO TERMINAL DISTRIBUZIONE  (10/001)            10
090800141110               NONINVIARE                                                                    10
090900141110               NONINVIARE                                                                    10
