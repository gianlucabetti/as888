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
016200041015     C*
016300041015     C* Carica i dati tabellati
016400041015     C                   EXSR      cartab
016500140308     C*
016600140308     C* Spia dati da inviare
016700140308     C                   SETOFF                                       45
016800041015     C*
016900041015     C* Esegue traduzione
017000140308     C                   SETOFF                                       31
017100150113     C  N31              EXSR      SPLIT_PARMS
017200131004     C  N31              EXSR      chkRECFILMBR
017300131004     C  N31              EXSR      rtvID
017400140308     C***  N31              EXSR      wriHeader
017500131004     C  N31              EXSR      traduci
017600140308     C  N31
017700140308     CAN 45              EXSR      wriFooter
017800140308 xxx C***   31              EXSR      sndAlert
017900110705     C*
018000921023     C                   SETON                                        LR
018100131004
018200131004
018300131004
018400131004     C*----------------------------------------------------*
018500131004     C*  STACCO ID UNIVOCO
018600131004     C*----------------------------------------------------*
018700131004     C     RTVID         BEGSR
018800131004     C*
018900131004     C                   z-add     *zeros        currUNIQ_ID      15 0
019000131004     C*
019100131004     C* NSP => Stacco un numeratore da AZNUM
019200131004     C                   clear                   TRUL33DS
019300131004     C                   eval      I33OPE = *zeros
019400131004     C                   eval      I33CNU = 302
019500131004     C                   eval      I33NUM = 1
019600131004     C                   movel     TRUL33DS      KPJBU
019700131004     C                   call      'TRUL33R'
019800131004     C                   parm                    KPJBA
019900131004     C                   movel     KPJBU         TRUL33DS
020000131004     C                   if        O33ERR = *zeros
020100131004     C                   z-add     O33NRF        currUNIQ_ID
020200131004     C                   else
020300131004     C                   seton                                        31
020400131004     C                   endif
020500131004     C*
020600131004     C                   ENDSR
020700131004
020800131004
020900131004
021000131004     C     chkRECFILMBR  BEGSR
021100131004     C*
021200131004     C     *start        setll     TIVAWWWT
021300131004     C                   if        %found(TIVAWWWT)
021400131004     C                   seton                                        31
021500131004     C                   endif
021600131004     C*
021700131004     C                   ENDSR
021800131004
021900131004
022000131004
022100131004     C     wriOUTPUT     BEGSR
022200131004     C*
022300131004     C                   write     TIVAW000
022400131004     C*
022500131004     C                   ENDSR
022600131004
022700130807
022800041015
022900041015     C     cartab        BEGSR
023000041015     C*
023100041015     C* Caricamento tipi anomalia C/A
023200041015     C     'TAD'         SETLL     tntbe01l
023300041015     C                   IF        %equal(tntbe01l)
023400041015     C     'TAD'         READE     tntbe01l
023500041015     C                   DOW       not %eof(tntbe01l)
023600041015     C                   EVAL      DTAD = tbeUNI
023700041015     C                   ADD       1             jTAD
023800041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
023900041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
024000041015     C     'TAD'         READE     tntbe01l
024100041015     C                   ENDDO
024200041015     C                   ENDIF
024300131104     C*
024400131104     C* Carico la mappatura dei codici status personalizzati cliente
024500131104     C                   z-add     1             jCAU
024600131104     C                   dow       jCAU <= %elem(WCAU) and
024700131104     C                                     WCAU(jCAU) <> *blanks
024800131104     C                   eval      DS_WCAU = WCAU(jCAU)
024900131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
025000141006     C                   eval      skCAU_EVT(jCAU) = dsCAU_EVT
025100141006     C                   eval      skCAU_RSN(jCAU) = dsCAU_RSN
025200131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
025300131104     C                   add       1             jCAU
025400131104     C                   enddo
025500131104     C*
025600131104     C                   z-add     1             jDCT
025700131104     C                   dow       jDCT <= %elem(WDCT) and
025800131104     C                                     WDCT(jDCT) <> *blanks
025900131104     C                   eval      DS_WDCT = WDCT(jDCT)
026000131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
026100141007     C                   eval      skDCT_EVT(jDCT) = dsDCT_EVT
026200141007     C                   eval      skDCT_RSN(jDCT) = dsDCT_RSN
026300131111     C                   add       1             jDCT
026400131104     C                   enddo
026500131104     C*
026600131104     C                   z-add     1             jTIS9
026700131104     C                   dow       jTIS9 <= %elem(WTIS9) and
026800131104     C                                      WTIS9(jTIS9) <> *blanks
026900131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
027000131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
027100141007     C                   eval      skTIS9_EVT(jTIS9) = dsTIS9_EVT
027200141007     C                   eval      skTIS9_RSN(jTIS9) = dsTIS9_RSN
027300131104     C                   add       1             jTIS9
027400131104     C                   enddo
027500131104     C*
027600131104     C                   ENDSR
027700131104
027800131004
027900131004
028000131004     C     Char_Speciali BEGSR
028100131111     C*
028200131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
028300131111     C*
028400131111     C/EXEC SQL
028500131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
028600131111     C/END-EXEC
028700131111     C*
028800131111     C/EXEC SQL
028900131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
029000131111     C/END-EXEC
029100131111     C*
029200131111     C/EXEC SQL
029300131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
029400131111     C/END-EXEC
029500131111     C*
029600131111     C/EXEC SQL
029700131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
029800131111     C/END-EXEC
029900131111     C*
030000131111     C/EXEC SQL
030100131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
030200131111     C/END-EXEC
030300131004     C*
030400131004     C                   ENDSR
030500131004
030600131004
030700150113
030800150113     C     wriHeader     BEGSR
030900131004     C*
031000131004     C                   clear                   vawDTA
031100131010     C*
031200131004     C*  UNB -
031300131004     C                   eval      vawDTA = UNB_sgm
031400150113     C                              + %trim(w§UNB_ID)
031500131004     C                              +   piu
031600131004     C                              + %trim(currDate6inv)
031700131004     C                              +   diviso
031800131004     C                              + %trim(currHHMM)
031900131004     C                              +   piu
032000131004     C                              + %trim(
032100131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
032200131004     C                              +   apice
032300131004     C                   exsr      wriOUTPUT
032400131004     C*
032500131004     C                   ENDSR
032600131004
032700131004
032800131004
032900131004     C     wriFooter     BEGSR
033000131004     C*
033100131004     C                   clear                   vawDTA
033200131004     C*
033300131004     C*  UNZ -
033400131004     C                   eval      vawDTA = UNZ_sgm
033500131004     C                              + %trim(%editc(Conta_msg:'4'))
033600131004     C                              +   piu
033700131004     C                              + %trim(
033800131004     C                                  %subst(%editc(currUNIQ_ID:'X'):15-5:6))
033900131004     C                              +   apice
034000131004     C                   exsr      wriOUTPUT
034100131004     C*
034200131004     C                   ENDSR
034300131004
034400030924
034500030924
034600991027     C     traduci       BEGSR
034700131004     C*
034800131004     C* Inizializzo i contatori a livello di transazione/messaggio
034900131004     C                   z-add     *zeros        Conta_msg
035000031103     C*
035100031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
035200031103     C* di traduzione (trattasi d "post-traduzione")
035300150113     C                   MOVE(P)   w§PPIKSU      wKstbKSU
035400031103     C                   EVAL      wKstbFTR = *blanks
035500110103     C     KEYstb03P     SETLL     tistb03l
035600110103     C                   IF        %equal(tistb03l)
035700110103     C     KEYstb03P     READE     tistb03l
035800991027     C*
035900031103     C* Ciclo x tuttii record del cliente da trasmettere
036000110103     C                   DOW       not %eof(tistb03l)
036100131004     C*
036200131004     C* Inizializzo i contatori a livello di segmento
036300131004     C                   z-add     *zeros        Conta_segm
036400080618     C*
036500130807     C                   SETON                                        50
036600031103     C*
036700031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
036800031103     C* deve restituire al cliente nel file tradotto
036900031103     C                   IF        stbFTR = *blanks                             * ridondante
037000110103     C*
037100110103     C* Gestisco solamente gli stati standard
037200130808     C                   IF        (stbTIS= '1' OR
037300130906     C                             (stbTIS= '9' AND
037400151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
037500151117     C                              (stbPRS= '1'                ))) AND
037600130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
037700130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
037800131004     C*
037900131004     C* Per "sicurezza" non invio stati senza data
038000131004     C                   IF        stbDAS > *zeros
038100110103     C*
038200110103     C                   SETOFF                                       50
038300031103     C*
038400031103     C* Innanzitutto chaino la bolla direttamente su TITAS
038500031103     C     KEYtas30P     CHAIN     titas30c
038600031103     C                   IF        %found(titas30c)
038700131010     C*
038800131010     C* Innanzitutto chaino la bolla direttamente su TITAS
038900131216     C                   MOVEL     *zeros        wPO               3 0
039000131213     C*
039100131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
039200131213     C                   if        tasDTI = *zeros
039300131213     C                   eval      wPO = tasLNP
039400131213     C                   else
039500131213     C                   eval      wPO = tasLNA
039600131213     C                   endif
039700131213     C*
039800131213     C     wPO           CHAIN     azorg01l
039900131010     C                   IF        %found(azorg01l)
040000080618     C*
040100110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
040200140606     C                   SETOFF                                       80
040300110103     C                   IF        stbTIS = '1'
040400031103     C                   Z-ADD     1             j
040500131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
040600141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
040700141006     C                   EVAL      wStsEvt = skCAU_EVT(j)
040800141006     C                   EVAL      wStsRsn = skCAU_RSN(j)
040900140514     C                   SETOFF                                       50
041000140606     C                   SETON                                        80
041100031103     C                   ELSE
041200031103     C                   SETON                                        50
041300031103     C                   ENDIF
041400110103     C                   ENDIF
041500110103     C*
041600110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
041700140606     C                   IF        not *IN80
041800140514     C***                IF        stbTIS <> '1'
041900110103     C                   Z-ADD     1             j
042000131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
042100141007     C                   IF        %found AND skTIS9_EVT(j) <> 'NONINVIARE'
042200141007     C                   EVAL      wStsEvt = skTIS9_EVT(j)
042300141007     C                   EVAL      wStsRsn = skTIS9_RSN(j)
042400140514     C                   SETOFF                                       50
042500140606     C                   SETON                                        80
042600110103     C                   ELSE
042700110103     C                   SETON                                        50
042800110103     C                   ENDIF
042900140514     C***                ENDIF
043000140606     C                   ENDIF
043100041015     C*
043200080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
043300130807     C                   IF        stbPRS =  '5'
043400080618     C*
043500130807     C                   SETOFF                                       50
043600080618     C*
043700080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
043800130807     C                   Z-ADD     1             jTAD
043900130807     C                   MOVEL(P)  stbCOS        wTADCod
044000130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
044100130807     C                   IF        %found
044200130807     C                   Z-ADD     1             j
044300131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
044400130807     C                   IF        %found
044500141007     C                   IF        %trim(skDCT_EVT(j)) <> 'NONINVIARE'
044600141007     C                   EVAL      wStsEvt = skDCT_EVT(j)
044700141007     C                   EVAL      wStsRsn = skDCT_RSN(j)
044800140514     C                   SETOFF                                       50
044900131004     C                   ELSE
045000131004     C                   SETON                                        50
045100131004     C                   ENDIF
045200130807     C                   ELSE
045300130807     C                   SETON                                        50
045400130807     C                   ENDIF
045500130807     C                   ELSE
045600130807     C                   SETON                                        50
045700130807     C                   ENDIF
045800130807     C                   ENDIF
045900140308     C*
046000140308     C* Al primo dettaglio da inviare => scrivo anche testata
046100140313     C                   IF        not *IN50
046200140313     C  N45              EXSR      wriHeader
046300140308     C                   SETON                                        45
046400140313     C                   ENDIF
046500080618     C*
046600080618     C* Scarico il buffer d output
046700131004     C  N50              EXSR      wriStato
046800110103     C*
046900131010     C                   ENDIF
047000131004     C                   ENDIF
047100110103     C                   ENDIF
047200110103     C                   ENDIF
047300110103     C                   ENDIF
047400031103     C*
047500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
047600031103     C                   EVAL      stbFTR = 'T'
047700031103     C                   UPDATE    tistb000
047800991027     C*
047900110103     C     KEYstb03P     READE     tistb03l
048000031103     C                   SETOFF                                       50
048100030325     C                   ENDDO
048200031103     C                   ENDIF
048300991027     C*
048400030325     C                   EVAL      wrkesito = '0'
048500991027     C*
048600910830     C                   ENDSR
048700031103
048800031103
048900031103
049000131004     C     wriStato      BEGSR
049100131004     C*
049200131004     C* Incremento il contatore di messaggio (al interno della transazione)
049300131004     C                   eval      Conta_msg = Conta_msg + 1
049400131004     C*
049500131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
049600131104     C                   movel     *blanks       wAlfaRMA        512
049700131024     C                   movel     *blanks       wAlfaRSD        512
049800131004     C                   movel     *blanks       wAlfaLOD        512
049900131004     C                   movel     *blanks       wAlfaIND        512
050000131025     C                   movel     *blanks       wAlfaORGDES     512
050100131004     C* RMA
050200131004     C                   eval      campo_alfa = stbRMA
050300131004     C                   exsr      Char_Speciali
050400131004     C                   eval      wAlfaRMA = campo_alfa
050500131024     C* RSD
050600131024     C                   eval      campo_alfa = tasRSD
050700131024     C                   exsr      Char_Speciali
050800131024     C                   eval      wAlfaRSD = campo_alfa
050900131004     C* LOD
051000131004     C                   eval      campo_alfa = tasLOD
051100131004     C                   exsr      Char_Speciali
051200131004     C                   eval      wAlfaLOD = campo_alfa
051300131004     C* IND
051400131004     C                   eval      campo_alfa = tasIND
051500131004     C                   exsr      Char_Speciali
051600131004     C                   eval      wAlfaIND = campo_alfa
051700131025     C* ORGDES
051800131025     C                   eval      campo_alfa = orgDES
051900131025     C                   exsr      Char_Speciali
052000131025     C                   eval      wAlfaORGDES = campo_alfa
052100131004     C*
052200131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
052300131004     C* (ancora corretto ???)
052400131004     C                   z-add     *zeros        wPeso             7 1
052500131004     C                   if        tasPKB > tasPKC
052600131004     C                   eval      wPeso = tasPKB
052700131004     C                   else
052800131004     C                   eval      wPeso = tasPKC
052900131004     C                   endif
053000131004     C*
053100131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
053200131004     C*
053300131004     C*  UNH -
053400131004     C                   eval      Conta_segm  = Conta_segm  + 1
053500131007     C                   eval      vawDTA = UNH_sgm_1
053600131007     C                              + %trim(%editc(Conta_msg:'4'))
053700131007     C                              + UNH_sgm_2
053800131004     C                              +   apice
053900131004     C                   exsr      wriOUTPUT
054000131004     C*
054100131004     C*  BGM -
054200131004     C                   eval      Conta_segm  = Conta_segm  + 1
054300131004     C                   eval      vawDTA = BGM_sgm
054400131004     C                              + %trim(%editc(stbRMN:'4'))
054500131004     C                              +   apice
054600131004     C                   exsr      wriOUTPUT
054700131004     C*
054800150113     C*  NAD SF -
054900150113     C                   eval      Conta_segm  = Conta_segm  + 1
055000150113     C                   eval      vawDTA = NAD_SF
055100150113     C                              + %trim(w§NAD_SF)
055200150113     C***                eval      vawDTA = NAD_SF_1
055300131008     C***                           + NAD_SF_2
055400131008     C***                           + NAD_SF_3
055500131008     C***                           + NAD_SF_4
055600131008     C***                           + NAD_SF_5
055700150113     C                              +   apice
055800150113     C                   exsr      wriOUTPUT
055900131004     C*
056000131004     C*  NAD ST -
056100131004     C                   eval      Conta_segm  = Conta_segm  + 1
056200131004     C                   eval      vawDTA = NAD_ST
056300131025     C                              + %trim(wAlfaRSD)
056400131004     C                              +   piu
056500131004     C                              +   piu
056600131004     C                              + %trim(wAlfaIND)
056700131004     C                              +   piu
056800131004     C                              + %trim(wAlfaLOD)
056900131004     C                              +   piu
057000131004     C                              + %trim(tasPRD)
057100131004     C                              +   piu
057200131004     C                              + %trim(tasCAD)
057300131004     C                              +   piu
057400131004     C                              + 'IT'
057500131004     C                              +   apice
057600131004     C                   exsr      wriOUTPUT
057700131004     C*
057800131010     C*  LOC -
057900131004     C                   eval      Conta_segm  = Conta_segm  + 1
058000131004     C                   eval      vawDTA = LOC_sgm1
058100131010     C                              + %trim(%editc(orgFIL:'X'))
058200131004     C                              +   LOC_sgm2
058300131025     C                              + %trim(wAlfaORGDES)
058400131010     C                              +   LOC_sgm3
058500131004     C                              +   apice
058600131004     C                   exsr      wriOUTPUT
058700131004     C*
058800131004     C*  CNI -
058900131004     C                   eval      Conta_segm  = Conta_segm  + 1
059000131004     C                   eval      vawDTA = CNI_sgm
059100131004     C                              +   apice
059200131004     C                   exsr      wriOUTPUT
059300131004     C*
059400131004     C*  STS -
059500131004     C                   eval      Conta_segm  = Conta_segm  + 1
059600141006     C                   eval      vawDTA = STS_sgm
059700150113     C                              + %trim(wStsTyp) + '+'
059800150113     C                              + %trim(wStsEvt) + '+'
059900141006     C                              + %trim(wStsRsn)
060000131004     C                              +   apice
060100131004     C                   exsr      wriOUTPUT
060200131004     C*
060300131004     C*  RFF -
060400131004     C                   eval      Conta_segm  = Conta_segm  + 1
060500131004     C                   eval      vawDTA = RFF_sgm
060600131004     C                              + %trim(wAlfaRMA)
060700131004     C                              +   apice
060800131004     C                   exsr      wriOUTPUT
060900131004     C*
061000131004     C*  TDT -
061100131004     C                   eval      Conta_segm  = Conta_segm  + 1
061200131004     C                   eval      vawDTA = TDT_sgm
061300131004     C                              +   apice
061400131004     C                   exsr      wriOUTPUT
061500131004     C*
061600131004     C*  DTM -
061700131004     C                   eval      Conta_segm  = Conta_segm  + 1
061800131004     C                   eval      vawDTA = DTM_sgm
061900131007     C                              + %trim(%editc(stbDAS*10000+stbORS:'4'))
062000131007     C                              + '00'
062100131004     C                              +   diviso
062200131007     C                              + '204'
062300131004     C                              +   apice
062400131004     C                   exsr      wriOUTPUT
062500131004     C*
062600131004     C*  EQD -
062700131004     C                   eval      Conta_segm  = Conta_segm  + 1
062800131004     C                   eval      vawDTA = EQD_sgm
062900131004     C                              +   apice
063000131004     C                   exsr      wriOUTPUT
063100131004     C*
063200131004     C*  MEA -
063300131004     C                   eval      Conta_segm  = Conta_segm  + 1
063400131004     C                   eval      vawDTA = MEA_sgm
063500131004     C                              + %trim(%editc(wPeso:'3'))
063600131004     C                              + '0'
063700131004     C                              +   apice
063800131004     C                   exsr      wriOUTPUT
063900160215     C*
064000160215     C*  GID -
064100160215     C                   eval      Conta_segm  = Conta_segm  + 1
064200160215     C                   eval      vawDTA = GID_sgm1
064300160215     C                              + %trim(%editc(tasNCL:'4'))
064400160215     C                              +   GID_sgm2
064500160215     C                              +   apice
064600160215     C                   exsr      wriOUTPUT
064700131004     C*
064800131004     C*  UNT -
064900131004     C                   eval      Conta_segm  = Conta_segm  + 1
065000131004     C                   eval      vawDTA = UNT_sgm
065100131004     C                              + %trim(%editc(Conta_segm:'4'))
065200131004     C                              +   piu
065300131007     C                              + %trim(%editc(Conta_msg:'4'))
065400131004     C                              +   apice
065500131004     C                   exsr      wriOUTPUT
065600031103     C*
065700031103     C                   ENDSR
065800150113
065900150113
066000150113     C*----------------------------------------------------*
066100150113     C*  ESEGUE SPLITTING SECONDO PARAMETRI RICHIESTI
066200150113     C*----------------------------------------------------*
066300150113     C     SPLIT_PARMS   BEGSR
066400150113     C*
066500150113     C                   reset                   w§UNB_ID
066600150113     C                   reset                   w§NAD_SF
066700150113     C*
066800150113     C* Reperiscoi parametri relativi alle "costanti cliente" segmenti EDIFACT
066900150113     C                   CLEAR                   DVIREPE
067000150113     C                   EVAL      vireTRC = 'PE'
067100150114     C                   MOVE(P)   w§PPIDTI      vireDTI
067200150113     C     KEYvire1C     CHAIN     tivire1l
067300150113     C                   IF        %found(tivire1l)
067400150113     C                   EVAL      DVIREPE = vireFLO
067500150113     C*
067600150113     C* Se reperiti parametri => effettuo splitting
067700150113     C                   eval      InStringa = %trim(§VIREPEKPJ)
067800150113     C*
067900150113     C* Chiamo il *pgm d utilità x lo "split" della riga delle intestazioni
068000150113     C                   CALL      'XSPLIT2'
068100150113     C                   PARM                    InStringa
068200150113     C                   PARM      '§'           InSepara
068300150113     C                   PARM      '6'           InTypeOut
068400150113     C                   PARM      ''            wSkParam
068500150113     C                   PARM                    OutErrore
068600150113     C                   PARM                    OutMsg
068700150113     C                   MOVEA     wSkParam      SkSplitCSV_6
068800150113     C*
068900150113     C* Valorizzo i parametri EDIFACT dallo splittamento parametri di traduzione
069000150113     C                   z-add     1             i                 4 0
069100150113     C                   dow       i <= %elem(SkSplitCSV_6)
069200150113     C                   if        SkSplitCSV_6(i) = *blanks
069300150113     C                   leave
069400150113     C                   else
069500150113     C                   select
069600150113     C* - parametro UNB
069700150113     C                   when      %subst(SkSplitCSV_6(i):1:4) = 'UNB+'
069800150113     C                   eval      w§UNB_ID = %trim(%subst(SkSplitCSV_6(i):5))
069900150113     C* - parametro NAD+SF
070000150113     C                   when      %subst(SkSplitCSV_6(i):1:6) = 'NADSF+'
070100150113     C                   eval      w§NAD_SF = %trim(%subst(SkSplitCSV_6(i):7))
070200150113     C*
070300150113     C                   endsl
070400150113     C                   endif
070500150113     C*
070600150113     C                   add       1             i
070700150113     C                   enddo
070800150113     C*
070900150113     C                   ENDIF
071000150113     C*
071100150113     C                   ENDSR
071200031103
071300031103
071400991027
071500991027      /TITLE Operazioni iniziali.
071600991027     C     *inzsr        BEGSR
071700991027     C*
071800991027     C     *ENTRY        PLIST
071900150113     C                   parm                    prmppi
072000991027     C     wrkesito      parm      wrkesito      prmesito
072100031103     C*
072200031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
072300150113     C                   EVAL      DSPPI = prmppi
072400131004     C*
072500131004     C* Reperisco i valori variabili ma a livello di transazione corrente
072600131004     C* (tipicamente data e ora relativ al file generato)
072700131004     C                   eval      currDate6    =
072800131004     C                              UBFMTDATE_Convert(
072900131004     C                                  %editc(%dec(%date():*ISO):'X') :
073000131004     C                                  'YYYYMMDD' : 'YYMMDD')
073100131004     C                   eval      currDate6inv =
073200131004     C                              UBFMTDATE_Convert(
073300131004     C                                  %editc(%dec(%date():*ISO):'X') :
073400131004     C                                  'YYYYMMDD' : 'DDMMYY')
073500131004     C                   eval      currHHMM =
073600131004     C                              %subst(%editc(%dec(%time():*ISO):'X'):1:4)
073700030709     C*
073800030709     C* Definizione chiavi
073900030924     C*
074000110103     C* Chiave su TISTB03L - Parziale
074100110103     C     KEYstb03P     KLIST
074200031104     C                   KFLD                    wKstbKSU
074300031103     C                   KFLD                    wKstbFTR
074400031103     C*
074500031103     C* Chiave su TITAS30C - Parziale
074600031103     C     KEYtas30P     KLIST
074700031103     C                   KFLD                    stbAAS
074800031103     C                   KFLD                    stbLNP
074900031103     C                   KFLD                    stbNRS
075000031103     C                   KFLD                    stbNSP
075100150113     C*
075200150113     C* Chiave su TIVIRE1L - Parziale
075300150113     C     KEYvire1C     KLIST
075400150113     C                   KFLD                    w§PPIKSU
075500150113     C                   KFLD                    w§PPITIP
075600150114     C                   KFLD                    vireDTI
075700150113     C                   KFLD                    vireTRC
075800991027     C*
075900991027     C                   ENDSR
076000131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
076100141006A              20        209  L.AVVISO                                                     1
076200141006ARS            56        265  AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
076300141006AVV            20        209  LASCIATO AVVISO 1GG                                          3
076400141006AV2            20        209  LASCIATO AVVISO 2GG                                          4
076500141006AV3            20        209  LASCIATO AVVISO 3GG                                          5
076600141006AV5            20        209  LASCIATO AVVISO 5GG                                          6
076700141006AV7            20        209  LASCIATO AVVISO 7GG                                          7
076800141006AV9            20        209  LASCIATO AVVISO 9 GIORNI                                     8
076900141006A16            56        265  L.A. RIMANDA LO SVINCOLO                                     9 ex 32
077000141006A23            56        212  L.A. CHIUSO FINO AL                                         10
077100131113CPO            NONINVIARE     CAMBIO PORTO                                                11
077200141006DAN            20        18   COLLO/I DANNEGGIATO/I                                       12
077300141007DIR            20        53   DIROTTAMENTO                                                13
077400131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
077500141006G              56        265  GIACENZA GENERICA TEMPORANEA                                15 ex 32
077600141006GEN            56        265  GIACENZA GENERICA                                           16 ex 32
077700131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
077800131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
077900131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
078000131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
078100141006IDD            14        117  CHIUSURA SPEDIZIONE CON IDD                                 21
078200141006MAN            20        117  COLLO/I MANCANTE/I                                          22
078300141006MIC            56        113  MESSA IN CONSEGNA                                           23
078400131113N              NONINVIARE     NON FATTA                                                   24
078500141006NAV            14        117  COLLO SCONDIZIONATO                                         25
078600131113NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
078700141006P              23        117  CONSEGNA PARZIALE                                           27
078800160517PAT            20        57   FESTIVITA PATRONALE                                         28
078900141006R              NONINVIARE???  RISERVA                                                     29
079000131113RDC            NONINVIARE     RIPRISTINO                                                  30
079100141006RFD            56        18   RIFIUTATO                                                   31
079200141006RFM            56        117  RIFIUTATO                                                   32
079300141006RIC            20        209  RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
079400131113RIP            NONINVIARE     RIPRISTINO                                                  34
079500141006RS             NONINVIARE???  RISERVA                                                     35
079600141006T              20        211  CHIUSO PER TURNO                                            36
079700141006WA             NONINVIARE???  AMMANCO SENZA EVENTO                                        37
079800141006WAE            NONINVIARE???  AMMANCO CON EVENTO                                          38
079900141006WM             NONINVIARE???  MANCANZA SENZA EVENTO                                       39
080000141006WME            NONINVIARE???  MANCANZA CON EVENTO                                         40
080100141006WV             NONINVIARE???  AVARIA SENZA EVENTO                                         41
080200141006WVE            NONINVIARE???  AVARIA CON EVENTO                                           42
080300141007ZAL            20        63   ALLUVIONE                                                   43
080400160517ZBS            20        56   BLOCCO STRADALE                                             44
080500160517ZFR            20        57   FESTIVITÀ REGIONALE                                         45
080600141007ZMM            20        63   MARE MOLTO MOSSO                                            46
080700141007ZMP            20        58   MANIFESTAZIONE PUBBLICA                                     47
080800141007ZNV            20        63   NEVICATA ECCEZIONALE                                        48
080900160517ZSC            20        55   SCIOPERO                                                    49
081000160517001            56        266  RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
081100141006002            56        284  MERCE NON ORDINATA O NON CONFORME                           51
081200141006003            56        294  MERCE SPEDITA IN RITARDO                                    52
081300160517004            56        285  MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
081400141006005            56        294  MERCE SPEDITA CON TROPPO ANTICIPO                           54
081500160517006            56        266  MERCE RESA SENZA AUTORIZZAZIONE                             55
081600141006007            NONINVIARE???  DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
081700141006008            56        292  DESTINATARIO NON PAGA IL C/ASSEGNO                          57
081800141006009            56        265  CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
081900141006010            NONINVIARE???  DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
082000141006012            56        297  IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
082100141006013            NONINVIARE???  DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
082200141006014            NONINVIARE???  DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
082300141006016            56        265  DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
082400141006017            20        209  DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
082500141006018            NONINVIARE???  DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
082600141006019            20        234  DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
082700141006020            NONINVIARE???  DESTINATARIO RISULTA TRASFERITO                             67
082800141006021            20        234  DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
082900141006022            20        213  INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
083000141006023            56        212  IL DESTINATARIO E' CHIUSO FINO AL                           70
083100141006024            20        212  IL DESTINATARIO E' CHIUSO PER FERIE                         71
083200141006025            NONINVIARE???  IL DESTINATARIO E' CHIUSO                                   72
083300160517026            56        268  IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
083400141006027            NONINVIARE???  DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
083500160517028            56        211  ESERCIZIO NON ANCORA IN ATTIVITA                            75
083600141006029            NONINVIARE???  IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
083700141006030            NONINVIARE???  IL DESTINATARIO RIFIUTA LA MERCE                            77
083800141006031            NONINVIARE???  IL DESTINATARIO NON INTENDE                                 78
083900141006032            56        265  FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
084000141006033            NONINVIARE???  IL DESTINATARIO RICHIEDE                                    80 ex 32
084100160517034            56        267  SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
084200141006035            56        241  DOCUMENTI INCOMPLETI O MANCANTI                             82
084300141006036            NONINVIARE???  IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
084400160517037            56        266  RIFIUTA LA CONSEGNA TASSATIVA                               84
084500141114100            56        265  CONSEGNA RICHIESTA OLTRE 72 ORE                             85
084600141114101            56        265  A SEGUITO L.AVV.CONCORDATA CONSEGNA OLTRE 72 ORE            86
084700141114999            NONINVIARE     -                                                           87
084800141006CONSOK         21             CONSEGNA OK                                                 88
084900141007RESO           14        279  RESO                                                        89
085000131104                                                                                          90
085100131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
085200141007A              NONINVIARE???  AMMANCO                                                        1
085300141007M              NONINVIARE???  MANCANZA                                                       2
085400141007V              NONINVIARE???  AVARIA                                                         3
085500141007E              NONINVIARE???  EVENTO AVVERSO                                                 4
085600131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
085700141007INCARICO1      1                                                                             1
085800141007INCARICO2      1                                                                             2
085900141007CONSOK1        21                                                                            3
086000141007CONSOK2        21                                                                            4
086100131113GIACNODIS1     NONINVIARE                                                                    5
086200131113GIACNODIS2     NONINVIARE                                                                    6
086300131113GIACSIDIS1     NONINVIARE                                                                    7
086400131113GIACSIDIS2     NONINVIARE                                                                    8
086500160517COLNOTAFF      14        116                                                                 9
086600141110ARRPRICOL      NONINVIARE     ARRIVO PRIMO COLLO TERMINAL DISTRIBUZIONE  (10/001)            10
086700141110               NONINVIARE                                                                    10
086800141110               NONINVIARE                                                                    10
