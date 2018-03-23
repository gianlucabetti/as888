000100030325     H DECEDIT('0.') DATEDIT(*DMY.)
000200131004     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
000300991027
000400131010     FAZORG01L  IF   E           K DISK
000500041015     FTNTBE01L  IF   E           K DISK
000600031103     FTITAS30C  IF   E           K DISK
000700110103     FTISTB03L  UF   E           K DISK    commit
000800090323     FTIVAWWWT  UF A E             DISK    commit
000900031103
001000031103     D*-----------
001100031103     D* SCHIERE A PROGRAMMA
001200031103     D*-----------
001300131104     D WCAU            S             90    DIM(90) CTDATA PERRCD(1)
001400141107     D WDCT            S             90    DIM(4)  CTDATA PERRCD(1)
001500141110     D WTIS9           S             90    DIM(12) CTDATA PERRCD(1)
001600131104
001700131104
001800131104     D*-----------
001900131104     D* DS RIDEFINIZIONE SCHIERE A PROGRAMMA
002000131104     D*-----------
002100131104     D DS_WCAU         DS
002200131104     D  dsCAU_BRT                    15
002300141006     D  dsCAU_EVT                    10
002400141006     D  dsCAU_RSN                     5
002500131104     D  dsCAU_DES                    60
002600131104     D*
002700131104     D DS_WDCT         DS
002800131104     D  dsDCT_BRT                    15
002900141007     D  dsDCT_EVT                    10
003000141007     D  dsDCT_RSN                     5
003100141107     D  dsDCT_DES                    60
003200131104     D*
003300131104     D DS_WTIS9        DS
003400131104     D  dsTIS9_BRT                   15
003500141007     D  dsTIS9_EVT                   10
003600141007     D  dsTIS9_RSN                    5
003700141107     D  dsTIS9_DES                   60
003800131104
003900141107
004000141107
004100141107     D*******
004200141107     D* DS CLIENTE
004300141107     D*******
004400141107     D*
004500141107     D* BGM
004600141107     D*
004700141107     D CLI_DS_BGM      DS           100
004800141107     D  CD_BGM_RECTYP          1      3A   inz('BGM')
004900141107     D  CD_BGM_DOCCOD          4      6A   inz('77')
005000141107     D  CD_BGM_DOCNAM          7     16A   inz('SHP_STATUS')
005100141107     D  CD_BGM_DOCREF         17     31A   inz(*all'?')
005200141107     D  CD_BGM_DOCVER         32     40A   inz('BRT_001')
005300141107     D  CD_BGM_DOCFUN         41     43A   inz('9')
005400141107     D  CD_BGM_DOCDAT         44     55A   inz(*all'?')
005500141107     D  CD_BGM_TOTSHP         91     95A   inz(*all'0')
005600141107     D  CD_BGM_TOTSTS         96    100A   inz(*all'0')
005700141107     D*
005800141107     D* NAD
005900141107     D*
006000141107     D CLI_DS_NAD      DS           113
006100141107     D  CD_NAD_RECTYP          1      3A   inz('NAD')
006200141107     D  CD_NAD_MSGMIT          4     38A   inz('BRT Corriere Espresso S.p.A.')
006300141113     D  CD_NAD_MITCOD         39     55A   inz('40042')
006400141107     D  CD_NAD_NAZID          56     58A   inz('IT')
006500141107     D  CD_NAD_MSGRCV         59     93A   inz('Ceva Logistics Italia Srl')
006600141107     D  CD_NAD_RCVCOD         94    110A   inz('1')
006700141107     D*
006800141107     D* CNI
006900141107     D*
007000141107     D CLI_DS_CNI      DS          1025
007100141107     D  CD_CNI_RECTYP          1      3A   inz('CNI')
007200141107     D  CD_CNI_SEQUEN          4      8A   inz(*all'?')
007300141107     D  CD_CNI_SHPRMN          9     43A   inz(*all'?')
007400141107     D  CD_CNI_LNPDES         61    316A   inz('BRT_166_LANDRIANO_PV_27015_IT')
007500141107     D  CD_CNI_STSOWN        318    318A   inz('1')
007600141107     D  CD_CNI_STSCOD        319    321A   inz(*all'?')
007700141107     D  CD_CNI_STSDES        322    356A   inz(*all'?')
007800141107     D  CD_CNI_STSRSN        357    376A   inz(*all'?')
007900141107     D  CD_CNI_SHPRMA        412    428A   inz(*all'?')
008000141107     D  CD_CNI_SHPDSP        429    440A   inz(*all'?')
008100141107     D  CD_CNI_STSDAT        528    544A   inz(*all'?')
008200141107     D  CD_CNI_SHPOK         912    914A   inz(*all'?')
008300141107     D  CD_CNI_SHPRSD        915    949A   inz(*all'?')
008400141107     D  CD_CNI_FIRMA         950    984A   inz(*blanks)
008500141107     D  CD_CNI_SHPPKB        985    996A   inz(*all'0')
008600141107     D  CD_CNI_SHPVLB        997   1008A   inz(*all'0')
008700141107     D*
008800141107
008900141107
009000131104
009100041015     D*-----------
009200041015     D* SCHIERE
009300041015     D*-----------
009400131104     D skCAU_BRT       S             10    DIM(%elem(WCAU))
009500141006     D skCAU_EVT       S             10    DIM(%elem(WCAU))
009600141007     D skCAU_RSN       S              5    DIM(%elem(WCAU))
009700131104     D skCAU_DES       S             60    DIM(%elem(WCAU))
009800131104     D jCAU            S              3  0 INZ(*zeros)
009900131104     D skDCT_BRT       S              1    DIM(%elem(WDCT))
010000141007     D skDCT_EVT       S             10    DIM(%elem(WDCT))
010100141007     D skDCT_RSN       S              5    DIM(%elem(WDCT))
010200141107     D skDCT_DES       S             60    DIM(%elem(WDCT))
010300131104     D jDCT            S              3  0 INZ(*zeros)
010400131104     D skTIS9_BRT      S             10    DIM(%elem(WTIS9))
010500141007     D skTIS9_EVT      S             10    DIM(%elem(WTIS9))
010600141007     D skTIS9_RSN      S              5    DIM(%elem(WTIS9))
010700141107     D skTIS9_DES      S             60    DIM(%elem(WTIS9))
010800131104     D jTIS9           S              3  0 INZ(*zeros)
010900131104     D skTADCod        S              2    DIM(100)
011000131104     D skTADRag        S              1    DIM(100)
011100041015     D jTAD            S              3  0 INZ(*zeros)
011200030924
011300041015     D*-----------
011400041015     D* VARIABILI D WRK
011500041015     D*-----------
011600031103     D prmppi          S             50
011700030924     D prmesito        S              1
011800030924     D wrkesito        S                   like(prmesito)
011900031103     D j               S              2  0
012000130807     D wTADCod         S              2
012100131004     D campo_alfa      S            512
012200141107     D wStsEvt         S              3
012300141107     D wStsDes         S             35
012400141107     D wStsRsn         S             20
012500141107     D Conta_msg       S             10i 0
012600141107     D Conta_segm      S             10i 0
012700141107     D wNum005_0       S              5  0
012800141107     D wNum010_0       S             10  0
012900131004
013000131004
013100131004     D*-----------
013200131004     D* DEFINIZIONE DS ESTERNE
013300131004     D*-----------
013400131004     D trul33ds      e ds                  inz
013500131004     D kpjba         e ds                  inz
013600131004     D DTAD          e ds                  inz
013700160614     D DTA4A         E DS
013800131004
013900131004
014000031103     D*-----------
014100031103     D* VARIABILI RIFERITE AL DATA-BASE
014200031103     D*-----------
014300080618     D wKstbKSU        S                   like(stbKSU)
014400080618     D wKstbFTR        S                   like(stbFTR)
014500030924
014600110705
014700031103
014800031103     D*-----------
014900031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
015000031103     D*-----------
015100031103     D DSPPI           DS
015200080618     D  w§PPIKSU                      7  0
015300131004
015400141118
015500141118
015600141118     D*------------------
015700141118     D* LINKING A DEFINIZIONI ESTERNE
015800141118     D*------------------
015900141118     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
016000141118     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
016100160614     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
016200131004
016300030924
016400030325
016500030924     C* MAIN/
016600041015     C*
016700041015     C* Carica i dati tabellati
016800041015     C                   EXSR      cartab
016900140308     C*
017000140308     C* Spia dati da inviare
017100140308     C                   SETOFF                                       45
017200041015     C*
017300041015     C* Esegue traduzione
017400140308     C                   SETOFF                                       31
017500131004     C  N31              EXSR      chkRECFILMBR
017600131004     C  N31              EXSR      rtvID
017700131004     C  N31              EXSR      traduci
017800141107     C**  N31
017900141107     C**AN 45              EXSR      wriFooter
018000110705     C*
018100921023     C                   SETON                                        LR
018200131004
018300131004
018400131004
018500131004     C*----------------------------------------------------*
018600131004     C*  STACCO ID UNIVOCO
018700131004     C*----------------------------------------------------*
018800131004     C     RTVID         BEGSR
018900131004     C*
019000131004     C                   z-add     *zeros        currUNIQ_ID      15 0
019100131004     C*
019200131004     C* NSP => Stacco un numeratore da AZNUM
019300131004     C                   clear                   TRUL33DS
019400131004     C                   eval      I33OPE = *zeros
019500131004     C                   eval      I33CNU = 302
019600131004     C                   eval      I33NUM = 1
019700131004     C                   movel     TRUL33DS      KPJBU
019800131004     C                   call      'TRUL33R'
019900131004     C                   parm                    KPJBA
020000131004     C                   movel     KPJBU         TRUL33DS
020100131004     C                   if        O33ERR = *zeros
020200131004     C                   z-add     O33NRF        currUNIQ_ID
020300131004     C                   else
020400131004     C                   seton                                        31
020500131004     C                   endif
020600131004     C*
020700131004     C                   ENDSR
020800131004
020900131004
021000131004
021100131004     C     chkRECFILMBR  BEGSR
021200131004     C*
021300131004     C     *start        setll     TIVAWWWT
021400131004     C                   if        %found(TIVAWWWT)
021500131004     C                   seton                                        31
021600131004     C                   endif
021700131004     C*
021800131004     C                   ENDSR
021900131004
022000131004
022100131004
022200131004     C     wriOUTPUT     BEGSR
022300131004     C*
022400131004     C                   write     TIVAW000
022500131004     C*
022600131004     C                   ENDSR
022700131004
022800130807
022900041015
023000041015     C     cartab        BEGSR
023100041015     C*
023200041015     C* Caricamento tipi anomalia C/A
023300041015     C     'TAD'         SETLL     tntbe01l
023400041015     C                   IF        %equal(tntbe01l)
023500041015     C     'TAD'         READE     tntbe01l
023600041015     C                   DOW       not %eof(tntbe01l)
023700041015     C                   EVAL      DTAD = tbeUNI
023800041015     C                   ADD       1             jTAD
023900041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
024000041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
024100041015     C     'TAD'         READE     tntbe01l
024200041015     C                   ENDDO
024300041015     C                   ENDIF
024400131104     C*
024500131104     C* Carico la mappatura dei codici status personalizzati cliente
024600131104     C                   z-add     1             jCAU
024700131104     C                   dow       jCAU <= %elem(WCAU) and
024800131104     C                                     WCAU(jCAU) <> *blanks
024900131104     C                   eval      DS_WCAU = WCAU(jCAU)
025000131104     C                   eval      skCAU_BRT(jCAU) = dsCAU_BRT
025100141006     C                   eval      skCAU_EVT(jCAU) = dsCAU_EVT
025200141006     C                   eval      skCAU_RSN(jCAU) = dsCAU_RSN
025300131104     C                   eval      skCAU_DES(jCAU) = dsCAU_DES
025400131104     C                   add       1             jCAU
025500131104     C                   enddo
025600131104     C*
025700131104     C                   z-add     1             jDCT
025800131104     C                   dow       jDCT <= %elem(WDCT) and
025900131104     C                                     WDCT(jDCT) <> *blanks
026000131104     C                   eval      DS_WDCT = WDCT(jDCT)
026100131104     C                   eval      skDCT_BRT(jDCT) = dsDCT_BRT
026200141007     C                   eval      skDCT_EVT(jDCT) = dsDCT_EVT
026300141007     C                   eval      skDCT_RSN(jDCT) = dsDCT_RSN
026400141107     C                   eval      skDCT_DES(jDCT) = dsDCT_DES
026500131111     C                   add       1             jDCT
026600131104     C                   enddo
026700131104     C*
026800131104     C                   z-add     1             jTIS9
026900131104     C                   dow       jTIS9 <= %elem(WTIS9) and
027000131104     C                                      WTIS9(jTIS9) <> *blanks
027100131104     C                   eval      DS_WTIS9 = WTIS9(jTIS9)
027200131104     C                   eval      skTIS9_BRT(jTIS9) = dsTIS9_BRT
027300141007     C                   eval      skTIS9_EVT(jTIS9) = dsTIS9_EVT
027400141007     C                   eval      skTIS9_RSN(jTIS9) = dsTIS9_RSN
027500141107     C                   eval      skTIS9_DES(jTIS9) = dsTIS9_DES
027600131104     C                   add       1             jTIS9
027700131104     C                   enddo
027800131104     C*
027900131104     C                   ENDSR
028000131104
028100131004
028200131004
028300131004     C     Char_Speciali BEGSR
028400131111     C*
028500131111     C*  Controlla la presenza di caratteri Speciali (campi strutturali)
028600131111     C*
028700131111     C/EXEC SQL
028800131111     C+ SET :campo_alfa = Replace(:campo_alfa, '?', '??')
028900131111     C/END-EXEC
029000131111     C*
029100131111     C/EXEC SQL
029200131111     C+ SET :campo_alfa = Replace(:campo_alfa, '+', '?+')
029300131111     C/END-EXEC
029400131111     C*
029500131111     C/EXEC SQL
029600131111     C+ SET :campo_alfa = Replace(:campo_alfa, ':', '?:')
029700131111     C/END-EXEC
029800131111     C*
029900131111     C/EXEC SQL
030000131111     C+ SET :campo_alfa = Replace(:campo_alfa, ',', '?,')
030100131111     C/END-EXEC
030200131111     C*
030300131111     C/EXEC SQL
030400131111     C+ SET :campo_alfa = Replace(:campo_alfa, '''', '?''')
030500131111     C/END-EXEC
030600131004     C*
030700131004     C                   ENDSR
030800131004
030900131004
031000131004
031100131004     C     wriHeader     BEGSR
031200131010     C*
031300141107     C*  Tipo record 'BGM'
031400141107     C                   reset                   CLI_DS_BGM
031500141107     C                   eval      CD_BGM_DOCREF = %editc(currUNIQ_ID:'X')
031600141107     C                   eval      CD_BGM_DOCDAT = %editc(datcor8:'X') +
031700141107     C                                             %editc(oracor4:'X')
031800141107     C                   clear                   vawDTA
031900141107     C                   eval      vawDTA = CLI_DS_BGM
032000131004     C                   exsr      wriOUTPUT
032100141107     C*
032200141107     C*  Tipo record 'NAD'
032300141107     C                   reset                   CLI_DS_NAD
032400141107     C                   clear                   vawDTA
032500141107     C                   eval      vawDTA = CLI_DS_NAD
032600141107     C                   exsr      wriOUTPUT
032700131004     C*
032800131004     C                   ENDSR
032900131004
033000131004
033100131004
033200131004     C     wriFooter     BEGSR
033300131004     C*
033400141107     C***                clear                   vawDTA
033500131004     C*
033600141107     C***                exsr      wriOUTPUT
033700131004     C*
033800131004     C                   ENDSR
033900131004
034000030924
034100030924
034200991027     C     traduci       BEGSR
034300131004     C*
034400131004     C* Inizializzo i contatori a livello di transazione/messaggio
034500141107     C                   z-add     *zeros        Conta_msg
034600141110     C*
034700141110     C* Inizializzo i contatori a livello di segmento (in questo caso CNI)
034800141110     C                   z-add     *zeros        Conta_segm
034900031103     C*
035000031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
035100031103     C* di traduzione (trattasi d "post-traduzione")
035200031104     C                   EVAL      wKstbKSU = w§PPIKSU
035300031103     C                   EVAL      wKstbFTR = *blanks
035400110103     C     KEYstb03P     SETLL     tistb03l
035500110103     C                   IF        %equal(tistb03l)
035600110103     C     KEYstb03P     READE     tistb03l
035700991027     C*
035800031103     C* Ciclo x tuttii record del cliente da trasmettere
035900110103     C                   DOW       not %eof(tistb03l)
036000080618     C*
036100130807     C                   SETON                                        50
036200141118     C*
036300141118     C* Solo bolle originali e valide ai fini della spedizione
036400141118     C                   movel     *blanks       wEsito1           1
036500141118     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
036600141118     C                                                stbAAS
036700141118     C                                               :stbLNP
036800141118     C                                               :stbNRS
036900141118     C                                               :stbNSP
037000141118     C                                               :pOutLblTyp
037100141118     C                                               :pOutAnnoBO
037200141118     C                                               :pOutLineaParBO
037300141118     C                                               :pOutSerieBO
037400141118     C                                               :pOutNumSpedBO
037500141118     C                                               :pOutDcmBO
037600141118     C                                               :pOutCcaBO
037700141118     C                                               :pOutRblBO))
037800141118     C*
037900141118     C                   if        pOutLblTyp='O' AND pOutRblBO ='N'
038000031103     C*
038100031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
038200031103     C* deve restituire al cliente nel file tradotto
038300031103     C                   IF        stbFTR = *blanks                             * ridondante
038400110103     C*
038500110103     C* Gestisco solamente gli stati standard
038600130808     C                   IF        (stbTIS= '1' OR
038700130906     C                             (stbTIS= '9' AND
038800151117     C***                           (stbPRS= '1' OR stbPRS = '4'))) AND
038900151117     C                              (stbPRS= '1'                ))) AND
039000130808     C                             (%subst(stbFOP:1:1)= 'O'  OR
039100130808     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
039200131004     C*
039300131004     C* Per "sicurezza" non invio stati senza data
039400131004     C                   IF        stbDAS > *zeros
039500110103     C*
039600110103     C                   SETOFF                                       50
039700031103     C*
039800031103     C* Innanzitutto chaino la bolla direttamente su TITAS
039900031103     C     KEYtas30P     CHAIN     titas30c
040000031103     C                   IF        %found(titas30c)
040100160614     C*
040200160614     C* Reperisco NATURA MERCE
040300160614     C                   EXSR      RTVNAS
040400131010     C*
040500160614     C* Inizializzo la filiale di riferimento dello status corrente
040600131216     C                   MOVEL     *zeros        wPO               3 0
040700131213     C*
040800131213     C* In relazione alla fase corrente del processo di delivery ritorno o la LNP o la LNA
040900131213     C                   if        tasDTI = *zeros
041000131213     C                   eval      wPO = tasLNP
041100131213     C                   else
041200131213     C                   eval      wPO = tasLNA
041300131213     C                   endif
041400131213     C*
041500131213     C     wPO           CHAIN     azorg01l
041600131010     C                   IF        %found(azorg01l)
041700080618     C*
041800110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
041900140606     C                   SETOFF                                       80
042000110103     C                   IF        stbTIS = '1'
042100031103     C                   Z-ADD     1             j
042200131104     C     stbCOS        LOOKUP    skCAU_BRT(j)                           55
042300141006     C                   IF        %found AND skCAU_EVT(j) <> 'NONINVIARE'
042400141107     C                   EVAL      wStsEvt = skCAU_BRT(j)
042500141107     C                   EVAL      wStsDes = skCAU_DES(j)
042600141107     C                   EVAL      wStsRsn = skCAU_EVT(j) + skCAU_RSN(j)
042700141107     C                   SETOFF                                       50
042800140606     C                   SETON                                        80
042900031103     C                   ELSE
043000031103     C                   SETON                                        50
043100031103     C                   ENDIF
043200110103     C                   ENDIF
043300110103     C*
043400110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
043500140606     C                   IF        not *IN80
043600140514     C***                IF        stbTIS <> '1'
043700110103     C                   Z-ADD     1             j
043800131104     C     stbCOS        LOOKUP    skTIS9_BRT(j)                          55
043900141007     C                   IF        %found AND skTIS9_EVT(j) <> 'NONINVIARE'
044000141107     C                   EVAL      wStsEvt = skTIS9_BRT(j)
044100141107     C                   EVAL      wStsDes = skTIS9_DES(j)
044200141107     C                   EVAL      wStsRsn = skTIS9_EVT(j) + skTIS9_RSN(j)
044300140514     C                   SETOFF                                       50
044400140606     C                   SETON                                        80
044500110103     C                   ELSE
044600110103     C                   SETON                                        50
044700110103     C                   ENDIF
044800140514     C***                ENDIF
044900140606     C                   ENDIF
045000041015     C*
045100080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
045200130807     C                   IF        stbPRS =  '5'
045300080618     C*
045400130807     C                   SETOFF                                       50
045500080618     C*
045600080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
045700130807     C                   Z-ADD     1             jTAD
045800130807     C                   MOVEL(P)  stbCOS        wTADCod
045900130807     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
046000130807     C                   IF        %found
046100130807     C                   Z-ADD     1             j
046200131104     C     skTADRag(jTAD)LOOKUP    skDCT_BRT(j)                           70
046300130807     C                   IF        %found
046400141007     C                   IF        %trim(skDCT_EVT(j)) <> 'NONINVIARE'
046500141107     C                   EVAL      wStsEvt = skDCT_BRT(j)
046600141107     C                   EVAL      wStsDes = skDCT_DES(j)
046700141107     C                   EVAL      wStsRsn = skDCT_EVT(j) + skDCT_RSN(j)
046800140514     C                   SETOFF                                       50
046900131004     C                   ELSE
047000131004     C                   SETON                                        50
047100131004     C                   ENDIF
047200130807     C                   ELSE
047300130807     C                   SETON                                        50
047400130807     C                   ENDIF
047500130807     C                   ELSE
047600130807     C                   SETON                                        50
047700130807     C                   ENDIF
047800130807     C                   ENDIF
047900140308     C*
048000140308     C* Al primo dettaglio da inviare => scrivo anche testata
048100140313     C                   IF        not *IN50
048200140313     C  N45              EXSR      wriHeader
048300140308     C                   SETON                                        45
048400140313     C                   ENDIF
048500080618     C*
048600080618     C* Scarico il buffer d output
048700131004     C  N50              EXSR      wriStato
048800110103     C*
048900131010     C                   ENDIF
049000131004     C                   ENDIF
049100110103     C                   ENDIF
049200110103     C                   ENDIF
049300110103     C                   ENDIF
049400141118     C*
049500141118     C                   endif
049600031103     C*
049700031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
049800031103     C                   EVAL      stbFTR = 'T'
049900031103     C                   UPDATE    tistb000
050000991027     C*
050100110103     C     KEYstb03P     READE     tistb03l
050200031103     C                   SETOFF                                       50
050300030325     C                   ENDDO
050400031103     C                   ENDIF
050500991027     C*
050600030325     C                   EVAL      wrkesito = '0'
050700991027     C*
050800910830     C                   ENDSR
050900031103
051000031103
051100031103
051200131004     C     wriStato      BEGSR
051300131004     C*
051400131004     C* Incremento il contatore di messaggio (al interno della transazione)
051500131004     C                   eval      Conta_msg = Conta_msg + 1
051600131004     C*
051700131004     C* Verifico eventuali caratteri non consentiti dal EDIFACT
051800131104     C                   movel     *blanks       wAlfaRMA        512
051900131024     C                   movel     *blanks       wAlfaRSD        512
052000131004     C                   movel     *blanks       wAlfaLOD        512
052100131004     C                   movel     *blanks       wAlfaIND        512
052200131025     C                   movel     *blanks       wAlfaORGDES     512
052300131004     C* RMA
052400131004     C                   eval      campo_alfa = stbRMA
052500131004     C                   exsr      Char_Speciali
052600131004     C                   eval      wAlfaRMA = campo_alfa
052700131024     C* RSD
052800131024     C                   eval      campo_alfa = tasRSD
052900131024     C                   exsr      Char_Speciali
053000131024     C                   eval      wAlfaRSD = campo_alfa
053100131004     C* LOD
053200131004     C                   eval      campo_alfa = tasLOD
053300131004     C                   exsr      Char_Speciali
053400131004     C                   eval      wAlfaLOD = campo_alfa
053500131004     C* IND
053600131004     C                   eval      campo_alfa = tasIND
053700131004     C                   exsr      Char_Speciali
053800131004     C                   eval      wAlfaIND = campo_alfa
053900131025     C* ORGDES
054000131025     C                   eval      campo_alfa = orgDES
054100131025     C                   exsr      Char_Speciali
054200131025     C                   eval      wAlfaORGDES = campo_alfa
054300131004     C*
054400131004     C* Effettuo considerazioni sul peso (bolletttato/rilevato) così come da EDI precedente
054500131004     C* (ancora corretto ???)
054600131004     C                   z-add     *zeros        wPeso             7 1
054700131004     C                   if        tasPKB > tasPKC
054800131004     C                   eval      wPeso = tasPKB
054900131004     C                   else
055000131004     C                   eval      wPeso = tasPKC
055100131004     C                   endif
055200131004     C*
055300131004     C* Compongo segmenti per dati di dettaglio spedizione corrente
055400141107     C*
055500141107     C*  Tipo record 'CNI'
055600141107     C                   eval      Conta_segm  = Conta_segm  + 1
055700141107     C                   reset                   CLI_DS_CNI
055800141107     C                   z-add     Conta_segm    wNum005_0
055900160614     C***                z-add     stbRMN        wNum010_0
056000141107     C                   eval      CD_CNI_SEQUEN = %editc(wNum005_0:'X')
056100160614     C***                eval      CD_CNI_SHPRMN = %editc(wNum010_0:'X')
056200160614     C                   eval      CD_CNI_SHPRMN = %trim(§TA4ANAS)
056300141107     C                   eval      CD_CNI_STSCOD = wStsEvt
056400141107     C                   eval      CD_CNI_STSDES = wStsDes
056500141107     C                   eval      CD_CNI_STSRSN = wStsRsn
056600141107     C                   eval      CD_CNI_SHPRMA = stbRMA
056700141107     C                   eval      CD_CNI_SHPDSP = %editc(stbDSP:'X')
056800141107     C                   eval      CD_CNI_STSDAT = %editc(stbDAS:'X') +
056900141107     C                                             %editc(stbORS:'X') + '+0100'
057000141107     C                   if        %subst(stbCOS:1:6) = 'CONSOK'
057100141107     C                   eval      CD_CNI_SHPOK  = 'ST'
057200141107     C                   eval      CD_CNI_SHPRSD = tasRSD
057300141107     C                   else
057400141107     C                   eval      CD_CNI_SHPOK  = *blanks
057500141107     C                   eval      CD_CNI_SHPRSD = *blanks
057600141107     C                   endif
057700141107     C                   clear                   vawDTA
057800141107     C                   eval      vawDTA = CLI_DS_CNI
057900141107     C                   exsr      wriOUTPUT
058000031103     C*
058100031103     C                   ENDSR
058200160614
058300160614
058400160614
058500160614     C     RTVNAS        BEGSR
058600160614     C*
058700160614     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
058800160614     C                   CLEAR                   DTA4A
058900160614     C*
059000160614     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
059100160614     C                   CALL      'UBTA400R'
059200160614     C                   PARM      *blanks       UBTA4IOPZ
059300160614     C                   PARM      *blanks       UBTA4ITLA
059400160614     C                   PARM      tasAAS        UBTA4IAAS
059500160614     C                   PARM      tasLNP        UBTA4ILNP
059600160614     C                   PARM      tasNRS        UBTA4INRS
059700160614     C                   PARM      tasNSP        UBTA4INSP
059800160614     C                   PARM      'A'           UBTA4ITRC
059900160614     C                   PARM                    UBTA4OERR
060000160614     C                   PARM                    UBTA4ODS
060100160614     C                   PARM                    UBTA4OLEN
060200160614     C                   PARM                    UBTA4ODATI
060300160614     C*
060400160614     C                   IF        UBTA4OERR = *zeros
060500160614     C                   SELECT
060600160614     C* Gestione output tipo record 'A'
060700160614     C                   WHEN      UBTA4ODS = 'DTA4A'
060800160614     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
060900160614     C                   ENDSL
061000160614     C*
061100160614     C                   ENDIF
061200160614     C*
061300160614     C                   ENDSR
061400031103
061500031103
061600991027
061700991027      /TITLE Operazioni iniziali.
061800991027     C     *inzsr        BEGSR
061900991027     C*
062000991027     C     *ENTRY        PLIST
062100031103     C                   parm                    prmppi
062200991027     C     wrkesito      parm      wrkesito      prmesito
062300031103     C*
062400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
062500031103     C                   EVAL      DSPPI = prmppi
062600131004     C*
062700131004     C* Reperisco i valori variabili ma a livello di transazione corrente
062800131004     C* (tipicamente data e ora relativ al file generato)
062900141107     C*
063000141107     C* CALCOLA LA DATA CORRENTE
063100141107     C                   time                    wn14             14 0
063200141107     C                   movel     wn14          oracor6           6 0          *ORA 6
063300141107     C                   movel     wn14          oracor4           4 0          *ORA 4
063400141107     C                   z-add     *zeros        datcor8           8 0          *DATA CORRENTE AA/M/
063500141107     C                   eval      datcor8 = %dec(%date() : *ISO)
063600030709     C*
063700030709     C* Definizione chiavi
063800030924     C*
063900110103     C* Chiave su TISTB03L - Parziale
064000110103     C     KEYstb03P     KLIST
064100031104     C                   KFLD                    wKstbKSU
064200031103     C                   KFLD                    wKstbFTR
064300031103     C*
064400031103     C* Chiave su TITAS30C - Parziale
064500031103     C     KEYtas30P     KLIST
064600031103     C                   KFLD                    stbAAS
064700031103     C                   KFLD                    stbLNP
064800031103     C                   KFLD                    stbNRS
064900031103     C                   KFLD                    stbNSP
065000991027     C*
065100991027     C                   ENDSR
065200131104** WCAU - CAUSALI BRT / CLIENTE: 15 byte BRT / 15 byte CLI / 60 byte DESCRIZIONE ITA
065300141006A              20        209  L.AVVISO                                                     1
065400141006ARS            56        265  AVVISATO-RIMANDA SVINCOLO 1GG                                2 ex 32
065500141006AVV            20        209  LASCIATO AVVISO 1GG                                          3
065600141006AV2            20        209  LASCIATO AVVISO 2GG                                          4
065700141006AV3            20        209  LASCIATO AVVISO 3GG                                          5
065800141006AV5            20        209  LASCIATO AVVISO 5GG                                          6
065900141006AV7            20        209  LASCIATO AVVISO 7GG                                          7
066000141006AV9            20        209  LASCIATO AVVISO 9 GIORNI                                     8
066100141006A16            56        265  L.A. RIMANDA LO SVINCOLO                                     9 ex 32
066200141006A23            56        212  L.A. CHIUSO FINO AL                                         10
066300131113CPO            NONINVIARE     CAMBIO PORTO                                                11
066400141006DAN            20        18   COLLO/I DANNEGGIATO/I                                       12
066500141007DIR            20        53   DIROTTAMENTO                                                13
066600131113F              NONINVIARE     FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA            14
066700141006G              56        265  GIACENZA GENERICA TEMPORANEA                                15 ex 32
066800141006GEN            56        265  GIACENZA GENERICA                                           16 ex 32
066900131113G02            NONINVIARE     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA                 17
067000131113G03            NONINVIARE     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA                 18
067100131113G05            NONINVIARE     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA                    19
067200131113G09            NONINVIARE     ATTENDERE 9 GIORNI                                          20
067300141006IDD            14        117  CHIUSURA SPEDIZIONE CON IDD                                 21
067400141006MAN            20        117  COLLO/I MANCANTE/I                                          22
067500141006MIC            56        113  MESSA IN CONSEGNA                                           23
067600131113N              NONINVIARE     NON FATTA                                                   24
067700141006NAV            14        117  COLLO SCONDIZIONATO                                         25
067800131113NIC            NONINVIARE     TOLTA DALLA CONSEGNA                                        26
067900141118P              NONINVIARE     CONSEGNA PARZIALE      (23/117)                             27
068000141007PAT            20        58   FESTIVITA PATRONALE                                         28
068100141006R              NONINVIARE???  RISERVA                                                     29
068200131113RDC            NONINVIARE     RIPRISTINO                                                  30
068300141006RFD            56        18   RIFIUTATO                                                   31
068400141006RFM            56        117  RIFIUTATO                                                   32
068500141006RIC            20        209  RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO                    33
068600131113RIP            NONINVIARE     RIPRISTINO                                                  34
068700141006RS             NONINVIARE???  RISERVA                                                     35
068800141006T              20        211  CHIUSO PER TURNO                                            36
068900141006WA             NONINVIARE???  AMMANCO SENZA EVENTO                                        37
069000141006WAE            NONINVIARE???  AMMANCO CON EVENTO                                          38
069100141006WM             NONINVIARE???  MANCANZA SENZA EVENTO                                       39
069200141006WME            NONINVIARE???  MANCANZA CON EVENTO                                         40
069300141006WV             NONINVIARE???  AVARIA SENZA EVENTO                                         41
069400141006WVE            NONINVIARE???  AVARIA CON EVENTO                                           42
069500141007ZAL            20        63   ALLUVIONE                                                   43
069600141007ZBS            20        58   BLOCCO STRADALE                                             44
069700141007ZFR            20        58   FESTIVITÀ REGIONALE                                         45
069800141007ZMM            20        63   MARE MOLTO MOSSO                                            46
069900141007ZMP            20        58   MANIFESTAZIONE PUBBLICA                                     47
070000141007ZNV            20        63   NEVICATA ECCEZIONALE                                        48
070100141007ZSC            20        58   SCIOPERO                                                    49
070200141006001            56        265  RIFIUTA SENZA SPECIFICARE IL MOTIVO                         50
070300141006002            56        284  MERCE NON ORDINATA O NON CONFORME                           51
070400141006003            56        294  MERCE SPEDITA IN RITARDO                                    52
070500141006004            56        284  MERCE GIA' RICEVUTA CON PRECEDENTE INVIO                    53
070600141006005            56        294  MERCE SPEDITA CON TROPPO ANTICIPO                           54
070700141006006            56        265  MERCE RESA SENZA AUTORIZZAZIONE                             55
070800141006007            NONINVIARE???  DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE               56
070900141006008            56        292  DESTINATARIO NON PAGA IL C/ASSEGNO                          57
071000141006009            56        265  CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO          58 ex 32
071100141006010            NONINVIARE???  DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO             59
071200141006012            56        297  IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO              60
071300141006013            NONINVIARE???  DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO             61
071400141006014            NONINVIARE???  DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO                 62
071500141006016            56        265  DESTINATARIO RIMANDA LO SVINCOLO AL                         63 ex 32
071600141006017            20        209  DESTINATARIO ERA ASSENTE: LASCIATO AVVISO                   64
071700141006018            NONINVIARE???  DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO            65 ex 32
071800141006019            20        234  DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO      66
071900141006020            NONINVIARE???  DESTINATARIO RISULTA TRASFERITO                             67
072000141006021            20        234  DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO       68 ex 16
072100141006022            20        213  INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO           69
072200141006023            56        212  IL DESTINATARIO E' CHIUSO FINO AL                           70
072300141006024            20        212  IL DESTINATARIO E' CHIUSO PER FERIE                         71
072400141006025            NONINVIARE???  IL DESTINATARIO E' CHIUSO                                   72
072500141006026            56        265  IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO          73
072600141006027            NONINVIARE???  DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE          74
072700141006028            56        212  ESERCIZIO NON ANCORA IN ATTIVITA                            75
072800141006029            NONINVIARE???  IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO         76
072900141006030            NONINVIARE???  IL DESTINATARIO RIFIUTA LA MERCE                            77
073000141006031            NONINVIARE???  IL DESTINATARIO NON INTENDE                                 78
073100141006032            56        265  FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO          79 ex 32
073200141006033            NONINVIARE???  IL DESTINATARIO RICHIEDE                                    80 ex 32
073300141006034            56        265  SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE            81
073400141006035            56        241  DOCUMENTI INCOMPLETI O MANCANTI                             82
073500141006036            NONINVIARE???  IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA        83
073600141006037            56        265  RIFIUTA LA CONSEGNA TASSATIVA                               84
073700141114100            56        265  CONSEGNA RICHIESTA OLTRE 72 ORE                             85
073800141114101            56        265  A SEGUITO L.AVV.CONCORDATA CONSEGNA OLTRE 72 ORE            86
073900131113999            NONINVIARE     -                                                           87
074000141006CONSOK         21             CONSEGNA OK                                                 88
074100141007RESO           14        279  RESO                                                        89
074200131104                                                                                          90
074300131104** WDCT - TIPI ANOMALIE BRT/CLIENTE: 15 byte BRT / 15 byte CLI
074400141007A              NONINVIARE???  AMMANCO                                                        1
074500141007M              NONINVIARE???  MANCANZA                                                       2
074600141007V              NONINVIARE???  AVARIA                                                         3
074700141007E              NONINVIARE???  EVENTO AVVERSO                                                 4
074800131104** WTIS9 - CODICI EVENTO "PERSONALIZZATI" - BRT/CLIENTE: 15 byte BRT / 15 byte CLI
074900141107INCARICO1      1              MERCE PRESA IN CARICO                                          1
075000141107INCARICO2      1              MERCE PRESA IN CARICO                                          2
075100141110CONSOK1        NONINVIARE     CONSEGNA OK                                                    3
075200141110CONSOK2        NONINVIARE     CONSEGNA OK                                                    4
075300141107GIACNODIS1     NONINVIARE     GIACENZA IN ATTESA DISPOSIZIONI                                5
075400141107GIACNODIS2     NONINVIARE     GIACENZA IN ATTESA DISPOSIZIONI                                6
075500141107GIACSIDIS1     NONINVIARE     GIACENZA CON DISPOSIZIONI RICEVUTE                             7
075600141107GIACSIDIS2     NONINVIARE     GIACENZA CON DISPOSIZIONI RICEVUTE                             8
075700141107COLNOTAFF      14        117  MERCE MAI AFFIDATA                                             9
075800141110ARRPRICOL      NONINVIARE     ARRIVO PRIMO COLLO TERMINAL DISTRIBUZIONE  (10/001)            10
075900141110               NONINVIARE                                                                    11
076000141110               NONINVIARE                                                                    12
