000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200110929     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300991027
000400110103     FAZORG01L  IF   E           K DISK
000500041015     FTNTBE01L  IF   E           K DISK
000600110927     FTABEL00F  IF   E           K DISK
000700031103     FTITAS30C  IF   E           K DISK
000800110103     FTNCSB03L  IF   E           K DISK
000900110927     FTIGCP51L  IF   E           K DISK
001000110927     FTIGNP01L  IF   E           K DISK
001100110929     FFNARB01L  IF   E           K DISK    usropn
001200110929     F                                     extfile(LibFileARB01)
001300110929     FFNBLP01L  IF   E           K DISK    usropn
001400110929     F                                     extfile(LibFileBLP01)
001500110103     FTISTB03L  UF   E           K DISK    commit
001600090323     FTIVAWWWT  UF A E             DISK    commit
001700031103
001800031103     D*-----------
001900031103     D* SCHIERE A PROGRAMMA
002000031103     D*-----------
002100110927     D CAUBAR          S             10    DIM(100) CTDATA PERRCD(1)
002200110927     D CAUCLI          S             20    DIM(100) CTDATA PERRCD(1)
002300110927     D CAUDES          S             80    DIM(100) CTDATA PERRCD(1)
002400110927     D WDCTBAR         S              1    DIM(4)   CTDATA PERRCD(1)
002500110927     D WDCTCLI         S             20    DIM(4)   CTDATA PERRCD(1)
002600110927     D WDCTDES         S             80    DIM(4)   CTDATA PERRCD(1)
002700110927     D WCOD9BAR        S             10    DIM(10)  CTDATA PERRCD(1)
002800110927     D WCOD9CLI        S             20    DIM(10)  CTDATA PERRCD(1)
002900110927     D WCOD9DES        S             80    DIM(10)  CTDATA PERRCD(1)
003000041015
003100041015     D*-----------
003200041015     D* SCHIERE
003300041015     D*-----------
003400041015     D skTADCod        S              2    DIM(500)
003500041015     D skTADRag        S              1    DIM(500)
003600041015     D jTAD            S              3  0 INZ(*zeros)
003700030924
003800041015     D*-----------
003900041015     D* VARIABILI D WRK
004000041015     D*-----------
004100031103     D prmppi          S             50
004200030924     D prmesito        S              1
004300030924     D wrkesito        S                   like(prmesito)
004400031103     D j               S              2  0
004500110927     D i               s              3  0 inz
004600110927     D ii              s              3  0 inz
004700110927     D wTADCod         S              2
004800110927     D sk2Alavv        s             10    DIM(300)
004900110927     D sk2Agiac        s             10    DIM(300)
005000111121     D sk2A_2Z         s             10    DIM(300)
005100110927     D sk2Agiacd       s             80    DIM(300)
005200110929
005300110929
005400110929     D LibFileARB01    s             21A   inz
005500110929     D LibFileBLP01    s             21A   inz
005600110929     D currSysNeta     s              8A   inz('*NULL')
005700110929
005800041015
005900041015     D*-----------
006000041015     D* DEFINIZIONE DS ESTERNE
006100041015     D*-----------
006200041015     D DTAD          E DS
006300110927     D DS2A          E DS
006400111116     D DS2Z          E DS
006500031103
006600031103     D*-----------
006700031103     D* VARIABILI RIFERITE AL DATA-BASE
006800031103     D*-----------
006900080618     D wKstbKSU        S                   like(stbKSU)
007000080618     D wKstbFTR        S                   like(stbFTR)
007100110103     D wCAS            S                   like(csbCAS)
007200110927     D wDDP            S                   like(csbDDP)
007300110927     D wTPI            S                   like(csbTPI)
007400130522     D wAAS            S                   like(tasAAS)
007500130522     D wNRS            S                   like(tasNRS)
007600130522     D wNSP            S                   like(tasNSP)
007700110928     D wLNP            S                   like(tasLNP)
007800110928     D wLNA            S                   like(tasLNA)
007900110928     D wDCR            S                   like(tasDCR)
008000130522     D wTSP            S                   like(tasTSP)
008100130522     D wRSD            S                   like(tasRSD)
008200130522     D wIND            S                   like(tasIND)
008300130522     D wCAD            S                   like(tasCAD)
008400130522     D wLOD            S                   like(tasLOD)
008500130522     D wPRD            S                   like(tasPRD)
008600130522     D wNCL            S                   like(tasNCL)
008700130522     D wPKB            S                   like(tasPKB)
008800130522     D wVLB            S                   like(tasVLB)
008900130522     D wCBO            S                   like(tasCBO)
009000140423     D wGC1            S                   like(tasGC1)
009100140423     D wGC2            S                   like(tasGC2)
009200110929
009300110929     D sAAS            S                   like(stbAAS)
009400110929     D sLNP            S                   like(stbLNP)
009500110929     D sNRS            S                   like(stbNRS)
009600110929     D sNSP            S                   like(stbNSP)
009700030924
009800110103
009900110103     D*----------
010000110927     D* DS CLIENTE
010100110103     D*----------
010200110103     D DS_Cli_Status   DS
010300130522     D  DS_CS_CCM                     7  0 inz
010400130522     D  DS_CS_NumSped                12  0 inz
010500130522     D  DS_CS_DatSped                 8  0 inz
010600130522     D  DS_CS_SrvType                20    inz
010700130522     D  DS_CS_RSD                    35    inz
010800130522     D  DS_CS_IND                    35    inz
010900130522     D  DS_CS_CAD                     9    inz
011000130522     D  DS_CS_LOD                    35    inz
011100130522     D  DS_CS_PRD                     2    inz
011200130522     D  DS_CS_NCL                     5  0 inz
011300130522     D  DS_CS_PKB                     7  1 inz
011400130522     D  DS_CS_VLB                     5  3 inz
011500130522     D  DS_CS_Incotrm                20    inz
011600130522     D  DS_CS_RMN                    15  0 inz
011700130522     D  DS_CS_RMA                    15    inz
011800130522     D  DS_CS_DCR                     8  0 inz
011900130522     D  DS_CS_EveDat                  8  0 inz
012000130522     D  DS_CS_EveTim                  4  0 inz
012100130522     D  DS_CS_EveCod                 20    inz
012200130522     D  DS_CS_EveDes                 80    inz
012300130522     D  DS_CS_GiacMot               300    inz
012400140423     D  DS_CS_GC1                     2    inz
012500140423     D  DS_CS_GC2                     2    inz
012600130522     C*
012700130522     D  DS_CS_LavDat                  8  0 inz
012800130522     D  DS_CS_GiacDat                 8  0 inz
012900130522     D  DS_CS_LNP                    20    inz
013000130522     D  DS_CS_LNA                    20    inz
013100111020     D  DS_CS_EveDaOr                12  0 inz
013200110927     D  DS_CS_CasImp                 13  3 inz
013300110927     D  DS_CS_CasDatP                 8  0 inz
013400110927     D  DS_CS_CasTipI                 1    inz
013500120507     D  DS_CS_KeySPED                12    inz
013600110103
013700031103
013800031103     D*-----------
013900031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
014000031103     D*-----------
014100031103     D DSPPI           DS
014200080618     D  w§PPIKSU                      7  0
014300031103
014400110928
014500110928     D*------------------
014600110928     D* LINKING A DEFINIZIONI ESTERNE
014700110928     D*------------------
014800110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
014900110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
015000110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
015100110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
015200110928
015300030924
015400030325
015500030924     C* MAIN/
015600110929     C*
015700110929     C* Reperisco il sistema AS/400 corrente
015800110929     C                   callp     UBRTVNETA_Rtv(currSysNeta)
015900110929     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
016000110929     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
016100110929     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
016200110929     C                   else
016300110929     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
016400110929     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
016500110929     C                   endif
016600110929     C*
016700110929     C* Apertura file "overraidati"
016800110929     C                   if        not %open(FNARB01L)
016900110929     C                   open      FNARB01L
017000110929     C                   endif
017100110929     C                   if        not %open(FNBLP01L)
017200110929     C                   open      FNBLP01L
017300110929     C                   endif
017400110929     C*
017500041015     C*
017600041015     C* Carica i dati tabellati
017700041015     C                   EXSR      cartab
017800130522     C*
017900130522     C* Verifica se necessaria scrittura intestazioni colonne
018000130522     C                   EXSR      chkRECFILMBR
018100041015     C*
018200041015     C* Esegue traduzione
018300130522     C                   EXSR      traduci
018400110929     C*
018500110929     C* Chiusura file "overraidati"
018600110929     C                   if        %open(FNARB01L)
018700110929     C                   close     FNARB01L
018800110929     C                   endif
018900110929     C                   if        %open(FNBLP01L)
019000110929     C                   close     FNBLP01L
019100110929     C                   endif
019200110929     C*
019300921023     C                   SETON                                        LR
019400111121
019500111121
019600111121
019700111121     C     cartab        BEGSR
019800111121     C*
019900111121     C* Carico causali evento 2A divise x categoria (A=Lavv G=Giac)
020000111121     C                   clear                   sk2Alavv
020100111121     C                   clear                   sk2Agiac
020200111121     C                   clear                   sk2Agiacd
020300111121     C                   z-add     *zeros        i
020400111121     C                   z-add     *zeros        ii
020500111121     C                   eval      tblKUT = 1
020600111121     C                   eval      tblCOD = '2A'
020700111121     C     KEYtab00P     setll     tabel00f
020800111121     C     KEYtab00P     reade     tabel00f
020900111121     C                   dow       not %eof(tabel00f)
021000111121     C                   movel(p)  tblUNI        ds2a
021100111121     C                   if        §2aFTC = 'G'
021200111121     C                   add       1             i
021300111121     C                   eval      sk2Agiac(i)   = tblKEY
021400111121     C                   eval      sk2Agiacd(i)  = §2ADES
021500111121     C                   endif
021600111121     C                   if        §2aFTC = 'A'
021700111121     C                   add       1             ii
021800111121     C                   eval      sk2Alavv(ii)  = tblKEY
021900111121     C                   endif
022000111121     C     KEYtab00P     reade     tabel00f
022100111121     C                   enddo
022200111121     C*
022300111121     C* Carico causali evento 2Z x eventi "fittizi"
022400111121     C                   clear                   sk2A_2Z
022500111121     C                   z-add     *zeros        i
022600111121     C                   eval      tblKUT = 1
022700111121     C                   eval      tblCOD = '2Z'
022800111121     C     KEYtab00P     setll     tabel00f
022900111121     C     KEYtab00P     reade     tabel00f
023000111121     C                   dow       not %eof(tabel00f)
023100111121     C                   movel(p)  tblUNI        ds2z
023200111122     C                   if        §2ZCEP <> *blanks
023300111121     C                   add       1             i
023400111122     C                   eval      sk2A_2Z(i) = tblKEY
023500111121     C                   endif
023600111121     C     KEYtab00P     reade     tabel00f
023700111121     C                   enddo
023800111121     C*
023900111121     C* Caricamento tipi anomalia C/A
024000111121     C     'TAD'         SETLL     tntbe01l
024100111121     C                   IF        %equal(tntbe01l)
024200111121     C     'TAD'         READE     tntbe01l
024300111121     C                   DOW       not %eof(tntbe01l)
024400111121     C                   EVAL      DTAD = tbeUNI
024500111121     C                   ADD       1             jTAD
024600111121     C                   EVAL      skTADCod(jTAD) = tbeKE1
024700111121     C                   EVAL      skTADRag(jTAD) = §TADRAGR
024800111121     C     'TAD'         READE     tntbe01l
024900111121     C                   ENDDO
025000111121     C                   ENDIF
025100111121     C*
025200111121     C                   ENDSR
025300130522
025400130522
025500130522
025600130522     C     chkRECFILMBR  BEGSR
025700130522     C*
025800130522     C                   READ      TIVAWWWT                               55
025900130522     C*
026000130522     C                   ENDSR
026100991027
026200030924
026300030924
026400991027     C     traduci       BEGSR
026500130522     C*
026600130522     C* Se occorre scrivo una prima riga con l'intestazione dei campi
026700130522     C                   IF        *IN55
026800130522     C                   EVAL      VAWDTA =
026900130522     C                             'CUSTOMER CODE'                         +';'+
027000130522     C                             'SHIPMENT NUMBER'                       +';'+
027100130522     C                             'SHIPMENT DATE'                         +';'+
027200130522     C                             'SERVICE TYPE'                          +';'+
027300130522     C                             'CONSIGNEE NAME'                        +';'+
027400130522     C                             'CONSIGNEE ADDRESS'                     +';'+
027500130522     C                             'CONSIGNEE ZIP CODE'                    +';'+
027600130522     C                             'CONSIGNEE CITY'                        +';'+
027700130522     C                             'CONSIGNEE AREA CODE'                   +';'+
027800130522     C                             'SHIPMENT PARCELS'                      +';'+
027900130522     C                             'SHIPMENT WEIGHT KG'                    +';'+
028000130522     C                             'SHIPMENT VOLUME M3'                    +';'+
028100130522     C                             'INCOTERMS'                             +';'+
028200130522     C                             'CASH ON DELIVERY'                      +';'+
028300130522     C                             'SENDER REFERENCE ID 1'                 +';'+
028400130522     C                             'SENDER REFERENCE ID 2'                 +';'+
028500130522     C                             'REQUESTED DELIVERY DATE'               +';'+
028600130522     C                             'STATUS DATE'                           +';'+
028700130522     C                             'STATUS TIME'                           +';'+
028800130522     C                             'STATUS CODE'                           +';'+
028900130522     C                             'STATUS DESCRIPTION'                    +';'+
029000140506     C                             'STATUS NOTE'                           +';'+
029100140506     C                             '1ST CONSIGNEE CLOSING DAY'             +';'+
029200140506     C                             '2ND CONSIGNEE CLOSING DAY'             +';'
029300130522     C*
029400130522     C                   WRITE     TIVAW000
029500130522     C                   ENDIF
029600031103     C*
029700031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
029800031103     C* di traduzione (trattasi d "post-traduzione")
029900031104     C                   EVAL      wKstbKSU = w§PPIKSU
030000031103     C                   EVAL      wKstbFTR = *blanks
030100110103     C     KEYstb03P     SETLL     tistb03l
030200110103     C                   IF        %equal(tistb03l)
030300110103     C     KEYstb03P     READE     tistb03l
030400991027     C*
030500031103     C* Ciclo x tuttii record del cliente da trasmettere
030600110103     C                   DOW       not %eof(tistb03l)
030700130304     C*
030800130304     C* Ignoro completamente gli stati provenienti dai PDA ...
030900130304     C                   IF        stbPRS = '7'
031000130304     C                   ELSE
031100121012     C*
031200121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
031300121012     C                   IF        stbDAS*10000+stbORS >
031400121012     C                             %int((%dec(%date():*ISO)*1000000+
031500121012     C                             %dec(%time()))/100)
031600121012     C                   ELSE
031700110929     C*
031800111103     C* Come primissima cosa verifico rottura d codice su key spedizione
031900110929     C                   IF        stbAAS = sAAS AND
032000110929     C                             stbLNP = sLNP AND
032100110929     C                             stbNRS = sNRS AND
032200110929     C                             stbNSP = sNSP
032300110929     C                   ELSE
032400110929     C                   SETOFF                                       40
032500110929     C                   EVAL      sAAS = stbAAS
032600110929     C                   EVAL      sLNP = stbLNP
032700110929     C                   EVAL      sNRS = stbNRS
032800110929     C                   EVAL      sNSP = stbNSP
032900110929     C                   ENDIF
033000031014     C*
033100030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
033200110103     C                   RESET                   DS_Cli_Status
033300080618     C*
033400110927     C                   SETON                                        50
033500031103     C*
033600031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
033700031103     C* deve restituire al cliente nel file tradotto
033800111215     C                   IF        stbFTR = *blanks                             * ridondante
033900111215     C*
034000111215     C* Verifico (subito) se spedizione in giacenza
034100111215     C                   exsr      rtvgiac
034200110103     C*
034300111031     C* Gestisco solamente alcuni stati
034400110928     C                   IF        (stbTIS= '1' OR
034500111103     C                             (stbTIS= '9' AND stbPRS= '1'))
034600110103     C*
034700110103     C                   SETOFF                                       50
034800031103     C*
034900031103     C* Innanzitutto chaino la bolla direttamente su TITAS
035000031103     C     KEYtas30P     CHAIN     titas30c
035100031103     C                   IF        %found(titas30c)
035200110103     C*
035300110103     C                   clear                   wCAS
035400110927     C                   clear                   wDDP
035500110927     C                   clear                   wTPI
035600110929     C                   clear                   wDCR
035700130522     C                   clear                   wTSP
035800130522     C                   clear                   wRSD
035900130522     C                   clear                   wIND
036000130522     C                   clear                   wCAD
036100130522     C                   clear                   wLOD
036200130522     C                   clear                   wPRD
036300130522     C                   clear                   wNCL
036400130522     C                   clear                   wPKB
036500130522     C                   clear                   wVLB
036600130522     C                   clear                   wCBO
036700140423     C                   clear                   wGC1
036800140423     C                   clear                   wGC2
036900110103     C*
037000110103     C* Quindi verifico subito esistenza contrassegno
037100110103     C     KEYcsb03C     CHAIN     tncsb03l
037200110103     C                   IF        %found(tncsb03l)
037300110103     C                   eval      wCAS = csbCAS
037400110927     C                   eval      wDDP = csbDDP
037500110927     C                   eval      wTPI = csbTPI
037600110103     C                   ENDIF
037700110929     C*
037800110929     C* Considerazione su eventi DIR
037900110929     C                   IF        stbCOS = 'DIR'
038000110929     C                   SETON                                        40
038100110929     C                   ENDIF
038200080618     C*
038300110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
038400110927     C                   IF         stbTIS= '1' AND
038500110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
038600110927     C*
038700110927     C                   MOVEL     *blanks       wCOS             10
038800110927     C                   MOVEL     *blanks       wCAU             10
038900110927     C                   IF        stbPRS = '3'
039000110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
039100110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
039200110927     C                   ELSE
039300110927     C                   EVAL      wCOS = stbCOS
039400110927     C                   ENDIF
039500110927     C*
039600031103     C                   Z-ADD     1             j
039700110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
039800110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
039900110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
040000110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
040100110927     C*
040200110927     C* Verifico se evento lasciato avviso
040300110927     C                   z-add     1             ii
040400110927     C     wCOS          lookup    sk2Alavv(ii)                           55
040500110927     C                   if        %found
040600110927     C                   eval      DS_CS_LavDat = stbDAS
040700110927     C                   endif
040800110927     C*
040900110927     C* Se stato d tipo giacenza
041000110927     C                   if        stbPRS = '3'
041100110927     C                   eval      DS_CS_GiacDat = stbDAS
041200110927     C*
041300110927     C* Se apertura reperisco il motivo apertura giacenza
041400110927     C                   if        stbSTS = '1'
041500110927     C                   z-add     1             i
041600110927     C     wCAU          lookup    sk2Agiac(i)                            55
041700110927     C                   if        %found
041800110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
041900110927     C                   endif
042000111121     C                   endif
042100111121     C*
042200111121     C                   if        wGiac = 'S'
042300110927     C*
042400110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
042500110927     C     KEYgnp01_P    setll     tignp01l
042600110927     C                   if        %equal(tignp01l)
042700110927     C     KEYgnp01_P    reade     tignp01l
042800110927     C                   dow       not %eof(tignp01l)
042900110927     C*** concateno note giacenza
043000110927     C                   eval      DS_CS_GiacMot =
043100110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
043200110927     C     KEYgnp01_P    reade     tignp01l
043300110927     C                   enddo
043400110927     C                   endif
043500111121     C*
043600110927     C                   endif
043700110927     C*
043800110927     C                   endif
043900110927     C*
044000031103     C                   ELSE
044100031103     C                   SETON                                        50
044200031103     C                   ENDIF
044300110927     C                   ENDIF
044400110103     C*
044500110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
044600110927     C                   IF        stbTIS= '9' AND
044700110927     C                             stbPRS= '1'
044800110927     C*
044900110927     C                   SETOFF                                       50
045000110927     C*
045100110103     C                   Z-ADD     1             j
045200110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
045300110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
045400110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
045500110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
045600110103     C                   ELSE
045700110103     C                   SETON                                        50
045800110103     C                   ENDIF
045900110103     C                   ENDIF
046000041015     C*
046100080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
046200110927     C                   IF        stbTIS= '1' AND
046300110927     C                             stbPRS= '5'
046400080618     C*
046500110927     C                   SETON                                        50
046600080618     C*
046700080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
046800080618     C                   Z-ADD     1             jTAD
046900080618     C                   MOVEL(P)  stbCOS        wTADCod
047000080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
047100080618     C                   IF        %found
047200110927     C                   Z-ADD     1             j
047300080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
047400080618     C                   IF        %found
047500110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
047600110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
047700110927     C                   SETOFF                                       50
047800080618     C                   ENDIF
047900080618     C                   ENDIF
048000080618     C                   ENDIF
048100111121     C*
048200111121     C* Se evento "fittizio" e spedizione in giacenza => nn restituisco lo stato "fittizio"
048300111121     C                   z-add     1             i
048400111121     C     wCOS          lookup    sk2A_2Z                                55
048500120130     C***                if        %found and wGiac = 'S'
048600120130     C                   if        %found
048700111121     C                   SETON                                        50
048800111121     C                   endif
048900080618     C*
049000080618     C* Scarico il buffer d output
049100080618     C  N50              EXSR      WRIREC
049200110103     C*
049300110103     C                   ENDIF
049400110103     C                   ENDIF
049500110103     C                   ENDIF
049600031103     C*
049700031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
049800031103     C                   EVAL      stbFTR = 'T'
049900031103     C                   UPDATE    tistb000
050000121012     C*
050100121012     C                   ENDIF
050200130305     C                   ENDIF
050300991027     C*
050400110103     C     KEYstb03P     READE     tistb03l
050500031103     C                   SETOFF                                       50
050600030325     C                   ENDDO
050700031103     C                   ENDIF
050800991027     C*
050900030325     C                   EVAL      wrkesito = '0'
051000991027     C*
051100910830     C                   ENDSR
051200111121
051300111121
051400111121
051500111121     C     rtvgiac       BEGSR
051600111121     C*
051700111121     C                   movel     'N'           wGiac             1
051800111121     C*
051900111121     C* Costruisco la chiave sul file TIGCP51L
052000111121     C                   eval      gcpFRG = *zeros
052100111121     C*
052200111121     C* Reperisco le giacenze relative alla bolla corrente
052300111121     C     KEYgcp51_P    chain     tigcp51l
052400111121     C                   if        %found(tigcp51l)
052500111216     C                   if        gcpATB = *blanks AND gcpDCG = 0
052600111121     C                   movel     'S'           wGiac
052700111216     C                   endif
052800111121     C                   endif
052900111121     C*
053000111121     C                   ENDSR
053100031103
053200031103
053300031103
053400110103     C     wrirec        BEGSR
053500110928     C*
053600130522     C                   eval      wAAS = tasAAS
053700130522     C                   eval      wNRS = tasNRS
053800130522     C                   eval      wNSP = tasNSP
053900110928     C                   eval      wLNP = tasLNP
054000110928     C                   eval      wLNA = tasLNA
054100110928     C                   eval      wDCR = tasDCR
054200130522     C                   eval      wTSP = tasTSP
054300130522     C                   eval      wRSD = tasRSD
054400130522     C                   eval      wIND = tasIND
054500130522     C                   eval      wCAD = tasCAD
054600130522     C                   eval      wLOD = tasLOD
054700130522     C                   eval      wPRD = tasPRD
054800130522     C                   eval      wNCL = tasNCL
054900130522     C                   eval      wPKB = tasPKB
055000130522     C                   eval      wVLB = tasVLB
055100130522     C                   eval      wCBO = tasCBO
055200140423     C                   eval      wGC1 = tasGC1
055300140423     C                   eval      wGC2 = tasGC2
055400120229     C*
055500120229     C* Prendo sempre i dati più recenti da ambiente filiale
055600120229     C                   setoff                                       60
055700120229     C*
055800120229     C                   eval      pOutAnnoFI     = tasAAS
055900120229     C                   eval      pOutLineaParFI = tasLNP
056000120229     C                   eval      pOutSerieFI    = tasNRS
056100120229     C                   eval      pOutNumSpedFI  = tasNSP
056200120229     C*
056300120229     C* - 1° tentativo in filiale arrivo   (FNARB)
056400120229     C                   if        not *in60
056500120229     C     KEYarb01C     chain     fnarb01l
056600120229     C                   if        %found(fnarb01l)
056700120229     C                   seton                                        60
056800130522     C                   eval      wAAS   = arbAAS
056900130522     C                   eval      wNRS   = arbNRS
057000130522     C                   eval      wNSP   = arbNSP
057100120229     C                   eval      wLNP   = arbLNP
057200120229     C                   eval      wLNA   = arbLNA
057300120229     C                   if        arbDCR > wDCR
057400120229     C                   eval      wDCR   = arbDCR
057500120229     C                   endif
057600130522     C                   eval      wTSP = arbTSP
057700130522     C                   eval      wRSD = arbRSD
057800130522     C                   eval      wIND = arbIND
057900130522     C                   eval      wCAD = arbCAD
058000130522     C                   eval      wLOD = arbLOD
058100130522     C                   eval      wPRD = arbPRD
058200130522     C                   eval      wNCL = arbNCL
058300130522     C                   eval      wPKB = arbPKB
058400130522     C                   eval      wVLB = arbVLB
058500130522     C                   eval      wCBO = arbCBO
058600140423     C                   eval      wGC1 = arbGC1
058700140423     C                   eval      wGC2 = arbGC2
058800120229     C                   endif
058900120229     C                   endif
059000120229     C*
059100120229     C* - 2° tentativo in filiale partenza (FNBLP)
059200120229     C                   if        not *in60
059300120229     C     KEYarb01C     chain     fnblp01l
059400120229     C                   if        %found(fnblp01l)
059500120229     C                   seton                                        60
059600130522     C                   eval      wAAS   = blpAAS
059700130522     C                   eval      wNRS   = blpNRS
059800130522     C                   eval      wNSP   = blpNSP
059900120229     C                   eval      wLNP   = blpLNP
060000120229     C                   eval      wLNA   = blpLNA
060100120229     C                   if        blpDCR > wDCR
060200120229     C                   eval      wDCR   = blpDCR
060300120229     C                   endif
060400130522     C                   eval      wTSP = blpTSP
060500130522     C                   eval      wRSD = blpRSD
060600130522     C                   eval      wIND = blpIND
060700130522     C                   eval      wCAD = blpCAD
060800130522     C                   eval      wLOD = blpLOD
060900130522     C                   eval      wPRD = blpPRD
061000130522     C                   eval      wNCL = blpNCL
061100130522     C                   eval      wPKB = blpPKB
061200130522     C                   eval      wVLB = blpVLB
061300130522     C                   eval      wCBO = blpCBO
061400140423     C                   eval      wGC1 = blpGC1
061500140423     C                   eval      wGC2 = blpGC2
061600120229     C                   endif
061700120229     C                   endif
061800111103     C*
061900111103     C                   movel     *blanks       wEsito1           1
062000110928     C                   movel     *blanks       wEsito2           1
062100111103     C*
062200111103     C* Chiamata metodo GetLblTyp
062300111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
062400111103     C                                                stbAAS
062500111103     C                                               :stbLNP
062600111103     C                                               :stbNRS
062700111103     C                                               :stbNSP
062800111103     C                                               :pOutLblTyp
062900111103     C                                               :pOutAnnoBO
063000111103     C                                               :pOutLineaParBO
063100111103     C                                               :pOutSerieBO
063200111103     C                                               :pOutNumSpedBO
063300111103     C                                               :pOutDcmBO
063400111103     C                                               :pOutCcaBO
063500111103     C                                               :pOutRblBO))
063600111103     C*
063700111103     C                   if        wEsito1 = '0'
063800111103     C                   if        pOutCcaBO = '2' AND
063900111103     C                             stbDAS    > pOutDcmBO
064000130522     C***                eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
064100130522     C***                          ' COMMERCITY'
064200111103     C                   endif
064300111103     C                   endif
064400111103     C*
064500111103     C* Se necessario reperisco l'ultima bolla figlia
064600111103     C                   if        *in40
064700110928     C*
064800110928     C* Chiamata metodo GetLastChild
064900110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
065000110928     C                                                stbAAS
065100110928     C                                               :stbLNP
065200110928     C                                               :stbNRS
065300110928     C                                               :stbNSP
065400110928     C                                               :pOutAnnoFI
065500110928     C                                               :pOutLineaParFI
065600110928     C                                               :pOutSerieFI
065700110928     C                                               :pOutNumSpedFI
065800110928     C                                               :pOutDcmFI
065900110928     C                                               :pOutCcaFI))
066000110928     C*
066100110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
066200110928     C                   if        wEsito2 = '0'
066300110929     C*
066400110929     C                   setoff                                       60
066500110928     C*
066600110928     C* - 1° tentativo in filiale arrivo   (FNARB)
066700111216     C                   if        not *in60
066800110929     C     KEYarb01C     chain     fnarb01l
066900110929     C                   if        %found(fnarb01l)
067000110929     C                   seton                                        60
067100130522     C                   eval      wAAS   = arbAAS
067200130522     C                   eval      wNRS   = arbNRS
067300130522     C                   eval      wNSP   = arbNSP
067400110929     C                   eval      wLNP   = arbLNP
067500110929     C                   eval      wLNA   = arbLNA
067600110929     C                   if        arbDCR > wDCR
067700110929     C                   eval      wDCR   = arbDCR
067800110929     C                   endif
067900130522     C                   eval      wTSP = arbTSP
068000130522     C                   eval      wRSD = arbRSD
068100130522     C                   eval      wIND = arbIND
068200130522     C                   eval      wCAD = arbCAD
068300130522     C                   eval      wLOD = arbLOD
068400130522     C                   eval      wPRD = arbPRD
068500130522     C                   eval      wNCL = arbNCL
068600130522     C                   eval      wPKB = arbPKB
068700130522     C                   eval      wVLB = arbVLB
068800130522     C                   eval      wCBO = arbCBO
068900140423     C                   eval      wGC1 = arbGC1
069000140423     C                   eval      wGC2 = arbGC2
069100110929     C                   endif
069200111216     C                   endif
069300110928     C*
069400110928     C* - 2° tentativo in filiale partenza (FNBLP)
069500111216     C                   if        not *in60
069600110929     C     KEYarb01C     chain     fnblp01l
069700110929     C                   if        %found(fnblp01l)
069800110929     C                   seton                                        60
069900130522     C                   eval      wAAS   = blpAAS
070000130522     C                   eval      wNRS   = blpNRS
070100130522     C                   eval      wNSP   = blpNSP
070200110929     C                   eval      wLNP   = blpLNP
070300110929     C                   eval      wLNA   = blpLNA
070400110929     C                   if        blpDCR > wDCR
070500110929     C                   eval      wDCR   = blpDCR
070600110929     C                   endif
070700130522     C                   eval      wTSP = blpTSP
070800130522     C                   eval      wRSD = blpRSD
070900130522     C                   eval      wIND = blpIND
071000130522     C                   eval      wCAD = blpCAD
071100130522     C                   eval      wLOD = blpLOD
071200130522     C                   eval      wPRD = blpPRD
071300130522     C                   eval      wNCL = blpNCL
071400130522     C                   eval      wPKB = blpPKB
071500130522     C                   eval      wVLB = blpVLB
071600130522     C                   eval      wCBO = blpCBO
071700140423     C                   eval      wGC1 = blpGC1
071800140423     C                   eval      wGC2 = blpGC2
071900110929     C                   endif
072000111216     C                   endif
072100110928     C*
072200110928     C* - 3° tentativo in sede             (TITAS)
072300110929     C                   if        not *in60
072400110929     C     KEYarb01C     chain     titas30c
072500110929     C                   if        %found(titas30c)
072600130522     C                   eval      wAAS = tasAAS
072700130522     C                   eval      wNRS = tasNRS
072800130522     C                   eval      wNSP = tasNSP
072900130522     C                   eval      wLNP = tasLNP
073000130522     C                   eval      wLNA = tasLNA
073100130522     C                   eval      wDCR = tasDCR
073200130522     C                   eval      wTSP = tasTSP
073300130522     C                   eval      wRSD = tasRSD
073400130522     C                   eval      wIND = tasIND
073500130522     C                   eval      wCAD = tasCAD
073600130522     C                   eval      wLOD = tasLOD
073700130522     C                   eval      wPRD = tasPRD
073800130522     C                   eval      wNCL = tasNCL
073900130522     C                   eval      wPKB = tasPKB
074000130522     C                   eval      wVLB = tasVLB
074100130522     C                   eval      wCBO = tasCBO
074200140423     C                   eval      wGC1 = tasGC1
074300140423     C                   eval      wGC2 = tasGC2
074400110929     C                   endif
074500110929     C                   endif
074600110929     C*
074700110928     C                   endif
074800110929     C                   endif
074900110927     C*
075000110928     C     wLNP          chain     azorg01l
075100110927     C                   if        %found(azorg01l)
075200110927     C                   eval      DS_CS_LNP = orgDES
075300110927     C                   endif
075400110927     C*
075500110928     C     wLNA          chain     azorg01l
075600110927     C                   if        %found(azorg01l)
075700110927     C                   eval      DS_CS_LNA = orgDES
075800110927     C                   endif
075900130522     C*
076000130522     C                   eval      DS_CS_CCM     = stbKSC
076100130522     C                   eval      DS_CS_NumSped = pOutLineaParBO * 1000000000+
076200130522     C                                             pOutSerieBO    * 10000000  +
076300130522     C                                             pOutNumSpedBO
076400130522     C                   eval      DS_CS_DatSped = stbDSP
076500130522     C*
076600130522     C                   select
076700130522     C                   when      wTSP = 'C'
076800130522     C                   eval      DS_CS_SrvType = 'EXPRESS'
076900130522     C                   when      wTSP = 'E'
077000130522     C                   eval      DS_CS_SrvType = 'PRIORITY'
077100130522     C                   when      wTSP = 'H'
077200130522     C                   eval      DS_CS_SrvType = 'BRT 10:30'
077300130522     C                   when      wTSP = 'D'
077400130522     C                   eval      DS_CS_SrvType = 'DISTRIBUTION'
077500130522     C                   endsl
077600130522     C*
077700130522     C                   eval      DS_CS_RSD = wRSD
077800130522     C                   eval      DS_CS_IND = wIND
077900130522     C                   eval      DS_CS_CAD = wCAD
078000130522     C                   eval      DS_CS_LOD = wLOD
078100130522     C                   eval      DS_CS_PRD = wPRD
078200130522     C                   eval      DS_CS_NCL = wNCL
078300130522     C                   eval      DS_CS_PKB = wPKB
078400130522     C                   eval      DS_CS_VLB = wVLB
078500140423     C                   eval      DS_CS_GC1 = wGC1
078600140423     C                   eval      DS_CS_GC2 = wGC2
078700130522     C*
078800130522     C                   select
078900130522     C                   when      wCBO = '1'
079000130522     C                   eval      DS_CS_Incotrm = 'FREE DOMICILE'
079100130522     C                   when      wCBO = '2'
079200130522     C                   eval      DS_CS_Incotrm = 'EX WORKS'
079300130522     C                   when      wCBO = '4'
079400130522     C                   eval      DS_CS_Incotrm = 'FREE DOLICILE + COD'
079500130522     C                   when      wCBO = '6'
079600130522     C                   eval      DS_CS_Incotrm = 'EX WORKS + COD'
079700130522     C                   other
079800130522     C* Chaino la bolla originale (in sede) per "ereditare" il tipo porto
079900130522     C     KEYtas30P_O   chain     titas30c
080000130522     C                   if        %found(titas30c)
080100130522     C                   select
080200130522     C                   when      %subst(tasTBL:1:1)='F' and tasFCA=*blanks
080300130522     C                   eval      DS_CS_Incotrm = 'FREE DOMICILE'
080400130522     C                   when      %subst(tasTBL:1:1)='A' and tasFCA=*blanks
080500130522     C                   eval      DS_CS_Incotrm = 'EX WORKS'
080600130522     C                   when      %subst(tasTBL:1:1)='F' and tasFCA='S'
080700130522     C                   eval      DS_CS_Incotrm = 'FREE DOLICILE + COD'
080800130522     C                   when      %subst(tasTBL:1:1)='A' and tasFCA='S'
080900130522     C                   eval      DS_CS_Incotrm = 'EX WORKS + COD'
081000130522     C                   other
081100130522     C                   eval      DS_CS_Incotrm = 'INTERNAL SERVICE'
081200130522     C                   endsl
081300130522     C                   endif
081400130522     C*
081500130522     C                   endsl
081600130522     C*
081700110927     C                   eval      DS_CS_RMN     = stbRMN
081800130522     C                   eval      DS_CS_RMA     = stbRMA
081900130522     C                   eval      DS_CS_EveDat  = stbDAS
082000130522     C                   eval      DS_CS_EveTim  = stbORS
082100111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
082200110928     C                   eval      DS_CS_DCR     = wDCR
082300110929     C                   eval      DS_CS_CasImp  = wCAS
082400110927     C                   eval      DS_CS_CasDatP = wDDP
082500110927     C                   eval      DS_CS_CasTipI = wTPI
082600130522     C                   eval      DS_CS_KeySPED = %trim(%editc(wLNP:'X'))+
082700130522     C                                             %trim(%editc(wNRS:'X'))+
082800130522     C                                             %trim(%editc(wNSP:'X'))
082900031103     C*
083000031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
083100110927     C                   EVAL      VAWDTA =
083200130522     C                                    %trim(%editc(DS_CS_CCM:'X'))
083300130522     C                               +';'+%trim(%editc(DS_CS_NumSped:'X'))
083400130522     C                               +';'+%trim(%editc(DS_CS_DatSped:'X'))
083500130522     C                               +';'+%trim(DS_CS_SrvType)
083600130522     C                               +';'+%trim(DS_CS_RSD)
083700130522     C                               +';'+%trim(DS_CS_IND)
083800130522     C                               +';'+%trim(DS_CS_CAD)
083900130522     C                               +';'+%trim(DS_CS_LOD)
084000130522     C                               +';'+%trim(DS_CS_PRD)
084100130522     C                               +';'+%trim(%editc(DS_CS_NCL:'4'))
084200130522     C                               +';'+%trim(%editc(DS_CS_PKB:'4'))
084300130522     C                               +';'+%trim(%editc(DS_CS_VLB:'4'))
084400130522     C                               +';'+%trim(DS_CS_Incotrm)
084500130522     C                               +';'+%trim(%editc(DS_CS_CasImp:'4'))
084600130522     C                               +';'+%trim(%editc(DS_CS_RMN:'4'))
084700130522     C                               +';'+%trim(DS_CS_RMA)
084800130522     C                               +';'+%trim(%editc(DS_CS_DCR:'X'))
084900130522     C                               +';'+%trim(%editc(DS_CS_EveDat:'X'))
085000130522     C                               +';'+%trim(%editc(DS_CS_EveTim:'X'))
085100130522     C                               +';'+%trim(DS_CS_EveCod)
085200130522     C                               +';'+%trim(DS_CS_EveDes)
085300130522     C                               +';'+%trim(DS_CS_GiacMot)
085400140506     C                               +';'+%trim(DS_CS_GC1)
085500140506     C                               +';'+%trim(DS_CS_GC2)
085600130522     C                               +';'
085700110927     C*
085800031103     C                   WRITE     TIVAW000
085900031103     C*
086000031103     C                   ENDSR
086100031103
086200031103
086300991027
086400991027      /TITLE Operazioni iniziali.
086500991027     C     *inzsr        BEGSR
086600991027     C*
086700991027     C     *ENTRY        PLIST
086800031103     C                   parm                    prmppi
086900991027     C     wrkesito      parm      wrkesito      prmesito
087000031103     C*
087100031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
087200031103     C                   EVAL      DSPPI = prmppi
087300030709     C*
087400030709     C* Definizione chiavi
087500110927     C*
087600110927     C* Chiave su TABEL00F - Parziale
087700110927     C     KEYtab00P     KLIST
087800110927     C                   KFLD                    tblKUT
087900110927     C                   KFLD                    tblCOD
088000030924     C*
088100110103     C* Chiave su TISTB03L - Parziale
088200110103     C     KEYstb03P     KLIST
088300031104     C                   KFLD                    wKstbKSU
088400031103     C                   KFLD                    wKstbFTR
088500031103     C*
088600031103     C* Chiave su TITAS30C - Parziale
088700031103     C     KEYtas30P     KLIST
088800031103     C                   KFLD                    stbAAS
088900031103     C                   KFLD                    stbLNP
089000031103     C                   KFLD                    stbNRS
089100031103     C                   KFLD                    stbNSP
089200130522     C*
089300130522     C* Chiave su TITAS30C - Parziale
089400130522     C     KEYtas30P_O   KLIST
089500130522     C                   KFLD                    pOutAnnoBO
089600130522     C                   KFLD                    pOutLineaParBO
089700130522     C                   KFLD                    pOutSerieBO
089800130522     C                   KFLD                    pOutNumSpedBO
089900110929     C*
090000110929     C* Chiave su FNARB01L / FNBLP01L - Completa
090100110929     C     KEYarb01C     KLIST
090200110929     C                   KFLD                    pOutAnnoFI
090300110929     C                   KFLD                    pOutLineaParFI
090400110929     C                   KFLD                    pOutSerieFI
090500110929     C                   KFLD                    pOutNumSpedFI
090600110103     C*
090700110103     C* Chiave su TNCSB03L - Completa
090800110103     C     KEYcsb03C     KLIST
090900110103     C                   KFLD                    stbAAS
091000110103     C                   KFLD                    stbLNP
091100110103     C                   KFLD                    stbNRS
091200110103     C                   KFLD                    stbNSP
091300110927     C*
091400110927     C* Chiave su TIGCP51L - Parziale
091500110927     C     KEYgcp51_P    KLIST
091600110927     C                   KFLD                    stbAAS
091700110927     C                   KFLD                    stbLNP
091800110927     C                   KFLD                    stbNRS
091900110927     C                   KFLD                    stbNSP
092000110927     C                   KFLD                    gcpFRG
092100110927     C*
092200110927     C* Chiave su TIGNP01L - Parziale
092300110927     C     KEYgnp01_P    KLIST
092400110927     C                   KFLD                    gcpAGC
092500110927     C                   KFLD                    gcpFGC
092600110927     C                   KFLD                    gcpNGC
092700110927     C                   KFLD                    gcpFRG
092800991027     C*
092900991027     C                   ENDSR
093000110103** CAUBAR - CAUSALI BARTOLINI
093100110103A                       L.AVVISO
093200110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
093300110103AVV                     LASCIATO AVVISO 1GG
093400110103AV2                     LASCIATO AVVISO 2GG
093500110103AV3                     LASCIATO AVVISO 3GG
093600110103AV5                     LASCIATO AVVISO 5GG
093700110103AV7                     LASCIATO AVVISO 7GG
093800110103AV9                     LASCIATO AVVISO 9 GIORNI
093900110103A16                     L.A. RIMANDA LO SVINCOLO
094000110103A23                     L.A. CHIUSO FINO AL
094100110103CPO                     CAMBIO PORTO
094200110103DAN                     COLLO/I DANNEGGIATO/I
094300110103DIR                     DIROTTAMENTO
094400110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
094500110103G                       GIACENZA GENERICA TEMPORANEA
094600110103GEN                     GIACENZA GENERICA
094700110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
094800110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
094900110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
095000110103G09                     ATTENDERE 9 GIORNI
095100110103IDD                     CHIUSURA SPEDIZIONE CON IDD
095200110103MAN                     COLLO/I MANCANTE/I
095300110103MIC                     MESSA IN CONSEGNA
095400110103N                       NON FATTA
095500110103NAV                     COLLO SCONDIZIONATO
095600110103NIC                     TOLTA DALLA CONSEGNA
095700110103P                       CONSEGNA PARZIALE
095800110103PAT                     FESTIVITA PATRONALE
095900110103R                       RISERVA
096000110103RDC                     RIPRISTINO
096100110103RFD                     RIFIUTATO
096200110103RFM                     RIFIUTATO
096300110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
096400110103RIP                     RIPRISTINO
096500110103RS                      RISERVA
096600110103T                       CHIUSO PER TURNO
096700110103WA                      AMMANCO SENZA EVENTO
096800110103WAE                     AMMANCO CON EVENTO
096900110103WM                      MANCANZA SENZA EVENTO
097000110103WME                     MANCANZA CON EVENTO
097100110103WV                      AVARIA SENZA EVENTO
097200110103WVE                     AVARIA CON EVENTO
097300110103ZAL                     ALLUVIONE
097400110103ZBS                     BLOCCO STRADALE
097500110103ZFR                     FESTIVITÀ REGIONALE
097600110103ZMM                     MARE MOLTO MOSSO
097700110103ZMP                     MANIFESTAZIONE PUBBLICA
097800110103ZNV                     NEVICATA ECCEZIONALE
097900110103ZSC                     SCIOPERO
098000110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
098100110103002                     MERCE NON ORDINATA O NON CONFORME
098200110103003                     MERCE SPEDITA IN RITARDO
098300110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
098400110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
098500110103006                     MERCE RESA SENZA AUTORIZZAZIONE
098600110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
098700110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
098800110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
098900110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
099000110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
099100110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
099200110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
099300110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
099400110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
099500110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
099600110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
099700110103020                     DESTINATARIO RISULTA TRASFERITO
099800110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
099900110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
100000110103023                     IL DESTINATARIO E' CHIUSO FINO AL
100100110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
100200110103025                     IL DESTINATARIO E' CHIUSO
100300110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
100400110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
100500110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
100600110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
100700110103030                     IL DESTINATARIO RIFIUTA LA MERCE
100800110103031                     IL DESTINATARIO NON INTENDE
100900110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
101000110103033                     IL DESTINATARIO RICHIEDE
101100110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
101200110103035                     DOCUMENTI INCOMPLETI O MANCANTI
101300110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
101400110103037                     RIFIUTA LA CONSEGNA TASSATIVA
101500110103100                     -
101600110103101                     -
101700110103999                     -
101800110103CONSOK                  CONSEGNA OK
101900110103RESO                    RESO
102000110927                        -
102100110929GIACAPE                 APERTURA GIACENZA
102200110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
102300110927GIACEND                 CHIUSURA GIACENZA
102400130523ARRPRICOL               ARRIVO PRIMO COLLO (LNA)
102500110927                        -
102600110927                        -
102700110927                        -
102800110927                        -
102900110927                        -
103000110927                        -
103100110103** CAUCLI - CAUSALI CLIENTE
103200110926A                       L.AVVISO
103300110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
103400110926AVV                     LASCIATO AVVISO 1GG
103500110926AV2                     LASCIATO AVVISO 2GG
103600110926AV3                     LASCIATO AVVISO 3GG
103700110926AV5                     LASCIATO AVVISO 5GG
103800110926AV7                     LASCIATO AVVISO 7GG
103900110926AV9                     LASCIATO AVVISO 9 GIORNI
104000110926A16                     L.A. RIMANDA LO SVINCOLO
104100110926A23                     L.A. CHIUSO FINO AL
104200110926CPO                     CAMBIO PORTO
104300110926DAN                     COLLO/I DANNEGGIATO/I
104400110926DIR                     DIROTTAMENTO
104500110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
104600110926G                       GIACENZA GENERICA TEMPORANEA
104700110926GEN                     GIACENZA GENERICA
104800110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
104900110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
105000110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
105100110926G09                     ATTENDERE 9 GIORNI
105200110926IDD                     CHIUSURA SPEDIZIONE CON IDD
105300110926MAN                     COLLO/I MANCANTE/I
105400110926MIC                     MESSA IN CONSEGNA
105500110926N                       NON FATTA
105600110926NAV                     COLLO SCONDIZIONATO
105700110926NIC                     TOLTA DALLA CONSEGNA
105800110926P                       CONSEGNA PARZIALE
105900110926PAT                     FESTIVITA PATRONALE
106000110926R                       RISERVA
106100110926RDC                     RIPRISTINO
106200110926RFD                     RIFIUTATO
106300110926RFM                     RIFIUTATO
106400110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
106500110926RIP                     RIPRISTINO
106600110926RS                      RISERVA
106700110926T                       CHIUSO PER TURNO
106800110926WA                      AMMANCO SENZA EVENTO
106900110926WAE                     AMMANCO CON EVENTO
107000110926WM                      MANCANZA SENZA EVENTO
107100110926WME                     MANCANZA CON EVENTO
107200110926WV                      AVARIA SENZA EVENTO
107300110926WVE                     AVARIA CON EVENTO
107400110926ZAL                     ALLUVIONE
107500110926ZBS                     BLOCCO STRADALE
107600110926ZFR                     FESTIVITÀ REGIONALE
107700110926ZMM                     MARE MOLTO MOSSO
107800110926ZMP                     MANIFESTAZIONE PUBBLICA
107900110926ZNV                     NEVICATA ECCEZIONALE
108000110926ZSC                     SCIOPERO
108100110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
108200110926002                     MERCE NON ORDINATA O NON CONFORME
108300110926003                     MERCE SPEDITA IN RITARDO
108400110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
108500110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
108600110926006                     MERCE RESA SENZA AUTORIZZAZIONE
108700110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
108800110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
108900110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
109000110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
109100110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
109200110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
109300110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
109400110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
109500110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
109600110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
109700110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
109800110926020                     DESTINATARIO RISULTA TRASFERITO
109900110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
110000110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
110100110926023                     IL DESTINATARIO E' CHIUSO FINO AL
110200110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
110300110926025                     IL DESTINATARIO E' CHIUSO
110400110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
110500110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
110600110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
110700110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
110800110926030                     IL DESTINATARIO RIFIUTA LA MERCE
110900110926031                     IL DESTINATARIO NON INTENDE
111000110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
111100110926033                     IL DESTINATARIO RICHIEDE
111200110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
111300110926035                     DOCUMENTI INCOMPLETI O MANCANTI
111400110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
111500110926037                     RIFIUTA LA CONSEGNA TASSATIVA
111600110926NON INVIARE             -
111700110926NON INVIARE             -
111800110926NON INVIARE             -
111900110926CONSOK                  CONSEGNA OK
112000110926RESO                    RESO
112100110926
112200110927*AG                     APERTURA GIACENZA
112300110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
112400110927*CG                     CHIUSURA GIACENZA
112500130523ARRPRICOL               ARRIVO PRIMO COLLO (LNA)
112600110927                        -
112700110927                        -
112800110927                        -
112900110927                        -
113000110927                        -
113100110927                        -
113200110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
113300110103L.AVVISO
113400110103AVVISATO-RIMANDA SVINCOLO 1GG
113500110103LASCIATO AVVISO 1GG
113600110103LASCIATO AVVISO 2GG
113700110103LASCIATO AVVISO 3GG
113800110103LASCIATO AVVISO 5GG
113900110103LASCIATO AVVISO 7GG
114000110103LASCIATO AVVISO 9 GIORNI
114100110103L.A. RIMANDA LO SVINCOLO
114200110103L.A. CHIUSO FINO AL
114300110103CAMBIO PORTO
114400110103COLLO/I DANNEGGIATO/I
114500110103DIROTTAMENTO
114600110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
114700110103GIACENZA GENERICA TEMPORANEA
114800110103GIACENZA GENERICA
114900110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
115000110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
115100110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
115200110103ATTENDERE 9 GIORNI
115300110103CHIUSURA SPEDIZIONE CON IDD
115400110103COLLO/I MANCANTE/I
115500110103MESSA IN CONSEGNA
115600110103NON FATTA
115700110103COLLO SCONDIZIONATO
115800110103TOLTA DALLA CONSEGNA
115900110103CONSEGNA PARZIALE
116000110103FESTIVITA PATRONALE
116100110103RISERVA
116200110103RIPRISTINO
116300110103RIFIUTATO
116400110103RIFIUTATO
116500110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
116600110103RIPRISTINO
116700110103RISERVA
116800110103CHIUSO PER TURNO
116900110103AMMANCO SENZA EVENTO
117000110103AMMANCO CON EVENTO
117100110103MANCANZA SENZA EVENTO
117200110103MANCANZA CON EVENTO
117300110103AVARIA SENZA EVENTO
117400110103AVARIA CON EVENTO
117500110103ALLUVIONE
117600110103BLOCCO STRADALE
117700110103FESTIVITÀ REGIONALE
117800110103MARE MOLTO MOSSO
117900110103MANIFESTAZIONE PUBBLICA
118000110103NEVICATA ECCEZIONALE
118100110103SCIOPERO
118200110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
118300110103MERCE NON ORDINATA O NON CONFORME
118400110103MERCE SPEDITA IN RITARDO
118500110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
118600110103MERCE SPEDITA CON TROPPO ANTICIPO
118700110103MERCE RESA SENZA AUTORIZZAZIONE
118800110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
118900110103DESTINATARIO NON PAGA IL C/ASSEGNO
119000110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
119100110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
119200110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
119300110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
119400110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
119500110103DESTINATARIO RIMANDA LO SVINCOLO AL
119600110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
119700110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
119800110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
119900110103DESTINATARIO RISULTA TRASFERITO
120000110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
120100110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
120200110103IL DESTINATARIO E' CHIUSO FINO AL
120300110103IL DESTINATARIO E' CHIUSO PER FERIE
120400110103IL DESTINATARIO E' CHIUSO
120500110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
120600110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
120700110103ESERCIZIO NON ANCORA IN ATTIVITA
120800110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
120900110103IL DESTINATARIO RIFIUTA LA MERCE
121000110103IL DESTINATARIO NON INTENDE
121100110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
121200110103IL DESTINATARIO RICHIEDE
121300110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
121400110103DOCUMENTI INCOMPLETI O MANCANTI
121500110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
121600110103RIFIUTA LA CONSEGNA TASSATIVA
121700110103-
121800110103-
121900110103-
122000110103CONSEGNA OK
122100110103RESO
122200110927-
122300110927APERTURA GIACENZA
122400110927RICEVUTE DISPOSIZIONI GIACENZA
122500110927CHIUSURA GIACENZA
122600130523ARRIVO MERCE TERMINAL DISTRIBUZIONE
122700110927-
122800110927-
122900110927-
123000110927-
123100110927-
123200110927-
123300041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
123400110103A                                                                                            1
123500110103M                                                                                            2
123600110103V                                                                                            3
123700110103E                                                                                            4
123800080901** WDCTCLI - CAUSALI CLIENTE
123900110926TAD_A                                                                                        1
124000110926TAD_M                                                                                        2
124100110926TAD_V                                                                                        3
124200110926TAD_E                                                                                        4
124300110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
124400110927AMMANCO                                                                                      1
124500110927MANCA COLLO                                                                                  2
124600110927AVARIA                                                                                       3
124700110927EVENTO                                                                                       4
124800110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
124900110103INCARICO1                                                                                    1
125000110103INCARICO2                                                                                    2
125100110103CONSOK1                                                                                      3
125200110103CONSOK2                                                                                      4
125300110927GIACNODIS1                                                                                   5
125400110927GIACNODIS2                                                                                   6
125500110927GIACSIDIS1                                                                                   7
125600110927GIACSIDIS2                                                                                   8
125700110927                                                                                             9
125800110927                                                                                             10
125900110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
126000110926ACCETTAZIONE                                                                                 1
126100110926ACCETTAZIONE                                                                                 2
126200110103NON INVIARE                                                                                  3
126300110103NON INVIARE                                                                                  4
126400110927GIAC_APERTA                                                                                  5
126500110927GIAC_APERTA                                                                                  6
126600110927GIAC_DISPOS                                                                                  7
126700110927GIAC_DISPOS                                                                                  8
126800110927NON INVIARE                                                                                  9
126900110927NON INVIARE                                                                                  10
127000110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
127100110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
127200110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
127300110926NON INVIARE                                                                                  3
127400110926NON INVIARE                                                                                  4
127500110927APERTURA GIACENZA                                                                            5
127600110927APERTURA GIACENZA                                                                            6
127700110927RICEVUTE DISPOSIZIONI                                                                        7
127800110927RICEVUTE DISPOSIZIONI                                                                        8
127900110927NON INVIARE                                                                                  8
128000110927NON INVIARE                                                                                  10
