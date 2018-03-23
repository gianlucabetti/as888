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
009000140328     D wTC1            S                   like(tasFTC)
009100140328     D wTC2            S                   like(tasTC2)
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
012400130522     C*
012500130522     D  DS_CS_LavDat                  8  0 inz
012600130522     D  DS_CS_GiacDat                 8  0 inz
012700130522     D  DS_CS_LNP                    20    inz
012800130522     D  DS_CS_LNA                    20    inz
012900111020     D  DS_CS_EveDaOr                12  0 inz
013000110927     D  DS_CS_CasImp                 13  3 inz
013100110927     D  DS_CS_CasDatP                 8  0 inz
013200110927     D  DS_CS_CasTipI                 1    inz
013300120507     D  DS_CS_KeySPED                12    inz
013400110103
013500031103
013600031103     D*-----------
013700031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
013800031103     D*-----------
013900031103     D DSPPI           DS
014000080618     D  w§PPIKSU                      7  0
014100031103
014200110928
014300110928     D*------------------
014400110928     D* LINKING A DEFINIZIONI ESTERNE
014500110928     D*------------------
014600110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
014700110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
014800110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
014900110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
015000110928
015100030924
015200030325
015300030924     C* MAIN/
015400110929     C*
015500110929     C* Reperisco il sistema AS/400 corrente
015600110929     C                   callp     UBRTVNETA_Rtv(currSysNeta)
015700110929     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
015800110929     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
015900110929     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
016000110929     C                   else
016100110929     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
016200110929     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
016300110929     C                   endif
016400110929     C*
016500110929     C* Apertura file "overraidati"
016600110929     C                   if        not %open(FNARB01L)
016700110929     C                   open      FNARB01L
016800110929     C                   endif
016900110929     C                   if        not %open(FNBLP01L)
017000110929     C                   open      FNBLP01L
017100110929     C                   endif
017200110929     C*
017300041015     C*
017400041015     C* Carica i dati tabellati
017500041015     C                   EXSR      cartab
017600130522     C*
017700130522     C* Verifica se necessaria scrittura intestazioni colonne
017800130522     C                   EXSR      chkRECFILMBR
017900041015     C*
018000041015     C* Esegue traduzione
018100130522     C                   EXSR      traduci
018200110929     C*
018300110929     C* Chiusura file "overraidati"
018400110929     C                   if        %open(FNARB01L)
018500110929     C                   close     FNARB01L
018600110929     C                   endif
018700110929     C                   if        %open(FNBLP01L)
018800110929     C                   close     FNBLP01L
018900110929     C                   endif
019000110929     C*
019100921023     C                   SETON                                        LR
019200111121
019300111121
019400111121
019500111121     C     cartab        BEGSR
019600111121     C*
019700111121     C* Carico causali evento 2A divise x categoria (A=Lavv G=Giac)
019800111121     C                   clear                   sk2Alavv
019900111121     C                   clear                   sk2Agiac
020000111121     C                   clear                   sk2Agiacd
020100111121     C                   z-add     *zeros        i
020200111121     C                   z-add     *zeros        ii
020300111121     C                   eval      tblKUT = 1
020400111121     C                   eval      tblCOD = '2A'
020500111121     C     KEYtab00P     setll     tabel00f
020600111121     C     KEYtab00P     reade     tabel00f
020700111121     C                   dow       not %eof(tabel00f)
020800111121     C                   movel(p)  tblUNI        ds2a
020900111121     C                   if        §2aFTC = 'G'
021000111121     C                   add       1             i
021100111121     C                   eval      sk2Agiac(i)   = tblKEY
021200111121     C                   eval      sk2Agiacd(i)  = §2ADES
021300111121     C                   endif
021400111121     C                   if        §2aFTC = 'A'
021500111121     C                   add       1             ii
021600111121     C                   eval      sk2Alavv(ii)  = tblKEY
021700111121     C                   endif
021800111121     C     KEYtab00P     reade     tabel00f
021900111121     C                   enddo
022000111121     C*
022100111121     C* Carico causali evento 2Z x eventi "fittizi"
022200111121     C                   clear                   sk2A_2Z
022300111121     C                   z-add     *zeros        i
022400111121     C                   eval      tblKUT = 1
022500111121     C                   eval      tblCOD = '2Z'
022600111121     C     KEYtab00P     setll     tabel00f
022700111121     C     KEYtab00P     reade     tabel00f
022800111121     C                   dow       not %eof(tabel00f)
022900111121     C                   movel(p)  tblUNI        ds2z
023000111122     C                   if        §2ZCEP <> *blanks
023100111121     C                   add       1             i
023200111122     C                   eval      sk2A_2Z(i) = tblKEY
023300111121     C                   endif
023400111121     C     KEYtab00P     reade     tabel00f
023500111121     C                   enddo
023600111121     C*
023700111121     C* Caricamento tipi anomalia C/A
023800111121     C     'TAD'         SETLL     tntbe01l
023900111121     C                   IF        %equal(tntbe01l)
024000111121     C     'TAD'         READE     tntbe01l
024100111121     C                   DOW       not %eof(tntbe01l)
024200111121     C                   EVAL      DTAD = tbeUNI
024300111121     C                   ADD       1             jTAD
024400111121     C                   EVAL      skTADCod(jTAD) = tbeKE1
024500111121     C                   EVAL      skTADRag(jTAD) = §TADRAGR
024600111121     C     'TAD'         READE     tntbe01l
024700111121     C                   ENDDO
024800111121     C                   ENDIF
024900111121     C*
025000111121     C                   ENDSR
025100130522
025200130522
025300130522
025400130522     C     chkRECFILMBR  BEGSR
025500130522     C*
025600130522     C                   READ      TIVAWWWT                               55
025700130522     C*
025800130522     C                   ENDSR
025900991027
026000030924
026100030924
026200991027     C     traduci       BEGSR
026300130522     C*
026400130522     C* Se occorre scrivo una prima riga con l'intestazione dei campi
026500130522     C                   IF        *IN55
026600130522     C                   EVAL      VAWDTA =
026700130522     C                             'CUSTOMER CODE'                         +';'+
026800130522     C                             'SHIPMENT NUMBER'                       +';'+
026900130522     C                             'SHIPMENT DATE'                         +';'+
027000130522     C                             'SERVICE TYPE'                          +';'+
027100130522     C                             'CONSIGNEE NAME'                        +';'+
027200130522     C                             'CONSIGNEE ADDRESS'                     +';'+
027300130522     C                             'CONSIGNEE ZIP CODE'                    +';'+
027400130522     C                             'CONSIGNEE CITY'                        +';'+
027500130522     C                             'CONSIGNEE AREA CODE'                   +';'+
027600130522     C                             'SHIPMENT PARCELS'                      +';'+
027700130522     C                             'SHIPMENT WEIGHT KG'                    +';'+
027800130522     C                             'SHIPMENT VOLUME M3'                    +';'+
027900130522     C                             'INCOTERMS'                             +';'+
028000130522     C                             'CASH ON DELIVERY'                      +';'+
028100130522     C                             'SENDER REFERENCE ID 1'                 +';'+
028200130522     C                             'SENDER REFERENCE ID 2'                 +';'+
028300130522     C                             'REQUESTED DELIVERY DATE'               +';'+
028400140328     C                             'SPECIAL SRV. 1ST'                      +';'+
028500140328     C                             'SPECIAL SRV. 2ND'                      +';'+
028600130522     C                             'STATUS DATE'                           +';'+
028700130522     C                             'STATUS TIME'                           +';'+
028800130522     C                             'STATUS CODE'                           +';'+
028900130522     C                             'STATUS DESCRIPTION'                    +';'+
029000130522     C                             'STATUS NOTE'                           +';'
029100130522     C*
029200130522     C                   WRITE     TIVAW000
029300130522     C                   ENDIF
029400031103     C*
029500031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
029600031103     C* di traduzione (trattasi d "post-traduzione")
029700031104     C                   EVAL      wKstbKSU = w§PPIKSU
029800031103     C                   EVAL      wKstbFTR = *blanks
029900110103     C     KEYstb03P     SETLL     tistb03l
030000110103     C                   IF        %equal(tistb03l)
030100110103     C     KEYstb03P     READE     tistb03l
030200991027     C*
030300031103     C* Ciclo x tuttii record del cliente da trasmettere
030400110103     C                   DOW       not %eof(tistb03l)
030500130304     C*
030600130304     C* Ignoro completamente gli stati provenienti dai PDA ...
030700130304     C                   IF        stbPRS = '7'
030800130304     C                   ELSE
030900121012     C*
031000121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
031100121012     C                   IF        stbDAS*10000+stbORS >
031200121012     C                             %int((%dec(%date():*ISO)*1000000+
031300121012     C                             %dec(%time()))/100)
031400121012     C                   ELSE
031500110929     C*
031600111103     C* Come primissima cosa verifico rottura d codice su key spedizione
031700110929     C                   IF        stbAAS = sAAS AND
031800110929     C                             stbLNP = sLNP AND
031900110929     C                             stbNRS = sNRS AND
032000110929     C                             stbNSP = sNSP
032100110929     C                   ELSE
032200110929     C                   SETOFF                                       40
032300110929     C                   EVAL      sAAS = stbAAS
032400110929     C                   EVAL      sLNP = stbLNP
032500110929     C                   EVAL      sNRS = stbNRS
032600110929     C                   EVAL      sNSP = stbNSP
032700110929     C                   ENDIF
032800031014     C*
032900030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
033000110103     C                   RESET                   DS_Cli_Status
033100080618     C*
033200110927     C                   SETON                                        50
033300031103     C*
033400031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
033500031103     C* deve restituire al cliente nel file tradotto
033600111215     C                   IF        stbFTR = *blanks                             * ridondante
033700111215     C*
033800111215     C* Verifico (subito) se spedizione in giacenza
033900111215     C                   exsr      rtvgiac
034000110103     C*
034100111031     C* Gestisco solamente alcuni stati
034200110928     C                   IF        (stbTIS= '1' OR
034300111103     C                             (stbTIS= '9' AND stbPRS= '1'))
034400110103     C*
034500110103     C                   SETOFF                                       50
034600031103     C*
034700031103     C* Innanzitutto chaino la bolla direttamente su TITAS
034800031103     C     KEYtas30P     CHAIN     titas30c
034900031103     C                   IF        %found(titas30c)
035000110103     C*
035100110103     C                   clear                   wCAS
035200110927     C                   clear                   wDDP
035300110927     C                   clear                   wTPI
035400110929     C                   clear                   wDCR
035500130522     C                   clear                   wTSP
035600130522     C                   clear                   wRSD
035700130522     C                   clear                   wIND
035800130522     C                   clear                   wCAD
035900130522     C                   clear                   wLOD
036000130522     C                   clear                   wPRD
036100130522     C                   clear                   wNCL
036200130522     C                   clear                   wPKB
036300130522     C                   clear                   wVLB
036400130522     C                   clear                   wCBO
036500140328     C                   clear                   wTC1
036600140328     C                   clear                   wTC2
036700110103     C*
036800110103     C* Quindi verifico subito esistenza contrassegno
036900110103     C     KEYcsb03C     CHAIN     tncsb03l
037000110103     C                   IF        %found(tncsb03l)
037100110103     C                   eval      wCAS = csbCAS
037200110927     C                   eval      wDDP = csbDDP
037300110927     C                   eval      wTPI = csbTPI
037400110103     C                   ENDIF
037500110929     C*
037600110929     C* Considerazione su eventi DIR
037700110929     C                   IF        stbCOS = 'DIR'
037800110929     C                   SETON                                        40
037900110929     C                   ENDIF
038000080618     C*
038100110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
038200110927     C                   IF         stbTIS= '1' AND
038300110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
038400110927     C*
038500110927     C                   MOVEL     *blanks       wCOS             10
038600110927     C                   MOVEL     *blanks       wCAU             10
038700110927     C                   IF        stbPRS = '3'
038800110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
038900110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
039000110927     C                   ELSE
039100110927     C                   EVAL      wCOS = stbCOS
039200110927     C                   ENDIF
039300110927     C*
039400031103     C                   Z-ADD     1             j
039500110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
039600110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
039700110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
039800110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
039900110927     C*
040000110927     C* Verifico se evento lasciato avviso
040100110927     C                   z-add     1             ii
040200110927     C     wCOS          lookup    sk2Alavv(ii)                           55
040300110927     C                   if        %found
040400110927     C                   eval      DS_CS_LavDat = stbDAS
040500110927     C                   endif
040600110927     C*
040700110927     C* Se stato d tipo giacenza
040800110927     C                   if        stbPRS = '3'
040900110927     C                   eval      DS_CS_GiacDat = stbDAS
041000110927     C*
041100110927     C* Se apertura reperisco il motivo apertura giacenza
041200110927     C                   if        stbSTS = '1'
041300110927     C                   z-add     1             i
041400110927     C     wCAU          lookup    sk2Agiac(i)                            55
041500110927     C                   if        %found
041600110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
041700110927     C                   endif
041800111121     C                   endif
041900111121     C*
042000111121     C                   if        wGiac = 'S'
042100110927     C*
042200110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
042300110927     C     KEYgnp01_P    setll     tignp01l
042400110927     C                   if        %equal(tignp01l)
042500110927     C     KEYgnp01_P    reade     tignp01l
042600110927     C                   dow       not %eof(tignp01l)
042700110927     C*** concateno note giacenza
042800110927     C                   eval      DS_CS_GiacMot =
042900110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
043000110927     C     KEYgnp01_P    reade     tignp01l
043100110927     C                   enddo
043200110927     C                   endif
043300111121     C*
043400110927     C                   endif
043500110927     C*
043600110927     C                   endif
043700110927     C*
043800031103     C                   ELSE
043900031103     C                   SETON                                        50
044000031103     C                   ENDIF
044100110927     C                   ENDIF
044200110103     C*
044300110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
044400110927     C                   IF        stbTIS= '9' AND
044500110927     C                             stbPRS= '1'
044600110927     C*
044700110927     C                   SETOFF                                       50
044800110927     C*
044900110103     C                   Z-ADD     1             j
045000110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
045100110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
045200110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
045300110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
045400110103     C                   ELSE
045500110103     C                   SETON                                        50
045600110103     C                   ENDIF
045700110103     C                   ENDIF
045800041015     C*
045900080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
046000110927     C                   IF        stbTIS= '1' AND
046100110927     C                             stbPRS= '5'
046200080618     C*
046300110927     C                   SETON                                        50
046400080618     C*
046500080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
046600080618     C                   Z-ADD     1             jTAD
046700080618     C                   MOVEL(P)  stbCOS        wTADCod
046800080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
046900080618     C                   IF        %found
047000110927     C                   Z-ADD     1             j
047100080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
047200080618     C                   IF        %found
047300110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
047400110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
047500110927     C                   SETOFF                                       50
047600080618     C                   ENDIF
047700080618     C                   ENDIF
047800080618     C                   ENDIF
047900111121     C*
048000111121     C* Se evento "fittizio" e spedizione in giacenza => nn restituisco lo stato "fittizio"
048100111121     C                   z-add     1             i
048200111121     C     wCOS          lookup    sk2A_2Z                                55
048300120130     C***                if        %found and wGiac = 'S'
048400120130     C                   if        %found
048500111121     C                   SETON                                        50
048600111121     C                   endif
048700080618     C*
048800080618     C* Scarico il buffer d output
048900080618     C  N50              EXSR      WRIREC
049000110103     C*
049100110103     C                   ENDIF
049200110103     C                   ENDIF
049300110103     C                   ENDIF
049400031103     C*
049500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
049600031103     C                   EVAL      stbFTR = 'T'
049700031103     C                   UPDATE    tistb000
049800121012     C*
049900121012     C                   ENDIF
050000130305     C                   ENDIF
050100991027     C*
050200110103     C     KEYstb03P     READE     tistb03l
050300031103     C                   SETOFF                                       50
050400030325     C                   ENDDO
050500031103     C                   ENDIF
050600991027     C*
050700030325     C                   EVAL      wrkesito = '0'
050800991027     C*
050900910830     C                   ENDSR
051000111121
051100111121
051200111121
051300111121     C     rtvgiac       BEGSR
051400111121     C*
051500111121     C                   movel     'N'           wGiac             1
051600111121     C*
051700111121     C* Costruisco la chiave sul file TIGCP51L
051800111121     C                   eval      gcpFRG = *zeros
051900111121     C*
052000111121     C* Reperisco le giacenze relative alla bolla corrente
052100111121     C     KEYgcp51_P    chain     tigcp51l
052200111121     C                   if        %found(tigcp51l)
052300111216     C                   if        gcpATB = *blanks AND gcpDCG = 0
052400111121     C                   movel     'S'           wGiac
052500111216     C                   endif
052600111121     C                   endif
052700111121     C*
052800111121     C                   ENDSR
052900031103
053000031103
053100031103
053200110103     C     wrirec        BEGSR
053300110928     C*
053400130522     C                   eval      wAAS = tasAAS
053500130522     C                   eval      wNRS = tasNRS
053600130522     C                   eval      wNSP = tasNSP
053700110928     C                   eval      wLNP = tasLNP
053800110928     C                   eval      wLNA = tasLNA
053900110928     C                   eval      wDCR = tasDCR
054000130522     C                   eval      wTSP = tasTSP
054100130522     C                   eval      wRSD = tasRSD
054200130522     C                   eval      wIND = tasIND
054300130522     C                   eval      wCAD = tasCAD
054400130522     C                   eval      wLOD = tasLOD
054500130522     C                   eval      wPRD = tasPRD
054600130522     C                   eval      wNCL = tasNCL
054700130522     C                   eval      wPKB = tasPKB
054800130522     C                   eval      wVLB = tasVLB
054900130522     C                   eval      wCBO = tasCBO
055000140328     C                   eval      wTC1 = tasFTC
055100140328     C                   eval      wTC2 = tasTC2
055200120229     C*
055300120229     C* Prendo sempre i dati più recenti da ambiente filiale
055400120229     C                   setoff                                       60
055500120229     C*
055600120229     C                   eval      pOutAnnoFI     = tasAAS
055700120229     C                   eval      pOutLineaParFI = tasLNP
055800120229     C                   eval      pOutSerieFI    = tasNRS
055900120229     C                   eval      pOutNumSpedFI  = tasNSP
056000120229     C*
056100120229     C* - 1° tentativo in filiale arrivo   (FNARB)
056200120229     C                   if        not *in60
056300120229     C     KEYarb01C     chain     fnarb01l
056400120229     C                   if        %found(fnarb01l)
056500120229     C                   seton                                        60
056600130522     C                   eval      wAAS   = arbAAS
056700130522     C                   eval      wNRS   = arbNRS
056800130522     C                   eval      wNSP   = arbNSP
056900120229     C                   eval      wLNP   = arbLNP
057000120229     C                   eval      wLNA   = arbLNA
057100120229     C                   if        arbDCR > wDCR
057200120229     C                   eval      wDCR   = arbDCR
057300120229     C                   endif
057400130522     C                   eval      wTSP = arbTSP
057500130522     C                   eval      wRSD = arbRSD
057600130522     C                   eval      wIND = arbIND
057700130522     C                   eval      wCAD = arbCAD
057800130522     C                   eval      wLOD = arbLOD
057900130522     C                   eval      wPRD = arbPRD
058000130522     C                   eval      wNCL = arbNCL
058100130522     C                   eval      wPKB = arbPKB
058200130522     C                   eval      wVLB = arbVLB
058300130522     C                   eval      wCBO = arbCBO
058400140328     C                   eval      wTC1 = arbTC1
058500140328     C                   eval      wTC2 = arbTC2
058600120229     C                   endif
058700120229     C                   endif
058800120229     C*
058900120229     C* - 2° tentativo in filiale partenza (FNBLP)
059000120229     C                   if        not *in60
059100120229     C     KEYarb01C     chain     fnblp01l
059200120229     C                   if        %found(fnblp01l)
059300120229     C                   seton                                        60
059400130522     C                   eval      wAAS   = blpAAS
059500130522     C                   eval      wNRS   = blpNRS
059600130522     C                   eval      wNSP   = blpNSP
059700120229     C                   eval      wLNP   = blpLNP
059800120229     C                   eval      wLNA   = blpLNA
059900120229     C                   if        blpDCR > wDCR
060000120229     C                   eval      wDCR   = blpDCR
060100120229     C                   endif
060200130522     C                   eval      wTSP = blpTSP
060300130522     C                   eval      wRSD = blpRSD
060400130522     C                   eval      wIND = blpIND
060500130522     C                   eval      wCAD = blpCAD
060600130522     C                   eval      wLOD = blpLOD
060700130522     C                   eval      wPRD = blpPRD
060800130522     C                   eval      wNCL = blpNCL
060900130522     C                   eval      wPKB = blpPKB
061000130522     C                   eval      wVLB = blpVLB
061100130522     C                   eval      wCBO = blpCBO
061200140328     C                   eval      wTC1 = blpTC1
061300140328     C                   eval      wTC2 = blpTC2
061400120229     C                   endif
061500120229     C                   endif
061600111103     C*
061700111103     C                   movel     *blanks       wEsito1           1
061800110928     C                   movel     *blanks       wEsito2           1
061900111103     C*
062000111103     C* Chiamata metodo GetLblTyp
062100111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
062200111103     C                                                stbAAS
062300111103     C                                               :stbLNP
062400111103     C                                               :stbNRS
062500111103     C                                               :stbNSP
062600111103     C                                               :pOutLblTyp
062700111103     C                                               :pOutAnnoBO
062800111103     C                                               :pOutLineaParBO
062900111103     C                                               :pOutSerieBO
063000111103     C                                               :pOutNumSpedBO
063100111103     C                                               :pOutDcmBO
063200111103     C                                               :pOutCcaBO
063300111103     C                                               :pOutRblBO))
063400111103     C*
063500111103     C                   if        wEsito1 = '0'
063600111103     C                   if        pOutCcaBO = '2' AND
063700111103     C                             stbDAS    > pOutDcmBO
063800130522     C***                eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
063900130522     C***                          ' COMMERCITY'
064000111103     C                   endif
064100111103     C                   endif
064200111103     C*
064300111103     C* Se necessario reperisco l'ultima bolla figlia
064400111103     C                   if        *in40
064500110928     C*
064600110928     C* Chiamata metodo GetLastChild
064700110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
064800110928     C                                                stbAAS
064900110928     C                                               :stbLNP
065000110928     C                                               :stbNRS
065100110928     C                                               :stbNSP
065200110928     C                                               :pOutAnnoFI
065300110928     C                                               :pOutLineaParFI
065400110928     C                                               :pOutSerieFI
065500110928     C                                               :pOutNumSpedFI
065600110928     C                                               :pOutDcmFI
065700110928     C                                               :pOutCcaFI))
065800110928     C*
065900110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
066000110928     C                   if        wEsito2 = '0'
066100110929     C*
066200110929     C                   setoff                                       60
066300110928     C*
066400110928     C* - 1° tentativo in filiale arrivo   (FNARB)
066500111216     C                   if        not *in60
066600110929     C     KEYarb01C     chain     fnarb01l
066700110929     C                   if        %found(fnarb01l)
066800110929     C                   seton                                        60
066900130522     C                   eval      wAAS   = arbAAS
067000130522     C                   eval      wNRS   = arbNRS
067100130522     C                   eval      wNSP   = arbNSP
067200110929     C                   eval      wLNP   = arbLNP
067300110929     C                   eval      wLNA   = arbLNA
067400110929     C                   if        arbDCR > wDCR
067500110929     C                   eval      wDCR   = arbDCR
067600110929     C                   endif
067700130522     C                   eval      wTSP = arbTSP
067800130522     C                   eval      wRSD = arbRSD
067900130522     C                   eval      wIND = arbIND
068000130522     C                   eval      wCAD = arbCAD
068100130522     C                   eval      wLOD = arbLOD
068200130522     C                   eval      wPRD = arbPRD
068300130522     C                   eval      wNCL = arbNCL
068400130522     C                   eval      wPKB = arbPKB
068500130522     C                   eval      wVLB = arbVLB
068600130522     C                   eval      wCBO = arbCBO
068700140328     C                   eval      wTC1 = arbTC1
068800140328     C                   eval      wTC2 = arbTC2
068900110929     C                   endif
069000111216     C                   endif
069100110928     C*
069200110928     C* - 2° tentativo in filiale partenza (FNBLP)
069300111216     C                   if        not *in60
069400110929     C     KEYarb01C     chain     fnblp01l
069500110929     C                   if        %found(fnblp01l)
069600110929     C                   seton                                        60
069700130522     C                   eval      wAAS   = blpAAS
069800130522     C                   eval      wNRS   = blpNRS
069900130522     C                   eval      wNSP   = blpNSP
070000110929     C                   eval      wLNP   = blpLNP
070100110929     C                   eval      wLNA   = blpLNA
070200110929     C                   if        blpDCR > wDCR
070300110929     C                   eval      wDCR   = blpDCR
070400110929     C                   endif
070500130522     C                   eval      wTSP = blpTSP
070600130522     C                   eval      wRSD = blpRSD
070700130522     C                   eval      wIND = blpIND
070800130522     C                   eval      wCAD = blpCAD
070900130522     C                   eval      wLOD = blpLOD
071000130522     C                   eval      wPRD = blpPRD
071100130522     C                   eval      wNCL = blpNCL
071200130522     C                   eval      wPKB = blpPKB
071300130522     C                   eval      wVLB = blpVLB
071400130522     C                   eval      wCBO = blpCBO
071500140328     C                   eval      wTC1 = blpTC1
071600140328     C                   eval      wTC2 = blpTC2
071700110929     C                   endif
071800111216     C                   endif
071900110928     C*
072000110928     C* - 3° tentativo in sede             (TITAS)
072100110929     C                   if        not *in60
072200110929     C     KEYarb01C     chain     titas30c
072300110929     C                   if        %found(titas30c)
072400130522     C                   eval      wAAS = tasAAS
072500130522     C                   eval      wNRS = tasNRS
072600130522     C                   eval      wNSP = tasNSP
072700130522     C                   eval      wLNP = tasLNP
072800130522     C                   eval      wLNA = tasLNA
072900130522     C                   eval      wDCR = tasDCR
073000130522     C                   eval      wTSP = tasTSP
073100130522     C                   eval      wRSD = tasRSD
073200130522     C                   eval      wIND = tasIND
073300130522     C                   eval      wCAD = tasCAD
073400130522     C                   eval      wLOD = tasLOD
073500130522     C                   eval      wPRD = tasPRD
073600130522     C                   eval      wNCL = tasNCL
073700130522     C                   eval      wPKB = tasPKB
073800130522     C                   eval      wVLB = tasVLB
073900130522     C                   eval      wCBO = tasCBO
074000140328     C                   eval      wTC1 = tasFTC
074100140328     C                   eval      wTC2 = tasTC2
074200110929     C                   endif
074300110929     C                   endif
074400110929     C*
074500110928     C                   endif
074600110929     C                   endif
074700110927     C*
074800110928     C     wLNP          chain     azorg01l
074900110927     C                   if        %found(azorg01l)
075000110927     C                   eval      DS_CS_LNP = orgDES
075100110927     C                   endif
075200110927     C*
075300110928     C     wLNA          chain     azorg01l
075400110927     C                   if        %found(azorg01l)
075500110927     C                   eval      DS_CS_LNA = orgDES
075600110927     C                   endif
075700130522     C*
075800130522     C                   eval      DS_CS_CCM     = stbKSC
075900130522     C                   eval      DS_CS_NumSped = pOutLineaParBO * 1000000000+
076000130522     C                                             pOutSerieBO    * 10000000  +
076100130522     C                                             pOutNumSpedBO
076200130522     C                   eval      DS_CS_DatSped = stbDSP
076300130522     C*
076400130522     C                   select
076500130522     C                   when      wTSP = 'C'
076600130522     C                   eval      DS_CS_SrvType = 'EXPRESS'
076700130522     C                   when      wTSP = 'E'
076800130522     C                   eval      DS_CS_SrvType = 'PRIORITY'
076900130522     C                   when      wTSP = 'H'
077000130522     C                   eval      DS_CS_SrvType = 'BRT 10:30'
077100130522     C                   when      wTSP = 'D'
077200130522     C                   eval      DS_CS_SrvType = 'DISTRIBUTION'
077300130522     C                   endsl
077400130522     C*
077500130522     C                   eval      DS_CS_RSD = wRSD
077600130522     C                   eval      DS_CS_IND = wIND
077700130522     C                   eval      DS_CS_CAD = wCAD
077800130522     C                   eval      DS_CS_LOD = wLOD
077900130522     C                   eval      DS_CS_PRD = wPRD
078000130522     C                   eval      DS_CS_NCL = wNCL
078100130522     C                   eval      DS_CS_PKB = wPKB
078200130522     C                   eval      DS_CS_VLB = wVLB
078300130522     C*
078400130522     C                   select
078500130522     C                   when      wCBO = '1'
078600130522     C                   eval      DS_CS_Incotrm = 'FREE DOMICILE'
078700130522     C                   when      wCBO = '2'
078800130522     C                   eval      DS_CS_Incotrm = 'EX WORKS'
078900130522     C                   when      wCBO = '4'
079000130522     C                   eval      DS_CS_Incotrm = 'FREE DOLICILE + COD'
079100130522     C                   when      wCBO = '6'
079200130522     C                   eval      DS_CS_Incotrm = 'EX WORKS + COD'
079300130522     C                   other
079400130522     C* Chaino la bolla originale (in sede) per "ereditare" il tipo porto
079500130522     C     KEYtas30P_O   chain     titas30c
079600130522     C                   if        %found(titas30c)
079700130522     C                   select
079800130522     C                   when      %subst(tasTBL:1:1)='F' and tasFCA=*blanks
079900130522     C                   eval      DS_CS_Incotrm = 'FREE DOMICILE'
080000130522     C                   when      %subst(tasTBL:1:1)='A' and tasFCA=*blanks
080100130522     C                   eval      DS_CS_Incotrm = 'EX WORKS'
080200130522     C                   when      %subst(tasTBL:1:1)='F' and tasFCA='S'
080300130522     C                   eval      DS_CS_Incotrm = 'FREE DOLICILE + COD'
080400130522     C                   when      %subst(tasTBL:1:1)='A' and tasFCA='S'
080500130522     C                   eval      DS_CS_Incotrm = 'EX WORKS + COD'
080600130522     C                   other
080700130522     C                   eval      DS_CS_Incotrm = 'INTERNAL SERVICE'
080800130522     C                   endsl
080900130522     C                   endif
081000130522     C*
081100130522     C                   endsl
081200130522     C*
081300110927     C                   eval      DS_CS_RMN     = stbRMN
081400130522     C                   eval      DS_CS_RMA     = stbRMA
081500130522     C                   eval      DS_CS_EveDat  = stbDAS
081600130522     C                   eval      DS_CS_EveTim  = stbORS
081700111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
081800110928     C                   eval      DS_CS_DCR     = wDCR
081900110929     C                   eval      DS_CS_CasImp  = wCAS
082000110927     C                   eval      DS_CS_CasDatP = wDDP
082100110927     C                   eval      DS_CS_CasTipI = wTPI
082200130522     C                   eval      DS_CS_KeySPED = %trim(%editc(wLNP:'X'))+
082300130522     C                                             %trim(%editc(wNRS:'X'))+
082400130522     C                                             %trim(%editc(wNSP:'X'))
082500031103     C*
082600031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
082700110927     C                   EVAL      VAWDTA =
082800130522     C                                    %trim(%editc(DS_CS_CCM:'X'))
082900130522     C                               +';'+%trim(%editc(DS_CS_NumSped:'X'))
083000130522     C                               +';'+%trim(%editc(DS_CS_DatSped:'X'))
083100130522     C                               +';'+%trim(DS_CS_SrvType)
083200130522     C                               +';'+%trim(DS_CS_RSD)
083300130522     C                               +';'+%trim(DS_CS_IND)
083400130522     C                               +';'+%trim(DS_CS_CAD)
083500130522     C                               +';'+%trim(DS_CS_LOD)
083600130522     C                               +';'+%trim(DS_CS_PRD)
083700130522     C                               +';'+%trim(%editc(DS_CS_NCL:'4'))
083800130522     C                               +';'+%trim(%editc(DS_CS_PKB:'4'))
083900130522     C                               +';'+%trim(%editc(DS_CS_VLB:'4'))
084000130522     C                               +';'+%trim(DS_CS_Incotrm)
084100130522     C                               +';'+%trim(%editc(DS_CS_CasImp:'4'))
084200130522     C                               +';'+%trim(%editc(DS_CS_RMN:'4'))
084300130522     C                               +';'+%trim(DS_CS_RMA)
084400130522     C                               +';'+%trim(%editc(DS_CS_DCR:'X'))
084500140328     C                               +';'+%trim(wTC1)
084600140328     C                               +';'+%trim(wTC2)
084700130522     C                               +';'+%trim(%editc(DS_CS_EveDat:'X'))
084800130522     C                               +';'+%trim(%editc(DS_CS_EveTim:'X'))
084900130522     C                               +';'+%trim(DS_CS_EveCod)
085000130522     C                               +';'+%trim(DS_CS_EveDes)
085100130522     C                               +';'+%trim(DS_CS_GiacMot)
085200130522     C                               +';'
085300110927     C*
085400031103     C                   WRITE     TIVAW000
085500031103     C*
085600031103     C                   ENDSR
085700031103
085800031103
085900991027
086000991027      /TITLE Operazioni iniziali.
086100991027     C     *inzsr        BEGSR
086200991027     C*
086300991027     C     *ENTRY        PLIST
086400031103     C                   parm                    prmppi
086500991027     C     wrkesito      parm      wrkesito      prmesito
086600031103     C*
086700031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
086800031103     C                   EVAL      DSPPI = prmppi
086900030709     C*
087000030709     C* Definizione chiavi
087100110927     C*
087200110927     C* Chiave su TABEL00F - Parziale
087300110927     C     KEYtab00P     KLIST
087400110927     C                   KFLD                    tblKUT
087500110927     C                   KFLD                    tblCOD
087600030924     C*
087700110103     C* Chiave su TISTB03L - Parziale
087800110103     C     KEYstb03P     KLIST
087900031104     C                   KFLD                    wKstbKSU
088000031103     C                   KFLD                    wKstbFTR
088100031103     C*
088200031103     C* Chiave su TITAS30C - Parziale
088300031103     C     KEYtas30P     KLIST
088400031103     C                   KFLD                    stbAAS
088500031103     C                   KFLD                    stbLNP
088600031103     C                   KFLD                    stbNRS
088700031103     C                   KFLD                    stbNSP
088800130522     C*
088900130522     C* Chiave su TITAS30C - Parziale
089000130522     C     KEYtas30P_O   KLIST
089100130522     C                   KFLD                    pOutAnnoBO
089200130522     C                   KFLD                    pOutLineaParBO
089300130522     C                   KFLD                    pOutSerieBO
089400130522     C                   KFLD                    pOutNumSpedBO
089500110929     C*
089600110929     C* Chiave su FNARB01L / FNBLP01L - Completa
089700110929     C     KEYarb01C     KLIST
089800110929     C                   KFLD                    pOutAnnoFI
089900110929     C                   KFLD                    pOutLineaParFI
090000110929     C                   KFLD                    pOutSerieFI
090100110929     C                   KFLD                    pOutNumSpedFI
090200110103     C*
090300110103     C* Chiave su TNCSB03L - Completa
090400110103     C     KEYcsb03C     KLIST
090500110103     C                   KFLD                    stbAAS
090600110103     C                   KFLD                    stbLNP
090700110103     C                   KFLD                    stbNRS
090800110103     C                   KFLD                    stbNSP
090900110927     C*
091000110927     C* Chiave su TIGCP51L - Parziale
091100110927     C     KEYgcp51_P    KLIST
091200110927     C                   KFLD                    stbAAS
091300110927     C                   KFLD                    stbLNP
091400110927     C                   KFLD                    stbNRS
091500110927     C                   KFLD                    stbNSP
091600110927     C                   KFLD                    gcpFRG
091700110927     C*
091800110927     C* Chiave su TIGNP01L - Parziale
091900110927     C     KEYgnp01_P    KLIST
092000110927     C                   KFLD                    gcpAGC
092100110927     C                   KFLD                    gcpFGC
092200110927     C                   KFLD                    gcpNGC
092300110927     C                   KFLD                    gcpFRG
092400991027     C*
092500991027     C                   ENDSR
092600110103** CAUBAR - CAUSALI BARTOLINI
092700110103A                       L.AVVISO
092800110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
092900110103AVV                     LASCIATO AVVISO 1GG
093000110103AV2                     LASCIATO AVVISO 2GG
093100110103AV3                     LASCIATO AVVISO 3GG
093200110103AV5                     LASCIATO AVVISO 5GG
093300110103AV7                     LASCIATO AVVISO 7GG
093400110103AV9                     LASCIATO AVVISO 9 GIORNI
093500110103A16                     L.A. RIMANDA LO SVINCOLO
093600110103A23                     L.A. CHIUSO FINO AL
093700110103CPO                     CAMBIO PORTO
093800110103DAN                     COLLO/I DANNEGGIATO/I
093900110103DIR                     DIROTTAMENTO
094000110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
094100110103G                       GIACENZA GENERICA TEMPORANEA
094200110103GEN                     GIACENZA GENERICA
094300110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
094400110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
094500110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
094600110103G09                     ATTENDERE 9 GIORNI
094700110103IDD                     CHIUSURA SPEDIZIONE CON IDD
094800110103MAN                     COLLO/I MANCANTE/I
094900110103MIC                     MESSA IN CONSEGNA
095000110103N                       NON FATTA
095100110103NAV                     COLLO SCONDIZIONATO
095200110103NIC                     TOLTA DALLA CONSEGNA
095300110103P                       CONSEGNA PARZIALE
095400110103PAT                     FESTIVITA PATRONALE
095500110103R                       RISERVA
095600110103RDC                     RIPRISTINO
095700110103RFD                     RIFIUTATO
095800110103RFM                     RIFIUTATO
095900110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
096000110103RIP                     RIPRISTINO
096100110103RS                      RISERVA
096200110103T                       CHIUSO PER TURNO
096300110103WA                      AMMANCO SENZA EVENTO
096400110103WAE                     AMMANCO CON EVENTO
096500110103WM                      MANCANZA SENZA EVENTO
096600110103WME                     MANCANZA CON EVENTO
096700110103WV                      AVARIA SENZA EVENTO
096800110103WVE                     AVARIA CON EVENTO
096900110103ZAL                     ALLUVIONE
097000110103ZBS                     BLOCCO STRADALE
097100110103ZFR                     FESTIVITÀ REGIONALE
097200110103ZMM                     MARE MOLTO MOSSO
097300110103ZMP                     MANIFESTAZIONE PUBBLICA
097400110103ZNV                     NEVICATA ECCEZIONALE
097500110103ZSC                     SCIOPERO
097600110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
097700110103002                     MERCE NON ORDINATA O NON CONFORME
097800110103003                     MERCE SPEDITA IN RITARDO
097900110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
098000110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
098100110103006                     MERCE RESA SENZA AUTORIZZAZIONE
098200110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
098300110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
098400110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
098500110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
098600110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
098700110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
098800110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
098900110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
099000110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
099100110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
099200110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
099300110103020                     DESTINATARIO RISULTA TRASFERITO
099400110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
099500110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
099600110103023                     IL DESTINATARIO E' CHIUSO FINO AL
099700110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
099800110103025                     IL DESTINATARIO E' CHIUSO
099900110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
100000110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
100100110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
100200110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
100300110103030                     IL DESTINATARIO RIFIUTA LA MERCE
100400110103031                     IL DESTINATARIO NON INTENDE
100500110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
100600110103033                     IL DESTINATARIO RICHIEDE
100700110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
100800110103035                     DOCUMENTI INCOMPLETI O MANCANTI
100900110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
101000110103037                     RIFIUTA LA CONSEGNA TASSATIVA
101100110103100                     -
101200110103101                     -
101300110103999                     -
101400110103CONSOK                  CONSEGNA OK
101500110103RESO                    RESO
101600110927                        -
101700110929GIACAPE                 APERTURA GIACENZA
101800110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
101900110927GIACEND                 CHIUSURA GIACENZA
102000130523ARRPRICOL               ARRIVO PRIMO COLLO (LNA)
102100110927                        -
102200110927                        -
102300110927                        -
102400110927                        -
102500110927                        -
102600110927                        -
102700110103** CAUCLI - CAUSALI CLIENTE
102800110926A                       L.AVVISO
102900110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
103000110926AVV                     LASCIATO AVVISO 1GG
103100110926AV2                     LASCIATO AVVISO 2GG
103200110926AV3                     LASCIATO AVVISO 3GG
103300110926AV5                     LASCIATO AVVISO 5GG
103400110926AV7                     LASCIATO AVVISO 7GG
103500110926AV9                     LASCIATO AVVISO 9 GIORNI
103600110926A16                     L.A. RIMANDA LO SVINCOLO
103700110926A23                     L.A. CHIUSO FINO AL
103800110926CPO                     CAMBIO PORTO
103900110926DAN                     COLLO/I DANNEGGIATO/I
104000110926DIR                     DIROTTAMENTO
104100110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
104200110926G                       GIACENZA GENERICA TEMPORANEA
104300110926GEN                     GIACENZA GENERICA
104400110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
104500110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
104600110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
104700110926G09                     ATTENDERE 9 GIORNI
104800110926IDD                     CHIUSURA SPEDIZIONE CON IDD
104900110926MAN                     COLLO/I MANCANTE/I
105000110926MIC                     MESSA IN CONSEGNA
105100110926N                       NON FATTA
105200110926NAV                     COLLO SCONDIZIONATO
105300110926NIC                     TOLTA DALLA CONSEGNA
105400110926P                       CONSEGNA PARZIALE
105500110926PAT                     FESTIVITA PATRONALE
105600110926R                       RISERVA
105700110926RDC                     RIPRISTINO
105800110926RFD                     RIFIUTATO
105900110926RFM                     RIFIUTATO
106000110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
106100110926RIP                     RIPRISTINO
106200110926RS                      RISERVA
106300110926T                       CHIUSO PER TURNO
106400110926WA                      AMMANCO SENZA EVENTO
106500110926WAE                     AMMANCO CON EVENTO
106600110926WM                      MANCANZA SENZA EVENTO
106700110926WME                     MANCANZA CON EVENTO
106800110926WV                      AVARIA SENZA EVENTO
106900110926WVE                     AVARIA CON EVENTO
107000110926ZAL                     ALLUVIONE
107100110926ZBS                     BLOCCO STRADALE
107200110926ZFR                     FESTIVITÀ REGIONALE
107300110926ZMM                     MARE MOLTO MOSSO
107400110926ZMP                     MANIFESTAZIONE PUBBLICA
107500110926ZNV                     NEVICATA ECCEZIONALE
107600110926ZSC                     SCIOPERO
107700110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
107800110926002                     MERCE NON ORDINATA O NON CONFORME
107900110926003                     MERCE SPEDITA IN RITARDO
108000110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
108100110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
108200110926006                     MERCE RESA SENZA AUTORIZZAZIONE
108300110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
108400110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
108500110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
108600110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
108700110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
108800110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
108900110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
109000110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
109100110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
109200110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
109300110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
109400110926020                     DESTINATARIO RISULTA TRASFERITO
109500110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
109600110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
109700110926023                     IL DESTINATARIO E' CHIUSO FINO AL
109800110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
109900110926025                     IL DESTINATARIO E' CHIUSO
110000110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
110100110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
110200110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
110300110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
110400110926030                     IL DESTINATARIO RIFIUTA LA MERCE
110500110926031                     IL DESTINATARIO NON INTENDE
110600110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
110700110926033                     IL DESTINATARIO RICHIEDE
110800110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
110900110926035                     DOCUMENTI INCOMPLETI O MANCANTI
111000110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
111100110926037                     RIFIUTA LA CONSEGNA TASSATIVA
111200110926NON INVIARE             -
111300110926NON INVIARE             -
111400110926NON INVIARE             -
111500110926CONSOK                  CONSEGNA OK
111600110926RESO                    RESO
111700110926
111800110927*AG                     APERTURA GIACENZA
111900110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
112000110927*CG                     CHIUSURA GIACENZA
112100130523ARRPRICOL               ARRIVO PRIMO COLLO (LNA)
112200110927                        -
112300110927                        -
112400110927                        -
112500110927                        -
112600110927                        -
112700110927                        -
112800110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
112900110103L.AVVISO
113000110103AVVISATO-RIMANDA SVINCOLO 1GG
113100110103LASCIATO AVVISO 1GG
113200110103LASCIATO AVVISO 2GG
113300110103LASCIATO AVVISO 3GG
113400110103LASCIATO AVVISO 5GG
113500110103LASCIATO AVVISO 7GG
113600110103LASCIATO AVVISO 9 GIORNI
113700110103L.A. RIMANDA LO SVINCOLO
113800110103L.A. CHIUSO FINO AL
113900110103CAMBIO PORTO
114000110103COLLO/I DANNEGGIATO/I
114100110103DIROTTAMENTO
114200110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
114300110103GIACENZA GENERICA TEMPORANEA
114400110103GIACENZA GENERICA
114500110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
114600110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
114700110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
114800110103ATTENDERE 9 GIORNI
114900110103CHIUSURA SPEDIZIONE CON IDD
115000110103COLLO/I MANCANTE/I
115100110103MESSA IN CONSEGNA
115200110103NON FATTA
115300110103COLLO SCONDIZIONATO
115400110103TOLTA DALLA CONSEGNA
115500110103CONSEGNA PARZIALE
115600110103FESTIVITA PATRONALE
115700110103RISERVA
115800110103RIPRISTINO
115900110103RIFIUTATO
116000110103RIFIUTATO
116100110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
116200110103RIPRISTINO
116300110103RISERVA
116400110103CHIUSO PER TURNO
116500110103AMMANCO SENZA EVENTO
116600110103AMMANCO CON EVENTO
116700110103MANCANZA SENZA EVENTO
116800110103MANCANZA CON EVENTO
116900110103AVARIA SENZA EVENTO
117000110103AVARIA CON EVENTO
117100110103ALLUVIONE
117200110103BLOCCO STRADALE
117300110103FESTIVITÀ REGIONALE
117400110103MARE MOLTO MOSSO
117500110103MANIFESTAZIONE PUBBLICA
117600110103NEVICATA ECCEZIONALE
117700110103SCIOPERO
117800110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
117900110103MERCE NON ORDINATA O NON CONFORME
118000110103MERCE SPEDITA IN RITARDO
118100110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
118200110103MERCE SPEDITA CON TROPPO ANTICIPO
118300110103MERCE RESA SENZA AUTORIZZAZIONE
118400110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
118500110103DESTINATARIO NON PAGA IL C/ASSEGNO
118600110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
118700110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
118800110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
118900110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
119000110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
119100110103DESTINATARIO RIMANDA LO SVINCOLO AL
119200110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
119300110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
119400110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
119500110103DESTINATARIO RISULTA TRASFERITO
119600110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
119700110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
119800110103IL DESTINATARIO E' CHIUSO FINO AL
119900110103IL DESTINATARIO E' CHIUSO PER FERIE
120000110103IL DESTINATARIO E' CHIUSO
120100110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
120200110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
120300110103ESERCIZIO NON ANCORA IN ATTIVITA
120400110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
120500110103IL DESTINATARIO RIFIUTA LA MERCE
120600110103IL DESTINATARIO NON INTENDE
120700110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
120800110103IL DESTINATARIO RICHIEDE
120900110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
121000110103DOCUMENTI INCOMPLETI O MANCANTI
121100110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
121200110103RIFIUTA LA CONSEGNA TASSATIVA
121300110103-
121400110103-
121500110103-
121600110103CONSEGNA OK
121700110103RESO
121800110927-
121900110927APERTURA GIACENZA
122000110927RICEVUTE DISPOSIZIONI GIACENZA
122100110927CHIUSURA GIACENZA
122200130523ARRIVO MERCE TERMINAL DISTRIBUZIONE
122300110927-
122400110927-
122500110927-
122600110927-
122700110927-
122800110927-
122900041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
123000110103A                                                                                            1
123100110103M                                                                                            2
123200110103V                                                                                            3
123300110103E                                                                                            4
123400080901** WDCTCLI - CAUSALI CLIENTE
123500110926TAD_A                                                                                        1
123600110926TAD_M                                                                                        2
123700110926TAD_V                                                                                        3
123800110926TAD_E                                                                                        4
123900110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
124000110927AMMANCO                                                                                      1
124100110927MANCA COLLO                                                                                  2
124200110927AVARIA                                                                                       3
124300110927EVENTO                                                                                       4
124400110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
124500110103INCARICO1                                                                                    1
124600110103INCARICO2                                                                                    2
124700110103CONSOK1                                                                                      3
124800110103CONSOK2                                                                                      4
124900110927GIACNODIS1                                                                                   5
125000110927GIACNODIS2                                                                                   6
125100110927GIACSIDIS1                                                                                   7
125200110927GIACSIDIS2                                                                                   8
125300110927                                                                                             9
125400110927                                                                                             10
125500110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
125600110926ACCETTAZIONE                                                                                 1
125700110926ACCETTAZIONE                                                                                 2
125800110103NON INVIARE                                                                                  3
125900110103NON INVIARE                                                                                  4
126000110927GIAC_APERTA                                                                                  5
126100110927GIAC_APERTA                                                                                  6
126200110927GIAC_DISPOS                                                                                  7
126300110927GIAC_DISPOS                                                                                  8
126400110927NON INVIARE                                                                                  9
126500110927NON INVIARE                                                                                  10
126600110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
126700110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
126800110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
126900110926NON INVIARE                                                                                  3
127000110926NON INVIARE                                                                                  4
127100110927APERTURA GIACENZA                                                                            5
127200110927APERTURA GIACENZA                                                                            6
127300110927RICEVUTE DISPOSIZIONI                                                                        7
127400110927RICEVUTE DISPOSIZIONI                                                                        8
127500110927NON INVIARE                                                                                  8
127600110927NON INVIARE                                                                                  10
