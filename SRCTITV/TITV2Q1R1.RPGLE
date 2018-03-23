000100110927     H DECEDIT('0,') DATEDIT(*DMY.)
000200140203     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP(*CALLER)
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
001600140203     FTITA430C  IF   E           K DISK
001700090323     FTIVAWWWT  UF A E             DISK    commit
001800031103
001900031103     D*-----------
002000031103     D* SCHIERE A PROGRAMMA
002100031103     D*-----------
002200110927     D CAUBAR          S             10    DIM(100) CTDATA PERRCD(1)
002300110927     D CAUCLI          S             20    DIM(100) CTDATA PERRCD(1)
002400110927     D CAUDES          S             80    DIM(100) CTDATA PERRCD(1)
002500110927     D WDCTBAR         S              1    DIM(4)   CTDATA PERRCD(1)
002600110927     D WDCTCLI         S             20    DIM(4)   CTDATA PERRCD(1)
002700110927     D WDCTDES         S             80    DIM(4)   CTDATA PERRCD(1)
002800110927     D WCOD9BAR        S             10    DIM(10)  CTDATA PERRCD(1)
002900110927     D WCOD9CLI        S             20    DIM(10)  CTDATA PERRCD(1)
003000110927     D WCOD9DES        S             80    DIM(10)  CTDATA PERRCD(1)
003100041015
003200041015     D*-----------
003300041015     D* SCHIERE
003400041015     D*-----------
003500041015     D skTADCod        S              2    DIM(500)
003600041015     D skTADRag        S              1    DIM(500)
003700041015     D jTAD            S              3  0 INZ(*zeros)
003800030924
003900041015     D*-----------
004000041015     D* VARIABILI D WRK
004100041015     D*-----------
004200031103     D prmppi          S             50
004300030924     D prmesito        S              1
004400030924     D wrkesito        S                   like(prmesito)
004500031103     D j               S              2  0
004600110927     D i               s              3  0 inz
004700110927     D ii              s              3  0 inz
004800110927     D wTADCod         S              2
004900110927     D sk2Alavv        s             10    DIM(300)
005000110927     D sk2Agiac        s             10    DIM(300)
005100111121     D sk2A_2Z         s             10    DIM(300)
005200110927     D sk2Agiacd       s             80    DIM(300)
005300110929
005400110929
005500110929     D LibFileARB01    s             21A   inz
005600110929     D LibFileBLP01    s             21A   inz
005700110929     D currSysNeta     s              8A   inz('*NULL')
005800110929
005900041015
006000041015     D*-----------
006100041015     D* DEFINIZIONE DS ESTERNE
006200041015     D*-----------
006300041015     D DTAD          E DS
006400110927     D DS2A          E DS
006500111116     D DS2Z          E DS
006600031103
006700031103     D*-----------
006800031103     D* VARIABILI RIFERITE AL DATA-BASE
006900031103     D*-----------
007000080618     D wKstbKSU        S                   like(stbKSU)
007100080618     D wKstbFTR        S                   like(stbFTR)
007200110103     D wCAS            S                   like(csbCAS)
007300110927     D wDDP            S                   like(csbDDP)
007400110927     D wTPI            S                   like(csbTPI)
007500130522     D wAAS            S                   like(tasAAS)
007600130522     D wNRS            S                   like(tasNRS)
007700130522     D wNSP            S                   like(tasNSP)
007800110928     D wLNP            S                   like(tasLNP)
007900110928     D wLNA            S                   like(tasLNA)
008000110928     D wDCR            S                   like(tasDCR)
008100130522     D wTSP            S                   like(tasTSP)
008200130522     D wRSD            S                   like(tasRSD)
008300130522     D wIND            S                   like(tasIND)
008400130522     D wCAD            S                   like(tasCAD)
008500130522     D wLOD            S                   like(tasLOD)
008600130522     D wPRD            S                   like(tasPRD)
008700130522     D wNCL            S                   like(tasNCL)
008800130522     D wPKB            S                   like(tasPKB)
008900130522     D wVLB            S                   like(tasVLB)
009000130522     D wCBO            S                   like(tasCBO)
009100140203     D wFIRMATAR       S                   inz(*blanks) like(ta4NOT)
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
013400140203     C*
013500140203     D  DS_CS_FIRMA                  35    inz
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
026900140203     C                             'CODICE CLIENTE'                        +';'+
027000140203     C                             'NUM. SPEDIZIONE'                       +';'+
027100140203     C                             'DATA SPEDIZIONE'                       +';'+
027200140203     C                             'TIPO SERVIZIO'                         +';'+
027300140203     C                             'RAG. SOC. DESTINATARIO'                +';'+
027400140203     C                             'INDIRIZZO DESTINATARIO'                +';'+
027500140203     C                             'CAP DESTINATARIO'                      +';'+
027600140203     C                             'LOCALITA DESTINATARIO'                 +';'+
027700140203     C                             'PROVINCIA DESTINATARIO'                +';'+
027800140203     C                             'TOT. COLLI'                            +';'+
027900140203     C                             'TOT. PESO KG'                          +';'+
028000140203     C                             'TOT. VOLUME M3'                        +';'+
028100140203     C                             'TIPO PORTO'                            +';'+
028200140203     C                             'CONTRASSEGNO'                          +';'+
028300140203     C                             'RIF. SPEDIZIONE 1'                     +';'+
028400140203     C                             'RIF. SPEDIZIONE 2'                     +';'+
028500140203     C                             'DATA CONSEGNA RICHIESTA'               +';'+
028600140203     C                             'ESITO DATA'                            +';'+
028700140203     C                             'ESITO ORA'                             +';'+
028800140203     C                             'ESITO CODICE'                          +';'+
028900140203     C                             'ESITO DESCRIZIONE'                     +';'+
029000140203     C                             'ESITO NOTE'                            +';'+
029100140203     C                             'FIRMA CONSEGNATARIO'                   +';'
029200130522     C*
029300130522     C                   WRITE     TIVAW000
029400130522     C                   ENDIF
029500031103     C*
029600031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
029700031103     C* di traduzione (trattasi d "post-traduzione")
029800031104     C                   EVAL      wKstbKSU = w§PPIKSU
029900031103     C                   EVAL      wKstbFTR = *blanks
030000110103     C     KEYstb03P     SETLL     tistb03l
030100110103     C                   IF        %equal(tistb03l)
030200110103     C     KEYstb03P     READE     tistb03l
030300991027     C*
030400031103     C* Ciclo x tuttii record del cliente da trasmettere
030500110103     C                   DOW       not %eof(tistb03l)
030600130304     C*
030700130304     C* Ignoro completamente gli stati provenienti dai PDA ...
030800130304     C                   IF        stbPRS = '7'
030900130304     C                   ELSE
031000121012     C*
031100121012     C* Verifico immediatamente data/ora stato corrente => se "futura" skippo completamente
031200121012     C                   IF        stbDAS*10000+stbORS >
031300121012     C                             %int((%dec(%date():*ISO)*1000000+
031400121012     C                             %dec(%time()))/100)
031500121012     C                   ELSE
031600110929     C*
031700111103     C* Come primissima cosa verifico rottura d codice su key spedizione
031800110929     C                   IF        stbAAS = sAAS AND
031900110929     C                             stbLNP = sLNP AND
032000110929     C                             stbNRS = sNRS AND
032100110929     C                             stbNSP = sNSP
032200110929     C                   ELSE
032300110929     C                   SETOFF                                       40
032400110929     C                   EVAL      sAAS = stbAAS
032500110929     C                   EVAL      sLNP = stbLNP
032600110929     C                   EVAL      sNRS = stbNRS
032700110929     C                   EVAL      sNSP = stbNSP
032800110929     C                   ENDIF
032900031014     C*
033000030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
033100110103     C                   RESET                   DS_Cli_Status
033200080618     C*
033300110927     C                   SETON                                        50
033400031103     C*
033500031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
033600031103     C* deve restituire al cliente nel file tradotto
033700111215     C                   IF        stbFTR = *blanks                             * ridondante
033800140203     C*
033900140203     C                   clear                   wCAS
034000140203     C                   clear                   wDDP
034100140203     C                   clear                   wTPI
034200140203     C                   clear                   wDCR
034300140203     C                   clear                   wTSP
034400140203     C                   clear                   wRSD
034500140203     C                   clear                   wIND
034600140203     C                   clear                   wCAD
034700140203     C                   clear                   wLOD
034800140203     C                   clear                   wPRD
034900140203     C                   clear                   wNCL
035000140203     C                   clear                   wPKB
035100140203     C                   clear                   wVLB
035200140203     C                   clear                   wCBO
035300140203     C                   clear                   wFIRMATAR
035400111215     C*
035500111215     C* Verifico (subito) se spedizione in giacenza
035600111215     C                   exsr      rtvgiac
035700140203     C*
035800140203     C* Reperisco (subito) la firma consegnatario
035900140203     C                   exsr      rtvfirma
036000110103     C*
036100111031     C* Gestisco solamente alcuni stati
036200110928     C                   IF        (stbTIS= '1' OR
036300111103     C                             (stbTIS= '9' AND stbPRS= '1'))
036400110103     C*
036500110103     C                   SETOFF                                       50
036600031103     C*
036700031103     C* Innanzitutto chaino la bolla direttamente su TITAS
036800031103     C     KEYtas30P     CHAIN     titas30c
036900031103     C                   IF        %found(titas30c)
037000110103     C*
037100110103     C* Quindi verifico subito esistenza contrassegno
037200110103     C     KEYcsb03C     CHAIN     tncsb03l
037300110103     C                   IF        %found(tncsb03l)
037400110103     C                   eval      wCAS = csbCAS
037500110927     C                   eval      wDDP = csbDDP
037600110927     C                   eval      wTPI = csbTPI
037700110103     C                   ENDIF
037800110929     C*
037900110929     C* Considerazione su eventi DIR
038000110929     C                   IF        stbCOS = 'DIR'
038100110929     C                   SETON                                        40
038200110929     C                   ENDIF
038300080618     C*
038400110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
038500110927     C                   IF         stbTIS= '1' AND
038600140306     C*** NO PRRS='3'              (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
038700140306     C                             (stbPRS= '2' OR stbPRS= '4')
038800110927     C*
038900110927     C                   MOVEL     *blanks       wCOS             10
039000110927     C                   MOVEL     *blanks       wCAU             10
039100110927     C                   IF        stbPRS = '3'
039200110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
039300110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
039400110927     C                   ELSE
039500110927     C                   EVAL      wCOS = stbCOS
039600110927     C                   ENDIF
039700110927     C*
039800031103     C                   Z-ADD     1             j
039900110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
040000110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
040100110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
040200110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
040300110927     C*
040400110927     C* Verifico se evento lasciato avviso
040500110927     C                   z-add     1             ii
040600110927     C     wCOS          lookup    sk2Alavv(ii)                           55
040700110927     C                   if        %found
040800110927     C                   eval      DS_CS_LavDat = stbDAS
040900110927     C                   endif
041000110927     C*
041100110927     C* Se stato d tipo giacenza
041200110927     C                   if        stbPRS = '3'
041300110927     C                   eval      DS_CS_GiacDat = stbDAS
041400110927     C*
041500110927     C* Se apertura reperisco il motivo apertura giacenza
041600110927     C                   if        stbSTS = '1'
041700110927     C                   z-add     1             i
041800110927     C     wCAU          lookup    sk2Agiac(i)                            55
041900110927     C                   if        %found
042000110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
042100110927     C                   endif
042200111121     C                   endif
042300111121     C*
042400111121     C                   if        wGiac = 'S'
042500110927     C*
042600110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
042700110927     C     KEYgnp01_P    setll     tignp01l
042800110927     C                   if        %equal(tignp01l)
042900110927     C     KEYgnp01_P    reade     tignp01l
043000110927     C                   dow       not %eof(tignp01l)
043100110927     C*** concateno note giacenza
043200110927     C                   eval      DS_CS_GiacMot =
043300110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
043400110927     C     KEYgnp01_P    reade     tignp01l
043500110927     C                   enddo
043600110927     C                   endif
043700111121     C*
043800110927     C                   endif
043900110927     C*
044000110927     C                   endif
044100110927     C*
044200031103     C                   ELSE
044300031103     C                   SETON                                        50
044400031103     C                   ENDIF
044500140306     C                   ELSE
044600140306     C                   SETON                                        50
044700110927     C                   ENDIF
044800110103     C*
044900110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
045000110927     C                   IF        stbTIS= '9' AND
045100110927     C                             stbPRS= '1'
045200110927     C*
045300110927     C                   SETOFF                                       50
045400110927     C*
045500110103     C                   Z-ADD     1             j
045600110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
045700110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
045800110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
045900110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
046000110103     C                   ELSE
046100110103     C                   SETON                                        50
046200110103     C                   ENDIF
046300110103     C                   ENDIF
046400041015     C*
046500080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
046600110927     C                   IF        stbTIS= '1' AND
046700110927     C                             stbPRS= '5'
046800080618     C*
046900110927     C                   SETON                                        50
047000080618     C*
047100080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
047200080618     C                   Z-ADD     1             jTAD
047300080618     C                   MOVEL(P)  stbCOS        wTADCod
047400080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
047500080618     C                   IF        %found
047600110927     C                   Z-ADD     1             j
047700080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
047800080618     C                   IF        %found
047900110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
048000110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
048100110927     C                   SETOFF                                       50
048200080618     C                   ENDIF
048300080618     C                   ENDIF
048400080618     C                   ENDIF
048500111121     C*
048600111121     C* Se evento "fittizio" e spedizione in giacenza => nn restituisco lo stato "fittizio"
048700111121     C                   z-add     1             i
048800111121     C     wCOS          lookup    sk2A_2Z                                55
048900120130     C***                if        %found and wGiac = 'S'
049000120130     C                   if        %found
049100111121     C                   SETON                                        50
049200111121     C                   endif
049300080618     C*
049400080618     C* Scarico il buffer d output
049500080618     C  N50              EXSR      WRIREC
049600110103     C*
049700110103     C                   ENDIF
049800110103     C                   ENDIF
049900110103     C                   ENDIF
050000031103     C*
050100031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
050200031103     C                   EVAL      stbFTR = 'T'
050300031103     C                   UPDATE    tistb000
050400121012     C*
050500121012     C                   ENDIF
050600130305     C                   ENDIF
050700991027     C*
050800110103     C     KEYstb03P     READE     tistb03l
050900031103     C                   SETOFF                                       50
051000030325     C                   ENDDO
051100031103     C                   ENDIF
051200991027     C*
051300030325     C                   EVAL      wrkesito = '0'
051400991027     C*
051500910830     C                   ENDSR
051600111121
051700111121
051800111121
051900111121     C     rtvgiac       BEGSR
052000111121     C*
052100111121     C                   movel     'N'           wGiac             1
052200111121     C*
052300111121     C* Costruisco la chiave sul file TIGCP51L
052400111121     C                   eval      gcpFRG = *zeros
052500111121     C*
052600111121     C* Reperisco le giacenze relative alla bolla corrente
052700111121     C     KEYgcp51_P    chain     tigcp51l
052800111121     C                   if        %found(tigcp51l)
052900111216     C                   if        gcpATB = *blanks AND gcpDCG = 0
053000111121     C                   movel     'S'           wGiac
053100111216     C                   endif
053200111121     C                   endif
053300111121     C*
053400111121     C                   ENDSR
053500140203
053600140203
053700140203
053800140203     C     rtvfirma      BEGSR
053900140203     C*
054000140203     C* Reperisco, se c'è, la firma del consegnatario
054100140203     C                   eval      ta4TRC = '1'
054200140203     C     KEYta430_C    chain     tita430c
054300140203     C                   if        %found(tita430c)
054400140203     C                   eval      wFIRMATAR = ta4NOT
054500140203     C                   endif
054600140203     C*
054700140203     C                   ENDSR
054800031103
054900031103
055000031103
055100110103     C     wrirec        BEGSR
055200110928     C*
055300130522     C                   eval      wAAS = tasAAS
055400130522     C                   eval      wNRS = tasNRS
055500130522     C                   eval      wNSP = tasNSP
055600110928     C                   eval      wLNP = tasLNP
055700110928     C                   eval      wLNA = tasLNA
055800110928     C                   eval      wDCR = tasDCR
055900130522     C                   eval      wTSP = tasTSP
056000130522     C                   eval      wRSD = tasRSD
056100130522     C                   eval      wIND = tasIND
056200130522     C                   eval      wCAD = tasCAD
056300130522     C                   eval      wLOD = tasLOD
056400130522     C                   eval      wPRD = tasPRD
056500130522     C                   eval      wNCL = tasNCL
056600130522     C                   eval      wPKB = tasPKB
056700130522     C                   eval      wVLB = tasVLB
056800130522     C                   eval      wCBO = tasCBO
056900120229     C*
057000120229     C* Prendo sempre i dati più recenti da ambiente filiale
057100120229     C                   setoff                                       60
057200120229     C*
057300120229     C                   eval      pOutAnnoFI     = tasAAS
057400120229     C                   eval      pOutLineaParFI = tasLNP
057500120229     C                   eval      pOutSerieFI    = tasNRS
057600120229     C                   eval      pOutNumSpedFI  = tasNSP
057700120229     C*
057800120229     C* - 1° tentativo in filiale arrivo   (FNARB)
057900120229     C                   if        not *in60
058000120229     C     KEYarb01C     chain     fnarb01l
058100120229     C                   if        %found(fnarb01l)
058200120229     C                   seton                                        60
058300130522     C                   eval      wAAS   = arbAAS
058400130522     C                   eval      wNRS   = arbNRS
058500130522     C                   eval      wNSP   = arbNSP
058600120229     C                   eval      wLNP   = arbLNP
058700120229     C                   eval      wLNA   = arbLNA
058800120229     C                   if        arbDCR > wDCR
058900120229     C                   eval      wDCR   = arbDCR
059000120229     C                   endif
059100130522     C                   eval      wTSP = arbTSP
059200130522     C                   eval      wRSD = arbRSD
059300130522     C                   eval      wIND = arbIND
059400130522     C                   eval      wCAD = arbCAD
059500130522     C                   eval      wLOD = arbLOD
059600130522     C                   eval      wPRD = arbPRD
059700130522     C                   eval      wNCL = arbNCL
059800130522     C                   eval      wPKB = arbPKB
059900130522     C                   eval      wVLB = arbVLB
060000130522     C                   eval      wCBO = arbCBO
060100120229     C                   endif
060200120229     C                   endif
060300120229     C*
060400120229     C* - 2° tentativo in filiale partenza (FNBLP)
060500120229     C                   if        not *in60
060600120229     C     KEYarb01C     chain     fnblp01l
060700120229     C                   if        %found(fnblp01l)
060800120229     C                   seton                                        60
060900130522     C                   eval      wAAS   = blpAAS
061000130522     C                   eval      wNRS   = blpNRS
061100130522     C                   eval      wNSP   = blpNSP
061200120229     C                   eval      wLNP   = blpLNP
061300120229     C                   eval      wLNA   = blpLNA
061400120229     C                   if        blpDCR > wDCR
061500120229     C                   eval      wDCR   = blpDCR
061600120229     C                   endif
061700130522     C                   eval      wTSP = blpTSP
061800130522     C                   eval      wRSD = blpRSD
061900130522     C                   eval      wIND = blpIND
062000130522     C                   eval      wCAD = blpCAD
062100130522     C                   eval      wLOD = blpLOD
062200130522     C                   eval      wPRD = blpPRD
062300130522     C                   eval      wNCL = blpNCL
062400130522     C                   eval      wPKB = blpPKB
062500130522     C                   eval      wVLB = blpVLB
062600130522     C                   eval      wCBO = blpCBO
062700120229     C                   endif
062800120229     C                   endif
062900111103     C*
063000111103     C                   movel     *blanks       wEsito1           1
063100110928     C                   movel     *blanks       wEsito2           1
063200111103     C*
063300111103     C* Chiamata metodo GetLblTyp
063400111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
063500111103     C                                                stbAAS
063600111103     C                                               :stbLNP
063700111103     C                                               :stbNRS
063800111103     C                                               :stbNSP
063900111103     C                                               :pOutLblTyp
064000111103     C                                               :pOutAnnoBO
064100111103     C                                               :pOutLineaParBO
064200111103     C                                               :pOutSerieBO
064300111103     C                                               :pOutNumSpedBO
064400111103     C                                               :pOutDcmBO
064500111103     C                                               :pOutCcaBO
064600111103     C                                               :pOutRblBO))
064700111103     C*
064800111103     C                   if        wEsito1 = '0'
064900111103     C                   if        pOutCcaBO = '2' AND
065000111103     C                             stbDAS    > pOutDcmBO
065100130522     C***                eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
065200130522     C***                          ' COMMERCITY'
065300111103     C                   endif
065400111103     C                   endif
065500111103     C*
065600111103     C* Se necessario reperisco l'ultima bolla figlia
065700111103     C                   if        *in40
065800110928     C*
065900110928     C* Chiamata metodo GetLastChild
066000110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
066100110928     C                                                stbAAS
066200110928     C                                               :stbLNP
066300110928     C                                               :stbNRS
066400110928     C                                               :stbNSP
066500110928     C                                               :pOutAnnoFI
066600110928     C                                               :pOutLineaParFI
066700110928     C                                               :pOutSerieFI
066800110928     C                                               :pOutNumSpedFI
066900110928     C                                               :pOutDcmFI
067000110928     C                                               :pOutCcaFI))
067100110928     C*
067200110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
067300110928     C                   if        wEsito2 = '0'
067400110929     C*
067500110929     C                   setoff                                       60
067600110928     C*
067700110928     C* - 1° tentativo in filiale arrivo   (FNARB)
067800111216     C                   if        not *in60
067900110929     C     KEYarb01C     chain     fnarb01l
068000110929     C                   if        %found(fnarb01l)
068100110929     C                   seton                                        60
068200130522     C                   eval      wAAS   = arbAAS
068300130522     C                   eval      wNRS   = arbNRS
068400130522     C                   eval      wNSP   = arbNSP
068500110929     C                   eval      wLNP   = arbLNP
068600110929     C                   eval      wLNA   = arbLNA
068700110929     C                   if        arbDCR > wDCR
068800110929     C                   eval      wDCR   = arbDCR
068900110929     C                   endif
069000130522     C                   eval      wTSP = arbTSP
069100130522     C                   eval      wRSD = arbRSD
069200130522     C                   eval      wIND = arbIND
069300130522     C                   eval      wCAD = arbCAD
069400130522     C                   eval      wLOD = arbLOD
069500130522     C                   eval      wPRD = arbPRD
069600130522     C                   eval      wNCL = arbNCL
069700130522     C                   eval      wPKB = arbPKB
069800130522     C                   eval      wVLB = arbVLB
069900130522     C                   eval      wCBO = arbCBO
070000110929     C                   endif
070100111216     C                   endif
070200110928     C*
070300110928     C* - 2° tentativo in filiale partenza (FNBLP)
070400111216     C                   if        not *in60
070500110929     C     KEYarb01C     chain     fnblp01l
070600110929     C                   if        %found(fnblp01l)
070700110929     C                   seton                                        60
070800130522     C                   eval      wAAS   = blpAAS
070900130522     C                   eval      wNRS   = blpNRS
071000130522     C                   eval      wNSP   = blpNSP
071100110929     C                   eval      wLNP   = blpLNP
071200110929     C                   eval      wLNA   = blpLNA
071300110929     C                   if        blpDCR > wDCR
071400110929     C                   eval      wDCR   = blpDCR
071500110929     C                   endif
071600130522     C                   eval      wTSP = blpTSP
071700130522     C                   eval      wRSD = blpRSD
071800130522     C                   eval      wIND = blpIND
071900130522     C                   eval      wCAD = blpCAD
072000130522     C                   eval      wLOD = blpLOD
072100130522     C                   eval      wPRD = blpPRD
072200130522     C                   eval      wNCL = blpNCL
072300130522     C                   eval      wPKB = blpPKB
072400130522     C                   eval      wVLB = blpVLB
072500130522     C                   eval      wCBO = blpCBO
072600110929     C                   endif
072700111216     C                   endif
072800110928     C*
072900110928     C* - 3° tentativo in sede             (TITAS)
073000110929     C                   if        not *in60
073100110929     C     KEYarb01C     chain     titas30c
073200110929     C                   if        %found(titas30c)
073300130522     C                   eval      wAAS = tasAAS
073400130522     C                   eval      wNRS = tasNRS
073500130522     C                   eval      wNSP = tasNSP
073600130522     C                   eval      wLNP = tasLNP
073700130522     C                   eval      wLNA = tasLNA
073800130522     C                   eval      wDCR = tasDCR
073900130522     C                   eval      wTSP = tasTSP
074000130522     C                   eval      wRSD = tasRSD
074100130522     C                   eval      wIND = tasIND
074200130522     C                   eval      wCAD = tasCAD
074300130522     C                   eval      wLOD = tasLOD
074400130522     C                   eval      wPRD = tasPRD
074500130522     C                   eval      wNCL = tasNCL
074600130522     C                   eval      wPKB = tasPKB
074700130522     C                   eval      wVLB = tasVLB
074800130522     C                   eval      wCBO = tasCBO
074900110929     C                   endif
075000110929     C                   endif
075100110929     C*
075200110928     C                   endif
075300110929     C                   endif
075400110927     C*
075500110928     C     wLNP          chain     azorg01l
075600110927     C                   if        %found(azorg01l)
075700110927     C                   eval      DS_CS_LNP = orgDES
075800110927     C                   endif
075900110927     C*
076000110928     C     wLNA          chain     azorg01l
076100110927     C                   if        %found(azorg01l)
076200110927     C                   eval      DS_CS_LNA = orgDES
076300110927     C                   endif
076400130522     C*
076500130522     C                   eval      DS_CS_CCM     = stbKSC
076600130522     C                   eval      DS_CS_NumSped = pOutLineaParBO * 1000000000+
076700130522     C                                             pOutSerieBO    * 10000000  +
076800130522     C                                             pOutNumSpedBO
076900130522     C                   eval      DS_CS_DatSped = stbDSP
077000130522     C*
077100130522     C                   select
077200130522     C                   when      wTSP = 'C'
077300130522     C                   eval      DS_CS_SrvType = 'EXPRESS'
077400130522     C                   when      wTSP = 'E'
077500130522     C                   eval      DS_CS_SrvType = 'PRIORITY'
077600130522     C                   when      wTSP = 'H'
077700130522     C                   eval      DS_CS_SrvType = 'BRT 10:30'
077800130522     C                   when      wTSP = 'D'
077900130522     C                   eval      DS_CS_SrvType = 'DISTRIBUTION'
078000130522     C                   endsl
078100130522     C*
078200130522     C                   eval      DS_CS_RSD = wRSD
078300130522     C                   eval      DS_CS_IND = wIND
078400130522     C                   eval      DS_CS_CAD = wCAD
078500130522     C                   eval      DS_CS_LOD = wLOD
078600130522     C                   eval      DS_CS_PRD = wPRD
078700130522     C                   eval      DS_CS_NCL = wNCL
078800130522     C                   eval      DS_CS_PKB = wPKB
078900130522     C                   eval      DS_CS_VLB = wVLB
079000130522     C*
079100130522     C                   select
079200130522     C                   when      wCBO = '1'
079300130522     C                   eval      DS_CS_Incotrm = 'FREE DOMICILE'
079400130522     C                   when      wCBO = '2'
079500130522     C                   eval      DS_CS_Incotrm = 'EX WORKS'
079600130522     C                   when      wCBO = '4'
079700130522     C                   eval      DS_CS_Incotrm = 'FREE DOLICILE + COD'
079800130522     C                   when      wCBO = '6'
079900130522     C                   eval      DS_CS_Incotrm = 'EX WORKS + COD'
080000130522     C                   other
080100130522     C* Chaino la bolla originale (in sede) per "ereditare" il tipo porto
080200130522     C     KEYtas30P_O   chain     titas30c
080300130522     C                   if        %found(titas30c)
080400130522     C                   select
080500130522     C                   when      %subst(tasTBL:1:1)='F' and tasFCA=*blanks
080600130522     C                   eval      DS_CS_Incotrm = 'FREE DOMICILE'
080700130522     C                   when      %subst(tasTBL:1:1)='A' and tasFCA=*blanks
080800130522     C                   eval      DS_CS_Incotrm = 'EX WORKS'
080900130522     C                   when      %subst(tasTBL:1:1)='F' and tasFCA='S'
081000130522     C                   eval      DS_CS_Incotrm = 'FREE DOLICILE + COD'
081100130522     C                   when      %subst(tasTBL:1:1)='A' and tasFCA='S'
081200130522     C                   eval      DS_CS_Incotrm = 'EX WORKS + COD'
081300130522     C                   other
081400130522     C                   eval      DS_CS_Incotrm = 'INTERNAL SERVICE'
081500130522     C                   endsl
081600130522     C                   endif
081700130522     C*
081800130522     C                   endsl
081900130522     C*
082000110927     C                   eval      DS_CS_RMN     = stbRMN
082100130522     C                   eval      DS_CS_RMA     = stbRMA
082200130522     C                   eval      DS_CS_EveDat  = stbDAS
082300130522     C                   eval      DS_CS_EveTim  = stbORS
082400111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
082500110928     C                   eval      DS_CS_DCR     = wDCR
082600110929     C                   eval      DS_CS_CasImp  = wCAS
082700110927     C                   eval      DS_CS_CasDatP = wDDP
082800110927     C                   eval      DS_CS_CasTipI = wTPI
082900130522     C                   eval      DS_CS_KeySPED = %trim(%editc(wLNP:'X'))+
083000130522     C                                             %trim(%editc(wNRS:'X'))+
083100130522     C                                             %trim(%editc(wNSP:'X'))
083200140203     C                   eval      DS_CS_FIRMA   = wFIRMATAR
083300031103     C*
083400031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
083500110927     C                   EVAL      VAWDTA =
083600130522     C                                    %trim(%editc(DS_CS_CCM:'X'))
083700130522     C                               +';'+%trim(%editc(DS_CS_NumSped:'X'))
083800130522     C                               +';'+%trim(%editc(DS_CS_DatSped:'X'))
083900130522     C                               +';'+%trim(DS_CS_SrvType)
084000130522     C                               +';'+%trim(DS_CS_RSD)
084100130522     C                               +';'+%trim(DS_CS_IND)
084200130522     C                               +';'+%trim(DS_CS_CAD)
084300130522     C                               +';'+%trim(DS_CS_LOD)
084400130522     C                               +';'+%trim(DS_CS_PRD)
084500130522     C                               +';'+%trim(%editc(DS_CS_NCL:'4'))
084600130522     C                               +';'+%trim(%editc(DS_CS_PKB:'4'))
084700130522     C                               +';'+%trim(%editc(DS_CS_VLB:'4'))
084800130522     C                               +';'+%trim(DS_CS_Incotrm)
084900130522     C                               +';'+%trim(%editc(DS_CS_CasImp:'4'))
085000130522     C                               +';'+%trim(%editc(DS_CS_RMN:'4'))
085100130522     C                               +';'+%trim(DS_CS_RMA)
085200130522     C                               +';'+%trim(%editc(DS_CS_DCR:'X'))
085300130522     C                               +';'+%trim(%editc(DS_CS_EveDat:'X'))
085400130522     C                               +';'+%trim(%editc(DS_CS_EveTim:'X'))
085500130522     C                               +';'+%trim(DS_CS_EveCod)
085600130522     C                               +';'+%trim(DS_CS_EveDes)
085700130522     C                               +';'+%trim(DS_CS_GiacMot)
085800140203     C                               +';'+%trim(DS_CS_FIRMA)
085900130522     C                               +';'
086000110927     C*
086100031103     C                   WRITE     TIVAW000
086200031103     C*
086300031103     C                   ENDSR
086400031103
086500031103
086600991027
086700991027      /TITLE Operazioni iniziali.
086800991027     C     *inzsr        BEGSR
086900991027     C*
087000991027     C     *ENTRY        PLIST
087100031103     C                   parm                    prmppi
087200991027     C     wrkesito      parm      wrkesito      prmesito
087300031103     C*
087400031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
087500031103     C                   EVAL      DSPPI = prmppi
087600030709     C*
087700030709     C* Definizione chiavi
087800110927     C*
087900110927     C* Chiave su TABEL00F - Parziale
088000110927     C     KEYtab00P     KLIST
088100110927     C                   KFLD                    tblKUT
088200110927     C                   KFLD                    tblCOD
088300030924     C*
088400110103     C* Chiave su TISTB03L - Parziale
088500110103     C     KEYstb03P     KLIST
088600031104     C                   KFLD                    wKstbKSU
088700031103     C                   KFLD                    wKstbFTR
088800031103     C*
088900031103     C* Chiave su TITAS30C - Parziale
089000031103     C     KEYtas30P     KLIST
089100031103     C                   KFLD                    stbAAS
089200031103     C                   KFLD                    stbLNP
089300031103     C                   KFLD                    stbNRS
089400031103     C                   KFLD                    stbNSP
089500130522     C*
089600130522     C* Chiave su TITAS30C - Parziale
089700130522     C     KEYtas30P_O   KLIST
089800130522     C                   KFLD                    pOutAnnoBO
089900130522     C                   KFLD                    pOutLineaParBO
090000130522     C                   KFLD                    pOutSerieBO
090100130522     C                   KFLD                    pOutNumSpedBO
090200110929     C*
090300110929     C* Chiave su FNARB01L / FNBLP01L - Completa
090400110929     C     KEYarb01C     KLIST
090500110929     C                   KFLD                    pOutAnnoFI
090600110929     C                   KFLD                    pOutLineaParFI
090700110929     C                   KFLD                    pOutSerieFI
090800110929     C                   KFLD                    pOutNumSpedFI
090900110103     C*
091000110103     C* Chiave su TNCSB03L - Completa
091100110103     C     KEYcsb03C     KLIST
091200110103     C                   KFLD                    stbAAS
091300110103     C                   KFLD                    stbLNP
091400110103     C                   KFLD                    stbNRS
091500110103     C                   KFLD                    stbNSP
091600110927     C*
091700110927     C* Chiave su TIGCP51L - Parziale
091800110927     C     KEYgcp51_P    KLIST
091900110927     C                   KFLD                    stbAAS
092000110927     C                   KFLD                    stbLNP
092100110927     C                   KFLD                    stbNRS
092200110927     C                   KFLD                    stbNSP
092300110927     C                   KFLD                    gcpFRG
092400110927     C*
092500110927     C* Chiave su TIGNP01L - Parziale
092600110927     C     KEYgnp01_P    KLIST
092700110927     C                   KFLD                    gcpAGC
092800110927     C                   KFLD                    gcpFGC
092900110927     C                   KFLD                    gcpNGC
093000110927     C                   KFLD                    gcpFRG
093100140203     C*
093200140203     C* TITA430C - Completa
093300140203     C     KEYta430_C    KLIST
093400140203     C                   KFLD                    stbAAS
093500140203     C                   KFLD                    stbLNP
093600140203     C                   KFLD                    stbNRS
093700140203     C                   KFLD                    stbNSP
093800140203     C                   KFLD                    ta4TRC
093900991027     C*
094000991027     C                   ENDSR
094100110103** CAUBAR - CAUSALI BARTOLINI
094200110103A                       L.AVVISO
094300110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
094400110103AVV                     LASCIATO AVVISO 1GG
094500110103AV2                     LASCIATO AVVISO 2GG
094600110103AV3                     LASCIATO AVVISO 3GG
094700110103AV5                     LASCIATO AVVISO 5GG
094800110103AV7                     LASCIATO AVVISO 7GG
094900110103AV9                     LASCIATO AVVISO 9 GIORNI
095000110103A16                     L.A. RIMANDA LO SVINCOLO
095100110103A23                     L.A. CHIUSO FINO AL
095200110103CPO                     CAMBIO PORTO
095300110103DAN                     COLLO/I DANNEGGIATO/I
095400110103DIR                     DIROTTAMENTO
095500110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
095600110103G                       GIACENZA GENERICA TEMPORANEA
095700110103GEN                     GIACENZA GENERICA
095800110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
095900110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
096000110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
096100110103G09                     ATTENDERE 9 GIORNI
096200110103IDD                     CHIUSURA SPEDIZIONE CON IDD
096300110103MAN                     COLLO/I MANCANTE/I
096400110103MIC                     MESSA IN CONSEGNA
096500110103N                       NON FATTA
096600110103NAV                     COLLO SCONDIZIONATO
096700110103NIC                     TOLTA DALLA CONSEGNA
096800110103P                       CONSEGNA PARZIALE
096900110103PAT                     FESTIVITA PATRONALE
097000110103R                       RISERVA
097100110103RDC                     RIPRISTINO
097200110103RFD                     RIFIUTATO
097300110103RFM                     RIFIUTATO
097400110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
097500110103RIP                     RIPRISTINO
097600110103RS                      RISERVA
097700110103T                       CHIUSO PER TURNO
097800110103WA                      AMMANCO SENZA EVENTO
097900110103WAE                     AMMANCO CON EVENTO
098000110103WM                      MANCANZA SENZA EVENTO
098100110103WME                     MANCANZA CON EVENTO
098200110103WV                      AVARIA SENZA EVENTO
098300110103WVE                     AVARIA CON EVENTO
098400110103ZAL                     ALLUVIONE
098500110103ZBS                     BLOCCO STRADALE
098600110103ZFR                     FESTIVITÀ REGIONALE
098700110103ZMM                     MARE MOLTO MOSSO
098800110103ZMP                     MANIFESTAZIONE PUBBLICA
098900110103ZNV                     NEVICATA ECCEZIONALE
099000110103ZSC                     SCIOPERO
099100110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
099200110103002                     MERCE NON ORDINATA O NON CONFORME
099300110103003                     MERCE SPEDITA IN RITARDO
099400110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
099500110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
099600110103006                     MERCE RESA SENZA AUTORIZZAZIONE
099700110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
099800110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
099900110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
100000110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
100100110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
100200110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
100300110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
100400110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
100500110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
100600110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
100700110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
100800110103020                     DESTINATARIO RISULTA TRASFERITO
100900110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
101000110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
101100110103023                     IL DESTINATARIO E' CHIUSO FINO AL
101200110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
101300110103025                     IL DESTINATARIO E' CHIUSO
101400110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
101500110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
101600110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
101700110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
101800110103030                     IL DESTINATARIO RIFIUTA LA MERCE
101900110103031                     IL DESTINATARIO NON INTENDE
102000110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
102100110103033                     IL DESTINATARIO RICHIEDE
102200110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
102300110103035                     DOCUMENTI INCOMPLETI O MANCANTI
102400110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
102500110103037                     RIFIUTA LA CONSEGNA TASSATIVA
102600110103100                     -
102700110103101                     -
102800110103999                     -
102900110103CONSOK                  CONSEGNA OK
103000110103RESO                    RESO
103100110927                        -
103200110929GIACAPE                 APERTURA GIACENZA
103300110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
103400110927GIACEND                 CHIUSURA GIACENZA
103500130523ARRPRICOL               ARRIVO PRIMO COLLO (LNA)
103600110927                        -
103700110927                        -
103800110927                        -
103900110927                        -
104000110927                        -
104100110927                        -
104200110103** CAUCLI - CAUSALI CLIENTE
104300110926A                       L.AVVISO
104400110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
104500110926AVV                     LASCIATO AVVISO 1GG
104600110926AV2                     LASCIATO AVVISO 2GG
104700110926AV3                     LASCIATO AVVISO 3GG
104800110926AV5                     LASCIATO AVVISO 5GG
104900110926AV7                     LASCIATO AVVISO 7GG
105000110926AV9                     LASCIATO AVVISO 9 GIORNI
105100110926A16                     L.A. RIMANDA LO SVINCOLO
105200110926A23                     L.A. CHIUSO FINO AL
105300110926CPO                     CAMBIO PORTO
105400110926DAN                     COLLO/I DANNEGGIATO/I
105500110926DIR                     DIROTTAMENTO
105600110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
105700110926G                       GIACENZA GENERICA TEMPORANEA
105800110926GEN                     GIACENZA GENERICA
105900110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
106000110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
106100110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
106200110926G09                     ATTENDERE 9 GIORNI
106300110926IDD                     CHIUSURA SPEDIZIONE CON IDD
106400110926MAN                     COLLO/I MANCANTE/I
106500140306NON INVIARE             MESSA IN CONSEGNA
106600110926N                       NON FATTA
106700110926NAV                     COLLO SCONDIZIONATO
106800110926NIC                     TOLTA DALLA CONSEGNA
106900110926P                       CONSEGNA PARZIALE
107000110926PAT                     FESTIVITA PATRONALE
107100110926R                       RISERVA
107200110926RDC                     RIPRISTINO
107300110926RFD                     RIFIUTATO
107400110926RFM                     RIFIUTATO
107500140306NON INVIARE             RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
107600110926RIP                     RIPRISTINO
107700110926RS                      RISERVA
107800110926T                       CHIUSO PER TURNO
107900110926WA                      AMMANCO SENZA EVENTO
108000110926WAE                     AMMANCO CON EVENTO
108100110926WM                      MANCANZA SENZA EVENTO
108200110926WME                     MANCANZA CON EVENTO
108300110926WV                      AVARIA SENZA EVENTO
108400110926WVE                     AVARIA CON EVENTO
108500110926ZAL                     ALLUVIONE
108600110926ZBS                     BLOCCO STRADALE
108700110926ZFR                     FESTIVITÀ REGIONALE
108800110926ZMM                     MARE MOLTO MOSSO
108900110926ZMP                     MANIFESTAZIONE PUBBLICA
109000110926ZNV                     NEVICATA ECCEZIONALE
109100110926ZSC                     SCIOPERO
109200110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
109300110926002                     MERCE NON ORDINATA O NON CONFORME
109400110926003                     MERCE SPEDITA IN RITARDO
109500110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
109600110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
109700110926006                     MERCE RESA SENZA AUTORIZZAZIONE
109800110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
109900110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
110000110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
110100110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
110200110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
110300110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
110400110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
110500110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
110600110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
110700110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
110800110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
110900110926020                     DESTINATARIO RISULTA TRASFERITO
111000110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
111100110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
111200110926023                     IL DESTINATARIO E' CHIUSO FINO AL
111300110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
111400110926025                     IL DESTINATARIO E' CHIUSO
111500110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
111600110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
111700110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
111800110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
111900110926030                     IL DESTINATARIO RIFIUTA LA MERCE
112000110926031                     IL DESTINATARIO NON INTENDE
112100110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
112200110926033                     IL DESTINATARIO RICHIEDE
112300110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
112400110926035                     DOCUMENTI INCOMPLETI O MANCANTI
112500110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
112600110926037                     RIFIUTA LA CONSEGNA TASSATIVA
112700110926NON INVIARE             -
112800110926NON INVIARE             -
112900110926NON INVIARE             -
113000110926CONSOK                  CONSEGNA OK
113100110926RESO                    RESO
113200110926
113300110927*AG                     APERTURA GIACENZA
113400110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
113500110927*CG                     CHIUSURA GIACENZA
113600130523ARRPRICOL               ARRIVO PRIMO COLLO (LNA)
113700110927                        -
113800110927                        -
113900110927                        -
114000110927                        -
114100110927                        -
114200110927                        -
114300110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
114400110103L.AVVISO
114500110103AVVISATO-RIMANDA SVINCOLO 1GG
114600110103LASCIATO AVVISO 1GG
114700110103LASCIATO AVVISO 2GG
114800110103LASCIATO AVVISO 3GG
114900110103LASCIATO AVVISO 5GG
115000110103LASCIATO AVVISO 7GG
115100110103LASCIATO AVVISO 9 GIORNI
115200110103L.A. RIMANDA LO SVINCOLO
115300110103L.A. CHIUSO FINO AL
115400110103CAMBIO PORTO
115500110103COLLO/I DANNEGGIATO/I
115600110103DIROTTAMENTO
115700110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
115800110103GIACENZA GENERICA TEMPORANEA
115900110103GIACENZA GENERICA
116000110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
116100110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
116200110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
116300110103ATTENDERE 9 GIORNI
116400110103CHIUSURA SPEDIZIONE CON IDD
116500110103COLLO/I MANCANTE/I
116600110103MESSA IN CONSEGNA
116700110103NON FATTA
116800110103COLLO SCONDIZIONATO
116900110103TOLTA DALLA CONSEGNA
117000110103CONSEGNA PARZIALE
117100110103FESTIVITA PATRONALE
117200110103RISERVA
117300110103RIPRISTINO
117400110103RIFIUTATO
117500110103RIFIUTATO
117600110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
117700110103RIPRISTINO
117800110103RISERVA
117900110103CHIUSO PER TURNO
118000110103AMMANCO SENZA EVENTO
118100110103AMMANCO CON EVENTO
118200110103MANCANZA SENZA EVENTO
118300110103MANCANZA CON EVENTO
118400110103AVARIA SENZA EVENTO
118500110103AVARIA CON EVENTO
118600110103ALLUVIONE
118700110103BLOCCO STRADALE
118800110103FESTIVITÀ REGIONALE
118900110103MARE MOLTO MOSSO
119000110103MANIFESTAZIONE PUBBLICA
119100110103NEVICATA ECCEZIONALE
119200110103SCIOPERO
119300110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
119400110103MERCE NON ORDINATA O NON CONFORME
119500110103MERCE SPEDITA IN RITARDO
119600110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
119700110103MERCE SPEDITA CON TROPPO ANTICIPO
119800110103MERCE RESA SENZA AUTORIZZAZIONE
119900110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
120000110103DESTINATARIO NON PAGA IL C/ASSEGNO
120100110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
120200110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
120300110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
120400110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
120500110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
120600110103DESTINATARIO RIMANDA LO SVINCOLO AL
120700110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
120800110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
120900110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
121000110103DESTINATARIO RISULTA TRASFERITO
121100110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
121200110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
121300110103IL DESTINATARIO E' CHIUSO FINO AL
121400110103IL DESTINATARIO E' CHIUSO PER FERIE
121500110103IL DESTINATARIO E' CHIUSO
121600110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
121700110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
121800110103ESERCIZIO NON ANCORA IN ATTIVITA
121900110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
122000110103IL DESTINATARIO RIFIUTA LA MERCE
122100110103IL DESTINATARIO NON INTENDE
122200110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
122300110103IL DESTINATARIO RICHIEDE
122400110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
122500110103DOCUMENTI INCOMPLETI O MANCANTI
122600110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
122700110103RIFIUTA LA CONSEGNA TASSATIVA
122800110103-
122900110103-
123000110103-
123100110103CONSEGNA OK
123200110103RESO
123300110927-
123400110927APERTURA GIACENZA
123500110927RICEVUTE DISPOSIZIONI GIACENZA
123600110927CHIUSURA GIACENZA
123700130523ARRIVO MERCE TERMINAL DISTRIBUZIONE
123800110927-
123900110927-
124000110927-
124100110927-
124200110927-
124300110927-
124400041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
124500110103A                                                                                            1
124600110103M                                                                                            2
124700110103V                                                                                            3
124800110103E                                                                                            4
124900080901** WDCTCLI - CAUSALI CLIENTE
125000110926TAD_A                                                                                        1
125100110926TAD_M                                                                                        2
125200110926TAD_V                                                                                        3
125300110926TAD_E                                                                                        4
125400110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
125500110927AMMANCO                                                                                      1
125600110927MANCA COLLO                                                                                  2
125700110927AVARIA                                                                                       3
125800110927EVENTO                                                                                       4
125900110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
126000110103INCARICO1                                                                                    1
126100110103INCARICO2                                                                                    2
126200110103CONSOK1                                                                                      3
126300110103CONSOK2                                                                                      4
126400110927GIACNODIS1                                                                                   5
126500110927GIACNODIS2                                                                                   6
126600110927GIACSIDIS1                                                                                   7
126700110927GIACSIDIS2                                                                                   8
126800110927                                                                                             9
126900110927                                                                                             10
127000110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
127100110926ACCETTAZIONE                                                                                 1
127200110926ACCETTAZIONE                                                                                 2
127300110103NON INVIARE                                                                                  3
127400110103NON INVIARE                                                                                  4
127500110927GIAC_APERTA                                                                                  5
127600110927GIAC_APERTA                                                                                  6
127700110927GIAC_DISPOS                                                                                  7
127800110927GIAC_DISPOS                                                                                  8
127900110927NON INVIARE                                                                                  9
128000110927NON INVIARE                                                                                  10
128100110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
128200110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
128300110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
128400110926NON INVIARE                                                                                  3
128500110926NON INVIARE                                                                                  4
128600110927APERTURA GIACENZA                                                                            5
128700110927APERTURA GIACENZA                                                                            6
128800110927RICEVUTE DISPOSIZIONI                                                                        7
128900110927RICEVUTE DISPOSIZIONI                                                                        8
129000110927NON INVIARE                                                                                  8
129100110927NON INVIARE                                                                                  10
