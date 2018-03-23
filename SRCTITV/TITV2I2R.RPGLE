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
005000110927     D sk2Agiacd       s             80    DIM(300)
005100110929
005200110929
005300110929     D LibFileARB01    s             21A   inz
005400110929     D LibFileBLP01    s             21A   inz
005500110929     D currSysNeta     s              8A   inz('*NULL')
005600110929
005700041015
005800041015     D*-----------
005900041015     D* DEFINIZIONE DS ESTERNE
006000041015     D*-----------
006100041015     D DTAD          E DS
006200110927     D DS2A          E DS
006300031103
006400031103     D*-----------
006500031103     D* VARIABILI RIFERITE AL DATA-BASE
006600031103     D*-----------
006700080618     D wKstbKSU        S                   like(stbKSU)
006800080618     D wKstbFTR        S                   like(stbFTR)
006900110103     D wCAS            S                   like(csbCAS)
007000110927     D wDDP            S                   like(csbDDP)
007100110927     D wTPI            S                   like(csbTPI)
007200110928     D wLNP            S                   like(tasLNP)
007300110928     D wLNA            S                   like(tasLNA)
007400110928     D wDCR            S                   like(tasDCR)
007500110929
007600110929     D sAAS            S                   like(stbAAS)
007700110929     D sLNP            S                   like(stbLNP)
007800110929     D sNRS            S                   like(stbNRS)
007900110929     D sNSP            S                   like(stbNSP)
008000030924
008100110103
008200110103     D*----------
008300110927     D* DS CLIENTE
008400110103     D*----------
008500110103     D DS_Cli_Status   DS
008600110927     D  DS_CS_RMN                    15  0 inz
008700110927     D  DS_CS_LNP                    20    inz
008800110927     D  DS_CS_LNA                    20    inz
008900111020     D  DS_CS_EveDaOr                12  0 inz
009000110927     D  DS_CS_EveCod                 20    inz
009100110927     D  DS_CS_EveDes                 80    inz
009200110927     D  DS_CS_GiacDat                 8  0 inz
009300110927     D  DS_CS_GiacMot               300    inz
009400110927     D  DS_CS_LavDat                  8  0 inz
009500110927     D  DS_CS_DCR                     8  0 inz
009600110927     D  DS_CS_CasImp                 13  3 inz
009700110927     D  DS_CS_CasDatP                 8  0 inz
009800110927     D  DS_CS_CasTipI                 1    inz
009900110103
010000031103
010100031103     D*-----------
010200031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
010300031103     D*-----------
010400031103     D DSPPI           DS
010500080618     D  w§PPIKSU                      7  0
010600031103
010700110928
010800110928     D*------------------
010900110928     D* LINKING A DEFINIZIONI ESTERNE
011000110928     D*------------------
011100110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
011200110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
011300110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011400110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
011500110928
011600030924
011700030325
011800030924     C* MAIN/
011900110929     C*
012000110929     C* Reperisco il sistema AS/400 corrente
012100110929     C                   callp     UBRTVNETA_Rtv(currSysNeta)
012200110929     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
012300110929     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
012400110929     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
012500110929     C                   else
012600110929     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
012700110929     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
012800110929     C                   endif
012900110929     C*
013000110929     C* Apertura file "overraidati"
013100110929     C                   if        not %open(FNARB01L)
013200110929     C                   open      FNARB01L
013300110929     C                   endif
013400110929     C                   if        not %open(FNBLP01L)
013500110929     C                   open      FNBLP01L
013600110929     C                   endif
013700110929     C*
013800041015     C*
013900041015     C* Carica i dati tabellati
014000041015     C                   EXSR      cartab
014100041015     C*
014200041015     C* Esegue traduzione
014300991027     C                   EXSR      traduci
014400110929     C*
014500110929     C* Chiusura file "overraidati"
014600110929     C                   if        %open(FNARB01L)
014700110929     C                   close     FNARB01L
014800110929     C                   endif
014900110929     C                   if        %open(FNBLP01L)
015000110929     C                   close     FNBLP01L
015100110929     C                   endif
015200110929     C*
015300921023     C                   SETON                                        LR
015400041015
015500041015
015600041015
015700041015     C     cartab        BEGSR
015800110927     C*
015900110927     C* Carico causali evento divise x categoria (A=Lavv G=Giac)
016000110927     C                   clear                   sk2Alavv
016100110927     C                   clear                   sk2Agiac
016200110927     C                   clear                   sk2Agiacd
016300110927     C                   z-add     *zeros        i
016400110927     C                   z-add     *zeros        ii
016500110927     C                   eval      tblKUT = 1
016600110927     C                   eval      tblCOD = '2A'
016700110927     C     KEYtab00P     setll     tabel00f
016800110927     C     KEYtab00P     reade     tabel00f
016900110927     C                   dow       not %eof(tabel00f)
017000110927     C                   movel(p)  tblUNI        ds2a
017100110927     C                   if        §2aFTC = 'G'
017200110927     C                   add       1             i
017300110927     C                   eval      sk2Agiac(i)   = tblKEY
017400110927     C                   eval      sk2Agiacd(i)  = §2ADES
017500110927     C                   endif
017600110927     C                   if        §2aFTC = 'A'
017700110927     C                   add       1             ii
017800110927     C                   eval      sk2Alavv(ii)  = tblKEY
017900110927     C                   endif
018000110927     C     KEYtab00P     reade     tabel00f
018100110927     C                   enddo
018200041015     C*
018300041015     C* Caricamento tipi anomalia C/A
018400041015     C     'TAD'         SETLL     tntbe01l
018500041015     C                   IF        %equal(tntbe01l)
018600041015     C     'TAD'         READE     tntbe01l
018700041015     C                   DOW       not %eof(tntbe01l)
018800041015     C                   EVAL      DTAD = tbeUNI
018900041015     C                   ADD       1             jTAD
019000041015     C                   EVAL      skTADCod(jTAD) = tbeKE1
019100041015     C                   EVAL      skTADRag(jTAD) = §TADRAGR
019200041015     C     'TAD'         READE     tntbe01l
019300041015     C                   ENDDO
019400041015     C                   ENDIF
019500041015     C*
019600041015     C                   ENDSR
019700991027
019800030924
019900030924
020000991027     C     traduci       BEGSR
020100031103     C*
020200031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
020300031103     C* di traduzione (trattasi d "post-traduzione")
020400031104     C                   EVAL      wKstbKSU = w§PPIKSU
020500031103     C                   EVAL      wKstbFTR = *blanks
020600110103     C     KEYstb03P     SETLL     tistb03l
020700110103     C                   IF        %equal(tistb03l)
020800110103     C     KEYstb03P     READE     tistb03l
020900991027     C*
021000031103     C* Ciclo x tuttii record del cliente da trasmettere
021100110103     C                   DOW       not %eof(tistb03l)
021200110929     C*
021300110929     C* Come primissima cosa verifico rittura d codice su key spedizione
021400110929     C                   IF        stbAAS = sAAS AND
021500110929     C                             stbLNP = sLNP AND
021600110929     C                             stbNRS = sNRS AND
021700110929     C                             stbNSP = sNSP
021800110929     C                   ELSE
021900110929     C                   SETOFF                                       40
022000110929     C                   EVAL      sAAS = stbAAS
022100110929     C                   EVAL      sLNP = stbLNP
022200110929     C                   EVAL      sNRS = stbNRS
022300110929     C                   EVAL      sNSP = stbNSP
022400110929     C                   ENDIF
022500031014     C*
022600030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
022700110103     C                   RESET                   DS_Cli_Status
022800080618     C*
022900110927     C                   SETON                                        50
023000031103     C*
023100031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
023200031103     C* deve restituire al cliente nel file tradotto
023300031103     C                   IF        stbFTR = *blanks                             * ridondante
023400110103     C*
023500111031     C* Gestisco solamente alcuni stati
023600110928     C                   IF        (stbTIS= '1' OR
023700110928     C                             (stbTIS= '9' AND stbPRS= '1')) AND
023800111031     C                             ((%subst(stbFOP:1:1)= 'O'  AND
023900111031     C                                        stbCOS<>'RESO') OR
024000111031     C                             ((%subst(stbFOP:1:2)<>'F2' AND
024100111031     C                                        stbSTS='9')))
024200110103     C*
024300110103     C                   SETOFF                                       50
024400031103     C*
024500031103     C* Innanzitutto chaino la bolla direttamente su TITAS
024600031103     C     KEYtas30P     CHAIN     titas30c
024700031103     C                   IF        %found(titas30c)
024800110103     C*
024900110103     C                   clear                   wCAS
025000110927     C                   clear                   wDDP
025100110927     C                   clear                   wTPI
025200110929     C                   clear                   wDCR
025300110103     C*
025400110103     C* Quindi verifico subito esistenza contrassegno
025500110103     C     KEYcsb03C     CHAIN     tncsb03l
025600110103     C                   IF        %found(tncsb03l)
025700110103     C                   eval      wCAS = csbCAS
025800110927     C                   eval      wDDP = csbDDP
025900110927     C                   eval      wTPI = csbTPI
026000110103     C                   ENDIF
026100110929     C*
026200110929     C* Considerazione su eventi DIR
026300110929     C                   IF        stbCOS = 'DIR'
026400110929     C                   SETON                                        40
026500110929     C                   ENDIF
026600080618     C*
026700110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
026800110927     C                   IF         stbTIS= '1' AND
026900110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
027000110927     C*
027100110927     C                   MOVEL     *blanks       wCOS             10
027200110927     C                   MOVEL     *blanks       wCAU             10
027300110927     C                   IF        stbPRS = '3'
027400110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
027500110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
027600110927     C                   ELSE
027700110927     C                   EVAL      wCOS = stbCOS
027800110927     C                   ENDIF
027900110927     C*
028000031103     C                   Z-ADD     1             j
028100110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
028200110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
028300110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
028400110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
028500110927     C*
028600110927     C* Verifico se evento lasciato avviso
028700110927     C                   z-add     1             ii
028800110927     C     wCOS          lookup    sk2Alavv(ii)                           55
028900110927     C                   if        %found
029000110927     C                   eval      DS_CS_LavDat = stbDAS
029100110927     C                   endif
029200110927     C*
029300110927     C* Se stato d tipo giacenza
029400110927     C                   if        stbPRS = '3'
029500110927     C                   eval      DS_CS_GiacDat = stbDAS
029600110927     C*
029700110927     C* Se apertura reperisco il motivo apertura giacenza
029800110927     C                   if        stbSTS = '1'
029900110927     C                   z-add     1             i
030000110927     C     wCAU          lookup    sk2Agiac(i)                            55
030100110927     C                   if        %found
030200110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
030300110927     C                   endif
030400110927     C                   endif
030500110927     C*
030600110927     C* Costruisco la chiave sul file TIGCP51L
030700110927     C                   eval      gcpFRG = *zeros
030800110927     C*
030900110927     C* Reperisco le giacenze relative alla bolla corrente
031000110927     C     KEYgcp51_P    chain     tigcp51l
031100110927     C                   if        %found(tigcp51l)
031200110927     C*
031300110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
031400110927     C     KEYgnp01_P    setll     tignp01l
031500110927     C                   if        %equal(tignp01l)
031600110927     C     KEYgnp01_P    reade     tignp01l
031700110927     C                   dow       not %eof(tignp01l)
031800110927     C*** concateno note giacenza
031900110927     C                   eval      DS_CS_GiacMot =
032000110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
032100110927     C     KEYgnp01_P    reade     tignp01l
032200110927     C                   enddo
032300110927     C                   endif
032400110927     C                   endif
032500110927     C*
032600110927     C                   endif
032700110927     C*
032800031103     C                   ELSE
032900031103     C                   SETON                                        50
033000031103     C                   ENDIF
033100110927     C                   ENDIF
033200110103     C*
033300110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
033400110927     C                   IF        stbTIS= '9' AND
033500110927     C                             stbPRS= '1'
033600110927     C*
033700110927     C                   SETOFF                                       50
033800110927     C*
033900110103     C                   Z-ADD     1             j
034000110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
034100110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
034200110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
034300110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
034400110103     C                   ELSE
034500110103     C                   SETON                                        50
034600110103     C                   ENDIF
034700110103     C                   ENDIF
034800041015     C*
034900080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
035000110927     C                   IF        stbTIS= '1' AND
035100110927     C                             stbPRS= '5'
035200080618     C*
035300110927     C                   SETON                                        50
035400080618     C*
035500080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
035600080618     C                   Z-ADD     1             jTAD
035700080618     C                   MOVEL(P)  stbCOS        wTADCod
035800080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
035900080618     C                   IF        %found
036000110927     C                   Z-ADD     1             j
036100080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
036200080618     C                   IF        %found
036300110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
036400110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
036500110927     C                   SETOFF                                       50
036600080618     C                   ENDIF
036700080618     C                   ENDIF
036800080618     C                   ENDIF
036900080618     C*
037000080618     C* Scarico il buffer d output
037100080618     C  N50              EXSR      WRIREC
037200110103     C*
037300110103     C                   ENDIF
037400110103     C                   ENDIF
037500110103     C                   ENDIF
037600031103     C*
037700031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
037800031103     C                   EVAL      stbFTR = 'T'
037900031103     C                   UPDATE    tistb000
038000991027     C*
038100110103     C     KEYstb03P     READE     tistb03l
038200031103     C                   SETOFF                                       50
038300030325     C                   ENDDO
038400031103     C                   ENDIF
038500991027     C*
038600030325     C                   EVAL      wrkesito = '0'
038700991027     C*
038800910830     C                   ENDSR
038900031103
039000031103
039100031103
039200110103     C     wrirec        BEGSR
039300110928     C*
039400110928     C                   eval      wLNP = tasLNP
039500110928     C                   eval      wLNA = tasLNA
039600110928     C                   eval      wDCR = tasDCR
039700110928     C*
039800110929     C* Se necessario reperisco l'ultima bolla figlia
039900110929     C                   if        *in40
040000110928     C                   movel     *blanks       wEsito2           1
040100110928     C*
040200110928     C* Chiamata metodo GetLastChild
040300110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
040400110928     C                                                stbAAS
040500110928     C                                               :stbLNP
040600110928     C                                               :stbNRS
040700110928     C                                               :stbNSP
040800110928     C                                               :pOutAnnoFI
040900110928     C                                               :pOutLineaParFI
041000110928     C                                               :pOutSerieFI
041100110928     C                                               :pOutNumSpedFI
041200110928     C                                               :pOutDcmFI
041300110928     C                                               :pOutCcaFI))
041400110928     C*
041500110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
041600110928     C                   if        wEsito2 = '0'
041700110929     C*
041800110929     C                   setoff                                       60
041900110928     C*
042000110928     C* - 1° tentativo in filiale arrivo   (FNARB)
042100110929     C     KEYarb01C     chain     fnarb01l
042200110929     C                   if        %found(fnarb01l)
042300110929     C                   seton                                        60
042400110929     C                   eval      wLNP   = arbLNP
042500110929     C                   eval      wLNA   = arbLNA
042600110929     C                   if        arbDCR > wDCR
042700110929     C                   eval      wDCR   = arbDCR
042800110929     C                   endif
042900110929     C                   endif
043000110928     C*
043100110928     C* - 2° tentativo in filiale partenza (FNBLP)
043200110929     C     KEYarb01C     chain     fnblp01l
043300110929     C                   if        %found(fnblp01l)
043400110929     C                   seton                                        60
043500110929     C                   eval      wLNP   = blpLNP
043600110929     C                   eval      wLNA   = blpLNA
043700110929     C                   if        blpDCR > wDCR
043800110929     C                   eval      wDCR   = blpDCR
043900110929     C                   endif
044000110929     C                   endif
044100110928     C*
044200110928     C* - 3° tentativo in sede             (TITAS)
044300110929     C                   if        not *in60
044400110929     C     KEYarb01C     chain     titas30c
044500110929     C                   if        %found(titas30c)
044600110929     C                   eval      wLNP   = tasLNP
044700110929     C                   eval      wLNA   = tasLNA
044800110929     C                   eval      wDCR   = tasDCR
044900110929     C                   endif
045000110929     C                   endif
045100110929     C*
045200110928     C                   endif
045300110929     C                   endif
045400110927     C*
045500110928     C     wLNP          chain     azorg01l
045600110927     C                   if        %found(azorg01l)
045700110927     C                   eval      DS_CS_LNP = orgDES
045800110927     C                   endif
045900110927     C*
046000110928     C     wLNA          chain     azorg01l
046100110927     C                   if        %found(azorg01l)
046200110927     C                   eval      DS_CS_LNA = orgDES
046300110927     C                   endif
046400110927     C*
046500110927     C                   eval      DS_CS_RMN     = stbRMN
046600111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
046700110928     C                   eval      DS_CS_DCR     = wDCR
046800110929     C                   eval      DS_CS_CasImp  = wCAS
046900110927     C                   eval      DS_CS_CasDatP = wDDP
047000110927     C                   eval      DS_CS_CasTipI = wTPI
047100031103     C*
047200031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
047300110927     C                   EVAL      VAWDTA =
047400110927     C                                    %trim(%editc(DS_CS_RMN:'X'))
047500110927     C                               +'|'+%trim(DS_CS_LNP)
047600110927     C                               +'|'+%trim(DS_CS_LNA)
047700111020     C                               +'|'+%trim(%editc(DS_CS_EveDaOr:'X'))
047800110927     C                               +'|'+%trim(DS_CS_EveCod)
047900110927     C                               +'|'+%trim(DS_CS_EveDes)
048000110927     C                               +'|'+%trim(%editc(DS_CS_GiacDat:'X'))
048100110927     C                               +'|'+%trim(DS_CS_GiacMot)
048200110927     C                               +'|'+%trim(%editc(DS_CS_LavDat:'X'))
048300110927     C                               +'|'+%trim(%editc(DS_CS_DCR:'X'))
048400110927     C                               +'|'+%trim(%editc(DS_CS_CasImp:'3'))
048500110927     C                               +'|'+%trim(%editc(DS_CS_CasDatP:'X'))
048600110927     C                               +'|'+%trim(DS_CS_CasTipI)
048700110927     C                               +'|'
048800110927     C*
048900031103     C                   WRITE     TIVAW000
049000031103     C*
049100031103     C                   ENDSR
049200031103
049300031103
049400991027
049500991027      /TITLE Operazioni iniziali.
049600991027     C     *inzsr        BEGSR
049700991027     C*
049800991027     C     *ENTRY        PLIST
049900031103     C                   parm                    prmppi
050000991027     C     wrkesito      parm      wrkesito      prmesito
050100031103     C*
050200031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
050300031103     C                   EVAL      DSPPI = prmppi
050400030709     C*
050500030709     C* Definizione chiavi
050600110927     C*
050700110927     C* Chiave su TABEL00F - Parziale
050800110927     C     KEYtab00P     KLIST
050900110927     C                   KFLD                    tblKUT
051000110927     C                   KFLD                    tblCOD
051100030924     C*
051200110103     C* Chiave su TISTB03L - Parziale
051300110103     C     KEYstb03P     KLIST
051400031104     C                   KFLD                    wKstbKSU
051500031103     C                   KFLD                    wKstbFTR
051600031103     C*
051700031103     C* Chiave su TITAS30C - Parziale
051800031103     C     KEYtas30P     KLIST
051900031103     C                   KFLD                    stbAAS
052000031103     C                   KFLD                    stbLNP
052100031103     C                   KFLD                    stbNRS
052200031103     C                   KFLD                    stbNSP
052300110929     C*
052400110929     C* Chiave su FNARB01L / FNBLP01L - Completa
052500110929     C     KEYarb01C     KLIST
052600110929     C                   KFLD                    pOutAnnoFI
052700110929     C                   KFLD                    pOutLineaParFI
052800110929     C                   KFLD                    pOutSerieFI
052900110929     C                   KFLD                    pOutNumSpedFI
053000110103     C*
053100110103     C* Chiave su TNCSB03L - Completa
053200110103     C     KEYcsb03C     KLIST
053300110103     C                   KFLD                    stbAAS
053400110103     C                   KFLD                    stbLNP
053500110103     C                   KFLD                    stbNRS
053600110103     C                   KFLD                    stbNSP
053700110927     C*
053800110927     C* Chiave su TIGCP51L - Parziale
053900110927     C     KEYgcp51_P    KLIST
054000110927     C                   KFLD                    stbAAS
054100110927     C                   KFLD                    stbLNP
054200110927     C                   KFLD                    stbNRS
054300110927     C                   KFLD                    stbNSP
054400110927     C                   KFLD                    gcpFRG
054500110927     C*
054600110927     C* Chiave su TIGNP01L - Parziale
054700110927     C     KEYgnp01_P    KLIST
054800110927     C                   KFLD                    gcpAGC
054900110927     C                   KFLD                    gcpFGC
055000110927     C                   KFLD                    gcpNGC
055100110927     C                   KFLD                    gcpFRG
055200991027     C*
055300991027     C                   ENDSR
055400110103** CAUBAR - CAUSALI BARTOLINI
055500110103A                       L.AVVISO
055600110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
055700110103AVV                     LASCIATO AVVISO 1GG
055800110103AV2                     LASCIATO AVVISO 2GG
055900110103AV3                     LASCIATO AVVISO 3GG
056000110103AV5                     LASCIATO AVVISO 5GG
056100110103AV7                     LASCIATO AVVISO 7GG
056200110103AV9                     LASCIATO AVVISO 9 GIORNI
056300110103A16                     L.A. RIMANDA LO SVINCOLO
056400110103A23                     L.A. CHIUSO FINO AL
056500110103CPO                     CAMBIO PORTO
056600110103DAN                     COLLO/I DANNEGGIATO/I
056700110103DIR                     DIROTTAMENTO
056800110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
056900110103G                       GIACENZA GENERICA TEMPORANEA
057000110103GEN                     GIACENZA GENERICA
057100110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
057200110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
057300110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
057400110103G09                     ATTENDERE 9 GIORNI
057500110103IDD                     CHIUSURA SPEDIZIONE CON IDD
057600110103MAN                     COLLO/I MANCANTE/I
057700110103MIC                     MESSA IN CONSEGNA
057800110103N                       NON FATTA
057900110103NAV                     COLLO SCONDIZIONATO
058000110103NIC                     TOLTA DALLA CONSEGNA
058100110103P                       CONSEGNA PARZIALE
058200110103PAT                     FESTIVITA PATRONALE
058300110103R                       RISERVA
058400110103RDC                     RIPRISTINO
058500110103RFD                     RIFIUTATO
058600110103RFM                     RIFIUTATO
058700110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
058800110103RIP                     RIPRISTINO
058900110103RS                      RISERVA
059000110103T                       CHIUSO PER TURNO
059100110103WA                      AMMANCO SENZA EVENTO
059200110103WAE                     AMMANCO CON EVENTO
059300110103WM                      MANCANZA SENZA EVENTO
059400110103WME                     MANCANZA CON EVENTO
059500110103WV                      AVARIA SENZA EVENTO
059600110103WVE                     AVARIA CON EVENTO
059700110103ZAL                     ALLUVIONE
059800110103ZBS                     BLOCCO STRADALE
059900110103ZFR                     FESTIVITÀ REGIONALE
060000110103ZMM                     MARE MOLTO MOSSO
060100110103ZMP                     MANIFESTAZIONE PUBBLICA
060200110103ZNV                     NEVICATA ECCEZIONALE
060300110103ZSC                     SCIOPERO
060400110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
060500110103002                     MERCE NON ORDINATA O NON CONFORME
060600110103003                     MERCE SPEDITA IN RITARDO
060700110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
060800110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
060900110103006                     MERCE RESA SENZA AUTORIZZAZIONE
061000110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
061100110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
061200110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
061300110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
061400110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
061500110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
061600110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
061700110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
061800110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
061900110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
062000110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
062100110103020                     DESTINATARIO RISULTA TRASFERITO
062200110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
062300110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
062400110103023                     IL DESTINATARIO E' CHIUSO FINO AL
062500110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
062600110103025                     IL DESTINATARIO E' CHIUSO
062700110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
062800110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
062900110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
063000110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
063100110103030                     IL DESTINATARIO RIFIUTA LA MERCE
063200110103031                     IL DESTINATARIO NON INTENDE
063300110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
063400110103033                     IL DESTINATARIO RICHIEDE
063500110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
063600110103035                     DOCUMENTI INCOMPLETI O MANCANTI
063700110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
063800110103037                     RIFIUTA LA CONSEGNA TASSATIVA
063900110103100                     -
064000110103101                     -
064100110103999                     -
064200110103CONSOK                  CONSEGNA OK
064300110103RESO                    RESO
064400110927                        -
064500110929GIACAPE                 APERTURA GIACENZA
064600110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
064700110927GIACEND                 CHIUSURA GIACENZA
064800110927                        -
064900110927                        -
065000110927                        -
065100110927                        -
065200110927                        -
065300110927                        -
065400110927                        -
065500110103** CAUCLI - CAUSALI CLIENTE
065600110926A                       L.AVVISO
065700110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
065800110926AVV                     LASCIATO AVVISO 1GG
065900110926AV2                     LASCIATO AVVISO 2GG
066000110926AV3                     LASCIATO AVVISO 3GG
066100110926AV5                     LASCIATO AVVISO 5GG
066200110926AV7                     LASCIATO AVVISO 7GG
066300110926AV9                     LASCIATO AVVISO 9 GIORNI
066400110926A16                     L.A. RIMANDA LO SVINCOLO
066500110926A23                     L.A. CHIUSO FINO AL
066600110926CPO                     CAMBIO PORTO
066700110926DAN                     COLLO/I DANNEGGIATO/I
066800110926DIR                     DIROTTAMENTO
066900110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
067000110926G                       GIACENZA GENERICA TEMPORANEA
067100110926GEN                     GIACENZA GENERICA
067200110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
067300110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
067400110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
067500110926G09                     ATTENDERE 9 GIORNI
067600110926IDD                     CHIUSURA SPEDIZIONE CON IDD
067700110926MAN                     COLLO/I MANCANTE/I
067800110926MIC                     MESSA IN CONSEGNA
067900110926N                       NON FATTA
068000110926NAV                     COLLO SCONDIZIONATO
068100110926NIC                     TOLTA DALLA CONSEGNA
068200110926P                       CONSEGNA PARZIALE
068300110926PAT                     FESTIVITA PATRONALE
068400110926R                       RISERVA
068500110926RDC                     RIPRISTINO
068600110926RFD                     RIFIUTATO
068700110926RFM                     RIFIUTATO
068800110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
068900110926RIP                     RIPRISTINO
069000110926RS                      RISERVA
069100110926T                       CHIUSO PER TURNO
069200110926WA                      AMMANCO SENZA EVENTO
069300110926WAE                     AMMANCO CON EVENTO
069400110926WM                      MANCANZA SENZA EVENTO
069500110926WME                     MANCANZA CON EVENTO
069600110926WV                      AVARIA SENZA EVENTO
069700110926WVE                     AVARIA CON EVENTO
069800110926ZAL                     ALLUVIONE
069900110926ZBS                     BLOCCO STRADALE
070000110926ZFR                     FESTIVITÀ REGIONALE
070100110926ZMM                     MARE MOLTO MOSSO
070200110926ZMP                     MANIFESTAZIONE PUBBLICA
070300110926ZNV                     NEVICATA ECCEZIONALE
070400110926ZSC                     SCIOPERO
070500110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
070600110926002                     MERCE NON ORDINATA O NON CONFORME
070700110926003                     MERCE SPEDITA IN RITARDO
070800110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
070900110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
071000110926006                     MERCE RESA SENZA AUTORIZZAZIONE
071100110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
071200110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
071300110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
071400110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
071500110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
071600110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
071700110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
071800110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
071900110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
072000110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
072100110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
072200110926020                     DESTINATARIO RISULTA TRASFERITO
072300110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
072400110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
072500110926023                     IL DESTINATARIO E' CHIUSO FINO AL
072600110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
072700110926025                     IL DESTINATARIO E' CHIUSO
072800110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
072900110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
073000110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
073100110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
073200110926030                     IL DESTINATARIO RIFIUTA LA MERCE
073300110926031                     IL DESTINATARIO NON INTENDE
073400110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
073500110926033                     IL DESTINATARIO RICHIEDE
073600110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
073700110926035                     DOCUMENTI INCOMPLETI O MANCANTI
073800110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
073900110926037                     RIFIUTA LA CONSEGNA TASSATIVA
074000110926NON INVIARE             -
074100110926NON INVIARE             -
074200110926NON INVIARE             -
074300110926CONSOK                  CONSEGNA OK
074400110926RESO                    RESO
074500110926
074600110927*AG                     APERTURA GIACENZA
074700110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
074800110927*CG                     CHIUSURA GIACENZA
074900110927                        -
075000110927                        -
075100110927                        -
075200110927                        -
075300110927                        -
075400110927                        -
075500110927                        -
075600110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
075700110103L.AVVISO
075800110103AVVISATO-RIMANDA SVINCOLO 1GG
075900110103LASCIATO AVVISO 1GG
076000110103LASCIATO AVVISO 2GG
076100110103LASCIATO AVVISO 3GG
076200110103LASCIATO AVVISO 5GG
076300110103LASCIATO AVVISO 7GG
076400110103LASCIATO AVVISO 9 GIORNI
076500110103L.A. RIMANDA LO SVINCOLO
076600110103L.A. CHIUSO FINO AL
076700110103CAMBIO PORTO
076800110103COLLO/I DANNEGGIATO/I
076900110103DIROTTAMENTO
077000110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
077100110103GIACENZA GENERICA TEMPORANEA
077200110103GIACENZA GENERICA
077300110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
077400110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
077500110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
077600110103ATTENDERE 9 GIORNI
077700110103CHIUSURA SPEDIZIONE CON IDD
077800110103COLLO/I MANCANTE/I
077900110103MESSA IN CONSEGNA
078000110103NON FATTA
078100110103COLLO SCONDIZIONATO
078200110103TOLTA DALLA CONSEGNA
078300110103CONSEGNA PARZIALE
078400110103FESTIVITA PATRONALE
078500110103RISERVA
078600110103RIPRISTINO
078700110103RIFIUTATO
078800110103RIFIUTATO
078900110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
079000110103RIPRISTINO
079100110103RISERVA
079200110103CHIUSO PER TURNO
079300110103AMMANCO SENZA EVENTO
079400110103AMMANCO CON EVENTO
079500110103MANCANZA SENZA EVENTO
079600110103MANCANZA CON EVENTO
079700110103AVARIA SENZA EVENTO
079800110103AVARIA CON EVENTO
079900110103ALLUVIONE
080000110103BLOCCO STRADALE
080100110103FESTIVITÀ REGIONALE
080200110103MARE MOLTO MOSSO
080300110103MANIFESTAZIONE PUBBLICA
080400110103NEVICATA ECCEZIONALE
080500110103SCIOPERO
080600110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
080700110103MERCE NON ORDINATA O NON CONFORME
080800110103MERCE SPEDITA IN RITARDO
080900110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
081000110103MERCE SPEDITA CON TROPPO ANTICIPO
081100110103MERCE RESA SENZA AUTORIZZAZIONE
081200110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
081300110103DESTINATARIO NON PAGA IL C/ASSEGNO
081400110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
081500110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
081600110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
081700110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
081800110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
081900110103DESTINATARIO RIMANDA LO SVINCOLO AL
082000110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
082100110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
082200110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
082300110103DESTINATARIO RISULTA TRASFERITO
082400110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
082500110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
082600110103IL DESTINATARIO E' CHIUSO FINO AL
082700110103IL DESTINATARIO E' CHIUSO PER FERIE
082800110103IL DESTINATARIO E' CHIUSO
082900110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
083000110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
083100110103ESERCIZIO NON ANCORA IN ATTIVITA
083200110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
083300110103IL DESTINATARIO RIFIUTA LA MERCE
083400110103IL DESTINATARIO NON INTENDE
083500110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
083600110103IL DESTINATARIO RICHIEDE
083700110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
083800110103DOCUMENTI INCOMPLETI O MANCANTI
083900110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
084000110103RIFIUTA LA CONSEGNA TASSATIVA
084100110103-
084200110103-
084300110103-
084400110103CONSEGNA OK
084500110103RESO
084600110927-
084700110927APERTURA GIACENZA
084800110927RICEVUTE DISPOSIZIONI GIACENZA
084900110927CHIUSURA GIACENZA
085000110927-
085100110927-
085200110927-
085300110927-
085400110927-
085500110927-
085600110927-
085700041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
085800110103A                                                                                            1
085900110103M                                                                                            2
086000110103V                                                                                            3
086100110103E                                                                                            4
086200080901** WDCTCLI - CAUSALI CLIENTE
086300110926TAD_A                                                                                        1
086400110926TAD_M                                                                                        2
086500110926TAD_V                                                                                        3
086600110926TAD_E                                                                                        4
086700110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
086800110927AMMANCO                                                                                      1
086900110927MANCA COLLO                                                                                  2
087000110927AVARIA                                                                                       3
087100110927EVENTO                                                                                       4
087200110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
087300110103INCARICO1                                                                                    1
087400110103INCARICO2                                                                                    2
087500110103CONSOK1                                                                                      3
087600110103CONSOK2                                                                                      4
087700110927GIACNODIS1                                                                                   5
087800110927GIACNODIS2                                                                                   6
087900110927GIACSIDIS1                                                                                   7
088000110927GIACSIDIS2                                                                                   8
088100110927                                                                                             9
088200110927                                                                                             10
088300110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
088400110926ACCETTAZIONE                                                                                 1
088500110926ACCETTAZIONE                                                                                 2
088600110103NON INVIARE                                                                                  3
088700110103NON INVIARE                                                                                  4
088800110927GIAC_APERTA                                                                                  5
088900110927GIAC_APERTA                                                                                  6
089000110927GIAC_DISPOS                                                                                  7
089100110927GIAC_DISPOS                                                                                  8
089200110927NON INVIARE                                                                                  9
089300110927NON INVIARE                                                                                  10
089400110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
089500110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
089600110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
089700110926NON INVIARE                                                                                  3
089800110926NON INVIARE                                                                                  4
089900110927APERTURA GIACENZA                                                                            5
090000110927APERTURA GIACENZA                                                                            6
090100110927RICEVUTE DISPOSIZIONI                                                                        7
090200110927RICEVUTE DISPOSIZIONI                                                                        8
090300110927NON INVIARE                                                                                  8
090400110927NON INVIARE                                                                                  10
