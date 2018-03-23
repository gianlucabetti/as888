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
021300111103     C* Come primissima cosa verifico rottura d codice su key spedizione
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
023800111103     C                             (%subst(stbFOP:1:1)= 'O'  OR
023900111103     C                             (%subst(stbFOP:1:1)= 'F' AND stbSTS= '9'))
024000110103     C*
024100110103     C                   SETOFF                                       50
024200031103     C*
024300031103     C* Innanzitutto chaino la bolla direttamente su TITAS
024400031103     C     KEYtas30P     CHAIN     titas30c
024500031103     C                   IF        %found(titas30c)
024600110103     C*
024700110103     C                   clear                   wCAS
024800110927     C                   clear                   wDDP
024900110927     C                   clear                   wTPI
025000110929     C                   clear                   wDCR
025100110103     C*
025200110103     C* Quindi verifico subito esistenza contrassegno
025300110103     C     KEYcsb03C     CHAIN     tncsb03l
025400110103     C                   IF        %found(tncsb03l)
025500110103     C                   eval      wCAS = csbCAS
025600110927     C                   eval      wDDP = csbDDP
025700110927     C                   eval      wTPI = csbTPI
025800110103     C                   ENDIF
025900110929     C*
026000110929     C* Considerazione su eventi DIR
026100110929     C                   IF        stbCOS = 'DIR'
026200110929     C                   SETON                                        40
026300110929     C                   ENDIF
026400080618     C*
026500110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
026600110927     C                   IF         stbTIS= '1' AND
026700110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
026800110927     C*
026900110927     C                   MOVEL     *blanks       wCOS             10
027000110927     C                   MOVEL     *blanks       wCAU             10
027100110927     C                   IF        stbPRS = '3'
027200110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
027300110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
027400110927     C                   ELSE
027500110927     C                   EVAL      wCOS = stbCOS
027600110927     C                   ENDIF
027700110927     C*
027800031103     C                   Z-ADD     1             j
027900110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
028000110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
028100110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
028200110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
028300110927     C*
028400110927     C* Verifico se evento lasciato avviso
028500110927     C                   z-add     1             ii
028600110927     C     wCOS          lookup    sk2Alavv(ii)                           55
028700110927     C                   if        %found
028800110927     C                   eval      DS_CS_LavDat = stbDAS
028900110927     C                   endif
029000110927     C*
029100110927     C* Se stato d tipo giacenza
029200110927     C                   if        stbPRS = '3'
029300110927     C                   eval      DS_CS_GiacDat = stbDAS
029400110927     C*
029500110927     C* Se apertura reperisco il motivo apertura giacenza
029600110927     C                   if        stbSTS = '1'
029700110927     C                   z-add     1             i
029800110927     C     wCAU          lookup    sk2Agiac(i)                            55
029900110927     C                   if        %found
030000110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
030100110927     C                   endif
030200110927     C                   endif
030300110927     C*
030400110927     C* Costruisco la chiave sul file TIGCP51L
030500110927     C                   eval      gcpFRG = *zeros
030600110927     C*
030700110927     C* Reperisco le giacenze relative alla bolla corrente
030800110927     C     KEYgcp51_P    chain     tigcp51l
030900110927     C                   if        %found(tigcp51l)
031000110927     C*
031100110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
031200110927     C     KEYgnp01_P    setll     tignp01l
031300110927     C                   if        %equal(tignp01l)
031400110927     C     KEYgnp01_P    reade     tignp01l
031500110927     C                   dow       not %eof(tignp01l)
031600110927     C*** concateno note giacenza
031700110927     C                   eval      DS_CS_GiacMot =
031800110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
031900110927     C     KEYgnp01_P    reade     tignp01l
032000110927     C                   enddo
032100110927     C                   endif
032200110927     C                   endif
032300110927     C*
032400110927     C                   endif
032500110927     C*
032600031103     C                   ELSE
032700031103     C                   SETON                                        50
032800031103     C                   ENDIF
032900110927     C                   ENDIF
033000110103     C*
033100110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
033200110927     C                   IF        stbTIS= '9' AND
033300110927     C                             stbPRS= '1'
033400110927     C*
033500110927     C                   SETOFF                                       50
033600110927     C*
033700110103     C                   Z-ADD     1             j
033800110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
033900110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
034000110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
034100110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
034200110103     C                   ELSE
034300110103     C                   SETON                                        50
034400110103     C                   ENDIF
034500110103     C                   ENDIF
034600041015     C*
034700080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
034800110927     C                   IF        stbTIS= '1' AND
034900110927     C                             stbPRS= '5'
035000080618     C*
035100110927     C                   SETON                                        50
035200080618     C*
035300080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
035400080618     C                   Z-ADD     1             jTAD
035500080618     C                   MOVEL(P)  stbCOS        wTADCod
035600080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
035700080618     C                   IF        %found
035800110927     C                   Z-ADD     1             j
035900080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
036000080618     C                   IF        %found
036100110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
036200110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
036300110927     C                   SETOFF                                       50
036400080618     C                   ENDIF
036500080618     C                   ENDIF
036600080618     C                   ENDIF
036700080618     C*
036800080618     C* Scarico il buffer d output
036900080618     C  N50              EXSR      WRIREC
037000110103     C*
037100110103     C                   ENDIF
037200110103     C                   ENDIF
037300110103     C                   ENDIF
037400031103     C*
037500031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
037600031103     C                   EVAL      stbFTR = 'T'
037700031103     C                   UPDATE    tistb000
037800991027     C*
037900110103     C     KEYstb03P     READE     tistb03l
038000031103     C                   SETOFF                                       50
038100030325     C                   ENDDO
038200031103     C                   ENDIF
038300991027     C*
038400030325     C                   EVAL      wrkesito = '0'
038500991027     C*
038600910830     C                   ENDSR
038700031103
038800031103
038900031103
039000110103     C     wrirec        BEGSR
039100110928     C*
039200110928     C                   eval      wLNP = tasLNP
039300110928     C                   eval      wLNA = tasLNA
039400110928     C                   eval      wDCR = tasDCR
039500111103     C*
039600111103     C                   movel     *blanks       wEsito1           1
039700110928     C                   movel     *blanks       wEsito2           1
039800111103     C*
039900111103     C* Chiamata metodo GetLblTyp
040000111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
040100111103     C                                                stbAAS
040200111103     C                                               :stbLNP
040300111103     C                                               :stbNRS
040400111103     C                                               :stbNSP
040500111103     C                                               :pOutLblTyp
040600111103     C                                               :pOutAnnoBO
040700111103     C                                               :pOutLineaParBO
040800111103     C                                               :pOutSerieBO
040900111103     C                                               :pOutNumSpedBO
041000111103     C                                               :pOutDcmBO
041100111103     C                                               :pOutCcaBO
041200111103     C                                               :pOutRblBO))
041300111103     C*
041400111103     C                   if        wEsito1 = '0'
041500111103     C                   if        pOutCcaBO = '2' AND
041600111103     C                             stbDAS    > pOutDcmBO
041700111103     C                   eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
041800111103     C                             ' COMMERCITY'
041900111103     C                   endif
042000111103     C                   endif
042100111103     C*
042200111103     C* Se necessario reperisco l'ultima bolla figlia
042300111103     C                   if        *in40
042400110928     C*
042500110928     C* Chiamata metodo GetLastChild
042600110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
042700110928     C                                                stbAAS
042800110928     C                                               :stbLNP
042900110928     C                                               :stbNRS
043000110928     C                                               :stbNSP
043100110928     C                                               :pOutAnnoFI
043200110928     C                                               :pOutLineaParFI
043300110928     C                                               :pOutSerieFI
043400110928     C                                               :pOutNumSpedFI
043500110928     C                                               :pOutDcmFI
043600110928     C                                               :pOutCcaFI))
043700110928     C*
043800110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
043900110928     C                   if        wEsito2 = '0'
044000110929     C*
044100110929     C                   setoff                                       60
044200110928     C*
044300110928     C* - 1° tentativo in filiale arrivo   (FNARB)
044400110929     C     KEYarb01C     chain     fnarb01l
044500110929     C                   if        %found(fnarb01l)
044600110929     C                   seton                                        60
044700110929     C                   eval      wLNP   = arbLNP
044800110929     C                   eval      wLNA   = arbLNA
044900110929     C                   if        arbDCR > wDCR
045000110929     C                   eval      wDCR   = arbDCR
045100110929     C                   endif
045200110929     C                   endif
045300110928     C*
045400110928     C* - 2° tentativo in filiale partenza (FNBLP)
045500110929     C     KEYarb01C     chain     fnblp01l
045600110929     C                   if        %found(fnblp01l)
045700110929     C                   seton                                        60
045800110929     C                   eval      wLNP   = blpLNP
045900110929     C                   eval      wLNA   = blpLNA
046000110929     C                   if        blpDCR > wDCR
046100110929     C                   eval      wDCR   = blpDCR
046200110929     C                   endif
046300110929     C                   endif
046400110928     C*
046500110928     C* - 3° tentativo in sede             (TITAS)
046600110929     C                   if        not *in60
046700110929     C     KEYarb01C     chain     titas30c
046800110929     C                   if        %found(titas30c)
046900110929     C                   eval      wLNP   = tasLNP
047000110929     C                   eval      wLNA   = tasLNA
047100110929     C                   eval      wDCR   = tasDCR
047200110929     C                   endif
047300110929     C                   endif
047400110929     C*
047500110928     C                   endif
047600110929     C                   endif
047700110927     C*
047800110928     C     wLNP          chain     azorg01l
047900110927     C                   if        %found(azorg01l)
048000110927     C                   eval      DS_CS_LNP = orgDES
048100110927     C                   endif
048200110927     C*
048300110928     C     wLNA          chain     azorg01l
048400110927     C                   if        %found(azorg01l)
048500110927     C                   eval      DS_CS_LNA = orgDES
048600110927     C                   endif
048700110927     C*
048800110927     C                   eval      DS_CS_RMN     = stbRMN
048900111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
049000110928     C                   eval      DS_CS_DCR     = wDCR
049100110929     C                   eval      DS_CS_CasImp  = wCAS
049200110927     C                   eval      DS_CS_CasDatP = wDDP
049300110927     C                   eval      DS_CS_CasTipI = wTPI
049400031103     C*
049500031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
049600110927     C                   EVAL      VAWDTA =
049700110927     C                                    %trim(%editc(DS_CS_RMN:'X'))
049800110927     C                               +'|'+%trim(DS_CS_LNP)
049900110927     C                               +'|'+%trim(DS_CS_LNA)
050000111020     C                               +'|'+%trim(%editc(DS_CS_EveDaOr:'X'))
050100110927     C                               +'|'+%trim(DS_CS_EveCod)
050200111103     C                               +'|'+%trim(DS_CS_EveDes)
050300110927     C                               +'|'+%trim(%editc(DS_CS_GiacDat:'X'))
050400110927     C                               +'|'+%trim(DS_CS_GiacMot)
050500110927     C                               +'|'+%trim(%editc(DS_CS_LavDat:'X'))
050600110927     C                               +'|'+%trim(%editc(DS_CS_DCR:'X'))
050700110927     C                               +'|'+%trim(%editc(DS_CS_CasImp:'3'))
050800110927     C                               +'|'+%trim(%editc(DS_CS_CasDatP:'X'))
050900110927     C                               +'|'+%trim(DS_CS_CasTipI)
051000110927     C                               +'|'
051100110927     C*
051200031103     C                   WRITE     TIVAW000
051300031103     C*
051400031103     C                   ENDSR
051500031103
051600031103
051700991027
051800991027      /TITLE Operazioni iniziali.
051900991027     C     *inzsr        BEGSR
052000991027     C*
052100991027     C     *ENTRY        PLIST
052200031103     C                   parm                    prmppi
052300991027     C     wrkesito      parm      wrkesito      prmesito
052400031103     C*
052500031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
052600031103     C                   EVAL      DSPPI = prmppi
052700030709     C*
052800030709     C* Definizione chiavi
052900110927     C*
053000110927     C* Chiave su TABEL00F - Parziale
053100110927     C     KEYtab00P     KLIST
053200110927     C                   KFLD                    tblKUT
053300110927     C                   KFLD                    tblCOD
053400030924     C*
053500110103     C* Chiave su TISTB03L - Parziale
053600110103     C     KEYstb03P     KLIST
053700031104     C                   KFLD                    wKstbKSU
053800031103     C                   KFLD                    wKstbFTR
053900031103     C*
054000031103     C* Chiave su TITAS30C - Parziale
054100031103     C     KEYtas30P     KLIST
054200031103     C                   KFLD                    stbAAS
054300031103     C                   KFLD                    stbLNP
054400031103     C                   KFLD                    stbNRS
054500031103     C                   KFLD                    stbNSP
054600110929     C*
054700110929     C* Chiave su FNARB01L / FNBLP01L - Completa
054800110929     C     KEYarb01C     KLIST
054900110929     C                   KFLD                    pOutAnnoFI
055000110929     C                   KFLD                    pOutLineaParFI
055100110929     C                   KFLD                    pOutSerieFI
055200110929     C                   KFLD                    pOutNumSpedFI
055300110103     C*
055400110103     C* Chiave su TNCSB03L - Completa
055500110103     C     KEYcsb03C     KLIST
055600110103     C                   KFLD                    stbAAS
055700110103     C                   KFLD                    stbLNP
055800110103     C                   KFLD                    stbNRS
055900110103     C                   KFLD                    stbNSP
056000110927     C*
056100110927     C* Chiave su TIGCP51L - Parziale
056200110927     C     KEYgcp51_P    KLIST
056300110927     C                   KFLD                    stbAAS
056400110927     C                   KFLD                    stbLNP
056500110927     C                   KFLD                    stbNRS
056600110927     C                   KFLD                    stbNSP
056700110927     C                   KFLD                    gcpFRG
056800110927     C*
056900110927     C* Chiave su TIGNP01L - Parziale
057000110927     C     KEYgnp01_P    KLIST
057100110927     C                   KFLD                    gcpAGC
057200110927     C                   KFLD                    gcpFGC
057300110927     C                   KFLD                    gcpNGC
057400110927     C                   KFLD                    gcpFRG
057500991027     C*
057600991027     C                   ENDSR
057700110103** CAUBAR - CAUSALI BARTOLINI
057800110103A                       L.AVVISO
057900110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
058000110103AVV                     LASCIATO AVVISO 1GG
058100110103AV2                     LASCIATO AVVISO 2GG
058200110103AV3                     LASCIATO AVVISO 3GG
058300110103AV5                     LASCIATO AVVISO 5GG
058400110103AV7                     LASCIATO AVVISO 7GG
058500110103AV9                     LASCIATO AVVISO 9 GIORNI
058600110103A16                     L.A. RIMANDA LO SVINCOLO
058700110103A23                     L.A. CHIUSO FINO AL
058800110103CPO                     CAMBIO PORTO
058900110103DAN                     COLLO/I DANNEGGIATO/I
059000110103DIR                     DIROTTAMENTO
059100110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
059200110103G                       GIACENZA GENERICA TEMPORANEA
059300110103GEN                     GIACENZA GENERICA
059400110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
059500110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
059600110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
059700110103G09                     ATTENDERE 9 GIORNI
059800110103IDD                     CHIUSURA SPEDIZIONE CON IDD
059900110103MAN                     COLLO/I MANCANTE/I
060000110103MIC                     MESSA IN CONSEGNA
060100110103N                       NON FATTA
060200110103NAV                     COLLO SCONDIZIONATO
060300110103NIC                     TOLTA DALLA CONSEGNA
060400110103P                       CONSEGNA PARZIALE
060500110103PAT                     FESTIVITA PATRONALE
060600110103R                       RISERVA
060700110103RDC                     RIPRISTINO
060800110103RFD                     RIFIUTATO
060900110103RFM                     RIFIUTATO
061000110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
061100110103RIP                     RIPRISTINO
061200110103RS                      RISERVA
061300110103T                       CHIUSO PER TURNO
061400110103WA                      AMMANCO SENZA EVENTO
061500110103WAE                     AMMANCO CON EVENTO
061600110103WM                      MANCANZA SENZA EVENTO
061700110103WME                     MANCANZA CON EVENTO
061800110103WV                      AVARIA SENZA EVENTO
061900110103WVE                     AVARIA CON EVENTO
062000110103ZAL                     ALLUVIONE
062100110103ZBS                     BLOCCO STRADALE
062200110103ZFR                     FESTIVITÀ REGIONALE
062300110103ZMM                     MARE MOLTO MOSSO
062400110103ZMP                     MANIFESTAZIONE PUBBLICA
062500110103ZNV                     NEVICATA ECCEZIONALE
062600110103ZSC                     SCIOPERO
062700110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
062800110103002                     MERCE NON ORDINATA O NON CONFORME
062900110103003                     MERCE SPEDITA IN RITARDO
063000110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
063100110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
063200110103006                     MERCE RESA SENZA AUTORIZZAZIONE
063300110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
063400110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
063500110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
063600110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
063700110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
063800110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
063900110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
064000110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
064100110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
064200110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
064300110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
064400110103020                     DESTINATARIO RISULTA TRASFERITO
064500110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
064600110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
064700110103023                     IL DESTINATARIO E' CHIUSO FINO AL
064800110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
064900110103025                     IL DESTINATARIO E' CHIUSO
065000110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
065100110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
065200110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
065300110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
065400110103030                     IL DESTINATARIO RIFIUTA LA MERCE
065500110103031                     IL DESTINATARIO NON INTENDE
065600110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
065700110103033                     IL DESTINATARIO RICHIEDE
065800110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
065900110103035                     DOCUMENTI INCOMPLETI O MANCANTI
066000110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
066100110103037                     RIFIUTA LA CONSEGNA TASSATIVA
066200110103100                     -
066300110103101                     -
066400110103999                     -
066500110103CONSOK                  CONSEGNA OK
066600110103RESO                    RESO
066700110927                        -
066800110929GIACAPE                 APERTURA GIACENZA
066900110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
067000110927GIACEND                 CHIUSURA GIACENZA
067100110927                        -
067200110927                        -
067300110927                        -
067400110927                        -
067500110927                        -
067600110927                        -
067700110927                        -
067800110103** CAUCLI - CAUSALI CLIENTE
067900110926A                       L.AVVISO
068000110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
068100110926AVV                     LASCIATO AVVISO 1GG
068200110926AV2                     LASCIATO AVVISO 2GG
068300110926AV3                     LASCIATO AVVISO 3GG
068400110926AV5                     LASCIATO AVVISO 5GG
068500110926AV7                     LASCIATO AVVISO 7GG
068600110926AV9                     LASCIATO AVVISO 9 GIORNI
068700110926A16                     L.A. RIMANDA LO SVINCOLO
068800110926A23                     L.A. CHIUSO FINO AL
068900110926CPO                     CAMBIO PORTO
069000110926DAN                     COLLO/I DANNEGGIATO/I
069100110926DIR                     DIROTTAMENTO
069200110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
069300110926G                       GIACENZA GENERICA TEMPORANEA
069400110926GEN                     GIACENZA GENERICA
069500110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
069600110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
069700110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
069800110926G09                     ATTENDERE 9 GIORNI
069900110926IDD                     CHIUSURA SPEDIZIONE CON IDD
070000110926MAN                     COLLO/I MANCANTE/I
070100110926MIC                     MESSA IN CONSEGNA
070200110926N                       NON FATTA
070300110926NAV                     COLLO SCONDIZIONATO
070400110926NIC                     TOLTA DALLA CONSEGNA
070500110926P                       CONSEGNA PARZIALE
070600110926PAT                     FESTIVITA PATRONALE
070700110926R                       RISERVA
070800110926RDC                     RIPRISTINO
070900110926RFD                     RIFIUTATO
071000110926RFM                     RIFIUTATO
071100110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
071200110926RIP                     RIPRISTINO
071300110926RS                      RISERVA
071400110926T                       CHIUSO PER TURNO
071500110926WA                      AMMANCO SENZA EVENTO
071600110926WAE                     AMMANCO CON EVENTO
071700110926WM                      MANCANZA SENZA EVENTO
071800110926WME                     MANCANZA CON EVENTO
071900110926WV                      AVARIA SENZA EVENTO
072000110926WVE                     AVARIA CON EVENTO
072100110926ZAL                     ALLUVIONE
072200110926ZBS                     BLOCCO STRADALE
072300110926ZFR                     FESTIVITÀ REGIONALE
072400110926ZMM                     MARE MOLTO MOSSO
072500110926ZMP                     MANIFESTAZIONE PUBBLICA
072600110926ZNV                     NEVICATA ECCEZIONALE
072700110926ZSC                     SCIOPERO
072800110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
072900110926002                     MERCE NON ORDINATA O NON CONFORME
073000110926003                     MERCE SPEDITA IN RITARDO
073100110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
073200110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
073300110926006                     MERCE RESA SENZA AUTORIZZAZIONE
073400110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
073500110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
073600110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
073700110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
073800110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
073900110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
074000110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
074100110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
074200110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
074300110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
074400110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
074500110926020                     DESTINATARIO RISULTA TRASFERITO
074600110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
074700110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
074800110926023                     IL DESTINATARIO E' CHIUSO FINO AL
074900110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
075000110926025                     IL DESTINATARIO E' CHIUSO
075100110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
075200110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
075300110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
075400110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
075500110926030                     IL DESTINATARIO RIFIUTA LA MERCE
075600110926031                     IL DESTINATARIO NON INTENDE
075700110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
075800110926033                     IL DESTINATARIO RICHIEDE
075900110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
076000110926035                     DOCUMENTI INCOMPLETI O MANCANTI
076100110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
076200110926037                     RIFIUTA LA CONSEGNA TASSATIVA
076300110926NON INVIARE             -
076400110926NON INVIARE             -
076500110926NON INVIARE             -
076600110926CONSOK                  CONSEGNA OK
076700110926RESO                    RESO
076800110926
076900110927*AG                     APERTURA GIACENZA
077000110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
077100110927*CG                     CHIUSURA GIACENZA
077200110927                        -
077300110927                        -
077400110927                        -
077500110927                        -
077600110927                        -
077700110927                        -
077800110927                        -
077900110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
078000110103L.AVVISO
078100110103AVVISATO-RIMANDA SVINCOLO 1GG
078200110103LASCIATO AVVISO 1GG
078300110103LASCIATO AVVISO 2GG
078400110103LASCIATO AVVISO 3GG
078500110103LASCIATO AVVISO 5GG
078600110103LASCIATO AVVISO 7GG
078700110103LASCIATO AVVISO 9 GIORNI
078800110103L.A. RIMANDA LO SVINCOLO
078900110103L.A. CHIUSO FINO AL
079000110103CAMBIO PORTO
079100110103COLLO/I DANNEGGIATO/I
079200110103DIROTTAMENTO
079300110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
079400110103GIACENZA GENERICA TEMPORANEA
079500110103GIACENZA GENERICA
079600110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
079700110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
079800110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
079900110103ATTENDERE 9 GIORNI
080000110103CHIUSURA SPEDIZIONE CON IDD
080100110103COLLO/I MANCANTE/I
080200110103MESSA IN CONSEGNA
080300110103NON FATTA
080400110103COLLO SCONDIZIONATO
080500110103TOLTA DALLA CONSEGNA
080600110103CONSEGNA PARZIALE
080700110103FESTIVITA PATRONALE
080800110103RISERVA
080900110103RIPRISTINO
081000110103RIFIUTATO
081100110103RIFIUTATO
081200110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
081300110103RIPRISTINO
081400110103RISERVA
081500110103CHIUSO PER TURNO
081600110103AMMANCO SENZA EVENTO
081700110103AMMANCO CON EVENTO
081800110103MANCANZA SENZA EVENTO
081900110103MANCANZA CON EVENTO
082000110103AVARIA SENZA EVENTO
082100110103AVARIA CON EVENTO
082200110103ALLUVIONE
082300110103BLOCCO STRADALE
082400110103FESTIVITÀ REGIONALE
082500110103MARE MOLTO MOSSO
082600110103MANIFESTAZIONE PUBBLICA
082700110103NEVICATA ECCEZIONALE
082800110103SCIOPERO
082900110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
083000110103MERCE NON ORDINATA O NON CONFORME
083100110103MERCE SPEDITA IN RITARDO
083200110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
083300110103MERCE SPEDITA CON TROPPO ANTICIPO
083400110103MERCE RESA SENZA AUTORIZZAZIONE
083500110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
083600110103DESTINATARIO NON PAGA IL C/ASSEGNO
083700110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
083800110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
083900110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
084000110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
084100110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
084200110103DESTINATARIO RIMANDA LO SVINCOLO AL
084300110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
084400110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
084500110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
084600110103DESTINATARIO RISULTA TRASFERITO
084700110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
084800110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
084900110103IL DESTINATARIO E' CHIUSO FINO AL
085000110103IL DESTINATARIO E' CHIUSO PER FERIE
085100110103IL DESTINATARIO E' CHIUSO
085200110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
085300110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
085400110103ESERCIZIO NON ANCORA IN ATTIVITA
085500110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
085600110103IL DESTINATARIO RIFIUTA LA MERCE
085700110103IL DESTINATARIO NON INTENDE
085800110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
085900110103IL DESTINATARIO RICHIEDE
086000110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
086100110103DOCUMENTI INCOMPLETI O MANCANTI
086200110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
086300110103RIFIUTA LA CONSEGNA TASSATIVA
086400110103-
086500110103-
086600110103-
086700110103CONSEGNA OK
086800110103RESO
086900110927-
087000110927APERTURA GIACENZA
087100110927RICEVUTE DISPOSIZIONI GIACENZA
087200110927CHIUSURA GIACENZA
087300110927-
087400110927-
087500110927-
087600110927-
087700110927-
087800110927-
087900110927-
088000041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
088100110103A                                                                                            1
088200110103M                                                                                            2
088300110103V                                                                                            3
088400110103E                                                                                            4
088500080901** WDCTCLI - CAUSALI CLIENTE
088600110926TAD_A                                                                                        1
088700110926TAD_M                                                                                        2
088800110926TAD_V                                                                                        3
088900110926TAD_E                                                                                        4
089000110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
089100110927AMMANCO                                                                                      1
089200110927MANCA COLLO                                                                                  2
089300110927AVARIA                                                                                       3
089400110927EVENTO                                                                                       4
089500110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
089600110103INCARICO1                                                                                    1
089700110103INCARICO2                                                                                    2
089800110103CONSOK1                                                                                      3
089900110103CONSOK2                                                                                      4
090000110927GIACNODIS1                                                                                   5
090100110927GIACNODIS2                                                                                   6
090200110927GIACSIDIS1                                                                                   7
090300110927GIACSIDIS2                                                                                   8
090400110927                                                                                             9
090500110927                                                                                             10
090600110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
090700110926ACCETTAZIONE                                                                                 1
090800110926ACCETTAZIONE                                                                                 2
090900110103NON INVIARE                                                                                  3
091000110103NON INVIARE                                                                                  4
091100110927GIAC_APERTA                                                                                  5
091200110927GIAC_APERTA                                                                                  6
091300110927GIAC_DISPOS                                                                                  7
091400110927GIAC_DISPOS                                                                                  8
091500110927NON INVIARE                                                                                  9
091600110927NON INVIARE                                                                                  10
091700110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
091800110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
091900110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
092000110926NON INVIARE                                                                                  3
092100110926NON INVIARE                                                                                  4
092200110927APERTURA GIACENZA                                                                            5
092300110927APERTURA GIACENZA                                                                            6
092400110927RICEVUTE DISPOSIZIONI                                                                        7
092500110927RICEVUTE DISPOSIZIONI                                                                        8
092600110927NON INVIARE                                                                                  8
092700110927NON INVIARE                                                                                  10
