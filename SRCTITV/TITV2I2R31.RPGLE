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
007400110928     D wLNP            S                   like(tasLNP)
007500110928     D wLNA            S                   like(tasLNA)
007600110928     D wDCR            S                   like(tasDCR)
007700110929
007800110929     D sAAS            S                   like(stbAAS)
007900110929     D sLNP            S                   like(stbLNP)
008000110929     D sNRS            S                   like(stbNRS)
008100110929     D sNSP            S                   like(stbNSP)
008200030924
008300110103
008400110103     D*----------
008500110927     D* DS CLIENTE
008600110103     D*----------
008700110103     D DS_Cli_Status   DS
008800110927     D  DS_CS_RMN                    15  0 inz
008900110927     D  DS_CS_LNP                    20    inz
009000110927     D  DS_CS_LNA                    20    inz
009100111020     D  DS_CS_EveDaOr                12  0 inz
009200110927     D  DS_CS_EveCod                 20    inz
009300110927     D  DS_CS_EveDes                 80    inz
009400110927     D  DS_CS_GiacDat                 8  0 inz
009500110927     D  DS_CS_GiacMot               300    inz
009600110927     D  DS_CS_LavDat                  8  0 inz
009700110927     D  DS_CS_DCR                     8  0 inz
009800110927     D  DS_CS_CasImp                 13  3 inz
009900110927     D  DS_CS_CasDatP                 8  0 inz
010000110927     D  DS_CS_CasTipI                 1    inz
010100120507     D  DS_CS_KeySPED                12    inz
010200110103
010300031103
010400031103     D*-----------
010500031103     D* RIDEFINIZIONE PARAMETRI "POST-TRADUZIONE"
010600031103     D*-----------
010700031103     D DSPPI           DS
010800080618     D  w§PPIKSU                      7  0
010900031103
011000110928
011100110928     D*------------------
011200110928     D* LINKING A DEFINIZIONI ESTERNE
011300110928     D*------------------
011400110928     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
011500110928     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
011600110929     D/COPY GAITRASRC/SRCPROTOPR,UBRTVNETA
011700110929     D/COPY GAITRASRC/SRCPROTOPI,UBRTVNETA
011800110928
011900030924
012000030325
012100030924     C* MAIN/
012200110929     C*
012300110929     C* Reperisco il sistema AS/400 corrente
012400110929     C                   callp     UBRTVNETA_Rtv(currSysNeta)
012500110929     C                   if        %subst(currSysNeta:1:6) = 'SETRAS'
012600110929     C                   eval      LibFileARB01 = 'FILTRA201/FNARB01L'
012700110929     C                   eval      LibFileBLP01 = 'FILTRA201/FNBLP01L'
012800110929     C                   else
012900110929     C                   eval      LibFileARB01 = 'FILTRAPRD/FNARB01L'
013000110929     C                   eval      LibFileBLP01 = 'FILTRAPRD/FNBLP01L'
013100110929     C                   endif
013200110929     C*
013300110929     C* Apertura file "overraidati"
013400110929     C                   if        not %open(FNARB01L)
013500110929     C                   open      FNARB01L
013600110929     C                   endif
013700110929     C                   if        not %open(FNBLP01L)
013800110929     C                   open      FNBLP01L
013900110929     C                   endif
014000110929     C*
014100041015     C*
014200041015     C* Carica i dati tabellati
014300041015     C                   EXSR      cartab
014400041015     C*
014500041015     C* Esegue traduzione
014600991027     C                   EXSR      traduci
014700110929     C*
014800110929     C* Chiusura file "overraidati"
014900110929     C                   if        %open(FNARB01L)
015000110929     C                   close     FNARB01L
015100110929     C                   endif
015200110929     C                   if        %open(FNBLP01L)
015300110929     C                   close     FNBLP01L
015400110929     C                   endif
015500110929     C*
015600921023     C                   SETON                                        LR
015700111121
015800111121
015900111121
016000111121     C     cartab        BEGSR
016100111121     C*
016200111121     C* Carico causali evento 2A divise x categoria (A=Lavv G=Giac)
016300111121     C                   clear                   sk2Alavv
016400111121     C                   clear                   sk2Agiac
016500111121     C                   clear                   sk2Agiacd
016600111121     C                   z-add     *zeros        i
016700111121     C                   z-add     *zeros        ii
016800111121     C                   eval      tblKUT = 1
016900111121     C                   eval      tblCOD = '2A'
017000111121     C     KEYtab00P     setll     tabel00f
017100111121     C     KEYtab00P     reade     tabel00f
017200111121     C                   dow       not %eof(tabel00f)
017300111121     C                   movel(p)  tblUNI        ds2a
017400111121     C                   if        §2aFTC = 'G'
017500111121     C                   add       1             i
017600111121     C                   eval      sk2Agiac(i)   = tblKEY
017700111121     C                   eval      sk2Agiacd(i)  = §2ADES
017800111121     C                   endif
017900111121     C                   if        §2aFTC = 'A'
018000111121     C                   add       1             ii
018100111121     C                   eval      sk2Alavv(ii)  = tblKEY
018200111121     C                   endif
018300111121     C     KEYtab00P     reade     tabel00f
018400111121     C                   enddo
018500111121     C*
018600111121     C* Carico causali evento 2Z x eventi "fittizi"
018700111121     C                   clear                   sk2A_2Z
018800111121     C                   z-add     *zeros        i
018900111121     C                   eval      tblKUT = 1
019000111121     C                   eval      tblCOD = '2Z'
019100111121     C     KEYtab00P     setll     tabel00f
019200111121     C     KEYtab00P     reade     tabel00f
019300111121     C                   dow       not %eof(tabel00f)
019400111121     C                   movel(p)  tblUNI        ds2z
019500111122     C                   if        §2ZCEP <> *blanks
019600111121     C                   add       1             i
019700111122     C                   eval      sk2A_2Z(i) = tblKEY
019800111121     C                   endif
019900111121     C     KEYtab00P     reade     tabel00f
020000111121     C                   enddo
020100111121     C*
020200111121     C* Caricamento tipi anomalia C/A
020300111121     C     'TAD'         SETLL     tntbe01l
020400111121     C                   IF        %equal(tntbe01l)
020500111121     C     'TAD'         READE     tntbe01l
020600111121     C                   DOW       not %eof(tntbe01l)
020700111121     C                   EVAL      DTAD = tbeUNI
020800111121     C                   ADD       1             jTAD
020900111121     C                   EVAL      skTADCod(jTAD) = tbeKE1
021000111121     C                   EVAL      skTADRag(jTAD) = §TADRAGR
021100111121     C     'TAD'         READE     tntbe01l
021200111121     C                   ENDDO
021300111121     C                   ENDIF
021400111121     C*
021500111121     C                   ENDSR
021600991027
021700030924
021800030924
021900991027     C     traduci       BEGSR
022000031103     C*
022100031103     C* Elaboro gli stati bolla ancora nn trasmessi al cliente, del cliente ricevuto nei parametri
022200031103     C* di traduzione (trattasi d "post-traduzione")
022300031104     C                   EVAL      wKstbKSU = w§PPIKSU
022400031103     C                   EVAL      wKstbFTR = *blanks
022500110103     C     KEYstb03P     SETLL     tistb03l
022600110103     C                   IF        %equal(tistb03l)
022700110103     C     KEYstb03P     READE     tistb03l
022800991027     C*
022900031103     C* Ciclo x tuttii record del cliente da trasmettere
023000110103     C                   DOW       not %eof(tistb03l)
023100110929     C*
023200111103     C* Come primissima cosa verifico rottura d codice su key spedizione
023300110929     C                   IF        stbAAS = sAAS AND
023400110929     C                             stbLNP = sLNP AND
023500110929     C                             stbNRS = sNRS AND
023600110929     C                             stbNSP = sNSP
023700110929     C                   ELSE
023800110929     C                   SETOFF                                       40
023900110929     C                   EVAL      sAAS = stbAAS
024000110929     C                   EVAL      sLNP = stbLNP
024100110929     C                   EVAL      sNRS = stbNRS
024200110929     C                   EVAL      sNSP = stbNSP
024300110929     C                   ENDIF
024400031014     C*
024500030925     C* Inizializzo la DS d output con i valori di default della dichiarazione della struttura dati
024600110103     C                   RESET                   DS_Cli_Status
024700080618     C*
024800110927     C                   SETON                                        50
024900031103     C*
025000031103     C* Effettuo le dovute considerazioni sullo stato della bolla in relazione all'output che si
025100031103     C* deve restituire al cliente nel file tradotto
025200111215     C                   IF        stbFTR = *blanks                             * ridondante
025300111215     C*
025400111215     C* Verifico (subito) se spedizione in giacenza
025500111215     C                   exsr      rtvgiac
025600110103     C*
025700111031     C* Gestisco solamente alcuni stati
025800110928     C                   IF        (stbTIS= '1' OR
025900111103     C                             (stbTIS= '9' AND stbPRS= '1'))
026000110103     C*
026100110103     C                   SETOFF                                       50
026200031103     C*
026300031103     C* Innanzitutto chaino la bolla direttamente su TITAS
026400031103     C     KEYtas30P     CHAIN     titas30c
026500031103     C                   IF        %found(titas30c)
026600110103     C*
026700110103     C                   clear                   wCAS
026800110927     C                   clear                   wDDP
026900110927     C                   clear                   wTPI
027000110929     C                   clear                   wDCR
027100110103     C*
027200110103     C* Quindi verifico subito esistenza contrassegno
027300110103     C     KEYcsb03C     CHAIN     tncsb03l
027400110103     C                   IF        %found(tncsb03l)
027500110103     C                   eval      wCAS = csbCAS
027600110927     C                   eval      wDDP = csbDDP
027700110927     C                   eval      wTPI = csbTPI
027800110103     C                   ENDIF
027900110929     C*
028000110929     C* Considerazione su eventi DIR
028100110929     C                   IF        stbCOS = 'DIR'
028200110929     C                   SETON                                        40
028300110929     C                   ENDIF
028400080618     C*
028500110103     C* Effettuo l'abbinamento tra causali standard Bartolini e causali cliente
028600110927     C                   IF         stbTIS= '1' AND
028700110927     C                             (stbPRS= '2' OR stbPRS= '3' OR stbPRS= '4')
028800110927     C*
028900110927     C                   MOVEL     *blanks       wCOS             10
029000110927     C                   MOVEL     *blanks       wCAU             10
029100110927     C                   IF        stbPRS = '3'
029200110927     C                   EVAL      wCOS = %subst(stbCOS:1:7)
029300110928     C                   EVAL      wCAU = %subst(stbCOS:8:3)
029400110927     C                   ELSE
029500110927     C                   EVAL      wCOS = stbCOS
029600110927     C                   ENDIF
029700110927     C*
029800031103     C                   Z-ADD     1             j
029900110927     C     wCOS          LOOKUP    CAUBAR(j)                              55
030000110103     C                   IF        %found AND CAUCLI(j) <> 'NON INVIARE'
030100110927     C                   EVAL      DS_CS_EveCod = CAUCLI(j)
030200110927     C                   EVAL      DS_CS_EveDes = CAUDES(j)
030300110927     C*
030400110927     C* Verifico se evento lasciato avviso
030500110927     C                   z-add     1             ii
030600110927     C     wCOS          lookup    sk2Alavv(ii)                           55
030700110927     C                   if        %found
030800110927     C                   eval      DS_CS_LavDat = stbDAS
030900110927     C                   endif
031000110927     C*
031100110927     C* Se stato d tipo giacenza
031200110927     C                   if        stbPRS = '3'
031300110927     C                   eval      DS_CS_GiacDat = stbDAS
031400110927     C*
031500110927     C* Se apertura reperisco il motivo apertura giacenza
031600110927     C                   if        stbSTS = '1'
031700110927     C                   z-add     1             i
031800110927     C     wCAU          lookup    sk2Agiac(i)                            55
031900110927     C                   if        %found
032000110927     C                   eval      DS_CS_EveDes = sk2Agiacd(i)
032100110927     C                   endif
032200111121     C                   endif
032300111121     C*
032400111121     C                   if        wGiac = 'S'
032500110927     C*
032600110927     C* Scorro tutte le note giacenza relative alla giacebza corrente
032700110927     C     KEYgnp01_P    setll     tignp01l
032800110927     C                   if        %equal(tignp01l)
032900110927     C     KEYgnp01_P    reade     tignp01l
033000110927     C                   dow       not %eof(tignp01l)
033100110927     C*** concateno note giacenza
033200110927     C                   eval      DS_CS_GiacMot =
033300110927     C                             %trim(DS_CS_GiacMot) + ' ' + gnpDMC
033400110927     C     KEYgnp01_P    reade     tignp01l
033500110927     C                   enddo
033600110927     C                   endif
033700111121     C*
033800110927     C                   endif
033900110927     C*
034000110927     C                   endif
034100110927     C*
034200031103     C                   ELSE
034300031103     C                   SETON                                        50
034400031103     C                   ENDIF
034500110927     C                   ENDIF
034600110103     C*
034700110103     C* Effettuo l'abbinamento tra causali personalizzate Bartolini e causali cliente
034800110927     C                   IF        stbTIS= '9' AND
034900110927     C                             stbPRS= '1'
035000110927     C*
035100110927     C                   SETOFF                                       50
035200110927     C*
035300110103     C                   Z-ADD     1             j
035400110927     C     stbCOS        LOOKUP    WCOD9BAR(j)                            55
035500110103     C                   IF        %found AND WCOD9CLI(j) <> 'NON INVIARE'
035600110103     C                   EVAL      DS_CS_EveCod = WCOD9CLI(j)
035700110927     C                   EVAL      DS_CS_EveDes = WCOD9DES(j)
035800110103     C                   ELSE
035900110103     C                   SETON                                        50
036000110103     C                   ENDIF
036100110103     C                   ENDIF
036200041015     C*
036300080618     C* X stati provenienti dai danni, se trattasi d tipi anomalia 'A' o 'M' o 'V'
036400110927     C                   IF        stbTIS= '1' AND
036500110927     C                             stbPRS= '5'
036600080618     C*
036700110927     C                   SETON                                        50
036800080618     C*
036900080901     C* Effettuo l'abbinamento tra causali Bartolini e causali cliente
037000080618     C                   Z-ADD     1             jTAD
037100080618     C                   MOVEL(P)  stbCOS        wTADCod
037200080618     C     wTADCod       LOOKUP    skTADCod(jTAD)                         70
037300080618     C                   IF        %found
037400110927     C                   Z-ADD     1             j
037500080618     C     skTADRag(jTAD)LOOKUP    WDCTBAR(j)                             70
037600080618     C                   IF        %found
037700110103     C                   EVAL      DS_CS_EveCod = WDCTCLI(j)
037800110927     C                   EVAL      DS_CS_EveDes = WDCTDES(j)
037900110927     C                   SETOFF                                       50
038000080618     C                   ENDIF
038100080618     C                   ENDIF
038200080618     C                   ENDIF
038300111121     C*
038400111121     C* Se evento "fittizio" e spedizione in giacenza => nn restituisco lo stato "fittizio"
038500111121     C                   z-add     1             i
038600111121     C     wCOS          lookup    sk2A_2Z                                55
038700120130     C***                if        %found and wGiac = 'S'
038800120130     C                   if        %found
038900111121     C                   SETON                                        50
039000111121     C                   endif
039100080618     C*
039200080618     C* Scarico il buffer d output
039300080618     C  N50              EXSR      WRIREC
039400110103     C*
039500110103     C                   ENDIF
039600110103     C                   ENDIF
039700110103     C                   ENDIF
039800031103     C*
039900031103     C* Aggiorno il flag d trasmissione a 'T'=TRASMESSO
040000031103     C                   EVAL      stbFTR = 'T'
040100031103     C                   UPDATE    tistb000
040200991027     C*
040300110103     C     KEYstb03P     READE     tistb03l
040400031103     C                   SETOFF                                       50
040500030325     C                   ENDDO
040600031103     C                   ENDIF
040700991027     C*
040800030325     C                   EVAL      wrkesito = '0'
040900991027     C*
041000910830     C                   ENDSR
041100111121
041200111121
041300111121
041400111121     C     rtvgiac       BEGSR
041500111121     C*
041600111121     C                   movel     'N'           wGiac             1
041700111121     C*
041800111121     C* Costruisco la chiave sul file TIGCP51L
041900111121     C                   eval      gcpFRG = *zeros
042000111121     C*
042100111121     C* Reperisco le giacenze relative alla bolla corrente
042200111121     C     KEYgcp51_P    chain     tigcp51l
042300111121     C                   if        %found(tigcp51l)
042400111216     C                   if        gcpATB = *blanks AND gcpDCG = 0
042500111121     C                   movel     'S'           wGiac
042600111216     C                   endif
042700111121     C                   endif
042800111121     C*
042900111121     C                   ENDSR
043000031103
043100031103
043200031103
043300110103     C     wrirec        BEGSR
043400110928     C*
043500110928     C                   eval      wLNP = tasLNP
043600110928     C                   eval      wLNA = tasLNA
043700110928     C                   eval      wDCR = tasDCR
043800120229     C*
043900120229     C* Prendo sempre i dati più recenti da ambiente filiale
044000120229     C                   setoff                                       60
044100120229     C*
044200120229     C                   eval      pOutAnnoFI     = tasAAS
044300120229     C                   eval      pOutLineaParFI = tasLNP
044400120229     C                   eval      pOutSerieFI    = tasNRS
044500120229     C                   eval      pOutNumSpedFI  = tasNSP
044600120229     C*
044700120229     C* - 1° tentativo in filiale arrivo   (FNARB)
044800120229     C                   if        not *in60
044900120229     C     KEYarb01C     chain     fnarb01l
045000120229     C                   if        %found(fnarb01l)
045100120229     C                   seton                                        60
045200120229     C                   eval      wLNP   = arbLNP
045300120229     C                   eval      wLNA   = arbLNA
045400120229     C                   if        arbDCR > wDCR
045500120229     C                   eval      wDCR   = arbDCR
045600120229     C                   endif
045700120229     C                   endif
045800120229     C                   endif
045900120229     C*
046000120229     C* - 2° tentativo in filiale partenza (FNBLP)
046100120229     C                   if        not *in60
046200120229     C     KEYarb01C     chain     fnblp01l
046300120229     C                   if        %found(fnblp01l)
046400120229     C                   seton                                        60
046500120229     C                   eval      wLNP   = blpLNP
046600120229     C                   eval      wLNA   = blpLNA
046700120229     C                   if        blpDCR > wDCR
046800120229     C                   eval      wDCR   = blpDCR
046900120229     C                   endif
047000120229     C                   endif
047100120229     C                   endif
047200111103     C*
047300111103     C                   movel     *blanks       wEsito1           1
047400110928     C                   movel     *blanks       wEsito2           1
047500111103     C*
047600111103     C* Chiamata metodo GetLblTyp
047700111103     C                   eval      wEsito1 = %char(UBLBLSPE_GetLblTyp(
047800111103     C                                                stbAAS
047900111103     C                                               :stbLNP
048000111103     C                                               :stbNRS
048100111103     C                                               :stbNSP
048200111103     C                                               :pOutLblTyp
048300111103     C                                               :pOutAnnoBO
048400111103     C                                               :pOutLineaParBO
048500111103     C                                               :pOutSerieBO
048600111103     C                                               :pOutNumSpedBO
048700111103     C                                               :pOutDcmBO
048800111103     C                                               :pOutCcaBO
048900111103     C                                               :pOutRblBO))
049000111103     C*
049100111103     C                   if        wEsito1 = '0'
049200111103     C                   if        pOutCcaBO = '2' AND
049300111103     C                             stbDAS    > pOutDcmBO
049400111103     C                   eval      DS_CS_EveDes = %trim(DS_CS_EveDes) +
049500111103     C                             ' COMMERCITY'
049600111103     C                   endif
049700111103     C                   endif
049800111103     C*
049900111103     C* Se necessario reperisco l'ultima bolla figlia
050000111103     C                   if        *in40
050100110928     C*
050200110928     C* Chiamata metodo GetLastChild
050300110928     C                   eval      wEsito2 = %char(UBLBLSPE_GetLastChild(
050400110928     C                                                stbAAS
050500110928     C                                               :stbLNP
050600110928     C                                               :stbNRS
050700110928     C                                               :stbNSP
050800110928     C                                               :pOutAnnoFI
050900110928     C                                               :pOutLineaParFI
051000110928     C                                               :pOutSerieFI
051100110928     C                                               :pOutNumSpedFI
051200110928     C                                               :pOutDcmFI
051300110928     C                                               :pOutCcaFI))
051400110928     C*
051500110928     C* Se esito chiamata ok => reperisco i dati della bolla figlia
051600110928     C                   if        wEsito2 = '0'
051700110929     C*
051800110929     C                   setoff                                       60
051900110928     C*
052000110928     C* - 1° tentativo in filiale arrivo   (FNARB)
052100111216     C                   if        not *in60
052200110929     C     KEYarb01C     chain     fnarb01l
052300110929     C                   if        %found(fnarb01l)
052400110929     C                   seton                                        60
052500110929     C                   eval      wLNP   = arbLNP
052600110929     C                   eval      wLNA   = arbLNA
052700110929     C                   if        arbDCR > wDCR
052800110929     C                   eval      wDCR   = arbDCR
052900110929     C                   endif
053000110929     C                   endif
053100111216     C                   endif
053200110928     C*
053300110928     C* - 2° tentativo in filiale partenza (FNBLP)
053400111216     C                   if        not *in60
053500110929     C     KEYarb01C     chain     fnblp01l
053600110929     C                   if        %found(fnblp01l)
053700110929     C                   seton                                        60
053800110929     C                   eval      wLNP   = blpLNP
053900110929     C                   eval      wLNA   = blpLNA
054000110929     C                   if        blpDCR > wDCR
054100110929     C                   eval      wDCR   = blpDCR
054200110929     C                   endif
054300110929     C                   endif
054400111216     C                   endif
054500110928     C*
054600110928     C* - 3° tentativo in sede             (TITAS)
054700110929     C                   if        not *in60
054800110929     C     KEYarb01C     chain     titas30c
054900110929     C                   if        %found(titas30c)
055000110929     C                   eval      wLNP   = tasLNP
055100110929     C                   eval      wLNA   = tasLNA
055200110929     C                   eval      wDCR   = tasDCR
055300110929     C                   endif
055400110929     C                   endif
055500110929     C*
055600110928     C                   endif
055700110929     C                   endif
055800110927     C*
055900110928     C     wLNP          chain     azorg01l
056000110927     C                   if        %found(azorg01l)
056100110927     C                   eval      DS_CS_LNP = orgDES
056200110927     C                   endif
056300110927     C*
056400110928     C     wLNA          chain     azorg01l
056500110927     C                   if        %found(azorg01l)
056600110927     C                   eval      DS_CS_LNA = orgDES
056700110927     C                   endif
056800110927     C*
056900110927     C                   eval      DS_CS_RMN     = stbRMN
057000111020     C                   eval      DS_CS_EveDaOr = stbDAS*10000+stbORS
057100110928     C                   eval      DS_CS_DCR     = wDCR
057200110929     C                   eval      DS_CS_CasImp  = wCAS
057300110927     C                   eval      DS_CS_CasDatP = wDDP
057400110927     C                   eval      DS_CS_CasTipI = wTPI
057500120507     C                   eval      DS_CS_KeySPED = %trim(%editc(tasLNP:'X'))+
057600120507     C                                             %trim(%editc(tasNRS:'X'))+
057700120507     C                                             %trim(%editc(tasNSP:'X'))
057800031103     C*
057900031103     C* Scarico il buffer della DS cliente nel campo dati del flat-file ASCII e scrivo
058000110927     C                   EVAL      VAWDTA =
058100110927     C                                    %trim(%editc(DS_CS_RMN:'X'))
058200110927     C                               +'|'+%trim(DS_CS_LNP)
058300110927     C                               +'|'+%trim(DS_CS_LNA)
058400111020     C                               +'|'+%trim(%editc(DS_CS_EveDaOr:'X'))
058500110927     C                               +'|'+%trim(DS_CS_EveCod)
058600111103     C                               +'|'+%trim(DS_CS_EveDes)
058700110927     C                               +'|'+%trim(%editc(DS_CS_GiacDat:'X'))
058800110927     C                               +'|'+%trim(DS_CS_GiacMot)
058900110927     C                               +'|'+%trim(%editc(DS_CS_LavDat:'X'))
059000110927     C                               +'|'+%trim(%editc(DS_CS_DCR:'X'))
059100110927     C                               +'|'+%trim(%editc(DS_CS_CasImp:'3'))
059200110927     C                               +'|'+%trim(%editc(DS_CS_CasDatP:'X'))
059300110927     C                               +'|'+%trim(DS_CS_CasTipI)
059400120507     C                               +'|'+%trim(DS_CS_KeySPED)
059500110927     C                               +'|'
059600110927     C*
059700031103     C                   WRITE     TIVAW000
059800031103     C*
059900031103     C                   ENDSR
060000031103
060100031103
060200991027
060300991027      /TITLE Operazioni iniziali.
060400991027     C     *inzsr        BEGSR
060500991027     C*
060600991027     C     *ENTRY        PLIST
060700031103     C                   parm                    prmppi
060800991027     C     wrkesito      parm      wrkesito      prmesito
060900031103     C*
061000031103     C* Ridefinisco subito i parametri d "post-traduzione" ricevuti in input
061100031103     C                   EVAL      DSPPI = prmppi
061200030709     C*
061300030709     C* Definizione chiavi
061400110927     C*
061500110927     C* Chiave su TABEL00F - Parziale
061600110927     C     KEYtab00P     KLIST
061700110927     C                   KFLD                    tblKUT
061800110927     C                   KFLD                    tblCOD
061900030924     C*
062000110103     C* Chiave su TISTB03L - Parziale
062100110103     C     KEYstb03P     KLIST
062200031104     C                   KFLD                    wKstbKSU
062300031103     C                   KFLD                    wKstbFTR
062400031103     C*
062500031103     C* Chiave su TITAS30C - Parziale
062600031103     C     KEYtas30P     KLIST
062700031103     C                   KFLD                    stbAAS
062800031103     C                   KFLD                    stbLNP
062900031103     C                   KFLD                    stbNRS
063000031103     C                   KFLD                    stbNSP
063100110929     C*
063200110929     C* Chiave su FNARB01L / FNBLP01L - Completa
063300110929     C     KEYarb01C     KLIST
063400110929     C                   KFLD                    pOutAnnoFI
063500110929     C                   KFLD                    pOutLineaParFI
063600110929     C                   KFLD                    pOutSerieFI
063700110929     C                   KFLD                    pOutNumSpedFI
063800110103     C*
063900110103     C* Chiave su TNCSB03L - Completa
064000110103     C     KEYcsb03C     KLIST
064100110103     C                   KFLD                    stbAAS
064200110103     C                   KFLD                    stbLNP
064300110103     C                   KFLD                    stbNRS
064400110103     C                   KFLD                    stbNSP
064500110927     C*
064600110927     C* Chiave su TIGCP51L - Parziale
064700110927     C     KEYgcp51_P    KLIST
064800110927     C                   KFLD                    stbAAS
064900110927     C                   KFLD                    stbLNP
065000110927     C                   KFLD                    stbNRS
065100110927     C                   KFLD                    stbNSP
065200110927     C                   KFLD                    gcpFRG
065300110927     C*
065400110927     C* Chiave su TIGNP01L - Parziale
065500110927     C     KEYgnp01_P    KLIST
065600110927     C                   KFLD                    gcpAGC
065700110927     C                   KFLD                    gcpFGC
065800110927     C                   KFLD                    gcpNGC
065900110927     C                   KFLD                    gcpFRG
066000991027     C*
066100991027     C                   ENDSR
066200110103** CAUBAR - CAUSALI BARTOLINI
066300110103A                       L.AVVISO
066400110103ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
066500110103AVV                     LASCIATO AVVISO 1GG
066600110103AV2                     LASCIATO AVVISO 2GG
066700110103AV3                     LASCIATO AVVISO 3GG
066800110103AV5                     LASCIATO AVVISO 5GG
066900110103AV7                     LASCIATO AVVISO 7GG
067000110103AV9                     LASCIATO AVVISO 9 GIORNI
067100110103A16                     L.A. RIMANDA LO SVINCOLO
067200110103A23                     L.A. CHIUSO FINO AL
067300110103CPO                     CAMBIO PORTO
067400110103DAN                     COLLO/I DANNEGGIATO/I
067500110103DIR                     DIROTTAMENTO
067600110103F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
067700110103G                       GIACENZA GENERICA TEMPORANEA
067800110103GEN                     GIACENZA GENERICA
067900110103G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
068000110103G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
068100110103G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
068200110103G09                     ATTENDERE 9 GIORNI
068300110103IDD                     CHIUSURA SPEDIZIONE CON IDD
068400110103MAN                     COLLO/I MANCANTE/I
068500110103MIC                     MESSA IN CONSEGNA
068600110103N                       NON FATTA
068700110103NAV                     COLLO SCONDIZIONATO
068800110103NIC                     TOLTA DALLA CONSEGNA
068900110103P                       CONSEGNA PARZIALE
069000110103PAT                     FESTIVITA PATRONALE
069100110103R                       RISERVA
069200110103RDC                     RIPRISTINO
069300110103RFD                     RIFIUTATO
069400110103RFM                     RIFIUTATO
069500110103RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
069600110103RIP                     RIPRISTINO
069700110103RS                      RISERVA
069800110103T                       CHIUSO PER TURNO
069900110103WA                      AMMANCO SENZA EVENTO
070000110103WAE                     AMMANCO CON EVENTO
070100110103WM                      MANCANZA SENZA EVENTO
070200110103WME                     MANCANZA CON EVENTO
070300110103WV                      AVARIA SENZA EVENTO
070400110103WVE                     AVARIA CON EVENTO
070500110103ZAL                     ALLUVIONE
070600110103ZBS                     BLOCCO STRADALE
070700110103ZFR                     FESTIVITÀ REGIONALE
070800110103ZMM                     MARE MOLTO MOSSO
070900110103ZMP                     MANIFESTAZIONE PUBBLICA
071000110103ZNV                     NEVICATA ECCEZIONALE
071100110103ZSC                     SCIOPERO
071200110103001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
071300110103002                     MERCE NON ORDINATA O NON CONFORME
071400110103003                     MERCE SPEDITA IN RITARDO
071500110103004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
071600110103005                     MERCE SPEDITA CON TROPPO ANTICIPO
071700110103006                     MERCE RESA SENZA AUTORIZZAZIONE
071800110103007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
071900110103008                     DESTINATARIO NON PAGA IL C/ASSEGNO
072000110103009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
072100110103010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
072200110103012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
072300110103013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
072400110103014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
072500110103016                     DESTINATARIO RIMANDA LO SVINCOLO AL
072600110103017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
072700110103018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
072800110103019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
072900110103020                     DESTINATARIO RISULTA TRASFERITO
073000110103021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
073100110103022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
073200110103023                     IL DESTINATARIO E' CHIUSO FINO AL
073300110103024                     IL DESTINATARIO E' CHIUSO PER FERIE
073400110103025                     IL DESTINATARIO E' CHIUSO
073500110103026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
073600110103027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
073700110103028                     ESERCIZIO NON ANCORA IN ATTIVITA
073800110103029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
073900110103030                     IL DESTINATARIO RIFIUTA LA MERCE
074000110103031                     IL DESTINATARIO NON INTENDE
074100110103032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
074200110103033                     IL DESTINATARIO RICHIEDE
074300110103034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
074400110103035                     DOCUMENTI INCOMPLETI O MANCANTI
074500110103036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
074600110103037                     RIFIUTA LA CONSEGNA TASSATIVA
074700110103100                     -
074800110103101                     -
074900110103999                     -
075000110103CONSOK                  CONSEGNA OK
075100110103RESO                    RESO
075200110927                        -
075300110929GIACAPE                 APERTURA GIACENZA
075400110927GIACDIS                 RICEVUTE DISPOSIZIONI GIACENZA
075500110927GIACEND                 CHIUSURA GIACENZA
075600110927                        -
075700110927                        -
075800110927                        -
075900110927                        -
076000110927                        -
076100110927                        -
076200110927                        -
076300110103** CAUCLI - CAUSALI CLIENTE
076400110926A                       L.AVVISO
076500110926ARS                     AVVISATO-RIMANDA SVINCOLO 1GG
076600110926AVV                     LASCIATO AVVISO 1GG
076700110926AV2                     LASCIATO AVVISO 2GG
076800110926AV3                     LASCIATO AVVISO 3GG
076900110926AV5                     LASCIATO AVVISO 5GG
077000110926AV7                     LASCIATO AVVISO 7GG
077100110926AV9                     LASCIATO AVVISO 9 GIORNI
077200110926A16                     L.A. RIMANDA LO SVINCOLO
077300110926A23                     L.A. CHIUSO FINO AL
077400110926CPO                     CAMBIO PORTO
077500110926DAN                     COLLO/I DANNEGGIATO/I
077600110926DIR                     DIROTTAMENTO
077700110926F                       FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
077800110926G                       GIACENZA GENERICA TEMPORANEA
077900110926GEN                     GIACENZA GENERICA
078000110926G02                     ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
078100110926G03                     ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
078200110926G05                     ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
078300110926G09                     ATTENDERE 9 GIORNI
078400110926IDD                     CHIUSURA SPEDIZIONE CON IDD
078500110926MAN                     COLLO/I MANCANTE/I
078600110926MIC                     MESSA IN CONSEGNA
078700110926N                       NON FATTA
078800110926NAV                     COLLO SCONDIZIONATO
078900110926NIC                     TOLTA DALLA CONSEGNA
079000110926P                       CONSEGNA PARZIALE
079100110926PAT                     FESTIVITA PATRONALE
079200110926R                       RISERVA
079300110926RDC                     RIPRISTINO
079400110926RFD                     RIFIUTATO
079500110926RFM                     RIFIUTATO
079600110926RIC                     RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
079700110926RIP                     RIPRISTINO
079800110926RS                      RISERVA
079900110926T                       CHIUSO PER TURNO
080000110926WA                      AMMANCO SENZA EVENTO
080100110926WAE                     AMMANCO CON EVENTO
080200110926WM                      MANCANZA SENZA EVENTO
080300110926WME                     MANCANZA CON EVENTO
080400110926WV                      AVARIA SENZA EVENTO
080500110926WVE                     AVARIA CON EVENTO
080600110926ZAL                     ALLUVIONE
080700110926ZBS                     BLOCCO STRADALE
080800110926ZFR                     FESTIVITÀ REGIONALE
080900110926ZMM                     MARE MOLTO MOSSO
081000110926ZMP                     MANIFESTAZIONE PUBBLICA
081100110926ZNV                     NEVICATA ECCEZIONALE
081200110926ZSC                     SCIOPERO
081300110926001                     RIFIUTA SENZA SPECIFICARE IL MOTIVO
081400110926002                     MERCE NON ORDINATA O NON CONFORME
081500110926003                     MERCE SPEDITA IN RITARDO
081600110926004                     MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
081700110926005                     MERCE SPEDITA CON TROPPO ANTICIPO
081800110926006                     MERCE RESA SENZA AUTORIZZAZIONE
081900110926007                     DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
082000110926008                     DESTINATARIO NON PAGA IL C/ASSEGNO
082100110926009                     CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
082200110926010                     DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
082300110926012                     IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
082400110926013                     DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
082500110926014                     DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
082600110926016                     DESTINATARIO RIMANDA LO SVINCOLO AL
082700110926017                     DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
082800110926018                     DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
082900110926019                     DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
083000110926020                     DESTINATARIO RISULTA TRASFERITO
083100110926021                     DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
083200110926022                     INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
083300110926023                     IL DESTINATARIO E' CHIUSO FINO AL
083400110926024                     IL DESTINATARIO E' CHIUSO PER FERIE
083500110926025                     IL DESTINATARIO E' CHIUSO
083600110926026                     IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
083700110926027                     DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
083800110926028                     ESERCIZIO NON ANCORA IN ATTIVITA
083900110926029                     IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
084000110926030                     IL DESTINATARIO RIFIUTA LA MERCE
084100110926031                     IL DESTINATARIO NON INTENDE
084200110926032                     FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
084300110926033                     IL DESTINATARIO RICHIEDE
084400110926034                     SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
084500110926035                     DOCUMENTI INCOMPLETI O MANCANTI
084600110926036                     IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
084700110926037                     RIFIUTA LA CONSEGNA TASSATIVA
084800110926NON INVIARE             -
084900110926NON INVIARE             -
085000110926NON INVIARE             -
085100110926CONSOK                  CONSEGNA OK
085200110926RESO                    RESO
085300110926
085400110927*AG                     APERTURA GIACENZA
085500110927*DG                     RICEVUTE DISPOSIZIONI GIACENZA
085600110927*CG                     CHIUSURA GIACENZA
085700110927                        -
085800110927                        -
085900110927                        -
086000110927                        -
086100110927                        -
086200110927                        -
086300110927                        -
086400110926** CAUDES - DESCRIZIONI CAUSALI CLIENTE
086500110103L.AVVISO
086600110103AVVISATO-RIMANDA SVINCOLO 1GG
086700110103LASCIATO AVVISO 1GG
086800110103LASCIATO AVVISO 2GG
086900110103LASCIATO AVVISO 3GG
087000110103LASCIATO AVVISO 5GG
087100110103LASCIATO AVVISO 7GG
087200110103LASCIATO AVVISO 9 GIORNI
087300110103L.A. RIMANDA LO SVINCOLO
087400110103L.A. CHIUSO FINO AL
087500110103CAMBIO PORTO
087600110103COLLO/I DANNEGGIATO/I
087700110103DIROTTAMENTO
087800110103FERMO DEPOSITO-2 GG ATTESA PER APERTURA GIACENZA
087900110103GIACENZA GENERICA TEMPORANEA
088000110103GIACENZA GENERICA
088100110103ATTENDERE 2 GIORNI PRIMA DI APRIRE GIACENZA
088200110103ATTENDERE 3 GIORNI PRIMA DI APRIRE GIACENZA
088300110103ATTENDERE 5 GG. PRIMA DI APRIRE GIACENZA
088400110103ATTENDERE 9 GIORNI
088500110103CHIUSURA SPEDIZIONE CON IDD
088600110103COLLO/I MANCANTE/I
088700110103MESSA IN CONSEGNA
088800110103NON FATTA
088900110103COLLO SCONDIZIONATO
089000110103TOLTA DALLA CONSEGNA
089100110103CONSEGNA PARZIALE
089200110103FESTIVITA PATRONALE
089300110103RISERVA
089400110103RIPRISTINO
089500110103RIFIUTATO
089600110103RIFIUTATO
089700110103RICONSEGNA X PRIMO PRIMO LASCIATO AVVISO
089800110103RIPRISTINO
089900110103RISERVA
090000110103CHIUSO PER TURNO
090100110103AMMANCO SENZA EVENTO
090200110103AMMANCO CON EVENTO
090300110103MANCANZA SENZA EVENTO
090400110103MANCANZA CON EVENTO
090500110103AVARIA SENZA EVENTO
090600110103AVARIA CON EVENTO
090700110103ALLUVIONE
090800110103BLOCCO STRADALE
090900110103FESTIVITÀ REGIONALE
091000110103MARE MOLTO MOSSO
091100110103MANIFESTAZIONE PUBBLICA
091200110103NEVICATA ECCEZIONALE
091300110103SCIOPERO
091400110103RIFIUTA SENZA SPECIFICARE IL MOTIVO
091500110103MERCE NON ORDINATA O NON CONFORME
091600110103MERCE SPEDITA IN RITARDO
091700110103MERCE GIA' RICEVUTA CON PRECEDENTE INVIO
091800110103MERCE SPEDITA CON TROPPO ANTICIPO
091900110103MERCE RESA SENZA AUTORIZZAZIONE
092000110103DESTINATARIO DICHIARA DI AVER ANN.TO L'ORDINE
092100110103DESTINATARIO NON PAGA IL C/ASSEGNO
092200110103CLIENTE VUOLE CONTROLLO MERCE PRIMA DELLO SVINCOLO
092300110103DESTINATARIO CONTESTA L'AMMONTARE DEL C/ASSEGNO
092400110103IL DESTINATARIO NON PAGA LE SPESE DI TRASPORTO
092500110103DESTINATARIO NON PAGA PROVVIGIONI SUL C/ASSEGNO
092600110103DESTINATARIO NON INTENDE PAGARE L'ANTEPORTO
092700110103DESTINATARIO RIMANDA LO SVINCOLO AL
092800110103DESTINATARIO ERA ASSENTE: LASCIATO AVVISO
092900110103DESTINATARIO AVVISATO NON PROVVEDE ALLO SVINCOLO
093000110103DESTINATARIO HA CESSATO L'ATTIVITA' O SI E' TRASFERITO
093100110103DESTINATARIO RISULTA TRASFERITO
093200110103DESTINATARIO SCONOSCIUTO ALL'INDIRIZZO DDT/INCOMPLETO
093300110103INDIRIZZO INDICATO SUL DDT INESISTENTE/INCOMPLETO
093400110103IL DESTINATARIO E' CHIUSO FINO AL
093500110103IL DESTINATARIO E' CHIUSO PER FERIE
093600110103IL DESTINATARIO E' CHIUSO
093700110103IL DESTINATARIO CHIEDE CONSEGNA AD ALTRO INDIRIZZO
093800110103DESTINATARIO DICHIARA MERCE NON CONFORME AD ORDINE
093900110103ESERCIZIO NON ANCORA IN ATTIVITA
094000110103IL DESTINATARIO CONTESTA LE CONDIZIONI DI PAGAMENTO
094100110103IL DESTINATARIO RIFIUTA LA MERCE
094200110103IL DESTINATARIO NON INTENDE
094300110103FERMO DEPOSITO-NESSUNO SI E' PRESENTATO PER RITIRO
094400110103IL DESTINATARIO RICHIEDE
094500110103SPEDIZIONE NON CONSEGNABILE CAUSA FORZA MAGGIORE
094600110103DOCUMENTI INCOMPLETI O MANCANTI
094700110103IL DESTINATARIO VUOLE ACCETTARE LA MERCE CON RISERVA
094800110103RIFIUTA LA CONSEGNA TASSATIVA
094900110103-
095000110103-
095100110103-
095200110103CONSEGNA OK
095300110103RESO
095400110927-
095500110927APERTURA GIACENZA
095600110927RICEVUTE DISPOSIZIONI GIACENZA
095700110927CHIUSURA GIACENZA
095800110927-
095900110927-
096000110927-
096100110927-
096200110927-
096300110927-
096400110927-
096500041015** WDCTBAR - TIPI ANOMALIE C/A BARTOLINI
096600110103A                                                                                            1
096700110103M                                                                                            2
096800110103V                                                                                            3
096900110103E                                                                                            4
097000080901** WDCTCLI - CAUSALI CLIENTE
097100110926TAD_A                                                                                        1
097200110926TAD_M                                                                                        2
097300110926TAD_V                                                                                        3
097400110926TAD_E                                                                                        4
097500110926** WDCTDES - DESCRIZIONE TIPI ANOMALIE C/A BARTOLINI
097600110927AMMANCO                                                                                      1
097700110927MANCA COLLO                                                                                  2
097800110927AVARIA                                                                                       3
097900110927EVENTO                                                                                       4
098000110103** WCOD9BAR - CODICI EVENTO "PERSONALIZZATI" - BARTOLINI
098100110103INCARICO1                                                                                    1
098200110103INCARICO2                                                                                    2
098300110103CONSOK1                                                                                      3
098400110103CONSOK2                                                                                      4
098500110927GIACNODIS1                                                                                   5
098600110927GIACNODIS2                                                                                   6
098700110927GIACSIDIS1                                                                                   7
098800110927GIACSIDIS2                                                                                   8
098900110927                                                                                             9
099000110927                                                                                             10
099100110103** WCOD9CLI - CODICI EVENTO "PERSONALIZZATI" - CLIENTE
099200110926ACCETTAZIONE                                                                                 1
099300110926ACCETTAZIONE                                                                                 2
099400110103NON INVIARE                                                                                  3
099500110103NON INVIARE                                                                                  4
099600110927GIAC_APERTA                                                                                  5
099700110927GIAC_APERTA                                                                                  6
099800110927GIAC_DISPOS                                                                                  7
099900110927GIAC_DISPOS                                                                                  8
100000110927NON INVIARE                                                                                  9
100100110927NON INVIARE                                                                                  10
100200110926** WCOD9DES - DESCRIZIONI CODICI EVENTO "PERSONALIZZATI"
100300110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           1
100400110926ACCETTAZIONE SPEDIZIONE (TRANSITO)                                                           2
100500110926NON INVIARE                                                                                  3
100600110926NON INVIARE                                                                                  4
100700110927APERTURA GIACENZA                                                                            5
100800110927APERTURA GIACENZA                                                                            6
100900110927RICEVUTE DISPOSIZIONI                                                                        7
101000110927RICEVUTE DISPOSIZIONI                                                                        8
101100110927NON INVIARE                                                                                  8
101200110927NON INVIARE                                                                                  10
